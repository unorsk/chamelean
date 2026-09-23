/-!
Long-running "watch" commands: poll the device until the user presses ESC.

The REPL reads whole lines, so the terminal normally holds keys back until Enter. While a
watch runs we switch it to key-at-a-time mode (via `stty`, which ships with the OS), poll stdin
between iterations, and restore the previous settings on the way out, error or not.

Ctrl-C is turned into a plain byte in key mode too, so it also stops the watch instead of killing
the whole REPL (and leaving the terminal stuck in key mode).

When stdin is not a terminal (input piped in) there are no keys to read: the loop just sleeps,
and Ctrl-C ends the process as usual.
-/
namespace Chamelean.Cli

/-- Run `stty` on our own stdin. `none` when it fails, i.e. stdin is not a terminal. -/
private def stty (args : Array String) : IO (Option String) := do
  -- `IO.Process.output` would give the child a null stdin; stty must see the real terminal.
  let child ← IO.Process.spawn
    { cmd := "stty", args, stdin := .inherit, stdout := .piped, stderr := .null }
  let out ← child.stdout.readToEnd
  return if (← child.wait) == 0 then some out.trimAscii.toString else none

/--
Run `act` with the terminal in key mode: no line buffering, no echo, Ctrl-C delivered as a byte,
and reads that return after at most 100 ms. `act` gets a handle to read keys from, or `none`
when stdin is not a terminal.

Keys are read through a separate `/dev/tty` handle, not stdin: a read that times out marks its
handle as at end-of-file, and on stdin that would make the REPL's next `getLine` come back empty
and quit.
-/
def withKeyMode (act : Option IO.FS.Handle → IO α) : IO α := do
  let some saved ← stty #["-g"] | act none
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let _ ← stty #["-icanon", "-echo", "-isig", "min", "0", "time", "1"]
  try act (some tty) finally let _ ← stty #[saved]

/--
Read whatever keys arrived on `tty` (waiting up to 100 ms) and report whether one of them means
stop: a lone ESC, Ctrl-C or Ctrl-D. Only valid inside `withKeyMode`.
-/
def stopKeyPressed (tty : IO.FS.Handle) : IO Bool := do
  let bytes := (← tty.read 64).toList
  -- Arrow and function keys arrive as ESC followed by `[` or `O`; those are not a stop.
  let rec scan : List UInt8 → Bool
    | [] => false
    | 0x1b :: next :: rest => if next == '['.toUInt8 || next == 'O'.toUInt8 then scan rest else true
    | 0x1b :: [] => true
    | b :: rest => b == 0x03 || b == 0x04 || scan rest
  return scan bytes

/--
Call `step` over and over, `intervalMs` apart, until the user presses ESC (or Ctrl-C).
`step` threads a state through the iterations, e.g. what it saw last time, so it can print only
changes. An error from `step` ends the watch and propagates, with the terminal restored.
-/
def watchLoop (init : σ) (step : σ → IO σ) (intervalMs : Nat := 300) : IO Unit :=
  withKeyMode fun tty? => do
    IO.println s!"Watching... press {if tty?.isSome then "ESC" else "Ctrl-C"} to stop."
    let mut s := init
    repeat
      s ← step s
      if let some tty := tty? then
        -- Each poll waits up to 100 ms, so this doubles as the sleep between steps.
        if ← (List.range (max 1 (intervalMs / 100))).anyM fun _ => stopKeyPressed tty then break
      else
        IO.sleep intervalMs.toUInt32

end Chamelean.Cli
