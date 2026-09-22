import Chamelean

/-!
Entry point: launch the interactive Chameleon Ultra REPL.

    chamelean            start the shell offline (use `hw connect` inside it)
    chamelean PORT       connect to PORT first, then start the shell
                         (PORT is a serial path, or `tcp:host:port`)

The command surface lives in `Chamelean/Cli/*`; this file only wires argv to `Cli.repl`.
-/
open Chamelean Chamelean.Cli

def main (args : List String) : IO UInt32 := do
  try
    match args with
    | [] => repl none
    | port :: _ => repl (some port)
    return 0
  catch e =>
    IO.eprintln s!"error: {e}"
    return 1
