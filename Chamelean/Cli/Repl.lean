import Chamelean.Cli.Tree
import Chamelean.Cli.Pretty

/-!
The interactive REPL: the command registry, argument dispatch, and the read-eval loop.

Port of `chameleon_cli_main.py`. No prompt_toolkit — a plain `IO.getLine` loop, so tab
completion and history are dropped (see PORTING.md). One command per input line; the shell's
own line editing stands in for readline.

The registry (`root`) is wired here with a proof-of-life subset (`clear`, `rem`, `exit`,
`dump_help`, `hw connect`/`disconnect`/`version`). Phases 3–5 fill in the remaining groups and
leaves; each new command is one `CliTree.leaf`.
-/
namespace Chamelean.Cli

open Chamelean

/-- First USB CDC serial device: `/dev/cu.usbmodem*` (macOS) or `/dev/ttyACM*` (Linux). -/
def autoDetectPort : IO (Option String) := do
  let pfx := if System.Platform.isOSX then "cu.usbmodem" else "ttyACM"
  let entries ← try System.FilePath.readDir "/dev" catch _ => pure #[]
  let names := entries.filter (·.fileName.startsWith pfx) |>.map (·.fileName) |>.qsort (· < ·)
  return names[0]?.map (s!"/dev/{·}")

/-! ## Root command registry -/

/-- Build a leaf command node. -/
private def leaf (name help : String) (parser : ArgParser)
    (run : ReplState → Args → IO Unit) : CliTree :=
  .leaf name help { parser, run }

private def clearCmd : CliTree :=
  leaf "clear" "Clear screen" { description := "Clear screen" } fun _ _ => do
    let cmd := if System.Platform.isWindows then "cls" else "clear"
    try let _ ← IO.Process.output { cmd } catch _ => pure ()

private def remCmd : CliTree :=
  leaf "rem" "Timestamped comment"
    { description := "Timestamped comment"
      specs := [{ key := "comment", help := "Your comment" }] } fun _ a => do
    let stamp ← try
        let out ← IO.Process.output { cmd := "date", args := #["-u", "+%Y-%m-%dT%H:%M:%SZ"] }
        pure out.stdout.trimAscii
      catch _ => pure ""
    IO.println s!"{stamp} remark: {a.str? "comment" |>.getD ""}"

private def exitCmd : CliTree :=
  leaf "exit" "Exit client" { description := "Exit client" } fun st _ => do
    IO.println "Bye, thank you.  ^.^ "
    st.setClient none
    st.running.set false

/-- Recursively print every command with its usage. Port of `RootDumpHelp.dump_help`. -/
private partial def dumpHelp (node : CliTree) (prefix_ : String) (showGroups : Bool) : IO Unit := do
  match node with
  | .leaf name _ unit =>
    let full := (if prefix_.isEmpty then "" else prefix_ ++ " ") ++ name
    IO.println s!"{green full}\t{yellow unit.parser.description}"
  | .group name _ children =>
    let full := (if prefix_.isEmpty then "" else prefix_ ++ " ") ++ name
    if showGroups && !full.isEmpty then IO.println (blue s!"== {full} ==")
    for child in children do
      dumpHelp child full showGroups

private def dumpHelpCmd (root : Unit → CliTree) : CliTree :=
  leaf "dump_help" "Dump available commands"
    { description := "Dump available commands"
      specs := [{ key := "showGroups", names := ["-g", "--show-groups"], kind := .flag,
                  help := "Dump command groups as well" }] } fun _ a => do
    dumpHelp (root ()) "" (a.has "showGroups")

private def connectCmd : CliTree :=
  leaf "connect" "Connect to Chameleon by serial port"
    { description := "Connect to Chameleon by serial port"
      specs := [{ key := "port", names := ["-p", "--port"], metavar := "<path>",
                  help := "Serial device path (auto-detected if omitted)" }] } fun st a => do
    let port? := a.str? "port" <|> (← autoDetectPort)
    let some port := port?
      | IO.println (red "Chameleon not found; connect the device or pass -p <path>.")
    try
      let c ← Client.connect port
      let _ ← c.loadCapabilities
      let ver ← c.sendCmd .getAppVersion
      let model ← c.sendCmd .getDeviceModel
      st.setClient (some c)
      let name := if model.data[0]?.getD 0 == 0 then "Ultra" else "Lite"
      IO.println s!" \{ Chameleon {name} connected: v{ver.data[0]!}.{ver.data[1]!} }"
    catch e =>
      IO.println (red s!"Chameleon connect failed: {e}")

private def disconnectCmd : CliTree :=
  leaf "disconnect" "Disconnect Chameleon" { description := "Disconnect Chameleon" } fun st _ => do
    st.setClient none

private def versionCmd : CliTree :=
  leaf "version" "Get application version" { description := "Get application version" } <|
    deviceRequired fun c _ => do
      let ver ← c.sendCmd .getAppVersion
      IO.println s!"v{ver.data[0]!}.{ver.data[1]!}"

/-- The whole command tree. A `Unit →` thunk so `dump_help` can reference the root it lives in. -/
partial def root (_ : Unit) : CliTree :=
  .group "" "" [
    clearCmd, remCmd, exitCmd, dumpHelpCmd root,
    .group "hw" "Hardware-related commands" [
      connectCmd, disconnectCmd, versionCmd
      -- TODO(phase 5): slot, settings, mode, chipid, address, dfu, factory_reset, battery, raw
    ]
    -- TODO(phase 5): hf, lf, data, emv groups
  ]

/-! ## Dispatch and loop -/

/-- Print a group's children as a menu, like the Python REPL does for an unimplemented group. -/
private def printGroup (node : CliTree) : IO Unit := do
  IO.println (String.ofList (List.replicate 60 '-'))
  for child in node.children do
    let title := green child.name
    let suffix := if child.isGroup then s!"\{ {child.help}... }" else child.help
    IO.println s!" - {title}\t{suffix}"

/--
Run one command line: resolve it in the tree, print a group menu or parse-and-run a leaf.
Aliases (`quit/q/e`→`exit`, leading `;#%`→`rem`) mirror the Python REPL. Port of `exec_cmd`.
-/
def execCmd (st : ReplState) (line : String) : IO Unit := do
  let line := line.trimAscii.toString
  if line.isEmpty then return
  -- Aliases.
  let line :=
    if line == "quit" || line == "q" || line == "e" then "exit"
    else if ";#%".toList.contains line.front then "rem " ++ (line.drop 1).toString
    else line
  let argv := words line
  let (node, rest) := (root ()).resolve argv
  match node with
  | .group .. => printGroup node
  | .leaf name _ unit =>
    -- Reconstruct the command's full name for help/error messages.
    let prog := String.intercalate " " (argv.take (argv.length - rest.length))
    match unit.parser.parse rest with
    | .error (.usage msg) =>
      IO.println (unit.parser.renderHelp prog)
      IO.println (yellow msg)
    | .error e => IO.println (red (toString e))
    | .ok args =>
      try unit.run st args
      catch e => IO.println (red s!"{name}: {e}")

def banner : String :=
  "\n" ++
  " ██████╗██╗  ██╗ █████╗ ███╗   ███╗███████╗██╗     ███████╗ █████╗ ███╗   ██╗\n" ++
  "██╔════╝██║  ██║██╔══██╗████╗ ████║██╔════╝██║     ██╔════╝██╔══██╗████╗  ██║\n" ++
  "██║     ███████║███████║██╔████╔██║█████╗  ██║     █████╗  ███████║██╔██╗ ██║\n" ++
  "██║     ██╔══██║██╔══██║██║╚██╔╝██║██╔══╝  ██║     ██╔══╝  ██╔══██║██║╚██╗██║\n" ++
  "╚██████╗██║  ██║██║  ██║██║ ╚═╝ ██║███████╗███████╗███████╗██║  ██║██║ ╚████║\n" ++
  " ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝\n"

/-- The prompt string, showing whether a device is connected. -/
def prompt (st : ReplState) : IO String := do
  let status := if (← st.device?).isSome then green "USB" else red "Offline"
  return s!"[{status}] chameleon --> "

/-- Read-eval loop: print the banner, then dispatch a command per line until `exit`/EOF. -/
partial def loop (st : ReplState) : IO Unit := do
  IO.println (yellow banner)
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  while ← st.running.get do
    stdout.putStr (← prompt st)
    stdout.flush
    let line ← stdin.getLine
    if line.isEmpty then break  -- EOF (Ctrl-D)
    execCmd st line
  -- Ensure the device is released on the way out.
  st.setClient none

/-- Launch the REPL. If `port?` is given, connect to it before the first prompt. -/
def repl (port? : Option String := none) : IO Unit := do
  let st ← ReplState.new
  if let some port := port? then
    execCmd st s!"hw connect -p {port}"
  loop st

end Chamelean.Cli
