import Chamelean.Client
import Chamelean.Cli.Args

/-!
The command tree and the shared runner context.

Python registered commands with decorators onto a `CLITree` and expressed shared behavior
through base classes (`DeviceRequiredUnit`, `ReaderRequiredUnit`, `SlotIndexArgsUnit`, ...).
In Lean the tree is a plain inductive of groups and leaves, each leaf a `CmdUnit` record
`{ parser, run }`, and the shared behavior becomes combinators that wrap a runner rather than
inheritance.

`ReplState` is the mutable session: the open device (or none) and whether the loop keeps
running. Commands like `hw connect`/`disconnect`/`exit` mutate it.
-/
namespace Chamelean.Cli

open Chamelean

/-- Mutable REPL session state, shared across command invocations. -/
structure ReplState where
  /-- The open device, or `none` when offline. -/
  client : IO.Ref (Option Client)
  /-- Cleared by `exit` to break the main loop. -/
  running : IO.Ref Bool

/-- A fresh, offline session. -/
def ReplState.new : IO ReplState :=
  return { client := ← IO.mkRef none, running := ← IO.mkRef true }

/-- The current device if one is open and live, else `none` (drops a closed handle). -/
def ReplState.device? (st : ReplState) : IO (Option Client) := do
  match ← st.client.get with
  | none => return none
  | some c => if ← c.isOpen then return some c else st.client.set none; return none

/-- The open device, or a `CliError` telling the user to connect first. -/
def ReplState.requireDevice (st : ReplState) : IO Client := do
  match ← st.device? with
  | some c => return c
  | none => throw (CliError.other "Please connect to a Chameleon device first (use 'hw connect').").toIO

/-- Replace the tracked device, closing any previous one. -/
def ReplState.setClient (st : ReplState) (c? : Option Client) : IO Unit := do
  if let some old ← st.client.get then old.close
  st.client.set c?

/-- A single runnable command: how to parse its arguments and what to do with them. -/
structure CmdUnit where
  parser : ArgParser
  /-- Run against the session and the already-parsed arguments. -/
  run : ReplState → Args → IO Unit

/-- The command tree: named groups nesting groups and leaves. -/
inductive CliTree where
  | group (name : String) (help : String) (children : List CliTree)
  | leaf (name : String) (help : String) (unit : CmdUnit)
deriving Inhabited

namespace CliTree

def name : CliTree → String
  | .group n _ _ => n
  | .leaf n _ _ => n

def help : CliTree → String
  | .group _ h _ => h
  | .leaf _ h _ => h

def isGroup : CliTree → Bool
  | .group .. => true
  | .leaf .. => false

def children : CliTree → List CliTree
  | .group _ _ cs => cs
  | .leaf .. => []

/--
Walk `argv` down the tree, consuming tokens that name children. Returns the deepest matching
node and the tokens left for its argument parser. Port of `get_cmd_node`.
-/
partial def resolve (node : CliTree) (argv : List String) : CliTree × List String :=
  match argv with
  | [] => (node, [])
  | tok :: more =>
    match node.children.find? (·.name == tok) with
    | some child => resolve child more
    | none => (node, argv)

end CliTree

/-! ## Runner combinators

These wrap a runner to add a precondition, replacing the Python base-class `before_exec`
chain. A runner that needs the device takes a `Client` directly; the combinator supplies it. -/

/-- Require an open device; hand the runner the `Client`. Port of `DeviceRequiredUnit`. -/
def deviceRequired (run : Client → Args → IO Unit) : ReplState → Args → IO Unit :=
  fun st a => do run (← st.requireDevice) a

/-- Require the device and put it in reader mode first. Port of `ReaderRequiredUnit`. -/
def readerRequired (run : Client → Args → IO Unit) : ReplState → Args → IO Unit :=
  fun st a => do
    let c ← st.requireDevice
    unless ← c.getDeviceMode do
      c.setReaderMode true
      IO.println "Switched to { Tag Reader } mode."
    run c a

-- TODO(phase 5): SlotIndexArgs(AndGo), SenseTypeArgs, MF1AuthArgs, MFUAuthArgs and the LF
-- *IdArgs families become combinators here once their Client wrappers (phase 3) land.

end Chamelean.Cli
