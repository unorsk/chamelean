import Std.Data.HashMap
import Chamelean.Command
import Chamelean.Client
import Chamelean.Cli.Pretty

/-!
Argument parsing for CLI commands, and the CLI error type.

Replaces argparse (`ArgumentParserNoExit`) with a small idiomatic parser covering exactly what
the commands use: boolean flags, value options (`str`/`int`/`hex`), aliases, required options,
value whitelists (`choices`), mutually-exclusive groups, and a trailing positional capture.
Parsing returns `Except CliError Args`; nothing ever exits the process.

`CliError` unifies Python's `ArgsParserError` (bad arguments) and `UnexpectedResponseError`
(a device response with an unaccepted status), plus a catch-all.
-/
namespace Chamelean.Cli

open Chamelean

/-- A recoverable CLI failure. The REPL prints these instead of crashing. -/
inductive CliError
  /-- Bad or missing arguments; the REPL shows the command's help alongside. -/
  | usage (msg : String)
  /-- A device response carried a status outside the accepted set. -/
  | unexpectedResponse (msg : String)
  /-- Anything else that should abort the command with a message. -/
  | other (msg : String)
deriving Inhabited

def CliError.message : CliError → String
  | .usage m | .unexpectedResponse m | .other m => m

instance : ToString CliError := ⟨CliError.message⟩

/-- Surface a `CliError` as an `IO` error, so command runners can stay in `IO`. -/
def CliError.toIO (e : CliError) : IO.Error := IO.userError e.message

/-- How an option's value is interpreted and validated. -/
inductive ArgKind
  /-- A boolean switch with no value (e.g. `--reader`). -/
  | flag
  /-- A raw string value. -/
  | str
  /-- A decimal integer value. -/
  | int
  /-- A hex string value, decoded to bytes. -/
  | hex
deriving BEq, Inhabited

/-- One argument in a command's spec. A spec with empty `names` is the trailing positional
capture (there is at most one; it collects every leftover non-flag token under `key`). -/
structure ArgSpec where
  /-- Canonical name used to look the value up after parsing. -/
  key : String
  /-- CLI tokens that introduce this arg, e.g. `["-s", "--slot"]`. Empty ⇒ positional. -/
  names : List String := []
  kind : ArgKind := .str
  required : Bool := false
  /-- If non-empty, the raw value must be one of these. -/
  choices : List String := []
  metavar : String := ""
  help : String := ""
deriving Inhabited

/-- A set of option keys of which at most one may appear (`add_mutually_exclusive_group`). -/
structure ArgGroup where
  members : List String
  required : Bool := false
deriving Inhabited

/-- A command's full argument specification. -/
structure ArgParser where
  description : String := ""
  specs : List ArgSpec := []
  groups : List ArgGroup := []
deriving Inhabited

/-- Parsed arguments: validated at parse time, read back through the typed accessors. -/
structure Args where
  /-- Value options and the positional capture (joined by spaces), keyed by `ArgSpec.key`. -/
  values : Std.HashMap String String := {}
  /-- Keys of flags and value options that were present. -/
  present : Std.HashMap String Unit := {}
deriving Inhabited

namespace Args

/-- Whether a flag or option with this key appeared. -/
def has (a : Args) (key : String) : Bool := a.present.contains key

/-- Raw string value for `key`, if present. -/
def str? (a : Args) (key : String) : Option String := a.values[key]?

/-- Integer value for `key`; `none` if absent (values are validated at parse time). -/
def int? (a : Args) (key : String) : Option Int :=
  a.values[key]? >>= fun s => (String.toInt? s)

/-- Decoded bytes for a `hex` value; `none` if absent. -/
def hex? (a : Args) (key : String) : Option ByteArray :=
  a.values[key]? >>= fun s => (Cli.ofHex s).toOption

end Args

namespace ArgParser

/-- Find the spec whose `names` contain `token`. -/
private def specFor (p : ArgParser) (token : String) : Option ArgSpec :=
  p.specs.find? (·.names.contains token)

/-- The single positional-capture spec, if the command declares one. -/
private def positional (p : ArgParser) : Option ArgSpec :=
  p.specs.find? (·.names.isEmpty)

/-- Validate a raw value against a spec's kind and choices. -/
private def validate (s : ArgSpec) (raw : String) : Except CliError String := do
  if !s.choices.isEmpty && !s.choices.contains raw then
    throw <| .usage s!"argument {s.key}: invalid choice '{raw}' (choose from {String.intercalate ", " s.choices})"
  match s.kind with
  | .int =>
    if (String.toInt? raw).isNone then throw <| .usage s!"argument {s.key}: '{raw}' is not an integer"
  | .hex =>
    match Cli.ofHex raw with
    | .ok _ => pure ()
    | .error e => throw <| .usage s!"argument {s.key}: {e}"
  | _ => pure ()
  return raw

/--
Parse `argv` against this spec. Handles `--opt value` and `--opt=value`, flag aliases,
required checks, choices, and mutually-exclusive groups. Unknown `-`-prefixed tokens error.
-/
def parse (p : ArgParser) (argv : List String) : Except CliError Args := do
  let mut args : Args := {}
  let mut rest : List String := []
  let mut toks := argv
  while true do
    match toks with
    | [] => break
    | tok :: more =>
      toks := more
      -- Split `--opt=value` into token and inline value.
      let (name, inlineVal) :=
        if tok.startsWith "--" && tok.toList.contains '=' then
          match tok.splitOn "=" with
          | key :: v :: vs => (key, some (String.intercalate "=" (v :: vs)))
          | _ => (tok, none)
        else (tok, none)
      if tok.startsWith "-" && (p.specFor name).isSome then
        let some s := p.specFor name | throw <| .other "unreachable"
        match s.kind with
        | .flag =>
          if inlineVal.isSome then throw <| .usage s!"argument {name}: takes no value"
          args := { args with present := args.present.insert s.key () }
        | _ =>
          let raw ← match inlineVal with
            | some v => pure v
            | none =>
              match toks with
              | v :: vRest => toks := vRest; pure v
              | [] => throw <| .usage s!"argument {name}: expected a value"
          let v ← validate s raw
          args := { args with values := args.values.insert s.key v,
                              present := args.present.insert s.key () }
      else if tok.startsWith "-" && tok != "-" &&
              !((tok.toList.drop 1).head?.map (·.isDigit) |>.getD false) then
        -- A dash-led token that is not a negative number and matches no spec.
        throw <| .usage s!"unknown option: {tok}"
      else
        rest := rest ++ [tok]
  -- Bind the trailing positional capture.
  match p.positional with
  | some s =>
    if !rest.isEmpty then
      let joined := String.intercalate " " rest
      args := { args with values := args.values.insert s.key joined,
                          present := args.present.insert s.key () }
    else if s.required then
      throw <| .usage s!"argument {s.key}: required"
  | none =>
    if !rest.isEmpty then throw <| .usage s!"unexpected argument(s): {String.intercalate " " rest}"
  -- Required value options.
  for s in p.specs do
    if s.required && !s.names.isEmpty && !args.has s.key then
      throw <| .usage s!"argument {(s.names.head?.getD s.key)}: required"
  -- Mutually-exclusive groups.
  for g in p.groups do
    let n := (g.members.filter args.has).length
    if n > 1 then
      throw <| .usage s!"arguments {String.intercalate "/" g.members} are mutually exclusive"
    if g.required && n == 0 then
      throw <| .usage s!"one of {String.intercalate "/" g.members} is required"
  return args

/-- One-line usage plus per-argument help, colored. Rendered when parsing fails. -/
def renderHelp (prog : String) (p : ArgParser) : String := Id.run do
  let mut out := s!"{Cli.green "usage:"} {Cli.red prog}"
  for s in p.specs do
    let label := if s.names.isEmpty then s!"<{if s.metavar.isEmpty then s.key else s.metavar}>"
                 else s.names.head!.append (if s.kind == .flag then "" else s!" <{if s.metavar.isEmpty then s.key else s.metavar}>")
    out := out ++ (if s.required then s!" {label}" else s!" [{label}]")
  if !p.description.isEmpty then out := out ++ "\n" ++ Cli.cyan p.description
  for s in p.specs do
    let names := if s.names.isEmpty then s.key else String.intercalate ", " s.names
    if !s.help.isEmpty then out := out ++ s!"\n  {Cli.green names}\t{s.help}"
  return out

end ArgParser

/--
Assert a response status is accepted, else raise `UnexpectedResponseError`. Returns the
payload on success. Port of `expect_response`; the default accepts the device-wide SUCCESS.
-/
def expectResponse (r : Response) (accepted : List UInt16 := [Status.success.toUInt16])
    (context : String := "") : Except CliError ByteArray :=
  if accepted.contains r.status then
    .ok r.data
  else
    let where_ := if context.isEmpty then "" else s!"{context}: "
    .error <| .unexpectedResponse s!"{where_}{Status.describe r.status}"

end Chamelean.Cli
