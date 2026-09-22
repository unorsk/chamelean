import Lean.Elab.Command

/-!
`uint_enum`: one table per protocol enum.

The firmware speaks small integers; we want real Lean types with exhaustive matching. Writing
the code → constructor → label mapping three times by hand is where typos live, so this macro
takes a single table

    uint_enum Status : UInt16 where
      | hfTagOk := 0x00 => "HF tag operation succeeded"
      | hfTagNo := 0x01 => "HF tag no found or lost"

and generates the inductive plus `toUInt16`, `ofUInt16?`, `all`, `name`, `description` and the
`BEq`/`Hashable`/`ToString` instances. The label after `=>` is optional; without it the
constructor name is used. The generated names follow the representation type, so an enum
declared `: UInt8` gets `toUInt8`/`ofUInt8?`.

`DecidableEq` is deliberately not derived: for a 200-constructor enum like `Command` the derived
instance is quadratic to compile, and comparing codes is just as good.
-/
namespace Chamelean
open Lean Elab Command

/-- One row of a `uint_enum` table: `| name := code => "label"`, optionally preceded by a
doc comment that carries over to the constructor. -/
syntax uintEnumItem := atomic((docComment)? "| ") ident " := " term (" => " str)?

syntax (docComment)? "uint_enum " ident " : " ident " where " uintEnumItem* : command

elab_rules : command
  | `(command| $[$doc?:docComment]? uint_enum $enum:ident : $rep:ident where $items*) => do
    let mut ctors : Array Ident := #[]
    let mut ctorDecls : Array (TSyntax ``Lean.Parser.Command.ctor) := #[]
    let mut codes : Array Term := #[]
    let mut labels : Array Term := #[]
    for item in items do
      let `(uintEnumItem| $[$ctorDoc?:docComment]? | $c:ident := $code:term $[=> $label?:str]?) := item
        | throwErrorAt item "expected `| name := code` or `| name := code => \"label\"`"
      ctors := ctors.push c
      ctorDecls := ctorDecls.push (← `(Lean.Parser.Command.ctor| $[$ctorDoc?:docComment]? | $c:ident))
      codes := codes.push code
      labels := labels.push (label?.map (⟨·.raw⟩) |>.getD (Syntax.mkStrLit c.getId.toString))
    if ctors.isEmpty then throwError "a uint_enum needs at least one constructor"
    -- Python marked these `@enum.unique`; a duplicated code would silently make one
    -- constructor unreachable through `of<Rep>?`, so reject it here.
    let mut seen : Std.HashMap Nat Ident := {}
    for (c, code) in ctors.zip codes do
      if let some n := code.raw.isNatLit? then
        if let some prev := seen[n]? then
          throwErrorAt code "code {n} is already used by `{prev.getId}`"
        seen := seen.insert n c
    -- Qualified constructor idents work both as patterns and as terms.
    let qual := ctors.map fun c => mkIdentFrom c (enum.getId ++ c.getId)
    let names := ctors.map fun c => Syntax.mkStrLit c.getId.toString
    let fn (stem : String) : Ident := mkIdentFrom enum (enum.getId ++ Name.mkSimple stem)
    let toFn := fn s!"to{rep.getId}"
    let ofFn := fn s!"of{rep.getId}?"
    let allFn := fn "all"
    let nameFn := fn "name"
    let descFn := fn "description"
    elabCommand <| ← `(command| $[$doc?:docComment]? inductive $enum where
      $ctorDecls*
      deriving Repr, Inhabited)
    elabCommand <| ← `(command|
      /-- The code this constructor has on the wire. -/
      def $toFn (x : $enum) : $rep := match x with $[| $qual:ident => $codes]*)
    elabCommand <| ← `(command|
      /-- Every constructor, in declaration order. -/
      def $allFn : Array $enum := #[$qual,*])
    elabCommand <| ← `(command|
      /-- The constructor with this wire code, if there is one. -/
      def $ofFn (code : $rep) : Option $enum := Array.find? (fun c => $toFn c == code) $allFn)
    elabCommand <| ← `(command|
      /-- The constructor's own name, e.g. `"getAppVersion"`. -/
      def $nameFn (x : $enum) : String := match x with $[| $qual:ident => $names]*)
    elabCommand <| ← `(command|
      /-- Human-readable label; falls back to the constructor name. -/
      def $descFn (x : $enum) : String := match x with $[| $qual:ident => $labels]*)
    elabCommand <| ← `(command| instance : BEq $enum := ⟨fun a b => $toFn a == $toFn b⟩)
    elabCommand <| ← `(command| instance : Hashable $enum := ⟨fun a => hash ($toFn a)⟩)
    elabCommand <| ← `(command| instance : ToString $enum := ⟨$descFn⟩)

end Chamelean
