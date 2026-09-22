/-!
Terminal display helpers for the CLI: ANSI colors, hex encode/decode, and block dumps.

Port of the color/format helpers in `chameleon_utils.py` (`color_string`, the `CR/CG/...`
shorthands, `print_mem_dump`, `print_key_table`). Dependency-free: raw ANSI escapes instead
of colorama.
-/
namespace Chamelean.Cli

/-! ANSI escape sequences. `reset` clears every attribute, matching colorama's autoreset. -/
namespace Ansi

def reset : String := "\x1b[0m"
def red : String := "\x1b[31m"
def green : String := "\x1b[32m"
def blue : String := "\x1b[34m"
def cyan : String := "\x1b[36m"
def yellow : String := "\x1b[33m"
def magenta : String := "\x1b[35m"

end Ansi

/-- Wrap `s` in a color and a trailing reset. -/
def color (code : String) (s : String) : String := code ++ s ++ Ansi.reset

def red (s : String) : String := color Ansi.red s
def green (s : String) : String := color Ansi.green s
def blue (s : String) : String := color Ansi.blue s
def cyan (s : String) : String := color Ansi.cyan s
def yellow (s : String) : String := color Ansi.yellow s
def magenta (s : String) : String := color Ansi.magenta s

/-- Split on ASCII whitespace, dropping empty runs. Replaces `str.split()` for tokenizing. -/
def words (s : String) : List String := Id.run do
  let mut out : List String := []
  let mut cur : List Char := []
  for c in s.toList do
    if c.isWhitespace then
      unless cur.isEmpty do out := out ++ [String.ofList cur.reverse]; cur := []
    else
      cur := c :: cur
  unless cur.isEmpty do out := out ++ [String.ofList cur.reverse]
  return out

/-- Lowercase two-digit hex for one byte. -/
def hexByte (b : UInt8) : String :=
  let s := Nat.toDigits 16 b.toNat |> String.ofList
  if s.length < 2 then "0" ++ s else s

/-- Lowercase hex string for a byte array, no separators (`b.hex()` in Python). -/
def toHex (bytes : ByteArray) : String :=
  String.join (bytes.toList.map hexByte)

/-- Parse a hex string into bytes. Rejects odd length and non-hex characters. -/
def ofHex (s : String) : Except String ByteArray := do
  let digit (c : Char) : Except String Nat :=
    let c := c.toLower
    if c.isDigit then pure (c.toNat - '0'.toNat)
    else if 'a' ≤ c && c ≤ 'f' then pure (c.toNat - 'a'.toNat + 10)
    else throw s!"not a hex digit: {c}"
  let chars := s.toList
  if chars.length % 2 != 0 then throw "hex needs an even number of digits"
  let rec go : List Char → Except String (List UInt8)
    | [] => pure []
    | hi :: lo :: rest => do
      pure ((16 * (← digit hi) + (← digit lo)).toUInt8 :: (← go rest))
    | _ => throw "unreachable"
  return ⟨(← go chars).toArray⟩

/-- Whether a byte is printable ASCII, else it shows as `.` in a dump. -/
private def isPrintable (b : UInt8) : Bool := b > 31 && b < 127

/--
Hex + ASCII block dump, one row per `blockSize` bytes, blocks numbered from 1.
Port of `print_mem_dump`.
-/
def printMemDump (data : ByteArray) (blockSize : Nat := 16) : IO Unit := do
  let hexWidth := blockSize * 3 + 1
  let asciiWidth := blockSize + 1
  let rule := s!"[=] ----+{String.ofList (List.replicate hexWidth '-')}+{String.ofList (List.replicate asciiWidth '-')}"
  IO.println rule
  IO.println s!"[=] blk | data{String.ofList (List.replicate (hexWidth - 5) ' ')}| ascii"
  IO.println rule
  let total := data.size
  let mut blk := 1
  let mut o := 0
  while o < total do
    let chunk := data.extract o (min (o + blockSize) total)
    let hexStr := String.intercalate " " (chunk.toList.map (fun b => (hexByte b).toUpper))
    let asciiStr := String.ofList (chunk.toList.map fun b => if isPrintable b then Char.ofNat b.toNat else '.')
    let idx := toString blk
    let idxPad := String.ofList (List.replicate (3 - min 3 idx.length) ' ') ++ idx
    IO.println s!"[=] {idxPad} | {hexStr} | {asciiStr} "
    blk := blk + 1
    o := o + blockSize

end Chamelean.Cli
