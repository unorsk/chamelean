/-!
Crypto1, the stream cipher in MIFARE Classic.

A 48-bit LFSR with a non-linear output filter. Port of `crypto1.py`; used by the
key-recovery paths (`mfkey32`, darkside/nested verification). All arithmetic is done
in `UInt64` and masked where the reference relies on Python's unbounded ints.

Reference: Nohl et al., "Reverse-Engineering a Cryptographic RFID Tag",
<https://sar.informatik.hu-berlin.de/research/publications/SAR-PR-2008-21/>.
-/
namespace Chamelean

/-- Output-filter lookup tables, indexed by a 5-bit / 4-bit tap selection. -/
private def filterA : UInt64 := 0x9E98
private def filterB : UInt64 := 0xB48E
private def filterC : UInt64 := 0xEC57E80A
/-- Feedback polynomial taps of the 48-bit LFSR. -/
private def poly : UInt64 := 0xE882B0AD621

/-- Bit `x` of `num`, as 0 or 1. -/
private def getBit (num : UInt64) (x : UInt64) : UInt64 := (num >>> x) &&& 1

/-- Gather the odd-indexed bits (1,3,5,7) of a byte into a nibble; the filter's tap picker. -/
private def u8ToOdd4 (u8 : UInt64) : UInt64 :=
  let u8 := u8 &&& 0xFF
  ((u8 &&& 0x80) >>> 4) + ((u8 &&& 0x20) >>> 3) + ((u8 &&& 0x08) >>> 2) + ((u8 &&& 0x02) >>> 1)

/-- Even parity of a byte, as 0 or 1. -/
def evenParityU8 (u8 : UInt64) : UInt64 :=
  let t := u8 &&& 0xFF
  let t := t ^^^ (t >>> 4)
  let t := t ^^^ (t >>> 2)
  (t ^^^ (t >>> 1)) &&& 1

def evenParityU16 (u16 : UInt64) : UInt64 := evenParityU8 ((u16 >>> 8) ^^^ u16)

def evenParityU48 (u48 : UInt64) : UInt64 :=
  evenParityU16 ((u48 >>> 32) ^^^ (u48 >>> 16) ^^^ u48)

/-- Swap the two bytes of a 16-bit value. -/
def swapEndianU16 (u16 : UInt64) : UInt64 :=
  ((u16 &&& 0xFF) <<< 8) ||| ((u16 >>> 8) &&& 0xFF)

/-- Swap the four bytes of a 32-bit value. -/
def swapEndianU32 (u32 : UInt64) : UInt64 :=
  (swapEndianU16 (u32 &&& 0xFFFF) <<< 16) ||| swapEndianU16 ((u32 >>> 16) &&& 0xFFFF)

/-- The cipher state: a 48-bit LFSR. -/
structure Crypto1 where
  lfsr48 : UInt64 := 0
deriving Inhabited, Repr

namespace Crypto1

/--
Load a 6-byte key. `key[0]` is the most-significant byte on the wire; the LFSR stores it
byte-reversed, so `key[0]` lands in the low bits (matching `crypto1.py`'s setter).
-/
def ofKey (key : ByteArray) : Crypto1 := Id.run do
  let mut lfsr : UInt64 := 0
  for b in key.toList.reverse do
    lfsr := (lfsr <<< 8) ||| b.toUInt64
  return { lfsr48 := lfsr }

/-- The 6-byte key currently held in the LFSR, inverse of `ofKey`. -/
def key (c : Crypto1) : ByteArray := Id.run do
  let mut tmp := c.lfsr48
  let mut out := ByteArray.empty
  for _ in [0:6] do
    out := out.push tmp.toUInt8
    tmp := tmp >>> 8
  return out

/-- The non-linear filter output bit for the current state. -/
def filter (c : Crypto1) : UInt64 :=
  let l := c.lfsr48
  let f :=
    getBit filterB (u8ToOdd4 (l >>> 8))
    ||| (getBit filterA (u8ToOdd4 (l >>> 16)) <<< 1)
    ||| (getBit filterA (u8ToOdd4 (l >>> 24)) <<< 2)
    ||| (getBit filterB (u8ToOdd4 (l >>> 32)) <<< 3)
    ||| (getBit filterA (u8ToOdd4 (l >>> 40)) <<< 4)
  getBit filterC f

/--
Clock the LFSR one bit. Returns the keystream bit and the advanced state. `bitIn` is mixed
into the feedback (nonzero only while absorbing a nonce); with `isEncrypted` the keystream
bit is fed back too, as the tag does when receiving ciphertext.
-/
def stepBit (c : Crypto1) (bitIn : UInt64 := 0) (isEncrypted : Bool := false) : Crypto1 × UInt64 :=
  let out := c.filter
  let enc := if isEncrypted then out else 0
  let feedback := evenParityU48 (poly &&& c.lfsr48) ^^^ (bitIn &&& 1) ^^^ enc
  ({ lfsr48 := (feedback <<< 47) ||| (c.lfsr48 >>> 1) }, out)

/-- Clock eight bits, LSB first, returning the keystream byte and the advanced state. -/
def stepU8 (c : Crypto1) (u8In : UInt64 := 0) (isEncrypted : Bool := false) : Crypto1 × UInt64 :=
  Id.run do
    let mut c := c
    let mut out : UInt64 := 0
    for i in [0:8] do
      let i := i.toUInt64
      let (c', bit) := c.stepBit (u8In >>> i) isEncrypted
      c := c'
      out := out ||| (bit <<< i)
    return (c, out)

/-- Clock 32 bits, most-significant byte first, returning the keystream word. -/
def stepU32 (c : Crypto1) (u32In : UInt64 := 0) (isEncrypted : Bool := false) : Crypto1 × UInt64 :=
  Id.run do
    let mut c := c
    let mut out : UInt64 := 0
    for i in [0:4] do
      let offset := ((3 - i) * 8).toUInt64
      let (c', b) := c.stepU8 (u32In >>> offset) isEncrypted
      c := c'
      out := out ||| (b <<< offset)
    return (c, out)

/-- Advance the MIFARE tag PRNG `n` times from a 32-bit state. -/
def prngNext (lfsr32 : UInt64) (n : Nat := 1) : UInt64 := Id.run do
  let mut l := swapEndianU32 lfsr32
  for _ in [0:n] do
    l := (evenParityU8 ((0x2D : UInt64) &&& (l >>> 16)) <<< 31) ||| (l >>> 1)
  return swapEndianU32 l

/--
`mfkey32`: does `key` reproduce the reader's encrypted authentication?

Replays one nested authentication (ks0 over `uid ⊕ nt`, ks1 over the encrypted reader nonce)
and checks the recovered `ar` equals `prng(nt, 64)`. `true` means the key is correct.
-/
def mfkey32HasKey (uid nt nrEnc arEnc : UInt64) (key : ByteArray) : Bool :=
  let c := ofKey key
  let (c, _) := c.stepU32 (uid ^^^ nt) false  -- ks0
  let (c, _) := c.stepU32 nrEnc true          -- ks1
  let (_, ks2) := c.stepU32 0 false           -- ks2
  (arEnc ^^^ ks2) == prngNext nt 64

end Crypto1

/-- Round-trip: a key loaded and read back is unchanged. -/
example : (Crypto1.ofKey (ByteArray.mk #[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])).key
    = ByteArray.mk #[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] := by native_decide

example : (Crypto1.ofKey (ByteArray.mk #[0x01, 0x02, 0x03, 0x04, 0x05, 0x06])).key
    = ByteArray.mk #[0x01, 0x02, 0x03, 0x04, 0x05, 0x06] := by native_decide

/-- Byte-swap is an involution on 32-bit values. -/
example : swapEndianU32 (swapEndianU32 0x11223344) = 0x11223344 := by native_decide

/-- Parity spot-checks. -/
example : evenParityU8 0x00 = 0 := by native_decide
example : evenParityU8 0x01 = 1 := by native_decide
example : evenParityU8 0xFF = 0 := by native_decide
example : evenParityU8 0x07 = 1 := by native_decide

end Chamelean
