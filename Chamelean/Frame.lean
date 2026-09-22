/-!
Chameleon Ultra wire format.

A frame is big-endian:

    SOF(0x11) LRC1 | CMD(2) STATUS(2) LEN(2) LRC2 | DATA(LEN) LRC3

Each LRC is the byte that makes the sum of every byte before it wrap to zero.
-/
namespace Chamelean

/-- Start-of-frame marker. -/
def sof : UInt8 := 0x11

/-- Largest payload the device accepts. -/
def maxDataLength : Nat := 4096

/-- Bytes before the payload: sof, lrc1, cmd, status, len, lrc2. -/
def headerSize : Nat := 9

/-- Longitudinal redundancy check: `(0x100 - sum) & 0xFF`, via wrapping UInt8 arithmetic. -/
def lrc (bytes : ByteArray) : UInt8 :=
  0 - bytes.foldl (· + ·) 0

def readU16 (bytes : ByteArray) (i : Nat) : UInt16 :=
  (bytes[i]!.toUInt16 <<< 8) ||| bytes[i + 1]!.toUInt16

def pushU16 (bytes : ByteArray) (v : UInt16) : ByteArray :=
  bytes.push (v >>> 8).toUInt8 |>.push v.toUInt8

def readU32 (bytes : ByteArray) (i : Nat) : UInt32 :=
  (bytes[i]!.toUInt32 <<< 24) ||| (bytes[i + 1]!.toUInt32 <<< 16) |||
  (bytes[i + 2]!.toUInt32 <<< 8) ||| bytes[i + 3]!.toUInt32

def pushU32 (bytes : ByteArray) (v : UInt32) : ByteArray :=
  bytes.push (v >>> 24).toUInt8 |>.push (v >>> 16).toUInt8
    |>.push (v >>> 8).toUInt8 |>.push v.toUInt8

structure Frame where
  cmd : UInt16
  status : UInt16 := 0
  data : ByteArray := .empty
deriving Inhabited

/-- Serialize a frame, computing all three LRC bytes. Caller must keep `data` within `maxDataLength`. -/
def Frame.encode (f : Frame) : ByteArray :=
  let head := ByteArray.empty.push sof
  let head := head.push (lrc head)
  let head := pushU16 (pushU16 (pushU16 head f.cmd) f.status) f.data.size.toUInt16
  let head := head.push (lrc head)
  let body := head ++ f.data
  body.push (lrc body)

/--
Incremental frame parser: feed one byte at a time. Any integrity failure drops the
partial frame and resynchronises on the next SOF, like the firmware's own parser.
-/
structure Decoder where
  buf : ByteArray := .empty
  /-- Payload length promised by the header, valid once the header has passed its LRC. -/
  len : Nat := 0

inductive Decoder.Event
  | none
  | frame (f : Frame)
  /-- A partial frame was discarded; carries the reason. -/
  | dropped (reason : String)

def Decoder.feed (d : Decoder) (b : UInt8) : Decoder × Decoder.Event :=
  let pos := d.buf.size
  let buf := d.buf.push b
  let drop reason := ({}, .dropped reason)
  if pos == 0 then
    if b == sof then ({ buf }, .none) else drop "no SOF byte"
  else if pos == 1 then
    if b == lrc d.buf then ({ buf }, .none) else drop "SOF lrc error"
  else if pos < headerSize - 1 then
    ({ buf }, .none)
  else if pos == headerSize - 1 then
    if b != lrc d.buf then drop "head lrc error"
    else
      let len := (readU16 buf 6).toNat
      if len > maxDataLength then drop s!"data length {len} larger than max"
      else ({ buf, len }, .none)
  else if pos == headerSize + d.len then
    if b == lrc d.buf then
      ({}, .frame { cmd := readU16 buf 2, status := readU16 buf 4,
                    data := buf.extract headerSize (headerSize + d.len) })
    else drop "global lrc error"
  else
    ({ buf, len := d.len }, .none)

end Chamelean
