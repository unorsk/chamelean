import Chamelean.Enum

/-! Device-side enums: slots, tag types, emulation modes, buttons and card formats. -/
namespace Chamelean

/-- The eight slots as the UI numbers them. The firmware counts from zero: see `toFw`. -/
uint_enum SlotNumber : UInt8 where
  | slot1 := 1
  | slot2 := 2
  | slot3 := 3
  | slot4 := 4
  | slot5 := 5
  | slot6 := 6
  | slot7 := 7
  | slot8 := 8

/-- Slot index as the firmware expects it, i.e. 0-based. -/
def SlotNumber.toFw (s : SlotNumber) : UInt8 := s.toUInt8 - 1

/-- Read a 0-based slot index coming from the firmware. -/
def SlotNumber.ofFw? (index : UInt8) : Option SlotNumber := SlotNumber.ofUInt8? (index + 1)

uint_enum TagSenseType : UInt8 where
  | undefined := 0 => "Undefined"
  | lf := 1 => "125 kHz"
  | hf := 2 => "13.56 MHz"

/--
Tag types a slot can hold.

Three constructors are markers rather than tags: `oldTagTypesEnd` and `tagTypesLfEnd` bound
the ranges, and everything below `oldTagTypesEnd` is a pre-migration type that only appears
when reading an old slot. `usable` filters them out.
-/
uint_enum TagSpecificType : UInt16 where
  | undefined := 0 => "Undefined"
  -- Old HF/LF common types. Slots using these must be migrated by newer firmware first.
  | oldEm410x := 1 => "Old tag type, must be migrated! Upgrade fw!"
  | oldMifareMini := 2 => "Old tag type, must be migrated! Upgrade fw!"
  | oldMifare1024 := 3 => "Old tag type, must be migrated! Upgrade fw!"
  | oldMifare2048 := 4 => "Old tag type, must be migrated! Upgrade fw!"
  | oldMifare4096 := 5 => "Old tag type, must be migrated! Upgrade fw!"
  | oldNtag213 := 6 => "Old tag type, must be migrated! Upgrade fw!"
  | oldNtag215 := 7 => "Old tag type, must be migrated! Upgrade fw!"
  | oldNtag216 := 8 => "Old tag type, must be migrated! Upgrade fw!"
  | oldTagTypesEnd := 9 => "Invalid"
  -- LF, ASK tag-talk-first (100)
  | em410x := 100 => "EM410X"
  | em410x16 := 101 => "EM410X/16"
  | em410x32 := 102 => "EM410X/32"
  | em410x64 := 103 => "EM410X/64"
  | em410xElectra := 104 => "EM410X Electra"
  | pac := 150 => "PAC/Stanley"
  | viking := 170 => "Viking"
  | jablotron := 180 => "Jablotron"
  -- LF, FSK tag-talk-first (200)
  | hidProx := 200 => "HIDProx"
  | ioProx := 201 => "ioProx"
  -- LF, PSK tag-talk-first (300)
  | idteck := 310 => "IDTECK"
  | tagTypesLfEnd := 999 => "Invalid"
  -- HF, MIFARE Classic series (1000)
  | mifareMini := 1000 => "Mifare Mini"
  | mifare1024 := 1001 => "Mifare Classic 1k"
  | mifare2048 := 1002 => "Mifare Classic 2k"
  | mifare4096 := 1003 => "Mifare Classic 4k"
  -- HF, MFUL / NTAG series (1100)
  | ntag213 := 1100 => "NTAG 213"
  | ntag215 := 1101 => "NTAG 215"
  | ntag216 := 1102 => "NTAG 216"
  | mf0icu1 := 1103 => "Mifare Ultralight"
  | mf0icu2 := 1104 => "Mifare Ultralight C"
  | mf0ul11 := 1105 => "Mifare Ultralight EV1 (640 bit)"
  | mf0ul21 := 1106 => "Mifare Ultralight EV1 (1312 bit)"
  | ntag210 := 1107 => "NTAG 210"
  | ntag212 := 1108 => "NTAG 212"
  -- HF, ISO14443-4 T=CL emulation
  | hf14a4 := 3000 => "HF14A-4"
  | seos := 3001 => "SEOS"

namespace TagSpecificType

/-- A pre-migration type; a slot holding one needs a firmware migration first. -/
def isOld (t : TagSpecificType) : Bool :=
  oldEm410x.toUInt16 ≤ t.toUInt16 && t.toUInt16 < oldTagTypesEnd.toUInt16

/-- Not a type a slot can be set to: `undefined`, a pre-migration type, or a range marker. -/
def isMeta (t : TagSpecificType) : Bool :=
  t.toUInt16 ≤ oldTagTypesEnd.toUInt16 || t == tagTypesLfEnd

/-- Tag types a slot can actually be set to, markers and old types excluded. -/
def usable : Array TagSpecificType := all.filter (!·.isMeta)

def usableLf : Array TagSpecificType :=
  usable.filter fun t => undefined.toUInt16 < t.toUInt16 && t.toUInt16 < tagTypesLfEnd.toUInt16

def usableHf : Array TagSpecificType :=
  usable.filter fun t => t.toUInt16 > tagTypesLfEnd.toUInt16

end TagSpecificType

/--
What the emulator does with a write from a reader. Used for both MIFARE Classic
(`mf1SetWriteMode`) and MIFARE Ultralight (`mf0NtagSetWriteMode`); the codes are the same.
-/
uint_enum WriteMode : UInt8 where
  /-- Normal write. -/
  | normal := 0 => "Normal"
  /-- Send NACK to write attempts. -/
  | denied := 1 => "Denied"
  /-- Acknowledge writes, but don't remember contents. -/
  | deceive := 2 => "Deceive"
  /-- Store data to RAM, but not to ROM. -/
  | shadow := 3 => "Shadow"
  /-- Shadow requested: becomes `shadow` once stored to ROM. -/
  | shadowReq := 4 => "Shadow requested"

/-- Modes a caller may set; `shadowReq` is a transient the firmware sets itself. -/
def WriteMode.settable : Array WriteMode := all.filter (· != shadowReq)

@[inherit_doc WriteMode] abbrev MifareClassicWriteMode := WriteMode
@[inherit_doc WriteMode] abbrev MifareUltralightWriteMode := WriteMode

/-- How predictable a MIFARE Classic tag's nonces are, which decides the viable attack. -/
uint_enum MifareClassicPrngType : UInt8 where
  /-- The random number of the card response is fixed. -/
  | static := 0 => "Static"
  /-- The random number of the card response is weak. -/
  | weak := 1 => "Weak"
  /-- The random number of the card response is unpredictable. -/
  | hard := 2 => "Hard"

uint_enum MifareClassicDarksideStatus : UInt8 where
  | ok := 0 => "Success"
  /-- Darkside can't fix NT (PRNG is unpredictable). -/
  | cantFixNt := 1 => "Cannot fix NT (unpredictable PRNG)"
  /-- Darkside is trying to recover a default key. -/
  | luckyAuthOk := 2 => "Try to recover a default key"
  /-- Darkside can't get the tag response enc(nak). -/
  | noNakSent := 3 => "Cannot get tag response enc(nak)"
  /-- The tag was swapped or moved while the attack was running. -/
  | tagChanged := 4 => "Tag changed during attack"

uint_enum AnimationMode : UInt8 where
  | full := 0 => "Full animation"
  | minimal := 1 => "Minimal animation"
  | none := 2 => "No animation"
  | symmetric := 3 => "Symmetric animation"

/-- The two physical buttons, coded by their ASCII letter. -/
uint_enum ButtonType : UInt8 where
  | a := 0x41 => "A"
  | b := 0x42 => "B"

/-- MIFARE Classic key slot, as sent in authentication commands. -/
uint_enum MfcKeyType : UInt8 where
  | a := 0x60 => "A"
  | b := 0x61 => "B"

/-- What a (long) button press does, configurable per button. -/
uint_enum ButtonPressFunction : UInt8 where
  | none := 0 => "No Function"
  | nextSlot := 1 => "Select next slot"
  | prevSlot := 2 => "Select previous slot"
  | clone := 3 => "Read then simulate the ID/UID card number"
  | battery := 4 => "Show Battery Level"
  | fieldGen := 5 => "Toggle NFC Field Generator"

uint_enum MfcValueBlockOperator : UInt8 where
  | decrement := 0xC0 => "Decrement"
  | increment := 0xC1 => "Increment"
  | restore := 0xC2 => "Restore"

/-- Wiegand formats the HID Prox encoder understands. -/
uint_enum HIDFormat : UInt8 where
  | h10301 := 1 => "HID H10301 26-bit"
  | ind26 := 2 => "Indala 26-bit"
  | ind27 := 3 => "Indala 27-bit"
  | indasc27 := 4 => "Indala ASC 27-bit"
  | tecom27 := 5 => "Tecom 27-bit"
  | w2804 := 6 => "2804 Wiegand 28-bit"
  | ind29 := 7 => "Indala 29-bit"
  | atsw30 := 8 => "ATS Wiegand 30-bit"
  | adt31 := 9 => "HID ADT 31-bit"
  | hcp32 := 10 => "HID Check Point 32-bit"
  | hpp32 := 11 => "HID Hewlett-Packard 32-bit"
  | kastle := 12 => "Kastle 32-bit"
  | kantech := 13 => "Indala/Kantech KFS 32-bit"
  | wie32 := 14 => "Wiegand 32-bit"
  | d10202 := 15 => "HID D10202 33-bit"
  | h10306 := 16 => "HID H10306 34-bit"
  | n10002 := 17 => "Honeywell/Northern N10002 34-bit"
  | optus34 := 18 => "Indala Optus 34-bit"
  | smp34 := 19 => "Cardkey Smartpass 34-bit"
  | bqt34 := 20 => "BQT 34-bit"
  | c1k35s := 21 => "HID Corporate 1000 35-bit Std"
  | c15001 := 22 => "HID KeyScan 36-bit"
  | s12906 := 23 => "HID Simplex 36-bit"
  | sie36 := 24 => "HID 36-bit Siemens"
  | h10320 := 25 => "HID H10320 37-bit BCD"
  | h10302 := 26 => "HID H10302 37-bit huge ID"
  | h10304 := 27 => "HID H10304 37-bit"
  | p10004 := 28 => "HID P10004 37-bit PCSC"
  | hgen37 := 29 => "HID Generic 37-bit"
  | mdi37 := 30 => "PointGuard MDI 37-bit"
  | actphid := 42 => "HID ACTProx 36-bit"

end Chamelean
