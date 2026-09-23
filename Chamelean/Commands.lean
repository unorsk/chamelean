import Chamelean.Client
import Chamelean.Device

/-!
The full device-command surface: one `Client` method per `ChameleonCMD` method in the
Python client.

Each method packs its request payload, sends the command and hands back the raw `Response`.
Turning a `Response` into the nicely-typed result the Python `resp.parsed` produced (a dict,
a tuple, a hex string) is deliberately left to the caller for now; the handful of methods
that already parse (`hf14aScan`, the BLE-pairing pair, `getDeviceMode`) live in `Client.lean`.
Length and range checks that Python raised `ValueError` for are kept here, since sending a
malformed frame just wastes a round-trip.

Timeouts are milliseconds (the Python client counted in seconds); the long-running attack and
sniff commands set an explicit one, everything else takes `sendCmd`'s default.
-/
namespace Chamelean
namespace Client

open Command

/-! ## Packing helpers -/

private def be16 (v : UInt16) : ByteArray := pushU16 .empty v
private def be32 (v : UInt32) : ByteArray := pushU32 .empty v
private def b1 (x : UInt8) : ByteArray := ByteArray.mk #[x]
private def boolByte (b : Bool) : UInt8 := if b then 1 else 0
private def concat (parts : List ByteArray) : ByteArray := parts.foldl (· ++ ·) .empty

/-- Default T55xx write key, and the two legacy keys tried when reprogramming a blank tag.
Matches `new_key` / `old_keys` in the Python client. -/
def newKey : ByteArray := ByteArray.mk #[0x20, 0x20, 0x66, 0x66]
def oldKeys : ByteArray := ByteArray.mk #[0x51, 0x24, 0x36, 0x48, 0x19, 0x92, 0x04, 0x27]

/-- Payload for a "write LF id onto T55xx" command: id, then the write key and legacy keys. -/
private def t55Payload (id : ByteArray) : ByteArray := concat [id, newKey, oldKeys]

/-! ## Device -/

/-- Firmware version of the running application (two bytes: major, minor). -/
def getAppVersion (c : Client) : IO Response := c.sendCmd .getAppVersion

/-- The nRF chip's factory id. -/
def getDeviceChipId (c : Client) : IO Response := c.sendCmd .getDeviceChipId

/-- The device's BLE MAC address. -/
def getDeviceAddress (c : Client) : IO Response := c.sendCmd .getDeviceAddress

/-- Git describe string of the firmware build. -/
def getGitVersion (c : Client) : IO Response := c.sendCmd .getGitVersion

/-- `0` Ultra, `1` Lite. -/
def getDeviceModel (c : Client) : IO Response := c.sendCmd .getDeviceModel

/-- All persisted settings in one blob (see `get_device_settings` for the layout). -/
def getDeviceSettings (c : Client) : IO Response := c.sendCmd .getDeviceSettings

/-- Change device mode by raw mode byte; `setReaderMode` is the boolean wrapper. -/
def changeDeviceMode (c : Client) (mode : UInt8) : IO Response :=
  c.sendCmd .changeDeviceMode (b1 mode)

/-- Whether the device is in reader mode. Alias of `getDeviceMode`, matching the Python client. -/
def isDeviceReaderMode (c : Client) : IO Bool := c.getDeviceMode

/-- Reboot into DFU (bootloader) mode. Fire-and-forget: the device drops the link on reset. -/
def enterBootloader (c : Client) : IO Unit := c.post Command.enterBootloader.toUInt16

/-! ## HF reader — MIFARE Classic and ISO14443-A -/

/-- Whether the tag in the field is a MIFARE Classic (status `HF_TAG_OK`). -/
def mf1DetectSupport (c : Client) : IO Bool := do
  return (← c.sendCmd .mf1DetectSupport).status == Status.hfTagOk.toUInt16

/-- Detect the tag's PRNG class (static / weak / hard). -/
def mf1DetectPrng (c : Client) : IO Response := c.sendCmd .mf1DetectPrng

/-- Measure the nonce distance for a known key/block, for the nested attack. -/
def mf1DetectNtDist (c : Client) (block keyType : UInt8) (key : ByteArray) : IO Response :=
  c.sendCmd .mf1DetectNtDist (concat [b1 keyType, b1 block, key])

/-- Collect nested-attack nonces for a target block, given one known key. -/
def mf1NestedAcquire (c : Client) (block keyType : UInt8) (key : ByteArray)
    (targetBlock targetType : UInt8) : IO Response :=
  c.sendCmd .mf1NestedAcquire (concat [b1 keyType, b1 block, key, b1 targetType, b1 targetBlock])

/-- Collect darkside-attack parameters. `syncMax` also scales the timeout. -/
def mf1DarksideAcquire (c : Client) (targetBlock targetType : UInt8) (firstRecover : Bool)
    (syncMax : UInt8) : IO Response :=
  c.sendCmd .mf1DarksideAcquire
    (ByteArray.mk #[targetType, targetBlock, boolByte firstRecover, syncMax])
    (timeoutMs := syncMax.toNat * 10 * 1000)

/-- Verify one key against one block (`HF_TAG_OK` = key correct, `MF_ERR_AUTH` = wrong). -/
def mf1AuthOneKeyBlock (c : Client) (block keyType : UInt8) (key : ByteArray) : IO Response :=
  c.sendCmd .mf1AuthOneKeyBlock (concat [b1 keyType, b1 block, key])

/-- Read one 16-byte block using the given key. -/
def mf1ReadOneBlock (c : Client) (block keyType : UInt8) (key : ByteArray) : IO Response :=
  c.sendCmd .mf1ReadOneBlock (concat [b1 keyType, b1 block, key])

/-- Write one 16-byte block using the given key. -/
def mf1WriteOneBlock (c : Client) (block keyType : UInt8) (key blockData : ByteArray) : IO Response :=
  c.sendCmd .mf1WriteOneBlock (concat [b1 keyType, b1 block, key, blockData])

/-- Collect static-nested nonces (for tags with a static PRNG). -/
def mf1StaticNestedAcquire (c : Client) (block keyType : UInt8) (key : ByteArray)
    (targetBlock targetType : UInt8) : IO Response :=
  c.sendCmd .mf1StaticNestedAcquire
    (concat [b1 keyType, b1 block, key, b1 targetType, b1 targetBlock])

/-- Collect the encrypted-nonce list for the hardnested attack. -/
def mf1HardNestedAcquire (c : Client) (slow : Bool) (block keyType : UInt8) (key : ByteArray)
    (targetBlock targetType : UInt8) : IO Response :=
  c.sendCmd .mf1HardnestedAcquire
    (concat [b1 (boolByte slow), b1 keyType, b1 block, key, b1 targetType, b1 targetBlock])
    (timeoutMs := 30000)

/-- Collect static-encrypted-nested nonces using a known backdoor key. -/
def mf1StaticEncryptedNestedAcquire (c : Client) (backdoorKey : ByteArray)
    (sectorCount startingSector : UInt8) : IO Response :=
  c.sendCmd .mf1EncNestedAcquire (concat [backdoorKey, ByteArray.mk #[sectorCount, startingSector]])
    (timeoutMs := 30000)

/--
Increment / decrement / restore a MIFARE value block, writing the result to a destination
block. `operand` is the signed 32-bit amount, taken here already in two's-complement form.
-/
def mf1ManipulateValueBlock (c : Client) (srcBlock srcType : UInt8) (srcKey : ByteArray)
    (operator : MfcValueBlockOperator) (operand : UInt32) (dstBlock dstType : UInt8)
    (dstKey : ByteArray) : IO Response :=
  c.sendCmd .mf1ManipulateValueBlock
    (concat [b1 srcType, b1 srcBlock, srcKey, b1 operator.toUInt8, be32 operand,
             b1 dstType, b1 dstBlock, dstKey])

/-- Check a list of keys against the sectors selected by a 10-byte mask. -/
def mf1CheckKeysOfSectors (c : Client) (mask : ByteArray) (keys : List ByteArray) : IO Response := do
  if mask.size != 10 then throw <| IO.userError "mask must be 10 bytes"
  if keys.isEmpty || keys.length > 83 then throw <| IO.userError "need between 1 and 83 keys"
  c.sendCmd .mf1CheckKeysOfSectors (concat (mask :: keys)) (timeoutMs := 10000)

/-- Check a list of keys against one block for one key type (`0x60`/`0x61`). -/
def mf1CheckKeysOnBlock (c : Client) (block : UInt8) (keyType : MfcKeyType)
    (keys : List ByteArray) : IO Response := do
  if keys.isEmpty || keys.length > 83 then throw <| IO.userError "need between 1 and 83 keys"
  c.sendCmd .mf1CheckKeysOnBlock
    (concat (ByteArray.mk #[block, keyType.toUInt8, keys.length.toUInt8] :: keys))
    (timeoutMs := 10000)

/-! ## HF reader — raw exchange, sniff, config -/

/-- Flags controlling one `hf14aRaw` exchange, packed into the leading options byte. -/
structure Hf14aRawOptions where
  activateRfField : Bool := false
  waitResponse : Bool := false
  appendCrc : Bool := false
  autoSelect : Bool := false
  keepRfField : Bool := false
  checkResponseCrc : Bool := false
deriving Inhabited

/-- The options byte, MSB first, matching the firmware's bitfield. -/
private def Hf14aRawOptions.toByte (o : Hf14aRawOptions) : UInt8 :=
  (if o.activateRfField then 0x80 else 0) ||| (if o.waitResponse then 0x40 else 0) |||
  (if o.appendCrc then 0x20 else 0) ||| (if o.autoSelect then 0x10 else 0) |||
  (if o.keepRfField then 0x08 else 0) ||| (if o.checkResponseCrc then 0x04 else 0)

/--
Send a raw ISO14443-A exchange and return the response bytes. `bitlen` overrides the bit
count of the last byte (for 7-bit short frames such as Gen1a unlock); it must lie within the
provided data. Returns the raw reply payload, like the Python method.
-/
def hf14aRaw (c : Client) (opts : Hf14aRawOptions) (respTimeoutMs : Nat := 100)
    (data : ByteArray := .empty) (bitlen : Option Nat := none) : IO ByteArray := do
  let bits ← match bitlen with
    | none => pure (data.size * 8)
    | some n =>
      if data.isEmpty then throw <| IO.userError s!"bitlen={n} but no data given"
      if !((data.size - 1) * 8 < n && n ≤ data.size * 8) then
        throw <| IO.userError s!"bitlen={n} incompatible with {data.size} bytes of data"
      pure n
  let payload := concat [b1 opts.toByte, be16 respTimeoutMs.toUInt16, be16 bits.toUInt16, data]
  return (← c.sendCmd .hf14aRaw payload (timeoutMs := (respTimeoutMs / 1000 + 1) * 1000)).data

/-- Sniff reader→tag frames for `timeoutMs` (clamped to 1..30000). -/
def hf14aSniff (c : Client) (timeoutMs : UInt16 := 5000) : IO Response :=
  let ms := max 1 (min 30000 timeoutMs)
  c.sendCmd .hf14aSniff (be16 ms) (timeoutMs := (ms.toNat / 1000 + 5) * 1000)

/-- Run a full reader-side auth against a real card and return every wire frame. -/
def hf14aAuthTrace (c : Client) (block : UInt8) (keyType : MfcKeyType) (key : ByteArray)
    (timeoutMs : UInt16 := 5000) : IO Response := do
  if key.size != 6 then throw <| IO.userError "key must be exactly 6 bytes"
  let ms := max 1 (min 30000 timeoutMs)
  c.sendCmd .hf14aAuthTrace (concat [b1 keyType.toUInt8, b1 block, key, be16 ms])
    (timeoutMs := (ms.toNat / 1000 + 3) * 1000)

/-- Read the HF14A reader config (bcc / cl2 / cl3 / rats override bytes). -/
def hf14aGetConfig (c : Client) : IO Response := c.sendCmd .hf14aGetConfig

/-- Set the HF14A reader config; each byte is a signed override (`-1` = auto). -/
def hf14aSetConfig (c : Client) (bcc cl2 cl3 rats : UInt8) : IO Response :=
  c.sendCmd .hf14aSetConfig (ByteArray.mk #[bcc, cl2, cl3, rats])

/-! ## HF reader — ISO14443-4 T=CL -/

/-- Set UID/ATQA/SAK/ATS for the active HF14A-4 slot. -/
def hf14a4SetAntiColl (c : Client) (uid atqa : ByteArray) (sak : UInt8) (ats : ByteArray)
    : IO Response :=
  c.sendCmd .hf14a4SetAntiColl
    (concat [b1 uid.size.toUInt8, uid, atqa, b1 sak, b1 ats.size.toUInt8, ats])

/-- Non-blocking poll for a pending APDU from the T=CL stack. -/
def hf14a4ApduRecv (c : Client) : IO Response := c.sendCmd .hf14a4ApduRecv (timeoutMs := 2000)

/-- Send an APDU response into the T=CL stack. -/
def hf14a4ApduSend (c : Client) (resp : ByteArray) : IO Response :=
  c.sendCmd .hf14a4ApduSend (concat [be16 resp.size.toUInt16, resp])

/-- Register a static command→response APDU pair the firmware answers on its own. -/
def hf14a4AddStaticResponse (c : Client) (cmd resp : ByteArray) : IO Response :=
  c.sendCmd .hf14a4StaticResp
    (concat [b1 cmd.size.toUInt8, cmd, be16 resp.size.toUInt16, resp])

/-- Clear all static APDU responses from the active HF14A-4 slot. -/
def hf14a4ClearStaticResponses (c : Client) : IO Response :=
  c.sendCmd .hf14a4StaticResp (b1 0)

/-- Select the card and exchange one APDU in a single firmware call. -/
def hf14a4ReaderApdu (c : Client) (apdu : ByteArray) : IO Response :=
  c.sendCmd .hf14a4ReaderApdu apdu (timeoutMs := 3000)

/-- Run a full EMV scan (select → PPSE → GPO → read records) in one firmware call. -/
def hf14a4EmvScan (c : Client) : IO Response := c.sendCmd .hf14a4EmvScan (timeoutMs := 10000)

/-! ## LF reader -/

/-- Read an EM410x card id. -/
def em410xScan (c : Client) : IO Response := c.sendCmd .em410xScan

/-- Write an EM410x (5-byte) or Electra (13-byte) id onto a T55xx tag. -/
def em410xWriteToT55xx (c : Client) (id : ByteArray) : IO Response :=
  match id.size with
  | 5 => c.sendCmd .em410xWriteToT55xx (t55Payload id)
  | 13 => c.sendCmd .em410xElectraWriteToT55xx (t55Payload id)
  | _ => throw <| IO.userError "id length must be 5 (EM410X) or 13 (Electra)"

/-- Read HID Prox length / facility code / card number in the given Wiegand format. -/
def hidproxScan (c : Client) (format : HIDFormat) : IO Response :=
  c.sendCmd .hidproxScan (b1 format.toUInt8)

/-- Write a 13-byte HID Prox id onto a T55xx tag. -/
def hidproxWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 13 then throw <| IO.userError "id length must be 13"
  c.sendCmd .hidproxWriteToT55xx (t55Payload id)

/-- Read ioProx version / facility / number / raw. -/
def ioproxScan (c : Client) : IO Response := c.sendCmd .ioproxScan

/-- Write a 16-byte ioProx frame onto a T55xx tag. -/
def ioproxWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 16 then throw <| IO.userError "id length must be 16"
  c.sendCmd .ioproxWriteToT55xx (t55Payload id)

/-- Decode 8 raw ioProx bytes into the 16-byte card structure (firmware side). -/
def ioproxDecodeRaw (c : Client) (raw8 : ByteArray) : IO Response :=
  c.sendCmd .ioproxDecodeRaw raw8

/-- Encode ioProx version / facility / card number into the 16-byte structure. -/
def ioproxComposeId (c : Client) (ver fc : UInt8) (cn : UInt16) : IO Response :=
  c.sendCmd .ioproxComposeId (concat [ByteArray.mk #[ver, fc], be16 cn])

/-- Capture raw LF ADC samples for `timeoutMs` (clamped to 1..10000). -/
def lfSniff (c : Client) (timeoutMs : UInt16 := 2000) : IO Response :=
  let ms := max 1 (min 10000 timeoutMs)
  c.sendCmd .lfSniff (be16 ms) (timeoutMs := (ms.toNat / 1000 + 2) * 1000)

/-- Read an EM4x05 / EM4x69 tag, optionally logging in with a 32-bit password. -/
def em4x05Scan (c : Client) (pwd : UInt32 := 0) : IO Response :=
  c.sendCmd .em4x05Scan (be32 pwd)

/-- Read a Viking card id. -/
def vikingScan (c : Client) : IO Response := c.sendCmd .vikingScan

/-- Write a 4-byte Viking id onto a T55xx tag. -/
def vikingWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 4 then throw <| IO.userError "id length must be 4"
  c.sendCmd .vikingWriteToT55xx (t55Payload id)

/-- Read a PAC/Stanley card id. -/
def pacScan (c : Client) : IO Response := c.sendCmd .pacScan

/-- Write an 8-byte PAC/Stanley id onto a T55xx tag. -/
def pacWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 8 then throw <| IO.userError "id length must be 8"
  c.sendCmd .pacWriteToT55xx (t55Payload id)

/-- Read a Jablotron card id. -/
def jablotronScan (c : Client) : IO Response := c.sendCmd .jablotronScan

/-- Write a 5-byte Jablotron id onto a T55xx tag. -/
def jablotronWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 5 then throw <| IO.userError "id length must be 5"
  c.sendCmd .jablotronWriteToT55xx (t55Payload id)

/-- Write an 8-byte IDTECK frame onto a T55xx tag. -/
def idteckWriteToT55xx (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 8 then throw <| IO.userError "id length must be 8"
  c.sendCmd .idteckWriteToT55xx (t55Payload id)

/-- Read the raw ADC value while the field is on. -/
def adcGenericRead (c : Client) : IO Response := c.sendCmd .adcGenericRead

/-! ## Slots and slot metadata -/

/-- Per-slot HF/LF tag types for all eight slots. -/
def getSlotInfo (c : Client) : IO Response := c.sendCmd .getSlotInfo

/-- The currently active slot (0-based on the wire). -/
def getActiveSlot (c : Client) : IO Response := c.sendCmd .getActiveSlot

/-- The active slot's LF tag type, from `getSlotInfo` + `getActiveSlot`; `none` if unknown.
Port of the private `_get_active_lf_tag_type`. -/
def getActiveLfTagType (c : Client) : IO (Option TagSpecificType) := do
  let info := (← c.getSlotInfo).data
  let activeFw := (← c.getActiveSlot).data[0]?.getD 0  -- 0-based slot index
  return TagSpecificType.ofUInt16? (readU16 info (activeFw.toNat * 4 + 2))

/-- Select the active slot. -/
def setActiveSlot (c : Client) (slot : SlotNumber) : IO Response :=
  c.sendCmd .setActiveSlot (b1 slot.toFw)

/-- Set a slot's emulated tag type (RAM only; save to persist). -/
def setSlotTagType (c : Client) (slot : SlotNumber) (tagType : TagSpecificType) : IO Response :=
  c.sendCmd .setSlotTagType (concat [b1 slot.toFw, be16 tagType.toUInt16])

/-- Reset a slot's data to the default for a tag type (writes flash). -/
def setSlotDataDefault (c : Client) (slot : SlotNumber) (tagType : TagSpecificType) : IO Response :=
  c.sendCmd .setSlotDataDefault (concat [b1 slot.toFw, be16 tagType.toUInt16])

/-- Enable or disable one sense (HF/LF) of a slot. -/
def setSlotEnable (c : Client) (slot : SlotNumber) (sense : TagSenseType) (enabled : Bool)
    : IO Response :=
  c.sendCmd .setSlotEnable (ByteArray.mk #[slot.toFw, sense.toUInt8, boolByte enabled])

/-- Delete one sense type from a slot. -/
def deleteSlotSenseType (c : Client) (slot : SlotNumber) (sense : TagSenseType) : IO Response :=
  c.sendCmd .deleteSlotSenseType (ByteArray.mk #[slot.toFw, sense.toUInt8])

/-- Enabled-state of both senses for every slot. -/
def getEnabledSlots (c : Client) : IO Response := c.sendCmd .getEnabledSlots

/-- Persist all slot data and config to flash. -/
def slotDataConfigSave (c : Client) : IO Response := c.sendCmd .slotDataConfigSave

/-- Set a slot's nickname for one sense (≤32 UTF-8 bytes). -/
def setSlotTagNick (c : Client) (slot : SlotNumber) (sense : TagSenseType) (name : String)
    : IO Response := do
  let encoded := name.toUTF8
  if encoded.size > 32 then throw <| IO.userError "tag nick name too long (max 32 bytes)"
  c.sendCmd .setSlotTagNick (concat [ByteArray.mk #[slot.toFw, sense.toUInt8], encoded])

/-- Get a slot's nickname for one sense. -/
def getSlotTagNick (c : Client) (slot : SlotNumber) (sense : TagSenseType) : IO Response :=
  c.sendCmd .getSlotTagNick (ByteArray.mk #[slot.toFw, sense.toUInt8])

/-- Delete a slot's nickname for one sense. -/
def deleteSlotTagNick (c : Client) (slot : SlotNumber) (sense : TagSenseType) : IO Response :=
  c.sendCmd .deleteSlotTagNick (ByteArray.mk #[slot.toFw, sense.toUInt8])

/-- All slot nicknames in one blob (HF then LF, length-prefixed). -/
def getAllSlotNicks (c : Client) : IO Response := c.sendCmd .getAllSlotNicks

/-! ## LF emulated ids -/

/-- Set the emulated EM410x id (5-byte EM410X or 13-byte Electra). -/
def em410xSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 5 && id.size != 13 then throw <| IO.userError "id length must be 5 or 13"
  c.sendCmd .em410xSetEmuId id

/-- Get the emulated EM410x id. -/
def em410xGetEmuId (c : Client) : IO Response := c.sendCmd .em410xGetEmuId

/-- Set the emulated 13-byte HID Prox id. -/
def hidproxSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 13 then throw <| IO.userError "id length must be 13"
  c.sendCmd .hidproxSetEmuId id

/-- Get the emulated HID Prox id. -/
def hidproxGetEmuId (c : Client) : IO Response := c.sendCmd .hidproxGetEmuId

/-- Set the emulated 16-byte ioProx id. -/
def ioproxSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 16 then throw <| IO.userError "id length must be 16"
  c.sendCmd .ioproxSetEmuId id

/-- Get the emulated ioProx id. -/
def ioproxGetEmuId (c : Client) : IO Response := c.sendCmd .ioproxGetEmuId

/-- Set the emulated 8-byte IDTECK frame. -/
def idteckSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 8 then throw <| IO.userError "id length must be 8"
  c.sendCmd .idteckSetEmuId id

/-- Get the emulated IDTECK frame. -/
def idteckGetEmuId (c : Client) : IO Response := c.sendCmd .idteckGetEmuId

/-- Set the emulated 4-byte Viking id. -/
def vikingSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 4 then throw <| IO.userError "id length must be 4"
  c.sendCmd .vikingSetEmuId id

/-- Get the emulated Viking id. -/
def vikingGetEmuId (c : Client) : IO Response := c.sendCmd .vikingGetEmuId

/-- Set the emulated 8-byte PAC/Stanley id. -/
def pacSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 8 then throw <| IO.userError "id length must be 8"
  c.sendCmd .pacSetEmuId id

/-- Get the emulated PAC/Stanley id. -/
def pacGetEmuId (c : Client) : IO Response := c.sendCmd .pacGetEmuId

/-- Set the emulated 5-byte Jablotron id. -/
def jablotronSetEmuId (c : Client) (id : ByteArray) : IO Response := do
  if id.size != 5 then throw <| IO.userError "id length must be 5"
  c.sendCmd .jablotronSetEmuId id

/-- Get the emulated Jablotron id. -/
def jablotronGetEmuId (c : Client) : IO Response := c.sendCmd .jablotronGetEmuId

/-! ## MIFARE Classic emulation and detection (mfkey32) -/

/-- Enable or disable mfkey32 nonce logging on the active slot. -/
def mf1SetDetectionEnable (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf1SetDetectionEnable (b1 (boolByte enabled))

/-- Number of logged detection records. -/
def mf1GetDetectionCount (c : Client) : IO Response := c.sendCmd .mf1GetDetectionCount

/-- Detection records starting at `index`. -/
def mf1GetDetectionLog (c : Client) (index : UInt32) : IO Response :=
  c.sendCmd .mf1GetDetectionLog (be32 index)

/-- Write emulator block data starting at `blockStart` (may span several blocks). -/
def mf1WriteEmuBlockData (c : Client) (blockStart : UInt8) (blockData : ByteArray) : IO Response :=
  c.sendCmd .mf1WriteEmuBlockData (concat [b1 blockStart, blockData])

/-- Read `blockCount` emulator blocks starting at `blockStart`. -/
def mf1ReadEmuBlockData (c : Client) (blockStart blockCount : UInt8) : IO Response :=
  c.sendCmd .mf1ReadEmuBlockData (ByteArray.mk #[blockStart, blockCount])

/-- The five MIFARE Classic emulator flags in one call. -/
def mf1GetEmulatorConfig (c : Client) : IO Response := c.sendCmd .mf1GetEmulatorConfig

/-- Toggle Gen1a magic mode. -/
def mf1SetGen1aMode (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf1SetGen1aMode (b1 (boolByte enabled))

/-- Toggle Gen2 magic mode. -/
def mf1SetGen2Mode (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf1SetGen2Mode (b1 (boolByte enabled))

/-- Toggle taking UID/BCC/SAK/ATQA from block 0. -/
def mf1SetBlockAntiCollMode (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf1SetBlockAntiCollMode (b1 (boolByte enabled))

/-- Set the emulator write mode. -/
def mf1SetWriteMode (c : Client) (mode : WriteMode) : IO Response :=
  c.sendCmd .mf1SetWriteMode (b1 mode.toUInt8)

/-- Get the emulated PRNG type. -/
def mf1GetPrngType (c : Client) : IO Response := c.sendCmd .mf1GetPrngType

/-- Set the emulated PRNG type (static / weak / hard). -/
def mf1SetPrngType (c : Client) (prng : MifareClassicPrngType) : IO Response :=
  c.sendCmd .mf1SetPrngType (b1 prng.toUInt8)

/-- Whether the emulator resets crypto state when the field drops. -/
def mf1GetFieldOffDoReset (c : Client) : IO Response := c.sendCmd .mf1GetFieldOffDoReset

/-- Set whether the emulator resets crypto state when the field drops. -/
def mf1SetFieldOffDoReset (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf1SetFieldOffDoReset (b1 (boolByte enabled))

/-! ## MIFARE Ultralight / NTAG emulation -/

/-- Whether NTAG password detection is on. -/
def mf0NtagGetDetectionEnable (c : Client) : IO Response := c.sendCmd .mf0NtagGetDetectionEnable

/-- Enable or disable NTAG password detection. -/
def mf0NtagSetDetectionEnable (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf0NtagSetDetectionEnable (b1 (boolByte enabled))

/-- Number of logged NTAG password records. -/
def mf0NtagGetDetectionCount (c : Client) : IO Response := c.sendCmd .mf0NtagGetDetectionCount

/-- NTAG password records starting at `index`. -/
def mf0NtagGetDetectionLog (c : Client) (index : UInt32) : IO Response :=
  c.sendCmd .mf0NtagGetDetectionLog (be32 index)

/-- Number of emulated pages in the current MF0/NTAG slot. -/
def mfuGetEmuPagesCount (c : Client) : IO Response := c.sendCmd .mf0NtagGetPageCount

/-- Read `pageCount` emulated pages starting at `pageStart`. -/
def mfuReadEmuPageData (c : Client) (pageStart pageCount : UInt8) : IO Response :=
  c.sendCmd .mf0NtagReadEmuPageData (ByteArray.mk #[pageStart, pageCount])

/-- Write emulated page data (a whole number of 4-byte pages) starting at `pageStart`. -/
def mfuWriteEmuPageData (c : Client) (pageStart : UInt8) (data : ByteArray) : IO Response := do
  if data.size % 4 != 0 then throw <| IO.userError "page data must be a multiple of 4 bytes"
  let count := data.size / 4
  if pageStart.toNat + count > 256 then throw <| IO.userError "page range exceeds 256"
  c.sendCmd .mf0NtagWriteEmuPageData (concat [ByteArray.mk #[pageStart, count.toUInt8], data])

/-- Read one NTAG counter (the firmware packs its value little-endian). -/
def mfuReadEmuCounterData (c : Client) (index : UInt8) : IO Response :=
  c.sendCmd .mf0NtagGetCounterData (b1 index)

/-- Set one NTAG counter, optionally clearing its tearing flag. -/
def mfuWriteEmuCounterData (c : Client) (index : UInt8) (value : UInt32) (resetTearing : Bool)
    : IO Response :=
  c.sendCmd .mf0NtagSetCounterData
    (ByteArray.mk #[index ||| (boolByte resetTearing <<< 7),
      value.toUInt8, (value >>> 8).toUInt8, (value >>> 16).toUInt8])

/-- Reset the NTAG authentication counter. -/
def mfuResetAuthCnt (c : Client) : IO Response := c.sendCmd .mf0NtagResetAuthCnt

/-- Whether the MF0/NTAG UID magic mode is on. -/
def mf0NtagGetUidMagicMode (c : Client) : IO Response := c.sendCmd .mf0NtagGetUidMagicMode

/-- Toggle the MF0/NTAG UID magic mode. -/
def mf0NtagSetUidMagicMode (c : Client) (enabled : Bool) : IO Response :=
  c.sendCmd .mf0NtagSetUidMagicMode (b1 (boolByte enabled))

/-- Get the emulated 8-byte GET_VERSION response. -/
def mf0NtagGetVersionData (c : Client) : IO Response := c.sendCmd .mf0NtagGetVersionData

/-- Set the emulated 8-byte GET_VERSION response. -/
def mf0NtagSetVersionData (c : Client) (data : ByteArray) : IO Response := do
  if data.size != 8 then throw <| IO.userError "version data must be 8 bytes"
  c.sendCmd .mf0NtagSetVersionData data

/-- Get the emulated 32-byte signature. -/
def mf0NtagGetSignatureData (c : Client) : IO Response := c.sendCmd .mf0NtagGetSignatureData

/-- Set the emulated 32-byte signature. -/
def mf0NtagSetSignatureData (c : Client) (data : ByteArray) : IO Response := do
  if data.size != 32 then throw <| IO.userError "signature data must be 32 bytes"
  c.sendCmd .mf0NtagSetSignatureData data

/-- Get the MF0/NTAG emulator write mode. -/
def mf0NtagGetWriteMode (c : Client) : IO Response := c.sendCmd .mf0NtagGetWriteMode

/-- Set the MF0/NTAG emulator write mode. -/
def mf0NtagSetWriteMode (c : Client) (mode : WriteMode) : IO Response :=
  c.sendCmd .mf0NtagSetWriteMode (b1 mode.toUInt8)

/-! ## HF14A anti-collision data (emulation) -/

/-- Set the active HF slot's anti-collision data (UID/ATQA/SAK/ATS). -/
def hf14aSetAntiCollData (c : Client) (uid atqa sak : ByteArray) (ats : ByteArray := .empty)
    : IO Response :=
  c.sendCmd .hf14aSetAntiCollData
    (concat [b1 uid.size.toUInt8, uid, atqa, sak, b1 ats.size.toUInt8, ats])

/-- Get the active HF slot's anti-collision data. -/
def hf14aGetAntiCollData (c : Client) : IO Response := c.sendCmd .hf14aGetAntiCollData

/-! ## SEOS emulation -/

/-- Read the emulated SEOS data (data/oid/tag/diversifier + hash/encr algorithm bytes). -/
def seosReadEmuData (c : Client) : IO Response := c.sendCmd .seosReadEmuData

/-- Write the emulated SEOS data. Each of the four blobs is length-prefixed. -/
def seosWriteEmuData (c : Client) (data oid tag diversifier : ByteArray) (hashAlg encrAlg : UInt8)
    : IO Response := do
  let lp (x : ByteArray) : ByteArray := concat [b1 x.size.toUInt8, x]
  let payload := concat [lp data, lp oid, lp tag, lp diversifier, ByteArray.mk #[hashAlg, encrAlg]]
  if payload.size > 4096 then throw <| IO.userError "too much SEOS data"
  c.sendCmd .seosWriteEmuData payload

/-- Write the three SEOS keys (auth / privenc / privmac), concatenated. -/
def seosWriteEmuKeys (c : Client) (auth privenc privmac : ByteArray) : IO Response :=
  c.sendCmd .seosWriteEmuKeys (concat [auth, privenc, privmac])

/-! ## Settings and device config -/

/-- Get the LED animation mode. -/
def getAnimationMode (c : Client) : IO Response := c.sendCmd .getAnimationMode

/-- Set the LED animation mode. -/
def setAnimationMode (c : Client) (mode : AnimationMode) : IO Response :=
  c.sendCmd .setAnimationMode (b1 mode.toUInt8)

/-- Get the post-wakeup sleep timeout in seconds. -/
def getSleepTimeout (c : Client) : IO Response := c.sendCmd .getSleepTimeout

/-- Set the post-wakeup sleep timeout in seconds. -/
def setSleepTimeout (c : Client) (seconds : UInt8) : IO Response :=
  c.sendCmd .setSleepTimeout (b1 seconds)

/-- Get a button's short-press function. -/
def getButtonPressConfig (c : Client) (button : ButtonType) : IO Response :=
  c.sendCmd .getButtonPressConfig (b1 button.toUInt8)

/-- Set a button's short-press function. -/
def setButtonPressConfig (c : Client) (button : ButtonType) (fn : ButtonPressFunction)
    : IO Response :=
  c.sendCmd .setButtonPressConfig (ByteArray.mk #[button.toUInt8, fn.toUInt8])

/-- Get a button's long-press function. -/
def getLongButtonPressConfig (c : Client) (button : ButtonType) : IO Response :=
  c.sendCmd .getLongButtonPressConfig (b1 button.toUInt8)

/-- Set a button's long-press function. -/
def setLongButtonPressConfig (c : Client) (button : ButtonType) (fn : ButtonPressFunction)
    : IO Response :=
  c.sendCmd .setLongButtonPressConfig (ByteArray.mk #[button.toUInt8, fn.toUInt8])

/-- Set the 6-character ASCII BLE pairing key. -/
def setBleConnectKey (c : Client) (key : String) : IO Response := do
  let bytes := key.toUTF8
  if bytes.size != 6 then throw <| IO.userError "BLE connect key must be 6 characters"
  c.sendCmd .setBlePairingKey bytes

/-- Delete all BLE bonds from the peer manager. -/
def deleteAllBleBonds (c : Client) : IO Response := c.sendCmd .deleteAllBleBonds

/-- Battery voltage and percentage. -/
def getBatteryInfo (c : Client) : IO Response := c.sendCmd .getBatteryInfo

/-- Persist settings to flash. -/
def saveSettings (c : Client) : IO Response := c.sendCmd .saveSettings

/-- Reset settings held in flash. -/
def resetSettings (c : Client) : IO Response := c.sendCmd .resetSettings

/-- Factory reset (wipe FDS), then drop the link as the device reboots. -/
def wipeFds (c : Client) : IO Response := do
  let r ← c.sendCmd .wipeFds
  c.close
  return r

end Client
end Chamelean
