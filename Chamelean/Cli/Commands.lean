import Chamelean.Cli.Tree
import Chamelean.Cli.Pretty
import Chamelean.Commands

/-!
The bulk of the command tree: the `hw` extras, `hf` and `lf` groups.

Every `ChameleonCMD` method has a leaf here so the whole surface is navigable and shows up in
`dump_help`. A handful are wired to the device (the ones backed by a parsed `Client` method);
the rest are `todoLeaf` placeholders that print "not implemented yet" and tolerate whatever
arguments they will eventually take. Filling one in means swapping its `todoLeaf` for a real
`mkLeaf` that calls the matching `Client` method and formats the response.

`Repl.lean` owns the root and the `hw connect/disconnect/version` leaves; it splices these
groups in.
-/
namespace Chamelean.Cli

open Chamelean

/-- Build a leaf command node. -/
def mkLeaf (name help : String) (parser : ArgParser) (run : ReplState → Args → IO Unit) : CliTree :=
  .leaf name help { parser, run }

/-- A registered-but-unimplemented command. Navigable and self-describing; ignores its args. -/
def todoLeaf (name help : String) : CliTree :=
  mkLeaf name help { description := help, allowUnknown := true } fun _ _ =>
    IO.println (yellow s!"{name}: not implemented yet")

/-- A named group of children. -/
private def grp (name help : String) (children : List CliTree) : CliTree :=
  .group name help children

/-! ## Wired commands -/

private def scanCmd : CliTree :=
  mkLeaf "scan" "Scan for ISO14443-A tags" { description := "Scan for ISO14443-A tags" } <|
    readerRequired fun c _ => do
      let tags ← c.hf14aScan
      if tags.isEmpty then IO.println "No tag found."
      for t in tags do
        IO.println s!"- UID : {toHex t.uid}"
        IO.println s!"  ATQA: {toHex t.atqa}  SAK: {hexByte t.sak}"
        unless t.ats.isEmpty do IO.println s!"  ATS : {toHex t.ats}"

private def chipIdCmd : CliTree :=
  mkLeaf "chipid" "Get device chip id" { description := "Get device chip id" } <|
    deviceRequired fun c _ => do IO.println (toHex (← c.getDeviceChipId).data)

private def addressCmd : CliTree :=
  mkLeaf "address" "Get device BLE address" { description := "Get device BLE address" } <|
    deviceRequired fun c _ => do IO.println (toHex (← c.getDeviceAddress).data)

private def modeCmd : CliTree :=
  mkLeaf "mode" "Show reader/tag mode" { description := "Show reader/tag mode" } <|
    deviceRequired fun c _ => do
      IO.println (if ← c.getDeviceMode then "Reader mode" else "Tag/emulator mode")

private def batteryCmd : CliTree :=
  mkLeaf "battery" "Get battery info" { description := "Get battery info" } <|
    deviceRequired fun c _ => do
      let d := (← c.getBatteryInfo).data
      IO.println s!"Battery: {readU16 d 0} mV, {d[2]!}%"

private def dfuCmd : CliTree :=
  mkLeaf "dfu" "Reboot into bootloader (DFU)" { description := "Reboot into bootloader (DFU)" } <|
    deviceRequired fun c _ => do
      c.enterBootloader
      IO.println "Entering bootloader..."

private def blekeyGetCmd : CliTree :=
  mkLeaf "get" "Get BLE pairing key" { description := "Get BLE pairing key" } <|
    deviceRequired fun c _ => do IO.println s!"BLE pairing key: {← c.getBlePairingKey}"

private def blepairGetCmd : CliTree :=
  mkLeaf "get" "Is BLE pairing required" { description := "Is BLE pairing required" } <|
    deviceRequired fun c _ => do
      IO.println (if ← c.getBlePairingEnable then "enabled" else "disabled")

/-! ## `hw` extras (spliced into the `hw` group after connect/disconnect/version) -/

def hwExtras : List CliTree := [
  chipIdCmd, addressCmd, modeCmd, batteryCmd, dfuCmd,
  todoLeaf "model" "Get device model (Ultra/Lite)",
  todoLeaf "gitversion" "Get firmware git version",
  todoLeaf "factory_reset" "Wipe all data and settings (factory reset)",
  grp "settings" "Device settings" [
    todoLeaf "dump" "Dump all settings",
    todoLeaf "store" "Save settings to flash",
    todoLeaf "reset" "Reset settings in flash",
    grp "animation" "LED animation mode" [
      todoLeaf "get" "Get animation mode", todoLeaf "set" "Set animation mode" ],
    grp "sleep" "Post-wakeup sleep timeout" [
      todoLeaf "get" "Get sleep timeout", todoLeaf "set" "Set sleep timeout" ],
    grp "button" "Button press functions" [
      todoLeaf "get" "Get short-press function", todoLeaf "set" "Set short-press function",
      todoLeaf "longget" "Get long-press function", todoLeaf "longset" "Set long-press function" ],
    grp "blekey" "BLE pairing key" [ blekeyGetCmd, todoLeaf "set" "Set BLE pairing key" ],
    grp "blepair" "BLE pairing requirement" [ blepairGetCmd, todoLeaf "set" "Enable/disable BLE pairing" ],
    todoLeaf "blebonds_clear" "Delete all BLE bonds" ],
  grp "slot" "Card slot commands" [
    todoLeaf "list" "List slot tag types",
    todoLeaf "active" "Get active slot",
    todoLeaf "change" "Set active slot",
    todoLeaf "type" "Set slot tag type",
    todoLeaf "init" "Reset slot data to default",
    todoLeaf "enable" "Enable/disable a slot sense",
    todoLeaf "delete" "Delete a slot sense type",
    todoLeaf "enabled" "Show enabled slots",
    todoLeaf "store" "Save slot data/config to flash",
    grp "nick" "Slot nicknames" [
      todoLeaf "get" "Get slot nickname", todoLeaf "set" "Set slot nickname",
      todoLeaf "delete" "Delete slot nickname", todoLeaf "list" "List all slot nicknames" ] ] ]

/-! ## `hf` group -/

private def hfMfEconfig : CliTree :=
  grp "econfig" "MIFARE Classic emulator config" [
    todoLeaf "view" "Show emulator config",
    todoLeaf "gen1a" "Set Gen1a magic mode",
    todoLeaf "gen2" "Set Gen2 magic mode",
    todoLeaf "coll" "Set block-0 anti-collision mode",
    todoLeaf "write" "Set emulator write mode",
    grp "prng" "Emulated PRNG type" [ todoLeaf "get" "Get PRNG type", todoLeaf "set" "Set PRNG type" ],
    grp "fieldreset" "Reset crypto on field off" [
      todoLeaf "get" "Get field-off reset", todoLeaf "set" "Set field-off reset" ],
    grp "detection" "mfkey32 nonce logging" [
      todoLeaf "enable" "Enable/disable detection",
      todoLeaf "count" "Get detection count",
      todoLeaf "log" "Get detection log" ] ]

def hfGroup : CliTree :=
  grp "hf" "High-frequency (13.56 MHz) commands" [
    grp "14a" "ISO14443-A" [
      scanCmd,
      todoLeaf "scankeep" "Scan, keeping the RF field alive",
      todoLeaf "raw" "Send a raw ISO14443-A exchange",
      todoLeaf "sniff" "Sniff reader frames",
      grp "config" "Reader config" [ todoLeaf "get" "Get HF14A config", todoLeaf "set" "Set HF14A config" ],
      grp "anticoll" "Emulated anti-collision data" [
        todoLeaf "get" "Get anti-collision data", todoLeaf "set" "Set anti-collision data" ] ],
    grp "mf" "MIFARE Classic" [
      todoLeaf "info" "Detect MIFARE Classic support",
      todoLeaf "nt" "Detect PRNG type",
      todoLeaf "ntdist" "Detect nonce distance",
      todoLeaf "nested" "Nested attack: collect nonces",
      todoLeaf "staticnested" "Static-nested: collect nonces",
      todoLeaf "hardnested" "Hardnested: collect nonces",
      todoLeaf "encnested" "Static-encrypted-nested: collect nonces",
      todoLeaf "darkside" "Darkside: collect parameters",
      todoLeaf "auth" "Verify a key against a block",
      todoLeaf "rdbl" "Read one block",
      todoLeaf "wrbl" "Write one block",
      todoLeaf "value" "Increment/decrement/restore a value block",
      todoLeaf "check" "Check keys against masked sectors",
      todoLeaf "checkblk" "Check keys against one block",
      todoLeaf "authtrace" "Capture a full reader auth trace",
      todoLeaf "eread" "Read emulator block data",
      todoLeaf "eload" "Write emulator block data",
      hfMfEconfig ],
    grp "mfu" "MIFARE Ultralight / NTAG" [
      todoLeaf "pages" "Get emulator page count",
      todoLeaf "rdpg" "Read emulator pages",
      todoLeaf "wrpg" "Write emulator pages",
      todoLeaf "resetauth" "Reset authentication counter",
      grp "counter" "NTAG counters" [ todoLeaf "get" "Read a counter", todoLeaf "set" "Set a counter" ],
      grp "version" "GET_VERSION data" [ todoLeaf "get" "Get version data", todoLeaf "set" "Set version data" ],
      grp "signature" "Signature data" [ todoLeaf "get" "Get signature", todoLeaf "set" "Set signature" ],
      grp "uidmagic" "UID magic mode" [ todoLeaf "get" "Get UID magic mode", todoLeaf "set" "Set UID magic mode" ],
      grp "write" "Emulator write mode" [ todoLeaf "get" "Get write mode", todoLeaf "set" "Set write mode" ],
      grp "detection" "NTAG password logging" [
        todoLeaf "enable" "Enable/disable detection",
        todoLeaf "count" "Get detection count",
        todoLeaf "log" "Get detection log" ] ],
    grp "seos" "SEOS emulation" [
      todoLeaf "read" "Read emulated SEOS data",
      todoLeaf "write" "Write emulated SEOS data",
      todoLeaf "keys" "Write SEOS keys" ],
    grp "emv" "EMV / ISO14443-4 T=CL" [
      todoLeaf "scan" "Full EMV scan",
      todoLeaf "apdu" "Select and send one APDU",
      grp "anticoll" "T=CL emulation anti-collision" [ todoLeaf "set" "Set UID/ATQA/SAK/ATS" ],
      grp "static" "Static APDU responses" [
        todoLeaf "add" "Add a static response", todoLeaf "clear" "Clear static responses" ],
      grp "relay" "APDU relay" [ todoLeaf "recv" "Poll for a pending APDU", todoLeaf "send" "Send an APDU response" ] ] ]

/-! ## `lf` group -/

/-- The read/write/emulate quartet most LF protocols share. -/
private def lfProto (name help : String) (extra : List CliTree := []) : CliTree :=
  grp name help ([
    todoLeaf "read" "Read a card",
    todoLeaf "write" "Write a card onto T55xx",
    grp "emu" "Emulated id" [ todoLeaf "get" "Get emulated id", todoLeaf "set" "Set emulated id" ]
  ] ++ extra)

def lfGroup : CliTree :=
  grp "lf" "Low-frequency (125 kHz) commands" [
    grp "em" "EM microelectronic" [
      lfProto "410x" "EM410x / Electra",
      grp "4x05" "EM4x05 / EM4x69" [ todoLeaf "read" "Read a tag" ] ],
    lfProto "hid" "HID Prox",
    lfProto "ioprox" "ioProx (XSF)" [
      todoLeaf "decode" "Decode 8 raw bytes", todoLeaf "compose" "Compose from ver/fc/cn" ],
    lfProto "viking" "Viking",
    lfProto "pac" "PAC/Stanley",
    lfProto "jablotron" "Jablotron",
    grp "idteck" "IDTECK" [
      todoLeaf "write" "Write a frame onto T55xx",
      grp "emu" "Emulated frame" [ todoLeaf "get" "Get emulated frame", todoLeaf "set" "Set emulated frame" ] ],
    todoLeaf "sniff" "Capture raw LF samples",
    todoLeaf "adc" "Read the ADC with field on" ]

end Chamelean.Cli
