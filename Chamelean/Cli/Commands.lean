import Chamelean.Cli.Tree
import Chamelean.Cli.Pretty
import Chamelean.Cli.Watch
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

private def printTag (t : Tag14a) : IO Unit := do
  IO.println s!"- UID : {toHex t.uid}"
  IO.println s!"  ATQA: {toHex t.atqa}  SAK: {hexByte t.sak}"
  unless t.ats.isEmpty do IO.println s!"  ATS : {toHex t.ats}"

private def scanCmd : CliTree :=
  mkLeaf "scan" "Scan for ISO14443-A tags" { description := "Scan for ISO14443-A tags" } <|
    readerRequired fun c _ => do
      let tags ← c.hf14aScan false
      if tags.isEmpty then IO.println "No tag found."
      tags.forM printTag

private def scanKeepCmd : CliTree :=
  mkLeaf "scankeep" "Scan, keeping the RF field alive"
      { description := "Scan once and leave the tag powered for raw exchanges" } <|
    readerRequired fun c _ => do
      let tags ← c.hf14aScan true
      if tags.isEmpty then IO.println "No tag found."
      tags.forM printTag

private def watchCmd : CliTree :=
  mkLeaf "watch" "Keep scanning, printing tags as they appear"
      { description := "Keep scanning for ISO14443-A tags until ESC" } <|
    readerRequired fun c _ =>
      -- The state is the UIDs in the field last time, so a tag prints once per arrival.
      watchLoop (#[] : Array ByteArray) fun seen => do
        let tags ← c.hf14aScan false
        for t in tags do
          unless seen.contains t.uid do printTag t
        return tags.map (·.uid)

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

private def modelCmd : CliTree :=
  mkLeaf "model" "Get device model (Ultra/Lite)" { description := "Get device model (Ultra/Lite)" } <|
    deviceRequired fun c _ => do
      IO.println (if (← c.getDeviceModel).data[0]?.getD 0 == 0 then "Ultra" else "Lite")

private def gitversionCmd : CliTree :=
  mkLeaf "gitversion" "Get firmware git version" { description := "Get firmware git version" } <|
    deviceRequired fun c _ => do IO.println (String.fromUTF8! (← c.getGitVersion).data)

/-! ## Small shared helpers -/

/-- Unwrap an `expectResponse` result into `IO`, raising a `CliError` on a bad status. -/
private def unwrap (r : Except CliError α) : IO α :=
  match r with
  | .ok a => pure a
  | .error e => throw e.toIO

/-- The constructor of `all` whose `name` matches `s`, if any. -/
private def parseEnum (all : Array α) (name : α → String) (s : String) : Option α :=
  all.find? (fun x => name x == s)

private def enumChoices (all : Array α) (name : α → String) : List String :=
  all.toList.map name

private def forceFlag : ArgSpec :=
  { key := "force", names := ["--force"], kind := .flag, help := "Just to be sure" }

/-- Require `--force`, else print a warning and return `false`. -/
private def requireForce (a : Args) : IO Bool := do
  unless a.has "force" do
    IO.println (yellow "If you are really sure, pass --force.")
  return a.has "force"

/-! ## `hw` extras (spliced into the `hw` group after connect/disconnect/version) -/

private def factoryResetCmd : CliTree :=
  mkLeaf "factory_reset" "Wipe all data and settings (factory reset)"
    { description := "Wipe all slot data and custom settings and return to factory settings"
      specs := [forceFlag] } <|
    deviceRequired fun c a => do
      if ← requireForce a then
        let _ ← c.wipeFds
        IO.println (green " - Reset successful! Please reconnect.")

private def settingsDumpCmd : CliTree :=
  mkLeaf "dump" "Dump all settings" { description := "Dump all settings" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getDeviceSettings) [Status.success.toUInt16] "settings dump")
      let animation := (AnimationMode.ofUInt8? d[1]!).map toString |>.getD "unknown"
      let btn (i : Nat) := (ButtonPressFunction.ofUInt8? d[i]!).map toString |>.getD "unknown"
      IO.println s!"Settings version : {d[0]!}"
      IO.println s!"Animation mode   : {animation}"
      IO.println s!"Button A (short) : {btn 2}"
      IO.println s!"Button B (short) : {btn 3}"
      IO.println s!"Button A (long)  : {btn 4}"
      IO.println s!"Button B (long)  : {btn 5}"
      IO.println s!"BLE pairing      : {if d[6]! != 0 then "enabled" else "disabled"}"
      IO.println s!"BLE pairing key  : {String.fromUTF8! (d.extract 7 13)}"
      IO.println s!"Sleep timeout    : {d[13]!}s"

private def settingsStoreCmd : CliTree :=
  mkLeaf "store" "Save settings to flash" { description := "Save settings to flash" } <|
    deviceRequired fun c _ => do
      IO.println (if (← c.saveSettings).ok then green " - Store success" else red " - Store failed")

private def settingsResetCmd : CliTree :=
  mkLeaf "reset" "Reset settings in flash"
    { description := "Reset settings to default values", specs := [forceFlag] } <|
    deviceRequired fun c a => do
      if ← requireForce a then
        IO.println (if (← c.resetSettings).ok then green " - Reset success" else red " - Reset failed")

private def animationGetCmd : CliTree :=
  mkLeaf "get" "Get animation mode" { description := "Get animation mode" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getAnimationMode) [Status.success.toUInt16] "animation get")
      IO.println ((AnimationMode.ofUInt8? d[0]!).map toString |>.getD "unknown")

private def animationSetCmd : CliTree :=
  mkLeaf "set" "Set animation mode"
    { description := "Set animation mode"
      specs := [{ key := "mode", names := ["-m", "--mode"], required := true, metavar := "MODE",
                  choices := enumChoices AnimationMode.all (·.name),
                  help := "full/minimal/none/symmetric" }] } <|
    deviceRequired fun c a => do
      let some mode := parseEnum AnimationMode.all (·.name) (a.str? "mode" |>.getD "")
        | throw (CliError.usage "invalid mode").toIO
      let _ ← unwrap (expectResponse (← c.setAnimationMode mode) [Status.success.toUInt16] "animation set")
      IO.println (green "Animation mode change success.")
      IO.println (yellow "Do not forget to store your settings in flash!")

private def sleepGetCmd : CliTree :=
  mkLeaf "get" "Get sleep timeout" { description := "Get the post-wakeup sleep timeout" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getSleepTimeout) [Status.success.toUInt16] "sleep get")
      IO.println s!"Current wake timeout: {d[0]!} seconds"

private def sleepSetCmd : CliTree :=
  mkLeaf "set" "Set sleep timeout"
    { description := "Set the wake timeout after a button press (5-60 seconds)"
      specs := [{ key := "seconds", names := ["-s", "--seconds"], kind := .int, required := true,
                  metavar := "SECONDS" }] } <|
    deviceRequired fun c a => do
      let secs := (a.int? "seconds").getD 0
      if secs < 5 ∨ 60 < secs then
        throw (CliError.usage "value must be between 5 and 60 seconds").toIO
      let _ ← unwrap (expectResponse (← c.setSleepTimeout secs.toNat.toUInt8) [Status.success.toUInt16]
        "sleep set")
      IO.println s!"Wake timeout set to {secs} seconds."
      if secs ≥ 30 then IO.println (yellow "Warning: a long wake timeout will drain the battery faster.")
      IO.println (yellow "Do not forget to store your settings in flash!")

/-- `-a`/`-b` selects the button; defaults to A. -/
private def buttonParser (extra : List ArgSpec := []) : ArgParser := {
  description := "Get or set a button's press function"
  specs := [
    { key := "a", names := ["-a", "-A"], kind := .flag, help := "Button A (default)" },
    { key := "b", names := ["-b", "-B"], kind := .flag, help := "Button B" } ] ++ extra
  groups := [{ members := ["a", "b"] }] }

private def buttonArg (a : Args) : ButtonType := if a.has "b" then .b else .a

private def buttonGetCmd (long : Bool) : CliTree :=
  mkLeaf (if long then "longget" else "get")
      (if long then "Get long-press function" else "Get short-press function") (buttonParser) <|
    deviceRequired fun c a => do
      let btn := buttonArg a
      let d ← unwrap (expectResponse
        (← if long then c.getLongButtonPressConfig btn else c.getButtonPressConfig btn)
        [Status.success.toUInt16] "button get")
      IO.println ((ButtonPressFunction.ofUInt8? d[0]!).map toString |>.getD "unknown")

private def buttonSetCmd (long : Bool) : CliTree :=
  mkLeaf (if long then "longset" else "set")
      (if long then "Set long-press function" else "Set short-press function")
      (buttonParser [{ key := "function", names := ["-f", "--function"], required := true,
                       metavar := "FUNCTION", choices := enumChoices ButtonPressFunction.all (·.name) }]) <|
    deviceRequired fun c a => do
      let btn := buttonArg a
      let some fn := parseEnum ButtonPressFunction.all (·.name) (a.str? "function" |>.getD "")
        | throw (CliError.usage "invalid function").toIO
      let _ ← unwrap (expectResponse
        (← if long then c.setLongButtonPressConfig btn fn else c.setButtonPressConfig btn fn)
        [Status.success.toUInt16] "button set")
      IO.println (green s!"Successfully set function '{fn}' to Button {btn} \
        {if long then "long-press" else "short-press"}")
      IO.println (yellow "Do not forget to store your settings in flash!")

private def blekeySetCmd : CliTree :=
  mkLeaf "set" "Set BLE pairing key"
    { description := "Set the 6-digit BLE pairing key"
      specs := [{ key := "key", names := ["-k", "--key"], required := true, metavar := "<6 digits>" }] } <|
    deviceRequired fun c a => do
      let key := a.str? "key" |>.getD ""
      unless key.length == 6 ∧ key.toList.all (·.isDigit) do
        throw (CliError.usage "the BLE pairing key must be 6 ASCII digits").toIO
      let _ ← unwrap (expectResponse (← c.setBleConnectKey key) [Status.success.toUInt16] "blekey set")
      IO.println (green s!"Successfully set ble connect key to: {key}")
      IO.println (yellow "Do not forget to store your settings in flash!")

private def blepairSetCmd : CliTree :=
  mkLeaf "set" "Enable/disable BLE pairing"
    { description := "Enable or disable BLE pairing"
      specs := [
        { key := "enable", names := ["-e", "--enable"], kind := .flag, help := "Enable BLE pairing" },
        { key := "disable", names := ["-d", "--disable"], kind := .flag, help := "Disable BLE pairing" }]
      groups := [{ members := ["enable", "disable"], required := true }] } <|
    deviceRequired fun c a => do
      let enable := a.has "enable"
      c.setBlePairingEnable enable
      IO.println (green s!"Successfully changed BLE pairing to {if enable then "enabled" else "disabled"}.")
      IO.println (yellow "Do not forget to store your settings in flash!")

private def blebondsClearCmd : CliTree :=
  mkLeaf "blebonds_clear" "Delete all BLE bonds"
    { description := "Clear all BLE bindings. Effect is immediate!", specs := [forceFlag] } <|
    deviceRequired fun c a => do
      if ← requireForce a then
        let _ ← unwrap (expectResponse (← c.deleteAllBleBonds) [Status.success.toUInt16] "blebonds_clear")
        IO.println (green " - Successfully cleared all bonds")

/-! ### `hw slot` -/

/-- `--slot` is optional on most slot commands; falls back to the active slot. -/
private def slotArg (c : Client) (a : Args) : IO SlotNumber := do
  match a.int? "slot" with
  | some n =>
    let some s := SlotNumber.ofUInt8? n.toNat.toUInt8 | throw (CliError.usage "slot must be 1..8").toIO
    return s
  | none =>
    let d ← unwrap (expectResponse (← c.getActiveSlot) [Status.success.toUInt16] "get active slot")
    let some s := SlotNumber.ofFw? d[0]! | throw (CliError.other "device returned an invalid slot").toIO
    return s

private def slotSpec (required : Bool := false) : ArgSpec :=
  { key := "slot", names := ["-s", "--slot"], kind := .int, required, metavar := "<1-8>",
    help := "Slot number (default: active slot)" }

private def senseSpecs : List ArgSpec :=
  [ { key := "hf", names := ["--hf"], kind := .flag, help := "HF sense (default)" },
    { key := "lf", names := ["--lf"], kind := .flag, help := "LF sense" } ]

private def senseArg (a : Args) : TagSenseType := if a.has "lf" then .lf else .hf

private def tagTypeSpec (required : Bool := true) : ArgSpec :=
  { key := "type", names := ["-t", "--type"], required, metavar := "TYPE",
    choices := enumChoices TagSpecificType.usable (·.name), help := "Tag type" }

private def tagTypeArg (a : Args) : IO TagSpecificType := do
  let some t := parseEnum TagSpecificType.usable (·.name) (a.str? "type" |>.getD "")
    | throw (CliError.usage "invalid tag type").toIO
  return t

private def slotListCmd : CliTree :=
  mkLeaf "list" "List slot tag types" { description := "List information about all 8 slots" } <|
    deviceRequired fun c _ => do
      let info ← unwrap (expectResponse (← c.getSlotInfo) [Status.success.toUInt16] "slot list")
      let selected ← unwrap (expectResponse (← c.getActiveSlot) [Status.success.toUInt16] "slot list")
      let enabled ← unwrap (expectResponse (← c.getEnabledSlots) [Status.success.toUInt16] "slot list")
      for slot in SlotNumber.all do
        let fw := slot.toFw.toNat
        let hf := TagSpecificType.ofUInt16? (readU16 info (fw * 4))
        let lf := TagSpecificType.ofUInt16? (readU16 info (fw * 4 + 2))
        let active := if fw.toUInt8 == selected[0]! then green " (active)" else ""
        IO.println s!"Slot {slot}{active}:"
        let describe (t : Option TagSpecificType) (en : UInt8) :=
          let name := t.map toString |>.getD "undefined"
          if en == 0 then s!"{name} {red "(disabled)"}" else name
        IO.println s!"  HF: {describe hf enabled[fw*2]!}"
        IO.println s!"  LF: {describe lf enabled[fw*2+1]!}"

private def slotActiveCmd : CliTree :=
  mkLeaf "active" "Get active slot" { description := "Get the active slot" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getActiveSlot) [Status.success.toUInt16] "slot active")
      let some s := SlotNumber.ofFw? d[0]! | throw (CliError.other "invalid slot").toIO
      IO.println s!"Active slot: {s}"

private def slotChangeCmd : CliTree :=
  mkLeaf "change" "Set active slot"
    { description := "Set the active slot", specs := [slotSpec (required := true)] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let _ ← unwrap (expectResponse (← c.setActiveSlot slot) [Status.success.toUInt16] "slot change")
      IO.println (green s!" - Set slot {slot} activated.")

private def slotTypeCmd : CliTree :=
  mkLeaf "type" "Set slot tag type"
    { description := "Set the emulation tag type for a slot"
      specs := [slotSpec, tagTypeSpec] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let ty ← tagTypeArg a
      let _ ← unwrap (expectResponse (← c.setSlotTagType slot ty) [Status.success.toUInt16] "slot type")
      let _ ← unwrap (expectResponse (← c.setSlotDataDefault slot ty) [Status.success.toUInt16] "slot type")
      IO.println (green s!" - Set slot {slot} tag type success.")

private def slotInitCmd : CliTree :=
  mkLeaf "init" "Reset slot data to default"
    { description := "Reset a slot's data to the default for its tag type"
      specs := [slotSpec, tagTypeSpec] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let ty ← tagTypeArg a
      let _ ← unwrap (expectResponse (← c.setSlotDataDefault slot ty) [Status.success.toUInt16] "slot init")
      IO.println (green " - Set slot tag data init success.")

private def slotEnableCmd : CliTree :=
  mkLeaf "enable" "Enable/disable a slot sense"
    { description := "Enable or disable a slot's HF/LF sense"
      specs := slotSpec :: senseSpecs ++
        [{ key := "off", names := ["--off"], kind := .flag, help := "Disable instead of enable" }]
      groups := [{ members := ["hf", "lf"] }] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let sense := senseArg a
      let _ ← unwrap (expectResponse (← c.setSlotEnable slot sense !(a.has "off")) [Status.success.toUInt16]
        "slot enable")
      IO.println (green s!" - {if a.has "off" then "Disable" else "Enable"} slot {slot} {sense} success.")

private def slotDeleteCmd : CliTree :=
  mkLeaf "delete" "Delete a slot sense type"
    { description := "Delete a slot's sense-type data"
      specs := slotSpec :: senseSpecs
      groups := [{ members := ["hf", "lf"] }] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let sense := senseArg a
      let _ ← unwrap (expectResponse (← c.deleteSlotSenseType slot sense) [Status.success.toUInt16]
        "slot delete")
      IO.println (green s!" - Delete slot {slot} {sense} tag type success.")

private def slotEnabledCmd : CliTree :=
  mkLeaf "enabled" "Show enabled slots" { description := "Show enabled state of all slots" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getEnabledSlots) [Status.success.toUInt16] "slot enabled")
      for slot in SlotNumber.all do
        let fw := slot.toFw.toNat
        let onOff (b : UInt8) := if b == 0 then red "disabled" else green "enabled"
        IO.println s!"Slot {slot}: HF {onOff d[fw*2]!}  LF {onOff d[fw*2+1]!}"

private def slotStoreCmd : CliTree :=
  mkLeaf "store" "Save slot data/config to flash" { description := "Save slot data/config to flash" } <|
    deviceRequired fun c _ => do
      let _ ← unwrap (expectResponse (← c.slotDataConfigSave) [Status.success.toUInt16] "slot store")
      IO.println (green " - Store slots config and data to flash success.")

private def slotNickGetCmd : CliTree :=
  mkLeaf "get" "Get slot nickname"
    { description := "Get a slot's nickname", specs := slotSpec :: senseSpecs
      groups := [{ members := ["hf", "lf"] }] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let sense := senseArg a
      let d ← unwrap (expectResponse (← c.getSlotTagNick slot sense) [Status.success.toUInt16] "slot nick get")
      IO.println (String.fromUTF8! d)

private def slotNickSetCmd : CliTree :=
  mkLeaf "set" "Set slot nickname"
    { description := "Set a slot's nickname"
      specs := slotSpec :: senseSpecs ++
        [{ key := "name", names := ["-n", "--name"], required := true, help := "Nickname" }]
      groups := [{ members := ["hf", "lf"] }] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let sense := senseArg a
      let name := a.str? "name" |>.getD ""
      let _ ← unwrap (expectResponse (← c.setSlotTagNick slot sense name) [Status.success.toUInt16]
        "slot nick set")
      IO.println (green s!" - Set nickname for slot {slot} {sense}: {name}")

private def slotNickDeleteCmd : CliTree :=
  mkLeaf "delete" "Delete slot nickname"
    { description := "Delete a slot's nickname", specs := slotSpec :: senseSpecs
      groups := [{ members := ["hf", "lf"] }] } <|
    deviceRequired fun c a => do
      let slot ← slotArg c a
      let sense := senseArg a
      let _ ← unwrap (expectResponse (← c.deleteSlotTagNick slot sense) [Status.success.toUInt16]
        "slot nick delete")
      IO.println (green s!" - Delete nickname for slot {slot} {sense}.")

private def slotNickListCmd : CliTree :=
  mkLeaf "list" "List all slot nicknames" { description := "List every slot's nicknames" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.getAllSlotNicks) [Status.success.toUInt16] "slot nick list")
      let mut o := 0
      for slot in SlotNumber.all do
        if o < d.size then
          let hfLen := d[o]!.toNat; o := o + 1
          let hf := String.fromUTF8! (d.extract o (o + hfLen)); o := o + hfLen
          let lfLen := d[o]!.toNat; o := o + 1
          let lf := String.fromUTF8! (d.extract o (o + lfLen)); o := o + lfLen
          IO.println s!"Slot {slot}: HF={hf}  LF={lf}"

/-! ## `hw` extras (spliced into the `hw` group after connect/disconnect/version) -/

def hwExtras : List CliTree := [
  chipIdCmd, addressCmd, modeCmd, batteryCmd, dfuCmd,
  modelCmd, gitversionCmd,
  factoryResetCmd,
  grp "settings" "Device settings" [
    settingsDumpCmd,
    settingsStoreCmd,
    settingsResetCmd,
    grp "animation" "LED animation mode" [ animationGetCmd, animationSetCmd ],
    grp "sleep" "Post-wakeup sleep timeout" [ sleepGetCmd, sleepSetCmd ],
    grp "button" "Button press functions" [
      buttonGetCmd false, buttonSetCmd false, buttonGetCmd true, buttonSetCmd true ],
    grp "blekey" "BLE pairing key" [ blekeyGetCmd, blekeySetCmd ],
    grp "blepair" "BLE pairing requirement" [ blepairGetCmd, blepairSetCmd ],
    blebondsClearCmd ],
  grp "slot" "Card slot commands" [
    slotListCmd,
    slotActiveCmd,
    slotChangeCmd,
    slotTypeCmd,
    slotInitCmd,
    slotEnableCmd,
    slotDeleteCmd,
    slotEnabledCmd,
    slotStoreCmd,
    grp "nick" "Slot nicknames" [
      slotNickGetCmd, slotNickSetCmd,
      slotNickDeleteCmd, slotNickListCmd ] ] ]

/-! ## `hf` group -/

/-! ### `hf 14a` -/

private def hf14aRawParser : ArgParser := {
  description := "Send a raw ISO14443-A exchange"
  specs := [
    { key := "activate", names := ["-a", "--activate-rf"], kind := .flag,
      help := "Turn the RF field on without selecting" },
    { key := "select", names := ["-s", "--select-tag"], kind := .flag,
      help := "Turn the RF field on and select the tag" },
    { key := "data", names := ["-d", "--data"], kind := .hex, metavar := "hex", help := "Data to send" },
    { key := "bits", names := ["-b", "--bits"], kind := .int, metavar := "dec",
      help := "Bits of the last byte to send (partial byte)" },
    { key := "crc", names := ["-c", "--crc"], kind := .flag, help := "Append CRC" },
    { key := "noResponse", names := ["-r", "--no-response"], kind := .flag,
      help := "Do not wait for a response" },
    { key := "crcClear", names := ["-cc", "--crc-clear"], kind := .flag,
      help := "Verify and strip the response CRC" },
    { key := "keepRf", names := ["-k", "--keep-rf"], kind := .flag, help := "Keep the RF field on afterwards" },
    { key := "timeout", names := ["-t", "--timeout"], kind := .int, metavar := "dec",
      help := "Response timeout in ms (default 100)" } ] }

private def hf14aRawCmd : CliTree :=
  mkLeaf "raw" "Send a raw ISO14443-A exchange" hf14aRawParser <|
    readerRequired fun c a => do
      if a.has "bits" ∧ a.has "crc" then
        throw (CliError.usage "--bits and --crc are mutually exclusive").toIO
      let opts : Client.Hf14aRawOptions :=
        { activateRfField := a.has "activate", waitResponse := !a.has "noResponse",
          appendCrc := a.has "crc", autoSelect := a.has "select", keepRfField := a.has "keepRf",
          checkResponseCrc := a.has "crcClear" }
      let timeout := (a.int? "timeout").map (·.toNat) |>.getD 100
      let bitlen := (a.int? "bits").map (·.toNat)
      let data := a.hex? "data" |>.getD .empty
      let resp ← c.hf14aRaw opts timeout data bitlen
      if resp.isEmpty then IO.println (yellow "No response")
      else IO.println s!" - {String.intercalate " " (resp.toList.map hexByte)}"

private def hf14aConfigGetCmd : CliTree :=
  mkLeaf "get" "Get HF14A config" { description := "Get the HF14A reader config" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.hf14aGetConfig) [Status.success.toUInt16] "hf14a config get")
      IO.println s!"bcc={d[0]!} cl2={d[1]!} cl3={d[2]!} rats={d[3]!}  \
        (each: 0=standard 1=force/fix 2=skip/ignore)"

private def hf14aConfigSetCmd : CliTree :=
  mkLeaf "set" "Set HF14A config"
    { description := "Set the HF14A reader config; unset fields keep their current value"
      specs := [
        { key := "bcc", names := ["--bcc"], kind := .int, metavar := "0-2" },
        { key := "cl2", names := ["--cl2"], kind := .int, metavar := "0-2" },
        { key := "cl3", names := ["--cl3"], kind := .int, metavar := "0-2" },
        { key := "rats", names := ["--rats"], kind := .int, metavar := "0-2" },
        { key := "std", names := ["--std"], kind := .flag, help := "Reset to standard (all 0)" } ] } <|
    deviceRequired fun c a => do
      let cur ← if a.has "std" then pure (ByteArray.mk #[0, 0, 0, 0])
                else unwrap (expectResponse (← c.hf14aGetConfig) [Status.success.toUInt16] "hf14a config get")
      let pick (key : String) (i : Nat) : UInt8 := (a.int? key).map (·.toNat.toUInt8) |>.getD cur[i]!
      let bcc := pick "bcc" 0
      let cl2 := pick "cl2" 1
      let cl3 := pick "cl3" 2
      let rats := pick "rats" 3
      let _ ← unwrap (expectResponse (← c.hf14aSetConfig bcc cl2 cl3 rats) [Status.success.toUInt16]
        "hf14a config set")
      IO.println s!"bcc={bcc} cl2={cl2} cl3={cl3} rats={rats}"

/-- Read the current anti-collision blob and split it into `(uid, atqa, sak, ats)`. -/
private def readAntiColl (c : Client) : IO (ByteArray × ByteArray × ByteArray × ByteArray) := do
  let d ← unwrap (expectResponse (← c.hf14aGetAntiCollData) [Status.success.toUInt16] "anticoll get")
  let uidLen := d[0]!.toNat
  let uid := d.extract 1 (1 + uidLen)
  let atqa := d.extract (1 + uidLen) (3 + uidLen)
  let sak := d.extract (3 + uidLen) (4 + uidLen)
  let atsLen := d[4 + uidLen]!.toNat
  let ats := d.extract (5 + uidLen) (5 + uidLen + atsLen)
  return (uid, atqa, sak, ats)

private def anticollGetCmd : CliTree :=
  mkLeaf "get" "Get anti-collision data" { description := "Get the emulated anti-collision data" } <|
    deviceRequired fun c _ => do
      let (uid, atqa, sak, ats) ← readAntiColl c
      IO.println s!"UID : {toHex uid}"
      IO.println s!"ATQA: {toHex atqa}  SAK: {toHex sak}"
      unless ats.isEmpty do IO.println s!"ATS : {toHex ats}"

private def anticollSetCmd : CliTree :=
  mkLeaf "set" "Set anti-collision data"
    { description := "Set anti-collision data; unset fields keep their current value"
      specs := [
        { key := "uid", names := ["--uid"], kind := .hex, metavar := "hex" },
        { key := "atqa", names := ["--atqa"], kind := .hex, metavar := "hex" },
        { key := "sak", names := ["--sak"], kind := .hex, metavar := "hex" },
        { key := "ats", names := ["--ats"], kind := .hex, metavar := "hex" },
        { key := "deleteAts", names := ["--delete-ats"], kind := .flag, help := "Clear the ATS" } ]
      groups := [{ members := ["ats", "deleteAts"] }] } <|
    deviceRequired fun c a => do
      let (curUid, curAtqa, curSak, curAts) ← readAntiColl c
      let uid := a.hex? "uid" |>.getD curUid
      unless [4, 7, 10].contains uid.size do throw (CliError.usage "uid must be 4, 7 or 10 bytes").toIO
      let atqa := a.hex? "atqa" |>.getD curAtqa
      unless atqa.size == 2 do throw (CliError.usage "atqa must be 2 bytes").toIO
      let sak := a.hex? "sak" |>.getD curSak
      unless sak.size == 1 do throw (CliError.usage "sak must be 1 byte").toIO
      let ats := if a.has "deleteAts" then .empty else a.hex? "ats" |>.getD curAts
      let _ ← unwrap (expectResponse (← c.hf14aSetAntiCollData uid atqa sak ats) [Status.success.toUInt16]
        "anticoll set")
      IO.println (green "Anti-collision data updated.")

/-! ### `hf mf` reader commands -/

/-- Args shared by `hf mf` commands that unlock a block with a known key: `--blk`, `-a`/`-b`,
`-k`. Port of `MF1AuthArgsUnit`'s parser. -/
private def mf1AuthParser (description : String) : ArgParser := {
  description
  specs := [
    { key := "blk", names := ["--blk", "--block"], kind := .int, required := true,
      metavar := "dec", help := "Block the known key belongs to" },
    { key := "a", names := ["-a", "-A"], kind := .flag, help := "Known key is A (default)" },
    { key := "b", names := ["-b", "-B"], kind := .flag, help := "Known key is B" },
    { key := "key", names := ["-k", "--key"], kind := .hex, required := true,
      metavar := "hex", help := "Sector key, 12 hex digits" } ]
  groups := [ { members := ["a", "b"] } ] }

/-- Require reader mode, then hand the runner the parsed block, key type and 6-byte key.
Port of `MF1AuthArgsUnit.get_param`. -/
private def mf1AuthArgs (run : Client → (block keyType : UInt8) → (key : ByteArray) → IO Unit)
    : ReplState → Args → IO Unit :=
  readerRequired fun c a => do
    -- `required` already guaranteed presence at parse time; the fallbacks keep us total.
    let blk := (a.int? "blk").getD 0
    if blk < 0 ∨ 255 < blk then throw (CliError.usage "block must be in 0..255").toIO
    let key := (a.hex? "key").getD .empty
    unless key.size == 6 do throw (CliError.usage "key must include 12 HEX symbols").toIO
    let keyType := if a.has "b" then MfcKeyType.b else MfcKeyType.a
    run c blk.toNat.toUInt8 keyType.toUInt8 key

private def mfInfoCmd : CliTree :=
  mkLeaf "info" "Detect MIFARE Classic support" { description := "Detect MIFARE Classic support" } <|
    readerRequired fun c _ => do
      IO.println (if ← c.mf1DetectSupport then "MIFARE Classic supported"
                  else "Not a MIFARE Classic tag")

private def mfNtCmd : CliTree :=
  mkLeaf "nt" "Detect PRNG type" { description := "Detect MIFARE Classic PRNG type" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.mf1DetectPrng) [Status.hfTagOk.toUInt16] "nt" with
      | .error e => throw e.toIO
      | .ok d =>
        let prng := (MifareClassicPrngType.ofUInt8? (d[0]?.getD 0)).map (·.description)
        IO.println s!"Prng: {prng.getD "Unknown"}"

private def mfAuthCmd : CliTree :=
  mkLeaf "auth" "Verify a key against a block" (mf1AuthParser "Verify a MIFARE Classic key on a block") <|
    mf1AuthArgs fun c blk keyType key => do
      let r ← c.mf1AuthOneKeyBlock blk keyType key
      IO.println (if r.status == Status.hfTagOk.toUInt16 then green " - Key valid" else red " - Key invalid")

private def mfRdblCmd : CliTree :=
  mkLeaf "rdbl" "Read one block" (mf1AuthParser "MIFARE Classic read one block") <|
    mf1AuthArgs fun c blk keyType key => do
      match expectResponse (← c.mf1ReadOneBlock blk keyType key) [Status.hfTagOk.toUInt16] "rdbl" with
      | .error e => throw e.toIO
      | .ok d => IO.println s!" - Data: {toHex d}"

private def mfNtdistCmd : CliTree :=
  mkLeaf "ntdist" "Detect nonce distance"
      (mf1AuthParser "Detect the nonce distance for a known key/block (nested-attack input)") <|
    mf1AuthArgs fun c blk keyType key => do
      let d ← unwrap (expectResponse (← c.mf1DetectNtDist blk keyType key) [Status.hfTagOk.toUInt16] "ntdist")
      IO.println s!"uid={toHex (d.extract 0 4)} dist={readU32 d 4}"

private def mfWrblCmd : CliTree :=
  mkLeaf "wrbl" "Write one block"
    { mf1AuthParser "Mifare Classic write one block" with
      specs := (mf1AuthParser "").specs ++
        [{ key := "data", names := ["-d", "--data"], kind := .hex, required := true, metavar := "hex",
           help := "16-byte block data" }] } <|
    readerRequired fun c a => do
      let blk := (a.int? "blk").getD 0
      if blk < 0 ∨ 255 < blk then throw (CliError.usage "block must be in 0..255").toIO
      let key := (a.hex? "key").getD .empty
      unless key.size == 6 do throw (CliError.usage "key must include 12 HEX symbols").toIO
      let keyType := if a.has "b" then MfcKeyType.b else MfcKeyType.a
      let data := (a.hex? "data").getD .empty
      unless data.size == 16 do throw (CliError.usage "data must include 32 HEX symbols").toIO
      let r ← c.mf1WriteOneBlock blk.toNat.toUInt8 keyType.toUInt8 key data
      IO.println (if r.ok then green " - Write done." else red " - Write fail.")

/-! ### `hf mf value` -/

private def leU32 (v : UInt32) : ByteArray :=
  ByteArray.mk #[v.toUInt8, (v >>> 8).toUInt8, (v >>> 16).toUInt8, (v >>> 24).toUInt8]

private def readU32LE (d : ByteArray) (i : Nat) : UInt32 :=
  d[i]!.toUInt32 ||| (d[i + 1]!.toUInt32 <<< 8) ||| (d[i + 2]!.toUInt32 <<< 16) |||
    (d[i + 3]!.toUInt32 <<< 24)

/-- Two's-complement `UInt32` bit pattern for a signed 32-bit value. -/
private def i32Bits (v : Int) : UInt32 := (if v < 0 then 4294967296 + v else v).toNat.toUInt32

/-- Signed value of a `UInt32`'s bit pattern, read back as a 32-bit two's complement. -/
private def i32Of (u : UInt32) : Int := if u.toNat ≥ 2147483648 then (u.toNat : Int) - 4294967296 else u.toNat

private def mfValueParser : ArgParser := {
  description := "MIFARE Classic value block commands"
  specs := [
    { key := "get", names := ["--get"], kind := .flag, help := "Get value from src block" },
    { key := "set", names := ["--set"], kind := .int, metavar := "dec",
      help := "Set value X (-2147483647..2147483647) on src block" },
    { key := "inc", names := ["--inc"], kind := .int, metavar := "dec",
      help := "Increment src by X (0..2147483647), write to dst" },
    { key := "dec", names := ["--dec"], kind := .int, metavar := "dec",
      help := "Decrement src by X (0..2147483647), write to dst" },
    { key := "res", names := ["--res", "--cp"], kind := .flag, help := "Copy src to dst" },
    { key := "blk", names := ["--blk", "--src-block"], kind := .int, required := true, metavar := "dec" },
    { key := "a", names := ["-a", "-A"], kind := .flag, help := "src key is A (default)" },
    { key := "b", names := ["-b", "-B"], kind := .flag, help := "src key is B" },
    { key := "key", names := ["-k", "--src-key"], kind := .hex, required := true, metavar := "hex" },
    { key := "tblk", names := ["--tblk", "--dst-block"], kind := .int, metavar := "dec",
      help := "dst block (default: src)" },
    { key := "ta", names := ["--ta", "--tA"], kind := .flag, help := "dst key is A" },
    { key := "tb", names := ["--tb", "--tB"], kind := .flag, help := "dst key is B" },
    { key := "tkey", names := ["--tkey", "--dst-key"], kind := .hex, metavar := "hex",
      help := "dst key (default: src key)" } ]
  groups := [ { members := ["get", "set", "inc", "dec", "res"], required := true },
              { members := ["a", "b"] }, { members := ["ta", "tb"] } ] }

private def mfValueGet (c : Client) (blk : UInt8) (keyType : MfcKeyType) (key : ByteArray) : IO Unit := do
  let d ← unwrap (expectResponse (← c.mf1ReadOneBlock blk keyType.toUInt8 key) [Status.hfTagOk.toUInt16]
    "value")
  let val1 := i32Of (readU32LE d 0)
  let val2 := i32Of (readU32LE d 4)
  let val3 := i32Of (readU32LE d 8)
  let adr1 := d[12]!; let adr2 := d[13]!; let adr3 := d[14]!; let adr4 := d[15]!
  if val1 != val3 ∨ val1 + val2 != -1 then
    IO.println (red s!" - Invalid value of value block: {toHex d}")
  else if adr1 != adr3 ∨ adr2 != adr4 ∨ adr1.toNat + adr2.toNat != 0xFF then
    IO.println (red s!" - Invalid address of value block: {toHex d}")
  else
    IO.println (green s!" - block[{blk}] = value: {val1}, adr: {adr1}")

private def mfValueCmd : CliTree :=
  mkLeaf "value" "Increment/decrement/restore a value block" mfValueParser <|
    readerRequired fun c a => do
      let srcBlk := (a.int? "blk").getD 0
      if srcBlk < 0 ∨ 255 < srcBlk then throw (CliError.usage "src block must be in 0..255").toIO
      let srcBlk := srcBlk.toNat.toUInt8
      let srcType := if a.has "b" then MfcKeyType.b else MfcKeyType.a
      let srcKey := (a.hex? "key").getD .empty
      unless srcKey.size == 6 do throw (CliError.usage "src key must include 12 HEX symbols").toIO
      if a.has "get" then mfValueGet c srcBlk srcType srcKey
      else if let some v := a.int? "set" then
        if v < -2147483647 ∨ 2147483647 < v then
          throw (CliError.usage "set value must be between -2147483647 and 2147483647").toIO
        let adrInv : UInt8 := 0xFF - srcBlk
        let data := leU32 (i32Bits v) ++ leU32 (i32Bits (-v - 1)) ++ leU32 (i32Bits v) ++
          ByteArray.mk #[srcBlk, adrInv, srcBlk, adrInv]
        let r ← c.mf1WriteOneBlock srcBlk srcType.toUInt8 srcKey data
        if r.ok then IO.println (green " - Set done."); mfValueGet c srcBlk srcType srcKey
        else IO.println (red " - Set fail.")
      else
        let dstBlk := (a.int? "tblk").map (·.toNat.toUInt8) |>.getD srcBlk
        let dstType := if a.has "ta" then MfcKeyType.a else if a.has "tb" then MfcKeyType.b else srcType
        let dstKey := a.hex? "tkey" |>.getD srcKey
        unless dstKey.size == 6 do throw (CliError.usage "dst key must include 12 HEX symbols").toIO
        let run (op : MfcValueBlockOperator) (operand : UInt32) (verb : String) : IO Unit := do
          let r ← c.mf1ManipulateValueBlock srcBlk srcType.toUInt8 srcKey op operand dstBlk dstType.toUInt8
            dstKey
          if r.ok then IO.println (green s!" - {verb} done."); mfValueGet c dstBlk dstType dstKey
          else IO.println (red s!" - {verb} fail.")
        if let some v := a.int? "inc" then
          if v < 0 ∨ 2147483647 < v then throw (CliError.usage "increment must be 0..2147483647").toIO
          run .increment v.toNat.toUInt32 "Increment"
        else if let some v := a.int? "dec" then
          if v < 0 ∨ 2147483647 < v then throw (CliError.usage "decrement must be 0..2147483647").toIO
          run .decrement v.toNat.toUInt32 "Decrement"
        else run .restore 0 "Restore"

/-! ### `hf mf check` -/

private def parseKeys (joined : String) : IO (Array ByteArray) := do
  let toks := words joined
  if toks.isEmpty then throw (CliError.usage "at least one key is required").toIO
  let mut out : Array ByteArray := #[]
  for t in toks do
    let b ← match Cli.ofHex t with
      | .ok b => pure b
      | .error e => throw (CliError.usage e).toIO
    unless b.size == 6 do throw (CliError.usage s!"key {t} must include 12 HEX symbols").toIO
    out := out.push b
  return out

private def mfCheckCmd : CliTree :=
  mkLeaf "check" "Check keys against masked sectors"
    { description := "Check a list of keys against every unmasked sector (chunks of 20)"
      specs := [
        { key := "mask", names := ["-m", "--mask"], kind := .hex, metavar := "hex20",
          help := "10-byte bitmask of sectors to skip (default: check all)" },
        { key := "keys", help := "Keys to test (hex, space-separated)" } ] } <|
    readerRequired fun c a => do
      let mut mask := a.hex? "mask" |>.getD (ByteArray.mk #[0,0,0,0,0,0,0,0,0,0])
      unless mask.size == 10 do throw (CliError.usage "mask must be 10 bytes").toIO
      let keys ← parseKeys (a.str? "keys" |>.getD "")
      let mut found : Std.HashMap Nat ByteArray := {}
      let mut i := 0
      while i < keys.size do
        let chunk := (keys.extract i (min (i + 20) keys.size)).toList
        IO.println s!" - checking keys {i}..{min (i + 20) keys.size} of {keys.size}"
        let r ← c.mf1CheckKeysOfSectors mask chunk
        if r.status != Status.hfTagOk.toUInt16 then
          IO.println (red s!" - check interrupted: {Status.describe r.status}"); break
        let d := r.data
        if d.size != 490 then
          IO.println (green " - all sector keys are found or masked"); break
        for j in [0:10] do mask := mask.set! j (mask[j]! ||| d[j]!)
        for k in [0:80] do
          let byteIdx := k / 8
          let bitIdx : UInt8 := (7 - k % 8).toUInt8
          if (d[byteIdx]! >>> bitIdx) &&& 1 == 1 then
            found := found.insert k (d.extract (6 * k + 10) (6 * k + 16))
        i := i + 20
      if found.isEmpty then IO.println (yellow "No keys found.")
      else for (sector, key) in found.toList do IO.println s!"sector {sector}: {green (toHex key)}"

private def mfCheckBlkCmd : CliTree :=
  mkLeaf "checkblk" "Check keys against one block"
    { description := "Check a list of keys against one block (up to 83)"
      specs := [
        { key := "blk", names := ["--blk", "--block"], kind := .int, required := true, metavar := "dec" },
        { key := "a", names := ["-a", "-A"], kind := .flag, help := "Key type A (default)" },
        { key := "b", names := ["-b", "-B"], kind := .flag, help := "Key type B" },
        { key := "keys", help := "Keys to test (hex, space-separated)" } ]
      groups := [{ members := ["a", "b"] }] } <|
    readerRequired fun c a => do
      let blk := (a.int? "blk").getD 0
      if blk < 0 ∨ 255 < blk then throw (CliError.usage "block must be in 0..255").toIO
      let keyType := if a.has "b" then MfcKeyType.b else MfcKeyType.a
      let keys ← parseKeys (a.str? "keys" |>.getD "")
      let r ← c.mf1CheckKeysOnBlock blk.toNat.toUInt8 keyType keys.toList
      if r.status == Status.hfTagOk.toUInt16 ∧ r.data.size == 7 ∧ r.data[0]! != 0 then
        IO.println (green s!"Found: {toHex (r.data.extract 1 7)}")
      else IO.println (yellow "Not found.")

/-! ### `hf mf authtrace` -/

/-- Split the sniff/authtrace frame buffer into `(bits, data, cardToReader)` triples. -/
private partial def parse14aFrames (buf : ByteArray) : List (Nat × ByteArray × Bool) := Id.run do
  let mut out : List (Nat × ByteArray × Bool) := []
  let mut i := 0
  while i + 2 ≤ buf.size do
    let hdr := readU16 buf i
    i := i + 2
    let isTx := hdr &&& 0x8000 != 0
    let szBits := (hdr &&& 0x7FFF).toNat
    if szBits == 0 then break
    let szBytes := (szBits + 7) / 8
    if i + szBytes > buf.size then break
    out := out ++ [(szBits, buf.extract i (i + szBytes), isTx)]
    i := i + szBytes
  return out

private def mfAuthTraceCmd : CliTree :=
  mkLeaf "authtrace" "Capture a full reader auth trace"
    { description := "Run a full reader-side auth against a real card, printing every wire frame"
      specs := [
        { key := "blk", names := ["--blk", "--block"], kind := .int, required := true, metavar := "dec" },
        { key := "a", names := ["-a", "-A"], kind := .flag, help := "Key type A (default)" },
        { key := "b", names := ["-b", "-B"], kind := .flag, help := "Key type B" },
        { key := "key", names := ["-k", "--key"], kind := .hex, required := true, metavar := "hex" },
        { key := "timeout", names := ["-t", "--timeout"], kind := .int, metavar := "ms",
          help := "Tag-presence polling timeout in ms (default 5000)" } ]
      groups := [{ members := ["a", "b"] }] } <|
    readerRequired fun c a => do
      let blk := (a.int? "blk").getD 0
      if blk < 0 ∨ 255 < blk then throw (CliError.usage "block must be in 0..255").toIO
      let key := (a.hex? "key").getD .empty
      unless key.size == 6 do throw (CliError.usage "key must include 12 HEX symbols").toIO
      let keyType := if a.has "b" then MfcKeyType.b else MfcKeyType.a
      let timeout := (a.int? "timeout").map (·.toNat.toUInt16) |>.getD 5000
      let r ← c.hf14aAuthTrace blk.toNat.toUInt8 keyType key timeout
      if r.status == Status.hfTagNo.toUInt16 then
        IO.println (red "No 14443A tag in field — auth aborted"); return
      if r.data.isEmpty then
        IO.println (red s!"No frames returned (status={Status.describe r.status})"); return
      let frames := parse14aFrames r.data
      if frames.isEmpty then IO.println (red "No frames decoded"); return
      let rx := frames.filter (fun (_, _, tx) => !tx) |>.length
      let tx := frames.filter (fun (_, _, tx) => tx) |>.length
      IO.println s!"Captured {frames.length} frame(s)  ({rx} reader→card  {tx} card→reader)  \
        status={Status.describe r.status}"
      let mut n := 1
      for (bits, data, isTx) in frames do
        let dir := if isTx then green "<<<" else yellow ">>>"
        IO.println s!"  {n} {dir} {bits}b {toHex data}"
        n := n + 1

/-! ### `hf mf` emulator memory -/

private def mfEreadCmd : CliTree :=
  mkLeaf "eread" "Read emulator block data"
    { description := "Read emulator block data"
      specs := [
        { key := "blk", names := ["--blk"], kind := .int, required := true, metavar := "dec" },
        { key := "cnt", names := ["--cnt"], kind := .int, required := true, metavar := "dec" } ] } <|
    deviceRequired fun c a => do
      let blk := (a.int? "blk").getD 0
      let cnt := (a.int? "cnt").getD 0
      if blk < 0 ∨ 255 < blk ∨ cnt < 0 ∨ 255 < cnt then throw (CliError.usage "out of range").toIO
      let d ← unwrap (expectResponse (← c.mf1ReadEmuBlockData blk.toNat.toUInt8 cnt.toNat.toUInt8)
        [Status.success.toUInt16] "eread")
      printMemDump d

private def mfEloadCmd : CliTree :=
  mkLeaf "eload" "Write emulator block data"
    { description := "Write emulator block data"
      specs := [
        { key := "blk", names := ["--blk"], kind := .int, required := true, metavar := "dec" },
        { key := "data", names := ["-d", "--data"], kind := .hex, required := true, metavar := "hex" } ] } <|
    deviceRequired fun c a => do
      let blk := (a.int? "blk").getD 0
      if blk < 0 ∨ 255 < blk then throw (CliError.usage "block must be in 0..255").toIO
      let data := a.hex? "data" |>.getD .empty
      let _ ← unwrap (expectResponse (← c.mf1WriteEmuBlockData blk.toNat.toUInt8 data)
        [Status.success.toUInt16] "eload")
      IO.println (green "Write done.")

/-! ### `hf mf econfig` -/

private def enableFlagGroup (enableHelp disableHelp : String) : List ArgSpec × ArgGroup :=
  ([{ key := "enable", names := ["-e", "--enable"], kind := .flag, help := enableHelp },
    { key := "disable", names := ["-d", "--disable"], kind := .flag, help := disableHelp }],
   { members := ["enable", "disable"], required := true })

private def mfEconfigViewCmd : CliTree :=
  mkLeaf "view" "Show emulator config" { description := "Show the MIFARE Classic emulator config" } <|
    deviceRequired fun c _ => do
      let cfg ← unwrap (expectResponse (← c.mf1GetEmulatorConfig) [Status.success.toUInt16] "econfig view")
      let onOff (b : UInt8) := if b == 0 then red "disabled" else green "enabled"
      IO.println s!"Log (mfkey32) mode:    {onOff cfg[0]!}"
      IO.println s!"Gen1a magic mode:      {onOff cfg[1]!}"
      IO.println s!"Gen2 magic mode:       {onOff cfg[2]!}"
      IO.println s!"Block-0 anti-coll:     {onOff cfg[3]!}"
      IO.println s!"Write mode:            \
        {(WriteMode.ofUInt8? cfg[4]!).map toString |>.getD "invalid"}"
      let prng ← unwrap (expectResponse (← c.mf1GetPrngType) [Status.success.toUInt16] "econfig view")
      IO.println s!"PRNG type:             {(MifareClassicPrngType.ofUInt8? prng[0]!).map toString |>.getD "unknown"}"
      let reset ← unwrap (expectResponse (← c.mf1GetFieldOffDoReset) [Status.success.toUInt16] "econfig view")
      IO.println s!"Field-off crypto reset: {onOff reset[0]!}"

private def mfEconfigToggleCmd (name help enableHelp disableHelp : String)
    (run : Client → Bool → IO Response) : CliTree :=
  let (specs, group) := enableFlagGroup enableHelp disableHelp
  mkLeaf name help { description := help, specs := specs, groups := [group] } <|
    deviceRequired fun c a => do
      let enable := a.has "enable"
      let _ ← unwrap (expectResponse (← run c enable) [Status.success.toUInt16] name)
      IO.println (green s!" - {if enable then "Enabled" else "Disabled"}.")

private def mfEconfigWriteCmd : CliTree :=
  mkLeaf "write" "Set emulator write mode"
    { description := "Set the MIFARE Classic emulator write mode"
      specs := [{ key := "mode", names := ["-m", "--mode"], required := true, metavar := "MODE",
                  choices := enumChoices WriteMode.settable (·.name) }] } <|
    deviceRequired fun c a => do
      let some mode := parseEnum WriteMode.settable (·.name) (a.str? "mode" |>.getD "")
        | throw (CliError.usage "invalid write mode").toIO
      let _ ← unwrap (expectResponse (← c.mf1SetWriteMode mode) [Status.success.toUInt16] "econfig write")
      IO.println (green s!" - Write mode set to {mode}.")

private def mfEconfigPrngGetCmd : CliTree :=
  mkLeaf "get" "Get PRNG type" { description := "Get the emulated MIFARE Classic PRNG type" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf1GetPrngType) [Status.success.toUInt16] "prng get")
      IO.println ((MifareClassicPrngType.ofUInt8? d[0]!).map toString |>.getD "unknown")

private def mfEconfigPrngSetCmd : CliTree :=
  mkLeaf "set" "Set PRNG type"
    { description := "Set the emulated MIFARE Classic PRNG type"
      specs := [{ key := "type", names := ["-t", "--type"], required := true, metavar := "TYPE",
                  choices := enumChoices MifareClassicPrngType.all (·.name) }] } <|
    deviceRequired fun c a => do
      let some ty := parseEnum MifareClassicPrngType.all (·.name) (a.str? "type" |>.getD "")
        | throw (CliError.usage "invalid PRNG type").toIO
      let _ ← unwrap (expectResponse (← c.mf1SetPrngType ty) [Status.success.toUInt16] "prng set")
      IO.println (green s!" - PRNG type set to {ty}.")

private def mfEconfigFieldresetGetCmd : CliTree :=
  mkLeaf "get" "Get field-off reset"
      { description := "Whether the emulator resets crypto state when the field drops" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf1GetFieldOffDoReset) [Status.success.toUInt16] "fieldreset get")
      IO.println (if d[0]! == 0 then "disabled" else "enabled")

private def mfEconfigFieldresetSetCmd : CliTree :=
  let (specs, group) := enableFlagGroup "Reset crypto on field off" "Keep crypto state on field off"
  mkLeaf "set" "Set field-off reset"
    { description := "Set whether the emulator resets crypto state when the field drops"
      specs := specs, groups := [group] } <|
    deviceRequired fun c a => do
      let enable := a.has "enable"
      let _ ← unwrap (expectResponse (← c.mf1SetFieldOffDoReset enable) [Status.success.toUInt16]
        "fieldreset set")
      IO.println (green s!" - {if enable then "Enabled" else "Disabled"}.")

private def mfDetectionCountCmd : CliTree :=
  mkLeaf "count" "Get detection count" { description := "Number of logged mfkey32 records" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf1GetDetectionCount) [Status.success.toUInt16] "detection count")
      IO.println s!"{readU32 d 0}"

private def mfDetectionLogCmd : CliTree :=
  mkLeaf "log" "Get detection log"
    { description := "Print logged mfkey32 records from an index"
      specs := [{ key := "index", names := ["-i", "--index"], kind := .int, metavar := "dec",
                  help := "Starting index (default 0)" }] } <|
    deviceRequired fun c a => do
      let idx := (a.int? "index").map (·.toNat.toUInt32) |>.getD 0
      let d ← unwrap (expectResponse (← c.mf1GetDetectionLog idx) [Status.success.toUInt16] "detection log")
      let mut o := 0
      let mut n := idx.toNat
      while o + 18 ≤ d.size do
        let block := d[o]!
        let bitfield := d[o + 1]!
        let uid := d.extract (o + 2) (o + 6)
        let nt := d.extract (o + 6) (o + 10)
        let nr := d.extract (o + 10) (o + 14)
        let ar := d.extract (o + 14) (o + 18)
        let keyType := if bitfield &&& 1 == 1 then "B" else "A"
        let nested := if bitfield &&& 2 == 2 then " nested" else ""
        IO.println s!"{n}: block={block} type={keyType}{nested} uid={toHex uid} nt={toHex nt} \
          nr={toHex nr} ar={toHex ar}"
        o := o + 18
        n := n + 1

private def hfMfEconfig : CliTree :=
  grp "econfig" "MIFARE Classic emulator config" [
    mfEconfigViewCmd,
    mfEconfigToggleCmd "gen1a" "Set Gen1a magic mode" "Enable Gen1a magic mode" "Disable Gen1a magic mode"
      (fun c e => c.mf1SetGen1aMode e),
    mfEconfigToggleCmd "gen2" "Set Gen2 magic mode" "Enable Gen2 magic mode" "Disable Gen2 magic mode"
      (fun c e => c.mf1SetGen2Mode e),
    mfEconfigToggleCmd "coll" "Set block-0 anti-collision mode"
      "Use anti-collision data from block 0" "Use anti-collision data from settings"
      (fun c e => c.mf1SetBlockAntiCollMode e),
    mfEconfigWriteCmd,
    grp "prng" "Emulated PRNG type" [ mfEconfigPrngGetCmd, mfEconfigPrngSetCmd ],
    grp "fieldreset" "Reset crypto on field off" [ mfEconfigFieldresetGetCmd, mfEconfigFieldresetSetCmd ],
    grp "detection" "mfkey32 nonce logging" [
      mfEconfigToggleCmd "enable" "Enable/disable detection" "Enable mfkey32 logging" "Disable mfkey32 logging"
        (fun c e => c.mf1SetDetectionEnable e),
      mfDetectionCountCmd,
      mfDetectionLogCmd ] ]

/-! ### `hf mfu` -/

private def mfuRdpgCmd : CliTree :=
  mkLeaf "rdpg" "Read emulator pages"
    { description := "Read emulator page data"
      specs := [
        { key := "start", names := ["--start"], kind := .int, required := true, metavar := "dec" },
        { key := "cnt", names := ["--cnt"], kind := .int, required := true, metavar := "dec" } ] } <|
    deviceRequired fun c a => do
      let start := (a.int? "start").getD 0
      let cnt := (a.int? "cnt").getD 0
      if start < 0 ∨ 255 < start ∨ cnt < 0 ∨ 255 < cnt then throw (CliError.usage "out of range").toIO
      let d ← unwrap (expectResponse (← c.mfuReadEmuPageData start.toNat.toUInt8 cnt.toNat.toUInt8)
        [Status.success.toUInt16] "rdpg")
      printMemDump d 4

private def mfuWrpgCmd : CliTree :=
  mkLeaf "wrpg" "Write emulator pages"
    { description := "Write emulator page data"
      specs := [
        { key := "start", names := ["--start"], kind := .int, required := true, metavar := "dec" },
        { key := "data", names := ["-d", "--data"], kind := .hex, required := true, metavar := "hex" } ] } <|
    deviceRequired fun c a => do
      let start := (a.int? "start").getD 0
      if start < 0 ∨ 255 < start then throw (CliError.usage "start out of range").toIO
      let data := a.hex? "data" |>.getD .empty
      let _ ← unwrap (expectResponse (← c.mfuWriteEmuPageData start.toNat.toUInt8 data)
        [Status.success.toUInt16] "wrpg")
      IO.println (green "Write done.")

private def mfuResetauthCmd : CliTree :=
  mkLeaf "resetauth" "Reset authentication counter" { description := "Reset authentication counter" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mfuResetAuthCnt) [Status.success.toUInt16] "resetauth")
      IO.println (green s!" - Reset (was {d[0]!}).")

private def mfuCounterGetCmd : CliTree :=
  mkLeaf "get" "Read a counter"
    { description := "Read an NTAG counter"
      specs := [{ key := "counter", names := ["-c", "--counter"], kind := .int, required := true,
                  metavar := "dec" }] } <|
    deviceRequired fun c a => do
      let idx := (a.int? "counter").getD 0
      if idx < 0 ∨ 2 < idx then throw (CliError.usage "counter index must be 0..2").toIO
      let d ← unwrap (expectResponse (← c.mfuReadEmuCounterData idx.toNat.toUInt8) [Status.success.toUInt16]
        "counter get")
      let value := d[0]!.toUInt32 ||| (d[1]!.toUInt32 <<< 8) ||| (d[2]!.toUInt32 <<< 16)
      let hex := toHex (ByteArray.mk #[(value >>> 16).toUInt8, (value >>> 8).toUInt8, value.toUInt8])
      IO.println s!"Value: {hex} ({value})"
      IO.println s!"Tearing: {if d[3]! == 0xBD then green "not set" else red "set"}"

private def mfuCounterSetCmd : CliTree :=
  mkLeaf "set" "Set a counter"
    { description := "Set an NTAG counter"
      specs := [
        { key := "counter", names := ["-c", "--counter"], kind := .int, required := true, metavar := "dec" },
        { key := "value", names := ["-v", "--value"], kind := .int, required := true, metavar := "dec" },
        { key := "resetTearing", names := ["-t", "--reset-tearing"], kind := .flag,
          help := "Reset the tearing event flag" } ] } <|
    deviceRequired fun c a => do
      let idx := (a.int? "counter").getD 0
      let value := (a.int? "value").getD 0
      if idx < 0 ∨ 2 < idx then throw (CliError.usage "counter index must be 0..2").toIO
      if value < 0 ∨ 0xFFFFFF < value then throw (CliError.usage "counter value must be 0..0xFFFFFF").toIO
      let _ ← unwrap (expectResponse
        (← c.mfuWriteEmuCounterData idx.toNat.toUInt8 value.toNat.toUInt32 (a.has "resetTearing"))
        [Status.success.toUInt16] "counter set")
      IO.println (green " - Ok")

private def mfuVersionGetCmd : CliTree :=
  mkLeaf "get" "Get version data" { description := "Get the emulated GET_VERSION response" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf0NtagGetVersionData) [Status.success.toUInt16] "version get")
      IO.println (toHex d)

private def mfuVersionSetCmd : CliTree :=
  mkLeaf "set" "Set version data"
    { description := "Set the emulated GET_VERSION response (8 bytes)"
      specs := [{ key := "data", names := ["-d", "--data"], kind := .hex, required := true,
                  metavar := "hex" }] } <|
    deviceRequired fun c a => do
      let data := a.hex? "data" |>.getD .empty
      unless data.size == 8 do throw (CliError.usage "version data must be 8 bytes").toIO
      let _ ← unwrap (expectResponse (← c.mf0NtagSetVersionData data) [Status.success.toUInt16] "version set")
      IO.println (green " - Ok")

private def mfuSignatureGetCmd : CliTree :=
  mkLeaf "get" "Get signature" { description := "Get the emulated ECC signature" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf0NtagGetSignatureData) [Status.success.toUInt16] "signature get")
      IO.println (toHex d)

private def mfuSignatureSetCmd : CliTree :=
  mkLeaf "set" "Set signature"
    { description := "Set the emulated ECC signature (32 bytes)"
      specs := [{ key := "data", names := ["-d", "--data"], kind := .hex, required := true,
                  metavar := "hex" }] } <|
    deviceRequired fun c a => do
      let data := a.hex? "data" |>.getD .empty
      unless data.size == 32 do throw (CliError.usage "signature data must be 32 bytes").toIO
      let _ ← unwrap (expectResponse (← c.mf0NtagSetSignatureData data) [Status.success.toUInt16]
        "signature set")
      IO.println (green " - Ok")

private def mfuUidmagicGetCmd : CliTree :=
  mkLeaf "get" "Get UID magic mode" { description := "Get UID magic mode" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf0NtagGetUidMagicMode) [Status.success.toUInt16] "uidmagic get")
      IO.println (if d[0]! == 0 then "disabled" else "enabled")

private def mfuUidmagicSetCmd : CliTree :=
  let (specs, group) := enableFlagGroup "Enable UID magic mode" "Disable UID magic mode"
  mkLeaf "set" "Set UID magic mode"
    { description := "Set UID magic mode", specs := specs, groups := [group] } <|
    deviceRequired fun c a => do
      let enable := a.has "enable"
      let _ ← unwrap (expectResponse (← c.mf0NtagSetUidMagicMode enable) [Status.success.toUInt16]
        "uidmagic set")
      IO.println (green s!" - {if enable then "Enabled" else "Disabled"}.")

private def mfuWriteGetCmd : CliTree :=
  mkLeaf "get" "Get write mode" { description := "Get the MFU/NTAG emulator write mode" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf0NtagGetWriteMode) [Status.success.toUInt16] "write get")
      IO.println ((WriteMode.ofUInt8? d[0]!).map toString |>.getD "invalid")

private def mfuWriteSetCmd : CliTree :=
  mkLeaf "set" "Set write mode"
    { description := "Set the MFU/NTAG emulator write mode"
      specs := [{ key := "mode", names := ["-m", "--mode"], required := true, metavar := "MODE",
                  choices := enumChoices WriteMode.settable (·.name) }] } <|
    deviceRequired fun c a => do
      let some mode := parseEnum WriteMode.settable (·.name) (a.str? "mode" |>.getD "")
        | throw (CliError.usage "invalid write mode").toIO
      let _ ← unwrap (expectResponse (← c.mf0NtagSetWriteMode mode) [Status.success.toUInt16] "write set")
      IO.println (green s!" - Write mode set to {mode}.")

private def mfuDetectionCountCmd : CliTree :=
  mkLeaf "count" "Get detection count" { description := "Number of logged NTAG password records" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.mf0NtagGetDetectionCount) [Status.success.toUInt16]
        "detection count")
      IO.println s!"{readU32 d 0}"

private def mfuDetectionLogCmd : CliTree :=
  mkLeaf "log" "Get detection log"
    { description := "Print logged NTAG password-attempt records from an index"
      specs := [{ key := "index", names := ["-i", "--index"], kind := .int, metavar := "dec",
                  help := "Starting index (default 0)" }] } <|
    deviceRequired fun c a => do
      let idx := (a.int? "index").map (·.toNat.toUInt32) |>.getD 0
      let d ← unwrap (expectResponse (← c.mf0NtagGetDetectionLog idx) [Status.success.toUInt16]
        "detection log")
      let mut o := 0
      let mut n := idx.toNat
      while o + 4 ≤ d.size do
        IO.println s!"{n}: {toHex (d.extract o (o + 4))}"
        o := o + 4
        n := n + 1

private def mfuPagesCmd : CliTree :=
  mkLeaf "pages" "Get emulator page count" { description := "Get emulator page count" } <|
    deviceRequired fun c _ => do
      IO.println s!"Emulator pages: {(← c.mfuGetEmuPagesCount).data[0]?.getD 0}"

/-! ### `hf seos` -/

/-- The active slot's HF tag type, from `getActiveSlot` + `getSlotInfo`. -/
private def activeHfTagType (c : Client) : IO (Option TagSpecificType) := do
  let slot ← unwrap (expectResponse (← c.getActiveSlot) [Status.success.toUInt16] "get active slot")
  let info ← unwrap (expectResponse (← c.getSlotInfo) [Status.success.toUInt16] "get slot info")
  return TagSpecificType.ofUInt16? (readU16 info (slot[0]!.toNat * 4))

/-- Read the four SEOS emulator fields as `(data, oid, tag, diversifier, hashAlg, encrAlg)`. -/
private def readSeosData (c : Client) : IO (ByteArray × ByteArray × ByteArray × ByteArray × UInt8 × UInt8) := do
  let d ← unwrap (expectResponse (← c.seosReadEmuData) [Status.success.toUInt16] "seos read")
  let next (o : Nat) : ByteArray × Nat := let len := d[o]!.toNat; (d.extract (o + 1) (o + 1 + len), o + 1 + len)
  let (data, o) := next 0
  let (oid, o) := next o
  let (tag, o) := next o
  let (diversifier, o) := next o
  return (data, oid, tag, diversifier, d[o]!, d[o+1]!)

private def seosReadCmd : CliTree :=
  mkLeaf "read" "Read emulated SEOS data" { description := "Show the emulated SEOS data" } <|
    deviceRequired fun c _ => do
      unless (← activeHfTagType c) == some .seos do
        throw (CliError.other "the active slot's card in current slot is not SEOS").toIO
      let (data, oid, tag, diversifier, _, _) ← readSeosData c
      IO.println s!"Data       : {toHex data}"
      IO.println s!"OID        : {toHex oid}"
      IO.println s!"Tag        : {toHex tag}"
      IO.println s!"Diversifier: {toHex diversifier}"

private def seosWriteCmd : CliTree :=
  mkLeaf "write" "Write emulated SEOS data"
    { description := "Write emulated SEOS data; unset fields keep their current value"
      specs := [
        { key := "data", names := ["-d", "--data"], kind := .hex, metavar := "hex",
          help := "Data to present to the reader, 2-255 bytes, must be valid BER-TLV" },
        { key := "oid", names := ["-o", "--oid"], kind := .hex, metavar := "hex",
          help := "Target OID, 1-32 bytes" },
        { key := "tag", names := ["-t", "--tag"], kind := .hex, metavar := "hex",
          help := "Tag of the presented data, 1-2 bytes" },
        { key := "diversifier", names := ["--diversifier"], kind := .hex, metavar := "hex",
          help := "Simulated card diversifier, 1-16 bytes" } ] } <|
    deviceRequired fun c a => do
      unless (← activeHfTagType c) == some .seos do
        throw (CliError.other "the card in the current slot is not SEOS").toIO
      let (curData, curOid, curTag, curDiv, hashAlg, encrAlg) ← readSeosData c
      let data := a.hex? "data" |>.getD curData
      let oid := a.hex? "oid" |>.getD curOid
      let tag := a.hex? "tag" |>.getD curTag
      let diversifier := a.hex? "diversifier" |>.getD curDiv
      if data.size < 2 ∨ 255 < data.size then throw (CliError.usage "data must be 2-255 bytes").toIO
      if oid.size < 1 ∨ 32 < oid.size then throw (CliError.usage "oid must be 1-32 bytes").toIO
      if tag.size < 1 ∨ 2 < tag.size then throw (CliError.usage "tag must be 1-2 bytes").toIO
      if diversifier.size < 1 ∨ 16 < diversifier.size then
        throw (CliError.usage "diversifier must be 1-16 bytes").toIO
      let _ ← unwrap (expectResponse (← c.seosWriteEmuData data oid tag diversifier hashAlg encrAlg)
        [Status.success.toUInt16] "seos write")
      IO.println (green " - Ok")

private def seosKeysCmd : CliTree :=
  mkLeaf "keys" "Write SEOS keys"
    { description := "Load the three SEOS keys (auth / privenc / privmac), 16 bytes each"
      specs := [
        { key := "auth", names := ["-a", "--auth"], kind := .hex, required := true, metavar := "hex" },
        { key := "privenc", names := ["-e", "--privenc"], kind := .hex, required := true, metavar := "hex" },
        { key := "privmac", names := ["-m", "--privmac"], kind := .hex, required := true, metavar := "hex" } ] } <|
    deviceRequired fun c a => do
      unless (← activeHfTagType c) == some .seos do
        throw (CliError.other "the card in the current slot is not SEOS").toIO
      let auth := a.hex? "auth" |>.getD .empty
      let privenc := a.hex? "privenc" |>.getD .empty
      let privmac := a.hex? "privmac" |>.getD .empty
      unless auth.size == 16 do throw (CliError.usage "auth key must be 16 bytes").toIO
      unless privenc.size == 16 do throw (CliError.usage "privenc key must be 16 bytes").toIO
      unless privmac.size == 16 do throw (CliError.usage "privmac key must be 16 bytes").toIO
      let _ ← unwrap (expectResponse (← c.seosWriteEmuKeys auth privenc privmac) [Status.success.toUInt16]
        "seos keys")
      IO.println (green " - Ok")

/-! ### `hf emv` (raw ISO14443-4 T=CL wrappers; no APDU/TLV decoding) -/

private def emvScanCmd : CliTree :=
  mkLeaf "scan" "Full EMV scan" { description := "Run the firmware's select→PPSE→GPO→read-records scan" } <|
    deviceRequired fun c _ => do
      IO.println (toHex (← c.hf14a4EmvScan).data)

private def emvApduCmd : CliTree :=
  mkLeaf "apdu" "Select and send one APDU"
    { description := "Select the card and exchange one APDU"
      specs := [{ key := "apdu", help := "APDU bytes, hex", required := true }] } <|
    deviceRequired fun c a => do
      let apdu ← match Cli.ofHex (a.str? "apdu" |>.getD "") with
        | .ok b => pure b
        | .error e => throw (CliError.usage e).toIO
      IO.println (toHex (← c.hf14a4ReaderApdu apdu).data)

private def emvAnticollSetCmd : CliTree :=
  mkLeaf "set" "Set UID/ATQA/SAK/ATS"
    { description := "Set the T=CL emulation anti-collision data"
      specs := [
        { key := "uid", help := "UID, hex", names := ["--uid"], kind := .hex, required := true },
        { key := "atqa", help := "ATQA, hex", names := ["--atqa"], kind := .hex, required := true },
        { key := "sak", help := "SAK, hex", names := ["--sak"], kind := .hex, required := true },
        { key := "ats", help := "ATS, hex", names := ["--ats"], kind := .hex } ] } <|
    deviceRequired fun c a => do
      let uid := a.hex? "uid" |>.getD .empty
      let atqa := a.hex? "atqa" |>.getD .empty
      let sak := a.hex? "sak" |>.getD .empty
      let ats := a.hex? "ats" |>.getD .empty
      unless sak.size == 1 do throw (CliError.usage "sak must be 1 byte").toIO
      let _ ← unwrap (expectResponse (← c.hf14a4SetAntiColl uid atqa sak[0]! ats) [Status.success.toUInt16]
        "emv anticoll set")
      IO.println (green " - Ok")

private def emvStaticAddCmd : CliTree :=
  mkLeaf "add" "Add a static response"
    { description := "Register a static command→response APDU pair"
      specs := [
        { key := "cmd", names := ["--cmd"], kind := .hex, required := true, metavar := "hex" },
        { key := "resp", names := ["--resp"], kind := .hex, required := true, metavar := "hex" } ] } <|
    deviceRequired fun c a => do
      let cmd := a.hex? "cmd" |>.getD .empty
      let resp := a.hex? "resp" |>.getD .empty
      let _ ← unwrap (expectResponse (← c.hf14a4AddStaticResponse cmd resp) [Status.success.toUInt16]
        "emv static add")
      IO.println (green " - Ok")

private def emvStaticClearCmd : CliTree :=
  mkLeaf "clear" "Clear static responses" { description := "Clear all static APDU responses" } <|
    deviceRequired fun c _ => do
      let _ ← unwrap (expectResponse (← c.hf14a4ClearStaticResponses) [Status.success.toUInt16]
        "emv static clear")
      IO.println (green " - Ok")

private def emvRelayRecvCmd : CliTree :=
  mkLeaf "recv" "Poll for a pending APDU" { description := "Non-blocking poll for a pending T=CL APDU" } <|
    deviceRequired fun c _ => do
      let r ← c.hf14a4ApduRecv
      if r.data.isEmpty then IO.println (yellow "No pending APDU.")
      else IO.println (toHex r.data)

private def emvRelaySendCmd : CliTree :=
  mkLeaf "send" "Send an APDU response"
    { description := "Send an APDU response into the T=CL stack"
      specs := [{ key := "resp", help := "APDU response bytes, hex", required := true }] } <|
    deviceRequired fun c a => do
      let resp ← match Cli.ofHex (a.str? "resp" |>.getD "") with
        | .ok b => pure b
        | .error e => throw (CliError.usage e).toIO
      let _ ← unwrap (expectResponse (← c.hf14a4ApduSend resp) [Status.success.toUInt16] "emv relay send")
      IO.println (green " - Ok")

def hfGroup : CliTree :=
  grp "hf" "High-frequency (13.56 MHz) commands" [
    grp "14a" "ISO14443-A" [
      scanCmd,
      scanKeepCmd,
      watchCmd,
      hf14aRawCmd,
      todoLeaf "sniff" "Sniff reader frames",
      grp "config" "Reader config" [ hf14aConfigGetCmd, hf14aConfigSetCmd ],
      grp "anticoll" "Emulated anti-collision data" [ anticollGetCmd, anticollSetCmd ] ],
    grp "mf" "MIFARE Classic" [
      mfInfoCmd,
      mfNtCmd,
      mfNtdistCmd,
      todoLeaf "nested" "Nested attack: collect nonces",
      todoLeaf "staticnested" "Static-nested: collect nonces",
      todoLeaf "hardnested" "Hardnested: collect nonces",
      todoLeaf "encnested" "Static-encrypted-nested: collect nonces",
      todoLeaf "darkside" "Darkside: collect parameters",
      mfAuthCmd,
      mfRdblCmd,
      mfWrblCmd,
      mfValueCmd,
      mfCheckCmd,
      mfCheckBlkCmd,
      mfAuthTraceCmd,
      mfEreadCmd,
      mfEloadCmd,
      hfMfEconfig ],
    grp "mfu" "MIFARE Ultralight / NTAG" [
      mfuPagesCmd,
      mfuRdpgCmd,
      mfuWrpgCmd,
      mfuResetauthCmd,
      grp "counter" "NTAG counters" [ mfuCounterGetCmd, mfuCounterSetCmd ],
      grp "version" "GET_VERSION data" [ mfuVersionGetCmd, mfuVersionSetCmd ],
      grp "signature" "Signature data" [ mfuSignatureGetCmd, mfuSignatureSetCmd ],
      grp "uidmagic" "UID magic mode" [ mfuUidmagicGetCmd, mfuUidmagicSetCmd ],
      grp "write" "Emulator write mode" [ mfuWriteGetCmd, mfuWriteSetCmd ],
      grp "detection" "NTAG password logging" [
        mfEconfigToggleCmd "enable" "Enable/disable detection" "Enable password logging" "Disable password logging"
          (fun c e => c.mf0NtagSetDetectionEnable e),
        mfuDetectionCountCmd,
        mfuDetectionLogCmd ] ],
    grp "seos" "SEOS emulation" [ seosReadCmd, seosWriteCmd, seosKeysCmd ],
    grp "emv" "EMV / ISO14443-4 T=CL" [
      emvScanCmd,
      emvApduCmd,
      grp "anticoll" "T=CL emulation anti-collision" [ emvAnticollSetCmd ],
      grp "static" "Static APDU responses" [ emvStaticAddCmd, emvStaticClearCmd ],
      grp "relay" "APDU relay" [ emvRelayRecvCmd, emvRelaySendCmd ] ] ]

/-! ## `lf` group -/

/-- Big-endian hex for a `UInt32`. -/
private def hex32 (v : UInt32) : String := toHex (pushU32 .empty v)

/-- The read/write/emulate quartet most LF protocols share. Each protocol passes its own
commands; a missing one falls back to a `todoLeaf` stub. -/
private def lfProto (name help : String) (extra : List CliTree := [])
    (read : CliTree := todoLeaf "read" "Read a card")
    (write : CliTree := todoLeaf "write" "Write a card onto T55xx")
    (emuGet : CliTree := todoLeaf "get" "Get emulated id")
    (emuSet : CliTree := todoLeaf "set" "Set emulated id") : CliTree :=
  grp name help ([ read, write, grp "emu" "Emulated id" [ emuGet, emuSet ] ] ++ extra)

private def em410xReadCmd : CliTree :=
  mkLeaf "read" "Scan an EM410x/Electra tag and print its id"
      { description := "Scan em410x tag and print id" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.em410xScan) [Status.lfTagOk.toUInt16] "em410x read" with
      | .error e => throw e.toIO
      | .ok d =>
        let tagType := readU16 d 0
        let name := (TagSpecificType.ofUInt16? tagType).map (·.description) |>.getD s!"tag {tagType}"
        let idLen := if tagType == TagSpecificType.em410xElectra.toUInt16 then 13 else 5
        IO.println s!"{name}: {green (toHex (d.extract 2 (2 + idLen)))}"

private def em410xWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Write an EM410x (5-byte) or Electra (13-byte) id onto a T55xx tag"
      specs := [{ key := "id", names := ["--id"], kind := .hex, required := true, metavar := "hex" }] } <|
    readerRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 5 ∨ id.size == 13 do throw (CliError.usage "id must be 5 or 13 bytes").toIO
      let _ ← unwrap (expectResponse (← c.em410xWriteToT55xx id) [Status.lfTagOk.toUInt16] "em410x write")
      IO.println (green s!" - EM410x id {toHex id} write done.")

private def em410xEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated EM410x id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.em410xGetEmuId) [Status.success.toUInt16] "em410x emu get")
      IO.println (toHex d)

private def em410xEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id"
    { description := "Set the emulated EM410x id"
      specs := [{ key := "id", names := ["--id"], kind := .hex, required := true, metavar := "hex" }] } <|
    deviceRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 5 ∨ id.size == 13 do throw (CliError.usage "id must be 5 or 13 bytes").toIO
      let _ ← unwrap (expectResponse (← c.em410xSetEmuId id) [Status.success.toUInt16] "em410x emu set")
      IO.println (green " - Ok")

private def em4x05ReadCmd : CliTree :=
  mkLeaf "read" "Read a tag"
    { description := "Scan an EM4x05/EM4x69 tag (reader-talk-first) and print its config/UID"
      specs := [{ key := "pwd", names := ["--pwd"], kind := .hex, metavar := "hex4",
                  help := "32-bit password for LOGIN (default 0)" }] } <|
    readerRequired fun c a => do
      let pwdBytes := a.hex? "pwd" |>.getD .empty
      unless pwdBytes.isEmpty ∨ pwdBytes.size == 4 do throw (CliError.usage "pwd must be 4 bytes").toIO
      let pwd := if pwdBytes.isEmpty then 0 else readU32 pwdBytes 0
      match expectResponse (← c.em4x05Scan pwd) [Status.lfTagOk.toUInt16] "em4x05 read" with
      | .error e => throw e.toIO
      | .ok d =>
        let config := readU32 d 0
        let uid := readU32 d 4
        let uidHi := readU32 d 8
        let isEm4x69 := d[12]! != 0
        let uidBlock := d[13]!
        IO.println s!"Tag type : {if isEm4x69 then "EM4x69" else "EM4x05"}"
        IO.println s!"Config   : 0x{hex32 config}"
        IO.println s!"UID block: {uidBlock}"
        if (config >>> 6) &&& 1 == 1 then
          IO.println s!"Auth     : LOGIN used (pwd={if pwdBytes.isEmpty then "00000000" else toHex pwdBytes})"
        if isEm4x69 then IO.println s!"UID (64) : {hex32 uidHi}{hex32 uid}"
        else IO.println s!"UID      : {hex32 uid}"

/-! ### `lf hid` -/

private def hidFormatSpec (required : Bool := false) : ArgSpec :=
  { key := "format", names := ["-f", "--format"], required, metavar := "FORMAT",
    choices := enumChoices HIDFormat.all (·.name), help := "HIDProx card format" }

private def printHidFields (d : ByteArray) : IO Unit := do
  let fmt := (HIDFormat.ofUInt8? d[0]!).map toString |>.getD "unknown"
  let fc := readU32 d 1
  let cn1 := d[5]!
  let cn2 := readU32 d 6
  let il := d[10]!
  let oem := readU16 d 11
  let cn : Nat := cn1.toNat * 4294967296 + cn2.toNat
  IO.println s!"HIDProx/{fmt}"
  if fc > 0 then IO.println s!"  FC: {fc}"
  if il > 0 then IO.println s!"  IL: {il}"
  if oem > 0 then IO.println s!"  OEM: {oem}"
  IO.println s!"  CN: {cn}"

private def hidCardSpecs : List ArgSpec := [
  hidFormatSpec,
  { key := "fc", names := ["--fc"], kind := .int, metavar := "int", help := "Facility code" },
  { key := "cn", names := ["--cn"], kind := .int, required := true, metavar := "int", help := "Card number" },
  { key := "il", names := ["--il"], kind := .int, metavar := "int", help := "Issue level" },
  { key := "oem", names := ["--oem"], kind := .int, metavar := "int", help := "OEM code" } ]

private def hidComposeFromArgs (a : Args) : ByteArray :=
  let fmt := parseEnum HIDFormat.all (·.name) (a.str? "format" |>.getD "") |>.getD .h10301
  let fc := (a.int? "fc").getD 0
  let cn := (a.int? "cn").getD 0
  let il := (a.int? "il").getD 0
  let oem := (a.int? "oem").getD 0
  let cn1 : UInt8 := (cn / 4294967296).toNat.toUInt8
  let cn2 : UInt32 := (cn % 4294967296).toNat.toUInt32
  ByteArray.mk #[fmt.toUInt8] ++ pushU32 .empty fc.toNat.toUInt32 ++ ByteArray.mk #[cn1] ++
    pushU32 .empty cn2 ++ ByteArray.mk #[il.toNat.toUInt8] ++ pushU16 .empty oem.toNat.toUInt16

private def hidReadCmd : CliTree :=
  mkLeaf "read" "Scan a HIDProx tag"
    { description := "Scan a HIDProx tag and print its fields", specs := [hidFormatSpec] } <|
    readerRequired fun c a => do
      let fmt := parseEnum HIDFormat.all (·.name) (a.str? "format" |>.getD "") |>.getD .h10301
      match expectResponse (← c.hidproxScan fmt) [Status.lfTagOk.toUInt16] "hid read" with
      | .error e => throw e.toIO
      | .ok d => printHidFields d

private def hidWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Compose a HIDProx card and write it onto a T55xx tag", specs := hidCardSpecs } <|
    readerRequired fun c a => do
      let id := hidComposeFromArgs a
      let _ ← unwrap (expectResponse (← c.hidproxWriteToT55xx id) [Status.lfTagOk.toUInt16] "hid write")
      IO.println (green " - Write done.")
      printHidFields id

private def hidEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated HIDProx id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.hidproxGetEmuId) [Status.success.toUInt16] "hid emu get")
      printHidFields d

private def hidEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id" { description := "Set the emulated HIDProx id", specs := hidCardSpecs } <|
    deviceRequired fun c a => do
      let id := hidComposeFromArgs a
      let _ ← unwrap (expectResponse (← c.hidproxSetEmuId id) [Status.success.toUInt16] "hid emu set")
      IO.println (green " - Ok")

/-! ### `lf ioprox` -/

private def ioproxFields (d : ByteArray) : IO Unit := do
  let ver := d[0]!
  let fc := d[1]!
  let cn := readU16 d 2
  let raw := d.extract 4 12
  IO.println "ioProx XSF format"
  IO.println s!"  Version: {ver}"
  IO.println s!"  Facility: {fc} [0x{hexByte fc}]"
  IO.println s!"  ID: {cn}"
  IO.println s!"  Raw: {toHex raw}"

private def ioproxReadCmd : CliTree :=
  mkLeaf "read" "Scan an ioProx tag" { description := "Scan an ioProx tag and print its fields" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.ioproxScan) [Status.lfTagOk.toUInt16] "ioprox read" with
      | .error e => throw e.toIO
      | .ok d => ioproxFields d

/-- Decode `--raw8`, or compose from `--ver`/`--fc`/`--cn`; returns the firmware's 16-byte
card-data structure. -/
private def ioproxResolve (c : Client) (a : Args) : IO ByteArray := do
  match a.hex? "raw8" with
  | some raw =>
    unless raw.size == 8 do throw (CliError.usage "raw8 must be 8 bytes").toIO
    unwrap (expectResponse (← c.ioproxDecodeRaw raw) [Status.success.toUInt16] "ioprox decode")
  | none =>
    let ver := (a.int? "ver").getD 1
    let fc := (a.int? "fc").getD 0
    let cn := (a.int? "cn").getD 0
    unwrap (expectResponse (← c.ioproxComposeId ver.toNat.toUInt8 fc.toNat.toUInt8 cn.toNat.toUInt16)
      [Status.success.toUInt16] "ioprox compose")

private def ioproxCardSpecs : List ArgSpec := [
  { key := "ver", names := ["--ver"], kind := .int, metavar := "int", help := "ioProx version" },
  { key := "fc", names := ["--fc"], kind := .int, metavar := "int", help := "Facility code" },
  { key := "cn", names := ["--cn"], kind := .int, metavar := "int", help := "Card number" },
  { key := "raw8", names := ["--raw8"], kind := .hex, metavar := "hex16", help := "Raw 8 bytes (takes priority)" } ]

private def ioproxWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Write ioProx card data onto a T55xx tag", specs := ioproxCardSpecs } <|
    readerRequired fun c a => do
      let d ← ioproxResolve c a
      let _ ← unwrap (expectResponse (← c.ioproxWriteToT55xx (d.extract 0 16)) [Status.lfTagOk.toUInt16]
        "ioprox write")
      ioproxFields d
      IO.println (green "Write done.")

private def ioproxEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated ioProx id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.ioproxGetEmuId) [Status.success.toUInt16] "ioprox emu get")
      ioproxFields d

private def ioproxEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id"
    { description := "Set the emulated ioProx id", specs := ioproxCardSpecs } <|
    deviceRequired fun c a => do
      let d ← ioproxResolve c a
      let _ ← unwrap (expectResponse (← c.ioproxSetEmuId (d.extract 0 16)) [Status.success.toUInt16]
        "ioprox emu set")
      ioproxFields d

private def ioproxDecodeCmd : CliTree :=
  mkLeaf "decode" "Decode 8 raw bytes"
    { description := "Decode 8 raw ioProx bytes into version/facility/card number"
      specs := [{ key := "raw8", help := "8 raw bytes, hex", required := true }] } <|
    deviceRequired fun c a => do
      let raw ← match Cli.ofHex (a.str? "raw8" |>.getD "") with
        | .ok b => pure b
        | .error e => throw (CliError.usage e).toIO
      unless raw.size == 8 do throw (CliError.usage "raw8 must be 8 bytes").toIO
      let d ← unwrap (expectResponse (← c.ioproxDecodeRaw raw) [Status.success.toUInt16] "ioprox decode")
      ioproxFields d

private def ioproxComposeCmd : CliTree :=
  mkLeaf "compose" "Compose from ver/fc/cn"
    { description := "Compose an ioProx frame from version/facility/card number"
      specs := [
        { key := "ver", names := ["--ver"], kind := .int, required := true, metavar := "int" },
        { key := "fc", names := ["--fc"], kind := .int, required := true, metavar := "int" },
        { key := "cn", names := ["--cn"], kind := .int, required := true, metavar := "int" } ] } <|
    deviceRequired fun c a => do
      let d ← ioproxResolve c a
      ioproxFields d

/-! ### `lf viking` -/

private def vikingIdSpec : ArgSpec :=
  { key := "id", names := ["--id"], kind := .hex, required := true, metavar := "hex8" }

private def vikingReadCmd : CliTree :=
  mkLeaf "read" "Scan a Viking tag" { description := "Scan a Viking tag and print its id" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.vikingScan) [Status.lfTagOk.toUInt16] "viking read" with
      | .error e => throw e.toIO
      | .ok d => IO.println s!"Viking: {green (toHex d)}"

private def vikingWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Write a Viking id onto a T55xx tag", specs := [vikingIdSpec] } <|
    readerRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 4 do throw (CliError.usage "id must be 4 bytes").toIO
      let _ ← unwrap (expectResponse (← c.vikingWriteToT55xx id) [Status.lfTagOk.toUInt16] "viking write")
      IO.println (green s!" - Viking id {toHex id} write done.")

private def vikingEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated Viking id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.vikingGetEmuId) [Status.success.toUInt16] "viking emu get")
      IO.println (toHex d)

private def vikingEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id" { description := "Set the emulated Viking id", specs := [vikingIdSpec] } <|
    deviceRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 4 do throw (CliError.usage "id must be 4 bytes").toIO
      let _ ← unwrap (expectResponse (← c.vikingSetEmuId id) [Status.success.toUInt16] "viking emu set")
      IO.println (green " - Ok")

/-! ### `lf pac` -/

private def pacCnSpec : ArgSpec :=
  { key := "cn", names := ["--cn"], required := true, metavar := "<8 ascii>",
    help := "8 ASCII characters, e.g. CARD0001" }

private def pacCnBytes (a : Args) : IO ByteArray := do
  let s := a.str? "cn" |>.getD ""
  unless s.length == 8 do throw (CliError.usage "card number must be exactly 8 characters").toIO
  return s.toUTF8

private def pacAscii (d : ByteArray) : String :=
  String.ofList (d.toList.map fun b => if 0x20 ≤ b ∧ b < 0x7f then Char.ofNat b.toNat else '.')

private def pacReadCmd : CliTree :=
  mkLeaf "read" "Scan a PAC/Stanley tag" { description := "Scan a PAC/Stanley tag and print its card id" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.pacScan) [Status.lfTagOk.toUInt16] "pac read" with
      | .error e => throw e.toIO
      | .ok d => IO.println s!"PAC/Stanley: {green (pacAscii d)} ({toHex d})"

private def pacWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Write a PAC/Stanley id onto a T55xx tag", specs := [pacCnSpec] } <|
    readerRequired fun c a => do
      let id ← pacCnBytes a
      let _ ← unwrap (expectResponse (← c.pacWriteToT55xx id) [Status.lfTagOk.toUInt16] "pac write")
      IO.println (green s!" - PAC/Stanley write done - CN: {pacAscii id}")

private def pacEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated PAC/Stanley card id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.pacGetEmuId) [Status.success.toUInt16] "pac emu get")
      IO.println s!"CN: {pacAscii d} ({toHex d})"

private def pacEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id"
    { description := "Set the emulated PAC/Stanley card id", specs := [pacCnSpec] } <|
    deviceRequired fun c a => do
      let id ← pacCnBytes a
      let _ ← unwrap (expectResponse (← c.pacSetEmuId id) [Status.success.toUInt16] "pac emu set")
      IO.println (green " - Ok")

/-! ### `lf jablotron` -/

private def jablotronIdSpec : ArgSpec :=
  { key := "id", names := ["--id"], kind := .hex, required := true, metavar := "hex10",
    help := "5-byte Jablotron id" }

private def jablotronReadCmd : CliTree :=
  mkLeaf "read" "Scan a Jablotron tag" { description := "Scan a Jablotron tag and print its id" } <|
    readerRequired fun c _ => do
      match expectResponse (← c.jablotronScan) [Status.lfTagOk.toUInt16] "jablotron read" with
      | .error e => throw e.toIO
      | .ok d => IO.println s!"Jablotron: {green (toHex d)}"

private def jablotronWriteCmd : CliTree :=
  mkLeaf "write" "Write a card onto T55xx"
    { description := "Write a Jablotron id onto a T55xx tag", specs := [jablotronIdSpec] } <|
    readerRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 5 do throw (CliError.usage "id must be 5 bytes").toIO
      let _ ← unwrap (expectResponse (← c.jablotronWriteToT55xx id) [Status.lfTagOk.toUInt16]
        "jablotron write")
      IO.println (green s!" - Jablotron id {toHex id} write done.")

private def jablotronEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated id" { description := "Get the emulated Jablotron id" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.jablotronGetEmuId) [Status.success.toUInt16] "jablotron emu get")
      IO.println (toHex d)

private def jablotronEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated id"
    { description := "Set the emulated Jablotron id", specs := [jablotronIdSpec] } <|
    deviceRequired fun c a => do
      let id := a.hex? "id" |>.getD .empty
      unless id.size == 5 do throw (CliError.usage "id must be 5 bytes").toIO
      let _ ← unwrap (expectResponse (← c.jablotronSetEmuId id) [Status.success.toUInt16]
        "jablotron emu set")
      IO.println (green " - Ok")

/-! ### `lf idteck` -/

private def idteckPreamble : UInt32 := 0x4944544B

private def idteckChecksum (lo3 : UInt32) : UInt8 := ((lo3 >>> 16) + (lo3 >>> 8) + lo3).toUInt8

private def idteckIdSpec : ArgSpec :=
  { key := "id", names := ["--id"], required := true, metavar := "hex",
    help := "IDTECK frame: 16 hex chars for the full frame, or 8 for the payload \
      (preamble auto-prepended)" }

private def idteckIdArg (a : Args) : IO ByteArray := do
  let raw := a.str? "id" |>.getD ""
  let hex ← match raw.length with
    | 16 => pure raw
    | 8 => pure (hex32 idteckPreamble ++ raw)
    | _ => throw (CliError.usage "id must be 8 or 16 HEX symbols").toIO
  match Cli.ofHex hex with
  | .ok b => pure b
  | .error e => throw (CliError.usage e).toIO

private def idteckFrameInfo (frame : ByteArray) : IO Unit := do
  unless frame.size == 8 do throw (CliError.other "IDTECK frame must be 8 bytes").toIO
  let preamble := readU32 frame 0
  let payload := readU32 frame 4
  let chk := (payload >>> 24).toUInt8
  let lo3 : UInt32 := payload &&& 0xFFFFFF
  let expected := idteckChecksum lo3
  -- The card id is the byte-reversal of `lo3`'s three bytes (matches PM3's layout).
  let lo3n := lo3.toNat
  let b0 := lo3n % 256
  let b1 := (lo3n / 256) % 256
  let b2 := (lo3n / 65536) % 256
  let cardId : Nat := b0 * 65536 + b1 * 256 + b2
  let cardIdHex := toHex (ByteArray.mk #[b0.toUInt8, b1.toUInt8, b2.toUInt8])
  IO.println s!"Preamble : {hex32 preamble}{if preamble == idteckPreamble then "" else red " (not IDTK)"}"
  IO.println s!"Payload  : {hex32 payload}"
  IO.println s!"Card ID  : {cardId} (0x{cardIdHex})"
  IO.println s!"Checksum : 0x{hexByte chk} (expected 0x{hexByte expected}, \
    {if chk == expected then green "ok" else yellow "mismatch"})"

private def idteckWriteCmd : CliTree :=
  mkLeaf "write" "Write a frame onto T55xx"
    { description := "Clone an IDTECK PSK1 frame onto a T55xx tag", specs := [idteckIdSpec] } <|
    readerRequired fun c a => do
      let id ← idteckIdArg a
      let _ ← unwrap (expectResponse (← c.idteckWriteToT55xx id) [Status.lfTagOk.toUInt16] "idteck write")
      IO.println (green s!" - IDTECK frame {toHex id} written to T55xx.")

private def idteckEmuGetCmd : CliTree :=
  mkLeaf "get" "Get emulated frame" { description := "Get the emulated IDTECK frame" } <|
    deviceRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.idteckGetEmuId) [Status.success.toUInt16] "idteck emu get")
      IO.println s!"Frame: {toHex d}"
      idteckFrameInfo d

private def idteckEmuSetCmd : CliTree :=
  mkLeaf "set" "Set emulated frame"
    { description := "Set the emulated IDTECK frame", specs := [idteckIdSpec] } <|
    deviceRequired fun c a => do
      let id ← idteckIdArg a
      let _ ← unwrap (expectResponse (← c.idteckSetEmuId id) [Status.success.toUInt16] "idteck emu set")
      IO.println (green " - Ok")

/-! ### `lf sniff` / `lf adc` -/

private def lfSniffCmd : CliTree :=
  mkLeaf "sniff" "Capture raw LF samples"
    { description := "Capture raw LF field ADC samples (125kHz, 8µs/sample)"
      specs := [
        { key := "timeout", names := ["--timeout"], kind := .int, metavar := "ms",
          help := "Capture duration in ms (default 2000, max 10000)" },
        { key := "hex", names := ["--hex"], kind := .flag, help := "Print a hex dump" } ] } <|
    readerRequired fun c a => do
      let timeout := (a.int? "timeout").map (·.toNat.toUInt16) |>.getD 2000
      let r ← c.lfSniff timeout
      if r.status != Status.lfTagOk.toUInt16 ∨ r.data.isEmpty then IO.println (red "No samples captured")
      else
        let d := r.data
        let n := d.size
        let mn := d.toList.foldl min d[0]!
        let mx := d.toList.foldl max d[0]!
        let mean := (d.toList.foldl (fun acc b => acc + b.toNat) 0) / n
        IO.println s!"Captured : {n} bytes ({n * 8 / 1000}ms)"
        IO.println s!"Range    : 0x{hexByte mn} - 0x{hexByte mx}  mean: 0x{hexByte mean.toUInt8}"
        if a.has "hex" then printMemDump d

private def lfAdcCmd : CliTree :=
  mkLeaf "adc" "Read the ADC with field on" { description := "Read the ADC and return the array" } <|
    readerRequired fun c _ => do
      let d ← unwrap (expectResponse (← c.adcGenericRead) [Status.success.toUInt16] "adc")
      printMemDump d 25
      let avg := (d.toList.foldl (fun acc b => acc + b.toNat) 0) / d.size
      IO.println s!"avg: 0x{hexByte avg.toUInt8}"

def lfGroup : CliTree :=
  grp "lf" "Low-frequency (125 kHz) commands" [
    grp "em" "EM microelectronic" [
      lfProto "410x" "EM410x / Electra" (read := em410xReadCmd) (write := em410xWriteCmd)
        (emuGet := em410xEmuGetCmd) (emuSet := em410xEmuSetCmd),
      grp "4x05" "EM4x05 / EM4x69" [ em4x05ReadCmd ] ],
    lfProto "hid" "HID Prox" (read := hidReadCmd) (write := hidWriteCmd) (emuGet := hidEmuGetCmd)
      (emuSet := hidEmuSetCmd),
    lfProto "ioprox" "ioProx (XSF)" [ ioproxDecodeCmd, ioproxComposeCmd ] (read := ioproxReadCmd)
      (write := ioproxWriteCmd) (emuGet := ioproxEmuGetCmd) (emuSet := ioproxEmuSetCmd),
    lfProto "viking" "Viking" (read := vikingReadCmd) (write := vikingWriteCmd) (emuGet := vikingEmuGetCmd)
      (emuSet := vikingEmuSetCmd),
    lfProto "pac" "PAC/Stanley" (read := pacReadCmd) (write := pacWriteCmd) (emuGet := pacEmuGetCmd)
      (emuSet := pacEmuSetCmd),
    lfProto "jablotron" "Jablotron" (read := jablotronReadCmd) (write := jablotronWriteCmd)
      (emuGet := jablotronEmuGetCmd) (emuSet := jablotronEmuSetCmd),
    grp "idteck" "IDTECK" [
      idteckWriteCmd,
      grp "emu" "Emulated frame" [ idteckEmuGetCmd, idteckEmuSetCmd ] ],
    lfSniffCmd,
    lfAdcCmd ]

end Chamelean.Cli
