import Chamelean

/-!
Test harness: connects to a Chameleon Ultra and prints device info, or sends one raw command.

    chamelean                       auto-detect the USB port and print device info
    chamelean /dev/cu.usbmodem1101  same, explicit port (Linux: /dev/ttyACM0, or tcp:host:port)
    chamelean PORT scan             scan for 14a (13.56 MHz) tags (needs reader mode)
    chamelean PORT ble-key          print the stored BLE pairing passcode
    chamelean PORT ble-pairing      show whether BLE pairing is enabled
    chamelean PORT ble-pairing on   enable BLE pairing (off to disable)
    chamelean PORT CMD [HEX]        send raw command CMD (decimal) with optional hex payload
-/
open Chamelean

def hex (bytes : ByteArray) : String :=
  String.join <| bytes.toList.map fun b =>
    let s := String.ofList (Nat.toDigits 16 b.toNat)
    if s.length < 2 then "0" ++ s else s

def parseHex (s : String) : Except String ByteArray := do
  let digit (c : Char) : Except String Nat :=
    if c.isDigit then pure (c.toNat - '0'.toNat)
    else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then pure (c.toLower.toNat - 'a'.toNat + 10)
    else throw s!"not a hex digit: {c}"
  let chars := s.toList
  if chars.length % 2 != 0 then throw "hex payload needs an even number of digits"
  let rec go : List Char → Except String (List UInt8)
    | [] => pure []
    | hi :: lo :: rest => do pure ((16 * (← digit hi) + (← digit lo)).toUInt8 :: (← go rest))
    | _ => throw "unreachable"
  return ⟨(← go chars).toArray⟩

/-- First USB CDC serial device: `/dev/cu.usbmodem*` on macOS, `/dev/ttyACM*` on Linux. -/
def detectPort : IO String := do
  let pfx := if System.Platform.isOSX then "cu.usbmodem" else "ttyACM"
  let entries ← System.FilePath.readDir "/dev"
  let candidates := entries.filter (·.fileName.startsWith pfx) |>.map (·.fileName) |>.qsort (· < ·)
  match candidates[0]? with
  | some name => return s!"/dev/{name}"
  | none => throw <| IO.userError s!"no /dev/{pfx}* device found; pass the port explicitly"

def showResponse (r : Response) : String :=
  s!"status {Status.describe r.status}, data {hex r.data} ({r.data.size} bytes)"

def deviceInfo (c : Client) : IO Unit := do
  let ver ← c.sendCmd .getAppVersion
  IO.println s!"app version   : {ver.data[0]!}.{ver.data[1]!}"
  let git ← c.sendCmd .getGitVersion
  IO.println s!"git version   : {String.fromUTF8! git.data}"
  let model ← c.sendCmd .getDeviceModel
  IO.println s!"model         : {if model.data[0]! == 0 then "Ultra" else "Lite"}"
  let chip ← c.sendCmd .getDeviceChipId
  IO.println s!"chip id       : {hex chip.data}"
  let addr ← c.sendCmd .getDeviceAddress
  IO.println s!"ble address   : {hex addr.data}"
  let mode ← c.sendCmd .getDeviceMode
  IO.println s!"mode          : {if mode.data[0]! == 1 then "reader" else "tag"}"
  let bat ← c.sendCmd .getBatteryInfo
  IO.println s!"battery       : {readU16 bat.data 0} mV, {bat.data[2]!}%"
  let caps ← c.loadCapabilities
  IO.println s!"capabilities  : {caps.size} commands"

def scan (c : Client) : IO Unit := do
  let tags ← c.hf14aScan
  if tags.isEmpty then
    IO.println "no 14a tag in field"
  for t in tags do
    IO.println s!"uid {hex t.uid}  atqa {hex t.atqa}  sak {hex (.mk #[t.sak])}\
      {if t.ats.isEmpty then "" else s!"  ats {hex t.ats}"}"

def blePairing (c : Client) (arg : List String) : IO Unit := do
  match arg with
  | [] => IO.println s!"ble pairing: {if ← c.getBlePairingEnable then "enabled" else "disabled"}"
  | "on" :: _ => c.setBlePairingEnable true;  IO.println "ble pairing enabled"
  | "off" :: _ => c.setBlePairingEnable false; IO.println "ble pairing disabled"
  | x :: _ => throw <| IO.userError s!"ble-pairing takes on|off or nothing, got: {x}"

def run (port : String) (rest : List String) : IO Unit := do
  IO.println s!"opening {port}"
  let c ← Client.connect port
  try
    match rest with
    | [] => deviceInfo c
    | "scan" :: _ => scan c
    | "ble-key" :: _ => IO.println s!"ble pairing key: {← c.getBlePairingKey}"
    | "ble-pairing" :: arg => blePairing c arg
    | cmdStr :: payload =>
      let some cmd := cmdStr.toNat? | throw <| IO.userError s!"bad command number: {cmdStr}"
      let data ← match payload with
        | [] => pure ByteArray.empty
        | h :: _ => IO.ofExcept (parseHex h |>.mapError IO.userError)
      IO.println s!"-> cmd {Command.describe cmd.toUInt16} data {hex data}"
      let r ← c.send cmd.toUInt16 data
      IO.println s!"<- {showResponse r}"
  finally
    c.close

def main (args : List String) : IO UInt32 := do
  try
    match args with
    | [] => run (← detectPort) []
    | port :: rest => run port rest
    return 0
  catch e =>
    IO.eprintln s!"error: {e}"
    return 1
