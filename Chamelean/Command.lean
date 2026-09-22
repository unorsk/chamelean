/-! Command and status codes understood by the Chameleon Ultra firmware. -/
namespace Chamelean

inductive Command
  | getAppVersion | changeDeviceMode | getDeviceMode | setActiveSlot | setSlotTagType
  | setSlotDataDefault | setSlotEnable | setSlotTagNick | getSlotTagNick | slotDataConfigSave
  | enterBootloader | getDeviceChipId | getDeviceAddress | saveSettings | resetSettings
  | setAnimationMode | getAnimationMode | getGitVersion | getActiveSlot | getSlotInfo
  | wipeFds | deleteSlotTagNick | getEnabledSlots | deleteSlotSenseType | getBatteryInfo
  | getButtonPressConfig | setButtonPressConfig | getLongButtonPressConfig | setLongButtonPressConfig
  | setBlePairingKey | getBlePairingKey | deleteAllBleBonds | getDeviceModel | getDeviceSettings
  | getDeviceCapabilities | getBlePairingEnable | setBlePairingEnable
  | hf14aScan | mf1DetectSupport | mf1DetectPrng | hf14aRaw
  | em410xScan
deriving Repr, DecidableEq, Inhabited

def Command.toUInt16 : Command → UInt16
  | .getAppVersion => 1000 | .changeDeviceMode => 1001 | .getDeviceMode => 1002
  | .setActiveSlot => 1003 | .setSlotTagType => 1004 | .setSlotDataDefault => 1005
  | .setSlotEnable => 1006 | .setSlotTagNick => 1007 | .getSlotTagNick => 1008
  | .slotDataConfigSave => 1009 | .enterBootloader => 1010 | .getDeviceChipId => 1011
  | .getDeviceAddress => 1012 | .saveSettings => 1013 | .resetSettings => 1014
  | .setAnimationMode => 1015 | .getAnimationMode => 1016 | .getGitVersion => 1017
  | .getActiveSlot => 1018 | .getSlotInfo => 1019 | .wipeFds => 1020
  | .deleteSlotTagNick => 1021 | .getEnabledSlots => 1023 | .deleteSlotSenseType => 1024
  | .getBatteryInfo => 1025 | .getButtonPressConfig => 1026 | .setButtonPressConfig => 1027
  | .getLongButtonPressConfig => 1028 | .setLongButtonPressConfig => 1029
  | .setBlePairingKey => 1030 | .getBlePairingKey => 1031 | .deleteAllBleBonds => 1032
  | .getDeviceModel => 1033 | .getDeviceSettings => 1034 | .getDeviceCapabilities => 1035
  | .getBlePairingEnable => 1036 | .setBlePairingEnable => 1037
  | .hf14aScan => 2000 | .mf1DetectSupport => 2001 | .mf1DetectPrng => 2002 | .hf14aRaw => 2010
  | .em410xScan => 3000

def Command.all : List Command :=
  [.getAppVersion, .changeDeviceMode, .getDeviceMode, .setActiveSlot, .setSlotTagType,
   .setSlotDataDefault, .setSlotEnable, .setSlotTagNick, .getSlotTagNick, .slotDataConfigSave,
   .enterBootloader, .getDeviceChipId, .getDeviceAddress, .saveSettings, .resetSettings,
   .setAnimationMode, .getAnimationMode, .getGitVersion, .getActiveSlot, .getSlotInfo,
   .wipeFds, .deleteSlotTagNick, .getEnabledSlots, .deleteSlotSenseType, .getBatteryInfo,
   .getButtonPressConfig, .setButtonPressConfig, .getLongButtonPressConfig, .setLongButtonPressConfig,
   .setBlePairingKey, .getBlePairingKey, .deleteAllBleBonds, .getDeviceModel, .getDeviceSettings,
   .getDeviceCapabilities, .getBlePairingEnable, .setBlePairingEnable,
   .hf14aScan, .mf1DetectSupport, .mf1DetectPrng, .hf14aRaw, .em410xScan]

def Command.ofUInt16? (code : UInt16) : Option Command :=
  Command.all.find? (·.toUInt16 == code)

/-- Constructor name without the namespace, e.g. `getAppVersion`. -/
def Command.name (c : Command) : String :=
  ((repr c).pretty.splitOn ".").getLast!

/-- `1000 getAppVersion`, or `1234 (unknown)`. -/
def Command.describe (code : UInt16) : String :=
  s!"{code} {(Command.ofUInt16? code).map (·.name) |>.getD "(unknown)"}"

inductive Status
  | hfTagOk | hfTagNo | hfErrStat | hfErrCrc | hfCollision | hfErrBcc | mfErrAuth | hfErrParity | hfErrAts
  | lfTagOk | em410xTagNoFound
  | parErr | deviceModeError | invalidCmd | success | notImplemented
  | flashWriteFail | flashReadFail | invalidSlotType
deriving Repr, DecidableEq, Inhabited

def Status.toUInt16 : Status → UInt16
  | .hfTagOk => 0x00 | .hfTagNo => 0x01 | .hfErrStat => 0x02 | .hfErrCrc => 0x03
  | .hfCollision => 0x04 | .hfErrBcc => 0x05 | .mfErrAuth => 0x06 | .hfErrParity => 0x07
  | .hfErrAts => 0x08 | .lfTagOk => 0x40 | .em410xTagNoFound => 0x41
  | .parErr => 0x60 | .deviceModeError => 0x66 | .invalidCmd => 0x67 | .success => 0x68
  | .notImplemented => 0x69 | .flashWriteFail => 0x70 | .flashReadFail => 0x71
  | .invalidSlotType => 0x72

def Status.all : List Status :=
  [.hfTagOk, .hfTagNo, .hfErrStat, .hfErrCrc, .hfCollision, .hfErrBcc, .mfErrAuth, .hfErrParity,
   .hfErrAts, .lfTagOk, .em410xTagNoFound, .parErr, .deviceModeError, .invalidCmd, .success,
   .notImplemented, .flashWriteFail, .flashReadFail, .invalidSlotType]

def Status.ofUInt16? (code : UInt16) : Option Status :=
  Status.all.find? (·.toUInt16 == code)

def Status.name (s : Status) : String :=
  ((repr s).pretty.splitOn ".").getLast!

def Status.describe (code : UInt16) : String :=
  s!"0x{String.ofList (Nat.toDigits 16 code.toNat)} {(Status.ofUInt16? code).map (·.name) |>.getD "(unknown)"}"

end Chamelean
