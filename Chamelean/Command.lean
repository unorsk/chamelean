import Chamelean.Enum

/-! Command and status codes understood by the Chameleon Ultra firmware. -/
namespace Chamelean

/-- Every command the firmware exposes. Grouped as in the firmware: 1xxx device, 2xxx HF
reader, 3xxx LF reader, 4xxx emulation, 5xxx LF emulation IDs, 6xxx ISO14443-4 T=CL. -/
uint_enum Command : UInt16 where
  -- Device
  | getAppVersion := 1000
  | changeDeviceMode := 1001
  | getDeviceMode := 1002
  | setActiveSlot := 1003
  | setSlotTagType := 1004
  | setSlotDataDefault := 1005
  | setSlotEnable := 1006
  | setSlotTagNick := 1007
  | getSlotTagNick := 1008
  | getAllSlotNicks := 1038
  | slotDataConfigSave := 1009
  | enterBootloader := 1010
  | getDeviceChipId := 1011
  | getDeviceAddress := 1012
  | saveSettings := 1013
  | resetSettings := 1014
  | setAnimationMode := 1015
  | getAnimationMode := 1016
  | getGitVersion := 1017
  | getActiveSlot := 1018
  | getSlotInfo := 1019
  | wipeFds := 1020
  | deleteSlotTagNick := 1021
  | getEnabledSlots := 1023
  | deleteSlotSenseType := 1024
  | getBatteryInfo := 1025
  | getButtonPressConfig := 1026
  | setButtonPressConfig := 1027
  | getLongButtonPressConfig := 1028
  | setLongButtonPressConfig := 1029
  | setBlePairingKey := 1030
  | getBlePairingKey := 1031
  | deleteAllBleBonds := 1032
  | getDeviceModel := 1033
  | getDeviceSettings := 1034
  | getDeviceCapabilities := 1035
  | getBlePairingEnable := 1036
  | setBlePairingEnable := 1037
  | getSleepTimeout := 1039
  | setSleepTimeout := 1040
  -- HF reader
  | hf14aScan := 2000
  | mf1DetectSupport := 2001
  | mf1DetectPrng := 2002
  | mf1StaticNestedAcquire := 2003
  | mf1DarksideAcquire := 2004
  | mf1DetectNtDist := 2005
  | mf1NestedAcquire := 2006
  | mf1AuthOneKeyBlock := 2007
  | mf1ReadOneBlock := 2008
  | mf1WriteOneBlock := 2009
  | hf14aRaw := 2010
  | mf1ManipulateValueBlock := 2011
  | mf1CheckKeysOfSectors := 2012
  | mf1HardnestedAcquire := 2013
  | mf1EncNestedAcquire := 2014
  | mf1CheckKeysOnBlock := 2015
  | hf14aScanKeep := 2016
  | hf14aAuthTrace := 2017
  | hf14aSniff := 2020
  | hf14aGetConfig := 2200
  | hf14aSetConfig := 2201
  -- LF reader
  | em410xScan := 3000
  | em410xWriteToT55xx := 3001
  | hidproxScan := 3002
  | hidproxWriteToT55xx := 3003
  | vikingScan := 3004
  | vikingWriteToT55xx := 3005
  | em410xElectraWriteToT55xx := 3006
  | adcGenericRead := 3009
  | ioproxScan := 3010
  | ioproxWriteToT55xx := 3011
  | ioproxDecodeRaw := 3012
  | ioproxComposeId := 3013
  | pacScan := 3014
  | pacWriteToT55xx := 3015
  | lfT55xxWrite := 3016
  | idteckWriteToT55xx := 3018
  | jablotronScan := 3019
  | jablotronWriteToT55xx := 3020
  | em4x05Scan := 3030
  | lfSniff := 3031
  | em4x05ReadSniff := 3032
  -- Emulation
  | mf1WriteEmuBlockData := 4000
  | hf14aSetAntiCollData := 4001
  | mf1SetDetectionEnable := 4004
  | mf1GetDetectionCount := 4005
  | mf1GetDetectionLog := 4006
  | mf1GetDetectionEnable := 4007          -- not implemented in firmware
  | mf1ReadEmuBlockData := 4008
  | mf1GetEmulatorConfig := 4009
  | mf1GetGen1aMode := 4010                -- not implemented in firmware
  | mf1SetGen1aMode := 4011
  | mf1GetGen2Mode := 4012                 -- not implemented in firmware
  | mf1SetGen2Mode := 4013
  | mf1GetBlockAntiCollMode := 4014        -- not implemented in firmware
  | mf1SetBlockAntiCollMode := 4015
  | mf1GetWriteMode := 4016                -- not implemented in firmware
  | mf1SetWriteMode := 4017
  | hf14aGetAntiCollData := 4018
  | mf0NtagGetUidMagicMode := 4019
  | mf0NtagSetUidMagicMode := 4020
  | mf0NtagReadEmuPageData := 4021
  | mf0NtagWriteEmuPageData := 4022
  | mf0NtagGetVersionData := 4023
  | mf0NtagSetVersionData := 4024
  | mf0NtagGetSignatureData := 4025
  | mf0NtagSetSignatureData := 4026
  | mf0NtagGetCounterData := 4027
  | mf0NtagSetCounterData := 4028
  | mf0NtagResetAuthCnt := 4029
  | mf0NtagGetPageCount := 4030
  | mf0NtagGetWriteMode := 4031
  | mf0NtagSetWriteMode := 4032
  | mf0NtagSetDetectionEnable := 4033
  | mf0NtagGetDetectionCount := 4034
  | mf0NtagGetDetectionLog := 4035
  | mf0NtagGetDetectionEnable := 4036
  | mf0NtagGetEmulatorConfig := 4037       -- not implemented in firmware
  | mf1SetFieldOffDoReset := 4038
  | mf1GetFieldOffDoReset := 4039
  | mf1GetPrngType := 4040
  | mf1SetPrngType := 4041
  | seosReadEmuData := 4042
  | seosWriteEmuData := 4043
  | seosWriteEmuKeys := 4044
  -- LF emulation IDs
  | em410xSetEmuId := 5000
  | em410xGetEmuId := 5001
  | hidproxSetEmuId := 5002
  | hidproxGetEmuId := 5003
  | vikingSetEmuId := 5004
  | vikingGetEmuId := 5005
  | pacSetEmuId := 5006
  | pacGetEmuId := 5007
  | ioproxSetEmuId := 5008
  | ioproxGetEmuId := 5009
  | jablotronSetEmuId := 5010
  | jablotronGetEmuId := 5011
  | idteckSetEmuId := 5012
  | idteckGetEmuId := 5013
  -- ISO14443-4 T=CL emulation
  | hf14a4ApduRecv := 6000
  | hf14a4ApduSend := 6001
  | hf14a4SetAntiColl := 6002
  | hf14a4StaticResp := 6003
  | hf14a4ReaderApdu := 6004
  | hf14a4EmvScan := 6005

/-- `1000 getAppVersion`, or `1234 (unknown)`. -/
def Command.describe (code : UInt16) : String :=
  s!"{code} {(Command.ofUInt16? code).map (·.name) |>.getD "(unknown)"}"

/-- Status byte returned in every response frame. -/
uint_enum Status : UInt16 where
  | hfTagOk := 0x00 => "HF tag operation succeeded"
  | hfTagNo := 0x01 => "HF tag no found or lost"
  | hfErrStat := 0x02 => "HF tag status error"
  | hfErrCrc := 0x03 => "HF tag data crc error"
  | hfCollision := 0x04 => "HF tag collision"
  | hfErrBcc := 0x05 => "HF tag uid bcc error"
  | mfErrAuth := 0x06 => "HF tag auth fail"
  | hfErrParity := 0x07 => "HF tag data parity error"
  /- ATS should be present but the card NAKed, or the ATS was too large. -/
  | hfErrAts := 0x08 => "HF tag was supposed to send ATS but didn't"
  | lfTagOk := 0x40 => "LF tag operation succeeded"
  | lfTagNoFound := 0x41 => "LF tag not found"
  | parErr := 0x60 => "API request fail, param error"
  | deviceModeError := 0x66 => "API request fail, device mode error"
  | invalidCmd := 0x67 => "API request fail, cmd invalid"
  | success := 0x68 => "Device operation succeeded"
  | notImplemented := 0x69 => "Some api not implemented"
  | flashWriteFail := 0x70 => "Flash write failed"
  | flashReadFail := 0x71 => "Flash read failed"
  | invalidSlotType := 0x72 => "Invalid card type in slot"

/-- `0x68 Device operation succeeded`, or `0x99 Invalid status`. -/
def Status.describe (code : UInt16) : String :=
  let hex := String.ofList (Nat.toDigits 16 code.toNat)
  s!"0x{hex} {(Status.ofUInt16? code).map (·.description) |>.getD "Invalid status"}"

end Chamelean
