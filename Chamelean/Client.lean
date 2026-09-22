import Std.Data.HashMap
import Std.Sync.Mutex
import Chamelean.Frame
import Chamelean.Command
import Chamelean.Transport

/-!
Request/response client on top of a `Transport`.

One background task reads bytes, decodes frames and resolves the `IO.Promise` registered
for that command. Senders write their frame and wait on the promise with a deadline, so
there is no separate transfer or timeout thread.
-/
namespace Chamelean

structure Response where
  cmd : UInt16
  status : UInt16
  data : ByteArray
deriving Inhabited

def Response.ok (r : Response) : Bool := r.status == Status.success.toUInt16

/-- One ISO14443-A tag as returned by `hf14aScan`. -/
structure Tag14a where
  uid : ByteArray
  /-- Answer To reQuest, type A: two bytes. -/
  atqa : ByteArray
  /-- Select AcKnowledge byte. -/
  sak : UInt8
  /-- Answer To Select; empty for tags that don't reach ISO14443-4. -/
  ats : ByteArray
deriving Inhabited

/-- Commands with a caller waiting for their reply, keyed by command code. -/
abbrev Pending := Std.HashMap UInt16 (IO.Promise Response)

structure Client where private mk ::
  transport : Transport
  pending : Std.Mutex Pending
  closing : IO.Ref Bool
  reader : Task (Except IO.Error Unit)
  /-- Command codes the device reported via `getDeviceCapabilities`; empty means "not checked". -/
  supported : IO.Ref (Array UInt16)

namespace Client

private def dispatch (pending : Std.Mutex Pending) (f : Frame) : IO Unit := do
  let waiter ← pending.atomically do
    let m ← get
    let w := m[f.cmd]?
    if w.isSome then set (m.erase f.cmd)
    pure w
  match waiter with
  | some p => p.resolve { cmd := f.cmd, status := f.status, data := f.data }
  | none => IO.eprintln s!"chamelean: no task waiting for response to cmd {Command.describe f.cmd}"

private partial def readLoop (t : Transport) (pending : Std.Mutex Pending) (closing : IO.Ref Bool)
    : IO Unit := do
  let mut dec : Decoder := {}
  while !(← closing.get) do
    let chunk ← try t.read catch e =>
      if !(← closing.get) then IO.eprintln s!"chamelean: {t.description}: {e}, receiver exiting"
      pure none
    match chunk with
    | none => closing.set true
    | some bytes =>
      for b in bytes do
        let (d, event) := dec.feed b
        dec := d
        match event with
        | .none => pure ()
        | .dropped reason => IO.eprintln s!"chamelean: dropped frame: {reason}"
        | .frame f => dispatch pending f

/-- Connect to `target` (a serial device path or `tcp:host:port`) and start receiving. -/
def connect (target : String) : IO Client := do
  let transport ← Transport.connect target
  let pending ← Std.Mutex.new {}
  let closing ← IO.mkRef false
  let reader ← IO.asTask (prio := .dedicated) (readLoop transport pending closing)
  return { transport, pending, closing, reader, supported := ← IO.mkRef #[] }

def isOpen (c : Client) : IO Bool := return !(← c.closing.get)

/-- Stop the receiver, drop the link and abandon any callers still waiting (they get an error). -/
def close (c : Client) : IO Unit := do
  c.closing.set true
  try c.transport.close catch _ => pure ()
  let _ ← IO.wait c.reader  -- returns within one read timeout
  c.pending.atomically (set ({} : Pending))

private def checkOpen (c : Client) : IO Unit := do
  unless ← c.isOpen do throw <| IO.userError "device is not open"

private def checkSupported (c : Client) (cmd : UInt16) : IO Unit := do
  let supported ← c.supported.get
  if !supported.isEmpty && !supported.contains cmd then
    throw <| IO.userError
      s!"device does not declare support for cmd {Command.describe cmd}; \
         make sure the firmware is up to date and matches the client"

/-- Write a frame without waiting for a reply. Used for commands that reboot the device. -/
def post (c : Client) (cmd : UInt16) (data : ByteArray := .empty) (status : UInt16 := 0)
    : IO Unit := do
  c.checkOpen
  if data.size > maxDataLength then
    throw <| IO.userError s!"payload of {data.size} bytes exceeds {maxDataLength}"
  c.pending.atomically (c.transport.write (Frame.encode { cmd, status, data }))

/--
Send a command and block until its reply arrives or `timeoutMs` elapses.
A newer `send` of the same command supersedes an older one still waiting, as in the Python client.
-/
def send (c : Client) (cmd : UInt16) (data : ByteArray := .empty) (status : UInt16 := 0)
    (timeoutMs : Nat := 3000) : IO Response := do
  c.checkOpen
  c.checkSupported cmd
  if data.size > maxDataLength then
    throw <| IO.userError s!"payload of {data.size} bytes exceeds {maxDataLength}"
  let promise ← IO.Promise.new
  let frame := Frame.encode { cmd, status, data }
  -- Register and write under one lock so frames never interleave and the reply cannot
  -- arrive before its waiter is registered.
  c.pending.atomically do
    modify (·.insert cmd promise)
    try c.transport.write frame
    catch e => modify (·.erase cmd); throw e
  let deadline := (← IO.monoMsNow) + timeoutMs
  let unregister := c.pending.atomically (modify (·.erase cmd))
  while !(← IO.hasFinished promise.result?) do
    if !(← c.isOpen) then
      unregister
      throw <| IO.userError s!"connection closed while waiting for cmd {Command.describe cmd}"
    if (← IO.monoMsNow) > deadline then
      unregister
      throw <| IO.userError s!"cmd {Command.describe cmd} timed out after {timeoutMs} ms"
    IO.sleep 5
  let some response ← IO.wait promise.result?
    | throw <| IO.userError s!"cmd {Command.describe cmd} was superseded by a newer send"
  if response.status == Status.invalidCmd.toUInt16 then
    throw <| IO.userError s!"device rejected unsupported cmd {Command.describe cmd}"
  return response

/-- Like `send`, but returns immediately with a task that yields the reply (or error). -/
def sendAsync (c : Client) (cmd : UInt16) (data : ByteArray := .empty) (status : UInt16 := 0)
    (timeoutMs : Nat := 3000) : IO (Task (Except IO.Error Response)) :=
  IO.asTask (prio := .dedicated) (c.send cmd data status timeoutMs)

/-- Typed convenience wrappers. -/
def sendCmd (c : Client) (cmd : Command) (data : ByteArray := .empty) (timeoutMs : Nat := 3000)
    : IO Response :=
  c.send cmd.toUInt16 data 0 timeoutMs

/--
Ask the device which commands it supports and remember them, so later sends fail fast
with a clear message instead of a timeout. Returns the list; empty on old firmware.
-/
def loadCapabilities (c : Client) : IO (Array UInt16) := do
  let r ← try c.sendCmd .getDeviceCapabilities catch _ => pure { cmd := 0, status := 0, data := .empty }
  let codes := (Array.range (r.data.size / 2)).map fun i => readU16 r.data (2 * i)
  c.supported.set codes
  return codes

/--
Scan for ISO14443-A (13.56 MHz) tags in the field. The device must be in reader mode
(`changeDeviceMode`); an empty result means the field is clear, an error status is raised.
-/
def hf14aScan (c : Client) : IO (Array Tag14a) := do
  let r ← c.sendCmd .hf14aScan
  -- This command reports success as HF_TAG_OK (0x00), not the device-wide SUCCESS (0x68).
  if r.status == Status.hfTagNo.toUInt16 then return #[]
  unless r.status == Status.hfTagOk.toUInt16 do
    throw <| IO.userError s!"hf14aScan failed: {Status.describe r.status} \
      (is the device in reader mode? changeDeviceMode 01)"
  let d := r.data
  let count := (d[0]?.getD 0).toNat
  let mut tags : Array Tag14a := #[]
  let mut o := 1  -- byte 0 is the tag count
  for _ in [0:count] do
    let uidLen := (d[o]?.getD 0).toNat; o := o + 1
    let uid := d.extract o (o + uidLen); o := o + uidLen
    let atqa := d.extract o (o + 2); o := o + 2
    let sak := d[o]?.getD 0; o := o + 1
    let atsLen := (d[o]?.getD 0).toNat; o := o + 1
    let ats := d.extract o (o + atsLen); o := o + atsLen
    tags := tags.push { uid, atqa, sak, ats }
  return tags

/-- The device's 6-digit BLE pairing passcode (stored on the device as ASCII digits). -/
def getBlePairingKey (c : Client) : IO String := do
  let r ← c.sendCmd .getBlePairingKey
  unless r.ok do throw <| IO.userError s!"getBlePairingKey failed: {Status.describe r.status}"
  return String.fromUTF8! r.data

/-- Whether BLE pairing (passcode required to bond) is currently enabled. -/
def getBlePairingEnable (c : Client) : IO Bool := do
  let r ← c.sendCmd .getBlePairingEnable
  unless r.ok do throw <| IO.userError s!"getBlePairingEnable failed: {Status.describe r.status}"
  return r.data[0]?.getD 0 != 0

/-- Turn BLE pairing on or off. -/
def setBlePairingEnable (c : Client) (enabled : Bool) : IO Unit := do
  let r ← c.sendCmd .setBlePairingEnable (ByteArray.mk #[if enabled then 1 else 0])
  unless r.ok do throw <| IO.userError s!"setBlePairingEnable failed: {Status.describe r.status}"

/-- Whether the device is in reader mode (`is_device_reader_mode`); false means tag/emulator. -/
def getDeviceMode (c : Client) : IO Bool := do
  let r ← c.sendCmd .getDeviceMode
  return r.data[0]?.getD 0 == 1

/-- Switch between reader mode (`true`) and tag/emulator mode (`change_device_mode`). -/
def setReaderMode (c : Client) (reader : Bool) : IO Unit := do
  let r ← c.sendCmd .changeDeviceMode (ByteArray.mk #[if reader then 1 else 0])
  unless r.ok do throw <| IO.userError s!"changeDeviceMode failed: {Status.describe r.status}"

end Client
end Chamelean
