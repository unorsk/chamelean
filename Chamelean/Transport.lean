/-!
Byte transports for the Chameleon. Two flavours, matching the Python client:

* serial: a USB CDC device such as `/dev/cu.usbmodemXXXX` (macOS) or `/dev/ttyACM0` (Linux),
* tcp: `tcp:host:port`, for a USB-to-TCP bridge (e.g. on Android), driven through `nc`.

Only the Lean core library and tools that ship with the OS (`sh`, `stty`, `nc`) are used.
-/
namespace Chamelean

structure Transport where
  /-- Bytes that arrived. `some empty` = nothing yet (returns within ~100 ms), `none` = link closed. -/
  read : IO (Option ByteArray)
  write : ByteArray → IO Unit
  close : IO Unit
  description : String

namespace Transport

/-- How long a blocking read waits before returning empty, so the reader thread can notice `close`. -/
def readTimeoutMs : Nat := 100

def serial (port : String) : IO Transport := do
  if System.Platform.isOSX && port.startsWith "/dev/tty." then
    throw <| IO.userError
      s!"use the callout device /dev/cu.{(port.drop "/dev/tty.".length).toString} instead of {port}; \
         /dev/tty.* blocks on open until carrier detect"
  -- Separate handles for each direction: a read blocked in the C stdio lock would otherwise
  -- stall every write for up to a full read timeout.
  let input ← IO.FS.Handle.mk port .read
  let output ← IO.FS.Handle.mk port .write
  -- Configure the line while our handles hold it open: macOS resets termios on last close.
  -- `min 0 time 1` after `raw` turns blocking reads into 100 ms polls.
  let stty ← IO.Process.output {
    cmd := "sh"
    args := #["-c", "stty raw -echo cs8 -parenb -cstopb 115200 min 0 time 1 < \"$1\"", "sh", port]
  }
  if stty.exitCode != 0 then
    throw <| IO.userError s!"stty failed for {port}: {stty.stderr.trimAscii}"
  return {
    read := do
      -- One byte per call: C `fread` would otherwise block until the whole request is filled.
      -- Stdio buffers whatever the tty delivered, so this is still cheap.
      let bytes ← input.read 1
      if bytes.isEmpty && !(← System.FilePath.pathExists port) then
        return none  -- device unplugged; on Linux the read itself throws instead
      return some bytes
    write := fun bytes => do output.write bytes; output.flush
    close := pure ()  -- handles close once the reader task and client drop them
    description := port
  }

def tcp (host port : String) : IO Transport := do
  let child ← IO.Process.spawn {
    cmd := "nc", args := #[host, port], stdin := .piped, stdout := .piped, stderr := .inherit
  }
  -- nc exits straight away when the connection is refused.
  IO.sleep 300
  if let some code ← child.tryWait then
    throw <| IO.userError s!"nc exited with code {code} connecting to {host}:{port}"
  return {
    read := do
      let bytes ← child.stdout.read 1
      return if bytes.isEmpty then none else some bytes
    write := fun bytes => do child.stdin.write bytes; child.stdin.flush
    close := child.kill
    description := s!"tcp:{host}:{port}"
  }

/-- `tcp:host:port` selects TCP, anything else is a serial device path. -/
def connect (target : String) : IO Transport := do
  if target.startsWith "tcp:" then
    match (target.drop 4).toString.splitOn ":" with
    | [host, port] => if host.isEmpty || port.isEmpty then usage else tcp host port
    | _ => usage
  else
    serial target
where
  usage := throw <| IO.userError "usage: tcp:127.0.0.1:4321"

end Transport
end Chamelean
