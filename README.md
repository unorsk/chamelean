# chamelean

Lean 4 client for the Chameleon Ultra serial protocol. Core library only, no packages.

```
lake build
.lake/build/bin/chamelean                        # auto-detect USB port, print device info
.lake/build/bin/chamelean /dev/cu.usbmodem1101   # explicit port (Linux: /dev/ttyACM0)
.lake/build/bin/chamelean PORT 1000              # send raw command 1000 (getAppVersion)
.lake/build/bin/chamelean PORT 2000 0102         # raw command with hex payload
.lake/build/bin/chamelean tcp:127.0.0.1:4321     # USB-to-TCP bridge (via nc)
```

Library layout:

- `Chamelean/Frame.lean`: wire format, LRC, encoder and incremental decoder.
- `Chamelean/Command.lean`: command and status codes.
- `Chamelean/Transport.lean`: serial (via `stty`) and tcp (via `nc`) byte transports.
- `Chamelean/Client.lean`: `Client.connect`, `send`, `sendAsync`, `post`, `close`.

On macOS use the `/dev/cu.*` device, not `/dev/tty.*` (the latter blocks on open).
