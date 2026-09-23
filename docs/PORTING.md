# Porting the Chameleon Ultra CLI to Lean

Status of the Python → Lean port. Source lives in
`ChameleonUltra/software/script/`. Already ported: `chameleon_enum.py`,
`chameleon_com.py`, `chameleon_cmd.py` (the wire client core), plus the full CLI
command tree in `Chamelean/Cli/Commands.lean`.

## Where the Python maps in Lean

| Python file | Lean home | Done? |
|---|---|---|
| `chameleon_enum.py` | `Chamelean/Command.lean`, `Chamelean/Device.lean` | ✅ |
| `chameleon_com.py` | `Chamelean/Frame.lean`, `Chamelean/Transport.lean`, `Chamelean/Client.lean` | ✅ |
| `chameleon_cmd.py` | `Chamelean/Commands.lean` (one `Client` method per firmware command) | ✅ |
| `chameleon_utils.py` | `Chamelean/Cli/Tree.lean`, `Chamelean/Cli/Args.lean`, `Chamelean/Cli/Pretty.lean` | ◐ scaffold done, some pure helpers still missing |
| `chameleon_cli_unit.py` | `Chamelean/Cli/Commands.lean` | ✅ commands; ▲ a few attack/decode commands intentionally stubbed |
| `chameleon_cli_main.py` | REPL in `Chamelean/Cli/Repl.lean`, launched from `Main.lean` | ✅ |
| `crypto1.py` | `Chamelean/Crypto1.lean` | ✅ |
| `hardnested_utils.py` | folded into `Chamelean/Crypto1.lean` | ◐ partial |

Note: `Main.lean` is now a thin REPL launcher (`Cli.repl`); the old test harness is gone.
The full command tree is wired: `hw`, `hf 14a`/`hf mf`/`hf mfu`/`hf seos`/`hf emv`, and
`lf em`/`hid`/`ioprox`/`viking`/`pac`/`jablotron`/`idteck`/`sniff`/`adc` all have real leaves
calling their `Client` methods. Six leaves remain deliberate `todoLeaf` stubs — see
"Deliberately left as stubs" below.

Gotcha (Lean v4.34 std): `String.split` returns an `Std.Iter`, `String.drop`/`trim*` return
`String.Slice`, and `String.get?`/`String.mk` are deprecated. Use `Cli.words` to tokenize and
`.toString` to demote a slice.

Gotcha: mixing `&&&`/`|||` with `<<<`/`>>>` in one unparenthesized `UInt32` expression can
confuse `binop%`'s type unification (it tries to unify shift-amount literals with the operand
type) and fail with a spurious `HAnd _ Nat` error. Either add an explicit `: UInt32` type
ascription on the `let`, or just do the byte-shuffling in `Nat` (mul/div/mod) instead — see
`idteckFrameInfo` in `Chamelean/Cli/Commands.lean`.

---

## Deliberately left as stubs (`todoLeaf`)

These need real work in a place other than the command itself — a host-side crypto attack,
an external cracking binary, or a frame-decoder pure helper — and were explicitly out of
scope for "port the commands":

- **`hf mf nested` / `staticnested` / `hardnested` / `encnested` / `darkside`** — the
  Python versions run the firmware "acquire" step and then shell out to a separate
  `nested`/`hardnested`/`staticnested`/`mfkey64` binary (or, for hardnested, a large
  offline bitslice attack) to actually recover the key from the collected nonces. The
  firmware `Client` wrappers for every acquire command already exist
  (`mf1NestedAcquire`, `mf1DarksideAcquire`, `mf1HardNestedAcquire`,
  `mf1StaticNestedAcquire`, `mf1StaticEncryptedNestedAcquire` in `Chamelean/Commands.lean`);
  what's missing is the attack math and the external-tool runner
  (`_run_mfkey64`/`_sniff_tool_path` et al., Phase 4 below).
- **`hf 14a sniff`** — needs the reader-frame decoder pure helpers
  (`_decode_14a_frame_col`, `_extract_sniff_nonces`, `_print_14a_sniff_summary`) that
  turn a raw captured-frame buffer into a readable protocol trace. `hf mf authtrace`
  ports the same wire format but with a much simpler, un-annotated frame dump, since
  that didn't need the decoder helpers.

Also out of scope, unchanged from before: DESFire (`hf des`, needs AES/DES/3DES —
never had a stub), `data plot`/`data modulation` (needs matplotlib/pyqtgraph, no `data`
group exists), and prompt_toolkit tab completion/history in the REPL.

## Scope simplifications (commands are real, but simpler than the Python CLI)

A few commands were ported with a deliberately smaller argument surface than their Python
counterpart, since matching their exact ergonomics would have pulled in a Phase 4 pure
helper. The wire protocol and firmware calls are the real thing in every case:

- **`hf mf check` / `checkblk`** — keys are given directly on the command line (hex,
  space-separated) or piped in; there's no `.key`/`.dic` file import/export
  (`load_key_file`/`load_dic_file`, `ItemGenerator`) and no `print_key_table` grid, just a
  plain `sector: key` listing.
- **`lf pac`** — accepts the 8-byte card number as ASCII directly (matching what
  `pac_write_to_t55xx`/`pac_get_emu_id` actually send over the wire), instead of also
  accepting a raw 128-bit T55xx bitstream (`pac_encode_raw`/`pac_decode_raw`).
- **`lf jablotron`** — prints the raw hex ID; doesn't decode it into the printed decimal
  card number (`jablotron_card_id`).
- **`hw slot list`** — shows tag type, nickname and enabled state per slot, but not the
  deep per-protocol dump (Gen1a/Gen2/write-mode/PRNG for MIFARE, or the LF emulated-id
  detail) that the Python version prints when a slot is active.
- **`hf emv`** — wraps the raw ISO14443-4 T=CL firmware calls (`scan`, `apdu`, anti-coll,
  static responses, relay) without the EMV-specific APDU/TLV decode tables
  (`_emv_decode_apdu`, `_known_aid`, `_known_bertag`) the Python `emv` command group has;
  output is raw hex.

---

## Scope calls (decide before starting)

Some Python leans on things that don't have a zero-dependency Lean equivalent.
Flag these now so we don't discover them mid-port.

- **`▲ out of scope / stub`** — `data plot` and `data modulation` render with
  matplotlib/pyqtgraph. No plotting without deps. Port the math, stub or drop the
  GUI.
- **`✅ crypto` (Crypto1 done)** — `Chamelean/Crypto1.lean` ports `crypto1.py`,
  verified byte-identical to the reference (keystream, PRNG, `mfkey32`). DESFire's
  DES/3DES/AES is still unported and remains the open crypto question.
- **prompt_toolkit** (tab completion, history, colored prompt) has no Lean
  equivalent. Plan for a plain `IO.getLine` REPL; completion is a nice-to-have we
  likely drop.

---

## Phase 1 — CLI plumbing (`chameleon_utils.py`)

Foundation everything else needs. Build this first.

- [x] `CLITree` — the command tree (`root`, `subgroup`, `command`). In Lean model
      as an inductive/structure tree of groups and leaf commands, not Python's
      decorator registry. Each leaf carries: name, help, arg-parser, runner.
- [x] Argument parsing — replaces `ArgumentParserNoExit`/argparse. Write a small
      idiomatic parser (flags, options with values, `-a/-b` mutually exclusive
      groups, required args, `int`/`hex`/`str` types). Return `Except String Args`.
      Don't reimplement argparse; build the minimum these commands use.
- [x] `ArgsParserError` / `UnexpectedResponseError` as an error type (likely one
      `CliError` sum used across the CLI).
- [x] `expect_response` — assert a `Response.status` is in an accepted set, else
      raise. Central helper used by nearly every command.
- [x] `print_help` — render a command's usage from its arg spec.
- [x] `print_mem_dump` — hex block dump.
- [ ] `print_key_table` — sector/key grid for MIFARE dumps (`hf mf check` prints a
      plain list instead — see "Scope simplifications" above).
- [x] `color_string` / color constants — ANSI color helper (keep, it's dependency-free).
- [ ] `prng_successor`, `reconstruct_full_nt`, `parity_to_str`, `_swap_endian` — only
      needed by the nested/hardnested attack math (Phase 4), still open.
- [x] `execute_tool` — done in `Chamelean/Tools.lean` (`executeTool`), plus
      `toolAvailable`/`toolPath` (`_sniff_tool_path`) and `missingTools`/`check_tools`.
- [ ] `get_resource_dir` — locate bundled dictionaries/resources.
- [ ] `check_tools` — startup check for optional binaries.
- [ ] Completers (`CustomNestedCompleter`, `ArgparseCompleter`) — **likely skip**
      (prompt_toolkit).

## Phase 2 — REPL (`chameleon_cli_main.py`)

- [x] `exec_cmd` — split input, walk the `CLITree`, on a group print children, on a
      leaf parse args and run `before_exec → on_exec → after_exec`.
- [x] Command aliases: `quit/q/e → exit`; leading `;#%` → `rem` comment.
- [x] `get_cmd_node` — resolve argv against the tree.
- [x] `get_prompt` / `print_banner` — prompt string (shows connection + slot), banner.
- [x] Main loop — plain `IO.getLine` loop, one command per line, EOF/`exit` ends it
      (no prompt_toolkit). Multi-line paste split dropped: `getLine` already yields one
      line per call.
- [x] Rewrite `Main.lean` to launch this REPL instead of the test harness.

## Phase 3 — device command wrappers (`chameleon_cmd.py`)

Done. Every `ChameleonCMD` method has a matching thin `Client` method in
`Chamelean/Commands.lean` — one `send`, decode the response, done. This covers device/slots/
settings, HF 14a/MIFARE Classic/Ultralight reader and emulation, ISO14443-4 T=CL, SEOS, and
every LF reader/writer/emulated-id protocol.

## Phase 4 — pure helpers from `chameleon_cli_unit.py`

Standalone functions (no device). A few small ones got ported inline as private helpers
next to the one command that needs them (`idteckChecksum`/`idteckFrameInfo` for
`lf idteck`, the value-block signed-int packing for `hf mf value`); the rest — mostly
needed by the attack commands and the fancier decode/print paths noted above — are still
open:

- [ ] `type_id_SAK_dict`, tag-type ↔ SAK/ATQA tables
- [ ] `load_key_file`, `load_dic_file` — parse key/dictionary files
- [x] IDTECK codec: checksum + frame decode (inline in `Chamelean/Cli/Commands.lean`,
      `lf idteck`). Compose (`_idteck_compose_frame`) not needed: `lf idteck write`
      takes the frame directly.
- [ ] Jablotron: `jablotron_card_id`
- [ ] PAC: `pac_encode_raw`, `pac_decode_raw`
- [ ] `ItemGenerator` — key-candidate generator for `fchk`
- [ ] EMV decode: `_emv_decode_apdu`, `_decode_sw`, `_known_aid`, `_known_bertag`
- [ ] 14a sniff decode: `_decode_14a_frame_col`, `_extract_sniff_nonces`,
      `_print_14a_sniff_summary`, `_get_capture`
- [ ] DESFire helpers: `_des_raw`, `_des_select`, `_des_wrap`, `_des_transceive`,
      `_desfire_auth_des/aes/3k3des`, `_desfire_get_app_ids`, `_desfire_select_app`,
      `_desfire_get_version` (+ the `_DESFIRE_*` tables). **▲ needs crypto1/AES/DES.**
- [ ] mfkey tool runners: `_sniff_tool_path`, `_run_mfkey64`, `_run_mfkey32v2`,
      `_run_mfkey32v2_sniff`. **▲ external binaries.**

## Phase 5 — CLI command classes (`chameleon_cli_unit.py`)

Modeled as records `{ name, help, parse, run }` registered into the `CLITree`, not as
subclasses (`Chamelean/Cli/Commands.lean`). Shared behavior (`DeviceRequiredUnit`,
`ReaderRequiredUnit`) is a combinator wrapping a runner (`Chamelean/Cli/Tree.lean`); the more
specific base units (`SlotIndexArgsUnit`, `MF1AuthArgsUnit`, ...) turned out not to need
their own combinator — the handful of commands that share an arg shape share a private
parser-builder function instead (`mf1AuthParser`, `enableFlagGroup`, `hidCardSpecs`, ...).

Commands by group (checkbox = command ported end-to-end):

`root`:
- [x] `clear`, `rem`, `exit`, `dump_help`

`hw`:
- [x] `connect`, `disconnect`, `mode`, `chipid`, `address`, `version`, `dfu`,
      `factory_reset`, `battery`, `raw` — all but a generic `hw raw` (no Python
      equivalent exists as a separate `hw` command either; `hf 14a raw` covers the raw
      wire-exchange use case)

`hw slot`:
- [x] `list`, `change`, `type`, `init`, `enable`, `disable`, `delete`, `nick`,
      `store`, `openall`, `prng` — modeled as `list/active/change/type/init/enable/
      delete/enabled/store` + `nick get/set/delete/list`; no separate `openall`/`prng`
      leaves (not in the Lean command tree's scope)

`hw settings`:
- [x] `animation`, `sleeptimeout`, `bleclearbonds`, `blekey`, `blepair`, `btnpress`,
      `store`, `reset`

`hf 14a`:
- [x] `scan`, `info`, `raw`, `auth-trace` (as `authtrace`), `config get/set`,
      `anticoll get/set`
- [ ] `sniff` — needs the frame-decode pure helpers, see "Deliberately left as stubs"

`hf mf` (MIFARE Classic):
- [x] `rdbl`, `wrbl`, `value`, `elog` (as `econfig detection log`), `eload` (as `eread`/
      `eload`), `econfig` (split into `view`/`gen1a`/`gen2`/`coll`/`write`/`prng`/
      `fieldreset`/`detection`), plus `ntdist`, `check`, `checkblk`
- [ ] `dump`, `clone`, `eview`, `esave` — convenience commands built from `rdbl`/`eread`;
      not ported as separate leaves
- [ ] `nested`, `staticnested`, `hardnested`, `darkside`, `encnested` (as `hardnested`'s
      static-encrypted variant) — see "Deliberately left as stubs"

`hf mfu` (Ultralight / NTAG):
- [x] `rcnt`/`ercnt` (as `counter get`), `ewcnt` (as `counter set`), `version`,
      `signature`, `econfig` (split into `uidmagic`/`write`/`detection`), `edetect` (as
      `detection log`), plus `rdpg`/`wrpg` (emulator page data, not the reader-mode raw
      read/write the Python commands of the same name do — see the description on each
      leaf), `resetauth`
- [ ] `dump`, `eview`, `eload`, `esave`, `authnonce`, `ulcg` — not ported as separate
      leaves

`hf des` (DESFire): **▲ crypto, no stub, not attempted**

`hf seos`:
- [x] `eview` (as `read`), `eload` (as `write`), `keys`

`lf em 410x`:
- [x] `read`, `write`, `econfig` (as `emu get/set`)

`lf em 4x05`:
- [x] `read`

`lf hid prox`:
- [x] `read`, `write`, `econfig` (as `emu get/set`)

`lf ioprox`:
- [x] `read`, `write`, `econfig` (as `emu get/set`), `decode`, `compose`

`lf pac`:
- [x] `read`, `write`, `econfig` (as `emu get/set`) — ASCII card number only, see
      "Scope simplifications"

`lf viking`:
- [x] `read`, `write`, `econfig` (as `emu get/set`)

`lf jablotron`:
- [x] `read`, `write`, `econfig` (as `emu get/set`) — raw hex only, see "Scope
      simplifications"

`lf idteck`:
- [x] `write`, `econfig` (as `emu get/set`)

`lf` / `lf generic`:
- [x] `sniff`, `adcread` (as `adc`)
- [ ] `clone` — a convenience wrapper that dispatches to each protocol's own `write`;
      not ported as a separate leaf

`data`:
- [ ] `hexsamples`, `manrawdecode`, `plot` **▲stub**, `modulation` **▲stub** — no `data`
      group exists yet

`emv`:
- [x] raw T=CL wrappers ported under `hf emv` (`scan`, `apdu`, `anticoll set`,
      `static add/clear`, `relay recv/send`) — no APDU/TLV decode, see "Scope
      simplifications"

---

## Suggested order

1. ~~Phase 1 (plumbing) + Phase 3 device/slot/settings wrappers → enough for `hw *` commands.~~ Done.
2. ~~Phase 2 REPL → interactive shell that actually runs.~~ Done.
3. ~~Phase 5 commands: `hw`, `hf 14a`/`hf mf`/`hf mfu`/`hf seos`/`hf emv`, and every `lf` protocol.~~ Done, except the six stubs above.
4. Phase 4 pure helpers (test without hardware): nested/hardnested/darkside attack math,
   the external mfkey tool runners, and the 14a sniff frame decoder — these unlock the
   six remaining `todoLeaf` commands.
