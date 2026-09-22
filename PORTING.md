# Porting the Chameleon Ultra CLI to Lean

Status of the Python → Lean port. Source lives in
`ChameleonUltra/software/script/`. Already ported: `chameleon_enum.py`,
`chameleon_com.py`, `chameleon_cmd.py` (the wire client core), plus a small test
harness in `Main.lean`.

## Where the Python maps in Lean

| Python file | Lean home | Done? |
|---|---|---|
| `chameleon_enum.py` | `Chamelean/Command.lean`, `Chamelean/Device.lean` | ✅ |
| `chameleon_com.py` | `Chamelean/Frame.lean`, `Chamelean/Transport.lean`, `Chamelean/Client.lean` | ✅ |
| `chameleon_cmd.py` | `Chamelean/Client.lean` (only a handful of wrappers exist) | ◐ partial |
| `chameleon_utils.py` | `Chamelean/Cli/Tree.lean`, `Chamelean/Cli/Args.lean`, `Chamelean/Cli/Pretty.lean` | ◐ scaffold |
| `chameleon_cli_unit.py` | new `Chamelean/Cli/Commands/*.lean` | ☐ |
| `chameleon_cli_main.py` | REPL in `Chamelean/Cli/Repl.lean`, launched from `Main.lean` | ◐ scaffold |
| `crypto1.py` | `Chamelean/Crypto1.lean` | ✅ |
| `hardnested_utils.py` | folded into `Chamelean/Crypto1.lean` | ◐ partial |

Note: `Main.lean` is now a thin REPL launcher (`Cli.repl`); the old test harness is gone.
The CLI scaffold (command tree, arg parser, errors, colors, dispatch loop) is in place under
`Chamelean/Cli/`, wired with a proof-of-life command set (`clear`, `rem`, `exit`, `dump_help`,
`hw connect`/`disconnect`/`version`). Porting from here = adding `CliTree.leaf`s and their
`Client` wrappers; the plumbing below is built.

Gotcha (Lean v4.34 std): `String.split` returns an `Std.Iter`, `String.drop`/`trim*` return
`String.Slice`, and `String.get?`/`String.mk` are deprecated. Use `Cli.words` to tokenize and
`.toString` to demote a slice.

---

## Scope calls (decide before starting)

Some Python leans on things that don't have a zero-dependency Lean equivalent.
Flag these now so we don't discover them mid-port.

- **`▲ out of scope / stub`** — `data plot` and `data modulation` render with
  matplotlib/pyqtgraph. No plotting without deps. Port the math, stub or drop the
  GUI.
- **`✅ external binaries` (plumbing done)** — `nested`, `darkside`, `hardnested`,
  `staticnested`, `mfkey32/64`, `fchk`, `autopwn` shell out to compiled crackers.
  The invocation layer is ported: `Chamelean/Tools.lean` (`executeTool`,
  `toolAvailable`, `missingTools`), mirroring the `stty`/`nc` process use. Each
  command still has to call it or stub `on_exec` (phase 5).
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
- [ ] `print_key_table` — sector/key grid for MIFARE dumps.
- [x] `color_string` / color constants — ANSI color helper (keep, it's dependency-free).
- [ ] `prng_successor`, `reconstruct_full_nt`, `parity_to_str`, `_swap_endian` —
      nonce math for the crack paths. Port only when a consumer needs them.
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

## Phase 3 — remaining command wrappers (`chameleon_cmd.py`)

Thin `Client` methods over `send`. Core client done; these are the rest.
Group them into `Chamelean/Client.lean` (or split by domain if it gets big).
Each is small — one `send`, decode the response.

Device / slots / settings:
- [ ] `get_app_version`, `get_device_chip_id`, `get_device_address`,
      `get_git_version`, `get_device_mode`, `is_device_reader_mode`,
      `change_device_mode`, `set_device_reader_mode`
- [ ] `get_slot_info`, `get_active_slot`, `set_active_slot`, `set_slot_tag_type`,
      `delete_slot_sense_type`, `set_slot_data_default`, `set_slot_enable`,
      `_get_active_lf_tag_type`
- [ ] `set_slot_tag_nick`, `get_slot_tag_nick`, `get_all_slot_nicks`,
      `delete_slot_tag_nick`
- [ ] `slot_data_config_save`, `enter_bootloader`, `get_enabled_slots`
- [ ] `get_animation_mode`, `set_animation_mode`, `get_sleep_timeout`,
      `set_sleep_timeout`, `reset_settings`, `save_settings`, `wipe_fds`,
      `factory_reset` path
- [ ] `get_battery_info`, `get_button_press_config`, `set_button_press_config`,
      `get_long_button_press_config`, `set_long_button_press_config`
- [ ] `set_ble_connect_key`, `get_ble_pairing_key` ✅, `delete_all_ble_bonds`,
      `get_ble_pairing_enable` ✅, `set_ble_pairing_enable` ✅
- [ ] `get_device_capabilities` ✅ (as `loadCapabilities`), `get_device_model`,
      `get_device_settings`

HF / MIFARE Classic reader:
- [ ] `hf14a_scan` ✅, `hf14a_scan_keep`, `mf1_detect_support`, `mf1_detect_prng`,
      `mf1_detect_nt_dist`
- [ ] `mf1_nested_acquire`, `mf1_darkside_acquire`, `mf1_static_nested_acquire`,
      `mf1_hard_nested_acquire`, `mf1_static_encrypted_nested_acquire`
- [ ] `mf1_auth_one_key_block`, `mf1_read_one_block`, `mf1_write_one_block`
- [ ] `mf1_manipulate_value_block`, `mf1_check_keys_of_sectors`,
      `mf1_check_keys_on_block`
- [ ] `hf14a_sniff`, `hf14a_auth_trace`, `hf14a_get_config`, `hf14a_set_config`,
      `hf14a_raw`

HF ISO14443-4 / EMV / SEOS:
- [ ] `hf14a_4_set_anti_coll`, `hf14a_4_apdu_recv`, `hf14a_4_apdu_send`,
      `hf14a_4_add_static_response`, `hf14a_4_clear_static_responses`,
      `hf14a_4_reader_apdu`, `hf14a_4_emv_scan`
- [ ] `seos_read_emu_data`, `seos_write_emu_data`, `seos_write_emu_keys`

MIFARE Classic emulation:
- [ ] `mf1_set_detection_enable`, `mf1_get_detection_count`, `mf1_get_detection_log`
- [ ] `mf1_write_emu_block_data`, `mf1_read_emu_block_data`, `mf1_get_emulator_config`
- [ ] `mf1_set_gen1a_mode`, `mf1_set_gen2_mode`, `mf1_set_block_anti_coll_mode`,
      `mf1_set_write_mode`, `mf1_get_prng_type`, `mf1_set_prng_type`,
      `mf1_get_field_off_do_reset`, `mf1_set_field_off_do_reset`
- [ ] `hf14a_set_anti_coll_data`, `hf14a_get_anti_coll_data`

MIFARE Ultralight / NTAG emulation:
- [ ] `mfu_get_emu_pages_count`, `mfu_read_emu_page_data`, `mfu_write_emu_page_data`
- [ ] `mfu_read_emu_counter_data`, `mfu_write_emu_counter_data`, `mfu_reset_auth_cnt`
- [ ] `mf0_ntag_get_uid_magic_mode`, `mf0_ntag_set_uid_magic_mode`,
      `mf0_ntag_get_version_data`, `mf0_ntag_set_version_data`,
      `mf0_ntag_get_signature_data`, `mf0_ntag_set_signature_data`
- [ ] `mf0_ntag_get_write_mode`, `mf0_ntag_set_write_mode`,
      `mf0_ntag_get_detection_enable`, `mf0_ntag_set_detection_enable`,
      `mf0_ntag_get_detection_count`, `mf0_ntag_get_detection_log`

LF reader / write / emu-id (each has scan + write_to_t55xx + set/get_emu_id):
- [ ] EM410x: `em410x_scan`, `em410x_write_to_t55xx`, `em410x_set/get_emu_id`
- [ ] HID Prox: `hidprox_scan`, `hidprox_write_to_t55xx`, `hidprox_set/get_emu_id`
- [ ] ioProx: `ioprox_scan`, `ioprox_write_to_t55xx`, `ioprox_set/get_emu_id`,
      `ioprox_decode_raw`, `ioprox_compose_id`
- [ ] Viking: `viking_scan`, `viking_write_to_t55xx`, `viking_set/get_emu_id`
- [ ] PAC: `pac_scan`, `pac_write_to_t55xx`, `pac_set/get_emu_id`
- [ ] Jablotron: `jablotron_scan`, `jablotron_write_to_t55xx`, `jablotron_set/get_emu_id`
- [ ] IDTECK: `idteck_write_to_t55xx`, `idteck_set/get_emu_id`
- [ ] `em4x05_scan`, `lf_sniff`, `lf_t55xx_write` (generic clone), `adc_generic_read`

## Phase 4 — pure helpers from `chameleon_cli_unit.py`

Standalone functions (no device), good to port early and unit-test.

- [ ] `type_id_SAK_dict`, tag-type ↔ SAK/ATQA tables
- [ ] `load_key_file`, `load_dic_file` — parse key/dictionary files
- [ ] IDTECK codec: `_idteck_compute_checksum`, `_idteck_compose_frame`,
      `_idteck_frame_info`
- [ ] Jablotron: `jablotron_card_id`
- [ ] PAC: `pac_encode_raw`, `pac_decode_raw`
- [ ] `ItemGenerator` — key-candidate generator for `fchk`/`autopwn`
- [ ] EMV decode: `_emv_decode_apdu`, `_decode_sw`, `_known_aid`, `_known_bertag`
- [ ] 14a sniff decode: `_decode_14a_frame_col`, `_extract_sniff_nonces`,
      `_print_14a_sniff_summary`, `_get_capture`
- [ ] DESFire helpers: `_des_raw`, `_des_select`, `_des_wrap`, `_des_transceive`,
      `_desfire_auth_des/aes/3k3des`, `_desfire_get_app_ids`, `_desfire_select_app`,
      `_desfire_get_version` (+ the `_DESFIRE_*` tables). **▲ needs crypto1/AES/DES.**
- [ ] mfkey tool runners: `_sniff_tool_path`, `_run_mfkey64`, `_run_mfkey32v2`,
      `_run_mfkey32v2_sniff`. **▲ external binaries.**
- [ ] `CrackEffect` (UL-C crack helper), `_plot_matplotlib`, `_plot_pyqtgraph`.
      **▲ plotting out of scope.**

## Phase 5 — CLI command classes (`chameleon_cli_unit.py`)

109 commands. Each is `args_parser` + `on_exec` (+ optional before/after). In Lean,
model as records `{ name, help, parse, run }` registered into the `CLITree`, not as
subclasses. Shared behavior (`DeviceRequiredUnit`, `ReaderRequiredUnit`,
`SlotIndexArgs`, `SenseTypeArgs`, `MF1AuthArgs`, etc.) becomes combinators/wrappers
around a runner, not inheritance.

Base-unit behaviors to model first:
- [x] `DeviceRequiredUnit` (require open device)
- [x] `ReaderRequiredUnit` (auto-switch to reader mode)
- [ ] `SlotIndexArgsUnit` / `SlotIndexArgsAndGoUnit` (slot arg; switch active slot, restore after)
- [ ] `SenseTypeArgsUnit` (`--hf`/`--lf`)
- [ ] `MF1AuthArgsUnit` (block/key/A-B args), `MFUAuthArgsUnit`
- [ ] `HF14AAntiCollArgsUnit`, `TagTypeArgsUnit`, and the LF `*IdArgsUnit` families

Commands by group (checkbox = command ported end-to-end):

`root`:
- [ ] `clear`, `rem`, `exit`, `dump_help`

`hw`:
- [ ] `connect`, `disconnect`, `mode`, `chipid`, `address`, `version`, `dfu`,
      `factory_reset`, `battery`, `raw`

`hw slot`:
- [ ] `list`, `change`, `type`, `init`, `enable`, `disable`, `delete`, `nick`,
      `store`, `openall`, `prng`

`hw settings`:
- [ ] `animation`, `sleeptimeout`, `bleclearbonds`, `blekey`, `blepair`, `btnpress`,
      `store`, `reset`

`hf 14a`:
- [ ] `scan`, `info`, `config`, `raw`, `sniff`, `auth-trace`

`hf mf` (MIFARE Classic):
- [ ] `rdbl`, `wrbl`, `view`, `dump`, `clone`, `value`, `elog`, `eload`, `esave`,
      `eview`, `econfig`
- [ ] `nested`, `darkside`, `hardnested`, `senested`, `autopwn`, `fchk` **▲ crackers**

`hf mfu` (Ultralight / NTAG):
- [ ] `rdpg`, `wrpg`, `rcnt`, `ercnt`, `ewcnt`, `dump`, `version`, `signature`,
      `authnonce`, `eview`, `eload`, `esave`, `econfig`, `edetect`, `ulcg`

`hf des` (DESFire): **▲ crypto**
- [ ] `info`, `chk`

`hf seos`:
- [ ] `eview`, `eload`, `keys`

`lf em 410x`:
- [ ] `read`, `write`, `econfig`

`lf em 4x05`:
- [ ] `read`

`lf hid prox`:
- [ ] `read`, `write`, `econfig`

`lf ioprox`:
- [ ] `read`, `write`, `econfig`

`lf pac`:
- [ ] `read`, `write`, `econfig`

`lf viking`:
- [ ] `read`, `write`, `econfig`

`lf jablotron`:
- [ ] `read`, `write`, `econfig`

`lf idteck`:
- [ ] `write`, `econfig`

`lf` / `lf generic`:
- [ ] `clone`, `sniff`, `adcread`

`data`:
- [ ] `hexsamples`, `manrawdecode`, `plot` **▲stub**, `modulation` **▲stub**

`emv`:
- [ ] `scan`, `debug`, `load`, `apdu`

## Phase 6 — crypto (only if crack/DESFire paths are wanted)

- [x] `crypto1.py` → `Chamelean/Crypto1.lean` (Crypto1 cipher, LFSR, filter fn,
      PRNG, `mfkey32HasKey`) — verified against the Python reference
- [~] `hardnested_utils.py` — parity helper ported (`evenParityU8`); the
      first-byte nonce-sum bookkeeping still to fold in when `hardnested` lands
- [ ] DES / 3DES / AES for DESFire auth (needed by `hf des`). No stdlib crypto in
      Lean core — **▲ big; decide if in scope.**

---

## Suggested order

1. Phase 1 (plumbing) + Phase 3 device/slot/settings wrappers → enough for
   `hw *` commands.
2. Phase 2 REPL → interactive shell that actually runs.
3. Phase 4 pure helpers (test without hardware) + the LF and `hf 14a`/`hf mf`
   read/write/emu commands.
4. Leave crackers, DESFire, EMV crypto, and plotting for last (or stub them).
