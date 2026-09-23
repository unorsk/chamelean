/-!
These ship as separate executables in a `bin/` directory next to the client; the MIFARE
attack commands shell out to them. The tools are optional, so `available` lets the CLI warn
up front instead of failing mid-attack. Mirrors the `IO.Process` use in `Transport.lean`;
only the OS process API is used.
-/
namespace Chamelean

/-- Directory holding the native tools (`get_resource_dir("bin")` in Python). -/
def defaultToolDir : System.FilePath := "bin"

/-- Executable name for a tool on this platform: `name.exe` on Windows, `name` elsewhere. -/
def toolExecutable (name : String) : String :=
  if System.Platform.isWindows then s!"{name}.exe" else name

/-- Path to a tool, or `none` if the binary is not present. Port of `_sniff_tool_path`. -/
def toolPath (name : String) (dir : System.FilePath := defaultToolDir) : IO (Option System.FilePath) := do
  let p := dir / toolExecutable name
  return if ← p.pathExists then some p else none

/-- Whether a tool's binary is present. -/
def toolAvailable (name : String) (dir : System.FilePath := defaultToolDir) : IO Bool :=
  return (← toolPath name dir).isSome

/-- The optional tools the CLI checks for at startup. -/
def optionalTools : Array String :=
  #["staticnested", "nested", "darkside", "mfkey32v2", "mfkey64",
    "staticnested_1nt", "staticnested_2x1nt_rf08s", "staticnested_2x1nt_rf08s_1key"]

/-- Names of any optional tools missing from `dir`; empty means all present. -/
def missingTools (dir : System.FilePath := defaultToolDir) : IO (Array String) := do
  let mut missing := #[]
  for t in optionalTools do
    unless ← toolAvailable t dir do
      missing := missing.push t
  return missing

/--
Run a native tool and return its combined stdout+stderr. Throws if the binary is absent or
exits non-zero (carrying the captured output), matching `execute_tool`. `workDir` is where the
tool runs; some tools write scratch files there, so it defaults to a fresh temp directory.
-/
def executeTool (name : String) (args : Array String)
    (dir : System.FilePath := defaultToolDir) (workDir : Option System.FilePath := none)
    : IO String := do
  let some path ← toolPath name dir
    | throw <| IO.userError s!"tool not found: {dir / toolExecutable name}"
  let cwd ← match workDir with
    | some d => pure d
    | none => IO.FS.createTempDir  -- isolate the tool's scratch files, like Python's tempdir
  -- Absolute path so it still resolves after we change the working directory.
  let absPath ← IO.FS.realPath path
  let out ← IO.Process.output { cmd := absPath.toString, args, cwd := some cwd }
  let combined := out.stdout ++ out.stderr
  if out.exitCode != 0 then
    throw <| IO.userError s!"failed to execute {name} (exit {out.exitCode}): {combined.trimAscii}"
  return combined

end Chamelean
