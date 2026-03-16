# ADR 0063: Remove File Path Sandbox

## Status
Proposed (2026-03-16)

## Context

### Background

The `File` class in `beamtalk_file.erl` enforces a path sandbox via `validate_path/1`:

1. **Absolute paths rejected** — any path starting with `/`, `\`, or a drive letter (`C:`) returns an `invalid_path` error.
2. **Directory traversal rejected** — any path containing `..` as a component returns an `invalid_path` error.

All 14 public path-accepting `File` operations (`readAll:`, `writeAll:contents:`, `exists:`, `delete:`, `mkdir:`, `listDirectory:`, `rename:to:`, `lines:`, `open:do:`, `isDirectory:`, `isFile:`, `mkdirAll:`, `deleteAll:`, `absolutePath:`) call `validate_path/1` before touching the filesystem. The intent was to confine file I/O to relative paths within the current working directory.

Three functions — `exists:`, `isDirectory:`, and `isFile:` — silently return `false` for invalid paths instead of returning an error, making `File exists: "/etc/passwd"` return `false` rather than signalling that the path was rejected.

### The Problem: Developers Bypass It via FFI

ADR 0058 (Platform Security Model) establishes the Trusted Developer Tool stance: authenticated code executes with the full privileges of the OS process. The `Erlang` global provides unrestricted access to every Erlang function, including `file:read_file/1`, `os:cmd/1`, and `filelib:ensure_dir/1`.

In practice, any Beamtalk application that needs to work with the real filesystem bypasses `File` entirely and calls Erlang FFI directly. The symphony project (an agent orchestrator built on Beamtalk) demonstrates this clearly — 4 of its 11 source files use FFI workarounds:

**`workspace_manager.bt`** — manages per-issue workspace directories:
- Defaults to `/tmp/symphony_workspaces` (absolute path — `File` rejects it)
- Uses `Erlang filename absname:` for path resolution
- Uses `Erlang os cmd:` for hook execution in directories

**`dot_env.bt`** — loads `.env` files:
- Uses `Erlang file read_file:` instead of `File readAll:` because env file paths may be absolute

**`config.bt`** — resolves workspace root:
- Uses `Erlang filename basedir:` for XDG cache directories
- Uses `Erlang os getenv:` and manual `~/` expansion
- Cannot use `File` for any of this because all resolved paths are absolute

**`workflow_loader.bt`** — loads YAML config:
- Uses `Erlang file read_file:` instead of `File readAll:` for the same reason

The pattern is consistent: real applications cannot use the `File` class for real work. The sandbox forces developers into a parallel API (`Erlang file ...`) that has:

- **No structured errors** — raw Erlang `{ok, V} | {error, Reason}` tuples instead of `#beamtalk_error{}`
- **No cross-platform handling** — developers must handle path separators and platform differences themselves
- **No discoverability** — `Erlang file read_file:` is not documented in Beamtalk's language reference or autocompleted by the LSP
- **No protection at all** — the FFI calls have zero validation

The sandbox was intended to prevent accidents. Instead, it creates a two-tier API where the safe, documented, ergonomic path (`File`) cannot be used, and the unsafe, undocumented, raw path (`Erlang file ...`) must be used. The net effect is less safety, not more.

### The Workspace Is Not the Application Runtime

The file sandbox implicitly assumes that the workspace's current working directory is the application's universe. This was reasonable for REPL scratch sessions but does not hold for real Beamtalk applications:

- **Agent orchestrators** (symphony) manage workspace directories under `/tmp` or `~/.cache`
- **Build tools** need to read/write to output directories that may be absolute
- **Config loaders** resolve XDG paths, `$HOME`, environment variables — all producing absolute paths
- **Test harnesses** create temp directories for isolation

The workspace is the development environment, not the application's filesystem boundary. Applications read and write wherever they need to — just like applications in Python, Ruby, Erlang, or any other language with a REPL.

### The `tempDirectory` Incoherence

`File tempDirectory` returns an absolute path (`/tmp`, `C:\Windows\Temp`, or the value of `$TMPDIR`). But `validate_path/1` rejects absolute paths. This means:

```beamtalk
tmp := File tempDirectory        // => "/tmp"
File mkdir: tmp ++ "/myapp"      // => invalid_path error
```

The `File` class provides a method that produces paths it refuses to accept. This is the clearest signal that the sandbox model is broken.

### Constraints

1. **ADR 0058 is authoritative** — the security boundary is cookie authentication, not filesystem restrictions.
2. **BEAM interop must be preserved** — `File` should be a convenience layer over Erlang's `file` module, not a restricted subset of it.
3. **Existing code using relative paths must continue to work** — this is a relaxation, not a breaking change.

## Decision

**Remove the path sandbox from the `File` class.** Specifically:

1. **Remove `validate_path/1` calls** from all `beamtalk_file.erl` public functions.
2. **Allow absolute paths** — `File readAll: "/etc/hosts"` works.
3. **Allow `..` in paths** — `File readAll: "../sibling-project/config.yaml"` works.
4. **Remove the `invalid_path` error kind** from `File` — it is no longer produced by any `File` method.
5. **Keep all other error handling** — `not_found`, `permission_denied`, `is_directory`, etc. remain unchanged. The OS is the authority on what the process can access.

The `validate_path/1` function and `result_invalid_path/3` helper are deleted entirely, not feature-flagged or made optional.

### REPL Session Example

```beamtalk
// Before: tempDirectory returns a path File refuses to use
tmp := File tempDirectory                    // => "/tmp"
File mkdir: tmp ++ "/myapp"                  // => Error: invalid_path

// After: tempDirectory composes naturally with File methods
tmp := File tempDirectory                    // => "/tmp"
File mkdir: tmp ++ "/myapp"                  // => Result ok: true
File writeAll: tmp ++ "/myapp/out.txt"
  contents: "hello"                          // => Result ok: true
File readAll: tmp ++ "/myapp/out.txt"        // => Result ok: "hello"

// OS errors still work — the OS is the authority
File readAll: "/root/.ssh/id_rsa"            // => Error: permission_denied
File readAll: "/nonexistent"                 // => Error: not_found
```

### What Changes

| Before | After |
|--------|-------|
| `File readAll: "/etc/hosts"` → `invalid_path` error | `File readAll: "/etc/hosts"` → file contents (or `permission_denied`) |
| `File exists: "/tmp"` → `false` (misleading) | `File exists: "/tmp"` → `true` |
| `File mkdir: "/tmp/my-workspace"` → `invalid_path` error | `File mkdir: "/tmp/my-workspace"` → creates directory |
| `File delete: "../important.txt"` → `invalid_path` error | `File delete: "../important.txt"` → deletes file |
| `File writeAll: "/tmp/out.txt" contents: "hi"` → `invalid_path` error | `File writeAll: "/tmp/out.txt" contents: "hi"` → writes file |

### What Does Not Change

- `File cwd` — already returns an absolute path, no change needed.
- `File absolutePath:` — already resolves paths via `filename:absname/1`, no change needed. Note: `absolutePath: "/already/absolute"` returns the input unchanged, which is correct.
- `File tempDirectory` — already returns an absolute path; now callers can pass it directly to other `File` methods.
- All `Result`-based error handling for OS errors (`not_found`, `permission_denied`, `is_directory`, etc.).
- The `File` Beamtalk class API (`.bt` file) — no method signatures change.

## Prior Art

### Erlang `file` Module

Erlang's `file` module accepts any path the OS accepts — absolute, relative, or containing `..`. There is no path validation or sandbox. This is the module that `beamtalk_file.erl` wraps; removing the sandbox makes `File` a faithful wrapper rather than a restricted subset.

### Elixir `File` / `Path`

Elixir's `File` module accepts absolute and relative paths without restriction. `Path.expand/1` resolves `..` and `~` — it warns in documentation about `..` but does not reject it. No sandbox.

### Python `open()` / `os.path`

Python's built-in `open()`, `os.remove()`, `os.makedirs()`, etc. accept any path. No sandbox. The interactive interpreter executes with the user's full OS privilege.

### Ruby `File`

Ruby's `File.read`, `File.write`, `File.delete`, etc. accept any path. No sandbox. IRB (interactive Ruby) executes with full user privilege.

### Pharo Smalltalk `FileReference`

Pharo's `FileReference` accepts any path. `'/etc/hosts' asFileReference contents` works. The image runs with the user's OS privilege. No sandbox.

### Livebook (Elixir)

Livebook's notebook cells can call `File.read!/1` with any path. The security boundary is the authentication token, not filesystem restrictions — identical to Beamtalk's model (ADR 0058).

**Summary:** No reference language or BEAM ecosystem tool restricts its standard file API to relative paths. Beamtalk's sandbox is unique and, based on the evidence, counterproductive.

## User Impact

### Newcomer (Python/JS Background)

The newcomer encounters `File` in tutorials. Before this change, `File exists: "/tmp"` returns `false`, which is confusing — the directory obviously exists. After this change, `File` behaves like Python's `open()` or Node's `fs`: it operates on whatever path you give it, and the OS decides what you can access. This matches expectations.

**Risk:** A newcomer who types `File deleteAll: "/"` in the REPL will attempt a recursive delete. This matches Python (`shutil.rmtree("/")`) and is consistent with the trust model. The REPL is a power tool.

### Smalltalk Developer

Pharo's `FileReference` has no sandbox. The Smalltalk developer expects `File` to work like `FileReference` — any path, any operation. The current sandbox is surprising and foreign.

### Erlang/BEAM Developer

The BEAM developer knows that `file:read_file(<<"/etc/hosts">>)` works. They expect `File readAll: "/etc/hosts"` to be equivalent. The current sandbox violates the "faithful BEAM wrapper" expectation and forces them into FFI calls they shouldn't need.

### Operator

The operator loses one layer of accident prevention for scripted Beamtalk. However, this layer was already ineffective — any script that needed absolute paths was already using `Erlang file ...` with zero protection. After this change, scripted Beamtalk uses `File` (with structured errors and cross-platform handling) instead of raw FFI. The operator's actual tool for restricting filesystem access remains OS-level controls (containers, filesystem permissions, SELinux/AppArmor).

## Steelman Analysis

### Steelman for Keeping the Sandbox (Accident Prevention Argument)

**Best argument (from a safety-conscious language designer):**

The sandbox is not a security boundary — ADR 0058 settled that. But it is an *accident prevention* mechanism. A seatbelt is trivially removable; it still prevents injuries. The sandbox catches two real classes of developer mistakes:

1. **Path construction bugs:** String concatenation producing `../../../../etc/passwd` from user input is a bug, not intentional access. The `..` rejection catches this class of bug at the `File` API boundary.
2. **Typos in destructive operations:** `File deleteAll: "/"` (instead of `"./build/"`) is more likely a typo than intentional. The absolute-path rejection catches this.

The FFI bypass argument ("developers can use `Erlang file ...`") proves too much — by that logic, no API should have any validation, because you can always call the underlying system directly. Good APIs have guardrails that catch common mistakes even when power users can bypass them.

**Why we reject this despite its merit:** The argument assumes developers *can* use the `File` API for their work and *choose* to bypass it for convenience. The evidence (symphony) shows the opposite: developers *cannot* use `File` for legitimate operations and are *forced* to bypass it. The seatbelt analogy breaks down when the seatbelt prevents you from driving the car. The accident-prevention value is real in theory but unrealized in practice because the API is too restrictive to be used.

### Steelman for Option D (Allow Absolute, Keep `..` Rejection)

**Best argument (from a pragmatic engineer):**

The symphony use cases all involve absolute paths. None of them involve `..`. Allowing absolute paths while keeping `..` rejection would:
- Fix the `tempDirectory` incoherence
- Unblock symphony-style applications
- Preserve `..` rejection as a bug-catcher for path construction errors

This is the minimum change that fixes the real problem.

**Why we reject this:** The `..` restriction catches a narrow class of bugs (accidental traversal from string concatenation) while blocking a legitimate use case (referencing sibling projects, parent directories). `File readAll: "../shared-config/settings.yaml"` is valid in a monorepo. The restriction is also inconsistent — `File readAll: "/etc/passwd"` would work but `File readAll: "../etc/passwd"` would not, which is confusing. And the `Erlang file ...` bypass for `..` paths would persist, maintaining the two-API problem for this subset.

### Steelman for MCP/Agent Safety

**Best argument (from someone building AI-powered tooling):**

Beamtalk already has MCP infrastructure. When an LLM-driven agent invokes `File deleteAll:` with a hallucinated path, the sandbox is the only programmatic guard. The "trusted developer" model assumes a human is typing at the REPL; an agent operating on an LLM's instructions is a different trust context.

**Why we accept this concern but reject the sandbox as the solution:** This is a valid future concern, but the file sandbox is the wrong mechanism to address it. An agent safety layer should operate at the MCP tool boundary (restricting what tools the agent can call and with what parameters), not at the `File` class level. A `File`-level sandbox that blocks agents also blocks human developers. Agent-specific guardrails belong in the agent framework, not the standard library. ADR 0058 Phase 3 (workspace-level feature flags) is the right vehicle for this.

### Tension Points

| Tension | Cohorts | Resolution |
|---------|---------|------------|
| Accident prevention vs. API usability | Safety advocate vs. application developer | Usability wins — the sandbox prevents use, not just misuse |
| Minimum change vs. clean design | Pragmatic engineer (Option D) vs. language designer | Clean design wins — half-removing the sandbox creates inconsistency |
| Agent safety vs. developer freedom | AI tooling builder vs. trusted-tool model | Agent guardrails belong in the agent layer, not the stdlib |

## Alternatives Considered

### Option A: Scoped Escape Hatch (`File withAbsoluteAccess: [...]`)

Add a block-scoped opt-out that disables path validation within a closure:

```beamtalk
File withAbsoluteAccess: [
  File readAll: "/etc/hosts"
]
```

**Rejected** because:
1. Adds API complexity for no security benefit — the `Erlang` global already provides unrestricted access.
2. Creates a "permission theater" pattern where the escape hatch becomes boilerplate in every real application.
3. Every symphony-style application would wrap its entire `File` usage in `withAbsoluteAccess:`, making it a noise token.
4. The strongest case for this ("explicit is better than implicit — it forces developers to think about what paths they're using") is a code-quality signal, not a safety signal. But it only works if the default mode is usable for most work. Since it isn't, the opt-out becomes mandatory boilerplate.

### Option B: Configurable Allowed Roots

Let the workspace configuration specify additional allowed root paths:

```yaml
file:
  allowed_roots:
    - /tmp
    - ~/.cache/symphony
```

**Rejected** because:
1. Adds configuration complexity for a marginal safety improvement.
2. Cannot anticipate all paths an application might need — developers would constantly hit the sandbox and add exceptions.
3. Still requires path validation logic, just with a more complex rule set.
4. The `Erlang` FFI bypass remains available regardless, so the restriction is advisory at best.

### Option C: Keep Sandbox, Improve FFI Ergonomics

Instead of removing the sandbox, make FFI file operations more ergonomic — e.g., wrap `Erlang file ...` calls in a helper class with structured errors.

**Rejected** because:
1. Enshrines a two-API world — "safe but restricted" and "powerful but raw."
2. The `File` class is the natural, documented, discoverable API. Making developers learn a second API for unrestricted access is poor language design.
3. Does not solve the core problem: the sandbox provides no actual protection while creating real friction.

### Option D: Allow Absolute Paths, Keep `..` Rejection

Remove the absolute-path restriction but keep `..` component rejection as a bug-catcher for path construction errors.

**Rejected** because:
1. `..` in paths is legitimate — referencing sibling projects (`../shared-config/`), parent directories in monorepos, or computed relative paths.
2. Creates an inconsistent model: `File readAll: "/etc/passwd"` works but `File readAll: "../etc/passwd"` does not.
3. Maintains the two-API problem for the `..` subset — developers needing `..` paths still fall back to `Erlang file ...`.
4. The class of bugs caught by `..` rejection (accidental traversal from string concatenation) is narrow, and the `Erlang` FFI bypass means it catches them only when developers happen to use `File` rather than FFI.

### Option E: Do Nothing (Document FFI Patterns)

Accept the current state and add a "Recipes" section to `File` documentation showing how to use `Erlang file read_file:` for absolute paths.

**Rejected** because:
1. Enshrines the two-API world as the intended design rather than a workaround.
2. Does not fix the `tempDirectory` incoherence or the misleading `exists:` return values.
3. New Beamtalk users would need to learn "use `File` for relative paths, `Erlang file ...` for absolute paths" as a fundamental pattern, which is poor language design.

## Consequences

### Positive

- **One API for all file operations.** Developers use `File` everywhere — REPL sessions, scripts, agent orchestrators, build tools. No more falling back to `Erlang file ...` for real work.
- **Structured errors for all paths.** `File readAll: "/nonexistent"` returns a proper `#beamtalk_error{kind = not_found}` instead of a raw Erlang tuple.
- **`File tempDirectory` becomes useful.** `File tempDirectory` returns an absolute path that can now be passed directly to `File mkdir:`, `File writeAll:contents:`, etc. Before this change, `tempDirectory` was a dead end — it returned a path that `File` refused to use.
- **Consistent with ADR 0058.** The security model says authenticated users are trusted. The `File` class now agrees.
- **Removes misleading behavior.** `File exists: "/etc/passwd"` returning `false` was actively misleading. After this change, the result reflects reality.
- **Less code to maintain.** `validate_path/1`, `result_invalid_path/3`, cross-platform path rejection logic, and ~30 associated test cases are removed.

### Negative

- **No accidental-traversal guard.** `..` rejection caught a narrow class of path-construction bugs. After this change, a string concatenation bug that produces `../../../../etc/passwd` will attempt to read that file (and likely get `not_found` or `permission_denied`) rather than returning `invalid_path`. Developers must validate paths in their own application logic if this matters.
- **REPL beginners can operate on files outside the project.** `File deleteAll: "/"` will attempt a recursive delete (subject to OS permissions). This matches Python, Ruby, Elixir, and Pharo, and is consistent with ADR 0058.
- **Forecloses `File`-level restriction infrastructure.** If a future feature (ADR 0058 Phase 3 `--restrict-fs`, or a plugin sandbox) wants to restrict `File` operations to certain paths, `validate_path` would need to be reimplemented. This is an intentional trade-off: the restriction infrastructure does not justify its cost today, and a future restriction mechanism should operate at the workspace/capability level rather than inside the `File` class.
- **Documentation and test churn.** Docs mentioning path restrictions need updating. ~30 `invalid_path` test cases are removed and replaced with absolute-path operation tests.

### Neutral

- **No security model change.** ADR 0058 already establishes that authenticated code has full OS privilege. The FFI escape hatch (`Erlang file ...`) already provided unrestricted access; this change makes the ergonomic API match the actual capability.
- **`absolutePath:` becomes simpler.** It resolves paths via `filename:absname/1` without validation. For already-absolute paths it is a no-op, which is correct.
- **Agent/MCP safety is a separate concern.** If Beamtalk adds agent-specific filesystem restrictions, those should operate at the MCP tool layer, not the `File` class. See Steelman Analysis.

## Implementation

### Phase 1: Remove Path Validation (Single PR)

1. **`beamtalk_file.erl`:**
   - Delete `validate_path/1` function (lines 787-807).
   - Delete `result_invalid_path/3` helper (lines 744-755).
   - Remove `validate_path` case wrappers from all 14 public functions — each function calls Erlang `file:*` / `filelib:*` directly with the binary path.
   - Remove dead `{error, _} -> false` branches in `exists:`, `isDirectory:`, and `isFile:` (these currently swallow `validate_path` failures as `false`).
   - Keep all OS error handling (`enoent` → `not_found`, `eacces` → `permission_denied`, etc.).

2. **`beamtalk_file_tests.erl`:**
   - Remove ~30 `invalid_path` test cases (absolute, traversal, and Windows variants for each operation).
   - Add tests for absolute path operations: read a known file by absolute path, create/delete a temp file in `File tempDirectory` result, `exists:` returns `true` for `/tmp`.

3. **Error infrastructure cleanup:**
   - `beamtalk_error.erl`: Remove `generate_message(invalid_path, ...)` clauses (lines 187-190). Keep `invalid_path` in the kind type if other modules use it; remove if `File` was the only producer.
   - `beamtalk.hrl`: Remove `invalid_path` from the error kind comment (line 44) if no other module produces it.
   - `beamtalk_exception_handler.erl`: Comment at line 83 references `invalid_path` — update to reflect removal.

4. **Documentation:**
   - `docs/beamtalk-language-features.md` (line 1570): Remove "All paths must be relative (absolute paths and `..` traversal are rejected for safety)".
   - `docs/learning/18-file-io.md` (lines 164-171): Remove "## Security" section about path sandboxing; replace with a note that `File` operates with the process's OS permissions.
   - `docs/development/architecture-principles.md` (line 318): Remove "Validate file paths (no directory traversal)" principle.

### Phase 2: Migrate FFI Workarounds (Follow-up, Optional)

After Phase 1 lands, existing `.bt` code that uses `Erlang file ...` FFI workarounds can be migrated to `File` methods. This is optional cleanup — the FFI calls still work. Symphony and similar projects can migrate at their own pace.

## Migration Path

This change removes the `invalid_path` error kind from `File` operations. Code that handled this error needs updating:

- **No Beamtalk (`.bt`) code in the repository matches on `invalid_path`.** The `stdlib/` directory has zero references.
- **Erlang test code** (`beamtalk_file_tests.erl`) has ~30 tests asserting `invalid_path` — these are removed in Phase 1.
- **Error infrastructure** (`beamtalk_error.erl`, `beamtalk.hrl`) references `invalid_path` — cleaned up in Phase 1.

For external Beamtalk code that catches `invalid_path` errors: remove those branches. Paths that previously returned `invalid_path` will now either succeed or return an OS-level error (`not_found`, `permission_denied`, etc.).

Relative-path code is unaffected — this is a pure relaxation of restrictions.

## References

- [ADR 0058 — Platform Security Model](0058-platform-security-model.md) — establishes the Trusted Developer Tool stance; §2c documents the file sandbox as accepted risk mitigation
- [ADR 0028 — BEAM Interop Strategy](0028-beam-interop-strategy.md) — unrestricted FFI via `Erlang` global
- `runtime/apps/beamtalk_stdlib/src/beamtalk_file.erl` — implementation to modify
- `runtime/apps/beamtalk_stdlib/test/beamtalk_file_tests.erl` — tests to update
- `runtime/apps/beamtalk_runtime/src/beamtalk_error.erl` — error message generation to clean up
- `../symphony/src/workspace_manager.bt` — exemplar of FFI workaround pattern
- `../symphony/src/dot_env.bt` — exemplar of FFI workaround pattern
- `../symphony/src/config.bt` — exemplar of FFI workaround pattern
