# ADR 0027: Windows Platform Support

## Status
Proposed (2026-02-16)

## Context

### The Problem

Beamtalk is developed and tested exclusively on Linux (Ubuntu in CI, devcontainers for development). There is no Windows CI, no Windows testing, and several components use Unix-specific system calls and tools. A developer on Windows cannot reliably build or run Beamtalk today.

This matters because:
1. **Developer reach** — Windows is ~45% of developer desktops. Excluding it limits adoption.
2. **Peer expectation** — Gleam, Elixir, and Erlang all have first-class Windows support. A BEAM language that doesn't run on Windows is an outlier.
3. **CI gaps** — Without Windows CI, regressions can silently break Windows compatibility even for code that should be portable.

### Current State

The codebase has **three tiers** of Windows readiness:

**Tier 1: Already portable (core compiler)**
The Rust compiler (`beamtalk-core`) — lexer, parser, AST, semantic analysis, codegen — is pure Rust with no platform-specific code. It uses `Path`/`PathBuf` for path handling and has no system calls. This should work on Windows today.

**Tier 2: Partially guarded (CLI tooling)**
The CLI (`beamtalk-cli`) has Unix-specific code that is *mostly* behind `#[cfg(unix)]` guards with fallbacks:

| Code | Unix | Windows fallback |
|------|------|-----------------|
| Parent PID (`libc::getppid`) | ✅ | `std::process::id()` stub |
| Cookie file permissions (0o600) | ✅ chmod | Writes without mode |
| Process detection (`ps` command) | ✅ | TCP probe fallback |
| escript permissions (0o755) | ✅ chmod | Skipped |
| Process termination (Unix signals) | ✅ | Returns error (not implemented) |

**Tier 3: Unix-only (workspace management, build scripts)**
Several components have no Windows path at all:

| Component | Issue | File(s) |
|-----------|-------|---------|
| `find_beam_pid_by_node()` | Calls `ps` without `#[cfg]` guard | `workspace/mod.rs:573` |
| `wait_for_process_exit()` | Unix-only, uses signal probing | `workspace/mod.rs:737-761` |
| `stop_workspace()` | Returns error on Windows | `workspace/mod.rs:787-812` |
| rebar3 pre-hook | `bash -c ./compile.sh` | `runtime/rebar.config:21` |
| Test fixture compilation | Bash script | `test_fixtures/compile.sh` |
| Home directory fallback | `os:getenv("HOME", "/tmp")` | `beamtalk_workspace_meta.erl:173` |
| Project root detection | Hardcoded `/` root check | `beamtalk_compiler_port.erl:153` |
| Justfile | `set shell := ["bash", "-uc"]` | `Justfile:9` |
| CI workflows | Ubuntu-only, bash commands | `.github/workflows/ci.yml` |

### Constraints

- Must not degrade Linux/macOS experience to accommodate Windows
- Must not add significant maintenance burden (Windows-only code paths should be minimal)
- Erlang/OTP must be installed on the target platform (prerequisite, not our problem to solve)
- rebar3 must be available for building the runtime (same prerequisite)
- Should follow the same pattern as Gleam: Rust compiler is fully portable, runtime needs Erlang

## Decision

### Adopt a tiered approach to Windows support

**Tier 1 (immediate): Compiler + build command work on Windows**

The core compilation path — `beamtalk build`, `beamtalk new`, `beamtalk test` — must work on Windows. This means:

1. **Add Windows CI job** — `windows-latest` in GitHub Actions, running `cargo test`, `cargo clippy`, and `beamtalk build` on a test project
2. **Fix unguarded Unix code** — Wrap `find_beam_pid_by_node()` with `#[cfg(unix)]` and add Windows fallback
3. **Portable path handling** — Replace any hardcoded `/` root checks with `std::path` methods or `Path::has_root()`
4. **No bash dependency for compilation** — The `beamtalk build` command must not require bash. The embedded compiler port (ADR 0022) already avoids shell scripts for compilation.

**Tier 2 (near-term): Workspace and REPL work on Windows**

The interactive development experience — `beamtalk repl`, workspace management — should work:

1. **Implement Windows process management** — Replace `ps`/signal usage with Windows-compatible alternatives:
   - Use `tasklist` for process enumeration or the `sysinfo` crate
   - Use `taskkill` or Rust's `Child::kill()` for process termination
   - Use Windows API (`OpenProcess` + `GetExitCodeProcess`) for process existence checks
2. **Fix `stop_workspace()` on Windows** — Currently returns an error; implement actual process termination
3. **Fix Erlang runtime Windows issues** — Replace `HOME` with `USERPROFILE`, fix `/` root detection in compiler port

**Tier 3 (deferred): Full parity including development tooling**

Scripts, benchmarks, and development tooling:

1. **Justfile Windows shell** — Add `set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]`
2. **rebar3 pre-hooks** — Replace `bash -c compile.sh` with a portable Erlang script or escript
3. **CI scripts** — Add PowerShell equivalents where needed (or use cross-platform tools)
4. **Fuzz testing** — Verify fuzz harness works on Windows (lower priority)

### What we explicitly do NOT do

- **No WSL requirement** — Users should not need WSL to run Beamtalk on Windows. WSL is a fine development environment but not a prerequisite.
- **No separate Windows build** — Same Cargo workspace, same crate structure. Platform differences handled with `#[cfg]` attributes.
- **No Windows-specific features** — No `.msi` installer, no Windows service support (yet). Binary distribution via GitHub releases and future package managers (Scoop, WinGet).

### Process management strategy

The biggest portability challenge is process management for workspaces. The approach:

```rust
// Abstract process operations behind a trait
pub trait ProcessManager {
    fn is_running(&self, pid: u32) -> bool;
    fn find_pid_by_name(&self, name: &str) -> Option<u32>;
    fn terminate(&self, pid: u32) -> Result<()>;
    fn force_terminate(&self, pid: u32) -> Result<()>;
}

// Platform implementations
#[cfg(unix)]
mod unix {
    // Uses signal probing, ps, Unix signals
}

#[cfg(windows)]
mod windows {
    // Uses OpenProcess, tasklist/taskkill or sysinfo crate
}
```

This keeps platform-specific code isolated and testable.

## Prior Art

### Gleam
Full Windows support from early on. Rust compiler is inherently portable. Uses `std::process::Command` for `erl`/`erlc` invocation (works cross-platform if Erlang is in PATH). CI runs on Windows. Ships Windows binaries via GitHub releases.

**Adopted:** Same model — Rust compiler portable, platform differences in process management only.

### Elixir
Works on Windows via Erlang/OTP's Windows support. `mix` (Elixir's build tool) is written in Elixir itself, running on BEAM — inherently cross-platform. Some ecosystem tools (like `phoenix_live_reload`) use `inotifywait` which requires alternatives on Windows.

**Adopted:** Philosophy that the BEAM runtime handles most portability; the compiler/tooling layer just needs to invoke it correctly.

### Rust (Cargo)
Exemplary cross-platform support. Uses `#[cfg(target_os)]` extensively. Has `std::process::Command` that handles path differences. CI matrix includes `windows-latest`, `macos-latest`, `ubuntu-latest`.

**Adopted:** `#[cfg]` attribute pattern, CI matrix strategy, `ProcessManager` trait abstraction.

### Erlang/OTP
Erlang itself has excellent Windows support — prebuilt Windows installers, `werl` (Windows Erlang shell), proper Windows service support. `rebar3` works on Windows but some plugins assume Unix tools. The BEAM VM is fully portable.

**Relevant:** Beamtalk's runtime (Erlang) is already portable. The issues are in the Rust tooling layer and bash-dependent build scripts.

## User Impact

### Windows developer (primary beneficiary)
Can download a binary or `cargo install beamtalk`, run `beamtalk new myapp`, `beamtalk build`, and `beamtalk repl` — the full workflow works. No WSL, no bash, no Unix tools required beyond Erlang/OTP.

### Linux/macOS developer (no change)
No regressions. Platform-specific code is behind `#[cfg]` guards. CI still runs on Linux. Development workflow unchanged.

### CI/CD operator
Windows CI job catches regressions early. Cross-platform matrix ensures releases work everywhere.

### Contributor
New `#[cfg]` patterns to follow when adding process management code. `ProcessManager` trait provides a clear abstraction. Must test on Windows CI (automatic via matrix).

## Steelman Analysis

### Option A: Tiered Windows support (this decision)

- 🧑‍💻 **Newcomer**: "I can install on my Windows laptop and follow the tutorial without needing Linux or WSL. That's the difference between trying Beamtalk and giving up at step 1."
- 🎩 **Smalltalk developer**: "Pharo runs everywhere — Windows, macOS, Linux. A Smalltalk-inspired language should meet that bar. Platform lock-in contradicts the philosophy of accessible, interactive development."
- ⚙️ **BEAM veteran**: "Erlang has had Windows support since the 90s. A BEAM language that doesn't run on Windows is leaving capability on the table."
- 🏭 **Operator**: "Cross-platform means I can develop on Windows and deploy on Linux — standard workflow. The tiered approach means I get compilation support immediately without waiting for full workspace parity."
- 🎨 **Language designer**: "Tiered rollout is pragmatic — ship the portable parts now, fix the hard parts later. The `ProcessManager` trait creates a clean abstraction boundary."

### Option B: Linux-only, recommend WSL on Windows

- 🧑‍💻 **Newcomer**: "WSL is already installed on most dev machines. Maintaining Windows-native support doubles testing surface for a small team. The Go project was Linux/macOS-focused for years and did fine."
- 🎩 **Smalltalk developer**: "Pharo's Windows support requires constant maintenance for edge cases. If the team is small, focus on one platform and do it well."
- ⚙️ **BEAM veteran**: "The Erlang ecosystem works fine in WSL. Most BEAM developers who use Windows already have WSL. This is a non-issue for the target audience."
- 🏭 **Operator**: "WSL gives a consistent Linux environment. Windows-native support means debugging platform-specific bugs that only surface on one OS."
- 🎨 **Language designer**: "Every `#[cfg]` branch is code that needs testing and maintenance. WSL eliminates the entire category of platform abstraction work."

### Tension Points

1. **Reach vs maintenance** — Windows support doubles the platform-specific test surface but reaches ~45% more developers. The tiered approach mitigates this by only adding Windows code where strictly necessary.
2. **WSL adequacy** — For experienced developers, WSL is fine. For newcomers, "install WSL first" is a friction point that competitors (Gleam, Elixir) don't have.
3. **Process management complexity** — The workspace/REPL code has the most Unix assumptions. A `ProcessManager` trait adds abstraction but also indirection.

## Alternatives Considered

### Alternative: WSL-only on Windows
Declare WSL as a prerequisite for Windows users. No Windows-native code paths needed.

**Rejected because:** Creates a second-class developer experience. Gleam doesn't require WSL. Newcomers hitting "install WSL" as step 1 will try Gleam instead. The core compiler is already portable — refusing to ship it natively on Windows wastes that portability.

### Alternative: Full Windows parity immediately
All features, all platforms, all at once. No tiers.

**Rejected because:** The workspace process management code needs significant work (signal replacements, Windows API integration). Blocking the portable parts (compiler, build) on the hard parts (workspace) delays value delivery. Ship what works now, iterate on the rest.

### Alternative: Cross-platform process library (sysinfo crate)
Use the `sysinfo` crate for all process management, replacing both Unix-specific tools and needing Windows alternatives.

**Deferred, not rejected:** `sysinfo` is a good candidate for the `ProcessManager` implementation but adds a dependency. Evaluate during Tier 2 implementation — it may be simpler than manual Windows API calls. Keep the `ProcessManager` trait so the implementation can be swapped.

## Consequences

### Positive
- Beamtalk runs on Windows — compiler, build, REPL, workspace management
- CI catches Windows regressions automatically
- Matches peer language expectations (Gleam, Elixir, Erlang all support Windows)
- `ProcessManager` trait creates clean abstraction for platform-specific process code
- Tiered approach delivers value incrementally

### Negative
- Maintenance burden increases — `#[cfg]` branches need testing on both platforms
- CI time increases with Windows matrix job
- Some edge cases in process management may behave differently across platforms
- Contributors need to consider Windows when adding process-related code

### Neutral
- Binary distribution needs Windows builds (GitHub Actions cross-compilation is straightforward)
- Documentation needs Windows installation instructions
- `Justfile` needs Windows shell configuration (minor)

## Implementation

### Phase 1: CI and compiler portability
- Add `windows-latest` job to `.github/workflows/ci.yml` running `cargo test` and `cargo clippy`
- Fix `find_beam_pid_by_node()` — add `#[cfg(unix)]` guard with Windows fallback
- Fix `/` root check in `beamtalk_compiler_port.erl` — use `filename:pathtype/1`
- Fix `HOME` → `USERPROFILE` fallback in `beamtalk_workspace_meta.erl`
- Verify `beamtalk build` and `beamtalk new` work in Windows CI
- Add Windows binary to GitHub release workflow

### Phase 2: Process management abstraction
- Create `ProcessManager` trait with platform implementations
- Replace scattered process utility calls with trait methods
- Implement Windows process management (evaluate `sysinfo` crate vs direct Windows API)
- `stop_workspace()` works on Windows
- `beamtalk repl` works on Windows

### Phase 3: Build script portability
- Replace `bash -c compile.sh` rebar3 hook with portable Erlang script
- Add `set windows-shell` to Justfile
- Create PowerShell equivalents for key scripts (or use `just` recipes directly)
- Verify `just test-runtime` works on Windows

**Affected components:** `beamtalk-cli` (process management, paths), runtime (Erlang build hooks, path handling), CI (workflow matrix), Justfile, documentation.

**Estimated size:** L (across all three phases)

## References
- Prior art: [Gleam Windows support](https://gleam.run), [Erlang Windows install](https://www.erlang.org/downloads)
- Related ADRs: ADR 0004 (workspaces — process management), ADR 0022 (embedded compiler — already portable)
- Rust cross-platform patterns: [`#[cfg]` attributes](https://doc.rust-lang.org/reference/conditional-compilation.html), [`sysinfo` crate](https://crates.io/crates/sysinfo)
- Platform-specific code inventory: `workspace/mod.rs`, `beam_compiler.rs`, `paths.rs`
