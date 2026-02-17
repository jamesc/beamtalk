# ADR 0027: Cross-Platform Support

## Status
Accepted (2026-02-16)

## Context

### The Problem

Beamtalk is developed and tested exclusively on Linux (Ubuntu in CI, devcontainers for development). There is no Windows CI, no macOS CI, no Windows testing, and several components use Unix-specific or Linux-specific system calls and tools. A developer on Windows cannot reliably build or run Beamtalk today, and macOS has degraded behavior in workspace management.

This matters because:
1. **Developer reach** — Windows is ~45% of developer desktops, macOS ~30% (as of 2025, per Stack Overflow Developer Survey). Supporting only Linux limits adoption to a fraction of developers.
2. **macOS degradation** — 6 `#[cfg(target_os = "linux")]` guards use `/proc` filesystem for process start-time verification. On macOS, these fall back to skipping stale-node detection, meaning a macOS developer could connect to a wrong/stale workspace without warning. TCP-first workspace management (see Decision) eliminates this entire category of macOS issues.
3. **Peer expectation** — Gleam, Elixir, and Erlang all have first-class Windows and macOS support. A BEAM language that only tests on Linux is an outlier.
4. **CI gaps** — Without Windows or macOS CI, regressions can silently break cross-platform compatibility even for code that should be portable.

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
| `find_beam_pid_by_node()` | Calls `ps` without `#[cfg]` guard | `crates/beamtalk-cli/src/commands/workspace/mod.rs:573` |
| `wait_for_process_exit()` | Unix-only, uses signal probing | `crates/beamtalk-cli/src/commands/workspace/mod.rs:737-761` |
| `stop_workspace()` | Returns error on Windows | `crates/beamtalk-cli/src/commands/workspace/mod.rs:787-812` |
| `/proc` start-time tracking | Linux-only, degrades silently on macOS | `crates/beamtalk-cli/src/commands/workspace/mod.rs:262,293,586` |
| REPL Unix guard | `#[cfg(unix)]` in REPL module | `crates/beamtalk-cli/src/commands/repl/mod.rs:941` |
| rebar3 pre-hook | ~~`bash -c ./compile.sh`~~ Replaced by portable `compile_fixtures.escript` | `runtime/rebar.config:21` |
| Test fixture compilation | ~~Bash script~~ Replaced by `compile_fixtures.escript` | `runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript` |
| Home directory fallback | `os:getenv("HOME", "/tmp")` | `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_meta.erl:173` |
| Project root detection | Hardcoded `/` root check | `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl:153` |
| Justfile | `set shell := ["bash", "-uc"]` | `Justfile:9` |
| CI workflows | Ubuntu-only, bash commands | `.github/workflows/ci.yml` |

### Constraints

- Must not degrade Linux/macOS experience to accommodate Windows
- Must not add significant maintenance burden (Windows-only code paths should be minimal)
- Erlang/OTP must be installed on the target platform (prerequisite, not our problem to solve)
- rebar3 must be available for building the runtime (same prerequisite)
- Should follow the same pattern as Gleam: Rust compiler is fully portable, runtime needs Erlang

## Decision

### Adopt a tiered approach to cross-platform support

**Tier 1 (immediate): Compiler + build command work on Windows**

The core compilation path — `beamtalk build`, `beamtalk new`, `beamtalk test` — must work on Windows. This means:

1. **Add Windows CI job** — `windows-latest` in GitHub Actions, running `cargo test`, `cargo clippy`, and `beamtalk build` on a test project.
2. **Add macOS CI job** — `macos-latest` to catch regressions (macOS works today via Unix compatibility, but without CI, regressions go undetected).
3. **Fix unguarded Unix code** — Wrap `find_beam_pid_by_node()` with `#[cfg(unix)]` and add Windows fallback
4. **Portable path handling** — Replace any hardcoded `/` root checks with `std::path` methods or `Path::has_root()`
5. **No bash dependency for compilation** — The `beamtalk build` command must not require bash. The embedded compiler port (ADR 0022) already avoids shell scripts for compilation.

**Tier 2 (near-term): Workspace and REPL work on Windows**

The interactive development experience — `beamtalk repl`, workspace management — should work:

1. **TCP-first workspace management** — Replace `ps`/signal/`/proc` usage with TCP-based alternatives (see Process Management Strategy below):
   - TCP health probes for liveness checking
   - TCP shutdown messages for graceful termination
   - Minimal `#[cfg]`-guarded force-kill for hung workspaces
2. **Fix `stop_workspace()` on Windows** — Currently returns an error; implement TCP shutdown + force-kill fallback
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

The biggest portability challenge is process management for workspaces. Workspaces already expose TCP ports for REPL and MCP client connections, and the non-Unix fallback for `is_node_running()` already uses TCP probes (line 315-327 in `workspace/mod.rs`). The recommended approach extends this to **TCP-first workspace management**:

1. **Liveness checking** — TCP health probe to workspace port (replaces `ps` + `/proc` + `kill -0` polling)
2. **Graceful shutdown** — TCP shutdown message with cookie authentication triggers `init:stop()` on the workspace's OTP application. This ensures OTP supervision trees shut down cleanly, running all `terminate/2` callbacks. Must use the same cookie auth as WebSocket connections (ADR 0020) to prevent local privilege escalation.
3. **Workspace discovery** — Port file in `~/.beamtalk/workspaces/` (already implemented). Port files should include a nonce verified on connection to detect stale entries.
4. **Force-kill (break glass only)** — OS-specific code for workspaces that don't respond to TCP shutdown within a timeout. This is a last resort for truly hung BEAM processes (e.g., stuck in NIF, crash loop before TCP listener starts). On Windows, `TerminateProcess` requires handle permissions — use `sysinfo` crate if edge cases proliferate.

**Why OTP-level shutdown matters:** `init:stop()` gives the BEAM a chance to flush state, persist data, and run cleanup callbacks. This is critical for future persistence features (workspace state, actor snapshots, session history). An OS-level kill (`kill -9`, `TerminateProcess`) bypasses all of this and risks data loss. By making OTP shutdown the primary path and OS kill the rare fallback, we get:
- Clean shutdown on all platforms (no `#[cfg]` needed for the happy path)
- Safe foundation for persistence features
- Force-kill only for the "break glass" scenario (~5% of shutdowns)

```rust
// TCP-first: portable liveness and shutdown
fn is_workspace_running(port: u16) -> bool {
    TcpStream::connect_timeout(&addr, Duration::from_secs(1)).is_ok()
}

fn stop_workspace(port: u16, cookie: &str) -> Result<()> {
    // 1. Send authenticated shutdown message over TCP
    //    → workspace calls init:stop() → OTP terminate callbacks run
    // 2. Wait for process exit with timeout (TCP disconnect or port probe)
    // 3. Only if timeout expires: OS force-kill (break glass)
}

// Only force-kill needs #[cfg] guards — and it's rarely invoked:
#[cfg(unix)]
fn force_kill(pid: u32) -> Result<()> { /* kill -9 */ }
#[cfg(windows)]
fn force_kill(pid: u32) -> Result<()> { /* TerminateProcess via sysinfo */ }
```

**Tradeoffs:**
- ✅ Eliminates most `ps`/`kill`/`/proc` platform-specific code
- ✅ OTP-level shutdown is cross-platform with no `#[cfg]` needed
- ✅ Safe foundation for persistence (flush state before exit)
- ✅ Aligns with REPL client and MCP server (already TCP-based)
- ✅ Enables future remote workspace management (SSH, Docker, cloud)
- ⚠️ **Observability loss** — TCP probes detect "port open" vs "port closed" but cannot detect a live-but-unresponsive BEAM process (e.g., hung in NIF). This is an acceptable tradeoff — a hung workspace that doesn't respond to TCP also won't respond to REPL or MCP clients.
- ⚠️ **Port file reliability** — Port files become the primary workspace discovery mechanism. Stale port files (from crashed workspaces) need handling: connect + nonce verification, with cleanup on failure.

## Prior Art

### Pharo (Smalltalk)
Full cross-platform support (Windows, macOS, Linux, ARM). The OpenSmalltalk VM abstracts OS differences — the same Pharo image runs unchanged on any supported platform. Platform-specific code is isolated in VM plugins and FFI bindings; application-level Smalltalk code is entirely portable.

**Relevant:** Pharo's "portable image on cross-platform VM" model parallels Beamtalk's architecture — portable `.beam` bytecode on cross-platform BEAM VM. In both cases, the runtime VM handles portability; the issue is tooling around it (build scripts, process management).

### Gleam
Full Windows support from early on. Rust compiler is inherently portable. Uses `std::process::Command` for `erl`/`erlc` invocation (works cross-platform if Erlang is in PATH). CI runs on Windows. Ships Windows binaries via GitHub releases.

**Adopted:** Same model — Rust compiler portable, platform differences in process management only.

**Key difference:** Gleam's compilation is stateless batch processing (compile files → output `.beam`). Beamtalk adds persistent workspaces, live hot-reload into running nodes, and process start-time tracking for stale-node detection. The process management layer is substantially more complex than Gleam's.

### Elixir
Works on Windows via Erlang/OTP's Windows support. `mix` (Elixir's build tool) is written in Elixir itself, running on BEAM — inherently cross-platform. Some ecosystem tools (like `phoenix_live_reload`) use `inotifywait` which requires alternatives on Windows.

**Adopted:** Philosophy that the BEAM runtime handles most portability; the compiler/tooling layer just needs to invoke it correctly.

### Rust (Cargo)
Exemplary cross-platform support. Uses `#[cfg(target_os)]` extensively. Has `std::process::Command` that handles path differences. CI matrix includes `windows-latest`, `macos-latest`, `ubuntu-latest`.

**Adopted:** `#[cfg]` attribute pattern, CI matrix strategy, TCP-first workspace management.

### Erlang/OTP
Erlang itself has excellent Windows support — prebuilt Windows installers, `werl` (Windows Erlang shell), proper Windows service support. `rebar3` works on Windows but some plugins assume Unix tools. The BEAM VM is fully portable.

**Relevant:** Beamtalk's runtime (Erlang) is already portable. The issues are in the Rust tooling layer and bash-dependent build scripts.

## User Impact

### Windows developer (primary beneficiary)
Can download a binary or `cargo install beamtalk`, run `beamtalk new myapp`, `beamtalk build`, and `beamtalk repl` — the full workflow works. No WSL, no bash, no Unix tools required beyond Erlang/OTP.

### Linux/macOS developer (improved)
macOS: TCP-first workspace management eliminates `/proc` degradation — stale-node detection works reliably via TCP probes instead of process-level checks. Other macOS quirks (Gatekeeper quarantine on downloaded binaries, different signal handling in `open_port`) may need individual fixes but are not systematic. Linux: no change.

### CI/CD operator
Windows and macOS CI jobs catch regressions early. Cross-platform matrix ensures releases work everywhere.

### Contributor
New `#[cfg]` patterns to follow when adding process management code. TCP-first approach means most workspace management code is platform-agnostic. Only force-kill remains OS-specific. Must test on Windows CI (automatic via matrix).

## Steelman Analysis

### Option A: Tiered cross-platform support (this decision)

- 🧑‍💻 **Newcomer**: "I can install on my Windows laptop and follow the tutorial without needing Linux or WSL. That's the difference between trying Beamtalk and giving up at step 1."
- 🎩 **Smalltalk developer**: "Pharo runs everywhere — Windows, macOS, Linux. A Smalltalk-inspired language should meet that bar. Platform lock-in contradicts the philosophy of accessible, interactive development."
- ⚙️ **BEAM veteran**: "Erlang has had Windows support since the 90s. A BEAM language that doesn't run on Windows is leaving capability on the table."
- 🏭 **Operator**: "Cross-platform means I can develop on Windows and deploy on Linux — standard workflow. The tiered approach means I get compilation support immediately without waiting for full workspace parity."
- 🎨 **Language designer**: "Tiered rollout is pragmatic — ship the portable parts now, fix the hard parts later. TCP-first workspace management means most code is platform-agnostic with only force-kill needing `#[cfg]`."

### Option B: Linux-only, recommend WSL on Windows

- 🧑‍💻 **Newcomer**: "WSL is already installed on most dev machines. Maintaining Windows-native support doubles testing surface for a small team. The Go project was Linux/macOS-focused for years and did fine."
- 🎩 **Smalltalk developer**: "Pharo's Windows support requires constant maintenance for edge cases. If the team is small, focus on one platform and do it well."
- ⚙️ **BEAM veteran**: "The Erlang ecosystem works fine in WSL. Most BEAM developers who use Windows already have WSL. This is a non-issue for the target audience."
- 🏭 **Operator**: "WSL gives a consistent Linux environment. Windows-native support means debugging platform-specific bugs that only surface on one OS."
- 🎨 **Language designer**: "Every `#[cfg]` branch is code that needs testing and maintenance. WSL eliminates the entire category of platform abstraction work."

### Tension Points

1. **Reach vs maintenance** — Windows support doubles the platform-specific test surface but reaches ~45% more developers. The tiered approach mitigates this by only adding Windows code where strictly necessary.
2. **WSL adequacy** — For experienced developers, WSL is fine. For newcomers, "install WSL first" is a friction point that competitors (Gleam, Elixir) don't have.
3. **Process management complexity** — The workspace/REPL code has the most Unix assumptions. TCP-first management eliminates most platform-specific code, but force-kill and process startup still need `#[cfg]` guards.

## Alternatives Considered

### Alternative: WSL-only on Windows
Declare WSL as a prerequisite for Windows users. No Windows-native code paths needed.

**Rejected because:** Creates a second-class developer experience. Gleam doesn't require WSL. Newcomers hitting "install WSL" as step 1 will try Gleam instead. The core compiler is already portable — refusing to ship it natively on Windows wastes that portability.

### Alternative: Full Windows parity immediately
All features, all platforms, all at once. No tiers.

**Rejected because:** The workspace process management code needs significant work (signal replacements, Windows API integration). Blocking the portable parts (compiler, build) on the hard parts (workspace) delays value delivery. Ship what works now, iterate on the rest.

### Alternative: Cross-platform process library (sysinfo crate)
Use the `sysinfo` crate for remaining OS-level operations (force-kill), replacing manual `#[cfg]` code.

**Deferred, likely needed:** `sysinfo` handles Windows `TerminateProcess` edge cases (handle permissions, access denied, wait-for-termination) that would otherwise require significant `#[cfg]` code. The "~10 lines" estimate for manual force-kill is optimistic — proper Windows error handling could reach 40-50 lines. Adopt `sysinfo` if force-kill complexity exceeds a single function.

### Rejected: ProcessManager trait abstraction (superseded by TCP-first)
Originally proposed abstracting all process operations behind a `ProcessManager` trait with Unix and Windows implementations. TCP-first management eliminates the need for most trait methods (`is_running`, `find_pid_by_name`, `terminate`). Only `force_terminate` needs platform-specific code, which doesn't justify a trait.

## Consequences

### Positive
- Beamtalk runs on Windows — compiler, build, REPL, workspace management
- macOS improved — TCP-first eliminates `/proc` degradation; remaining quirks (Gatekeeper, signal handling) addressed individually
- CI catches Windows and macOS regressions automatically
- Matches peer language expectations (Gleam, Elixir, Erlang all support Windows)
- TCP-first workspace management means most code is platform-agnostic, reducing `#[cfg]` surface
- OTP-level shutdown (`init:stop()`) ensures clean teardown — safe foundation for future persistence (workspace state, actor snapshots, session history)
- Tiered approach delivers value incrementally
- TCP foundation enables future remote workspace management (SSH, Docker, cloud)

### Negative
- Maintenance burden increases — `#[cfg]` branches need testing on both platforms
- CI time increases with Windows and macOS matrix jobs
- Some edge cases in process management may behave differently across platforms
- Contributors need to consider Windows when adding process-related code

### Neutral
- Binary distribution needs Windows builds (GitHub Actions cross-compilation is straightforward)
- Documentation needs Windows installation instructions
- `Justfile` needs Windows shell configuration (minor)

## Implementation

### Phase 1: CI and compiler portability
- Add `windows-latest` and `macos-latest` jobs to `.github/workflows/ci.yml` running `cargo test` and `cargo clippy`
- Fix `find_beam_pid_by_node()` — add `#[cfg(unix)]` guard with Windows fallback
- Fix `/` root check in `beamtalk_compiler_port.erl` — use `filename:pathtype/1`
- Fix `HOME` → `USERPROFILE` fallback in `beamtalk_workspace_meta.erl`
- Verify `beamtalk build` and `beamtalk new` work in Windows CI
- Add Windows binary to GitHub release workflow
- **macOS note:** After TCP-first workspace management (Phase 2), macOS `/proc` degradation is eliminated. macOS CI ensures no new platform-specific regressions.
- **CI matrix strategy:** Run fast checks (clippy, rustfmt, unit tests) on all platforms; run slow tests (E2E, stdlib) only on Linux to limit CI time. Windows runners are ~2x slower than Linux.

### Phase 2: TCP-first workspace management
- Add TCP health endpoint to workspace (probe port for liveness)
- Add TCP shutdown message → workspace calls `init:stop()` (OTP-level graceful shutdown)
- Replace `ps`/`kill -0`/`/proc` checks with TCP probes in `is_node_running()`, `find_beam_pid_by_node()`, `wait_for_process_exit()`
- Add `#[cfg]`-guarded force-kill as break-glass fallback (consider `sysinfo` crate for Windows)
- `stop_workspace()` works on all platforms: TCP→`init:stop()` (primary), OS force-kill (timeout fallback)
- `beamtalk repl` works on Windows

### Phase 3: Build script portability
- ~~Replace `bash -c compile.sh` rebar3 hook with portable Erlang script~~ ✅ Done (`compile_fixtures.escript`)
- ~~Add `set windows-shell` to Justfile~~ ✅ Done
- ~~Create PowerShell equivalents for key scripts (or use `just` recipes directly)~~ ✅ Done (`scripts/worktree-*.ps1`)
- Verify `just test-runtime` works on Windows

**Affected components:** `beamtalk-cli` (process management, paths), runtime (Erlang build hooks, path handling), CI (workflow matrix), Justfile, documentation.

**Affected test suites:** `workspace/mod.rs` tests (18+ workspace management tests), `paths.rs` tests (PPID/session tests), `beam_compiler.rs` tests (23 compiler tests), `beamtalk_workspace_meta_tests.erl` (18 metadata tests), `beamtalk_compiler_port_tests.erl` (8 port tests). All must pass on Windows and macOS CI.

**Estimated size:** L (across all three phases)

## Migration Path

Not applicable. This is an infrastructure enhancement — no existing user code, APIs, or language semantics change. All modifications are internal to the toolchain (CI configuration, build scripts, `#[cfg]` guards in workspace management).

## Implementation Tracking

**Epic:** [BT-609](https://linear.app/beamtalk/issue/BT-609)
**Status:** Done

| Phase | Issue | Title | Size |
|-------|-------|-------|------|
| 1 | BT-610 | Fix cross-platform code issues (Rust + Erlang) | M |
| 2 | BT-611 | Add TCP health and shutdown endpoints to workspace | L |
| 2 | BT-612 | Replace process management with TCP probes and force-kill | L |
| 3 | BT-613 | Add cross-platform CI and release workflow | M |
| 3 | BT-619 | Bundle beamtalk-lsp per-platform in VS Code extension | M |

**Dependency graph:**
```
BT-610 (code fixes)
  ├── BT-611 (TCP endpoints) ──► BT-612 (TCP process mgmt)
  │                                      │
  └──────────────────────────────────────►├── BT-613 (CI + release)
```

## References
- Prior art: [Gleam Windows support](https://gleam.run), [Erlang Windows install](https://www.erlang.org/downloads)
- Related ADRs: ADR 0004 (persistent workspaces — architecture affected by TCP-first), ADR 0017 (browser connectivity — originally assumed Unix host, but ADR 0022 eliminated the Unix socket daemon; **Proposed**), ADR 0020 (connection security — TCP shutdown must use same cookie auth; **Proposed**), ADR 0022 (embedded compiler — already portable, solved Unix socket dependency), ADR 0024 (IDE tooling — workspace dependency affects Windows IDE experience)
- Rust cross-platform patterns: [`#[cfg]` attributes](https://doc.rust-lang.org/reference/conditional-compilation.html), [`sysinfo` crate](https://crates.io/crates/sysinfo)
- Platform-specific code inventory: `workspace/mod.rs` (31 `#[cfg]` guards), `beam_compiler.rs`, `paths.rs`, `repl/mod.rs`
