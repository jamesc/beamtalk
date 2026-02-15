# ADR 0022: Embedded Compiler via OTP Port (with NIF option)

## Status
Implemented (2026-02-15)

## Context

### Problem Statement

The Beamtalk compiler currently runs as a **separate daemon process** written in Rust, communicating with the BEAM runtime via JSON-RPC over Unix domain sockets. This architecture creates several pain points:

1. **Windows incompatibility** — The daemon uses Unix domain sockets (`~/.beamtalk/sessions/<session>/daemon.sock`) and Unix-specific lifecycle management (`SIGTERM`, `O_EXCL` lockfiles). Windows has no native Unix socket support.

2. **Deployment complexity** — Users must install both the `beamtalk` Rust binary *and* Erlang/OTP. The daemon must be started before the REPL can compile, adding a process management concern.

3. **Daemon lifecycle fragility** — If the daemon crashes, the socket file remains orphaned. Clients get `{error, {daemon_unavailable, ...}}` errors. Recovery requires manual intervention (`beamtalk daemon stop && beamtalk daemon start`).

4. **Serialization overhead** — Every compilation round-trips through JSON-RPC: Beamtalk source → JSON → Unix socket → JSON parse → compile → JSON encode → Unix socket → JSON parse → Core Erlang string. For REPL interactions this adds measurable latency.

5. **Two-process coordination** — The REPL must discover the daemon socket path, handle connection failures, manage timeouts, and deal with protocol version mismatches. This is ~100 lines of IPC code in `beamtalk_repl_eval.erl` (daemon connection, JSON-RPC encoding, response parsing).

### Current Architecture

```text
┌─────────────────────┐         ┌─────────────────────┐
│   beamtalk CLI      │         │   BEAM Node          │
│   (Rust binary)     │         │                      │
│                     │  JSON   │  beamtalk_workspace  │
│  ┌───────────────┐  │  RPC    │  ┌────────────────┐  │
│  │ Compiler      │◄─┼────────┼──│ repl_eval      │  │
│  │ Daemon        │  │  Unix   │  │ (Erlang)       │  │
│  │ (Rust)        │──┼─socket──┼─►│                │  │
│  └───────────────┘  │         │  └────────────────┘  │
│                     │         │                      │
│  ┌───────────────┐  │         │  beamtalk_runtime    │
│  │ beam_compiler │  │ escript │  beamtalk_stdlib     │
│  │ (Core→BEAM)   │──┼────────┼─►(erlc)              │
│  └───────────────┘  │         │                      │
└─────────────────────┘         └──────────────────────┘
```

The daemon exposes five JSON-RPC methods: `compile`, `compile_expression`, `diagnostics`, `ping`, and `shutdown`. The REPL connects via `gen_tcp:connect({local, SocketPath}, ...)` and sends line-delimited JSON.

### Constraints

- **Compilation is CPU-bound** — Parsing, semantic analysis, and codegen are CPU-intensive. NIF calls must not block the BEAM scheduler.
- **`erlc` dependency remains** — Core Erlang → BEAM bytecode compilation uses OTP's `compile` module. This doesn't go away with embedding.
- **Platform matrix** — Must support at minimum: linux-x86_64, linux-aarch64, macos-x86_64, macos-aarch64, windows-x86_64.
- **beamtalk-core has no Erlang dependencies** — The `beamtalk-core` crate is pure Rust (lexer, parser, analyzer, codegen). It can be wrapped by Rustler without introducing circular dependencies.

## Decision

**Replace the separate compiler daemon with a `beamtalk_compiler` OTP application that abstracts the compilation backend. Start with OTP Port as the primary backend; add Rustler NIF as an optional high-performance backend later if incremental analysis demands sub-millisecond overhead.**

The `beamtalk-core` crate (lexer, parser, semantic analysis, codegen) will be compiled as a standalone binary invoked via OTP Port, managed by an OTP supervisor. The REPL and build tools will call the compiler through the `beamtalk_compiler` API instead of JSON-RPC over Unix sockets. A NIF backend can be added behind `beamtalk_compiler_backend` if latency requirements change.

**Rationale for Port-first:** The steelman analysis (below) shows that Port solves every stated problem (Windows, daemon lifecycle, deployment) with better fault isolation than NIF. The latency difference (approximately 2 ms versus 0.01 ms) is negligible compared to 10–500 ms compilation times; NIF's sub-millisecond advantage matters only if compilation becomes a keystroke-level hot path, which is not on the current roadmap.

### Architecture After

```text
┌──────────────────────────────────────────┐
│              BEAM Node                    │
│                                           │
│  beamtalk_workspace (Live Programming)    │
│  ┌────────────────┐                       │
│  │ repl_eval      │                       │
│  │ (Erlang)       │                       │
│  └───────┬────────┘                       │
│          │ compile_expression/3            │
│          ▼                                │
│  beamtalk_compiler (Anti-Corruption Layer)│
│  ┌────────────────┐  ┌──────────────────┐ │
│  │ compiler_      │  │ beamtalk-core    │ │
│  │ backend.erl    │─►│ (Rust, OTP Port) │ │
│  └────────────────┘  └──────────────────┘ │
│                                           │
│  beamtalk_runtime  (Actor/Object System)  │
│  beamtalk_stdlib   (Standard Library)     │
│                                           │
│  OTP compile module (Core Erlang → BEAM)  │
└───────────────────────────────────────────┘
```

### Compiler API

The `beamtalk_compiler` module exposes a backend-agnostic API. The implementation dispatches to the configured backend (Port by default, NIF in future Phase 6):

```erlang
-module(beamtalk_compiler).
-export([compile/2, compile_expression/3, diagnostics/1, version/0]).

%% Compile a file, returning Core Erlang + diagnostics
-spec compile(Source :: binary(), ModuleName :: binary()) ->
    {ok, #{core_erlang := binary(), diagnostics := [map()]}} |
    {error, #{diagnostics := [map()]}}.
compile(Source, ModuleName) ->
    beamtalk_compiler_backend:compile(Source, ModuleName).

%% Compile a REPL expression with known variable bindings
-spec compile_expression(Source :: binary(), ModuleName :: binary(),
                         KnownVars :: [binary()]) ->
    {ok, #{core_erlang := binary(), diagnostics := [map()]}} |
    {error, #{diagnostics := [map()]}}.
compile_expression(Source, ModuleName, KnownVars) ->
    beamtalk_compiler_backend:compile_expression(Source, ModuleName, KnownVars).

%% Get diagnostics only (no codegen)
-spec diagnostics(Source :: binary()) ->
    {ok, [map()]}.
diagnostics(Source) ->
    beamtalk_compiler_backend:diagnostics(Source).

%% Return compiler version
-spec version() -> binary().
version() ->
    beamtalk_compiler_backend:version().
```

### Dirty Scheduler Usage (Future NIF Backend — Phase 6)

If the NIF backend is added in Phase 6, all compilation NIFs will use `schedule = "DirtyCpu"` to avoid blocking the BEAM scheduler:

```rust
#[rustler::nif(schedule = "DirtyCpu")]
fn compile(source: Binary, module_name: Binary) -> NifResult<Term> {
    // ... parse, analyze, codegen ...
}
```

Compilation typically takes 1–50 ms for REPL expressions and up to several seconds for large files — well beyond the 1 ms NIF budget for normal schedulers. The Port backend avoids this concern entirely since compilation runs in a separate OS process.

### Port Wire Format

The Port uses **Erlang External Term Format (ETF)** over length-prefixed frames (`{packet, 4}`):

- **Erlang side:** `term_to_binary/1` and `binary_to_term/1` — zero-copy, no parsing library needed
- **Rust side:** `eetf` or `erlang-term` crate for ETF encode/decode
- **Framing:** 4-byte big-endian length header + ETF payload, handled automatically by `open_port/2` with `{packet, 4}`

```erlang
%% Erlang sends a request to the Port
Port = open_port({spawn_executable, CompilerBinary}, [{packet, 4}, binary, exit_status]),
Request = term_to_binary(#{command => compile, source => Source, module => ModuleName}),
port_command(Port, Request),

%% Erlang receives the response
receive
    {Port, {data, Data}} ->
        Response = binary_to_term(Data)
        %% #{status => ok, core_erlang => ..., diagnostics => [...]}
end.
```

**Why ETF over JSON or protobuf:**
- No serialization library on the Erlang side (built-in `term_to_binary`)
- Native support for Erlang maps, binaries, atoms, and lists
- Eliminates the JSON-RPC overhead that motivated the daemon replacement
- Protobuf adds a schema dependency and build step with no benefit for this use case

### REPL Integration

`beamtalk_repl_eval.erl` simplifies from ~200 lines of socket/JSON-RPC code to a direct function call:

```erlang
%% Before (daemon)
compile_via_daemon(Expression, ModuleName, Bindings, State) ->
    SocketPath = beamtalk_repl_state:get_daemon_socket_path(State),
    case connect_to_daemon(SocketPath) of
        {ok, Socket} ->
            Request = jsx:encode(#{...}),
            gen_tcp:send(Socket, [Request, <<"\n">>]),
            receive_and_parse_response(Socket);
        {error, _} ->
            {error, {daemon_unavailable, SocketPath}}
    end.

%% After (beamtalk_compiler — Port backend)
compile_expression(Expression, ModuleName, Bindings) ->
    KnownVars = [atom_to_binary(V) || V <- maps:keys(Bindings)],
    beamtalk_compiler:compile_expression(Expression, ModuleName, KnownVars).
```

### Precompiled Binaries (OTP Port)

For the OTP Port backend, we distribute the **compiler as a standalone executable**, not as a NIF shared library. The Erlang node starts this executable via `open_port/2`.

| Platform | Architecture | Executable artifact |
|----------|-------------|---------------------|
| Linux (glibc) | x86_64, aarch64 | `beamtalk_compiler_port` |
| Linux (musl) | x86_64, aarch64 | `beamtalk_compiler_port` |
| macOS | x86_64, aarch64 | `beamtalk_compiler_port` |
| Windows | x86_64 | `beamtalk_compiler_port.exe` |

CI builds these precompiled Port binaries via a GitHub Actions cross-compilation matrix. Users without a Rust toolchain get the appropriate executable downloaded automatically; if the platform is not covered, the Erlang side falls back to compiling the compiler from source at install time.

> **NIF backend note (Phase 6):** If we later introduce a Rustler-based NIF backend, its `.so`/`.dylib`/`.dll` distribution and any `rustler_precompiled` usage will be specified in a separate ADR/phase, not here.

### Core Erlang → BEAM Compilation

The `erlc` step (Core Erlang → BEAM bytecode) moves **inside the BEAM node**, fully in-memory — no temp files, no disk I/O. The Port returns Core Erlang as a binary in the ETF response, which is parsed and compiled directly:

```erlang
%% Fully in-memory: Core Erlang binary → scan → parse → compile → load
compile_core_to_beam(CoreErlangBin, ModuleName) ->
    {ok, Tokens, _} = core_scan:string(binary_to_list(CoreErlangBin)),
    {ok, Forms} = core_parse:parse(Tokens),
    case compile:forms(Forms, [from_core, binary, return_errors]) of
        {ok, ModuleName, BeamBinary} ->
            code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", BeamBinary);
        {error, Errors, _Warnings} ->
            {error, Errors}
    end.
```

This eliminates both the `escript` subprocess spawn and temporary `.core` files on disk (ref: BT-48). The entire pipeline is in-memory: `Source → Port/ETF → Core Erlang binary → core_scan → core_parse → compile:forms → code:load_binary`.

## Prior Art

### Gleam (Rust compiler, targets BEAM)
Gleam's compiler is a standalone Rust binary that generates Erlang source files. It does **not** embed into the BEAM — it's a build tool that runs before the BEAM starts. Gleam has no REPL (as of 2026), so the latency of a separate process isn't a concern.

**What we learn:** A separate Rust compiler works well for batch compilation. But Beamtalk's interactive-first philosophy demands tighter integration for REPL responsiveness.

### Elixir + Rustler ecosystem
Many Elixir libraries use Rustler NIFs for CPU-intensive work: `explorer` (data frames), `tokenizers` (ML tokenization), `html5ever` (HTML parsing). These prove the pattern is production-ready at scale.

**What we learn:** `rustler_precompiled` solves the distribution problem. Dirty CPU schedulers handle CPU-bound work safely. The pattern is well-established.

### Pharo/Squeak (Smalltalk)
The compiler is embedded in the image — parsing and compilation happen inside the VM. This enables the live, interactive development that Beamtalk aspires to.

**What we learn:** Embedding the compiler is the Smalltalk way. The compiler should be part of the live environment, not external tooling.

### LFE (Lisp Flavoured Erlang)
LFE's compiler is written in Erlang and runs inside the BEAM. Compilation is a function call, not an external process. This gives LFE a seamless REPL experience.

**What we learn:** In-process compilation on BEAM is the natural model. Our Rust compiler needs to cross the Port boundary, but the result should feel the same.

### TypeScript (Mainstream — language server architecture)
TypeScript's `tsc` is a standalone compiler, but `tsserver` (the language server) embeds the compiler for IDE responsiveness. The language server runs as a separate Node.js process communicating via JSON-RPC — similar to our current daemon. TypeScript considered but rejected in-process embedding for VS Code due to crash isolation concerns.

**What we learn:** Even mainstream toolchains face the same daemon-vs-embedded trade-off. TypeScript chose process isolation for safety. However, TypeScript's compilation is orders of magnitude heavier than Beamtalk's; the risk calculus is different for a language with sub-100ms REPL compilations.

## User Impact

### Newcomer
- **Positive:** Single install step (Erlang/OTP + beamtalk package). No daemon to manage.
- **Positive:** Windows support opens the platform to more developers.
- **Positive:** Faster REPL response (no JSON-RPC overhead).
- **Positive (Port):** If the compiler crashes, only the port dies — the supervisor restarts it automatically. No REPL state lost.
- **Concern (NIF, Phase 6):** If a future NIF crash kills the node, a newcomer loses all REPL state with no explanation. This is why Port is the default.

### Smalltalk Developer
- **Positive:** Compiler-in-the-image aligns with Smalltalk philosophy.
- **Neutral:** The compiler is still Rust, not Beamtalk. But the boundary becomes invisible.
- **Concern (NIF):** Smalltalk developers expect the compiler to be introspectable and modifiable. A NIF is a black box — you can't browse its methods in the inspector. A Port is similarly opaque but safer.

### Erlang/BEAM Developer
- **Positive:** Standard OTP application, standard Port supervision. No foreign process management.
- **Positive:** Can inspect compiler module with standard tools (`observer`, `code:which/1`).
- **Positive (Port):** Ports are the idiomatic BEAM pattern for native code. The anti-corruption layer pattern means the backend can be swapped without changing the rest of the system.

### Production Operator
- **Positive:** One process to monitor, not two. Standard OTP supervision.
- **Positive:** No socket file management, no daemon health checks.
- **Positive (Port):** Compiler crash = port restart = automatic recovery. No actors lost.
- **Concern:** Compiler binary must match the platform. Precompiled binaries mitigate this.

## Steelman Analysis

### Option A: OTP Port (Supervised External Process) — Recommended
- 🧑‍💻 **Newcomer**: "Same simplicity as NIF — no daemon management, no socket files. But if the compiler crashes, only the port dies. The supervisor restarts it in milliseconds and I try again."
- 🎩 **Smalltalk purist**: "The compiler is *accessible* from the live environment (supervised, restartable) without being *embedded* in it. Best of both worlds."
- ⚙️ **BEAM veteran**: "Ports are *the* BEAM pattern for native code — they exist precisely because NIFs are risky. OTP's `heart` module, `epmd`, and `inet_gethost` all use ports, not NIFs. The compiler is a perfect port use case: infrequent, CPU-intensive, crash-tolerant. The ~2 ms port overhead is invisible in a REPL interaction where the user is typing."
- 🏭 **Operator**: "Port crash = supervisor restart = automatic recovery. NIF crash = node down = all actors dead = manual recovery. For a production workspace with running actors, this matters enormously."
- 🎨 **Language designer**: "Ports solve every problem the daemon has (no socket files, no manual lifecycle, Windows-compatible via stdin/stdout) without the NIF risk. The serialization overhead is real but small: approximately 2 ms for port versus 0.01 ms for NIF versus 5–10 ms for daemon. The user perceives none of these — compilation itself takes 10–500 ms."

### Option B: Keep Separate Daemon (Status Quo)
- 🧑‍💻 **Newcomer**: "If the compiler crashes, the REPL keeps running with all my actors alive. Restart the daemon and compile again — no state lost."
- 🎩 **Smalltalk purist**: "Process isolation is an Erlang virtue. The BEAM exists because shared-memory systems are fragile. A NIF reintroduces that fragility."
- ⚙️ **BEAM veteran**: "NIFs are dangerous — the `crypto`/`ssl` comparison is misleading. Those are tiny stateless functions. A compiler NIF is large, stateful, runs for hundreds of milliseconds, and exercises complex code paths (error recovery, diagnostics formatting, Core Erlang generation). One memory corruption bug and the whole node is gone — all actors, all state, all sessions. A daemon crash loses one compilation attempt."
- 🏭 **Operator**: "I can run the compiler under `rust-gdb` or `valgrind` independently. I can upgrade the compiler without restarting the runtime. I can rate-limit or load-balance compilation across multiple daemons. None of this works with a NIF."
- 🎨 **Language designer**: "The daemon boundary *is* the DDD anti-corruption layer — enforced by process isolation, not just convention. A NIF boundary is a function signature; a process boundary is a wall. Also: the daemon could serve multiple workspaces simultaneously."

### Option C: Embedded Compiler (Rustler NIF) — Future Option (Phase 6)
- 🧑‍💻 **Newcomer**: "One thing to install, it just works. No daemon to understand or debug."
- 🎩 **Smalltalk purist**: "The compiler belongs inside the live environment. This is closer to the Smalltalk ideal than a separate process. In Pharo, you'd never run the compiler as an external daemon."
- ⚙️ **BEAM veteran**: "NIFs are well-understood for small, focused operations. Dirty schedulers prevent blocking. The `beamtalk_compiler` app encapsulates the NIF — if we need to swap to a port later, only that app changes."
- 🏭 **Operator**: "One BEAM node to monitor. Standard OTP release. No IPC failure modes. No stale socket files to clean up."
- 🎨 **Language designer**: "Eliminates an entire category of errors (daemon unavailable, socket stale, JSON parse failure, version mismatch). Lowest possible latency for REPL interactions."

### Tension Points

**The core tension is latency vs fault isolation:**

| | NIF | Port | Daemon |
|---|---|---|---|
| Call overhead | ~0.01ms | ~2ms | ~5-10ms |
| Typical compilation | 10-500ms | 10-500ms | 10-500ms |
| User-perceived difference | None | None | Slight |
| Compiler crash impact | **Node dies** (all actors, state, sessions lost) | Port restarts (~50ms) | Daemon restarts (~500ms) |
| Windows support | ✅ .dll | ✅ stdin/stdout | ❌ No Unix sockets (TCP workaround) |
| Deployment | Single release | Single release + binary | Two components |
| Debuggability | Hard (NIF in BEAM process) | Easy (separate process) | Easy (separate process) |
| Independent upgrades | No | No | Yes |

**Key observations:**
1. **Latency doesn't differentiate.** All three options are dominated by compilation time (10–500 ms). The call overhead difference (0.01 ms vs 2 ms vs 10 ms) is noise.
2. **Fault isolation is the real differentiator.** In a production workspace with running actors, a NIF crash is catastrophic. A port crash is a hiccup.
3. **The `crypto`/`ssl` comparison is misleading.** Those NIFs run ~0.1 ms stateless operations. A compiler NIF runs ~100 ms with complex state. Different risk profile entirely.
4. **Port solves the same deployment problems as NIF** — no socket files, no daemon lifecycle, Windows-compatible — without the crash risk.
5. **NIF's only real advantage is if compilation becomes a hot path** — e.g., live recompilation on every keystroke for incremental analysis. Today's REPL model (compile on Enter) doesn't need sub-millisecond overhead.

## Alternatives Considered

### Alternative 1: Keep Separate Daemon (Status Quo)
The daemon works today on Linux and macOS. Windows support could be added with TCP instead of Unix sockets.

**Rejected because:**
- Adds permanent complexity (daemon lifecycle, IPC protocol, error handling)
- TCP on Windows introduces security concerns (other local processes can connect)
- Doesn't address deployment complexity (two components to install and coordinate)
- JSON-RPC serialization overhead is unnecessary when both sides are in the same process

### Alternative 1b: Daemon with TCP for Windows (Incremental Fix)
Keep the daemon architecture but replace Unix sockets with TCP, adding named pipe support on Windows. Lowest-risk change.

**Not chosen because:**
- Fixes Windows but doesn't simplify deployment or eliminate daemon lifecycle complexity
- TCP on localhost has security implications (any local process can connect)
- Named pipes on Windows add a third transport to maintain (Unix socket, TCP, named pipe)
- Preserves all existing IPC complexity (~100 lines) that embedding eliminates
- However: this is the **safest short-term option** if NIF/Port work is delayed

### Alternative 2: Rustler NIF (Embedded Compiler)
Embed the Rust compiler directly into the BEAM node as a NIF using Rustler. Lowest possible call overhead (~0.01 ms), native term exchange, single OTP release with no separate binary.

**Deferred to Phase 6 because:**
- Compiler NIF is large, stateful, and runs for 10–500 ms — different risk profile than typical NIFs (`crypto`, `ssl`)
- NIF crash kills the entire BEAM node (all actors, state, sessions lost)
- Port solves the same deployment problems (no socket files, no daemon lifecycle, Windows-compatible) without the crash risk
- NIF's sub-millisecond advantage is irrelevant when compilation itself takes 10–500 ms
- May revisit if incremental analysis or keystroke-level compilation demands sub-millisecond overhead

### Alternative 3: WebAssembly (Compile beamtalk-core to WASM, run in BEAM)
Compile the Rust compiler to WASM and run it via a WASM runtime (wasmex) inside the BEAM.

**Rejected because:**
- WASM adds significant performance overhead (2-5x slower than native)
- WASM-BEAM interop is less mature than Rustler
- Compilation is CPU-bound — we want maximum performance, not sandboxing
- Adds a WASM runtime dependency

### Alternative 4: Rewrite Compiler in Erlang/Elixir
Rewrite the compiler (lexer, parser, codegen) in Erlang or Elixir so it runs natively inside the BEAM with zero IPC overhead. This is what LFE does — the compiler is just Erlang modules, compilation is a function call.

**Arguments for:**
- Zero boundary — no Port, no NIF, no serialization, no separate binary. Compilation is a direct function call with native Erlang terms
- Hot code upgradeable — load a new compiler version without restarting anything
- Inspectable — Smalltalk developers can browse the compiler in the inspector, matching the Smalltalk philosophy of a transparent, modifiable live system
- Stepping stone to self-hosting — a Beamtalk compiler written in Erlang is closer to a Beamtalk compiler written in Beamtalk
- LFE proves the model works at production quality

**Not chosen because:**
- ~20k lines of working Rust compiler already exist (lexer, parser, semantic analysis, codegen) — rewriting is months of effort with zero new features
- Rust's type system catches bugs at compile time (exhaustive pattern matching, ownership, no null) that would become runtime crashes in an Erlang compiler
- Rust is 10–50x faster for CPU-bound parsing/codegen — not critical today but matters for large projects and future incremental compilation
- The existing Rust test suite (429+ parser tests, 176 compiler tests, 654 stdlib assertions) validates the current implementation
- The `beamtalk_compiler` anti-corruption layer preserves the option: if we later rewrite the compiler in Beamtalk itself, it becomes another backend behind `beamtalk_compiler_backend` — the workspace never knows

## Consequences

### Positive
- **Windows support** — Port communication via stdin/stdout works natively on Windows (no Unix socket dependency)
- **Simpler deployment** — One OTP release, no separate daemon process
- **Lower latency** — Direct IPC (Port) vs JSON-RPC over socket
- **Eliminated failure modes** — No daemon unavailable, no stale sockets, no JSON parse errors, no protocol version mismatches
- **Simpler REPL code** — ~100 lines of IPC code replaced by ~10 lines of `beamtalk_compiler` calls
- **Aligns with Smalltalk philosophy** — Compiler lives in the environment (supervised, restartable)

### Negative
- **Build complexity** — CI must cross-compile the compiler binary for 6+ platform targets. Mitigated by: GitHub Actions matrix, same toolchain already used for the CLI binary
- **Coupled releases** — Compiler and runtime must be released together as part of a unified OTP application and versioned anti-corruption layer. This is already effectively true (they share Core Erlang format). With a daemon, the compiler binary can be upgraded independently without restarting the BEAM node; with the embedded compiler (Port or future NIF backend) we intentionally tie the compiler version to the OTP app, so a compiler upgrade in practice requires node restart (killing running actors), even though the Port backend could technically support independent upgrades
- **Serialization overhead (Port backend)** — Binary protocol over stdin/stdout adds ~2ms per call, negligible vs compilation time but non-zero
- **Port startup latency** — First compilation or recovery after crash takes ~50-100ms for port process spawn
- **Resource opacity** — Compiler memory (allocated in Rust heap via Port/NIF) is invisible to BEAM tooling (`observer`, `instrument`). Memory leaks in the compiler degrade the node gradually without clear attribution
- **Remote/cloud deployment** — Every node that compiles must have the platform-specific compiler binary. Cloud workspaces (Codespaces, etc.) need matching binaries in the container image
- **Debugging compiler bugs** — When the compiler crashes in a Port, you get the exit status but limited diagnostics. In a daemon, you can attach `rust-gdb`. Mitigated by: Rust's error handling, crash logs, and the ability to run the compiler binary standalone for reproduction

### Neutral
- **`beamtalk` CLI binary still exists** — For `beamtalk build` batch compilation, the CLI can either use the Port backend itself or delegate to a running BEAM node. The CLI remains useful for project management, package management, and tooling.
- **ADR 0003 unaffected** — Core Erlang remains the codegen target. Only the process boundary changes.
- **ADR 0009 enhanced** — The workspace/runtime split benefits from embedded compilation. The compiler backend lives in a new `beamtalk_compiler` OTP app (its own bounded context), as a **peer** of `beamtalk_runtime` — not above or below it. The workspace depends on both, while the compiler and runtime are independent bounded contexts. This preserves DDD boundaries and enables standalone compilation tools.
- **DDD Context Map** — The Published Language boundary (Core Erlang IR) between Compilation and Runtime contexts is preserved. The `beamtalk_compiler` app acts as an Anti-Corruption Layer: it exposes a clean Erlang API (`compile/2`, `compile_expression/3`) while hiding the backend implementation detail (Port, NIF, or external daemon). Error formatting lives in `beamtalk_workspace`, which combines compiler diagnostics with runtime context.

## Implementation

### Phase 0: Wire Check (S)
Prove the core assumption: the `beamtalk-core` Rust binary can be invoked as an OTP port, receive a Beamtalk expression on stdin, and return Core Erlang on stdout. Minimal viable slice — no backend dispatch, no REPL integration.

**Validation criteria:**
- Compile `beamtalk-core` as a standalone binary with stdin/stdout mode
- OTP port spawns the binary and sends `<<"1 + 2">>`
- Port returns `{ok, #{core_erlang := ...}}`
- Port crash (e.g., send invalid input) is caught by supervisor and restarted
- BEAM node survives a compiler crash

**Affected components:**
- Modified: `crates/beamtalk-core/` or new `crates/beamtalk-compiler-port/` (stdin/stdout binary mode)
- New: `runtime/apps/beamtalk_compiler/` (minimal app, one module, port supervision)
- Test: Manual verification in `erl` shell

### Phase 1: Port Backend + Anti-Corruption Layer (M)
Create `beamtalk_compiler` as a new OTP application with OTP Port as the primary backend.

**DDD Alignment:** The compiler is its own bounded context (Source Analysis + Semantic Analysis + Code Generation). It becomes a fourth OTP application in the umbrella — an **Anti-Corruption Layer** translating between the Compilation Context and the Live Programming Domain.

```text
beamtalk_workspace  (Live Programming Domain)
    ↓ depends on both
beamtalk_compiler   beamtalk_runtime    ← peers (independent bounded contexts)
(Compilation)       (Actor/Object System)
                        ↓ depends on
                    beamtalk_stdlib     (Standard Library Context)
```

`beamtalk_compiler` and `beamtalk_runtime` are **peers**, not layered. The compiler has no dependency on the runtime — it compiles `Source → Core Erlang` without needing actors, objects, or primitives. Error formatting lives in `beamtalk_workspace`, which depends on both and can combine compiler diagnostics with runtime context. This also enables standalone compilation tools (e.g., `beamtalk check`) that don't load the runtime.

The workspace asks the compiler to compile; it never knows *how* compilation happens (Port vs NIF vs daemon). This preserves the Published Language boundary (Core Erlang IR) from the DDD model.

**Affected components:**
- Modified: `crates/beamtalk-core/` or new `crates/beamtalk-compiler-port/` (stdin/stdout binary)
- New: `runtime/apps/beamtalk_compiler/` (OTP application — anti-corruption layer)
  - `beamtalk_compiler.erl` — public API (compile, compile_expression, diagnostics)
  - `beamtalk_compiler_backend.erl` — backend dispatch (port vs daemon, workspace-level config)
  - `beamtalk_compiler_port.erl` — OTP port supervisor and communication
- Modified: `Cargo.toml` (workspace member)
- Modified: `runtime/rebar.config` (new app dependency)

### Phase 2: REPL Integration (M)
Replace daemon IPC in `beamtalk_repl_eval.erl` with calls through `beamtalk_compiler_backend`.

**Affected components:**
- Modified: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` (replace daemon calls)
- Modified: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_state.erl` (remove daemon socket tracking)
- Modified: `crates/beamtalk-cli/src/commands/repl.rs` (remove daemon auto-start)

**Testing:**
- All existing E2E tests (`just test-e2e`) must pass with both Port and daemon backends
- All stdlib tests (`just test-stdlib`) must pass with the Port backend
- CI should run tests with `BEAMTALK_COMPILER=port` and `BEAMTALK_COMPILER=daemon` to verify identical behavior

### Phase 3: Build Integration (M)
Move `beamtalk build` to use the Port-based compiler (via an OTP release or escript).

**Affected components:**
- Modified: `crates/beamtalk-cli/src/beam_compiler.rs` (option to compile via embedded compiler backend/Port abstraction)
- Modified: `crates/beamtalk-cli/src/commands/build.rs`
- Deprecated: `crates/beamtalk-cli/src/commands/daemon/` (entire daemon module)

### Phase 4: Precompiled Binaries & Windows (L)
Set up CI cross-compilation matrix for the compiler port binary.

**Affected components:**
- New: `.github/workflows/compiler-binary-precompile.yml`
- Modified: `runtime/apps/beamtalk_compiler/` (platform-specific binary discovery)
- New: Windows CI testing

### Phase 5: Daemon Removal (S)
Remove daemon code after migration period.

**Affected components:**
- Removed: `crates/beamtalk-cli/src/commands/daemon/` (protocol, lifecycle, transport)
- Removed: Socket/lockfile management code
- Modified: CLI help text and documentation

### Phase 6 (Future): NIF Backend (M, optional)
If incremental analysis or keystroke-level compilation requires sub-millisecond overhead, add Rustler NIF as an alternative backend behind `beamtalk_compiler_backend`.

**Trigger:** Port's ~2ms overhead becomes measurable bottleneck in LSP/IDE workflows.

**Affected components:**
- New: `crates/beamtalk-compiler-nif/` (Rustler crate)
- Modified: `runtime/apps/beamtalk_compiler/beamtalk_compiler_backend.erl` (add `nif` dispatch)
- New: `rustler_precompiled` CI matrix

## Migration Path

### Compiler Backend Selection

During the transition (Phases 2–4), the compiler backend is selectable via environment variable or CLI flag:

```erlang
%% In beamtalk_compiler_backend.erl (part of beamtalk_compiler app)
%% Compiler-context setting — the workspace depends on beamtalk_compiler
%% but never knows whether compilation uses Port, NIF, or daemon.
compiler_backend() ->
    case os:getenv("BEAMTALK_COMPILER") of
        "daemon" -> daemon;
        "port"   -> port;
        "nif"    -> nif;     %% Phase 6 only
        false    ->
            %% Default changes over time:
            %% Phase 2: daemon (Port opt-in)
            %% Phase 3: port (daemon opt-in)
            application:get_env(beamtalk_compiler, backend, default_backend())
    end.
```

```bash
# Phase 2: Port available but daemon is default
BEAMTALK_COMPILER=port beamtalk repl         # opt-in to Port (workspace-wide)
BEAMTALK_COMPILER=port beamtalk build .      # same env var for build
beamtalk repl                                 # uses daemon (default)

# Phase 3: Port is default, daemon still available
beamtalk repl                                 # uses Port (default)
beamtalk build .                              # uses Port (default)
BEAMTALK_COMPILER=daemon beamtalk repl       # fallback to daemon
beamtalk workspace start --compiler=daemon   # workspace-level flag

# Phase 5: daemon removed
beamtalk repl                                 # Port only
BEAMTALK_COMPILER=daemon beamtalk repl       # warns: "daemon backend removed, using port"

# Phase 6 (future): NIF available as opt-in
BEAMTALK_COMPILER=nif beamtalk repl          # opt-in to NIF for low-latency
```

The setting lives at the **workspace** level (not per-REPL-session), so all compilation within a workspace uses the same backend. This allows:
- **Gradual rollout** — test Port in development before making it the default (daemon → Port), and later test NIF as an opt-in backend
- **Quick rollback** — if Port has issues on a platform, switch back to the daemon during migration; if NIF has issues, switch back to Port
- **CI comparison** — run tests with both backends to verify identical behavior

### For users
1. **Phase 1-2:** Daemon still works and is default. Set `BEAMTALK_COMPILER=port` to opt in.
2. **Phase 3:** Port becomes default. Set `BEAMTALK_COMPILER=daemon` or `--compiler=daemon` to fall back.
3. **Phase 5:** Daemon removed. Environment variable ignored with deprecation warning.
4. **Phase 6 (future):** NIF available as opt-in via `BEAMTALK_COMPILER=nif` for low-latency workflows.

### For the codebase
- New: `beamtalk_compiler_backend` module dispatches to Port or daemon based on configuration
- `beamtalk_repl_eval.erl` calls `beamtalk_compiler_backend` instead of daemon directly
- `beamtalk_repl_state` retains daemon socket path until Phase 5
- CLI `beamtalk daemon start/stop/status` commands deprecated at Phase 3, removed at Phase 5
- JSON-RPC protocol code (`protocol.rs`, `transport.rs`) removed at Phase 5

## Implementation Tracking

**Epic:** BT-543 — Epic: Embedded Compiler via OTP Port (ADR 0022)
**Progress:** 100% complete (8/8 issues done)

| Phase | Issue | Title | Size | Status |
|-------|-------|-------|------|--------|
| Baseline | BT-544 | Establish compilation latency baseline | S | ✅ Done |
| Phase 0 | BT-545 | Wire check — OTP Port invokes Rust compiler binary | S | ✅ Done |
| Phase 1 | BT-546 | beamtalk_compiler OTP app with Port backend | M | ✅ Done |
| Phase 2 | BT-547 | Replace daemon IPC in REPL with beamtalk_compiler | M | ✅ Done |
| Perf | BT-548 | Validate compilation latency improvement | S | ✅ Done |
| Phase 3 | BT-549 | Move beamtalk build to use Port-based compiler | M | ✅ Done |
| Phase 4 | BT-550 | Release CI: Linux distributable workflow | L | ✅ Done |
| Phase 5 | BT-551 | Remove daemon code | M | ✅ Done |
| Phase 6 | — | NIF backend (future, optional) | M | ⏳ Deferred |

## References
- Related ADRs: [ADR 0003](0003-core-erlang-vs-erlang-source.md) (Core Erlang target — unaffected), [ADR 0009](0009-otp-application-structure.md) (app structure — enhanced), [ADR 0004](0004-persistent-workspace-management.md) (workspace architecture)
- Rustler: https://github.com/rusterlium/rustler
- Rustler precompiled: https://hexdocs.pm/rustler_precompiled/precompilation_guide.html
- Gleam compiler architecture: https://github.com/gleam-lang/gleam
- Documentation: `docs/beamtalk-architecture.md` (compiler-runtime split section)
