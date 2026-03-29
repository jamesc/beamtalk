# ADR 0072: User Erlang Sources in Beamtalk Packages

## Status
Proposed (2026-03-29)

## Context

Beamtalk compiles to BEAM via Core Erlang. ADR 0055 established the `(Erlang module)` FFI protocol for calling into hand-written Erlang from Beamtalk classes, and ADR 0056 introduced the `native:` keyword for Actor subclasses backed by hand-written `gen_server` modules. Both ADRs assume the backing Erlang modules live in the Beamtalk runtime — they don't address how **user packages** author and ship native Erlang code.

The package system infrastructure now exists: `beamtalk.toml` manifests (ADR 0026), dependency resolution with lockfiles (ADR 0070), and module namespacing via `bt@{package}@{module}` (ADR 0031). The missing piece is support for native Erlang source files and hex dependencies in user packages.

### Motivating Example: Extracting HTTP from stdlib

The HTTP classes in stdlib are a concrete candidate for extraction into a standalone package:

**7 Beamtalk classes:**
- `HTTPClient` (Actor, FFI → `beamtalk_http`)
- `HTTPServer` (Actor, native → `beamtalk_http_server`)
- `HTTPRouter` (Sealed Value, FFI → `beamtalk_http_router`)
- `HTTPRouteBuilder` (Actor, pure Beamtalk)
- `HTTPRoute` (Sealed Value, pure Beamtalk)
- `HTTPRequest` (Sealed Value, pure Beamtalk)
- `HTTPResponse` (Sealed Value, pure Beamtalk)

**5 Erlang backing modules:**
- `beamtalk_http.erl` — uses `gun` for HTTP client
- `beamtalk_http_server.erl` — uses `cowboy` for HTTP server
- `beamtalk_http_server_handler.erl` — cowboy handler bridge
- `beamtalk_http_router.erl` — pure Erlang route matching
- `beamtalk_http_response.erl` — type-only module (may be removed per ADR 0055 BT-1155 migration)

**2 hex dependencies:** `gun 2.1.0`, `cowboy 2.12.0` (+ transitive `ranch`)

Today, these Erlang modules live in `runtime/apps/beamtalk_stdlib/src/` and are compiled by rebar3 as part of the runtime build. There is no mechanism for a user package to include `.erl` files or declare hex dependencies.

### Constraints

- **BEAM ecosystem convention:** All BEAM languages ship source on hex, never `.beam` files. Consumers compile locally.
- **Build tool ownership:** `beamtalk build` must handle `.erl` compilation — users shouldn't need to run a separate tool.
- **rebar3 is already a dependency** for building the Beamtalk runtime. It is not a new requirement.
- **Hex deps are complex:** Many hex packages use rebar plugins, have C NIFs, or compile hooks. Reliably compiling them requires rebar3 or mix — reimplementing this is not viable.
- **Compilation order matters:** Hex deps must be compiled before native `.erl` files (which may depend on them), and native `.erl` files must be compiled before `.bt` files (which reference them via FFI or `native:`).

## Decision

### 1. Project Layout

Native Erlang source files live in a `native/` directory, separate from `src/`:

```
packages/http/
  beamtalk.toml
  src/                              # Beamtalk sources
    HTTPClient.bt
    HTTPServer.bt
    HTTPRouter.bt
    HTTPRouteBuilder.bt
    HTTPRoute.bt
    HTTPRequest.bt
    HTTPResponse.bt
  native/                           # Erlang sources
    beamtalk_http.erl
    beamtalk_http_server.erl
    beamtalk_http_server_handler.erl
    beamtalk_http_router.erl
    beamtalk_http_response.erl
  native/include/                   # Erlang headers (optional)
    beamtalk_http.hrl
  test/
    HTTPClientTest.bt
  _build/
    dev/
      ebin/                         # .beam output (bt + erl)
      native_deps/                  # hex dep artifacts
```

Rationale for `native/` over `src/` (Gleam's approach) or Elixir's overloaded `src/`:
- Unambiguous — clear what language each directory contains
- Consistent with `native:` keyword from ADR 0056
- Avoids filename collision (e.g., `http.bt` vs `http.erl` both in `src/`)

### 2. Manifest Format

The `[native.dependencies]` section in `beamtalk.toml` declares hex dependencies:

```toml
[package]
name = "http"
version = "0.1.0"
description = "HTTP client and server for Beamtalk"

[dependencies]
# Beamtalk package dependencies (none for http)

[native.dependencies]
# Hex dependencies for native Erlang code
gun = "~> 2.1"
cowboy = "~> 2.12"
```

**Version constraint syntax** follows hex.pm conventions:
- `"~> 2.1"` — `>= 2.1.0 and < 3.0.0`
- `"~> 2.1.0"` — `>= 2.1.0 and < 2.2.0`
- `">= 1.0.0 and < 2.0.0"` — explicit range
- `"2.12.0"` — exact version

Naming: `[native.dependencies]` rather than `[erlang.dependencies]` because:
- Consistent with the `native/` directory name
- Consistent with the `native:` keyword (ADR 0056)
- Language-agnostic if we ever support other BEAM languages in `native/`

### 3. Build Pipeline

`beamtalk build` extends to handle three compilation phases:

```
Phase 1: Hex deps (if [native.dependencies] present)
  → Generate _build/dev/rebar.config from beamtalk.toml
  → Run rebar3 compile in _build/dev/
  → Output: _build/dev/native_deps/lib/{dep}/ebin/*.beam

Phase 2: Native .erl files (if native/ directory exists)
  → Compile via extended build worker port (new compile:file/2 support)
  → Include paths: native/include/, hex dep ebin dirs
  → Output: _build/dev/ebin/*.beam

Phase 3: Beamtalk .bt files (existing pipeline)
  → Compile .bt → .core → .beam
  → Code path includes Phase 1 + Phase 2 output
  → Output: _build/dev/ebin/bt@{package}@*.beam
```

**Phase 1 detail — rebar3 integration:**

When `[native.dependencies]` is present, `beamtalk build` generates a minimal `rebar.config`:

```erlang
%% Auto-generated by beamtalk build — do not edit
{deps, [
    {gun, "2.1.0"},
    {cowboy, "2.12.0"}
]}.
{erl_opts, [debug_info]}.
```

Then runs `rebar3 compile` in `_build/dev/`. The generated `rebar.config` and `rebar.lock` live in `_build/dev/` — they are build artifacts, not source files.

**However:** The hex lock information is recorded in `beamtalk.lock` alongside Beamtalk package locks (ADR 0070). On first build (no lockfile), rebar3 resolves version constraints and the selected versions are captured in `beamtalk.lock`. On subsequent builds, `beamtalk build` generates `rebar.config` with exact versions from `beamtalk.lock` to ensure reproducible builds. `beamtalk deps update` re-resolves constraints and updates the lock.

**Phase 2 detail — native .erl compilation:**

For packages **without** hex deps, `.erl` files are compiled through the build worker port (ADR 0022), extended with `compile:file/2` support. The worker already runs a BEAM node with the `compiler` application loaded; the extension adds a new input type alongside the existing Core Erlang path:

```erlang
compile:file("native/beamtalk_http_router.erl",
  [report_errors, report_warnings, debug_info,
   {outdir, "_build/dev/ebin/"},
   {i, "native/include/"}]).
```

For packages **with** hex deps, the include and code paths are extended:

```erlang
compile:file("native/beamtalk_http.erl",
  [report_errors, report_warnings, debug_info,
   {outdir, "_build/dev/ebin/"},
   {i, "native/include/"},
   {i, "_build/dev/native_deps/lib/gun/include/"},
   {i, "_build/dev/native_deps/lib/cowboy/include/"}]).
```

**When rebar3 is not needed:**

If a package has `native/` but no `[native.dependencies]`, the build worker compiles `.erl` files directly. No rebar3 invocation. This is the fast path for packages that just need Erlang glue code without external dependencies.

### 4. Native Module Naming

Native Erlang modules keep their authored names — they are **not** namespaced with `bt@` prefixes. The `bt@{package}@{module}` convention applies only to Beamtalk-generated modules.

Rationale:
- Erlang modules referenced by `native:` and `(Erlang module)` use their real names
- Hex deps expect to call modules by their real names
- Adding prefixes would break interop with the broader BEAM ecosystem

**Collision prevention:** Package authors are responsible for choosing non-colliding Erlang module names. Convention: prefix with the package name (e.g., `beamtalk_http_*` for the `http` package). The build tool emits a warning if two packages in the dependency graph define the same native module name.

### 5. Code Path Management

At runtime, `beamtalk` ensures all necessary ebin directories are on the code path:

```
-pa _build/dev/ebin/                                    # package .beam files (bt + erl)
-pa _build/dev/native_deps/lib/gun/ebin/                # hex dep: gun
-pa _build/dev/native_deps/lib/cowboy/ebin/              # hex dep: cowboy
-pa _build/dev/native_deps/lib/ranch/ebin/               # hex dep: ranch (transitive)
-pa {beamtalk_runtime_ebin}                              # core runtime
```

The `.app` file for the package includes hex deps in its `applications` list:

```erlang
{application, http, [
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, gun, cowboy, beamtalk_runtime]},
    {env, [{classes, [...]},
           {native_modules, [beamtalk_http, beamtalk_http_server,
                             beamtalk_http_server_handler,
                             beamtalk_http_router,
                             beamtalk_http_response]}]}
]}.
```

### 6. Package Distribution

Packages ship source, following the universal BEAM convention:

```
http-0.1.0.tar.gz (hex tarball)
  ├── beamtalk.toml
  ├── src/
  │   ├── HTTPClient.bt
  │   ├── HTTPServer.bt
  │   └── ...
  ├── native/
  │   ├── beamtalk_http.erl
  │   ├── beamtalk_http_server.erl
  │   └── ...
  ├── native/include/     (if present)
  └── README.md, LICENSE
```

Consumers compile everything locally via `beamtalk build`. If the package declares `[native.dependencies]`, the consumer's build invokes rebar3 to fetch and compile hex deps.

### 7. REPL and Hot-Loading

Native Erlang modules participate in workspace hot-loading:

- `beamtalk build` detects changes to `native/*.erl` files (mtime comparison)
- Changed `.erl` files are recompiled and hot-loaded via `code:load_file/1`
- The REPL's file watcher includes `native/` alongside `src/`

### 8. Implicit Runtime Dependency

All native Erlang modules in a Beamtalk package have an implicit dependency on `beamtalk_runtime`. They may call:
- `beamtalk_error` — structured error creation
- `beamtalk_actor` — `sync_send/3` for actor dispatch
- `beamtalk_object_ops` — object protocol operations
- `beamtalk_result` — `ok/1`, `error/1` result wrapping

This dependency does not need to be declared in `beamtalk.toml` — the build tool ensures the runtime is on the code path.

## Prior Art

### Gleam
- `.erl` FFI files live in `src/` alongside `.gleam` files (mixed directory)
- `@external(erlang, "module_name", "function_name")` for FFI linkage
- Hex deps declared in `gleam.toml` `[dependencies]` (mixed with Gleam deps)
- `gleam build` compiles `.erl` via internal `erlc` invocation
- Ships source (`.gleam` + `.erl`) on hex, consumers compile locally
- **Adopted:** Ship source convention, build tool owns compilation
- **Rejected:** Mixed directory (Beamtalk uses separate `native/`), mixed dep sections

### Elixir/Mix
- `.erl` files in `src/`, `.ex` files in `lib/` — separate directories by language
- Compilation order: Erlang first, then Elixir
- `mix compile.erlang` handles `.erl` compilation with `:erlc_paths`, `:erlc_options`
- Hex deps in `deps/0` in `mix.exs`
- Ships source on hex
- **Adopted:** Separate directories for host and native language, compilation ordering
- **Adapted:** `native/` instead of `src/` (clearer naming)

### rebar3
- `src/` for `.erl`, `include/` for `.hrl`
- `rebar.config` for deps, `rebar.lock` for pinning
- Handles transitive deps, C NIFs, port drivers, rebar plugins
- **Adopted:** Use rebar3 for hex dep compilation (don't reimplement)
- **Adapted:** Generated `rebar.config` from `beamtalk.toml` rather than user-authored

### LFE (Lisp Flavoured Erlang)
- Uses rebar3 via `rebar3_lfe` plugin
- `.lfe` and `.erl` coexist in `src/`
- **Observed:** Even LFE, which is very close to Erlang, delegates to rebar3 for the ecosystem

## User Impact

### Newcomer (from Python/JS/Ruby)
Most users will never write native Erlang — they'll use packages that contain it. The `[native.dependencies]` section in `beamtalk.toml` is the only visible surface. `beamtalk build` handles everything transparently.

### Smalltalk Developer
Beamtalk's approach mirrors Smalltalk's FFI tradition: the language provides a clean boundary to call into native code. The `native/` directory is analogous to a Smalltalk VM plugin — powerful but separate.

### Erlang/BEAM Developer
This is the primary audience. They can write idiomatic Erlang in `native/`, use familiar hex packages, and bridge into Beamtalk via the `native:` protocol (ADR 0056) and `(Erlang module)` FFI (ADR 0055). The generated `rebar.config` means their hex deps work exactly as expected.

### Production Operator
Native modules are standard BEAM modules — fully observable with `observer`, `recon`, `dbg`. Hot code loading works via standard `code:load_file/1`. The `.app` file declares all dependencies for proper OTP application boot ordering.

## Steelman Analysis

### Alternative: Require rebar3 for all packages with native/ (no build worker path)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One tool for native compilation means fewer edge cases and bugs" |
| **Erlang veteran** | "rebar3 is the standard — using anything else for .erl compilation is surprising" |
| **Operator** | "rebar3's compilation is battle-tested; the build worker path is new and unproven" |
| **Language designer** | "Simpler implementation — one code path instead of two" |

### Alternative: Mixed src/ directory (Gleam model)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One directory is simpler — I don't have to learn where files go" |
| **Gleam developer** | "This is how Gleam does it and it works fine" |
| **Language designer** | "Fewer concepts — a file's extension tells you what it is" |

### Tension Points
- Erlang veterans prefer rebar3 for everything; Beamtalk wants to minimize external tool dependencies for simple cases
- Gleam developers expect mixed `src/`; Elixir developers expect separate directories
- The two-path approach (build worker vs rebar3) adds complexity but removes a hard dependency for the common case

## Alternatives Considered

### A: Always Require rebar3

Every package with `native/` generates a `rebar.config` and compiles via rebar3, even without hex deps.

Rejected because:
- Adds ~2-3 seconds to build for a simple `.erl` file that `compile:file/2` handles in milliseconds
- Makes rebar3 a hard dependency for all Beamtalk developers who use any native Erlang
- The build worker already has the Erlang compiler loaded — using it is natural

### B: Mixed src/ Directory (Gleam Model)

Place `.erl` files alongside `.bt` files in `src/`.

Rejected because:
- Naming collision risk (e.g., `http.bt` and `http.erl`)
- Harder to apply different build rules per language
- Less clear project structure — `native/` is self-documenting

### C: Embed hex_core for Dependency Resolution

Use the `hex_core` Erlang library directly instead of rebar3 for fetching and compiling hex deps.

Rejected because:
- Would need to handle transitive deps, C NIFs, rebar plugins, compile hooks
- Essentially reimplementing rebar3 — high effort, bug-prone
- rebar3 is already present in the Beamtalk development environment

## Consequences

### Positive
- Enables extracting HTTP, WebSocket, and other stdlib classes into standalone packages
- Package authors can use the entire hex.pm ecosystem
- Clean separation: `src/` for Beamtalk, `native/` for Erlang
- Fast path (no rebar3) for packages with simple Erlang glue code
- Follows universal BEAM convention of shipping source

### Negative
- Two compilation paths for `.erl` files (build worker vs rebar3) adds complexity
- rebar3 becomes a runtime dependency for consumers of packages with hex deps
- Native module naming collisions are possible (mitigated by convention + warnings)
- Generated `rebar.config` is another artifact to manage in `_build/`

### Neutral
- Does not affect packages without native Erlang code (majority of packages)
- Lock file format extends but does not change (hex dep pins added to `beamtalk.lock`)
- No changes to Beamtalk language syntax — this is purely build tooling

## Implementation

### Phase 1: Native Directory Support
- Extend `build.rs` to discover `native/*.erl` files
- Extend build worker to compile `.erl` files (not just `.core`)
- Add `native_modules` to `.app` file generation
- Incremental rebuild: mtime tracking for `.erl` files

### Phase 2: Hex Dependency Support
- Extend `manifest.rs` to parse `[native.dependencies]`
- Generate `rebar.config` from manifest
- Invoke rebar3 for hex dep compilation
- Wire hex dep ebin paths into code path
- Extend `beamtalk.lock` with hex dep pins

### Phase 3: HTTP Package Extraction
- Create `packages/http/` with `beamtalk.toml`
- Move 7 `.bt` classes from `stdlib/src/` to `packages/http/src/`
- Move 5 `.erl` modules from `runtime/apps/beamtalk_stdlib/src/` to `packages/http/native/`
- Remove `gun` and `cowboy` from runtime `rebar.config`
- Update stdlib tests

### Phase 4: REPL Hot-Loading
- Extend file watcher to include `native/` directory
- Hot-load recompiled `.erl` modules via `code:load_file/1`

### Affected Components
- `crates/beamtalk-cli/src/commands/build.rs` — build orchestration
- `crates/beamtalk-cli/src/commands/manifest.rs` — `[native.dependencies]` parsing
- `crates/beamtalk-cli/src/commands/deps/` — hex dep lock integration
- `runtime/apps/beamtalk_compiler/src/beamtalk_build_worker.erl` — `.erl` compilation support
- `crates/beamtalk-cli/src/beam_compiler.rs` — code path management

## Migration Path

### For stdlib HTTP extraction (Phase 3)

1. Create `packages/http/` with manifest and sources
2. Add `http = { path = "packages/http" }` to stdlib's dev dependencies for testing
3. Remove HTTP classes from `stdlib/src/` and backing modules from `runtime/apps/beamtalk_stdlib/src/`
4. Remove `gun` and `cowboy` from `runtime/rebar.config`
5. Users who depend on HTTP classes add `http` to their `[dependencies]`

### For existing projects

No migration needed — this is additive. Projects without `native/` or `[native.dependencies]` are unaffected.

## References
- Related issues: BT-1153
- Related ADRs: ADR 0026 (Project Manifest), ADR 0031 (Flat Namespace), ADR 0055 (Erlang-Backed Class Authoring), ADR 0056 (Native Erlang-Backed Actors), ADR 0070 (Package Namespaces and Dependencies)
- Prior art: [Gleam FFI](https://gleam.run/writing-gleam/gleam-toml/), [Mix.Tasks.Compile.Erlang](https://hexdocs.pm/mix/Mix.Tasks.Compile.Erlang.html), [rebar3 dependencies](https://rebar3.org/docs/configuration/dependencies/)
