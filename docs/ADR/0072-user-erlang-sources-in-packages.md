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
- **rebar3 is a single-file escript** (~1.8MB) — it can be bundled with the Beamtalk compiler, eliminating it as an external dependency. Gleam takes this approach, embedding rebar3 in its compiler binary.
- **Hex deps are complex:** Many hex packages use rebar plugins, have C NIFs, or compile hooks. Reliably compiling them requires rebar3 or mix — reimplementing this is not viable.
- **Compilation order matters:** Hex deps must be compiled before native `.erl` files (which may depend on them), and native `.erl` files must be compiled before `.bt` files (which reference them via FFI or `native:`).
- **BEAM has a flat module namespace:** Only one version of any given module can be loaded at a time. Multiple packages declaring the same hex dep must resolve to a single version.

## Decision

### 1. Project Layout

Native Erlang source files live in a `native/` directory, separate from `src/`:

```text
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
  native/test/                      # Erlang tests (EUnit/Common Test)
    beamtalk_http_router_tests.erl
  test/                             # Beamtalk tests (BUnit)
    HTTPClientTest.bt
  _build/
    dev/
      ebin/                         # .beam output (bt)
      native/                       # rebar3 output (erl + hex deps)
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

### 3. Bundled rebar3

Beamtalk bundles a pinned copy of rebar3 (a single-file escript, ~1.8MB) with the compiler distribution. This eliminates rebar3 as an external dependency — users only need Erlang/OTP itself.

```text
runtime/tools/rebar3     # vendled escript, pinned version
```

The build tool locates it automatically:

```rust
fn rebar3_path() -> PathBuf {
    // Bundled copy next to runtime
    let bundled = runtime_dir().join("tools/rebar3");
    if bundled.exists() { return bundled; }
    // Fall back to system rebar3
    which::which("rebar3").unwrap_or_else(|_| panic!("rebar3 not found"))
}
```

Gleam takes the same approach — it embeds rebar3 in its compiler binary and extracts it to a cache directory on first use.

### 4. Build Pipeline

Since rebar3 is always available (bundled), all `native/` compilation uses a single rebar3 path — no split between "simple" and "with hex deps" cases. `beamtalk build` extends to handle two compilation phases:

```text
Phase 1: Native Erlang (if native/ directory exists)
  → Generate _build/dev/native/rebar.config from beamtalk.toml
  → Run bundled rebar3 compile in _build/dev/native/
  → Output: _build/dev/native/default/lib/{app}/ebin/*.beam

Phase 2: Beamtalk .bt files (existing pipeline)
  → Compile .bt → .core → .beam
  → Code path includes Phase 1 output
  → Output: _build/dev/ebin/bt@{package}@*.beam
```

**Phase 1 detail — rebar3 integration:**

`beamtalk build` generates a `rebar.config` that compiles both hex deps and the package's own `.erl` files in a single rebar3 invocation:

```erlang
%% Auto-generated by beamtalk build — do not edit
{deps, [
    {gun, "~> 2.1"},
    {cowboy, "~> 2.12"}
]}.
{src_dirs, ["native"]}.           %% points to project's native/ dir
{include_dirs, ["native/include"]}.
{erl_opts, [debug_info]}.
```

For packages without hex deps, `{deps, []}` is empty but rebar3 still compiles the `.erl` files — one code path for all cases.

Packages may also declare `[native.dependencies]` **without** a `native/` directory — for example, to call a hex package directly via `(Erlang module)` FFI without any hand-written Erlang glue. In this case the generated `rebar.config` has deps but no `{src_dirs, [...]}`. rebar3 fetches and compiles the hex dep; `beamtalk build` puts its ebin on the code path.

Then runs `rebar3 compile` in `_build/dev/native/` (with `REBAR_BASE_DIR=.`). The generated `rebar.config` and `rebar.lock` live in `_build/dev/native/` — they are build artifacts, not source files.

rebar3 handles version resolution against hex.pm, transitive deps, include paths, code paths, compilation order, and incremental rebuilds automatically.

**Error handling:** rebar3 errors are surfaced through `beamtalk build`. Common error patterns (version conflicts, missing packages) are translated into Beamtalk-style diagnostics with context about which packages contributed the conflicting constraints. Unrecognized errors are passed through verbatim with a note indicating they originate from rebar3. Authors of native Erlang code should expect to debug Erlang build issues directly when they arise.

**On rebuild from lockfile:** When `beamtalk.lock` already contains resolved versions from a previous build, `beamtalk build` generates `rebar.config` with exact pinned versions instead of constraints, ensuring reproducible builds without re-resolving against hex.pm.

### 5. Native Dependency Resolution

Libraries declare version **constraints** in `[native.dependencies]`. Beamtalk **collects and merges** constraints across the dependency graph; **rebar3 resolves** them against hex.pm. Only the root application triggers resolution — matching how hex.pm, rebar3, and Mix all work.

**Problem:** BEAM has a flat module namespace. If package `http` depends on `gun ~> 2.1` and package `websocket` depends on `gun ~> 2.0`, they must resolve to a single version at runtime. Per-package isolation would silently load conflicting versions.

**Solution: Top-level constraint aggregation.**

`beamtalk build` walks the entire Beamtalk dependency graph, collects all `[native.dependencies]` constraints from every transitive package, and merges them into a single `rebar.config`. The **constraints** (not resolved versions) are passed through to rebar3:

```erlang
%% Merged from: http (gun ~> 2.1, cowboy ~> 2.12), websocket (gun ~> 2.0, cowboy ~> 2.12)
{deps, [
    {gun, "~> 2.1"},       %% tightest constraint that satisfies both packages
    {cowboy, "~> 2.12"}    %% identical constraint from both packages
]}.
```

When two packages declare the same hex dep, Beamtalk takes the **tighter constraint** (the one that is a subset of the other). If constraints don't overlap, Beamtalk emits an error before invoking rebar3.

**Resolution flow:**

1. `beamtalk build` collects `[native.dependencies]` from all packages in the dependency graph
2. Merges constraints — for each shared hex dep, computes the tightest compatible constraint
3. Detects **direct** constraint conflicts early (non-overlapping constraints) with helpful error messages
4. On first build (no lockfile): passes merged constraints to rebar3, which resolves actual versions against hex.pm — including **transitive** hex deps that Beamtalk has no visibility into. Transitive conflicts are caught and reported by rebar3. Resolved versions are captured in `beamtalk.lock`
5. On subsequent builds: generates `rebar.config` with exact pinned versions from `beamtalk.lock`
6. `beamtalk deps update` re-resolves constraints and updates the lock

**Conflict detection** (Beamtalk, before invoking rebar3):

```text
error: incompatible native dependency constraints
  package 'http' requires gun ~> 2.1
  package 'legacy_client' requires gun ~> 1.3
  = help: these constraints have no overlap (>= 2.1.0 vs < 2.0.0)
  = help: update 'legacy_client' to a version compatible with gun ~> 2.x
```

**Lock file integration:**

Native dep pins are stored in `beamtalk.lock` alongside Beamtalk package locks (ADR 0070):

```toml
# Beamtalk package locks
[[package]]
name = "http"
url = "https://github.com/jamesc/beamtalk-http"
reference = "tag:v0.1.0"
sha = "abc123..."

# Native (hex) dependency locks
[[native_package]]
name = "gun"
version = "2.1.3"
sha = "def456..."

[[native_package]]
name = "cowboy"
version = "2.12.0"
sha = "ghi789..."
```

### 6. Native Module Naming

Native Erlang modules keep their authored names — they are **not** namespaced with `bt@` prefixes. The `bt@{package}@{module}` convention applies only to Beamtalk-generated modules.

Rationale:
- Erlang modules referenced by `native:` and `(Erlang module)` use their real names
- Hex deps expect to call modules by their real names
- Adding prefixes would break interop with the broader BEAM ecosystem

**Collision prevention:** Package authors are responsible for choosing non-colliding Erlang module names. Convention: prefix with the package name (e.g., `beamtalk_http_*` for the `http` package). The build tool emits a warning if two packages in the dependency graph define the same native module name.

### 7. Code Path Management

At runtime, `beamtalk` ensures all necessary ebin directories are on the code path:

```bash
-pa _build/dev/ebin/                                    # Beamtalk .beam files
-pa _build/dev/native/default/lib/{pkg}/ebin/            # package native .beam files
-pa _build/dev/native/default/lib/gun/ebin/              # hex dep: gun
-pa _build/dev/native/default/lib/cowboy/ebin/           # hex dep: cowboy
-pa _build/dev/native/default/lib/ranch/ebin/            # hex dep: ranch (transitive)
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

### 8. Package Distribution

Packages ship source, following the universal BEAM convention:

```text
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

### 9. REPL and Hot-Loading

Native Erlang modules participate in workspace hot-loading:

- `beamtalk build` detects changes to `native/*.erl` files (mtime comparison)
- Changed `.erl` files are recompiled and hot-loaded via `code:load_file/1`
- The REPL's file watcher includes `native/` alongside `src/`

### 10. Testing Native Code

Native Erlang tests live in `native/test/` and are run by rebar3 alongside Beamtalk tests:

```text
beamtalk test
  → rebar3 eunit (native/test/*_tests.erl)
  → rebar3 ct    (native/test/*_SUITE.erl, if present)
  → BUnit        (test/*.bt)
```

Standard Erlang test conventions apply — EUnit modules named `*_tests.erl`, Common Test suites named `*_SUITE.erl`. The generated `rebar.config` includes:

```erlang
{eunit_dirs, ["native/test"]}.
{ct_dirs, ["native/test"]}.
```

This gives package authors full Erlang testing tools for their native modules, while BUnit tests exercise the Beamtalk-facing API.

### 11. Implicit Runtime Dependency

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
- **Bundles rebar3** in the compiler binary — extracts to cache on first use
- Ships source (`.gleam` + `.erl`) on hex, consumers compile locally
- **Adopted:** Ship source convention, build tool owns compilation, bundled rebar3
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

### Alternative: Split compilation path (build worker for simple .erl, rebar3 only for hex deps)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Simple packages build faster without rebar3 overhead" |
| **Language designer** | "The build worker already has `compile` loaded — adding `compile:file/2` is minimal work" |
| **Operator** | "Fewer moving parts for packages that don't need hex deps" |

### Alternative: Mixed src/ directory (Gleam model)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One directory is simpler — I don't have to learn where files go" |
| **Gleam developer** | "This is how Gleam does it and it works fine" |
| **Language designer** | "Fewer concepts — a file's extension tells you what it is" |

### Alternative: Per-package native dep isolation (each package resolves independently)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Each package works in isolation — no surprising cross-package interactions" |
| **Language designer** | "Simpler implementation — no need to merge constraints across the graph" |

### Tension Points
- The split compilation path is marginally faster for simple cases but adds implementation complexity and a second code path to maintain
- Gleam developers expect mixed `src/`; Elixir developers expect separate directories
- Per-package isolation is simpler but fundamentally broken on BEAM due to the flat module namespace

## Alternatives Considered

### A: Split Compilation Path (build worker for simple .erl, rebar3 only for hex deps)

Use the build worker's `compile:file/2` for packages without hex deps, and rebar3 only when `[native.dependencies]` is present.

Rejected because:
- Two code paths to implement, test, and maintain
- Behavioural differences between the paths (include handling, incremental rebuilds) would surface as subtle bugs
- Bundling rebar3 eliminates the external dependency concern that motivated the split
- rebar3 handles include paths, code paths, and incremental rebuilds for free

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
- Bundling rebar3 is simpler and more reliable

### D: Per-Package Native Dep Isolation

Each package runs its own rebar3 resolution independently, with separate `_build` directories.

Rejected because:
- BEAM has a flat module namespace — two versions of `gun` cannot coexist at runtime
- The last version loaded silently wins, causing hard-to-debug failures
- Every other BEAM build tool (Mix, rebar3, Gleam) does top-level resolution for this reason

## Consequences

### Positive
- Enables extracting HTTP, WebSocket, and other stdlib classes into standalone packages
- Package authors can use the entire hex.pm ecosystem
- Clean separation: `src/` for Beamtalk, `native/` for Erlang
- Single compilation path — bundled rebar3 handles all `.erl` compilation uniformly
- Bundled rebar3 means zero additional setup for users
- Top-level native dep resolution prevents version conflicts at runtime
- Follows universal BEAM convention of shipping source

### Negative
- Bundled rebar3 adds ~1.8MB to the Beamtalk distribution
- Native module naming collisions are possible (mitigated by convention + warnings)
- Generated `rebar.config` is another artifact to manage in `_build/`
- Cross-package native dep constraint merging adds complexity to the dependency resolver

### Neutral
- Does not affect packages without native Erlang code (majority of packages)
- Lock file format extends but does not change (hex dep pins added to `beamtalk.lock`)
- No changes to Beamtalk language syntax — this is purely build tooling

## Implementation

### Phase 1: Bundle rebar3 and Native Directory Support
- Vendor rebar3 escript into `runtime/tools/rebar3`
- Extend `build.rs` to discover `native/*.erl` files
- Generate `rebar.config` from manifest and invoke bundled rebar3
- Add `native_modules` to `.app` file generation
- Wire rebar3 output ebin paths into runtime code path

### Phase 2: Native Dependency Resolution
- Extend `manifest.rs` to parse `[native.dependencies]`
- Implement cross-package constraint collection and merging in `deps/`
- Extend `beamtalk.lock` with `[[native_package]]` entries
- Add conflict detection with helpful error messages
- `beamtalk deps update` re-resolves native constraints

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
- `runtime/tools/rebar3` — bundled rebar3 escript (new)
- `crates/beamtalk-cli/src/commands/build.rs` — build orchestration, rebar3 invocation
- `crates/beamtalk-cli/src/commands/manifest.rs` — `[native.dependencies]` parsing
- `crates/beamtalk-cli/src/commands/deps/` — native dep constraint merging, lock integration
- `crates/beamtalk-cli/src/beam_compiler.rs` — code path management

## Migration Path

### For stdlib HTTP extraction (Phase 3)

1. Create `packages/http/` with manifest, `.bt` classes, `native/` Erlang modules, and `native/test/` tests
2. Remove HTTP classes from `stdlib/src/` and backing modules from `runtime/apps/beamtalk_stdlib/src/`
3. Remove `gun` and `cowboy` from `runtime/rebar.config`
4. Users who depend on HTTP classes add `http` to their `[dependencies]`

### For existing projects

No migration needed — this is additive. Projects without `native/` or `[native.dependencies]` are unaffected.

## References
- Related issues: BT-1153
- Related ADRs: ADR 0026 (Project Manifest), ADR 0031 (Flat Namespace), ADR 0055 (Erlang-Backed Class Authoring), ADR 0056 (Native Erlang-Backed Actors), ADR 0070 (Package Namespaces and Dependencies)
- Prior art: [Gleam FFI](https://gleam.run/writing-gleam/gleam-toml/), [Mix.Tasks.Compile.Erlang](https://hexdocs.pm/mix/Mix.Tasks.Compile.Erlang.html), [rebar3 dependencies](https://rebar3.org/docs/configuration/dependencies/)
