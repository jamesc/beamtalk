# ADR 0072: User Erlang Sources in Beamtalk Packages

## Status
Accepted (2026-03-29)

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
      ebin/                         # .beam output (bt + native .erl)
      native/                       # rebar3 output (hex deps, or hex deps + .erl)
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
fn rebar3_path() -> miette::Result<PathBuf> {
    // Bundled copy next to runtime
    let bundled = runtime_dir().join("tools/rebar3");
    if bundled.exists() { return Ok(bundled); }
    // Fall back to system rebar3
    which::which("rebar3")
        .into_diagnostic()
        .wrap_err("rebar3 not found — expected bundled copy at {bundled}")
}
```

Gleam takes the same approach — it embeds rebar3 in its compiler binary and extracts it to a cache directory on first use.

**Update policy:** The bundled rebar3 is updated when a new rebar3 release adds needed features or fixes, when OTP compatibility requires it, or on security advisories. The pinned version and its minimum OTP requirement are documented in the release notes.

### 4. Build Pipeline

`beamtalk build` uses two compilation strategies for native Erlang, selected by the presence of `[native.dependencies]` in `beamtalk.toml`:

**Path A: No hex deps** — packages with `native/` but no `[native.dependencies]` compile `.erl` files directly via `compile:file/2` in the build worker. This is fast (~200ms for typical packages) and requires no external tooling. Gleam uses the same approach — a long-lived escript calling `compile:file/2` for its own packages' FFI files.

**Path B: With hex deps** — packages declaring `[native.dependencies]` use rebar3, which compiles both the hex deps and the package's own `.erl` files in a single invocation. rebar3 handles version resolution, transitive deps, include paths, and incremental rebuilds.

```text
Phase 1: Native Erlang (if native/ directory exists)
  Path A (no hex deps):
    → compile:file/2 for each native/*.erl
    → Output: _build/dev/native/ebin/*.beam

  Path B (with hex deps):
    → Generate _build/dev/native/rebar.config from beamtalk.toml
    → Run bundled rebar3 compile in _build/dev/native/
    → Output: _build/dev/native/default/lib/{app}/ebin/*.beam

Phase 2: Beamtalk .bt files (existing pipeline)
  → Compile .bt → .core → .beam
  → Code path includes Phase 1 output
  → Output: _build/dev/ebin/bt@{package}@*.beam
```

The decision point is static — determined by `beamtalk.toml` contents, not by heuristics.

**Path B detail — rebar3 integration:**

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

Packages may also declare `[native.dependencies]` **without** a `native/` directory — for example, to call a hex package directly via `(Erlang module)` FFI without any hand-written Erlang glue. In this case the generated `rebar.config` has deps but no `{src_dirs, [...]}`. rebar3 fetches and compiles the hex dep; `beamtalk build` puts its ebin on the code path.

rebar3 runs from the **project root** with `REBAR_BASE_DIR=_build/dev/native/` to direct output there. The generated `rebar.config` and `rebar.lock` live in `_build/dev/native/` — they are build artifacts, not source files. The `{src_dirs, ["native"]}` directive resolves relative to the project root where rebar3 is invoked.

rebar3 handles version resolution against hex.pm, transitive deps, include paths, code paths, compilation order, and incremental rebuilds automatically.

**Error handling:** rebar3 errors are surfaced through `beamtalk build`. Common error patterns (version conflicts, missing packages) are translated into Beamtalk-style diagnostics with context about which packages contributed the conflicting constraints. Unrecognized errors are passed through verbatim with a note indicating they originate from rebar3. Authors of native Erlang code should expect to debug Erlang build issues directly when they arise.

**On rebuild from lockfile:** When `beamtalk.lock` already contains resolved versions from a previous build, `beamtalk build` generates `rebar.config` with exact pinned versions instead of constraints, ensuring reproducible builds without re-resolving against hex.pm.

### 5. Native Dependency Resolution

Libraries declare version **constraints** in `[native.dependencies]`. **rebar3 resolves** them against hex.pm. Only the root application triggers resolution — matching how hex.pm, rebar3, and Mix all work.

**Problem:** BEAM has a flat module namespace. If package `http` depends on `gun ~> 2.1` and package `websocket` depends on `gun ~> 2.0`, they must resolve to a single version at runtime. Per-package isolation would silently load conflicting versions.

**Solution: Top-level constraint aggregation — let rebar3 solve it.**

`beamtalk build` walks the entire Beamtalk dependency graph, collects all `[native.dependencies]` constraints from every transitive package, and passes them all through to a single generated `rebar.config`. rebar3's resolver (backed by hex_core's constraint solver) handles the actual version resolution, including transitive hex deps:

```erlang
%% Aggregated from: http (gun ~> 2.1, cowboy ~> 2.12), websocket (gun ~> 2.0)
{deps, [
    {gun, "~> 2.1"},       %% from package 'http'
    {gun, "~> 2.0"},       %% from package 'websocket' — rebar3 resolves both
    {cowboy, "~> 2.12"}    %% from package 'http'
]}.
```

**Version constraint syntax** in `beamtalk.toml` follows hex.pm semantics (the `~>` operator uses the same interpretation as hex.pm and rebar3, which both delegate to hex_core):
- `~> 2.1` — `>= 2.1.0 and < 3.0.0`
- `~> 2.1.0` — `>= 2.1.0 and < 2.2.0`

Constraints are passed through to the generated `rebar.config` verbatim — no translation needed.

**Resolution flow:**

1. `beamtalk build` collects `[native.dependencies]` from all packages in the dependency graph
2. Aggregates all constraints into a single generated `rebar.config`
3. On first build (no lockfile): rebar3 resolves actual versions against hex.pm, handling constraint intersection, transitive deps, and conflict detection. All resolved versions are captured in `beamtalk.lock`
4. On subsequent builds: generates `rebar.config` with exact pinned versions from `beamtalk.lock`, ensuring reproducible builds including transitive deps
5. `beamtalk deps update` re-resolves constraints and updates the lock

rebar3's error messages for version conflicts are passed through to the user. If rebar3's error output proves insufficient for common cases, Beamtalk-level pre-checking can be added as a follow-up — but rebar3's solver is battle-tested and handles the general case correctly.

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

[[native_package]]
name = "ranch"       # transitive dep (via cowboy)
version = "2.1.0"
sha = "jkl012..."
```

All resolved versions — direct and transitive — are pinned. This ensures `rebar.lock` in `_build/` can be regenerated deterministically from `beamtalk.lock`.

### 6. Native Module Naming

Native Erlang modules keep their authored names — they are **not** namespaced with `bt@` prefixes. The `bt@{package}@{module}` convention applies only to Beamtalk-generated modules.

Rationale:
- Erlang modules referenced by `native:` and `(Erlang module)` use their real names
- Hex deps expect to call modules by their real names
- Adding prefixes would break interop with the broader BEAM ecosystem

**Collision prevention:** Package authors are responsible for choosing non-colliding Erlang module names. Convention: prefix with the package name (e.g., `beamtalk_http_*` for the `http` package). The build tool emits an **error** (not a warning) if two packages in the dependency graph define the same native module name — BEAM's flat namespace means only one version would load, causing silent breakage.

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

The `.app` file for the package lists **direct** hex deps in its `applications` list, following standard OTP convention. Transitive deps (e.g., `ranch` via `cowboy`) are covered by each dependency's own `.app` file and do not need to be repeated:

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

Native Erlang modules participate in workspace hot-loading via two mechanisms:

**Project load** (`:load_project` / MCP `load-project`): scans `native/` alongside `src/`, compiles all changed `.erl` files via `compile:file/2` before compiling `.bt` files. Incremental — uses mtime comparison, same as `.bt` files.

**Single file reload** (`:reload ClassName`): demand-driven — when the compiler encounters a `native:` annotation or `(Erlang module)` FFI reference, it checks whether the referenced `.erl` file in `native/` is newer than its `.beam` and recompiles it first via `compile:file/2`.

Both paths use `compile:file/2` directly within the running VM — no rebar3 invocation. Hex deps are compiled once at `beamtalk build` time and are already on the code path.

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
- **Split compilation path:** Gleam packages' own `.erl` files are compiled via a long-lived escript using `compile:file/2` with parallel workers — fast, no rebar3 overhead. Non-Gleam hex deps (rebar3 projects) use `rebar3 bare compile`
- **Bundles rebar3** in the compiler binary — extracts to cache on first use, but only invoked for non-Gleam hex deps
- Ships source (`.gleam` + `.erl`) on hex, consumers compile locally
- **Adopted:** Ship source convention, build tool owns compilation, bundled rebar3, split compilation (`compile:file/2` for own files, rebar3 for hex deps)
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

### Against the chosen design (split path + `native/` + rebar3)

| Cohort | Strongest argument |
|--------|-------------------|
| **BEAM developer** | "I already have rebar.config and mix.exs in my muscle memory. Now I have to learn a *third* manifest format (beamtalk.toml) that generates a rebar.config I can't edit? If something breaks, I'm debugging generated artifacts instead of my own config. Just let me write rebar.config." |
| **Operator** | "You're bundling a 1.8MB binary I didn't ask for, pinned to a version I don't control. When CVE-2027-XXXX drops for rebar3, I'm waiting on a Beamtalk release to patch it. Every other Erlang tool on my system uses the system rebar3 — now I have two versions to track." |
| **Language designer** | "Two compilation paths means two sets of include-path semantics, two incremental rebuild strategies, and two error message formats. The decision point (`[native.dependencies]` present or not) is static, but the *behavioural surface* isn't — users will hit differences when they add their first hex dep and their `.erl` compilation subtly changes." |

### Alternative: Mixed src/ directory (Gleam model)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One directory, one mental model. I put source files in `src/`. The extension tells me the language. I don't have to learn which directory gets which language — Python doesn't make me put C extensions in a separate folder." |
| **BEAM developer** | "Gleam ships to hex with mixed `.gleam` and `.erl` in `src/` and it works. The collision risk is theoretical — who names an Erlang module `HTTPClient.erl`? Extensions are unambiguous." |
| **Language designer** | "Separate directories create a conceptual wall between Beamtalk and Erlang code. But the whole point of FFI is that they're collaborating closely. A mixed directory signals that Erlang is a first-class citizen in your project, not a second-class afterthought filed away in `native/`." |
| **Operator** | "One directory to include in CI linting rules, one directory to watch for changes, one directory in my Dockerfile COPY. Fewer moving parts." |
| **Smalltalk developer** | "Smalltalk never separates primitives into a different browser. The image presents them inline. `native/` feels like the Java model of shoving JNI code somewhere else." |

### Alternative: Per-package native dep isolation (each package resolves independently)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "My package works in `beamtalk build`. I publish it. Someone else's package uses a different version of the same dep. Top-level aggregation means *my* working package now fails in *their* project because of a constraint I can't control. Per-package isolation means my package always builds the same way." |
| **Language designer** | "Constraint aggregation pushes resolution complexity into the Beamtalk build tool — it must understand hex version semantics, implement intersection logic, and produce useful error messages. Per-package isolation means each rebar3 invocation is self-contained. The BEAM flat namespace problem is real, but it's BEAM's problem — we could document it as a known limitation rather than engineering around it." |
| **BEAM developer** | "Elixir umbrella apps have per-app `mix.exs` with their own deps and it works fine. The flat namespace collision is rare in real codebases — most apps don't pull in conflicting versions of the same package." |

### Alternative: Embed hex_core directly (no rebar3 dependency)

| Cohort | Strongest argument |
|--------|-------------------|
| **Language designer** | "rebar3 is a 1.8MB escript with 15 years of accumulated complexity — plugins, profiles, hooks, overlays. We use maybe 5% of it. hex_core is ~500 lines and does exactly the one thing we need: resolve version constraints against hex.pm. By embedding hex_core and calling `erlc` directly, we own the entire compilation pipeline. Gleam already proved this works — their compiler only falls back to rebar3 for complex legacy packages." |
| **Operator** | "A vendored rebar3 is an opaque binary. hex_core is auditable Erlang source. When something breaks, I can read the code. I can also point hex_core at a private hex mirror without wondering whether rebar3's plugin system will interfere." |
| **BEAM developer** | "Every Erlang developer has `erlc` on their PATH. Using `compile:file/2` for `.erl` compilation and hex_core for package fetching gives us the thinnest possible dependency surface. If rebar3 ever makes a breaking change to its escript format or profile semantics, we're insulated." |
| **Newcomer** | "I installed Beamtalk. I don't want to learn what rebar3 is. With hex_core, there's no 'rebar3 error' in my build output — it's all Beamtalk." |

### Tension Points

- **Mixed vs separate directories** is a genuine values disagreement: "Erlang is a first-class collaborator" (mixed) vs "Erlang is a clearly-bounded escape hatch" (separate). The ADR chose the latter — Beamtalk's identity is Smalltalk-on-BEAM, not multi-language-on-BEAM.
- **rebar3 vs hex_core** is a build-vs-buy tradeoff. rebar3 handles NIF compilation, rebar plugins, and 15 years of edge cases. hex_core is clean but would require reimplementing those edge cases. The split compilation path mitigates this: `compile:file/2` for the common case, rebar3 only when hex deps bring in ecosystem complexity.
- **Per-package isolation** is appealing in theory but BEAM's flat module namespace makes it fundamentally unsound. The Elixir umbrella counterexample is instructive — umbrella apps *appear* to have per-app deps but Mix actually resolves them into a single lockfile at the umbrella root. Per-package isolation on BEAM is an illusion; top-level aggregation is the only correct approach.
- **The chosen design's operator cost** (bundled rebar3 updates, generated artifacts) is real but bounded — rebar3 is mature and releases infrequently. The split path limits rebar3 exposure to packages that opt into hex deps.

## Alternatives Considered

### ~~A: Split Compilation Path~~ → Adopted (see Section 4)

Use the build worker's `compile:file/2` for packages without hex deps, and rebar3 only when `[native.dependencies]` is present. This is the same split Gleam uses in production — `compile:file/2` via escript for own packages, rebar3 only for non-Gleam hex deps.

Initially considered rejected due to two-code-path maintenance concerns. Adopted after recognising that:
- The build worker already has `compile:file/2` infrastructure
- Gleam validates this exact split in production
- The REPL benefits significantly — `compile:file/2` is near-instant vs ~3s rebar3 startup overhead
- The decision point is static (`[native.dependencies]` present or not), not heuristic

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
- BEAM has a flat module namespace — only one version of any module can be loaded per VM. There is no classloader hierarchy (JVM/OSGi), no per-caller resolution (Node.js `node_modules`), and no link-time binding (Rust). Two versions of `gun` cannot coexist at runtime; the last loaded silently wins
- The Elixir umbrella counterexample is misleading — while each umbrella app has its own `mix.exs` declaring deps, Mix resolves all deps across the entire umbrella into a single `mix.lock` at the root. Umbrella apps work *because* Mix does top-level aggregation, not because per-app isolation is viable
- Every BEAM build tool (Mix, rebar3, Gleam) does top-level resolution for this reason — it's not a design preference, it's a platform constraint

## Consequences

### Positive
- Enables extracting HTTP, WebSocket, and other stdlib classes into standalone packages
- Package authors can use the entire hex.pm ecosystem
- Clean separation: `src/` for Beamtalk, `native/` for Erlang
- Split compilation path — `compile:file/2` for simple packages (fast), rebar3 for hex deps (comprehensive). Same pattern Gleam uses in production
- Bundled rebar3 means zero additional setup for users
- Top-level native dep resolution prevents version conflicts at runtime
- Follows universal BEAM convention of shipping source

### Negative
- Bundled rebar3 adds ~1.8MB to the Beamtalk distribution
- Native module naming collisions are possible (mitigated by convention + build error on collision)
- Generated `rebar.config` is another artifact to manage in `_build/`
- Two compilation paths to maintain (`compile:file/2` and rebar3), though the decision point is static
- Cold builds with hex deps are slow (30–120s for first rebar3 invocation with no hex cache) — significantly slower than pure Beamtalk packages
- Hex deps with rebar plugins, C NIFs, or custom compile hooks may require additional host tooling (`gcc`, `make`, `cmake`) not covered by Beamtalk's install — these are passed through to rebar3 as-is
- Third-party hex packages enter the workspace's trust boundary with full `erlang:apply/3` reachability — the same supply chain risk accepted by all BEAM ecosystems (see ADR 0058)

### Neutral
- Does not affect packages without native Erlang code (majority of packages)
- Lock file format extends but does not change (hex dep pins added to `beamtalk.lock`)
- No changes to Beamtalk language syntax — this is purely build tooling

## Implementation

### Phase 1: Native Directory Support (compile:file/2 path)
- Extend `build.rs` to discover `native/*.erl` files
- Compile via `compile:file/2` in the build worker (existing infrastructure)
- Add `native_modules` to `.app` file generation
- Wire output ebin paths into runtime code path

### Phase 2: Hex Dependencies (rebar3 path)
- Vendor rebar3 escript into `runtime/tools/rebar3`
- Extend `manifest.rs` to parse `[native.dependencies]`
- Generate `rebar.config` from aggregated constraints and invoke bundled rebar3
- Extend `beamtalk.lock` with `[[native_package]]` entries
- `beamtalk deps update` re-resolves native constraints

### Phase 3: HTTP Package Extraction
- Create `packages/http/` with `beamtalk.toml`
- Move 7 `.bt` classes from `stdlib/src/` to `packages/http/src/`
- Move 5 `.erl` modules from `runtime/apps/beamtalk_stdlib/src/` to `packages/http/native/`
- Remove `gun` from runtime `rebar.config` (`cowboy` stays in `runtime/rebar.config` — still needed by `beamtalk_workspace` for the WebSocket REPL server)
- Update stdlib tests

### Phase 4: REPL Hot-Loading
- Extend `load-project` to scan `native/` and compile all `.erl` files upfront via `compile:file/2`
- Extend single-file reload to demand-compile native `.erl` when `native:`/FFI annotation references it
- Incremental mtime tracking for `native/*.erl` files alongside `.bt` files

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
3. Remove `gun` from `runtime/rebar.config` (`cowboy` stays in `runtime/rebar.config` — still needed by `beamtalk_workspace` for the WebSocket REPL server)
4. Users who depend on HTTP classes add `http` to their `[dependencies]`

### For existing projects

No migration needed — this is additive. Projects without `native/` or `[native.dependencies]` are unaffected.

## Implementation Tracking

**Epic:** BT-1708
**Issues:** BT-1709, BT-1710, BT-1711, BT-1712, BT-1713, BT-1714, BT-1715, BT-1716, BT-1717, BT-1718, BT-1719
**Status:** Planned

## References
- Related issues: BT-1153, BT-1708
- Related ADRs: ADR 0026 (Project Manifest), ADR 0031 (Flat Namespace), ADR 0055 (Erlang-Backed Class Authoring), ADR 0056 (Native Erlang-Backed Actors), ADR 0058 (Platform Security Model), ADR 0070 (Package Namespaces and Dependencies), ADR 0071 (Class Visibility)
- Prior art: [Gleam FFI](https://gleam.run/writing-gleam/gleam-toml/), [Mix.Tasks.Compile.Erlang](https://hexdocs.pm/mix/Mix.Tasks.Compile.Erlang.html), [rebar3 dependencies](https://rebar3.org/docs/configuration/dependencies/)
