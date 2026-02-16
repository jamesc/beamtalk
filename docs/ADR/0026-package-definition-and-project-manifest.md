# ADR 0026: Package Definition and Project Manifest

## Status
Accepted (2026-02-16)

## Context

### The Problem

Beamtalk has a `beamtalk new` command that scaffolds a project with a `beamtalk.toml` manifest, but:

1. **The manifest is never parsed** — `beamtalk build` ignores `beamtalk.toml` entirely. It simply finds all `.bt` files and compiles them. The manifest is decorative.

2. **"Package" is undefined** — There is no formal definition of what constitutes a Beamtalk package. The architecture doc shows a `my_project/` layout with `beamtalk.toml`, `src/`, `test/`, `_build/`, and `deps/`, but this is aspirational — the build system doesn't enforce or use any of it.

3. **No OTP application mapping** — A Beamtalk package should compile to an OTP application (`.app` file) so it can participate in the BEAM ecosystem: be started, stopped, supervised, and depended on by Erlang/Elixir code. Today, `beamtalk build` produces loose `.beam` files with no application metadata.

4. **Module naming is disconnected** — ADR 0016 established `bt@package@module` naming (e.g., `bt@stdlib@integer`), but the build system has no concept of "which package am I building?" to fill in the middle segment.

5. **No entry point convention** — There's no standard way to say "run this package" (`beamtalk run`). The current `beamtalk run` command exists but has no manifest-driven entry point.

### Current State

**`beamtalk new myapp` creates:**
```
myapp/
├── beamtalk.toml       # Decorative — never read
├── src/
│   └── main.bt         # Entry point by convention
├── README.md
└── .gitignore
```

**`beamtalk.toml` template:**
```toml
[package]
name = "myapp"
version = "0.1.0"

[dependencies]
```

**`beamtalk build` does:**
1. Find all `.bt` files recursively
2. Compile each to Core Erlang (`.core`)
3. Compile each `.core` to BEAM (`.beam`) via `erlc`
4. Place all outputs in `build/`

**What's missing:**
- Manifest parsing → package name, version, metadata
- OTP `.app` file generation
- Module naming using package name (`bt@myapp@counter`)
- Entry point resolution
- Build directory structure (`_build/` with profiles)
- Source directory configuration

### Constraints

- Must align with ADR 0016's `bt@package@module` naming convention
- Must produce valid OTP applications (`.app` files) for BEAM interop
- Must support the workspace model (ADR 0004, ADR 0009)
- Must be forward-compatible with future dependency management (Hex.pm)
- Must feel natural to Gleam/Elixir/Cargo users coming to Beamtalk
- Must not break existing `beamtalk build path/to/file.bt` single-file compilation

## Decision

### 1. A Package is the unit of code distribution and compilation

A **package** is a directory containing a `beamtalk.toml` manifest. It is:
- The unit you publish to Hex.pm
- The unit you depend on
- The unit that compiles to an OTP application
- The middle segment in `bt@package@module` naming

There is no separate "project" concept. Following Gleam and Cargo, the term **package** covers both local development and distribution.

**Relationship to workspaces (ADR 0004):** Your package *is* the workspace — it's the running application you develop interactively. Dependency packages are libraries loaded into your workspace. When you `beamtalk repl` in a package directory, you create (or reconnect to) a workspace that owns that package's actors and state. Dependencies provide classes but don't own the workspace. The lifecycle is: **author** (source files) → **develop** (workspace = your running package) → **deploy** (OTP release).

**Long-term vision: hybrid workspace-as-package.** This ADR establishes the file-based foundation (Option A), but the architecture is designed to evolve toward a hybrid model where the workspace is the primary artifact and source files are a synced view (see [Future Direction](#future-direction-toward-workspace-as-package-option-c)). Each phase of this ADR — manifest parsing, workspace auto-compile, application start — is a stepping stone toward that goal. Option A is not the destination; it's the pragmatic starting point that lets us ship today while building toward the bold choice incrementally.

### 2. `beamtalk.toml` manifest format

```toml
[package]
name = "my_counter"
version = "0.1.0"
description = "A simple counter example"
licenses = ["Apache-2.0"]

# Optional metadata (for Hex.pm publishing — future ADR)
# repository = "https://github.com/user/my_counter"
# links = { homepage = "https://example.com" }

[dependencies]
# Future ADR — dependency resolution
# beamtalk_json = "~> 1.0"
```

**Required fields:**
- `name` — Package name. Must be lowercase, alphanumeric with underscores. Used as the OTP application name and the middle segment in module naming (`bt@{name}@{module}`).
- `version` — Semantic version (major.minor.patch).

**Optional fields (initial):**
- `description` — Short description (for Hex.pm, documentation).
- `licenses` — SPDX license identifiers.

**Reserved for future ADRs:**
- `[dependencies]` / `[dev-dependencies]` — Dependency resolution.
- `repository`, `links` — Publishing metadata.
- `target` — Compilation target options.
- `beamtalk` — Minimum compiler version.
- `start` — Application entry point module (see Phase 5 below). Added with the start callback implementation, not the initial manifest parser.

### 3. Package ↔ OTP Application mapping

Each package compiles to a single OTP application. The build system generates a `.app` file:

```erlang
{application, my_counter, [
    {description, "A simple counter example"},
    {vsn, "0.1.0"},
    {modules, ['bt@my_counter@main', 'bt@my_counter@counter']},
    {registered, []},
    {applications, [kernel, stdlib, beamtalk_runtime]},
    {env, [
        {classes, [
            {'bt@my_counter@counter', 'Counter', 'Actor'}
        ]}
    ]}
]}.
```

**Key mappings:**
| `beamtalk.toml` | OTP `.app` |
|---|---|
| `name` | Application name |
| `version` | `{vsn, ...}` |
| `description` | `{description, ...}` |
| Source files | `{modules, [...]}` — auto-discovered |
| Class definitions | `{env, [{classes, [...]}]}` — for runtime registration |

Every package implicitly depends on `beamtalk_runtime` (and transitively `beamtalk_stdlib`).

### 4. Module naming integration

When building a package, source files are named according to ADR 0016:

```
src/counter.bt    → bt@my_counter@counter
src/main.bt       → bt@my_counter@main
src/util/math.bt  → bt@my_counter@util@math
```

The formula is: `bt@{package_name}@{relative_path_without_extension}` where path separators become `@`.

Subdirectories within `src/` are **namespacing only** — they do not create subpackages. The entire `src/` tree belongs to one flat package. This follows the Gleam model.

**Class name uniqueness (temporary constraint):** Within a workspace, class names must be globally unique across all loaded packages. If your package and a dependency both define `Counter`, the compiler will report an error. The BEAM module names are already unique (`bt@my_app@counter` vs `bt@other_lib@counter`), but Beamtalk dispatches by class name, not module name. This is a known limitation — qualified class names (e.g., `Counting.Counter`) require a module/namespace system (future ADR, high priority once dependencies are supported). Until then, the strict compiler error keeps behavior predictable.

**Single-file mode** (no manifest): When `beamtalk build file.bt` is invoked on a file outside any package, the module name is the file stem without any package prefix (current behavior, preserved for scripting/experimentation).

### 5. Directory structure

```
my_counter/
├── beamtalk.toml           # Package manifest (required)
├── src/                    # Source files (required)
│   ├── main.bt             # Entry point (conventional)
│   └── counter.bt          # Additional modules
├── test/                   # BUnit tests (optional)
│   └── counter_test.bt
├── _build/                 # Build output (generated)
│   └── dev/                # Profile (future: test, prod)
│       └── ebin/           # .beam files + .app file
│           ├── my_counter.app
│           ├── bt@my_counter@main.beam
│           └── bt@my_counter@counter.beam
├── AGENTS.md               # AI agent guide (generated)
├── .github/
│   └── copilot-instructions.md  # Copilot custom instructions (generated)
├── .mcp.json               # MCP server config (generated)
├── README.md               # Documentation (optional)
└── .gitignore
```

**Changes from current layout:**
| Current | New | Reason |
|---|---|---|
| `build/` | `_build/dev/ebin/` | Profile support, matches Erlang/Elixir convention |
| No `.app` file | `_build/dev/ebin/{name}.app` | OTP application |
| Flat `.beam` output | Namespaced `.beam` files | ADR 0016 module naming |

### 6. Build behavior changes

**Package mode** (manifest found):
```bash
$ cd my_counter
$ beamtalk build
Building my_counter v0.1.0
  Compiling counter.bt → bt@my_counter@counter
  Compiling main.bt → bt@my_counter@main
  Generating my_counter.app
Build complete: 2 modules in _build/dev/ebin/
```

The build system:
1. Reads `beamtalk.toml` for package name and version
2. Discovers `.bt` files in `src/` (not the root — prevents compiling test files)
3. Compiles each with `bt@{name}@` prefix via the embedded compiler port (ADR 0022)
4. Generates `.app` file with module list and class metadata
5. Writes `.beam` files to `_build/dev/ebin/` **relative to the package root** (where `beamtalk.toml` lives), regardless of the current working directory

**Single compilation path:** Both `beamtalk build` and `beamtalk repl` use the same embedded compiler port (ADR 0022). There are no intermediate `.core` files on disk — compilation is fully in-memory (`Source → Port → Core Erlang → compile:forms → .beam`). For debugging, `beamtalk build --emit-core` can dump Core Erlang to disk.

**Note:** This fixes a current bug where `beamtalk build` creates `build/` relative to the CWD. In package mode, `_build/` is always anchored to the manifest location.

**File mode** (no manifest):
```bash
$ beamtalk build script.bt
  Compiling script.bt → script
Build complete: 1 module in build/
```

Preserved for quick scripting — no package prefix, output to `build/`.

### 7. `beamtalk new` scaffolding updates

```bash
$ beamtalk new my_counter
Created package 'my_counter'

Next steps:
  cd my_counter
  beamtalk build
  beamtalk repl
```

The scaffold creates the directory structure from §5 with:
- `beamtalk.toml` with `name` and `version`
- `src/main.bt` with a hello-world example
- `test/` directory (empty)
- `.gitignore` including `_build/`
- `README.md`
- `AGENTS.md` — AI agent guide (build commands, project structure, Beamtalk syntax basics, links to language docs)
- `.github/copilot-instructions.md` — GitHub Copilot custom instructions for the package
- `.mcp.json` — MCP server configuration (if applicable tooling exists)

### 8. Package name validation

Package names must be:
- Lowercase ASCII letters, digits, and underscores
- Start with a letter
- 1–64 characters
- Not a reserved name: `beamtalk`, `stdlib`, `kernel`, `runtime`, `workspace`, `compiler`
- Not an Erlang standard application name: `crypto`, `ssl`, `inets`, `mnesia`, `observer`, etc.
- Valid as an Erlang atom (for OTP application name)
- Valid as a Hex.pm package name (for future publishing)

```
✅ my_counter, json_parser, web_utils
❌ MyCounter, 123app, beamtalk, -dashes-, CamelCase, stdlib, kernel
```

**Error on invalid name:**
```bash
$ beamtalk new stdlib
Error: 'stdlib' is a reserved package name (conflicts with Beamtalk standard library)

$ beamtalk new MyApp
Error: Package name 'MyApp' is invalid — must be lowercase (try 'my_app')
```

## Prior Art

### Gleam (`gleam.toml`)
Closest model. Simple TOML manifest, compiles to OTP applications, publishes to Hex.pm. Uses `@` separator for module namespacing (`gleam@json`). Package = project, no distinction. `gleam new` scaffolds everything. PubGrub for dependency resolution.

**Adopted:** TOML format, package-as-project terminology, `@` naming (already in ADR 0016), `.app` generation, `_build/` directory.

### Cargo (`Cargo.toml`)
Rich manifest supporting workspaces, multiple targets (lib/bin/test/bench), feature flags. Distinguishes "package" (manifest unit) from "crate" (compilation unit). Extremely mature dependency resolution.

**Adopted:** Manifest-driven builds, semantic versioning, required `name`+`version`. **Not adopted:** Package/crate distinction (too complex for Beamtalk's needs), `[[bin]]` target syntax.

### Elixir (`mix.exs`)
Code-as-configuration (Elixir script, not data format). Tight OTP integration — every Mix project is an OTP application. `mix new` scaffolds with supervision tree option.

**Adopted:** Every package = OTP application, implicit `beamtalk_runtime` dependency. **Not adopted:** Code-as-config format (TOML is simpler, more tooling-friendly).

### Erlang (`rebar.config`)
Erlang-term configuration. Close to OTP primitives. Umbrella projects via `apps/` directory. Release management via `relx`.

**Adopted:** `.app` file generation, `ebin/` output directory. **Not adopted:** Erlang-term config format (TOML is more approachable).

### Pharo (Monticello + Metacello)
Image-based — no files on disk in the traditional sense. Packages are in-image collections of classes/methods. Metacello adds project-level dependency management on top.

**Not adopted:** Image-based model doesn't translate to file-based compilation. But the philosophy — a package is a coherent collection of classes — informs our design.

### Newspeak
No packages or global namespace at all. Modules are top-level classes. Dependencies are constructor parameters (explicit injection). Extremely pure, but requires an image-based environment.

**Not adopted:** Pure DI module system is too radical for file-based BEAM compilation. But the principle — explicit dependencies — influences our future dependency design.

## User Impact

### Newcomer (from Python/JS/Ruby)
`beamtalk new` + `beamtalk build` works like `cargo new` + `cargo build` or `gleam new` + `gleam build`. The `beamtalk.toml` format is familiar TOML. Source goes in `src/`, tests in `test/`, output in `_build/`. No surprises.

### Smalltalk developer
Departure from image-based development — packages are directories, not in-image collections. But the REPL/workspace (ADR 0004) provides the interactive, live-coding experience. The package is the on-disk representation of what would be a Monticello package.

### Erlang/Elixir developer
Packages compile to OTP applications — this is exactly what they expect. The `.app` file, `ebin/` directory, and application dependency tree are all standard BEAM patterns. They can depend on a Beamtalk package from rebar3 or Mix.

### Production operator
OTP application structure means standard release tooling works: `relx`, `mix release`, `rebar3 release`. Applications start/stop cleanly with supervision trees. Observable with `observer`, `recon`, etc.

## Steelman Analysis

### Option A: Manifest-driven packages (this decision)

- 🧑‍💻 **Newcomer**: "TOML manifests are universal now — `beamtalk.toml` is immediately familiar from Cargo, Gleam, and every modern language. I can edit metadata without learning Beamtalk syntax first, use existing TOML tooling, and onboard in minutes."
- 🎩 **Smalltalk developer**: "I'm losing something real here. In Pharo, the running image *is* the single source of truth — manifests force out-of-band metadata edits and rebuild steps that break the live REPL-first flow. This adds cognitive overhead and drift risk to what should be a pure 'define classes and reload' loop. Worse, package renames change module identity and break persistent workspaces (ADR 0004), making manifests actively hostile to image-style development."
- ⚙️ **BEAM veteran**: "This is exactly right for interop. I can depend on a Beamtalk package from rebar3 or Mix because it produces a proper `.app` file. The module naming (`bt@package@module`) is clean and callable from Erlang."
- 🏭 **Operator**: "OTP applications mean standard release tooling: `relx`, `mix release`, systemd. Explicit version numbers and metadata for compliance and audit trails. This is production-ready."
- 🎨 **Language designer**: "Clean separation: manifest declares, build system executes, runtime loads. But TOML is static data — I can't do `version = git_describe()`. This pushes computed metadata into the build tool. Still, TOML's simplicity is its strength — harder to have security issues or confusing evaluation order in static data."

### Option B: Convention-only (no manifest, directory structure implies package)

- 🧑‍💻 **Newcomer**: "Zero config is compelling — I just write code. Go did this for a decade before `go.mod`. No manifest means no manifest to mess up, no TOML syntax errors, no 'forgot to bump version' bugs. The BEAM itself uses conventions: `apps/`, `src/`, `ebin/` are structural, not declared in a file."
- 🎩 **Smalltalk developer**: "Convention-only is closer to the Smalltalk philosophy — the code is the truth. In Pharo, there's no 'project manifest' — the package is defined by its contents. Directory structure as convention means the code organization *is* the metadata."
- ⚙️ **BEAM veteran**: "rebar3 and Mix already infer a lot from directory structure. OTP's `.app` files can be generated from directory scans. I've shipped Erlang apps where the only 'manifest' was the directory name. But Hex.pm, rebar3, and Mix all expect manifests for metadata and dependency resolution — going convention-only would fight the established BEAM tooling and limit interop."
- 🏭 **Operator**: "Convention-only can work if the conventions are strong enough — Git tags for versions, directory name for package name, LICENSE file for license. But I'd need robust tooling to extract metadata for compliance."
- 🎨 **Language designer**: "Convention over configuration is a legitimate design principle. Go ran convention-only (GOPATH, ≈2009–2018) but eventually hit walls on vendoring, version pinning, and reproducibility — `go.mod` was added to fix those real problems. Starting convention-only in the BEAM ecosystem would repeat known pain given Hex.pm already exists and expects manifests."

### Option C: Smalltalk-style image packages (Monticello-like)

- 🧑‍💻 **Newcomer**: "I'd need to learn a new paradigm, but there's real value. Pharo's image means the running system *is* the source of truth — no 'it works on my machine', no compile step, no stale builds. The barrier is tooling: can I use VS Code? Git? If those are solved, images are compelling."
- 🎩 **Smalltalk developer**: "Beamtalk already has persistent workspaces (ADR 0004) where actors survive disconnection — that's already halfway to an image. The workspace *is* a running system with state. Why not lean into that fully? Package a workspace as the unit of distribution. The BEAM's hot code loading makes this *more* viable than in standard Smalltalk."
- ⚙️ **BEAM veteran**: "The difference between 'image' and 'BEAM node with hot code loading' is smaller than it looks. OTP supports two-version code loading, release handlers, and `code_change` callbacks — exporting workspaces as releases leverages native hot upgrade semantics, making image-like packages uniquely natural on BEAM. If Beamtalk workspaces exported as releases, that's functionally an image. The question is interop: can Erlang/Elixir code import a Beamtalk 'image package'?"
- 🏭 **Operator**: "Images solve reproducibility differently: instead of 'here's source, build it identically', images say 'here's the exact running system, deploy it'. Docker popularized this for good reason. But I'd need rollback, multi-instance, and inspection tooling that Pharo doesn't have."
- 🎨 **Language designer**: "The image model is internally consistent. The workspace (ADR 0004) already maintains running actors — the gap to 'export workspace as deployable unit' is smaller than it seems. But I'd be building a new ecosystem: image diff/merge, image-based CI. Git and text files have 50 years of tooling."

### Option D: Beamtalk-as-config (manifest written in Beamtalk, like mix.exs)

- 🧑‍💻 **Newcomer**: "One language for everything sounds nice, but executable manifests raise learning and security hurdles — I'd need to understand Beamtalk before I can configure a project. And external tools (CI, Hex.pm, IDEs) would need a Beamtalk evaluator to read package metadata."
- 🎩 **Smalltalk developer**: "Configuration is code, code is configuration. In Pharo, configuration happens through method execution. A Beamtalk manifest could be expressive and consistent with the language philosophy — `dependencies: Collection gatherFrom: src/`."
- ⚙️ **BEAM veteran**: "Elixir does this with `mix.exs` and it works. But rebar3/relx/Hex.pm expect data-format configs — code manifests add bootstrapping complexity and interop friction. And there's a chicken-and-egg: you need a compiler to read the manifest, but you need the manifest to configure the compiler."
- 🏭 **Operator**: "Executable configs are a security concern. TOML is data — I can inspect it safely. Beamtalk code could do anything. How do I audit it? Mix addresses this with conventions ('don't do I/O in mix.exs'), but it's trust-based."
- 🎨 **Language designer**: "Manifests are *just metadata*, and metadata should be data, not code. TOML is readable by any language. But the counter-argument is real: computed metadata (`version = git_describe()`) is useful, and TOML can't do it — you'd need build tool hooks instead."

### Tension Points

1. **The Workspace Paradox** — ADR 0004 already creates persistent workspaces with running actors that survive disconnection. This *is* an image model, just not called that. If workspaces are essentially long-running BEAM nodes with state, why not embrace that and make the workspace exportable as the package format (Option C)? **Resolution:** We will — but incrementally. Option A builds the foundation (manifest, OTP mapping, build output) that Option C needs anyway. The hybrid destination is workspace-primary with source files as a synced view, giving image benefits with file-based tooling compatibility. See [Future Direction](#future-direction-toward-workspace-as-package-option-c).

2. **Interop vs Purity** — The strongest argument for A is BEAM interop: OTP applications, Hex.pm, rebar3/Mix compatibility. The strongest argument for C is internal consistency: the running system is the truth. The hybrid approach resolves this: the workspace is the truth at development time, but it exports standard OTP releases and generates `.app` files for ecosystem interop. Both sides get what they need.

3. **The Go Counter-Example** — Option B has a real case study: Go thrived for years with convention-only (GOPATH). Modules (`go.mod`) were controversial when introduced. But Go's convention-only also created problems: vendoring complexity, GOPATH friction, version pinning hacks. The question is whether Beamtalk should learn from Go's eventual destination (manifests) or its successful journey (start convention-only, add manifests when the pain is clear).

4. **Metadata Drift** — Option A introduces a class of bugs that B and C don't have: the manifest says one thing, the code says another. Version numbers not bumped, descriptions stale, module lists out of sync. Auto-generation mitigates this (we generate `.app` from source scanning) but doesn't eliminate it (version must be manually maintained).

5. **Convention vs Explicit** — Option B argues conventions can replace manifests. True for some metadata (directory name → package name) but problematic for others (version numbers, licenses). The tension is "derive what you can" (less boilerplate) vs "declare what you mean" (less magic). Reasonable engineers disagree here.

6. **Build Reproducibility** — Manifests enable lockfiles and deterministic builds; images snapshot state but don't guarantee that independent rebuilds produce identical results. For CI/CD pipelines, manifests win; for deployment snapshots, images win.

7. **Team Development** — Manifests and source files merge in Git; images require bespoke diff/merge/introspection tooling that doesn't exist yet. For teams using standard code review workflows, this is a hard constraint favoring A.

8. **Debugging Production** — Source files are greppable, log-linkable, and map to stack traces. Images demand specialized introspection tools (`observer`, process inspection). BEAM veterans have these tools; newcomers don't.

## Alternatives Considered

### Alternative: No manifest — derive everything from directory name
Package name = directory name, version = git tag, no `beamtalk.toml` needed.

**Rejected because:** Can't set description, license, or any metadata. Git tags are unreliable as the sole version source. Breaks when directory is renamed. Every other BEAM language uses a manifest.

### Alternative: Erlang-term config (`beamtalk.config`)
Use Erlang term format instead of TOML for the manifest.

**Rejected because:** Erlang terms are unfamiliar to newcomers. TOML is the established choice for new languages (Gleam, Cargo, Hugo, etc.). TOML has better editor support and is more readable.

### Alternative: Multiple packages per repository (workspace support)
Support a `[workspace]` section in `beamtalk.toml` listing sub-packages, like Cargo workspaces.

**Deferred:** Valuable for large projects but adds complexity. Can be added as an extension to `beamtalk.toml` in a future ADR without breaking changes. Start with one package per repository.

### Alternative: Subpackages (Pharo-style)
Allow subdirectories within `src/` to be independent packages with their own `beamtalk.toml`, similar to Pharo's package categories or Cargo workspaces.

**Rejected in favor of flat packages with nested modules:** Subdirectories within `src/` are purely namespace segments — `src/util/math.bt` becomes module `bt@my_app@util@math` within the single `my_app` package. This follows the Gleam model and is the simplest design. Multi-package repositories can be supported later via workspaces without breaking this convention.

## Consequences

### Positive
- `beamtalk.toml` goes from decorative to functional — single source of truth for package metadata
- Packages produce OTP applications — first-class BEAM citizens, interoperable with Erlang/Elixir
- Module naming (ADR 0016) gets its missing piece — the package name comes from the manifest
- `beamtalk new` scaffolds a complete, buildable, correct package
- Clear path to Hex.pm publishing — just add `[dependencies]` support later
- `_build/` directory with profiles prepares for dev/test/prod builds

### Negative
- Breaking change to build output: `build/` → `_build/dev/ebin/` (but `build/` was never stable API)
- Existing projects created with `beamtalk new` need to add a `src/` directory (mild migration)
- Single-file scripting requires detecting "no manifest" mode (added complexity in build command)
- TOML parsing dependency needed in the compiler
- **Package rename breaks actor identity** — actors hold module references; renaming a package changes all module names, breaking running actors in persistent workspaces (ADR 0004). Mitigation: document as limitation, recommend stopping workspace before rename. Future path: module aliasing or actor identity indirection could decouple actor identity from module names, but requires the module/namespace system.

### Neutral
- Package name validation rules may reject some creative names (but prevents Hex.pm conflicts later)
- `.app` file generation adds a build step (but it's fast and standard)
- `_build/` directory convention borrowed from Elixir — familiar to some, new to others

## Implementation

### Phase 1: Manifest parsing
- Add `toml` crate dependency to `beamtalk-cli`
- Create `PackageManifest` struct: `name`, `version`, `description`, `licenses`
- Parse `beamtalk.toml` in build command when present
- Validate package name (rules from §8)
- Fallback to current behavior when no manifest found

### Phase 2: Build output restructuring
- Migrate `beamtalk build` to use embedded compiler port (ADR 0022) — same path as REPL, no `.core` files on disk
- Output to `_build/dev/ebin/` when in package mode
- Generate `.app` file from manifest + discovered modules
- Apply `bt@{name}@{module}` naming to compiled modules
- Add `--emit-core` flag for debugging (writes `.core` files to `_build/dev/core/`)
- Update `.gitignore` template to include `_build/`

### Phase 3: Scaffold updates
- Update `beamtalk new` to create `src/` with source files
- Create empty `test/` directory
- Update `.gitignore` for `_build/`
- Validate package name at creation time

### Phase 4: Workspace integration
- Workspace discovery (ADR 0004) uses `beamtalk.toml` as primary marker (already does)
- When `beamtalk repl` finds a `beamtalk.toml`, it auto-compiles the package and adds `_build/dev/ebin/` to the code path — all package classes are available in the REPL immediately
- **On compile failure:** show errors but start the REPL anyway — stdlib and workspace singletons are still available, just without the package's classes. The user can fix errors and `:reload`
- `beamtalk test` discovers tests in `test/` relative to package root
- `beamtalk run` resolves entry point from package

### Phase 5: Application start callback
A package without a start callback is a **library** — it provides classes but doesn't do anything on its own. A package with a start callback is an **application** — it runs.

The manifest declares the entry point:
```toml
[package]
name = "my_web_app"
version = "0.1.0"
start = "app"    # Module containing start method (src/app.bt)
```

The start module contains a `start` method — conceptually like `main` in C/Go, but it starts a long-running system rather than running to completion:

```beamtalk
// src/app.bt — Application entry point
start =>
    server := WebServer spawn
    server listen: 8080
    Transcript show: 'Web server started on port 8080'; cr
```

**How it works under the hood:**
1. `beamtalk run` compiles the package (Phase 2)
2. Starts a BEAM node with `beamtalk_runtime` and `beamtalk_stdlib`
3. Loads the package's `.app` file (adds `_build/dev/ebin/` to code path)
4. Calls the start module's `start` method
5. The BEAM node keeps running (actors are supervised under `beamtalk_actor_sup`)

The `start` method is imperative — it spawns actors and they're auto-supervised. This works today because all Beamtalk actors are already `gen_server` processes under `beamtalk_actor_sup`. A future ADR on OTP Behaviour Mapping may evolve this to support declarative supervision trees and restart strategies, but the simple imperative `start` is the right starting point.

**Library vs Application:**
| | Library (no `start`) | Application (`start = "app"`) |
|---|---|---|
| `beamtalk build` | ✅ Compiles to `.beam` + `.app` | ✅ Same |
| `beamtalk repl` | ✅ Classes available | ✅ Classes available + start method callable |
| `beamtalk run` | ❌ Error: "no start module" | ✅ Starts the application |
| OTP `.app` | No `{mod, ...}` entry | Includes OTP application metadata; `{mod, ...}` callback and Erlang shim mechanism defined in a future ADR |
| As dependency | Loaded on code path | Started as OTP application |

**Affected components:** `beamtalk-cli` (build, new, run commands), `beamtalk-core` (module naming in codegen), workspace discovery, REPL module loading.

**Estimated size:** L (manifest parsing + build restructure + scaffold + integration)

## Migration Path

### Existing projects created with `beamtalk new`

Projects scaffolded before this ADR already have:
- ✅ `beamtalk.toml` with `[package]` section — no change needed
- ✅ `src/` directory with `main.bt` — no change needed
- ❌ Build output in `build/` — will move to `_build/dev/ebin/`
- ❌ No `.app` file — will be generated automatically
- ❌ Missing `AGENTS.md`, GitHub Copilot instructions (`.github/copilot-instructions.md`), `.mcp.json` — can be generated with a future `beamtalk init` command

**Migration steps:**
1. Delete old `build/` directory
2. Update `.gitignore` to include `_build/` instead of `/build/`
3. Run `beamtalk build` — output now goes to `_build/dev/ebin/`

### Single-file scripts

No migration needed. `beamtalk build file.bt` continues to work as before — no package prefix, output to `build/`.

### Stdlib and test suites

The stdlib (`lib/*.bt`) and test suites (`tests/stdlib/`, `tests/e2e/`) are **not packages** — they are compiled by dedicated build commands (`just build-stdlib`, `just test-stdlib`, `just test-e2e`) that use their own module naming conventions. Package-mode compilation only activates when a `beamtalk.toml` is found, so these existing workflows are unaffected.

## Future Direction: Toward Workspace-as-Package (Option C)

This ADR chooses Option A (manifest + source files) as the pragmatic foundation, but the long-term destination is Option C — workspaces as the primary artifact, with source files as a derived view. Option A is a stepping stone, not the end state.

The path from A to C has natural milestones, each independently valuable:

### Step 1: Package foundation (this ADR)
`beamtalk.toml` + `src/` + OTP application. Standard file-based workflow. Git, CI, code review all work out of the box.

### Step 2: Workspace auto-compile (this ADR, Phase 4)
`beamtalk repl` in a package directory compiles source and boots the workspace. The workspace *is* the running package. This is already halfway to an image — the workspace has running actors, loaded classes, and live state.

### Step 3: Durable workspaces
[Khepri](https://github.com/rabbitmq/khepri)-backed persistence for workspace state — actor state, loaded classes, REPL bindings survive restarts. The workspace becomes a real live image, not just a development session. Separate ADR required.

### Step 4: Workspace → source sync
Bidirectional sync between workspace state and `.bt` source files (inspired by Pharo's Tonel format). Define a class in the REPL → `.bt` file appears. Edit a `.bt` file → workspace hot-reloads. Source files become a *view* of the workspace, not the sole source of truth. This is the key enabler — it preserves Git/review/CI workflows while making the workspace primary.

### Step 5: Workspace export as release
`beamtalk release` exports the running workspace as a deployable OTP release. No separate build step — the workspace *is* the built artifact. Dependencies are other workspace exports (or standard OTP applications). Hex.pm publishing generates metadata from workspace introspection.

### Why not start at C?

Each step requires the previous one, and each delivers value independently:
- Step 1 gives a working package system today with existing tools
- Step 2 gives REPL-first development
- Step 3 gives persistence (no more "restart and lose everything")
- Step 4 gives the hybrid model (image-primary, files for tooling)
- Step 5 gives image-based deployment

Starting at C would require building all five simultaneously — workspace sync, Khepri integration, release export, *and* the manifest parsing from Option A anyway (for interop). Option A lets us ship a useful package system now while building toward the bold vision incrementally.

**The BEAM makes this path uniquely viable.** OTP's native hot code loading, two-version module support, release handlers, and `code_change` callbacks mean "workspace as deployable image" isn't a novel runtime — it's leveraging infrastructure that already exists. Beamtalk just needs to expose it with the right abstractions.

## References
- Related ADRs: ADR 0004 (workspaces), ADR 0007 (compilable stdlib), ADR 0009 (OTP app structure), ADR 0016 (module naming), ADR 0022 (embedded compiler)
- Prior art: [Gleam `gleam.toml`](https://gleam.run/writing-gleam/gleam-toml/), [Cargo manifest](https://doc.rust-lang.org/cargo/reference/manifest.html), [Elixir Mix](https://hexdocs.pm/mix/Mix.html)
- Future work: Dependency resolution and Hex.pm publishing (separate ADR)
- Future exploration: [Khepri](https://github.com/rabbitmq/khepri) (Raft-based replicated tree database) as workspace persistence layer (Step 3 above)
- Tooling inspiration: [Pharo Tonel format](https://github.com/pharo-vcs/tonel) — source file format enabling round-trip between image and file-based workflows (Step 4 above)
- Documentation: `docs/beamtalk-architecture.md` §Directory Structure
