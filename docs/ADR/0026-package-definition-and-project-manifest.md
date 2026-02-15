# ADR 0026: Package Definition and Project Manifest

## Status
Proposed (2026-02-15)

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

### 2. `beamtalk.toml` manifest format

```toml
[package]
name = "my_counter"
version = "0.1.0"
description = "A simple counter example"
licences = ["Apache-2.0"]

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
- `licences` — SPDX licence identifiers.

**Reserved for future ADRs:**
- `[dependencies]` / `[dev-dependencies]` — Dependency resolution.
- `repository`, `links` — Publishing metadata.
- `target` — Compilation target options.
- `beamtalk` — Minimum compiler version.

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
│       ├── ebin/           # .beam files + .app file
│       │   ├── my_counter.app
│       │   ├── bt@my_counter@main.beam
│       │   └── bt@my_counter@counter.beam
│       └── core/           # Intermediate .core files
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
3. Compiles each with `bt@{name}@` prefix
4. Generates `.app` file with module list and class metadata
5. Outputs to `_build/dev/ebin/`

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

### 8. Package name validation

Package names must be:
- Lowercase ASCII letters, digits, and underscores
- Start with a letter
- 1–64 characters
- Not a reserved name (`beamtalk`, `stdlib`, `kernel`, `runtime`)
- Valid as an Erlang atom (for OTP application name)
- Valid as a Hex.pm package name (for future publishing)

```
✅ my_counter, json_parser, web_utils
❌ MyCounter, 123app, beamtalk, -dashes-, CamelCase
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
- 🧑‍💻 **Newcomer**: "This is what I expect — `cargo new`, `npm init`, `gleam new` all work this way. I know how to read TOML."
- 🎩 **Smalltalk purist**: "At least the REPL still gives me live coding. The package is just persistence — I can still `fileIn:` code interactively."
- ⚙️ **BEAM veteran**: "Finally, proper OTP applications! I can add this as a dependency in my Elixir project."
- 🏭 **Operator**: "OTP releases, proper `.app` files, standard deployment tooling. This is production-ready."
- 🎨 **Language designer**: "Clean separation: manifest declares, build system executes, runtime loads. Each concern has one place."

### Option B: Convention-only (no manifest, directory structure implies package)
- 🧑‍💻 **Newcomer**: "Zero config is nice — just put files in a directory and build."
- 🎩 **Smalltalk purist**: "Closer to image-based — the code is the truth, not a config file."
- ⚙️ **BEAM veteran**: "But how do I set the application name? Version? I need `.app` files."
- 🏭 **Operator**: "How do I control what gets deployed? I need explicit metadata."
- 🎨 **Language designer**: "Convention over configuration is elegant, but you always end up needing a manifest anyway (for publishing, versioning, dependencies)."

### Option C: Smalltalk-style image packages (Monticello-like)
- 🧑‍💻 **Newcomer**: "I don't know what an image is. I want files on disk and git."
- 🎩 **Smalltalk purist**: "This is the real way! Code lives in the system, not in files."
- ⚙️ **BEAM veteran**: "This is completely alien to the BEAM ecosystem. I can't use Hex.pm."
- 🏭 **Operator**: "How do I deploy this? How do I roll back? I need reproducible builds."
- 🎨 **Language designer**: "Elegant in a Smalltalk context, but forces every tool (CI, editor, debugger) to understand images."

### Tension Points
- **Smalltalk purists prefer C** but every other cohort strongly prefers A
- **Newcomers could live with B** but would miss version/metadata when publishing
- **All cohorts except Smalltalk purists agree** that OTP application mapping (A) is essential for BEAM interop

## Alternatives Considered

### Alternative: No manifest — derive everything from directory name
Package name = directory name, version = git tag, no `beamtalk.toml` needed.

**Rejected because:** Can't set description, licence, or any metadata. Git tags are unreliable as the sole version source. Breaks when directory is renamed. Every other BEAM language uses a manifest.

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

### Neutral
- Package name validation rules may reject some creative names (but prevents Hex.pm conflicts later)
- `.app` file generation adds a build step (but it's fast and standard)
- `_build/` directory convention borrowed from Elixir — familiar to some, new to others

## Implementation

### Phase 1: Manifest parsing
- Add `toml` crate dependency to `beamtalk-cli`
- Create `PackageManifest` struct: `name`, `version`, `description`, `licences`
- Parse `beamtalk.toml` in build command when present
- Validate package name (rules from §8)
- Fallback to current behavior when no manifest found

### Phase 2: Build output restructuring
- Output to `_build/dev/ebin/` when in package mode
- Generate `.app` file from manifest + discovered modules
- Apply `bt@{name}@{module}` naming to compiled modules
- Update `.gitignore` template to include `_build/`

### Phase 3: Scaffold updates
- Update `beamtalk new` to create `src/` with source files
- Create empty `test/` directory
- Update `.gitignore` for `_build/`
- Validate package name at creation time

### Phase 4: Workspace integration
- Workspace discovery (ADR 0004) uses `beamtalk.toml` as primary marker (already does)
- When `beamtalk repl` finds a `beamtalk.toml`, it auto-compiles the package and adds `_build/dev/ebin/` to the code path — all package classes are available in the REPL immediately
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
| OTP `.app` | No `{mod, ...}` entry | `{mod, {StartModule, []}}` |
| As dependency | Loaded on code path | Started as OTP application |

**Affected components:** `beamtalk-cli` (build, new, run commands), `beamtalk-core` (module naming in codegen), workspace discovery, REPL module loading.

**Estimated size:** L (manifest parsing + build restructure + scaffold + integration)

## References
- Related ADRs: ADR 0004 (workspaces), ADR 0007 (compilable stdlib), ADR 0009 (OTP app structure), ADR 0016 (module naming)
- Prior art: [Gleam `gleam.toml`](https://gleam.run/writing-gleam/gleam-toml/), [Cargo manifest](https://doc.rust-lang.org/cargo/reference/manifest.html), [Elixir Mix](https://hexdocs.pm/mix/Mix.html)
- Future work: Dependency resolution and Hex.pm publishing (separate ADR)
- Documentation: `docs/beamtalk-architecture.md` §Directory Structure
