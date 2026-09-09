# Package Management

Beamtalk organizes code into **packages** — the unit of compilation, distribution, and dependency management. Each package compiles to an OTP application, making it a first-class citizen on the BEAM.

## Project Manifest (`beamtalk.toml`)

Every package has a `beamtalk.toml` manifest at its root. This is the single source of truth for the package's identity and dependencies.

### Creating a New Package

```bash
beamtalk new my_app          # Library (default)
beamtalk new my_app --app    # Application with supervisor + Main
```

Both commands scaffold a project with `beamtalk.toml`, source files, tests, a `Justfile`, CI config, and agent guides. See [Tooling — Project Types](beamtalk-tooling.md#project-types) for the full breakdown.

### `[package]` Section

```toml
[package]
name = "my_app"
version = "0.1.0"
description = "A short description of the package"
licenses = ["Apache-2.0"]
```

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Package name. Lowercase ASCII letters, digits, and underscores. Must start with a letter, 1–64 characters. Used as the OTP application name and the middle segment in BEAM module naming (`bt@my_app@counter`). |
| `version` | Yes | Semantic version (`major.minor.patch`). |
| `description` | No | Short human-readable description. |
| `licenses` | No | SPDX license identifiers (e.g. `["Apache-2.0"]`). |
| `strict-deps` | No | When `true`, using a class from a transitive dependency is a compile error instead of a warning. Defaults to `false`. Recommended for libraries. |

**Reserved names:** `beamtalk`, `stdlib`, `kernel`, `runtime`, `workspace`, `compiler`, and Erlang standard application names (`crypto`, `ssl`, `inets`, `mnesia`, `observer`, etc.) cannot be used as package names.

```text
✅ my_counter, json_parser, web_utils
❌ MyCounter, 123app, beamtalk, -dashes-, CamelCase, stdlib, kernel
```

### `[dependencies]` Section

Dependencies are declared with three source types — registry, path, and git:

```toml
[dependencies]
# Registry — resolved by name + exact version through the package registry
yaml = "0.2.1"

# Local path — for monorepos and local development
utils = { path = "../my-utils" }

# Git repository — pinned to a tag, branch, or commit
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
http = { git = "https://github.com/someone/beamtalk-http", branch = "main" }
crypto_utils = { git = "https://github.com/someone/bt-crypto", rev = "abc1234" }
```

Each dependency must point to a directory containing its own `beamtalk.toml`. Dependencies are resolved transitively — if `json` depends on `utils`, both are compiled and available.

**Git references:** Exactly one of `tag`, `branch`, or `rev` must be specified for git dependencies. Tags are recommended for stable releases; branches track a moving target; revs pin an exact commit.

**Registry versions:** A bare string value (`name = "X.Y.Z"`) is a registry dependency — see [Package Registry](#package-registry) below. Registry dependencies pin an exact `major.minor.patch` version; version ranges (`~>`, `>=`, `^`, etc.) are deliberately not supported, so the resolved graph is always reproducible without a constraint solver. For a moving target or a pre-release commit, use a git dependency instead.

### `[application]` Section

For OTP application packages (created with `beamtalk new --app`):

```toml
[application]
supervisor = "MyAppSup"
```

This tells `beamtalk run` to start the package as a supervised OTP application. Library packages omit this section.

### `[native]` Section

For packages that depend on hex.pm Erlang packages:

```toml
[native.dependencies]
gun = "~> 2.1"
cowboy = "2.12.0"
```

Native dependencies are resolved via rebar3 and included on the BEAM code path. See [ADR 0072](ADR/0072-user-erlang-sources-in-packages.md) for details on Erlang interop within packages.

### `[stubs]` Section

Declares where a package's own FFI type stub files live, relative to the package root ([ADR 0075](ADR/0075-erlang-ffi-type-definitions.md)). A package declares stubs only for its own native Erlang code — not for its dependencies.

```toml
[stubs]
path = "stubs/"
```

Stubs are `.bt` files containing `declare native: <module>` forms — type-only declarations for Erlang module functions. When a package with a `[stubs]` section is used as a dependency, its stubs are automatically available to consumers.

`beamtalk build` and `beamtalk lint` resolve stubs in precedence order: project-local (`stubs/`) > package-bundled (dependencies' `[stubs]` paths) > distribution (`share/beamtalk/stubs/`, overridden by `BEAMTALK_STUBS_DIR` env var) > auto-extracted from `.beam` files. A stub for a specific function/arity overrides only that function at its layer; all other functions in the module keep their lower-layer types.

### `[diagnostics]` Section

Per-category diagnostic severity overrides (ADR 0100 Rule 3). Every key and the section itself are optional — an absent `[diagnostics]` section (the default for new packages) preserves the compiler's built-in completeness-ladder severities (ADR 0100 Rule 1: e.g. an unresolved selector on a fully-known class is a `Hint`, silent when the receiver's method surface can't be proven complete).

```toml
[diagnostics]
dnu = "hint"                # ADR 0100 Rule 1 default
unresolved-class = "warn"
unresolved-ffi = "warn"
arity-mismatch = "warn"
deprecation = "warn"
type = "hint"
```

Keys are kebab-case diagnostic categories (`dnu`, `type`, `unused`, `empty-body`, `lint`, `dead-assignment`, `extension-conflict`, `deprecation`, `actor-new`, `visibility`, `unresolved-class`, `unresolved-ffi`, `arity-mismatch`, `shadowed-class`, `type-annotation`, `inheritance`, `sendability`, `native-declaration-location`). Values are one of `"off"` (drop the diagnostic entirely), `"lint"`, `"hint"`, `"warn"`, or `"error"` (fails the build unconditionally, independent of `--warnings-as-errors`).

**Precedence (most-specific wins):**

1. A site-level `@expect dnu` / `@expect type` directive (ADR 0077) always wins — it silences the diagnostic regardless of this table.
2. This table sets the category's base severity for the package.
3. The Rule 1 completeness-ladder default, when the table doesn't mention the category.

`--warnings-as-errors` remains a *final promotion pass* over whatever 1–3 resolve to: it promotes `Warning`/`Hint` to `Error`, excluding the gradual-migration categories (`unresolved-class`, `unresolved-ffi`, `arity-mismatch`, `deprecation`) — **unless** the table explicitly sets that category, in which case the explicit value wins over the exclusion. This applies for *any* value you choose, not just `"warn"`: `unresolved-class = "hint"` also lifts the exclusion (so `--warnings-as-errors` fails the build), while `unresolved-class = "warn"` does the same. The only value that stays inert under `--warnings-as-errors` is `"lint"`, because `Severity::Lint` is never promoted — it's suppressed from normal build output entirely and shown only by `beamtalk lint`.

**Severity floor:** the table can only affect diagnostics the checker considers *soft* (`Hint`/`Warning`/`Lint`). A diagnostic that is already a hard structural error — e.g. `actor-new` (`Actor subclass` must use `spawn`, not `new`), `inheritance` (subclassing a sealed class), `empty-body`, `native-declaration-location` (`declare native:` outside `stubs/`) — is never touched by a table entry. Rule 3 is an escalation mechanism for the open-world completeness-ladder diagnostics (Rule 1); it cannot be used to silence a category that's a hard compile error by construction.

**Cross-surface parity:** `beamtalk build`/`check`, the LSP (BT-2800), and the REPL (BT-2839) all apply this table identically — a package that sets `dnu = "error"` fails the CLI build, shows the same site as an `Error` in the editor, *and* shows an `Error` at the REPL, never a soft hint. The LSP loads `beamtalk.toml` once per workspace root at startup; the REPL's compiler-port process loads it once per session from its working directory (the project root the REPL was started in). Edits to the `[diagnostics]` section while the server/session is running require an editor/LSP restart or a fresh REPL session to take effect.

**Want a provable DNU to fail your build, even on an ordinary single-receiver send?** ADR 0100 Rule 1's default (`Hint`) stays quiet even when the checker has fully proven a selector doesn't exist on a concrete, fully-loaded class with no `doesNotUnderstand:` handler — deliberately, to avoid turning today's large body of quiet hints into warnings the instant a project's cross-file/dependency knowledge becomes complete (see the ADR's "Hint vs Warning for the closed-complete case" tension point). That default has a real cost: a genuinely-broken send (e.g. two chained keyword messages accidentally merging into one selector no class implements) can stay a silent `Hint` until it's exercised at runtime. If your project would rather fail the build on a provable DNU than risk that, plain `--warnings-as-errors` already does it with no table entry at all — `dnu` isn't among the gradual-migration categories excluded from that promotion (`deprecation`, `unresolved-class`, `unresolved-ffi`, `arity-mismatch`), so its default `Hint` promotes straight to `Error`. Set `dnu = "error"` instead if you want that failure unconditionally, even without the flag — this table is exactly the opt-in escalation lever ADR 0100 Rule 3 reserves for that choice.

See [ADR 0100](ADR/0100-open-world-diagnostic-policy.md) for the full policy rationale.

## Dependency Management CLI

The `beamtalk deps` subcommand manages dependencies declared in `beamtalk.toml`.

### Adding Dependencies

```bash
# Add a registry dependency, pinned to the latest published version
beamtalk deps add yaml

# Add a registry dependency, pinned to a specific version
beamtalk deps add yaml --version 0.2.1

# Add a path dependency
beamtalk deps add utils --path ../my-utils

# Add a git dependency pinned to a tag
beamtalk deps add json --git https://github.com/jamesc/beamtalk-json --tag v1.0.0

# Add a git dependency tracking a branch
beamtalk deps add http --git https://github.com/someone/beamtalk-http --branch main

# Add a git dependency pinned to a commit
beamtalk deps add crypto_utils --git https://github.com/someone/bt-crypto --rev abc1234
```

`deps add` writes the entry to `beamtalk.toml`, resolves the dependency (cloning git repos, validating path deps, or resolving a registry version through the index — see [Package Registry](#package-registry)), and updates the lockfile. With none of `--path`/`--git`/`--version` given, the name is looked up in the registry and pinned to its latest published release.

### Listing Dependencies

```bash
beamtalk deps list
```

Shows all resolved dependencies with their sources and pinned versions:

```text
json  v1.0.0  (git: github.com/jamesc/beamtalk-json @ abc1234)
utils 0.1.0   (path: ../my-utils)
yaml  0.2.1   (registry: github.com/jamesc/beamtalk-yaml tag: v0.2.1 @ fed4321)
```

A registry dependency's row shows the resolved git URL, tag, and locked commit SHA behind the version, the same provenance a git dependency's row shows.

### Updating Dependencies

```bash
beamtalk deps update          # Update all git and registry dependencies
beamtalk deps update json     # Update a single dependency
```

Advances git dependencies to the latest commit matching their spec (the latest tag, the head of the branch, etc.). For a registry dependency, `deps update` refreshes the registry index and re-resolves the dependency's *already-pinned* version to a fresh commit SHA — picking up a moved tag or a corrected index entry. It does not change which version is pinned; to take a newer release, edit the version string directly in `beamtalk.toml` (`yaml = "0.3.0"`) and run `beamtalk deps update yaml` (or any build) to re-resolve it. Either way, the lockfile is updated.

### Manual Editing

You can always edit `beamtalk.toml` directly instead of using `beamtalk deps add`. The CLI commands are convenience wrappers — the manifest file is the source of truth.

## Package Registry

A **registry dependency** (`yaml = "0.2.1"` in `[dependencies]`) is resolved through a *registry index* into a `(git url, tag)` pair, which then flows through the same git-dependency machinery a `{ git = ..., tag = ... }` entry uses — cloned into `_build/deps/`, pinned to an exact commit SHA in `beamtalk.lock`. The registry itself never hosts source code; it is only a lookup table from `(name, version)` to a git repository and tag.

### Registry Location

The index location is resolved in priority order:

1. the `BEAMTALK_REGISTRY` environment variable
2. `[registry] url` in the project's `beamtalk.toml`
3. the default registry, `https://github.com/jamesc/beamtalk-registry`

```toml
[registry]
url = "https://github.com/jamesc/beamtalk-registry"
```

```bash
# Override for a single command, without touching beamtalk.toml
BEAMTALK_REGISTRY=https://github.com/my-org/internal-registry beamtalk deps add yaml
```

A value naming an existing local directory is read in place — no git, no network; a relative path is resolved against the project root (not the process's current directory), so the same `beamtalk.toml` names the same registry regardless of where a surface (CLI, LSP, MCP) happens to run from. Anything else is treated as a git URL and cloned into a shared, user-level cache, refreshed only on a lookup miss (a package or version not found in the on-disk clone triggers one retry against a freshly pulled index, so a dependency published moments ago resolves without a manual step). A local-directory registry is useful for CI fixtures, air-gapped environments, and testing a not-yet-published package before pushing its index entry.

#### Index cache location

A git-backed index is cloned once per *registry*, not once per *project*: every project pointing at the same registry URL shares one clone under `~/.beamtalk/registry/<hash of the URL>/`, the same way Cargo and Gleam share their registry caches, instead of each project cloning its own copy into `_build/`. Concurrent beamtalk processes (a CLI `build` alongside the LSP server, an MCP `lint` mid-refresh) take an advisory lock around the shared clone, so none of them ever observes a half-refreshed index.

`BEAMTALK_REGISTRY_CACHE_DIR` overrides the cache *root* — a hash of the registry URL is still appended underneath it (same as the default location), so two different registry URLs pointed at the same override never clobber each other's clone. A relative value (e.g. `_build/registry`, for a per-project cache close to the pre-BT-2996 layout) is resolved against the project root, not the process's current directory — the same rule `BEAMTALK_REGISTRY` follows above — so the CLI, LSP and MCP server still agree on one absolute cache directory, and therefore one advisory lock file, for the same project even when they don't share a working directory. An absolute path (e.g. a team-wide mount, to share a pre-warmed clone across machines) is used as-is. A stale or corrupt cache is cleared by deleting its directory (or `~/.beamtalk/registry/` entirely, or `_build/registry/` via `beamtalk clean --deps` when the override is in effect) — it is rebuilt from a fresh clone on the next lookup.

### Index Format

The index is a directory (typically a git repository) containing one TOML file per package under `packages/`. Copy this template for a new package's first release:

```toml
# packages/<name>.toml
name = "<name>"
description = "One-line description of the package"

[[versions]]
version = "0.1.0"
git = "https://github.com/<owner>/beamtalk-<name>"
tag = "v0.1.0"   # optional — defaults to "v" + the version field (e.g. "v0.1.0")
```

A package with multiple published releases simply has more `[[versions]]` blocks, oldest first:

```toml
name = "yaml"
description = "YAML parsing for Beamtalk"

[[versions]]
version = "0.1.0"
git = "https://github.com/jamesc/beamtalk-yaml"
tag = "v0.1.0"

[[versions]]
version = "0.2.1"
git = "https://github.com/jamesc/beamtalk-yaml"
tag = "v0.2.1"
```

Unknown top-level or per-version keys are accepted (not rejected) — the index is a separately versioned artifact served to clients this binary doesn't control, so a future index format can add fields like `yanked` or `checksum` without breaking older `beamtalk` binaries reading it.

### Browsing the Registry

The default registry is browsable at **[www.beamtalk.dev/registry/](https://www.beamtalk.dev/registry/)** — published alongside the main docs site, on every push that touches the generator and on a daily schedule (to pick up packages published to the registry index between deploys of this repo). It's an index page of every published package with its latest version, and a per-package page listing every published version with its git repo link, tag, and a copy-paste `name = "x.y.z"` dependency snippet.

The site is generated from any registry index — the default or a self-hosted one — with:

```bash
beamtalk registry site --index <path-or-git-url> --output <dir>
```

Like `beamtalk doc --site`, this is a purely static, read-only renderer (hand-rolled HTML/CSS, no server) — `--index` accepts the same kind of value `BEAMTALK_REGISTRY`/`[registry] url` do: an existing local directory is read in place, anything else is treated as a git URL and cloned. Useful for previewing a self-hosted registry's site before publishing it, or for regenerating `www.beamtalk.dev/registry/` locally.

### Hosting Your Own Registry

A registry index is nothing more than a git repository with a `packages/` directory — there is no server, database, or publishing service to run. To host one:

1. Create a new (usually public) git repository, e.g. `github.com/your-org/beamtalk-registry`.
2. Add an empty `packages/` directory (a `.gitkeep` file, or the first package's `.toml`) and commit it.
3. Point consumers at it via `[registry] url = "https://github.com/your-org/beamtalk-registry"` in their `beamtalk.toml`, or `BEAMTALK_REGISTRY` for a one-off override.
4. Authors publish releases with `beamtalk publish` (below) — each run appends a `[[versions]]` block (or creates the package's first `packages/<name>.toml`) and pushes the commit, so the registry needs no maintainer intervention for ordinary releases.

#### Bootstrapping `jamesc/beamtalk-registry`

The default registry (`https://github.com/jamesc/beamtalk-registry`) is bootstrapped the same way any self-hosted registry is — there is nothing Beamtalk-specific about creating it beyond the `packages/` layout above. A minimal `README.md` for the index repo:

```markdown
# beamtalk-registry

The default package registry index for [Beamtalk](https://github.com/jamesc/beamtalk).

This repository has no code — it is a lookup table from package name + version to a
git repository and tag, read by the Beamtalk CLI's `deps` and `publish` commands. See
[Package Registry](https://github.com/jamesc/beamtalk/blob/main/docs/beamtalk-packages.md#package-registry)
in the main repo's docs for the full format.

## Publishing

Publish a release from your package's own repository — do not edit this repository
by hand:

    beamtalk version bump minor   # or `beamtalk version X.Y.Z`
    git add beamtalk.toml && git commit -m "release 0.3.0"
    beamtalk publish

`beamtalk publish` tags your repository, pushes the tag, and opens (or updates)
`packages/<your-package>.toml` here on your behalf, provided you have push access
to this repository (or the `[registry] url` in your project points at a fork or
private mirror you do have access to).

## Layout

    packages/
      <name>.toml   # one file per published package — see the template linked above
```

Repository settings worth setting up before the first publish: branch protection on the default branch is *not* required — `beamtalk publish` pushes a plain commit, not a PR — but requiring signed commits or a CI check that validates every `packages/*.toml` still parses (`toml::from_str`, matching name field, no duplicate versions — the same checks `beamtalk` itself applies when reading an entry) catches a hand-edited or corrupted entry before it reaches consumers.

### Author Workflow

Publishing a new version of a package you maintain:

```bash
beamtalk version bump minor      # or: beamtalk version 0.3.0
git add beamtalk.toml && git commit -m "release 0.3.0"
beamtalk publish
```

`beamtalk version` shows, sets, or bumps the `[package] version` field in your own `beamtalk.toml` (a surgical text edit — comments and formatting elsewhere in the file are untouched):

```bash
beamtalk version               # print the current version
beamtalk version 0.3.0         # set an exact version (must be greater than current)
beamtalk version bump patch    # 1.2.3 -> 1.2.4
beamtalk version bump minor    # 1.2.3 -> 1.3.0
beamtalk version bump major    # 1.2.3 -> 2.0.0
```

`beamtalk publish` then, for the version currently in `beamtalk.toml`:

1. **Preflights:** a clean git working tree, an `origin` remote, the release tag (`vX.Y.Z`) absent both locally and on `origin`, and the version not already recorded in the registry index (the index is refreshed first so this check is never stale).
2. Creates an annotated tag `vX.Y.Z` and pushes it to `origin` — `origin`'s URL becomes the index entry's `git` field.
3. Updates the registry index: creates `packages/<name>.toml` for a package's first release, or appends a `[[versions]]` block for a subsequent one, then commits and pushes the change (skipped, with a note, when the registry is a plain local directory rather than a git checkout).

```bash
beamtalk publish --dry-run     # print what would happen without tagging, pushing, or writing anything
```

**If step 3 fails** (stage, commit, or push), the tag from step 2 is already live on `origin`. `beamtalk publish`'s error message names exactly what's left to do — if the *push* failed, `git pull --rebase && git push` from the registry index's cache directory (see [Index cache location](#index-cache-location) above — `~/.beamtalk/registry/<hash of the URL>/index/` by default, or `<BEAMTALK_REGISTRY_CACHE_DIR>/<hash of the URL>/index/` when that env var overrides the cache root) (a plain `git push` suffices for a transient network error; `git pull --rebase` is needed if another author pushed to the same registry concurrently — if the rebase itself conflicts, resolve the conflict in `packages/<name>.toml`, then `git add packages/<name>.toml && git rebase --continue && git push`); if staging or the *commit* failed instead, there is no local commit yet, so `git add . && git commit -m "registry: <name> vX.Y.Z"` there first (substituting your package name and version, e.g. `registry: yaml v0.2.1`), then `git push`. Do **not** re-run `beamtalk publish` or bump the version in either case: the tag already exists, so a retry reports this same partial-failure message again — it recognizes the version isn't in the index yet and won't suggest bumping — and bumping the version anyway would permanently abandon the release you already tagged (BT-3000).

**If step 2's tag push itself fails** (before step 3 runs at all), the annotated tag exists locally but not on `origin` — the opposite situation. Here the preflight's *local-tag check* rejects a retry with "Tag 'vX.Y.Z' already exists locally." Delete it first with `git tag -d vX.Y.Z`, then re-run `beamtalk publish`.

A registry dependency and the package it names always agree on which registry is authoritative — `beamtalk publish` resolves the target registry through the exact same `BEAMTALK_REGISTRY` → `[registry] url` → default chain that consuming a registry dependency does, using the *library's own* `beamtalk.toml`.

## Lockfile (`beamtalk.lock`)

The lockfile pins exact commit SHAs for git dependencies, ensuring reproducible builds across machines and CI environments. It is auto-generated on first resolve and updated by `beamtalk deps update`.

```text
# This file is auto-generated by beamtalk. Do not edit manually.
# It pins exact versions of dependencies for reproducible builds.

[[package]]
name = "json"
url = "https://github.com/jamesc/beamtalk-json"
reference = "tag:v1.0.0"
sha = "abc1234def5678..."

# A registry dependency additionally records the exact version requested in
# beamtalk.toml, so a resolve that finds a matching lock entry can skip the
# registry index entirely and go straight to the git checkout.
[[package]]
name = "yaml"
version = "0.2.1"
url = "https://github.com/jamesc/beamtalk-yaml"
reference = "tag:v0.2.1"
sha = "fed4321cba9876..."

[[native_package]]
name = "gun"
version = "2.1.3"
sha = "def456..."
```

**Key points:**
- **Commit to version control.** The lockfile should be checked into git so all developers and CI use the same dependency versions.
- **Path dependencies are not locked.** They resolve to whatever is on disk, so they are only reproducible within a single repository checkout.
- **Registry dependencies lock like git dependencies**, plus the requested `version` field shown above.
- **Implicit fetch on build.** `beamtalk build`, `beamtalk test`, and `beamtalk repl` automatically fetch and compile dependencies if the lockfile is missing or stale. No separate "deps get" step is needed.

## Qualified Names (`package@Class`)

When two dependencies export the same class name, or when you want to be explicit about where a class comes from, use the qualified name syntax:

```beamtalk
Object subclass: MyApp
  convert: input =>
    jsonTree := json@Parser parse: input.
    xmlTree := xml@Parser parse: input.
    jsonTree
```

The `@` separator matches the BEAM module naming convention (`bt@json@parser`) and is visually distinctive.

### Where Qualified Names Work

Qualified names are accepted everywhere a class name is valid:

```beamtalk
// Message sends
json@Parser parse: '{"key": "value"}'

// Subclassing
json@Parser subclass: LenientParser
  // ...

// Type annotations
parse: input :: String -> json@Parser =>
  json@Parser parse: input
```

### When Qualified Names Are Required

If two dependencies export the same class name and your code references that name, the compiler emits an error:

```text
error[E0301]: Class name 'Parser' is exported by multiple dependencies
  --> src/app.bt:5:12
   |
   = note: 'Parser' is defined in package 'json' (json@Parser)
   = note: 'Parser' is defined in package 'xml' (xml@Parser)
   = help: use qualified name: json@Parser or xml@Parser
```

The collision is detected lazily — only when you actually reference the ambiguous name. If two dependencies both export `Parser` but your code never uses it, there is no error.

## Collision Detection

Beamtalk prevents silent class name shadowing. The compiler checks for collisions at two levels:

1. **Cross-package collisions:** Two dependencies exporting the same class name is an error at the use site, resolvable with qualified names.
2. **Stdlib reservation:** Dependency classes cannot shadow stdlib names (`Integer`, `String`, `Actor`, `Object`, `Value`, etc.). A dependency exporting a reserved name triggers a compile error.

## Transitive Dependencies

When your package depends on `json` and `json` depends on `utils`, classes from `utils` are visible in your code — the runtime loads them onto the BEAM code path. However, using a transitive dependency's class without declaring it directly emits a **warning**:

```text
warning[W0302]: Class 'StringUtils' is from transitive dependency 'utils' (via 'json')
  --> src/app.bt:5:12
   |
   = help: add 'utils' to [dependencies] in beamtalk.toml to make this explicit
```

This preserves Smalltalk's "you can always reach the object" philosophy while nudging toward explicit declarations.

### Strict Mode

Library authors who need clean dependency boundaries can opt into strict checking:

```toml
[package]
name = "my_library"
strict-deps = true
```

With `strict-deps = true`, using a transitive dependency's class becomes a **compile error** instead of a warning. This is recommended for published libraries to ensure their dependencies are fully declared.

## Directory Structure

A typical package looks like this:

```text
my_app/
├── beamtalk.toml           # Package manifest
├── beamtalk.lock           # Dependency lockfile (generated)
├── src/                    # Source files
│   ├── Main.bt             # Entry point (by convention)
│   └── Counter.bt          # Additional classes
├── test/                   # BUnit tests
│   └── CounterTest.bt
├── _build/                 # Build output (generated)
│   └── dev/
│       └── ebin/           # .beam files + .app file
├── Justfile                # Build/test/CI targets
├── AGENTS.md               # AI agent guide (generated)
├── .mcp.json               # MCP server config (generated)
└── .gitignore
```

Source files in `src/` compile to BEAM modules named `bt@{package}@{class}`:

```text
src/Counter.bt    → bt@my_app@counter
src/Main.bt       → bt@my_app@main
src/util/Math.bt  → bt@my_app@util@math
```

Subdirectories within `src/` are namespacing only — they do not create subpackages. The entire `src/` tree belongs to one flat package.

## Build Behavior

### Package Mode (manifest found)

When `beamtalk build` runs in a directory with `beamtalk.toml`:

```bash
$ cd my_app
$ beamtalk build
Building my_app v0.1.0
  Compiling Counter.bt → bt@my_app@counter
  Compiling Main.bt → bt@my_app@main
  Generating my_app.app
Build complete: 2 modules in _build/dev/ebin/
```

The build system:
1. Reads `beamtalk.toml` for package name and version
2. Fetches and compiles dependencies (if needed)
3. Discovers `.bt` files in `src/`
4. Compiles each with the `bt@{package}@` prefix
5. Generates an OTP `.app` file with module list and class metadata
6. Writes output to `_build/dev/ebin/` relative to the package root

### File Mode (no manifest)

```bash
$ beamtalk build script.bt
  Compiling script.bt → script
Build complete: 1 module in build/
```

Single-file compilation works without a manifest — no package prefix, output to `build/`. This is preserved for quick scripting and experimentation.

## REPL Integration

When the REPL starts in a package directory, all dependencies are automatically loaded onto the code path:

```bash
$ cd my_app
$ beamtalk repl
```

- Dependency classes are immediately available by name
- Qualified names (`json@Parser`) work in expressions, `:help`, and chain completion
- Ambiguous names produce a warning (not an error) with a hint to use the qualified form — the REPL is exploratory, so hard errors would break the interactive flow

## Complete Example

Here is a complete example of a package that depends on a local utility library:

**`my_app/beamtalk.toml`:**
```toml
[package]
name = "my_app"
version = "0.1.0"
description = "Example application"

[dependencies]
utils = { path = "../my-utils" }

[application]
supervisor = "MyAppSup"
```

**`my-utils/beamtalk.toml`:**
```toml
[package]
name = "utils"
version = "0.1.0"
description = "Shared utilities"
```

**`my_app/src/Main.bt`:**
```beamtalk
Object subclass: Main
  /// Entry point — called by the application supervisor.
  class start =>
    // StringHelper is from the 'utils' package — available because
    // we declared it as a dependency in beamtalk.toml
    greeting := StringHelper capitalize: "hello, world"
    Console println: greeting
```

## Design References

The package system is specified in these Architecture Decision Records:

- [ADR 0016](ADR/0016-unified-stdlib-module-naming.md) — BEAM module naming with `@` separator
- [ADR 0026](ADR/0026-package-definition-and-project-manifest.md) — Package definition and `beamtalk.toml` format
- [ADR 0070](ADR/0070-package-namespaces-and-dependencies.md) — Package namespaces, dependencies, qualified names, and collision detection
- [ADR 0072](ADR/0072-user-erlang-sources-in-packages.md) — User Erlang sources in packages
- [ADR 0100](ADR/0100-open-world-diagnostic-policy.md) — Open-world diagnostic policy; the `[diagnostics]` section's severity-override schema (Rule 3)

The [Package Registry](#package-registry) section above (registry dependency resolution, `deps add`/`update` registry support, and the `beamtalk version`/`beamtalk publish` author workflow) implements the registry phase ADR 0070 explicitly deferred ("out of scope for this ADR"; see its Alternatives section, "Registry-Based Dependencies"). It shipped as Linear epic BT-2977 (BT-2978, BT-2979, BT-2980) without a dedicated ADR — the git-repository-as-index design and its module-level rationale are documented in `crates/beamtalk-cli/src/commands/deps/registry.rs`, `publish.rs`, and `version.rs`.
