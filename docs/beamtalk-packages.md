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

Dependencies are declared with two source types — path and git:

```toml
[dependencies]
# Local path — for monorepos and local development
utils = { path = "../my-utils" }

# Git repository — pinned to a tag, branch, or commit
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
http = { git = "https://github.com/someone/beamtalk-http", branch = "main" }
crypto_utils = { git = "https://github.com/someone/bt-crypto", rev = "abc1234" }
```

Each dependency must point to a directory containing its own `beamtalk.toml`. Dependencies are resolved transitively — if `json` depends on `utils`, both are compiled and available.

**Git references:** Exactly one of `tag`, `branch`, or `rev` must be specified for git dependencies. Tags are recommended for stable releases; branches track a moving target; revs pin an exact commit.

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

## Dependency Management CLI

The `beamtalk deps` subcommand manages dependencies declared in `beamtalk.toml`.

### Adding Dependencies

```bash
# Add a path dependency
beamtalk deps add utils --path ../my-utils

# Add a git dependency pinned to a tag
beamtalk deps add json --git https://github.com/jamesc/beamtalk-json --tag v1.0.0

# Add a git dependency tracking a branch
beamtalk deps add http --git https://github.com/someone/beamtalk-http --branch main

# Add a git dependency pinned to a commit
beamtalk deps add crypto_utils --git https://github.com/someone/bt-crypto --rev abc1234
```

`deps add` writes the entry to `beamtalk.toml`, resolves the dependency (cloning git repos, validating path deps), and updates the lockfile.

### Listing Dependencies

```bash
beamtalk deps list
```

Shows all resolved dependencies with their sources and pinned versions:

```text
json  v1.0.0  (git: github.com/jamesc/beamtalk-json @ abc1234)
utils 0.1.0   (path: ../my-utils)
```

### Updating Dependencies

```bash
beamtalk deps update          # Update all git dependencies
beamtalk deps update json     # Update a single dependency
```

Advances git dependencies to the latest commit matching their spec (the latest tag, the head of the branch, etc.) and updates the lockfile.

### Manual Editing

You can always edit `beamtalk.toml` directly instead of using `beamtalk deps add`. The CLI commands are convenience wrappers — the manifest file is the source of truth.

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

[[native_package]]
name = "gun"
version = "2.1.3"
sha = "def456..."
```

**Key points:**
- **Commit to version control.** The lockfile should be checked into git so all developers and CI use the same dependency versions.
- **Path dependencies are not locked.** They resolve to whatever is on disk, so they are only reproducible within a single repository checkout.
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
