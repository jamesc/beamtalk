# Tooling

Tooling is part of the language, not an afterthought. Beamtalk is designed to be used interactively — by humans and AI agents alike — through a live workspace that all tools connect to over the same WebSocket protocol.

## CLI Tools

```bash
# Project management
beamtalk new mylib          # Create new library project
beamtalk new myapp --app    # Create new application project (supervisor + Main)
beamtalk build              # Compile to BEAM
beamtalk run                # Compile and start
beamtalk check              # Check for errors without compiling

# Dependencies (see Package Management guide)
beamtalk deps add json --git https://github.com/jamesc/beamtalk-json --tag v1.0.0
beamtalk deps list          # Show resolved dependencies
beamtalk deps update        # Update lockfile

# Development
beamtalk repl               # Interactive REPL (connects to workspace)
beamtalk test               # Run test suite

# Native Erlang
beamtalk gen-native MyActor # Generate skeleton gen_server from a native: Actor class
```

For full details on `beamtalk.toml`, dependencies, lockfiles, qualified names, and collision detection, see the [Package Management](beamtalk-packages.md) guide.

### Project Types

`beamtalk new` creates a **library** by default — a package with source and tests, no application supervisor. Pass `--app` to create an **application** project with a supervisor and `Main` entry point.

| Variant | Command | What you get |
|---------|---------|--------------|
| Library (default) | `beamtalk new foo` | `beamtalk.toml`, `src/Foo.bt`, `test/FooTest.bt` |
| Application | `beamtalk new foo --app` | Above plus `[application]` in toml, `src/FooAppSup.bt`, `src/Main.bt` |

Both variants generate a `Justfile`, `.github/workflows/ci.yml`, `AGENTS.md`, and `.mcp.json`.

### Standard Justfile Targets

Every generated project includes a `Justfile` with these targets:

| Target | Command | Description |
|--------|---------|-------------|
| `build` | `beamtalk build` | Compile the project |
| `test` | `beamtalk test` | Run the test suite |
| `fmt` | `beamtalk fmt --check` | Check formatting |
| `fmt-fix` | `beamtalk fmt` | Format in place |
| `ci` | `fmt build test` | Full CI check |
| `release` | *(script)* | Tag a release from `beamtalk.toml` version |
| `publish` | `git push origin --tags` | Push release tags |
| `run` | `beamtalk run` | Run the application (**`--app` only**) |

## Build System

`beamtalk build` compiles a project (or a single `.bt` file) to BEAM bytecode. The build pipeline is incremental — unchanged files are skipped, and the compiler caches class metadata across builds.

### Build Phases

Every build runs through three sequential phases:

1. **Pass 1 (Discovery)** — Read each `.bt` source file and parse it into an AST. Extract class names, superclass relationships, and method signatures. This produces the `class_module_index` (class name to BEAM module name) and `class_superclass_index` (class to superclass) needed for cross-file resolution in Pass 2. Dependency validation (stdlib reservation checks, native module collision detection) also runs here.

2. **Pass 2 (Compile)** — For each changed source file: run semantic analysis (type checking, protocol conformance, dependency warnings), then generate Core Erlang. The full class-module index from Pass 1 is injected so cross-file class references resolve correctly. Each `.bt` file produces a `.core` file in the build directory.

3. **BEAM compilation (`erlc`)** — Invoke OTP's `compile` module to turn each `.core` file into a `.beam` file. This is the standard Erlang compiler — Beamtalk delegates BEAM bytecode generation entirely to OTP.

### Incremental Compilation

Beamtalk uses mtime-based change detection to avoid redundant work:

- Each `.bt` source file is compared against its corresponding `.beam` output. If the source is newer (or no `.beam` exists), the file is recompiled.
- **Pass 1 caching** — The class-module index from Pass 1 is cached in `_build/dev/ebin/.beamtalk-pass1-cache.json`. On subsequent builds, only files whose source has changed since the cache was written are re-parsed. Unchanged files reuse their cached class metadata. If the `beamtalk.toml` manifest changes, the entire cache is invalidated and all files are re-parsed.
- **Force rebuild** — `beamtalk build --force` ignores all caches and recompiles everything. Useful after toolchain upgrades or when build artifacts may be stale (e.g. switching git branches or worktrees).
- **Orphan detection** — `.beam` files with no corresponding `.bt` source are reported as warnings (the source may have been deleted or renamed).

### Build Artifact Layout (`BuildLayout`)

All build output goes under `_build/` in the project root. The `BuildLayout` struct centralises path construction so all commands (build, test, run, repl, deps) use consistent locations.

```text
<project_root>/
  _build/
    dev/
      ebin/                        — compiled .beam files from .bt sources
        .beamtalk-pass1-cache.json — Pass 1 incremental cache
      native/
        ebin/                      — compiled .beam files from native .erl sources
        include/                   — generated headers (e.g. beamtalk_classes.hrl)
        default/lib/               — rebar3 hex dependency libs (ERL_LIBS)
        rebar.config               — generated rebar3 config (when hex deps exist)
    deps/
      <name>/                      — git dependency checkout
        ebin/                      — compiled .beam files for the dependency
```

For single-file builds (no `beamtalk.toml`), output goes to `build/` alongside the source file instead of `_build/dev/ebin/`.

### Module Naming

Beamtalk uses a namespaced BEAM module naming scheme to avoid collisions in the flat BEAM module namespace:

| Mode | Module name | Example |
|------|-------------|---------|
| Package (`beamtalk.toml` present) | `bt@<package>@<relative_path>` | `bt@myapp@Counter` |
| Single file (no manifest) | `bt@<module>` | `bt@Counter` |

Subdirectories within `src/` are reflected in the module name: `src/models/User.bt` in package `myapp` becomes `bt@myapp@models@User`.

### Dependency Compilation Order

When a project has dependencies (declared in `beamtalk.toml`), the build resolves and compiles the full transitive dependency graph in topological order before compiling the project's own sources. Each dependency's class-module index is merged into the project's index so cross-package class references resolve during codegen.

Native Erlang dependencies (`.erl` files under `native/` or hex packages) are compiled after Pass 1 but before Pass 2. This ordering ensures the generated `beamtalk_classes.hrl` header (which maps Beamtalk class names to BEAM module names) is available for native `.erl` files to include.

### `__beamtalk_meta/0` Protocol

Every compiled Beamtalk class module exports a `__beamtalk_meta/0` function — a zero-process reflection entry point that returns a map of class metadata without requiring any gen_server process to be running:

```erlang
Module:'__beamtalk_meta'() ->
    #{
        class             => 'Counter',
        superclass        => 'Actor',
        meta_version      => 2,
        is_sealed         => false,
        is_abstract       => false,
        is_value          => false,
        fields            => [count],
        field_types       => #{count => 'Integer'},
        methods           => [{increment, 1}, {value, 0}],
        class_methods     => [{new, 0}],
        method_info       => #{...},
        class_method_info => #{...}
    }
```

This metadata serves multiple purposes:
- **Incremental builds**: The compiler server caches `__beamtalk_meta/0` output for all loaded classes, injecting it into each compilation request so the type checker can validate cross-class method calls on user-defined classes (see [ADR 0050](ADR/0050-incremental-compiler-class-hierarchy.md)).
- **Crash recovery**: When the compiler port restarts, the server scans all loaded BEAM modules and repopulates its class cache from `__beamtalk_meta/0` — no persistent state files needed.
- **Tooling**: Debuggers, documentation generators, and the reflection API (`Beamtalk allClasses`, `Beamtalk help:`) can query class structure without going through a class process.

### Hot-Patching

When a `.bt` file is reloaded (via `Workspace load:`, `:reload`, or a VS Code file save), the compiler produces new BEAM bytecode and the runtime hot-loads it using standard OTP code loading (`code:load_binary/3`). The class gen_server process updates its live state to reflect the new methods — hot-patched methods take effect on the next message send without restarting the actor.

The compiler server is also notified of the change so its class cache stays current. Since `__beamtalk_meta/0` is a compiled function baked into the module, it reflects the original compilation — not hot-patches. The gen_server's `class_state` record is the authoritative live view; the compiler server receives updates synthesised from `class_state`, not from the stale `__beamtalk_meta/0`.

### Embedded Compiler Architecture

The Beamtalk compiler (`beamtalk-core`, written in Rust) runs as an OTP Port managed by `beamtalk_compiler_server` — a supervised gen_server inside the BEAM node ([ADR 0022](ADR/0022-embedded-compiler-via-otp-port.md)). This replaces the older separate-daemon architecture and provides:

- **Supervised lifecycle** — The compiler port is restarted automatically on crash. No orphaned socket files or manual recovery.
- **Cross-platform** — OTP Ports work on all platforms (Linux, macOS, Windows). No Unix-specific IPC.
- **Stateless port** — The Rust compiler binary is a pure function: source code + metadata in, Core Erlang out. All session state (class cache, known variables) lives in the Erlang gen_server and is injected per request via ETF (Erlang Term Format) over length-prefixed frames.

## REPL

The REPL connects to a persistent workspace — a detached BEAM node that survives disconnections ([ADR 0004](ADR/0004-persistent-workspace-management.md)). Actors keep running between sessions; REPL variable bindings are session-local.

```beamtalk
// Spawn and interact
counter := Counter spawn
counter increment
counter getValue  // => 1

// Live-patch a method — takes effect on next message send
Counter >> increment =>
  Transcript show: "incrementing"
  self.value := self.value + 1

counter increment  // prints "incrementing", returns 2
```

### REPL `:` Commands

`:` commands are thin wrappers around native message sends:

| REPL shortcut | Native equivalent |
|---------------|-------------------|
| `:load path/to/file.bt` | `Workspace load: "path/to/file.bt"` |
| `:reload Counter` | `Counter reload` |
| `:modules` | `Workspace classes` |
| `:test` | `Workspace test` |
| `:test CounterTest` | `Workspace test: CounterTest` |
| `:help Counter` | `Beamtalk help: Counter` |
| `:help Counter increment` | `Beamtalk help: Counter selector: #increment` |

The native forms work from compiled code, scripts, and actor methods — not just the REPL.

### Multi-line Input

The REPL automatically detects incomplete input (unclosed brackets, trailing operators, etc.) and shows a `..>` continuation prompt. For constructs where the REPL cannot determine completeness from syntax alone — protocol definitions and class definitions without methods — press **Enter on a blank line** to submit the accumulated input.

```beamtalk
> Protocol define: Greetable
..>   greet -> String
..>                          ← blank line submits the definition
Protocol Greetable defined
```

Class definitions with at least one method (`=>`) auto-submit. A blank line is only needed for class headers with state declarations only (before you have added methods):

```beamtalk
> Actor subclass: Config
..>   state: verbose = false
..>                          ← blank line submits the class
```

Use **Ctrl+C** to cancel multi-line input without submitting.

### Workspace and Reflection Singletons

Two global objects provide introspection and project operations:

**`Beamtalk`** — system reflection:
```beamtalk
Beamtalk version             // => "0.3.1"
Beamtalk allClasses          // All registered class names
Beamtalk classNamed: #Counter // Look up a class by name
Beamtalk globals             // Snapshot of system namespace
Beamtalk help: Counter       // Formatted documentation
```

**`Workspace`** — project operations:
```beamtalk
Workspace load: "examples/counter.bt"  // Compile and hot-load
Workspace classes            // All loaded user classes
Workspace actors             // All live actors
Workspace actorsOf: Counter  // All Counter instances
Workspace test               // Run all test classes
Workspace test: CounterTest  // Run specific test class
Workspace bind: myActor as: #MyTool  // Register for later lookup
```

## MCP Server (AI Agent Interface)

Beamtalk includes an MCP (Model Context Protocol) server that gives AI coding agents structured access to the live workspace. The MCP tools connect via the same WebSocket protocol as the REPL and VS Code extension — the agent interacts with the same running system.

| MCP Tool | Description |
|----------|-------------|
| `evaluate` | Run any Beamtalk expression in the workspace |
| `load_file` | Compile and hot-reload a `.bt` file |
| `load_project` | Load all project files in dependency order |
| `list_modules` | List loaded BEAM modules |
| `list_classes` | List loaded Beamtalk classes (with optional filter) |
| `docs` | Query class/method documentation |
| `test` | Run tests (all, by class, or by file) |
| `search_examples` | Search the bundled example corpus |
| `search_classes` | Search class names and documentation |
| `enable-tracing` | Enable detailed trace event capture for actor dispatch |
| `disable-tracing` | Disable trace event capture (aggregate stats remain) |
| `get-traces` | Retrieve trace events with optional filters (actor, selector, class, outcome, duration) |
| `export-traces` | Export trace events to a JSON file |
| `actor-stats` | Get aggregate per-actor, per-method statistics (call counts, durations, error rates) |

All MCP tools map to the same `Workspace` and `Beamtalk` APIs available in the REPL. The tracing tools correspond to the `Tracing` stdlib class (see [Language Features — Actor Observability](beamtalk-language-features.md#actor-observability-and-tracing-adr-0069)).

## VS Code Extension

The [Beamtalk VS Code extension](https://github.com/jamesc/beamtalk/tree/main/editors/vscode) connects to the live workspace and provides:

- **Syntax highlighting** for `.bt` files
- **LSP integration** — diagnostics, completions, go-to-definition
- **Workspace tree view** — live actors, loaded classes, and session bindings in the sidebar
- **Transcript view** — real-time output from the workspace
- **Inspector panels** — inspect live actor state
- **Workspace status** — connected/disconnected indicator

The extension connects to the same workspace as the REPL and MCP server. Changes made in VS Code (file saves trigger hot-reload) are immediately visible in the REPL and to AI agents.

## Testing Framework

Beamtalk includes a native test framework inspired by Smalltalk's SUnit.

```beamtalk
// stdlib/test/counter_test.bt

TestCase subclass: CounterTest

  testInitialValue =>
    self assert: (Counter spawn getValue) equals: 0

  testIncrement =>
    self assert: (Counter spawn increment) equals: 1

  testMultipleIncrements =>
    counter := Counter spawn
    3 timesRepeat: [counter increment]
    self assert: (counter getValue) equals: 3
```

Each test method gets a fresh instance with `setUp` → test → `tearDown` lifecycle.

### Assertion Methods

| Method | Description | Example |
|--------|-------------|---------|
| `assert:` | Assert condition is true | `self assert: (x > 0)` |
| `assert:equals:` | Assert two values are equal | `self assert: result equals: 42` |
| `deny:` | Assert condition is false | `self deny: list isEmpty` |
| `should:raise:` | Assert block raises error | `self should: [1 / 0] raise: #badarith` |
| `fail:` | Unconditional failure | `self fail: "not implemented"` |

### Running Tests

```text
// From the REPL:
> :load stdlib/test/counter_test.bt
Loaded CounterTest

> :test CounterTest
Running 1 test class...
  ✓ testIncrement
  ✓ testMultipleIncrements
2 passed, 0 failed

// Equivalent native API — works from compiled code too:
> (Workspace test: CounterTest) failed
// => 0
```

### Parallel Test Runner

By default, `beamtalk test` runs test classes concurrently using the BEAM scheduler count:

```bash
beamtalk test              # Auto parallelism (default)
beamtalk test --jobs 4     # Up to 4 classes concurrently
beamtalk test -j 1         # Sequential (backward-compatible)
```

From code: `TestRunner runAll: 0` (auto), `TestRunner runAll: 4` (bounded), `TestRunner runAll` (sequential).

**Serial opt-out:** Test classes that touch global state (registered names, ETS tables, `TranscriptStream`) should override `serial` to prevent interference:

```beamtalk
TestCase subclass: TracingTest
  class serial -> Boolean => true

  testEnable =>
    self assert: Tracing isEnabled equals: false
```

Serial classes run alone, sequentially, after all concurrent classes complete.

## Shared Protocol Architecture

All interfaces — REPL, VS Code, MCP — connect to the same live BEAM workspace node via the same WebSocket JSON protocol (`beamtalk_ws_handler`). This means:

- An agent loading a file via MCP is immediately visible in VS Code and the REPL
- A developer hot-reloading a class in VS Code is immediately available to the agent
- All interfaces share actors, loaded modules, and workspace state

Adding a new interface requires only a protocol adapter over the existing WebSocket transport.

## Native Erlang Integration

For packages that need hand-written Erlang code — gen_server implementations, hex.pm dependencies, or direct OTP control — see the [Native Erlang Integration guide](beamtalk-native-erlang.md). It covers the `native/` directory layout, the `native:` keyword for actor classes, the `gen-native` stub generator, and hex dependency management via `[native.dependencies]` in `beamtalk.toml`.
