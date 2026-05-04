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

# Type coverage (ADR 0077)
beamtalk type-coverage              # Per-class coverage report
beamtalk type-coverage --detail     # Show each Dynamic expression with location and reason
beamtalk type-coverage --format json          # Machine-readable JSON output
beamtalk type-coverage --at-least 75          # Exit non-zero if coverage < 75% (CI ratchet)
beamtalk type-coverage --class MyApp          # Filter to a specific class
beamtalk type-coverage --detail --class MyApp # Detail for one class

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

### Diagnostic Summary (BT-2014)

At the end of a successful build, `beamtalk build` prints a summary of non-error diagnostics grouped by category and severity. This lets you track typing progress at a glance:

```
Diagnostic summary (42 files):
  Type              12 warning
  Dnu                3 warning
  Total             15  (15 warning)
```

The summary is suppressed when `--no-warnings` is passed.

`beamtalk lint` prints the same summary at end of run. In `--format json` mode, a `summary` JSON object is emitted as the last line with `totals_by_severity`, `totals_by_category`, `files_checked`, and `total` fields for CI diffing.

`beamtalk lint` also loads FFI type signatures from the build cache (`_build/type_cache/`) so that lint and build agree on Erlang FFI return types. Cache entries are validated against live `.beam` file mtimes — if the underlying module has been recompiled since the cache was written, the stale entry is skipped and lint falls back to untyped-FFI inference. If the project has never been built, lint proceeds without a cache (matching its pre-cache behaviour).

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

The Beamtalk REPL is an interactive development environment connected to a persistent workspace — a detached BEAM node that survives disconnections ([ADR 0004](ADR/0004-persistent-workspace-management.md)). Actors keep running between sessions; REPL variable bindings are session-local.

### Starting the REPL

```bash
beamtalk repl                # Start or reconnect to workspace
beamtalk repl --foreground   # Start node in foreground (debug mode)
beamtalk repl --port 9090    # Use a specific port
beamtalk repl --node mynode  # Use a specific node name
```

On startup, the REPL prints the version and a help hint:

```
Beamtalk v0.4.0
Type :help for available commands, :exit to quit.

>
```

If a workspace is already running (from a previous session or another terminal), the REPL reconnects to it. All previously loaded classes and running actors are still available.

### Expression Evaluation

The REPL evaluates any Beamtalk expression and prints the result. Variables assigned in one expression persist across subsequent expressions within the same session.

```beamtalk
> x := 42
42

> x + 10
52

> counter := Counter spawn
#Actor<Counter,0.234.0>

> counter increment
1

> counter getValue
1
```

Actor message sends are **auto-awaited** in the REPL — when you send a message to an actor, the REPL waits for the reply and prints it. In compiled code, you would need to handle the asynchronous response explicitly.

Variable reassignment works as expected:

```beamtalk
> x := 100
100

> x + 10
110
```

Loop mutations also persist across REPL inputs:

```beamtalk
> count := 0
0

> 5 timesRepeat: [count := count + 1]
5

> count
5
```

### Commands Reference

REPL commands start with `:` and provide shortcuts for common operations. Most are thin wrappers around Beamtalk-native message sends ([ADR 0040](ADR/0040-workspace-native-repl-commands.md)), so the same operations work from compiled code, scripts, and actor methods.

#### Code Loading

| Command | Native equivalent | Description |
|---------|-------------------|-------------|
| `:load path/to/file.bt` | `Workspace load: "path/to/file.bt"` | Compile and hot-load a `.bt` file |
| `:load path/to/dir/` | `Workspace load: "path/to/dir/"` | Load all `.bt` files from a directory recursively |
| `:reload Counter` | `Counter reload` | Recompile a class from its source file |
| `:reload` | *(reload last loaded file/dir)* | Reload the most recently loaded file or directory |
| `:sync` / `:s` | `Workspace sync` | Sync workspace with project (requires `beamtalk.toml`) |
| `:unload Counter` | *(removes class)* | Unload a user class from the workspace |

**`:load`** compiles the file and hot-loads the resulting BEAM module. If a class with the same name already exists, the new code replaces it — live actors pick up the new methods on their next message send.

```beamtalk
> :load examples/counter.bt
Loaded: Counter

> c := Counter spawn
#Actor<Counter,0.234.0>

> c increment
1
```

**`:load`** also accepts directories, loading all `.bt` files recursively:

```beamtalk
> :load src/models/
Loaded 3 files from src/models/
```

**`:sync`** loads (or reloads) the entire project defined by `beamtalk.toml` in the current directory. It is incremental — unchanged files are skipped:

```beamtalk
> :sync
Reloaded 2 of 5 files

> :s
Reloaded 0 of 5 files (5 unchanged)
```

**`:unload`** removes a user class from the workspace. Stdlib classes cannot be unloaded:

```beamtalk
> :unload MyClass
ok

> :unload Integer
ERROR: Cannot remove stdlib class
```

#### Inspection and Documentation

| Command | Native equivalent | Description |
|---------|-------------------|-------------|
| `:help` / `:h` / `:?` | *(none)* | Show REPL help message |
| `:help Counter` | `Beamtalk help: Counter` | Show class documentation and methods |
| `:help Counter increment` | `Beamtalk help: Counter selector: #increment` | Show method documentation |
| `:help Counter class` | *(class-side docs)* | Show class-side methods (spawn, new, reload, ...) |
| `:help Counter class create:` | *(class-side method docs)* | Show class-side method documentation |
| `:help Erlang lists` | `Beamtalk erlangHelp: "lists"` | Show Erlang module docs and function signatures |
| `:help Erlang lists reverse` | `Beamtalk erlangHelp: "lists" selector: #reverse` | Show Erlang function docs and type signature |
| `:bindings` / `:b` | *(session-local)* | Show current variable bindings |
| `:show-codegen <expr>` / `:sc <expr>` | *(REPL-only)* | Show generated Core Erlang for an expression |

**`:help`** provides interactive documentation lookup:

```beamtalk
> :help Integer
== Integer < Number ==
Instance methods:
  + - * / ...

> :help Integer +
== Integer >> + ==
  ...

> :help Integer isPositive
== Integer >> isPositive ==
  (inherited from Number)
  ...
```

Package-qualified class names are also supported:

```beamtalk
> :help stdlib@Integer
== Integer < Number ==
  ...
```

Erlang FFI modules are also supported — shows type signatures and EEP-48 docs:

```beamtalk
> :help Erlang lists
Erlang module: lists

Functions:
  append: list1 :: List list2: list2 :: List -> List
  reverse: list :: List -> List
  ...

> :help Erlang lists reverse
lists:reverse
  Erlang: -spec reverse(List) -> List when List :: [T], T :: term().
  reverse: list :: List -> List
```

Tab completion works for module names (`:h Erlang li<TAB>`) and function names (`:h Erlang lists re<TAB>`).

**`:bindings`** shows all variables in the current session:

```beamtalk
> x := 42
42

> name := "hello"
hello

> :bindings
name = hello
x = 42
```

**`:show-codegen`** displays the Core Erlang output for any expression, useful for understanding what the compiler generates:

```beamtalk
> :sc 2 + 3
'erlang':'+'(2, 3)
```

#### Testing

| Command | Native equivalent | Description |
|---------|-------------------|-------------|
| `:test` / `:t` | `Workspace test` | Run all loaded test classes |
| `:test CounterTest` | `Workspace test: CounterTest` | Run a specific test class |

```beamtalk
> :load test/counter_test.bt
Loaded: CounterTest

> :test CounterTest
Running 1 test class...
  ✓ testIncrement
  ✓ testMultipleIncrements
2 passed, 0 failed
```

#### Session Control

| Command | Description |
|---------|-------------|
| `:interrupt` / `:int` | Cancel a running evaluation (out-of-band interrupt over a separate connection) |
| `:clear` | Clear all variable bindings |
| `:exit` / `:quit` / `:q` | Exit the REPL (Ctrl+D also works) |

**`:clear`** removes all session-local variable bindings:

```beamtalk
> x := 42
42

> :clear
ok

> x
ERROR: Undefined variable
```

**`:interrupt`** cancels a running evaluation. Because the main connection is blocked while an eval is in progress, the interrupt is sent out-of-band on a separate connection. Ctrl-C during an eval sends an interrupt automatically.

```beamtalk
> :interrupt
ok
```

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

Multi-line expressions work naturally with blocks, collections, and keyword messages:

```beamtalk
> [
..>   :x |
..>   x * 2
..> ] value: 21
42

> #{
..>   #name => "Alice",
..>   #age => 30
..> } at: #name
Alice
```

Use **Ctrl+C** to cancel multi-line input without submitting.

### Interactive Class Definitions

Classes can be defined directly at the REPL prompt without writing a `.bt` file:

```beamtalk
> Actor subclass: InlineCounter
..>   state: value = 0
..>   increment => self.value := self.value + 1
```

Methods can be added or replaced on existing classes using Tonel-style standalone method definitions (`>>`):

```beamtalk
> InlineCounter >> getValue => self.value

> c := InlineCounter spawn
#Actor<InlineCounter,0.234.0>

> c increment
1

> c getValue
1
```

Redefining a method replaces it for all future message sends, including on existing actor instances:

```beamtalk
> InlineCounter >> increment => self.value := self.value + 10

> c2 := InlineCounter spawn
#Actor<InlineCounter,0.234.0>

> c2 increment
10
```

### Hot Reload

When a class is reloaded (via `:load`, `:reload`, or an inline redefinition), existing actor instances pick up the new code on their next message send. State is preserved across the reload.

```beamtalk
> :load examples/counter.bt
Loaded: Counter

> c := Counter spawn
#Actor<Counter,0.234.0>

> c increment
1

> c increment
2

// Edit counter.bt to change increment behavior, then:
> :reload Counter
Counter

// Existing actor uses new code, state preserved
> c increment
12
```

Class-based reload uses the source file path recorded at load time:

```beamtalk
> Counter sourceFile
examples/counter.bt

> Counter reload     // Recompiles from that path
Counter

> Integer sourceFile // Stdlib classes have no source file
nil

> Integer reload     // Cannot reload stdlib
ERROR: Integer has no source file
```

### Workspace and Reflection Singletons

Two global singleton objects provide introspection and project operations. These are available in the REPL, in compiled code, and via the MCP server.

**`Beamtalk`** (class: `BeamtalkInterface`) — system reflection:

| Method | Description |
|--------|-------------|
| `version` | Beamtalk version string |
| `allClasses` | All registered classes as class objects |
| `classNamed: #Counter` | Look up a class by name |
| `globals` | Snapshot of system namespace as a Dictionary |
| `help: Counter` | Formatted class documentation |
| `help: Counter selector: #increment` | Formatted method documentation |

```beamtalk
> Beamtalk version
0.4.0

> Beamtalk allClasses
#(Integer, String, Array, ...)

> Beamtalk classNamed: #Integer
Integer

> Beamtalk globals
#{#Integer => Integer, #String => String, ...}
```

**`Workspace`** (class: `WorkspaceInterface`) — project operations:

| Method | Description |
|--------|-------------|
| `load: "path"` | Compile and hot-load a file or directory |
| `sync` | Sync workspace with project (`beamtalk.toml`) |
| `classes` | All loaded user classes |
| `testClasses` | All loaded test classes |
| `globals` | Snapshot of project namespace as a Dictionary |
| `actors` | All live actors |
| `actorAt: pidString` | Look up an actor by pid string |
| `actorsOf: Counter` | All live instances of a class |
| `test` | Run all test classes |
| `test: CounterTest` | Run a specific test class |
| `bind: value as: #Name` | Register a value in the workspace namespace |
| `unbind: #Name` | Remove a workspace binding |

```beamtalk
> Workspace load: "examples/counter.bt"
["Counter"]

> Workspace classes
#(Counter, ...)

> Workspace actors
#(#Actor<Counter,0.234.0>, ...)

> Workspace actorsOf: Counter
#(#Actor<Counter,0.234.0>)

> Workspace test
3 passed, 0 failed

> Workspace bind: myActor as: #MyTool
nil

> MyTool
#Actor<Counter,0.234.0>

> Workspace unbind: #MyTool
nil
```

### Variable Persistence and Name Resolution

REPL variable bindings are **session-local** — each connected REPL session has its own variable scope. Bindings persist across expressions within the same session but are lost when the session disconnects.

Workspace bindings (via `Workspace bind:as:`) are **workspace-level** — they persist across sessions and are visible to all connected clients.

Name resolution follows a scoped chain:

```
Session locals  →  Workspace user bindings  →  Workspace globals  →  Beamtalk globals
  x = 42            MyTool = <actor>            Transcript = ...      Integer = <class>
  counter = ...                                  Counter = <class>     String = <class>
```

1. **Session locals** — per-connection variables (`x := 42`), created by `:=` assignment
2. **Workspace user bindings** — workspace-level names registered via `Workspace bind:as:`
3. **Workspace globals** — project-level entries (Transcript, loaded classes, singletons)
4. **Beamtalk globals** — system-level entries (all registered classes, version)

### Common Workflows

#### Load, Edit, Reload, Test

The typical interactive development cycle:

```beamtalk
> :load src/Counter.bt          // Load the class
Loaded: Counter

> c := Counter spawn            // Try it out
#Actor<Counter,0.234.0>

> c increment
1

// ... edit Counter.bt in your editor ...

> :reload Counter               // Hot-reload the change
Counter

> c increment                   // Existing actor uses new code
11

> :load test/CounterTest.bt     // Load tests
Loaded: CounterTest

> :test CounterTest             // Run tests
2 passed, 0 failed
```

#### Project Sync Workflow

For projects with `beamtalk.toml`, use `:sync` to load the whole project:

```beamtalk
> :sync                         // Initial load
Reloaded 5 of 5 files

// ... edit files ...

> :s                            // Incremental reload (short alias)
Reloaded 1 of 5 files (4 unchanged)

> :test                         // Run all tests
10 passed, 0 failed
```

#### Prototyping with Inline Classes

Define and iterate on classes without creating files:

```beamtalk
> Actor subclass: Greeter
..>   state: name = "World"
..>   greet => "Hello, " ++ self.name

> g := Greeter spawn
#Actor<Greeter,0.234.0>

> g greet
Hello, World

// Add a method
> Greeter >> greetWith: prefix => prefix ++ " " ++ self.name

> g greetWith: "Hi"
Hi World

// Replace a method
> Greeter >> greet => "Hey there, " ++ self.name

> g greet
Hey there, World
```

#### Actor Lifecycle Management

Inspect, stop, and kill actors through the workspace:

```beamtalk
> c := Counter spawn
#Actor<Counter,0.234.0>

> c isAlive
true

> Workspace actors              // See all live actors
#(#Actor<Counter,0.234.0>)

> c stop                        // Graceful shutdown
ok

> c isAlive
false

> c stop                        // Idempotent
ok
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
| `diagnostic_summary` | Aggregated diagnostic counts by category/severity plus type-coverage stats (offline) |

All MCP tools map to the same `Workspace` and `Beamtalk` APIs available in the REPL. The tracing tools correspond to the `Tracing` stdlib class (see [Language Features — Actor Observability](beamtalk-language-features.md#actor-observability-and-tracing-adr-0069)).

## VS Code Extension

The [Beamtalk VS Code extension](https://github.com/jamesc/beamtalk/tree/main/editors/vscode) connects to the live workspace and provides:

- **Syntax highlighting** for `.bt` files
- **LSP integration** — diagnostics, completions, go-to-definition, workspace symbol search. The LSP preloads classes from `_build/deps/*/src/` so dependency classes resolve without false-positive `Unresolved class` warnings.
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

## Type Coverage (ADR 0077)

`beamtalk type-coverage` reports what percentage of expressions in your project have non-Dynamic inferred types. This helps track type adoption progress and gate CI on coverage regressions.

### Basic Usage

```console
$ beamtalk type-coverage
Type Coverage Report
====================

File                          Class              Coverage
src/AccountService.bt         AccountService     92.3%  (48/52 expressions)
src/Router.bt                 Router             87.1%  (54/62 expressions)
src/ConfigLoader.bt           ConfigLoader       71.4%  (30/42 expressions)
src/MyApp.bt                  MyApp              45.0%  (9/20 expressions)
──────────────────────────────────────────────────────────────────────
Total                                            80.1%  (141/176 expressions)
```

Coverage is defined as **(expressions with non-Dynamic inferred type) / (total expressions in the TypeMap)**. Every AST node that produces a TypeMap entry is counted — identifiers, message sends, assignments, literals, block bodies, and binary operations. Sub-expressions are counted individually.

### Flags

| Flag | Description |
|------|-------------|
| `--detail` | Show each Dynamic expression with `file:line:col` location and reason |
| `--format json` | Machine-readable JSON output for dashboards and CI |
| `--at-least N` | Exit non-zero if total coverage < N% (CI ratchet) |
| `--class ClassName` | Filter to a specific class |

### Detail Mode

```console
$ beamtalk type-coverage --detail --class MyApp
Type Coverage Report
====================

File                          Class              Coverage
src/MyApp.bt                  MyApp              45.0%  (9/20 expressions)
──────────────────────────────────────────────────────────────────────
Total                                            45.0%  (9/20 expressions)

Dynamic expressions:
  src/MyApp.bt:12:5                              (unannotated return)
  src/MyApp.bt:15:9                              (dynamic receiver)
  src/MyApp.bt:18:5                              (dynamic receiver)
  ...
```

Each Dynamic expression shows its `file:line:col` location and the reason the type could not be determined. The reason strings match those shown in LSP hover (see [Dynamic Type Visibility](beamtalk-language-features.md#dynamic-type-visibility-adr-0077)).

### CI Integration

Use `--at-least` to prevent coverage regressions in CI pipelines:

```yaml
# In CI pipeline
- run: beamtalk type-coverage --at-least 75 --format json
```

The command exits non-zero if total coverage falls below the threshold. JSON output includes a `passed` field for easy scripting:

```json
{
  "total_expressions": 176,
  "typed_expressions": 141,
  "coverage_percent": 80.1,
  "threshold": 75,
  "passed": true,
  "classes": [
    {
      "name": "AccountService",
      "file": "src/AccountService.bt",
      "total": 52,
      "typed": 48,
      "coverage_percent": 92.3
    }
  ]
}
```

### Scope and Limitations

- Coverage reports only the project's own classes. Dependencies and stdlib classes are excluded.
- Block parameters and block bodies in collection methods (`collect:`, `inject:into:`) often infer as Dynamic because stored blocks lose type context. Classes using heavy iteration idioms may show systematically lower coverage.
- Coverage measures what the compiler knows, not what you annotated — good type inference yields high coverage with few annotations. Well-spec'd Erlang FFI modules (via `NativeTypeRegistry`) also contribute positively.

## Shared Protocol Architecture

All interfaces — REPL, VS Code, MCP — connect to the same live BEAM workspace node via the same WebSocket JSON protocol (`beamtalk_ws_handler`). This means:

- An agent loading a file via MCP is immediately visible in VS Code and the REPL
- A developer hot-reloading a class in VS Code is immediately available to the agent
- All interfaces share actors, loaded modules, and workspace state

Adding a new interface requires only a protocol adapter over the existing WebSocket transport.

## Native Erlang Integration

For packages that need hand-written Erlang code — gen_server implementations, hex.pm dependencies, or direct OTP control — see the [Native Erlang Integration guide](beamtalk-native-erlang.md). It covers the `native/` directory layout, the `native:` keyword for actor classes, the `gen-native` stub generator, and hex dependency management via `[native.dependencies]` in `beamtalk.toml`.
