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

# Development
beamtalk repl               # Interactive REPL (connects to workspace)
beamtalk test               # Run test suite

# Native Erlang
beamtalk gen-native MyActor # Generate skeleton gen_server from a native: Actor class
```

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
