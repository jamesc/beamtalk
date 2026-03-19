# Changelog

## 0.2.0 — 2026-03-19

### Language

- **Three distinct class kinds** — `Value subclass:` (immutable, `field:`), `Actor subclass:` (mutable process, `state:`), `Object subclass:` (methods only, no data). Wrong keyword/class-kind combinations are now compile errors ([ADR 0065](docs/ADR/0065-complete-otp-primitives.md))
- **Extension methods** (`>>`) — add methods to sealed classes from outside, with compile-time conflict detection and type metadata ([ADR documented](docs/ADR/))
- **Extension type annotations** — `:: -> ReturnType` on `>>` definitions for gradual type checking
- **`initialize:` lifecycle hook** — Supervisor and DynamicSupervisor support post-start initialization
- **`terminate:` lifecycle hook** — documented and tested actor cleanup on shutdown
- **Actor process monitoring** — `monitor`, `pid`, `onExit:` for observing actor lifecycle
- **Non-local return fix** — `^` in blocks correctly unpacks super/tier2/self-dispatch tuples

### Standard Library

78 classes (up from 76), including new and improved:

- **Binary** — `serialize:`, `deserialize:`, `size:`, `fromIolist:` for byte-level operations
- **Logger** — stdlib class wrapping Erlang's OTP logger with `info:`, `warning:`, `error:`, `debug:` and compile-time domain metadata inlining
- **File** — `readBinary:`, `writeBinary:contents:`, `appendBinary:contents:` for binary I/O; `lastModified:` for file timestamps
- **Pid** — `kill`, `exit:` for forced process termination; `isAlive` for liveness checks
- **System** — `uniqueId` for monotonic ID generation; `setEnv:value:`, `unsetEnv:` for environment variables
- **Time** — `nowS` for seconds-precision timestamps
- **Ets** — `exists:`, `newOrExisting:type:` for safer table lifecycle
- **Subprocess** — `dir:` parameter for working directory control
- **Server** — `handleInfo:` for handling raw BEAM messages in actors
- **Timer** — `spawn_link` variant for linked timer processes
- **Supervisor** — `withShutdown:` for configurable shutdown timeouts
- **BUnit** — `setUpOnce`/`tearDownOnce` suite-level lifecycle hooks; `TestCase` converted to Value subclass with functional `setUp`

#### Bug fixes in stdlib

- `String collect:` now returns List (was incorrectly returning String)
- `String asString` returns `self` instead of `printString`
- `Integer raisedTo:` returns Integer for non-negative integer exponents
- `HTTPClient` works in compiled test contexts
- Erlang FFI charlist return values auto-converted to String
- File path sandbox removed — `File` now accesses unrestricted paths ([ADR 0063](docs/ADR/0063-remove-file-path-sandbox.md))

### Compiler

- **State threading overhaul** — local variable mutations inside blocks, `ifTrue:`/`ifFalse:` branches, and self-sends in assignment RHS now correctly propagate in both Value types and Actors. Covers `collect:`, `select:`, `reject:`, `detect:`, `flatMap:`, `do:`, `doWithKey:`, `takeWhile:`, `dropWhile:`, `groupBy:`, `partition:`, `sort:`, `anySatisfy:`, `allSatisfy:`, `each:`, `inject:into:`, and `count:`
- **Cross-file class hierarchy** — type checker resolves class kinds and methods across files; `ClassKind` propagation walks ancestor chain
- **Extension method pipeline** — scanner collects `>>` definitions across project, emitter generates ETS-dispatch code, type checker reads extension metadata
- **`@expect dead_assignment`** — new suppression annotation for intentional dead assignments
- **Keyword constructor hashing** — long keyword constructor atom names hashed to stay within Erlang's 255-char limit
- **Parser** — helpful error for unescaped `{` in strings (was a crash); stale `@expect type` treated as error
- **`new`/`new:` moved from Object to Value** — Object-kind classes can no longer be instantiated; constructors are Value-only
- **Lint improvements** — warn when Object subclass has only state + getters (suggests Value); block-scoped variable mutation lint for value types; removed false-positive self-capture lint (BT-953)
- **Codegen refactoring** — decomposed large functions across gen_server dispatch, state threading, method body loops, and register_class; explicit return values replace implicit side-channel fields; unified branch body loops with classify/dispatch pattern
- **Performance** — cache Pass 1 ASTs to eliminate double-parse in package build; REPL `:load` builds class indexes once per batch

### Tooling

- **Logging and observability** ([ADR 0064](docs/ADR/0064-runtime-logging-control-and-observability.md)):
  - `beamtalk logs` CLI command for workspace log access
  - JSON log format switching via `Beamtalk logFormat:`
  - Per-class and per-actor debug filtering with `enableDebug:`
  - `Beamtalk logLevel:` replaces deprecated `Logger setLevel:`
  - Actor lifecycle, message dispatch, compilation pipeline, supervisor lifecycle, and MCP tool invocation logging
  - SASL reporting routed to workspace file logger
  - VS Code output panel with live log streaming
- **MCP** — `list_classes`, `search_classes`, docs `see-also` links, WebSocket keepalive, per-diagnostic line numbers in `load_project`, client reconnect after workspace restart
- **VS Code** — log level picker and show-logs button
- **REPL** — fixed destructured bindings escaping into persistent scope; REPL codegen extracted from Compilation context
- **AGENTS.md** — essential patterns, MCP workflow, scaffold templates extracted to external files

### Bug Fixes

- Actor self-call state mutations no longer silently lost via `safe_dispatch`
- Supervisor correctly calls Actor `initialize` hook on child start
- `ifTrue:`/`ifFalse:` propagate local var mutations in value types and REPL
- Class method calls inside blocks in class methods no longer produce codegen error
- Self-cast sends in blocks no longer route through actor mailbox (deadlock fix)
- Logger formatter crashes on `{string, Binary}` tuples and unicode — hardened with crash-proof JSON formatter
- `beamtalk run .` hostname error when workspace already running
- `File absolutePath:` returns correct path on Windows/macOS
- Dispatch error logging demoted to debug (reduces runtime log noise)
- Scope leak fix when `generate_method_body_with_reply` returns Err in dispatch codegen
- Actor method error messages enriched with stacktrace and source location
- Move `initialize` dispatch to `handle_continue` to surface init errors properly

## 0.1.0 — 2026-03-15

Initial public release.

### Language

- Smalltalk-inspired message passing syntax with modern pragmatic choices ([ADR 0039](docs/ADR/0039-syntax-pragmatism-vs-smalltalk.md))
- **Actors** — BEAM processes with state, mailbox, and sync-by-default messaging ([ADR 0043](docs/ADR/0043-sync-by-default-actor-messaging.md))
- **Value types** — immutable data objects without process overhead ([ADR 0042](docs/ADR/0042-immutable-value-objects-actor-mutable-state.md))
- **Pattern matching** — literals, arrays, dictionaries, tuples, constructors, guards, rest patterns
- **Gradual typing** — optional type annotations with compile-time warnings ([ADR 0025](docs/ADR/0025-gradual-typing-and-protocols.md))
- **String interpolation** — `"Hello, {name}!"` with auto `printString` conversion ([ADR 0023](docs/ADR/0023-string-interpolation-and-binaries.md))
- **Blocks** — first-class closures with non-local return (`^`)
- **Supervision trees** — declarative OTP supervisor syntax ([ADR 0059](docs/ADR/0059-supervision-tree-syntax.md))
- **Result type** — `Result ok:` / `Result error:` with `tryDo:` for expected failures ([ADR 0060](docs/ADR/0060-result-type-hybrid-error-handling.md))
- **Hot code reload** — redefine methods on running actors; state is preserved
- **Destructuring** — array (`#[a, ...rest]`), tuple (`{x, y}`), and dictionary (`#{#k => v}`) destructuring
- **Streams** — lazy pipelines for collection processing, I/O, and generators ([ADR 0021](docs/ADR/0021-streams-and-io-design.md))

### Standard Library

76 classes written in Beamtalk, including:

- Core types: Integer, Float, String, Symbol, Character, Boolean, Nil, Block
- Collections: Array, List, Dictionary, Set, Bag, Tuple, Queue, Interval, Stream, Ets
- Actors: Actor, Supervisor, DynamicSupervisor, AtomicCounter, Timer
- Error handling: Result, Error, RuntimeError, TypeError, Exception
- I/O: File, FileHandle, Subprocess, ReactiveSubprocess
- Networking: HTTPServer, HTTPClient, HTTPRouter
- Data: Json, Yaml, Regex, DateTime, Random
- Reflection: Class, Metaclass, ClassBuilder, CompiledMethod
- Testing: TestCase, TestResult, TestRunner (BUnit framework)

### Tooling

- **REPL** — interactive workspace with hot reload, live patching, and `:` commands
- **VS Code extension** — syntax highlighting, diagnostics, workspace sidebar
- **LSP** — language server with completions, diagnostics, go-to-definition
- **MCP server** — Model Context Protocol integration for AI-assisted development
- **BUnit** — SUnit-style test framework with `beamtalk test`
- **API docs** — auto-generated from doc comments ([API Reference](https://www.beamtalk.dev/apidocs/))

### Compiler

- Compiles Beamtalk to Core Erlang via Rust
- Document-tree codegen — all Core Erlang output via `Document`/`docvec!` API ([ADR 0018](docs/ADR/0018-document-tree-codegen.md))
- Incremental compilation with class hierarchy metadata streaming ([ADR 0050](docs/ADR/0050-incremental-compiler-class-hierarchy.md))
- Embedded compiler via OTP port ([ADR 0022](docs/ADR/0022-embedded-compiler-via-otp-port.md))
- Diagnostic suppression with `@expect` annotations

### Platforms

- Linux x86_64
- macOS x86_64 and ARM64 (Apple Silicon)
- Windows x86_64
