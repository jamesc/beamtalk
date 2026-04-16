# Changelog

## Unreleased

### Standard Library

- **Named actor registration** ([ADR 0079](docs/ADR/0079-named-actor-registration.md)) — actors can now be registered under a `Symbol` name and looked up from anywhere via a name-resolving proxy that survives supervised restarts (BT-1985 epic, BT-1986..BT-1991):
  - `Class spawnAs: name` / `Class spawnWith: args as: name` — atomic spawn + register, returns `Result(Self, Error)`.
  - `Class named: name` — typed lookup returning `Result(Self, Error)`; `Self` resolves to the receiver class at the call site.
  - `actor registerAs: name` / `actor unregister` / `actor registeredName` / `actor isRegistered` — post-spawn registration API.
  - `Actor allRegistered` — enumerate Beamtalk-registered actors (excludes raw OTP kernel processes via the `$beamtalk_actor` process-dict marker).
  - `SupervisionSpec withName:` — declaratively name a supervised child so the supervisor re-registers the name on every restart.

### Runtime

- **Named-registration intrinsics and supervisor wiring** — `beamtalk_actor:'spawnAs'/2,3`, `registerAs/2`, `unregister/1`, `named/2`, `allRegistered/1`, name-resolving `{registered, Name}` proxy dispatch, and supervisor routing of `SupervisionSpec withName:` through `beamtalk_actor:'spawnAs'/2,3` so supervised restarts re-register atomically (BT-1987, BT-1988, BT-1990).
- Reserved-name blocklist at registration time for OTP kernel/stdlib atoms and the `beamtalk_` prefix; returns `Result error: (beamtalk_error reserved_name)`.
- Fix `'spawnAs'/2` defaulting to `[]` instead of `#{}`, which crashed supervised named children in `init/1` with `{badmap, []}` (BT-1991).
- Fix REPL JSON formatter crashing when displaying a name-resolving proxy: `#beamtalk_object{pid = {registered, Name}}` now renders as `#Actor<Class,registered,Name>` instead of calling `pid_to_list/1` on a non-pid term (BT-1991).

### Documentation

- ADR 0079: Named Actor Registration.
- Language features: new "Named Actor Registration" chapter covering the API surface, proxy semantics caveats (monitors don't re-arm across restarts; equality rules; unregister makes proxy dead), reserved-name policy, BEAM mapping, and a before/after migration example from `Supervisor which:` + `initialize:` to named registration (BT-1991).

## 0.3.1 — 2026-03-26

### Language

- **Actor message timeout configuration syntax** — configure per-message timeouts with language-level syntax (BT-1190)
- **Rename `trace:`/`traceCr:` to `show:`/`showCr:` on Object** — clearer naming for debug output methods (BT-1636)
- **Fix `^` and `:=` in match arm bodies** — non-local returns and assignments inside match arms now compile correctly
- **Fix `whileTrue:` silently drops mutations in value-type context** (BT-1609)

### Standard Library

- **Tracing** — new `Tracing` stdlib class with Erlang shim for trace context, propagated context across actor boundaries, causal trace linking, and application-level metadata enrichment (BT-1604, BT-1605, BT-1625, BT-1633, BT-1639)
- **Protocol enhancements** — class methods in protocol definitions, REPL support for `Protocol define:` declarations, fix protocol-only files not generating `register_class/0`, fix class prefix before doc comments in protocol signatures (BT-1610, BT-1611, BT-1612, BT-1616, BT-1617, BT-1618)
- **`performLocally:withArguments:`** — new class method dispatch primitive that executes in the caller's process, enabling synchronous class method calls without actor messaging (BT-1664)
- **`Foo class` methods return user-defined class methods** (BT-1635)
- **Emit class method doc comments in codegen** (BT-1634)
- Revert method combinations (`before/after` daemons) and `migrate:` protocol — removed BT-102 and BT-106 pending redesign

### Compiler

- **Direct-call optimization for sealed class methods** — sealed classes now emit direct function calls instead of dynamic dispatch, improving performance (BT-1639)
- **Enforce `field:`/`state:` keyword alignment by class kind** — the compiler rejects `state:` in value classes and `field:` in stateful classes (BT-1663)
- **Improved lint diagnostics** — origin tracing and severity levels in lint messages (BT-1588)
- Surface `protocol register_class/0` failures as structured errors (BT-1616)

### Runtime

- **Actor tracing infrastructure** — trace store gen_server with lock-free storage, actor send wrapper telemetry, lifecycle telemetry from compiled actor `init`/`terminate`, lifecycle events (spawn, stop, destroy), aggregate actor stats with min/max duration and class name, outcome/class/duration trace filters, wall-clock timestamps with serialized counter, export_traces for trace snapshots (BT-1601, BT-1602, BT-1603, BT-1620, BT-1621, BT-1622, BT-1627, BT-1628, BT-1629, BT-1632, BT-1638, BT-1640, BT-1641, BT-1642)
- Fix `String#asAtom` intermittent failures on valid strings (BT-1585)

### Tooling

- **MCP tracing tools** — lean surface for trace inspection, integration fixes, and e2e test coverage (BT-1606, BT-1622)
- **BUnit parallel test runner** with serial opt-out (BT-1624)
- **BUnit stack traces show Beamtalk class names and source line numbers** — test failures display `.bt` source locations instead of compiled Erlang module names
- **Improved BUnit failure output** — caller-first stack frames and relative file paths
- **Parallelize CI test suite** (BT-1623)
- Fix MCP lint server reporting zero warnings when CLI finds real issues (BT-1587)
- Fix flaky MCP REPL startup tests (BT-1599)

### Documentation

- ADR 0070: Package Namespaces and Dependencies (BT-714)
- Tracing documentation and examples (BT-1607)

### Internal

- Add parser tests for class member ordering before fields
- Fix flaky `testGreeting` — stop own actor in `tearDown` (BT-1662)
- Fix `TranscriptStream` timing races in getting-started tests (BT-1662)
- Fix `trigger_hot_reload` tests failing under cover compilation (BT-1630)
- Dependency updates: tungstenite 0.29, tokio-tungstenite 0.29, actions/download-artifact v8

## 0.3.0 — 2026-03-21

### Language

- **Parametric types (generics)** — classes can declare type parameters (`Value subclass: Stack(T)`), with full type checker substitution, constructor inference, and generic inheritance via superclass type application ([ADR 0068](docs/ADR/0068-parametric-types-and-protocols.md))
- **Protocols** — `Protocol` declarations with class-body syntax, protocol registry with conformance checking, runtime queries (`conformsTo:`, `protocols`), variance for protocol-typed parameters, and type parameter bounds (`T :: Printable`) ([ADR 0068](docs/ADR/0068-parametric-types-and-protocols.md))
- **Union types** — `T | U` syntax with exhaustive type checking and provenance tracking
- **Control flow narrowing** — `class =`, `isKindOf:`, `isNil`, and `respondsTo:` narrow types in `ifTrue:`/`ifFalse:` branches
- **String is now a subclass of Binary** — String inherits Binary's byte-level methods; Binary moved under Collection hierarchy ([ADR 0069](docs/ADR/0069-string-subclass-of-binary.md))
- **DynamicSupervisor type parameter** — `startChild` return type narrows based on the supervisor's declared child type

### Standard Library

- **Protocol** — new stdlib class wrapping the runtime protocol registry
- **Binary** — expanded with instance methods: `size`, `serialize:`, `deserialize:`, `fromIolist:`, `at:`, `slice:length:`, `toList`, and more
- Stdlib generic annotations added to `Result`, `Array`, `Dictionary`, and `Set`

### Compiler

- **Generic type substitution** — type checker resolves type arguments through method calls, field access, and constructor patterns
- **Dialyzer spec generation** — generic types emit proper `-spec` attributes; CI validates generated specs
- **Codegen runtime metadata** — generic type information preserved at runtime for reflection
- **Union type provenance** — `InferredType::Union` unified to `Vec<InferredType>` members with source tracking
- **Fix false type warnings** for generic field defaults

### Tooling

- **MCP** — removed `:modules` command; purged "module" terminology in favour of "class" throughout
- **Single-source versioning** — `VERSION` file at repo root with automatic dev suffix from git state
- **Nightly builds** — distribution builds and `install.sh --nightly` support
- **MCP load_project** — rebuild class indexes after each file load, fixing test fixture resolution (BT-1608)

### Documentation

- ADR 0068: Parametric Types and Protocols (updated)
- ADR 0069: Actor Observability and Tracing (BT-1429)
- ADR 0069: Make String a subclass of Binary
- Learning guide 27: Generics and Protocols

### Internal

- Fix flaky CI: WebSocket health check with exponential backoff (BT-1598)
- Fix broken streamlinear-cli installation in cloud and devcontainer
- Binary instance method tests and `Binary size:` migration (BT-1594)
- String method audit verifying Binary inheritance (BT-1595)

## 0.2.0 — 2026-03-20

### Language

- **Three distinct class kinds** — `Value subclass:` (immutable, `field:`), `Actor subclass:` (mutable process, `state:`), `Object subclass:` (methods only, no data). Wrong keyword/class-kind combinations are now compile errors ([ADR 0067](docs/ADR/0067-separate-state-field-keywords-by-class-kind.md))
- **Extension methods** (`>>`) — add methods to sealed classes from outside, with compile-time conflict detection and type metadata ([ADR 0066](docs/ADR/0066-open-class-extension-methods.md))
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
- **`new`/`new:` moved from Object to Value** — Object-kind classes can no longer be instantiated; constructors are Value-only. Sub-subclass instantiation (`new`/`new:`/keyword ctors) now works correctly
- **Lint improvements** — warn when Object subclass has only state + getters (suggests Value); block-scoped variable mutation lint for value types; removed false-positive self-capture lint (BT-953)
- **Codegen refactoring** — decomposed large functions across gen_server dispatch, state threading, method body loops, and register_class; explicit return values replace implicit side-channel fields; unified branch body loops with classify/dispatch pattern
- **Performance** — cache Pass 1 ASTs to eliminate double-parse in package build; REPL `:load` builds class indexes once per batch

### Tooling

- **Logging and observability** ([ADR 0064](docs/ADR/0064-runtime-logging-control-and-observability-api.md)):
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
- Float literals retain decimal in codegen for strict equality correctness
- BUnit fixture superclass index fix for Value sub-subclass tests

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
