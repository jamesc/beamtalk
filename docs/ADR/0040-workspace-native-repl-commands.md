# ADR 0040: Workspace-Native REPL Commands, Facade/Dictionary Split, and Class-Based Reload

## Status
Accepted (2026-02-24)

## Context

### Problem

Beamtalk's REPL has 8 magic commands that bypass the language entirely:

| Command | Purpose |
|---------|---------|
| `:load` / `:l` | Load file or directory |
| `:reload` / `:r` | Reload module (with hot code migration) |
| `:modules` / `:m` | List loaded modules |
| `:help` / `:h` | Show documentation |
| `:bindings` / `:b` | List variable bindings |
| `:clear` | Clear bindings |
| `:test` / `:t` | Run test classes |
| `:show-codegen` / `:sc` | Display generated Core Erlang |

These commands are parsed in Rust (`mod.rs:repl_loop`), converted to JSON protocol messages, and dispatched to Erlang handler modules (`beamtalk_repl_ops_*.erl`). They exist outside the Beamtalk type system, are invisible to compiled code, cannot be composed with other expressions, and cannot be invoked from an LSP, test harness, or programmatic client without reimplementing the `:` prefix protocol.

This violates Principle 6 (*Everything is a message send*) and Principle 8 (*Reflection as primitive*). The REPL already migrated actor commands to Beamtalk-native message sends (ADR 0019: `:actors` → `Workspace actors`, `:kill` → `actorAt: kill`, `:unload` → `removeFromSystem`). The remaining commands should follow the same pattern.

### The Beamtalk / Workspace Tension

Two singleton objects exist today:

| Binding | Class | Current Scope |
|---------|-------|---------------|
| `Beamtalk` | SystemDictionary | VM reflection: `allClasses`, `classNamed:`, `globals`, `version` |
| `Workspace` | WorkspaceInterface | Actor introspection: `actors`, `actorAt:`, `actorsOf:`, `sessions` |

Three problems with the current design:

1. **No home for REPL commands.** Loading files, running tests, and querying the project namespace have no Beamtalk-native API.

2. **`Beamtalk` conflates facade and dictionary.** Today `Beamtalk` is an instance of `SystemDictionary` — it is both the typed facade (`allClasses`, `version`) and the namespace container. In Pharo, this was recognized as a mistake and fixed post-2.0 by splitting `SmalltalkImage` (facade) from `SystemDictionary` (dictionary).

3. **File-based, not class-based.** The current `:reload` and `:modules` commands think in terms of files and modules. But Beamtalk is a Smalltalk: the unit of code is the *class*, not the file. In Pharo, you reload a class (`Counter recompile`), not a file. Files are a transport mechanism.

### Constraints

- REPL `:` shortcuts must remain as ergonomic aliases — we are not removing them, just making them thin wrappers over real message sends
- `:exit` / `:quit` is a client concern (disconnects TCP/WebSocket) and stays as REPL-only
- `:show-codegen` takes a raw expression as argument (not a string), requiring special parsing — it stays as REPL-only magic for now
- `:bindings` queries per-session locals, which are implicit scope — stays REPL-only (like Erlang's `b()`)
- Auto-await in the REPL is an accepted context-specific affordance (not a magic command)
- The language-native API must be usable from compiled code, not just the REPL
- Following Anders Hejlsberg's principle (Beamtalk Principle 12): design for IntelliSense — typed, discoverable methods are the primary interface

## Decision

### 1. Facade/Dictionary Split

**Separate the typed facade from the namespace dictionary.** The facades (`BeamtalkInterface`, `WorkspaceInterface`) provide typed, discoverable methods. The `globals` accessor on each returns a plain `Dictionary` — the same class for both.

```text
Pharo:
  Smalltalk         → SmalltalkImage   (facade: version, allClasses, snapshot)
  Smalltalk globals → SystemDictionary (dictionary: at:, keys, values)

Beamtalk:
  Beamtalk          → BeamtalkInterface      (facade: version, help:, allClasses)
  Beamtalk globals  → Dictionary             (dictionary: at:, keys, values)

  Workspace         → WorkspaceInterface   (facade: classes, actors, load:, test)
  Workspace globals → Dictionary             (dictionary: at:, keys, values)
```

The intelligence lives on the facade, not the dictionary. `Beamtalk allClasses` filters `self globals values select: [:each | each isClass]`. The dictionary itself is a plain `Dictionary` with no special behavior — same class for both `Beamtalk globals` and `Workspace globals`.

`SystemDictionary` as a separate class goes away. It was only special because it had class-aware methods — but those belong on the facade. What's left is just `Dictionary`.

### 2. Two-Object Model: Beamtalk (System) and Workspace (Project)

**`Beamtalk`** (class: `BeamtalkInterface`) — *"What does the language know?"*

The system facade. Classes, version, documentation. Read-only for user code.

| Category | Methods |
|----------|---------|
| Identity | `version` |
| Classes | `allClasses`, `classNamed:` |
| Documentation | `help: aClass`, `help: aClass selector: aSelector` |
| Dictionary | `globals` → `Dictionary` (system namespace: classes, system singletons) |

`allClasses` and `classNamed:` are typed convenience methods that query the class registry directly (for liveness). `globals` returns an immutable snapshot of the system namespace for inspection. Both views contain the same classes, but `allClasses` is always up-to-date while `globals` is a point-in-time snapshot. (`Class >> isClass` already exists at `stdlib/src/Class.bt:44`.)

**`Workspace`** (class: `WorkspaceInterface`) — *"What is my working context?"*

The project facade. Loaded classes, actors, project operations. Mutable.

| Category | Methods |
|----------|---------|
| Code loading | `load: aPath` |
| Classes | `classes`, `testClasses` |
| Actors | `actors`, `actorAt:`, `actorsOf:` |
| Testing | `test`, `test: aTestClass` |
| Dictionary | `globals` → `Dictionary` (project namespace: Transcript, loaded classes) |

> **Implementation note (Phase 3):** `clear` was dropped during implementation. Session-local variable bindings are a per-shell concern, not a workspace concern — and the only way to locate the correct session was via a process dictionary side-channel (`bt_session_pid`), making it inherently non-composable and broken under async dispatch. The `:clear` REPL shortcut remains as a direct shell operation. Additionally, `testClasses`, `globals`, `test`, `test:`, and `actorsOf:` are implemented as Beamtalk facades rather than Erlang primitives — they delegate to `classes`, `TestRunner`, and `Behaviour>>includesBehaviour:` respectively, reducing the primitive surface area in the gen_server.

Removed from current API:
- `sessions` — returned opaque pid strings with no useful operations; not worth promoting
- `modules` — file/module-centric; replaced by class-centric `classes`
- `reload` / `reload:` — moved to `Behaviour` (see below)

### 3. Class-Based Reload on Behaviour

**Reload is a class operation, not a workspace operation.** When `Workspace load: "examples/counter.bt"` compiles a class, the runtime records the source file association on the class metadata (alongside doc comments per ADR 0033). The class then knows how to reload itself.

New methods on **`Behaviour`** (`stdlib/src/Behaviour.bt`):

| Method | Return | Description |
|--------|--------|-------------|
| `sourceFile` | `String` or `nil` | Path the class was compiled from; `nil` for stdlib/bootstrap or ClassBuilder-created classes |
| `reload` | `self` | Recompile from `sourceFile`, load new BEAM module; live actors pick up new code on next dispatch |

```beamtalk
Counter sourceFile          // => "examples/counter.bt"
Counter reload              // recompile + hot-swap

Integer sourceFile          // => nil (stdlib built-in)
Integer reload              // => Error: Integer has no source file — stdlib classes cannot be reloaded
```

This follows the Erlang/Elixir pattern:
- Erlang: `Module:module_info(compile)` records `source`; `c:c(Module)` recompiles from it and loads new code
- Elixir: `r(MyModule)` looks up the recorded source and recompiles
- Pharo: `Counter recompile` recompiles all methods of a class

**Hot code swap semantics follow BEAM conventions:** `reload` compiles the source file and loads the new BEAM module. Live actors running old code continue their current message handler; the next message dispatch uses the new code. This is standard BEAM two-version code loading — no custom state migration. (If state migration is needed in the future, it would use OTP's `code_change/3` convention, but that is out of scope for this ADR.)

**One class per file:** Each `.bt` file defines exactly one class. `Counter reload` recompiles `Counter sourceFile` and loads the resulting BEAM module. This one-to-one mapping keeps file-system-backed images simple — the file *is* the class.

For **dynamic classes** created via ClassBuilder (ADR 0038): `sourceFile => nil`, `reload` returns an error. Dynamic classes exist as runtime objects with method closures — they have no source file. Structural reflection (`help:`, `methods`, `allFieldNames`) still works because it queries the live class object, not source text.

When modules/imports exist in the future, stdlib classes could track their `.bt` source paths and `reload` would just work.

### 4. Dictionary Chain Mental Model

Beamtalk's name resolution can be understood as a scoped resolution order inspired by GemStone/S's SymbolList:

```text
Session locals (implicit)  →  Workspace user bindings  →  Workspace globals  →  Beamtalk globals
   x = 42                     MyTool = <actor>            Transcript = ...       Integer = <class>
   counter = #Actor<...>                                   Counter = <class>      String = <class>
                                                           project singletons     Object = <class>
```

This is a **conceptual model for users**, not an implementation change. The compiler continues to resolve names through its existing mechanisms (session binding maps, `beamtalk_class_registry`, workspace binding injection). The model describes the *effective* resolution order that users experience:

1. **Session locals** — per-connection variable bindings (`x := 42`), implicit scope
2. **Workspace user bindings** — workspace-level bindings registered via `bind:as:`
3. **Workspace globals** — project-level entries (Transcript, loaded classes, singletons)
4. **Beamtalk globals** — system-level entries (all registered classes, version)

Session locals are implicit scope (like method-local variables) — not a named object. `Workspace` and `Beamtalk` are the two named facades with their backing dictionaries.

**Important distinction from GemStone/S:** GemStone's SymbolDictionaries are mutable containers that the system writes into. Beamtalk's `Dictionary` is immutable — `globals` returns a **snapshot** of the current namespace state, not a live mutable container. `Beamtalk globals at: #Integer put: nil` returns a new Dictionary; it does not mutate the system namespace. The source of truth for class registration is `beamtalk_class_registry`, not the `globals` dictionary. The facade's typed methods (`allClasses`, `classNamed:`) query the registry directly for liveness; `globals` provides a point-in-time snapshot for inspection and meta-programming.

### 5. Two Interfaces: Typed Methods and Dictionary Access

Following Anders Hejlsberg's principle — design for IntelliSense — the **primary interface is typed methods**: discoverable, completable, statically analyzable. The **dictionary protocol** (`globals`, `at:`) provides reflective access for meta-programming.

```beamtalk
// Primary interface: typed methods (discoverable, completable)
Beamtalk allClasses
Beamtalk help: Counter
Beamtalk version
Workspace load: "examples/counter.bt"
Workspace classes
Workspace actors
Counter reload

// Reflective interface: dictionary access (meta-programming)
Beamtalk globals                    // => {#Integer: <class>, #String: <class>, ...}
Beamtalk globals at: #Integer       // => Integer class object

Workspace globals                   // => {#Transcript: <actor>, #Counter: <class>, ...}
Workspace globals at: #Transcript   // => TranscriptStream singleton
```

Both layers follow the same pattern: typed convenience methods for daily use, `globals` for when you need raw namespace access. The facades own the typing; the dictionaries are dumb containers.

### REPL Session Examples

```beamtalk
>> Workspace load: "examples/counter.bt"
Loaded: Counter (examples/counter.bt)

>> Workspace classes
Counter    examples/counter.bt

>> c := Counter spawn
#Actor<Counter,0.234.0>

>> Counter sourceFile
"examples/counter.bt"

>> Counter reload
Reloaded: Counter (new code loaded)

>> Workspace globals
{#Transcript: #Actor<TranscriptStream,0.90.0>,
 #Counter: Counter,
 #Beamtalk: #Actor<BeamtalkInterface,0.89.0>,
 #Workspace: #Actor<WorkspaceInterface,0.91.0>}

>> Beamtalk globals at: #Counter
Counter

>> Workspace test: CounterTest
CounterTest: 3 tests, 3 passed, 0 failed (12ms)

>> Beamtalk help: Counter
Counter : Actor
  getValue -> Integer
  increment -> Nil

>> Beamtalk help: Counter selector: #increment
increment -> Nil
  Defined in Counter (not inherited)
```

### REPL Alias Mapping

`:` commands become thin wrappers. The Rust CLI translates them to eval requests:

| REPL shortcut | Desugars to |
|---------------|-------------|
| `:load path/file.bt` | `Workspace load: 'path/file.bt'` |
| `:reload` | `(Workspace classes last) reload` |
| `:reload Counter` | `Counter reload` |
| `:modules` | `Workspace classes` |
| `:clear` | *(stays REPL-only — session bindings are shell-scoped)* |
| `:test` | `Workspace test` |
| `:test CounterTest` | `Workspace test: CounterTest` |
| `:help Counter` | `Beamtalk help: Counter` |
| `:help Counter increment` | `Beamtalk help: Counter selector: #increment` |

Note: `:reload` (no argument) and `:modules` are retained as REPL aliases for backward compatibility, but they desugar to the class-based API.

### What Stays as REPL-Only Magic

| Command | Reason |
|---------|--------|
| `:exit` / `:quit` / `:q` | Client-side concern — disconnects the TCP/WebSocket session |
| `:show-codegen` / `:sc` | Takes a raw expression, not a string — requires parser-level handling |
| `:bindings` / `:b` | Session locals are implicit scope — no language-level object to query them. Like Erlang's `b()`. |

`:show-codegen` could become `Beamtalk showCodegenFor: '1 + 2'` in the future, but passing code as a string is awkward and loses static analysis. Deferring this to a future ADR on compiler-as-service APIs.

### Error Examples

```beamtalk
>> Workspace load: 42
Error: Workspace>>load: expects a String path, got Integer

>> Integer reload
Error: Integer has no source file — stdlib classes cannot be reloaded

>> (ClassBuilder new: #Dyn superclass: Object; register) reload
Error: Dyn has no source file — dynamic classes cannot be reloaded from source

>> Beamtalk help: #NoSuchClass
Error: Class 'NoSuchClass' not found. Use Beamtalk allClasses for available classes.

>> Beamtalk globals at: #Integer put: nil
{#Integer: nil, #String: <class>, ...}   // returns a NEW Dictionary — system unchanged
>> Beamtalk classNamed: #Integer
Integer                                   // still there — globals is a read-only snapshot
```

## Prior Art

### Pharo/Squeak Smalltalk

In modern Pharo (post-2.0), `Smalltalk` is a `SmalltalkImage` facade; the dictionary protocol (`at:`, `at:put:`) lives on `Smalltalk globals` (a `SystemDictionary`). The split exists because `SmalltalkImage` accumulated non-dictionary responsibilities (VM info, snapshots). Typed convenience methods (`allClasses`, `version`) live on the facade; raw dictionary access goes through `globals`.

`SmalltalkImage` provides: `version`, `allClasses`, `classNamed:`, `hasClassNamed:`, `renameClassNamed:as:`, `removeClassNamed:`, `compiler`, `garbageCollect`, `snapshot:andQuit:`, `packages`, `organization`. The facade filters and queries the dictionary; the dictionary (`SystemDictionary`) is "just" a Dictionary subclass with `at:`, `keys`, `values`.

Code operations are class-based, not file-based. `Counter recompile` recompiles all methods. `Counter compile: 'foo ^ 42'` adds a method. `Counter removeFromSystem` removes the class. Files exist only as serialization (Tonel format, Monticello `.mcz`). The Playground (workspace) has per-session bindings with no `self`, similar to our implicit session locals.

Beamtalk adopts the facade/dictionary split directly from Pharo's post-2.0 design, but with two facades instead of one (system + project).

### GemStone/S

GemStone/S uses **multiple SymbolDictionaries** in an ordered chain (SymbolList). Each user has their own list: `UserGlobals` → project dictionaries → `Globals` → system. Name resolution walks the list; first match wins. The dictionary protocol (`at:`, `at:put:`) is on each individual SymbolDictionary — they are plain dictionaries with no special behavior.

| GemStone/S | Beamtalk | Scope |
|------------|----------|-------|
| `UserGlobals` | Session locals (implicit) | Per-connection |
| Project dictionaries | `Workspace globals` | Per-project |
| `Globals` | `Beamtalk globals` | Per-VM |

GemStone proved that a single SystemDictionary doesn't scale past a single-user image. Multiple scoped dictionaries with a resolution chain is the production Smalltalk answer. GemStone's dictionaries are plain mutable containers — the intelligence is in the SymbolList resolution, not in the dictionaries themselves. We adopt the structural principle (plain dictionaries, intelligence on the facade) but differ on mutability: Beamtalk's `Dictionary` is immutable, so `globals` returns a snapshot for inspection, not a live mutable namespace. The class registry remains the source of truth for writes.

### Newspeak

No globals at all. Dependencies are injected via `usingPlatform:` factory methods. Reflection requires an explicit mirror capability (`platform Mirrors reflect: obj`). The IDE is a regular Newspeak application. Beamtalk's two-facade model is a pragmatic middle ground between Newspeak's purity (no globals, full DI) and Pharo's single global.

### Elixir IEx

IEx helpers (`c`, `r`, `l`, `h`, `i`) are functions auto-imported from `IEx.Helpers`. They are **thin wrappers** over language-native APIs: `c/1` wraps `Code.compile_file/1`, `r/1` recompiles from recorded source via `Code.get_compiler_option(:source)`, `h/1` reads EEP-48 doc chunks via `Code.fetch_docs/1`. The language-level `Code` module works anywhere — not just in IEx. This is exactly the pattern we are adopting: shell shortcuts wrapping real APIs. Notably, `r/1` takes a *module* (class), not a file — it's class-based reload.

### Erlang Shell

Shell-internal commands (`f()`, `v()`, `b()`) manage shell state with no language equivalent — like our implicit session locals. `Module:module_info(compile)` records the `source` path at compile time. The `c` module's `c:c(Module)` uses this to recompile from source — class-based reload. This is the direct precedent for `Counter reload` using `Counter sourceFile`.

### Anders Hejlsberg / TypeScript Principle

"The compiler is the language service" (Beamtalk Principle 12). Applied to API design: typed methods are the primary interface because they're discoverable via IntelliSense/tab-completion. Raw dictionary access (`globals at:`) exists for meta-programming but isn't the daily-use API. This is why we have both `Beamtalk allClasses` (typed, completable) and `Beamtalk globals` (reflective, dynamic) rather than forcing users to go through `at:` for everything. The facade owns the typing; the dictionary is dumb.

### Summary

| Language | System Object | Project/Session Object | Reload Unit | Dictionary |
|----------|--------------|----------------------|-------------|-----------|
| Pharo | `SmalltalkImage` facade + `SystemDictionary` globals | Playground (UI tool) | Class (`recompile`) | On `Smalltalk globals` |
| GemStone/S | `Globals` dictionary | `UserGlobals`, project dicts | Class/method | Plain SymbolDictionary |
| Newspeak | `platform` (injected) | None — modules are classes | N/A | N/A (no globals) |
| Elixir | `Code`, `System` modules | N/A | Module (`r/1`) | N/A (module functions) |
| Erlang | `erlang`, `code` modules | N/A | Module (`c:c/1`) | N/A (module functions) |
| **Beamtalk** | **`BeamtalkInterface`** facade + `Dictionary` globals | **`WorkspaceInterface`** facade + `Dictionary` globals | **Class** (`reload`) | **Plain `Dictionary` on both** |

## User Impact

**Newcomer (from Python/JS):** `Workspace load: 'file.bt'` reads naturally and is discoverable — tab-completing `Workspace` reveals all project operations. `Counter reload` is intuitive: "reload that class." The `:load` shortcut still works for quick use. Error messages guide toward the class-based API. No need to learn a separate command language.

**Smalltalk developer:** Everything is now a message send, consistent with Principle 6. The facade/dictionary split mirrors Pharo's `SmalltalkImage` / `SystemDictionary` design. GemStone/S developers will recognize the dictionary chain pattern. `Beamtalk globals` is the direct analog of `Smalltalk globals`, and both are plain dictionaries. Class-based reload (`Counter reload`) feels natural — it's how Pharo works. The two-facade split with typed convenience methods is more structured than Pharo's single god-object.

**Erlang/BEAM developer:** Clean mapping to the `c` module pattern — `Workspace load:` is like `c:c/1`. `Counter sourceFile` mirrors `Module:module_info(compile) → source`. `Counter reload` follows the same pattern as Elixir's `r(Module)`. The API is callable from compiled code, scripts, and remote REPL sessions. `Beamtalk help:` wraps EEP-48 doc chunks, same as Erlang's `h/1`.

**Production operator:** `Workspace classes` and `Workspace actors` are callable from a remote REPL or programmatic client — no `:` prefix protocol needed. `Workspace globals` provides a machine-readable view of project state. Class-based reload means operators can hot-swap specific classes without reloading entire files.

**Tooling developer:** LSP can provide completions for `Workspace load:`, `Beamtalk help:`, `Counter reload` — typed methods are statically analyzable. The facade/dictionary split means tooling resolves typed methods through normal dispatch, and `globals` through dictionary protocol. No need to parse `:` command syntax separately.

## Steelman Analysis

### Alternative B: Three Named Dictionaries (Beamtalk + Workspace + Session)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "`Session globals` makes it clear these are *my* variables. I know `x := 42` goes somewhere specific, not into the shared project." |
| **Smalltalk purist** | "GemStone has UserGlobals as an explicit, named dictionary. Session state IS different from project state. Making it implicit hides a real architectural boundary." |
| **BEAM veteran** | "Each REPL session is a process. Session state is per-process. Making Session explicit maps directly to the BEAM process model — `Session` wraps `beamtalk_repl_shell`." |
| **Language designer** | "Three named dictionaries with explicit resolution chain is the most honest model. But implicit session locals are simpler and locals-are-implicit is universal across programming languages." |

### Alternative C: Single Workspace Dictionary

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One object! I just type `Workspace` and explore everything. `Workspace globals` shows me the whole world." |
| **Smalltalk purist** | ~~"Pharo has `Smalltalk` as one global"~~ — but GemStone/S proved multiple dictionaries scale better, and Pharo itself split `SmalltalkImage` from `SystemDictionary`. Weak argument. |
| **BEAM veteran** | "Erlang's `c` module puts everything in one place for the shell. Pragmatic and discoverable." |
| **Language designer** | "Simplest surface area. But conflating VM identity with project state means `Workspace version` and `Workspace load:` are categorically different. A god-dictionary." |

### Alternative D: Keep SystemDictionary as Special Class

| Cohort | Strongest argument |
|--------|-------------------|
| **Smalltalk purist** | "`SystemDictionary` IS the tradition. Pharo still has it. GemStone has it. Removing it loses a well-known concept." |
| **Language designer** | "Having class-aware methods on the dictionary makes the dictionary self-describing. `globals allClasses` is more natural than `facade allClasses`." |

### Alternative E: Dictionary-Only (no typed methods)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One protocol to learn — `at:` everywhere. Uniform." |
| **Smalltalk purist** | "Pure dictionary protocol. GemStone-native. Smalltalk-idiomatic. No facade methods needed." |
| **BEAM veteran** | "Maps directly to Erlang's `maps:get/2`. Simple, predictable." |
| **Language designer** | "Maximally uniform. But you lose discoverability — `Workspace at: #load` tells you nothing about the parameter type. Anders would not approve." |

### Tension Points

- **Newcomers prefer C** (one thing) but **language designers prefer A** (clean separation)
- **Smalltalk purists split on D**: tradition says SystemDictionary, but Pharo's own evolution says facade/dictionary split
- **Anders-style developers strongly reject E**: typed methods beat stringly-typed lookups for discoverability
- **Session as explicit (B) vs implicit**: principled but adds cognitive load; implicit locals are how every language works
- **File-based vs class-based reload**: BEAM veterans initially expect file-based (like Erlang's `c:c/1` takes a module), but Pharo developers expect class-based — and Elixir's `r/1` proves class-based works on BEAM too

## Alternatives Considered

### Alternative B: Three Named Dictionaries

Add an explicit `Session` object wrapping per-connection state:

```beamtalk
Session globals               // => {#x: 42, #counter: #Actor<...>}
Workspace globals             // => {#Transcript: <actor>, ...}
Beamtalk globals              // => {#Integer: <class>, ...}
```

**Rejected because:** Session-local variables are implicit scope — like method-local variables. No language makes you write `locals at: #x` to access a local variable. Making session state implicit matches universal programming language convention. If multi-session isolation becomes important later, `Session` can be introduced without breaking the model.

### Alternative C: Single Object (merge Beamtalk into Workspace)

One facade with all operations:

```beamtalk
Workspace allClasses
Workspace load: 'f'
Workspace version
Workspace globals              // => everything in one dict
```

**Rejected because:** God object mixing categorically different concerns. `Workspace version` is about the VM; `Workspace load:` is about the project. GemStone/S demonstrated that a single dictionary doesn't scale. Pharo split `SmalltalkImage` from `SystemDictionary` for the same reason.

### Alternative D: Keep SystemDictionary as Special Dictionary Class

Keep `globals` returning a `SystemDictionary` (subclass of `Dictionary` with `allClasses`, `classNamed:`, etc.) instead of a plain `Dictionary`:

```beamtalk
Beamtalk globals allClasses          // class-aware method on the dictionary
Beamtalk globals classNamed: #Counter // instead of at: + isClass filter
```

**Rejected because:** The intelligence belongs on the facade, not the dictionary. GemStone/S's SymbolDictionaries are plain containers — the system-awareness is in the resolution chain, not the dictionary. Having the same plain `Dictionary` class for both `Beamtalk globals` and `Workspace globals` is simpler and avoids the question of what class `Workspace globals` would be. The facade already provides `allClasses` and `classNamed:` as typed convenience methods.

### Alternative E: Auto-Imported REPL Helper (Erlang `user_default` Pattern)

Define a `ReplHelper` class whose methods are auto-imported into REPL scope:

```beamtalk
load: "examples/counter.bt"    // bare call, auto-imported from ReplHelper
help: Counter                   // bare call
```

**Rejected because:** Auto-imported bare calls look like method calls on `self`, not on a specific object. This harms discoverability — `load:` doesn't reveal where the capability lives. It also creates flat namespace collision risk. However, this pattern could complement the facade model in the future.

### Alternative F: Keep REPL Magic, No Language-Native API (Status Quo)

Leave `:load`, `:help`, etc. as REPL-only commands.

**Rejected because:** Violates Principle 6 (everything is a message send) and Principle 8 (reflection as primitive). Compiled code, LSP tooling, and programmatic clients cannot access these operations. The migration pattern from ADR 0019 Phase 3 already demonstrated the right direction.

### Alternative G: Dictionary-Only (no typed methods)

Use `at:` as the primary interface, no typed convenience methods:

```beamtalk
Beamtalk at: #Integer          // instead of classNamed:
Workspace at: #Transcript      // instead of... what?
```

**Rejected because:** Violates the Anders/IntelliSense principle (Beamtalk Principle 12). `Workspace load: 'foo.bt'` is discoverable, type-checkable, and self-documenting. `Workspace at: #load` is opaque — the tooling cannot help you. Typed methods for daily use, `globals` for meta-programming.

### Alternative H: File-Based Reload on Workspace

Keep `Workspace reload` and `Workspace reload: Counter` instead of `Counter reload`:

```beamtalk
Workspace reload                     // reload last loaded file
Workspace reload: Counter            // reload the file Counter came from
```

**Rejected because:** This is file-centric thinking. In Smalltalk, the class is the unit of code, not the file. A file may define multiple classes. Putting `reload` on `Behaviour` means the class knows its own source — following the Erlang pattern (`Module:module_info(compile) → source`) and the Elixir pattern (`r(Module)`). This also means ClassBuilder-created dynamic classes naturally respond with "no source file" rather than Workspace needing to handle that case.

## Consequences

### Positive

- All project operations become Beamtalk-native message sends, consistent with Principle 6
- Compiled code can invoke `Workspace load:`, `Beamtalk help:`, `Counter reload` — enables scripting, build tools, and programmatic workflows
- Clean facade/dictionary separation follows proven Pharo post-2.0 pattern
- Both `globals` return plain `Dictionary` — one class, no special subclass needed
- Class-based `reload` follows Erlang/Elixir/Pharo precedent — the class knows its source
- `sourceFile` on Behaviour provides class-level provenance metadata
- LSP can provide completions and diagnostics for typed facade methods
- Remote REPL clients use the same API without parsing `:` prefixes
- `:` shortcuts remain as ergonomic aliases — no loss of REPL productivity
- Establishes clear domain boundaries: `BeamtalkInterface` = system facade, `WorkspaceInterface` = project facade

### Negative

- Two facades to learn instead of one — newcomers must know where to look (`Beamtalk` vs `Workspace`)
- Dual interface (typed methods + `globals` dictionary) could confuse: "which do I use?"
- Renaming `SystemDictionary` to `BeamtalkInterface` is a breaking change for existing code referencing the class
- Implementation requires new `@primitive` methods in both Erlang backing modules and on `Behaviour`
- `Beamtalk help:` requires routing doc-extraction logic from `beamtalk_repl_ops_dev.erl` into the `BeamtalkInterface` backing module
- **Circular dependency hazard:** `Workspace load:` is a synchronous gen_server call. If loaded code itself calls `Workspace` synchronously during module initialization, the gen_server will deadlock. Implementation must either (a) spawn a helper process for file compilation, or (b) document that module init code must not call `Workspace` synchronously.
- **Eval round-trip overhead (Phase 4):** Converting `:load path` to `Workspace load: 'path'` and sending it through eval adds parsing + compilation overhead vs the current direct protocol dispatch. For interactive REPL use this is negligible, but the direct protocol ops should remain available for programmatic clients that need low-latency.

### Neutral

- `:exit` and `:show-codegen` remain as REPL-only commands (no change)
- `:bindings` remains REPL-only — session locals are implicit scope
- The REPL `:` prefix parser continues to exist — it just generates eval requests instead of custom protocol ops
- Existing e2e tests using `:` commands continue to work unmodified
- Auto-await in the REPL is unaffected — `Workspace load: 'f'` auto-awaits like any other async method
- Service singletons like `Transcript` live as entries in `Workspace globals` — accessed via their own typed messages, not through Workspace methods
- Session locals remain implicit — the REPL's `beamtalk_repl_shell` continues to manage per-session bindings as today
- Dynamic classes (ClassBuilder) have `sourceFile => nil` and `reload` returns an error — this is expected and consistent
- `globals` returns an immutable snapshot, not a live namespace — `Beamtalk globals at: #Integer put: nil` creates a new Dictionary without mutating the system
- One `.bt` file = one class — `Counter reload` recompiles exactly one class

## Implementation

### Phase 1: Rename SystemDictionary to BeamtalkInterface, globals returns Dictionary

Rename the class from `SystemDictionary` to `BeamtalkInterface`. Change `globals` to return a plain `Dictionary` populated from the class registry. Existing methods (`allClasses`, `classNamed:`, `version`) stay but are now understood as facade convenience methods.

**Affected files:**
- `stdlib/src/SystemDictionary.bt` → rename to `stdlib/src/BeamtalkInterface.bt`
- `runtime/apps/beamtalk_runtime/src/beamtalk_system_dictionary.erl` → rename to `beamtalk_interface.erl`
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_config.erl` — update singleton registration
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_bootstrap.erl` — update bootstrap binding

### Phase 2: Add sourceFile and reload to Behaviour

Add `sourceFile` and `reload` methods to `Behaviour`. Implement backing primitives that record source paths at compile time and trigger recompilation + hot code swap.

**Affected files:**
- `stdlib/src/Behaviour.bt` — add `sourceFile`, `reload`
- `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` — add primitive handlers
- `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` — store source file in class metadata
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` — record source file on class after compilation

### Phase 3: Expand WorkspaceInterface with project operations

Rename `WorkspaceEnvironment` to `WorkspaceInterface` (matching `BeamtalkInterface` naming convention). Add `classes`, `testClasses`, `globals`, `load:`, `test`, `test:`. Remove `sessions`. `testClasses`, `globals`, `test`, `test:`, and `actorsOf:` are implemented as Beamtalk facades rather than primitives. `clear` was dropped (see implementation note above). The `globals` method returns a plain `Dictionary`.

Implement backing primitives in `beamtalk_workspace_environment.erl` (rename to `beamtalk_workspace_interface.erl`), extracting logic from existing `beamtalk_repl_ops_load.erl` and `beamtalk_repl_ops_eval.erl`.

**Affected files:**
- `stdlib/src/WorkspaceInterface.bt` — add/remove method declarations
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_environment.erl` — add primitive handlers
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` — extract shared logic

### Phase 4: Add documentation API to BeamtalkInterface

Add `help:` and `help:selector:` to `BeamtalkInterface`.

**Affected files:**
- `stdlib/src/BeamtalkInterface.bt` — add `help:`, `help:selector:`
- `runtime/apps/beamtalk_runtime/src/beamtalk_interface.erl` — add primitive handlers
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl` — extract shared doc logic

### Phase 5: Convert REPL `:` commands to eval aliases

Change the Rust CLI to translate `:load path` into `Workspace load: 'path'` and send it as a normal eval request. `:reload Counter` becomes `Counter reload`. `:modules` becomes `Workspace classes`.

**Implementation note — string escaping:** The alias translation must properly escape path arguments. For double-quoted strings, escape backslashes and quotes (e.g., `path.replace("\\", "\\\\").replace("\"", "\\\"")`).

**Affected files:**
- `crates/beamtalk-cli/src/commands/repl/mod.rs` — rewrite command handlers as eval translations

### Phase 6: Deprecate custom protocol ops and add tests

Mark dedicated protocol operations (`"load-file"`, `"reload"`, `"modules"`, `"bindings"`, `"clear"`, `"docs"`, `"test"`) as deprecated. Keep them working for backward compatibility with WebSocket clients (ADR 0017), routing through the new Beamtalk-native implementations.

Add BUnit tests for all new facade and Behaviour methods. Add e2e tests exercising the message-send forms alongside `:` aliases. Update documentation.

**Affected files:**
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl` — deprecation routing
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` — delegate to workspace_environment
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl` — delegate to beamtalk_interface
- `stdlib/test/*.bt` — BUnit tests for new methods
- `tests/e2e/cases/*.bt` — e2e tests for `:` aliases and message-send forms
- `docs/beamtalk-language-features.md` — update workspace section

**Affected components summary:**

| Component | Change |
|-----------|--------|
| `stdlib/src/SystemDictionary.bt` | Rename to `BeamtalkInterface.bt` |
| `stdlib/src/WorkspaceInterface.bt` | Add `classes`, `testClasses`, `globals`, `load:`, `test`, `test:`; remove `sessions`; `clear` dropped |
| `stdlib/src/Behaviour.bt` | Add `sourceFile`, `reload` |
| `beamtalk_system_dictionary.erl` | Rename to `beamtalk_interface.erl`; add `help:`, `help:selector:` |
| `beamtalk_workspace_environment.erl` | New primitive handlers for project ops + `globals` |
| `beamtalk_behaviour_intrinsics.erl` | New primitives for `sourceFile`, `reload` |
| `beamtalk_object_class.erl` | Store source file in class metadata |
| `beamtalk_repl_ops_load.erl` | Record source file; extract shared logic |
| `beamtalk_repl_ops_dev.erl` | Extract doc logic |
| `beamtalk_repl_ops_eval.erl` | *(clear logic not extracted — clear was dropped)* |
| `mod.rs` (REPL loop) | Translate `:` commands to eval of message sends |
| `beamtalk_repl_server.erl` | Deprecation routing for old protocol ops |

## Migration Path

### For REPL Users

**No breaking changes.** All `:` commands continue to work as aliases:

| Before | After (also works) | Shortcut still works? |
|--------|--------------------|-----------------------|
| `:load examples/counter.bt` | `Workspace load: "examples/counter.bt"` | Yes |
| `:reload` | `Counter reload` (class-based) | Yes (desugars to last-loaded class) |
| `:reload Counter` | `Counter reload` | Yes |
| `:modules` | `Workspace classes` | Yes |
| `:help Counter` | `Beamtalk help: Counter` | Yes |
| `:bindings` | *(stays REPL-only — session locals are implicit)* | Yes |
| `:clear` | *(stays REPL-only — session bindings are shell-scoped)* | Yes |
| `:test CounterTest` | `Workspace test: CounterTest` | Yes |

### For SystemDictionary Users

`SystemDictionary` is renamed to `BeamtalkInterface`. Any code referencing `SystemDictionary` by name must update to `BeamtalkInterface`. The `Beamtalk` binding continues to work — its type changes but its API is a superset.

### For WebSocket/Protocol Clients

Dedicated protocol ops (`"load-file"`, `"docs"`, etc.) continue to work but are deprecated. Clients should migrate to sending eval requests with the Beamtalk expressions. The protocol wire format is unchanged.

### For Compiled Code

New capability — compiled code can now invoke workspace operations, reload classes, and inspect namespaces:

```beamtalk
Actor subclass: BuildScript
  run =>
    Workspace load: 'src/'.
    Workspace test.
    Transcript show: 'Build complete'.
    Transcript show: (Beamtalk allClasses size) printString; show: ' classes loaded'
```

## References

- Related issues: BT-841
- Builds on: ADR 0019 (Singleton Access via Class Variables) — established the `Workspace` and `Beamtalk` bindings and migrated actor commands
- Builds on: ADR 0032 (Early Class Protocol) — class objects are just objects, dispatch normally
- Builds on: ADR 0033 (Runtime-Embedded Documentation) — doc data available via class reflection
- Builds on: ADR 0037 (Collection Class Hierarchy) — `Dictionary` class used for `globals`
- Related: ADR 0004 (Persistent Workspace Management) — workspace as persistent BEAM node
- Related: ADR 0017 (Browser Connectivity) — WebSocket clients affected by protocol deprecation
- Related: ADR 0026 (Package Definition and Project Manifest) — `Workspace load:` must align with package loading semantics
- Related: ADR 0038 (Subclass ClassBuilder Protocol) — ClassBuilder-created classes have `sourceFile => nil`
- Related: BT-842 / ADR 0041 (Universal State-Threading Block Protocol) — the REPL eval function is the outermost "block" in BT-842's `fun(StateAcc) -> {Result, NewStateAcc}` protocol. Today the REPL's session `Bindings` map and codegen's `StateAcc` are already the same map (`let State = Bindings` in `repl_codegen.rs`). BT-842's universal protocol should preserve this: one map, not two. If Session ever becomes a first-class object (left open by this ADR), its bindings must be the same `StateAcc` that blocks thread.
- Future work: **Per-session services.** `Workspace` is a shared singleton — all connected REPLs see the same state. A per-session `Terminal` actor (spawned on connect, injected into session locals) could route output to the correct connection, avoiding the interleaving problem with the shared `Transcript`. This requires no new language concept — the REPL shell already manages per-session bindings, so a pre-populated `Terminal` binding is just an automatic session local. See Alternative B for the broader `Session`-as-facade discussion.
## Amendment: Mutable Workspace Globals — bind:as: / unbind: (BT-881)

**Date:** 2026-02-25

### Context

ADR-40 established `Workspace globals` as a read-only snapshot with no write path. In Smalltalk (Pharo/GemStone), globals are read/write — `Smalltalk globals at: #X put: y` or `UserGlobals at: #X put: y` are standard patterns. Beamtalk needed an explicit, typed write path for registering named objects into the workspace namespace.

### Decision

Add `bind:as:` and `unbind:` methods to `WorkspaceInterface`:

```beamtalk
Workspace bind: myActor as: #MyTool     // registers, with checks
Workspace unbind: #MyTool               // removes, errors if not found
Workspace globals                        // R/O snapshot now includes user bindings
Workspace globals at: #MyTool           // read a registered value
```

**Design choices:**

- `Workspace globals` stays R/O (snapshot) — unchanged from ADR-40
- Write path is explicit typed methods: `bind:as:` and `unbind:`
- Any value allowed (not just actors)
- `bind:as:` errors if name exists in Beamtalk globals (system name conflict — fail loud, fail early)
- `bind:as:` warns (via Transcript) if name is an existing loaded class (use `reload` instead)
- `bind:as:` silently overwrites if name already exists in user bindings (last writer wins)
- `unbind:` errors if name not found in user bindings
- No persistence — registrations are lost on workspace restart
- Workspace user bindings are stored in the `WorkspaceInterface` gen_server state and merged into REPL session bindings before each eval
- Business logic (conflict checks, warnings) implemented in Erlang primitives due to compiler limitations with block variable capture in actor methods

**Name resolution order** (updated from Section 4):

```text
Session locals (implicit)  →  Workspace user bindings  →  Workspace globals  →  Beamtalk globals
   x = 42                     MyTool = <actor>            Transcript = ...       Integer = <class>
   counter = #Actor<...>                                   Counter = <class>      String = <class>
```

Session locals override workspace user bindings, which override workspace globals and Beamtalk globals. The codegen's `maps:find(Name, State)` lookup handles this naturally since workspace user bindings are merged into the session bindings map before each eval.

### Implementation

- `beamtalk_workspace_interface.erl`: Added `user_bindings` field to gen_server state, `bind:as:` and `unbind:` dispatch with conflict checking
- `beamtalk_repl_eval.erl`: Merges workspace user bindings into session bindings before each eval; strips them from result to prevent session state accumulation
- `WorkspaceInterface.bt`: Added `bind:as:` and `unbind:` as `@primitive` methods
- `handle_globals/2`: Updated to include user bindings in the snapshot

- Prior art: Pharo `SmalltalkImage` facade + `Smalltalk globals` (SystemDictionary) — facade/dictionary split
- Prior art: GemStone/S SymbolList and multiple SymbolDictionaries — plain dictionaries, intelligence in resolution
- Prior art: Elixir `Code` module + IEx helpers, `r/1` class-based reload
- Prior art: Erlang `c` module, `Module:module_info(compile) → source`
- Prior art: Anders Hejlsberg's "design for IntelliSense" principle
- Principles: docs/beamtalk-principles.md (Principle 6: Everything is a message send, Principle 8: Reflection as primitive, Principle 12: Compiler is the language service)
