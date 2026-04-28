# Changelog

## Unreleased

### Language

- **Character literal dispatch** — character literals (`$A`, `$a`, etc.) now dispatch through the Character method table instead of Integer's. `$A asString` returns `"A"` (not `"65"`), `$A class` returns `Character`, and methods like `uppercase`, `lowercase`, `printString` work correctly on character literals. `Character value: 65` class-method sends also work (BT-2095).

### Internal

- Fix `build --stdlib-mode` to resolve FFI types via `NativeTypeRegistry` — eliminates 202 spurious untyped-FFI warnings from `just dialyzer-specs` (#2138).

## 0.4.0 — 2026-04-27

The headline of 0.4 is a **type system that finally has teeth**: classes can opt into a `typed` modifier, the inferencer flows types through generics, unions, narrowing predicates, FFI calls, block parameters, and method-local type variables, and a new `beamtalk type-coverage` CLI plus LSP "Dynamic with reason" hover make typing visible end-to-end. Around it: a real **package manager** (path / git / hex deps with lockfile, qualified `pkg@Class` names, cross-package collision detection), **native Erlang sources inside Beamtalk packages** (compiled via vendored rebar3, EUnit run from `beamtalk test`), **Result-shaped FFI and supervisor lifecycles** (ADR 0076 + 0080), **named actor registration** (ADR 0079), and an `internal` access-control modifier (ADR 0071).

### Language

- **Local variable type annotations** — `name :: Type := expr` syntax for type-checked variable bindings at type-erasure boundaries; supports simple, parametric, and union types; annotation is erased at codegen (BT-2012).
- Accept narrowing RHS in local type annotations — `dict :: Dictionary := someMethodReturningObject` no longer warns when the declared type is more specific than the inferred type (BT-2015).
- Fix `@expect` directives inside block bodies (`ifTrue: [...]`, `collect: [...]`, `whileTrue: [...]`) being silently ignored — they now correctly suppress diagnostics on the next statement within the block (BT-2010).
- Typechecker warns when a non-Block value is passed to a `Block(T, R)` parameter — previously, parametric type annotations like `Block(T, R)` were treated as unknown classes and the warning was suppressed (BT-2002).
- **`<ClassName> class` metatype annotation** — `Actor class`, `MyService class`, etc. can now appear in any type position (fields, parameters, return types, locals) to denote a class object in the named hierarchy. Class-side methods on that class resolve without false DNU warnings after nil-check narrowing. Resolves to `Dynamic` at runtime (BT-2034).
- **Conditional return type inference** — `ifTrue:ifFalse:` now declares `Block(R), Block(R) -> R`, so the type checker unifies both arms to a common return type instead of collapsing to `Dynamic` (BT-2020).
- Typechecker warns when a method declared `-> Never` has a body that returns a known type (BT-2033).
- Fix generic return type inner arg validation — the type checker now correctly warns when inner type args of a generic return type mismatch (e.g., `-> Result(Integer, Error)` with body returning `Result(String, Error)`) (BT-2022).
- **Nil-check narrowing extensions** — `isNil ifFalse:` narrows receiver to non-nil inside the block; diverging guards (`isNil ifTrue: [self error: "..."]`) narrow to non-nil for the rest of the method, not just `^` returns; `self.field` reads narrow after nil-check guards (BT-2048, BT-2049).
- **`ifNotNil:` block parameter narrowing** — block parameters in `ifNotNil:`, `ifNil:ifNotNil:`, and `ifNotNil:ifNil:` are typed as the non-nil branch of the receiver's type (BT-2046).
- **`ifNil:ifNotNil:` branch-union return type** — `receiver ifNil: [a] ifNotNil: [:x | b]` infers as `typeof(a) | typeof(b)` instead of `Dynamic`; a branch containing `^` contributes `Never` (BT-2047).
- **`on:do:` handler block parameter inference** — block parameters in `on: SomeException do: [:e | ...]` are typed as the exception class argument instead of `Dynamic` (BT-2045).
- Diagnostics now render `Nil` where the source uses `Nil`, instead of the internal name `UndefinedObject` (BT-2066).
- Deep-descendant `Never` detection in diverging guard blocks — nested `Never`-typed expressions (e.g., `[Sink log: (self error: "...")]`) correctly trigger nil-check narrowing (BT-2051).
- Fix false-positive "Unused variable" warning from typed block parameters — block parameters with `:: Type` annotations are now parsed correctly (BT-2043).
- Fix spurious `Dynamic` inference on block parameters when iterating a `Dynamic`-typed receiver in a typed class (BT-2042).
- **`Never` bottom type** — divergent expressions (`self error: ...`, `^expr`, infinite loops) infer as `Never`, so branch-union return types like `cond ifTrue: [^x] ifFalse: [y]` collapse cleanly to `typeof(y)` instead of `Dynamic` (BT-1945).
- **`Self class` metatype** — `Self class` annotations on class-side return types correctly substitute through inheritance, enabling typed `new`-style factories on subclasses (BT-1952).
- **Auto-chain `initialize` across the actor hierarchy** — subclasses no longer need to call `super initialize` manually; the runtime walks the class chain and inherited typed-no-default fields are validated at construction ([ADR 0078](docs/ADR/0078-actor-initialize-inheritance.md), BT-1951, BT-1976).
- **One class or protocol per file** — enforced at parse time so cross-file class resolution and per-class hot reload have a single source of truth (BT-1666).
- **Strict inequality `=/=`** — first-class stdlib method declaration alongside `=` and `~=` (BT-1563).
- **Match-arm pattern variables are bound into the arm's type environment** — `match: [Foo(x) -> ...]` types `x` correctly inside the arm instead of falling back to `Dynamic` (BT-1946).
- Allow omitting the default value on typed `state:`/`field:` declarations — `state: counter :: Integer` is now legal in typed actors (BT-1947).
- Fix type annotation on a method parameter changing the runtime dispatch behavior (BT-1944).
- Parser: nested keyword messages inside map literals now parse correctly (BT-1854).
- Fix unterminated block comments and stale `@expect type` directives that previously crashed or silently passed.
- Remove deprecated `trace:` / `traceCr:` from Object (replaced by `show:` / `showCr:` in 0.3.1) (BT-1767).

### Standard Library

- **BREAKING: Supervisor lifecycle methods now return `Result`** ([ADR 0080](docs/ADR/0080-supervisor-lifecycle-result.md), BT-1993 epic) — `Supervisor class>>supervise` and `Supervisor>>terminate:` on the static path, plus `DynamicSupervisor class>>supervise`, `DynamicSupervisor>>startChild`, `DynamicSupervisor>>startChild:`, and `DynamicSupervisor>>terminateChild:` on the dynamic path, all return `Result` values instead of raising. `stop`, `current`, `children`, and `count` are unchanged.
  - Adopts the **idempotent-startup convention**: an operation succeeds (returns `Result ok: ...`) when the caller's target end state already holds — `supervise` on an already-running supervisor returns `Result ok: sup` (preserved from the prior runtime), and `terminate:` / `terminateChild:` on a child that is already gone returns `Result ok: nil` (new on the static path — previously raised).
  - **Mechanical migration.** Boot-style call sites add `unwrap`: `app := WebApp supervise` becomes `app := (WebApp supervise) unwrap`. Recoverable flows use `ifOk:ifError:` / `andThen:`. Call sites that were swallowing the raise to express "stop it if it's running" — e.g. `[app terminate: Child] on: Error do: [:_e | nil]` — can now be written as `(app terminate: Child) unwrap` (idempotent by construction — `Ok` whether the child was alive or already gone). See ADR 0080 §Migration Path for the full rewrite guide.
  - Structured `beamtalk_error` kinds (`kind` field): `#supervisor_start_failed`, `#child_start_failed`, `#terminate_failed`, `#stale_handle` — greppable in logs (BT-1999, BT-2000, BT-2001).
- **Named actor registration** ([ADR 0079](docs/ADR/0079-named-actor-registration.md)) — actors can now be registered under a `Symbol` name and looked up from anywhere via a name-resolving proxy that survives supervised restarts (BT-1985 epic, BT-1986..BT-1991):
  - `Class spawnAs: name` / `Class spawnWith: args as: name` — atomic spawn + register, returns `Result(Self, Error)`.
  - `Class named: name` — typed lookup returning `Result(Self, Error)`; `Self` resolves to the receiver class at the call site.
  - `actor registerAs: name` / `actor unregister` / `actor registeredName` / `actor isRegistered` — post-spawn registration API.
  - `Actor allRegistered` — enumerate Beamtalk-registered actors (excludes raw OTP kernel processes via the `$beamtalk_actor` process-dict marker).
  - `SupervisionSpec withName:` — declaratively name a supervised child so the supervisor re-registers the name on every restart.
- **Integer rounding methods** — `ceiling`, `floor`, `rounded`, and `truncated` on Integer return `self` (identity), so numeric code can call rounding methods on any `Number` without branching on type (BT-2011).
- Tighter parametric type annotations across stdlib classes (`File`, `Subprocess`, `ReactiveSubprocess`, `Regex`, `Result`, `SupervisionSpec`) — `Result` return types now carry concrete `T`/`E` parameters for better type flow through `andThen:`/`map:` chains.
- **`Supervisor>>which:` now returns `Result(Object, Error)`** — migrated to ADR 0080 Phase 2 Result signature, consistent with `supervise` and `terminate:`. Callers must `unwrap` or use Result combinators; returns `Result ok: nil` when the class is not in the supervision tree, `Result error: (beamtalk_error stale_handle)` when the supervisor is stopped (BT-2041).
- **`Collection(E)` parametrized** — `Collection` is now `Collection(E)` with typed block parameters on `do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`, `anySatisfy:`, `allSatisfy:`, and `includes:`. `Bag` is now `Bag(E)` with typed `add:`, `remove:`, `occurrencesOf:`. Enables type-safe iteration and better IDE support (BT-2036).
- `Float>>min:/max:` and `Integer>>min:/max:` return type corrected from `Float`/`Integer` to `Number` (BT-2020).
- **`Printable` protocol** — formal protocol abstracting `printString`/`displayString`; stdlib classes opt in and the type checker uses it to type generic display chains (BT-1766).
- **`Package` stdlib class** — first-class reflection over loaded packages (`name`, `classes`, `dependencies`, `version`); `packageName` on `Metaclass` resolves a class's owning package (BT-1657).
- **Protocol definitions are first-class class objects** — `Foo isProtocol`, `Foo conformingClasses`, `Foo methods` work uniformly with class reflection (BT-1928).
- **`Result fromTuple:`** — bridge from raw `{ok, V}` / `{error, R}` Erlang tuples into typed `Result` values (BT-1865).
- **Typed `Ets(K, V)`** — Ets table API gains key/value type parameters that flow through `insert:`, `lookup:`, etc. (BT-1860).
- **All stdlib classes carry the `typed` modifier** — every state declaration has a type annotation, every public method has a return type, FFI specs are filled in, and the build runs with 0 untyped-FFI warnings (BT-1827, BT-1828, BT-1829, BT-1836, BT-1825, BT-1826, BT-1933, BT-1952).
- **HTTP and YAML extracted to standalone packages** — `beamtalk-http` and `beamtalk-yaml` are now cross-repo packages consuming the public package API; the monorepo's `packages/http` was removed once the cross-repo CI build was green (BT-1718, BT-1741, BT-1742).
- `Workspace sync` expression — Beamtalk-level handle on `:sync` so scripts and tests can drive workspace reload without dropping into the meta-command surface (BT-1723).
- `Object` no longer overrides `module_for_value` for `FileHandle`, `Erlang`, `ErlangModule`, `StackFrame` — these are dispatched through the canonical class table now (BT-1761, BT-1762, BT-1763).

### Type System

The biggest theme of 0.4. The compiler now flows types through the full surface of the language and warns when inference falls back to `Dynamic` in a class that asked to be checked.

- **`typed` class modifier** — `typed Object subclass: Foo` opts a class into stricter checking: untyped state, untyped FFI calls, missing return types, and `Dynamic` inference all become diagnostics scoped to typed classes (BT-1829, BT-1913, BT-1914). Annotation completion is tracked across the stdlib and 178 → 12 untyped-FFI warnings cleared in one pass (BT-1952).
- **`DynamicReason` enum + LSP "Dynamic with reason" hover** — every `Dynamic` inference now records *why* (untyped FFI return, unresolved class, narrowing dropout, missing annotation, …); the LSP hover surface displays it inline so you can see why a chain "decayed" (BT-1911, BT-1912).
- **`beamtalk type-coverage` CLI** — per-file and per-class typed-vs-Dynamic ratios, gateable in CI ([ADR 0077](docs/ADR/0077-type-coverage-visibility.md), BT-1915).
- **Erlang → Beamtalk type mapping + `NativeTypeRegistry`** — Erlang spec atoms (`atom()`, `binary()`, `[T]`, `{ok, T} | {error, R}`, …) are translated into the Beamtalk type lattice at FFI boundaries, so calling an OTP function gives back a typed value ([ADR 0075](docs/ADR/0075-erlang-ffi-type-definitions.md), BT-1842).
- **FFI call inference + keyword-mismatch warning** — typed FFI calls now infer return type from the resolved spec and warn when the keyword form mismatches the underlying Erlang arity (BT-1843).
- **Generic type parameter resolution in return types** — `MyClass(T)>>foo: x -> List(T)` resolves `T` at the use site so chains of typed generic methods stop dropping to `Dynamic` (BT-1834, BT-1818).
- **Singleton type inference** — `#ok` and similar atom literals infer as singleton types and are checked against parameter declarations (BT-1830, BT-1878).
- **Union type validation everywhere** — fields, parameters, return types, locals; union-typed receivers resolve methods by intersecting member responses (BT-1832, BT-1857). Non-responding union members produce a single warning instead of widening the result with `Dynamic` (BT-1871, BT-1872).
- **`respondsTo:` narrows to a protocol type** — `obj respondsTo: #foo` inside an `ifTrue:` branch lets the checker infer `obj` as a synthesized "responds to `foo`" protocol instead of `Dynamic` (BT-1833).
- **`isOk` / `isError` narrowing on `Result(T, E)`** — both arms get the appropriate concrete branch type (BT-1859).
- **Typed state validation** — assignment to a typed `state:` is checked against its declaration; uninitialized typed-no-default fields warn at first use (BT-1831, BT-1947).
- **Block parameter type propagation from method signatures** — typed `Block(T, R)` parameters propagate `T` into the block body (#1970).
- **`@expect` reasons + `TypeAnnotation` diagnostic category** — `@expect type "<reason>"` records *why* a suppression exists, surfaced by lint and visible in the diagnostic summary (BT-1918, BT-1921, BT-1922).
- **Diagnostic deduplication** — repeated identical warnings on the same span are coalesced (BT-1921), and warning/hint diagnostics carry `.with_hint()` text consistently (BT-1922, BT-1923).
- **Self-type metatype** — `Self`-typed return values now substitute correctly through inheritance and through generic positions like `Result(Self, Error)` (BT-1952, BT-1986, BT-1992).
- **Generic unification** — fixes for nested type args, union shapes, FFI shapes, super sends, and class-method-call-bound locals (BT-2017, BT-2018, BT-2019, BT-2021, BT-2022, BT-2023, BT-2025).
- **Method-local type parameters** — `infer_method_local_params` generalized over arbitrary parametric types and gated on identifier validity (BT-1818, BT-1820, BT-1895).
- Class literals are accepted as `Behaviour` / `Class` arguments without a synthetic `class` send (BT-2038).
- `Foo class` (metatype) annotations work in any type position; nil-check narrowing clears DNU warnings on class-side methods (BT-2034).
- Conditional return type unification: `ifTrue:ifFalse:` declares `Block(R), Block(R) -> R` so both arms unify to a common return type (BT-2020).
- Warn when a method declared `-> Never` has a body that returns a known type (BT-2033).

### Compiler

- Fix `Self` type substitution in generic return types on parameterised receivers — e.g., `Result(Self, Error)` on `Box(Integer)` now correctly resolves to `Result(Box(Integer), Error)` instead of bare `Result(Box, Error)` (BT-1992).
- **Compile-time structural validation** — unresolved class names, FFI module names, and arity mismatches are now diagnostics surfaced before codegen, not opaque runtime errors (BT-1726).
- **Topological compilation ordering for dependencies** — packages compile in dependency order so cross-package class resolution succeeds on a clean build (BT-1647).
- **Fixture protocols thread through the BUnit compile path** — eliminates false `Unresolved class` warnings for protocols defined in test fixtures (BT-2006).
- Cross-file inherited typed-no-default field validation (BT-1976).
- Lint reduces false positives for uninitialized-field warnings (BT-1837).
- Warn when type args are supplied for a class with no type parameters (BT-1861).
- Codegen: route inherited class-method self-sends via a runtime helper so the superclass chain resolves correctly without re-entering the class gen_server (BT-2007).
- Codegen: route `self spawnAs:` / `self spawnWith:as:` in class methods through metaclass dispatch helpers to avoid `undef` (BT-2004).
- Fix codegen for class-method call results in sub-expression positions, including class-var mutations from sub-expression class-method self-sends (BT-1935, BT-1937, BT-1942).
- Codegen: emit visibility metadata in `__beamtalk_meta/0` (BT-1699).
- Codegen: emit qualified `bt@{package}@{class}` references for cross-package classes (BT-1652).
- Codegen: define `>>` as a `@primitive` method on `Behaviour` instead of a special case in the emitter (BT-1735).
- Wire stacktraces through `safe_dispatch` so actor callback errors carry full trace + source location (BT-1822).
- **Build performance** — Pass-2 builds are now incremental with file-level change detection, persisted class-metadata cache, and cached built-in `ClassHierarchy` via `OnceLock`; parser `advance()` no longer clones tokens; the Document renderer is faster on hot paths (BT-1677, BT-1678, BT-1680, BT-1682, BT-1683, BT-1684, BT-1685). Criterion benchmarks land in CI (BT-1674, BT-1686).
- **Dialyzer-friendly stdlib** — `beamtalk_result:t()` and `beamtalk_error:t()` exported types so user packages can write valid `-spec`s against the FFI surface (BT-1908).
- Singleton-union assignability fix; charlist coercion regression fix (a8ad07a9).
- Block-scoped variable mutation lint, dead-assignment `@expect` annotation, and other lint quality-of-life from the late-cycle batch.

### FFI

The FFI boundary stops being a `Dynamic` cliff. Most of this lives behind two ADRs.

- **Erlang FFI Type Definitions** ([ADR 0075](docs/ADR/0075-erlang-ffi-type-definitions.md)) — Erlang `-spec` attributes are read out of `.beam` abstract code (including dependency `.beam` files, not just OTP), translated through `NativeTypeRegistry`, and applied at the call site (BT-1840, BT-1841, BT-1842, BT-1906). `user_type` and `remote_type` references resolve transparently (BT-1902).
- **`ok`/`error` tuples → `Result` at the FFI boundary** ([ADR 0076](docs/ADR/0076-ok-error-tuple-to-result-at-ffi-boundary.md)) — calls to typed Erlang functions whose spec is `{ok, T} | {error, R}` are coerced into typed `Result` values automatically; the spec reader recognizes the union shape and synthesizes a `Result(T, E)` return type (BT-1864, BT-1867, BT-1868).
- **`beamtalk gen-native` / `beamtalk generate stubs`** — generate Beamtalk-side stub classes for Erlang modules from their EEP-48 docs and specs (BT-1214, BT-1850).
- **Native Erlang docs surface** — `:h Erlang <module>` lookup, REPL tab completion, MCP `docs` tool integration; `beamtalk_native_docs` reads EEP-48 attributes from `.beam`, supporting both EDoc-converted and EEP-59 attribute forms (BT-1851, BT-1852, BT-1903).
- **LSP for FFI** — hover, signature help, and go-to-definition on FFI calls (BT-1853).
- **EEP-59 doc attributes for native Erlang modules** — runtime modules migrated from EDoc to EEP-59; `:h` shows the original Erlang spec and supports type-definition lookup (BT-1900, BT-1901, BT-1905, BT-1960).
- Hardened FFI boundary validation and defensive patterns (BT-1927); resolve `binary_to_term` from untrusted `.beam` data with `[safe]` (BT-1876).
- Fix charlist coercion regression and bypassed coercion in `Result` wrapping (a8ad07a9, BT-1879).
- Fix false-positive "untyped FFI" warnings in typed classes (BT-1926, BT-2039).
- Filter hidden functions from `:h Erlang` and elide `Dynamic` in Result types (9b8c505c).
- Replace triple-fallback FFI spec lookup with a canonical selector-to-function mapping (BT-1925).
- Migration guide and ADR companion docs for the Result-at-FFI conversion (BT-1869).

### Packages

Beamtalk gains a real package manager. Builds, testing, and the REPL all flow through it.

- **`[dependencies]` in `beamtalk.toml`** — path, git, and hex (via vendored hex bridge) dependency declarations parsed by the build (BT-1644).
- **`beamtalk deps add | list | update`** — CLI surface that mutates `beamtalk.toml` and `beamtalk.lock` (BT-1649).
- **Implicit dependency fetch on `build` / `test` / `repl`** so first-run experiences "just work" (BT-1648).
- **Lockfile** — `beamtalk.lock` for both Beamtalk and `[[native_package]]` Erlang deps; lockfile entries replay across machines (BT-1715).
- **Path / git / hex resolution** — git dependencies clone into `_build/deps/` with revision pinning; hex packages flow through the vendored hex bridge; auto-clone on build; transitive native deps detected and compiled (BT-1645, BT-1646, BT-1648).
- **Qualified `pkg@Class` names** — lexer + parser + AST + semantic analysis support `mypkg@Foo` qualified references; codegen emits `bt@{package}@{class}` module names; collisions are detected at build time (BT-1650, BT-1651, BT-1652, BT-1653, BT-1659).
- **Cross-package class collision detection** — diagnostics fire when two dependencies export the same class without a qualifier; a transitive-dependency warning fires when a class flows through two non-direct hops; a `--strict-deps` mode upgrades these to errors (BT-1653, BT-1654).
- **`Package` reflection in REPL/MCP/LSP** — `:browse`, `:doc`, completions, and the MCP class index all show package provenance and filter internal classes/methods from cross-package completions (BT-1658, BT-1703, BT-1704).
- **Per-package `corpus.json`** — MCP discovery now serves per-package corpora rather than a monorepo-only file (BT-1722).
- **Generated `beamtalk_classes.hrl`** — native Erlang code includes generated header instead of hardcoding module references (BT-1730).
- Fix Package and Workspace dependency discovery for user packages, error propagation, and Package as a proper Value object (BT-1724, BT-1798, BT-1809).
- Cross-repo CI: pushes that touch the compiler trigger a downstream `beamtalk-http` build to catch surface breaks before release (BT-1742).
- `beamtalk new --app` ships a Justfile template alongside the project skeleton (BT-1738).

### Native Erlang in Packages

Beamtalk packages can now contain Erlang sources alongside `.bt` files ([ADR 0072](docs/ADR/0072-user-erlang-sources-in-packages.md)).

- **`native/*.erl` files compile as part of `beamtalk build`** via a vendored `rebar3` escript shipped in `runtime/tools/` (BT-1709, BT-1712, BT-1714, BT-1717).
- **`[native.dependencies]` in `beamtalk.toml`** — Erlang/OTP application dependencies parsed and threaded into a generated `rebar.config` (BT-1713).
- **`native_modules` in the `.app` file** — runtime knows which Erlang modules belong to which package, enabling per-package application start at workspace boot (BT-1710, BT-1724).
- **EUnit tests in the package test pipeline** — `beamtalk test` discovers and runs `native/test/*.erl` EUnit suites alongside BUnit tests (#1785).
- **EEP-48 doc generation for native modules** — `.beam` files compile with doc chunks so `:h Erlang <module>` works against user-defined modules too (BT-1900, BT-1905).
- **Native module collision detection across packages** — same module name across two deps is a build error with a useful diagnostic (BT-1711).
- **Demand-driven native compilation on single-file reload** — touching `native/foo.erl` rebuilds only that module on REPL `:sync` (BT-1717).
- **Include headers shipped in dist bundles** for native compilation outside the source tree (BT-1731).
- **Dynamic OTP app discovery in dist packaging** — Windows root walk, no hardcoded paths (BT-1779, BT-1784).
- **`beamtalk fmt` / `lint` cover `.erl` files** in addition to `.bt` (BT-1909, BT-2054).
- **ELP (Erlang LSP) configuration** lands so editor lint matches CI (#1958, #1959, #1962).

### Runtime

- **Named-registration intrinsics and supervisor wiring** — `beamtalk_actor:'spawnAs'/2,3`, `registerAs/2`, `unregister/1`, `named/2`, `allRegistered/1`, name-resolving `{registered, Name}` proxy dispatch, and supervisor routing of `SupervisionSpec withName:` through `beamtalk_actor:'spawnAs'/2,3` so supervised restarts re-register atomically (BT-1987, BT-1988, BT-1990).
- Reserved-name blocklist at registration time for OTP kernel/stdlib atoms and the `beamtalk_` prefix; returns `Result error: (beamtalk_error reserved_name)`.
- Fix `'spawnAs'/2` defaulting to `[]` instead of `#{}`, which crashed supervised named children in `init/1` with `{badmap, []}` (BT-1991).
- Fix REPL JSON formatter crashing when displaying a name-resolving proxy: `#beamtalk_object{pid = {registered, Name}}` now renders as `#Actor<Class,registered,Name>` instead of calling `pid_to_list/1` on a non-pid term (BT-1991).
- **Supervisor lifecycle Result migration** ([ADR 0080](docs/ADR/0080-supervisor-lifecycle-result.md), BT-1993 epic) — runtime functions in `beamtalk_supervisor.erl` (`startLink/1`, `startChild/1,2`, `terminateChild/2` in both arities, `with_live_supervisor/3`) now return `{ok, V} | {error, #beamtalk_error{}}` instead of raising. FFI coercion (ADR 0076) lifts these to `Result` values at the Beamtalk boundary. The `beamtalk_class_dispatch` post-dispatch hook was extended to match both the bare `{beamtalk_supervisor_new, ...}` tuple and a `Result` tagged map wrapping it, preserving the ADR 0059 / BT-1285 guarantee that `class initialize:` runs in the caller's process (BT-1994, BT-1996, BT-1997, BT-1998).
- `terminateChild` is now idempotent across both static and dynamic supervisor paths — terminating an already-terminated child returns `{ok, nil}` instead of raising. Previously only the dynamic path had this behavior (BT-1998).
- Fix deadlock when calling `self class spawnAs:` / `self class spawnWith:as:` inside a class factory method — metaclass dispatch now short-circuits spawn selectors to avoid re-entering the class gen_server (BT-2005).
- Fix `undef` crash for `self spawnAs:` / `self spawnWith:as:` in class methods — new runtime helpers read class metadata from the process dictionary to avoid the gen_server deadlock (BT-2004).
- Fix `undef` crash for inherited class-method self-sends — dispatch now walks the superclass chain correctly while preserving class-var state (BT-2007).
- **Workspace project loads accumulate** — loading project A then project B on the same REPL/MCP workspace no longer evicts project A's classes. Previous-mtime tracking is now scoped per-project root, so each `:sync` / `load_project` only treats files under its own tree as candidates for "deleted" classification. Cross-project class collisions surface as `warnings` in the load-project response instead of silently overwriting earlier classes (BT-2089).
- Fix spurious namespace collision error when hot-reloading a `Protocol define:` file across surfaces (e.g., MCP after REPL) — the compiler now filters pre-loaded protocol class entries before hierarchy injection, and protocol re-registration is allowed when the existing class has superclass `Protocol` (BT-2088).
- **Class crash recovery via ETS heir** — class processes that crash are detected and restarted; ETS class metadata survives via the `heir` option so live actors keep their dispatch table (BT-1768, BT-1888).
- **Self-send detection and cycle detection for actors** — runtime watchdog catches the common deadlock of `self foo` (synchronous self-send to a busy mailbox) and reports it as a structured error rather than hanging (BT-1325).
- **Auto-chained `initialize`** ([ADR 0078](docs/ADR/0078-actor-initialize-inheritance.md)) — supervisor / dynamic supervisor / actor base class run inherited `initialize` automatically up the chain; uninitialized typed-no-default fields are validated at construction (BT-1949, BT-1951).
- **`UninitializedStateError`** when a typed-no-default state field is read before being set (BT-1949).
- **`safe_dispatch` preserves stacktraces** and propagates the original exception class instead of collapsing to a generic error (BT-1822, BT-1889).
- **Auto-cloning git deps during build** with transitive-dep tracking (BT-1788).
- **`code:lib_dir/2` deprecation cleanup** in beamtalk_stdlib for OTP 27+ (49bac205).
- Backward-compat error path no longer discards exception class (BT-1889).
- Class registry / protocol registry / dispatch pipeline coverage raised to ≥85% with new EUnit suites (BT-1958..BT-1984).
- Replace `jsx` with the OTP 27+ built-in `json` module (BT-1671).

### REPL & Tooling

- **`:sync` REPL meta-command replaces `:load` / `:reload`** — single command does the right thing per file (load if new, reload if changed, drop if deleted), with auto-sync of test files when running `:test` / `:t` (BT-1707, #1803).
- **`:test` / `:t` parallel execution + failure detail** — REPL test runner uses parallel workers and prints failure detail per test, matching `beamtalk test` output (BT-1668, BT-1669, BT-1673).
- **`:browse` and `:doc` filter internal entries** and annotate dependency provenance (BT-1704).
- **`:help Erlang <module>` + tab completion** — discoverable native FFI from inside the REPL (BT-1852).
- **`beamtalk type-coverage` CLI command** — per-file and per-class typed/Dynamic ratios (BT-1915).
- **`beamtalk gen-native` and `beamtalk generate stubs` subcommands** (BT-1214, BT-1849, BT-1850).
- **`beamtalk deps` subcommand group** (`add` / `list` / `update`) (BT-1649).
- **`beamtalk logs` --log-level CLI option** for actor initialize error reporting (#1792).
- **`beamtalk doctor`** environment verification surface stays current with new dependencies.
- **Justfile template + `--app` flag for `beamtalk new`** (BT-1738).
- **MCP `load_project` reports failed file paths** when individual files don't compile (BT-1855).
- **Surface parity test harness** (`tests/parity/`) drives identical input through REPL, MCP, CLI, and LSP and asserts equivalent output (BT-2077).
- **`docs/development/surface-parity.md`** + `just check-surface-drift` CI gate keep documented operations aligned with code (BT-2075, BT-2082).
- **Surface-only audit and promote-or-lock decisions** for asymmetric ops (BT-2083).
- **LSP improvements**:
  - Workspace symbol search (BT-2081).
  - Go-to-Definition on method headers navigates to overridden parent (BT-1939).
  - Find-refs / goto-def on constructor pattern class names (BT-1940).
  - Goto-def and find-refs for protocol names (BT-1936).
  - Find All References on method definition headers (BT-1938).
  - Local variable + `self.field` completions (#1956).
  - Completions for protocol class objects (BT-1933).
  - Hover, signature help, and goto-def for FFI (BT-1853).
  - Single-pass `find_overridden_method_definition` perf fix (BT-1943).
  - Filter internal entries from cross-package completions (BT-1703).
  - Build cache integration + typed completions (BT-1844).
  - Typed completions emit FFI return-type hints (BT-1844).
- **VS Code extension fixes** — init flow, binary discovery, esbuild ^0.28 to satisfy vite peer (#1947, #1948).
- **MCP**:
  - Native EEP-48 doc lookup wired through MCP `docs`.
  - `load_project` activates dependency classes during bootstrap (#1796).
  - Lint surface unreadable target files (BT-2056, BT-2067).
  - REPL diagnostics parity audit and unification with CLI (BT-2009, BT-2052).
  - MCP `test` tool timeout raised for large suites (BT-1668).
- Improve error messages for dependency and native compilation failures (BT-1732).
- Use `print_string` instead of `~p` for user-facing error messages (#1791).
- BUnit `--quiet` flag and per-class test output cleanup (#1932); Beamtalk class names + source line numbers in stack traces (preserved from 0.3.1, hardened in this cycle).
- Fix `beamtalk lint` / `fmt` test-fixture handling and stale `_build/deps` cleanup after dep resolution (BT-1907).
- Cross-platform CI: `beamtalk-exec` Job Objects on Windows; consistent temp-dir handling across macOS/Linux/Windows (BT-1133, #1783, #1896).

### Visibility & Access Control

New language modifier ([ADR 0071](docs/ADR/0071-class-visibility-internal-modifier.md)).

- **`internal` modifier** for classes and methods — package-scoped access; cross-package use is a compile-time error (E0401, E0402, E0403) with method-level warning (W0401) for borderline cases (BT-1698, BT-1700, BT-1701, BT-1702, BT-1705).
- **Codegen emits visibility in `__beamtalk_meta/0`** so reflection and the LSP can mirror the source-level rule (BT-1699).
- **Formatter preserves `internal` modifier** on classes and methods (#1731).
- **REPL `:browse` / `:doc` and LSP completions filter internal entries** when crossing a package boundary (BT-1703, BT-1704).
- E2E coverage for internal-visibility scenarios (BT-1705).

### Diagnostics, Lint & Build Tooling

- **Diagnostic summary** — `beamtalk build` and `beamtalk lint` print an aggregated diagnostic summary (category × severity) at end of run; `beamtalk lint --format json` emits a `summary` JSON object for CI diffing; new `diagnostic_summary` MCP tool for agent use (BT-2014).
- Fix `beamtalk lint` diagnostic summary accounting — `files_checked` count, error surfacing, and `--format json` buffered output now report correctly (BT-2031).
- Fix `beamtalk lint test/` emitting spurious `Unresolved class` diagnostics for classes defined in the package's `src/`; fix LSP falsely flagging every class as "conflicts with a stdlib class" when opening stdlib `.bt` files (BT-2027).
- Fix `beamtalk lint` false-positive on keyword-form function calls inside Erlang function bodies being misidentified as undocumented exports (BT-2053).
- Fix `beamtalk fmt` silently failing on `.erl` files containing non-ASCII Unicode characters (BT-2054).
- Fix MCP `lint` tool producing different diagnostics from CLI `beamtalk lint` for cross-file type/DNU issues (BT-2052).
- MCP `lint` tool now surfaces warning diagnostics when package files are unreadable during cross-file class extraction (BT-2056).
- Preserve `startup.log` on final workspace retry failure so error diagnostics referencing the file remain valid (BT-2057).
- MCP `lint` tool now surfaces error diagnostics when direct target files are unreadable, instead of returning a deceptively clean zero-files-checked result (BT-2067).
- LSP `workspace/symbol` search — find class definitions across the workspace from the editor's symbol picker (BT-2081).
- `just test-repl-protocol` replaces `just test-e2e`; deprecated alias kept for one release cycle (BT-2085).
- `just check-surface-drift` CI gate ensures documented surface parity stays in sync with code (BT-2082).
- **BREAKING: REPL protocol 2.0 — deprecated ops `docs`, `load-file`, `reload`, and `modules` removed.** WebSocket clients sending these ops now receive an `unknown_op` error. Migrate to the equivalent eval'd message-sends: `Beamtalk help: ClassName` (optionally `selector: #sel`), `Workspace load: "path"`, `ClassName reload`, `Workspace classes`. The `versions.protocol` field returned by `describe` is bumped from `1.0` to `2.0` to mark the break. The MCP tools `docs`, `load_file`, and `reload_class` continue to work — they were already routed through `evaluate` of the migration target. The CLI's `:help`, `:load`, and `:reload` REPL meta-commands are unaffected (they desugar to the new message-sends locally) (BT-2091).
- `:interrupt` / `:int` REPL meta-command — sends an out-of-band interrupt to cancel a running evaluation. Because the main connection is blocked during eval, the interrupt is sent on a separate connection. Also fires automatically on Ctrl-C during eval (BT-2090).
- Sealed-class and sealed-method constraint diagnostics now reach CLI and MCP lint surfaces — new `inheritance` diagnostic category ensures these are no longer silently dropped (BT-2087).

### Documentation

- **ADR 0071**: Class Visibility — Internal Modifier.
- **ADR 0072**: User Erlang Sources in Beamtalk Packages.
- **ADR 0073**: Package Distribution and Discovery.
- **ADR 0074**: Deferred Metaprogramming (BT-303).
- **ADR 0075**: Erlang FFI Type Definitions (BT-1823).
- **ADR 0076**: Convert Erlang `ok`/`error` Tuples to `Result` at FFI Boundary (BT-1838).
- **ADR 0077**: Type Coverage Visibility.
- **ADR 0078**: Actor Initialize Inheritance.
- **ADR 0079**: Named Actor Registration.
- **ADR 0080**: Migrate Supervisor Lifecycle to Result — Implementation Tracking section lists the full BT-1993 epic breakdown (8 child issues across Phase 0 probes, Phase 1 runtime, Phase 2 stdlib, Phase 3 e2e + docs).
- Language features: supervision chapter rewritten to document the Result-shaped lifecycle API, the idempotent-startup convention ("target end state holds = `Ok`"), and boot-style vs recoverable call-site patterns; cross-linked to Actor Named Registration for the parallel registry/boundary pattern (BT-2001).
- Language features: new "Named Actor Registration" chapter (BT-1991).
- Language features: new "Local Variable Type Annotations" section; updated `@expect` documentation to reflect block-body support.
- Language features: type-coverage visibility, package management system, native Erlang integration, REPL commands and interactive development, build system / incremental compilation, visibility and access control, stdlib updates (Printable protocol and new APIs) (BT-1804, BT-1805, BT-1806, BT-1807, BT-1808, BT-1809, BT-1810).
- FFI Result conversion + migration guide (BT-1869).
- Refresh of stale docs: known limitations, installation, version refs (BT-1810).

### Internal

- Extract `beamtalk-repl-protocol` crate — shared REPL response types and `RequestBuilder` replacing duplicated structs in CLI and MCP (BT-2076).
- Cross-surface parity test harness (`tests/parity/`) — drives identical input through REPL, MCP, CLI, and LSP and asserts equivalent output (BT-2077).
- Shared output formatters for diagnostics, values, and trace steps — eliminates ~130 lines of duplicated rendering between CLI and MCP (BT-2086).
- Extend `WellKnownSelector` to cover remaining selector-universal intrinsics: block loops, ensure, hash, error, field reflection, perform family (BT-2073).
- ADR 0080: Migrate Supervisor Lifecycle to Result — proposed, accepted, and implemented across Phase 0 probes, Phase 1 runtime, Phase 2 stdlib, Phase 3 e2e + examples + docs (BT-1977, BT-1993..BT-2001).
- Typechecker probe confirms class-level type parameter substitution through `Result(C, Error)` works without extension (BT-1995).
- Unified LSP and CLI diagnostic pipelines into shared `compute_project_diagnostics` function (BT-2009).
- Thread fixture-defined protocols through BUnit compile path to eliminate false `Unresolved class` warnings (BT-2006).
- Centralised parametric type resolution into shared `type_resolver` submodule — infrastructure for BT-2018..BT-2023 bug fixes (BT-2025).
- Fix generic return type args lost when assigned to a local from a class method call (BT-2018).
- Fix receiver type_args not threaded through super sends in generic classes (BT-2021).
- Fix generic unification failures on nested, union, and FFI type shapes (BT-2023).
- Fix union return types stored as flat `Known` instead of proper `Union`, causing false numeric-operand warnings (BT-2017).
- Fix `Nil` in type annotations not canonicalized to `UndefinedObject`, causing `isNil` narrowing to silently fail (BT-2016).
- Preserve type_args on concrete parametric instance-method returns (BT-2019).
- Propagate `-> Never` through class-side dispatch fallback so `self error:` infers as Never (BT-2037).
- Accept class literals as valid arguments for `Behaviour`/`Class` typed parameters (BT-2038).
- Dedup cascade receiver inference so inner DNU warnings fire once instead of duplicating (BT-2035).
- Fix spurious untyped-FFI warning on narrowing-path blocks in Result.bt (BT-2039).
- Remove 11+ stale `@expect type` directives across stdlib following type checker improvements (#2076).
- Split 16k-line `type_checker/tests.rs` into 28 per-feature test modules (BT-2061).
- Typed `EnvKey` enum replaces string-key convention for type environment entries (BT-2062).
- Typed `WellKnownClass` enum replaces fragile class-name string comparisons across type checker (BT-2064).
- Refactor narrowing into `NarrowingRule` table with per-rule modules and env-level refinement (BT-2050).
- Unify workspace retry helpers into single parameterized `retry_with_cleanup` wrapper (BT-2058).
- Replace erlfmt inline Erlang eval strings with structured `ErlangEval` builder (BT-2059).
- Share package-root resolution between MCP lint and CLI lint via `project::package` module (BT-2060).
- `WellKnownSelector` enum replaces fragile selector-name string comparisons across narrowing rules, codegen intrinsic dispatch, lint validators, and state-threading predicates (BT-2065 epic: BT-2069, BT-2070, BT-2071, BT-2072).
- Generic `Visitor` trait with exhaustive `walk_expr` replaces hand-rolled AST walkers in narrowing and type inference (BT-2063).
- Test layout cleanup: rename `tests/e2e/` to `tests/repl-protocol/` (and harness `crates/beamtalk-cli/tests/e2e.rs` to `repl_protocol.rs`) because these tests exercise the REPL TCP surface, not "end-to-end across surfaces". `just test-e2e` remains as a deprecated alias for one release; use `just test-repl-protocol` going forward (BT-2085).

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
