# Changelog

## Unreleased

### Language

- **Character literal dispatch** — character literals (`$A`, `$a`, etc.) now dispatch through the Character method table instead of Integer's. `$A asString` returns `"A"` (not `"65"`), `$A class` returns `Character`, and methods like `uppercase`, `lowercase`, `printString` work correctly on character literals. `Character value: 65` class-method sends also work (BT-2095).
- **`@expect inheritance` diagnostic category** — sealed-class and sealed-method constraint diagnostics are now categorized as `Inheritance`, so they reach CLI and MCP lint surfaces that filter to categorized diagnostics. Previously these diagnostics were uncategorized and only appeared in the LSP (BT-2087).
- **Class-side block parameter type propagation** — the type checker now infers block parameter types from class-side method signatures (e.g., `ClassName build: [:r | ...]` where the method declares `Block(RouteBuilder, R)` correctly types `r` as `RouteBuilder`), eliminating the need for `@expect type` pragmas on class-side sends and cascades (BT-2158).
- **Metaclass-aware type inference (ADR 0083 Slice 1)** — metatypes are first-class in inference. `Self class` / `ClassName class` annotations resolve to a tracked metatype instead of `Dynamic`. Sends on metatype receivers route to class-side lookup with metaclass tower fallback. `new`/`basicNew` on concrete metatypes infer instances. `Meta{C} <: Class <: Behaviour` subtyping in validation. LSP completion routes metatype receivers to class-side, and hover renders `C class` (BT-2255).
- **Class literal metatype inference** — bare class literals (e.g., `Counter`) now infer as the metatype `Counter class` instead of as an instance type. A class value stored in a variable, passed through a collection, or returned from FFI routes class-side sends correctly without annotation — `klass := Counter. klass new` infers a `Counter` instance (BT-2260).
- **Typed FFI collection element types** — Erlang `-spec` list/tuple element types now carry through to Beamtalk types: `[integer()]` → `List(Integer)`, `{atom(), binary()}` → `Tuple(Symbol, String | Binary)`. Uninformative elements (`term()`, bare `tuple()`) collapse to the unparameterized collection type. Literal-index `at:` on typed `Tuple(T1, …, Tn)` infers the element type at that 1-based position; non-literal or out-of-range indices fall back to `Dynamic` (BT-2254).
- Fix foreign cross-class extension methods (`TargetClass >> sel => …` where the target is defined in another file) being silently dropped by codegen — the compiler now emits `beamtalk_extensions:register/5` at module load for each foreign extension, so they dispatch correctly at runtime. Class-side extensions register under the metaclass tag. Pure-extension files (no host class) now emit `register_class/0` + `on_load` (BT-2250).
- **`ClassBuilder classMethods:` arity validation** — a `classMethods:` block whose parameter count does not match its selector (e.g. `#{ #greet => [42] }`, forgetting the leading `:self`) is now rejected at compile time with a clear diagnostic naming the selector and expected shape, instead of silently lowering to a wrong-arity fun that crashed with an opaque `error:undef` only when the class method was first called. A computed (non-literal) `classMethods:` fun of the wrong arity is rejected at registration time with a structured `#beamtalk_error{}` (`arity_mismatch`), since its arity is unknown until runtime. Correctly-shaped blocks and funs (following the `fun(ClassSelf, ClassVars, …)` convention) are unaffected (BT-2276).
- **`Behaviour compile:source:` / `tryCompile:source:`** — keyword method-patching primitives (ADR 0082 Phase 1). `aClass compile: #sel source: "body"` installs a method and logs a durable ChangeEntry; `tryCompile:source:` does the same with ephemeral intent. Both take the body as a String value (no escaping needed) and are equivalent in effect to `>>` — tools (MCP, LSP, browser editors) call these directly. Every successful in-memory patch (including `>>`) now emits a ChangeEntry to the workspace ChangeLog (BT-2283).
- Fix `addClassMethod:body:` allowing `super` in blocks with non-literal selectors — the name resolver now mirrors codegen's literal-symbol gate, correctly rejecting `super` when the selector is not a compile-time known symbol (BT-2279).
- Fix `Integer to:do:` / `to:by:do:` not threading outer-variable mutations back to the caller — `last := 0. 1 to: 5 do: [:i | last := i]` now correctly leaves `last` at 5 instead of silently dropping the mutation (BT-2308).
- Fix float literals that overflow `f64` (e.g. `1e309`) producing confusing `erlc` failures — now rejected at parse time with a clear "Float literal out of range" diagnostic (BT-2338).

### Standard Library

- **Supervision-tree introspection (ADR 0092)** — `Workspace processes` returns a navigable `SupervisionTree` snapshot of the live OTP supervision tree (the dynamic twin of `SystemNavigation`, the process-structure counterpart to `Workspace actors`). New stdlib classes `ProcessNavigation` (`default` / `system` / `from:` constructors), `SupervisionTree` (collection protocol — `root`, `nodes`, `size`, `do:`, `select:`, `detect:`, `collect:`, `nodesOfKind:`, `findClass:` — plus serialisable `asDictionaries`), and `SupervisionNode` (an immutable record: `pid`, `registeredName`, `kind`, `behaviourClass`, `childCount`, `strategy`, `restartIntensity`, `children`, `parent`, `isSupervisor`, `isBeamtalk`, and the lazy timeout-guarded `status`). `kind` classifies each process as `#beamtalkActor` / `#beamtalkSupervisor` / `#otpSupervisor` / `#otpProcess` / `#restarting`. Snapshots are best-effort point-in-time (re-call to refresh); `node status` is a lazy `sys:get_status` fetch returning `nil` for a dead/non-`sys` process. Backed by a pure OTP wrapper (`beamtalk_process_navigation`) — no new bookkeeping process. The `default` scope filters runtime plumbing (an ADR-0091 Read op); `system` (everything, incl. runtime internals) is privileged, and a large `simple_one_for_one` `DynamicSupervisor` is reported `truncated` past a child cap. Surfaced to MCP (`supervision_tree` tool) and the LiveView IDE (`processes` / `processes_system` ops) (BT-2425).
- **`SystemNavigation`** — `implementorsOf:` and `sendersOf:` moved off `Beamtalk` onto a new `SystemNavigation` class, matching Pharo/Squeak/Cuis convention. Reach an instance via `SystemNavigation default` and dispatch queries against it: `SystemNavigation default implementorsOf: #foo`, `SystemNavigation default sendersOf: #foo`. The class side is constructors only — `default` today, with `over: aPackage` and `forClasses: aList` planned for the BT-2201 query API expansion — so future scoped queries fit cleanly without breaking the protocol. The methods on `Beamtalk` are removed outright (no deprecation period; pre-1.0 reflective queries with no broad surface area) (BT-2214).
- **`SystemNavigation methodsMatching:`** — new source-text grep query that returns `{class, selector}` records for every loaded method whose `CompiledMethod source` matches a compiled `Regex`. Argument must be a `Regex` (raises a typed error on a raw `String` so callers stay explicit about compile-once semantics). Iterates `allClasses` and skips methods whose source is missing (BT-2205).
- **`SystemNavigation` class-side scanning** — `sendersOf:`, `referencesTo:`, and `methodsMatching:` now walk class-side method bodies in addition to instance-side ones. Each result's `#class` field is the class object for an instance-side hit and the metaclass object (`Counter class`) for a class-side hit. Required two runtime pieces: class-side `CompiledMethod.source` is now populated end-to-end (codegen emits `classMethodSource`, the class gen_server stores it, and the `{class_method, _}` reply plumbs it through), and `>>` now accepts a metaclass receiver (the method resolver routes `class = 'Metaclass'` objects to the class-side dictionary, walking the parallel metaclass hierarchy) (BT-2195).
- **`SystemNavigation` extension method scanning** — `sendersOf:`, `referencesTo:`, and `methodsMatching:` now include extension method bodies (registered via `>>`) in their scans. Extension methods registered with source text are fully visible; legacy extensions without source remain excluded. Each extension hit reports the target class object (or metaclass for class-side extensions) in the `#class` field (BT-2196).
- **`ClassBuilder` block-method source indexing** — classes built programmatically via the `ClassBuilder` cascade API (`Object classBuilder name: #C; methods: #{ #m => [block] }; register`) now expose their instance-method bodies to `SystemNavigation` source-text queries. The compiler recognises the builder construction (terminating in `register`) and auto-populates `methodSource:` from the literal block bodies (reconstructed via the unparser), so builder-defined instance methods are visible to `sendersOf:` / `methodsMatching:` like file-defined ones. Methods built from computed (non-literal) funs have no source literal and remain known-present but unindexable, never crashing the scan. Class-side methods were out of scope at the time but are now supported too — the programmatic builder gained callable class methods (BT-2267) and class-side `classMethodSource` indexing (BT-2270) (BT-2246).
- **`ClassBuilder` incremental class-piece API** — `addMethod:body:`, `addClassMethod:body:`, `addClassState:default:`, and the matching `removeMethod:` / `removeClassMethod:` let a class be assembled one piece at a time before `register` (the "browser" use case), as the incremental counterparts of the bulk `methods:` / `classMethods:` / `classVars:` setters. They write the same builder state maps, so they compose freely with the bulk setters. In a `classBuilder … register` cascade the compiler lowers each `add*:body:` block exactly as it lowers the corresponding bulk-setter map value — instance blocks become `fun(Self, Args…)` methods and class blocks become class-method funs that thread class-variable state through `{class_var_result, …}`, so this does not reproduce the BT-873 closure-dispatch regression (state mutation is compiler-lowered, never a naive runtime closure) (BT-2269).
- **`SystemNavigation unimplementedSelectors`** — returns selectors that are *sent* somewhere in the loaded registry but *defined* nowhere — a typo-finder lint. Each result is a `#{#selector, #sites}` record where each site has `#{#class, #selector, #line}` for IDE jump-to-source. Excludes Erlang FFI sends, `doesNotUnderstand:args:` DNU dispatchers, and parse-recovery phantoms to minimize false positives (BT-2206).
- **`SystemNavigation unusedSelectors`** — returns selectors *defined* on some class but *sent* nowhere — dead-method candidates. Each result is a `#{#class, #selector}` record (class object for instance-side, metaclass for class-side). Conservatively excludes runtime-called overrides (`initialize`, `printString`, `hash`, etc.), TestCase lifecycle methods (`setUp`/`tearDown`/`test*`), and `@primitive`/`@intrinsic` methods (BT-2207).
- **`Behaviour >> superclassChain`** — returns the chain from self up to and including `Object` as a list. Self is at the head; `Object` is at the tail. For metaclass receivers, walks the parallel metaclass hierarchy: `Counter class superclassChain` returns `[Counter class, Actor class, Object class]` (BT-2189).
- **`Behaviour >> name` promoted from `Class`** — `name` now lives on `Behaviour` (the abstract superclass of `Class` and `Metaclass`) so registry walks holding the `Behaviour` type can read it without `@expect dnu`. `Metaclass >> name` now returns a `Symbol` (e.g. `#'Integer class'`) instead of a `String`, matching `Class >> name` (BT-2232).
- **`Behaviour >> classVarNames` / `allClassVarNames`** — class-side reflection that enumerates a class's class variable names (the slots declared with `classState:`), the class-side counterparts to `fieldNames` / `allFieldNames`. `allClassVarNames` walks the superclass chain in slot order. Backed by a new `class_fields` key in `__beamtalk_meta/0`. Distinct selectors are used (rather than reusing `fieldNames` on the metaclass) because `fieldNames` is intercepted at the call site as an instance-reflection primitive and never reaches the method table (BT-2238).
- **`SystemNavigation actorClasses`** — returns every class whose superclass chain includes `Actor`, sorted alphabetically. Includes `Actor` itself; returns `[]` if `Actor` is not loaded (BT-2209).
- **`SystemNavigation dnuHandlers`** — returns every class that locally overrides `doesNotUnderstand:args:` (the DNU handler), sorted alphabetically. Useful for identifying proxy/builder/DSL classes that absorb arbitrary sends (BT-2210).
- **`SystemNavigation extendersOf:`** / **`extensionsBy:`** — reverse extension-method navigation. `extendersOf: aClass` returns the packages contributing extension methods to `aClass`; `extensionsBy: aPackage` returns `#{#class, #selector}` records for each extension method a package contributes (BT-2212).
- **`SystemNavigation selectorsMatching:` performance** — accumulator changed from O(n²) left-append to O(n) right-append. No behavior change (BT-2218).
- **`SystemNavigation ffiSitesFor:`** — returns `#{#class, #selector, #line}` for every method body that calls Erlang `module:function` (optionally arity-qualified, e.g. `"lists:reverse/1"`). Scans instance-side, class-side, and extension method bodies (BT-2211).
- **`SystemNavigation fieldReadersOf:in:` / `fieldWritersOf:in:`** — field/class-var usage queries. `fieldReadersOf: #slot in: aClass` returns `#{#class, #selector, #line}` for every method that reads `#slot`, scanning `aClass` + subclasses on instance and class sides. `fieldWritersOf:in:` does the same for writes (BT-2208).
- **`SystemNavigation classesInPackage:` / `subclassesOf:in:`** — package-scoped class queries. `classesInPackage: aPackage` returns class objects belonging to a package; `subclassesOf: aClass in: aPackage` filters `allSubclasses` by package (BT-2213).
- **`ChangeLog` and `ChangeEntry`** — new stdlib classes exposing the workspace ChangeLog as a navigable Beamtalk object (ADR 0082 Phase 1). `Workspace changes` returns a `ChangeLog` with collection protocol: `size`, `isEmpty`, `notEmpty`, `do:`, `select:`, `dirtyMethods`, `activeEntries`, `allEntries`. Each `ChangeEntry` wraps one logged patch with predicates (`isOrphan`, `isActive`, `isDurable`, `isEphemeral`, `isAgent`, `isHuman`, `isFlushable`, `isNewClass`) and accessors (`className`, `selector`, `kind`, `intent`, `authorKind`, `sourceFile`, `seq`). Default views reflect only active entries (current epoch, not orphaned); `select:` ranges over the full set (BT-2284).
- **`ClassBuilder >> register` returns canonical class object** — previously returned an unusable wrapper that failed all dispatch; now returns the same shape as `classNamed:` (dispatchable, `isClass: true`, `==` to the registry reference) (BT-2258).
- **`ClassBuilder` metadata parity setters** — `methodSignatures:`, `classMethodSignatures:`, `methodDocs:`, `classMethodDocs:`, `methodReturnTypes:`, `classMethodReturnTypes:`, `classDoc:`, `meta:`, and `isConstructible:` bring programmatically built classes to `:help` parity with file-defined ones (BT-2268).
- **`Workspace newClass:at:`** — compiles and installs a brand-new class in memory from a source String and target path, logging a durable `new-class` ChangeEntry for later `Workspace flush`. Validates that the target does not already exist on disk, lies within the project source tree, matches the declared class name, and is not already loaded (BT-2285).
- **`ChangeLog revert:` / `clear` / `flushKinds:` and `Workspace autoflush`** — `revert:` re-installs a ChangeEntry's previous method body via durable `compile:source:`, `clear` discards all pending entries without touching disk, and `flushKinds:` selectively flushes entries filtered by kind (`#instance`, `#class`, `#'new-class'`) and/or author kind (`#human`, `#agent`). `Workspace autoflush` / `autoflush:` is a persistent workspace setting that, when enabled, immediately flushes every successful durable in-memory patch to disk (BT-2290).
- **`SystemNavigation messagesSentBy:`** — returns `#{#selector, #line}` for every message send in a `CompiledMethod`'s body — the outgoing-call dual of `sendersOf:`. Excludes Erlang FFI sends. Backs the `callHierarchy/outgoingCalls` LSP query (BT-2243).
- **Announcements stdlib veneer (ADR 0093 Phase 2)** — new `Announcement`, `Announcer`, `SystemAnnouncer`, and `Subscription` classes for typed event publishing. `Announcer new` mints a fresh dispatcher; `when:do:` / `when:send:to:` / `when:doOnce:` subscribe; `announce:` / `announceAndWait:` dispatch; `unsubscribe:` removes a handler. MRO matching: subscribing to a superclass receives subclass events. `SystemAnnouncer current` is the singleton system bus (async-only — `announceAndWait:` raises `UnsupportedOperation`). `Subscription` tracks handler liveness via `isActive` (BT-2443).
- **Announcements introspection (ADR 0093 Phase 2)** — new `AnnouncementNavigation` and `SubscriptionNode` classes for querying the announcements bus. `AnnouncementNavigation default subscribersOf: ActorSpawned` discovers subscribers; `announcedClasses` lists distinct event types in use; `AnnouncementNavigation of: anAnnouncer` scopes to one announcer. `Announcer` self-inspection via `subscriptions` also available. The third navigation sibling alongside `SystemNavigation` / `ProcessNavigation` (BT-2444).
- **System `Announcement` subclasses (ADR 0093 Phase 3)** — well-known typed system events the runtime publishes on `SystemAnnouncer current`: `ActorSpawned` / `ActorStopped` (actorClass, pid, reason), `ClassLoaded` / `ClassRemoved` (className), `BindingChanged` (name, value), and `SupervisionChildAdded` / `SupervisionChildCrashed` (supervisor, childClass, childPid, reason). A tool subscribes once and filters by event class instead of wiring four bespoke notification channels — e.g. `SystemAnnouncer current when: ActorSpawned do: [:e | Transcript showLine: e actorClass asString]` (BT-2445).
- **Object string representation (ADR 0094)** — `printString` defaults redesigned. The `a`/`an` article prefix is removed entirely. Value subclasses now render as `ClassName(field: value, ...)` with labelled fields in sorted order (e.g. `Point(x: 3, y: 4)`; no fields → `Point()`). Actor instances render as `Actor(ClassName, pid)`, supervisors as `Supervisor(ClassName, pid)` / `DynamicSupervisor(ClassName, pid)`. Plain objects render as bare `ClassName`. `displayString` (the string interpolation hook) defaults to `printString`. Blocks render as `Block/N` (was `a Block/N`) (BT-2459, BT-2460, BT-2461, BT-2462).
- **`SystemNavigation announcementsSentBy:`** — returns distinct `Announcement` subclasses a class emits via `announce:` / `announceAndWait:` / `announceAndWait:timeout:`, resolved statically from constructor-call arguments. `announcementSitesSentBy:` is the site-level form (`#{#class, #selector, #line, #announcementClass}`). Advisory: `Dynamic`/indirect arguments are skipped. The publisher-side static dual of `AnnouncementNavigation` (BT-2475).
- **Announcer per-instance subscription isolation** — each `Announcer new` now mints a fully independent dispatcher. Subscriptions on one announcer are never matched by an `announce:` on another, even for the same event class. `AnnouncementNavigation of: anAnnouncer` reports only that announcer's subscriptions. `SystemAnnouncer` behaviour is unchanged (BT-2454).

### Runtime

- **System announcement emit points (ADR 0093 Phase 3)** — the runtime now publishes the well-known system events on `SystemAnnouncer current` at their triggering actions: `ClassLoaded` from `beamtalk_object_class` after the class metadata row commits (and on hot redefinition), `ClassRemoved` on class-process termination, `ActorSpawned` / `ActorStopped` via the universal actor-lifecycle telemetry hook (every spawn/stop path), `SupervisionChildAdded` / `SupervisionChildCrashed` from `beamtalk_supervisor`'s `startChild` success/failure, and `BindingChanged` from a workspace assignment (`x := …`). All emits are best-effort and fault-isolated — guarded so they never fail or delay the triggering action (BT-2445).
- **Structural object printer** — new `beamtalk_object_printer` module provides the canonical `ClassName(field: value, ...)` renderer with bounded recursion (depth cap 5, width cap 50, total-length cap 10000) and cycle guard. All value/object formatters funnel through this single renderer (BT-2459).
- **Actor/Supervisor display unification** — live-process rendering unified via `process_label/1`; the REPL's hard-coded `#Actor<...>` / `#Supervisor<...>` paths removed. `printString` dispatch uses a 250ms timeout with fallback to the tuple-derived label, preventing REPL hangs on wedged actors. User subclass `printString` overrides are honoured (BT-2462).
- Fix `beamtalk_interface:class_object_for_pid/2` (called by `findClass/1` via `handle_class_named/1`) to return `nil` on `noproc`/timeout, making `findClass/1` return `nil` for transiently-unavailable class processes under heavy parallel load — `SystemNavigation classesInPackage:` filters out those `nil` results silently, matching the documented miss policy (BT-2467).
- Fix namespace collision diagnostic when hot-reloading a `Protocol define:` file on a second surface (e.g., MCP after REPL) — the compiler's class cache now filters pre-loaded protocol entries before hierarchy injection, and `register_module` accepts protocol re-registration when the existing class has superclass `Protocol` (BT-2088).
- Fix `conformsTo:` returning `true` for unknown or non-protocol names — now correctly returns `false` when the protocol name is not registered (BT-2136).
- Fix telemetry handler attachment race in `beamtalk_trace_store` — declare `telemetry` and `telemetry_poller` as OTP application dependencies so handler attachment is deterministic (BT-2116).
- Fix `isKindOf:` / `inheritsFrom:` / `includesBehaviour:` returning semantically incorrect results on class-object receivers — `Integer isKindOf: Number` now correctly returns `false` (Integer is an instance of `Integer class`, whose parallel metaclass chain does not include `Number`). `classAllSuperclasses` now walks the parallel metaclass hierarchy for metaclass-tagged receivers (BT-2194).
- **Ground the parallel metaclass chain at `Class`** — `ProtoObject class superclass` now returns `Class` (instead of `nil`), matching Smalltalk-80, Pharo, and ADR 0036. The parallel chain `Foo class → … → ProtoObject class` merges into the instance-side `Class → Behaviour → Object → ProtoObject` tower, so `metaclassSuperclass` and `classAllSuperclasses` return a fully-grounded chain on metaclass receivers. As a result, `Integer isKindOf: Object` is now `true` (was `false` after BT-2194) and `Integer isKindOf: Class` is now `true` — agreeing with the dispatch chain, which already routed metaclass receivers through `Class`/`Behaviour`/`Object` (BT-2217).
- ETS-backed caching for class-method dispatch — `find_class_method_in_ancestors` now reads selectors and module names from an ETS table instead of issuing `gen_server:call` round-trips per ancestor hop, reducing inherited class-method dispatch median latency from ~5–6μs to ~1.6μs (BT-2008).
- Fix `findClass:` not resolving metaclass display tags — `findClass: #'Integer class'` now returns the metaclass object instead of `nil`, so stdlib code no longer needs string surgery to resolve metaclass names (BT-2223).
- Consolidate three hot class-metadata ETS tables (`beamtalk_class_module_table`, `beamtalk_class_methods_table`, `beamtalk_class_hierarchy_table`) into a single `beamtalk_class_metadata` table with typed field accessors. One atomic insert/delete per class instead of three; `inherits_from/2` reads via `ets:lookup_element/4` for ~10–14% speedup on the per-exception-match hot path (BT-2222).
- **`beamtalk_extensions:extenders_of/1`** and **`extensions_by/1`** — reverse-index queries for extension methods. `extenders_of(ClassName)` returns unique owner atoms contributing extensions to a target class; `extensions_by(OwnerName)` returns `{TargetClass, Selector}` tuples for every extension an owner contributes (BT-2202).
- Compiler-port diagnostics in `findSendersIn`/`findReferencesToIn` now logged via OTP logger instead of silently discarded — port-unavailability escalates to `?LOG_ERROR`, parse errors use `?LOG_WARNING`, all with `domain => [beamtalk, stdlib]` metadata (BT-2219).
- Fix `super` in a value/primitive context (a value-type method or a foreign extension on a value/primitive class) generating invalid Core Erlang. Value-context funs are `fun(Args, Self) -> Result` with no `State` binding, so the old codegen's `beamtalk_dispatch:super(Sel, Args, Self, State, Class)` referenced an unbound `State` and failed to compile. Such `super` sends now route to the new `beamtalk_dispatch:super_value/4`, which walks the same superclass chain without threading state and returns a plain value (BT-2252).
- **Install-hook ChangeEntry emission** — the `>>` runtime install chokepoint (`beamtalk_repl_loader:load_recompiled_method`) now emits a ChangeEntry after every successful in-memory method patch. The entry captures flushability (true iff the class has an in-project source file), byte span + previous source (via the resolver) for flushable classes, and `intent`/`authorKind` metadata. Non-flushable patches (stdlib, dynamic, dependency classes) still log with `flushable: false`. Emission is best-effort and never blocks the install (BT-2283).
- **Byte-span resolver bridge** — new `resolve_method_span` compiler-port command returns the exact byte span and previous source bytes for a method in a `.bt` file, enabling the install hook to capture flush-time splice coordinates (BT-2283).
- **Class-side runtime method dispatch** — programmatic `ClassBuilder` classes can now install and dispatch class-method funs via an ETS retrieval store, mirroring the instance extension path. `classMethods:` blocks lower to anonymous class-method funs matching the compiled convention, with `self`/`super` resolution and class-variable state threading (BT-2266, BT-2267).
- **Module-less `ClassBuilder` instantiation** — classes built purely programmatically via `ClassBuilder` can now be instantiated with `new` / `new:` before being flushed to a compiled module. The generic instance uses the same tagged-map shape as compiled value types for seamless post-flush behavior (BT-2275).
- Harden module-less `ClassBuilder` instantiation — depth-guard logging on ancestor chain walks, deadlock-free self-dispatch for fun-backed instance methods inside the class gen_server, and robust ancestor exit handling (BT-2277).
- Fix `selector_arity` for malformed selectors — interior-colon atoms (e.g. `'at:put'`) are now treated as unary instead of keyword, avoiding spurious `arity_mismatch` diagnostics (BT-2278).
- **`beamtalk_workspace_changelog`** gen_server — workspace-local ChangeLog that records every live method patch as a crash-safe, session-aware change entry with two-part on-disk persistence (ADR 0082 Phase 1) (BT-2282).
- **`Workspace flush` / `flush:`** — writes pending ChangeLog entries to disk via trivia-preserving byte-span splice (no AST reprint) with atomic rename, multi-file two-phase commit, and external-edit conflict detection. Method patches splice the new body into the exact byte span of the old one; new-class entries write the full source to the target path. Conflicts are reported per-file rather than raised as exceptions (BT-2286).
- **`beamtalk_xref` gen_server** — maintained selector-to-sites cross-reference index for `SystemNavigation` queries (ADR 0087). Four ETS tables track method definitions, senders, references, and per-class generation counters. Reads hit ETS directly without serializing through the gen_server; writes serialize for atomic per-class publish (BT-2297).
- **`beamtalk_xref` hot-reload atomicity** — xref reads now filter by generation counter, ensuring that a concurrent `register_class` never exposes stale rows mid-reload. Old-generation rows are reclaimed asynchronously via a gen_server cast sweep. `put_method/4` joins the current generation instead of bumping it, so sibling methods remain visible to readers throughout a single-method hot-patch (BT-2300).
- **`beamtalk_xref` lifecycle hooks** — `put_method/4` (instance and class-side), extension `register/4`/`register/5`, extension `unregister/2`, and `ClassBuilder` registration now update the xref index incrementally. Live-edits via `>>` and `compile:source:` are indexed immediately; extensions with source get full send-indexing, sourceless extensions get `unindexed_runtime_fun` marker rows. New `purge_method/3` API removes one method's xref rows without touching siblings (BT-2301).
- **`beamtalk_xref` O(1) loaded-class set** — the `miss_partition` query (used by every `SystemNavigation` read) now resolves the loaded-class set from a new `beamtalk_loaded_classes` ETS table (populated by class `init/1`, cleaned by `terminate/1`) instead of issuing O(classes) `gen_server:call` round-trips. `sendersOf: #asString` end-to-end drops from ~1.0 ms to ~0.42 ms (BT-2384).
- Fix native-facade and protocol classes (`Subprocess`, `TranscriptStream`, `Printable`) missing from the `beamtalk_xref` index — native codegen now calls `build_method_xref_list`, and `beamtalk_protocol_registry` populates `method_xref` with `unindexed_runtime_fun` rows. Eliminates spurious `xref_miss` warnings on `SystemNavigation` queries involving these classes (BT-2385).

### Tooling

- **LiveView IDE (Attach topology)** — new Phoenix LiveView client at `editors/liveview/` that connects to the workspace over Erlang distribution, consuming native `op_result()` terms end-to-end without the JSON encoding step. Wave 1 implements eval (display-formatted with the same surface-shared rules as CLI / REPL / MCP), live Transcript streaming via `beamtalk_repl_subscriptions`, and per-mount session lifecycle. Includes Elixir toolchain setup (`scripts/setup-cloud.sh`), CI workflow (`.github/workflows/liveview.yml`), and `just web` / `just web-setup` commands (BT-2401, BT-2407).
- **LSP infra: runtime-attached navigation query channel + `delegateToRuntime` flag** — foundation for the epic that flips LSP nav features over to a running workspace (BT-2215). A new `nav-query` REPL op (handled by `beamtalk_repl_ops_nav`) answers structured `senders` / `implementors` / `references` queries directly from `beamtalk_xref` site records, bypassing the inspect-string serialiser so `beamtalk-lsp` can decode typed `NavSite { class, class_side, method, line, source_file }` rows. A reusable two-mode dispatch seam (`Backend::delegate_nav_query`) routes per-method LSP queries to the runtime when `initializationOptions.delegateToRuntime` is on **and** a workspace is reachable; falls back to the in-process AST walker otherwise. Find-references is wired as the worked example; per-method children (BT-2240..2244) flip the remaining queries over. Push-channel listener consumes `classes/loaded` frames and invalidates a generation-counter nav cache so live patches (`Behaviour >>`, `compile:source:`) and full reloads don't return stale answers. Flag defaults `false` — no behaviour change on existing LSP sessions (BT-2239).
- Fix `beamtalk new` emitting scaffold `.bt` files that were not format-clean — a freshly created project now passes `beamtalk fmt-check` immediately. Generated templates are run through the formatter before writing, so the canonical form adapts to package-name length (BT-2476).
- **Faster FFI type-spec extraction** — `beamtalk build` now reads `.beam` `-spec` attributes across all schedulers and memoises each referenced remote module's parsed types for the duration of a batch (previously a single-process pass that re-read modules like `sets`/`queue` once per referencing spec). Cold extraction of the OTP apps is ~29× faster (≈25 s → ≈0.9 s on a 4-core machine); results are byte-for-byte identical (BT-2469).
- **Shared OTP type-spec cache** — the OTP portion of the FFI type cache (stdlib, kernel, erts, crypto, …) is now stored in a shared, version-keyed directory outside `_build/` (`$BEAMTALK_CACHE_DIR` or the platform cache dir, e.g. `$XDG_CACHE_HOME/beamtalk/otp-specs/<otp>-<erts>/`). A freshly cloned workspace with an empty `_build/type_cache/` reuses this cache instead of re-reading hundreds of OTP `.beam` files on the first build — the dominant cold-start cost. Dependency/native specs stay project-local; shared hits are mirrored into `_build/type_cache/` so the LSP and `beamtalk lint` are unaffected; cache writes are atomic; an OTP upgrade lands under a fresh version key (BT-2470).
- OTP type discovery now includes `erts`, so `:erlang` module BIFs (`whereis/1`, `spawn/3`, `self/0`, `node/0`, `monotonic_time/0`, etc.) type-check with proper specs instead of `Dynamic` (BT-2159).
- **REPL `:interrupt` / `:int` meta-command** — sends an out-of-band interrupt over a separate connection to cancel a running evaluation. This is the one REPL operation that cannot be expressed as a normal message-send (the session is blocked while an eval is in-flight) (BT-2090).
- `beamtalk lint` now loads the FFI type cache from `_build/type_cache/`, so lint and build agree on FFI return types — previously lint always inferred untyped-FFI for Erlang calls (BT-2134).
- `beamtalk lint` validates cache entries against live `.beam` file mtimes, skipping stale entries when the underlying module has been recompiled (BT-2139).
- Fix false-positive `Unresolved class` warnings in the LSP for classes defined in fetched dependencies — the LSP now preloads `.bt` files from `_build/deps/*/src/` (BT-2137).
- Fix LSP `is_protocol_type` always returning `false` for registered protocols, causing false-positive type warnings on protocol-typed parameters (BT-2135).
- **Redundant type annotation lint** — `name :: T := <expr>` is flagged when the inferred type of `<expr>` exactly matches `T`, since the annotation adds no information the type checker doesn't already have. Suppressible with `@expect type_annotation`. Skips unions and widening annotations where the annotation does real work (BT-2140).
- **REPL `:flush` / `:changes` / `:dirty` meta-commands** — `:flush` desugars to `Workspace flush`, `:flush <arg>` to `Workspace flush: <arg>` (accepting a class name, symbol kind, or dictionary filter), `:changes` to `Workspace changes`, and `:dirty` to `Workspace changes dirtyMethods` (BT-2287).
- **MCP `save_method` / `try_method` / `save_class` / `flush` / `list_changes` / `dirty_methods` tools** — six new MCP tools exposing the ADR 0082 ChangeLog and flush operations to IDE and agent clients. `save_method` and `try_method` install durable and ephemeral method patches respectively, `save_class` creates a new class via `Workspace newClass:at:`, `flush` writes pending changes to disk, and `list_changes` / `dirty_methods` query the ChangeLog (BT-2288).
- **LSP `executeCommand` handlers and `workspace/applyEdit` on flush** — `beamtalk.flush`, `beamtalk.flush.class`, `beamtalk.flush.file`, `beamtalk.flush.kind`, and `beamtalk.saveClass` editor commands dispatch through the attached workspace. After flush the LSP emits `workspace/applyEdit` with a whole-document `TextEdit` for every flushed file currently open in the editor, so buffers refresh automatically (BT-2289).
- **LSP `textDocument/implementation`** — go-to-implementation for selectors, routing through `NavQuery::ImplementorsOf` via `delegate_nav_query` (runtime path) or AST fallback (BT-2241).
- **LSP `textDocument/references` declaration-merge** — `context.includeDeclaration` now overlays method-definition sites (selector cursor) or class-declaration spans (class cursor) onto reference results. Both runtime and cold-file paths respect the flag identically (BT-2240).
- **LSP `textDocument/prepareTypeHierarchy` / `typeHierarchy/supertypes` / `typeHierarchy/subtypes`** — type hierarchy navigation answering from the in-process `ClassHierarchy` index for both runtime-attached and cold-file modes (BT-2242).
- **LSP `textDocument/prepareCallHierarchy` / `callHierarchy/incomingCalls` / `callHierarchy/outgoingCalls`** — call hierarchy navigation. Incoming calls route through the `nav-query` `senders` channel; outgoing calls walk the method-body AST via `SystemNavigation messagesSentBy:` (BT-2243).
- Fix LSP `textDocument/prepareTypeHierarchy` resolving to synthetic protocol headers instead of real classes when both shared a name — now honours the BT-1933 protocol/class shadowing rule, so goto-definition and type-hierarchy agree on resolution (#2381).
- **LSP `textDocument/documentSymbol` / `workspace/symbol` runtime-attached mode** — document and workspace symbol outlines now source from the live class registry via a new `nav-symbols` REPL op when `delegateToRuntime` is on. REPL-loaded classes with no source file appear in workspace symbols (attached to the workspace-root URI, marked `«no source»`). Cold-file fallback is unchanged (BT-2244).
- **LiveView IDE Wave 2: Inspector + bindings panes** — bindings pane lists session variables with real-time updates via the bindings subscription stream; object-valued bindings offer Inspect. Inspector pane renders structured fields of live objects with drillable reference-following into nested objects (BT-2408).
- **LiveView IDE Wave 3: method edit/save/flush** — Method Editor pane for editing and saving methods via the write-surface (ADR 0082); failed compiles render the structured `#beamtalk_error{}`. Changes pane shows the ChangeLog; Flush persists pending changes to disk (BT-2409).
- **LiveView IDE Wave 4: multi-tab session isolation + resume** — per-tab `sessionStorage` token isolates sessions across browser tabs. Reconnecting tabs resume their existing session within a grace window instead of starting fresh. Hot-reload coherence: a method saved in one tab is visible to evals in all tabs (BT-2410).
- **Remove Phase-1 browser frontend** — the vanilla-JS browser workspace (`priv/index.html`, `workspace.js`, `workspace.css`) and its `--web` / `--web-port` CLI flags are removed, superseded by the LiveView IDE (ADR 0017 Phase 3). The `/ws` WebSocket REPL transport for CLI, MCP, and VS Code is kept intact (BT-2415).

### Internal

- **Cockpit LiveView IDE UX design spike** — throwaway interactive prototype under `spikes/cockpit-ux-spike/` exploring two IDE perspectives: a System Browser with method editing, Workspace, and stackable Inspector windows (Cockpit), and a Squeak-style direct-manipulation Morphic world. Pure front-end over a faked in-memory image; not wired to the real LiveView app (#2512).
- **`SystemNavigation` xref read-path migration (ADR 0087 Phase 5)** — `referencesTo:`, `implementorsOf:`, and `selectorsMatching:` now resolve from the runtime-maintained `beamtalk_xref` index (new `references_to_bt/1`, `implementors_of_bt/1`, `defined_selectors_bt/0` read APIs) instead of walking every method source / probing every class on each call, mirroring the `sendersOf:` migration (BT-2299). Each applies the ADR 0087 miss policy: indexed rows whose owning class is unloaded are dropped silently, and a loaded-but-unindexed class falls back to the legacy source-scan / `includesSelector:` probe for that class only, emitting one `xref_miss` warning. Public BT API and result shapes are unchanged; extension methods stay on the unconditional source scan (not yet indexed) (BT-2302).
- **`SystemNavigation` xref read-path migration (ADR 0087 Phase 5b)** — `unimplementedSelectors` and `unusedSelectors` now resolve from the `beamtalk_xref` index (new `all_sends_bt/0` and `all_sent_selectors_bt/0` read APIs) instead of re-parsing every method AST on each call. Same miss-policy as the Phase 5a migration (BT-2302); public BT API and result shapes unchanged (BT-2303).
- **Synthetic-method xref emission** — codegen now appends xref rows for auto-generated value-type accessors (`field/1` getters, `withField:/2` setters) tagged `source_status => synthetic` with `synthetic_origin` pointing at the declaring slot. User-defined accessor overrides correctly suppress the synthetic row. Makes `implementorsOf:` return non-empty results for auto-accessor selectors (BT-2304).
- Fix `build --stdlib-mode` to resolve FFI types via `NativeTypeRegistry` — eliminates 202 spurious untyped-FFI warnings from `just dialyzer-specs` (#2138).
- Replace 16 `format!()` calls with `Document` API in `counted_loops.rs` codegen — continues the BT-875 cleanup (BT-2102).
- Deduplicate atom-char escape helper into `beamtalk-core::util` (BT-2113).
- Replace 3 `format!()` calls with `docvec!` in `generate_start_link_doc`, `generate_class_reference`, and `class_not_found_error_doc` — continues the BT-875 cleanup (BT-2143).
- Extract `call_self_p0`/`call_p0_self` helpers in primitives codegen — deduplicates ~56 matching arms across `string.rs`, `list.rs`, and `dictionary.rs` (BT-2146).
- Extract `try_canonicalize` helper in `package.rs` — deduplicates 5 identical inline `canonicalize`-with-fallback patterns (BT-2149).
- Add missing `domain => [beamtalk, stdlib]` metadata to nine `?LOG_*` calls across five stdlib modules (BT-2141).
- Extract duplicated analysis pipeline in MCP server into shared `run_module_analysis` helper (BT-2152).
- Extract duplicated NLR catch-var allocation into shared `NlrCatchVars` struct/helper (BT-2155).
- Replace `format!()` calls with `Document` API in `core_erlang_binary_string` — continues the BT-875 cleanup (BT-2163).
- Replace 6 `Document::String(format!())` calls with `docvec!` in `callbacks.rs`, `mod.rs`, `while_loops.rs`, and `control_flow/mod.rs` — continues the BT-875 cleanup (BT-2166).
- Replace all 44 `Document::String(format!())` violations with `docvec!` equivalents in `exception_handling.rs`, extract `on_do_catch_preamble` and `make_handler_apply` helpers — continues the BT-875 cleanup (BT-2169).
- Centralize byte-offset→line-number logic into `Span::line_number`, removing duplicate implementations from codegen and MCP server (BT-2160).
- Extract duplicate `build.rs` version-injection logic into shared `beamtalk-build` workspace crate — single source of truth for `BEAMTALK_VERSION` emission (BT-2172).
- Replace 5 `format!()` calls with `fresh_temp_var` / `write!` in `repl/codegen.rs` and `value_type_codegen.rs` — continues the BT-875 cleanup (BT-2175).
- Extract shared `parse_and_check_expression` helper in compiler-port — deduplicates ~20-line lex/parse/validate preamble from `handle_compile_expression` and `handle_compile_expression_trace` (BT-2178).
- Replace `format!()` violations with `Document`/`docvec!` API in `value_type_codegen.rs` module header — continues the BT-875 cleanup (BT-2181).
- Resolve type references into preloaded modules — `resolve_remote_type` for `:erlang` and other preloaded modules now reads type specs from the BEAM binary via `code:get_object_code/1` instead of returning `Dynamic` (BT-2185).
- Remove three duplicate Erlang string escape implementations in beamtalk-cli — consolidated onto `escape_for_erlang_string` from `erlang_eval.rs` (BT-2224).
- Unmapped quoted `@primitive` in stdlib value-type codegen now fails the build with a clear error naming class + selector, instead of silently falling back to runtime dispatch (BT-2233).
- Shared `beamtalk_error:raise_type_error/3` extracted from five stdlib modules — zero behavior change (BT-2229).
- Unify Behaviour/Class/Metaclass primitive-lowering into a single `REGISTRY` table — tower classes share one function pointer so any tower selector resolves regardless of which class declares it, preventing the BT-2232 regression pattern where moving a method up the tower silently dropped its inline BIF (BT-2234).
- Byte-span resolver validates that the parser can resolve exact method byte spans for flush-time splice replacement — 1347 methods across 161 files pass no-op round-trip (ADR 0082 Phase 0) (BT-2281).
- Extract `kill_and_wait/1` and `delegate_error/1` helpers in `beamtalk_actor.erl` — zero behavior change (BT-2272).
- Replace 7 `format!()` calls with `versioned_var` helper in Core Erlang variable-name generators — continues the BT-875 cleanup (BT-2305).
- **ADR 0089 Phase 3 flag-day** — removed `Document::String` and `Document::Eco` from the public API, structurally closing the BT-875 recurrence vector. All codegen now flows exclusively through typed `document::leaf` / `unparse::leaf` helpers (BT-2331).
- Migrate remaining codegen modules (`expressions.rs`, `intrinsics.rs`, `repl/codegen.rs`, `control_flow/`, `dispatch_codegen.rs`, `value_type_codegen.rs`, `gen_server/`, `mod.rs`, primitives) to `document::leaf` API (ADR 0089 Phase 2) (BT-2320..BT-2330).
- Use canonical `safe_class_method_fn_name` in supervisor and spec codegen — adds atom-length safety guard for pathologically long selectors (BT-2339).
- Remove `capture_expression` in favour of direct `Document` splicing, closing the value-type raw-fragment leaf seam (BT-2348).
- Fix ADR 0089 leaf-API violations in `instantiation_error_stub` and `build_dnu_error_doc` — replaced manual `'...'` atom quoting with `leaf::atom()` and `core_erlang_binary_string` with `leaf::binary_lit` (BT-2390).
- **Phoenix LiveView topology spike (ADR 0017 Phase 3)** — validated the Attach topology (Livebook-style: Phoenix as a separate BEAM node connecting to the workspace over Erlang distribution) end-to-end with zero runtime changes. Report in `docs/research/phoenix-topology-spike.md`; throwaway code in `spikes/phoenix_topology/` (BT-2394).
- Document integration-testing strategy for modules only reachable via live external clients (`beamtalk_ws_handler`, `beamtalk_build_worker`) — added to `docs/development/testing-strategy.md` (BT-2389).
- **Term-returning REPL op layer** — refactored curated REPL ops to return native Erlang `op_result()` terms, with JSON encoding pushed to the WebSocket transport edge only. `beamtalk_repl_ops` is the central dispatch table; `beamtalk_repl_subscriptions` provides a stable facade for push streams. Dist-attached clients (LiveView IDE) consume terms natively; browser wire format unchanged (BT-2399, BT-2402).

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
- **String is now a subclass of Binary** — String inherits Binary's byte-level methods; Binary moved under Collection hierarchy ([ADR 0086](docs/ADR/0086-string-subclass-of-binary.md))
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
- ADR 0086 (originally 0069): Make String a subclass of Binary
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
