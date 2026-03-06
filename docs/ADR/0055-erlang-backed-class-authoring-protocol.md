# ADR 0055: Erlang-Backed Class Authoring Protocol

## Status
Accepted (2026-03-06)

## Context

### The Problem

Beamtalk currently has two mechanisms for connecting `.bt` class definitions to Erlang implementations:

1. **`@primitive "selector"`** (ADR 0007) — used exclusively in stdlib. The Rust compiler maps the primitive annotation to a direct Erlang BIF call or a call into a hand-written Erlang dispatch module. Adding a new primitive requires modifying the Rust compiler.

2. **`(Erlang module) message: args`** (ADR 0028) — introduced as a user-facing escape hatch for calling arbitrary Erlang functions from Beamtalk code. No structure or convention governs how classes use this for their backing implementations.

Neither mechanism is designed as a general **authoring protocol** for Erlang-backed classes. The result is that:

- Only stdlib authors can create primitive-backed classes (requires Rust compiler changes)
- There is no convention for how a `.bt` class declares its Erlang backing
- Value object structure (map fields) is implicit — `inspect` cannot enumerate fields, the LSP cannot infer types, and there is no compile-time validation between the `.bt` declaration and its Erlang producer
- There is no declared relationship between Actor classes backed by hand-written OTP gen_servers and their Erlang module (addressed in ADR 0056)

### Current State

A `sealed Value subclass: HTTPResponse` today requires:
- `beamtalk_http_response.erl` with `dispatch/3`, `has_method/1`, and individual getter functions (`status/1`, `headers/1`, etc.)
- No field declarations in `HTTPResponse.bt` — structure is implicit in the Erlang map
- `@primitive "status"` etc. in the `.bt` file routing through the Rust compiler
- No way for `inspect` to enumerate fields or for the LSP to offer completions on a response value

### What the Compiler Already Generates from `state:` Declarations

`state:` declarations are used for both Actor mutable state and Value immutable fields. The compiler (`value_type_codegen.rs`) already generates the following from `state:` declarations on Value classes — this pipeline is implemented and tested (e.g. `stdlib/test/fixtures/tagged_value.bt`). Shown in Erlang source syntax for readability; actual output is Core Erlang:

```erlang
%% new/0 — default constructor (tagged map with declared defaults)
new() ->
    #{'$beamtalk_class' => 'ClassName', field1 => default1, field2 => default2}.

%% new/1 — init constructor (merges caller-supplied map with defaults)
new(Args) ->
    maps:merge(DefaultMap, Args).

%% fieldName/1 — getter (inline maps:get)
fieldName(Self) ->
    maps:get(fieldName, Self).

%% withFieldName:/2 — functional updater (returns new map, never mutates)
'withFieldName:'(Self, NewVal) ->
    maps:put(fieldName, NewVal, Self).

%% class_field1:field2:/N — keyword class-side constructor
%% N = number of fields + 2 (ClassSelf, ClassVars, then one arg per field)
'class_status:headers:body:'(_ClassSelf, _ClassVars, Arg0, Arg1, Arg2) ->
    #{'$beamtalk_class' => 'HTTPResponse',
      status => Arg0, headers => Arg1, body => Arg2}.
```

This generation pipeline already works for Value classes. The semantic analysis enforces immutability — `self.x := v` on a Value class is a compile error. What is missing is not the mechanism but the **convention and migration**: existing stdlib classes like `HTTPResponse` still use `@primitive` instead of `state:` declarations, and there is no declared protocol for Erlang-backed Actors.

### Constraints

1. **Open to all authors** — any library author must be able to create Erlang-backed classes without modifying the Beamtalk compiler
2. **Messages all the way down** — interop must use message-send syntax (Principle 6)
3. **Single source of truth** — field structure should be declared once, not duplicated between `.bt` and Erlang
4. **Compile-time safety** — changes to the `.bt` declaration should produce Erlang compile errors at call sites if the contract breaks
5. **Reflection works** — `inspect`, `fieldNames`, and LSP completions should work on Erlang-backed classes
6. **Actor convention** — the (Erlang module) FFI pattern applies to Actors too, pending a dedicated `@native` ADR (ADR 0056) for gen_server-backed Actors

## Decision

The Erlang-backed class authoring protocol consists of two complementary mechanisms. A third mechanism (`@native` for Actors backed by hand-written gen_servers) is specified in ADR 0056.

### 1. `state:` Declarations on Value Subclasses

Value subclasses may declare their map structure using `state:` declarations — the same keyword already used for Actor fields. The compiler generates getter methods, functional updaters, and keyword constructors from these declarations. The semantics are context-dependent: `state:` on a `Value` subclass declares **immutable fields** (the map is never mutated in place); `state:` on an `Actor` subclass declares **mutable state** (the gen_server can update it).

```beamtalk
sealed Value subclass: HTTPResponse
  state: status :: Integer = 0
  state: headers :: List = #()
  state: body :: String = ""

  /// True if the status code is in the 2xx success range.
  sealed ok -> Boolean => (self status >= 200) and: [self status <= 299]

  /// Parse the response body as JSON.
  sealed bodyAsJson -> Object => JSON parse: self body

  /// Human-readable description including the status code.
  sealed printString -> String => "an HTTPResponse(" ++ self status printString ++ ")"
```

The compiler generates in the compiled stdlib module (`bt@stdlib@httpresponse`). The examples below use Erlang source syntax for readability — the actual compiler output is Core Erlang with abstract type attribute tuples (see `spec_codegen.rs`):

```erlang
%% Auto-generated by Beamtalk compiler
%% (shown in Erlang source syntax for clarity; actual output is Core Erlang)

-type t() :: #{'$beamtalk_class' := 'HTTPResponse',
               'status' := integer(),
               'headers' := list(),
               'body' := binary()}.

%% Default constructor
-spec 'new'() -> t().
new() ->
    #{'$beamtalk_class' => 'HTTPResponse',
      status => 0, headers => [], body => <<>>}.

%% Keyword class-side constructor — called from Erlang producers
-spec 'class_status:headers:body:'(term(), term(), integer(), list(), binary()) -> t().
'class_status:headers:body:'(_ClassSelf, _ClassVars, Status, Headers, Body) ->
    #{'$beamtalk_class' => 'HTTPResponse',
      status => Status, headers => Headers, body => Body}.

%% Getters
-spec status(t()) -> integer().
status(Self) -> maps:get(status, Self).
-spec headers(t()) -> list().
headers(Self) -> maps:get(headers, Self).
-spec body(t()) -> binary().
body(Self) -> maps:get(body, Self).

%% Functional updaters
-spec 'withStatus:'(t(), integer()) -> t().
'withStatus:'(Self, V) -> maps:put(status, V, Self).
-spec 'withHeaders:'(t(), list()) -> t().
'withHeaders:'(Self, V) -> maps:put(headers, V, Self).
-spec 'withBody:'(t(), binary()) -> t().
'withBody:'(Self, V) -> maps:put(body, V, Self).
```

The Erlang HTTP implementation calls the generated keyword constructor instead of hand-writing tagged maps:

```erlang
%% beamtalk_http.erl — calls generated constructor, no tagged map literal
Response = 'bt@stdlib@httpresponse':'class_status:headers:body:'(
    undefined, undefined, Status, Headers, Body),
```

**Compile-time safety:** If a `state:` declaration is removed or renamed, the keyword constructor arity changes and Erlang callers fail at compile time. If a type changes (e.g. `Integer` → `String`), the generated `-spec` changes and Dialyzer flags callers passing the old type. The `.bt` file is the single source of truth for both Beamtalk users and Erlang producers.

**What is generated from `state:` on a Value class:**
- Getter methods (`status`, `headers`, `body`) — inline `maps:get` with `-spec`
- Functional updater methods (`withStatus:`, `withHeaders:`, `withBody:`) with `-spec`
- `new/0` — default constructor with `-spec`
- `new/1` — init constructor merging caller map with defaults
- Keyword class-side constructor (`class_status:headers:body:/5`) with `-spec` — callable from Erlang with Dialyzer validation
- A `-type t()` map type alias representing the value
- Reflection metadata (`fieldNames` returns `#(#status #headers #body)`)

**Computed properties** use pure Beamtalk expressions in the method body:

```beamtalk
sealed ok -> Boolean => (self status >= 200) and: [self status <= 299]
sealed bodyAsJson -> Object => JSON parse: self body
sealed printString -> String => "an HTTPResponse(" ++ self status printString ++ ")"
```

No backing Erlang module is needed for `HTTPResponse` at all — the hand-written `beamtalk_http_response.erl` can be **deleted entirely** once `state:` declarations are in place.

### 2. `(Erlang module)` FFI for All Other Cases

For operations that don't fit `state:` getters (or `@native` gen_server delegation — see ADR 0056), any method (in any class, stdlib or user-defined) may delegate to an Erlang module using the `(Erlang module)` FFI from ADR 0028.

**Instance methods** pass `self` explicitly:

```beamtalk
sealed retryWith: opts: Dictionary -> HTTPResponse =>
  (Erlang beamtalk_http) retry: self options: opts
```

**Class-side-only utility objects** (e.g. `System`, `JSON`, `Yaml`, `Random`, `Timer`) are `sealed Object subclass:` — neither Actor nor Value, no instances. They have no `self` to pass; each class method body delegates directly:

```beamtalk
sealed Object subclass: System

  class getEnv: name: String -> String | Nil =>
    (Erlang beamtalk_system) getEnv: name

  class osPlatform -> String =>
    (Erlang beamtalk_system) osPlatform
```

The Erlang module uses the keyword naming convention throughout:

```erlang
%% beamtalk_system.erl
'getEnv:'(Name) -> ...
'osPlatform'() -> ...

%% beamtalk_http.erl
'retry:options:'(Response, Opts) -> ...
```

This mechanism is available to **all library authors** — no compiler changes required. It is the recommended approach for any operation that cannot be expressed in pure Beamtalk.

### Scope of `@primitive` and `@intrinsic`

With this protocol, the two pragma forms are narrowed to one case each (per the ADR 0007 amendment that introduced `@intrinsic`):

1. **`@primitive "selector"`** — Fixed Erlang/OTP function calls (BIFs or selected OTP standard library modules) — `erlang:'+'`, `erlang:integer_to_binary`, `string:length` — where the Erlang module name is fixed by the BEAM/OTP standard library and cannot follow the keyword convention
2. **`@intrinsic name`** — Structural intrinsics — compiler-generated patterns with no corresponding Erlang function: `timesRepeat:`, `blockValue`, `actorSpawn`

**`@primitive` must not be used** for calls into `beamtalk_*` modules — those use `(Erlang module)` FFI.

### Complete Protocol Summary

| Situation | Mechanism |
|-----------|-----------|
| Value class field reads/writes | `state:` declarations → auto-generated getter + updater |
| Value class construction (Erlang side) | Call `Module:'class_f1:f2:'/N` from generated module |
| Value class computed property | Pure Beamtalk expression in method body |
| Value class complex Erlang op | `(Erlang module) selector: self` |
| Actor with Beamtalk logic | `state:` declarations + method bodies (existing) |
| Actor backed by Erlang gen_server | See ADR 0056 (`@native`) |
| Utility/namespace Object subclass (class-side only) | `(Erlang module) selector: arg` in each class method body |
| Direct BIF or Erlang stdlib call | `@primitive "selector"` (narrowed scope) |
| Structural compiler intrinsic | `@intrinsic name` (e.g. `blockValue`, `actorSpawn`) |

## Prior Art

### Gleam — `@external`

Gleam annotates individual functions with `@external(erlang, "module", "function")` to bind a Gleam function signature to a specific Erlang MFA. For example:

```gleam
@external(erlang, "lists", "reverse")
pub fn reverse(list: List(a)) -> List(a)
```

This is a **per-function, per-MFA** binding — the direct analogue of our `(Erlang module) selector: arg` FFI. Gleam also has external types (opaque foreign values), but provides no mechanism to declare their field structure or auto-generate constructors.

**What we adopted:** Per-method explicitness for complex operations (our `(Erlang module)` FFI mirrors Gleam's `@external` in purpose and granularity). The idea that the Beamtalk method signature is the source of truth for the public API.

**What we improved:** Gleam's `@external` requires one annotation per delegating function with an explicit module and function name. Our `state:` declarations generate multiple getters, constructors, and updaters from a single declaration. A separate `@native` class annotation for Actors (ADR 0056) names a backing OTP gen_server module — a concept with no Gleam equivalent.

### Pharo — `<primitive: N>` and ExternalStructure

Pharo VM primitives use numbered pragmas. UFFI lets any author call C libraries by declaring `ffiCall:` in method bodies. `ExternalStructure` subclasses declare C struct fields with `fieldsDesc`, auto-generating typed accessors.

**What we adopted:** The `ExternalStructure` pattern directly inspired `state:` on Value classes — declare the struct shape once, get typed accessors for free. The co-location of declaration (in the class) with the generated accessor is the same insight.

**What we improved:** Pharo's `fieldsDesc` and Smalltalk instance variable declarations are separate from the generated accessors. Our `state:` declarations are the authoritative definition for both the Beamtalk API and the Erlang-facing constructor. The keyword constructor provides compile-time validation that Pharo's primitive system does not.

### Elixir — `defdelegate` and Module Wrapping

Elixir wraps Erlang modules with thin Elixir modules using `defdelegate`. The Elixir standard library (Enum, Map, String) is largely thin wrappers over Erlang `:lists`, `:maps`, `:binary`. There is no struct field declaration convention for foreign-backed types; Elixir `defstruct` is separate from FFI.

**What we adopted:** The "thin wrapper" philosophy — the `.bt` file defines the Beamtalk-idiomatic API, the Erlang module provides the implementation. This is the same motivation as Elixir's stdlib wrappers.

**What we improved:** `defdelegate` is per-function and generates no constructor or reflection metadata. Our `state:` declarations on Value classes provide a richer contract. The `@native` annotation for Actors (ADR 0056) provides explicit gen_server integration rather than leaving it implicit.

### Newspeak — Platform Objects and Aliens

Newspeak accesses foreign code through capability-injected "Alien" objects. Libraries are never globally accessible — they must be injected via constructor parameters. All foreign calls are message sends on alien objects.

**What we adopted:** The "DLL/module as object" metaphor — our `(Erlang module)` proxy (ADR 0028) is the same concept adapted for BEAM.

**What we rejected:** Dependency injection for Erlang modules. Beamtalk uses a global `Erlang` object (ADR 0028) for pragmatic interactive-first reasons. Newspeak's purity is valuable but incompatible with REPL-first design.

### Swift — ClangImporter and Bridging Headers

Swift auto-generates Swift-compatible APIs from Objective-C headers. `NS_SWIFT_NAME` lets ObjC authors control their Swift-facing API name. The compiler handles the cross-language mapping automatically with zero wrapper code for most cases.

**What we adopted:** The principle that the foreign code should declare its preferred name in the consuming language. Our keyword naming convention (`'selector:'/N`) is a lightweight version of this — the Erlang author names their functions to match the Beamtalk message they respond to.

**What we rejected:** Full automatic API generation from Erlang module exports. Erlang modules don't carry enough metadata for this, and the interactive-first design requires explicit declarations in the `.bt` file.

## User Impact

### Newcomer (coming from Python/JS)

The `state:` syntax is already familiar from Actor classes they may have seen. On a Value class, it reads as "these are the fields of this value object." The auto-generated getters mean `resp status` just works without hunting for where `status` is defined. A future `@native` annotation (ADR 0056) on an Actor will be a clear signal that "this class is backed by hand-written Erlang code" without requiring understanding of gen_server internals.

### Smalltalk Developer

`state:` declarations on Value classes map directly to `instanceVariableNames:` in Smalltalk class definitions — a familiar concept. The auto-generated accessors follow Smalltalk convention (accessor name = field name). The fact that the backing module is not named in a Value class definition respects encapsulation — the class declares its interface, not its implementation details.

### Erlang/BEAM Developer

The generated keyword constructor replaces error-prone hand-written tagged maps. The arity-based compile-time safety is idiomatic Erlang — changing a `state:` declaration changes constructor arity and breaks callers loudly. The `@native` annotation for Actors (ADR 0056) maps cleanly to the OTP `start_link/N` + `handle_call/3` pattern they already know. The `'selector:'/N` keyword naming convention is mechanical and easy to follow.

### Production Operator

Field declarations make `inspect` useful — operators can see the structure of a response object in the REPL or in Observer without reading source. Compile-time errors when field contracts break mean mismatches surface at build time, not at runtime in production.

### Tooling Developer (LSP/IDE)

`state:` declarations give the LSP statically-known field names and types for completion on value objects. After `resp := HTTPClient get: url`, the LSP knows `resp` has `status`, `headers`, `body` as completable fields and their return types. The `@native` annotation (ADR 0056) will let the LSP know that Actor method bodies are facades, not implementations, and can link to the Erlang source.

## Steelman Analysis

### Steelman for `@primitive` everywhere (rejected status quo)

- **Newcomer:** "I never need to think about whether something is FFI or primitive — it's all `@primitive` and the compiler handles it."
- **BEAM veteran:** "The Rust compiler validates primitive bindings at compile time. FFI calls are just function calls — no validation."
- **Language designer:** "A uniform `@primitive` mechanism is simpler than two mechanisms (`state:`, `(Erlang module)` FFI) with different rules — plus `@native` for Actors coming in ADR 0056."
- **Tension:** The validation argument is real. `state:` + keyword constructors provides Erlang-side compile-time safety, but there is no Beamtalk-side validation that the Erlang module exports the right functions.

### Steelman for a separate `field:` keyword distinct from `state:`

- **Newcomer:** "A distinct `field:` keyword makes it obvious at a glance that this is a Value class with immutable fields, not an Actor with mutable state — even without knowing the class hierarchy."
- **Language designer:** "Reusing `state:` conflates two different things — the word 'state' implies mutability and process-owned data, not immutable value fields."
- **Smalltalk purist:** "Smalltalk calls them `instanceVariableNames:`, not state. `field:` is closer to the right abstraction for a value object."
- **Tension:** The distinction is real — `state:` on an Actor is mutable, `state:` on a Value is immutable. However, the compiler already generates the correct semantics based on the superclass, and adding a new keyword adds parser/semantic complexity for a distinction that is already captured by the superclass declaration. We decided to reuse `state:` because any experienced user reading the code knows whether they're looking at a `Value` or `Actor` subclass.

### Steelman for class-level `@backing module` annotation (auto-dispatch to Erlang)

- **Newcomer:** "One annotation at the top of the class names the Erlang module — I don't need to repeat the module name in every method."
- **Language designer:** "A class-level declaration is more declarative. The module relationship is visible at a glance, not scattered across method bodies."
- **Tension:** `@backing` would reduce repetition when many methods delegate to the same Erlang module. The decision to use per-method FFI was made because it is more explicit, grep-able, and does not require the compiler to infer which methods are delegated vs pure Beamtalk. `@backing` can be added as sugar later if repetition becomes painful in practice.

### Tension Points

- Language designers and Smalltalk purists lean toward a distinct `field:` keyword; BEAM veterans and tooling developers prefer reusing `state:` (less vocabulary to learn)
- Newcomers and language designers lean toward less repetition (`@backing`); BEAM veterans and operators prefer per-method explicitness
- The keyword constructor provides arity safety at Erlang compile time and type safety via Dialyzer — but neither fires when the `.bt` file is compiled; a mismatch is only caught when the Erlang producer is built or analysed

## Alternatives Considered

### Separate `field:` Keyword for Value Class Fields

```beamtalk
sealed Value subclass: HTTPResponse
  field: status :: Integer
  field: headers :: List
  field: body :: String
```

Adds a distinct keyword to make clear that Value class fields are immutable. Rejected because `state:` already exists, the compiler already generates the correct immutable semantics for Value classes from `state:` declarations, and adding a new keyword increases vocabulary without changing the generated code. The distinction is already captured by the `Value` vs `Actor` superclass.

### Class-Level `@backing module` with Auto-Dispatch

```beamtalk
@backing beamtalk_http_response
sealed Value subclass: HTTPResponse
  sealed status -> Integer
  sealed ok -> Boolean
```

The class annotation names one Erlang module; all methods without bodies auto-dispatch to it. Rejected because it hides which methods are delegated vs pure Beamtalk, makes it harder to delegate different methods to different modules, and adds a new concept when the existing `(Erlang module)` FFI already handles it explicitly. Also superseded by `state:` auto-generation: for Value classes, there is no logic to back at all — `state:` declarations generate everything.

### Keep `dispatch/3` and `has_method/1` Pattern

Continue requiring hand-written Erlang dispatch modules. Rejected: this duplicates information from the `.bt` file, prevents reflection from working, provides no compile-time validation of the contract, and is unavailable to library authors who cannot modify the Rust compiler.

### Auto-Generate Backing Module from `.bt` Declarations

Generate the entire `beamtalk_foo.erl` from the `.bt` file, with stubs for complex methods. Rejected: complex operations (HTTP requests, YAML parsing, subprocess management) cannot be generated — they require hand-written Erlang logic. Generating partial modules with stubs creates a maintenance burden (regeneration would overwrite customizations).

## Consequences

### Positive

- Any library author can create Erlang-backed classes without touching the Rust compiler
- `state:` declarations on Value classes make object structure visible to `inspect`, the LSP, and gradual typing
- The keyword constructor provides compile-time validation when field declarations change
- `@primitive` scope is narrowed to direct BIF calls only — the list stops growing. `@intrinsic` covers structural compiler intrinsics (`blockValue`, `actorSpawn`, etc.) and is already separate (ADR 0007 amendment). Of ~442 `@primitive` entries across stdlib, approximately 24 (in HTTPResponse, JSON, Yaml, Regex, System) are targeted for immediate migration to `state:` or FFI; an additional ~12 (Timer, Random) are candidates; the remainder are direct BIF calls that correctly remain `@primitive`
- `dispatch/3` and `has_method/1` boilerplate can be removed from existing stdlib modules
- `beamtalk_http_response.erl` can be **deleted entirely** — `state:` declarations + pure Beamtalk methods replace all its functionality
- Extends generation pipeline that already exists in `value_type_codegen.rs` — minimal new work

### Negative

- `state:` on Value subclasses already works in the compiler but is not yet used by any stdlib class — migration effort is needed to convert existing `@primitive`-backed classes
- No Beamtalk-side validation that the Erlang module exports the right functions — arity mismatches surface at Erlang compile time, type mismatches surface via Dialyzer; neither is caught when the `.bt` file is compiled
- Library authors must follow the keyword naming convention (`'selector:'/N`) — unfamiliar to Erlang authors who use snake_case
- Reusing `state:` for both mutable Actor state and immutable Value fields requires documentation to clarify the context-dependent semantics
- Hot code reload: if a `state:` declaration is added or removed, the keyword constructor arity changes. In-flight Erlang callers using the old arity get a `function_clause` crash during reload — there is no `code_change/3` equivalent for Value class constructors

### Neutral

- `(Erlang module)` FFI already exists (ADR 0028); this ADR formalises its use as a class authoring tool, not just a REPL escape hatch
- Existing stdlib classes using `@primitive` for module-backed operations will be migrated incrementally (see Migration Path)
- Capability restriction (any class can call any Erlang module) remains unchanged; a future ADR may address this

## Implementation

### Phase 1 — Migrate Stdlib from `@primitive` to FFI and `state:` (No compiler changes)

The compiler already supports `state:` on Value classes and `(Erlang module)` FFI. This phase is purely migration work:

- Migrate `JSON`, `Yaml`, `Regex` from `@primitive` to `(Erlang module)` FFI in method bodies
- Migrate `HTTPResponse.bt` from `@primitive` accessors to `state:` declarations with pure Beamtalk computed properties (`ok`, `bodyAsJson`, `printString`); delete `beamtalk_http_response.erl`
- Update `beamtalk_http.erl` to call the generated keyword constructor instead of hand-writing tagged maps
- Remove `dispatch/3` and `has_method/1` from migrated modules
- Move `beamtalk_json`, `beamtalk_yaml`, `beamtalk_regex` from `beamtalk_runtime` to `beamtalk_stdlib`

**Issues:**
- BT-1142 ✅ Done — JSON/Yaml/Regex FFI conversion
- BT-1143 — Move beamtalk_json/yaml/regex to beamtalk_stdlib (In Review)
- BT-1155 — Migrate HTTPResponse.bt to `state:` declarations; delete `beamtalk_http_response.erl`
- BT-1156 — Generate `-type t()` for Value classes with `state:` declarations (blocked by BT-1155)

**Epic:** BT-1154

### Affected Components

- `crates/beamtalk-core/src/codegen/core_erlang/value_type_codegen.rs` — already supports `state:` on Value classes; needs new `-type t()` generation for the class map type (currently only `-spec` is generated by `spec_codegen.rs`)
- `crates/beamtalk-core/src/codegen/core_erlang/primitives/` — remove JSON/Yaml/Regex/HTTPResponse primitive codegen
- `runtime/apps/beamtalk_runtime/src/` — remove `dispatch/3`/`has_method/1` from migrated modules; delete `beamtalk_http_response.erl`
- `stdlib/src/*.bt` — migrate from `@primitive` to `state:` and `(Erlang module)` FFI

## Migration Path

### Existing `@primitive` Module-Backed Methods

Replace `@primitive "selector"` with `(Erlang beamtalk_module) selector: self` (for instance methods) or `(Erlang beamtalk_module) selector: arg` (for class methods). Remove `dispatch/3`, `has_method/1`, and individual getter functions from the Erlang module.

### Existing Sealed Value Classes with `@primitive` Field Accessors

Replace individual `@primitive` accessor methods with `state:` declarations. Replace the computed properties (`ok`, `bodyAsJson`) with pure Beamtalk method bodies. Delete the hand-written Erlang module (e.g. `beamtalk_http_response.erl`). Update the Erlang producer (`beamtalk_http.erl`) to call the generated keyword constructor.

### No Breaking Changes for Callers

The Beamtalk API (message selectors) does not change. Callers of `resp status`, `resp headers`, etc. are unaffected — the generated getters produce identical results to the hand-written equivalents.

## References

- Related issues:
  - BT-1142, BT-1143, BT-1144, BT-1145, BT-1146, BT-1147
- Related ADRs:
  - ADR 0005 (BEAM Object Model — Value vs Actor)
  - ADR 0007 (Compilable Stdlib with Primitive Injection — scope narrowed by this ADR)
  - ADR 0013 (Class Variables, Class-Side Methods, Instantiation — keyword constructor protocol)
  - ADR 0025 (Gradual Typing and Protocols — Phase 2 type annotations on `state:` use `: Type`; ADR 0053 supersedes with `:: Type`)
  - ADR 0028 (BEAM Interop Strategy — FFI mechanism)
  - ADR 0034 (Stdlib Self-Hosting)
  - ADR 0042 (Immutable Value Objects and Actor-Only Mutable State — immutability guarantee for Value `state:`)
  - ADR 0043 (Sync-by-Default Actor Messaging — messaging model for Erlang-backed Actor facades)
  - ADR 0049 (Remove Method-Level `sealed`)
  - ADR 0051 (Subprocess Execution — `@native` proof-of-concept)
  - ADR 0053 (Double-Colon Type Annotation Syntax — `state:` type annotation form; supersedes ADR 0025 `: Type` syntax)
- External references:
  - Gleam external functions: https://gleam.run/documentation/externals/
  - Pharo UFFI: https://files.pharo.org/books-pdfs/booklet-uFFI/UFFIDRAFT.pdf
  - Newspeak Alien FFI: https://bracha.org/newspeak.pdf
