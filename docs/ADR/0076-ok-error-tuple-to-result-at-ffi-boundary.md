# ADR 0076: Convert Erlang ok/error Tuples to Result at FFI Boundary

## Status
Proposed (2026-04-03)

## Context

Erlang's most common return pattern — `{ok, Value} | {error, Reason}` — currently passes through the FFI proxy as a raw Beamtalk `Tuple`. Users must use `isOk`/`unwrap` from the Tuple class, losing inner type information and access to Result's combinator methods (`map:`, `andThen:`, `mapError:`, `ifOk:ifError:`).

ADR 0075 (Erlang FFI Type Definitions) identifies this as the **single largest gap** in typed FFI coverage:

> "A codebase that is fully typed in Beamtalk loses all type information the moment it touches Erlang."

The gate evaluation (BT-1845, PR #1869) confirmed that **86.5% of specced functions in the top-20 OTP modules have useful types** — meaning spec-driven Result type inference is viable for the vast majority of FFI calls.

### Current state

Today, calling an Erlang function that returns ok/error requires manual tuple unwrapping:

```beamtalk
// Current: returns Tuple, no type information
result := Erlang file readFile: "/tmp/hello.txt"
result isOk
  ifTrue: [result at: 2]    // manual positional extraction
  ifFalse: [result at: 2]   // error reason, also positional
```

The Tuple class provides `isOk`, `isError`, `unwrap`, and `unwrapOr:` as convenience methods specifically for this pattern, but they cannot provide:

- **Typed values:** `unwrap` returns `Object`, losing the `binary()` / `posix()` info from the Erlang spec
- **Combinators:** No `map:`, `andThen:`, `mapError:` — users must manually branch on every call
- **Consistency:** Beamtalk's own APIs return `Result` objects; FFI calls return `Tuple` — two different error-handling idioms in the same codebase

### Precedent: charlist coercion (BT-1127)

The charlist → String coercion in `beamtalk_erlang_proxy.erl` established the pattern for transparent boundary conversion:

1. Beamtalk strings are UTF-8 binaries; Erlang functions may expect charlists
2. On `badarg`, the proxy retries with `unicode:characters_to_list/2` conversion
3. Charlist results are converted back to binary for consistency
4. Users see native Beamtalk strings throughout — conversion is invisible

This ADR follows the same philosophy: convert at the boundary so users work with native Beamtalk types.

### Constraints

- Must not break existing code that passes ok/error tuples to Erlang functions (outbound direction)
- Must handle edge cases: bare `ok` atoms, 3+ element tuples, non-ok/error tuples
- Performance overhead must be negligible (single pattern match per FFI return)
- This is a second exception to ADR 0028's "transparent interop" principle — must be justified

## Decision

### 1. Runtime: auto-convert ok/error tuples to Result in the FFI proxy

All `{ok, V}` and `{error, R}` tuples returned from Erlang calls via `direct_call/3` in `beamtalk_erlang_proxy.erl` are automatically converted to `Result` objects using the existing `beamtalk_result:from_tagged_tuple/1` helper.

**Conversion rules:**

| Erlang return value | Beamtalk value | Rationale |
|---------------------|----------------|-----------|
| `{ok, Value}` | `Result ok: Value` | Standard two-element ok tuple |
| `{error, Reason}` | `Result error: Reason` | Standard two-element error tuple (reason wrapped via `ensure_wrapped/1`) |
| `ok` (bare atom) | `Result ok: nil` | Common in OTP (`file:write_file/2`, `application:start/1`) |
| `error` (bare atom) | `Result error: nil` | Rare, but symmetric with bare `ok` |
| `{ok, V1, V2, ...}` (3+ elements) | `Tuple` (no conversion) | Not a standard ok/error pattern; semantically different |
| Any other tuple | `Tuple` (no conversion) | Only `{ok, _}` and `{error, _}` are recognized |
| Non-tuple values | Unchanged | Integers, lists, maps, etc. pass through as today |

**Implementation in the proxy:**

```erlang
%% In beamtalk_erlang_proxy.erl, after direct_call/3 gets a result:
coerce_result(Result) ->
    case Result of
        {ok, Value} ->
            beamtalk_result:from_tagged_tuple({ok, Value});
        {error, Reason} ->
            beamtalk_result:from_tagged_tuple({error, Reason});
        ok ->
            beamtalk_result:from_tagged_tuple({ok, nil});
        error ->
            beamtalk_result:from_tagged_tuple({error, nil});
        Other ->
            Other
    end.
```

This integrates into the existing coercion pipeline alongside charlist coercion — the result of `coerce_charlist_result/1` feeds into `coerce_result/1`.

### 2. Type system: map Erlang specs to Result(T, E) in auto-extract

The spec reader from ADR 0075 already parses Erlang type specs and maps them to Beamtalk types. This ADR adds a recognition rule: when the return type of an Erlang spec is a union containing `{ok, T}` and/or `{error, E}`, map it to `Result(T, E)`.

**Spec mapping rules:**

| Erlang spec return type | Beamtalk type | Notes |
|-------------------------|---------------|-------|
| `{ok, binary()} \| {error, posix()}` | `Result(String, Symbol)` | Full typed Result |
| `{ok, pid()} \| {error, term()}` | `Result(Pid, Dynamic)` | Error type falls back to Dynamic |
| `{ok, T} \| {error, E} \| Other` | `Result(T, E) \| Other` | Union preserved for non-ok/error branches |
| `ok \| {error, E}` | `Result(Nil, E)` | Bare ok atom maps to ok value of Nil |
| `{ok, T}` (no error branch) | `Result(T, Dynamic)` | Conservative: error type unknown |
| `{error, E}` (no ok branch) | `Result(Dynamic, E)` | Rare but handled consistently |
| `term()` / `any()` | `Dynamic` | No ok/error structure visible — no conversion |

This feeds directly into ADR 0075's type signature generation pipeline. The spec reader recognizes the ok/error union pattern and emits `Result(T, E)` in the generated type stub, so the type checker and LSP completions show precise Result types.

**Example — file:read_file/1:**

```erlang
%% Erlang spec:
-spec read_file(Filename) -> {ok, Binary} | {error, posix()} when
    Filename :: name_all(),
    Binary :: binary().
```

```beamtalk
// Generated type signature (ADR 0075 auto-extract):
// Erlang file readFile: filename :: String -> Result(String, Symbol)

// User code:
result := Erlang file readFile: "/tmp/hello.txt"
result map: [:content | content size]  // Result(Integer, Symbol)
```

### 3. REPL and user experience

```beamtalk
// Reading a file — ok path
result := Erlang file readFile: "/tmp/hello.txt"
result
// => Result ok: "Hello, world!\n"

result map: [:content | content size]
// => Result ok: 14

result value
// => "Hello, world!\n"

// Reading a file — error path
result := Erlang file readFile: "/nonexistent"
result
// => Result error: (ErlangError reason: #enoent)

result isError
// => true

result mapError: [:e | "File not found: " ++ e reason asString]
// => Result error: "File not found: enoent"

// Chaining FFI calls with combinators
(Erlang file readFile: "/tmp/config.json")
  andThen: [:content | Erlang json decode: content]
  mapError: [:e | "Config load failed: " ++ e reason asString]
// => Result ok: #{"key" -> "value"}

// Bare ok atom (file:write_file/2)
Erlang file writeFile: "/tmp/out.txt" with: "data"
// => Result ok: nil

// Timer returns — fully typed from spec
Erlang timer sendAfter: 1000 with: #timeout
// => Result ok: #<TimerRef>
```

### 4. Error examples

```beamtalk
// Calling map: on an error Result — safe, returns the error unchanged
(Erlang file readFile: "/nonexistent") map: [:c | c size]
// => Result error: (ErlangError reason: #enoent)

// Calling value on an error Result — raises
(Erlang file readFile: "/nonexistent") value
// => ERROR: Result is error: (ErlangError reason: #enoent)

// Non-ok/error tuples are still Tuples
Erlang erlang timestamp
// => #(1712, 150000, 0)  — Tuple, not Result
```

## Prior Art

### Gleam (BEAM)
Gleam's `Result(value, error)` type compiles directly to `{ok, Value}` / `{error, Reason}` tuples — zero conversion needed because the representations are identical. When calling Erlang via `@external`, the programmer declares the return type; Gleam trusts the annotation at compile time. No runtime coercion.

**What we adopt:** The insight that ok/error is so pervasive on BEAM that it deserves first-class Result treatment.
**What we adapt:** Where Gleam achieves this through identical representation (possible because Gleam controls its own compilation), Beamtalk needs runtime conversion because Result is a tagged map, not a bare tuple.

### Elixir (BEAM)
Entirely manual and convention-based. `{:ok, value} | {:error, reason}` is a community pattern; the `with` macro chains ok-path matching but provides no automatic conversion. Each library may have slightly different error shapes.

**What we learn:** Manual wrapping is boilerplate-heavy and error-prone. The `with` macro shows that chaining ok-path operations is a common need — our `andThen:` combinator serves the same role with better composability.

### Swift / Objective-C interop
The strongest precedent for automatic FFI result conversion. Objective-C methods following the `(BOOL)doThing:(NSError **)error` convention are automatically imported as `func doThing() throws`. The compiler recognizes the rigid error convention and synthesizes the conversion.

**What we adopt:** The core idea — a rigid, well-known error convention can be reliably recognized and automatically converted at the boundary.
**What we adapt:** Swift does this at compile time; we do it at runtime (simpler, works for dynamically-loaded modules).

### Rust FFI
Entirely manual. C functions return error codes; Rust wrappers convert to `Result<T, E>` by hand. No automatic conversion.

**What we learn:** Manual boundary wrapping is the norm when conventions aren't rigid. Erlang's convention is rigid enough to automate.

### Kotlin / Java interop
Kotlin removes Java's checked exception requirement but doesn't convert to a Result type. `runCatching { }` is opt-in.

**What we learn:** Ignoring the problem pushes burden to users. Our approach is more helpful.

## User Impact

### Newcomer (from Python/JS/Ruby)
**Positive.** Pattern matching on tuples is unfamiliar; `Result` with `map:`, `value`, and `isOk` feels like working with `Optional` or `Promise`. Error handling via combinators is more intuitive than positional tuple extraction. The REPL shows `Result ok: "..."` which is self-documenting.

### Smalltalk developer
**Mostly positive.** Result is a proper object with methods — more aligned with message-passing philosophy than raw tuples. Smalltalk traditionally uses exceptions for errors, so Result is a pragmatic departure, but the combinator API (`map:`, `andThen:`) follows familiar block-passing patterns.

### Erlang/Elixir developer
**Mixed.** They know ok/error tuples intimately and may initially wonder where their tuples went. However, `Result` provides the same information with better ergonomics. The type system benefit (precise `Result(String, Symbol)` instead of untyped `Tuple`) is compelling. If they need a raw tuple for forwarding to Erlang code, they can destructure: `#(#ok, value)`.

### Production operator
**Positive.** Consistent Result objects mean consistent error handling patterns. Wrapped errors via `ensure_wrapped/1` provide structured error information for logging and monitoring. No change to BEAM-level observability — Result is a tagged map, visible in Observer like any other term.

### Tooling developer (LSP, IDE)
**Positive.** `Result(T, E)` in type signatures enables precise completions after `.` — the LSP can offer `map:`, `andThen:`, `value`, etc. with correct generic types. Without this, FFI return values show only Tuple methods.

## Steelman Analysis

### Option A: Universal Auto-Conversion (Chosen — same runtime as Option D)

- 🧑‍💻 **Newcomer**: "I just call Erlang and get Result — no ceremony, no surprises. This is what I'd expect."
- 🎩 **Smalltalk purist**: "Result responds to `map:` — that's more Smalltalk than positional `at: 2` on a tuple."
- ⚙️ **BEAM veteran**: "Every BEAM language wraps ok/error eventually. Once at the boundary is cleaner than in every caller."
- 🏭 **Operator**: "Consistent Result objects mean consistent error logging patterns."
- 🎨 **Language designer**: "Charlist coercion proved boundary conversion works. This is higher-frequency — bigger payoff."

### Option B: Opt-in via Type Annotation (Rejected)

- 🧑‍💻 **Newcomer**: "I can learn gradually — raw tuples first, then Result when I understand it."
- 🎩 **Smalltalk purist**: "Explicit is better. I control when conversion happens."
- ⚙️ **BEAM veteran**: "I might *want* the raw tuple for pattern matching or forwarding. Don't convert for me."
- 🏭 **Operator**: "No hidden runtime work — I can reason about exactly what happens."
- 🎨 **Language designer**: "Type annotations driving behavior is clean and composable."

### Option C: Spec-Dependent Conversion (Rejected)

- 🎨 **Language designer**: "Only convert when we have evidence. This is principled."

### Tension Points

- **BEAM veterans** may prefer opt-in (Option B) while **newcomers** strongly prefer auto-conversion — we resolve this in favor of auto-conversion because the charlist precedent already established that boundary coercion is acceptable, and the BEAM veteran's escape hatch (tuple destructuring) exists
- **Transparent interop purists** are uncomfortable with a second coercion, but the alternative — two different error-handling idioms in every Beamtalk codebase — is worse for all user cohorts
- Option C's inconsistency (behavior depends on whether someone wrote a spec) is a dealbreaker for most cohorts

## Alternatives Considered

### Alternative: Opt-in Conversion via Type Annotation

Conversion only triggers when the user declares `Result` as the expected type:

```beamtalk
result :: Result(String, Symbol) := Erlang file readFile: "/tmp/hello.txt"
// vs
raw := Erlang file readFile: "/tmp/hello.txt"  // stays Tuple
```

**Rejected because:** Adds ceremony to every FFI call. Newcomers won't know to add the annotation. Inconsistent with charlist coercion (which is automatic). Forces users to maintain two mental models for the same underlying pattern.

### Alternative: Spec-Dependent Conversion

Only auto-convert when the Erlang module has a `-spec` returning `{ok, T} | {error, E}`. Fall back to Tuple for unspecced functions.

**Rejected because:** Creates confusing inconsistency — the same `{ok, "hello"}` value becomes `Result` from one module and `Tuple` from another, depending on whether the author wrote a spec. Spec presence affects *type precision* (Result(T,E) vs Result(Dynamic,Dynamic)), not whether conversion happens at all.

### Alternative: Keep Tuple with Better Methods

Enhance Tuple's API to provide combinator-like methods (`map:`, `andThen:`) directly:

```beamtalk
result := Erlang file readFile: "/tmp/hello.txt"
result map: [:v | v size]  // on Tuple, not Result
```

**Rejected because:** Duplicates the Result API on Tuple. Creates a parallel error-handling idiom. Tuple is documented as "an interop artifact, not a general-purpose data structure." Adding combinator methods contradicts that design intent and bloats the Tuple interface.

## Consequences

### Positive

- **Unified error handling:** Beamtalk code and FFI calls both return `Result`, eliminating the dual-idiom problem
- **Full combinator access:** `map:`, `andThen:`, `mapError:`, `ifOk:ifError:` work on all FFI returns
- **Typed FFI returns:** With spec data (86.5% of top-20 OTP modules), users get `Result(String, Symbol)` instead of untyped `Tuple`
- **LSP/IDE completions:** Precise `Result(T, E)` types enable method completions with correct generic types
- **Consistent REPL display:** `Result ok: "hello"` is more informative than `#(ok, "hello")`
- **Follows established pattern:** Same approach as charlist coercion — proven, understood, well-tested

### Negative

- **Second exception to transparent interop (ADR 0028):** ok/error tuples are no longer passed through verbatim. Justified by the same rationale as charlist coercion — user ergonomics outweigh purity for this high-frequency pattern
- **Slight runtime overhead:** One pattern match per FFI return value. Negligible — `case` on tuple tag is a single BEAM instruction
- **BEAM veteran surprise:** Developers expecting raw tuples will initially encounter Result objects. Mitigated by clear documentation and the fact that Result provides a strict superset of the information
- **Existing code using Tuple methods breaks:** Code calling `isOk`/`unwrap` on FFI returns will get a `does_not_understand` error since Result doesn't respond to `at:`. Migration path provided below

### Neutral

- Tuple class retains `isOk`/`isError`/`unwrap` methods — they remain useful for tuples from non-FFI sources (e.g., Erlang data structures stored in ETS, messages from Erlang processes)
- `beamtalk_result:from_tagged_tuple/1` already exists and handles the conversion correctly — minimal new runtime code required

## Implementation

### Phase 1: Runtime Conversion (core change)
- **beamtalk_erlang_proxy.erl:** Add `coerce_result/1` after `coerce_charlist_result/1` in the `direct_call/3` pipeline
- **beamtalk_result.erl:** Add clauses for bare `ok`/`error` atoms in `from_tagged_tuple/1`
- **Tests:** EUnit tests for all conversion rules (2-element, bare atom, 3+ element passthrough, non-tuple passthrough)
- **Affected components:** Runtime only (no parser/codegen changes)
- **Effort:** S

### Phase 2: Type Mapping (integrates with ADR 0075)
- **beamtalk_spec_reader.erl:** Add ok/error union recognition to the return-type mapping logic — when a spec's return type is a union containing `{ok, T}` and/or `{error, E}`, emit `Result(T, E)` instead of `Tuple`
- **Type signature generation:** The `beamtalk generate` pipeline (BT-1849) produces `Result(T, E)` in generated stubs
- **Tests:** Verify type mapping for common patterns: `{ok, binary()} | {error, posix()}`, bare `ok | {error, term()}`, `{ok, T}` without error branch
- **Affected components:** Runtime (spec reader), type checker (Result generic resolution)
- **Effort:** M

### Phase 3: Documentation and Migration
- Update `docs/beamtalk-language-features.md` FFI section with Result examples
- Update Tuple class doc-comment to note that FFI calls now return Result
- Add migration guidance for existing code
- **Effort:** S

## Migration Path

### Code using Tuple methods on FFI returns

Before:
```beamtalk
result := Erlang file readFile: path
result isOk ifTrue: [
  content := result unwrap.
  content asString
] ifFalse: [
  "Error: " ++ (result at: 2) asString
]
```

After:
```beamtalk
result := Erlang file readFile: path
result
  map: [:content | content asString]
  mapError: [:e | "Error: " ++ e reason asString]

// Or more directly:
result ifOk: [:content |
  content asString
] ifError: [:e |
  "Error: " ++ e reason asString
]

// Or simply:
result value  // raises on error, returns content on success
```

### Code forwarding tuples to Erlang

If existing code passes an FFI result back to an Erlang function expecting a tuple, this will break because Result is a tagged map, not a tuple. This is expected to be rare — most code consumes the result rather than forwarding it. If needed, construct the tuple explicitly:

```beamtalk
// If you need to pass an ok/error tuple to Erlang:
Erlang someModule process: #(#ok, value)
```

## References
- Related issues: BT-1838, BT-1127 (charlist coercion precedent)
- Related ADRs: ADR 0028 (BEAM interop — transparent interop principle), ADR 0060 (Result type — hybrid error handling), ADR 0068 (parametric types — `Result(T, E)` generics), ADR 0075 (Erlang FFI type definitions — spec extraction pipeline)
- Gate evaluation: `docs/ADR/0075-gate-evaluation.md` — 86.5% useful types in top-20 OTP modules
- Prior art: Swift/Obj-C automatic NSError → throws conversion, Gleam identical representation, Elixir manual `with` macro
