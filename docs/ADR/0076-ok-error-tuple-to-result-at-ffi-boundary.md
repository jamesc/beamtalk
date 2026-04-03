# ADR 0076: Convert Erlang ok/error Tuples to Result at FFI Boundary

## Status
Proposed (2026-04-03)

## Context

Erlang's most common return pattern — `{ok, Value} | {error, Reason}` — currently passes through the FFI proxy as a raw Beamtalk `Tuple`. Users must call Tuple's `isOk`/`unwrap` instance methods on the result, losing inner type information and access to Result's combinator methods (`map:`, `andThen:`, `mapError:`, `ifOk:ifError:`).

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

- Must not break outbound direction (passing ok/error tuples to Erlang). Codebase audit found zero production instances of this pattern — outbound tuples are constructed via `Tuple withAll:` (stdlib dispatch, not FFI proxy) and are unaffected
- Must handle edge cases: bare `ok` atoms, 3+ element tuples, non-ok/error tuples
- Performance overhead must be negligible (single pattern match per FFI return)
- This is a second exception to ADR 0028's "transparent interop" principle — must be justified
- Must define clear scope: which code paths trigger conversion, which don't

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
            beamtalk_result:ok(nil);
        error ->
            beamtalk_result:make_error(nil);
        Other ->
            Other
    end.
```

Note: bare `ok`/`error` atoms use `ok/1` and `make_error/1` directly rather than `from_tagged_tuple/1`, because `from_tagged_tuple({error, nil})` would wrap `nil` through `ensure_wrapped/1` producing a wrapped Exception object. For bare `error` with no reason, we want a literal `nil` error reason, not a wrapped exception.

This integrates into the existing coercion pipeline alongside charlist coercion — the result of `coerce_charlist_result/1` feeds into `coerce_result/1`.

**Conversion scope — where this applies and where it does not:**

| Code path | Converts? | Rationale |
|-----------|-----------|-----------|
| `Erlang module fn: args` (FFI call via `direct_call/3`) | **Yes** | This is the FFI boundary — the single integration point |
| Messages received from Erlang processes (`receive`, actor mailbox) | **No** | Messages travel through OTP message passing, not the proxy. An Erlang process sending `{ok, Data}` delivers a raw `Tuple` |
| ETS reads (`Ets at:`, `Ets select:`) | **N/A** | Ets class methods call `(Erlang beamtalk_ets)` which does go through `direct_call/3`, but `beamtalk_ets` returns clean Beamtalk values (`Value`, `nil`, tagged maps), never ok/error tuples. If a user stores an ok/error tuple as a *value* in ETS and reads it back, the value passes through the proxy — but it was stored as a tagged map (Result) or a Tuple, not as `{ok, V}`, so no double-conversion occurs |
| Values inside converted Results | **No** | Only the outermost return value is checked. `{ok, {ok, "nested"}}` becomes `Result ok: #(ok, "nested")` — the inner tuple is not recursively converted |
| Outbound arguments to Erlang | **No** | Conversion is return-value only. Passing a `Result` to an Erlang function does not auto-convert it back to a tuple |

This scope matches charlist coercion, which also only applies at the `direct_call/3` boundary.

**The message/ETS asymmetry is deliberate:** converting messages would require hooking into OTP's message delivery, which is neither feasible nor desirable. Users receiving ok/error tuples from Erlang messages use `Tuple isOk`/`unwrap` as they do today — or call `Result fromTuple: tuple` to explicitly convert. This is a narrow inconsistency (FFI calls return Result, received messages return Tuple) but the alternative — converting in some message paths but not others — would be worse.

**Round-trip escape hatch:**

When a user needs to pass a Result back to Erlang as a tuple (e.g., forwarding a return value), Result provides a `toTuple` method:

```beamtalk
// Get a Result from FFI
result := Erlang file readFile: "/tmp/hello.txt"
// => Result ok: "Hello, world!\n"

// Forward the raw tuple to an Erlang function
Erlang myModule process: result toTuple
// Erlang sees: {ok, <<"Hello, world!\n">>}
```

`toTuple` reconstructs an `{ok, Value}` or `{error, Reason}` tuple. For ok Results, the value is the original unwrapped value. For error Results, the reason is the wrapped exception object (not the original bare atom) — because `from_tagged_tuple/1` wraps error reasons via `ensure_wrapped/1`, the round-trip is `{error, enoent}` → `Result error: (ErlangError reason: #enoent)` → `{error, #beamtalk_error{...}}`. If the caller needs the original bare reason for Erlang interop, use `result error reason` to extract it:

```beamtalk
// Round-trip with original bare reason:
result := Erlang file readFile: "/nonexistent"
Erlang myModule handleError: result error reason  // passes #enoent (Symbol)
```

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
Gleam's `Result(value, error)` type compiles directly to `{ok, Value}` / `{error, Reason}` tuples — zero conversion needed because the representations are identical. When calling Erlang via `@external`, the programmer declares the return type; Gleam trusts the annotation at compile time. No runtime coercion. Gleam is **not** a precedent for automatic coercion — it avoids the problem entirely through representation choice.

**What we adopt:** The insight that ok/error is so pervasive on BEAM that it deserves first-class Result treatment, and that users expect Result semantics on FFI returns.
**Why Gleam's approach doesn't work for Beamtalk:** Beamtalk's Result is a tagged map (ADR 0060), not a bare tuple. This enables rich method dispatch (`map:`, `andThen:`) but means the representations differ. We cannot use identical representation without giving up the object model — hence runtime conversion at the boundary.

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
**Positive.** Pattern matching on tuples is unfamiliar; `Result` with `map:`, `value`, and `ok`/`isError` feels like working with `Optional` or `Promise`. Error handling via combinators is more intuitive than positional tuple extraction. The REPL shows `Result ok: "..."` which is self-documenting.

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

### Option E: Explicit `asResult` on Tuple (Rejected)

- 🧑‍💻 **Newcomer**: "I can explore with Tuple first, then upgrade to Result when I need combinators."
- 🎩 **Smalltalk purist**: "Sending `asResult` is an explicit message — I control the conversion."
- ⚙️ **BEAM veteran**: "This is the safest option. My tuples stay tuples unless I choose otherwise. No round-trip surprises, no false positives."
- 🏭 **Operator**: "Zero hidden runtime work. I can grep for `asResult` to find every conversion point."
- 🎨 **Language designer**: "This is the most conservative, least surprising option. It composes cleanly and has zero edge cases."

This is the strongest rejected alternative. It loses on the **consistency argument**: charlist coercion is automatic, so ok/error coercion should be too. If we adopted `asResult`, we'd need to justify why strings are coerced automatically but the most common return pattern isn't.

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

### Alternative: Explicit `asResult` on Tuple (opt-in conversion)

Keep Tuple as the FFI return type but add an `asResult` method for explicit, user-controlled conversion:

```beamtalk
result := Erlang file readFile: "/tmp/hello.txt"
result asResult map: [:content | content size]  // explicit conversion

// Or without conversion:
result isOk ifTrue: [result at: 2]  // still works
```

The LSP could suggest `asResult` when the Erlang spec indicates an ok/error return type.

**Rejected because:** Adds ceremony to every FFI call site — the most common Erlang return pattern requires an extra method call everywhere. Breaks the charlist coercion precedent (charlist conversion is transparent, not opt-in). Newcomers won't discover `asResult` without documentation. However, this is the strongest alternative: it preserves transparent interop fully and gives BEAM veterans control. The deciding factor is consistency with the charlist coercion decision — if charlist conversion is automatic, ok/error conversion should be too, since both are high-frequency boundary patterns.

### Alternative: Keep Tuple with Better Methods

Enhance Tuple's API to provide combinator-like methods (`map:`, `andThen:`) directly:

```beamtalk
result := Erlang file readFile: "/tmp/hello.txt"
result map: [:v | v size]  // on Tuple, not Result
```

**Rejected because:** Duplicates the Result API on Tuple. Creates a parallel error-handling idiom. Tuple is documented as "an interop artifact, not a general-purpose data structure." Adding combinator methods contradicts that design intent and bloats the Tuple interface.

## Coercion Policy

This is the second automatic coercion at the FFI boundary (after charlist → String, BT-1127). To prevent ad-hoc coercion creep, this ADR establishes criteria for when boundary coercion is acceptable:

**A coercion is justified when ALL of the following hold:**

1. **Rigid convention:** The source pattern is a well-defined, universally-recognized convention on BEAM (not a structural coincidence). Charlists and ok/error tuples both qualify — they are documented OTP conventions used by essentially all Erlang libraries.
2. **High frequency:** The pattern appears in the vast majority of FFI interactions. ok/error is the most common Erlang return pattern; charlists appear whenever string-accepting functions are called.
3. **Lossless:** The conversion preserves all information. `{ok, V}` → `Result ok: V` loses nothing; `Result toTuple` recovers the original. Charlist ↔ binary is similarly lossless for valid Unicode.
4. **Single boundary point:** The conversion happens at exactly one code path (`direct_call/3`), not scattered across the runtime. This keeps the coercion auditable and debuggable.
5. **Escape hatch exists:** Users can bypass the coercion when needed (`toTuple` for Result, explicit `Erlang unicode charactersToBinary:` for charlists).

**Patterns that do NOT qualify for future coercion** (for avoidance of doubt):
- Erlang records (`{record_name, ...}`) — not a universal convention, structure varies per module
- Property lists (`[{key, value}, ...]`) — ambiguous with regular lists of tuples
- Erlang maps (`#{key => value}`) — already pass through as Beamtalk Dictionaries via the object model

If a future proposal seeks a third coercion, it must satisfy all five criteria and reference this policy.

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
- **Existing code using Tuple-only methods breaks:** Code calling `isOk` or `at:` (positional extraction) on FFI returns will get a `does_not_understand` error since Result uses `ok` instead of `isOk` and doesn't support positional access. Note that `unwrap` and `isError` work on both Tuple and Result, so code using those methods continues to work. Migration path provided below
- **FFI/message asymmetry:** ok/error tuples from FFI calls become Result, but ok/error tuples received as messages from Erlang processes remain Tuple. This is a narrow inconsistency — documented in the scope section above, with `Result fromTuple:` available for explicit message conversion
- **Observer/recon display:** Result objects display as tagged maps (`#{'$beamtalk_class' => 'Result', ...}`) in Erlang debugging tools rather than the familiar `{ok, Value}` tuple. BEAM veterans inspecting process state may find this disorienting initially. Mitigated by the `'$beamtalk_class' => 'Result'` tag making the intent clear

### Neutral

- Tuple class retains `isOk`/`isError`/`unwrap` methods — they remain useful for tuples from non-FFI sources (e.g., Erlang data structures stored in ETS, messages from Erlang processes)
- `beamtalk_result:from_tagged_tuple/1` already exists and handles the conversion correctly — minimal new runtime code required
- The false-positive rate (non-error tuples tagged with `ok`/`error` that happen to be 2-element) is negligible in practice — `ok` and `error` atoms are used overwhelmingly for their conventional purpose in OTP and Hex packages

## Implementation

### Phase 1: Runtime Conversion (core change)
- **beamtalk_erlang_proxy.erl:** Add `coerce_result/1` after `coerce_charlist_result/1` in the `direct_call/3` pipeline
- **beamtalk_result.erl:** Add clauses for bare `ok`/`error` atoms in `from_tagged_tuple/1`
- **Result.bt:** Add `toTuple` instance method (reconstructs `{ok, V}` or `{error, R}`) and `Result fromTuple:` class method (explicit conversion for messages/ETS)
- **Tests:** EUnit tests for all conversion rules (2-element, bare atom, 3+ element passthrough, non-tuple passthrough); BUnit tests for `toTuple` round-tripping; e2e btscript tests for FFI calls returning Result
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

If existing code passes an FFI result back to an Erlang function expecting a tuple, this will break because Result is a tagged map, not a tuple. This is expected to be rare — a codebase audit found **zero production instances** of constructing ok/error tuples to pass to Erlang. If needed, construct the tuple explicitly:

```beamtalk
// If you need to pass an ok/error tuple to Erlang:
Erlang someModule process: #(#ok, value)
```

### Tests using `erlang:list_to_tuple/1` to create ok/error tuples

~15 test cases in `stdlib/test/` and `docs/learning/fixtures/` use `Erlang erlang list_to_tuple: #(#ok, 42)` to construct ok/error tuples for destructuring and pattern-matching tests. After this change, `list_to_tuple` returns a Result (since it's an FFI call returning `{ok, 42}`), breaking these tests.

Before:
```beamtalk
// Creates a Tuple, used for destructuring tests
t := Erlang erlang list_to_tuple: #(#ok, 42)
{#ok, value} := t  // destructures Tuple
```

After:
```beamtalk
// Use Tuple withAll: instead — not an FFI call, no conversion
t := Tuple withAll: #(#ok, 42)
{#ok, value} := t  // destructures Tuple as before
```

`Tuple withAll:` goes through stdlib dispatch, not the FFI proxy, so it is unaffected by the conversion. This is the recommended pattern for constructing tuples in Beamtalk regardless of this ADR — `list_to_tuple` was always an unnecessary Erlang detour.

**Affected files:**
- `stdlib/test/tuple_test.bt` (~6 test methods)
- `stdlib/test/destructuring_test.bt` (~4 test methods)
- `stdlib/test/pattern_matching_test.bt` (~2 test methods)
- `docs/learning/fixtures/ch14tuple_destructuring.bt` (~3 examples)

## References
- Related issues: BT-1838, BT-1127 (charlist coercion precedent)
- Related ADRs: ADR 0028 (BEAM interop — transparent interop principle), ADR 0060 (Result type — hybrid error handling), ADR 0068 (parametric types — `Result(T, E)` generics), ADR 0075 (Erlang FFI type definitions — spec extraction pipeline)
- Gate evaluation: `docs/ADR/0075-gate-evaluation.md` — 86.5% useful types in top-20 OTP modules
- Prior art: Swift/Obj-C automatic NSError → throws conversion, Gleam identical representation, Elixir manual `with` macro
