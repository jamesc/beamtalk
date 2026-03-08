# ADR 0060: Result Type — Hybrid Error Handling for Expected Failures

## Status
Proposed (2026-03-08)

## Context

### The Problem

Beamtalk currently uses **exception-based error handling** throughout, with structured `#beamtalk_error{}` records and the `on:do:` block syntax (ADR 0015). This works well for programming errors (`does_not_understand`, `arity_mismatch`, `immutable_value`) and aligns with BEAM's "let it crash" supervision model.

However, some operations have **expected failure modes** that aren't bugs — file I/O, parsing, network operations, type coercion. Using exceptions for these conflates "something went wrong in the program" with "this operation might not succeed."

### Current Pain Points

**1. FFI impedance mismatch:** Every Erlang FFI wrapper that calls functions returning `{ok, V} | {error, R}` must translate these structured tuples into Beamtalk exceptions. There are ~50+ such translation sites across `beamtalk_file.erl`, `beamtalk_http.erl`, `beamtalk_regex.erl`, `beamtalk_datetime.erl`, `beamtalk_subprocess.erl`, and `beamtalk_reactive_subprocess.erl`. Each new FFI wrapper (FileHandle — BT-1188, ReactiveSubprocess — BT-1187) adds more.

**2. Three incompatible error conventions** emerged in the Symphony orchestrator (~1000 lines of real Beamtalk application code):

```beamtalk
// Convention 1: Exceptions (stdlib)
content := [File readAll: path] on: Exception do: [:e | ^#missing_workflow_file]

// Convention 2: HTTPResponse.ok (HTTP-specific, not composable)
resp ok ifTrue: [resp bodyAsJson] ifFalse: [^#linear_api_status]

// Convention 3: Error symbols (ad-hoc, manual propagation)
result := self graphql: query variables: vars
(result class) =:= Symbol ifTrue: [^result]   // manual — noisy and error-prone
```

**3. The chain problem:** Multi-step fallible pipelines require manual sentinel checking at every step:

```beamtalk
// Current: manual error checking at each layer
load: path =>
  content := [File readAll: path] on: Exception do: [:e | ^#missing_workflow_file]
  parsed  := [Yaml parse: content] on: Exception do: [:e | ^#workflow_parse_error]
  (parsed class) =:= Symbol ifTrue: [^parsed]
  // ... more steps, each with its own error convention
```

### What Stays Unchanged

This ADR does NOT change the exception infrastructure from ADR 0015. Exceptions remain the correct mechanism for:

- Core dispatch errors (`does_not_understand`, `arity_mismatch`)
- Type violations in primitive operations
- Actor lifecycle errors (`immutable_value`, `instantiation_error`)
- OTP supervision / let-it-crash
- All codegen-emitted errors (~17 templates)
- REPL error display and `_error` binding

### Constraints

1. **BEAM-native** — Must interoperate cleanly with Erlang's `{ok, V} | {error, R}` convention
2. **Smalltalk-idiomatic** — Must feel like message sends, not a foreign concept
3. **Coexist with exceptions** — `on:do:` and supervision remain for bugs/crashes
4. **Value type** — Result is a tagged map (immutable value), not an actor
5. **No implicit unwrapping** — Results must be explicitly handled; no auto-unwrap that hides failures

## Decision

### Core Principle: Exceptions for Bugs, Results for Expected Failures

Introduce a `Result` value class for operations where failure is a normal, expected outcome. The dividing line:

| Failure mode | Mechanism | Example |
|---|---|---|
| File doesn't exist | **Result** | `File read: path` → `Result error: #file_not_found` |
| Parse input is malformed | **Result** | `Yaml parse: text` → `Result error: #parse_error` |
| Network is unreachable | **Result** | `HTTPClient get: url` → `Result error: #connection_failed` |
| Object doesn't understand message | **Exception** | `42 foo` → RuntimeError |
| Wrong number of arguments | **Exception** | `Array new: 1 with: 2 extra: 3` → RuntimeError |
| Actor is dead | **Exception** | Supervision restarts it |

**Guideline:** If the caller should reasonably expect and handle the failure as part of normal program flow, return a Result. If the failure indicates a programming mistake, raise an exception.

### 1. The Result Class

`Result` is a sealed Value subclass with two states: ok and error.

```beamtalk
// stdlib/src/Result.bt
sealed Value subclass: Result
  state: isOk :: Boolean = true
  state: value :: Object = nil
  state: error :: Object = nil

  // --- Constructors (class-side) ---

  /// Create a successful Result wrapping a value.
  class ok: value -> Result => Result new: #{ #isOk => true, #value => value }

  /// Create a failed Result wrapping an error reason.
  class error: reason -> Result => Result new: #{ #isOk => false, #error => reason }

  // --- Querying ---

  /// True if this Result holds a success value.
  sealed ok -> Boolean => self isOk

  /// True if this Result holds an error.
  sealed isError -> Boolean => self isOk not

  // --- Extracting ---

  /// Unwrap the success value, or return the default if error.
  sealed valueOr: default -> Object =>
    self isOk ifTrue: [self value] ifFalse: [default]

  /// Unwrap the success value, or evaluate a block with the error.
  sealed valueOrDo: block -> Object =>
    self isOk ifTrue: [self value] ifFalse: [block value: self error]

  /// Unwrap the success value, or raise an exception.
  sealed unwrap -> Object =>
    self isOk ifTrue: [self value] ifFalse: [
      Exception signal: "unwrap called on Result error: " ++ self error printString
    ]

  // --- Transforming ---

  /// Apply a block to the success value, wrapping the result in a new Result.
  /// If this is an error, return self unchanged.
  sealed map: block -> Result =>
    self isOk ifTrue: [Result ok: (block value: self value)] ifFalse: [self]

  /// Apply a block that itself returns a Result. Flattens the nesting.
  /// If this is an error, return self unchanged.
  sealed andThen: block -> Result =>
    self isOk ifTrue: [block value: self value] ifFalse: [self]

  /// Apply a block to the error, wrapping the result in a new error Result.
  /// If this is ok, return self unchanged.
  sealed mapError: block -> Result =>
    self isOk ifTrue: [self] ifFalse: [Result error: (block value: self error)]

  // --- Pattern matching ---

  /// Handle both cases with blocks.
  sealed ifOk: okBlock ifError: errorBlock -> Object =>
    self isOk
      ifTrue: [okBlock value: self value]
      ifFalse: [errorBlock value: self error]

  // --- Display ---

  sealed printString -> String =>
    self isOk
      ifTrue: ["Result ok: " ++ self value printString]
      ifFalse: ["Result error: " ++ self error printString]
```

### 2. REPL Usage

```
> Result ok: 42
// => Result ok: 42

> Result error: #file_not_found
// => Result error: #file_not_found

> (Result ok: 42) map: [:v | v + 1]
// => Result ok: 43

> (Result ok: 42) andThen: [:v | Result ok: v * 2]
// => Result ok: 84

> (Result error: #nope) map: [:v | v + 1]
// => Result error: #nope

> (Result ok: 42) valueOr: 0
// => 42

> (Result error: #nope) valueOr: 0
// => 0

> (Result ok: 42) ifOk: [:v | "got " ++ v printString]
                   ifError: [:e | "failed: " ++ e printString]
// => "got 42"

> (Result error: #nope) unwrap
// => Exception: unwrap called on Result error: #nope
```

### 3. Error Examples (Misuse)

```
> (Result ok: 42) andThen: [:v | v + 1]
// => TypeError: andThen: block must return a Result, got Integer
//    Hint: Use map: to transform the value, or wrap in Result ok:

> Result ok: 42 map: [:v | v + 1]
// => RuntimeError: Integer does not understand 'map:'
//    Hint: Wrap in parentheses: (Result ok: 42) map: [...]
//    (Beamtalk keyword messages associate right — ok: consumes everything after it)
```

### 4. Erlang FFI Convention

Erlang modules that return `{ok, V} | {error, R}` can surface these as Results via a helper in the runtime:

```erlang
%% beamtalk_result.erl — new module
-module(beamtalk_result).
-export([from_tagged_tuple/1]).

%% Convert Erlang {ok, V} | {error, R} | ok to Result tagged maps
from_tagged_tuple({ok, Value}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => true, 'value' => Value, 'error' => nil};
from_tagged_tuple(ok) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => true, 'value' => true, 'error' => nil};
from_tagged_tuple({error, Reason}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'value' => nil, 'error' => Reason}.
```

Usage in FFI modules:

```erlang
%% beamtalk_file.erl — AFTER migration
'readAll:'(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            beamtalk_result:from_tagged_tuple({ok, Content});
        {error, Reason} ->
            beamtalk_result:from_tagged_tuple({error, format_file_error(Reason, Path)})
    end.
```

This replaces the current 5-line error builder chain per error case:

```erlang
%% beamtalk_file.erl — BEFORE (current pattern, repeated ~50 times)
'readAll:'(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> Content;
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_error:raise(Error3);
        {error, eacces} ->
            %% ... another 5-line chain for permission_denied
    end.
```

### 5. tryDo: — Bridging Exceptions to Results

A class-side method on Result wraps exception-raising code into a Result:

```beamtalk
// Wrap any exception-raising expression into a Result
result := Result tryDo: [Yaml parse: untrustedInput]
// => Result ok: parsedValue   OR   Result error: anException

// Useful for calling legacy stdlib code that still raises
result := Result tryDo: [SomeLegacyLib process: data]
result ifOk: [:v | use: v] ifError: [:e | log: e message]
```

Implementation in Erlang:

```erlang
%% beamtalk_result.erl
'class_tryDo:'(_ClassSelf, _ClassVars, Block) ->
    try beamtalk_dispatch:send(Block, 'value', []) of
        Value -> from_tagged_tuple({ok, Value})
    catch
        error:Reason:_Stack ->
            ExObj = beamtalk_exception_handler:ensure_wrapped(Reason),
            from_tagged_tuple({error, ExObj})
    end.
```

### 6. Symphony Rewritten with Result

The multi-step pipeline from the Symphony motivating example:

```beamtalk
// BEFORE: three different error conventions, manual propagation
load: path =>
  content := [File readAll: path] on: Exception do: [:e | ^#missing_workflow_file]
  parsed  := [Yaml parse: content] on: Exception do: [:e | ^#workflow_parse_error]
  (parsed class) =:= Symbol ifTrue: [^parsed]
  self buildDefinition: parsed

// AFTER: composable Result chain
load: path =>
  (File readAll: path)
    andThen: [:content | Yaml parse: content]
    andThen: [:parsed  | self buildDefinition: parsed]
```

```beamtalk
// BEFORE: manual type-test propagation at every boundary
fetchCandidateIssues =>
  result := self graphql: query variables: vars
  (result class) =:= Symbol ifTrue: [^result]
  // ... process result

// AFTER: Result propagates automatically
fetchCandidateIssues =>
  (self graphql: query variables: vars)
    andThen: [:data | self extractIssues: data]
    andThen: [:issues | self filterCandidates: issues]
```

### 7. Guidelines: When to Use Which

| Situation | Use | Why |
|---|---|---|
| File might not exist | `Result` | Expected — the caller should handle it |
| YAML input might be malformed | `Result` | Expected — untrusted input |
| Network might be unreachable | `Result` | Expected — infrastructure is fallible |
| HTTP response might have error status | Neither — return `HTTPResponse` | Status codes are data, not errors |
| Integer doesn't understand `foo` | `Exception` | Bug — wrong message for type |
| Actor crashes | `Exception` + supervision | Infrastructure — let it crash |
| Wrong number of arguments | `Exception` | Bug — programming mistake |
| Type mismatch in primitive op | `Exception` | Bug — wrong types passed |
| Division by zero | `Exception` | Bug — mathematical error |
| Regex pattern is invalid | `Result` | Expected — user-supplied pattern |
| JSON parse of user input | `Result` | Expected — untrusted input |

**Rule of thumb:** If you'd put the operation in a `try` block in Elixir or use `?` in Rust, it should return a Result in Beamtalk. If you'd `raise` in Elixir or `panic!` in Rust, it should be an exception in Beamtalk.

## Prior Art

### Pharo / Squeak Smalltalk — Block-Based Fallbacks

Pharo handles expected failures via **block arguments**: `at: key ifAbsent: [default]`, `detect: [:x | pred] ifNone: [fallback]`, `readStreamDo: [:s | ...] ifAbsent: [nil]`. The block IS the error handler — the caller provides a closure that runs on failure.

There is no Result/Maybe/Optional type in Smalltalk tradition. The `ifAbsent:` pattern avoids wrapper types entirely.

**What we adopted:** The `ifOk:ifError:` message name follows the `ifTrue:ifFalse:` / `ifAbsent:` naming convention — Smalltalk-idiomatic keyword messages. `valueOr:` mirrors `valueOrDefault:` patterns. The block-argument style is preserved.

**What we departed from:** Pharo doesn't wrap the result — the caller provides inline handlers. We chose a Result wrapper because (a) it composes via `andThen:`, which block-argument APIs cannot, and (b) it maps directly to Erlang's `{ok, V} | {error, R}`, which block-argument APIs do not. The departure is justified by the chaining problem (§6 above) and FFI mapping needs.

### Gleam — Result Type (BEAM-native)

Gleam uses `Result(value, error)` exclusively — no exceptions exist. `use` expressions desugar into monadic chains:

```gleam
use username <- result.try(validate_username(input))
use password <- result.try(validate_password(input))
register_user(username, password)
```

`let assert Ok(value) = expr` crashes the process on `Error` (equivalent to `unwrap`).

**What we adopted:** The `Result` type with `map`, `try` (our `andThen:`), and explicit `unwrap`. The principle that libraries return Results and supervisors handle crashes.

**What we adapted:** Gleam has no exceptions at all — Result is the only error mechanism. Beamtalk keeps exceptions for bugs (ADR 0015) and adds Result for expected failures. This hybrid is closer to Elixir's model than Gleam's.

**What we rejected:** Gleam's `use` syntax sugar. Beamtalk's `andThen:` achieves the same composition via standard message sends — no special syntax needed.

### Elixir — `{:ok, v} | {:error, r}` Convention

Elixir uses tagged tuples by convention: `File.read("path")` returns `{:ok, contents}` or `{:error, :enoent}`. The `with` statement chains fallible operations:

```elixir
with {:ok, user} <- fetch_user(id),
     {:ok, account} <- fetch_account(user) do
  {:ok, account}
end
```

**What we adopted:** The principle of "errors as values" for expected failures. The clear separation between `{:ok, v}` (expected failure → Result) and `raise` (bugs → exceptions).

**What we adapted:** Elixir's tuples are raw data; our Result is a Beamtalk object that responds to messages. This follows Beamtalk's "everything is an object" principle while achieving the same semantics.

### Rust — `Result<T, E>` with `?` Operator

Rust's `Result<T, E>` with `?` for propagation is the gold standard for typed error handling. The `?` operator short-circuits on `Err`, propagating the error up the call stack.

**What we adopted:** `map`, `and_then` (our `andThen:`), `unwrap_or` (our `valueOr:`). The principle that Result is the return type for fallible operations.

**What we rejected:** The `?` operator. Beamtalk doesn't need special syntax because `andThen:` achieves propagation via standard message sends. Adding a new operator would violate "messages all the way down."

### Newspeak — Promises for Async Errors

Newspeak uses promises for asynchronous operations: if processing produces a result, the promise is "fulfilled"; if it raises an exception, the promise is "broken." This maps to the actor-based model — async errors are handled by promise chaining, not try/catch.

**What we noted:** Beamtalk's actor messaging is synchronous by default (ADR 0043), so promises are less immediately relevant. But the principle — fallible async operations return a value (Result/Promise) rather than raising — aligns with our decision.

### Summary

| Feature | Pharo | Gleam | Elixir | Rust | **Beamtalk (proposed)** |
|---------|-------|-------|--------|------|------------------------|
| Expected failure mechanism | `ifAbsent:` blocks | `Result(v, e)` | `{:ok, v} \| {:error, r}` | `Result<T, E>` | **`Result` class** |
| Bug mechanism | Exceptions | `panic` / `let assert` | `raise` | `panic!` | **Exceptions (ADR 0015)** |
| Composition | None (blocks inline) | `use` + `result.try` | `with` statement | `?` operator | **`andThen:`** |
| FFI mapping | N/A | Native | Native tuples | FFI crate-specific | **`beamtalk_result:from_tagged_tuple/1`** |
| Unwrap with default | `ifAbsent:` block | `result.unwrap` | `elem(tuple, 1)` | `.unwrap_or()` | **`valueOr:`** |
| Two systems coexist? | Yes (exceptions + blocks) | No (Result only) | Yes (tuples + raise) | Yes (Result + panic) | **Yes (Result + exceptions)** |

## User Impact

### Newcomer (from Python/JS/Ruby/Rust)

- `Result ok:` / `Result error:` reads clearly — "this might fail, handle both cases"
- `valueOr:` is the easy on-ramp: `config := (File read: path) valueOr: "{}"`
- `andThen:` is familiar from Promises (`.then()`), Rust (`.and_then()`), or functional JS
- Error messages guide them: `andThen: block must return a Result` tells them to use `map:` instead
- They can start with `valueOr:` and grow into `andThen:` chaining as they learn

### Smalltalk Developer

- `ifOk:ifError:` follows the `ifTrue:ifFalse:` naming convention — keyword messages, blocks as arguments
- Result IS an object that responds to messages — "everything is an object" is preserved
- The departure from Pharo's `ifAbsent:` pattern is justified by the composition problem that block-argument APIs cannot solve
- `on:do:` remains for catching bugs — the familiar Smalltalk exception mechanism is unchanged
- May initially question why a wrapper type is needed when blocks work in Pharo — the answer is `andThen:` chaining across function boundaries

### Erlang/BEAM Developer

- `{ok, V} | {error, R}` maps 1:1 to `Result ok: v` / `Result error: r` — the most natural BEAM interop possible
- `beamtalk_result:from_tagged_tuple/1` eliminates 50+ lines of error translation boilerplate per FFI module
- OTP supervision is unchanged — actor crashes are still exceptions, handled by supervisors
- They can pattern match on Result tagged maps from Erlang: `#{'$beamtalk_class' := 'Result', 'isOk' := true, 'value' := V}`
- `tryDo:` lets them wrap legacy exception-based code without refactoring it

### Production Operator

- Clear separation: Result errors are expected and logged at info/warning level; exceptions are bugs logged at error level
- Result values in crash logs show `Result error: reason` — more informative than raw exception traces for expected failures
- No change to supervision behavior — only programming errors trigger restarts
- `unwrap` crashes are intentional "this should never fail" assertions — visible in crash logs as explicit choices

### Tooling Developer (LSP/IDE)

- `state:` declarations on Result give the LSP field completions (`isOk`, `value`, `error`)
- Return type annotations (`-> Result`) let the LSP suggest `ifOk:ifError:`, `map:`, `andThen:` completions
- The `andThen:` chain is statically analyzable — each step's block parameter type flows from the previous step's Result value type (future gradual typing)

## Steelman Analysis

### Option A: Pure Result (no block sugar)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Fewer methods to learn — just `map`, `andThen`, and `unwrap`. Less API surface." |
| **BEAM veteran** | "Cleaner mapping to Gleam's Result — one canonical API, no Smalltalk sugar on top." |
| **Language designer** | "Simpler implementation — Result is just a data type with standard methods. No bridge patterns." |

**Tension:** Pure Result is sufficient functionally but misses the Smalltalk feel that makes Beamtalk distinctive.

### Option B: Block-Based Fallbacks (Smalltalk-pure, no Result type)

| Cohort | Strongest argument |
|--------|-------------------|
| **Smalltalk purist** | "This IS how Pharo does it. `at: key ifAbsent:` is the canonical pattern. No wrapper types needed — blocks are the composition tool." |
| **Newcomer** | "I just add `ifError:` to the method call — no new type to learn, no wrapping/unwrapping." |
| **Language designer** | "One mechanism (blocks) instead of two (blocks + Result). Every fallible method gets an `ifError:` variant — consistent, discoverable." |

**Tension:** Block-based fallbacks cannot compose across function boundaries. Symphony's `fetchCandidateIssues → graphql → extractIssues → filterCandidates` pipeline requires manual propagation with blocks; `andThen:` solves this structurally. The API explosion (every fallible method needs 2+ variants) is also a real cost.

### Option D: Status Quo (exceptions only)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One error system is simpler than two. I just learn `on:do:` and I'm done." |
| **BEAM veteran** | "Let it crash works. If you're catching expected errors, you're doing it wrong — design your system so failures restart cleanly." |
| **Operator** | "Fewer moving parts, fewer surprises. One error path to monitor and alert on." |

**Tension:** The status quo is defensible for small programs but breaks down at application scale (Symphony). The real-world evidence of three incompatible conventions emerging organically demonstrates that the language needs to provide a standard mechanism before users invent ad-hoc ones.

### Tension Points

- **Smalltalk purists vs BEAM veterans:** Purists want block-based handlers (Pharo tradition); BEAM veterans want Result types (Erlang/Gleam tradition). The hybrid satisfies both by making Result respond to block-argument messages (`ifOk:ifError:`)
- **Simplicity vs composition:** One error system (Option D) is simpler, but the chaining problem is real and worsens with FFI surface area growth
- **API migration scope:** Converting File I/O from exceptions to Results is a breaking change for existing `on:do:` callers. Migration must be phased: add Result-returning variants first, deprecate exception-raising versions later
- **Two systems overhead:** Developers must choose between Result and exceptions. The guideline ("expected failures → Result, bugs → exceptions") is clear in principle but boundary cases exist (e.g., is "parse error" expected or a bug? Depends on whether input is trusted)

## Alternatives Considered

### Alternative A: Block-Based Fallbacks Only (Pharo-style)

Add `ifError:` variants to all fallible methods:

```beamtalk
content := File readAll: path ifError: [:e | "{}"]
parsed := Yaml parse: content ifError: [:e | ^defaultConfig]
```

**Rejected because:** Cannot compose across function boundaries. Each fallible method needs 2+ variants (with/without `ifError:`), creating API explosion. The Symphony pipeline problem (`andThen:` chaining) cannot be solved with this approach. And it doesn't map naturally to Erlang's `{ok, V} | {error, R}` — there's no value to pass around or chain over.

### Alternative B: Extend on:do: with Result Sugar

Make `on:do:` return a Result instead of re-raising:

```beamtalk
result := [File readAll: path] asResult
result andThen: [:content | [Yaml parse: content] asResult]
```

**Rejected because:** Conflates two different semantics. `on:do:` catches exceptions (including bugs); Result represents expected failures. Wrapping exceptions as Results hides bugs that should crash. The `tryDo:` escape hatch covers the legitimate use case (wrapping legacy exception-based code) without making it the primary pattern.

### Alternative C: Erlang-Style Tagged Tuples

Return raw `{ok, V}` / `{error, R}` tuples to Beamtalk code:

```beamtalk
result := File readAll: path   // returns #(#ok, content) or #(#error, reason)
result first =:= #ok ifTrue: [process: result second]
```

**Rejected because:** Raw tuples are not objects — they don't respond to `map:`, `andThen:`, `ifOk:ifError:`. This violates "everything is an object" (Principle 6). Pattern matching on tuple position is fragile and unreadable. It's the anti-pattern that both Gleam and Beamtalk's object model are designed to improve upon.

### Alternative D: Optional/Maybe Type (Separate from Result)

Add `Optional` for "might be nil" and `Result` for "might fail with a reason":

```beamtalk
// Optional — no error reason
name := dict at: "name"  // => Optional some: "James" or Optional none

// Result — with error reason
content := File readAll: path  // => Result ok: "..." or Result error: #file_not_found
```

**Rejected for now:** Adds complexity. Beamtalk already uses `nil` for absent values (Smalltalk tradition). An `Optional` type would compete with `nil`. Result can represent "absent" via `Result error: #not_found`. If demand emerges for a nil-safe wrapper, it can be added as a separate ADR without affecting Result.

## Consequences

### Positive

- **Composable error handling** — `andThen:` chains replace manual sentinel propagation
- **Natural FFI mapping** — `{ok, V} | {error, R}` maps directly to `Result ok: v` / `Result error: r`
- **Eliminates FFI boilerplate** — `beamtalk_result:from_tagged_tuple/1` replaces ~50+ error builder chains
- **Clear separation** — Exceptions for bugs, Results for expected failures. Developers know which to use
- **Smalltalk-idiomatic** — `ifOk:ifError:` follows `ifTrue:ifFalse:` convention; Result responds to messages
- **One standard convention** — Prevents ad-hoc error symbols and incompatible per-library conventions
- **REPL-friendly** — `Result ok: 42` and `Result error: #nope` display clearly
- **Discoverable** — `Result` has a small, focused API; completion shows `map:`, `andThen:`, `valueOr:`

### Negative

- **Two error systems** — Developers must learn both `on:do:` (exceptions) and `Result` (expected failures). The guideline is clear but edge cases exist
- **Breaking change for File I/O callers** — `File readAll:` changing from direct value / exception to Result requires callers to handle Result. Must be phased carefully
- **New dependency in stdlib** — Result must load before any Result-returning class (File, Yaml, etc.)
- **Unwrap-induced crashes** — `unwrap` on a Result error crashes with an exception. Developers who overuse `unwrap` get the worst of both worlds
- **API documentation effort** — Every method migrated to Result needs updated docs and examples

### Neutral

- **Exception infrastructure unchanged** — `on:do:`, `ensure:`, `_error`, error class hierarchy all remain as-is (ADR 0015)
- **Supervision unchanged** — Actor crashes are still exceptions; OTP supervisors handle them
- **REPL error display unchanged** — Exceptions still display as `RuntimeError: ...`. Results display as `Result error: ...`
- **Existing exception-based code continues to work** — `on:do:` still catches exceptions. No forced migration
- **`tryDo:` bridges the gap** — Legacy code that raises exceptions can be wrapped in Result without refactoring

## Implementation

### Phase 1: Result Class + Runtime Helper (S)

**Components:** stdlib (Beamtalk), runtime (Erlang)

1. **`stdlib/src/Result.bt`** — Value class with `state:` declarations, `ifOk:ifError:`, `map:`, `andThen:`, `valueOr:`, `valueOrDo:`, `unwrap`, `mapError:`, `printString`
2. **`runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl`** — `from_tagged_tuple/1` helper for FFI modules, `class_tryDo:/3` for `tryDo:` class method
3. **`stdlib/test/result_test.bt`** — BUnit tests for all Result methods
4. **Boot order** — Ensure Result loads before File, Yaml, etc. in `build_stdlib.rs`

### Phase 2: Migrate File I/O to Result (M)

**Components:** runtime (Erlang), stdlib tests

1. **`beamtalk_file.erl`** — Convert `readAll:`, `writeAll:to:`, `exists:`, `makeDirectory:`, `deleteRecursive:`, `list:`, `stat:` to return Result via `from_tagged_tuple/1`. ~8 methods, each replacing 10-30 lines of error builder chains with 2-3 lines
2. **Preserve exception variants** — Add `readAll:orRaise:` (or similar) for callers who want exception behavior. Or: provide `unwrap` as the escape hatch
3. **Update stdlib tests** — `stdlib/test/file_test.bt` tests for Result-returning methods
4. **Update e2e tests** — `tests/e2e/cases/` file-related tests

### Phase 3: Migrate Other FFI Modules (M)

**Components:** runtime (Erlang), stdlib (Beamtalk)

1. **`beamtalk_regex.erl`** — `Regex from:` returns Result (pattern might be invalid)
2. **`beamtalk_http.erl`** — Network errors return Result; HTTP status codes remain on HTTPResponse (not errors)
3. **`beamtalk_subprocess.erl`** — Process startup failures return Result
4. **Yaml/JSON parsing** — Parse errors return Result (untrusted input)
5. **Update tests and docs**

### Phase 4: Language Features Documentation (S)

**Components:** docs

1. **`docs/beamtalk-language-features.md`** — Add Result type section with examples
2. **Error handling guide** — When to use Result vs exceptions, with examples
3. **FFI authoring guide** — How to use `from_tagged_tuple/1` in new Erlang modules

## Migration Path

### For Callers of File I/O Methods

```beamtalk
// BEFORE: exception-based
content := File readAll: path
// or
content := [File readAll: path] on: IOError do: [:e | "default"]

// AFTER: Result-based
content := (File readAll: path) unwrap          // crashes on error (same as before)
// or
content := (File readAll: path) valueOr: "default"  // safe fallback
// or
(File readAll: path)
  ifOk: [:content | process: content]
  ifError: [:e | handleError: e]
```

### For FFI Module Authors

```erlang
%% BEFORE: manual error builder chain
case some_erlang_call(Args) of
    {ok, Value} -> Value;
    {error, Reason} ->
        Error0 = beamtalk_error:new(some_kind, 'MyClass'),
        Error1 = beamtalk_error:with_selector(Error0, 'myMethod:'),
        Error2 = beamtalk_error:with_details(Error1, #{arg => Args}),
        Error3 = beamtalk_error:with_hint(Error2, <<"Helpful hint">>),
        beamtalk_error:raise(Error3)
end.

%% AFTER: one-line conversion
beamtalk_result:from_tagged_tuple(some_erlang_call(Args)).
%% Or with error context:
case some_erlang_call(Args) of
    {ok, Value} -> beamtalk_result:from_tagged_tuple({ok, Value});
    {error, Reason} ->
        beamtalk_result:from_tagged_tuple({error, #{reason => Reason, path => Path}})
end.
```

### Phased Rollout

1. **Phase 1:** Ship `Result.bt` and `beamtalk_result.erl`. No existing APIs change. Users can create Results manually and use `tryDo:` to wrap exception-based code.
2. **Phase 2:** Migrate `File` methods to return Result. This is a breaking change for direct callers but `unwrap` provides backward-compatible behavior.
3. **Phase 3:** Migrate remaining FFI modules. Each migration is independent.
4. **Phase 4:** Document the convention. New FFI modules should return Result by default.

## References

- Related issues: [BT-449](https://linear.app/beamtalk/issue/BT-449) — Research: Errors as Results
- Related ADRs:
  - [ADR 0015](0015-repl-error-objects-and-exception-hierarchy.md) — Signal-Time Exception Objects and Error Class Hierarchy (exceptions infrastructure — unchanged)
  - [ADR 0055](0055-erlang-backed-class-authoring-protocol.md) — Erlang-Backed Class Authoring Protocol (FFI surface area)
  - [ADR 0056](0056-native-erlang-backed-actors.md) — Native Erlang-Backed Actors (FFI surface area)
  - [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) — BEAM Object Model (Result is a value type)
  - [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) — Immutable Value Objects (Result is immutable)
  - [ADR 0028](0028-beam-interop-strategy.md) — BEAM Interop Strategy (FFI mechanism)
- External references:
  - Gleam Result type: https://hexdocs.pm/gleam_stdlib/gleam/result.html
  - Rust std::result: https://doc.rust-lang.org/std/result/
  - Elixir error handling: https://hexdocs.pm/elixir/main/try-catch-and-rescue.html
  - Pharo exception handling: https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Exceptions.md
