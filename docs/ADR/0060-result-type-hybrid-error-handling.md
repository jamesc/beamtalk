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
  state: okValue :: Object = nil
  state: errReason :: Object = nil
  state: isOk :: Boolean = true

  // --- Constructors (class-side) ---

  /// Create a successful Result wrapping a value.
  class ok: value -> Result => Result new: #{ #isOk => true, #okValue => value }

  /// Create a failed Result wrapping an error reason.
  class error: reason -> Result => Result new: #{ #isOk => false, #errReason => reason }

  // --- Querying ---

  /// True if this Result holds a success value.
  sealed ok -> Boolean => self isOk

  /// True if this Result holds an error.
  sealed isError -> Boolean => self isOk not

  // --- Guarded accessors ---
  // Named `value` and `error` for ergonomics, but guarded to prevent
  // silent nil when accessing the wrong state. The internal fields
  // (okValue, errReason) are available but undocumented — public API
  // is via these guarded methods or the safe combinators below.

  /// The success value. Raises if this is an error Result.
  sealed value -> Object =>
    self isOk ifTrue: [self okValue] ifFalse: [
      Exception signal: "Cannot access 'value' on Result error — use valueOr:, ifOk:ifError:, or unwrap"
    ]

  /// The error reason. Raises if this is an ok Result.
  sealed error -> Object =>
    self isOk ifFalse: [self errReason] ifTrue: [
      Exception signal: "Cannot access 'error' on Result ok — use ifOk:ifError: or mapError:"
    ]

  // --- Extracting ---

  /// Unwrap the success value, or return the default if error.
  sealed valueOr: default -> Object =>
    self isOk ifTrue: [self okValue] ifFalse: [default]

  /// Unwrap the success value, or evaluate a block with the error.
  sealed valueOrDo: block -> Object =>
    self isOk ifTrue: [self okValue] ifFalse: [block value: self errReason]

  /// Unwrap the success value, or raise an exception.
  sealed unwrap -> Object =>
    self isOk ifTrue: [self okValue] ifFalse: [
      Exception signal: "unwrap called on Result error: " ++ self errReason printString
    ]

  // --- Transforming ---

  /// Apply a block to the success value, wrapping the result in a new Result.
  /// If this is an error, return self unchanged.
  sealed map: block -> Result =>
    self isOk ifTrue: [Result ok: (block value: self okValue)] ifFalse: [self]

  /// Apply a block that itself returns a Result. Flattens the nesting.
  /// If this is an error, return self unchanged.
  sealed andThen: block -> Result =>
    self isOk ifTrue: [block value: self okValue] ifFalse: [self]

  /// Apply a block to the error, wrapping the result in a new error Result.
  /// If this is ok, return self unchanged.
  sealed mapError: block -> Result =>
    self isOk ifTrue: [self] ifFalse: [Result error: (block value: self errReason)]

  // --- Pattern matching ---

  /// Handle both cases with blocks.
  sealed ifOk: okBlock ifError: errorBlock -> Object =>
    self isOk
      ifTrue: [okBlock value: self okValue]
      ifFalse: [errorBlock value: self errReason]

  // --- Display ---

  sealed printString -> String =>
    self isOk
      ifTrue: ["Result ok: " ++ self okValue printString]
      ifFalse: ["Result error: " ++ self errReason printString]
```

### 2. REPL Usage

```text
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

```text
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

%% Convert Erlang {ok, V} | {error, R} to Result tagged maps.
%% Uses internal field names (okValue, errReason) to prevent unguarded
%% field access — the public API goes through guarded value/error methods.
%%
%% NOTE: from_tagged_tuple/1 strictly handles {ok, V} | {error, R}.
%% It does NOT accept bare ok atoms — the atom ok maps to the Beamtalk
%% Symbol #ok, not the boolean true, and conflating the two would silently
%% change the payload type. For functions returning bare ok (e.g., file:close/1),
%% FFI authors must handle the atom explicitly:
%%   ok -> beamtalk_result:ok(ok_symbol_here)  % or wrap in {ok, unit_value}
%% For {ok, V1, V2} multi-value tuples, normalize before calling this helper.
from_tagged_tuple({ok, Value}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => Value, 'errReason' => nil};
from_tagged_tuple({error, Reason}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil, 'errReason' => Reason}.
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

**Scope of `from_tagged_tuple/1`:** This helper covers `{ok, V} | {error, R}` only. It does **not** accept bare `ok` atoms — the atom `ok` is the Beamtalk Symbol `#ok`, not `true`, and silently coercing it would change the payload type. FFI authors must handle bare `ok` explicitly (e.g., wrapping it as `{ok, nil}` or constructing the Result map directly). Multi-value tuples `{ok, V1, V2}` and other shapes must also be normalized before calling this helper. Functions that crash on failure (e.g., `erlang:binary_to_integer/1` raising `badarg`) should use `tryDo:` instead.

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
            from_tagged_tuple({error, ExObj});
        throw:Reason:_Stack ->
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

### 7. Actor Methods Returning Result

An actor method can return a Result. This is a normal return value — it flows through `gen_server:call` as `{ok, ResultMap}` and is unwrapped by `sync_send/3` to just `ResultMap`. Crucially:

- **A `Result error:` from an actor method does NOT trigger a supervisor restart.** The actor handled the message successfully — it just returned a value indicating the operation failed.
- **An exception from an actor method DOES trigger a restart** (via the normal `{error, Error}` reply path).
- This is the correct semantics: expected failures (file not found, parse error) should not crash the actor. Bugs (wrong message, type error) should.

```beamtalk
// Actor method returns Result — actor stays alive
readConfig =>
  (File readAll: self configPath)
    andThen: [:content | Yaml parse: content]

// Caller handles the Result
config := worker readConfig
config ifOk: [:c | use: c] ifError: [:e | useDefaults]
// worker is still alive regardless of the Result
```

**Caution — self-sends inside `andThen:` blocks:** If an actor chains `andThen:` blocks that send messages back to self, the same deadlock risk applies as with any self-send inside a block (known issue — synchronous gen_server:call to self blocks). This is not unique to Result but is more likely with `andThen:` chains:

```beamtalk
// DEADLOCK RISK: self-send inside andThen: block in an actor method
processFile: path =>
  (File readAll: path)
    andThen: [:content | self validate: content]   // self-send — deadlocks!

// SAFE: extract to local variable or use map: for pure transforms
processFile: path =>
  result := File readAll: path
  result andThen: [:content | self validate: content]  // same deadlock — still a self-send
  // Solution: don't self-send in fallible chains. Use pure functions or refactor.
```

### 8. REPL Display of Result Errors

When a Result error is returned at the REPL, it is displayed as a **normal result** (not an error):

```text
> File readAll: "/nonexistent"
// => Result error: #file_not_found     (displayed as a value, not an error)
> _
// => Result error: #file_not_found     (bound to _ as last result)
> _error
// => nil                               (_error is NOT set — no exception occurred)
```

This is correct: the expression evaluated successfully and returned a Result value. `_error` is only set when an exception is raised. Users coming from exception-based error handling may initially expect `_error` to be set — the documentation should clarify this distinction.

**Future consideration:** The REPL could render `Result error:` values with different formatting (e.g., yellow text vs green) to visually signal that the result represents a failure. This is a UX enhancement, not a semantic change.

### 9. Forward Compatibility: Match Expression Integration

If `match:` gains class/structural patterns (planned separately), Result becomes directly destructurable:

```beamtalk
// Future: class pattern arms in match:
(File readAll: path) match: [
  Result ok: content -> process: content;
  Result error: e    -> handleError: e
]
```

This is an alternative to `ifOk:ifError:` for **exhaustive case analysis** — the match enforces that both arms are present and the compiler can warn on missing cases. `ifOk:ifError:` remains the idiomatic API for **chaining** (`andThen:`, `map:`), where the Result stays wrapped and propagates through a pipeline.

The two are complementary: `ifOk:ifError:` for pipelines, `match:` for terminal case dispatch. No changes to ADR 0060's design are needed to support this — Result's tagged map representation (`isOk`, `okValue`, `errReason`) is straightforwardly matchable by a structural pattern system.

### 10. Forward Compatibility: Parameterized Result Types

The current design uses `-> Result` as the return type annotation, which doesn't express what types the ok value and error carry. When gradual typing (ADR 0025) matures, Result should support parameterized type annotations:

```beamtalk
// Current (unparameterized)
sealed readAll: path :: String -> Result => ...

// Future (parameterized, when ADR 0025 supports it)
sealed readAll: path :: String -> Result(String, IOError) => ...
```

The runtime representation (tagged map with `isOk`, `okValue`, `errReason` fields) is compatible with future type parameterization — the type parameters constrain what values the fields may hold, they don't change the representation. No runtime changes will be needed; this is purely a type-system concern.

### 11. Guidelines: When to Use Which

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

**Heuristic — the boundary test:** If the method's primary input comes from outside the program's control (user input, filesystem, network, external process), return Result. If the method operates on already-validated internal data, raise exceptions. Concretely:

- **External input → Result:** The caller passes data that might be malformed, missing, or inaccessible. The method cannot assume the input is valid.
- **Internal contract → Exception:** The caller passes data that the program itself produced and should have validated. A failure here means a bug in the program, not bad input.

**Edge cases and how to resolve them:**

| Situation | Result or Exception? | Why |
|---|---|---|
| `Integer parse: userInput` | **Result** | User input — might not be a number |
| `42 + "abc"` | **Exception** | Internal — program passed wrong type |
| `HTTPClient get: url` (timeout) | **Result** | Network — external, inherently unreliable |
| `actor someMethod` (gen_server timeout) | **Exception** | Internal — actor should have responded |
| `File readAll: path` (from config) | **Result** | Filesystem — file might not exist |
| `dict at: key` (key from user) | Existing `ifAbsent:` pattern | Already handled by block fallback |
| `Yaml parse: content` (from API) | **Result** | External data — might be malformed |
| `Yaml parse: content` (from own code) | Use `unwrap` or `tryDo:` | You trust your own data but can assert |

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

### Ruby — `dry-monads` (Dynamic Language Precedent)

Ruby is dynamically typed and has no gradual typing. The `dry-monads` gem (part of the dry-rb ecosystem) provides `Success(value)` / `Failure(reason)` with `fmap` (our `map:`), `bind` (our `andThen:`), and `value_or` (our `valueOr:`):

```ruby
result = File.read("config.yml")
  .then { |content| YAML.safe_load(content) }
  .then { |parsed| build_config(parsed) }

case result
in Success(config) then use(config)
in Failure(reason) then use_defaults
end
```

`dry-monads` is widely used in production Rails applications — without type annotations. The chaining ergonomics (`bind`, `fmap`) provide value purely as a runtime convention: callers know what shape they're getting back, and the composition is explicit. The Ruby community does not find the absence of static enforcement to be a blocker; the convention itself is sufficient.

**What this confirms for Beamtalk:** Result works as a dynamic runtime convention. The ergonomic value — composable chaining, explicit failure shape, no surprise nil — does not require a type checker to be useful. This is the strongest dynamic-language precedent for our design.

**What we adapted:** Ruby uses `Success`/`Failure` constructors (following Haskell/Scala terminology). We use `Result ok:`/`Result error:` — more explicit about the container type, consistent with Beamtalk's keyword message style.

### What We're Actually Borrowing from Rust and Gleam

Rust and Gleam are both statically typed. It might seem odd to cite them as prior art for a dynamic, live-environment language. The distinction is important: **we are adopting their runtime ergonomics and FFI convention, not their static enforcement model.**

What makes Rust's `Result<T, E>` and Gleam's `Result(v, e)` valuable has two parts:

1. **Static part** — The type checker enforces exhaustive handling at every call site. Neither we nor Elixir have this for dynamic code, and Beamtalk cannot have it in the general case because new classes can be defined at the REPL at any time (open-world, live system).

2. **Runtime part** — `Result` is a structured value with known shape. `map`, `and_then`, `unwrap_or` are message sends that compose at runtime, regardless of what the type checker knows. The FFI mapping from `{ok, V} | {error, R}` is a runtime convention, not a type system feature.

Beamtalk adopts part 2 entirely. Part 1 is available in typed contexts (gradual typing annotations) but is never the primary enforcement mechanism. This is the right split: the runtime convention works uniformly across dynamic and typed code, and typed code gets additional static guarantees on top. Attempting to rely on part 1 alone — union types without a runtime wrapper — fails for Beamtalk because the open-world live system makes exhaustiveness checking unsound (Alternative G).

### Summary

| Feature | Pharo | Ruby dry-monads | Gleam | Elixir | Rust | **Beamtalk (proposed)** |
|---------|-------|-----------------|-------|--------|------|------------------------|
| Typed? | Dynamic | Dynamic | Static | Dynamic | Static | **Dynamic + optional types** |
| Expected failure mechanism | `ifAbsent:` blocks | `Success`/`Failure` | `Result(v, e)` | `{:ok, v} \| {:error, r}` | `Result<T, E>` | **`Result` class** |
| Bug mechanism | Exceptions | Exceptions | `panic` / `let assert` | `raise` | `panic!` | **Exceptions (ADR 0015)** |
| Composition | None (blocks inline) | `bind` / `fmap` | `use` + `result.try` | `with` statement | `?` operator | **`andThen:`** |
| Static exhaustiveness? | No | No | Yes | No | Yes | **No (open-world live system)** |
| FFI mapping | N/A | N/A | Native | Native tuples | FFI crate-specific | **`beamtalk_result:from_tagged_tuple/1`** |
| Unwrap with default | `ifAbsent:` block | `value_or` | `result.unwrap` | `elem(tuple, 1)` | `.unwrap_or()` | **`valueOr:`** |
| Two systems coexist? | Yes (exceptions + blocks) | Yes | No (Result only) | Yes (tuples + raise) | Yes (Result + panic) | **Yes (Result + exceptions)** |

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
- They can pattern match on Result tagged maps from Erlang: `#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := V}`
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
| **Newcomer** | "One error system is simpler than two. I don't want to learn BOTH `on:do:` AND Result — which do I use when?" |
| **Smalltalk purist** | "Result is a Haskell monad in Smalltalk clothing. Blocks + `on:do:` IS the Smalltalk way. The Symphony pain could be solved by a `pipeline:steps:` method that chains `on:do:` wrappers, not by importing Rust's type system." |
| **BEAM veteran** | "Let it crash works. If you're catching expected errors, you're doing it wrong — design your system so failures restart cleanly." |
| **Operator** | "Fewer moving parts, fewer surprises. One error path to monitor and alert on." |

**Tension:** The status quo is defensible for small programs but breaks down at application scale (Symphony). The real-world evidence of three incompatible conventions emerging organically demonstrates that the language needs to provide a standard mechanism before users invent ad-hoc ones. The Smalltalk purist's pipeline suggestion is worth exploring but would need its own design — and it still doesn't solve the FFI impedance mismatch with `{ok, V} | {error, R}`.

### Option E: `tryDo:` Only (Minimal Phase 1 as Final State)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "I wrap things in `tryDo:` when I want to chain them — one new concept, not a whole new error system." |
| **BEAM veteran** | "No breaking changes to existing APIs. The FFI modules keep working. I add composition on top without touching the foundation." |
| **Operator** | "Zero migration risk. Existing crash logs don't change. New code CAN use Result if it wants." |

**Tension:** `tryDo:` alone provides 80% of the composability value with 0% breaking changes. The argument for native Result returns (Phases 2-3) is primarily performance — avoiding the exception construction + catch + wrap round-trip — and ergonomics — `File readAll: path` reading more naturally than `Result tryDo: [File readAll: path]`. If the performance argument is weak (error paths are already slow), `tryDo:`-only may be sufficient for a long time.

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

### Alternative D: Dual API Convention (Elixir `!` Pattern)

Provide both exception-raising and Result-returning variants of every fallible method:

```beamtalk
// Result-returning (new)
result := File readAll: path
result ifOk: [:content | process: content] ifError: [:e | handleError: e]

// Exception-raising (existing, kept as convenience)
content := File readAllOrRaise: path   // raises IOError on failure
```

This follows Elixir's `File.read/1` vs `File.read!/1` convention. The caller chooses the error handling style at the call site.

**Not adopted as the primary pattern because:** API surface doubles — every fallible method needs two variants. The naming convention (`readAllOrRaise:` or `readAll!:`) is un-Smalltalk-like. And `unwrap` already provides the "I want an exception" escape hatch: `(File readAll: path) unwrap`.

**However:** The adversarial review correctly notes that `unwrap` produces a generic error message ("unwrap called on Result error: ...") that loses the original error context (file path, hint text). Implementation should consider whether `unwrap` can preserve the original error context — e.g., if the error reason is an Exception object (from `tryDo:`), re-raise it directly rather than wrapping in a new signal.

### Alternative E: `tryDo:` Only (Minimal Approach)

Ship only `Result.bt` and `tryDo:`. Don't migrate any FFI modules. Users compose via `tryDo:` wrapping:

```beamtalk
(Result tryDo: [File readAll: path])
  andThen: [:content | Result tryDo: [Yaml parse: content]]
  andThen: [:parsed  | self buildDefinition: parsed]
```

This provides composability with **zero breaking changes** — all existing APIs keep raising exceptions, and `tryDo:` wraps them into Results at the call site.

**Not adopted as the final state because:** Every `tryDo:` pays the cost of constructing a `#beamtalk_error{}`, raising it, catching it, and wrapping it — when the FFI could return a Result directly from `{ok, V} | {error, R}` without the exception round-trip. For methods called in tight loops (e.g., parsing each line of a file), this overhead matters.

**However:** This was considered as a cautious Phase 1 approach. Ultimately, existing callers (Symphony) are already in poor shape with ad-hoc error conventions, and deferring the FFI migration only entrenches `tryDo:` as a substitute convention. The ADR ships Result and migrates FFI modules together.

### Alternative G: Naked Union Return Types (TypeScript Approach)

Return the value or an error object directly, without a wrapper. Callers discriminate with `isKindOf:` or a type annotation:

```beamtalk
// No wrapper — return String or FileError directly
File readAll: path  // => "content..." or FileError(#file_not_found)

// Caller discriminates at runtime
content := File readAll: path
(content isKindOf: FileError)
  ifTrue: [useDefaults]
  ifFalse: [process: content]
```

With gradual typing, the return annotation could express the union:

```beamtalk
class File
  readAll: path :: String -> String | IOError =>
    ...
```

**Why TypeScript uses this:** TypeScript's structural type system + exhaustiveness checking enforces that callers handle both branches at compile time. Type narrowing (`if (result instanceof Error)`) is syntactically lightweight. The runtime value is just the value — no wrapper object, no allocation.

**Why this degrades in Beamtalk's dynamic mode:**

In dynamic mode (the common case), `-> String | IOError` is an unenforced annotation — documentation, not a contract. Callers can ignore the error branch silently. There is no compile-time exhaustiveness check. This is strictly *worse* than exceptions, which at minimum surface as runtime crashes. It's also exactly what Symphony was already doing with Symbol sentinels (`(result class) =:= Symbol`) — the worst of the three ad-hoc conventions the ADR was written to replace.

The discrimination syntax (`isKindOf:`) is also heavier than TypeScript's `instanceof` narrowing, and the result is not composable — there is no `andThen:` or `map:` without reinventing the Result container.

**The structural problem — not a maturity problem:** This alternative is sometimes framed as "viable once gradual typing matures." That framing is wrong at two levels.

First, typing is always optional — dynamic-mode callers permanently exist with no exhaustiveness enforcement.

Second, and more fundamentally: Beamtalk is a **live, open-world system**. New classes can be created at the REPL at any time. The class hierarchy is open and mutable at runtime. Static exhaustiveness checking assumes a closed world where the set of types is fixed at compile time — an assumption Beamtalk explicitly rejects. Even fully-annotated code cannot guarantee exhaustiveness over `String | IOError` because new `IOError` subclasses can be defined mid-session. The type checker would have to re-verify all union branches on every new class definition, which is either unsound or requires whole-program re-checking on every REPL eval.

This is not a gap to close with better tooling. It is a consequence of the core design principle: **the REPL is not a sandbox, it is the live system.** A static exhaustiveness discipline that breaks on every new class definition is not a discipline — it is friction.

**Not adopted because:** Static exhaustiveness is irreconcilable with a live open-world system. The Result wrapper is the correct design for this context: it provides runtime shape guarantees (`$beamtalk_class`, `isOk`, guarded accessors) that hold regardless of typing mode or class hierarchy mutations. Composability (`andThen:`, `map:`) works at runtime without a type checker. Union type annotations could complement Result (e.g., `result :: Result(String, IOError)` for tooling hints) but cannot replace the runtime convention.

### Alternative F: Optional/Maybe Type (Separate from Result)

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
- **`unwrap` loses error context** — `Exception: unwrap called on Result error: #file_not_found` is less informative than `IOError: File not found at /path/to/file` with details and hint. The original structured error is compressed into a generic signal. Implementation should mitigate this by re-raising the original Exception when the error reason is an Exception object
- **REPL confusion for Result errors** — `_error` is not set when a Result error is returned (it's a successful evaluation). Users expecting `_error` will find `nil`. Documentation must address this
- **Gradual typing gap** — `-> Result` doesn't express what types the ok value and error carry. Future parameterized types (`-> Result(String, IOError)`) will be needed for full type safety; the current design must be forward-compatible with this

### Neutral

- **Exception infrastructure unchanged** — `on:do:`, `ensure:`, `_error`, error class hierarchy all remain as-is (ADR 0015)
- **Supervision unchanged** — Actor crashes are still exceptions; OTP supervisors handle them
- **REPL error display unchanged** — Exceptions still display as `RuntimeError: ...`. Results display as `Result error: ...`
- **Existing exception-based code continues to work** — `on:do:` still catches exceptions. No forced migration
- **`tryDo:` bridges the gap** — Legacy code that raises exceptions can be wrapped in Result without refactoring

## Implementation

### Phase 1: Result Class + FFI Migration (M)

Ship Result and migrate all FFI modules in one step. Existing callers (including Symphony) are already in poor shape with ad-hoc error conventions — deferring the FFI migration only adds more callers to the future migration burden and allows `tryDo:` to become an entrenched substitute convention.

**Components:** stdlib (Beamtalk), runtime (Erlang), stdlib tests, e2e tests

1. **`stdlib/src/Result.bt`** — Value class with `state:` declarations, `ifOk:ifError:`, `map:`, `andThen:`, `valueOr:`, `valueOrDo:`, `unwrap`, `mapError:`, `printString`. Boot order: ensure Result loads before File, Yaml, etc. in `build_stdlib.rs`
2. **`runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl`** — `from_tagged_tuple/1` helper, `class_tryDo:/3` for `tryDo:` class method
3. **`beamtalk_file.erl`** — Convert all 12 fallible methods to return Result via `from_tagged_tuple/1`. Each replaces a 5-line error builder chain per error case with 2-3 lines
4. **`beamtalk_regex.erl`** — `Regex from:` returns Result (invalid pattern is expected)
5. **`beamtalk_http.erl`** — Network errors return Result; HTTP status codes remain on HTTPResponse
6. **`beamtalk_subprocess.erl`** / **`beamtalk_reactive_subprocess.erl`** — Startup failures return Result
7. **Yaml/JSON parsing** — Parse errors return Result
8. **`stdlib/test/result_test.bt`** — BUnit tests for all Result methods
9. **Update stdlib tests** — file, regex, HTTP, subprocess tests for Result-returning methods
10. **Update e2e tests** — `tests/e2e/cases/` for file-related cases

### Phase 2: Language Features Documentation (S)

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

### Rollout

**Phase 1** ships Result and migrates all FFI modules simultaneously. Existing callers (Symphony and stdlib tests) need updating, but `unwrap` provides a mechanical escape hatch for any call site that wants to preserve exception-raising behavior. New FFI modules must return Result by default from day one.

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
