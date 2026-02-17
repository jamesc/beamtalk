<!-- Copyright 2026 James Casey SPDX-License-Identifier: Apache-2.0 -->

# ADR 0028: BEAM Interop Strategy

## Status
Proposed (2026-02-17)

## Context

Beamtalk's Principle 9 states: "Beamtalk is a **first-class BEAM citizen**, not an isolated language." The BEAM ecosystem has decades of battle-tested libraries — OTP, Phoenix, Ecto, Nx — and Beamtalk must use them seamlessly.

Today, Beamtalk has strong **outbound** interop: actors compile to standard `gen_server` modules, values cross boundaries unwrapped, and any Erlang/Elixir code can call Beamtalk actors via `gen_server:call/2`. However, there is **no way for Beamtalk code to call arbitrary Erlang modules**. The only FFI mechanism is `@primitive`/`@intrinsic`, which is restricted to stdlib authors.

This ADR addresses three questions:
1. **How does a Beamtalk user call Erlang functions?** (e.g., `lists:reverse/1`, `maps:merge/2`)
2. **How do BEAM types map at boundaries?** (documenting what exists and filling gaps)
3. **How does Beamtalk supervise foreign Erlang gen_servers?**

The FFI philosophy is also recorded: transparent interop, not wrapper-based.

### Current State

**What works today:**
- Beamtalk actors are standard gen_servers — callable from any BEAM language
- Values cross boundaries unwrapped (integers, strings, maps, lists, tuples are native BEAM terms)
- `class_of/1` classifies all BEAM types including foreign ones (Pid, Port, Reference → named classes)
- `#beamtalk_object{}` records are internal — they don't leak to Erlang callers

**What's missing:**
- No syntax for calling Erlang modules from Beamtalk code
- Pid, Port, Reference have class names but zero methods (immediate `does_not_understand`)
- No way to supervise external Erlang gen_servers from a Beamtalk supervision tree

### Constraints

1. **Messages all the way down** — Beamtalk's design principle requires interop to use message-send syntax, not special forms
2. **Interactive-first** — Erlang calls must work in the REPL with no boilerplate
3. **No new syntax** — prefer solving with objects and messages over parser changes
4. **Type safety where possible** — gradual typing (ADR 0025) should apply to foreign calls in the future
5. **Erlang-first** — Elixir and Gleam interop are future work; this ADR focuses on Erlang modules

## Decision

### 1. The `Erlang` Global Object — Module Proxy Pattern

Introduce `Erlang` as a **global singleton object** (ADR 0010) that responds to module names as unary messages, returning a **module proxy** that dispatches keyword/unary/binary messages as Erlang function calls.

```beamtalk
// Call lists:reverse/1
Erlang lists reverse: #(3, 2, 1)
// => #(1, 2, 3)

// Call maps:merge/2
Erlang maps merge: baseMap with: overrides
// => #{...merged...}

// Call erlang:system_time/1
Erlang erlang system_time: #microsecond
// => 1739802983611000

// Call string:uppercase/1
Erlang string uppercase: "hello"
// => "HELLO"
```

**How it works:**

1. `Erlang` is a **value-type global** — a tagged map injected as a workspace binding (extending ADR 0010's pattern beyond actor singletons). Unlike `Transcript` and `Beamtalk` which are actor singletons, `Erlang` is a stateless value type — no gen_server process, no serialization bottleneck. Note: ADR 0010 rejected value-type globals for `Transcript` (Alternative C) because it needs shared identity for I/O coordination; the `Erlang` global is different — it's stateless and merely acts as a namespace proxy, making the value-type approach appropriate here.
2. Sending a unary message like `lists` returns an **ErlangModule proxy** for the atom `lists`
3. The proxy translates Beamtalk message sends to Erlang function calls:
   - Unary message `reverse:` with one arg → `lists:reverse(Arg)`
   - Keyword message `merge:with:` → `maps:merge(Arg1, Arg2)` (keywords stripped, args positional)
4. Return values are native BEAM terms — `class_of/1` handles them automatically

**Important:** The `Erlang` global and `ErlangModule` proxies are **not gen_server actors**. They are **value type objects** (tagged maps, per ADR 0005) dispatched through `beamtalk_primitive:send/3`. This avoids the serialization bottleneck of routing every Erlang call through a single process. Proxy creation is a simple map allocation; `erlang:apply/3` calls happen in the caller's process.

**Hot code reload:** Because proxies call `erlang:apply/3` dynamically (not bound to a specific module version), they automatically pick up reloaded Erlang modules — no proxy invalidation needed.

**REPL session:**

```beamtalk
> Erlang lists reverse: #(1, 2, 3)
#(3, 2, 1)

> Erlang lists seq: 1 to: 10
#(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

> proxy := Erlang maps
#ErlangModule<maps>

> proxy merge: #{#a => 1} with: #{#b => 2}
#{a => 1, b => 2}

> proxy class
ErlangModule
```

**Selector-to-function mapping:**

Beamtalk keyword messages map to Erlang functions by using the **first keyword as the function name** and passing subsequent arguments positionally:

| Beamtalk message send | Erlang call | Rule |
|---|---|---|
| `Erlang lists reverse: xs` | `lists:reverse(Xs)` | Unary: keyword = function name |
| `Erlang lists seq: 1 to: 10` | `lists:seq(1, 10)` | Multi-keyword: first keyword = function, rest = extra args |
| `Erlang erlang now` | `erlang:now()` | Unary (no args): message name = function name |
| `Erlang math pow: 2 exp: 10` | `math:pow(2, 10)` | First keyword = function, additional args positional |

**Escaping for unusual function names:**

Some Erlang functions have names that conflict with Beamtalk syntax or are not valid identifiers. Use symbol literals for these:

```beamtalk
// Erlang function with underscores (works naturally)
Erlang erlang system_info: #schedulers

// If needed, quoted selectors for edge cases
Erlang io format: "~p~n" args: #(value)
```

**Error handling — what happens on misuse:**

```beamtalk
> Erlang lists nonexistent: 42
ERROR: #RuntimeError
  Module: lists
  Function: nonexistent/1
  Hint: This Erlang function does not exist. Check spelling and arity.

> Erlang bogus_module reverse: #(1, 2, 3)
ERROR: #RuntimeError
  Module: bogus_module
  Function: reverse/1
  Hint: Erlang module 'bogus_module' is not loaded. Is it on the code path?

> Erlang lists reverse: 1 extra: 2
ERROR: #RuntimeError
  Module: lists
  Function: reverse/2
  Hint: lists:reverse/1 exists but was called with 2 arguments.
```

Errors from Erlang calls are wrapped in structured Beamtalk exceptions, not raw Erlang errors. The proxy catches `error:undef` and translates it to actionable `#UndefinedFunctionError` with module, function, and hint.

**Exception mapping:**

Erlang functions can fail via `error`, `exit`, or `throw`. Erlang `error` exceptions map to existing Beamtalk exception classes (they're conceptually the same errors). Erlang `exit` and `throw` are foreign concepts that map to a new `BEAMError` subclass hierarchy:

| Erlang exception | Beamtalk exception | Rationale |
|---|---|---|
| `error:undef` | `RuntimeError` (kind: `does_not_understand`) | Same concept as Beamtalk's "method not found" |
| `error:badarg` | `TypeError` | Same concept as Beamtalk's type mismatch |
| `error:function_clause` | `RuntimeError` (kind: `arity_mismatch`) | Pattern match / arity failure |
| `error:badarith` | `TypeError` | Arithmetic type error |
| `exit:Reason` | `ExitError` (subclass of `BEAMError`) | Foreign concept — BEAM process exit signal |
| `throw:Value` | `ThrowError` (subclass of `BEAMError`) | Foreign concept — BEAM non-local return |

**New exception hierarchy addition:**

```
Error
└── BEAMError              ← base class for foreign BEAM-specific exceptions
    ├── ExitError           ← exit:Reason
    └── ThrowError          ← throw:Value
```

`BEAMError` exists because `exit` and `throw` have no Beamtalk equivalent — they are BEAM-specific control flow mechanisms. Standard Erlang `error` exceptions reuse existing Beamtalk classes since they represent the same concepts (type errors, missing functions, etc.).

All exceptions include the original Erlang error in `details` for debugging:
```beamtalk
> Erlang lists nth: 0 from: #(1, 2)
ERROR: #RuntimeError
  Module: lists
  Function: nth/2
  Reason: function_clause
  Hint: Erlang function raised 'function_clause'. Check argument types and values.
```

**Note:** `BEAMError`, `ExitError`, and `ThrowError` are new classes extending ADR 0015's exception hierarchy. Implementation requires adding these classes to `lib/` and updating `beamtalk_exception_handler.erl`. Existing classes (`RuntimeError`, `TypeError`) are reused for Erlang `error:*` exceptions — no changes needed.

**Proxy selector collision — reserved names:**

The `ErlangModule` proxy is a value type with minimal methods. To avoid collision with Erlang function names, the proxy reserves **only** `class` and `printString` as its own methods. All other messages — including `asString`, `new`, `spawn`, `size` — are forwarded to `erlang:apply/3` as Erlang function calls.

If a user needs to call an Erlang function named `class` or `printString`, they use the explicit `call:args:` escape hatch:
```beamtalk
proxy := Erlang someModule
proxy class                           // => ErlangModule (proxy's own method)
proxy call: #class args: #()          // => calls someModule:class()
proxy call: #printString args: #()    // => calls someModule:printString()
```

**Arity variants:**

Some Erlang functions have multiple arities (e.g., `lists:seq/2` and `lists:seq/3`). Arity is determined by the number of arguments in the message send — keyword count maps directly:

```beamtalk
Erlang lists seq: 1 to: 10              // => lists:seq(1, 10) — arity 2
Erlang lists seq: 1 to: 10 step: 2      // => lists:seq(1, 10, 2) — arity 3
```

### 2. FFI Philosophy: Transparent, Not Wrapper-Based

Beamtalk's interop is **transparent** — values are not wrapped, marshalled, or converted at boundaries.

**Principle:** A Beamtalk integer IS an Erlang integer. A Beamtalk map IS an Erlang map. There is no serialization layer, no foreign value wrapper, no overhead.

| Beamtalk type | BEAM representation | Erlang sees... |
|---|---|---|
| Integer | `integer()` | An integer |
| Float | `float()` | A float |
| String | `binary()` | A binary |
| Symbol | `atom()` | An atom |
| Boolean | `true` \| `false` | A boolean |
| List | `list()` | A list |
| Dictionary | `map()` | A map |
| Tuple | `tuple()` | A tuple |
| Block | `fun()` | A fun |
| Actor instance | `pid()` | A pid (via `gen_server`) |
| `nil` | `nil` atom | The atom `nil` |

**Consequences of transparent interop:**
- ✅ Zero overhead — no marshalling cost at boundaries
- ✅ Erlang tools (Observer, recon, crash dumps) see native values
- ✅ Pattern matching works identically across languages
- ⚠️ Erlang atoms like `ok`, `error`, `undefined` enter Beamtalk as Symbols — no automatic wrapping
- ⚠️ Erlang charlists (`[104, 101, 108, 108, 111]`) are Lists, not Strings — programmer must convert

### 3. Type Mapping at Boundaries

`class_of/1` already handles all BEAM types. This ADR documents the full mapping and adds basic methods to currently-opaque types:

**Currently working (no changes):**

| BEAM type | Beamtalk class | Methods |
|---|---|---|
| `integer()` | `Integer` | Full arithmetic, comparison, iteration |
| `float()` | `Float` | Full arithmetic, comparison |
| `binary()` | `String` | Full string operations |
| `atom()` | `Symbol` | `asString`, comparison |
| `true`/`false` | `True`/`False` | Boolean logic, control flow |
| `list()` | `List` | Full collection protocol |
| `map()` | `Dictionary` (or tagged class) | Full collection protocol |
| `tuple()` | `Tuple` | `at:`, `size`, `isOk`, `isError`, `unwrap` |
| `fun()` | `Block` | `value`, `value:`, control flow |
| `#beamtalk_object{}` | User class | Full method dispatch |

**Gaps to fill — add basic Object protocol to opaque types:**

| BEAM type | Beamtalk class | Current methods | Add |
|---|---|---|---|
| `pid()` | `Pid` | None | `class`, `printString`, `asString`, `isAlive`, `==` |
| `port()` | `Port` | None | `class`, `printString`, `asString`, `==` |
| `reference()` | `Reference` | None | `class`, `printString`, `asString`, `==` |

These types are **interop artifacts** — they appear when calling Erlang code that returns pids, ports, or references. Providing basic Object protocol methods (especially `printString`) prevents confusing `does_not_understand` errors when users inspect foreign return values.

### 4. Tuple is for Interop

Tuples are **Erlang interop artifacts**, not general-purpose data structures. Users should not construct tuples directly — they receive them from Erlang calls and work with them via the Tuple protocol.

```beamtalk
// Tuples arrive from Erlang
result := Erlang file read_file: "config.json"
// => {ok, <<"...">>}

// Work with tuples via the Tuple protocol
result isOk
  ifTrue: [result unwrap]
  ifFalse: [Transcript show: "Failed to read config.json"]

// Tuple protocol is for inspection and unwrapping
result isOk      // => true
result unwrap     // => <<"...">>
```

Future language versions may add tuple pattern matching in `match:` blocks for more ergonomic destructuring (the parser already supports `Pattern::Tuple`, but end-to-end support is not yet tested — see ADR 0012 future work).

### 5. Supervising Foreign Erlang Gen_Servers (Future Work)

*This section depends on a future ADR defining OTP supervision concepts (Supervisor class, child specs, restart strategies) in Beamtalk. The interop pattern below shows how foreign gen_servers will integrate once that ADR is accepted.*

Beamtalk supervision trees can include foreign Erlang gen_servers via an explicit child spec:

```beamtalk
Supervisor subclass: MyApp
  children: [
    Counter spawn,
    Erlang childSpec: #{
      #id => #pg_pool,
      #start => {#pgpool, #start_link, #(connectionArgs)},
      #restart => #permanent
    }
  ]
  strategy: #oneForOne
```

The `Erlang childSpec:` factory creates a supervision child spec from a map matching OTP's `child_spec()` type. This allows mixing Beamtalk actors and Erlang gen_servers under a single supervision tree.

**Implementation:** The Beamtalk supervisor translates child specs to standard OTP `#{id => ..., start => {M, F, A}, ...}` maps and passes them to `supervisor:start_child/2`. No wrapping or adaptation needed — OTP supervisors already handle arbitrary child specs.

## Prior Art

### Smalltalk — No FFI Needed
Traditional Smalltalk has no FFI because everything lives in the image. Newspeak introduced "Alien" objects for C interop — low-level, unsafe, platform-specific. Beamtalk's situation is fundamentally different: BEAM languages share a runtime, so interop is safe and zero-cost.

### Gleam — `@external` Pragma
```gleam
@external(erlang, "lists", "reverse")
pub fn reverse_list(list: List(a)) -> List(a)
```
Type-safe, explicit, but requires per-function declarations. Good for libraries, heavy for exploration. Beamtalk rejects this approach for user code because it conflicts with interactive-first design — you shouldn't need to declare a function before calling it in the REPL.

### LFE — Direct Syntax
```lisp
(: lists reverse '(1 2 3))
```
Zero boilerplate, but LFE adds special `:` syntax for module calls. Beamtalk achieves the same ergonomics using message sends to the `Erlang` global object — no new syntax needed.

### Elixir — Erlang Atom Calls
```elixir
:lists.reverse([1, 2, 3])
```
Minimal syntax (`:atom.function`). Elixir can do this because its syntax already has the `.` operator for function calls. Beamtalk doesn't have `.` for dispatch — we use message sends — so the `Erlang` global object provides equivalent ergonomics.

### Ruby — FFI via Objects
```ruby
# Ruby's Fiddle FFI wraps C libraries as objects
lib = Fiddle.dlopen('/usr/lib/libm.so')
```
Ruby's FFI follows a similar proxy pattern: open a library (returns an object), call methods on it. Beamtalk's `Erlang` global is conceptually the same but zero-cost since BEAM modules are already loaded. The difference: Ruby FFI requires explicit type declarations; Beamtalk's transparent interop makes them unnecessary.

## User Impact

### Newcomer
The `Erlang` object is discoverable — type `Erlang` in the REPL and explore. Module proxies respond to `methods` for introspection. No configuration or declarations needed.
⚠️ **Downside:** Newcomers may not know Erlang module/function names. Mitigation: REPL help system (`:help Erlang`) should list common modules and suggest stdlib alternatives where they exist.

### Smalltalk Developer
Message-send syntax feels natural. `Erlang lists reverse: xs` reads like any other message cascade. The `Erlang` global is analogous to Newspeak's platform object injection — dependencies arrive via the environment, not imports.
⚠️ **Downside:** Keyword-to-positional mapping is unfamiliar — Smalltalk keyword messages carry semantic names, but Erlang calls make the non-first keywords arbitrary. Mitigation: documentation and examples make the convention clear.

### Erlang/BEAM Developer
`Erlang lists reverse: #(1, 2, 3)` maps directly to `lists:reverse([1,2,3])`. The mental model is: first keyword = function name, remaining args are positional. Supervision interop uses standard OTP child specs — no new concepts.
⚠️ **Downside:** The proxy indirection may feel unnecessary when they're used to `:module.function(args)`. Mitigation: compiler optimization (Phase 4) makes the overhead zero at runtime.

### Operator
Transparent interop means Observer, recon, and crash dumps show native BEAM values. Beamtalk actors appear as normal gen_server processes. No hidden wrapping layers to debug.
⚠️ **Downside:** Erlang exceptions are translated to Beamtalk exceptions — operators debugging crash logs need to understand the mapping. Mitigation: the original Erlang error is preserved in the exception's `details` field.

## Steelman Analysis

### Best Argument for Gleam-Style `@external` Pragma
**From the type safety advocate:** "The `Erlang` proxy approach is dynamically typed — you can call `Erlang lists nonexistent: 42` and get a runtime error. With `@external`, the compiler knows the function signature, can check arity, and IDE tooling can autocomplete. For production code, declaration-then-use is worth the boilerplate."

**Response:** Agreed for library authors. When gradual typing (ADR 0025) lands, we can add optional type annotations to `ErlangModule` proxies via protocol declarations. But the dynamic path must exist for REPL exploration and rapid prototyping. Both can coexist.

### Best Argument for Direct Namespace Syntax (`Erlang:lists`)
**From the Erlang developer:** "Adding a `:` after `Erlang` makes it visually obvious this is a foreign call, not a message send. It's a single dispatch (not two-step proxy), so it's faster and the compiler can optimize it directly."

**Response:** The two-step dispatch (global → proxy → call) is optimizable: the compiler can detect `Erlang <module>` patterns and emit direct `call 'module':'function'(args)` Core Erlang. The visual difference is one character — not worth adding new syntax for.

### Best Argument for Keeping Current State (Do Nothing)
**From the minimalist:** "The stdlib already wraps every Erlang function users need via `@primitive`. Adding direct Erlang calls creates two ways to do everything — `#(1, 2, 3) reverse` via stdlib or `Erlang lists reverse: #(1, 2, 3)` via proxy. Users will be confused about which to prefer, and the stdlib wrappers become dead weight."

**Response:** The stdlib provides idiomatic Beamtalk interfaces and should remain the *preferred* path. `Erlang` is an escape hatch for accessing the long tail of BEAM libraries that the stdlib will never wrap — file systems, networking, crypto, third-party packages. Discoverability guides users toward stdlib first (`#(1, 2, 3) reverse` autocompletes; `Erlang lists` doesn't yet).

### Tension Points Between Cohorts
- **Smalltalk purist vs BEAM veteran:** Smalltalk developers want everything to be a message send (proxy pattern is natural). BEAM veterans want `module:function(args)` and find the proxy indirection unnecessary. The decision favors the Smalltalk cohort.
- **Library author vs REPL explorer:** Library authors want type safety and compile-time checking. REPL explorers want zero-boilerplate dynamic calls. The decision favors REPL exploration, with a path to type safety via ADR 0025.

## Alternatives Considered

### Alternative A: Do Nothing — Stdlib Wraps Everything
Keep the current model: all Erlang interaction happens through `@primitive`/`@intrinsic` in stdlib classes. Users never call Erlang directly.

**Rejected.** The BEAM ecosystem has thousands of modules. The stdlib cannot wrap them all. Users who need `crypto:hash/2`, `ets:lookup/2`, or a third-party Hex package would have no path forward. This also prevents gradual migration — teams with existing Erlang code couldn't call it from Beamtalk.

### Alternative B: Pragma-Based FFI (Gleam-Style)
```beamtalk
@external(erlang, "lists", "reverse")
reverse: aList => @primitive 'reverse'
```
**Rejected.** Conflicts with interactive-first design. Users shouldn't need to declare a wrapper function before calling an Erlang function in the REPL. Also adds syntactic complexity — Beamtalk already has `@primitive` and `@intrinsic`, a third pragma form is too many.

### Alternative C: Namespace Prefix Syntax
```beamtalk
Erlang:lists reverse: #(1, 2, 3)
```
**Rejected.** Requires parser changes to handle `Name:name` as a single token. The message-send approach achieves the same ergonomics with zero syntax changes. If we later want a shorthand, it can be added as syntactic sugar over the proxy pattern.

### Alternative D: String-Based Module Names
```beamtalk
Erlang call: "lists" function: "reverse" args: #(#(1, 2, 3))
```
**Rejected.** Too verbose, not discoverable, loses the elegance of message-send syntax. Module names as strings prevent compile-time checking.

## Consequences

### Positive
- **Zero new syntax** — `Erlang` is just a global object, module proxies are just objects, everything is message sends
- **REPL-friendly** — call any Erlang function immediately with no setup
- **Discoverable** — `Erlang` and its proxies respond to introspection messages
- **Extensible** — `Elixir` and `Gleam` globals can follow the same pattern later
- **Optimizable** — compiler can detect `Erlang <mod> <fn>:` patterns and emit direct BEAM calls
- **Supervision works** — standard OTP child specs, no adaptation layer

### Negative
- **No compile-time arity checking** — calling `Erlang lists reverse: 1 extra: 2` fails at runtime, not compile time
- **Two-step dispatch overhead** — unoptimized path has extra allocation for module proxy (mitigated by compiler optimization)
- **Keyword-to-positional mapping** is a convention, not enforced — users must know Erlang function arities
- **Selector naming ambiguity** — `Erlang lists seq: 1 to: 10` works, but the keyword names are arbitrary (they don't match Erlang's parameter names)
- **Reserved selector collision** — `class` and `printString` on proxies shadow same-named Erlang functions; `call:args:` escape hatch required for edge cases

### Neutral
- Type mapping is documented as-is (already implemented, no changes to class_of/1)
- Pid/Port/Reference get basic methods (small runtime addition)
- Tuple framed as interop-only (documentation change, no code change)

## Implementation

### Phase 0: Wire Check — Single Erlang Call Round-Trip
- Hardcode `Erlang` as a workspace binding pointing to a runtime module
- Implement a single `doesNotUnderstand:` handler that returns an ErlangModule proxy map
- Proxy handles one keyword message via `erlang:apply/3`
- Verify in REPL: `Erlang lists reverse: #(1, 2, 3)` returns `#(3, 2, 1)`
- **Components:** `beamtalk_erlang_proxy.erl` (new), workspace binding injection
- **Tests:** One stdlib test, one REPL E2E test

### Phase 1: Erlang Global Object and Module Proxies
- Add `Erlang` as a global singleton (ADR 0010 pattern)
- Implement `ErlangModule` proxy class that dispatches to `erlang:apply/3`
- Wire up `class_of/1` for proxy instances
- **Components:** `lib/Erlang.bt`, `lib/ErlangModule.bt`, runtime dispatch in `beamtalk_primitive.erl`

### Phase 2: Pid/Port/Reference Methods
- Add basic Object protocol to opaque BEAM types
- `printString`, `asString`, `class`, `==` at minimum
- `isAlive` for Pid
- **Components:** `beamtalk_primitive.erl` (new dispatch clauses), new `beamtalk_pid_ops.erl`

### Phase 3: Supervision Interop
- Add `Erlang childSpec:` factory for foreign child specs
- Integrate with Beamtalk Supervisor class (when implemented)
- **Components:** Supervisor implementation (depends on supervision language features)

### Phase 4: Compiler Optimization (Optional)
- Detect `Erlang <mod> <fn>:` patterns at compile time
- Emit direct `call 'module':'function'(args)` without proxy allocation
- **Components:** `crates/beamtalk-core/src/codegen/`

### Future Work
- **`Elixir` global** — same proxy pattern, maps `Elixir enum` → `'Elixir.Enum'`
- **`Gleam` global** — maps `Gleam list` → `gleam@list`
- **Type annotations** — optional protocol declarations for Erlang module proxies (leveraging ADR 0025)
- **Hex.pm integration** — separate ADR (ADR 0026 scope)

## Migration Path

No existing user code is affected. This ADR introduces new capabilities without changing existing behavior:

- `@primitive` and `@intrinsic` continue to work unchanged for stdlib authors
- Existing `class_of/1` behavior is preserved; Pid/Port/Reference gain methods but retain their class names
- `Tuple` API remains unchanged; only documentation and framing changes (interop-only positioning)
- The `Erlang` global is additive — no existing bindings are modified

## References
- Related issues: BT-302
- Related ADRs: ADR 0005 (object model), ADR 0006 (method dispatch), ADR 0010 (global objects — extended here to support value-type globals), ADR 0015 (exception hierarchy — extended here with interop exceptions), ADR 0016 (module naming), ADR 0025 (gradual typing), ADR 0026 (package manifest)
- Design principles: §9 Seamless BEAM Ecosystem Integration
