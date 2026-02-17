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

Introduce `Erlang` as a **ProtoObject subclass** that responds to module names as class-side messages, returning a **module proxy** that dispatches keyword/unary/binary messages as Erlang function calls.

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

1. `Erlang` is a **class** (ProtoObject subclass) in `lib/Erlang.bt` — always loaded as part of stdlib. Since `Erlang` is an uppercase identifier, the parser treats it as a `ClassReference` — it works everywhere: REPL, compiled code, and inside actor methods. No workspace binding is needed; `Erlang lists` is a class-side message send, just like `Counter spawn` or `Integer methods`.
2. `Erlang`'s class-side `doesNotUnderstand:args:` is an **`@intrinsic`** — the compiler recognizes it and emits inline proxy construction (or direct `erlang:apply/3`) rather than routing through the class process gen_server. This follows the same pattern as `@intrinsic blockValue` on Block.
3. Sending a unary message like `lists` returns an **ErlangModule proxy** for the atom `lists`
4. The proxy translates Beamtalk message sends to Erlang function calls:
   - Unary message `reverse:` with one arg → `lists:reverse(Arg)`
   - Keyword message `merge:with:` → `maps:merge(Arg1, Arg2)` (keywords stripped, args positional)
5. Return values are native BEAM terms — `class_of/1` handles them automatically

**Important:** Because the `@intrinsic` generates code inline in the caller's process, there is no gen_server bottleneck — no serialization, no single-process routing. The `ErlangModule` proxy is a simple tagged map (value type, per ADR 0005), and the `erlang:apply/3` call runs in the caller.

**Hot code reload:** Because proxies call `erlang:apply/3` dynamically (not bound to a specific module version), they automatically pick up reloaded Erlang modules — no proxy invalidation needed.

**Name conflicts:** `Erlang` is a reserved class name, like `Integer` or `Object`. A user-defined class named `Erlang` would shadow the built-in — the same way redefining `Integer` would. Future module/namespace support will provide proper scoping.

**REPL session:**

```beamtalk
> Erlang lists reverse: #(1, 2, 3)
#(3, 2, 1)

> Erlang lists seq: 1 with: 10
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
| `Erlang lists seq: 1 with: 10` | `lists:seq(1, 10)` | Multi-keyword: first keyword = function, rest = extra args |
| `Erlang erlang system_time: #microsecond` | `erlang:system_time(microsecond)` | Keyword: function name + arg |
| `Erlang math pow: 2 with: 10` | `math:pow(2, 10)` | First keyword = function, additional args positional |

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

**Note:** `BEAMError`, `ExitError`, and `ThrowError` are new classes extending ADR 0015's exception hierarchy. Implementation requires adding these classes to `lib/` and updating `beamtalk_exception_handler.erl`. The proxy's catch clause must use Erlang's three-class catch (`catch Class:Reason:Stack`) and preserve the `Class` (`error`/`exit`/`throw`) to map correctly — the current `wrap/1` only handles `error` class exceptions. Existing classes (`RuntimeError`, `TypeError`) are reused for Erlang `error:*` exceptions — no changes needed.

**Proxy selector collision — minimal surface via ProtoObject:**

`ErlangModule` inherits from **ProtoObject**, not Object. ProtoObject's protocol is minimal: `class`, `==`, `/=`, `doesNotUnderstand:args:` (ADR 0005, Q1). This means the proxy reserves only **three selectors** (`class`, `==`, `/=`) — everything else, including `printString`, `asString`, `new`, `spawn`, `size`, is forwarded to `erlang:apply/3` via the `doesNotUnderstand:args:` handler.

If a user needs to call an Erlang function named `class`, they use the explicit `call:args:` escape hatch:
```beamtalk
proxy := Erlang someModule
proxy class                           // => ErlangModule (ProtoObject method)
proxy call: #class args: #()          // => calls someModule:class()
```

Similarly, `Erlang` itself inherits from ProtoObject — it responds to module names via `doesNotUnderstand:args:`, returning `ErlangModule` proxies.

**Arity variants and zero-argument functions:**

Some Erlang functions have multiple arities (e.g., `lists:seq/2` and `lists:seq/3`). Arity is determined by the number of arguments in the message send — keyword count maps directly:

```beamtalk
Erlang lists seq: 1 with: 10              // => lists:seq(1, 10) — arity 2
Erlang lists seq: 1 with: 10 with: 2     // => lists:seq(1, 10, 2) — arity 3
```

Zero-argument Erlang functions are called as unary messages on the proxy. Since `ErlangModule` inherits from ProtoObject, unary messages like `self` or `module_info` hit `doesNotUnderstand:args:` and forward to `erlang:apply(Module, FunctionName, [])`:

```beamtalk
Erlang erlang node                         // => erlang:node() — arity 0
(Erlang lists) module_info                 // => lists:module_info() — arity 0
```

**Note:** Beamtalk keywords like `self` and `super` are handled by the parser before message dispatch, so `Erlang erlang self` would not call `erlang:self/0`. Use `Erlang erlang call: #self args: #()` for Erlang functions that share names with Beamtalk keywords.

### 2. FFI Philosophy: Transparent, Not Wrapper-Based

Beamtalk's interop is **transparent** — values are not wrapped, marshalled, or converted at boundaries.

**Principle:** A Beamtalk integer IS an Erlang integer. A Beamtalk map IS an Erlang map. There is no serialization layer, no foreign value wrapper, no overhead.

| Beamtalk type | BEAM representation | Erlang sees... |
|---|---|---|
| Integer | `integer()` | An integer |
| Float | `float()` | A float |
| String | `binary()` | A binary |
| Symbol | `atom()` | An atom |
| True / False | `true` \| `false` | A boolean |
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
Zero boilerplate, but LFE adds special `:` syntax for module calls. Beamtalk achieves the same ergonomics using message sends to the `Erlang` class — no new syntax needed.

### Elixir — Erlang Atom Calls
```elixir
:lists.reverse([1, 2, 3])
```
Minimal syntax (`:atom.function`). Elixir can do this because its syntax already has the `.` operator for function calls. Beamtalk doesn't have `.` for dispatch — we use message sends — so the `Erlang` class provides equivalent ergonomics.

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
- **Zero new syntax** — `Erlang` is just a class, module proxies are just objects, everything is message sends
- **REPL-friendly** — call any Erlang function immediately with no setup
- **Discoverable** — `Erlang` and its proxies respond to introspection messages
- **Extensible** — `Elixir` and `Gleam` globals can follow the same pattern later
- **Optimizable** — compiler can detect `Erlang <mod> <fn>:` patterns and emit direct BEAM calls
- **Supervision works** — standard OTP child specs, no adaptation layer

### Negative
- **No compile-time arity checking** — calling `Erlang lists reverse: 1 extra: 2` fails at runtime, not compile time
- **Two-step dispatch overhead** — unoptimized path creates a proxy map then calls `erlang:apply/3`; Phase 4 collapses to a single direct call. The `@intrinsic` DNU already avoids the class process bottleneck
- **Keyword-to-positional mapping** is a convention, not enforced — users must know Erlang function arities
- **Selector naming ambiguity** — `Erlang lists seq: 1 to: 10` works, but the keyword names are arbitrary (they don't match Erlang's parameter names)
- **Reserved selector collision** — `class`, `==`, and `/=` on proxies shadow same-named Erlang functions (ProtoObject methods); `call:args:` escape hatch required for edge cases
- **Beamtalk keyword conflicts** — Erlang functions named `self`, `super`, or `true` cannot be called as unary messages because the parser handles these as keywords; use `call:args:` escape hatch

### Neutral
- Type mapping is documented as-is (already implemented, no changes to class_of/1)
- Pid/Port/Reference get basic methods (small runtime addition)
- Tuple framed as interop-only (documentation change, no code change)

## Implementation

### Phase 0: Wire Check — Single Erlang Call Round-Trip
- Add `lib/Erlang.bt` as a ProtoObject subclass with class-side `doesNotUnderstand:args:`
- Class-side handler returns an ErlangModule proxy map for the module name
- Proxy handles one keyword message via `erlang:apply/3`
- Verify in REPL: `Erlang lists reverse: #(1, 2, 3)` returns `#(3, 2, 1)`
- Works in actors too — `Erlang` is a stdlib class, always loaded
- **Components:** `lib/Erlang.bt`, `lib/ErlangModule.bt`, `beamtalk_erlang_proxy.erl` (new)
- **Tests:** One stdlib test, one REPL E2E test

### Phase 1: Full Erlang Class and Module Proxies
- Complete `Erlang` class with error handling and introspection
- Implement `ErlangModule` proxy class that dispatches to `erlang:apply/3`
- Wire up `class_of/1` for proxy instances
- **Export introspection:** Proxies use `module_info(exports)` to validate function existence and arity at dispatch time. This is always available (no `+debug_info` needed) and enables:
  - Runtime arity validation with actionable error messages
  - LSP completions: typing `Erlang lists` triggers `lists:module_info(exports)` to offer `reverse:`, `seq:with:`, `map:with:`, etc.
  - REPL discoverability: `(Erlang lists) methods` returns all exported functions
- **Components:** `lib/Erlang.bt`, `lib/ErlangModule.bt`, runtime dispatch in `beamtalk_primitive.erl`, LSP completion extension in `completion_provider.rs`

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
- Collapse `Erlang <mod> <fn>:` patterns to direct `call 'module':'function'(args)` — no proxy map allocation at all
- The `@intrinsic` DNU (Phase 0) already avoids the class process; this phase eliminates the intermediate proxy map too
- **Components:** `crates/beamtalk-core/src/codegen/`

### Future Work
- **`Elixir` global** — same proxy pattern, maps `Elixir enum` → `'Elixir.Enum'`
- **`Gleam` global** — maps `Gleam list` → `gleam@list`
- **Type annotations** — optional protocol declarations for Erlang module proxies (leveraging ADR 0025)
- **Erlang parameter name introspection** — with `+debug_info`, `beam_lib:chunks/2` can extract parameter names from the abstract code AST, enabling meaningful keyword names (e.g., `seq: 1 to: 10 step: 2` validated against actual parameter names) and richer IDE tooltips. Complements Phase 1's export-based arity introspection
- **Hex.pm integration** — separate ADR (ADR 0026 scope)

## Migration Path

No existing user code is affected. This ADR introduces new capabilities without changing existing behavior:

- `@primitive` and `@intrinsic` continue to work unchanged for stdlib authors
- Existing `class_of/1` behavior is preserved; Pid/Port/Reference gain methods but retain their class names
- `Tuple` API remains unchanged; only documentation and framing changes (interop-only positioning)
- The `Erlang` class is additive — a new stdlib class, no existing classes or bindings modified

## References
- Related issues: BT-302
- Related ADRs: ADR 0005 (object model), ADR 0006 (method dispatch), ADR 0010 (global objects — `Erlang` differs: it's a class, not a workspace-bound actor), ADR 0015 (exception hierarchy — extended here with interop exceptions), ADR 0016 (module naming), ADR 0025 (gradual typing), ADR 0026 (package manifest)
- Design principles: §9 Seamless BEAM Ecosystem Integration
