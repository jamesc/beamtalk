# ADR 0140: Spec-Directed String Conversion at the Erlang Boundary

## Status
Proposed (2026-10-28)

## Context

### The problem

Since BT-4173 (epic BT-4170), a Beamtalk `String` is the tagged term `{beamtalk_string, Bin}` and a bare BEAM binary is always a `Binary`. Class follows construction type, with no UTF-8 sniffing. Erlang code knows nothing about the tag, so every call into a stock Erlang module has to decide which Strings to convert to raw binaries.

Today the rule is **shallow**. `beamtalk_erlang_proxy:apply_with_coercion/6` unwraps **top-level** `String` arguments only. Strings nested inside a list, tuple or map argument stay tagged. If the callee raises `badarg`, the call is retried once with every String unwrapped and binaries converted to charlists (`maybe_retry_badarg/8`).

Mixing depths gives silently wrong answers whenever the callee *compares* a top-level argument with nested data:

```beamtalk
Erlang lists member: "a" with: #("a")                // => false   (raw <<"a">> vs tagged "a")
Erlang lists keyfind: "a" with: 1 with: #({"a", 1})  // => false
Erlang maps get: "k" with: #{"k" => 1}               // raises: key not found <<"k">>
```

The tag isn't the cause. The *mismatch* is: one side of the comparison was converted and the other wasn't. If both sides had the same representation, all three calls would answer correctly, and `maps:get` would stay O(log n).

BT-4223 (PR #4609) documented this in `docs/beamtalk-native-erlang.md` ("Strings at the FFI boundary") and told users to use `includes:`/`at:` instead. That is an acceptable stopgap, but it asks users to know the String representation to call an OTP function correctly. ADR 0139 rules that out: the FFI membrane is "TYPED + Beamtalk-shaped", and codegen owns conversion at the boundary in both directions.

### Current state

* **Call sites.** Codegen lowers every inline `Erlang mod fn:` send to `beamtalk_erlang_proxy:direct_call(Mod, Fn, Args, Mode)`, and every `native:` delegation to `native_call(Mod, Fn, [Self | Args], {Class, Sel}, Mode)`. `Mode` is `stock` or `beamtalk`, precomputed per module (BT-4153). Beamtalk-owned modules (`beamtalk_*`, `bt@*`) get `beamtalk` mode and receive arguments untouched; they own the representation.
* **Signatures.** ADR 0075 gives every `(module, function, arity)` a `FunctionSignature` in `NativeTypeRegistry`, with one `ParamType` per argument. Signatures come from `-spec`s extracted from `.beam` abstract code (`beamtalk_spec_reader`), or from hand-written `declare native:` stubs in `stubs/`, which take priority. **Codegen already receives the registry** (`CodegenOptions::native_type_registry`, `driver.rs`).
* **The spec→type mapping is lossy for this purpose.** `binary()` maps to `String | Binary`, `string()` to `String | List`, and `term()`/`any()` to `Dynamic`. But `iodata()`/`iolist()` also map to `Dynamic`, and a map's key and value types collapse to plain `Dictionary`. The Beamtalk type alone therefore can't say which positions of an argument the callee reads as text.
* **Results.** `coerce_ffi_result/3` returns any binary untouched, so a callee spec'd `-> binary()` returns a `Binary` at runtime while the checker types it `String | Binary`. Charlist results are converted by **content**: `coerce_charlist_result/1` turns any list that `io_lib:printable_unicode_list/1` accepts into a `String`. So `Erlang lists seq: 65 with: 70` answers `"ABCDEF"`, not a List of Integers, which is the same kind of content sniffing BT-4170 removed for binaries.

### Constraints

1. **Nothing fails silently** (the maintainer's requirement for BT-4243). Every case where the boundary can't know the right conversion must produce a diagnostic, at compile time where possible.
2. **No per-call cost when nothing needs converting.** `maps:get` on a large map must not become O(n).
3. **No hand-kept list that changes behaviour** (CLAUDE.md: no "keep in sync" tables without an enforcing mechanism). Conversion must follow from data the compiler already has: specs and stubs.
4. **Strings must round-trip.** A String container passed through `lists:reverse/sort`, `maps:merge` or ets must come back as Strings (`testNestedStringsRoundTripThroughStockFfi`).
5. **No sniffing.** A binary's class is never guessed from its bytes (BT-4170).

## Decision

**The callee's signature decides, per argument and per position, which values are converted. Codegen emits the conversion at the call site.**

### 1. A conversion shape per parameter

Alongside each `ParamType`, the signature carries a **conversion shape**, computed from the *Erlang* spec, not from the lossy Beamtalk type:

| Erlang spec (after resolving remote/user types) | Shape | Effect on the argument |
|---|---|---|
| `binary()`, `nonempty_binary()`, `unicode:unicode_binary()`, `unicode:chardata()`, `iodata()`, `iolist()`, `bitstring()` | `text` | Unwrap every String at that position and below to its raw binary. Inside iodata/chardata this covers nested lists |
| `string()`, `nonempty_string()`, `[char()]` | `charlist` | String → charlist (`unicode:characters_to_list/1`) |
| `[S]`, `nonempty_list(S)` | `{list, S}` | Apply S to each element |
| `{S1, …, Sn}` | `{tuple, [S1…Sn]}` | Apply each Si to the matching element |
| `#{K => V}` / `#{K := V}` | `{map, SK, SV}` | Apply SK to keys and SV to values |
| a union | the union's text-bearing member if it has one, else `opaque` | `atom() \| binary()` → `text`; `binary() \| string()` → `text` (no conversion needed beyond the unwrap) |
| `term()`, `any()`, a type variable, anything else | `opaque` | **Pass through untouched**, tags included |

A shape whose only leaf is `opaque` collapses to `opaque`, so most parameters cost nothing. `declare native:` stubs get shapes from their Beamtalk types: `String`/`Binary` → `text`, `List(String)` → `{list, text}`, `Dictionary(String, V)` → `{map, text, opaque}`, a type variable → `opaque`.

The shape is computed in the spec reader. It sees the raw Erlang type, so it doesn't widen the existing Beamtalk type mapping. It travels in the registry next to the Beamtalk type.

### 2. Codegen emits per-argument conversion

For a call whose signature is known, codegen wraps only the non-`opaque` arguments:

```erlang
%% Erlang string uppercase: s           -- spec: (unicode:chardata()) -> unicode:chardata()
call 'beamtalk_erlang_proxy':'direct_call'('string','uppercase',
    [call 'beamtalk_ffi_convert':'arg'('text', S)], 'stock')

%% Erlang lists member: x with: xs      -- spec: (term(), [term()]) -> boolean()
call 'beamtalk_erlang_proxy':'direct_call'('lists','member', [X, Xs], 'stock')   %% no conversion
```

`beamtalk_ffi_convert:arg/2` interprets a literal shape. It's one shared runtime function, so the Rust-generated shapes and the Erlang interpreter of them need a conformance fixture (CLAUDE.md, shared-leaf-module rule): a single table of `{spec, shape, input, expected}` cases, read by both the Rust test and the EUnit test. The proxy no longer unwraps for calls that have a known signature (a new `Mode` value, `typed`, tells it the arguments are already converted). The shallow unwrap and the `badarg` retry remain **only** for calls without a signature (section 4).

Results for the opening examples:

```beamtalk
Erlang lists member: "a" with: #("a")                // => true   (both tagged: term())
Erlang lists keyfind: "a" with: 1 with: #({"a", 1})  // => {"a", 1}
Erlang maps get: "k" with: #{"k" => 1}               // => 1      (O(log n), nothing walked)
Erlang lists reverse: #("a", "b")                    // => #("b", "a") — still Strings
Erlang string uppercase: "abc"                       // => "ABC"  (see section 3)
```

### 3. Results are converted by the return spec

The return type gets a shape too, read in the other direction:

* A `unicode:chardata()`, `unicode:unicode_binary()` or `string()` result is tagged `String` (charlists are converted first). If it isn't valid UTF-8, the boundary **raises** `#invalid_utf8` naming the MFA, so it never quietly returns a `Binary`. These types are text by declaration, so this follows the spec rather than guessing from content.
* A list result is converted by its spec, not by its content. `[integer()]`, `[char()]` inside a non-text position, or any other non-text list stays a List. Only a `string()` result becomes a `String`. For calls with a signature this **replaces** the `printable_unicode_list` heuristic of `coerce_charlist_result/1`. Calls without a signature keep it (section 4).
* A `binary()`/`iodata()` result stays a `Binary` (bytes, as today). Its **static** type becomes `Binary` instead of `String | Binary`, so the checker stops promising a String the runtime never returns.
* `opaque` positions are untouched, so tagged values handed in come back tagged.

### 4. No signature: today's behaviour, plus a compile-time warning

A call with no signature (no spec, no stub, a dynamic module or function, a module unknown at compile time) keeps today's runtime rule: shallow unwrap and `badarg` retry. Then at compile time:

```
warning[ffi-untyped-strings]: no signature for mylib:lookup/2; Strings nested in its
  arguments reach it tagged, so comparisons against them may silently fail
  --> src/app.bt:12:5
   |
12 |     Erlang mylib lookup: key with: table
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   = help: add a `declare native: mylib` stub declaring the parameter types
```

The warning fires when the call has no signature **and** some argument's static type can't rule out containing a String. A `Dynamic` argument counts as "may contain one". A call whose arguments are all statically String-free (Integers, Symbols, `List(Integer)`, …) gets no warning. Suppress with `@expect ffi_untyped_strings` or `ffi-untyped-strings = "off"` under `[diagnostics]`. The REPL shows the same warning.

### 5. Strings stored into Erlang-owned state (decision E1)

An `opaque` parameter that **stores** its argument, such as `ets:insert/2`, `persistent_term:put/2`, `erlang:send/2` or `gen_server:call/2`, also receives tagged Strings. Beamtalk code reading the data back is correct, because tagged compares equal to tagged. Erlang code reading the same data mostly fails loudly (`badarg`, `function_clause` on a tuple where it expected a binary). The one silent case is an **equality lookup from Erlang** with a binary key (`ets:lookup(T, <<"k">>)` misses).

No spec says "this argument is stored", so the boundary can't tell storing apart from comparing. The decision:

* **The contract is the spec.** Data shared with Erlang code is declared as text with a stub (`declare native: ets  insert: tab :: Symbol with: obj :: Tuple(String, Object) -> Boolean`), which converts it. This goes in the native-interop guide as the boundary's rule.
* **A compile-time lint for what's left: `ffi-stored-strings`.** At a call to a storage or messaging function, the type checker warns when an argument at an `opaque` position has a static type that may contain a String. A `Dynamic` type counts, the same rule as `ffi-untyped-strings`. The functions are `ets:insert/2`, `insert_new/2`, `dets:insert/2`, `persistent_term:put/2`, `erlang:send/2`, `erlang:term_to_binary/1,2`, `gen_server:call/2,3`, `cast/2` and `gen_statem:call/2,3`, `cast/2`:

  ```
  warning[ffi-stored-strings]: a String passed to ets:insert/2 is stored tagged; Erlang code
    reading it back by binary key won't find it
    --> src/cache.bt:8:5
     |
   8 |     Erlang ets insert: table with: {key, value}
     |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     = help: if Erlang code reads this data, declare the parameter as text in a
       `declare native: ets` stub; if only Beamtalk reads it, add `@expect ffi_stored_strings`
  ```

  The lint runs at compile time, so it behaves identically in the REPL, `beamtalk run`, `beamtalk test` and releases (CLAUDE.md: no image mechanics) and costs nothing at runtime. A stub that declares the position as text converts the argument and silences the lint, as does a stub that declares it `Object` (the explicit "only Beamtalk reads this"). It also silences with `@expect ffi_stored_strings`, or package-wide with `ffi-stored-strings = "off"` under `[diagnostics]`. The function list only decides **where to warn**: it never changes behaviour, and a missing entry means a missing warning, not a wrong answer. It lives in one table in the type checker, next to the other FFI lints.

### 6. `native:` delegation

`native_call/5` follows the same rule. A `native:` class whose backing module is Beamtalk-owned stays in `beamtalk` mode (untouched, it owns the representation). One backed by a stock module gets conversion by its signature, exactly like inline FFI.

## Prior Art

| System | Approach | What we take |
|---|---|---|
| **Pharo uFFI** | `self ffiCall: #(int strlen(String s))`: the declared C signature drives marshalling, so a Smalltalk `String` becomes a `char*` only where the signature says `String`. `void*`/`ExternalAddress` parameters pass through untouched | The direct ancestor of this decision: the foreign function's declared signature, not the value, decides the conversion |
| **Gleam** `@external` | The user declares the Erlang function's Gleam types. No conversion happens, because Gleam `String` *is* an Erlang binary | Declared signatures as the boundary contract. Gleam doesn't need conversion only because its representation coincides with Erlang's; ours doesn't since BT-4173 |
| **Elixir** | Strings are binaries; charlist interop is explicit (`String.to_charlist/1`) | The `charlist` shape automates exactly the conversion Elixir developers write by hand for `string()` APIs |
| **Python ctypes / PyO3** | Marshalling is driven by the declared signature (`argtypes`, `FromPyObject` per parameter type), never by inspecting the value | The core idea: **conversion follows the declared parameter type**. An undeclared function gets the conservative default plus a warning |
| **JNA / JNI** | Java `String` ↔ `char*` conversion follows the native signature; a `void*` parameter is opaque and passed through | `opaque` = `void*`: the boundary keeps its hands off |
| **Kotlin/JS, Scala.js** | Typed external declarations; `dynamic` / `js.Any` disables conversion and type checking at that position | The no-signature tier: allowed, explicitly unchecked, visibly marked (our warning) |
| **ADR 0076/0139 (Beamtalk)** | Codegen converts `{ok, _}`/`{error, _}` ↔ `Result` at the boundary | The same "codegen owns the boundary in both directions" rule, applied to Strings |

## User Impact

* **Newcomer:** `Erlang lists member: "a" with: #("a")` answers `true`, and the tagged representation never comes up. When a library has no specs, the warning says exactly what to add.
* **Smalltalk developer:** Strings stay Strings when they go out to Erlang and come back. The boundary behaves like a well-typed message send, not a byte bridge.
* **Erlang/BEAM developer:** `-spec` becomes the interop contract they already maintain. Generated Core shows each conversion explicitly (`beamtalk_ffi_convert:arg('text', S)`), so it's debuggable. The one thing to learn is that sharing stored Strings with Erlang code needs a stub, and the `ffi-stored-strings` lint points it out at compile time.
* **Production operator:** calls whose arguments are all `opaque` (most of them) get slightly *cheaper*: the top-level unwrap scan is gone. The error-wrapping `try` (ADR 0101) stays. Conversion costs only as much as the spec'd text positions. The storage lint is compile-time only, so it adds no runtime cost.
* **Tooling developer:** the shape lives in `NativeTypeRegistry`, so the LSP can show "converts to binary" in hover. The warning is an ordinary diagnostic with a span.

## Steelman Analysis

| Alternative | Best case for it | Why not |
|---|---|---|
| **Status quo: shallow unwrap plus docs (BT-4223)** | 🎩 Smalltalk purist: "Interop is the edge of the system. Keep the core simple; the guide already says use `includes:`/`at:`, which are the Smalltalk way anyway" | The wrong answers stay silent. A user who reaches for `lists:keyfind` (or any third-party library) gets `false` with no signal, which breaks constraint 1 |
| **Per-function deep-unwrap table** | ⚙️ BEAM veteran: "The five lookups people actually write just work, and nothing else changes" | Open-ended. `maps:get` becomes O(n). Every function missing from the table stays silently wrong, breaking constraints 1–3 |
| **Explicit `asErlangTerm`** | 🎨 Language designer: "Explicit beats magic; the user controls cost and depth" | 🧑‍💻 The newcomer has to know the String representation to call `lists:member`, which is the abstraction leaking. Forgetting it is still silent |
| **Pass everything tagged; on `badarg`, unwrap and retry** | 🏭 Operator: "No compile-time machinery, and it's cheap on success" | Any function that accepts any term without raising silently gets the tag (`erlang:size/1` answers 2, `term_to_binary`, `phash2`). It also retries side effects |
| **E2: warn when a String goes to an `opaque` parameter not reflected in the return type** | 🧑‍💻 Newcomer: "I'm told every time, so nothing is truly silent" | Fires on `lists:member` (`T` isn't in `boolean()`), the very call this ADR fixes. Too many false positives, so users would learn to ignore it |
| **Runtime storage warning, on in the REPL and tests, off in releases** | ⚙️ BEAM veteran: "It sees the actual values, so it catches Strings inside `Dynamic` data that no static check can" | Its behaviour differs between REPL/test and release, which CLAUDE.md's no-image-mechanics rule rules out, and it costs a per-call check wherever it's on. The compile-time `ffi-stored-strings` lint behaves the same everywhere at no runtime cost. It covers `Dynamic` by warning conservatively instead of inspecting values |
| **E3: convert `opaque` arguments for a list of storage/messaging MFAs** | 🏭 Operator: "My first ETS table shared with an Erlang library just works" | A hand-kept list that changes behaviour (constraint 3). Beamtalk reads from those stores come back `Binary`, which breaks the round trip (constraint 4) |

**Tension:** operators prefer E3 (sharing works with no stub), and BEAM veterans prefer E1 (specs are the contract). E1 wins because it keeps behaviour derivable from specs alone, and the compile-time `ffi-stored-strings` lint turns the remaining silent case into a visible one before the code ever runs.

## Alternatives Considered

See the steelman table above. Additionally:

### Infer conversion from the Beamtalk type alone

Use the existing `ParamType.type_` (`String | Binary` → convert, `Dynamic` → don't). Rejected because the mapping is lossy for this purpose: `iodata()`/`iolist()` map to `Dynamic` (they would pass through tagged although they are text), and map key/value types collapse to `Dictionary`. Widening the Beamtalk type mapping to carry this would change type-checking for everyone. A separate shape, computed from the raw spec, changes nothing else.

### Re-tag `binary()` results as `String`

Rejected. `binary()` is the type of `crypto:hash/2` and `term_to_binary/1` too; tagging those as `String` would be sniffing by spec (constraint 5). Only types that are text *by declaration* (`unicode:chardata()`, `unicode_binary()`, `string()`) are tagged.

## Consequences

### Positive
* Comparison and lookup callees answer correctly with no user action. The BT-4243 repros are fixed by construction, not by listing them.
* Strings round-trip through opaque Erlang code.
* Most calls get slightly cheaper: no top-level unwrap scan when the signature is known, and no `badarg` charlist retry, since `string()` parameters are converted up front.
* The static types of FFI results become honest (`binary()` → `Binary`).
* Every case the boundary can't resolve gets a diagnostic at compile time: `ffi-untyped-strings` for calls with no signature, `ffi-stored-strings` for Strings passed to storage or messaging functions.

### Negative
* **Breaking:** a **top-level** String passed to a `term()` parameter used to arrive as a raw binary and now arrives tagged. Examples: `Erlang erlang send: pid with: "hi"`, `Erlang persistent_term put: #k with: "v"`, `Erlang erlang term_to_binary: "x"`. Erlang receivers of such data must be declared through a stub (or the data converted explicitly). The `ffi-stored-strings` lint flags these at compile time, and the sibling-repo audit (see Migration Path) finds existing ones.
* A `string()` result that isn't valid UTF-8 now raises instead of returning bytes.
* **Behaviour change for list results:** with a signature, a printable list of integers whose spec isn't `string()` stays a List. `Erlang lists seq: 65 with: 70` answers `#(65, 66, 67, 68, 69, 70)` instead of `"ABCDEF"`. This is the correct answer, but code that relied on the old guess changes.
* One more runtime module (`beamtalk_ffi_convert`) and a shape field in the registry. The shapes cross the Rust/Erlang boundary, so they need a conformance fixture.
* The `ffi-stored-strings` lint uses a list of storage and messaging MFAs. It only decides where to warn, but it does need occasional additions. Like any static check, it can't see a String inside a value it can't type: a `Dynamic` argument is warned conservatively, and that can be noisy in untyped code.

### Neutral
* Beamtalk-owned modules (`beamtalk` mode) are unchanged.
* Calls without a signature behave exactly as today at runtime. The only difference is the new warning.

## Implementation

0. **Prove the approach first (S).** Compute shapes for the repro MFAs (`lists:member/2`, `keyfind/3`, `keymember/3`, `maps:get/2`, `maps:is_key/2`, `proplists:get_value/2`) and for `string:uppercase/1` and `lists:seq/2`, straight from the OTP 27 and 28 specs. Hand-write the converted Core for one call of each and run it. This confirms the specs resolve to the shapes in the table above (remote types such as `unicode:chardata()` included) before any of the pipeline is built.
1. **Spec reader and registry (core + compiler app).** Compute the conversion shape for each parameter and the return type from the raw Erlang spec. Resolve remote and user types (`unicode:chardata()`, `iodata()`) before classifying. Carry it in `FunctionSignature`, and get it from stub declarations as well. Add the shared conformance fixture.
2. **Runtime.** Add `beamtalk_ffi_convert:arg/2` and `result/2`, with the `#invalid_utf8` error. Add a `typed` mode to `direct_call`/`native_call`, which skips the shallow unwrap and the retry.
3. **Codegen.** At each FFI and `native:` call site with a signature, wrap each non-`opaque` argument and the result, and emit `typed`. Leave the call unchanged when there's no signature.
4. **Type checker.** Map `binary()`-family *returns* to `Binary`. Add the `ffi-untyped-strings` and `ffi-stored-strings` warnings (with `@expect` and `[diagnostics]` support).
5. **Docs and tests.**
   * Rewrite "Strings at the FFI boundary" in `docs/beamtalk-native-erlang.md`.
   * BUnit tests for the repros, the round trip, chardata results, invalid UTF-8, and the no-signature warning per call shape.
   * Record the new warnings (`ffi-untyped-strings`, `ffi-stored-strings`) in `docs/development/surface-parity.md`.

Rough size: L. Steps 1–2 can land before 3–4 behind the `typed` mode, which nothing emits until step 3.

## Migration Path

* **Audit:** before step 3 lands, grep the 8 sibling repos audited by BT-4170 and the stdlib for FFI calls that pass a top-level String to a `term()` parameter of a storage or messaging function: `erlang:send`, `gen_server`/`gen_statem` `call`/`cast`, `ets`, `persistent_term`, `term_to_binary`. For each one, add a stub declaring the parameter as text, or convert explicitly.
* **Compile-time signal:** the `ffi-stored-strings` lint fires on every build, so any case the audit misses shows up as a warning at the call site.
* **List results:** the same audit greps for FFI results used as Strings that come from a non-`string()` list spec (the old printable-list guess). Each one gets a stub declaring `string()`, or an explicit conversion.
* The `ffi-untyped-strings` warning is new. Existing code compiles with warnings, not errors.

## References
- Related issues: BT-4243 (this decision), BT-4223 / PR #4609 (shallow-unwrap documentation), BT-4173 / BT-4170 (tagged String), BT-4023 (FFI Result typing), BT-4153 (precomputed coercion mode)
- Related ADRs: ADR 0075 (FFI type definitions), ADR 0076 (Result at the boundary), ADR 0101 (FFI error boundary / `native:`), ADR 0139 (BEAM abstraction boundaries — "FFI membrane: TYPED")
- Documentation: `docs/beamtalk-native-erlang.md` § Strings at the FFI boundary
