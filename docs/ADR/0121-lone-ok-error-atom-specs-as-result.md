# ADR 0121: Lone `ok`/`error` Atom Return Specs Map to `Result` in FFI Type Inference

## Status
Proposed

## Context

ADR 0076 established that the FFI proxy (`beamtalk_erlang_proxy.erl`) unconditionally converts any bare `ok`/`error` atom returned from an Erlang call into `Result ok: nil` / `Result error: nil`, and that the spec reader (`beamtalk_spec_reader.erl`, from ADR 0075) mirrors this at the type level: when an Erlang `-spec`'s return type is a **union** containing `ok`/`error` — e.g. `ok | {error, E}` or `{ok, T} | {error, E}` — it emits `Result(T, E)` instead of a plain type.

### Current state

The union-recognition machinery lives in `map_union/2` → `map_union_result/1` → `classify_union_branches/1` (`beamtalk_spec_reader.erl`, ~lines 1005–1206). It correctly classifies `ok`/`error` branches (bare atom, `{ok, T}`, or `{error, E}`) out of a union, resolves the ok/error inner types, and formats `Result(T, E)` (or the `Result(T)` shorthand when the error side is `Dynamic`, or bare `Result` when both sides are `Dynamic`).

This machinery is only reached when the return type is a `{type, _, union, Branches}` node. A **lone**, non-union return type — `-spec f(...) -> ok.` or `-spec f(...) -> error.` — never reaches `map_union`/`map_union_result` at all. It falls through the general `map_type/1` clause list to:

```erlang
map_type({atom, _, _}) ->
    <<"Symbol">>.
```

So `beamtalk_actor:unregister/1`, specced as:

```erlang
-spec unregister(#beamtalk_object{}) -> ok.
```

is typed as `Symbol` by the spec reader, even though `beamtalk_erlang_proxy:coerce_result/1` (ADR 0076, §1) converts its `ok` return value to `Result ok: nil` **unconditionally**, with no dependence on whether the originating spec was a union. The static type (`Symbol`) and the runtime value (`Result`) disagree.

This surfaced concretely in `stdlib/src/actor.bt`, `Actor>>unregisterName`:

```beamtalk
/// Deregisters this actor's registered name; callers do not need to call
/// `unregister` from `terminate:`. Returns bare `ok` which auto-converts
/// to `Result ok: nil` per ADR 0079.
internal unregisterName -> Symbol => (Erlang beamtalk_actor) unregister: self
```

Declaring the honest `-> Result(Nil, Dynamic)` fails to compile (`declares return type Result(Nil, Dynamic), but body returns Symbol`), forcing the method to keep the misleading `-> Symbol` annotation — which misrepresents the runtime shape to callers, the type checker, and LSP completions.

### Constraints

- Runtime coercion (ADR 0076) is **unconditional** on the returned atom, not on the spec shape. ADR 0076 explicitly rejected spec-dependent conversion (see its "Alternative: Spec-Dependent Conversion") because it would make the same `ok` atom convert differently depending on whether the callee happened to declare a union. Type inference must track that same unconditional rule, or the static/dynamic mismatch this ADR fixes simply reappears in a different shape.
- The fix must not touch `map_type/1`'s general atom-literal clause, which is reached recursively for *nested* atom positions (a tuple element, a list element, a param type, a resolved user-type body) — nowhere else in the codebase should a bare `ok` appearing inside a larger type suddenly become `Result`. Only the **top-level return-type position** of a function clause is where the ADR-0076 runtime coercion applies.
- Per `docs/development/architecture-principles.md` § Duplication & the Shared-Leaf-Module Pattern, the fix must reuse the existing `classify_union_branches/1` / `resolve_ok_type/1` / `resolve_err_type/1` / `format_result_type/2` helpers rather than re-implementing ok/error-to-`Result` mapping a second time for the non-union case.
- Blast radius (searched `stdlib/src/*.bt` for `-> Symbol => (Erlang ...)`): only `Actor>>unregisterName` (`actor.bt:258`, backed by `beamtalk_actor:unregister/1`, spec'd bare `-> ok.`) is affected. `Inspector>>kind` (`inspector.bt:88`) also returns `Symbol` from an FFI call, but its callee `beamtalk_inspector:kindOf/1` is specced `-> atom()` and returns a real content atom (`#actor`, `#object`, ...), not the `ok`/`error` coercion pattern — unaffected. Other stdlib wrappers around bare-`ok`-specced Erlang functions (e.g. `beamtalk_announcements:'unsubscribeRef'/1`) already avoid the mismatch by specing their public-facing wrapper as `-> nil.` and returning the literal atom `nil` rather than exposing the raw `ok` coercion — so this ADR's fix does not change their declared types.
- Out of scope: a **lone, non-union tuple** return spec — `-spec f(...) -> {ok, T}.` with no union — has the analogous gap (`map_type`'s generic tuple clause maps it to `Tuple(Symbol, T)` rather than `Result(T, Dynamic)`). No current stdlib `.bt` wrapper hits this case (grep found none), so it is left for a follow-up issue rather than folded into this fix; the mechanism proposed below (a single return-type entry point) makes that follow-up a small, symmetric addition rather than a redesign.

## Decision

Introduce a `map_return_type/1` entry point in `beamtalk_spec_reader.erl` that is used **only** at the return-type position of a function clause (never for nested/recursive type positions), and route a lone literal `ok` or `error` atom through the *existing* ADR-0076 recognition machinery as a one-branch pseudo-union, rather than through the general `map_type/1` atom clause:

```erlang
%% Return-type entry point — the only call sites that may trigger ADR-0076
%% ok/error-atom Result recognition for a *lone* (non-union) atom. Nested
%% type positions keep calling map_type/1 directly and keep mapping a lone
%% `ok`/`error` atom to Symbol, unchanged.
-spec map_return_type(tuple()) -> binary().
map_return_type({type, Line, union, Branches}) ->
    map_union(Branches, Line);
map_return_type({atom, _, ok} = RetType) ->
    map_union_result([RetType]);
map_return_type({atom, _, error} = RetType) ->
    map_union_result([RetType]);
map_return_type(RetType) ->
    map_type(RetType).
```

The two call sites that currently call `map_type(RetType)` directly on a function clause's return type switch to `map_return_type/1`:

- `extract_clause/1`'s plain `fun` clause (~line 428): `{ok, ParamList, map_type(RetType)}` → `{ok, ParamList, map_return_type(RetType)}`.
- `resolve_type_with_constraints/2`'s fallback clause (~line 604), used for `bounded_fun` (`when ...`) clauses whose return type is neither a union nor a constrained type variable: `map_type(Type)` → `map_return_type(Type)`.

All other call sites of `map_type/1` (tuple elements, list elements, user-type/remote-type body resolution, union-branch resolution) are untouched — a bare `ok`/`error` appearing in any of those positions still maps to `Symbol`, exactly as today.

Because `map_union_result([{atom, _, ok}])` reuses `classify_union_branches/1`, a lone `ok` classifies into `OkTypes = [nil]`, `ErrTypes = []`, which `resolve_ok_type/1` / `resolve_err_type/1` / `format_result_type/2` already turn into the shorthand `Result(Nil)` — the same string a real union branch `{ok, T}` with no error arm produces today. No new formatting rule is introduced; a lone atom is treated as the one-branch case of the union machinery that already exists.

**Beamtalk-facing effect:**

```erlang
%% beamtalk_actor.erl
-spec unregister(#beamtalk_object{}) -> ok.
```

```beamtalk
// Before (misleading — compiles, but disagrees with the runtime value):
internal unregisterName -> Symbol => (Erlang beamtalk_actor) unregister: self

// After (honest — matches ADR-0076's runtime coercion):
internal unregisterName -> Result(Nil) => (Erlang beamtalk_actor) unregister: self
```

**REPL session:**

```beamtalk
counter unregister
// => Result ok: nil
```

**Error example — what a caller sees if they treat the old `Symbol` contract as still valid:**

```beamtalk
counter unregisterName == #ok
// Before: true (Symbol equality)
// After:  ERROR: does_not_understand: Result does not understand '=='
//         with a Symbol argument in the way this comparison was written —
//         callers must migrate to `result isOk` / `result ok`, matching
//         every other ADR-0076 FFI Result consumer.
```

## Prior Art

### This ADR vs. ADR 0075/0076
This is not a new design — it is closing a gap ADR 0076 opened but did not fully cover: it establishes *unconditional* runtime coercion of bare `ok`/`error`, but its type-side counterpart (ADR 0075's spec reader) only recognized the pattern inside unions. The "prior art" for *this* decision is therefore internal: the union-branch recognition rules in `map_union_result/1` are the precedent, extended to a domain (lone atom returns) they were always meant to imply but didn't yet cover.

### Swift/Objective-C (via ADR 0076)
ADR 0076 already adopted the idea that a rigid, universally-recognized convention on the boundary language can be reliably recognized and converted — Objective-C's `NSError **` outparam convention → Swift `throws`. The gap this ADR closes is the same idea applied consistently: if the *runtime* coercion doesn't care whether the source spec was a union, the *static* recognition of that coercion shouldn't either.

### Gleam (BEAM)
Not directly applicable — Gleam avoids this problem entirely by using `{ok, V}`/`{error, R}` as its native `Result` representation, so there is no separate "was this spec a union" question. Beamtalk's tagged-map `Result` (ADR 0060) requires the boundary conversion that creates this gap in the first place; Gleam's approach was already considered and distinguished in ADR 0076.

## User Impact

### Newcomer
No visible change to the REPL value (`Result ok: nil` today and after — the runtime behavior was already correct). The improvement is behind the scenes: a newcomer writing a `.bt` wrapper around a bare-`ok`-specced Erlang function can now declare `-> Result(Nil)` and have it compile, instead of hitting a confusing "declares Result, but body returns Symbol" error that offers no clue that the spec reader — not their code — is the source of the mismatch.

### Smalltalk developer
Reinforces that FFI-wrapped methods should expose a proper object (`Result`) with real messages (`ok`, `isOk`, `map:`), not a bare `Symbol` that happens to be `#ok`, closing a small crack where a "primitive-looking" value leaked through the type system.

### Erlang/BEAM developer
Matches their existing expectation from ADR 0076: any Erlang function whose contract is "returns `ok`, raises on failure" behaves identically whether it's specced as `-> ok.` or `-> ok | {error, _}.` at the value level. This ADR makes the *type* level track that same equivalence, removing a case where the spec's shape (not its meaning) determined the declared Beamtalk type.

### Production operator
No runtime change at all — this is a compile-time type-inference fix. Nothing to test in production; `beamtalk_erlang_proxy:coerce_result/1` is unchanged.

### Tooling developer (LSP, IDE)
Auto-extract (ADR 0075) and LSP hover/completions for any Erlang function specced as a lone `-> ok.`/`-> error.` now show `Result(Nil)` instead of `Symbol`, so completions after `.` on such a call offer `map:`, `andThen:`, `ok`, etc. instead of nothing useful.

## Steelman Analysis

### Option A: Reuse union machinery for lone atom returns at the return-type entry point (Chosen)

- 🧑‍💻 **Newcomer**: "I get a working, honest type without having to know anything about how the spec reader is implemented — `-> Result(Nil)` just compiles."
- 🎩 **Smalltalk purist**: "The declared type finally matches the object I actually get back and can send messages to — that's the whole point of a type annotation."
- ⚙️ **BEAM veteran**: "This is exactly consistent with how I already think about `ok`-only specs: they're specs that never document their failure mode via a union, not specs that behave differently."
- 🏭 **Operator**: "Zero runtime change — this is purely a compile-time inference fix, the safest kind of fix to ship."
- 🎨 **Language designer**: "Reuses `classify_union_branches`/`resolve_ok_type`/`format_result_type` verbatim as a one-branch pseudo-union — no duplicated ok/error-recognition logic, no second place to keep in sync."

### Option B: Require an explicit type override annotation instead of extending inference (Rejected)

- 🧑‍💻 **Newcomer**: "At least there'd be one obvious place to look — an override table — instead of wondering why inference disagrees with the runtime."
- 🎩 **Smalltalk purist**: "Explicit is better than implicit — a human decides the type, not a heuristic."
- ⚙️ **BEAM veteran**: "I can see every exception listed in one file rather than trusting a spec-shape heuristic to always get it right."
- 🏭 **Operator**: "No change to spec-reader behavior — the safest possible change is no change."
- 🎨 **Language designer**: "This mirrors TypeScript's `.d.ts` ambient-override model — a human correction layer over inference the tool can't fully trust."

This is a real alternative but is rejected for the same reason ADR 0076 rejected "opt-in via type annotation" for the runtime coercion: it adds ceremony (a per-function override entry) to close a gap that has one clear, mechanical, unconditional rule — "the runtime coerces every bare `ok`/`error`, so the type inference should too." An override table would need one entry per affected function today and silently miss every *future* bare-`ok`-specced function someone adds, reproducing this exact bug on a rolling basis. Fixing the inference rule once closes the whole class.

### Option C: Make runtime coercion spec-dependent instead (Rejected)

- 🎨 **Language designer**: "If the type inference can't easily see it without a union, maybe the *runtime* is wrong to convert unconditionally — align the runtime with what's staticaly knowable instead of the other way around."

This is ADR 0076's own "Alternative: Spec-Dependent Conversion," already rejected there: it would make the identical `ok` atom returned by two Erlang functions convert to `Result` or stay `Symbol`/`Tuple` purely based on whether one author happened to write a union spec and the other didn't — "confusing inconsistency" in ADR 0076's own words. Re-opening it here would also require reverting shipped, implemented runtime behavior to fix a type-checker gap, which is a much larger and riskier change than fixing the type checker.

### Tension Points

- **BEAM veterans** and **language designers** are the two cohorts most likely to prefer Option B (an explicit, auditable override list) over Option A (heuristic extension) — but only until they notice that Option A's "heuristic" is not a heuristic at all: it mirrors a rule the runtime already applies unconditionally (ADR 0076), so there is no case where Option A's inference and the runtime disagree that Option B's override list would need to catch instead.
- Nobody seriously steelmans Option C beyond "it's the more conservative side to move" — it directly contradicts a rationale ADR 0076 already established and shipped, and reverting implemented runtime behavior is a strictly larger change than fixing spec-reader inference.

## Alternatives Considered

### Alternative: Make `map_type/1`'s atom clause itself Result-aware

Change `map_type({atom, _, ok}) -> <<"Symbol">>.` to unconditionally return `Result(Nil)`, without introducing a separate return-type entry point.

**Rejected because:** `map_type/1` is called recursively for every nested type position — tuple elements, list elements, resolved user-type/remote-type bodies, individual union branches (`map_union_result/1` itself calls `map_type/1` on inner ok/error payload types). Making the atom clause Result-aware globally would, for example, turn a `-spec f() -> {ok, T}.`'s tuple-element mapping of the `ok` tag itself into `Result(Nil)` nested inside a `Tuple(..)`, or turn a param type literally named `ok` into `Result(Nil)` where a param can never receive ADR-0076 coercion (coercion applies to *return* values only). Keeping the recognition at the return-type entry point only, and threading it through the existing per-clause call sites, avoids this collateral damage.

## Consequences

### Positive

- Closes the static/dynamic type mismatch identified in BT-3498: any Erlang function specced as a lone `-> ok.` or `-> error.` now infers `Result(Nil)` / `Result(Nil)` (error-side), matching `beamtalk_erlang_proxy:coerce_result/1`'s unconditional runtime coercion.
- No duplicated ok/error-recognition logic — `map_return_type/1` is a 4-clause dispatcher that delegates to the existing `map_union/2` and `map_union_result/1`.
- `Actor>>unregisterName` can declare its honest return type (`-> Result(Nil)` instead of `-> Symbol`), and its doc comment's claim about ADR 0076/0079 coercion is now enforceable by the type checker rather than just true at runtime.
- Auto-extract (ADR 0075) and LSP completions improve for the same class of functions, with no per-function manual work.

### Negative

- **Source-breaking for the one affected stdlib site:** `Actor>>unregisterName`'s declared return type changes from `Symbol` to `Result(Nil)`. Any caller comparing its result with `== #ok` or treating it as a `Symbol` breaks with a `does_not_understand` error and must migrate to `Result`'s API (`isOk`, `ok`, `value`). A codebase-wide grep is required as part of implementation to confirm no other caller depends on the old `Symbol` contract.
- **Widens the surface silently for anyone who later specs a new bare-`ok` Erlang function:** future FFI wrappers around such functions will get `Result(Nil)` inference "for free," which is the intended fix, but means any *future* code that assumed "FFI calls without a union spec return atoms" (an assumption BT-3498 shows was never actually true) will need to account for `Result` from day one.

### Neutral

- No runtime behavior changes anywhere — this is a compiler/spec-reader-only fix. `beamtalk_erlang_proxy.erl` is unchanged.
- The symmetric gap for lone non-union **tuple** returns (`-spec f() -> {ok, T}.`) is left open; per the Context section, no current stdlib code hits it, and `map_return_type/1`'s dispatcher gives a natural, low-risk place to add that case later without revisiting this design.

## Implementation

- **`beamtalk_spec_reader.erl`:** add `map_return_type/1`; change `extract_clause/1`'s plain-`fun` clause and `resolve_type_with_constraints/2`'s fallback clause to call it instead of `map_type/1` directly.
- **Regenerate generated artifacts:** re-run the native type registry / `generated_builtins.rs` generation step that consumes spec-reader output (per `build_stdlib.rs`-style generator ownership — do not hand-edit the generated file).
- **`stdlib/src/actor.bt`:** update `Actor>>unregisterName`'s declared return type from `-> Symbol` to `-> Result(Nil)`; audit callers (`Actor>>unregister`, `stdlib/test/*.bt`) for `Symbol`-shaped usage and migrate to `Result`.
- **Tests:**
  - `beamtalk_spec_reader_tests.erl`: unit tests for `map_return_type/1` covering `-> ok.` → `Result(Nil)`, `-> error.` → `Result(Nil)`, confirming nested atom positions (tuple element, list element, param type) are unaffected.
  - A type-checker test (alongside the existing `Result`/FFI inference tests in `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/`) confirming `infer_ffi_call` resolves a bare-`ok`-specced Erlang function to `Result(Nil)`.
  - `stdlib/test/actor_test.bt` (or equivalent): confirm `unregisterName`'s declared type checks against its FFI body post-fix.
- **Affected components:** Erlang spec reader (Compilation-adjacent runtime tooling), type checker's FFI inference (`crates/beamtalk-core/src/semantic_analysis/type_checker/inference/send/ffi.rs`) only insofar as it consumes the now-corrected type string — no logic change expected there since union-sourced `Result(...)` strings already parse correctly today.
- **Effort:** S — a 4-clause dispatcher function, two call-site changes, one `.bt` annotation update, and matching unit/type-checker tests.

## Migration Path

### `Actor>>unregisterName` callers comparing against `Symbol`

Before:
```beamtalk
(counter unregisterName) == #ok
  ifTrue: [...]
```

After:
```beamtalk
(counter unregisterName) isOk
  ifTrue: [...]
```

A grep of `stdlib/src/*.bt` and `stdlib/test/*.bt` for `unregisterName` found only the internal call from `Actor>>unregister` (`actor.bt:253`), which discards the result (`self unregisterName.`) and is unaffected by the type change.

## References
- Related issues: BT-3498
- Related ADRs: ADR 0075 (Erlang FFI type definitions — spec reader / auto-extract pipeline), ADR 0076 (ok/error tuple → Result at FFI boundary — establishes the unconditional runtime coercion this ADR's type inference must track), ADR 0079 (cited in `Actor>>unregisterName`'s doc comment for the runtime coercion claim), ADR 0060 (Result type), ADR 0068 (parametric types — `Result(T, E)` generics)
- Documentation: `docs/development/architecture-principles.md` § Duplication & the Shared-Leaf-Module Pattern
