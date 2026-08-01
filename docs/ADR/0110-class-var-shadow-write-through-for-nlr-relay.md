# ADR 0110: Class-Variable Shadow Write-Through for Foreign NLR Relay

## Status
Proposed (2026-08-01)

## Context

### Problem statement

A class method that mutates a class variable and then invokes a caller-supplied block loses that mutation if the block escapes with `^` (a non-local return, NLR):

```beamtalk
Value subclass: CollectionDriver
  classState: runs = 0

  class countedRun: aBlock :: Block over: aList :: List -> Nil =>
    self.runs := self.runs + 1
    aList do: [:x | aBlock value: x]
    nil

  class runCount -> Integer => self.runs
```

```beamtalk
Value subclass: CollectionProbe
  class escapeAfterCountedRun: aList :: List -> Integer =>
    CollectionDriver countedRun: [:x | x > 1 ifTrue: [^x]] over: aList
    0
```

```beamtalk
before := CollectionDriver runCount
(CollectionProbe escapeAfterCountedRun: #(1, 2, 3))   // => 2   (correct, BT-3022)
CollectionDriver runCount                              // => before   (WRONG, want before + 1)
```

The returned value is correct; only the class-variable mutation is lost. This is not a hypothetical — it is reproducible today and documented as a known trap in `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods.

### Why this happens

ADR 0041 (Universal State-Threading Block Protocol) established that every non-local return throws a 4-tuple `{'$bt_nlr', Token, Value, State}`, and a method's own try/catch recovers `State` only when the token matches its own (`CatchTok =:= OurToken`). For a class method, that `State` slot is the method's threaded `ClassVars`; the catch arm for a matching token yields `{'class_var_result', Value, State}` (`NlrBoundary::ClassMethod` in `crates/beamtalk-core/src/codegen/core_erlang/mod.rs`).

The failure case is a **foreign** NLR: a block invoked *indirectly* — through `aList do: [:x | aBlock value: x]`, which lowers to `lists:foldl`/similar inside compiled library code — whose `^` belongs to a *different* method's frame (here, `CollectionProbe escapeAfterCountedRun:`). Its token does not match `CollectionDriver countedRun:over:`'s own token, so the catch guard fails and the exception falls through to the generic arm, which re-raises the tuple unchanged. That tuple's `State` field holds the **foreign frame's own state** (whatever `CollectionProbe`'s method was threading), not `CollectionDriver`'s `ClassVars` — the two are different buckets, and there is no slot to carry both simultaneously.

The re-thrown exception propagates to the Erlang runtime layer. `apply_class_method_fun/6` (`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`) already distinguishes this case from a genuine failure — a dedicated clause matches `throw:Nlr:NlrST when ?IS_NLR(Nlr)` and passes it through unlogged (added for BT-3022), separately from the catch-all `ErrClass:Error:ErrST` clause that logs and classifies as a real error. But both outcomes are then folded into the same `{error, {raised, _, _, _}}` shape by `apply_class_method_in_context/6`, and `invoke_class_method/7`'s handler for that shape always replies with the **pre-call** `ClassVars`:

```erlang
{error, {raised, _ErrClass, Error, _ST}} ->
    {reply, {error, Error}, ClassVars}     %% original, pre-call ClassVars
```

There is no `NewClassVars` value available at this point to use instead — the mutated value exists only as a `let`-bound Core Erlang variable inside the `try` body, invisible to the catch handler.

### Why there is no surgical per-call-site fix

Wrapping each block-invocation site in the class method's own generated code (to catch a foreign NLR and re-attach `ClassVars` before re-throwing) does not generalize: in the repro above, the block is invoked *indirectly*, through `beamtalk_collection:to_list/1` and a `lists:foldl` inside `do:`'s own implementation. Codegen has no call site there to instrument — the block escapes into library code the class method's generated body never touches directly.

### Existing precedent for a write-through shadow

`beamtalk_actor.erl` already solves a shaped-alike problem for actor state. `self_dispatch/2`'s `unwrap_dispatch_result/1` writes the actor's current state into the process dictionary (`put('$bt_actor_state', NewState)`) after every nested self-dispatch call, so other code running later in the same handler sees a live, current state without it being threaded functionally through every intermediate call frame. `restore_dispatch_pdict/1` runs once, in the `after` block of the *outermost* `handle_call`/`handle_cast` — it restores whatever value (or absence) preceded that call, so the shadow never leaks into the *next*, unrelated invocation of the same gen_server. This ADR follows the same two-part discipline — write-through during the call, single cleanup at the outer boundary — not a per-nesting-level save/restore; see the Runtime change below for why that distinction matters here.

### Constraints

- A class method's own `^` (matching token) must keep working exactly as today — this path is correct and already tested (BT-3022).
- A genuine runtime error after a class-var mutation must still revert the mutation — this is the existing, desired, documented behavior and is one of BT-3032's acceptance criteria.
- The fix must not add overhead to class methods that never mutate class vars (the majority of the ~100+ stdlib classes).
- Class methods execute one-at-a-time inside their class's singleton `gen_server` — there is never more than one in-flight call per class process, so a single well-known process-dictionary key per class process is safe without additional synchronization.

## Decision

Add a **process-dictionary shadow write-through**, scoped narrowly to the one case that needs it: relaying a foreign NLR out of a class method that mutated class vars beforehand. The existing functional `ClassVars` threading is untouched for every other path, and neither the relay mechanism (`class_send_dispatch/3`, `metaclass_send_dispatch/4`, `class_self_dispatch/4`) nor the outcome type `apply_class_method_in_context/6` computes needs to change — the fix is entirely contained in `invoke_class_method/7`, the one place that decides what `ClassVars` value the class gen_server retains.

### Codegen change

Class-var assignment (`self.foo := value`) already funnels through one function, `CoreErlangGenerator::generate_field_assignment` (`crates/beamtalk-core/src/codegen/core_erlang/expressions.rs`), which emits `let ClassVarsN = call 'maps':'put'(field, Val, ClassVars{N-1}) in` and flips a sticky per-method `class_var_mutated` flag via `next_class_var()` (`core_erlang/mod.rs`) — the same flag that already gates `NlrBoundary::ClassMethod { has_class_vars }`. Add one more line at that emission site, writing the just-updated map into a fixed process-dictionary key:

```erlang
let Val = <value> in
let ClassVars1 = call 'maps':'put'('runs', Val, ClassVars0) in
let _ = call 'erlang':'put'('$bt_class_vars_shadow', ClassVars1) in
```

**This must be gated on `self.block_depth == 0`, not just `self.in_class_method()`.** `in_class_method()` is a lexical flag that stays `true` for the entire method body, including inside nested block literals — but a block literal written inside class P's method can be invoked *from a different class's process* (per ADR 0109 / the block-runs-where-invoked semantics documented in `beamtalk-language-features.md`). Writing the shadow unconditionally would let a block invoked while executing inside class C's gen_server write class P's `ClassVars` into the pdict key that C's own `invoke_class_method/7` reads back — corrupting an unrelated class's persisted state on a foreign-NLR relay. `block_depth` (already tracked in `CoreErlangGenerator`, incremented in `generate_block`) distinguishes "top-level statement in the method body" from "inside a nested block," so the gate becomes `self.in_class_method() && self.block_depth == 0 && self.class_var_names().contains(field_name)`.

This scoping is not a new limitation — it matches existing behavior. `generate_block` already saves and restores `class_var_version` around a block's body (BT-1550, "so self-calls inside a conditional branch don't leak `ClassVars{N}` bindings into the outer scope"), meaning a class-var mutation made *inside* a block is already discarded on the method's normal-return path today. Scoping the shadow to `block_depth == 0` keeps the shadow's contents consistent with what already survives normal return — it does not shadow anything that wasn't already going to be lost.

This only fires for top-level mutations in class methods that already have `class_var_mutated = true` — i.e., exactly the methods that already pay the `ClassVars` threading cost today. Methods with no class-var mutations, and blocks nested inside any method, emit nothing new; no new analysis pass is needed.

**Scope boundary:** this covers compiled Beamtalk class methods only. Class methods installed as runtime `ClassBuilder` funs (ADR 0084) and class methods implemented directly in hand-written Erlang do not go through `generate_field_assignment`, so a class-var mutation in either of those, followed by a foreign-NLR relay, keeps today's lossy behavior. Extending coverage to those paths is out of scope for this ADR.

### Runtime change

`invoke_class_method/7` (`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`) already has a clause for `{error, {raised, _ErrClass, Error, _ST}}` that always replies with the pre-call `ClassVars`. Split out the foreign-NLR-relay sub-case — already distinguishable via the very `?IS_NLR/1` guard macro this module defines at the top (`ErrClass =:= throw andalso ?IS_NLR(Error)` can only be true here because `apply_class_method_fun/6` and `apply_compiled_class_method/7` already route genuine errors and NLR passthroughs to separate `catch` clauses before either reaches this point) — and read the shadow only for that sub-case:

```erlang
invoke_class_method(Selector, Args, ClassName, _Module, DefiningClass, DefiningModule, ClassVars) ->
    try
        case apply_class_method_in_context(Selector, Args, ClassName, DefiningClass, DefiningModule, ClassVars) of
            test_spawn ->
                test_spawn;
            {ok, {class_var_result, Result, NewClassVars}} ->
                {reply, {ok, Result}, NewClassVars};
            {ok, Result} ->
                {reply, {ok, Result}, ClassVars};
            {error, #beamtalk_error{} = Error} ->
                {reply, {error, Error}, ClassVars};
            {error, undef_in_body} ->
                {reply, {error, undef}, ClassVars};
            %% BT-3032: a foreign `^` relay is not a failure of *this* method —
            %% recover ClassVars mutated before the relay from the shadow written
            %% by generate_field_assignment, instead of reverting to ClassVars
            %% as it stood before this call.
            {error, {raised, throw, Error, _ST}} when ?IS_NLR(Error) ->
                NewClassVars =
                    case erlang:get('$bt_class_vars_shadow') of
                        undefined -> ClassVars;   %% no mutation occurred before the relay
                        Shadow -> Shadow
                    end,
                {reply, {error, Error}, NewClassVars};
            {error, {raised, _ErrClass, Error, _ST}} ->
                %% Genuine error: revert, exactly as today.
                {reply, {error, Error}, ClassVars}
        end
    after
        erlang:erase('$bt_class_vars_shadow')
    end.
```

The `after` clause — not a bare trailing statement — is required here: `apply_class_method_in_context/6` has an un-`try`'d prelude (`beamtalk_class_registry:class_object_tag/1`, `lookup_class_method_fun/2`, `is_test_execution_selector/1`) that could in principle raise before the `case` ever completes, and the class gen_server is long-lived, so any path that skipped the erase would leave a stale shadow for the *next unrelated call* to read. `try ... after ... end` guarantees the erase runs regardless.

This guarantees the shadow never survives past a single `class_method_call`/`metaclass_method_call`, so a later, unrelated call on the same class gen_server can never read a stale value. `invoke_class_method/7` is always the outermost frame for a given external call: `class_self_dispatch/4` and `class_self_dispatch_local/4` (the self-send path) call `apply_class_method_in_context/6` directly and run in the same process without a new gen_server hop, so a chain of self-dispatched calls within one external call writes the shadow sequentially (each mutation overwrites the previous value) and a foreign NLR relayed through any depth of that chain still unwinds to this same `try` before the call returns. No per-nesting-level save/restore is needed — unlike `beamtalk_actor.erl`'s `'$bt_actor_state'`, which restores the pre-call value in its `after` because instance self-dispatch exposes "the currently live state" *during* the chain for other code to read; here nothing reads the shadow until after the whole chain has finished, so straight overwrite-and-erase-once-at-the-end is sufficient. Cross-class self-dispatch is impossible by construction (`class_self_dispatch` only walks one class's own superclass chain), and different classes are different gen_server processes with independent process dictionaries, so there is no cross-class shadow collision through self-dispatch — the collision risk was in codegen (see above), not here.

### REPL example

```
st> CollectionDriver runCount
0
st> CollectionProbe escapeAfterCountedRun: #(1, 2, 3)
2
st> CollectionDriver runCount
1
```

### Error example (genuine error still reverts)

```beamtalk
class countedRun: aBlock :: Block over: aList :: List -> Nil =>
  self.runs := self.runs + 1
  1/0.   "genuine error after mutation"
  nil
```

```
st> CollectionDriver runCount
0
st> CollectionDriver countedRun: [:x | x] over: #(1)
Error: ArithmeticError: division by zero
st> CollectionDriver runCount
0    "mutation reverted, exactly as today"
```

## Prior Art

### Pharo/Squeak Smalltalk — why this bug class doesn't exist there

This bug is BEAM-specific, not Smalltalk-general, and it's worth being explicit about why. In Pharo, a class variable is a true mutable slot shared by the class and its instances; a non-local return unwinds the real call stack (via `BlockContext`/`thisContext` machinery) directly to the home context, and there is no separate "commit" step for a class-variable write to survive — the assignment already happened, in place, before the unwind. **Not adopted, but explains the constraint**: Beamtalk inherits Smalltalk's mutable-class-variable *semantics* while compiling to an immutable substrate (Core Erlang has no mutable variables), so it must reconstruct "the assignment already happened" via functional threading — and it's precisely the threading reconstruction that this bug exposes as incomplete for one relay path. The fix's goal is to restore the Pharo-equivalent guarantee, not to add a new one.

### Kotlin/Java — mutable fields, same story as Smalltalk

A `companion object` field in Kotlin (or a `static` field in Java) mutated before a lambda-escaping early return (e.g. via a labeled `return@outer` or a checked exception used for control flow) is never "lost," for the same reason as Pharo: the write is a direct memory mutation, not a functional rebinding that a catch handler might fail to recover. **Confirms the same point from the mainstream-OO side**: this entire bug class is an artifact of choosing functional state-threading as the *compilation strategy* for mutable semantics, not something inherent to "a variable mutated before a non-local return."

### Erlang/OTP — process dictionary as an escape hatch, not a primary mechanism

Erlang style guides discourage the process dictionary for general state, but OTP itself uses it for exactly this shape of problem: `logger` metadata, `seq_trace`, and stdlib's own `error_logger` all use `put`/`get` to make state visible across call boundaries a functional threading model can't reach without threading it through every intervening function signature. **Adopted:** using the process dictionary as a narrow, single-purpose escape hatch rather than a general state mechanism — the functional threading remains the source of truth for every path except the one it structurally cannot cover.

### Haskell — `IORef` as an escape from pure threading

Haskell's `State` monad (cited in ADR 0041 as the model for `StateAcc`) is the general case; `IORef`/`STRef` exist precisely for the cases where purely functional threading can't reach across a boundary (callback into foreign code, FFI). **Adopted:** the same split — functional threading for the general case, a mutable cell for the one boundary case that needs it.

### This codebase — `beamtalk_actor.erl`'s `'$bt_actor_state'`

Already discussed above under Context; this ADR extends the same pattern to class variables and follows its cleanup discipline (`restore_dispatch_pdict/1`'s use of `erase/1` in an `after`-equivalent path).

## User Impact

### Newcomer
No visible syntax change. The trap described in `beamtalk-language-features.md` § Passing Blocks Through Class Methods disappears rather than needing to be learned. A newcomer writing a class method that mutates state and takes a block simply gets correct behavior.

### Smalltalk developer
Restores the expected Smalltalk invariant that a non-local return is ordinary control flow, not a special case that silently drops side effects — matching how `^` behaves everywhere else in the language (ADR 0041 already made this true for actor and value-type state; this closes the one remaining gap for class-side state).

### Erlang/BEAM developer
The process-dictionary shadow is a narrow, well-precedented pattern (see `beamtalk_actor.erl`) rather than a novel mechanism — reviewers familiar with OTP idioms for crossing callback boundaries will recognize it immediately. The `erase/1` discipline means it introduces no new leak surface.

### Production operator
No change to the hot dispatch path for the majority of class methods (no class vars, no new codegen emitted). For the minority that do mutate class vars, cost is one extra `put/2` per mutation — negligible relative to the existing `maps:put/3` threading cost they already pay. No new failure mode: genuine errors keep today's revert behavior exactly.

### Tooling developer (LSP, IDE)
No AST or type-level change; this is purely a runtime/codegen fix for existing, already-typed constructs. No new diagnostics are introduced (a static "this method mutates class vars and takes a Block" warning was considered in the original issue and rejected as producing more false positives than value, since that shape is common and legitimate — only `^`-through-the-block is rare).

## Steelman Analysis

### Best argument for Option B (full write-through — class vars always live in the shadow)

| Cohort | Their strongest argument |
|--------|---------------------------|
| **BEAM veteran** | "One mechanism is simpler than two. If class vars always live in the process dictionary, there's no functional/shadow split to keep in sync — `invoke_class_method/7` just reads the pdict, period." |
| **Language designer** | "This removes an entire category of 'did the shadow get written before the read' bugs. A single source of truth is more robust than two representations of the same state that must agree." |
| **Operator** | "Fewer code paths to reason about during an incident — one state model for class vars, not a functional one for the common case and a shadow for the rare one." |

### Why Option C (scoped shadow) wins despite the steelman

1. **The AC that genuine errors revert mutations is not free under Option B.** A durable, always-on write-through means a genuine error after a mutation also needs an explicit snapshot-and-restore step to get today's revert behavior back — Option B doesn't eliminate that complexity, it just moves it from "an extra `put` at mutation sites" to "a mandatory snapshot/restore around every class method call, including the ~100+ stdlib classes with no class vars at all." Option C's functional path already gives revert-on-error for free, because it changes nothing about the existing, working, tested behavior.
2. **Hot-path cost is not symmetric.** Option B's snapshot/restore must run for every class method invocation to preserve error-revert semantics, even for methods that never touch a class var — Option C only instruments methods that already pay the `ClassVars` threading cost.
3. **Smaller blast radius.** Option C adds one guarded clause to a single existing function (`invoke_class_method/7`) plus one codegen emission line at an existing site; no outcome type changes, and no other function in the dispatch module needs to change. Option B changes how every class method's state is represented at runtime, which is a much larger regression surface for something this narrow (BT-3032's own investigation already flagged this cost for what it called "Option 2").

### Tension point

BEAM veterans and language designers reasonably prefer Option B's conceptual simplicity; the deciding factor is that Option C gets the *same* observable fix with a fraction of the changed surface and none of the risk to the error-revert invariant, which the acceptance criteria treat as non-negotiable.

## Alternatives Considered

### Alternative A: Accept the limitation, document only

Promote the existing `beamtalk-language-features.md` note to a fuller worked example; close BT-3032 without a runtime fix.

**Rejected:** the pattern (mutate a class var, then hand a block to a method that invokes it indirectly) is a natural way to write a `Collection` subclass's `do:` delegating to a class-side helper — exactly the shape `docs/beamtalk-language-features.md` itself recommends elsewhere in the same section. Silent data loss in class state is a trust-eroding correctness bug even though the trigger is narrow; a fix that costs only the mutating class methods is affordable enough not to accept the limitation.

### Alternative B: Full write-through (class vars always live in the shadow)

See Steelman Analysis above. Rejected as strictly more invasive than necessary: it solves the same problem Option C solves, at the cost of adding snapshot/restore machinery to every class method call (to preserve error-revert semantics) rather than only the mutation sites that need it.

### Alternative D: Per-block-invocation-site wrapping

Wrap each place a class method's generated code invokes a block, catching a foreign NLR there and re-attaching the current `ClassVars` before re-throwing.

**Rejected** (already investigated in the BT-3032 issue itself): does not generalize. The repro's block reaches `aBlock value: x` through `beamtalk_collection:to_list/1` and `lists:foldl` inside `do:`'s own compiled implementation — there is no call site in the class method's own generated code to instrument. Any indirect invocation through library code defeats this approach.

### Alternative E: Compile-time diagnostic

Warn when a class method both mutates a class variable and takes a `Block` parameter.

**Rejected** (already investigated in the BT-3032 issue itself): that shape is common and legitimate (most block-taking class methods with class-var mutations never have the block's `^` escape); the warning would be wrong almost every time it fired.

### Alternative F: Wrap the continuation at each mutation site (pure codegen, no pdict)

Instead of a process-dictionary shadow, wrap the *continuation* after each top-level class-var mutation in a `try`/`catch` that intercepts any escaping exception, and — if it's a non-matching (`foreign`) NLR — re-throws it carrying the in-scope `ClassVarsN` as an auxiliary payload rather than the original tuple's own (unrelated) state field:

```erlang
let ClassVars1 = call 'maps':'put'('runs', Val, ClassVars0) in
try
    <rest of method body, using ClassVars1>
catch <Cls, Err, Stk> ->
    case {Cls, Err} of
      <{'throw', {'$bt_nlr', _T, _V, _S} = Nlr}> when 'true' ->
          primop 'raw_raise'('throw', {'$bt_nlr_with_cv', Nlr, ClassVars1}, Stk)
      <_> when 'true' ->
          primop 'raw_raise'(Cls, Err, Stk)
    end
end
```

This is a genuinely strong alternative: it never touches the process dictionary, so it has none of Option C's cross-process contamination risk (this ADR's own codegen fix above exists only because the pdict approach needed one) and composes with nesting for free (each mutation's `try` wraps a strictly smaller continuation than the last, so the innermost one to fire carries the most current `ClassVarsN`). The outermost `wrap_class_method_body_with_nlr_catch` would need to recognize the new `{'$bt_nlr_with_cv', Nlr, CV}` wrapper and use `CV` instead of `ClassVars` when relaying `Nlr` onward.

**Rejected in favor of Option C, but noted as the strongest alternative found during review:** it requires new `try`/`catch` scaffolding at *every* class-var mutation site (not just a one-line `put/2`), plus a second wrapper shape the relay path and every consumer of the NLR tuple must now recognize alongside the plain 4-tuple — more codegen surface than Option C for the same observable fix. If Option C's process-dictionary side channel proves troublesome in practice (e.g. the coupling noted in Consequences below), this is the fallback to revisit.

## Consequences

### Positive
- Class-var mutations made before a `^` escapes a class method survive the unwind, matching the existing correct behavior for value types and actor fields (ADR 0041) and for a class method's own `^` (BT-3022).
- Genuine errors continue to revert class-var mutations, unchanged.
- No hot-path cost for the majority of class methods (no class vars), and none for mutations made inside blocks (already excluded, matching their existing normal-return behavior).
- Reuses an existing, narrow, well-understood pattern already in the codebase (`'$bt_actor_state'`) rather than introducing a new state-management concept.
- The trap documented in `beamtalk-language-features.md` § Passing Blocks Through Class Methods is closed rather than merely better-documented, for the compiled-class-method case.
- The process dictionary dies with its process: a class gen_server crash and supervisor restart cannot carry a stale shadow into the fresh process, since `erlang:get/1` on a freshly-started process is always `undefined`.

### Negative
- Introduces a process-dictionary side channel, which is easy to misuse if a future change adds another write site without also erasing it on every exit path. Mitigated by scoping the write to a single codegen emission point (`generate_field_assignment`, gated on `block_depth == 0`) and the erase to a single `try ... after ... end` at the end of `invoke_class_method/7`.
- Two representations of "the current class-var mutation" exist simultaneously for the duration of a call that has one (the functional `ClassVarsN` binding and the shadow) — a future maintainer touching class-var codegen must know both need updating together, and that the shadow write must stay excluded inside blocks. A code comment at both emission sites should cross-reference this ADR.
- `invoke_class_method/7` gains a guard (`?IS_NLR(Error)`) whose correctness depends on `apply_class_method_fun/6` and `apply_compiled_class_method/7` continuing to route genuine errors and NLR passthroughs to separate `catch` clauses before either reaches this point — true today, but a future change to either function's exception handling could silently break the discrimination. Worth a code comment cross-referencing this ADR at both sites, not just the new one.
- Coverage is partial: only class methods compiled through `generate_field_assignment` are fixed. Class methods installed as runtime `ClassBuilder` funs (ADR 0084) or implemented directly in hand-written Erlang keep today's lossy behavior, since they never emit the shadow write. Not addressed by this ADR.

### Neutral
- No change to the NLR token/relay mechanism itself (`class_send_dispatch/3`, `metaclass_send_dispatch/4`, `class_self_dispatch/4`'s existing BT-3022 relays), only to which `ClassVars` value `invoke_class_method/7` hands back to the gen_server callback afterward.
- No change to the actor or value-type NLR paths — this is scoped to class methods only, since they are the only context with a separate `ClassVars` bucket distinct from the block's own `State`.
- No change to `class_method_outcome/0` or any of its other consumers — the fix is a single new guarded clause plus a `try ... after` erase, both inside `invoke_class_method/7`.

## Implementation

Affected components: codegen (`crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `generate_field_assignment`, the class-var mutation emission site) and runtime (`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` — `invoke_class_method/7` only).

1. Add the `'$bt_class_vars_shadow'` `put/2` emission immediately after the existing `let ClassVarsN = call 'maps':'put'(...) in` in `generate_field_assignment`'s class-var branch, gated on `self.in_class_method() && self.block_depth == 0 && self.class_var_names().contains(field_name)` — the `block_depth == 0` clause is new; the rest is the existing condition. No new analysis pass needed.
2. Restructure `invoke_class_method/7` to wrap its existing `case` in `try ... after erlang:erase('$bt_class_vars_shadow') end`, and add the new `{error, {raised, throw, Error, _ST}} when ?IS_NLR(Error)` clause ordered before the existing generic `{error, {raised, _ErrClass, Error, _ST}}` clause.
3. Regression tests in `stdlib/test/`: the repro from this ADR's Context section; a companion test asserting a genuine error after a mutation still reverts; a third asserting a self-dispatched (`self otherClassMethod:`) inherited method's mutation also survives a foreign-NLR relay; and a fourth asserting a mutation made *inside a block* passed to another class's method still behaves as it does today (discarded on normal return, not newly preserved) — to lock in the `block_depth == 0` scoping decision as intentional, not an oversight.

## References
- Related issues: BT-3032 (the issue this ADR resolves), BT-3022 (parent — fixed the value-return path, left class vars unfixed)
- Related ADRs: ADR 0041 (Universal State-Threading Block Protocol — establishes the 4-tuple NLR convention this ADR extends), ADR 0109 (Block-Scoped Class Methods — same "block runs in the class process" root context, different problem), ADR 0013 (Class Variables, Class-Side Methods, Instantiation — origin of `classState:`), ADR 0042 (Immutable Value Objects, Actor Mutable State — confirms class-level state lives in the class object's gen_server state, consistent with this ADR's mechanism)
- Documentation: `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` — `invoke_class_method/7` (the fix site), `apply_class_method_in_context/6`, `apply_class_method_fun/6`, `apply_compiled_class_method/7`, `class_send_dispatch/3`, `class_self_dispatch/4`, the `?IS_NLR/1` macro
- `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` — `dispatch_class_method/5` (turns `invoke_class_method/7`'s reply into the class gen_server's persisted `#class_state.class_state`)
- `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` — `unwrap_dispatch_result/1`, `restore_dispatch_pdict/1` (prior art for the shadow pattern)
- `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `generate_field_assignment` (the codegen fix site)
- `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — `nlr_arm_result`, `NlrBoundary::ClassMethod`, `wrap_class_method_body_with_nlr_catch`, `class_var_version`/`next_class_var` (the `class_var_mutated` gate)
