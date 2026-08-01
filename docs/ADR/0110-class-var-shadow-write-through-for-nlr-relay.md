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

`beamtalk_actor.erl` already solves a shaped-alike problem for actor state. `self_dispatch/2`'s `unwrap_dispatch_result/1` writes the actor's current state into the process dictionary (`put('$bt_actor_state', NewState)`) after every nested self-dispatch call, specifically so state mutated by a self-send doesn't need to be threaded functionally through every intermediate call frame; `restore_dispatch_pdict/1` cleans it up in an `after` block. This is the same shape of problem — state that a purely functional threading model cannot recover across a call boundary it doesn't control — solved with the same tool.

### Constraints

- A class method's own `^` (matching token) must keep working exactly as today — this path is correct and already tested (BT-3022).
- A genuine runtime error after a class-var mutation must still revert the mutation — this is the existing, desired, documented behavior and is one of BT-3032's acceptance criteria.
- The fix must not add overhead to class methods that never mutate class vars (the majority of the ~100+ stdlib classes).
- Class methods execute one-at-a-time inside their class's singleton `gen_server` — there is never more than one in-flight call per class process, so a single well-known process-dictionary key per class process is safe without additional synchronization.

## Decision

Add a **process-dictionary shadow write-through**, scoped narrowly to the one case that needs it: relaying a foreign NLR out of a class method that mutated class vars beforehand. The existing functional `ClassVars` threading is untouched for every other path.

### Codegen change

At each class-var mutation site (`self.foo := value`) inside a class method body, in addition to the existing functional `let ClassVarsN = ...` threading, emit a `put/2` of the current `ClassVars` map into a fixed process-dictionary key, e.g. `'$bt_class_vars_shadow'`:

```erlang
let ClassVars1 = call 'maps':'put'('runs', NewRuns, ClassVars0) in
let _ = call 'erlang':'put'('$bt_class_vars_shadow', ClassVars1) in
```

This only fires for class methods whose `BlockMutationAnalysis` already shows a class-var write — i.e., exactly the methods that already pay the `ClassVars` threading cost today. Methods with no class-var mutations emit nothing new.

### Runtime change

Split the outcome type `apply_class_method_in_context/6` already computes so the NLR-relay case (already distinguished in `apply_class_method_fun/6`'s catch clauses) is a distinct variant instead of being folded into `{error, {raised, ...}}`:

```erlang
-type class_method_outcome() ::
    test_spawn
    | {ok, term()}
    | {nlr_relay, term(), list()}        %% NEW: foreign non-local return, not a failure
    | {error, #beamtalk_error{}}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.
```

`invoke_class_method/7` handles the new variant by reading the shadow instead of reusing the pre-call `ClassVars`, then erasing the shadow so a stale value can never leak into an unrelated later call:

```erlang
{nlr_relay, Nlr, NlrST} ->
    NewClassVars =
        case erlang:erase('$bt_class_vars_shadow') of
            undefined -> ClassVars;   %% no mutation occurred before the relay
            Shadow -> Shadow
        end,
    %% store NewClassVars for the gen_server's own state before re-throwing Nlr
    {relay, Nlr, NlrST, NewClassVars};
{error, {raised, _ErrClass, Error, _ST}} ->
    erlang:erase('$bt_class_vars_shadow'),   %% genuine error: discard any shadowed mutation
    {reply, {error, Error}, ClassVars}
```

(The exact call-site shape of "reply vs. re-throw" for the relay case mirrors what `class_send_dispatch/3` already does for BT-3022 — this ADR does not change that relay mechanism, only which `ClassVars` value the gen_server retains afterward.)

The shadow key is always erased before the call returns, on every path — normal return, own-token NLR, foreign-NLR relay, and genuine error — so it never survives past a single `class_method_call`.

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
3. **Smaller blast radius.** Option C changes one outcome variant and one codegen emission site; Option B changes how every class method's state is represented at runtime, which is a much larger regression surface for something this narrow (BT-3032's own investigation already flagged this cost for what it called "Option 2").

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

## Consequences

### Positive
- Class-var mutations made before a `^` escapes a class method survive the unwind, matching the existing correct behavior for value types and actor fields (ADR 0041) and for a class method's own `^` (BT-3022).
- Genuine errors continue to revert class-var mutations, unchanged.
- No hot-path cost for the majority of class methods (no class vars).
- Reuses an existing, narrow, well-understood pattern already in the codebase (`'$bt_actor_state'`) rather than introducing a new state-management concept.
- The trap documented in `beamtalk-language-features.md` § Passing Blocks Through Class Methods is closed rather than merely better-documented.

### Negative
- Introduces a process-dictionary side channel, which is easy to misuse if a future change adds another write site without also erasing it on every exit path. Mitigated by scoping the write to a single codegen emission point and the erase to a single runtime choke point (`invoke_class_method/7`), analogous to `restore_dispatch_pdict/1`'s discipline.
- Two representations of "the current class-var mutation" exist simultaneously for the duration of a call that has one (the functional `ClassVarsN` binding and the shadow) — a future maintainer touching class-var codegen must know both need updating together. A code comment at both emission sites should cross-reference this ADR.
- Adds one new outcome variant (`{nlr_relay, ...}`) to `class_method_outcome/0`, which both `invoke_class_method/7` and `class_self_dispatch/4` (the two consumers noted in `apply_class_method_in_context/6`'s docstring) must handle.

### Neutral
- No change to the NLR token/relay mechanism itself (`class_send_dispatch/3`'s existing BT-3022 relay), only to which `ClassVars` value is retained by the gen_server afterward.
- No change to the actor or value-type NLR paths — this is scoped to class methods only, since they are the only context with a separate `ClassVars` bucket distinct from the block's own `State`.

## Implementation

Affected components: codegen (`crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — class-var mutation emission) and runtime (`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` — `apply_class_method_in_context/6`, `invoke_class_method/7`, `class_self_dispatch/4`).

1. Add the `'$bt_class_vars_shadow'` `put/2` emission at class-var mutation sites in class methods, gated on the method already having class-var writes (no new analysis pass needed — `BlockMutationAnalysis` already tracks this).
2. Add the `{nlr_relay, Nlr, NlrST}` outcome variant to `class_method_outcome/0`; update `apply_class_method_fun/6` and `apply_compiled_class_method/7`'s existing `throw:Nlr:NlrST when ?IS_NLR(Nlr)` clauses to produce it instead of folding into `{error, {raised, ...}}`.
3. Update `invoke_class_method/7` to read-and-erase the shadow on `{nlr_relay, ...}`, and to erase (without reading) the shadow on every other outcome.
4. Update `class_self_dispatch/4` (the self-send path, which shares `apply_class_method_in_context/6` per its BT-2007 docstring) to handle the new variant identically.
5. Regression test in `stdlib/test/` using the repro from this ADR's Context section, plus a companion test asserting a genuine error after a mutation still reverts.

## References
- Related issues: BT-3032 (the issue this ADR resolves), BT-3022 (parent — fixed the value-return path, left class vars unfixed)
- Related ADRs: ADR 0041 (Universal State-Threading Block Protocol — establishes the 4-tuple NLR convention this ADR extends), ADR 0109 (Block-Scoped Class Methods — same "block runs in the class process" root context, different problem)
- Documentation: `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` — `invoke_class_method/7`, `apply_class_method_in_context/6`, `apply_class_method_fun/6`, `class_send_dispatch/3`
- `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` — `unwrap_dispatch_result/1`, `restore_dispatch_pdict/1` (prior art for the shadow pattern)
- `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — `nlr_arm_result`, `NlrBoundary::ClassMethod`, `wrap_class_method_body_with_nlr_catch`
