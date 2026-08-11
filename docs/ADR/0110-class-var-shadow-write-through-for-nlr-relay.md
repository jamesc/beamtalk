# ADR 0110: Class-Variable Shadow Write-Through for Foreign NLR Relay

## Status
Implemented (2026-08-03)

## Implementation Tracking

**Epic:** BT-3035
**Issues:** BT-3036 (runtime outcome variant + shadow read/erase) → BT-3037 (codegen emission + BUnit regression suite) → BT-3038 (docs + e2e close-out)
**Status:** Done

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

Add a **process-dictionary shadow write-through**, scoped narrowly to the one case that needs it: relaying a foreign NLR out of a class method that mutated class vars beforehand. The existing functional `ClassVars` threading is untouched for every other path, and the relay mechanism (`class_send_dispatch/3`, `metaclass_send_dispatch/4`, `class_self_dispatch/4`) is unchanged. The runtime diff is confined to one module: a new `{nlr_relay, ...}` variant in `class_method_outcome()`, produced by the two catch clauses that already distinguish a relayed `^` from a genuine error, and consumed by `invoke_class_method/7` — the one place that decides what `ClassVars` value the class gen_server retains.

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

**Amendment (2026-08-04, BT-3039):** the single well-known key `'$bt_class_vars_shadow'` is not enough. A block literal invoked from a *different* class's process (exactly the case the `block_depth == 0` gate above was already written to worry about) can itself contain a **mutating self-send** — `self bump` where `self` is the block's captured home-class identity, not the process it happens to execute in. `bump`'s own top-frame mutation (block_depth resets to 0 on entry to its own method body) writes the shadow too, under the *same* global key, physically inside the foreign process — clobbering that process's own class's shadow write before its `invoke_class_method/7` reads it back. The fix: key the write by the *dynamic* runtime identity of `self` for this invocation, not a single shared key:

```erlang
let _ = call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1) in
```

`element(2, ClassSelf)` (the `#beamtalk_object.class` field) is used rather than a class name baked in statically at compile time: a method's `self.class_name()` at codegen time is fixed to the class where the method is *defined*, but `ClassSelf` — already threaded dynamically as this call's `class = beamtalk_class_registry:class_object_tag(ClassName)`, per the Runtime change below — correctly reflects the *calling* class for an inherited self-dispatch chain (`self otherClassMethod:` inherited from an ancestor still tags its shadow write with the subclass's own identity, not the ancestor's) while also correctly separating a block's foreign captured self (tagged with its own home class) from the process it executes in. A static class-name tag would get the inherited case wrong; the dynamic `ClassSelf` field gets both cases right for the same reason it already exists — see the `class_mod`/`class` split noted in the Runtime change below.

**`ClassBuilder` class methods are covered by the same emission.** Runtime-installed class-method funs (ADR 0084) are not a separate implementation: `generate_class_method_fun_from_block` (`gen_server/methods.rs`) lowers each `classMethods:` block through the *shared* class-method body path, with `enter_builder_class_method_context` setting `in_class_method = true` and the cascade's `classVars:` keys as `class_var_names` — so `generate_field_assignment`'s class-var branch, and with it the shadow write, fires inside builder funs exactly as in compiled methods. One adjustment is required for the gate to hold there: `generate_class_method_fun_from_block` already resets `class_var_version` and `class_var_mutated` on entry (the fun body is a fresh method frame); it must also save/reset/restore `block_depth` the same way, because the builder cascade is itself an expression that may lexically sit inside a block (`block_depth > 0` at the cascade's position) even though the fun body executes at runtime as a class method's own top frame. Without that reset, a builder cascade written inside a block would silently lose the shadow write; with it, "`block_depth == 0`" uniformly means "the method's own top frame" across both compilation paths.

**Scope boundary:** the one path not covered is a class method implemented directly in hand-written Erlang that mutates class vars by returning `{class_var_result, ...}` itself. As of this ADR, no such method exists — no module under `beamtalk_stdlib/src` or `beamtalk_runtime/src` *produces* `class_var_result` (only the dispatch layer and `beamtalk_supervisor` consume it) — so this is an FFI authoring rule, not a live gap: an Erlang-implemented class method that mutates class vars and can have a foreign NLR pass through it should also `put('$bt_class_vars_shadow', NewCV)` at its mutation points. Record this in `docs/development/erlang-guidelines.md`.

### Runtime change

The relay-vs-genuine-error distinction is *already made* in exactly two places — the `throw:Nlr:NlrST when ?IS_NLR(Nlr)` catch clauses of `apply_class_method_fun/6` and `apply_compiled_class_method/7` (both added for BT-3022) — and then immediately erased by folding both into the same `{error, {raised, ...}}` shape. Instead of re-inferring the distinction downstream from tuple shape (fragile: it would silently break if those catch clauses were ever consolidated), carry it in the outcome type. Those two clauses change from `{error, {raised, throw, Nlr, NlrST}}` to a new variant:

```erlang
-type class_method_outcome() ::
    test_spawn
    | {ok, term()}
    %% BT-3032: a foreign `^` passing through — control flow to relay, not a failure.
    | {nlr_relay, term(), list()}
    | {error, #beamtalk_error{}}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.
```

`invoke_class_method/7` handles the new variant by reading the shadow, and wraps everything in `try ... after` so the shadow is erased on every path:

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
            %% BT-3032: a foreign `^` relay is not a failure of *this* method —
            %% recover ClassVars mutated before the relay from the shadow written
            %% by generate_field_assignment, instead of reverting to ClassVars
            %% as it stood before this call. The reply shape is unchanged, so
            %% class_send_dispatch/3's existing `{error, Nlr} when ?IS_NLR(Nlr)`
            %% re-throw clause (BT-3022) works untouched.
            {nlr_relay, Nlr, _ST} ->
                %% BT-3039 amendment: keyed by this call's own class tag so a
                %% foreign class's shadow write (see amendment above) can never
                %% be read back here.
                ShadowKey = {'$bt_class_vars_shadow', beamtalk_class_registry:class_object_tag(ClassName)},
                NewClassVars =
                    case erlang:get(ShadowKey) of
                        undefined -> ClassVars;   %% no mutation occurred before the relay
                        Shadow -> Shadow
                    end,
                {reply, {error, Nlr}, NewClassVars};
            {error, #beamtalk_error{} = Error} ->
                {reply, {error, Error}, ClassVars};
            {error, undef_in_body} ->
                {reply, {error, undef}, ClassVars};
            {error, {raised, _ErrClass, Error, _ST}} ->
                %% Genuine error: revert, exactly as today.
                {reply, {error, Error}, ClassVars}
        end
    after
        erlang:erase({'$bt_class_vars_shadow', beamtalk_class_registry:class_object_tag(ClassName)})
    end.
```

The other consumer of `class_method_outcome()`, `unwrap_self_dispatch_outcome/3`, gains a clause with behavior identical to how those throws unwind today — currently there is no throw-specific clause; the generic `{error, {raised, ErrClass, Error, ST}} -> erlang:raise(ErrClass, Error, ST)` catch-all handles them, binding `ErrClass = throw` for a relayed NLR:

```erlang
{nlr_relay, Nlr, ST} ->
    erlang:raise(throw, Nlr, ST);
```

The explicit variant means dialyzer sees the relay case as part of the contract: a future refactor of either apply function's exception handling cannot silently collapse relay into error — the variant would have to be deliberately removed, which is loud, not silent.

The `after` clause — not a bare trailing statement — is required for the erase: `apply_class_method_in_context/6` has an un-`try`'d prelude (`beamtalk_class_registry:class_object_tag/1`, `lookup_class_method_fun/2`, `is_test_execution_selector/1`) that could in principle raise before the `case` ever completes, and the class gen_server is long-lived, so any path that skipped the erase would leave a stale shadow for the *next unrelated call* to read. `try ... after ... end` guarantees the erase runs regardless.

This guarantees the shadow never survives past a single `class_method_call`/`metaclass_method_call`, so a later, unrelated call on the same class gen_server can never read a stale value. `invoke_class_method/7` is always the outermost frame for a given external call: `class_self_dispatch/4` and `class_self_dispatch_local/4` (the self-send path) call `apply_class_method_in_context/6` directly and run in the same process without a new gen_server hop, so a chain of self-dispatched calls within one external call writes the shadow sequentially (each mutation overwrites the previous value) and a foreign NLR relayed through any depth of that chain still unwinds to this same `try` before the call returns. No per-nesting-level save/restore is needed — unlike `beamtalk_actor.erl`'s `'$bt_actor_state'`, which restores the pre-call value in its `after` because instance self-dispatch exposes "the currently live state" *during* the chain for other code to read; here nothing reads the shadow until after the whole chain has finished, so straight overwrite-and-erase-once-at-the-end is sufficient. Cross-class self-dispatch is impossible by construction (`class_self_dispatch` only walks one class's own superclass chain), and different classes are different gen_server processes with independent process dictionaries, so there is no cross-class shadow collision through self-dispatch *when the process a class method runs in is that class's own gen_server*.

**Amendment (2026-08-04, BT-3039):** that premise doesn't hold for a block. A block literal is lexically part of one class's method but, per ADR 0109, executes in whichever process *invokes* it — which can be a different class's gen_server entirely. A mutating self-send inside such a block (`self bump`, `self`'s captured home class) then runs `class_self_dispatch`'s target method body physically inside the *foreign* process, and that body's own top-frame shadow write (see the Codegen change amendment above) was landing in the single global key — the exact collision this paragraph said couldn't happen, just reached by a different route than a literal cross-class `class_self_dispatch` call. The class-keyed shadow (Codegen change amendment) closes this: the foreign write is now tagged with its own class's identity, so it can never be read back by the process's *own* `invoke_class_method/7`, which only ever reads its own class-tagged key. The foreign entry itself is simply never read — a bounded, harmless stale process-dictionary entry (bounded by the number of classes that have ever passed a mutating block into this one, i.e. by the total class count in the running system) that lives until this gen_server restarts. See Consequences below.

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

1. **Option B's runtime side is cheap, but its codegen side is not.** To be honest about it: revert-on-error would be nearly free under Option B too — the pre-call `ClassVars` is already a bound variable at the dispatch choke point, so "restore on genuine error" is just replying with it, and the per-call `put`/`get` cost is noise next to the gen_server hop every class-method call already pays. The real cost of B is the **codegen migration**: every class-var read and write site, in compiled methods and `ClassBuilder` funs alike, changes representation, and the entire threading machinery (`ClassVars{N}` versioning, `class_var_result` unwrapping at self-send sites, the class-method NLR state slot) has to be dismantled or kept in sync during the transition. That is an L-sized refactor of working, tested code for the same observable fix.
2. **Option B has a semantics edge Option C doesn't touch.** A block created in class P's method that *reads* a class var currently captures the threaded map value — a creation-time snapshot that stays correct wherever the block later runs. Under pdict-resident class vars, a naive read-compiles-to-`get` would read whatever process the block happens to execute in (wrong class, or no class at all). Preserving today's snapshot semantics for block-captured reads is solvable but is exactly the kind of subtle, cross-process regression surface this narrow bug doesn't justify opening.
3. **Smaller blast radius.** Option C is one new outcome variant, two one-line catch-clause changes, one new clause plus a `try/after` in `invoke_class_method/7`, one clause in `unwrap_self_dispatch_outcome/3`, and two codegen touch-points (the emission line, and a `block_depth` reset in the builder-fun lowering) — all in existing functions, none changing how state is represented. Option B changes how every class method's state is represented at runtime (BT-3032's own investigation already flagged this cost for what it called "Option 2").

### Tension point

BEAM veterans and language designers reasonably prefer Option B's conceptual simplicity; the deciding factor is that Option C gets the *same* observable fix with a fraction of the changed surface and none of the risk to the error-revert invariant, which the acceptance criteria treat as non-negotiable.

## Alternatives Considered

### Alternative A: Accept the limitation, document only

Promote the existing `beamtalk-language-features.md` note to a fuller worked example; close BT-3032 without a runtime fix.

**Rejected:** the pattern (mutate a class var, then hand a block to a method that invokes it indirectly) is a natural way to write a `Collection` subclass's `do:` delegating to a class-side helper — exactly the shape `docs/beamtalk-language-features.md` itself recommends elsewhere in the same section. Silent data loss in class state is a trust-eroding correctness bug even though the trigger is narrow; a fix that costs only the mutating class methods is affordable enough not to accept the limitation.

### Alternative B: Full write-through (class vars always live in the shadow)

See Steelman Analysis above. Rejected not for runtime cost (revert-on-error and the per-call `put`/`get` are both cheap at the dispatch choke point) but for migration size and semantics risk: it replaces the entire class-var threading representation across compiled methods and `ClassBuilder` funs, and it opens a block-captured-read semantics edge (creation-time snapshot vs. executing-process pdict) that Option C never touches. It would, however, also cover hand-written Erlang class methods for free and delete the threading machinery long-term — if class-var-heavy code becomes common enough that the threading machinery itself is a maintenance burden, B is the direction to revisit.

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
- The trap documented in `beamtalk-language-features.md` § Passing Blocks Through Class Methods is closed rather than merely better-documented — for compiled classes and `ClassBuilder`-defined classes alike, since both lower class-var assignment through the same emission site.
- The process dictionary dies with its process: a class gen_server crash and supervisor restart cannot carry a stale shadow into the fresh process, since `erlang:get/1` on a freshly-started process is always `undefined`.

### Negative
- Introduces a process-dictionary side channel, which is easy to misuse if a future change adds another write site without also erasing it on every exit path. Mitigated by scoping the write to a single codegen emission point (`generate_field_assignment`, gated on `block_depth == 0`) and the erase to a single `try ... after ... end` at the end of `invoke_class_method/7`.
- Two representations of "the current class-var mutation" exist simultaneously for the duration of a call that has one (the functional `ClassVarsN` binding and the shadow) — a future maintainer touching class-var codegen must know both need updating together, and that the shadow write must stay excluded inside blocks. A code comment at both emission sites should cross-reference this ADR.
- The `block_depth == 0` gate's meaning ("the method's own top frame") holds for `ClassBuilder` funs only because `generate_class_method_fun_from_block` resets `block_depth` on entry — a save/reset/restore that must ride alongside the existing `class_var_version`/`class_var_mutated` resets there. Forgetting it in a future refactor would silently disable the fix for builder classes defined inside blocks; the builder-class regression test below exists to catch exactly that.
- A class method implemented directly in hand-written Erlang that mutates class vars would need to write the shadow itself (an FFI authoring rule, recorded in `docs/development/erlang-guidelines.md`). No such method exists in-tree today, so this is a documented obligation, not a live gap.
- **Amendment (2026-08-04, BT-3039):** a mutating self-send inside a block that executes in a foreign class's process (see amendments above) leaves its class-tagged shadow entry behind in that foreign process's dictionary — nothing ever reads or erases it, since `invoke_class_method/7` only ever touches its own class's key. This is bounded (at most one stale entry per distinct class that has ever passed such a block into this one — bounded by the total class count in the running system) and cleared on the next gen_server restart; it is not accumulated per-call. Not actively cleaned up, on the same proportionality basis as the rest of this ADR's scoped fix: closing the corruption is the requirement, not eliminating every stray pdict entry.
- **Amendment (2026-08-11, BT-3140):** investigated whether a class-var mutation made *inside* a `whileTrue:`/`timesRepeat:` loop body (rather than at the method's top frame, before the block_depth == 0 gate) survives a foreign NLR escaping mid-loop. It does not reach this ADR's mechanism at all — and the reason is not the `block_depth == 0` gate (loop bodies never increment `block_depth`; they are compiled by a wholly separate state-threading path, `control_flow::generate_threaded_loop_body`, not `generate_block`). The actual gap: that path's field-assignment codegen (`generate_field_assignment_open`, `dispatch_codegen.rs`) threads writes through the loop's own `State`/`StateAcc` map — built for Actor instance state and `ValueType` `Self` threading — and has no class-var branch at all, unlike `generate_field_assignment`. A class-var write reached through it silently threads into the loop's own scratch state map instead of `ClassVars`, and that map is discarded once the loop finishes — losing the mutation **identically on both normal return and a foreign NLR escape** (confirmed by inspecting the generated Core Erlang and running both cases: no asymmetry, just total loss; codegen unit tests: `test_class_var_mutation_in_while_loop_body_is_compile_error` and friends in `tests/gen_server.rs`). This is now a compile-time error (`CodeGenError::ClassVarAssignmentInThreadedBody`) rather than a silent runtime no-op, mirroring BT-2792's `FieldAssignmentInUnsupportedBlock` for the same underlying category of bug ("this specific state shape can't thread back correctly here"). Note this gap is reached only when the loop body has *some other* mutation (a local counter/accumulator, a self-send) that legitimately triggers `needs_mutation_threading` — BT-1346 already excludes bare class-var-only bodies from loop-threading in a class method, so those already hit `FieldAssignmentInUnsupportedBlock` via the ordinary stored-closure path.
- **Amendment (2026-08-11, BT-3150):** a self-send to a same-class class method (`self bump`) used as a bare statement inside a `whileTrue:`/`timesRepeat:` loop body previously produced a `core_parse_error` — a doubled `in in` around the self-send's `{class_var_result, ...}` tuple-unwrapping, from `emit_class_var_result_unwrap`'s open let-chain being re-wrapped by the loop body's naive `let _ = <expr> in` statement sequencing (`generate_threaded_loop_body_inner`, `control_flow/mod.rs`). Fixing only the syntax was prototyped and rejected: the mutation is silently discarded by the time the loop finishes (confirmed empirically — a `bump`-based counter stayed at 0 across 3 iterations instead of accumulating), because `ClassVarsN` is never threaded through the loop's recursive tail call the way `StateAcc` is — the self-send analog of BT-3140's finding for direct field writes just above, and reached the same way: only when the loop body has some other mutation (a local counter/accumulator) that legitimately triggers state threading in the first place, since a bare self-send-only body never reaches `generate_threaded_loop_body` at all. Rejected at compile time instead (`CodeGenError::ClassMethodSelfSendInThreadedLoopBody`), unconditionally on any class-method self-send inside a `whileTrue:`/`timesRepeat:`/`to:do:`/`to:by:do:` (`BodyKind::Letrec`) loop body — not just ones provably mutating a class var, since every same-class self-send routes through the same open-scope unwrap convention regardless of the callee's actual effect, and the caller can't know that statically. Deliberately scoped to `Letrec` only, not any `BodyKind::Foldl*` construct (`do:`/`collect:`/`select:`/`inject:into:`/...): a wider rejection was tried twice during review and reverted both times after breaking a real, pre-existing stdlib fixture (`stdlib/test/fixtures/class_method_block.bt`, BT-2350) that relies on pure self-sends as (or within) a fold's own return value, including in *discarded* statement positions — unlike `Letrec`, whose body value is unconditionally `nil` regardless of its last statement, so a self-send there can only ever be for a side effect. The identical class-var-mutation-loss bug is reachable via `Foldl*` bodies too (confirmed empirically for `do:`), left open and tracked under BT-3151 pending either real `ClassVars` fold-threading or static purity analysis of the self-sent callee.

### Neutral
- No change to the NLR token/relay mechanism itself (`class_send_dispatch/3`, `metaclass_send_dispatch/4`, `class_self_dispatch/4`'s existing BT-3022 relays), only to which `ClassVars` value `invoke_class_method/7` hands back to the gen_server callback afterward. The relay reply shape (`{error, Nlr}`) is byte-identical to today's.
- No change to the actor or value-type NLR paths — this is scoped to class methods only, since they are the only context with a separate `ClassVars` bucket distinct from the block's own `State`.
- `class_method_outcome/0` gains the `{nlr_relay, term(), list()}` variant; its two consumers (`invoke_class_method/7`, `unwrap_self_dispatch_outcome/3`) each gain one clause. This is deliberate: carrying the relay/error distinction in the type makes it dialyzer-checked rather than re-inferred from tuple shape, so a future consolidation of the apply functions' catch clauses fails loudly instead of silently reverting the fix.

## Implementation

Affected components: codegen (`crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `generate_field_assignment`; `gen_server/methods.rs` — `generate_class_method_fun_from_block`) and runtime (`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` only).

1. Add the `'$bt_class_vars_shadow'` `put/2` emission immediately after the existing `let ClassVarsN = call 'maps':'put'(...) in` in `generate_field_assignment`'s class-var branch, gated on `self.in_class_method() && self.block_depth == 0 && self.class_var_names().contains(field_name)` — the `block_depth == 0` clause is new; the rest is the existing condition. No new analysis pass needed.
2. In `generate_class_method_fun_from_block`, save/reset/restore `block_depth` alongside the existing `class_var_version`/`class_var_mutated` resets, so the gate in step 1 fires correctly inside `ClassBuilder` class-method funs regardless of where the cascade lexically sits.
3. Add the `{nlr_relay, term(), list()}` variant to `class_method_outcome()`; change the two `throw:Nlr:NlrST when ?IS_NLR(Nlr)` catch clauses (`apply_class_method_fun/6`, `apply_compiled_class_method/7`) to produce it.
4. In `invoke_class_method/7`: wrap the `case` in `try ... after erlang:erase('$bt_class_vars_shadow') end` and add the `{nlr_relay, Nlr, _ST}` clause (shadow read, reply `{error, Nlr}` with the recovered class vars). In `unwrap_self_dispatch_outcome/3`: add `{nlr_relay, Nlr, ST} -> erlang:raise(throw, Nlr, ST)`.
5. Record the FFI authoring rule (Erlang-implemented class methods that mutate class vars must shadow-write) in `docs/development/erlang-guidelines.md`, and update the `beamtalk-language-features.md` § Passing Blocks Through Class Methods caveat to reflect the fix.
6. Regression tests in `stdlib/test/`: the repro from this ADR's Context section; a companion test asserting a genuine error after a mutation still reverts; a third asserting a self-dispatched (`self otherClassMethod:`) inherited method's mutation also survives a foreign-NLR relay; a fourth asserting a mutation made *inside a block* passed to another class's method still behaves as it does today (discarded on normal return, not newly preserved) — locking in the `block_depth == 0` scoping as intentional; and a fifth running the Context repro against a `ClassBuilder`-defined class (including one defined inside a block) to pin the builder-fun coverage and the step-2 `block_depth` reset.

## References
- Related issues: BT-3032 (the issue this ADR resolves), BT-3022 (parent — fixed the value-return path, left class vars unfixed), BT-3039 (2026-08-04 amendment — class-keyed shadow closing the cross-class contamination hole found in BT-3037 review)
- Related ADRs: ADR 0041 (Universal State-Threading Block Protocol — establishes the 4-tuple NLR convention this ADR extends), ADR 0109 (Block-Scoped Class Methods — same "block runs in the class process" root context, different problem), ADR 0013 (Class Variables, Class-Side Methods, Instantiation — origin of `classState:`), ADR 0042 (Immutable Value Objects, Actor Mutable State — confirms class-level state lives in the class object's gen_server state, consistent with this ADR's mechanism)
- Documentation: `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` — `invoke_class_method/7` (the fix site), `apply_class_method_in_context/6`, `apply_class_method_fun/6`, `apply_compiled_class_method/7`, `class_send_dispatch/3`, `class_self_dispatch/4`, the `?IS_NLR/1` macro
- `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` — `dispatch_class_method/5` (turns `invoke_class_method/7`'s reply into the class gen_server's persisted `#class_state.class_state`)
- `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` — `unwrap_dispatch_result/1`, `restore_dispatch_pdict/1` (prior art for the shadow pattern)
- `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `generate_field_assignment` (the codegen fix site)
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` — `generate_class_method_fun_from_block`, `enter_builder_class_method_context` (why `ClassBuilder` funs share the fix, and the `block_depth` reset they need)
- `docs/development/erlang-guidelines.md` — FFI authoring rule for Erlang-implemented class methods (added by this ADR's implementation)
- `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — `nlr_arm_result`, `NlrBoundary::ClassMethod`, `wrap_class_method_body_with_nlr_catch`, `class_var_version`/`next_class_var` (the `class_var_mutated` gate)
