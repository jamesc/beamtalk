# ADR 0041: Universal State-Threading Block Protocol

## Status
Accepted (2026-02-24)

## Context

### The Composability Problem

Beamtalk compiles to Core Erlang, which runs on the BEAM — a platform with no mutable variables. When Beamtalk code mutates a variable inside a block (closure), the compiler must **thread state** through the generated code: packing mutated variables into a `StateAcc` map, passing it through recursive calls, and unpacking afterward.

Today, this state threading is gated by a **hardcoded whitelist** of ~20 control-flow selectors in `is_control_flow_selector()`:

```text
whileTrue:  whileFalse:  timesRepeat:  do:  collect:
select:     reject:      ensure:       to:do:  to:by:do:
inject:into:  on:do:  ifTrue:  ifFalse:  ifNil:  ifNotNil:
ifTrue:ifFalse:  ifNil:ifNotNil:
```

Only literal blocks passed to these selectors receive state threading. User-defined higher-order methods (HOMs — methods that accept block arguments, such as custom iterators or visitors) do not participate — mutations in blocks passed to custom methods **silently fail to propagate**.

### Example of the Failure

```smalltalk
"Works — whileTrue: is whitelisted"
count := 0.
[count < 10] whileTrue: [count := count + 1].
count  "=> 10"

"Fails — myCustomLoop: is NOT whitelisted"
MyCollection >> myCustomLoop: aBlock
  self do: [:each | aBlock value: each].

count := 0.
items myCustomLoop: [:x | count := count + x].
count  "=> 0 — mutations lost!"
```

This breaks Smalltalk's fundamental assumption that blocks are composable. A library author cannot write custom iteration methods that work with mutating blocks, even though equivalent stdlib methods work fine.

### Current Architecture

The state-threading pipeline has four stages:

1. **Semantic analysis** (`block_context.rs`): `is_control_flow_selector()` classifies blocks into `ControlFlow`, `Stored`, `Passed`, `Other`, or `Unknown` contexts based on a hardcoded whitelist.

2. **Block mutation analysis** (`block_analysis.rs`): `BlockMutationAnalysis` tracks `local_reads`, `local_writes`, `captured_reads`, `field_reads`, `field_writes`, and `has_self_sends` per block. `needs_mutation_threading()` decides whether to use stateful codegen.

3. **Control-flow codegen** (`while_loops.rs`, `counted_loops.rs`, `list_ops.rs`): Each whitelisted selector has a dedicated codegen path that generates tail-recursive loops with pack/unpack/restore using `StateAcc` maps. The loop signature is `fun(StateAcc) -> {Result, StateAcc}`.

4. **Non-local returns** (`value_type_codegen.rs`, `mod.rs`): `^` inside blocks throws `{'$bt_nlr', Token, Value}` (value types) or `{'$bt_nlr', Token, Value, State}` (actors). Methods with blocks containing `^` are wrapped in try/catch.

### The Two-Convention Problem

The current system uses **two different calling conventions** for blocks:

- **Whitelisted control flow:** `fun(StateAcc) -> {Result, NewStateAcc}` — state is threaded
- **Everything else:** `fun(Args) -> Result` — plain closure, no state

This duality means:
- Every new collection method must be added to the whitelist
- User-defined HOMs can never participate in state threading
- The `BlockContext` enum (`ControlFlow`/`Stored`/`Passed`) exists solely to route between these two paths
- Two parallel codegen paths (pure and stateful) must be maintained for every control-flow construct

## Decision

Replace the hardcoded control-flow whitelist with a **universal block protocol** where all blocks that capture mutable state use `fun(Args..., StateAcc) -> {Result, NewStateAcc}`. This makes state threading a first-class part of the block calling convention rather than a special case for known selectors.

### The Universal Block Calling Convention

**All blocks** use the stateful signature when passed to user-defined HOMs or stored in variables:

```erlang
fun(Arg1, ..., ArgN, StateAcc) -> {Result, NewStateAcc}
```

- `StateAcc` is an Erlang map containing the current values of all captured mutable variables (local variables and fields)
- The block returns a 2-tuple: `{Result, NewStateAcc}` where `NewStateAcc` reflects any mutations
- State keys use prefixed names: `'__local__varName'` for locals, `'field__fieldName'` for fields
- For pure blocks (no mutations), `StateAcc` passes through unchanged: `{Result, StateAcc}`

This uniform convention means **callers never need to know whether a block mutates**. Every block has the same calling interface. A HOM like `eachPair:` always passes `StateAcc` as the final argument and always destructures `{Result, NewState}` from the return — regardless of what the block does internally.

### Two-Tier Optimization

The uniform convention applies to all blocks at **unknown call sites** (HOMs, stored blocks, cross-module calls). At **known inline call sites** — where the compiler controls both the block and the caller — the compiler may optimize away the protocol:

- **Known inline sites** (current whitelisted control flow: `do:`, `collect:`, `whileTrue:`, etc.): The compiler generates pack/unpack scaffolding directly. Pure blocks at these sites can skip the state parameter entirely, since the compiler generates both the block and the loop.
- **Unknown call sites** (user-defined HOMs, stored blocks, cross-module calls): All blocks use the `fun(Args..., StateAcc) -> {Result, NewStateAcc}` convention uniformly.

This two-tier approach preserves zero-overhead codegen for the hot path (stdlib iteration) while enabling universal composability for user-defined HOMs. The key distinction: the **whitelist no longer gates whether state threading happens** — it only serves as an optimization hint for inlineable call sites.

### Caller-Side Protocol

When a method receives a block argument (a HOM), the compiler generates code that:

1. **Passes `StateAcc` as the final argument** to every block invocation
2. **Destructures `{Result, NewState}`** from the return
3. **Threads `NewState` to the next invocation** (in loops) or returns it (for single calls)

```erlang
%% Compiled body of: eachPair: aBlock
%% The compiler always generates the stateful calling convention
'eachPair:'/3 = fun (Self, ABlock, State) ->
    letrec 'loop'/2 = fun (I, StateAcc) ->
        case call 'erlang':'<'(I, call 'beamtalk_dispatch':'dispatch'(Self, 'size', [])) of
          <'true'> when 'true' ->
            let A = call 'beamtalk_dispatch':'dispatch'(Self, 'at:', [I]) in
            let B = call 'beamtalk_dispatch':'dispatch'(Self, 'at:', [call 'erlang':'+'(I, 1)]) in
            let {_Result, StateAcc1} = apply ABlock (A, B, StateAcc) in
            apply 'loop'/2 (call 'erlang':'+'(I, 2), StateAcc1)
          <'false'> when 'true' -> {'nil', StateAcc}
        end
    in apply 'loop'/2 (0, State)
```

The block argument `ABlock` is always called with `StateAcc` and always returns `{Result, NewState}`. This is true whether `ABlock` mutates state or not — a pure block simply returns `{Result, StateAcc}` unchanged.

### Erlang Interop Boundary

When a Beamtalk block is passed to Erlang/Elixir code (which doesn't know about the state protocol), the compiler generates a **wrapper fun** that strips the protocol:

```erlang
%% Beamtalk block: fun(X, StateAcc) -> {X + 1, StateAcc}
%% Wrapped for Erlang: fun(X) -> X + 1
%% The wrapper captures CurrentStateAcc at creation time so reads succeed,
%% but mutations are dropped (Erlang cannot propagate the updated state).
let ErlangFun = fun (X) ->
    let {Result, _State} = apply BeamtalkBlock (X, CurrentStateAcc) in
    Result
end
```

The wrapper captures the enclosing `CurrentStateAcc` so that stateful blocks can read their captured variables. However, **mutations made inside the block are discarded** — the updated `_State` is not propagated back to the Beamtalk caller. The compiler should emit a warning when a stateful block is passed to an Erlang call site, since mutations will be silently dropped.

Conversely, when Erlang funs are received by Beamtalk HOMs, they are wrapped to conform to the protocol. This marshalling happens at the language boundary, preserving ADR 0028's interop contract.

### Interaction with Gradual Typing (ADR 0025)

The `StateAcc` parameter is a **compiler implementation detail** hidden from the type system. Block types reflect user-visible arity only:

- `[:x | x + 1]` has type `Block(Integer -> Integer)` — the hidden StateAcc parameter is not part of the type signature
- `[:x | count := count + x]` also has type `Block(Integer -> Integer)` — same user-visible signature, even though the compiled fun has an extra parameter

This means the type checker validates the user's mental model (declared parameters and return type), and the compiler independently handles state threading. A HOM typed as `eachPair: (Block(A, A -> A))` accepts both pure and mutating blocks — the calling convention is uniform at the machine level.

This decision avoids leaking compiler internals into the type language and ensures that adding or removing a mutation inside a block never changes its type signature.

### Interaction with Session/Workspace State (ADR 0040)

There are two distinct "state" concepts in play:

1. **`beamtalk_repl_state`** (Erlang record) — the REPL session infrastructure living in the `beamtalk_repl_shell` gen_server. Contains bindings (session locals like `x = 42`), `eval_counter`, `module_tracker`, `actor_registry`, `class_sources`. This is what ADR 0040 (BT-841) restructures.

2. **`StateAcc`** (Core Erlang map) — the compiler's state-threading map for mutation inside blocks. Generated by codegen, passed as `fun(Args..., StateAcc) -> {Result, NewStateAcc}`. (In REPL eval, `Args...` is empty — the outermost eval fun is `fun(Bindings) -> {Result, UpdatedBindings}`.) This is what BT-842 universalizes.

The overlap is at the eval boundary (`beamtalk_repl_shell.erl:155`):

```erlang
{RawResult, UpdatedBindings} = apply(ModuleName, eval, [BindingsWithRegistry])
```

The REPL eval module receives Bindings as its argument, aliases it to `State` inside the generated code (`repl_codegen.rs:30`), and returns `{Result, UpdatedBindings}`. This is already the same `{Result, State}` tuple shape that BT-842 universalizes.

**Today they are one map, and they must stay one map.** The REPL codegen does `let State = Bindings`, then control-flow blocks thread that same `State`. Mutations to session locals inside `whileTrue:` blocks work because they share the map. The universal protocol changes nothing here — it just means *every* block threads this map, not just whitelisted ones.

**The REPL eval function is just the outermost "block"** that receives the session's `StateAcc`. If a future Session object (as ADR 0040 left open) owns bindings as a first-class object, its bindings map must be the same map that blocks thread. Two maps (session bindings + block locals) would require merge/split at every block boundary — exactly the complexity this ADR eliminates.

### Generated Core Erlang Examples

#### Simple block (no mutations — unchanged)

```smalltalk
items collect: [:x | x + 1]
```

```erlang
%% Pure block — no StateAcc parameter, no tuple return
call 'lists':'map'(fun (Elem) -> call 'erlang':'+'(Elem, 1) end, Items)
```

#### `whileTrue:` with mutation (current pattern, now universal)

```smalltalk
count := 0.
[count < 10] whileTrue: [count := count + 1].
count
```

```erlang
%% Pack: store 'count' into StateAcc before loop
let Packed0 = call 'maps':'put'('__local__count', Count, State) in
letrec 'while'/1 = fun (StateAcc) ->
    %% Unpack: read 'count' from StateAcc at each iteration
    let Count = call 'maps':'get'('__local__count', StateAcc) in
    %% Condition (inline — known control-flow site, no fun wrapper)
    let CondResult = call 'erlang':'<'(Count, 10) in
    case CondResult of
      <'true'> when 'true' ->
        %% Body: mutate count, update StateAcc
        let NewCount = call 'erlang':'+'(Count, 1) in
        let StateAcc1 = call 'maps':'put'('__local__count', NewCount, StateAcc) in
        apply 'while'/1 (StateAcc1)
      <'false'> when 'true' -> {'nil', StateAcc}
    end
in apply 'while'/1 (Packed0)
```

#### User-defined HOM with mutation (NEW — previously impossible)

```smalltalk
MyIterator >> eachPair: aBlock
  | i |
  i := 0.
  [i < self size] whileTrue: [
    aBlock value: (self at: i) value: (self at: i + 1).
    i := i + 2
  ].

"Usage — count mutations now propagate through eachPair:"
count := 0.
pairs eachPair: [:a :b | count := count + a + b].
count  "=> correct sum"
```

```erlang
%% The block passed to eachPair: — compiled with StateAcc parameter
fun (A, B, StateAcc) ->
    let Count = call 'maps':'get'('__local__count', StateAcc) in
    let Sum = call 'erlang':'+'(call 'erlang':'+'(Count, A), B) in
    let StateAcc1 = call 'maps':'put'('__local__count', Sum, StateAcc) in
    {Sum, StateAcc1}
end

%% eachPair: — compiled to always use the stateful protocol
%% It passes StateAcc to the block and destructures {Result, NewState}
'eachPair:'/3 = fun (Self, ABlock, State) ->
    letrec 'loop'/2 = fun (I, StateAcc) ->
        case call 'erlang':'<'(I, call 'beamtalk_dispatch':'dispatch'(Self, 'size', [])) of
          <'true'> when 'true' ->
            let A = call 'beamtalk_dispatch':'dispatch'(Self, 'at:', [I]) in
            let B = call 'beamtalk_dispatch':'dispatch'(Self, 'at:', [call 'erlang':'+'(I, 1)]) in
            let {_Result, StateAcc1} = apply ABlock (A, B, StateAcc) in
            apply 'loop'/2 (call 'erlang':'+'(I, 2), StateAcc1)
          <'false'> when 'true' -> {'nil', StateAcc}
        end
    in apply 'loop'/2 (0, State)
```

The key insight: `eachPair:` always uses the stateful protocol. It doesn't need to know whether the block mutates — it passes `StateAcc` and destructures the return uniformly. A pure block like `[:a :b | a + b]` would compile to `fun(A, B, StateAcc) -> {call 'erlang':'+'(A, B), StateAcc}` — state passes through unchanged.

#### Stored closures that mutate

```smalltalk
count := 0.
myBlock := [:x | count := count + x].
10 timesRepeat: [myBlock value: 1].
count  "=> 10"
```

```erlang
%% Stored block: compiled with StateAcc parameter
%% At creation, captures nothing (state comes in at call time)
let MyBlock = fun (X, StateAcc) ->
    let Count = call 'maps':'get'('__local__count', StateAcc) in
    let NewCount = call 'erlang':'+'(Count, X) in
    let StateAcc1 = call 'maps':'put'('__local__count', NewCount, StateAcc) in
    {NewCount, StateAcc1}
end in
%% timesRepeat: calls: apply MyBlock (1, StateAcc) and threads result state
let Packed0 = call 'maps':'put'('__local__count', Count, State) in
letrec 'loop'/2 = fun (I, StateAcc) ->
    case call 'erlang':'>'(I, 0) of
      <'true'> when 'true' ->
        let {_Result, StateAcc1} = apply MyBlock (1, StateAcc) in
        apply 'loop'/2 (call 'erlang':'-'(I, 1), StateAcc1)
      <'false'> when 'true' -> {'nil', StateAcc}
    end
in apply 'loop'/2 (10, Packed0)
```

### State-Carrying Non-Local Returns

Currently, `^` inside blocks generates:
- Value types: `throw({'$bt_nlr', Token, Value})` — 3-tuple, no state
- Actors: `throw({'$bt_nlr', Token, Value, State})` — 4-tuple, carries state

With universal state threading, **all** NLR throws must carry state, because field mutations accumulated before the `^` would otherwise be silently lost:

```smalltalk
"detect: uses ^ inside do: — state must be preserved"
MyCollection >> detect: aBlock
  self do: [:each |
    (aBlock value: each) ifTrue: [^each]
  ].
  nil
```

```erlang
%% Universal protocol: ^ always carries state
%% Value type: now 4-tuple WITH state (was 3-tuple without)
call 'erlang':'throw'({'$bt_nlr', Token, Value, CurrentStateAcc})

%% Actor: unchanged (already carries state)
call 'erlang':'throw'({'$bt_nlr', Token, Value, CurrentStateAcc})
```

The try/catch at the method boundary extracts and applies the state:

```erlang
%% Value type method with NLR
let NlrToken = call 'erlang':'make_ref'() in
try
    {Result, FinalState} = <method body>
of {NlrResult, NlrState} -> {NlrResult, NlrState}
catch <NlrCls, NlrErr, NlrStk> ->
    case {NlrCls, NlrErr} of
      <{'throw', {'$bt_nlr', CatchTok, NlrVal, NlrState}}>
        when call 'erlang':'=:='(CatchTok, NlrToken) ->
          {NlrVal, NlrState}   %% preserve mutations up to the ^
      <OtherPair> when 'true' ->
        primop 'raw_raise'(NlrCls, NlrErr, NlrStk)
    end
```

This ensures that `detect:`, `anySatisfy:`, `includes:` (which use `^` inside `do:`) preserve all field mutations made before the early return.

### Performance: Pure Block Optimization

The critical optimization: **blocks with no captured mutations skip the protocol entirely**.

The compiler already performs `BlockMutationAnalysis` on every block. When a block has no `local_writes`, no `field_writes`, and no `has_self_sends`, it compiles to a plain `fun(Args) -> Result` with zero overhead.

In practice, the majority of blocks are pure:
- `[:x | x + 1]` — pure, no state parameter
- `[:x | x > 0]` — pure, no state parameter
- `[:x | x printString]` — pure (no captured mutation), no state parameter
- `[:x | count := count + x]` — mutates `count`, gets state parameter

**Estimated overhead per block invocation:**
- **Pure blocks at unknown call sites:** ~20ns for `{Result, StateAcc}` tuple allocation + unwrap. State passes through unchanged.
- **Stateful blocks:** ~20ns tuple allocation + ~10ns per captured variable for `maps:get/2` (unpack) + ~15ns per mutated variable for `maps:put/3` (repack). For a block capturing 2 variables and mutating 1: ~65ns total overhead.
- **Pure blocks at known inline sites** (stdlib control flow): zero overhead — compiler generates direct code.

For context, a process dictionary `get`/`put` pair costs 100-500ns, and an ETS read costs ~1μs. The tuple+map approach is the cheapest mutable-state mechanism available on BEAM, and the two-tier optimization ensures stdlib hot paths pay nothing.

### Reclassification of `BlockContext` and Whitelist

The `is_control_flow_selector()` whitelist is **reclassified** from a correctness gate to an optimization hint. It is not deleted — it continues to identify Tier 1 inline sites where the compiler can generate optimized pack/unpack codegen without the universal protocol overhead.

| Current | Universal Protocol |
|---------|-------------------|
| `BlockContext::ControlFlow` — **gates** state threading | **Optimization hint** — triggers Tier 1 inline codegen |
| `BlockContext::Stored` — compiled as plain fun | Tier 2 — stored blocks get `fun(Args..., StateAcc) -> {Result, NewStateAcc}` |
| `BlockContext::Passed` — compiled as plain fun | Tier 2 — passed blocks get stateful convention |
| `is_control_flow_selector()` — correctness gate | **Optimization hint** — identifies Tier 1 fast path |
| `classify_block()` function | Retained — routes between Tier 1 and Tier 2 codegen |

The key change: blocks at `Stored`/`Passed`/`Unknown` sites now get state threading (previously they got nothing). The whitelist no longer prevents state threading — it identifies sites where the compiler can skip the universal protocol and use optimized inline codegen instead.

### Impact on Existing Codegen Modules

| Module | Change |
|--------|--------|
| `block_context.rs` | **Reclassified.** `is_control_flow_selector()` becomes optimization hint (Tier 1 routing), no longer a correctness gate. `classify_block()` retained to route between Tier 1 and Tier 2 codegen. |
| `block_analysis.rs` | **Unchanged.** `BlockMutationAnalysis` remains the source of truth. |
| `while_loops.rs` | **Unchanged for Tier 1.** The existing dual paths (`_simple` / `_with_mutations`) remain — they are the Tier 1 optimized codegen for known inline sites. No code deleted here. |
| `counted_loops.rs` | **Unchanged for Tier 1.** Same as `while_loops.rs` — existing `_simple` / `_with_mutations` paths stay. |
| `list_ops.rs` | **Unchanged for Tier 1.** Existing `generate_list_do_with_mutations()` / `generate_list_inject_with_mutations()` paths stay. |
| `value_type_codegen.rs` | **Modified.** NLR throws gain state parameter (3-tuple → 4-tuple for value types). Try/catch extracts state from the throw. |
| `mod.rs` (main codegen) | **Modified.** New Tier 2 codegen: block compilation at unknown call sites emits `fun(Args..., StateAcc) -> {Result, NewStateAcc}`. Caller-side protocol for HOMs. Expression::Return codegen always includes state when NLR token is active. |

**Net effect: code increases.** The Tier 1 inline codegen (~12 existing pure/stateful function pairs across `while_loops.rs`, `counted_loops.rs`, `list_ops.rs`) is retained unchanged. New Tier 2 universal protocol code is added. The win is **correctness** (HOMs work with mutating blocks), not code reduction.

## Prior Art

### Haskell State Monad — `s -> (a, s)`

The proposed protocol is identical in shape to Haskell's `State` monad: `newtype State s a = State { runState :: s -> (a, s) }`. GHC optimizes this aggressively via newtype erasure (zero-cost wrapper), unboxed tuples (no heap allocation for the pair), and the "state hack" (treating state-threaded lambdas as single-entry for inlining).

**Adopted:** The `s -> (a, s)` return convention. This is the standard functional approach to threading mutable state through pure computations.

**Not applicable:** GHC's newtype erasure and unboxed tuples have no BEAM equivalent. BEAM always allocates tuples, but small tuple allocation is highly optimized (~20ns).

### Kotlin `inline` + Ref Boxing

Kotlin wraps captured mutable variables in `IntRef`/`ObjectRef` containers. The `inline` keyword copies lambda bodies at call sites, eliminating the `Ref` overhead for known call sites.

**Rejected for Beamtalk:** On BEAM, the equivalent of `Ref` boxing would require process dictionary or ETS, which are 5-25x more expensive than tuple returns. Kotlin's approach works because JVM field writes cost ~1ns; BEAM has no equivalent.

**Insight adopted:** Kotlin's split between `inline` (zero overhead, incomplete) and non-inline (Ref overhead, universal) mirrors Beamtalk's current whitelist vs. the proposed universal protocol. We choose universality over zero-overhead incompleteness.

### Elixir/Erlang SSA and Accumulator Threading

Erlang's SSA compiler (`beam_ssa`) already optimizes accumulator-threaded loops with tail-call optimization and Y-register allocation. `Enum.reduce/3` is the canonical Elixir pattern: `Enum.reduce(list, acc, fn x, acc -> acc + x end)`.

**Adopted:** The universal protocol generates code that matches BEAM's native accumulator patterns, ensuring the SSA optimizer can work effectively. The generated `lists:foldl`-style loops are idiomatic BEAM code.

### Clojerl (Clojure on BEAM)

Clojerl implements mutable atoms using dedicated Erlang processes (with a proposal to switch to ETS + compare-and-swap). Every `swap!`/`reset!` requires inter-process communication.

**Cautionary tale:** External mutable state on BEAM is expensive. Process-based atoms cost microseconds per operation vs. nanoseconds for tuple returns. This strongly validates the value-threading approach over Ref-boxing.

### Gleam — `gleam-eval` Library

Gleam's community-developed `gleam-eval` library implements exactly the `fn(state) -> #(state, result)` pattern as a general-purpose state-threading mechanism. Combined with Gleam's `use` expressions, this provides ergonomic implicit state threading on BEAM.

**Validation:** The fact that this pattern emerged independently as a community library on BEAM confirms it is natural, practical, and performant for the target platform.

### Pony — Reference Capabilities

Pony uses compile-time reference capabilities to prevent data races. Closures copy captured variables — mutating a captured variable inside a lambda does NOT affect the outer scope. Workarounds include array indirection or actor delegation.

**Insight:** Pony's approach of making closures NOT alias outer mutable variables is the opposite of what Beamtalk aims for. However, Pony's distinction between "values you can mutate" and "values you can share" inspired the pure/stateful block optimization: pure blocks (shareable, no state) vs. stateful blocks (mutable captures, need state threading).

## User Impact

### Newcomer (from Python/JS/Ruby)
**Positive:** Mutation in blocks "just works" everywhere — no mysterious failures when passing blocks to custom methods. The mental model is simple: "variables you assign to inside a block stay assigned."

**Neutral:** No new syntax to learn. The protocol is entirely compiler-internal.

**Concern:** When passing blocks to Erlang libraries, the wrapper marshalling adds a layer of indirection that may produce confusing error messages if something goes wrong.

### Smalltalk Developer
**Positive:** Block composability is restored. Library authors can write custom iteration methods (`myCustomLoop:`, `eachPair:`, `visitNodes:`) and callers can pass mutating blocks, matching Smalltalk's fundamental promise.

**Neutral:** The distinction from Smalltalk images (where variables are truly mutable slots) is invisible — the compiled behavior matches expectations.

**Concern:** The two-tier optimization (inline for stdlib, universal for HOMs) is an implementation detail that occasionally leaks — performance characteristics differ between `items do: [...]` and `items myDo: [...]` even if they do the same thing.

### Erlang/Elixir Developer
**Positive:** Generated code follows idiomatic BEAM patterns (accumulator threading via `foldl`-style loops). The `{Result, State}` convention is familiar from `gen_server` callbacks and `Enum.reduce`.

**Neutral:** The state map in crash dumps is readable and debuggable. Variable names are preserved as map keys (`'__local__count'`).

**Concern:** Blocks passed across the Erlang boundary require wrapper funs (see Erlang Interop Boundary). Erlang code receiving a raw Beamtalk block would see an unexpected extra arity. The wrapper captures the enclosing `StateAcc` at creation time so reads succeed, but mutations made inside the block are silently dropped since Erlang cannot propagate the updated state. The compiler emits a warning when a stateful block is passed to an Erlang call site.

### Production Operator
**Positive:** One calling convention is more predictable than two. Fewer special cases means fewer edge-case bugs in production.

**Neutral:** Performance overhead (tuple wrap/unwrap) is negligible for stateful blocks and zero for inline-optimized blocks.

**Concern:** Hot code reloading a class that changes a block's mutation status (e.g., adding a field write to a formerly pure block) is safe under the universal protocol (arity doesn't change), but the two-tier optimization for stdlib means inline-optimized blocks could theoretically change shape. The implementation must ensure that inline optimization decisions are stable across reloads.

### Tooling Developer (LSP, IDE)
**Positive:** Eliminating the `BlockContext` classification from the codegen path simplifies the compilation pipeline. The LSP no longer needs to know which selectors are "special" for correctness.

**Consideration:** IDE tooltips could show whether a block is compiled as pure-passthrough or actively-stateful, aiding debugging. The `BlockContext` enum may be retained for this purpose even though it no longer gates codegen.

## Steelman Analysis

### Best Argument for Option B (Pragma Whitelist) — the strongest rejected alternative

| Cohort | Their strongest argument for keeping the whitelist |
|--------|--------------------------------------------------|
| **Newcomer** | "Stdlib methods already work. I'd rather have zero overhead for `do:` and `collect:` than a universal protocol I'll never notice." |
| **Smalltalk purist** | "Pragmas are well-precedented in Pharo. `<controlFlow: 0>` is explicit and self-documenting. Magic conventions are worse than explicit annotations." |
| **BEAM veteran** | "Zero overhead for the common case. 99% of blocks are passed to stdlib methods. Why pay tuple overhead for all of them when the whitelist covers everything users actually use?" |
| **Operator** | "No new calling convention means no new failure modes. The whitelist has been stable and predictable. Adding a universal protocol is a risk." |
| **Language designer** | "Minimal change, maximum backwards compatibility. The whitelist works for stdlib; the pragma extends it to user code. Two mechanisms (whitelist + pragma) are simpler than rewriting all block codegen." |

### Why Option D Wins Despite the Steelman

The BEAM veteran's argument is the strongest: zero overhead for the common case. However:

1. **The "99% stdlib" assumption is temporary.** As the ecosystem grows, library authors will write custom HOMs. The whitelist doesn't scale.
2. **The pure block optimization eliminates the overhead argument.** Blocks with no mutations compile to plain funs — same as today. Only blocks that actually mutate pay the tuple cost.
3. **Two calling conventions is permanent complexity.** Every future codegen feature must handle both paths. One convention is fundamentally simpler to maintain, test, and debug.
4. **Pre-release is the right time.** After v1.0, changing the block calling convention would be a breaking change. Now it's free.

### Tension Points

- **BEAM veterans and operators** prefer Option B's conservatism — valid, but outweighed by long-term architectural simplicity
- **Newcomers and Smalltalk purists** strongly prefer Option D — blocks should "just work"
- **Language designers** are split: B is lower-risk, D is architecturally superior
- **The deciding factor:** one calling convention eliminates an entire category of compiler complexity (the whitelist, `BlockContext` enum, dual codegen paths)

## Alternatives Considered

### Alternative 0: Status Quo (Do Nothing)

Keep the current hardcoded whitelist. Accept that user-defined HOMs don't support mutating blocks.

**Rejected because:**
- The composability failure is not theoretical — it will bite every library author who writes a custom iterator
- The whitelist requires manual maintenance for every new collection method
- Pre-release is the time to fix foundational issues; after v1.0 this becomes a permanent limitation
- However, it's worth noting: the whitelist covers all stdlib iteration patterns today, and the ecosystem is small. The pain is real but currently low-frequency.

### Alternative A: Process Dictionary Boxing (Kotlin-style Ref)

Store mutated variables in the process dictionary. Blocks capture a key, reads/writes go through `get`/`put`.

```erlang
%% count := 0. items do: [:x | count := count + x]
put('__local__count', 0),
lists:foreach(fun(X) -> put('__local__count', get('__local__count') + X) end, Items)
```

**Rejected because:**
- Process dict operations cost 100-500ns per read/write (vs. ~20ns for tuple return)
- Clojerl's experience confirms external mutable state on BEAM is expensive
- Process dict state is global within a process — no isolation between nested scopes
- Complicates actor state management (process dict vs. gen_server state)

### Alternative B: User-Extensible Whitelist (Pragma)

Keep the current whitelist but let library authors add methods via pragma:

```smalltalk
MyCollection >> customLoop: aBlock
  <controlFlow: 0>
  ...
```

**Rejected because:**
- Leaky abstraction — users must know about the annotation to make their HOMs work
- Two calling conventions remain — all dual-path codegen complexity persists
- Doesn't solve stored blocks (`myBlock := [count := count + 1]`)
- Pragmas are viral — every HOM in the chain must be annotated

### Alternative C: Inline Expansion

Inline block bodies at call sites, eliminating the closure boundary:

**Rejected because:**
- BEAM does not inline across module boundaries
- Same-module-only restriction defeats the purpose (library HOMs are cross-module)
- Code bloat from inlining large blocks
- Doesn't compose: inline expansion is all-or-nothing per call site

### Alternative D-Hybrid: Universal for HOMs, Optimized for Stdlib

Keep optimized codegen for whitelisted stdlib selectors (current fast path), but compile all blocks passed to user-defined HOMs with the universal protocol.

This is a tempting middle ground that preserves zero-overhead stdlib iteration while enabling HOM composability. The current implementation already has dual codegen paths — this approach keeps them and removes only the whitelist gate.

**Rejected in favor of the full universal protocol because:**
- The whitelist is retained as an optimization hint, not eliminated — maintenance burden persists for new stdlib methods
- Two calling conventions still exist, so every future codegen feature (e.g., async blocks, pattern-matching blocks) must handle both paths
- The two-tier optimization in the chosen approach achieves the same performance benefit (inline stdlib, universal HOMs) without requiring the whitelist to be a correctness gate — it becomes a pure performance optimization that can be extended incrementally

## Consequences

### Positive

- **Block composability restored.** User-defined HOMs work with mutating blocks, matching Smalltalk's fundamental promise.
- **Whitelist reclassified.** The hardcoded selector list becomes an optimization hint, not a correctness gate. New collection methods get state threading automatically without whitelist changes.
- **`BlockContext` repurposed.** The classification is retained but now routes between Tier 1 (optimized inline) and Tier 2 (universal protocol) codegen, rather than gating correctness.
- **One calling convention at unknown sites.** HOMs, stored blocks, and cross-module blocks all use the same protocol. Future codegen features for these sites only need one path.
- **Stored blocks work.** `myBlock := [count := count + 1]` can be passed to any HOM and mutations propagate.
- **NLR state preservation.** Non-local returns (`^`) now carry accumulated state, preventing silent mutation loss in `detect:`, `anySatisfy:`, etc.

### Negative

- **Tuple overhead for stateful blocks.** Each invocation of a stateful block allocates a 2-tuple and performs maps:get/put per captured variable. Full cost: ~65ns for a typical block with 2 captured variables and 1 mutation. Mitigated by two-tier optimization (zero overhead at known inline sites).
- **Large implementation scope.** Touches block compilation, all control-flow codegen, NLR infrastructure, and REPL state management. Estimated L-sized effort.
- **Map overhead for state access.** `maps:get/2` and `maps:put/3` per variable per iteration. For blocks with many captured variables, this grows linearly. However, most blocks capture 1-3 variables.
- **Debugging complexity.** State is threaded implicitly — when debugging generated Core Erlang, developers must trace `StateAcc` flow through nested constructs.
- **`has_self_sends` over-approximation.** Currently, any block containing a self-send (even read-only like `self size`) triggers state threading. This is conservative but adds unnecessary StateAcc overhead for pure query blocks. Future optimization: distinguish read-only self-sends from mutating self-sends.
- **Erlang interop wrapper overhead and mutation loss.** Blocks passed to Erlang/Elixir code require wrapper funs that strip the state protocol. The wrapper captures the enclosing `StateAcc` at creation time (so reads succeed), but mutations made inside the block are dropped since Erlang cannot propagate the updated state. The compiler warns when a stateful block crosses the boundary.

### Neutral

- **REPL codegen.** The REPL threads workspace-global state through `StateAcc` maps; compiled blocks thread block-local state through per-invocation `StateAcc`. These are different scopes but use the same mechanism. The `is_repl_mode` conditional in current codegen will remain — REPL blocks thread workspace state, compiled blocks thread captured-variable state. The protocol shape is the same even though the state contents differ. In the REPL, the workspace StateAcc *is* the block's StateAcc (one map, not two), since workspace bindings are the captured variables.
- **Actor codegen.** Actors already use 4-tuple NLR throws with state. The universal protocol extends this to value types, unifying the two paths.
- **Test coverage.** Existing control-flow tests remain valid. New tests needed for user-defined HOMs with mutation, stored blocks with mutation, and NLR with state in value types.

## Implementation

### Phase 0: Wire Check (proof of concept)
- Compile one block with the universal convention (`fun(X, StateAcc) -> {Result, StateAcc}`)
- Compile one HOM that calls it with the stateful protocol
- Verify round-trip: block mutation propagates through the HOM back to the caller
- Verify Erlang boundary wrapper: stateful block passed to `lists:map` via wrapper fun
- **Goal:** Prove the core calling convention works before rewriting all codegen
- **Affected:** `mod.rs` (block compilation), manual test case

### Phase 1: Block Compilation + Caller-Side Protocol
- Modify block codegen to generate `fun(Args..., StateAcc) -> {Result, NewStateAcc}` for all blocks at unknown call sites (HOMs, stored blocks)
- Pure blocks at unknown call sites get passthrough: `fun(Args..., StateAcc) -> {Result, StateAcc}`
- Pure blocks at known inline call sites (stdlib control flow) remain optimized: `fun(Args...) -> Result`
- Generate **caller-side protocol** for methods receiving block arguments: always pass `StateAcc`, always destructure `{Result, NewState}`
- Generate Erlang boundary wrappers for blocks crossing language boundaries
- **Affected:** `mod.rs` (block compilation + method codegen), `block_analysis.rs` (already correct)

### Phase 2: Control-Flow Codegen Migration
- Update `while_loops.rs`, `counted_loops.rs`, `list_ops.rs` to use the universal protocol for stateful blocks
- Retain optimized inline codegen for known control-flow selectors (these remain the fast path)
- Unify stateful list operations around `lists:foldl` with state-aware lambdas
- **Affected:** `control_flow/` directory

### Phase 3: NLR State Threading
- Update value type NLR throws to include `StateAcc` (3-tuple → 4-tuple)
- Update try/catch wrappers to extract state from NLR throws
- Unify actor and value type NLR paths
- **Affected:** `value_type_codegen.rs`, `mod.rs` (Expression::Return)

### Phase 4: Whitelist Reclassification
- Remove `is_control_flow_selector()` as a **correctness gate** — state threading no longer depends on it
- Retain the whitelist as a **performance optimization hint** for inline codegen
- Repurpose `BlockContext` for IDE/LSP use and inline optimization decisions
- **Affected:** `block_context.rs`, `semantic_analysis/mod.rs`

### Phase 5: Testing and Validation
- Add tests for user-defined HOMs with mutation (the core scenario)
- Add tests for stored blocks with mutation passed to HOMs
- Add tests for NLR with state preservation (`detect:`, `anySatisfy:`)
- Add tests for Erlang interop boundary (blocks passed to Erlang funs)
- Add tests for dynamic block assignment (pure/stateful blocks in same variable)
- Verify all existing stdlib tests pass unchanged
- Performance benchmarks: inline-optimized blocks (zero overhead), universal blocks (tuple+map overhead)

## Migration Path

This is a **compiler-internal change** with no syntax changes. Existing Beamtalk code continues to work unchanged. The only observable difference is that mutations in blocks passed to user-defined HOMs now propagate correctly — previously silent failures become correct behavior.

No user migration is required.

## References

- Related issues: [BT-842](https://linear.app/beamtalk/issue/BT-842/adr-universal-state-threading-block-protocol)
- Blocking: [BT-305](https://linear.app/beamtalk/issue/BT-305/adr-syntax-pragmatism-vs-smalltalk) (Syntax Pragmatism vs Smalltalk)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0018 (Document Tree Codegen), ADR 0025 (Gradual Typing — block arity implications), ADR 0028 (BEAM Interop — boundary wrapper design), ADR 0040 (Workspace-Native REPL Commands — session state interaction)
- Current implementation: `block_context.rs` (whitelist), `block_analysis.rs` (mutation analysis), `control_flow/` (state threading codegen)
- Appel, "Compiling with Continuations" (1992) — closure conversion and mutable variable boxing
- Kennedy, "Compiling with Continuations, Continued" (ICFP 2007) — CPS vs direct-style compilation
- Haskell State monad: `s -> (a, s)` — same shape as the universal protocol
- Gleam `gleam-eval` library — community validation of this pattern on BEAM
- Clojerl atom implementation — cautionary tale for external mutable state on BEAM
