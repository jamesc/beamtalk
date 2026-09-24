# ADR 0128: Thread Captured-Local Block Accumulators Through Opaque Callable Forwarding to Stdlib Collection HOMs

## Status
Proposed (2026-09-23)

## Context

### Problem statement

BT-3583 (blocked on this ADR): in an `Actor subclass:`, a captured-local
accumulator mutated by a block that is forwarded — as an opaque, unnamed
parameter, not a literal — from one self-sent, user-defined higher-order
method (HOM) to a stdlib collection primitive is silently dropped. The
identical method bodies on a `Value subclass:` work correctly.

```beamtalk
Actor subclass: SelfSendHomPool
  state: items = #(1, 2, 3, 4)

  do: block => self.items do: block

  inject: initial into: block =>
    acc := initial
    self do: [:each | acc := block value: acc value: each]
    acc

  size => self inject: 0 into: [:n :_each | n + 1]
```

```
st> SelfSendHomPool spawn size
0          "expected: 4"
```

(`stdlib/test/fixtures/self_send_hom_pool.bt`, `stdlib/test/fixtures/self_send_hom_sackful.bt`
— the `Value` twin, which already returns `4`; `stdlib/test/actor_self_send_hom_test.bt`'s
`testSizeThreadsCapturedLocalWriteThroughSelfSendHom`, currently skipped.)

This is not a narrow edge case. ADR 0127 (traits) found this exact gap
while drafting `Enumerable`'s actor provisions — `inject:into:`/`detect:ifNone:`
provisions written over a required `do:` (`self do: [...]`) lost both `^`
and captured-local writes on any `Actor` that `uses: Enumerable`. ADR 0127
worked around it by requiring `Enumerable` provisions to forward through a
**snapshot** `elements -> List(E)` accessor instead of `self do:` (ADR
0127 §9) — a real design cost, adopted specifically because this bug
existed. Every future trait provision that wants to route a caller's block
through `self <selector>:` inherits the same gap until this ADR's
mechanism lands.

### Two things that already work, so the fix is narrower than it first looks

Two adjacent mechanisms, easy to suspect first, are already correct and
must not regress:

1. **The self-dispatch reply convention already carries a caller-chosen
   local accumulator.** `generate_tier2_self_send_open`
   (`crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs:3311-3416`)
   packs every captured-mutated local into the SAME map passed as
   `safe_dispatch`'s `State` argument (`maps:put('__local__acc', Acc, State)`)
   before the self-send, and unpacks it from the reply's `NewState`
   afterward (`maps:get('__local__acc', State2)`). A local accumulator and
   actor field-state already share one map crossing a self-send — this is
   exactly the "one map, not two" principle ADR 0041 states for the REPL
   boundary, generalized to self-sends. `inject:into:`'s own self-send to
   `do:` (`self do: [:each | acc := block value: acc value: each]`) uses
   this path today and it works: `State1` handed to `safe_dispatch` already
   contains the live `acc`.
2. **NLR already relays correctly through the same boundary.** BT-3580
   fixed `generate_self_dispatch_error_clause`
   (`dispatch_codegen.rs:216-275`) to re-throw a `$bt_nlr` 4-tuple arriving
   through `safe_dispatch`'s `{'error', ...}` channel instead of
   misclassifying it as a runtime error — `testDetectIfNoneReturnsMatchViaNlrThroughSelfSendHom`
   and `testDoInvokedCrossProcessRelaysNlr` (`actor_self_send_hom_test.bt`)
   both pass today.

So the loss is not at the self-send boundary. It happens **strictly
inside `do:`'s own compiled body**, one level in.

### Root cause

`do: block => self.items do: block` compiles `block` — an opaque
parameter of unknown tier at `do:`'s own compile time, since `do:` can be
called with a pure block, a captured-mutating block, or (via the Tier 2
self-send above) a genuine ADR 0041 Tier 2 `fun(Elem, StateAcc) ->
{Result, StateAcc}` — and forwards it to `self.items do:`. Because the
argument is an **identifier, not a literal block**, `generate_list_do`
(`crates/beamtalk-codegen/src/core_erlang/control_flow/list_ops/basic_ops.rs:19-40`)
cannot run its literal-block mutation analysis and falls through to
`generate_simple_list_op`'s **non-literal-callable branch**
(`crates/beamtalk-codegen/src/core_erlang/control_flow/list_ops/mod.rs:234-317`),
which emits a **generic, tier-discriminating wrapper fun** so the block can
be handed to the real Erlang BIF, `beamtalk_list:do/2`
(`runtime/apps/beamtalk_stdlib/src/beamtalk_list.erl:120-124`,
`-spec do(list(), function()) -> 'nil'`, `is_function(Block, 1)`):

```erlang
case call 'erlang':'is_function'(_Callable, 1) of
  <'true'> when 'true' -> _Callable
  <'false'> when 'true' ->
    case call 'erlang':'is_function'(_Callable, 2) of
      <'true'> when 'true' ->
        fun (_WArg) ->
          let _T = apply _Callable (_WArg, State) in    %% (a)
          let _WRes = call 'erlang':'element'(1, _T) in %% (b)
          _WRes
      <'false'> when 'true' -> _Callable
    end
end
```

Two compounding defects, both visible in this fragment:

- **(a) `State` is frozen at wrap time**, not threaded. The wrapper is a
  plain arity-1 closure — the shape `beamtalk_list:do/2`'s `is_function(Block, 1)`
  guard requires — created once before the BIF call. Every one of the
  BIF's four per-element invocations closes over the SAME `State`, the
  value live at `do:`'s method entry. Element 2's update never reaches
  element 3's call.
- **(b) `element(2, _T)` (the block's own returned, updated `StateAcc`) is
  discarded** on every call. There is nowhere to put it — the wrapper's
  return type, dictated by the BIF's contract, is a bare per-element
  `Result`, not a tuple.
- **Compounding structural cause: `beamtalk_list:do/2` itself has no return
  channel for an accumulator.** Its spec is `-> 'nil'`. Even a
  hypothetically-fixed wrapper that DID thread state internally has no way
  to hand the final accumulated state back to `do:`'s own body, because
  the BIF it's delegating the loop to always answers `'nil'`.

This is, structurally, exactly the case ADR 0041 §"Erlang Interop
Boundary" already names and accepts: *"the wrapper captures the enclosing
`CurrentStateAcc` at creation time... mutations made inside the block are
discarded... the compiler should emit a warning."* `beamtalk_list:do/2` **is**
genuinely foreign, BIF-implemented Erlang from the compiler's point of
view — the accepted, documented tradeoff of ADR 0041's two-tier design is
firing exactly as designed. What makes it a bug rather than the intended
behavior is that, unlike a real third-party Erlang library, `List>>do:` is
a **first-class Beamtalk collection primitive with a compiler-known,
already-implemented, ThreadedIr-based stateful lowering for the literal-block
case** — `generate_list_do_with_mutations`
(`list_ops/basic_ops.rs:43-166`) compiles `self.items do: [:each | ...mutates...]`
via `lists:foldl` with a real, per-iteration-threaded `StateAcc`/tuple
accumulator, verified by `ThreadedIr::verify()`. The bug is that this
existing, correct, verified machinery is available only when the block is
syntactically literal at the `do:` call site — not when it is a parameter
forwarded opaquely, even though the parameter may, at runtime, be exactly
the same kind of Tier 2 block.

### Current architecture

- **ADR 0041** established the two-tier universal state-threading
  protocol: `fun(Args...) -> Result` for pure/Tier 1 blocks, `fun(Args...,
  StateAcc) -> {Result, NewStateAcc}` for Tier 2 (mutating) blocks at
  unknown call sites, with `erlang:is_function/2` used elsewhere in this
  compiler (`generate_block_value_call_runtime_discriminated`,
  `intrinsics.rs:1145-1218`; `generate_stateful_block_guard`,
  `intrinsics.rs:1384-1415`) to runtime-discriminate an opaque callable's
  tier when the compiler cannot know it statically.
- **ADR 0111** introduced `ThreadedIr` (`crates/beamtalk-codegen/src/core_erlang/threaded_ir/`)
  as the required emission input for state threading, with `verify()`
  checking per-frame version linearity (`NonLinearVersion`), unbound
  versions (`UnboundVersion`), mode/unpack consistency
  (`ThreadingModeUnpackMismatch`, `TupleAccUnpackModeMismatch`), the ADR
  0110 shadow-write contract (`ShadowWriteMissing`), and several later
  additions (`EarlyExitGateSlotMismatch`, `TupleAccInValueTypeContext`,
  `NestedStateAccFallbackUnderDirectParams`, `StateEffectEscapesExpression`).
  `ThreadingMode::StateAcc(StateAccFallbackReason)` and the `TupleAcc` mode
  already model exactly the "fold with a threaded accumulator" shape a
  fix here needs.
- **`self-send` machinery** (`dispatch_codegen.rs`) already promotes a
  literal, captured-mutating block passed to `self <selector>:` to Tier 2
  (`scan_class_for_tier2_blocks`, `detect_tier2_self_send`,
  `generate_tier2_self_send_open`), and already propagates Tier-2-ness
  through a chain of self-sends when the forwarded argument is a
  known Tier 2 block **parameter** (`tier2_block_params`,
  `dispatch_codegen.rs:3267-3277`) — i.e. `self foo: block => self bar: block`
  (another self-send) already threads correctly. The gap is specifically
  forwarding into a **non-self-send call to a stdlib collection
  primitive**.
- **`generate_simple_list_op`** (`list_ops/mod.rs:196-333`) is the single
  call site of the lossy wrapper described above; it backs `do:`
  ("foreach"), `collect:` ("map"), and `select:` ("filter") uniformly for
  non-literal callables. The `_with_mutations` sibling functions
  (`generate_list_do_with_mutations`, `generate_list_collect_with_mutations`,
  `generate_list_filter_with_mutations`, `..._bool_predicate_...`,
  `..._detect_...`, `..._count_...`, `..._inject_...`, and others in
  `list_ops/{basic_ops,filter_ops,search_ops,transform_ops}.rs`) already
  implement the correct, `ThreadedIr`-verified fold-accumulator lowering —
  but only reachable from a literal block at the call site.

### Constraints

- No new `VerifyError` variant should be needed if the fix reuses
  `ThreadingMode::StateAcc`/`TupleAcc`'s existing fold-accumulator shape —
  per CLAUDE.md, a new ad-hoc state-threading path outside `ThreadedIr` is
  not acceptable.
- The direct-call-site Tier 2 case (`self do: [:each | count := count + 1]`,
  literal block, receiver known) must not regress — it already works and
  is covered by existing snapshot tests.
- The actor-field-mutation-via-opaque-block case the current generic
  wrapper correctly serves (`self.items do: block` where `block` only
  mutates `self.field`, threaded via the actor's `State` directly, no
  local accumulator) must not regress.
- Must not weaken NLR relay (BT-3580) or exception-handling threading
  (`exception_handling.rs`'s self-dispatch shape, ADR 0111 Addendum 5/8).
- Must not introduce a correctness hazard for cross-actor sends — a block
  handed to a genuinely different process must keep dropping mutations
  (per ADR 0127 §9's documented, intentional semantics), not silently
  "fix" that case in a way that implies false safety.

## Decision

**Extend the compiler's existing call-site specialization for stdlib
collection HOM primitives — already used for literal blocks via the
`ThreadedIr`/`lists:foldl`-based `_with_mutations` lowering family — to
non-literal (opaque, runtime-tier-unknown) callable arguments, by
replacing `generate_simple_list_op`'s lossy tier-discriminating wrapper
(passed to the void-returning `beamtalk_list:do/2`/`lists:foreach`/`lists:map`/`lists:filter`
BIFs) with a `lists:foldl`-driven loop whose accumulator genuinely
threads a `StateAcc`, discriminating the callable's tier once per element
inside the fold body rather than once at wrap time.**

This is the same underlying principle as "call-site specialization of a
statically-known callee" (Option 4 in this ADR's evaluation below), but
applied to where the loss structurally occurs — the primitive collection
call site, not the self-send boundary. §Alternatives Considered explains
why the self-send boundary is not where a fix belongs for this defect, and
why the other three evaluated options are each worse fits.

### The fix in one sentence

Give the fold loop's accumulator slot to `StateAcc` itself (not just the
block's own declared locals), and decide per element, at runtime, whether
the callable needs that slot threaded through or ignored — instead of
deciding once, statically, whether to loop via a state-aware `foldl` or a
state-blind `foreach`/BIF call.

### Concrete lowering sketch: `SelfSendHomPool>>do:`

**Before** (current, buggy — `do:`'s compiled `dispatch/4` clause body,
elided to the relevant fragment):

```erlang
'dispatch'('do:', [Block], _Self, State) ->
  let _Items = call 'maps':'get'('field__items', State) in
  let _Callable = Block in
  let _Wrapped =
    case call 'erlang':'is_function'(_Callable, 1) of
      <'true'> when 'true' -> _Callable
      <'false'> when 'true' ->
        case call 'erlang':'is_function'(_Callable, 2) of
          <'true'> when 'true' ->
            fun (_WArg) ->
              let _T = apply _Callable (_WArg, State) in       %% State frozen HERE
              let _WRes = call 'erlang':'element'(1, _T) in    %% updated acc thrown away HERE
              _WRes
          <'false'> when 'true' -> _Callable
        end
    end in
  let _Result = call 'beamtalk_list':'do'(_Items, _Wrapped) in %% -spec do(...) -> 'nil'. — no return channel
  {'reply', _Result, State}                                    %% State UNCHANGED — mutation never escapes
```

**After** (this ADR's fix — `do:` no longer calls the void
`beamtalk_list:do/2` BIF when its block argument is non-literal; it
compiles the same `lists:foldl`-based accumulator shape
`generate_list_do_with_mutations` already emits for a literal block,
except the fold lambda gates on the callable's runtime arity **per
element** instead of assuming a fixed shape):

```erlang
'dispatch'('do:', [Block], _Self, State) ->
  let _Items = call 'maps':'get'('field__items', State) in
  let _Callable = Block in
  let _FoldFun =
    fun (_Elem, _Acc) ->
      case call 'erlang':'is_function'(_Callable, 1) of
        <'true'> when 'true' ->
          let _ = apply _Callable (_Elem) in _Acc            %% Tier 1: StateAcc passes through unchanged
        <'false'> when 'true' ->
          apply _Callable (_Elem, _Acc)                       %% Tier 2: {Result, NewStateAcc} — _Acc genuinely threaded
      end in
  let _FinalState = call 'lists':'foldl'(_FoldFun, State, _Items) in
  {'reply', 'nil', _FinalState}                                %% real threading — mutation escapes correctly
```

The difference from the buggy version is exactly the two defects named
above: the accumulator (`_Acc`, seeded from `State`) is the `lists:foldl`
loop's OWN accumulator, threaded by `lists:foldl` itself across all four
elements (not frozen once), and the Tier 2 branch's full `{Result,
NewStateAcc}` pair is preserved and becomes the next iteration's `_Acc`
(not discarded via `element(1, _T)`).

### Full trace through `size => self inject: 0 into: [:n :_each | n + 1]`

1. `self inject: 0 into: [pure block]` — `into:`'s literal block has no
   captured mutations, so this self-send stays Tier 1 (unchanged; no fix
   needed here).
2. Inside `inject:into:`: `self do: [:each | acc := block value: acc
   value: each]` — a Tier 2 self-send (the literal block mutates captured
   local `acc`). `generate_tier2_self_send_open` packs
   `State1 = maps:put('__local__acc', 0, State0)` and calls
   `safe_dispatch('do:', [TheBlock], State1)`, where `TheBlock =
   fun(Each, StateAcc) -> Acc = maps:get('__local__acc', StateAcc), Result
   = (block value: Acc value: Each) [Tier 1, unchanged], StateAcc1 =
   maps:put('__local__acc', Result, StateAcc), {Result, StateAcc1} end`.
   **Unchanged by this ADR** — already correct.
3. `do:`'s new body (§above) folds `TheBlock` over `#(1,2,3,4)` with
   `_Acc` seeded from `State1` (which already contains `'__local__acc' =>
   0` from step 2). Each element applies the Tier 2 branch, threading
   `_Acc` for real: `0 -> 1 -> 2 -> 3 -> 4`. `_FinalState` carries
   `'__local__acc' => 4`. **This is the fix.**
4. `do:` replies `{'reply', 'nil', _FinalState}`. Back in `inject:into:`:
   `State2 = element(2, dispatch_var)` is `_FinalState`; `Acc =
   maps:get('__local__acc', State2)` is `4`. `inject:into:` returns `4`.
5. `size` returns `4`. ✓ (`testSizeThreadsCapturedLocalWriteThroughSelfSendHom`
   passes.)

### Representation in, and verification by, `ThreadedIr`

The fix is expressed as a `ThreadedStmt::Threaded { mode:
ThreadingMode::StateAcc(StateAccFallbackReason::NonLiteralCallable), frame,
body, produces: vec![state_var], .. }` node — the same `ThreadingMode`
variant, `FrameId`, and `VersionedVar`/`Bind` machinery
`generate_list_do_with_mutations`'s literal-block path already builds and
`verify()` already checks (`NonLinearVersion`, `UnboundVersion`,
`ThreadingModeUnpackMismatch`). **No new `VerifyError` variant is
required.** The accumulator's per-iteration identity — `_Acc` above — is a
real `VersionedVar` produced once per fold-lambda invocation, exactly as
today's `Bind`-based fold body already models a threaded local; `verify()`
cannot distinguish "this `StateAcc` came from a literal block's known
mutation set" from "this `StateAcc` came from an opaque callable" and does
not need to — the invariant it checks (each version has exactly one
producer, consumed by at most one successor, in its frame) holds
identically either way.

The one genuinely new shape is **inside the fold lambda's own frame**: the
`case is_function(_Callable, N)` gate that chooses the Tier 1 vs Tier 2
`apply`. This is control flow with no state-threading content of its
own — it selects which expression computes `_Acc`'s next version, it does
not itself mutate anything — so it is legal `ThreadedStmt::Statement`
opacity (the same class already used for `ConditionalLoop`'s `continue_arm`
pattern fragment): the `Bind` that follows it targets a real
`VersionedVar` whose `op: BindOp::Direct(...)` wraps the whole `case`
expression as one `ValueRef::Doc`, exactly as an ordinary self-send or
opaque call result is modeled today (`ThreadedStmt::Bind { op:
Direct(...), .. }`, already used pervasively, e.g. the self-dispatch
`element(2, _SDn)` extraction pattern in ADR 0111 Addendum 5's
exception-handling shapes). No verifier change, no new node variant.

`StateAccFallbackReason` (`threaded_ir/ir.rs:44`) gains one new variant
(`NonLiteralCallable`) purely for diagnostic/debugging attribution — a
data-only addition, not a verifier behavior change.

### Scope

Applies uniformly to every selector routed through
`generate_simple_list_op`'s non-literal branch today: `do:`, `collect:`,
`select:` (the "foreach"/"map"/"filter" cases). The same principle —
route the non-literal-callable case through the existing
`_with_mutations` fold machinery instead of a frozen-state wrapper around
a void BIF — extends by the same pattern to `reject:`, `inject:into:`,
`detect:`, `detect:ifNone:`, `count:`, `anySatisfy:`, `allSatisfy:` (each
already has a literal-block `_with_mutations` sibling in
`list_ops/{basic_ops,filter_ops,search_ops,transform_ops}.rs` to
generalize from) and to the equivalent primitives on `Array`/`Set`, which
share `generate_simple_list_op`'s `is_list`-guarded fallback to
`beamtalk_collection:to_list` today. BT-3583's acceptance criteria (§below)
scope the *required* fix to `do:`/`collect:`/`select:` (the three
`generate_simple_list_op` covers) — `inject:into:`, `detect:`, etc. do not
need this fix to make `testSizeThreadsCapturedLocalWriteThroughSelfSendHom`
pass (per the trace above, `inject:into:`'s own body never forwards an
opaque callable to a BIF — only `do:`'s does), but are the same latent gap
and are flagged as tracked follow-up rather than silently left
inconsistent.

### Explicit non-goal: fully general opaque forwarding

This ADR does **not** build a general mechanism for a captured-local
accumulator to survive an arbitrary chain of opaque-block forwarding that
never bottoms out at a compiler-known stdlib collection primitive — e.g. a
hand-rolled, non-stdlib recursive iterator that calls `block value: x`
directly inside its own manually-written control flow, several
self-sends deep, with no `do:`/`collect:`/`select:` anywhere in the chain.
That case is not exercised by BT-3583's acceptance criteria or by any
known stdlib/trait usage today (ADR 0127's `Enumerable` deliberately
avoids it via the snapshot design). Should it surface as a real gap, the
natural escalation is Option 1 (mutable reference cell) from
§Alternatives Considered below — recorded there with its cost so a future
ADR revision does not have to re-derive why it was deferred rather than
built now.

## Prior Art

| System | Approach | What we adopt / reject |
|---|---|---|
| **Haskell `foldl'`/`mapAccumL`** | An accumulator-threading fold is the canonical way to compose a "loop with running state" over an otherwise pure, higher-order iteration combinator. | **Adopted the shape**: the fix is precisely "make the fold's accumulator carry `StateAcc`," the same idea ADR 0041 already used for the literal-block case — extended, not replaced. |
| **GHC rewrite rules / stream fusion for `foldr`/`build`** | GHC specializes a known combinator (`foldr`/`build`) at compile time when the producer/consumer shape is statically visible, falling back to a generic (slower) path otherwise. | **Adopted the principle**: specialize the KNOWN callee (`List>>do:`) at the call site when its shape is known, regardless of whether the argument is a literal or an opaque value — the same specialization already exists for literals; this ADR widens its trigger condition, not its mechanism. |
| **Erlang `lists:foldl/3` vs. `lists:foreach/2`** | `foldl` has a return channel (the final accumulator); `foreach` is `-> ok`, intentionally void — exactly the structural gap this ADR's root-cause section identifies in `beamtalk_list:do/2`. | **Confirms the diagnosis**: the fix is "use the combinator with a return channel," not "invent a new one." |
| **ADR 0041's own Erlang Interop Boundary** | Accepts, by design, that a Tier 2 block crossing into genuinely foreign Erlang loses mutation threading, with a compiler warning. | **Distinguished, not contradicted**: `List>>do:`'s Erlang implementation is foreign from a *codegen* perspective but not from a *language* perspective — it is a first-class Beamtalk collection primitive with an existing compiler-authored stateful lowering. This ADR closes the gap for exactly the cases where that lowering exists and was merely unreachable from a non-literal call site; it does not touch or weaken the genuine-interop warning path (`generate_erlang_interop_wrapper`, real `Erlang foo bar:` sends) at all. |
| **rustc / MLIR "narrow, single-purpose IR dialect" framing (ADR 0111's own Prior Art)** | A verifier's job is to check a stated invariant, not discover one. | **Reaffirmed**: this ADR adds no new invariant to state-thread — it reuses `ThreadingMode::StateAcc`'s existing one, in a new place it wasn't reachable from before. |

## User Impact

| Persona | Impact |
|---|---|
| **Newcomer** | `size`/`inject:into:`/custom iteration methods on an `Actor` now behave the same as on a `Value` — no silent, surprising `0`. Nothing new to learn; the fix is entirely compiler-internal. |
| **Smalltalk developer** | Restores the composability ADR 0041 promised for user-defined HOMs, specifically for the common case of "my HOM forwards a block one hop to a stdlib collection." `Enumerable`-style trait provisions (ADR 0127) that route through `self do:` become safe for actors again — though ADR 0127's snapshot design remains the *documented* pattern; this ADR removes one reason it was necessary but does not retroactively change ADR 0127's decision. |
| **Erlang/BEAM developer** | The generated code for the fixed cases now always drives the loop via `lists:foldl` instead of conditionally via `lists:foreach`/`beamtalk_list:do`+wrapper — one fewer shape to reason about when reading generated Core Erlang for these three selectors. The genuine Erlang-interop wrapper path (real `Erlang` sends) is untouched. |
| **Production operator** | No behavior change for existing passing programs (every case the old wrapper handled correctly — a pure block, or a block that only mutates actor fields — is a strict subset of what the new fold-based lowering also handles correctly; see §Consequences Negative for the one intentionally-preserved loss case, cross-process forwarding). One more `lists:foldl` call per non-literal `do:`/`collect:`/`select:` site instead of `lists:foreach`/`lists:map`/`lists:filter` — foldl is not meaningfully slower on BEAM for this shape. |
| **Tooling developer (LSP, debugger)** | `StateAccFallbackReason::NonLiteralCallable` gives future "why did this loop use StateAcc mode" tooling one more, now-distinguishable, reason — no new debugging surface beyond that. |

## Steelman Analysis

### Option A: Runtime-discriminated fold-accumulator specialization for opaque callables at known stdlib collection call sites (chosen — a refinement of Option 4)

| Cohort | Their strongest argument |
|---|---|
| **Newcomer** | "I don't have to think about self-sends vs. direct calls at all — collection methods just work the same whether the block came from me directly or was handed through one more method." |
| **Smalltalk purist** | "This is the SAME fix ADR 0041 already made for literal blocks, just made to also see through one layer of parameter-passing indirection — it's not a new mechanism, it's closing a gap in an existing one." |
| **BEAM veteran** | "`lists:foldl` already has the return channel `lists:foreach`/`beamtalk_list:do` structurally lack — this is 'use the combinator with the channel you need,' not new infrastructure." |
| **Operator** | "Zero new runtime state, zero cleanup discipline, zero new failure mode — it's a strictly more capable version of code that already ships and is already tested." |
| **Language designer** | "The fix lives exactly where the loss happens (the primitive collection call site), not one layer removed at the self-dispatch boundary where it would be a red herring — that's the honest fix, not the convenient one." |

### Option B: Mutable reference cell

| Cohort | Their strongest argument |
|---|---|
| **Newcomer** | Neutral-to-negative — invisible either way, but a leaked cell surfacing as a confusing crash on some rare exception path is a worse newcomer experience than today's silent-but-consistent loss. |
| **Smalltalk purist** | "A real mutable variable slot, shared by reference, is the MOST faithful match to what `[:each | acc := ...]` means in a real Smalltalk image — Beamtalk's functional threading is already the compromise; a cell is the 'least lie' option." |
| **BEAM veteran** | "Same-process is guaranteed for a self-send — a `make_ref()`-keyed process-dictionary cell is cheap (~100-500ns/access, per ADR 0041's own numbers) and this is exactly the situation (in-process, bounded lifetime) process-dict state is least bad for." |
| **Operator** | Weak — this is the one persona a new implicit side-channel state source with its own cleanup/escape-detection burden is hardest to sell to; ADR 0041 already rejected process-dict boxing for the general case on these grounds (§Alternative A). |
| **Language designer** | "This is the only one of the four options general enough to close EVERY opaque-forwarding gap in one mechanism, not just the stdlib-collection-shaped ones." |

### Option C: Extended self-dispatch reply convention

| Cohort | Their strongest argument |
|---|---|
| **Newcomer** | Neutral — invisible either way. |
| **Smalltalk purist** | Weak here specifically — doesn't address a case where the loss isn't at a self-dispatch reply at all. |
| **BEAM veteran** | "`{reply, Result, NewState}` is already the one true channel back from a self-dispatch — extending it, rather than adding a parallel one, keeps `safe_dispatch`'s ABI singular." |
| **Operator** | "A convention change is auditable in one place (`safe_dispatch`'s call sites) rather than scattered across every collection primitive." |
| **Language designer** | "If a FUTURE gap turns out to be genuinely at the self-dispatch boundary (not inside a callee's body, as this one is), this is the right shape to reach for — worth stating precisely so it isn't rediscovered as a mystery." |

### Option D: Compile-time diagnostic

| Cohort | Their strongest argument |
|---|---|
| **Newcomer** | "A clear compile error today is better than a silent wrong answer — `0` instead of `4` with no warning is the worst outcome of the four options." |
| **Smalltalk purist** | Weak — forbidding a working, common Smalltalk idiom (compose a HOM with `self do:`) is the least Smalltalk-like of the four. |
| **BEAM veteran** | "Cheapest to implement and audit — zero new codegen, just a static check." |
| **Operator** | "No behavior change to reason about at all — the safest option for a system already in production, if the actual fix can wait." |
| **Language designer** | "ADR 0127 already had to design AROUND this gap once (`Enumerable`'s snapshot semantics) — a diagnostic documents the constraint instead of leaving it as a silent trap, which is strictly better than doing nothing, even if it's not the final answer." |

### Tension points

- **BEAM veterans and language designers split** between Option A (targeted,
  reuses proven machinery, but scoped) and Option B (general, but new
  runtime-state infrastructure with real cleanup/escape-detection cost).
  The deciding factor: BT-3583's actual failure is fully explained and
  fully fixed by Option A's narrower mechanism, so Option B's extra
  generality is currently unpriced speculation, not a requirement.
- **Smalltalk purists lean toward Option B's fidelity to real mutable
  slots**, but Option A delivers the SAME observable Smalltalk-level
  behavior (mutation survives) for every case that actually occurs in the
  stdlib and in ADR 0127's design today, at much lower risk.
- **Operators and the "ship a diagnostic first" instinct (Option D) are the
  real tension worth naming**: Option D is safer in the narrow sense of
  "changes no compiled output," but it is not what BT-3583 asks for
  (working code, not a rejection), and it directly costs ADR 0127's
  `Enumerable` design the composability it wants back.

## Alternatives Considered

### Option B: Mutable reference cell

A block that captures-and-mutates an outer local, and may escape through
an opaque call, threads that local through a cell instead of a value —
e.g. a `make_ref()`-keyed process-dictionary entry (sound within one
process, since an actor self-send and every downstream call it triggers
run in the same process), or a cell owned by the defining frame.

```erlang
%% Sketch: do:'s body, ref-cell variant
let _Ref = call 'erlang':'get'({'$bt_acc_cell', SelfSendToken}) in
%% wrapper reads/writes _Ref instead of threading StateAcc through a return value
call 'beamtalk_list':'do'(_Items, WrapperThatMutatesRef) in
let FinalAcc = call 'erlang':'get'({'$bt_acc_cell', SelfSendToken}) in
call 'erlang':'erase'({'$bt_acc_cell', SelfSendToken})
```

**Correctness under `^`/NLR**: a `^` thrown mid-iteration must still erase
the cell before the throw propagates past the method that owns it, or a
future call in the same process could read a stale/erased key — this
needs the cell's owning frame to wrap acquisition/release in the SAME
try/after discipline `wrap_body_with_nlr_catch` already uses for the
4-tuple relay (BT-3580), but as a SECOND, parallel cleanup mechanism next
to it, not the same one, since the cell isn't part of the `$bt_nlr` throw
payload.

**Exceptions**: same shape as NLR — an `on:do:`/uncaught exception
unwinding past the cell's owning frame needs the same cleanup, doubling
the number of unwind paths that must remember to erase it.

**Cross-process escape**: the actual, structural hazard. A block handed
into `self do: block` and then, inside `do:`'s body, forwarded not to
`self.items do:` but to a genuinely different actor (`someOtherActor do:
block`) would have the cell keyed to the WRONG process — reads/writes
from inside that other actor's process would silently miss (process
dictionaries are per-process) or, worse, a badly-chosen key scheme could
collide across processes. Detecting this statically requires knowing,
compiler-side, that every downstream forwarding hop of an opaque block
stays within the current process — undecidable in general (a message
send's target type is not always statically known) — so this needs either
a conservative "only allowed within statically-verified same-class
self-send chains" restriction (shrinking this option's generality back
toward Option A's scope anyway) or a runtime guard that detects
cross-process use and raises, adding a check to every wrapper.

**ThreadedIr interaction**: a process-dictionary cell is, by definition, a
side channel `ThreadedIr` does not model — its value is not a
`VersionedVar` threaded through `Bind`/`Threaded` nodes at all.
Representing it faithfully would need either (a) a new `ThreadedStmt`
variant for "acquire/release a side-channel cell," whose "linearity" the
existing `verify()` machinery cannot check (a cell is not per-frame
version-linear; it is a single mutable slot visited by asynchronous,
BIF-driven callbacks) — a materially larger change to `ThreadedIr`'s
verification model than this ADR's chosen option needs — or (b) leaving it
UNMODELED, which is exactly the ad-hoc, unverified state-threading path
CLAUDE.md's rule forbids ("never add a new ad-hoc `debug_assert!`-style
state-threading call site").

**Rejected for now** because BT-3583's concrete failure does not need
this option's extra generality (§Decision's non-goal), and this option's
cost — new cleanup discipline duplicated across NLR/exception unwind
paths, a cross-process-escape hazard needing either restriction or a
runtime guard, and a `ThreadedIr` modeling gap — is real and not
currently justified by a real, observed gap beyond what Option A already
closes. Recorded here as the natural escalation if a future case needs it.

### Option C: Extended self-dispatch reply convention

The callee returns the Tier 2 block's final accumulator alongside
`NewState` explicitly, rather than relying on the caller having pre-packed
it into the shared `State` map.

**Why it does not fix BT-3583**: as established in §Context, the self-send
boundary (`inject:into:` → `do:`) already correctly carries `acc` via the
shared-map convention — `State1` handed to `safe_dispatch` already
contains `'__local__acc'`, and `State2` extracted from the reply is read
correctly. The loss happens entirely inside `do:`'s OWN body, at a
non-self-send call (`self.items do: block`) that never goes through
`safe_dispatch`'s reply tuple at all. Extending the reply convention
changes nothing about how `do:`'s internal `lists:foreach`/wrapper call
handles its block argument.

**How the callee would know which parameter's accumulator to return**:
even for a hypothetical future case where this WOULD apply, this is a real
open design question this option would need to answer — a callee's method
signature does not name which of the caller's locals are "the"
accumulator; the caller already solves this today by packing into the
shared map (no naming needed on the callee side at all), which is
arguably why the existing convention chose that shape over an explicit
return-channel design.

**Effect on `safe_dispatch`'s ABI**: today every self-dispatch reply is
uniformly `{'reply', Result, NewState}`; a channel for "also this
specific accumulator" would need either a new reply shape (a third
element, breaking every existing pattern-match on the 3-tuple across
`dispatch_codegen.rs` and the runtime) or overloading `NewState` further
(which is already how the current, working mechanism operates, making
this option redundant with what exists).

**Rejected**: does not address the actual root cause; would add ABI
complexity to fix a problem that is not where this option looks.

### Option D: Compile-time diagnostic

Reject or warn on a captured-local-mutating block passed through a
self-send to a user HOM, pointing the user at a workaround (e.g. "restructure
to avoid forwarding a mutating block through `self <selector>:`; use a
snapshot accessor instead").

**Cost to ADR 0127 trait provisions**: ADR 0127 §9 already documents that
an EARLIER draft of `Enumerable`'s actor provisions wrote `size` as `self
inject: 0 into: [...]` and `detect:ifNone:` as `self do: [...]` — exactly
the pattern this diagnostic would need to reject — and had to redesign
around a snapshot `elements` accessor specifically because this bug
existed. A diagnostic formalizes that constraint as permanent language
policy rather than a bug to fix; §User Impact already notes the
composability cost this represents for any future trait or user HOM that
wants the `self do:`-forwarding shape ADR 0127's `elements` design
explicitly avoided.

**Rejected**: forecloses working, correct code (the `Value`-class twin
already proves the identical method bodies are meaningful and correct) in
favor of a workaround, when a real, scoped, low-risk fix exists.

## Consequences

### Positive
- `do:`/`collect:`/`select:` on an `Actor`, called with a non-literal
  (opaque, possibly-Tier-2) block, now thread captured-local mutations
  correctly — parity with the `Value`-class twin restored for the fixture
  and test named in BT-3583's acceptance criteria.
- No new runtime mechanism, no new `VerifyError` variant, no new
  cleanup/escape-detection discipline — the fix is additive to an
  existing, already-shipped, already-tested `ThreadedIr` shape
  (`ThreadingMode::StateAcc`/fold-accumulator lowering).
- Removes one motivating reason for ADR 0127's `Enumerable` snapshot
  design's strictness — future trait provisions or user HOMs that want to
  forward a block through `self do:` on an actor are safe for the three
  selectors this ADR covers.
- Generated code for the fixed selectors becomes MORE uniform: `do:`/
  `collect:`/`select:` on a list always drive their loop via `lists:foldl`
  now (whether the block is literal or opaque), removing one conditional
  branch (`lists:foreach`/`lists:map`/`lists:filter` vs. `foldl`) from the
  set of shapes a reader of generated Core Erlang needs to hold in mind.

### Negative
- Does not generalize to opaque-block forwarding chains that never reach
  a stdlib collection primitive (§Decision's explicit non-goal) — a
  hand-rolled recursive iterator with no `do:`/`collect:`/`select:`
  anywhere in its chain still loses mutations, unchanged from today.
  Silent, not diagnosed — the same silent-loss shape as the bug this ADR
  fixes, just in a narrower remaining case.
- `lists:foldl` is used unconditionally for the non-literal-callable
  branch even when the callable turns out, at runtime, to be Tier 1
  (pure) — a small, fixed per-call overhead (one `is_function/2` check per
  element instead of zero) versus the old `lists:foreach`/`lists:map`
  path for that (already-passing) case. Bounded and consistent with ADR
  0041's own accepted "tuple/map overhead for the cases that need it"
  tradeoff.
- Cross-process forwarding (a block sent, from inside `do:`'s body, to a
  genuinely different actor) continues to silently drop mutations — this
  ADR does not change that, and does not add a diagnostic for it either
  (out of scope; ADR 0127 §9 documents this as the actor model's correct,
  intentional semantics, not a bug).

### Neutral
- `StateAccFallbackReason` gains one new variant
  (`NonLiteralCallable`) — data-only, no verifier behavior change.
- `beamtalk_list:do/2`'s Erlang-side signature and the genuine
  Erlang-interop wrapper path (`generate_erlang_interop_wrapper`, real
  `Erlang foo bar:` sends) are untouched.
- `inject:into:`, `detect:`, `detect:ifNone:`, `count:`, `anySatisfy:`,
  `allSatisfy:`, and the `Array`/`Set` equivalents share the same latent
  gap but are not required by BT-3583's acceptance criteria; tracked as
  follow-up scope, not silently left inconsistent (§Decision's Scope
  paragraph).

## Implementation

High-level phases for BT-3583 (concrete files/acceptance criteria to be
written into BT-3583's Linear description as part of this ADR's own
acceptance criteria):

1. **`generate_simple_list_op`'s non-literal branch** (`list_ops/mod.rs:234-317`):
   replace the frozen-`State`, `element(1, ...)`-discarding wrapper with a
   `lists:foldl`-driven loop whose fold lambda runtime-discriminates the
   callable's arity per element (`is_function(_Callable, 1)` → Tier 1,
   pass `_Acc` through; else → Tier 2, `apply _Callable(_Elem, _Acc)`
   directly, no wrapper fun needed at all). Model the fold-accumulator
   `Bind` through `ThreadedIr`'s existing `ThreadingMode::StateAcc`
   machinery — reuse `ThreadingPlan`/`generate_foldl_loop_body`'s
   plumbing rather than hand-rolling a parallel `Document` fragment.
2. **Un-skip** `testSizeThreadsCapturedLocalWriteThroughSelfSendHom`
   (`stdlib/test/actor_self_send_hom_test.bt`) and confirm it passes.
3. **Regression coverage**: confirm the two cases §Context's Constraints
   names — a pure (Tier 1) opaque callable, and an opaque callable that
   only mutates `self.field` (no local accumulator) — still compile and
   behave identically (existing snapshot/BUnit coverage should already
   exercise these; extend if not).
4. **`just verify-threaded-ir`** over the full stdlib + bootstrap-test
   corpus, to confirm no `VerifyError` regressions anywhere the changed
   code path is reachable.
5. Record `collect:`/`select:` as covered by the same change (same
   function); record `reject:`/`inject:into:`/`detect:`/`detect:ifNone:`/
   `count:`/`anySatisfy:`/`allSatisfy:` and `Array`/`Set` parity as
   explicit follow-up scope (not required for BT-3583, not silently
   dropped).

### Affected components
- `crates/beamtalk-codegen/src/core_erlang/control_flow/list_ops/mod.rs`
  (`generate_simple_list_op`)
- `crates/beamtalk-codegen/src/core_erlang/control_flow/plan.rs`
  (`ThreadingPlan`, reused rather than duplicated)
- `crates/beamtalk-codegen/src/core_erlang/threaded_ir/ir.rs`
  (`StateAccFallbackReason` — one new variant, data-only)
- `stdlib/test/actor_self_send_hom_test.bt`,
  `stdlib/test/fixtures/self_send_hom_pool.bt`,
  `stdlib/test/fixtures/self_send_hom_sackful.bt`

## References
- Related issues: BT-3610 (this ADR), BT-3583 (implementation, blocked on
  this ADR), BT-3580 (NLR relay through actor self-send — landed,
  prerequisite context)
- Related ADRs: ADR 0041 (Universal State-Threading Block Protocol —
  Tier 1/Tier 2, the Erlang Interop Boundary this ADR distinguishes from),
  ADR 0111 (Lowered IR + Verifier for State Threading — `ThreadedIr`,
  `ThreadingMode`, `verify()`), ADR 0122 (ThreadedIr Storage-Family
  Generalization Scope — `plan.rs`'s `ThreadingPlan` as the existing
  fold-accumulator single source of truth), ADR 0127 (Traits for
  Behavioural Reuse — `Enumerable`'s snapshot design, adopted specifically
  because this bug existed; §9)
- Documentation: `docs/development/debugging.md` § ThreadedIr verifier
