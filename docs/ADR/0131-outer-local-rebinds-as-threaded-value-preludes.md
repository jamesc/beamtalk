# ADR 0131: Outer-Local Rebinds as `ThreadedValue` Preludes in Every Context

## Status
Proposed (2026-10-08)

## Context

### Problem statement

A block that writes a local of its enclosing method is ordinary Smalltalk:

```beamtalk
count := 0
r := ([count := count + 1. 1] on: Error do: [:e | 0]) + 1
```

ADR 0041 promised this works "in all blocks". It works when the threading
construct (the loop, list-op, conditional or `on:do:`/`ensure:` whose block
writes `count`) is a **statement** or the **right-hand side of a local
assignment**. Anywhere else, in **operand position** (a receiver, an argument,
a binary operand, a `^` value inside an arm, a field or class-variable write's
value, an element of a literal), the outer-local write is broken, in every
method context.

BT-3738 was filed as "threading constructs in operand position inside class
methods leak their `{Value, StateAcc}` tuple". Reproducing it showed it is
neither class-method specific nor limited to a tuple leak. A 13-shape probe
(one class or value or actor per shape, each run as a BUnit test on the debug
build, 2026-10-08):

| # | Body (after `t := 0`) | Expected | Class method | Value-type method | Actor method |
|---|---|---|---|---|---|
| o1 | `r := ([t := t + 1. 1] on: Error do: [:e \| 0]) + 1` | `#[2, 1]` | `Tuple` DNU `+` | `Tuple` DNU `+` | `Tuple` DNU `+` |
| o2 | `r := ([t := t + 1. 1] ensure: [t := t + 1]) + 1` | `#[2, 2]` | `Tuple` DNU `+` | `Tuple` DNU `+` | `Tuple` DNU `+` |
| o3 | `r := (4 =:= 5 ifTrue: [2] ifFalse: [t := t + 1. 1]) + 1` | `#[2, 1]` | unbound `State` (erlc) | unbound `State` (erlc) | **`#[2, 0]`, write dropped** |
| o4 | `r := (#(1, 2) collect: [:x \| t := t + x]) size` | `#[2, 3]` | **`#[2, 0]`** | **`#[2, 0]`** | **`#[2, 0]`** |
| o5 | `self.n := [t := t + 1. 1] on: Error do: [:e \| 0]` | `#[1, 1]` | tuple stored in `n`, `t` dropped | pass | pass |
| o6 | `cond ifTrue: [^#[([t := t + 1. 1] on: Error do: [:e \| 0]), t]]` | `#[1, 1]` | tuple in result | tuple in result | tuple in result |
| o7 | `r := [c ifTrue: [2] ifFalse: [([t := t + 1. 1] on: …) + 1]. 7] on: Error do: [:x \| 3]` | `#[7, 1]` | **`#[3, 0]`** | **`#[3, 0]`** | **`#[3, 0]`** |
| o8 | as o7 without the trailing `7` | `#[2, 1]` | **`#[3, 0]`** | **`#[3, 0]`** | **`#[3, 0]`** |
| o9/o10 | o1 as `r :=` / as `^` | | `Tuple` DNU | `Tuple` DNU | `Tuple` DNU |
| o11 | statement-position conditional, then `[t := t + 1] ensure: [nil]` | `#[1, 2]` | pass | pass | pass |
| o12 | `self.n := (c ifTrue: [2] ifFalse: [t := t + 1. 1]) + 1` | `#[2, 1]` | unbound `State` | unbound `State` | **`#[2, 0]`** |
| o13 | `self.n := c ifTrue: [2] ifFalse: [t := t + 1. 1]` | `#[1, 1]` | `value` via `perform:` raises | pass | pass |

Bold cells are **silent wrong answers**: the program runs and returns a
value that is not what the source says. Only o11, the statement-position
control, passes everywhere.

Two related failures share the cause:

- **`Result tryDo:`** with a block that writes an outer local raises
  `tryDo: expected a zero-arity block, got: Block/1` in every context, in every
  position, including as a plain statement. The block is compiled as a Tier 2
  `fun (StateAcc) -> {V, StateAcc1}` and passed to the native
  `beamtalk_result:'tryDo:'/1`, which only accepts arity 0. (The BT-3718
  fix made the codegen accept the block. It did not make the runtime call it.)
- **A Tier 2 block passed to any non-intrinsic send outside an actor
  instance method** (`CvA ap: [t := t + 1. 1]` where `class ap: b => b value`)
  raises `Cannot call 'value' on Block via perform:` at the first `value`.
  Only actor self-sends carry a `StateAcc` back (BT-912, via the gen_server
  `State`). Class and value-type methods have no return channel for it.

The `local_touch` shape of the ADR 0130 class-variable agreement corpus
(`crates/beamtalk-core/src/test_helpers/class_var_program.rs`), which adds
`t := t + 1` to the head of every nested block, measures the same thing
end-to-end. At `CV_CORPUS_CASES=300`, **147 of 300 programs fail (49 %)**:
39 do not compile (all `unbound variable 'State'` / `'StateN'` / `'StateAcc'`
in `class_run` or a `hN:` helper), and 108 answer wrong in all three
spellings (most raise at runtime, from a leaked tuple, `tryDo:` arity or a
`perform:` on a Tier 2 block; 36 return the raw `{V, #{'__local__t' => …}}`
tuple). In-process codegen with the `ThreadedIr` verifier passes all 1000
programs drawn: **the verifier sees none of this**, because the broken
shapes never reach the IR as IR.

### Why it keeps recurring

The outer-local family is threaded by **position**, not by construct. Each
position that works has its own hand-written rebind:

| Position / mode | Where the rebind lives today |
|---|---|
| Last / `^` (class, value-type) | `lower_threaded_last` (drops element 2) |
| Assign-RHS (class, value-type) | `emit_threaded_assign_rhs` → `emit_vt_threaded_local_assignment`, `emit_vt_conditional_assign_rhs`, `emit_vt_exception_assign_rhs` |
| Assign-RHS (actor) | `emit_actor_threaded_assign_rhs_stmts` + `push_threaded_var_rebinds` |
| Loop body, `StateAcc` mode | `generate_local_var_assignment_in_loop` + `push_control_flow_threaded_var_rereads` |
| Loop body, direct-params / hybrid | `lower_direct_var_update_in_loop_bind` (`direct_params_list_op_result` side channel) |
| Loop body, plain `let` fast path | `try_generate_block_local_plain_let`, with four "fall back if…" guards (Tier 2 call, control-flow-with-mutations, BT-3493 field write, REPL) |
| Nested value-type `do:` | `lower_nested_vt_do` write-back (BT-3718) |
| `ensure:` cleanup | `rebind_threaded_vars_from_state` (BT-3718) |

Every row is a fix for a position someone hit (BT-912, BT-2342, BT-2349,
BT-2358, BT-2814, BT-3162, BT-3428, BT-3492, BT-3493, BT-3718). A position
with no row (a receiver, an argument, a binary operand, a field write's
value, a `^` inside an arm) gets the construct's raw `{Value, StateAcc}`
tuple. The BT-3718 agent found that a fix in one row "fixed one corpus program
and broke another" for exactly this reason: whether a rebind should be a
plain `let`, a `maps:put` into `StateAcc`, a `Gensym` rebind of a direct
parameter or a slot in a tuple accumulator depends on the **enclosing
frame's** threading mode, and a producer running inside an expression has
no way to ask.

This is the problem ADR 0118 solved for actor self-sends ("every one of the
ten recent fixes is the same patch applied to a new syntactic position"). ADR
0118 made a state-effecting expression return a `ThreadedValue` (a prelude of
`ThreadedStmt`s plus a pure value) that every consumer splices. It did that
for the `State`, `SelfVt` and (since removed by ADR 0130) `ClassVars`
families. It never did it for outer locals: `inline_control_flow_producer`
does wrap conditionals as producers, but it extracts only the actor `State`
family (`actor_conditional_families()` is hard-coded to `[State]`). That is
why o3/o12 reference an unbound `State` in class and value-type methods, and
why in an actor the `__local__t` key comes back in `State` while the Core
Erlang variable `T` is never re-read from it.

### Constraints

- ADR 0041: the `{Result, StateAcc}` calling convention for Tier 2 blocks
  stays. Locals stay immutable Core Erlang variables. There is no process
  dictionary or mutable cell.
- ADR 0111 / CLAUDE.md state-threading rule: the fix must be general (state
  the invariant for every scope kind), owned by the `ThreadedIr` node that
  owns the scope, with a `VerifyError` that fails on the repro first and
  proof over a generated corpus. No per-call-site sync, refresh or commit
  step.
- ADR 0118 §Decision 3: source evaluation order is a language guarantee.
  `(items at: i) + ([t := t + 1] on: …)` must raise from `at:` before the
  block runs.
- ADR 0122: families are data (`ThreadedFamilies`). A site declares which
  families it can carry and rejects the rest through the shared rejection
  function.
- ADR 0130: class variables are not a threading family. Nothing here touches
  them, and the `OnDoCatch` class-variable restore stays as it is.
- ADR 0111's ≤3 % build-time gate applies.

## Decision

**An outer-local write inside a threading construct is a state effect of
that construct's expression, in every position and every context. Every
local-threading construct is a `ThreadedValue` producer. Its prelude carries
one `LocalRebind` node per threaded local. The enclosing frame, not the
producer, decides how a `LocalRebind` is lowered. The per-position rebind
paths are deleted.**

### 1. Every local-threading construct is a producer

`threaded_expression` gains one producer, `local_threading_producer`, ahead
of its existing cases. It recognizes every construct that returns a
`{Value, StateAcc}` (or flat `{Value, L1, …, Ln}`) tuple for threaded outer
locals:

- loops (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`/
  `repeat`), foldl list-ops and `do:`, and the ADR 0128 opaque-callable folds;
- read+write conditionals (`ifTrue:` … `match:`; this is
  `inline_control_flow_producer` with its family list taken from context,
  §3);
- `on:do:` and `ensure:`;
- `Result tryDo:` (§5);
- an inline Tier 2 `value`/`value:`… call on a block-valued local.

Its result is:

```text
prelude: [ Statement(let CF = <construct tuple> in),
           LocalRebind { local: "t", carrier: CF, slot: Key("__local__t") | Pos(k) },
           … one per threaded local, in get_control_flow_threaded_vars order … ]
value:   element(1, CF)
```

The recognizer and the rebind set come from **one** source,
`get_control_flow_threaded_vars(expr)`. That is the same function the
construct's own tuple builder packs from, so a gate that says "this threads"
can never pair with a smaller write-back set. This is the BT-3738 acceptance
criterion for `lower_nested_vt_do`, generalized. A construct whose threaded
set is empty is not a producer, and nothing changes for it.

### 2. `LocalRebind` is lowered by its frame

`ThreadedStmt` gains:

```rust
/// ADR 0131: the outer local `local` takes the value its construct threaded
/// out, read from `carrier` at `slot`. How the new value is bound is decided
/// by the enclosing frame's recorded mode, never by the producer.
LocalRebind { local: String, carrier: String, slot: CarrierSlot, frame: FrameId, span: Span },
```

`ThreadedIr` already records each loop or conditional frame's resolved
`ThreadingMode` (`DirectParams`, `TupleAcc(g)`, `Hybrid`,
`StateAcc(reason)`). This ADR adds the two frame kinds that do not have one
today, `MethodBody` and `BranchArm`, and a `FrameModes` table that the
lowering owns. It is filled when a frame opens, read when a `LocalRebind` in
that frame is lowered, and visible to the verifier. This is the "shared way
for a producer to learn the enclosing frame's threading mode" that BT-3738
asks for: the producer does not learn it. It emits a mode-free node, and the
node's lowering looks it up.

| Enclosing frame mode | `LocalRebind` lowers to |
|---|---|
| `MethodBody` (any context) | `let T1 = <read> in` and `bind_var(t, T1)` |
| `StateAcc(_)` loop / handler body | `Bind { State_{n+1} ← Put("__local__t", <read>) }` (+ `bind_var`) |
| `DirectParams` / `Hybrid` | `Bind { Gensym(T1) ← Direct(<read>) }`, which joins the existing `final_loop_arg_identities` rebind chain |
| `TupleAcc(g)` | as `DirectParams`; the fold's closing tuple picks up the new identity |
| `BranchArm` | `let T1 = <read> in`, collected by the arm closer's existing family-slot packing |

`<read>` is `maps:get(Key, element(2, CF))` for a `StateAcc`-carrying
construct and `element(k, CF)` for a flat-tuple one. The construct reports
which (`CarrierSlot`), because the construct built the tuple.

With this in place, these become ordinary consumers that splice a prelude,
and are deleted: `emit_threaded_assign_rhs` and its three `emit_vt_*`
helpers, `emit_actor_threaded_assign_rhs_stmts`'s raw-tuple path,
`push_threaded_var_rebinds`, `push_control_flow_threaded_var_rereads`, the
`direct_params_list_op_result` side channel, the "control-flow-with-mutations"
guard in `try_generate_block_local_plain_let`, `lower_nested_vt_do`'s
write-back and `rebind_threaded_vars_from_state`. `lower_threaded_last`
stays only as a consumer that does not need the rebinds (a last expression's
locals do not escape) and drops them by not splicing them, not by special
case.

### 3. Families are taken from context, not hard-coded

`inline_control_flow_producer` and the conditional builders take their
`ThreadedFamilies` from the context (ADR 0122's `eligible_families`): `[State]`
in an actor instance method, `[SelfVt]` in a value-type method when it
writes a field, and **no `State` family** in a class method or a value-type
method that writes no field. In those, the scratch slot is the local
`StateAcc` map, seeded from `~{}~`, as ADR 0122 §2 already specifies. This
removes the unbound-`State` class (o3, o12 and 39 corpus programs) at its
source, and it is what BT-3725's `ActorStateInClassMethod` check was written
to catch. BT-3725's rule is extended to value-type methods (`ScopeKind`
gains `ValueType`): a value-type method never references the actor `State`
family.

### 4. Verifier obligations

Each check fails on a repro before the fix:

- **`VerifyError::ThreadedLocalDropped { local, construct_span }`.** A
  `Statement` binding a construct tuple whose threaded set contains `local`
  must be followed in the same frame by a `LocalRebind` for `local` before
  the frame ends, or before any other reference to the carrier. The one
  exemption is a consumer that provably discards the locals (last position,
  where the method or block returns), which records it with an explicit
  `DiscardLocals { construct_span }` node so the exemption is visible in the
  IR. To check this, the producer records the construct's threaded set on
  its `Statement` (a `ConstructTuple { threads: Vec<String> }` variant
  instead of an opaque `Statement`). Fails on o4, o7, o8 and actor o3/o12.
- **`VerifyError::LocalRebindModeMismatch { frame, mode, lowered_as }`.** A
  `LocalRebind` lowered to a shape its frame's recorded mode does not allow
  (a plain `let` inside a `StateAcc` frame, a `Put` at `MethodBody`). This
  covers the BT-3718 `ensure:` rebind and nested `do:` write-back, which
  today are guarded only by a hand-written BUnit fixture.
- **`ActorStateInClassMethod`** (BT-3725), extended to `ScopeKind::ValueType`.
  Fails on o3/o12 in value-type methods.
- **`StateEffectEscapesExpression`** (ADR 0118 §5), already defined, now also
  reported for a `LocalRebind` left in a prelude that is closed in an
  `Opaque` context (a Tier 1 closure body or an FFI argument).

`report_threaded_ir_verify_errors` reports all four. Nothing gets a new
`debug_assert!`.

### 5. `Result tryDo:` is a catch-boundary construct

`Result tryDo: <block literal>` is recognized as a compiler construct, like
`on:do:`, and lowered **at the IR level** (not as an AST rewrite) to an
`OnDoCatch` node with a new catch-all clause shape:

```text
snapshot := beamtalk_class_vars:snapshot()
try  <block body, threaded as an on:do: body: CF = {V, StateAcc}>
of   CF -> {beamtalk_result:from_tagged_tuple({ok, element(1, CF)}), element(2, CF)}
catch NLR pass-through arms (unchanged)
      Class:Reason:Stack -> restore(snapshot),
                            {beamtalk_result:from_tagged_tuple(
                               {error, beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack)}),
                             StateAccAtEntry}
```

This is the same runtime sequence as `beamtalk_result:'tryDo:'/1`, so a pure
block's result is identical whether it takes this path or the native one. It
is then a producer under §1 like `on:do:`. This answers each objection that
removed the AST lowering from PR #4187:

| #4187 objection | Answer here |
|---|---|
| A final `t := e` was lost in `Result ok: (t := e)` | The body is threaded as a block body, so the last statement is a statement. `Result ok:` is applied to `element(1, CF)` afterwards. |
| The cascade-receiver rewrite | The construct is recognized only when `Result tryDo:` is the whole send. As a cascade's first message it stays a generic send, and §6's diagnostic applies if the block is Tier 2. |
| An `Exception` filter vs a native catch-all | There is no class filter. The catch-all clause is the native one, and `ensure_wrapped` is the same call. |
| Handler-parameter aliasing | There is no handler block and no user-visible parameter. The catch variables are gensyms. |
| "Only helps once operand-position producers exist" | §1 makes it one. |

`beamtalk_result:'tryDo:'/1` stays for dynamic sends (`perform:`, a block in
a variable) with Tier 1 blocks.

### 6. A Tier 2 block with no return channel is a compile error

Outside an actor instance method there is no channel through which a callee
can return a `StateAcc`. So a block literal that writes an outer local,
passed as an argument to a send that is neither a §1 construct nor an actor
self-send, is rejected at compile time:

```text
error: block writes outer local `t`, but `ap:` cannot return the write
  ┌─ cv_a.bt:5:16
5 │     r := CvA ap: [t := t + 1. 1]
  │                  ^^^^^^^^^^^^^^^ `t` is written here
  = help: return the new value from the block and assign it: `t := CvA ap: [t + 1]`,
          or use a control-flow message (`do:`, `inject:into:`, `on:do:`) that threads locals
```

This replaces today's runtime `perform:` failure and the native-callee arity
crash. The same send compiled from an actor instance method keeps working
(BT-912). It is a `#beamtalk_error{}`-shaped diagnostic with a structured
code, emitted from the semantic-analysis pass that already classifies block
arguments (`block_analyzer`), so the LSP reports it as you type. Giving class
and value-type HOMs a real return channel (a callee-side `{Result,
StateAcc}` protocol) is out of scope and stays an open question (below).

### REPL and error examples

```beamtalk
> t := 0
0
> r := ([t := t + 1. 1] on: Error do: [:e | 0]) + 1
2
> t
1
> (#(1, 2) collect: [:x | t := t + x]) size
2
> t
4
> (Result tryDo: [t := t + 1. 1 / 0]) isError
true
> t                                   // the protected region raised: its write is discarded
4
```

A local written inside a protected region whose block then raises keeps the
value it had on entering the region. This is what `on:do:` does today in
every context (checked 2026-10-08), and it matches ADR 0130 §4's rule for
class variables. `Result tryDo:` keeps the same rule, which is why §5's catch
arm returns `StateAccAtEntry`.

(The REPL threads locals through its bindings map, the outermost
`StateAcc`, ADR 0041. Its `MethodBody`-equivalent frame lowers `LocalRebind`
to a `maps:put` into the bindings map. REPL display is unchanged.)

## Prior Art

- **Pharo / Squeak.** Blocks close over method temporaries by reference
  (heap-allocated "temp vectors" for any temp a block writes). Operand
  position is not special because the write is to a shared cell. Beamtalk
  cannot do this on BEAM without a mutable cell (ruled out by ADR 0041/0042),
  so it must thread. This ADR makes threading as position-independent as a
  cell is.
- **Erlang / Elixir.** No mutable locals. Elixir's compiler lowers to Core
  Erlang in A-normal form, where every compound sub-expression is bound by a
  `let` before use. Elixir's own block rebinding rule (variables rebound
  inside `if`/`case` do not leak out since 1.7) is the restriction this ADR
  rejects as user-facing semantics (§Alternatives C). It is fine for Elixir
  because Elixir never promised Smalltalk block semantics.
- **Gleam.** No mutation. `use` and explicit accumulator threading make the
  data flow visible in the source. This is the shape §6's help text points
  users to when no channel exists.
- **Compilers generally (SSA construction).** A write inside a structured
  construct becomes a φ at the construct's exit, placed by the dominance
  structure of the enclosing region, not by the instruction that produced the
  value. `LocalRebind` lowered by its frame is that idea: the frame (region)
  decides the φ's form.
- **Within Beamtalk.** ADR 0118 is the direct precedent, the same move for
  the `State` family. ADR 0122 supplies the families-as-data machinery this
  reuses.

## User Impact

- **Newcomer.** Code that looks right stops returning tuples or wrong
  numbers. The one new error (§6) names the variable, the send, and a
  rewrite. Before, they got `Tuple does not understand '+'`, which points
  nowhere near the cause.
- **Smalltalk developer.** `x := (coll inject: 0 into: […]) + (… on: … do:
  [count := count + 1])` behaves as in Pharo. §6 is a departure: in Pharo a
  user HOM can write a caller's temp. It is justified because BEAM has no
  shared cell. The error says so, and the actor case still works.
- **Erlang/BEAM developer.** Generated Core Erlang stays ANF-shaped `let`
  chains. A rebind is a visible `let T1 = maps:get(…)`. Nothing new reaches
  the runtime except `Result tryDo:`'s inline `try`, which calls the same
  runtime functions as before.
- **Operator.** A class of silent wrong answers becomes either correct or a
  compile error. Hot code reload is unaffected (no runtime protocol change).
  The ≤3 % build-time gate applies (one `Vec` per producing expression, as in
  ADR 0118).
- **Tooling developer.** §6 is a semantic-analysis diagnostic, so the LSP
  reports it as you type. No AST changes.

## Steelman Analysis

### B. AST-level ANF hoist (rewrite each operand-position construct to `__tN := …` ahead of its statement)
- 🧑‍💻 **Newcomer**: "Same visible result, sooner."
- ⚙️ **BEAM veteran**: "It's what Elixir's compiler does: ANF at the source level, simple and predictable, and it reuses the assign-RHS path that already works."
- 🎨 **Language designer**: "It's a smaller diff, and one pre-pass instead of a new IR node."
- **Why not:** to keep evaluation order it must hoist every earlier sibling too, which re-implements ADR 0118's sequencing rule outside the IR. It adds no verifier obligation, so the next position bug is silent again. Synthetic names leak into diagnostics and the LSP. And the assign-RHS path it would reuse is itself broken in some contexts (o5/o13 in class methods), so it inherits those bugs instead of fixing the cause.

### C. Reject the shape at compile time ("an outer local written inside an operand-position construct is an error")
- 🏭 **Operator**: "A compile error beats a silent wrong answer, and it's an S-sized change I can have this week."
- 🎩 **Smalltalk purist**: (no strong case: it bans ordinary Smalltalk.)
- ⚙️ **BEAM veteran**: "Elixir made this exact choice in 1.7 and nobody misses the old behaviour."
- **Why not as the end state:** it contradicts ADR 0041's documented guarantee and the language docs ("local mutations work in all blocks"). The rule is about *position*, which users don't think in. And it still leaves `Result tryDo:` broken in statement position. **Adopted as Phase 0** (below): ship the diagnostic for the shapes not yet migrated, then remove it phase by phase as producers land. That way nothing is silently wrong in between.

### D. Give every HOM a callee-side `{Result, StateAcc}` protocol (fix §6 instead of rejecting)
- 🎩 **Smalltalk purist**: "This is the real Smalltalk semantics: any method can run my block and my temps update."
- 🎨 **Language designer**: "One protocol everywhere, so §6 disappears."
- **Why not now:** it changes the calling convention of every block-taking method, including stdlib natives and Erlang FFI, and needs a migration of compiled code (hot reload of mixed old/new modules). That is its own ADR. This ADR doesn't block it, because §6's error is exactly the set of call sites that protocol would light up.

### Tension points
- Operators favour C for speed. Smalltalkers and language designers favour A for correctness. Phasing C in as Phase 0 of A gives both.
- BEAM veterans find B more familiar. The deciding argument against it is verifiability, which is the lesson of ADR 0111 and 0118.

## Alternatives Considered

### A narrow class-method fix (BT-3738 as filed)
Guard on frame, `in_loop_body`, direct params and hybrid loop in the
class-method producer only. The BT-3738 agent tried each guard. Each fixed
one corpus program and broke another, and the same shapes stay broken in
value-type and actor methods (table above). Rejected under CLAUDE.md's "a
state-threading fix is general or it is not a fix".

### B, C, D
See the Steelman Analysis.

## Consequences

### Positive
- One mechanism for outer-local writes in every position and every context.
  About ten per-position paths (§2's list) are deleted.
- The 13 probe shapes and the `local_touch` corpus become verifier-checked.
  A future position bug is a `VerifyError`, not a wrong answer.
- `Result tryDo:` works with stateful blocks in every position.
- BT-3725's class-method `State` rule gets its value-type twin, and the
  unbound-`State` class goes away at its source.
- `local_touch` can join `Shapes::ENABLED`, which closes the last gap in the
  ADR 0130 agreement property.

### Negative
- L–XL across five phases, touching every control-flow lowering. Each phase
  must be byte-identical on the existing corpus except where the change is
  the point (ADR 0122's `.core` diff harness).
- §6 rejects code that compiles today. All of that code fails at runtime
  today, so no working program breaks, but a program that never ran that
  path will now fail to compile.
- `FrameModes` is new lowering state that must be pushed and popped exactly
  with frames. A mismatch is a verifier error, not a silent bug, but it is
  one more invariant.

### Neutral
- The REPL's bindings map is the outermost `StateAcc`. Its frame mode is
  `StateAcc(Repl)` and needs no special case.
- Generated code for a statement-position construct is unchanged in shape
  (same `let` rebinds, now produced by `LocalRebind` lowering).

## Implementation

| Phase | Scope | Proof |
|---|---|---|
| 0 | §6 diagnostic for Tier 2 blocks to non-intrinsic sends outside actor instance methods. A temporary diagnostic for each operand-position local-threading shape the IR cannot carry yet (removed per phase). Add the probe matrix as a BUnit file covering all three contexts, with `PIN-BUG` assertions citing this ADR's epic. | Probe file red→compile-error. No silent wrong answer remains. |
| 1 | IR: `LocalRebind`, `ConstructTuple`, `DiscardLocals`, `FrameModes` (`MethodBody`, `BranchArm` added). `ThreadedLocalDropped`, `LocalRebindModeMismatch`, `ScopeKind::ValueType`. Verifier unit tests on hand-built IR for each, failing before phase 2. | Unit tests. `just verify-threaded-ir` clean. |
| 2 | `local_threading_producer` for loops, list-ops, `do:` and opaque-callable folds. Delete the assign-RHS and loop-body rebind paths for them. | o4, corpus `do`/`fold`/`while`/… with `local_touch`. `.core` diff harness byte-identical elsewhere. |
| 3 | Conditionals and `match:` families from context (§3). `on:do:`/`ensure:` as producers. Delete `lower_nested_vt_do` write-back and `rebind_threaded_vars_from_state`. **This is BT-3738.** | o1–o3, o5–o12 in all contexts. |
| 4 | `Result tryDo:` construct (§5). | `tryDo:` probes. `local_touch` + `try_do` corpus. |
| 5 | Close-out: `local_touch` into `Shapes::ENABLED`, un-`#[ignore]` both properties, `CV_CORPUS_CASES=500` green, remove Phase 0's temporary diagnostics, flip every `PIN-BUG`, docs (`beamtalk-language-features.md` "What Works and What Doesn't", `docs/agents/expanded.md` § State-Threading Codegen, `debugging.md` verifier table), build-time measurement. | CI. |

Affected components: `beamtalk-codegen` (`threaded_ir/{ir,verify,emit,build}.rs`,
`util.rs`'s `threaded_expression`, `control_flow/*`, `threaded_expr.rs`,
`blocks.rs`, `value_type_codegen.rs`, `gen_server/methods.rs`),
`beamtalk-core` (`semantic_analysis/block_analyzer.rs` for §6,
`test_helpers/class_var_program.rs`), stdlib tests. There is no runtime
change: `beamtalk_result:'tryDo:'/1` is kept as is.

**Shared-kernel check.** The threaded-local set has exactly one source,
`get_control_flow_threaded_vars` (plus `compute_threaded_locals_for_loop`,
which phase 2 folds into it or asserts equal to it with a
`LocalRebindModeMismatch`-style verifier check, never a comment). Families
come from ADR 0122's `ThreadedFamilies` / `eligible_families`. No new
selector table is introduced: the recognizer reuses
`beamtalk_core::state_threading_selectors`.

## Migration Path

No working program changes behaviour. Programs that today fail at runtime or
answer wrong either become correct (phases 2–4) or get a compile error
(§6, phase 0). The §6 error's help text gives the rewrite.

## Open Questions

1. **Callee-side Tier 2 protocol for class and value-type HOMs** (Alternative
   D). This needs its own ADR. Until then §6 stands.
2. Should a `LocalRebind` whose local is dead after the construct be elided?
   The verifier exemption would need a liveness fact the IR does not carry
   today. The default is to emit it (correctness first) and measure.

## References
- Related issues: BT-3738 (becomes phase 3), BT-3718, BT-3725, BT-3737,
  BT-3694, BT-912, BT-3493
- Related ADRs: ADR 0041, ADR 0111, ADR 0118, ADR 0122, ADR 0128, ADR 0130
- Documentation: `docs/beamtalk-language-features.md` § Control Flow and
  Mutations; `docs/agents/expanded.md` § State-Threading Codegen;
  `docs/development/debugging.md` § ThreadedIr verifier
