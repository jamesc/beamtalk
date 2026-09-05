# ADR 0118: Expression-Level State Threading via ThreadedIr Preludes

## Status
Implemented (2026-09-05)

## Implementation Tracking

**Epic:** [BT-3413](https://linear.app/beamtalk/issue/BT-3413)
**Status:** Done

| Phase | Issue | Description | Size | PR |
|---|---|---|---|---|
| 0 | [BT-3414](https://linear.app/beamtalk/issue/BT-3414) | Self-send position regression matrix + threading-predicate conformance test | M | [#3712](https://github.com/jamesc/beamtalk/pull/3712) |
| 1a | [BT-3415](https://linear.app/beamtalk/issue/BT-3415) | `ThreadedValue`, `close()`, `StateEffectEscapesExpression`; self-dispatch producer; Actor body consumer; sequencing rule for sends and binary operands | M | [#3717](https://github.com/jamesc/beamtalk/pull/3717) |
| 1b | [BT-3416](https://linear.app/beamtalk/issue/BT-3416) | Sequencing rule for literals, interpolation, return, assignment, cascade, `match:` scrutinee | M | [#3718](https://github.com/jamesc/beamtalk/pull/3718) |
| 2a | [BT-3417](https://linear.app/beamtalk/issue/BT-3417) | Conditional arms, exception arms, stateful-block bodies, conditional receiver consume `ThreadedValue` | M | [#3719](https://github.com/jamesc/beamtalk/pull/3719) |
| 2b | [BT-3418](https://linear.app/beamtalk/issue/BT-3418) | Loop-body consumers; delete planner, `HoistSink`, registries, BT-3399 warning | M | [#3720](https://github.com/jamesc/beamtalk/pull/3720) |
| 3 | [BT-3419](https://linear.app/beamtalk/issue/BT-3419) | `ConditionalLoop` condition as IR | M | [#3723](https://github.com/jamesc/beamtalk/pull/3723) |
| 4 | [BT-3420](https://linear.app/beamtalk/issue/BT-3420) | Inline-threaded control flow as producer; `ifNil:ifNotNil:`, `match:` arms, `ifNone:`, generic Tier 2 blocks | M | [#3722](https://github.com/jamesc/beamtalk/pull/3722) |
| 5a | [BT-3421](https://linear.app/beamtalk/issue/BT-3421) | ClassVars producers via `ThreadedValue`, open-scope shimmed | M | [#3730](https://github.com/jamesc/beamtalk/pull/3730) |
| 5b | [BT-3422](https://linear.app/beamtalk/issue/BT-3422) | Delete the open-scope protocol and helpers | M | [#3733](https://github.com/jamesc/beamtalk/pull/3733) |
| 6 | [BT-3423](https://linear.app/beamtalk/issue/BT-3423) | One `state_effects` fact, one selector table, gates collapsed | M | [#3734](https://github.com/jamesc/beamtalk/pull/3734) |
| 7 | [BT-3424](https://linear.app/beamtalk/issue/BT-3424) | Close-out: gates removed, `verify-threaded-ir`, docs, ADR 0111 addendum, measurement, REPL e2e | S | this PR |

Dependency order: 3414 → 3415 → 3416 → 3417 → 3418 → {3419, 3420, 3421} → 3422 → 3423 → 3424.

**Follow-up filed separately, not part of this epic's close-out:** [BT-3430](https://linear.app/beamtalk/issue/BT-3430) — wiring `ThreadedValue::close`'s `StateEffectEscapesExpression` into `check_no_unsafe_class_method_self_sends`'s user-facing diagnostic (phase 5b's own acceptance criteria named this and it was deliberately not completed in BT-3422).

**Final measurement** — see this ADR's own addendum below for the whole-epic ≤3% gate result against the pre-epic baseline commit.

## Context

### Problem statement

ADR 0111 gave the state-threading subset of codegen a small lowered IR
(`ThreadedIr`) with a verifier, and the BT-3141/BT-3155 epics made that IR the
real emission input for every *statement-level* construct family: loops,
conditionals, list-ops, exception handling, Actor and class-method bodies, and
Tier 2 stateful blocks. That part of the design has held: none of the ten
self-send fixes landed between 2026-08-30 and 2026-09-03 (BT-3374, BT-3382,
BT-3392, BT-3396, BT-3399, BT-3402, BT-3403, BT-3405, BT-3406, BT-3385) touched
`threaded_ir.rs`, and the verifier caught the BT-3374 shape before it shipped.

What has not held is the boundary ADR 0111 deliberately drew around it:
"general expression codegen stays AST-directed." An actor self-send
(`self bumpCount`) is a *state-effecting expression*. When it sits in
statement position, the statement-lowering site threads its `NewState`
through a real `ThreadedStmt::Bind`. When it sits anywhere *inside* an
expression, it reaches `generate_self_dispatch`
(`dispatch_codegen.rs`), whose contract is to return the reply value and
**discard** the new state. Every one of the ten recent fixes is the same
patch applied to a new syntactic position: a hoisting step that runs ahead of
the statement, dispatches the nested self-send into a temp, and substitutes
the temp by source span when the ordinary expression compile later reaches
the same node.

That patch has now grown into a parallel, unverified sub-system:

- **An AST-directed hoist planner** (`plan_self_send_hoists` /
  `hoist_plan_walk` / `emit_hoist_plan`, `control_flow/conditionals.rs`,
  ~420 lines) with its own hand-maintained opacity rules per `Expression`
  variant, wired at **13 call sites** across `conditionals.rs`,
  `exception_handling.rs`, `expressions.rs`, and `gen_server/methods.rs`.
  Any lowering site not on that list keeps the discarding behaviour.
- **Two emission sinks.** `HoistSink::Threaded` produces real `Bind` nodes
  the verifier sees. `HoistSink::OpenDocs` produces raw `let … in`
  `Document`s that bypass `verify()` entirely, and is what
  `compile_conditional_receiver`, the loop-body catch-all
  `emit_non_assign_expr`, and the `Return` handler in `mod.rs` use.
- **Two span-keyed substitution registries** on the generator
  (`hoisted_self_send_results`, `hoisted_field_reads`) that the generic
  dispatch path consults to know whether "someone already ran this node."
- **An order-safety escape hatch.** The planner cannot reorder a self-send
  ahead of an operand that may raise, so BT-3399 made it *give up*: the
  self-send is recorded as `HoistAction::Dropped`, compiled through the
  discarding path, and a warning is emitted. The mutation is still lost.
- **A second, older mechanism for the same problem** in class-method
  context. A class-method self-send threads `ClassVars` via an "open
  let-chain": `generate_expression` emits `let ClassVarsN = … in` with no
  body and records the result variable in `last_open_scope_result`;
  consumers must notice and close the scope (`expression_doc_with_open_scope`,
  `closed_expression_doc`, `capture_subexpr_sequence`,
  `split_subexpr_for_preamble`, `hoist_subexpr_splits`, `bind_args_to_temps`,
  `hoist_open_scope_receiver`/`_argument` — 8 producers/helpers, ~80
  consumer call sites). BT-3406 was a cascade argument that forgot to close
  it and produced a `core_parse_error`. This is the same design as the
  `State` hoist planner, implemented a second time with `Document`s instead
  of IR nodes.
- **Predicate sprawl on the decision side.** The same four-term
  "does this need inline threading?" disjunction is inlined four times in
  `intrinsics.rs` (`ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:`);
  only the `and:`/`or:` copy got a name (`and_or_needs_mutation_threading`).
  `control_flow_has_mutations` in `gen_server/methods.rs` recomputes those
  gates under a "must stay in sync" comment with no enforcing test.
  `contains_hoistable_self_send` needed a manual `and:`/`or:` carve-out
  because the walker treats conditional selectors as opaque. Four parallel
  selector tables now exist (`get_control_flow_threaded_vars`,
  `is_state_threading_keyword_selector`, `is_conditional_selector`, and the
  lint's `is_state_threaded_block_arg`); the last did not receive
  `and:`/`or:` when BT-3402 added them to the others.

### Current state, as measured

A 47-shape probe (an Actor fixture with `self bumpCount` nested in one
syntactic position per method, run as BUnit under the debug build on
2026-09-03) gives the honest picture. Everything the ten PRs targeted passes.
The rest:

| Shape | Outcome today |
|---|---|
| `[i := i + 1. (self bumpCount) > 0 and: [i < 3]] whileTrue: [nil]` | Verifier panic `UnboundVersion` |
| `[i := i + 1. (self flagTrue) and: [i < 3]] whileTrue: [nil]` | Verifier panic `UnboundVersion` |
| `items do: [:x \| x > 0 ifTrue: [(self flagTrue) and: [true]] ifFalse: [nil]]` | Verifier panic `NonLinearVersion` |
| `[i := i + 1. (self bumpCount) + i < 5] whileTrue: [nil]` | Runtime crash (block arity) |
| `[i := i + 1. (i < 3) and: [(self bumpCount) > 0]] whileTrue: [nil]` | Runtime crash (block arity) |
| `self record: (flag ifTrue: [self bumpCount] ifFalse: [0])` | Runtime crash: `{Result, State}` tuple leaked into the argument |
| `items at: (flag ifTrue: [self bumpCount] ifFalse: [1])` | Runtime crash: tuple leaked |
| `items detect: [:x \| x > 100] ifNone: [self bumpCount]` | Runtime crash: invalid argument |
| `self record: 1; record: 2` (cascade on `self`) | **Silent drop, no warning** |
| `self record: (self bumpCount); record: 1` | Silent drop, no warning |
| `v ifNil: [0] ifNotNil: [:x \| x + (self bumpCount)]` | Silent drop, no warning |
| `v match: [1 -> 1 + (self bumpCount); _ -> 0]` and `(self bumpCount) match: […]` | Silent drop, no warning |
| `items sort: [:a :b \| (a + (self bumpCount)) < b]` | Silent drop, no warning |
| `items do: [:x \| self.count := self.count + (self bumpCount)]` | Silent drop, no warning |
| `items do: [:x \| y := 1 + (self bumpCount)]` | Silent drop, BT-3399 warning |
| `total := items size + (self bumpCount)` | Silent drop, BT-3399 warning |
| `"{items size}-{self bumpCount}"` | Silent drop, BT-3399 warning |

The cascade row is the one that should worry us most: `self a; b` is
ordinary Smalltalk style, `generate_cascade` (`expressions.rs`) routes every
message through the generic runtime send, and no actor fixture in the
repository uses a cascade on `self`, so nothing would have caught it.

None of these shapes is exotic. Each is "a state-effecting expression in a
position the planner was never wired for." The verifier panics are the good
news (ADR 0111 doing its job); the silent drops are the bad news, and they
are silent precisely because they happen on the side of the boundary the
verifier cannot see.

### Constraints

- ADR 0041/0042: actor state is immutable and explicitly threaded;
  `{reply, Reply, NewState}` is the gen_server contract. No process
  dictionary, no mutable state cell.
- ADR 0110: class-var `Bind`s at NLR-relay points must carry a shadow write;
  `VerifyError::ShadowWriteMissing` must keep seeing them.
- ADR 0111 §Verifier honesty: the verifier can only check what is in the IR.
  A fix that keeps state effects outside the IR keeps them unverifiable.
- ADR 0018: no full typed Core Erlang IR. The `Document` tree stays the
  emission substrate; the ≤3% build-time gate from ADR 0111 applies to any
  codegen-shape change.
- CLAUDE.md: no duplicate implementations; a "keep in sync" comment is not a
  mechanism.
- Source evaluation order is a language guarantee: `(items at: idx) + (self
  bump)` must raise from `at:` before `bump` runs.

## Decision

**Every expression compiled in a state-threading context returns a value
plus a prelude of `ThreadedStmt`s, and every consumer must splice the prelude
before using the value.** State effects inside expressions stop being a
special case handled by a hoisting pre-pass and become the ordinary shape of
expression codegen, in A-normal form, verified by the existing
`ThreadedIr::verify()` because the prelude *is* IR.

Concretely:

### 1. `ThreadedValue`: the expression-level result

```rust
/// The result of compiling one expression in a state-threading context.
/// `prelude` runs first, in source evaluation order, and may advance any
/// versioned prefix (`State`, `ClassVars`, `Self`); `value` is then a pure
/// reference to the expression's result.
#[must_use = "a ThreadedValue's prelude carries state Binds; splice it or close it"]
pub(super) struct ThreadedValue {
    pub prelude: Vec<ThreadedStmt>,
    pub value: ValueRef,
}
```

`ThreadedValue` replaces, in one type, both the `State` hoist registries
(`hoisted_self_send_results`, `hoisted_field_reads`) and the `ClassVars`
open-scope protocol (`last_open_scope_result` / `OpenScopeResult`). A pure
expression is `ThreadedValue { prelude: vec![], value }`; the common case
costs nothing.

### 2. Producers

The state-effecting expression forms produce non-empty preludes:

- **Actor self-send** (`generate_self_dispatch`): prelude is
  `[Statement(let SD = safe_dispatch(…) in), Bind(State_{n+1} ← element(2, SD))]`,
  value `element(1, SD)`. This is exactly what `dispatch_self_send_as_bind`
  builds today, moved to the one place the self-send is compiled.
- **Class-method self-send / class-var assignment**: prelude carries the
  `ClassVars` `Bind` (with `shadow_write` per ADR 0110); the open let-chain
  and `last_open_scope_result` are deleted.
- **Inline-threaded control flow in expression position** (a conditional,
  `and:`/`or:`, `match:`, loop, or list-op that the classifier routes through
  its `_with_mutations` generator): prelude is `[Statement(let CF = <case …>
  in), Bind(State_{n+1} ← element(2, CF))]`, value `element(1, CF)`. This is
  the row that today leaks a `{Result, State}` tuple into an argument.
- **Field assignment** (`self.f := …`) and **`at:put:` on a field** in
  expression position: the existing `lower_field_assignment_bind` output,
  as a prelude.

### 3. Sequencing rule (evaluation order by construction)

When `generate_expression` compiles a node with sub-expressions
(`MessageSend` receiver and arguments, binary operands, `Cascade` messages,
literal elements, `StringInterpolation` segments, `Return` and assignment
values, `match:` scrutinee), it compiles the children in evaluation order
and applies one rule:

> If any child at position *k* has a non-empty prelude, every child at a
> position *< k* whose value is not a literal or plain variable is bound to
> a fresh temp (`Statement(let TmpN = value in)`) in the parent's prelude
> **before** child *k*'s prelude is appended.

This is the "decide once, hoist all or none" rule BT-3406's
`hoist_subexpr_splits` already applies to cascade arguments, made universal.
It makes `HoistAction::Dropped` unrepresentable: `(items at: idx) + (self
bump)` compiles to `let Tmp1 = items at: idx in let SD = … in let State2 =
element(2, SD) in Tmp1 + element(1, SD)`, which raises from `at:` first and
threads `bump`'s state. The `self.field`-read snapshot special case
(`hoisted_field_reads`) is the same rule applied to a `FieldAccess` child,
so it needs no separate machinery.

Because children are compiled in order and each producer advances the
version counter as it emits its `Bind`, the version chain in a prelude is
linear and the verifier's `UnboundVersion`/`NonLinearVersion` checks apply
to it unchanged.

### 4. Consumers

Every statement-lowering site that today calls `hoist_nested_self_sends` or
closes an open scope instead does:

```rust
let tv = self.threaded_expression(expr)?;   // ThreadedValue
stmts.extend(tv.prelude);                   // into the enclosing frame's IR
stmts.push(ThreadedStmt::Statement(render_value(tv.value), span));
```

The consumer set is finite and named: Actor method bodies
(`lower_body_exprs_with_reply`), class-method bodies
(`lower_class_method_body`), conditional branch arms
(`generate_conditional_branch_inline`), `on:do:`/`ensure:` arms, Tier 2
stateful-block bodies (`generate_block_stateful_body`), loop bodies
(`generate_threaded_loop_body_inner`, `emit_non_assign_expr`,
`generate_local_var_assignment_in_loop`), and the **loop condition** (see
§6). `HoistSink` and `hoist_nested_self_sends` are deleted once the last
consumer is migrated.

### 5. Closing a prelude: the verifier's new obligation

A consumer that must produce a self-contained `Document` (a Tier 1 closure
body, an Erlang FFI argument, a block passed to a class method, spec/doc
codegen) calls `tv.close(ctx)`, which renders the prelude as nested `let`s
around the value. `close()` is the *only* way to discard a prelude, and it
reports:

```rust
VerifyError::StateEffectEscapesExpression {
    prefix: VersionPrefix,   // State | ClassVars | Self
    at: Span,
}
```

whenever the prelude contains a `Bind` for a prefix the enclosing context
cannot thread. In debug/CI builds that is a hard failure through the
existing `report_threaded_ir_verify_errors`; in release builds it is an
`internal:` diagnostic. Where today's code already emits a user-facing
diagnostic for the same situation (`warn_stateful_block_at_erlang_boundary`,
the class-method "self-send in a closure" compile errors), that diagnostic
is produced *from* the `close()` result rather than from a separate
predicate, so the two cannot disagree.

The `#[must_use]` on `ThreadedValue` turns "forgot to splice" from a silent
drop into a compiler warning (denied in CI via `clippy` with warnings as
errors).

### 6. `ConditionalLoop` carries its condition as IR

`ThreadedStmt::ConditionalLoop` today holds the loop condition as an opaque
`continue_header: Document`. A `whileTrue:` condition block containing a
self-send or an inline-threaded `and:` is the shape that panics the verifier
today, because the condition is compiled outside the loop's frame. The node
gains:

```rust
condition: Vec<ThreadedStmt>,   // the condition block's prelude, in the loop frame
condition_value: ValueRef,
```

and `render_conditional_loop` emits the prelude inside the loop `fun` before
the `case`. The condition's `Bind`s are then in the frame the verifier
checks, and its final `State` version is what the loop's continue call
threads.

### 7. One decision predicate, owned by semantic analysis

The "does this sub-tree have state effects?" question is answered once.
`beamtalk-core`'s semantic analysis already computes `block_profiles` per
block (BT-1309). It gains a per-node `state_effects: StateEffects` fact
(`{ actor_self_send, field_write, class_var_write, class_self_send }`,
computed bottom-up over the whole expression tree, seeing through
parentheses and into non-closure sub-expressions). Codegen's gates
(`control_flow_has_mutations`, `match_needs_mutation_threading`,
`and_or_needs_mutation_threading`, the four inlined intrinsic gates,
`contains_hoistable_self_send`, `needs_mutation_threading`'s self-send arm)
collapse to one `self.subtree_needs_threading(span)` reading that fact.
The four selector tables collapse to one in
`beamtalk-core::state_threading_selectors`, with the lint's
`is_state_threaded_block_arg` deleted in favour of it, and a conformance
test that enumerates every `WellKnownSelector` and asserts the codegen
threaded-vars map and the selector predicate agree.

### What this looks like

The user-visible change is that code which silently lost state now works,
and BT-3399's warning disappears because the case it warned about no longer
exists.

```beamtalk
Actor subclass: Counter
  state: count = 0
  state: log = #()

  bump =>
    self.count := self.count + 1
    self.count

  record: n =>
    self.log := self.log ++ #(n)
    n

  // Cascade on self: both mutations now land.
  twice => self record: 1; record: 2

  // Nested in an argument, after an operand that may raise: `at:` still
  // raises first, and when it doesn't, `bump`'s mutation is kept.
  pick: idx => self.log at: idx + (self bump)

  // Threaded conditional as an argument: no tuple leaks into `record:`.
  maybeRecord: flag => self record: (flag ifTrue: [self bump] ifFalse: [0])

  // Loop condition with a self-send: no verifier panic, state threads
  // through the loop's own parameter.
  drain =>
    [self bump < 3] whileTrue: [nil]
    self.count
```

```
> c := Counter spawn
> c twice. c getLog
#(1, 2)
> c maybeRecord: true. c getCount
1
```

### Error examples

A self-send inside a block that must be a real closure still cannot thread,
and now says so from the same mechanism that would otherwise thread it:

```
error: `self bump` inside this block cannot thread actor state
  --> counter.bt:14:31
   |
14 |     items sortBy: [:a :b | (self bump) < b]
   |                            ^^^^^^^^^ mutates `count` in a closure that runs outside this method's state
   = help: bind the value before the block: `n := self bump. items sortBy: [:a :b | n < b]`
```

(Whether a given selector's block is threaded or a closure is decided by the
single selector table in §7, as today; the change is that the boundary is
reported from `close()`, not from a second predicate.)

## Prior Art

- **Smalltalk (Pharo, Squeak, Newspeak).** No analogue: objects are
  mutable, so `self bump` mutates in place wherever it appears. The
  *expectation* Smalltalk sets is the relevant part: any expression
  position, cascades included, may send to `self`. Beamtalk's immutable
  actor state (ADR 0042) is a departure the compiler is responsible for
  hiding; a position-dependent list of where `self` sends work is not
  acceptable to a Smalltalk developer.
- **Erlang/Elixir gen_server.** Programmers write the state plumbing by
  hand, in A-normal form: `{Reply1, S1} = do_bump(S0), {Reply2, S2} =
  do_record(Reply1, S1)`. This ADR makes the compiler emit exactly the code
  an Erlang developer would write, which is also the code they will read
  when debugging generated Core Erlang.
- **A-normal form** (Flanagan, Sabry, Duba, Felleisen, *The Essence of
  Compiling with Continuations*, PLDI 1993). Naming every intermediate
  computation so that effects sequence explicitly is the standard
  compiler answer to "effects inside expressions." Monadic state-passing
  (Haskell's `State`, `do` notation) is the same idea at the type level.
  The prelude/value split is ANF restricted to the expressions that
  actually have effects.
- **Gleam / Pony / Akka.** Gleam has no mutable state and forces the
  programmer into explicit threading. Pony and Akka actors have mutable
  in-process state, so the question does not arise. Neither offers a
  compile-time approach to steal; the ANF literature does.
- **Rust `#[must_use]`.** Making a value that carries an obligation
  impossible to drop silently is a well-understood linear-discipline
  technique; the same trick applies to `ThreadedValue`.

## User Impact

- **Newcomer.** Invisible except that things work: `self a; b` and
  `self foo: (self bar)` behave as they would in any object language. The
  BT-3399 warning they might have seen today, with its advice to restructure
  the expression, goes away.
- **Smalltalk developer.** Cascades on `self` and nested sends to `self` are
  idiomatic; today's silent drop is the worst possible failure mode for this
  cohort. After this ADR the only remaining restriction is the existing,
  documented one (a self-send inside a block that must be a real closure),
  and it is a compile error, not a lost mutation.
- **Erlang/Elixir developer.** Generated Core Erlang becomes more uniform:
  every effectful sub-expression is a named `let`, the shape they would
  write by hand. No runtime change; `{reply, R, NewState}` is untouched.
- **Production operator.** No runtime behaviour or hot-reload impact. Codegen
  emits a few more `let` bindings in methods that nest self-sends; the ADR
  0111 ≤3% build-time gate applies.
- **Tooling developer.** No AST change. The `state_effects` semantic fact
  (§7) is reusable by the LSP for a "this block closes over actor state"
  diagnostic and by the lint, which today keeps its own selector table.

## Steelman Analysis

### Option A: Expression-level preludes (this ADR)
- 🧑‍💻 **Newcomer**: "I never have to learn which positions `self` works in."
- 🎩 **Smalltalk purist**: "This is the first design where cascades on `self` are not a special case."
- ⚙️ **BEAM veteran**: "The output is the ANF I'd write myself; the `#[must_use]` obligation is the compiler doing my `{R, S1} = …` bookkeeping."
- 🏭 **Operator**: "No runtime change, and the verifier gate is the same one that already runs in CI."
- 🎨 **Language designer**: "One type replaces two hoisting mechanisms, two registries, and five predicates. The verifier's scope grows to match the language's."

### Option B: Keep the planner, wire the remaining sites
- 🧑‍💻 **Newcomer**: "Each fix ships in a day; I get my bug fixed fast."
- 🎩 **Smalltalk purist**: "As long as the next fix is cascades, I'm fine."
- ⚙️ **BEAM veteran**: "Small, reviewable diffs against code I already know."
- 🏭 **Operator**: "Least risk to the build-time gate; each PR is measurable."
- 🎨 **Language designer**: "Incremental is how ADR 0111 succeeded; why change strategy now?"

### Option C: Full typed Core Erlang IR (ADR 0018 Alt 3, ADR 0111 Alt 4)
- 🧑‍💻 **Newcomer**: no argument.
- 🎩 **Smalltalk purist**: no argument.
- ⚙️ **BEAM veteran**: "A real IR would let us run `core_lint`-grade checks before erlc, and open the door to optimisation passes."
- 🏭 **Operator**: "One representation to debug instead of `Document` plus a side IR."
- 🎨 **Language designer**: "This ADR is a partial IR by another name; do it properly once."

### Option D: Runtime state cell (process dictionary during self-dispatch)
- 🧑‍💻 **Newcomer**: "Mutable state that just works, like every other language I know."
- 🎩 **Smalltalk purist**: "Closest to real Smalltalk semantics: `self bump` mutates, full stop."
- ⚙️ **BEAM veteran**: "The process dictionary is a standard, cheap, process-local trick; gen_server itself uses it for `'$ancestors'`."
- 🏭 **Operator**: "Zero compile-time machinery to go wrong; the failure modes are all at runtime where I can trace them."
- 🎨 **Language designer**: "Deletes the entire hoisting problem instead of solving it."

### Option E: Make nested self-sends a compile error
- 🧑‍💻 **Newcomer**: "An error with a fix-it is better than a silent drop."
- 🎩 **Smalltalk purist**: no argument.
- ⚙️ **BEAM veteran**: "Explicit is better; force `{R, S} = …` into the source like I'd write in Erlang."
- 🏭 **Operator**: "Nothing to measure, nothing to break."
- 🎨 **Language designer**: "Honest about the boundary; today's warning is already half of this."

### Tension points
- BEAM veterans are split between A (ANF output) and D (process dictionary).
  D loses on ADR 0041/0042 and on making the existing verified IR
  meaningless; that is a decision already taken, not one this ADR reopens.
- Language designers are split between A and C. C is rejected for the same
  reasons as in ADR 0018 and ADR 0111: A obtains the expression-level
  property with one struct and one verifier variant, without a full IR.
- Operators and incrementalists prefer B. B is the status quo whose measured
  outcome is the table in §Context: ten fixes, three verifier panics, six
  crashes, nine silent drops still present. Its cost is not small; it is
  paid one bug report at a time.

## Alternatives Considered

### B. Keep the AST-directed planner and wire the remaining consumers
Each of the probe rows is fixable with one more `hoist_nested_self_sends`
call site or one more `is_conditional_selector` entry. **Rejected** because
the mechanism is structurally unable to reach two of the rows (BT-3399's
order-unsafe operands, and any position reached through `expression_doc`
inside a `Document`-only sink), because half of its output bypasses the
verifier, and because the ClassVars open-scope protocol would remain a
second copy of the same idea.

### C. Full typed Core Erlang IR
**Rejected** per ADR 0018 §Alternative 3 and ADR 0111 §Alternative 4; the
reasoning there still holds. This ADR's `ThreadedValue` is deliberately not
an expression IR: `value` is a `ValueRef` (a `Document` or a versioned
variable), and only the effectful prefix is structured.

### D. Runtime state cell
Have `safe_dispatch` read and write the current state from the process
dictionary so a nested self-send mutates "in place," and have the method
prologue/epilogue load and store it. **Rejected**: it violates ADR
0041/0042's explicit-threading contract, breaks the `{reply, R, NewState}`
invariant every other construct (NLR relay, ADR 0110 shadow writes,
`ensure:` cleanup) is built on, makes the ThreadedIr verifier unable to see
the effects it exists to verify, and produces generated code an Erlang
developer cannot read as gen_server code.

### E. Compile error for state-effecting sub-expressions
Reject any actor self-send, field write, or inline-threaded construct that
is not in statement, assignment-RHS, or return position. **Rejected** as the
primary decision: it is a language restriction motivated by a compiler
limitation, and Smalltalk developers would hit it immediately (`self a; b`).
Its honest core is adopted in §5: where a position genuinely cannot thread
(a real closure, an FFI boundary), the outcome is a diagnostic derived from
the verifier, never a silent drop.

## Consequences

### Positive
- The verifier sees every state effect, in every position: the "silent
  drop" class of bug becomes either correct code or a `VerifyError`.
- Evaluation order is preserved by construction; BT-3399's `Dropped` case
  and its warning are deleted.
- Two hoisting mechanisms (State planner, ClassVars open scope), two
  span-keyed registries, `HoistSink`, and roughly 500 lines of position-
  specific hoisting code are replaced by one `#[must_use]` type.
- Five overlapping "needs threading" predicates and four selector tables
  collapse to one semantic fact and one table, with a conformance test.
- The `whileTrue:` condition becomes verified IR, closing the three probe
  rows that panic today.

### Negative
- A large migration touching every statement-lowering site in the codegen
  crate; it must be phased so that each PR leaves CI green and the ADR 0111
  ≤3% gate is re-measured at each codegen-shape change.
- Generated code for methods with nested self-sends gains `let` bindings for
  preceding operands. This is bounded (one `let` per non-trivial operand
  preceding an effect) and is the shape an Erlang developer expects, but it
  is a visible change in `.core` snapshots.
- `#[must_use]` on `ThreadedValue` will produce warnings at every call site
  that compiles an expression and ignores state today; each must be
  reviewed, which is the point, but it is work.

### Neutral
- No AST, parser, runtime, or REPL changes. No user-facing syntax changes.
- ADR 0111's per-construct `verify()` granularity is unchanged; preludes are
  verified as part of whichever construct's frame they are spliced into.
- `ThreadedStmt::Statement` remains the opaque-embedding node; preludes are
  sequences of `Statement` and `Bind`, not a new statement kind. The only
  new IR surface is the `ConditionalLoop` condition fields (§6) and one
  `VerifyError` variant (§5).
- Single-source-of-truth modules this touches: `threaded_ir.rs` (the IR),
  `beamtalk-core::state_threading_selectors` (the one selector table),
  `beamtalk-core::semantic_analysis` `block_profiles` (the one effects
  fact). Nothing crosses the Rust/Erlang boundary; no new conformance
  fixture is needed beyond the selector-table test in §7.

## Implementation

Phased so that each step is independently landable and leaves the build
green. The regression matrix lands first so every later step flips rows
from expected-fail to pass rather than adding one-off tests.

0. **Regression matrix.** Turn the 47-shape probe into
   `stdlib/test/actor_self_send_position_matrix_test.bt` (+ fixture) with
   the currently-failing rows gated as expected-fail the way
   `ValueTypeMutationMatrixTest` gates BT-2371; add a self-send-position
   axis to the shared `mutation_corpus_*.bt` fragments so the metamorphic
   harness covers the positions across contexts; add the predicate
   conformance test that today's "must stay in sync" comment lacks.
1. **`ThreadedValue` and the first producer.** Add the type, `close()`,
   `VerifyError::StateEffectEscapesExpression`; make `generate_self_dispatch`
   the first producer and the Actor method body the first consumer, with
   the sequencing rule in `generate_expression` for `MessageSend`, binary
   operands, literals, interpolation, `Return`, and assignment values.
   `hoisted_self_send_results`/`hoisted_field_reads` become dead once the
   Actor-body call sites stop consulting them.
2. **Remaining State consumers.** Conditional arms, exception arms, Tier 2
   stateful blocks, loop bodies, loop-body local/field assignment RHS,
   cascades on `self`, `match:` scrutinee and arms, `ifNil:ifNotNil:`,
   `ifNone:`. Delete the planner, `HoistSink`, the `Dropped` warning.
3. **`ConditionalLoop` condition as IR.** The `whileTrue:`/`whileFalse:`
   condition prelude in the loop frame.
4. **Inline-threaded control flow as a producer.** A threaded conditional /
   `and:`/`or:` / `match:` / loop in expression position returns a prelude
   instead of a `{Result, State}` tuple document; closes the tuple-leak
   rows.
5. **ClassVars unification.** Class-method self-sends and class-var
   assignments produce preludes; delete `last_open_scope_result`,
   `OpenScopeResult`, `closed_expression_doc`, `capture_subexpr_sequence`,
   `split_subexpr_for_preamble`, `hoist_subexpr_splits`, `bind_args_to_temps`,
   `hoist_open_scope_receiver`/`_argument`. `ShadowWriteMissing` continues
   to see the class-var `Bind`s because they are now in the prelude.
6. **One predicate, one table.** `state_effects` semantic fact; collapse the
   codegen gates; unify the selector tables; delete the lint's copy.
   BT-3423 note: phases 2b/4/5b's `ThreadedValue`/prelude machinery already
   converged codegen's own "does this receiver sub-tree need threading"
   question onto one internal predicate
   (`CoreErlangGenerator::conditional_receiver_needs_threading`, a thin
   wrapper over `subexpr_needs_prelude` — formerly `contains_hoistable_self_send`)
   ahead of this phase, by a different mechanism than this section
   originally proposed (`subtree_needs_threading` reading `state_effects`
   directly). `state_effects` is added as specified for the consumers that
   *cannot* reach that codegen-internal predicate — `beamtalk-core` cannot
   depend on `beamtalk-codegen` (§Architecture), so the lint and any future
   LSP diagnostic need their own semantic-level fact — while codegen's
   remaining "does this literal block's body need mutation threading"
   duplication (four independently-inlined copies across
   `control_flow_has_mutations`, `enumeration_block_needs_threading`,
   `conditional_needs_mutation_threading`) collapses to one
   `block_arg_needs_threading` helper instead. The selector tables unify as
   planned (`state_threaded_block_arg_indices`, one source for
   `get_control_flow_threaded_vars` and the lint).
7. **Close-out.** `just verify-threaded-ir` runs the matrix corpus (the
   bootstrap-test corpus it names today contains no actor code); docs
   (`debugging.md` verifier table, `beamtalk-language-features.md`'s
   "Passing Blocks Through Class Methods"); ADR 0111 addendum pointing here;
   final ≤3% measurement.

Affected components: `crates/beamtalk-codegen/src/core_erlang/` (all
statement-lowering modules, `dispatch_codegen.rs`, `expressions.rs`,
`intrinsics.rs`, `threaded_ir.rs`), `crates/beamtalk-core/src/semantic_analysis/`
and `state_threading_selectors.rs`, `crates/beamtalk-lint` (selector table
consumer), `stdlib/test/`, `docs/development/debugging.md`.

## References
- Epic: BT-3413 (issues BT-3414 – BT-3424)
- Related issues: BT-3374, BT-3382, BT-3392, BT-3396, BT-3399, BT-3402,
  BT-3403, BT-3405, BT-3406, BT-3385 (the ten position-specific fixes this
  ADR replaces); BT-3141, BT-3155 (ADR 0111 epics)
- Related ADRs: ADR 0018 (Document tree, Alt 3), ADR 0041 (state-threading
  block protocol), ADR 0042 (immutable actor state), ADR 0110 (shadow
  write-through), ADR 0111 (ThreadedIr + verifier; §Verifier honesty,
  Addendum 7 "full-pipeline re-evaluation")
- Documentation: `docs/development/debugging.md` § ThreadedIr verifier,
  `docs/development/architecture-principles.md` § Duplication & the
  Shared-Leaf-Module Pattern
- Flanagan, Sabry, Duba, Felleisen. *The Essence of Compiling with
  Continuations.* PLDI 1993 (A-normal form).
