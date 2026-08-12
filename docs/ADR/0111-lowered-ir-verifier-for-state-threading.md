# ADR 0111: Mid-Level Lowered IR + Verifier for State Threading, Control Flow, and Non-Local Return

## Status
Implemented (2026-08-11)

## Implementation Tracking

**Epic:** [BT-3128](https://linear.app/beamtalk/issue/BT-3128) — Lowered IR + Verifier for State Threading (ADR 0111)
**Status:** Done

| Phase | Issue | Description | Size | PR |
|---|---|---|---|---|
| 1 | [BT-3129](https://linear.app/beamtalk/issue/BT-3129) | ThreadedIr types + verifier + test shim, gated by Phase A0 measurement | M | [#3319](https://github.com/jamesc/beamtalk/pull/3319) |
| 1 | [BT-3130](https://linear.app/beamtalk/issue/BT-3130) | Expand codegen snapshot corpus over the threading-mode matrix | M | [#3318](https://github.com/jamesc/beamtalk/pull/3318) |
| 2 | [BT-3131](https://linear.app/beamtalk/issue/BT-3131) | Unify version counters behind typestate VersionedVar + RAII branch guard | M | [#3322](https://github.com/jamesc/beamtalk/pull/3322) |
| 3 | [BT-3132](https://linear.app/beamtalk/issue/BT-3132) | Migrate while_loops + counted_loops; delete the 4 unpack debug_asserts | M | [#3323](https://github.com/jamesc/beamtalk/pull/3323) |
| 4 | [BT-3133](https://linear.app/beamtalk/issue/BT-3133) | Migrate list_ops (fold-shaped + early-exit) | L | [#3327](https://github.com/jamesc/beamtalk/pull/3327) |
| 4 | [BT-3134](https://linear.app/beamtalk/issue/BT-3134) | Migrate conditionals + exception_handling | M | [#3326](https://github.com/jamesc/beamtalk/pull/3326) |
| 4 | [BT-3135](https://linear.app/beamtalk/issue/BT-3135) | Migrate gen_server state threading + NLR + ShadowWriteMissing contract; delete the 2 routing debug_asserts | L | [#3329](https://github.com/jamesc/beamtalk/pull/3329) |
| 5 | [BT-3136](https://linear.app/beamtalk/issue/BT-3136) | Close-out: verifier CI wiring, docs, status → Implemented | S | [#3335](https://github.com/jamesc/beamtalk/pull/3335) |

All seven phases landed on `main`. `just verify-threaded-ir` (BT-3136) now runs
`threaded_ir::verify()` over the full `stdlib/test/*.bt` +
`stdlib/bootstrap-test/*.btscript` corpus in CI, and the six original
routing/threading `debug_assert!`s this epic set out to replace are gone from
source — see `docs/development/debugging.md` § ThreadedIr verifier for the
`VerifyError` variant reference.

## Addendum (2026-08-11): Delivered vs. designed

This ADR's status stays **Implemented** — every phase in the table above
shipped, is load-bearing, and stays. This addendum corrects the record on
*what* shipped: BT-3128 delivered this ADR's own **Alternative 1b / Steelman
Option B2** ("record decisions as they are made," §Alternatives Considered
#1b) under Option A's name and pipeline diagram. `ThreadedIr` is a
**verification-only side channel**, not the emission input the §Pipeline
shape diagram commits to. `Document` construction still happens directly
from AST + generator state, on a separate, unconnected path; the
classifier/emitter dual computation the ADR's central argument for Option A
over B2 says the IR should *remove by construction* (§Decision, "Why A
still wins — narrowly") still exists — it is *checked*, not removed.
[BT-3141](https://linear.app/beamtalk/issue/BT-3141) is the follow-up epic
that completes Option A as originally designed; this section is the honest
accounting that motivates it, plus the design commitments BT-3141's
children build against.

### What shipped (Option A's letter, not its substance)

- **Counter unification (Phase A2 / BT-3131).** Real, in production.
  `VersionedVar`/`VersionCounter` is the single implementation behind all
  three of `CoreErlangGenerator`'s formerly-independent counters, with
  frame identity and the `self_version` save/restore hole closed. This
  piece never depended on the IR being the emission input — it is
  typestate hardening (§Alternatives #5) that stands on its own.
- **`debug_assert!` deletion.** All six of the originally-named
  routing/threading `debug_assert!`s are gone from source, per the table
  above (BT-3132's four unpack asserts, BT-3135's two routing asserts).
- **Snapshot corpus expansion (Phase A3 / BT-3130)** and **CI wiring**
  (`just verify-threaded-ir`, BT-3136) both landed as designed.
- **The verifier itself runs, and finds real invariant violations** — but
  only in the sense §Verifier honesty already scoped: it checks
  *hand-assembled fixtures built from the generator's own already-made
  decisions*, not the generator's actual `Document`-construction path.

### What did not ship: single-sourcing ("one node, one emitter")

The ADR's decisive argument for Option A over B2 (§Decision, "Why A still
wins — narrowly") was that "B2 checks that two independently-computed
decisions agree; A removes the second computation... A's IR is the decision
record *and* the emission input, so an unrecorded decision cannot emit."
None of the seven phases actually wired `ThreadedIr` into emission. The
concrete evidence, all in
`crates/beamtalk-core/src/codegen/core_erlang/threaded_ir.rs` unless noted:

- **Every production `verify_*` entry point is a fixture-and-discard
  wrapper**, not a lowering step. `verify_loop_unpack_invariant`,
  `verify_tuple_acc_unpack_invariant`, `verify_tuple_acc_value_type_exclusion`,
  `verify_nested_list_op_stateacc_compat`, and `verify_branch_frame_linearity`
  each take the generator's *already-resolved* decision (a `bool`, a
  `ThreadingMode`, a `usize` gate-slot count) as a parameter, build a
  minimal `ThreadedIr` fragment that encodes that same decision, call
  `verify()` on it, and return `Vec<VerifyError>` — the fragment is never
  returned to the caller and never touches `Document` construction. The
  real `Document` for the same construct is built separately, directly
  from AST + generator state, by the unmigrated emitter code sitting right
  next to the `verify_*` call.
- **The `lower_and_render` test shim is explicitly not full-fidelity**
  (`threaded_ir.rs`, doc comment on `lower_and_render`): "`Threaded` and
  `NlrCatch` render at **skeleton fidelity only** — full `letrec`/try-catch
  scaffolding requires fresh-variable allocation from `CoreErlangGenerator`
  (`fresh_temp_var`, `alloc_nlr_catch_vars`) that this pure
  `&[ThreadedStmt] -> Document` shim deliberately does not have access to."
  A shim that cannot allocate fresh variables or NLR catch scaffolding
  cannot be the real emitter for any construct that needs them — which is
  most of what's in scope.
- **`EarlyExitGateSlotMismatch` is a tautology today**
  (`threaded_ir.rs:1097-1106`): the check compares the synthesized
  `ThreadingMode::TupleAcc(gate_slots)` against the `TupleAccUnpack` node's
  own `gate_slots` — but `verify_tuple_acc_unpack_invariant` builds *both*
  from the single `gate_slots` argument it received, so they cannot
  disagree at any real call site. Only a hand-built-IR unit test
  (`verify_early_exit_gate_slot_mismatch_fires_when_node_disagrees_with_mode`)
  exercises the mismatch branch. The check has no second, independently
  derived source to disagree with — exactly the "two independently-computed
  decisions" shape the ADR's own argument for A over B2 says the IR should
  eliminate, still present here in a different guise: the fixture *encodes*
  the single decision twice instead of computing it twice, so it can't
  catch a real divergence either way.
- **Branch-frame linearity is scaffolding, not a live regression guard**
  (`threaded_ir.rs:1204-1214`, `verify_branch_frame_linearity`): the doc
  comment states plainly that because the caller always allocates a fresh,
  distinct `FrameId` per arm and the check only synthesizes a `Bind` chain
  from a scalar `final_version` *count* (not the real per-arm mutation
  sequence the generator produced), "no two arms can ever collide at any of
  today's nine call sites... this smoke-tests the verifier's
  `FrameId`/linearity plumbing... but cannot yet catch a real generator
  bug." The comment attributes "giving it that teeth" to "BT-3135+ as it
  migrates the mutation-`Bind` emission sites themselves onto
  `ThreadedIr`" — but BT-3135 (Phase D, gen_server state threading + NLR +
  `ShadowWriteMissing`) shipped as Done in the table above without doing
  that migration; the promise landed on `main` unowned, attached to no
  open issue, until this addendum names it via BT-3141.
- **NLR relay is unmodeled at its one production emission site.**
  `wrap_body_with_nlr_catch` (`mod.rs:2660`) is, per its own comment, "the
  true call site `ThreadedStmt::NlrCatch` faithfully models" — but the
  function builds its `try`/`catch` scaffolding directly with
  `docvec!`/`leaf::*` and explicitly does *not* construct or verify a
  `ThreadedIr` fragment: "No standalone `verify()` call here: a lone
  `NlrCatch` with no `Bind` can never trigger any `VerifyError`... so
  constructing one on every NLR-catch wrap — a hot path — would pay a real
  allocation for a check that can't fire." That reasoning is sound *for the
  check as designed*, but it also means the NLR boundary the ADR's IR was
  meant to model (`ThreadedStmt::NlrCatch`, §The IR) has never been
  constructed at a real call site — the `ShadowWriteMissing` contract
  (§Worked example) is verified only via the class-var-`Bind` fixtures at
  the mutation sites, never jointly with the NLR boundary it's stated
  against.
- **`ShadowWriteMissing`'s Erlang-side half is out of Rust-side reach by
  design** (§Verifier honesty, unchanged by this addendum) — noted here
  only because it means even a fully single-sourced Rust-side `ThreadedIr`
  would still leave that cross-boundary conformance fixture as a separate
  deliverable, not something single-sourcing subsumes.

**Net effect:** the classifier/emitter dual computation the ADR set out to
remove by construction is still a dual computation. What changed is that
one side of it (the generator's decision) is now also *checked* against a
purpose-built model of itself, via the counter-unification and verifier
machinery — real diagnosis-quality value (§Consequences, Positive), but the
B2 shape, not the A shape. Everything B2 was designed to deliver (§Steelman
Option B2) is what actually shipped; the "IR's remaining unique
contribution" the ADR's own language-designer steelman voice named —
"single-sourcing (one node, one emitter) and the counter unification" — the
counter-unification half shipped, the single-sourcing half did not.

### Re-scope decision

[BT-3141](https://linear.app/beamtalk/issue/BT-3141) ("Epic: ThreadedIr as
emission input — complete ADR 0111 single-sourcing") is the re-scope that
completes Option A as originally designed: lowering builds a real
`ThreadedIr` from real generator state (not a fixture reconstructed from a
decision already made), one renderer produces the `Document` from that real
IR, and the verifier runs on the true emission input — closing the gap this
addendum documents. This ADR's status stays **Implemented**; BT-3141 is
**in progress** (this addendum's own issue is its first landed child) and
tracked as a follow-up epic against this same architectural decision, not a
new ADR, because it changes no decision recorded above — it finishes
building what §Decision and §Pipeline shape already committed to.

### Renderer design sketch

The reason `lower_and_render` stops at skeleton fidelity is concrete, not
incidental: rendering several `ThreadedStmt` shapes to real Core Erlang
needs generator context a pure `&[ThreadedStmt] -> Document` function
cannot have —

- `fresh_temp_var` (`util.rs`) — fresh-variable allocation for `letrec`
  loop scaffolding and intermediate binds;
- `alloc_nlr_catch_vars` (`mod.rs:2587`) — the class/error/stack/token/
  value/state variable set every NLR `try`/`catch` wrap allocates fresh,
  per wrap;
- loop-context `StateAcc`/`State` prefix selection — `in_loop_body`/
  `in_hybrid_loop` (`mod.rs:1436`, `:1442`) decide at *render time* whether
  a `State` counter's prefix renders as `StateN` or `StateAccN` for the
  same `VersionedVar` (§The IR's `VersionedVar` note: "prefix rendering is
  a function of (counter, loop context), decided at Document-construction
  time, not stored in the IR" — a decision the renderer needs generator
  state to make, by the ADR's own design).

BT-3141's children share one renderer shape instead of each migration issue
inventing its own generator-context threading:

```rust
/// A narrow, purpose-built borrow of CoreErlangGenerator's rendering-time
/// facilities — NOT the god object. RenderCtx exposes exactly the methods
/// the renderer needs (fresh-variable allocation, NLR scaffolding
/// allocation, loop-context prefix selection) and nothing else generator
/// state offers (no direct field access, no AST-directed emission paths).
/// Constructed per top-level render call as `RenderCtx { gen: &mut
/// CoreErlangGenerator }`(or an equivalent narrow-trait borrow — the
/// exact mechanism is a BT-3144 implementation decision; the *shape* this
/// ADR commits its migration issues to is "one render entry point per
/// ThreadedStmt kind, taking &mut RenderCtx, no second decision path").
struct RenderCtx<'gen> {
    gen: &'gen mut CoreErlangGenerator,
}

impl RenderCtx<'_> {
    fn fresh_temp_var(&mut self, base: &str) -> String { .. }
    fn alloc_nlr_catch_vars(&mut self) -> NlrCatchVars { .. }
    /// Resolves a VersionedVar's State/ClassVars/SelfVt prefix against
    /// current loop context (in_loop_body / in_hybrid_loop) — the one
    /// piece of "render depends on context, not just on the IR node"
    /// the ADR's VersionedVar doc comment already calls out.
    fn resolve_prefix(&self, var: &VersionedVar) -> RenderedPrefix { .. }
}

/// Replaces lower_and_render as the real emitter: full-fidelity, not
/// skeleton-fidelity. Same &[ThreadedStmt] input shape so migration
/// issues change their emitter's *body*, not its call sites' argument
/// shape.
fn render(ir: &[ThreadedStmt], ctx: &mut RenderCtx) -> Document<'static>;
```

This is a sketch to align BT-3141's children on one shape, not an
implementation — no production code changes in this addendum. Open
questions left to BT-3144 (renderer foundation) rather than pre-decided
here: whether `RenderCtx` wraps `&mut CoreErlangGenerator` directly or a
narrow trait it implements (the trait form would let unit tests construct
a `RenderCtx` without a full generator instance, which the god-object
concern this ADR already raised — §Constraints, "`CoreErlangGenerator`
~90 fields" — makes worth deciding deliberately rather than defaulting
into); and whether `resolve_prefix` needs its own `VerifyError` variant for
the case a `VersionedVar` reaches render time with no resolvable loop
context (today that can't happen because rendering is fused with the
decision that also picks the context; once render is a separate pass over
a real IR, "unresolvable context" becomes a representable — and thus
checkable — failure mode).

### Measurement gate, restated

Phase A0's prototype (§Implementation, "Phase A0 — Measurement gate")
measured build/verify/render-and-discard: `prototype_direct_params_ir`
constructs a `ThreadedIr` fragment, verifies it, and throws it away — cost
measured was allocation + verification, with **no rendering of real output
from it**, because none of the seven delivered phases render from the IR at
all. BT-3141 changes that premise: IR construction and full-fidelity
rendering go on the hot path for real, for every construct a migration
issue converts. The original gate is re-affirmed against that harder
target, not loosened:

- **Threshold, unchanged:** end-to-end `beamtalk build` on the fixed
  fixture set (§Implementation, Phase A0) regresses **≤ 3%**.
- **Checked at the pilot-migration flag flip** —
  [BT-3145](https://linear.app/beamtalk/issue/BT-3145) ("Pilot migration:
  while_loops + counted_loops emit through ThreadedIr... gated on ≤3% build
  measurement") is where this is first measured against real
  construct-and-render cost, not fixture-and-discard cost.
- **Checked cumulatively at each later phase** — every subsequent
  migration issue re-measures against the same fixture set and the same
  cumulative 3% budget (not a fresh 3% per phase), since each phase adds
  its own construct-and-render cost on top of the prior phases'.
- **Descope path, named:** if the cumulative measurement fails the gate at
  any phase, the response is the same one this ADR always named for A0
  failure (§Alternatives #1b): keep the side-channel verification exactly
  as it exists today (real value, already shipped, already cited above),
  abandon the emission-input migration for the remaining unconverted
  constructs, and close BT-3141 documenting which constructs converted
  before the gate tripped and which stay on the B2 shape permanently. A
  partial conversion is an acceptable, explicitly pre-planned outcome, not
  a failure requiring rollback of already-converted constructs.

## Addendum 2 (2026-08-11): Loop-family blockers for BT-3145 — condition/case-split node design + Bind naming reconciliation

[BT-3145](https://linear.app/beamtalk/issue/BT-3145) (pilot migration:
`while_loops.rs` + `counted_loops.rs` onto the BT-3144 renderer) attempted
to point a real call site at [`render`](#renderer-design-sketch) and found
two concrete, empirically-verified gaps that block it as scoped — not new
invariant classes to check, but missing IR/renderer expressiveness the
prior addendum's "Renderer design sketch" and BT-3144's
`render`/`render_loop_letrec` did not anticipate. BT-3145's investigation
comment (2026-08-11T19:02) has the full empirical trail, including
compiled Core Erlang output; this section restates the evidence with
source citations, proposes a concrete design for each gap, evaluates the
descope alternative this ADR's own Alternative 1b names as the pre-planned
fallback, and gives a recommendation
[BT-3153](https://linear.app/beamtalk/issue/BT-3153) (this section's own
issue) can hand directly to a re-attempt at BT-3145. All line numbers below
are against `crates/beamtalk-core/src/codegen/core_erlang/` unless noted,
current as of `main` at commit `89bb697` (BT-3151, the commit after the
BT-3144 renderer landing — BT-3151 shifted line numbers in
`while_loops.rs` and `mod.rs`, and the citations below reflect that).
Citations anchor on **symbol names**; the line numbers are approximate
hints as of that commit, expected to drift as adjacent work lands (e.g.
BT-3154, in flight at the time of writing, deletes the A0 prototype block
and the `check_loop_unpack_invariant` call sites from the exact regions
Gap 1 cites) — resolve by symbol name first, treat `:NNN` as a hint.

### Gap 1 — no condition/case-split loop node

**Evidence.** `render_loop_letrec` (`threaded_ir.rs:1089-1160`) emits
exactly one skeleton: `letrec Name/N = fun (Params) -> <body_doc> apply
Name(final_args) in apply Name(outer_args)` — unconditional tail
recursion, no condition, no `case`, no exit arm. Every real while/counted
loop call site instead interleaves a condition test and a two-arm `case`
around that same recursive skeleton:

- `generate_while_loop_direct` (`control_flow/while_loops.rs:287-429`):
  allocates a fresh `CondFun` closure (`cond_var = self.fresh_temp_var
  ("CondFun")`, `:329`), builds `let CondFun = fun (Params) -> <cond_doc>
  in case apply CondFun (Params) of <'true'|'false'> when 'true' -> `
  (case header at `:380`), then the loop body, then a second arm —
  `<'false'|'true'> when 'true' -> <exit_stateacc> end ` (`:403-412`) —
  before the closing `in apply 'while'/N(initial_args)`. `negate` (the
  `whileFalse:` case) swaps which literal each arm tests, not the shape.
  `generate_while_loop_hybrid` (`:439` onward) is the identical shape
  under `Hybrid` mode.
- `generate_counted_stateful_loop`/`_direct`/`_hybrid`
  (`control_flow/mod.rs:2702-2882`) use a *different* condition
  mechanism — a direct comparison
  (`case call 'erlang':'=<'(Counter, N) of`, `counted_loops.rs:74-79`)
  rather than a `CondFun` closure — but the *same* overall
  letrec-with-case-split-and-exit-arm skeleton, already factored out once
  in production as `CountedLoopFrame`
  (`control_flow/mod.rs:1112-1138`): `preamble`, `fn_name`,
  `continue_header` (condition test up to and including the chosen arm's
  `-> `), `next_counter`, `initial_counter`, `false_arm` — six
  `Document<'static>`/`String` fields threaded uniformly through
  `generate_counted_stateful_loop` and its direct/hybrid variants
  (`:2702-2772`, `:2778-2882`). Counted loops also carry a gensym'd loop
  *index* (`frame.counter`, `fresh_temp_var("loopidx")`,
  `control_flow/mod.rs` `CountedLoopFrame::counter` doc, `:1129-1137`) as
  an extra fun parameter that is never a `Bind` target or source — it is
  threaded by a raw "next value" expression (`next_counter`, e.g. `call
  'erlang':'+'(Counter, 1)`), structurally closer to `AccParam` (an
  unversioned, generator-allocated identity outside `VersionedVar`'s
  producer/consumer bookkeeping — see `threaded_ir.rs:244-268`) than to a
  threaded local.

No `ThreadedStmt` variant today can express any of this: `Threaded` only
carries `mode`/`frame`/`body`/`produces`, all of which presuppose
`render_loop_letrec`'s unconditional shape. This is exactly why BT-3144's
own dual-run tests (`threaded_ir.rs:3114-3134`, the `dual_run_*` block's
own comment) say outright that "there is no single production function
today that emits *only* the letrec skeleton these tests check," and
hand-author a condition-free skeleton to compare `render()` against
instead of a real call site's actual output — the tests prove `render()`
reproduces a hand-written reference, not that it reproduces production.

**Design options.**

1. **New `ThreadedStmt::ConditionalLoop` variant; condition and exit arm
   stay opaque pre-rendered `Document`s (the `NlrCatch` precedent);
   skeleton shape (frame/mode/body/produces) stays structural — Recommended.**

   ```rust
   ThreadedStmt::ConditionalLoop {
       /// Static per-construct function name — "while", "loop", "repeat"
       /// — never gensym'd in production (see Gap 2's "loop-fn-name"
       /// finding). Supplied by the caller, mirroring
       /// `CountedLoopFrame::fn_name`, whose field type this matches
       /// (`control_flow/mod.rs:1116`).
       fn_name: String,
       mode: ThreadingMode,
       frame: FrameId,
       /// Present only for counted loops. Not a VersionedVar — see
       /// AccParam's doc comment for why an unversioned, single-alloc
       /// identity is modeled outside the Bind-linearity machinery.
       counter: Option<LoopCounter>,
       /// Opaque, AST-directed condition scrutinee + the case's chosen
       /// continue-arm pattern, up to and including "-> " — e.g. "let
       /// CondFun = <cond> in case apply CondFun (Params) of <'true'>
       /// when 'true' -> " or "case call 'erlang':'=<'(I, N) of <'true'>
       /// when 'true' -> ". Built by the SAME condition-codegen call
       /// production already runs (`with_branch_context(|this| ..)`,
       /// `while_loops.rs:360-370`) — the IR does not re-derive
       /// condition semantics. Exactly the `NlrCatch` precedent: "a
       /// similar shape may fit" per the issue body, confirmed here.
       continue_header: Document<'static>,
       body: Vec<ThreadedStmt>,
       produces: Vec<VersionedVar>,
       /// Opaque exit arm: pattern + exit value + "end " — e.g.
       /// "<'false'> when 'true' -> {'nil', _ExitSA8} end " (built by
       /// `generate_exit_stateacc`). ORDERING CONSTRAINT: must be
       /// constructed AFTER the body IR is lowered — its `ExitSA` temps
       /// come from the same module-wide `fresh_temp_var` counter as the
       /// body's rebind temps, and legacy mints body temps first (see
       /// Gap 2's option analysis — mint order decides byte-identity).
       exit_arm: Document<'static>,
       span: Span,
   }
   ```

   `verify()` treats this exactly like `Threaded` today: push `frame`,
   push `mode`, walk `body`, `check_use` every `produces` entry, pop both
   — no new `VerifyError` variant is required. `verify()` cannot, and is
   not meant to, check that `continue_header`'s embedded polarity
   actually agrees with which arm holds `body` vs. which holds
   `exit_arm`'s value.

   The two opaque arms deserve *separate* opacity justifications — they
   are not the same case:

   - **`continue_header`'s opacity is sound on option 3's own grounds:**
     the condition body is ordinary AST-directed expression codegen with
     no state-threading content of its own — the same class of opaque
     embed as `NlrCatch`'s body (§Verifier honesty; the `NlrCatch` doc
     comment's own reasoning for why a lone `NlrCatch` "can never trigger
     any `VerifyError`"). Nothing state-threading-relevant is hidden.
   - **`exit_arm`'s opacity hides genuine state-threading content, and
     this is a named, deliberate pilot limitation.** `exit_arm` is
     `generate_exit_stateacc`'s output: it repacks threaded locals and
     mutated fields into the `StateAcc` map (`control_flow/mod.rs:741`,
     `:753` — `maps:put` chains over fresh `ExitSA` temps). That is
     state-threading work on the loop's *exit path* — exactly the
     boundary class where BT-3140/BT-3150 lost class-var mutations. An
     IR that models the body structurally but the exit repack opaquely
     cannot see that boundary; BT-3145 accepts this as an explicit
     §Verifier-honesty-class limitation: **the loop exit repack is
     unverified by design in the pilot.** A future refinement can model
     the repack structurally — a `Bind` chain of `BindOp::Put`s with
     `Gensym`-prefixed targets (which Gap 2's recommended option 2 makes
     directly expressible) — and is deferred, not rejected: the pilot's
     hard gate is byte-identity, and a structural exit model adds no new
     *check* until `ClassVars`/`State` `Bind`s flow through loop exits
     jointly with NLR boundaries (Phase D territory); modeling it in the
     pilot would grow scope without verification payoff yet.

   Rendering: `render_loop_letrec`'s existing `param_list`/`final_args`/
   `outer_args` plumbing (`:1104-1136`) is unchanged and reusable as a
   shared helper — only the middle of the `fun` body (currently just
   `body_doc` then `apply Name(final_args)`) needs `continue_header`
   prepended and `exit_arm` appended around that same pair. Concretely:
   factor `render_loop_letrec`'s three closures (`outer_args`,
   `render_in_loop_body` building `param_list`/`body_doc`/`final_args`)
   into a shared function taking an optional `(continue_header, exit_arm)`
   pair, so the existing bare-`Threaded` shape (`DirectParams`/`Hybrid`
   today, still exercised by BT-3144's own tests) and the new
   `ConditionalLoop` shape share one implementation instead of two
   near-duplicates (CLAUDE.md's no-duplicate-implementations rule applies
   within this file, not just across the Rust/Erlang boundary). Once
   `ConditionalLoop` is real, evaluate whether the bare unconditional
   `Threaded`-for-loops shape has any remaining production caller — if
   not (current evidence says it doesn't; it exists only for BT-3144's
   own dual-run proof), delete it rather than keep two loop-rendering
   paths alive.

   BT-3144 dual-run test changes: the three `dual_run_*` tests
   (`threaded_ir.rs:3136-3216`, `:3218-3300`ish, `:3371`+) need to
   hand-author the REAL condition/case shape (mirroring
   `generate_while_loop_direct`'s literal `docvec!` shape, not a
   condition-free skeleton) as their "legacy" comparison side, and prove
   `render()`'s new `ConditionalLoop` arm reproduces it byte-for-byte —
   closing the exact gap the tests' own comment names today ("no single
   production function... emits only the letrec skeleton"). This is
   necessary regardless of which design option is chosen, since it is the
   dual-run harness's own honesty issue, not specific to option 1.
   **Effort: S** — one new enum variant, one new `render`/`verify` arm
   pair, a shared-helper refactor of existing code, three test rewrites.

2. **Extend `Threaded` itself with `Option` condition/exit fields instead
   of a new variant.** Same runtime shape as option 1, expressed as
   `Threaded { mode, frame, body, produces, span, condition:
   Option<LoopCondition> }` where `LoopCondition` bundles
   `fn_name`/`counter`/`continue_header`/`exit_arm`. **Rejected in favor
   of option 1**: every non-loop `Threaded` use today (conditionals'
   `with_branch_context` arms, `on:do:`/`ensure:` bodies — BT-3134,
   `threaded_ir.rs` module docs) is condition-free by construction, so an
   `Option` field would be `None` at every non-loop call site forever —
   an enum variant makes "this construct's condition/exit shape" a type
   distinction the compiler enforces (`match` exhaustiveness) rather than
   an invariant every non-loop caller must remember to leave `None`,
   consistent with this ADR's own repeated preference for "unrepresentable
   invalid states" over runtime-checked optional fields (§Alternatives
   #5, the `VersionedVar` typestate precedent).

3. **Model the condition as a nested, fully-structural `ThreadedStmt`
   sub-tree instead of an opaque `Document` (no `NlrCatch`-style
   embedding at all).** Would require `ThreadedStmt` to represent
   arbitrary Beamtalk boolean-expression codegen (block bodies,
   comparisons, arbitrary sends) as IR nodes — exactly the "full-pipeline
   IR covering all of Core Erlang codegen" ADR 0111 §Constraints already
   names as an explicit non-goal, absorbing ADR 0018's rejected
   "over-engineered... we don't transform or optimize the IR, we just
   emit it" reasoning (§Alternatives #4). **Rejected** — the condition
   body is ordinary AST-directed expression codegen with no
   state-threading content of its own; modeling it structurally buys no
   new `verify()` check (a condition's *result* is consumed structurally
   the moment the case executes; nothing about its *internals* is
   state-threading-relevant) at real IR-scope-creep cost.

### Gap 2 — naming-scheme mismatch (`fresh_temp_var` vs. `VersionedVar`)

**Evidence.** Real per-iteration variable rebinds go through
`generate_direct_var_update_in_loop`
(`control_flow/mod.rs:3359-3403`): `let new_var =
self.fresh_temp_var(&CoreErlangGenerator::to_core_erlang_var(&id.name))`
(`:3389-3390`) — producing names like `_Sum7`. The exit-`StateAcc`
rebuild is the same story: `generate_exit_stateacc`/
`_full_extract` (`control_flow/mod.rs:727-833`) call
`generator.fresh_temp_var("ExitSA")` once per repacked field (`:741`,
`:753`, `:798`, `:816`), producing `_ExitSA8`-style chains. Both go
through `fresh_temp_var` → `VariableContext::fresh_var`
(`util.rs:378-384` → `variable_context.rs:103-117`), whose `var_counter:
usize` (`variable_context.rs:36`) is a **single field on one
`VariableContext`, itself a single field on one `CoreErlangGenerator`
constructed once per *module* compile** (`CoreErlangGenerator::new`,
`mod.rs:1750-1755`, doc comment: "Creates a new code generator for the
given module name") — every `fresh_temp_var`/`fresh_var` call across
*every method in that module* shares the one counter. This is even wider
than BT-3145's investigation comment's "global counter shared across all
codegen in a method" framing: it is shared across the whole module's
codegen, not reset per method.

`VersionedVar::render_name` (`threaded_ir.rs:231-241`) instead names
`Local`-prefixed vars via `super::util::versioned_var(&core_name,
self.version)` — a bare-prefix, per-name sequential scheme (`Sum1`,
`Sum2`, …) wholly independent of `fresh_temp_var`'s counter. BT-3144's own
dual-run tests hard-code this literally as their "legacy" comparison
fixture (`threaded_ir.rs:3142-3159`: `leaf::var("Sum1")`, `"Count1"`
literals) — confirming the current tests assert `render()` against a
naming scheme production never actually emits, not merely an
under-specified one.

A related, smaller finding folds into the same fix: `render_loop_letrec`'s
loop-function name is *also* gensym'd (`ctx.fresh_temp_var("Loop")`,
`threaded_ir.rs:1096`, producing `_Loop3`-style names), but production
never gensyms this name — `while_loops.rs:334` uses the static literal
`"while"`, and `CountedLoopFrame::fn_name` (`control_flow/mod.rs:1116`)
carries static literals (`"repeat"`, `"loop"`). This is folded into Gap
1's `ConditionalLoop::fn_name: String` field (supplied by the caller,
never minted by `render`) rather than treated as a third, separate gap.

**Design options.**

1. **Render-time memoizing gensym cache on `RenderCtx` — REJECTED on
   composition grounds: it inverts gensym mint order when combined with
   Gap 1's recommended design (analysis at the end of this option; the
   deciding argument for option 2 follows there).** `RenderCtx`
   gains a `gensym_names: HashMap<VersionedVar, String>` field (cleared
   implicitly — a fresh `RenderCtx` is already constructed per real call
   site today, per `RenderCtx::new`'s existing usage in
   `lower_and_render`, `threaded_ir.rs:1029-1033`, so no explicit reset
   logic is needed). `RenderCtx::resolve_prefix`
   (`threaded_ir.rs:904-915`) gains a branch: for `VersionPrefix::Local
   (name)` at `version > 0` (version 0 is always the loop's declared
   `fun` parameter — never gensym'd in production, e.g. `while_loops.rs`'s
   plain `param_names` — so it keeps rendering via the existing
   `to_core_erlang_var` path unconditionally), look up `(var, name)` in
   the cache; on miss, call `self.fresh_temp_var(&to_core_erlang_var
   (name))`, insert, return. Every later reference to the *same*
   `VersionedVar` value — as a later `Bind`'s `source`, as a `produces`
   entry, as `Return`'s value — is a cache hit, reproducing the exact
   "mint once, reuse via scope lookup thereafter" discipline
   `generate_direct_var_update_in_loop`'s `self.bind_var(&id.name,
   &new_var)` (`:3391`) already gives production, without needing the IR
   itself to carry a literal string.

   This is not a new naming strategy: `render_loop_letrec`'s `fn_name`
   (`:1096`) and `render_nlr_catch`'s `token_var` (`:1269`) *already*
   mint via `ctx.fresh_temp_var` at render time rather than storing a
   literal in the IR — option 1 makes `Bind`-target naming consistent
   with two mechanisms `render()` already uses, rather than introducing
   a third. Correctness rests on one condition, worth stating explicitly:
   `FrameId` uniqueness (a fresh, distinct frame per loop body/branch arm
   — already a ADR 0111 design requirement, §The IR's `FrameId` doc
   comment) means two unrelated `VersionedVar`s can never collide in the
   cache even if they happen to share `(prefix, version)`, so no explicit
   per-construct cache-scoping logic is needed beyond "one `RenderCtx`
   per lowered-and-rendered construct region," which is already how every
   current call site is shaped.
   **Why this fails when composed with Gap 1's recommended design** (the
   reason it is rejected despite its smaller type surface):
   `fresh_temp_var` draws from **one module-wide counter**, so
   byte-identity depends on mint *order*, not just on which names are
   minted. Legacy order inside `generate_while_loop_direct` is: (1)
   `CondFun` (`:329`), (2) condition internals (`:360-370`), (3) **body
   temps** via `generate_threaded_loop_body` (`:384`), (4) the **`ExitSA`
   chain** via `generate_exit_stateacc` (`:391`). Gap 1's
   `ConditionalLoop` carries `continue_header` and `exit_arm` as
   pre-rendered `Document` *fields* — both fully built at
   node-construction (lowering) time — while this option defers body-temp
   minting to `render()` time, strictly after node construction. The
   order becomes 1, 2, **4, 3**: steps 3 and 4 swap, every gensym number
   after the swap shifts, and output is no longer byte-identical — the
   epic's hard acceptance criterion. A "lower and render each construct
   in one place" discipline does not cover this: the inversion is
   *intra*-construct, not interleaving with other codegen. Rescuing
   option 1 would require making `exit_arm` lazily rendered (a thunk
   field or a two-phase render API) — extra API complexity that erases
   this option's "less churn" advantage, so it is rejected rather than
   patched.

2. **Lowering-time pre-allocation: new `VersionPrefix::Gensym(String)`
   variant, IR carries the literal rendered name — Recommended.** The
   lowering pass
   (which already needs `&mut CoreErlangGenerator` to call
   `fresh_temp_var` at the same point production does today) calls it
   once per rebind and stores the resulting string directly in the
   `Bind`'s target `VersionedVar` via `VersionPrefix::Gensym(name)`;
   `render_name()`/`resolve_prefix` for `Gensym` returns the stored
   string verbatim, no allocation at render time. **Trade-offs against
   option 1:** (+) the IR becomes self-describing — a `ThreadedIr`
   fragment can be printed/debugged without a live generator, which
   matters if a future consumer (LSP tooling, per this ADR's User Impact
   table) wants to inspect lowered-but-not-yet-rendered IR; (+) no
   render-time cache invariant to maintain. (−) widens `VersionPrefix`'s
   contract — it stops being purely "a counter identity render derives a
   name from" and gains a second mode "a name render must reproduce
   verbatim," a real API-surface cost for every existing `match
   prefix { .. }` site (`render_name`, `resolve_prefix`, and any future
   consumer); (−) requires lowering itself (not just rendering) to run
   with live generator access at exactly the right point in the AST
   walk — a coupling cost that turns out to be **already paid**: Gap 1's
   own recommended design requires `&mut CoreErlangGenerator` at lowering
   time regardless, to build the opaque `continue_header` (condition
   codegen) and `exit_arm` (`generate_exit_stateacc`) `Document`s, so
   option 2 adds no coupling Gap 1 hasn't already introduced.
   **Effort: S–M. Recommended — the deciding argument is composition
   with Gap 1's design.** Because option 2 mints during *lowering*, mint
   order is controlled by lowering-code order alone: mint `CondFun`,
   render the condition, lower the body's `Bind`s (minting each rebind
   temp in encounter order, exactly where legacy
   `generate_direct_var_update_in_loop` mints today), and only then build
   `exit_arm` — reproducing legacy mint order (1)-(2)-(3)-(4) by
   construction, where option 1 structurally inverts it (see option 1's
   closing analysis). The lowering pass must still preserve that order —
   **lower the body IR before constructing `exit_arm`** — but under
   option 2 this is an ordinary code-ordering fact of one function,
   enforced naturally by writing the lowering to mirror the legacy
   function it replaces, not a cross-phase constraint spanning
   node-construction and render time. The remaining cost is the
   `VersionPrefix::Gensym` type-surface widening, which buys the
   self-describing-IR benefit noted above — and makes a future
   structural model of the exit repack (Gap 1's `exit_arm` limitation)
   directly expressible as `Bind`s with `Gensym` targets.

3. **Change production naming to match `VersionedVar::render_name()`'s
   `Sum1`/`Sum2` scheme (i.e. stop using `fresh_temp_var` for loop
   locals).** **Rejected outright** — this is a real behavior change
   to compiled output, violating both ADR 0111 §Constraints' "No
   behavior change — every phase must produce byte-identical output over
   the expanded snapshot corpus" and this issue's own acceptance
   criterion ("a naming-scheme reconciliation for `Bind` targets that
   doesn't touch production output"). Named only for completeness, per
   this ADR's practice of recording rejected alternatives rather than
   omitting them.

**BT-3144 dual-run test changes needed regardless of option chosen.** The
hard-coded `"Sum1"`/`"Count1"` literals (`threaded_ir.rs:3142-3159`) need
to become generator-seeded, mirroring the pattern
`dual_run_hybrid_letrec_state_prefix_matches_live_generator`
(`:3218-3237`) already uses for the `State`-prefix case — call the real
accessor (`legacy_gen.fresh_temp_var(..)`, seeded identically to the
`render()` side's generator) instead of hard-coding the string, so the
tests assert `render()` against production's actual naming scheme instead
of a fictional one. This closes a real, if quiet, correctness gap in
BT-3144's own acceptance evidence, independent of which Gap 2 option is
implemented.

### Descope alternative, considered and rejected (scoped to BT-3145 only)

Per this ADR's Alternative 1b and the prior addendum's "Re-scope
decision," the pre-planned fallback whenever a migration phase's
design/implementation cost outweighs its payoff is: keep `ThreadedIr`
verification-only for the construct in question, and stop — not a
failure, an explicitly pre-planned outcome (§Measurement gate, restated,
"Descope path, named").

Both gaps here turn out, on inspection, to be missing *renderer
expressiveness* BT-3144 didn't build yet, not a fundamental mismatch
between `ThreadedIr`'s shape and what while/counted loops need:

- **Gap 1's fix reuses two patterns that already exist in this codebase.**
  The condition/case-split shape is already factored out once in
  production (`CountedLoopFrame`, `control_flow/mod.rs:1112-1138`), and
  the "embed an opaque, AST-directed `Document` inside a `ThreadedStmt`
  node" technique is already established by `NlrCatch` — the issue body's
  own framing ("a similar shape may fit") is confirmed, not merely
  hoped for, by this section's design.
- **Gap 2's recommended fix reuses production's own allocator at
  production's own call order** — the lowering pass calls the same
  `fresh_temp_var` the legacy code calls today, at the same points in the
  same order, storing the result in the IR via `VersionPrefix::Gensym`;
  byte-identity is by-construction, not maintained by a cross-phase
  constraint (see Gap 2 option 1's rejected composition analysis).

Neither option touches `ThreadedStmt`'s existing verified shapes (`Bind`,
`NlrCatch`, `TupleAccUnpack`, `Return`), extends `verify()`'s existing
checks in a way that risks regressing BT-3132's/BT-3133's/BT-3134's
already-shipped production call sites, or re-opens any of BT-3128's seven
already-shipped phases. Both are estimated **S**/**S–M** effort (Gap 1:
one new enum variant + its render/verify arms + a shared-helper refactor;
Gap 2: one `VersionPrefix` variant + its `render_name`/`resolve_prefix`
arms + lowering-time mint calls mirroring legacy order) against
BT-3145's own **M** size estimate — neither gap dominates the issue's
existing budget, and BT-3145's investigation already spent the cost of
finding them; a re-attempt starts from a concrete design, not from zero.

**Recommendation: do not descope BT-3145.** Design and implement both
fixes as part of a direct re-attempt at BT-3145, scoped exactly to the
while/counted-loop family (Phase B). This recommendation is deliberately
narrow — it says nothing about Phase C (`list_ops/`) or Phase D
(actor/class-method threading + NLR + shadow-write contract), each of
which this ADR's own §Implementation already flags as introducing
invariant classes the prior phase never exercised ("Phase C... sized L,
not a mechanical follow-on to Phase B... introduces invariant classes
Phase B never exercises"). Each later phase needs its own gap analysis
before a similar "continue" recommendation could be made honestly; this
section's descope evaluation is not a blanket endorsement of continuing
all of BT-3141, only of finishing the loop family specifically, on the
concrete evidence above. If a real re-attempt at implementing these two
designs surfaces a third, materially larger gap, that is the point to
re-open the descope question for the loop family — not now, while both
known gaps are S-sized, directly buildable from precedent already in the
codebase, and smaller than the phase's own size estimate.

### What a BT-3145 re-attempt should do, concretely

1. Add `ThreadedStmt::ConditionalLoop` (Gap 1, option 1) and its
   `verify`/`render` arms; refactor `render_loop_letrec`'s
   `param_list`/`final_args`/`outer_args` closures into a shared helper
   used by both the existing bare-loop `Threaded` shape and the new
   variant.
   Add `LoopCounter` (a thin, unversioned, single-alloc identity for the
   counted-loop index, mirroring `AccParam`'s existing precedent).
2. Add `VersionPrefix::Gensym(String)` and its `render_name`/
   `resolve_prefix` arms (Gap 2, option 2). The lowering pass mints via
   `fresh_temp_var` **in legacy order — `CondFun` + condition first, then
   each body rebind as its `Bind` is lowered, and `exit_arm`'s `ExitSA`
   chain LAST, i.e. lower the body IR before constructing `exit_arm`**
   (the module-wide counter makes byte-identity order-sensitive; see Gap
   2 option 1's rejected composition analysis). Fix
   `ConditionalLoop::fn_name` to be caller-supplied, never minted by
   `render`, closing the related loop-fn-name finding.
3. Rewrite the three existing `dual_run_*` tests to hand-author the real
   condition/case shape (not the condition-free skeleton) and to mint
   rebind-temp names via a real `fresh_temp_var` call on a same-seeded
   generator — carried in the IR as `VersionPrefix::Gensym` per Gap 2's
   recommendation, mirroring the existing `State`-prefix test's
   real-accessor pattern — rather than hard-coded literals.
4. Only then wire one real call site (`generate_while_loop_direct` is the
   smallest — no hybrid pre-extraction, no counted-loop counter) to
   lower, verify, and render through `ThreadedIr`, gated behind the
   existing measurement flag, and re-run the ≤3% gate (§Measurement gate,
   restated) against real construct-and-render cost for the first time.

## Addendum 3 (2026-08-11): BT-3145 re-attempt — what actually shipped, a third discovered gap, and the measurement outcome

BT-3145 implemented Addendum 2's design in full: `ThreadedStmt::ConditionalLoop`
(Gap 1), `VersionPrefix::Gensym` (Gap 2), `render_loop_letrec` refactored
into a shared `render_loop_skeleton` behind both the bare `Threaded` shape
and `ConditionalLoop`, and the three `dual_run_*` tests rewritten to
hand-author the real condition/case-split shape and mint via a real,
identically-seeded `fresh_temp_var` generator — all exactly as specified,
with zero deviation from the reviewed design's field shapes.

**A real implementation attempt at wiring `generate_while_loop_direct`
surfaced two small, closely-related mechanical gaps Addendum 2's design
didn't anticipate** (verified against real compiled output, same standard
as the original investigation and Addendum 2 itself):

- **`ValueRef` had no way to carry an arbitrary computed RHS.** A real
  rebind's value is not a bare version reference — `sum := sum + 1`'s RHS is
  `call 'erlang':'+'(Sum, 1)`, ordinary AST-directed expression codegen.
  Closed by adding `ValueRef::Doc(Document<'static>)`, the same class of
  opaque embed `continue_header`/`exit_arm` already use, with the identical
  §Verifier-honesty justification.
- **`BodyKind::Letrec` inserts a literal `" "` between body statements**
  (`generate_threaded_loop_body_inner`, confirmed by compiling a two-rebind
  loop and finding `"... in  let ..."` — a genuine double space) that
  neither `render()` nor the pre-Addendum-2 dual-run fixtures reproduced.
  Closed via `render_loop_body_statements`, scoped to `ConditionalLoop`
  bodies only (the bare `Threaded` shape's body was never a real production
  shape, so it correctly never needed this).

Both were small (one new `ValueRef` variant, one small rendering helper) and
are implemented, tested (including dedicated dual-run byte-parity tests),
and landed — not deferred.

**A third, larger, genuinely-deferred gap: full loop-body coverage.**
Modeling *every* shape `select_direct_params` allows through a direct-params
loop body — plain-let temporaries for non-threaded locals
(`try_generate_block_local_plain_let`), destructuring, and the
`direct_params_list_op_result` open-chain shape a nested tuple-safe list op
produces — needs either a new "opaque non-`Bind` body statement"
`ThreadedStmt` variant or a materially different body model. This is exactly
the "third, materially larger gap" this ADR's own Addendum 2 named as the
point to re-open the descope question, rather than force it through under
this issue's already-spent budget. BT-3145 does not attempt it: the pilot's
`while_direct_body_is_bind_representable` conservatively routes only
straight-line, `Bind`-representable bodies (every statement a reassignment
of a threaded local, no plain-let temporaries, no nested control flow, no
list-op RHS) through `ConditionalLoop`; anything else falls back to the
unmodified legacy path, unconditionally correct. See the BT-3145 Linear
issue for the follow-up recommendation.

**Measurement gate: inconclusive in this environment, gate not cleared.**
`beamtalk build-stdlib` (the real stdlib corpus, `just build-stdlib`),
4 runs each with the flag off vs on, cold `ebin/` each time:

| | wall-clock (s) | user CPU (s) |
|---|---|---|
| flag off (mean of 4) | 12.44 | 14.79 |
| flag on (mean of 4) | 15.30 (18.57 outlier included) / 14.21 (excluded) | 15.18 |
| Δ | +14–23% wall-clock | +2.6% user CPU |

Wall-clock and CPU-time deltas disagree by an order of magnitude, and the
wall-clock samples include one clear outlier (18.57s vs. 13–15s for the
other three) — this machine's shared/virtualized environment does not give
a clean enough signal to confidently say the gate passes OR fails at a
precise number. What is clear: overhead is not obviously near zero, and the
covered subset (straight-line bodies) is a small fraction of the full
stdlib's while-loop population, so the ≤3% gate is **not affirmatively
cleared** — this is the ADR's own pre-planned "over threshold → stop, do not
flip the default" outcome (§Measurement gate), reached here honestly rather
than rounded away.

**Outcome:** the default stays the legacy path. `BEAMTALK_THREADED_IR_WHILE_DIRECT=1`
exists as an opt-in, fully-tested (byte-identical on every covered shape)
path for future measurement once full body coverage closes gap three, but
is not flipped, and the legacy code is not deleted — both explicitly
conditional on the ≤3% gate per BT-3145's own acceptance criteria, and
neither condition holds today. This is not a failure to fix at all costs;
it is the pre-planned, acceptable descope path this ADR names, reached only
after landing real, tested, reviewed infrastructure (the two Addendum-2
gaps, both now closed) rather than stopping at the investigation stage a
second time.

## Addendum 4 (2026-08-12): General opaque AST-directed statement node (BT-3156) — unblocking BT-3148's routing/NLR/absorption tasks, a real mint-order hazard found before landing, and why BT-3146 needs something else

All line numbers below are against
`crates/beamtalk-core/src/codegen/core_erlang/` unless noted, current as of
`main` at the commit landing BT-3148's task 3
(`Migrate class-var Bind producers to emission-input ThreadedIr (BT-3148)
(#3350)`). As Addendum 2 notes for its own citations: resolve by symbol
name first, treat `:NNN` as a hint — this file is under active migration
and line numbers drift.

[BT-3148](https://linear.app/beamtalk/issue/BT-3148) ("gen_server state
threading, NLR boundaries, class-var Bind/shadow-write producers") landed
its task 3 of 4 — the two class-var `Bind` producer sites
(`expressions.rs::generate_class_var_field_assignment`,
`dispatch_codegen.rs::emit_class_var_result_unwrap`) now construct, verify,
and `render` a real `ThreadedStmt::Bind`, deleting the
`verify_class_var_bind` fixture-mirror. Tasks 1 (routing unification), 2
(a production `NlrCatch` constructor for `wrap_body_with_nlr_catch`), and 4
(`threaded_expr.rs`'s `ThreadingBoundary` absorption) were investigated and
explicitly not attempted — the BT-3148 PR's own closing comment names the
reason: `gen_server/methods.rs`'s `classify_body_expr` has ~18
`BodyExprKind` variants, and most of them (message sends, dispatch, Tier 2
calls, `EarlyReturn`, …) are ordinary AST-directed statements with **no
`ThreadedStmt` representation at all** — not a narrower version of the
class-var `Bind` shape task 3 closed, a different, missing kind of node
entirely. That gap was split out as this issue.

Separately, [BT-3146](https://linear.app/beamtalk/issue/BT-3146)
("conditionals + exception_handling") investigated its own migration and
reached a **different conclusion about the same-sounding problem**: its
2026-08-12T08:11 investigation comment compiled `self.x := flag ifTrue:
[self.y := 1. 42] ifFalse: [0]` and confirmed that
`ValueRef::Doc`/`VersionPrefix::Gensym` (BT-3145's own additions) are
**already** expressive enough to model every one of its ~10 mutation
shapes as a two-hop `Bind` chain — quoting the comment directly: *"no
single shape is architecturally impossible with today's
`ThreadedStmt`/`BindOp`/`VersionPrefix::Gensym` vocabulary. The gap is
volume and risk, not a missing IR node."* BT-3146 was set to `needs-spec`
on that basis, not because a node was missing.

This addendum's central finding, stated up front because it governs
everything below: **BT-3148's remaining tasks and BT-3146 are not the same
gap wearing two names.** BT-3148 tasks 1/2/4 need a genuinely new IR node —
there is no existing vocabulary for "an ordinary AST-directed statement,
sitting in a straight-line body next to real `Bind`s, that this pass
doesn't need to understand." BT-3146's blocker is applying an
**already-sufficient** vocabulary correctly, by hand, across 10+
structurally distinct shapes, each needing its own empirical
byte-identity confirmation — a volume-and-discipline problem a new node
does not shrink. Building a general node and then reaching for it to
paper over BT-3146's per-shape work would be the wrong fix for BT-3146
specifically (§"Why BT-3146 needs something else, not this node", below) —
this addendum designs the node BT-3148 needs, evaluates it honestly
against BT-3146's actual blocker, and — per this ADR's own repeated
practice — recommends BT-3146 proceed differently rather than forcing one
solution onto both.

### Evidence: two repro shapes, compiled and read (this session's standard)

**Shape 1 — ADR 0110's own `CollectionDriver countedRun:over:`
(BT-3148's cited repro), no local `^`:**

```beamtalk
Value subclass: CollectionDriver
  classState: runs = 0

  class countedRun: aBlock :: Block over: aList :: List -> Nil =>
    self.runs := self.runs + 1
    aList do: [:x | aBlock value: x]
    nil
```

compiles (`beamtalk build`, read from `bt@collection_driver.core`) to:

```erlang
'class_countedRun:over:'/4 = fun (ClassSelf, ClassVars, _aBlock1, _aList2) ->
    let _Val3 = ( call 'erlang':'+'(call 'maps':'get'('runs', ClassVars), 1) ) in
    let ClassVars1 = call 'maps':'put'('runs', _Val3, ClassVars) in
    let _ = call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1) in
    let _seq4 = ( <aList do: [...] — ordinary AST-directed dispatch codegen> ) in
    let _Ret13 = 'nil' in
    {'class_var_result', _Ret13, ClassVars1}
```

(span/`-|` annotations elided; the `do:` block's own dispatch codegen is
reproduced verbatim in the actual `.core` file and is irrelevant to this
addendum — it is exactly the kind of ordinary AST-directed statement in
question.) **No NLR try/catch at all** — `needs_nlr` is `false` because
`countedRun:over:`'s own body contains no literal `^` in its own directly
nested blocks (the `^` that ADR 0110 is about lives in the *caller's*
block, a different method, invisible to this method's own classifier).
The shadow write is unconditional on `block_depth == 0`, not on local
NLR-catch presence — confirmed empirically here.

**Shape 2 — the same class, with a literal `^` added so `needs_nlr` is
`true`, to exercise the boundary this repro lacks:**

```beamtalk
Value subclass: NlrClassVar
  classState: runs = 0

  class bump: x :: Integer -> Integer =>
    self.runs := self.runs + 1
    x > 5 ifTrue: [^x]
    0
```

compiles to:

```erlang
'class_bump:'/3 = fun (ClassSelf, ClassVars, _x1) ->
    let _NlrToken2 = call 'erlang':'make_ref'() in
    try
        let _Val3 = ( call 'erlang':'+'(call 'maps':'get'('runs', ClassVars), 1) ) in
        let ClassVars1 = call 'maps':'put'('runs', _Val3, ClassVars) in
        let _ = call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1) in
        let _seq4 = ( call 'beamtalk_message_dispatch':'send'(( call 'erlang':'>'(_x1, 5) ), 'ifTrue:',
                        [fun () -> call 'erlang':'throw'({'$bt_nlr', _NlrToken2, _x1, ClassVars1})]) ) in
        let _Ret5 = 0 in
        {'class_var_result', _Ret5, ClassVars1}
    of _NlrResult6 -> _NlrResult6
    catch <_NlrCls7, _NlrErr8, _NlrStk9> ->
        case {_NlrCls7, _NlrErr8} of
            <{'throw', {'$bt_nlr', _CatchTok10, _NlrVal11, _NlrState12}}> when _CatchTok10 =:= _NlrToken2 ->
                {'class_var_result', _NlrVal11, _NlrState12}
            <_OtherPair13> when 'true' ->
                primop 'raw_raise'(_NlrCls7, _NlrErr8, _NlrStk9)
        end
```

**`_NlrToken2` is minted before `_Val3`.** Production
(`gen_server/methods.rs:305-314`, both the Actor and class-method call
sites) mints `NlrToken` first, unconditionally, then calls
`generate_method_definition_body_with_reply` — every temp the body itself
mints (`_Val3`, `_seq4`, `_Ret5`, …) necessarily gets a **later** module-wide
counter value. This is the load-bearing fact behind Gap 3, below.

### The node: `ThreadedStmt::Statement`

```rust
enum ThreadedStmt {
    // ...Bind, Threaded, NlrCatch, Return, TupleAccUnpack, ConditionalLoop...

    /// An ordinary AST-directed statement, embedded verbatim as one
    /// opaque entry in a straight-line ThreadedStmt sequence — the
    /// statement-level counterpart of ValueRef::Doc's value-level
    /// opacity (ADR 0111 Addendum 3), built by the SAME codegen call
    /// production already runs at this point (`self.expression_doc(expr)`,
    /// `self.generate_self_dispatch_open(expr)`, the `{'reply', ...}`/
    /// `{'class_var_result', ...}` epilogue builders, …). Legal in any
    /// straight-line sequence rendered by `render`'s top-level loop (a
    /// gen_server method body, an `NlrCatch` try-body); see the
    /// separator note below before placing one inside a
    /// `ConditionalLoop` body. Carries no state-threading
    /// content of its own by construction (see the type-level rule
    /// below) — a statement that DOES mutate a threaded
    /// version must be a Bind, never a Statement; there is no `BindOp`
    /// escape hatch here the way `ValueRef::Doc` is one inside a `Bind`'s
    /// own `op`.
    Statement(Document<'static>, Span),
}
```

`render`'s dispatch loop needs exactly one new arm:

```rust
ThreadedStmt::Statement(doc, _) => docs.push(doc.clone()),
```

No other change to `render`'s structure. This is the smallest possible
extension because `render`'s existing loop already has the property that
matters: it concatenates each statement's `Document` with **no separator**
(`Document::Vec(docs)`, confirmed by reading the current implementation),
relying on each statement's own trailing glue (`Bind`'s render always ends
`" in "`; a terminal statement carries none). A `Statement`'s `Document`
must therefore carry its own correct trailing glue, exactly the discipline
`BodyExprKind::Pure`'s non-last-position arm already follows today (`let
seqN = <expr_doc> in `, `gen_server/methods.rs:1533-1536`) — lowering
constructs it, `render` does not need to know it exists.

**Separator note (loop bodies differ):** the no-separator property above
is specific to `render`'s top-level loop. A `ConditionalLoop` body renders
through `render_loop_body_statements`, which inserts a literal `" "`
between statements — Addendum 3's own empirically-confirmed double-space
finding for `BodyKind::Letrec` bodies. A `Statement` placed inside a loop
body therefore composes as glue-plus-separator (`"... in "` + `" "` →
`"... in  "`), which is exactly what production's Letrec bodies emit — the
composition is coherent, but no production shape mixes `Statement` into a
loop body yet (BT-3145's `while_direct_body_is_bind_representable`
deliberately excludes non-`Bind` statements), so it is stated here rather
than pinned by a dual-run test. The first implementation that routes a
mixed `Bind`/`Statement` loop body through `ConditionalLoop` must add that
dual-run byte-parity test before relying on this paragraph.

**Design options considered:**

1. **Fully opaque, `Statement(Document<'static>, Span)`, invisible to
   `verify()` — Recommended.** Mirrors the established precedent exactly:
   `NlrCatch` (no body field, `verify()`'s `walk_stmt` arm is `{}`),
   `ConditionalLoop::continue_header`, and `ValueRef::Doc` are all opaque,
   pre-rendered `Document`s that `verify()` does not look inside, for the
   same reason each time (§Verifier honesty: "the condition body is
   ordinary AST-directed expression codegen with no state-threading
   content of its own"). A `Statement` used correctly (see the
   type-level rule below) carries the same property. Smallest surface,
   smallest effort, and — crucially — a **strict improvement over
   today with zero new risk**: today these ~15 `BodyExprKind` shapes have
   *zero* IR representation and are not part of any single-sourced
   emission path at all; wrapping them in `Statement` gives them a real
   place in the real `Vec<ThreadedStmt>` `render()` actually emits from,
   without asking `verify()` to promise anything about them it cannot
   honestly check.
2. **`Statement { doc: Document<'static>, reads: Vec<VersionedVar>, span:
   Span }` — declared reads only, no declared writes.** Would let
   `check_use` catch `UnboundVersion` through the opacity boundary (e.g. a
   future lowering bug that emits a `Statement` referencing `State3`
   before any `Bind` produced it). **Considered, not recommended for this
   landing.** Two reasons: first, it asks every one of the ~15 call sites
   this closes to newly enumerate which `VersionedVar`s their embedded
   `Document` references — bookkeeping none of today's hand-rolled
   `Document` construction does, for a class of bug (`UnboundVersion`
   through an opaque statement) that is no *more* likely than the same
   class of bug already accepted, unverified, inside every `ValueRef::Doc`
   and `ConditionalLoop::continue_header`/`exit_arm` today. Second, and
   decisively, per the "verifier honesty" doctrine this ADR has followed
   since its own §Verifier honesty section: a check is worth adding when
   it can be sound and is worth the bookkeeping; here it would be *no more
   sound* than the existing accepted gaps (a `reads` list can go stale
   exactly like a `shadow_write: bool` can, and unlike `shadow_write`
   there is no downstream contract like ADR 0110's forcing it to be kept
   honest by a dedicated `VerifyError`). Not rejected outright — a real,
   buildable follow-up if a specific bug class ever motivates it (the
   field shape above is the natural extension point) — but adding it
   speculatively here would be exactly the "IR scope creep without
   verification payoff" this ADR has repeatedly declined (Gap 1 option 3;
   `exit_arm`'s own accepted limitation).
3. **Fully structural — model every AST shape `classify_body_expr` can
   produce as `ThreadedStmt` nodes.** **Rejected**, same grounds as Gap 1
   option 3 and the ADR's own §Constraints non-goal: this is "a
   full-pipeline IR covering all of Core Erlang codegen," absorbing ADR
   0018's rejected reasoning. `EarlyReturn`, `DispatchingSelfSend`,
   `SuperSend`, `ErrorSend`, `Tier2SelfSend`, ordinary `Pure` sends — none
   of these carry state-threading content whose *structure* `verify()`
   could usefully check; modeling their internals buys nothing.

**Type-level rule, made explicit because it is load-bearing:** a
`Statement` **must never** be the sole representation of a version
mutation. Any statement that produces a new `VersionedVar` — a class-var
write, a state write, a self-field write — is constructed as a `Bind`
(using the existing `Direct(ValueRef::Doc(...))`/`Gensym`-two-hop idioms
BT-3145/BT-3146's own investigation already established for opaque RHS
extraction), never folded into an opaque `Statement` merely because doing
so would be less code. This is the same "make the invalid state
unrepresentable" preference this ADR has applied since Alternative #5
(the `VersionedVar` typestate) — here enforced by convention and code
review rather than the type system (a `Statement`'s `Document` is
opaque by definition; nothing stops a future call site from building one
that happens to contain a `maps:put` no `Bind` ever sees), so it is
recorded here as an explicit rule a reviewer must enforce, not a
guarantee `verify()` can check. This is precisely the distinction BT-3146
already draws between `continue_header` (sound opacity, `ConditionalLoop`'s
own doc comment) and `exit_arm` (**hides genuine state-threading content**,
a named, deliberate limitation) — a `Statement` is only ever the
`continue_header` kind.

### Gap 3 — NLR-token mint order (found empirically, before landing)

The issue body's own instructions asked specifically for this class of
check, and it catches something real: **`render`'s current `NlrCatch` arm
mints its token in the wrong position relative to a real body.**

```rust
ThreadedStmt::NlrCatch { boundary, .. } => {
    let body_doc = render(&ir[i + 1..], ctx);      // body rendered FIRST
    docs.push(render_nlr_catch(*boundary, body_doc, ctx));  // token minted AFTER
    return Document::Vec(docs);
}
```

`render_nlr_catch` mints `ctx.fresh_temp_var("NlrToken")` internally, at
render time, **after** the "rest of slice" body has already rendered.
Shape 2's compiled output above shows production's real order is the
opposite: `_NlrToken2` is minted **before** `_Val3`/`_seq4`/`_Ret5` — the
token consumes the module-wide counter slot the body's own temps would
otherwise get, shifting every subsequent number.

This is currently **dormant, not yet triggered**, for the same reason
§"What did not ship" already gives for `NlrCatch` generally: nothing
constructs one at a real call site yet. It is dormant in the *test suite*
too, for a subtler reason worth naming: `dual_run_nlr_catch_reuses_wrap_body_with_nlr_catch_verbatim`
(`threaded_ir.rs:4081`) is the one existing test that renders a
`Bind`-then-`NlrCatch`-adjacent IR — but its body is the **literal,
hand-written** `docvec!["let ", leaf::var("Sum1"), " = ", leaf::var("Sum"),
" in "]`, which contains **zero `fresh_temp_var` calls of its own**. Both
the "legacy" and "render(lower(..))" sides therefore produce `_NlrToken0`
regardless of whether the token is minted before or after the body — the
test cannot distinguish correct order from inverted order, because
nothing in its fixture consumes a counter slot the inversion could
misplace. Wiring a real `NlrCatch` node into BT-3148 task 2 — a body that
genuinely mints `_Val`/`_CfTuple`/`_seq`-style temps, exactly Shape 2 above
— would be the **first** thing to hit this, silently: the compiled output
would still be valid Core Erlang (every reference still resolves; nothing
crashes), just with every temp number shifted from `_Val3` to `_Val4`+ and
so on — a byte-identity break invisible to anything except the snapshot
corpus, exactly the class of bug the issue's own instructions warned this
session has already hit twice (BT-3144's initial commit,
BT-3145/Addendum-2's design).

**Fix, mirroring Gap 2 option 2's already-adopted pattern exactly:** mint
the NLR token **during lowering**, at the same relative position
production mints it (before lowering the body's own `Bind`/`Statement`
nodes), and carry the resulting literal name in the IR — the same
"lowering-time pre-allocation, IR carries the rendered name" idiom
`VersionPrefix::Gensym` already established for `Bind` targets.

```rust
/// Was TokenId(u32) — an opaque numeric identity never rendered directly
/// (render_nlr_catch minted its own name independently). Now carries the
/// literal Core Erlang name minted at lowering time, in production's real
/// mint position (before the body), exactly the fix Gap 2 applied to
/// Bind-target naming for ConditionalLoop.
pub(super) struct TokenId(String);

impl TokenId {
    pub(super) fn new(name: String) -> Self { Self(name) }
    fn name(&self) -> &str { &self.0 }
}
```

```rust
ThreadedStmt::NlrCatch { boundary, token, .. } => {
    let body_doc = render(&ir[i + 1..], ctx);
    docs.push(render_nlr_catch(*boundary, token.name(), body_doc, ctx));
    return Document::Vec(docs);
}
```

`render_nlr_catch` drops its own `ctx.fresh_temp_var("NlrToken")` call and
takes the name as a parameter instead — rendering no longer mints
anything for this node, matching every other `Statement`/`Bind` render
path's "no allocation at render time" property. The lowering pass that
BT-3148 task 2 writes must mint the token **before** it lowers the body's
`Bind`/`Statement` sequence (an ordinary code-ordering fact of one
function, exactly how Gap 2's option 2 closed the equivalent hazard for
loop-body rebind temps — "the lowering pass must still preserve that
order... enforced naturally by writing the lowering to mirror the legacy
function it replaces"). This also requires updating every existing
`TokenId::new(0)` call site to pass a name instead of a numeric
placeholder — five at the time of writing: four unit tests
(`threaded_ir.rs:2771,2806,2864,4100`) plus one production site,
`construct_and_verify_class_var_bind`'s synthetic `NlrCatch` marker
(`threaded_ir.rs:2124`) — that marker is fixture-only (never rendered,
per its own doc comment: "not in the returned `Bind`, which callers
render alone"), so this is a mechanical compile-fix there, not a
byte-identity concern; called out here so it is not mistaken for scope
creep when a real implementation attempt touches those lines.

**Composition note:** this fix does not affect `Statement`'s own design —
`render`'s `Statement` arm still performs zero minting either way, and the
hazard is specific to `NlrCatch`'s pre-existing (already-in-the-ADR, since
BT-3129) placeholder `TokenId`, not something this addendum's new node
introduces. It surfaces here because `Statement` is what makes wiring a
*real*, temp-minting body next to `NlrCatch` possible for the first time —
exactly the composition the issue asked this addendum to verify before
committing to a design.

### `Return`'s scope stays as-is; reply epilogues are `Statement`s, not `Return`s

`ThreadedStmt::Return(ValueRef, VersionedVar, Span)` renders a bare
`{Value, State}` 2-tuple (`render_return`). Both compiled shapes above
show this is **not** what a gen_server method body's final statement
emits: Shape 1/2 emit `{'class_var_result', Value, ClassVars}` (a
3-element **tagged** tuple); a bare-value class method with no mutation
(`class_runCount` — `let _Ret14 = call 'maps':'get'('runs', ClassVars) in
_Ret14`, confirmed in the same compile) emits **no tuple at all**; an
Actor method emits `{'reply', Value, State}`. `NlrBoundary`'s own
`nlr_arm_result` (`mod.rs:1086`) already encodes this three-way split for
the NLR-catch-arm case — the same three shapes recur for the **normal**
(non-NLR) return path, driven by `emit_dispatch_reply`/
`emit_tuple_unpack_reply`/`emit_pure_reply`/the inline `{'class_var_result',
...}` literal in `generate_body_exprs_with_reply`'s class-method-EarlyReturn
arm.

**Recommendation: leave `Return`'s contract unchanged; represent every
reply-wrapping epilogue as a final `Statement`, built by the same
`emit_*_reply` family already in production.** Two reasons this is
better than widening `Return`: first, `Return`'s existing 2-tuple shape
has a real, narrower use today (embedded inside `ControlFlowWithMutations`/
`Tier2ValueCall`'s own `{Result, State}` tuple production, a different
context from a method's outermost reply) — changing its meaning to
"whatever this boundary's reply shape is" would be a second, incompatible
contract wearing the same node name. Second, the reply-shape diversity
(bare value / `{'reply', ...}` / `{'class_var_result', ...}` / the
NLR-catch-arm's own three-way echo of the same split) is not
state-threading structure `verify()` could usefully check regardless of
which node carries it — it is exactly the kind of "ordinary AST-directed
tail construction, no threading content of its own" `Statement` exists
for. Concretely: `docs.push(ThreadedStmt::Statement(<the `emit_*_reply`
family's existing Document output, unchanged>, span))` as the body
sequence's last entry, instead of inventing a `ReplyKind` parameter on
`Return` no other `Return` call site needs.

### Byte-identity walkthrough (Shape 1, the no-NLR case)

```rust
let f0 = FrameId::ROOT;
let ir = vec![
    ThreadedStmt::Statement(docvec!["let ", leaf::var("_Val3"), " = ", rhs_doc, " in "], span),
    ThreadedStmt::Bind {
        target: VersionedVar::new(VersionPrefix::ClassVars, 1, f0),
        source: VersionedVar::new(VersionPrefix::ClassVars, 0, f0),
        op: BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val3".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        },
        shadow_write: true,
        span,
    },
    ThreadedStmt::Statement(docvec!["let ", leaf::var("_seq4"), " = ", do_doc, " in "], span),
    ThreadedStmt::Statement(
        docvec!["let ", leaf::var("_Ret13"), " = 'nil' in {'class_var_result', ",
                leaf::var("_Ret13"), ", ", leaf::var("ClassVars1"), "}"],
        span,
    ),
];
```

`render`'s loop concatenates, in order, with no separator: the first
`Statement`'s doc (`"let _Val3 = <rhs_doc> in "`) — unchanged from
`generate_class_var_field_assignment`'s existing `val_doc` construction —
then `render_bind`'s `BindOp::Put` output (unchanged, task 3's already-shipped
code path: `"let ClassVars1 = call 'maps':'put'('runs', _Val3, ClassVars) in
let _ = call 'erlang':'put'({'$bt_class_vars_shadow', ...}, ClassVars1) in
"`), then the second `Statement` (the `do:` dispatch, unchanged AST-directed
codegen), then the third `Statement` (the reply epilogue). Concatenated,
this is character-for-character the compiled output quoted above — every
sub-`Document` is either (a) already exactly what production builds today
via an unchanged codegen call, or (b) `render_bind`'s already-shipped,
already-tested `BindOp::Put` rendering. Nothing about wrapping these in
`ThreadedStmt` changes what text any of them produce; the change is that
they are now one `Vec<ThreadedStmt>` `render()` emits from, not four
separately-glued `Document` fragments assembled by hand outside any
verifier's reach.

### Why BT-3146 needs something else, not this node

`Statement` does not unblock BT-3146, and reaching for it there would be
the wrong fix. BT-3146's own investigation (quoted above) already
confirmed every one of its ~10 mutation shapes is representable with
**existing** vocabulary — a `Gensym`-named two-hop `Bind` chain
(`Bind{target: Gensym("_CfState8"), source: State(prior), op:
Direct(Doc(rhs))}` → `Bind{target: State(next), source:
Gensym("_CfState8"), op: Put{...}}`, confirmed against real compiled
`_CfTuple`/`_CfVal`/`_CfState` output) or a direct `Direct(ValueRef::Doc(...))`
rebind (`Tier2ValueCall`/`ControlFlowWithMutations`, which target
`next_state_var()` directly). **Wrapping one of these mutation-carrying
shapes in an opaque `Statement` instead would be a regression, not a
migration** — it would give `check_branch_frame_linearity`'s six call
sites (`conditionals.rs`/`exception_handling.rs`) a real IR node to point
at while making the actual mutation invisible to `verify()` again, the
opposite of BT-3146's stated goal ("each arm's *actual* mutation sequence
... give the verifier real teeth"). The `Statement`/`Bind` type-level rule
above says this explicitly: a statement that mutates a version must be a
`Bind`, and BT-3146's shapes all mutate a version.

What BT-3146 is actually blocked on — per its own comment — is **volume
and risk**: ~10+ structurally distinct shapes across two files, each
needing the `Gensym`-two-hop (or direct) pattern applied by hand and
individually confirmed against real compiled output, at a scope BT-3145's
own narrower precedent (one loop family, one measurement gate, still two
follow-up gap-closing rounds) suggests is not safely completable in one
pass. That is exactly what BT-3146's own recommended follow-up already
names: an Addendum-2-style design pass enumerating each `BodyExprKind`
shape's exact `Bind` sequence, one at a time, citing compiled output per
shape — a `/plan-adr`-sized effort in its own right, not a consequence of
any node being absent. This addendum does not attempt that enumeration
(out of scope per BT-3156's own acceptance criteria — implementation
re-attempts are separate issues, mirroring BT-3153 → BT-3145); it records
the finding so BT-3146's own follow-up does not restart from "is a node
missing" a second time.

`Statement` **does** help BT-3146 secondarily, once that per-shape work
happens: today, any genuinely non-mutating statement interleaved inside a
conditional/exception arm (a `Pure` send with no threaded result, a
`DispatchingSelfSend` with no field write) has no home in `ThreadedStmt`
either — the same ~15-variant gap BT-3148 has, recurring inside branch
bodies. Once BT-3146's mutation shapes are real `Bind` chains, the
non-mutating statements sharing their arm can use `Statement` rather than
needing their own bespoke per-shape handling. This is real but
incremental value, not the blocking piece.

### Descope alternative, considered and rejected

Per this ADR's Alternative 1b and the practice both prior addenda follow:
could BT-3148 tasks 1/2/4 simply stay undone, with the class-var slice
(task 3) as the final state? **Considered, rejected for this addendum's
scope** (design work only — BT-3156's own acceptance criteria requires
either an actionable path or a reasoned descope, not silence): tasks 1/2/4
are not merely "more of the same size of win" as task 3 — `RoutingMismatch`'s
two `debug_assert!`-replacement call sites stay permanently dual-computed
without them (the exact "two independently-computed decisions" shape this
ADR's own §Decision names as Option A's central argument over B2), and
`NlrCatch`/`ShadowWriteMissing` stay unable to see a real class-var
mutation jointly with the NLR boundary it is stated against — the
`ShadowWriteMissing` worked example (§Worked example) has still never run
over real body IR, only per-call-site fixtures, even after task 3. Unlike
BT-3146, where the descope-vs-continue question turns on real,
non-trivial per-shape verification cost, `Statement`'s own cost here is
small (one node, one `render` arm, the `Gap 3` `TokenId` fix) against a
real, previously-named gap in what task 3 alone delivers. **Recommendation:
do not descope BT-3148's remaining tasks;** implement `Statement` and the
`TokenId` fix as a direct BT-3148 re-attempt, mirroring how BT-3153 → BT-3145
worked.

### What a BT-3156 implementation attempt should do, concretely

1. Add `ThreadedStmt::Statement(Document<'static>, Span)` and its `render`
   arm (one line). No `verify()` change — `walk_stmt`,
   `collect_producer_consumer_counts`, and `contains_class_var_nlr_catch`
   each need a `Statement(..) => {}` / `false` arm, matching `Bind`'s/
   `Return`'s existing non-recursive treatment.
2. Fix Gap 3: change `TokenId` to carry a lowering-time-minted `String`
   (mirroring `VersionPrefix::Gensym`'s already-adopted pattern exactly);
   update `render`'s `NlrCatch` arm and `render_nlr_catch` to consume it
   instead of minting at render time; update the three existing
   `TokenId::new(0)` test call sites.
3. BT-3148 task 1 (routing): rewrite `classify_body_expr`'s Phase 2 loop
   (`generate_body_exprs_with_reply`) to build one real `Vec<ThreadedStmt>`
   per body instead of a `Vec<Document>` — mutating kinds (`FieldAssignment`,
   `SelfFieldAtPut`, `DestructureAssignment`, and their `ControlFlow`
   variants) become `Bind`s via task 3's already-shipped
   `construct_and_verify_class_var_bind`-style construction (generalized
   beyond class vars to the `State`/`SelfVt` prefixes the non-class-var
   sites need); non-mutating kinds become `Statement`s; call `verify()`
   **once** over the real per-body sequence, replacing every per-call-site
   fixture-and-discard `verify_*` wrapper this module currently has for
   this slice. `RoutingMismatch`'s two `debug_assert!`-replacement call
   sites (`gen_server/methods.rs:1263,1463`) become unrepresentable by
   construction (there is only one classification pass now, not two to
   compare) and are deleted, per this ADR's own repeated "single-sourcing
   removes the check by removing the second computation" argument.
4. BT-3148 task 2 (`NlrCatch`): `wrap_body_with_nlr_catch`'s real call
   sites (`gen_server/methods.rs:305-335` and the class-method equivalent)
   mint the token via the same `TokenId`-carrying lowering step (item 2,
   above) **before** lowering the body from item 3, then prepend a real
   `NlrCatch { boundary, token, frame, span }` to the body's
   `Vec<ThreadedStmt>` and render the whole sequence through `render`'s
   already-implemented "rest of slice is my body" convention — no change
   to that convention is needed, only to what now sits on both sides of
   it.
5. BT-3148 task 4 (`threaded_expr.rs` absorption): once every
   `BodyExprKind` variant always produces IR (items 3-4), `ThreadingBoundary`'s
   job — deciding whether a construct routes through the shared emitter or
   falls through to a generic path — collapses to "did lowering, not a
   second recheck, decide"; audit whether any of its logic survives as
   the lowering-time classification itself, or whether it is fully
   replaced.
6. Snapshot-corpus + `class_var_shadow_contract.rs` + `verify-threaded-ir`
   green, per this ADR's own §Implementation exit criteria for every
   migration phase; re-measure the cumulative ≤3% gate per §Measurement
   gate, restated (this migration touches every gen_server method body,
   unlike BT-3145's narrow while-direct-only slice, so the gate is worth
   re-checking even though task 3 alone judged itself exempt as "the same
   code paths, just via an intermediate node").
7. Only then does BT-3146 gain a place to route its own non-mutating
   interleaved statements (§"Why BT-3146 needs something else"'s closing
   paragraph) — BT-3146 itself still needs its own separate
   Addendum-2-style per-shape design pass, out of this issue's scope.

## Addendum 5 (2026-08-12): BT-3148 re-attempt — tasks 1/2/4 landed, measurement result

This concretely-scoped re-attempt landed steps 1-6 of Addendum 4's checklist:
`ThreadedStmt::Statement` + the `TokenId` mint-order fix (already shipped,
commit `699bae82`), then task 1 (routing unification —
`gen_server/methods.rs::lower_body_exprs_with_reply` builds one real
`Vec<ThreadedStmt>` per Actor method body; `RoutingMismatch` and both
`verify_routing_invariant` call sites deleted), task 2 (the two named
`wrap_body_with_nlr_catch` call sites — `generate_method_dispatch`'s Actor
path and `generate_class_method_functions`'s class-method path — now prepend
a real `NlrCatch` stmt, token minted before lowering), and task 4 (audited:
`ThreadingBoundary` survives, narrowed to `threading_result_tail`'s pure
reply-shape-adapter duty — see `threaded_expr.rs`'s own module-doc addendum
for the full audit). Step 7 (full `just ci` exit criteria) passed: the
5666-test Rust suite, 478-case snapshot corpus, 233 stdlib tests, 3216 BUnit
tests, 7460 Erlang runtime unit tests, and 520 metamorphic assertions all
pass byte-identically against both the pre-existing snapshot corpus and the
`class_var_shadow_contract` conformance fixture.

**Not attempted** (scope boundary, not a gap in what was promised): the
class-method body pipeline (`generate_class_method_body`) is a separate,
hand-written `Document` builder pre-dating `BodyExprKind`/`classify_body_expr`
entirely, so its NLR wrap carries its output as one opaque `Statement` rather
than a fully-classified `Vec<ThreadedStmt>` — converting it is a peer
migration of comparable size, out of this issue's scope. Consequence,
restating this addendum's own "Descope alternative" concern: `VerifyError::
ShadowWriteMissing` still cannot see a real class-var `Bind` jointly with this
now-real class-method `NlrCatch` — the mint-order hazard (Gap 3) is fixed in
production for both boundaries, but the ADR 0110 joint-visibility gap the
descope note named stays open until that pipeline gets its own migration.
The 5 other `wrap_body_with_nlr_catch` call sites beyond the two this
addendum names (`gen_server/dispatch.rs`, `gen_server/extensions.rs` ×2,
`value_type_codegen.rs`, `actor_codegen.rs`,
`generate_class_method_fun_from_block`) are untouched.

**Measurement gate.** Unlike BT-3145's flag-gated pilot, task 1 replaces the
Actor body-generation path outright (no on/off flag — `RoutingMismatch` is
unrepresentable by construction, so there is no second path left to gate),
so "matched conditions" here means two separate release binaries (baseline
commit `699bae82` vs. this re-attempt's `HEAD`), `beamtalk build-stdlib`
(real stdlib corpus), cold `ebin/` each run, 8 runs per side (double
BT-3145's n=4, given this migration's larger surface):

| | wall-clock (s) | user CPU (s) |
|---|---|---|
| baseline (mean of 8) | 7.80 (range 6.50–9.46) | 10.76 |
| this re-attempt (mean of 8) | 6.96 (range 6.39–7.42) | 10.77 |
| Δ | −10.8% | **+0.10%** |

Same disagreement-by-an-order-of-magnitude pattern Addendum 3 found, for the
same reason: this machine's shared/virtualized environment makes wall-clock
noisy (baseline's own 8 runs span 6.50–9.46s, a 45% spread within one
unchanged binary — clearly scheduling jitter, not signal), while user CPU
time is comparatively stable and is the number Addendum 3 itself named as
the more trustworthy read of actual compute cost. Read on user CPU: **+0.10%,
inside the ≤3% gate with wide margin** — consistent with what the change
architecturally is (the same codegen calls, now populating a `Vec<ThreadedStmt>`
instead of a `Vec<Document>` directly, plus one linear `verify()` pass per
body). **Gate cleared; the new path ships as the unconditional default**
(there being no flag to leave off).

## Context

### Problem statement

The 2026-08 compiler pipeline architecture review (reflected in this issue and
its siblings [BT-3111](https://linear.app/beamtalk/issue/BT-3111) and
[BT-3112](https://linear.app/beamtalk/issue/BT-3112); there is no standalone
document for the review itself — it exists only as the shared rationale cited
by these three issues) identified the AST→Core-Erlang-text gap as Beamtalk's
main divergence from state-of-the-art compiler shape. All of the semantically
hard lowering — actor/instance state threading (`State`/`State1`/`State2`
versioning), class-variable threading (`ClassVars`/`ClassVars1`/…),
value-type field threading (`Self`/`Self1`/…), non-local-return (NLR)
lowering, and the `{Value, StateAcc}` calling convention (ADR 0041) —
happens **interleaved with `Document` construction** (the ADR 0089
pretty-printer tree), coordinated through a handful of scattered
`debug_assert!`s and a set of independently-implemented decisions that must
agree with each other by convention, not by construction.

[ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) is a real,
shipped bug in the adjacent class: a class method that mutated a class
variable and then handed a block to another method lost the mutation when the
block's `^` escaped through library code, while still returning the *correct
value*. The generated Core Erlang was well-formed the entire time — every
variable bound, every arity correct — so `core_lint` (which runs
unconditionally on every compile; see below) passed it without complaint.

**What this ADR honestly claims about that bug — and what it does not.**
A verifier catches violations of invariants someone has already stated; it
does not discover unknown-unknowns. ADR 0110's bug was an unknown-unknown:
before it was found, no lowering pass would have encoded the requirement
"every class-var mutation must be recoverable at every relay exit," because
that requirement was exactly the missing knowledge. The mechanism that hunts
that kind of bug is BT-3112's planned metamorphic/differential harness
(semantics-preserving transforms must preserve results — ADR 0110's
signature, "the returned value is correct; only the mutation is lost," is
the canonical metamorphic-oracle target). What this ADR's verifier does for
the 0110 class is narrower and still valuable: **now that the invariant is
known**, the IR models the shadow-write emission explicitly, and the
verifier enforces it at every existing and future mutation site — pinning
ADR 0110's fix against regression and extending it mechanically to code
paths its authors never saw (a new control-flow construct, a new mutation
emission site) instead of relying on each future author to remember the
rule. The two mechanisms are complementary and this ADR sequences itself
accordingly (see §Coordination).

### Current state, as measured

BT-3122 estimated the in-scope surface as "the `control_flow/` cluster
(4.4K LOC), `threaded_expr.rs`, and the state-versioning parts of
`gen_server/methods.rs` (5.8K LOC)," guarded by "~16 scattered
`debug_assert!`s" inside a "~90-field `CoreErlangGenerator`." Direct
measurement during this ADR's research corrects all three figures — the
underlying problem is the same, but the ADR should proceed from accurate
numbers:

| Claimed | Measured | Detail |
|---|---|---|
| `control_flow/` cluster: 4.4K LOC | **16,329 LOC total** (≈11,264 excluding inline/separate test modules) across 13 files | 4.4K matches `control_flow/mod.rs` alone (4454 lines); the cluster also includes `conditionals.rs` (1127), `counted_loops.rs` (617), `dict_ops.rs` (641), `exception_handling.rs` (1272), `while_loops.rs` (1248), and `list_ops/` (6 files, ≈4310 lines led by `transform_ops.rs` at 2074 and `search_ops.rs` at 917) |
| `threaded_expr.rs`: implied to carry significant complexity | **416 LOC** | Confirmed small — it is a boundary *adapter* (`ThreadingBoundary::{ValueType, ClassMethod, Actor}`, `ThreadedExpr`), not where the bulk of state-threading logic lives. That logic (the `ThreadingPlan`/`StateAccFallbackReason` mode selection) is in `control_flow/mod.rs` |
| "~16 scattered `debug_assert!`s" | **6 in the named in-scope files** (29 repo-wide in `codegen/core_erlang/`, but 23 are arity/argument-count checks in `intrinsics.rs`/`erlang_types.rs` — expression/primitive codegen, explicitly out of scope per BT-3122's own "that's not where the failures are") | Full 6: `while_loops.rs:317`, `while_loops.rs:454`, `control_flow/mod.rs:2522`, `control_flow/mod.rs:2651` (all four assert an *optimized threading mode implies no `StateAcc` unpack code* — the same invariant, independently asserted at four call sites instead of centralized once); `gen_server/methods.rs:1258`, `gen_server/methods.rs:1450` (both assert that `classify_body_expr`'s upfront classification and `threaded_expr.rs`'s downstream recognizer *agree* on whether a construct routes through the Actor threaded emitter) |
| "`CoreErlangGenerator` ~90 fields" | **36 fields** on `CoreErlangGenerator` (`mod.rs:1288-1481`), ≈39 including its two state-carrying sub-structs (`VariableContext`, `StateThreading`) | Still large enough to be the "coordinates everything" bottleneck the issue describes — just not 90 |

Two honesty notes on the `debug_assert!` situation:

- **The six assertions are not the last line of defense.** In release
  builds, a violation of any of the six degrades into malformed Core
  Erlang — an unbound `StateAcc` reference (the four unpack asserts) or an
  assignment target/reply that was never emitted (the two routing asserts)
  — which `core_lint` catches unconditionally at compile time. The honest
  statement is *"these six invariants have a structural backstop that
  produces a bad, Erlang-level error message far from the real cause, not
  no backstop"* — the verifier's contribution for these six is diagnosis
  quality and centralization, not net-new detection.
- **Every one of the six is a "two independently-computed decisions must
  agree" check** (threading-mode vs. unpack logic; classifier vs.
  emitter). That is the shape of invariant an IR removes by construction
  — one IR node has one emitter — rather than merely checks.

### The duplication this ADR's IR would also close

Research for this ADR surfaced a second, previously undocumented instance of
the CLAUDE.md/`architecture-principles.md` §6 duplication anti-pattern
("layer X can't depend on layer Y" leading to independent reimplementation
instead of a shared leaf module — see
[`docs/development/architecture-principles.md`](../development/architecture-principles.md)
§6): **three structurally identical, independently implemented
monotonic-version-counter services**, all producing `Prefix`, `Prefix1`,
`Prefix2`, … names via the same `util::versioned_var(prefix, version)`
helper, but tracked as three separate fields with three separate — and
deliberately *different* — save/restore disciplines:

| Counter | Type | Where | Produces | Branch discipline (`with_branch_context`, `mod.rs:2064-2080`) |
|---|---|---|---|---|
| Actor/instance state | `StateThreading` struct (`state_codegen.rs:42`) | `CoreErlangGenerator.state_threading` field | `State`, `State1`, … — **rendered as `StateAcc{N}` inside non-hybrid loop bodies** (`mod.rs:1994-2028`) | reset to 0 on branch entry, restored on exit |
| Class variables | `usize` field + `next_class_var()` (`mod.rs:1132`, `mod.rs:2088`) | `ClassContext.class_var_version` | `ClassVars`, `ClassVars1`, … (ADR 0110's mechanism) | restored but **not** reset (BT-1550); `class_var_mutated` deliberately sticky, never restored |
| Value-type fields | `usize` field + `next_self_var()` (`mod.rs:1211`, `mod.rs:2104`) | `ValueTypeContext.self_version` | `Self`, `Self1`, … | **neither saved nor restored** — the live landmine |

These asymmetries are load-bearing (BT-1449/BT-1550), which means
**unifying the representation is not the same as unifying the discipline**:
a naive "one counter type for all three" would change generated variable
names and break byte-parity. The `VersionedVar` design below therefore
unifies *naming and identity only*; per-prefix scope discipline remains an
explicit, per-prefix policy, preserved exactly and pinned by tests (see
Phase A2). Note also that the `State` counter's prefix is a *render-time*
function of generator context (`in_loop_body`/`in_hybrid_loop` select
`StateAcc*` vs `State*` naming for the same counter), and `"StateAcc"`
additionally serves as an unversioned lambda *parameter* name in foldl
list-ops (`basic_ops.rs:84`, `dict_ops.rs:90`, …) — two distinct roles one
string currently covers, which the IR must model as distinct nodes.
(A fourth, unrelated "State" name, `_BuilderStateN` in
`gen_server/methods.rs:2514`'s `build_builder_state_doc`, is a per-class
positional index with no relation to mutation-version threading at all.)

### What already exists and does *not* need to change

- **`core_lint`** (BT-3115) already runs unconditionally as part of every
  `compile:forms(Forms, [from_core | Opts])` call — independent of the
  `clint`/`no_lint` options, which only gate a different code path
  (compiling from Erlang *source*, which Beamtalk never does). It reliably
  catches unbound-variable and duplicate-variable well-formedness bugs
  (`docs/development/debugging.md` §"core_lint (BT-3115)"). It cannot
  catch semantic bugs like ADR 0110's — the generated code was well-formed.
  This ADR's verifier operates one layer earlier, on Beamtalk's own
  lowering decisions, and is a complement to `core_lint`, not a
  replacement.
- **`core_erlang_validity_tests.rs`** (part of BT-3112's epic) is a
  proptest suite over a fixed 20-element `FRAGMENTS` array checking three
  purely textual properties on rendered Core Erlang: balanced delimiters,
  module-name match, and absence of `Debug`/`Display`-format artifacts
  (the BT-875 class ADR 0089 closed). None of the three inspect
  state-threading correctness, NLR semantics, or variable-binding
  provenance.
- **`document/` (ADR 0089)** is the closest existing precedent for
  "typed structure with its own discipline," but it solves a different
  class of bug by a different method: it makes the BT-875 string-escape
  vector *unrepresentable*, rather than building a checkable structure and
  running a separate verification pass over it. There is no existing
  verifier-pattern precedent in this codebase; this ADR introduces one.
- **The codegen snapshot corpus is currently too thin to gate this
  migration** — measured: of the 318 snapshots under
  `test-package-compiler/tests/snapshots/`, exactly **1** contains a
  `letrec` loop, **1** contains `$bt_nlr`, **1** contains `ClassVars1`,
  **1** contains `class_vars_shadow`, and **4** contain `StateAcc`. A
  "byte-identical snapshots" gate over that corpus is near-vacuous for
  precisely the constructs this ADR migrates. Expanding the corpus is
  therefore a *prerequisite deliverable* of this ADR (Phase A3), not an
  assumed pre-existing safety net. (ADR 0089's byte-parity discipline,
  which this ADR follows in spirit, was likewise not a standing harness —
  its `.beam`-parity check was an ad-hoc `cmp -l` during its flag-day PR.)

### Constraints

- **Narrow scope, as specified by BT-3122 — with one addition.** The
  lowered form covers the control-flow + state-threading core: the
  `control_flow/` cluster, `threaded_expr.rs`, the state-versioning subset
  of `gen_server/methods.rs` (measured at roughly 1900–2400 of its 5805
  lines), **and the version-`Bind`-emitting slices of `expressions.rs` and
  `dispatch_codegen.rs`**: `generate_field_assignment`
  (`expressions.rs:552` calls `next_class_var()`; the analogous field
  paths drive `next_self_var()`/`next_state_var()`) and
  `dispatch_codegen.rs:463`. These two sites are the *sole producers* of
  the version bindings the verifier checks; an IR that cannot see its own
  `Bind` producers cannot verify them. The rest of those two files
  (expression and dispatch codegen generally) stays AST-directed and out
  of scope, as do `intrinsics.rs` and `value_type_codegen.rs`.
- **Explicit non-goals** (BT-3122): no full-pipeline IR covering all of
  Core Erlang codegen — already considered and rejected once, as "Typed
  Core Erlang IR," in [ADR 0018](0018-document-tree-codegen.md)
  §Alternatives Considered #3 ("over-engineered for our needs — we don't
  transform or optimize the IR, we just emit it"); that reasoning still
  holds for the codegen this ADR does not touch. No cerl-wire change —
  [ADR 0088](0088-direct-cerl-emission.md) stays closed/withdrawn; this
  ADR's IR is entirely internal to `beamtalk-core` and never reaches the
  Port. No behavior change — every phase must produce byte-identical
  output over the *expanded* snapshot corpus (Phase A3) plus an ordered
  diagnostic-stream equality check (the `BEAMTALK_CODEGEN_DIAGNOSTICS`
  pipeline's `Vec<Diagnostic>` order and multiplicity are observable and
  moving `ThreadingPlan` selection into lowering changes when diagnostics
  fire — parity must be asserted, not assumed).
- **The `Document`/text printer is unchanged.** The typed-leaf API (ADR
  0089) remains the only path from a lowered leaf to Core Erlang text.
- **Coordination with BT-3111/BT-3125.** BT-3125 (In Progress, PR open at
  the time of writing) is moving the writeback trio out of codegen's
  discretion into a driver-level `lower_module_for_codegen(&mut module,
  &analysis)` preparation step, explicitly noting *"if the BT-3122 ADR is
  accepted, this preparation step is where it slots in."* Because BT-3125
  is landing imminently and on the same snapshot gate, **it is a soft
  prerequisite for this ADR's Phase B onward** — building an interim
  analysis re-derivation path only to delete it later would reproduce the
  exact anti-pattern BT-3111 is closing. Phase A (types, verifier,
  snapshot expansion, measurement) has no dependency on BT-3125 and can
  proceed in parallel.
- **Coordination with BT-3112.** The metamorphic/differential harness in
  that epic is the unknown-unknown hunter for this bug space; this ADR's
  verifier is the known-invariant enforcer. They are complementary, and
  the Phase A0 measurement gate below gives an explicit decision point to
  descope this ADR toward its cheaper fallback (Alternative 1b) if the
  harness lands first and changes the value calculus.

## Decision

**Introduce a small, narrowly-scoped lowered IR — `ThreadedIr` — covering
state-version bindings (with frame identity), threading-mode selection,
shadow-write emission, and NLR relay boundaries, plus a verifier that
checks it before `Document` construction.** Everything else in codegen
stays AST-directed and unaffected. The decision is gated: Phase A0
measures IR-construction/verification cost on a fixed fixture set against
a pre-declared threshold before any migration phase begins (the ADR 0088
lesson, applied to ourselves).

### Pipeline shape

```
Beamtalk AST
    │  (BT-3125: lower_module_for_codegen(&mut module, &analysis))
    ▼
AST, prepared with threaded AnalysisResult / SemanticFacts
    │  (this ADR: lower_control_flow — the in-scope slices only)
    ▼
ThreadedIr                      ◄── verify(&ThreadedIr) -> Vec<VerifyError>
    │  (unchanged: typed-leaf Document construction, ADR 0089)
    ▼
Document
    │  (unchanged: Wadler-Lindig pretty-printer, ADR 0018)
    ▼
Core Erlang text ──► core_lint (unconditional, OTP) ──► BEAM
```

### The IR

```rust
/// Frame identity — allocated at each method entry, with_branch_context
/// entry, builder-fun entry. Version linearity is PER FRAME: the existing
/// counters are deliberately not SSA (with_branch_context resets
/// state_version to 0 per branch arm, so sibling arms legitimately both
/// produce State1 in disjoint scopes). Without frame identity, a linearity
/// check false-positives on every branching method — this field is a
/// design requirement, not a nicety.
struct FrameId(u32);

/// One of the three (formerly independent) version counters, unified in
/// NAMING AND IDENTITY ONLY. Per-prefix scope discipline (state:
/// reset+restore per branch; class_vars: restore-only, mutated-flag
/// sticky; self: currently neither — see Phase A2) remains explicit
/// per-prefix policy, preserved exactly.
struct VersionedVar {
    prefix: VersionPrefix,   // State | ClassVars | SelfVt
    version: usize,
    frame: FrameId,
}
/// NOTE: the State counter renders as `StateAcc{N}` inside non-hybrid
/// loop bodies — prefix rendering is a function of (counter, loop
/// context), decided at Document-construction time, not stored in the IR.
/// The unversioned `StateAcc` lambda *parameter* of foldl list-ops is a
/// separate node (AccParam), not a VersionedVar.

enum ThreadingMode { DirectParams, TupleAcc, Hybrid, StateAcc(StateAccFallbackReason) }

enum ThreadedStmt {
    /// A mutation: binds a fresh version from a prior one in the same
    /// frame. `shadow_write: bool` records whether this Bind also emits
    /// the ADR 0110 process-dictionary shadow write — modeling the side
    /// channel explicitly is what makes the ShadowWriteMissing check
    /// (below) possible.
    Bind { target: VersionedVar, source: VersionedVar, op: BindOp, shadow_write: bool },

    /// A loop or mutation-carrying conditional, with its mode already
    /// resolved (by the existing ThreadingPlan logic, run once during
    /// lowering instead of re-derived at emission).
    Threaded { mode: ThreadingMode, frame: FrameId, body: Vec<ThreadedStmt>, produces: Vec<VersionedVar> },

    /// An NLR boundary. Faithful to what codegen actually emits
    /// (mod.rs:2480-2549): the token-MATCHING catch arm binds its state
    /// from the thrown 4-tuple's pattern variable (NlrCatchVars.state_var
    /// — a fresh pattern binding, not a versioned var), and the
    /// token-non-matching (foreign) arm carries nothing and re-raises.
    /// The IR does NOT pretend the relay "carries" a VersionedVar.
    NlrCatch { boundary: NlrBoundary, token: TokenId, frame: FrameId },

    Return(ValueRef, VersionedVar),
}
```

`NlrBoundary` (`ActorReply | ClassMethod { has_class_vars: bool } | ValueType`)
is the existing enum from `mod.rs:902-911`, reused as-is.

### The verifier

```rust
enum VerifyError {
    /// A versioned var referenced with no producing Bind in its frame (or
    /// a parent frame per the explicit frame-flow rule). Catches the
    /// unbound-StateX class one layer earlier than core_lint, with a
    /// Beamtalk-source-attributable message instead of erlc's raw
    /// "unbound variable 'State3' in myMethod/2". Diagnosis-quality
    /// improvement over an existing backstop, not net-new detection.
    UnboundVersion { var: VersionedVar, at: Span },

    /// Per-frame linearity: within one FrameId, each version produced by
    /// exactly one Bind, consumed as the source of at most one successor.
    /// Frame-scoped by design — see FrameId above.
    NonLinearVersion { var: VersionedVar, producers: usize, consumers: usize },

    /// Replaces the four "unpack should emit no code" debug_asserts as a
    /// structural property: an optimized ThreadingMode node cannot
    /// contain an unpack Bind. (In release today this failure degrades to
    /// a core_lint unbound-variable error; the gain is a correct,
    /// centralized, source-attributed diagnosis.)
    ThreadingModeUnpackMismatch { mode: ThreadingMode, at: Span },

    /// The ADR 0110 CONTRACT check — regression-pinning, not
    /// counterfactual detection (see "Verifier honesty" below). Now that
    /// ADR 0110 established the invariant, it is checkable structurally:
    /// a class-var Bind at frame depth 0 (method top frame) inside a
    /// method whose body can relay a foreign NLR (has_class_vars: true
    /// boundary present) MUST have shadow_write: true. Fires if a future
    /// emission path forgets the shadow write ADR 0110's fix depends on,
    /// or if a new mutation site is added without it.
    ShadowWriteMissing { mutated: VersionedVar, at: Span },
}

fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError>;
```

**Failure behavior.** The verifier runs in debug builds and CI
unconditionally. In release builds, a `VerifyError` does **not** panic —
per CLAUDE.md ("never panic on user input"; "always return
`(Result, Vec<Diagnostic>)`"), it is reported as an internal-error
diagnostic attached to the compile result while emission proceeds with the
generator's output (i.e., release behavior on a verifier bug is *no worse
than today*, plus a diagnostic; debug/CI behavior is a hard failure). A
false positive in a shipped compiler must degrade to a warning, not a
refusal to compile valid code.

### Verifier honesty — what this catches and what it cannot

These limits are structural, not incidental — they follow directly from how
the generator emits NLR scaffolding (`wrap_body_with_nlr_catch`,
`mod.rs:2480-2549`, and `NlrCatchVars`, `mod.rs:880-890`):

- **The verifier checks what the lowering pass said, not what it meant.**
  The lowering pass and the emitter share their source of truth
  (`current_class_var()`, `ThreadingPlan`, `classify_body_expr`'s
  outputs). A check comparing the generator against itself is silent when
  both are consistently wrong. The checks above are chosen to be
  *structural* — properties of the IR shape that don't depend on the
  generator's beliefs being correct: a Bind either has a shadow-write
  companion or it doesn't; a mode node either contains an unpack or it
  doesn't; a version either has a producer in its frame or it doesn't.
- **`ShadowWriteMissing` would not have caught ADR 0110's bug before it
  was known.** Pre-0110, the shadow write didn't exist, the invariant was
  unstated, and no verifier rule could have encoded it. The claim this ADR
  makes is regression-pinning and forward extension: the invariant, once
  paid for at production cost, becomes mechanically enforced at every
  current and future mutation site, rather than living in a code comment
  and one BUnit test. The unknown-unknown-hunting role belongs to
  BT-3112's metamorphic harness.
- **Part of the 0110 contract lives on the Erlang side and is outside any
  Rust-side verifier's reach.** The shadow key shape and erase discipline
  in `invoke_class_method/7` (`beamtalk_class_dispatch.erl`) must agree
  with what codegen emits. Per CLAUDE.md's cross-boundary rule, that
  agreement needs a **shared conformance fixture** (a compiled fixture
  whose shadow key/erase behavior both sides assert against), not a
  comment — added in Phase D alongside the `ShadowWriteMissing` check as
  the two halves of the same contract.

### Worked example: what the IR and verifier actually do for the 0110 class

Post-ADR-0110, `generate_field_assignment` (`expressions.rs:552`) emits,
for a top-frame class-var mutation:

```erlang
let ClassVars1 = call 'maps':'put'('runs', Val, ClassVars0) in
let _ = call 'erlang':'put'({'$bt_class_vars_shadow', ...}, ClassVars1) in
```

Lowered:

```
Bind { target: ClassVars1@F0, source: ClassVars0@F0, op: Put(runs, ...), shadow_write: true }
NlrCatch { boundary: ClassMethod { has_class_vars: true }, token: T0, frame: F0 }
```

`verify()` is silent. If a future refactor of the emission site (or a new
mutation path — say, a future `self.field +=`-style sugar lowered through a
different function) produces `shadow_write: false` in a
`has_class_vars: true` method, `verify()` reports `ShadowWriteMissing` at
the Beamtalk source line — in CI, on the day the regression is written.
That is the deliverable: the invariant ADR 0110 discovered at production
cost becomes structurally un-forgettable.

### Observable behavior — unchanged

No Beamtalk program's compiled output or REPL behavior changes:

```
st> CollectionDriver runCount
0
st> CollectionProbe escapeAfterCountedRun: #(1, 2, 3)
2
st> CollectionDriver runCount
1
```
(identical to ADR 0110's example — verified over the *expanded* snapshot
corpus, Phase A3, at the end of each migration phase.)

## Prior Art

| System | Approach | What we adopt / reject |
|---|---|---|
| **rustc: HIR → THIR → MIR** | Each IR is progressively simplified and single-purpose; MIR specifically exists to run flow-sensitive checks (the borrow checker) via a general dataflow framework (`rustc_mir_dataflow`), not to represent the whole language. [Source](https://github.com/rust-lang/rustc-dev-guide/blob/main/src/mir/index.md) | **Adopted:** the "small, single-purpose, narrowly-scoped IR whose job is to make one class of check possible" shape — what makes the "no full-pipeline IR" non-goal defensible rather than merely expedient. |
| **Swift SIL ownership verifier** | A static verifier over an SSA-form IR checks ownership-model invariants at compile time, explicitly framed as catching bugs in *SILGen and optimization passes* — compiler bugs, not user bugs. [Source](https://forums.swift.org/t/proposal-sil-ownership-model-verifier/4665) | **Adopted:** the framing that this verifier's job is to catch *codegen's own* bugs. **Adapted with a caveat SIL's docs are honest about and so is this ADR:** a verifier enforces stated invariants; SIL's ownership verifier postdates and encodes an ownership *model* — it did not discover the model. Same relationship as `ShadowWriteMissing` to ADR 0110. |
| **Cranelift's `verifier` module** | CLIF ships an explicit verifier pass checked in CI and optionally per-compilation. | **Adopted:** "the verifier is a standard, expected part of a codegen backend that lowers through an internal IR" — ordinary compiler engineering, not novel infrastructure. |
| **MLIR dialects + per-op `Verifier`** | Narrow, purpose-built dialects coexist, each with its own verification; only the program parts needing a dialect's semantics are lowered into it. | **Adopted as the closest structural analogy:** `ThreadedIr` is a small dialect for the control-flow/state-threading subset, verified on its own terms, coexisting with AST-directed codegen for everything else. |
| **Erlang/OTP's `core_lint`** | Runs unconditionally on every `from_core` compile, checking well-formedness (unbound/duplicate variables) — downstream of codegen, syntactic only. | **Kept, complemented, not replaced.** It also serves as the release-mode backstop for several of this ADR's checks (see §Verifier honesty) — the verifier's gain over it for those is diagnosis quality and source attribution, and this ADR says so rather than claiming net-new detection. |
| **Gleam's codegen** | No actor/mutable-state threading requirement analogous to Beamtalk's; its `Document`-tree codegen doesn't face this problem. | **Not directly transferable** — noted for completeness. |
| **Pharo/Squeak Smalltalk** | Mutation is a direct memory write; NLR unwinds a real stack; no "commit" step exists for a mutation to survive. | **Confirms the constraint, not the solution:** Beamtalk inherits mutable-variable semantics on an immutable substrate, so it must reconstruct "the assignment already happened" via functional threading — this ADR targets the reconstruction machinery Smalltalk never needed. |

## User Impact

| Persona | Impact |
|---|---|
| **Newcomer** | None. No language syntax, semantics, or REPL output changes. |
| **Smalltalk developer** | None directly; indirectly, the invariant that `^` never silently drops side effects — established for class vars by ADR 0110 — becomes mechanically pinned against regression rather than convention-guarded. |
| **Erlang/BEAM developer** | None to generated `.beam` artifacts. Verifier error shapes will read like `core_lint` messages one layer earlier, with Beamtalk-source attribution. |
| **Production operator** | None at runtime. Verifier cost is compile-time only, budgeted and measured in Phase A0 before any migration proceeds; in release builds a verifier finding degrades to an internal-error diagnostic, never a panic or a refusal to compile (CLAUDE.md error-recovery rules). |
| **Tooling developer (LSP, debugger)** | Mildly positive, longer-term: a structured `ThreadedIr` with spans is a better substrate for future "why did this loop fall back to StateAcc?" tooling than log-line parsing; no such tooling is proposed here. |
| **Compiler contributor** | The main audience, and honestly a mixed bag: (+) adding a stateful construct becomes "add a `ThreadedStmt` case + verifier check" with single-sourced classifier/emitter decisions; the three duplicated counters collapse into one frame-disciplined type. (−) new internal API surface to learn; a real test-migration cost (see Consequences — the existing `Document`-asserting tests survive only via the `lower_and_render` shim committed to in Phase A1); and the in-scope files are under active feature development, so phases carry a stated conflict protocol (see Implementation). |

## Steelman Analysis

### Option A: Narrow lowered IR + verifier, phase-gated with a measurement gate (Recommended)

- 🧑‍💻 **Newcomer-to-compiler-internals contributor**: "When I add a new stateful loop variant or a new mutation-emission path, CI tells me — with a Beamtalk-source-attributed error — if I forgot the shadow write or emitted an unpack in a direct-params loop, instead of me discovering it as an erlc unbound-variable message pointing at generated code, or worse, as silently-wrong runtime state."
- 🎩 **Smalltalk purist**: "ADR 0110 established that `^` must never silently drop side effects, at the cost of a shipped bug. This makes that invariant un-forgettable at every future mutation site instead of living in one test and a comment."
- ⚙️ **BEAM veteran**: "`core_lint` proved the value of an unconditional structural check in the pipeline. This is the same idea one layer up, and it's honest that for several checks it improves the diagnosis rather than adds detection — that's still worth having when the alternative is an erlc error naming a generated variable three layers from the cause."
- 🏭 **Operator**: "Zero runtime change, zero wire change, release-mode failure degrades to a diagnostic. The Phase A0 gate means if the IR's compile-time cost is real, the project stops before migrating anything — the ADR 0088 discipline applied in advance rather than in retrospect."
- 🎨 **Language/compiler designer**: "The three version counters with three inconsistent disciplines — one of which (`self_version`) is saved/restored *nowhere* — is a latent bug factory independent of everything else in this ADR. The IR is the natural home for fixing it with frame identity rather than a fourth convention."

### Option B: Verifier without IR

Two forms, split because they have opposite verdicts:

**B1 — re-derive post-hoc (from Document/text or generator state after the
fact).** Rejected: the needed structure (version provenance, frame
identity, mode membership) doesn't exist post-emission and re-deriving it means
re-implementing `classify_body_expr`/`ThreadingPlan` a second time — the
§6 duplication anti-pattern.

**B2 — record decisions as they are made (the strong form, and this ADR's
designated fallback).** `ThreadingPlan`, `BodyExprKind`, and the
version-counter calls all exist as in-memory values at emission time
today. Push each decision into a `Vec<Decision>` side-channel as it is
made; verify that. Re-derives nothing, needs no emission restructuring,
gets the mode/unpack and classifier/emitter checks always-on at a fraction
of the cost.

- 🧑‍💻 **Newcomer**: "No new pipeline stage to learn — the codegen I read is still the codegen that runs."
- ⚙️ **BEAM veteran**: "This is the 90/10 point. The checks that are actually sound (§Verifier honesty) are exactly the ones this gets."
- 🏭 **Operator**: "No migration freeze on 11K LOC of actively-developed files."
- 🎨 **Language designer (sharpest form)**: "On §Verifier honesty's own accounting, the verifier's *checks* don't require the IR — they require the *decisions*, which already exist as data. The IR's remaining unique contribution is single-sourcing (one node, one emitter) and the counter unification. Is that worth the migration?"

**Why A still wins — narrowly.** B2 checks that
two independently-computed decisions agree; A removes the second
computation. B2's recorder must itself be maintained at every decision
site (a site that forgets to record is invisible to the validator — the
same "remember to keep in sync" failure mode, relocated); A's IR is the
decision record *and* the emission input, so an unrecorded decision cannot
emit. And the counter unification with frame discipline (the
`self_version` landmine) needs a typed home either way. But the margin is
real only if the migration cost stays bounded — hence the Phase A0 gate,
and hence B2's designation as the **explicit descope target** if A0 fails
its threshold: everything in Phase A1 (types, checks, snapshot corpus)
transfers to B2 directly.

### Option C: Status quo — `debug_assert!`s + `core_lint` + code review

- 🧑‍💻 **Newcomer**: "The state-threading code has shipped correct for most of its history; review caught 0110 eventually."
- ⚙️ **BEAM veteran**: "In release, all six assert-guarded failures degrade to `core_lint` errors anyway — there's a backstop, not a void."
- 🏭 **Operator**: "Zero cost, zero risk, zero freeze."
- 🎨 **Language designer**: "One shipped bug in the codebase's lifetime for this class; is any of this proportionate?"

**Why A wins despite this steelman.** The six assertions *do*
have a release-mode backstop (`core_lint`), but it fires far from the
cause with an Erlang-level message about generated code, and it covers
only the failures that happen to produce malformed output — a
classification/emission drift that produces *well-formed but wrong* output
(the 0110 shape) has no backstop at all. The status quo's real gap is not
"zero protection" but "no protection for the silent-wrong class plus poor
diagnosis for the loud class," and — decisively — nothing in the status
quo addresses the `self_version` save/restore hole or prevents the next
mutation-emission site from forgetting the shadow write.

### Option E: Metamorphic/differential testing first (BT-3112), IR later or never

- 🧑‍💻 / ⚙️ / 🏭 / 🎨 (shared core argument): "BT-3112's planned metamorphic harness would have caught ADR 0110's actual bug — result-preservation under semantics-preserving transforms is precisely the oracle for 'right value, lost side effect.' It hunts unknown-unknowns, covers all of codegen (not just the in-scope 30%), requires no migration of working code, and is already committed roadmap. Run it first; buy the IR only if the harness finds bug clusters it can't localize."

**Why this ADR proceeds anyway — as a complement, with the sequencing
acknowledged.** The harness and the verifier answer different questions:
the harness detects *that* a semantic property broke somewhere in a
generated program; the verifier (and the IR's single-sourcing) prevents
and localizes specific, known, named invariant violations at the source
line that introduced them, and removes the classifier/emitter dual
computation that produces them. The harness also does nothing for the
counter-discipline defect, which is a compiler-internals problem invisible
to any black-box oracle. This ADR's Phase A runs in parallel with
BT-3112's early children, and the A0 gate provides the explicit
reconsideration point if the harness's early results change the calculus.
What this ADR does *not* claim is that the verifier substitutes for the
harness (§Verifier honesty).

### Tension Points

- **A vs. B2 is the live disagreement** — reasonable engineers land on
  either side of "single-sourcing is worth a phased migration of ~11K
  LOC." This ADR's answer is conditional (the A0 gate + B2 as designated
  descope), which is as honest as the disagreement allows.
- **Scope discipline remains the whole argument for A over D** (the
  full-pipeline IR): the phase gates and the measurement gate exist to
  keep A from becoming D by accretion.
- **Freeze cost vs. correctness payoff:** the in-scope files carry active
  feature work (BT-references spanning BT-1275→BT-3055). The conflict
  protocol (Implementation, below) is: feature work lands first, migration
  phases rebase; each phase has a wall-clock cap, after which it is split
  or abandoned rather than extended.

## Alternatives Considered

### 1a. Verifier without IR — re-derive post-hoc
See Steelman Option B1. **Rejected**: re-derivation duplicates the
classifier/plan logic — the §6 anti-pattern.

### 1b. Verifier without IR — record decisions as made
See Steelman Option B2. **Not rejected — designated descope target.** If
Phase A0's measurement fails its threshold, or if BT-3112's harness
results change the value calculus before Phase B begins, this ADR's
deliverables reduce to: Phase A1's types and checks re-hosted on a
decision side-channel + Phase A2's counter unification + Phase A3's
snapshot corpus. That outcome is explicitly acceptable and pre-planned,
not a failure mode.

### 2. Status quo
See Steelman Option C. **Rejected** (no
protection for the silent-wrong class; poor diagnosis for the loud class;
`self_version` hole and shadow-write forgettability unaddressed).

### 3. Unify the three version counters only
**Absorbed rather than rejected.** This is precisely Phase A2, and it is
independently justified — done as *naming-only* unification with
per-prefix discipline preserved and pinned by tests, plus the typestate
hardening of Alternative 5. What the full ADR adds beyond it is the
single-sourcing of classifier/emitter decisions and the shadow-write
contract check. (Note the six `debug_assert!`s are *not* an argument
against this alternative: none of the six is on ADR 0110's path — they
guard the mode/unpack and classifier/emitter invariants, which counter
unification alone indeed does not address.)

### 4. Full-pipeline typed Core Erlang IR
**Rejected** — per ADR 0018's still-valid reasoning and BT-3122's explicit
non-goal.

### 5. Typestate hardening in place (newtype `VersionedVar` + RAII branch guard, no IR)
Make the version counters' invariants unrepresentable in place: a
`VersionedVar` newtype whose only constructors are the `next_*` methods,
emitter signatures taking `VersionedVar` instead of `String` (an
unproduced version can no longer be named), and an RAII guard replacing
`with_branch_context`'s manual save/restore (closing the `self_version`
hole by construction). **Absorbed into Phase A2** — this is how the
counter unification should be built regardless of whether the full IR
lands, and it delivers `UnboundVersion`-by-construction (stronger than a
check) for the counter slice specifically. It does not cover mode/unpack
membership, classifier/emitter single-sourcing, or the shadow-write
contract, which is what the IR proper adds.

### 6. Erlang-side strengthening only
The dialyzer-visible `{nlr_relay, ...}` outcome variant
(ADR 0110 §Neutral) is today's strongest guard for the 0110 contract's
runtime half. Strengthening it (EUnit conformance on
`class_method_outcome()`, an FFI-rule check) is worth doing and Phase D's
shared conformance fixture does part of it — but it cannot see the Rust
side's emission decisions at all, so it is a complement, not an
alternative. **Partially adopted** (the fixture); rejected as a
substitute.

## Consequences

### Positive
- **ADR 0110's invariant becomes structurally un-forgettable** at every
  current and future class-var mutation site (`ShadowWriteMissing` +
  the cross-boundary conformance fixture), instead of living in one
  emission site's code and one BUnit test.
- **The classifier/emitter and mode/unpack dual-computations are removed
  by construction**, not just checked — one IR node, one emitter. The six
  `debug_assert!`s are deleted with their invariants absorbed.
- **The three-counter duplication and the `self_version` save/restore hole
  are closed** with frame identity and typestate constructors (Phase A2 —
  independently valuable even under the descope outcome).
- **Diagnosis quality:** failures that today surface as erlc messages
  about generated variables surface as Beamtalk-source-attributed verifier
  errors in CI.
- **`Document`, typed leaves, printer, wire, runtime: unchanged.**
- **The decision is measurement-gated (Phase A0) with a pre-planned
  descope target (Alternative 1b)** — the ADR 0088 lesson applied
  prospectively.

### Negative
- **New internal API surface** for contributors to the in-scope files.
- **A real test-migration cost:** ~1,484 `#[test]`s
  and 231 `to_pretty_string()` assertion sites live under `codegen/`
  (`tests/control_flow.rs` alone is 2,591 lines; `list_ops/tests.rs`
  2,660). Functions returning `ThreadedIr` instead of `Document` would
  break every test asserting on their output. **Mitigation, committed in
  Phase A1:** a `lower_and_render(…) -> Document` shim preserving existing
  test call shapes verbatim; tests migrate opportunistically, never as a
  phase precondition.
- **Compile-time cost is real until measured:** IR construction allocates
  a second representation for the hottest codegen paths. Phase A0 measures
  it on a fixed fixture set against a pre-declared threshold (proposed:
  abandon/descope if end-to-end `beamtalk build` on the fixture set
  regresses > 3%; tune in `/plan-adr`).
- **Migration freeze risk on actively-developed files.** Protocol:
  feature work lands first and phases rebase; each phase carries a
  wall-clock cap (proposed: one release cycle), after which it is split
  or handed to the descope path rather than extended.
- **The verifier can produce false confidence** — it enforces stated
  invariants only. Unknown-unknown discovery remains BT-3112's harness's
  job, and this ADR says so.
- **Release-mode verifier findings are diagnostics, not errors** — a real
  compiler-internal bug in release degrades to today's behavior plus a
  warning. This is the CLAUDE.md-compliant choice, but it means the hard
  guarantee is CI-time, not user-compile-time.

### Neutral
- **`NlrBoundary`, `ThreadingPlan`, `StateAccFallbackReason`,
  `BodyExprKind` are reused, not redesigned** — the IR changes *when*
  their outputs become durable data, not what they compute.
- **The `BEAMTALK_CODEGEN_DIAGNOSTICS` pipeline keeps its wording and env
  vars; firing *order* may shift** when plan selection moves into
  lowering — ordered diagnostic-stream equality is therefore an explicit
  per-phase exit criterion rather than an assumption.
- **ADR 0110's runtime fix is untouched**; this ADR adds the compile-time
  half of its contract, plus the shared conformance fixture the
  cross-boundary rule in CLAUDE.md requires.

## Implementation

Phase-gated with a measurement gate at the front. Exit criteria for every
migration phase (B–D): byte-identical output over the **expanded** snapshot
corpus (A3), ordered diagnostic-stream equality, verifier green on all
stdlib fixtures, full behavioral suite green, and the phase's superseded
`debug_assert!`s deleted from source. The `/plan-adr` output decomposes
into issues; this section names the commitment level.

### Phase A0 — Measurement gate (S)
Prototype the IR types and lower *one* construct (`whileTrue:`
direct-params) behind a flag; measure end-to-end `beamtalk build` on a
fixed fixture set. Pre-declared gate: proceed only if regression ≤ 3%
(threshold finalized in `/plan-adr`). On failure: descope to
Alternative 1b, carrying Phases A1–A3 forward on the decision-side-channel
design.

### Phase A1 — IR types + verifier + test shim (S)
`threaded_ir.rs`: `FrameId`, `VersionedVar`, `ThreadedStmt`,
`VerifyError`, `verify()`, plus the `lower_and_render` test shim. Unit
tests against hand-built IR fixtures. Nothing consumes the IR yet — this
sub-phase is genuinely zero-behavioral-risk.

### Phase A2 — Counter unification via typestate (S, snapshot-gated)
`VersionedVar` newtype with constructor-only production, RAII branch
guard replacing `with_branch_context`'s manual save/restore, per-prefix
discipline preserved exactly (state: reset+restore; class_vars:
restore-only + sticky mutated flag; self: **given** the save/restore it
currently lacks, as an explicit, snapshot-gated behavior decision — if
snapshots change, that's a bug find, not a regression). Discipline
asymmetries pinned by dedicated tests. *Not* zero-risk, hence separated
from A1 and gated on the corpus.

### Phase A3 — Snapshot corpus expansion (S–M, prerequisite for B–D)
Land fixtures covering the mode matrix — {DirectParams, TupleAcc, Hybrid,
StateAcc} × {Actor, ValueType, ClassMethod} × {NLR present/absent} ×
{class-var mutated/not} — before any migration phase. Measured baseline
today: of 318 snapshots, `letrec` 1, `$bt_nlr` 1, `ClassVars1` 1,
`class_vars_shadow` 1, `StateAcc` 4. Without this phase the parity gate is
vacuous and the plan's "learning from ADR 0088" claim is unsupported.

### Phase B — `while_loops.rs` + `counted_loops.rs` (M)
Migrate the letrec-loop family. Soft prerequisite: BT-3125 landed (so
lowering consumes the threaded `AnalysisResult` from day one instead of
building a to-be-deleted re-derivation). Deletes the four unpack
`debug_assert!`s.

### Phase C — `list_ops/` (L)
Migrate the foldl list-op family — sized **L**, not a mechanical
follow-on to Phase B: it introduces invariant classes Phase B never
exercises, each needing IR modeling and verifier coverage: (1) the flat `{FoldAcc, Var1..VarN}` positional-unpack
accumulator discipline (distinct from parameter threading); (2)
`select_tuple_acc`'s ValueType-context exclusion (`control_flow/mod.rs:448-466`);
(3) the recursive inter-construct `list_op_needs_stateacc_fallback`
analysis for nested list ops; (4) early-exit ops (`detect:`,
`anySatisfy:`, `takeWhile:`, …) changing accumulator liveness at the exit
point. May split fold-shaped vs. early-exit-shaped in `/plan-adr`.
`list_ops/tests.rs` (2,660 lines) migrates via the A1 shim.

### Phase D — Actor/class-method threading + NLR + shadow-write contract (L)
Migrate the state-versioning slice of `gen_server/methods.rs`
(`BodyExprKind`, `generate_body_exprs_with_reply`, tier-2 helpers), the
`ClassVars` threading path **including its `Bind`-emission sites in
`expressions.rs:552` and `dispatch_codegen.rs:463`** (these are the
producers of everything the verifier checks),
`conditionals.rs`, `exception_handling.rs`, and `threaded_expr.rs`'s
boundary adapter. Lands `ShadowWriteMissing` plus the **cross-boundary
conformance fixture** pinning codegen's shadow-write emission against
`invoke_class_method/7`'s key/erase discipline (CLAUDE.md cross-boundary
rule). Deletes the two routing `debug_assert!`s. May split into sub-PRs in
`/plan-adr`.

### Conflict protocol (all phases)
Feature work in the in-scope files lands first; migration phases rebase.
Each phase has a wall-clock cap (proposed: one release cycle); a phase
that exceeds it is split or routed to the Alternative 1b descope, not
extended.

### Affected Components
- **New**: `codegen/core_erlang/threaded_ir.rs` (types, verifier, shim);
  snapshot fixtures (A3); cross-boundary conformance fixture (D).
- **Modified**: `control_flow/` (all production files), `threaded_expr.rs`,
  `gen_server/methods.rs` (state-versioning slice), `mod.rs` (counters →
  `VersionedVar` + RAII guard), **`expressions.rs` and
  `dispatch_codegen.rs` (version-`Bind`/shadow-write emission sites only)**.
- **Unchanged**: `document/` (ADR 0089), `intrinsics.rs`,
  `value_type_codegen.rs`, the remainder of `expressions.rs`/
  `dispatch_codegen.rs`, all Erlang-side runtime code (the conformance
  fixture asserts against it but does not change it), wire format.

### Verification
- **Expanded snapshot corpus (A3) byte-parity** after every phase — the
  real gate; the pre-expansion corpus covers the in-scope constructs in
  only 1–4 of 318 snapshots and cannot serve as one.
- **Ordered diagnostic-stream equality** (`BEAMTALK_CODEGEN_DIAGNOSTICS`
  output as a sequence, not a set) after every phase.
- **Verifier green** on all `stdlib/test/*.bt` +
  `stdlib/bootstrap-test/*.btscript` fixtures in CI — `just verify-threaded-ir`
  (BT-3136), wired into `just ci`.
- **Behavioral suite** (`just test-stdlib`/`test-bunit`/
  `test-repl-protocol`) green after every phase.
- **0110 contract, both halves**: `ShadowWriteMissing` unit tests (fires
  when a top-frame class-var Bind in a `has_class_vars` method lacks the
  shadow write; silent on the fixed shape) + the cross-boundary
  conformance fixture against `invoke_class_method/7`. Honest framing per
  §Verifier honesty: regression-pinning, not counterfactual detection.

## References

- Related issues:
  - [BT-3122](https://linear.app/beamtalk/issue/BT-3122) — this ADR
  - [BT-3141](https://linear.app/beamtalk/issue/BT-3141) — follow-up epic
    (§Addendum) completing Option A single-sourcing; in progress
  - [BT-3142](https://linear.app/beamtalk/issue/BT-3142) — this addendum's
    own issue
  - [BT-3144](https://linear.app/beamtalk/issue/BT-3144) — renderer
    foundation, consumes the `RenderCtx` sketch in §Addendum
  - [BT-3145](https://linear.app/beamtalk/issue/BT-3145) — pilot migration,
    first checkpoint for the restated ≤3% measurement gate; blocked as
    scoped until the designs in Addendum 2 land (its own investigation
    comment, 2026-08-11T19:02, is Addendum 2's evidence source)
  - [BT-3153](https://linear.app/beamtalk/issue/BT-3153) — Addendum 2's own
    issue: condition/case-split loop node design + Bind naming
    reconciliation, unblocking BT-3145
  - [BT-3111](https://linear.app/beamtalk/issue/BT-3111) /
    [BT-3125](https://linear.app/beamtalk/issue/BT-3125) — analysis→codegen
    handoff; BT-3125 is a soft prerequisite for Phase B onward
  - [BT-3112](https://linear.app/beamtalk/issue/BT-3112) — generated-code
    correctness epic; its metamorphic harness is the unknown-unknown
    hunter this ADR's verifier explicitly does not replace (§Verifier
    honesty, Steelman Option E)
  - [BT-3035](https://linear.app/beamtalk/issue/BT-3035) — ADR 0110's
    implementation epic; `ShadowWriteMissing` + the conformance fixture
    pin its invariant
  - BT-3115 — `core_lint` readability (the release-mode backstop this
    ADR's checks improve upon rather than replace)
- Related ADRs:
  - [ADR 0018](0018-document-tree-codegen.md) — `Document` tree; its
    §Alternatives #3 rejection of a full-pipeline IR still holds for
    everything out of this ADR's scope
  - [ADR 0041](0041-universal-state-threading-block-protocol.md) — the
    `{Value, StateAcc}` and 4-tuple NLR conventions formalized as IR data
  - [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) — the
    semantic split `NlrBoundary` encodes
  - [ADR 0088](0088-direct-cerl-emission.md) — wire-format proposal,
    closed; source of the measurement-gate discipline Phase A0 applies
  - [ADR 0089](0089-typed-document-leaves.md) — typed-leaf printer,
    unchanged; precedent for byte-parity migration (via ad-hoc `cmp -l`,
    not a standing harness — which is why A3 exists)
  - [ADR 0109](0109-block-scoped-class-methods-run-blocks-in-the-caller.md)
    — block-runs-where-invoked semantics underlying the relay problem
  - [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) — the
    shipped bug whose invariant this ADR pins; runtime fix untouched
- Documentation:
  - `docs/development/debugging.md` §"Codegen Diagnostics", §"core_lint"
  - `docs/development/architecture-principles.md` §6 (duplication /
    shared-leaf pattern), CLAUDE.md cross-boundary conformance rule
- Code (all under `crates/beamtalk-core/src/codegen/core_erlang/` unless
  noted):
  - `control_flow/` (13 files, 16,329 LOC), `threaded_expr.rs` (416),
    `gen_server/methods.rs` (5805; ≈1900–2400 in scope) — in scope
  - `expressions.rs:552`, `dispatch_codegen.rs:463` — the
    version-`Bind`/shadow-write emission sites (in scope)
  - `mod.rs` — `NlrBoundary` (:902), `NlrCatchVars` (:880),
    `wrap_body_with_nlr_catch` (:2480), `with_branch_context` (:2064),
    counters (:1132, :1211, :2088, :2104)
  - `state_codegen.rs:42` (`StateThreading`), `util.rs:38`
    (`versioned_var`)
  - `../core_erlang_validity_tests.rs` — existing text-shape properties,
    unaffected
  - `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` —
    `invoke_class_method/7`, the Erlang half of the 0110 contract
  - `runtime/apps/beamtalk_compiler/src/beamtalk_compile_diagnostics.erl`
    — `core_lint` integration
