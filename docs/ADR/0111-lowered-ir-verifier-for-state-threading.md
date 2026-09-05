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

## Addendum 5 (2026-08-12): Per-shape Bind sequences for conditionals + exception_handling (BT-3157) — BT-3146's implementation table

This is the per-shape design pass BT-3146's own investigation
(2026-08-12T08:11 comment) and Addendum 4 §"Why BT-3146 needs something
else" both called for: every `BodyExprKind` mutation shape in
`control_flow/conditionals.rs`'s `generate_conditional_branch_inline` and
`control_flow/exception_handling.rs`'s
`generate_exception_body_with_threading_inner`, each compiled from a
minimal repro with a locally-built `beamtalk` binary, its real Core Erlang
read from the emitted `.core` file, and its exact
`Bind`/`Gensym`/`ValueRef::Doc` decomposition pinned against that output —
including explicit gensym mint order per shape, the bug class found three
times this cycle (BT-3144's initial commit, Addendum 2 Gap 2's composition
analysis, Addendum 4 Gap 3).

All line numbers are against `crates/beamtalk-core/src/codegen/core_erlang/`
unless noted, current as of `main` at commit `d1612626` (BT-3156's landing).
Per Addendum 2's practice: resolve by symbol name first, treat `:NNN` as a
hint. Every compiled fragment below is quoted from a real `.core` file
produced by `beamtalk build` on the cited repro (span `-|` annotations
elided for readability; nothing else altered).

### Scope and routing boundaries — what these two body loops actually serve

Empirical routing facts a re-attempt must not re-derive wrongly:

- **`generate_conditional_branch_inline` has six producing functions across
  seven call sites, not four.** The four `conditionals.rs` functions
  (`generate_if_true_with_mutations`, `generate_if_false_with_mutations`,
  `generate_if_true_if_false_with_mutations`,
  `generate_if_not_nil_with_mutations`) account for **five** raw calls —
  `generate_if_true_if_false_with_mutations` calls it twice, once per arm
  (`conditionals.rs:237,241`) — plus **two** more from BT-3139's
  instrumentation pass: `intrinsics.rs:1695` (REPL-mode block inlining)
  and `expressions.rs:2592` (the `match:`-arm case inside
  `generate_match_arm_body`). All seven calls reach the same body loop, so
  migrating it once gives all seven real per-arm IR.
  Separately: BT-3139 added `check_branch_frame_linearity` to **three**
  sites (the production doc comment at `control_flow/mod.rs:1318-1321`
  records "BT-3134's original six plus BT-3139's three"), but only **two**
  of those three — `intrinsics.rs:1701` and `expressions.rs:2600` — check
  a `generate_conditional_branch_inline` arm. The third,
  `expressions.rs:1125` (inside `generate_block_stateful`), checks a
  **different** body loop entirely — `generate_block_stateful_body`, the
  general Tier-2-stateful-block-body helper used for list-op/message-send
  block arguments, unrelated to `ifTrue:`/`ifFalse:`/`match:` — and none
  of this addendum's shapes decompose it. So of the module's nine
  `check_branch_frame_linearity` call sites, **eight** are in scope for
  this addendum's migration (the four `conditionals.rs` sites,
  `intrinsics.rs:1701`, `expressions.rs:2600`, and
  `exception_handling.rs:446,649`) and flip from scalar synthesis to real
  IR together (§Migration order, step 4); the ninth, `expressions.rs:1125`,
  is out of scope and stays on scalar synthesis until a separate pass
  decomposes `generate_block_stateful_body`.
- **Value-type conditionals never route here.** Compiled evidence: `Object
  subclass: S14` with `flag ifTrue: [x := 2]` emits `case _Cond2 of
  <'true'> when 'true' -> let X = 2 in X <'false'> when 'true' -> X end` —
  plain rebinding, no `StateAcc`, no `{Result, State}` tuple. A different,
  simpler path owns this shape (`value_type_codegen.rs`'s vt-conditional
  family — `generate_vt_conditional_branch` (`value_type_codegen.rs:3089`)
  and the `_CondResult` wrapper it builds
  (`value_type_codegen.rs:2762`)); the `SelfVt` prefix and its restore-only
  discipline are **out of these decompositions' scope**.
- **Class-var mutations never route here either — by construction.**
  Direct class-var writes in threaded bodies are rejected at compile time
  (`generate_field_assignment_open`'s BT-3140
  `ClassVarAssignmentInThreadedBody` check, `dispatch_codegen.rs:2364`),
  and a class-method conditional containing a class-var-mutating self-send
  (`class m: flag => flag ifTrue: [self bump]`) compiles to a **runtime
  `ifTrue:` dispatch closure** (`beamtalk_message_dispatch:send(_flag3,
  'ifTrue:', [fun () -> ...])` with the `_CMR`/`ClassVars1` unwrap inside
  the fun — compiled and read), not through
  `generate_conditional_branch_inline` at all. So no `ClassVars`-prefixed
  `Bind` can appear inside these arms today — no exception: shape C4
  below looks like a class-var-mutating local-assign candidate, but
  turns out (on inspection) to *also* never reach these arms — its
  natural repro routes through `value_type_codegen.rs`'s vt-conditional
  path instead and breaks there, outside either body loop this addendum
  covers.
- **`generate_field_assignment_open`'s hybrid full-extract sub-branch
  (`in_hybrid_loop && hybrid_mutated_fields`, `dispatch_codegen.rs:2378`)
  is unreachable from these arms.** A loop body containing a
  mutation-carrying conditional falls back to `StateAcc` mode
  (`StateAccFallbackReason::ControlFlowMutations`,
  `control_flow/mod.rs:74`) — confirmed by compiling a `whileTrue:` whose
  body contains `i > 1 ifTrue: [self.n := self.n + 1]`: the loop emits
  `letrec 'while'/1 = fun (StateAcc) -> ...`, StateAcc-mode, no
  pre-extracted field params. Hybrid mode and inline mutation conditionals
  are mutually exclusive by the plan selector, so the sub-branch needs no
  decomposition here. (Likewise `generate_local_var_assignment_in_loop`'s
  `!in_loop_body` else-branches, `control_flow/mod.rs:3395-3399`, `:3444-3448`:
  `with_branch_context` sets `in_loop_body = true` unconditionally, so
  inside these two body loops the `format!("State{}", ...)` legs never
  run.)

### The decomposition vocabulary — five building blocks, three rules

Every shape below decomposes into existing vocabulary plus Addendum 4's
`ThreadedStmt::Statement` (a dependency, not new design — see §Migration
order, PR 1):

1. **`Statement(doc, span)`** — genuinely non-threading text: value-temp
   `let`s, tuple/element extraction of a *result* (not state), destructure
   bindings, `__local__` re-reads, pure sends. Carries its own trailing
   glue exactly as Addendum 4 specifies.
2. **`Bind { target: State(v+1)@F, source: State(v)@F, op: Put { field,
   value: ValueRef::Var(temp) } }`** — a static-field state write. Renders
   (via `render_bind`, in branch context) byte-for-byte as `let
   StateAcc{v+1} = call 'maps':'put'('field', _Temp, StateAcc{v}) in `.
3. **`Bind { target: State(v+1)@F, source: State(v)@F, op:
   Direct(ValueRef::Doc(doc)) }`** — a state rebind from an opaque
   computed RHS (`element(2, _Tuple)`), rendering `let StateAcc{v+1} =
   <doc> in `.
4. **The `Gensym` two-hop** (the BT-3146 investigation's `_CfState8`
   chain, sanctioned here as the single idiom for "opaque nested-construct
   state extraction feeding a `maps:put`"):
   `Bind { target: Gensym(name)@F, source: State(v)@F, op:
   Direct(Doc(element-2-extract)) }` then `Bind { target: State(v+1)@F,
   source: Gensym(name)@F, op: Put { field, value: Var(result_temp) } }`.
   The Gensym var's `version` follows BT-3145's already-shipped production
   convention (`while_loops.rs:570`): a per-name rebind ordinal ≥ 1, so
   linearity checks apply to it (exactly one producer, at most one
   consumer) instead of the version-0 exemption.
5. **`Return(ValueRef, State(final)@F, span)`** — the conditional arm's
   closing `{Result, StateAcc{final}}` 2-tuple. This is precisely the
   "narrower use today" Addendum 4 preserved `Return` for; `render_return`
   already produces this shape byte-for-byte.

Three rules the compiled outputs force, stated once so no shape re-derives
them wrongly:

- **Rule 1 — a version may be consumed by at most one `Bind`'s `source`.**
  `verify()` counts consumers from `Bind::source` only
  (`collect_producer_consumer_counts`, `threaded_ir.rs:923`), and
  `NonLinearVersion` fires at `consumed > 1`. Consequence: a value-temp
  `let` (`let _Val4 = <rhs> in`) must be a `Statement`, **not** a
  `Gensym`-target `Bind` sourced from `State(v)` — modeling it as a Bind
  would double-source `State(v)` (once by the temp, once by the `Put`)
  and false-positive `NonLinearVersion` on every field assignment. The
  two-hop idiom (block 4) is reserved for the case where the extracted
  value **is** the next state map (the `Put`'s `source` is the Gensym, so
  `State(v)` is consumed exactly once, by hop 1).
- **Rule 2 — statement separators differ between the two files, and both
  are already implemented.** `generate_conditional_branch_inline` pushes
  statement docs adjacent with **no separator** (each open chain carries
  its own trailing `" in "`) — `render`'s top-level loop has exactly this
  property (Addendum 4's "no separator" note).
  `generate_exception_body_with_threading_inner` inserts a literal `" "`
  between statements (`exception_handling.rs:974-976`) — producing the
  `"in  let"` double space after every open chain — which is exactly
  `render_loop_body_statements`' behavior (Addendum 3's `BodyKind::Letrec`
  finding, reused verbatim; the helper is not loop-specific despite its
  name). A re-attempt renders conditional arms through the no-separator
  loop and exception bodies through the space-separated loop; inventing a
  third gluing scheme is how byte-identity dies.
- **Rule 3 — the arm wrapper is `Threaded { mode:
  StateAcc(StateAccFallbackReason::None), frame }`.**
  `verify_branch_frame_linearity` (`threaded_ir.rs:1975-1981`) already
  pinned this convention for synthesized arms; the real migration keeps
  it (one `Threaded` node per `with_branch_context` arm, fresh `FrameId`,
  `produces` = the arm's final `State` version when > 0). `StateAcc(None)`
  is also semantically honest: branch arms thread through the `StateAcc`
  map naming convention, and `Unpack` stays legal under `StateAcc` mode,
  which these arms never emit anyway.

### Dynamic-field puts — the one place `BindOp::Put` is not enough

`SelfFieldAtPut`/`SelfFieldAtPutControlFlow` write through a **runtime**
field name: `let StateAcc1 = call 'maps':'put'(_Name4, _Val5, StateAcc)` —
the map key is a *variable*, but `BindOp::Put::field` is a `String`
rendered through `leaf::atom` (`render_bind`, `threaded_ir.rs:1690`), which
can only produce `'atom'` literals. Three options:

1. **Model the whole `maps:put` as `BindOp::Direct(ValueRef::Doc(...))` —
   Recommended.** `Bind { target: State(v+1), source: State(v) (or
   Gensym(name) for the ControlFlow variant), op: Direct(Doc("call
   'maps':'put'(<name-var>, <val-var>, <source-name>)")) }`. Byte-identical
   with zero IR change; linearity/unbound checks (the only checks that have
   teeth for these arms — `ShadowWriteMissing` is ClassVars-only,
   unpack-mismatch is `Unpack`-only) apply in full, because they hang off
   `target`/`source`, not off `Put`-ness. The put's *structure* becomes
   opaque — an accepted §Verifier-honesty-class opacity: no current or
   planned check inspects `Put::field` at all, so nothing checkable is
   hidden. One wrinkle the lowering must respect: the `Doc` must embed the
   rendered source-state name, because `Direct` renders only `let <target>
   = <doc> in ` — the `source` field feeds verification, not rendering,
   for this op (already true of every `Direct(Doc(...))` rebind BT-3145
   ships).
2. **Widen `Put::field` to an enum (`Atom(String) | Var(String)`).**
   Buys rendering fidelity inside `Put`, at the cost of touching every
   existing `Put` constructor and `render_bind` arm for a distinction no
   verifier check consumes. Rejected for this migration — pure type-surface
   cost, zero verification payoff (Addendum 2 Gap 1 option 3's standard).
3. **A new `BindOp::PutDynamic` variant.** Same payoff/cost analysis as
   option 2 with more enum surface. Rejected.

### Shape-by-shape: conditionals.rs (`generate_conditional_branch_inline`)

Preliminaries that apply to every conditional call site, compiled and
confirmed (repro `s12`: `x := 1` then `flag ifTrue: [x := x + 1] ifFalse:
[x := x + 100]` in an Actor method):

```erlang
let _SeededState4 = call 'maps':'put'('__local__x', X, State) in
let _Cond3 = _flag1 in case _Cond3 of
  <'true'> when 'true' -> let StateAcc = _SeededState4 in
    let _Val5 = ( <x + 1> ) in
    let StateAcc1 = call 'maps':'put'('__local__x', _Val5, StateAcc) in
    {_Val5, StateAcc1}
  <'false'> when 'true' -> let StateAcc = _SeededState4 in
    let _Val8 = ( <x + 100> ) in
    let StateAcc1 = call 'maps':'put'('__local__x', _Val8, StateAcc) in
    {_Val8, StateAcc1}
end
```

- **Call-site mint order inverts emission order**: the receiver's own doc
  mints first (here: none), then `Cond` (**3**), then
  `seed_conditional_locals`' `SeededState` (**4**) — yet `_SeededState4`'s
  `let` is *emitted before* `_Cond3`'s. A lowering that mints in emission
  order shifts every later number. (`ifNotNil:` mints `Obj` instead of
  `Cond` and binds the block parameter to it via scope — no extra mint.)
- **Arms generate strictly in source order** (true arm's mints all precede
  the false arm's), each inside its own `with_branch_context`; both arms
  here legitimately produce `StateAcc1` — the sibling-arm same-version
  case `FrameId` exists for.
- The call-site skeleton (`case`/arm headers/`let StateAcc = <base> in`/
  the non-taken `{'nil', <base>}` arm) stays AST-directed `Document`
  construction in BT-3146 — the migration unit is the **arm body**, which
  becomes `Threaded { mode: StateAcc(None), frame, body: <the shapes
  below>, produces }` + `verify()` + `render` at each of the seven
  consumers.

Each shape below gives: the emitting arm, compiled evidence (repro name),
the pinned statement sequence, and mint order. `F` is the arm's fresh
`FrameId`; `State(v)@F` renders `StateAcc{v}` (branch context keeps
`in_loop_body = true`, `in_hybrid_loop = false`, so `resolve_prefix` is
faithful — the render must run inside the same branch context the
lowering runs in, or via `with_loop_context` with those flags).

**C1 — `FieldAssignment`** (via `generate_field_assignment_open`;
repro `s01`/`s02`). Compiled: `let _Val4 = ( <rhs> ) in let StateAcc1 =
call 'maps':'put'('n', _Val4, StateAcc) in `.

```text
Statement("let _Val4 = <rhs_doc> in ")                      // Rule 1: temp let is a Statement
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Put { field: "n", value: Var("_Val4") }, shadow_write: false }
```

Mint order: `Val` first, **then** the RHS doc's own mints
(`generate_field_assignment_value_doc`), then `next_state_var()` (version
bump, no gensym). `is_last` ⇒ the arm's `Return` value is
`Var("_Val4")`.

**C2 — `LocalAssignPure`/`LocalAssignControlFlow`/`LocalAssignSelfSend`,
plain sub-branch** (via `generate_local_var_assignment_in_loop`; repro
`s02`). Compiled: `let _Val4 = ( <rhs> ) in let StateAcc1 = call
'maps':'put'('__local__y', _Val4, StateAcc) in `. Identical decomposition
to C1 with field `"__local__y"` — plus two lowering-time side effects that
produce no text: `bind_var(name, _Val4)` (so later reads in the same arm
resolve to the temp — `s02`'s second statement compiles to `let _Val5 =
_Val4 in`), and REPL mode swapping the key to the bare name
(`control_flow/mod.rs:3378-3382`). The REPL sub-branch is a one-atom
difference in `Put::field`, computed by the same `is_repl_mode()` call at
the same lowering point — no separate compiled repro exists because no
offline REPL-compile path emits inspectable `.core` (descope note: pinned
by the `intrinsics.rs:1701` consumer's existing REPL-protocol e2e coverage
instead).

**C3 — `LocalAssignTier2` sub-branch** (Tier-2 RHS returning `{Result,
NewStateAcc}`; repro `s11`). Compiled:

```erlang
let _T28 = <tier2-call doc> in
let _Val7 = call 'erlang':'element'(1, _T28) in
let _T2St9 = call 'erlang':'element'(2, _T28) in
let StateAcc1 = call 'maps':'put'('__local__r', _Val7, _T2St9) in
```

**Mint order trap (the reason this addendum exists):** `Val` is minted
**first** (**7**), then `T2` (**8**), then `T2St` (**9**), then the
tier-2 doc's own mints (`_Fun10`, `_Arg11`) — the *emission* order
(`_T28`, `_Val7`, `_T2St9`) does not match the *mint* order
(`_Val7`, `_T28`, `_T2St9`). Source: `control_flow/mod.rs:3370,3390-3392`.
Decomposition — the sanctioned two-hop, with a `Statement` prefix:

```text
Statement("let _T28 = <t2_doc> in let _Val7 = call 'erlang':'element'(1, _T28) in ")
Bind { target: Gensym("_T2St9", 1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _T28)")) }
Bind { target: State(v+1)@F, source: Gensym("_T2St9", 1)@F,
       op: Put { field: "__local__r", value: Var("_Val7") } }
```

**C4 — `LocalAssign*` open-scope sub-branch (BT-1397,
`control_flow/mod.rs:3456-3486`) — no reachable, pinnable repro; descoped
with evidence.** The sub-branch handles a local assignment whose RHS is a
class-method self-send emitting an open `ClassVars` unwrap chain. This
pass could not produce a compiled repro that reaches it *through*
`generate_conditional_branch_inline`: the natural repro (`s15`, `Value
subclass` with `classState`, `class m: flag => x := 1. flag ifTrue: [x :=
self bump]. x`) routes through `value_type_codegen.rs`'s vt-conditional
path instead (`generate_vt_conditional_branch`, `value_type_codegen.rs:3089`
— specifically the `_CondResult` wrapper it builds,
`value_type_codegen.rs:2762`) — and
breaks **there**, emitting syntactically invalid Core Erlang: `let X =
<open ClassVars unwrap chain> in  in X` (empty value doc, doubled `in`;
erlc: "syntax error before: in") — a production bug in its own right,
filed as [BT-3159](https://linear.app/beamtalk/issue/BT-3159) (see
§Production bugs). Net: there is no valid byte-identity target to pin a
decomposition against, and no demonstrated route into this sub-branch
from either of the two body loops. Descope rule for BT-3146: leave the
sub-branch on the legacy path with a code comment citing this addendum;
if BT-3159's fix (or a future routing change) makes it reachable with
valid output, its decomposition is C2's plus a leading
`Statement(<open-chain preamble>)`, derived then against real output —
not pre-pinned now against output that cannot be compiled.

**C5 — `DestructureAssignment` (pure) — exempt from `Bind` modeling**
(answering the BT-3146 investigation's question 3 directly; repro `s06`).
Compiled: `let _Tup4 = <rhs> in let _SizeOk6 = case
'erlang':'tuple_size'(_Tup4) of ... in let A = element(1, _Tup4) in let
_b = element(2, _Tup4) in ` — **no state version is produced or
consumed**; every binding is a plain local. Decomposition: one
`Statement` carrying `generate_destructure_bindings`' docs verbatim.
Mint order: `Tup` (**4**), RHS doc mints, `SizeOk` (**6**),
`BadArity` inside the case (**7**); pattern names (`A`, `_b`) are
`to_core_erlang_var` conversions, never minted.

**C6 — `FieldAssignmentControlFlow`** (the investigation's `_CfState8`
shape, now confirmed *inside* a branch arm; repro `s03`,
`flag ifTrue: [self.x := (g ifTrue: [self.y := 1. 42] ifFalse: [0])]`).
Compiled (outer arm):

```erlang
let _CfTuple5 = ( <nested conditional, opaque> ) in
let _CfVal6 = call 'erlang':'element'(1, _CfTuple5) in
let _CfState11 = call 'erlang':'element'(2, _CfTuple5) in
let StateAcc1 = call 'maps':'put'('x', _CfVal6, _CfState11) in
```

Mint order: `CfTuple` (**5**), `CfVal` (**6**) **before** the RHS doc's
mints (**7–10**, the nested conditional's own `Cond`/`Val`/
`BranchResult` temps), then `CfState` (**11**) **after** the RHS doc,
then `next_state_var()`. Source: `conditionals.rs:427-431`.
Decomposition:

```text
Statement("let _CfTuple5 = <rhs_doc> in let _CfVal6 = call 'erlang':'element'(1, _CfTuple5) in ")
Bind { target: Gensym("_CfState11", 1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _CfTuple5)")) }
Bind { target: State(v+1)@F, source: Gensym("_CfState11", 1)@F,
       op: Put { field: "x", value: Var("_CfVal6") } }
Statement(<threaded __local__ re-read lets, if any — maps:get from the new state>)
```

The trailing `__local__` re-reads (`conditionals.rs:457-475`) read the
*new* state version but bind plain locals — `Statement`s by Rule 1. The
nested construct itself stays one opaque `Doc`; its own arms get their
own frames, `verify()`, and rendering at their own (also-migrated) call
site — recursion composes without any cross-frame bookkeeping. (Note the
nested call site's base-state binding emits the curious-but-real `let
StateAcc = StateAcc in` when the outer arm is still at version 0 —
call-site skeleton text, outside the arm-body migration unit.)

**C7 — `SelfFieldAtPut`** (via `generate_self_field_at_put_open`; repro
`s04`). Compiled: `let _Name4 = 'x' in let _Val5 = 42 in let StateAcc1 =
call 'maps':'put'(_Name4, _Val5, StateAcc) in `. Dynamic field ⇒
§Dynamic-field option 1:

```text
Statement("let _Name4 = <name_doc> in let _Val5 = <val_doc> in ")
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Direct(Doc("call 'maps':'put'(_Name4, _Val5, StateAcc{v})")) }
```

Mint order (`dispatch_codegen.rs:2484-2493`): `Name` via **`fresh_var`**
(same counter as `fresh_temp_var`, plus a scope bind), then `Val`, then
the *name* doc's mints, then the *value* doc's mints, then
`next_state_var()`. `is_last` ⇒ `Return` value `Var("_Val5")`.

**C8 — `SelfFieldAtPutControlFlow`** (repro `s05`). Compiled: `let _Name5
= 'x' in let _CfTuple6 = <nested> in let _CfVal7 = element(1, _CfTuple6)
in let _CfState12 = element(2, _CfTuple6) in let StateAcc1 = call
'maps':'put'(_Name5, _CfVal7, _CfState12) in `. Mint order differs from
C7 (`conditionals.rs:494-500`): `Name` (**5**), then the **name** doc's
mints, then `CfTuple` (**6**), `CfVal` (**7**), then the **value** doc's
mints (**8–11**), then `CfState` (**12**). Decomposition = C6's two-hop
with the final `Put` replaced by the dynamic-field `Direct(Doc(...))` of
C7 (hop 2's `source` is still the Gensym, so linearity is preserved):

```text
Statement("let _Name5 = <name_doc> in let _CfTuple6 = <val_doc> in let _CfVal7 = element(1, _CfTuple6) in ")
Bind { target: Gensym("_CfState12", 1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _CfTuple6)")) }
Bind { target: State(v+1)@F, source: Gensym("_CfState12", 1)@F,
       op: Direct(Doc("call 'maps':'put'(_Name5, _CfVal7, _CfState12)")) }
Statement(<threaded __local__ re-reads, if any>)
```

**C9 — `DestructureAssignmentControlFlow`** (repro `s07`). Compiled:
`let _CfTuple5 = <nested> in let _CfVal6 = element(1, _CfTuple5) in let
StateAcc1 = element(2, _CfTuple5) in <__local__ re-reads> <size-check +
pattern-bind lets> `. **No `CfState` hop** — the new state version binds
`element(2, ...)` directly (`conditionals.rs:561,572-575`):

```text
Statement("let _CfTuple5 = <rhs_doc> in let _CfVal6 = element(1, _CfTuple5) in ")
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _CfTuple5)")) }
Statement(<threaded __local__ re-reads, if any>)
Statement(<generate_destructure_bindings_from_var docs — SizeOk/BadArity/pattern lets>)
```

Mint order: `CfTuple` (**5**), `CfVal` (**6**), RHS doc mints (**7–12**),
`next_state_var()` (no mint), **then** the destructure-binding mints
(`SizeOk` **13**, `BadArity` **14**) — the size-check temps mint *after*
the state bump, unlike C5 where they follow the tuple temp directly.

**C10 — `ControlFlowWithMutations`, both positions** (repro `s08`).
Non-last position discards the result entirely — **no `element(1)` is
emitted at all**:

```erlang
let _Tuple5 = ( <nested> ) in
let StateAcc1 = call 'erlang':'element'(2, _Tuple5) in
<__local__ re-reads>
```

```text
Statement("let _Tuple5 = <expr_doc> in ")
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _Tuple5)")) }
Statement(<threaded __local__ re-reads, if any>)
```

Mint order: `Tuple` (**5**), expr doc mints (**6–7**),
`next_state_var()`. Last position adds a result extraction, and its
`BranchResult` mints **before** the expr doc (`conditionals.rs:607-610`):
`Tuple` (**14**), `BranchResult` (**15**), expr doc mints (**16–17**),
`next_state_var()`:

```text
Statement("let _Tuple14 = <expr_doc> in let _BranchResult15 = element(1, _Tuple14) in ")
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _Tuple14)")) }
// Return value: Var("_BranchResult15")
```

**C11 — `Tier2ValueCall`, both positions** (repro `s09`). Last position is
C10-last with `T2Tuple`/`BranchResult` naming and
`generate_tier2_value_call_doc` as the opaque doc (mint order `T2Tuple`
**10**, `BranchResult` **11**, tier-2 doc's `_Fun12`/`_Arg13` after).
Non-last position (`conditionals.rs:700-718`) mints `T2Tuple` (**20**),
`T2Discard` (**21**), then the tier-2 doc (**22–23**), and carries a
**byte-identity quirk**: the discard extraction embeds a literal newline —
`")\n in let "` — so the compiled text is

```erlang
let _T2Discard21 = call 'erlang':'element'(1, _T2Tuple20)
 in let StateAcc2 = call 'erlang':'element'(2, _T2Tuple20) in
```

(the Addendum-3-double-space class of finding, confirmed in the emitted
`.core`). The `Statement` carrying that fragment must reproduce the
newline verbatim. Decomposition mirrors C10 (non-last) with the discard
`let` inside the leading `Statement`, plus trailing
`get_inline_block_captured_mutations` re-read `Statement`s
(`maps:get('__local__<v>', <new state>)`) when the stored block mutates
captured locals.

**C12 — catch-all pure statements** (`EarlyReturn`, `SuperSend`,
`ErrorSend`, `Tier2SelfSend`, `DispatchingSelfSend`, `Pure`; repro `s10`).
Non-last: `Statement("let _seq5 = <expr_doc> in ")`; last:
`Statement("let _BranchResult7 = <expr_doc> in ")` with `Return` value
`Var("_BranchResult7")`. Exactly Addendum 4's secondary-value case —
`Statement` is their home, confirmed against compiled output
interleaving them with real `Bind`s in one arm.

**C13 — empty block and the arm closer.** An empty block short-circuits
(`conditionals.rs:356-361`) to a bare `Return(Literal("'nil'"),
State(0)@F)`, rendering `{'nil', StateAcc}`. Every non-empty arm closes
with `Return(<last_result or Literal("'nil'")>, State(final)@F)`
rendering `{<result>, StateAcc{final}}` — byte-identical via the existing
`render_return` (the preceding open chain's trailing `" in "` supplies
the separating space; `render_return` itself adds none).

### Shape-by-shape: exception_handling.rs (`generate_exception_body_with_threading_inner`)

Call-site header mint tables first — both headers mint **every**
scaffolding temp up front, *before* the `ex_class`/body docs, in strict
field order (compiled, repros `e01`/`e02`):

- **`generate_on_do_with_mutations`** (`exception_handling.rs:343-359`),
  14 mints in order: `ExClass`, `Type`, `Error`, `Stack`, `BuiltStack`,
  `ExObj`, `Match`, `StateAfterTry`, `NlrCheckTok`, `NlrCheckVal`,
  `NlrCheckState`, `NlrCheckTok`(2nd), `NlrCheckVal`(2nd), `OtherPair` —
  **then** the `ex_class` expression doc's own mints, then the try-body
  arm, then the handler arm. (In `e01` these are `_ExClass3` … `_OtherPair16`,
  ex-class doc **17–20**, try body **21**, handler **22**.) The handler
  parameter binds via `let E = _ExObj8 in` — `to_core_erlang_var`, no
  mint.
- **`generate_ensure_with_mutations`** (`:574-577`), 4 mints: `Type`,
  `Error`, `Stack`, `StateAfterTry`; then the try-body arm; **then**
  `TryResult` (`:603` — minted *after* the try body, *before* the
  success-path cleanup); then the success-cleanup arm; then the
  error-cleanup arm. The cleanup block is compiled **twice** — two
  separate `with_branch_context` arms with distinct frames, distinct
  mints, byte-different only in their temp numbers (`e02`: `_Val10` vs
  `_Val11`). The success path rebinds the bare name `StateAcc` from
  `element(2, _StateAfterTry6)` — a *name shadow*, not a version: in IR
  terms each arm's `State(0)@F` entry parameter is whatever the call-site
  skeleton bound `StateAcc` to; the skeleton stays AST-directed.

Body-loop shapes (separator: `Document::Str(" ")` between statements —
Rule 2's `render_loop_body_statements` semantics):

**E1 — field assignment** (repros `e01`, `e02`): same
`generate_field_assignment_open` open chain as C1, same
`Statement` + `Put`-`Bind` decomposition. Two byte-facts specific to this
file: the `" "` separator after an open chain produces `"in  let"`
(double space), and `is_last` field assignment sets the body's result to
the **literal `'nil'`** (`exception_handling.rs:979-990` — the helper's
own comment: the assigned value's var name isn't readily available at
that point, so it is not threaded out), so `e01`'s handler closes
`... in  {'nil', StateAcc1}` — the caller's ` {result, state} ` closer
with `state_acc_var_doc(final)`.

**E2 — actor self-send** (`is_actor_self_send` arm via
`generate_self_dispatch_open`; repro `e03`). Compiled: `let _SD23 = case
call 'bt@...':'safe_dispatch'('bump', [], StateAcc1) of <{'reply', ...}>
... end in let StateAcc2 = call 'erlang':'element'(2, _SD23) in ` — the
open helper both emits the dispatch `Statement` **and bumps the state
version itself**:

```text
Statement("let _SD23 = <safe_dispatch case doc> in ")
Bind { target: State(v+1)@F, source: State(v)@F,
       op: Direct(Doc("call 'erlang':'element'(2, _SD23)")) }
// is_last additionally:
Statement("let _ExResult31 = call 'erlang':'element'(1, _SD23) in ")
// Return value: Var("_ExResult31")
```

(`_ExResult` mints *after* the dispatch doc's many internal temps.) This
shape's compiled output currently **fails erlc's backend** — see
§Production bugs; the decomposition above is pinned against the emitted
`.core` text, which is well-formed Core Erlang at the text level.

**E3 — local var assignment** — same helper and decomposition as C2/C3
(all three sub-branches). One production bug specific to this file's use
of it: the body loop never pushes a scope around the block
(`generate_exception_body_with_threading_inner` has no
`push_scope`/`pop_scope`, unlike `generate_conditional_branch_inline`),
so the helper's `bind_var(name, _ValN)` **leaks** past the `try` — repro
`e04` compiles a method-level read of `t` after the `ensure:` to the
try-scoped `_Val6`, and erlc rejects the module (`unbound variable
'_Val6' in dispatch/4`). See §Production bugs; the in-arm decomposition
is unaffected (the leak manifests outside the arm).

**E4 — destructure assignment** (repro `e05`): identical to C5 —
`Statement`s only, exempt from `Bind` modeling.

**E5 — last expression, nested control flow with mutations** (repro
`e06`). Compiled: `let _Tuple8 = <nested> in let _ExResult9 = element(1,
_Tuple8) in let StateAcc1 = element(2, _Tuple8) in` — C10-last's shape
with `ExResult` naming and one mint-order subtlety
(`exception_handling.rs:1033-1053`): the target state name is taken via
**`peek_next_state_var()`** (no mint, no bump) *before* the expr doc is
built, and the version bump (`let _ = self.next_state_var()`) happens
*after* — mint order is `Tuple` (**8**), `ExResult` (**9**), expr doc
mints (**10–11**), bump. Same
`Statement`+`Direct(Doc(element-2))`-`Bind`+`Statement` decomposition as
C10-last; the lowering keeps peek-then-bump only as a code-ordering fact
(the IR just targets `State(v+1)@F`). Also currently erlc-rejected —
§Production bugs.

**E6 / E7 — last / non-last plain expressions** (repro `e07`): last is
`Statement("let _ExResultN = <expr_doc> in")` (both the
`has_direct_field_assignments` and plain sub-branches emit the same
shape, trailing `" in"` **without** a trailing space — the separator/
closer supplies it); non-last is `Statement("let _ = <expr_doc> in")` —
the one place production uses a bare `_` wildcard instead of a minted
`seq` temp (contrast C12). No mints beyond the expr doc's own except
`ExResult`.

### Branch-context version discipline — how BT-1550's asymmetry maps onto the decompositions

`with_branch_context` (`mod.rs:2392`, RAII `BranchContextGuard`) is the
frame boundary. The per-prefix policy (BT-3131, table in §The duplication
this ADR's IR would also close) maps onto the IR as follows:

- **`State`: reset to 0 on entry, restored on exit** ⇒ every arm's
  version sequence starts at `State(0)@F` (the `StateAcc` entry binding),
  and `VersionedVar`'s frame field makes sibling arms' identical version
  numbers distinct identities — this is exactly why the decompositions
  above can use bare `v`/`v+1` without any cross-arm renumbering, and why
  `verify_branch_frame_linearity`'s acceptance criterion (sibling arms
  reaching the same version must not trip `NonLinearVersion`) holds for
  real IR, not just synthesized chains. Rendering: `State@F` resolves to
  `StateAcc{N}` because the guard holds `in_loop_body = true` — so the
  migration renders arm IR *inside* the guard (or under
  `with_loop_context` with the same flags), never after exit.
- **`ClassVars`: restore-only (no reset), `class_var_mutated` sticky
  (BT-1550)** ⇒ irrelevant to these decompositions *today*, because no
  `ClassVars` `Bind` can occur inside these arms (§Scope: BT-3140
  rejection + runtime-dispatch routing, both compiled-and-confirmed). C4
  looked like the one path that might exercise it (a class-var-mutating
  self-send's `ClassVars{N}` advance, reached via a local-assign inside
  an arm) but turns out, on inspection, not to reach these arms at all —
  its natural repro routes through `value_type_codegen.rs`'s
  vt-conditional path instead (§Scope, §C4) — so today there is no
  candidate path at all, broken or otherwise. **If** a future construct
  (or a fix that changes C4's routing) does put a `ClassVars`-mutating
  self-send inside one of these arms, the natural IR model is a
  `ClassVars`-prefixed `Bind` in the *arm's* frame whose source version
  is the inherited outer version — restore-only means the arm inherits
  the outer `ClassVars{n}`, emits `ClassVars{n+1}`, and the outer scope
  resumes at its saved value afterward — which `check_use`'s frame-flow
  rule (ancestor-frame producers are visible) already accepts without
  further design work; this is recorded here so a future pass does not
  have to re-derive it, not because it is needed by anything in scope
  today.
- **`SelfVt`: restore-only (BT-3131's revision)** ⇒ out of scope here
  entirely — value-type conditionals never reach these body loops (§Scope,
  repro `s14`).

**FrameId allocation is the one missing production mechanism.** Today the
scalar check synthesizes positional frames (`FrameId::new(i + 1)`,
`control_flow/mod.rs:1341`). Real per-arm IR needs real frame identities:
recommend a monotonic frame counter on `CoreErlangGenerator` minted by
`enter_branch_context` (the guard already owns entry/exit), stored so the
arm's lowering reads `self.current_branch_frame()`. Nested arms then get
distinct frames automatically (the counter never repeats), and the
verify-time frame stack is built by the `Threaded` wrapper nodes, exactly
as `verify()` already implements. Alternative (pass a frame down every
call path) touches dozens of signatures for no additional guarantee —
rejected.

### Production bugs found while pinning (all compiled from minimal repros)

Byte-identity migration reproduces current output — **including buggy
output**. Three of the ~20 repros exposed real, previously-unfiled
production bugs; they are findings of this pass, not blockers introduced
by it, and each is filed as its own Linear issue rather than silently
papered over:

1. **Class-method conditional local-assign-from-self-send emits invalid
   Core Erlang** (found probing shape C4; repro `s15`;
   [BT-3159](https://linear.app/beamtalk/issue/BT-3159)): `let X =
   <unwrap chain> in  in X` — empty value doc, doubled `in`, hard erlc
   syntax error. The emitting path is `value_type_codegen.rs`'s
   vt-conditional local-rebind machinery (`generate_vt_conditional_branch`,
   `value_type_codegen.rs:3089` — specifically the `_CondResult` wrapper it
   builds, `value_type_codegen.rs:2762`), not either of this addendum's two
   body loops — but it means any `Value`/`Object` class method with
   `classState` doing `flag ifTrue: [x := self <classVarMutatingMethod>]`
   fails to build today, and C4 has no compilable byte-identity target.
2. **Exception-body local-assign scope leak** (E3, repro `e04`;
   [BT-3160](https://linear.app/beamtalk/issue/BT-3160)):
   `generate_exception_body_with_threading_inner` lacks the
   `push_scope`/`pop_scope` bracket `generate_conditional_branch_inline`
   has, so `bind_var(local, _ValN)` escapes the `try` body and a later
   method-level read of the local emits the out-of-scope temp — erlc:
   `unbound variable '_Val6' in dispatch/4`. This is precisely the
   `UnboundVersion`-adjacent class ADR 0111 exists for, caught today only
   by `core_lint`'s far-from-cause message.
3. **erlc backend `ambiguous_catch_try_state` on dispatch/nested-case
   inside `try`** (E2/E5, repros `e03`, `e06`;
   [BT-3161](https://linear.app/beamtalk/issue/BT-3161)): an actor
   self-send or a
   nested annotated conditional as the try body's last statement produces
   `.core` text that OTP's beam_validator rejects ("Internal consistency
   check failed … ambiguous_catch_try_state"). Two repro flavors, one
   filed issue.

Migration consequence, stated explicitly: shapes E2/E5 have well-formed
`.core` text to pin against (their decompositions above are safe to
implement byte-identically — the erlc failure is downstream and
orthogonal); shape C4 has no valid output to pin and stays legacy until
its fix; bug 2's fix (adding the scope bracket) changes **no bytes** of
any currently-*compiling* program (the leak only manifests in programs
that fail to build), so it can land before or alongside the migration
without perturbing the parity gate.

### Migration order — PR boundaries for the BT-3146 re-attempt

Precedent: BT-3147/BT-3148-task-3's flagless per-shape promotion (the
`render()`ed node IS the emission at that site, byte-identical by
construction, snapshot-corpus-gated) — not BT-3145's whole-construct
flag, because every shape here is a drop-in replacement for a hand-built
`docvec!` fragment inside an unchanged skeleton. Dual-run parity tests
per shape (legacy `docvec!` vs `render(lower(..))`) before each site
flips, per Addendum 4's discipline. The cumulative ≤3% gate
(§Measurement gate, restated) is re-measured at each PR.

1. **PR 1 — conditionals.rs, single-`Bind` shapes + statements:** C1, C2,
   C3, C5, C7, C12, C13 (and C4's descope note in code). Introduces the
   generator frame counter (§Branch-context) and the arm-level
   `Threaded`-wrapper + `verify()` call at the seven
   `generate_conditional_branch_inline` consumers, replacing nothing yet
   at the check sites (the scalar check runs in parallel during this PR).
   Depends on `ThreadedStmt::Statement` (Addendum 4 / BT-3148's
   re-attempt item 1); if that has not landed first, PR 1's first commit
   is exactly Addendum 4's items 1–2 lifted verbatim (coordinate via the
   epic to avoid double-landing).
2. **PR 2 — conditionals.rs, nested-construct shapes:** C6, C8, C9, C10,
   C11 — the `Gensym` two-hop and `Direct(Doc(element-2))` families,
   including the C11 newline quirk's dual-run byte test.
3. **PR 3 — exception_handling.rs:** E1–E7 through
   `render_loop_body_statements`-style separated rendering, the two
   call-site header mint tables pinned by dual-run tests (they are the
   densest mint-order surface in either file — 14 consecutive header
   mints in `on:do:`), and `ensure:`'s compile-twice cleanup arms.
4. **PR 4 — teeth:** flip the eight in-scope `check_branch_frame_linearity`
   call sites (§Scope and routing boundaries) to verify the real per-arm
   IR produced by PRs 1–3, delete `verify_branch_frame_linearity`'s
   scalar-synthesis path **for those eight**, and update
   `control_flow/mod.rs:1312-1327`'s "scaffolding, not yet a live
   regression guard" doc comment — including its "today's nine call
   sites" line, which becomes "eight of today's nine call sites" (the
   ninth, `expressions.rs:1125`, still routes through
   `verify_branch_frame_linearity`'s scalar synthesis, since
   `generate_block_stateful_body` is not one of this addendum's shapes) —
   the promise BT-3134 left on `main` is discharged here for the shapes
   in scope. `NonLinearVersion`/`UnboundVersion` become live regression
   guards for the eight migrated branch arms from this PR on;
   `expressions.rs:1125` keeps its pre-existing scaffolding-only behavior.

Descope alternative (Alternative 1b, evaluated per this ADR's practice):
stopping after PR 1 or 2 leaves exception bodies on scalar synthesis —
acceptable if the gate trips (each PR is independently shippable and
independently valuable), but not recommended a priori: unlike BT-3145's
loop-family measurement unknowns, every shape here reuses
already-measured `render` paths (`render_bind`, `Statement`'s zero-cost
push), so the marginal cost per PR is construction-only, the exact class
task 3 already judged inside budget.

### Acceptance criteria for the re-attempt, restated against this table

- Each PR's shapes emit through `render()` at their production sites,
  byte-identical over the expanded snapshot corpus + the per-shape
  dual-run tests; mint order per the tables above (the lowering mints at
  the same code positions legacy does — every ordering fact above is an
  ordinary code-ordering fact of one function, Gap-2-option-2 style).
- `verify()` runs once per arm over real IR; after PR 4, the scalar
  fixture path is gone for the eight in-scope call sites and they cannot
  drift from what they emit (`expressions.rs:1125` is not one of the
  eight — see §Scope and routing boundaries).
- C4 stays legacy with a code comment citing this addendum and its bug
  issue; E2/E5 migrate byte-identically despite their downstream erlc
  rejection (their fix is separate).
- `just verify-threaded-ir`, `class_var_shadow_contract.rs`, behavioral
  suites, and the ordered diagnostic-stream check green per this ADR's
  standing exit criteria.
## Addendum 6 (2026-08-12): BT-3148 re-attempt — tasks 1/2/4 landed, measurement result

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

## Addendum 7 (2026-08-13): BT-3149 close-out — Epic BT-3141 implementation tracking, final measurement, and full-pipeline re-evaluation

**Epic:** [BT-3141](https://linear.app/beamtalk/issue/BT-3141) — ThreadedIr as emission input, complete ADR 0111 single-sourcing
**Status:** Substantially Done — one known gap tracked as a dedicated follow-up (see below)

| Phase | Issue | Description | Size | PR |
|---|---|---|---|---|
| 1 | [BT-3142](https://linear.app/beamtalk/issue/BT-3142) | Amendment + gate: delivered-vs-designed addendum, renderer design, ≤3% build-time gate criteria | S | [#3339](https://github.com/jamesc/beamtalk/pull/3339) |
| 2 | [BT-3144](https://linear.app/beamtalk/issue/BT-3144) | Renderer foundation: full-fidelity `render(&ir, ctx) → Document`, dual-run parity harness | L | [#3341](https://github.com/jamesc/beamtalk/pull/3341) |
| 2 | [BT-3153](https://linear.app/beamtalk/issue/BT-3153) | Amend ADR 0111: `ConditionalLoop` node design + `Bind` naming reconciliation, unblocking BT-3145 | M | [#3343](https://github.com/jamesc/beamtalk/pull/3343) |
| 3 | [BT-3145](https://linear.app/beamtalk/issue/BT-3145) | Pilot migration: while_loops + counted_loops emit through ThreadedIr, gated on ≤3% measurement | L | [#3345](https://github.com/jamesc/beamtalk/pull/3345) |
| 4 | [BT-3147](https://linear.app/beamtalk/issue/BT-3147) | Migrate list_ops + dict_ops; derive gate slots independently | L | [#3351](https://github.com/jamesc/beamtalk/pull/3351) |
| 4 | [BT-3156](https://linear.app/beamtalk/issue/BT-3156) | Design `ThreadedStmt::Statement` (opaque AST-directed statement node), unblocking BT-3146 + BT-3148's remainder | L | [#3352](https://github.com/jamesc/beamtalk/pull/3352) |
| 4 | [BT-3148](https://linear.app/beamtalk/issue/BT-3148) | Migrate gen_server state threading, NLR boundaries, class-var Bind/shadow-write producers | L | [#3350](https://github.com/jamesc/beamtalk/pull/3350), [#3356](https://github.com/jamesc/beamtalk/pull/3356) |
| 4 | [BT-3146](https://linear.app/beamtalk/issue/BT-3146) | Migrate conditionals to emission-input ThreadedIr with real per-arm Bind chains | L | [#3359](https://github.com/jamesc/beamtalk/pull/3359) |
| 5 | [BT-3149](https://linear.app/beamtalk/issue/BT-3149) | Close-out: migrate `generate_block_stateful` (the last real caller BT-3146 didn't cover), dead-code allowances reduced 12→4, docs, this addendum | M | this PR |

**Known gap, filed as a dedicated follow-up rather than folded into this close-out**:
[BT-3165](https://linear.app/beamtalk/issue/BT-3165) — `exception_handling.rs`'s `on:do:`/`ensure:` mutation-threading generators. BT-3146's issue description named `exception_handling.rs` in its own scope alongside `conditionals.rs`, and its own module-doc comment (`threaded_ir.rs`, pre-BT-3149) said so explicitly — but the PR that shipped covered `conditionals.rs` only. BT-3149's task list required confirming "genuinely the last caller" by grep before deleting the branch-frame-linearity scaffolding; the grep found `exception_handling.rs`'s two call sites still live, contradicting that premise. Migrating them is comparable in size to BT-3146's own `conditionals.rs` slice (a full E1-E7 per-shape decomposition, not a small residual) — out of proportion for an "M"-sized close-out issue, so it is tracked separately rather than rushed. Until BT-3165 lands, `check_branch_frame_linearity` (`control_flow/mod.rs`) and `verify_branch_frame_linearity` (`threaded_ir.rs`) remain live scaffolding, not deletable residue.

**Dead-code allowances**: BT-3149 audited every `#[allow(dead_code)]` in `threaded_ir.rs` (twelve at the start of this issue) by removing each and letting the compiler's own `dead_code` lint say what's still true. Eight were stale — either the annotated item now has a genuine production constructor (most of the `VersionPrefix`/`ThreadingMode`/`ValueRef`/`BindOp`/`ThreadedStmt` enum-level blanket allowances, one construct family's migration at a time across BT-3145-3149) or the annotated item is only ever exercised by `#[cfg(test)]` code, for which `#[cfg(test)]` is the honest annotation, not `#[allow(dead_code)]` (`lower_and_render`, the three regression-pinning `verify_tuple_acc_*`/`verify_nested_list_op_stateacc_compat` functions, and the two `VerifyError` variants only those construct). Four remain, each narrowed from whole-type to one variant or one function, each with a doc comment naming exactly which not-yet-attempted migration would give it a production constructor: `LoopCounter::new` (counted loops), `ThreadingMode::Hybrid` (hybrid loops), `ValueRef::Version` (no current `Bind`/`Return` producer needs a second value-position version reference), `BindOp::Unpack` (the `StateAcc`-mode per-iteration unpack). None of the four can be `#[cfg(test)]`-gated instead — each variant's `render`/match arm is real, always-compiled production code (`render_bind`, `render_value`, `render_threaded`, the `LoopContextFlags` builder); only their lowering-side constructors are missing, and cfg-gating the variant would make those production match arms fail to compile in non-test builds. This is itself evidence for this addendum's full-pipeline note below: even "delete all `#[allow(dead_code)]`" has a floor set by which construct families have and haven't migrated, not a target zero reachable by cleanup alone.

**Method-level `verify()`**: fully realized for gen_server Actor method bodies — `lower_body_exprs_with_reply` + `verify_body_with_opaque_version_gaps` (BT-3148) already run `verify()` exactly once per method over the method's assembled real IR, the shape this close-out's task 1 named. For constructs nested inside expression position — conditionals' branch arms, loops, list-op unpacks, and (as of this issue) stateful-block bodies — `verify()` stays per-construct-invocation (one call per branch arm / loop / unpack / stateful block), not literally one call spanning the whole enclosing method. The two are not the same shape for a structural reason, not an oversight: gen_server method bodies get one call because BT-3148 made `lower_body_exprs_with_reply` build ONE `Vec<ThreadedStmt>` for the entire body, with every non-`Bind` statement embedded as an opaque `ThreadedStmt::Statement` (BT-3156's design). A conditional or loop reachable from a class-method body (the hand-written `generate_class_method_body` pipeline, `threaded_ir.rs`'s own module docs "Not attempted in this pass") has no equivalent whole-body `Vec<ThreadedStmt>` to embed into — it renders to a `Document` at its own construct boundary and returns that `Document` up an ordinary AST-walking call chain. Making its `verify()` call literally merge into one call for its enclosing method would require that construct to hand its real `Vec<ThreadedStmt>` fragment up instead of rendering locally — the same "opaque statement, embedded in a straight-line sequence" idea BT-3156 built for gen_server bodies, generalized to every expression-position call site across the whole compiler. That is meaningfully bigger than "close out the epic" — it's ADR-0018-§Alternative-3-scale (full-pipeline IR) territory, which ADR 0111 itself named as explicitly out of scope from the start (see this ADR's own §Scope), not a gap this epic left open by accident.

**Full-pipeline re-evaluation** (per this issue's task 7 — a data point for a future ADR, not a proposal): across BT-3145-3149, every migration that promoted a construct family to real emission-input `ThreadedIr` reused the SAME shape — the construct's existing codegen calls, unchanged, now populating `Vec<ThreadedStmt>` instead of `Vec<Document>` directly, with `verify()` + `render()` inserted at the construct's existing rendering boundary. That shape held cleanly for five different construct families (loops, conditionals, list-ops, gen_server bodies, stateful blocks) without needing to touch the boundary itself — i.e., ADR 0111's narrow scope (state-threading constructs specifically, not general expression codegen) was never a limiting factor for the migrations actually attempted. The one place the boundary itself became the obstacle was `ThreadedStmt::Statement` (BT-3156) — needed because gen_server method bodies mix real `Bind`s with ordinary AST-directed statements in one straight-line sequence, and nothing represented "here's an opaque statement, keep going." That gap was closed narrowly (one new node variant), not by extending IR scope. Nothing in this epic surfaced evidence that the general expression/dispatch/intrinsics/value-type/spec codegen ADR 0111 §Scope excludes needs IR coverage — every remaining boundary (class-method bodies, `exception_handling.rs`) is a "construct family not yet migrated," the same shape as the five that were, not a case where the construct-local rendering boundary itself is the blocker. This is a point *against* extending IR beyond ADR 0111's scope for now: the narrow-scope bet keeps paying off one construct family at a time, at negligible measured cost (see below), without needing the boundary redesign ADR 0018 §Alt 3 would require.

**Final measurement** (BT-3149's own incremental delta against `origin/main`, the same methodology Addenda 3/6 used — mean of 5 cold-`ebin` `beamtalk build-stdlib` runs per side, this session's shared/virtualized container):

| | wall-clock (s) | user CPU (s) |
|---|---|---|
| baseline (mean of 5, `origin/main`) | 12.48 (range 12.01–13.95) | 17.24 (range 16.91–17.45) |
| this issue (mean of 5, HEAD) | 12.81 (range 12.10–13.75) | 17.72 (range 17.38–18.19) |
| Δ | +2.6% | +2.75% |

Inside the ≤3% gate, but closer to it than BT-3148's +0.10% — expected, given this issue's actual codegen-shape change (`generate_block_stateful_body` migrating to real `Bind` construction, plus the shared `lower_field_assignment_bind` extraction conditionals.rs's C1 arm now also calls) touches two call sites rather than one linear method-body pass, on a stdlib corpus where stateful blocks are a smaller fraction of total codegen than gen_server method bodies. The baseline's and this issue's ranges nearly overlap (baseline max 17.45s vs. this issue's min 17.38s), consistent with Addenda 3/6's own finding that this shared/virtualized environment's noise floor is comparable in magnitude to small real deltas — not distinguishable from noise at n=5. **Gate cleared.**

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

## Addendum 8 (2026-08-13): BT-3165 — `exception_handling.rs` slice lands, closing the gap Addendum 5/7 tracked

BT-3165 implemented Addendum 5's E1–E7 table for `exception_handling.rs`'s
`generate_exception_body_with_threading_inner` (both `on:do:`'s try/handler
bodies and `ensure:`'s try/success-cleanup/error-cleanup bodies), the last
construct family this ADR's epic (BT-3141) had not yet migrated to real
`ThreadedIr` emission input. E1/E3 reused `conditionals.rs`'s
`lower_field_assignment_bind`/`lower_local_var_assignment_bind` directly, as
this addendum recommended; E2 required splitting
`dispatch_codegen.rs::generate_self_dispatch_open` into a call-only half
(`generate_self_dispatch_call_doc`) so the state-version bump it used to
bake into one opaque `Document` could become a real `Bind` instead. Both
`check_branch_frame_linearity` (`control_flow/mod.rs`) and
`verify_branch_frame_linearity` (`threaded_ir.rs`) — the scalar-synthesis
scaffolding these two call sites were the last production users of anywhere
in the codebase — are deleted.

**One correction to this addendum's Rule 2, found while implementing it.**
§"The decomposition vocabulary" describes gluing exception-body statements
by routing them "through `render_loop_body_statements`-style separated
rendering" (§Migration order, PR 3's description) and Rule 2 says exception
bodies render "through the space-separated loop" — i.e., literally through
[`render_loop_body_statements`]. That does not hold: `render_loop_body_statements`
inserts its literal `" "` separator between every RAW entry of the
`&[ThreadedStmt]` slice it is given, not once per *source-level* Beamtalk
statement. Every E1/E2/E3 shape decomposes into more than one raw
`ThreadedStmt` (at minimum a `Statement` + a `Bind`), so feeding the flat
per-shape sequence through it directly would inject a spurious extra space
*inside* each shape's own decomposition — e.g. between E1's value-temp
`Statement` and its `Put` `Bind` — that the legacy hand-rolled code never
had. Confirmed empirically: implementing it literally as specified would
have broken byte-identity. The fix implemented instead: the lowering pushes
the legacy separator as its own `ThreadedStmt::Statement(Document::Str("
"), span)` at each *source-statement* boundary (mirroring the pre-migration
`if i > 0 { docs.push(" ") }` loop exactly), and the whole arm renders
through plain [`render`] — the same no-separator function
`conditionals.rs` uses — so the manually-placed `Statement`s are the only
separators that end up in the output. `render_loop_body_statements` itself
is untouched, still used only by real `ConditionalLoop` bodies
(`while_loops.rs`), where every body statement is (today) representable as
exactly one `Bind` — the raw-entry-vs-source-statement distinction happens
to not yet matter there. Byte-identical over the full snapshot corpus +
`stdlib`/`BUnit`/REPL-protocol suites, confirmed before and after.

## Addendum 9 (2026-08-13): BT-3166 — ClassVars threading through loop/fold bodies: six design questions resolved against compiled repros

[BT-3155](https://linear.app/beamtalk/issue/BT-3155)'s epic goal — thread
`ClassVars` mutations through `whileTrue:`/`timesRepeat:`/`to:do:` loop
bodies (`BodyKind::Letrec`) and `do:`/`collect:`/`select:`/`inject:into:`
fold bodies (`BodyKind::Foldl*`), closing BT-3140/BT-3150/BT-3151's
compile-time rejections into correctly-compiling code — cannot start
implementation until six open design questions are pinned. This section
resolves all six, each against a real compiled repro (a locally built
`beamtalk` binary, `.core` read from the actual `build/` output), following
Addendum 2/4/5/8's own methodology: resolve by symbol name first, treat
`:NNN` line numbers as a hint. All line numbers below are against
`crates/beamtalk-core/src/codegen/core_erlang/` unless noted, current as of
`main` at commit `d4c2e57` — past the `73eeef1` floor this issue's branch
note required, confirmed before starting. No production code changed as
part of this addendum (design-only, per the issue's own "Files to Modify:
None").

**Repro naming.** `s01`–`s15` below are throwaway `.bt` fixtures compiled
directly with `beamtalk build <file>.bt`, read from the emitted
`build/bt@<file>.core`; they are not committed anywhere (scratch files,
deleted with the work session) — quoted here only for their compiled
output. One repro (`s01`'s equivalent trace) needed a temporary
`eprintln!` inserted into `ThreadingPlan::new_impl` to observe mode
selection *before* today's compile-time rejection fires (§Question 4);
the instrumentation was reverted immediately after capturing the two data
points it produced (`git diff` confirmed clean before continuing) — no
trace of it remains in this branch's history.

### Baseline, confirmed empirically: `generate_threaded_loop_body_inner` is unmigrated, and a narrower, disabled-by-default pilot sits beside it

The issue's own framing — "zero `ThreadedStmt`/`threaded_ir::` construction
anywhere in [`generate_threaded_loop_body`/`_inner`]" — is confirmed exactly:

```
$ awk 'NR==1351,NR==2067' control_flow/mod.rs | grep -c "ThreadedStmt\|threaded_ir::"
0
```

(`generate_threaded_loop_body_inner` spans `mod.rs:1351`–`2067`, ending at
the next function boundary, `emit_field_assign_last_expr` at `:2068`.) Every
statement shape inside it — field assignment, actor self-send, class-method
self-send (rejected), Tier-2 value call, local-var assignment, destructure —
still builds a hand-rolled `Document` directly, exactly as BT-3141's
research found for the rest of the pre-migration codebase.

One nuance the issue's framing doesn't mention, found while establishing
the baseline: `while_loops.rs` already carries a **narrower, parallel,
disabled-by-default** `ThreadedIr` pilot — `try_render_while_direct_via_threaded_ir`
(`while_loops.rs:604`+), gated by `BEAMTALK_THREADED_IR_WHILE_DIRECT=1`
(`:320-321`, off unless explicitly set — confirmed not read anywhere in
`beamtalk build`'s normal path). It lowers a real `ThreadedStmt::ConditionalLoop`
for the single narrowest case — `whileTrue:`/`whileFalse:`, `DirectParams`
mode only, no field writes, no self-sends — and is a genuinely separate code
path from `generate_threaded_loop_body`/`_inner` (it builds its own
`ir_body: Vec<ThreadedStmt>` by walking `filtered_body` directly). It does
not contradict the issue's claim — `generate_threaded_loop_body_inner`
itself is still 100% legacy, confirmed above — but it means BT-3155's
migration issues are not literally starting from a blank page for
`ConditionalLoop`'s wire-up mechanics (verify/render call sites, `RenderCtx`
plumbing) — only for the `StateAcc`/`Hybrid`/`Foldl*` shapes this design
targets, which the pilot deliberately does not cover.

### Question 1 — Frame semantics for `ShadowWriteMissing`

**Evidence.** `ShadowWriteMissing`'s gate (`walk_stmt`'s `Bind` arm,
`threaded_ir.rs:1276-1280`) is:

```rust
if matches!(target.prefix, VersionPrefix::ClassVars)
    && target.frame == FrameId::ROOT
    && !*shadow_write
    && self.has_class_vars_nlr
{ /* ShadowWriteMissing */ }
```

`with_branch_context`/`enter_branch_context` (`mod.rs:2373-2403`) mints a
**fresh, non-ROOT `FrameId`** on every entry, unconditionally:

```rust
self.set_state_version(0);
self.in_loop_body = true;
self.branch_frame_counter += 1;   // mod.rs:2384 — never reset, globally unique
```

— and `generate_threaded_loop_body` (`mod.rs:1339-1346`) calls
`with_branch_context` to run `generate_threaded_loop_body_inner`. So a
loop-body class-var `Bind`'s real `FrameId` (`current_branch_frame()`,
`mod.rs:2366-2368`) is **always** non-`ROOT` — `ShadowWriteMissing` would
never fire for one, exactly as the issue states.

Independently, ADR 0110's real production shadow-write gate is
`self.block_depth == 0` (`expressions.rs:703,711` — the two arguments to
`construct_and_verify_class_var_bind`, "independently re-derived per ADR
0111 §Verifier honesty"), never `FrameId`. `block_depth`
(`mod.rs:1615`) is incremented/decremented in exactly two places,
`expressions.rs:992/1012` and `:1063/1132` — both inside `generate_block`,
the compiler for a **first-class block literal closure** (`select:`/`do:`
arguments, stored blocks, `ifTrue:`'s dynamically-dispatched arm). Grepping
the full body of `generate_threaded_loop_body_inner` (`mod.rs:1351-2067`)
for `block_depth` returns zero matches — a `whileTrue:`/`do:` loop body
compiled through it never touches the counter at all. This is the concrete
mechanism behind BT-3140's amendment ("loop bodies never increment
`block_depth`"): a loop body is control flow, not a lexical closure
boundary, so `block_depth` correctly stays at whatever it was on entry
(0, at a class method's own top level) — while `FrameId` is minted fresh
regardless, because `FrameId`'s job (version-linearity scoping across
sibling branch arms/loop iterations) is orthogonal to `block_depth`'s job
(shadow-write eligibility: "is a foreign NLR still guaranteed to observe
this mutation via the process-dictionary side channel").

**Decision: a parallel `shadow_write_eligible: bool` field on
`ThreadedStmt::Threaded` and `ThreadedStmt::ConditionalLoop`** — the
issue's own second option, not a new `FrameId`-adjacent classification
type. Concretely:

- At lowering time, whenever a `Threaded`/`ConditionalLoop` node is
  constructed, the caller reads `self.block_depth == 0` fresh from live
  generator state (the same independently-re-derived pattern
  `expressions.rs:711` already establishes) and stores it as the node's
  `shadow_write_eligible`. This is correct by construction for every
  current and future call site: `block_depth` is a single, monotonically
  correct scalar counter across the *whole* nesting stack (not per-frame),
  so reading it once, at construction time, already reflects every
  enclosing block-literal boundary crossed so far — no AND-combination
  with a parent node's own flag is needed at construction time.
- `verify()` gains a `shadow_write_eligible_stack: Vec<bool>`, seeded
  `[true]` (mirroring `frame_stack: vec![FrameId::ROOT]` at
  `threaded_ir.rs:1146`), pushed/popped in lockstep with `frame_stack`/
  `mode_stack` in the `Threaded | ConditionalLoop` arm of `walk_stmt`
  (`threaded_ir.rs:1287-1316`) — `AND`ed with the parent's current top
  (`push(*shadow_write_eligible_stack.last().unwrap() && node.shadow_write_eligible)`)
  for defense-in-depth on hand-built fixtures, even though correct lowering
  never needs the AND (a nested frame's own `block_depth`-derived flag
  already encodes total nesting depth).
- `ShadowWriteMissing`'s gate changes from `target.frame == FrameId::ROOT`
  to `*shadow_write_eligible_stack.last().unwrap()`. `FrameId` keeps its
  existing, unrelated job (version linearity) untouched.
- Scope check, confirmed via Addendum 5: conditionals/`exception_handling.rs`
  do **not** need this field — Addendum 5's "Class-var mutations never route
  here either — by construction" finding (§"Scope and routing boundaries")
  still holds today (`reject_class_var_field_assignment`,
  `dispatch_codegen.rs:2398`, fires before mode selection for *any*
  threaded body, conditionals included) — so `shadow_write_eligible` is
  needed only on the two loop/fold-shaped variants this epic touches; giving
  it to `Threaded` too (not only `ConditionalLoop`) is forward-looking for
  when conditionals/exception-handling eventually gain the same capability,
  not something this epic's own repros require today.

### Question 2 — `construct_and_verify_class_var_bind`'s frame model

**Evidence.** The function (`threaded_ir.rs:2252-2329`) takes
`at_method_top_frame: bool` and hardcodes:

```rust
let frame = if at_method_top_frame { FrameId::ROOT } else { FrameId::new(1) };
```

Both of today's two production call sites confirm this is fixture-and-discard,
never a real per-call-site frame: `expressions.rs:704` (the top-frame class-var
field-write branch of `generate_field_assignment`) always passes
`at_method_top_frame: true` in practice (it's the top-frame branch by
construction); `dispatch_codegen.rs:522` (`emit_class_var_result_unwrap`,
the self-dispatch `ClassVarsN` rebind) always passes `false`/`false`
unconditionally, per its own doc comment (`threaded_ir.rs:2247-2251`):
"not a claim about its real block-depth... exempted" — that call site can
never need `ShadowWriteMissing` (it never sets `shadow_write: true`) or a
real frame identity, so `FrameId::new(1)`'s sentinel value has never mattered.

**Decision: replace `at_method_top_frame: bool` with a real `frame:
FrameId` parameter**, plus a separate `shadow_write_eligible: bool`
parameter (Question 1's flag — now genuinely independent of `frame`, so it
can no longer be derived from `frame == FrameId::ROOT` inside this
function either). Every future call site already has the real value at
hand: the two existing call sites use `FrameId::ROOT` (top-frame writes,
unchanged) and an unconditionally-exempt frame (the self-dispatch rebind,
where `frame` can now honestly be `FrameId::ROOT` too, since the call site
never claims a real nested identity — the old `FrameId::new(1)` sentinel
was never inspected for any purpose other than "not ROOT," and dropping it
removes a small, previously load-bearing-by-convention magic number); a new
loop-body class-var `Bind` call site passes `self.current_branch_frame()`
(`mod.rs:2366-2368`), the loop's real, already-minted frame.

**Correction to the function's internal fixture-building branch
(`threaded_ir.rs:2312-2327`).** This branch cannot drop its existing
`frame == FrameId::ROOT` trigger — that trigger is load-bearing for a
reason unrelated to `shadow_write_eligible`: `verify()`'s `frame_stack` is
seeded `[FrameId::ROOT]` and is only ever pushed by a `Threaded`/
`ConditionalLoop` wrapper (`threaded_ir.rs:1308`), and `check_use`
(`threaded_ir.rs:1235-1247`) requires `frame_stack.contains(&var.frame)`
for any `version > 0` use. A `Bind` verified bare (no wrapper) while its
`target.frame` is non-`ROOT` — exactly what the new loop-body call site
above supplies via `self.current_branch_frame()` — would spuriously fail
`check_use` with `UnboundVersion` the moment the same class var is
mutated a second time in one iteration (`source_version > 0`), i.e. an
ordinary loop body like `[ myVar := myVar + 1. myVar := myVar * 2 ]
timesRepeat: 3` would crash the compiler (`debug_assert!` in debug
builds, a hard "internal:" diagnostic otherwise, per
`report_threaded_ir_verify_errors`) once this design is implemented as
written. This is a distinct failure mode from Question 1's
`ShadowWriteMissing` false-positive, driven by a separate need
(version-linearity frame tracking, not shadow-write eligibility), so the
fix is to **add** the `shadow_write_eligible` trigger alongside the
existing `frame` trigger, not replace one with the other: wrap whenever
`frame != FrameId::ROOT || !shadow_write_eligible` (bare only when *both*
`frame == FrameId::ROOT` and `shadow_write_eligible == true`). This still
resolves the self-dispatch rebind's false positive (its
`shadow_write_eligible` is `false`, so it wraps regardless of its now-ROOT
frame) without breaking the loop-body call site (its `frame != ROOT`, so
it always wraps, regardless of `shadow_write_eligible`) or the real
top-frame class-var write at `expressions.rs:704` (`frame == ROOT` *and*
`shadow_write_eligible == true`, so it stays on the bare path, unchanged).
Only the call site's *supplied* frame identity changes from a
boolean-selected sentinel to the caller's real value — the wrap decision
itself gains a second, independent trigger alongside the original one.

### Question 3 — How `ClassVars` threads through the loop's own recursive tail call

**Evidence — three real fun signatures, one per today's local-threading
convention, none carrying a `ClassVars` slot:**

- **`DirectParams`** (repro `s06`, `whileTrue:`, local-only mutation):
  `letrec 'while'/2 = fun (I, Sum) -> ...` — `mod.rs`'s
  `DirectParams` doc comment ("`fun (Var1, ..., VarN)` — no `StateAcc` map
  at all," `threaded_ir.rs:677-678`) confirmed exactly; no map anywhere in
  the signature.
- **Counted-loop `LoopCounter`** (repro `s07`, `to:do:`): `letrec 'loop'/2
  = fun (_loopidx4, Sum) -> ... apply 'loop'/2 (call 'erlang':'+'(_loopidx4,
  1), _Sum7) ...` — confirms `LoopCounter`'s doc comment precedent
  (`threaded_ir.rs:567-579`): an extra, unversioned, raw-next-value-expression
  parameter, never a `Bind` target/source.
- **`StateAcc` fallback** (repro `s08`, Actor instance `whileTrue:` with a
  self-send): `letrec 'while'/1 = fun (StateAcc) -> ...` — exactly one map
  parameter.
- **`TupleAcc` (Foldl, gate_slots=1)** (repro `s10`, `select:`, local-only):
  `fun (X, _AccSt5) -> let AccList = element(1, _AccSt5) in let StateAcc =
  element(2, _AccSt5) in ...` — a 2-tuple wrapping the early-exit gate slot
  and the (still map-shaped) locals accumulator; **`TupleAcc(0)`** (repro
  `s12`, plain `do:`) degenerates to a *bare* `StateAcc` — `fun (X, StateAcc)
  -> ...`, no tuple wrapper at all (correcting `ThreadingMode::TupleAcc`'s
  own doc comment, `threaded_ir.rs:679-698`, which describes gate_slots=0
  as `{Var1,...,VarN}` — the real, compiled shape at gate_slots=0 is a bare
  map, not a flat per-local tuple; locals are *never* flattened positionally
  into the tuple in any of `s10`/`s12`'s output, only the early-exit gate
  value(s) are — see Question 6 for why this matters).

None of the four shapes has room for `ClassVars`: `DirectParams`/`Hybrid`
have no map to piggyback a key on (Question 3's own framing, confirmed); the
`StateAcc` map's other keys are all scalars (a nested `ClassVars` map under
one key would work syntactically but is semantically wrong — see below);
`TupleAcc`'s tuple wraps only the early-exit gate + the locals map, not a
third independent value.

**Decision: `ClassVars`, when a loop/fold body threads it, is always an
extra, explicit trailing fun parameter — parallel to, but distinct in kind
from, `LoopCounter`** — present *regardless* of which of the three
local-threading conventions is chosen for the loop's own locals:

- `DirectParams`/`Hybrid`: `fun (Var1, ..., VarN, ClassVars)`.
- `StateAcc`: `fun (StateAcc, ClassVars)` — two explicit map parameters,
  never one key folded into the other. This directly answers the "fold
  into StateAcc under a reserved key" question the issue poses and rejects
  it: every other `StateAcc` key holds a scalar (`Question 3`'s own
  observation); a `ClassVars` key would hold a *second, independently
  keyed and independently shadow-written* map with entirely different
  mutation semantics (ADR 0110's `class_tag`-keyed process-dictionary
  shadow write has nothing to do with the loop's own local-var `StateAcc`
  map) — conflating the two into one map buys nothing and risks a key
  collision if a Beamtalk field is ever literally named the reserved key.
- **Unlike `LoopCounter`, `ClassVars` needs real `Bind` linearity**:
  `LoopCounter` is unversioned (a single gensym'd identity, threaded by a
  raw next-value expression with no producer/consumer bookkeeping,
  `threaded_ir.rs:567-579`) because a loop index has no meaningful "was
  this mutated correctly" invariant beyond arithmetic. A class-var mutation
  is exactly what `ShadowWriteMissing`/version linearity exist to check —
  so `ClassVars` inside a loop body is a real `VersionedVar`
  (`VersionPrefix::ClassVars`, real per-iteration versions), not an
  `AccParam`/`LoopCounter`-shaped unversioned identity, structurally closer
  to `StateAcc`'s own threaded-local `Bind` chain than to the loop index.

**Foldl's structural constraint (forces a different mechanism from
Letrec's).** `lists:foldl(Fun, Acc0, List)`'s callback is fixed at 2
arguments (`fun(Elem, Acc) -> NewAcc`) — there is no third "extra fun
parameter" slot the way a `letrec` fun can just grow its parameter list.
`ClassVars` therefore cannot be a sibling parameter for Foldl the way it is
for Letrec; it must be folded into the accumulator value ITSELF, tuple-packed
alongside `StateAcc` — this is Question 6's subject, resolved there in full,
composing with this question's "`ClassVars` is always adjacent to the
locals map, never inside it" answer: for Foldl, "adjacent" means the tuple
position immediately before `StateAcc`, not a map key inside it.

### Question 4 — Mode-selection interaction with `has_self_sends`/Hybrid pre-extraction

**Part A — `has_self_sends` always forces `StateAcc`, confirmed both by
source and by a compiled repro.** `body_analysis.has_state_effects()`
(`semantic_analysis/block_facts.rs:60-62`) is `!field_writes.is_empty() ||
has_self_sends || has_field_value_call` — consumed by `select_direct_params`
(`mod.rs:513-527`, `!body_analysis.has_state_effects()`) and
`select_tuple_acc` (`mod.rs:535-553`, same check) — both unconditionally
excluded whenever `has_self_sends`. `select_hybrid_params`
(`mod.rs:560-582`) excludes it explicitly: `!body_analysis.has_self_sends`
(`:576`). Compiled confirmation (repro `s08`, an Actor instance self-send
inside `whileTrue:`): `letrec 'while'/1 = fun (StateAcc) -> ...` —
single-param `StateAcc` mode, exactly as source analysis predicts. **This
simplifies the epic's scope directly**: BT-3150's self-send-in-`Letrec`
repro and BT-3151's self-send-in-`Foldl` repro both *always* land in
`StateAcc`/plain-map-fold mode today — `DirectParams`/`Hybrid`/`TupleAcc`
never need to reason about a self-send at all, for either Letrec or Foldl.

**Part B — Hybrid's pre-extraction is unconditionally wrong for class-var
writes, confirmed empirically, and the fix is broader than the issue's own
framing suggests.** `select_hybrid_params`'s gate is
`!body_analysis.field_writes.is_empty()` (`:575`) — and `field_writes` comes
from `is_field_assignment` (`dispatch_codegen.rs:2121-2130`), a purely
syntactic `self.field := value` check with **no class-var awareness**
(`is_class_var_assignment`, the class-var-aware predicate, is a *separate*
function, consulted only later, inside `generate_field_assignment_open`'s
`reject_class_var_field_assignment` call, `dispatch_codegen.rs:2398` — after
mode selection has already run). `hybrid_mutated_fields`
(`while_loops.rs:967-969`) is populated straight from `plan.mutated_fields`,
itself `body_analysis.field_writes` verbatim (`mod.rs:481-487`) — so mode
selection cannot distinguish a class-var write from an instance-field write
at all.

Confirmed with a real (pre-rejection) trace: a temporary `eprintln!` after
`ThreadingPlan::new_impl`'s mode-selection block showed, for repro `s01`
(`Value subclass:`, a `whileTrue:` body writing only a class var, no
self-send): `hybrid=false` — but for repro `s11` (the **identical** loop
body, on an `Actor subclass:`'s class method instead of a `Value
subclass:`'s): `hybrid=true, field_writes={"runs"}`. The difference is
`select_hybrid_params`'s `matches!(context, CodeGenContext::Actor)` gate
(`:571`) — and a class method's `context` is `CodeGenContext::Actor` when
the class itself is an `Actor` subclass, `ValueType` when it's a `Value`
subclass (confirmed by the two traces; `CodeGenContext` has only `Actor`/
`ValueType`/`Repl` variants, `mod.rs:1013-1034` — there is no distinct
"class-method" context). **This means Hybrid mode is, today, latently
reachable for an `Actor` subclass's class-method loop body that mutates
only a class var — exactly the wrong-pre-extraction shape the issue
describes — and it has simply never manifested as a bug because
`reject_class_var_field_assignment` (`dispatch_codegen.rs:2398`) fires
downstream, inside body generation, regardless of which mode was already
selected.**

A further structural fact narrows the fix: a class method has no instance
`self.field` at all — `self.x` inside a `class` method resolves to a class
var or nothing (DNU), never an instance field — so `field_writes` inside
**any** `in_class_method()` loop body is, by construction, 100% class-var
names. There is no legitimate "mix of real instance-field pre-extraction
candidates plus an incidental class-var write" case for Hybrid to partially
exclude; Hybrid's entire premise (amortize the `State` map's per-iteration
`maps:get`/`maps:put` cost by pre-extracting mutated fields as direct fun
params) has no `State` to amortize against inside a class method at all.

**Decision: add `!generator.in_class_method()` to `select_hybrid_params`'s
existing guard clause**, excluding Hybrid mode entirely for any
class-method loop/fold body — not merely "exclude class-var field names
from `hybrid_mutated_fields`." Once this lands, combined with Part A's
finding, **every** `ClassVars`-mutating shape this epic cares about (a
direct field write, repro `s01`/`s11`; a self-send, repro `s02`/`s08`) is
excluded from `DirectParams` (field-write-excluded already, `has_state_effects()`),
`TupleAcc` (same check, `select_tuple_acc:548`), *and* now `Hybrid` — always
falling to `StateAcc`/plain-map-fold mode for the loop's own local-variable
threading. The two migration issues therefore only need to design/implement
`ClassVars` composition against `ThreadingMode::StateAcc`'s single-map
`fun(StateAcc)` (Letrec) and the plain, gate_slots=0 map-fold `fun(X,
StateAcc)` (Foldl) shapes — `DirectParams`/`Hybrid`/`TupleAcc(>0)`
composition with `ClassVars` is out of scope for this epic's three concrete
repros and is not designed here (a real, but not currently reachable,
future generalization — see Question 6's TupleAcc note).

### Question 5 — Is `ClassSelf` visible inside a loop's `fun` body without being an explicit parameter?

**Confirmed empirically, decisively, via repro `s09`** (`do:` with a
co-occurring local accumulator and a class-method self-send — the exact
BT-3151-addendum "silent loss" shape, which compiles today without
rejection because the `Letrec`-only guard, `mod.rs:1415`, doesn't cover
`Foldl*`). The class method itself is `fun (ClassSelf, ClassVars,
_aList2) -> ...`; its fold's inner closure is:

```erlang
let _temp7 = fun (X, StateAcc) ->
  let Total = call 'maps':'get'('__local__total', StateAcc) in
  let _CMR8 = call 'bt@...':'class_bump'(ClassSelf, ClassVars) in
  let ClassVars1 = case _CMR8 of
    <{'class_var_result', _MR10, _CV9}> when 'true' -> _CV9
    <_PCV11> when 'true' -> ClassVars
  end in
  ...
```

`_temp7`'s own parameter list is `(X, StateAcc)` — **no `ClassSelf`, no
`ClassVars`** — yet its body references both directly, closing over them
from the enclosing `class_countedRun:`/3 fun's own scope. This is ordinary
Core Erlang closure semantics (a nested `fun` closes over its lexical
environment, exactly like `letrec`-bound funs do too — `s06`/`s07`/`s08`'s
compiled `letrec 'while'`/`'loop'` bodies already reference `_n1`/`_n3`
(the enclosing method's own parameters) freely, without them being
declared loop parameters, confirming the SAME mechanism applies at
`letrec` nesting depth, not only plain `fun` nesting depth). **Answer: yes
— reading `ClassSelf` (for the shadow write's `element(2, ClassSelf)` key)
and the *original* `ClassVars` value needs zero extra plumbing inside
either a `letrec` loop body or a `Foldl` closure; both are already free
variables at that nesting depth.**

What is *not* free — and is exactly why Question 3's extra-parameter design
is necessary — is threading an *updated* `ClassVars` value forward across
iterations and back out to the caller after the construct completes. Core
Erlang funs are immutable/single-assignment: `ClassVars1`, computed inside
one iteration's closure body, is invisible to the next iteration and to the
method's own final `{'class_var_result', ..., ClassVars}` return unless it
is explicitly re-passed as a parameter (Letrec) or folded into the
accumulator (Foldl) — which is precisely why `s09`'s `ClassVars1` is a dead
local today: `_temp7`'s recursive-via-`foldl` invocation only ever carries
`StateAcc` forward, so `class_bump`'s mutation is computed, verified,
shadow-written (via `construct_and_verify_class_var_bind`, already correct
in isolation) — and then discarded, never reaching the fold's own output.
The shadow write itself (the process-dictionary side channel) already
fires correctly per-iteration today, for free, because it only needs the
free `ClassSelf`/`ClassVars` read — it is the *pure-functional* return path
that is missing, confirming this is a state-threading-plumbing gap, not a
`Bind`-construction gap.

### Question 6 — Letrec vs. Foldl accumulator shape

**Letrec never returns a value** (confirmed: `s06`'s exit arm is `<'false'>
when 'true' -> let _ExitSA11 = ... in {'nil', _ExitSA13} end`; `s08`'s is
`<'false'> when 'true' -> {'nil', StateAcc} end` — always `{'nil', ...}`
regardless of body shape). `ConditionalLoop`'s existing `exit_arm` field
(opaque `Document`, built by `generate_exit_stateacc`) already owns this
shape; a `ClassVars` extra parameter (Question 3) simply becomes one more
argument the exit arm's repack and the recursive tail call both carry — no
new accumulator-shape design needed for Letrec beyond "add one more
parameter, consistently, everywhere the existing `Var1..VarN`/`StateAcc`
parameters are threaded."

**Foldl's accumulator IS the body's output**, and — corrected against
compiled evidence, not `ThreadingMode::TupleAcc`'s doc comment (Question
3's finding) — locals are *never* positionally flattened into the tuple;
only the early-exit gate slot(s) are tuple-wrapped ahead of a trailing,
still-map-shaped `StateAcc`. Since `lists:foldl`'s callback has exactly one
accumulator parameter (no room for a sibling `ClassVars` parameter the way
Letrec's `letrec` fun can simply grow), `ClassVars` must ride inside that
one accumulator value, as one more tuple position — but per Question 4's
Part B finding, no compiled repro today ever reaches `TupleAcc(>0)` with a
class-var write in play (it's excluded by the same `has_state_effects()`
check `DirectParams` uses), so this design only needs to specify, and only
needs to be *implemented* for, the plain (`gate_slots=0`) map-fold shape:

- **`ClassVars` absent** (today, `s09`/`s12`): accumulator is bare
  `StateAcc`; `fun (X, StateAcc) -> ... StateAcc1 end`.
- **`ClassVars` present** (the fix): accumulator becomes a 2-tuple `{ClassVars,
  StateAcc}`, unconditionally, whenever the body threads `ClassVars` — `fun
  (X, {ClassVars, StateAcc}) -> ... {ClassVars1, StateAcc1} end` (rendered,
  per Addendum 5's established two-hop `element(1,_)`/`element(2,_)` idiom
  already used for `TupleAcc`'s own gate/`StateAcc` split, `s10`'s compiled
  shape). `emit_class_var_result_unwrap`'s already-correct, already-verified
  `Bind` (`dispatch_codegen.rs:466-521`) needs no change to how it
  *constructs* `ClassVars1` — only to where that value goes: into the new
  tuple slot instead of a dead `let`.
- **General composition (specified, not implemented in this pass):** for a
  future body that reaches `TupleAcc(G)` (`G>0`) *and* threads `ClassVars`
  — not reachable by any of BT-3155's three repros today, since
  `select_tuple_acc` excludes class-var/self-send bodies exactly like
  `select_direct_params` does — the accumulator generalizes to `{Gate1, ...,
  GateG, ClassVars, StateAcc}`: `ClassVars` always sits immediately before
  the trailing `StateAcc`, as one more position *after* the existing
  early-exit gate slots. Represented as an orthogonal boolean (e.g.
  `ThreadingMode::TupleAcc { gate_slots: usize, threads_class_vars: bool }`
  — exact field name a migration-issue implementation decision, not pinned
  here), never by incrementing `gate_slots` itself — `EarlyExitGateSlotMismatch`
  (`threaded_ir.rs:1336-1340`) compares `mode_gate_slots` against
  `TupleAccUnpack`'s own `gate_slots`, and that comparison must stay about
  the pre-existing early-exit-result count only; folding `ClassVars` into
  the same counter would make the check fire (or fail to fire) for reasons
  having nothing to do with early-exit gate-slot liveness, the exact kind
  of invariant-conflation this ADR's methodology (Addendum 2 Gap 1 option 3;
  Addendum 5's dynamic-field-`Put` options analysis) consistently rejects.
  Deferred, not designed further — no compiled evidence exists to pin an
  unpack `Bind` sequence against, and (per Question 4 Part B) none can exist
  until a *separate*, later change relaxes `select_tuple_acc`'s exclusion —
  out of this epic's scope.

**The `dict_ops.rs::generate_dict_do` bare-self-send coverage question,
confirmed with real evidence, not inference.** Two repros:

- `s13` (list-shaped `do:`, receiver a variable — not a `MapLiteral`, so
  `try_generate_dict_message`'s `do:` arm never matches, per its own doc
  comment "only intercept when receiver is a dictionary literal... other
  receivers fall through to the list `do:` handler," `intrinsics.rs:760-762`)
  and `s14` (the true `dict_ops.rs`-routed shape: a literal `#{...}`
  receiver, matching `try_generate_dict_message`'s actual precondition,
  `intrinsics.rs:782-784`) both compile a bare (no co-occurring local
  mutation), class-method-self-send-only `do:` block. **Both are correctly
  rejected**, with BT-3151's exact `check_no_unsafe_class_method_self_sends`
  error text ("this self-send cannot be proven free of class-variable
  mutation... unlike a threaded loop body, this block has no way to thread
  such a mutation back"). For `s14` specifically: `generate_dict_do`
  (`dict_ops.rs:25-47`) calls `needs_mutation_threading` (`false` here — a
  bare self-send in class-method context is not a "captured local variable
  mutation," `mod.rs:2606-2621`'s `in_class_method()` branch), returns
  `Document::Nil`, and `try_generate_dict_message` (`intrinsics.rs:789-794`)
  converts that into `Ok(None)` — ceding control to the fallthrough chain,
  which reaches `list_ops`'s own `check_no_unsafe_class_method_self_sends`
  call (`list_ops/mod.rs:105`/`list_ops/transform_ops.rs:546`) via the same
  "any collection-like receiver, `beamtalk_collection:to_list`" path `s13`
  exercises directly. **Corrected finding: there is no `dict_ops.rs`
  bare-self-send coverage gap** — coverage is real, provided by construction
  of the fallthrough architecture (every mode-declining call site ceding to
  a generic path that itself carries the check), not by `dict_ops.rs`
  needing its own copy of the check.
- `s15` (the `dict_ops.rs`-routed **co-occurring**-mutation shape — a
  literal `#{...}` `do:` with both a class-method self-send and a
  co-occurring local accumulator, mirroring `s09`'s `DriverDo` shape exactly)
  **compiles successfully and silently loses the mutation** — confirmed via
  its `.core` output, byte-for-byte the same dead-`ClassVars1`-local shape
  as `s09`. This is **not a new or different gap**: `generate_dict_do_with_mutations`
  (`dict_ops.rs:51`+) calls the *same* `generate_threaded_loop_body` this
  entire addendum targets — it inherits Question 6's fix automatically once
  the plain-map-fold `{ClassVars, StateAcc}` accumulator design lands there,
  exactly as every other `Foldl*` call site does. Confirms the issue's own
  framing (`generate_threaded_loop_body`/`_inner` is *the* shared body
  generator for every `Foldl*` construct, dict-specific or list-generic
  alike) rather than requiring a separate design.

### Implementation table for BT-3155's Letrec/Foldl migration issues

| # | Question | Decision | Primary site(s) |
|---|---|---|---|
| 1 | `ShadowWriteMissing` frame gate | New `shadow_write_eligible: bool` on `Threaded`/`ConditionalLoop`, lowered from live `self.block_depth == 0`; `verify()` gains a parallel `shadow_write_eligible_stack`, AND-combined on push. Gate changes from `target.frame == FrameId::ROOT` to the stack's top. | `threaded_ir.rs:845-862,925-960,1276-1285,1287-1316` |
| 2 | `construct_and_verify_class_var_bind`'s frame param | Replace `at_method_top_frame: bool` with a real `frame: FrameId` (caller's `current_branch_frame()` or `FrameId::ROOT`) + the new `shadow_write_eligible: bool` (Question 1) as a separate parameter. Internal branching re-derives `at_method_top_frame` locally; both existing call sites pass their real values (one of which is now honestly `FrameId::ROOT` instead of the old sentinel `FrameId::new(1)`). | `threaded_ir.rs:2252-2329`; callers `expressions.rs:704`, `dispatch_codegen.rs:522` |
| 3 | `ClassVars` through the tail call | Always an extra, explicit, real-`VersionedVar`-backed fun parameter — `fun(Var1..VarN, ClassVars)` (DirectParams/Hybrid) or `fun(StateAcc, ClassVars)` (StateAcc) for Letrec. Never folded into `StateAcc`'s own map. | `while_loops.rs` (`ConditionalLoop` construction), `control_flow/mod.rs`'s counted-loop/StateAcc-mode fun-header builders |
| 4 | Mode-selection interaction | (a) Confirmed: `has_self_sends` already unconditionally forces `StateAcc`/plain-map-fold for both Letrec and Foldl — no change needed there. (b) Fix: add `!generator.in_class_method()` to `select_hybrid_params`'s guard, closing the latent wrong-Hybrid-selection gap for Actor-class-method loop bodies. Net: every ClassVars-mutating body in scope always lands in `StateAcc`/plain-map-fold mode. | `control_flow/mod.rs:560-582` (`select_hybrid_params`) |
| 5 | `ClassSelf` visibility | Confirmed free (ordinary Core Erlang closure scoping) inside both `letrec` and `Foldl` fun bodies — no plumbing needed for the shadow write's `ClassSelf` read or for reading the loop-entry `ClassVars` value. Only the *updated*, per-iteration value needs the Question 3/6 extra-parameter/tuple-slot plumbing to escape the closure. | n/a (confirms no code change needed for this half) |
| 6 | Letrec vs. Foldl accumulator shape | Letrec: `ClassVars` is one more parameter next to the existing `exit_arm`/tail-call argument list — no new shape. Foldl: accumulator becomes `{ClassVars, StateAcc}` (2-tuple) whenever `ClassVars` threads, unconditionally at `gate_slots=0` (the only reachable case per Question 4); the general `{Gate1..GateG, ClassVars, StateAcc}` composition is specified but deliberately not implemented (no reachable repro; `EarlyExitGateSlotMismatch`'s `gate_slots` count must stay untouched by `ClassVars`'s presence — model as an orthogonal bool, not an extra gate slot). | `control_flow/mod.rs`'s Foldl accumulator plumbing (`ThreadingPlan`, `generate_tuple_unpack_docs`), `dispatch_codegen.rs:466-521` (`emit_class_var_result_unwrap`, unchanged in *how* it builds `ClassVars1`, changed in where that value is threaded) |

**Explicitly out of scope for this addendum** (left to the Letrec-migration
and Foldl-migration implementation issues themselves, mirroring how
Addendum 5 was its own dedicated per-shape design pass, sequenced *after*
Addenda 2/4 pinned the general node shapes): the full per-statement-shape
`Bind`/`Gensym`/`ValueRef::Doc` decomposition of every branch inside
`generate_threaded_loop_body_inner` (field assignment, actor self-send,
Tier-2 value call, local-var assignment, destructure — the ~15-shape
enumeration Addendum 5 did for `conditionals.rs`/`exception_handling.rs`).
This addendum answers the six cross-cutting design questions BT-3155's own
epic body named as blocking; a full per-shape decomposition, if the
migration issues need one before implementing, is their own follow-up design
pass against this table, not pre-empted here.

## Addendum 10 (2026-08-13): BT-3170 close-out — Epic BT-3155 implementation tracking and final measurement

**Epic:** [BT-3155](https://linear.app/beamtalk/issue/BT-3155) — Thread ClassVars through loop bodies and fold accumulators
**Status:** Done

| Issue | Description | Size | PR |
|---|---|---|---|
| [BT-3166](https://linear.app/beamtalk/issue/BT-3166) | Design: ClassVars threading through loop/fold bodies — six design questions resolved (this ADR's own Addendum 9) | M | [#3367](https://github.com/jamesc/beamtalk/pull/3367) |
| [BT-3167](https://linear.app/beamtalk/issue/BT-3167) | Infra: widen class-var `Bind` frame construction (`frame`/`shadow_write_eligible` replacing `at_method_top_frame`) + `ShadowWriteMissing` frame-scoping fix | M | [#3369](https://github.com/jamesc/beamtalk/pull/3369) |
| [BT-3168](https://linear.app/beamtalk/issue/BT-3168) | Migrate `Letrec` (loop) bodies: un-reject BT-3140/BT-3150 for `whileTrue:`/`timesRepeat:`/`to:do:`/`to:by:do:` | L | [#3370](https://github.com/jamesc/beamtalk/pull/3370) |
| [BT-3169](https://linear.app/beamtalk/issue/BT-3169) | Migrate `Foldl*` bodies: close BT-3151's silent-loss gap for list_ops + dict_ops | L | [#3371](https://github.com/jamesc/beamtalk/pull/3371) |
| [BT-3170](https://linear.app/beamtalk/issue/BT-3170) | Close-out: regression sweep, new Foldl runtime coverage, docs, this addendum | M | this PR |

**Regression sweep (task 1 of this issue).** Every `Err(CodeGenError::ClassVarAssignmentInThreadedBody{..})`/
`Err(CodeGenError::ClassMethodSelfSendInThreadedLoopBody{..})` test in
`tests/gen_server.rs` pinning the old (broken) rejection behavior for the
`Letrec`/`Foldl*` shapes BT-3168/BT-3169 fixed had already been flipped to
success assertions as part of those two issues' own PRs — confirmed by
grep (`Err(CodeGenError::` now matches only `ClassMethodSelfSendInUnthreadedBlock`
and `FieldAssignmentInUnsupportedBlock`, the two still-intentionally-rejected
shapes: a bare loop/fold block with no co-occurring local mutation, which
never reaches state threading at all — see this ADR's main body's
`ThreadedIr verifier` cross-reference in `docs/development/debugging.md`
and `docs/beamtalk-language-features.md`'s "Passing Blocks Through Class
Methods" section for the user-facing version of that remaining boundary).
`test_class_method_self_send_alongside_local_in_do_body_survives_via_class_vars_threading`
and its `select:` sibling (BT-3169) were re-checked and already assert
success + the `{ClassVars, StateAcc}` accumulator shape correctly. No
further flips were needed.

**New runtime-correctness coverage (task 2).** BT-3168 already shipped
`stdlib/test/fixtures/loop_class_var_mutation.bt` +
`loop_class_var_mutation_test.bt`, running BT-3140/BT-3150's exact
`Driver countedRun:`-style repro (direct field write and self-send, inside
`whileTrue:`/`timesRepeat:`/`to:do:`, alongside a co-occurring local) end
to end and checking the class var's actual final value — this closes task
2 for the `Letrec` side. The `Foldl*` side had only a pure-self-send
regression test (`foldl_class_var_regression_test.bt`, added during
BT-3169's own PR review to pin an unrelated shadow-read correctness bug);
no runtime test exercised a *mutating* self-send inside a `do:`/`select:`/
`collect:` body actually accumulating across iterations — the direct
`Foldl*` analogue of BT-3151's `DriverSelect positives:` repro. This issue
adds `stdlib/test/fixtures/fold_class_var_mutation.bt` +
`fold_class_var_mutation_test.bt`, covering `countedRun:` (do:),
`positives:` (select:), and `doubleAllCounting:` (collect:), each with a
same-class mutating self-send alongside a co-occurring local — the shape
that reaches `Foldl*` threading — plus cross-call persistence and
zero-iteration edge cases, mirroring `loop_class_var_mutation_test.bt`'s
own structure. All pass.

**Docs (tasks 3-4).** `docs/beamtalk-language-features.md`'s "Passing
Blocks Through Class Methods" trap note was rewritten: the previous
blanket "class-var mutations inside loops/blocks are rejected at compile
time" statement is now accurate only for the narrower bare-body case (no
co-occurring local mutation); the common case (a loop/fold body that also
has its own accumulator/counter/index) now compiles and threads correctly,
shown with a positive example instead of only a rejection + workaround.
`docs/development/debugging.md`'s `ShadowWriteMissing` row was already
updated by BT-3167's own PR (confirmed by `git log`/`git show` against
that commit, per this issue's own instruction to check first rather than
redo it) — this issue adds a short paragraph after the `VerifyError` table
summarizing what BT-3166-BT-3169 actually landed (`ConditionalLoop`'s
`shadow_write_eligible`-gated `ClassVars` fun parameter for `Letrec`
bodies; the `Foldl*` accumulator's `{ClassVars, StateAcc}` 2-tuple shape)
and confirms no new `VerifyError` variant was introduced — both migrations
route through the pre-existing `UnboundVersion`/`NonLinearVersion`/
`ShadowWriteMissing` checks against the now-real `Bind`s these node kinds
produce.

**Cumulative measurement** (this issue's own task 5 — BT-3148 Addendum
6 / BT-3149 Addendum 7 methodology: two separate release binaries, cold
`ebin/` each run, `beamtalk build-stdlib --quiet --warnings-as-errors`,
8 runs per side, alternating baseline/HEAD per round to spread any
systematic drift evenly across the run order). Baseline is `d4c2e57`
(`main` immediately before BT-3166, the epic's first commit); "this epic"
is this issue's own branch HEAD, which carries every `Letrec`/`Foldl*`
`ClassVars`-threading codegen change BT-3167-BT-3169 shipped to `main`
(this issue's own changes are tests/docs only — no codegen, confirmed by
`git diff --stat`: two stale doc-comment path references in
`tests/gen_server.rs` (a test file, not a codegen production module), new
`stdlib/test/*.bt` fixtures, and doc/ADR prose — no file under
`crates/beamtalk-core/src/codegen/` outside its `tests/` subtree changed):

| | wall-clock (s) | user CPU (s) |
|---|---|---|
| baseline (mean of 8, `d4c2e57`) | 7.80 (range 6.36–9.47) | 10.97 (range 10.71–11.34) |
| this epic (mean of 8, HEAD) | 7.71 (range 6.87–8.89) | 10.93 (range 10.55–11.57) |
| Δ | −1.17% | **−0.39%** |

Both ranges overlap heavily (baseline user CPU 10.71–11.34s vs. this
epic's 10.55–11.57s), consistent with Addenda 3/6/7's own finding that
this shared/virtualized environment's noise floor dominates at this scale
— not distinguishable from noise at n=8, and in either case the measured
delta is a **reduction**, not a regression. Read on user CPU (the more
trustworthy metric per Addenda 3/6/7): **−0.39%, comfortably inside the
≤3% gate**. This is consistent with what the `ClassVars`-threading changes
architecturally are: an extra fun parameter (`Letrec`) or a wider
accumulator tuple (`Foldl*`) on the specific bodies that mutate a class
var from inside a loop/fold — a small fraction of the stdlib corpus's
total loop/fold bodies, most of which don't touch class vars at all and
so take neither code path. **Gate cleared.**

All acceptance criteria for BT-3155 are met: BT-3140/BT-3150/BT-3151 are
closed (marked Done in Linear, each with a comment linking the PR that
fixed it — BT-3168's #3370 for BT-3140/BT-3150, BT-3169's #3371 for
BT-3151); the stale-rejection test sweep found nothing left to flip;
runtime-correctness regression coverage exists for both the `Letrec` and
`Foldl*` sides; docs reflect the new, narrower restriction; and the
cumulative build-time gate is cleared.

## Addendum 11 (2026-08-14): BT-3174 spike — does `value_type_codegen.rs`'s
vt-conditional family duplicate `ThreadingMode::TupleAcc`? No-go, with a
structural reason beyond Addendum 5's original exclusion

BT-3174 asked whether `value_type_codegen.rs`'s vt-conditional tuple-threading
functions (`build_vt_conditional_value_and_mutations_parts`,
`build_vt_conditional_branch_pieces`/`_inner`, `resolve_mutation_value_docs`,
`finish_vt_conditional_branch`, `rebind_vt_conditional_mutations`,
`build_vt_conditional_branch_value`/`_parts`) hand-roll a pattern
`ThreadingMode::TupleAcc` already formalizes — "thread N mutated locals
through a branch as a tuple, `element(1, ...)` for the logical value,
`element(i+2, ...)` per mutated local" — and, if so, whether CLAUDE.md's
no-duplicate-implementations rule calls for migrating this family onto
`ThreadedIr`. All line numbers below are against
`crates/beamtalk-core/src/codegen/core_erlang/` at `main` commit
`91fe1af8`; resolve by symbol name first, per this ADR's established
practice.

**Answer: no-go.** Not because the textual shape doesn't match (it does,
closely) but because `ThreadingMode::TupleAcc` is a fold/loop-accumulator
construct with invariants this shape structurally cannot satisfy — and the
compiler's own existing guards already say so, independent of anything this
spike adds. The most direct evidence is a pre-existing test that answers the
exact question this issue asks:

```rust
// control_flow/mod.rs:4756 (existing, unmodified by this spike)
fn select_tuple_acc_blocked_by_conditional_threaded_writes() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_conditional_threaded_writes = true;
    assert!(!ThreadingPlan::select_tuple_acc(
        true, &threaded, CodeGenContext::Actor, &analysis, &effects
    ));
}
```

`TupleAcc` mode is blocked whenever `has_conditional_threaded_writes` is
true — **regardless of `CodeGenContext`** (the test passes `Actor`, not
`ValueType`). `select_tuple_acc`'s guard (`control_flow/mod.rs:649-667`)
returns `false` if `matches!(context, CodeGenContext::ValueType)` **or**
`body_analysis.has_state_effects()` **or** `effects.has_cf_mutations` **or**
`effects.has_conditional_threaded_writes` **or** `effects.last_is_destructure`.
When the conditional-writes guard trips, `diagnose_guard_failure`
(`control_flow/mod.rs:766-767`) names the fallback reason explicitly:
`StateAccFallbackReason::InlineConditionalThreadedWrite` — a loop body
containing an inline conditional that writes a threaded local falls all the
way back to the heavyweight `StateAcc`-map mode, skipping `TupleAcc`
entirely. So even in the one context ADR 0111 already migrated
(`CodeGenContext::Actor`, `conditionals.rs`'s `generate_conditional_branch_inline`,
Addendum 5), a conditional-branch tuple join was never routed through
`TupleAcc` — Addendum 5's own vocabulary (§"The decomposition vocabulary")
confirms this independently: actor-side conditionals decompose into direct
`Bind` sequences (rules 2-4, `State`/`ClassVars`-prefixed `maps:put` chains
and the `Gensym` two-hop), never into a `Threaded{mode: TupleAcc}` node.
**`TupleAcc` has never been the tool this codebase uses for "thread values
out of a two-arm branch" in *either* context** — vt-conditional's
Value-context hand-rolling isn't a parallel implementation of a pattern
ThreadedIr formalizes elsewhere; the pattern ThreadedIr formalizes is a
different pattern that happens to render with similar-looking
`element(N, ...)` syntax.

### Why the two are different constructs, not the same one twice

1. **Cardinality and origin.** `ThreadedStmt::TupleAccUnpack`
   (`threaded_ir.rs:1018-1024`) unpacks a flat `{Gate1, .., GateG, Var1, ..,
   VarN}` accumulator from an `AccParam` — the **second parameter of a fold
   lambda**, re-entered once per collection element
   (`render_tuple_acc_unpack`, `threaded_ir.rs:2063-2082`: `element(idx,
   Param)` where `Param` is literally the fold lambda's own bound name, e.g.
   `"StateAcc"`). `AccParam`'s doc comment (`threaded_ir.rs:656-670`) is
   explicit: "bound exactly once per lambda invocation... never a
   `VersionedVar`." vt-conditional's tuple (`_CondAssign3` below) is instead
   the `let`-bound result of a **two-arm `case` evaluated exactly once** —
   there is no lambda, no re-entry, no accumulator identity to name as an
   `AccParam`. The unpack direction is inverted too: `TupleAccUnpack` reads
   its tuple **at the start of an iteration**, before that iteration's body
   runs, to seed the next round of mutation; vt-conditional's
   `element(N, ...)` chain reads its tuple **after both arms have already
   finished computing**, to merge their two independently-computed results
   into the enclosing scope. One is "unpack, then compute"; the other is
   "compute (twice, once per arm), then unpack."
2. **Gate slots encode operation-specific continuation state, not a generic
   logical value.** `ThreadingMode::TupleAcc(usize)`'s `gate_slots`
   (`threaded_ir.rs:792-812`) reserve `0`/`1`/`2` leading tuple positions
   for `do:`'s (nothing) / `collect:`'s accumulator list / `detect:`'s
   `{FoundItem, FoundFlag}` pair — each a **list-op-specific** continuation
   channel read by that op's own post-fold wrapper, never a `VersionedVar`.
   vt-conditional's leading slot is a completely different thing: the
   branch's **logical/last-expression value** (compiled evidence below:
   `element(1, _CondAssign3)` binds `_r`, the conditional's own result, not
   an operation-specific gate). Reusing `TupleAcc(usize)`'s `gate_slots`
   field to mean "1, for the branch value" would overload one field with
   two unrelated meanings the existing `VerifyError::EarlyExitGateSlotMismatch`
   check (`threaded_ir.rs:1172-1189`) is specifically built to police for
   its *actual* meaning — list-op gate-slot arithmetic, not conditional
   value-slot arithmetic.
3. **The verifier already has a dedicated, regression-pinning check against
   exactly this reuse.** `VerifyError::TupleAccInValueTypeContext`
   (`threaded_ir.rs:1191-1206`, `#[cfg(test)]`) exists specifically because
   `select_tuple_acc`'s `ValueType` exclusion is a structural invariant this
   ADR already considers worth guarding: "fires if a future change to
   `select_tuple_acc`'s guard ordering ever lets `use_tuple_acc` become
   `true` in a `ValueType` context." Adopting `TupleAcc` for
   vt-conditional's tuple would require *removing or narrowing* this
   existing guard, not merely adding new code next to it — a real
   regression-pinning invariant this ADR already ships would have to
   change, not just an unaddressed gap being filled in.
4. **No existing `ThreadedStmt` variant models a two-arm branch join at
   all.** `Threaded` wraps a fold/loop body (`render_threaded`,
   `threaded_ir.rs:1748-1777`); `ConditionalLoop` (Addendum 2) is a
   condition-gated *tail-recursive* skeleton, not an if/else; `Bind` is
   single-source/single-target, with no positional-tuple join semantics.
   A faithful IR model of vt-conditional's shape — two sibling `Bind`
   sequences (one per arm, each its own `FrameId`, mirroring
   `with_branch_context`'s existing per-arm-frame discipline) merged into a
   positional tuple at the join point — is a **new `ThreadedStmt` variant**,
   not a reuse of `TupleAcc`. That is a materially different, larger
   proposal than this issue scoped ("use `TupleAcc`"), and is not evaluated
   here — see "What a real proposal would need" below.

### Compiled evidence

Two repros compiled with a locally-built `beamtalk` (`cargo build --bin
beamtalk`, then `beamtalk build <dir>`), read from the real `.core` output.
Not committed as fixtures (this is a no-go spike, so per this issue's own
scope no test fixtures are added — see BT-3159's existing
`stdlib/test/fixtures/class_method_conditional_local_and_class_var.bt` for
an equivalent, already-landed repro covering the `ClassVars`-carrying
variant cited below); reproducible directly from the source shown here:

**1 mutated local** (`Value subclass: VtCondOneLocal`, `bump: flag => x :=
0. _r := flag ifTrue: [x := 5. 42] ifFalse: [0]. x + _r`), routed through
`emit_vt_conditional_assign_rhs` (`value_type_codegen.rs:2994`) via
`build_vt_conditional_value_and_mutations_parts` (`:3132`):

```erlang
let X = 0 in
let _Cond2 = _flag1 in
let _CondAssign3 = case _Cond2 of
  <'true'> when 'true' -> let X = 5 in {42, X}
  <'false'> when 'true' -> {0, X}
end in
let _r = call 'erlang':'element'(1, _CondAssign3) in
let _x4 = call 'erlang':'element'(2, _CondAssign3) in
```

**2 mutated locals** (`Value subclass: VtCondTwoLocals`, `bump: flag => a :=
0. b := 0. _r := flag ifTrue: [a := 1. b := 2. 99] ifFalse: [a := 3. b := 4.
0]. a + b + _r`), same call path:

```erlang
let A = 0 in
let B = 0 in
let _Cond2 = _flag1 in
let _CondAssign3 = case _Cond2 of
  <'true'> when 'true' -> let A = 1 in let B = 2 in {99, A, B}
  <'false'> when 'true' -> let A = 3 in let B = 4 in {0, A, B}
end in
let _r = call 'erlang':'element'(1, _CondAssign3) in
let _a4 = call 'erlang':'element'(2, _CondAssign3) in
let _b5 = call 'erlang':'element'(3, _CondAssign3) in
```

Both confirm the task's framing exactly — `element(1, ...)` for the branch
value, `element(i+2, ...)` (i.e. `element(2, ...)`, `element(3, ...)`, …)
for each mutated local, in declaration order. No `StateAcc`, no gate slots,
no fold lambda anywhere in sight — a plain, one-shot `case`-result
destructure.

### A refinement to Addendum 5's exclusion rationale (does not change the
verdict)

Addendum 5 states "Class-var mutations never route here either — by
construction" for `conditionals.rs`'s **Actor-context**
`generate_conditional_branch_inline`. That claim is correct as scoped, but
this spike found a *different*, already-shipped, already-tested code path
where `value_type_codegen.rs`'s **own** vt-conditional family — the same
family this issue is about — genuinely does thread a version-counted
`ClassVarsN` slot through its branch tuple: `build_vt_conditional_branch_pieces`
+ `finish_vt_conditional_branch` (`:3432`) append a trailing `ClassVars`
tuple element whenever `any_cv_mutated`, and `rebind_vt_conditional_mutations`
(`:3476`) extracts it via `element(all_mutations.len() + 1, ...)`. Compiled
proof (`stdlib/test/fixtures/class_method_conditional_local_and_class_var.bt`,
BT-3159, already a landed regression fixture with its own passing test suite
— `class_method_conditional_local_and_class_var_test.bt`), `class m: flag =>
x := 1. flag ifTrue: [x := self bump]. x`:

```erlang
'class_m:'/3 = fun (ClassSelf, ClassVars, _flag3) ->
    let X = 1 in
    let _Cond4 = _flag3 in
    let _CondResult12 = case _Cond4 of
      <'true'> when 'true' ->
        let _CMR5 = call 'bt@...':'class_bump'(ClassSelf, ClassVars) in
        let ClassVars1 = case _CMR5 of
          <{'class_var_result', _MR7, _CV6}> when 'true' -> _CV6
          <_PCV8> when 'true' -> ClassVars
        end in
        let _Unwrapped9 = ... in let X = _Unwrapped9 in {X, ClassVars1}
      <'false'> when 'true' -> {X, ClassVars}
    end in
    let X = call 'erlang':'element'(1, _CondResult12) in
    let ClassVars1 = call 'erlang':'element'(2, _CondResult12) in
    ...
```

This is real: `class m:`/`n:`/`p:` are **class-side** methods, and per
CLAUDE.md, "a class method runs in its class's gen_server process" — so
`ClassVars` here is that gen_server's own actor state, threaded through a
vt-conditional's branch tuple exactly the way ADR 0110 threads it elsewhere.
Addendum 5's finding — instance-side vt-conditionals carry no `StateAcc`
or `{Result, State}` tuple, only plain rebinding (§"Value-type conditionals
never route here") — is accurate for *instance*-side Value methods (no
gen_server backs a Value instance) but is not the reason this particular
exclusion holds for the *class-side* vt-conditional path —
that path can and does carry `ClassVars`. **This does not change the
verdict above**: the class-var-carrying tuple still isn't `TupleAcc` shaped
(it's the same one-shot `case`-result join, just with one more trailing
element) and is still blocked by the same `select_tuple_acc` guards
(`has_conditional_threaded_writes`, and separately `has_state_effects` once
a class-var write is present). It is recorded here only so a future reader
does not generalize Addendum 5's instance-side no-`StateAcc` finding into a
blanket "no vt-conditional path ever carries state" justification broader
than the evidence supports — the real reason `TupleAcc` doesn't apply is
the structural mismatch in the four points above, not the absence of any
state-like value anywhere in this file.

### What a real proposal would need (not evaluated here — out of scope)

If a future spike wants to pursue IR-backed verification for this shape
(not `TupleAcc` reuse, a *different* proposal), it would need: a new
`ThreadedStmt` variant modeling a two-arm value-and-locals join (per-arm
`Bind` sequences under sibling `FrameId`s, a join op producing the
positional tuple, `produces` listing the post-join versions) plus its own
`render`/`verify` arms — comparable in size to `ConditionalLoop`'s own
addition (Addendum 2: one variant, ~45 lines of type definition, a
dedicated render function, no new `VerifyError` — "S" effort) — and would
need to independently decide whether `element(N, ...)` linearity is worth
checking at all here, since (per point 4 above and `TupleAccInValueTypeContext`'s
own reasoning) there is no early-exit, no loop, and no gate-slot arithmetic
for a `VerifyError` to catch that hand-written `i + 2` index math couldn't
already get right or wrong on inspection. That is a materially different,
new-IR-surface proposal, not a migration onto existing `TupleAcc`
machinery, and is not scoped or recommended by this spike.

### LOC accounting

The current hand-rolled vt-conditional tuple-threading family
(`value_type_codegen.rs:2739-3536`, doc comments included) is **798
lines** across nine functions. No rewrite onto `ThreadingMode::TupleAcc`
was attempted beyond the structural analysis above, because §"Why the two
are different constructs" (points 1-3) establishes the reuse is blocked at
the type/invariant level before an LOC comparison would be meaningful — a
rewrite that must first defeat an existing `#[cfg(test)]` regression guard
(`TupleAccInValueTypeContext`) and misuse `AccParam`'s per-iteration
contract is not a fair "would it shrink the code" comparison; it is a
proposal to weaken an invariant this ADR already ships, evaluated and
rejected on those grounds above the LOC question.

### Decision

**No-go.** `value_type_codegen.rs`'s vt-conditional family stays
hand-rolled, AST-directed Core Erlang construction, exactly where ADR 0111
§Constraints already placed it. The textual similarity to `TupleAcc`'s
`element(N, ...)` shape is superficial — same rendering idiom, different
construct: a fold-lambda accumulator threaded across N loop iterations
versus a two-arm branch's one-shot result join. This is not the
"Module X sits below Y in the dependency graph" excuse CLAUDE.md's
duplication rule rejects — it is two different rules that happen to render
similarly, the case CLAUDE.md's rule is not aimed at. No follow-up
migration issue is filed. If a future reader wants to reopen this, the new
evidence needed is not "the shapes look similar" (already true, already
evaluated here) but either (a) a proposal for the new `ThreadedStmt`
variant sketched above with its own justification for why `verify()` would
catch something real for a construct with no loop and no early exit, or (b)
a change to `select_tuple_acc`'s guards that already-shipped, already-tested
production code depends on staying as they are.

## Addendum 12 (2026-08-14): BT-3164/BT-3171/BT-3172 close-out — correcting Addendum 6's stale "5 other call sites... untouched" claim

Found during a post-Addendum-11 cleanup review: this ADR had gone three
addenda (9, 10, 11) without recording the three issues that closed out
Addendum 6's own "Not attempted" note, even though all three merged to
`main` before Addendum 11 was written. This addendum records them and
corrects the one sentence of Addendum 6 they make false.

**BT-3164 — class-method body pipeline migrated (PR
[#3366](https://github.com/jamesc/beamtalk/pull/3366), merged
2026-08-13).** `gen_server/methods.rs::generate_class_method_body` — the
"separate, hand-written `Document` builder pre-dating `BodyExprKind`/
`classify_body_expr` entirely" Addendum 6 named as out of its scope — is
now `lower_class_method_body`, returning a real `Vec<ThreadedStmt>`.
`generate_class_method_fun_from_block` was migrated the same way, its
`wrap_class_method_body_with_nlr_catch` call site deleted. Both now run
their class-var `Bind` and their `NlrCatch` through one
`verify_and_render_body_stmts` call, closing the ADR 0110 joint-visibility
gap Addendum 6 itself flagged: `VerifyError::ShadowWriteMissing` can now
see a real class-var `Bind` jointly with a real class-method `NlrCatch`.
BT-3164 also audited the remaining Actor-side `wrap_body_with_nlr_catch`-family
call sites and named three of them as real, well-understood follow-up
(tracked as BT-3171 rather than folded in), and confirmed
`gen_server/extensions.rs`'s `generate_value_extension_fun` is a
structurally different call site, not deferred work — see below. Full
citations: `threaded_ir.rs`'s own "Status (as of BT-3164 — the
class-method body pipeline)" section, lines 179-244.

**BT-3171 — the 3 remaining Actor-boundary call sites migrated (PR
[#3373](https://github.com/jamesc/beamtalk/pull/3373), merged
2026-08-13).** The three sites BT-3164's audit named —
`gen_server/dispatch.rs`'s `generate_legacy_method_clause`,
`gen_server/extensions.rs`'s `generate_actor_extension_fun`, and
`actor_codegen.rs`'s sealed-method generator — each now lower their body
via `lower_method_definition_body_with_reply` (widened to
`pub(in crate::codegen::core_erlang)`) or the new Block-based
`lower_method_body_with_reply`, prepend a real `ThreadedStmt::NlrCatch`
when NLR was detected, and share one tail,
`gen_server/methods.rs::prepend_nlr_catch_and_render`, also adopted by
`generate_method_dispatch`'s own BT-3148-era call site rather than left
duplicated. `wrap_actor_body_with_nlr_catch` had no callers left afterward
and was deleted, the same way BT-3164 deleted
`wrap_class_method_body_with_nlr_catch` — confirmed still gone by grep
against current `main` (only `prepend_nlr_catch_and_render` call sites
remain in `dispatch.rs`, `extensions.rs`, `methods.rs`, `actor_codegen.rs`).
Full citations: `threaded_ir.rs`'s "Status (as of BT-3171 — the remaining
Actor-boundary call sites)" section, lines 246-291.

**The one call site that is not migrated, and was never meant to be:**
`gen_server/extensions.rs`'s `generate_value_extension_fun`
(`wrap_value_type_body_with_nlr_catch`). Both BT-3164's audit and BT-3171's
own scope confirm this is a different shape entirely — it never renders a
body `Document` and wraps it after; its catch scaffolding is built from
`NlrCatchVars` directly, integrated inline into the streaming
`generate_vt_body_exprs`/`emit_vt_*` construction in
`value_type_codegen.rs`, the vt-conditional family Addendum 5 and Addendum
11 both independently confirm is a permanent, ADR-documented exception to
this migration (§Addendum 11's "Decision," `value_type_codegen.rs`'s
798-line hand-rolled family stays hand-rolled, AST-directed Core Erlang
construction). This is the one site of Addendum 6's original "5 other"
list that stays unmigrated by design, not by omission.

**BT-3172 — nested-loop class-var mutation rejected at compile time (PR
[#3375](https://github.com/jamesc/beamtalk/pull/3375), merged
2026-08-14).** Unlike BT-3164/BT-3171, this is not a `wrap_body_with_nlr_catch`
migration at all — it closes a silent-data-loss gap BT-3140/BT-3150/BT-3151
(Addendum 10) left standing one level deeper: a class-var mutation (or
same-class self-send) inside a loop/fold that is itself nested inside
another loop/fold, where the *outer* construct has no class-var mutation
of its own to carry the inner one back out. `control_flow/mod.rs`'s
`find_class_var_mutating_stmt` (~line 1623) now detects the shape via two
independent triggers mirroring each body kind's own real threading gate
(`loop_body_threads_class_vars` for `Letrec`, `block_analysis::analyze_block`'s
recursive `has_self_sends` for `Foldl*`), and rejects it at compile time
with `CodeGenError::ClassVarMutationLostAcrossNestedLoop`
(`mod.rs:393-398`) — an actionable diagnostic naming the mutation and
location, with a fix (accumulate into a local across both loops, mutate
the class var once after the outer loop finishes). This is a **compile-time
rejection**, not a `ThreadedIr`-threaded construct: no new `ThreadedStmt`
variant or `VerifyError` was introduced, and none was needed — the
nested-loop shape stays permanently unrepresentable input, the same class
of guard as the bare-loop-no-co-occurring-local rejection Addendum 10's
own regression sweep names as "still intentionally-rejected."

**Correcting Addendum 6.** Addendum 6 (2026-08-12) states: "The 5 other
`wrap_body_with_nlr_catch` call sites beyond the two this addendum names
... are untouched." That sentence was accurate on 2026-08-12 and is false
as of BT-3164/BT-3171 (2026-08-13): four of those five sites are migrated
to real `NlrCatch` prepend, and the fifth
(`generate_value_extension_fun`) is confirmed permanently out of scope
rather than merely untouched. Per this ADR's own precedent for correcting
an earlier addendum — Addendum 8's "One correction to this addendum's Rule
2" (correcting Addendum 5's Rule 2 without editing Addendum 5's text) and
Addendum 11's correction of Addendum 5's instance-side finding (again
without editing Addendum 5) — this addendum leaves Addendum 6's text
unedited as the honest record of what was known on 2026-08-12, and
supersedes only that one sentence going forward: readers following
Addendum 6's "untouched" claim past 2026-08-13 should read this addendum
instead. `threaded_ir.rs`'s own module-doc "Status" sections (BT-3164:
lines 179-244; BT-3171: lines 246-291) are the accurate, current record
this addendum draws from and should stay the first place future migrations
of the remaining permanent exception (should one ever be proposed) update
— cross-referenced here specifically so this ADR and that module doc do
not drift apart again the way this addendum's own existence shows they
already did once.

**No remaining ADR claim contradicted by current `main`, beyond the one
sentence corrected above** — spot-checked by re-reading Addendum 6 in full
against `threaded_ir.rs`'s BT-3164/BT-3171 status sections and the three
PRs' diffs; no other claim in Addendum 6, or elsewhere in this ADR, refers
to these three issues' scope.

## Addendum 13 (2026-08-14): BT-3182 — `BEAMTALK_THREADED_IR_WHILE_DIRECT` decided: deleted

Addendum 3 left `BEAMTALK_THREADED_IR_WHILE_DIRECT` (the BT-3145 pilot
routing `generate_while_loop_direct` through `ThreadedIr`) parked in an
explicitly unresolved state: off by default, not flipped, not deleted,
pending either a re-measurement on a lower-noise setup or a decision to
give up on it. Ten addenda later, nothing had revisited it — found during a
post-ADR-0111 cleanup review as the one place the migration's "legacy paths
are deleted" premise still didn't hold on `main`.

**The three options, weighed against what actually changed (nothing) since
Addendum 3:**

1. **Finish it** — scope the "gap three" work (full loop-body coverage:
   plain-let temporaries, destructuring, list-op RHS) as its own design
   pass, then re-measure. Rejected: no concrete trigger exists for this
   investment. The legacy path is not an unverified gap — BT-3132's
   side-channel checks already run real `ThreadedIr` verification against
   every while/counted loop body today, flag on or off — so finishing the
   pilot buys only a narrower dual-computation-drift risk for the subset it
   would cover, not a correctness fix. Spending a BT-3156-sized design pass
   to chase an inconclusive ≤3% measurement with no reproduction plan for
   the noise that made it inconclusive is not a good bet.
2. **Formally park** — document the flag as a permanent, maintained,
   opt-in path with an explicit re-open trigger. Rejected: this is what
   Addendum 3 already did, and the ten-addendum silence since is exactly
   the failure mode "formally park" predicts — an opt-in path nobody
   measures, exercises only by its own dual-run tests, accumulating drift
   risk against the legacy path it was supposed to eventually replace, with
   no forcing function to ever revisit it. CLAUDE.md is explicit here:
   "Don't use feature flags or backwards-compatibility shims when you can
   just change the code" — an env-flag-gated dual path that has sat
   unflippable for three issues and ten addenda is precisely that.
3. **Delete** — remove the pilot's gating/lowering layer, leave while/
   counted loops on side-channel verification only. **Chosen.**

**What was deleted** (`control_flow/while_loops.rs`): the
`BEAMTALK_THREADED_IR_WHILE_DIRECT` env-flag check
(`threaded_ir_while_direct_enabled`), `try_render_while_direct_via_threaded_ir`
(the ~240-line lowering/verify/render attempt), its eligibility gate
`while_direct_body_is_bind_representable` and RHS allowlist
`is_simple_threaded_rhs`, the six `dual_run_*`/byte-identity tests, and
their `codegen_with_threaded_ir_while_direct` harness — `generate_while_loop_direct`
no longer branches on the flag at all. `generate_loop_condition_body`
(factored out in Addendum 2 specifically so the pilot and the legacy path
shared one condition-codegen) stays — it is now the legacy path's only
caller, not dead.

**What was kept, and why this isn't a partial deletion:**
`ThreadedStmt::ConditionalLoop`, `ThreadingMode::DirectParams`, and
`VersionPrefix::Local` — the IR shapes Addendum 2's design introduced for
this pilot's lowering — are unconstructed by any production code once the
pilot's lowering is gone (confirmed by the `dead_code` lint after deletion:
these three, and only these three, of Addendum 2's additions fire it —
`ValueRef::Doc` and `VersionPrefix::Gensym`, Addendum 2's other two
additions, remain heavily used by `conditionals.rs`/`exception_handling.rs`/
`gen_server/methods.rs` and are untouched). Rather than delete these too
(which would also mean unpicking `render_conditional_loop`,
`render_loop_skeleton`'s shared plumbing with bare `Threaded`, the
`verify()` match arms handling `ConditionalLoop`, and their own dedicated
unit tests — a materially larger, higher-risk change than this issue's
scope, for a marginal win since the code is inert either way), they are
marked `#[allow(dead_code)]` with a citation to why: `ConditionalLoop`'s own
doc comment already named a second, not-yet-attempted consumer for this
exact shape at design time (Addendum 2) — a real counted-loop
(`to:do:`/`timesRepeat:`/`repeat`) migration — not a vague "might be useful
later." If that migration is never attempted, revisiting deletion of these
three is the honest follow-up, not carrying them forever on the same
reasoning.

**Outcome:** while/counted loops stay on `ThreadedIr` side-channel
verification only, identical to their state before BT-3145 — no behavior
change, confirmed by the full snapshot corpus + `stdlib`/`BUnit` suites
(`just verify-threaded-ir`) passing unchanged. `threaded_ir.rs`'s own
module-doc §Status (BT-3182) is the accurate, current record; this addendum
is the ADR-level pointer to it, per this ADR's own established
cross-referencing precedent (Addendum 12).

## Addendum 14 (2026-09-05): ADR 0118 close-out — Addendum 7's "narrow scope
was never the limiting factor" conclusion superseded

Addendum 7 (§Full-pipeline re-evaluation) concluded, across the five
construct-family migrations attempted through BT-3149 (loops, conditionals,
list-ops, gen_server bodies, stateful blocks): "ADR 0111's narrow scope
(state-threading constructs specifically, not general expression codegen)
was never a limiting factor for the migrations actually attempted," and
that nothing surfaced evidence the general expression codegen this ADR's
own §Scope excludes needed IR coverage. That conclusion was scoped
correctly to what had been attempted *by construct family* — a new
statement-shaped node kind reaching a new statement-shaped call site — but
it did not anticipate the axis [BT-3399](https://linear.app/beamtalk/issue/BT-3399)
found a few weeks later: a state effect nested *inside expression
position*, within a construct family already migrated. `generate_self_dispatch`
(an Actor self-send) had no `ThreadedIr` producer of its own — every
migrated consumer's `verify()` call covered its OWN construct's `Bind`
sequence, but a self-send sitting as a binary-op operand, a keyword-send
argument, or a `whileTrue:` condition was invisible to all of them, because
nothing represented "an expression, compiled for its value, that also needs
to advance `State`." Ten narrow fixes (BT-3392, BT-3396, BT-3399, BT-3402,
BT-3403, BT-3405, BT-3406, BT-3385) chased instances of this one shape
before [ADR 0118](0118-expression-level-state-threading-preludes.md) named
it as a single missing abstraction rather than an open-ended list of
positions.

**Superseded, not contradicted.** Addendum 7's own reasoning already named
the shape of the exception in advance, in the very same paragraph: "the
narrow-scope bet keeps paying off one construct family at a time... every
remaining boundary... is a 'construct family not yet migrated,' the same
shape as the five that were, not a case where the construct-local rendering
boundary itself is the blocker." ADR 0118's gap was not a sixth construct
family — it was a dimension orthogonal to construct family (statement
position vs. expression position) that the "one construct family at a time"
framing had no slot for. Addendum 7 was right that no migration attempted
through BT-3149 needed IR coverage beyond ADR 0111's scope; it was
incomplete in treating "every remaining boundary is a construct family not
yet migrated" as exhaustive, when a second axis existed that the five
completed migrations happened not to cross.

**Resolved without the ADR-0018-§Alternative-3-scale rewrite Addendum 7
itself estimated this generalization would cost.** ADR 0118 did not hand
each nested construct's whole `Vec<ThreadedStmt>` fragment up to its
enclosing method body (the full-pipeline design Addendum 7 priced at
"meaningfully bigger than 'close out the epic'"). Instead, `ThreadedValue`
makes every state-effecting expression form a *producer* of a small
`{ prelude, value }` pair, and a sequencing rule at each expression's
existing compile site splices that prelude into whichever frame already
calls `verify()` — the same per-construct verification this ADR's Addendum
7 described, now also seeing `Bind`s nested inside expression position, not
only ones at statement-top-level. See `docs/development/debugging.md`'s
`StateEffectEscapesExpression` row and "Emission-input coverage" paragraph,
and ADR 0118 itself, for the full design and its own final ≤3% measurement
against the pre-epic baseline.
