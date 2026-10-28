// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Boolean conditional compilation with field mutation state threading.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! When `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:` is used inside an
//! actor method and the block argument(s) contain field mutations
//! (`self.slot :=`), the compiler generates inline case expressions
//! that thread actor state correctly through both branches.
//!
//! Without this, mutations inside `ifTrue:` blocks are lost because
//! the block executes as a Tier 1 closure: the mutated state is bound
//! as a local let-variable inside the closure but never returned to
//! the enclosing actor method's `handle_call`.
//!
//! # Generated Pattern
//!
//! `flag ifTrue: [self.count := self.count + 1]` generates:
//!
//! ```erlang
//! let _Cond1 = flag in
//! case _Cond1 of
//!   <'true'> when 'true' ->
//!     let StateAcc = State0 in
//!     let _Val1 = call 'erlang':'+'(call 'maps':'get'('count', StateAcc), 1) in
//!     let StateAcc1 = call 'maps':'put'('count', _Val1, StateAcc) in
//!     {_Val1, StateAcc1}
//!   <'false'> when 'true' ->
//!     {'nil', State0}
//!   <_CondNoMatch2> when 'true' ->
//!     call 'erlang':'error'({'case_clause', _CondNoMatch2})
//! end
//! ```
//!
//! The caller (method body generator) unpacks `{Result, NewState}` via
//! `element/2` to thread the new state to subsequent expressions.
//!
//! The trailing wildcard clause is unreachable at runtime — `flag`
//! is always a genuine boolean here — but is required to make the `case`
//! *statically* exhaustive to the Core Erlang compiler; see
//! `CoreErlangGenerator::case_clause_fallback`'s doc comment for why an
//! implicit fallback isn't good enough when this `case` is nested inside a
//! `try`'s protected region (`on:do:`/`ensure:`).
//!
//! # State Naming
//!
//! Branch bodies use `StateAcc` / `StateAcc{N}` (loop-body naming) so
//! that inner state variable names do not conflict with the outer
//! `State{N}` chain managed by the method body generator.

use super::super::gen_server::BodyExprKind;
use super::super::patterns::MatchThreading;
use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{
    self, BindOp, FrameId, RebindFrame, RebindFrameKind, ThreadedStmt, ThreadedValue,
    ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use super::StateAccFallbackReason;
use super::analysis::ThreadedFamilies;
use super::family_slots::{self, FamilyVersionStep};
use crate::core_erlang::threading_analysis::ThreadedLocals;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, MatchArm, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::match_arm_block;
use beamtalk_core::source_analysis::Span;

// ─── FieldWriteSite / Closure ────────────────────────────────────
//
// `self.field := value` emission existed three times before this issue —
// `expressions.rs`'s `generate_field_assignment` (a complete, "closed"
// `Document`), `dispatch_codegen.rs`'s `generate_field_assignment_open` (an
// "open" `Document`, caller supplies the continuation), and this module's own
// `lower_field_assignment_bind` (pushes a real, un-rendered `ThreadedStmt::Bind`
// into a `Vec<ThreadedStmt>` body under construction) — one axis (which
// storage family: `State`/`Self`) encoded ad hoc, differently, in
// each; the other (whether the caller wants a self-contained expression or an
// open one) not named at all. `FieldWriteSite`/`Closure` name both axes once;
// [`CoreErlangGenerator::lower_field_write`] is the single lowering core all
// three sites above now call through (directly, or via
// [`CoreErlangGenerator::lower_simple_field_write_bind`] for the
// un-rendered-`Bind` consumption style). Missing the `ValueType` arm on the
// "open"/threaded shapes was this bug family's root
// cause (ADR 0110/0111): a value-type field write reaching either shape
// silently threaded through the actor `State`/`StateAcc` map — a variable
// that does not exist in a value-type method — instead of `Self`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum FieldWriteSite {
    /// Actor/instance field: threads through `State`/`StateAcc` (ADR 0043).
    Actor,
    /// Value-type field: threads through `Self`.
    ValueType,
}

impl FieldWriteSite {
    /// The `Actor`/`ValueType` split every plain (non-class-var)
    /// `self.field := value` write site shares — `ValueType` in
    /// `CodeGenContext::ValueType`, `Actor` otherwise (also covers
    /// `CodeGenContext::Repl`, matching every pre-existing call site's
    /// implicit default). A class-variable write (ADR 0130 §2) is not a
    /// storage family at all: a caller that may be in a class method decides
    /// that axis itself (`in_class_method()`) and lowers it as a plain
    /// expression before falling back to this, since `context` alone can't
    /// distinguish a class method's write from an ordinary instance write (a
    /// class method's own `context` is `Actor`, not a third variant).
    ///
    /// The single implementation behind what were three independent
    /// `if matches!(context, CodeGenContext::ValueType) { .. } else { .. }`
    /// copies (`expressions.rs`'s `generate_field_assignment`,
    /// `dispatch_codegen.rs`'s `generate_field_assignment_open`, and this
    /// module's own `lower_field_assignment_bind`) — CLAUDE.md's
    /// no-duplicate-implementations rule applied to the very axis this
    /// issue unifies.
    pub(in crate::core_erlang) fn for_context(context: CodeGenContext) -> Self {
        if matches!(context, CodeGenContext::ValueType) {
            Self::ValueType
        } else {
            Self::Actor
        }
    }
}

/// Whether a [`CoreErlangGenerator::lower_field_write`] caller wants the
/// mutation as a fully self-contained expression (`Closed` —
/// `generate_field_assignment`'s historical shape: the assigned value is the
/// `Document`'s own trailing result) or wants the `Bind`'s render left open
/// for the caller's own continuation glue (`Open` —
/// `generate_field_assignment_open`'s historical shape: the `Document` ends
/// with `"in "`, no trailing value). Orthogonal to [`FieldWriteSite`] — every
/// site supports both. Both compile the RHS the same way, via plain
/// `expression_doc` (BT-3986 removed `Open`'s Tier-2 stateful-block RHS
/// special case: a block that writes a field cannot be stored in a field).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum Closure {
    /// Caller supplies the continuation; the returned `Document` ends with
    /// `"in "`, no trailing value.
    Open,
    /// The returned `Document` is a complete expression whose value is the
    /// field write's assigned value (Smalltalk `:=` semantics).
    Closed,
}

impl FieldWriteSite {
    /// The label [`CoreErlangGenerator::check_simple_field_bind_invariant`]
    /// reports on a verify failure — cosmetic only (diagnostic text, never
    /// emitted Core Erlang), kept distinct per (site, closure) purely to
    /// match each historical call site's own wording.
    fn invariant_label(self, closure: Closure) -> &'static str {
        match (self, closure) {
            (Self::Actor, Closure::Closed) => "actor State field-assignment version bind",
            (Self::Actor, Closure::Open) => "actor State open field-assignment version bind",
            (Self::ValueType, Closure::Closed) => "value-type Self field-assignment version bind",
            (Self::ValueType, Closure::Open) => {
                "value-type Self open field-assignment version bind"
            }
        }
    }
}

impl CoreErlangGenerator {
    /// builds the real, un-rendered `Bind` (plus its `"let Val =
    /// <value> in "` preamble) for an `Actor`/`ValueType` field write.
    /// `span` is the constructed `Bind`'s own span (only ever observed by a
    /// `verify()` diagnostic on the node, never by rendering); callers that
    /// already have a `ThreadedStmt`-level span (this module's
    /// `lower_field_assignment_bind`) pass that one, matching its own
    /// hand-rolled `Bind` exactly, while [`Self::lower_field_write`] passes
    /// `value.span()`, matching `generate_field_assignment`'s/
    /// `generate_field_assignment_open`'s pre-existing
    /// `check_simple_field_bind_invariant` call (which always used
    /// `value.span()`, never the whole assignment's).
    pub(in crate::core_erlang) fn lower_simple_field_write_bind(
        &mut self,
        site: FieldWriteSite,
        closure: Closure,
        field_name: &str,
        value: &Expression,
        frame: FrameId,
        span: Span,
    ) -> Result<(Document<'static>, ThreadedStmt, String)> {
        let prefix = match site {
            FieldWriteSite::ValueType => VersionPrefix::SelfVt,
            FieldWriteSite::Actor => VersionPrefix::State,
        };
        let val_var = self.fresh_temp_var("Val");
        // Capture the source version BEFORE generating the value expression —
        // the RHS may itself read the field being assigned (`self.x := self.x
        // + 1`) and must see the pre-assignment snapshot.
        let (prelude_doc, value_doc, source_version) =
            if site == FieldWriteSite::Actor && self.is_sync_site(value.unwrap_parens()) {
                // ADR 0137: the assigned value is a call that may run user code. It is
                // bracketed (its prelude steps `State`), and the write goes on the
                // version the reload produced. The prelude is rendered ahead of the
                // `Val` binding, in the same statement sequence as the `Bind`.
                let tv = self.threaded_expression(value, frame)?;
                let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                (prelude_doc, value_doc, self.state_version())
            } else {
                let source_version = match site {
                    FieldWriteSite::ValueType => self.self_version(),
                    FieldWriteSite::Actor => self.state_version(),
                };
                (Document::Nil, self.expression_doc(value)?, source_version)
            };
        let target_version = match site {
            FieldWriteSite::ValueType => {
                self.next_self_var();
                self.self_version()
            }
            FieldWriteSite::Actor => {
                self.next_state_var();
                self.state_version()
            }
        };
        // isolated-verify this mint against a synthetic,
        // backfilled fixture — see the helper's own doc comment.
        self.check_simple_field_bind_invariant(
            prefix.clone(),
            source_version,
            target_version,
            site.invariant_label(closure),
            value.span(),
        );
        let preamble = docvec![
            prelude_doc,
            "let ",
            leaf::var(val_var.clone()),
            " = ",
            value_doc,
            " in ",
        ];
        let bind = ThreadedStmt::Bind {
            target: VersionedVar::new(prefix.clone(), target_version, frame),
            source: VersionedVar::new(prefix, source_version, frame),
            op: BindOp::Put {
                field: field_name.to_string(),
                value: ValueRef::Var(val_var.clone()),
            },
            span,
        };
        Ok((preamble, bind, val_var))
    }

    /// the single lowering core for `self.field := value`,
    /// parameterized over [`FieldWriteSite`] (which storage family) and
    /// [`Closure`] (self-contained vs. open `Document`) — see both enums'
    /// doc comments for the full contract. Always constructs the real
    /// [`ThreadedStmt::Bind`] and renders it through the same
    /// [`threaded_ir::render`] every spliced `Bind` goes through (never a
    /// second, hand-rolled `maps:put` fragment), so a `Closed` result is
    /// byte-identical to `generate_field_assignment`'s original hand-built
    /// `Document` and an `Open` result is byte-identical to
    /// `generate_field_assignment_open`'s.
    ///
    /// `frame` is the constructed `Bind`'s real [`FrameId`] — the caller's
    /// own `current_branch_frame()` for a loop/fold-body class-var mutation
    /// (see `lower_class_var_field_assignment_bind`'s doc comment), or
    /// [`FrameId::ROOT`] otherwise; `Actor`/`ValueType` writes reached
    /// through this function are always rendered immediately (never spliced
    /// into a larger, independently-verified `ThreadedIr` tree), so `frame`
    /// is inert for them.
    ///
    /// BT-3803: a `Closed` write is reported when it mints a version `Bind`
    /// (see [`Self::render_field_write`], which renders without checking).
    pub(in crate::core_erlang) fn lower_field_write(
        &mut self,
        site: FieldWriteSite,
        closure: Closure,
        field_name: &str,
        value: &Expression,
        frame: FrameId,
    ) -> Result<(Document<'static>, String)> {
        let span = value.span();
        let (doc, val_var, bind) =
            self.render_field_write(site, closure, field_name, value, frame)?;
        if closure == Closure::Closed {
            // BT-3803: a `Closed` write is a self-contained document, so the
            // `State`/`Self` version its `Bind` mints is scoped away from the
            // reply and every later statement (`unbound variable 'State1'`).
            // A field assignment in value position is a producer that
            // `threaded_expression` splices (`lower_field_assignment_bind`);
            // reaching this closed rendering with a version `Bind` means a
            // consumer compiled it as a plain sub-expression instead.
            let escaped = threaded_ir::opaque_escapes(std::slice::from_ref(&bind), false);
            self.report_threaded_ir_verify_errors(
                &escaped,
                "field-assignment state rebind trapped in a closed expression",
                span,
            );
        }
        Ok((doc, val_var))
    }

    /// [`Self::lower_field_write`] without the closed-rendering check: the
    /// rendered write, its value variable and the real `Bind` it minted.
    /// Exists so the byte-identical contract of the `Closed` rendering stays
    /// directly testable (`expressions/tests.rs`) without tripping the
    /// verifier that reports it as an escape in production.
    pub(in crate::core_erlang) fn render_field_write(
        &mut self,
        site: FieldWriteSite,
        closure: Closure,
        field_name: &str,
        value: &Expression,
        frame: FrameId,
    ) -> Result<(Document<'static>, String, ThreadedStmt)> {
        let span = value.span();
        let (preamble, bind, val_var) =
            self.lower_simple_field_write_bind(site, closure, field_name, value, frame, span)?;
        let bind_doc = {
            let mut ctx = threaded_ir::RenderCtx::new(self);
            threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
        };
        let doc = docvec![preamble, bind_doc];
        let doc = match closure {
            Closure::Open => doc,
            Closure::Closed => docvec![doc, leaf::var(val_var.clone())],
        };
        Ok((doc, val_var, bind))
    }
}

/// The ADR 0131 §2 `BranchArm` frame of an arm of a conditional that is a
/// local-threading producer in a class or value-type method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ArmFrame {
    /// The locals the construct threads: the keys its seeded `StateAcc`
    /// carries ([`CoreErlangGenerator::branch_threaded_locals`] over every arm).
    pub(in crate::core_erlang) threads: Vec<String>,
}

/// BT-4070: what one arm of a pure-inline boolean `case`
/// ([`CoreErlangGenerator::generate_pure_boolean_case`]) answers.
#[derive(Debug, Clone, Copy)]
pub(in crate::core_erlang) enum PureArm<'b> {
    /// The block's body compiled in place as the arm's plain value.
    Inline(&'b Block),
    /// The receiver itself (`C`) — the non-taken solo-branch contract
    /// (`False>>ifTrue: => self`, `True>>ifFalse: => self`).
    Receiver,
    /// A boolean literal atom — `and:`'s/`or:`'s short-circuit arm
    /// (`Boolean>>and:`/`or:`, `stdlib/src/boolean.bt`). Stored as a
    /// `bool` and emitted via `leaf::atom` (typed leaves, ADR 0089).
    Literal(bool),
}

/// Where a conditional's scratch map — element 2 of its result tuple, the
/// `StateAcc` its arms thread their locals through — starts (ADR 0131 §3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ScratchStart {
    /// Continue the enclosing scratch map: the actor `State` (an actor
    /// instance method, where element 2 is adopted as the next `State`
    /// version), or the `StateAcc` of the enclosing loop or arm frame.
    Continue,
    /// A fresh `maps:new()` seeded with only the construct's own threaded
    /// locals: the root of a class or value-type method, which has no `State`
    /// to continue.
    Fresh,
}

/// A local-threading producer's scratch seed
/// ([`CoreErlangGenerator::producer_scratch_seed`]).
pub(in crate::core_erlang) struct ProducerSeed {
    /// The `let SeededState = ... in` binding (`Document::Nil` when nothing is
    /// seeded).
    pub(in crate::core_erlang) doc: Document<'static>,
    /// The variable every arm's scratch map starts from.
    pub(in crate::core_erlang) base_state: String,
    /// The `State` version `base_state` continues, when it continues one.
    pub(in crate::core_erlang) base: Option<VersionedVar>,
}

impl CoreErlangGenerator {
    /// Seeds the `__local__` keys for the outer locals a conditional's
    /// branches thread, returning `(seed_doc, base_state_var)`.
    ///
    /// Each branch — including the synthetic non-taken branch (`{'nil', State}`)
    /// and any branch that does not itself write a given local — must return a
    /// state map that already contains every threaded `__local__` key, so the
    /// method-body sequencer's `maps:get/2` extraction never hits a missing key.
    /// Seeding the keys from the locals' current (pre-conditional) bindings before
    /// the `case` makes the base state self-consistent; taken branches simply
    /// overwrite the seeded value via `maps:put`.
    ///
    /// When there are no threaded locals, returns `(Document::Nil, outer_state)`
    /// so field-only conditionals emit byte-for-byte the same code as before.
    ///
    /// also reused by `exception_handling.rs`'s `on:do:`/`ensure:`
    /// generators — a try (receiver) block and its handler/cleanup block(s) are
    /// the same "compiled-but-not-all-taken" shape as a conditional's branches,
    /// so they share this seeding helper rather than re-deriving it.
    pub(in crate::core_erlang::control_flow) fn seed_conditional_locals(
        &mut self,
        blocks: &[&Block],
        outer_state: &str,
    ) -> (Document<'static>, String) {
        self.seed_scratch_locals(blocks, ScratchStart::Continue, outer_state)
    }

    /// [`Self::seed_conditional_locals`] with the scratch map's start chosen
    /// by `scratch` (ADR 0131 §3): [`ScratchStart::Continue`] seeds the keys
    /// into `outer_state` (the actor `State`, or the enclosing frame's
    /// `StateAcc`); [`ScratchStart::Fresh`] seeds them into a new empty map,
    /// so the construct's result tuple carries only its own threaded locals
    /// and no `State` variable needs to exist (a class method, a value-type
    /// method).
    pub(in crate::core_erlang::control_flow) fn seed_scratch_locals(
        &mut self,
        blocks: &[&Block],
        scratch: ScratchStart,
        outer_state: &str,
    ) -> (Document<'static>, String) {
        let threaded = self.branch_threaded_locals(blocks);
        if threaded.is_empty() && scratch == ScratchStart::Continue {
            return (Document::Nil, outer_state.to_string());
        }

        let seeded_state = self.fresh_temp_var("SeededState");
        let mut chain: Document<'static> = match scratch {
            ScratchStart::Continue => leaf::var(outer_state.to_string()),
            // A plain closure body in an actor method starts from the `State` it
            // captured (BT-3912): its arms self-send and read fields on it.
            ScratchStart::Fresh if self.closure_scratch_continues_state(self.current_frame()) => {
                leaf::var(self.current_field_read_state_var())
            }
            ScratchStart::Fresh => Document::Str("call 'maps':'new'()"),
        };
        // Fold from the last var inward so the emitted nesting reads naturally.
        for var in threaded.iter().rev() {
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            chain = docvec![
                "call 'maps':'put'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(core_var),
                ", ",
                chain,
                ")",
            ];
        }
        let seed_doc = docvec![
            "let ",
            leaf::var(seeded_state.clone()),
            " = ",
            chain,
            " in "
        ];
        (seed_doc, seeded_state)
    }

    /// BT-4206: [`Self::seed_scratch_locals`] for a local-threading producer
    /// whose operands may splice a prelude into its frame (`match:`, a
    /// lookup), seeding from the frame's current `State` and recording, in
    /// [`ProducerSeed::base`], the `State` version that seed reads — the
    /// input of the opaque tuple the verifier checks against the steps
    /// spliced ahead of it ([`threaded_ir::VerifyError::StaleConstructSeed`]).
    /// `base` is `None` where the scratch map does not continue the `State`
    /// family (a fresh map; a frame with no `State` family).
    pub(in crate::core_erlang::control_flow) fn producer_scratch_seed(
        &mut self,
        blocks: &[&Block],
        scratch: ScratchStart,
        frame: FrameId,
    ) -> ProducerSeed {
        let continues_state = scratch == ScratchStart::Continue
            && !self.conditional_families(frame).as_slice().is_empty();
        let base = continues_state
            .then(|| VersionedVar::new(VersionPrefix::State, self.state_version(), frame));
        let outer_state = self.current_state_var();
        let (doc, base_state) = self.seed_scratch_locals(blocks, scratch, &outer_state);
        ProducerSeed {
            doc,
            base_state,
            base,
        }
    }

    /// ADR 0137: whether a local-threading producer's operand `preludes` (a
    /// `match:` scrutinee's; a lookup's receiver and key) are spliced into
    /// the producer's own frame, ahead of its construct tuple, so `verify()`
    /// sees their `Bind`s — in an actor instance method, when any is
    /// non-empty — rather than rendered inside the tuple's document. The one
    /// placement rule for every such producer.
    pub(in crate::core_erlang::control_flow) fn splices_operand_preludes(
        &self,
        preludes: &[&[ThreadedStmt]],
    ) -> bool {
        self.synced_calls_active() && preludes.iter().any(|p| !p.is_empty())
    }

    /// Finishes a local-threading producer whose operands spliced `head` into
    /// its frame ([`Self::splices_operand_preludes`]): `head` runs ahead of
    /// the construct tuple, and the tuple records the `State` version its
    /// scratch seed read (`scratch_base`, BT-4206) so the verifier can check
    /// it against `head`'s steps.
    pub(in crate::core_erlang::control_flow) fn with_producer_head(
        mut head: Vec<ThreadedStmt>,
        scratch_base: Option<VersionedVar>,
        mut tv: ThreadedValue,
    ) -> ThreadedValue {
        if let Some(ThreadedStmt::ConstructTuple {
            scratch_base: slot, ..
        }) = tv
            .prelude
            .iter_mut()
            .find(|stmt| matches!(stmt, ThreadedStmt::ConstructTuple { .. }))
        {
            *slot = scratch_base;
        }
        if !head.is_empty() {
            head.append(&mut tv.prelude);
            tv.prelude = head;
        }
        tv
    }

    /// ADR 0118 phase 2a: compiles a
    /// conditional/`ifNotNil:` receiver, returning a [`ThreadedValue`] —
    /// `prelude` is any real `Bind`/`Statement` sequence that must run
    /// BEFORE the `case`'s condition binding (so an actor self-send's new
    /// `State`/`StateAcc` stays in scope inside the `case`), and `value` is the receiver's own
    /// boolean/nil value to test. Shared by all six `generate_*_with_mutations`
    /// generators below (previously each hand-duplicated this exact match —
    /// CLAUDE.md's no-duplicate-implementations rule). `frame` is the
    /// caller's own current frame ([`CoreErlangGenerator::current_frame`]),
    /// tagging every real `Bind` this prelude carries with the SAME frame
    /// identity the caller's own `case` construct uses.
    ///
    /// The six callers render `prelude` via
    /// [`CoreErlangGenerator::threaded_prelude_doc`] rather than
    /// `render_branch_arm`'s own wrap-and-`verify()`: unlike a
    /// branch arm (whose `state_version` `enter_branch_context` always
    /// resets to 0 before it runs), a conditional's receiver compiles at
    /// the CALLER's current, possibly non-zero state version — its
    /// threaded-ahead `Bind`'s own SOURCE routinely references a version some
    /// earlier, unrelated statement already produced, which
    /// [`threaded_ir::verify`] run on `prelude` alone (with no visibility
    /// into that earlier statement) reports as a false `UnboundVersion`/
    /// `NonLinearVersion` — confirmed by a `just test-bunit` panic on
    /// `generate_if_true_if_false_with_mutations` during this migration.
    /// `prelude`'s `Bind`s still render through the exact same
    /// `render`/`render_value` production every SPLICED prelude uses
    /// (`threaded_prelude_doc` and `render_branch_arm` share
    /// that renderer), so this is a real, correctly-shaped `ThreadedIr`
    /// prelude — just one this phase cannot re-verify in isolation; full
    /// verification arrives with ADR 0118 phase 4, when a mutation-threaded
    /// conditional becomes a producer with a real enclosing frame to splice
    /// into instead of the six callers' own opaque `Document` return.
    ///
    /// Two receiver shapes thread state through this position:
    /// - ADR 0130: a class-method self-send (or sub-expression containing
    ///   one) is recognized by `threaded_expression` itself
    ///   (`compile_conditional_receiver` delegates to it directly) and bound
    ///   in an ordered `let` of its own — it threads nothing, but its
    ///   evaluation order against the condition is pinned.
    /// - ADR 0118 phase 4: an ACTOR-INSTANCE
    ///   self-send anywhere in the receiver's sequenceable sub-tree — the
    ///   receiver itself (`(self recordOnce: which) ifTrue:ifFalse:`),
    ///   nested inside it (as the receiver of an `and:`/`or:`),
    ///   OR the receiver being itself a nested inline-threaded
    ///   construct (`((self recordOnce: which) and: [x]) ifTrue:`; since
    ///   `and:`/`or:`/the nil-conditional family/`match:` producers are
    ///   recognized directly by [`CoreErlangGenerator::threaded_expression`],
    ///   this no longer needs its own `and:`/`or:` special case) — is
    ///   threaded ahead of the receiver expression by [`Self::thread_ahead`]
    ///   into real `Bind`s (the sequencing rule), so the receiver expression
    ///   itself compiles to a plain reference to the already-threaded
    ///   result. Nothing in a conditional evaluates before its receiver, so
    ///   this position is always order-safe to thread ahead into. The
    ///   inlining decision that gets us here (`try_generate_boolean_protocol`,
    ///   and `classify_body_expr`'s `control_flow_has_mutations`) uses the
    ///   same underlying probe
    ///   ([`CoreErlangGenerator::conditional_receiver_needs_threading`]) so
    ///   the two can never disagree.
    fn compile_conditional_receiver(
        &mut self,
        receiver: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        // ADR 0118 phase 5b: `threaded_expression` now recognizes
        // every producer this function used to handle by hand (an Actor
        // self-send via the sequencing rule, AND — since `subexpr_needs_prelude`
        // gained the class-var-producer check this issue adds — a
        // same-class self-send/class-var assignment too), so this is a
        // plain delegation. The two-call dance this replaced
        // (`thread_ahead` then a second `generate_expression`-reaching
        // compile) would otherwise double-dispatch a class-var receiver:
        // `thread_ahead` now threads it, so a second compile must not
        // re-run its producer a second time.
        self.threaded_expression(receiver, frame)
    }

    /// Whether an arm statement `expr` with mutations is lowered through
    /// [`Self::threaded_expression`], its prelude spliced into the arm
    /// (`arm_is_frame`: the arm is a `BranchArm` frame of a producer), rather
    /// than as a raw `{Value, State}` tuple the arm unpacks: a local-threading
    /// producer of a `BranchArm` frame or a REPL arm (ADR 0131 Phase 3,
    /// BT-3823), and (BT-4000) a conditional whose receiver is sequenced ahead
    /// ([`Self::conditional_receiver_is_sequenced`]) in any arm, so the
    /// receiver's `Bind`s are the arm's own statements. The one predicate for
    /// the conditional arm and the `on:do:`/`ensure:` arm.
    pub(in crate::core_erlang::control_flow) fn arm_statement_is_producer(
        &self,
        expr: &Expression,
        arm_is_frame: bool,
    ) -> bool {
        ((arm_is_frame || self.is_repl_mode()) && self.local_producer_locals(expr).is_some())
            || self.is_conditional_with_sequenced_receiver(expr)
    }

    /// Whether a conditional in `frame` is lowered as a local-threading
    /// producer: it is one (`producer`, [`Self::conditional_producer_locals`]),
    /// `frame` is the method root or a registered frame (a `BranchArm`, or a loop
    /// or fold body, where the conditional is one of the body's producer sites),
    /// and it is not the REPL. The one predicate for both the arm frames and the
    /// `LocalRebind`s.
    fn lowers_as_conditional_producer(&self, frame: FrameId, producer: bool) -> bool {
        producer
            && (frame == FrameId::ROOT || self.rebind_frames.contains_key(&frame))
            && !self.is_repl_mode()
    }

    /// The `BranchArm` frame the arms of a conditional producer are lowered
    /// as (ADR 0131 §2): the construct's seeded set, for a conditional
    /// producer at the method root, in another conditional producer's arm (a
    /// registered `BranchArm` frame) or in a loop or fold body (ADR 0131 Phase 3c).
    /// `producer` says whether the construct is one
    /// ([`Self::conditional_producer_locals`]): a conditional the Phase 0 check
    /// does not name as a producer keeps the legacy arm. The arms in the REPL
    /// are not `BranchArm` frames either: they are REPL arms
    /// ([`Self::generate_repl_arm_inline`], BT-3823), whose producers adopt the
    /// bindings map.
    pub(in crate::core_erlang::control_flow) fn conditional_arm_frame(
        &self,
        blocks: &[&Block],
        producer: bool,
    ) -> Option<ArmFrame> {
        self.lowers_as_conditional_producer(self.current_frame(), producer)
            .then(|| ArmFrame {
                threads: self.branch_threaded_locals(blocks),
            })
    }

    /// One arm of a conditional producer: a `BranchArm` frame
    /// ([`Self::generate_conditional_arm`]) when `arm` is given, else the
    /// legacy arm.
    pub(in crate::core_erlang::control_flow) fn conditional_arm(
        &mut self,
        block: &Block,
        arm: Option<&ArmFrame>,
    ) -> Result<(Document<'static>, usize)> {
        match arm {
            Some(arm) => self.generate_conditional_arm(block, arm),
            None => self.generate_conditional_branch_inline(block),
        }
    }

    /// The case-clause body of one arm of a conditional or lookup producer:
    /// `let StateAcc = <base_state> in <arm>` ([`Self::conditional_arm`] in a
    /// branch context). When `param_value` is given, the block's parameter
    /// (`ifNotNil: [:v | ...]`, `at:ifPresent: [:v | ...]`) is bound to that
    /// Core Erlang variable inside the arm.
    pub(in crate::core_erlang) fn conditional_arm_clause(
        &mut self,
        block: &Block,
        param_value: Option<&str>,
        arm: Option<&ArmFrame>,
        base_state: &str,
    ) -> Result<Document<'static>> {
        let (branch_doc, _branch_final) = self.with_branch_context(|this| {
            let Some(value) = param_value else {
                return this.conditional_arm(block, arm);
            };
            // A scope so the parameter binding is cleaned up after generation.
            this.push_scope();
            if let Some(param) = block.parameters.first() {
                this.bind_var(&param.name, value);
            }
            let result = this.conditional_arm(block, arm);
            this.pop_scope();
            result
        })?;
        Ok(docvec![
            "let StateAcc = ",
            leaf::var(base_state.to_string()),
            " in ",
            branch_doc,
        ])
    }

    /// ADR 0131 §3: whether a conditional producer in `frame` keeps its scratch
    /// map private. True at the method root of a method that has no `State`
    /// family to continue (a class method, a value-type method): the
    /// construct's result tuple is `{Value, Scratch}` where `Scratch` is a
    /// fresh map seeded from `maps:new()` with only the construct's own
    /// threaded locals, and its locals come back through `LocalRebind`s. No
    /// `State` variable is referenced, because none exists there.
    ///
    /// Not private: an actor instance method's root (`State` is an eligible
    /// family, [`Self::eligible_families`]: element 2 *is* the next `State`
    /// version), the REPL (its `State` is the bindings map), a `StateAcc` or
    /// hybrid loop or fold frame (the construct continues the frame's own
    /// `StateAcc`), and a registered `BranchArm` frame (it continues the arm's
    /// `StateAcc`).
    ///
    /// ADR 0131 Phase 3c (BT-3822): also private in a direct-params or
    /// tuple-acc loop or fold frame, whose body has no `StateAcc` (its locals
    /// are the loop's parameters, `RebindFrameKind::extends_state_chain`):
    /// there is no map to continue, exactly as for an `on:do:`/`ensure:`
    /// producer in that frame (`exception_body_outer_state`).
    pub(in crate::core_erlang) fn conditional_has_private_scratch(&self, frame: FrameId) -> bool {
        if self.is_repl_mode() {
            return false;
        }
        if frame == FrameId::ROOT {
            return !self.eligible_families().contains(&VersionPrefix::State);
        }
        self.rebind_frames.get(&frame).is_some_and(|rebind| {
            matches!(
                rebind.kind,
                RebindFrameKind::Loop(ThreadingMode::DirectParams | ThreadingMode::TupleAcc(_))
            )
        })
    }

    /// Where a conditional producer in `frame` starts its scratch map.
    pub(in crate::core_erlang) fn conditional_scratch(&self, frame: FrameId) -> ScratchStart {
        if self.conditional_has_private_scratch(frame) {
            ScratchStart::Fresh
        } else {
            ScratchStart::Continue
        }
    }

    /// ADR 0122 Decision 2 / ADR 0131 §3: the families a conditional's result
    /// tuple hands back to `frame`, taken from context
    /// ([`Self::eligible_families`]) rather than hard-coded: `[State]` in an
    /// actor instance method (and in a non-root frame, which continues its
    /// own scratch map, see [`Self::conditional_has_private_scratch`]), and
    /// none at the root of a class method or a value-type method. A value-type
    /// `SelfVt` family is not carried by THIS tuple: a conditional whose arms
    /// write a field is not a local-threading producer
    /// (`beamtalk_core::semantic_analysis::block_facts::conditional_arms_lower_as_producer`)
    /// but has its own producer, `vt_conditional_value_producer` (BT-3821),
    /// whose tuple carries the arms' value, outer locals and `Self`
    /// (`lower_vt_conditional`).
    fn conditional_families(&self, frame: FrameId) -> ThreadedFamilies {
        if self.conditional_has_private_scratch(frame) {
            ThreadedFamilies::default()
        } else {
            ThreadedFamilies::from_matches(&[VersionPrefix::State])
        }
    }

    /// The slot list of a conditional's result tuple past the value: always
    /// the scratch map at element 2 (ADR 0122 §"State already fits"), whether
    /// the context adopts it as the `State` family or keeps it private.
    fn conditional_result_slots() -> ThreadedFamilies {
        ThreadedFamilies::from_matches(&[VersionPrefix::State])
    }

    /// ADR 0118 phase 4: wraps a `_tuple` builder's raw
    /// `{Value, NewState}` case-expression `Document` — the shape every
    /// `generate_*_with_mutations_tuple` builder below still produces
    /// internally — as a [`ThreadedValue`] per §Decision 2: prelude
    /// `[Statement(let CF = <case…> in), Bind(State_{n+1} ← element(2,
    /// CF))]`, value `element(1, CF)`.
    ///
    /// ADR 0131 §1/§2 (Phase 3, 3c): at the method root, in a registered
    /// `BranchArm` frame or in a loop or fold body the prelude also carries one
    /// `LocalRebind` per local the construct threads (`arms`' threaded set),
    /// lowered by that frame: the construct is a local-threading producer. Where
    /// the scratch map is private ([`Self::conditional_has_private_scratch`]:
    /// the root of a class or value-type method, a direct-params or tuple-acc
    /// frame) there is no `State` `Bind`; in an actor instance method the
    /// `State` `Bind` comes first and each rebind reads through it. A
    /// conditional that is not a producer gets the legacy prelude.
    ///
    /// `frame` MUST be captured by the caller via
    /// [`CoreErlangGenerator::current_frame`] BEFORE building `tuple_doc` —
    /// never recomputed here. Building `tuple_doc` calls
    /// `generate_conditional_branch_inline` for the construct's own
    /// branch(es), each via `with_branch_context`, which mints a fresh
    /// `branch_frame_counter` value and never decrements it on exit (frame
    /// identity is globally unique per module, by design); recomputing
    /// `current_frame()` AFTER that would read the LAST inner branch's
    /// frame instead of this call's own enclosing frame, misattributing
    /// this wrapper's `Bind` and tripping the verifier
    /// (`NonLinearVersion`/`UnboundVersion` on a frame nothing else
    /// references).
    pub(in crate::core_erlang::control_flow) fn conditional_tuple_to_threaded_value(
        &mut self,
        tuple_doc: Document<'static>,
        frame: FrameId,
        span: Span,
        arms: &[&Block],
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let carrier = self.fresh_temp_var("CF");
        let value = ValueRef::Doc(docvec![
            "call 'erlang':'element'(1, ",
            leaf::var(carrier.clone()),
            ")",
        ]);
        // The method root and a registered frame (a `BranchArm`, a loop or fold
        // body) lower the construct's `LocalRebind`s.
        let rebinds_locals = self.lowers_as_conditional_producer(frame, producer);
        let locals = if rebinds_locals {
            self.branch_threaded_locals(arms)
        } else {
            Vec::new()
        };
        let family_binds = self.extract_state_family(&carrier, frame, span);
        if !rebinds_locals {
            let mut prelude = vec![ThreadedStmt::Statement(
                docvec!["let ", leaf::var(carrier.clone()), " = ", tuple_doc, " in "],
                span,
            )];
            prelude.extend(family_binds);
            return Ok(ThreadedValue { prelude, value });
        }
        let enclosing = self.rebind_frame_of(frame)?;
        let prelude = self.construct_rebind_prelude(
            &enclosing,
            &carrier,
            tuple_doc,
            &locals,
            tail,
            family_binds,
            frame,
            span,
        );
        Ok(ThreadedValue { prelude, value })
    }

    /// [`Self::conditional_tuple_to_threaded_value`] for a construct that
    /// threads no outer local of its own (`match:`, an ADR 0128 opaque fold):
    /// actor instance methods only, so its scratch map is always the `State`
    /// family.
    fn control_flow_tuple_to_threaded_value(
        &mut self,
        tuple_doc: Document<'static>,
        frame: FrameId,
        span: Span,
    ) -> Result<ThreadedValue> {
        if self.conditional_has_private_scratch(frame) {
            return Err(CodeGenError::Internal(
                "a match: or opaque-callable fold producer was reached in a frame with a \
                 private scratch map (no State family to continue; ADR 0131 §3)"
                    .to_string(),
            ));
        }
        self.conditional_tuple_to_threaded_value(
            tuple_doc,
            frame,
            span,
            &[],
            ProducerTail::Rebind,
            true,
        )
    }

    /// ADR 0122 Decision 3/BT-3514: this construct's post-merge `[State]`
    /// extraction — the `Bind` half the producer used to hand-roll
    /// (`State_{n+1} <- element(2, CF)`) — routed through the shared
    /// [`family_slots::extract_family_slots`] instead. `step` mints the
    /// family's fresh successor version FIRST (exactly as the hand-rolled
    /// version did: `next_state_var()` before reading `state_version()`),
    /// matching every other `extract_family_slots` caller's "caller mints
    /// `target` itself" contract. `base_arity` is always `1` here — every
    /// `_tuple` builder's own value occupies element 1, `State` is the sole
    /// family and always occupies element 2 (ADR 0122 §"State already fits":
    /// "element 2 of the same result tuple the other families are appended
    /// to").
    ///
    /// The families come from context ([`Self::conditional_families`]): none
    /// at the root of a class or value-type method, where no `State` version
    /// is minted at all.
    fn extract_state_family(
        &mut self,
        tuple_var: &str,
        frame: FrameId,
        span: Span,
    ) -> Vec<ThreadedStmt> {
        let families = self.conditional_families(frame);
        if families.as_slice().is_empty() {
            return Vec::new();
        }
        let source_version = self.state_version();
        let _ = self.next_state_var();
        let target_version = self.state_version();
        family_slots::extract_family_slots(
            tuple_var,
            1,
            &families,
            |_| {
                FamilyVersionStep::new(
                    VersionedVar::new(VersionPrefix::State, source_version, frame),
                    VersionedVar::new(VersionPrefix::State, target_version, frame),
                )
            },
            span,
        )
    }

    /// ADR 0122 Decision 3/BT-3514: this conditional's own
    /// non-taken-arm / absent-block-passthrough tuple — `{<value_doc>,
    /// <base_state>}` — through the shared
    /// [`family_slots::append_baseline_family_slots`] helper with `[State]`,
    /// rather than each of the six `generate_*_with_mutations` producers
    /// hand-rolling its own `"{lit, ", base_state, "}"` `Document` splice.
    /// Byte-identical to the hand-rolled shape: `append_family_slots`'s
    /// `base` here always carries one prior element (`value_doc`), so the
    /// helper always prepends `", "` before the (sole) family slot — the
    /// exact `"{'nil', "` + var + `"}"` splice every call site built by hand.
    ///
    /// `base_state` is a plain Core Erlang variable name, not necessarily a
    /// canonical `State{N}`/`StateAcc{N}` counter value —
    /// `seed_conditional_locals` mints a disconnected `SeededState` temp
    /// when outer locals are threaded through this conditional (a
    /// pre-minted, verbatim name, ADR 0111 Addendum 2's "naming-scheme
    /// mismatch" Gap 2) — so it renders through [`VersionPrefix::Gensym`],
    /// whose [`VersionedVar::render_name`] returns the stored name back out
    /// VERBATIM regardless of `version`, rather than through a numbered
    /// `VersionPrefix::State` [`VersionedVar`] (which would render via
    /// `RenderCtx`'s loop-context-dependent `State{N}`/`StateAcc{N}`
    /// counter naming instead of `base_state`'s own text). The slot LIST
    /// passed to the helper is [`Self::conditional_result_slots`]: element 2
    /// is the scratch map in every context.
    pub(in crate::core_erlang::control_flow) fn conditional_baseline_tuple(
        &mut self,
        value_doc: Document<'static>,
        base_state: &str,
    ) -> Document<'static> {
        let base = docvec!["{", value_doc];
        let slot = VersionedVar::new(
            VersionPrefix::Gensym(base_state.to_string()),
            0,
            FrameId::ROOT,
        );
        let ctx = threaded_ir::RenderCtx::new(self);
        family_slots::append_baseline_family_slots(
            base,
            &Self::conditional_result_slots(),
            |_| slot.clone(),
            &ctx,
        )
    }
}

impl CoreErlangGenerator {
    /// BT-3947 (closes BT-3875): the [`ThreadedValue`] producer for a `match:` that
    /// is a local-threading producer ([`CoreErlangGenerator::conditional_producer_locals`]):
    /// a conditional with one arm per `match:` arm. The scratch map is seeded with
    /// the locals its arms write (`seed_scratch_locals` over the arms as blocks,
    /// [`match_arm_block`]), each arm is a `BranchArm` frame carrying them, and the
    /// result tuple's `StateAcc` comes back through one `LocalRebind` per local,
    /// lowered by the enclosing frame (`conditional_tuple_to_threaded_value`).
    ///
    /// `frame` is captured before the arms are lowered, like every conditional
    /// producer's: the arms mint frames of their own.
    pub(in crate::core_erlang) fn generate_match_producer(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
        span: Span,
        tail: ProducerTail,
    ) -> Result<ThreadedValue> {
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let blocks: Vec<Block> = arms.iter().map(match_arm_block).collect();
        let block_refs: Vec<&Block> = blocks.iter().collect();
        // Test-only: the pre-BT-4196 ordering, seeding before the scrutinee
        // (`StaleConstructSeed`).
        #[cfg(test)]
        let early_seed = (super::super::threaded_expr::fault::active()
            == Some(super::super::threaded_expr::fault::LocalRebindFault::SeedBeforeScrutinee))
        .then(|| self.producer_scratch_seed(&block_refs, scratch, frame));
        #[cfg(not(test))]
        let early_seed: Option<ProducerSeed> = None;
        // BT-4196: the scrutinee compiles FIRST, so its State steps (a field
        // write, a bracket reload) are in the base State the scratch seed and
        // the arms thread from, like `ifTrue:`'s receiver and the lookup
        // producer's receiver/key.
        let recv_tv = self.compile_match_scrutinee(value)?;
        let value_doc = self.threaded_value_doc(&recv_tv.value);
        let splice = self.splices_operand_preludes(&[&recv_tv.prelude]);
        let (head, inline_prelude): (Vec<ThreadedStmt>, Document<'static>) = if splice {
            (recv_tv.prelude, Document::Nil)
        } else {
            (Vec::new(), self.threaded_prelude_doc(&recv_tv.prelude))
        };
        let seed = match early_seed {
            Some(seed) => seed,
            None => self.producer_scratch_seed(&block_refs, scratch, frame),
        };
        let arm_frame = self.conditional_arm_frame(&block_refs, true);
        let threading = MatchThreading {
            base_state: &seed.base_state,
            arm: arm_frame.as_ref(),
        };
        let match_doc = self.generate_match_after_scrutinee(arms, &value_doc, Some(&threading))?;
        let tuple_doc = docvec![inline_prelude, seed.doc, match_doc];
        let tv = self.conditional_tuple_to_threaded_value(
            tuple_doc,
            frame,
            span,
            &block_refs,
            tail,
            true,
        )?;
        Ok(Self::with_producer_head(head, seed.base, tv))
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// ifTrue: [block]` — the expression-position counterpart of
    /// [`Self::generate_if_true_with_mutations_tuple`], whose raw tuple
    /// `Document` this wraps via
    /// [`Self::control_flow_tuple_to_threaded_value`]. Called by
    /// [`CoreErlangGenerator::threaded_expression`]'s inline-control-flow
    /// producer branch and by [`super::super::threaded_expr`]'s Actor-body
    /// consumers — never by [`super::super::intrinsics::CoreErlangGenerator::try_generate_boolean_protocol`],
    /// which keeps calling the `_tuple` builder directly for its own
    /// Document-returning contract.
    pub(in crate::core_erlang) fn generate_if_true_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc =
            self.generate_if_true_with_mutations_tuple(receiver, block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// Generates inline code for `flag ifTrue: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{block_result, mutated_state}`
    /// - False branch: `{'nil', unchanged_state}`
    pub(in crate::core_erlang) fn generate_if_true_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so the non-taken (false) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);

        // ADR 0111 Addendum 5: `generate_conditional_branch_inline`
        // now builds, `verify()`s, and `render()`s this arm's real per-frame
        // `ThreadedIr` internally — the pre-migration
        // `check_branch_frame_linearity` scalar-synthesis scaffolding check
        // that used to run here is gone; `NonLinearVersion`/`UnboundVersion`
        // are live checks against the real IR now.
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        // explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");
        // ADR 0122 Decision 3/BT-3514: the non-taken (false) arm's
        // {'nil', <state>} tuple, through the shared family emission
        // helper with [State] rather than a hand-rolled splice.
        let false_tuple = self.conditional_baseline_tuple(Document::Str("'nil'"), &base_state);

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            branch_doc,
            " <'false'> when 'true' -> ",
            false_tuple,
            no_match_fallback,
            " end",
        ])
    }

    /// BT-3841: the [`ThreadedValue`] producer for an evaluated literal block
    /// (`[block] value`, `beamtalk_core`'s `literal_block_value_arm`) that is a
    /// local-threading producer: the degenerate conditional whose one arm always
    /// runs. It shares the conditional producer's scratch map, arm frame and
    /// tuple-to-`ThreadedValue` step, so the arm's locals come back through
    /// `LocalRebind`s lowered by the enclosing frame.
    pub(in crate::core_erlang) fn generate_block_value_with_mutations(
        &mut self,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_block_value_with_mutations_tuple(block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// The `{Result, NewState}` tuple of [`Self::generate_block_value_with_mutations`]:
    /// the seeded scratch map, then the arm. There is no `case`: the arm is
    /// taken every time.
    pub(in crate::core_erlang) fn generate_block_value_with_mutations_tuple(
        &mut self,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let outer_state = self.current_state_var();
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        Ok(docvec![
            seed_doc,
            "let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// ifFalse: [block]` — see [`Self::generate_if_true_with_mutations`]'s
    /// doc comment for the shape every one of these producers shares.
    pub(in crate::core_erlang) fn generate_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc =
            self.generate_if_false_with_mutations_tuple(receiver, block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// Generates inline code for `flag ifFalse: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{'nil', unchanged_state}`
    /// - False branch: `{block_result, mutated_state}`
    pub(in crate::core_erlang) fn generate_if_false_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so the non-taken (true) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);

        // ADR 0111 Addendum 5: see generate_if_true_with_mutations'
        // matching comment — real per-frame verify() now runs inside
        // generate_conditional_branch_inline itself.
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        // explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");
        // ADR 0122 Decision 3/BT-3514: the non-taken (true) arm's
        // {'nil', <state>} tuple, through the shared family emission
        // helper with [State] rather than a hand-rolled splice.
        let true_tuple = self.conditional_baseline_tuple(Document::Str("'nil'"), &base_state);

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_tuple,
            " <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// and: [block]` — see [`Self::generate_if_true_with_mutations`]'s doc
    /// comment for the shape every one of these producers shares.
    pub(in crate::core_erlang) fn generate_and_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc =
            self.generate_and_with_mutations_tuple(receiver, block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// generates inline code for `flag and: [block]` in actor
    /// context when the block contains field mutations (or the receiver is
    /// itself an actor self-send needing threading — see
    /// `compile_conditional_receiver`).
    ///
    /// Structurally identical to [`Self::generate_if_true_with_mutations`] —
    /// `and:` short-circuits to the *literal* `false` on the non-taken path
    /// rather than `ifTrue:`'s `nil`, matching `boolean.bt`'s own
    /// self-hosted definition (`and: aBlock => self ifTrue: aBlock ifFalse:
    /// [false]`), so the inlined fast path stays behaviorally identical to
    /// the generic-dispatch path it replaces.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{block_result, mutated_state}`
    /// - False branch: `{'false', unchanged_state}`
    pub(in crate::core_erlang) fn generate_and_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so the non-taken (false) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);

        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        // explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");
        // ADR 0122 Decision 3/BT-3514: the non-taken (false) arm's
        // {'false', <state>} tuple, through the shared family emission
        // helper with [State] rather than a hand-rolled splice.
        let false_tuple = self.conditional_baseline_tuple(Document::Str("'false'"), &base_state);

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            branch_doc,
            " <'false'> when 'true' -> ",
            false_tuple,
            no_match_fallback,
            " end",
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// or: [block]` — see [`Self::generate_if_true_with_mutations`]'s doc
    /// comment for the shape every one of these producers shares.
    pub(in crate::core_erlang) fn generate_or_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc =
            self.generate_or_with_mutations_tuple(receiver, block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// generates inline code for `flag or: [block]` in actor
    /// context when the block contains field mutations (or the receiver is
    /// itself an actor self-send needing threading).
    ///
    /// The mirror image of [`Self::generate_and_with_mutations`] — see its
    /// doc comment. `or:`'s non-taken (true) path short-circuits to the
    /// literal `true`, matching `boolean.bt`'s `or: aBlock => self ifTrue:
    /// [true] ifFalse: aBlock`.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{'true', unchanged_state}`
    /// - False branch: `{block_result, mutated_state}`
    pub(in crate::core_erlang) fn generate_or_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so the non-taken (true) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);

        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        // explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");
        // ADR 0122 Decision 3/BT-3514: the non-taken (true) arm's
        // {'true', <state>} tuple, through the shared family emission
        // helper with [State] rather than a hand-rolled splice.
        let true_tuple = self.conditional_baseline_tuple(Document::Str("'true'"), &base_state);

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_tuple,
            " <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// ifTrue: [t_block] ifFalse: [f_block]` — see
    /// [`Self::generate_if_true_with_mutations`]'s doc comment for the
    /// shape every one of these producers shares.
    pub(in crate::core_erlang) fn generate_if_true_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = false_block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_if_true_if_false_with_mutations_tuple(
            receiver,
            true_block,
            false_block,
            scratch,
            producer,
        )?;
        self.conditional_tuple_to_threaded_value(
            tuple_doc,
            frame,
            span,
            &[true_block, false_block],
            tail,
            producer,
        )
    }

    /// Generates inline code for `flag ifTrue: [t_block] ifFalse: [f_block]` in actor context
    /// when at least one block contains field mutations.
    ///
    /// Returns `{Result, NewState}` from whichever branch is taken.
    pub(in crate::core_erlang) fn generate_if_true_if_false_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so a branch that does not itself
        // write a given local (and the post-conditional extraction) still sees the
        // `__local__` key.
        let (seed_doc, base_state) =
            self.seed_scratch_locals(&[true_block, false_block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[true_block, false_block], producer);

        // True branch
        let (true_branch_doc, _true_final) =
            self.with_branch_context(|this| this.conditional_arm(true_block, arm_frame.as_ref()))?;

        // False branch (reset to same initial state)
        let (false_branch_doc, _false_final) =
            self.with_branch_context(|this| this.conditional_arm(false_block, arm_frame.as_ref()))?;

        // ADR 0111 Addendum 5: the true/false arms are sibling
        // with_branch_context frames — each `generate_conditional_branch_inline`
        // call mints its own fresh FrameId (`current_branch_frame`) and
        // `verify()`s its own real IR internally, so either arm
        // independently reaching the same version number as the other
        // (e.g. both perform exactly one field mutation, both producing
        // "StateAcc1" in their own frame) is correctly NOT a
        // NonLinearVersion violation — the check that used to run here
        // (`check_branch_frame_linearity`) is gone; real per-frame
        // verification now happens where the IR is actually built.
        // explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            true_branch_doc,
            " <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            false_branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for the
    /// nil-testing conditional family — `ifNil:` (`nil_block: Some`,
    /// `not_nil_block: None`), `ifNotNil:` (`nil_block: None`,
    /// `not_nil_block: Some`), and `ifNil:ifNotNil:`/`ifNotNil:ifNil:`
    /// (both `Some` — the caller passes them by MEANING, not by source
    /// keyword order, so this generator itself doesn't need to know which
    /// selector produced the call). Generalizes the single-block
    /// `ifNotNil:`-only generator this replaces (see
    /// [`Self::generate_if_true_with_mutations`]'s doc comment for the
    /// producer shape every one of these shares) so `ifNil:` and the
    /// two-block forms get the same inline-case treatment instead of
    /// falling through to a plain Tier 1 closure that discards a nested
    /// self-send's mutation.
    pub(in crate::core_erlang) fn generate_nil_conditional_with_mutations(
        &mut self,
        receiver: &Expression,
        nil_block: Option<&Block>,
        not_nil_block: Option<&Block>,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = not_nil_block
            .or(nil_block)
            .map_or_else(|| receiver.unwrap_parens().span(), |b| b.span);
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_nil_conditional_with_mutations_tuple(
            receiver,
            nil_block,
            not_nil_block,
            scratch,
            producer,
        )?;
        let arms: Vec<&Block> = nil_block.into_iter().chain(not_nil_block).collect();
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &arms, tail, producer)
    }

    /// Generates inline code for the nil-testing conditional family in
    /// actor context when a block contains field mutations. See
    /// [`Self::generate_nil_conditional_with_mutations`]'s doc comment for
    /// what `nil_block`/`not_nil_block` mean.
    ///
    /// Returns `{Result, NewState}`:
    /// - Nil branch: `nil_block`'s own `{value, state}` when present,
    ///   else `{'nil', unchanged_state}` (bare `ifNotNil:`).
    /// - Non-nil branch: `not_nil_block`'s own `{value, state}` when
    ///   present, else `{receiver_value, unchanged_state}` (bare `ifNil:`
    ///   — Smalltalk's "answers self when not nil" contract).
    ///
    /// If `not_nil_block` has a parameter (`:v`), it is bound to the
    /// receiver object value inside its branch body.
    pub(in crate::core_erlang) fn generate_nil_conditional_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        nil_block: Option<&Block>,
        not_nil_block: Option<&Block>,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        // The REPL assembler (and `lower_repl_construct_tuple` for an operand)
        // unpacks the `{Result, Bindings}` tuple this compiles to.
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let recv_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let recv_val_doc = self.threaded_value_doc(&recv_tv.value);
        let obj_var = self.fresh_temp_var("Obj");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so a branch that does not
        // itself write a given local (and the post-conditional extraction)
        // still sees the `__local__` key.
        let mut seed_blocks: Vec<&Block> = Vec::new();
        seed_blocks.extend(nil_block);
        seed_blocks.extend(not_nil_block);
        let (seed_doc, base_state) = self.seed_scratch_locals(&seed_blocks, scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&seed_blocks, producer);

        // ADR 0111 Addendum 5: see generate_if_true_with_mutations'
        // matching comment — real per-frame verify() now runs inside
        // generate_conditional_branch_inline itself.
        let nil_arm = if let Some(block) = nil_block {
            self.conditional_arm_clause(block, None, arm_frame.as_ref(), &base_state)?
        } else {
            // ADR 0122 Decision 3/BT-3514: through the shared family
            // emission helper with [State] rather than a hand-rolled splice.
            self.conditional_baseline_tuple(Document::Str("'nil'"), &base_state)
        };

        let not_nil_arm = if let Some(block) = not_nil_block {
            // The block parameter is bound to the receiver value (`obj_var`).
            self.conditional_arm_clause(block, Some(&obj_var), arm_frame.as_ref(), &base_state)?
        } else {
            // ADR 0122 Decision 3/BT-3514: through the shared family
            // emission helper with [State] rather than a hand-rolled splice.
            self.conditional_baseline_tuple(leaf::var(obj_var.clone()), &base_state)
        };

        Ok(docvec![
            recv_preamble,
            seed_doc,
            "let ",
            leaf::var(obj_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            leaf::var(obj_var),
            " of <'nil'> when 'true' -> ",
            nil_arm,
            " <_> when 'true' -> ",
            not_nil_arm,
            " end",
        ])
    }

    /// BT-4070: the pure-inline producer for `R ifTrue: [B]` — the
    /// expression-position counterpart of
    /// [`Self::generate_if_true_with_mutations_tuple`], but a plain value,
    /// not a `{Result, NewState}` tuple: the gate
    /// ([`Self::try_generate_pure_boolean_conditional`]) guarantees no block
    /// or receiver needs threading, so no state is threaded and no classifier
    /// change is needed (`control_flow_has_mutations` is false, the plain
    /// consumer path). Matches dynamic dispatch exactly: the non-taken
    /// (false) arm answers the receiver (`False>>ifTrue: => self`), NOT
    /// `nil` (the mutation path's `{'nil', State}` is a pre-existing
    /// divergence, pinned by
    /// `test_if_true_mutation_bypasses_runtime_dispatch` — follow-up, not
    /// touched here).
    pub(in crate::core_erlang) fn generate_if_true_pure(
        &mut self,
        receiver: &Expression,
        block: &Block,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            "ifTrue:",
            PureArm::Inline(block),
            PureArm::Receiver,
            fallback_args,
        )
    }

    /// BT-4070: the pure-inline producer for `R ifFalse: [B]` — the mirror
    /// of [`Self::generate_if_true_pure`] (`True>>ifFalse: => self`).
    pub(in crate::core_erlang) fn generate_if_false_pure(
        &mut self,
        receiver: &Expression,
        block: &Block,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            "ifFalse:",
            PureArm::Receiver,
            PureArm::Inline(block),
            fallback_args,
        )
    }

    /// BT-4070: the pure-inline producer for `R ifTrue: [T] ifFalse: [F]`.
    pub(in crate::core_erlang) fn generate_if_true_if_false_pure(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            "ifTrue:ifFalse:",
            PureArm::Inline(true_block),
            PureArm::Inline(false_block),
            fallback_args,
        )
    }

    /// BT-4070: the pure-inline producer for `R and: [B]`. The non-taken
    /// (false) arm is the literal `false`, matching `Boolean>>and:`
    /// (`stdlib/src/boolean.bt`: `self ifTrue: aBlock ifFalse: [false]`).
    pub(in crate::core_erlang) fn generate_and_pure(
        &mut self,
        receiver: &Expression,
        block: &Block,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            "and:",
            PureArm::Inline(block),
            PureArm::Literal(false),
            fallback_args,
        )
    }

    /// BT-4070: the pure-inline producer for `R or: [B]`. The non-taken
    /// (true) arm is the literal `true`, matching `Boolean>>or:`
    /// (`stdlib/src/boolean.bt`: `self ifTrue: [true] ifFalse: aBlock`).
    pub(in crate::core_erlang) fn generate_or_pure(
        &mut self,
        receiver: &Expression,
        block: &Block,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            "or:",
            PureArm::Literal(true),
            PureArm::Inline(block),
            fallback_args,
        )
    }

    /// BT-4070: the shared core behind the five `generate_*_pure`
    /// builders — `R <sel> [blocks]` with literal, non-mutating blocks as an
    /// inline `case` over the once-evaluated receiver `C`:
    ///
    /// ```erlang
    /// let _Cond1 = <receiver> in
    /// case _Cond1 of
    ///   <'true'> when 'true' -> <true_arm>
    ///   <'false'> when 'true' -> <false_arm>
    ///   <_CondNoMatch2> when 'true' -> call 'beamtalk_message_dispatch':'send'(
    ///     _Cond1, '<sel>', [fun () -> <block> end, ...])
    /// end
    /// ```
    ///
    /// The wildcard arm is a real dynamic send — never a `case_clause`
    /// raise — so a non-Boolean receiver (a custom `ifTrue:`
    /// implementor, DNU, a proxy, `perform:`) behaves exactly as today; it
    /// doubles as the static-exhaustiveness arm (see
    /// `case_clause_fallback`'s doc comment) and shares its `CondNoMatch`
    /// naming for grep-ability. The fallback closures compile through
    /// today's Tier-1 builder via `expression_doc`, over the original
    /// argument expressions, so they are byte-identical to the closures the
    /// fully-dynamic send would allocate — allocated only when the arm is
    /// taken. Pure-inline is declined while compiling them: they run only
    /// for non-Boolean receivers, so the optimisation buys nothing there,
    /// and inlining inside them would duplicate nested blocks exponentially
    /// (each level compiles its blocks both inline and as fallback
    /// closures). Codegen warnings are deduplicated across the fallback
    /// loop for the same reason — the fallback lowers the same AST the
    /// inline arm already reported, and `add_codegen_warning` does not
    /// dedup — while lowering-specific findings are kept (see
    /// `dedup_append_codegen_warnings`).
    ///
    /// The receiver compiles through [`Self::compile_conditional_receiver`],
    /// the same path the mutation builders use: the gate guarantees an
    /// empty prelude, but any residual one still runs before `C` binds, so
    /// the binding order can never be wrong.
    pub(in crate::core_erlang) fn generate_pure_boolean_case(
        &mut self,
        receiver: &Expression,
        selector_atom: &'static str,
        true_arm: PureArm<'_>,
        false_arm: PureArm<'_>,
        fallback_args: &[Expression],
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let recv_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let recv_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let true_doc = self.pure_arm_doc(true_arm, &cond_var)?;
        let false_doc = self.pure_arm_doc(false_arm, &cond_var)?;
        // Decline pure-inline inside the fallback closures (same
        // save/restore shape as `while_loops.rs` — bind the result,
        // restore, then `?`, so an error never leaks the flag): without
        // this, nested pure conditionals emit 2^n copies of the innermost
        // arm. Warnings are deduplicated across the loop: the fallback
        // lowers the same AST the inline arm already reported, so exact
        // repeats are dropped — but lowering-specific findings survive.
        let prev = std::mem::replace(&mut self.decline_pure_boolean_inline, true);
        let warn_len = self.codegen_warnings.len();
        let closures: Result<Vec<_>> = fallback_args
            .iter()
            .map(|a| self.expression_doc(a))
            .collect();
        self.decline_pure_boolean_inline = prev;
        let new_warnings = self.codegen_warnings.split_off(warn_len);
        self.dedup_append_codegen_warnings(new_warnings);
        let closure_docs = closures?;
        let fallback =
            Self::runtime_send_call(leaf::var(cond_var.clone()), selector_atom, closure_docs);
        let no_match_var = self.fresh_temp_var("CondNoMatch");

        Ok(docvec![
            recv_preamble,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_doc,
            " <'false'> when 'true' -> ",
            false_doc,
            " <",
            leaf::var(no_match_var),
            "> when 'true' -> ",
            fallback,
            " end",
        ])
    }

    /// BT-4070: one arm's document of [`Self::generate_pure_boolean_case`].
    fn pure_arm_doc(&mut self, arm: PureArm<'_>, cond_var: &str) -> Result<Document<'static>> {
        match arm {
            PureArm::Inline(block) => {
                // A `case` arm is a Core Erlang scope of its own, but the
                // generator's variable scope is not: a block-local temp the
                // arm binds (`[t := 5. t]`) must not leak into the
                // enclosing method's scope, where a later `t` would resolve
                // to the arm's `let`-bound variable. Push/pop around the
                // arm, as every closed-`fun` lowering does — but WITHOUT
                // `with_closed_fun_scope`'s state-version reset, which is
                // for closed funs only: a pure arm is inlined and shares
                // the enclosing versioning.
                self.push_scope();
                let body = self.generate_block_body(block);
                self.pop_scope();
                body
            }
            PureArm::Receiver => Ok(leaf::var(cond_var)),
            PureArm::Literal(value) => Ok(leaf::atom(if value { "true" } else { "false" })),
        }
    }

    /// ADR 0118 phase 4: `true` if `expr` (already paren-unwrapped)
    /// is an inline-threaded control-flow construct — `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:`, `and:`/`or:`, the nil-conditional family, or
    /// `match:` — that needs mutation threading.
    /// The pure gate behind
    /// [`Self::inline_control_flow_producer`], reusing the exact same
    /// per-family predicates that producer dispatches on, so the two can
    /// never disagree. Called by
    /// [`CoreErlangGenerator::subexpr_needs_prelude`] (`util.rs`) to decide
    /// whether compiling `expr` — receiver, argument, or any other
    /// sequenced position — would give it a non-empty prelude.
    pub(in crate::core_erlang) fn inline_control_flow_needs_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        let expr = expr.unwrap_parens();
        if let Expression::Match { arms, .. } = expr {
            return self.match_needs_state_threading(arms);
        }
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        match (sel.as_str(), arguments.as_slice()) {
            ("and:" | "or:", [Expression::Block(block)]) => {
                self.and_or_needs_mutation_threading(receiver, block)
            }
            ("ifTrue:" | "ifFalse:" | "ifNotNil:" | "ifNil:", [Expression::Block(block)]) => {
                self.conditional_needs_mutation_threading(receiver, &[block])
            }
            (
                "ifTrue:ifFalse:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:",
                [Expression::Block(b0), Expression::Block(b1)],
            ) => self.conditional_needs_mutation_threading(receiver, &[b0, b1]),
            _ => false,
        }
    }

    /// ADR 0118 phase 4 (ADR 0118 §Decision 2): the [`ThreadedValue`]
    /// producer for every inline-threaded control-flow construct that can
    /// sit anywhere in expression position — `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:`, `and:`/`or:`, the nil-conditional family, and
    /// `match:`. `Ok(None)` when `expr` (already paren-unwrapped) is not one
    /// of these, or is but [`Self::inline_control_flow_needs_threading`]
    /// says it doesn't need threading — the caller then falls through to
    /// its ordinary (unthreaded) compile.
    ///
    /// The single recognizer both
    /// [`CoreErlangGenerator::threaded_expression`]'s inline-control-flow
    /// branch (nested/argument expression position — the row that used to
    /// leak a `{Result, State}` tuple into an argument) and
    /// [`super::super::threaded_expr`]'s Actor-body
    /// `emit_actor_threaded_last_stmts`/`emit_actor_threaded_assign_rhs_stmts`
    /// (an already-classified top-level `ControlFlowWithMutations`
    /// statement) call, so a decision to inline can never disagree with
    /// what inlines — and the one place any of these six-plus-`match:`
    /// producers' preludes are built (via
    /// [`Self::control_flow_tuple_to_threaded_value`]), so no
    /// `{Result, State}` tuple `Document` crosses an expression boundary
    /// through this path.
    pub(in crate::core_erlang) fn inline_control_flow_producer(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<ThreadedValue>> {
        self.inline_control_flow_producer_with(expr, ProducerTail::Rebind)
    }

    /// [`Self::inline_control_flow_producer`] with the tail of the construct's
    /// locals chosen by the caller (ADR 0131 §2): `Rebind` rebinds every
    /// threaded local; `Discard` is the method root's last expression, whose
    /// rebinds nothing can observe.
    ///
    /// A conditional that is a root local-threading producer
    /// ([`Self::conditional_producer_locals`]) is lowered even when the
    /// statement-position gate ([`Self::inline_control_flow_needs_threading`],
    /// read+write analysis) would not inline it: a write-only arm
    /// (`c ifTrue: [t := 5]`) threads `t` all the same.
    pub(in crate::core_erlang) fn inline_control_flow_producer_with(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
    ) -> Result<Option<ThreadedValue>> {
        let expr = expr.unwrap_parens();
        let root_producer_locals = if self.is_producer_frame_for(self.current_frame(), expr) {
            self.conditional_producer_locals(expr)
        } else {
            None
        };
        let lowered = self.lower_inline_control_flow(expr, tail, root_producer_locals.is_some())?;
        #[cfg(test)]
        if let (Some(_), Some(locals)) = (&lowered, &root_producer_locals) {
            super::super::threaded_expr::fault::record_producer(expr.span(), locals);
        }
        Ok(lowered)
    }

    /// [`Self::inline_control_flow_producer_with`] once `expr` is
    /// paren-unwrapped; `root_producer` says it is a conditional local-threading
    /// producer ([`Self::conditional_producer_locals`]).
    #[allow(clippy::too_many_lines)] // one arm per conditional selector
    fn lower_inline_control_flow(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
        root_producer: bool,
    ) -> Result<Option<ThreadedValue>> {
        if !root_producer && !self.inline_control_flow_needs_threading(expr) {
            return Ok(None);
        }
        if let Expression::Match { value, arms, .. } = expr {
            let span = expr.span();
            // BT-3947: a `match:` that is a local-threading producer here has
            // one `BranchArm` frame per arm, like a conditional's blocks.
            if root_producer && !self.is_repl_mode() {
                return Ok(Some(self.generate_match_producer(value, arms, span, tail)?));
            }
            let frame = self.current_frame();
            let tuple_doc = self.generate_match(value, arms)?;
            return Ok(Some(
                self.control_flow_tuple_to_threaded_value(tuple_doc, frame, span)?,
            ));
        }
        // ADR 0131 Phase 2d (BT-3798): a `Dictionary` lookup whose handler
        // writes an outer local, a producer here (never `needs_threading`).
        if root_producer {
            if let Some(tv) = self.generate_lookup_producer(expr, tail)? {
                return Ok(Some(tv));
            }
        }
        // BT-3841: an evaluated literal block (`[...] value`) that is a
        // producer here is a conditional whose one arm always runs.
        if root_producer {
            if let Some(block) =
                beamtalk_core::semantic_analysis::block_facts::literal_block_value_arm(expr)
            {
                return Ok(Some(self.generate_block_value_with_mutations(
                    block,
                    tail,
                    root_producer,
                )?));
            }
        }
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            // Unreachable: `inline_control_flow_needs_threading` only
            // returns `true` for a `Match` or a keyword `MessageSend`.
            return Ok(None);
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let tv = match (sel.as_str(), arguments.as_slice()) {
            ("and:", [Expression::Block(block)]) => {
                self.generate_and_with_mutations(receiver, block, tail, root_producer)?
            }
            ("or:", [Expression::Block(block)]) => {
                self.generate_or_with_mutations(receiver, block, tail, root_producer)?
            }
            ("ifTrue:", [Expression::Block(block)]) => {
                self.generate_if_true_with_mutations(receiver, block, tail, root_producer)?
            }
            ("ifFalse:", [Expression::Block(block)]) => {
                self.generate_if_false_with_mutations(receiver, block, tail, root_producer)?
            }
            (
                "ifTrue:ifFalse:",
                [
                    Expression::Block(true_block),
                    Expression::Block(false_block),
                ],
            ) => self.generate_if_true_if_false_with_mutations(
                receiver,
                true_block,
                false_block,
                tail,
                root_producer,
            )?,
            ("ifNotNil:", [Expression::Block(block)]) => self
                .generate_nil_conditional_with_mutations(
                    receiver,
                    None,
                    Some(block),
                    tail,
                    root_producer,
                )?,
            ("ifNil:", [Expression::Block(block)]) => self
                .generate_nil_conditional_with_mutations(
                    receiver,
                    Some(block),
                    None,
                    tail,
                    root_producer,
                )?,
            (
                "ifNil:ifNotNil:",
                [
                    Expression::Block(nil_block),
                    Expression::Block(not_nil_block),
                ],
            )
            | (
                "ifNotNil:ifNil:",
                [
                    Expression::Block(not_nil_block),
                    Expression::Block(nil_block),
                ],
            ) => self.generate_nil_conditional_with_mutations(
                receiver,
                Some(nil_block),
                Some(not_nil_block),
                tail,
                root_producer,
            )?,
            _ => return Ok(None),
        };
        Ok(Some(tv))
    }

    /// ADR 0111 Addendum 5 §C1: lowers a `self.field := value`
    /// conditional-branch/block-body statement to its real `Bind` sequence,
    /// appending it to `stmts` and returning the assigned value's temp var
    /// name. Mirrors [`CoreErlangGenerator::generate_field_assignment_open`]'s
    /// normal (non hybrid-full-extract) branch exactly — same helper calls,
    /// same mint order, same [`FieldWriteSite`] dispatch — but
    /// models the state mutation as a [`ThreadedStmt::Bind`] instead of a
    /// hand-rolled `maps:put` `Document` fragment.
    ///
    /// The hybrid full-extract sub-branch (`in_hybrid_loop &&
    /// hybrid_mutated_fields.contains(field)`) rebinds a direct fun parameter
    /// instead of `maps:put`-ing into `StateAcc`; it lowers through
    /// [`Self::lower_hybrid_field_rebind`], the one lowering
    /// `generate_field_assignment_open`'s hybrid branch renders too.
    /// §Scope: a mutation-carrying conditional forces `StateAcc` fallback,
    /// mutually exclusive with hybrid mode, but `generate_block_stateful_body`
    /// and the Letrec bodies reach this with the exclusion less directly
    /// self-evident, so the check is real rather than assumed away.
    pub(in crate::core_erlang) fn lower_field_assignment_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let Expression::Assignment { target, value, .. } = expr else {
            unreachable!("field-assignment lowering requires an Assignment expr");
        };
        let Expression::FieldAccess { field, .. } = target.as_ref() else {
            unreachable!("field-assignment lowering requires a FieldAccess target");
        };
        // BT-4122 (`FieldReadFromStateAcc` fun-body arm): a Tier 2 `fun`
        // threads this write through its `StateAcc`. After the merge the
        // `fun` writes the cell instead; a merged body never reaches here.
        if self.in_tier2_stateful_body && self.synced_calls_scope() {
            self.record_tier2_stateacc_gap(
                format!(
                    "{} := (tier2 fun body writes field via StateAcc)",
                    field.name
                ),
                span,
            );
        }
        if self.loop_mode.in_hybrid_loop
            && self
                .loop_mode
                .hybrid_mutated_fields
                .contains(field.name.as_str())
        {
            return self.lower_hybrid_field_rebind(&field.name, value, frame, span, stmts);
        }

        // ADR 0118 phase 2a: `self.log := self.log ++
        // #(self getValue)` — thread every order-safe self-send nested in
        // the RHS as real `Bind`s *before* `source_version` is read below,
        // so this assignment's own `Bind` chains from the post-dispatch
        // state and the RHS compile that follows is pure (the exact call
        // site an earlier, reverted version-bump-on-compile prototype
        // desynced).
        let site = FieldWriteSite::for_context(self.context);
        let thread_scope = self.thread_ahead(value, stmts, frame)?;
        let (preamble, bind, val_var) = self.lower_simple_field_write_bind(
            site,
            Closure::Open,
            &field.name,
            value,
            frame,
            span,
        )?;
        self.finish_precompiled_scope(thread_scope)?;
        stmts.push(ThreadedStmt::Statement(preamble, span));
        stmts.push(bind);
        Ok(val_var)
    }

    /// The one lowering of a hybrid-loop field rebind (BT-3811): in a hybrid
    /// loop a mutated field is extracted to a direct fun parameter, so
    /// `self.field := value` rebinds that parameter instead of stepping the
    /// `State` map (`let Val = <value> in let FieldN = Val in`, plus an update
    /// of `hybrid_readonly_field_params` so later reads see the new variable),
    /// decomposed into a real `Bind` so the field's own rebind chain is
    /// traceable from its `produces` seed (`VersionPrefix::Gensym` of the
    /// pre-extracted param name).
    ///
    /// Appends to `stmts`, in `frame`: the RHS's ahead prelude (a field
    /// assignment or self-send nested in the RHS, threaded by
    /// [`Self::thread_ahead`] so its steps land in the caller's statement
    /// list, not in the RHS document), the opaque `let Val = ...`, and the
    /// rebind `Bind`. Returns the assigned value's temp var.
    ///
    /// Shared by [`Self::lower_field_assignment_bind`] (every `Vec<ThreadedStmt>`
    /// consumer: Letrec bodies, conditional arms, stateful-block bodies) and
    /// [`CoreErlangGenerator::generate_field_assignment_open`] (the
    /// `Document`-returning consumers, which render these statements through
    /// [`CoreErlangGenerator::rendered_field_rebind_doc`]).
    pub(in crate::core_erlang) fn lower_hybrid_field_rebind(
        &mut self,
        field: &str,
        value: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let val_var = self.fresh_temp_var("Val");
        let ahead_scope = self.thread_ahead(value, stmts, frame)?;
        // `thread_ahead` above already committed any nested field
        // assignment's update (`self.x := (self.y := 42)` keeps `y`'s new
        // param); only the RHS's own (precompiled, value-only) compile below
        // is scoped, so snapshot the params around it.
        let saved_field_params = self.loop_mode.hybrid_readonly_field_params.clone();
        let val_doc = self.expression_doc(value)?;
        self.finish_precompiled_scope(ahead_scope)?;
        self.loop_mode.hybrid_readonly_field_params = saved_field_params;
        let current_field_var = self
            .loop_mode
            .hybrid_readonly_field_params
            .get(field)
            .cloned()
            .unwrap_or_else(|| field.to_string());
        let source = VersionedVar::new(VersionPrefix::Gensym(current_field_var), 0, frame);
        let new_field_var =
            self.fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(field)));
        self.loop_mode
            .hybrid_readonly_field_params
            .insert(field.to_string(), new_field_var.clone());
        stmts.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(val_var.clone()), " = ", val_doc, " in "],
            span,
        ));
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::Gensym(new_field_var), 1, frame),
            source,
            op: BindOp::Direct(ValueRef::Var(val_var.clone())),
            span,
        });
        Ok(val_var)
    }

    /// ADR 0111 Addendum 5 §C2/§C3/§C4: lowers a `LocalAssignPure`/
    /// `LocalAssignTier2`/`LocalAssignControlFlow`/`LocalAssignSelfSend`
    /// conditional-branch statement to its real `Bind` sequence, appending
    /// it to `stmts` and returning the assigned value's temp var name. The
    /// mutation is a [`ThreadedStmt::Bind`] rather than a hand-rolled
    /// `maps:put` `Document` fragment. Unlike the top-level `BodyExprKind`
    /// shapes `gen_server/methods.rs` lowers (which route
    /// `LocalAssignSelfSend` through `generate_self_dispatch_parts`
    /// instead), the four kinds above ALL reach this one lowering — its own
    /// `is_tier2_value_call`/open-scope checks decide the actual shape at
    /// runtime, not `classify_body_expr`'s static kind.
    ///
    /// The one `StateAcc` local-write lowering: a conditional branch, a
    /// `StateAcc` loop body, a carried while condition (BT-4015, which
    /// deleted the `Document`-returning twin it was written to mirror) and
    /// `expressions.rs`'s `generate_block_stateful_body` (the Tier 2
    /// stateful-block-body local var assignment case) all call it.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn lower_local_var_assignment_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let Expression::Assignment { target, value, .. } = expr else {
            unreachable!("LocalAssign* kinds must ensure expr is an Assignment");
        };
        let Expression::Identifier(id) = target.as_ref() else {
            unreachable!("LocalAssign* kinds must ensure assignment target is an Identifier");
        };

        let val_var = self.fresh_temp_var("Val");
        // In REPL mode, use the plain variable name as the key (no
        // __local__ prefix), so reads and writes agree on the key.
        let state_key: String = if self.is_repl_mode() {
            id.name.to_string()
        } else {
            Self::local_state_key(&id.name)
        };

        // C2z — BT-3493: the RHS is itself a field write (`r := (self.x
        // := ...)`, at any parenthesization depth) — the shared ADR 0111
        // Addendum 5 statement classifier (`classify_body_expr`) sees only
        // the outer `var := <expr>` shape and hands every RHS here as
        // `LocalAssignPure`, so this is the "see through the value-carrying
        // parent" step the field write itself needs: lower it through the
        // SAME real-`Bind` producer C1 (`lower_field_assignment_bind`) uses
        // for a bare `self.field := ...` statement — arbitrary-RHS support,
        // `reject_class_var_field_assignment`'s class-var gate, and
        // `thread_ahead`-based sequencing all included — then alias this
        // statement's own local var to the identical assigned value, exactly
        // as `:=`'s "the whole assignment evaluates to the assigned value"
        // semantics require. Checked before C3/C3b/C4 below: a field write's
        // own target is `self.<field>`, which can never itself satisfy
        // `is_tier2_value_call`/`control_flow_has_mutations`/
        // `is_dispatching_actor_self_send` (all of which pattern-match a
        // `MessageSend`, not an `Assignment`), so this check cannot shadow
        // any of theirs.
        //
        // Claude Review follow-up on this PR: the field write's own `Bind`
        // chain (built by `lower_field_assignment_bind`, above) advances
        // `State`/`StateAcc` but says nothing about THIS statement's local
        // var — like every other branch below, `r` must also be `Put` into
        // the arm's own `StateAcc` under its `__local__` key, mirroring C2's
        // own `Put` immediately below. Without it, `seed_conditional_locals`'s
        // documented invariant (every taken branch overwrites the SEEDED key
        // for any local it mutates) breaks for exactly this shape: a local
        // declared before the conditional and reassigned via `r := (self.x
        // := ...)` inside one arm reads back the STALE pre-conditional
        // value once the branches merge and a later statement re-extracts
        // `r` via `maps:get` — silently wrong, not a crash, since the local's
        // in-branch lexical binding (`bind_var`) is real but never escapes
        // this arm's own `StateAcc`.
        if let Some(field_write) = self.local_assign_field_write(value) {
            let field_val_var =
                self.lower_field_assignment_bind(field_write, frame, span, stmts)?;
            self.push_local_slot_put(stmts, frame, &id.name, state_key, &field_val_var, span);
            self.bind_var(&id.name, &field_val_var);
            return Ok(field_val_var);
        }

        // C3 — Tier 2 block-call RHS returns {Result, NewStateAcc};
        // the sanctioned Gensym two-hop (ADR 0111 Addendum 5's "opaque
        // nested-construct state extraction feeding a maps:put" idiom).
        // In every frame, a value that is itself a local-threading producer
        // here is threaded ahead by C2 below instead, like a loop or fold
        // (BT-3823). In a method frame that never changes the verdict: an
        // evaluated block that is a producer there is not a Tier 2 call
        // (`is_tier2_value_call` defers to `block_value_producer_in_frame`). In
        // a REPL arm it routes the evaluated block to the REPL producer. Only
        // the value itself counts: a Tier 2 call whose *operand* is a producer
        // keeps this lowering, so the call's own `NewState` is never closed
        // away by C2.
        if self.is_tier2_value_call(value) && self.local_producer_locals(value).is_none() {
            let t2_tuple = self.fresh_temp_var("T2");
            let t2_state = self.fresh_temp_var("T2St");
            let value_code = self.generate_tier2_value_call_doc(value)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(t2_tuple.clone()),
                    " = ",
                    value_code,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(t2_tuple.clone()),
                    ") in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(t2_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(t2_tuple),
                    ")",
                ])),
                span,
            });
            self.push_local_slot_put_from(
                stmts,
                frame,
                &id.name,
                state_key,
                &val_var,
                gensym_state,
                span,
            );
            self.bind_var(&id.name, &val_var);
            return Ok(val_var);
        }

        // C3b — RHS is itself control-flow-with-mutations (a
        // mutating list-op like `collect:`/`do:`/`select:` whose block —
        // or whose own receiver — needs state threading, or a nested
        // `ifTrue:ifFalse:`/`match:`/`on:do:` with field mutations).
        // Codegen for such an RHS (`expression_doc`) returns a closed
        // `{Value, StateAcc}` 2-tuple — the same shape C3's Tier 2 call
        // returns — which must be unwrapped exactly like C3, instead of
        // falling into C2 below (which assumes a single, non-tuple value
        // and would bind `val_var` to the raw tuple itself). Mirrors
        // `emit_actor_threaded_assign_rhs_stmts`, the top-level flat-body
        // counterpart of this same `LocalAssignControlFlow` RHS shape —
        // this body loop's C2 fallback previously had no equivalent,
        // silently leaking the tuple as the assigned value and the
        // branch's own result.
        //
        // ADR 0131 Phase 2b: not when the RHS is a loop or fold that is a
        // local-threading producer in this body — C2 below threads it ahead
        // (`thread_ahead` splices the construct tuple and its `LocalRebind`s,
        // lowered by this frame's mode) and reads its unwrapped value back.
        // BT-4000: nor an inline-threaded construct (a conditional, `match:`,
        // an exception or evaluated-block producer): `thread_ahead` splices its
        // prelude and registers its already-unwrapped value, as at the method
        // root (`emit_actor_threaded_assign_rhs_stmts`), so C2 reads it back.
        // Unwrapped a second time here, `element(1, ...)` of the value raised
        // `badarg` at run time.
        if self.control_flow_has_mutations(value)
            && !self.expr_has_local_producer(value)
            && !self.thread_ahead_unwraps(value)
        {
            let cf_tuple = self.fresh_temp_var("CfTuple");
            let cf_state = self.fresh_temp_var("CfSt");
            let thread_scope = self.thread_ahead(value, stmts, frame)?;
            let value_code = self.expression_doc(value)?;
            self.finish_precompiled_scope(thread_scope)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(cf_tuple.clone()),
                    " = ",
                    value_code,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(cf_tuple.clone()),
                    ") in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(cf_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(cf_tuple),
                    ")",
                ])),
                span,
            });
            self.push_local_slot_put_from(
                stmts,
                frame,
                &id.name,
                state_key,
                &val_var,
                gensym_state,
                span,
            );
            self.bind_var(&id.name, &val_var);
            self.push_control_flow_threaded_var_rereads(value, span, stmts);
            return Ok(val_var);
        }

        // ADR 0118 phase 2a: as in `lower_field_assignment_bind`
        // — thread every order-safe self-send nested in the RHS (or the RHS
        // itself, `v := self bump`) as real `Bind`s before the RHS compiles
        // and before either `source_version` below is read. ADR 0118 phase
        // 5b: `thread_ahead`'s `subexpr_needs_prelude` now
        // recognizes a class-var producer too (a same-class self-send or
        // class-var assignment), so the former open-scope branch here (C4 —
        // already dead in practice per ADR 0111 Addendum 5, no
        // live program reached it) is now structurally unreachable: any
        // such producer is threaded ahead of this point, and the plain
        // compile below reads it back via `precompiled_subexprs`
        // substitution, exactly like the C3b branch above.
        let thread_scope = self.thread_ahead(value, stmts, frame)?;
        let value_code = self.expression_doc(value)?;
        self.finish_precompiled_scope(thread_scope)?;

        // C2 — the common case (plain / REPL-mode key).
        stmts.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(val_var.clone()),
                " = ",
                value_code,
                " in "
            ],
            span,
        ));
        self.push_local_slot_put(stmts, frame, &id.name, state_key, &val_var, span);
        self.bind_var(&id.name, &val_var);
        Ok(val_var)
    }

    /// Whether an assignment to `local` in `frame` writes the local's slot in
    /// the frame's scratch map (its `__local__` key, or the binding in the
    /// REPL). ADR 0133 Phase 3 (BT-3911): in an arm frame ([`RebindFrameKind::BranchArm`])
    /// only a local the frame carries (`threads`) has a slot: the construct's
    /// result tuple hands back exactly those, and a local bound in the arm is
    /// a plain `let`. Every other frame keeps its slot write.
    fn local_has_scratch_slot(&self, frame: FrameId, local: &str) -> bool {
        if self.is_repl_mode() {
            return true;
        }
        #[cfg(test)]
        if super::super::threaded_expr::fault::active()
            == Some(super::super::threaded_expr::fault::LocalRebindFault::PutUncarriedLocal)
        {
            return true;
        }
        match self.rebind_frames.get(&frame) {
            Some(rebind) if rebind.kind == RebindFrameKind::BranchArm => {
                rebind.threads_local(local)
            }
            _ => true,
        }
    }

    /// The `Put` of `val_var` into `local`'s scratch slot that follows an
    /// assignment of it ([`Self::local_has_scratch_slot`]), stepping the
    /// frame's current `State` version; nothing for a local the frame does not
    /// carry.
    fn push_local_slot_put(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        frame: FrameId,
        local: &str,
        state_key: String,
        val_var: &str,
        span: Span,
    ) {
        if !self.local_has_scratch_slot(frame, local) {
            return;
        }
        let source = VersionedVar::new(VersionPrefix::State, self.state_version(), frame);
        self.push_local_slot_put_from(stmts, frame, local, state_key, val_var, source, span);
    }

    /// [`Self::push_local_slot_put`] stepping from `source`: the `State`
    /// version a Tier 2 call's or construct's closed-over tuple handed back
    /// (a `Gensym` identity), which the frame's `State` chain must continue
    /// from whether or not `local` has a slot to write it into.
    #[allow(clippy::too_many_arguments)] // one argument per fact of the write
    fn push_local_slot_put_from(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        frame: FrameId,
        local: &str,
        state_key: String,
        val_var: &str,
        source: VersionedVar,
        span: Span,
    ) {
        let slot = self.local_has_scratch_slot(frame, local);
        let _ = self.next_state_var();
        let target_version = self.state_version();
        let target = VersionedVar::new(VersionPrefix::State, target_version, frame);
        let op = if slot {
            BindOp::Put {
                field: state_key,
                value: ValueRef::Var(val_var.to_string()),
            }
        } else {
            BindOp::Direct(ValueRef::Version(source.clone()))
        };
        stmts.push(ThreadedStmt::Bind {
            target,
            source,
            op,
            span,
        });
    }

    /// Generates an inline block body for a conditional branch with field mutation threading.
    ///
    /// **Precondition**: Caller must set `in_loop_body = true` and `state_version = 0`
    /// before calling, and restore them afterwards. The initial state inside the branch
    /// is named `StateAcc` (bound to the outer state by the caller via
    /// `let StateAcc = State{N} in ...`).
    ///
    /// Returns `(body_doc, final_state_version)`. The generated code ends with
    /// `{<result>, <final_state>}`.
    ///
    /// ADR 0111 Addendum 5: this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s (per the addendum's C1–C13 per-shape
    /// decomposition table), wrapped in one [`ThreadedStmt::Threaded`] node
    /// (`mode: StateAcc(None)`, this arm's own [`FrameId`] — Rule 3), then
    /// [`threaded_ir::verify`]d and [`threaded_ir::render`]ed — the
    /// `render`()ed `Document` IS this function's return value, byte-identical
    /// by construction (every shape below reuses the exact codegen calls and
    /// mint order the pre-migration hand-rolled version used). `StateAcc`
    /// mode's rendering is a transparent pass-through (`render_threaded`'s
    /// `ThreadingMode::StateAcc(_) => render(body, ctx)` arm), so wrapping
    /// changes zero bytes versus rendering `stmts` directly.
    pub(in crate::core_erlang) fn generate_conditional_branch_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        if self.is_repl_mode() {
            return self.generate_repl_arm_inline(block);
        }
        self.generate_plain_arm(block)
    }

    /// ADR 0133 Phase 3 (BT-3911): an arm of a construct that is not a
    /// local-threading producer (a conditional, a `detect:ifNone:` handler, a
    /// `match:` arm or a Tier 2 block inlined over the enclosing scratch map)
    /// is a `BranchArm` frame too, so the frame it runs in is always a
    /// registered one: the scratch map its statements write is the frame's,
    /// and a local the frame does not carry (one bound in the arm) is never
    /// written into it. It carries the locals the arm's block threads
    /// ([`Self::branch_threaded_locals`]: none for an arm that writes only
    /// locals of its own) and registers no producer sites, so a loop or fold
    /// in it unpacks its own tuple as before.
    ///
    /// Must be called inside [`Self::with_branch_context`].
    fn generate_plain_arm(&mut self, block: &Block) -> Result<(Document<'static>, usize)> {
        let arm = ArmFrame {
            threads: self.branch_threaded_locals(&[block]),
        };
        let rebind = RebindFrame {
            frame: self.current_branch_frame(),
            kind: RebindFrameKind::BranchArm,
            threads: arm.threads.clone(),
        };
        self.with_rebind_frame(rebind, |this| {
            this.generate_conditional_arm_statements(block, Some(&arm))
        })
    }

    /// BT-3823: [`Self::generate_conditional_branch_inline`] for a *REPL arm*
    /// (a conditional arm, an `on:do:`/`ensure:` block or a `tryDo:` block,
    /// inlined over the bindings map): a producer at one of its statements'
    /// `repl_arm_producer_sites` adopts that map
    /// ([`Self::with_repl_arm_producer_sites`]). Outside the REPL it is
    /// [`Self::generate_conditional_branch_inline`].
    pub(in crate::core_erlang) fn generate_repl_arm_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        let body = super::super::util::collect_body_exprs(&block.body);
        self.with_repl_arm_rebind_frame(|this| {
            this.with_repl_arm_producer_sites(&body, |this| {
                this.generate_conditional_arm_statements(block, None)
            })
        })
    }

    /// ADR 0131 §2 (Phase 3): one arm of a conditional that is a
    /// local-threading producer in a class or value-type method, lowered as a
    /// `BranchArm` frame. Registers the arm's [`RebindFrame`] (its `threads`
    /// are the construct's seeded set), and the arm statements' producer
    /// sites (the rule a loop body uses, `with_body_producer_sites`), so a
    /// loop or fold in the arm is a producer whose `LocalRebind`s are lowered
    /// by this frame: a `Put` into the arm's seeded `StateAcc` for a local the
    /// construct threads, a plain `let` for any other.
    ///
    /// Must be called inside [`Self::with_branch_context`] (the frame is
    /// [`Self::current_branch_frame`]).
    pub(in crate::core_erlang) fn generate_conditional_arm(
        &mut self,
        block: &Block,
        arm: &ArmFrame,
    ) -> Result<(Document<'static>, usize)> {
        let frame = self.current_branch_frame();
        let rebind = RebindFrame {
            frame,
            kind: RebindFrameKind::BranchArm,
            threads: arm.threads.clone(),
        };
        let body = super::super::util::collect_body_exprs(&block.body);
        self.with_rebind_frame(rebind, |this| {
            this.with_body_producer_sites(&body, false, true, |this| {
                this.generate_conditional_arm_statements(block, Some(arm))
            })
        })
    }

    /// The arm lowering behind [`Self::generate_conditional_branch_inline`]
    /// (`arm: None`, the legacy arm: its nested constructs unpack their own
    /// tuples) and [`Self::generate_conditional_arm`] (`arm: Some`, a
    /// `BranchArm` frame whose nested producers rebind by it).
    #[allow(clippy::too_many_lines)]
    fn generate_conditional_arm_statements(
        &mut self,
        block: &Block,
        arm: Option<&ArmFrame>,
    ) -> Result<(Document<'static>, usize)> {
        let frame = self.current_branch_frame();
        self.push_scope();

        let body = super::super::util::collect_body_exprs(&block.body);

        // Empty block returns nil with unchanged state (C13).
        if body.is_empty() {
            let final_version = self.state_version();
            self.pop_scope();
            return Ok(self.render_branch_arm(
                vec![ThreadedStmt::Return(
                    ValueRef::Literal("'nil'"),
                    VersionedVar::new(VersionPrefix::State, final_version, frame),
                    block.span,
                )],
                frame,
                final_version,
                block.span,
                arm,
            ));
        }

        let mut stmts: Vec<ThreadedStmt> = Vec::new();
        let mut last_result: Option<ValueRef> = None;

        for (i, expr) in body.iter().enumerate() {
            // BT-3838: a NON-last arm statement's value is discarded, so `(s)` is exactly `s`
            // (see `lower_body_exprs_with_reply_in_sites`); the last one is the arm's value.
            let expr = if i == body.len() - 1 {
                expr
            } else {
                &expr.unwrap_parens()
            };
            // Classified with the shared classifier just before the statement
            // is lowered, so a local bound earlier in this arm is live when a
            // catch boundary or loop in the statement is judged against the
            // scope (ADR 0131 Phase 4, BT-3840).
            let kind = self.classify_body_expr(expr);
            // BT-3823: a statement of a REPL arm that is itself a producer there
            // (`with_repl_arm_producer_sites`) is lowered by the producer
            // branch below, whatever the classifier says (the REPL has no
            // dispatch facts, so a loop is not `ControlFlowWithMutations`, and
            // an evaluated literal block is a `Tier2ValueCall`).
            let kind = if arm.is_none()
                && self.is_repl_mode()
                && self.local_producer_locals(expr).is_some()
            {
                BodyExprKind::ControlFlowWithMutations
            } else {
                kind
            };
            let is_last = i == body.len() - 1;
            let span = expr.span();

            // C0b — `^self otherMethod` (early return whose value is
            // a dispatching actor self-send, e.g. `ifTrue: [^self
            // configureVictim]`). `classify_body_expr` always classifies the
            // outer `Return` as `EarlyReturn` (never `DispatchingSelfSend` —
            // that classification only applies to a bare, un-returned
            // self-send), so without this check the statement below falls to
            // the C12 catch-all, which renders the whole `Return` as one
            // opaque `expression_doc` blob via the generic AST-directed
            // `Expression::Return` handler. That handler (pre-ADR-0118-phase-5b)
            // reached `generate_self_dispatch`'s *closed* form for a plain
            // self-send value — which computed the call's `Result` but
            // dropped its `NewState`, so the NLR throw's 4-tuple carried
            // this branch's
            // pre-call `StateAcc` instead of the mutation the self-send just
            // made. The call still runs; its effects vanish the instant this
            // `^` unwinds. Confirmed by `ThreadedIr::verify()` itself: the
            // opaque blob's raw text and this loop's own version bookkeeping
            // silently diverge (`report_threaded_ir_verify_errors` panics
            // debug-fatal on this exact shape).
            //
            // Mirrors C12b's `DispatchingSelfSend` Bind pattern immediately
            // below: dispatch via `generate_self_dispatch_call_doc_for`, Bind the
            // arm's next real `State` version from the call's `NewState`
            // element, then throw the NLR tuple against *that* version
            // instead of the branch's stale entry state — the same
            // `generate_self_dispatch_parts`-vs-closed distinction
            // `lower_body_exprs_with_reply`'s own `DispatchingSelfSend` arm
            // already applies for the top-level (unnested) `^self foo` shape
            // — just expressed as this loop's own `Bind`+`Statement`
            // pair rather than that flat body lowering's open let-chain, since
            // this arm's `ThreadedIr` wrapper needs the version bump to be a
            // real, verifiable production, not text buried in an opaque `Doc`.
            // excludes class methods (`in_class_method()`) for the
            // same reason `mod.rs`'s `Expression::Return` handler does —
            // `generate_self_dispatch_call_doc_for` unconditionally threads
            // `current_state_var()` (Actor instance state), which a class
            // method does not have — so a class-method self-send here falls
            // to the C12 catch-all below instead, unchanged from before this
            // fix.
            if let BodyExprKind::EarlyReturn = kind {
                if let Expression::Return { value, .. } = expr {
                    if !self.threads_no_actor_state()
                        && matches!(
                            self.classify_body_expr(value),
                            BodyExprKind::DispatchingSelfSend
                        )
                    {
                        // ADR 0118 phase 2a: `threaded_expression`'s
                        // producer path (`is_prelude_producer`, matching this
                        // arm's own `is_dispatching_actor_self_send`
                        // classification exactly, per C12b's identical swap
                        // above) replaces `dispatch_self_send_as_bind` —
                        // same `Statement`+`Bind` pair, plus (a genuine fix)
                        // `value`'s own arguments are now sequenced too
                        // (`^self log: (self nextId)`), which the old direct
                        // call never threaded.
                        let tv = self.threaded_expression(value, frame)?;
                        stmts.extend(tv.prelude);
                        let dispatch_value_doc = self.threaded_value_doc(&tv.value);
                        let nlr_token = self.current_nlr_token().cloned().ok_or_else(|| {
                            CodeGenError::Internal(
                                "BT-3374: EarlyReturn classification implies an active NLR \
                                 context, but none is set"
                                    .to_string(),
                            )
                        })?;
                        let new_state = self.current_state_var();
                        let throw_var = self.fresh_temp_var("NlrThrow");
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(throw_var.clone()),
                                " = call 'erlang':'throw'({'$bt_nlr', ",
                                leaf::var(nlr_token),
                                ", ",
                                dispatch_value_doc,
                                ", ",
                                leaf::var(new_state),
                                "}) in ",
                            ],
                            span,
                        ));
                        if is_last {
                            last_result = Some(ValueRef::Var(throw_var));
                        }
                        continue;
                    }

                    // BT-3495: `^ (self.field := ...)` — the RHS is itself
                    // a field write, at any parenthesization depth (the
                    // same "value-carrying parent" shape BT-3493 fixed for
                    // `r := (self.x := ...)`, but here the wrapper is
                    // `Return` instead of a local assignment). Lower it
                    // through the SAME real-`Bind` producer C1
                    // (`lower_field_assignment_bind`) uses for a bare
                    // `self.field := ...` statement, then throw the NLR
                    // tuple carrying the assigned value — mirroring C0b's
                    // `DispatchingSelfSend` throw immediately above, just
                    // with the field write's own value in place of the
                    // self-send's result.
                    if let Some(field_write) = self.local_assign_field_write(value) {
                        let field_val_var =
                            self.lower_field_assignment_bind(field_write, frame, span, &mut stmts)?;
                        let nlr_token = self.current_nlr_token().cloned().ok_or_else(|| {
                            CodeGenError::Internal(
                                "BT-3495: EarlyReturn classification implies an active NLR \
                                 context, but none is set"
                                    .to_string(),
                            )
                        })?;
                        let new_state = self.current_state_var();
                        let throw_var = self.fresh_temp_var("NlrThrow");
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(throw_var.clone()),
                                " = call 'erlang':'throw'({'$bt_nlr', ",
                                leaf::var(nlr_token),
                                ", ",
                                leaf::var(field_val_var),
                                ", ",
                                leaf::var(new_state),
                                "}) in ",
                            ],
                            span,
                        ));
                        if is_last {
                            last_result = Some(ValueRef::Var(throw_var));
                        }
                        continue;
                    }
                }
            }

            // BT-4000: `self.f := <conditional whose receiver is sequenced>` is
            // lowered by C1, whose `lower_field_assignment_bind` threads the
            // value ahead, so the receiver's `Bind`s are this arm's own
            // statements rather than text inside C6's raw tuple.
            let kind = match kind {
                BodyExprKind::FieldAssignmentControlFlow
                    if matches!(expr, Expression::Assignment { value, .. }
                        if self.is_conditional_with_sequenced_receiver(value)) =>
                {
                    BodyExprKind::FieldAssignment
                }
                kind => kind,
            };
            match kind {
                // C1
                BodyExprKind::FieldAssignment => {
                    let val_var =
                        self.lower_field_assignment_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        // val_var holds the assigned value variable
                        last_result = Some(ValueRef::Var(val_var));
                    }
                }
                // C2/C3/C4
                BodyExprKind::LocalAssignPure
                | BodyExprKind::LocalAssignTier2
                | BodyExprKind::LocalAssignControlFlow
                | BodyExprKind::LocalAssignSelfSend => {
                    let val_var =
                        self.lower_local_var_assignment_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        last_result = Some(ValueRef::Var(val_var));
                    }
                }
                // C5 — exempt from Bind modeling: no state version is
                // produced or consumed, every binding is a plain local.
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // BT-3495: `{a, b} := (self.field := ...)` — the
                        // RHS is itself a field write, at any
                        // parenthesization depth (mirrors C2z's `r := (self.x
                        // := ...)` fix in `lower_local_var_assignment_bind`,
                        // for `{a, b} :=` instead of a plain local
                        // assignment). Lower the field write through its own
                        // real-`Bind` producer C1 uses, then destructure the
                        // pattern from the already-evaluated assigned value —
                        // `generate_destructure_bindings_from_var` is the
                        // established "RHS already evaluated" entry point
                        // (`DestructureAssignmentControlFlow`'s own sibling
                        // shape uses the same idiom in `gen_server/methods.rs`).
                        if let Some(field_write) = self.local_assign_field_write(value) {
                            let field_val_var = self.lower_field_assignment_bind(
                                field_write,
                                frame,
                                span,
                                &mut stmts,
                            )?;
                            let binding_docs = self
                                .generate_destructure_bindings_from_var(pattern, &field_val_var)?;
                            for d in binding_docs {
                                stmts.push(ThreadedStmt::Statement(d, span));
                            }
                            if is_last {
                                // BT-3889: the arm answers the right-hand side.
                                last_result = Some(ValueRef::Var(field_val_var));
                            }
                        } else {
                            let (binding_docs, rhs_var) = self
                                .generate_destructure_bindings_sequenced(
                                    pattern, value, frame, &mut stmts,
                                )?;
                            for d in binding_docs {
                                stmts.push(ThreadedStmt::Statement(d, span));
                            }
                            if is_last {
                                last_result = Some(ValueRef::Var(rhs_var));
                            }
                        }
                    }
                }
                // C6 — self.field := <control-flow-with-mutations>.
                // The sanctioned Gensym two-hop.
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let tuple_var = self.fresh_temp_var("CfTuple");
                            let val_var = self.fresh_temp_var("CfVal");
                            let rhs_doc = self.expression_doc(value)?;
                            let rhs_state = self.fresh_temp_var("CfState");
                            let source_version = self.state_version();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(tuple_var.clone()),
                                    " = ",
                                    rhs_doc,
                                    " in let ",
                                    leaf::var(val_var.clone()),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var.clone()),
                                    ") in ",
                                ],
                                span,
                            ));
                            let gensym_state =
                                VersionedVar::new(VersionPrefix::Gensym(rhs_state), 1, frame);
                            stmts.push(ThreadedStmt::Bind {
                                target: gensym_state.clone(),
                                source: VersionedVar::new(
                                    VersionPrefix::State,
                                    source_version,
                                    frame,
                                ),
                                op: BindOp::Direct(ValueRef::Doc(docvec![
                                    "call 'erlang':'element'(2, ",
                                    leaf::var(tuple_var),
                                    ")",
                                ])),
                                span,
                            });
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: VersionedVar::new(
                                    VersionPrefix::State,
                                    target_version,
                                    frame,
                                ),
                                source: gensym_state,
                                op: BindOp::Put {
                                    field: field.name.to_string(),
                                    value: ValueRef::Var(val_var.clone()),
                                },
                                span,
                            });
                            self.push_control_flow_threaded_var_rereads(value, span, &mut stmts);
                            if is_last {
                                last_result = Some(ValueRef::Var(val_var));
                            }
                        }
                    }
                }
                // C7 — self fieldAt: name put: value. Dynamic field
                // name ⇒ §Dynamic-field-puts option 1 (Direct, not Put).
                BodyExprKind::SelfFieldAtPut => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_var("Name");
                        let val_var = self.fresh_temp_var("Val");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let source_version = self.state_version();
                        let current_state_name = self.current_state_var();
                        let val_code = self.expression_doc(&arguments[1])?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in let ",
                                leaf::var(val_var.clone()),
                                " = ",
                                val_code,
                                " in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'maps':'put'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(current_state_name),
                                ")",
                            ])),
                            span,
                        });
                        if is_last {
                            last_result = Some(ValueRef::Var(val_var));
                        }
                    }
                }
                // C7b — self clearField: name (ADR 0124 §1/B4), mirroring C7
                // immediately above: same `BindOp::Direct` shape with
                // `maps:remove` in place of `maps:put` and no value argument
                // to thread. Returns `Self` (`clearField: -> Self`,
                // `generate_self_clear_field_open`'s own convention) rather
                // than a written value.
                BodyExprKind::SelfClearField => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_var("Name");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let source_version = self.state_version();
                        let current_state_name = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'maps':'remove'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(current_state_name),
                                ")",
                            ])),
                            span,
                        });
                        if is_last {
                            last_result = Some(ValueRef::Var("Self".to_string()));
                        }
                    }
                }
                // C8 — self fieldAt: name put: <control-flow-with-mutations>.
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_temp_var("Name");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let val_var = self.fresh_temp_var("CfVal");
                        let val_code = self.expression_doc(&arguments[1])?;
                        let rhs_state = self.fresh_temp_var("CfState");
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                val_code,
                                " in let ",
                                leaf::var(val_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let gensym_state =
                            VersionedVar::new(VersionPrefix::Gensym(rhs_state.clone()), 1, frame);
                        stmts.push(ThreadedStmt::Bind {
                            target: gensym_state.clone(),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: gensym_state,
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'maps':'put'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(rhs_state),
                                ")",
                            ])),
                            span,
                        });
                        self.push_control_flow_threaded_var_rereads(
                            &arguments[1],
                            span,
                            &mut stmts,
                        );
                        if is_last {
                            last_result = Some(ValueRef::Var(val_var));
                        }
                    }
                }
                // BT-4000: `{a, b} := <conditional whose receiver is sequenced>`:
                // the value's prelude is this arm's own statements (C9's raw
                // tuple would hold the receiver's `Bind`s inside it).
                BodyExprKind::DestructureAssignmentControlFlow
                    if matches!(expr, Expression::DestructureAssignment { value, .. }
                        if self.is_conditional_with_sequenced_receiver(value)) =>
                {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let tv = self.threaded_expression(value, frame)?;
                        stmts.extend(tv.prelude);
                        let actual_val = self.fresh_temp_var("CfVal");
                        let value_doc = self.threaded_value_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(actual_val.clone()),
                                " = ",
                                value_doc,
                                " in "
                            ],
                            span,
                        ));
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        if is_last {
                            last_result = Some(ValueRef::Var(actual_val));
                        }
                    }
                }
                // C9 — {a, b} := <control-flow-with-mutations>. No
                // Gensym hop — element(2, tuple) binds State(v+1) directly.
                BodyExprKind::DestructureAssignmentControlFlow => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let actual_val = self.fresh_temp_var("CfVal");
                        let value_str = self.expression_doc(value)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                leaf::var(actual_val.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        self.push_control_flow_threaded_var_rereads(value, span, &mut stmts);
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        if is_last {
                            last_result = Some(ValueRef::Var(actual_val));
                        }
                    }
                }
                // ADR 0131 Phase 3: a loop or fold that is itself the statement of
                // a `BranchArm` frame is a local-threading producer there. Its
                // prelude (the construct tuple and one `LocalRebind` per local,
                // lowered by this arm) is spliced, so the arm's `StateAcc` keeps
                // every other key it carries.
                // BT-3823: so is one that is a statement of a REPL arm
                // (`generate_repl_arm_inline`): its prelude adopts the bindings
                // map the arm's `StateAcc` is.
                // BT-4000: and so is a conditional whose receiver advances
                // `State`, in every arm, as at the method root and in a loop
                // body: through `threaded_expression` its receiver's prelude
                // (`conditional_receiver_operand`) and its `State` extraction are
                // this arm's own statements. Compiled as the raw tuple of C10
                // below, the receiver's `Bind`s were rendered inside the tuple
                // document, and the arm's extraction read a version no `Bind` of
                // the arm produced (`UnboundVersion`).
                BodyExprKind::ControlFlowWithMutations
                    if self.arm_statement_is_producer(expr, arm.is_some()) =>
                {
                    let tv = self.threaded_expression(expr, frame)?;
                    stmts.extend(tv.prelude);
                    let value_doc = self.threaded_value_doc(&tv.value);
                    let var = self.fresh_temp_var(if is_last { "BranchResult" } else { "seq" });
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(var.clone()), " = ", value_doc, " in "],
                        span,
                    ));
                    if is_last {
                        last_result = Some(ValueRef::Var(var));
                    }
                }
                // C10
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let result_var = self.fresh_temp_var("BranchResult");
                        let expr_doc = self.expression_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_doc,
                                " in let ",
                                leaf::var(result_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        last_result = Some(ValueRef::Var(result_var));
                    } else {
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let expr_doc = self.expression_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_doc,
                                " in "
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        self.push_control_flow_threaded_var_rereads(expr, span, &mut stmts);
                    }
                }
                // C11 — a Tier 2 `value(:...)` call on a stored
                // block. Non-last carries the newline quirk
                // (`")\n in let "`) confirmed against real compiled output.
                BodyExprKind::Tier2ValueCall => {
                    if is_last {
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let result_var = self.fresh_temp_var("BranchResult");
                        let expr_doc = self.generate_tier2_value_call_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_doc,
                                " in let ",
                                leaf::var(result_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        last_result = Some(ValueRef::Var(result_var));
                    } else {
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let discard_var = self.fresh_temp_var("T2Discard");
                        let expr_doc = self.generate_tier2_value_call_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_doc,
                                " in let ",
                                leaf::var(discard_var),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ")\n in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        // rebind captured local-var mutations from
                        // NewState so a later read in this branch sees the
                        // mutated value.
                        if let Some(mutations) = self.get_inline_block_captured_mutations(expr) {
                            let new_state_name = self.current_state_var();
                            let mut re_reads: Vec<Document<'static>> = Vec::new();
                            for var in &mutations {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                re_reads.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state_name.clone()),
                                    ") in ",
                                ]);
                            }
                            if !re_reads.is_empty() {
                                stmts.push(ThreadedStmt::Statement(Document::Vec(re_reads), span));
                            }
                        }
                    }
                }
                // C12b — a same-class self-send dispatched through
                // `safe_dispatch`/a sealed call, which itself may mutate
                // `self`'s state — returns `{Result, NewState}`. Unlike the
                // C12 catch-all below (whose `expression_doc` render+`let`
                // discards everything but the raw tuple value), this must
                // Bind the returned `NewState` as this branch's own next
                // real `State` version, mirroring C11
                // (`ControlFlowWithMutations`)'s tuple-unpack `Bind` above —
                // otherwise a mutation performed via the self-send (as
                // opposed to a direct `self.field := value`) is silently
                // dropped once the branch closes.
                BodyExprKind::DispatchingSelfSend => {
                    // ADR 0118 phase 2a: `self log: (self
                    // nextId)` — `threaded_expression`'s producer path
                    // (`is_prelude_producer`, matching this arm's own
                    // `is_dispatching_actor_self_send` classification
                    // exactly) sequences the arguments' own self-sends
                    // ahead of this dispatch and builds the
                    // `Statement`+`Bind` pair — the same shape the
                    // pre-ADR-0118 planner built by hand.
                    let tv = self.threaded_expression(expr, frame)?;
                    stmts.extend(tv.prelude);
                    if is_last {
                        let result_var = self.fresh_temp_var("SDResultVal");
                        let value_doc = self.threaded_value_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(result_var.clone()),
                                " = ",
                                value_doc,
                                " in "
                            ],
                            span,
                        ));
                        last_result = Some(ValueRef::Var(result_var));
                    }
                }
                // C12 — catch-all pure statements (EarlyReturn, SuperSend,
                // ErrorSend, Pure).
                //
                // ADR 0118 phase 2a/2b: before compiling,
                // thread ahead (as real `Bind`s pushed into `stmts`) every
                // state-effecting sub-expression nested anywhere inside
                // this statement, via `thread_ahead` — a producer or a
                // covered send gets the sequencing rule ("decide once,
                // sequence all or none", now made universal);
                // every parent kind the rule covers (message sends, cast
                // sends, literal
                // containers, string interpolation, `^`/`:=`, a self-
                // cascade) is threaded in full — there is no longer an
                // order-unsafe subset that only gets a warning (the
                // gap the deleted planner used to leave). When nothing
                // needed threading (the overwhelmingly common case), this
                // is a no-op and `expression_doc` below behaves exactly as
                // before.
                _ => {
                    let thread_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                    if is_last {
                        let result_var = self.fresh_temp_var("BranchResult");
                        let expr_doc = self.expression_doc(expr)?;
                        self.finish_precompiled_scope(thread_scope)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(result_var.clone()),
                                " = ",
                                expr_doc,
                                " in "
                            ],
                            span,
                        ));
                        last_result = Some(ValueRef::Var(result_var));
                    } else {
                        let seq_var = self.fresh_temp_var("seq");
                        let expr_doc = self.expression_doc(expr)?;
                        self.finish_precompiled_scope(thread_scope)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(seq_var), " = ", expr_doc, " in "],
                            span,
                        ));
                    }
                }
            }
        }

        // C13 — the arm closer: {<result>, <final_state>}.
        let final_version = self.state_version();
        let result_value = last_result.unwrap_or(ValueRef::Literal("'nil'"));
        let closing_span = body.last().map_or(block.span, |e| e.span());
        stmts.push(ThreadedStmt::Return(
            result_value,
            VersionedVar::new(VersionPrefix::State, final_version, frame),
            closing_span,
        ));

        self.pop_scope();
        Ok(self.render_branch_arm(stmts, frame, final_version, block.span, arm))
    }

    /// ADR 0111 Addendum 5 (rebind idiom, shared by C6/C8/
    /// C9/C10): rebinds each of a nested control-flow construct's threaded
    /// `__local__` captured vars from the just-produced state, appending
    /// one `Statement` (or none, if there are no threaded vars) to `stmts`.
    /// A rebind read is never itself a version mutation (Rule 1) — always a
    /// `Statement`, never a `Bind`.
    ///
    /// ADR 0131 Phase 3c (BT-3822): reached only from the legacy arm of a
    /// conditional that is not a local-threading producer (it writes no outer
    /// local, so no `BranchArm` frame is registered), whose nested loop or
    /// fold writes a local *of that arm*; pinned by
    /// `adr0131cond_non_producer_test.bt`. A conditional in a loop or fold
    /// body that writes an outer local is a producer in that frame and never
    /// gets here. Deleting this (BT-3833) needs the arms of every non-producer
    /// conditional to be frames too (follow-up).
    fn push_control_flow_threaded_var_rereads(
        &mut self,
        source_expr: &Expression,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let Some(threaded_vars) = self
            .threaded_locals_of(source_expr)
            .and_then(ThreadedLocals::into_lowered)
        else {
            return;
        };
        let new_state_name = self.current_state_var();
        let re_reads = self.rebind_threaded_vars_from_state(&threaded_vars, &new_state_name);
        if !re_reads.is_empty() {
            stmts.push(ThreadedStmt::Statement(Document::Vec(re_reads), span));
        }
    }

    /// ADR 0111 Addendum 5, Rule 3: wraps a branch arm's real `Bind`/`Statement`
    /// sequence in one node, `verify()`s it and `render()`s it. With an
    /// [`ArmFrame`] the arm is a [`ThreadedStmt::BranchArm`] node recording the
    /// construct's seeded `threads` (ADR 0131 §2), so the verifier reads the
    /// frame's rebind shapes off the node (a conditional arm, and since BT-3939
    /// a Tier 2 block body, `generate_block_stateful`); without one it is the
    /// legacy `Threaded` wrapper (`StateAcc` mode, a transparent pass-through
    /// whose only job is giving `verify()` the arm's non-ROOT frame), which
    /// threads nothing of its own.
    pub(in crate::core_erlang) fn render_branch_arm(
        &mut self,
        stmts: Vec<ThreadedStmt>,
        frame: FrameId,
        final_version: usize,
        span: Span,
        arm: Option<&ArmFrame>,
    ) -> (Document<'static>, usize) {
        if let Some(arm) = arm {
            let wrapper = vec![threaded_ir::build_branch_arm(
                frame,
                arm.threads.clone(),
                stmts,
            )];
            let errors = self.verify_threaded_ir(&wrapper);
            self.report_threaded_ir_verify_errors(
                &errors,
                "conditional branch arm ThreadedIr must be well-formed",
                span,
            );
            let mut ctx = threaded_ir::RenderCtx::new(self);
            let doc = threaded_ir::render(&wrapper, &mut ctx);
            return (doc, final_version);
        }
        let produces = if final_version > 0 {
            vec![VersionedVar::new(
                VersionPrefix::State,
                final_version,
                frame,
            )]
        } else {
            Vec::new()
        };
        let wrapper = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame,
            threads: self.registered_state_acc_threads(frame),
            body: stmts,
            produces,
            span,
        }];
        let errors = self.verify_threaded_ir(&wrapper);
        self.report_threaded_ir_verify_errors(
            &errors,
            "conditional branch arm ThreadedIr must be well-formed",
            span,
        );
        let mut ctx = threaded_ir::RenderCtx::new(self);
        let doc = threaded_ir::render(&wrapper, &mut ctx);
        (doc, final_version)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_erlang::tests::codegen;

    // ── ADR 0111 Addendum 5: NonLinearVersion / UnboundVersion ──
    // are now LIVE checks for branch-arm IR (previously scaffolding-only —
    // `check_branch_frame_linearity`'s scalar synthesis could never
    // construct two arms colliding on the same frame, by construction).
    // Both regressions below are built through the exact production
    // types/constructors `generate_conditional_branch_inline`'s lowering
    // uses — a real `CoreErlangGenerator::with_branch_context` call for
    // frame allocation (`current_branch_frame`), real `VersionedVar`/
    // `ThreadedStmt::Bind`/`BindOp::Put` construction — not an isolated
    // hand-fixture disconnected from any real construction path.

    #[test]
    fn test_bt3146_nonlinear_version_detected_via_production_lowering_types() {
        let mut generator = CoreErlangGenerator::new("bt3146_regression_nonlinear");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // Two field-mutation Binds that BOTH (incorrectly) target
            // State(1)@frame from State(0)@frame — the exact shape a
            // broken lowering (e.g. forgetting to call `next_state_var()`
            // between two field assignments in the same arm) would
            // produce. Mirrors C1's real `BindOp::Put` shape exactly.
            let source = VersionedVar::new(VersionPrefix::State, 0, frame);
            let target = VersionedVar::new(VersionPrefix::State, 1, frame);
            let make_put = |field: &str, val: &str, target: VersionedVar, source: VersionedVar| {
                ThreadedStmt::Bind {
                    target,
                    source,
                    op: BindOp::Put {
                        field: field.to_string(),
                        value: ValueRef::Var(val.to_string()),
                    },
                    span: Span::default(),
                }
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                threads: Vec::new(),
                body: vec![
                    make_put("n", "_Val1", target.clone(), source.clone()),
                    make_put("n", "_Val2", target.clone(), source),
                ],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors.iter().any(|e| matches!(
                e,
                threaded_ir::VerifyError::NonLinearVersion { producers: 2, .. }
            )),
            "expected NonLinearVersion(producers: 2) for the duplicate \
             State(1) producer, got: {errors:?}"
        );
    }

    #[test]
    fn test_bt3146_unbound_version_detected_via_production_lowering_types() {
        let mut generator = CoreErlangGenerator::new("bt3146_regression_unbound");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // A Bind whose source references a version this frame never
            // produced — the exact shape a broken lowering (e.g. reading a
            // stale `state_version()` snapshot from before an earlier
            // mutation actually landed) would produce.
            let phantom_source = VersionedVar::new(VersionPrefix::State, 5, frame);
            let target = VersionedVar::new(VersionPrefix::State, 6, frame);
            let bind = ThreadedStmt::Bind {
                target: target.clone(),
                source: phantom_source,
                op: BindOp::Put {
                    field: "n".to_string(),
                    value: ValueRef::Var("_Val1".to_string()),
                },
                span: Span::default(),
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                threads: Vec::new(),
                body: vec![bind],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors.iter().any(|e| matches!(
                e,
                threaded_ir::VerifyError::UnboundVersion { var, .. } if var.version == 5
            )),
            "expected UnboundVersion for the phantom State(5) source, got: {errors:?}"
        );
    }

    #[test]
    fn test_if_true_mutation_bypasses_runtime_dispatch() {
        // Actor ifTrue: with field mutation compiles to inline case (not runtime dispatch).
        // Also verifies the non-taken (false) branch returns {'nil', State}.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  inc: flag =>\n    flag ifTrue: [self.n := self.n + 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue: with field mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "True branch should update 'n' via maps:put. Got:\n{code}"
        );
        // The untyped `self.n + 1` keeps its operator guard (whose fallback
        // sends `+`, BT-3925); the conditional itself is never sent.
        assert!(
            !code.contains(", 'ifTrue:', ["),
            "ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
        // Non-taken branch returns {'nil', unchanged_state} with StateAcc naming
        assert!(
            code.contains("{'nil',"),
            "Non-taken branch should return {{'nil', State}}. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "Branch bodies should use StateAcc naming. Got:\n{code}"
        );
    }

    #[test]
    fn test_nested_list_op_in_branch_threads_outer_local() {
        // Case (B): a nested list op (`do:`) inside an `ifTrue:` branch
        // mutates an outer local. The conditional must be recognised as
        // state-threading (via the nested cross-scope mutation), the local must be
        // seeded, packed by the nested op, and extracted after the conditional.
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: x = 0\n\n",
            "  run: flag =>\n",
            "    sum := 0\n",
            "    flag ifTrue: [#(1, 2, 3) do: [:i | sum := sum + i]]\n",
            "    sum\n",
        );
        let code = codegen(src);
        // The conditional must compile to an inline case, NOT a runtime dispatch.
        // A runtime `send(_, 'ifTrue:', [Fun])` returns `nil` on the false branch, so
        // the sequencer's `element(2, _)` unpack crashes with badarg (regression).
        //
        // ADR 0087 Phase 2: `register_class/0` now bakes a methodXref
        // index that lists `ifTrue:` as a sent selector (metadata, not dispatch).
        // Exclude that line so the assertion still checks only for a real runtime
        // dispatch of the selector.
        let code_no_xref: String = code
            .lines()
            .filter(|line| !line.contains("'methodXref'"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            !code_no_xref.contains("'ifTrue:'"),
            "nested-op-in-branch must inline ifTrue: (no runtime 'ifTrue:' dispatch). Got:\n{code}"
        );
        // The outer local 'sum' is seeded into the StateAcc before the branch.
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "nested-op-in-branch should pack/seed '__local__sum'. Got:\n{code}"
        );
        // The method body must extract the threaded 'sum' back after the conditional.
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "nested-op-in-branch should extract 'sum' via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_false_mutation_threads_state_in_false_arm() {
        // Actor ifFalse: with field mutation compiles to inline case with false branch mutating
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  dec: flag =>\n    flag ifFalse: [self.n := self.n - 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifFalse: with field mutation should generate inline case. Got:\n{code}"
        );
        // False branch maps:put happens in the '<false>' arm
        assert!(
            code.contains("maps':'put'('n'"),
            "False branch should update 'n' via maps:put. Got:\n{code}"
        );
        // True arm returns nil with unchanged state, as {'nil', State}
        assert!(
            code.contains("{'nil',"),
            "True arm of ifFalse: should return {{'nil', State}}. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_if_false_with_mutations_generates_two_threaded_branches() {
        // ifTrue:ifFalse: with mutations in both branches generates two threaded arms
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.n := 1] ifFalse: [self.n := 0].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue:ifFalse: with mutations should generate inline case. Got:\n{code}"
        );
        // maps:put should appear at least twice (once per branch)
        let put_count = code.matches("maps':'put'('n'").count();
        assert!(
            put_count >= 2,
            "Both branches should call maps:put for 'n'. Found {put_count}. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_write_only_conditional_seeds_and_extracts_local() {
        // `flag ifTrue: [m := 9]` then read `m` (non-last). The outer
        // local `m` must thread back even though the block only writes it.
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    val := 0\n    flag ifTrue: [val := 9]\n    val\n";
        let code = codegen(src);
        // The non-taken branch and extraction need the seeded key.
        assert!(
            code.contains("maps':'put'('__local__val'"),
            "write-only conditional should seed/put '__local__val'. Got:\n{code}"
        );
        // After the conditional, the local is read back out of the threaded state.
        assert!(
            code.contains("maps':'get'('__local__val'"),
            "outer local 'val' should be extracted via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_read_write_conditional_extracts_local() {
        // `flag ifTrue: [sum := sum + 7]` then read `sum` (non-last).
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    sum := 0\n    flag ifTrue: [sum := sum + 7]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "outer local 'sum' should be extracted via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_if_true_if_false_seeds_local_for_both_branches() {
        // a local written in only one branch must still be extractable, so
        // the seed key must precede the case (be present in both branches' base).
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    x := 1\n    flag ifTrue: [x := x + 1] ifFalse: [x := x + 100]\n    x\n";
        let code = codegen(src);
        // One seed put (before the case) + one put per branch = 3 puts of '__local__x'.
        let put_count = code.matches("maps':'put'('__local__x'").count();
        assert!(
            put_count >= 3,
            "expected a seed put plus a put in each branch for '__local__x' (>=3), found {put_count}. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__x'"),
            "outer local 'x' should be extracted after ifTrue:ifFalse:. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_true_block_reads_back_correctly() {
        // Local var assigned inside ifTrue: block must be readable in subsequent
        // expressions of the same block without a {badkey,VarName} runtime crash.
        // `y` is a local of the arm, which the arm's frame does not carry
        // (ADR 0133 Phase 3, BT-3911): it is a plain `let`, never written into
        // the scratch map under a `__local__y` key; reads resolve to the temp
        // var, not 'y'.
        let src = "Actor subclass: BrokenActor\n  state: x = 5\n\n  myMethod: cond =>\n    cond ifTrue: [\n      y := self.x + 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        assert!(
            !code.contains("__local__y"),
            "An arm-local 'y' must not be written into the scratch map. Got:\n{code}"
        );
        // The field assignment self.x := y should NOT use maps:get('y', ...) — that would
        // be the buggy read (key mismatch). Instead, it should reference the temp var directly.
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_false_block_reads_back_correctly() {
        // Same fix applies to ifFalse: blocks.
        let src = "Actor subclass: TestActor\n  state: x = 10\n\n  myMethod: cond =>\n    cond ifFalse: [\n      y := self.x - 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        assert!(
            !code.contains("__local__y"),
            "An arm-local 'y' must not be written into the scratch map. Got:\n{code}"
        );
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_local_mutation_generates_inline_case() {
        // Value type ifTrue: with captured local mutation should
        // generate an inline case expression (not runtime dispatch).
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue: with local mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            !code.contains("'beamtalk_message_dispatch':'send'"),
            "Value type ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_false_local_mutation_generates_inline_case() {
        // Value type ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifFalse: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_if_false_local_mutation() {
        // Value type ifTrue:ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2] ifFalse: [x := 3]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue:ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_collect_wrapping_if_true_with_field_mutation() {
        // collect: block containing ifTrue: with self.field := mutation.
        // The field mutation inside the conditional must be threaded through the
        // collect: loop accumulator, not silently lost.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list collect: [:each | each > 0 ifTrue: [self.n := self.n + 1]. each * 2]\n    self.n\n";
        let code = codegen(src);
        // The collect: should use a stateful accumulator (maps:put for field 'n')
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: collect: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_wrapping_do_with_field_mutation() {
        // ifTrue: wrapping do: block with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: ifTrue: wrapping do: should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: inside ifTrue: should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_do_wrapping_if_true_with_field_mutation() {
        // do: block containing ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_triple_nested_if_true_do_if_true_with_field_mutation() {
        // ifTrue: wrapping do: wrapping ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: triple-nested should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: triple-nested should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_field_assignment_control_flow_rhs_unpacks_tuple() {
        // self.field := <control-flow-with-mutations> must unpack {Value, State}
        let src = "Actor subclass: A\n  state: x = 0\n  state: y = 0\n\n  m: flag =>\n    self.x := flag ifTrue: [self.y := 1. 42] ifFalse: [0]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "FieldAssignmentControlFlow should unpack element(1) from RHS tuple. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "FieldAssignmentControlFlow should unpack element(2) for state. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('x'"),
            "Should update field 'x' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_conditional_branch() {
        // SelfFieldAtPut inside conditional branch must not fall through to wildcard
        let src = "Actor subclass: A\n  state: x = 0\n\n  m: flag =>\n    flag ifTrue: [self fieldAt: #x put: 42]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut in conditional branch should generate maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_method_body() {
        // self fieldAt: name put: value in method body (non-conditional context)
        let src = "Actor subclass: A\n  state: x = 0\n\n  m =>\n    self fieldAt: #x put: 42\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut should generate maps:put. Got:\n{code}"
        );
    }
}
