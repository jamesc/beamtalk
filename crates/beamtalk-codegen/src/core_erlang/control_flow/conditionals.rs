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
//!     {_Cond1, State0}
//!   <_CondNoMatch2> when 'true' ->
//!     call 'erlang':'error'({'case_clause', _CondNoMatch2})
//! end
//! ```
//!
//! The caller (method body generator) unpacks `{Result, NewState}` via
//! `element/2` to thread the new state to subsequent expressions.
//!
//! The non-taken arm answers the receiver (`_Cond1`), as dynamic dispatch
//! does (`False>>ifTrue: => self`); `and:`/`or:` answer `false`/`true`. Both
//! the pure and the mutation builders read that answer from one table,
//! [`SoloBoolean`] (BT-4189).
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

use super::super::synced_call::ActorStateSource;
use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{
    self, BindOp, FrameId, RebindFrameKind, StateReadSite, ThreadedStmt, ThreadedValue,
    ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenContext, CoreErlangGenerator, Result};
use super::analysis::ThreadedFamilies;
use super::family_slots::{self, FamilyVersionStep};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};
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
    /// module's own `lower_field_assignment_bind`) — AGENTS.md's
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
        if site == FieldWriteSite::Actor {
            // BT-4369: a loop that brackets admits no field write (ADR 0137 §4b).
            self.note_field_write(field_name, span);
        }
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
    /// own `current_branch_frame()` for a loop/fold-body field mutation, or
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
    /// A single-block conditional's non-taken answer ([`SoloAnswer`]).
    Answer(SoloAnswer),
}

/// BT-4189: what a single-block boolean conditional ([`SoloBoolean`])
/// answers on the arm that does NOT run its block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum SoloAnswer {
    /// The receiver itself (`C`) — the non-taken solo-branch contract
    /// (`False>>ifTrue: => self`, `True>>ifFalse: => self`).
    Receiver,
    /// A boolean literal atom — `and:`'s/`or:`'s short-circuit arm
    /// (`Boolean>>and:`/`or:`, `stdlib/src/boolean.bt`). Stored as a
    /// `bool` and emitted via `leaf::atom` (typed leaves, ADR 0089).
    Literal(bool),
}

impl SoloAnswer {
    /// The answer as a Core Erlang value, `cond_var` naming the
    /// once-evaluated receiver. The one emission both the pure builder
    /// ([`CoreErlangGenerator::generate_pure_boolean_case`]) and the
    /// mutation builder
    /// ([`CoreErlangGenerator::generate_solo_boolean_with_mutations_tuple`])
    /// use.
    pub(in crate::core_erlang) fn doc(self, cond_var: &str) -> Document<'static> {
        match self {
            SoloAnswer::Receiver => leaf::var(cond_var),
            SoloAnswer::Literal(value) => leaf::atom(if value { "true" } else { "false" }),
        }
    }

    /// The Boolean this answer evaluates to on a Boolean `receiver`. Test
    /// oracles that model a conditional's result on a BEAM read it here
    /// rather than restating the rule (BT-4189).
    #[cfg(test)]
    pub(in crate::core_erlang) fn evaluate(self, receiver: bool) -> bool {
        match self {
            SoloAnswer::Receiver => receiver,
            SoloAnswer::Literal(value) => value,
        }
    }
}

/// The two-block boolean conditional's selector, as the atom its non-Boolean
/// fallback send names ([`CoreErlangGenerator::boolean_fallback_send`]).
pub(in crate::core_erlang) const IF_TRUE_IF_FALSE: &str = "ifTrue:ifFalse:";

/// BT-4244: the hint of the `stateful_block_dispatch` error a conditional's
/// fallback closure raises when a non-Boolean receiver's method calls an arm
/// whose state effects only the inline conditional can thread
/// ([`CoreErlangGenerator::fallback_block_doc`]).
pub(super) const FALLBACK_WRITING_BLOCK_HINT: &str =
    "A non-Boolean receiver called a conditional's block that only runs inline";

/// BT-4189: the single-block boolean conditionals — `ifTrue:`, `ifFalse:`,
/// `and:`, `or:` — and the ONE table of which receiver arm runs the block
/// ([`SoloBoolean::block_on_true`]) and what the other arm answers
/// ([`SoloBoolean::non_taken`]). Both the pure-inline path (BT-4070,
/// [`CoreErlangGenerator::generate_solo_boolean_pure`]) and the
/// mutation/threading path
/// ([`CoreErlangGenerator::generate_solo_boolean_with_mutations_tuple`]) read
/// it, so a non-taken arm can never answer differently depending on whether
/// the block is pure (before BT-4189 the mutation path hard-coded `nil` for a
/// non-taken `ifTrue:`/`ifFalse:`, where dispatch answers the receiver).
///
/// The table follows the stdlib definitions dynamic dispatch runs:
/// `False>>ifTrue: => self`, `True>>ifFalse: => self` (`stdlib/src/false.bt`,
/// `true.bt`), and `Boolean>>and:`/`or:` (`stdlib/src/boolean.bt`). It also
/// names the selector atom ([`SoloBoolean::selector_atom`]), so a builder
/// that falls back to a dynamic send for a non-Boolean receiver (the pure
/// path's wildcard arm; BT-4244 for the mutation path) sends the same
/// message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum SoloBoolean {
    /// `R ifTrue: [B]`
    IfTrue,
    /// `R ifFalse: [B]`
    IfFalse,
    /// `R and: [B]`
    And,
    /// `R or: [B]`
    Or,
}

impl SoloBoolean {
    /// Every variant, for table-driven tests.
    #[cfg(test)]
    pub(in crate::core_erlang) const ALL: [SoloBoolean; 4] =
        [Self::IfTrue, Self::IfFalse, Self::And, Self::Or];

    /// The variant for a one-keyword selector, or `None` for any other.
    pub(in crate::core_erlang) fn from_keyword(keyword: &str) -> Option<Self> {
        match keyword {
            "ifTrue:" => Some(Self::IfTrue),
            "ifFalse:" => Some(Self::IfFalse),
            "and:" => Some(Self::And),
            "or:" => Some(Self::Or),
            _ => None,
        }
    }

    /// The selector, as the atom a dynamic send names.
    pub(in crate::core_erlang) fn selector_atom(self) -> &'static str {
        match self {
            Self::IfTrue => "ifTrue:",
            Self::IfFalse => "ifFalse:",
            Self::And => "and:",
            Self::Or => "or:",
        }
    }

    /// `true` when the block runs on a `true` receiver (`ifTrue:`, `and:`).
    pub(in crate::core_erlang) fn block_on_true(self) -> bool {
        matches!(self, Self::IfTrue | Self::And)
    }

    /// What the arm that does not run the block answers.
    pub(in crate::core_erlang) fn non_taken(self) -> SoloAnswer {
        match self {
            Self::IfTrue | Self::IfFalse => SoloAnswer::Receiver,
            Self::And => SoloAnswer::Literal(false),
            Self::Or => SoloAnswer::Literal(true),
        }
    }

    /// `(true_arm, false_arm)` of the pure-inline `case`, built from
    /// [`Self::block_on_true`] and [`Self::non_taken`].
    pub(in crate::core_erlang) fn pure_arms(self, block: &Block) -> (PureArm<'_>, PureArm<'_>) {
        let answer = PureArm::Answer(self.non_taken());
        if self.block_on_true() {
            (PureArm::Inline(block), answer)
        } else {
            (answer, PureArm::Inline(block))
        }
    }
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
    /// Each branch — including the synthetic non-taken branch (`{<answer>, State}`)
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
            // BT-4180: inside a bracketed region (a loop-condition closure of
            // a loop that brackets guarded operators) that is the cell: the
            // captured entry `State` misses the slow paths' writes.
            ScratchStart::Fresh if self.closure_scratch_continues_state(self.current_frame()) => {
                let at = blocks.first().map_or_else(Span::default, |b| b.span);
                match self.actor_state_source(StateReadSite::BranchScratchSeed, at) {
                    ActorStateSource::Cell => threaded_ir::cell_take_call(),
                    ActorStateSource::Var(var) => leaf::var(var),
                }
            }
            ScratchStart::Fresh => Document::Str("call 'maps':'new'()"),
        };
        // Fold from the last var inward so the emitted nesting reads naturally.
        for var in threaded.iter().rev() {
            let core_var = self.resolve_local_core_var(var);
            chain = docvec![
                "call 'maps':'put'(",
                leaf::atom(self.local_scratch_key(var)),
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

    /// BT-5281: the base `State` a construct that threads no outer local of
    /// its own (a non-producer `match:` whose arms thread `State`) starts its
    /// arms from in `frame`: the frame's `State` where it continues one, else
    /// a private scratch seeded as a conditional's is there
    /// ([`ScratchStart::Fresh`]: the captured `State` or the cell in a plain
    /// closure body, an empty map in a direct-params or tuple-acc loop or fold
    /// body, which binds no `StateAcc`). Returns the seed's `let` (empty when
    /// none is needed) and the base variable.
    pub(in crate::core_erlang) fn construct_base_state(
        &mut self,
        frame: FrameId,
    ) -> (Document<'static>, String) {
        if self.conditional_has_private_scratch(frame) {
            self.seed_scratch_locals(&[], ScratchStart::Fresh, "")
        } else {
            (Document::Nil, self.current_state_var())
        }
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
    /// boolean/nil value to test. Shared by all the `generate_*_with_mutations`
    /// generators below (previously each hand-duplicated this exact match —
    /// AGENTS.md's no-duplicate-implementations rule). `frame` is the
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
    pub(super) fn compile_conditional_receiver(
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
        let mut tv = self.threaded_expression(receiver, frame)?;
        // BT-4415: a rewritten guard's condition is decided by
        // `beamtalk_guard_return` for a non-Boolean receiver.
        if self.is_guard_condition(receiver) {
            let value_doc = self.threaded_value_doc(&tv.value);
            tv.value = ValueRef::Doc(self.guard_condition_doc(receiver, value_doc));
        }
        Ok(tv)
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
            || self.is_construct_with_sequenced_operand(expr)
            // BT-3982: an actor evaluated block that threads `State` is a
            // producer in every arm.
            || self
                .actor_evaluated_block_producer_parts(expr)
                .is_some()
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
                RebindFrameKind::Loop(ThreadingMode::DirectParams | ThreadingMode::TupleAcc(..))
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
        let value = ValueRef::Doc(docvec![form::element(1, leaf::var(carrier.clone())),]);
        // The method root and a registered frame (a `BranchArm`, a loop or fold
        // body) lower the construct's `LocalRebind`s.
        let rebinds_locals = self.lowers_as_conditional_producer(frame, producer);
        let locals = if rebinds_locals {
            self.branch_threaded_locals(arms)
        } else {
            Vec::new()
        };
        let family_binds = self.extract_state_family(&carrier, frame, span);
        // BT-4461: in the REPL the construct adopts the bindings map whole; the
        // bindings its frame holds in variables that it writes are read back
        // out of that map (`repl_adoption_prelude`).
        let cached = if rebinds_locals {
            Vec::new()
        } else {
            self.repl_producer_cached_bindings_of_blocks(arms)
        };
        if !cached.is_empty() {
            let prelude = self.repl_adoption_prelude(
                &carrier,
                tuple_doc,
                &cached,
                family_binds,
                frame,
                span,
            )?;
            return Ok(ThreadedValue { prelude, value });
        }
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

    /// [`Self::conditional_tuple_to_threaded_value`] for a `match:` that
    /// threads no outer local of its own but whose arms thread the actor's
    /// `State` (`match_needs_state_threading`).
    ///
    /// BT-5281: in a frame whose scratch map is private
    /// ([`Self::conditional_has_private_scratch`]: a plain closure body or a
    /// direct-params/tuple-acc loop or fold body of an actor method) there is
    /// no `State` family to continue, so the tuple's element 2 is adopted by
    /// nothing ([`Self::conditional_families`] is empty there), exactly as for
    /// a conditional producer in that frame. Field writes in such a frame do
    /// not ride that element: a closure writes through the actor-state cell
    /// (ADR 0137), and a loop that writes a field threads `State` and is not
    /// direct-params.
    pub(super) fn control_flow_tuple_to_threaded_value(
        &mut self,
        tuple_doc: Document<'static>,
        frame: FrameId,
        span: Span,
    ) -> Result<ThreadedValue> {
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
    pub(super) fn extract_state_family(
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
    /// rather than each of the `generate_*_with_mutations` producers
    /// hand-rolling its own `"{lit, ", base_state, "}"` `Document` splice.
    /// Byte-identical to the hand-rolled shape: `append_family_slots`'s
    /// `base` here always carries one prior element (`value_doc`), so the
    /// helper always prepends `", "` before the (sole) family slot — the
    /// exact `"{<answer>, "` + var + `"}"` splice every call site built by hand.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_erlang::control_flow::StateAccFallbackReason;
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
        // Also verifies the non-taken (false) branch answers the receiver,
        // `{_CondN, State}` (BT-4189: `False>>ifTrue: => self`, not `nil`).
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
        // sends `+`, BT-3925); the conditional itself is sent only for a
        // non-Boolean receiver (BT-4244).
        crate::core_erlang::tests::assert_selector_sent_only_by_fallback(&code, "ifTrue:");
        // Non-taken branch answers the receiver with unchanged state.
        assert_non_taken_answers_receiver(&code, "false");
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
        // The only send is the non-Boolean receiver's fallback (BT-4244), whose
        // arm answers the `{Result, State}` shape too.
        crate::core_erlang::tests::assert_selector_sent_only_by_fallback(&code, "ifTrue:");
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
        // True arm answers the receiver with unchanged state (BT-4189:
        // `True>>ifFalse: => self`, not `nil`).
        assert_non_taken_answers_receiver(&code, "true");
    }

    /// BT-4189: the `<'arm'>` clause of a single-block conditional answers
    /// `{_CondN, State}` — the receiver the `case` scrutinises — and no
    /// non-taken arm answers `nil`.
    fn assert_non_taken_answers_receiver(code: &str, arm: &str) {
        let head = format!("<'{arm}'> when 'true' -> {{");
        let at = code
            .find(&head)
            .unwrap_or_else(|| panic!("no `{head}` non-taken arm. Got:\n{code}"));
        let answer = code[at + head.len()..]
            .split(',')
            .next()
            .unwrap_or_default()
            .trim();
        assert!(
            answer.starts_with("_Cond") && code.contains(&format!("case {answer} of")),
            "non-taken `{arm}` arm should answer the receiver `case` var, got `{answer}`. Got:\n{code}"
        );
        assert!(
            !code.contains("{'nil',"),
            "no non-taken arm should answer nil. Got:\n{code}"
        );
    }

    /// BT-4244: the wildcard clause of `code`'s (first) mutation-path
    /// conditional, from `CondNoMatch` to the `case`'s `end`.
    /// BT-4244: `(depth, CondNoMatch count, code bytes)` for each nesting
    /// depth 1..=6 of `wrap(level, inner)` nested conditionals, in a class
    /// body built by `class(body)`.
    fn nested_sizes(
        class: impl Fn(&str) -> String,
        wrap: impl Fn(usize, &str) -> String,
    ) -> Vec<(usize, usize, usize)> {
        (1..=6)
            .map(|depth| {
                let mut body = String::from("0");
                for level in (0..depth).rev() {
                    body = wrap(level, &body);
                }
                let code = codegen(&class(&body));
                // The arms reach the fallback as real closures (the shape in
                // question), not raising stubs.
                assert!(
                    !code.contains("'stateful_block_dispatch'"),
                    "depth {depth}: expected real fallback closures:\n{code}"
                );
                (depth, code.matches("CondNoMatch").count(), code.len())
            })
            .collect()
    }

    /// BT-4244: no doubling per level. One fallback clause per level (a
    /// nested conditional lowered inline again inside each enclosing
    /// fallback closure would double the clauses), and each level's added
    /// bytes at most 1.5x the previous level's (doubling would be 2x; each
    /// fallback closure holding its subtree once, as dynamic sends, is the
    /// quadratic shape the pure path has too, whose step ratio falls
    /// towards 1).
    fn assert_grows_linearly(label: &str, sizes: &[(usize, usize, usize)]) {
        eprintln!("{label}: (depth, CondNoMatch, bytes) = {sizes:?}");
        for &(depth, clauses, _) in sizes {
            assert_eq!(
                clauses, depth,
                "{label}: one fallback clause per level: {sizes:?}"
            );
        }
        let steps: Vec<usize> = sizes.windows(2).map(|p| p[1].2 - p[0].2).collect();
        for pair in steps.windows(2) {
            assert!(
                2 * pair[1] <= 3 * pair[0],
                "{label}: byte growth per level rises from {} to {} (doubling?): {sizes:?}",
                pair[0],
                pair[1]
            );
        }
    }

    /// BT-4244: a mutation-path conditional nested in another's arm is
    /// compiled inline and again inside the outer fallback closure, so the
    /// code must not double per nesting level (a fallback closure's own
    /// nested conditionals must not repeat the whole tree). Pins linear
    /// growth in actor (field-reading arms, passed as real closures at the
    /// method root), value-type and class-method (arms writing block temps
    /// inside a loop body) code; the REPL shape is
    /// `crates/beamtalk-repl/tests/repl_nested_conditional_size.rs`.
    #[test]
    fn nested_mutation_conditionals_grow_linearly() {
        let actor = nested_sizes(
            |body| format!("Actor subclass: C\n  state: n = 0\n\n  run: x =>\n    {body}\n"),
            |level, inner| format!("x ifTrue: [self.n. {inner}. {level}]"),
        );
        assert_grows_linearly("actor", &actor);
        assert!(
            actor[0].1 >= 1,
            "the actor shape takes the mutation path: {actor:?}"
        );
        let value = nested_sizes(
            |body| format!("Value subclass: C\n  run: x =>\n    #(1) do: [:i | {body}]\n"),
            |level, inner| format!("x ifTrue: [y{level} := i. {inner}. y{level}]"),
        );
        assert_grows_linearly("value type", &value);
        let class = nested_sizes(
            |body| format!("Object subclass: C\n  class run: x =>\n    #(1) do: [:i | {body}]\n"),
            |level, inner| format!("x ifTrue: [y{level} := i. {inner}. y{level}]"),
        );
        assert_grows_linearly("class method", &class);
    }

    fn fallback_clause(code: &str) -> &str {
        let start = code
            .find("<_CondNoMatch@")
            .unwrap_or_else(|| panic!("no CondNoMatch clause:\n{code}"));
        let rest = &code[start..];
        &rest[..rest.find(" end").unwrap_or(rest.len())]
    }

    /// BT-4244: in every context the mutation path's wildcard sends the
    /// selector (never a raw `case_clause`), as the pure path's does.
    #[test]
    fn mutation_fallback_sends_the_selector_in_every_context() {
        let cases = [
            (
                "actor field",
                "Actor subclass: C\n  state: n = 0\n\n  run: x =>\n    x ifTrue: [self.n := 1]\n    self.n\n",
                "ifTrue:",
            ),
            (
                "actor local",
                "Actor subclass: C\n  state: n = 0\n\n  run: x =>\n    t := 0\n    x ifFalse: [t := 1]\n    t\n",
                "ifFalse:",
            ),
            (
                "class local",
                "Object subclass: C\n  class run: x =>\n    t := 0\n    r := x ifTrue: [t := 1] ifFalse: [t := 2]\n    #[r, t]\n",
                "ifTrue:ifFalse:",
            ),
            (
                "value-type local",
                "Value subclass: C\n  run: x =>\n    t := 0\n    r := x ifTrue: [t := 1]\n    #[r, t]\n",
                "ifTrue:",
            ),
            (
                "value-type field",
                "TestCase subclass: C\n  field: n = 0\n\n  run: x =>\n    r := x ifTrue: [self.n := 1]\n    r\n",
                "ifTrue:",
            ),
        ];
        for (label, src, selector) in cases {
            let code = codegen(src);
            assert!(
                !code.contains("{'case_clause', _CondNoMatch"),
                "{label}: the wildcard must not raise case_clause:\n{code}"
            );
            crate::core_erlang::tests::assert_selector_sent_only_by_fallback(&code, selector);
        }
    }

    /// BT-4244: an actor field-writing arm reaches the non-Boolean receiver's
    /// method as a cell closure, and the send is bracketed: store the
    /// scratch map, send, answer the reloaded cell as the arm's state.
    #[test]
    fn actor_field_fallback_brackets_the_send() {
        let code = codegen(
            "Actor subclass: C\n  state: n = 0\n\n  run: x =>\n    x ifTrue: [self.n := 1]\n    self.n\n",
        );
        let clause = fallback_clause(&code);
        let store = clause.find("'beamtalk_actor_state':'store'(");
        let send = clause.find("'beamtalk_message_dispatch':'send'(");
        let take = clause.find("'beamtalk_actor_state':'take'()");
        assert!(
            store < send && send < take && store.is_some(),
            "store, send, take in order:\n{clause}"
        );
        assert!(
            clause.contains("'beamtalk_actor_state':'put'(Self, 'n'"),
            "the closure writes the field through the cell:\n{clause}"
        );
    }

    /// BT-4244: an arm that writes a captured local or a value-type field
    /// cannot run as a closure, so the fallback passes a raising stub (and
    /// no bracket, the stub touches nothing).
    #[test]
    fn unthreadable_arm_fallback_passes_a_raising_stub() {
        for src in [
            "Actor subclass: C\n  state: n = 0\n\n  run: x =>\n    t := 0\n    x ifTrue: [t := 1]\n    t\n",
            // A local write that also reads a field: still a stub, so still
            // no bracket (and no exit store for the method).
            "Actor subclass: C\n  state: count :: Integer = 0\n\n  run: flag =>\n    n := 0\n    flag ifTrue: [n := self.count + 1]\n    n\n",
            "TestCase subclass: C\n  field: n = 0\n\n  run: x =>\n    r := x ifTrue: [self.n := 1]\n    r\n",
        ] {
            let code = codegen(src);
            let clause = fallback_clause(&code);
            assert!(
                clause.contains("'stateful_block_dispatch'"),
                "the stub raises stateful_block_dispatch:\n{clause}"
            );
            assert!(
                !clause.contains("'beamtalk_actor_state':'store'("),
                "a stub-only fallback is not bracketed:\n{clause}"
            );
            assert!(
                !code.contains("_ExitReply"),
                "a stub-only fallback adds no exit store:\n{code}"
            );
        }
    }

    /// BT-4189: the one `SoloBoolean` table agrees with the stdlib
    /// definitions dynamic dispatch runs, and the pure arms are built from
    /// it (exactly one arm runs the block).
    #[test]
    fn solo_boolean_table_matches_dispatch() {
        for sel in SoloBoolean::ALL {
            assert_eq!(SoloBoolean::from_keyword(sel.selector_atom()), Some(sel));
            let expected = match sel {
                SoloBoolean::IfTrue => (true, SoloAnswer::Receiver),
                SoloBoolean::IfFalse => (false, SoloAnswer::Receiver),
                SoloBoolean::And => (true, SoloAnswer::Literal(false)),
                SoloBoolean::Or => (false, SoloAnswer::Literal(true)),
            };
            assert_eq!((sel.block_on_true(), sel.non_taken()), expected, "{sel:?}");
            let block = Block::new(Vec::new(), Vec::new(), Span::default());
            let (t, f) = sel.pure_arms(&block);
            let inline = |a: PureArm<'_>| matches!(a, PureArm::Inline(_));
            assert_eq!(
                (inline(t), inline(f)),
                (sel.block_on_true(), !sel.block_on_true())
            );
        }
        assert_eq!(SoloBoolean::from_keyword("ifTrue:ifFalse:"), None);
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
        crate::core_erlang::tests::assert_selector_sent_only_by_fallback(&code, "ifTrue:");
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
