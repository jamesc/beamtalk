// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Rebind frames, body / family-only producer sites, local-assignment
//! producer lowering and the verified root preludes (ADR 0131), split from
//! `threaded_expr.rs`.

use super::threaded_ir::{
    self, BindOp, CarrierSlot, FrameId, RebindFrame, RebindFrameKind, RebindLowering, RebindShape,
    ThreadedLocalSlot, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

#[cfg(any(test, feature = "test"))]
use super::fault;
use super::threaded_expr::ProducerTail;

/// Which sites of an actor loop or fold threading only `State` a frame body
/// registers beyond the core rule's (`is_loop_body_producer_site`): both,
/// except under a test-only fault that withholds one kind.
#[derive(Debug, Clone, Copy)]
struct FamilyOnlySites {
    /// BT-4287: the value-bearing sites the core rule leaves out under a fold
    /// frame (`is_family_only_fold_value_site`).
    value_sites: bool,
    /// BT-4310: the operands the sequencing rule leaves to another lowering
    /// (`family_only_operand_site_exprs`).
    unsequenced_operands: bool,
}

impl FamilyOnlySites {
    const ALL: Self = Self {
        value_sites: true,
        unsequenced_operands: true,
    };
}

impl CoreErlangGenerator {
    /// BT-4629: the span of the body block of `expr` when it is a plain
    /// `do:` / `doWithKey:` send with a literal block, the only folds whose
    /// closed tuple form can drop the carrier map ([`Self::take_discarded_fold`]).
    fn discardable_fold_block(expr: &Expression) -> Option<Span> {
        let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };
        if !matches!(selector.name().as_str(), "do:" | "doWithKey:") {
            return None;
        }
        match arguments.first() {
            Some(Expression::Block(b)) => Some(b.span),
            _ => None,
        }
    }

    /// BT-4651: the one place that decides whether `LoopMode::discarded_fold_block`
    /// is armed for a construct tuple. Only a `ProducerTail::Discard` producer
    /// can arm it (nothing reads its locals afterwards); a `Rebind` producer
    /// never does, since later code reads the repacked locals. Actor frames and
    /// self-family folds (`no_self_families` false) never arm it either.
    pub(super) fn discarded_fold_arm(
        tail: ProducerTail,
        actor: bool,
        no_self_families: bool,
        expr: &Expression,
    ) -> Option<Span> {
        (tail == ProducerTail::Discard && !actor && no_self_families)
            .then(|| Self::discardable_fold_block(expr))
            .flatten()
    }

    /// BT-4629: consumes [`LoopMode::discarded_fold_block`]: whether the fold
    /// whose body block is at `body_span` is the discarded construct.
    pub(super) fn take_discarded_fold(&mut self, body_span: Span) -> bool {
        self.loop_mode.discarded_fold_block.take() == Some(body_span)
    }

    /// Runs `f` with the enclosing direct-params loop's list-op mode
    /// (`LoopMode::in_direct_params_loop`) off, restoring it afterwards.
    pub(super) fn with_cleared_direct_params_loop<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let saved = std::mem::replace(&mut self.loop_mode.in_direct_params_loop, false);
        let out = f(self);
        self.loop_mode.in_direct_params_loop = saved;
        out
    }

    /// The frame a local-threading producer in `frame` lowers its
    /// `LocalRebind`s by (ADR 0131 §2): the loop or fold body registered for
    /// it ([`Self::with_rebind_frame`]), or the method root.
    pub(super) fn rebind_frame_of(&self, frame: FrameId) -> Result<RebindFrame> {
        if frame == FrameId::ROOT {
            return Ok(RebindFrame::implicit_root());
        }
        self.rebind_frames.get(&frame).cloned().ok_or_else(|| {
            CodeGenError::Internal(format!(
                "ADR 0131: a local-threading producer in frame {frame:?} has no registered \
                 enclosing frame"
            ))
        })
    }

    /// Whether `frame` lowers a conditional producer's `LocalRebind`s: the
    /// method root, or a registered `BranchArm` frame (ADR 0131 Phase 3).
    pub(super) fn is_conditional_producer_frame(&self, frame: FrameId) -> bool {
        frame == FrameId::ROOT
            || self
                .rebind_frames
                .get(&frame)
                .is_some_and(|rebind| rebind.kind == RebindFrameKind::BranchArm)
    }

    /// Runs `f` (a loop or fold body's lowering in frame `rebind.frame`)
    /// with `rebind` registered as that frame's [`RebindFrame`], so a
    /// local-threading producer nested in the body lowers its `LocalRebind`s
    /// by it ([`Self::rebind_frame_of`]). `rebind` must be what
    /// [`RebindFrame::of`] reads off the node built around the body
    /// (`ThreadingPlan::rebind_frame`); the verifier's
    /// `LocalRebindModeMismatch` checks that they agree.
    pub(super) fn with_rebind_frame<T>(
        &mut self,
        rebind: RebindFrame,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let frame = rebind.frame;
        // BT-4194: a direct-params or tuple-acc loop frame has no whole
        // `State` (its locals are parameters), and an arm frame nested in it
        // runs on a scratch map seeded fresh from those locals, so a bracket
        // there would `store` that scratch map over the cell. Every other
        // frame kind starts a scratch of its own.
        let private_scratch = match &rebind.kind {
            RebindFrameKind::Loop(
                threaded_ir::ThreadingMode::DirectParams | threaded_ir::ThreadingMode::TupleAcc(..),
            ) => true,
            RebindFrameKind::BranchArm => self.in_private_scratch,
            _ => false,
        };
        let saved_private = std::mem::replace(&mut self.in_private_scratch, private_scratch);
        let saved = self.rebind_frames.insert(frame, rebind);
        let out = f(self);
        self.in_private_scratch = saved_private;
        match saved {
            Some(previous) => self.rebind_frames.insert(frame, previous),
            None => self.rebind_frames.remove(&frame),
        };
        out
    }

    /// The producer sites of the loop, fold or arm body statement `stmt`
    /// (`beamtalk_core`'s `loop_body_producer_sites` narrowed by
    /// `is_loop_body_producer_site`): the one rule behind
    /// [`Self::with_body_producer_sites`] and the BT-3939 check that every
    /// such site a Tier 2 block body threads a local through was lowered as a
    /// producer.
    pub(super) fn body_producer_site_exprs<'a>(
        &self,
        stmt: &'a Expression,
        fold: bool,
        arm: bool,
    ) -> Vec<&'a Expression> {
        self.body_producer_site_exprs_with(stmt, fold, arm, FamilyOnlySites::ALL)
    }

    /// The sites [`Self::with_body_producer_sites`] registers for `stmt`:
    /// [`Self::body_producer_site_exprs`], except that the test-only fault
    /// `FoldOperandFamilyOnlyInPlace` withholds the BT-4287 sites
    /// ([`Self::is_family_only_fold_value_site`]) while the check still reads
    /// them.
    fn registered_body_producer_site_exprs<'a>(
        &self,
        stmt: &'a Expression,
        fold: bool,
        arm: bool,
    ) -> Vec<&'a Expression> {
        // (The BT-4310 sites hold the BT-4287 operand sites' constructs too,
        // so the BT-4287 fault withholds both, as before BT-4287.)
        if fault!(FoldOperandFamilyOnlyInPlace) {
            return self.body_producer_site_exprs_with(
                stmt,
                fold,
                arm,
                FamilyOnlySites {
                    value_sites: false,
                    unsequenced_operands: false,
                },
            );
        }
        if fault!(UnsequencedOperandInPlace) {
            return self.body_producer_site_exprs_with(
                stmt,
                fold,
                arm,
                FamilyOnlySites {
                    value_sites: true,
                    unsequenced_operands: false,
                },
            );
        }
        self.body_producer_site_exprs(stmt, fold, arm)
    }

    /// [`Self::body_producer_site_exprs`], with the family-only sites `family`
    /// names.
    fn body_producer_site_exprs_with<'a>(
        &self,
        stmt: &'a Expression,
        fold: bool,
        arm: bool,
        family: FamilyOnlySites,
    ) -> Vec<&'a Expression> {
        use beamtalk_core::semantic_analysis::block_facts::{
            LoopBodyContext, is_loop_body_producer_site, loop_body_producer_sites,
        };
        let mut ctx = LoopBodyContext::new(
            stmt,
            fold,
            self.in_fold_frame,
            self.in_actor_instance_context(),
        );
        if arm {
            ctx = ctx.in_arm();
        }
        let mut sites: Vec<&Expression> = loop_body_producer_sites(stmt)
            .into_iter()
            .filter(|site| {
                is_loop_body_producer_site(site, &ctx)
                    || (family.value_sites && self.is_family_only_fold_value_site(site))
            })
            .collect();
        if family.unsequenced_operands {
            Self::extend_new_sites(&mut sites, self.family_only_operand_site_exprs(stmt));
        }
        #[cfg(any(test, feature = "test"))]
        if arm && fault::in_plain_arm() && fault!(ArmLoopOrFoldSitesOnly) {
            sites.retain(|site| {
                beamtalk_core::semantic_analysis::block_facts::producer_body_block(site).is_some()
            });
        }
        sites
    }

    /// Appends each of `more` that `sites` does not already hold (by span).
    pub(super) fn extend_new_sites<'a>(sites: &mut Vec<&'a Expression>, more: Vec<&'a Expression>) {
        for site in more {
            if !sites.iter().any(|s| s.span() == site.span()) {
                sites.push(site);
            }
        }
    }

    /// BT-4310: the actor loops and folds threading only `State`
    /// ([`Self::is_family_only_fold_value_site`]) among the operands of the
    /// statement `stmt`, at any depth (`beamtalk_core`'s
    /// `operand_loop_constructs`): every one of them is a producer site of the
    /// frame that lowers `stmt`, the method root included, whichever lowering
    /// owns the operand that holds it.
    ///
    /// The sequencing rule's site lists (`sequenced_operand_sites`,
    /// `loop_body_producer_sites`) stop at the operands another lowering
    /// compiles itself: every kind `beamtalk_core`'s `UnsequencedOperand`
    /// names (a conditional's receiver, a lookup's key, a cascade message's
    /// argument, the arguments of a `super`, `error:`, `fieldAt:put:`,
    /// `clearField:`, block-invocation, cast or block-taking send, ...),
    /// which `unsequenced_operand_holder` decides. They stop there for a
    /// producer that rebinds *outer locals*, which the analysis still
    /// rejects at those operands. A construct that rebinds none but threads
    /// `State` compiles to a `{Value, State}` carrier all the same, and every
    /// one of those lowerings compiles the operand through
    /// [`Self::threaded_expression`] (the conditional receiver splice,
    /// `sequence_children` over a send's or a cascade's operands, the
    /// evaluated-block producer's arguments), which lowers a registered
    /// construct as a producer: its `ConstructTuple` and `State` `Bind` land
    /// in the frame's body ahead of the operand's consumer, and its value is
    /// element 1. Compiled in place instead (before BT-4310) the carrier
    /// became the operand (`Tuple does not understand '>'`, a lookup that
    /// missed its key, a raw `{Value, State}` answer) or the advanced `State`
    /// was dropped (`ConstructCarrierInPlace`, which every frame and the
    /// method root check over these sites).
    pub(super) fn family_only_operand_site_exprs<'a>(
        &self,
        stmt: &'a Expression,
    ) -> Vec<&'a Expression> {
        if !self.in_actor_instance_context() || self.is_repl_mode() {
            return Vec::new();
        }
        beamtalk_core::semantic_analysis::block_facts::operand_loop_constructs(stmt)
            .into_iter()
            .filter(|site| self.is_family_only_fold_value_site(site))
            .collect()
    }

    /// The BT-4310 sites of the statements `body` of one frame
    /// ([`Self::family_only_operand_site_exprs`]), as `(span, no locals)` for
    /// [`Self::check_family_only_operand_sites`]. Read before the statements
    /// are lowered, by the rule their registration reads.
    pub(super) fn family_only_operand_sites_of(
        &self,
        body: &[&Expression],
    ) -> Vec<(Span, Vec<String>)> {
        body.iter()
            .flat_map(|stmt| self.family_only_operand_site_exprs(stmt))
            .map(|site| (site.span(), Vec::new()))
            .collect()
    }

    /// BT-4310: reports each of `sites` ([`Self::family_only_operand_sites_of`])
    /// with no `ConstructTuple` at the top level of the lowered frame body
    /// `stmts` (`ConstructCarrierInPlace`): a loop or fold threading only
    /// `State` at an operand another lowering compiles, left in place as a
    /// carrier instead of lowered as a producer of the frame. The one check the
    /// frames that do not read [`Self::frame_producer_sites`] run (the actor
    /// method root, a `whileTrue:` condition, an `on:do:`/`ensure:` arm); the
    /// others check these sites among their own.
    pub(super) fn check_family_only_operand_sites(
        &mut self,
        stmts: &[ThreadedStmt],
        sites: &[(Span, Vec<String>)],
        frame_kind: &str,
        span: Span,
    ) {
        let errors = threaded_ir::verify_producer_sites_threaded(stmts, sites);
        self.report_threaded_ir_verify_errors(
            &errors,
            &format!(
                "{frame_kind}'s loop or fold threading only `State` at an operand, compiled in \
                 place instead of as a producer of the frame"
            ),
            span,
        );
    }

    /// [`Self::family_only_operand_site_exprs`] as the method root registers
    /// them: none under the test-only fault `UnsequencedOperandInPlace`, while
    /// the method root's check still reads them.
    pub(super) fn registered_family_only_operand_site_exprs<'a>(
        &self,
        stmt: &'a Expression,
    ) -> Vec<&'a Expression> {
        if fault!(UnsequencedOperandInPlace) {
            return Vec::new();
        }
        self.family_only_operand_site_exprs(stmt)
    }

    /// BT-4287: whether `site`, a value-bearing site of a frame-body statement
    /// in an actor method that the core rule leaves out
    /// (`is_actor_fold_value_site`: an operand under a fold frame), is a loop
    /// or fold that writes no outer local but threads the `State` family (its
    /// block sends to `self` or writes a field). The core rule's exclusion is
    /// about the *locals* a producer rebinds, which an actor fold's tuple
    /// accumulator does not fit (BT-3797); such a construct rebinds none. It
    /// still compiles to a `{Value, State}` carrier, so it is a producer of
    /// the frame that lowers it, at every site: its `ConstructTuple` and
    /// `State` `Bind` land in that frame's body, and its value is element 1.
    /// Compiled in place instead, the carrier became the operand (`Tuple does
    /// not understand 'plusFromNumber:'`) and the `State` it advanced was
    /// dropped (`ConstructCarrierInPlace`). `site` comes from
    /// `loop_body_producer_sites`, which answers each site without parentheses.
    ///
    /// The invariant, for an actor instance method: such a construct at a
    /// producer site of its statement is lowered as a producer of the frame
    /// that holds the statement, its carrier bound and unpacked there; at any
    /// other operand (a conditional's receiver, a lookup key, a cascade
    /// message's argument, an argument of a `super`, `error:`,
    /// block-invocation, cast or block-taking send) it is one too, BT-4310
    /// ([`Self::family_only_operand_site_exprs`]). (A `match:` guard allows
    /// only comparisons and arithmetic, so it never holds one.)
    /// `arm_operand_carrier_corpus.rs` proves both over the frame kinds it
    /// generates: fold, letrec and `whileTrue:` bodies and conditions, plain
    /// and producer conditional arms, plain and producer `on:do:`/`ensure:`
    /// bodies and handlers, `match:` arms, lookup and `ifNone:` handlers,
    /// evaluated, applied and stored blocks, and the method root. A block that
    /// is a real closure (a stored block, a lookup handler that is not
    /// inlined) compiles no carrier at all (its self-sends go through the
    /// actor-state cell, ADR 0137). The fold, letrec, arm and closure frames
    /// check their sites with `verify_producer_sites_threaded`; the method
    /// root, a `whileTrue:` condition and an `on:do:`/`ensure:` arm check the
    /// BT-4310 ones (`check_family_only_operand_sites`). The REPL
    /// has its own rule (its producers adopt the bindings map), and class and
    /// value-type methods thread no `State`.
    fn is_family_only_fold_value_site(&self, site: &Expression) -> bool {
        self.in_actor_instance_context()
            && !self.is_repl_mode()
            && beamtalk_core::semantic_analysis::block_facts::producer_body_block(site).is_some()
            && self
                .producer_locals_at_site(site)
                .is_some_and(|locals| locals.is_empty())
    }

    /// BT-3939 / BT-4014: the producer sites of the frame-body statement
    /// `stmt` that thread a local, as `(site span, locals)`, for
    /// [`threaded_ir::verify_producer_sites_threaded`] once the body is built:
    /// [`Self::body_producer_site_exprs`] narrowed to the sites
    /// [`Self::producer_site_locals`] names a producer. Read just before
    /// `stmt` is lowered, so a local an earlier statement bound is live. Empty
    /// in the REPL, whose producers adopt the bindings map. Shared by the
    /// Tier 2 block body and the Letrec and fold bodies, so every frame body
    /// checks its own statements by one rule.
    pub(super) fn frame_producer_sites(
        &self,
        stmt: &Expression,
        fold: bool,
        arm: bool,
    ) -> Vec<(Span, Vec<String>)> {
        if self.is_repl_mode() {
            return Vec::new();
        }
        self.producer_sites_of(self.body_producer_site_exprs(stmt, fold, arm))
    }

    /// BT-4461: [`Self::frame_producer_sites`] for a statement of a REPL frame
    /// (a REPL arm, [`Self::with_repl_arm_producer_sites`]): each of its
    /// `repl_arm_producer_sites` that writes a binding the frame holds in a
    /// Core variable, as `(site span, those bindings)`
    /// ([`Self::repl_cached_bindings_written_by`]). The producer there must
    /// thread each of them ([`threaded_ir::verify_repl_producer_sites_threaded`]).
    /// Empty outside the REPL.
    pub(super) fn repl_frame_producer_sites(&self, stmt: &Expression) -> Vec<(Span, Vec<String>)> {
        if !self.is_repl_mode() {
            return Vec::new();
        }
        beamtalk_core::semantic_analysis::block_facts::repl_arm_producer_sites(stmt)
            .into_iter()
            .filter(|site| self.local_producer_locals(site).is_some())
            .filter_map(|site| {
                let cached = self.repl_frame_bindings_written_by(site);
                // A paren-wrapped site's span holds its construct's.
                (!cached.is_empty()).then(|| (site.span(), cached))
            })
            .collect()
    }

    /// Reports [`threaded_ir::verify_repl_producer_sites_threaded`] over a REPL
    /// frame's lowered statements `stmts` and the `sites` its statements
    /// registered ([`Self::repl_frame_producer_sites`]).
    pub(super) fn check_repl_frame_producer_sites(
        &mut self,
        stmts: &[ThreadedStmt],
        sites: &[(Span, Vec<String>)],
        span: Span,
    ) {
        let errors = threaded_ir::verify_repl_producer_sites_threaded(stmts, sites);
        self.report_threaded_ir_verify_errors(
            &errors,
            "REPL frame's producer leaving a binding the frame holds in a variable stale",
            span,
        );
    }

    /// The `(site span, locals)` of each of `sites` that
    /// [`Self::producer_site_locals`] names a producer.
    fn producer_sites_of(&self, sites: Vec<&Expression>) -> Vec<(Span, Vec<String>)> {
        sites
            .into_iter()
            .filter_map(|site| {
                self.producer_site_locals(site)
                    .map(|locals| (site.unwrap_parens().span(), locals))
            })
            .collect()
    }

    /// BT-4014: the locals the construct at the producer site `site` threads
    /// when it is a local-threading producer there, as
    /// `ProducerSiteNotThreaded` reads it at every frame: a loop, fold,
    /// conditional or exception construct ([`Self::producer_locals_at_site`]),
    /// or a value-position local assignment (BT-3910, whose carrier is a
    /// `ConstructTuple` at its own span), which threads its outermost target.
    /// `None` for anything else.
    pub(super) fn producer_site_locals(&self, site: &Expression) -> Option<Vec<String>> {
        if let Some(locals) = self.producer_locals_at_site(site.unwrap_parens()) {
            return Some(locals);
        }
        if !self.is_local_assignment_producer(site) {
            return None;
        }
        beamtalk_core::semantic_analysis::block_facts::assignments_outside_blocks(site)
            .into_iter()
            .max_by_key(|(_, span)| span.len())
            .map(|(local, _)| vec![local.to_string()])
    }

    /// Runs `f` (a loop or fold body's lowering) with the body's own
    /// statements as local-producer sites (ADR 0131 Phase 2b): each
    /// statement that is itself a loop or fold, and each sequenced operand
    /// site of a statement (`sequenced_operand_sites`: an assignment's whole
    /// value, a `^` value, a send's receiver and arguments, a literal's
    /// elements) — narrowed by `beamtalk_core`'s `is_loop_body_producer_site`,
    /// the rule the Phase 0 check reads too. `fold` says whether the body is a
    /// fold's (`is_fold_frame_body`) rather than a letrec loop's. Restores the
    /// enclosing set afterwards, so a site is a producer only in the body that
    /// lowers it through [`Self::threaded_expression`] under a registered
    /// frame.
    pub(super) fn with_body_producer_sites<T>(
        &mut self,
        body: &[&Expression],
        fold: bool,
        arm: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut sites: std::collections::HashSet<Span> = self.local_producer_sites.clone();
        for stmt in body {
            sites.extend(
                self.registered_body_producer_site_exprs(stmt, fold, arm)
                    .into_iter()
                    .map(Expression::span),
            );
        }
        self.with_producer_site_set(sites, fold, f)
    }

    /// Runs `f` with [`Self::local_producer_sites`] set to `sites` (and the
    /// fold-frame flag widened by `fold`), restoring both afterwards.
    fn with_producer_site_set<T>(
        &mut self,
        sites: std::collections::HashSet<Span>,
        fold: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer_fold = self.in_fold_frame;
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let saved_fold = std::mem::replace(&mut self.in_fold_frame, outer_fold || fold);
        let out = f(self);
        self.local_producer_sites = saved;
        self.in_fold_frame = saved_fold;
        out
    }

    /// BT-3910 (ADR 0131 §2): runs `f` (the lowering of a REPL arm, inlined over
    /// the bindings map) with the arm registered as a rebind frame, so a
    /// value-position assignment in it lowers by that frame. The arm's
    /// `StateAcc` *is* the bindings map: the `StateAcc` loop row of §2's table,
    /// every binding a member (`RebindFrame::with_repl_binding`). The frame is
    /// the current branch frame. (A legacy workspace block body inlines its
    /// `[...] value` over the same arm lowering outside the REPL; there the
    /// frame carries nothing, so a write stays arm-local.)
    pub(super) fn with_repl_arm_rebind_frame<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let rebind = RebindFrame {
            frame: self.current_branch_frame(),
            kind: RebindFrameKind::Loop(threaded_ir::ThreadingMode::StateAcc(
                threaded_ir::StateAccFallbackReason::None,
            )),
            threads: Vec::new(),
        };
        self.with_rebind_frame(rebind, f)
    }

    /// BT-3910: the threaded set a legacy (non-`BranchArm`) arm's `Threaded`
    /// wrapper records: that of the `StateAcc` frame registered for `frame` (a
    /// Tier 2 stateful block body, [`Self::with_rebind_frame`]); empty for any
    /// other arm. The verifier re-derives a rebind's shape from the node.
    pub(super) fn registered_state_acc_threads(&self, frame: FrameId) -> Vec<String> {
        match self.rebind_frames.get(&frame) {
            Some(rebind)
                if matches!(
                    rebind.kind,
                    RebindFrameKind::Loop(threaded_ir::ThreadingMode::StateAcc(_))
                ) =>
            {
                rebind.threads.clone()
            }
            _ => Vec::new(),
        }
    }

    /// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): runs `f` (the lowering
    /// of a REPL arm: a conditional arm, an `on:do:`/`ensure:` block or a
    /// `tryDo:` block, inlined over the bindings map) with the arm statements'
    /// `repl_arm_producer_sites` added to the local-producer sites, so a loop,
    /// fold, conditional or exception construct there that writes a binding is
    /// a producer adopting the arm's `StateAcc` (the bindings map itself,
    /// [`Self::lower_repl_construct_tuple`]) instead of leaking its
    /// `{Value, Bindings}` tuple into an operand. A no-op outside the REPL.
    pub(super) fn with_repl_arm_producer_sites<T>(
        &mut self,
        body: &[&Expression],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        if !self.is_repl_mode() {
            return f(self);
        }
        let mut sites: std::collections::HashSet<Span> = self.local_producer_sites.clone();
        for stmt in body {
            sites.extend(
                beamtalk_core::semantic_analysis::block_facts::repl_arm_producer_sites(stmt)
                    .into_iter()
                    .map(Expression::span),
            );
        }
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let out = f(self);
        self.local_producer_sites = saved;
        out
    }

    /// Whether `expr` is a local-threading producer
    /// ([`Self::local_producer_locals`]) or has one at a sequenced operand
    /// site — the question a loop-body statement lowering asks to leave the
    /// statement to [`Self::threaded_expression`] (which splices the
    /// producer's prelude) instead of unpacking a construct tuple itself.
    pub(super) fn expr_has_local_producer(&self, expr: &Expression) -> bool {
        // BT-3910: a value-position local assignment inside it is a producer.
        !self.contained_local_assignment_targets(expr).is_empty()
            || self.local_producer_locals(expr).is_some()
            || beamtalk_core::semantic_analysis::block_facts::loop_body_operand_sites(expr)
                .into_iter()
                .chain(self.family_only_operand_site_exprs(expr))
                .any(|site| self.local_producer_locals(site).is_some())
    }

    /// BT-3910 (ADR 0131 §1): whether `expr` (paren-unwrapped) is a local
    /// assignment in value position, a producer
    /// ([`Self::lower_local_assignment_producer`]). One rule, decided once per
    /// body by `beamtalk_core`'s `value_position_assignment_spans`: a
    /// statement-position assignment is never in the set.
    pub(super) fn is_local_assignment_producer(&self, expr: &Expression) -> bool {
        if fault!(NotAProducer) {
            return false;
        }
        let inner = expr.unwrap_parens();
        matches!(
            inner,
            Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_))
        ) && self.value_assign_sites.contains(&inner.span())
    }

    /// BT-3910 (ADR 0131 §1, §2 `CarrierSlot::Whole`): lowers the
    /// value-position local assignment `(t := V)` as `V`'s own prelude, then
    /// the assigned value bound as the carrier (`ConstructTuple`), then one
    /// `LocalRebind { local: t, carrier, slot: Whole }` lowered by the
    /// enclosing `frame` (§2's table). Answers the carrier, the assigned value.
    pub(super) fn lower_local_assignment_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        let inner = expr.unwrap_parens();
        let span = inner.span();
        let Expression::Assignment { target, value, .. } = inner else {
            return Err(CodeGenError::Internal(
                "lower_local_assignment_producer on a non-assignment".to_string(),
            ));
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Err(CodeGenError::Internal(
                "lower_local_assignment_producer on a field assignment".to_string(),
            ));
        };
        let local = id.name.to_string();
        let inner_value = self.threaded_expression(value, frame)?;
        let mut prelude = inner_value.prelude;
        let value_doc = self.threaded_value_doc(&inner_value.value);
        // In a REPL input the frame's map is the bindings map: the assigned
        // binding is a member of it (`RebindFrame::with_repl_binding`), so it is
        // `Put` there.
        let repl_root = self.is_repl_mode() && frame == FrameId::ROOT;
        let enclosing = if repl_root {
            RebindFrame::implicit_root()
        } else {
            self.rebind_frame_of(frame)?
        };
        let enclosing = if self.is_repl_mode() {
            enclosing.with_repl_binding(&local)
        } else {
            enclosing
        };
        let carrier = self.fresh_temp_var("Assigned");
        let slot = ThreadedLocalSlot {
            local: local.clone(),
            slot: CarrierSlot::Whole,
            value_var: self.fresh_temp_var(&local),
        };
        let rebind = threaded_ir::build_local_threading_prelude(
            &enclosing,
            &carrier,
            value_doc,
            std::slice::from_ref(&slot),
            Vec::new(),
            span,
            |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
        );
        self.note_repl_map_rebinds(&rebind);
        prelude.extend(rebind);
        // (At the REPL root a binding is read back from the bindings map, which
        // the `Put` above stepped, so it is not bound to a variable.)
        if !repl_root {
            self.bind_var(&local, &slot.value_var);
        }
        #[cfg(any(test, feature = "test"))]
        let prelude = Self::inject_local_rebind_fault(prelude);
        Ok(ThreadedValue {
            prelude,
            value: ValueRef::Var(carrier),
        })
    }

    /// The `RebindLowering` ADR 0131 §2's table assigns the rebind of `slot`
    /// in `shape`'s cell, with its lowering-time names minted from the live
    /// generator: a `LoopParam` steps the local's loop identity (the chain
    /// `final_loop_arg_identities` follows into the recursive call) to
    /// `Gensym(value_var)`; a `MapPut` steps the frame's `State` map by one
    /// version.
    pub(super) fn mint_rebind_lowering(
        &mut self,
        slot: &ThreadedLocalSlot,
        shape: RebindShape,
        frame: FrameId,
    ) -> RebindLowering {
        match shape {
            RebindShape::Let => RebindLowering::Let,
            RebindShape::LoopParam => {
                let canonical = Self::to_core_erlang_var(&slot.local);
                let current = self
                    .lookup_var(&slot.local)
                    .cloned()
                    .unwrap_or_else(|| canonical.clone());
                let source = if current == canonical {
                    VersionedVar::new(VersionPrefix::Local(slot.local.clone()), 0, frame)
                } else {
                    VersionedVar::new(VersionPrefix::Gensym(current), 1, frame)
                };
                RebindLowering::LoopParam {
                    source,
                    target: VersionedVar::new(
                        VersionPrefix::Gensym(slot.value_var.clone()),
                        1,
                        frame,
                    ),
                }
            }
            RebindShape::MapPut => {
                let source_version = self.state_version();
                let _ = self.next_state_var();
                let target_version = self.state_version();
                let key = match &slot.slot {
                    CarrierSlot::Key(key) => key.clone(),
                    // The REPL's bindings map is keyed by the bare name.
                    CarrierSlot::Whole if self.is_repl_mode() => slot.local.clone(),
                    CarrierSlot::Pos(_) | CarrierSlot::Whole => Self::local_state_key(&slot.local),
                };
                RebindLowering::MapPut {
                    key,
                    source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                    target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                }
            }
        }
    }

    /// The construct-tuple half of [`Self::lower_local_threading_construct`],
    /// once its operands are sequenced.
    pub(super) fn lower_construct_tuple(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        let span = expr.span();
        let actor = self.in_actor_instance_context();
        let enclosing = self.rebind_frame_of(frame)?;
        let self_families = if actor {
            super::control_flow::analysis::ThreadedFamilies::default()
        } else {
            self.vt_loop_threaded_families(expr)
        };
        let source_version = self.state_version();
        // A nested construct returns its own `{Value, StateAcc}` tuple: the
        // enclosing direct-params loop's open-chain list-op forms (which
        // rebind by name and return no tuple) must not apply to it.
        // BT-4629: a `Discard` tail reads no local back, so a `do:` fold that
        // is this construct need not repack them into its carrier's map.
        // Self-family folds also read the carrier's other slots: they keep it.
        let discard_block = Self::discarded_fold_arm(
            tail,
            actor,
            self_families == super::control_flow::analysis::ThreadedFamilies::default(),
            expr,
        );
        self.loop_mode.discarded_fold_block = discard_block;
        let doc = self.with_fold_self_slot_consumer(expr, |this| {
            this.with_cleared_direct_params_loop(|this| this.expression_doc(expr))
        });
        self.loop_mode.discarded_fold_block = None;
        let doc = doc?;
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: usize, carrier: &str| form::element(k, leaf::var(carrier.to_string()));
        let mut family_binds = Vec::new();
        // In an actor instance frame the carrier's element 2 is the `State`
        // family's next version (ADR 0131 §2). A direct-params or tuple-acc
        // frame has no `State` chain to extend: its body has no state effect
        // (`select_direct_params`/`select_tuple_acc`), so the nested
        // construct's `State` is the one already in scope.
        if actor && enclosing.kind.extends_state_chain() {
            let _ = self.next_state_var();
            family_binds.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
                span,
            });
        }
        let mut prelude = self.construct_rebind_prelude(
            &enclosing,
            &carrier,
            doc,
            locals,
            tail,
            family_binds,
            frame,
            span,
        );
        if let Some(rebind) = self.extract_vt_loop_family_slot(&carrier, &self_families, span) {
            prelude.push(ThreadedStmt::Statement(docvec![rebind, " "], span));
        }
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// The prelude every local-threading producer ends with (ADR 0131 §1): the
    /// construct tuple bound to `carrier`, the construct's family extraction
    /// `family_binds` and, per `tail`, one `LocalRebind` per entry of `locals`
    /// lowered by `enclosing` (§2's table) or a `DiscardLocals`. Shared by the
    /// loop/fold producer ([`Self::lower_construct_tuple`]) and the conditional
    /// producer, so the two cannot disagree on the rebind shape.
    #[allow(clippy::too_many_arguments)] // one argument per part of the §1 prelude
    pub(super) fn construct_rebind_prelude(
        &mut self,
        enclosing: &RebindFrame,
        carrier: &str,
        doc: Document<'static>,
        locals: &[String],
        tail: ProducerTail,
        family_binds: Vec<ThreadedStmt>,
        frame: FrameId,
        span: Span,
    ) -> Vec<ThreadedStmt> {
        match tail {
            ProducerTail::Rebind => {
                let slots: Vec<ThreadedLocalSlot> = locals
                    .iter()
                    .map(|local| ThreadedLocalSlot {
                        local: local.clone(),
                        slot: CarrierSlot::Key(self.local_scratch_key(local)),
                        value_var: self.fresh_temp_var(local),
                    })
                    .collect();
                let prelude = threaded_ir::build_local_threading_prelude(
                    enclosing,
                    carrier,
                    doc,
                    &slots,
                    family_binds,
                    span,
                    |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
                );
                self.note_repl_map_rebinds(&prelude);
                for slot in &slots {
                    self.bind_var(&slot.local, &slot.value_var);
                }
                #[cfg(any(test, feature = "test"))]
                let prelude = Self::inject_local_rebind_fault(prelude);
                prelude
            }
            ProducerTail::Discard => {
                let mut prelude = vec![threaded_ir::build_construct_tuple(
                    carrier.to_string(),
                    doc,
                    locals.to_vec(),
                    span,
                )];
                prelude.extend(family_binds);
                prelude.push(threaded_ir::build_discard_locals(carrier.to_string(), span));
                prelude
            }
        }
    }

    /// Applies the active local-rebind [`Fault`] (test-only) to a
    /// producer's `Rebind` prelude.
    #[cfg(any(test, feature = "test"))]
    fn inject_local_rebind_fault(mut prelude: Vec<ThreadedStmt>) -> Vec<ThreadedStmt> {
        let last_rebind = prelude
            .iter()
            .rposition(|stmt| matches!(stmt, ThreadedStmt::LocalRebind { .. }));
        if fault!(DropRebind) {
            if let Some(i) = last_rebind {
                prelude.remove(i);
            }
        } else if fault!(WrongLowering) {
            if let Some(i) = last_rebind
                && let ThreadedStmt::LocalRebind {
                    local, lowering, ..
                } = &mut prelude[i]
            {
                *lowering = RebindLowering::MapPut {
                    key: Self::local_state_key(local),
                    source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
                    target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
                };
            }
        } else if fault!(StaleHeld) {
            // The last `MapPut` rebind (not necessarily the last rebind).
            let last_put = prelude.iter().rposition(|stmt| {
                matches!(
                    stmt,
                    ThreadedStmt::LocalRebind {
                        lowering: RebindLowering::MapPut { .. },
                        ..
                    }
                )
            });
            if let Some(i) = last_put
                && let ThreadedStmt::LocalRebind {
                    state_first,
                    lowering,
                    ..
                } = &mut prelude[i]
                && let RebindLowering::MapPut { key, target, .. } = lowering.clone()
            {
                *state_first = Some(target.clone());
                *lowering = RebindLowering::MapHeld { key, map: target };
            }
        } else if fault!(WrongFrame)
            && let Some(i) = last_rebind
            && let ThreadedStmt::LocalRebind { frame, .. } = &mut prelude[i]
        {
            *frame = FrameId::new(u32::MAX);
        }
        prelude
    }

    /// BT-3910: [`Self::verified_root_prelude_doc`] for a REPL input
    /// statement's prelude. A value-position assignment `Put`s into the
    /// bindings map, the REPL root's threaded set (§2's `MethodBody (REPL)`
    /// row), so a prelude holding `LocalRebind`s is verified and rendered as
    /// that explicit frame node.
    pub(super) fn verified_repl_root_prelude_doc(
        &mut self,
        prelude: Vec<ThreadedStmt>,
        span: Span,
    ) -> Document<'static> {
        let threads: Vec<String> = prelude
            .iter()
            .filter_map(|stmt| match stmt {
                ThreadedStmt::LocalRebind { local, .. } => Some(local.clone()),
                _ => None,
            })
            .collect();
        if threads.is_empty() {
            return self.verified_root_prelude_doc(&prelude, span);
        }
        let backfilled = threaded_ir::backfill_opaque_version_gaps(
            &prelude,
            FrameId::ROOT,
            threaded_ir::opaque_gap_families(self.threaded_scope()),
        );
        let checked = [threaded_ir::build_method_body(
            FrameId::ROOT,
            threads.clone(),
            backfilled,
        )];
        let errors = self.verify_threaded_ir(&checked);
        self.report_threaded_ir_verify_errors(
            &errors,
            "REPL-root local-threading prelude must be well-formed",
            span,
        );
        let root = [threaded_ir::build_method_body(
            FrameId::ROOT,
            threads,
            prelude,
        )];
        self.threaded_prelude_doc(&root)
    }

    /// Renders a producer prelude a value-type method body splices as
    /// `Document`s (it has no `Vec<ThreadedStmt>` of its own), after
    /// verifying it at the method root like every other method-body IR.
    pub(super) fn verified_root_prelude_doc(
        &mut self,
        prelude: &[ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let errors =
            threaded_ir::verify_body_with_opaque_version_gaps(prelude, self.threaded_scope());
        self.report_threaded_ir_verify_errors(
            &errors,
            "method-root local-threading prelude must be well-formed",
            span,
        );
        self.threaded_prelude_doc(prelude)
    }
}
