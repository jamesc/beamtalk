// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type conditional lowering: `ifTrue:`/`ifFalse:` (and friends) whose
//! branches read or write `self` fields and outer locals, plus the branch
//! piece builders and merge helpers they share.
//!
//! **DDD Context:** Code Generation

use super::{VtBranchPieces, VtCondBaseline, VtConditionalLowered};
use crate::core_erlang::threading_analysis::ThreadedLocals;

use super::super::control_flow::SoloBoolean;
use super::super::control_flow::analysis::ThreadedFamilies;
use super::super::control_flow::family_slots::{self, FamilyVersionStep};
use super::super::threaded_ir::{
    CarrierSlot, FrameId, RebindLowering, RenderCtx, ThreadedLocalSlot, ThreadedStmt,
    ThreadedValue, ValueRef, VersionPrefix, VersionedVar, build_family_construct_tuple,
    build_family_local_threading_prelude,
};
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::{Block, Expression, MessageSelector};

impl CoreErlangGenerator {
    /// Returns the list of local variables written in `block` that are already
    /// bound in the enclosing scope. This is scope-aware — it checks `lookup_var` to
    /// distinguish "reassigning outer x" from "defining new local y".
    pub(super) fn outer_scope_mutations_in_block(
        &self,
        block: &beamtalk_core::ast::Block,
    ) -> Vec<String> {
        use crate::core_erlang::block_analysis::analyze_block;
        let analysis = analyze_block(block);
        let mut result: Vec<String> = analysis
            .local_writes
            .iter()
            .filter(|var| self.lookup_var(var).is_some())
            .cloned()
            .collect();
        result.sort();
        result
    }

    /// Emits a read+write conditional (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
    /// whose branch blocks read+write an outer local) as an inline `case` binding its logical
    /// value to a fresh result var, pushed onto `body_parts` as an open let chain
    /// (`let CondVal = case ... in\n`). Returns the result var name.
    ///
    /// Returns `None` when `expr` is not one of the supported conditional shapes, so the caller
    /// can fall back to the generic last-expression path. The threaded locals don't escape in
    /// last position, so each branch yields only its logical value (its last expression).
    ///
    /// Reached via the shared [`Self::lower_threaded_last`] transform for both the value-type
    /// and the class-method boundary, which both return it bare.
    pub(in crate::core_erlang) fn emit_vt_conditional_case_to_var(
        &mut self,
        expr: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<String>> {
        let (receiver, selector_name, arguments) = match expr {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                (receiver.as_ref(), sel, arguments)
            }
            _ => return Ok(None),
        };

        // A solo `ifTrue:`/`ifFalse:`'s non-taken arm answers what the one
        // `SoloBoolean` table says (the receiver, BT-4189); it is rendered once
        // the receiver's `Cond` var is minted, below.
        let arms = match (
            SoloBoolean::from_keyword(&selector_name),
            arguments.as_slice(),
        ) {
            (
                Some(sel @ (SoloBoolean::IfTrue | SoloBoolean::IfFalse)),
                [Expression::Block(block)],
            ) => VtCondArms::Solo(sel, self.build_vt_conditional_branch_value(block)?),
            (None, [Expression::Block(tb), Expression::Block(fb)])
                if selector_name == "ifTrue:ifFalse:" =>
            {
                VtCondArms::Both(
                    self.build_vt_conditional_branch_value(tb)?,
                    self.build_vt_conditional_branch_value(fb)?,
                )
            }
            _ => return Ok(None),
        };

        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let cond_doc = self.guard_condition_doc(receiver, cond_doc);
        let result_var = self.fresh_temp_var("CondVal");
        let (true_branch, false_branch) = match arms {
            VtCondArms::Both(t, f) => (t, f),
            VtCondArms::Solo(sel, taken) => {
                let answer = sel.non_taken().doc(&cond_var);
                if sel.block_on_true() {
                    (taken, answer)
                } else {
                    (answer, taken)
                }
            }
        };
        // BT-4244: a non-Boolean receiver sends the message (the shared
        // fallback the pure and actor paths use).
        let blocks: Vec<&Block> = arguments
            .iter()
            .filter_map(|arg| match arg {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        let fallback = self.boolean_fallback_send(&cond_var, &selector_name, &blocks, false)?;
        let no_match_var = self.fresh_temp_var("CondNoMatch");
        body_parts.push(docvec![
            "    let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_doc,
            " in\n    let ",
            leaf::var(result_var.clone()),
            " = case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_branch,
            " <'false'> when 'true' -> ",
            false_branch,
            " <",
            leaf::var(no_match_var),
            "> when 'true' -> ",
            fallback,
            " end in\n",
        ]);
        Ok(Some(result_var))
    }

    /// Inlines a conditional branch's block body, returning the block's logical
    /// value (its last expression) rather than the mutated-local tuple that
    /// [`Self::generate_vt_conditional_branch`] produces.
    ///
    /// Intermediate expressions are emitted as sequential `let` bindings (assignments rebind
    /// their target in scope so later expressions see the update); the final expression is
    /// the branch's value. An empty block yields `'nil'`.
    fn build_vt_conditional_branch_value(
        &mut self,
        block: &beamtalk_core::ast::Block,
    ) -> Result<Document<'static>> {
        self.push_scope();
        let result = self.build_vt_conditional_branch_value_parts(block);
        self.pop_scope();
        result
    }

    /// Inner implementation for [`Self::build_vt_conditional_branch_value`], bracketed by
    /// `push_scope`/`pop_scope` so the fallible work always restores scope.
    fn build_vt_conditional_branch_value_parts(
        &mut self,
        block: &beamtalk_core::ast::Block,
    ) -> Result<Document<'static>> {
        let body = super::super::util::collect_body_exprs(&block.body);
        if body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }
        let last = body.len() - 1;
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (i, body_expr) in body.iter().enumerate() {
            if i == last {
                // Value position: the assignment's value (for `x := v`) or the expression.
                let value_expr = match body_expr {
                    Expression::Assignment { value, .. } => value.as_ref(),
                    other => *other,
                };
                // a threading construct (loop / foldl list-op mutating captured
                // locals) here yields a `{value, StateAcc}` tuple. The branch value is its
                // logical result — element 1 — not the raw tuple. The threaded locals don't
                // need to escape in value position, so unwrap without extracting them.
                if self.expr_yields_vt_threaded_tuple(value_expr) {
                    let tuple_var = self.fresh_temp_var("BranchThreadedResult");
                    let val_doc = self.expression_doc(value_expr)?;
                    parts.push(docvec![
                        "let ",
                        leaf::var(tuple_var.clone()),
                        " = ",
                        val_doc,
                        " in call 'erlang':'element'(1, ",
                        leaf::var(tuple_var),
                        ")",
                    ]);
                } else {
                    parts.push(self.expression_doc(value_expr)?);
                }
            } else if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        // a threading-construct RHS returns a `{value, StateAcc}`
                        // tuple. Bind the target to element 1 and rebind the threaded locals
                        // from the StateAcc, so both the assigned value and the mutated locals
                        // are visible to later statements in this branch — rather than binding
                        // the target to the raw tuple.
                        if self.expr_yields_vt_threaded_tuple(value) {
                            self.emit_vt_threaded_local_assignment(&id.name, value, &mut parts)?;
                        } else {
                            let core_var = self
                                .lookup_var(&id.name)
                                .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                            let val_doc = self.expression_doc(value)?;
                            parts.push(docvec![
                                "let ",
                                leaf::var(core_var.clone()),
                                " = ",
                                val_doc,
                                " in ",
                            ]);
                            self.bind_var(&id.name, &core_var);
                        }
                    }
                }
            } else if self.expr_yields_vt_threaded_tuple(body_expr) {
                // a non-last loop / foldl list-op that mutates captured locals returns
                // a `{value, StateAcc}` tuple. Extract and rebind the threaded locals so later
                // statements in this branch see the updates (the value itself is discarded).
                let families = self.vt_loop_threaded_families(body_expr);
                let loop_doc = self.with_fold_self_slot_consumer(body_expr, |this| {
                    this.expression_doc(body_expr)
                })?;
                let threaded_locals = self.vt_construct_threaded_locals(body_expr);
                let doc = self.emit_vt_loop_open_extraction(
                    loop_doc,
                    &threaded_locals,
                    &families,
                    body_expr.span(),
                    "BranchThreadedResult",
                    "BranchThreadedState",
                );
                parts.push(doc);
            } else {
                let tmp = self.fresh_temp_var("seq");
                let doc = self.expression_doc(body_expr)?;
                parts.push(docvec!["let ", leaf::var(tmp), " = ", doc, " in ",]);
            }
        }
        Ok(Document::Vec(parts))
    }

    /// Generates a non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` with captured
    /// local mutations as an **open let chain** in value-type or class-method context.
    ///
    /// Generates an inline `case` expression where:
    /// - Each branch inlines the block body as sequential let-bindings
    /// - The case returns the updated values of captured variables (tuple for N>1)
    /// - After the case, variables are rebound via `element/N` extraction
    ///
    /// This makes the updated locals visible to all subsequent method body expressions.
    ///
    /// BT-3821: renders [`Self::lower_vt_conditional`]'s prelude, the same lowering
    /// [`Self::vt_conditional_value_producer`] splices for the conditional in value
    /// position, with the arms' values discarded.
    pub(in crate::core_erlang) fn generate_vt_conditional_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // Always the method root. This statement form is only reachable there: the producer's
        // gate (`is_vt_field_conditional_producer`) declines a conditional in a loop / branch /
        // exception frame, and every such shape is rejected before it gets here
        // (`field_writing_arm_conditionals_in_non_root_frames_are_rejected_not_internal_errors`
        // pins the outcome per shape). Lowering it in another frame would need that frame's
        // `RebindFrame` registered (`rebind_frame_of`), which a class-method loop body does not.
        let Some(lowered) = self.lower_vt_conditional(expr, FrameId::ROOT, false)? else {
            return Ok(Document::Nil);
        };
        // A method-root prelude like the producer's: verified (`ConstructFamilyDropped`
        // included) before it is flattened to text.
        Ok(self.verified_root_prelude_doc(&lowered.prelude, expr.span()))
    }

    /// BT-3821: whether `expr` is a value-type `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:` whose every branch argument is a literal block
    /// (the shape [`Self::lower_vt_conditional`] inlines), whatever its arms do.
    fn vt_conditional_has_literal_arms(expr: &Expression) -> bool {
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        matches!(
            (sel.as_str(), arguments.as_slice()),
            ("ifTrue:" | "ifFalse:", [Expression::Block(_)])
                | (
                    "ifTrue:ifFalse:",
                    [Expression::Block(_), Expression::Block(_)]
                )
        )
    }

    /// BT-3821: whether `expr` (paren-unwrapped) is a value-type conditional
    /// whose arms write a field (`Self`) and that lowers as a *producer* in
    /// value position ([`Self::vt_conditional_value_producer`]): in a value-type
    /// instance method, outside every loop body (whose own frame threads `Self`
    /// through the loop's accumulator), with literal-block arms.
    ///
    /// The one gate [`CoreErlangGenerator::threaded_expression`]'s producer and
    /// `subexpr_needs_prelude`'s mirror of it share.
    pub(in crate::core_erlang) fn is_vt_field_conditional_producer(
        &self,
        expr: &Expression,
    ) -> bool {
        let expr = expr.unwrap_parens();
        self.context == CodeGenContext::ValueType
            && !self.in_class_method()
            // The method-root frame only: the producer, its probe mirrors and
            // `statement_has_value_type_producer` all read this one condition.
            // `current_frame()` is derived from `in_loop_body` (ROOT exactly when it is
            // unset, the branch frame otherwise), so the loop-body and frame conditions
            // cannot drift apart: there is one source, not two to keep in step.
            && self.current_frame() == FrameId::ROOT
            && Self::vt_conditional_has_literal_arms(expr)
            && self.is_conditional_with_vt_self_field_threading(expr)
    }

    /// BT-3821: the producer lowering of a value-type conditional whose arms
    /// write a field, in value position (`self take: (c ifTrue: [self.m := 1]
    /// ifFalse: [0])`, `x := c ifTrue: [self.m := 1] ifFalse: [0]`, a `^`
    /// value, an operand of any sequenced parent).
    ///
    /// **Invariant** (every frame the gate admits): the conditional's `Self`
    /// merge `Bind` — `Self{N+1} <- element(k, <carrier>)` — and its value
    /// `element(1, <carrier>)` are in the *caller's* statement sequence, never
    /// inside the closed operand document, so the statements after the
    /// conditional read the merged `Self`. The construct declares the `Self`
    /// family its arms advanced ([`ThreadedStmt::ConstructTuple`]'s
    /// `families`); the verifier reports `ConstructFamilyDropped` if the merge
    /// `Bind` does not follow.
    ///
    /// The receiver evaluates before either arm and may itself hold a field
    /// write, so it is sequenced first ([`Self::sequence_children`]); the
    /// arms' baseline `Self` is the version live after it.
    pub(in crate::core_erlang) fn vt_conditional_value_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if !self.is_vt_field_conditional_producer(expr) {
            return Ok(None);
        }
        // The gate admits the method-root frame only; a caller threading it into any
        // other frame disagrees with the probe mirrors, which would mis-sequence.
        if frame != FrameId::ROOT {
            return Err(CodeGenError::Internal(format!(
                "a value-type conditional producer was asked to lower in frame {frame:?}, \
                 but its gate admits only the method root"
            )));
        }
        let Some(receiver) = Self::conditional_send_receiver(expr) else {
            return Ok(None);
        };
        let (mut prelude, scope) = self.sequence_children(&[receiver], frame)?;
        let lowered = self.lower_vt_conditional(expr, frame, true)?;
        self.finish_precompiled_scope(scope)?;
        let Some(VtConditionalLowered {
            prelude: case_prelude,
            value: Some(value),
        }) = lowered
        else {
            return Err(CodeGenError::Internal(
                "is_vt_field_conditional_producer accepted a conditional \
                 lower_vt_conditional did not lower to a value"
                    .to_string(),
            ));
        };
        prelude.extend(case_prelude);
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// BT-3821: lowers a value-type `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
    /// whose arms mutate an outer local and/or write a field (`Self`) to its
    /// [`ThreadedStmt`] prelude, in `frame`:
    ///
    /// ```text
    /// Statement   let Cond = <receiver> in
    /// ConstructTuple CondResult = case Cond of <true> -> <arm> <false> -> <arm> end
    ///                             (families: [SelfVt] when an arm wrote a field)
    /// Bind        Self{N+1} <- element(k, CondResult)        (the Self merge)
    /// Statement   let <local> = element(i, CondResult) in …  (one per outer local)
    /// ```
    ///
    /// Each arm returns `{[Value,] local…, Self}`: the arm's own value first when
    /// `capture_value` (the value position), then the outer locals it may have
    /// rebound, then the `Self` family slot both arms agree on. The returned
    /// `value` is `element(1, CondResult)`; `None` for the statement form, which
    /// discards the arms' values.
    ///
    /// `Ok(None)`, with nothing minted, when `expr` is not such a conditional.
    /// The receiver is compiled as-is: a caller whose receiver holds a field
    /// write has sequenced it ahead (`hoist_vt_operand_producers`,
    /// [`Self::vt_conditional_value_producer`]).
    fn lower_vt_conditional(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        capture_value: bool,
    ) -> Result<Option<VtConditionalLowered>> {
        // BT-4366: the conditional's nested call nodes carry its line.
        self.with_current_span(expr.span(), |this| {
            this.lower_vt_conditional_inner(expr, frame, capture_value)
        })
    }

    #[allow(clippy::too_many_lines)]
    fn lower_vt_conditional_inner(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        capture_value: bool,
    ) -> Result<Option<VtConditionalLowered>> {
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return Ok(None);
        };
        let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if !Self::vt_conditional_has_literal_arms(expr) {
            return Ok(None);
        }
        let span = expr.span();
        // Outer locals any arm rebinds, and whether an arm writes a field: a branch
        // whose only mutation is `self.field := ...` still needs this inline-`case`
        // (the generic Tier 1 closure path rejects a field write outright).
        let all_mutations = self.vt_conditional_outer_mutations(arguments);
        // (BT-3912: the recursive detector `is_conditional_with_vt_self_field_threading`
        // reads, so an arm whose only write is a field-writing fold's body counts.)
        let branch_blocks: Vec<&Block> = arguments
            .iter()
            .filter_map(|arg| match arg {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        let threads_self = !self
            .exception_construct_families(&branch_blocks)
            .as_slice()
            .is_empty();
        // BT-3522: `threads_self` covers a write that is a BARE TOP-LEVEL STATEMENT of a
        // branch block. A write nested deeper mints a `Self{N}` that dies with its own
        // nested scope, so no branch tuple can name it: it was a SILENT DROP (the early
        // return below erased the conditional). The same per-statement rejection
        // `exception_handling.rs` applies to its arms turns it into one diagnostic.
        self.reject_nested_vt_conditional_self_field_writes(arguments)?;
        if all_mutations.is_empty() && !threads_self {
            return Ok(None);
        }

        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let cond_doc = self.guard_condition_doc(receiver, cond_doc);
        let mut prelude: Vec<ThreadedStmt> = vec![ThreadedStmt::Statement(
            docvec!["let ", leaf::var(cond_var.clone()), " = ", cond_doc, " in "],
            span,
        )];

        // capture the `Self` version before generating either arm so both
        // are true siblings (each starts from the same baseline, mirroring
        // `with_branch_context`'s restore-only-no-reset discipline) and so a
        // value-type `self.field := ...` write inside an arm can be detected
        // by comparing the version before/after that arm's own generation.
        let baseline = VtCondBaseline {
            self_vt: self.self_version(),
        };

        let Some((true_pieces, false_pieces)) = self.build_vt_conditional_arm_pieces(
            &selector_name,
            &cond_var,
            arguments,
            &all_mutations,
            baseline,
            capture_value,
        )?
        else {
            return Ok(None);
        };

        // `Self` is threaded through the
        // case's return tuple iff either arm mutated it — both arms must
        // agree on the return shape since the `element/N` extraction after the
        // case is fixed at compile time and runs regardless of which arm
        // actually executed. ADR 0122 / BT-3513: this set of "either arm
        // mutated it" families IS the construct's own `ThreadedFamilies` —
        // the direct successor of the old `VtCondSlots` bool pair.
        let mutated_families: Vec<VersionPrefix> = (true_pieces.self_mutated_version.is_some()
            || false_pieces.self_mutated_version.is_some())
        .then_some(VersionPrefix::SelfVt)
        .into_iter()
        .collect();
        let families = ThreadedFamilies::from_matches(&mutated_families);
        // BT-4244: a non-Boolean receiver sends the message (the shared
        // fallback), answering the locals and `Self` unchanged.
        let fallback_pieces = self.build_vt_conditional_fallback_pieces(
            &selector_name,
            &cond_var,
            &branch_blocks,
            &all_mutations,
            capture_value,
        )?;
        let true_branch = self.finish_vt_conditional_branch(true_pieces, &families, baseline);
        let false_branch = self.finish_vt_conditional_branch(false_pieces, &families, baseline);
        let fallback_branch =
            self.finish_vt_conditional_branch(fallback_pieces, &families, baseline);
        let no_match_var = self.fresh_temp_var("CondNoMatch");

        // Generate the case expression and rebind variables after it.
        let result_var = self.fresh_temp_var("CondResult");
        let case_doc = docvec![
            "case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_branch,
            " <'false'> when 'true' -> ",
            false_branch,
            " <",
            leaf::var(no_match_var),
            "> when 'true' -> ",
            fallback_branch,
            " end"
        ];

        // The value occupies element 1 of the tuple when requested, shifting
        // every local and the family slot up by one.
        let value_slots = usize::from(capture_value);
        let family_binds = self.vt_conditional_family_binds(
            &result_var,
            all_mutations.len() + value_slots,
            &families,
            baseline,
            frame,
            span,
        );
        if families.as_slice().is_empty() && !capture_value {
            // Plain local threading (no field write, no value): the case returns the
            // bare local, or a flat tuple of them, and the rebinds stay statements.
            prelude.push(build_family_construct_tuple(
                result_var.clone(),
                case_doc,
                Vec::new(),
                Vec::new(),
                span,
            ));
            prelude.extend(self.vt_conditional_local_rebinds(
                &all_mutations,
                &result_var,
                true,
                value_slots,
                span,
            ));
        } else {
            prelude.extend(self.vt_conditional_threaded_merge(
                &all_mutations,
                value_slots,
                &result_var,
                case_doc,
                mutated_families,
                family_binds,
                frame,
                span,
            )?);
        }
        let value = capture_value.then(|| {
            ValueRef::Doc(docvec![
                "call 'erlang':'element'(1, ",
                leaf::var(result_var),
                ")"
            ])
        });
        Ok(Some(VtConditionalLowered { prelude, value }))
    }

    /// The outer-scope locals any literal-block arm of a conditional rebinds, in
    /// first-seen order.
    fn vt_conditional_outer_mutations(&self, arguments: &[Expression]) -> Vec<String> {
        let mut all_mutations: Vec<String> = Vec::new();
        let mut blocks: Vec<&Block> = Vec::new();
        for arg in arguments {
            if let Expression::Block(block) = arg {
                blocks.push(block);
                for var in self.outer_scope_mutations_in_block(block) {
                    if !all_mutations.contains(&var) {
                        all_mutations.push(var);
                    }
                }
            }
        }
        // BT-3912 (BT-3854): and the outer locals a loop or fold nested in an arm
        // threads (`outer_scope_mutations_in_block` reads the arm's own
        // statements only): the arm is a frame whose producers rebind them.
        for var in self.branch_threaded_locals(&blocks) {
            if !all_mutations.contains(&var) {
                all_mutations.push(var);
            }
        }
        all_mutations
    }

    /// BT-3821: the merge of a value-position (or field-writing) conditional's case
    /// result at the method root: the construct declares the outer locals it threads
    /// (`all_mutations`) and the `Self` family its arms advanced, so the verifier sees
    /// one `LocalRebind` per local (`ThreadedLocalDropped`,
    /// `LocalReadAfterSiblingRebind`) and the family merge `Bind`
    /// (`ConstructFamilyDropped`). Local `i` sits at tuple position
    /// `i + value_slots + 1`.
    #[allow(clippy::too_many_arguments)]
    fn vt_conditional_threaded_merge(
        &mut self,
        all_mutations: &[String],
        value_slots: usize,
        result_var: &str,
        case_doc: Document<'static>,
        mutated_families: Vec<VersionPrefix>,
        family_binds: Vec<ThreadedStmt>,
        frame: FrameId,
        span: beamtalk_core::source_analysis::Span,
    ) -> Result<Vec<ThreadedStmt>> {
        let slots: Vec<ThreadedLocalSlot> = all_mutations
            .iter()
            .enumerate()
            .map(|(i, var)| {
                let value_var = Self::to_core_erlang_var(var);
                self.bind_var(var, &value_var);
                ThreadedLocalSlot {
                    local: var.clone(),
                    slot: CarrierSlot::Pos(i + value_slots + 1),
                    value_var,
                }
            })
            .collect();
        let enclosing = self.rebind_frame_of(frame)?;
        let built = build_family_local_threading_prelude(
            &enclosing,
            result_var,
            case_doc,
            &slots,
            mutated_families,
            family_binds,
            span,
            // The method root threads no local of its own: every rebind is a plain `let`.
            |_, _| RebindLowering::Let,
        );
        // BT-3821 fault injection: the last local's rebind is omitted
        // (`ThreadedLocalDropped` on a real compile). Only test builds mutate `built`.
        #[cfg(test)]
        let mut built = built;
        #[cfg(test)]
        if super::super::threaded_expr::fault::active()
            == Some(super::super::threaded_expr::fault::LocalRebindFault::DropRebind)
        {
            if let Some(at) = built
                .iter()
                .rposition(|stmt| matches!(stmt, ThreadedStmt::LocalRebind { .. }))
            {
                built.remove(at);
            }
        }
        Ok(built)
    }

    /// BT-3522: rejects a value-type `self.field := ...` write that a
    /// conditional's branch block hides below its own top level, which
    /// [`Self::generate_vt_conditional_open`]'s branch-merge tuple cannot
    /// carry — see that function's own call-site comment for the shape and
    /// for why it used to be a silent drop.
    ///
    /// Delegates per top-level statement to the shared
    /// [`CoreErlangGenerator::reject_unthreadable_value_self_field_write`],
    /// passing `threads_value_self = is_field_assignment(stmt)` exactly as
    /// `exception_handling.rs`'s own per-statement pass does: that helper's
    /// pointer-identity root skip then lets a bare top-level write (the
    /// carried base case) through while rejecting anything deeper, including
    /// one buried in that statement's own right-hand side. A no-op outside
    /// value-type instance-method context (the helper's own guard).
    fn reject_nested_vt_conditional_self_field_writes(
        &self,
        arguments: &[Expression],
    ) -> Result<()> {
        for arg in arguments {
            let Expression::Block(block) = arg else {
                continue;
            };
            for stmt in super::super::util::collect_body_exprs(&block.body) {
                self.reject_unthreadable_value_self_field_write_in_arm(stmt)?;
            }
        }
        Ok(())
    }

    /// Builds the `(true_arm, false_arm)` [`VtBranchPieces`] pair for one of
    /// the three supported conditional selectors — the taken arm(s) from
    /// [`Self::build_vt_conditional_branch_pieces`], the absent arm (for the
    /// one-block forms) from
    /// [`Self::build_vt_conditional_passthrough_pieces`].
    ///
    /// `Ok(None)` for an unsupported selector, or for a supported one whose
    /// argument in a block position is not actually a block — both cases the
    /// caller answers by returning what it has built so far, exactly as the
    /// inline `match` this was extracted from did.
    fn build_vt_conditional_arm_pieces(
        &mut self,
        selector_name: &str,
        cond_var: &str,
        arguments: &[Expression],
        all_mutations: &[String],
        baseline: VtCondBaseline,
        capture_value: bool,
    ) -> Result<Option<(VtBranchPieces, VtBranchPieces)>> {
        let pair = match (SoloBoolean::from_keyword(selector_name), arguments) {
            (
                Some(sel @ (SoloBoolean::IfTrue | SoloBoolean::IfFalse)),
                [Expression::Block(block), ..],
            ) => {
                // The arm that runs the block and the non-taken passthrough,
                // placed by the one `SoloBoolean` table (BT-4189). Each is
                // built in receiver-arm order (true first), as before, so the
                // temp numbering is unchanged.
                if sel.block_on_true() {
                    let taken = self.build_vt_conditional_branch_pieces(
                        block,
                        all_mutations,
                        baseline,
                        capture_value,
                    )?;
                    let passthrough = Self::build_vt_conditional_passthrough_pieces(
                        self,
                        sel,
                        cond_var,
                        all_mutations,
                        capture_value,
                    );
                    (taken, passthrough)
                } else {
                    let passthrough = Self::build_vt_conditional_passthrough_pieces(
                        self,
                        sel,
                        cond_var,
                        all_mutations,
                        capture_value,
                    );
                    let taken = self.build_vt_conditional_branch_pieces(
                        block,
                        all_mutations,
                        baseline,
                        capture_value,
                    )?;
                    (passthrough, taken)
                }
            }
            (
                None,
                [
                    Expression::Block(true_block),
                    Expression::Block(false_block),
                    ..,
                ],
            ) if selector_name == "ifTrue:ifFalse:" => {
                let tb = self.build_vt_conditional_branch_pieces(
                    true_block,
                    all_mutations,
                    baseline,
                    capture_value,
                )?;
                let fb = self.build_vt_conditional_branch_pieces(
                    false_block,
                    all_mutations,
                    baseline,
                    capture_value,
                )?;
                (tb, fb)
            }
            _ => return Ok(None),
        };
        Ok(Some(pair))
    }

    /// whether `expr` is an `on:do:`/`ensure:` whose try/handler/
    /// cleanup blocks mutate an outer local, OR the `SelfVt`
    /// family (ADR 0122 / BT-3506), in value-type or class-method context.
    /// Unlike [`Self::is_conditional_with_vt_local_threading`] and its
    /// loop/foldl siblings, `on:do:`/`ensure:`'s own codegen
    /// (`exception_handling.rs`'s `generate_on_do_with_mutations`/
    /// `generate_ensure_with_mutations`) is already context-agnostic since
    /// `exception_body_outer_state` — it needs no separate
    /// vt-specific construction, only its returned
    /// `{Result, StateAcc[, Family]}` tuple unpacked here at the
    /// non-last-position call site (see
    /// [`Self::generate_vt_exception_construct_open`]).
    ///
    /// Folds what used to be a two-predicate OR at every call site
    /// (`self.is_exception_construct_with_vt_local_threading(expr) ||
    /// self.is_exception_construct_with_vt_self_field_threading(expr)`) into
    /// this one function.
    pub(in crate::core_erlang) fn is_exception_construct_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if !beamtalk_core::state_threading_selectors::is_exception_selector(&sel) {
            return false;
        }
        self.threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
            .is_some()
            || !self
                .exception_construct_threaded_families(expr)
                .as_slice()
                .is_empty()
    }

    /// the protected block and the handler/cleanup block of an
    /// `on:do:`/`ensure:` send — precisely the two blocks
    /// `exception_handling.rs`'s `generate_on_do_with_mutations` /
    /// `generate_ensure_with_mutations` inline with state threading.
    ///
    /// `None` for any other selector, and for a supported one whose receiver
    /// or block argument is not actually a literal block (those never reach
    /// the mutation-threading emitters at all — `generate_on_do` /
    /// `generate_ensure` fall back to the closure-based path).
    ///
    /// Both halves of "which send is this, and which argument holds its
    /// handler/cleanup block" come from `beamtalk_core`'s shared
    /// `state_threading_selectors` leaf module — `is_exception_selector` for
    /// the first and `state_threaded_block_arg_indices` for the second
    /// (`ensure:` → arg 0, `on:do:` → arg 1) — rather than being restated
    /// here, so this predicate can never drift from the table the rest of
    /// state-threading already reads.
    fn exception_construct_blocks(expr: &Expression) -> Option<[&beamtalk_core::ast::Block; 2]> {
        use beamtalk_core::state_threading_selectors::{
            is_exception_selector, state_threaded_block_arg_indices,
        };

        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return None;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if !is_exception_selector(&sel) {
            return None;
        }
        // Both exception selectors have exactly one state-threaded block
        // argument; anything else would be a new shape this predicate has
        // not been taught, so bail rather than guess.
        let &[handler_index] = state_threaded_block_arg_indices(&sel) else {
            return None;
        };
        let other = arguments.get(handler_index)?;
        let (Expression::Block(protected), Expression::Block(other)) = (receiver.as_ref(), other)
        else {
            return None;
        };
        Some([protected, other])
    }

    /// ADR 0122 / BT-3506: the storage families (`SelfVt`) that
    /// `expr` — an `on:do:`/`ensure:` `MessageSend` — threads out through its
    /// trailing tuple slot, so its result tuple carries the slot that
    /// [`Self::generate_vt_exception_construct_open`] (and its
    /// last-position/assign-RHS siblings) extract. Empty (never rejected)
    /// for any other `Expression`.
    ///
    /// Deliberately shares
    /// [`CoreErlangGenerator::exception_construct_families`] with the
    /// emitter rather than restating the rule, so the tuple shape written and
    /// the shape read back cannot drift — the successor to BT-3486's
    /// `is_exception_construct_with_vt_self_field_threading` (`SelfVt`-only,
    /// a bare `bool`).
    pub(in crate::core_erlang) fn exception_construct_threaded_families(
        &self,
        expr: &Expression,
    ) -> super::super::control_flow::analysis::ThreadedFamilies {
        Self::exception_construct_blocks(expr)
            .map(|blocks| self.exception_construct_families(&blocks))
            .unwrap_or_default()
    }

    /// emits a non-last `on:do:`/`ensure:` that mutates captured
    /// outer locals, in value-type or class-method context, as an open let
    /// chain — binds the construct's own `{Result, StateAcc}` tuple, then
    /// rebinds each mutated outer local from `StateAcc`'s `__local__` keys.
    /// Mirrors `gen_server/methods.rs`'s Actor-context
    /// `BodyExprKind::ControlFlowWithMutations` non-last handling (the same
    /// extraction, minus that path's `ThreadedStmt::Bind`/`next_state_var`
    /// step — there is no ambient `gen_server` `State` to thread into here).
    ///
    /// BT-3486/BT-3506: when the construct also threads the
    /// `SelfVt` family ([`Self::exception_construct_threaded_families`]),
    /// ALSO extracts the trailing element 3 and rebinds it via
    /// [`Self::extract_vt_loop_family_slot`] (BT-3512's shared extraction
    /// helper) — the same trailing slot, in the same position, and the same
    /// rebind that BT-3484's [`Self::emit_vt_loop_open_extraction`] gives a
    /// Letrec loop. Without it the construct's own mutated version stays
    /// scoped inside the Core Erlang `try` and every later read in the
    /// method silently sees the pre-`try` snapshot.
    pub(in crate::core_erlang) fn generate_vt_exception_construct_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let families = self.exception_construct_threaded_families(expr);
        let span = expr.span();
        let tuple_var = self.fresh_temp_var("ExTuple");
        let expr_doc = self.expression_doc(expr)?;
        let mut docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            expr_doc,
            " in ",
        ]];
        if let Some(threaded_vars) = self
            .threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
        {
            let state_var = self.fresh_temp_var("ExState");
            docs.push(docvec![
                "let ",
                leaf::var(state_var.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var.clone()),
                ") in ",
            ]);
            for var in &threaded_vars {
                let core_var = self
                    .lookup_var(var)
                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                docs.push(docvec![
                    "let ",
                    leaf::var(core_var),
                    " = call 'maps':'get'(",
                    leaf::atom(Self::local_state_key(var)),
                    ", ",
                    leaf::var(state_var.clone()),
                    ") in ",
                ]);
            }
        }
        if let Some(rebind_doc) = self.extract_vt_loop_family_slot(&tuple_var, &families, span) {
            docs.push(rebind_doc);
        }
        Ok(Document::Vec(docs))
    }

    /// Generates a branch body for a VT conditional that inlines the block, resolving
    /// (but not yet finalizing — see [`VtBranchPieces`]) the final values of all
    /// captured mutations and detecting any class-var mutation.
    fn build_vt_conditional_branch_pieces(
        &mut self,
        block: &beamtalk_core::ast::Block,
        all_mutations: &[String],
        baseline: VtCondBaseline,
        capture_value: bool,
    ) -> Result<VtBranchPieces> {
        let VtCondBaseline {
            self_vt: self_before,
        } = baseline;
        self.push_scope();
        // `self_version` gets a save/reset/restore discipline here — the two
        // arms are true SIBLINGS: each must start from the
        // same baseline `Self{N}` (so both arms' `maps:put` chains source
        // from the value live before the `case`), and neither may leak its
        // own in-branch `Self{N+1}` binding — scoped inside that arm's own
        // case clause — out to the code that follows the conditional. The
        // post-`case` family merge (`vt_conditional_family_binds`) is what
        // legitimately advances the outer version, exactly once.
        self.set_self_version(self_before);
        // BT-3912 (BT-3854): a loop or fold at a producer site of an arm
        // statement is a producer: its prelude (the construct tuple, the `Self`
        // merge, the local rebinds) is spliced into the arm's own statements.
        let body = super::super::util::collect_body_exprs(&block.body);
        let result = self.with_body_producer_sites(&body, false, false, |this| {
            this.build_vt_conditional_branch_pieces_inner(
                block,
                all_mutations,
                baseline,
                capture_value,
            )
        });
        self.set_self_version(self_before);
        self.pop_scope();
        result
    }

    /// Inner implementation for `build_vt_conditional_branch_pieces`.
    ///
    /// Separated so that `push_scope`/`pop_scope` and the `Self`-version
    /// save/restore always bracket the fallible work regardless of whether `?`
    /// propagates an error.
    fn build_vt_conditional_branch_pieces_inner(
        &mut self,
        block: &beamtalk_core::ast::Block,
        all_mutations: &[String],
        baseline: VtCondBaseline,
        capture_value: bool,
    ) -> Result<VtBranchPieces> {
        let VtCondBaseline {
            self_vt: self_before,
        } = baseline;
        let body = super::super::util::collect_body_exprs(&block.body);

        let mut preamble: Vec<Document<'static>> = Vec::new();
        // BT-3821: in value position the arm's value is its last statement's
        // (`nil` for an empty block); the statement form discards it.
        let mut arm_value: Option<Document<'static>> = capture_value.then_some(leaf::atom("nil"));
        let last_index = body.len().checked_sub(1);
        for (i, body_expr) in body.iter().enumerate() {
            let is_value_stmt = capture_value && Some(i) == last_index;
            if self.is_vt_self_field_assignment(body_expr) {
                // a value-type `self.field:=...` write. Emitted as
                // an OPEN let-chain (`let _Val = … in let Self{N} =
                // maps:put(…) in `) spliced straight into this arm's
                // preamble, NOT as the `let _seqN = <closed expr> in ` the
                // generic arm below would produce: the closed shape scopes
                // the new `Self{N}` binding inside its own `let`'s RHS, so
                // neither a later statement in this same branch nor the
                // arm's own return tuple can see it — the compile-time
                // `unbound variable 'Self1'` failure this construction avoids.
                let (doc, val_var) = self.generate_field_assignment_open(body_expr)?;
                preamble.push(doc);
                if is_value_stmt {
                    arm_value = Some(leaf::var(val_var));
                }
            } else if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let core_var = self
                            .lookup_var(&id.name)
                            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                        // ADR 0118 phase 5b: a class-method
                        // self-send on the RHS (`x := self bump`), at any
                        // nesting depth, threads as a real prelude via
                        // `threaded_expression` — spliced ahead of this
                        // `let core_var = ... in` (mirrors
                        // `try_generate_block_local_plain_let`'s approach,
                        // built on `ThreadedValue` rather than an
                        // open-chain side channel).
                        let frame = self.current_frame();
                        let tv = self.threaded_expression(value, frame)?;
                        let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                        let value_doc = self.threaded_value_doc(&tv.value);
                        self.bind_var(&id.name, &core_var);
                        preamble.push(docvec![
                            prelude_doc,
                            "let ",
                            leaf::var(core_var.clone()),
                            " = ",
                            value_doc,
                            " in ",
                        ]);
                        if is_value_stmt {
                            arm_value = Some(leaf::var(core_var));
                        }
                    }
                }
            } else if is_value_stmt {
                // The arm's last statement is its value: no `let _seqN` sink.
                let frame = self.current_frame();
                let tv = self.threaded_expression(body_expr, frame)?;
                let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                preamble.push(prelude_doc);
                arm_value = Some(self.threaded_value_doc(&tv.value));
            } else {
                let tmp = self.fresh_temp_var("seq");
                // ADR 0118 phase 5b: mirror the assignment arm
                // above — a bare-statement class-method self-send also
                // threads as a real prelude.
                let frame = self.current_frame();
                let tv = self.threaded_expression(body_expr, frame)?;
                let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                preamble.push(docvec![
                    prelude_doc,
                    "let ",
                    leaf::var(tmp),
                    " = ",
                    value_doc,
                    " in ",
                ]);
            }
        }

        let local_values = Self::resolve_mutation_value_docs(self, all_mutations);
        let self_after = self.self_version();
        let self_mutated_version = (self_after != self_before).then_some(self_after);
        Ok(VtBranchPieces {
            preamble,
            local_values,
            self_mutated_version,
            value: arm_value,
        })
    }

    /// Generates a pass-through arm's pieces: the current (pre-branch) values of all
    /// captured mutations, leaving them unchanged, and no class-var mutation (a
    /// passthrough arm executes no branch code). In value position
    /// (`capture_value`) the absent arm's value is `sel`'s non-taken answer
    /// from the one [`SoloBoolean`] table: the receiver `cond_var` for a solo
    /// `ifTrue:`/`ifFalse:`, as dynamic dispatch answers (BT-4189).
    fn build_vt_conditional_passthrough_pieces(
        cg: &Self,
        sel: SoloBoolean,
        cond_var: &str,
        all_mutations: &[String],
        capture_value: bool,
    ) -> VtBranchPieces {
        VtBranchPieces {
            preamble: Vec::new(),
            local_values: Self::resolve_mutation_value_docs(cg, all_mutations),
            self_mutated_version: None,
            value: capture_value.then(|| sel.non_taken().doc(cond_var)),
        }
    }

    /// BT-4244: the wildcard arm's pieces for a receiver that is neither
    /// `true` nor `false`: the shared dynamic send
    /// ([`Self::boolean_fallback_send`], the one the pure and actor paths use),
    /// with every captured local and `Self` unchanged, as a passthrough arm
    /// answers them. The send's answer is the arm's value in value position;
    /// in statement position the send is bound and discarded.
    fn build_vt_conditional_fallback_pieces(
        &mut self,
        selector_name: &str,
        cond_var: &str,
        blocks: &[&Block],
        all_mutations: &[String],
        capture_value: bool,
    ) -> Result<VtBranchPieces> {
        let send = self.boolean_fallback_send(cond_var, selector_name, blocks, false)?;
        let (preamble, value) = if capture_value {
            (Vec::new(), Some(send))
        } else {
            (vec![docvec!["let _ = ", send, " in "]], None)
        };
        Ok(VtBranchPieces {
            preamble,
            local_values: Self::resolve_mutation_value_docs(self, all_mutations),
            self_mutated_version: None,
            value,
        })
    }

    /// Resolves each of `all_mutations` to its current Core Erlang variable doc.
    fn resolve_mutation_value_docs(cg: &Self, all_mutations: &[String]) -> Vec<Document<'static>> {
        all_mutations
            .iter()
            .map(|var| {
                let core_var = cg
                    .lookup_var(var)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var));
                leaf::var(core_var)
            })
            .collect()
    }

    /// Combines an arm's preamble with its finalized return value,
    /// appending a trailing `Self` slot per `families` (each
    /// carrying this arm's own resulting version if IT mutated that family,
    /// else the unchanged baseline `VtCondBaseline` carries) — i.e. when
    /// *either* sibling arm threads one, both arms must agree on the tuple shape so the
    /// `element/N` extraction after the case is valid regardless of which arm ran.
    ///
    /// Slot order is fixed: the arm's value (value position only, BT-3821),
    /// locals, then the family slot — [`ThreadedFamilies`]' own canonical
    /// order, matching [`Self::vt_conditional_local_rebinds`]'s and
    /// [`Self::vt_conditional_family_binds`]'s extraction order.
    ///
    /// When `families` is empty (and there is no value slot), this renders
    /// byte-identically to the original shape (bare value for one mutation,
    /// `{v1, v2,...}` tuple otherwise).
    ///
    /// ADR 0122 / BT-3513: the hand-rolled `any_self_mutated` arm this
    /// replaced now routes through
    /// [`family_slots::append_family_slots`] for the (common) case of at
    /// least one local — the join'd `pieces.local_values` become the
    /// already-open `base` the helper appends onto. The zero-locals edge
    /// case (`ifTrue: [self.x := v]` with no local write —
    /// `VtBodyExprKind::ConditionalWithSelfFieldThreading`) has no prior
    /// tuple element for `base` to open with, so it passes
    /// [`Document::Nil`] (see that function's own doc comment on why this is
    /// additive, not a new code path the helper's other callers share).
    fn finish_vt_conditional_branch(
        &mut self,
        pieces: VtBranchPieces,
        families: &ThreadedFamilies,
        baseline: VtCondBaseline,
    ) -> Document<'static> {
        let VtBranchPieces {
            preamble,
            local_values,
            self_mutated_version,
            value,
        } = pieces;
        // this arm's own value for `prefix`: its own mutated version
        // if IT advanced that family's counter, else the baseline (unchanged)
        // version inherited from before the conditional — the same
        // both-or-neither resolution `exception_handling.rs`'s
        // `exception_family_slot` already performs for `on:do:`/`ensure:`.
        let arm_version_for = |prefix: &VersionPrefix| -> usize {
            let mutated = match prefix {
                VersionPrefix::SelfVt => self_mutated_version,
                other => unreachable!(
                    "a value-type conditional's trailing family slot only ever carries \
                     SelfVt, got {other:?}"
                ),
            };
            mutated.unwrap_or_else(|| baseline.version_for(prefix))
        };
        // BT-3821: in value position the arm's value opens the tuple, ahead of
        // the locals, so the tuple always has at least one prior element.
        let elements: Vec<Document<'static>> = value.into_iter().chain(local_values).collect();
        let return_doc = if families.as_slice().is_empty() && elements.len() == 1 {
            elements.into_iter().next().unwrap_or(Document::Nil)
        } else {
            let base = if elements.is_empty() {
                // no prior tuple element for `append_family_slots` to open
                // with (see that function's own doc comment on this
                // `Document::Nil` convention).
                Document::Nil
            } else {
                docvec!["{", join(elements, &Document::Str(", "))]
            };
            let ctx = RenderCtx::new(self);
            family_slots::append_family_slots(
                base,
                families,
                |prefix| VersionedVar::new(prefix.clone(), arm_version_for(prefix), FrameId::ROOT),
                &ctx,
            )
        };
        Document::Vec(vec![Document::Vec(preamble), return_doc])
    }

    /// Rebinds each of `all_mutations` from the case result as a
    /// `let VAR = <result_var>` or `let VAR = element(N, <result_var>)`
    /// statement, updating the scope so subsequent expressions see the new
    /// variable names.
    ///
    /// `bare` says the case returns the single local directly rather than in a
    /// tuple (one mutation, no family slot, no value slot). `slot_offset` is the
    /// number of tuple elements ahead of the locals (`1` when the arms' value
    /// opens the tuple, BT-3821).
    fn vt_conditional_local_rebinds(
        &mut self,
        all_mutations: &[String],
        result_var: &str,
        bare: bool,
        slot_offset: usize,
        span: beamtalk_core::source_analysis::Span,
    ) -> Vec<ThreadedStmt> {
        if bare && all_mutations.len() == 1 {
            // Single variable, no family threading: case returns the value directly.
            let var = &all_mutations[0];
            let core_var = Self::to_core_erlang_var(var);
            self.bind_var(var, &core_var);
            return vec![ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(core_var),
                    " = ",
                    leaf::var(result_var.to_string()),
                    " in ",
                ],
                span,
            )];
        }
        // Multiple variables (and/or a threaded family or value): case returns a
        // tuple, extract with element/N.
        all_mutations
            .iter()
            .enumerate()
            .map(|(i, var)| {
                let core_var = Self::to_core_erlang_var(var);
                self.bind_var(var, &core_var);
                ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'erlang':'element'(",
                        leaf::int_lit(i64::try_from(i + slot_offset + 1).unwrap_or(i64::MAX)),
                        ", ",
                        leaf::var(result_var.to_string()),
                        ") in ",
                    ],
                    span,
                )
            })
            .collect()
    }

    /// The merge of each of the conditional's threaded `families` out of the
    /// case result: one real [`ThreadedStmt::Bind`] per family, in `frame`, so
    /// a value-type `self.field := ...` write made inside either arm is visible
    /// to code following the conditional.
    ///
    /// `base_arity` is the number of tuple elements ahead of the family slots
    /// (the locals, plus the value slot in value position).
    ///
    /// ADR 0122 / BT-3513: the trailing-slot pickup this replaced (`next_slot
    /// = all_mutations.len() + 1`, a hand-rolled `element/N` arm) now
    /// mints each family's fresh successor version exactly as before
    /// (`next_self_var`, a generator-state side effect
    /// [`family_slots::extract_family_slots`] itself never performs; see
    /// that function's own doc comment: "a caller mints `target` itself...
    /// BEFORE calling this"), then builds the extraction `Bind`s through
    /// [`family_slots::extract_family_slots`] — the SAME `let NewCv =
    /// element(N, Result) in ` shape, just built from typed leaves instead of
    /// hand-spliced `Document`s.
    fn vt_conditional_family_binds(
        &mut self,
        result_var: &str,
        base_arity: usize,
        families: &ThreadedFamilies,
        baseline: VtCondBaseline,
        frame: FrameId,
        span: beamtalk_core::source_analysis::Span,
    ) -> Vec<ThreadedStmt> {
        if families.as_slice().is_empty() {
            return Vec::new();
        }
        // mint each family's fresh successor version FIRST (the
        // side-effecting step `family_slots::extract_family_slots` itself
        // never performs), then hand the resulting (source, target) pairs to
        // the shared extraction helper.
        let steps: Vec<(VersionPrefix, FamilyVersionStep)> = families
            .as_slice()
            .iter()
            .map(|prefix| {
                let (source_version, target_version) = match prefix {
                    VersionPrefix::SelfVt => {
                        // the merged `Self` becomes the method's new LIVE
                        // `Self{N}` for every statement after the conditional
                        // — the direct counterpart of Actor's `let State1 =
                        // element(2, _CF10) in`. `set_self_version` first so
                        // the minted name is the single successor of the
                        // pre-`case` version, never of whichever arm happened
                        // to be generated last.
                        self.set_self_version(baseline.self_vt);
                        self.next_self_var();
                        (baseline.self_vt, self.self_version())
                    }
                    other => unreachable!(
                        "a value-type conditional's trailing family slot only ever carries \
                         SelfVt, got {other:?}"
                    ),
                };
                (
                    prefix.clone(),
                    FamilyVersionStep::new(
                        VersionedVar::new(prefix.clone(), source_version, frame),
                        VersionedVar::new(prefix.clone(), target_version, frame),
                    ),
                )
            })
            .collect();
        let extraction = family_slots::extract_family_slots(
            result_var,
            base_arity,
            families,
            |prefix| {
                steps
                    .iter()
                    .find(|(p, _)| p == prefix)
                    .expect("families and steps built from the same list, in the same order")
                    .1
                    .clone()
            },
            span,
        );
        // `extract_family_slots` itself never verifies its own output (its
        // doc comment flags this): check each family's minted version step
        // via the same generic per-mutation invariant every other
        // `Self{N}`/`State{N}` version step already uses
        // (`check_simple_field_bind_invariant` / `verify_simple_bind`).
        for (prefix, step) in &steps {
            self.check_simple_field_bind_invariant(
                prefix.clone(),
                step.source.version,
                step.target.version,
                "value-type conditional's family-slot extraction",
                span,
            );
        }
        // BT-3821 fault injection: the merge `Bind` the construct declared its family
        // for is omitted, so the verifier's `ConstructFamilyDropped` is shown to fire
        // on a real compile (`tests/value_type_conditional_family_verifier_corpus.rs`).
        #[cfg(test)]
        if super::super::threaded_expr::fault::active()
            == Some(super::super::threaded_expr::fault::LocalRebindFault::DropFamilyBind)
        {
            return Vec::new();
        }
        extraction
    }
}

/// The arms of a last-position value-type read+write conditional
/// ([`CoreErlangGenerator::emit_vt_conditional_case_to_var`]): a solo
/// `ifTrue:`/`ifFalse:` (its block's value; the non-taken answer comes from
/// [`SoloBoolean`] once the receiver var exists), or both `ifTrue:ifFalse:`
/// arms.
enum VtCondArms {
    Solo(SoloBoolean, Document<'static>),
    Both(Document<'static>, Document<'static>),
}
