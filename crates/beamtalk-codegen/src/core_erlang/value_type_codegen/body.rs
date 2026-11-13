// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type body lowering: statement classification, `self.field :=`
//! assignments, loops, and the "open" state-threading layer for method bodies.
//!
//! **DDD Context:** Code Generation

use super::VtBodyExprKind;

use super::super::control_flow::analysis::ThreadedFamilies;
use super::super::control_flow::family_slots::{self};
use super::super::control_flow::{BodyKind, ListOpKind, ThreadingPlan};
use super::super::intrinsics::validate_block_arity_exact;
use super::super::method_frame::{MethodBoundary, MethodFrame};
use super::super::threaded_ir::{
    FrameId, ThreadedStmt, ThreadedValue, TokenId, ValueRef, VersionPrefix,
};
use super::super::{CodeGenContext, CoreErlangGenerator, NlrBoundary, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, concat, join, leaf};
use beamtalk_core::ast::{
    Block, ClassDefinition, Expression, MessageSelector, MethodDefinition, WellKnownSelector,
};
use beamtalk_core::semantic_analysis::InferredType;
use beamtalk_core::semantic_analysis::type_checker::ffi_annotation_type;

impl CoreErlangGenerator {
    // ──────────────────────────────────────────────────────────────────────────
    // Auto-generated slot methods for `Value subclass:` classes
    // ──────────────────────────────────────────────────────────────────────────

    // ── Unified value-type method body classification ─────────

    /// BT-3816: sequences the operands of a value-type body statement that
    /// hold a field assignment ahead of the statement.
    ///
    /// The threading constructs (`do:` and the other loops and folds, the
    /// conditionals, `on:do:`/`ensure:`, a captured-mutation block value) are
    /// lowered by their own open-chain builders, each of which compiles its
    /// receiver and non-block arguments with a plain `expression_doc`. A field
    /// assignment there (`((self.n := 5) > 3) ifTrue: [...]`,
    /// `(self.n := 3) timesRepeat: [...]`) would scope its `Self{N}` rebind
    /// away. The operands that hold one are lowered through
    /// [`Self::sequence_children`] instead — the sequencing rule
    /// `lower_local_threading_construct` applies to a loop's own operands —
    /// and their prelude is pushed to `body_parts` ahead of the statement,
    /// which then reads each operand's value from its temp
    /// (`precompiled_subexprs`). The returned scope must be closed with
    /// `finish_precompiled_scope` once the statement is emitted. `None`, with
    /// nothing pushed, when no operand holds a field assignment.
    fn hoist_vt_operand_producers(
        &mut self,
        expr: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<super::super::sequencing::PrecompiledScope>> {
        if self.context != CodeGenContext::ValueType || self.in_class_method() {
            return Ok(None);
        }
        // `x := <construct>`: the construct is the assignment's value.
        let construct = match expr.unwrap_parens() {
            Expression::Assignment { target, value, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)) =>
            {
                value.unwrap_parens()
            }
            other => other,
        };
        let children = match construct {
            // `{a, b} := <value>`: the value is evaluated before the bindings.
            Expression::DestructureAssignment { value, .. } => vec![value.as_ref()],
            _ => match Self::sequenced_send_children(construct) {
                Some(children) => children,
                // A conditional send is opaque to the sequencing rule; its
                // receiver is its only operand (the arms are blocks).
                None => match Self::conditional_send_receiver(construct) {
                    Some(receiver) => vec![receiver],
                    None => return Ok(None),
                },
            },
        };
        if !children
            .iter()
            .any(|child| self.subexpr_has_field_write_producer(child))
        {
            return Ok(None);
        }
        let (prelude, scope) = self.sequence_children(&children, FrameId::ROOT)?;
        let doc = self.verified_root_prelude_doc(&prelude, expr.span());
        body_parts.push(docvec!["    ", doc, "\n"]);
        Ok(Some(scope))
    }

    /// BT-3816: whether `expr` is an inlined literal-block `value` send, in a
    /// value-type instance method, whose block writes a field. The block body
    /// is spliced into the enclosing statement sequence, so its `Self{N}`
    /// rebinds must reach the statements after it: such a send is a producer
    /// ([`Self::vt_inline_block_value_producer`]), mirrored by
    /// `subexpr_needs_prelude`.
    pub(in crate::core_erlang) fn is_vt_field_writing_block_value(
        &self,
        expr: &Expression,
    ) -> bool {
        self.context == CodeGenContext::ValueType
            && !self.in_class_method()
            && beamtalk_core::semantic_analysis::block_facts::literal_block_value_send(expr)
                .is_some_and(|(block, _)| {
                    !super::super::block_analysis::analyze_block(block)
                    .field_writes
                    .is_empty()
                    // The block's value is its last statement's: a construct
                    // there (its value is not recoverable from its open chain)
                    // keeps the existing path.
                    && super::super::util::collect_body_exprs(&block.body)
                        .last()
                        .is_none_or(|last| self.vt_block_stmt_is_threadable(last))
                })
    }

    /// BT-3816: whether a statement of an inlined block body is lowered through
    /// `threaded_expression` (a plain expression, a field assignment, a local
    /// assignment of one, a nested field-writing block value) rather than an
    /// open-chain construct builder.
    fn vt_block_stmt_is_threadable(&self, stmt: &Expression) -> bool {
        match self.classify_vt_body_expr(stmt) {
            VtBodyExprKind::Pure
            | VtBodyExprKind::FieldAssignment
            | VtBodyExprKind::EarlyReturn => true,
            VtBodyExprKind::LocalAssignment => match stmt {
                Expression::Assignment { value, .. } => {
                    matches!(self.classify_vt_body_expr(value), VtBodyExprKind::Pure)
                }
                _ => false,
            },
            VtBodyExprKind::BlockWithCapturedMutations(_) => {
                self.is_vt_field_writing_block_value(stmt)
            }
            // BT-3821: a conditional whose arms write a field is a producer
            // ([`Self::vt_conditional_value_producer`]): its value and `Self`
            // merge splice into the sequence like any other threadable statement.
            VtBodyExprKind::ConditionalWithLocalThreading
            | VtBodyExprKind::ConditionalWithSelfFieldThreading => {
                self.is_vt_field_conditional_producer(stmt)
            }
            _ => false,
        }
    }

    /// BT-3816: the producer lowering of an inlined `[body] value` send whose
    /// block writes a field ([`Self::is_vt_field_writing_block_value`]): the
    /// block body is spliced into the caller's statement sequence as
    /// statements of its own, each lowered through
    /// [`Self::threaded_expression`], so a field assignment anywhere in the
    /// body (`[self take: (self.n := 5)] value`) is a real `Self` `Bind` the
    /// statements after the send read. The block's value is its last
    /// expression. Parameters are bound to the arguments in a scope of their
    /// own; a captured outer local the block writes stays rebound after it,
    /// as in [`Self::generate_vt_block_value_open`].
    pub(in crate::core_erlang) fn vt_inline_block_value_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if !self.is_vt_field_writing_block_value(expr) {
            return Ok(None);
        }
        let Some((block, arguments)) =
            beamtalk_core::semantic_analysis::block_facts::literal_block_value_send(expr)
        else {
            return Ok(None);
        };
        let children: Vec<&Expression> = arguments.iter().collect();
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        self.push_scope();
        for (param, arg) in block.parameters.iter().zip(arguments) {
            let arg_var = self.fresh_temp_var(&param.name);
            let arg_doc = self.expression_doc(arg)?;
            prelude.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(arg_var.clone()), " = ", arg_doc, " in "],
                arg.span(),
            ));
            self.bind_var(&param.name, &arg_var);
        }
        self.finish_precompiled_scope(scope)?;

        let captured = Self::captured_mutations_for_block(block);
        let body = super::super::util::collect_body_exprs(&block.body);
        let mut rebound: Vec<(String, String)> = Vec::new();
        let mut value = ValueRef::Literal("'nil'");
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i + 1 == body.len();
            let kind = self.classify_vt_body_expr(stmt);
            if !self.vt_block_stmt_is_threadable(stmt) {
                // A loop / conditional / `on:do:` statement thread `Self` through
                // their own open chains, which render a Document: keep them
                // opaque statements (`is_vt_field_writing_block_value` has
                // checked the block's value is not one).
                let mut parts = Vec::new();
                let operands = self.hoist_vt_operand_producers(stmt, &mut parts)?;
                self.emit_vt_body_expr(stmt, &kind, 1, false, &mut parts)?;
                if let Some(scope) = operands {
                    self.finish_precompiled_scope(scope)?;
                }
                prelude.push(ThreadedStmt::Statement(Document::Vec(parts), stmt.span()));
                continue;
            }
            let local_target = match stmt {
                Expression::Assignment { target, value, .. } => match target.as_ref() {
                    Expression::Identifier(id) => Some((id.name.as_str(), value.as_ref())),
                    _ => None,
                },
                _ => None,
            };
            if let Some((name, rhs)) = local_target {
                let tv = self.threaded_expression(rhs, frame)?;
                prelude.extend(tv.prelude);
                let core_var = self
                    .lookup_var(name)
                    .map_or_else(|| Self::to_core_erlang_var(name), String::clone);
                let rhs_doc = self.threaded_value_doc(&tv.value);
                prelude.push(ThreadedStmt::Statement(
                    docvec!["let ", leaf::var(core_var.clone()), " = ", rhs_doc, " in "],
                    stmt.span(),
                ));
                self.bind_var(name, &core_var);
                if is_last {
                    value = ValueRef::Var(core_var);
                }
            } else {
                let tv = self.threaded_expression(stmt, frame)?;
                prelude.extend(tv.prelude);
                if is_last {
                    value = tv.value;
                } else {
                    let tmp = self.fresh_temp_var("seq");
                    let doc = self.threaded_value_doc(&tv.value);
                    prelude.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(tmp), " = ", doc, " in "],
                        stmt.span(),
                    ));
                }
            }
        }
        // A captured outer local the body (re)bound stays rebound after the
        // block's scope is popped.
        for name in &captured {
            if let Some(core_var) = self.lookup_var(name) {
                rebound.push((name.clone(), core_var.clone()));
            }
        }
        self.pop_scope();
        for (name, core_var) in &rebound {
            self.bind_var(name, core_var);
        }
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// BT-3816: whether the value-type instance-method body statement `stmt`
    /// holds a `Self`-rebinding producer — a field assignment in value
    /// position (`self take: (self.n := 5)`, `^ (self.n := 5)`, `c := (self.n
    /// := 5)`, `self.m := (self.n := 5)`, a binary / list / interpolation /
    /// cascade operand) — so the statement must be lowered through
    /// [`Self::threaded_expression`] and its prelude spliced into the method
    /// body (`lower_root_statement_with_producer`). Compiled as a plain closed
    /// sub-expression instead, the `Self{N}` rebind is scoped away and every
    /// later statement reads an unbound or stale `Self`: the
    /// `StateEffectEscapesExpression { prefix: SelfVt }` verifier finding.
    ///
    /// The statement being itself a field or local assignment is not a
    /// producer (the body lowering owns those shapes); only its right-hand side
    /// is probed. The loop / fold / conditional / `on:do:` statement kinds
    /// thread `Self` through their own open chains and are not probed here.
    /// `false` outside a value-type instance method.
    pub(in crate::core_erlang) fn statement_has_value_type_producer(
        &self,
        stmt: &Expression,
    ) -> bool {
        if self.context != CodeGenContext::ValueType || self.in_class_method() {
            return false;
        }
        match stmt.unwrap_parens() {
            Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
                self.subexpr_has_field_write_producer(value)
            }
            Expression::DestructureAssignment { .. } => false,
            // BT-3821: a statement that IS a conditional whose arms write a field
            // is a producer itself, in last position too (where it is the method's
            // value and its arms' `Self` merge must still reach the returned `Self`).
            other if self.is_vt_field_conditional_producer(other) => true,
            other => {
                matches!(
                    self.classify_vt_body_expr(other),
                    VtBodyExprKind::Pure | VtBodyExprKind::BlockWithCapturedMutations(_)
                ) && self.subexpr_has_field_write_producer(other)
            }
        }
    }

    /// Classify a value-type body expression for Self-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment,
    /// `do:` with local threading) must come before general ones (e.g. pure).
    fn classify_vt_body_expr(&self, expr: &Expression) -> VtBodyExprKind {
        if matches!(expr, Expression::Return { .. }) {
            return VtBodyExprKind::EarlyReturn;
        }
        if self.is_field_assignment(expr) {
            return VtBodyExprKind::FieldAssignment;
        }
        if Self::is_local_var_assignment(expr) {
            return VtBodyExprKind::LocalAssignment;
        }
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return VtBodyExprKind::DestructureAssignment;
        }
        if self.is_do_with_vt_local_threading(expr) {
            return VtBodyExprKind::DoWithLocalThreading;
        }
        if self.is_counted_loop_with_vt_local_threading(expr) {
            return VtBodyExprKind::CountedLoopWithLocalThreading;
        }
        if self.is_while_with_vt_local_threading(expr) {
            return VtBodyExprKind::WhileWithLocalThreading;
        }
        if self.is_foldl_list_op_with_vt_local_threading(expr) {
            return VtBodyExprKind::FoldlListOpWithLocalThreading;
        }
        if self.is_conditional_with_vt_local_threading(expr) {
            return VtBodyExprKind::ConditionalWithLocalThreading;
        }
        if self.is_conditional_with_vt_self_field_threading(expr) {
            return VtBodyExprKind::ConditionalWithSelfFieldThreading;
        }
        // BT-3486/BT-3506: an `on:do:`/`ensure:` whose ONLY mutation is a
        // `SelfVt` family write has no threaded outer local, so
        // the BT-3177 predicate alone says `false` and the construct falls
        // through to `Pure` — sequenced away as `let _seqN = <construct> in`,
        // which discards the trailing family slot and silently loses the
        // mutation.
        // `is_exception_construct_with_vt_local_threading` (ADR 0122) now
        // ALSO answers `true` for family threading, folding what used to be
        // a two-predicate OR here into one call.
        if self.is_exception_construct_with_vt_local_threading(expr) {
            return VtBodyExprKind::ExceptionConstructWithThreading;
        }
        if let Some(mutations) = Self::inline_block_captured_mutations(expr) {
            return VtBodyExprKind::BlockWithCapturedMutations(mutations);
        }
        VtBodyExprKind::Pure
    }

    /// Emit a "last expression" in a value-type method body: the expression's
    /// value, which is the method's (BT-4503: with or without a block `^`).
    pub(in crate::core_erlang) fn emit_vt_last_expr(
        &mut self,
        expr: &Expression,
        index: usize,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        // A last expression that is a local-threading construct —
        // a loop (whileTrue:/whileFalse:, to:do:/to:by:do:/timesRepeat:), a foldl list-op
        // (collect:/select:/reject:/inject:into:), or a read+write conditional — yields a
        // `{value, StateAcc}` tuple. In last position the threaded locals don't escape, so
        // the method value is the construct's *logical* result — element 1 — not the raw
        // tuple. Route through the shared `ThreadedExpr` emitter, which unwraps it and
        // applies the value-type boundary.
        if self.emit_threaded_last(
            expr,
            super::super::threaded_expr::ThreadingBoundary::ValueType,
            body_parts,
        )? {
            return Ok(());
        }

        // ADR 0089: splice the expression Document directly. The typed
        // leaves the expression is built from carry the Core Erlang structure, so
        // there is no raw rendered fragment to re-wrap in `leaf::var`.
        let expr_doc = self.expression_doc(expr)?;
        if index > 0 {
            body_parts.push(Document::Str("    "));
        }
        body_parts.push(expr_doc);
        Ok(())
    }

    /// ADR 0122 Decision 3: the ONE place a construct's trailing family slot
    /// at element 3 of its `{Value, StateAcc, Self}` result tuple
    /// is extracted and rebound — shared by every construct whose extra
    /// slot sits at that same position: the value-type/class-method Letrec
    /// loop's three extraction sites
    /// (the local-threading producer's construct tuple —
    /// `lower_local_threading_construct`, ADR 0131 — in last, `^` and
    /// assignment-RHS position, [`Self::emit_vt_loop_open_extraction`]'s non-last-position
    /// open chain, [`Self::emit_vt_threaded_local_assignment`]'s
    /// assignment-RHS threading inside a conditional branch) AND `on:do:`/`ensure:`'s own three mirror
    /// sites (BT-3506: [`Self::emit_vt_exception_tuple_unwrap_to_var`],
    /// [`Self::generate_vt_exception_construct_open`],
    /// [`Self::emit_vt_exception_assign_rhs`]) — so none of the six can
    /// independently drift on which family maps to which rebind (AGENTS.md's
    /// no-duplicate-implementations rule).
    ///
    /// `families` is the caller's own [`ThreadedFamilies`] answer for the
    /// SAME construct `tuple_var` was bound from
    /// ([`Self::vt_loop_threaded_families`] for a loop,
    /// [`Self::exception_construct_threaded_families`] for `on:do:`/
    /// `ensure:`) — at most one family (`SelfVt`) ever threads through
    /// either construct's own trailing slot (see
    /// [`CoreErlangGenerator::loop_body_threads_value_self`]), so this reads
    /// only `families.as_slice().first()`. Returns `None` when the construct
    /// carries no extra slot at all.
    pub(in crate::core_erlang) fn extract_vt_loop_family_slot(
        &mut self,
        tuple_var: &str,
        families: &ThreadedFamilies,
        span: beamtalk_core::source_analysis::Span,
    ) -> Option<Document<'static>> {
        let prefix = families.as_slice().first()?;
        let value_doc = docvec![
            "call 'erlang':'element'(3, ",
            leaf::var(tuple_var.to_string()),
            ")"
        ];
        Some(match prefix {
            VersionPrefix::SelfVt => self.rebind_value_self_from_doc(value_doc, span),
            other => unreachable!(
                "a value-type Letrec loop's or on:do:/ensure:'s own trailing slot \
                 only ever carries SelfVt, got {other:?}"
            ),
        })
    }

    /// Lowers a last/return-position `on:do:`/`ensure:` into its logical
    /// result var: binds the construct's own `{Result, StateAcc[, Family]}`
    /// tuple and extracts element 1. The exception-construct mirror of the
    /// loop/fold local-threading producer's last-position lowering
    /// (`lower_local_threading_construct`, ADR 0131). Threaded outer locals (element 2) do not
    /// escape in last position, unlike
    /// [`Self::generate_vt_exception_construct_open`]'s non-last extraction,
    /// and are discarded here. When the construct also threads the `SelfVt`
    /// family ([`Self::exception_construct_threaded_families`]),
    /// element 3 is rebound via [`Self::extract_vt_loop_family_slot`] (BT-3512's
    /// shared extraction helper — this construct's own trailing slot is the
    /// same shape at the same position, so it reuses that helper rather than
    /// a second one) so the method's own returned value (and NLR tuple)
    /// reflects the construct's mutation rather than the pre-`try` snapshot.
    pub(in crate::core_erlang) fn emit_vt_exception_tuple_unwrap_to_var(
        &mut self,
        expr: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<String> {
        let families = self.exception_construct_threaded_families(expr);
        let span = expr.span();
        let tuple_var = self.fresh_temp_var("ExTuple");
        let result_var = self.fresh_temp_var("ExResult");
        let expr_doc = self.expression_doc(expr)?;
        body_parts.push(docvec![
            "    let ",
            leaf::var(tuple_var.clone()),
            " = ",
            expr_doc,
            " in\n    let ",
            leaf::var(result_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in\n",
        ]);
        if let Some(rebind_doc) = self.extract_vt_loop_family_slot(&tuple_var, &families, span) {
            body_parts.push(docvec!["    ", rebind_doc, "\n"]);
        }
        Ok(result_var)
    }

    /// Returns `true` if `expr` evaluates to a `{value, StateAcc}` threaded
    /// tuple in value-type / class-method context, whose element 1 is the logical result and
    /// element 2 is the `StateAcc` map of mutated outer locals.
    ///
    /// Covers:
    /// - the local-threading **loops** — `whileTrue:`/`whileFalse:` and the counted loops
    ///   `to:do:`/`to:by:do:`/`timesRepeat:` — whose element 1 is `'nil'`;
    /// - the foldl **list-ops** — `collect:`/`select:`/`reject:`/`inject:into:` — whose
    ///   element 1 is the meaningful collected/folded result.
    ///
    /// Used both to unwrap such a construct as a method's last expression
    /// ([`Self::emit_vt_last_expr`]) and to detect a threading-construct assignment RHS
    /// ([`Self::emit_vt_threaded_local_assignment`]). All branches use the same predicates
    /// that route these to their `VtBodyExprKind`, so detection and the open-extraction path
    /// stay consistent.
    ///
    /// `do:` is excluded — its value-type codegen already returns a bare `nil`.
    pub(in crate::core_erlang) fn expr_yields_vt_threaded_tuple(&self, expr: &Expression) -> bool {
        // a threading construct used as a (parenthesized) assignment RHS —
        // `_r := (1 to: 5 do: [...])` — wraps the construct in `Expression::Parenthesized`.
        // Peel the wrappers so the `{value, StateAcc}` predicates (which only match
        // `MessageSend`) see the real construct.
        let expr = expr.unwrap_parens();
        self.is_while_with_vt_local_threading(expr)
            || self.is_counted_loop_with_vt_local_threading(expr)
            || self.is_foldl_list_op_with_vt_local_threading(expr)
    }

    /// Generate a fallback body for an empty value-type method: `nil`.
    pub(in crate::core_erlang) fn generate_vt_empty_body() -> Vec<Document<'static>> {
        vec![docvec!["    nil\n"]]
    }

    /// Generate classified body expressions for a value-type method.
    ///
    /// Classifies each expression via [`Self::classify_vt_body_expr`] and dispatches
    /// to the appropriate handler. Classification happens inline (not upfront) because
    /// some checks (e.g. `is_conditional_with_vt_local_threading`) depend on scope
    /// bindings established by earlier expressions.
    pub(in crate::core_erlang) fn generate_vt_body_exprs(
        &mut self,
        body: &[&Expression],
    ) -> Result<Vec<Document<'static>>> {
        self.with_local_producer_sites(body, |this| this.generate_vt_body_exprs_in_sites(body))
    }

    /// [`Self::generate_vt_body_exprs`] once the method body's local-producer
    /// sites are set (ADR 0131 Phase 2).
    #[allow(clippy::too_many_lines)]
    fn generate_vt_body_exprs_in_sites(
        &mut self,
        body: &[&Expression],
    ) -> Result<Vec<Document<'static>>> {
        let mut body_parts: Vec<Document<'static>> = Vec::with_capacity(body.len());
        let body_len = body.len();

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body_len - 1;
            // BT-3830/BT-4092: INVARIANT - classification and emission see the same node,
            // and a statement means the same with or without parentheses. Most of
            // `classify_vt_body_expr`'s predicates do not look through parentheses, while
            // others (`is_vt_field_conditional_producer`) do, so a parenthesised statement
            // was classified `Pure` while its bare form got a threading kind. `(s)` is
            // exactly `s` in every statement position, last included: a trailing field
            // assignment answers the updated `Self` whether or not it is parenthesised
            // (BT-4092; write `self.x := v. v` to answer the value). Unwrap it once here,
            // for every kind and every position.
            let expr = expr.unwrap_parens();
            let kind = self.classify_vt_body_expr(expr);

            // ADR 0131 Phase 2: a local-threading producer in this statement
            // rebinds its locals at the method root, so the statement is
            // lowered through `threaded_expression` with its prelude spliced
            // (`lower_root_statement_with_producer`, shared with class
            // methods).
            // BT-3821: a NON-LAST bare field-writing-arm conditional keeps its statement
            // form (`generate_vt_conditional_open`, arms' values discarded); only its
            // value positions are the producer's.
            let statement_form = !is_last && self.is_vt_field_conditional_producer(expr);
            if !matches!(kind, VtBodyExprKind::EarlyReturn) && !statement_form {
                let mut stmts = Vec::new();
                if let Some(result_var) =
                    self.lower_root_statement_with_producer(expr, &mut stmts)?
                {
                    let doc = self.verified_root_prelude_doc(&stmts, expr.span());
                    body_parts.push(docvec!["    ", doc, "\n"]);
                    if is_last && matches!(kind, VtBodyExprKind::FieldAssignment) {
                        body_parts.push(self.vt_updated_self_tail());
                    } else if is_last {
                        body_parts.push(self.threading_result_tail(
                            &result_var,
                            None,
                            super::super::threaded_expr::ThreadingBoundary::ValueType,
                        ));
                    }
                    continue;
                }
            }

            // Early return (^) at the method body level — emit value and stop generating.
            // Note: ^ inside a block is handled via the NLR throw mechanism.
            if matches!(kind, VtBodyExprKind::EarlyReturn) {
                if let Expression::Return { value, .. } = expr {
                    // ADR 0131 Phase 2: `^<loop or fold>`, or a `^` value
                    // with one at an operand site — the producer's prelude
                    // runs before the value is returned.
                    let threaded_value = if let Some(locals) =
                        self.construct_tuple_locals(value.unwrap_parens())
                    {
                        Some(self.lower_local_threading_construct(
                            value.unwrap_parens(),
                            &locals,
                            super::super::threaded_expr::ProducerTail::Discard,
                            super::super::threaded_ir::FrameId::ROOT,
                        )?)
                    } else if self.statement_has_local_producer(expr)
                        || self.statement_has_value_type_producer(expr)
                    {
                        let tv = self
                            .threaded_expression(value, super::super::threaded_ir::FrameId::ROOT)?;
                        Some((tv.prelude, tv.value))
                    } else {
                        None
                    };
                    if let Some((prelude, value)) = threaded_value {
                        let prelude_doc = self.verified_root_prelude_doc(&prelude, expr.span());
                        let value_doc = self.threaded_value_doc(&value);
                        let result_var = self.fresh_temp_var("EarlyResult");
                        body_parts.push(docvec![
                            "    ",
                            prelude_doc,
                            "let ",
                            leaf::var(result_var.clone()),
                            " = ",
                            value_doc,
                            " in\n",
                        ]);
                        body_parts.push(self.threading_result_tail(
                            &result_var,
                            None,
                            super::super::threaded_expr::ThreadingBoundary::ValueType,
                        ));
                        break;
                    }
                    // BT-3816: a conditional / loop `^` value whose receiver holds a
                    // field write is sequenced ahead of the return.
                    let operands = self.hoist_vt_operand_producers(value, &mut body_parts)?;
                    let expr_doc = self.expression_doc(value)?;
                    if i > 0 {
                        body_parts.push(Document::Str("    "));
                    }
                    body_parts.push(expr_doc);
                    if let Some(scope) = operands {
                        self.finish_precompiled_scope(scope)?;
                    }
                }
                break;
            }

            let operands = self.hoist_vt_operand_producers(expr, &mut body_parts)?;
            self.emit_vt_body_expr(expr, &kind, i, is_last, &mut body_parts)?;
            if let Some(scope) = operands {
                self.finish_precompiled_scope(scope)?;
            }
        }

        Ok(body_parts)
    }

    /// The tail of a method whose last statement is a field assignment: the
    /// updated `Self`, not the assigned value.
    fn vt_updated_self_tail(&self) -> Document<'static> {
        docvec!["    ", leaf::var(self.current_self_var()), "\n"]
    }

    /// Emit code for a single classified value-type body expression.
    ///
    /// Dispatches on the [`VtBodyExprKind`] to generate the appropriate
    /// Self-threading code for each expression kind.
    ///
    /// `EarlyReturn` is handled by the caller (`generate_vt_body_exprs`)
    /// before calling this method, so it is not matched here.
    #[allow(clippy::too_many_lines)]
    fn emit_vt_body_expr(
        &mut self,
        expr: &Expression,
        kind: &VtBodyExprKind,
        index: usize,
        is_last: bool,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        match kind {
            VtBodyExprKind::EarlyReturn => unreachable!("handled before dispatch"),
            VtBodyExprKind::FieldAssignment => {
                // Value type field assignment.
                // Non-last: open Self-threading let chain so Self{N} stays in scope.
                // Last: return the updated Self (not the assigned value).
                let doc = self.generate_vt_field_assignment_open(expr)?;
                body_parts.push(doc);
                if is_last {
                    body_parts.push(self.vt_updated_self_tail());
                }
            }
            VtBodyExprKind::LocalAssignment => {
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    // Local variable assignment — create a proper Core Erlang
                    // let binding so the variable is accessible in subsequent expressions.
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let val_doc = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            body_parts.push(docvec![
                                "    let ",
                                leaf::var(core_var),
                                " = ",
                                val_doc,
                                " in\n"
                            ]);
                        }
                    }
                }
            }
            VtBodyExprKind::DestructureAssignment => {
                if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                    let (binding_docs, rhs_var) =
                        self.generate_destructure_bindings_with_value(pattern, value)?;
                    if is_last {
                        // BT-3889: a destructuring assignment evaluates to its
                        // right-hand side, the method's value.
                        for d in binding_docs {
                            body_parts.push(docvec!["    ", d]);
                        }
                        body_parts.push(docvec!["    ", leaf::var(rhs_var), "\n"]);
                    } else {
                        for d in binding_docs {
                            body_parts.push(d);
                        }
                    }
                }
            }
            VtBodyExprKind::DoWithLocalThreading => {
                // Non-last `do:` loop that mutates captured outer locals.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_value_type_do_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::ConditionalWithLocalThreading => {
                // Non-last conditional with captured local mutations.
                // In last position the block is stateful (it reads+writes an outer
                // local), so the normal dispatch (`True>>ifTrue:`) would call the block with
                // 0 args and crash. Inline the conditional as a `case` returning the branch's
                // logical value instead — the threaded locals don't need to escape in last
                // position — route through the shared `emit_vt_last_expr` like the loop
                // / foldl-list-op last branches — it dispatches the conditional through the same
                // unified `emit_threaded_last` emitter, so the dedicated `emit_vt_conditional_last`
                // wrapper is redundant.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_conditional_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::ConditionalWithSelfFieldThreading => {
                // BT-3821: a method-root conditional the producer admits
                // (`is_vt_field_conditional_producer`) never reaches this arm —
                // `lower_root_statement_with_producer` lowers it first, in last
                // position too. Only a conditional with a non-literal branch
                // argument still gets here (a loop / branch / exception frame never
                // does: those shapes are rejected earlier, see
                // `generate_vt_conditional_open`): non-last, that is the statement
                // form; in LAST position it deliberately falls through to the
                // ordinary last-expression path, which reproduces the
                // `FieldAssignmentInUnsupportedBlock` diagnostic — see
                // `is_conditional_with_vt_self_field_threading`'s doc comment
                // for why a clean compile error is the right answer there
                // rather than a half-threaded one.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_conditional_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::WhileWithLocalThreading => {
                // Non-last whileTrue:/whileFalse: with captured local mutations.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_while_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::ExceptionConstructWithThreading => {
                // Non-last on:do:/ensure: with captured local
                // mutations, and/or the value-type `self.field := ...`
                // case on the same non-last path. Last position is not yet
                // wired through the shared `ThreadedExpr` emitter
                // (`emit_threaded_last` has no on:do:/ensure: case) — tracked
                // as a follow-up; today's fixtures/tests only exercise this in
                // non-last position, same as this issue's repro shapes.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_exception_construct_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::CountedLoopWithLocalThreading => {
                // to:do:/to:by:do:/timesRepeat: with captured local mutations.
                // In last position emit_vt_last_expr unwraps the {'nil', StateAcc} tuple.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_counted_loop_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::FoldlListOpWithLocalThreading => {
                // collect:/select:/reject:/inject:into: with captured local
                // mutations. These emit a `{value, StateAcc}` tuple where element 1 is the
                // meaningful result. In last position emit_vt_last_expr unwraps element 1;
                // in non-last position we extract the threaded locals (discarding the value).
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_foldl_list_op_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::BlockWithCapturedMutations(mutations) => {
                // Non-last block value with captured mutations.
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    let doc = self.generate_vt_block_value_open(expr, mutations)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::Pure => {
                if is_last {
                    self.emit_vt_last_expr(expr, index, body_parts)?;
                } else {
                    // Non-last expressions: wrap in let to sequence side effects
                    let tmp_var = self.fresh_temp_var("seq");
                    let expr_doc = self.expression_doc(expr)?;
                    body_parts.push(docvec![
                        "    let ",
                        leaf::var(tmp_var),
                        " = ",
                        expr_doc,
                        " in\n",
                    ]);
                }
            }
        }
        Ok(())
    }

    /// Generates an instance method for a value type.
    ///
    /// Value type methods are pure functions that take Self as first parameter
    /// and return a new instance (immutable semantics).
    /// ADR 0101: Build the body of a `self delegate` method on a
    /// `native:` Object as a call through the unified FFI boundary.
    ///
    /// Lowers to:
    /// ```erlang
    /// call 'beamtalk_erlang_proxy':'native_call'(
    ///     'BackingModule', 'fnName', [Arg1, ...], {'ClassName', 'selector:'}, 'beamtalk')
    /// ```
    ///
    /// `arg_vars` are the Core Erlang variables to forward verbatim — the caller
    /// prepends `Self` for instance methods and omits it for class methods. The
    /// `{Class, Sel}` context lets the proxy report `ClassName>>selector:` on a
    /// wrapped error instead of the bare Erlang MFA. The trailing mode atom is
    /// the `stock`/`beamtalk` coercion mode, precomputed from the
    /// statically-known backing module (BT-4153). Routing through
    /// `native_call/5` (rather than a bare `Mod:Fn` call) is what gives the
    /// generated dispatch wrap-by-default error handling and ADR 0076
    /// ok/error → `Result` coercion, identical to inline `(Erlang …)` FFI.
    ///
    /// BT-4765: when the backing function's spec proves the result needs no
    /// coercion and every argument is proved to reach it unchanged
    /// (`arg_types`, one per `arg_vars`: the receiver's class first for an
    /// instance method, then each declared parameter type), the body is a
    /// guarded direct call instead, with `native_call/5` as its fallback (see
    /// [`CoreErlangGenerator::proved_native_delegate_call`]).
    pub(in crate::core_erlang) fn native_delegate_body_doc(
        &mut self,
        backing_module: &str,
        class_name: &str,
        selector: &MessageSelector,
        arg_vars: &[String],
        arg_types: &[Option<InferredType>],
    ) -> Document<'static> {
        if let Some(direct) = self.proved_native_delegate_call(
            backing_module,
            class_name,
            selector,
            arg_vars,
            arg_types,
        ) {
            return direct;
        }
        let fn_name = selector.leading_word();
        let args_list = join(
            arg_vars.iter().map(|v| leaf::var(v.clone())),
            &Document::Str(", "),
        );
        let mode = super::super::util::ffi_coercion_mode(backing_module);
        docvec![
            "call 'beamtalk_erlang_proxy':'native_call'(",
            leaf::atom(backing_module.to_string()),
            ", ",
            leaf::atom(fn_name),
            ", [",
            args_list,
            "], {",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::atom(selector.name().to_string()),
            "}, ",
            leaf::atom(mode),
            ")"
        ]
    }

    /// BT-4503: a value-type function body (a method's or a value-type
    /// extension fun's) inside its NLR boundary when `token` is set (a block
    /// of it holds a `^`), else the body as is. The boundary lowers through
    /// `ThreadedIr` as a [`ThreadedStmt::NlrCatch`] of
    /// [`NlrBoundary::ValueType`] closing the body, verified in the
    /// value-type scope (`VerifyError::MethodNlrAnswersPair`) and rendered by
    /// the one NLR catch template. Its catch arm answers the bare value, as
    /// the body does, so the function answers exactly what a send of it
    /// answers.
    pub(in crate::core_erlang) fn value_type_nlr_boundary_doc(
        &mut self,
        body: Document<'static>,
        token: Option<&str>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Document<'static> {
        let Some(token) = token else {
            return body;
        };
        let stmts = [
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::ValueType,
                token: TokenId::new(token),
                frame: FrameId::ROOT,
                span,
            },
            ThreadedStmt::Statement(body, span),
        ];
        let errors = self.verify_threaded_ir(&stmts);
        self.report_threaded_ir_verify_errors(
            &errors,
            "a value-type method's NLR boundary must answer the bare value",
            span,
        );
        let mut ctx = super::super::threaded_ir::RenderCtx::new(self);
        super::super::threaded_ir::render(&stmts, &mut ctx)
    }

    pub(in crate::core_erlang) fn generate_value_type_method(
        &mut self,
        method: &MethodDefinition,
        class_def: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let mangled = method.selector.name().to_string();
        let arity = method.parameters.len() + 1; // +1 for Self

        // Bind parameters via fresh_var (not to_core_var) so names go through
        // the counter and can't collide with sequencing temp vars.
        let (mut frame, user_params) = MethodFrame::enter(self, method, MethodBoundary::ValueType);
        let mut params = vec!["Self".to_string()];
        params.extend(user_params);

        // ADR 0101: On a `native:` Object, a `self delegate` body lowers
        // through the unified FFI boundary (`beamtalk_erlang_proxy:
        // native_call/4`) carrying `{Class, Sel}` — instance methods prepend
        // `Self` to the arg list (`params` already starts with Self).
        if let Some(backing) = class_def.backing_module.as_ref() {
            if method.is_self_delegate() {
                // The receiver is `Self`, an instance of this class.
                let arg_types: Vec<Option<InferredType>> =
                    std::iter::once(Some(InferredType::known(class_def.name.name.clone())))
                        .chain(
                            method
                                .parameters
                                .iter()
                                .map(|p| p.type_annotation.as_ref().and_then(ffi_annotation_type)),
                        )
                        .collect();
                let body = frame.native_delegate_body_doc(
                    backing.name.as_str(),
                    class_def.name.name.as_str(),
                    &method.selector,
                    &params,
                    &arg_types,
                );
                let params_doc = join(params.into_iter().map(leaf::var), &Document::Str(", "));
                let mut fun_doc = docvec!["fun (", params_doc, ") ->\n    ", body, "\n"];
                if let Some(line_num) = frame.span_to_line(method.span) {
                    fun_doc = frame.annotate_with_line(fun_doc, line_num);
                }
                // `frame` drops here (pops the scope, restores the selector)
                // on this early return, same as the fall-through path below.
                return Ok(docvec![leaf::fname(mangled, arity), " = ", fun_doc, "\n"]);
            }
        }

        // Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing method.
        let needs_nlr = frame
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);

        // Generate method body expressions. Value types support Self-threading
        // for field assignments: each `:=` produces a new Self{N} snapshot via
        // maps:put (see generate_field_assignment); field reads use
        // current_self_var() to reference the latest snapshot. Filter out
        // @expect directives — they are compile-time only and generate no code.
        let body = super::super::util::collect_body_exprs(&method.body);

        // If filtering leaves no executable expressions, emit a safe fallback to
        // avoid generating an empty Core Erlang function body which would be
        // syntactically invalid (e.g., `fun (...) ->\n\n`).
        // Capture the result so cleanup runs unconditionally before the `?`
        // below propagates an error, same as on the success path.
        let (body_result, nlr_token_var) = frame.with_nlr_scope(needs_nlr, |g| {
            if body.is_empty() {
                Ok(Self::generate_vt_empty_body())
            } else {
                g.generate_vt_body_exprs(&body)
            }
        });
        // Resolve the method-level line + file while `frame` still has a
        // flattened provision's protocol source swapped in (ADR 0127 §3);
        // after the drop below, `self` maps through this module's own source.
        let line_annotation = frame.span_to_line(method.span);
        let annotation_path = frame.source_path.clone();
        // `frame` has a `Drop` impl, so it borrows `self` until this explicit
        // drop (or the end of the function) rather than its last use under
        // NLL — drop it now so `self` is free again below, and so the scope
        // pop and selector restore run before `?` propagates an error, not
        // only at the end of the function.
        drop(frame);

        let body_parts = body_result?;

        // Annotate the `fun` expression (not just the body) with source line.
        // Annotating only the body would create invalid double-annotation when the body
        // is itself a single annotated MessageSend expression: `( ( e -| [...] ) -| [...] )`.

        // Wrap the method body in its NLR boundary to catch non-local
        // returns thrown by ^ inside block closures.
        let body_doc = self.value_type_nlr_boundary_doc(
            Document::Vec(body_parts),
            nlr_token_var.as_deref(),
            method.span,
        );
        let params_doc = join(params.into_iter().map(leaf::var), &Document::Str(", "));
        let fun_doc = docvec!["fun (", params_doc, ") ->\n", body_doc];
        let fun_doc = if let Some(line_num) = line_annotation {
            Self::annotate_with_line_in(annotation_path.as_deref(), fun_doc, line_num)
        } else {
            fun_doc
        };
        Ok(docvec![leaf::fname(mangled, arity), " = ", fun_doc, "\n",])
    }

    /// Generates an open Self-threading let chain for a non-last field assignment.
    ///
    /// For `self.field := value`, produces:
    /// ```erlang
    ///     let _Val = <value> in let Self{N} = call 'maps':'put'('field', _Val, Self{N-1}) in
    /// ```
    ///
    /// The trailing `in` leaves the let chain open so subsequent `body_parts` expressions
    /// flow as the continuation, keeping `Self{N}` in scope.
    fn generate_vt_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                // Capture current Self BEFORE generating value so RHS field reads
                // (e.g., `self.x := self.x + 1`) see the current snapshot.
                let current_self = self.current_self_var();
                let val_doc = self.expression_doc(value)?;
                let new_self = self.next_self_var();
                return Ok(docvec![
                    "    let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    val_doc,
                    " in let ",
                    leaf::var(new_self),
                    " = call 'maps':'put'(",
                    leaf::atom(field.name.clone()),
                    ", ",
                    leaf::var(val_var),
                    ", ",
                    leaf::var(current_self),
                    ") in\n",
                ]);
            }
        }
        // Fallback: sequence as a side-effecting expression
        let tmp_var = self.fresh_temp_var("seq");
        let expr_doc = self.expression_doc(expr)?;
        Ok(docvec![
            "    let ",
            leaf::var(tmp_var),
            " = ",
            expr_doc,
            " in\n",
        ])
    }

    /// Generate an open let-chain for a non-last `[block_withmutations] value`
    /// in `ValueType` context. Each assignment in the block body becomes a top-level
    /// `let Var = val in\n` so the mutation is visible to subsequent expressions.
    fn generate_vt_block_value_open(
        &mut self,
        expr: &Expression,
        mutations: &[String],
    ) -> Result<Document<'static>> {
        let (block, arguments) = if let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        {
            if let Expression::Block(block) = receiver.as_ref() {
                (block, arguments.as_slice())
            } else {
                // Fallback: sequence as side effect
                let tmp_var = self.fresh_temp_var("seq");
                let doc = self.expression_doc(expr)?;
                return Ok(docvec!["    let ", leaf::var(tmp_var), " = ", doc, " in\n",]);
            }
        } else {
            let tmp_var = self.fresh_temp_var("seq");
            let doc = self.expression_doc(expr)?;
            return Ok(docvec!["    let ", leaf::var(tmp_var), " = ", doc, " in\n",]);
        };

        let mut parts: Vec<Document<'static>> = Vec::new();

        // Use a nested scope so block parameters and block-local variables
        // don't leak into the method scope. Only captured mutations are
        // re-bound in the outer scope after the block finishes.
        self.push_scope();

        // Bind block parameters to arguments (for value: variants)
        for (i, param) in block.parameters.iter().enumerate() {
            if i < arguments.len() {
                let arg_var = self.fresh_temp_var(&param.name);
                let arg_doc = self.expression_doc(&arguments[i])?;
                parts.push(docvec![
                    "    let ",
                    leaf::var(arg_var.clone()),
                    " = ",
                    arg_doc,
                    " in\n",
                ]);
                self.bind_var(&param.name, &arg_var);
            }
        }

        // Emit block body as open let-chains.
        // Track which variables are captured mutations so we can re-bind them
        // in the outer scope after popping the parameter scope.
        let mut rebound_vars: Vec<(String, String)> = Vec::new();

        let body = super::super::util::collect_body_exprs(&block.body);

        for body_expr in &body {
            if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let core_var = self
                            .lookup_var(&id.name)
                            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                        let val_doc = self.expression_doc(value)?;
                        parts.push(docvec![
                            "    let ",
                            leaf::var(core_var.clone()),
                            " = ",
                            val_doc,
                            " in\n",
                        ]);
                        self.bind_var(&id.name, &core_var);
                        // Track for re-binding in outer scope
                        if mutations.iter().any(|m| m == id.name.as_str()) {
                            rebound_vars.push((id.name.to_string(), core_var));
                        }
                    }
                }
            } else {
                let tmp = self.fresh_temp_var("seq");
                let doc = self.expression_doc(body_expr)?;
                parts.push(docvec!["    let ", leaf::var(tmp), " = ", doc, " in\n",]);
            }
        }

        // Pop the block scope, then re-bind only captured mutations in the outer scope
        self.pop_scope();
        for (name, core_var) in &rebound_vars {
            self.bind_var(name, core_var);
        }

        Ok(Document::Vec(parts))
    }

    /// Returns `true` if `expr` is a `do:` message send with a literal
    /// block that, in `ValueType` context, captures and mutates outer local variables.
    ///
    /// Used by `generate_value_type_method` and `lower_class_method_body`
    /// (renamed from `generate_class_method_body`)
    /// to select the open-let-chain path for non-last `do:` loops so the mutated
    /// locals are visible to subsequent exprs.
    pub(in crate::core_erlang) fn is_do_with_vt_local_threading(&self, expr: &Expression) -> bool {
        // Allow class methods regardless of module context (Actor classes
        // have context=Actor but class methods still need local-map threading).
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if sel == "do:" {
                if let Some(Expression::Block(body)) = arguments.first() {
                    // BT-3827: a `do:` whose only mutation is a field write
                    // returns a threaded tuple too (`{'nil', StateAcc, Self{N}}`).
                    return !self.loop_threaded_locals(body, None).is_empty()
                        || self.fold_threads_value_self(expr);
                }
            }
        }
        false
    }

    /// Returns `true` if `expr` is a `whileTrue:` or `whileFalse:` message send
    /// whose body block captures and mutates outer local variables in value-type context.
    ///
    /// Uses the same `needs_mutation_threading` check that the while-loop codegen uses
    /// to decide whether to emit the stateful variant (returning `{'nil', StateAcc}`)
    /// or the simple variant (returning `'nil'`). This ensures the detection and codegen
    /// are consistent — we only extract threaded locals when the codegen actually packed them.
    pub(in crate::core_erlang) fn is_while_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            // Match `whileTrue:` / `whileFalse:` via the `WellKnownSelector` enum.
            if matches!(
                selector.well_known(),
                Some(WellKnownSelector::WhileTrue | WellKnownSelector::WhileFalse)
            ) {
                if let Some(Expression::Block(body)) = arguments.first() {
                    // The same check `generate_while_true`/`generate_while_false`
                    // use, including a stateful condition (BT-3746: a
                    // condition-only write used to produce a tuple nobody
                    // unpacked).
                    if !self.while_loop_threads(receiver, body) {
                        return false;
                    }
                    // Verify there are actually threaded locals to extract
                    let condition = if let Expression::Block(_) = receiver.as_ref() {
                        Some(receiver.as_ref())
                    } else {
                        None
                    };
                    // see the analogous comment in
                    // `is_counted_loop_with_vt_local_threading`.
                    // BT-4002: a field-writing condition threads `Self` too.
                    return !self.loop_threaded_locals(body, condition).is_empty()
                        || self.letrec_loop_threads_value_self(Some(receiver), body);
                }
            }
        }
        false
    }

    /// Generates a non-last `whileTrue:` / `whileFalse:` loop (with value-type
    /// captured local threading) as an **open let chain**.
    ///
    /// The while loop returns `{'nil', StateAcc}`. This method:
    /// 1. Binds the tuple result to a temp var
    /// 2. Extracts the `StateAcc` from element 2
    /// 3. Extracts each threaded local from the `StateAcc` via `maps:get`
    /// 4. Rebinds the variable names in scope so subsequent code sees the updated values
    pub(in crate::core_erlang) fn generate_vt_while_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // computed BEFORE `expression_doc` runs the loop's own codegen —
        // keeping it adjacent to the other pre-loop reads
        // (`get_while_threaded_locals`) matches the loop codegen's own read
        // order.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the while loop expression (returns {'nil', StateAcc} tuple)
        let loop_doc = self.expression_doc(expr)?;
        let threaded_locals = self.get_while_threaded_locals(expr);
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "WhileResult",
            "WhileState",
        ))
    }

    /// Shared open-let-chain extraction for non-last value-type loops
    /// that return a `{'nil', StateAcc}` tuple (`whileTrue:`/`whileFalse:` and the counted
    /// loops `to:do:`/`to:by:do:`/`timesRepeat:`).
    ///
    /// 1. Binds the tuple result to a temp var
    /// 2. Extracts the `StateAcc` from element 2
    /// 3. Extracts each threaded local from the `StateAcc` via `maps:get`, rebinding the
    ///    variable names in scope so subsequent code sees the updated values
    /// 4. ADR 0111 Addendum 9, Question 3 / ADR 0122 Decision 3: when
    ///    `families` is non-empty, ALSO extracts the loop's trailing element
    ///    3 and rebinds it via [`Self::extract_vt_loop_family_slot`] —
    ///    matching the extra trailing tuple slot `while_loops.rs`/
    ///    `counted_loops.rs` append to the `{'nil', StateAcc, …}` result.
    ///    Empty for a fold unless it writes a field and is one of
    ///    `SELF_THREADING_FOLDS` (BT-3827: `do:`/`collect:`/`inject:into:`
    ///    return `Self` there through `FoldFamilySlots::close_tuple`);
    ///    `select:` and the other folds' accumulators have no such slot.
    ///
    /// When there are no threaded locals AND no extra family slot, falls
    /// back to sequencing the loop as a side effect (`let _seqN = <loop> in`).
    pub(super) fn emit_vt_loop_open_extraction(
        &mut self,
        loop_doc: Document<'static>,
        threaded_locals: &[String],
        families: &ThreadedFamilies,
        span: beamtalk_core::source_analysis::Span,
        result_var_prefix: &str,
        state_var_prefix: &str,
    ) -> Document<'static> {
        if threaded_locals.is_empty() && families.as_slice().is_empty() {
            // Fallback: just sequence the expression
            let tmp = self.fresh_temp_var("seq");
            return docvec!["    let ", leaf::var(tmp), " = ", loop_doc, " in\n"];
        }

        let tuple_var = self.fresh_temp_var(result_var_prefix);

        let mut parts: Vec<Document<'static>> = Vec::new();

        // Bind the tuple result
        parts.push(docvec![
            "    let ",
            leaf::var(tuple_var.clone()),
            " = ",
            loop_doc,
            " in\n",
        ]);

        if !threaded_locals.is_empty() {
            let state_var = self.fresh_temp_var(state_var_prefix);

            // Extract StateAcc from element 2 of the tuple
            parts.push(docvec![
                "    let ",
                leaf::var(state_var.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var.clone()),
                ") in\n",
            ]);

            // Extract each threaded local from the state map
            for var_name in threaded_locals {
                let core_var = self.fresh_var(var_name);
                let key = Self::local_state_key(var_name);
                parts.push(docvec![
                    "    let ",
                    leaf::var(core_var.clone()),
                    " = call 'maps':'get'(",
                    leaf::atom(key),
                    ", ",
                    leaf::var(state_var.clone()),
                    ") in\n",
                ]);
                self.bind_var(var_name, &core_var);
            }
        }

        if let Some(rebind_doc) = self.extract_vt_loop_family_slot(&tuple_var, families, span) {
            parts.push(docvec!["    ", rebind_doc, "\n"]);
        }

        Document::Vec(parts)
    }

    /// Returns the threaded local variable names for any value-type threading
    /// construct that yields a `{value, StateAcc}` tuple (a `whileTrue:`/`whileFalse:` or
    /// counted loop, or a `collect:`/`select:`/`reject:`/`inject:into:` foldl list-op).
    ///
    /// Used by [`Self::emit_vt_threaded_local_assignment`] to rebind the threaded locals
    /// after extracting them from the `StateAcc`. Returns the same set the construct's
    /// codegen packed into the `StateAcc` (`loop_threaded_locals`).
    pub(in crate::core_erlang) fn vt_construct_threaded_locals(
        &self,
        expr: &Expression,
    ) -> Vec<String> {
        // peel `Expression::Parenthesized` so a parenthesized construct
        // (`(1 to: 5 do: [...])`) reports the same threaded locals its codegen packed.
        let expr = expr.unwrap_parens();
        if self.is_while_with_vt_local_threading(expr) {
            self.get_while_threaded_locals(expr)
        } else if self.is_counted_loop_with_vt_local_threading(expr) {
            Self::counted_loop_body_block(expr)
                .map(|body| self.loop_threaded_locals(body, None))
                .unwrap_or_default()
        } else {
            Self::foldl_list_op_body_block(expr)
                .map(|_| self.producer_threaded_locals(expr))
                .unwrap_or_default()
        }
    }

    /// Emits a value-type local assignment whose RHS is a threading construct
    /// returning a `{value, StateAcc}` tuple (a loop or foldl list-op).
    ///
    /// Binds the target variable to element 1 (the construct's logical value), then rebinds
    /// each threaded local from the `StateAcc` map at element 2 — so both the assigned value
    /// and the mutated locals are visible to subsequent method-body expressions. Without
    /// this, the target would be bound to the raw `{value, StateAcc}` tuple and the threaded
    /// locals would keep their pre-construct values.
    ///
    /// ADR 0131 Phase 2 (BT-3749): at the method root this is the
    /// local-threading producer's job (`lower_root_statement_with_producer`);
    /// the remaining callers are statements inside a conditional branch
    /// (`build_vt_conditional_branch_value_parts` and the open-branch
    /// builder), branch-arm frames that ADR 0131 Phase 3 (`BranchArm`)
    /// lowers, deleting this.
    ///
    /// Returns the Core Erlang variable bound to the assignment target.
    pub(in crate::core_erlang) fn emit_vt_threaded_local_assignment(
        &mut self,
        var_name: &str,
        value: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<String> {
        // ADR 0122 Decision 3: which extra trailing slot `value` carries as an
        // explicit 3rd tuple element, when it is a Letrec-shaped construct
        // (while/counted loop) or a field-writing `do:`/`collect:`/
        // `inject:into:` fold (BT-3827) — see `vt_loop_threaded_families`.
        let families = self.vt_loop_threaded_families(value);
        let span = value.span();
        let rhs_doc =
            self.with_fold_self_slot_consumer(value, |this| this.expression_doc(value))?;
        let tuple_var = self.fresh_temp_var("AssignThreaded");

        // Bind the {value, StateAcc} tuple.
        body_parts.push(docvec![
            "    let ",
            leaf::var(tuple_var.clone()),
            " = ",
            rhs_doc,
            " in\n",
        ]);
        // Bind the assignment target to element 1 (the logical value).
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
        body_parts.push(docvec![
            "    let ",
            leaf::var(core_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in\n",
        ]);
        self.bind_var(var_name, &core_var);

        // Extract the StateAcc (element 2) and rebind each threaded local from it.
        let threaded_locals = self.vt_construct_threaded_locals(value);
        let rebind: Vec<&String> = threaded_locals
            .iter()
            .filter(|tl| *tl != var_name)
            .collect();
        if !rebind.is_empty() {
            let state_var = self.fresh_temp_var("AssignThreadedState");
            body_parts.push(docvec![
                "    let ",
                leaf::var(state_var.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var.clone()),
                ") in\n",
            ]);
            for tl in rebind {
                let tl_core = self.fresh_var(tl);
                let key = Self::local_state_key(tl);
                body_parts.push(docvec![
                    "    let ",
                    leaf::var(tl_core.clone()),
                    " = call 'maps':'get'(",
                    leaf::atom(key),
                    ", ",
                    leaf::var(state_var.clone()),
                    ") in\n",
                ]);
                self.bind_var(tl, &tl_core);
            }
        }

        // extract the loop's own trailing slot from element
        // 3 and rebind it, same as the non-last-statement / last-position
        // consumers.
        if let Some(rebind_doc) = self.extract_vt_loop_family_slot(&tuple_var, &families, span) {
            body_parts.push(docvec!["    ", rebind_doc, "\n"]);
        }

        Ok(core_var)
    }

    /// Returns `true` if `expr` is a `to:do:` / `to:by:do:` / `timesRepeat:`
    /// counted loop whose body block captures and mutates outer local variables in
    /// value-type or class-method context.
    ///
    /// Mirrors `is_while_with_vt_local_threading`: only triggers when the loop codegen
    /// will actually pack threaded locals into the `{'nil', StateAcc}` result, so the
    /// detection and the extraction stay consistent.
    pub(in crate::core_erlang) fn is_counted_loop_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        let Some(body) = Self::counted_loop_body_block(expr) else {
            return false;
        };
        // a loop whose ONLY mutation is `self.field:=...` still
        // returns a threaded tuple (`{'nil', StateAcc, Self{N}}`) that must
        // be unpacked here — before this issue it had no threaded locals, so
        // this predicate said `false` and the loop's result was sequenced
        // away, discarding the mutation.
        !self.loop_threaded_locals(body, None).is_empty() || self.loop_body_threads_value_self(body)
    }

    /// ADR 0122 Decision 3: which extra trailing family the loop construct
    /// `expr`'s result tuple carries — non-empty for the Letrec-shaped
    /// constructs (`whileTrue:`/`whileFalse:`, `to:do:`/`to:by:do:`/
    /// `timesRepeat:`) and, since BT-3827, for a `do:`/`collect:`/
    /// `inject:into:` fold whose body writes a field
    /// ([`CoreErlangGenerator::fold_threads_value_self`]). Every other fold
    /// (`select:`, …) never matches here — its accumulator has no matching
    /// tuple slot and a value-type `self.field :=` inside one is rejected.
    /// Shares [`CoreErlangGenerator::loop_body_threads_value_self`] with
    /// `ThreadingPlan::new_impl` (`control_flow/plan.rs`) so the routing
    /// decision here and the tuple-shape decision the loop's own codegen
    /// makes can never independently drift out of sync.
    ///
    /// Deliberately keeps that narrow, top-level-only detector rather than
    /// the recursive [`CoreErlangGenerator::body_threaded_families`]; its
    /// result is a [`ThreadedFamilies`], the same type
    /// [`Self::extract_vt_loop_family_slot`] and
    /// `while_loops.rs`/`counted_loops.rs`'s own exit-arm tuple share.
    pub(in crate::core_erlang) fn vt_loop_threaded_families(
        &self,
        expr: &Expression,
    ) -> ThreadedFamilies {
        let expr = expr.unwrap_parens();
        // BT-3827: a fold's result tuple carries `Self` at the same trailing slot.
        if self.fold_threads_value_self(expr) {
            return ThreadedFamilies::from_matches(&[VersionPrefix::SelfVt]);
        }
        // BT-4002: a while loop's condition joins its body (`condition`).
        let (condition, body) = if self.is_while_with_vt_local_threading(expr) {
            match expr {
                Expression::MessageSend {
                    receiver,
                    arguments,
                    ..
                } => match arguments.first() {
                    Some(Expression::Block(body)) => (Some(receiver.as_ref()), Some(body)),
                    _ => (None, None),
                },
                _ => (None, None),
            }
        } else if self.is_counted_loop_with_vt_local_threading(expr) {
            (None, Self::counted_loop_body_block(expr))
        } else {
            (None, None)
        };
        let Some(body) = body else {
            return ThreadedFamilies::default();
        };
        if self.letrec_loop_threads_value_self(condition, body) {
            ThreadedFamilies::from_matches(&[VersionPrefix::SelfVt])
        } else {
            ThreadedFamilies::default()
        }
    }

    /// Returns the body block of a counted loop message send
    /// (`timesRepeat:`, `to:do:`, `to:by:do:`), or `None` if `expr` is not one.
    fn counted_loop_body_block(expr: &Expression) -> Option<&Block> {
        let (selector, body) = Self::producer_body(expr)?;
        beamtalk_core::state_threading_selectors::is_counted_loop_selector(&selector)
            .then_some(body)
    }

    /// The selector and literal body block of a loop or fold in
    /// `beamtalk_core`'s one body-index table
    /// (`state_threading_selectors::producer_body_arg_index`), shared with the
    /// Phase 0 check and the local-threading producer.
    fn producer_body(expr: &Expression) -> Option<(String, &Block)> {
        beamtalk_core::semantic_analysis::block_facts::producer_body_block(expr)
    }

    /// Generates a non-last counted loop (`to:do:`/`to:by:do:`/`timesRepeat:`)
    /// with value-type captured local threading as an **open let chain**, extracting the
    /// threaded locals from the loop's `{'nil', StateAcc}` result (see
    /// [`Self::emit_vt_loop_open_extraction`]).
    pub(in crate::core_erlang) fn generate_vt_counted_loop_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // see the analogous comment in `generate_vt_while_open`.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the counted loop expression (returns {'nil', StateAcc} tuple).
        let loop_doc = self.expression_doc(expr)?;
        let threaded_locals = Self::counted_loop_body_block(expr)
            .map(|body| self.loop_threaded_locals(body, None))
            .unwrap_or_default();
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "CountedLoopResult",
            "CountedLoopState",
        ))
    }

    /// Returns `true` if `expr` is a `collect:` / `select:` / `reject:` /
    /// `inject:into:` / `count:` / `detect:` / `detect:ifNone:` foldl list-op whose body
    /// block captures and mutates outer local variables in value-type or class-method context.
    ///
    /// Mirrors `is_counted_loop_with_vt_local_threading`: only triggers when the list-op
    /// codegen will actually pack threaded locals into the `{value, StateAcc}` result
    /// (i.e. the mutation-threading path is taken), so detection and the extraction stay
    /// consistent.
    pub(in crate::core_erlang) fn is_foldl_list_op_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if Self::foldl_list_op_body_block(expr).is_none() {
            return false;
        }
        // BT-3827: a fold whose only mutation is a field write still returns a
        // threaded tuple (the `Self` slot), like a counted loop does.
        !self.producer_threaded_locals(expr).is_empty() || self.fold_threads_value_self(expr)
    }

    /// Returns the body block of a foldl list-op message send
    /// (`collect:`, `select:`, `reject:`, `inject:into:`, `count:`, `detect:`,
    /// `detect:ifNone:`), or `None` if `expr` is not one.
    ///
    /// The block argument position differs by selector: `collect:`/`select:`/`reject:`/
    /// `count:`/`detect:`/`detect:ifNone:` take the predicate/transform block as their first
    /// argument; `inject:into:` takes it as the second. All of these emit a `{value, StateAcc}`
    /// result (state in element 2) when their body threads captured outer locals, so the
    /// same open-let extraction applies uniformly.
    fn foldl_list_op_body_block(expr: &Expression) -> Option<&Block> {
        let (selector, body) = Self::producer_body(expr)?;
        (!matches!(selector.as_str(), "whileTrue:" | "whileFalse:")
            && !beamtalk_core::state_threading_selectors::is_counted_loop_selector(&selector))
        .then_some(body)
    }

    /// Generates a non-last foldl list-op (`collect:`/`select:`/`reject:`/
    /// `inject:into:`) with value-type captured local threading as an **open let chain**,
    /// extracting the threaded locals from the list-op's `{value, StateAcc}` result (see
    /// [`Self::emit_vt_loop_open_extraction`]).
    ///
    /// In non-last position the list-op's logical value (element 1) is discarded; only the
    /// threaded-local mutations need to escape.
    pub(in crate::core_erlang) fn generate_vt_foldl_list_op_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // BT-3827: the `Self` family a field-writing fold carries at element 3.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the list-op expression (returns a {value, StateAcc} tuple).
        let loop_doc = self.with_fold_self_slot_consumer(expr, |this| this.expression_doc(expr))?;
        let threaded_locals = Self::foldl_list_op_body_block(expr)
            .map(|_| self.producer_threaded_locals(expr))
            .unwrap_or_default();
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "FoldlListOpResult",
            "FoldlListOpState",
        ))
    }

    /// Returns the threaded local variable names for a `whileTrue:` / `whileFalse:`
    /// expression, or an empty Vec if none.
    fn get_while_threaded_locals(&self, expr: &Expression) -> Vec<String> {
        let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        else {
            return Vec::new();
        };
        let Some(Expression::Block(body)) = arguments.first() else {
            return Vec::new();
        };
        let condition = if let Expression::Block(_) = receiver.as_ref() {
            Some(receiver.as_ref())
        } else {
            None
        };
        self.loop_threaded_locals(body, condition)
    }

    /// Generates a non-last `do:` loop (with value-type/class-method
    /// captured local threading) as an **open let chain**.
    ///
    /// Unlike the closed form (which buries the extracted locals inside `_seqN = (... 'nil')`)
    /// this form emits the foldl and the extractions (plain lets / `element/2` for the
    /// tuple/bare accumulator, `maps:get` for the map accumulator) directly in
    /// the method body, ending with `in ` so the caller's next `body_part` provides the
    /// continuation.  This makes the updated locals visible to all subsequent expressions.
    #[allow(clippy::too_many_lines)] // Document-based foldl scaffolding spans many lines
    pub(in crate::core_erlang) fn generate_value_type_do_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let (receiver, block_expr, body) = match expr {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } if matches!(arguments.first(), Some(Expression::Block(_))) => {
                let block_arg = arguments.first().expect("checked above");
                let Some(Expression::Block(b)) = arguments.first() else {
                    return Ok(Document::Nil);
                };
                (receiver.as_ref(), block_arg, b)
            }
            _ => return Ok(Document::Nil),
        };

        // Validate that the block has exactly 1 parameter,
        // matching the check in the normal generate_list_do path.
        validate_block_arity_exact(
            block_expr,
            1,
            "do:",
            "The do: block must take exactly one argument: [:each | ...]",
        )?;

        // The fold's real plan (BT-4116: `new_for_foldl_list_op`, so it can pick
        // the tuple/bare accumulator). It drives either the tuple/bare open
        // form (`generate_value_type_do_open_tuple`) or the map open form
        // below; both emit the loop-convention diagnostic from it.
        let fold_plan = self.with_fold_self_slot_consumer(expr, |this| {
            ThreadingPlan::new_for_foldl_list_op(this, body, ListOpKind::Do)
                .with_value_self_slot(this, body)
        })?;
        let threaded_locals = fold_plan.threaded_locals.clone();

        // BT-4116: a body that threads only outer locals folds a bare value (one
        // local) or an N-tuple (several) — no map, no per-element `maps:get`/`put`.
        if fold_plan.use_tuple_acc {
            return self.generate_value_type_do_open_tuple(receiver, body, &fold_plan);
        }

        // Phase 1: create a fresh map and pack each captured local into it.
        let init_map_var = self.fresh_temp_var("InitMap");
        let mut pack_docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(init_map_var.clone()),
            " = call 'maps':'new'() in ",
        ]];
        let mut current = init_map_var;
        for var_name in &threaded_locals {
            let packed_var = self.fresh_temp_var("Packed");
            let core_var = self
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
            let key = Self::local_state_key(var_name);
            pack_docs.push(docvec![
                "let ",
                leaf::var(packed_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(core_var),
                ", ",
                leaf::var(current),
                ") in ",
            ]);
            current = packed_var;
        }
        let init_state_code = current;

        // Phase 2: generate receiver + list guard + lambda.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        // BT-3827: a body that writes a field carries `Self` in a `{Acc, Self}`
        // wrapper around the accumulator (`generate_list_do_with_mutations`).
        let slots = fold_plan.fold_family_slots(self);

        let mut docs: Vec<Document<'static>> = vec![
            concat(pack_docs),
            docvec![
                super::super::control_flow::list_recv_to_safe_list_doc(
                    recv_code,
                    None,
                    list_var,
                    safe_list_var.clone(),
                ),
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(
                    slots
                        .as_ref()
                        .map_or("StateAcc", family_slots::FoldFamilySlots::lambda_param)
                        .to_string()
                ),
                ") -> ",
                slots
                    .as_ref()
                    .map_or(Document::Nil, |s| s.lambda_prologue("StateAcc")),
            ],
        ];

        // Phase 3: generate foldl lambda body — ADR 0111 Addendum 15 (Foldl
        // migration): inlined replacement for the deleted
        // `generate_list_do_body_with_threading` compat shim, which built
        // its own second, throwaway `ThreadingPlan::new(self, body, None)`
        // (documented above as pure and computed identically to `fold_plan`)
        // purely to pass to `emit_loop_convention_diagnostic` — reusing
        // `fold_plan` here instead avoids that duplication while keeping the
        // diagnostic emitted exactly once, at this same relative point.
        self.emit_loop_convention_diagnostic(&fold_plan, body.span);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, &fold_plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
        self.pop_scope();
        docs.push(body_doc);

        // Phase 4: call foldl, then extract each local as an open `let X = ... in `.
        // These `let` bindings are emitted into the method body scope, not inside an
        // expression, so they shadow the pre-loop bindings and are visible to all
        // subsequent `body_parts`.
        let fold_result = self.fresh_temp_var("FoldResult");
        let init_acc = match &slots {
            Some(s) => s.wrap_init(leaf::var(init_state_code), self),
            None => leaf::var(init_state_code),
        };
        let mut post_docs: Vec<Document<'static>> = vec![ThreadingPlan::foldl_call_doc(
            &lambda_var,
            init_acc,
            &safe_list_var,
            slots.as_ref().map_or(
                fold_result.as_str(),
                family_slots::FoldFamilySlots::result_var,
            ),
        )];
        if let Some(s) = &slots {
            post_docs.push(s.unwrap_result(&fold_result));
        }
        for var_name in &threaded_locals {
            let core_var = Self::to_core_erlang_var(var_name);
            // Update the scope so subsequent method-body expressions look up
            // this Erlang variable name.
            self.bind_var(var_name, &core_var);
            let key = Self::local_state_key(var_name);
            post_docs.push(docvec![
                "let ",
                leaf::var(core_var),
                " = call 'maps':'get'(",
                leaf::atom(key),
                ", ",
                leaf::var(fold_result.clone()),
                ") in ",
            ]);
        }
        // BT-3827: the open chain's `Self` is the fold's final one.
        if let Some(s) = &slots {
            for out in s.out_vars() {
                post_docs.push(self.rebind_value_self_from_doc(leaf::var(out.clone()), body.span));
                post_docs.push(Document::Str(" "));
            }
        }
        // Intentionally open — the caller's next body_part provides the continuation.
        docs.push(concat(post_docs));

        Ok(Document::Vec(docs))
    }

    /// BT-4116: the tuple/bare-accumulator form of
    /// [`Self::generate_value_type_do_open`]. The fold seed is the threaded
    /// locals' current values (the single value for a bare accumulator), the
    /// lambda unpacks them per element, and the post-fold chain rebinds each
    /// local as an open `let` for the caller's next body part.
    fn generate_value_type_do_open_tuple(
        &mut self,
        receiver: &Expression,
        body: &beamtalk_core::ast::Block,
        fold_plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        let init_acc = fold_plan.initial_vars_tuple_doc(self);
        let acc_var = self.fresh_temp_var("AccSt");
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let mut docs: Vec<Document<'static>> = vec![docvec![
            super::super::control_flow::list_recv_to_safe_list_doc(
                recv_code,
                None,
                list_var,
                safe_list_var.clone(),
            ),
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_var.clone()),
            ") -> ",
        ]];

        self.emit_loop_convention_diagnostic(fold_plan, body.span);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, fold_plan, &BodyKind::FoldlDo, &acc_var, 0)?;
        self.pop_scope();
        docs.push(body_doc);

        let fold_result = self.fresh_temp_var("FoldResult");
        docs.push(ThreadingPlan::foldl_call_doc(
            &lambda_var,
            init_acc,
            &safe_list_var,
            &fold_result,
        ));
        // The extraction rebinds each local under its current Core name (the
        // suffix reads it via `lookup_var`), shadowing the pre-loop binding for
        // every later body part.
        docs.push(fold_plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self));
        // Intentionally open — the caller's next body_part provides the continuation.
        Ok(Document::Vec(docs))
    }

    /// Returns `true` if `expr` is an `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:`
    /// message send where at least one block argument writes to a local variable that is
    /// already bound in the enclosing scope, in value-type or class-method context.
    ///
    /// Unlike `captured_mutations_for_block` (which requires `captured_reads ∩ local_writes`),
    /// this also catches pure assignments like `[x := 2]` where `x` is only written, not
    /// read, inside the block.
    pub(in crate::core_erlang) fn is_conditional_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.threads_no_actor_state() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            match sel.as_str() {
                "ifTrue:" | "ifFalse:" => {
                    if let Some(Expression::Block(block)) = arguments.first() {
                        return !self.outer_scope_mutations_in_block(block).is_empty();
                    }
                }
                "ifTrue:ifFalse:" => {
                    let has_mutations = arguments.iter().any(|a| {
                        if let Expression::Block(b) = a {
                            !self.outer_scope_mutations_in_block(b).is_empty()
                        } else {
                            false
                        }
                    });
                    return has_mutations;
                }
                _ => {}
            }
        }
        false
    }

    /// `true` if `expr` is a value-type instance-method
    /// `self.field := ...` write — the shape that threads `Self{N}`
    /// (`FieldWriteSite::ValueType`).
    ///
    /// Deliberately excludes class methods: inside one, `self.x :=` is a
    /// CLASS-var write, an in-place `put` into the class process (ADR 0130),
    /// so it must never be routed through this issue's `Self` slot.
    pub(super) fn is_vt_self_field_assignment(&self, expr: &Expression) -> bool {
        self.is_family_mutation(&super::super::threaded_ir::VersionPrefix::SelfVt, expr)
    }

    /// `true` if `expr` is an `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
    /// whose branch block(s) contain a value-type `self.field := ...` write.
    ///
    /// Deliberately SEPARATE from
    /// [`Self::is_conditional_with_vt_local_threading`] rather than folded
    /// into it: that predicate also routes the LAST-position
    /// (`emit_vt_conditional_case_to_var`) path (and, before ADR 0131 Phase 3,
    /// the assignment-RHS path, now a conditional producer), which does not
    /// thread `Self` — a field write reaching it would be silently dropped
    /// (last position) or, as an assign RHS, emit an `erlc`-rejected reference
    /// to a branch-scoped `Self{N}`, both strictly worse than a clean
    /// `FieldAssignmentInUnsupportedBlock` diagnostic.
    ///
    /// BT-3821: the shapes that diagnostic used to cover — the last position, an
    /// assignment right-hand side, an operand — are now lowered by
    /// [`Self::vt_conditional_value_producer`] (gated by
    /// [`Self::is_vt_field_conditional_producer`], which reads THIS predicate),
    /// which carries the arms' value and their `Self` merge into the caller's
    /// statement sequence. What still reaches `generate_vt_conditional_open`
    /// through `VtBodyExprKind::ConditionalWithSelfFieldThreading`, and keeps
    /// the clean rejection, is a conditional with a non-literal branch argument.
    /// A conditional in a loop / branch / exception frame never gets that far: those
    /// shapes are rejected earlier, so the statement form is method-root only.
    ///
    /// Only reports conditionals with NO outer-local mutation of their own:
    /// a mixed one (locals AND a field write) is already
    /// [`Self::is_conditional_with_vt_local_threading`], classified ahead of
    /// this in [`Self::classify_vt_body_expr`], and routed to the same
    /// `generate_vt_conditional_open` for the non-last case.
    ///
    /// ADR 0122 / BT-3513: reads [`ThreadedFamilies`] via
    /// [`CoreErlangGenerator::exception_construct_families`] — the same
    /// general-purpose "does any of these blocks mutate an eligible family"
    /// detector `on:do:`/`ensure:` already shares with
    /// `Self::exception_construct_threaded_families` — rather than this
    /// predicate's own the former top-level-only write predicate walk. The
    /// `!self.in_class_method()` guard above fixes
    /// [`Self::eligible_families`] at `[SelfVt]` for every input this
    /// function can still reach (a class method threads no family), so that
    /// detector can only ever answer `[]` or `[SelfVt]` here.
    ///
    /// **BT-3522:** that shared detector is now ADR 0122's RECURSIVE
    /// `body_threaded_families`, so this predicate also reports a write
    /// nested BELOW a branch block's own top level — which
    /// [`Self::generate_vt_conditional_open`] cannot carry. It rejects that
    /// shape there (`reject_nested_vt_conditional_self_field_writes`) rather
    /// than narrowing the detector back: before BT-3522 the same shape was
    /// classified `Pure` and sequenced away, which dropped the mutation just
    /// as silently.
    pub(super) fn is_conditional_with_vt_self_field_threading(&self, expr: &Expression) -> bool {
        if self.in_class_method() || !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if !matches!(sel.as_str(), "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:") {
            return false;
        }
        let blocks: Vec<&Block> = arguments
            .iter()
            .filter_map(|arg| match arg {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        !self
            .exception_construct_families(&blocks)
            .as_slice()
            .is_empty()
    }
}
