// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor method body code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples for actor (`gen_server`) methods.

use super::super::method_frame::{MethodBoundary, MethodFrame};
use super::super::sequencing::PrecompiledScope;
use super::super::{CodeGenError, CoreErlangGenerator, Result, threaded_ir};
use crate::core_erlang::threading_analysis::ThreadedLocals;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, INDENT, join, leaf, line, nest};
use beamtalk_core::ast::{
    Block, CascadeMessage, ClassDefinition, Expression, MessageSelector, MethodDefinition,
    MethodKind,
};
use beamtalk_core::source_analysis::Span;
use ecow::EcoString;

/// Extracts the package name from a BEAM module name following the
/// `bt@{package}@{class}` convention (ADR 0016/0070).
///
/// Returns `None` for module names that don't follow this convention
/// (e.g., stdlib modules like `beamtalk_integer` or REPL workspace modules).
///
/// # Examples
/// - `"bt@my_counter@counter"` → `Some("my_counter")`
/// - `"bt@stdlib@integer"` → `Some("stdlib")`
/// - `"beamtalk_integer"` → `None`
pub(crate) fn extract_package_from_module_name(module_name: &str) -> Option<String> {
    let parts: Vec<&str> = module_name.splitn(3, '@').collect();
    if parts.len() >= 3 && parts[0] == "bt" {
        Some(parts[1].to_string())
    } else {
        None
    }
}

/// Classification of how a method body expression should be handled for
/// state threading.  Produced by [`CoreErlangGenerator::classify_body_expr`]
/// and consumed by the unified [`CoreErlangGenerator::lower_body_exprs_with_reply`]
/// and [`CoreErlangGenerator::generate_conditional_branch_inline`].
pub(in crate::core_erlang) enum BodyExprKind {
    /// `^ value` — early return from method.
    EarlyReturn,
    /// `self fieldAt: name put: val` — reflective field mutation.
    SelfFieldAtPut,
    /// `self clearField: name` — reflective field un-assign (ADR 0124 §1/B4).
    SelfClearField,
    /// `self.field := value` — direct field assignment.
    FieldAssignment,
    /// `self.field := expr` where the RHS is control flow with mutations.
    FieldAssignmentControlFlow,
    /// `self fieldAt: name put: expr` where the RHS is control flow with field mutations.
    SelfFieldAtPutControlFlow,
    /// `{a, b} := expr` where the RHS is control flow with field mutations.
    DestructureAssignmentControlFlow,
    /// `var := expr` where the RHS is a Tier 2 `value:` call.
    LocalAssignTier2,
    /// `var := [block]` where the block literal itself needs Tier 2 (captured-local
    /// or field mutations). Unlike `LocalAssignTier2` (RHS *invokes* a
    /// Tier 2 block), here the RHS *is* the block literal being stored for later
    /// invocation (e.g. `blk := [:x | self.total := self.total + x]`).
    LocalAssignTier2Block,
    /// `var := expr` where the RHS is control flow with field mutations.
    LocalAssignControlFlow,
    /// `var := self method` — local assignment where RHS is a dispatching self-send.
    LocalAssignSelfSend,
    /// `var := expr` — simple local assignment.
    LocalAssignPure,
    /// `{a, b} := expr` — destructure assignment.
    DestructureAssignment,
    /// `super method` — super message send.
    SuperSend,
    /// `self error: "..."` in an instance method — never returns (BT-4075:
    /// only for a receiver known to be an instance).
    ErrorSend,
    /// Tier 2 `value:` call — returns `{Result, NewState}`.
    Tier2ValueCall,
    /// Control flow with field mutations — returns `{Result, State}`.
    ControlFlowWithMutations,
    /// `self userMethod` — dispatching self-send via `safe_dispatch`.
    DispatchingSelfSend,
    /// Regular expression with no special state-threading needs.
    Pure,
}

impl CoreErlangGenerator {
    /// Generates dispatch case clauses for all methods in a class definition.
    pub(in crate::core_erlang) fn generate_class_method_dispatches(
        &mut self,
        class: &ClassDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        let mut docs = Vec::new();
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            if method.kind == MethodKind::Primary {
                docs.push(self.generate_method_dispatch(method, indent_level)?);
            }
        }
        Ok(Document::Vec(docs))
    }

    /// Generates a single method dispatch case clause.
    pub(in crate::core_erlang) fn generate_method_dispatch(
        &mut self,
        method: &MethodDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        let selector_name = method.selector.name();
        let (mut frame, param_vars) = MethodFrame::enter(self, method, MethodBoundary::Actor);

        // Reset the same-method local-var tracking. The real (re-)population
        // happens inside lower_body_exprs_with_reply via
        // prescan_tier2_local_vars — this clear here is belt-and-suspenders
        // for the case where a caller inspects the field between the clear
        // and the body being generated.
        frame.tier2_local_vars.clear();
        frame.tier2_local_var_captured_mutations.clear();
        frame.tier2_local_var_may_write_state.clear();

        // Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing actor method.
        let needs_nlr = frame
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);

        // `frame`'s `Drop` pops the scope and restores the selector on both
        // the success and the `?` error path here, reproducing the cleanup
        // the old hand-rolled `match { Err(e) => { pop_scope(); ...; } }`
        // ran explicitly.
        let method_body_doc = frame.generate_actor_method_body_with_nlr(method, needs_nlr)?;

        // Build method clause as Document tree
        let has_params = !param_vars.is_empty();
        let body_doc: Document = if has_params {
            docvec![
                "<",
                leaf::atom(selector_name.to_string()),
                "> when 'true' ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "case Args of",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<[",
                                join(
                                    param_vars.iter().map(|p| leaf::var(p.clone())),
                                    &Document::Str(", ")
                                ),
                                "]> when 'true' ->",
                                nest(INDENT, docvec![line(), method_body_doc,]),
                                line(),
                                "<_> when 'true' -> {'reply', {'error', 'bad_arity'}, State}",
                            ]
                        ),
                        line(),
                        "end",
                    ]
                ),
                "\n",
            ]
        } else {
            docvec![
                "<",
                leaf::atom(selector_name.to_string()),
                "> when 'true' ->",
                nest(INDENT, docvec![line(), method_body_doc,]),
                "\n",
            ]
        };

        // Render at correct indent level
        let indent_spaces = indent_level * INDENT;
        #[allow(clippy::cast_sign_loss)] // indent_spaces is always non-negative
        let indent_width = indent_spaces as usize;
        let result_doc = docvec![
            leaf::whitespace(indent_width),
            nest(indent_spaces, body_doc)
        ];

        // `frame` drops here, popping the scope and restoring the selector.
        Ok(result_doc)
    }

    /// (ADR 0111 Addendum 4 task 2): lowering-only counterpart of the
    /// old `generate_method_definition_body_with_reply` (deleted once
    /// its last caller migrated off it), returning the raw `Vec<ThreadedStmt>`
    /// instead of rendering it. Used by [`Self::generate_method_dispatch`]
    /// and the other Actor-boundary NLR call sites (sealed
    /// methods, actor extension funs), all of which need the IR itself so
    /// they can prepend a real `NlrCatch` stmt (token already minted, in
    /// production's real mint position) before the single `verify()` +
    /// `render()` pass — rather than rendering the body first and wrapping
    /// the resulting `Document` afterward.
    pub(in crate::core_erlang) fn lower_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        // Scope the cell-store mark to this one method body: a mark left over
        // from an earlier (possibly failed) lowering must not leak into this
        // body, and `with_exit_store` takes it when this body is done.
        threaded_ir::take_cell_store_emitted();
        let body = super::super::util::collect_body_exprs(&method.body);
        self.lower_body_exprs_with_reply(&body, true)
    }

    /// (ADR 0111 Addendum 4 task 2): mints the NLR token (if
    /// `needs_nlr`), lowers the method body, prepends a real `NlrCatch` stmt
    /// carrying that token, and verifies + renders the whole sequence —
    /// the token is minted BEFORE lowering, matching production's real mint
    /// order (§Gap 3). Callers still own `set_current_nlr_token(None)` /
    /// scope cleanup on error via the `?` this returns through.
    fn generate_actor_method_body_with_nlr(
        &mut self,
        method: &MethodDefinition,
        needs_nlr: bool,
    ) -> Result<Document<'static>> {
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let span = method
            .body
            .first()
            .map_or_else(|| method.span, |s| s.expression.span());
        // The mark is scoped to one method body by the lowering wrappers
        // (`with_exit_store` takes it when the body is done).
        let lowered = self.lower_method_definition_body_with_reply(method);
        self.set_current_nlr_token(None);
        let stmts = lowered?;

        // Case-arm context (dispatch clause): always needs the letrec frame
        // when NLR is present — see `prepend_nlr_catch_and_render`'s doc.
        Ok(self.prepend_nlr_catch_and_render(stmts, nlr_token_var.as_deref(), span, true))
    }

    /// (ADR 0111 Addendum 4/6): shared tail step for every
    /// Actor-boundary NLR call site — prepends a real `ThreadedStmt::NlrCatch`
    /// (when `nlr_token_var` is `Some`; the token is already minted, in
    /// production's real mint position, by the caller) to an already-lowered
    /// method body, then verifies and renders the whole sequence in one pass.
    /// Replaces the old two-step "render body, then
    /// `wrap_actor_body_with_nlr_catch` the rendered `Document`" shape.
    ///
    /// `needs_letrec` mirrors `wrap_actor_body_with_nlr_catch`'s own
    /// parameter of the same name: `true` when the try/catch would otherwise
    /// nest inside a `case` arm (dispatch clauses — BEAM validator
    /// `ambiguous_catch_try_state`), `false` for standalone functions (sealed
    /// methods, extension funs) that don't need the extra function frame. No
    /// letrec is emitted when `nlr_token_var` is `None` regardless of
    /// `needs_letrec` — there is no try/catch to isolate.
    pub(in crate::core_erlang) fn prepend_nlr_catch_and_render(
        &mut self,
        mut stmts: Vec<threaded_ir::ThreadedStmt>,
        nlr_token_var: Option<&str>,
        span: Span,
        needs_letrec: bool,
    ) -> Document<'static> {
        if let Some(token_var) = nlr_token_var {
            stmts.insert(
                0,
                threaded_ir::ThreadedStmt::NlrCatch {
                    boundary: super::super::NlrBoundary::ActorReply,
                    token: threaded_ir::TokenId::new(token_var.to_string()),
                    frame: threaded_ir::FrameId::ROOT,
                    span,
                },
            );
        }
        let rendered_body = self.verify_and_render_body_stmts(&stmts, span);

        // If NLR was detected, wrap the rendered try/catch in a
        // letrec function — letrec creates a genuine separate function frame,
        // avoiding BEAM validator ambiguous_catch_try_state errors that arise
        // when try/catch is nested inside case arms. `render`'s `NlrCatch` arm
        // already produced the try/catch itself; the letrec is a Document-level
        // wrapper around that, unchanged from `wrap_actor_body_with_nlr_catch`'s
        // own `needs_letrec` shape.
        let body = if needs_letrec && nlr_token_var.is_some() {
            docvec![
                "letrec '__nlr_body'/0 = fun () ->\n",
                rendered_body,
                "in apply '__nlr_body'/0 ()",
            ]
        } else {
            rendered_body
        };
        self.with_exit_store(body)
    }

    /// (ADR 0111 Addendum 4/6): lowering-only counterpart of the old
    /// `generate_method_body_with_reply` (deleted once its last
    /// caller migrated off it), returning the raw `Vec<ThreadedStmt>` instead
    /// of rendering it — the Block-based sibling of
    /// [`Self::lower_method_definition_body_with_reply`]. Used by
    /// `generate_legacy_method_clause` (top-level `name := [block]`
    /// workspace bindings), which needs the IR itself so it can prepend a
    /// real `NlrCatch` stmt before the single verify+render pass.
    pub(in crate::core_erlang) fn lower_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        // Same per-body scoping as `lower_method_definition_body_with_reply`:
        // legacy clauses lower through here, not through that wrapper.
        threaded_ir::take_cell_store_emitted();
        let body = super::super::util::collect_body_exprs(&block.body);
        self.lower_body_exprs_with_reply(&body, false)
    }

    // ── Unified method body state-threading ──────────────────

    /// Pre-scans a method/block body for `var := [block]` assignments
    /// where the block itself needs Tier 2 (captured-local or field mutations),
    /// and populates `self.tier2_local_vars` with the ones that are *provably
    /// safe* to promote — i.e. every later reference to `var` in this same body
    /// is the receiver of a `value`/`value:`/`value:value:`/`value:value:value:`
    /// send, never a bare read (return, argument, reassignment, ...).
    ///
    /// Only considers `var := [block]` assignments that are themselves *flat
    /// top-level statements* of `body` — one that's nested inside e.g. an
    /// `ifTrue:`/`do:` block argument isn't a candidate here. Such a nested
    /// assignment still falls through to the existing
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic,
    /// which is conservative but correct.
    ///
    /// **Safety invariant**: `scan_var_uses` marks *any* reference to `var`
    /// found inside a nested `Block` literal as unsafe, even a `value:` send
    /// that would otherwise qualify as safe. A nested block literal compiles
    /// through a completely separate path
    /// (`generate_block_body_slice`/`BlockExprKind` in `expressions.rs`, not
    /// `lower_body_exprs_with_reply`/`BodyExprKind` here) that has no
    /// Tier2-tuple-unpacking logic and never resets `tier2_local_vars` for
    /// its own body — so a "safe-looking" `value:` call on a promoted var
    /// found inside a nested block either leaks an unpacked
    /// `{Result, NewState}` tuple as the inner block's return value (a Tier 1
    /// inner block, which never resets `tier2_local_vars`) or calls a
    /// 2-arity Tier 2 fun with only 1 argument (a Tier 2 inner block, which
    /// resets `tier2_local_vars` for its own body and never re-adds `var`
    /// since it isn't assigned there) — `badarity` at runtime either way.
    /// Only a direct top-level method-body `var value:` statement is
    /// provably safe.
    ///
    /// This runs as a full pre-scan rather than incrementally during codegen,
    /// specifically so that a block which *escapes* this method unsafely
    /// (returned, stored elsewhere, passed as an argument) is never promoted —
    /// it must keep hitting the `generate_block`/`validate_stored_closure`
    /// compile-time diagnostic, since no known call site would thread state
    /// through it correctly.
    fn prescan_tier2_local_vars(&mut self, body: &[&Expression]) {
        for (i, expr) in body.iter().enumerate() {
            let Expression::Assignment { target, value, .. } = expr else {
                continue;
            };
            let (Expression::Identifier(id), Expression::Block(block)) =
                (target.as_ref(), value.as_ref())
            else {
                continue;
            };
            // One analysis pass feeds both facts below (mutations and the
            // Tier 2 merge predicate) instead of re-walking the block.
            let analysis = super::super::block_analysis::analyze_block(block);
            let captured_mutations = Self::captured_mutations_from_analysis(&analysis);
            // ADR 0137: only a block that mutates a captured local is Tier 2. A
            // block that writes a field is an ordinary closure over the actor's
            // state (`actor_state_access.rs`); one that does both stays Tier 2.
            // BT-4122: such a block's `StateAcc` carries its locals, while its
            // fields go through the cell (the Tier 2 merge) — recorded in
            // `tier2_local_var_may_write_state` when promoted below.
            let needs_tier2 = !captured_mutations.is_empty();
            if !needs_tier2 {
                continue;
            }
            let var_name = id.name.as_str();
            let (has_unsafe, has_safe) = body[i + 1..]
                .iter()
                .map(|stmt| Self::scan_var_uses(stmt, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            // Require at least one safe use, not just the absence of unsafe
            // ones — otherwise a variable that's never referenced again (e.g.
            // `var := [block]` as the method's last statement, so the block
            // value implicitly escapes as the return value) would vacuously
            // pass "no unsafe use found" and be wrongly promoted.
            if has_safe && !has_unsafe {
                self.tier2_local_vars.insert(var_name.to_string());
                // Record which outer locals this var's block captures
                // and mutates, so a later `value(:...)` call site — which only
                // has the variable name, not the block AST — can still rebind
                // them after the call (mirroring the inline-block-literal case).
                if !captured_mutations.is_empty() {
                    self.tier2_local_var_captured_mutations
                        .insert(var_name.to_string(), captured_mutations);
                }
                // BT-4122: and whether the block may write the actor's
                // fields, so the Tier 2 merge knows which calls bracket.
                if analysis.may_write_actor_state() {
                    self.tier2_local_var_may_write_state
                        .insert(var_name.to_string());
                }
            }
        }
    }

    /// See [`beamtalk_core::semantic_analysis::block_facts::normalize_cascade`].
    pub(in crate::core_erlang) fn normalize_cascade<'a>(
        receiver: &'a Expression,
        messages: &'a [CascadeMessage],
    ) -> (&'a Expression, Vec<(&'a MessageSelector, &'a [Expression])>) {
        beamtalk_core::semantic_analysis::block_facts::normalize_cascade(receiver, messages)
    }

    /// See [`beamtalk_core::semantic_analysis::block_facts::stored_block_var_uses`],
    /// shared with the ADR 0131 §6 check.
    fn scan_var_uses(expr: &Expression, var_name: &str) -> (bool, bool) {
        beamtalk_core::semantic_analysis::block_facts::stored_block_var_uses(expr, var_name)
    }

    /// BT-4310: the one lowering of the arguments of a send whose statement
    /// lowering compiles them itself: a `self error:`, `self fieldAt:put:` or
    /// `self clearField:` statement, at the method root (`frame` is `ROOT`)
    /// or in an arm frame. (A `super` send's arguments are sequenced by
    /// `super_send_producer`, BT-4318.)
    /// Their state-effecting sub-expressions thread ahead of the send into
    /// `stmts` (the sequencing rule), so the send reads the post-operand
    /// `State` and an operand's rebind is not scoped away inside its closed
    /// argument list; a loop or fold threading only `State` there
    /// (`family_only_operand_site_exprs`) is a producer whose carrier is
    /// unpacked ahead of the send. With no operand that needs a prelude this
    /// compiles nothing. The returned scope goes back to
    /// [`Self::finish_precompiled_scope`] once the send itself is compiled.
    pub(in crate::core_erlang) fn thread_send_arguments(
        &mut self,
        send: &Expression,
        frame: threaded_ir::FrameId,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
    ) -> Result<PrecompiledScope> {
        let operands: Vec<&Expression> = if let Expression::MessageSend { arguments, .. } = send {
            arguments.iter().collect()
        } else {
            Vec::new()
        };
        let (operand_prelude, scope) = self.sequence_children(&operands, frame)?;
        stmts.extend(operand_prelude);
        Ok(scope)
    }

    /// Classify a body expression for state-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment)
    /// must come before general ones (e.g. pure expression).
    pub(in crate::core_erlang) fn classify_body_expr(&self, expr: &Expression) -> BodyExprKind {
        // Early return — `^ value`
        if matches!(expr, Expression::Return { .. }) {
            return BodyExprKind::EarlyReturn;
        }

        // self fieldAt: name put: val — sub-classify by RHS for control flow with mutations
        if self.is_self_field_at_put(expr) {
            if let Expression::MessageSend { arguments, .. } = expr {
                // BT-4310: a value holding a loop or fold that threads only
                // `State` at an operand is sequenced ahead of the put
                // (`thread_send_arguments`) by the `SelfFieldAtPut` lowering;
                // the control-flow one would read it as a raw tuple.
                if arguments.len() >= 2
                    && self.control_flow_has_mutations(&arguments[1])
                    && self.family_only_operand_site_exprs(expr).is_empty()
                {
                    return BodyExprKind::SelfFieldAtPutControlFlow;
                }
            }
            return BodyExprKind::SelfFieldAtPut;
        }

        // self clearField: name — a write (ADR 0124 §1/B4), but its one
        // argument is the field NAME, not a value expression that could
        // itself embed control flow with mutations (unlike `fieldAt:put:`'s
        // second argument), so there is no `...ControlFlow` sub-classification
        // needed here.
        if self.is_self_clear_field(expr) {
            return BodyExprKind::SelfClearField;
        }

        // self.field := value — sub-classify by RHS for control flow with mutations
        if self.is_field_assignment(expr) {
            if let Expression::Assignment { value, .. } = expr {
                if self.control_flow_has_mutations(value) {
                    return BodyExprKind::FieldAssignmentControlFlow;
                }
            }
            return BodyExprKind::FieldAssignment;
        }

        // var := expr — sub-classify by RHS
        if Self::is_local_var_assignment(expr) {
            if let Expression::Assignment { target, value, .. } = expr {
                // Var := [block] where the block itself needs Tier 2 —
                // stored for invocation later in this method (`blk value: x`),
                // not invoked here. Must be classified before is_tier2_value_call
                // (which detects the opposite shape: RHS *invoking* a Tier 2 block).
                //
                // Only takes this path if `tier2_local_vars` already proved (via
                // `prescan_tier2_local_vars`, run before classification starts) that
                // every later use of this variable in the method is a safe
                // `value`/`value:`/etc. call. Otherwise the block may escape (be
                // returned, passed elsewhere, reassigned) with no call site that
                // knows to thread state through it — fall through to the plain
                // `generate_block` path, which raises the compile-time
                // `FieldAssignmentInUnsupportedBlock` diagnostic for that case.
                if let (Expression::Identifier(id), Expression::Block(_)) =
                    (target.as_ref(), value.as_ref())
                {
                    if self.tier2_local_vars.contains(id.name.as_str()) {
                        return BodyExprKind::LocalAssignTier2Block;
                    }
                }
                if self.is_tier2_value_call(value) {
                    return BodyExprKind::LocalAssignTier2;
                }
                if self.control_flow_has_mutations(value) {
                    return BodyExprKind::LocalAssignControlFlow;
                }
                // Var := self method — self-send as assignment RHS
                if self.is_dispatching_actor_self_send(value) {
                    return BodyExprKind::LocalAssignSelfSend;
                }
            }
            return BodyExprKind::LocalAssignPure;
        }

        // {a, b} := expr — sub-classify by RHS for control flow with mutations
        if let Expression::DestructureAssignment { value, .. } = expr {
            if self.control_flow_has_mutations(value) {
                return BodyExprKind::DestructureAssignmentControlFlow;
            }
            return BodyExprKind::DestructureAssignment;
        }

        // super send
        if Self::is_super_message_send(expr) {
            return BodyExprKind::SuperSend;
        }

        // self error: "..." — never returns. Only when the receiver is known to
        // be an instance (BT-4075): any other receiver may be a class object
        // whose class-side `error:` answers a value, so it is a `Pure` send
        // that replies with its result.
        if Self::is_error_message_send(expr)
            && matches!(
                expr,
                Expression::MessageSend { receiver, .. }
                    if self.error_signal_receiver_kind(receiver)
                        == crate::core_erlang::intrinsics::ErrorSignalReceiver::Instance
            )
        {
            return BodyExprKind::ErrorSend;
        }

        // Tier 2 value: call
        if self.is_tier2_value_call(expr) {
            return BodyExprKind::Tier2ValueCall;
        }

        // Control flow with field mutations
        if self.control_flow_has_mutations(expr) {
            return BodyExprKind::ControlFlowWithMutations;
        }

        // Dispatching self-send
        if self.is_dispatching_actor_self_send(expr) {
            return BodyExprKind::DispatchingSelfSend;
        }

        BodyExprKind::Pure
    }

    /// Verifies a lowered method-body IR once (via
    /// [`threaded_ir::verify_body_with_opaque_version_gaps`] — see its doc
    /// comment for what the opaque-statement backfill does and does not
    /// check) and renders it through [`threaded_ir::render`], the same
    /// renderer every other emission-input `ThreadedIr` producer uses.
    pub(in crate::core_erlang) fn verify_and_render_body_stmts(
        &mut self,
        stmts: &[threaded_ir::ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let errors =
            threaded_ir::verify_body_with_opaque_version_gaps(stmts, self.threaded_scope());
        self.report_threaded_ir_verify_errors(
            &errors,
            "gen_server method-body ThreadedIr must be well-formed",
            span,
        );
        let mut ctx = threaded_ir::RenderCtx::new(self);
        threaded_ir::render(stmts, &mut ctx)
    }

    /// The shared two-hop `Bind` chain for a
    /// `self.field := <control-flow-with-mutations>` step (both the
    /// `BodyExprKind::FieldAssignmentControlFlow` arm and its `^`-return
    /// variant) — the investigation-confirmed idiom for a mutation
    /// whose map source is a computed temp rather than the prior `State`
    /// version:
    ///
    /// 1. `Bind { target: Gensym(CfState), source: State(n), op:
    ///    Direct(Doc(element(2, CfTuple))) }` — the RHS construct's returned
    ///    state, bound to its pre-minted `_CfState{N}` temp;
    /// 2. `Bind { target: State(n+1), source: Gensym(CfState), op: Put {
    ///    field, CfVal } }` — the real field mutation, a genuine
    ///    [`threaded_ir::BindOp::Put`] whose `maps:put` rendering is
    ///    `render_bind`'s.
    ///
    /// `prefix_doc` (the `CfTuple`/`CfVal` unpack) precedes the chain as an
    /// opaque `Statement`. Mint order is the caller's responsibility and
    /// matches the original emission exactly: `CfTuple`/`CfVal`/RHS doc/
    /// `CfState` are all minted before this is called; `next_state_var`
    /// advances here, after them.
    #[expect(
        clippy::too_many_arguments,
        reason = "two call sites share one mint-order-sensitive lowering step; a params struct would obscure the order the doc comment pins"
    )]
    fn lower_cf_field_assignment_binds(
        &mut self,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
        prefix_doc: Document<'static>,
        tuple_var: &str,
        rhs_state: &str,
        field_name: &str,
        val_var: &str,
        span: Span,
    ) {
        use threaded_ir::{BindOp, FrameId, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar};

        let source_version = self.state_version();
        stmts.push(ThreadedStmt::Statement(prefix_doc, span));
        let cf_state = VersionedVar::new(
            VersionPrefix::Gensym(rhs_state.to_string()),
            1,
            FrameId::ROOT,
        );
        stmts.push(ThreadedStmt::Bind {
            target: cf_state.clone(),
            source: VersionedVar::new(VersionPrefix::State, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(tuple_var.to_string()),
                ")",
            ])),
            span,
        });
        // BT-4369: every field write records itself for the loop checkout.
        self.note_field_write(field_name, span);
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: cf_state,
            op: BindOp::Put {
                field: field_name.to_string(),
                value: ValueRef::Var(val_var.to_string()),
            },
            span,
        });
    }

    /// Lowers a method body to one straight-line `Vec<ThreadedStmt>` (ADR
    /// 0111 Addendum 4 task 1): every `State`-version step this
    /// function itself emits is a real [`threaded_ir::ThreadedStmt::Bind`]
    /// (target/source versions read off the live counter); every ordinary
    /// AST-directed statement — dispatch, sends, Tier-2 calls, destructure
    /// bindings, reply epilogues — is an opaque
    /// [`threaded_ir::ThreadedStmt::Statement`] built by the SAME codegen
    /// call production ran before this migration (byte-identity: only the
    /// container changed, from `Vec<Document>` to `Vec<ThreadedStmt>`).
    /// Version steps hidden inside shared multi-module helpers
    /// (`generate_field_assignment_open`,
    /// `generate_self_field_at_put_open`) remain inside their `Statement`s —
    /// see `verify_body_with_opaque_version_gaps`'s backfill accounting.
    ///
    /// Classification happens exactly once per statement (`classify_body_expr`),
    /// interleaved with lowering: just before a statement is lowered, so the
    /// locals bound by the statements before it are live in scope (a catch
    /// boundary in a loop body is judged against that scope, ADR 0131 Phase 4,
    /// BT-3840). The mutating control-flow arms consume that decision via
    /// `emit_actor_threaded_last_stmts`/`emit_actor_threaded_assign_rhs_stmts`,
    /// which never decline — the earlier `verify_routing_invariant`
    /// call sites (and `VerifyError::RoutingMismatch`) are deleted because
    /// there is no second computation left to disagree with the first.
    ///
    /// `supports_early_return` controls whether `^ value` expressions are handled.
    /// Method definitions support it; block bodies do not (NLR uses throw/catch).
    fn lower_body_exprs_with_reply(
        &mut self,
        body: &[&Expression],
        supports_early_return: bool,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        // ADR 0131 Phase 2: a method body's sequenced operand sites are
        // where a loop or fold is a local-threading producer. A legacy
        // workspace block body (`supports_early_return: false`) has none.
        if supports_early_return {
            // BT-4310: the loops and folds threading only `State` at the
            // statements' other operands (`family_only_operand_site_exprs`),
            // read by the rule `with_local_producer_sites` registers them by,
            // are checked against the lowered body: each one's
            // `ConstructTuple` is a statement of the method body, never a
            // carrier compiled in place inside an operand
            // (`ConstructCarrierInPlace`), as every frame body checks its own.
            let sites = self.family_only_operand_sites_of(body);
            let stmts = self.with_local_producer_sites(body, |this| {
                this.lower_body_exprs_with_reply_in_sites(body, supports_early_return)
            })?;
            self.check_family_only_operand_sites(
                &stmts,
                &sites,
                "actor method root",
                body.first().map_or_else(Span::default, |e| e.span()),
            );
            Ok(stmts)
        } else {
            // BT-3910: it still has value-position local assignments to lower.
            self.with_value_assign_sites(body, |this| {
                this.lower_body_exprs_with_reply_in_sites(body, supports_early_return)
            })
        }
    }

    /// [`Self::lower_body_exprs_with_reply`] once the local-producer sites
    /// are set.
    #[expect(
        clippy::too_many_lines,
        reason = "unified handler for all method body expression types with state threading"
    )]
    fn lower_body_exprs_with_reply_in_sites(
        &mut self,
        body: &[&Expression],
        supports_early_return: bool,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        use threaded_ir::ThreadedStmt;

        if body.is_empty() {
            let state = self.current_state_var();
            return Ok(vec![ThreadedStmt::Statement(
                docvec!["{'reply', Self, ", leaf::var(state), "}"],
                Span::default(),
            )]);
        }

        // (Re-)populate tier2_local_vars for *this* body before
        // classification reads it. Cleared here (not just in
        // generate_method_dispatch) because generate_legacy_method_clause
        // (top-level `name := [block]` workspace methods) calls into this
        // function without clearing it first.
        self.tier2_local_vars.clear();
        self.tier2_local_var_captured_mutations.clear();
        self.tier2_local_var_may_write_state.clear();
        self.prescan_tier2_local_vars(body);

        // Classify and lower each expression to ThreadedStmts, in order.
        let mut stmts: Vec<ThreadedStmt> = Vec::with_capacity(body.len());
        let body_len = body.len();

        for (i, expr) in body.iter().enumerate() {
            // BT-3838: INVARIANT - classification and emission see the same node (the
            // value-type twin is BT-3830). `classify_body_expr` and most of its predicates
            // do not look through parentheses, so a parenthesised statement was classified
            // `Pure` while its bare form got a threading kind: `(t := t + 1)` dropped the
            // local write. A NON-last statement's value is discarded, so `(s)` is exactly
            // `s`: unwrap it once here, for every kind. The last statement is the method
            // value, so it is left as written.
            let expr = if i == body.len() - 1 {
                expr
            } else {
                &expr.unwrap_parens()
            };
            // Classified here, just before the statement is lowered, so the
            // locals bound by the statements before it are live in scope: a
            // loop's threaded set (`threaded_locals_of`) is decided against
            // that scope, and an upfront pass would not see it (ADR 0131
            // Phase 4, BT-3840). Classification reads no state a lowering of
            // an earlier statement changes other than that scope.
            let kind = {
                let kind = self.classify_body_expr(expr);
                if matches!(&kind, BodyExprKind::EarlyReturn) && !supports_early_return {
                    BodyExprKind::Pure
                } else {
                    kind
                }
            };
            let is_last = i == body_len - 1;
            let is_early_return = matches!(&kind, BodyExprKind::EarlyReturn);
            let span = expr.span();

            // Early return — always terminates generation regardless of position.
            // Classify the inner value to handle super/tier2/dispatch returns.
            if is_early_return && supports_early_return {
                if let Expression::Return { value, .. } = expr {
                    let mut value_kind = self.classify_body_expr(value);
                    // ADR 0131 Phase 2: a `^` value that is a local-threading
                    // producer, or holds one at an operand site, is lowered
                    // through `threaded_expression` (the generic arm below),
                    // which splices the producer's prelude; the dedicated arms
                    // compile their value with plain `expression_doc` and would
                    // leak the construct tuple.
                    // BT-3982: so is an evaluated literal block that is a
                    // producer (`^[:y | self.n := y] value: 5`).
                    if matches!(
                        value_kind,
                        BodyExprKind::ControlFlowWithMutations | BodyExprKind::Pure
                    ) && (self.statement_has_local_producer(expr)
                        || self.evaluated_block_is_producer(value))
                    {
                        value_kind = BodyExprKind::Pure;
                    }
                    match value_kind {
                        BodyExprKind::SuperSend => {
                            // BT-3819 / BT-4318: `^super take: (c ifTrue: [...])`
                            // is the `super` producer, like the statement form.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let reply = self.threaded_value_reply_doc(&tv.value);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::Tier2ValueCall => {
                            let expr_str = self.generate_tier2_value_call_doc(value)?;
                            let reply = self.emit_tuple_unpack_reply("T2Tuple", expr_str);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::DispatchingSelfSend => {
                            // ADR 0118 phase 1a: `^self log: (self nextId)` — the
                            // producer sequences its own arguments; its prelude
                            // is this body's real `Statement` + `Bind` pair.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let reply = self.threaded_value_reply_doc(&tv.value);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::ControlFlowWithMutations => {
                            let expr_str = self.expression_doc(value)?;
                            let reply = self.emit_tuple_unpack_reply("Tuple", expr_str);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        // ^ self.field := <control-flow-with-mutations>
                        BodyExprKind::FieldAssignmentControlFlow => {
                            if let Expression::Assignment {
                                target, value: rhs, ..
                            } = &**value
                            {
                                if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                    let tuple_var = self.fresh_temp_var("CfTuple");
                                    let val_var = self.fresh_temp_var("CfVal");
                                    let rhs_str = self.expression_doc(rhs)?;
                                    let rhs_state = self.fresh_temp_var("CfState");
                                    self.lower_cf_field_assignment_binds(
                                        &mut stmts,
                                        docvec![
                                            "let ",
                                            leaf::var(tuple_var.clone()),
                                            " = ",
                                            rhs_str,
                                            " in let ",
                                            leaf::var(val_var.clone()),
                                            " = call 'erlang':'element'(1, ",
                                            leaf::var(tuple_var.clone()),
                                            ") in ",
                                        ],
                                        &tuple_var,
                                        &rhs_state,
                                        field.name.as_str(),
                                        &val_var,
                                        span,
                                    );
                                    let field_state = self.current_state_var();
                                    stmts.push(ThreadedStmt::Statement(
                                        docvec![
                                            "{'reply', ",
                                            leaf::var(val_var),
                                            ", ",
                                            leaf::var(field_state),
                                            "}",
                                        ],
                                        span,
                                    ));
                                }
                            }
                        }
                        _ => {
                            // ADR 0118 phase 1a: `^ Array with: (self bump)`,
                            // `^ (items at: i) + (self bump)` — the value's
                            // state-effecting sub-expressions land in this
                            // body's IR as real `Bind`s (in source order, via
                            // the sequencing rule), then the reply carries the
                            // post-dispatch state.
                            //
                            // The reply's state is the one AFTER the prelude,
                            // not `current_state_var()`: the value's own compile
                            // may mint versions inside its closed document (a
                            // conditional receiver's dispatch chain) that are
                            // out of scope here — `state_var_after_prelude`.
                            let version_before = self.state_version();
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            let final_state =
                                self.state_var_after_prelude(&tv.prelude, version_before);
                            stmts.extend(tv.prelude);
                            let value_str = self.threaded_value_doc(&tv.value);
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let _ReturnValue = ",
                                    value_str,
                                    " in {'reply', _ReturnValue, ",
                                    leaf::var(final_state),
                                    "}",
                                ],
                                span,
                            ));
                        }
                    }
                    return Ok(stmts);
                }
            }

            match kind {
                // Mutation hidden inside a shared multi-module helper
                // (`generate_self_field_at_put_open` — also called from
                // conditionals.rs): stays an opaque Statement; the version
                // step it performs is accounted for by
                // `verify_body_with_opaque_version_gaps`'s backfill.
                BodyExprKind::SelfFieldAtPut => {
                    // BT-4310: the name and value are sequenced ahead of the put.
                    let scope =
                        self.thread_send_arguments(expr, threaded_ir::FrameId::ROOT, &mut stmts)?;
                    let (doc, val_var) = self.generate_self_field_at_put_open(expr)?;
                    self.finish_precompiled_scope(scope)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    if is_last {
                        let final_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "{'reply', ",
                                leaf::var(val_var),
                                ", ",
                                leaf::var(final_state),
                                "}",
                            ],
                            span,
                        ));
                    }
                }
                // Mirrors SelfFieldAtPut immediately above — same opaque
                // Statement + is_last reply shape, mirroring
                // `generate_self_clear_field_open` (ADR 0124 §1/B4).
                BodyExprKind::SelfClearField => {
                    let scope =
                        self.thread_send_arguments(expr, threaded_ir::FrameId::ROOT, &mut stmts)?;
                    let (doc, val_var) = self.generate_self_clear_field_open(expr)?;
                    self.finish_precompiled_scope(scope)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    if is_last {
                        let final_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "{'reply', ",
                                leaf::var(val_var),
                                ", ",
                                leaf::var(final_state),
                                "}",
                            ],
                            span,
                        ));
                    }
                }
                BodyExprKind::FieldAssignment => {
                    // ADR 0118 phase 1a: `self.log := self.log ++ #(self
                    // getValue)`, `self.count := self.count + (self bump)` —
                    // the RHS's state-effecting sub-expressions land in this
                    // body's IR as real `Bind`s BEFORE `source_version` is
                    // read (the exact snapshot a reverted prototype once
                    // desynced) and before the shared open helper mints its
                    // own step; the RHS compile below substitutes the
                    // already-sequenced value.
                    let rhs_scope = match expr {
                        Expression::Assignment { value, .. } => {
                            self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?
                        }
                        _ => PrecompiledScope::new(),
                    };
                    if is_last {
                        if let Expression::Assignment { target, value, .. } = expr {
                            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                let val_var = self.fresh_temp_var("Val");
                                let source_version = self.state_version();
                                let value_str = self.expression_doc(value)?;
                                let new_state = self.next_state_var();
                                let target_version = self.state_version();
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "let ",
                                        leaf::var(val_var.clone()),
                                        " = ",
                                        value_str,
                                        " in ",
                                    ],
                                    span,
                                ));
                                // BT-4369: every field write records itself.
                                self.note_field_write(&field.name, span);
                                // The real field mutation, as a real Bind —
                                // `render_bind`'s `BindOp::Put` arm is the
                                // single place the `maps:put` shape lives.
                                stmts.push(ThreadedStmt::Bind {
                                    target: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        target_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    source: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        source_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    op: threaded_ir::BindOp::Put {
                                        field: field.name.to_string(),
                                        value: threaded_ir::ValueRef::Var(val_var.clone()),
                                        // Unused placeholder — see
                                        // `lower_cf_field_assignment_binds`.
                                    },
                                    span,
                                });
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "{'reply', ",
                                        leaf::var(val_var),
                                        ", ",
                                        leaf::var(new_state),
                                        "}",
                                    ],
                                    span,
                                ));
                            }
                        }
                    } else {
                        // Shared helper (`generate_field_assignment_open`,
                        // also called from conditionals/exception_handling/
                        // list_ops/intrinsics): opaque Statement, version
                        // step backfilled at verify time.
                        let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                        stmts.push(ThreadedStmt::Statement(doc, span));
                    }
                    self.finish_precompiled_scope(rhs_scope)?;
                }
                // Self.field := expr where RHS is control flow returning {Value, State}
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            // ADR 0118 phase 1a: `self.f := 1 to: (self bump)
                            // do: [..]` — the construct's own state-effecting
                            // operands thread ahead of it.
                            let producer = self.local_producer_locals(value.unwrap_parens());
                            let rhs_scope =
                                self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                            // ADR 0118 phase 4: when the RHS is
                            // itself an inline-threaded control-flow
                            // construct, `thread_ahead` above already
                            // spliced its real prelude into `stmts` (via
                            // `subexpr_needs_prelude`/
                            // `inline_control_flow_needs_threading`
                            // recognizing this shape) and registered its
                            // ALREADY-UNWRAPPED value for substitution —
                            // `expression_doc` below returns that value
                            // directly, not a `{Value, State}` tuple, so no
                            // further `element/1` unwrap runs. Only the
                            // field's own `maps:put` version-bump remains.
                            // See `emit_actor_threaded_last_stmts`'s matching
                            // comment. Every other `FieldAssignmentControlFlow`
                            // shape (loops, list-ops) still returns a raw
                            // tuple `Document`, unpacked by the `tuple_var`/
                            // `lower_cf_field_assignment_binds` path below.
                            let val_var = self.fresh_temp_var("CfVal");
                            // ADR 0131 Phase 2d (BT-3795): a loop or fold that
                            // is a local-threading producer (a sequenced site
                            // of this write, `sequenced_operand_sites`) was
                            // spliced by `thread_ahead` above too: its tuple
                            // is unpacked and its locals rebound in the
                            // prelude, so the value is already unwrapped.
                            if producer.is_some()
                                || self.inline_control_flow_needs_threading(value.unwrap_parens())
                            {
                                let value_str = self.expression_doc(value)?;
                                self.finish_precompiled_scope(rhs_scope)?;
                                let source_version = self.state_version();
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "let ",
                                        leaf::var(val_var.clone()),
                                        " = ",
                                        value_str,
                                        " in "
                                    ],
                                    span,
                                ));
                                // BT-4369: every field write records itself.
                                self.note_field_write(&field.name, span);
                                let _ = self.next_state_var();
                                let target_version = self.state_version();
                                stmts.push(ThreadedStmt::Bind {
                                    target: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        target_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    source: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        source_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    op: threaded_ir::BindOp::Put {
                                        field: field.name.to_string(),
                                        value: threaded_ir::ValueRef::Var(val_var.clone()),
                                    },
                                    span,
                                });
                            } else {
                                // Evaluate the RHS (returns {Value, State} tuple)
                                let tuple_var = self.fresh_temp_var("CfTuple");
                                let value_str = self.expression_doc(value)?;
                                self.finish_precompiled_scope(rhs_scope)?;
                                // Unpack the tuple: element(1) is the value, element(2) is the state
                                let rhs_state = self.fresh_temp_var("CfState");
                                self.lower_cf_field_assignment_binds(
                                    &mut stmts,
                                    docvec![
                                        "let ",
                                        leaf::var(tuple_var.clone()),
                                        " = ",
                                        value_str,
                                        " in let ",
                                        leaf::var(val_var.clone()),
                                        " = call 'erlang':'element'(1, ",
                                        leaf::var(tuple_var.clone()),
                                        ") in ",
                                    ],
                                    &tuple_var,
                                    &rhs_state,
                                    field.name.as_str(),
                                    &val_var,
                                    span,
                                );
                            }
                            let field_state = self.current_state_var();
                            // Extract threaded locals from the control flow state
                            // (e.g. ifTrue: [y := 1. y + 1] threads y via __local__ keys)
                            let mut doc_parts: Vec<Document<'static>> = Vec::new();
                            if let Some(threaded_vars) = self
                                .threaded_locals_of(value)
                                .and_then(ThreadedLocals::into_lowered)
                                .filter(|_| {
                                    producer.is_none()
                                        && !self.inline_producer_construct(value.unwrap_parens())
                                })
                            {
                                for var in &threaded_vars {
                                    let tv_core = self.lookup_var(var).map_or_else(
                                        || Self::to_core_erlang_var(var),
                                        String::clone,
                                    );
                                    doc_parts.push(docvec![
                                        "let ",
                                        leaf::var(tv_core),
                                        " = call 'maps':'get'(",
                                        leaf::atom(Self::local_state_key(var)),
                                        ", ",
                                        leaf::var(field_state.clone()),
                                        ") in ",
                                    ]);
                                }
                            }
                            if !doc_parts.is_empty() {
                                stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                            }
                            if is_last {
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "{'reply', ",
                                        leaf::var(val_var),
                                        ", ",
                                        leaf::var(field_state),
                                        "}",
                                    ],
                                    span,
                                ));
                            }
                        }
                    }
                }
                // Self fieldAt: name put: expr where RHS is control flow returning {Value, State}
                //
                // The mutation is a real Bind: the map key is a dynamic
                // (computed) value, not a static field name, so it cannot
                // use `BindOp::Put` (whose `field` is a literal atom) —
                // the investigation-established `Direct(ValueRef::Doc(...))`
                // idiom carries the whole `maps:put` expression opaquely
                // instead (ADR 0111 Addendum 4's type-level rule: a
                // version-mutating statement must be a `Bind`, never a
                // `Statement`, but `Bind`'s `op` may still be opaque).
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        // BT-4369: a dynamic-key field write records itself for the loop checkout.
                        self.note_field_write("<dynamic>", span);
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
                                ") in let ",
                                leaf::var(rhs_state.clone()),
                                " = call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
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
                        let field_state = self.current_state_var();
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self
                            .threaded_locals_of(&arguments[1])
                            .and_then(ThreadedLocals::into_lowered)
                        {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(tv_core),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(field_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                        if is_last {
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "{'reply', ",
                                    leaf::var(val_var),
                                    ", ",
                                    leaf::var(field_state),
                                    "}",
                                ],
                                span,
                            ));
                        }
                    }
                }
                // {a, b} := expr where RHS is control flow returning {Value, State}.
                // Element 2 of the RHS tuple becomes the next real `State` Bind
                // (Direct — the RHS is a computed map, not a static field Put);
                // the tuple unpack and threaded-local rebinds stay opaque Statements.
                BodyExprKind::DestructureAssignmentControlFlow => {
                    let mut rhs_value = None;
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // Evaluate RHS (returns {Value, State} tuple)
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
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        let new_state = self.current_state_var();
                        // Extract threaded locals
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self
                            .threaded_locals_of(value)
                            .and_then(ThreadedLocals::into_lowered)
                        {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(tv_core),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                        // Now destructure the unpacked value
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        rhs_value = Some(actual_val);
                    }
                    if is_last {
                        let reply = self.destructure_reply_doc(rhs_value)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                // Real state Bind: the RHS's element 2 IS the next `State`
                // version (a computed map, not a static field — Direct, not Put).
                BodyExprKind::LocalAssignTier2 => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let tuple_var = self.fresh_temp_var("T2Tuple");
                            let value_str = self.generate_tier2_value_call_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            let source_version = self.state_version();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(tuple_var.clone()),
                                    " = ",
                                    value_str,
                                    " in let ",
                                    leaf::var(core_var),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var.clone()),
                                    ")\n in ",
                                ],
                                span,
                            ));
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    target_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                source: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    source_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(
                                    docvec![
                                        "call 'erlang':'element'(2, ",
                                        leaf::var(tuple_var),
                                        ")",
                                    ],
                                )),
                                span,
                            });
                        }
                    }
                    if is_last {
                        let reply = self.local_assign_reply_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::LocalAssignControlFlow => {
                    // Route the actor `var := <control-flow-with-mutations>`
                    // assign-RHS through the shared `ThreadedExpr` emitter. The Actor boundary
                    // binds the target to element 1, advances the state version to element 2
                    // (the threaded gen_server `State`, via a real Bind) and rebinds
                    // `__local__`-threaded sibling outer-locals.
                    //
                    // ADR 0111 Addendum 4: `classify_body_expr` already decided this is
                    // `LocalAssignControlFlow` (Phase 1) — `emit_actor_threaded_assign_rhs_stmts`
                    // never declines, so there is no second computation left to disagree
                    // with, and `verify_routing_invariant`/`RoutingMismatch` are deleted.
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            // ADR 0131 Phase 2: a loop or fold is a
                            // local-threading producer — its construct
                            // tuple, `State` `Bind` and `LocalRebind`s land
                            // in this body's IR, then the target is bound
                            // to its logical value.
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            if let Some(tv) =
                                self.threaded_construct_value(value, threaded_ir::FrameId::ROOT)?
                            {
                                stmts.extend(tv.prelude);
                                let value_str = self.threaded_value_doc(&tv.value);
                                self.bind_var(var_name, &core_var);
                                stmts.push(ThreadedStmt::Statement(
                                    docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                    span,
                                ));
                            } else {
                                // ADR 0118 phase 1a: `x := 1 to: (self bump) do:
                                // [..]` — the construct's own state-effecting
                                // operands thread ahead of it.
                                let rhs_scope = self.thread_ahead(
                                    value,
                                    &mut stmts,
                                    threaded_ir::FrameId::ROOT,
                                )?;
                                self.emit_actor_threaded_assign_rhs_stmts(
                                    var_name, value, &mut stmts,
                                )?;
                                self.finish_precompiled_scope(rhs_scope)?;
                            }
                        }
                    }
                    if is_last {
                        let reply = self.local_assign_reply_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                // Var := self method — dispatch RHS, bind result to var
                BodyExprKind::LocalAssignSelfSend => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            // ADR 0118 phase 1a: `v := self log: (self nextId)`
                            // — the producer's `Statement` + real `Bind`
                            // (its arguments sequenced first), then the
                            // local bound to its pure result reference.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let value_str = self.threaded_value_doc(&tv.value);
                            self.bind_var(var_name, &core_var);
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                span,
                            ));
                        }
                    }
                    if is_last {
                        let reply = self.local_assign_reply_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                // Var := [block needing Tier 2], where prescan_tier2_local_vars
                // already proved every later use of `var` in this body is a safe
                // `value`/`value:`/etc. call. Generate the block via
                // generate_block_stateful directly (bypassing generate_block's
                // "unsupported block" rejection, which is only needed when the
                // compiler can't prove the later invocation site is safe).
                BodyExprKind::LocalAssignTier2Block => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let (Expression::Identifier(id), Expression::Block(block)) =
                            (target.as_ref(), value.as_ref())
                        {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let captured_mutations = Self::captured_mutations_for_block(block);
                            let value_str =
                                self.generate_block_stateful(block, &captured_mutations)?;
                            self.bind_var(var_name, &core_var);
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                span,
                            ));
                        }
                    }
                    if is_last {
                        let reply = self.local_assign_reply_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::LocalAssignPure => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            // BT-3495: `r := (self.field := ...)` as a flat
                            // top-level method-body statement — the RHS is
                            // itself a field write, at any parenthesization
                            // depth. `classify_body_expr` hands every
                            // `LocalAssignPure` RHS here regardless of shape,
                            // and the generic `threaded_expression` compile
                            // below has no way to fold a NESTED field write's
                            // own state mutation into this body's `State`
                            // chain (it recompiles the whole `self.field :=
                            // ...` expression via plain `generate_expression`,
                            // producing an `erlc: unbound variable` crash).
                            // Lower it through the SAME real-`Bind` producer
                            // `FieldAssignment` (above) and the branch/loop
                            // lowerings use — `lower_field_assignment_bind`
                            // — then alias the local var to the identical
                            // assigned value, exactly as `:=`'s "the whole
                            // assignment evaluates to the assigned value"
                            // semantics require (mirrors
                            // `lower_local_var_assignment_bind`'s C2z fix,
                            // BT-3493, one level up at this flat top-level
                            // body instead of a branch/loop frame). Falls
                            // through to this arm's own reply behavior below
                            // (`local_assign_reply_doc`, the assigned value)
                            // when this is the body's last statement — only
                            // the RHS lowering differs.
                            if let Some(field_write) = self.local_assign_field_write(value) {
                                let field_val_var = self.lower_field_assignment_bind(
                                    field_write,
                                    threaded_ir::FrameId::ROOT,
                                    span,
                                    &mut stmts,
                                )?;
                                self.bind_var(&id.name, &field_val_var);
                            } else {
                                let var_name = &id.name;
                                let core_var = self.lookup_var(var_name).map_or_else(
                                    || Self::to_core_erlang_var(var_name),
                                    String::clone,
                                );
                                // ADR 0118 phase 1a: `ok := (self recordOnce: x)
                                // and: [y]`, `total := items size + (self bump)` —
                                // the RHS's state-effecting sub-expressions land
                                // in this body's IR as real `Bind`s, in source
                                // order, via the sequencing rule.
                                let tv =
                                    self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                                stmts.extend(tv.prelude);
                                let value_str = self.threaded_value_doc(&tv.value);
                                self.bind_var(var_name, &core_var);
                                stmts.push(ThreadedStmt::Statement(
                                    docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                    span,
                                ));
                            }
                        }
                    }
                    if is_last {
                        let reply = self.local_assign_reply_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    let mut rhs_value = None;
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // ADR 0118 phase 1b: `{a, b} := #(1, self bump)` —
                        // the RHS's state-effecting sub-expressions land in
                        // this body's IR as real `Bind`s before
                        // `generate_destructure_bindings`'s own
                        // `eval_rhs_to_temp_var` call (`expressions.rs`)
                        // compiles `value` via `expression_doc`, which
                        // substitutes the already-sequenced value.
                        let (binding_docs, rhs_var) = self
                            .generate_destructure_bindings_sequenced(
                                pattern,
                                value,
                                threaded_ir::FrameId::ROOT,
                                &mut stmts,
                            )?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        rhs_value = Some(rhs_var);
                    }
                    if is_last {
                        let reply = self.destructure_reply_doc(rhs_value)?;
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::SuperSend => {
                    // BT-3814 / BT-4318: the one `super` producer — the
                    // operands thread ahead of the dispatch (the sequencing
                    // rule), and its `State` `Bind` carries the callee's
                    // state change to the next statement or the reply.
                    let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                    stmts.extend(tv.prelude);
                    if is_last {
                        let reply = self.threaded_value_reply_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::ErrorSend => {
                    // BT-4310: `self error: (xs inject: ...) printString` — the
                    // argument is sequenced ahead of the send, like a `super`
                    // send's.
                    let error_scope =
                        self.thread_send_arguments(expr, threaded_ir::FrameId::ROOT, &mut stmts)?;
                    if is_last {
                        // Error send never returns — no reply tuple needed.
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(docvec![expr_str], span));
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in "],
                            span,
                        ));
                    }
                    self.finish_precompiled_scope(error_scope)?;
                }
                BodyExprKind::Tier2ValueCall => {
                    if is_last {
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        let reply = self.emit_tuple_unpack_reply("T2Tuple", expr_str);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    } else {
                        // Real state Bind: element 2 of the Tier-2 tuple IS the
                        // next `State` version (a computed map — Direct, not Put).
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let discard_var = self.fresh_temp_var("T2Discard");
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_str,
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
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            span,
                        });
                        let new_state = self.current_state_var();

                        // Extract captured local mutations from NewState
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(mutations) = self.get_inline_block_captured_mutations(expr) {
                            for var in &mutations {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                    }
                }
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        // Route the last-position actor control-flow
                        // construct through the shared `ThreadedExpr` emitter. The Actor
                        // boundary binds element 1 (Reply) and element 2 (the threaded
                        // gen_server `State`, via a real Bind) and returns
                        // `{'reply', Reply, NewState}`.
                        //
                        // ADR 0111 Addendum 4: `classify_body_expr` already decided this
                        // is `ControlFlowWithMutations` (Phase 1) —
                        // `emit_actor_threaded_last_stmts` never declines, so
                        // `verify_routing_invariant`/`RoutingMismatch` are deleted (no
                        // second computation left to disagree with).
                        //
                        // ADR 0118 phase 1a: `1 to: (self bump) do: [..]`,
                        // `(self bump) timesRepeat: [..]` — the construct's own
                        // state-effecting operands (bounds, receiver) thread
                        // ahead of it, so it starts from their post-dispatch
                        // state.
                        let scope =
                            self.thread_ahead(expr, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        self.emit_actor_threaded_last_stmts(expr, &mut stmts)?;
                        self.finish_precompiled_scope(scope)?;
                    } else {
                        // Real state Bind: element 2 of the construct's tuple IS the
                        // next `State` version (a computed map — Direct, not Put).
                        let scope =
                            self.thread_ahead(expr, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        // ADR 0118 phase 4: when `expr` is itself an
                        // inline-threaded control-flow construct, the
                        // `thread_ahead` call just above (now that
                        // `subexpr_needs_prelude` recognizes this shape) already
                        // spliced its real prelude into `stmts` and registered
                        // its already-unwrapped value — see
                        // `emit_actor_threaded_last_stmts`'s matching comment.
                        // `expression_doc` below then returns that value
                        // directly, so no further `element/2` unwrap runs.
                        // Every other `ControlFlowWithMutations` shape (loops,
                        // list-ops) still returns a raw tuple `Document` here,
                        // unpacked by the manual `Tuple`/`Bind` pair below.
                        //
                        // ADR 0131 Phase 3: that conditional is a local-threading
                        // producer, so its prelude also carries one
                        // `LocalRebind` per threaded local; only the raw tuple
                        // path below re-reads its locals from the new state.
                        let inline_conditional = self.thread_ahead_unwraps(expr);
                        if inline_conditional {
                            let expr_str = self.expression_doc(expr)?;
                            self.finish_precompiled_scope(scope)?;
                            let seq_var = self.fresh_temp_var("seq");
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(seq_var), " = ", expr_str, " in "],
                                span,
                            ));
                        } else {
                            let tuple_var = self.fresh_temp_var("Tuple");
                            let expr_str = self.expression_doc(expr)?;
                            self.finish_precompiled_scope(scope)?;
                            let source_version = self.state_version();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(tuple_var.clone()),
                                    " = ",
                                    expr_str,
                                    " in "
                                ],
                                span,
                            ));
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    target_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                source: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    source_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(
                                    docvec![
                                        "call 'erlang':'element'(2, ",
                                        leaf::var(tuple_var),
                                        ")",
                                    ],
                                )),
                                span,
                            });
                        }
                        let new_state = self.current_state_var();

                        // Extract threaded locals from the updated state
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self
                            .threaded_locals_of(expr)
                            .and_then(ThreadedLocals::into_lowered)
                            .filter(|_| !inline_conditional)
                        {
                            for var in &threaded_vars {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                    }
                }
                BodyExprKind::DispatchingSelfSend => {
                    // ADR 0118 phase 1a: `self log: (self nextId)` — the
                    // producer (`generate_self_dispatch`) sequences its own
                    // arguments and yields the `Statement` + real `Bind` pair
                    // this body splices; the reply reads its pure result.
                    let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                    stmts.extend(tv.prelude);
                    if is_last {
                        let reply = self.threaded_value_reply_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::EarlyReturn => {
                    return Err(CodeGenError::Internal(
                        "EarlyReturn should be handled before match dispatch".to_string(),
                    ));
                }
                // The top-level counterpart of `conditionals.rs`'s
                // C12 catch-all — `Array with: (self bump)`, `(self
                // recordOnce: x) and: [y]`, `"{self next}"` as a method-body
                // statement of its own. Every order-safe nested self-send
                // is threaded as a real `Bind` ahead of the compile, so the
                // `post_state` read after it (and the next statement) see
                // the dispatch's `NewState`. No-op when there is nothing to
                // hoist.
                //
                // ADR 0118 phase 1a: `threaded_expression` replaces the planner
                // here — the statement's state-effecting sub-expressions land
                // in this body's IR as real `Bind`s in source order (the
                // sequencing rule temp-binds whatever precedes them), and the
                // `post_state` read after them (and the next statement) see
                // the dispatch's `NewState`. A pure statement costs one
                // `generate_expression` call, as before.
                BodyExprKind::Pure => {
                    if is_last {
                        let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                        stmts.extend(tv.prelude);
                        let expr_str = self.threaded_value_doc(&tv.value);
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let _Result = ",
                                expr_str,
                                " in {'reply', _Result, ",
                                leaf::var(post_state),
                                "}",
                            ],
                            span,
                        ));
                    } else {
                        // Mint order: `seq` before the expression, as before.
                        let tmp_var = self.fresh_temp_var("seq");
                        let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                        stmts.extend(tv.prelude);
                        let expr_str = self.threaded_value_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in "],
                            span,
                        ));
                    }
                }
            }
        }

        Ok(stmts)
    }

    /// Emit the `{'reply', Var, State}` close for a local assignment
    /// `x := expr` in last position, once its open let chain has bound `x`:
    /// an assignment evaluates to the assigned value, so the method answers
    /// it, as class and value-type methods do (BT-3882). Every caller has
    /// bound an identifier target, so an unbound one is an internal error
    /// rather than a silent `nil`.
    fn local_assign_reply_doc(&mut self, expr: &Expression) -> Result<Document<'static>> {
        let value = match expr {
            Expression::Assignment { target, .. } => match target.as_ref() {
                Expression::Identifier(id) => self.lookup_var(&id.name).cloned(),
                _ => None,
            },
            _ => None,
        };
        let var = value.ok_or_else(|| {
            CodeGenError::Internal(
                "local_assign_reply_doc: last-statement assignment target is not a bound local"
                    .to_string(),
            )
        })?;
        let post_state = self.current_state_var();
        Ok(docvec![
            "{'reply', ",
            leaf::var(var),
            ", ",
            leaf::var(post_state),
            "}"
        ])
    }

    /// Emit the `{'reply', Rhs, State}` close for a destructuring assignment
    /// `{a, b} := expr` in last position: it evaluates to its right-hand side,
    /// `rhs_value` the variable bound to it (BT-3889).
    fn destructure_reply_doc(&mut self, rhs_value: Option<String>) -> Result<Document<'static>> {
        let rhs = rhs_value.ok_or_else(|| {
            CodeGenError::Internal(
                "destructure_reply_doc: a destructuring arm bound no right-hand side".to_string(),
            )
        })?;
        let post_state = self.current_state_var();
        Ok(docvec![
            "{'reply', ",
            leaf::var(rhs),
            ", ",
            leaf::var(post_state),
            "}"
        ])
    }

    /// ADR 0118 phase 1a: the last-position reply for a spliced
    /// [`threaded_ir::ThreadedValue`] — `{'reply', <value>, StateN}` where
    /// `StateN` is the state after the value's prelude.
    fn threaded_value_reply_doc(&mut self, value: &threaded_ir::ValueRef) -> Document<'static> {
        let value_doc = self.threaded_value_doc(value);
        let final_state = self.current_state_var();
        docvec!["{'reply', ", value_doc, ", ", leaf::var(final_state), "}"]
    }

    /// Emit the last-position reply for an expression that returns a
    /// `{Result, State}` tuple (Tier 2 value calls, control flow with
    /// mutations, early returns with mutations).
    fn emit_tuple_unpack_reply(
        &mut self,
        tuple_label: &str,
        expr_doc: Document<'static>,
    ) -> Document<'static> {
        let tuple_var = self.fresh_temp_var(tuple_label);
        docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            expr_doc,
            " in let _Result = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in let _NewState = call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ") in {'reply', _Result, _NewState}",
        ]
    }

    /// Extracts source text for a method using the AST unparser.
    ///
    /// The unparser produces complete, comment-inclusive source for all methods,
    /// whether parsed from a `.bt` file or constructed programmatically by a live
    /// tool (synthesized methods have no source text but still produce valid output).
    ///
    /// Previously this used raw byte-range slicing (`source[span.start..span.end]`),
    /// which silently dropped leading comments (they appear before `method.span.start()`)
    /// and fell back to the selector name for synthesized methods. The unparser fixes
    /// both deficiencies — see ADR 0044 Phase 4.
    ///
    /// `class_name`/`is_class_method` identify `method` within
    /// `self.method_return_types_written_back` (keyed by
    /// `(class_name, selector, is_class_method)` — the caller's own
    /// knowledge of which method list `method` came from, rather than
    /// `method.is_class_method`, which a standalone extension method
    /// (`Target class >> sel`) does not reliably carry). When present, the
    /// return-type writeback pass — not the user — wrote `method.return_type`,
    /// so this unparses a throwaway clone with `return_type` reset to `None`
    /// instead of `method` itself. Without this, the image-resident
    /// `__source__` text this feeds (used by the System Browser / cockpit /
    /// `SystemNavigation` scanners) carries an inferred `-> Type` annotation
    /// the `ChangeLog`'s canonical `source_ref` (unparsed pre-writeback) never
    /// had — the divergence that produces a spurious revert-then-resave
    /// `ChangeLog` entry. `method`'s own `return_type` is never mutated:
    /// codegen elsewhere (specs, `method_return_types` metadata) still needs
    /// the inferred type.
    pub(in crate::core_erlang) fn extract_method_source(
        &self,
        class_name: &str,
        is_class_method: bool,
        method: &MethodDefinition,
    ) -> String {
        let key: beamtalk_core::semantic_analysis::MethodReturnKey = (
            EcoString::from(class_name),
            method.selector.name(),
            is_class_method,
        );
        if method.return_type.is_some() && self.method_return_types_written_back.contains_key(&key)
        {
            let mut stripped = method.clone();
            beamtalk_core::semantic_analysis::clear_return_type_writeback_for_key(
                &mut stripped,
                &key,
                &self.method_return_types_written_back,
            );
            self.as_written_method_source(&stripped)
        } else {
            self.as_written_method_source(method)
        }
    }
}

#[cfg(test)]
mod tests;
