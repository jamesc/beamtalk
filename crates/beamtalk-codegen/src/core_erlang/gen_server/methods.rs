// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method body code generation and class registration.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples, and the `register_class/0` on-load function.

use super::super::method_frame::{MethodBoundary, MethodFrame};
use super::super::selector_mangler::safe_class_method_fn_name;
use super::super::sequencing::PrecompiledScope;
use super::super::{
    CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis, threaded_ir,
};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::fname;
use beamtalk_cerl_doc::{Document, INDENT, join, leaf, line, nest};
use beamtalk_core::ast::{
    Block, CascadeMessage, ClassDefinition, Expression, Identifier, Literal, MapPair,
    MessageSelector, MethodDefinition, MethodKind, ParameterDefinition, WellKnownSelector,
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
    /// `self error: "..."` — never returns.
    ErrorSend,
    /// Tier 2 `value:` call — returns `{Result, NewState}`.
    Tier2ValueCall,
    /// Tier 2 self-send with stateful block arguments.
    Tier2SelfSend(Vec<(usize, Vec<String>)>),
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
        // Pre-scan for Tier 2 block parameters before generating method bodies
        self.scan_class_for_tier2_blocks(class);

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
        let (mut frame, param_vars) = MethodFrame::enter(
            self,
            selector_name.as_str(),
            &method.parameters,
            MethodBoundary::Actor,
        );

        // Populate tier2_block_params for this method from pre-scanned info.
        frame.tier2_block_params.clear();
        // Reset the same-method local-var tracking. The real (re-)population
        // happens inside lower_body_exprs_with_reply via
        // prescan_tier2_local_vars — this clear here is belt-and-suspenders
        // for the case where a caller inspects the field between the clear
        // and the body being generated.
        frame.tier2_local_vars.clear();
        frame.tier2_local_var_captured_mutations.clear();
        let selector_name_for_t2 = selector_name.to_string();
        if let Some(positions) = frame.tier2_method_info.get(&selector_name_for_t2).cloned() {
            for pos in &positions {
                if *pos < method.parameters.len() {
                    frame
                        .tier2_block_params
                        .insert(method.parameters[*pos].name.name.to_string());
                }
            }
        }

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
        if needs_letrec && nlr_token_var.is_some() {
            docvec![
                "letrec '__nlr_body'/0 = fun () ->\n",
                rendered_body,
                "\n",
                "in apply '__nlr_body'/0 ()",
            ]
        } else {
            rendered_body
        }
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
    /// This runs as a full pre-scan (like `tier2_block_params`'s class-level
    /// scan) rather than incrementally during codegen, specifically so that a
    /// block which *escapes* this method unsafely (returned, stored elsewhere,
    /// passed as an argument) is never promoted — it must keep hitting the
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic,
    /// since no known call site would thread state through it correctly.
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
            let captured_mutations = Self::captured_mutations_for_block(block);
            let needs_tier2 = !captured_mutations.is_empty()
                || (self.context == CodeGenContext::Actor
                    && !block_analysis::analyze_block(block).field_writes.is_empty());
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
            }
        }
    }

    /// Normalizes a `Cascade` into its true underlying receiver and the
    /// full ordered list of messages sent to it.
    ///
    /// The parser (`parse_cascade`) folds the cascade's *first* message into
    /// `Cascade.receiver` as a whole `MessageSend` — e.g. for `blk value: x;
    /// value: y`, `receiver` is `MessageSend(blk, value:, [x])` and `messages`
    /// holds only the remaining `value: y`. Every safety/codegen decision needs
    /// the TRUE receiver (`blk`) and ALL messages sent to it (both `value: x`
    /// and `value: y`), so this mirrors the same normalization
    /// `generate_cascade` (expressions.rs) already performs for ordinary
    /// (non-Tier-2) cascade codegen.
    fn normalize_cascade<'a>(
        receiver: &'a Expression,
        messages: &'a [CascadeMessage],
    ) -> (&'a Expression, Vec<(&'a MessageSelector, &'a [Expression])>) {
        if let Expression::MessageSend {
            receiver: inner,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            let mut all: Vec<(&MessageSelector, &[Expression])> =
                Vec::with_capacity(messages.len() + 1);
            all.push((first_selector, first_arguments.as_slice()));
            for msg in messages {
                all.push((&msg.selector, msg.arguments.as_slice()));
            }
            (inner.as_ref(), all)
        } else {
            let all: Vec<(&MessageSelector, &[Expression])> = messages
                .iter()
                .map(|msg| (&msg.selector, msg.arguments.as_slice()))
                .collect();
            (receiver, all)
        }
    }

    /// Returns true if `selector` is a `value`/`value:`/
    /// `value:value:`/`value:value:value:` send — the "safe" family that lets a
    /// Tier 2 block value be invoked without escaping to a call site that
    /// doesn't know to thread state through it.
    fn is_safe_value_family_selector(selector: &MessageSelector) -> bool {
        matches!(
            selector.well_known(),
            Some(
                WellKnownSelector::Value
                    | WellKnownSelector::ValueColon
                    | WellKnownSelector::ValueValue
                    | WellKnownSelector::ValueValueValue
            )
        )
    }

    /// Scans `expr` for references to `var_name`, returning
    /// `(has_unsafe_use, has_safe_use)`.
    ///
    /// A *safe* use is the receiver of a `value`/`value:`/`value:value:`/
    /// `value:value:value:` send. Any other reference — a bare return, an
    /// argument to another call, a reassignment, ... — is *unsafe*, since it
    /// would let a Tier 2 block value escape to a call site that doesn't know
    /// to thread state through it. A variable that's *never* referenced at
    /// all yields `(false, false)`, which the caller must treat as unsafe
    /// (not "no unsafe use found") — see `prescan_tier2_local_vars`.
    ///
    /// Deliberately conservative: exhaustively matches every `Expression`
    /// variant so a use hidden inside e.g. a map literal or string
    /// interpolation is never silently missed. A shadowing block parameter
    /// with the same name is *not* special-cased — that only makes this
    /// over-conservative (a missed promotion), never unsafe.
    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive match over every Expression variant, kept as one function for locality with its single caller"
    )]
    fn scan_var_uses(expr: &Expression, var_name: &str) -> (bool, bool) {
        match expr {
            Expression::Identifier(id) => (id.name == var_name, false),
            Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Error { .. } => (false, false),
            Expression::Spread { name, .. } => (name.name == var_name, false),
            Expression::FieldAccess { receiver, .. } => Self::scan_var_uses(receiver, var_name),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                let is_safe_value_send = matches!(
                    receiver.as_ref(),
                    Expression::Identifier(id) if id.name == var_name
                ) && Self::is_safe_value_family_selector(selector);
                let (mut unsafe_, mut safe) = if is_safe_value_send {
                    (false, true)
                } else {
                    Self::scan_var_uses(receiver, var_name)
                };
                for arg in arguments {
                    let (u, s) = Self::scan_var_uses(arg, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::Block(block) => {
                // Any reference to var_name inside a nested block literal is
                // unsafe — see the safety invariant note on
                // prescan_tier2_local_vars above (a nested block compiles
                // through a completely different path with no Tier2-tuple
                // unpacking and no tier2_local_vars reset of its own).
                let (any_unsafe, any_safe) = block
                    .body
                    .iter()
                    .map(|stmt| Self::scan_var_uses(&stmt.expression, var_name))
                    .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
                (any_unsafe || any_safe, false)
            }
            Expression::Assignment { target, value, .. } => {
                let (u1, s1) = Self::scan_var_uses(target, var_name);
                let (u2, s2) = Self::scan_var_uses(value, var_name);
                (u1 || u2, s1 || s2)
            }
            Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
                Self::scan_var_uses(value, var_name)
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // When the cascade's true underlying receiver (see
                // `normalize_cascade`) *is* var_name itself (e.g. `blk value: x;
                // value: y`), the generic recursive scan would hit the plain
                // `Identifier` arm and unconditionally report it unsafe. Mirror the
                // `MessageSend` arm's `is_safe_value_send` check instead: if EVERY
                // message sent to that receiver (including the one folded into
                // `receiver` by the parser) is itself a safe
                // `value`/`value:`/`value:value:`/`value:value:value:` send, the
                // whole cascade is as safe as a single safe value send would be.
                let (underlying_receiver, all_messages) =
                    Self::normalize_cascade(receiver, messages);
                let receiver_is_var = matches!(
                    underlying_receiver,
                    Expression::Identifier(id) if id.name == var_name
                );
                let all_messages_safe_value_sends = receiver_is_var
                    && !all_messages.is_empty()
                    && all_messages
                        .iter()
                        .all(|(sel, _)| Self::is_safe_value_family_selector(sel));
                let (mut unsafe_, mut safe) = if all_messages_safe_value_sends {
                    (false, true)
                } else {
                    Self::scan_var_uses(underlying_receiver, var_name)
                };
                for (_, args) in &all_messages {
                    for arg in *args {
                        let (u, s) = Self::scan_var_uses(arg, var_name);
                        unsafe_ |= u;
                        safe |= s;
                    }
                }
                (unsafe_, safe)
            }
            Expression::Parenthesized { expression, .. } => {
                Self::scan_var_uses(expression, var_name)
            }
            Expression::Match { value, arms, .. } => {
                let (mut unsafe_, mut safe) = Self::scan_var_uses(value, var_name);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        let (u, s) = Self::scan_var_uses(guard, var_name);
                        unsafe_ |= u;
                        safe |= s;
                    }
                    let (u, s) = Self::scan_var_uses(&arm.body, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::MapLiteral { pairs, .. } => pairs
                .iter()
                .map(|pair| {
                    let (u1, s1) = Self::scan_var_uses(&pair.key, var_name);
                    let (u2, s2) = Self::scan_var_uses(&pair.value, var_name);
                    (u1 || u2, s1 || s2)
                })
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
            Expression::ListLiteral { elements, tail, .. } => {
                let (mut unsafe_, mut safe) = elements
                    .iter()
                    .map(|e| Self::scan_var_uses(e, var_name))
                    .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
                if let Some(t) = tail {
                    let (u, s) = Self::scan_var_uses(t, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::ArrayLiteral { elements, .. } => elements
                .iter()
                .map(|e| Self::scan_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
            Expression::StringInterpolation { segments, .. } => segments
                .iter()
                .map(|seg| match seg {
                    beamtalk_core::ast::StringSegment::Interpolation(e) => {
                        Self::scan_var_uses(e, var_name)
                    }
                    beamtalk_core::ast::StringSegment::Literal(_) => (false, false),
                })
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        }
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
                if arguments.len() >= 2 && self.control_flow_has_mutations(&arguments[1]) {
                    return BodyExprKind::SelfFieldAtPutControlFlow;
                }
            }
            return BodyExprKind::SelfFieldAtPut;
        }

        // self.field := value — sub-classify by RHS for control flow with mutations
        if Self::is_field_assignment(expr) {
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

        // self error: "..." — never returns
        if Self::is_error_message_send(expr) {
            return BodyExprKind::ErrorSend;
        }

        // Tier 2 value: call
        if self.is_tier2_value_call(expr) {
            return BodyExprKind::Tier2ValueCall;
        }

        // Tier 2 self-send with block args
        if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
            return BodyExprKind::Tier2SelfSend(tier2_args);
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
    fn verify_and_render_body_stmts(
        &mut self,
        stmts: &[threaded_ir::ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let errors = threaded_ir::verify_body_with_opaque_version_gaps(stmts);
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
    ///    `render_bind`'s (`shadow_write` is `false`: actor `State` writes
    ///    never carry the ADR 0110 class-var obligation, so `class_tag` is
    ///    an unused placeholder).
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
            shadow_write: false,
            span,
        });
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: cf_state,
            op: BindOp::Put {
                field: field_name.to_string(),
                value: ValueRef::Var(val_var.to_string()),
                // Unused placeholder: only rendered when shadow_write is
                // true, which only class-var Puts ever set (ADR 0110).
                class_tag: ValueRef::Literal("'nil'"),
            },
            shadow_write: false,
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
    /// (`generate_self_dispatch_open`, `emit_super_send_open`,
    /// `generate_tier2_self_send_open`, `generate_field_assignment_open`,
    /// `generate_self_field_at_put_open`) remain inside their `Statement`s —
    /// see `verify_body_with_opaque_version_gaps`'s backfill accounting.
    ///
    /// Classification happens exactly once (`classify_body_expr`, Phase 1);
    /// the mutating control-flow arms consume that decision via
    /// `emit_actor_threaded_last_stmts`/`emit_actor_threaded_assign_rhs_stmts`,
    /// which never decline — the earlier `verify_routing_invariant`
    /// call sites (and `VerifyError::RoutingMismatch`) are deleted because
    /// there is no second computation left to disagree with the first.
    ///
    /// `supports_early_return` controls whether `^ value` expressions are handled.
    /// Method definitions support it; block bodies do not (NLR uses throw/catch).
    #[expect(
        clippy::too_many_lines,
        reason = "unified handler for all method body expression types with state threading"
    )]
    fn lower_body_exprs_with_reply(
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
        self.prescan_tier2_local_vars(body);

        // Phase 1: classify every expression upfront.  Classification is
        // stateless w.r.t. codegen state (state_version, variable bindings),
        // so pre-computing is safe and separates "what" from "how".
        let plan: Vec<BodyExprKind> = body
            .iter()
            .map(|expr| {
                let kind = self.classify_body_expr(expr);
                if matches!(&kind, BodyExprKind::EarlyReturn) && !supports_early_return {
                    BodyExprKind::Pure
                } else {
                    kind
                }
            })
            .collect();

        // Phase 2: lower each (expression, kind) pair to ThreadedStmts.
        let mut stmts: Vec<ThreadedStmt> = Vec::with_capacity(body.len());
        let body_len = body.len();

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body_len - 1;
            let is_early_return = matches!(&kind, BodyExprKind::EarlyReturn);
            let span = expr.span();

            // Early return — always terminates generation regardless of position.
            // Classify the inner value to handle super/tier2/dispatch returns.
            if is_early_return && supports_early_return {
                if let Expression::Return { value, .. } = expr {
                    let value_kind = self.classify_body_expr(value);
                    match value_kind {
                        BodyExprKind::SuperSend => {
                            let expr_str = self.expression_doc(value)?;
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let _SuperTuple = ",
                                    expr_str,
                                    " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                                    " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                                    " in {'reply', _Result, _NewState}",
                                ],
                                span,
                            ));
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
                        BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                            let tier2_args = tier2_args.clone();
                            let (doc, dispatch_var) =
                                self.generate_tier2_self_send_open(value, &tier2_args)?;
                            stmts.push(ThreadedStmt::Statement(doc, span));
                            let reply = self.dispatch_reply_doc(&dispatch_var);
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
                    let (doc, val_var) = self.generate_self_field_at_put_open(expr)?;
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
                                let value_str = self.generate_field_assignment_value_doc(value)?;
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
                                        class_tag: threaded_ir::ValueRef::Literal("'nil'"),
                                    },
                                    shadow_write: false,
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
                            if self.inline_control_flow_needs_threading(value.unwrap_parens()) {
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
                                        class_tag: threaded_ir::ValueRef::Literal("'nil'"),
                                    },
                                    shadow_write: false,
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
                            if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value)
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
                            shadow_write: false,
                            span,
                        });
                        let field_state = self.current_state_var();
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) =
                            self.get_control_flow_threaded_vars(&arguments[1])
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
                            shadow_write: false,
                            span,
                        });
                        let new_state = self.current_state_var();
                        // Extract threaded locals
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value) {
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
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"],
                            span,
                        ));
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
                                shadow_write: false,
                                span,
                            });
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
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
                            // ADR 0118 phase 1a: `x := 1 to: (self bump) do:
                            // [..]` — the construct's own state-effecting
                            // operands thread ahead of it.
                            let rhs_scope =
                                self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                            self.emit_actor_threaded_assign_rhs_stmts(&id.name, value, &mut stmts)?;
                            self.finish_precompiled_scope(rhs_scope)?;
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
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
                        let reply = self.pure_reply_doc();
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
                        let reply = self.pure_reply_doc();
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
                            // (`pure_reply_doc`, not the assigned value) when
                            // this is the body's last statement — only the
                            // RHS lowering differs.
                            if let Some(field_write) = Self::local_assign_field_write(value) {
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
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // ADR 0118 phase 1b: `{a, b} := #(1, self bump)` —
                        // the RHS's state-effecting sub-expressions land in
                        // this body's IR as real `Bind`s before
                        // `generate_destructure_bindings`'s own
                        // `eval_rhs_to_temp_var` call (`expressions.rs`)
                        // compiles `value` via `expression_doc`, which
                        // substitutes the already-sequenced value.
                        let rhs_scope =
                            self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        self.finish_precompiled_scope(rhs_scope)?;
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"],
                            span,
                        ));
                    }
                }
                BodyExprKind::SuperSend => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let _SuperTuple = ",
                                expr_str,
                                " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                                " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                                " in {'reply', _Result, _NewState}",
                            ],
                            span,
                        ));
                    } else {
                        let mut open_docs: Vec<Document<'static>> = Vec::new();
                        self.emit_super_send_open(expr, &mut open_docs)?;
                        stmts.push(ThreadedStmt::Statement(Document::Vec(open_docs), span));
                    }
                }
                BodyExprKind::ErrorSend => {
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
                            shadow_write: false,
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
                BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                    let (doc, dispatch_var) =
                        self.generate_tier2_self_send_open(expr, tier2_args)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    if is_last {
                        let reply = self.dispatch_reply_doc(&dispatch_var);
                        stmts.push(ThreadedStmt::Statement(reply, span));
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
                        if self.inline_control_flow_needs_threading(expr.unwrap_parens()) {
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
                                shadow_write: false,
                                span,
                            });
                        }
                        let new_state = self.current_state_var();

                        // Extract threaded locals from the updated state
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(expr) {
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

    /// Emit a generic `{'reply', _Result, State}` close for the last expression
    /// when the expression itself has already been emitted as an open let chain.
    /// Used by local assignments and other open-chain handlers in last position.
    fn pure_reply_doc(&mut self) -> Document<'static> {
        let post_state = self.current_state_var();
        docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"]
    }

    /// Emit the last-position reply for a dispatch open call (Tier 2 self-send
    /// or dispatching self-send).  Uses `current_state_var` and the
    /// explicitly-passed `dispatch_var`.
    fn dispatch_reply_doc(&mut self, dispatch_var: &str) -> Document<'static> {
        let final_state = self.current_state_var();
        docvec![
            "{'reply', call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            "), ",
            leaf::var(final_state),
            "}",
        ]
    }

    /// ADR 0118 phase 1a: the last-position reply for a spliced
    /// [`threaded_ir::ThreadedValue`] — `{'reply', <value>, StateN}` where
    /// `StateN` is the state after the value's prelude. Byte-identical to
    /// [`Self::dispatch_reply_doc`] for a self-send (whose value IS
    /// `element(1, _SD)`), which stays for the Tier 2 self-send arm.
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

    /// Emit a super message send in non-last position, threading state.
    fn emit_super_send_open(
        &mut self,
        expr: &Expression,
        docs: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        let super_result_var = self.fresh_temp_var("SuperReply");
        let current_state = self.current_state_var();
        let new_state = self.next_state_var();
        let class_name = self.class_name();

        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let selector_atom = selector.name().to_string();
            let mut arg_docs: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
            for (j, arg) in arguments.iter().enumerate() {
                if j > 0 {
                    arg_docs.push(Document::Str(", "));
                }
                arg_docs.push(self.expression_doc(arg)?);
            }
            docs.push(docvec![
                "let ",
                leaf::var(super_result_var.clone()),
                " = call 'beamtalk_dispatch':'super'(",
                leaf::atom(selector_atom),
                ", [",
                Document::Vec(arg_docs),
                "], Self, ",
                leaf::var(current_state),
                ", ",
                leaf::atom(class_name),
                ")",
            ]);
        }

        docs.push(docvec![
            " in let ",
            leaf::var(new_state),
            " = call 'erlang':'element'(3, ",
            leaf::var(super_result_var),
            ") in ",
        ]);
        Ok(())
    }

    /// Generates standalone function bodies for class-side methods.
    ///
    /// Class methods are module-level functions with a `class_` prefix.
    /// They take `ClassSelf` as the first parameter (the class object),
    /// followed by any user-defined parameters.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'class_defaultValue'/1 = fun (ClassSelf) ->
    ///     42
    ///
    /// 'class_create'/1 = fun (ClassSelf) ->
    ///     let _Result = call 'beamtalk_object_class':'class_send'(
    ///         call 'erlang':'element'(4, ClassSelf), 'new:', [~{}~])
    ///     in _Result
    /// ```
    #[allow(clippy::too_many_lines)] // Error-path cleanup adds necessary lines
    pub(in crate::core_erlang) fn generate_class_method_functions(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        // Populate class variable names for field access validation
        *self.class_var_names_mut() = class
            .class_variables
            .iter()
            .map(|cv| cv.name.name.to_string())
            .collect();

        // Populate class method selectors for self-send routing
        *self.class_method_selectors_mut() = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| m.selector.name().to_string())
            .collect();

        // Populate the class-var-mutating selector set (transitive
        // closure over same-class self-sends) — see
        // `compute_class_var_mutating_selectors`'s doc comment. Depends on
        // `class_var_names` above, so must run after it; independent of
        // `class_method_selectors` above (recomputes its own local view).
        *self.class_var_mutating_selectors_mut() =
            crate::core_erlang::block_analysis::compute_class_var_mutating_selectors(
                class,
                self.class_var_names(),
            );

        // Populate auto-generated keyword constructor selector for Value subclass: classes.
        // This allows `ClassName slot: value` inside a class method to route to the correct
        // class-side constructor instead of falling through to the instance-side getter.
        self.set_class_slot_constructor_selector(
            crate::core_erlang::value_accessors::compute_auto_slot_methods(class)
                .and_then(|auto| auto.keyword_constructor),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();

        for method in &class.class_methods {
            if method.kind != MethodKind::Primary {
                continue;
            }

            let selector_name = method.selector.name();
            // +2 for ClassSelf and ClassVars parameters
            let arity = method.selector.arity() + 2;

            let (mut frame, param_vars) = MethodFrame::enter(
                self,
                selector_name.as_str(),
                &method.parameters,
                MethodBoundary::ClassMethod,
            );

            // Detect if method body has ^ inside blocks (needs NLR).
            let needs_nlr = frame
                .semantic_facts
                .has_block_nlr_or_walk(&method.span, &method.body);

            let nlr_token_var = if needs_nlr {
                let token_var = frame.fresh_temp_var("NlrToken");
                frame.set_current_nlr_token(Some(token_var.clone()));
                Some(token_var)
            } else {
                None
            };

            // ADR 0101: On a `native:` Object, a class-side
            // `self delegate` body lowers through the unified FFI boundary,
            // omitting `self` from the arg list (class methods are not
            // instances). Gated to value-type codegen so native *actor* class
            // methods (compiled via the native facade with context=Actor) keep
            // their existing lowering.
            let native_class_delegate = matches!(frame.context, CodeGenContext::ValueType)
                && method.is_self_delegate()
                && class.backing_module.is_some();

            // Generate body as Document and keep it in the Document pipeline (ADR 0089).
            let body_doc: Document<'static> = if native_class_delegate {
                frame.set_current_nlr_token(None);
                let backing = class
                    .backing_module
                    .as_ref()
                    .expect("native_class_delegate implies backing_module is Some");
                Self::native_delegate_body_doc(
                    backing.name.as_str(),
                    class.name.name.as_str(),
                    &method.selector,
                    &param_vars,
                )
            } else if method.body.is_empty() {
                frame.set_current_nlr_token(None);
                // Empty class method body returns self (ClassSelf)
                docvec!["ClassSelf"]
            } else {
                // Capture the result so `frame`'s `Drop` (pop scope, clear
                // `in_class_method`, restore the selector) runs before the
                // `?` below propagates an error, same as on the success path.
                let body_stmts_result =
                    frame.lower_class_method_body(method, !class.class_variables.is_empty());
                frame.set_current_nlr_token(None);
                let mut body_stmts = body_stmts_result?;
                // Use class_var_mutated (not just whether class vars are declared)
                // to preserve the {class_var_result, ...} contract. The normal path only wraps
                // in class_var_result when class vars were actually mutated; the NLR path must
                // match. class_var_mutated is set by lower_class_method_body when it sees a
                // class var assignment.
                let returns_class_var_result = frame.class_var_mutated();
                // (ADR 0111 Addendum 4 task 2, closed out
                // for class methods separately): the token was already minted
                // above, before `lower_class_method_body` ran (production's real
                // mint order). `lower_class_method_body` returns a
                // real `Vec<ThreadedStmt>` (a real class-var `Bind` when the
                // body's last statement mutates one — see
                // `lower_class_method_last_class_var_bind`'s doc comment) rather
                // than one opaque `Statement` wrapping an already-rendered
                // `Document` — prepending a real `NlrCatch` here and verifying
                // the whole sequence in one `verify_and_render_body_stmts` call
                // is what lets `VerifyError::ShadowWriteMissing` see a real
                // class-var `Bind` jointly with this real `NlrCatch` for the
                // first time (ADR 0111 Addendum 6's closing note).
                if let Some(ref token_var) = nlr_token_var {
                    body_stmts.insert(
                        0,
                        threaded_ir::ThreadedStmt::NlrCatch {
                            boundary: super::super::NlrBoundary::ClassMethod {
                                has_class_vars: returns_class_var_result,
                            },
                            token: threaded_ir::TokenId::new(token_var.clone()),
                            frame: threaded_ir::FrameId::ROOT,
                            span: method.span,
                        },
                    );
                }
                frame.verify_and_render_body_stmts(&body_stmts, method.span)
            };

            // Build function header with params (Document pieces, not format! —
            // Core Erlang fragments must use the Document API, ADR 0089).
            let doc = docvec![
                "\n",
                fname(safe_class_method_fn_name(&selector_name), arity),
                " = fun (ClassSelf, ClassVars",
                Self::class_method_params_suffix_doc(&param_vars),
                ") ->",
                nest(INDENT, docvec![line(), body_doc,]),
                "\n",
            ];
            docs.push(doc);

            // `frame` drops here, popping the scope, clearing
            // `in_class_method`, and restoring the selector.
        }
        self.class_var_names_mut().clear();
        self.class_method_selectors_mut().clear();
        self.set_class_slot_constructor_selector(None);
        Ok(Document::Vec(docs))
    }

    /// ADR 0084: Lower the `classMethods:` argument of a programmatic
    /// `ClassBuilder` cascade — a map literal whose values are class-method block
    /// literals — into a Core Erlang map whose values are class-method funs.
    ///
    /// Each `#selector => [:self ... | body]` entry becomes
    /// `'selector' => fun (ClassSelf, ClassVars, A1..An) -> ... end`, matching the
    /// compiled `class_<sel>` calling convention so the runtime's fun-dispatch
    /// path installs and invokes it identically. Non-block values, or
    /// blocks whose shape does not match the selector, fall through to ordinary
    /// expression lowering (a computed fun the user supplied).
    ///
    /// `class_var_names` are the keys of the cascade's `classVars:` map; they make
    /// `self.cvar` reads/writes lower as class-variable access (threaded through
    /// `{class_var_result, …}`). `class_name` keys the runtime self/`super`
    /// dispatch the funs emit (they have no module export to call).
    pub(in crate::core_erlang) fn generate_class_methods_map_arg(
        &mut self,
        pairs: &[MapPair],
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        if pairs.is_empty() {
            return Ok(Document::Str("~{}~"));
        }

        // Establish the shared class-method context for every fun in the map,
        // saving/restoring any enclosing class context so a builder cascade
        // inside another class's method (or at the REPL top level) is unaffected.
        let saved = self.enter_builder_class_method_context(class_name, class_var_names);

        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];
        let mut result: Result<()> = Ok(());
        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let key_doc = match self.expression_doc(&pair.key) {
                Ok(d) => d,
                Err(e) => {
                    result = Err(e);
                    break;
                }
            };
            let val_doc = match self.class_method_map_value_doc(&pair.key, &pair.value) {
                Ok(d) => d,
                Err(e) => {
                    result = Err(e);
                    break;
                }
            };
            parts.push(key_doc);
            parts.push(Document::Str(" => "));
            parts.push(val_doc);
        }
        parts.push(Document::Str(" }~"));

        self.exit_builder_class_method_context(saved);
        result?;
        Ok(Document::Vec(parts))
    }

    /// Lower the block argument of an incremental
    /// `addClassMethod: #sel body: [block]` setter into a class-method fun,
    /// mirroring how a `classMethods:` map value is lowered. The `key` is the
    /// selector-symbol argument (used to validate the block's parameter count).
    /// Enters/exits the builder class-method context around the single value so
    /// `self.cvar` access and self/`super` sends lower correctly, exactly as the
    /// map path does for each entry.
    pub(in crate::core_erlang) fn generate_class_method_single_arg(
        &mut self,
        key: &Expression,
        block: &Block,
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        let value = Expression::Block(block.clone());
        let saved = self.enter_builder_class_method_context(class_name, class_var_names);
        let result = self.class_method_map_value_doc(key, &value);
        self.exit_builder_class_method_context(saved);
        result
    }

    /// Lowers a single `classMethods:` map value: a class-method fun for a literal
    /// block of the right shape, else ordinary expression lowering.
    ///
    /// A block literal whose parameter count does not match the
    /// selector (`self` plus one parameter per selector slot) is rejected at
    /// compile time with a [`CodeGenError::BlockArityError`]. Previously such a
    /// block fell through to ordinary expression lowering, producing a fun of the
    /// wrong arity that crashed with an opaque `error:undef` only when the class
    /// method was first called. A computed (non-block) value cannot have its
    /// arity checked at compile time, so it still falls through here and is
    /// validated at registration time (`beamtalk_class_builder:validate_class_method_arities/2`).
    fn class_method_map_value_doc(
        &mut self,
        key: &Expression,
        value: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Literal(Literal::Symbol(sym), _) = key {
            if let Some(selector) = super::super::class_builder_source::selector_from_symbol(sym) {
                if let Expression::Block(block) = value {
                    // A class-method block declares `self` plus one parameter per
                    // selector slot.
                    let expected = selector.arity() + 1;
                    if block.parameters.len() == expected {
                        return self.generate_class_method_fun_from_block(&selector, block);
                    }
                    return Err(CodeGenError::BlockArityError {
                        selector: format!("classMethods: {sym}"),
                        expected: expected.to_string(),
                        actual: block.parameters.len(),
                        hint: format!(
                            "Fix: A classMethods: block takes `self` plus one parameter per \
                             selector argument, so #{sym} needs {expected} parameter(s):\n\
                             \x20 classMethods: #{{ #{sym} => [:self{} | ...] }}",
                            Self::class_method_block_param_example(selector.arity())
                        ),
                    });
                }
            }
        }
        // Computed fun or non-conforming key: lower as an ordinary value. A
        // computed fun's arity is unknown until runtime and is validated at
        // registration time.
        self.expression_doc(value)
    }

    /// Builds the example trailing block parameters (`:a1 :a2 …`) for the
    /// `BlockArityError` hint shown when a `classMethods:` block has the wrong
    /// parameter count. Empty for a unary selector (just `:self`).
    fn class_method_block_param_example(selector_arity: usize) -> String {
        use std::fmt::Write as _;
        let mut out = String::new();
        for i in 1..=selector_arity {
            let _ = write!(out, " :a{i}");
        }
        out
    }

    /// Emits an anonymous class-method fun from a builder block literal.
    ///
    /// `fun (ClassSelf, ClassVars, P1..Pn) -> body` where the block's first
    /// parameter (the receiver) binds to `ClassSelf`, the remaining parameters to
    /// `P1..Pn`, and the body is lowered with the class-method machinery
    /// (`{class_var_result, …}` wrapping; self/`super` routed to runtime dispatch
    /// because there is no `class_<sel>` export). Assumes the caller has already
    /// entered the builder class-method context.
    fn generate_class_method_fun_from_block(
        &mut self,
        selector: &MessageSelector,
        block: &Block,
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.current_method_params.clear();
        // Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();
        self.reset_state_version();
        self.set_class_var_version(0);
        self.set_class_var_mutated(false);
        // ADR 0110: the fun body executes at runtime as a class
        // method's own top frame, even when the builder cascade lexically sits
        // inside a block (`block_depth > 0` at the cascade's position). Reset
        // `block_depth` so `generate_field_assignment`'s shadow-write gate
        // (`block_depth == 0`) uniformly means "the method's own top frame"
        // across compiled methods and ClassBuilder funs alike; restored on
        // every exit path below.
        let saved_block_depth = self.block_depth;
        self.block_depth = 0;

        // The class is reachable via the conventional literal `self` (so
        // `self.cvar` access and self-sends lower correctly — both key on the
        // `self` identifier) and also via the block's receiver parameter under
        // whatever name it was declared.
        self.bind_var("self", "ClassSelf");
        if let Some(receiver_param) = block.parameters.first() {
            self.bind_var(&receiver_param.name, "ClassSelf");
        }
        // Remaining parameters become the fun's user parameters P1..Pn.
        let param_vars: Vec<String> = block.parameters[1..]
            .iter()
            .map(|bp| {
                let var_name = self.fresh_var(&bp.name);
                self.current_method_params.push(var_name.clone());
                var_name
            })
            .collect();

        // Synthesize a MethodDefinition so the shared class-method body lowering
        // applies unchanged.
        let params: Vec<ParameterDefinition> = block.parameters[1..]
            .iter()
            .map(|bp| ParameterDefinition::new(Identifier::new(bp.name.clone(), bp.span)))
            .collect();
        let method =
            MethodDefinition::new(selector.clone(), params, block.body.clone(), block.span);

        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&block.span, &block.body);
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let has_class_vars = !self.class_var_names().is_empty();
        let body_doc: Document<'static> = if method.body.is_empty() {
            self.set_current_nlr_token(None);
            docvec!["ClassSelf"]
        } else {
            let mut body_stmts = match self.lower_class_method_body(&method, has_class_vars) {
                Ok(stmts) => stmts,
                Err(e) => {
                    self.set_current_nlr_token(None);
                    self.block_depth = saved_block_depth;
                    self.pop_scope();
                    return Err(e);
                }
            };
            self.set_current_nlr_token(None);
            let returns_class_var_result = self.class_var_mutated();
            // Same real-`NlrCatch`-prepend pattern as
            // `generate_class_method_functions` — see that call site's
            // comment for why this replaces the old
            // `wrap_class_method_body_with_nlr_catch` Document-wrap.
            if let Some(ref token_var) = nlr_token_var {
                body_stmts.insert(
                    0,
                    threaded_ir::ThreadedStmt::NlrCatch {
                        boundary: super::super::NlrBoundary::ClassMethod {
                            has_class_vars: returns_class_var_result,
                        },
                        token: threaded_ir::TokenId::new(token_var.clone()),
                        frame: threaded_ir::FrameId::ROOT,
                        span: method.span,
                    },
                );
            }
            self.verify_and_render_body_stmts(&body_stmts, method.span)
        };

        let doc = docvec![
            "fun (ClassSelf, ClassVars",
            Self::class_method_params_suffix_doc(&param_vars),
            ") ->",
            nest(INDENT, docvec![line(), body_doc]),
        ];

        self.block_depth = saved_block_depth;
        self.pop_scope();
        Ok(doc)
    }

    /// Builds the trailing fun parameter list `, P1, P2, …` as `Document` pieces
    /// (never `format!` — Core Erlang fragments must use the Document API,
    /// ADR 0089). Empty when there are no user parameters.
    fn class_method_params_suffix_doc(param_vars: &[String]) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for var in param_vars {
            parts.push(Document::Str(", "));
            parts.push(leaf::var(var.clone()));
        }
        Document::Vec(parts)
    }

    /// Lowers the body of a class-side method to one straight-line
    /// `Vec<ThreadedStmt>` (ADR 0111 Addendum 4/6: mirrors
    /// `lower_body_exprs_with_reply`'s pattern for the Actor pipeline, for
    /// the class-method body pipeline the Actor-pipeline migration explicitly
    /// left as a hand-written `Document` builder).
    ///
    /// Unlike instance methods, class methods have no `State` threading —
    /// the only version-mutating construct a class method's own body can
    /// directly produce is a class-var write (`self.classVar := value`, ADR
    /// 0110's `ClassVars` counter). Every other body statement — local-var
    /// bindings, destructuring, `^`-returns, class-method self-sends (whose
    /// own class-var rebind, if any, is produced by the shared
    /// `emit_class_var_result_unwrap` helper and stays opaque here — the
    /// same "mutation hidden inside a shared multi-module helper" treatment
    /// given to `generate_self_dispatch_open` et al. in the Actor
    /// pipeline) — is an opaque [`threaded_ir::ThreadedStmt::Statement`]
    /// built by the SAME `generate_class_method_*` codegen calls production
    /// used before this migration (byte-identity: only the container
    /// changed, from `Vec<Document>` to `Vec<ThreadedStmt>`).
    ///
    /// The ONE case promoted to a real [`threaded_ir::ThreadedStmt::Bind`]
    /// is a class method's own direct `self.classVar := value` when it is
    /// the body's *last* statement (implicit return) — mirroring exactly
    /// how the Actor pipeline only promotes
    /// `BodyExprKind::FieldAssignment` to a real `Bind` in its `is_last`
    /// arm (`lower_body_exprs_with_reply`), leaving every other position's
    /// field mutation inside a shared helper's opaque `Statement`. This is
    /// the ADR 0110 joint-visibility case this issue exists to close: once
    /// the caller (`generate_class_method_functions`/
    /// `generate_class_method_fun_from_block`) prepends a real `NlrCatch`,
    /// this `Bind` and that `NlrCatch` are visible to the SAME `verify()`
    /// call for the first time — see `lower_class_method_last_class_var_bind`'s
    /// own doc comment for why the pre-existing isolated
    /// `construct_and_verify_class_var_bind` check stays alongside the new
    /// joint one rather than being replaced by it.
    ///
    /// When a class-var write happened anywhere in the body
    /// (`class_var_mutated()`), the caller wraps the final result in
    /// `{class_var_result, Result, ClassVarsN}` — unchanged, decided after
    /// this function returns, exactly as before.
    fn lower_class_method_body(
        &mut self,
        method: &MethodDefinition,
        has_class_vars: bool,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        use threaded_ir::ThreadedStmt;

        let mut stmts: Vec<ThreadedStmt> = Vec::new();

        // Filter out @expect directives (compile-time only, no runtime representation).
        let body = super::super::util::collect_body_exprs(&method.body);
        let body_len = body.len();
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body_len - 1;
            let span = expr.span();

            if let Expression::Return { value, .. } = expr {
                let doc = self.generate_class_method_return(value, has_class_vars)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
                return Ok(stmts);
            }

            if is_last && has_class_vars && self.is_class_var_assignment(expr) {
                self.lower_class_method_last_class_var_bind(&mut stmts, expr, span)?;
            } else if is_last {
                let doc = self.generate_class_method_last_expr(expr, has_class_vars)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
            } else if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
                // ADR 0118 phase 5a: splice the real `ClassVars`
                // prelude instead of wrapping one opaque `Statement` around
                // an already-rendered open-Document (`generate_class_method_non_last_expr`'s
                // old branch for this same condition) — every non-last
                // class-var mutation is now a genuine, verified `Bind` in
                // this body's own IR, closing the ADR 0111 Addendum 6 gap
                // `verify_body_with_opaque_version_gaps`'s `ClassVars`
                // backfill used to paper over for class methods. The
                // statement's own value is discarded (matching the old
                // behaviour: nothing here ever referenced it), so only the
                // prelude is spliced.
                let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                stmts.extend(tv.prelude);
            } else {
                let doc = self.generate_class_method_non_last_expr(expr)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
            }
        }
        Ok(stmts)
    }

    /// Constructs the real `ThreadedStmt::Bind` for a class
    /// method's own `self.classVar := value` when it is the body's last
    /// statement — the shape `lower_class_method_body` promotes out of the
    /// generic `generate_class_method_last_expr_with_class_vars` path.
    /// Delegates the actual `Bind` construction to the shared
    /// [`Self::lower_class_var_field_assignment_bind`] (`expressions.rs`;
    /// same struct, `impl` block in a different file — the identical
    /// sequence `expressions.rs::generate_class_var_field_assignment`
    /// builds for every OTHER position, not hand-rolled a second time here,
    /// CLAUDE.md's no-duplicate-implementations rule), but — unlike that
    /// call site, which still renders its `Bind` immediately and keeps it
    /// inside an opaque `Statement` — pushes the returned `Bind` into
    /// `stmts` as a real IR node, so the method's real `NlrCatch`
    /// (prepended by the caller after this function returns) and this
    /// `Bind` are both visible to the single `verify_and_render_body_stmts`
    /// call over the whole body, closing the ADR 0110 joint-visibility gap
    /// ADR 0111 Addendum 6 left open for class methods.
    ///
    /// The isolated, synthetic-marker `ShadowWriteMissing` check the shared
    /// helper runs internally is deliberately still reported (not dropped
    /// in favor of the new joint check): it is the ONLY check that still
    /// fires for a method with no literal `^` at all (`needs_nlr: false`,
    /// so no real `NlrCatch` in the body at all) — the exact ADR 0110
    /// `CollectionDriver countedRun:over:` repro shape (the mutation must
    /// still be shadow-written even though this specific method never
    /// mints a local NLR catch, because the relay can happen one layer out
    /// via a caller-supplied block) — so dropping it would regress
    /// coverage the joint check cannot replace. The two checks are
    /// complementary, not redundant: the isolated one always assumes the
    /// worst case; the joint one is precise when a real `NlrCatch` is
    /// actually present.
    fn lower_class_method_last_class_var_bind(
        &mut self,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
        expr: &Expression,
        span: Span,
    ) -> Result<()> {
        let (field_name, value) = match expr {
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::FieldAccess { field, .. } => (field.name.to_string(), value.as_ref()),
                _ => unreachable!(
                    "is_class_var_assignment guarantees an Assignment with a FieldAccess target"
                ),
            },
            _ => unreachable!("is_class_var_assignment guarantees an Assignment"),
        };

        let (preamble_doc, bind, val_var) = self.lower_class_var_field_assignment_bind(
            &field_name,
            value,
            threaded_ir::FrameId::ROOT,
        )?;

        let final_cv = self.current_class_var();
        stmts.push(threaded_ir::ThreadedStmt::Statement(preamble_doc, span));
        stmts.push(bind);
        stmts.push(threaded_ir::ThreadedStmt::Statement(
            docvec![
                "{'class_var_result', ",
                leaf::var(val_var),
                ", ",
                leaf::var(final_cv),
                "}",
            ],
            span,
        ));
        Ok(())
    }

    /// Generates code for an explicit `^` return in a class method.
    fn generate_class_method_return(
        &mut self,
        value: &Expression,
        has_class_vars: bool,
    ) -> Result<Document<'static>> {
        // An explicit `^` return of a value-type threading construct
        // (counted/while loop, foldl list-op, or read+write conditional) must
        // unwrap the construct's logical value rather than leak the raw
        // `{value, StateAcc}` tuple (or crash dispatching a read+write
        // conditional's stateful block at the wrong arity). This mirrors the
        // implicit last-expression path (`generate_class_method_last_expr`);
        // the shared helper applies the `{class_var_result, …}` wrapping based on
        // `class_var_mutated()`, identical to the wrapping below — so it is
        // correct for both the class-vars and no-class-vars cases.
        if let Some(doc) = self.try_generate_class_method_threaded_last(
            value,
            super::super::threaded_expr::ThreadingPosition::Return,
        )? {
            return Ok(doc);
        }
        // ADR 0118 phase 5b: mirrors
        // `generate_class_method_last_expr_with_class_vars` — when `value`
        // is itself a recognized producer (a class-var assignment, or a
        // *locally declared* class-method self-send per
        // `is_class_method_self_send`'s `class_method_selectors()` check),
        // `threaded_expression` gives it a real prelude whose rebound
        // `ClassVarsN` stays lexically visible here, so `current_class_var()`
        // below is safe to read directly. Otherwise `value` may still
        // dispatch a class-var-mutating self-send that the compile below
        // reaches opaquely and closes (e.g. inherited dispatch,
        // deliberately excluded by that same check) — closing loses the
        // mutated name's LEXICAL visibility, but not the mutation itself,
        // so `refresh_class_var_after_opaque_scope` recovers the live value
        // via the ADR 0110 shadow write instead of relying on lexical scope.
        if has_class_vars {
            if self.is_class_var_assignment(value) || self.is_class_method_self_send(value) {
                let result_var = self.fresh_temp_var("Ret");
                let frame = self.current_frame();
                let tv = self.threaded_expression(value, frame)?;
                let preamble = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                if self.class_var_mutated() {
                    let final_cv = self.current_class_var();
                    Ok(docvec![
                        preamble,
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        value_doc,
                        " in {'class_var_result', ",
                        leaf::var(result_var),
                        ", ",
                        leaf::var(final_cv),
                        "}",
                    ])
                } else {
                    Ok(docvec![
                        preamble,
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        value_doc,
                        " in ",
                        leaf::var(result_var),
                    ])
                }
            } else {
                let result_var = self.fresh_temp_var("Ret");
                let cv_version_before = self.class_var_version();
                let expr_doc = self.expression_doc(value)?;
                let refresh = self.refresh_class_var_after_opaque_scope(cv_version_before);
                if self.class_var_mutated() {
                    let final_cv = self.current_class_var();
                    Ok(docvec![
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        expr_doc,
                        " in ",
                        refresh.unwrap_or(Document::Nil),
                        "{'class_var_result', ",
                        leaf::var(result_var),
                        ", ",
                        leaf::var(final_cv),
                        "}",
                    ])
                } else {
                    Ok(docvec![
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        expr_doc,
                        " in ",
                        leaf::var(result_var),
                    ])
                }
            }
        } else {
            // ADR 0118 phase 5b: same treatment for the
            // no-class-vars path — no `class_var_mutated()`/`current_class_var()`
            // read follows, so the prelude and value simply concatenate.
            let frame = self.current_frame();
            self.threaded_expression_doc(value, frame)
        }
    }

    /// Generates code for the last expression in a class method body.
    fn generate_class_method_last_expr(
        &mut self,
        expr: &Expression,
        has_class_vars: bool,
    ) -> Result<Document<'static>> {
        // A last-position threading construct (counted/while loop or foldl list-op
        // yielding a `{value, StateAcc}` tuple) or a read+write conditional must unwrap the
        // construct's logical value rather than leak the raw tuple (or crash on the 0-arg
        // stateful-block dispatch). Handled here because the `{class_var_result, ...}` wrapping
        // is identical whether or not the class declares class vars — threading constructs
        // mutate *locals*, not class vars, so the wrapping is driven solely by whether an
        // earlier statement mutated a class var (`class_var_mutated()`).
        if let Some(doc) = self.try_generate_class_method_threaded_last(
            expr,
            super::super::threaded_expr::ThreadingPosition::Last,
        )? {
            return Ok(doc);
        }
        if has_class_vars {
            self.generate_class_method_last_expr_with_class_vars(expr)
        } else {
            self.generate_class_method_last_expr_no_class_vars(expr)
        }
    }

    /// Handles a class method's last expression when it is a value-type threading
    /// construct (counted/while loop or foldl list-op) or a read+write conditional.
    ///
    /// Returns `None` when `expr` is neither, so the caller falls back to the standard
    /// last-expression paths.
    ///
    /// Both shapes produce a logical value bound to a fresh result var (via the shared
    /// value-type primitives), which is then wrapped in `{class_var_result, Result, ClassVarsN}`
    /// when an earlier statement mutated a class var, or returned bare otherwise.
    fn try_generate_class_method_threaded_last(
        &mut self,
        expr: &Expression,
        position: super::super::threaded_expr::ThreadingPosition,
    ) -> Result<Option<Document<'static>>> {
        // Route through the shared `ThreadedExpr` transform + boundary emitter.
        // It peels redundant parentheses (e.g. `^(items collect: …)` or
        // `(flag ifTrue: [...])`) so the threading construct inside is unwrapped to its
        // logical value rather than leaking its raw `{value, StateAcc}` tuple. Applies to
        // both the explicit `^`-return and the implicit last-expression callers.
        let mut parts: Vec<Document<'static>> = Vec::new();
        if self.emit_threaded_last(
            expr,
            position,
            super::super::threaded_expr::ThreadingBoundary::ClassMethod,
            &mut parts,
        )? {
            Ok(Some(Document::Vec(parts)))
        } else {
            Ok(None)
        }
    }

    /// Last expression with class vars: may need `{class_var_result, ...}` wrapping.
    fn generate_class_method_last_expr_with_class_vars(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let frame = self.current_frame();
        if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
            // ADR 0118 phase 5b: `expr` is itself a producer at
            // its own top level, so `threaded_expression` always gives it a
            // real value (never the do:-in-direct-params-loop `'nil'` case
            // — a class-var assignment/self-send never produces that).
            // `final_cv` is read AFTER threading so it reflects the rebind.
            let tv = self.threaded_expression(expr, frame)?;
            let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
            let value_doc = self.threaded_value_doc(&tv.value);
            let final_cv = self.current_class_var();
            Ok(docvec![
                prelude_doc,
                "{'class_var_result', ",
                value_doc,
                ", ",
                leaf::var(final_cv),
                "}",
            ])
        } else {
            // `expr` is not ITSELF a recognized producer at this level, but
            // may still dispatch one that the compile below reaches
            // opaquely and closes (e.g. a same-class self-send NOT declared
            // locally — inherited dispatch — which
            // `is_class_method_self_send`'s `class_method_selectors()`
            // check deliberately excludes, per its own doc comment, since
            // `try_handle_class_method_self_send`'s real reach is any
            // `self`-receiver send regardless of selector). Closing loses
            // the mutated `ClassVarsN` name's LEXICAL visibility, but not
            // the mutation itself — `refresh_class_var_after_opaque_scope`
            // recovers the live value via the ADR 0110 shadow write rather
            // than relying on lexical scope, so this is robust to whatever
            // depth/shape the opaque compile below reaches.
            let result_var = self.fresh_temp_var("Ret");
            let cv_version_before = self.class_var_version();
            let expr_doc = self.expression_doc(expr)?;
            let refresh = self.refresh_class_var_after_opaque_scope(cv_version_before);
            if self.class_var_mutated() {
                let final_cv = self.current_class_var();
                Ok(docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in ",
                    refresh.unwrap_or(Document::Nil),
                    "{'class_var_result', ",
                    leaf::var(result_var),
                    ", ",
                    leaf::var(final_cv),
                    "}",
                ])
            } else {
                Ok(docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in ",
                    leaf::var(result_var),
                ])
            }
        }
    }

    /// Last expression without class vars: simpler wrapping.
    fn generate_class_method_last_expr_no_class_vars(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // ADR 0118 phase 5b: no `class_var_result` wrapping and no
        // later read of `current_class_var()` follows either branch below,
        // so both the bare self-send case and the general case
        // collapse to the same plain threaded compile.
        let frame = self.current_frame();
        self.threaded_expression_doc(expr, frame)
    }

    /// Generates code for a non-last expression in a class method body.
    ///
    /// ADR 0118 phase 5a: a class-var assignment or class-method
    /// self-send is intercepted one level up, in `lower_class_method_body`,
    /// which splices its real `ClassVars` prelude directly instead of
    /// calling this function — so this function's own callers never reach
    /// it with either of those shapes any more.
    fn generate_class_method_non_last_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if Self::is_local_var_assignment(expr) {
            self.generate_class_method_local_var_binding(expr)
        } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let binding_docs = self.generate_destructure_bindings(pattern, value)?;
            Ok(Document::Vec(binding_docs))
        } else if self.is_do_with_vt_local_threading(expr) {
            // Non-last `do:` loop that mutates captured outer locals.
            self.generate_value_type_do_open(expr)
        } else if self.is_counted_loop_with_vt_local_threading(expr) {
            // Non-last counted loop (to:do:/to:by:do:/timesRepeat:) that
            // mutates captured outer locals. Extracts the threaded locals from the
            // `{'nil', StateAcc}` tuple so subsequent statements see the updates.
            self.generate_vt_counted_loop_open(expr)
        } else if self.is_while_with_vt_local_threading(expr) {
            // Non-last whileTrue:/whileFalse: that mutates captured outer locals.
            self.generate_vt_while_open(expr)
        } else if self.is_foldl_list_op_with_vt_local_threading(expr) {
            // Non-last collect:/select:/reject:/inject:into: that mutates captured
            // outer locals. Extracts the threaded locals from the `{value, StateAcc}` tuple
            // (the logical value is discarded in non-last position).
            self.generate_vt_foldl_list_op_open(expr)
        } else if self.is_conditional_with_vt_local_threading(expr) {
            // Non-last conditional that mutates captured outer locals.
            self.generate_vt_conditional_open(expr)
        } else if self.is_exception_construct_with_vt_local_threading(expr) {
            // Non-last on:do:/ensure: that mutates captured outer
            // locals. Extracts the threaded locals from the returned
            // `{Result, StateAcc}` tuple, same idiom as the loop/conditional
            // arms above.
            self.generate_vt_exception_construct_open(expr)
        } else {
            // `expr` may dispatch a class-method self-send (locally
            // declared or inherited) that rebinds `ClassVarsN`
            // opaquely, closed by the time this call returns —
            // `refresh_class_var_after_opaque_scope` recovers the live
            // value via the ADR 0110 shadow write (rather than relying on
            // lexical scope) so the NEXT statement in this same body — which
            // reads `current_class_var()` when it builds its own call —
            // sees it regardless of nesting depth. Bind the result to the
            // seq temp so subsequent code can sequence after it.
            let tmp_var = self.fresh_temp_var("seq");
            let cv_version_before = self.class_var_version();
            let expr_doc = self.expression_doc(expr)?;
            let refresh = self
                .refresh_class_var_after_opaque_scope(cv_version_before)
                .unwrap_or(Document::Nil);
            Ok(docvec![
                "let ",
                leaf::var(tmp_var),
                " = ",
                expr_doc,
                " in ",
                refresh,
            ])
        }
    }

    /// Local variable assignment in class method — create a proper `let` binding.
    fn generate_class_method_local_var_binding(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                // When the RHS is a threading construct (value-type loop /
                // foldl list-op yielding `{value, StateAcc}`) or a read+write conditional
                // (each branch emits `{LogicalValue, Mut1..MutN}`), bind the target to the
                // logical value and rebind the threaded siblings — rather than binding the
                // target to the raw tuple. Shared with the value-type instance-method
                // body sequencer via `emit_threaded_assign_rhs`.
                let mut parts: Vec<Document<'static>> = Vec::new();
                if self
                    .emit_threaded_assign_rhs(&id.name, value, &mut parts)?
                    .is_some()
                {
                    return Ok(Document::Vec(parts));
                }
                let var_name = &id.name;
                let core_var = self
                    .lookup_var(var_name)
                    .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                // Captured before generating `value` — a
                // class-method self-send inside it (locally declared or
                // inherited — `is_class_method_self_send`'s
                // `class_method_selectors()` check only recognizes the
                // former) may rebind `ClassVarsN` opaquely, closed by the
                // time this call returns; `refresh_class_var_after_opaque_scope`
                // recovers the live value via the ADR 0110 shadow write
                // rather than relying on lexical scope, so this is robust
                // to whatever depth/shape the compile below reaches.
                let cv_version_before = self.class_var_version();
                let val_doc = self.expression_doc(value)?;
                self.bind_var(var_name, &core_var);
                let refresh = self
                    .refresh_class_var_after_opaque_scope(cv_version_before)
                    .unwrap_or(Document::Nil);
                return Ok(docvec![
                    "let ",
                    leaf::var(core_var),
                    " = ",
                    val_doc,
                    " in ",
                    refresh,
                ]);
            }
        }
        Ok(Document::Nil)
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
            beamtalk_core::unparse::unparse_method(&stripped)
        } else {
            beamtalk_core::unparse::unparse_method(method)
        }
    }

    /// Checks if an expression is a `value:` call on a Tier 2 block parameter.
    ///
    /// When true, the expression will generate a `{Result, NewState}` tuple via
    /// `generate_block_value_call_stateful()` and must be unpacked by the caller.
    pub(in crate::core_erlang) fn is_tier2_value_call(&self, expr: &Expression) -> bool {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let (is_positional_value_selector, is_value_with_arguments) = match selector {
                beamtalk_core::ast::MessageSelector::Unary(name) => (name == "value", false),
                beamtalk_core::ast::MessageSelector::Keyword(parts) => {
                    let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    (
                        matches!(
                            selector_name.as_str(),
                            "value:" | "value:value:" | "value:value:value:"
                        ),
                        selector_name == "valueWithArguments:",
                    )
                }
                beamtalk_core::ast::MessageSelector::Binary(_) => (false, false),
            };
            if is_positional_value_selector || is_value_with_arguments {
                // Tier 2 block parameter (variable holding a stateful block)
                // Or a local variable this method itself assigned a Tier 2
                // block literal to earlier in its own body (tier2_local_vars).
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if self.tier2_block_params.contains(id.name.as_str())
                        || self.tier2_local_vars.contains(id.name.as_str())
                    {
                        return true;
                    }
                }
                // `self.field value(:...)` — the field may hold a Tier 2
                // block assigned from a different method than this call site, so
                // it needs the runtime is_function/2 discrimination generated by
                // generate_block_value_call_runtime_discriminated, which always
                // returns a {Result, NewState} tuple that this call site must
                // unpack (same as the statically-known-Tier-2 cases above).
                // `valueWithArguments:` gets the same treatment via
                // generate_block_value_with_arguments_call_runtime_discriminated.
                if self.context == super::super::CodeGenContext::Actor
                    && Self::is_self_field_access(receiver)
                {
                    return true;
                }
            }
            // Literal-block-with-mutations receivers stay scoped to the
            // positional value/value:/... selectors — generate_block_value_inline_with_mutations
            // binds `arguments` directly to the block's own parameters, which
            // doesn't hold for valueWithArguments: (a single runtime list, not
            // per-parameter positional args). Not a motivating shape here.
            if is_positional_value_selector {
                // Inline block literal with captured mutations
                // (e.g. [errors := errors add: #foo] value)
                // Only in Actor/REPL context — ValueType inlines as plain value (no tuple).
                if let Expression::Block(block) = receiver.as_ref() {
                    if self.context != super::super::CodeGenContext::ValueType
                        && !Self::captured_mutations_for_block(block).is_empty()
                    {
                        return true;
                    }
                    // Block literal with field mutations (actor state threading)
                    if self.context != super::super::CodeGenContext::ValueType {
                        let analysis = super::super::block_analysis::analyze_block(block);
                        if self.needs_mutation_threading(&analysis) {
                            return true;
                        }
                    }
                }
            }
        }
        // `blk value: x; value: y` — a cascade where every message
        // (including the one the parser folds into `receiver` — see
        // `normalize_cascade`) is itself a safe value-family send on a receiver
        // that (by the same rules as the single-send case above) may hold a
        // Tier 2 block. Each message needs the same tuple-unpacking treatment
        // as a single Tier2ValueCall, sequenced through
        // `generate_tier2_cascade_doc`.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            let all_safe_value_sends = !all_messages.is_empty()
                && all_messages
                    .iter()
                    .all(|(sel, _)| Self::is_safe_value_family_selector(sel));
            if all_safe_value_sends {
                if let Expression::Identifier(id) = underlying_receiver {
                    if self.tier2_block_params.contains(id.name.as_str())
                        || self.tier2_local_vars.contains(id.name.as_str())
                    {
                        return true;
                    }
                }
                if self.context == super::super::CodeGenContext::Actor
                    && Self::is_self_field_access(underlying_receiver)
                {
                    return true;
                }
            }
        }
        false
    }

    /// `true` when a `match:` needs state threading — i.e. at least one arm's
    /// body either is a Tier 2 value-call (most commonly a state-mutating
    /// `[...] value` block), is itself a nested control-flow-with-mutations
    /// construct (`ifTrue:`/`ifFalse:`/a nested `match:`/etc. with no
    /// `[...] value` wrapper, e.g. `nil -> flag ifTrue: [self.x := 1]`), or
    /// is a bare `self.field := ...` write, parentheses aside (BT-3489).
    /// `generate_match` checks this
    /// once per `match:` and, when true, compiles every arm's body to a uniform
    /// `{Value, State}` shape so the whole expression can be unwrapped by the
    /// same machinery as `ifTrue:`/`ifFalse:` mutations
    /// (`control_flow_has_mutations`'s `Expression::Match` branch above — this
    /// function is mutually recursive with it, which is what lets a nested
    /// `match:` arm body be detected too).
    ///
    /// # Context gating (BT-3489)
    ///
    /// The Tier 2 / nested-control-flow / hoistable-self-send disjuncts are
    /// Actor-only: each is about actor `State` threading or an actor self-send,
    /// neither of which exists for a value type.
    ///
    /// The field-write disjunct (`threads_fields`) additionally covers a
    /// value-type CLASS method, where `self.x :=` is a CLASS-var write on the
    /// `ClassVars` chain: threading it routes the arm through
    /// `lower_field_assignment_bind`'s shared `reject_class_var_field_assignment`
    /// gate and yields a clean
    /// [`CodeGenError::ClassVarAssignmentInThreadedBody`]. Without it that arm
    /// compiled through plain `expression_doc` into a second arm reading the
    /// first arm's `ClassVars1` binding — `erlc: unbound variable 'ClassVars1'`,
    /// the class-var sibling of this issue's `State1`/`Self1` crash.
    ///
    /// A value-type INSTANCE method is the one field-writing context excluded:
    /// its `Self`/`SelfN` chain (`VersionPrefix::SelfVt`) has no N-arm merge at
    /// all, so `generate_match` rejects that shape up front as
    /// [`CodeGenError::ValueSelfFieldAssignmentInMatchArm`] rather than
    /// threading it.
    ///
    /// [`CodeGenError::ValueSelfFieldAssignmentInMatchArm`]: super::super::CodeGenError::ValueSelfFieldAssignmentInMatchArm
    /// [`CodeGenError::ClassVarAssignmentInThreadedBody`]: super::super::CodeGenError::ClassVarAssignmentInThreadedBody
    pub(in crate::core_erlang) fn match_needs_mutation_threading(
        &self,
        arms: &[beamtalk_core::ast::MatchArm],
    ) -> bool {
        let is_actor = self.context == super::super::CodeGenContext::Actor;
        // A field write in a `match:` arm can only be merged back where there
        // is an N-arm-capable version chain to merge it into: an actor's
        // `State` (either method kind), or a class method's `ClassVars`. The
        // remaining case — a value-type INSTANCE method's `Self` chain — has
        // no such merge, so it is excluded here rather than left to depend on
        // `generate_match`'s up-front
        // [`CodeGenError::ValueSelfFieldAssignmentInMatchArm`] rejection
        // running first: that rejection is what users see, but this gate stays
        // correct on its own if it ever moves.
        let threads_fields = is_actor
            || (matches!(self.context, super::super::CodeGenContext::ValueType)
                && self.in_class_method());
        arms.iter().any(|arm| {
            // BT-3489: a `self.field := ...` arm body. Before this, nothing
            // here matched it, so `generate_match` left `base_state` as
            // `None` and the arm compiled through plain `expression_doc`,
            // whose field-write binding (`State1`/`ClassVars1`) is scoped to
            // that one `case` arm — yet the code after the `match:` referenced
            // it unconditionally, so `erlc` rejected the module outright.
            (threads_fields && Self::is_field_assignment(arm.body.unwrap_parens()))
                || (is_actor
                    && (self.is_tier2_value_call(&arm.body)
                        || self.control_flow_has_mutations(&arm.body)
                        // (ADR 0118 phase 4): an arm body that is
                        // neither a Tier 2 block-value call nor itself a nested
                        // control-flow-with-mutations construct, but DOES
                        // contain a (possibly nested, hoistable) actor
                        // self-send — `1 -> 1 + (self bumpCount)` — still needs
                        // this `match:` threaded, so `generate_match_arm_body`'s
                        // plain-wrap arm gets a chance to hoist it instead of
                        // silently dropping the mutation via a bare
                        // `expression_doc` compile.
                        || self.conditional_receiver_needs_threading(&arm.body)))
        })
    }

    /// Returns captured mutation variable names for a Tier 2
    /// value-call statement (`expr` already classified/proven
    /// `BodyExprKind::Tier2ValueCall` by `is_tier2_value_call`) whose receiver
    /// mutates outer locals, so the caller can rebind them after the call.
    ///
    /// Handles both:
    /// - An inline block literal receiver (`[block] value`/`value:`/... —
    ///   the original scope), via `captured_mutations_for_block` on
    ///   the literal directly.
    /// - A NAMED `tier2_local_vars` identifier receiver (`blk value:
    ///   x`) whose block literal was assigned earlier in the same method —
    ///   the call site only has the identifier, not the block AST, so this
    ///   looks up the mutations `prescan_tier2_local_vars` already recorded
    ///   for that variable name in `tier2_local_var_captured_mutations`.
    ///
    /// Also handles a `Cascade` expression (`blk value: x; value: x`) by
    /// normalizing to its true underlying receiver first — the same
    /// receiver-shape checks then apply.
    //
    // Widened from private to
    // `pub(in crate::core_erlang)` so `control_flow/conditionals.rs`
    // can rebind captured local-var mutations for a bare `Tier2ValueCall`
    // statement inside a conditional branch, mirroring this file's own
    // `Tier2ValueCall` handling.
    pub(in crate::core_erlang) fn get_inline_block_captured_mutations(
        &self,
        expr: &Expression,
    ) -> Option<Vec<String>> {
        let receiver = match expr {
            Expression::MessageSend { receiver, .. } => receiver.as_ref(),
            Expression::Cascade {
                receiver, messages, ..
            } => Self::normalize_cascade(receiver, messages).0,
            _ => return None,
        };
        if let Expression::Identifier(id) = receiver {
            if let Some(mutations) = self
                .tier2_local_var_captured_mutations
                .get(id.name.as_str())
            {
                return Some(mutations.clone());
            }
        }
        Self::inline_block_captured_mutations(expr)
    }

    /// Generates the `Document` for an
    /// expression already classified as `BodyExprKind::Tier2ValueCall` or the
    /// RHS of `BodyExprKind::LocalAssignTier2` — i.e. a `value`/`value:`/etc.
    /// send that `is_tier2_value_call` proved needs Tier 2 tuple-unpacking
    /// treatment.
    ///
    /// When the receiver is a `self.field` access, this calls
    /// `generate_block_value_call_runtime_discriminated` directly instead of
    /// going through the generic `expression_doc` dispatch. That function is
    /// deliberately NOT reachable from `expression_doc` (see the matching
    /// comment on it and in `intrinsics.rs`'s `try_generate_block_value_unary`/
    /// `try_generate_block_value_keyword`): every call site of *this* helper
    /// unpacks the `{Result, NewState}` tuple it returns, but an arbitrary
    /// sub-expression reached via plain `expression_doc` would not, silently
    /// handing the raw tuple to code expecting a plain value.
    ///
    /// For every other `Tier2ValueCall` shape (a `tier2_block_params`/
    /// `tier2_local_vars` identifier receiver, or an inline literal block with
    /// captured/field mutations), falls through to `expression_doc`, which
    /// already handles those correctly.
    ///
    /// Also called from `control_flow/mod.rs`'s
    /// `generate_local_var_assignment_in_loop` (the `is_tier2_value_call`
    /// branch there) for the same reason: it unpacks a
    /// `{Result, NewState}` tuple, so it must reach the same
    /// runtime-discriminated codegen for a `self.field` receiver.
    ///
    /// `valueWithArguments:` has no compile-time-known-Tier-2
    /// "stateful" fast path the way `value`/`value:` do
    /// (`generate_block_value_call_stateful`) — `is_tier2_value_call` only
    /// ever proves a `valueWithArguments:` send needs Tier 2 handling at
    /// all, never which arity branch statically applies, so every match
    /// (`self.field`, `tier2_block_params`, `tier2_local_vars`) routes
    /// through the same runtime-discriminated codegen here, unconditionally
    /// — unlike the positional selectors' `self.field`-only special case
    /// below.
    pub(in crate::core_erlang) fn generate_tier2_value_call_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if selector.name() == "valueWithArguments:" {
                // The parser always gives a keyword message at least one
                // argument, so `arguments` is never empty here — but fail
                // loudly instead of silently falling through to the
                // `self.field` positional-value: branch below, which would
                // emit a malformed 0-arity value call for this selector.
                let args_expr = arguments.first().ok_or_else(|| {
                    CodeGenError::Internal(
                        "valueWithArguments: with no argument expression".to_string(),
                    )
                })?;
                return self.generate_block_value_with_arguments_call_runtime_discriminated(
                    receiver, args_expr,
                );
            }
            if self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver) {
                return self.generate_block_value_call_runtime_discriminated(
                    receiver,
                    arguments,
                    &selector.name(),
                );
            }
            // `tier2_local_vars`/`tier2_block_params` receivers must
            // also bypass `expression_doc` below — since `try_generate_block_value_unary`/
            // `try_generate_block_value_keyword` now intercept this same
            // receiver shape from the generic sub-expression dispatch and
            // (correctly, for THAT position) close the tuple down to just
            // `Result`. Calling `generate_block_value_call_stateful` directly
            // here, exactly like the `self.field` case above, keeps this
            // TOP-LEVEL statement path getting the raw `{Result, NewState}`
            // tuple its callers (`BodyExprKind::Tier2ValueCall` handling in
            // this file, `conditionals.rs`, `control_flow/mod.rs`) unpack.
            if let Expression::Identifier(id) = receiver.as_ref() {
                if self.tier2_block_params.contains(id.name.as_str())
                    || self.tier2_local_vars.contains(id.name.as_str())
                {
                    return self.generate_block_value_call_stateful(receiver, arguments);
                }
            }
        }
        // `blk value: x; value: y` — proved safe by `is_tier2_value_call`'s
        // Cascade branch. Unlike the single-send case, `expression_doc` has no
        // generic Tier 2 cascade handling to fall through to, so this must be
        // generated directly regardless of receiver kind.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            return self.generate_tier2_cascade_doc(underlying_receiver, &all_messages);
        }
        self.expression_doc(expr)
    }

    /// Generates a sequential Tier 2 tuple-unpacking cascade
    /// (`blk value: x; value: y`), reached only for a `Cascade` expression that
    /// `is_tier2_value_call` already proved is entirely safe `value`-family
    /// sends on a receiver that may hold a Tier 2 block.
    ///
    /// The receiver is evaluated once per message rather than hoisted into a
    /// single shared binding — harmless here since a `tier2_block_params`/
    /// `tier2_local_vars`/`self.field` receiver is always a pure variable or
    /// field read, never an expression with side effects. Each message is
    /// generated with the *current* threaded state (via
    /// `generate_block_value_call_stateful`/`generate_block_value_call_runtime_discriminated`,
    /// both of which read `self.current_state_var()` internally), then its
    /// returned `{Result, NewState}` tuple is unpacked and `NewState` becomes
    /// the current state for the next message — mirroring the sequencing the
    /// bare (non-cascade) `Tier2ValueCall`/`LocalAssignTier2` call sites already
    /// use between statements.
    ///
    /// Matching ordinary (non-Tier-2) cascade semantics, the overall value is
    /// the result of the LAST message. Returns a `{Result, NewState}` tuple
    /// with that contract — callers unpack it exactly like a single
    /// `Tier2ValueCall` (see `generate_tier2_value_call_doc`'s own callers).
    fn generate_tier2_cascade_doc(
        &mut self,
        receiver: &Expression,
        all_messages: &[(&MessageSelector, &[Expression])],
    ) -> Result<Document<'static>> {
        let use_runtime_discrimination =
            self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver);

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(all_messages.len() + 1);
        let mut result_var: Option<String> = None;
        let mut state_var: Option<String> = None;
        for (selector, args) in all_messages {
            let call_doc = if use_runtime_discrimination {
                self.generate_block_value_call_runtime_discriminated(
                    receiver,
                    args,
                    &selector.name(),
                )?
            } else {
                self.generate_block_value_call_stateful(receiver, args)?
            };
            let tuple_var = self.fresh_temp_var("CascTuple");
            let this_result = self.fresh_temp_var("CascResult");
            let this_state = self.next_state_var();
            parts.push(docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                call_doc,
                " in let ",
                leaf::var(this_result.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple_var.clone()),
                ") in let ",
                leaf::var(this_state.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ") in ",
            ]);
            result_var = Some(this_result);
            state_var = Some(this_state);
        }
        // is_tier2_value_call requires a non-empty message list before ever
        // routing here, so both are always populated by the loop above.
        let result_var = result_var.expect("cascade must have at least one message");
        let state_var = state_var.expect("cascade must have at least one message");
        parts.push(docvec![
            "{",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}"
        ]);
        Ok(Document::Vec(parts))
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond mere selector-based classification by analysing whether
    /// the block argument(s) contain mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a `ControlFlow` dispatch (from pre-computed `dispatch_kinds`),
    ///    or — when semantic facts are unavailable — the selector matches a known
    ///    exception/conditional selector as a fallback.
    /// 2. The relevant block argument(s) need state threading in the current context
    ///    (checked via `needs_mutation_threading` on pre-computed `block_profiles`).
    ///
    /// Using pre-computed `dispatch_kinds` and `block_profiles` avoids the repeated
    /// selector-based re-classification and `analyze_block` calls that the original
    /// implementation performed.
    pub(in crate::core_erlang) fn control_flow_has_mutations(&self, expr: &Expression) -> bool {
        // See through parentheses so `_r := (1 to: 5 do: [...])` is still
        // classified as control flow with mutations (and thus unpacked + threaded)
        // rather than falling through to a plain pure local assignment.
        let expr = expr.unwrap_parens();

        // `match:` is a dedicated `Expression::Match` node, not a
        // `MessageSend`, so it's otherwise invisible to this classifier — a
        // `match:` arm body that is a state-mutating `[...] value` block would
        // fall through to `BodyExprKind::Pure` and leak its raw `{Result,
        // NewState}` tuple as the match's value. `generate_match` threads state
        // through every arm (see `match_needs_mutation_threading`) whenever any
        // arm needs it, so route it through the same tuple-unwrap machinery as
        // `ifTrue:`/`ifFalse:`.
        if let Expression::Match { arms, .. } = expr {
            return self.match_needs_mutation_threading(arms);
        }

        let Expression::MessageSend {
            receiver,
            arguments,
            selector: beamtalk_core::ast::MessageSelector::Keyword(parts),
            span,
            ..
        } = expr
        else {
            return false;
        };

        // Use pre-computed dispatch classification instead of re-deriving it.
        // When semantic_facts is empty (e.g. in unit tests constructed via
        // `CoreErlangGenerator::new`), `dispatch_kind` returns `Unknown`.
        // In that case fall back to local selector-based classification so the
        // function still returns the correct result for known control-flow
        // selectors rather than silently returning `false` for all of them.
        let dispatch_kind = self.semantic_facts.dispatch_kind(span);
        let sel_str: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let is_control_flow = match dispatch_kind {
            beamtalk_core::semantic_analysis::DispatchKind::ControlFlow => true,
            beamtalk_core::semantic_analysis::DispatchKind::Unknown => {
                beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str())
                    || beamtalk_core::state_threading_selectors::is_conditional_selector(
                        sel_str.as_str(),
                    )
            }
            _ => false,
        };
        if !is_control_flow {
            return false;
        }

        // ADR 0118 phase 3: `whileTrue:`/`whileFalse:`'s RECEIVER
        // is the condition block — like `ensure:`/`on:do:`'s try-body
        // receiver just below, and the conditional-selector receiver
        // further down, it may itself have state effects (a self-send, or
        // an `and:`/`or:` that carries one) even when the BODY argument has
        // none, and neither `is_exception_selector` nor
        // `is_conditional_selector` cover it — the "standard check" below
        // only ever walks `arguments` (the body), never `receiver`. Shares
        // `while_loops.rs`'s own gate (`condition_has_state_effects`)
        // rather than re-deriving it, so the two decisions — "does this
        // statement need `ControlFlowWithMutations` classification" here,
        // "does this loop's own condition need threading" there — cannot
        // disagree.
        if matches!(sel_str.as_str(), "whileTrue:" | "whileFalse:")
            && super::super::control_flow::condition_has_state_effects(receiver)
        {
            return true;
        }

        // For on:do: and ensure:, the receiver (try body) is also
        // a block that may contain field mutations.
        if beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str()) {
            if let Expression::Block(block) = receiver.as_ref() {
                // Also covers a nested list-op/counted-loop inside the
                // try body mutating an outer local even when the try body's own
                // top-level analysis sees no direct mutation (`analyze_block`
                // does not propagate writes out of a nested, non-conditional
                // block — same gap already closed elsewhere for
                // conditionals and ordinary block arguments below). Without
                // this, `[nested-loop] ensure: [...]`/`on:do:` would be
                // classified as pure here even though the nested loop's own
                // cross-scope collector (this call site's sibling,
                // `compute_threaded_locals_for_loop`) correctly detects the
                // mutation — the same "two decision points disagree" shape as
                // the do:/collect: self-classification gap this issue fixes.
                if self.block_arg_needs_threading(block) {
                    return true;
                }
            }
        }

        // For Boolean conditionals, any block argument may contain mutations.
        // IfNotNil: also needs per-block mutation detection.
        if beamtalk_core::state_threading_selectors::is_conditional_selector(sel_str.as_str()) {
            // The conditional's own RECEIVER may be an actor
            // self-send (`(self recordOnce: x) ifTrue:ifFalse:`) whose own
            // state mutation must be threaded, even when neither block
            // argument mutates anything itself. Must stay in sync with
            // `intrinsics.rs`'s `try_generate_boolean_protocol`'s matching
            // check — the two are independently-computed decisions that
            // must agree (see this file's own commentary on that class of
            // bug, e.g. the "two decision points disagree" note above).
            // Widened to any self-send needing threading in the
            // receiver's sub-tree (`((self recordOnce: x) and: [y])
            // ifTrue: [...]`) — the same probe `compile_conditional_receiver`
            // threads ahead with, so the two cannot disagree.
            if self.conditional_receiver_needs_threading(receiver) {
                return true;
            }
            for arg in arguments {
                // `block_arg_needs_threading` also catches a nested list
                // op inside a branch mutating an outer local even when the
                // branch block itself has no direct mutation (`analyze_block`
                // does not propagate writes out of nested blocks) — e.g.
                // `flag ifTrue: [ items do: [:x | sum := sum + x] ]`.
                if let Expression::Block(block) = arg {
                    if self.block_arg_needs_threading(block) {
                        return true;
                    }
                }
            }
            return false;
        }

        // Standard check: analyse argument blocks for mutations.
        // Check ALL block arguments, not just the last one.
        // For selectors like `detect:ifNone:`, the mutation-bearing block is the
        // first argument (predicate), not the last (ifNone handler).
        // `block_arg_needs_threading` also catches nested list ops with
        // cross-scope mutations that `analyze_block` alone can't see.
        for arg in arguments {
            if let Expression::Block(block) = arg {
                if self.block_arg_needs_threading(block) {
                    return true;
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests;
