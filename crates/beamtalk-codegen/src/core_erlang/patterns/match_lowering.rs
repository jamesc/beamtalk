// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `match:` expression compilation and native `Pattern` lowering.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module handles:
//! - `match:` expression compilation (`generate_match`), including the
//!   all-native fast path and the `Pattern::Array`/`Pattern::Type` chain
//!   fallback (`generate_match_chain`, `generate_array_match_arm`)
//! - Native `Pattern` → Core Erlang pattern lowering (`generate_pattern`),
//!   sealed-type constructor patterns, and binary segment patterns
//! - Guard expressions and pattern-variable collection
//!
//! Note: the `Pattern::Type` runtime-test strategies `generate_match_chain`
//! dispatches to live in [`super::type_tests`]; the general-purpose
//! destructuring-assignment extraction helpers live in
//! [`super::destructure`].

use std::collections::HashSet;

use super::super::util::index_lit;
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, Expression,
    ExpressionStatement, MatchArm, MessageSelector, Pattern, WellKnownSelector,
};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates code for a match expression.
    ///
    /// `value match: [pattern -> body ...]` compiles to Core Erlang:
    /// `let _Match1 = <value> in case _Match1 of <Pattern1> when Guard1 -> Body1 ... end`
    ///
    /// When the match contains a `Pattern::Array` arm, each arm is compiled as a
    /// chain of conditional case expressions instead of a single native case, because
    /// Beamtalk arrays are opaque tagged maps that cannot be matched structurally in
    /// Core Erlang patterns. See [`Self::generate_match_chain`].
    pub(in crate::core_erlang) fn generate_match(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "match expression with no arms".to_string(),
                span: Some(value.span()),
            });
        }

        // BT-3489: a value-type instance-method `self.field := ...` arm body
        // has no `Self`-version merge to thread through (only
        // `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` have one — see
        // `generate_vt_conditional_open`), so it must be rejected here rather
        // than compiled into a `Self{N}` reference that never escapes its own
        // `case` clause. The Actor form of the same shape IS supported, via
        // `generate_match_arm_body`'s branch-merge route below.
        //
        // Runs before `match_needs_mutation_threading` decides `base_state`
        // below, so a value-type instance method never reaches the threading
        // path at all — see that function's own context-gating note.
        //
        // Scoped to a value-type INSTANCE method: inside a value-type CLASS
        // method `self.x :=` is a class-var write on the `ClassVars` chain,
        // which threads (and is rejected on its own terms by
        // `reject_class_var_field_assignment`) rather than needing this.
        if matches!(self.context, super::super::CodeGenContext::ValueType)
            && !self.in_class_method()
        {
            for arm in arms {
                if let Some((field, span)) = Self::vt_match_arm_field_write(arm) {
                    return Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm {
                        field: field.to_string(),
                        location: self.location_label(span),
                    });
                }
            }
        }

        let match_var = self.fresh_temp_var("Match");
        let value_doc = self.expression_doc(value)?;

        // Guard against unsupported pattern forms reaching array-match codegen.
        for arm in arms {
            if let Pattern::Array {
                list_syntax: true,
                span,
                ..
            } = &arm.pattern
            {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "List-syntax pattern `#(...)` in match: arm is not yet supported"
                        .to_string(),
                    span: Some(*span),
                });
            }
            // Rest patterns in match arms are not yet supported (rest is only
            // supported in destructuring assignments).
            if let Pattern::Array {
                rest: Some(_),
                span,
                ..
            } = &arm.pattern
            {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Rest pattern `...` in match: arm is not yet supported".to_string(),
                    span: Some(*span),
                });
            }
        }

        let has_array_arm = arms
            .iter()
            .any(|arm| matches!(arm.pattern, Pattern::Array { .. }));
        // ADR 0107 Phase A: a `Pattern::Type` arm needs the same
        // chain-of-nested-`case`s path as `Pattern::Array` — its runtime
        // test (a guard-safe BIF, an atom-exclusion guard, or a
        // `maps:get`-based class-tag check) isn't expressible as a single
        // native Core Erlang pattern the "all-native" fast path below
        // builds. This is also what makes a `match:` mixing a primitive
        // `Type` arm and a `Constructor`/native arm compile correctly into
        // one `case`: both arm kinds flow through `generate_match_chain`,
        // which dispatches each arm (in source order) to its own strategy
        // and recurses for the rest.
        let has_type_arm = arms
            .iter()
            .any(|arm| matches!(arm.pattern, Pattern::Type { .. }));

        // Decide once, for the whole `match:`, whether any arm needs
        // actor state threading (e.g. an arm body that is a state-mutating
        // `[...] value` block). When it does, every arm below is compiled to a
        // uniform `{Value, State}` shape via `generate_match_arm_body`, so the
        // whole `match:` expression threads state exactly like `ifTrue:`/
        // `ifFalse:` mutations — see `match_needs_mutation_threading`.
        let base_state = if self.match_needs_mutation_threading(arms) {
            Some(self.current_state_var())
        } else {
            None
        };

        let inner_doc = if has_array_arm || has_type_arm {
            self.generate_match_chain(&match_var, arms, base_state.as_deref())?
        } else {
            // All arms use native Core Erlang patterns (literals, variables, tuples, maps).
            let mut parts: Vec<Document<'static>> = Vec::new();
            parts.push(docvec!["case ", leaf::var(match_var.clone()), " of "]);
            for (i, arm) in arms.iter().enumerate() {
                if i > 0 {
                    parts.push(Document::Str(" "));
                }
                let pattern_doc = self.generate_pattern(&arm.pattern)?;
                parts.push(Document::Str("<"));
                parts.push(pattern_doc);
                parts.push(Document::Str(">"));
                self.push_scope();
                Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
                    self.bind_var(name, core_var);
                });
                if let Some(guard) = &arm.guard {
                    parts.push(Document::Str(" when "));
                    let guard_doc = self.generate_guard_expression(guard)?;
                    parts.push(guard_doc);
                } else {
                    parts.push(Document::Str(" when 'true'"));
                }
                parts.push(Document::Str(" -> "));
                let body_doc = self.generate_match_arm_body(&arm.body, base_state.as_deref())?;
                parts.push(body_doc);
                self.pop_scope();
            }
            parts.push(Document::Str(" end"));
            Document::Vec(parts)
        };

        Ok(docvec![
            "let ",
            leaf::var(match_var.clone()),
            " = ",
            value_doc,
            " in ",
            inner_doc
        ])
    }

    /// [`Self::generate_match`]'s value-type-instance rejection detector:
    /// the assigned field's name and span when `arm`'s body is a
    /// `self.field := ...` write with no `Self`-version merge to thread
    /// through — either bare (BT-3489, `1 -> (self.x := 1)`, paren-stripped)
    /// or wrapped one level deeper in a local assignment (BT-3493, `1 -> r
    /// := (self.x := ...)`) — or `None` for every other arm-body shape.
    ///
    /// Scoped to exactly the two shapes `generate_match`'s caller already
    /// gates on (a value-type INSTANCE method): a `[...] value`-wrapped
    /// field write is a separate, pre-existing gap this detector does not
    /// (yet) cover — see BT-3493's own follow-up notes.
    fn vt_match_arm_field_write(arm: &MatchArm) -> Option<(&str, Span)> {
        let bare = arm.body.unwrap_parens();
        // BT-3495: a field write nested inside a `[...] value` block's own
        // statements (bare, or local-assign-wrapped) — `arm.body` alone is
        // a `MessageSend` (`value`, zero args) here, not the field write
        // itself, so the checks below never see it; `match_needs_mutation_threading`
        // is unconditionally `false` for a value-type instance method (this
        // function's only caller), so nothing downstream catches this shape
        // either. Every statement is checked, not just the block's last one —
        // a field write buried earlier in the block is still a state
        // mutation with no way to thread through this context.
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = bare
            && arguments.is_empty()
            && matches!(selector.well_known(), Some(WellKnownSelector::Value))
            && let Expression::Block(block) = receiver.unwrap_parens()
        {
            return block
                .body
                .iter()
                .find_map(|stmt| Self::field_write_shape(&stmt.expression))
                .map(Self::field_write_name_and_span);
        }
        Self::field_write_shape(bare).map(Self::field_write_name_and_span)
    }

    /// The field write nested in `expr` for exactly the two shapes
    /// [`Self::vt_match_arm_field_write`] and [`Self::generate_match`]'s
    /// bare-arm check need to recognize: `expr` itself is a `self.field :=
    /// ...` write (paren-stripped so `(self.x := 1)` matches too — it
    /// crashes `erlc` identically), or `expr` is a local assignment (`var :=
    /// ...`) whose own RHS is one (BT-3493's `local_assign_field_write`
    /// shape). `None` for every other shape.
    fn field_write_shape(expr: &Expression) -> Option<&Expression> {
        let bare = expr.unwrap_parens();
        if Self::is_field_assignment(bare) {
            return Some(bare);
        }
        if let Expression::Assignment { target, value, .. } = bare
            && matches!(target.as_ref(), Expression::Identifier(_))
        {
            return Self::local_assign_field_write(value);
        }
        None
    }

    /// Destructures a [`Self::field_write_shape`] result into the
    /// `(field name, span)` pair [`Self::vt_match_arm_field_write`] returns.
    fn field_write_name_and_span(field_write: &Expression) -> (&str, Span) {
        let Expression::Assignment { target, span, .. } = field_write else {
            unreachable!("field_write is always an Assignment, set only via is_field_assignment");
        };
        let Expression::FieldAccess { field, .. } = target.as_ref() else {
            unreachable!("is_field_assignment guarantees a FieldAccess target");
        };
        (field.name.as_str(), *span)
    }

    /// Compiles a `match:` arm body.
    ///
    /// When `base_state` is `None` (no arm in this `match:` needs actor state
    /// threading — the common case), this is exactly `expression_doc`.
    ///
    /// When `base_state` is `Some` (`match_needs_mutation_threading` found at
    /// least one arm that does), every arm must yield a `{Value, State}` tuple
    /// so the whole `match:` expression has one consistent shape that the
    /// caller (`generate_match`) can return as-is, letting the existing
    /// `ifTrue:`/`ifFalse:`-style tuple-unwrap machinery
    /// (`control_flow_has_mutations`'s `Expression::Match` branch) consume it:
    /// - An arm body that is itself a state-mutating `[...] value` block
    ///   (`is_tier2_value_call` with a block-literal receiver) is inlined via
    ///   `generate_conditional_branch_inline` — the same mechanism `ifTrue:`/
    ///   `ifFalse:` branches use — so `self.<field> :=` assignments inside it
    ///   thread state correctly instead of leaking their raw internal tuple
    ///   as the match's value.
    /// - An arm body that is itself a nested control-flow-with-mutations
    ///   construct — `ifTrue:`/`ifFalse:`/a nested `match:`/etc., e.g. `nil ->
    ///   flag ifTrue: [self.x := 1]` with no `[...] value` wrapper — already
    ///   compiles to `{Value, State}` rooted at the same `base_state` (both
    ///   derive from the `self.current_state_var()` snapshot taken before
    ///   this `match:` began, and every intermediate arm's own state-version
    ///   bookkeeping is saved/restored via `with_branch_context`, so it never
    ///   leaks into that snapshot). Passed through via `expression_doc`
    ///   unchanged — no extra wrapping needed.
    /// - Any other arm body — including the remaining `is_tier2_value_call`
    ///   shapes this function doesn't special-case (`self.<field> value`, a
    ///   named `tier2_local_vars`/`tier2_block_params` identifier receiver,
    ///   or a `Cascade` of safe `value:` sends, e.g. `blk value: a; value:
    ///   b`), for which `expression_doc` already unwraps/discards that
    ///   call's own state via `close_tier2_value_subexpr_doc` (a
    ///   sub-expression-position limitation) — is wrapped unchanged as
    ///   `{<value>, <base_state>}`.
    pub(super) fn generate_match_arm_body(
        &mut self,
        body: &Expression,
        base_state: Option<&str>,
    ) -> Result<Document<'static>> {
        let Some(base_state) = base_state else {
            return self.expression_doc(body);
        };
        let base_state_var = leaf::var(base_state.to_string());
        if self.is_tier2_value_call(body) {
            if let Expression::MessageSend { receiver, .. } = body {
                if let Expression::Block(block) = receiver.as_ref() {
                    // ADR 0111 Addendum 5: tier2 conditional-branch
                    // inlining inside a `match:` arm — reaches the SAME
                    // `generate_conditional_branch_inline` single-arm helper
                    // conditionals.rs's ifTrue:/ifFalse: use, which builds,
                    // `verify()`s, and `render()`s this arm's real per-frame
                    // ThreadedIr internally.
                    let (branch_doc, _branch_final) = self.with_branch_context(|this| {
                        this.generate_conditional_branch_inline(block)
                    })?;
                    return Ok(docvec![
                        "let StateAcc = ",
                        base_state_var,
                        " in ",
                        branch_doc
                    ]);
                }
                // Any other is_tier2_value_call receiver (self.<field> value,
                // a named tier2_local_vars/tier2_block_params identifier, or
                // a Cascade of safe value: sends) isn't a literal block, so
                // there's no block to inline here — fall through to
                // expression_doc below, which already unwraps/discards that
                // call's own state via close_tier2_value_subexpr_doc.
            }
        }
        if self.control_flow_has_mutations(body) {
            return self.expression_doc(body);
        }
        // ADR 0118 phase 4: an arm body that is plain AST-directed
        // code but contains a (possibly nested, hoistable) actor self-send —
        // `1 -> 1 + (self bumpCount)` — must not compile through a bare
        // `expression_doc` call, which has no hoisting of its own and
        // silently drops the self-send's mutation. Route it through
        // `generate_conditional_branch_inline` instead — the SAME per-frame
        // `ThreadedIr` machinery `ifTrue:`/`ifFalse:` branches use, wrapped
        // in a synthetic single-statement `Block` — so its full C1-C13
        // statement classification (field/local assignment, a nested
        // self-send anywhere `thread_ahead` reaches) applies here too,
        // rather than re-deriving a narrower hoist by hand.
        // BT-3489: an arm body that writes `self.field := ...` on its own
        // evaluation path takes the same route, and for the same reason —
        // `expression_doc` would emit the field write's `State1`/`Self1`
        // binding scoped inside this one `case` arm, leaving every reference
        // after the `match:` unbound (`erlc: unbound variable 'State1'`).
        // `generate_conditional_branch_inline` classifies it as ADR 0111
        // Addendum 5 §C1 and merges the mutated state into this arm's
        // `{Value, State}` tuple, exactly as an `ifTrue:` branch's own field
        // write already does.
        //
        // Matched on the paren-stripped body, and the synthetic block is built
        // from that same stripped expression, so `1 -> (self.total := 1)` takes
        // this route too — parentheses are pure grouping, but without the strip
        // the arm fell through to `expression_doc` and crashed `erlc` exactly
        // like the unparenthesized form did. (`subexpr_needs_prelude` already
        // strips parens internally, so the first disjunct is unaffected.)
        //
        // Deliberately matches only a field write that IS the arm body, not one
        // nested inside a local assignment (`1 -> r := (self.total := 1)`).
        // That wrapped shape is a separate, pre-existing gap in the shared
        // ADR 0111 Addendum 5 statement classifier — it fails the SAME
        // `ThreadedIr` verify (`UnboundVersion State1`) inside a plain
        // `ifTrue:` branch and a loop body, with no `match:` involved — so
        // widening the detector here would only convert this arm's `erlc`
        // crash into a verifier panic without fixing anything. Tracked as
        // BT-3493, which fixes it in the classifier (covering all three
        // constructs at once) and can then widen this condition.
        let bare_body = body.unwrap_parens();
        if self.conditional_receiver_needs_threading(body) || Self::is_field_assignment(bare_body) {
            let synthetic_block = Block::new(
                Vec::new(),
                vec![ExpressionStatement::bare(bare_body.clone())],
                body.span(),
            );
            let (branch_doc, _branch_final) = self.with_branch_context(|this| {
                this.generate_conditional_branch_inline(&synthetic_block)
            })?;
            return Ok(docvec![
                "let StateAcc = ",
                base_state_var,
                " in ",
                branch_doc
            ]);
        }
        let body_doc = self.expression_doc(body)?;
        Ok(docvec!["{", body_doc, ", ", base_state_var, "}"])
    }

    /// Compiles match arms as a chain of nested case expressions.
    ///
    /// Used when any arm contains a `Pattern::Array` (opaque tagged map — cannot be
    /// matched natively in Core Erlang). Each arm becomes its own case expression with
    /// a wildcard fallthrough to the next arm.
    ///
    /// For `Pattern::Array` arms the generated structure is:
    /// ```text
    /// case is_map(_Match) of
    ///   <'true'> when 'true' ->
    ///     case maps:get('$beamtalk_class', _Match, 'undefined') of
    ///       <'Array'> when 'true' ->
    ///         case beamtalk_array:size(_Match) of
    ///           <N> when 'true' ->
    ///             let Elem1 = at(_Match, 1) in ... body
    ///           <_NoMatch> when 'true' -> [rest]
    ///         end
    ///       <_NoMatch> when 'true' -> [rest]
    ///     end
    ///   <'false'> when 'true' -> [rest]
    /// end
    /// ```
    ///
    /// For all other arms:
    /// `case _Match of <pattern> when guard -> body <_FT> when 'true' -> [rest] end`
    pub(super) fn generate_match_chain(
        &mut self,
        match_var: &str,
        arms: &[MatchArm],
        base_state: Option<&str>,
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Ok(docvec![
                "call 'erlang':'error'({'case_clause', ",
                leaf::var(match_var.to_string()),
                "})"
            ]);
        }

        let arm = &arms[0];
        let rest = &arms[1..];

        if let Pattern::Array { elements, .. } = &arm.pattern {
            // Generate current arm body FIRST so that state_version and temp-var counters
            // are not advanced by later arms before we codegen the current one.
            return self.generate_array_match_arm(match_var, arm, elements, rest, base_state);
        }

        // ADR 0107 Phase A: `Pattern::Type` (`binding :: ClassName`)
        // dispatches to a per-class runtime test (BIF test, atom-exclusion
        // guard, or map-tag check) — see `generate_type_pattern`. Like
        // `Pattern::Array` above, this can't reuse the plain native-arm path
        // below because at least one class (`Dictionary`, or an exact tagged
        // `Value`/sealed class) needs a `maps:get` test in case-scrutinee
        // position, which is not expressible as a single Core Erlang guard.
        if let Pattern::Type { binding, class, .. } = &arm.pattern {
            return self.generate_type_pattern(match_var, arm, binding, class, rest, base_state);
        }

        // Native arm: case _Match of <pattern> when guard -> body <_FT> when 'true' -> rest end
        // Compute current arm first — body codegen must not see state advanced by later arms.
        let fallthrough_var = self.fresh_temp_var("NoMatch");
        let pattern_doc = self.generate_pattern(&arm.pattern)?;

        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_part = if let Some(guard) = &arm.guard {
            let gd = self.generate_guard_expression(guard)?;
            docvec![" when ", gd]
        } else {
            Document::Str(" when 'true'")
        };
        let body_doc = self.generate_match_arm_body(&arm.body, base_state)?;
        self.pop_scope();

        // Rest is generated AFTER the current arm to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest, base_state)?;

        Ok(docvec![
            "case ",
            leaf::var(match_var.to_string()),
            " of ",
            "<",
            pattern_doc,
            ">",
            guard_part,
            " -> ",
            body_doc,
            " <",
            leaf::var(fallthrough_var),
            "> when 'true' -> ",
            rest_doc,
            " end"
        ])
    }

    /// Compiles a single `Pattern::Array` match arm.
    ///
    /// Emits a three-level conditional check (`is_map` → class → size), then element
    /// extractions via `at:` dispatch. `rest_doc` is cloned for all failure branches.
    ///
    /// Takes `rest_arms` (not a pre-built `rest_doc`) so that the current arm's body
    /// is fully generated before any later arm's codegen advances generator state.
    fn generate_array_match_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        elements: &[Pattern],
        rest_arms: &[MatchArm],
        base_state: Option<&str>,
    ) -> Result<Document<'static>> {
        let n = elements.len();

        // Bind all pattern variables (including from nested arrays) into scope
        // before generating guard/body so they can reference the bound names.
        // This MUST happen before rest_arms codegen so state is not leaked.
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_doc_opt = if let Some(guard) = &arm.guard {
            Some(self.generate_guard_expression(guard)?)
        } else {
            None
        };
        let body_doc = self.generate_match_arm_body(&arm.body, base_state)?;
        self.pop_scope();

        // Generate rest AFTER the current arm body to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest_arms, base_state)?;

        // Build body section: element extractions + optional guard check.
        // Use `case 'true' of <'true'> when GUARD ->` (Core Erlang guard position) so that
        // guard evaluation errors silently fail and fall through, matching Erlang guard semantics.
        // A plain `case GUARD of <'true'>` would propagate evaluation errors as exceptions.
        let success_doc = if let Some(guard_doc) = guard_doc_opt {
            docvec![
                "case 'true' of ",
                "<'true'> when ",
                guard_doc,
                " -> ",
                body_doc,
                " <'true'> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ]
        } else {
            body_doc
        };

        // Recursively build element extraction + nested array checks
        let mut bound_vars: HashSet<String> = HashSet::new();
        let inner_body = self.build_array_arm_body(
            match_var,
            elements,
            0,
            success_doc,
            &rest_doc,
            &mut bound_vars,
        )?;

        let no_match_size = self.fresh_temp_var("NoMatch");
        let no_match_class = self.fresh_temp_var("NoMatch");

        Ok(docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(match_var.to_string()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<",
            index_lit(n),
            "> when 'true' -> ",
            inner_body,
            " <",
            leaf::var(no_match_size),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<",
            leaf::var(no_match_class),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest_doc,
            " end"
        ])
    }

    /// Handles a `Pattern::Array` element inside an outer array match arm.
    ///
    /// Extracts the element, verifies it is an `Array` of the right size, then
    /// recurses into its sub-elements before continuing with the outer arm.
    #[allow(clippy::too_many_arguments)]
    fn build_nested_array_element(
        &mut self,
        outer_array_var: &str,
        outer_elements: &[Pattern],
        start: usize,
        one_based: usize,
        inner_elems: &[Pattern],
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        let nested_var = self.fresh_temp_var("ArrElem");
        let n_inner = inner_elems.len();

        let after_nested = self.build_array_arm_body(
            outer_array_var,
            outer_elements,
            start + 1,
            continuation,
            failure_doc,
            already_bound,
        )?;
        let inner_body = self.build_array_arm_body(
            &nested_var,
            inner_elems,
            0,
            after_nested,
            failure_doc,
            already_bound,
        )?;

        let no_match_size_n = self.fresh_temp_var("NoMatch");
        let no_match_class_n = self.fresh_temp_var("NoMatch");

        Ok(docvec![
            "let ",
            leaf::var(nested_var.clone()),
            " = call 'beamtalk_message_dispatch':'send'(",
            leaf::var(outer_array_var.to_string()),
            ", 'at:', [",
            index_lit(one_based),
            "]) in ",
            "case call 'erlang':'is_map'(",
            leaf::var(nested_var.clone()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(nested_var.clone()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            leaf::var(nested_var.clone()),
            ") of ",
            "<",
            index_lit(n_inner),
            "> when 'true' -> ",
            inner_body,
            " <",
            leaf::var(no_match_size_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<",
            leaf::var(no_match_class_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ])
    }

    /// Emits the equality-check extraction for a duplicate `Pattern::Variable` in an
    /// array match arm: `let VarDup = at:[N] in case erlang:=:=(Var, VarDup) of ...`
    fn build_array_variable_element(
        &mut self,
        array_var: &str,
        core_var: String,
        one_based: usize,
        next: Document<'static>,
        failure_doc: &Document<'static>,
    ) -> Document<'static> {
        let dup_var = self.fresh_temp_var(&format!("{core_var}Dup"));
        let mismatch_var = self.fresh_temp_var("Mismatch");
        docvec![
            "let ",
            leaf::var(dup_var.clone()),
            " = call 'beamtalk_message_dispatch':'send'(",
            leaf::var(array_var.to_string()),
            ", 'at:', [",
            index_lit(one_based),
            "]) in ",
            "case call 'erlang':'=:='(",
            leaf::var(core_var),
            ", ",
            leaf::var(dup_var),
            ") of ",
            "<'true'> when 'true' -> ",
            next,
            " <",
            leaf::var(mismatch_var),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ]
    }

    /// Recursively builds element-extraction `let`-bindings for an array pattern arm.
    ///
    /// Handles nested `Pattern::Array` elements by wrapping the continuation in a
    /// sub-array check. Variables are pre-registered in scope by
    /// [`Self::generate_array_match_arm`] via [`Self::collect_pattern_variables`].
    ///
    /// `continuation` is what to execute after all elements are extracted.
    /// `failure_doc` is what to execute if any nested array check fails.
    /// `already_bound` tracks variable names already extracted in this arm; a second
    /// occurrence emits an `erlang:=:=` equality check rather than a new binding.
    fn build_array_arm_body(
        &mut self,
        array_var: &str,
        elements: &[Pattern],
        start: usize,
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        if start >= elements.len() {
            return Ok(continuation);
        }

        let one_based = start + 1;
        match &elements[start] {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                if already_bound.contains(id.name.as_str()) {
                    // Duplicate: build rest first, then wrap with equality check.
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(self.build_array_variable_element(
                        array_var,
                        core_var,
                        one_based,
                        next,
                        failure_doc,
                    ))
                } else {
                    // First occurrence: register BEFORE recursing so later positions
                    // with the same name are recognised as duplicates.
                    already_bound.insert(id.name.to_string());
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        leaf::var(array_var.to_string()),
                        ", 'at:', [",
                        index_lit(one_based),
                        "]) in ",
                        next
                    ])
                }
            }
            Pattern::Wildcard(_) => self.build_array_arm_body(
                array_var,
                elements,
                start + 1,
                continuation,
                failure_doc,
                already_bound,
            ),
            Pattern::Array {
                elements: inner_elems,
                ..
            } => {
                let inner_elems_clone: Vec<Pattern> = inner_elems.clone();
                self.build_nested_array_element(
                    array_var,
                    elements,
                    start,
                    one_based,
                    &inner_elems_clone,
                    continuation,
                    failure_doc,
                    already_bound,
                )
            }
            elem => Err(CodeGenError::UnsupportedFeature {
                feature: "Unsupported pattern in Array match arm element".to_string(),
                span: Some(elem.span()),
            }),
        }
    }

    /// Generates a Core Erlang pattern from a Pattern AST node.
    pub(in crate::core_erlang) fn generate_pattern(
        &mut self,
        pattern: &Pattern,
    ) -> Result<Document<'static>> {
        match pattern {
            Pattern::Wildcard(_) => Ok(Document::Str("_")),
            // Reuses the existing atom-literal codegen path verbatim
            // (ADR 0107 Phase A) — `nil` has one canonical runtime
            // representation, no new mechanism needed.
            Pattern::Nil(_) => Ok(Document::Str("'nil'")),
            // `Pattern::Type` only has codegen as a top-level match arm
            // pattern (`generate_type_pattern`, dispatched from
            // `generate_match_chain`) — its per-class runtime test needs to
            // wrap the *whole* arm (body, guard, rest-arm fallthrough), which
            // isn't expressible as a plain native sub-pattern nested inside a
            // `Tuple`/`List`/`Map`/`Constructor`. Mirrors `Pattern::Array`'s
            // identical restriction just below.
            Pattern::Type { span, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: "Type pattern (`binding :: ClassName`) nested inside a composite \
                          pattern (tuple/list/map/constructor) is not supported — only \
                          top-level match arm patterns are supported (ADR 0107 Phase A)"
                    .to_string(),
                span: Some(*span),
            }),
            Pattern::Variable(id) => {
                let var_name = Self::to_core_erlang_var(&id.name);
                Ok(leaf::var(var_name))
            }
            Pattern::Literal(lit, _) => self.generate_literal(lit),
            Pattern::Tuple { elements, .. } => {
                let mut parts = vec![Document::Str("{")];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                parts.push(Document::Str("}"));
                Ok(Document::Vec(parts))
            }
            Pattern::Array { .. } => {
                // Beamtalk arrays are opaque tagged maps — they cannot appear as a
                // native Core Erlang sub-pattern (inside tuples, lists, binary segments, etc.).
                // Top-level array patterns in `match:` arms are handled by
                // `generate_array_match_arm` and never reach this path.
                Err(CodeGenError::UnsupportedFeature {
                    feature: "Array pattern nested inside a composite native pattern (tuple/list) is not supported".to_string(),
                    span: Some(pattern.span()),
                })
            }
            Pattern::List { elements, tail, .. } => {
                if elements.is_empty() && tail.is_none() {
                    return Ok(Document::Str("[]"));
                }
                let mut parts = vec![Document::Str("[")];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                if let Some(tail_pat) = tail {
                    parts.push(Document::Str(" | "));
                    parts.push(self.generate_pattern(tail_pat)?);
                }
                parts.push(Document::Str("]"));
                Ok(Document::Vec(parts))
            }
            Pattern::Binary { segments, .. } => {
                let mut parts = vec![Document::Str("#{")];
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(","));
                    }
                    parts.push(self.generate_binary_segment(seg)?);
                }
                parts.push(Document::Str("}#"));
                Ok(Document::Vec(parts))
            }
            Pattern::Map { pairs, .. } => {
                // Beamtalk Dictionaries are plain Erlang maps — emit a Core Erlang map pattern.
                // `#{#k => v}` compiles to `~{ 'k' := V }~`   (symbol key → atom)
                // `#{"k" => v}` compiles to `~{ <binary literal for "k"> := V }~` (string key → binary)
                // `:=` is the Core Erlang match-binding form.
                if pairs.is_empty() {
                    return Ok(Document::Str("~{}~"));
                }
                let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];
                for (i, pair) in pairs.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    let key_doc = super::map_pattern_key_doc(&pair.key);
                    parts.push(key_doc);
                    parts.push(Document::Str(" := "));
                    parts.push(self.generate_pattern(&pair.value)?);
                }
                parts.push(Document::Str(" }~"));
                Ok(Document::Vec(parts))
            }
            Pattern::Constructor {
                class,
                keywords,
                span,
            } => self.generate_constructor_pattern(class, keywords, *span),
        }
    }

    /// Generates a Core Erlang map pattern for a sealed-type constructor pattern.
    ///
    /// Maps `Result ok: v` to:
    /// ```text
    /// ~{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := V}~
    /// ```
    ///
    /// The field mapping is looked up from [`sealed_constructor_fields`].
    fn generate_constructor_pattern(
        &mut self,
        class: &beamtalk_core::ast::Identifier,
        keywords: &[(beamtalk_core::ast::Identifier, Pattern)],
        span: beamtalk_core::source_analysis::Span,
    ) -> Result<Document<'static>> {
        // Build the full selector from keyword parts (e.g. "ok:" or "attempt:identifier:")
        let selector: String = keywords.iter().map(|(kw, _)| kw.name.as_str()).collect();

        let fields = sealed_constructor_fields(&class.name, &selector).ok_or_else(|| {
            CodeGenError::UnsupportedFeature {
                feature: format!(
                    "Constructor pattern `{} {}` — '{}' is not a known sealed type or \
                     '{}' is not a recognised constructor. \
                     Only stdlib sealed types (e.g. Result) support constructor patterns in this release.",
                    class.name, selector, class.name, selector
                ),
                span: Some(span),
            }
        })?;

        // Validate arity
        if keywords.len() != fields.binding_fields.len() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "Constructor pattern `{} {}` has {} argument(s) but constructor expects {}",
                    class.name,
                    selector,
                    keywords.len(),
                    fields.binding_fields.len()
                ),
                span: Some(span),
            });
        }

        // ~{'$beamtalk_class' := 'ClassName', 'discriminator' := Value, ..., 'field' := Binding}~
        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{")];

        // Class tag
        parts.push(Document::Str("'$beamtalk_class' := "));
        parts.push(leaf::atom(class.name.to_string()));

        // Discriminator fields (fixed values distinguishing variants)
        for (field_name, field_value) in &fields.discriminators {
            parts.push(Document::Str(", "));
            parts.push(leaf::atom(*field_name));
            parts.push(Document::Str(" := "));
            // field_value is a compile-time-constant Core Erlang term (e.g. `'true'`).
            parts.push(Document::Str(field_value));
        }

        // Binding fields — one per keyword argument. Wildcards still emit the
        // field key (requiring the key to exist in the map) but bind to `_`.
        for ((_, binding_pattern), field_name) in keywords.iter().zip(fields.binding_fields.iter())
        {
            parts.push(Document::Str(", "));
            parts.push(leaf::atom(*field_name));
            parts.push(Document::Str(" := "));
            parts.push(self.generate_pattern(binding_pattern)?);
        }

        parts.push(Document::Str("}~"));
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang binary segment: `#<Value>(Size,Unit,Type,Flags)`.
    ///
    /// Core Erlang binary segment format:
    /// ```text
    /// #<VarName>(Size, Unit, Type, ['flag1'|['flag2']])
    /// ```
    ///
    /// Defaults: type=integer, signedness=unsigned, endianness=big,
    /// size=8 for integer/float, 'all' for binary, 'undefined' for utf8.
    /// Unit=1 for integer/float, 8 for binary, 'undefined' for utf8.
    pub(in crate::core_erlang) fn generate_binary_segment(
        &mut self,
        seg: &BinarySegment,
    ) -> Result<Document<'static>> {
        // Value: the variable or wildcard
        let value_doc = self.generate_pattern(&seg.value)?;

        // Resolve type (default: integer)
        let seg_type = seg.segment_type.unwrap_or(BinarySegmentType::Integer);

        // Size: explicit, or default based on type
        let size_doc = if let Some(size_expr) = &seg.size {
            self.expression_doc(size_expr)?
        } else {
            match seg_type {
                BinarySegmentType::Binary => Document::Str("'all'"),
                BinarySegmentType::Utf8 => Document::Str("'undefined'"),
                BinarySegmentType::Integer => Document::Str("8"),
                BinarySegmentType::Float => Document::Str("64"),
            }
        };

        // Unit: 8 for binary, 'undefined' for utf8, 1 for integer/float
        let unit_doc = match seg_type {
            BinarySegmentType::Binary => Document::Str("8"),
            BinarySegmentType::Utf8 => Document::Str("'undefined'"),
            BinarySegmentType::Integer | BinarySegmentType::Float => Document::Str("1"),
        };

        // Type atom
        let type_doc = match seg_type {
            BinarySegmentType::Integer => Document::Str("'integer'"),
            BinarySegmentType::Float => Document::Str("'float'"),
            BinarySegmentType::Binary => Document::Str("'binary'"),
            BinarySegmentType::Utf8 => Document::Str("'utf8'"),
        };

        // Flags: cons-cell list ['signedness'|['endianness']]
        // Defaults: unsigned, big
        let sign_str = match seg.signedness.unwrap_or(BinarySignedness::Unsigned) {
            BinarySignedness::Signed => "'signed'",
            BinarySignedness::Unsigned => "'unsigned'",
        };
        let end_str = match seg.endianness.unwrap_or(BinaryEndianness::Big) {
            BinaryEndianness::Big => "'big'",
            BinaryEndianness::Little => "'little'",
            BinaryEndianness::Native => "'native'",
        };
        let flags_doc = docvec![
            "[",
            Document::Str(sign_str),
            "|[",
            Document::Str(end_str),
            "]]"
        ];

        Ok(docvec![
            "#<", value_doc, ">(", size_doc, ",", unit_doc, ",", type_doc, ",", flags_doc, ")"
        ])
    }

    /// Generates a Core Erlang guard expression.
    ///
    /// Core Erlang guards only allow a restricted set of BIFs:
    /// comparisons, arithmetic, and type checks.
    pub(super) fn generate_guard_expression(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => match id.name.as_str() {
                "true" => Ok(Document::Str("'true'")),
                "false" => Ok(Document::Str("'false'")),
                "nil" => Ok(Document::Str("'nil'")),
                _ => {
                    if let Some(var_name) = self.lookup_var(&id.name) {
                        Ok(leaf::var(var_name.clone()))
                    } else {
                        let var_name = Self::to_core_erlang_var(&id.name);
                        Ok(leaf::var(var_name))
                    }
                }
            },
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                let left = self.generate_guard_expression(receiver)?;
                let right = self.generate_guard_expression(&arguments[0])?;
                let erlang_op = match op.as_str() {
                    ">" => ">",
                    "<" => "<",
                    ">=" => ">=",
                    "<=" => "=<",
                    "=:=" => "=:=",
                    "/=" => "/=",
                    "=/=" => "=/=",
                    "+" => "+",
                    "-" => "-",
                    "*" => "*",
                    "/" => "/",
                    _ => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: format!("operator '{op}' in guard expression"),
                            span: Some(expr.span()),
                        });
                    }
                };
                Ok(docvec![
                    "call 'erlang':",
                    leaf::atom(erlang_op),
                    "(",
                    left,
                    ", ",
                    right,
                    ")",
                ])
            }
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: "complex guard expression (only comparisons and arithmetic allowed)"
                    .to_string(),
                span: Some(expr.span()),
            }),
        }
    }

    /// Collects all variable names from a pattern and calls the callback
    /// with the Beamtalk name and the corresponding Core Erlang variable name.
    fn collect_pattern_variables(pattern: &Pattern, mut bind: impl FnMut(&str, &str)) {
        Self::collect_pattern_variables_inner(pattern, &mut bind);
    }

    fn collect_pattern_variables_inner(pattern: &Pattern, bind: &mut impl FnMut(&str, &str)) {
        match pattern {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                bind(&id.name, &core_var);
            }
            Pattern::Tuple { elements, .. } | Pattern::List { elements, .. } => {
                for elem in elements {
                    Self::collect_pattern_variables_inner(elem, bind);
                }
                if let Pattern::List { tail: Some(t), .. } = pattern {
                    Self::collect_pattern_variables_inner(t, bind);
                }
            }
            Pattern::Array { elements, rest, .. } => {
                for elem in elements {
                    Self::collect_pattern_variables_inner(elem, bind);
                }
                if let Some(rest_pat) = rest {
                    Self::collect_pattern_variables_inner(rest_pat, bind);
                }
            }
            Pattern::Binary { segments, .. } => {
                for seg in segments {
                    Self::collect_pattern_variables_inner(&seg.value, bind);
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    Self::collect_pattern_variables_inner(&pair.value, bind);
                }
            }
            Pattern::Constructor { keywords, .. } => {
                for (_, binding) in keywords {
                    Self::collect_pattern_variables_inner(binding, bind);
                }
            }
            // `Pattern::Type`'s `binding` is bound directly by
            // `generate_type_pattern` when it's the arm's
            // top-level pattern — this helper is only reached for *nested*
            // sub-patterns (inside `Tuple`/`List`/`Map`/`Constructor`), and
            // `generate_pattern` already rejects a nested `Pattern::Type`
            // before this would matter (same restriction as
            // `Pattern::Array`).
            Pattern::Wildcard(_)
            | Pattern::Literal(_, _)
            | Pattern::Nil(_)
            | Pattern::Type { .. } => {}
        }
    }
}

/// Field layout for a sealed-type constructor pattern (Phase 1: stdlib types only).
///
/// `discriminators` — fixed field values that distinguish variants of the same sealed type
///   (e.g. `isOk => true` for `Result ok:`).
/// `binding_fields` — ordered list of map keys that receive the constructor arguments.
struct ConstructorPatternFields {
    discriminators: Vec<(&'static str, &'static str)>,
    binding_fields: Vec<&'static str>,
}

/// Returns the Core Erlang field layout for a known sealed-type constructor.
///
/// Returns `None` for unknown classes or unknown selectors (caller emits a compile error).
///
/// # Phase 1 scope
/// Only stdlib sealed types are supported. User-defined sealed types require the
/// Phase 2 `[pattern: ...]` annotation (tracked separately).
fn sealed_constructor_fields(class: &str, selector: &str) -> Option<ConstructorPatternFields> {
    match (class, selector) {
        ("Result", "ok:") => Some(ConstructorPatternFields {
            discriminators: vec![("isOk", "'true'")],
            binding_fields: vec!["okValue"],
        }),
        ("Result", "error:") => Some(ConstructorPatternFields {
            discriminators: vec![("isOk", "'false'")],
            binding_fields: vec!["errReason"],
        }),
        _ => None,
    }
}
