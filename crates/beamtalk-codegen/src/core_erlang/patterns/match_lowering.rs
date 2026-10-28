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

use super::super::control_flow::ArmFrame;
use super::super::threaded_ir::{ThreadedValue, VersionPrefix};
use super::super::util::index_lit;
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, Expression,
    ExpressionStatement, Identifier, MatchArm, MessageSelector, Pattern, WellKnownSelector,
};
use beamtalk_core::source_analysis::Span;

/// ADR 0122 Decision 4: the storage families a `match:` arm can carry
/// through [`CoreErlangGenerator::generate_match_arm_body`]'s merge —
/// data, not a hand-written per-context check. `SelfVt` is deliberately
/// excluded: a value-type INSTANCE method's `Self`/`SelfN` chain has no
/// N-arm merge to thread a field write through at all (see
/// [`CoreErlangGenerator::generate_match`]'s up-front rejection, driven by
/// this same declaration, and
/// [`CoreErlangGenerator::match_needs_state_threading`], which uses it to
/// decide whether a bare field-write arm threads). Adding `SelfVt` support
/// later is the one-line change ADR 0122's Implementation 9 "Out of Scope"
/// note describes — extend this list, nothing else.
const MATCH_ARM_FAMILIES: &[VersionPrefix] = &[VersionPrefix::State];

/// How the arms of one `match:` thread state: every arm hands back
/// `{Value, StateAcc}` rooted at `base_state`. `arm` is set when the `match:` is a
/// local-threading producer (BT-3947, [`CoreErlangGenerator::generate_match_producer`]):
/// each arm is then a `BranchArm` frame carrying the construct's threaded locals,
/// lowered like a conditional's block, so a write to an outer local in an arm
/// is a `Put` into the arm's `StateAcc` that the producer's `LocalRebind`s read back.
pub(in crate::core_erlang) struct MatchThreading<'a> {
    /// The scratch map every arm starts from.
    pub(in crate::core_erlang) base_state: &'a str,
    /// The arm frame of a producer's arms; `None` for the actor `State` threading of
    /// a `match:` whose arms only write fields or send to `self`.
    pub(in crate::core_erlang) arm: Option<&'a ArmFrame>,
}

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
        let recv_tv = self.compile_match_scrutinee(value)?;
        let value_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let value_doc = self.threaded_value_doc(&recv_tv.value);
        let inner = self.generate_match_after_scrutinee(arms, &value_doc, None)?;
        Ok(docvec![value_preamble, inner])
    }

    /// BT-4146: the scrutinee compiles through `threaded_expression`, like a
    /// conditional's receiver (`compile_conditional_receiver`): it may be a
    /// call that must run inside a `SyncedCall` window in an actor method
    /// (`0 + x` coerces through a user `plusFromNumber:` when `x` is not
    /// statically numeric), or thread state ahead of the `case`. An enclosing
    /// sequencing that already compiled it ahead (`single_sequenced_child`)
    /// reads the precompiled value back here instead of compiling it twice.
    /// The prelude is empty for a pure scrutinee.
    ///
    /// BT-4196: a local-threading producer calls this BEFORE reading the base
    /// `State` for its scratch seed, so the scrutinee's field writes are seen.
    pub(in crate::core_erlang) fn compile_match_scrutinee(
        &mut self,
        value: &Expression,
    ) -> Result<ThreadedValue> {
        let recv_frame = self.current_frame();
        self.threaded_expression(value, recv_frame)
    }

    /// The arms half of [`Self::generate_match`]: everything after the
    /// scrutinee has been compiled. Returns `let Match = <value> in case ...`
    /// without the scrutinee's prelude (the caller places it). `producer` is a
    /// local-threading producer's seeded scratch map and arm frame (BT-3947,
    /// [`Self::generate_match_producer`]); `None` threads the arms by the
    /// actor-`State` rule ([`Self::match_needs_state_threading`]).
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_match_after_scrutinee(
        &mut self,
        arms: &[MatchArm],
        value_doc: &Document<'static>,
        producer: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // BT-3489: a value-type instance-method `self.field := ...` arm body
        // has no `Self`-version merge to thread through (only
        // `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` have one — see
        // `generate_vt_conditional_open`), so it must be rejected here rather
        // than compiled into a `Self{N}` reference that never escapes its own
        // `case` clause. The Actor form of the same shape IS supported, via
        // `generate_match_arm_body`'s branch-merge route below.
        //
        // ADR 0122: driven by the shared capability declaration
        // (`MATCH_ARM_FAMILIES`) against the shared, per-context detector
        // (`Self::eligible_families`) rather than a hand-written
        // `ValueType && !in_class_method()` check — `SelfVt` is the one
        // family `eligible_families` can report here that `MATCH_ARM_FAMILIES`
        // excludes, and `eligible_families` reports it in exactly the same
        // context the old check named (a value-type INSTANCE method), so the
        // two are equivalent by construction.
        //
        // Runs before `match_needs_state_threading` decides `base_state`
        // below, so a value-type instance method never reaches the threading
        // path at all — see that function's own context-gating note.
        //
        // Scoped to a value-type INSTANCE method: inside a value-type CLASS
        // method `self.x :=` is a class-var write, an in-place `put` into the
        // class process (ADR 0130) that threads nothing, so it needs no
        // rejection here.
        if self.eligible_families().contains(&VersionPrefix::SelfVt)
            && !MATCH_ARM_FAMILIES.contains(&VersionPrefix::SelfVt)
        {
            for arm in arms {
                if let Some((field, span)) = self.vt_match_arm_field_write(arm) {
                    return Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm {
                        field: field.to_string(),
                        location: self.location_label(span),
                    });
                }
            }
        }

        let match_var = self.fresh_temp_var("Match");

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

        // BT-4096: only a top-level binary arm pattern splits; a split-needing
        // binary nested inside a composite pattern is a clean error here,
        // not invalid Core Erlang from the pattern lowering below.
        for arm in arms {
            if !matches!(arm.pattern, Pattern::Binary { .. }) {
                if let Some(span) = nested_split_span(&arm.pattern) {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "a binary segment size naming an earlier segment's variable, or a size expression, is only supported when the binary pattern is the whole match-arm pattern — match the enclosing value first, then match the binary"
                            .to_string(),
                        span: Some(span),
                    });
                }
            }
        }

        let has_array_arm = arms
            .iter()
            .any(|arm| matches!(arm.pattern, Pattern::Array { .. }));
        // BT-4096: a top-level binary pattern with dependent sizes compiles
        // to nested matches with per-arm fallthrough, like Array/Type arms.
        let has_segmented_binary_arm = arms.iter().any(|arm| {
            matches!(&arm.pattern, Pattern::Binary { segments, .. } if binary_needs_segmented(segments))
        });
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
        // BT-4173: a top-level binary arm unwraps a `String` scrutinee to
        // its bytes before the native binary match (see
        // `binary_match_scrutinee`) — a per-arm scrutinee rewrite the
        // single-scrutinee fast path below cannot express, so any binary
        // arm routes the whole `match:` through the chain. BT-4223: so does
        // a binary nested inside a composite arm pattern — it is hoisted
        // out and matched (unwrapped) after the outer pattern
        // (`generate_nested_binary_arm`).
        let has_binary_arm = arms.iter().any(|arm| pattern_has_binary(&arm.pattern));

        // Decide once, for the whole `match:`, whether any arm needs
        // actor state threading (e.g. an arm body that is a state-mutating
        // `[...] value` block). When it does, every arm below is compiled to a
        // uniform `{Value, State}` shape via `generate_match_arm_body`, so the
        // whole `match:` expression threads state exactly like `ifTrue:`/
        // `ifFalse:` mutations — see `match_needs_state_threading`.
        let state_threading_base = if producer.is_none() && self.match_needs_state_threading(arms) {
            Some(self.current_state_var())
        } else {
            None
        };
        let state_threading = state_threading_base
            .as_deref()
            .map(|base_state| MatchThreading {
                base_state,
                arm: None,
            });
        let threading = producer.or(state_threading.as_ref());

        let inner_doc =
            if has_array_arm || has_type_arm || has_segmented_binary_arm || has_binary_arm {
                self.generate_match_chain(&match_var, arms, threading)?
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
                    let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
                    parts.push(body_doc);
                    self.pop_scope();
                }
                // BT-3943: no arm may match — raise a structured `#pattern_mismatch`
                // rather than the compiler's implicit raw `case_clause`
                // (`internal_error`). Skipped when an unguarded wildcard/variable
                // arm already makes the `case` exhaustive (the clause would be dead).
                let exhaustive = arms.iter().any(|arm| {
                    arm.guard.is_none()
                        && matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Variable(_))
                });
                if !exhaustive {
                    let no_match_var = self.fresh_temp_var("NoMatch");
                    let raise = self.pattern_mismatch_raise_doc(
                        &match_var,
                        "match:",
                        "no match: arm matches the value",
                    );
                    parts.push(docvec![
                        " <",
                        leaf::var(no_match_var),
                        "> when 'true' -> ",
                        raise
                    ]);
                }
                parts.push(Document::Str(" end"));
                Document::Vec(parts)
            };

        Ok(docvec![
            "let ",
            leaf::var(match_var.clone()),
            " = ",
            value_doc.clone(),
            " in ",
            inner_doc
        ])
    }

    /// `true` when a `match:` needs state threading — i.e. at least one arm's
    /// body either is a Tier 2 value-call (most commonly a state-mutating
    /// `[...] value` block), is itself a nested control-flow-with-mutations
    /// construct (`ifTrue:`/`ifFalse:`/a nested `match:`/etc. with no
    /// `[...] value` wrapper, e.g. `nil -> flag ifTrue: [self.x := 1]`), or
    /// is a bare `self.field := ...` write, parentheses aside (BT-3489), IN A
    /// CONTEXT where this `match:` can actually carry that write's family
    /// (`MATCH_ARM_FAMILIES`). `generate_match` checks this once per `match:`
    /// and, when true, compiles every arm's body to a uniform `{Value,
    /// State}` shape so the whole expression can be unwrapped by the same
    /// machinery as `ifTrue:`/`ifFalse:` mutations
    /// (`CoreErlangGenerator::control_flow_has_mutations`'s `Expression::Match`
    /// branch — this function is mutually recursive with it, which is what
    /// lets a nested `match:` arm body be detected too).
    ///
    /// # ADR 0122: capability as data
    ///
    /// The "does a bare field write in this context thread at all" question
    /// (formerly `threads_fields`, a hand-written `is_actor ||
    /// (ValueType && in_class_method())` check) is now answered by
    /// intersecting [`CoreErlangGenerator::eligible_families`] — the shared,
    /// Decision 1 answer to "which storage families exist in the CURRENT
    /// generator context" — with [`MATCH_ARM_FAMILIES`], this site's own
    /// declared capability. The two formulas are equivalent by construction:
    /// `eligible_families` reports at most one family per context (its own
    /// doc comment proves this), and that family is exactly the one the old
    /// formula named — `State` for an Actor instance method, `SelfVt` —
    /// excluded from `MATCH_ARM_FAMILIES` — for a value-type instance
    /// method; a class method has none (ADR 0130). A
    /// value-type instance method DOES still reach this function (a
    /// `match:` with no field-writing arm has nothing for `generate_match`'s
    /// up-front rejection to catch), but `carries_family_write` is `false`
    /// there — `eligible_families` reports only `SelfVt`, which
    /// `MATCH_ARM_FAMILIES` excludes — so the field-write disjunct never
    /// fires for it either; the two checks agree on every arm shape, not
    /// just the rejected one (BT-3489).
    ///
    /// The remaining three disjuncts (Tier 2 value-calls, nested
    /// control-flow-with-mutations, hoistable self-sends) answer a strictly
    /// wider question than "does a family's own bare mutation shape appear"
    /// — [`super::super::control_flow::analysis::body_threaded_families`]'s
    /// `State`-is-trivially-true rule (correct for a loop/conditional's own
    /// always-threaded `StateAcc`) does not apply here without changing what
    /// this function decides — so they stay their own Actor-only checks,
    /// unmigrated by ADR 0122.
    pub(in crate::core_erlang) fn match_needs_state_threading(&self, arms: &[MatchArm]) -> bool {
        let is_actor = self.context == super::super::CodeGenContext::Actor;
        // A field write in a `match:` arm can only be merged back where
        // there is an N-arm-capable version chain to merge it into — exactly
        // the families this site declares it can carry.
        let carries_family_write = self
            .eligible_families()
            .iter()
            .any(|family| MATCH_ARM_FAMILIES.contains(family));
        arms.iter().any(|arm| {
            // BT-3947 (closes BT-3875): at the REPL an arm that writes a binding
            // threads the bindings map, like a REPL conditional's arm.
            let repl_binding_write = self.repl_match_arm_writes_bindings(&arm.body);
            // BT-3489: a `self.field := ...` arm body. Before this, nothing
            // here matched it, so `generate_match` left `base_state` as
            // `None` and the arm compiled through plain `expression_doc`,
            // whose field-write binding (`State1`) is scoped to
            // that one `case` arm — yet the code after the `match:` referenced
            // it unconditionally, so `erlc` rejected the module outright.
            repl_binding_write
                || (carries_family_write && self.is_field_assignment(arm.body.unwrap_parens()))
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

    /// BT-3947: whether the arm body `body` of a REPL `match:` writes a REPL
    /// binding, a nested construct's included: the arm is then inlined over the
    /// bindings map (`generate_repl_arm_inline`). Never outside the REPL.
    fn repl_match_arm_writes_bindings(&self, body: &Expression) -> bool {
        if !self.is_repl_mode() {
            return false;
        }
        let block = Block::new(
            Vec::new(),
            vec![ExpressionStatement::bare(body.unwrap_parens().clone())],
            body.span(),
        );
        self.repl_blocks_write_bindings(&[&block])
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
    fn vt_match_arm_field_write<'a>(&self, arm: &'a MatchArm) -> Option<(&'a str, Span)> {
        let bare = arm.body.unwrap_parens();
        // BT-3495: a field write nested inside a `[...] value` block's own
        // statements (bare, or local-assign-wrapped) — `arm.body` alone is
        // a `MessageSend` (`value`, zero args) here, not the field write
        // itself, so the checks below never see it; `match_needs_state_threading`
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
            && let Expression::Block(block) = receiver.as_ref()
        {
            return block
                .body
                .iter()
                .find_map(|stmt| self.field_write_shape(&stmt.expression))
                .map(Self::field_write_name_and_span);
        }
        self.field_write_shape(bare)
            .map(Self::field_write_name_and_span)
    }

    /// The field write nested in `expr` for exactly the two shapes
    /// [`Self::vt_match_arm_field_write`] and [`Self::generate_match`]'s
    /// bare-arm check need to recognize: `expr` itself is a `self.field :=
    /// ...` write (paren-stripped so `(self.x := 1)` matches too — it
    /// crashes `erlc` identically), or `expr` is a local assignment (`var :=
    /// ...`) whose own RHS is one (BT-3493's `local_assign_field_write`
    /// shape). `None` for every other shape.
    fn field_write_shape<'e>(&self, expr: &'e Expression) -> Option<&'e Expression> {
        let bare = expr.unwrap_parens();
        if self.is_field_assignment(bare) {
            return Some(bare);
        }
        if let Expression::Assignment { target, value, .. } = bare
            && matches!(target.as_ref(), Expression::Identifier(_))
        {
            return self.local_assign_field_write(value);
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

    /// BT-4147: whether the legacy arm body `body` holds a value-position
    /// assignment to a local nothing binds yet — the one shape the legacy
    /// path must not compile as an expression (the write would drop). A
    /// block literal's own statements bind themselves, so only assignments
    /// outside one count.
    fn match_arm_first_binds_local(&self, body: &Expression) -> bool {
        beamtalk_core::semantic_analysis::block_facts::assignments_outside_blocks(body)
            .iter()
            .filter(|(_, span)| self.value_assign_sites.contains(span))
            .any(|(name, _)| !self.binds_threadable_local(name))
    }

    /// Compiles a `match:` arm body.
    ///
    /// When `base_state` is `None` (no arm in this `match:` needs actor state
    /// threading — the common case), this is exactly `expression_doc`,
    /// except an arm body that first-binds a local (BT-4147), which
    /// compiles through `threaded_expression` so the write binds.
    ///
    /// When `base_state` is `Some` (`match_needs_state_threading` found at
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
    ///   named `tier2_local_vars` identifier receiver,
    ///   or a `Cascade` of safe `value:` sends, e.g. `blk value: a; value:
    ///   b`), for which `expression_doc` already unwraps/discards that
    ///   call's own state via `close_tier2_value_subexpr_doc` (a
    ///   sub-expression-position limitation) — is wrapped unchanged as
    ///   `{<value>, <base_state>}`.
    pub(super) fn generate_match_arm_body(
        &mut self,
        body: &Expression,
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        let Some(threading) = threading else {
            // BT-4147: an arm body that first-binds a local (`1 -> (u :=
            // 11)` with `u` bound nowhere else) compiles through
            // `threaded_expression`, like the scrutinee (BT-4146): the
            // assignment is a producer (BT-3910), so its prelude binds the
            // local and answers the value — bare `expression_doc` would
            // drop the write and report it. The `match:` itself is not a
            // producer for such a write (producers thread outer-bound
            // locals only), and anything else keeps the legacy path
            // byte-identically. A bound write stays loud: analysis rejects
            // it as MatchArmWrite. (Not the synthetic-block arm lowering
            // below: that answers a `{Value, StateAcc}` tuple, which has
            // no `StateAcc` to close over on this path.)
            if self.match_arm_first_binds_local(body) {
                let frame = self.current_frame();
                let tv = self.threaded_expression(body, frame)?;
                let preamble = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                return Ok(docvec![preamble, value_doc]);
            }
            return self.expression_doc(body);
        };
        // BT-3947: an arm of a producer is a `BranchArm` frame, lowered as the
        // one-statement block a conditional's arm is: the statements of the arm
        // (its body) are classified, threaded and closed with `{Value, StateAcc}`
        // by the one arm lowering, whatever the body is.
        if let Some(arm) = threading.arm {
            let block = Block::new(
                Vec::new(),
                vec![ExpressionStatement::bare(body.clone())],
                body.span(),
            );
            return self.conditional_arm_clause(&block, None, Some(arm), threading.base_state);
        }
        let base_state = threading.base_state;
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
                // a named tier2_local_vars identifier, or
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
        if self.conditional_receiver_needs_threading(body)
            || self.is_field_assignment(bare_body)
            || self.repl_match_arm_writes_bindings(body)
        {
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
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            // BT-3943: user-visible no-arm failure is a structured
            // `#pattern_mismatch`, not a raw `case_clause` (`internal_error`).
            return Ok(self.pattern_mismatch_raise_doc(
                match_var,
                "match:",
                "no match: arm matches the value",
            ));
        }

        let arm = &arms[0];
        let rest = &arms[1..];

        if let Pattern::Array { elements, .. } = &arm.pattern {
            // Generate current arm body FIRST so that state_version and temp-var counters
            // are not advanced by later arms before we codegen the current one.
            return self.generate_array_match_arm(match_var, arm, elements, rest, threading);
        }

        // ADR 0107 Phase A: `Pattern::Type` (`binding :: ClassName`)
        // dispatches to a per-class runtime test (BIF test, atom-exclusion
        // guard, or map-tag check) — see `generate_type_pattern`. Like
        // `Pattern::Array` above, this can't reuse the plain native-arm path
        // below because at least one class (`Dictionary`, or an exact tagged
        // `Value`/sealed class) needs a `maps:get` test in case-scrutinee
        // position, which is not expressible as a single Core Erlang guard.
        if let Pattern::Type { binding, class, .. } = &arm.pattern {
            return self.generate_type_pattern(match_var, arm, binding, class, rest, threading);
        }

        // BT-4096: a top-level binary pattern with dependent sizes compiles
        // to nested matches (Core sizes must be bound before the pattern),
        // which needs per-arm fallthrough like the Array/Type arms above.
        if let Pattern::Binary { segments, .. } = &arm.pattern {
            if binary_needs_segmented(segments) {
                return self
                    .generate_segmented_binary_arm(match_var, arm, segments, rest, threading);
            }
        } else {
            // BT-4223: a binary nested inside a composite pattern can't
            // unwrap a `String` element in native pattern position, so it
            // is hoisted out to a fresh variable and matched afterwards.
            let mut outer = arm.pattern.clone();
            let mut hoisted = Vec::new();
            self.hoist_nested_binaries(&mut outer, &mut hoisted);
            if !hoisted.is_empty() {
                return self
                    .generate_nested_binary_arm(match_var, arm, &outer, &hoisted, rest, threading);
            }
        }

        // Native arm: case _Match of <pattern> when guard -> body <_FT> when 'true' -> rest end
        // Compute current arm first — body codegen must not see state advanced by later arms.
        let fallthrough_var = self.fresh_temp_var("NoMatch");
        let pattern_doc = self.generate_pattern(&arm.pattern)?;
        // BT-4173: a top-level binary arm matches against unwrapped bytes
        // (a `String` subject matches like the `Binary` it encodes); every
        // other arm keeps the raw scrutinee.
        let (scrutinee_prefix, scrutinee_var) = if matches!(arm.pattern, Pattern::Binary { .. }) {
            let (prefix, var) = self.binary_match_scrutinee(match_var);
            (prefix, var)
        } else {
            (Document::Vec(Vec::new()), match_var.to_string())
        };

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
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        // Rest is generated AFTER the current arm to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest, threading)?;

        Ok(docvec![
            scrutinee_prefix,
            "case ",
            leaf::var(scrutinee_var),
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

    /// Compiles the later arms of a match chain once and binds them to a
    /// zero-arity `letrec` label, for an arm that falls through from several
    /// sites (BT-4141). Returns the label's binding prefix (ending in `in `,
    /// wrapped around the arm by the caller) and the `apply` every miss site
    /// uses in place of a copy of the later arms.
    ///
    /// Without it each such arm embedded a full copy of the rest of the
    /// chain at every miss site, so N consecutive Array/Type/binary arms
    /// emitted O(k^N) Core. `erlc` shares a fallthrough the same way. A
    /// `letrec` that is only applied is lambda-lifted to a local function
    /// (no closure is allocated, unlike a `let`-bound fun). The label is
    /// defined outside the arm's own `case`s, so the later arms' free
    /// variables resolve exactly as before: the arm's pattern bindings are
    /// popped before the rest is generated, and threading state (`StateAcc`,
    /// the producer's base map) is bound outside the whole chain.
    pub(super) fn generate_shared_rest(
        &mut self,
        match_var: &str,
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<(Document<'static>, Document<'static>)> {
        let rest_doc = self.generate_match_chain(match_var, rest_arms, threading)?;
        let label = self.fresh_temp_var("Fallthrough");
        let bind = docvec![
            "letrec ",
            leaf::fname(label.clone(), 0),
            " = fun () -> ",
            rest_doc,
            " in "
        ];
        let apply = docvec!["apply ", leaf::fname(label, 0), " ()"];
        Ok((bind, apply))
    }

    /// Compiles a single `Pattern::Array` match arm.
    ///
    /// Emits a three-level conditional check (`is_map` → class → size), then element
    /// extractions via `at:` dispatch. Every failure branch applies the shared
    /// fallthrough label ([`Self::generate_shared_rest`]).
    ///
    /// Takes `rest_arms` (not a pre-built `rest_doc`) so that the current arm's body
    /// is fully generated before any later arm's codegen advances generator state.
    fn generate_array_match_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        elements: &[Pattern],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
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
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        // Generate rest AFTER the current arm body to prevent state leakage.
        let (rest_bind, rest_doc) = self.generate_shared_rest(match_var, rest_arms, threading)?;

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
            rest_bind,
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

    /// Compiles a top-level binary arm with dependent sizes (BT-4096).
    ///
    /// Core Erlang requires segment sizes bound *before* the pattern, so the
    /// arm's chunks (see [`plan_binary_split`]) become nested matches over
    /// `('all',1,'binary')` rest tails — exactly what `erlc` emits for the
    /// same Erlang. Non-trivial size expressions are evaluated into fresh
    /// variables under `try`/`catch` before their chunk's match, so a
    /// raising size fails the arm instead of crashing (review finding 1).
    /// The arm guard applies at the innermost level (all variables bound);
    /// every level falls through to the later arms.
    /// BT-4173: binds a fresh scrutinee for a top-level binary arm — a
    /// `String` subject (`{'beamtalk_string', bytes}`) unwraps to its bytes
    /// so `<<...>>` keeps matching Strings as UTF-8 binaries (BT-663); a
    /// bare `Binary` (or any other value) passes through untouched, so
    /// non-binaries still fail the native binary pattern and fall through.
    /// `/binary` segments therefore always bind bare binaries (`Binary`).
    ///
    /// Returns the `let`-prefix (ending in `in `) plus the fresh variable
    /// name — the same open-`let`-chain convention
    /// `interpolation_segment_chain` uses.
    fn binary_match_scrutinee(&mut self, match_var: &str) -> (Document<'static>, String) {
        let scrutinee = self.fresh_temp_var("BinMatch");
        let inner = self.fresh_temp_var("BinInner");
        let other = self.fresh_temp_var("BinOther");
        let prefix = docvec![
            "let ",
            leaf::var(scrutinee.clone()),
            " = case ",
            leaf::var(match_var.to_string()),
            " of ",
            "<{'beamtalk_string', ",
            leaf::var(inner.clone()),
            "}> when 'true' -> ",
            leaf::var(inner),
            " <",
            leaf::var(other.clone()),
            "> when 'true' -> ",
            leaf::var(other),
            " end in ",
        ];
        (prefix, scrutinee)
    }

    /// BT-4223: replaces every binary sub-pattern nested inside a composite
    /// pattern (tuple, list, map value, constructor binding) with a fresh
    /// variable, collecting `(variable, binary pattern)` pairs in source
    /// order. A top-level binary arm is not hoisted — callers route it
    /// through [`Self::binary_match_scrutinee`] directly.
    fn hoist_nested_binaries(
        &mut self,
        pattern: &mut Pattern,
        hoisted: &mut Vec<(String, Pattern)>,
    ) {
        match pattern {
            Pattern::Tuple { elements, .. } => {
                for elem in elements {
                    self.hoist_binary_subpattern(elem, hoisted);
                }
            }
            Pattern::List { elements, tail, .. } => {
                for elem in elements {
                    self.hoist_binary_subpattern(elem, hoisted);
                }
                if let Some(tail) = tail {
                    self.hoist_binary_subpattern(tail, hoisted);
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    self.hoist_binary_subpattern(&mut pair.value, hoisted);
                }
            }
            Pattern::Constructor { keywords, .. } => {
                for (_, binding) in keywords {
                    self.hoist_binary_subpattern(binding, hoisted);
                }
            }
            // A top-level binary is matched by its own arm path; `Array`
            // and `Type` sub-patterns are rejected by `generate_pattern`;
            // leaves hold no sub-patterns.
            Pattern::Binary { .. }
            | Pattern::Array { .. }
            | Pattern::Type { .. }
            | Pattern::Wildcard(_)
            | Pattern::Literal(..)
            | Pattern::Nil(_)
            | Pattern::Variable(_) => {}
        }
    }

    /// One position of [`Self::hoist_nested_binaries`]: a binary sub-pattern
    /// is swapped for a fresh variable; anything else is searched further.
    fn hoist_binary_subpattern(&mut self, sub: &mut Pattern, hoisted: &mut Vec<(String, Pattern)>) {
        if let Pattern::Binary { span, .. } = sub {
            let tmp = self.fresh_temp_var("BinNested");
            let replacement = Pattern::Variable(Identifier::new(tmp.as_str(), *span));
            let binary = std::mem::replace(sub, replacement);
            hoisted.push((tmp, binary));
        } else {
            self.hoist_nested_binaries(sub, hoisted);
        }
    }

    /// Compiles a match arm whose composite pattern nests binary
    /// sub-patterns (BT-4223).
    ///
    /// Strings are tagged `{'beamtalk_string', bytes}` (BT-4173), so a
    /// native binary sub-pattern would never match a `String` element. The
    /// binaries are hoisted to fresh variables (`outer`/`hoisted`, see
    /// [`Self::hoist_nested_binaries`]); the outer pattern matches first,
    /// then each hoisted value is unwrapped by
    /// [`Self::binary_match_scrutinee`] — exactly like a top-level binary
    /// arm — and matched against its binary pattern in a nested `case`.
    /// The arm guard applies at the innermost level (every variable bound);
    /// every level falls through to the later arms.
    fn generate_nested_binary_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        outer: &Pattern,
        hoisted: &[(String, Pattern)],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // Bind all pattern variables, generate guard + body first (mirror
        // the array arm: later arms must not advance generator state first).
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let mut guard_part = if let Some(guard) = &arm.guard {
            let gd = self.generate_guard_expression(guard)?;
            Some(docvec![" when ", gd])
        } else {
            None
        };
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        let (rest_bind, rest_doc) = self.generate_shared_rest(match_var, rest_arms, threading)?;

        // Each level runs after the outer pattern and every earlier hoisted
        // binary have matched, so a segment size may name their variables
        // (`{n, <<payload:n/binary>>}`): mirror exactly those bindings in
        // scope while each binary pattern is lowered.
        let outer_doc = self.generate_pattern(outer)?;
        self.push_scope();
        Self::collect_pattern_variables(outer, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let mut levels = Vec::with_capacity(hoisted.len());
        for (var, binary) in hoisted {
            let (prefix, scrutinee) = self.binary_match_scrutinee(var);
            let pattern_doc = match self.generate_pattern(binary) {
                Ok(doc) => doc,
                Err(err) => {
                    self.pop_scope();
                    return Err(err);
                }
            };
            Self::collect_pattern_variables(binary, |name, core_var| {
                self.bind_var(name, core_var);
            });
            levels.push((prefix, scrutinee, pattern_doc));
        }
        self.pop_scope();

        // Assemble innermost-first: the last level carries the guard.
        let mut inner = body_doc;
        for (prefix, scrutinee, pattern_doc) in levels.into_iter().rev() {
            let guard = guard_part.take().unwrap_or(Document::Str(" when 'true'"));
            let fallthrough = self.fresh_temp_var("NoMatch");
            inner = docvec![
                prefix,
                "case ",
                leaf::var(scrutinee),
                " of <",
                pattern_doc,
                ">",
                guard,
                " -> ",
                inner,
                " <",
                leaf::var(fallthrough),
                "> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ];
        }

        let fallthrough = self.fresh_temp_var("NoMatch");
        Ok(docvec![
            rest_bind,
            "case ",
            leaf::var(match_var.to_string()),
            " of <",
            outer_doc,
            "> when 'true' -> ",
            inner,
            " <",
            leaf::var(fallthrough),
            "> when 'true' -> ",
            rest_doc,
            " end"
        ])
    }

    fn generate_segmented_binary_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        segments: &[BinarySegment],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // Bind all pattern variables, generate guard + body first (mirror
        // the array arm: later arms must not advance generator state first).
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
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        let (rest_bind, rest_doc) = self.generate_shared_rest(match_var, rest_arms, threading)?;

        let plan = plan_binary_split(segments);
        debug_assert!(
            plan.len() > 1 || plan.iter().any(|chunk| !chunk.hoists.is_empty()),
            "dispatch only routes split or hoisted plans here"
        );

        // BT-4173: match the first chunk against unwrapped bytes so a
        // `String` subject matches like the `Binary` it encodes.
        let (scrutinee_prefix, scrutinee) = self.binary_match_scrutinee(match_var);
        let levels =
            self.build_segmented_levels(&scrutinee, arm.pattern.span(), segments, &plan)?;
        Ok(docvec![
            rest_bind,
            scrutinee_prefix,
            self.assemble_segmented_levels(levels, guard_part, body_doc, &rest_doc)
        ])
    }

    /// Emits one [`SegmentedLevel`] per chunk of a split plan (BT-4096).
    ///
    /// Runs in a single emission scope mirroring the Core-bound variables
    /// (earlier chunks, hoists) so size validation resolves exactly what
    /// the nested matches bind.
    fn build_segmented_levels(
        &mut self,
        match_var: &str,
        arm_span: Span,
        segments: &[BinarySegment],
        plan: &[BinaryChunkPlan],
    ) -> Result<Vec<SegmentedLevel>> {
        self.push_scope();
        let mut levels: Vec<SegmentedLevel> = Vec::with_capacity(plan.len());
        let mut scrutinee = match_var.to_string();
        for (chunk_idx, chunk) in plan.iter().enumerate() {
            let mut hoists = Vec::with_capacity(chunk.hoists.len());
            let mut synthetic: Vec<BinarySegment> = Vec::with_capacity(chunk.end - chunk.start + 1);
            for (offset, seg) in segments[chunk.start..chunk.end].iter().enumerate() {
                let i = chunk.start + offset;
                let size = match &seg.size {
                    Some(size) if chunk.hoists.contains(&i) => {
                        self.check_binary_size(size)?;
                        let fresh = self.fresh_temp_var("Size");
                        let hoist_doc = self.generate_guard_expression(size)?;
                        self.bind_var(&fresh, &fresh);
                        hoists.push((fresh.clone(), hoist_doc));
                        Some(Box::new(Expression::Identifier(Identifier::new(
                            fresh.as_str(),
                            seg.span,
                        ))))
                    }
                    other => other.clone(),
                };
                synthetic.push(BinarySegment {
                    value: seg.value.clone(),
                    size,
                    segment_type: seg.segment_type,
                    signedness: seg.signedness,
                    endianness: seg.endianness,
                    unit: seg.unit,
                    span: seg.span,
                });
            }
            // Non-final chunks match a rest tail for the next level, exactly
            // as `erlc` does: `('all',1,'binary')` (unit 1: the split point
            // may be bit-unaligned).
            let next_scrutinee = if chunk_idx + 1 < plan.len() {
                let tmp = self.fresh_temp_var("Split");
                synthetic.push(BinarySegment {
                    value: Pattern::Variable(Identifier::new(tmp.as_str(), arm_span)),
                    size: None,
                    segment_type: Some(BinarySegmentType::Binary),
                    signedness: None,
                    endianness: None,
                    unit: Some(1),
                    span: arm_span,
                });
                self.bind_var(&tmp, &tmp);
                tmp
            } else {
                String::new()
            };
            let chunk_doc = self.generate_pattern(&Pattern::Binary {
                segments: synthetic,
                span: arm_span,
            })?;
            // Bind this chunk's values for later chunks' sizes.
            for seg in &segments[chunk.start..chunk.end] {
                if let Pattern::Variable(id) = &seg.value {
                    let core_var = Self::to_core_erlang_var(&id.name);
                    self.bind_var(&id.name, &core_var);
                }
            }
            levels.push(SegmentedLevel {
                scrutinee: std::mem::replace(&mut scrutinee, next_scrutinee),
                chunk_doc,
                hoists,
            });
        }
        self.pop_scope();
        Ok(levels)
    }

    /// Assembles [`SegmentedLevel`]s inside-out: the guard applies
    /// innermost; every level falls through to the later arms; each level's
    /// hoists evaluate under `try`/`catch` before its own match (see
    /// [`Self::wrap_hoists_in_try`]).
    fn assemble_segmented_levels(
        &mut self,
        levels: Vec<SegmentedLevel>,
        guard_part: Document<'static>,
        body_doc: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        let mut rev = levels.into_iter().rev();
        let last = rev.next().expect("a split plan always has chunks");
        let fallthrough_var = self.fresh_temp_var("NoMatch");
        let mut next = docvec![
            "case ",
            leaf::var(last.scrutinee),
            " of <",
            last.chunk_doc,
            ">",
            guard_part,
            " -> ",
            body_doc,
            " <",
            leaf::var(fallthrough_var),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end"
        ];
        next = self.wrap_hoists_in_try(last.hoists, next, rest_doc);
        for level in rev {
            let fallthrough_var = self.fresh_temp_var("NoMatch");
            next = docvec![
                "case ",
                leaf::var(level.scrutinee),
                " of <",
                level.chunk_doc,
                "> when 'true' -> ",
                next,
                " <",
                leaf::var(fallthrough_var),
                "> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ];
            next = self.wrap_hoists_in_try(level.hoists, next, rest_doc);
        }
        next
    }

    /// Wraps `next` in one `try`/`catch` per hoist (review finding 1).
    ///
    /// A raising size expression must fail the arm — not crash the match —
    /// so each hoist evaluates under `try`, whose catch answers the later
    /// arms. Observably equivalent to Erlang's `try` → `'bad_size'` (the
    /// whole-scrutinee pre-match `erlc` adds is only an optimization: with
    /// catch-to-rest, a non-binary scrutinee falls through identically).
    fn wrap_hoists_in_try(
        &mut self,
        hoists: Vec<(String, Document<'static>)>,
        next: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        let mut next = next;
        for (var, value) in hoists.into_iter().rev() {
            let (t, e, s) = (
                self.fresh_temp_var("Catch"),
                self.fresh_temp_var("Catch"),
                self.fresh_temp_var("Catch"),
            );
            // Note: concrete Core `try` takes no `end` terminator (unlike
            // `case`/`let` nesting might suggest — see `erlc +to_core`
            // output or `number_coercion_try_catch`).
            next = docvec![
                "try ",
                value,
                " of <",
                leaf::var(var),
                "> -> ",
                next,
                " catch <",
                leaf::var(t),
                ", ",
                leaf::var(e),
                ", ",
                leaf::var(s),
                "> -> ",
                rest_doc.clone()
            ];
        }
        next
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
            // BT-4098: in Core Erlang `_` is an ordinary variable name, so
            // two of them in one pattern are a duplicate binding. Freshen
            // every occurrence (underscore-prefixed: no unused warnings).
            Pattern::Wildcard(_) => Ok(leaf::var(self.fresh_temp_var("Wildcard"))),
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
            Pattern::Binary { segments, .. } => self.generate_binary_pattern(segments),
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

    /// Generates a Core Erlang binary pattern: `#{seg,seg,...}#` (BT-4096).
    ///
    /// Segment sizes resolve left to right. A size naming a variable bound
    /// by an earlier segment of this same pattern cannot emit inline — Core
    /// Erlang requires sizes bound before the pattern — so it is a clean
    /// error here (top-level match arms split instead; see the chain
    /// dispatch, which never routes them here). Anything else bound
    /// (parameters, locals, earlier chunks) emits inline. Each segment's
    /// value binds into a scope that ends with the pattern; the caller binds
    /// all pattern variables separately for the guard/body.
    fn generate_binary_pattern(&mut self, segments: &[BinarySegment]) -> Result<Document<'static>> {
        self.push_scope();
        let mut bound_here: HashSet<&str> = HashSet::new();
        let mut parts = vec![Document::Str("#{")];
        for (i, seg) in segments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(","));
            }
            if let Some(size) = &seg.size {
                if let Some(name) = size_vars(size).iter().find(|v| bound_here.contains(*v)) {
                    self.pop_scope();
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: format!(
                            "variable '{name}' cannot be used as a binary segment size here: it is bound by an earlier segment of the same pattern, and Core Erlang requires sizes bound before the pattern (top-level match arms split automatically; nested patterns must match the enclosing value first)"
                        ),
                        span: Some(seg.span),
                    });
                }
            }
            parts.push(self.generate_binary_segment(seg)?);
            if let Pattern::Variable(id) = &seg.value {
                let core_var = Self::to_core_erlang_var(&id.name);
                self.bind_var(&id.name, &core_var);
                bound_here.insert(id.name.as_str());
            }
        }
        parts.push(Document::Str("}#"));
        self.pop_scope();
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang binary segment size (BT-4096, BT-4097).
    ///
    /// A size is a guard position in Core Erlang: only literals, bound
    /// variables, and arithmetic/comparison expressions are valid. Anything
    /// else — a field read, a message send, a variable bound only later in
    /// the pattern — would compile to Core Erlang that `erlc` rejects, so
    /// it is a clean compile error here instead.
    fn generate_binary_size(&mut self, size: &Expression) -> Result<Document<'static>> {
        self.check_binary_size(size)?;
        self.generate_guard_expression(size)
            .map_err(|err| match err {
                CodeGenError::UnsupportedFeature { feature, span } => {
                    CodeGenError::UnsupportedFeature {
                        feature: format!("in binary segment size: {feature}"),
                        span,
                    }
                }
                other => other,
            })
    }

    /// Validates a segment size before [`Self::generate_binary_size`] lowers it.
    ///
    /// Every identifier must already be bound (an earlier segment's value, a
    /// parameter, or a local); anything that is not a literal, a variable, or
    /// a message send is rejected outright (operator validity is left to the
    /// guard lowering, which rephrases the error with size context).
    fn check_binary_size(&self, size: &Expression) -> Result<()> {
        match size {
            Expression::Literal(..) => Ok(()),
            Expression::Identifier(id) => {
                if self.lookup_var(id.name.as_str()).is_none() {
                    Err(CodeGenError::UnsupportedFeature {
                        feature: format!(
                            "variable '{}' cannot be used as a binary segment size here: it is not bound to a value. \
                             A size may name a variable bound by an earlier segment of the same binary pattern, \
                             a method parameter, or a local variable (assign a field to a local first)",
                            id.name
                        ),
                        span: Some(id.span),
                    })
                } else {
                    Ok(())
                }
            }
            Expression::Parenthesized { expression, .. } => self.check_binary_size(expression),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                self.check_binary_size(receiver)?;
                for arg in arguments {
                    self.check_binary_size(arg)?;
                }
                Ok(())
            }
            other => Err(CodeGenError::UnsupportedFeature {
                feature: "only literals, variables, and arithmetic/comparison expressions are supported in a binary segment size".to_string(),
                span: Some(other.span()),
            }),
        }
    }

    /// Generates a Core Erlang binary segment: `#<Value>(Size,Unit,Type,Flags)`.
    ///
    /// Core Erlang binary segment format:
    /// ```text
    /// #<VarName>(Size, Unit, Type, ['flag1'|['flag2']])
    /// ```
    ///
    /// Defaults: type=integer, signedness=unsigned, endianness=big,
    /// size=8 for integer/float, 'all' for binary/bitstring, 'undefined' for utf8.
    /// Unit=1 for integer/float/bitstring, 8 for binary, 'undefined' for utf8.
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
            self.generate_binary_size(size_expr)?
        } else {
            match seg_type {
                BinarySegmentType::Binary | BinarySegmentType::Bitstring => Document::Str("'all'"),
                BinarySegmentType::Utf8 => Document::Str("'undefined'"),
                BinarySegmentType::Integer => Document::Str("8"),
                BinarySegmentType::Float => Document::Str("64"),
            }
        };

        // Unit: explicit override (synthetic split tails, BT-4096) or the
        // type default — 8 for binary, 1 for bitstring (bits) and
        // integer/float, 'undefined' for utf8.
        let unit_doc = match seg.unit {
            Some(unit) => Document::Owned(unit.to_string()),
            None => match seg_type {
                BinarySegmentType::Binary => Document::Str("8"),
                BinarySegmentType::Utf8 => Document::Str("'undefined'"),
                BinarySegmentType::Integer
                | BinarySegmentType::Float
                | BinarySegmentType::Bitstring => Document::Str("1"),
            },
        };

        // Type atom. Note: `bits`/`bitstring` segments emit the `binary`
        // type with unit 1 — Core Erlang patterns have no `bitstring` type;
        // this is exactly what `erlc` emits for `/bits` and `/bitstring`.
        let type_doc = match seg_type {
            BinarySegmentType::Integer => Document::Str("'integer'"),
            BinarySegmentType::Float => Document::Str("'float'"),
            BinarySegmentType::Binary | BinarySegmentType::Bitstring => Document::Str("'binary'"),
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
            // Parentheses are transparent in guards (and in segment sizes,
            // which lower through this function): `when: [(x + 1) > 2]`.
            Expression::Parenthesized { expression, .. } => {
                self.generate_guard_expression(expression)
            }
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

/// One Core Erlang binary pattern's worth of segments (BT-4096).
///
/// A Core binary pattern's sizes must be bound *before* the pattern, so a
/// segment whose size names a variable bound by an earlier segment of the
/// same chunk starts a new chunk; each chunk matches the previous chunk's
/// rest tail (mirroring what `erlc` emits for the same Erlang).
pub(in crate::core_erlang) struct BinaryChunkPlan {
    /// First segment index (inclusive) within the arm's segment list.
    pub(in crate::core_erlang) start: usize,
    /// Last segment index (exclusive) within the arm's segment list.
    pub(in crate::core_erlang) end: usize,
    /// Absolute indices of segments whose size is a non-trivial expression,
    /// replaced by a hoisted fresh variable evaluated before this chunk.
    pub(in crate::core_erlang) hoists: Vec<usize>,
}

/// Splits binary segments into [`BinaryChunkPlan`]s (BT-4096, BT-4097).
///
/// Pure over the pattern AST: a size naming a variable bound by an earlier
/// segment of the current chunk forces a split there; names bound by
/// earlier chunks or outside the pattern need none (validated at emission).
/// More than one chunk means the arm needs segmented lowering.
pub(in crate::core_erlang) fn plan_binary_split(
    segments: &[BinarySegment],
) -> Vec<BinaryChunkPlan> {
    let mut chunks = Vec::new();
    let mut chunk_start = 0;
    let mut hoists = Vec::new();
    let mut bound_in_chunk: HashSet<&str> = HashSet::new();
    for (i, seg) in segments.iter().enumerate() {
        if let Some(size) = &seg.size {
            if size_vars(size).iter().any(|v| bound_in_chunk.contains(*v)) {
                chunks.push(BinaryChunkPlan {
                    start: chunk_start,
                    end: i,
                    hoists: std::mem::take(&mut hoists),
                });
                chunk_start = i;
                bound_in_chunk.clear();
            }
            if !is_trivial_size(size) {
                hoists.push(i);
            }
        }
        if let Pattern::Variable(id) = &seg.value {
            bound_in_chunk.insert(id.name.as_str());
        }
    }
    chunks.push(BinaryChunkPlan {
        start: chunk_start,
        end: segments.len(),
        hoists,
    });
    chunks
}

/// True when a top-level binary pattern needs segmented lowering: more
/// than one chunk, or hoisted size expressions (BT-4096, BT-4097). Either
/// way the pattern cannot emit as one native Core binary pattern.
fn binary_needs_segmented(segments: &[BinarySegment]) -> bool {
    let plan = plan_binary_split(segments);
    plan.len() > 1 || plan.iter().any(|chunk| !chunk.hoists.is_empty())
}

/// True for sizes that emit inline: literals and variables, with any
/// parentheses unwrapped. Only genuine expressions hoist, so `<<a:(8)>>`
/// stays a one-pattern match (review finding 6).
fn is_trivial_size(size: &Expression) -> bool {
    match size {
        Expression::Literal(..) | Expression::Identifier(..) => true,
        Expression::Parenthesized { expression, .. } => is_trivial_size(expression),
        _ => false,
    }
}

/// Collects every variable named in a segment size expression.
fn size_vars(size: &Expression) -> Vec<&str> {
    fn inner<'a>(size: &'a Expression, vars: &mut Vec<&'a str>) {
        match size {
            Expression::Identifier(id) => vars.push(id.name.as_str()),
            Expression::Parenthesized { expression, .. } => inner(expression, vars),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                inner(receiver, vars);
                for arg in arguments {
                    inner(arg, vars);
                }
            }
            _ => {}
        }
    }
    let mut vars = Vec::new();
    inner(size, &mut vars);
    vars
}

/// Whether `pattern` is, or nests inside a composite pattern, a binary
/// pattern (BT-4173/BT-4223): such an arm needs a per-arm `String` unwrap
/// the all-native `case` fast path can't express.
fn pattern_has_binary(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Binary { .. } => true,
        Pattern::Tuple { elements, .. } => elements.iter().any(pattern_has_binary),
        Pattern::List { elements, tail, .. } => {
            elements.iter().any(pattern_has_binary)
                || tail.as_deref().is_some_and(pattern_has_binary)
        }
        Pattern::Map { pairs, .. } => pairs.iter().any(|pair| pattern_has_binary(&pair.value)),
        Pattern::Constructor { keywords, .. } => keywords
            .iter()
            .any(|(_, binding)| pattern_has_binary(binding)),
        Pattern::Array { .. }
        | Pattern::Type { .. }
        | Pattern::Wildcard(_)
        | Pattern::Literal(..)
        | Pattern::Nil(_)
        | Pattern::Variable(_) => false,
    }
}

/// Span of a binary pattern needing segmented lowering nested inside a
/// composite pattern, if any. Only a top-level match-arm binary pattern
/// splits (BT-4096); anything nested is a clean compile error at the call
/// site.
fn nested_split_span(pattern: &Pattern) -> Option<Span> {
    match pattern {
        Pattern::Binary { segments, span } => {
            if binary_needs_segmented(segments) {
                Some(*span)
            } else {
                // Non-split binaries are fine nested — but recurse into
                // segment values defensively (constructed ASTs only).
                segments
                    .iter()
                    .find_map(|seg| nested_split_span(&seg.value))
            }
        }
        Pattern::Tuple { elements, .. } | Pattern::List { elements, .. } => {
            elements.iter().find_map(nested_split_span).or_else(|| {
                if let Pattern::List { tail: Some(t), .. } = pattern {
                    nested_split_span(t)
                } else {
                    None
                }
            })
        }
        Pattern::Array { elements, rest, .. } => elements
            .iter()
            .find_map(nested_split_span)
            .or_else(|| rest.as_ref().and_then(|r| nested_split_span(r))),
        Pattern::Map { pairs, .. } => pairs.iter().find_map(|pair| nested_split_span(&pair.value)),
        Pattern::Constructor { keywords, .. } => {
            keywords.iter().find_map(|(_, b)| nested_split_span(b))
        }
        Pattern::Wildcard(_)
        | Pattern::Literal(..)
        | Pattern::Nil(_)
        | Pattern::Variable(_)
        | Pattern::Type { .. } => None,
    }
}

/// One nested level of a segmented binary arm (BT-4096): this chunk's
/// pattern, the size-expression hoists evaluated before it, and the
/// scrutinee it matches.
struct SegmentedLevel {
    scrutinee: String,
    chunk_doc: Document<'static>,
    hoists: Vec<(String, Document<'static>)>,
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
