// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utility functions for Core Erlang code generation.
//!
//! This module provides helper functions for:
//! - Indentation management
//! - Variable name generation (fresh variables, temp variables)
//! - State variable threading (State, State1, State2, ...)
//! - Name conversions (class names, module names)
//! - Class identity (DDD Value Object bundling module + class names)

use std::fmt::Write as _;

use super::threaded_ir::{
    FrameId, RenderCtx, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, render, render_value,
};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, PrecompiledScope, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::{atom, string_lit};
use beamtalk_cerl_doc::{Document, join};
use beamtalk_core::ast::{
    CascadeMessage, ClassDefinition, Expression, ExpressionStatement, Identifier, MessageSelector,
    StringSegment,
};
use beamtalk_core::source_analysis::Span;

/// One cascade message's decomposed shape — selector, arguments, and the
/// message's own span — as [`CoreErlangGenerator::cascade_self_dispatch_messages`]
/// and [`CoreErlangGenerator::threaded_cascade_on_self`] pass it around.
/// Named so the type isn't repeated (and so clippy's `type_complexity`
/// lint stays quiet about the tuple).
type CascadeSelfMessage<'e> = (&'e MessageSelector, &'e [Expression], Span);

/// Builds a versioned Core Erlang variable name.
///
/// Returns `prefix` when `version == 0`, otherwise `prefix{version}`
/// (e.g. `"State"`, `"State1"`, `"StateAcc2"`, `"ClassVars1"`, `"Self3"`).
///
/// Uses [`std::fmt::Write`] on a pre-allocated buffer rather than `format!()`
/// to comply with CLAUDE.md: "NEVER use `format!()` to produce Core Erlang
/// fragments."  See also: `variable_context::VariableContext::fresh_var` which
/// uses the same `write!` pattern (introduced in BT-875).
///
/// # Examples
///
/// ```ignore
/// assert_eq!(versioned_var("State", 0), "State");
/// assert_eq!(versioned_var("State", 1), "State1");
/// assert_eq!(versioned_var("StateAcc", 2), "StateAcc2");
/// ```
pub(super) fn versioned_var(prefix: &str, version: usize) -> String {
    if version == 0 {
        prefix.to_string()
    } else {
        // Pre-allocate: prefix length + up to 4 digits (handles version ≤ 9999).
        let mut s = String::with_capacity(prefix.len() + 4);
        s.push_str(prefix);
        // Write the counter directly instead of using format!.
        let _ = write!(s, "{version}");
        s
    }
}

/// Builds the metaclass tag string `"{class_name} class"`.
///
/// This is the naming convention for metaclass atoms throughout the Beamtalk runtime
/// (e.g. `'Array class'`, `'Object class'`). Extracted as a named helper so the
/// convention is documented in one place and callers don't need to remember the
/// exact suffix.
pub(super) fn metaclass_tag(class_name: &str) -> String {
    // " class" is 6 bytes.
    let mut s = String::with_capacity(class_name.len() + 6);
    s.push_str(class_name);
    s.push_str(" class");
    s
}

/// Builds a stable, self-contained extension binding name `"_Ext{idx}"`.
///
/// Using the per-module loop index keeps snapshot values stable across unrelated
/// codegen changes that would otherwise shift the global temp-var counter.
/// Collision with `fresh_var("Ext")` (yielding `_Ext1`, `_Ext2`, …) is not a
/// concern in practice because no call site uses `"Ext"` as a `fresh_var` base.
pub(super) fn ext_var(idx: usize) -> String {
    // "_Ext" is 4 bytes; reserve a few more for the digits.
    let mut s = String::with_capacity(8);
    s.push_str("_Ext");
    let _ = write!(s, "{idx}");
    s
}

/// BT-745: Generate a `'beamtalk_class' = [{...}]` attribute fragment for the
/// module attributes section. Returns `Document::Nil` when classes is empty.
pub(super) fn beamtalk_class_attribute(classes: &[ClassDefinition]) -> Document<'static> {
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|c| {
        docvec![
            "{",
            atom(c.name.name.to_string()),
            ", ",
            atom(c.superclass_name().to_string()),
            "}"
        ]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]"
    ]
}

/// Filters `@expect` directives from a statement body, returning only the
/// codegen-relevant expressions.  This is the canonical extraction point —
/// every call-site that previously inlined this `.iter().map().filter()` chain
/// should call this helper instead.
pub(super) fn collect_body_exprs(body: &[ExpressionStatement]) -> Vec<&Expression> {
    body.iter()
        .map(|s| &s.expression)
        .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
        .collect()
}

/// BT-940: `'file'` module attribute helper for BEAM stacktrace file names.
impl CoreErlangGenerator {
    /// Returns `, 'file' = [{"<path>", 1}]` when `source_path` is set,
    /// or `Document::Nil` when no source path is available.
    ///
    /// The `erlc` compiler uses the `'file'` attribute to populate the BEAM
    /// `Line` chunk file table, making stacktraces show the `.bt` source file.
    pub(super) fn file_attr(&self) -> Document<'static> {
        match &self.source_path {
            Some(path) => {
                docvec![", 'file' = [{", string_lit(path), ", 1}]"]
            }
            None => Document::Nil,
        }
    }
}

/// Value Object: A class's compile-time identity.
///
/// **DDD Context:** Code Generation
///
/// Holds the user-facing class name (from the AST class definition).
/// This decouples class identity from the Erlang module name, which may
/// differ for stdlib classes (e.g., module `bt@stdlib@string` → class `String`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ClassIdentity {
    class_name: String,
    /// BT-403: Whether this class is sealed (no subclasses allowed).
    /// Enables codegen optimizations: direct dispatch for self-sends.
    is_sealed: bool,
    /// BT-403: Whether this class is abstract (cannot be instantiated).
    /// Enables codegen optimization: reduced `gen_server` scaffolding.
    is_abstract: bool,
}

impl ClassIdentity {
    /// Create from an AST class name.
    pub fn new(class_name: &str) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed: false,
            is_abstract: false,
        }
    }

    /// Create from an AST class definition with sealed/abstract flags.
    pub fn from_class_def(class_name: &str, is_sealed: bool, is_abstract: bool) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed,
            is_abstract,
        }
    }

    /// The user-facing class name (CamelCase).
    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    /// Whether the class is sealed (BT-403).
    pub fn is_sealed(&self) -> bool {
        self.is_sealed
    }
}

impl CoreErlangGenerator {
    /// Returns the expression as a `Document` for direct composition via `docvec!`.
    ///
    /// ADR 0018: Simple forwarding to `generate_expression`.
    ///
    /// ADR 0118 (BT-3415, phase 1a): in a state-threading context the
    /// expression-level entry point is [`Self::threaded_expression`], which
    /// returns the value together with the prelude of `ThreadedStmt`s
    /// (real `Bind`s) its state-effecting sub-expressions need; the Actor
    /// method body (`lower_body_exprs_with_reply`) splices that prelude. This
    /// function stays a plain forwarder for now rather than becoming
    /// `threaded_expression(expr)?.close(..)` (§Decision 5): every consumer
    /// not yet migrated (branch arms, loop bodies, cascades, `match:`,
    /// interpolation — ADR 0118 phases 1b–4) still reaches nested self-sends
    /// through here, and each of those positions is a row of
    /// `stdlib/test/actor_self_send_position_matrix_test.bt` that `BUnit`
    /// compiles under the dev profile — a `close()` that reported
    /// `StateEffectEscapesExpression` through `report_threaded_ir_verify_errors`
    /// would `debug_assert!`-abort that whole fixture. The un-migrated
    /// positions therefore keep the byte-identical discarding fallback
    /// (`generate_discarding_self_dispatch`) until phase 2b removes the last
    /// `Document`-only consumer, at which point this becomes the `close()`
    /// site and the drop a verifier finding.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`](super::CodeGenError) if generating `expr` fails.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` compiles
    // individual expressions this way while assembling a REPL module.
    pub fn expression_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<beamtalk_cerl_doc::Document<'static>> {
        self.generate_expression(expr)
    }

    /// ADR 0118 §Decisions 1–3 (BT-3415): compiles `expr` in a
    /// state-threading context, returning its value together with the
    /// prelude of `ThreadedStmt`s that must run first — the expression-level
    /// counterpart of [`Self::expression_doc`]. The caller either splices
    /// the prelude into its own frame's IR and then uses the value
    /// (§Decision 4), or closes it ([`ThreadedValue::close`], §Decision 5).
    ///
    /// Phase 1a scope — what produces a non-empty prelude:
    /// - **An Actor self-send** (`self bump`): the producer
    ///   [`Self::generate_self_dispatch`], after its own arguments have been
    ///   sequenced by the rule below.
    /// - **A message send** (unary, binary or keyword; not a conditional /
    ///   `and:`/`or:` selector, which stay opaque here and are threaded by
    ///   their own intrinsics) whose receiver or non-block arguments need
    ///   a prelude: the **sequencing rule** (§Decision 3). Children are
    ///   compiled in evaluation order; if child *k* has a non-empty
    ///   prelude, every earlier child whose value is not a bare variable
    ///   or literal is bound to a fresh `_TmpN` temp ahead of child *k*'s
    ///   prelude, so `(items at: idx) + (self bump)` becomes `let _Tmp0 =
    ///   <at:> in <bump dispatch + Bind> in _Tmp0 + element(1, _SD)` —
    ///   `at:` still raises first, and `bump`'s state is threaded. The
    ///   parent itself is then compiled through its ordinary AST-directed
    ///   path, which substitutes each child's sequenced value via
    ///   `precompiled_subexprs`. This is `hoist_subexpr_splits`'s "decide
    ///   once, hoist all or none" rule (BT-3406) made universal; both use
    ///   [`Self::bind_subexpr_to_temp`].
    /// - **Any other parent** (literal elements, interpolation segments,
    ///   a `^` value): the planner (`hoist_nested_self_sends`) as before —
    ///   its order-safe hoists land in the prelude as real `Bind`s, its
    ///   order-unsafe ones keep the BT-3399 warning. Phase 1b replaces
    ///   this fallback with the sequencing rule for those parent kinds.
    ///
    /// Everything else is pure: one `generate_expression` call, empty
    /// prelude. The decision is taken by a pure AST probe
    /// ([`Self::subexpr_needs_prelude`]) before anything is compiled, so
    /// no sub-expression is ever compiled twice.
    ///
    /// Frame: phase 1a/1b's only consumer was the flat Actor method body
    /// (`lower_body_exprs_with_reply`), always [`FrameId::ROOT`]. ADR 0118
    /// phase 2a (BT-3417) widened this to any real frame — a conditional
    /// branch arm, an `on:do:`/`ensure:` body, a Tier 2 stateful-block body
    /// — via the `frame` parameter; phase 2b (BT-3418) widens it once more
    /// to a real loop body's own frame, the last remaining consumer, so
    /// EVERY `Bind` this call (and everything it recurses into — sequenced
    /// children, cascade messages, interpolation segments) produces lands
    /// in the CALLER's own frame instead of always claiming `ROOT`. Pass
    /// [`CoreErlangGenerator::current_frame`] unless the call site already
    /// has its own branch frame in hand (a conditional/exception arm, which
    /// minted it via `current_branch_frame()` on entry).
    pub(super) fn threaded_expression(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        let inner = expr.unwrap_parens();
        let span = inner.span();

        // ADR 0118 phase 4 (BT-3420): an inline-threaded control-flow
        // construct — `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`, `and:`/`or:`,
        // the nil-conditional family, or `match:` — that needs mutation
        // threading is a producer in its own right: its `_with_mutations`/
        // `generate_match` builder's internal `{Value, NewState}` case
        // becomes a real prelude via `control_flow_tuple_to_threaded_value`
        // instead of an opaque tuple `Document` a caller might embed
        // unwrapped. This closes the row that used to leak that tuple into
        // an argument position (`self record: (flag ifTrue: [self bump]
        // ifFalse: [0])`).
        if let Some(tv) = self.inline_control_flow_producer(inner)? {
            return Ok(tv);
        }

        // ADR 0118 phase 5a (BT-3421): a same-class class-method self-send
        // or a direct class-var assignment at the top level of `expr` —
        // the class-method-context counterpart of the Actor `is_prelude_producer`
        // check below. Gated to `frame == FrameId::ROOT`: both underlying
        // producers (`generate_class_method_self_send`/
        // `generate_class_var_field_assignment`) still derive their `Bind`'s
        // frame internally (`self.in_loop_body` / a hardcoded `FrameId::ROOT`,
        // unchanged by this issue), so this only engages where that
        // derivation is already known-correct — a class method's own flat
        // top-level body (`lower_class_method_body`). A class-var self-send
        // reached from a nested branch frame keeps falling through to the
        // generic paths below, unchanged from before this issue.
        if self.in_class_method() {
            if let Some(tv) = self.class_method_prelude_producer(inner, frame)? {
                return Ok(tv);
            }
        }

        if self.is_prelude_producer(inner) {
            let Expression::MessageSend {
                selector,
                arguments,
                ..
            } = inner
            else {
                return Err(CodeGenError::Internal(
                    "is_prelude_producer accepted a non-MessageSend expression".to_string(),
                ));
            };
            let children: Vec<&Expression> = arguments.iter().collect();
            let (mut prelude, scope) = self.sequence_children(&children, frame)?;
            let dispatch = self.generate_self_dispatch(selector, arguments, frame, span)?;
            self.finish_precompiled_scope(scope)?;
            prelude.extend(dispatch.prelude);
            return Ok(ThreadedValue {
                prelude,
                value: dispatch.value,
            });
        }

        if let Some(children) = Self::sequenced_send_children(inner) {
            let (mut prelude, scope) = self.sequence_children(&children, frame)?;
            let value = self.generate_expression_as_value(expr, span, &mut prelude)?;
            self.finish_precompiled_scope(scope)?;
            return Ok(ThreadedValue { prelude, value });
        }

        // ADR 0118 phase 1b: literal-container parents (`ListLiteral`
        // elements + tail, `ArrayLiteral` elements, `MapLiteral` keys/
        // values) — no effect happens between compiling one child and the
        // next (unlike `Cascade`/`StringInterpolation` below), so the
        // ordinary sequencing rule applies unchanged: sequence the
        // children, then let the parent's own (unaffected) codegen
        // substitute them via `precompiled_subexprs`.
        if let Some(children) = Self::literal_container_children(inner) {
            let (mut prelude, scope) = self.sequence_children(&children, frame)?;
            let value = self.generate_expression_as_value(expr, span, &mut prelude)?;
            self.finish_precompiled_scope(scope)?;
            return Ok(ThreadedValue { prelude, value });
        }

        // ADR 0118 phase 1b: `^ value`, `target := value`, `{a, b} :=
        // value`, and a `match:`'s scrutinee (arms are phase 4) — the
        // sequencing rule applied to the sole child whose evaluation must
        // finish before the parent's own effect (the NLR throw, the
        // assignment's mutation, the `case`) runs.
        if let Some(child) = Self::single_sequenced_child(inner) {
            let (mut prelude, scope) =
                self.sequence_children(std::slice::from_ref(&child), frame)?;
            let value = self.generate_expression_as_value(expr, span, &mut prelude)?;
            self.finish_precompiled_scope(scope)?;
            return Ok(ThreadedValue { prelude, value });
        }

        // ADR 0118 phase 1b: each interpolation segment performs its own
        // `displayString` dispatch immediately after its value — a real
        // effect a later segment's prelude must not run ahead of — so
        // this gets its own sequencing, not the generic children pattern
        // above.
        if let Expression::StringInterpolation { segments, .. } = inner {
            return self.threaded_string_interpolation(segments, span, frame);
        }

        // ADR 0118 phase 1b: a cascade on `self` in Actor instance
        // context threads every message's own dispatch, in order — see
        // `threaded_cascade_on_self`. Anything else (a remote receiver, a
        // class-method cascade, a message an intrinsic intercepts before
        // self-dispatch) falls through to the unmigrated `generate_cascade`
        // path below, unchanged from before this ADR.
        if let Expression::Cascade {
            receiver, messages, ..
        } = inner
        {
            if let Some(tv) = self.threaded_cascade_on_self(receiver, messages, frame)? {
                return Ok(tv);
            }
        }

        // Anything else — a `Block` (a closure; its contents run later, or
        // never) or a non-`self` / class-method `Cascade`: pure, empty
        // prelude. ADR 0118 phase 2b (BT-3418) review finding on #3718
        // (`sequenced_send_children` treating an `is_cast` send as opaque)
        // is fixed at the source instead of patched here: cast sends are
        // now a covered send like any other (see `sequenced_send_children`'s
        // own doc comment), so a self-send nested in a cast's receiver
        // (`(self next) process!`) is threaded by the covered-send branch
        // above, not this fallback. A non-`self` `Cascade` was never walked
        // by the old planner either (its walk only matched an explicit list
        // of parent shapes and treated everything else — Cascade included —
        // as a no-op leaf), so this fallback is a genuine no-op for every
        // shape that still reaches it: byte-identical to the pre-ADR-0118
        // planner's behaviour for those shapes, just without running it.
        //
        // ADR 0118 phase 5b (BT-3422): see `generate_expression_as_value`'s
        // doc comment for the one exception (a mutation-threaded
        // `do:`/dict-`do:` nested in a direct-params loop) this plain
        // `Doc(doc)` wrapping does not cover on its own.
        let mut prelude = Vec::new();
        let value = self.generate_expression_as_value(expr, span, &mut prelude)?;
        Ok(ThreadedValue { prelude, value })
    }

    /// ADR 0118 phase 5b (BT-3422): compiles `expr` via plain
    /// `generate_expression` and wraps the result as a [`ValueRef`] —
    /// `ValueRef::Doc` in the common case, but `ValueRef::Literal("'nil'")`
    /// with the returned `Document` pushed onto `prelude` instead when
    /// `direct_params_do_open_chain` comes back set: a mutation-threaded
    /// `do:`/dict-`do:` nested in a direct-params loop
    /// (`in_direct_params_loop`) leaves its own returned `Document` as an
    /// open, dangling let-chain and sets that flag rather than returning a
    /// closed value (BT-3053: several accumulator vars may have been
    /// rebound, not one meaningful result, so `do:`'s own `nil`
    /// return-value contract is substituted instead of a name). Every
    /// `generate_expression` call [`Self::threaded_expression`] itself
    /// makes (as opposed to ones its producers make internally, already
    /// self-contained) funnels through this one function so the check
    /// never has to be repeated at each call site.
    fn generate_expression_as_value(
        &mut self,
        expr: &Expression,
        span: beamtalk_core::source_analysis::Span,
        prelude: &mut Vec<ThreadedStmt>,
    ) -> Result<ValueRef> {
        self.direct_params_do_open_chain = false;
        let doc = self.generate_expression(expr)?;
        if self.direct_params_do_open_chain {
            self.direct_params_do_open_chain = false;
            prelude.push(ThreadedStmt::Statement(doc, span));
            return Ok(ValueRef::Literal("'nil'"));
        }
        Ok(ValueRef::Doc(doc))
    }

    /// The children of a literal-container parent the sequencing rule
    /// applies to, in evaluation order: `ListLiteral` elements then an
    /// optional tail, `ArrayLiteral` elements, `MapLiteral` keys and
    /// values (key before value, pair by pair). `None` for anything else.
    /// Safe to treat like [`Self::sequenced_send_children`]'s children
    /// (no interleaved effect between compiling one and the next): each
    /// of these parents' own codegen
    /// ([`Self::generate_list_literal`]/[`Self::generate_array_literal`]/
    /// [`Self::generate_map_literal`]) compiles every element through
    /// `capture_subexpr_sequence` → `expression_doc`, which is exactly
    /// the route [`Self::register_precompiled_subexpr`]'s substitution
    /// reaches.
    fn literal_container_children(expr: &Expression) -> Option<Vec<&Expression>> {
        match expr {
            Expression::ListLiteral { elements, tail, .. } => {
                let mut children: Vec<&Expression> = elements.iter().collect();
                if let Some(tail) = tail {
                    children.push(tail.as_ref());
                }
                Some(children)
            }
            Expression::ArrayLiteral { elements, .. } => Some(elements.iter().collect()),
            Expression::MapLiteral { pairs, .. } => {
                let mut children = Vec::with_capacity(pairs.len() * 2);
                for pair in pairs {
                    children.push(&pair.key);
                    children.push(&pair.value);
                }
                Some(children)
            }
            _ => None,
        }
    }

    /// The single child the sequencing rule applies to for a "value-
    /// carrying" parent whose own effect (an NLR throw, an assignment's
    /// mutation, a `match:`'s `case`) runs only after that child is fully
    /// evaluated: `^ value`, `target := value`, `{a, b} := value`, and a
    /// `match:`'s scrutinee (its arms are phase 4 — untouched here).
    /// `None` for every other kind.
    fn single_sequenced_child(expr: &Expression) -> Option<&Expression> {
        match expr {
            Expression::Return { value, .. }
            | Expression::Assignment { value, .. }
            | Expression::DestructureAssignment { value, .. }
            | Expression::Match { value, .. } => Some(value.as_ref()),
            _ => None,
        }
    }

    /// For a cascade whose underlying receiver is a bare `self` in Actor
    /// instance context, the list of `(selector, arguments, span)` for
    /// every message — the first message folded out of `receiver` per
    /// the parser's own cascade shape (`generate_cascade`'s doc comment),
    /// then `messages` in order — when EVERY message would dispatch
    /// through [`Self::generate_self_dispatch`] were it a standalone
    /// self-send. `None` when the cascade doesn't qualify: a non-`self`
    /// receiver (an ordinary remote/class-method send — unaffected, same
    /// as before this ADR), or any message whose selector is intercepted
    /// before `try_handle_self_dispatch` (binary — already rejected by
    /// `generate_cascade` itself for cascades — or a well-known
    /// intrinsic).
    fn cascade_self_dispatch_messages<'e>(
        &self,
        receiver: &'e Expression,
        messages: &'e [CascadeMessage],
    ) -> Option<Vec<CascadeSelfMessage<'e>>> {
        if !self.in_actor_instance_context() {
            return None;
        }
        let (underlying_receiver, first): (&Expression, Option<CascadeSelfMessage<'e>>) =
            match receiver {
                Expression::MessageSend {
                    receiver: inner,
                    selector,
                    arguments,
                    span,
                    is_cast: false,
                    ..
                } => (
                    inner.as_ref(),
                    Some((selector, arguments.as_slice(), *span)),
                ),
                _ => (receiver, None),
            };
        if !matches!(underlying_receiver, Expression::Identifier(id) if id.name == "self") {
            return None;
        }
        let mut all: Vec<CascadeSelfMessage<'e>> = Vec::with_capacity(messages.len() + 1);
        all.extend(first);
        for msg in messages {
            all.push((&msg.selector, msg.arguments.as_slice(), msg.span));
        }
        if all.is_empty()
            || !all
                .iter()
                .all(|(selector, ..)| Self::selector_dispatches_via_self(selector))
        {
            return None;
        }
        Some(all)
    }

    /// ADR 0118 phase 1b: a cascade on `self` in Actor instance context —
    /// `self record: 1; record: 2` — threads through
    /// [`Self::generate_self_dispatch`] one message at a time, in order,
    /// exactly like an ordinary self-send statement: each message's own
    /// (non-block) arguments are sequenced first, so a self-send nested
    /// in an EARLIER message's argument list (`self record: (self
    /// bumpCount); record: 1`) dispatches before the next message runs.
    /// Each message's dispatch reads `current_state_var()` as it starts
    /// building its call — the same generator-global counter the
    /// previous message's `Bind` just advanced — so `State` threads
    /// across messages the same way it threads across ordinary
    /// statements. `Ok(None)` when the cascade doesn't qualify (see
    /// [`Self::cascade_self_dispatch_messages`]); the caller falls
    /// through to the unmigrated `generate_cascade` path, unchanged from
    /// before this ADR.
    fn threaded_cascade_on_self(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let Some(all_messages) = self.cascade_self_dispatch_messages(receiver, messages) else {
            return Ok(None);
        };
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        // Overwritten by the loop below on every iteration: `all_messages`
        // is non-empty (`cascade_self_dispatch_messages` rejects the
        // empty case), so the cascade's own value is always the LAST
        // message's — matching `generate_cascade`'s "the cascade returns
        // the result of the final message" contract.
        let mut value = ValueRef::Literal("'nil'");
        for (selector, arguments, msg_span) in all_messages {
            let children: Vec<&Expression> = arguments.iter().collect();
            let (mut msg_prelude, scope) = self.sequence_children(&children, frame)?;
            let dispatch = self.generate_self_dispatch(selector, arguments, frame, msg_span)?;
            self.finish_precompiled_scope(scope)?;
            msg_prelude.extend(dispatch.prelude);
            prelude.extend(msg_prelude);
            value = dispatch.value;
        }
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// `true` inside an Actor *instance* method — the only context in
    /// which a self-send threads `State` (a class method threads
    /// `ClassVars`, never `State`; same exclusion as the planner's).
    pub(super) fn in_actor_instance_context(&self) -> bool {
        self.context == CodeGenContext::Actor && !self.in_class_method()
    }

    /// `true` if `expr` (already paren-unwrapped) is a dispatching Actor
    /// self-send [`Self::threaded_expression`] must compile through the
    /// producer: in an Actor instance context.
    fn is_prelude_producer(&self, expr: &Expression) -> bool {
        self.in_actor_instance_context() && self.is_dispatching_actor_self_send(expr)
    }

    /// ADR 0118 phase 5a/5b (BT-3421/BT-3422): compiles `expr` (already
    /// paren-unwrapped) through the class-method-context producers when it
    /// is, at its own top level, a direct class-var assignment
    /// (`self.classVar := value`) or a same-class class-method self-send
    /// (`self someSelector`) — `None` for anything else. Called from
    /// [`Self::threaded_expression`] (falls through to its generic paths on
    /// `None`), gated to `self.in_class_method()` alone — `frame` is
    /// whatever real frame the caller is threading into (ROOT at a class
    /// method's own flat top level, or a branch/loop frame nested inside
    /// one), passed straight through to the producer.
    ///
    /// A same-class self-send is checked with [`Self::is_class_method_self_send`]
    /// — selectors in `class_method_selectors()` only — DELIBERATELY
    /// narrower than [`Self::try_handle_class_method_self_send`]'s own
    /// condition (any `self` receiver): `generate_message_send`'s real
    /// dispatch order runs ProtoObject/Object/Block/Dict/List/Boolean/
    /// spawn-await/Erlang-interop/Logger/class-reference checks BEFORE
    /// `try_handle_class_method_self_send` ever runs, so `self class` or
    /// `self isNil` must still reach THOSE handlers, never
    /// `generate_class_method_self_send` directly — this function has no
    /// way to replicate that whole priority chain, but a selector the class
    /// itself declares as a class method can never collide with one of
    /// those reserved well-known names (the same assumption
    /// `generate_class_method_last_expr_with_class_vars`/
    /// `generate_class_method_non_last_expr` already made for this exact
    /// predicate, pre-dating this issue).
    fn class_method_prelude_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if self.is_class_var_assignment(expr) {
            let Expression::Assignment { target, value, .. } = expr else {
                unreachable!("is_class_var_assignment guarantees an Assignment");
            };
            let Expression::FieldAccess { field, .. } = target.as_ref() else {
                unreachable!("is_class_var_assignment guarantees a FieldAccess target");
            };
            let field_name = field.name.to_string();
            return Ok(Some(self.generate_class_var_field_assignment(
                &field_name,
                value,
                frame,
            )?));
        }
        if self.is_class_method_self_send(expr) {
            let Expression::MessageSend {
                selector,
                arguments,
                ..
            } = expr
            else {
                unreachable!("is_class_method_self_send guarantees a MessageSend");
            };
            return Ok(Some(
                self.generate_class_method_self_send(selector, arguments)?,
            ));
        }
        Ok(None)
    }

    /// The children the sequencing rule applies to when `expr` (already
    /// paren-unwrapped) is a message send it covers: the receiver, then
    /// every non-block argument, in evaluation order. `None` for anything
    /// that is not a message send, and for a conditional / `and:` / `or:`
    /// send — those are opaque to the rule (their own intrinsics thread
    /// their receiver and arms; ADR 0118 phase 4 makes them producers). A
    /// block argument is a closure: whatever it contains runs later, or
    /// never, so it is neither sequenced nor temp-bound.
    ///
    /// ADR 0118 phase 2b (BT-3418): a cast send (`X!`) is covered here too
    /// — it is compiled through `generate_cast_send`/`generate_expression`
    /// like any other send, consulting `precompiled_subexprs` for its
    /// receiver and arguments the same way (`capture_argument_list_doc`/
    /// `capture_subexpr_sequence`), so nothing about it needs opaque
    /// treatment. Before this, a self-send nested in a cast's receiver
    /// (`(self next) process!`) reached `threaded_expression`'s planner
    /// fallback instead — the planner is gone, so this is now the ONLY
    /// path that threads it (`bt3416_self_send_nested_in_a_cast_sends_
    /// receiver_still_threads`, `tests/gen_server.rs`).
    fn sequenced_send_children(expr: &Expression) -> Option<Vec<&Expression>> {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };
        if beamtalk_core::state_threading_selectors::is_conditional_selector(
            selector.name().as_str(),
        ) {
            return None;
        }
        let mut children = Vec::with_capacity(arguments.len() + 1);
        // An FFI receiver (`Erlang lists reverse: x`) is consumed
        // STRUCTURALLY by `try_handle_erlang_interop` — the same
        // `erlang_module_of_receiver` predicate turns it into a module-name
        // atom and never compiles it — so it is never sequenced or
        // temp-bound: a registered value it would never consult is an
        // internal error in `finish_precompiled_scope`. Its arguments are
        // compiled normally and are sequenced like any other send's.
        if beamtalk_core::ffi_receiver::erlang_module_of_receiver(receiver).is_none() {
            children.push(receiver.as_ref());
        }
        children.extend(
            arguments
                .iter()
                .filter(|arg| !matches!(arg, Expression::Block(_))),
        );
        Some(children)
    }

    /// Pure AST probe: would [`Self::threaded_expression`] give `expr` a
    /// non-empty prelude? Mirrors that function's cases exactly — a
    /// producer, a covered send (cast sends included, ADR 0118 phase 2b)
    /// with a child that needs one, a literal container, a `^`/`:=`/
    /// `match:` scrutinee, a string interpolation, or a self-cascade — so
    /// the decision to compile children individually can never disagree
    /// with what compiling them would do. Anything else (a `FieldAccess`,
    /// which can never itself contain a nested self-send since its
    /// receiver is always literally `self`; a conditional/`and:`/`or:`
    /// send, opaque to this rule and threaded by its own intrinsics
    /// instead; a `Block`, already excluded above as trivial) needs no
    /// prelude — ADR 0118 phase 2b (BT-3418) removed the planner-based
    /// fallback this used to fall through to here: every one of those
    /// residual shapes was already a no-op under the old planner's own
    /// walk (a `Block`/anything unmatched hit its catch-all without
    /// recursing at all; a `FieldAccess` only ever produced a `Snapshot`,
    /// never a `Dispatch`; a conditional selector returned immediately
    /// without walking its receiver), so this is behaviourally identical,
    /// just without running the planner to reach the same answer.
    pub(super) fn subexpr_needs_prelude(&self, expr: &Expression) -> bool {
        let inner = expr.unwrap_parens();
        if Self::is_trivial_subexpr(inner) {
            return false;
        }
        // ADR 0118 phase 5b (BT-3422): a child an ENCLOSING `sequence_children`
        // call already registered in `precompiled_subexprs` has already been
        // dispatched and its prelude already spliced into that outer call's
        // own prelude — "compiling" it now means only reading the
        // substitution back (`take_precompiled_subexpr`), which contributes
        // no further prelude of its own. Without this check, a second,
        // independent sequencing pass over the SAME children (e.g.
        // `generate_binary_op` reached through `threaded_expression`'s own
        // `sequenced_send_children` branch, whose `generate_expression`
        // recompile of the parent reaches `thread_subexprs` for its
        // operands) would re-dispatch an Actor self-send a second time and
        // leave the outer registration's `finish_precompiled_scope` check
        // failing with "never substituted" (confirmed by a
        // `just test` failure on exactly this shape during this issue).
        if self.precompiled_subexprs_contains(inner) {
            return false;
        }
        // ADR 0118 phase 4 (BT-3420): mirrors `threaded_expression`'s new
        // inline-control-flow producer branch — see its doc comment.
        if self.inline_control_flow_needs_threading(inner) {
            return true;
        }
        // ADR 0118 phase 5b (BT-3422): mirrors `threaded_expression`'s
        // `class_method_prelude_producer` branch — a same-class self-send
        // or a direct class-var assignment, in class-method context, needs
        // a prelude regardless of nesting depth (a message argument, a
        // binary operand, a cascade message, ...). A pure predicate check
        // (not the mutating producer call itself) so this stays a probe.
        if self.in_class_method()
            && (self.is_class_var_assignment(inner) || self.is_class_method_self_send(inner))
        {
            return true;
        }
        if self.is_prelude_producer(inner) {
            return true;
        }
        if let Some(children) = Self::sequenced_send_children(inner) {
            return children
                .iter()
                .any(|child| self.subexpr_needs_prelude(child));
        }
        // ADR 0118 phase 1b: mirrors `threaded_expression`'s own cases
        // exactly, for the same reason phase 1a's version of this
        // function mirrored ITS cases — the probe must never disagree
        // with what compiling would do.
        if let Some(children) = Self::literal_container_children(inner) {
            return children
                .iter()
                .any(|child| self.subexpr_needs_prelude(child));
        }
        if let Some(value) = Self::single_sequenced_child(inner) {
            return self.subexpr_needs_prelude(value);
        }
        if let Expression::StringInterpolation { segments, .. } = inner {
            return segments.iter().any(|seg| match seg {
                StringSegment::Interpolation(e) => self.subexpr_needs_prelude(e),
                StringSegment::Literal(_) => false,
            });
        }
        if let Expression::Cascade {
            receiver, messages, ..
        } = inner
        {
            return self
                .cascade_self_dispatch_messages(receiver, messages)
                .is_some();
        }
        false
    }

    /// BT-3396/BT-3414 (ADR 0118 phase 0): `true` if compiling `expr`
    /// through [`Self::threaded_expression`] would give it a non-empty
    /// prelude — the decision predicate for "does this conditional need
    /// inlining because of its receiver?" (`conditional_needs_mutation_threading`).
    ///
    /// Formerly `contains_hoistable_self_send`
    /// (`control_flow/conditionals.rs`), backed by the hoist planner's own
    /// walk so a decision and its emission could never disagree. ADR 0118
    /// phase 2b (BT-3418) deleted that walk; this predicate shares
    /// [`Self::subexpr_needs_prelude`] with the sequencing rule itself
    /// instead — the same "would compiling this actually produce a
    /// prelude?" question the rule already answers for every one of its
    /// own children — so it still cannot disagree with what compiling
    /// `expr` does, without sharing code with an emitting walk. Lives here
    /// (not `control_flow/conditionals.rs`) because it is now a thin
    /// wrapper over `subexpr_needs_prelude`.
    ///
    /// ADR 0118 phase 4 (BT-3420) deleted this function's own `and:`/`or:`
    /// carve-out: `subexpr_needs_prelude` now recognizes a nested
    /// mutation-threaded `and:`/`or:`/nil-conditional/`match:` receiver
    /// (`((self recordOnce: which) and: [x]) ifTrue:ifFalse:`) directly, via
    /// [`Self::inline_control_flow_needs_threading`], so the signal this
    /// carve-out used to restore here is never lost in the first place.
    /// Phase 6 unifies the remaining class-var/actor-state receiver
    /// positions this predicate still special-cases below (there are none
    /// left as of this phase — kept as a thin wrapper for its own call
    /// sites' sake).
    pub(in crate::core_erlang) fn conditional_receiver_needs_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        self.subexpr_needs_prelude(expr)
    }

    /// The `State` variable a consumer must continue from once it has
    /// spliced `prelude` — the target of the prelude's last top-level
    /// `State` `Bind`, or `version_before` (the counter as read BEFORE
    /// `threaded_expression` ran) when the prelude has none. Read this
    /// instead of `current_state_var()` after a `threaded_expression` call:
    /// the value's own compile may have minted versions INSIDE its closed
    /// document (a conditional receiver's `generate_self_dispatch_open`
    /// chain, say), and those are not in scope where the consumer's reply
    /// or next statement runs.
    pub(super) fn state_var_after_prelude(
        &self,
        prelude: &[ThreadedStmt],
        version_before: usize,
    ) -> String {
        let version = prelude
            .iter()
            .rev()
            .find_map(|stmt| match stmt {
                ThreadedStmt::Bind { target, .. }
                    if target.prefix == VersionPrefix::State && target.frame == FrameId::ROOT =>
                {
                    Some(target.version)
                }
                _ => None,
            })
            .unwrap_or(version_before);
        super::render_state_prefix(self.in_hybrid_loop, self.in_loop_body, version)
    }

    /// A child the sequencing rule never compiles ahead of its parent: a
    /// literal, an identifier, a class reference, `super`, or a block —
    /// each compiles to a value that cannot raise or observe threaded
    /// state, so the parent may compile it in place after every prelude.
    fn is_trivial_subexpr(expr: &Expression) -> bool {
        matches!(
            expr,
            Expression::Literal(..)
                | Expression::Identifier(_)
                | Expression::ClassReference { .. }
                | Expression::Super(_)
                | Expression::Block(_)
        )
    }

    /// The sequencing rule itself (ADR 0118 §Decision 3) over `children`,
    /// in evaluation order. Let *k* be the last child that needs a prelude
    /// (per [`Self::subexpr_needs_prelude`]); if there is none, nothing is
    /// compiled here and the parent compiles every child in place, exactly
    /// as before. Otherwise every non-trivial child up to and including
    /// *k* is compiled exactly once via [`Self::threaded_expression`], in
    /// order, and the parent's prelude is built from them: a child before
    /// *k* whose value is not a bare variable or literal is bound to a
    /// `_TmpN` temp ([`Self::bind_subexpr_to_temp`]) right after its own
    /// prelude; child *k*'s value is used in place. Children after *k*
    /// evaluate after every prelude anyway, so the parent compiles them in
    /// place too (keeping their temps' mint order unchanged). Each
    /// pre-compiled child is registered in `precompiled_subexprs` so the
    /// parent's compile substitutes it; the caller hands the returned
    /// scope back to [`Self::finish_precompiled_scope`] afterwards.
    pub(super) fn sequence_children(
        &mut self,
        children: &[&Expression],
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, PrecompiledScope)> {
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let mut scope = PrecompiledScope::new();
        let Some(k) = children
            .iter()
            .rposition(|child| self.subexpr_needs_prelude(child))
        else {
            return Ok((prelude, scope));
        };

        let mut compiled: Vec<(&Expression, bool, ThreadedValue)> = Vec::with_capacity(k + 1);
        for child in &children[..=k] {
            if Self::is_trivial_subexpr(child.unwrap_parens()) {
                continue;
            }
            let is_producer = self.is_prelude_producer(child.unwrap_parens());
            let tv = self.threaded_expression(child, frame)?;
            compiled.push((child, is_producer, tv));
        }
        let last = compiled.len().saturating_sub(1);

        for (i, (child, is_producer, tv)) in compiled.into_iter().enumerate() {
            let span = child.unwrap_parens().span();
            let must_bind = i < last && !tv.value_is_trivial();
            prelude.extend(tv.prelude);
            let value_doc = self.threaded_value_doc(&tv.value);
            if must_bind {
                let (binding, var) = self.bind_subexpr_to_temp("Tmp", value_doc);
                prelude.push(ThreadedStmt::Statement(binding, span));
                self.register_precompiled_subexpr(
                    &mut scope,
                    child,
                    beamtalk_cerl_doc::leaf::var(var),
                    false,
                )?;
            } else {
                self.register_precompiled_subexpr(&mut scope, child, value_doc, is_producer)?;
            }
        }
        Ok((prelude, scope))
    }

    /// Renders a [`ThreadedValue`]'s value for use by a consumer that has
    /// spliced (or is about to splice) its prelude — through the same
    /// `render_value` every `Bind`/`Return` node renders its values with.
    pub(super) fn threaded_value_doc(&mut self, value: &ValueRef) -> Document<'static> {
        let ctx = RenderCtx::new(self);
        render_value(value, &ctx)
    }

    /// The prelude counterpart of [`Self::threaded_value_doc`]: renders a
    /// [`ThreadedValue`]'s `prelude` — a `Vec<ThreadedStmt>` from a
    /// consumer that has not (yet, or ever) spliced it into a real
    /// `ThreadedIr` frame — through the same `render` every spliced
    /// prelude goes through, so a rendered-standalone and a spliced
    /// prelude never differ in bytes. `Document::Nil`-equivalent (empty)
    /// when `prelude` is empty.
    pub(super) fn threaded_prelude_doc(&mut self, prelude: &[ThreadedStmt]) -> Document<'static> {
        let mut ctx = RenderCtx::new(self);
        render(prelude, &mut ctx)
    }

    /// ADR 0118 phase 1a (BT-3415): for a consumer whose existing lowering
    /// compiles `expr` itself through a specialised path
    /// (`generate_field_assignment_open`, `emit_actor_threaded_last_stmts`,
    /// …) rather than via one `expression_doc` call it could replace with
    /// [`Self::threaded_expression`]: if `expr` needs a prelude, compiles
    /// it once through `threaded_expression`, splices the prelude into
    /// `stmts`, and registers the value so that path's own compile of
    /// `expr` substitutes it; otherwise does nothing. The returned scope
    /// goes back to [`Self::finish_precompiled_scope`] once that compile
    /// is done. ADR 0118 phase 2b (BT-3418): the drop-in replacement for
    /// every remaining `hoist_nested_self_sends(expr, HoistSink::Threaded {
    /// .. })` call, now that the sequencing rule (not the order-safety
    /// drop the deleted planner used) is the only mechanism.
    pub(super) fn thread_ahead(
        &mut self,
        expr: &Expression,
        stmts: &mut Vec<ThreadedStmt>,
        frame: FrameId,
    ) -> Result<PrecompiledScope> {
        let mut scope = PrecompiledScope::new();
        if !self.subexpr_needs_prelude(expr) {
            return Ok(scope);
        }
        let is_producer = self.is_prelude_producer(expr.unwrap_parens());
        let tv = self.threaded_expression(expr, frame)?;
        stmts.extend(tv.prelude);
        let doc = self.threaded_value_doc(&tv.value);
        self.register_precompiled_subexpr(&mut scope, expr, doc, is_producer)?;
        Ok(scope)
    }

    /// ADR 0118 phase 5b (BT-3422): the self-contained-`Document` replacement for the
    /// deleted `closed_expression_doc`. Renders `expr`'s prelude and value
    /// back-to-back through the same [`render`]/[`render_value`] every
    /// spliced prelude goes through, so the bytes match a spliced prelude
    /// exactly (a `ClassVars` `Bind` in the prelude stays lexically visible
    /// to whatever Core Erlang the caller concatenates after this
    /// `Document` — the pre-ADR-0118 open-let-chain's own contract,
    /// preserved). Use where the caller has no `Vec<ThreadedStmt>`/
    /// `Vec<Document>` of its own to splice into (a single expression
    /// embedded directly as another `Document`'s sub-tree).
    pub(super) fn threaded_expression_doc(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<beamtalk_cerl_doc::Document<'static>> {
        let tv = self.threaded_expression(expr, frame)?;
        Ok(self.close_threaded_value_doc(tv))
    }

    /// ADR 0118 phase 5b (BT-3422): the same self-contained rendering as
    /// [`Self::threaded_expression_doc`], for a caller that already holds a
    /// [`ThreadedValue`] (a producer's own return value) rather than an
    /// `Expression` to compile — used at every ambient
    /// (non-`threaded_expression`) re-entry point a class-var producer has
    /// (`try_handle_class_method_self_send`, `try_handle_class_reference`,
    /// `generate_field_assignment`'s class-var branch): these are reached
    /// through ordinary `generate_expression`, which returns a bare
    /// `Document` with no prelude side-channel, so the prelude is always
    /// closed here rather than left open for a caller to propagate.
    // `tv` is taken by value deliberately, matching `ThreadedValue::close`'s
    // own consuming signature — the `#[must_use]` linear-discipline design
    // (see `ThreadedValue`'s doc comment) wants "closed" to mean consumed,
    // not merely read.
    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn close_threaded_value_doc(
        &mut self,
        tv: ThreadedValue,
    ) -> beamtalk_cerl_doc::Document<'static> {
        if tv.prelude.is_empty() {
            return self.threaded_value_doc(&tv.value);
        }
        let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        docvec![prelude_doc, value_doc]
    }

    /// Generates an expression and returns whether it set `repl_loop_mutated`.
    ///
    /// BT-1448: Replaces the manual reset-before/read-after pattern on `repl_loop_mutated`.
    /// Mutation-threaded control flow (loops, conditionals, exception handlers) and inline
    /// value calls set `repl_loop_mutated` deep in the call stack. REPL codegen needs to
    /// know whether the expression returned a `{Result, State}` tuple that must be unpacked.
    /// This method encapsulates the side-channel into an explicit return value.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`](super::CodeGenError) if generating `expr` fails.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` calls this while
    // generating REPL-mode expression bodies.
    pub fn expression_doc_with_repl_mutation_tracking(
        &mut self,
        expr: &beamtalk_core::ast::Expression,
    ) -> Result<(beamtalk_cerl_doc::Document<'static>, bool)> {
        self.set_repl_loop_mutated(false);
        let doc = self.generate_expression(expr)?;
        let mutated = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        Ok((doc, mutated))
    }

    /// Generates a fresh variable name and binds it in the current scope.
    ///
    /// Use this for user-visible bindings (block parameters, assignments, etc.)
    /// where the name should be looked up later via `lookup_var`.
    pub(super) fn fresh_var(&mut self, base: &str) -> String {
        let var_name = self.var_context.fresh_var(base);
        // Bind it in the current scope
        self.var_context.bind(base, &var_name);
        var_name
    }

    /// Generates a fresh temporary variable name WITHOUT binding it in scope.
    ///
    /// Use this for internal codegen temporaries (loop variables, function bindings,
    /// etc.) that should never shadow or be confused with user identifiers.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` mints internal
    // temporaries (e.g. result variables) while assembling a REPL module.
    pub fn fresh_temp_var(&mut self, base: &str) -> String {
        self.var_context.fresh_var(base)
    }

    /// Converts a Beamtalk identifier to a valid Core Erlang variable name.
    ///
    /// Core Erlang variables must start with an uppercase letter or underscore.
    /// This function capitalizes the first letter of the identifier.
    pub(super) fn to_core_erlang_var(name: &str) -> String {
        super::variable_context::VariableContext::to_core_var(name)
    }

    /// BT-3161: builds an explicit wildcard fallback clause — `<Var> when
    /// 'true' -> call 'erlang':'error'({'case_clause', Var})` — for a `case`
    /// whose other clauses match specific literal/tagged-tuple shapes (e.g. a
    /// boolean `<'true'>`/`<'false'>` case, or a dispatch-result case
    /// matching only `{'reply',...}`/`{'error',...}` shapes) that are not
    /// *statically provable* exhaustive to the Core Erlang compiler, even
    /// when they are exhaustive over every shape the value can genuinely
    /// take at runtime.
    ///
    /// Without an explicit wildcard, the compiler synthesizes an implicit
    /// one for the same fallback — but when the `case` sits inside a
    /// `try`'s protected region, OTP's `beam_validator` rejects the
    /// resulting BEAM with `ambiguous_catch_try_state` ("Internal
    /// consistency check failed"), a genuine BEAM-compiler edge case
    /// (confirmed by bisecting the emitted `.core`: on an otherwise-fixed
    /// `case`-in-`try` repro, adding vs. omitting only this clause was the
    /// one difference between a passing and a failing `erlc` run — nesting
    /// depth, tail position, and clause bodies were not the trigger). Making
    /// the fallback explicit is behavior-preserving (a non-matching value
    /// already raises `case_clause` via the compiler's own implicit clause)
    /// and sidesteps the validator bug unconditionally, so callers don't
    /// need to know whether a given call site happens to be nested inside a
    /// `try`.
    pub(super) fn case_clause_fallback(
        &mut self,
        var_prefix: &str,
    ) -> beamtalk_cerl_doc::Document<'static> {
        let var = self.fresh_temp_var(var_prefix);
        docvec![
            " <",
            beamtalk_cerl_doc::leaf::var(var.clone()),
            "> when 'true' -> call 'erlang':'error'({'case_clause', ",
            beamtalk_cerl_doc::leaf::var(var),
            "})",
        ]
    }

    /// BT-3140: class-var writes can't thread through the generic
    /// `State`/`StateAcc` mechanism used by [`Self::generate_field_assignment_open`]
    /// and the conditional-branch `Bind`-chain codegen it mirrors — see
    /// [`super::CodeGenError::ClassVarAssignmentInThreadedBody`]'s doc comment
    /// for why. Rejects at compile time (mirroring BT-2792's
    /// `FieldAssignmentInUnsupportedBlock` for the analogous "can't thread
    /// this state" shape) instead of silently losing the mutation on both
    /// normal return and NLR escape.
    ///
    /// Shared by both call sites per CLAUDE.md's no-duplicate-implementations
    /// rule — `is_class_var_assignment`'s `receiver == "self"` gate is the
    /// authoritative rule for what counts as a class-var assignment, and this
    /// helper is the single place that turns a positive match into the
    /// rejection error, so the two call sites can't drift out of sync.
    ///
    /// BT-3168 (ADR 0111 Addendum 9): `generate_field_assignment_open` now
    /// calls this only as its fallback branch — a class-var write directly
    /// inside a Letrec loop body that threads `ClassVars` through the loop's
    /// own recursive tail call (`loop_threads_class_vars`) is threaded via a
    /// real `Bind` instead, before ever reaching this call. This helper's own
    /// behavior is unchanged; only its one call site inside
    /// `generate_field_assignment_open` became conditional.
    ///
    /// `expr` must be the `Expression::Assignment` whose `target` is the
    /// given `field`'s `FieldAccess` (the caller has already matched this
    /// shape before calling in).
    pub(super) fn reject_class_var_field_assignment(
        &self,
        expr: &Expression,
        field: &Identifier,
    ) -> Result<()> {
        if self.is_class_var_assignment(expr) {
            let location = self.span_to_line(expr.span()).map_or_else(
                || format!("offset {}", expr.span().start()),
                |line| format!("line {line}"),
            );
            return Err(CodeGenError::ClassVarAssignmentInThreadedBody {
                field: field.name.to_string(),
                location,
            });
        }
        Ok(())
    }

    /// Returns the class name for the currently compiled class.
    ///
    /// Prefers the AST-derived class identity when available (set during class
    /// compilation). Falls back to deriving from the module name for backward
    /// compatibility with compilation units that don't set class identity.
    ///
    /// # Examples
    ///
    /// - Module `"counter"` (no class identity) → `"Counter"`
    /// - Module `"bt@stdlib@string"` with class identity `"String"` → `"String"`
    pub(super) fn class_name(&self) -> String {
        if let Some(identity) = self.class_identity() {
            return identity.class_name().to_string();
        }
        // Fall back to deriving from module name (snake_case → CamelCase)
        self.module_name
            .split('_')
            .map(|s| {
                let mut chars = s.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                }
            })
            .collect()
    }

    /// Whether the current class is sealed (BT-403).
    pub(super) fn is_class_sealed(&self) -> bool {
        self.class_identity().is_some_and(ClassIdentity::is_sealed)
    }

    /// Build the `beamtalk_source` Core Erlang attribute fragment (BT-845/BT-860).
    ///
    /// Returns `, 'beamtalk_source' = ["<path>"]` when `source_path` is set
    /// and NOT in stdlib mode. Stdlib classes return nil from `sourceFile` at
    /// runtime, but still use `source_path` for line annotations and the
    /// `'file'` attribute (BEAM stacktrace debug info).
    pub(super) fn source_path_attr(&self) -> Document<'static> {
        if self.stdlib_mode() {
            return Document::Nil;
        }
        match &self.source_path {
            Some(path) => {
                docvec![", 'beamtalk_source' = [", string_lit(path), "]"]
            }
            None => Document::Nil,
        }
    }
}

/// Converts class name (`CamelCase`) to module name (`snake_case`).
///
/// Delegates to [`beamtalk_core::ast::to_module_name`] (Shared Kernel).
///
/// Visibility: `pub` to allow usage in IDE queries (hover, completion, etc.)
pub fn to_module_name(class_name: &str) -> String {
    beamtalk_core::ast::to_module_name(class_name)
}

/// Extracts the user package prefix from a workspace-qualified module name (BT-794).
///
/// Given `bt@{package}@{rest}`, returns `Some("bt@{package}@")`.
/// Returns `None` for stdlib modules (`bt@stdlib@...`), unprefixed names, or
/// names without a package segment.
///
/// # Limitations
///
/// This function intentionally returns only the top-level package segment
/// (`bt@{package}@`), discarding any subdirectory path components. For example,
/// `bt@sicp@scheme@eval` returns `bt@sicp@` rather than `bt@sicp@scheme@`.
///
/// Callers such as `compiled_module_name` use this prefix to construct module
/// names for referenced classes. This means cross-module references within a
/// package only produce correct names when the referenced class lives at the
/// package root (e.g. `bt@{package}@{class}`). Classes nested in subdirectories
/// (e.g. `bt@{package}@{subdir}@{class}`) cannot be resolved by class name alone
/// and are not currently supported for inter-class dispatch.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(user_package_prefix("bt@bank@account"), Some("bt@bank@".into()));
/// // Subdirectory segments are stripped — `scheme@` is not preserved:
/// assert_eq!(user_package_prefix("bt@sicp@scheme@eval"), Some("bt@sicp@".into()));
/// assert_eq!(user_package_prefix("bt@stdlib@integer"), None);
/// assert_eq!(user_package_prefix("counter"), None);
/// assert_eq!(user_package_prefix("bt@counter"), None);
/// ```
pub(super) fn user_package_prefix(module_name: &str) -> Option<String> {
    let rest = module_name.strip_prefix("bt@")?;
    let (pkg, suffix) = rest.split_once('@')?;
    if pkg == "stdlib" || suffix.is_empty() {
        return None;
    }
    Some(format!("bt@{pkg}@"))
}

/// Returns true if `module_name` corresponds to the compiled form of `class_name`.
///
/// ADR 0016/0026: Module names may be prefixed with `bt@` (user code),
/// `bt@stdlib@` (stdlib), `bt@{package}@` (package mode), or unprefixed (legacy/tests).
/// The unprefixed arm is retained because hand-constructed test-fixture `Module`s use
/// bare class names (e.g. "counter"); real compilation always emits a `bt@…` prefix.
pub(super) fn module_matches_class(module_name: &str, class_name: &str) -> bool {
    let snake = to_module_name(class_name);
    module_name == snake
        || module_name == format!("bt@{snake}")
        || module_name == format!("bt@stdlib@{snake}")
        || module_name
            .strip_prefix("bt@")
            .and_then(|rest| rest.rsplit_once('@'))
            .is_some_and(|(_, suffix)| suffix == snake)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_matches_class_unprefixed() {
        assert!(module_matches_class("counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_bt_prefix() {
        assert!(module_matches_class("bt@counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_stdlib_prefix() {
        assert!(module_matches_class("bt@stdlib@integer", "Integer"));
    }

    #[test]
    fn test_module_matches_class_package_prefix() {
        assert!(module_matches_class("bt@my_app@counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_package_multi_word() {
        assert!(module_matches_class("bt@my_app@my_counter", "MyCounter"));
    }

    #[test]
    fn test_module_matches_class_package_subdirectory() {
        // bt@my_app@util@math should match class Math (rsplit_once on last @)
        assert!(module_matches_class("bt@my_app@util@math", "Math"));
    }

    #[test]
    fn test_module_matches_class_no_match() {
        assert!(!module_matches_class("bt@other", "Counter"));
    }

    #[test]
    fn test_user_package_prefix_package_mode() {
        assert_eq!(
            user_package_prefix("bt@bank@account"),
            Some("bt@bank@".into())
        );
    }

    #[test]
    fn test_user_package_prefix_deep_path() {
        assert_eq!(
            user_package_prefix("bt@sicp@scheme@eval"),
            Some("bt@sicp@".into())
        );
    }

    #[test]
    fn test_user_package_prefix_stdlib() {
        assert_eq!(user_package_prefix("bt@stdlib@integer"), None);
    }

    #[test]
    fn test_user_package_prefix_unprefixed() {
        assert_eq!(user_package_prefix("counter"), None);
    }

    #[test]
    fn test_user_package_prefix_bt_only() {
        assert_eq!(user_package_prefix("bt@counter"), None);
    }

    #[test]
    fn test_versioned_var_version_zero_returns_prefix() {
        assert_eq!(versioned_var("State", 0), "State");
        assert_eq!(versioned_var("StateAcc", 0), "StateAcc");
        assert_eq!(versioned_var("ClassVars", 0), "ClassVars");
        assert_eq!(versioned_var("Self", 0), "Self");
    }

    #[test]
    fn test_versioned_var_appends_version_number() {
        assert_eq!(versioned_var("State", 1), "State1");
        assert_eq!(versioned_var("State", 2), "State2");
        assert_eq!(versioned_var("StateAcc", 1), "StateAcc1");
        assert_eq!(versioned_var("ClassVars", 3), "ClassVars3");
        assert_eq!(versioned_var("Self", 5), "Self5");
    }

    #[test]
    fn test_versioned_var_large_version() {
        // Verify capacity pre-allocation does not truncate for larger versions.
        assert_eq!(versioned_var("State", 100), "State100");
        assert_eq!(versioned_var("State", 9999), "State9999");
    }

    #[test]
    fn test_metaclass_tag_appends_class_suffix() {
        assert_eq!(metaclass_tag("Array"), "Array class");
        assert_eq!(metaclass_tag("Object"), "Object class");
        assert_eq!(metaclass_tag("MyApp"), "MyApp class");
    }

    #[test]
    fn test_ext_var_formats_index() {
        assert_eq!(ext_var(0), "_Ext0");
        assert_eq!(ext_var(1), "_Ext1");
        assert_eq!(ext_var(42), "_Ext42");
    }
}
