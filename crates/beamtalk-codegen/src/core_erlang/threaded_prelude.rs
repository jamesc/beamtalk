// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The threaded-expression prelude and sequencing pipeline
//! (`threaded_expression`, `sequence_children`, `subexpr_needs_prelude`, ...),
//! split from `util.rs`.

use super::sequencing::PrecompiledScope;
use super::threaded_expr::ProducerTail;
use super::threaded_ir::{
    BindOp, FrameId, RenderCtx, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
    render, render_value,
};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    BinarySegment, CascadeMessage, Expression, MessageSelector, StringSegment,
};
use beamtalk_core::source_analysis::Span;

/// One cascade message's decomposed shape — selector, arguments, and the
/// message's own span — as [`CoreErlangGenerator::cascade_self_dispatch_messages`]
/// and [`CoreErlangGenerator::threaded_cascade_on_self`] pass it around.
/// Named so the type isn't repeated (and so clippy's `type_complexity`
/// lint stays quiet about the tuple).
type CascadeSelfMessage<'e> = (&'e MessageSelector, &'e [Expression], Span);

/// Which effects [`CoreErlangGenerator::subexpr_needs_prelude`]'s probe counts
/// as needing a prelude.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PreludeProbe {
    /// Everything `threaded_expression` produces a prelude for, including a
    /// class-side in-place effect (a class-variable write or a same-class
    /// self-send, ADR 0130), which must pin its evaluation order against its
    /// siblings.
    Sequencing,
    /// As [`Self::Sequencing`] without the class-side effects: they thread
    /// nothing, so they do not make a conditional need its mutation-threaded
    /// form.
    Threading,
    /// BT-3816: only a field assignment in value position (the value-type
    /// method body's `Self` rebind).
    FieldWrites,
}

impl CoreErlangGenerator {
    /// ADR 0118 §Decisions 1–3: compiles `expr` in a
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
    ///   once, hoist all or none" rule made universal; both use
    ///   [`Self::bind_subexpr_to_temp`].
    /// - **Any other parent** (literal elements, interpolation segments,
    ///   a `^` value): the planner (`hoist_nested_self_sends`) as before —
    ///   its order-safe hoists land in the prelude as real `Bind`s, its
    ///   order-unsafe ones keep the same warning. Phase 1b replaces
    ///   this fallback with the sequencing rule for those parent kinds.
    ///
    /// Everything else is pure: one `generate_expression` call, empty
    /// prelude. The decision is taken by a pure AST probe
    /// ([`Self::subexpr_needs_prelude`]) before anything is compiled, so
    /// no sub-expression is ever compiled twice.
    ///
    /// Frame: phase 1a/1b's only consumer was the flat Actor method body
    /// (`lower_body_exprs_with_reply`), always [`FrameId::ROOT`]. ADR 0118
    /// phase 2a widened this to any real frame — a conditional
    /// branch arm, an `on:do:`/`ensure:` body, a Tier 2 stateful-block body
    /// — via the `frame` parameter; phase 2b widens it once more
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
        // BT-4366: a producer lowered here never passes through
        // `generate_expression`, so it names its own line
        // for the call nodes nested in its `let`/`case` (see
        // `current_send_span`).
        self.with_current_span(expr.span(), |this| {
            this.threaded_expression_with_sync_window(expr, frame)
        })
    }

    fn threaded_expression_with_sync_window(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        // ADR 0137: with the flag on, a call that may run user code is bracketed.
        if let Some(window) = self.open_sync_window(expr) {
            let version_before = self.state_version();
            let tv = self.threaded_expression_core(expr, frame);
            self.close_sync_window(window);
            return Ok(self.synced_call_value(tv?, expr, frame, version_before));
        }
        self.threaded_expression_core(expr, frame)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the one ordered list of producers, each branch mirrored by `subexpr_needs_prelude`"
    )]
    fn threaded_expression_core(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        let inner = expr.unwrap_parens();
        let span = inner.span();

        // BT-3821: a child an enclosing `sequence_children` already compiled ahead
        // (its prelude spliced into that caller's own) is read back here, never
        // threaded a second time: a producer that threads its operand itself (the
        // inline-control-flow conditional's receiver) sees the sequenced value.
        if let Some(doc) = self.take_precompiled_subexpr(inner) {
            return Ok(ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        }

        // BT-3910 (ADR 0131 §1): a local assignment in value position is a
        // producer: one `LocalRebind` of the enclosing frame, answering the
        // assigned value.
        if self.is_local_assignment_producer(inner) {
            return self.lower_local_assignment_producer(inner, frame);
        }

        // BT-3910 / BT-4000: a conditional whose receiver's prelude rebinds a
        // threaded family (a value-position local assignment, an actor
        // self-send) sequences the receiver ahead, so that prelude lands in the
        // caller's frame.
        if let Some(tv) = self.conditional_receiver_operand(expr, frame)? {
            return Ok(tv);
        }

        // ADR 0131 §1 (Phase 2): a loop or fold at a sequenced operand site
        // of the method body that writes an outer local is a producer: its
        // construct tuple's threaded locals are rebound in the prelude,
        // whatever position it sits in (`local_threading_producer`).
        if let Some(tv) = self.local_threading_producer(inner, frame)? {
            return Ok(tv);
        }

        // BT-3816 / BT-3821: a value-type conditional whose arms or receiver write a
        // field splices its `Self` rebinds into the caller's statement sequence.
        if let Some(tv) = self.vt_conditional_operand(expr, frame)? {
            return Ok(tv);
        }

        // ADR 0118 phase 4: an inline-threaded control-flow
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

        // ADR 0131 §1 (Phase 3b, BT-3817): an `on:do:`/`ensure:` that writes an
        // outer local is a producer at the method root and in an arm frame,
        // whatever position it sits in (its protected body, handler and cleanup
        // are `BranchArm` frames).
        if self.exception_producer_in_frame(inner)
            && let Some(tv) = self.exception_producer_value(inner, ProducerTail::Rebind, frame)?
        {
            return Ok(tv);
        }

        // ADR 0130: a class-method context's own producer — a class-variable
        // write or a same-class self-send — threads nothing, but pins its
        // evaluation order against its siblings (see
        // `class_method_prelude_producer`).
        if self.in_class_method()
            && let Some(tv) = self.class_method_prelude_producer(inner, frame)?
        {
            return Ok(tv);
        }

        // BT-3803: a field assignment in value position (`Transcript show:
        // (self.n := 5)`) rebinds `State` and answers the assigned value, so
        // it is a producer: its `Bind` must land in the caller's statement
        // sequence. Compiled as a plain sub-expression it was a closed
        // document that scoped the rebind away (`unbound variable 'State1'`).
        // (`is_field_assignment` is `false` in a class method, where the write
        // is an in-place class-variable `put`, ADR 0130 §2.)
        if self.is_field_assignment(inner) {
            let mut prelude = Vec::new();
            let val_var = self.lower_field_assignment_bind(inner, frame, span, &mut prelude)?;
            return Ok(ThreadedValue {
                prelude,
                value: ValueRef::Var(val_var),
            });
        }

        // BT-3816: an inlined `[... self.f := ...] value` splices its body's
        // `Self` rebinds into the caller's statement sequence.
        if let Some(tv) = self.vt_inline_block_value_producer(inner, frame)? {
            return Ok(tv);
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
            let (prelude, dispatch_var) =
                self.sequenced_self_dispatch(selector, arguments, frame, span)?;
            return Ok(ThreadedValue {
                prelude,
                value: Self::self_dispatch_result_value(&dispatch_var),
            });
        }

        // BT-4318: a `super` send that threads the method's `State` is a
        // producer: its `{reply, Result, NewState}` tuple is unpacked into a
        // `State` `Bind`, and it answers `Result` in any value position.
        if self.is_super_send_producer(inner) {
            return self.super_send_producer(inner, frame);
        }

        if let Some(children) = Self::sequenced_send_children(inner) {
            // BT-4591 fault: skip a `super` send's argument pre-sequencing.
            let children = if fault!(SuperArgsNotPresequenced) && Self::is_super_message_send(inner)
            {
                Vec::new()
            } else {
                children
            };
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
            // BT-3791: a cascade on any other receiver whose operands need a
            // prelude (a state-threading self-send in an argument, e.g. a
            // computed `classMethods:` map value) is a producer too: its
            // hoisted `State` rebinds must reach the consumer's statement
            // sequence, not stay nested in the cascade's value expression.
            if self.cascade_operands_need_prelude(receiver, messages, PreludeProbe::Sequencing) {
                let (prelude, value) = self.generate_cascade_parts(receiver, messages)?;
                return Ok(ThreadedValue {
                    prelude,
                    value: ValueRef::Doc(value),
                });
            }
        }

        // Anything else — a `Block` (a closure; its contents run later, or
        // never) or a non-`self` / class-method `Cascade`: pure, empty
        // prelude. ADR 0118 phase 2b: `sequenced_send_children` treating an
        // `is_cast` send as opaque
        // is fixed at the source instead of patched here: cast sends are
        // a covered send like any other (see `sequenced_send_children`'s
        // own doc comment), so a self-send nested in a cast's receiver
        // (`(self next) process!`) is threaded by the covered-send branch
        // above, not this fallback. A non-`self` `Cascade` was never walked
        // by the old planner either (its walk only matched an explicit list
        // of parent shapes and treated everything else — Cascade included —
        // as a no-op leaf), so this fallback is a genuine no-op for every
        // shape that still reaches it: byte-identical to the pre-ADR-0118
        // planner's behaviour for those shapes, just without running it.
        //
        // ADR 0118 phase 5b: see `generate_expression_as_value`'s
        // doc comment for the one exception (a mutation-threaded
        // `do:`/dict-`do:` nested in a direct-params loop) this plain
        // `Doc(doc)` wrapping does not cover on its own.
        let mut prelude = Vec::new();
        let value = self.generate_expression_as_value(expr, span, &mut prelude)?;
        Ok(ThreadedValue { prelude, value })
    }

    /// The receiver-prelude splice: a conditional or `match:` (producer or not)
    /// whose receiver's (scrutinee's) prelude rebinds a threaded family
    /// ([`Self::conditional_receiver_is_sequenced`]) sequences the receiver
    /// ahead, so that prelude lands in the caller's frame instead of a closed
    /// document, and then lowers the conditional over the sequenced receiver
    /// (already registered, so every lowering reads its value back).
    ///
    /// - BT-3910 (ADR 0131 §1): a receiver holding a value-position local
    ///   assignment (`((t := t + 1) > 0) and: [...]`): its `LocalRebind`.
    /// - BT-4000: a receiver whose evaluation advances the actor `State`
    ///   (`(self done) ifTrue: [...]`, `((self isAlive: n) and: [...]) ifTrue:`):
    ///   its dispatch `Bind`s. Rendered into the conditional's own tuple
    ///   document they were invisible to the frame, so in a `Letrec` loop body
    ///   the conditional's `State` extraction read a version no `Bind` of the
    ///   loop frame produced (`UnboundVersion`), and the loop's recursive call
    ///   carried the pre-statement `State`, dropping the self-send's change.
    /// - BT-5289: a `match:` scrutinee whose evaluation advances `State` (the
    ///   construct's [`Self::sequenced_construct_operand`]), for the same reason.
    fn conditional_receiver_operand(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let inner = expr.unwrap_parens();
        let Some(receiver) = Self::sequenced_construct_operand(inner)
            .filter(|r| self.conditional_receiver_is_sequenced(r))
        else {
            return Ok(None);
        };
        #[cfg(test)]
        let early_prestore_read = self.fault_prestore_read_before_operands(inner);
        let (mut prelude, scope) = self.sequence_children(&[receiver], frame)?;
        #[cfg(test)]
        Self::publish_fault_prestore_read(early_prestore_read);
        let over_receiver = match self.local_threading_producer(inner, frame)? {
            Some(tv) => Some(tv),
            None => self.inline_control_flow_producer(inner)?,
        };
        let value = if let Some(tv) = over_receiver {
            prelude.extend(tv.prelude);
            tv.value
        } else {
            self.generate_expression_as_value(expr, inner.span(), &mut prelude)?
        };
        self.finish_precompiled_scope(scope)?;
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// The value-type conditional cases of [`Self::threaded_expression`]
    /// (`None` outside a value-type instance method, and for any other
    /// expression): a conditional whose field writes the caller's statement
    /// sequence must see.
    ///
    /// - BT-3821: a conditional whose ARMS write a field (`self take: (c ifTrue:
    ///   [self.m := 1] ifFalse: [0])`): its `Self` merge `Bind` and its value are
    ///   spliced into the caller's statement sequence
    ///   ([`Self::vt_conditional_value_producer`]).
    /// - BT-3816: a conditional whose receiver holds a field write (`self take:
    ///   (((self.n := 5) > 3) ifTrue: [1] ifFalse: [2])`): the receiver is
    ///   sequenced ahead, the conditional then reads its temp. The arms may still
    ///   need threading of their own (an outer local): compiled as a plain
    ///   document, their stateful blocks were dispatched as closures
    ///   (`erlc`-clean, wrong at runtime) and the local lost. The
    ///   inline-control-flow producer lowers the conditional over the sequenced
    ///   receiver instead — the receiver is already registered, so the producer
    ///   reads its temp back (the top of `threaded_expression`) rather than
    ///   re-threading its field write into a closed document that would scope the
    ///   `Self` rebind away.
    fn vt_conditional_operand(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if self.context != CodeGenContext::ValueType || self.in_class_method() {
            return Ok(None);
        }
        let inner = expr.unwrap_parens();
        if let Some(tv) = self.vt_conditional_value_producer(inner, frame)? {
            return Ok(Some(tv));
        }
        let Some(receiver) = Self::conditional_send_receiver(inner)
            .filter(|r| self.subexpr_has_field_write_producer(r))
        else {
            return Ok(None);
        };
        let (mut prelude, scope) = self.sequence_children(&[receiver], frame)?;
        let value = if let Some(tv) = self.inline_control_flow_producer(inner)? {
            prelude.extend(tv.prelude);
            tv.value
        } else {
            self.generate_expression_as_value(expr, inner.span(), &mut prelude)?
        };
        self.finish_precompiled_scope(scope)?;
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// BT-3809: the one lowering of an Actor self-send *statement* with its
    /// operands sequenced (ADR 0118 §Decision 3): each operand that needs a
    /// prelude is compiled first and its `State` rebind spliced into the
    /// returned prelude, then the dispatch `Statement` + `Bind` follow, so the
    /// dispatch reads the post-operand `State` and a sibling operand sees the
    /// previous operand's rebind. Returns the prelude and the dispatch
    /// variable holding the reply tuple.
    ///
    /// Shared by [`Self::threaded_expression`]'s self-send producer, the
    /// Letrec body (`lower_letrec_non_assign_expr` via `threaded_expression`),
    /// every fold body statement that is a self-send (`lower_foldl_body`) and
    /// the `on:do:`/`ensure:` arm's self-send statement
    /// (`generate_exception_body_with_threading_inner`, which keeps its own `ExResult`
    /// epilogue), so a body statement cannot compile its operands in a
    /// closed argument list that scopes their rebind away.
    pub(super) fn sequenced_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<(Vec<ThreadedStmt>, String)> {
        let children: Vec<&Expression> = arguments.iter().collect();
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        let (dispatch_prelude, dispatch_var) =
            self.generate_self_dispatch_parts(selector, arguments, frame, span)?;
        self.finish_precompiled_scope(scope)?;
        prelude.extend(dispatch_prelude);
        Ok((prelude, dispatch_var))
    }

    /// ADR 0118 phase 5b: compiles `expr` via plain
    /// `generate_expression` and wraps the result as a [`ValueRef`] —
    /// `ValueRef::Doc` in the common case, but `ValueRef::Literal("'nil'")`
    /// with the returned `Document` pushed onto `prelude` instead when
    /// `direct_params_do_open_chain` comes back set: a mutation-threaded
    /// `do:`/dict-`do:` nested in a direct-params loop
    /// (`in_direct_params_loop`) leaves its own returned `Document` as an
    /// open, dangling let-chain and sets that flag rather than returning a
    /// closed value (several accumulator vars may have been
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
        self.loop_mode.direct_params_do_open_chain = false;
        let doc = self.generate_expression(expr)?;
        if self.loop_mode.direct_params_do_open_chain {
            self.loop_mode.direct_params_do_open_chain = false;
            prelude.push(ThreadedStmt::Statement(doc, span));
            return Ok(ValueRef::Literal("'nil'"));
        }
        Ok(ValueRef::Doc(doc))
    }

    /// The children of a literal-container parent the sequencing rule
    /// applies to, in evaluation order: `ListLiteral` elements then an
    /// optional tail, `ArrayLiteral`/`TupleLiteral` elements, `MapLiteral` keys and
    /// values (key before value, pair by pair), and `BinaryConstruction`
    /// segment values and sizes (value before size, segment by segment,
    /// ADR 0143). `None` for anything else.
    /// Safe to treat like [`Self::sequenced_send_children`]'s children
    /// (no interleaved effect between compiling one and the next): each
    /// of these parents' own codegen
    /// ([`Self::generate_list_literal`]/[`Self::generate_array_literal`]/[`Self::generate_tuple_literal`]/
    /// [`Self::generate_map_literal`]/[`Self::generate_binary_construction`],
    /// whose literal values and sizes are trivial children no sequencing
    /// registers) compiles every element through
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
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => Some(elements.iter().collect()),
            Expression::MapLiteral { pairs, .. } => {
                let mut children = Vec::with_capacity(pairs.len() * 2);
                for pair in pairs {
                    children.push(&pair.key);
                    children.push(&pair.value);
                }
                Some(children)
            }
            Expression::BinaryConstruction { segments, .. } => Some(
                segments
                    .iter()
                    .flat_map(BinarySegment::sub_expressions)
                    .collect(),
            ),
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
        if !underlying_receiver.is_self() {
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
    /// which a self-send threads `State` (a class method threads nothing,
    /// ADR 0130; same exclusion as the planner's).
    pub(super) fn in_actor_instance_context(&self) -> bool {
        self.context == CodeGenContext::Actor && !self.in_class_method() && !self.in_cell_closure()
    }

    /// `true` if `expr` (already paren-unwrapped) is a dispatching Actor
    /// self-send [`Self::threaded_expression`] must compile through the
    /// producer: in an Actor instance context.
    fn is_prelude_producer(&self, expr: &Expression) -> bool {
        self.in_actor_instance_context() && self.is_dispatching_actor_self_send(expr)
    }

    /// `true` if `expr` (already paren-unwrapped) is a `super` send whose
    /// dispatch threads the method's own `State`
    /// ([`Self::super_send_threads_state`]) — [`Self::super_send_producer`]'s
    /// shape. Every other `super` send answers a plain value.
    pub(super) fn is_super_send_producer(&self, expr: &Expression) -> bool {
        if fault!(SuperSendNotAProducer) {
            return false;
        }
        matches!(expr, Expression::MessageSend { is_cast: false, .. })
            && Self::is_super_message_send(expr)
            && self.super_send_threads_state()
    }

    /// BT-4318: the one lowering of a `State`-threading `super` send
    /// ([`Self::is_super_send_producer`]), in statement and value position
    /// alike. Its arguments are sequenced ahead (BT-3814), the
    /// `beamtalk_dispatch:super_reply/5` call is bound to a `_SuperReply` temp
    /// (bracketed by `generate_super_send` when it may reach the default
    /// arm), and a `State` `Bind` takes the callee's state from the reply
    /// tuple; the value is the reply's result.
    pub(super) fn super_send_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        let Expression::MessageSend {
            selector,
            arguments,
            span,
            ..
        } = expr
        else {
            return Err(CodeGenError::Internal(
                "super_send_producer called on a non-MessageSend expression".to_string(),
            ));
        };
        let span = *span;
        let children: Vec<&Expression> = arguments.iter().collect();
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        let source_version = self.state_version();
        self.in_super_send_producer = true;
        let call = self.generate_super_send(selector, arguments);
        self.in_super_send_producer = false;
        let call = call?;
        self.finish_precompiled_scope(scope)?;
        let reply_var = self.fresh_temp_var("SuperReply");
        prelude.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(reply_var.clone()), " = ", call, " in "],
            span,
        ));
        self.push_state_bind(
            &mut prelude,
            frame,
            VersionedVar::new(VersionPrefix::State, source_version, frame),
            BindOp::Direct(ValueRef::Doc(docvec![form::element(
                3,
                leaf::var(reply_var.clone())
            ),])),
            span,
        );
        Ok(ThreadedValue {
            prelude,
            value: ValueRef::Doc(docvec![form::element(2, leaf::var(reply_var)),]),
        })
    }

    /// ADR 0130: compiles `expr` (already paren-unwrapped) through the
    /// class-method-context producer when it is, at its own top level, a
    /// class-variable write or a same-class class-method self-send
    /// ([`Self::is_class_side_effect`]) — `None` for anything else. Called
    /// from [`Self::threaded_expression`] (falls through to its generic paths
    /// on `None`), gated to `self.in_class_method()`.
    ///
    /// Nothing is threaded (a write is an in-place `put`, a send rebinds
    /// nothing), but a class variable is shared mutable state and Core Erlang
    /// leaves the evaluation order of call arguments and tuple/list elements
    /// unspecified, so the effect is bound in an ordered `let` of its own to
    /// pin its order against its siblings.
    ///
    /// [`Self::is_class_side_effect`] is DELIBERATELY narrower than
    /// [`Self::try_handle_class_method_self_send`]'s own condition (any
    /// `self` receiver): `generate_message_send`'s real dispatch order runs
    /// ProtoObject/Object/Block/Dict/List/Boolean/spawn-await/Erlang-interop/
    /// Logger/class-reference checks BEFORE `try_handle_class_method_self_send`
    /// ever runs, so `self class` or `self isNil` must still reach THOSE
    /// handlers — a selector the class itself declares as a class method can
    /// never collide with one of those reserved well-known names.
    ///
    /// ADR 0131 Phase 2d (BT-3795): the value of a class-variable write is a
    /// sequenced operand site (`sequenced_operand_sites`), so a local-threading
    /// producer in it (`self.n := (xs collect: [...]) size`) is sequenced
    /// ahead of the write and its prelude spliced before it. BT-4014: so are
    /// the arguments of a same-class self-send (also sequenced operand
    /// sites). Only an effect that holds a producer is sequenced, so every
    /// other class-variable write and self-send compiles exactly as before.
    fn class_method_prelude_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if self.is_class_side_effect(expr) {
            // BT-4014: the operands of either kind of effect are sequenced
            // alike: a class-variable write's value, and a self-send's
            // arguments (`self take: ([n > 0] whileTrue: [n := n - 1])`). The
            // effect itself is closed into the `let` below, so a producer left
            // to compile inside it would scope its `LocalRebind`s there and
            // every later read of the local would be unbound
            // (`ProducerSiteNotThreaded`, checked by
            // `lower_root_statement_with_producer`).
            // Test-only fault injection: the pre-BT-4014 lowering, which
            // sequenced a write's value only.
            let only_writes = fault!(ClassSendOperandsInPlace);
            let children: Vec<&Expression> =
                if only_writes && !matches!(expr, Expression::Assignment { .. }) {
                    Vec::new()
                } else if self.statement_has_local_producer(expr) {
                    Self::single_sequenced_child(expr)
                        .map(|value| vec![value])
                        .or_else(|| Self::sequenced_send_children(expr))
                        .unwrap_or_default()
                } else {
                    Vec::new()
                };
            let (mut prelude, scope) = self.sequence_children(&children, frame)?;
            let doc = self.generate_expression(expr)?;
            self.finish_precompiled_scope(scope)?;
            let result_var = self.fresh_temp_var("Effect");
            prelude.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    beamtalk_cerl_doc::leaf::var(result_var.clone()),
                    " = ",
                    doc,
                    " in "
                ],
                expr.span(),
            ));
            return Ok(Some(ThreadedValue {
                prelude,
                value: ValueRef::Var(result_var),
            }));
        }
        Ok(None)
    }

    /// Whether a conditional's `receiver` (or a `match:`'s scrutinee,
    /// BT-5289: [`Self::sequenced_construct_operand`]) is sequenced ahead of
    /// the construct by [`Self::conditional_receiver_operand`], its prelude
    /// spliced into the caller's frame: it holds a value-position local
    /// assignment (BT-3910), or, in an actor instance method, its evaluation
    /// advances the `State` family (an actor self-send anywhere in its
    /// sequenceable sub-tree, BT-4000).
    ///
    /// **Invariant** (every scope kind): a statement whose evaluation produces
    /// a `State` version binds it in the frame the statement runs in — the
    /// method root, a conditional arm, a `Letrec` loop body, a fold body, an
    /// `on:do:`/`ensure:` arm — before anything reads it. A conditional's
    /// receiver is evaluated first and unconditionally, so its prelude belongs
    /// to that frame, not inside the conditional's own tuple document. A
    /// value-type receiver that writes a field has its own splice
    /// (`vt_conditional_operand`); a class-method receiver produces no family
    /// version (ADR 0130).
    pub(in crate::core_erlang) fn conditional_receiver_is_sequenced(
        &self,
        receiver: &Expression,
    ) -> bool {
        if !self.contained_local_assignment_targets(receiver).is_empty() {
            return true;
        }
        // Test-only fault injection: the pre-BT-4000 lowering, so the
        // verifier's `UnboundVersion` is shown to fire on a real compile
        // (`tests/loop_body_conditional_receiver_corpus.rs`).
        if fault!(ReceiverPreludeInTuple) {
            return false;
        }
        self.in_actor_instance_context() && self.subexpr_needs_prelude(receiver)
    }

    /// Whether `expr` is a conditional send or a `match:` whose
    /// [`Self::sequenced_construct_operand`] is sequenced ahead of it
    /// ([`Self::conditional_receiver_is_sequenced`], BT-3910 / BT-4000 /
    /// BT-5289).
    pub(in crate::core_erlang) fn is_construct_with_sequenced_operand(
        &self,
        expr: &Expression,
    ) -> bool {
        Self::sequenced_construct_operand(expr.unwrap_parens())
            .is_some_and(|r| self.conditional_receiver_is_sequenced(r))
    }

    /// The operand an inline-lowered construct evaluates first and
    /// unconditionally, ahead of choosing an arm: a conditional send's
    /// receiver ([`Self::conditional_send_receiver`]) or a `match:`'s
    /// scrutinee. The one operand [`Self::conditional_receiver_operand`]
    /// sequences into the caller's frame when its prelude advances a threaded
    /// family.
    ///
    /// BT-5289: a `match:` scrutinee is that operand too. Compiled inside the
    /// `match:`'s own tuple document (`generate_match`), a scrutinee whose
    /// evaluation advances `State` (a send run in a `SyncedCall` window, a
    /// self-send) bound that version where `verify()` could not see it, so
    /// the frame's `State` extraction off the tuple read a version no `Bind`
    /// of the frame produced (`UnboundVersion` / `NonLinearVersion`). In a
    /// conditional arm the emitted code was still right; in a `Letrec` loop
    /// body the recursive call carried the pre-statement `State`, dropping the
    /// scrutinee's and the arms' field writes.
    pub(super) fn sequenced_construct_operand(expr: &Expression) -> Option<&Expression> {
        match expr {
            // Test-only fault injection: the pre-BT-5289 routing, so a
            // producer `match:` compiles its own scrutinee and
            // `SeedBeforeScrutinee` can seed ahead of it
            // (`tests/match_scrutinee_state_corpus.rs`).
            Expression::Match { .. } if fault!(SeedBeforeScrutinee) => None,
            Expression::Match { value, .. } => Some(value.as_ref()),
            _ => Self::conditional_send_receiver(expr),
        }
    }

    /// The receiver of a conditional send (`ifTrue:`, `and:`, ...), the one
    /// operand the sequencing rule leaves to the conditional's own lowering.
    pub(super) fn conditional_send_receiver(expr: &Expression) -> Option<&Expression> {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };
        Self::is_conditional_send(selector, arguments).then_some(receiver.as_ref())
    }

    /// BT-4451: whether a send of `selector` with `arguments` is a conditional
    /// the compiler lowers in place — the shared
    /// [`beamtalk_core::state_threading_selectors::is_inlined_conditional_send`],
    /// behind [`Self::conditional_send_receiver`] (whose receiver the
    /// conditional's lowering threads) and [`Self::sequenced_send_children`]
    /// (which leaves a conditional opaque). Any other conditional-selector send
    /// is a covered send: `Erlang beamtalk_guard_return ifTrue: c` is an FFI
    /// call whose `Erlang <module>` receiver `try_handle_erlang_interop`
    /// consumes structurally, so sequencing it ahead registered a value
    /// nothing read back ("never substituted").
    fn is_conditional_send(selector: &MessageSelector, arguments: &[Expression]) -> bool {
        beamtalk_core::state_threading_selectors::is_inlined_conditional_send(
            selector.name().as_str(),
            arguments,
        )
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
    /// ADR 0118 phase 2b: a cast send (`X!`) is covered here too
    /// — it is compiled through `generate_cast_send`/`generate_expression`
    /// like any other send, consulting `precompiled_subexprs` for its
    /// receiver and arguments the same way (`capture_argument_list_doc`/
    /// `capture_subexpr_sequence`), so nothing about it needs opaque
    /// treatment. This is the ONLY path that threads a self-send nested
    /// in a cast's receiver (`(self next) process!`) — the planner-based
    /// fallback that previously handled it is gone
    /// (`bt3416_self_send_nested_in_a_cast_sends_
    /// receiver_still_threads`, `tests/gen_server.rs`).
    pub(super) fn sequenced_send_children(expr: &Expression) -> Option<Vec<&Expression>> {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };
        if Self::is_conditional_send(selector, arguments) {
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
    /// prelude — a `Block`/anything unmatched needs no recursion at all; a
    /// `FieldAccess` never produces a dispatch requiring a prelude; a
    /// conditional selector's receiver is threaded by its own intrinsic
    /// instead — so none of these residual shapes ever need a prelude
    /// here.
    pub(super) fn subexpr_needs_prelude(&self, expr: &Expression) -> bool {
        self.subexpr_needs_prelude_with(expr, PreludeProbe::Sequencing)
    }

    /// BT-3816: whether the value-position expression `expr` holds a field
    /// assignment ([`PreludeProbe::FieldWrites`]) — the only producer the
    /// value-type method body lowers through `threaded_expression`.
    pub(super) fn subexpr_has_field_write_producer(&self, expr: &Expression) -> bool {
        self.subexpr_needs_prelude_with(expr, PreludeProbe::FieldWrites)
    }

    /// BT-4034: the one list of constructs [`Self::thread_ahead`] registers
    /// already unwrapped (paren-unwrapped `expr`), restricted to those `probe`
    /// counts: an inline-threaded conditional or `match:` (ADR 0118 phase 4;
    /// not a field-write producer), an `on:do:`/`ensure:` producer (ADR 0131
    /// Phase 3b), an evaluated literal block (BT-3841), or a conditional or
    /// `match:` whose receiver's (scrutinee's) prelude is spliced into the
    /// caller's frame (BT-3910 / BT-4000 / BT-5289,
    /// `conditional_receiver_operand`).
    ///
    /// Both [`Self::thread_ahead_unwraps`] (at [`PreludeProbe::Sequencing`])
    /// and [`Self::subexpr_needs_prelude_with`] read this one function, so
    /// `thread_ahead_unwraps(e)` implies `subexpr_needs_prelude(e)` by
    /// construction: a disjunct added here is a prelude producer for
    /// `thread_ahead`'s gate too. (The gate's only earlier exits are a trivial
    /// expression — never one of these sends — and a value an enclosing scope
    /// already registered, which `expression_doc` reads back unwrapped.)
    fn unwrapped_producer_under(&self, expr: &Expression, probe: PreludeProbe) -> bool {
        let inner = expr.unwrap_parens();
        self.inline_producer_construct_under(inner, probe)
            || self.is_construct_with_sequenced_operand(inner)
    }

    /// The inline-threaded constructs of [`Self::unwrapped_producer_under`]
    /// (paren-unwrapped `expr`) — everything but the sequenced-receiver
    /// conditional; [`Self::inline_producer_construct`] at
    /// [`PreludeProbe::Sequencing`].
    fn inline_producer_construct_under(&self, expr: &Expression, probe: PreludeProbe) -> bool {
        (probe != PreludeProbe::FieldWrites && self.inline_control_flow_needs_threading(expr))
            || self.exception_producer_in_frame(expr)
            || self.block_value_producer_in_frame(expr)
    }

    /// [`Self::unwrapped_producer_under`] at [`PreludeProbe::Sequencing`], the
    /// probe [`Self::thread_ahead`] gates on.
    pub(super) fn thread_ahead_unwraps(&self, expr: &Expression) -> bool {
        self.unwrapped_producer_under(expr, PreludeProbe::Sequencing)
    }

    /// [`Self::inline_producer_construct_under`] at
    /// [`PreludeProbe::Sequencing`]: whether `thread_ahead` has already spliced
    /// `expr` (paren-unwrapped) as an inline-threaded producer when a consumer
    /// compiles it. `expression_doc` then answers the already-unwrapped value,
    /// not a `{Value, State}` tuple.
    pub(super) fn inline_producer_construct(&self, expr: &Expression) -> bool {
        self.inline_producer_construct_under(expr, PreludeProbe::Sequencing)
    }

    /// [`Self::subexpr_needs_prelude`], with `probe` saying which effects count
    /// as needing a prelude ([`PreludeProbe`]).
    fn subexpr_needs_prelude_with(&self, expr: &Expression, probe: PreludeProbe) -> bool {
        let inner = expr.unwrap_parens();
        if Self::is_trivial_subexpr(inner) {
            return false;
        }
        // ADR 0118 phase 5b: a child an ENCLOSING `sequence_children`
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
        // failing with "never substituted".
        if self.precompiled_subexprs_contains(inner) {
            return false;
        }
        // BT-3816: the value-type body lowering asks only about field writes;
        // every other producer below has its own value-type path.
        let all_producers = probe != PreludeProbe::FieldWrites;
        // BT-3910: mirrors `threaded_expression`'s value-position local
        // assignment producer, for every probe: its `LocalRebind` belongs to
        // the consumer's statement sequence whatever it is sequencing.
        if self.is_local_assignment_producer(inner) {
            return true;
        }
        // BT-4034: every construct `thread_ahead` registers already unwrapped
        // ([`Self::unwrapped_producer_under`], the one list
        // `thread_ahead_unwraps` reads) needs a prelude — derived from that
        // list, not mirrored by hand, so a consumer routing a
        // `thread_ahead_unwraps` statement through `thread_ahead` always gets
        // the prelude and the unwrapped value it relies on.
        if self.unwrapped_producer_under(inner, probe) {
            return true;
        }
        // ADR 0131 Phase 2: mirrors `threaded_expression`'s
        // `local_threading_producer` branch.
        if all_producers && self.local_producer_locals(inner).is_some() {
            return true;
        }
        // ADR 0118 phase 5b: mirrors `threaded_expression`'s
        // `class_method_prelude_producer` branch — a same-class self-send
        // or a direct class-var assignment, in class-method context, needs
        // a prelude regardless of nesting depth (a message argument, a
        // binary operand, a cascade message, ...). A pure predicate check
        // (not the mutating producer call itself) so this stays a probe.
        if self.in_class_method()
            && probe == PreludeProbe::Sequencing
            && self.is_class_side_effect(inner)
        {
            return true;
        }
        if self.is_prelude_producer(inner) || self.is_super_send_producer(inner) {
            return true;
        }
        // ADR 0137: mirrors `threaded_expression`'s bracket window.
        if all_producers && self.is_sync_site(inner) {
            return true;
        }
        // BT-3803: mirrors `threaded_expression`'s field-assignment producer.
        if self.is_field_assignment(inner) {
            return true;
        }
        // BT-3821: mirrors `threaded_expression`'s field-writing-arms
        // conditional producer, for every probe: its prelude carries the `Self`
        // merge `Bind` whatever the consumer is sequencing.
        if self.is_vt_field_conditional_producer(inner) {
            return true;
        }
        // BT-3816: mirrors `threaded_expression`'s conditional-receiver branch
        // (a conditional send is otherwise opaque to the sequencing rule).
        if probe == PreludeProbe::FieldWrites
            && let Some(receiver) = Self::conditional_send_receiver(inner)
        {
            return self.subexpr_needs_prelude_with(receiver, probe);
        }
        // BT-3816: mirrors `threaded_expression`'s inlined-block-value producer.
        if self.is_vt_field_writing_block_value(inner) {
            return true;
        }
        if let Some(children) = Self::sequenced_send_children(inner) {
            return children
                .iter()
                .any(|child| self.subexpr_needs_prelude_with(child, probe));
        }
        // ADR 0118 phase 1b: mirrors `threaded_expression`'s own cases
        // exactly, for the same reason phase 1a's version of this
        // function mirrored ITS cases — the probe must never disagree
        // with what compiling would do.
        if let Some(children) = Self::literal_container_children(inner) {
            return children
                .iter()
                .any(|child| self.subexpr_needs_prelude_with(child, probe));
        }
        if let Some(value) = Self::single_sequenced_child(inner) {
            return self.subexpr_needs_prelude_with(value, probe);
        }
        if let Expression::StringInterpolation { segments, .. } = inner {
            return segments.iter().any(|seg| match seg {
                StringSegment::Interpolation(e) => self.subexpr_needs_prelude_with(e, probe),
                StringSegment::Literal(_) => false,
            });
        }
        if let Expression::Cascade {
            receiver, messages, ..
        } = inner
        {
            return (all_producers
                && self
                    .cascade_self_dispatch_messages(receiver, messages)
                    .is_some())
                || self.cascade_operands_need_prelude(receiver, messages, probe);
        }
        false
    }

    /// BT-3791: `true` when a cascade NOT on `self` has an operand (its
    /// receiver, or any message argument — a `classMethods:` map's computed
    /// values included, via the literal-container rule) that needs a prelude.
    /// `generate_cascade_parts` hoists such preludes in front of the message
    /// that uses them; mirrors `threaded_expression`'s cascade branch so the
    /// hoisted `State` rebinds reach the consumer instead of being nested
    /// inside the cascade's value binding.
    fn cascade_operands_need_prelude(
        &self,
        receiver: &Expression,
        messages: &[CascadeMessage],
        probe: PreludeProbe,
    ) -> bool {
        let (underlying, first_args): (&Expression, &[Expression]) = match receiver {
            Expression::MessageSend {
                receiver: inner,
                arguments,
                ..
            } => (inner.as_ref(), arguments.as_slice()),
            _ => (receiver, &[]),
        };
        std::iter::once(underlying)
            .chain(first_args.iter())
            .chain(messages.iter().flat_map(|m| m.arguments.iter()))
            .any(|e| self.subexpr_needs_prelude_with(e, probe))
    }

    /// ADR 0118 phase 0: `true` if compiling `expr`
    /// through [`Self::threaded_expression`] would give it a non-empty
    /// prelude — the decision predicate for "does this conditional need
    /// inlining because of its receiver?" (`conditional_needs_mutation_threading`).
    ///
    /// This predicate shares [`Self::subexpr_needs_prelude`] with the
    /// sequencing rule itself — the same "would compiling this actually
    /// produce a prelude?" question the rule already answers for every one
    /// of its own children — so it cannot disagree with what compiling
    /// `expr` does, without duplicating an emitting walk. Lives here
    /// (not `control_flow/conditionals.rs`) because it is a thin
    /// wrapper over `subexpr_needs_prelude`.
    ///
    /// ADR 0118 phase 4: `subexpr_needs_prelude` recognizes a nested
    /// mutation-threaded `and:`/`or:`/nil-conditional/`match:` receiver
    /// (`((self recordOnce: which) and: [x]) ifTrue:ifFalse:`) directly, via
    /// [`Self::inline_control_flow_needs_threading`], so this function needs
    /// no `and:`/`or:` carve-out of its own — the signal is never lost in
    /// the first place. This predicate special-cases no remaining
    /// class-var/actor-state receiver positions — kept as a thin wrapper
    /// for its own call sites' sake.
    pub(in crate::core_erlang) fn conditional_receiver_needs_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        self.subexpr_needs_prelude_with(expr, PreludeProbe::Threading)
    }

    /// The `State` variable a consumer must continue from once it has
    /// spliced `prelude` — the target of the prelude's last top-level
    /// `State` `Bind`, or `version_before` (the counter as read BEFORE
    /// `threaded_expression` ran) when the prelude has none. Read this
    /// instead of `current_state_var()` after a `threaded_expression` call:
    /// the value's own compile may have minted versions INSIDE its closed
    /// document (a conditional receiver's `generate_self_dispatch_call_doc_for`
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
            .find_map(|stmt| match stmt.version_step() {
                Some((target, _, _))
                    if target.prefix == VersionPrefix::State && target.frame == FrameId::ROOT =>
                {
                    Some(target.version)
                }
                _ => None,
            })
            .unwrap_or(version_before);
        super::render_state_prefix(self.loop_mode.in_hybrid_loop, self.in_loop_body, version)
    }

    /// A child the sequencing rule never compiles ahead of its parent: a
    /// literal, an identifier, a class reference, `super`, or a block —
    /// each compiles to a value that cannot raise or observe threaded
    /// state, so the parent may compile it in place after every prelude.
    pub(super) fn is_trivial_subexpr(expr: &Expression) -> bool {
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
    ///
    /// ADR 0131 §1a amends the plain-variable exemption: a plain-variable
    /// child is trivial only if no later sibling (up to *k*) that needs a
    /// prelude threads it through a construct reachable inside it
    /// ([`Self::reachable_threaded_locals`]). Otherwise a later
    /// sibling's `LocalRebind` could run before the parent reads it, so it
    /// is compiled ahead and snapshot to a `Tmp` like any other value
    /// ([`Self::snapshot_before_sibling_rebind`]). The verifier backs the
    /// rule: [`threaded_ir::verify_sibling_reads`] reports a child still
    /// read in place while a later sibling's prelude rebinds it
    /// (`LocalReadAfterSiblingRebind`).
    pub(super) fn sequence_children(
        &mut self,
        children: &[&Expression],
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, PrecompiledScope)> {
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let mut scope = PrecompiledScope::new();
        // BT-4722 / BT-5339: a block literal whose capture reads live state, or
        // a `late` field read that may raise, races with a sibling that runs
        // code (`last_racing_operand`), so the hoisting point extends to it;
        // the sibling is bound to a temp ahead of it.
        let Some(k) = children
            .iter()
            .rposition(|child| self.subexpr_needs_prelude(child))
            .max(self.last_racing_in_place_child(children))
        else {
            return Ok((prelude, scope));
        };

        // `None`: left for the parent to compile in place (reading the
        // named local, if a plain variable); `Some(i)`: `compiled[i]`.
        let mut order: Vec<(Option<usize>, Option<&str>, Span)> = Vec::with_capacity(k + 1);
        let mut compiled: Vec<(&Expression, bool, bool, ThreadedValue)> = Vec::with_capacity(k + 1);
        let mut source_index: Vec<usize> = Vec::with_capacity(k + 1);
        for (i, child) in children[..=k].iter().enumerate() {
            let inner = child.unwrap_parens();
            let snapshot = self.snapshot_before_sibling_rebind(inner, &children[i + 1..=k]);
            if Self::is_trivial_subexpr(inner) && !snapshot {
                let reads = match inner {
                    Expression::Identifier(id) => Some(id.name.as_str()),
                    _ => None,
                };
                order.push((None, reads, inner.span()));
                continue;
            }
            let is_producer = self.is_prelude_producer(inner);
            let tv = self.threaded_expression(child, frame)?;
            order.push((Some(compiled.len()), None, inner.span()));
            compiled.push((child, is_producer, snapshot, tv));
            source_index.push(i);
        }

        let mut ranges: Vec<std::ops::Range<usize>> = Vec::with_capacity(compiled.len());
        for (i, (child, is_producer, snapshot, tv)) in compiled.into_iter().enumerate() {
            let span = child.unwrap_parens().span();
            let must_bind = source_index[i] < k && (snapshot || !tv.value_is_trivial());
            let start = prelude.len();
            prelude.extend(tv.prelude);
            ranges.push(start..prelude.len());
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

        let siblings: Vec<super::threaded_ir::SequencedSibling<'_>> = order
            .iter()
            .map(|(idx, reads, span)| super::threaded_ir::SequencedSibling {
                reads_in_place: *reads,
                prelude: idx.map_or(&[][..], |i| &prelude[ranges[i].clone()]),
                span: *span,
            })
            .collect();
        let errors = super::threaded_ir::verify_sibling_reads(&siblings);
        let at = order
            .first()
            .map_or_else(Span::default, |(_, _, span)| *span);
        self.report_threaded_ir_verify_errors(
            &errors,
            "sequenced sibling reads a local a later sibling rebinds",
            at,
        );
        Ok((prelude, scope))
    }

    /// ADR 0131 §1a: whether the plain-variable child `child` must be
    /// snapshot to a `Tmp` before its later siblings' preludes run — true
    /// when it names an outer local that some later sibling needing a
    /// prelude threads through any construct reachable inside it
    /// ([`Self::reachable_threaded_locals`]: the sibling itself, or a
    /// construct nested in its operands, as in `t + (self id: (c ifTrue: [t
    /// := t + 1. 1] ifFalse: [0]))`), so that construct's `LocalRebind` may
    /// change the binding before the parent reads it. Anything other than a
    /// plain variable is decided by the ordinary rule.
    fn snapshot_before_sibling_rebind(&self, child: &Expression, later: &[&Expression]) -> bool {
        let Expression::Identifier(id) = child else {
            return false;
        };
        if fault!(SkipSiblingSnapshot) {
            return false;
        }
        later.iter().any(|sibling| {
            self.subexpr_needs_prelude(sibling)
                && self
                    .reachable_threaded_locals(sibling, &[])
                    .iter()
                    .any(|n| *n == id.name.as_str())
        })
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
    ///
    /// ADR 0131 §4 (`StateEffectEscapesExpression`): a `LocalRebind` whose
    /// lowering steps a loop argument or the frame's `State` map belongs in
    /// that frame's statement list, where the loop's recursive call and exit
    /// follow the step; rendered here to a `Document` the step would be
    /// invisible to them and the write silently lost. Reported, never
    /// dropped.
    pub(super) fn threaded_prelude_doc(&mut self, prelude: &[ThreadedStmt]) -> Document<'static> {
        let (escaped, at) = super::threaded_ir::stepping_local_rebinds(prelude);
        self.report_threaded_ir_verify_errors(
            &escaped,
            "a loop-frame local rebind must be spliced into its frame, not rendered to a document",
            at,
        );
        let mut ctx = RenderCtx::new(self);
        render(prelude, &mut ctx)
    }

    /// BT-3811: [`Self::threaded_prelude_doc`] for a `Document`-returning
    /// consumer of a hybrid field rebind
    /// ([`Self::lower_hybrid_field_rebind`]). The statements are checked
    /// against `frame` (the caller's) before they are flattened to text,
    /// where no later verification can see them. The caller derives `frame`
    /// once and passes it to both the lowering and this check, so the check
    /// guards a later edit that lowers a nested piece under another frame;
    /// the full-walk `BindFrameMismatch` over the spliced `Vec<ThreadedStmt>`
    /// consumers is the independent check.
    pub(super) fn rendered_field_rebind_doc(
        &mut self,
        stmts: &[ThreadedStmt],
        frame: FrameId,
        span: Span,
    ) -> Document<'static> {
        let errors = super::threaded_ir::verify_bind_frames(stmts, frame);
        self.report_threaded_ir_verify_errors(
            &errors,
            "hybrid field rebind lowered in a different frame than its caller's",
            span,
        );
        self.threaded_prelude_doc(stmts)
    }

    /// ADR 0118 phase 1a: for a consumer whose existing lowering
    /// compiles `expr` itself through a specialised path
    /// (`generate_field_assignment_open`, `emit_actor_threaded_last_stmts`,
    /// …) rather than via one `expression_doc` call it could replace with
    /// [`Self::threaded_expression`]: if `expr` needs a prelude, compiles
    /// it once through `threaded_expression`, splices the prelude into
    /// `stmts`, and registers the value so that path's own compile of
    /// `expr` substitutes it; otherwise does nothing. The returned scope
    /// goes back to [`Self::finish_precompiled_scope`] once that compile
    /// is done. ADR 0118 phase 2b: the drop-in replacement for
    /// every `hoist_nested_self_sends(expr, HoistSink::Threaded {
    /// .. })` call — the sequencing rule (not an order-safety
    /// drop) is the only mechanism.
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

    /// ADR 0118 phase 5b: the self-contained-`Document` replacement for the
    /// deleted `closed_expression_doc`. Renders `expr`'s prelude and value
    /// back-to-back through the same [`render`]/[`render_value`] every
    /// spliced prelude goes through, so the bytes match a spliced prelude
    /// exactly. Use where the caller has no `Vec<ThreadedStmt>`/
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

    /// ADR 0118 phase 5b: the same self-contained rendering as
    /// [`Self::threaded_expression_doc`], for a caller that already holds a
    /// [`ThreadedValue`] (a producer's own return value) rather than an
    /// `Expression` to compile — used at every ambient
    /// (non-`threaded_expression`) re-entry point a producer has
    /// (`try_handle_class_method_self_send`, `try_handle_class_reference`):
    /// these are reached through ordinary `generate_expression`, which
    /// returns a bare `Document` with no prelude side-channel, so the prelude
    /// is always closed here rather than left open for a caller to propagate.
    ///
    /// Deliberately does NOT call `ThreadedValue::close` — it renders
    /// prelude-then-value unconditionally. `close(ctx, CloseContext::Opaque)`
    /// (and its `VerifyError::StateEffectEscapesExpression` backstop) has no
    /// production caller.
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
}
