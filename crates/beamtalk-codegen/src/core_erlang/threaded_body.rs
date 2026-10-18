// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The dead remainder of ADR 0132's threaded body (the `Acc` producer).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0132 Phase 1c compiled a class method with a threading block parameter
//! to a second, threaded entry `'__t2_class_<sel>'` whose body threaded the
//! caller's `Acc` through every use of the parameter. ADR 0133 Phase 2
//! (BT-3904) removed those entries, their metadata, specs and exports, and the
//! threading ABI: nothing in Beamtalk called them. Nothing sets
//! `in_threaded_body` any more, so every function here answers "not a use"
//! and the producers are unreachable. They stay only because the `Acc` family
//! they lower (`VersionPrefix::Acc`, `ScopeKind::Threaded`, the opaque fold's
//! `FoldCarrier::Acc`) is still in the IR; BT-3905 deletes this module with it.

use super::generator::CoreErlangGenerator;
use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VerifyError, VersionCounter,
    VersionPrefix, VersionedVar,
};
use super::{CodeGenError, Result};
use beamtalk_cerl_doc::{Document, docvec, leaf};
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::normalize_cascade;
use beamtalk_core::semantic_analysis::tier2_callee::{
    ThreadingUse, UseShape, classify_threading_use, forward_target_of,
};
use beamtalk_core::source_analysis::Span;
use std::collections::HashSet;

/// The per-body state of a threaded body. Always empty since BT-3904 (see the
/// module docs).
#[derive(Debug, Default)]
pub(in crate::core_erlang) struct ThreadedBodyState {
    /// The Beamtalk names of the body's threading parameters.
    params: Vec<String>,
    /// The `Acc` version counter (`Acc@0` is the entry's own parameter).
    acc: VersionCounter,
    /// Non-zero while a plain (late-bound) forward legitimately reads a
    /// threading parameter as an ordinary value.
    plain_reads: u32,
    /// The spans of the uses at the method root (statement, `:=` value, `^`
    /// value, sequenced operand): the only nodes the producer may lower. A use
    /// anywhere else — inside a loop, handler, conditional arm or nested block —
    /// is refused, wherever the framework would lower it, because the step it
    /// would mint cannot be seen by the code after the construct.
    root_uses: HashSet<Span>,
    /// The spans of the root uses the producer has lowered so far; equal to
    /// `root_uses` when the body is complete, so a root use that no producer
    /// lowered is an error, never a dropped `Acc` step.
    lowered_uses: HashSet<Span>,
}

impl CoreErlangGenerator {
    /// The Core Erlang variable of the current `Acc` version (`Acc@0` is the
    /// entry's own parameter).
    pub(in crate::core_erlang) fn current_acc_var_name(&self) -> String {
        self.threaded_body.acc.current_var(VersionPrefix::Acc)
    }

    /// The current `Acc` version as an IR variable of the root frame.
    pub(in crate::core_erlang) fn current_acc_var(&self) -> VersionedVar {
        VersionedVar::new(
            VersionPrefix::Acc,
            self.threaded_body.acc.version(),
            FrameId::ROOT,
        )
    }

    /// Mints the next `Acc` version: `(source, target)` of the `Bind` that
    /// steps it.
    fn next_acc_step(&mut self) -> (VersionedVar, VersionedVar) {
        let source = self.current_acc_var();
        let mut counter = self.threaded_body.acc;
        counter.next_var(VersionPrefix::Acc);
        self.threaded_body.acc = counter;
        (source, self.current_acc_var())
    }

    /// Whether `expr` (paren-unwrapped here) is the bare identifier of a
    /// threading parameter of the body being lowered.
    pub(in crate::core_erlang) fn is_threading_param_read(&self, expr: &Expression) -> bool {
        self.in_threaded_body
            && matches!(
                expr.unwrap_parens(),
                Expression::Identifier(id) if self.threaded_body.params.iter().any(|p| *p == id.name)
            )
    }

    /// Whether `name` is a threading parameter of the body being lowered and no
    /// plain forward is reading it: the identifier then has no lowering of its
    /// own (`generate_identifier` refuses it).
    pub(in crate::core_erlang) fn is_unreadable_threading_param(&self, name: &str) -> bool {
        self.in_threaded_body
            && self.threaded_body.plain_reads == 0
            && self.threaded_body.params.iter().any(|p| p == name)
    }

    /// The value of a threading parameter's identifier as a Core Erlang
    /// variable: how a use hands the block on (the fold's callable, a static
    /// forward's argument).
    pub(in crate::core_erlang) fn threading_param_value_doc(
        &self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let Expression::Identifier(id) = expr.unwrap_parens() else {
            return Err(CodeGenError::Internal(
                "threading_param_value_doc: not an identifier".to_string(),
            ));
        };
        self.lookup_var(id.name.as_str())
            .map(|var| leaf::var(var.clone()))
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "threading parameter `{}` is not bound in the threaded body",
                    id.name
                ))
            })
    }

    /// Runs `f` with the threading parameters readable as plain values: a
    /// late-bound forward is today's plain send of the block.
    fn with_plain_param_reads<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.threaded_body.plain_reads += 1;
        let result = f(self);
        self.threaded_body.plain_reads -= 1;
        result
    }

    /// Whether a send to `receiver` resolves inside the unit, exactly as the
    /// threading index resolved it (`forward_target_of`).
    fn forward_resolves(&self, receiver: &Expression) -> bool {
        forward_target_of(receiver, &self.class_name().into(), true, &|c| {
            self.unit_class_names.contains(c)
        })
        .is_some()
    }

    /// The use of a threading parameter that the node `expr` itself is (never
    /// one in a child), with the index of the parameter in the body's list.
    /// Pure: the probe `subexpr_needs_prelude` and the producer share it.
    pub(in crate::core_erlang) fn threaded_use_at(
        &self,
        expr: &Expression,
    ) -> Option<(usize, ThreadingUse)> {
        if !self.in_threaded_body {
            return None;
        }
        self.threaded_body
            .params
            .iter()
            .enumerate()
            .find_map(|(i, name)| {
                match classify_threading_use(expr, name, &|r| self.forward_resolves(r)) {
                    UseShape::Use(kind) => Some((i, kind)),
                    UseShape::NotAUse | UseShape::Malformed => None,
                }
            })
    }

    /// Whether `expr` is a use or holds one at a node `threaded_expression`
    /// sequences (the spine of sends, `^`, `:=`, parentheses and literal
    /// containers).
    pub(in crate::core_erlang) fn expr_has_threaded_use(&self, expr: &Expression) -> bool {
        if !self.in_threaded_body {
            return false;
        }
        let inner = expr.unwrap_parens();
        if self.threaded_use_at(inner).is_some() {
            return true;
        }
        if let Some(children) = Self::sequenced_send_children(inner) {
            return children.iter().any(|c| self.expr_has_threaded_use(c));
        }
        if let Some(child) = Self::single_sequenced_child(inner) {
            return self.expr_has_threaded_use(child);
        }
        if let Some(receiver) = Self::conditional_send_receiver(inner) {
            return self.expr_has_threaded_use(receiver);
        }
        false
    }

    /// ADR 0132 §2: a use of a threading parameter as a producer of
    /// `threaded_expression`: its prelude steps `Acc` and its value is the
    /// result. `Ok(None)` when `expr` is not a use here.
    pub(in crate::core_erlang) fn threaded_use_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let Some((_, kind)) = self.threaded_use_at(expr) else {
            return Ok(None);
        };
        // The predicate admits a use at the method root only (ADR 0132 §1,
        // Phase 1); `Acc` is not carried through any other frame or scope. A
        // use that gets here anyway — in a loop or fold body, a conditional
        // arm, a handler, a nested block — is refused, never dropped; and a
        // root use is lowered exactly once.
        let span = expr.span();
        if frame != FrameId::ROOT || !self.threaded_body.root_uses.contains(&span) {
            return Err(Self::use_in_construct_error(span));
        }
        if !self.threaded_body.lowered_uses.insert(span) {
            return Err(CodeGenError::Internal(
                "a threading-parameter use was lowered twice (its Acc step would be minted twice)"
                    .to_string(),
            ));
        }
        // The use's operands first, by the one sequencing rule every covered
        // send follows: an operand that is itself a use of another threading
        // parameter (`a value: (b value)`) steps `Acc` here, in evaluation
        // order, and the lowering below reads its value back
        // (`precompiled_subexprs`) instead of compiling it in place, where its
        // step would be scoped away or refused as a use outside the root.
        let children = Self::use_children(expr);
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        let value = match kind {
            ThreadingUse::Invocation => self.lower_threaded_invocation(expr, span)?,
            ThreadingUse::CallableHom => self.lower_threaded_callable_hom(expr, span)?,
            ThreadingUse::Forward { .. } => self.lower_threaded_forward(expr)?,
        };
        self.finish_precompiled_scope(scope)?;
        prelude.extend(value.prelude);
        Ok(Some(ThreadedValue {
            prelude,
            value: value.value,
        }))
    }

    /// The operands of the use `expr` (paren-unwrapped) that the sequencing
    /// rule walks: a send's receiver and non-block arguments
    /// ([`Self::sequenced_send_children`]); a cascade's non-block arguments,
    /// message by message (its receiver is the parameter itself). The same
    /// list [`Self::collect_root_uses`] descends into and
    /// [`Self::threaded_use_producer`] sequences ahead of the use.
    fn use_children(expr: &Expression) -> Vec<&Expression> {
        match expr {
            Expression::MessageSend { .. } => {
                Self::sequenced_send_children(expr).unwrap_or_default()
            }
            Expression::Cascade {
                receiver, messages, ..
            } => normalize_cascade(receiver, messages)
                .1
                .into_iter()
                .flat_map(|(_, args)| args.iter())
                .filter(|arg| !matches!(arg, Expression::Block(_)))
                .collect(),
            _ => Vec::new(),
        }
    }

    /// A conditional send (`ifTrue:`, `and:`, `ifNil:`, ...) whose receiver
    /// holds a use of a threading parameter (`(b value: x) > 0 ifTrue: [...]`):
    /// the sequencing rule leaves a conditional's receiver to its own lowering,
    /// whose result tuple carries `State` (an actor) or nothing (a class) but
    /// never `Acc`, so the receiver's `Acc` step would be scoped away inside the
    /// conditional's document. The receiver is sequenced ahead instead, and the
    /// conditional re-enters `threaded_expression` with its value registered.
    pub(in crate::core_erlang) fn threaded_conditional_receiver_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        if !self.in_threaded_body {
            return Ok(None);
        }
        let Some(receiver) = Self::conditional_send_receiver(expr) else {
            return Ok(None);
        };
        if !self.expr_has_threaded_use(receiver) || self.precompiled_subexprs_contains(receiver) {
            return Ok(None);
        }
        let (mut prelude, scope) = self.sequence_children(&[receiver], frame)?;
        let tv = self.threaded_expression(expr, frame)?;
        self.finish_precompiled_scope(scope)?;
        prelude.extend(tv.prelude);
        Ok(Some(ThreadedValue {
            prelude,
            value: tv.value,
        }))
    }

    fn use_in_construct_error(span: Span) -> CodeGenError {
        CodeGenError::UnsupportedFeature {
            feature: "a threading block parameter used inside a loop, conditional, handler or \
                      nested block (ADR 0132 Phase 3): the method has no threaded entry for it"
                .to_string(),
            span: Some(span),
        }
    }

    /// `let T = <call> in let V = element(1, T) in` followed by the `Bind` of
    /// the next `Acc` version from `element(2, T)`; the value is `V`. `call` is
    /// anything answering `{Result, Acc1}`.
    fn bind_result_and_step_acc(
        &mut self,
        call: Document<'static>,
        prelude: &mut Vec<ThreadedStmt>,
        span: Span,
    ) -> ValueRef {
        let tuple = self.fresh_temp_var("Th");
        let result = self.fresh_temp_var("ThV");
        prelude.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(tuple.clone()),
                " = ",
                call,
                " in let ",
                leaf::var(result.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple.clone()),
                ") in "
            ],
            span,
        ));
        let (source, target) = self.next_acc_step();
        prelude.push(ThreadedStmt::Bind {
            target,
            source,
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(tuple),
                ")"
            ])),
            span,
        });
        ValueRef::Var(result)
    }

    /// `b value`, `b value: x`, `b valueWithArguments: xs`, or a cascade of
    /// those: one `call_threaded/3` per message, the cascade's value the last.
    fn lower_threaded_invocation(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Result<ThreadedValue> {
        let messages: Vec<(&MessageSelector, &[Expression])> = match expr {
            Expression::MessageSend {
                selector,
                arguments,
                ..
            } => vec![(selector, arguments.as_slice())],
            Expression::Cascade {
                receiver, messages, ..
            } => normalize_cascade(receiver, messages).1,
            _ => {
                return Err(CodeGenError::Internal(
                    "threaded invocation: not a send or cascade".to_string(),
                ));
            }
        };
        let block_doc = self.threading_param_for_use(expr)?;
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let mut value = ValueRef::Literal("'nil'");
        for (selector, arguments) in messages {
            let (arg_prelude, arg_refs) = self.thread_args_bound(arguments, "Arg")?;
            prelude.extend(arg_prelude);
            // `valueWithArguments: xs` hands the list itself to the gate that
            // keeps the plain lowering's behaviour for a non-list `xs` and a
            // non-fun callable (`call_threaded_with_arguments/3`).
            let (gate, args_doc) = if selector.name() == "valueWithArguments:" {
                (
                    "call_threaded_with_arguments",
                    arg_refs.into_iter().next().unwrap_or(Document::Str("[]")),
                )
            } else {
                (
                    "call_threaded",
                    docvec!["[", Self::join_docs_with_commas(arg_refs), "]"],
                )
            };
            let call = docvec![
                "call 'beamtalk_dispatch':",
                leaf::atom(gate),
                "(",
                block_doc.clone(),
                ", ",
                args_doc,
                ", ",
                leaf::var(self.current_acc_var_name()),
                ")"
            ];
            value = self.bind_result_and_step_acc(call, &mut prelude, span);
        }
        Ok(ThreadedValue { prelude, value })
    }

    /// The Core Erlang variable of the threading parameter `expr` is a use of.
    fn threading_param_for_use(&self, expr: &Expression) -> Result<Document<'static>> {
        let Some((index, _)) = self.threaded_use_at(expr) else {
            return Err(CodeGenError::Internal(
                "threading_param_for_use: not a use".to_string(),
            ));
        };
        let name = &self.threaded_body.params[index];
        self.lookup_var(name.as_str())
            .map(|var| leaf::var(var.clone()))
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "threading parameter `{name}` is not bound in the threaded body"
                ))
            })
    }

    /// `items do: b` and the other covered collection HOMs: the ADR 0128 fold
    /// over `Acc` (`FoldCarrier::Acc`), answering `{Result, NewAcc}`.
    fn lower_threaded_callable_hom(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Result<ThreadedValue> {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return Err(CodeGenError::Internal(
                "threaded collection HOM: not a send".to_string(),
            ));
        };
        let Some(tuple_doc) = self.try_generate_list_message(receiver, selector, arguments)? else {
            return Err(CodeGenError::Internal(format!(
                "threaded collection HOM `{}` has no list lowering",
                selector.name()
            )));
        };
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let value = self.bind_result_and_step_acc(tuple_doc, &mut prelude, span);
        Ok(ThreadedValue { prelude, value })
    }

    /// `receiver selector: ... b ...` with `b` a bare argument: the plain send
    /// of the block, `Acc` unchanged (no callee has a threaded entry since
    /// BT-3904).
    fn lower_threaded_forward(&mut self, expr: &Expression) -> Result<ThreadedValue> {
        let doc = self.with_plain_param_reads(|this| this.generate_expression(expr))?;
        Ok(ThreadedValue {
            prelude: Vec::new(),
            value: ValueRef::Doc(doc),
        })
    }

    /// ADR 0132 Phase 3 guard: a construct of a threaded body whose body holds a
    /// use of a threading parameter carries `Acc`, and the single-family
    /// consumers (`exception_family_slot`, the loop-family reader, the
    /// conditional gates) cannot carry it yet. `Ok` when `families` does not
    /// hold `Acc`; otherwise the construct is refused loudly.
    ///
    /// The refusal is a `VerifyError` reported through the shared
    /// `report_threaded_ir_verify_errors` path (a hard failure in debug builds,
    /// an `internal:` diagnostic in release), and the compile fails with the
    /// user-facing error either way: a dropped `Acc` is a silent wrong answer.
    pub(in crate::core_erlang) fn reject_unthreadable_acc_family(
        &mut self,
        families: &[VersionPrefix],
        construct: &str,
        span: Span,
    ) -> Result<()> {
        if families.contains(&VersionPrefix::Acc) {
            self.report_threaded_ir_verify_errors(
                &[VerifyError::ConstructFamilyDropped {
                    family: VersionPrefix::Acc,
                    carrier: construct.to_string(),
                    at: span,
                }],
                "a construct of a threaded body holds a use of a threading parameter its lowering cannot carry `Acc` through",
                span,
            );
            return Err(Self::use_in_construct_error(span));
        }
        Ok(())
    }
}
