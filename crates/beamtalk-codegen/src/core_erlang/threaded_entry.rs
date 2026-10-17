// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0132 Phase 1c (BT-3871): the threaded entry of a class method.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A class method with a *threading parameter* (`MethodInfo::threading_params`,
//! computed once by `beamtalk-core`'s `ThreadingIndex` and read here from the
//! class hierarchy, never re-derived) is compiled as **one body with two entry
//! points**:
//!
//! ```erlang
//! '__t2_class_twice:'/3 = fun (ClassSelf, ABlock, Acc@0) -> {Result, Acc@N}
//! 'class_twice:'/2 = fun (ClassSelf, ABlock) ->
//!     call 'erlang':'element'(1, apply '__t2_class_twice:'/3 (ClassSelf, ABlock, ~{}~))
//! ```
//!
//! The body is a `ThreadedStmt::MethodBody` frame over the trailing `Acc`
//! family; every use of a threading parameter is a *producer* in the sense of
//! ADR 0118/0131 (a prelude of `ThreadedStmt`s and a value), reached through the
//! same `threaded_expression` that sequences every other producer, so its
//! position in a statement, an assignment right-hand side, a `^` value or an
//! operand needs no machinery of its own:
//!
//! - a direct invocation (`b value: x`) is
//!   `call 'beamtalk_dispatch':'call_threaded'(B, [X], Acc@k)`, destructured into
//!   the value and `Acc@k+1`;
//! - the callable argument of a collection HOM (`items do: b`) is the ADR 0128
//!   opaque-callable fold with `Acc` as its trailing accumulator and
//!   `call_threaded/3` as its per-element gate (`FoldCarrier::Acc`);
//! - a bare-argument forward to a callee of this class that threads that
//!   position and is bound statically (the rule `generate_class_method_self_send`
//!   applies to every class-side self-send) calls the callee's threaded entry
//!   directly with the current `Acc`. Any other forward — a late-bound `self`
//!   send in an open class, a send to another class of the unit — is the plain
//!   send of today with `Acc` unchanged (Phase 2 resolves those at run time).
//!
//! `Acc` is a family of a *construct* only when that construct holds a use (see
//! [`CoreErlangGenerator::is_family_mutation`]), so a loop, conditional or
//! `on:do:` that does not invoke a threading parameter is lowered exactly as in
//! any class method and never carries `Acc`. ADR 0132 §1's predicate admits a
//! use only at the method root (Phase 1), so no construct ever holds one; a use
//! that nevertheless reaches a construct is refused loudly
//! ([`CoreErlangGenerator::reject_unthreadable_acc_family`], and the
//! `generate_identifier` guard) rather than dropped.

use super::class_meta::Tier2Entry;
use super::generator::CoreErlangGenerator;
use super::selector_mangler::threaded_class_fn_name;
use super::spec_codegen;
use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VerifyError, VersionCounter,
    VersionPrefix, VersionedVar,
};
use super::{CodeGenError, Result};
use beamtalk_cerl_doc::{Document, docvec, join, leaf};
use beamtalk_core::ast::{
    ClassDefinition, Expression, MessageSelector, MethodDefinition, MethodKind,
};
use beamtalk_core::semantic_analysis::block_facts::{normalize_cascade, sequenced_operand_sites};
use beamtalk_core::semantic_analysis::tier2_callee::{
    ThreadingUse, UseShape, classify_threading_use, forward_target_of,
};
use beamtalk_core::source_analysis::Span;
use ecow::EcoString;
use std::cell::RefCell;
use std::collections::HashSet;

/// Test-only override of `MethodInfo::threading_params` by selector.
#[cfg(test)]
pub(in crate::core_erlang) mod forced_positions {
    use std::cell::RefCell;
    use std::collections::HashMap;

    thread_local! {
        static FORCED: RefCell<HashMap<String, Vec<usize>>> = RefCell::new(HashMap::new());
    }

    pub(in crate::core_erlang) fn get(selector: &str) -> Option<Vec<usize>> {
        FORCED.with(|f| f.borrow().get(selector).cloned())
    }

    /// Runs `body` with `forced` (selector, positions) in effect on this thread.
    pub(in crate::core_erlang) fn with<T>(
        forced: &[(&str, &[usize])],
        body: impl FnOnce() -> T,
    ) -> T {
        struct Reset;
        impl Drop for Reset {
            fn drop(&mut self) {
                FORCED.with(|f| f.borrow_mut().clear());
            }
        }
        FORCED.with(|f| {
            *f.borrow_mut() = forced
                .iter()
                .map(|(s, p)| ((*s).to_string(), p.to_vec()))
                .collect();
        });
        let _reset = Reset;
        body()
    }
}

/// The per-body state of the threaded entry being lowered. Empty outside one;
/// scoped by [`CoreErlangGenerator::begin_threaded_class_body`] /
/// [`CoreErlangGenerator::end_threaded_class_body`].
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
    /// `root_uses` when the body is complete (checked by
    /// [`CoreErlangGenerator::finish_threaded_body_stmts`]), so a root use that
    /// no producer lowered is an error, never a dropped `Acc` step.
    lowered_uses: HashSet<Span>,
}

impl CoreErlangGenerator {
    /// The threading positions (0-based) of `class_name`'s class-side
    /// `selector`: `MethodInfo::threading_params`, the one answer the entry
    /// function, the plain wrapper, the metadata and the spec are all emitted
    /// from. Empty when the class is unknown or the method has none.
    pub(in crate::core_erlang) fn class_method_threading_positions(
        &self,
        class_name: &str,
        selector: &str,
    ) -> Vec<usize> {
        // Test hook: force a method's positions, bypassing the predicate, to
        // drive the body lowering over shapes the predicate never admits (the
        // negative corpus of `tests/threaded_class_entry.rs`).
        #[cfg(test)]
        if let Some(forced) = forced_positions::get(selector) {
            return forced;
        }
        self.class_hierarchy
            .as_ref()
            .and_then(|hierarchy| hierarchy.get_class(class_name))
            .and_then(|class| class.class_methods.iter().find(|m| m.selector == selector))
            .map(|method| method.threading_params.clone())
            .unwrap_or_default()
    }

    /// The threading positions of `method` of `class`, or empty when it is not
    /// a `Primary` method that gets an entry. The one place the "this class
    /// method has a threaded entry" decision is taken.
    pub(in crate::core_erlang) fn threaded_entry_positions(
        &self,
        class: &ClassDefinition,
        method: &MethodDefinition,
    ) -> Vec<usize> {
        if method.kind != MethodKind::Primary {
            return Vec::new();
        }
        self.class_method_threading_positions(&class.name.name, &method.selector.name())
            .into_iter()
            .filter(|position| *position < method.parameters.len())
            .collect()
    }

    /// `class`'s class-side threaded entries as they are advertised in
    /// `'tier2_class_methods'`, from the same [`Self::threaded_entry_positions`]
    /// answer the entries are emitted from.
    pub(in crate::core_erlang) fn collect_own_tier2_class_methods(
        &self,
        class: &ClassDefinition,
    ) -> Vec<Tier2Entry> {
        class
            .class_methods
            .iter()
            .filter_map(|method| {
                let positions = self.threaded_entry_positions(class, method);
                if positions.is_empty() {
                    return None;
                }
                let selector = method.selector.name().to_string();
                Some(Tier2Entry {
                    entry_fn: threaded_class_fn_name(&selector),
                    selector,
                    positions,
                })
            })
            .collect()
    }

    /// All `-spec` attributes of `class`: the plain methods' specs
    /// ([`spec_codegen::generate_class_specs`]) and, beside them, the spec of
    /// each class-side threaded entry. The one call every module kind builds its
    /// specs through, so an exported entry always has its spec.
    pub(in crate::core_erlang) fn class_spec_attrs(
        &self,
        class: &ClassDefinition,
        is_value_type: bool,
        referenced: &RefCell<HashSet<EcoString>>,
    ) -> Vec<Document<'static>> {
        let mut specs = spec_codegen::generate_class_specs(
            class,
            is_value_type,
            Some(&self.alias_registry),
            Some(referenced),
        );
        specs.extend(spec_codegen::generate_threaded_class_specs(
            class,
            &|method| !self.threaded_entry_positions(class, method).is_empty(),
            Some(&self.alias_registry),
            Some(referenced),
        ));
        specs
    }

    /// Records the module's class names and its own class-side entries (called
    /// once per module by the driver, with the hierarchy installed).
    pub(in crate::core_erlang) fn set_threaded_entries(
        &mut self,
        module: &beamtalk_core::ast::Module,
    ) {
        self.unit_class_names = module
            .classes
            .iter()
            .map(|c| c.name.name.to_string())
            .collect();
        self.own_tier2_class_methods = module
            .classes
            .first()
            .map(|class| self.collect_own_tier2_class_methods(class))
            .unwrap_or_default();
    }

    /// Enters the threaded body of `method`: its threading `positions` become
    /// the body's threading parameters and `Acc@0` the current `Acc` version.
    /// Pair with [`Self::end_threaded_class_body`] (the [`MethodFrame`] guard
    /// does it on every exit path).
    ///
    /// [`MethodFrame`]: super::method_frame::MethodFrame
    pub(in crate::core_erlang) fn begin_threaded_class_body(
        &mut self,
        method: &MethodDefinition,
        positions: &[usize],
    ) {
        self.in_threaded_body = true;
        self.threaded_body = ThreadedBodyState {
            params: positions
                .iter()
                .filter_map(|i| method.parameters.get(*i))
                .map(|p| p.name.name.to_string())
                .collect(),
            acc: VersionCounter::new(),
            plain_reads: 0,
            root_uses: HashSet::new(),
            lowered_uses: HashSet::new(),
        };
        let mut root_uses = HashSet::new();
        for statement in &method.body {
            self.collect_root_uses(&statement.expression, &mut root_uses);
            // A root `^` ends the body: nothing after it is lowered.
            if matches!(statement.expression, Expression::Return { .. }) {
                break;
            }
        }
        self.threaded_body.root_uses = root_uses;
    }

    /// The spans of the uses of a threading parameter at the method root of
    /// `expr`: `expr` itself, the value of a `:=` or `^`, and the operands the
    /// sequencing rule walks (`threaded_expression`), recursively. A use is
    /// not descended into (its arguments do not mention the parameter).
    fn collect_root_uses(&self, expr: &Expression, out: &mut HashSet<Span>) {
        let inner = expr.unwrap_parens();
        if self.threaded_use_at(inner).is_some() {
            out.insert(inner.span());
        } else if let Some(children) = Self::sequenced_send_children(inner) {
            for child in children {
                self.collect_root_uses(child, out);
            }
        } else if let Some(child) = Self::single_sequenced_child(inner) {
            self.collect_root_uses(child, out);
        } else if let Some(receiver) = Self::conditional_send_receiver(inner) {
            // `(b value) ifTrue: [...]`: the conditional's own lowering threads
            // its receiver ahead of its arms (`inline_control_flow_producer`).
            self.collect_root_uses(receiver, out);
        }
    }

    /// The uses at the method root the producer has not lowered yet — empty
    /// once the body is complete.
    pub(in crate::core_erlang) fn unlowered_root_uses(&self) -> Vec<Span> {
        let mut missing: Vec<Span> = self
            .threaded_body
            .root_uses
            .difference(&self.threaded_body.lowered_uses)
            .copied()
            .collect();
        missing.sort_by_key(|s| s.start());
        missing
    }

    /// Leaves the threaded body: the next method compiles with no `Acc`.
    pub(in crate::core_erlang) fn end_threaded_class_body(&mut self) {
        self.in_threaded_body = false;
        self.threaded_body = ThreadedBodyState::default();
    }

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

    /// Whether the method-body statement `stmt` is a use of a threading
    /// parameter or holds one at a sequenced operand site, so it must be
    /// compiled through `threaded_expression` with its prelude spliced into the
    /// body ([`Self::lower_root_statement_with_producer`]).
    pub(in crate::core_erlang) fn statement_has_threaded_use(&self, stmt: &Expression) -> bool {
        self.in_threaded_body
            && (self.expr_has_threaded_use(stmt)
                || sequenced_operand_sites(stmt)
                    .into_iter()
                    .any(|site| self.expr_has_threaded_use(site)))
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
        let value = match kind {
            ThreadingUse::Invocation => self.lower_threaded_invocation(expr, span)?,
            ThreadingUse::CallableHom => self.lower_threaded_callable_hom(expr, span)?,
            ThreadingUse::Forward { position } => {
                self.lower_threaded_forward(expr, position, span)?
            }
        };
        Ok(Some(value))
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
            // `valueWithArguments: xs` hands `call_threaded/3` the list itself.
            let args_doc = if selector.name() == "valueWithArguments:" {
                arg_refs.into_iter().next().unwrap_or(Document::Str("[]"))
            } else {
                docvec!["[", Self::join_docs_with_commas(arg_refs), "]"]
            };
            let call = docvec![
                "call 'beamtalk_dispatch':'call_threaded'(",
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

    /// `receiver selector: ... b ...` with `b` the bare argument at `position`.
    ///
    /// A callee of this class that threads that position and is bound
    /// statically (the rule every class-side self-send applies) is called
    /// through its threaded entry with the current `Acc`. Every other forward is
    /// today's plain send of the block, `Acc` unchanged.
    fn lower_threaded_forward(
        &mut self,
        expr: &Expression,
        position: usize,
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
                "threaded forward: not a send".to_string(),
            ));
        };
        let selector_name = selector.name().to_string();
        if self.forward_is_statically_threaded(receiver, &selector_name, position) {
            let mut prelude: Vec<ThreadedStmt> = Vec::new();
            let (arg_prelude, arg_refs) =
                self.with_plain_param_reads(|this| this.thread_args_bound(arguments, "Arg"))?;
            prelude.extend(arg_prelude);
            let mut call_args: Vec<Document<'static>> = vec![leaf::var("ClassSelf")];
            call_args.extend(arg_refs);
            call_args.push(leaf::var(self.current_acc_var_name()));
            let call = docvec![
                "call ",
                leaf::atom(self.module_name.to_string()),
                ":",
                leaf::atom(threaded_class_fn_name(&selector_name)),
                "(",
                join(call_args, &Document::Str(", ")),
                ")"
            ];
            let value = self.bind_result_and_step_acc(call, &mut prelude, span);
            return Ok(ThreadedValue { prelude, value });
        }
        // Late-bound (an open class's `self` send, another class of the unit):
        // the plain send, exactly as without a threaded entry. `Acc` passes
        // through unchanged; ADR 0132 Phase 2 resolves these at run time.
        let doc = self.with_plain_param_reads(|this| this.generate_expression(expr))?;
        Ok(ThreadedValue {
            prelude: Vec::new(),
            value: ValueRef::Doc(doc),
        })
    }

    /// Whether the forward `receiver selector: ... b ...` (`b` at `position`)
    /// is a statically bound send to a class method of this class whose
    /// threaded entry threads `position`.
    fn forward_is_statically_threaded(
        &self,
        receiver: &Expression,
        selector: &str,
        position: usize,
    ) -> bool {
        let own = self.class_name();
        let receiver_is_self = matches!(receiver, Expression::Identifier(id) if id.name == "self");
        let names_own_class = matches!(
            receiver,
            Expression::ClassReference { name, package: None, .. } if name.name == own
        );
        if !(receiver_is_self || names_own_class) {
            return false;
        }
        // The binding rule of `generate_class_method_self_send`: a selector this
        // class defines is a direct call when the class is sealed, the
        // selector is `class sealed`, or the receiver names the class.
        let defines = self.class_method_selectors().contains(selector);
        let bound = self.is_class_sealed()
            || !receiver_is_self
            || self.sealed_class_method_selectors().contains(selector);
        defines
            && bound
            && self
                .class_method_threading_positions(&own, selector)
                .contains(&position)
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

    /// ADR 0132 §2, the NLR row: the body's `^` (a non-local return from a block)
    /// carries the `Acc` version **in scope where the block is created**, and the
    /// body's catch answers `{Val, AccAtThrow}`. That is the caller's writes made
    /// before the `^` exactly when no use of a threading parameter runs between
    /// the block's creation and its invocation.
    ///
    /// A block that is consumed in the statement that creates it is safe: the
    /// statement's uses are sequenced ahead of the expression that holds the
    /// block (`threaded_expression`), so the block captures the version after
    /// them. A block *stored in a local* (`f := [... ^x]`) may be invoked after a
    /// later statement's use stepped `Acc`, and would answer the stale version —
    /// the caller's later writes lost. That shape is refused.
    ///
    /// (A block handed to a callee that itself stores it and calls it after a
    /// later use is the same hazard, and not detectable here; the callee would
    /// have to outlive the statement that passed it.)
    pub(in crate::core_erlang) fn reject_stale_nlr_capture(
        &self,
        method: &MethodDefinition,
    ) -> Result<()> {
        let mut stored: Option<Span> = None;
        for statement in &method.body {
            let expr = &statement.expression;
            if stored.is_some() && self.expr_mentions_threading_param(expr) {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "a block that does `^` is stored in a local before a later use of a \
                              threading block parameter (ADR 0132 §2): the stored block would \
                              answer the parameter's caller a stale state. Pass the block \
                              directly to the message that uses it"
                        .to_string(),
                    span: stored,
                });
            }
            if let Expression::Assignment {
                target,
                value,
                span,
                ..
            } = expr.unwrap_parens()
                && matches!(target.as_ref(), Expression::Identifier(_))
                && let Expression::Block(_) = value.unwrap_parens()
                && Self::expression_has_return(value)
            {
                stored = Some(*span);
            }
        }
        Ok(())
    }

    /// Whether `expr` mentions any threading parameter of the body.
    fn expr_mentions_threading_param(&self, expr: &Expression) -> bool {
        self.threaded_body.params.iter().any(|name| {
            beamtalk_core::semantic_analysis::block_facts::stored_block_var_uses(expr, name)
                != (false, false)
        })
    }

    fn expression_has_return(expr: &Expression) -> bool {
        let mut found = false;
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            found |= matches!(e, Expression::Return { .. });
        });
        found
    }

    /// `{Value, Acc}` as the threaded body's answer, replacing the value
    /// document `last` the body's last statement produced.
    pub(in crate::core_erlang) fn threaded_body_answer_doc(
        &mut self,
        last: Document<'static>,
    ) -> Document<'static> {
        let result = self.fresh_temp_var("ThAnswer");
        docvec![
            "let ",
            leaf::var(result.clone()),
            " = ",
            last,
            " in {",
            leaf::var(result),
            ", ",
            leaf::var(self.current_acc_var_name()),
            "}"
        ]
    }
}
