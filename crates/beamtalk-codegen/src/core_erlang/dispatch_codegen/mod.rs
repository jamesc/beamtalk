// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles the **core domain operation** of Beamtalk: message
//! sending. In Smalltalk and Beamtalk, message sending is THE fundamental operation,
//! not method calls or function invocation.
//!
//! # Message Send Protocol (ADR 0007 Phase 4)
//!
//! Messages are dispatched through the following strategy:
//!
//! 1. **Compiler intrinsics**: Language-level constructs that the compiler must
//!    generate inline code for (binary operators, block evaluation, spawn/await,
//!    class/nil testing). These are structural requirements, not type-specific dispatch.
//!
//! 2. **Runtime dispatch**: All other messages go through the unified entry point
//!    `beamtalk_message_dispatch:send/3`, which routes to:
//!    - **Actors** (`beamtalk_object` records): Sync via `beamtalk_actor:sync_send/3` (ADR 0043)
//!    - **Class objects**: Sync via `beamtalk_object_class:class_send/3`
//!    - **Primitives** (everything else): Sync via `beamtalk_primitive:send/3`
//!
//! The primitive binding table from `lib/*.bt` (ADR 0007) drives stdlib method
//! compilation, while call-site dispatch uses runtime type checking — except
//! for receivers with a statically-known tagged-map (Value/Object) class
//! type, which compile to a guarded direct `Module:dispatch/3` call (BT-4069,
//! widened to open classes by BT-4293), and receivers statically typed as a
//! representation-routed built-in (`List`, `Dictionary`, `Integer`, …), which
//! compile to a direct call guarded by the representation test the dynamic
//! path routes on (BT-4294; both are the `typed_guarded` handler below).
//!
//! # Special Cases (Compiler Intrinsics)
//!
//! - **Binary operators**: `+`, `-`, `*`, `/` → Direct Erlang arithmetic
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Direct function calls
//! - **ProtoObject/Object**: `class`, `isNil`, `respondsTo:` → Pattern matching
//! - **Spawn messages**: `Class spawn`, `Class spawnWith: args` → `gen_server:start_link`
//! - **Await messages**: `future await` → Blocking future resolution
//! - **Super sends**: `super methodName:` → Parent class dispatch

//!
//! # Layout
//!
//! This file keeps the entry point that routes a send to its lowering
//! ([`HANDLERS`], `generate_message_send`), the runtime-dispatch default, and
//! the shared types. One sibling file per kind of send:
//!
//! - `typed_guarded`: statically-typed receivers, guarded direct calls
//! - `ffi`: Erlang interop
//! - `class_reference` / `class_send`: class-literal and class-method sends
//! - `spawn`: spawn and instantiation
//! - `self_dispatch`: self-dispatch and sealed dispatch
//! - `field_assignment`: field-assignment opens
//! - `super_send`: super sends

mod class_reference;
mod class_send;
mod ffi;
mod field_assignment;
mod self_dispatch;
mod spawn;
mod super_send;
mod typed_guarded;

pub(in crate::core_erlang) use spawn::SpawnTargetKind;
pub(in crate::core_erlang) use typed_guarded::{
    MAX_PROTOCOL_ARMS, StaysDynamic, TypedSendTarget, supervisor_family_root,
};

// Re-exported for the test modules, which name these through `dispatch_codegen`.
#[cfg(test)]
use class_reference::is_class_auto_export_selector;
#[cfg(test)]
pub(in crate::core_erlang) use spawn::NamedSpawn;
#[cfg(test)]
use spawn::is_supervisor_spawn_selector;
#[cfg(test)]
pub(in crate::core_erlang) use typed_guarded::{is_exception_family, is_representation_ancestor};

use crate::core_erlang::threaded_ir::{FrameId, ThreadedStmt};
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};

/// One `generate_message_send` dispatch strategy: `Some(doc)` claims the
/// send, `None` defers to the next entry in [`HANDLERS`].
type SendHandler = fn(
    &mut CoreErlangGenerator,
    &Expression,
    &MessageSelector,
    &[Expression],
) -> Result<Option<Document<'static>>>;

/// The arguments of a class-side send: the source expressions, threaded by
/// the lowering that consumes them, or (BT-4317) already-bound variable
/// references, when one argument binding feeds several lowerings spliced into
/// the arms of a runtime `case` (each arm would otherwise thread, and so
/// compile, the same expressions again).
#[derive(Clone, Copy)]
pub(in crate::core_erlang) enum ClassSendArgs<'a> {
    /// Source expressions, threaded at the use site.
    Exprs(&'a [Expression]),
    /// Variables or literals already bound ahead of the send.
    Bound(&'a [Document<'static>]),
}

impl ClassSendArgs<'_> {
    pub(super) fn len(self) -> usize {
        match self {
            Self::Exprs(exprs) => exprs.len(),
            Self::Bound(docs) => docs.len(),
        }
    }

    pub(super) fn is_empty(self) -> bool {
        self.len() == 0
    }
}

/// `generate_message_send`'s dispatch priority as data, replacing
/// what were 15 source-ordered `if let Some(doc) = self.try_*()?` calls (two
/// of them a duplicated `is_character_typed_receiver` check — see
/// [`CoreErlangGenerator::try_handle_character_typed_message`]'s doc, now
/// this table's single `character_typed` entry). Order is significant and
/// load-bearing: earlier entries intentionally preempt later, more generic
/// ones (e.g. `character_typed` before `protoobject`/`object`, `dict` before
/// `list`, `class_method_self_send` before `self_dispatch`) — see each
/// handler's own doc for why. `typed_guarded` is last deliberately: it only
/// converts sends that would otherwise be fully dynamic, so every earlier,
/// more specific handler keeps its sends. A send that no entry claims falls
/// through to `generate_message_send`'s `generate_runtime_dispatch` default.
/// Pinned by `typed_sealed::send_handlers_keep_priority_positions`.
pub(in crate::core_erlang) const HANDLERS: &[(&str, SendHandler)] = &[
    (
        "character_typed",
        CoreErlangGenerator::try_handle_character_typed_message,
    ),
    (
        "protoobject",
        CoreErlangGenerator::try_generate_protoobject_message,
    ),
    ("object", CoreErlangGenerator::try_generate_object_message),
    ("block", CoreErlangGenerator::try_generate_block_message),
    ("dict", CoreErlangGenerator::try_generate_dict_message),
    ("list", CoreErlangGenerator::try_generate_list_message),
    (
        "boolean_protocol",
        CoreErlangGenerator::try_generate_boolean_protocol,
    ),
    ("spawn_await", CoreErlangGenerator::try_handle_spawn_await),
    (
        "erlang_interop",
        CoreErlangGenerator::try_handle_erlang_interop,
    ),
    (
        "logger_intrinsic",
        CoreErlangGenerator::try_generate_logger_intrinsic,
    ),
    (
        "class_reference",
        CoreErlangGenerator::try_handle_class_reference,
    ),
    (
        "class_method_self_send",
        CoreErlangGenerator::try_handle_class_method_self_send,
    ),
    (
        "self_dispatch",
        CoreErlangGenerator::try_handle_self_dispatch,
    ),
    (
        "typed_guarded",
        CoreErlangGenerator::try_handle_typed_guarded_send,
    ),
];

impl CoreErlangGenerator {
    /// Rebinds `Self{N}` from an
    /// already-produced value `Document`: a construct's own returned trailing
    /// tuple slot, carrying the `self.field := ...` mutations threaded out of
    /// its inlined body. This is what makes the construct's final `Self` the
    /// method's new LIVE `Self` for every statement that follows — the direct
    /// counterpart of Actor's `let State1 = element(2, _CF10) in`.
    ///
    /// Two callers, one shape: BT-3484's Letrec loop (threaded through its
    /// recursive tail call) and BT-3486's `on:do:`/`ensure:` (threaded through
    /// the `try`'s result tuple).
    ///
    /// Verifies through the plain [`Self::check_simple_field_bind_invariant`]
    /// every other `Self{N}`/`State{N}` version step already uses.
    pub(in crate::core_erlang) fn rebind_value_self_from_doc(
        &mut self,
        value_doc: Document<'static>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Document<'static> {
        let source_version = self.self_version();
        self.next_self_var();
        let target_version = self.self_version();
        self.check_simple_field_bind_invariant(
            super::threaded_ir::VersionPrefix::SelfVt,
            source_version,
            target_version,
            "value-type Self rebind from a threading construct's result",
            span,
        );
        let bind = ThreadedStmt::Bind {
            target: super::threaded_ir::VersionedVar::new(
                super::threaded_ir::VersionPrefix::SelfVt,
                target_version,
                FrameId::ROOT,
            ),
            source: super::threaded_ir::VersionedVar::new(
                super::threaded_ir::VersionPrefix::SelfVt,
                source_version,
                FrameId::ROOT,
            ),
            op: super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(value_doc)),
            span,
        };
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
    }

    /// Generates code for a message send.
    ///
    /// This is the **main entry point** for message compilation. It dispatches
    /// to specialized handlers for different message patterns, and falls back
    /// to runtime dispatch via `beamtalk_message_dispatch:send/3`
    /// which handles actors, class objects, and primitives uniformly.
    ///
    /// # Message Dispatch Strategy (ADR 0007 Phase 4)
    ///
    /// 1. **Super sends** → `generate_super_send`
    /// 2. **Binary operators** → `generate_binary_op` (synchronous Erlang ops)
    /// 3. **`HANDLERS`** → priority-ordered table; see its doc for
    ///    the full breakdown (Character-typed dispatch, `ProtoObject`/Object
    ///    messages, Block/Dictionary/List messages, Boolean conditionals,
    ///    spawn/await, Erlang interop, Logger intrinsics, class references,
    ///    class-method and actor self-sends)
    /// 4. **Default** → Runtime dispatch (actor vs primitive check)
    pub(in crate::core_erlang) fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // ADR 0137: note the call for the bracket check (inert unless the flag is on).
        self.note_send(receiver, selector, arguments, false, receiver.span());
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // Compile-time type assertion: `expr asType: SomeClass` (ADR 0025 Phase 2b)
        // Erased at codegen — generates only the receiver expression (zero runtime cost)
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "asType:" && arguments.len() == 1 {
                return self.expression_doc(receiver);
            }
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            // Method lookup via `>>` operator (e.g., Counter >> #increment)
            // Support `>>` on any expression, not just class literals
            if super::operators::structural_binary(op)
                == Some(super::operators::StructuralBinary::MethodLookup)
            {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // Runtime fallback: evaluate receiver and call method/2
                return self.generate_runtime_method_lookup(receiver, arguments);
            }

            // BT-3985: a user-defined binary selector sent to `self` in an
            // actor instance method is a self-dispatch like any other (in a
            // closure it runs against the actor-state cell, ADR 0137); in a
            // class method it is a direct class-side self-send (a send
            // through the class's own gen_server would deadlock). Both take
            // the HANDLERS path
            // (`try_handle_self_dispatch` / `try_handle_class_method_self_send`)
            // instead of the operator lowering. The threaded positions already route it through
            // `generate_self_dispatch` via `is_dispatching_actor_self_send`.
            // BT-4005: an own-class reference (`C + x` inside `C`'s class
            // method) is the dispatching self too, and reaches
            // `try_handle_class_reference`'s direct class-side call.
            if !super::expr_shape::binary_send_dispatches_to_self(&self.shape_ctx(), receiver, op) {
                let doc = self.generate_binary_op(op, receiver, arguments)?;
                return Ok(doc);
            }
        }

        // dispatch priority for every remaining message shape is
        // `HANDLERS` — see its doc for why `character_typed` leads the list.
        for (_, handler) in HANDLERS {
            if let Some(doc) = handler(self, receiver, selector, arguments)? {
                return Ok(doc);
            }
        }

        // Unified dispatch via beamtalk_message_dispatch:send/3
        self.generate_runtime_dispatch(receiver, selector, arguments)
    }

    /// Generates a cast (fire-and-forget) message send.
    ///
    /// Called when the AST `MessageSend` node has `is_cast: true` (the `!` suffix).
    ///
    /// # Dispatch Strategy
    ///
    /// - **All sends, including `self someMethod!`** (BT-3998): Routes through `beamtalk_message_dispatch:cast/3`, which
    ///   extracts the actor PID and calls `beamtalk_actor:cast_send/3`. Non-actor
    ///   receivers are silently ignored.
    ///
    /// Cast sends always evaluate to `'ok'`.
    pub(in crate::core_erlang) fn generate_cast_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Every cast, including `self foo!` (BT-3998), routes through
        // beamtalk_message_dispatch:cast/3 to the receiver's mailbox. A
        // self-cast therefore lands after the current message (what `!`
        // means), instead of running now and dropping its State.
        self.generate_runtime_cast(receiver, selector, arguments)
    }

    /// Generates unified runtime cast via `beamtalk_message_dispatch:cast/3`.
    ///
    /// Fire-and-forget path: routes to the actor's message queue via
    /// `beamtalk_actor:cast_send/3`. Non-actor receivers are silently ignored.
    /// Always returns `'ok'`.
    fn generate_runtime_cast(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        // Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved when ANY
        // sub-expression has an open scope from a class method self-send.
        // capture_subexpr_sequence force-hoists every sub-expression in that
        // case; the fast path (no open scopes) leaves them inline.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let mut seq = self.sequence_call(&all_exprs, "Cast")?;
        let actual_receiver = seq.next();
        let args_doc = Self::join_docs_with_commas(seq.rest());

        let call_doc = docvec![
            "call 'beamtalk_message_dispatch':'cast'(",
            actual_receiver,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])",
        ];

        Ok(seq.close(self, call_doc, "CastRes"))
    }

    /// BT-4070: the `beamtalk_message_dispatch:send/3` call itself, over
    /// already-compiled operand documents — the one construction of the
    /// runtime-dispatch call shape, shared by
    /// [`Self::generate_runtime_dispatch`] (which sequences `&Expression`
    /// operands first) and the pure-conditional fallback arm (whose receiver
    /// is the precomputed `Cond` variable and whose closures compile where
    /// the arm is taken). The call is byte-identical however the operands
    /// were sourced; only the sourcing differs.
    ///
    /// BT-4366: the call carries the enclosing send's line itself — the
    /// pure-conditional fallback arm nests it inside a `case`.
    pub(in crate::core_erlang) fn runtime_send_call(
        &self,
        receiver_doc: Document<'static>,
        selector_atom: &str,
        arg_docs: Vec<Document<'static>>,
    ) -> Document<'static> {
        self.annotate_current_send_call(docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            receiver_doc,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            Self::join_docs_with_commas(arg_docs),
            "])"
        ])
    }

    /// Generates unified runtime dispatch via `beamtalk_message_dispatch:send/3`.
    ///
    /// This is the fallback path for messages that don't match any compiler intrinsic.
    /// Routes through the unified entry point which handles actors (sync via `gen_server:call`),
    /// class objects (sync), and primitives (sync). Returns a value directly — no Future
    /// wrapping (ADR 0043).
    pub(super) fn generate_runtime_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        // Emit the dynamic-site report entry (BT-4292): the receiver's static
        // type and the gate that kept the send dynamic.
        self.report_dynamic_site(receiver, selector);

        // Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        // BT-4612: no `^`-closure pre-store here, nor in the typed guarded
        // send (`generate_typed_guarded_send`), which takes a send to a
        // receiver of statically known class instead and falls back to this
        // same call. Outside every bracket window a send reaches either
        // dispatch only when `classify_send` answered `UserCodeCall::No` (a
        // `Yes` send there is a gap, and a self or `super` send dispatches
        // elsewhere). Each way to `No`, with a block literal holding a `^`
        // among the operands:
        //
        // - `SendUserCode::Never` (`p spawnWith: [^1]`): runs none of its
        //   operands, so there is nothing to store for (BT-4606). Reaches
        //   this dispatch (`never_spawn_with`).
        // - `is_no_user_code_send`: a sealed primitive the stdlib marked
        //   `no_user_code` runs no user code for any argument, so it runs
        //   none of its operands either: a block passed to it (even to a
        //   parameter annotated `Integer`, since annotations are not checked
        //   at run time) is never run, and `note_nlr_operands` records
        //   nothing for it, like a `Never` send (BT-4633). Its receiver's
        //   class is known, so it reaches the typed guarded send
        //   (`no_user_code_block_argument`). Both are
        //   `send_runs_no_operand`.
        // - `Inlined`, `LoopOrClosureCall`, `InlinedOrClosureCall` answered
        //   `No`: the construct is lowered in place by its own lowering
        //   (a conditional's fallback stores for itself,
        //   `boolean_fallback_send`), never through this dispatch.
        //
        // Checked, not argued: in test builds every such send that reaches
        // either dispatch is logged (`note_dispatch_nlr_literals`), and
        // `only_sends_that_run_no_operand_reach_the_dispatch_with_a_nlr_literal`
        // compiles the whole `nlr_closure_call_corpus` (every in-place
        // construct, the fallback shapes with a non-literal operand, and a
        // test-only parameterised `no_user_code` mark given a `^` block) and
        // fails on any logged send that may run an operand. In any build,
        // such a send would leave its `^` block an `UnbracketedNlrClosure`
        // finding.
        #[cfg(test)]
        self.note_dispatch_nlr_literals(receiver, selector, arguments);
        let mut seq = self.sequence_call(&all_exprs, "Disp")?;
        let actual_receiver = seq.next();

        // BT-4070: the call shape itself lives in `runtime_send_call`,
        // shared with the pure-conditional fallback arm — byte-identical
        // to the inline splice this replaces.
        let call_doc = self.runtime_send_call(actual_receiver, &selector_atom, seq.rest());

        Ok(seq.close(self, call_doc, "DispRes"))
    }
}

#[cfg(test)]
mod tests;
