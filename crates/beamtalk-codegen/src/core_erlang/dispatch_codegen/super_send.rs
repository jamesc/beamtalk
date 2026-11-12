// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Super sends.
//!
//! Lowers `super selector` to a parent-class dispatch call and answers whether a
//! super send threads state.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::method_frame::CLASS_SELF_VAR;
use crate::core_erlang::threaded_ir::{VerifyError, VersionPrefix};
use crate::core_erlang::{CodeGenContext, CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates code for a super message send.
    ///
    /// A method-root actor super call uses `beamtalk_dispatch:super_reply/5`
    /// (`super/5`, re-raising an error answer, BT-4592) to invoke the
    /// superclass implementation via hierarchy walking (ADR 0006).
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// super increment
    /// super getValue
    /// super at: 1 put: value
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// call 'beamtalk_dispatch':'super_reply'('increment', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super_reply'('getValue', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super_reply'('at:put:', [1, Value], Self, State, 'Counter')
    /// ```
    pub(in crate::core_erlang) fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Taken before any argument compiles, so a nested `super` send is
        // judged on its own.
        let from_producer = std::mem::take(&mut self.in_super_send_producer);
        let selector_atom = selector.name().to_string();

        // A class-side `super` resolves up the metaclass chain via the runtime
        // helper, keyed on the class the method belongs to (`class_var_home`):
        // the builder class inside a programmatic ClassBuilder class-method
        // fun (ADR 0084), else a class-side extension's target, else the
        // class being compiled (BT-4001). `class_super_dispatch/4` begins the
        // walk at that class's superclass and never consults the class itself
        // (not even its own class-side extensions, so an extension override
        // that sends `super` cannot recurse into itself); at each ancestor it
        // checks that ancestor's class-side extensions, then its class methods.
        // The live receiver (`ClassSelf`) is passed as is (BT-4301), so a
        // subclass receiver runs the superclass method under its own
        // class-variable key with its real reference. A class method binds no instance `Self`/`State`,
        // so it must never reach the `beamtalk_dispatch:super_reply/5` instance path
        // below.
        let class_side_super_class = (self.builder_class_method_class().is_some()
            || self.in_class_method())
        .then(|| self.class_var_home());
        if let Some(defining_class) = class_side_super_class {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_super_dispatch'(",
                leaf::atom(defining_class),
                ", ",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], ",
                leaf::var(CLASS_SELF_VAR),
                ")"
            ];
            // ADR 0130 §3: `super` passes nothing and rebinds nothing.
            let tv = self.class_self_send_value(&args_preamble, call_doc);
            return Ok(self.close_threaded_value_doc(tv));
        }

        let class_name = self.class_name();

        // value/primitive-context funs (`fun(Args, Self) -> Result`)
        // have no `State` binding, so `super` must not reference one. Route to
        // `super_value/4`, which walks the same chain and returns a plain value.
        // BT-4296: its arguments are sequenced (`thread_args`) so the argument
        // list evaluates them in source order.
        if self.context == CodeGenContext::ValueType {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let call_doc = docvec![
                "call 'beamtalk_dispatch':'super_value'(",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], Self, ",
                leaf::atom(class_name),
                ")",
            ];
            return Ok(self.close_prelude(&args_preamble, call_doc, "SuperRes"));
        }

        // ADR 0137 §3: inside an actor closure a `super` send runs the
        // superclass method against the cell (the actor's current state at
        // home), stores its answer back, and answers its value. A block or loop
        // body threads no `State` for a `super` send either (BT-3995): it runs
        // against the cell, which the enclosing call brackets
        // (`classify_super_send`), and answers its value. Sends there are not
        // sync-bracketed, so the arguments are sequenced (`thread_args`) to
        // evaluate in source order (BT-4296).
        if !self.super_send_threads_state() {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            // The prelude closes around this call, so a threaded-state rebind in
            // it would be scoped away from later statements (BT-3809): report
            // it, as `capture_argument_list_doc` does for its arguments.
            let escaped = crate::core_erlang::threaded_ir::opaque_escapes(&args_preamble, false);
            // BT-4591: the report's main span is the escaping argument (the one
            // holding the escaped bind), not merely the first argument.
            let at = escaped
                .iter()
                .find_map(|e| match e {
                    VerifyError::StateEffectEscapesExpression { at, .. } => arguments
                        .iter()
                        .find(|a| a.span().start() <= at.start() && at.end() <= a.span().end())
                        .map(Expression::span),
                    _ => None,
                })
                .or_else(|| arguments.first().map(Expression::span))
                .unwrap_or_default();
            self.report_threaded_ir_verify_errors(
                &escaped,
                "super-send argument state rebind trapped in a closed argument list",
                at,
            );
            let call_doc = self.cell_super_send_doc(&selector_atom, &class_name, args_doc);
            return Ok(self.close_prelude(&args_preamble, call_doc, "SuperRes"));
        }

        // BT-4318: this form answers `{reply, Result, NewState}`; only
        // `super_send_producer` unpacks it into a `State` `Bind`. Anywhere
        // else the tuple leaks as the value and the state change is lost.
        if !from_producer {
            let at = self.current_send_span.unwrap_or_default();
            self.report_threaded_ir_verify_errors(
                &[VerifyError::StateEffectEscapesExpression {
                    prefix: VersionPrefix::State,
                    at,
                }],
                "state-threading super send compiled outside its producer",
                at,
            );
        }

        // A method-body `super` send's arguments are sequenced ahead by
        // `super_send_producer` (`sequence_children`) and read back here.
        let args_doc = self.capture_argument_list_doc(arguments)?;

        // BT-4592: `super_reply/5`, not `super/5`, so an inherited method that
        // raises re-raises its error here instead of answering an `{error, E}`
        // 2-tuple that the `{reply, Result, NewState}` unpack fails on.
        let current_state = self.current_state_var();
        let doc = docvec![
            "call 'beamtalk_dispatch':'super_reply'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], Self, ",
            leaf::var(current_state),
            ", ",
            leaf::atom(class_name),
            ")",
        ];
        // ADR 0137: a `super` send that may reach the default arm is bracketed.
        // A `super` send in a closure runs against the cell (the early returns
        // above), so its change is kept and no guard is needed (BT-3985 retired).
        Ok(self.bracket_super_send(selector, doc, Span::default()))
    }

    /// Whether a `super` send compiled now threads the method's own `State`:
    /// `generate_super_send` answers the raw `{reply, Result, NewState}`
    /// tuple of `beamtalk_dispatch:super_reply/5`, which the caller must unpack
    /// (BT-4318) — through [`Self::threaded_expression`]'s `super` producer
    /// in any value position. False on the class side, in a value type, and
    /// wherever the send runs against the actor-state cell (a closure, a
    /// block or loop body), all of which answer a plain value.
    pub(in crate::core_erlang) fn super_send_threads_state(&self) -> bool {
        self.builder_class_method_class().is_none()
            && !self.in_class_method()
            && self.context != CodeGenContext::ValueType
            && !self.in_cell_closure()
            && !self.super_send_uses_cell()
    }
}
