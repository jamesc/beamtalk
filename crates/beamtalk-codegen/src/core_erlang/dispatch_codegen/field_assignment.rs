// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Field-assignment opens.
//!
//! Open-document lowerings for `self.field := value`, `fieldAt:put:` and
//! `clearField:`, which thread the actor or value state through the rest of the
//! enclosing statement sequence.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::control_flow::{Closure, FieldWriteSite};
use crate::core_erlang::threaded_ir::FrameId;
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::Expression;

impl CoreErlangGenerator {
    /// Generates the opening part of a field assignment with state threading.
    ///
    /// For `self.field := value`, generates:
    /// ```erlang
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'('field', _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression (generating the body
    /// that uses the new state).
    ///
    /// the plain `Actor`/`ValueType` tail (the `else` fallthrough
    /// below) is now a thin `Closure::Open` call into
    /// [`Self::lower_field_write`] — the single lowering core this,
    /// `expressions.rs`'s `generate_field_assignment` (`Closure::Closed`),
    /// and `control_flow::conditionals`'s `lower_field_assignment_bind` (the
    /// un-rendered-`Bind`-push consumption style) all now share. Before this
    /// issue this branch was hand-built and Actor-only — a `ValueType` field
    /// write reaching it (from inside a loop/conditional/block body) would
    /// have silently threaded through the actor `State`/`StateAcc` map, a
    /// variable that does not exist in a value-type method, instead of
    /// `Self` — the missing `ValueType` arm `lower_field_write`'s
    /// [`FieldWriteSite`] dispatch now supplies.
    pub(in crate::core_erlang) fn generate_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::Assignment { target, value, .. } = expr
            && let Expression::FieldAccess { field, .. } = target.as_ref()
        {
            // ADR 0130 §2: a class-variable write is an in-place `put`,
            // an expression like any other: bind its value, no rebinding.
            if self.in_class_method() {
                let val_var = self.fresh_temp_var("Val");
                let write_doc = self.generate_field_assignment(&field.name, value)?;
                return Ok((
                    docvec!["let ", leaf::var(val_var.clone()), " = ", write_doc, " in "],
                    val_var,
                ));
            }
            // Full-extract mode — rebind field param instead of maps:put.
            // When the field is in hybrid_mutated_fields, the field has been extracted
            // to a direct fun parameter. We rebind it to a fresh variable and update
            // the readonly params map so subsequent reads use the new variable.
            if self.loop_mode.in_hybrid_loop
                && self
                    .loop_mode
                    .hybrid_mutated_fields
                    .contains(field.name.as_str())
            {
                // BT-3811: the one hybrid-rebind lowering
                // (`lower_hybrid_field_rebind`), spliced as real
                // `ThreadedStmt`s in the caller's frame and rendered
                // here for this `Document`-returning API.
                let frame = self.current_frame();
                let span = expr.span();
                let mut stmts = Vec::new();
                let val_var =
                    self.lower_hybrid_field_rebind(&field.name, value, frame, span, &mut stmts)?;
                let doc = self.rendered_field_rebind_doc(&stmts, frame, span);
                return Ok((doc, val_var));
            }

            // `ValueType` gains the `Self`-threading arm it
            // lacked before this issue (see this function's own doc
            // comment) — `FieldWriteSite::for_context` is the same
            // dispatch `generate_field_assignment`'s (the `Closed`
            // sibling's) plain-write default uses.
            let site = FieldWriteSite::for_context(self.context);
            // `lower_field_write` returns the val var so callers
            // (e.g. cascade codegen) can reference the assigned value
            // after hoisting the binding.
            return self.lower_field_write(site, Closure::Open, &field.name, value, FrameId::ROOT);
        }
        Err(CodeGenError::Internal(
            "generate_field_assignment_open called on non-field-assignment expression".to_string(),
        ))
    }

    /// Generates the opening part of a `self fieldAt: name put: value` with state threading.
    ///
    /// Similar to `generate_field_assignment_open` but with a dynamic field name.
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'(_Name, _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(in crate::core_erlang) fn generate_self_field_at_put_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            // BT-4369: a dynamic-key field write records itself for the loop checkout.
            self.note_field_write("<dynamic>", expr.span());
            let name_var = self.fresh_var("Name");
            let val_var = self.fresh_temp_var("Val");
            let name_code = self.expression_doc(&arguments[0])?;
            // The state the put builds on. Every caller has sequenced the name
            // and value first (BT-4310, `thread_send_arguments`), so a value
            // that changes the state (a self-send, a nested field write, a
            // State-only fold) has already stepped it, and this is the state
            // after the value: its change survives the put. (Before BT-4310
            // this read the state from before the value and dropped it.)
            let current_state = self.current_state_var();
            let val_code = self.expression_doc(&arguments[1])?;
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                leaf::var(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                leaf::var(val_var.clone()),
                " = ",
                val_code,
                " in let ",
                leaf::var(new_state),
                " = call 'maps':'put'(",
                leaf::var(name_var),
                ", ",
                leaf::var(val_var.clone()),
                ", ",
                leaf::var(current_state),
                ") in ",
            ];

            return Ok((doc, val_var));
        }
        Err(CodeGenError::Internal(
            "generate_self_field_at_put_open called on non-fieldAt:put: expression".to_string(),
        ))
    }

    /// Generates the opening part of `self clearField: name` with state
    /// threading (ADR 0124 §1/§4f, B4) — the instance-`State` counterpart to
    /// [`Self::generate_self_field_at_put_open`], which this mirrors exactly
    /// except for the map op (`maps:remove` in place of `maps:put`) and arity
    /// (one argument, no value to thread). `clearField:` returns `Self`
    /// (`Object>>clearField: -> Self`, matching `fieldAt:put:`'s convention)
    /// rather than a written value, since a clear has none.
    ///
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let StateN = call 'maps':'remove'(_Name, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(in crate::core_erlang) fn generate_self_clear_field_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            // BT-4369: a dynamic-key field write records itself for the loop checkout.
            self.note_field_write("<dynamic>", expr.span());
            let name_var = self.fresh_var("Name");
            let name_code = self.expression_doc(&arguments[0])?;
            let current_state = self.current_state_var();
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                leaf::var(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                leaf::var(new_state),
                " = call 'maps':'remove'(",
                leaf::var(name_var),
                ", ",
                leaf::var(current_state),
                ") in ",
            ];

            return Ok((doc, "Self".to_string()));
        }
        Err(CodeGenError::Internal(
            "generate_self_clear_field_open called on non-clearField: expression".to_string(),
        ))
    }
}
