// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! While loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for `whileTrue:` and `whileFalse:` loop constructs
//! with both pure and state-threading variants.

use std::fmt::Write;

use super::super::document::Document;
use super::super::intrinsics::validate_block_arity_exact;
use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_while_true(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileTrue:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x < 10] whileTrue: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_while_true_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_true_simple(condition, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_true_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Generate a recursive function:
        // letrec '_LoopN'/0 = fun () ->
        //     let _CondFun = <condition> in
        //     case apply _CondFun& () of
        //       'true' -> let _BodyFun = <body> in
        //                let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        //       'false' -> 'nil'
        //     end
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        let cond_var = self.fresh_temp_var("CondFun");
        let cond_code = self.expression_doc(condition)?;
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;

        let doc = docvec![
            format!("letrec '{loop_fn}'/0 = fun () -> "),
            format!("let {cond_var} = "),
            cond_code,
            " in ",
            format!("case apply {cond_var} () of "),
            "<'true'> when 'true' -> ",
            format!("let {body_var} = "),
            body_code,
            format!(" in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "),
            "<'false'> when 'true' -> 'nil' ",
            "end ",
            format!("in apply '{loop_fn}'/0 ()"),
        ];

        Ok(doc)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_true_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-478: Simplified loop signature — only (StateAcc), no separate field params.

        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // BT-598: Compute threaded locals for actor methods
        let threaded_locals = self.compute_threaded_locals_for_loop(body, Some(condition));

        let cond_var = self.fresh_temp_var("CondFun");

        // BT-598: Pack threaded locals into state before starting the loop
        let initial_state = self.current_state_var();
        let mut init_state_code = initial_state.clone();
        let mut pack_prefix = String::new();
        if !threaded_locals.is_empty() {
            let mut current = initial_state;
            for var_name in &threaded_locals {
                let packed_var = self.fresh_temp_var("Packed");
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    pack_prefix,
                    "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
                );
                current = packed_var;
            }
            init_state_code = current;
        }

        let mut docs: Vec<Document<'static>> = vec![
            Document::String(pack_prefix),
            docvec!["letrec 'while'/1 = fun (StateAcc) -> "],
        ];

        // BT-598: At the start of each loop iteration, read threaded locals from StateAcc.
        // Use push_scope so bindings don't leak to caller after the letrec.
        self.push_scope();
        for var_name in &threaded_locals {
            let core_var = Self::to_core_erlang_var(var_name);
            let key = Self::local_state_key(var_name);
            self.bind_var(var_name, &core_var);
            docs.push(Document::String(format!(
                "let {core_var} = call 'maps':'get'('{key}', StateAcc) in "
            )));
        }

        docs.push(docvec![format!("let {cond_var} = fun (StateAcc) -> "),]);

        // Save and override loop/body context while generating the condition
        let prev_in_loop_body = self.in_loop_body;
        let prev_state_version = self.state_version();
        self.in_loop_body = true;
        self.set_state_version(0);

        if let Expression::Block(cond_block) = condition {
            docs.push(self.generate_block_body(cond_block)?);
        } else {
            docs.push(self.generate_expression(condition)?);
        }

        self.in_loop_body = prev_in_loop_body;
        self.set_state_version(prev_state_version);

        docs.push(docvec![
            format!(" in case apply {cond_var} (StateAcc) of "),
            "<'true'> when 'true' -> ",
        ]);

        let (body_doc, final_state_version) = self.generate_while_body_with_threading(body, &[])?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        docs.push(docvec![
            format!(" apply 'while'/1 ({final_state_var}) "),
            "<'false'> when 'true' -> {'nil', StateAcc} ",
            "end ",
        ]);

        // Pop scope to restore original bindings (before the letrec)
        self.pop_scope();

        // Initial call with packed state
        docs.push(Document::String(format!(
            "in apply 'while'/1 ({init_state_code})"
        )));

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_while_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<(Document<'static>, usize)> {
        // Generate the body with state threading
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        // Save the previous loop-body context so nested loops don't corrupt it
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        // BT-478: Check if body has direct field assignments. If not, mutations
        // come from nested constructs and the last expression's result must be bound.
        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        let mut docs: Vec<Document<'static>> = Vec::new();

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                docs.push(Document::Str(" "));
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
            } else if self.is_actor_self_send(expr) {
                // BT-245: Self-sends may mutate state — thread state through dispatch
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
            } else if is_last && !has_direct_field_assignments {
                // BT-478/BT-483: Last expression with no direct field assignments in body.
                // Mutations come from nested constructs (e.g., inner to:do:).
                // Nested constructs return {Result, State} — extract State via element(2).
                let next_version = self.state_version() + 1;
                let next_var = format!("StateAcc{next_version}");
                let tuple_var = format!("_NestTuple{next_version}");
                let expr_code = self.expression_doc(expr)?;
                self.set_state_version(next_version);
                docs.push(docvec![
                    format!("let {tuple_var} = "),
                    expr_code,
                    format!(" in let {next_var} = call 'erlang':'element'(2, {tuple_var}) in"),
                ]);
            } else {
                let expr_code = self.expression_doc(expr)?;
                docs.push(docvec!["let _ = ", expr_code, " in"]);
            }
        }

        // Capture the final state version before restoring
        let final_state_version = self.state_version();

        // Restore state
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok((Document::Vec(docs), final_state_version))
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileFalse:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x > 0] whileFalse: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_while_false_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_false_simple(condition, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Generate: whileFalse is whileTrue with negated condition
        let loop_fn = self.fresh_temp_var("Loop");
        let cond_var = self.fresh_temp_var("CondFun");
        let cond_code = self.expression_doc(condition)?;
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;

        let doc = docvec![
            format!("letrec '{loop_fn}'/0 = fun () -> "),
            format!("let {cond_var} = "),
            cond_code,
            " in ",
            format!("case apply {cond_var} () of "),
            "<'false'> when 'true' -> ",
            format!("let {body_var} = "),
            body_code,
            format!(" in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "),
            "<'true'> when 'true' -> 'nil' ",
            "end ",
            format!("in apply '{loop_fn}'/0 ()"),
        ];

        Ok(doc)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-478: Simplified loop signature — only (StateAcc), no separate field params.

        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // BT-598: Compute threaded locals for actor methods
        let threaded_locals = self.compute_threaded_locals_for_loop(body, Some(condition));

        let cond_var = self.fresh_temp_var("CondFun");

        // BT-598: Pack threaded locals into state before starting the loop
        let initial_state = self.current_state_var();
        let mut init_state_code = initial_state.clone();
        let mut pack_prefix = String::new();
        if !threaded_locals.is_empty() {
            let mut current = initial_state;
            for var_name in &threaded_locals {
                let packed_var = self.fresh_temp_var("Packed");
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    pack_prefix,
                    "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
                );
                current = packed_var;
            }
            init_state_code = current;
        }

        let mut docs: Vec<Document<'static>> = vec![
            Document::String(pack_prefix),
            docvec!["letrec 'while'/1 = fun (StateAcc) -> "],
        ];

        // BT-598: At the start of each loop iteration, read threaded locals from StateAcc.
        self.push_scope();
        for var_name in &threaded_locals {
            let core_var = Self::to_core_erlang_var(var_name);
            let key = Self::local_state_key(var_name);
            self.bind_var(var_name, &core_var);
            docs.push(Document::String(format!(
                "let {core_var} = call 'maps':'get'('{key}', StateAcc) in "
            )));
        }

        docs.push(docvec![format!("let {cond_var} = fun (StateAcc) -> "),]);

        // Save and override loop/body context while generating the condition
        let prev_in_loop_body = self.in_loop_body;
        let prev_state_version = self.state_version();
        self.in_loop_body = true;
        self.set_state_version(0);

        if let Expression::Block(cond_block) = condition {
            docs.push(self.generate_block_body(cond_block)?);
        } else {
            docs.push(self.generate_expression(condition)?);
        }

        self.in_loop_body = prev_in_loop_body;
        self.set_state_version(prev_state_version);

        docs.push(docvec![
            format!(" in case apply {cond_var} (StateAcc) of "),
            "<'false'> when 'true' -> ",
        ]);

        // FALSE case: execute body and recurse
        let (body_doc, final_state_version) = self.generate_while_body_with_threading(body, &[])?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        docs.push(docvec![
            format!(" apply 'while'/1 ({final_state_var}) "),
            "<'true'> when 'true' -> {'nil', StateAcc} ",
            "end ",
        ]);

        // Pop scope to restore original bindings
        self.pop_scope();

        // Initial call with packed state
        docs.push(Document::String(format!(
            "in apply 'while'/1 ({init_state_code})"
        )));

        Ok(Document::Vec(docs))
    }
}
