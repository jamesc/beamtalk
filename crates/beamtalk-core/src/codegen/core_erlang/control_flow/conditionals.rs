// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Boolean conditional compilation with field mutation state threading.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! When `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:` is used inside an
//! actor method and the block argument(s) contain field mutations
//! (`self.slot :=`), the compiler generates inline case expressions
//! that thread actor state correctly through both branches.
//!
//! Without this, mutations inside `ifTrue:` blocks are lost because
//! the block executes as a Tier 1 closure: the mutated state is bound
//! as a local let-variable inside the closure but never returned to
//! the enclosing actor method's `handle_call`.
//!
//! # Generated Pattern
//!
//! `flag ifTrue: [self.count := self.count + 1]` generates:
//!
//! ```erlang
//! let _Cond1 = flag in
//! case _Cond1 of
//!   <'true'> when 'true' ->
//!     let StateAcc = State0 in
//!     let _Val1 = call 'erlang':'+'(call 'maps':'get'('count', StateAcc), 1) in
//!     let StateAcc1 = call 'maps':'put'('count', _Val1, StateAcc) in
//!     {_Val1, StateAcc1}
//!   <'false'> when 'true' ->
//!     {'nil', State0}
//! end
//! ```
//!
//! The caller (method body generator) unpacks `{Result, NewState}` via
//! `element/2` to thread the new state to subsequent expressions.
//!
//! # State Naming
//!
//! Branch bodies use `StateAcc` / `StateAcc{N}` (loop-body naming) so
//! that inner state variable names do not conflict with the outer
//! `State{N}` chain managed by the method body generator.

use super::super::document::Document;
use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates inline code for `flag ifTrue: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{block_result, mutated_state}`
    /// - False branch: `{'nil', unchanged_state}`
    pub(in crate::codegen::core_erlang) fn generate_if_true_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let outer_state = self.current_state_var();

        let saved_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        self.set_state_version(0);
        self.in_loop_body = true;

        let (branch_doc, _) = self.generate_conditional_branch_inline(block)?;

        self.in_loop_body = saved_in_loop;
        self.set_state_version(saved_version);

        Ok(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_doc,
            " in case ",
            Document::String(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            Document::String(outer_state.clone()),
            " in ",
            branch_doc,
            " <'false'> when 'true' -> {'nil', ",
            Document::String(outer_state),
            "} end",
        ])
    }

    /// Generates inline code for `flag ifFalse: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{'nil', unchanged_state}`
    /// - False branch: `{block_result, mutated_state}`
    pub(in crate::codegen::core_erlang) fn generate_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let outer_state = self.current_state_var();

        let saved_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        self.set_state_version(0);
        self.in_loop_body = true;

        let (branch_doc, _) = self.generate_conditional_branch_inline(block)?;

        self.in_loop_body = saved_in_loop;
        self.set_state_version(saved_version);

        Ok(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_doc,
            " in case ",
            Document::String(cond_var),
            " of <'true'> when 'true' -> {'nil', ",
            Document::String(outer_state.clone()),
            "} <'false'> when 'true' -> let StateAcc = ",
            Document::String(outer_state),
            " in ",
            branch_doc,
            " end",
        ])
    }

    /// Generates inline code for `flag ifTrue: [t_block] ifFalse: [f_block]` in actor context
    /// when at least one block contains field mutations.
    ///
    /// Returns `{Result, NewState}` from whichever branch is taken.
    pub(in crate::codegen::core_erlang) fn generate_if_true_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
    ) -> Result<Document<'static>> {
        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let outer_state = self.current_state_var();

        // True branch
        let saved_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        self.set_state_version(0);
        self.in_loop_body = true;
        let (true_branch_doc, _) = self.generate_conditional_branch_inline(true_block)?;
        self.in_loop_body = saved_in_loop;
        self.set_state_version(saved_version);

        // False branch (reset to same initial state)
        self.set_state_version(0);
        self.in_loop_body = true;
        let (false_branch_doc, _) = self.generate_conditional_branch_inline(false_block)?;
        self.in_loop_body = saved_in_loop;
        self.set_state_version(saved_version);

        Ok(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_doc,
            " in case ",
            Document::String(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            Document::String(outer_state.clone()),
            " in ",
            true_branch_doc,
            " <'false'> when 'true' -> let StateAcc = ",
            Document::String(outer_state),
            " in ",
            false_branch_doc,
            " end",
        ])
    }

    /// Generates an inline block body for a conditional branch with field mutation threading.
    ///
    /// **Precondition**: Caller must set `in_loop_body = true` and `state_version = 0`
    /// before calling, and restore them afterwards. The initial state inside the branch
    /// is named `StateAcc` (bound to the outer state by the caller via
    /// `let StateAcc = State{N} in ...`).
    ///
    /// Returns `(body_doc, final_state_version)`. The generated code ends with
    /// `{<result>, <final_state>}`.
    pub(in crate::codegen::core_erlang) fn generate_conditional_branch_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        self.push_scope();

        let body: Vec<&Expression> = block
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();

        // Empty block returns nil with unchanged state
        if body.is_empty() {
            let final_state = self.current_state_var();
            let version = self.state_version();
            self.pop_scope();
            return Ok((
                docvec!["{'nil', ", Document::String(final_state), "}"],
                version,
            ));
        }

        let mut last_result: Option<String> = None;

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;

            if Self::is_field_assignment(expr) {
                // generate_field_assignment_open emits open let chain:
                // "let _Val = <value> in let StateAccN = maps:put(...) in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                if is_last {
                    // BT-884: last_open_scope_result holds the assigned value variable
                    last_result.clone_from(&self.last_open_scope_result);
                }
                // If not last, the let chain stays open for the next expression
            } else if is_last && self.control_flow_has_mutations(expr) {
                // Last expression is nested control flow with mutations.
                // It returns {Result, State} — unpack both.
                let tuple_var = self.fresh_temp_var("Tuple");
                let result_var = self.fresh_temp_var("BranchResult");
                let expr_doc = self.expression_doc(expr)?;
                let next_version = self.state_version() + 1;
                self.set_state_version(next_version);
                let next_state = self.current_state_var();
                docs.push(docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    Document::String(result_var.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(tuple_var.clone()),
                    ") in let ",
                    Document::String(next_state),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ]);
                last_result = Some(result_var);
            } else if !is_last && self.control_flow_has_mutations(expr) {
                // Non-last expression is nested control flow with mutations.
                // Unpack {Result, State} and thread the state forward.
                let tuple_var = self.fresh_temp_var("Tuple");
                let expr_doc = self.expression_doc(expr)?;
                let next_version = self.state_version() + 1;
                self.set_state_version(next_version);
                let next_state = self.current_state_var();
                docs.push(docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    Document::String(next_state),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ]);
            } else if is_last {
                // Last non-assignment: bind to result variable
                let result_var = self.fresh_temp_var("BranchResult");
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec![
                    "let ",
                    Document::String(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in ",
                ]);
                last_result = Some(result_var);
            } else {
                // Non-last, non-assignment: discard result
                let seq_var = self.fresh_temp_var("seq");
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec![
                    "let ",
                    Document::String(seq_var),
                    " = ",
                    expr_doc,
                    " in ",
                ]);
            }
        }

        let final_state = self.current_state_var();
        let final_version = self.state_version();
        let result = last_result.unwrap_or_else(|| "'nil'".to_string());

        // Close with {result, final_state} tuple
        docs.push(docvec![
            "{",
            Document::String(result),
            ", ",
            Document::String(final_state),
            "}",
        ]);

        self.pop_scope();
        Ok((Document::Vec(docs), final_version))
    }
}
