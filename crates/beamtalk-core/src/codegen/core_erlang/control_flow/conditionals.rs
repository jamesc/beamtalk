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
use super::super::gen_server::BodyExprKind;
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
        // BT-1942: The receiver may be a class method self-send (or sub-expression
        // containing one) whose class var mutations are emitted as an open
        // let-chain. Split the open chain into a preamble (the let-chain) and
        // the value doc (the result variable). The preamble is emitted BEFORE
        // the case binding so ClassVarsN stays in scope inside the case.
        let (cond_chain, cond_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let (cond_preamble, cond_val_doc) = if let Some(result_var) = cond_open_scope {
            (cond_chain, Document::String(result_var))
        } else {
            (Document::Nil, cond_chain)
        };
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();

        let (branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;

        Ok(docvec![
            cond_preamble,
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_val_doc,
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
        // BT-1942: Split open let-chain from class method self-send sub-expressions.
        let (cond_chain, cond_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let (cond_preamble, cond_val_doc) = if let Some(result_var) = cond_open_scope {
            (cond_chain, Document::String(result_var))
        } else {
            (Document::Nil, cond_chain)
        };
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();

        let (branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;

        Ok(docvec![
            cond_preamble,
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_val_doc,
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
        // BT-1942: Split open let-chain from class method self-send sub-expressions.
        let (cond_chain, cond_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let (cond_preamble, cond_val_doc) = if let Some(result_var) = cond_open_scope {
            (cond_chain, Document::String(result_var))
        } else {
            (Document::Nil, cond_chain)
        };
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();

        // True branch
        let (true_branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(true_block))?;

        // False branch (reset to same initial state)
        let (false_branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(false_block))?;

        Ok(docvec![
            cond_preamble,
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_val_doc,
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

    /// Generates inline code for `obj ifNotNil: [block]` or `obj ifNotNil: [:v | block]`
    /// in actor context when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - Nil branch: `{'nil', unchanged_state}`
    /// - Non-nil branch: `{block_result, mutated_state}`
    ///
    /// If the block has a parameter (`:v`), it is bound to the receiver object value
    /// inside the branch body.
    pub(in crate::codegen::core_erlang) fn generate_if_not_nil_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        // BT-1942: Split open let-chain from class method self-send sub-expressions.
        let (recv_chain, recv_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let (recv_preamble, recv_val_doc) = if let Some(result_var) = recv_open_scope {
            (recv_chain, Document::String(result_var))
        } else {
            (Document::Nil, recv_chain)
        };
        let obj_var = self.fresh_temp_var("Obj");
        let outer_state = self.current_state_var();

        let (branch_doc, _) = self.with_branch_context(|this| {
            // Push a scope so the block-parameter binding is cleaned up after generation
            this.push_scope();
            if let Some(param) = block.parameters.first() {
                // Bind the block parameter to the receiver value (already bound to obj_var)
                this.bind_var(&param.name, &obj_var);
            }
            let result = this.generate_conditional_branch_inline(block);
            this.pop_scope();
            result
        })?;

        Ok(docvec![
            recv_preamble,
            "let ",
            Document::String(obj_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            Document::String(obj_var),
            " of <'nil'> when 'true' -> {'nil', ",
            Document::String(outer_state.clone()),
            "} <_> when 'true' -> let StateAcc = ",
            Document::String(outer_state),
            " in ",
            branch_doc,
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
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_conditional_branch_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        self.push_scope();

        let body = super::super::util::collect_body_exprs(&block.body);

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

        // Classify every expression upfront using the shared classifier (BT-1447).
        let plan: Vec<BodyExprKind> = body.iter().map(|e| self.classify_body_expr(e)).collect();

        let mut last_result: Option<String> = None;

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body.len() - 1;

            match kind {
                BodyExprKind::FieldAssignment => {
                    // generate_field_assignment_open emits open let chain:
                    // "let _Val = <value> in let StateAccN = maps:put(...) in "
                    let (doc, val_var) = self.generate_field_assignment_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        // BT-884: val_var holds the assigned value variable
                        last_result = Some(val_var);
                    }
                    // If not last, the let chain stays open for the next expression
                }
                BodyExprKind::LocalAssignPure
                | BodyExprKind::LocalAssignTier2
                | BodyExprKind::LocalAssignControlFlow
                | BodyExprKind::LocalAssignSelfSend => {
                    // BT-1053/BT-1225: Local variable mutation inside conditional branch.
                    // generate_local_var_assignment_in_loop emits open let chain:
                    // "let _Val = <value> in let StateAccN = maps:put('__local__key', _Val, StateAccM) in "
                    let (doc, val_var) = self.generate_local_var_assignment_in_loop(expr)?;
                    docs.push(doc);
                    if is_last {
                        // val_var is the _Val bound by generate_local_var_assignment_in_loop
                        last_result = Some(val_var);
                    } else {
                        // BT-1225: Bind the variable name to the temp var so subsequent reads
                        // within this block resolve directly to the temp var (e.g. `_Val1`)
                        // rather than falling through to maps:get without the __local__ prefix,
                        // which would cause a {badkey,VarName} crash at runtime.
                        //
                        // classify_body_expr guarantees expr is Assignment { target: Identifier }.
                        // If either invariant breaks, fail loudly rather than silently skipping bind_var.
                        let Expression::Assignment { target, .. } = expr else {
                            unreachable!("LocalAssign* kinds must ensure expr is an Assignment");
                        };
                        let Expression::Identifier(id) = target.as_ref() else {
                            unreachable!(
                                "LocalAssign* kinds must ensure assignment target is an Identifier"
                            );
                        };
                        self.bind_var(&id.name, &val_var);
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            docs.push(d);
                        }
                    }
                }
                // BT-1477: self.field := <control-flow-with-mutations> inside a conditional branch.
                // Unpack the {Value, State} tuple from the RHS, then apply maps:put for the field.
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let tuple_var = self.fresh_temp_var("CfTuple");
                            let val_var = self.fresh_temp_var("CfVal");
                            let rhs_doc = self.expression_doc(value)?;
                            let rhs_state = self.fresh_temp_var("CfState");
                            let new_state = self.next_state_var();
                            let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                                "let ",
                                Document::String(tuple_var.clone()),
                                " = ",
                                rhs_doc,
                                " in let ",
                                Document::String(val_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                Document::String(tuple_var.clone()),
                                ") in let ",
                                Document::String(rhs_state.clone()),
                                " = call 'erlang':'element'(2, ",
                                Document::String(tuple_var),
                                ") in let ",
                                Document::String(new_state.clone()),
                                " = call 'maps':'put'('",
                                Document::String(field.name.to_string()),
                                "', ",
                                Document::String(val_var.clone()),
                                ", ",
                                Document::String(rhs_state),
                                ") in ",
                            ]];
                            // Rebind threaded __local__ vars from the updated state
                            // so subsequent expressions in this branch see fresh values.
                            if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value)
                            {
                                for var in &threaded_vars {
                                    let tv_core = self.lookup_var(var).map_or_else(
                                        || Self::to_core_erlang_var(var),
                                        String::clone,
                                    );
                                    doc_parts.push(docvec![
                                        "let ",
                                        Document::String(tv_core.clone()),
                                        " = call 'maps':'get'('",
                                        Document::String(Self::local_state_key(var)),
                                        "', ",
                                        Document::String(new_state.clone()),
                                        ") in ",
                                    ]);
                                    self.bind_var(var, &tv_core);
                                }
                            }
                            docs.push(Document::Vec(doc_parts));
                            if is_last {
                                last_result = Some(val_var);
                            }
                        }
                    }
                }
                // BT-1479: self fieldAt: name put: expr — explicit handler for reflective field mutation
                BodyExprKind::SelfFieldAtPut => {
                    let (doc, val_var) = self.generate_self_field_at_put_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        last_result = Some(val_var);
                    }
                }
                // BT-1479: self fieldAt: name put: expr where RHS is control flow returning {Value, State}
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_temp_var("Name");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let val_var = self.fresh_temp_var("CfVal");
                        let val_code = self.expression_doc(&arguments[1])?;
                        let rhs_state = self.fresh_temp_var("CfState");
                        let new_state = self.next_state_var();
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            Document::String(name_var.clone()),
                            " = ",
                            name_code,
                            " in let ",
                            Document::String(tuple_var.clone()),
                            " = ",
                            val_code,
                            " in let ",
                            Document::String(val_var.clone()),
                            " = call 'erlang':'element'(1, ",
                            Document::String(tuple_var.clone()),
                            ") in let ",
                            Document::String(rhs_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            Document::String(tuple_var),
                            ") in let ",
                            Document::String(new_state.clone()),
                            " = call 'maps':'put'(",
                            Document::String(name_var),
                            ", ",
                            Document::String(val_var.clone()),
                            ", ",
                            Document::String(rhs_state),
                            ") in ",
                        ]];
                        // Rebind threaded __local__ vars from the updated state
                        // so subsequent expressions in this branch see fresh values.
                        if let Some(threaded_vars) =
                            self.get_control_flow_threaded_vars(&arguments[1])
                        {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    Document::String(tv_core.clone()),
                                    " = call 'maps':'get'('",
                                    Document::String(Self::local_state_key(var)),
                                    "', ",
                                    Document::String(new_state.clone()),
                                    ") in ",
                                ]);
                                self.bind_var(var, &tv_core);
                            }
                        }
                        docs.push(Document::Vec(doc_parts));
                        if is_last {
                            last_result = Some(val_var);
                        }
                    }
                }
                // BT-1479: {a, b} := expr where RHS is control flow returning {Value, State}
                BodyExprKind::DestructureAssignmentControlFlow => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let actual_val = self.fresh_temp_var("CfVal");
                        let value_str = self.expression_doc(value)?;
                        let next_state = self.next_state_var();
                        docs.push(docvec![
                            "let ",
                            Document::String(tuple_var.clone()),
                            " = ",
                            value_str,
                            " in let ",
                            Document::String(actual_val.clone()),
                            " = call 'erlang':'element'(1, ",
                            Document::String(tuple_var.clone()),
                            ") in let ",
                            Document::String(next_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            Document::String(tuple_var),
                            ") in ",
                        ]);
                        // Rebind threaded __local__ vars from the updated state
                        // so subsequent expressions in this branch see fresh values.
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value) {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                docs.push(docvec![
                                    "let ",
                                    Document::String(tv_core.clone()),
                                    " = call 'maps':'get'('",
                                    Document::String(Self::local_state_key(var)),
                                    "', ",
                                    Document::String(next_state.clone()),
                                    ") in ",
                                ]);
                                self.bind_var(var, &tv_core);
                            }
                        }
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            docs.push(d);
                        }
                    }
                }
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        // Last expression is nested control flow with mutations.
                        // It returns {Result, State} — unpack both.
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let result_var = self.fresh_temp_var("BranchResult");
                        let expr_doc = self.expression_doc(expr)?;
                        let next_state = self.next_state_var();
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
                    } else {
                        // Non-last expression is nested control flow with mutations.
                        // Unpack {Result, State} and thread the state forward.
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let expr_doc = self.expression_doc(expr)?;
                        let next_state = self.next_state_var();
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
                    }
                }
                // All other kinds (EarlyReturn, SuperSend, ErrorSend,
                // Tier2ValueCall, Tier2SelfSend, DispatchingSelfSend, Pure) are treated
                // as pure expressions in the conditional branch context.
                _ => {
                    if is_last {
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

#[cfg(test)]
mod tests {
    fn codegen(src: &str) -> String {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _) = crate::source_analysis::parse(tokens);
        crate::codegen::core_erlang::generate_module(
            &module,
            crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
        )
        .expect("codegen should succeed")
    }

    #[test]
    fn test_if_true_mutation_bypasses_runtime_dispatch() {
        // Actor ifTrue: with field mutation compiles to inline case (not runtime dispatch).
        // Also verifies the non-taken (false) branch returns {'nil', State}.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  inc: flag =>\n    flag ifTrue: [self.n := self.n + 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue: with field mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "True branch should update 'n' via maps:put. Got:\n{code}"
        );
        assert!(
            !code.contains("'beamtalk_message_dispatch':'send'"),
            "ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
        // Non-taken branch returns {'nil', unchanged_state} with StateAcc naming
        assert!(
            code.contains("{'nil',"),
            "Non-taken branch should return {{'nil', State}}. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "Branch bodies should use StateAcc naming. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_false_mutation_threads_state_in_false_arm() {
        // Actor ifFalse: with field mutation compiles to inline case with false branch mutating
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  dec: flag =>\n    flag ifFalse: [self.n := self.n - 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifFalse: with field mutation should generate inline case. Got:\n{code}"
        );
        // False branch maps:put happens in the '<false>' arm
        assert!(
            code.contains("maps':'put'('n'"),
            "False branch should update 'n' via maps:put. Got:\n{code}"
        );
        // True arm returns nil with unchanged state, as {'nil', State}
        assert!(
            code.contains("{'nil',"),
            "True arm of ifFalse: should return {{'nil', State}}. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_if_false_with_mutations_generates_two_threaded_branches() {
        // ifTrue:ifFalse: with mutations in both branches generates two threaded arms
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.n := 1] ifFalse: [self.n := 0].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue:ifFalse: with mutations should generate inline case. Got:\n{code}"
        );
        // maps:put should appear at least twice (once per branch)
        let put_count = code.matches("maps':'put'('n'").count();
        assert!(
            put_count >= 2,
            "Both branches should call maps:put for 'n'. Found {put_count}. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_true_block_reads_back_correctly() {
        // BT-1225: Local var assigned inside ifTrue: block must be readable in subsequent
        // expressions of the same block without a {badkey,VarName} runtime crash.
        // The write uses '__local__y' key; reads must resolve to the temp var, not 'y'.
        let src = "Actor subclass: BrokenActor\n  state: x = 5\n\n  myMethod: cond =>\n    cond ifTrue: [\n      y := self.x + 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        // The local var 'y' should be stored with __local__ prefix
        assert!(
            code.contains("__local__y"),
            "Local var 'y' should use __local__ prefix in maps:put. Got:\n{code}"
        );
        // The field assignment self.x := y should NOT use maps:get('y', ...) — that would
        // be the buggy read (key mismatch). Instead, it should reference the temp var directly.
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_false_block_reads_back_correctly() {
        // BT-1225: Same fix applies to ifFalse: blocks.
        let src = "Actor subclass: TestActor\n  state: x = 10\n\n  myMethod: cond =>\n    cond ifFalse: [\n      y := self.x - 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("__local__y"),
            "Local var 'y' should use __local__ prefix in maps:put. Got:\n{code}"
        );
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_local_mutation_generates_inline_case() {
        // BT-1392: Value type ifTrue: with captured local mutation should
        // generate an inline case expression (not runtime dispatch).
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue: with local mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            !code.contains("'beamtalk_message_dispatch':'send'"),
            "Value type ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_false_local_mutation_generates_inline_case() {
        // BT-1392: Value type ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifFalse: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_if_false_local_mutation() {
        // BT-1392: Value type ifTrue:ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2] ifFalse: [x := 3]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue:ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_collect_wrapping_if_true_with_field_mutation() {
        // BT-1477: collect: block containing ifTrue: with self.field := mutation.
        // The field mutation inside the conditional must be threaded through the
        // collect: loop accumulator, not silently lost.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list collect: [:each | each > 0 ifTrue: [self.n := self.n + 1]. each * 2]\n    self.n\n";
        let code = codegen(src);
        // The collect: should use a stateful accumulator (maps:put for field 'n')
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: collect: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_wrapping_do_with_field_mutation() {
        // BT-1477: ifTrue: wrapping do: block with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: ifTrue: wrapping do: should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: inside ifTrue: should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_do_wrapping_if_true_with_field_mutation() {
        // BT-1477: do: block containing ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_triple_nested_if_true_do_if_true_with_field_mutation() {
        // BT-1477: ifTrue: wrapping do: wrapping ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: triple-nested should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: triple-nested should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_field_assignment_control_flow_rhs_unpacks_tuple() {
        // BT-1479: self.field := <control-flow-with-mutations> must unpack {Value, State}
        let src = "Actor subclass: A\n  state: x = 0\n  state: y = 0\n\n  m: flag =>\n    self.x := flag ifTrue: [self.y := 1. 42] ifFalse: [0]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "FieldAssignmentControlFlow should unpack element(1) from RHS tuple. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "FieldAssignmentControlFlow should unpack element(2) for state. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('x'"),
            "Should update field 'x' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_conditional_branch() {
        // BT-1479: SelfFieldAtPut inside conditional branch must not fall through to wildcard
        let src = "Actor subclass: A\n  state: x = 0\n\n  m: flag =>\n    flag ifTrue: [self fieldAt: #x put: 42]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut in conditional branch should generate maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_method_body() {
        // BT-1479: self fieldAt: name put: value in method body (non-conditional context)
        let src = "Actor subclass: A\n  state: x = 0\n\n  m =>\n    self fieldAt: #x put: 42\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut should generate maps:put. Got:\n{code}"
        );
    }
}
