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

        let (branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;

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

        let (branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;

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
        let (true_branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(true_block))?;

        // False branch (reset to same initial state)
        let (false_branch_doc, _) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(false_block))?;

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
        let obj_var = self.fresh_temp_var("Obj");
        let recv_code = self.expression_doc(receiver)?;
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
            "let ",
            Document::String(obj_var.clone()),
            " = ",
            recv_code,
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

        let body: Vec<&Expression> = block
            .body
            .iter()
            .map(|s| &s.expression)
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
            } else if Self::is_local_var_assignment(expr) {
                // BT-1053/BT-1225: Local variable mutation inside conditional branch.
                // generate_local_var_assignment_in_loop emits open let chain:
                // "let _Val = <value> in let StateAccN = maps:put('__local__key', _Val, StateAccM) in "
                let doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(doc);
                if is_last {
                    // last_open_scope_result is set to _Val by generate_local_var_assignment_in_loop
                    last_result.clone_from(&self.last_open_scope_result);
                } else {
                    // BT-1225: Bind the variable name to the temp var so subsequent reads
                    // within this block resolve directly to the temp var (e.g. `_Val1`)
                    // rather than falling through to maps:get without the __local__ prefix,
                    // which would cause a {badkey,VarName} crash at runtime.
                    //
                    // is_local_var_assignment(expr) guarantees expr is Assignment { target: Identifier }.
                    // If either invariant breaks, fail loudly rather than silently skipping bind_var.
                    let Expression::Assignment { target, .. } = expr else {
                        unreachable!("is_local_var_assignment must ensure expr is an Assignment");
                    };
                    let Expression::Identifier(id) = target.as_ref() else {
                        unreachable!(
                            "is_local_var_assignment must ensure assignment target is an Identifier"
                        );
                    };
                    let val_var = self.last_open_scope_result.clone().expect(
                        "generate_local_var_assignment_in_loop must set last_open_scope_result",
                    );
                    self.bind_var(&id.name, &val_var);
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    docs.push(d);
                }
            } else if is_last && self.control_flow_has_mutations(expr) {
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
            } else if !is_last && self.control_flow_has_mutations(expr) {
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
}
