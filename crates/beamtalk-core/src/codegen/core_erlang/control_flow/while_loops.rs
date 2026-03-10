// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! While loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for `whileTrue:` and `whileFalse:` loop constructs
//! with both pure and state-threading variants.

use super::super::document::{Document, join};
use super::super::intrinsics::validate_block_arity_exact;
use super::super::{CoreErlangGenerator, Result, block_analysis};
use super::{BodyKind, ThreadingPlan};
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
        self.generate_while_loop_with_mutations(condition, body, false)
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
        self.generate_while_loop_with_mutations(condition, body, true)
    }

    /// Shared implementation for `whileTrue:` and `whileFalse:` stateful loops.
    ///
    /// `negate = false` → continue when condition is `'true'` (whileTrue:).
    /// `negate = true`  → continue when condition is `'false'` (whileFalse:).
    ///
    /// In direct-params mode (BT-1275, no field mutations) the fun signature is
    /// `(Var1, ..., VarN)` instead of `(StateAcc)`.
    fn generate_while_loop_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, Some(condition));

        if plan.use_direct_params {
            return self.generate_while_loop_direct(condition, body, &plan, negate);
        }
        if plan.use_hybrid_params {
            return self.generate_while_loop_hybrid(condition, body, &plan, negate);
        }

        let cond_var = self.fresh_temp_var("CondFun");

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec!["letrec 'while'/1 = fun (StateAcc) -> "]);

        // BT-598: At the start of each loop iteration, read threaded locals from StateAcc.
        // Use push_scope so bindings don't leak to caller after the letrec.
        self.push_scope();
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

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

        // Condition application and true/false arm headers
        if negate {
            docs.push(docvec![
                format!(" in case apply {cond_var} (StateAcc) of "),
                "<'false'> when 'true' -> ",
            ]);
        } else {
            docs.push(docvec![
                format!(" in case apply {cond_var} (StateAcc) of "),
                "<'true'> when 'true' -> ",
            ]);
        }

        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::Letrec)?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        if negate {
            docs.push(docvec![
                format!(" apply 'while'/1 ({final_state_var}) "),
                "<'true'> when 'true' -> {'nil', StateAcc} ",
                "end ",
            ]);
        } else {
            docs.push(docvec![
                format!(" apply 'while'/1 ({final_state_var}) "),
                "<'false'> when 'true' -> {'nil', StateAcc} ",
                "end ",
            ]);
        }

        // Pop scope to restore original bindings (before the letrec)
        self.pop_scope();

        // Initial call with packed state
        docs.push(Document::String(format!(
            "in apply 'while'/1 ({init_state})"
        )));

        Ok(Document::Vec(docs))
    }

    /// BT-1275: Direct-params variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN)` instead of `fun (StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    fn generate_while_loop_direct(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope).
        let initial_direct_args = plan.initial_direct_args(self);

        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = param_names.len();
        let param_list_doc = || {
            join(
                param_names.iter().map(|v| Document::String(v.clone())),
                &Document::Str(", "),
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(docvec![
            "letrec 'while'/",
            Document::String(arity.to_string()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register var bindings — no unpack docs in direct-params mode.
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        debug_assert!(unpack_docs.is_empty());

        // The condition closure captures the current vars from scope.
        // We pass only the params (not StateAcc) since there is no StateAcc.
        docs.push(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

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

        // Apply condition with current params.
        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            Document::String(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        let (body_doc, _) = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        docs.push(body_doc);

        // Collect final var names after body execution.
        let final_args: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect();

        // Build exit StateAcc using the CURRENT iteration's param names.
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        let final_args_doc = join(
            final_args.into_iter().map(Document::String),
            &Document::Str(", "),
        );
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply 'while'/",
            Document::String(arity.to_string()),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);

        self.pop_scope();

        let initial_args_doc = join(
            initial_direct_args.into_iter().map(Document::String),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply 'while'/",
            Document::String(arity.to_string()),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1326: Hybrid direct-params + State threading variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN, RField1, ..., State)` — locals and read-only fields as
    /// direct params, actor state as an explicit `State` parameter.
    /// Field mutations use `State*` naming (not `StateAcc*`).
    #[allow(clippy::too_many_lines)]
    fn generate_while_loop_hybrid(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // BT-1326: Pre-extract read-only fields before the letrec.
        let mut pre_extract_docs: Vec<Document<'static>> = Vec::new();
        let readonly_params: Vec<(String, String)> = plan
            .readonly_fields
            .iter()
            .map(|field| {
                let var_name = self.fresh_temp_var(&format!(
                    "{}Field",
                    CoreErlangGenerator::to_core_erlang_var(field)
                ));
                pre_extract_docs.push(docvec![
                    "let ",
                    Document::String(var_name.clone()),
                    " = call 'maps':'get'('",
                    Document::String(field.clone()),
                    "', ",
                    Document::String(initial_state.clone()),
                    ") in ",
                ]);
                (field.clone(), var_name)
            })
            .collect();

        let local_param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let readonly_param_names: Vec<String> =
            readonly_params.iter().map(|(_, v)| v.clone()).collect();
        let state_param_name = "State".to_string();
        let arity = local_param_names.len() + readonly_param_names.len() + 1;

        // Param list: (Var1, ..., VarN, RField1, ..., State)
        let param_list_doc = || {
            join(
                local_param_names
                    .iter()
                    .map(|v| Document::String(v.clone()))
                    .chain(
                        readonly_param_names
                            .iter()
                            .map(|v| Document::String(v.clone())),
                    )
                    .chain(std::iter::once(Document::String(state_param_name.clone()))),
                &Document::Str(", "),
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.extend(pre_extract_docs);
        docs.push(docvec![
            "letrec 'while'/",
            Document::String(arity.to_string()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register local var bindings — no unpack docs in hybrid mode.
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        debug_assert!(
            unpack_docs.is_empty(),
            "hybrid while: unpack should emit no code"
        );

        // Condition closure captures local vars + readonly fields + State parameter.
        docs.push(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        let prev_in_loop_body = self.in_loop_body;
        let prev_hybrid = self.in_hybrid_loop;
        let prev_state_version = self.state_version();
        let prev_readonly_field_params = std::mem::replace(
            &mut self.hybrid_readonly_field_params,
            readonly_params.iter().cloned().collect(),
        );
        self.in_loop_body = true;
        self.in_hybrid_loop = true;
        self.set_state_version(0);

        let cond_result = if let Expression::Block(cond_block) = condition {
            self.generate_block_body(cond_block)
        } else {
            self.generate_expression(condition)
        };

        self.in_loop_body = prev_in_loop_body;
        self.in_hybrid_loop = prev_hybrid;
        self.set_state_version(prev_state_version);

        let cond_doc = match cond_result {
            Ok(doc) => doc,
            Err(e) => {
                self.hybrid_readonly_field_params = prev_readonly_field_params;
                return Err(e);
            }
        };
        docs.push(cond_doc);

        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            Document::String(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        self.in_hybrid_loop = true;
        // Re-set readonly params for body generation (they were modified during condition codegen).
        self.hybrid_readonly_field_params = readonly_params.iter().cloned().collect();
        let body_result = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec);
        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.in_hybrid_loop = prev_hybrid;
        let (body_doc, final_state_version) = body_result?;
        docs.push(body_doc);

        let final_state_var = if final_state_version == 0 {
            Document::String(state_param_name.clone())
        } else {
            docvec![
                Document::String(state_param_name.clone()),
                Document::String(final_state_version.to_string()),
            ]
        };

        let final_local_args: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect();

        let exit_stateacc =
            plan.generate_exit_stateacc_hybrid(&local_param_names, &state_param_name, self);

        // Recursive call: updated locals, readonly fields (unchanged), updated state
        let final_args_doc = join(
            final_local_args
                .into_iter()
                .map(Document::String)
                .chain(
                    readonly_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(std::iter::once(final_state_var)),
            &Document::Str(", "),
        );
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply 'while'/",
            Document::String(arity.to_string()),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);

        self.pop_scope();

        // Initial call: initial locals, initial readonly vals, initial state
        let initial_args_doc = join(
            initial_local_args
                .into_iter()
                .map(Document::String)
                .chain(
                    readonly_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(std::iter::once(Document::String(initial_state))),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply 'while'/",
            Document::String(arity.to_string()),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
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
    fn test_while_true_simple_generates_letrec() {
        // Pure whileTrue: (no mutations) generates a letrec-based loop
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileTrue: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: should generate a letrec for the loop. Got:\n{code}"
        );
        // Simple variant evaluates condition via case apply (distinguishing it from repeat)
        assert!(
            code.contains("case apply"),
            "whileTrue: should evaluate condition via case apply. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true'"),
            "whileTrue: should match true to continue. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' -> 'nil'"),
            "whileTrue: should return nil when condition is false. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_simple_generates_letrec_with_opposite_pattern() {
        // Pure whileFalse: (no mutations) generates a letrec matching false to continue
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileFalse: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileFalse: should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' ->"),
            "whileFalse: should continue when condition is false. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true' -> 'nil'"),
            "whileFalse: should stop when condition is true. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_with_field_mutation_threads_actor_state() {
        // whileTrue: with field mutation uses actor state threading (not simple letrec)
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    [self.n < 10] whileTrue: [self.n := self.n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with mutation should generate a letrec. Got:\n{code}"
        );
        // State-threading variant uses 'while'/1 with StateAcc parameter
        assert!(
            code.contains("'while'/1"),
            "Mutating whileTrue: should use 'while'/1 with state parameter. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "whileTrue: body should update 'n' via maps:put. Got:\n{code}"
        );
    }

    // ── BT-1275: direct-params optimisation ──────────────────────────────────

    #[test]
    fn test_while_true_local_var_only_uses_direct_params() {
        // whileTrue: with only local-var mutations uses direct fun params, not StateAcc.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with local mutation should generate a letrec. Got:\n{code}"
        );
        // Direct-params: fun takes (Sum) not (StateAcc).
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileTrue: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Exactly one maps:put for exit StateAcc rebuild.
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() == 1,
            "direct-params: whileTrue: at most one maps:put (exit rebuild). Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_local_var_only_uses_direct_params() {
        // whileFalse: with only local-var mutations uses direct fun params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum >= 10] whileFalse: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileFalse: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileFalse: fun must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileFalse: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    // ── BT-1326: hybrid direct-params + State threading ──────────────────────

    #[test]
    fn test_while_true_field_plus_local_mutation_uses_hybrid_params() {
        // whileTrue: with BOTH local var mutation AND field mutation uses hybrid mode:
        // fun(LocalVar, State) — locals as direct params, actor State as explicit param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "hybrid: whileTrue: should generate a letrec. Got:\n{code}"
        );
        // Hybrid signature: fun (Sum, State) — local as direct param, State explicit
        assert!(
            code.contains("fun (Sum, State)"),
            "hybrid: whileTrue: fun should take (Sum, State) as params. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "hybrid: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Field mutation uses maps:put on State (not StateAcc)
        assert!(
            code.contains("maps':'put'('n'"),
            "hybrid: whileTrue: field mutation should use maps:put. Got:\n{code}"
        );
        // Exit rebuilds StateAcc from State + local params
        assert!(
            code.contains("ExitSA"),
            "hybrid: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
        // Exit packs local into ExitSA via maps:put
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() <= 1,
            "hybrid: whileTrue: at most one maps:put for local (exit rebuild). Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_readonly_field_pre_extracted_as_direct_param() {
        // BT-1326: whileTrue: body reads self.step (never written) and writes self.n:
        // self.step is pre-extracted before the letrec and passed as a direct param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n  state: step = 1\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + self.step. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            !code.contains("fun (StateAcc)"),
            "readonly field: whileTrue: must not use StateAcc signature. Got:\n{code}"
        );
        // 'step' is read-only — pre-extracted before letrec.
        assert!(
            code.contains("StepField"),
            "readonly field: whileTrue: fun should have a StepField param. Got:\n{code}"
        );
        // Only one maps:get for 'step' (the pre-extraction).
        assert!(
            code.match_indices("maps':'get'('step'").count() == 1,
            "readonly field: whileTrue: exactly one maps:get for 'step'. Got:\n{code}"
        );
        // Mutable field 'n' still uses maps:put for writes.
        assert!(
            code.contains("maps':'put'('n'"),
            "readonly field: whileTrue: 'n' writes should still use maps:put. Got:\n{code}"
        );
    }
}
