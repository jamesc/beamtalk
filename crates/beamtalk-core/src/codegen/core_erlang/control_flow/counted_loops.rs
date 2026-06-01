// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Counted loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for counted loop constructs: `repeat`, and mutation-threading
//! variants of `timesRepeat:`, `to:do:`, and `to:by:do:`.
//! Non-mutating cases are handled by the pure-BT tail-recursive Integer methods (BT-1054).

use super::super::document::Document;
use super::super::document::leaf;
use super::super::{CoreErlangGenerator, Result};
use super::{CountedLoopFrame, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_repeat(
        &mut self,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // repeat: infinite loop - execute body forever
        // Generate: letrec '_LoopN'/0 = fun () ->
        //     let _BodyFun = <body> in
        //     let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;
        Ok(docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in let _ = apply ",
            leaf::var(body_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_times_repeat_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let n_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        // BT-2354: gensym the loop counter so a user local named `i` (→ Core `I`)
        // cannot collide with the loop fun parameter.
        let counter = self.fresh_temp_var("loopidx");

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(n_var.clone()),
                " = ",
                receiver_code,
                " in"
            ],
            fn_name: "repeat".to_string(),
            continue_header: docvec![
                "case call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(n_var),
                ") of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::int_lit(1),
            false_arm: docvec!["<'false'> when 'true' -> {'nil', StateAcc} ", "end "],
            body_param: None,
            counter,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }

    pub(in crate::codegen::core_erlang) fn generate_to_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;
        // BT-2354: gensym the loop counter (the block param is aliased to it).
        let counter = self.fresh_temp_var("loopidx");

        // Bind the block parameter name (e.g. "i" in [:i | ...])
        let body_param = body.parameters.first().map(|p| p.name.to_string());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(start_var.clone()),
                " = ",
                receiver_code,
                " in let ",
                leaf::var(end_var.clone()),
                " = ",
                limit_code,
                " in",
            ],
            fn_name: "loop".to_string(),
            continue_header: docvec![
                "case call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var),
                ") of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::var(start_var),
            false_arm: docvec!["<'false'> when 'true' -> {'nil', StateAcc} ", "end "],
            body_param,
            counter,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }

    pub(in crate::codegen::core_erlang) fn generate_to_by_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        step: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;
        let step_var = self.fresh_temp_var("temp");
        let step_code = self.expression_doc(step)?;
        // BT-2354: gensym the loop counter (the block param is aliased to it).
        let counter = self.fresh_temp_var("loopidx");

        let body_param = body.parameters.first().map(|p| p.name.to_string());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(start_var.clone()),
                " = ",
                receiver_code,
                " in let ",
                leaf::var(end_var.clone()),
                " = ",
                limit_code,
                " in let ",
                leaf::var(step_var.clone()),
                " = ",
                step_code,
                " in",
            ],
            fn_name: "loop".to_string(),
            continue_header: docvec![
                "let Continue = case call 'erlang':'>'(",
                leaf::var(step_var.clone()),
                ", 0) of ",
                "<'true'> when 'true' -> call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var.clone()),
                ") ",
                "<'false'> when 'true' -> ",
                "case call 'erlang':'<'(",
                leaf::var(step_var.clone()),
                ", 0) of ",
                "<'true'> when 'true' -> call 'erlang':'>='(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var),
                ") ",
                "<'false'> when 'true' -> 'false' ",
                "end ",
                "end in case Continue of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec![
                "call 'erlang':'+'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(step_var),
                ")"
            ],
            initial_counter: leaf::var(start_var),
            false_arm: docvec!["<'false'> when 'true' -> {'nil', StateAcc} ", "end "],
            body_param,
            counter,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::core_erlang::tests::codegen;

    #[test]
    fn test_repeat_generates_infinite_letrec_loop() {
        // repeat generates a letrec that loops forever (no condition check)
        let src = "Actor subclass: Server\n  state: x = 0\n\n  run =>\n    [42] repeat\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "repeat should generate a letrec. Got:\n{code}"
        );
        // repeat uses a simple loop function that recurses unconditionally
        assert!(
            !code.contains("case apply"),
            "repeat should NOT have a case on a condition. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_with_field_mutation_threads_actor_state() {
        // timesRepeat: with field mutation generates state-threading loop with StateAcc
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    3 timesRepeat: [self.n := self.n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "timesRepeat: with mutation should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "timesRepeat: with mutation should thread state via StateAcc. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "timesRepeat: body should update 'n' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_do_with_field_mutation_generates_counted_loop_with_state() {
        // to:do: with field mutation generates a counted loop with state threading
        let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run =>\n    1 to: 5 do: [:i | self.sum := self.sum + i]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "to:do: with mutation should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "to:do: with mutation should thread state via StateAcc. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('sum'"),
            "to:do: body should update 'sum' via maps:put. Got:\n{code}"
        );
    }

    // ── BT-1275: direct-params optimisation ──────────────────────────────────

    #[test]
    fn test_to_do_local_var_only_uses_direct_params() {
        // to:do: with only local-var mutations must use direct fun params, not StateAcc map.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "to:do: with local mutation should generate a letrec. Got:\n{code}"
        );
        // Fun signature must be (<gensym'd counter>, Sum), not (_, StateAcc).
        // BT-2354: the counter is gensym'd (e.g. `_loopidx3`) so it can never
        // collide with a user local named `i` (→ Core `I`).
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "direct-params: letrec fun should have gensym'd counter + Sum params. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "direct-params: letrec fun must not use StateAcc signature. Got:\n{code}"
        );
        // At most one maps:put for the local variable (the exit StateAcc rebuild).
        // Per-iteration maps:put is eliminated.
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() == 1,
            "direct-params: exactly one maps:put for local sum (exit rebuild). Got:\n{code}"
        );
        // Rebuild at exit must be present.
        assert!(
            code.contains("ExitSA"),
            "direct-params: exit StateAcc must be rebuilt before returning. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_local_var_only_uses_direct_params() {
        // timesRepeat: with local-var-only mutation uses direct params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    3 timesRepeat: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "timesRepeat: direct-params: letrec fun should have gensym'd counter + Sum. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "timesRepeat: direct-params: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "timesRepeat: direct-params: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_by_do_local_var_only_uses_direct_params() {
        // to:by:do: with local-var-only mutation uses direct params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 10 by: 2 do: [:i | sum := sum + i]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "to:by:do: direct-params: letrec fun should have gensym'd counter + Sum. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "to:by:do: direct-params: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "to:by:do: direct-params: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    // ── BT-1326/BT-1342: full-extract direct-params + field extraction ───────

    #[test]
    fn test_to_do_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: When both field AND local vars are mutated, full-extract mode
        // extracts mutated fields to direct params (no State param in loop).
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "full-extract: should generate a letrec. Got:\n{code}"
        );
        // Fun signature must NOT have State — mutated field 'n' is a direct param.
        assert!(
            !code.contains(", Sum, State) ->"),
            "full-extract: letrec fun must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "full-extract: letrec fun must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' is pre-extracted before the letrec.
        assert!(
            code.contains("NField"),
            "full-extract: mutated field 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Field 'n' is pre-extracted with maps:get('n') and repacked at exit.
        assert!(
            code.contains("maps':'get'('n'"),
            "full-extract: field 'n' should be pre-extracted via maps:get. Got:\n{code}"
        );
        // Exit arm packs mutated field AND locals back.
        // Verify the repack is inside the exit arm (near ExitSA), not just from
        // the trailing `self.n := sum` outside the loop.
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "full-extract: exit arm must repack 'n' (separate from post-loop assignment). Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: timesRepeat: with both field + local mutations uses full-extract mode.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    3 timesRepeat: [sum := sum + 1. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        // No State in fun signature.
        assert!(
            !code.contains(", Sum, State) ->"),
            "timesRepeat: full-extract: must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "timesRepeat: full-extract: must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' is a direct param.
        assert!(
            code.contains("NField"),
            "timesRepeat: full-extract: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit arm repacks 'n' (separate from post-loop `self.n := sum`).
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "timesRepeat: full-extract: exit arm must repack 'n'. Got:\n{code}"
        );
        // Exit arm packs locals back.
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "timesRepeat: full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_by_do_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: to:by:do: with both field + local mutations uses full-extract mode.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 10 by: 2 do: [:i | sum := sum + i. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            !code.contains(", Sum, State) ->"),
            "to:by:do: full-extract: must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "to:by:do: full-extract: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("NField"),
            "to:by:do: full-extract: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit arm repacks 'n' (separate from post-loop `self.n := sum`).
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "to:by:do: full-extract: exit arm must repack 'n'. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "to:by:do: full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    // ── BT-2308: value-type local threading for counted loops ────────────────

    #[test]
    fn test_value_type_to_do_write_only_threads_local() {
        // BT-2308: `[:i | last := i]` mutates an outer local write-only. In value-type
        // context this must thread `last` back via the {'nil', StateAcc} tuple even
        // though it is never read inside the block.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    1 to: 5 do: [:i | last := i]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("CountedLoopResult"),
            "value-type to:do: should bind loop result to CountedLoopResult. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "value-type to:do: should extract state from element 2. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type to:do: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_to_do_read_write_threads_local() {
        // BT-2308: read+write `[:i | sum := sum + i]` must thread `sum` back to the caller.
        let src = "Object subclass: Calc\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "value-type to:do: should extract 'sum' from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_times_repeat_write_only_threads_local() {
        // BT-2308: timesRepeat: with a write-only outer-local mutation threads it back.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    3 timesRepeat: [last := 7]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type timesRepeat: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_to_by_do_threads_local() {
        // BT-2308: to:by:do: threads outer-local mutations in value-type context.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    1 to: 10 by: 2 do: [:i | last := i]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type to:by:do: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_counted_loop_last_expr_unwraps_nil() {
        // BT-2308: a mutating counted loop as the method's LAST expression must return
        // the loop's logical value (element 1 = nil), not the raw {'nil', StateAcc} tuple.
        let src = "Object subclass: Calc\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "last-expr counted loop should unwrap element 1 (nil). Got:\n{code}"
        );
    }

    #[test]
    fn test_to_do_readonly_field_pre_extracted_as_direct_param() {
        // BT-1326: When the loop body reads a field that it never writes, that field is
        // pre-extracted before the letrec and passed as a direct fun parameter.
        // Body reads self.step (readonly) and writes self.n + local sum (hybrid mode).
        let src = "Actor subclass: Ctr\n  state: n = 0\n  state: step = 1\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + self.step. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        // Hybrid mode triggered (field write for n, local sum).
        assert!(
            !code.contains(", StateAcc) ->"),
            "readonly field: must not use StateAcc signature. Got:\n{code}"
        );
        // Readonly field pre-extracted before the letrec with maps:get.
        assert!(
            code.contains("maps':'get'('step'"),
            "readonly field: 'step' should be pre-extracted via maps:get. Got:\n{code}"
        );
        // The pre-extracted value used as a fun parameter (StepField1 or similar).
        assert!(
            code.contains("StepField"),
            "readonly field: fun should have a StepField param. Got:\n{code}"
        );
        // Inside the loop, self.step should NOT generate additional maps:get for 'step'.
        // The single maps:get is the pre-extraction; body uses the param directly.
        assert!(
            code.match_indices("maps':'get'('step'").count() == 1,
            "readonly field: exactly one maps:get for 'step' (pre-extraction only). Got:\n{code}"
        );
        // Mutable field 'n' still uses maps:put for writes.
        assert!(
            code.contains("maps':'put'('n'"),
            "readonly field: 'n' writes should still use maps:put. Got:\n{code}"
        );
    }
}

#[cfg(test)]
mod bt2363_nested_counted_loops {
    use crate::codegen::core_erlang::tests::codegen;

    // BT-2363: A nested `timesRepeat:` mutating an outer local must thread the inner
    // loop's `{value, StateAcc}` result back out through the OUTER loop. Previously the
    // outer loop was emitted as a plain `beamtalk_message_dispatch:send` (no threading)
    // and the inner mutation was dropped, returning 0 instead of the accumulated value.
    #[test]
    fn test_nested_times_repeat_threads_outer_local() {
        let src = "Object subclass: NestedRepro\n\n  run =>\n    total := 0\n    3 timesRepeat: [2 timesRepeat: [total := total + 1]]\n    total\n";
        let code = codegen(src);
        // Outer loop must thread via StateAcc (not direct-params), so it can unpack the
        // inner loop's tuple result.
        assert!(
            code.contains("fun (_loopidx") && code.contains(", StateAcc) ->"),
            "outer nested-loop must use StateAcc threading, not direct-params. Got:\n{code}"
        );
        // The inner loop's tuple result is unpacked via element(2, _NestTuple...) and the
        // updated StateAcc is fed to the next outer iteration.
        assert!(
            code.contains("_NestTuple"),
            "outer loop must bind the inner loop result to a _NestTuple var. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2, _NestTuple"),
            "outer loop must extract element(2) of the inner loop tuple. Got:\n{code}"
        );
        // `total` is packed/unpacked under the __local__ key throughout.
        assert!(
            code.contains("maps':'get'('__local__total'")
                && code.contains("maps':'put'('__local__total'"),
            "nested loop must thread 'total' under the __local__ key. Got:\n{code}"
        );
    }

    // BT-2363: write-only outer-local mutation inside a nested counted loop must also be
    // threaded back (the read+write detector alone misses write-only mutations).
    #[test]
    fn test_nested_times_repeat_write_only_outer_local_threaded() {
        let src = "Object subclass: NestedRepro\n\n  run =>\n    last := 0\n    3 timesRepeat: [2 timesRepeat: [last := 7]]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "write-only nested mutation of 'last' must be threaded. Got:\n{code}"
        );
    }
}
