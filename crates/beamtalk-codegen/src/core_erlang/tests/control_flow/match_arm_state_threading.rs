// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State threading through `match:` arms and through loop/
//! conditional assignment right-hand-sides: self-mutating value
//! blocks, Tier 2 value calls, and bare field-write match-arm
//! fixtures.

use super::*;

#[test]
fn test_match_arm_self_mutating_value_block_threads_actor_state() {
    // a `match:` arm body that is a multi-statement `[...] value`
    // block mutating `self.<state>` must thread actor state correctly and
    // unwrap to the block's real value — not leak the internal `{Value,
    // NewState}` state-threading tuple as the match's result. Mixes a
    // native `nil ->` arm (mutating) with a `Pattern::Type` arm (`x ::
    // Integer -> x`, non-mutating) so this exercises `generate_match_chain`,
    // the exact path the original bug report's repro takes.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch -> Integer =>\n    nil match: [\n      nil -> [\n        self.count := self.count + 1\n        self.count\n      ] value;\n      x :: Integer -> x\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self-mutating value block:\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Self-mutating match: arm must thread state via maps:put. Got:\n{code}"
    );
    // The bug produced a raw {Value, NewState} tuple as the whole match:'s
    // result instead of unwrapping it — the generated `handle_call` clause
    // for `bumpMatch` must return `{'reply', <ResultVar>, <StateVar>}` (two
    // bare variables), not a doubly-nested tuple wrapping the match's own
    // {Value, NewState} tuple as the reply value.
    let bump_match_clause = code
        .split("<'bumpMatch'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch clause must be followed by the OtherSelector fallback");
    assert!(
        bump_match_clause.contains("{'reply', ") && !bump_match_clause.contains("{'reply', {"),
        "bumpMatch's state-threading tuple must be unwrapped into plain \
         Result/State vars before the gen_server reply, not leaked as the \
         reply value itself. Got clause:\n{bump_match_clause}"
    );

    assert_compiles_through_erlc("registry", &code);
}

#[test]
fn test_match_two_adjacent_mutating_arms_each_thread_from_the_same_base_state() {
    // Two DIFFERENT arms in the same match: each
    // mutate a different field via a `[...] value` block. Since only one arm
    // fires at runtime, each arm's `with_branch_context` call must reset to
    // the SAME pre-match base_state — arm 2's compilation must not see any
    // state_version advancement leaked from arm 1's compilation (they are
    // alternatives, not a sequence). Both arms use `Pattern::Literal`
    // (Integer), a genuine native case-literal match (unlike bare `true`/
    // `false`, which the parser treats as `Pattern::Variable` — an
    // unconditional catch-all binding, not a boolean literal test; only
    // `nil` is a reserved pattern keyword per ADR 0107), so this also
    // re-confirms the all-native fast path handles more than one mutating
    // arm.
    let src = "Actor subclass: TwoMutatingArms\n  state: a :: Integer = 0\n  state: b :: Integer = 0\n\n  run: choice -> Integer =>\n    choice match: [\n      1 -> [self.a := self.a + 1. self.a] value;\n      2 -> [self.b := self.b + 1. self.b] value\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("two_mutating_arms"))
        .expect("codegen should succeed");

    eprintln!("Generated code for match: with two adjacent mutating arms:\n{code}");

    assert!(
        code.contains("'maps':'put'('a'") && code.contains("'maps':'put'('b'"),
        "both arms must thread their own field's mutation via maps:put. Got:\n{code}"
    );
    let run_clause = code
        .split("<'run:'>")
        .nth(1)
        .expect("handle_call must have a run: clause")
        .split("<OtherSelector>")
        .next()
        .expect("run: clause must be followed by the OtherSelector fallback");
    assert!(
        run_clause.contains("'erlang':'element'(2, "),
        "run:'s match: result must be unpacked via erlang:element/2 before \
         the gen_server reply, regardless of which arm fired. Got clause:\n{run_clause}"
    );

    assert_compiles_through_erlc("two_mutating_arms", &code);
}

#[test]
fn test_match_arm_self_field_held_block_value_call_compiles_without_double_wrapping() {
    // a match: arm body that invokes a *dynamically held*
    // Tier 2 block via `self.<field> value` (not a `[...] value` block
    // literal) is a different receiver shape than the one this fix inlines.
    // `is_tier2_value_call` still classifies it as needing threading (so
    // `match_needs_mutation_threading` returns true for the whole match:),
    // but `generate_match_arm_body`'s literal-block branch doesn't match a
    // `self.field` receiver, so it falls to `expression_doc`, which already
    // unwraps+discards that call's own NewState via
    // `close_tier2_value_subexpr_doc` (the same sub-expression-
    // position limitation `test_bt2814_field_stored_tier2_value_call_in_argument_position_unpacks_result`
    // pins elsewhere — the held block's mutation is not threaded forward,
    // by design, both before and after this fix). This test only pins that
    // the combination compiles cleanly through erlc and does not
    // double-wrap the already-unwrapped value in an extra tuple layer.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  run: n =>\n    n match: [\n      x :: Integer -> self.onTick value: x;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("ctr")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self.field-held block value call:\n{code}");

    // The arm's own value: call must be unwrapped exactly once (via
    // close_tier2_value_subexpr_doc), not left as a raw tuple nor wrapped a
    // second time by generate_match_arm_body's plain-arm fallback.
    assert!(
        code.contains("'erlang':'element'(1, "),
        "self.onTick value: x must still go through the BT-2814 single-unwrap \
         path inside the match: arm. Got:\n{code}"
    );

    assert_compiles_through_erlc("ctr", &code);
}

#[test]
fn test_match_arm_cascade_tier2_value_call_compiles_without_double_wrapping() {
    // A match: arm body that is a `Cascade`
    // (`blk value: a; value: b`) on a Tier 2 local var is a third
    // `is_tier2_value_call` receiver shape besides the block-literal one
    // this fix inlines and the self.field-held one the sibling test above
    // pins. `generate_match_arm_body`'s literal-block branch only matches an
    // `Expression::MessageSend` with an `Expression::Block` receiver, so a
    // `Cascade` body falls through to `expression_doc`. Two separate layers
    // are at work here, and this test only pins the outer one:
    // - The cascade codegen correctly threads state BETWEEN the
    //   two cascaded `value:` sends (each sees the prior send's mutation),
    //   which is why `maps:put('total'...)` appears in the generated code
    //   at all.
    // - But per the sub-expression-position limitation,
    //   `close_tier2_value_subexpr_doc` then discards that cascade's FINAL
    //   NewState and returns only its logical value — so as a match: arm
    //   body, `self.total`'s mutation across the whole cascade does not
    //   persist to the actor's real state; `generate_match_arm_body`'s
    //   plain-arm fallback wraps that already-unwrapped value against the
    //   *pre-match* `base_state`, unchanged. This is the same discard that
    //   already applied to `self.field value:` in sub-expression position
    //   everywhere else, not a regression.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item -> Integer =>\n    blk := [:x | self.total := self.total + x]\n    nil match: [\n      nil -> (blk value: item; value: item);\n      x :: Integer -> x\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("ctr")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm cascade tier2 value: call:\n{code}");

    assert!(
        code.contains("'maps':'put'('total'"),
        "the cascade's own internal codegen must still generate the \
         maps:put mutation (even though the match: arm wrapper below \
         discards it, per the BT-2814 limitation documented above). Got:\n{code}"
    );

    assert_compiles_through_erlc("ctr", &code);
}

#[test]
fn test_match_arm_nested_if_true_mutation_without_value_wrapper_threads_actor_state() {
    // generalization: the same bug also reproduces when an arm
    // body is itself a nested control-flow-with-mutations construct — here
    // `flag ifTrue: [self.count := ...]` — directly, with no `[...] value`
    // wrapper around it. `ifTrue:`/`ifFalse:` already compile such a body to
    // a correctly state-threaded `{Value, NewState}` tuple on their own; the
    // bug was that `match:` didn't know to route that tuple through the
    // gen_server reply's unwrap machinery, since `Expression::Match` was
    // invisible to `control_flow_has_mutations`.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch: flag -> Integer =>\n    nil match: [\n      nil -> flag ifTrue: [self.count := self.count + 1] ifFalse: [self.count := self.count - 1];\n      x :: Integer -> x\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm nested ifTrue:/ifFalse: mutation:\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Nested ifTrue:/ifFalse: mutation inside a match: arm must thread state via maps:put. Got:\n{code}"
    );
    let bump_match_clause = code
        .split("<'bumpMatch:'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch: clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch: clause must be followed by the OtherSelector fallback");
    // The bug leaked the match:'s raw {Value, NewState} tuple as the reply
    // itself (`{'reply', {_Val, _State}, State}`, a tuple nested inside the
    // reply). The fix unwraps it via `erlang:element/2` first, so the reply
    // wraps two bare variables. (Ignore the unrelated `bad_arity` fallback
    // clause, which legitimately contains a literal `{'reply', {'error', ...`.)
    assert!(
        bump_match_clause.contains("'erlang':'element'(2, "),
        "bumpMatch:'s match: result must be unpacked via erlang:element/2 \
         before the gen_server reply. Got clause:\n{bump_match_clause}"
    );
    let reply_re =
        regex::Regex::new(r"\{'reply', (_?[A-Za-z][A-Za-z0-9_]*), (_?[A-Za-z][A-Za-z0-9_]*)\}")
            .unwrap();
    assert!(
        reply_re.is_match(bump_match_clause),
        "bumpMatch:'s state-threading tuple must be unwrapped into plain \
         Result/State vars before the gen_server reply, not leaked as the \
         reply value itself (e.g. `{{'reply', {{Val, State}}, State}}`). Got clause:\n{bump_match_clause}"
    );

    assert_compiles_through_erlc("registry", &code);
}

#[test]
fn test_match_two_adjacent_nested_if_true_mutation_arms_thread_from_the_same_base_state() {
    // The `control_flow_has_mutations` pass-through
    // branch of `generate_match_arm_body` (a nested `ifTrue:`/`ifFalse:` arm
    // body with no `[...] value` wrapper) is separately verified by
    // `test_match_two_adjacent_mutating_arms_each_thread_from_the_same_base_state`
    // ONLY for the block-literal `with_branch_context` branch — this pins
    // the SAME two-adjacent-mutating-arms property for the OTHER branch: two
    // sibling arms that are each themselves a nested ifTrue:/ifFalse:
    // mutation, no [...] value wrapper on either. `ifTrue:ifFalse:`'s own
    // codegen (`generate_if_true_if_false_with_mutations`) wraps its
    // branches in `with_branch_context` and never advances the outer
    // `state_version` counter itself, so arm 2's `expression_doc` call must
    // see the exact same pre-match `base_state` arm 1's did.
    let src = "Actor subclass: TwoIfTrueArms\n  state: a :: Integer = 0\n  state: b :: Integer = 0\n\n  run: choice -> Integer =>\n    choice match: [\n      1 -> true ifTrue: [self.a := self.a + 1] ifFalse: [self.a := self.a - 1];\n      2 -> true ifTrue: [self.b := self.b + 1] ifFalse: [self.b := self.b - 1]\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("two_if_true_arms"))
        .expect("codegen should succeed");

    eprintln!("Generated code for match: with two adjacent nested ifTrue: mutation arms:\n{code}");

    assert!(
        code.contains("'maps':'put'('a'") && code.contains("'maps':'put'('b'"),
        "both arms must thread their own field's mutation via maps:put. Got:\n{code}"
    );
    let run_clause = code
        .split("<'run:'>")
        .nth(1)
        .expect("handle_call must have a run: clause")
        .split("<OtherSelector>")
        .next()
        .expect("run: clause must be followed by the OtherSelector fallback");
    assert!(
        run_clause.contains("'erlang':'element'(2, "),
        "run:'s match: result must be unpacked via erlang:element/2 before \
         the gen_server reply, regardless of which arm fired. Got clause:\n{run_clause}"
    );
    // Also assert the reply *shape* directly (mirroring the sibling
    // single-mutating-arm test) — element(2, ...) alone wouldn't catch a
    // double-nested `{'reply', {Val, State}, State}` if some other part of
    // the wrap regressed while still happening to contain that substring.
    let reply_re =
        regex::Regex::new(r"\{'reply', (_?[A-Za-z][A-Za-z0-9_]*), (_?[A-Za-z][A-Za-z0-9_]*)\}")
            .unwrap();
    assert!(
        reply_re.is_match(run_clause),
        "run:'s state-threading tuple must be unwrapped into plain \
         Result/State vars before the gen_server reply, not leaked as the \
         reply value itself. Got clause:\n{run_clause}"
    );

    assert_compiles_through_erlc("two_if_true_arms", &code);
}

#[test]
fn test_match_arm_self_mutating_value_block_threads_actor_state_all_native_fast_path() {
    // same bug, but with every arm using a native Core Erlang
    // pattern (`nil` and `_`, no `Pattern::Type`/`Pattern::Array`) — this
    // routes through `generate_match`'s flat all-native fast path (the
    // single `case` built directly in `generate_match`) rather than the
    // recursive `generate_match_chain`, which the other regression
    // tests exercise via their `x :: Integer` arm. Both arm-body compile
    // call sites needed the same fix.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch -> Integer =>\n    nil match: [\n      nil -> [\n        self.count := self.count + 1\n        self.count\n      ] value;\n      _ -> -1\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self-mutating value block (fast path):\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Self-mutating match: arm must thread state via maps:put. Got:\n{code}"
    );
    let bump_match_clause = code
        .split("<'bumpMatch'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch clause must be followed by the OtherSelector fallback");
    assert!(
        bump_match_clause.contains("'erlang':'element'(2, "),
        "bumpMatch's match: result must be unpacked via erlang:element/2 \
         before the gen_server reply. Got clause:\n{bump_match_clause}"
    );

    assert_compiles_through_erlc("registry", &code);
}

// value-type outer-local threading for count:/detect: predicates and
// threading constructs used as a (parenthesized) assignment RHS.

#[test]
fn test_vt_count_predicate_threads_outer_local() {
    // `count:` whose predicate read+writes a captured outer local must
    // thread the mutation back so the post-`count:` read sees the final value.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    n := 0\n    #(1, 2, 3) count: [:i | n := n + 1. i > 0]\n    n\n";
    let code = codegen(src);
    eprintln!("Generated code for vt count: threading:\n{code}");
    // The count: foldl packs the threaded local into the StateAcc; the open
    // extraction must read it back via maps:get('__local__n', ...).
    assert!(
        code.contains("'__local__n'"),
        "count: must thread the captured outer local 'n' via the StateAcc. Got:\n{code}"
    );
    assert!(
        code.contains("'lists':'foldl'"),
        "count: with a mutating predicate must compile to a stateful lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_vt_detect_if_none_predicate_threads_outer_local() {
    // `detect:ifNone:` whose predicate read+writes a captured outer
    // local must thread the mutation back.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    n := 0\n    #(1, 2, 3) detect: [:i | n := n + 1. i > 10] ifNone: [-1]\n    n\n";
    let code = codegen(src);
    eprintln!("Generated code for vt detect:ifNone: threading:\n{code}");
    assert!(
        code.contains("'__local__n'"),
        "detect:ifNone: must thread the captured outer local 'n' via the StateAcc. Got:\n{code}"
    );
}

#[test]
fn test_vt_loop_as_parenthesized_assign_rhs_threads_sibling_local() {
    // a counted loop used as a parenthesized RHS — `_r := (1 to: 5 do:
    // [...])` — must thread its sibling outer local (`sum`) into method scope.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    sum := 0\n    _r := (1 to: 5 do: [:i | sum := sum + i])\n    sum\n";
    let code = codegen(src);
    eprintln!("Generated code for vt parenthesized loop assign-RHS:\n{code}");
    // The loop packs sum into the StateAcc; the assignment must extract it back
    // (element 2 of the {value, StateAcc} tuple → maps:get('__local__sum', ...)).
    assert!(
        code.contains("'__local__sum'"),
        "Parenthesized loop assign-RHS must thread sibling local 'sum'. Got:\n{code}"
    );
}

#[test]
fn test_vt_conditional_as_assign_rhs_threads_sibling_local() {
    // a threading conditional as an assignment RHS — `_r := flag ifTrue:
    // [x := 5. 42] ifFalse: [0]` — must bind the target to the branch's logical
    // value AND thread the sibling local (`x`) into method scope.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run: flag =>\n    x := 0\n    _r := flag ifTrue: [x := 5. 42] ifFalse: [0]\n    x\n";
    let code = codegen(src);
    eprintln!("Generated code for vt conditional assign-RHS:\n{code}");
    // Each branch returns {LogicalValue, X}; the assignment binds the target to
    // element 1 and rebinds x from element 2 of the case result.
    assert!(
        code.contains("'erlang':'element'(1,") && code.contains("'erlang':'element'(2,"),
        "Conditional assign-RHS must extract logical value (element 1) and sibling local (element 2). Got:\n{code}"
    );
    assert!(
        code.contains("case "),
        "Conditional assign-RHS must compile to an inline case. Got:\n{code}"
    );
}

#[test]
fn test_vt_nested_loop_in_conditional_assign_rhs_threads_local() {
    // CodeRabbit follow-up: a threaded loop *nested* inside an
    // assign-RHS conditional branch must still rebind its outer local, so a
    // later read in the same branch (and after) sees the update.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run: flag =>\n    sum := 0\n    _r := flag ifTrue: [1 to: 5 do: [:i | sum := sum + i]. sum] ifFalse: [0]\n    sum\n";
    let code = codegen(src);
    eprintln!("Generated code for nested loop in conditional assign-RHS:\n{code}");
    // The nested loop packs sum into its StateAcc; the branch must extract it
    // back via maps:get('__local__sum', ...).
    assert!(
        code.contains("'__local__sum'"),
        "Nested loop inside a conditional assign-RHS branch must thread 'sum'. Got:\n{code}"
    );
}

#[test]
fn test_actor_conditional_last_expr_lower_actor_threaded_last() {
    // When the LAST expression of an Actor method is a conditional with
    // field mutations, `lower_actor_threaded_last` must bind element 1 of the
    // {Value, NewState} tuple as the reply value and element 2 as the new
    // gen_server State, then emit {'reply', ReplyValue, NewState}.
    //
    // All prior Actor conditional-mutation tests read `self.count` AFTER the
    // conditional, so the conditional was never in last position. This exercises
    // the `lower_actor_threaded_last` path that was completely uncovered.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  setByFlag: flag =>\n    flag ifTrue: [self.count := 1] ifFalse: [self.count := -1]\n";
    let code = codegen(src);
    eprintln!("Generated code for actor conditional-as-last:\n{code}");

    // lower_actor_threaded_last binds element 1 (reply value) and element 2 (State).
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Last-position conditional must extract reply value via element(1, ...). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Last-position conditional must extract new State via element(2, ...). Got:\n{code}"
    );
    // The gen_server reply tuple must carry the State extracted from element 2.
    assert!(
        code.contains("{'reply',"),
        "Actor method must return a gen_server reply tuple. Got:\n{code}"
    );
    // The conditional compiles to an inline case.
    assert!(
        code.contains("case "),
        "Conditional must compile to an inline case expression. Got:\n{code}"
    );
}

#[test]
fn test_actor_conditional_assign_rhs_emit_actor_threaded_assign_rhs() {
    // `result := flag ifTrue: [...] ifFalse: [...]` with local variable
    // mutations in an Actor method must route through `emit_actor_threaded_assign_rhs`:
    //   - bind element 1 of the {Value, NewState} tuple → assignment target
    //   - bind element 2 → next gen_server State version
    //   - rebind sibling outer locals (here `x`) from the new State map
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  compute: flag =>\n    x := 0\n    result := flag ifTrue: [x := 1. x] ifFalse: [x := -1. x]\n    result\n";
    let code = codegen(src);
    eprintln!("Generated code for actor conditional assign-RHS:\n{code}");

    // emit_actor_threaded_assign_rhs must extract value (element 1) and State (element 2).
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Conditional assign-RHS must extract value via element(1, ...). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Conditional assign-RHS must extract new State via element(2, ...). Got:\n{code}"
    );
    // The sibling local `x` must be rebound from the updated State map.
    assert!(
        code.contains("'__local__x'"),
        "Sibling local 'x' must be rebound from the updated State map. Got:\n{code}"
    );
    // The conditional compiles to an inline case.
    assert!(
        code.contains("case "),
        "Conditional must compile to an inline case expression. Got:\n{code}"
    );
}

// ── `self.field := ...` as a bare `match:` arm body ──────────────────────────

#[test]
fn test_actor_field_write_in_match_arm_threads_state() {
    // Before the fix, nothing classified a bare `self.field := ...`
    // arm body as needing threading, so `generate_match` left `base_state` as
    // `None`, each arm's `State{N}` binding stayed scoped to its own `case`
    // clause, and the trailing `self.total` read referenced it anyway —
    // `erlc: unbound variable 'State1' in dispatch/4`. The arm now routes
    // through the same `generate_conditional_branch_inline` branch merge an
    // `ifTrue:` branch's field write uses.
    let src = concat!(
        "Actor subclass: ActorMatchArmSelfWrite\n",
        "  state: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> self.total := self.total + 10;\n",
        "      _ -> self.total := self.total + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@actormatcharmselfwrite").with_workspace_mode(true),
    )
    .expect("an actor field write in a match: arm must compile");
    assert!(
        code.contains("let StateAcc = "),
        "each arm must be lowered as a threaded branch seeded from the pre-match state. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@actormatcharmselfwrite", &code);
}

#[test]
fn test_value_type_field_write_in_match_arm_is_compile_error() {
    // The accepted scope limit, mirroring
    // `test_value_type_field_write_in_last_position_conditional_still_rejected`
    // above: a value type's field writes merge through
    // `generate_vt_conditional_open`'s trailing-slot tuple, which is wired for
    // exactly the two arms of `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`. A
    // `match:`'s N pattern arms have no equivalent, so the arm's `Self{N}`
    // binding never escapes its own `case` clause (`erlc: unbound variable
    // 'Self1'`). Rejected cleanly rather than left to crash.
    let src = concat!(
        "TestCase subclass: VtMatchArmSelfWrite\n",
        "  field: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> self.total := self.total + 10;\n",
        "      _ -> self.total := self.total + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtmatcharmselfwrite").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ValueSelfFieldAssignmentInMatchArm for a value-type field write in a \
             match: arm. Got: {other:?}"
        ),
    }
}

#[test]
fn test_value_type_class_method_class_var_write_in_match_arm_is_compile_error() {
    // Inside a VALUE-TYPE class method `self.x :=` is a CLASS-var write
    // with its own ADR 0110 threading, which a `match:` arm cannot carry
    // either. It reaches `lower_field_assignment_bind`'s shared
    // `reject_class_var_field_assignment` gate, so it produces the same clean
    // `ClassVarAssignmentInThreadedBody` diagnostic every other threaded body
    // does — never a crash, and never routed through the `Self` slot.
    let src = concat!(
        "Value subclass: ClassVarMatchArmWrite\n",
        "  classState: total = 0\n\n",
        "  class computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> self.total := self.total + 10;\n",
        "      _ -> self.total := self.total + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@classvarmatcharmwrite").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarAssignmentInThreadedBody { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ClassVarAssignmentInThreadedBody for a class-var write in a match: arm. \
             Got: {other:?}"
        ),
    }
}

#[test]
fn test_actor_class_method_class_var_write_in_match_arm_is_compile_error() {
    // The fourth context: an ACTOR class method. `self.x :=`
    // there is a class-var write (ADR 0110's `ClassVars` chain), not the
    // instance-`State` write the instance-method form threads, and a `match:`
    // arm cannot carry it either. Threading now routes the arm through
    // `lower_field_assignment_bind`'s shared `reject_class_var_field_assignment`
    // gate, so it lands on the same clean diagnostic the value-type class
    // method above produces — never the `unbound variable 'ClassVars1'` an
    // unthreaded `expression_doc` compile of the same arm used to emit.
    let src = concat!(
        "Actor subclass: ActorClassVarMatchArmWrite\n",
        "  classState: total = 0\n\n",
        "  class computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> self.total := self.total + 10;\n",
        "      _ -> self.total := self.total + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@actorclassvarmatcharmwrite").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarAssignmentInThreadedBody { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ClassVarAssignmentInThreadedBody for an actor class-var write in a \
             match: arm. Got: {other:?}"
        ),
    }
}

#[test]
fn test_actor_field_write_in_match_chain_arm_threads_state() {
    // The second lowering path: a `Pattern::Type` arm (`x :: Integer`)
    // forces `generate_match_chain`'s chain-of-nested-`case`s instead of the
    // all-native single-`case` fast path
    // `test_actor_field_write_in_match_arm_threads_state` above covers. Both
    // funnel every arm through `generate_match_arm_body` with the same
    // `base_state`, so a bare `self.field := ...` arm must thread identically
    // here — pinned so a future change to only one of the two paths can't
    // reintroduce the `unbound variable 'State1'` crash in the other.
    let src = concat!(
        "Actor subclass: ActorMatchChainSelfWrite\n",
        "  state: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      x :: Integer -> self.total := self.total + x;\n",
        "      _ -> self.total := self.total + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@actormatchchainselfwrite").with_workspace_mode(true),
    )
    .expect("an actor field write in a chain-lowered match: arm must compile");
    assert!(
        code.contains("let StateAcc = "),
        "each chain arm must be lowered as a threaded branch seeded from the pre-match state. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@actormatchchainselfwrite", &code);
}

#[test]
fn test_actor_parenthesized_field_write_in_match_arm_threads_state() {
    // Parentheses around the arm body are pure grouping, but the
    // detector originally matched `Expression::Assignment` directly, so
    // `1 -> (self.total := ...)` fell through to `expression_doc` and produced
    // the identical `erlc: unbound variable 'State1'` the unparenthesized form
    // did. Both the threading decision and the arm lowering now strip parens
    // first (and lower the stripped expression, so the ADR 0111 statement
    // classifier sees a plain `Assignment` rather than a `Parenthesized`).
    let src = concat!(
        "Actor subclass: ActorParenMatchArmSelfWrite\n",
        "  state: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> (self.total := self.total + 10);\n",
        "      _ -> (self.total := self.total + 1)\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@actorparenmatcharmselfwrite").with_workspace_mode(true),
    )
    .expect("a parenthesized actor field write in a match: arm must compile");
    assert!(
        code.contains("let StateAcc = "),
        "a parenthesized field-write arm must still be lowered as a threaded branch. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@actorparenmatcharmselfwrite", &code);
}

#[test]
fn test_value_type_parenthesized_field_write_in_match_arm_is_compile_error() {
    // The value-type half of the same paren-stripping fix — the
    // rejection must not be dodgeable by wrapping the write in parentheses,
    // which would drop straight back to the `unbound variable 'Self1'` crash.
    let src = concat!(
        "TestCase subclass: VtParenMatchArmSelfWrite\n",
        "  field: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> (self.total := self.total + 10);\n",
        "      _ -> (self.total := self.total + 1)\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtparenmatcharmselfwrite").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ValueSelfFieldAssignmentInMatchArm for a parenthesized value-type field \
             write in a match: arm. Got: {other:?}"
        ),
    }
}

#[test]
fn test_actor_field_write_wrapped_in_local_assign_in_match_arm_threads_state() {
    // `self.field := ...` wrapped one level deeper in a LOCAL
    // ASSIGNMENT (`r := (self.x := ...)`) inside a match: arm's `[...]
    // value` block body — not the bare shape fixed above. The RHS's own
    // classifier (`classify_body_expr`'s local-var-assignment branch) has
    // to see through the `r := <expr>` wrapper to notice the field write
    // underneath, or the field's own `State{N}` `Bind` is never produced,
    // reproducing the same `erlc: unbound variable 'State1'` crash
    // one nesting level down.
    let src = concat!(
        "Actor subclass: ActorLocalAssignWrappedFieldWriteInMatchArm\n",
        "  state: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> [\n",
        "        r := (self.total := self.total + 10)\n",
        "        r\n",
        "      ] value;\n",
        "      _ -> [\n",
        "        r := (self.total := self.total + 1)\n",
        "        r\n",
        "      ] value\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@actorlocalassignwrappedfieldwriteinmatcharm")
            .with_workspace_mode(true),
    )
    .expect("a local-assign-wrapped actor field write in a match: arm must compile");
    assert!(
        code.contains("let StateAcc = "),
        "a local-assign-wrapped field-write arm must still be lowered as a threaded branch. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@actorlocalassignwrappedfieldwriteinmatcharm", &code);
}

#[test]
fn test_value_type_field_write_wrapped_in_local_assign_in_match_arm_is_compile_error() {
    // The value-type half of the same local-assign-wrapping shape
    // — same rejection `test_value_type_field_write_in_match_arm_is_compile_error`
    // gets for the bare form, so the wrapper can't be used to dodge it and
    // fall back to a crash. Unlike the Actor test above, this uses the
    // BARE arm-body shape (`1 -> r := (self.x := ...)`, no `[...] value`
    // wrapper) — `generate_match`'s up-front rejection loop inspects
    // `arm.body` directly, and only that bare shape reaches it (a
    // `[...] value`-wrapped field write is a pre-existing, separate gap
    // this issue's fix does not touch).
    let src = concat!(
        "TestCase subclass: VtLocalAssignWrappedFieldWriteInMatchArm\n",
        "  field: total = 0\n\n",
        "  computeIt: v =>\n",
        "    v match: [\n",
        "      1 -> r := (self.total := self.total + 10);\n",
        "      _ -> r := (self.total := self.total + 1)\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtlocalassignwrappedfieldwriteinmatcharm")
            .with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ValueSelfFieldAssignmentInMatchArm for a local-assign-wrapped value-type \
             field write in a match: arm. Got: {other:?}"
        ),
    }
}

#[test]
fn test_value_type_field_write_in_match_arm_value_block_is_compile_error() {
    // BT-3495: unlike the two rejection tests above (a bare, or
    // local-assign-wrapped, field write directly as `arm.body`), this
    // field write is nested one level deeper inside a `[...] value`
    // block's own statements — the shape every Actor `bumpMatch`-style
    // fixture uses. `generate_match`'s up-front rejection loop previously
    // inspected `arm.body` only, never looking *inside* a `[...] value`
    // block, so this reached `erlc: unbound variable 'Self1'` instead of
    // the same clean diagnostic the bare/local-assign-wrapped shapes get.
    let src = concat!(
        "TestCase subclass: VtMatchArmValueBlockSelfWrite\n",
        "  field: total = 0\n\n",
        "  computeIt: v -> Integer =>\n",
        "    v match: [\n",
        "      1 -> [self.total := self.total + 10] value;\n",
        "      _ -> 0\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtmatcharmvalueblockselfwrite").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ValueSelfFieldAssignmentInMatchArm for a value-type field write nested in \
             a match: arm's `[...] value` block. Got: {other:?}"
        ),
    }
}

#[test]
fn test_value_type_field_write_in_match_arm_value_block_local_assign_wrapped_is_compile_error() {
    // BT-3495: the same `[...] value`-block nesting, but the field write
    // is ALSO wrapped one level deeper in a local assignment inside the
    // block (`r := (self.x := ...)`) — pins that the widened detector
    // looks through both wrappers at once, not just one or the other.
    let src = concat!(
        "TestCase subclass: VtMatchArmValueBlockLocalAssignSelfWrite\n",
        "  field: total = 0\n\n",
        "  computeIt: v -> Integer =>\n",
        "    v match: [\n",
        "      1 -> [\n",
        "        r := (self.total := self.total + 10)\n",
        "        r\n",
        "      ] value;\n",
        "      _ -> 0\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtmatcharmvalueblocklocalassignselfwrite")
            .with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfFieldAssignmentInMatchArm { field, .. }) => {
            assert_eq!(field, "total");
        }
        other => panic!(
            "Expected ValueSelfFieldAssignmentInMatchArm for a local-assign-wrapped value-type \
             field write nested in a match: arm's `[...] value` block. Got: {other:?}"
        ),
    }
}
