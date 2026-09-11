// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type and actor `self` threading through `on:do:`/
//! `ensure:` exception-handling constructs, including chained and
//! nested ensure blocks.

use super::*;

// The guard rail — that the rejection stays scoped to writes the
// loop genuinely cannot carry, and never swallows a BARE, TOP-LEVEL
// `self.field := ...` statement in a `Letrec` body (the one shape
// `plan.threads_value_self` does carry, and hence the one shape
// `reject_unthreadable_value_self_field_write`'s root-node skip lets through)
// — is `test_value_type_field_write_in_to_do_threads_self_through_tail_call`
// and `test_value_type_field_only_loop_packs_from_fresh_map_not_state` above.
// Those two tests already pin exactly the two single-write guard-rail
// fixtures (with and without a sibling local mutation) and assert the full
// `Self` threading, not merely that `erlc` accepts the module, so an
// over-firing rejection turns them red. Deliberately NOT restated as a third,
// weaker copy here (CLAUDE.md's no-duplicate-implementations rule).
//
// The two cases those fixtures leave open — a loop body with MORE THAN ONE
// top-level write, and one mixing a threadable top-level write with an
// unthreadable nested one — are covered above by
// `test_value_type_multiple_top_level_field_writes_in_loop_still_thread` and
// `test_value_type_nested_field_write_rejected_beside_threadable_top_level_write`,
// which together pin that the root-node skip is per-statement rather than
// per-body.

// ─── Value-type `Self` threading through `on:do:`/`ensure:` ─────────────────
//
// The third construct family in ADR 0120's gap list, and the only one that
// failed SILENTLY: the construct compiled cleanly, dropped the mutation, and
// left every later `self.field` read looking at the pre-`try` snapshot. Same
// `TestCase subclass:` framing as the loop block above (see its header for
// why that is the only shape that can reach `CodeGenContext::ValueType` with a
// field write). Runtime ground truths for these shapes are pinned in
// `stdlib/test/value_type_mutation_matrix_test.bt`'s axis-4 `on:do:`/`ensure:`
// cells, each paired with the Actor cell for the identical fragment body.

#[test]
fn test_value_type_field_write_in_ensure_try_body_threads_self_out() {
    // The headline repro. Before the fix the try body's own
    // `let Self1 = maps:put('total', _Val, Self) in` was computed and then
    // never referenced again — the construct returned a two-element
    // `{Result, StateAcc}` tuple with nowhere to put it — so the method's
    // trailing read was `maps:get('total', Self)`, the ORIGINAL parameter.
    let src = concat!(
        "TestCase subclass: VtEnsureSelfThread\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    seen := 0\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      seen := seen + 1\n",
        "    ] ensure: [nil]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtensureselfthread").with_workspace_mode(true),
    )
    .expect("value-type field write inside an ensure: try body must compile");

    assert!(
        code.contains(", StateAcc1, Self1}"),
        "the try body's return tuple must grow a trailing slot carrying its own \
         mutated Self1. Got:\n{code}"
    );
    assert!(
        code.contains("let Self1 = call 'erlang':'element'(3,"),
        "the post-construct rebind must extract the threaded Self from tuple slot 3. \
         Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the method's trailing field read must see the threaded Self1, not the \
         original Self parameter. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtensureselfthread", &code);
}

#[test]
fn test_value_type_field_write_only_ensure_is_not_sequenced_away() {
    // The minimal repro: with no outer local in either block, the
    // outer-local predicate reports "nothing to thread", so before this fix
    // the whole construct was classified `Pure` and emitted as a bare
    // `let _seqN = <construct> in` — discarding the result tuple, and with it
    // the mutation.
    let src = concat!(
        "TestCase subclass: VtEnsureSelfOnly\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtensureselfonly").with_workspace_mode(true),
    )
    .expect("a field-write-only value-type ensure: must compile");

    assert!(
        code.contains("let _ExTuple"),
        "the construct must be bound as a threading construct, not sequenced away \
         as a discarded `let _seqN`. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see the threaded Self1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtensureselfonly", &code);
}

#[test]
fn test_value_type_ensure_cleanup_chains_from_try_bodys_self() {
    // On the success path the cleanup runs AFTER the try body, so it must see
    // the try body's write — but an Erlang binding made inside `try` is not in
    // scope in the `of` arm, so the cleanup re-seeds the unversioned `Self`
    // from the tuple's own trailing slot, exactly as it already re-seeds
    // `StateAcc` from slot 2. Ground truth for this shape is 11, not 10.
    let src = concat!(
        "TestCase subclass: VtEnsureBothSelf\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [\n",
        "      self.total := self.total + 10\n",
        "      nil\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtensurebothself").with_workspace_mode(true),
    )
    .expect("field writes in both an ensure: try body and its cleanup must compile");

    assert!(
        code.contains("let Self = call 'erlang':'element'(3,"),
        "the success arm must re-seed the version-0 `Self` from the try tuple's \
         trailing slot before running the cleanup. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see the cleanup's own threaded Self1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtensurebothself", &code);
}

#[test]
fn test_value_type_field_write_in_on_do_handler_threads_self_out() {
    // The handler is a SIBLING arm of the try body: only one of the two ever
    // runs, so both must return the same tuple shape. The arm that does not
    // itself write carries the construct's pre-`try` `Self` in the slot.
    let src = concat!(
        "TestCase subclass: VtOnDoSelfThread\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    seen := 0\n",
        "    [\n",
        "      seen := seen + 1\n",
        "      nil\n",
        "    ] on: Error do: [:e |\n",
        "      self.total := self.total + 5\n",
        "      nil\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtondoselfthread").with_workspace_mode(true),
    )
    .expect("value-type field write inside an on:do: handler must compile");

    assert!(
        code.contains(", Self}"),
        "the non-writing try arm must carry the pre-try Self in the same slot. \
         Got:\n{code}"
    );
    assert!(
        code.contains(", Self1}"),
        "the writing handler arm must carry its own mutated Self1. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see the threaded Self1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtondoselfthread", &code);
}

#[test]
fn test_value_type_chained_ensure_constructs_seed_each_arm_from_live_self() {
    // Two such constructs back to back. The second one's arms open with
    // `let Self = Self1 in` — the version-0 shadow that both makes the second
    // construct read the first's threaded value AND gives each arm's own
    // ThreadedIr frame the version-0 entry parameter `verify` requires (a
    // frame never produces a version it was merely handed).
    let src = concat!(
        "TestCase subclass: VtEnsureChainSelf\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    [\n",
        "      self.total := self.total + 2\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtensurechainself").with_workspace_mode(true),
    )
    .expect("two chained value-type field-writing ensure: constructs must compile");

    assert!(
        code.contains("try let Self = Self1 in"),
        "the second construct's try arm must seed its version-0 Self from the \
         first construct's threaded Self1. Got:\n{code}"
    );
    assert!(
        code.contains("let Self2 = call 'erlang':'element'(3,"),
        "the second construct's own rebind must advance to Self2. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self2)"),
        "the trailing field read must see the SECOND construct's Self2. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtensurechainself", &code);
}

#[test]
fn test_value_type_on_do_both_arms_writing_mint_sibling_self_versions() {
    // The `SelfVt` counterpart of the sibling-arm hazard (which the two
    // `..._do_not_trip_nonlinear_version` tests pin for `StateAcc`): the try
    // body and the handler are alternatives, so both restart from the same
    // pre-`try` baseline and both mint `Self1`. Two sibling `ThreadedIr`
    // frames reaching the SAME version must not read as a non-linear version
    // step, and the two `Self1` bindings must stay in their own Core Erlang
    // scopes (try body vs catch clause) rather than colliding.
    let src = concat!(
        "TestCase subclass: VtOnDoBothSelf\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      Error signal: \"boom\"\n",
        "    ]\n",
        "      on: Error\n",
        "      do: [:e |\n",
        "        self.total := self.total + 5\n",
        "        nil\n",
        "      ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtondobothself").with_workspace_mode(true),
    )
    .expect("field writes in BOTH on:do: arms must compile");

    // Both arms carry their own mutated Self1 in the trailing slot — the
    // handler's raise discards the try body's, which is why the runtime
    // ground truth for this shape is 5 and not 6 (pinned in
    // value_type_mutation_matrix_test.bt's
    // `testStateFieldOnDoHandlerNonLastReadWrite*` pair).
    assert_eq!(
        code.matches(", Self1} ").count(),
        2,
        "each of the two sibling arms must close its own three-element tuple \
         with its own Self1. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see whichever arm's threaded Self1 ran. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtondobothself", &code);
}

#[test]
fn test_actor_ensure_keeps_two_element_result_tuple() {
    // The trailing slot is value-type-only. For an actor, `state.field :=` and
    // the construct's own `StateAcc` are the SAME map, so the mutation already
    // threads out through slot 2 and adding a third would be dead weight —
    // `exception_blocks_thread_value_self` is gated on
    // `CodeGenContext::ValueType`, and this pins that gate.
    let src = concat!(
        "Actor subclass: ActorEnsureNoSelfSlot\n",
        "  state: total = 0\n\n",
        "  computeTotal =>\n",
        "    seen := 0\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      seen := seen + 1\n",
        "    ] ensure: [nil]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@actorensurenoselfslot").with_workspace_mode(true),
    )
    .expect("an actor ensure: with a state-field write must compile");

    assert!(
        !code.contains(", Self}") && !code.contains(", Self1}"),
        "an actor construct must not grow a value-type Self slot. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@actorensurenoselfslot", &code);
}

#[test]
fn test_value_type_ensure_in_last_position_unwraps_tuple_to_self() {
    // Before this fix, a value-type `on:do:`/`ensure:` in the
    // method's LAST-statement position leaked its raw
    // `{Result, StateAcc, Self}` tuple as the method's own return value —
    // `computeTotal`'s own last statement was this `ensure:` send, so the
    // whole method returned the tuple instead of `nil`. Now it routes
    // through the shared `lower_threaded_last` transform (same as loops
    // and conditionals), unwrapping element 1 as the logical value and
    // rebinding `Self` from element 3.
    let src = concat!(
        "TestCase subclass: VtLastPositionEnsure\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    [\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtlastpositionensure").with_workspace_mode(true),
    )
    .expect("a value-type ensure: in last position must compile");
    // The method body's OWN trailing expression — not the exception
    // construct's internal try/catch tuple bookkeeping, which legitimately
    // builds `{Result, StateAcc, Self}` tuples throughout — must be the
    // unwrapped logical value, not a raw tuple.
    let compute_total = code
        .split("'computeTotal'/1 = fun (Self) ->")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("generated code must contain computeTotal's body");
    let trailing_line = compute_total
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .expect("computeTotal must have a non-empty body")
        .trim();
    assert!(
        !trailing_line.starts_with('{'),
        "computeTotal's own return value must be the unwrapped logical result, not the \
         raw exception-construct tuple. Trailing line was: {trailing_line:?}. Full \
         function:\n{compute_total}"
    );
}

#[test]
fn test_value_type_ensure_as_assignment_rhs_unwraps_tuple_and_rebinds_self() {
    // Before this fix, `r := [...] ensure: [...]` bound `r` to the
    // raw `{Result, StateAcc, Self}` tuple and never rebound the method's
    // live `Self`, so a later `self.total` read the PRE-try snapshot and the
    // mutation was silently dropped. Now `emit_threaded_assign_rhs` extracts
    // element 1 to the target and rebinds `Self` from element 3 (mirroring
    // `generate_vt_exception_construct_open`'s non-last extraction).
    let src = concat!(
        "TestCase subclass: VtAssignRhsEnsure\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    r := [\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtassignrhsensure").with_workspace_mode(true),
    )
    .expect("a value-type ensure: as an assignment RHS must compile");
    assert!(
        !code.contains("let R = let StateAcc"),
        "the assignment target must not be bound to the raw exception-construct tuple. \
         Got:\n{code}"
    );
}

#[test]
fn test_value_type_field_write_in_ensure_nested_in_ensure_is_compile_error() {
    // `block_writes_vt_self_field` is deliberately top-level-body-only
    // (same reason as `loop_body_threads_value_self`), so a field write nested
    // inside an INNER `ensure:`'s block is invisible to the OUTER `ensure:`'s
    // own threading decision. Before this fix that mutation was silently
    // dropped (the outer construct's tuple carried no trailing `Self` slot at
    // all). Now `generate_exception_body_with_threading_inner` rejects it
    // with the same `FieldAssignmentInUnsupportedBlock` diagnostic the
    // identical loop-nesting shape already gets.
    let field = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtEnsureInEnsureSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    [\n",
            "      [\n",
            "        self.total := self.total + 1\n",
            "        nil\n",
            "      ] ensure: [nil]\n",
            "      nil\n",
            "    ] ensure: [nil]\n",
            "    self.total\n",
        ),
        "bt@vtensureinensureself",
    );
    assert_eq!(field, "total");
}

#[test]
fn test_value_type_parenthesized_ensure_as_assignment_rhs_unwraps_tuple() {
    // A parenthesized `ensure:`/`on:do:` assignment RHS
    // (`r := (... ensure: [...])`) must not dodge `emit_threaded_assign_rhs`'s
    // detection — mirrors the identical paren-stripping fix for
    // match-arm field writes. `is_exception_construct_with_vt_local_threading`
    // / `is_exception_construct_with_vt_self_field_threading` are checked
    // against `value.unwrap_parens()`, so the parenthesized wrapper must not
    // hide the construct and fall back to the generic (tuple-leaking) path.
    let src = concat!(
        "TestCase subclass: VtParenAssignRhsEnsure\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    r := ([\n",
        "      self.total := self.total + 1\n",
        "      nil\n",
        "    ] ensure: [nil])\n",
        "    self.total + r\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtparenassignrhsensure").with_workspace_mode(true),
    )
    .expect("a parenthesized value-type ensure: as an assignment RHS must compile");
    assert!(
        !code.contains("let R = let StateAcc"),
        "the assignment target must not be bound to the raw exception-construct tuple \
         even when parenthesized. Got:\n{code}"
    );
}

#[test]
fn test_class_method_ensure_last_position_and_assign_rhs_thread_local() {
    // `lower_threaded_last` / `emit_threaded_assign_rhs` are shared
    // by the value-type instance-method boundary AND the class-method
    // boundary (`try_generate_class_method_threaded_last` /
    // `generate_class_method_local_var_binding`, gen_server/methods.rs) —
    // `is_exception_construct_with_vt_local_threading` gates on
    // `in_class_method() || ValueType`. So this fix also closes the
    // identical gap for a class-method `on:do:`/`ensure:` that mutates a
    // captured OUTER LOCAL (not a class var) in last-statement / assign-RHS
    // position — previously the raw `{Result, StateAcc}` tuple leaked the
    // same way. Two class methods exercise both positions on one class so a
    // single compile proves both.
    let src = concat!(
        "Object subclass: ClassMethodEnsureProbe\n\n",
        "  class computeLast =>\n",
        "    sum := 0\n",
        "    [\n",
        "      sum := sum + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    sum\n\n",
        "  class computeAssignRhs =>\n",
        "    sum := 0\n",
        "    r := [\n",
        "      sum := sum + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    sum\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@classmethodensureprobe").with_workspace_mode(true),
    )
    .expect("a class-method ensure: in last / assign-RHS position must compile");
    assert!(
        !code.contains("let R = let StateAcc"),
        "computeAssignRhs's target must not bind to the raw exception-construct tuple. \
         Got:\n{code}"
    );
    let compute_last = code
        .split("'class_computeLast'/2 = fun (ClassSelf, ClassVars) ->")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("generated code must contain class_computeLast's body");
    let trailing_line = compute_last
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .expect("class_computeLast must have a non-empty body")
        .trim();
    assert!(
        !trailing_line.starts_with('{'),
        "class_computeLast's own return value must be the unwrapped logical result, not \
         the raw exception-construct tuple. Trailing line was: {trailing_line:?}. Full \
         function:\n{compute_last}"
    );
}
