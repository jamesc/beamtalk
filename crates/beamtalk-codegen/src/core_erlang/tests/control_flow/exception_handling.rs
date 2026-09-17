// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type/`ClassVars` mutation threading through `on:do:`/
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
    // The trailing slot is value-type/class-method-only. For an actor,
    // `state.field :=` and the construct's own `StateAcc` are the SAME map,
    // so the mutation already threads out through slot 2 and adding a third
    // would be dead weight — `exception_construct_families` filters `State`
    // out of `eligible_families` for exactly this reason, and this pins that
    // gate.
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
    // is checked against `value.unwrap_parens()`, so the parenthesized
    // wrapper must not hide the construct and fall back to the generic
    // (tuple-leaking) path.
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

// ─── ADR 0122 / BT-3506: `ClassVars` threading through `on:do:`/`ensure:` ──
//
// The `ClassVars` twin of the `SelfVt` threading pinned above (BT-3486):
// a class-method self-send inside `on:do:`/`ensure:` mutates a class var, but
// (before this fix) the construct's own `{Result, StateAcc}` tuple had no
// slot to carry it out, so the mutation was silently discarded on every
// normal return. Runtime ground truths for these shapes are pinned in
// `stdlib/test/fixtures/mutation_corpus_class_method.bt`.

#[test]
fn test_class_method_self_send_in_ensure_try_body_threads_class_vars_out() {
    // BT-3506's headline repro (case (b)): before the fix this compiled
    // cleanly and returned 0 — `bump`'s own `ClassVars1` was bound only
    // inside the try body's own rendered Document, out of scope once the
    // `try` closed.
    let src = concat!(
        "Object subclass: CvEnsureSelfSend\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class selfSendEnsure =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      self bump\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@cvensureselfsend").with_workspace_mode(true),
    )
    .expect("a class-method self-send inside an ensure: try body must compile");

    assert!(
        code.contains(", StateAcc1, ClassVars1}") || code.contains(", StateAcc, ClassVars1}"),
        "the try body's return tuple must grow a trailing slot carrying its own \
         mutated ClassVars. Got:\n{code}"
    );
    assert!(
        code.contains("let ClassVars1 = call 'erlang':'element'(3,")
            || code.contains("let ClassVars2 = call 'erlang':'element'(3,"),
        "the post-construct rebind must extract the threaded ClassVars from tuple slot 3. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@cvensureselfsend", &code);
}

#[test]
fn test_class_method_self_send_in_on_do_handler_threads_class_vars_out() {
    // The `on:do:` handler mirror of the `ensure:` repro above — the
    // handler is a SIBLING arm of the try body, so both must return the
    // same tuple shape, and the try arm (which does not itself mutate)
    // carries the construct's pre-`try` `ClassVars` in its own slot.
    let src = concat!(
        "Object subclass: CvOnDoSelfSend\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 5\n\n",
        "  class selfSendOnDo =>\n",
        "    [\n",
        "      nil\n",
        "    ] on: Error do: [:e |\n",
        "      self bump\n",
        "      nil\n",
        "    ]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@cvondoselfsend").with_workspace_mode(true),
    )
    .expect("a class-method self-send inside an on:do: handler must compile");

    assert!(
        code.contains(", ClassVars}"),
        "the non-mutating try arm must carry the pre-try ClassVars in the same slot. \
         Got:\n{code}"
    );
    assert!(
        code.contains(", ClassVars1}"),
        "the mutating handler arm must carry its own mutated ClassVars1. Got:\n{code}"
    );
    assert!(
        code.contains("let ClassVars1 = call 'erlang':'element'(3,"),
        "the post-construct rebind must extract the threaded ClassVars from tuple slot 3. \
         Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('runs', ClassVars1)"),
        "the trailing field read must see the threaded ClassVars1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@cvondoselfsend", &code);
}

#[test]
fn test_class_method_self_send_ensure_with_co_occurring_local_mutation_compiles() {
    // BT-3506's shape (c): a self-send AND a local-var mutation in the same
    // try body. Before this fix, the self-send's own `ClassVars` `Bind`
    // sourced from whatever `class_var_version()` the method's own top
    // frame had already reached — a version this arm's OWN fresh
    // `ThreadedIr` frame never produced — tripping `NonLinearVersion`/
    // `UnboundVersion` in a debug-build `verify()` panic. The new
    // `seed_exception_arm_family` reset-and-shadow (mirroring BT-3486's
    // `SelfVt` fix) resets the arm's own `ClassVars` counter to 0 first, so
    // the self-send's `Bind` sources from the frame's own implicit,
    // always-bound version-0 entry instead.
    let src = concat!(
        "Object subclass: CvEnsureSelfSendWithLocal\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class selfSendEnsureWithLocal =>\n",
        "    self.runs := 0\n",
        "    seen := 0\n",
        "    [\n",
        "      self bump\n",
        "      seen := seen + 1\n",
        "    ] ensure: [nil]\n",
        "    self.runs + seen\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@cvensureselfsendwithlocal").with_workspace_mode(true),
    )
    .expect(
        "a class-method self-send alongside a co-occurring local mutation inside an \
         ensure: try body must compile without a ThreadedIr verify failure",
    );
    assert_compiles_through_erlc("bt@cvensureselfsendwithlocal", &code);
}

#[test]
fn test_class_method_direct_class_var_write_in_ensure_try_body_still_rejected() {
    // BT-3506's shape (a) decision: a BARE, direct `self.classVar := ...`
    // write remains rejected at compile time — unlike the identical `SelfVt`
    // shape, which BT-3486 made supported. `lower_field_assignment_bind`
    // (shared by every threaded-body construct: loops, conditionals, blocks,
    // `on:do:`/`ensure:`) unconditionally rejects a bare class-var write via
    // `reject_class_var_field_assignment` — widening that shared gate to
    // support this one shape for `on:do:`/`ensure:` alone would require
    // threading a new bypass flag through every one of its call sites for a
    // shape BT-3506 deliberately left rejected rather than support: assign to
    // a local inside the block, then mutate the class var once after, is
    // already a clean, well-established fix. This pins that the construct
    // still rejects it, and with the accurate `ClassVarAssignmentInThreadedBody`
    // diagnostic (not a claim that the shape compiles fine).
    let src = concat!(
        "Object subclass: CvEnsureDirectWrite\n",
        "  classState: runs = 0\n\n",
        "  class directWriteEnsure =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      self.runs := self.runs + 1\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvensuredirectwrite").with_workspace_mode(true),
    );
    let err = match result {
        Err(err @ CodeGenError::ClassVarAssignmentInThreadedBody { .. }) => err,
        other => panic!(
            "expected ClassVarAssignmentInThreadedBody for a direct class-var write inside \
             an ensure: try body. Got: {other:?}"
        ),
    };
    let rendered = err.to_string();
    assert!(
        rendered.contains("Cannot assign to class variable 'runs'"),
        "the diagnostic must accurately describe a class-variable write (not a generic \
         'field'). Got:\n{rendered}"
    );
}

// ─── BT-3522: mutations BELOW an arm's own top level ────────────────────────
//
// BT-3506 gated this construct's trailing family slot on a top-level-only walk
// of its own (`block_top_level_mutates_family`), so ANY mutation at depth >= 1
// was invisible: no slot was allocated, and the mutation was silently
// discarded on normal return. BT-3522 routes the gate through ADR 0122's
// recursive `body_threaded_families` instead, which splits the newly-visible
// shapes in two — see `exception_construct_families`' own doc comment:
//
//  * a mutation in a top-level statement's own SUB-EXPRESSION is carried
//    end-to-end (ADR 0118's `thread_ahead` already binds it in the arm's own
//    frame; it only ever lacked a slot to land in);
//  * a mutation inside a NESTED BLOCK is not carried, and is now rejected
//    per-statement instead of dropped.

#[test]
fn test_class_method_self_send_in_sub_expression_of_ensure_try_body_threads_class_vars_out() {
    // The carry half. `_t := 1 + (self bump)` is a top-level statement whose
    // class-var producer sits in its own right-hand side — E6/E7's
    // `thread_ahead` lowers it into a real `Bind` in this arm's frame, which
    // advances the ambient class-var version that `ExceptionArm`'s
    // before/after diff reads back into the trailing slot.
    //
    // On the parent commit this shape did NOT merely lose the mutation: with
    // `families` empty, the `Bind`'s target had no slot to be rendered into
    // and `threaded_ir::verify()` reported `UnboundVersion` for `ClassVars1`
    // (a debug-build panic, an `erlc` unbound-variable crash in release). So
    // the recursive detector is what makes this shape correct, and rejecting
    // it instead would trade one regression for another.
    let src = concat!(
        "Object subclass: CvEnsureSubExprSelfSend\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class selfSendInSubExpr =>\n",
        "    self.runs := 0\n",
        "    _t := 0\n",
        "    [\n",
        "      _t := 1 + (self bump)\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@cvensuresubexprselfsend").with_workspace_mode(true),
    )
    .expect("a class-method self-send in a try-body statement's sub-expression must compile");

    assert!(
        code.contains(", ClassVars1}"),
        "the try body's return tuple must grow a trailing slot carrying the arm's own \
         mutated ClassVars1. Got:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'element'(3, "),
        "the post-construct rebind must extract the threaded ClassVars from tuple slot 3. \
         Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@cvensuresubexprselfsend", &code);
}

#[test]
fn test_class_method_self_send_nested_in_conditional_in_ensure_try_body_is_compile_error() {
    // The reject half, and BT-3522's headline repro verbatim. The `ifTrue:`
    // block compiles to its own closure whose internal `ClassVars{N}` rebind
    // is scoped strictly inside it; nothing in E1..E7 unpacks that back out,
    // so before this fix the construct returned the PRE-`try` class-var map
    // and the method answered 0 instead of 1 — no error, no warning.
    //
    // `ClassMethodSelfSendInUnthreadedBlock` is reused rather than a new
    // variant minted: its message ("this block has no way to thread such a
    // mutation back to the class method that owns it") already describes
    // exactly this shape.
    let src = concat!(
        "Object subclass: CvNestedEnsureProbe\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class selfSendNestedInEnsure: flag =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      flag ifTrue: [ self bump ]\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedensureprobe").with_workspace_mode(true),
    );
    let err = match result {
        Err(err @ CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. }) => err,
        other => panic!(
            "expected ClassMethodSelfSendInUnthreadedBlock for a class-method self-send \
             nested inside an ifTrue: within an ensure: try body. Got: {other:?}"
        ),
    };
    let rendered = err.to_string();
    assert!(
        rendered.contains("Cannot send 'bump' to self inside this block"),
        "the diagnostic must name the self-sent selector. Got:\n{rendered}"
    );
}

#[test]
fn test_class_method_self_send_nested_in_conditional_in_on_do_handler_is_compile_error() {
    // Same shape in the OTHER arm — the per-statement rejection lives in
    // `generate_exception_body_with_threading_inner`, which every arm (try
    // body, `on:do:` handler, both `ensure:` cleanup runs) lowers through, so
    // one call covers all of them. Pinned for the handler specifically
    // because that arm reaches the loop through a different caller path
    // (`push_exception_arm` after the catch preamble).
    let src = concat!(
        "Object subclass: CvNestedOnDoHandler\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class selfSendNestedInHandler: flag =>\n",
        "    self.runs := 0\n",
        "    [nil]\n",
        "      on: Error\n",
        "      do: [:e |\n",
        "        flag ifTrue: [ self bump ]\n",
        "      ]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedondohandler").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "a class-method self-send nested inside an on:do: handler's own ifTrue: must be \
         a clean compile error, not a silent drop. Got: {result:?}"
    );
}

#[test]
fn test_class_method_bare_class_var_write_nested_in_conditional_in_ensure_is_compile_error() {
    // The other `ClassVars` shape `is_family_mutation` recognises. This one
    // was already rejected before BT-3522 — but through the generic
    // stored-closure fall-through (`FieldAssignmentInUnsupportedBlock`, whose
    // text talks about threading state back to an ACTOR). The per-statement
    // check now fires first with `ClassVarAssignmentInThreadedBody`, which
    // names the class variable and the loop/conditional body it sits in.
    // Pinned so the two `ClassVars` shapes are known to route through one
    // rule rather than two accidents.
    let src = concat!(
        "Object subclass: CvNestedBareWriteEnsure\n",
        "  classState: runs = 0\n\n",
        "  class bareWriteNested: flag =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      flag ifTrue: [ self.runs := self.runs + 1 ]\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedbarewriteensure").with_workspace_mode(true),
    );
    let err = match result {
        Err(err @ CodeGenError::ClassVarAssignmentInThreadedBody { .. }) => err,
        other => panic!(
            "expected ClassVarAssignmentInThreadedBody for a bare class-var write nested \
             inside an ifTrue: within an ensure: try body. Got: {other:?}"
        ),
    };
    let rendered = err.to_string();
    assert!(
        rendered.contains("Cannot assign to class variable 'runs'"),
        "the diagnostic must name the class variable. Got:\n{rendered}"
    );
}

#[test]
fn test_value_type_field_write_nested_in_conditional_in_ensure_is_still_rejected() {
    // The `SelfVt` half must NOT regress: `reject_unthreadable_value_self_field_write`
    // has rejected this since BT-3486 (the acceptance criteria's "do not touch
    // the SelfVt-in-on:do:/ensure: path"), and widening the shared detector
    // must not change that. `test_value_type_field_write_in_ensure_nested_in_ensure_is_compile_error`
    // above pins the `ensure:`-in-`ensure:` nesting; this pins the
    // conditional nesting, which is the shape the recursive detector newly
    // classifies as family-threading.
    let field = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtCondInEnsureSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal: flag =>\n",
            "    [\n",
            "      flag ifTrue: [self.total := self.total + 1]\n",
            "    ] ensure: [nil]\n",
            "    self.total\n",
        ),
        "bt@vtcondinensureself",
    );
    assert_eq!(field, "total");
}

#[test]
fn test_class_method_non_mutating_self_send_nested_in_conditional_in_ensure_still_compiles() {
    // The guard rail for the rejection above. `is_family_mutation` counts
    // EVERY same-class self-send for `ClassVars` (the detection question:
    // `generate_class_method_self_send` rebinds `ClassVars` from the
    // callee's reply unconditionally, so a slot is needed either way), but a
    // callee that provably never writes a class variable returns the
    // caller's own map unchanged — losing that rebind inside a nested block
    // loses nothing. `reject_unthreadable_class_var_mutation` therefore
    // narrows the self-send shape through `class_var_mutating_selectors()`
    // before erroring; without that narrowing this compiles-and-behaves
    // correctly shape became a spurious compile error.
    let src = concat!(
        "Object subclass: CvNestedPureSelfSend\n",
        "  classState: runs = 0\n\n",
        "  class helper => 42\n\n",
        "  class pureSelfSendNested: flag =>\n",
        "    self.runs := 0\n",
        "    _t := 0\n",
        "    [\n",
        "      flag ifTrue: [ _t := self helper ]\n",
        "      nil\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedpureselfsend").with_workspace_mode(true),
    )
    .expect("a provably non-class-var-mutating nested self-send must still compile");
    assert_compiles_through_erlc("bt@cvnestedpureselfsend", &code);
}

#[test]
fn test_class_method_self_send_reaching_mutation_via_class_reference_wrapper_is_compile_error() {
    // Adversarial-review finding on BT-3522: `class_var_mutating_selectors()`'s
    // fixed point originally only followed `self foo`-spelled same-class
    // sends when deciding whether a callee transitively mutates — a callee
    // reached only via `ClassName foo` (still a same-class, same-activation
    // send; `generate_class_method_self_send` treats the two spellings
    // identically) was invisible to it. `wrapper`'s body is `Holder bump`
    // (a `ClassReference` send, not a `self` send) to `bump`, which DOES
    // write `self.runs`. Before the fix this made
    // `reject_unthreadable_class_var_mutation` treat `self wrapper` (nested
    // inside `ifTrue:`, so not itself carried) as provably pure and let it
    // through — reintroducing this issue's own headline silent-drop bug one
    // level of indirection away from the direct case.
    let src = concat!(
        "Object subclass: Holder\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class wrapper => Holder bump\n\n",
        "  class probe: flag =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      flag ifTrue: [ self wrapper ]\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvclassrefwrapper").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "a self-send nested in a conditional must be rejected even when the mutation is only \
         reachable through a `ClassName foo`-spelled same-class send, not just a `self foo` \
         one. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_nested_in_match_arm_in_ensure_is_compile_error() {
    // Adversarial-review finding on BT-3522: `MatchArm::body` is a bare
    // `Expression`, not a `Block`, so `reject_unthreadable_class_var_mutation`'s
    // original walk (which only special-cased `Expression::Block`) never
    // searched inside a `match:` arm at all — a class-var mutation reached
    // only through a `1 -> self bump` arm, itself nested inside an `ensure:`
    // try body, compiled cleanly and silently returned the pre-mutation
    // value. Confirmed empirically with `beamtalk test` before adding this
    // Rust-level pin. The fix extends the walk to search `match:` arm
    // bodies (and guards) the same way it already searches a nested block's
    // statements.
    let src = concat!(
        "Object subclass: CvMatchArmNestedEnsure\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class probe: v =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      v match: [\n",
        "        1 -> self bump;\n",
        "        _ -> 0\n",
        "      ]\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvmatcharmnestedensure").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "a class-var-mutating self-send nested inside a match: arm within an ensure: try body \
         must be a clean compile error, not a silent drop. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_nested_in_nested_ensure_in_ensure_try_body_is_compile_error() {
    // Adversarial-review finding on BT-3522: a nested `ensure:`/`on:do:` is
    // itself a top-level statement of the OUTER arm, and its own codegen
    // recursion emits its own real `ClassVars` rebind (the same detection
    // shape as the carried sub-expression case). This test pins that the
    // OUTER construct still rejects rather than silently drops: the inner
    // `ensure:`'s own `ClassVars` slot is scoped to its own generated tuple,
    // which nothing in the outer arm's E1..E7 dispatch unpacks, so the same
    // "can't carry, so reject" rule applies as it does to a bare nested
    // block. No test previously pinned either the carry or the reject
    // direction for this specific double-nested-construct shape.
    let src = concat!(
        "Object subclass: CvNestedEnsureInEnsure\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class probe =>\n",
        "    self.runs := 0\n",
        "    [\n",
        "      [ self bump ] ensure: [nil]\n",
        "    ] ensure: [nil]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedensureinensure").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "a class-var-mutating self-send nested inside an ensure: within another ensure:'s try \
         body must be a clean compile error, not a silent drop. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_nested_in_conditional_in_ensure_cleanup_block_is_compile_error() {
    // Coverage-gap note from the BT-3522 adversarial review: every other new
    // test in this module targets the try body or the `on:do:` handler; none
    // targeted the `ensure:` CLEANUP block specifically, which is lowered
    // twice (once for the normal-return path, once for the exception path)
    // through its own call to the same `generate_exception_body_with_threading`
    // this issue's per-statement rejection lives in. Pinning it here confirms
    // that sharing, rather than assuming it from the try-body/handler cases.
    let src = concat!(
        "Object subclass: CvNestedEnsureCleanup\n",
        "  classState: runs = 0\n\n",
        "  class bump => self.runs := self.runs + 1\n\n",
        "  class probe: flag =>\n",
        "    self.runs := 0\n",
        "    [nil] ensure: [\n",
        "      flag ifTrue: [ self bump ]\n",
        "    ]\n",
        "    self.runs\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvnestedensurecleanup").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "a class-var-mutating self-send nested inside an ensure:'s own CLEANUP block must be a \
         clean compile error, not a silent drop. Got: {result:?}"
    );
}
