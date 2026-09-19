// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0124 §3/B3: the guarded `maps:find` read for a `late` slot, in both
//! `generate_field_access` branches (instance `State`/`Self` map,
//! class-method `ClassVars` map) — `expressions.rs`. Companion to BT-3548's
//! `test_late_state_omitted_from_init_state_literal` /
//! `test_late_classstate_omitted_from_classstate_map` in
//! `class_registration.rs`, which cover the *representation* (key absence);
//! this covers the *read*.

use super::*;

/// Renders `text` exactly as [`beamtalk_cerl_doc::leaf::binary_lit`] would
/// inline it into generated Core Erlang (`#{#<byte>(8,1,...),...}#`) — error
/// hints are UTF-8 byte-segment binaries, not literal source text, so a
/// hint-text assertion must compare against this encoded form rather than
/// the human-readable string.
fn hint_binary(text: &str) -> String {
    beamtalk_cerl_doc::leaf::binary_lit(text).to_pretty_string()
}

/// Instance branch: `self.proc` on a `late` field must compile to the
/// three-arm `maps:find` guard, not a bare `maps:get`, and the result must
/// be real erlc-valid Core Erlang.
#[test]
fn late_instance_field_read_emits_guarded_maps_find_and_compiles() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Subprocess\n",
        "  state: id :: Integer = 0\n\n",
        "  sendLine: line => self.proc\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    assert!(
        code.contains("case call 'maps':'find'('proc', State) of"),
        "a `late` instance field read must guard with maps:find, not maps:get. Got:\n{code}"
    );
    assert!(
        code.contains("<{'ok', 'nil'}> when 'true' ->"),
        "the guard must have a {{ok, nil}} arm (ADR 0124 §3 — nil injected via spawnWith:/fieldAt:put:/perform:). Got:\n{code}"
    );
    assert!(
        code.contains("<'error'> when 'true' ->"),
        "the guard must have an absent-key ('error') arm. Got:\n{code}"
    );
    assert!(
        code.contains("'uninitialized_state_error'"),
        "both raise arms must construct an uninitialized_state_error. Got:\n{code}"
    );
    let expected_hint = hint_binary(
        "CodexClient field 'proc' (:: Subprocess) is declared `late` and has not been assigned yet",
    );
    assert!(
        code.contains(&expected_hint),
        "the hint must name the class, field, and declared type per the ADR's documented text. Got:\n{code}"
    );
    // Bare maps:get on 'proc' must never appear — every read is guarded.
    assert!(
        !code.contains("call 'maps':'get'('proc', State)"),
        "a `late` field must never fall through to a bare maps:get. Got:\n{code}"
    );

    assert_compiles_through_erlc("codex_client", &code);
}

/// An eager field alongside a `late` one must stay an unguarded `maps:get`
/// — the guard is opt-in per slot, not a blanket change to field access.
#[test]
fn eager_instance_field_read_stays_bare_maps_get_alongside_late_sibling() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Subprocess\n",
        "  state: id :: Integer = 0\n\n",
        "  getId => self.id\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    assert!(
        code.contains("call 'maps':'get'('id', State)"),
        "an eager field's read must stay a bare maps:get. Got:\n{code}"
    );
}

/// Class-method branch: `self.current` on a `late classState:` var, read
/// from a class method, must compile to the same three-arm `maps:find`
/// guard against `ClassVars`.
#[test]
fn late_class_var_read_emits_guarded_maps_find_and_compiles() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late classState: current :: CodexClient\n",
        "  classState: total :: Integer = 0\n\n",
        "  class fetch => self.current\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    assert!(
        code.contains("case call 'maps':'find'('current', ClassVars) of"),
        "a `late` class variable read must guard with maps:find against ClassVars, not maps:get. Got:\n{code}"
    );
    assert!(
        code.contains("<{'ok', 'nil'}> when 'true' ->")
            && code.contains("<'error'> when 'true' ->"),
        "the class-var guard must have both the nil and absent-key arms. Got:\n{code}"
    );
    let expected_hint = hint_binary(
        "CodexClient field 'current' (:: CodexClient) is declared `late` and has not been assigned yet",
    );
    assert!(
        code.contains(&expected_hint),
        "the class-var hint must name the class, field, and declared type. Got:\n{code}"
    );

    assert_compiles_through_erlc("codex_client", &code);
}

/// An eager class variable alongside a `late` one must stay an unguarded
/// `maps:get` against `ClassVars`.
#[test]
fn eager_class_var_read_stays_bare_maps_get_alongside_late_sibling() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late classState: current :: CodexClient\n",
        "  classState: total :: Integer = 0\n\n",
        "  class fetchTotal => self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    assert!(
        code.contains("call 'maps':'get'('total', ClassVars)"),
        "an eager class variable's read must stay a bare maps:get. Got:\n{code}"
    );
}

/// Cross-file inheritance: a `late` instance field declared on a
/// compiled-elsewhere ancestor (`ClassInfo` metadata only, no AST) must
/// still be guarded when read in the subclass's own method — the flattened
/// `ClassHierarchy::state_field_kind` walk (BT-3547) is what
/// `generate_field_access` depends on for this, exactly like B2 depends on
/// it for the init-literal/post-initialize-check exclusion.
#[test]
fn cross_file_inherited_late_field_read_is_guarded() {
    use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, DeclaredType};
    use std::collections::HashMap;

    let ancestor = ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("BaseActor"),
        superclass: Some(ecow::EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![ecow::EcoString::from("proc")],
        state_types: {
            let mut m = HashMap::new();
            m.insert(
                ecow::EcoString::from("proc"),
                DeclaredType::parse("Subprocess"),
            );
            m
        },
        state_has_default: {
            let mut m = HashMap::new();
            m.insert(ecow::EcoString::from("proc"), false);
            m
        },
        state_kinds: {
            let mut m = HashMap::new();
            m.insert(
                ecow::EcoString::from("proc"),
                beamtalk_core::ast::SlotKind::Late,
            );
            m
        },
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    let src = "BaseActor subclass: CodexClient\n  useIt => self.proc\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@codex_client").with_class_hierarchy(vec![ancestor]),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));

    assert!(
        code.contains("case call 'maps':'find'('proc', State) of"),
        "a `late` field inherited from a cross-file ancestor must still be guarded in the subclass. Got:\n{code}"
    );
}

/// ADR 0124 §4b: a `late` field read inside a hybrid `whileTrue:` loop body
/// must NOT be pre-extracted before the loop (`pre_extract_hybrid_fields`,
/// `while_loops.rs`) — that would turn "raises when the body actually reads
/// it" into "raises even if the loop condition is false on the very first
/// check and the body never runs at all". The read stays a guarded
/// `maps:find` inside the loop body instead.
///
/// Hybrid mode only activates when the body has at least one field
/// *mutation* (`select_hybrid_params`, `plan.rs`) — a pure read-only loop
/// body takes the ordinary `StateAcc`-threaded path, which was never
/// pre-extracted and so was never at risk. Mutating an eager sibling field
/// (`n`) alongside the `late` read is what actually exercises the
/// hybrid pre-extraction path this test targets.
///
/// The `late` read is bound to a local (`p := self.proc`) rather than left
/// as a bare discarded mid-body statement: a bare discarded field-read
/// statement in the middle of a hybrid loop body fails to compile even for
/// an ordinary eager field (a separate, pre-existing hybrid-loop
/// statement-sequencing gap, unrelated to `late` — confirmed via a probe
/// test below and tracked as a follow-up, not part of ADR 0124 B3).
#[test]
fn late_field_read_inside_hybrid_while_loop_is_not_pre_extracted_and_compiles() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Subprocess\n",
        "  state: n :: Integer = 0\n\n",
        "  pump: limit =>\n",
        "    i := 0\n",
        "    [i < limit] whileTrue: [\n",
        "      p := self.proc\n",
        "      self.n := self.n + 1\n",
        "      i := i + 1\n",
        "    ]\n",
        "    nil\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    // The hybrid pre-extraction preamble must mutate-extract 'n' (eager)
    // but never pre-extract 'proc' via a bare maps:get.
    assert!(
        code.contains("call 'maps':'get'('n', "),
        "the eager mutated field 'n' should still be pre-extracted by hybrid mode. Got:\n{code}"
    );
    assert!(
        !code.contains("call 'maps':'get'('proc', "),
        "a `late` field must never be pre-extracted with a bare maps:get ahead of the loop. Got:\n{code}"
    );
    // The guarded read must still appear (inside the loop body), reading
    // against the hybrid loop's closed-over State (its live-map naming
    // under `in_hybrid_loop`, per `current_state_var`), not a pre-extracted var.
    assert!(
        code.contains("case call 'maps':'find'('proc', State) of"),
        "the `late` field read inside the loop body must still be the guarded maps:find form. Got:\n{code}"
    );

    assert_compiles_through_erlc("codex_client", &code);
}

/// A `late` field read inside a `whileTrue:` body whose condition is false
/// from the very first check — the body never runs, so the guarded read
/// inside it never executes and must not raise. Driven by a parameter
/// rather than a mutated local counter: a plain (non-hybrid, non-
/// direct-params) `whileTrue:` loop that threads a mutated local alongside
/// *any* field read — eager or `late` — hits a separate, pre-existing
/// codegen gap (`current_state_var()` names a `StateAcc` the loop's `letrec`
/// fun never actually receives as a parameter when the loop carries no
/// field mutation) unrelated to this ADR; tracked as a follow-up, not part
/// of B3. A boolean parameter used directly as the condition needs no
/// per-iteration local threading at all, so it sidesteps that gap and still
/// exercises exactly what B3 requires: the guard costs nothing when the
/// loop body it lives in never runs.
#[test]
fn late_field_read_inside_while_loop_that_never_runs_compiles() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Integer\n\n",
        "  pump: shouldRun :: Boolean -> Nil =>\n",
        "    [shouldRun] whileTrue: [\n",
        "      self.proc\n",
        "    ]\n",
        "    nil\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");
    assert!(
        code.contains("case call 'maps':'find'('proc', "),
        "the `late` field read inside the loop body must be the guarded maps:find form. Got:\n{code}"
    );
    assert_compiles_through_erlc("codex_client", &code);
}

/// ADR 0124 §4e/B3: a `late` field that is *mutated* inside a
/// `whileTrue:` body must never select Hybrid mode at all
/// (`select_hybrid_params`, `plan.rs`) — unlike a `late` read, which is
/// individually skipped by `pre_extract_hybrid_fields`'s `readonly_params`
/// filter, a mutated field has no such per-field skip in `mutated_params`
/// (it must stay a plain fun param for the `hybrid_mutated_fields`
/// rebind-on-write path), so pre-extracting it would still `badkey`-crash
/// on the loop's very first pre-extraction, before the loop's condition is
/// even tested once. The whole loop must instead fall back to the ordinary
/// `StateAcc` convention, where the `late` field's read and write both go
/// through the guarded per-iteration path.
#[test]
fn late_field_mutated_inside_while_loop_does_not_select_hybrid_and_compiles() {
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: total :: Integer\n\n",
        "  pump: limit =>\n",
        "    i := 0\n",
        "    [i < limit] whileTrue: [\n",
        "      self.total := i\n",
        "      i := i + 1\n",
        "    ]\n",
        "    nil\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("codex_client"))
        .expect("codegen should succeed");

    // A `late` mutated field must never be pre-extracted with a bare
    // maps:get ahead of the loop (Hybrid mode's pre-extraction preamble).
    assert!(
        !code.contains("call 'maps':'get'('total', "),
        "a `late` mutated field must never be pre-extracted with a bare maps:get. Got:\n{code}"
    );
    // The write must go through the guarded per-iteration maps:put path,
    // not the Hybrid fun-param rebind.
    assert!(
        code.contains("call 'maps':'put'('total', "),
        "the `late` field's write must fall back to the ordinary guarded maps:put. Got:\n{code}"
    );

    assert_compiles_through_erlc("codex_client", &code);
}
