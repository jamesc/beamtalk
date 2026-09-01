// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3123: verifies that threading a driver's `AnalysisResult` into codegen
//! via `CodegenOptions::with_analysis` skips codegen's own re-derivation of
//! the class hierarchy, semantic facts, and inferred method return types —
//! the duplicate work this issue eliminates.
//!
//! Uses thread-local call counters (`#[cfg(any(test, feature = "test"))]` in
//! `beamtalk-core`, so this crate's own tests can see them too — BT-3362)
//! on the three costly functions (`ClassHierarchy::build_with_options`,
//! `compute_semantic_facts`, `TypeChecker::check_module`) rather than a
//! shared global counter, since plenty of *other* tests call these
//! functions too and may run concurrently on other threads — only the delta
//! this test's own thread accumulates is meaningful. See
//! `CHECK_MODULE_CALL_COUNT`'s doc for the full rationale.
//!
//! BT-3125 extends this file with `with_analysis_trusts_driver_prepared_module`
//! and `with_analysis_without_driver_prep_omits_writeback` (below) — these pin
//! the *new* contract `CodegenOptions::with_analysis` documents: codegen no
//! longer runs the writeback trio itself when the hand-off is trustworthy, so
//! a driver that forgets `semantic_analysis::lower_module_for_codegen` gets
//! silently incomplete output rather than codegen quietly covering for it.

use beamtalk_core::semantic_analysis::class_hierarchy::BUILD_CALL_COUNT;
use beamtalk_core::semantic_analysis::facts::COMPUTE_SEMANTIC_FACTS_CALL_COUNT;
use beamtalk_core::semantic_analysis::type_checker::{CHECK_MODULE_CALL_COUNT, InferredType};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full, lower_module_for_codegen};

fn parse_fixture(src: &str) -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(errors.is_empty(), "fixture failed to parse: {errors:?}");
    module
}

/// A driver that runs `analyse_full` and threads the result into codegen via
/// `with_analysis` performs each of the three costly passes exactly once —
/// codegen makes zero *additional* calls of its own.
#[test]
fn with_analysis_skips_codegen_re_derivation() {
    let module = parse_fixture("Object subclass: Greeter\n  greet => \"hello\".\n");

    let build_before = BUILD_CALL_COUNT.with(std::cell::Cell::get);
    let facts_before = COMPUTE_SEMANTIC_FACTS_CALL_COUNT.with(std::cell::Cell::get);
    let check_before = CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get);

    let analysis = analyse_full(&module, AnalysisContext::default());
    assert_eq!(
        BUILD_CALL_COUNT.with(std::cell::Cell::get) - build_before,
        1,
        "analyse_full should build the class hierarchy exactly once"
    );
    assert_eq!(
        COMPUTE_SEMANTIC_FACTS_CALL_COUNT.with(std::cell::Cell::get) - facts_before,
        1,
        "analyse_full should compute semantic facts exactly once"
    );
    assert_eq!(
        CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get) - check_before,
        1,
        "analyse_full should run the type checker exactly once"
    );

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("greeter").with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(code.contains("greet"));

    // BT-3123: codegen must not have made ANY additional calls to the three
    // costly functions — it consumed the driver's already-computed analysis
    // instead of re-deriving it from scratch.
    assert_eq!(
        BUILD_CALL_COUNT.with(std::cell::Cell::get) - build_before,
        1,
        "codegen must not re-build the class hierarchy when analysis is supplied"
    );
    assert_eq!(
        COMPUTE_SEMANTIC_FACTS_CALL_COUNT.with(std::cell::Cell::get) - facts_before,
        1,
        "codegen must not recompute semantic facts when analysis is supplied"
    );
    assert_eq!(
        CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get) - check_before,
        1,
        "codegen must not re-run the type checker when analysis is supplied"
    );
}

/// Baseline: without `with_analysis`, codegen computes its own — confirming
/// the counters actually detect a re-derivation (i.e. the assertions above
/// aren't vacuously true because codegen never calls these functions at all).
#[test]
fn without_analysis_codegen_computes_its_own() {
    let module = parse_fixture("Object subclass: Greeter\n  greet => \"hello\".\n");

    let build_before = BUILD_CALL_COUNT.with(std::cell::Cell::get);
    let facts_before = COMPUTE_SEMANTIC_FACTS_CALL_COUNT.with(std::cell::Cell::get);
    let check_before = CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get);

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("greeter"),
    )
    .expect("codegen should succeed");
    assert!(code.contains("greet"));

    assert_eq!(
        BUILD_CALL_COUNT.with(std::cell::Cell::get) - build_before,
        1,
        "codegen without an analysis handoff should build its own hierarchy"
    );
    assert_eq!(
        COMPUTE_SEMANTIC_FACTS_CALL_COUNT.with(std::cell::Cell::get) - facts_before,
        1,
        "codegen without an analysis handoff should compute its own semantic facts"
    );
    assert_eq!(
        CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get) - check_before,
        1,
        "codegen without an analysis handoff should run its own type-checking pass \
         (via infer_method_return_types) for the return-type writeback"
    );
}

/// A minimal `ClassInfo` stub, mirroring the private helpers of the same
/// name used throughout `semantic_analysis` tests (e.g.
/// `receiver_knowledge::tests::base_class_info`).
fn base_class_info(
    name: &str,
    superclass: &str,
) -> beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
    beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
        surface_incomplete: false,
        name: name.into(),
        superclass: Some(superclass.into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

/// Real multi-file package drivers (`compile_source_with_bindings`,
/// compiler-port's `handle_compile`) feed the *same* cross-file class list to
/// both `AnalysisContext::with_pre_loaded_classes` (for `analyse_full`) and
/// `CodegenOptions::with_class_hierarchy`/`with_class_superclass_index` (for
/// codegen) — see `write_core_erlang_with_bindings`'s BT-3123 comment. This
/// guards against a regression where that overlap stops being a no-op: if
/// `add_from_beam_meta`/`add_external_superclasses` ever "see" a class that
/// wasn't already in the handed-off hierarchy for realistic same-driver
/// inputs, codegen's safety net (correctly!) falls back to recomputing
/// `method_return_types` — which would silently defeat the fix for every
/// real multi-file package build, while every other test here only exercises
/// the empty-cross-file-metadata (REPL/single-file) case.
#[test]
fn with_analysis_skips_re_derivation_with_overlapping_cross_file_metadata() {
    let module = parse_fixture(
        "Object subclass: Bar\n  useFoo: f => f.\n\nBar subclass: Baz\n  hi => \"hi\".\n",
    );

    // Mirrors a package build's Pass 1 output: full ClassInfo for a
    // same-package sibling file's class, AND a name-only superclass index
    // entry for the very same class (BT-894's codegen-only field, populated
    // from the same Pass 1 walk as the full ClassInfo list in every real
    // driver).
    let foo_info = base_class_info("Foo", "Object");
    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("Foo".to_string(), "Object".to_string());

    let build_before = BUILD_CALL_COUNT.with(std::cell::Cell::get);
    let check_before = CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get);

    let analysis = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_classes(vec![foo_info.clone()]),
    );
    assert_eq!(
        BUILD_CALL_COUNT.with(std::cell::Cell::get) - build_before,
        1
    );
    assert_eq!(
        CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get) - check_before,
        1
    );

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bar")
            .with_class_hierarchy(vec![foo_info])
            .with_class_superclass_index(superclass_index)
            .with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(code.contains("useFoo"));

    // The overlap must be recognised as a no-op: codegen must not fall back
    // to rebuilding the hierarchy or re-running the type checker just
    // because `pre_class_hierarchy`/`class_superclass_index` were non-empty.
    assert_eq!(
        BUILD_CALL_COUNT.with(std::cell::Cell::get) - build_before,
        1,
        "codegen must not rebuild the hierarchy when the pre-loaded class/superclass \
         data it's given is already reflected in the handed-off analysis"
    );
    assert_eq!(
        CHECK_MODULE_CALL_COUNT.with(std::cell::Cell::get) - check_before,
        1,
        "codegen must not re-run the type checker when the pre-loaded class/superclass \
         data it's given is already reflected in the handed-off analysis"
    );
}

/// BT-3125: a driver that calls `lower_module_for_codegen` on its own module
/// — using the same `AnalysisResult` it goes on to hand to `with_analysis`
/// — gets the return-type writeback reflected in the generated `-spec`: an
/// unannotated method whose body infers to `Integer` gets a spec entry
/// (`generate_method_spec` returns `None` for an entirely unannotated
/// method — see its doc — so the spec's mere presence, not just its
/// content, proves the writeback ran).
#[test]
fn with_analysis_trusts_driver_prepared_module() {
    let mut module = parse_fixture("Object subclass: Foo\n  bar => 42.\n");
    let analysis = analyse_full(&module, AnalysisContext::default());

    lower_module_for_codegen(
        &mut module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
    );

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("foo").with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("{'type', 0, 'integer', []}"),
        "expected the driver-prepared module's inferred Integer return type \
         to appear in the generated spec; got:\n{code}"
    );
}

/// BT-3125's inverse: a driver that hands off `AnalysisResult` via
/// `with_analysis` WITHOUT first calling `lower_module_for_codegen` no
/// longer gets the writeback applied on its behalf — codegen trusts the
/// hand-off is already prepared instead of re-running it. This pins the
/// contract documented on `CodegenOptions::with_analysis` and is the
/// behavioural counterpart to `with_analysis_skips_codegen_re_derivation`'s
/// call-count assertions above (which prove *no work happens*; this proves
/// *the AST is consequently unprepared*).
#[test]
fn with_analysis_without_driver_prep_omits_writeback() {
    let module = parse_fixture("Object subclass: Foo\n  bar => 42.\n");
    let analysis = analyse_full(&module, AnalysisContext::default());

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("foo").with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("{'type', 0, 'integer', []}"),
        "expected no writeback-derived spec without a prior \
         `lower_module_for_codegen` call; got:\n{code}"
    );
}

/// BT-3125 (post-review fix): when codegen's own cross-file enrichment
/// (`add_from_beam_meta`/`add_external_superclasses`) invalidates a driver's
/// hand-off, the "fuller hierarchy" recompute must actually *overwrite* a
/// stale return type the driver's earlier (narrower) pass already wrote back
/// — not silently keep it, because `infer_method_return_types`/
/// `apply_return_type_writeback_from_map` both gate on `return_type.is_none()`
/// (to protect real user annotations) and would otherwise treat the driver's
/// own prior writeback output as already-settled.
///
/// Simulates staleness deterministically: after running real analysis (which
/// correctly infers `bar => 42` as `Integer`), corrupt the driver's own
/// `method_return_types` map to `String` before writing it back — exactly as
/// if an earlier, less-informed pass had gotten it wrong — then hand the
/// *same* (corrupted) map to codegen via `with_analysis`, together with a
/// `with_class_hierarchy` stub codegen's own analysis never saw (forcing the
/// fallback). The correct, freshly-reinferred `Integer` must win.
#[test]
fn with_analysis_refreshes_stale_return_type_when_hand_off_invalidated() {
    let mut module = parse_fixture("Object subclass: Foo\n  bar => 42.\n");
    let mut analysis = analyse_full(&module, AnalysisContext::default());

    let key: beamtalk_core::semantic_analysis::type_checker::MethodReturnKey =
        ("Foo".into(), "bar".into(), false);
    assert_eq!(
        analysis.method_return_types.get(&key),
        Some(&InferredType::known("Integer")),
        "test setup: real inference must actually infer Integer for `bar => 42`, \
         or corrupting it to String below wouldn't prove anything"
    );
    analysis
        .method_return_types
        .insert(key, InferredType::known("String"));

    // A real driver's own `lower_module_for_codegen` call, using the now-stale map.
    lower_module_for_codegen(
        &mut module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
    );
    assert!(
        matches!(
            &module.classes[0].methods[0].return_type,
            Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) if id.name == "String"
        ),
        "test setup: the module should carry the corrupted String writeback \
         before codegen ever sees it; got:\n{:?}",
        module.classes[0].methods[0].return_type
    );

    // A class stub codegen's own `add_from_beam_meta` hasn't seen before —
    // this is what actually invalidates the driver's hand-off.
    let unrelated_info = base_class_info("Unrelated", "Object");
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("foo")
            .with_class_hierarchy(vec![unrelated_info])
            .with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("{'type', 0, 'integer', []}"),
        "expected the fallback recompute to refresh the stale writeback with \
         the correct Integer answer; got:\n{code}"
    );
    assert!(
        !code.contains("{'type', 0, 'binary', []}"),
        "expected the stale String writeback to NOT survive into the \
         generated spec; got:\n{code}"
    );
}
