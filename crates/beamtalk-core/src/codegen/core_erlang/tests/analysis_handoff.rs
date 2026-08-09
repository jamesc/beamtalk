// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3123: verifies that threading a driver's `AnalysisResult` into codegen
//! via `CodegenOptions::with_analysis` skips codegen's own re-derivation of
//! the class hierarchy, semantic facts, and inferred method return types —
//! the duplicate work this issue eliminates.
//!
//! Uses `#[cfg(test)]`-only thread-local call counters on the three costly
//! functions (`ClassHierarchy::build_with_options`, `compute_semantic_facts`,
//! `TypeChecker::check_module`) rather than a shared global counter, since
//! plenty of *other* tests call these functions too and may run concurrently
//! on other threads — only the delta this test's own thread accumulates is
//! meaningful. See `CHECK_MODULE_CALL_COUNT`'s doc for the full rationale.

use crate::semantic_analysis::class_hierarchy::BUILD_CALL_COUNT;
use crate::semantic_analysis::facts::COMPUTE_SEMANTIC_FACTS_CALL_COUNT;
use crate::semantic_analysis::type_checker::CHECK_MODULE_CALL_COUNT;
use crate::semantic_analysis::{AnalysisContext, analyse_full};

fn parse_fixture(src: &str) -> crate::ast::Module {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = crate::source_analysis::parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == crate::source_analysis::Severity::Error)
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

    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("greeter").with_analysis(analysis),
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

    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("greeter"),
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
) -> crate::semantic_analysis::class_hierarchy::ClassInfo {
    crate::semantic_analysis::class_hierarchy::ClassInfo {
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

    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("bar")
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
