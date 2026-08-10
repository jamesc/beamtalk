// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pre-codegen AST lowering (BT-3125).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Bundles the three writeback passes that must run on the AST *after*
//! analysis has produced a [`ClassHierarchy`](crate::semantic_analysis::class_hierarchy::ClassHierarchy)
//! and inferred method return types, but *before* codegen reads
//! `MethodDefinition.return_type` / `ClassDefinition.class_kind` /
//! `ClassDefinition.supervisor_kind`:
//!
//! - [`apply_return_type_writeback_from_map`](crate::semantic_analysis::return_type_writeback::apply_return_type_writeback_from_map)
//! - [`apply_supervisor_kind_writeback`](crate::semantic_analysis::supervisor_kind_writeback::apply_supervisor_kind_writeback)
//! - [`apply_class_kind_writeback`](crate::semantic_analysis::class_kind_writeback::apply_class_kind_writeback)
//!
//! Each of those is already a pure application of already-computed data — no
//! inference or hierarchy construction happens in this module either. What
//! BT-3125 changes is *who schedules* the trio: previously
//! `generate_module_with_warnings` called all three itself, on a clone it
//! made of the caller's module, every single codegen invocation. Now a
//! driver that has already run [`analyse_full`](crate::semantic_analysis::analyse_full)
//! calls [`lower_module_for_codegen`] once, directly on its own
//! (already-owned, mutable) module, before ever calling `generate_module` —
//! so codegen receives an already-prepared AST and, when the driver's
//! [`AnalysisResult`](crate::semantic_analysis::AnalysisResult) is still
//! trustworthy at that point (see `CodegenOptions::with_analysis`'s doc),
//! does not need to repeat the work.
//!
//! Codegen's own self-sufficient path (no `AnalysisResult` handed off — unit
//! tests, ad-hoc codegen, REPL trace mode) still calls this same function
//! internally, on its own clone, exactly as before — the seam moved, the
//! writeback semantics did not.

use crate::ast::Module;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::class_kind_writeback::apply_class_kind_writeback;
use crate::semantic_analysis::return_type_writeback::apply_return_type_writeback_from_map;
use crate::semantic_analysis::supervisor_kind_writeback::apply_supervisor_kind_writeback;
use crate::semantic_analysis::type_checker::{InferredType, MethodReturnKey};
use std::collections::HashMap;

/// Prepares a module's AST for codegen by applying every pre-codegen
/// writeback pass against already-computed analysis outputs.
///
/// This is a mechanical application of `hierarchy` and `method_return_types`
/// — it never builds a hierarchy or runs type inference itself. Callers that
/// already have a full [`AnalysisResult`](crate::semantic_analysis::AnalysisResult)
/// (from [`analyse_full`](crate::semantic_analysis::analyse_full)) should
/// pass `&analysis.class_hierarchy` and `&analysis.method_return_types`.
///
/// # Ordering
///
/// Must run **after** the class hierarchy is built and method return types
/// are inferred, and **before** codegen reads any of the three fields this
/// writes: `MethodDefinition.return_type`, `ClassDefinition.supervisor_kind`,
/// `ClassDefinition.class_kind`.
#[allow(clippy::implicit_hasher)] // concrete HashMap (matches AnalysisResult::method_return_types) is simpler for callers
pub fn lower_module_for_codegen(
    module: &mut Module,
    hierarchy: &ClassHierarchy,
    method_return_types: &HashMap<MethodReturnKey, InferredType>,
) {
    // BT-1005: Writeback inferred return types into the AST so unannotated
    // methods appear in the emitted `method_return_types` map.
    apply_return_type_writeback_from_map(module, method_return_types);
    // BT-1218: Writeback supervisor_kind for Supervisor/DynamicSupervisor subclasses.
    apply_supervisor_kind_writeback(module, hierarchy);
    // BT-1534: Correct class_kind for indirect Value/Actor subclasses.
    // E.g. `TestCase subclass: MyTest` gets ClassKind::Object from the parser
    // (TestCase is not literally "Value"/"Actor"), but needs ClassKind::Value
    // so codegen generates auto-slot methods (withX: setters).
    apply_class_kind_writeback(module, hierarchy);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ClassKind, SupervisorKind};

    fn parse_module(src: &str) -> Module {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        let parse_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == crate::source_analysis::Severity::Error)
            .collect();
        assert!(
            parse_errors.is_empty(),
            "Test fixture failed to parse cleanly: {parse_errors:?}"
        );
        module
    }

    fn build_hierarchy(module: &Module) -> ClassHierarchy {
        let (result, diagnostics) = ClassHierarchy::build(module);
        assert!(
            diagnostics
                .iter()
                .all(|d| d.severity != crate::source_analysis::Severity::Error),
            "Hierarchy build produced errors: {diagnostics:?}"
        );
        result.expect("ClassHierarchy::build failed for test fixture")
    }

    /// A single call applies all three writeback passes: inferred return
    /// type (a `Supervisor subclass:`'s method), `supervisor_kind` (the same
    /// class), and the indirect-Value-subclass `class_kind` fix (`TestCase
    /// subclass:` — the parser sees `ClassKind::Object` since `TestCase`
    /// isn't literally `Value`, but `TestCase` itself indirectly inherits
    /// from `Value`).
    #[test]
    fn applies_all_three_writebacks() {
        let src =
            "Supervisor subclass: WebApp\n  bar => 42\n\nTestCase subclass: MyTest\n  baz => 1";
        let mut module = parse_module(src);
        let hierarchy = build_hierarchy(&module);
        let method_return_types = crate::semantic_analysis::type_checker::infer_method_return_types(
            &module, &hierarchy, None,
        );
        lower_module_for_codegen(&mut module, &hierarchy, &method_return_types);

        let web_app = &module.classes[0];
        assert!(
            web_app.methods[0].return_type.is_some(),
            "Expected return-type writeback to run"
        );
        assert_eq!(
            web_app.supervisor_kind,
            Some(SupervisorKind::Static),
            "Expected supervisor_kind writeback to run"
        );

        let my_test = &module.classes[1];
        assert_eq!(
            my_test.class_kind,
            ClassKind::Value,
            "Expected class_kind writeback to correct the indirect Value subclass"
        );
    }

    /// Never builds a hierarchy or runs inference itself — an empty
    /// `method_return_types` map (as if inference found nothing to infer)
    /// leaves every method's `return_type` untouched.
    #[test]
    fn does_not_infer_when_map_is_empty() {
        let src = "Object subclass: Foo\n  bar => 42";
        let mut module = parse_module(src);
        let hierarchy = build_hierarchy(&module);
        lower_module_for_codegen(&mut module, &hierarchy, &HashMap::new());
        assert!(
            module.classes[0].methods[0].return_type.is_none(),
            "Expected no writeback without a precomputed map entry"
        );
    }
}
