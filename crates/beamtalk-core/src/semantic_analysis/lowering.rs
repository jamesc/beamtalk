// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pre-codegen AST lowering.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Bundles the trait-flattening step and the three writeback passes that
//! must run on the AST *after* analysis has produced a
//! [`ClassHierarchy`](crate::semantic_analysis::class_hierarchy::ClassHierarchy)
//! and inferred method return types, but *before* codegen reads
//! `MethodDefinition.return_type` / `ClassDefinition.class_kind` /
//! `ClassDefinition.supervisor_kind` / `ClassDefinition.methods`:
//!
//! - [`trait_expansion::expand_module`](crate::semantic_analysis::trait_expansion::expand_module)
//! - [`apply_return_type_writeback_from_map`](crate::semantic_analysis::return_type_writeback::apply_return_type_writeback_from_map)
//! - [`apply_supervisor_kind_writeback`](crate::semantic_analysis::supervisor_kind_writeback::apply_supervisor_kind_writeback)
//! - [`apply_class_kind_writeback`](crate::semantic_analysis::class_kind_writeback::apply_class_kind_writeback)
//!
//! The three writebacks are each a pure application of already-computed
//! data — no inference or hierarchy construction happens in this module.
//! Flattening runs first and is the one step here that isn't purely
//! "apply already-computed data": it's a second, deterministic run of the
//! *same* pass `analyse_full` already ran once (on its own internal clone,
//! to build `hierarchy`/`method_return_types`) — see the next paragraph for
//! why a second run, rather than threading that clone through, is the
//! chosen fix.
//!
//! # Closing the flattening/codegen boundary (ADR 0127 §3, BT-3590)
//!
//! `analyse_full` flattens a *clone* of the module it's given, local to
//! that call — its `AnalysisResult` (diagnostics, `ClassHierarchy`,
//! `ProtocolRegistry`, `method_return_types`) is correct, but the caller's
//! own `Module` (the one every driver hands to `generate_module` after
//! this function) was never mutated, so a `uses:` class's flattened
//! provisions never reached compiled output — see
//! [`trait_expansion`](crate::semantic_analysis::trait_expansion)'s module
//! doc for the full history of that gap. Calling `expand_module` again
//! here, directly on the driver's real module, closes it: `expand_module`
//! is a pure function of `module.classes`/`module.protocols` alone (no
//! external registry, ADR 0127 §3's module doc), so re-running it on the
//! same (unmutated-in-between) module the driver already analysed
//! reproduces byte-for-byte the same flattened methods `analyse_full`'s
//! clone produced — same selectors, same sort order, same substituted
//! signatures — which is exactly why `hierarchy`/`method_return_types`
//! (both keyed structurally by `(class, selector)`, not by AST identity)
//! still apply correctly to the methods this second run splices in.
//! `expand_module` is also idempotent on an already-flattened class (every
//! provided selector it would add is already present, so "class wins"
//! drops it the second time) — safe for every caller in this module's own
//! doc, including a caller that runs the writeback trio on an
//! already-flattened module.
//!
//! Its diagnostics are discarded here — `analyse_full`'s own run over the
//! (structurally identical) flattened clone already reported them to
//! whichever diagnostic pipeline the driver used, and every CLI/LSP driver
//! bails out on an error diagnostic before ever reaching this function, so
//! a second copy would only ever be a duplicate of one already surfaced.
//!
//! A driver that has already run [`analyse_full`](crate::semantic_analysis::analyse_full)
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
use crate::semantic_analysis::trait_expansion;
use crate::semantic_analysis::type_checker::{InferredType, MethodReturnKey};
use std::collections::HashMap;

/// Prepares a module's AST for codegen: flattens `uses:` provisions into
/// their classes' own bodies (ADR 0127 §3, BT-3590 — see this module's own
/// doc, "Closing the flattening/codegen boundary"), then applies every
/// pre-codegen writeback pass against already-computed analysis outputs.
///
/// The writeback trio is a mechanical application of `hierarchy` and
/// `method_return_types` — it never builds a hierarchy or runs type
/// inference itself. Callers that already have a full
/// [`AnalysisResult`](crate::semantic_analysis::AnalysisResult) (from
/// [`analyse_full`](crate::semantic_analysis::analyse_full)) should pass
/// `&analysis.class_hierarchy` and `&analysis.method_return_types` —
/// flattening `module` again here reproduces exactly the methods that
/// `hierarchy`/`method_return_types` already describe (see the module doc).
///
/// `external_protocols` must be the same map the driver's [`AnalysisResult`](
/// crate::semantic_analysis::AnalysisResult) carries (its own field of the
/// same name) — re-flattening `module` here only reproduces byte-for-byte
/// what `analyse_full` already did (see the module doc) when it resolves a
/// cross-file/cross-package `uses:` line against the same externally-carried
/// protocol definitions. A caller with no such analysis to hand off (codegen's
/// self-sufficient path) passes an empty map, same as it always could not
/// resolve cross-file `uses:` either.
///
/// # Ordering
///
/// Must run **after** the class hierarchy is built and method return types
/// are inferred, and **before** codegen reads any of the four fields this
/// writes: `ClassDefinition.methods` (the flattened provisions),
/// `MethodDefinition.return_type`, `ClassDefinition.supervisor_kind`,
/// `ClassDefinition.class_kind`.
#[allow(clippy::implicit_hasher)] // concrete HashMap (matches AnalysisResult::method_return_types) is simpler for callers
pub fn lower_module_for_codegen(
    module: &mut Module,
    hierarchy: &ClassHierarchy,
    method_return_types: &HashMap<MethodReturnKey, InferredType>,
    external_protocols: &HashMap<ecow::EcoString, crate::ast::ProtocolDefinition>,
) {
    // Flatten `uses:` provisions into their classes' own bodies so codegen
    // (which reads `ClassDefinition.methods` directly, with no knowledge of
    // `uses:`) emits them. Diagnostics are discarded — see the module doc's
    // "Closing the flattening/codegen boundary" for why that's safe here.
    let (_diagnostics, _origins) = trait_expansion::expand_module(module, external_protocols);
    // Writeback inferred return types into the AST so unannotated
    // methods appear in the emitted `method_return_types` map. Runs after
    // flattening so a flattened provision's own `(class, selector)` key
    // — present in `method_return_types` because `analyse_full` inferred
    // types over its own (structurally identical) flattened clone — finds
    // its freshly-spliced-in `MethodDefinition` here to write into.
    apply_return_type_writeback_from_map(module, method_return_types);
    // Writeback supervisor_kind for Supervisor/DynamicSupervisor subclasses.
    apply_supervisor_kind_writeback(module, hierarchy);
    // Correct class_kind for indirect Value/Actor subclasses.
    // E.g. `TestCase subclass: MyTest` gets ClassKind::Object from the parser
    // (TestCase is not literally "Value"/"Actor"), but needs ClassKind::Value
    // so codegen generates auto-slot methods (withX: setters).
    apply_class_kind_writeback(module, hierarchy);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ClassKind, SupervisorKind};
    use crate::test_helpers::test_support::parse_bt;

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
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        let method_return_types = crate::semantic_analysis::type_checker::infer_method_return_types(
            &module, &hierarchy, None,
        );
        lower_module_for_codegen(
            &mut module,
            &hierarchy,
            &method_return_types,
            &HashMap::new(),
        );

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

    /// BT-3590: closes the gap where `analyse_full` flattened only its own
    /// internal clone (see this module's "Closing the flattening/codegen
    /// boundary" doc) while the driver's own module — the one handed to
    /// codegen — was never mutated. A driver preparing its module here via
    /// `lower_module_for_codegen` must see the protocol's provision
    /// spliced into `ClassDefinition.methods`, or codegen never emits it.
    #[test]
    fn flattens_uses_into_the_drivers_own_module() {
        let src = "Protocol define: Describable\n  \
                       printString -> String => \"a describable thing\"\n\n\
                   Object subclass: Report\n  \
                       uses: Describable\n";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        lower_module_for_codegen(&mut module, &hierarchy, &HashMap::new(), &HashMap::new());

        let report = &module.classes[0];
        assert!(
            report
                .methods
                .iter()
                .any(|m| m.selector.name() == "printString"),
            "expected the protocol's provided `printString` to be spliced \
             into Report's own methods after lower_module_for_codegen, \
             got: {:?}",
            report
                .methods
                .iter()
                .map(|m| m.selector.name())
                .collect::<Vec<_>>()
        );
    }

    /// Calling `lower_module_for_codegen` a second time on an
    /// already-flattened module (mirroring `generate_module_with_warnings`'s
    /// own re-derive path, which can call it again on a module this
    /// function already flattened once) must not duplicate the provision —
    /// `expand_module`'s "class wins" rule already makes it idempotent
    /// (`trait_expansion`'s own module doc), this just pins that through
    /// the driver-facing entry point.
    #[test]
    fn flattening_uses_twice_does_not_duplicate_the_provision() {
        let src = "Protocol define: Describable\n  \
                       printString -> String => \"a describable thing\"\n\n\
                   Object subclass: Report\n  \
                       uses: Describable\n";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        lower_module_for_codegen(&mut module, &hierarchy, &HashMap::new(), &HashMap::new());
        lower_module_for_codegen(&mut module, &hierarchy, &HashMap::new(), &HashMap::new());

        let report = &module.classes[0];
        let count = report
            .methods
            .iter()
            .filter(|m| m.selector.name() == "printString")
            .count();
        assert_eq!(
            count, 1,
            "expected exactly one printString method, not a duplicate"
        );
    }

    /// Never builds a hierarchy or runs inference itself — an empty
    /// `method_return_types` map (as if inference found nothing to infer)
    /// leaves every method's `return_type` untouched.
    #[test]
    fn does_not_infer_when_map_is_empty() {
        let src = "Object subclass: Foo\n  bar => 42";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        lower_module_for_codegen(&mut module, &hierarchy, &HashMap::new(), &HashMap::new());
        assert!(
            module.classes[0].methods[0].return_type.is_none(),
            "Expected no writeback without a precomputed map entry"
        );
    }
}
