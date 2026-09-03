// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compile-time return-type writeback pass (BT-1005, ADR 0045 Phase 1b).
//!
//! **DDD Context:** Semantic Analysis
//!
//! After the `TypeChecker` infers method body types, this pass writes the
//! inferred types back into `MethodDefinition.return_type` for methods that
//! have no explicit annotation and an `InferredType::Known` body result.
//!
//! This allows the codegen to emit `method_return_types` entries for
//! unannotated user-defined methods, enabling chain-based REPL completion
//! without requiring explicit `-> ClassName` annotations on every method.
//!
//! **Conservative scope**: Only `InferredType::Known(class_name)` results
//! are written back. `Dynamic` and complex inferred types are left as `None`.
//! Primitive methods (`@primitive`) are excluded.
//!
//! **References:**
//! - `docs/ADR/0045-repl-expression-completion-type-inference.md` (Phase 1b)
//! - `TypeChecker::infer_method_return_types`
//! - `MethodDefinition.return_type`

use crate::ast::{Identifier, Module, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::{
    InferredType, MethodReturnKey, NativeTypeRegistry, infer_method_return_types,
};
use crate::source_analysis::Span;
use ecow::EcoString;
use std::collections::HashMap;

/// Build a `TypeAnnotation` from an `InferredType` for AST writeback.
///
/// `BT-2022` + `CodeRabbit` on PR #2059: returns a `TypeAnnotation::Generic` with
/// recursive parameters when the inferred type carries `type_args`, so an
/// inferred `List(String)` writes back as `-> List(String)` rather than the
/// erased `-> List`. Cross-module consumers (codegen, language service) read
/// `MethodDefinition.return_type`, so dropping `type_args` here would
/// reintroduce the type-arg loss the cache fix eliminated.
///
/// Returns `None` for `Dynamic` and `Union` — those don't have a single
/// canonical annotation and shouldn't be written back.
fn writeback_annotation(ty: &InferredType, span: Span) -> Option<TypeAnnotation> {
    match ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } if type_args.is_empty() => Some(TypeAnnotation::simple(class_name.clone(), span)),
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => Some(TypeAnnotation::Generic {
            base: Identifier {
                name: class_name.clone(),
                span,
            },
            parameters: type_args
                .iter()
                .map(|arg| writeback_annotation(arg, span))
                .collect::<Option<Vec<_>>>()?,
            span,
        }),
        InferredType::Never => Some(TypeAnnotation::simple(EcoString::from("Never"), span)),
        _ => None,
    }
}

/// Writes inferred return types back into `MethodDefinition.return_type` for
/// unannotated methods where body inference resolves to a known class name.
///
/// This pass must run **after** the `TypeChecker` completes (so divergence
/// diagnostics from `check_return_type` are unaffected) and **before** codegen
/// (so the emitted `method_return_types` map contains inferred types).
///
/// # Arguments
///
/// * `module` - Mutable AST module to update in place.
/// * `hierarchy` - Class hierarchy used for type inference.
/// * `native_type_registry` - BT-2887: optional FFI type registry (ADR 0075)
///   so methods whose body type is inferred purely via an FFI call (e.g.
///   `foo => Erlang lists reverse: x`) get their return type written back too.
///   `None` preserves the previous registry-blind behaviour.
pub fn apply_return_type_writeback(
    module: &mut Module,
    hierarchy: &ClassHierarchy,
    native_type_registry: Option<&NativeTypeRegistry>,
) {
    let inferred = infer_method_return_types(module, hierarchy, native_type_registry);
    apply_return_type_writeback_from_map(module, &inferred);
}

/// [`apply_return_type_writeback`], given an already-computed inferred-return-types
/// map instead of running [`infer_method_return_types`] itself.
///
/// BT-3123: used by codegen when a driver hands off an [`AnalysisResult`](crate::semantic_analysis::AnalysisResult)
/// whose `method_return_types` field was populated by the same [`TypeChecker`](crate::semantic_analysis::type_checker::TypeChecker)
/// pass that already ran for diagnostics — avoids a second, full type-checking
/// pass over the module purely to re-derive the same map.
#[allow(clippy::implicit_hasher)] // concrete HashMap (matches AnalysisResult::method_return_types) is simpler for callers
pub fn apply_return_type_writeback_from_map(
    module: &mut Module,
    inferred: &HashMap<MethodReturnKey, InferredType>,
) {
    for class in &mut module.classes {
        for method in &mut class.methods {
            if method.return_type.is_some() {
                continue;
            }
            let key = (class.name.name.clone(), method.selector.name(), false);
            if let Some(inferred_ty) = inferred.get(&key) {
                if let Some(annotation) = writeback_annotation(inferred_ty, method.span) {
                    method.return_type = Some(annotation);
                }
            }
        }

        for method in &mut class.class_methods {
            if method.return_type.is_some() {
                continue;
            }
            let key = (class.name.name.clone(), method.selector.name(), true);
            if let Some(inferred_ty) = inferred.get(&key) {
                if let Some(annotation) = writeback_annotation(inferred_ty, method.span) {
                    method.return_type = Some(annotation);
                }
            }
        }
    }

    for standalone in &mut module.method_definitions {
        if standalone.method.return_type.is_some() {
            continue;
        }
        let key = (
            standalone.class_name.name.clone(),
            standalone.method.selector.name(),
            standalone.is_class_method,
        );
        if let Some(inferred_ty) = inferred.get(&key) {
            if let Some(annotation) = writeback_annotation(inferred_ty, standalone.method.span) {
                standalone.method.return_type = Some(annotation);
            }
        }
    }
}

/// Undoes a prior [`apply_return_type_writeback_from_map`] call for exactly
/// the methods present in `written_by`, resetting their `return_type` back
/// to `None`.
///
/// BT-3125: codegen's "untrusted hand-off" fallback (`generate_module_with_warnings`)
/// re-infers return types against a fuller, cross-file-enriched hierarchy when a
/// driver's own `lower_module_for_codegen` call (against a narrower hierarchy) may be
/// stale. But by the time that fallback runs, the driver has *already* written the
/// narrower inference's answers into `module` — and both `infer_method_return_types`
/// (via `resolve_self_delegate_return_type`, which trusts an already-populated
/// `return_type` as a declared annotation) and `apply_return_type_writeback_from_map`
/// only ever touch a method whose `return_type` is still `None`, to protect genuine
/// user annotations. Left alone, every method the narrower pass already answered would
/// silently keep that stale answer instead of being refreshed.
///
/// `written_by` must be the exact map the driver's own writeback used (i.e.
/// [`AnalysisResult::method_return_types`](crate::semantic_analysis::AnalysisResult::method_return_types)) —
/// every key in it was, by construction, written by inference rather than a user, so
/// clearing exactly those keys can never discard a real declared annotation.
#[allow(clippy::implicit_hasher)] // concrete HashMap (matches AnalysisResult::method_return_types) is simpler for callers
pub fn clear_return_type_writeback_for_keys(
    module: &mut Module,
    written_by: &HashMap<MethodReturnKey, InferredType>,
) {
    for class in &mut module.classes {
        for method in &mut class.methods {
            let key = (class.name.name.clone(), method.selector.name(), false);
            clear_return_type_writeback_for_key(method, &key, written_by);
        }
        for method in &mut class.class_methods {
            let key = (class.name.name.clone(), method.selector.name(), true);
            clear_return_type_writeback_for_key(method, &key, written_by);
        }
    }

    for standalone in &mut module.method_definitions {
        let key = (
            standalone.class_name.name.clone(),
            standalone.method.selector.name(),
            standalone.is_class_method,
        );
        clear_return_type_writeback_for_key(&mut standalone.method, &key, written_by);
    }
}

/// Single-method core of [`clear_return_type_writeback_for_keys`]: resets
/// `method.return_type` to `None` if — and only if — `key` (this method's
/// `(class_name, selector, is_class_method)` identity) is present in
/// `written_by`, i.e. was set by [`apply_return_type_writeback_from_map`]
/// rather than typed by the user.
///
/// BT-3249: also called directly by codegen's `extract_method_source`
/// (`crates/beamtalk-codegen/src/core_erlang/gen_server/methods.rs`) on
/// a throwaway clone of a single method, so the image-resident `__source__`
/// text it bakes never carries an inferred `-> Type` annotation the user
/// never wrote — while the method's *real* `return_type` (used for codegen's
/// `method_return_types` metadata, specs, etc.) is left untouched. Without
/// this, a save → revert → re-save of a visually-unchanged buffer records a
/// spurious `ChangeLog` entry whose only diff is the inferred annotation,
/// because the `ChangeLog`'s `source_ref` is unparsed pre-writeback while the
/// browsed `__source__` was unparsed post-writeback.
#[allow(clippy::implicit_hasher)] // concrete HashMap (matches AnalysisResult::method_return_types) is simpler for callers
pub fn clear_return_type_writeback_for_key(
    method: &mut crate::ast::MethodDefinition,
    key: &MethodReturnKey,
    written_by: &HashMap<MethodReturnKey, InferredType>,
) {
    if written_by.contains_key(key) {
        method.return_type = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
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

    #[test]
    fn writeback_sets_return_type_for_integer_method() {
        let src = "Object subclass: Foo\n  bar => 42";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        apply_return_type_writeback(&mut module, &hierarchy, None);
        let return_type = &module.classes[0].methods[0].return_type;
        assert!(
            return_type.is_some(),
            "Expected writeback to set return_type for Integer method, got None"
        );
    }

    #[test]
    fn writeback_preserves_explicit_annotation() {
        let src = "Object subclass: Foo\n  bar -> Integer => 42";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        let original = module.classes[0].methods[0].return_type.clone();
        assert!(
            original.is_some(),
            "Method should already have an explicit annotation"
        );
        apply_return_type_writeback(&mut module, &hierarchy, None);
        assert_eq!(
            module.classes[0].methods[0].return_type, original,
            "Writeback should not overwrite an explicit type annotation"
        );
    }

    #[test]
    fn writeback_does_not_set_type_for_dynamic_method() {
        // A method whose body type cannot be statically resolved stays None
        let src = "Object subclass: Foo\n  bar: x => x doSomething";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);
        apply_return_type_writeback(&mut module, &hierarchy, None);
        let return_type = &module.classes[0].methods[0].return_type;
        assert!(
            return_type.is_none(),
            "Dynamic method should not get writeback, got: {return_type:?}"
        );
    }

    #[test]
    fn writeback_with_native_registry_sets_return_type_for_ffi_only_method() {
        // BT-2887: a method whose body return type is inferred purely via an
        // FFI call only writes back when a NativeTypeRegistry is supplied.
        use crate::semantic_analysis::type_checker::TypeProvenance;
        use crate::semantic_analysis::type_checker::native_type_registry::{
            FunctionSignature, ParamType,
        };

        let src = "Object subclass: Foo\n  bar: x => Erlang lists reverse: x";
        let mut module = parse_bt(src);
        let hierarchy = build_hierarchy(&module);

        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "lists",
            vec![FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            }],
        );

        apply_return_type_writeback(&mut module, &hierarchy, Some(&registry));
        let return_type = module.classes[0].methods[0].return_type.clone();
        assert!(
            return_type.is_some(),
            "FFI-inferred return type should be written back when registry is provided, got None"
        );

        // Registry-blind default (None) preserves the previous behaviour.
        let mut module_without_registry = parse_bt(src);
        apply_return_type_writeback(&mut module_without_registry, &hierarchy, None);
        assert!(
            module_without_registry.classes[0].methods[0]
                .return_type
                .is_none(),
            "Without a registry, FFI-only inference should stay registry-blind"
        );
    }
}
