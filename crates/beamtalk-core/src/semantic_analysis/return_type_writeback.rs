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
use crate::semantic_analysis::type_checker::{InferredType, infer_method_return_types};
use crate::source_analysis::Span;
use ecow::EcoString;

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
pub fn apply_return_type_writeback(module: &mut Module, hierarchy: &ClassHierarchy) {
    let inferred = infer_method_return_types(module, hierarchy);

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;

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

    #[test]
    fn writeback_sets_return_type_for_integer_method() {
        let src = "Object subclass: Foo\n  bar => 42";
        let mut module = parse_module(src);
        let hierarchy = build_hierarchy(&module);
        apply_return_type_writeback(&mut module, &hierarchy);
        let return_type = &module.classes[0].methods[0].return_type;
        assert!(
            return_type.is_some(),
            "Expected writeback to set return_type for Integer method, got None"
        );
    }

    #[test]
    fn writeback_preserves_explicit_annotation() {
        let src = "Object subclass: Foo\n  bar -> Integer => 42";
        let mut module = parse_module(src);
        let hierarchy = build_hierarchy(&module);
        let original = module.classes[0].methods[0].return_type.clone();
        assert!(
            original.is_some(),
            "Method should already have an explicit annotation"
        );
        apply_return_type_writeback(&mut module, &hierarchy);
        assert_eq!(
            module.classes[0].methods[0].return_type, original,
            "Writeback should not overwrite an explicit type annotation"
        );
    }

    #[test]
    fn writeback_does_not_set_type_for_dynamic_method() {
        // A method whose body type cannot be statically resolved stays None
        let src = "Object subclass: Foo\n  bar: x => x doSomething";
        let mut module = parse_module(src);
        let hierarchy = build_hierarchy(&module);
        apply_return_type_writeback(&mut module, &hierarchy);
        let return_type = &module.classes[0].methods[0].return_type;
        assert!(
            return_type.is_none(),
            "Dynamic method should not get writeback, got: {return_type:?}"
        );
    }
}
