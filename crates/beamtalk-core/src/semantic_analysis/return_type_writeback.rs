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

use crate::ast::{Module, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::infer_method_return_types;

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
            if let Some(class_name) = inferred.get(&key) {
                method.return_type =
                    Some(TypeAnnotation::simple(class_name.clone(), method.span));
            }
        }

        for method in &mut class.class_methods {
            if method.return_type.is_some() {
                continue;
            }
            let key = (class.name.name.clone(), method.selector.name(), true);
            if let Some(class_name) = inferred.get(&key) {
                method.return_type =
                    Some(TypeAnnotation::simple(class_name.clone(), method.span));
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
        if let Some(class_name) = inferred.get(&key) {
            standalone.method.return_type =
                Some(TypeAnnotation::simple(class_name.clone(), standalone.method.span));
        }
    }
}
