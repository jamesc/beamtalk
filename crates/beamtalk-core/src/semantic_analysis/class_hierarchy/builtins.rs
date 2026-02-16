// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in class definitions for the class hierarchy.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module defines the built-in classes (`ProtoObject`, `Object`, `Actor`,
//! `Integer`, `String`, etc.) that form the foundation of the Beamtalk type
//! hierarchy. These are registered before any user-defined classes.
//!
//! Most class definitions are auto-generated from `lib/*.bt` by `beamtalk build-stdlib`.
//! Only runtime-only classes without `.bt` source files (e.g., `Future`) are
//! defined manually here.

use super::ClassInfo;
#[cfg(test)]
use super::MethodInfo;
#[cfg(test)]
use crate::ast::MethodKind;
use ecow::EcoString;
use std::collections::HashMap;

// Auto-generated stdlib class definitions from lib/*.bt
#[path = "generated_builtins.rs"]
mod generated;

/// Create a built-in method info (always primary, never sealed).
#[cfg(test)]
pub(super) fn builtin_method(selector: &str, arity: usize, defined_in: &str) -> MethodInfo {
    MethodInfo {
        selector: selector.into(),
        arity,
        kind: MethodKind::Primary,
        defined_in: defined_in.into(),
        is_sealed: false,
        return_type: None,
    }
}

/// Returns true if the given class name is a built-in class.
///
/// This is a fast O(1) check using a static set, suitable for hot paths
/// like `merge()` and `remove_classes()`.
pub(super) fn is_builtin_class(name: &str) -> bool {
    // Runtime-only classes (no lib/*.bt source file)
    name == "Future" || generated::is_generated_builtin_class(name)
}

/// Returns all built-in class definitions.
///
/// Combines auto-generated definitions from `lib/*.bt` with runtime-only
/// classes that have no source file (e.g., `Future`).
pub(super) fn builtin_classes() -> HashMap<EcoString, ClassInfo> {
    let mut classes = generated::generated_builtin_classes();

    // Future — runtime-only class (no lib/Future.bt exists).
    // If lib/Future.bt is ever created, remove this manual entry and let
    // the generated code handle it.
    classes.insert(
        "Future".into(),
        ClassInfo {
            name: "Future".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            is_typed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    classes
}
