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
//! Most class definitions are auto-generated from `stdlib/src/*.bt` by `beamtalk build-stdlib`.
//! Only runtime-only classes without `.bt` source files (e.g., `Future`) are
//! defined manually here. `Future` has no source file by design (BT-1057); see BT-507.

use super::ClassInfo;
#[cfg(test)]
use super::MethodInfo;
#[cfg(test)]
use crate::ast::MethodKind;
use ecow::EcoString;
use std::collections::HashMap;

// Auto-generated stdlib class definitions from stdlib/src/*.bt
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
        param_types: vec![None; arity],
        doc: None,
    }
}

/// Returns true if the given class name is a built-in class.
///
/// This is a fast O(1) check using a static set, suitable for hot paths
/// like `merge()`, `remove_classes()`, and project index filtering.
///
/// Includes both generated stdlib classes (from `stdlib/src/*.bt`) and runtime-only
/// built-ins like `Future`.
pub(super) fn is_builtin_class(name: &str) -> bool {
    // Future is the only runtime-only built-in (no stdlib/src/Future.bt source; see BT-507).
    // All other built-ins (including Value) are in the generated set.
    name == "Future" || generated::is_generated_builtin_class(name)
}

/// Returns true if the given class name has runtime shadowing protection.
///
/// Includes both generated stdlib classes and runtime-only built-ins
/// (e.g., `Future`) that are protected from user shadowing.
///
/// Used by `check_stdlib_name_shadowing` to warn when user code tries to
/// define a class with a name that shadows a protected built-in.
pub(super) fn is_runtime_protected_class(name: &str) -> bool {
    // Future is runtime-only (not in the generated set); all other protected
    // classes (including Value) are covered by is_generated_builtin_class.
    name == "Future" || generated::is_generated_builtin_class(name)
}

/// Returns all built-in class definitions.
///
/// Combines auto-generated definitions from `stdlib/src/*.bt` with runtime-only
/// classes that have no source file (e.g., `Future`).
pub(super) fn builtin_classes() -> HashMap<EcoString, ClassInfo> {
    let mut classes = generated::generated_builtin_classes();

    // Future — runtime-only class (no stdlib/src/Future.bt source).
    // The stub was removed in BT-1057; the real design is tracked in BT-507.
    // When BT-507 is resolved and a proper Future.bt is created, remove this
    // manual entry and let the generated code handle it.
    classes.insert(
        "Future".into(),
        ClassInfo {
            name: "Future".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Value is already present in the generated set (stdlib/src/Value.bt).
    // Do NOT insert it manually here — that would silently overwrite generated
    // metadata (methods, state) if Value.bt ever gains them (BT-507, ADR 0042).

    classes
}

#[cfg(test)]
mod tests {
    #[test]
    fn builtin_set_includes_future() {
        assert!(super::is_builtin_class("Future"));
    }

    #[test]
    fn builtin_set_includes_value() {
        assert!(super::is_builtin_class("Value"));
    }

    #[test]
    fn builtin_set_includes_generated_classes() {
        assert!(super::is_builtin_class("Integer"));
        assert!(super::is_builtin_class("Object"));
        assert!(super::is_builtin_class("Array"));
    }

    #[test]
    fn builtin_set_excludes_user_classes() {
        assert!(!super::is_builtin_class("MyCustomClass"));
        assert!(!super::is_builtin_class("Person"));
    }

    #[test]
    fn protected_set_includes_runtime_only_classes() {
        assert!(super::is_runtime_protected_class("Future"));
        assert!(super::is_runtime_protected_class("Value"));
    }

    #[test]
    fn protected_set_includes_generated_classes() {
        assert!(super::is_runtime_protected_class("Integer"));
        assert!(super::is_runtime_protected_class("String"));
    }

    #[test]
    fn protected_set_excludes_user_classes() {
        assert!(!super::is_runtime_protected_class("MyCustomClass"));
    }

    #[test]
    fn builtin_classes_contains_future() {
        let classes = super::builtin_classes();
        assert!(
            classes.contains_key("Future"),
            "Future should be in builtin_classes()"
        );
    }

    #[test]
    fn builtin_classes_contains_value() {
        let classes = super::builtin_classes();
        assert!(
            classes.contains_key("Value"),
            "Value should be in builtin_classes()"
        );
    }

    #[test]
    fn future_is_sealed() {
        let classes = super::builtin_classes();
        let future = &classes["Future"];
        assert!(future.is_sealed, "Future should be marked as sealed");
    }

    #[test]
    fn value_is_not_sealed() {
        let classes = super::builtin_classes();
        let value = &classes["Value"];
        assert!(!value.is_sealed, "Value should not be sealed");
    }
}
