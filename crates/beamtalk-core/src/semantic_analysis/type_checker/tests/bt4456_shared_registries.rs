// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4456: the native-type and alias registries reach the `TypeChecker`
//! through shared `Arc`s, not a deep clone per request.

use super::super::*;
use std::sync::Arc;

#[test]
fn checker_shares_the_registries_it_is_given() {
    let natives = Arc::new(NativeTypeRegistry::new());
    let aliases = Arc::new(AliasRegistry::new());
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(Arc::clone(&natives));
    checker.set_alias_registry(Arc::clone(&aliases));
    assert!(Arc::ptr_eq(
        checker.native_type_registry.as_ref().unwrap(),
        &natives
    ));
    assert!(Arc::ptr_eq(
        checker.alias_registry.as_ref().unwrap(),
        &aliases
    ));
}

#[test]
fn infer_with_registries_does_not_retain_or_copy_the_arcs() {
    let module = crate::test_helpers::test_support::parse_source("x := 1");
    let hierarchy = ClassHierarchy::with_builtins();
    let natives = Arc::new(NativeTypeRegistry::new());
    let aliases = Arc::new(AliasRegistry::new());
    let _ = infer_types_and_returns_with_registries(
        &module,
        &hierarchy,
        Some(Arc::clone(&natives)),
        None,
        Some(Arc::clone(&aliases)),
    );
    // The checker was dropped: only the caller's handles remain.
    assert_eq!(Arc::strong_count(&natives), 1);
    assert_eq!(Arc::strong_count(&aliases), 1);
}
