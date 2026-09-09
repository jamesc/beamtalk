// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-hierarchy queries (`type_hierarchy_prepare_at`, `supertypes_of`,
//! `subtypes_of`) and protocol/class shadowing on the type-hierarchy
//! path.

use super::common::*;

// ---------- type hierarchy ----------

#[test]
fn type_hierarchy_prepare_at_resolves_class_name_on_subclass_clause() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("hierarchy.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Foo\nFoo subclass: Bar\n".to_string(),
    );

    // Line 1 = "Foo subclass: Bar"; col 1 is on the `Foo` token.
    let result = service.type_hierarchy_prepare_at(&file, Position::new(1, 1));
    let (name, loc) = result.expect("Some for cursor on class reference");
    assert_eq!(name.as_str(), "Foo");
    let loc = loc.expect("Foo is declared in the same file");
    assert_eq!(loc.file, file);
    // The declaration span is the `Foo` token on line 0 (col 17 inside
    // "Object subclass: Foo"). We only assert the file here; the span
    // exact-offset is exercised by `find_class_declaration_location`'s
    // direct call site (the LSP handler test).
}

#[test]
fn type_hierarchy_prepare_at_returns_none_for_selector_cursor() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("class_with_method.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar => 42\n".to_string(),
    );

    // Cursor on the `bar` selector token (line 1, col 2..5). The
    // identifier walker rejects this (selector, not a known class
    // name), so prepare returns None.
    let result = service.type_hierarchy_prepare_at(&file, Position::new(1, 4));
    assert!(
        result.is_none(),
        "expected None for selector cursor, got {result:?}"
    );
}

#[test]
fn supertypes_of_returns_chain_for_user_class() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("chain.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Foo\nFoo subclass: Bar\n".to_string(),
    );

    let supers = service.supertypes_of("Bar");
    let names: Vec<String> = supers.iter().map(|(n, _)| n.to_string()).collect();
    assert_eq!(names, vec!["Foo", "Object", "ProtoObject"]);

    // `Foo` is declared in this file → should resolve.
    let foo_row = supers
        .iter()
        .find(|(n, _)| n.as_str() == "Foo")
        .expect("Foo row present");
    assert!(foo_row.1.is_some(), "Foo's declaration must be indexed");
    // Object / ProtoObject are builtins with no indexed source → None.
    let object_row = supers
        .iter()
        .find(|(n, _)| n.as_str() == "Object")
        .expect("Object row present");
    assert!(
        object_row.1.is_none(),
        "Object should have no indexed declaration in this test setup"
    );
}

#[test]
fn subtypes_of_returns_descendants_in_bfs_order() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("descendants.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Foo\n\
         Foo subclass: Bar\n\
         Foo subclass: Baz\n\
         Bar subclass: Qux\n"
            .to_string(),
    );

    let subs = service.subtypes_of("Foo");
    let names: Vec<&str> = subs.iter().map(|(n, _)| n.as_str()).collect();
    assert_eq!(names.len(), 3, "expected 3 descendants, got {names:?}");
    assert!(names.contains(&"Bar"));
    assert!(names.contains(&"Baz"));
    assert!(names.contains(&"Qux"));
    // BFS: Qux (grandchild) must come after Bar (its parent).
    let bar_pos = names.iter().position(|n| *n == "Bar").unwrap();
    let qux_pos = names.iter().position(|n| *n == "Qux").unwrap();
    assert!(qux_pos > bar_pos);
    // The receiver itself is not included.
    assert!(!names.contains(&"Foo"));
}

#[test]
fn subtypes_of_returns_empty_for_class_with_no_children() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("childless.bt");
    service.update_file(file.clone(), "Object subclass: Foo\n".to_string());
    let subs = service.subtypes_of("Foo");
    assert!(subs.is_empty());
}

// ---------- protocol/class shadowing on the type-hierarchy path ----------

#[test]
fn type_hierarchy_prepare_at_prefers_class_over_same_named_protocol() {
    // A name defined as both a real class and a synthetic
    // protocol across separate files must resolve to the class
    // on the type-hierarchy path, mirroring goto-definition — resolution
    // must not depend on `HashMap` iteration order and land on the
    // protocol header instead.
    let mut service = SimpleLanguageService::new();
    let class_file = Utf8PathBuf::from("real_class.bt");
    let protocol_file = Utf8PathBuf::from("protocol_foo.bt");
    let ref_file = Utf8PathBuf::from("uses_foo.bt");

    service.update_file(
        class_file.clone(),
        "Object subclass: Foo\n  bar => 1\n".to_string(),
    );
    service.update_file(
        protocol_file.clone(),
        "Protocol define: Foo\n  baz -> Integer\n".to_string(),
    );
    // `Foo subclass: Sub` gives us a `Foo` reference token to put the
    // cursor on (col 1 lands inside the `Foo` identifier).
    service.update_file(ref_file.clone(), "Foo subclass: Sub\n".to_string());

    let (name, loc) = service
        .type_hierarchy_prepare_at(&ref_file, Position::new(0, 1))
        .expect("Some for cursor on the Foo class reference");
    assert_eq!(name.as_str(), "Foo");
    let loc = loc.expect("Foo's real class declaration is indexed");
    assert_eq!(
        loc.file, class_file,
        "type hierarchy must resolve to the real class, not the protocol header"
    );
}
