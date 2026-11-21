// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Coverage tests for uncovered branches in `identifier_lookup.rs`:
//! type-annotation variants (`Union`, `Generic`, `Difference`, `Intersection`,
//! `ClassOf`, `Shape`), `Pattern::Type`, `Pattern::Map`, `Pattern::Binary`,
//! the parameter-annotation path in `find_identifier_in_method_signature_and_body`,
//! and the `collect_identifiers` expression variants (`Cascade`, `Match`,
//! `StringInterpolation`, `BinaryConstruction`, `Return`, `FieldAccess`).

use super::common::*;

// ── Type annotation variants (via goto_definition) ───────────────────────────

/// `TypeAnnotation::Union` — cursor on the second member navigates to its file.
#[test]
fn type_annotation_union() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let string_file = Utf8PathBuf::from("string.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(integer_file, "sealed Number subclass: Integer".to_string());
    service.update_file(
        string_file.clone(),
        "sealed Collection subclass: String".to_string(),
    );
    // `  bar -> Integer | String => 0`
    //  0         1         2
    //  0123456789012345678901234
    //  "String" starts at col 19; col 21 is inside "String".
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Integer | String => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 21));
    assert!(
        def.is_some(),
        "goto-def on union second member returned None"
    );
    assert_eq!(def.unwrap().file, string_file);
}

/// `TypeAnnotation::Generic` — cursor on the type *parameter* navigates to its file.
#[test]
fn type_annotation_generic_parameter() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let collection_file = Utf8PathBuf::from("collection.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(
        integer_file.clone(),
        "sealed Number subclass: Integer".to_string(),
    );
    service.update_file(
        collection_file,
        "abstract Object subclass: Collection".to_string(),
    );
    // `  bar -> Collection(Integer) => 0`
    //  0         1         2         3
    //  01234567890123456789012345678
    //  "Collection" = cols 9-18; "(" = 19; "Integer" starts at col 20; col 23 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Collection(Integer) => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 23));
    assert!(
        def.is_some(),
        "goto-def on generic type parameter returned None"
    );
    assert_eq!(def.unwrap().file, integer_file);
}

/// `TypeAnnotation::Generic` — cursor on the *base* name navigates to its file.
#[test]
fn type_annotation_generic_base() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let collection_file = Utf8PathBuf::from("collection.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(integer_file, "sealed Number subclass: Integer".to_string());
    service.update_file(
        collection_file.clone(),
        "abstract Object subclass: Collection".to_string(),
    );
    // "Collection" starts at col 9; col 12 is inside "Collection".
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Collection(Integer) => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 12));
    assert!(def.is_some(), "goto-def on generic base name returned None");
    assert_eq!(def.unwrap().file, collection_file);
}

/// `TypeAnnotation::Difference` — cursor on the *excluded* member navigates to its file.
#[test]
fn type_annotation_difference_excluded() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let string_file = Utf8PathBuf::from("string.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(integer_file, "sealed Number subclass: Integer".to_string());
    service.update_file(
        string_file.clone(),
        "sealed Collection subclass: String".to_string(),
    );
    // `  bar -> Integer \ String => 0`
    //  0         1         2
    //  012345678901234567890123456
    //  "Integer" = cols 9-15; " \ " = 16-18; "String" starts at col 19; col 21 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Integer \\ String => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 21));
    assert!(
        def.is_some(),
        "goto-def on difference excluded member returned None"
    );
    assert_eq!(def.unwrap().file, string_file);
}

/// `TypeAnnotation::Intersection` — cursor on the right member navigates to its file.
#[test]
fn type_annotation_intersection_right() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let comparable_file = Utf8PathBuf::from("comparable.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(integer_file, "sealed Number subclass: Integer".to_string());
    service.update_file(
        comparable_file.clone(),
        "Object subclass: Comparable".to_string(),
    );
    // `  bar -> Integer & Comparable => 0`
    //  0         1         2         3
    //  01234567890123456789012345678
    //  "Integer" = cols 9-15; " & " = 16-18; "Comparable" starts at col 19; col 22 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Integer & Comparable => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 22));
    assert!(
        def.is_some(),
        "goto-def on intersection right member returned None"
    );
    assert_eq!(def.unwrap().file, comparable_file);
}

/// `TypeAnnotation::ClassOf` — cursor on the class name inside `C class` navigates to `C`.
#[test]
fn type_annotation_class_of() {
    let mut service = SimpleLanguageService::new();
    let actor_file = Utf8PathBuf::from("actor.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(
        actor_file.clone(),
        "abstract Object subclass: Actor".to_string(),
    );
    // `  bar -> Actor class => Actor new`
    //  0         1         2         3
    //  "Actor" starts at col 9; col 11 is inside "Actor".
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> Actor class => Actor new".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 11));
    assert!(
        def.is_some(),
        "goto-def on class-of type annotation returned None"
    );
    assert_eq!(def.unwrap().file, actor_file);
}

/// `TypeAnnotation::Shape` — cursor on a field's value type navigates to that class.
#[test]
fn type_annotation_shape_field_value() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(
        integer_file.clone(),
        "sealed Number subclass: Integer".to_string(),
    );
    // `  bar -> #{#count => Integer} => 0`
    //  0         1         2         3
    //  01234567890123456789012345678
    //  "  bar -> #{#count => " = 21 chars; "Integer" starts at col 21; col 24 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar -> #{#count => Integer} => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 24));
    assert!(
        def.is_some(),
        "goto-def on shape field value type returned None"
    );
    assert_eq!(def.unwrap().file, integer_file);
}

/// `find_identifier_in_method_signature_and_body` — parameter annotation path.
/// Cursor on a parameter's type annotation navigates to that class's definition
/// (exercises the parameter-loop branch at lines 415-422).
#[test]
fn type_annotation_parameter_type() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("integer.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(
        integer_file.clone(),
        "sealed Number subclass: Integer".to_string(),
    );
    // `  bar: x :: Integer => 0`
    //  0         1         2
    //  012345678901234567890123
    //  "  bar: x :: " = 12 chars; "Integer" starts at col 12; col 14 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x :: Integer => 0".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(1, 14));
    assert!(
        def.is_some(),
        "goto-def on parameter type annotation returned None"
    );
    assert_eq!(def.unwrap().file, integer_file);
}

// ── Pattern variants (via goto_definition) ───────────────────────────────────

/// `Pattern::Type` — cursor on the class name in a type pattern navigates to it.
#[test]
fn pattern_type_class_name() {
    let mut service = SimpleLanguageService::new();
    let string_file = Utf8PathBuf::from("string.bt");
    let foo_file = Utf8PathBuf::from("foo.bt");

    service.update_file(
        string_file.clone(),
        "sealed Collection subclass: String".to_string(),
    );
    // Line 2: `    x match: [path :: String -> path; _ -> 0]`
    //  0         1         2         3
    //  0123456789012345678901234567890
    //  "    x match: [path :: " = 22 chars; "String" starts at col 22; col 24 inside.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  classify: x =>\n    x match: [path :: String -> path; _ -> 0]"
            .to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(2, 24));
    assert!(
        def.is_some(),
        "goto-def on type-pattern class name returned None"
    );
    assert_eq!(def.unwrap().file, string_file);
}

/// `Pattern::Map` — goto-definition with the cursor on the binding variable
/// *inside* a map pattern enters the `Pattern::Map` branch (even though a
/// binding is not a navigable class reference and the result is `None`).
#[test]
fn pattern_map_binding_in_value_is_not_navigable() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x match: [#{#port => binding} -> binding; _ -> 0]`
    //  0         1         2         3
    //  0123456789012345678901234567890123456789
    //  "    x match: [#{#port => " = 25 chars; "binding" starts at col 25.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  classify: x =>\n    x match: [#{#port => binding} -> binding; _ -> 0]"
            .to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(2, 25));
    assert!(
        def.is_none(),
        "goto-def on a map-pattern binding variable should return None, got: {def:?}"
    );
}

/// `Pattern::Binary` — goto-definition with the cursor on the binding inside
/// a binary pattern enters the `Pattern::Binary` branch (bindings are not
/// navigable class references, so the result is `None`).
#[test]
fn pattern_binary_binding_is_not_navigable() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x match: [<<v:4>> -> v; _ -> 0]`
    //  0         1
    //  01234567890123456789
    //  "    x match: [<<" = 16 chars; "v" at col 16.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  classify: x =>\n    x match: [<<v:4>> -> v; _ -> 0]".to_string(),
    );

    let def = service.goto_definition(&foo_file, Position::new(2, 16));
    assert!(
        def.is_none(),
        "goto-def on a binary-pattern binding should return None, got: {def:?}"
    );
}

// ── collect_identifiers variants (via find_references) ───────────────────────

/// `Expression::Cascade` branch in `collect_identifiers` — the cascade
/// receiver is found when scanning a method body for references.
#[test]
fn collect_identifiers_cascade() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x increment; decrement`  — x (receiver) is at col 4.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x increment; decrement".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    assert!(
        !refs.is_empty(),
        "expected at least one reference to `x` via cascade receiver, got: {refs:?}"
    );
}

/// `Expression::Match` branch in `collect_identifiers` — identifiers in the
/// match value and arm bodies are both collected.
#[test]
fn collect_identifiers_match() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x match: [0 -> x; _ -> 0]`  — x (value) is at col 4.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x match: [0 -> x; _ -> 0]".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    // `x` appears as the match value and again inside the first arm body.
    assert!(
        refs.len() >= 2,
        "expected at least 2 references to `x` via match, got: {refs:?}"
    );
}

/// `Expression::StringInterpolation` branch in `collect_identifiers` —
/// an identifier embedded inside a string interpolation is collected.
#[test]
fn collect_identifiers_string_interpolation() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x add: "result: {x}"`  — x (receiver) at col 4;
    // the string arg contains a second reference via interpolation.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x add: \"result: {x}\"".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    assert!(
        refs.len() >= 2,
        "expected at least 2 references to `x` (receiver + string interpolation), got: {refs:?}"
    );
}

/// `Expression::BinaryConstruction` branch in `collect_identifiers` —
/// a variable used as a binary segment value is collected.
#[test]
fn collect_identifiers_binary_construction() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x add: <<x:8>>`  — x (receiver) at col 4;
    // the binary construction arg contains a second reference.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x add: <<x:8>>".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    assert!(
        refs.len() >= 2,
        "expected at least 2 references to `x` (receiver + binary segment), got: {refs:?}"
    );
}

/// `Expression::Return` branch in `collect_identifiers` — a non-local return
/// (`^x`) inside a block is traversed when scanning references.
#[test]
fn collect_identifiers_return_in_block() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x add: ([^x] value)`  — x (receiver) at col 4;
    // the block arg contains a non-local return whose value is a second reference.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x add: ([^x] value)".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    assert!(
        refs.len() >= 2,
        "expected at least 2 references to `x` (receiver + return in block), got: {refs:?}"
    );
}

/// `Expression::FieldAccess` branch in `collect_identifiers` — the field name
/// `x` in a `self.x` access is collected when scanning for `x`.
#[test]
fn collect_identifiers_field_access() {
    let mut service = SimpleLanguageService::new();
    let foo_file = Utf8PathBuf::from("foo.bt");

    // Line 2: `    x add: self.x`  — x (receiver) at col 4;
    // the `self.x` arg contributes a second reference via the field name.
    service.update_file(
        foo_file.clone(),
        "Object subclass: Foo\n  bar: x =>\n    x add: self.x".to_string(),
    );

    let refs = service.find_references(&foo_file, Position::new(2, 4));
    assert!(
        refs.len() >= 2,
        "expected at least 2 references to `x` (receiver + field access), got: {refs:?}"
    );
}
