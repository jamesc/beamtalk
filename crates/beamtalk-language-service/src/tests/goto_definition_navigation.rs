// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `goto_definition` navigation: cross-file classes/methods, stdlib classes,
//! superclass/type-annotation/constructor-pattern targets, receiver-context
//! resolution, and header-to-parent-override navigation.

use super::common::*;

#[test]
fn goto_definition_cross_file_class() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );
    service.update_file(file_b.clone(), "x := Foo new".to_string());

    // Go to definition of 'Foo' from file_b should find class in file_a
    let def = service.goto_definition(&file_b, Position::new(0, 5));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, file_a);
}

#[test]
fn goto_definition_cross_file_method_keyword() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  at: i put: v => v".to_string(),
    );
    // "x at: 1 put: 2" — cursor on "at:" keyword
    service.update_file(file_b.clone(), "x at: 1 put: 2".to_string());

    // Position 2 is on "at:" — should navigate to method definition in file_a
    let def = service.goto_definition(&file_b, Position::new(0, 2));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, file_a);
}

#[test]
fn goto_definition_stdlib_class() {
    let stdlib = vec![(
        Utf8PathBuf::from("stdlib/src/Counter.bt"),
        "Object subclass: Counter\n  increment => 1".to_string(),
    )];
    let index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
    let mut service = SimpleLanguageService::with_project_index(index);

    // Add the stdlib file as an open file too so cross-file lookup can find it
    service.update_file(
        Utf8PathBuf::from("stdlib/src/Counter.bt"),
        "Object subclass: Counter\n  increment => 1".to_string(),
    );

    let user_file = Utf8PathBuf::from("user.bt");
    service.update_file(user_file.clone(), "x := Counter new".to_string());

    // Go to definition of 'Counter' from user.bt should find in lib/Counter.bt
    let def = service.goto_definition(&user_file, Position::new(0, 5));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, Utf8PathBuf::from("stdlib/src/Counter.bt"));
}

#[test]
fn goto_definition_superclass_in_class_header() {
    let mut service = SimpleLanguageService::new();

    let collection_file = Utf8PathBuf::from("stdlib/src/collection.bt");
    let set_file = Utf8PathBuf::from("stdlib/src/set.bt");

    service.update_file(
        collection_file.clone(),
        "abstract Object subclass: Collection".to_string(),
    );
    service.update_file(
        set_file.clone(),
        "sealed Collection subclass: Set".to_string(),
    );

    // Cursor on "Collection" in class header.
    let def = service.goto_definition(&set_file, Position::new(0, 8));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, collection_file);
}

#[test]
fn goto_definition_type_annotation_identifier() {
    let mut service = SimpleLanguageService::new();
    let integer_file = Utf8PathBuf::from("stdlib/src/integer.bt");
    let tuple_file = Utf8PathBuf::from("stdlib/src/tuple.bt");

    service.update_file(
        integer_file.clone(),
        "sealed Number subclass: Integer".to_string(),
    );
    service.update_file(
        tuple_file.clone(),
        "sealed Collection subclass: Tuple\n  size -> Integer => 0".to_string(),
    );

    let def = service.goto_definition(&tuple_file, Position::new(1, 10));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, integer_file);
}

// Goto-definition on the class name inside a constructor
// pattern (e.g. `Result` in `Result ok: v`) must navigate to the class
// declaration.
#[test]
fn goto_definition_constructor_pattern_class_name() {
    let mut service = SimpleLanguageService::new();
    let result_file = Utf8PathBuf::from("result.bt");
    let classify_file = Utf8PathBuf::from("classify.bt");

    service.update_file(
        result_file.clone(),
        "Value subclass: Result\n  ok: v\n  error: e".to_string(),
    );
    // The match arm `Result ok: v -> v` starts on line 2, col 4.
    // `Result` occupies columns 4..10, so column 5 lands on the 'e' of
    // "Result" — inside the class identifier.
    service.update_file(
        classify_file.clone(),
        "Object subclass: Classifier\n  \
         classify: r =>\n    \
         r match: [\n      \
         Result ok: v -> v;\n      \
         Result error: _ -> 0\n    ]"
            .to_string(),
    );

    // Line 3 (0-indexed) is `      Result ok: v -> v;`; column 8
    // lands inside "Result".
    let def = service.goto_definition(&classify_file, Position::new(3, 8));
    assert!(
        def.is_some(),
        "goto-definition on constructor pattern class name returned None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, result_file);
}

// Goto-definition must also reach identifiers inside a `when:`
// guard, not just the pattern and the arm body. `references_provider`
// already walks guards; this test pins the parity in `find_identifier_in_expr`.
#[test]
fn goto_definition_inside_match_arm_guard() {
    let mut service = SimpleLanguageService::new();
    let threshold_file = Utf8PathBuf::from("Threshold.bt");
    let guarded_file = Utf8PathBuf::from("guarded.bt");

    service.update_file(
        threshold_file.clone(),
        "Object subclass: Threshold\n  limit -> Integer => 10".to_string(),
    );
    // Match arm with a guard referencing `Threshold` from the class body:
    //   r match: [
    //     v when: v > Threshold limit -> v;
    //     _ -> 0
    //   ]
    service.update_file(
        guarded_file.clone(),
        "Object subclass: Guarded\n  \
         check: r =>\n    \
         r match: [\n      \
         v when: v > Threshold limit -> v;\n      \
         _ -> 0\n    ]"
            .to_string(),
    );

    // Line 3 (0-indexed) is `      v when: v > Threshold limit -> v;`.
    // `Threshold` starts at column 17 ("      v when: v > " = 18 chars, so
    // column 18 lands on 'T').
    let def = service.goto_definition(&guarded_file, Position::new(3, 20));
    assert!(
        def.is_some(),
        "goto-definition on identifier inside match arm guard returned None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, threshold_file);
}

#[test]
fn find_references_class_cross_file() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );
    service.update_file(file_b.clone(), "x := Foo new".to_string());

    // Find references to 'Foo' from file_a — should find in both files
    let refs = service.find_references(&file_a, Position::new(0, 17));
    assert!(refs.len() >= 2);
    assert!(refs.iter().any(|r| r.file == file_a));
    assert!(refs.iter().any(|r| r.file == file_b));
}

#[test]
fn find_references_selector_cross_file() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  at: i put: v => v".to_string(),
    );
    // "x at: 1 put: 2" — cursor on "at:" keyword
    service.update_file(file_b.clone(), "x at: 1 put: 2".to_string());

    // Find references to 'at:put:' from file_b
    let refs = service.find_references(&file_b, Position::new(0, 2));
    assert!(refs.len() >= 2);
    assert!(refs.iter().any(|r| r.file == file_a));
    assert!(refs.iter().any(|r| r.file == file_b));
}

#[test]
fn goto_definition_cross_file_method_unary() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );
    // Cursor on unary selector "bar"
    service.update_file(file_b.clone(), "x bar".to_string());

    let def = service.goto_definition(&file_b, Position::new(0, 2));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, file_a);
}

#[test]
fn goto_definition_method_uses_inferred_receiver_class_context() {
    let mut service = SimpleLanguageService::new();
    let file_foo = Utf8PathBuf::from("foo.bt");
    let file_bar = Utf8PathBuf::from("bar.bt");
    let file_calls = Utf8PathBuf::from("calls.bt");

    service.update_file(
        file_foo.clone(),
        "Object subclass: Foo\n  ping => 1".to_string(),
    );
    service.update_file(
        file_bar.clone(),
        "Object subclass: Bar\n  ping => 2".to_string(),
    );
    service.update_file(
        file_calls.clone(),
        "x := Foo new\nx ping\ny := Bar new\ny ping".to_string(),
    );

    let first = service.goto_definition(&file_calls, Position::new(1, 2));
    assert!(first.is_some());
    assert_eq!(first.unwrap().file, file_foo);

    let second = service.goto_definition(&file_calls, Position::new(3, 2));
    assert!(second.is_some());
    assert_eq!(second.unwrap().file, file_bar);
}

#[test]
fn goto_definition_method_uses_receiver_class_side_context() {
    let mut service = SimpleLanguageService::new();
    let file_foo = Utf8PathBuf::from("foo.bt");
    let file_bar = Utf8PathBuf::from("bar.bt");
    let file_calls = Utf8PathBuf::from("calls.bt");

    service.update_file(
        file_foo.clone(),
        "Object subclass: Foo\n  class ping => 1".to_string(),
    );
    service.update_file(
        file_bar.clone(),
        "Object subclass: Bar\n  class ping => 2".to_string(),
    );
    service.update_file(file_calls.clone(), "Foo ping\nBar ping".to_string());

    let first = service.goto_definition(&file_calls, Position::new(0, 4));
    assert!(first.is_some());
    assert_eq!(first.unwrap().file, file_foo);

    let second = service.goto_definition(&file_calls, Position::new(1, 4));
    assert!(second.is_some());
    assert_eq!(second.unwrap().file, file_bar);
}

#[test]
fn goto_definition_method_ambiguous_without_receiver_is_deterministic() {
    let mut service = SimpleLanguageService::new();
    let file_alpha = Utf8PathBuf::from("alpha.bt");
    let file_zed = Utf8PathBuf::from("zed.bt");
    let file_calls = Utf8PathBuf::from("calls.bt");

    service.update_file(
        file_alpha.clone(),
        "Object subclass: Alpha\n  ping => 1".to_string(),
    );
    service.update_file(
        file_zed.clone(),
        "Object subclass: Zed\n  ping => 2".to_string(),
    );
    service.update_file(file_calls.clone(), "unknown ping".to_string());

    let def = service.goto_definition(&file_calls, Position::new(0, 8));
    assert!(def.is_some());
    assert_eq!(def.unwrap().file, file_alpha);
}

#[test]
fn goto_definition_method_with_self_receiver_resolves_in_class() {
    let mut service = SimpleLanguageService::new();
    let file_foo = Utf8PathBuf::from("foo.bt");

    service.update_file(
        file_foo.clone(),
        "Object subclass: Foo\n  ping => self other\n  other => 1".to_string(),
    );

    let def = service.goto_definition(&file_foo, Position::new(1, 15));
    assert!(def.is_some());
    assert_eq!(def.unwrap().file, file_foo);
}

#[test]
fn goto_definition_method_with_super_receiver_resolves_in_superclass() {
    let mut service = SimpleLanguageService::new();
    let file_hierarchy = Utf8PathBuf::from("hierarchy.bt");

    service.update_file(
        file_hierarchy.clone(),
        "Object subclass: Foo\n  other => 1\nFoo subclass: Bar\n  ping => super other".to_string(),
    );

    let def = service.goto_definition(&file_hierarchy, Position::new(3, 16));
    assert!(def.is_some());
    assert_eq!(def.unwrap().file, file_hierarchy);
}

// ── Go to Definition on method definition headers ─────────────

#[test]
fn goto_definition_from_method_header_unary_navigates_to_parent() {
    // Clicking Go to Definition on the selector in a method's
    // own definition header should navigate to the overridden parent
    // method, mirroring the header path added for find_references.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Parent
    //  1:   greet => 1
    //  2: Parent subclass: Child
    //  3:   greet => 2
    let source =
        "Object subclass: Parent\n  greet => 1\nParent subclass: Child\n  greet => 2".to_string();
    service.update_file(file.clone(), source.clone());

    // Cursor on `greet` in Child's header: line 3, col 2 (after the two
    // leading spaces).
    let def = service.goto_definition(&file, Position::new(3, 2));
    assert!(
        def.is_some(),
        "expected navigation to parent's greet, got None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    // Parent's method body lives before the `Parent subclass: Child`
    // declaration, so any span inside Parent must start before that
    // declaration's offset.
    let child_decl_offset = source.find("Parent subclass: Child").unwrap();
    assert!(
        (loc.span.start() as usize) < child_decl_offset,
        "expected Parent's method span (< {child_decl_offset}), got {}",
        loc.span.start()
    );
}

#[test]
fn goto_definition_from_method_header_binary_navigates_to_parent() {
    // Binary selectors (`+`, `-`, etc.) must also resolve.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Vec
    //  1:   + other => other
    //  2: Vec subclass: Vec3
    //  3:   + other => other
    let source = "Object subclass: Vec\n  + other => other\nVec subclass: Vec3\n  + other => other"
        .to_string();
    service.update_file(file.clone(), source.clone());

    // Cursor on `+` in Vec3's header: line 3, col 2.
    let def = service.goto_definition(&file, Position::new(3, 2));
    assert!(
        def.is_some(),
        "expected navigation to parent's `+`, got None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    let child_decl_offset = source.find("Vec subclass: Vec3").unwrap();
    assert!(
        (loc.span.start() as usize) < child_decl_offset,
        "expected Vec's `+` span (< {child_decl_offset}), got {}",
        loc.span.start()
    );
}

#[test]
fn goto_definition_from_method_header_keyword_navigates_to_parent() {
    // Keyword selectors — both keyword parts must navigate to
    // the parent definition.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Dict
    //  1:   at: k put: v => v
    //  2: Dict subclass: OrderedDict
    //  3:   at: k put: v => v
    let source = "Object subclass: Dict\n  at: k put: v => v\nDict subclass: OrderedDict\n  at: k put: v => v"
        .to_string();
    service.update_file(file.clone(), source.clone());

    let child_decl_offset = source.find("Dict subclass: OrderedDict").unwrap();

    // Cursor on `at:` in OrderedDict's header: line 3, col 2.
    let def_at = service.goto_definition(&file, Position::new(3, 2));
    assert!(def_at.is_some(), "expected navigation from `at:`, got None");
    let loc_at = def_at.unwrap();
    assert_eq!(loc_at.file, file);
    assert!(
        (loc_at.span.start() as usize) < child_decl_offset,
        "expected Dict's at:put: span (< {child_decl_offset}), got {}",
        loc_at.span.start()
    );

    // Cursor on `put:` in the same header: line 3, col 8.
    let def_put = service.goto_definition(&file, Position::new(3, 8));
    assert!(
        def_put.is_some(),
        "expected navigation from `put:`, got None"
    );
    let loc_put = def_put.unwrap();
    assert_eq!(loc_put.file, file);
    assert!(
        (loc_put.span.start() as usize) < child_decl_offset,
        "expected Dict's at:put: span (< {child_decl_offset}), got {}",
        loc_put.span.start()
    );

    // Both keyword parts must navigate to the same definition.
    assert_eq!(
        loc_at.span.start(),
        loc_put.span.start(),
        "`at:` and `put:` keyword clicks should resolve to the same definition"
    );
}

#[test]
fn goto_definition_from_method_header_no_override_returns_none() {
    // If the method does not override anything in
    // any ancestor, Go to Definition on the header returns None rather
    // than navigating elsewhere.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Foo
    //  1:   bar => 1
    // Object defines no `bar` (in a stripped hierarchy), so `bar` does
    // not override anything in Foo.
    service.update_file(file.clone(), "Object subclass: Foo\n  bar => 1".to_string());

    // Cursor on `bar` in Foo's header: line 1, col 2.
    let def = service.goto_definition(&file, Position::new(1, 2));
    assert!(
        def.is_none(),
        "expected None for non-overriding method header, got {def:?}"
    );
}

#[test]
fn goto_definition_from_class_method_header_navigates_to_parent() {
    // Class-side methods (`class foo => ...`) must resolve the
    // same way as instance methods. The `class_side` flag on the
    // receiver context threads through `find_method_in_module` so the
    // MRO walk matches only class-side methods on each ancestor.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Parent
    //  1:   class ping => 1
    //  2: Parent subclass: Child
    //  3:   class ping => 2
    let source =
        "Object subclass: Parent\n  class ping => 1\nParent subclass: Child\n  class ping => 2"
            .to_string();
    service.update_file(file.clone(), source.clone());

    // Cursor on `ping` in Child's class-method header: line 3, col 8
    // (after `  class `).
    let def = service.goto_definition(&file, Position::new(3, 8));
    assert!(
        def.is_some(),
        "expected navigation to parent's class-side ping, got None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    let child_decl_offset = source.find("Parent subclass: Child").unwrap();
    assert!(
        (loc.span.start() as usize) < child_decl_offset,
        "expected Parent's class ping span (< {child_decl_offset}), got {}",
        loc.span.start()
    );
}

#[test]
fn goto_definition_from_standalone_method_header_navigates_to_parent() {
    // Standalone (Tonel-style) method definitions
    // (`Foo >> bar => ...`) must also resolve correctly. These live in
    // `module.method_definitions` rather than on `Class::methods`.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Parent
    //  1: Parent subclass: Child
    //  2: Parent >> greet => 1
    //  3: Child >> greet => 2
    let source =
        "Object subclass: Parent\nParent subclass: Child\nParent >> greet => 1\nChild >> greet => 2"
            .to_string();
    service.update_file(file.clone(), source.clone());

    // Cursor on `greet` in Child's standalone header: line 3, col 9
    // (C=0, h=1, i=2, l=3, d=4, ' '=5, >=6, >=7, ' '=8, g=9).
    let def = service.goto_definition(&file, Position::new(3, 9));
    assert!(
        def.is_some(),
        "expected navigation to parent's standalone greet, got None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    // Parent's standalone `greet` lives before `Child >> greet`, so the
    // returned span must start before that marker.
    let child_standalone_offset = source.find("Child >> greet").unwrap();
    assert!(
        (loc.span.start() as usize) < child_standalone_offset,
        "expected Parent's standalone greet span (< {child_standalone_offset}), got {}",
        loc.span.start()
    );
}

#[test]
fn goto_definition_from_method_header_skips_non_overriding_intermediate() {
    // Walk MRO order correctly. Given
    //   Grandparent defines greet, Middle does NOT, Leaf overrides greet
    // clicking on Leaf's greet header should navigate to Grandparent
    // (skipping Middle). This pins the MRO walk order guarantee — a
    // naive implementation that only checks the immediate superclass
    // would return None here, which would be wrong.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    //  0: Object subclass: Grandparent
    //  1:   greet => 1
    //  2: Grandparent subclass: Middle
    //  3: Middle subclass: Leaf
    //  4:   greet => 3
    let source = "Object subclass: Grandparent\n  greet => 1\nGrandparent subclass: Middle\nMiddle subclass: Leaf\n  greet => 3"
        .to_string();
    service.update_file(file.clone(), source.clone());

    // Cursor on `greet` in Leaf's header: line 4, col 2.
    let def = service.goto_definition(&file, Position::new(4, 2));
    assert!(
        def.is_some(),
        "expected MRO walk to find Grandparent's greet, got None"
    );
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    // Grandparent's greet lives before the `Grandparent subclass: Middle`
    // declaration, so any span inside Grandparent must start before that
    // marker.
    let middle_decl_offset = source.find("Grandparent subclass: Middle").unwrap();
    assert!(
        (loc.span.start() as usize) < middle_decl_offset,
        "expected Grandparent's greet span (< {middle_decl_offset}), got {}",
        loc.span.start()
    );
}

#[test]
fn goto_definition_from_method_header_navigates_cross_file() {
    // The parent definition can live in a different file from
    // the overriding child. The cross-file walker in
    // `find_overridden_method_definition` should handle it.
    let mut service = SimpleLanguageService::new();
    let file_parent = Utf8PathBuf::from("parent.bt");
    let file_child = Utf8PathBuf::from("child.bt");

    service.update_file(
        file_parent.clone(),
        "Object subclass: Parent\n  greet => 1".to_string(),
    );
    service.update_file(
        file_child.clone(),
        "Parent subclass: Child\n  greet => 2".to_string(),
    );

    // Cursor on `greet` in Child's header (in child.bt): line 1, col 2.
    let def = service.goto_definition(&file_child, Position::new(1, 2));
    assert!(
        def.is_some(),
        "expected cross-file navigation to parent.bt, got None"
    );
    let loc = def.unwrap();
    assert_eq!(
        loc.file, file_parent,
        "expected parent.bt, got {:?}",
        loc.file
    );
}
