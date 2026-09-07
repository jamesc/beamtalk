// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `find_references` from method-definition headers (unary/keyword/binary,
//! cross-file, class/standalone methods) and the click-target rejection
//! matrix for non-name header tokens (moved from `lib.rs` per BT-3450).

use super::common::*;

#[test]
fn find_references_from_method_definition_header_unary() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    // Method `bar` defined in class Foo, and called via `x bar`.
    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar => 1\nx := Foo new\nx bar".to_string(),
    );

    // Cursor ON the unary method definition header (the `bar` on line 1 col 2).
    // Expected: 1 definition + 1 call site = 2 refs.
    let refs = service.find_references(&file, Position::new(1, 2));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (definition + call site) from unary header, got {}",
        refs.len()
    );
}

#[test]
fn find_references_from_method_definition_header_keyword() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  at: i put: v => v\nx at: 1 put: 2".to_string(),
    );

    // Cursor on the `at:` keyword in the method header (line 1, col 2).
    // Expected: 1 definition + 1 call site = 2 refs.
    let refs = service.find_references(&file, Position::new(1, 2));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `at:` header, got {}",
        refs.len()
    );

    // Cursor on the `put:` keyword in the method header (line 1, col 8) —
    // must return the same full `at:put:` reference set.
    let refs_put = service.find_references(&file, Position::new(1, 8));
    assert_eq!(
        refs_put.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `put:` header, got {}",
        refs_put.len()
    );
}

#[test]
fn find_references_from_method_definition_header_binary() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Vec\n  + other => other\nx + y".to_string(),
    );

    // Cursor on the `+` binary selector in the method header (line 1, col 2).
    // Expected: 1 definition + 1 call site = 2 refs.
    let refs = service.find_references(&file, Position::new(1, 2));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (definition + call site) from binary header `+`, got {}",
        refs.len()
    );
}

#[test]
fn find_references_from_method_definition_header_cross_file() {
    // The definition-header path must integrate with cross-file search:
    // clicking on a method name in its definition should find every call
    // site in every indexed file.
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );
    service.update_file(file_b.clone(), "x := Foo new\nx bar".to_string());

    // Cursor on the `bar` in the definition in file_a.
    // Expected: 1 definition in file_a + 1 call site in file_b = 2 refs.
    let refs = service.find_references(&file_a, Position::new(1, 2));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 cross-file refs (def in file_a + call in file_b), got {}: {refs:?}",
        refs.len()
    );
    assert!(
        refs.iter().any(|r| r.file == file_a),
        "missing definition in file_a: {refs:?}"
    );
    assert!(
        refs.iter().any(|r| r.file == file_b),
        "missing call site in file_b: {refs:?}"
    );
}

#[test]
fn find_references_rejects_click_on_parameter_name_in_header() {
    // Clicking on a parameter name must NOT be treated as "on the
    // selector". Here we click on `i` in `at: i put: v`. The parameter
    // name has no uses in the body (body is just `v`), so the identifier
    // fallback should also return no cross-file matches. The point is
    // that the click must not be mistaken for a click on the `at:put:`
    // selector (which would return 2 refs).
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  at: i put: v => v\nx at: 1 put: 2".to_string(),
    );

    // Column 6 = position of `i` on line 1 (`  at: i put: v => v`)
    let refs_on_param = service.find_references(&file, Position::new(1, 6));
    // Sanity-check that a click on the selector would return 2, so an
    // over-matching bug in the header heuristic would surface as 2 here.
    let refs_on_selector = service.find_references(&file, Position::new(1, 2));
    assert_eq!(refs_on_selector.len(), 2);
    assert_ne!(
        refs_on_param.len(),
        2,
        "param-name click was mistaken for `at:put:` selector click"
    );
}

// ── BT-1941: tightened unary/binary selector-span precision ────────

#[test]
fn find_references_rejects_click_on_sealed_modifier_in_header() {
    // Clicking on the `sealed` modifier keyword must NOT be treated as
    // a click on the selector `bar`. Before BT-1941 the coarse
    // "inside method.span, before the first param/return-type/body"
    // rule was permissive here because `method.span.start()` is
    // captured before modifiers are consumed.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  sealed bar => 1\nx := Foo new\nx bar".to_string(),
    );

    // Line 1: `  sealed bar => 1` — `sealed` spans columns 2-7, `bar`
    // spans columns 9-11.
    let refs_on_selector = service.find_references(&file, Position::new(1, 9));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_modifier = service.find_references(&file, Position::new(1, 4));
    assert_ne!(
        refs_on_modifier.len(),
        2,
        "click on `sealed` modifier was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_rejects_click_on_arrow_in_header() {
    // Clicking on the `->` punctuation between a unary selector and its
    // return type annotation must NOT be treated as a click on the
    // selector. Before BT-1941 this was permissive because `->` falls
    // inside `method.span` but outside the return-type annotation's own
    // span.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar -> Integer => 1\nx := Foo new\nx bar".to_string(),
    );

    // Line 1: `  bar -> Integer => 1` — `bar` spans columns 2-4, `->`
    // spans columns 6-7.
    let refs_on_selector = service.find_references(&file, Position::new(1, 2));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_arrow = service.find_references(&file, Position::new(1, 6));
    assert_ne!(
        refs_on_arrow.len(),
        2,
        "click on `->` punctuation was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_rejects_click_on_whitespace_in_header() {
    // Clicking on whitespace between the selector and the `=>` must NOT
    // be treated as a click on the selector.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar => 1\nx := Foo new\nx bar".to_string(),
    );

    // Line 1: `  bar => 1` — `bar` spans columns 2-4, whitespace at
    // column 5.
    let refs_on_selector = service.find_references(&file, Position::new(1, 2));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_whitespace = service.find_references(&file, Position::new(1, 5));
    assert_ne!(
        refs_on_whitespace.len(),
        2,
        "click on whitespace was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_rejects_click_on_class_and_internal_modifiers_in_header() {
    // Both `class` and `internal` modifiers, combined on one header,
    // must be rejected — and the skip-count logic must correctly walk
    // past *both* of them to land on the real selector `bar`.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  internal class bar => 1\nFoo bar".to_string(),
    );

    // Line 1: `  internal class bar => 1` — `internal` spans columns
    // 2-9, `class` spans columns 11-15, `bar` spans columns 17-19.
    let refs_on_selector = service.find_references(&file, Position::new(1, 17));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_internal = service.find_references(&file, Position::new(1, 5));
    assert_ne!(
        refs_on_internal.len(),
        2,
        "click on `internal` modifier was mistaken for a click on the `bar` selector"
    );

    let refs_on_class = service.find_references(&file, Position::new(1, 13));
    assert_ne!(
        refs_on_class.len(),
        2,
        "click on `class` modifier was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_rejects_click_on_duplicate_sealed_modifiers_in_header() {
    // The parser consumes `sealed` unconditionally and does not dedupe,
    // so `sealed sealed bar => 1` parses with `is_sealed == true` but two
    // `sealed` tokens consumed. The header scan must skip *both* tokens
    // (mirroring the parser token-for-token) rather than skipping a fixed
    // count derived from the single `is_sealed` flag — otherwise the
    // second `sealed` would be mis-identified as the `bar` selector.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  sealed sealed bar => 1\nx := Foo new\nx bar".to_string(),
    );

    // Line 1: `  sealed sealed bar => 1` — first `sealed` cols 2-7,
    // second `sealed` cols 9-14, `bar` cols 16-18.
    let refs_on_selector = service.find_references(&file, Position::new(1, 16));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_second_sealed = service.find_references(&file, Position::new(1, 11));
    assert_ne!(
        refs_on_second_sealed.len(),
        2,
        "click on the second `sealed` modifier was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_rejects_click_on_duplicate_class_modifiers_in_header() {
    // `class` is a modifier when its lookahead is not `=>`/`->`/`::`, so
    // `class class bar => 1` consumes *two* `class` tokens as modifiers
    // (each keeping `is_class_method == true`) before the `bar` selector.
    // The scan must walk past both.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  class class bar => 1\nFoo bar".to_string(),
    );

    // Line 1: `  class class bar => 1` — first `class` cols 2-7, second
    // `class` cols 8-13, `bar` cols 14-17.
    let refs_on_selector = service.find_references(&file, Position::new(1, 14));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_second_class = service.find_references(&file, Position::new(1, 9));
    assert_ne!(
        refs_on_second_class.len(),
        2,
        "click on the second `class` modifier was mistaken for a click on the `bar` selector"
    );
}

#[test]
fn find_references_from_binary_method_header_with_modifier() {
    // Binary selector (`+`) with a leading modifier: the scan must skip
    // `sealed` and land on the `+` selector token (a `BinarySelector`
    // token kind, exercising the non-`Identifier` selector path).
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  sealed + other => other\nx := Foo new\nx + 1".to_string(),
    );

    // Line 1: `  sealed + other => other` — `sealed` cols 2-7, `+` col 9.
    let refs_on_selector = service.find_references(&file, Position::new(1, 9));
    assert_eq!(
        refs_on_selector.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `+` selector click, got {}",
        refs_on_selector.len()
    );

    let refs_on_sealed = service.find_references(&file, Position::new(1, 4));
    assert_ne!(
        refs_on_sealed.len(),
        2,
        "click on `sealed` modifier was mistaken for a click on the `+` selector"
    );
}

#[test]
fn find_references_from_method_header_named_after_modifier_keyword() {
    // Edge case called out in BT-1941: a unary method whose selector
    // name is itself a modifier keyword. `class => ...` parses with
    // `is_class_method == false` because the parser's own lookahead
    // (`is_fat_arrow_or_return_type`) resolves the ambiguity in favour
    // of "this is the selector" when `class` is immediately followed by
    // `=>`. The header scan must drive its skip count off that flag —
    // not off pattern-matching the literal word `class` — so it must
    // still recognize `class` as the selector rather than skipping it
    // as a modifier and then finding nothing.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  class => 1\nx := Foo new\nx class".to_string(),
    );

    // Line 1: `  class => 1` — `class` spans columns 2-6.
    let refs = service.find_references(&file, Position::new(1, 4));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (definition + call site) from `class`-named selector click, got {}",
        refs.len()
    );
}

#[test]
fn find_references_from_class_method_definition_header() {
    // The helper walks both `class.methods` and `class.class_methods`;
    // pin coverage for the class-side path so it doesn't regress.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  class ping => 1\nFoo ping".to_string(),
    );

    // Cursor on `ping` in the class-method definition header (line 1 col 8
    // — after `  class `).
    let refs = service.find_references(&file, Position::new(1, 8));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (class-method def + `Foo ping` call), got {}: {refs:?}",
        refs.len()
    );
}

#[test]
fn find_references_from_standalone_method_definition_header() {
    // The helper walks `module.method_definitions` (Tonel-style `Foo >>
    // bar => ...`); pin coverage for that path too.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    // Line layout:
    //   0: Object subclass: Foo
    //   1: Foo >> bar => 1
    //   2: x := Foo new
    //   3: x bar
    service.update_file(
        file.clone(),
        "Object subclass: Foo\nFoo >> bar => 1\nx := Foo new\nx bar".to_string(),
    );

    // Cursor on `bar` in the standalone definition header: line 1,
    // column 7 (columns: F=0, o=1, o=2, ' '=3, >=4, >=5, ' '=6, b=7).
    let refs = service.find_references(&file, Position::new(1, 7));
    assert_eq!(
        refs.len(),
        2,
        "expected exactly 2 refs (standalone def + `x bar` call), got {}: {refs:?}",
        refs.len()
    );
}

#[test]
fn find_references_rejects_click_on_body_expression_in_method() {
    // Clicking on a body expression must not be treated as "on the
    // selector" — otherwise every expression in the method body would
    // falsely return the full set of call sites for the method.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar => myVar\nx bar\ny bar".to_string(),
    );

    // Line 1, col 9 = `myVar` in the body (not the selector `bar`)
    let refs_on_body = service.find_references(&file, Position::new(1, 9));
    // Line 1, col 2 = `bar` selector in the header
    let refs_on_header = service.find_references(&file, Position::new(1, 2));

    // The header click finds `bar` refs: 1 def + 2 call sites = 3.
    assert_eq!(
        refs_on_header.len(),
        3,
        "header click should return def + two call sites, got {}",
        refs_on_header.len()
    );
    // The body click finds `myVar` refs: just the one occurrence in the body.
    assert_eq!(
        refs_on_body.len(),
        1,
        "body click should only return the single `myVar` occurrence, got {}",
        refs_on_body.len()
    );
}
