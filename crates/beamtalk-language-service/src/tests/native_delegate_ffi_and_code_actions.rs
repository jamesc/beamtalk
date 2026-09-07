// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native-delegate/FFI-call detection and `code_actions` (annotation
//! suggestions, range filtering, cursor-boundary matching) (moved from
//! `lib.rs` per BT-3450).

use super::common::*;

// ── native delegate tests (BT-1215) ────────────────────────────────────

#[test]
fn check_native_delegate_on_self_delegate_method() {
    let mut service = SimpleLanguageService::new();
    let file_def = Utf8PathBuf::from("native.bt");
    service.update_file(
        file_def.clone(),
        "Actor subclass: Proc native: beamtalk_proc\n  run => self delegate".to_string(),
    );
    let file_call = Utf8PathBuf::from("caller.bt");
    service.update_file(file_call.clone(), "p := Proc spawn\np run".to_string());

    // Go-to-definition on the `run` call navigates to the method definition
    let def = service.goto_definition(&file_call, Position::new(1, 2));
    assert!(def.is_some(), "should find the method definition");
    let loc = def.unwrap();
    assert_eq!(loc.file, file_def);

    let delegate_info = service.check_native_delegate(&loc);
    assert!(delegate_info.is_some(), "should detect native delegate");
    assert_eq!(delegate_info.unwrap().backing_module, "beamtalk_proc");
}

#[test]
fn check_native_delegate_returns_none_for_normal_method() {
    let mut service = SimpleLanguageService::new();
    let file_def = Utf8PathBuf::from("normal.bt");
    service.update_file(
        file_def.clone(),
        "Object subclass: Foo\n  bar => 42".to_string(),
    );
    let file_call = Utf8PathBuf::from("caller.bt");
    service.update_file(file_call.clone(), "f := Foo new\nf bar".to_string());

    let def = service.goto_definition(&file_call, Position::new(1, 2));
    assert!(def.is_some());
    let loc = def.unwrap();
    let delegate_info = service.check_native_delegate(&loc);
    assert!(delegate_info.is_none());
}

// ── FFI goto-definition tests ─────────────────────────────────────────

#[test]
fn check_ffi_call_on_selector() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("ffi.bt");
    service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

    // Cursor on `reverse:` — should detect FFI call
    let ffi = service.check_ffi_call(&file, Position::new(0, 20));
    assert!(ffi.is_some(), "should detect FFI call on selector");
    let info = ffi.unwrap();
    assert_eq!(info.module_name, "lists");
    assert_eq!(info.function_name, "reverse");
}

#[test]
fn check_ffi_call_on_module_name() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("ffi.bt");
    service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

    // Cursor on `lists` — should detect FFI call with module but no function
    let ffi = service.check_ffi_call(&file, Position::new(0, 8));
    assert!(ffi.is_some(), "should detect FFI call on module name");
    let info = ffi.unwrap();
    assert_eq!(info.module_name, "lists");
    assert!(info.function_name.is_empty());
}

#[test]
fn check_ffi_call_returns_none_for_normal_send() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("normal.bt");
    service.update_file(file.clone(), "items reverse".to_string());

    let ffi = service.check_ffi_call(&file, Position::new(0, 7));
    assert!(
        ffi.is_none(),
        "should not detect FFI on normal message send"
    );
}

#[test]
fn check_ffi_call_in_method_body() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("method.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Foo\n  bar => Erlang io format: \"hello\"".to_string(),
    );

    // Cursor on `format:` inside method body
    let ffi = service.check_ffi_call(&file, Position::new(1, 20));
    assert!(ffi.is_some(), "should detect FFI in method body");
    let info = ffi.unwrap();
    assert_eq!(info.module_name, "io");
    assert_eq!(info.function_name, "format");
}

#[test]
fn check_ffi_call_enriches_line_from_registry() {
    use beamtalk_core::semantic_analysis::InferredType;
    use beamtalk_core::semantic_analysis::type_checker::{
        FunctionSignature, NativeTypeRegistry, ParamType, TypeProvenance,
    };

    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("ffi.bt");
    service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

    // Set up registry with a line number for lists:reverse/1
    let mut registry = NativeTypeRegistry::new();
    registry.register_module(
        "lists",
        vec![FunctionSignature {
            name: "reverse".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("list")),
                type_: InferredType::known("List"),
            }],
            return_type: InferredType::known("List"),
            provenance: TypeProvenance::Extracted,
            line: Some(42),
        }],
    );
    service.set_native_types(registry);

    let ffi = service.check_ffi_call(&file, Position::new(0, 20));
    assert!(ffi.is_some());
    let info = ffi.unwrap();
    assert_eq!(info.module_name, "lists");
    assert_eq!(info.function_name, "reverse");
    assert_eq!(info.line, Some(42));
}

#[test]
fn check_ffi_call_line_is_none_without_registry() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("ffi.bt");
    service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

    // No registry set — line should be None
    let ffi = service.check_ffi_call(&file, Position::new(0, 20));
    assert!(ffi.is_some());
    assert!(ffi.unwrap().line.is_none());
}

// ── code_actions tests (BT-1067) ────────────────────────────────────────

/// Helper: byte length of a source string as `u32` (test-only).
#[expect(
    clippy::cast_possible_truncation,
    reason = "test strings are trivially small"
)]
fn len32(s: &str) -> u32 {
    s.len() as u32
}

#[test]
fn code_actions_returns_annotation_suggestion_for_unary_method() {
    // `count => 42` — inferred Integer; action inserts `-> Integer ` before `=>`
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count => 42";
    service.update_file(file.clone(), source.to_string());

    // Request code actions spanning the whole file
    let actions = service.code_actions(&file, 0, len32(source));
    assert_eq!(actions.len(), 1, "expected exactly one code action");

    let action = &actions[0];
    assert!(
        action.title.contains("Integer"),
        "title should mention inferred type: {title}",
        title = action.title
    );
    assert_eq!(action.new_text, "-> Integer ", "wrong inserted text");

    // Verify insertion point is at the `=>` of the method
    let before_body = &source[..action.insert_at as usize];
    assert!(
        before_body.ends_with("count "),
        "insertion should be before `=>`, got prefix: {before_body:?}"
    );
}

#[test]
fn code_actions_returns_annotation_suggestion_for_early_return_method() {
    // `count => ^ 42` — inferred Integer via early return; same insertion logic
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count => ^ 42";
    service.update_file(file.clone(), source.to_string());

    let actions = service.code_actions(&file, 0, len32(source));
    assert_eq!(actions.len(), 1, "expected exactly one code action");

    let action = &actions[0];
    assert!(action.title.contains("Integer"));
    assert_eq!(action.new_text, "-> Integer ");
}

#[test]
fn code_actions_skips_already_annotated_methods() {
    // Explicit annotation — no code action expected
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count -> Integer => 42";
    service.update_file(file.clone(), source.to_string());

    let actions = service.code_actions(&file, 0, len32(source));
    assert!(
        actions.is_empty(),
        "annotated method should produce no action"
    );
}

#[test]
fn code_actions_skips_methods_outside_requested_range() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count => 42";
    service.update_file(file.clone(), source.to_string());

    // Request code actions for a range that doesn't overlap the method
    let actions = service.code_actions(&file, 0, 5);
    assert!(
        actions.is_empty(),
        "method outside range should produce no action"
    );
}

#[test]
fn code_actions_cursor_at_method_span_start_matches() {
    // Cursor (start == end) placed exactly at the first byte of the method should match.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count => 42";
    service.update_file(file.clone(), source.to_string());

    // "  count => 42" starts at byte 27 (after "Object subclass: Counter\n")
    let method_start = len32("Object subclass: Counter\n  "); // 27
    let actions = service.code_actions(&file, method_start, method_start);
    assert!(
        !actions.is_empty(),
        "cursor at method span start should match (start == end boundary)"
    );
}

#[test]
fn code_actions_cursor_at_method_span_end_does_not_match() {
    // Cursor placed exactly at span.end() (exclusive) should NOT match.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Counter\n  count => 42";
    service.update_file(file.clone(), source.to_string());

    let method_end = len32(source); // end of source == end of method span
    let actions = service.code_actions(&file, method_end, method_end);
    assert!(
        actions.is_empty(),
        "cursor at exclusive span end should NOT match"
    );
}

#[test]
fn code_actions_empty_for_unknown_file() {
    let service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("nonexistent.bt");
    let actions = service.code_actions(&file, 0, 100);
    assert!(
        actions.is_empty(),
        "unknown file should return empty actions"
    );
}

#[test]
fn code_actions_returns_annotation_for_keyword_method() {
    // `greet: name => "hello"` — keyword method with known String return
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Calc\n  greet: name => \"hello\"";
    service.update_file(file.clone(), source.to_string());

    let actions = service.code_actions(&file, 0, len32(source));
    assert_eq!(
        actions.len(),
        1,
        "expected one code action for keyword method"
    );
    assert!(actions[0].title.contains("String"));
    assert_eq!(actions[0].new_text, "-> String ");
}

#[test]
fn code_actions_returns_annotation_for_class_method() {
    // `class answer => 42` — class-side method inferred Integer
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    let source = "Object subclass: Calc\n  class answer => 42";
    service.update_file(file.clone(), source.to_string());

    let actions = service.code_actions(&file, 0, len32(source));
    assert_eq!(
        actions.len(),
        1,
        "expected one code action for class method"
    );
    assert!(actions[0].title.contains("Integer"));
}

#[test]
fn find_body_open_offset_finds_fat_arrow() {
    use beamtalk_core::source_analysis::Span;
    // `count => 42` — the `=>` is at byte 6 within "count => 42"
    let source = "count => 42";
    let span = Span::new(0, len32(source));
    let offset = find_body_open_offset(source, span).expect("should find `=>`");
    assert_eq!(offset, 6, "should point to `=>`");
}

#[test]
fn find_body_open_offset_with_keyword_selector() {
    use beamtalk_core::source_analysis::Span;
    // keyword method: `add: x => x + 1`
    let source = "add: x => x + 1";
    let span = Span::new(0, len32(source));
    let offset = find_body_open_offset(source, span).expect("should find `=>`");
    assert_eq!(offset, 7, "should point to `=>`");
}

#[test]
fn find_body_open_offset_returns_none_without_fat_arrow() {
    use beamtalk_core::source_analysis::Span;
    let source = "no body opener here";
    let span = Span::new(0, len32(source));
    assert!(find_body_open_offset(source, span).is_none());
}

#[test]
fn find_references_selector_cross_file_unary() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");

    service.update_file(
        file_a.clone(),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );
    // Cursor on unary selector "bar"
    service.update_file(file_b.clone(), "x bar".to_string());

    let refs = service.find_references(&file_b, Position::new(0, 2));
    assert!(refs.len() >= 2);
    assert!(refs.iter().any(|r| r.file == file_a));
    assert!(refs.iter().any(|r| r.file == file_b));
}
