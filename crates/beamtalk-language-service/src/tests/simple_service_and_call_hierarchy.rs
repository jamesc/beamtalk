// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `SimpleLanguageService` smoke tests (diagnostics, completions, hover,
//! go-to-definition) and the call-hierarchy prepare classifier.

use super::common::*;

#[test]
fn simple_language_service_update_and_diagnostics() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(file.clone(), "x := 42".to_string());
    let diagnostics = service.diagnostics(&file);
    assert!(diagnostics.is_empty());

    // Invalid syntax should produce diagnostics
    service.update_file(file.clone(), "x := :=".to_string());
    let diagnostics = service.diagnostics(&file);
    assert!(!diagnostics.is_empty());
}

#[test]
fn simple_language_service_completions() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(file.clone(), "x := 42".to_string());
    let completions = service.completions(&file, Position::new(0, 0));
    assert!(!completions.is_empty());

    // Should have keyword completions
    assert!(
        completions
            .iter()
            .any(|c| c.label == "self" && c.kind == CompletionKind::Keyword)
    );

    // Should have identifier completions from source
    assert!(
        completions
            .iter()
            .any(|c| c.label == "x" && c.kind == CompletionKind::Variable)
    );

    // Should have message completions (from class hierarchy)
    assert!(
        completions
            .iter()
            .any(|c| c.label == "isNil" && c.kind == CompletionKind::Function)
    );
}

#[test]
fn simple_language_service_hover() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(file.clone(), "x := 42".to_string());

    // Hover on 'x' at position 0
    let hover = service.hover(&file, Position::new(0, 0));
    assert!(hover.is_some());
    let hover = hover.unwrap();
    assert!(hover.contents.contains('x'));

    // Hover on literal '42' at position 5
    let hover_literal = service.hover(&file, Position::new(0, 5));
    assert!(hover_literal.is_some());
    let hover_literal = hover_literal.unwrap();
    assert!(hover_literal.contents.contains("42"));
}

#[test]
fn simple_language_service_goto_definition() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(file.clone(), "x := 42.\ny := x".to_string());

    // Go to definition of 'x' at position (1, 5) should find assignment at (0, 0)
    let def = service.goto_definition(&file, Position::new(1, 5));
    assert!(def.is_some());
    let loc = def.unwrap();
    assert_eq!(loc.file, file);
    assert_eq!(loc.span.start(), 0);
}

// --- call hierarchy prepare classifier ---

/// Cursor on a method-definition header (instance side) populates
/// selector + class + `class_side=false`.
#[test]
fn call_hierarchy_prepare_at_method_header_instance() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("counter.bt");
    // Line 2, column 2: `increment` selector token in a method header.
    let source = "Object subclass: Counter\n  increment => 1\n";
    service.update_file(file.clone(), source.to_string());

    let target = service
        .call_hierarchy_prepare_at(&file, Position::new(1, 2))
        .expect("prepare hit on method header");
    assert_eq!(target.selector, "increment");
    assert_eq!(target.class_name.as_deref(), Some("Counter"));
    assert!(!target.class_side);
    assert_eq!(target.file, file);
}

/// Cursor on a class-method header populates `class_side=true`.
#[test]
fn call_hierarchy_prepare_at_method_header_class_side() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("counter.bt");
    let source = "Object subclass: Counter\n  class make => self new\n";
    service.update_file(file.clone(), source.to_string());

    // Column 8: inside the `make` selector on the class-method header.
    let target = service
        .call_hierarchy_prepare_at(&file, Position::new(1, 8))
        .expect("prepare hit on class-method header");
    assert_eq!(target.selector, "make");
    assert_eq!(target.class_name.as_deref(), Some("Counter"));
    assert!(target.class_side);
}

/// Cursor on a call-site selector populates selector but leaves
/// `class_name` empty — the receiver class is dynamic.
#[test]
fn call_hierarchy_prepare_at_call_site_has_no_class_context() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("counter.bt");
    let source = "Object subclass: Counter\n  greet => self name asString\n";
    service.update_file(file.clone(), source.to_string());

    // Column 23: inside `asString` at the call site
    // (2-space indent + `greet => self name ` = 21 chars; column 23
    // lands on the third letter of `asString`).
    let target = service
        .call_hierarchy_prepare_at(&file, Position::new(1, 23))
        .expect("prepare hit on call site");
    assert_eq!(target.selector, "asString");
    assert!(target.class_name.is_none());
}

/// Cursor on whitespace / non-selector returns None.
#[test]
fn call_hierarchy_prepare_at_returns_none_on_local_identifier() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("counter.bt");
    let source = "x := 42\n".to_string();
    service.update_file(file.clone(), source);

    // Column 0 is the bare local variable `x` — not a selector.
    let target = service.call_hierarchy_prepare_at(&file, Position::new(0, 0));
    assert!(target.is_none());
}

/// Cold-file incoming-calls fallback walks every file and
/// returns *only* sender sites — method-definition headers must be
/// excluded (otherwise the editor would list the method itself as
/// "calling itself"). Compare with `find_references`, which also
/// emits the definition for the `textDocument/references` UI.
#[test]
fn find_selector_send_sites_across_files_excludes_definitions() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("a.bt");
    let file_b = Utf8PathBuf::from("b.bt");
    // File A defines `increment` but does not send it.
    service.update_file(
        file_a.clone(),
        "Object subclass: A\n  increment => 1\n".to_string(),
    );
    // File B sends `increment` from another method.
    service.update_file(
        file_b.clone(),
        "Object subclass: B\n  use => a increment\n".to_string(),
    );

    let sites = service.find_selector_send_sites_across_files("increment");
    // The sender lives in B, and only in B — the definition header in
    // A must not appear, even though A is the implementor.
    assert!(
        sites.iter().any(|loc| loc.file == file_b),
        "expected call site in B, got {sites:?}"
    );
    assert!(
        !sites.iter().any(|loc| loc.file == file_a),
        "definition in A leaked into senders: {sites:?}"
    );
}

#[test]
fn simple_language_service_find_references() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");

    service.update_file(file.clone(), "x := 42.\ny := x".to_string());

    // Find references to 'x'
    let refs = service.find_references(&file, Position::new(0, 0));
    assert_eq!(refs.len(), 2); // Assignment and usage
}
