// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `format_source`, `resolve_path_for_uri`, and `position_to_offset` tests.

use super::*;

#[test]
fn format_source_returns_none_for_parse_errors() {
    let result = format_source("@@@invalid beamtalk@@@");
    assert!(result.is_none(), "parse errors must suppress formatting");
}

#[test]
fn format_source_returns_formatted_string() {
    // An unformatted source must produce non-empty output, and that output
    // must be stable (formatting the result again returns the same string).
    let source = "Object subclass: Foo\n  bar => 42\n";
    let pass1 = format_source(source).expect("valid source must produce output");
    assert!(!pass1.is_empty(), "formatted output must not be empty");
    let pass2 = format_source(&pass1).expect("formatted output must be valid");
    assert_eq!(pass1, pass2, "output must be canonical (idempotent)");
}

#[test]
fn format_source_ensures_trailing_newline() {
    let source = "x := 42";
    let result = format_source(source).expect("valid source");
    assert!(
        result.ends_with('\n'),
        "formatted output must end with newline"
    );
}

#[test]
fn format_source_idempotent() {
    let source = "Object subclass: Foo\n  bar => 42\n";
    let pass1 = format_source(source).expect("pass 1");
    let pass2 = format_source(&pass1).expect("pass 2");
    assert_eq!(pass1, pass2, "formatting must be idempotent");
}

#[test]
fn resolve_path_for_uri_stdlib_uri_resolves_to_real_path() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let real_path = Utf8PathBuf::from("/fake/stdlib/integer.bt");
    {
        let mut stdlib_paths = backend
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        stdlib_paths.insert(real_path.clone());
    }

    let uri = Url::parse("beamtalk-stdlib:///integer.bt").expect("valid URI");
    let result = backend.resolve_path_for_uri(&uri);
    assert_eq!(result, Some(real_path));
}

#[test]
fn resolve_path_for_uri_unknown_class_returns_none() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let uri = Url::parse("beamtalk-stdlib:///NonExistent.bt").expect("valid URI");
    let result = backend.resolve_path_for_uri(&uri);
    assert!(result.is_none());
}

#[test]
fn resolve_path_for_uri_invalid_stdlib_form_returns_none() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let real_path = Utf8PathBuf::from("/fake/stdlib/integer.bt");
    {
        let mut stdlib_paths = backend
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        stdlib_paths.insert(real_path);
    }

    for bad_uri in &[
        "beamtalk-stdlib:///",
        "beamtalk-stdlib:///sub/integer.bt",
        "beamtalk-stdlib:///Integer.erl",
        "beamtalk-stdlib://host/integer.bt",
        "beamtalk-stdlib:///integer.bt?x=1",
        "beamtalk-stdlib:///integer.bt#section",
    ] {
        let uri = Url::parse(bad_uri).expect("parseable URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert!(result.is_none(), "expected None for {bad_uri}");
    }
}

#[test]
fn resolve_path_for_uri_ambiguous_stdlib_returns_none() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    {
        let mut stdlib_paths = backend
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/a/integer.bt"));
        stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/b/integer.bt"));
    }

    let uri = Url::parse("beamtalk-stdlib:///integer.bt").expect("valid URI");
    let result = backend.resolve_path_for_uri(&uri);
    assert!(result.is_none(), "ambiguous filename must return None");
}

#[test]
#[cfg(unix)]
fn resolve_path_for_uri_file_uri_still_works() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let uri = Url::parse("file:///some/project/main.bt").expect("valid URI");
    let result = backend.resolve_path_for_uri(&uri);
    assert_eq!(result, Some(Utf8PathBuf::from("/some/project/main.bt")));
}

#[test]
#[cfg(windows)]
fn resolve_path_for_uri_file_uri_still_works() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let uri = Url::parse("file:///C:/project/main.bt").expect("valid URI");
    let result = backend.resolve_path_for_uri(&uri);
    assert_eq!(result, Some(Utf8PathBuf::from("C:/project/main.bt")));
}

// ── position_to_offset tests ─────────────────────────────

#[test]
fn position_to_offset_first_line() {
    let source = "hello world";
    let pos = tower_lsp::lsp_types::Position::new(0, 6);
    assert_eq!(position_to_offset(pos, source), 6);
}

#[test]
fn position_to_offset_second_line() {
    let source = "hello\nworld";
    let pos = tower_lsp::lsp_types::Position::new(1, 3);
    assert_eq!(position_to_offset(pos, source), 9); // 'l' in "world"
}

#[test]
fn position_to_offset_beyond_eof() {
    let source = "hi";
    let pos = tower_lsp::lsp_types::Position::new(5, 0);
    assert_eq!(position_to_offset(pos, source), source.len());
}

#[test]
fn position_to_offset_roundtrip_with_offset_to_position() {
    let source = "Object subclass: Counter\n  count => 42";
    for byte_offset in [0, 5, 24, 25, 30, 38] {
        let pos = offset_to_position(byte_offset, source);
        let back = position_to_offset(pos, source);
        assert_eq!(
            back, byte_offset,
            "roundtrip failed for offset {byte_offset}"
        );
    }
}
