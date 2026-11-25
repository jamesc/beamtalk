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
        let mut stdlib_paths = backend.stdlib_paths.lock_recover();
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
        let mut stdlib_paths = backend.stdlib_paths.lock_recover();
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
        let mut stdlib_paths = backend.stdlib_paths.lock_recover();
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

// ── LineIndex: UTF-16 surrogate pairs and CRLF ───────────

#[test]
fn line_index_surrogate_pair_columns() {
    // '😀' is 4 UTF-8 bytes and 2 UTF-16 code units; 'é' is 2 bytes / 1 unit.
    let source = "a😀é\nb";
    let ix = LineIndex::new(source);
    // After 'a' (1 byte, 1 unit) and '😀' (4 bytes, 2 units).
    assert_eq!(ix.offset_to_position(5), Position::new(0, 3));
    assert_eq!(ix.position_to_offset(Position::new(0, 3)), 5);
    // After 'é'.
    assert_eq!(ix.offset_to_position(7), Position::new(0, 4));
    assert_eq!(ix.position_to_offset(Position::new(0, 4)), 7);
    // A column inside the surrogate pair rounds up to the end of the char.
    assert_eq!(ix.position_to_offset(Position::new(0, 2)), 5);
    // beamtalk columns are byte offsets within the line.
    assert_eq!(
        ix.to_bt_position(Position::new(0, 3)),
        beamtalk_language_service::Position::new(0, 5)
    );
    assert_eq!(
        ix.to_bt_position(Position::new(1, 1)),
        beamtalk_language_service::Position::new(1, 1)
    );
}

#[test]
fn line_index_crlf_lines_split_on_lf_only() {
    let source = "ab\r\ncd\r\n";
    let ix = LineIndex::new(source);
    // '\r' is an ordinary trailing column of line 0.
    assert_eq!(ix.offset_to_position(2), Position::new(0, 2));
    assert_eq!(ix.offset_to_position(3), Position::new(0, 3));
    // Offset just after "\r\n" starts line 1.
    assert_eq!(ix.offset_to_position(4), Position::new(1, 0));
    assert_eq!(ix.position_to_offset(Position::new(1, 1)), 5);
    // Column past the end of the line clamps before the '\n'.
    assert_eq!(ix.position_to_offset(Position::new(0, 99)), 3);
    // Trailing empty line after the final "\r\n".
    assert_eq!(ix.offset_to_position(source.len()), Position::new(2, 0));
    assert_eq!(ix.position_to_offset(Position::new(2, 0)), source.len());
    assert_eq!(ix.position_to_offset(Position::new(9, 0)), source.len());
}

#[test]
fn line_index_roundtrips_every_char_boundary() {
    let source = "x := '😀'.\r\n  é + 1\n\nlast";
    let ix = LineIndex::new(source);
    for offset in (0..=source.len()).filter(|&i| source.is_char_boundary(i)) {
        let pos = ix.offset_to_position(offset);
        assert_eq!(ix.position_to_offset(pos), offset, "offset {offset}");
    }
}

// ── Range formatting (BT-5080) ────────────────────────────────────────────

fn apply_byte_edits(source: &str, edits: &[range_format::ByteEdit]) -> String {
    let mut out = source.to_string();
    for e in edits.iter().rev() {
        out.replace_range(e.start..e.end, &e.new_text);
    }
    out
}

const TWO_CLASSES: &str =
    "Object subclass: Alpha\n  one =>   1\n\nObject subclass: Beta\n  two =>   2\n";

#[test]
fn range_format_only_changes_overlapping_construct() {
    let start = TWO_CLASSES.find("two").unwrap();
    let edits = range_format::format_range_edits(TWO_CLASSES, start, start + 3).unwrap();
    assert!(!edits.is_empty(), "selected construct needs formatting");
    let out = apply_byte_edits(TWO_CLASSES, &edits);
    let first = TWO_CLASSES.find("\n\nObject subclass: Beta").unwrap();
    assert_eq!(
        &out[..first],
        &TWO_CLASSES[..first],
        "text outside the selection must be unchanged"
    );
    assert_ne!(out, TWO_CLASSES);
    assert!(
        !out.contains("=>   2"),
        "selected method is formatted: {out}"
    );
}

#[test]
fn range_format_other_construct_untouched_when_selecting_first() {
    let start = TWO_CLASSES.find("one").unwrap();
    let edits = range_format::format_range_edits(TWO_CLASSES, start, start).unwrap();
    let out = apply_byte_edits(TWO_CLASSES, &edits);
    assert!(out.ends_with("two =>   2\n"), "Beta unchanged: {out}");
    assert!(!out.contains("=>   1"), "Alpha formatted: {out}");
}

#[test]
fn range_format_full_range_matches_whole_document() {
    let edits = range_format::format_range_edits(TWO_CLASSES, 0, TWO_CLASSES.len()).unwrap();
    assert_eq!(
        apply_byte_edits(TWO_CLASSES, &edits),
        format_source(TWO_CLASSES).unwrap()
    );
}

#[test]
fn range_format_parse_error_returns_none() {
    assert!(range_format::format_range_edits("@@@invalid@@@", 0, 3).is_none());
}

/// BT-5109: `FormattingOptions` are deliberately ignored — one canonical
/// style, identical to `beamtalk fmt`.
#[tokio::test]
async fn formatting_ignores_formatting_options() {
    use tower_lsp::lsp_types::{
        DocumentFormattingParams, FormattingOptions, TextDocumentIdentifier,
    };
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let dir = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-5109-fmt")).expect("utf8");
    let path = dir.join("f.bt");
    let uri = open_test_file(backend, &path, "Object subclass: Foo\n    bar =>   42\n");
    let run = |opts: FormattingOptions| {
        let uri = uri.clone();
        async move {
            backend
                .formatting(DocumentFormattingParams {
                    text_document: TextDocumentIdentifier { uri },
                    options: opts,
                    work_done_progress_params:
                        tower_lsp::lsp_types::WorkDoneProgressParams::default(),
                })
                .await
                .expect("formatting")
        }
    };
    let spaces = run(FormattingOptions {
        tab_size: 4,
        insert_spaces: true,
        ..Default::default()
    })
    .await;
    let tabs = run(FormattingOptions {
        tab_size: 8,
        insert_spaces: false,
        ..Default::default()
    })
    .await;
    assert_eq!(spaces, tabs, "options must not change the output");
}

#[test]
fn range_format_selection_ending_at_column_zero_does_not_touch_next_construct() {
    // BT-5132: the selection covers Alpha's lines and ends at column 0 of
    // Beta's header line; the end is exclusive, so Beta is not touched.
    let beta = TWO_CLASSES.find("Object subclass: Beta").unwrap();
    let edits = range_format::format_range_edits(TWO_CLASSES, 0, beta).unwrap();
    let out = apply_byte_edits(TWO_CLASSES, &edits);
    assert!(!out.contains("=>   1"), "Alpha formatted: {out}");
    assert!(out.ends_with("two =>   2\n"), "Beta unchanged: {out}");
}
