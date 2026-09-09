// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Position, span, and URI conversions between beamtalk-core/runtime
//! coordinates and LSP protocol types.

use beamtalk_core::source_analysis::{Severity, Span};
use beamtalk_language_service::{DocumentSymbolKind, Position as BtPosition};
use camino::Utf8PathBuf;
use tower_lsp::lsp_types::{
    DiagnosticSeverity, FoldingRange, FoldingRangeKind, Position, Range, SymbolKind, Url,
};

/// Converts an LSP URI to a `Utf8PathBuf`.
pub(in crate::server) fn uri_to_path(uri: &Url) -> Option<Utf8PathBuf> {
    match uri.scheme() {
        "file" => uri
            .to_file_path()
            .ok()
            .and_then(|p| Utf8PathBuf::try_from(p).ok()),
        "untitled" => {
            let name = uri.path().trim_start_matches('/');
            Some(Utf8PathBuf::from(format!("__untitled__/{name}")))
        }
        _ => None,
    }
}
/// Converts a `Utf8PathBuf` to an LSP URI.
pub(in crate::server) fn path_to_uri(path: &Utf8PathBuf) -> Option<Url> {
    if let Some(name) = path.as_str().strip_prefix("__untitled__/") {
        Url::parse(&format!("untitled:{name}")).ok()
    } else {
        Url::from_file_path(path.as_str()).ok()
    }
}
/// Converts a stdlib file path to a `beamtalk-stdlib:///ClassName.bt` virtual URI.
pub(in crate::server) fn path_to_stdlib_uri(path: &Utf8PathBuf) -> Option<Url> {
    let filename = path.file_name()?;
    Url::parse(&format!("beamtalk-stdlib:///{filename}")).ok()
}
/// Converts an LSP `Position` (UTF-16 code units) to a beamtalk `Position` (byte offsets).
///
/// LSP positions use UTF-16 code units for the character field.
/// Beamtalk positions use byte offsets within the line.
pub(in crate::server) fn to_bt_position(
    pos: tower_lsp::lsp_types::Position,
    source: &str,
) -> BtPosition {
    let target_line = pos.line;
    let target_utf16_col = pos.character;

    let mut current_line = 0u32;
    let mut line_start = 0usize;

    // Find the start of the target line
    for (i, ch) in source.char_indices() {
        if current_line == target_line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Walk the target line, counting UTF-16 code units until we reach the target column
    let mut utf16_col = 0u32;
    let mut byte_col = 0u32;
    for ch in source[line_start..].chars() {
        if ch == '\n' || utf16_col >= target_utf16_col {
            break;
        }
        // UTF-16 len is always 1 or 2, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf16() is always 1 or 2"
        )]
        {
            utf16_col += ch.len_utf16() as u32;
        }
        // len_utf8 is always 1-4, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf8() is always 1 to 4"
        )]
        {
            byte_col += ch.len_utf8() as u32;
        }
    }

    BtPosition::new(target_line, byte_col)
}
/// Converts a beamtalk `Span` to an LSP `Range` using source text.
pub(in crate::server) fn span_to_range(span: Span, source: &str) -> Range {
    let start = offset_to_position(span.start() as usize, source);
    let end = offset_to_position(span.end() as usize, source);
    Range { start, end }
}
/// Converts a [`Span`] to an LSP [`FoldingRange`].
///
/// Line-only, matching typical LSP folding-range providers: `start_line` is
/// the divider's own banner line, `end_line` is the line of the category's
/// last method. Character offsets are left unset (defaults to the full
/// line), and `kind` is `Region` — VS Code's default fold-gutter affordance,
/// distinct from `Comment`/`Imports`.
pub(in crate::server) fn span_to_folding_range(span: Span, source: &str) -> FoldingRange {
    let range = span_to_range(span, source);
    FoldingRange {
        start_line: range.start.line,
        start_character: None,
        end_line: range.end.line,
        end_character: None,
        kind: Some(FoldingRangeKind::Region),
        collapsed_text: None,
    }
}
/// Converts a byte offset to an LSP `Position` (0-based line/character in UTF-16 code units).
pub(in crate::server) fn offset_to_position(
    offset: usize,
    source: &str,
) -> tower_lsp::lsp_types::Position {
    let offset = offset.min(source.len());
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            // UTF-16 len is always 1 or 2, safe to truncate
            #[expect(
                clippy::cast_possible_truncation,
                reason = "char::len_utf16() is always 1 or 2"
            )]
            {
                col += ch.len_utf16() as u32;
            }
        }
    }
    tower_lsp::lsp_types::Position::new(line, col)
}
/// Converts an LSP `Position` (line, UTF-16 column) to a byte offset in `source`.
///
/// Returns `source.len()` when the position is beyond the end of the file.
pub(in crate::server) fn position_to_offset(
    pos: tower_lsp::lsp_types::Position,
    source: &str,
) -> usize {
    let mut line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if line == pos.line {
            // Walk UTF-16 columns within this line
            let mut col_utf16 = 0u32;
            let mut byte_offset = line_start;
            for (j, c) in source[line_start..].char_indices() {
                if col_utf16 >= pos.character {
                    return line_start + j;
                }
                if c == '\n' {
                    break;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "char::len_utf16() is always 1 or 2"
                )]
                {
                    col_utf16 += c.len_utf16() as u32;
                }
                byte_offset = line_start + j + c.len_utf8();
            }
            return byte_offset;
        }
        if ch == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    source.len()
}
/// Converts a beamtalk `Diagnostic` to an LSP `Diagnostic`.
pub(in crate::server) fn to_lsp_diagnostic(
    diag: &beamtalk_language_service::Diagnostic,
    source: Option<&str>,
) -> tower_lsp::lsp_types::Diagnostic {
    let range = source
        .map(|src| span_to_range(diag.span, src))
        .unwrap_or_default();

    tower_lsp::lsp_types::Diagnostic {
        range,
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            // Lint and Hint map to LSP HINT (informational)
            Severity::Lint | Severity::Hint => DiagnosticSeverity::HINT,
        }),
        source: Some("beamtalk".into()),
        message: {
            use std::fmt::Write;
            let mut msg = diag.message.to_string();
            // Append notes for origin tracing
            for note in &diag.notes {
                let _ = write!(msg, "\n  = {}", note.message);
            }
            if let Some(ref hint) = diag.hint {
                let _ = write!(msg, "\nHint: {hint}");
            }
            msg
        },
        ..Default::default()
    }
}
/// 1-based runtime line → zero-width LSP `Range`. Defends
/// against the runtime emitting line 0 (which it shouldn't) by clamping
/// to row 0.
pub(in crate::server) fn zero_width_range_for_line(line: u32) -> Range {
    let row = line.saturating_sub(1);
    Range {
        start: Position::new(row, 0),
        end: Position::new(row, 0),
    }
}
/// the `SymbolKind`/`detail` pair for a method `DocumentSymbol`,
/// shared by every conversion path that builds one (currently the
/// AST-fallback `to_lsp_symbol` and the runtime `runtime_class_to_document_symbol`)
/// so they can't silently disagree on how VS Code's Outline, breadcrumbs,
/// and Go to Symbol in File distinguish a class-side method from an
/// instance-side method sharing the same selector. `FUNCTION` isn't used
/// elsewhere in either path's `SymbolKind` mapping, so it's free to
/// repurpose for "static-ish member" — the closest standard fit.
pub(in crate::server) fn method_symbol_kind_and_detail(
    class_side: bool,
) -> (SymbolKind, Option<String>) {
    if class_side {
        (SymbolKind::FUNCTION, Some("class method".to_string()))
    } else {
        (SymbolKind::METHOD, None)
    }
}
/// Converts a beamtalk `DocumentSymbol` to an LSP `DocumentSymbol`.
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
pub(in crate::server) fn to_lsp_symbol(
    sym: beamtalk_language_service::DocumentSymbol,
    source: &str,
) -> tower_lsp::lsp_types::DocumentSymbol {
    let range = span_to_range(sym.span, source);
    let children = sym
        .children
        .into_iter()
        .map(|c| to_lsp_symbol(c, source))
        .collect();

    let selection_range = sym.name_span.map_or(range, |s| span_to_range(s, source));
    let (kind, detail) = match sym.kind {
        DocumentSymbolKind::Class => (SymbolKind::CLASS, None),
        DocumentSymbolKind::Method => method_symbol_kind_and_detail(false),
        DocumentSymbolKind::ClassMethod => method_symbol_kind_and_detail(true),
        DocumentSymbolKind::Field => (SymbolKind::FIELD, None),
        // a `// === Name ===` divider's method-category
        // container. NAMESPACE is the closest standard LSP `SymbolKind`
        // for "a named grouping of members that isn't itself a
        // type/function" — VS Code renders it with a distinct icon from
        // Method/Class, which is all that's needed here (nesting,
        // breadcrumbs, and sticky-scroll come from the tree shape, not
        // the icon choice).
        DocumentSymbolKind::Category => (SymbolKind::NAMESPACE, None),
    };
    tower_lsp::lsp_types::DocumentSymbol {
        name: sym.name.to_string(),
        kind,
        detail,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    }
}
