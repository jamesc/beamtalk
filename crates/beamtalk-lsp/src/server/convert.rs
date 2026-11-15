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
/// Line-start offsets for one source text; backs every position conversion.
///
/// LSP columns are UTF-16 code units, beamtalk columns are byte offsets within
/// the line. Lines are split on `\n` only, so in CRLF text the `\r` is an
/// ordinary one-unit column at the end of its line. Building the index is one
/// scan; each conversion afterwards is a binary search plus a walk of one line,
/// so converting many spans in a large file stays linear overall.
pub(in crate::server) struct LineIndex<'a> {
    source: &'a str,
    /// Byte offset of the start of each line; `starts[0] == 0`.
    starts: Vec<usize>,
}

impl<'a> LineIndex<'a> {
    pub(in crate::server) fn new(source: &'a str) -> Self {
        let mut starts = vec![0];
        starts.extend(source.match_indices('\n').map(|(i, _)| i + 1));
        Self { source, starts }
    }

    /// Sum of UTF-16 code units of `chars`, for columns (always fits `u32`).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "char::len_utf16() is always 1 or 2"
    )]
    fn utf16_len(ch: char) -> u32 {
        ch.len_utf16() as u32
    }

    /// Converts an LSP `Position` (UTF-16 code units) to a beamtalk `Position`
    /// (byte offsets within the line).
    ///
    /// A line past the end of the text resolves against the last line.
    pub(in crate::server) fn to_bt_position(&self, pos: Position) -> BtPosition {
        let line = (pos.line as usize).min(self.starts.len() - 1);
        let line_start = self.starts[line];

        // Walk the target line, counting UTF-16 code units until we reach the target column
        let mut utf16_col = 0u32;
        let mut byte_col = 0usize;
        for ch in self.source[line_start..].chars() {
            if ch == '\n' || utf16_col >= pos.character {
                break;
            }
            utf16_col += Self::utf16_len(ch);
            byte_col += ch.len_utf8();
        }

        #[expect(
            clippy::cast_possible_truncation,
            reason = "a column within a single line fits u32"
        )]
        BtPosition::new(pos.line, byte_col as u32)
    }

    /// Converts a byte offset to an LSP `Position` (0-based line/character in UTF-16 code units).
    pub(in crate::server) fn offset_to_position(&self, offset: usize) -> Position {
        let offset = offset.min(self.source.len());
        // Line = number of line starts at or before `offset`, minus line 0.
        let line = self.starts.partition_point(|&s| s <= offset) - 1;
        let col = self.source[self.starts[line]..]
            .char_indices()
            .take_while(|&(i, ch)| self.starts[line] + i < offset && ch != '\n')
            .map(|(_, ch)| Self::utf16_len(ch))
            .sum();
        #[expect(
            clippy::cast_possible_truncation,
            reason = "a line number fits u32 for any LSP document"
        )]
        Position::new(line as u32, col)
    }

    /// Converts an LSP `Position` (line, UTF-16 column) to a byte offset.
    ///
    /// Returns `source.len()` when the line is beyond the end of the file, and
    /// clamps to the end of the line when the column is past it.
    pub(in crate::server) fn position_to_offset(&self, pos: Position) -> usize {
        let Some(&line_start) = self.starts.get(pos.line as usize) else {
            return self.source.len();
        };
        let mut col_utf16 = 0u32;
        let mut offset = line_start;
        for ch in self.source[line_start..].chars() {
            if col_utf16 >= pos.character || ch == '\n' {
                break;
            }
            col_utf16 += Self::utf16_len(ch);
            offset += ch.len_utf8();
        }
        offset
    }

    /// Converts a beamtalk `Span` to an LSP `Range`.
    pub(in crate::server) fn span_to_range(&self, span: Span) -> Range {
        Range {
            start: self.offset_to_position(span.start() as usize),
            end: self.offset_to_position(span.end() as usize),
        }
    }
}

/// Converts an LSP `Position` to a beamtalk `Position`; see [`LineIndex::to_bt_position`].
pub(in crate::server) fn to_bt_position(pos: Position, source: &str) -> BtPosition {
    LineIndex::new(source).to_bt_position(pos)
}
/// Converts a beamtalk `Span` to an LSP `Range` using source text.
pub(in crate::server) fn span_to_range(span: Span, source: &str) -> Range {
    LineIndex::new(source).span_to_range(span)
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
/// Converts a byte offset to an LSP `Position`; see [`LineIndex::offset_to_position`].
pub(in crate::server) fn offset_to_position(offset: usize, source: &str) -> Position {
    LineIndex::new(source).offset_to_position(offset)
}
/// Converts an LSP `Position` to a byte offset; see [`LineIndex::position_to_offset`].
pub(in crate::server) fn position_to_offset(pos: Position, source: &str) -> usize {
    LineIndex::new(source).position_to_offset(pos)
}
/// Converts a beamtalk `Diagnostic` to an LSP `Diagnostic`.
pub(in crate::server) fn to_lsp_diagnostic(
    diag: &beamtalk_language_service::Diagnostic,
    index: Option<&LineIndex<'_>>,
) -> tower_lsp::lsp_types::Diagnostic {
    let range = index
        .map(|ix| ix.span_to_range(diag.span))
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
