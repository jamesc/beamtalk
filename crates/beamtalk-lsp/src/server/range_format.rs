// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Range formatting (`textDocument/rangeFormatting`).
//!
//! **DDD Context:** Language Service
//!
//! The unparser is a whole-module operation, so range formatting formats the
//! whole file and then keeps only the changes that touch the top-level
//! constructs (classes, standalone methods, protocols, aliases, native
//! declarations, script expressions) overlapping the requested range. Text
//! outside those constructs is returned unchanged (BT-5080).

use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_core::unparse::format_source;
use similar::{DiffTag, TextDiff};

/// A replacement of `source[start..end]` with `new_text`.
#[derive(Debug, PartialEq, Eq)]
pub(in crate::server) struct ByteEdit {
    pub start: usize,
    pub end: usize,
    pub new_text: String,
}

/// Computes the edits needed to format the top-level constructs overlapping
/// the byte range `start..end` of `source`.
///
/// Returns `None` when the source has parse errors (formatting a broken file
/// could corrupt it) or the formatter's round-trip check refuses its output.
pub(in crate::server) fn format_range_edits(
    source: &str,
    start: usize,
    end: usize,
) -> Option<Vec<ByteEdit>> {
    let formatted = format_source(source).ok()?;
    if formatted == source {
        return Some(vec![]);
    }
    let (start, end) = (start.min(end), start.max(end));

    // Expand the range to the top-level constructs it touches.
    let (module, diags) = parse(lex_with_eof(source));
    if diags.iter().any(|d| d.severity == Severity::Error) {
        return None;
    }
    let mut spans: Vec<(usize, usize)> = Vec::new();
    let mut push = |s: beamtalk_core::source_analysis::Span| {
        spans.push((s.start() as usize, s.end() as usize));
    };
    module.classes.iter().for_each(|c| push(c.span));
    module.method_definitions.iter().for_each(|m| push(m.span));
    module.protocols.iter().for_each(|p| push(p.span));
    module.type_aliases.iter().for_each(|t| push(t.span));
    module.native_declarations.iter().for_each(|n| push(n.span));
    module
        .expressions
        .iter()
        .for_each(|e| push(e.expression.span()));

    let (mut lo, mut hi) = (start, end);
    let mut touched = false;
    for &(s, e) in &spans {
        // A zero-width range (caret) touches a construct it sits inside or at
        // the edge of. A selection's end is exclusive (BT-5132): one ending at
        // column 0 of the next line, or right at a construct's start, does
        // not touch the construct that begins there.
        let overlaps_selection = if start == end {
            s <= start && start <= e
        } else {
            s < end && start < e
        };
        if overlaps_selection {
            lo = lo.min(s);
            hi = hi.max(e);
            touched = true;
        }
    }
    if !touched {
        // Range lies entirely between constructs (blank lines / comments): nothing to format.
        return Some(vec![]);
    }

    // Line offsets of the original.
    let mut line_starts = vec![0usize];
    line_starts.extend(source.match_indices('\n').map(|(i, _)| i + 1));
    let line_of = |off: usize| line_starts.partition_point(|&s| s <= off) - 1;
    let (lo_line, hi_line) = (line_of(lo), line_of(hi.saturating_sub(1).max(lo)));
    let line_off = |l: usize| line_starts.get(l).copied().unwrap_or(source.len());

    let new_lines: Vec<&str> = formatted.split_inclusive('\n').collect();
    let diff = TextDiff::from_lines(source, &formatted);
    let mut edits = Vec::new();
    for op in diff.ops() {
        if op.tag() == DiffTag::Equal {
            continue;
        }
        let old = op.old_range();
        let new = op.new_range();
        // An insertion (empty old range) sits at a line boundary; it belongs to
        // the range when that boundary is inside or at the edges of it.
        // BT-5132: a replacement must lie wholly inside the touched constructs'
        // lines; a hunk that also rewrites neighbouring lines (blank-line
        // separators, an adjacent construct) is skipped rather than applied
        // beyond the construct.
        let overlaps = if old.is_empty() {
            old.start >= lo_line && old.start <= hi_line + 1
        } else {
            old.start >= lo_line && old.end <= hi_line + 1
        };
        if !overlaps {
            continue;
        }
        edits.push(ByteEdit {
            start: line_off(old.start),
            end: line_off(old.end),
            new_text: new_lines[new.start..new.end].concat(),
        });
    }
    Some(edits)
}
