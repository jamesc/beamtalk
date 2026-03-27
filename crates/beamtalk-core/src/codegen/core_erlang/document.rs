// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Wadler-Lindig document tree for Core Erlang code generation (ADR 0018).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module provides a composable `Document` type for building Core Erlang
//! output declaratively. Instead of writing directly to a string buffer with
//! manual indentation tracking, codegen functions return `Document` values
//! that are rendered in a final pass.
//!
//! # Example
//!
//! ```
//! use beamtalk_core::codegen::core_erlang::document::{Document, line, nest, nil};
//! use beamtalk_core::docvec;
//!
//! let doc = docvec![
//!     "'method_table'/0 = fun () ->",
//!     nest(4, docvec![line(), "~{entries}~"]),
//! ];
//! assert_eq!(doc.to_pretty_string(), "'method_table'/0 = fun () ->\n    ~{entries}~");
//! ```
//!
//! Based on Gleam's Document implementation, adapted for Beamtalk's needs.

/// Indentation width used throughout Core Erlang generation.
pub const INDENT: isize = 4;

/// Default line width for pretty-printing (characters per line).
pub const DEFAULT_LINE_WIDTH: isize = 80;

/// A pretty-printable document tree.
///
/// Documents are composable, immutable tree structures that describe
/// the layout of Core Erlang output. They are rendered to strings
/// in a final pass, with automatic indentation handling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document<'a> {
    /// A borrowed string literal.
    Str(&'a str),
    /// An owned string.
    String(String),
    /// A newline followed by current indentation.
    Line,
    /// Increase indentation for nested content.
    Nest(isize, Box<Document<'a>>),
    /// A sequence of documents.
    Vec(Vec<Document<'a>>),
    /// A group that can be rendered flat or broken across lines.
    Group(Box<Document<'a>>),
    /// A break point — rendered as given string when flat, newline when broken.
    Break {
        /// Text emitted when the group is rendered in broken (multi-line) mode.
        broken: &'a str,
        /// Text emitted when the group is rendered in flat (single-line) mode.
        unbroken: &'a str,
    },
    /// Empty document.
    Nil,
}

/// Coerce a value into a `Document`.
pub trait Documentable<'a> {
    /// Converts this value into a `Document`.
    fn to_doc(self) -> Document<'a>;
}

impl<'a> Documentable<'a> for &'a str {
    fn to_doc(self) -> Document<'a> {
        Document::Str(self)
    }
}

impl<'a> Documentable<'a> for String {
    fn to_doc(self) -> Document<'a> {
        Document::String(self)
    }
}

impl<'a> Documentable<'a> for Document<'a> {
    fn to_doc(self) -> Document<'a> {
        self
    }
}

impl<'a> Documentable<'a> for Vec<Document<'a>> {
    fn to_doc(self) -> Document<'a> {
        Document::Vec(self)
    }
}

impl<'a> Documentable<'a> for usize {
    fn to_doc(self) -> Document<'a> {
        Document::String(self.to_string())
    }
}

impl<'a> Documentable<'a> for isize {
    fn to_doc(self) -> Document<'a> {
        Document::String(self.to_string())
    }
}

/// Join multiple documents together in a vector.
///
/// Each element is converted to a `Document` via the `Documentable` trait.
/// Documents are concatenated directly — no separator is inserted.
///
/// ```
/// use beamtalk_core::docvec;
/// use beamtalk_core::codegen::core_erlang::document::Document;
///
/// let doc = docvec!["hello", " ", "world"];
/// assert_eq!(doc.to_pretty_string(), "hello world");
/// ```
#[macro_export]
macro_rules! docvec {
    () => {
        $crate::codegen::core_erlang::document::Document::Vec(Vec::new())
    };

    ($first:expr $(,)?) => {
        $crate::codegen::core_erlang::document::Document::Vec(
            vec![$crate::codegen::core_erlang::document::Documentable::to_doc($first)]
        )
    };

    ($first:expr, $($rest:expr),+ $(,)?) => {
        match $crate::codegen::core_erlang::document::Documentable::to_doc($first) {
            $crate::codegen::core_erlang::document::Document::Vec(mut vec) => {
                $(
                    vec.push($crate::codegen::core_erlang::document::Documentable::to_doc($rest));
                )*
                $crate::codegen::core_erlang::document::Document::Vec(vec)
            },
            first => {
                $crate::codegen::core_erlang::document::Document::Vec(
                    vec![first, $($crate::codegen::core_erlang::document::Documentable::to_doc($rest)),+]
                )
            }
        }
    };
}

/// Creates a `Line` document — a mandatory newline followed by indentation.
#[must_use]
pub fn line() -> Document<'static> {
    Document::Line
}

/// Creates a `Nil` document — an empty document.
#[must_use]
pub fn nil() -> Document<'static> {
    Document::Nil
}

/// Creates a `Nest` document — increases indentation for the inner document.
#[must_use]
pub fn nest(indent: isize, doc: Document<'_>) -> Document<'_> {
    Document::Nest(indent, Box::new(doc))
}

/// Creates a `Group` document — attempts to render on one line, breaks if needed.
#[must_use]
pub fn group(doc: Document<'_>) -> Document<'_> {
    Document::Group(Box::new(doc))
}

/// Creates a `Break` document — renders `broken` string followed by a newline
/// when in break mode, or `unbroken` string when in flat mode.
#[must_use]
pub fn break_<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break { broken, unbroken }
}

/// Joins documents with a separator between each pair.
#[must_use]
pub fn join<'a>(
    docs: impl IntoIterator<Item = Document<'a>>,
    separator: &Document<'a>,
) -> Document<'a> {
    let docs: Vec<_> = docs.into_iter().collect();
    if docs.is_empty() {
        return Document::Nil;
    }
    let mut result = Vec::with_capacity(docs.len() * 2 - 1);
    let mut first = true;
    for doc in docs {
        if !first {
            result.push(separator.clone());
        }
        result.push(doc);
        first = false;
    }
    Document::Vec(result)
}

/// Concatenates documents without any separator.
#[must_use]
pub fn concat<'a>(docs: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    Document::Vec(docs.into_iter().collect())
}

// --- Rendering ---

/// Rendering mode for break/group layout decisions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// All breaks render as their unbroken string (flat).
    Flat,
    /// All breaks render as newlines (broken).
    Break,
}

/// Pre-allocated indentation buffer. Lines are indented at most this many spaces;
/// deeper indentation falls back to a per-call allocation (rare in practice).
const INDENT_BUF: &str = "                                                                                                                                ";

/// Convert `usize` to `isize`, saturating at `isize::MAX`.
///
/// Used for column tracking in the renderer. String lengths never exceed
/// `isize::MAX` in practice, but we handle it gracefully.
#[inline]
fn len_as_isize(len: usize) -> isize {
    isize::try_from(len).unwrap_or(isize::MAX)
}

impl Document<'_> {
    /// Renders the document to a string using the default line width.
    ///
    /// Uses the Wadler-Lindig algorithm: `Group` nodes are rendered flat when
    /// their content fits within [`DEFAULT_LINE_WIDTH`] columns, and broken
    /// (multi-line) otherwise.
    #[must_use]
    pub fn to_pretty_string(&self) -> String {
        self.to_pretty_string_width(DEFAULT_LINE_WIDTH)
    }

    /// Renders the document to a string using the given line width.
    ///
    /// Uses the Wadler-Lindig algorithm iteratively with a work-list. When
    /// deciding whether to render a `Group` flat, the fit check considers both
    /// the group's content **and** all trailing siblings in the same container,
    /// so a group is only flattened when `group + continuation` fits within
    /// `width` columns — matching the standard Wadler-Lindig semantics.
    #[must_use]
    pub fn to_pretty_string_width(&self, width: isize) -> String {
        use std::collections::VecDeque;

        let mut output = String::new();
        let mut col = 0_isize;

        // Lazy indentation: instead of writing spaces immediately after a
        // newline, we record the pending indent and only emit it when actual
        // text content follows. This avoids generating trailing whitespace
        // on blank lines, eliminating the need for a post-processing pass.
        let mut pending_indent: Option<isize> = None;

        // Work list: (indent, mode, document_ref).
        // Elements are processed front-to-back; items pushed with push_front
        // are processed next, allowing us to expand composite documents in order.
        let mut work: VecDeque<(isize, Mode, &Document<'_>)> = VecDeque::new();
        work.push_back((0, Mode::Break, self));

        while let Some((indent, mode, doc)) = work.pop_front() {
            match doc {
                Document::Nil => {}
                Document::Str(s) => {
                    if let Some(ind) = pending_indent.take() {
                        write_indent(&mut output, ind);
                    }
                    output.push_str(s);
                    col += len_as_isize(s.len());
                }
                Document::String(s) => {
                    if let Some(ind) = pending_indent.take() {
                        write_indent(&mut output, ind);
                    }
                    output.push_str(s.as_str());
                    col += len_as_isize(s.len());
                }
                Document::Line => {
                    // Trim any trailing whitespace before emitting newline.
                    trim_trailing_spaces(&mut output);
                    output.push('\n');
                    pending_indent = Some(indent);
                    col = indent;
                }
                Document::Nest(extra, inner) => {
                    work.push_front((indent + extra, mode, inner));
                }
                Document::Vec(docs) => {
                    // Push in reverse so the first element is processed first.
                    for d in docs.iter().rev() {
                        work.push_front((indent, mode, d));
                    }
                }
                Document::Group(inner) => {
                    // Fit check: group content in Flat mode + continuation in
                    // their current modes, to correctly account for trailing
                    // siblings that share the same line.
                    let remaining = width - col;
                    let fits_flat = fits_deque(remaining, inner.as_ref(), Mode::Flat, &work);
                    let child_mode = if fits_flat { Mode::Flat } else { Mode::Break };
                    work.push_front((indent, child_mode, inner));
                }
                Document::Break { broken, unbroken } => match mode {
                    Mode::Break => {
                        if let Some(ind) = pending_indent.take() {
                            write_indent(&mut output, ind);
                        }
                        output.push_str(broken);
                        // Trim any trailing whitespace before emitting newline,
                        // matching the original post-processing behavior.
                        trim_trailing_spaces(&mut output);
                        output.push('\n');
                        pending_indent = Some(indent);
                        col = indent;
                    }
                    Mode::Flat => {
                        if let Some(ind) = pending_indent.take() {
                            write_indent(&mut output, ind);
                        }
                        output.push_str(unbroken);
                        col += len_as_isize(unbroken.len());
                    }
                },
            }
        }

        output
    }
}

/// Returns `true` if rendering `doc` in `doc_mode` followed by the continuation
/// items in `continuation` fits within `remaining` columns before the next
/// mandatory line break.
///
/// This avoids copying the entire work deque into a new collection. Instead it
/// walks the document using a small local stack, then lazily iterates the
/// continuation deque only if the primary document didn't already determine
/// the result (which it almost always does — groups typically contain a Line
/// or Break within a few nodes).
fn fits_deque(
    remaining: isize,
    doc: &Document<'_>,
    doc_mode: Mode,
    continuation: &std::collections::VecDeque<(isize, Mode, &Document<'_>)>,
) -> bool {
    // Small local stack for expanding documents without cloning the deque.
    let mut local: Vec<(Mode, &Document<'_>)> = Vec::new();
    local.push((doc_mode, doc));

    // Index into continuation (front-to-back iteration order).
    let mut cont_idx = 0;
    let mut remaining = remaining;

    loop {
        // Try to get the next item from the local stack, or from continuation.
        let (mode, current) = if let Some(item) = local.pop() {
            item
        } else {
            // Local stack exhausted — pull next continuation item.
            if cont_idx >= continuation.len() {
                return remaining >= 0;
            }
            let (_, cont_mode, cont_doc) = continuation[cont_idx];
            cont_idx += 1;
            (cont_mode, cont_doc)
        };

        if remaining < 0 {
            return false;
        }

        match current {
            Document::Nil => {}
            Document::Str(s) => remaining -= len_as_isize(s.len()),
            Document::String(s) => remaining -= len_as_isize(s.len()),
            Document::Line => return true,
            Document::Break { unbroken, .. } => match mode {
                Mode::Flat => remaining -= len_as_isize(unbroken.len()),
                Mode::Break => return true,
            },
            Document::Nest(_, inner) => local.push((mode, inner)),
            Document::Vec(docs) => {
                for d in docs.iter().rev() {
                    local.push((mode, d));
                }
            }
            Document::Group(inner) => local.push((Mode::Flat, inner)),
        }
    }
}

/// Removes trailing ASCII spaces and tabs from the end of the output string.
///
/// Called just before emitting a newline to ensure no line has trailing
/// whitespace, matching the original post-processing behavior.
#[inline]
fn trim_trailing_spaces(output: &mut String) {
    let trimmed_len = output.trim_end_matches([' ', '\t']).len();
    output.truncate(trimmed_len);
}

/// Writes `indent` spaces to the output string using a pre-allocated buffer.
fn write_indent(output: &mut String, indent: isize) {
    // SAFETY: `indent.max(0)` is non-negative, so the cast to `usize` cannot lose sign.
    #[allow(clippy::cast_sign_loss)]
    let n = indent.max(0) as usize;
    if n <= INDENT_BUF.len() {
        output.push_str(&INDENT_BUF[..n]);
    } else {
        // Rare: extremely deep nesting beyond the buffer.
        output.push_str(INDENT_BUF);
        for _ in INDENT_BUF.len()..n {
            output.push(' ');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn str_document() {
        let doc = Document::Str("hello");
        assert_eq!(doc.to_pretty_string(), "hello");
    }

    #[test]
    fn string_document() {
        let doc = Document::String("world".to_string());
        assert_eq!(doc.to_pretty_string(), "world");
    }

    #[test]
    fn nil_document() {
        let doc = Document::Nil;
        assert_eq!(doc.to_pretty_string(), "");
    }

    #[test]
    fn line_document() {
        let doc = Document::Vec(vec![Document::Str("a"), Document::Line, Document::Str("b")]);
        assert_eq!(doc.to_pretty_string(), "a\nb");
    }

    #[test]
    fn nest_document() {
        let doc = Document::Vec(vec![
            Document::Str("fun () ->"),
            nest(4, Document::Vec(vec![line(), Document::Str("body")])),
        ]);
        assert_eq!(doc.to_pretty_string(), "fun () ->\n    body");
    }

    #[test]
    fn nested_nest() {
        let doc = nest(
            2,
            Document::Vec(vec![
                line(),
                Document::Str("outer"),
                nest(2, Document::Vec(vec![line(), Document::Str("inner")])),
            ]),
        );
        assert_eq!(doc.to_pretty_string(), "\n  outer\n    inner");
    }

    #[test]
    fn vec_document() {
        let doc = Document::Vec(vec![
            Document::Str("a"),
            Document::Str("b"),
            Document::Str("c"),
        ]);
        assert_eq!(doc.to_pretty_string(), "abc");
    }

    #[test]
    fn docvec_macro_empty() {
        let doc = docvec![];
        assert_eq!(doc.to_pretty_string(), "");
    }

    #[test]
    fn docvec_macro_single() {
        let doc = docvec!["hello"];
        assert_eq!(doc.to_pretty_string(), "hello");
    }

    #[test]
    fn docvec_macro_multiple() {
        let doc = docvec!["a", "b", "c"];
        assert_eq!(doc.to_pretty_string(), "abc");
    }

    #[test]
    fn docvec_macro_mixed_types() {
        let owned = "world".to_string();
        let doc = docvec!["hello ", Document::String(owned)];
        assert_eq!(doc.to_pretty_string(), "hello world");
    }

    #[test]
    fn docvec_macro_with_line() {
        let doc = docvec!["first", line(), "second"];
        assert_eq!(doc.to_pretty_string(), "first\nsecond");
    }

    #[test]
    fn docvec_flattens_leading_vec() {
        // When the first element is already a Vec, it should be flattened
        let inner = docvec!["a", "b"];
        let doc = docvec![inner, "c"];
        // Should be Vec[a, b, c] not Vec[Vec[a, b], c]
        assert_eq!(doc.to_pretty_string(), "abc");
        if let Document::Vec(v) = doc {
            assert_eq!(v.len(), 3);
        } else {
            panic!("Expected Vec");
        }
    }

    #[test]
    fn join_documents() {
        let docs = vec![Document::Str("a"), Document::Str("b"), Document::Str("c")];
        let doc = join(docs, &Document::Str(", "));
        assert_eq!(doc.to_pretty_string(), "a, b, c");
    }

    #[test]
    fn join_empty() {
        let docs: Vec<Document> = vec![];
        let doc = join(docs, &Document::Str(", "));
        assert_eq!(doc.to_pretty_string(), "");
    }

    #[test]
    fn join_single() {
        let docs = vec![Document::Str("only")];
        let doc = join(docs, &Document::Str(", "));
        assert_eq!(doc.to_pretty_string(), "only");
    }

    #[test]
    fn group_fits_inline() {
        // "a b" is 3 chars — fits in 80 columns, so group renders flat
        let doc = group(docvec!["a", break_("", " "), "b"]);
        assert_eq!(doc.to_pretty_string(), "a b");
    }

    #[test]
    fn group_breaks_when_too_long() {
        // 80 'x' chars + " y" overflows width=80, so group breaks
        let long = "x".repeat(79);
        let doc = group(docvec![Document::String(long), break_("", " "), "y"]);
        assert_eq!(doc.to_pretty_string(), "x".repeat(79) + "\ny");
    }

    #[test]
    fn group_fits_with_explicit_narrow_width() {
        // "ab" is 2 chars; with width=1 it overflows, group breaks
        let doc = group(docvec!["a", break_("", ""), "b"]);
        assert_eq!(doc.to_pretty_string_width(1), "a\nb");
    }

    #[test]
    fn group_fit_considers_continuation_docs() {
        // group("a b") fits in 4 chars alone, but "a b c" = 5 chars overflows width=4.
        // The fit check must consider trailing " c" sibling, so the group breaks.
        let doc = docvec![group(docvec!["a", break_("", " "), "b"]), " c"];
        assert_eq!(doc.to_pretty_string_width(4), "a\nb c");
    }

    #[test]
    fn break_document() {
        let doc = docvec![
            "a",
            Document::Break {
                broken: "",
                unbroken: " ",
            },
            "b",
        ];
        // In break mode, renders broken string + newline
        assert_eq!(doc.to_pretty_string(), "a\nb");
    }

    #[test]
    fn concat_documents() {
        let docs = vec![
            Document::Str("hello"),
            Document::Str(" "),
            Document::Str("world"),
        ];
        let doc = concat(docs);
        assert_eq!(doc.to_pretty_string(), "hello world");
    }

    #[test]
    fn documentable_usize() {
        let doc: Document = 42_usize.to_doc();
        assert_eq!(doc.to_pretty_string(), "42");
    }

    #[test]
    fn documentable_isize() {
        let doc: Document = (-1_isize).to_doc();
        assert_eq!(doc.to_pretty_string(), "-1");
    }

    #[test]
    fn realistic_core_erlang_function() {
        let doc = docvec![
            "'method_table'/0 = fun () ->",
            nest(
                INDENT,
                docvec![line(), "~{'increment' => 0, 'value' => 0}~",]
            ),
        ];
        assert_eq!(
            doc.to_pretty_string(),
            "'method_table'/0 = fun () ->\n    ~{'increment' => 0, 'value' => 0}~"
        );
    }

    #[test]
    fn realistic_let_binding() {
        let doc = docvec![
            "let _BlockFun = apply receiver () in ",
            "let _HandlerFun = apply handler () in ",
            "try apply _BlockFun () ",
            "of _Result -> _Result ",
            "catch <_Type, _Error, _Stack> -> ",
            "primop 'raw_raise'(_Type, _Error, _Stack)",
        ];
        assert_eq!(
            doc.to_pretty_string(),
            "let _BlockFun = apply receiver () in \
             let _HandlerFun = apply handler () in \
             try apply _BlockFun () \
             of _Result -> _Result \
             catch <_Type, _Error, _Stack> -> \
             primop 'raw_raise'(_Type, _Error, _Stack)"
        );
    }

    #[test]
    fn realistic_module_header() {
        let doc = docvec![
            "module 'counter' ['eval'/1]\n",
            "  attributes []\n",
            "\n",
            "'eval'/1 = fun (Bindings) ->\n",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let State = Bindings in\n",
                    "    let Result = body in\n",
                    "    {Result, State}\n",
                ]
            ),
            "end\n",
        ];
        let expected = "module 'counter' ['eval'/1]\n\
                        \x20 attributes []\n\
                        \n\
                        'eval'/1 = fun (Bindings) ->\n\
                        \n\
                        \x20   let State = Bindings in\n\
                        \x20   let Result = body in\n\
                        \x20   {Result, State}\n\
                        end\n";
        assert_eq!(doc.to_pretty_string(), expected);
    }
}
