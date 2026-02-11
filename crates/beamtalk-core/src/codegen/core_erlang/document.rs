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
    Break { broken: &'a str, unbroken: &'a str },
    /// Empty document.
    Nil,
}

/// Coerce a value into a `Document`.
pub trait Documentable<'a> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// All breaks render as their unbroken string (flat).
    #[expect(dead_code, reason = "Will be used for Group fitting in Phase 2")]
    Flat,
    /// All breaks render as newlines (broken).
    Break,
}

impl Document<'_> {
    /// Renders the document to a string.
    ///
    /// This is a simple renderer that does not perform line-width fitting.
    /// Core Erlang has mostly fixed formatting, so we render `Group`/`Break`
    /// in break mode (always break). This keeps output predictable and
    /// byte-identical with the existing `write!` approach.
    #[must_use]
    pub fn to_pretty_string(&self) -> String {
        let mut output = String::new();
        self.render_to(&mut output, 0, Mode::Break);
        output
    }

    fn render_to(&self, output: &mut String, indent: isize, mode: Mode) {
        match self {
            Document::Str(s) => output.push_str(s),
            Document::String(s) => output.push_str(s),
            Document::Nil => {}
            Document::Line => {
                output.push('\n');
                write_indent(output, indent);
            }
            Document::Nest(extra, doc) => {
                doc.render_to(output, indent + extra, mode);
            }
            Document::Vec(docs) => {
                for doc in docs {
                    doc.render_to(output, indent, mode);
                }
            }
            Document::Group(doc) => {
                // Try flat mode first: if it fits on one conceptual line, use it
                // For now, always break (Core Erlang has fixed formatting)
                doc.render_to(output, indent, Mode::Break);
            }
            Document::Break { broken, unbroken } => match mode {
                Mode::Break => {
                    output.push_str(broken);
                    output.push('\n');
                    write_indent(output, indent);
                }
                Mode::Flat => {
                    output.push_str(unbroken);
                }
            },
        }
    }
}

fn write_indent(output: &mut String, indent: isize) {
    for _ in 0..indent {
        output.push(' ');
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
    fn group_break_in_break_mode() {
        let doc = group(docvec!["a", break_("", " "), "b",]);
        // In break mode (default), breaks render as broken (newline)
        assert_eq!(doc.to_pretty_string(), "a\nb");
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
