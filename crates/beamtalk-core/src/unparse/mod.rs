// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! AST unparser: converts Beamtalk AST nodes back to source text (ADR 0044).
//!
//! **DDD Context:** Language Service — Formatting / Unparse
//!
//! The unparser produces a [`Document`] tree from AST nodes, then renders it
//! to a string via [`Document::to_pretty_string`]. It is the inverse of the
//! parser and is used by:
//!
//! - `extract_method_source` in codegen (to include leading comments in
//!   `CompiledMethod.source` and support synthesized methods with no source text)
//! - `beamtalk fmt` CLI command
//!
//! # Comment Handling
//!
//! Every AST node type that carries a [`CommentAttachment`] emits its comments
//! at the correct position:
//!
//! - **Leading comments** appear one per line immediately before the node.
//! - **Trailing comment** appears after the node on the same line.
//!
//! # Design Rules
//!
//! - All output goes through the [`Document`] API — **never** `format!()` or
//!   string concatenation (AGENTS.md / ADR 0018).
//! - Text leaves derived from AST data (identifiers, literal values, comments,
//!   etc.) are constructed through the intent-carrying [`leaf`] helpers. After
//!   ADR 0089 Phase 3 removed the open `Document::String` escape hatch, [`leaf`]
//!   is the single place in the unparser that constructs an owned-string leaf.

mod comments;
mod declarations;
mod expressions;
mod leaf;
mod round_trip;
mod signature_text;
mod types;

pub use round_trip::FormatError;
#[cfg(any(test, feature = "test"))]
pub(crate) use round_trip::debug_without_layout;
pub use signature_text::{
    SignatureParam, SignatureRenderOptions, SignatureSelector, render_signature_text,
};

use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, ClassDefinition, Comment, CommentAttachment, CommentKind, ExpectCategory,
    Expression, ExpressionStatement, GapComments, GapSlot, Identifier, KeywordPart, Literal,
    LiteralCommentSlots, LiteralComments, MapPair, MapPatternKey, MatchArm, MessageSelector,
    MethodDefinition, Module, NativeDeclaration, ParameterDefinition, Pattern, PipelineSends,
    ProtocolDefinition, ProtocolMethodSignature, ProtocolUse, ShapeField, SlotKind,
    StandaloneMethodDefinition, StateDeclaration, StringSegment, TypeAliasDefinition,
    TypeAnnotation,
};
use crate::source_analysis::{
    Severity, TokenKind, binary_binding_power, lex_with_eof, line_ending, parse, parse_method,
};
use crate::span::Span;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{DEFAULT_LINE_WIDTH, Document, break_, concat, group, line, nest, nil};

use comments::{
    leading_ends_with_orphaned_doc_comment, unparse_comment, unparse_comment_attachment_leading,
};
pub(crate) use declarations::*;
pub use expressions::prints_as_pipeline;
pub(crate) use expressions::*;
use types::{unparse_type_annotation, unparse_type_annotation_opt};

// --- Public entry points ---

/// Unparses a [`Module`] to source text.
///
/// Emits file-level leading comments (if any) followed by all classes,
/// standalone method definitions, and top-level expressions.
#[must_use]
pub fn unparse_module(module: &Module) -> String {
    unparse_module_doc(module).to_pretty_string()
}

/// Unparses a [`MethodDefinition`] to source text.
///
/// Emits leading comments (`//` / `/* */`), doc comment (`///`), the method
/// signature, and the method body. This is used by `extract_method_source` to
/// produce `CompiledMethod.source` that includes comments and works for
/// synthesized methods.
#[must_use]
pub fn unparse_method(method: &MethodDefinition) -> String {
    // The per-method source is the method's *edit unit*, and must match the
    // byte span the resolver assigns it (ADR 0082: `source_ref ==
    // disk[span]`). That span deliberately starts at the method's `///` doc
    // block or its own line, excluding any leading non-doc `//` comments —
    // notably `// === section ===` dividers, which are inter-method file
    // structure, not part of the method. Emitting them here would
    // make the stored/compiled source diverge from disk, so a no-op cockpit
    // save/flush would duplicate the divider. Whole-file unparse
    // (`unparse_module`) still preserves them in place.
    // A future change will surface section dividers as first-class method
    // categories instead of free comments.
    //
    // The `class ` prefix is emitted from `method.is_class_method` so a class-side
    // method's stored source matches its on-disk span (which includes `class `);
    // whole-file unparse supplies the prefix from its own context instead.
    unparse_method_definition_inner(method, class_prefix(method), EmitLeadingComments::No)
        .to_pretty_string()
}

/// The signature prefix for a stand-alone per-method render: `class ` for a
/// class-side method, nothing otherwise. Whole-file unparse passes its own
/// prefix and does not use this.
fn class_prefix(method: &MethodDefinition) -> Document<'static> {
    if method.is_class_method {
        Document::Str("class ")
    } else {
        nil()
    }
}

/// Re-lays-out a canonical (column-0) method source at `base_indent`, so the
/// result is byte-identical to what `bt fmt` (`unparse_module`) produces for the
/// same method on disk at that indentation (ADR 0082).
///
/// [`unparse_method`] renders a method at **column 0**, where the pretty-printer
/// makes line-break decisions against the full 80-column budget. On disk the same
/// method is indented under its class body, so it has `base_indent` fewer columns
/// available and a line that fit inline at column 0 must break. A pure
/// whitespace shift cannot *re-break* such a
/// line, so the stored `source_ref` would diverge from the on-disk span for any
/// width-sensitive method — flushing it would reformat the file.
///
/// To make `source_ref == disk[span]` hold by construction, this re-parses the
/// canonical body and re-renders it with the line-width budget reduced by the
/// indent, then prepends `base_indent` to every non-blank line. Reducing the
/// budget by the indent makes every break decision identically to rendering the
/// method *at* `base_indent` — the uniform shift the disk slice has. The `class `
/// prefix is preserved via [`MethodDefinition::is_class_method`], so class-side
/// methods round-trip too.
///
/// Blank lines are emitted empty (no indent, no trailing whitespace) and the
/// source's trailing-newline state is preserved, matching the disk slice
/// [`crate::source_analysis::method_span`] produces.
///
/// Falls back to a pure whitespace shift ([`shift_method_indent`]) when the
/// source does not re-parse cleanly — a malformed or partial body must still get
/// *some* re-indent rather than be dropped. This fallback also covers a body
/// carrying a leading `@expect` directive: `parse_method` does not consume a
/// top-level `@expect` (it is attached by the class-body parser, not
/// `parse_method_definition`), so such a body re-parses as `None` and is shifted
/// rather than re-laid-out. A width-sensitive `@expect` method (rare — `@expect`
/// is mostly on narrow test methods) could therefore still diverge from disk; the
/// corpus round-trip test would catch it if any such method existed.
#[must_use]
pub fn reindent_method_source(base_indent: &str, source: &str) -> String {
    let (method, diags) = parse_method(lex_with_eof(source));
    let Some(method) = method.filter(|_| !diags.iter().any(|d| d.severity == Severity::Error))
    else {
        return shift_method_indent(base_indent, source);
    };

    // The indent costs one column per char (spaces/tabs are single-column here);
    // reducing the budget by it makes break decisions as if rendered at the indent.
    // `max(0)` clamps the (unreachable in practice) case of an indent wider than
    // the line budget, where the pretty-printer would otherwise see a negative width.
    let indent_cols = isize::try_from(base_indent.chars().count()).unwrap_or(DEFAULT_LINE_WIDTH);
    let width = (DEFAULT_LINE_WIDTH - indent_cols).max(0);
    let rendered =
        unparse_method_definition_inner(&method, class_prefix(&method), EmitLeadingComments::No)
            .to_pretty_string_width(width);

    let reindented = shift_method_indent(base_indent, &rendered);
    // `to_pretty_string_width` never emits a trailing newline; restore the
    // source's so callers (the install hook's trailing-newline match) see the
    // same shape the old reshape produced.
    if source.ends_with('\n') && !reindented.ends_with('\n') {
        let mut out = reindented;
        out.push('\n');
        out
    } else {
        reindented
    }
}

/// Strips the shared leading indentation from `source` and re-prepends
/// `base_indent` to every non-blank line, preserving relative indentation. Blank
/// lines stay empty (no indent, no trailing whitespace; a CRLF blank line keeps
/// its `\r`). This is the pure whitespace shift — used to apply an
/// already-correct layout's indentation, and as the fallback in
/// [`reindent_method_source`] when re-layout is not possible.
///
/// A line that starts inside a token — the continuation lines of a raw
/// multi-line string literal (or quoted symbol) — is part of that token's
/// value, so it is copied verbatim and plays no part in the shared indent
/// (BT-5183). The source need not parse; the lexer alone finds such lines.
#[must_use]
fn shift_method_indent(base_indent: &str, source: &str) -> String {
    let inside_token = lines_starting_inside_a_token(source);
    let min_indent = source
        .split('\n')
        .enumerate()
        .filter(|(i, line)| !inside_token.contains(i) && !is_blank_line(line))
        .map(|(_, line)| leading_ws_len(line))
        .min()
        .unwrap_or(0);

    let mut out = String::with_capacity(source.len() + source.len() / 8);
    for (i, line) in source.split('\n').enumerate() {
        if i > 0 {
            out.push('\n');
        }
        if inside_token.contains(&i) {
            out.push_str(line);
            continue;
        }
        if is_blank_line(line) {
            // Blank lines stay empty — no base indent, no trailing whitespace —
            // but keep a CRLF line ending's `\r`.
            if line.ends_with('\r') {
                out.push('\r');
            }
            continue;
        }
        out.push_str(base_indent);
        // `min_indent` counts ASCII whitespace bytes, which are single-byte, so
        // the byte slice is always on a char boundary.
        out.push_str(&line[min_indent..]);
    }
    out
}

/// The indices of the `\n`-separated lines of `source` whose first byte lies
/// strictly inside a string or symbol literal (a raw multi-line string's
/// continuation lines).
fn lines_starting_inside_a_token(source: &str) -> std::collections::HashSet<usize> {
    // Tokens come out of the lexer in source order and do not overlap, so
    // one forward walk over them serves every line start.
    // Only literal tokens hold source text as their value. An unterminated
    // string lexes as an error token running to the end of the input; the
    // lines after it are still code to re-indent.
    let spans: Vec<Span> = lex_with_eof(source)
        .iter()
        .filter(|t| {
            matches!(
                t.kind(),
                TokenKind::String(_)
                    | TokenKind::StringStart(_)
                    | TokenKind::StringSegment(_)
                    | TokenKind::StringEnd(_)
                    | TokenKind::Symbol(_)
            )
        })
        .map(crate::source_analysis::Token::span)
        .collect();
    let mut next = 0usize;
    let mut out = std::collections::HashSet::new();
    let mut line_start = 0usize;
    for (i, line) in source.split('\n').enumerate() {
        let offset = u32::try_from(line_start).unwrap_or(u32::MAX);
        while next < spans.len() && spans[next].end() <= offset {
            next += 1;
        }
        if spans
            .get(next)
            .is_some_and(|span| span.start() < offset && offset < span.end())
        {
            out.insert(i);
        }
        line_start += line.len() + 1;
    }
    out
}

/// The number of leading space/tab bytes in `line`.
fn leading_ws_len(line: &str) -> usize {
    line.bytes()
        .take_while(|&b| b == b' ' || b == b'\t')
        .count()
}

/// Whether `line` is blank: empty or only spaces/tabs, allowing the `\r` of
/// a CRLF line ending.
fn is_blank_line(line: &str) -> bool {
    let line = line.strip_suffix('\r').unwrap_or(line);
    leading_ws_len(line) == line.len()
}

/// Unparses a method signature for help display.
///
/// Renders `selector params -> ReturnType` without `sealed` prefix or ` =>` suffix.
/// Used by codegen to embed display signatures in `methodSignatures` maps.
#[must_use]
pub fn unparse_method_display_signature(method: &MethodDefinition) -> String {
    unparse_method_display_signature_doc(method).to_pretty_string()
}

/// Formats a Beamtalk source string using the unparser.
///
/// Runs the full pipeline: lex → parse → error-check → unparse → ensure
/// trailing newline → verify. Returns [`FormatError::ParseErrors`] if the
/// source has any `Severity::Error` diagnostic (formatting a broken file
/// could corrupt it). Otherwise the formatted text is checked against the
/// input before it is returned (BT-5183): it must re-parse cleanly, keep
/// every comment and parse to the same program. If any check fails the
/// formatter has a bug, and the error says what would have been lost, so
/// callers never write a lossy result to disk.
///
/// The output is either empty or ends with a trailing newline. This function
/// is idempotent: formatting an already-formatted string produces the same
/// output.
///
/// # Errors
///
/// See [`FormatError`].
pub fn format_source(source: &str) -> Result<String, FormatError> {
    let tokens = lex_with_eof(source);
    let (module, diags) = parse(tokens);

    let has_errors = diags.iter().any(|d| d.severity == Severity::Error);
    if has_errors {
        return Err(FormatError::ParseErrors);
    }

    let formatted = unparse_module(&module);
    let formatted = if formatted.is_empty() || formatted.ends_with('\n') {
        formatted
    } else {
        format!("{formatted}\n")
    };
    let formatted = apply_line_endings(&formatted, layout_line_ending(source));

    round_trip::verify_round_trip(source, &module, &formatted)?;
    Ok(formatted)
}

/// The line ending of `source`'s layout (BT-5183): the shared
/// [`line_ending`] rule applied to the text outside multi-line literal
/// tokens, so a CRLF written inside a string or quoted symbol of an LF file
/// does not make the file count as CRLF.
fn layout_line_ending(source: &str) -> &'static str {
    let mut layout = String::with_capacity(source.len());
    for piece in split_at_multiline_tokens(source) {
        if let SourcePiece::Layout(text) = piece {
            layout.push_str(text);
        }
    }
    line_ending(&layout)
}

/// Rewrites every line break of the printer's output to `eol` (`"\n"` or
/// `"\r\n"`, from [`layout_line_ending`]; BT-5183).
///
/// The printer emits `\n` for its own layout but copies comment text
/// verbatim, so a CRLF file's comments arrive with a `\r` still attached.
/// String literals print CR and LF as `\r` and `\n` escapes, but a quoted
/// symbol (`#'a<newline>b'`) keeps its raw line break, which is part of its
/// value: a break inside such a token is copied unchanged.
fn apply_line_endings(printed: &str, eol: &str) -> String {
    let mut out = String::with_capacity(printed.len() + printed.len() / 16);
    for piece in split_at_multiline_tokens(printed) {
        match piece {
            SourcePiece::Layout(text) => {
                let lf = text.replace("\r\n", "\n");
                if eol == "\n" {
                    out.push_str(&lf);
                } else {
                    out.push_str(&lf.replace('\n', eol));
                }
            }
            SourcePiece::Token(text) => out.push_str(text),
        }
    }
    out
}

/// A piece of source text: layout (code, whitespace and comments) or the
/// whole of a token whose value holds a raw line break.
enum SourcePiece<'a> {
    Layout(&'a str),
    Token(&'a str),
}

/// `text` cut into alternating [`SourcePiece`]s, in order.
fn split_at_multiline_tokens(text: &str) -> Vec<SourcePiece<'_>> {
    let mut pieces = Vec::new();
    let mut copied = 0usize;
    for span in lex_with_eof(text)
        .iter()
        .map(crate::source_analysis::Token::span)
        .filter(|span| text[span.as_range()].contains('\n'))
    {
        pieces.push(SourcePiece::Layout(&text[copied..span.start() as usize]));
        pieces.push(SourcePiece::Token(&text[span.as_range()]));
        copied = span.end() as usize;
    }
    pieces.push(SourcePiece::Layout(&text[copied..]));
    pieces
}

/// Escapes an arbitrary string for embedding inside a Beamtalk double-quoted
/// string literal.
///
/// This is the one convention for tool-generated code (tool expressions, MCP
/// editing/evaluate surfaces, LSP commands): embedded quotes double (`""`),
/// matching the unparser's `string_content` leaf helper and `String
/// printString` (ADR 0138 "One mapping, every direction"), so generated
/// expressions are `fmt`-stable — `fmt` canonicalizes every embedded quote to
/// `""` doubling, and `\"` would not survive a format pass. Beamtalk strings
/// also treat `{` as the start of an interpolation sequence (ADR 0023), so a
/// literal `{` must be written as `\{`; a bare `}` needs no escape.
///
/// The escape order is significant: `\\` must be processed first so a caller-
/// supplied `\{` is rendered as `\\\{` (escaped backslash `\\` then escaped
/// brace `\{`) rather than `\\{` (escaped backslash followed by an unescaped
/// `{` that would start interpolation).
///
/// Control characters (CR/LF/TAB/NUL) deliberately pass through verbatim —
/// Beamtalk strings are multi-line, and generated expressions embed multi-line
/// sources readably. They still round-trip: raw controls re-lex to themselves.
/// This is the one intentional divergence from `string_content` and
/// `printString`, which use the `\r` `\n` `\t` `\0` spellings.
#[must_use]
pub fn escape_string_literal(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\"\"")
        .replace('{', "\\{")
}

/// Renders a type annotation to its Beamtalk display form (e.g. `Integer`,
/// `String | Nil`, `List(Integer)`).
///
/// Used by codegen to build the `__signature__` string for value-type
/// auto-accessors, whose slot types come straight from the `StateDeclaration`
/// annotation rather than a full `MethodDefinition`.
#[must_use]
pub fn unparse_type_annotation_display(ty: &TypeAnnotation) -> String {
    unparse_type_annotation(ty).to_pretty_string()
}

/// Renders a literal AST node as Beamtalk source syntax (e.g. `"a ""quoted"" string"`,
/// `#'with space'`, `$\n`, `1.0`).
///
/// This is the single source of truth for literal-to-source rendering — used by
/// codegen doc comments, hover info, and generated stdlib metadata alike, so all
/// three agree on escaping/quoting rules instead of drifting.
#[must_use]
pub fn unparse_literal_display(lit: &Literal) -> String {
    unparse_literal(lit).to_pretty_string()
}

/// Builds a [`Document`] for a method display signature (no `sealed`, no ` =>`).
fn unparse_method_display_signature_doc(method: &MethodDefinition) -> Document<'static> {
    let sig = unparse_selector_and_params(&method.selector, &method.parameters);

    let return_type = if let Some(ret) = &method.return_type {
        docvec![" -> ", unparse_type_annotation(ret)]
    } else {
        nil()
    };

    docvec![sig, return_type]
}

/// Builds the selector-and-parameters portion of a signature — no return
/// type, no `sealed`/`internal`/`class` prefix, no trailing ` =>`: `foo`,
/// `+ other :: Number`, or `at: index :: Integer put: value`.
///
/// This is the one place that interleaves keyword parts with parameter
/// names/types — shared by [`unparse_method_display_signature_doc`]
/// (and, through it, [`unparse_method_signature`]) and
/// [`unparse_protocol_method_signature`], rather than each keeping its own
/// independently-drifting copy of the same loop. A missing binary
/// parameter (`parameters.first()` returns `None` — not producible by the
/// parser for a real method definition, but reachable for a
/// hand-constructed/synthesized one) degrades to the bare operator rather
/// than panicking, matching protocol signatures' existing behaviour and
/// AGENTS.md's "never panic on user input" rule.
fn unparse_selector_and_params(
    selector: &MessageSelector,
    parameters: &[ParameterDefinition],
) -> Document<'static> {
    match selector {
        MessageSelector::Unary(name) => leaf::ident(name),
        MessageSelector::Binary(op) => {
            if let Some(param) = parameters.first() {
                docvec![
                    leaf::ident(op),
                    " ",
                    leaf::ident(&param.name.name),
                    unparse_type_annotation_opt(param.type_annotation.as_ref()),
                ]
            } else {
                leaf::ident(op)
            }
        }
        MessageSelector::Keyword(parts) => {
            let mut sig_docs: Vec<Document<'static>> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig_docs.push(Document::Str(" "));
                }
                sig_docs.push(unparse_keyword_part(part));
                if let Some(param) = parameters.get(i) {
                    sig_docs.push(Document::Str(" "));
                    sig_docs.push(leaf::ident(&param.name.name));
                    sig_docs.push(unparse_type_annotation_opt(param.type_annotation.as_ref()));
                }
            }
            concat(sig_docs)
        }
    }
}

// --- Document builders (pub(crate) for testing) ---

// --- Helper utilities ---

/// Joins a list of documents with a literal separator string.
fn join_docs(docs: Vec<Document<'static>>, sep: &'static str) -> Document<'static> {
    if docs.is_empty() {
        return nil();
    }
    let mut result: Vec<Document<'static>> = Vec::with_capacity(docs.len() * 2 - 1);
    let mut first = true;
    for doc in docs {
        if !first {
            result.push(Document::Str(sep));
        }
        result.push(doc);
        first = false;
    }
    concat(result)
}

/// Same as `join_docs` but returns a Vec for use with `docs.extend()`.
fn join_docs_vec(docs: Vec<Document<'static>>, sep: &'static str) -> Vec<Document<'static>> {
    if docs.is_empty() {
        return Vec::new();
    }
    let mut result: Vec<Document<'static>> = Vec::with_capacity(docs.len() * 2 - 1);
    let mut first = true;
    for doc in docs {
        if !first {
            result.push(Document::Str(sep));
        }
        result.push(doc);
        first = false;
    }
    result
}

// --- Tests ---
//
// All test code lives in `tests/`, split by feature: the per-feature files
// above, plus the property-based (`property_tests`) and corpus-wide
// conformance (`corpus_conformance_tests`) suites.
#[cfg(test)]
mod tests;
