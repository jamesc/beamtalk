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
//! - `beamtalk fmt` CLI command (Issue BT-978)
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
//!   string concatenation (CLAUDE.md / ADR 0018).
//! - Text leaves derived from AST data (identifiers, literal values, comments,
//!   etc.) are constructed through the intent-carrying [`leaf`] helpers. After
//!   ADR 0089 Phase 3 removed the open `Document::String` escape hatch, [`leaf`]
//!   is the single place in the unparser that constructs an owned-string leaf.

mod leaf;
mod signature_text;

pub use signature_text::{
    SignatureParam, SignatureRenderOptions, SignatureSelector, render_signature_text,
};

use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, ClassDefinition, Comment, CommentAttachment, CommentKind, ExpectCategory,
    Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MapPair, MapPatternKey,
    MatchArm, MessageSelector, MethodDefinition, Module, ParameterDefinition, Pattern,
    ProtocolDefinition, ProtocolMethodSignature, StandaloneMethodDefinition, StateDeclaration,
    StringSegment, TypeAliasDefinition, TypeAnnotation,
};
use crate::source_analysis::{Severity, lex_with_eof, parse, parse_method};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{DEFAULT_LINE_WIDTH, Document, break_, concat, group, line, nest, nil};

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
    // byte span the resolver assigns it (ADR 0082 / BT-2584: `source_ref ==
    // disk[span]`). That span deliberately starts at the method's `///` doc
    // block or its own line, excluding any leading non-doc `//` comments —
    // notably `// === section ===` dividers, which are inter-method file
    // structure, not part of the method (BT-2577). Emitting them here would
    // make the stored/compiled source diverge from disk, so a no-op cockpit
    // save/flush would duplicate the divider (BT-2594). Whole-file unparse
    // (`unparse_class` / `unparse_module`) still preserves them in place.
    // A future change will surface section dividers as first-class method
    // categories instead of free comments (BT-2601).
    //
    // The `class ` prefix is emitted from `method.is_class_method` so a class-side
    // method's stored source matches its on-disk span (which includes `class `);
    // whole-file unparse supplies the prefix from its own context instead (BT-2594).
    unparse_method_definition_inner(method, class_prefix(method), EmitLeadingComments::No)
        .to_pretty_string()
}

/// The signature prefix for a stand-alone per-method render: `class ` for a
/// class-side method, nothing otherwise. Whole-file unparse passes its own
/// prefix and does not use this (BT-2594).
fn class_prefix(method: &MethodDefinition) -> Document<'static> {
    if method.is_class_method {
        Document::Str("class ")
    } else {
        nil()
    }
}

/// Unparses a [`ClassDefinition`] to source text.
#[must_use]
pub fn unparse_class(class: &ClassDefinition) -> String {
    unparse_class_definition(class).to_pretty_string()
}

/// Re-lays-out a canonical (column-0) method source at `base_indent`, so the
/// result is byte-identical to what `bt fmt` (`unparse_module`) produces for the
/// same method on disk at that indentation (ADR 0082 / BT-2584 / BT-2594).
///
/// [`unparse_method`] renders a method at **column 0**, where the pretty-printer
/// makes line-break decisions against the full 80-column budget. On disk the same
/// method is indented under its class body, so it has `base_indent` fewer columns
/// available and a line that fit inline at column 0 must break. A pure
/// whitespace shift (the original BT-2584 behaviour) cannot *re-break* such a
/// line, so the stored `source_ref` diverged from the on-disk span for any
/// width-sensitive method — flushing it would reformat the file (BT-2594).
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
/// lines stay empty (no indent, no trailing whitespace). This is the pure
/// whitespace shift — used to apply an already-correct layout's indentation, and
/// as the fallback in [`reindent_method_source`] when re-layout is not possible.
#[must_use]
fn shift_method_indent(base_indent: &str, source: &str) -> String {
    let min_indent = source
        .split('\n')
        .filter(|line| !is_blank_line(line))
        .map(leading_ws_len)
        .min()
        .unwrap_or(0);

    let mut out = String::with_capacity(source.len() + source.len() / 8);
    let mut first = true;
    for line in source.split('\n') {
        if !first {
            out.push('\n');
        }
        first = false;
        if is_blank_line(line) {
            // Blank lines stay empty — no base indent, no trailing whitespace.
            continue;
        }
        out.push_str(base_indent);
        // `min_indent` counts ASCII whitespace bytes, which are single-byte, so
        // the byte slice is always on a char boundary.
        out.push_str(&line[min_indent..]);
    }
    out
}

/// The number of leading space/tab bytes in `line`.
fn leading_ws_len(line: &str) -> usize {
    line.bytes()
        .take_while(|&b| b == b' ' || b == b'\t')
        .count()
}

/// Whether `line` is blank: empty or only spaces/tabs.
fn is_blank_line(line: &str) -> bool {
    leading_ws_len(line) == line.len()
}

/// Unparses a method signature for help display (BT-988).
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
/// trailing newline. Returns `None` if the source has any `Severity::Error`
/// diagnostic (formatting a broken file could corrupt it). Otherwise returns
/// the formatted string, which is either empty or ends with a trailing
/// newline.
///
/// This function is idempotent: formatting an already-formatted string
/// produces the same output.
#[must_use]
pub fn format_source(source: &str) -> Option<String> {
    let tokens = lex_with_eof(source);
    let (module, diags) = parse(tokens);

    let has_errors = diags.iter().any(|d| d.severity == Severity::Error);
    if has_errors {
        return None;
    }

    let formatted = unparse_module(&module);
    let formatted = if formatted.is_empty() || formatted.ends_with('\n') {
        formatted
    } else {
        format!("{formatted}\n")
    };

    Some(formatted)
}

/// Escapes an arbitrary string for embedding inside a Beamtalk double-quoted
/// string literal.
///
/// Beamtalk strings share `\\` and `"` escaping with most languages, and also
/// treat `{` as the start of an interpolation sequence (ADR 0023), so a
/// literal `{` must be written as `\{`.
///
/// The escape order is significant: `\\` must be processed first so a caller-
/// supplied `\{` is rendered as `\\\{` (escaped backslash `\\` then escaped
/// brace `\{`) rather than `\\{` (escaped backslash followed by an unescaped
/// `{` that would start interpolation).
#[must_use]
pub fn escape_string_literal(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('{', "\\{")
}

/// Renders a type annotation to its Beamtalk display form (e.g. `Integer`,
/// `String | Nil`, `List(Integer)`).
///
/// Used by codegen (BT-2734) to build the `__signature__` string for value-type
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
/// three agree on escaping/quoting rules instead of drifting (BT-3088).
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
/// names/types (BT-3097) — shared by [`unparse_method_display_signature_doc`]
/// (and, through it, [`unparse_method_signature`]) and
/// [`unparse_protocol_method_signature`], which previously carried
/// independently-drifting copies of the same loop. A missing binary
/// parameter (`parameters.first()` returns `None` — not producible by the
/// parser for a real method definition, but reachable for a
/// hand-constructed/synthesized one) degrades to the bare operator rather
/// than panicking, matching protocol signatures' existing behaviour and
/// CLAUDE.md's "never panic on user input" rule.
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

/// One of the three top-level declaration kinds that can appear in a
/// [`Module`] outside of standalone methods and expressions.
///
/// Used only by [`unparse_module_doc`] to interleave classes, protocols, and
/// type aliases back into their original source order (BT-2907) rather than
/// grouping them by kind.
enum TopLevelDecl<'a> {
    Class(&'a ClassDefinition),
    Protocol(&'a ProtocolDefinition),
    TypeAlias(&'a TypeAliasDefinition),
}

impl TopLevelDecl<'_> {
    /// Whether a blank line preceded this declaration in the source (BT-2929).
    fn preceding_blank_line(&self) -> bool {
        match self {
            Self::Class(class) => class.comments.leading_blank_line,
            Self::Protocol(protocol) => protocol.comments.leading_blank_line,
            Self::TypeAlias(type_alias) => type_alias.comments.leading_blank_line,
        }
    }
}

/// Builds a [`Document`] for a [`Module`].
#[must_use]
pub(crate) fn unparse_module_doc(module: &Module) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // File-level leading comments (empty module edge case — ADR 0044)
    for comment in &module.file_leading_comments {
        docs.push(unparse_comment(comment));
        docs.push(line());
    }

    // Classes, protocols, and type aliases (ADR 0068 Phase 2a, ADR 0108 Phase 1)
    // are interleaved back into their original source order (BT-2907) rather
    // than grouped by kind — a `type` alias declared after a class must stay
    // after that class on a format round-trip. `sort_by_key` is a stable
    // sort, so declarations that legitimately share a start offset
    // (shouldn't happen for top-level decls) keep this append order:
    // classes, then protocols, then type aliases.
    let mut top_level_decls: Vec<TopLevelDecl<'_>> = Vec::new();
    top_level_decls.extend(module.classes.iter().map(TopLevelDecl::Class));
    top_level_decls.extend(module.protocols.iter().map(TopLevelDecl::Protocol));
    top_level_decls.extend(module.type_aliases.iter().map(TopLevelDecl::TypeAlias));
    top_level_decls.sort_by_key(|decl| match decl {
        TopLevelDecl::Class(class) => class.span.start(),
        TopLevelDecl::Protocol(protocol) => protocol.span.start(),
        TopLevelDecl::TypeAlias(type_alias) => type_alias.span.start(),
    });

    for (i, decl) in top_level_decls.into_iter().enumerate() {
        // BT-2929: re-emit the blank line the author placed between this
        // declaration and the previous one. Skipped for the first
        // declaration in the section — a blank line there (before the very
        // first top-level declaration) is dropped, same as pre-existing
        // behaviour; we only preserve inter-declaration gaps.
        if i > 0 && decl.preceding_blank_line() {
            docs.push(line());
        }
        match decl {
            TopLevelDecl::Class(class) => docs.push(unparse_class_definition(class)),
            TopLevelDecl::Protocol(protocol) => docs.push(unparse_protocol_definition(protocol)),
            TopLevelDecl::TypeAlias(type_alias) => {
                docs.push(unparse_type_alias_definition(type_alias));
            }
        }
        docs.push(line());
    }

    // Standalone method definitions
    for (i, smd) in module.method_definitions.iter().enumerate() {
        // BT-2943: re-emit the blank line the author placed before this
        // standalone method — either separating it from the previous
        // standalone method (i > 0), or separating the whole
        // standalone-methods section from the class/protocol/type-alias
        // declarations above it (i == 0, but `docs` already has content).
        // Dropped when nothing precedes it (same first-item convention as
        // the declarations loop above, BT-2929).
        if (i > 0 || !docs.is_empty()) && smd.method.comments.leading_blank_line {
            docs.push(line());
        }
        docs.push(unparse_standalone_method_definition(smd));
        docs.push(line());
    }

    // Top-level expressions (script / REPL)
    for (i, stmt) in module.expressions.iter().enumerate() {
        if i > 0 {
            // BT-987: emit an extra blank line if present in source
            if stmt.preceding_blank_line {
                docs.push(line());
            }
            docs.push(line());
        } else if !docs.is_empty() && stmt.preceding_blank_line {
            // BT-2943: preserve a blank line separating the first top-level
            // expression from the declarations/standalone-methods section(s)
            // above it. The i > 0 branch above already provides its own
            // separator `line()` between expressions; here, the previous
            // section's trailing `line()` already plays that role, so a
            // single extra `line()` is enough to open up the blank line.
            docs.push(line());
        }
        docs.push(unparse_expression_statement(stmt));
    }

    // File-level trailing comments (after the last class/method/expression)
    for comment in &module.file_trailing_comments {
        if comment.preceding_blank_line {
            docs.push(line());
        }
        docs.push(unparse_comment(comment));
        docs.push(line());
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`ClassDefinition`].
///
/// Emits non-doc comments, then the optional doc comment, then the class header,
/// state declarations, and methods.
#[must_use]
#[allow(clippy::too_many_lines)]
pub(crate) fn unparse_class_definition(class: &ClassDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&class.comments));

    // Blank line between leading comments (e.g. license block) and doc/header:
    // either because the source actually had one there (BT-2945), or because
    // the last leading entry is an orphaned `///` block that must, by
    // construction, always be separated from what follows (BT-2924) — see
    // `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&class.comments)
        || class.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &class.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Class header: `Superclass subclass: ClassName`
    let superclass = class
        .superclass
        .as_ref()
        .map_or_else(|| "nil".to_string(), |s| s.name.to_string());

    let mut modifiers: Vec<Document<'static>> = Vec::new();
    if class.is_internal {
        modifiers.push(Document::Str("internal "));
    }
    if class.is_abstract {
        modifiers.push(Document::Str("abstract "));
    }
    if class.is_sealed {
        modifiers.push(Document::Str("sealed "));
    }
    if class.is_typed {
        modifiers.push(Document::Str("typed "));
    }

    let class_name_doc = if class.type_params.is_empty() {
        leaf::ident(&class.name.name)
    } else {
        let params: Vec<Document<'static>> = class
            .type_params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let param_doc = if let Some(ref bound) = p.bound {
                    docvec![leaf::ident(&p.name.name), " :: ", leaf::ident(&bound.name)]
                } else {
                    leaf::ident(&p.name.name)
                };
                if i == 0 {
                    param_doc
                } else {
                    docvec![", ", param_doc]
                }
            })
            .collect();
        docvec![leaf::ident(&class.name.name), "(", concat(params), ")"]
    };

    // Emit superclass type args: `Collection(E)` or `Collection(Integer)`
    let superclass_with_type_args = if class.superclass_type_args.is_empty() {
        leaf::ident(&superclass)
    } else {
        let mut parts = vec![leaf::ident(&superclass), Document::Str("(")];
        for (i, ta) in class.superclass_type_args.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::ident(ta.type_name()));
        }
        parts.push(Document::Str(")"));
        concat(parts)
    };

    let class_header = docvec![
        concat(modifiers),
        superclass_with_type_args,
        " subclass: ",
        class_name_doc,
    ];

    let header = if let Some(module) = &class.backing_module {
        docvec![class_header, " native: ", leaf::ident(&module.name)]
    } else {
        class_header
    };

    let header = if let Some(trail) = &class.comments.trailing {
        docvec![header, "  ", unparse_comment(trail)]
    } else {
        header
    };

    // `handleScope: #symbol` (ADR 0103) — canonical style puts it on its own
    // indented line following the class header (and its trailing comment, if
    // any), mirroring `docs/ADR/0103-sendability-typing-from-class-kinds.md`.
    let header = if let Some(hs) = &class.handle_scope {
        docvec![
            header,
            nest(2, docvec![line(), "handleScope: #", leaf::ident(&hs.name)])
        ]
    } else {
        header
    };

    docs.push(header);

    // State declarations — use nest(2, ...) so that leading comments and
    // doc-comment lines inside the declaration are also indented at column 2,
    // matching the `line()` inside nest() pattern used for instance methods.
    for state in &class.state {
        docs.push(nest(2, docvec![line(), unparse_state_declaration(state)]));
    }

    // Class variables
    for state in &class.class_variables {
        docs.push(nest(
            2,
            docvec![line(), unparse_class_state_declaration(state)],
        ));
    }

    // Blank line before first method (always, regardless of whether state is present)
    if !class.methods.is_empty() || !class.class_methods.is_empty() {
        docs.push(line());
    }

    // Class-side methods (before instance methods)
    for (i, method) in class.class_methods.iter().enumerate() {
        if i > 0 {
            // Blank line between consecutive class-side methods
            docs.push(line());
        }
        docs.push(nest(
            2,
            docvec![
                line(),
                unparse_method_definition_with_prefix(method, Document::Str("class "))
            ],
        ));
    }

    // Blank line between last class-side method and first instance method
    if !class.methods.is_empty() && !class.class_methods.is_empty() {
        docs.push(line());
    }

    // Instance methods — the line() is placed INSIDE nest(2, ...) so it renders
    // at indent=2, giving the leading comment and method signature their correct
    // 2-space indentation.  A leading comment's trailing line() also runs at
    // indent=2, so the signature is never shifted to column 0.
    for (i, method) in class.methods.iter().enumerate() {
        if i > 0 {
            // Blank line between consecutive methods
            docs.push(line());
        }
        docs.push(nest(2, docvec![line(), unparse_method_definition(method)]));
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`StandaloneMethodDefinition`].
///
/// Example: `Counter >> increment => self.value := self.value + 1`
#[must_use]
pub(crate) fn unparse_standalone_method_definition(
    smd: &StandaloneMethodDefinition,
) -> Document<'static> {
    let class = leaf::ident(&smd.class_name.name);
    // Cross-package extension methods (ADR 0070): `package@ClassName >> ...`.
    // Found while adding BT-2943's package-qualified regression test:
    // `smd.package` was previously never consulted here, so `beamtalk fmt`
    // silently dropped the package qualifier from any cross-package
    // standalone method — a pre-existing, unrelated bug, fixed alongside
    // this issue since the fix is a one-line addition and the test that
    // caught it already lives in this file.
    let class = if let Some(package) = &smd.package {
        docvec![leaf::ident(&package.name), "@", class]
    } else {
        class
    };
    let separator = if smd.is_class_method {
        Document::Str(" class >> ")
    } else {
        Document::Str(" >> ")
    };
    docvec![class, separator, unparse_method_definition(&smd.method)]
}

/// Builds a [`Document`] for a [`TypeAliasDefinition`] (ADR 0108 Phase 1).
///
/// Emits the optional doc comment followed by the declaration header
/// (`type Name = <TypeAnnotation>`).
#[must_use]
fn unparse_type_alias_definition(type_alias: &TypeAliasDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&type_alias.comments));

    // Blank line between this alias's leading comments and its own doc
    // comment/header: either because the source actually had one there
    // (BT-2945), or because the last leading entry is a preserved, earlier
    // `///` block that broke away from a different declaration (BT-2924) —
    // see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&type_alias.comments)
        || type_alias.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &type_alias.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // `(internal )?type Name = <TypeAnnotation>`
    let internal_prefix = if type_alias.is_internal {
        Document::Str("internal ")
    } else {
        Document::Nil
    };
    let header = docvec![
        internal_prefix,
        "type ",
        leaf::ident(&type_alias.name.name),
        " = ",
        unparse_type_annotation(&type_alias.annotation),
    ];

    // Trailing end-of-line comment on the declaration line, e.g.
    // `type Port = Integer // comment` (BT-2906).
    let header = if let Some(trail) = &type_alias.comments.trailing {
        docvec![header, "  ", unparse_comment(trail)]
    } else {
        header
    };

    docs.push(header);

    concat(docs)
}

/// Builds a [`Document`] for a [`ProtocolDefinition`] (ADR 0068 Phase 2a).
///
/// Emits the protocol header (`Protocol define: Name`) followed by
/// optional type parameters, `extending:` clause, and method signatures.
#[must_use]
fn unparse_protocol_definition(protocol: &ProtocolDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&protocol.comments));

    // Blank line between this protocol's leading comments and its own doc
    // comment/header: either because the source actually had one there
    // (BT-2945), or because the last leading entry is a preserved, earlier
    // `///` block that broke away from a different declaration (BT-2924) —
    // see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&protocol.comments)
        || protocol.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &protocol.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Protocol header: `Protocol define: Name`
    let mut header: Vec<Document<'static>> = vec![
        Document::Str("Protocol define: "),
        leaf::ident(&protocol.name.name),
    ];

    // Type parameters: `(E)`, `(K, V)`
    if !protocol.type_params.is_empty() {
        header.push(Document::Str("("));
        for (i, tp) in protocol.type_params.iter().enumerate() {
            if i > 0 {
                header.push(Document::Str(", "));
            }
            header.push(leaf::ident(&tp.name.name));
        }
        header.push(Document::Str(")"));
    }

    docs.push(Document::Vec(header));

    // Trailing end-of-line comment on the declaration header line, e.g.
    // `Protocol define: Sortable // comment` (BT-2906). Emitted right after
    // the header, matching where the parser collects it — before
    // `extending:`/the body, which start on their own indented lines.
    if let Some(trail) = &protocol.comments.trailing {
        docs.push(Document::Str("  "));
        docs.push(unparse_comment(trail));
    }

    // `extending: ParentProtocol`
    if let Some(ext) = &protocol.extending {
        docs.push(line());
        docs.push(docvec!["  extending: ", leaf::ident(&ext.name)]);
    }

    // Instance and class method signatures (indented by 2 spaces). Tracked
    // across both lists with a single `first_signature` flag so a blank
    // line is preserved at the instance/class boundary too (a `class`
    // signature immediately after an instance signature is just as much
    // an "inter-signature gap" as two instance signatures).
    let mut first_signature = true;
    for sig in &protocol.method_signatures {
        docs.push(nest(
            2,
            unparse_protocol_signature_entry(sig, None, &mut first_signature),
        ));
    }

    // Class method signatures (BT-1611, indented by 2 spaces, prefixed with `class`)
    for sig in &protocol.class_method_signatures {
        docs.push(nest(
            2,
            unparse_protocol_signature_entry(sig, Some("class "), &mut first_signature),
        ));
    }

    concat(docs)
}

/// Builds the `Document` for a single protocol method signature entry
/// within `unparse_protocol_definition`'s signature list — the separating
/// `line()` before it, plus (BT-2946) an extra blank line re-emitted when
/// the signature had one before it in the source. Skipped for the very
/// first signature in the body — a blank line there (between the header/
/// `extending:` clause and the first signature) is dropped, same as
/// pre-existing behaviour and mirroring BT-2929's top-level-declaration
/// fix; only inter-signature gaps are preserved.
fn unparse_protocol_signature_entry(
    sig: &ProtocolMethodSignature,
    prefix: Option<&'static str>,
    first_signature: &mut bool,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    if !*first_signature && sig.comments.leading_blank_line {
        docs.push(line());
    }
    docs.push(line());
    docs.push(unparse_protocol_method_signature(sig, prefix));
    *first_signature = false;
    concat(docs)
}

/// Builds a [`Document`] for a [`ProtocolMethodSignature`].
///
/// Protocol method signatures look like method definitions without `=>` and body:
/// `asString -> String`, `do: block :: Block(E, Object)`
///
/// An optional `prefix` (e.g. `"class "`) is inserted after any doc comment lines
/// but before the selector, so doc comments appear above the signature line.
fn unparse_protocol_method_signature(
    sig: &ProtocolMethodSignature,
    prefix: Option<&'static str>,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&sig.comments));

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration (BT-2924) and this signature's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&sig.comments) {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &sig.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Optional prefix (e.g. `class `) appears on the signature line, after doc comments
    if let Some(p) = prefix {
        docs.push(Document::Str(p));
    }

    // Selector and parameters (BT-3097: shared with method signatures via
    // `unparse_selector_and_params`, rather than an independent copy of the
    // same keyword/parameter interleaving loop).
    docs.push(unparse_selector_and_params(&sig.selector, &sig.parameters));

    // Return type
    if let Some(ref ret) = sig.return_type {
        docs.push(Document::Str(" -> "));
        docs.push(unparse_type_annotation(ret));
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`MethodDefinition`].
///
/// Emits:
/// 1. Non-doc leading comments (one per line)
/// 2. Doc comment lines (one per line)
/// 3. Method signature (`selector param =>`)
/// 4. Method body — single-expression (no leading comments) goes on the same
///    line; multi-expression bodies or bodies with leading comments go on new lines.
#[must_use]
pub(crate) fn unparse_method_definition(method: &MethodDefinition) -> Document<'static> {
    unparse_method_definition_with_prefix(method, nil())
}

/// Builds a method definition document with an optional prefix before the signature.
///
/// The prefix (e.g. `"class "`) is inserted between the comments/doc-comment and the
/// method signature, so that `class` appears on the signature line, not before the comments.
fn unparse_method_definition_with_prefix(
    method: &MethodDefinition,
    prefix: Document<'static>,
) -> Document<'static> {
    unparse_method_definition_inner(method, prefix, EmitLeadingComments::Yes)
}

/// Whether to emit a method's leading non-doc comments. Whole-file unparse keeps
/// them (file fidelity); the per-method [`unparse_method`] drops them so the
/// per-method source matches its byte span (BT-2594 — see `unparse_method`).
#[derive(Clone, Copy, PartialEq, Eq)]
enum EmitLeadingComments {
    Yes,
    No,
}

fn unparse_method_definition_inner(
    method: &MethodDefinition,
    prefix: Document<'static>,
    emit_leading: EmitLeadingComments,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments (section dividers etc.) — emitted only on the
    // whole-file path; the per-method edit unit excludes them (BT-2594).
    let emit_leading = emit_leading == EmitLeadingComments::Yes;
    if emit_leading {
        docs.extend(unparse_comment_attachment_leading(&method.comments));
    }

    // A method with no doc comment of its own still needs a separator after
    // a preserved, earlier `///` block that broke away from a different
    // declaration (BT-2924) — otherwise the orphaned block re-attaches to
    // this method as its doc comment on the next parse. When the method
    // *does* have its own doc comment, the blank-line push inside the `if
    // let Some(doc)` block below already covers this (it fires for any
    // non-empty leading comments, orphaned or not) — do not also push here,
    // or the separator doubles up.
    if method.doc_comment.is_none()
        && emit_leading
        && leading_ends_with_orphaned_doc_comment(&method.comments)
    {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &method.doc_comment {
        // Blank line between leading comments and doc comment (e.g. section separators)
        if emit_leading && !method.comments.leading.is_empty() {
            docs.push(line());
        }
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // BT-1856: Emit @expect directive before the method declaration
    if let Some((ref cats, ref reason, _)) = method.expect {
        let base = docvec!["@expect ", unparse_expect_categories(cats)];
        if let Some(reason) = reason {
            docs.push(docvec![base, " \"", leaf::string_content(reason), "\""]);
        } else {
            docs.push(base);
        }
        docs.push(line());
    }

    // Optional prefix (e.g. "class ") then method signature
    docs.push(prefix);
    docs.push(unparse_method_signature(method));

    // Body — single expression with no leading comments goes on the same line;
    // multi-expression bodies (or expressions with leading comments) go on new lines.
    match method.body.as_slice() {
        [] => {
            // Empty body — just the signature
            if let Some(trail) = &method.comments.trailing {
                docs.push(Document::Str("  "));
                docs.push(unparse_comment(trail));
            }
        }
        [single] if single.comments.leading.is_empty() => {
            // Single expression with no leading comments — try inline,
            // break to indented next line if too wide or if body is multi-line.
            let body = unparse_expression(&single.expression);
            let trail_doc = if let Some(trail) = &single.comments.trailing {
                docvec!["  ", unparse_comment(trail)]
            } else {
                nil()
            };
            // If the body renders as multi-line, always break to next line
            // to avoid half the expression dangling on the signature line.
            let body_str = body.to_pretty_string();
            if body_str.contains('\n') {
                docs.push(nest(2, docvec![line(), body, trail_doc]));
            } else {
                docs.push(group(docvec![nest(
                    2,
                    docvec![break_("", " "), body, trail_doc]
                ),]));
            }
        }
        stmts => {
            // Multiple expressions, or single with leading comments — emit on new lines
            if let Some(trail) = &method.comments.trailing {
                docs.push(Document::Str("  "));
                docs.push(unparse_comment(trail));
            }
            let mut body_docs: Vec<Document<'static>> = Vec::new();
            for stmt in stmts {
                // BT-987: emit an extra blank line before statements that had one in source
                if stmt.preceding_blank_line {
                    // Use a raw newline for blank lines to avoid trailing whitespace
                    // from indentation on empty lines.
                    body_docs.push(line());
                }
                body_docs.push(line());
                body_docs.extend(unparse_comment_attachment_leading(&stmt.comments));
                body_docs.push(unparse_expression(&stmt.expression));
                if let Some(trail) = &stmt.comments.trailing {
                    body_docs.push(Document::Str("  "));
                    body_docs.push(unparse_comment(trail));
                }
            }
            docs.push(nest(2, concat(body_docs)));
        }
    }

    concat(docs)
}

/// Builds the method signature (selector + parameters + return type + arrow).
///
/// Delegates the selector/parameters/return-type portion to
/// [`unparse_method_display_signature_doc`] and adds the declaration-only
/// wrapping: `sealed `/`internal ` prefixes and the trailing ` =>` (BT-3097).
fn unparse_method_signature(method: &MethodDefinition) -> Document<'static> {
    let sealed = if method.is_sealed {
        Document::Str("sealed ")
    } else {
        nil()
    };

    let internal = if method.is_internal {
        Document::Str("internal ")
    } else {
        nil()
    };

    docvec![
        internal,
        sealed,
        unparse_method_display_signature_doc(method),
        " =>"
    ]
}

fn unparse_keyword_part(part: &KeywordPart) -> Document<'static> {
    leaf::ident(&part.keyword)
}

/// Builds a [`Document`] for a [`StateDeclaration`].
///
/// Emits non-doc comments, doc comment, then `state: name [: Type] [= default]`.
#[must_use]
pub(crate) fn unparse_state_declaration(state: &StateDeclaration) -> Document<'static> {
    unparse_state_declaration_inner(state, false)
}

/// Builds a [`Document`] for a class-variable declaration.
fn unparse_class_state_declaration(state: &StateDeclaration) -> Document<'static> {
    unparse_state_declaration_inner(state, true)
}

fn unparse_state_declaration_inner(state: &StateDeclaration, is_class: bool) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&state.comments));

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration (BT-2924) and this field's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&state.comments) {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &state.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // BT-1856: Emit @expect directive before the declaration
    if let Some((ref cats, ref reason, _)) = state.expect {
        if let Some(reason) = reason {
            docs.push(docvec![
                "@expect ",
                unparse_expect_categories(cats),
                " \"",
                leaf::string_content(reason),
                "\""
            ]);
        } else {
            docs.push(docvec!["@expect ", unparse_expect_categories(cats)]);
        }
        docs.push(line());
    }

    let keyword = if is_class {
        "classState: "
    } else {
        state.declared_keyword.as_str()
    };
    let mut decl: Vec<Document<'static>> =
        vec![Document::Str(keyword), leaf::ident(&state.name.name)];

    if let Some(ty) = &state.type_annotation {
        decl.push(Document::Str(" :: "));
        decl.push(unparse_type_annotation(ty));
    }

    if let Some(default) = &state.default_value {
        decl.push(Document::Str(" = "));
        decl.push(unparse_expression(default));
    }

    // Trailing comment
    if let Some(trail) = &state.comments.trailing {
        decl.push(Document::Str("  "));
        decl.push(unparse_comment(trail));
    }

    docs.push(concat(decl));
    concat(docs)
}

/// Builds a [`Document`] for an [`ExpressionStatement`].
///
/// Emits leading comments (one per line before the expression) and
/// trailing comment (end of line after the expression).
#[must_use]
pub(crate) fn unparse_expression_statement(stmt: &ExpressionStatement) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Leading comments — each on its own line before the expression
    docs.extend(unparse_comment_attachment_leading(&stmt.comments));

    // The expression
    docs.push(unparse_expression(&stmt.expression));

    // Trailing comment — end of the same line
    if let Some(trail) = &stmt.comments.trailing {
        docs.push(Document::Str("  "));
        docs.push(unparse_comment(trail));
    }

    concat(docs)
}

/// Builds a [`Document`] for an [`Expression`].
#[must_use]
#[allow(clippy::too_many_lines)]
pub(crate) fn unparse_expression(expr: &Expression) -> Document<'static> {
    match expr {
        Expression::Literal(lit, _) => unparse_literal(lit),
        Expression::Identifier(id) => unparse_identifier(id),
        Expression::ClassReference { name, package, .. } => {
            leaf::class_ref(package.as_ref().map(|pkg| pkg.name.as_str()), &name.name)
        }
        Expression::Super(_) => Document::Str("super"),
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            docvec![unparse_expression(receiver), ".", leaf::ident(&field.name),]
        }
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            ..
        } => {
            // Flatten ++ chains for width-aware line breaking
            if matches!(selector, MessageSelector::Binary(op) if op.as_str() == "++") && !*is_cast {
                if let Some(doc) = unparse_concat_chain(expr) {
                    return doc;
                }
            }
            unparse_message_send(receiver, selector, arguments, *is_cast)
        }
        Expression::Block(block) => unparse_block(block),
        Expression::Assignment {
            target,
            value,
            type_annotation,
            ..
        } => {
            let target_doc = unparse_expression(target);
            if let Some(ann) = type_annotation {
                docvec![
                    target_doc,
                    " :: ",
                    unparse_type_annotation(ann),
                    " := ",
                    unparse_expression(value)
                ]
            } else {
                docvec![target_doc, " := ", unparse_expression(value)]
            }
        }
        Expression::Return { value, .. } => {
            docvec!["^", unparse_expression(value)]
        }
        Expression::Cascade {
            receiver, messages, ..
        } => unparse_cascade(receiver, messages),
        Expression::Parenthesized { expression, .. } => {
            docvec!["(", unparse_expression(expression), ")"]
        }
        Expression::Match {
            value,
            arms,
            exhaustive,
            ..
        } => unparse_match(value, arms, *exhaustive),
        Expression::MapLiteral { pairs, .. } => unparse_map_literal(pairs),
        Expression::ListLiteral { elements, tail, .. } => {
            unparse_list_literal(elements, tail.as_deref())
        }
        Expression::ArrayLiteral { elements, .. } => unparse_array_literal(elements),
        Expression::Primitive {
            name,
            is_quoted,
            is_intrinsic,
            is_inferred,
            ..
        } => {
            let directive = if *is_intrinsic {
                "@intrinsic"
            } else {
                "@primitive"
            };
            if *is_inferred {
                // Bare `@primitive` — selector inferred from the method (BT-2724).
                docvec![directive]
            } else if *is_quoted {
                docvec![directive, " \"", leaf::string_content(name), "\""]
            } else {
                docvec![directive, " ", leaf::ident(name)]
            }
        }
        Expression::StringInterpolation { segments, .. } => unparse_string_interpolation(segments),
        Expression::ExpectDirective {
            categories, reason, ..
        } => {
            let base = docvec!["@expect ", unparse_expect_categories(categories)];
            if let Some(reason) = reason {
                docvec![base, " \"", leaf::string_content(reason), "\""]
            } else {
                base
            }
        }
        Expression::Error { message, .. } => {
            // Emit a comment indicating the error rather than nothing.
            // Escape `*/` in the message to prevent breaking the block comment.
            let safe_msg = message.as_str().replace("*/", "* /");
            docvec!["/* error: ", leaf::raw_text(&safe_msg), " */"]
        }
        Expression::DestructureAssignment { pattern, value, .. } => {
            docvec![unparse_pattern(pattern), " := ", unparse_expression(value)]
        }
        Expression::Spread { name, .. } => {
            docvec!["...", leaf::ident(&name.name)]
        }
    }
}

// --- Literal unparsing ---

fn unparse_literal(lit: &Literal) -> Document<'static> {
    match lit {
        Literal::Integer(n) => leaf::int_lit(*n),
        Literal::Float(f) => leaf::float_lit(*f),
        Literal::String(s) => {
            // The lexer unescapes doubled delimiters ("" → ") in the AST.
            // We re-escape any bare " using the Beamtalk convention: double it ("").
            docvec!["\"", leaf::string_content(s), "\""]
        }
        Literal::Symbol(s) => {
            // Symbols: #name or #'name with spaces'
            if needs_symbol_quoting(s) {
                docvec!["#'", leaf::symbol_content(s), "'"]
            } else {
                docvec!["#", leaf::symbol_content(s)]
            }
        }
        Literal::List(items) => {
            // Literal list: #(1, 2, 3)
            if items.is_empty() {
                Document::Str("#()")
            } else {
                let item_docs: Vec<Document<'static>> = items.iter().map(unparse_literal).collect();
                let joined = join_docs(item_docs, ", ");
                docvec!["#(", joined, ")"]
            }
        }
        // $a, $\n, $\t etc. — re-escape control characters (see `leaf::char_lit`)
        Literal::Character(c) => leaf::char_lit(*c),
    }
}

/// Returns true if a symbol name needs quoting.
fn needs_symbol_quoting(s: &str) -> bool {
    s.is_empty()
        || s.contains(' ')
        || s.chars()
            .next()
            .is_none_or(|c| !c.is_alphabetic() && c != '_')
        || s.chars()
            .any(|c| !c.is_alphanumeric() && c != '_' && c != ':')
}

// --- Identifier unparsing ---

fn unparse_identifier(id: &Identifier) -> Document<'static> {
    leaf::ident(&id.name)
}

// --- Message send unparsing ---

/// Returns `true` if this expression is a block that will always render
/// across multiple lines — because it contains multiple statements, has a
/// trailing line comment, or its single expression renders to multiple lines
/// (e.g. a keyword send with multiline block arguments).
///
/// Used by `unparse_message_send` to decide whether keyword messages should
/// break each keyword to its own indented line.
fn block_renders_multiline(expr: &Expression) -> bool {
    let Expression::Block(block) = expr else {
        return false;
    };
    match block.body.as_slice() {
        // Multi-statement blocks always break.
        [_, _, ..] => true,
        // Single-statement block: forced-multiline when it has a trailing LINE
        // comment (which would otherwise land inside the `// …` text), or when
        // the formatted body itself spans multiple lines.
        //
        // Note: `unparse_expression` is called here purely for the multiline
        // predicate; the caller will unparse the block again when building the
        // actual output document.  This is a known O(n) duplication trade-off
        // kept intentionally simple until profiling shows it matters.
        [single] => {
            let has_trailing_line_comment = matches!(
                single.comments.trailing.as_ref().map(|c| c.kind),
                Some(CommentKind::Line)
            );
            has_trailing_line_comment
                || unparse_expression(&single.expression)
                    .to_pretty_string()
                    .contains('\n')
        }
        // Empty block `[]` stays inline.
        [] => false,
    }
}

fn unparse_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
) -> Document<'static> {
    let recv_doc = unparse_expression(receiver);
    let cast_suffix = if is_cast { Document::Str("!") } else { nil() };

    let msg = match selector {
        MessageSelector::Unary(name) => {
            docvec![recv_doc, " ", leaf::ident(name)]
        }
        MessageSelector::Binary(op) => {
            let arg = unparse_expression(&arguments[0]);
            docvec![recv_doc, " ", leaf::ident(op), " ", arg]
        }
        MessageSelector::Keyword(parts) => {
            // Break all keywords to their own indented lines when:
            // - 3+ keyword parts (elm-format style: always break multi-keyword messages), or
            // - any argument renders multi-line (block or otherwise — e.g. a
            //   parenthesized 3-keyword send that itself breaks).
            // Otherwise use the compact inline form (1-2 keywords, width-aware).
            //
            // For 3+ keywords, short-circuit to avoid re-unparsing every argument
            // just for the `any_multiline` predicate (it doesn't matter since
            // `parts.len() >= 3` guarantees a break regardless).
            let always_break = parts.len() >= 3
                || arguments.iter().any(|arg| {
                    block_renders_multiline(arg)
                        || unparse_expression(arg).to_pretty_string().contains('\n')
                });

            if always_break {
                let mut kw_docs: Vec<Document<'static>> = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    kw_docs.push(line());
                    let part_doc = unparse_keyword_part(part);
                    if i < arguments.len() {
                        kw_docs.push(docvec![part_doc, " ", unparse_expression(&arguments[i])]);
                    } else {
                        kw_docs.push(part_doc);
                    }
                }
                docvec![recv_doc, nest(2, concat(kw_docs))]
            } else {
                let mut docs: Vec<Document<'static>> = vec![recv_doc];
                for (i, part) in parts.iter().enumerate() {
                    docs.push(Document::Str(" "));
                    docs.push(unparse_keyword_part(part));
                    if i < arguments.len() {
                        docs.push(Document::Str(" "));
                        docs.push(unparse_expression(&arguments[i]));
                    }
                }
                concat(docs)
            }
        }
    };

    docvec![msg, cast_suffix]
}

// --- String concatenation chain (++) ---

/// Flattens a `++` chain from the nested AST into a list of segments,
/// then formats as all-inline or all-broken (one segment per line).
///
/// ```text
/// // Inline (fits):
/// "hello " ++ name ++ "!"
///
/// // Broken (doesn't fit):
/// "Arity mismatch: expected "
///   ++ params size printString
///   ++ ", got "
///   ++ vals size printString
/// ```
fn unparse_concat_chain(expr: &Expression) -> Option<Document<'static>> {
    let mut segments = Vec::new();
    collect_concat_segments(expr, &mut segments);
    if segments.len() < 2 {
        return None;
    }

    // Build the inline version to measure width
    let seg_docs: Vec<Document<'static>> = segments.iter().map(|s| unparse_expression(s)).collect();
    let inline = join_docs(seg_docs.clone(), " ++ ");
    let inline_width = inline.to_pretty_string().len();

    if inline_width <= 80 {
        Some(inline)
    } else {
        // One segment per line, continuation lines start with "++ "
        let first = seg_docs[0].clone();
        let mut continuation: Vec<Document<'static>> = Vec::new();
        for seg_doc in &seg_docs[1..] {
            continuation.push(line());
            continuation.push(docvec!["++ ", seg_doc.clone()]);
        }
        Some(docvec![first, nest(2, concat(continuation))])
    }
}

/// Walks left-nested `++` binary sends and collects the leaf expressions.
fn collect_concat_segments<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Binary(op),
        arguments,
        is_cast: false,
        ..
    } = expr
    {
        if op.as_str() == "++" && arguments.len() == 1 {
            collect_concat_segments(receiver, out);
            out.push(&arguments[0]);
            return;
        }
    }
    out.push(expr);
}

// --- Block unparsing ---

fn unparse_block(block: &Block) -> Document<'static> {
    // Build the parameter prefix: `:x :y | ` (if any)
    let params_doc: Document<'static> = if block.parameters.is_empty() {
        nil()
    } else {
        let params: Vec<Document<'static>> = block
            .parameters
            .iter()
            .map(unparse_block_parameter)
            .collect();
        let mut p = join_docs_vec(params, " ");
        p.push(Document::Str(" | "));
        concat(p)
    };

    match block.body.as_slice() {
        // Empty block: `[]`
        [] => docvec!["[", params_doc, "]"],

        // Multi-statement block: always break, statements separated by newlines.
        // Newlines act as statement separators — no `.` needed.
        // Closed bracket goes on its own line.
        stmts if stmts.len() > 1 => {
            let mut body_docs: Vec<Document<'static>> = Vec::new();
            for (i, stmt) in stmts.iter().enumerate() {
                if i > 0 {
                    // BT-987: emit an extra blank line if present in source
                    if stmt.preceding_blank_line {
                        body_docs.push(line());
                    }
                    body_docs.push(line());
                }
                // Leading comments
                body_docs.extend(unparse_comment_attachment_leading(&stmt.comments));
                // Expression
                body_docs.push(unparse_expression(&stmt.expression));
                // Trailing comment (on the same line)
                if let Some(trail) = &stmt.comments.trailing {
                    body_docs.push(Document::Str("  "));
                    body_docs.push(unparse_comment(trail));
                }
            }
            let body = concat(body_docs);
            docvec!["[", params_doc, nest(2, docvec![line(), body]), line(), "]"]
        }

        // Single-statement block with a trailing LINE comment must break:
        // rendering `[stmt // comment]` inline would put `]` inside the comment.
        [single]
            if matches!(
                single.comments.trailing.as_ref().map(|c| c.kind),
                Some(CommentKind::Line)
            ) =>
        {
            let body = unparse_expression_statement(single);
            docvec!["[", params_doc, nest(2, docvec![line(), body]), line(), "]"]
        }

        // Single-statement block: width-aware via group().
        // Fits on one line → `[expr]`; too long → broken across lines.
        // Exception: if the formatted body spans multiple lines (e.g. a keyword
        // send with multiline block args), always use the break form so the
        // `[:param |` header stays on one line, the body is indented, and `]`
        // lands on its own line (preventing `]]` stacking with outer closers).
        [single] => {
            let body = unparse_expression_statement(single);
            if body.to_pretty_string().contains('\n') {
                // Body renders multiline — always break so `]` gets its own line.
                docvec!["[", params_doc, nest(2, docvec![line(), body]), line(), "]"]
            } else {
                group(docvec![
                    "[",
                    params_doc,
                    nest(2, docvec![break_("", ""), body]),
                    break_("", ""),
                    "]",
                ])
            }
        }

        // Unreachable: the slice patterns above are exhaustive.
        _ => unreachable!(),
    }
}

fn unparse_block_parameter(param: &BlockParameter) -> Document<'static> {
    docvec![":", leaf::ident(&param.name)]
}

// --- Cascade unparsing ---

fn unparse_cascade_message(msg: &CascadeMessage) -> Document<'static> {
    match &msg.selector {
        MessageSelector::Unary(name) => leaf::ident(name),
        MessageSelector::Binary(op) => {
            let arg = unparse_expression(&msg.arguments[0]);
            docvec![leaf::ident(op), " ", arg]
        }
        MessageSelector::Keyword(parts) => {
            let mut kw_docs: Vec<Document<'static>> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    kw_docs.push(Document::Str(" "));
                }
                kw_docs.push(unparse_keyword_part(part));
                if i < msg.arguments.len() {
                    kw_docs.push(Document::Str(" "));
                    kw_docs.push(unparse_expression(&msg.arguments[i]));
                }
            }
            concat(kw_docs)
        }
    }
}

fn unparse_cascade(receiver: &Expression, messages: &[CascadeMessage]) -> Document<'static> {
    let receiver_doc = unparse_expression(receiver);
    let msg_docs: Vec<Document<'static>> = messages.iter().map(unparse_cascade_message).collect();

    if messages.len() == 1 {
        // Single cascade message: always inline
        docvec![receiver_doc, "; ", msg_docs.into_iter().next().unwrap()]
    } else {
        // Multiple cascade messages: try inline, break to one-per-line
        // Build inline version to measure
        let mut inline_parts: Vec<Document<'static>> = vec![receiver_doc.clone()];
        for msg_doc in &msg_docs {
            inline_parts.push(Document::Str("; "));
            inline_parts.push(msg_doc.clone());
        }
        let inline = concat(inline_parts);
        let inline_width = inline.to_pretty_string().len();

        if inline_width <= 80 {
            inline
        } else {
            // One message per line: receiver on first line, then ;-separated continuations
            let mut continuation: Vec<Document<'static>> = Vec::new();
            for (i, msg_doc) in msg_docs.into_iter().enumerate() {
                if i > 0 {
                    continuation.push(Document::Str(";"));
                }
                continuation.push(line());
                continuation.push(msg_doc);
            }
            docvec![receiver_doc, ";", nest(2, concat(continuation))]
        }
    }
}

// --- Match unparsing ---

fn unparse_match(value: &Expression, arms: &[MatchArm], exhaustive: bool) -> Document<'static> {
    // BT-2763 / ADR 0106: `matchExhaustive:` round-trips through unparse just
    // like `match:` — only the keyword selector differs.
    let keyword = if exhaustive {
        " matchExhaustive: ["
    } else {
        " match: ["
    };
    let arm_docs: Vec<Document<'static>> = arms.iter().map(unparse_match_arm).collect();
    if arm_docs.len() <= 1 {
        // Single arm: try inline, break if too wide
        let joined = join_docs(arm_docs, "; ");
        group(docvec![unparse_expression(value), keyword, joined, "]"])
    } else {
        // Multiple arms: one per line
        let mut body: Vec<Document<'static>> = Vec::new();
        for (i, (arm, arm_doc)) in arms.iter().zip(arm_docs).enumerate() {
            // Preserve blank line before arms that had one in source
            // (skip for first arm — the opening bracket provides separation)
            let has_blank = i > 0
                && arm
                    .comments
                    .leading
                    .first()
                    .is_some_and(|c| c.preceding_blank_line);
            if has_blank {
                body.push(line());
            }
            body.push(line());
            body.push(arm_doc);
            if i < arms.len() - 1 {
                body.push(Document::Str(";"));
            }
        }
        docvec![
            unparse_expression(value),
            keyword,
            nest(2, concat(body)),
            line(),
            "]"
        ]
    }
}

fn unparse_match_arm(arm: &MatchArm) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Leading comments (e.g. `// (quote expr) — return unevaluated`)
    let leading = unparse_comment_attachment_leading(&arm.comments);
    if !leading.is_empty() {
        docs.extend(leading);
    }

    let pat = unparse_pattern(&arm.pattern);
    let guard = if let Some(g) = &arm.guard {
        docvec![" when: [", unparse_expression(g), "]"]
    } else {
        nil()
    };
    let body = unparse_expression(&arm.body);
    docs.push(docvec![pat, guard, " -> ", body]);
    concat(docs)
}

fn unparse_pattern(pattern: &Pattern) -> Document<'static> {
    match pattern {
        Pattern::Wildcard(_) => Document::Str("_"),
        Pattern::Literal(lit, _) => unparse_literal(lit),
        Pattern::Nil(_) => Document::Str("nil"),
        Pattern::Variable(id) => unparse_identifier(id),
        Pattern::Type { binding, class, .. } => {
            docvec![
                unparse_identifier(binding),
                " :: ",
                leaf::ident(&class.name)
            ]
        }
        Pattern::Tuple { elements, .. } => {
            let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_pattern).collect();
            let joined = join_docs(elem_docs, ", ");
            docvec!["{", joined, "}"]
        }
        Pattern::List { elements, tail, .. } => {
            let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_pattern).collect();
            let joined = join_docs(elem_docs, ", ");
            if let Some(t) = tail {
                docvec!["[", joined, " | ", unparse_pattern(t), "]"]
            } else {
                docvec!["[", joined, "]"]
            }
        }
        Pattern::Binary { segments, .. } => {
            let seg_docs: Vec<Document<'static>> =
                segments.iter().map(unparse_binary_segment).collect();
            let joined = join_docs(seg_docs, ", ");
            docvec!["<<", joined, ">>"]
        }
        Pattern::Array {
            elements,
            rest,
            list_syntax,
            ..
        } => {
            let mut elem_docs: Vec<Document<'static>> =
                elements.iter().map(unparse_pattern).collect();
            if let Some(rest_pat) = rest {
                elem_docs.push(docvec!["...", unparse_pattern(rest_pat)]);
            }
            let joined = join_docs(elem_docs, ", ");
            if *list_syntax {
                docvec!["#(", joined, ")"]
            } else {
                docvec!["#[", joined, "]"]
            }
        }
        Pattern::Map { pairs, .. } => {
            let pair_docs: Vec<Document<'static>> = pairs
                .iter()
                .map(|p| {
                    let key_doc = match &p.key {
                        MapPatternKey::Symbol(s) => unparse_literal(&Literal::Symbol(s.clone())),
                        MapPatternKey::StringLit(s) => unparse_literal(&Literal::String(s.clone())),
                    };
                    docvec![key_doc, " => ", unparse_pattern(&p.value)]
                })
                .collect();
            let joined = join_docs(pair_docs, ", ");
            docvec!["#{", joined, "}"]
        }
        Pattern::Constructor {
            class, keywords, ..
        } => {
            let mut parts: Vec<Document<'static>> = vec![leaf::ident(&class.name)];
            for (kw, binding) in keywords {
                parts.push(Document::Str(" "));
                parts.push(leaf::ident(&kw.name));
                parts.push(Document::Str(" "));
                parts.push(unparse_pattern(binding));
            }
            Document::Vec(parts)
        }
    }
}

fn unparse_binary_segment(seg: &BinarySegment) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = vec![unparse_pattern(&seg.value)];
    if let Some(size) = &seg.size {
        docs.push(Document::Str(":"));
        docs.push(unparse_expression(size));
    }
    let mut specs: Vec<&str> = Vec::new();
    if let Some(t) = &seg.segment_type {
        specs.push(match t {
            BinarySegmentType::Integer => "integer",
            BinarySegmentType::Float => "float",
            BinarySegmentType::Binary => "binary",
            BinarySegmentType::Utf8 => "utf8",
        });
    }
    if let Some(s) = &seg.signedness {
        specs.push(match s {
            BinarySignedness::Signed => "signed",
            BinarySignedness::Unsigned => "unsigned",
        });
    }
    if let Some(e) = &seg.endianness {
        specs.push(match e {
            BinaryEndianness::Big => "big",
            BinaryEndianness::Little => "little",
            BinaryEndianness::Native => "native",
        });
    }
    if !specs.is_empty() {
        docs.push(Document::Str("/"));
        let spec_docs: Vec<Document<'static>> = specs.iter().map(|s| Document::Str(s)).collect();
        docs.push(join_docs(spec_docs, "-"));
    }
    if let Some(unit) = seg.unit {
        docs.push(Document::Str(":"));
        docs.push(leaf::nat_lit(unit));
    }
    concat(docs)
}

// --- Map literal unparsing ---

fn unparse_map_literal(pairs: &[MapPair]) -> Document<'static> {
    if pairs.is_empty() {
        return Document::Str("#{}");
    }
    let pair_docs: Vec<Document<'static>> = pairs.iter().map(unparse_map_pair).collect();
    // Try inline first (#{a => 1, b => 2}), break to one-per-line if too wide
    let mut body = Vec::new();
    for (i, pair_doc) in pair_docs.into_iter().enumerate() {
        if i > 0 {
            body.push(Document::Str(","));
            body.push(break_("", " "));
        } else {
            body.push(break_("", ""));
        }
        body.push(pair_doc);
    }
    group(docvec!["#{", nest(2, concat(body)), break_("", ""), "}",])
}

fn unparse_map_pair(pair: &MapPair) -> Document<'static> {
    docvec![
        unparse_expression(&pair.key),
        " => ",
        unparse_expression(&pair.value),
    ]
}

// --- List literal unparsing ---

fn unparse_list_literal(elements: &[Expression], tail: Option<&Expression>) -> Document<'static> {
    if elements.is_empty() && tail.is_none() {
        return Document::Str("#()");
    }
    let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_expression).collect();
    let joined = join_docs(elem_docs, ", ");
    if let Some(t) = tail {
        docvec!["#(", joined, " | ", unparse_expression(t), ")"]
    } else {
        docvec!["#(", joined, ")"]
    }
}

// --- Array literal unparsing ---

fn unparse_array_literal(elements: &[Expression]) -> Document<'static> {
    if elements.is_empty() {
        return Document::Str("#[]");
    }
    let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_expression).collect();
    let joined = join_docs(elem_docs, ", ");
    docvec!["#[", joined, "]"]
}

// --- String interpolation unparsing ---

fn unparse_string_interpolation(segments: &[StringSegment]) -> Document<'static> {
    let mut inner: Vec<Document<'static>> = Vec::new();
    for seg in segments {
        match seg {
            StringSegment::Literal(s) => {
                // Literal segments may contain bare " from doubled-delimiter unescaping.
                inner.push(leaf::string_content(s));
            }
            StringSegment::Interpolation(expr) => {
                inner.push(Document::Str("{"));
                inner.push(unparse_expression(expr));
                inner.push(Document::Str("}"));
            }
        }
    }
    docvec!["\"", concat(inner), "\""]
}

// --- Type annotation unparsing ---

fn unparse_type_annotation(ty: &TypeAnnotation) -> Document<'static> {
    match ty {
        TypeAnnotation::Simple(id) => leaf::ident(&id.name),
        TypeAnnotation::Singleton { name, .. } => {
            docvec!["#", leaf::ident(name)]
        }
        TypeAnnotation::Union { types, .. } => {
            let type_docs: Vec<Document<'static>> =
                types.iter().map(unparse_type_annotation).collect();
            join_docs(type_docs, " | ")
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            let param_docs: Vec<Document<'static>> =
                parameters.iter().map(unparse_type_annotation).collect();
            let joined = join_docs(param_docs, ", ");
            docvec![leaf::ident(&base.name), "(", joined, ")"]
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            docvec![unparse_type_annotation(inner), " | False"]
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            // Re-derive grouping parens (BT-2760) where re-parsing would
            // otherwise change the AST. The predicate is shared with
            // `TypeAnnotation::type_name` — single source of truth.
            docvec![
                unparse_grouped_type(base, base.needs_parens_in_difference(false)),
                " \\ ",
                unparse_grouped_type(excluded, excluded.needs_parens_in_difference(true))
            ]
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            // Mirror image of `Difference` above; predicate shared with
            // `TypeAnnotation::type_name`.
            docvec![
                unparse_grouped_type(left, left.needs_parens_in_intersection(false)),
                " & ",
                unparse_grouped_type(right, right.needs_parens_in_intersection(true))
            ]
        }
        TypeAnnotation::SelfType { .. } => Document::Str("Self"),
        TypeAnnotation::SelfClass { .. } => Document::Str("Self class"),
        TypeAnnotation::ClassOf { class_name, .. } => {
            docvec![leaf::ident(&class_name.name), " class"]
        }
    }
}

/// Unparses a `\`/`&` operand, wrapping it in grouping parentheses
/// (BT-2760) when `parens` is set — i.e. when re-parsing the bare operand
/// would bind differently (see the `Difference`/`Intersection` arms of
/// [`unparse_type_annotation`]).
fn unparse_grouped_type(ty: &TypeAnnotation, parens: bool) -> Document<'static> {
    if parens {
        docvec!["(", unparse_type_annotation(ty), ")"]
    } else {
        unparse_type_annotation(ty)
    }
}

fn unparse_type_annotation_opt(ty: Option<&TypeAnnotation>) -> Document<'static> {
    if let Some(t) = ty {
        docvec![" :: ", unparse_type_annotation(t)]
    } else {
        nil()
    }
}

// --- Expect category unparsing ---

fn unparse_expect_category(cat: ExpectCategory) -> Document<'static> {
    Document::Str(cat.as_str())
}

/// Unparses a (possibly multi-category, BT-3387) `@expect` category list as
/// `cat1, cat2, ...`.
fn unparse_expect_categories(cats: &[ExpectCategory]) -> Document<'static> {
    let docs: Vec<Document<'static>> = cats.iter().copied().map(unparse_expect_category).collect();
    join_docs(docs, ", ")
}

// --- Comment unparsing ---

/// Builds a [`Document`] for a single [`Comment`].
///
/// Line comments become `// content`, block comments become `/* content */`,
/// doc-style leading comments become `/// content` (BT-2924).
fn unparse_comment(comment: &Comment) -> Document<'static> {
    match comment.kind {
        CommentKind::Line => {
            if comment.content.is_empty() {
                Document::Str("//")
            } else {
                docvec!["// ", leaf::raw_text(&comment.content)]
            }
        }
        CommentKind::Block => {
            docvec!["/* ", leaf::raw_text(&comment.content), " */"]
        }
        CommentKind::Doc => {
            if comment.content.is_empty() {
                Document::Str("///")
            } else {
                docvec!["/// ", leaf::raw_text(&comment.content)]
            }
        }
    }
}

/// Produces the leading-comments documents with trailing newlines.
///
/// Each leading comment is emitted followed by a `line()` so that the next
/// element starts on a fresh line.
fn unparse_comment_attachment_leading(ca: &CommentAttachment) -> Vec<Document<'static>> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, comment) in ca.leading.iter().enumerate() {
        // Emit blank line between comment groups (but not before the first —
        // the calling context already manages spacing before the attachment).
        if i > 0 && comment.preceding_blank_line {
            docs.push(line());
        }
        docs.push(unparse_comment(comment));
        docs.push(line());
    }
    docs
}

/// Whether a blank line must separate `comments.leading` from whatever
/// follows (a doc comment, or the declaration header when there is no doc
/// comment).
///
/// Only true when the *last* leading comment is [`CommentKind::Doc`] — a
/// `///` block that `collect_comment_attachment` preserved because a blank
/// line (or `//` comment) broke it away from the declaration it visually
/// precedes (BT-2924). Such a block is, by construction, never adjacent to
/// what follows in the original source — a blank line always separated it —
/// so reinserting one here reconstructs that gap and stops the preserved
/// block from visually gluing onto (and, on the next parse, merging into)
/// this declaration's own doc comment.
///
/// Ordinary leading comments (license headers, section notes) are
/// deliberately excluded: several declaration kinds (`Protocol define:`,
/// `state:`/`classState:`) have established, already-canonical stdlib source
/// where such comments sit directly against the following doc comment or
/// header with no blank line, and this must not force one in.
///
/// Checking only the *last* entry (not `any()` over the whole slice) is
/// correct because a [`CommentKind::Doc`] entry that *needs* a blank-line
/// separator (i.e., nothing in the original source separated it from what
/// follows) will always be the last item in `comments.leading`. If a
/// [`CommentKind::Line`] (`//` comment) follows an orphaned `///` block in
/// the trivia, it is appended to `leading` *after* the `Doc` entry and
/// already provides the separation — so this function correctly returns
/// `false` and no extra blank line is inserted.
fn leading_ends_with_orphaned_doc_comment(comments: &CommentAttachment) -> bool {
    comments
        .leading
        .last()
        .is_some_and(|c| c.kind == CommentKind::Doc)
}

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
