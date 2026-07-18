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

use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, ClassDefinition, Comment, CommentAttachment, CommentKind, ExpectCategory,
    Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MapPair, MapPatternKey,
    MatchArm, MessageSelector, MethodDefinition, Module, Pattern, ProtocolDefinition,
    ProtocolMethodSignature, StandaloneMethodDefinition, StateDeclaration, StringSegment,
    TypeAliasDefinition, TypeAnnotation,
};
use crate::codegen::core_erlang::document::{
    DEFAULT_LINE_WIDTH, Document, break_, concat, group, line, nest, nil,
};
use crate::docvec;
use crate::source_analysis::{Severity, lex_with_eof, parse, parse_method};

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

/// Builds a [`Document`] for a method display signature (no `sealed`, no ` =>`).
fn unparse_method_display_signature_doc(method: &MethodDefinition) -> Document<'static> {
    let sig = match &method.selector {
        MessageSelector::Unary(name) => leaf::ident(name),
        MessageSelector::Binary(op) => {
            let param = &method.parameters[0];
            docvec![
                leaf::ident(op),
                " ",
                leaf::ident(&param.name.name),
                unparse_type_annotation_opt(param.type_annotation.as_ref()),
            ]
        }
        MessageSelector::Keyword(parts) => {
            let mut sig_docs: Vec<Document<'static>> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig_docs.push(Document::Str(" "));
                }
                sig_docs.push(unparse_keyword_part(part));
                if i < method.parameters.len() {
                    sig_docs.push(Document::Str(" "));
                    let param = &method.parameters[i];
                    sig_docs.push(leaf::ident(&param.name.name));
                    sig_docs.push(unparse_type_annotation_opt(param.type_annotation.as_ref()));
                }
            }
            concat(sig_docs)
        }
    };

    let return_type = if let Some(ret) = &method.return_type {
        docvec![" -> ", unparse_type_annotation(ret)]
    } else {
        nil()
    };

    docvec![sig, return_type]
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
        // declaration — there's nothing before it in this section to
        // separate from (any file-level leading comments/blank line were
        // already handled above).
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
    for smd in &module.method_definitions {
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

    // Blank line between leading comments (e.g. license block) and doc/header
    if !class.comments.leading.is_empty() {
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

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration (BT-2924) and this alias's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&type_alias.comments) {
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

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration (BT-2924) and this protocol's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&protocol.comments) {
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

    // Instance method signatures (indented by 2 spaces)
    for sig in &protocol.method_signatures {
        docs.push(nest(
            2,
            docvec![line(), unparse_protocol_method_signature(sig, None)],
        ));
    }

    // Class method signatures (BT-1611, indented by 2 spaces, prefixed with `class`)
    for sig in &protocol.class_method_signatures {
        docs.push(nest(
            2,
            docvec![
                line(),
                unparse_protocol_method_signature(sig, Some("class "))
            ],
        ));
    }

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

    // Selector and parameters
    match &sig.selector {
        MessageSelector::Unary(name) => {
            docs.push(leaf::ident(name));
        }
        MessageSelector::Binary(op) => {
            docs.push(leaf::ident(op));
            if let Some(param) = sig.parameters.first() {
                docs.push(Document::Str(" "));
                docs.push(leaf::ident(&param.name.name));
                docs.push(unparse_type_annotation_opt(param.type_annotation.as_ref()));
            }
        }
        MessageSelector::Keyword(parts) => {
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    docs.push(Document::Str(" "));
                }
                docs.push(leaf::ident(&part.keyword));
                if let Some(param) = sig.parameters.get(i) {
                    docs.push(Document::Str(" "));
                    docs.push(leaf::ident(&param.name.name));
                    docs.push(unparse_type_annotation_opt(param.type_annotation.as_ref()));
                }
            }
        }
    }

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
    if let Some((cat, ref reason, _)) = method.expect {
        let base = docvec!["@expect ", unparse_expect_category(cat)];
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
fn unparse_method_signature(method: &MethodDefinition) -> Document<'static> {
    let sig = match &method.selector {
        MessageSelector::Unary(name) => leaf::ident(name),
        MessageSelector::Binary(op) => {
            let param = &method.parameters[0];
            docvec![
                leaf::ident(op),
                " ",
                leaf::ident(&param.name.name),
                unparse_type_annotation_opt(param.type_annotation.as_ref()),
            ]
        }
        MessageSelector::Keyword(parts) => {
            let mut sig_docs: Vec<Document<'static>> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig_docs.push(Document::Str(" "));
                }
                sig_docs.push(unparse_keyword_part(part));
                if i < method.parameters.len() {
                    sig_docs.push(Document::Str(" "));
                    let param = &method.parameters[i];
                    sig_docs.push(leaf::ident(&param.name.name));
                    sig_docs.push(unparse_type_annotation_opt(param.type_annotation.as_ref()));
                }
            }
            concat(sig_docs)
        }
    };

    let return_type = if let Some(ret) = &method.return_type {
        docvec![" -> ", unparse_type_annotation(ret)]
    } else {
        nil()
    };

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

    docvec![internal, sealed, sig, return_type, " =>"]
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
    if let Some((cat, ref reason, _)) = state.expect {
        if let Some(reason) = reason {
            docs.push(docvec![
                "@expect ",
                unparse_expect_category(cat),
                " \"",
                leaf::string_content(reason),
                "\""
            ]);
        } else {
            docs.push(docvec!["@expect ", unparse_expect_category(cat)]);
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
            category, reason, ..
        } => {
            let base = docvec!["@expect ", unparse_expect_category(*category)];
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

// Property-based tests for the unparser (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Expression, ExpressionStatement, Identifier, Literal, MessageSelector, MethodDefinition,
        ParameterDefinition, StateDeclaration,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    // --- format_source ---

    #[test]
    fn format_source_parse_error_returns_none() {
        // Source with a parse error must return None.
        let result = format_source("@@@invalid beamtalk source@@@");
        assert!(result.is_none(), "parse error should return None");
    }

    #[test]
    fn format_source_valid_source_is_formatted() {
        // Valid source must be returned formatted (with trailing newline).
        let source = "Object subclass: Foo\n  bar => 42\n";
        let result = format_source(source);
        assert!(result.is_some(), "valid source should return Some");
        let formatted = result.unwrap();
        assert!(formatted.ends_with('\n'), "output must end with newline");
        assert!(
            formatted.contains("bar => 42"),
            "formatted output must contain the method"
        );
    }

    #[test]
    fn format_source_is_idempotent() {
        // format_source(format_source(s).unwrap()).unwrap() == format_source(s).unwrap()
        let source = "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value\n";
        let pass1 = format_source(source).expect("pass1 should succeed");
        let pass2 = format_source(&pass1).expect("pass2 should succeed");
        assert_eq!(pass1, pass2, "format_source must be idempotent");
    }

    // --- Difference type annotation unparsing (BT-2742) ---

    #[test]
    fn difference_type_annotation_unparses() {
        // `Symbol \ #foo` unparses back to `Symbol \ #foo`.
        let ann = crate::ast::TypeAnnotation::difference(
            crate::ast::TypeAnnotation::simple("Symbol", span()),
            crate::ast::TypeAnnotation::singleton("foo", span()),
            span(),
        );
        assert_eq!(
            unparse_type_annotation(&ann).to_pretty_string(),
            "Symbol \\ #foo"
        );
    }

    #[test]
    fn difference_return_type_round_trips_through_format_source() {
        // Parse → unparse must preserve `Symbol \ #foo` in a method return type.
        let source = "Object subclass: Foo\n  narrow -> Symbol \\ #foo => #bar\n";
        let formatted = format_source(source).expect("should format");
        assert!(
            formatted.contains("Symbol \\ #foo"),
            "difference type must survive round-trip, got: {formatted}"
        );
    }

    // --- Intersection type annotation unparsing (ADR 0068 §Protocol
    // Composition, ADR 0102 §1/§3, BT-2743) ---

    #[test]
    fn intersection_type_annotation_unparses() {
        // `Printable & Comparable` unparses back to `Printable & Comparable`.
        let ann = crate::ast::TypeAnnotation::intersection(
            crate::ast::TypeAnnotation::simple("Printable", span()),
            crate::ast::TypeAnnotation::simple("Comparable", span()),
            span(),
        );
        assert_eq!(
            unparse_type_annotation(&ann).to_pretty_string(),
            "Printable & Comparable"
        );
    }

    #[test]
    fn intersection_return_type_round_trips_through_format_source() {
        // Parse → unparse must preserve `Collection(Object) & Comparable` in a
        // method return type.
        let source = "Object subclass: Foo\n  narrow -> Collection(Object) & Comparable => self\n";
        let formatted = format_source(source).expect("should format");
        assert!(
            formatted.contains("Collection(Object) & Comparable"),
            "intersection type must survive round-trip, got: {formatted}"
        );
    }

    // --- Grouping parentheses in type annotations (BT-2760) ---

    #[test]
    fn grouped_difference_operands_unparse_with_parens() {
        // `Difference { base: Intersection }` must re-derive the grouping
        // parens — the bare form `A & B \ #c` is the mixed-operator parse
        // error (ADR 0102 §3).
        let ann = crate::ast::TypeAnnotation::difference(
            crate::ast::TypeAnnotation::intersection(
                crate::ast::TypeAnnotation::simple("A", span()),
                crate::ast::TypeAnnotation::simple("B", span()),
                span(),
            ),
            crate::ast::TypeAnnotation::singleton("c", span()),
            span(),
        );
        assert_eq!(
            unparse_type_annotation(&ann).to_pretty_string(),
            "(A & B) \\ #c"
        );
    }

    #[test]
    fn grouped_union_excluded_unparses_with_parens() {
        // `Symbol \ (#a | #b)` — a union excluded operand keeps its parens
        // (`\` binds tighter than `|`).
        let ann = crate::ast::TypeAnnotation::difference(
            crate::ast::TypeAnnotation::simple("Symbol", span()),
            crate::ast::TypeAnnotation::union(
                vec![
                    crate::ast::TypeAnnotation::singleton("a", span()),
                    crate::ast::TypeAnnotation::singleton("b", span()),
                ],
                span(),
            ),
            span(),
        );
        assert_eq!(
            unparse_type_annotation(&ann).to_pretty_string(),
            "Symbol \\ (#a | #b)"
        );
    }

    #[test]
    fn right_nested_difference_unparses_with_parens() {
        // `\` is left-associative, so a right-nested difference must keep
        // its parens: `Symbol \ (#a \ #b)`, not `Symbol \ #a \ #b`.
        let ann = crate::ast::TypeAnnotation::difference(
            crate::ast::TypeAnnotation::simple("Symbol", span()),
            crate::ast::TypeAnnotation::difference(
                crate::ast::TypeAnnotation::singleton("a", span()),
                crate::ast::TypeAnnotation::singleton("b", span()),
                span(),
            ),
            span(),
        );
        assert_eq!(
            unparse_type_annotation(&ann).to_pretty_string(),
            "Symbol \\ (#a \\ #b)"
        );
    }

    #[test]
    fn grouped_mixed_types_round_trip_through_format_source() {
        // Parse → unparse preserves the parenthesised mixed forms (BT-2760),
        // and the formatter is idempotent on them.
        for (source, expected) in [
            (
                "Object subclass: Foo\n  narrow -> (A & B) \\ #c => self\n",
                "(A & B) \\ #c",
            ),
            (
                "Object subclass: Foo\n  narrow -> A & (B \\ #c) => self\n",
                "A & (B \\ #c)",
            ),
            (
                "Object subclass: Foo\n  narrow -> Symbol \\ (#a | #b) => #c\n",
                "Symbol \\ (#a | #b)",
            ),
            (
                "Object subclass: Foo\n  narrow -> Integer | (Symbol \\ #foo) => 1\n",
                // The group is redundant (`\` already binds tighter than
                // `|`), so the formatter drops it.
                "Integer | Symbol \\ #foo",
            ),
        ] {
            let formatted = format_source(source).expect("should format");
            assert!(
                formatted.contains(expected),
                "grouped type must survive round-trip: expected `{expected}` in: {formatted}"
            );
            let reformatted = format_source(&formatted).expect("should reformat");
            assert_eq!(formatted, reformatted, "format must be idempotent");
        }
    }

    // --- Comment unparsing ---

    #[test]
    fn line_comment_document() {
        let c = Comment::line("hello world", span());
        assert_eq!(unparse_comment(&c).to_pretty_string(), "// hello world");
    }

    #[test]
    fn block_comment_document() {
        let c = Comment::block("block text", span());
        assert_eq!(unparse_comment(&c).to_pretty_string(), "/* block text */");
    }

    // --- Expression statement with comments ---

    #[test]
    fn expression_statement_no_comments() {
        let stmt = ExpressionStatement::bare(Expression::Literal(Literal::Integer(42), span()));
        assert_eq!(unparse_expression_statement(&stmt).to_pretty_string(), "42");
    }

    #[test]
    fn expression_statement_leading_comment() {
        let stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![Comment::line("This is 42", span())],
                trailing: None,
                leading_blank_line: false,
            },
            expression: Expression::Literal(Literal::Integer(42), span()),
            preceding_blank_line: false,
        };
        let output = unparse_expression_statement(&stmt).to_pretty_string();
        assert_eq!(output, "// This is 42\n42");
    }

    #[test]
    fn expression_statement_trailing_comment() {
        let stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: Vec::new(),
                trailing: Some(Comment::line("trailing", span())),
                leading_blank_line: false,
            },
            expression: Expression::Literal(Literal::Integer(1), span()),
            preceding_blank_line: false,
        };
        let output = unparse_expression_statement(&stmt).to_pretty_string();
        assert_eq!(output, "1  // trailing");
    }

    #[test]
    fn expression_statement_both_comments() {
        let stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![Comment::line("before", span())],
                trailing: Some(Comment::line("after", span())),
                leading_blank_line: false,
            },
            expression: Expression::Literal(Literal::Integer(99), span()),
            preceding_blank_line: false,
        };
        let output = unparse_expression_statement(&stmt).to_pretty_string();
        assert_eq!(output, "// before\n99  // after");
    }

    // --- Method definition ---

    #[test]
    fn method_unary_single_expr_inline() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            Vec::new(),
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                span(),
            ))],
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        // Single-expression body goes on same line
        assert_eq!(output, "increment => 1");
    }

    #[test]
    fn method_with_leading_comment_single_expr() {
        let mut method = MethodDefinition::new(
            MessageSelector::Unary("getValue".into()),
            Vec::new(),
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(0),
                span(),
            ))],
            span(),
        );
        method.comments = CommentAttachment {
            leading: vec![Comment::line("Returns the current value", span())],
            trailing: None,
            leading_blank_line: false,
        };
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(output, "// Returns the current value\ngetValue => 0");
    }

    #[test]
    fn method_with_doc_comment() {
        let mut method = MethodDefinition::new(
            MessageSelector::Unary("size".into()),
            Vec::new(),
            vec![ExpressionStatement::bare(Expression::Identifier(
                Identifier::new("n", span()),
            ))],
            span(),
        );
        method.doc_comment = Some("Returns the size.".into());
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(output, "/// Returns the size.\nsize => n");
    }

    #[test]
    fn state_declaration_with_doc_comment() {
        let mut state = StateDeclaration::new(Identifier::new("count", span()), span());
        state.doc_comment = Some("The current count.".into());
        let output = unparse_state_declaration(&state).to_pretty_string();
        assert_eq!(output, "/// The current count.\nstate: count");
    }

    #[test]
    fn field_declaration_unparse() {
        use crate::ast::DeclaredKeyword;
        let mut decl = StateDeclaration::new(Identifier::new("x", span()), span());
        decl.declared_keyword = DeclaredKeyword::Field;
        let output = unparse_state_declaration(&decl).to_pretty_string();
        assert_eq!(output, "field: x");
    }

    #[test]
    fn field_declaration_round_trip() {
        let source = "Object subclass: Foo\n  field: count = 0\n";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("field: count = 0"),
            "expected field: in output, got: {output}"
        );
    }

    #[test]
    fn state_declaration_with_multiline_doc_comment() {
        let mut state = StateDeclaration::new(Identifier::new("timeout", span()), span());
        state.doc_comment = Some("Timeout in milliseconds.\n\nDefaults to 30000.".into());
        let output = unparse_state_declaration(&state).to_pretty_string();
        assert_eq!(
            output,
            "/// Timeout in milliseconds.\n///\n/// Defaults to 30000.\nstate: timeout"
        );
    }

    #[test]
    fn method_multi_expr_on_new_lines() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("doTwoThings".into()),
            Vec::new(),
            vec![
                ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
                ExpressionStatement::bare(Expression::Literal(Literal::Integer(2), span())),
            ],
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(output, "doTwoThings =>\n  1\n  2");
    }

    // --- Literal round-trip ---

    #[test]
    fn integer_literal() {
        let expr = Expression::Literal(Literal::Integer(42), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "42");
    }

    #[test]
    fn string_literal_with_escape() {
        let expr = Expression::Literal(Literal::String("it's".into()), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "\"it's\"");
    }

    #[test]
    fn string_literal_with_embedded_double_quotes() {
        // Bare " in the AST (from doubled-delimiter unescaping) gets re-escaped as ""
        let expr = Expression::Literal(Literal::String("say \"hello\"".into()), span());
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "\"say \"\"hello\"\"\""
        );
    }

    #[test]
    fn symbol_literal_simple() {
        let expr = Expression::Literal(Literal::Symbol("ok".into()), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "#ok");
    }

    #[test]
    fn symbol_literal_with_space_needs_quoting() {
        let expr = Expression::Literal(Literal::Symbol("with space".into()), span());
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "#'with space'"
        );
    }

    #[test]
    fn float_literal() {
        let expr = Expression::Literal(Literal::Float(1.5), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "1.5");
    }

    #[test]
    fn character_literal_plain() {
        let expr = Expression::Literal(Literal::Character('A'), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "$A");
    }

    #[test]
    fn character_literal_newline() {
        let expr = Expression::Literal(Literal::Character('\n'), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\n");
    }

    #[test]
    fn character_literal_tab() {
        let expr = Expression::Literal(Literal::Character('\t'), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\t");
    }

    #[test]
    fn character_literal_backslash() {
        let expr = Expression::Literal(Literal::Character('\\'), span());
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\\\");
    }

    // --- Assignment ---

    #[test]
    fn assignment_expression() {
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", span()))),
            value: Box::new(Expression::Literal(Literal::Integer(5), span())),
            type_annotation: None,
            span: span(),
        };
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "x := 5");
    }

    #[test]
    fn annotated_assignment_expression() {
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", span()))),
            value: Box::new(Expression::Literal(Literal::Integer(5), span())),
            type_annotation: Some(TypeAnnotation::simple("Integer", span())),
            span: span(),
        };
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "x :: Integer := 5"
        );
    }

    #[test]
    fn annotated_assignment_union_type() {
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("name", span()))),
            value: Box::new(Expression::Identifier(Identifier::new("val", span()))),
            type_annotation: Some(TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("String", span()),
                    TypeAnnotation::simple("nil", span()),
                ],
                span(),
            )),
            span: span(),
        };
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "name :: String | nil := val"
        );
    }

    #[test]
    fn annotated_assignment_generic_type() {
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("r", span()))),
            value: Box::new(Expression::Identifier(Identifier::new("compute", span()))),
            type_annotation: Some(TypeAnnotation::generic(
                Identifier::new("Result", span()),
                vec![
                    TypeAnnotation::simple("Integer", span()),
                    TypeAnnotation::simple("Error", span()),
                ],
                span(),
            )),
            span: span(),
        };
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "r :: Result(Integer, Error) := compute"
        );
    }

    // --- Return ---

    #[test]
    fn return_expression() {
        let expr = Expression::Return {
            value: Box::new(Expression::Identifier(Identifier::new("x", span()))),
            span: span(),
        };
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "^x");
    }

    // --- Message send ---

    #[test]
    fn unary_message_send() {
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("coll", span()))),
            selector: MessageSelector::Unary("size".into()),
            arguments: Vec::new(),
            is_cast: false,
            span: span(),
        };
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "coll size");
    }

    #[test]
    fn binary_message_send() {
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Literal(Literal::Integer(3), span())),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(4), span())],
            is_cast: false,
            span: span(),
        };
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "3 + 4");
    }

    #[test]
    fn keyword_message_send() {
        use crate::ast::KeywordPart;
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("arr", span()))),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("at:", span()),
                KeywordPart::new("put:", span()),
            ]),
            arguments: vec![
                Expression::Literal(Literal::Integer(1), span()),
                Expression::Literal(Literal::String("x".into()), span()),
            ],
            is_cast: false,
            span: span(),
        };
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "arr at: 1 put: \"x\""
        );
    }

    // --- Synthesized method (no source text) ---

    #[test]
    fn synthesized_method_produces_valid_source() {
        // Synthesized method — built from data with no source text, Span::default()
        let method = MethodDefinition::new(
            MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "withValue:",
                Span::default(),
            )]),
            vec![ParameterDefinition::new(Identifier::new(
                "v",
                Span::default(),
            ))],
            vec![ExpressionStatement::bare(Expression::Identifier(
                Identifier::new("v", Span::default()),
            ))],
            Span::default(),
        );
        let source = unparse_method(&method);
        // Single-expr body is inline; must be valid-looking source
        assert_eq!(source, "withValue: v => v");
    }

    #[test]
    fn method_with_leading_comment_in_body() {
        // Method where a body expression has a leading comment — forces multi-line output
        let body_stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![Comment::line("the result", span())],
                trailing: None,
                leading_blank_line: false,
            },
            expression: Expression::Identifier(Identifier::new("x", span())),
            preceding_blank_line: false,
        };
        let method = MethodDefinition::new(
            MessageSelector::Unary("compute".into()),
            Vec::new(),
            vec![body_stmt],
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        // Leading comment in body forces multi-line, comment indented with body
        assert_eq!(output, "compute =>\n  // the result\n  x");
    }

    // --- Blank line preservation (BT-987) ---

    #[test]
    fn method_body_blank_line_preserved() {
        let body = vec![
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
            ExpressionStatement {
                comments: CommentAttachment::default(),
                expression: Expression::Literal(Literal::Integer(2), span()),
                preceding_blank_line: true,
            },
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(3), span())),
        ];
        let method = MethodDefinition::new(
            MessageSelector::Unary("doStuff".into()),
            Vec::new(),
            body,
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        // Blank line before `2` but not before `3`
        assert_eq!(output, "doStuff =>\n  1\n\n  2\n  3");
    }

    #[test]
    fn method_body_no_blank_lines() {
        let body = vec![
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(2), span())),
        ];
        let method = MethodDefinition::new(
            MessageSelector::Unary("doStuff".into()),
            Vec::new(),
            body,
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(output, "doStuff =>\n  1\n  2");
    }

    // --- Method body comment indentation ---

    #[test]
    fn method_body_leading_comment_indented_with_body() {
        // A single expression with a leading comment forces multi-line.
        // The comment must be indented at the same level as the expression.
        let body_stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![Comment::line("do the thing", span())],
                trailing: None,
                leading_blank_line: false,
            },
            expression: Expression::Identifier(Identifier::new("x", span())),
            preceding_blank_line: false,
        };
        let method = MethodDefinition::new(
            MessageSelector::Unary("compute".into()),
            Vec::new(),
            vec![body_stmt],
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(output, "compute =>\n  // do the thing\n  x");
    }

    #[test]
    fn method_body_multiple_leading_comments_indented() {
        let body_stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![
                    Comment::line("first comment", span()),
                    Comment::line("second comment", span()),
                ],
                trailing: None,
                leading_blank_line: false,
            },
            expression: Expression::Literal(Literal::Integer(42), span()),
            preceding_blank_line: false,
        };
        let method = MethodDefinition::new(
            MessageSelector::Unary("run".into()),
            Vec::new(),
            vec![body_stmt],
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        assert_eq!(
            output,
            "run =>\n  // first comment\n  // second comment\n  42"
        );
    }

    #[test]
    fn method_body_blank_line_has_no_trailing_whitespace() {
        let body = vec![
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
            ExpressionStatement {
                comments: CommentAttachment::default(),
                expression: Expression::Literal(Literal::Integer(2), span()),
                preceding_blank_line: true,
            },
        ];
        let method = MethodDefinition::new(
            MessageSelector::Unary("doStuff".into()),
            Vec::new(),
            body,
            span(),
        );
        let output = unparse_method_definition(&method).to_pretty_string();
        // The blank line between 1 and 2 must be truly empty (no trailing spaces).
        for (i, line_text) in output.lines().enumerate() {
            assert_eq!(
                line_text,
                line_text.trim_end(),
                "line {i} has trailing whitespace: {line_text:?}"
            );
        }
    }

    // --- Class-side method prefix placement ---

    #[test]
    fn class_method_doc_comment_before_class_keyword() {
        // The `class` keyword must appear on the signature line, not before the doc comment.
        let source = "Object subclass: Foo\n  /// Doc for bar\n  class bar => 1\n";
        let module = parse_source(source);
        let output = unparse_module(&module);
        // Doc comment should come first, then "class bar =>"
        assert!(
            output.contains("/// Doc for bar\n  class bar => 1"),
            "expected doc comment before 'class' keyword in: {output}"
        );
    }

    #[test]
    fn state_declaration_doc_comment_round_trips() {
        // A `///` doc comment before `state:` must survive a parse → unparse round-trip.
        let source = "Object subclass: Foo\n  /// The current count.\n  state: count = 0\n";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("  /// The current count.\n  state: count = 0"),
            "expected indented doc comment before state: in: {output}"
        );
    }

    // --- File trailing comments ---

    #[test]
    fn file_trailing_comments_preserved() {
        let source =
            "Object subclass: Foo\n  bar => 1\n\n// trailing comment 1\n// trailing comment 2\n";
        let module = parse_source(source);
        assert_eq!(
            module.file_trailing_comments.len(),
            2,
            "expected 2 trailing comments, got: {:?}",
            module.file_trailing_comments
        );
        let output = unparse_module(&module);
        assert!(
            output.contains("// trailing comment 1"),
            "missing trailing comment 1 in: {output}"
        );
        assert!(
            output.contains("// trailing comment 2"),
            "missing trailing comment 2 in: {output}"
        );
    }

    // --- Match expression formatting ---

    #[test]
    fn match_single_arm_inline() {
        let source = "Actor subclass: A\n  m => x match: [1 -> \"one\"]";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("x match: [1 -> \"one\"]"),
            "single arm should be inline: {output}"
        );
    }

    #[test]
    fn match_multi_arm_one_per_line() {
        let source =
            "Actor subclass: A\n  m => x match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]";
        let module = parse_source(source);
        let output = unparse_module(&module);
        // Each arm should be on its own line, indented inside the brackets
        assert!(
            output.contains("x match: [\n"),
            "multi-arm match should break after opening bracket: {output}"
        );
        assert!(
            output.contains("1 -> \"one\";\n"),
            "first arm should end with semicolon: {output}"
        );
        assert!(
            output.contains("_ -> \"other\"\n"),
            "last arm should not have semicolon: {output}"
        );
    }

    #[test]
    fn match_nil_pattern_round_trips() {
        // ADR 0107 Phase A: `nil` pattern round-trips through parse + unparse.
        let source = "Actor subclass: A\n  m => x match: [nil -> \"none\"; _ -> \"other\"]";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("nil -> \"none\""),
            "nil pattern should round-trip: {output}"
        );
    }

    #[test]
    fn match_type_pattern_round_trips() {
        // ADR 0107 Phase A: `binding :: ClassName` type pattern round-trips
        // through parse + unparse.
        let source = "Actor subclass: A\n  m => x match: [path :: String -> path; _ -> \"other\"]";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("path :: String -> path"),
            "type pattern should round-trip: {output}"
        );
    }

    #[test]
    fn cascade_short_stays_inline() {
        let source = "Actor subclass: A\n  m => self foo; bar; baz";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("self foo; bar; baz"),
            "short cascade should stay inline: {output}"
        );
    }

    #[test]
    fn cascade_multi_message_one_per_line() {
        let source = "Actor subclass: A\n  m =>\n    self assert: txn amount equals: 500; assert: txn from equals: \"Alice\"; assert: txn to equals: \"Bob\"";
        let module = parse_source(source);
        let output = unparse_module(&module);
        // Each cascade message should be on its own line
        assert!(
            output.contains("assert: txn amount equals: 500;\n"),
            "first cascade message should end with semicolon+newline: {output}"
        );
        assert!(
            output.contains("assert: txn from equals: \"Alice\";\n"),
            "middle cascade message should end with semicolon+newline: {output}"
        );
        assert!(
            output.contains("assert: txn to equals: \"Bob\""),
            "last cascade message should not have trailing semicolon: {output}"
        );
    }

    // --- Map literal formatting ---

    #[test]
    fn map_literal_short_stays_inline() {
        let source = "Actor subclass: A\n  m => #{#a => 1, #b => 2}";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("#{#a => 1, #b => 2}"),
            "short map should stay inline: {output}"
        );
    }

    #[test]
    fn map_literal_long_breaks() {
        let source = "Actor subclass: A\n  m => #{#name => \"Alice\", #age => 30, #city => \"Wonderland\", #country => \"Fantasy\"}";
        let module = parse_source(source);
        let output = unparse_module(&module);
        assert!(
            output.contains("#{\n"),
            "long map should break to multi-line: {output}"
        );
    }

    // --- Round-trip: parse → unparse → parse ---

    fn parse_source(source: &str) -> crate::ast::Module {
        use crate::source_analysis::{lex_with_eof, parse};
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        module
    }

    #[test]
    fn round_trip_simple_expression() {
        let source = "42";
        let module = parse_source(source);
        let unparsed = unparse_module(&module);
        let module2 = parse_source(&unparsed);
        assert_eq!(module.expressions.len(), module2.expressions.len());
        // Expressions should be structurally equivalent (ignoring spans)
        for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
            assert!(
                expressions_equivalent(&a.expression, &b.expression),
                "Round-trip mismatch:\n  original: {a:?}\n  after round-trip: {b:?}"
            );
        }
    }

    #[test]
    fn round_trip_assignment() {
        let source = "x := 42";
        let module = parse_source(source);
        let unparsed = unparse_module(&module);
        let module2 = parse_source(&unparsed);
        assert_eq!(module.expressions.len(), module2.expressions.len());
        for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
            assert!(expressions_equivalent(&a.expression, &b.expression));
        }
    }

    #[test]
    fn round_trip_message_send() {
        let source = "x size";
        let module = parse_source(source);
        let unparsed = unparse_module(&module);
        let module2 = parse_source(&unparsed);
        assert_eq!(module.expressions.len(), module2.expressions.len());
        for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
            assert!(
                expressions_equivalent(&a.expression, &b.expression),
                "Round-trip mismatch:\n  original: {a:?}\n  after round-trip: {b:?}"
            );
        }
    }

    #[test]
    fn round_trip_class_definition() {
        let source = "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value";
        let module = parse_source(source);
        let unparsed = unparse_module(&module);
        let module2 = parse_source(&unparsed);
        // Class count must match
        assert_eq!(
            module.classes.len(),
            module2.classes.len(),
            "Class count mismatch after round-trip.\n  unparsed: {unparsed:?}"
        );
        // Class name must match
        assert_eq!(module.classes[0].name.name, module2.classes[0].name.name);
        // Method count must match
        assert_eq!(
            module.classes[0].methods.len(),
            module2.classes[0].methods.len()
        );
    }

    #[test]
    fn round_trip_class_with_native_keyword() {
        let source = "Actor subclass: Foo native: my_module\n  doIt => 42";
        let module = parse_source(source);
        let unparsed = unparse_module(&module);
        let module2 = parse_source(&unparsed);
        assert_eq!(module2.classes.len(), 1);
        assert_eq!(
            module2.classes[0]
                .backing_module
                .as_ref()
                .map(|id| id.name.as_str()),
            Some("my_module"),
            "backing_module lost after round-trip.\n  unparsed: {unparsed:?}"
        );
        assert_eq!(module2.classes[0].methods.len(), 1);
    }

    /// Structural equivalence check ignoring spans.
    fn expressions_equivalent(a: &Expression, b: &Expression) -> bool {
        match (a, b) {
            (Expression::Literal(la, _), Expression::Literal(lb, _)) => la == lb,
            (Expression::Identifier(ia), Expression::Identifier(ib)) => ia.name == ib.name,
            (
                Expression::Assignment {
                    target: ta,
                    value: va,
                    ..
                },
                Expression::Assignment {
                    target: tb,
                    value: vb,
                    ..
                },
            ) => expressions_equivalent(ta, tb) && expressions_equivalent(va, vb),
            (
                Expression::MessageSend {
                    receiver: ra,
                    selector: sa,
                    arguments: aa,
                    is_cast: ca,
                    ..
                },
                Expression::MessageSend {
                    receiver: rb,
                    selector: sb,
                    arguments: ab,
                    is_cast: cb,
                    ..
                },
            ) => {
                ca == cb
                    && sa == sb
                    && expressions_equivalent(ra, rb)
                    && aa.len() == ab.len()
                    && aa.iter().zip(ab).all(|(x, y)| expressions_equivalent(x, y))
            }
            _ => false,
        }
    }

    /// Assert that `source` round-trips through parse→unparse and that the
    /// second pass is idempotent: `unparse(parse(unparse(parse(source)))) ==
    /// unparse(parse(source))`.
    ///
    /// Use this for sources that are already in canonical form so the first
    /// unparse pass should not change them.
    #[track_caller]
    fn assert_idempotent(source: &str) {
        let pass1 = unparse_module(&parse_source(source));
        let pass2 = unparse_module(&parse_source(&pass1));
        assert_eq!(
            pass1, pass2,
            "unparser is not idempotent for source:\n{source}\n\npass1:\n{pass1}\n\npass2:\n{pass2}"
        );
    }

    /// Assert that `source` is already in canonical format: the first format
    /// pass must be a no-op (`format_source(source) == source`).
    ///
    /// Use this for fixture files that are pre-formatted so regressions are
    /// caught immediately — any formatter change that silently alters
    /// already-canonical code will fail here.
    #[track_caller]
    fn assert_identity(source: &str) {
        let formatted = format_source(source)
            .expect("format_source must succeed for canonical source (no parse errors)");
        assert_eq!(
            formatted, source,
            "formatter changed already-canonical source.\n\noriginal:\n{source}\n\nformatted:\n{formatted}"
        );
    }

    // --- Idempotency tests with realistic source strings ---

    #[test]
    fn idempotent_line_comment() {
        assert_idempotent("// a line comment\nx := 42\n");
    }

    #[test]
    fn idempotent_license_header() {
        assert_idempotent(
            "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\nx := 1\n",
        );
    }

    #[test]
    fn idempotent_class_with_line_comment() {
        assert_idempotent("// A useful class\nObject subclass: Foo\n  bar => 42\n");
    }

    #[test]
    fn idempotent_class_definition() {
        assert_idempotent(
            "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value\n",
        );
    }

    // --- Blank line round-trip (BT-987) ---

    #[test]
    fn idempotent_method_body_blank_lines() {
        assert_idempotent("Object subclass: Foo\n\n  doStuff =>\n    x := 1\n\n    x + 1\n");
    }

    #[test]
    fn idempotent_method_body_no_blank_lines() {
        assert_idempotent("Object subclass: Foo\n\n  doStuff =>\n    x := 1\n    x + 1\n");
    }

    #[test]
    fn consecutive_blank_lines_collapsed() {
        // Multiple blank lines should collapse to a single one
        let source = "Object subclass: Foo\n\n  doStuff =>\n    x := 1\n\n\n\n    x + 1\n";
        let pass1 = unparse_module(&parse_source(source));
        // Count actual blank lines between x := 1 and x + 1
        let between = pass1
            .split("x := 1")
            .nth(1)
            .unwrap()
            .split("x + 1")
            .next()
            .unwrap();
        let newline_count = between.chars().filter(|&c| c == '\n').count();
        // Should have exactly 2 newlines (one for end of `x := 1` line, one blank line)
        assert_eq!(
            newline_count, 2,
            "Expected 2 newlines (= 1 blank line) between statements, got {newline_count} in: {between:?}"
        );
        // And it should be idempotent after that
        let pass2 = unparse_module(&parse_source(&pass1));
        assert_eq!(pass1, pass2, "Not idempotent after blank line collapse");
    }

    #[test]
    fn idempotent_module_level_blank_lines() {
        assert_idempotent("x := 1\n\ny := 2\n");
    }

    // --- Block formatting ---

    #[test]
    fn short_block_renders_inline() {
        // A short single-statement block fits within 80 columns → inline
        let source = "x ifTrue: [^1]";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert_eq!(out, "x ifTrue: [^1]");
    }

    #[test]
    fn short_block_with_param_renders_inline() {
        let source = "coll do: [:x | x println]";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert_eq!(out, "coll do: [:x | x println]");
    }

    #[test]
    fn multi_statement_block_always_breaks() {
        let source = "[\n  x println.\n  y println\n]";
        let module = parse_source(source);
        let out = unparse_module(&module);
        // Multi-statement: always broken, newlines separate statements (no dots)
        assert!(
            out.contains("x println\n  y println"),
            "expected broken multi-stmt block without dots in: {out:?}"
        );
    }

    #[test]
    fn long_block_breaks() {
        // Construct a block whose content exceeds 80 columns
        let long_name =
            "aVeryLongVariableNameThatDefinitelyExceedsTheLineWidthLimitWhenInsideABlock";
        let source = format!("x ifTrue: [{long_name}]");
        let module = parse_source(&source);
        let out = unparse_module(&module);
        // Should break: body on next line with 2-space indent, ] on its own line
        assert!(
            out.contains(&format!("[\n  {long_name}\n]")),
            "expected broken block in: {out:?}"
        );
    }

    // --- Idempotency tests for block formatting ---

    #[test]
    fn idempotent_short_block_inline() {
        assert_idempotent("x ifTrue: [^1]\n");
    }

    #[test]
    fn idempotent_block_with_param() {
        assert_idempotent("coll do: [:x | x println]\n");
    }

    #[test]
    fn idempotent_multi_statement_block() {
        assert_idempotent("[\n  x println.\n  y println\n]\n");
    }

    #[test]
    fn idempotent_iftrue_guard() {
        assert_idempotent("flag ifTrue: [^42]\n");
    }

    // --- Idempotency tests using example .bt files ---

    #[test]
    fn idempotent_hello_bt() {
        assert_idempotent(include_str!(
            "../../../../examples/getting-started/src/hello.bt"
        ));
    }

    #[test]
    fn idempotent_hanoi_bt() {
        assert_idempotent(include_str!(
            "../../../../examples/getting-started/src/hanoi.bt"
        ));
    }

    #[test]
    fn idempotent_point_bt() {
        assert_idempotent(include_str!(
            "../../../../examples/getting-started/src/point.bt"
        ));
    }

    // --- Identity tests: first-pass must be a no-op on canonical source ---

    #[test]
    fn identity_hello_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/hello.bt"
        ));
    }

    #[test]
    fn identity_hanoi_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/hanoi.bt"
        ));
    }

    #[test]
    fn identity_point_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/point.bt"
        ));
    }

    #[test]
    fn identity_counter_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/counter.bt"
        ));
    }

    #[test]
    fn identity_logging_counter_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/logging_counter.bt"
        ));
    }

    #[test]
    fn identity_protoobject_proxy_bt() {
        assert_identity(include_str!(
            "../../../../examples/getting-started/src/protoobject_proxy.bt"
        ));
    }

    // --- Identity tests: fixture files ---

    #[test]
    fn identity_fixture_value_class() {
        assert_identity(include_str!("fixtures/value_class.bt"));
    }

    #[test]
    fn identity_fixture_actor_class() {
        assert_identity(include_str!("fixtures/actor_class.bt"));
    }

    #[test]
    fn identity_fixture_method_comments() {
        assert_identity(include_str!("fixtures/method_comments.bt"));
    }

    #[test]
    fn identity_fixture_blocks() {
        assert_identity(include_str!("fixtures/blocks.bt"));
    }

    #[test]
    fn identity_fixture_standalone_methods() {
        assert_identity(include_str!("fixtures/standalone_methods.bt"));
    }

    #[test]
    fn identity_fixture_keyword_blocks() {
        assert_identity(include_str!("fixtures/keyword_blocks.bt"));
    }

    #[test]
    fn identity_fixture_long_keyword_messages() {
        assert_identity(include_str!("fixtures/long_keyword_messages.bt"));
    }

    #[test]
    fn identity_empty_source() {
        assert_identity("");
    }

    // --- Keyword message formatting with block arguments (BT-1064) ---

    #[test]
    fn keyword_single_stmt_block_stays_inline() {
        // Single-statement block: keyword stays on the same line as receiver.
        let source = "Object subclass: A\n  m => x ifTrue: [^42]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("x ifTrue: [^42]"),
            "single-stmt block should stay inline: {out}"
        );
    }

    #[test]
    fn keyword_multi_stmt_block_breaks_keywords() {
        // Multi-statement block: all keywords break to their own indented lines.
        let source = "Object subclass: A\n  m =>\n    flag\n      ifTrue: [\n        a showCr: \"x\"\n        b showCr: \"y\"\n      ]\n      ifFalse: [nil]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("flag\n      ifTrue: ["),
            "receiver should be on its own line before ifTrue: {out}"
        );
        assert!(
            out.contains("      ifFalse: [nil]"),
            "ifFalse: should be on its own indented line: {out}"
        );
    }

    #[test]
    fn keyword_multi_stmt_block_idempotent() {
        assert_idempotent(
            "Object subclass: A\n  m =>\n    flag\n      ifTrue: [\n        self showCr: \"yes\"\n        self showCr: \"done\"\n      ]\n      ifFalse: [nil]\n",
        );
    }

    #[test]
    fn keyword_single_keyword_multi_stmt_breaks() {
        // Single keyword with a multi-statement block: keyword breaks to own line.
        let source = "Object subclass: A\n  m =>\n    coll\n      do: [\n        self showCr: \"a\"\n        self showCr: \"b\"\n      ]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("coll\n      do: ["),
            "receiver should be on its own line before do: {out}"
        );
    }

    // --- BT-1294: 3+ keyword always-break, block body multiline, no ]] stacking ---

    #[test]
    fn three_keywords_always_break() {
        // 3 keyword parts → always break, regardless of total length.
        let source = "Object subclass: A\n  m => CandidateFilter filterEligible: c config: self.config running: self.running\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("CandidateFilter\n"),
            "receiver must be on its own line for 3+ keywords: {out}"
        );
        assert!(
            out.contains("      filterEligible:"),
            "first keyword must be indented: {out}"
        );
        assert!(
            out.contains("      config:"),
            "second keyword must be indented: {out}"
        );
        assert!(
            out.contains("      running:"),
            "third keyword must be indented: {out}"
        );
    }

    #[test]
    fn three_keywords_always_break_idempotent() {
        let source = "Object subclass: A\n  m =>\n    CandidateFilter\n      filterEligible: c\n      config: self.config\n      running: self.running\n";
        assert_idempotent(source);
    }

    #[test]
    fn four_keywords_always_break() {
        // 4 keyword parts — real symphony pattern (orchestrator.bt:96).
        let source = "Object subclass: A\n  m => CandidateFilter terminatedEntries: self.running stateMap: stateMap activeStates: self.config trackerActiveStates terminalStates: self.config trackerTerminalStates\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        // No line should exceed 120 chars (measured by raw byte length).
        for line in out.lines() {
            assert!(
                line.len() <= 120,
                "line exceeds 120 chars after formatting: {line:?}"
            );
        }
        assert!(
            out.contains("      terminatedEntries:"),
            "first keyword must break: {out}"
        );
        assert!(
            out.contains("      terminalStates:"),
            "last keyword must break: {out}"
        );
    }

    #[test]
    fn two_keywords_short_stays_inline() {
        // 2 keyword parts that are short → stays inline (no change to current behaviour).
        let source = "Object subclass: A\n  m => dict at: #key ifAbsent: [nil]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("dict at: #key ifAbsent: [nil]"),
            "short 2-keyword message should stay inline: {out}"
        );
    }

    #[test]
    fn block_with_multiline_body_breaks_outer_close() {
        // Block whose body renders multiline (keyword send with multiline block args)
        // must use always-break form so `]` gets its own line (no `]]` stacking).
        let source = "Object subclass: A\n  m =>\n    resp\n      andThen: [:r |\n        r ok\n          ifTrue: [\n            x := r body\n            Result ok: x\n          ]\n          ifFalse: [Result error: #http_error]\n      ]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        // `]]` must not appear — every `]` must be on its own line.
        assert!(!out.contains("]]"), "no `]]` stacking allowed: {out}");
        assert_idempotent(source);
    }

    #[test]
    fn block_with_single_short_keyword_stays_inline() {
        // Single-statement block whose body is a short 1-keyword send must stay inline.
        let source = "Object subclass: A\n  m => flag ifTrue: [self showCr: \"yes\"]\n";
        let module = parse_source(source);
        let out = unparse_module(&module);
        assert!(
            out.contains("ifTrue: [self showCr: \"yes\"]"),
            "short single-keyword block body should stay inline: {out}"
        );
    }

    // --- BT-1294: class-side method with 3+ keyword body (non-idempotency regression) ---

    #[test]
    fn class_side_four_keyword_body_idempotent() {
        // Actor subclass with a `class` method whose body is a 4-keyword send followed
        // by an instance method.  The formatter must not merge the broken keyword lines
        // with the next method's selector on the second pass.
        let source = concat!(
            "Actor subclass: Subprocess\n",
            "\n",
            "  class open: command args: args env: env dir: dir -> Result =>\n",
            "    (Erlang beamtalk_subprocess)\n",
            "      open: command\n",
            "      args: args\n",
            "      env: env\n",
            "      dir: dir\n",
            "\n",
            "  writeLine: data -> Nil =>\n",
            "    (Erlang beamtalk_subprocess) writeLine: self data: data\n",
        );
        assert_idempotent(source);
    }

    #[test]
    fn class_side_four_keyword_body_identity() {
        // Same source must be already in canonical form (no change on first pass).
        let source = concat!(
            "Actor subclass: Subprocess\n",
            "\n",
            "  class open: command args: args env: env dir: dir -> Result =>\n",
            "    (Erlang beamtalk_subprocess)\n",
            "      open: command\n",
            "      args: args\n",
            "      env: env\n",
            "      dir: dir\n",
            "\n",
            "  writeLine: data -> Nil =>\n",
            "    (Erlang beamtalk_subprocess) writeLine: self data: data\n",
        );
        assert_identity(source);
    }

    // --- Protocol round-trip (BT-1618) ---

    #[test]
    fn protocol_class_method_doc_comment_round_trip() {
        // BT-1618: `class` prefix must appear on the signature line, after doc comments.
        let source = concat!(
            "Protocol define: Parseable\n",
            "  /// Reconstruct from string.\n",
            "  class fromString: aString :: String -> Self\n",
        );
        assert_identity(source);
    }

    #[test]
    fn protocol_class_method_no_doc_comment_round_trip() {
        let source = concat!("Protocol define: Creatable\n", "  class create -> Self\n",);
        assert_identity(source);
    }

    #[test]
    fn bare_primitive_round_trips_without_selector_string() {
        // BT-2724: a bare `@primitive` (selector inferred from the method) must
        // not be rewritten with an explicit selector string by the formatter.
        let source = concat!("Object subclass: Foo\n", "  size => @primitive\n");
        let formatted = format_source(source).expect("format_source must succeed");
        assert!(
            formatted.contains("@primitive\n"),
            "expected bare @primitive, got:\n{formatted}"
        );
        assert!(
            !formatted.contains("@primitive \""),
            "bare @primitive should not gain a selector string:\n{formatted}"
        );
        assert_idempotent(source);
    }

    #[test]
    fn explicit_primitive_selector_string_preserved() {
        // BT-2724: an explicit selector string (genuine rename) is preserved.
        let source = concat!(
            "Object subclass: Foo\n",
            "  signal => @primitive \"classSignal\"\n"
        );
        let formatted = format_source(source).expect("format_source must succeed");
        assert!(
            formatted.contains("@primitive \"classSignal\""),
            "explicit selector string should be preserved, got:\n{formatted}"
        );
    }

    #[test]
    fn protocol_instance_method_doc_comment_round_trip() {
        let source = concat!(
            "Protocol define: Displayable\n",
            "  /// Convert to display string.\n",
            "  asString -> String\n",
        );
        assert_identity(source);
    }

    #[test]
    fn protocol_trailing_comment_round_trip() {
        // BT-2906: a trailing end-of-line comment on the `Protocol define:`
        // declaration line must round-trip losslessly, mirroring the
        // identical fix for `type` declarations below.
        let source = concat!(
            "Protocol define: Sortable  // comment\n",
            "  sortKey -> Object\n",
        );
        assert_identity(source);
    }

    // --- Type alias round-trip (ADR 0108, Phase 1, BT-2894) ---

    #[test]
    fn type_alias_simple_round_trip() {
        let source = "type Port = Integer\n";
        assert_identity(source);
    }

    #[test]
    fn type_alias_singleton_union_round_trip() {
        let source = "type RestartStrategy = #temporary | #transient | #permanent\n";
        assert_identity(source);
    }

    #[test]
    fn type_alias_doc_comment_round_trip() {
        let source = concat!(
            "/// How a supervised child restarts after exit.\n",
            "type RestartStrategy = #temporary | #transient | #permanent\n",
        );
        assert_identity(source);
    }

    #[test]
    fn type_alias_generic_round_trip() {
        let source = "type IntList = List(Integer)\n";
        assert_identity(source);
    }

    #[test]
    fn type_alias_difference_round_trip() {
        let source = "type PublicTag = Symbol \\ (#reserved | #internal)\n";
        assert_identity(source);
    }

    #[test]
    fn internal_type_alias_round_trip() {
        // ADR 0071, ADR 0108 Phase 5, BT-2898.
        let source = "internal type ParserState = Integer | String\n";
        assert_identity(source);
    }

    #[test]
    fn type_alias_trailing_comment_round_trip() {
        // BT-2906: a trailing end-of-line comment on the declaration line
        // must round-trip losslessly instead of being dropped.
        let source = "type Port = Integer  // comment\n";
        assert_identity(source);
    }

    // --- Class definition header trailing comment (BT-2933) ---

    #[test]
    fn class_header_trailing_comment_round_trip() {
        // BT-2933: a trailing end-of-line comment on the `subclass:` header
        // line must round-trip losslessly, mirroring the identical fix for
        // type alias/protocol declarations (BT-2906). Before the fix,
        // `parse_class_definition` never populated `comments.trailing`, so
        // `unparse_class_definition`'s trailing-comment branch was dead code
        // and the comment was silently dropped.
        // A class with no `state:` declarations always gets a blank line
        // before its first method (canonical formatting, unrelated to this
        // fix), hence the blank line in the expected output below.
        let source = concat!(
            "Object subclass: Foo  // header comment\n",
            "\n",
            "  x => 1\n",
        );
        assert_identity(source);
    }

    #[test]
    fn class_header_trailing_comment_with_type_params_round_trip() {
        // BT-2933: the trailing comment attaches after the last header
        // token — here, the type parameter list.
        let source = concat!(
            "Object subclass: Box(T)  // header comment\n",
            "\n",
            "  x => 1\n",
        );
        assert_identity(source);
    }

    #[test]
    fn class_header_trailing_comment_with_native_round_trip() {
        // BT-2933: the trailing comment attaches after the `native:` module
        // name — the last header token when present.
        let source = concat!(
            "Object subclass: Foo native: my_module  // header comment\n",
            "\n",
            "  x => 1\n",
        );
        assert_identity(source);
    }

    #[test]
    fn class_header_trailing_comment_with_state_round_trip() {
        // BT-2933 (review follow-up): the most common real-world class
        // shape — a header trailing comment plus a `state:` declaration.
        let source = concat!(
            "Object subclass: Counter  // counter class\n",
            "  state: count :: Integer = 0\n",
            "\n",
            "  increment => count := count + 1\n",
        );
        assert_identity(source);
    }

    // --- BT-2924 regression: `type` alias sandwiched between a class's doc
    // comment and the class itself must not lose the class's doc comment. ---

    /// The exact repro shape from BT-2924: a class's doc comment, a blank
    /// line, a `type` alias with its own directly-adjacent doc comment, a
    /// blank line, then the class declaration the first doc comment
    /// describes.
    ///
    /// Before the fix, `format_source` deleted the class's doc comment
    /// outright (it lives in the `type` alias token's leading trivia, and
    /// `collect_doc_comment` kept only the alias's own — nearer — block,
    /// discarding the earlier one without preserving it anywhere).
    const HTTPSERVER_REPRO_SOURCE: &str = concat!(
        "/// HTTPServer — cowboy-backed HTTP server actor for Beamtalk.\n",
        "/// Some more description text here.\n",
        "/// @see HTTPResponse (for building response objects)\n",
        "\n",
        "/// Anything `HTTPServer start:handler:` accepts as a request handler.\n",
        "type HTTPHandlerLike = Integer | String\n",
        "\n",
        "Actor subclass: HTTPServer\n",
        "  start => 1\n",
    );

    #[test]
    fn type_alias_preceded_by_orphaned_class_doc_preserves_all_text() {
        let formatted = format_source(HTTPSERVER_REPRO_SOURCE)
            .expect("format_source must succeed for valid source");

        // Neither doc comment's content may be dropped.
        for line in [
            "/// HTTPServer — cowboy-backed HTTP server actor for Beamtalk.",
            "/// Some more description text here.",
            "/// @see HTTPResponse (for building response objects)",
            "/// Anything `HTTPServer start:handler:` accepts as a request handler.",
        ] {
            assert!(
                formatted.contains(line),
                "formatted output lost {line:?}:\n{formatted}"
            );
        }

        // Formatting must be idempotent — a second pass changes nothing.
        let formatted_again =
            format_source(&formatted).expect("format_source must succeed on formatted output");
        assert_eq!(
            formatted, formatted_again,
            "format_source is not idempotent for the BT-2924 repro shape"
        );
    }

    #[test]
    fn type_alias_preceded_by_orphaned_class_doc_reattaches_correctly_on_reparse() {
        let formatted = format_source(HTTPSERVER_REPRO_SOURCE)
            .expect("format_source must succeed for valid source");
        let module = parse_source(&formatted);

        assert_eq!(module.type_aliases.len(), 1);
        assert_eq!(
            module.type_aliases[0].doc_comment.as_deref(),
            Some("Anything `HTTPServer start:handler:` accepts as a request handler."),
            "the alias's own doc comment must still attach to the alias after a format round-trip"
        );
        assert_eq!(module.classes.len(), 1);
        assert_eq!(module.classes[0].name.name, "HTTPServer");
    }

    #[test]
    fn type_alias_with_directly_adjacent_doc_comment_has_no_unattached_warning() {
        // BT-2924 secondary bug: the lint must not flag the alias's own
        // directly-adjacent `///` comment as unattached just because an
        // earlier, unrelated block shares the same leading trivia.
        use crate::source_analysis::{Severity, lex_with_eof, parse};
        let tokens = lex_with_eof(HTTPSERVER_REPRO_SOURCE);
        let (_module, diagnostics) = parse(tokens);
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert!(
            warnings.is_empty(),
            "expected no warnings for the BT-2924 repro shape, got: {warnings:?}"
        );
    }

    // --- BT-2907: top-level declaration order (classes/protocols/type aliases) ---
    //
    // `unparse_module_doc` used to always emit type aliases first, then
    // protocols, then classes — regardless of where each declaration
    // appeared in the source. A `type` alias (or a protocol) declared
    // *after* a class would jump to the top of the file on a format
    // round-trip. The three declaration kinds are now interleaved back into
    // their original source order.

    #[test]
    fn type_alias_after_class_preserves_source_order() {
        // Before the fix, this `type` alias — declared after the class —
        // would have been hoisted above `Actor subclass: Server`.
        let source = concat!(
            "Actor subclass: Server\n",
            "  start => 1\n",
            "\n",
            "type Port = Integer\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");
        let class_pos = formatted
            .find("Actor subclass: Server")
            .expect("class declaration must be present");
        let alias_pos = formatted
            .find("type Port = Integer")
            .expect("type alias declaration must be present");
        assert!(
            class_pos < alias_pos,
            "a type alias declared after a class must stay after it on format; \
             formatted output:\n{formatted}"
        );

        // Idempotent: formatting the already-formatted output changes nothing.
        let formatted_again =
            format_source(&formatted).expect("format_source must succeed on formatted output");
        assert_eq!(
            formatted, formatted_again,
            "format_source is not idempotent"
        );
    }

    #[test]
    fn protocol_after_class_preserves_source_order() {
        // Same category of bug as the type-alias case above, for protocols:
        // before the fix, a protocol declared after a class would jump above it.
        let source = concat!(
            "Actor subclass: Server\n",
            "  start => 1\n",
            "\n",
            "Protocol define: Startable\n",
            "  start -> Integer\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");
        let class_pos = formatted
            .find("Actor subclass: Server")
            .expect("class declaration must be present");
        let protocol_pos = formatted
            .find("Protocol define: Startable")
            .expect("protocol declaration must be present");
        assert!(
            class_pos < protocol_pos,
            "a protocol declared after a class must stay after it on format; \
             formatted output:\n{formatted}"
        );

        let formatted_again =
            format_source(&formatted).expect("format_source must succeed on formatted output");
        assert_eq!(
            formatted, formatted_again,
            "format_source is not idempotent"
        );
    }

    #[test]
    fn class_protocol_type_alias_interleaved_preserves_relative_order() {
        // Classes, protocols, and type aliases interleaved in source must
        // come back out in the same relative order — not grouped by kind.
        let source = concat!(
            "type Port = Integer\n",
            "\n",
            "Protocol define: Startable\n",
            "  start -> Integer\n",
            "\n",
            "Actor subclass: Server\n",
            "  start => 1\n",
            "\n",
            "type Timeout = Integer\n",
            "\n",
            "Actor subclass: Client\n",
            "  connect => 1\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");

        let positions = [
            ("type Port = Integer", "type Port"),
            ("Protocol define: Startable", "Protocol define: Startable"),
            ("Actor subclass: Server", "Actor subclass: Server"),
            ("type Timeout = Integer", "type Timeout"),
            ("Actor subclass: Client", "Actor subclass: Client"),
        ]
        .map(|(needle, label)| {
            (
                formatted
                    .find(needle)
                    .unwrap_or_else(|| panic!("{label} must be present:\n{formatted}")),
                label,
            )
        });

        for pair in positions.windows(2) {
            let (prev_pos, prev_label) = pair[0];
            let (next_pos, next_label) = pair[1];
            assert!(
                prev_pos < next_pos,
                "{prev_label} must come before {next_label} on format; \
                 formatted output:\n{formatted}"
            );
        }

        let formatted_again =
            format_source(&formatted).expect("format_source must succeed on formatted output");
        assert_eq!(
            formatted, formatted_again,
            "format_source is not idempotent"
        );
    }

    // --- BT-2929: blank line between top-level declarations ---
    //
    // `unparse_module_doc` used to always emit exactly one `line()` after
    // each top-level declaration, regardless of whether the source had a
    // blank line there — so the separating blank line was either silently
    // dropped, or (for a class) looked "relocated" into the class's own
    // body, since a class unconditionally gets a blank line before its
    // first method whether or not it has state declarations (an unrelated,
    // pre-existing formatting rule — see the `Blank line before first
    // method` comment in `unparse_class_definition`). All three assertions
    // below use `assert_identity` with sources that are already in that
    // canonical form, so a regression here fails on the *first* format
    // pass rather than requiring a second one to notice non-idempotency.

    #[test]
    fn blank_line_preserved_type_alias_then_class() {
        // Repro A from BT-2929.
        assert_identity(concat!(
            "type Port = Integer\n",
            "\n",
            "Object subclass: Foo\n",
            "\n",
            "  m => 1\n",
        ));
    }

    #[test]
    fn blank_line_preserved_class_then_class() {
        // Repro B from BT-2929 — confirms the bug wasn't type-alias-specific.
        assert_identity(concat!(
            "Object subclass: A\n",
            "\n",
            "  m => 1\n",
            "\n",
            "Object subclass: B\n",
            "\n",
            "  n => 2\n",
        ));
    }

    #[test]
    fn blank_line_preserved_type_alias_then_type_alias() {
        // Repro C from BT-2929 — the blank line was dropped outright here
        // (neither declaration has a body to "absorb" it into).
        assert_identity(concat!(
            "type Port = Integer\n",
            "\n",
            "type Timeout = Integer\n",
        ));
    }

    #[test]
    fn no_blank_line_between_top_level_declarations_stays_absent() {
        // The converse of the three tests above: declarations with *no*
        // blank line between them in the source must not gain one.
        assert_identity(concat!("type Port = Integer\n", "type Timeout = Integer\n",));
    }

    #[test]
    fn blank_line_preserved_across_all_three_declaration_kinds_interleaved() {
        // Full combination: type alias, protocol, and class, each separated
        // by a blank line — every pairwise gap (type-alias/protocol,
        // protocol/class, class/type-alias) must round-trip.
        assert_identity(concat!(
            "type Port = Integer\n",
            "\n",
            "Protocol define: Startable\n",
            "  start -> Integer\n",
            "\n",
            "Object subclass: Server\n",
            "\n",
            "  start => 1\n",
            "\n",
            "type Timeout = Integer\n",
        ));
    }

    #[test]
    fn state_declaration_preceded_by_orphaned_doc_block_does_not_merge_into_own_doc_comment() {
        // BT-2924 follow-up (found in adversarial review): a `///` block that
        // breaks away from a *different* declaration must not get glued onto
        // — and then silently merged into — a state field's own doc comment
        // on a format round-trip.
        let source = concat!(
            "Object subclass: Foo\n",
            "  /// Section header orphan.\n",
            "\n",
            "  /// The port to bind.\n",
            "  state: port :: Integer = 0\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");
        let module = parse_source(&formatted);
        assert_eq!(module.classes.len(), 1);
        let field = &module.classes[0].state[0];
        assert_eq!(
            field.doc_comment.as_deref(),
            Some("The port to bind."),
            "the orphaned block above must not merge into the field's own doc comment; \
             formatted output:\n{formatted}"
        );
        assert!(
            formatted.contains("Section header orphan."),
            "the orphaned block's text must still be preserved somewhere:\n{formatted}"
        );
    }

    #[test]
    fn protocol_preceded_by_orphaned_doc_block_does_not_merge_into_own_doc_comment() {
        // BT-2924 follow-up (found in adversarial review): same shape as
        // above, for a `Protocol define:` declaration.
        let source = concat!(
            "/// Orphaned block, meant for something else entirely.\n",
            "\n",
            "/// Things that can be printed.\n",
            "Protocol define: Displayable\n",
            "  asString -> String\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");
        let module = parse_source(&formatted);
        assert_eq!(module.protocols.len(), 1);
        assert_eq!(
            module.protocols[0].doc_comment.as_deref(),
            Some("Things that can be printed."),
            "the orphaned block above must not merge into the protocol's own doc comment; \
             formatted output:\n{formatted}"
        );
        assert!(
            formatted.contains("Orphaned block, meant for something else entirely."),
            "the orphaned block's text must still be preserved somewhere:\n{formatted}"
        );
    }

    #[test]
    fn method_with_no_doc_comment_preceded_by_orphaned_doc_block_does_not_attach_it() {
        // BT-2924 follow-up (review finding on the fix itself): a `///` block
        // that breaks away from a different declaration must not attach to a
        // *method with no doc comment of its own* on a format round-trip —
        // the class/protocol/state cases were fixed, but the method case was
        // missed since its blank-line guard lives inside `if let Some(doc)`.
        let source = concat!(
            "Object subclass: Foo\n",
            "  /// Section header (orphaned — blank line below).\n",
            "\n",
            "  doSomething => 1\n",
        );
        let formatted = format_source(source).expect("format_source must succeed for valid source");
        let module = parse_source(&formatted);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(
            method.doc_comment, None,
            "the orphaned block above must not attach as the method's own doc comment; \
             formatted output:\n{formatted}"
        );
        assert!(
            formatted.contains("Section header (orphaned"),
            "the orphaned block's text must still be preserved somewhere:\n{formatted}"
        );
    }

    // --- escape_string_literal ---

    #[test]
    fn escape_string_literal_plain_string_unchanged() {
        assert_eq!(escape_string_literal("hello"), "hello");
    }

    #[test]
    fn escape_string_literal_escapes_double_quote() {
        assert_eq!(escape_string_literal("a\"b"), "a\\\"b");
    }

    #[test]
    fn escape_string_literal_escapes_backslash() {
        assert_eq!(escape_string_literal("a\\b"), "a\\\\b");
    }

    #[test]
    fn escape_string_literal_escapes_open_brace() {
        assert_eq!(escape_string_literal("a{b}"), "a\\{b}");
    }

    #[test]
    fn escape_string_literal_backslash_before_brace_both_escaped() {
        // Backslash must be escaped before brace so "\{x}" → "\\\{x}" (escaped
        // backslash `\\` + escaped brace `\{`), not "\\{x}" (escaped backslash
        // followed by an unescaped `{` that would start interpolation).
        assert_eq!(escape_string_literal("\\{x}"), "\\\\\\{x}");
    }

    #[test]
    fn escape_string_literal_mixed_backslash_quote_brace() {
        // Verifies all three escape rules fire correctly in one input and that
        // replacement order does not corrupt any of them.
        assert_eq!(escape_string_literal("\\\"\\{"), "\\\\\\\"\\\\\\{");
    }

    // --- reindent_method_source (BT-2584) ---

    #[test]
    fn reindent_empty_base_is_identity_for_canonical() {
        // Canonical (column-0) input + empty base indent is a no-op.
        let src = "foo => 1\n";
        assert_eq!(reindent_method_source("", src), src);
    }

    #[test]
    fn reindent_shifts_canonical_body_to_base() {
        // A column-0 single-line method shifted to 2-space class-body indent.
        assert_eq!(reindent_method_source("  ", "foo => 1\n"), "  foo => 1\n");
    }

    #[test]
    fn reindent_preserves_relative_indentation() {
        // A multi-statement body keeps its 2-space relative step under the new
        // base (the body stays multi-line because it is genuinely > 1 statement).
        assert_eq!(
            reindent_method_source("  ", "foo =>\n  a\n  b\n"),
            "  foo =>\n    a\n    b\n"
        );
    }

    #[test]
    fn reindent_rebreaks_line_too_wide_at_indent() {
        // BT-2594: the core re-layout property. A single-expression body that
        // fits inline at column 0 (exactly 80 cols) must break to its own line
        // once indented, because the indent steals from the width budget — a pure
        // whitespace shift could not do this.
        let src =
            "compute => self firstThing + self secondThing + self thirdThing + moreStuffHereX\n";
        // At column 0 it fits and stays inline.
        assert_eq!(reindent_method_source("", src), src);
        // Indented two spaces, the same body must break to the next line.
        assert_eq!(
            reindent_method_source("  ", src),
            "  compute =>\n    self firstThing + self secondThing + self thirdThing + moreStuffHereX\n"
        );
    }

    #[test]
    fn reindent_preserves_blank_line_between_statements() {
        // A blank line the author left between two body statements (BT-987) is
        // preserved by the re-layout and shifted to the new base (blank stays
        // empty — no indent, no trailing space).
        assert_eq!(
            reindent_method_source("  ", "foo =>\n  a\n\n  b\n"),
            "  foo =>\n    a\n\n    b\n"
        );
    }

    #[test]
    fn reindent_inline_comment_preserved() {
        // A trailing `//` comment rides along with its statement.
        let src = "foo => 1  // bump\n";
        assert_eq!(reindent_method_source("  ", src), "  foo => 1  // bump\n");
    }

    #[test]
    fn reindent_doc_comment_and_multiline_body() {
        // A `///` doc comment sits directly above the signature, and a
        // multi-statement body keeps its relative indentation.
        let src = "/// doc\nfoo =>\n  a\n  b\n";
        assert_eq!(
            reindent_method_source("  ", src),
            "  /// doc\n  foo =>\n    a\n    b\n"
        );
    }

    #[test]
    fn reindent_tab_base_indent() {
        // A tab base indent is prepended verbatim to every non-blank line.
        assert_eq!(
            reindent_method_source("\t", "foo =>\n  a\n  b\n"),
            "\tfoo =>\n\t  a\n\t  b\n"
        );
    }

    #[test]
    fn reindent_no_trailing_newline_is_preserved() {
        // The disk slice clamps to EOF when the final body line has no trailing
        // newline; reindent must not add one.
        assert_eq!(reindent_method_source("  ", "foo => 1"), "  foo => 1");
    }

    #[test]
    fn reindent_is_idempotent_on_canonical_disk_shape() {
        // Re-indenting an already-canonical, already-disk-shaped method to the
        // same base is a no-op.
        let disk = "  /// doc\n  foo =>\n    a\n    b\n";
        assert_eq!(reindent_method_source("  ", disk), disk);
    }

    #[test]
    fn reindent_emits_class_prefix_for_class_side_method() {
        // BT-2594: the `class ` modifier is recovered from the re-parsed method
        // and re-emitted, so a class-side method's stored source keeps its prefix.
        assert_eq!(
            reindent_method_source("  ", "class spawn => self new\n"),
            "  class spawn => self new\n"
        );
    }
}
