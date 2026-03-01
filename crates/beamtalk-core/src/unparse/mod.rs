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
//! - Functions return `Document<'static>` using `Document::String` for all
//!   content derived from AST data (identifiers, literal values, etc.).

use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, ClassDefinition, Comment, CommentAttachment, CommentKind, ExpectCategory,
    Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MapPair, MatchArm,
    MessageSelector, MethodDefinition, Module, Pattern, StandaloneMethodDefinition,
    StateDeclaration, StringSegment, TypeAnnotation,
};
use crate::codegen::core_erlang::document::{Document, break_, concat, group, line, nest, nil};
use crate::docvec;

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
    unparse_method_definition(method).to_pretty_string()
}

/// Unparses a [`ClassDefinition`] to source text.
#[must_use]
pub fn unparse_class(class: &ClassDefinition) -> String {
    unparse_class_definition(class).to_pretty_string()
}

/// Unparses a method signature for help display (BT-988).
///
/// Renders `selector params -> ReturnType` without `sealed` prefix or ` =>` suffix.
/// Used by codegen to embed display signatures in `methodSignatures` maps.
#[must_use]
pub fn unparse_method_display_signature(method: &MethodDefinition) -> String {
    unparse_method_display_signature_doc(method).to_pretty_string()
}

/// Builds a [`Document`] for a method display signature (no `sealed`, no ` =>`).
fn unparse_method_display_signature_doc(method: &MethodDefinition) -> Document<'static> {
    let sig = match &method.selector {
        MessageSelector::Unary(name) => Document::String(name.to_string()),
        MessageSelector::Binary(op) => {
            let param = &method.parameters[0];
            docvec![
                Document::String(op.to_string()),
                " ",
                Document::String(param.name.name.to_string()),
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
                    sig_docs.push(Document::String(param.name.name.to_string()));
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

/// Builds a [`Document`] for a [`Module`].
#[must_use]
pub(crate) fn unparse_module_doc(module: &Module) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // File-level leading comments (empty module edge case — ADR 0044)
    for comment in &module.file_leading_comments {
        docs.push(unparse_comment(comment));
        docs.push(line());
    }

    // Classes
    for class in &module.classes {
        docs.push(unparse_class_definition(class));
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
                docs.push(docvec!["/// ", Document::String(line_text.to_string())]);
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
    if class.is_abstract {
        modifiers.push(Document::Str("abstract "));
    }
    if class.is_sealed {
        modifiers.push(Document::Str("sealed "));
    }
    if class.is_typed {
        modifiers.push(Document::Str("typed "));
    }

    let header = docvec![
        concat(modifiers),
        Document::String(superclass),
        " subclass: ",
        Document::String(class.name.name.to_string()),
    ];

    let header = if let Some(trail) = &class.comments.trailing {
        docvec![header, "  ", unparse_comment(trail)]
    } else {
        header
    };

    docs.push(header);

    // State declarations
    for state in &class.state {
        docs.push(line());
        docs.push(Document::Str("  "));
        docs.push(unparse_state_declaration(state));
    }

    // Class variables
    for state in &class.class_variables {
        docs.push(line());
        docs.push(Document::Str("  "));
        docs.push(unparse_class_state_declaration(state));
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
    let class = Document::String(smd.class_name.name.to_string());
    let separator = if smd.is_class_method {
        Document::Str(" class >> ")
    } else {
        Document::Str(" >> ")
    };
    docvec![class, separator, unparse_method_definition(&smd.method)]
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
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&method.comments));

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &method.doc_comment {
        // Blank line between leading comments and doc comment (e.g. section separators)
        if !method.comments.leading.is_empty() {
            docs.push(line());
        }
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", Document::String(line_text.to_string())]);
            }
            docs.push(line());
        }
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
        MessageSelector::Unary(name) => Document::String(name.to_string()),
        MessageSelector::Binary(op) => {
            let param = &method.parameters[0];
            docvec![
                Document::String(op.to_string()),
                " ",
                Document::String(param.name.name.to_string()),
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
                    sig_docs.push(Document::String(param.name.name.to_string()));
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

    docvec![sealed, sig, return_type, " =>"]
}

fn unparse_keyword_part(part: &KeywordPart) -> Document<'static> {
    Document::String(part.keyword.to_string())
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

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &state.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", Document::String(line_text.to_string())]);
            }
            docs.push(line());
        }
    }

    let keyword = if is_class { "classState: " } else { "state: " };
    let mut decl: Vec<Document<'static>> = vec![
        Document::Str(keyword),
        Document::String(state.name.name.to_string()),
    ];

    if let Some(ty) = &state.type_annotation {
        decl.push(Document::Str(": "));
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
pub(crate) fn unparse_expression(expr: &Expression) -> Document<'static> {
    match expr {
        Expression::Literal(lit, _) => unparse_literal(lit),
        Expression::Identifier(id) => unparse_identifier(id),
        Expression::ClassReference { name, .. } => Document::String(name.name.to_string()),
        Expression::Super(_) => Document::Str("super"),
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            docvec![
                unparse_expression(receiver),
                ".",
                Document::String(field.name.to_string()),
            ]
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
        Expression::Assignment { target, value, .. } => {
            docvec![
                unparse_expression(target),
                " := ",
                unparse_expression(value)
            ]
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
        Expression::Match { value, arms, .. } => unparse_match(value, arms),
        Expression::MapLiteral { pairs, .. } => unparse_map_literal(pairs),
        Expression::ListLiteral { elements, tail, .. } => {
            unparse_list_literal(elements, tail.as_deref())
        }
        Expression::ArrayLiteral { elements, .. } => unparse_array_literal(elements),
        Expression::Primitive {
            name,
            is_quoted,
            is_intrinsic,
            ..
        } => {
            let directive = if *is_intrinsic {
                "@intrinsic"
            } else {
                "@primitive"
            };
            if *is_quoted {
                docvec![directive, " \"", Document::String(name.to_string()), "\""]
            } else {
                docvec![directive, " ", Document::String(name.to_string())]
            }
        }
        Expression::StringInterpolation { segments, .. } => unparse_string_interpolation(segments),
        Expression::ExpectDirective { category, .. } => {
            docvec!["@expect ", unparse_expect_category(*category)]
        }
        Expression::Error { message, .. } => {
            // Emit a comment indicating the error rather than nothing.
            // Escape `*/` in the message to prevent breaking the block comment.
            let safe_msg = message.as_str().replace("*/", "* /");
            docvec!["/* error: ", Document::String(safe_msg), " */"]
        }
    }
}

// --- Literal unparsing ---

fn unparse_literal(lit: &Literal) -> Document<'static> {
    match lit {
        Literal::Integer(n) => Document::String(n.to_string()),
        Literal::Float(f) => Document::String(format_float(*f)),
        Literal::String(s) => {
            // The lexer unescapes doubled delimiters ("") to bare " in the AST,
            // but preserves backslash escapes (\" stays as \"). We need to
            // re-escape any bare " that isn't already preceded by \.
            docvec!["\"", Document::String(escape_string_quotes(s)), "\""]
        }
        Literal::Symbol(s) => {
            // Symbols: #name or #'name with spaces'
            if needs_symbol_quoting(s) {
                docvec!["#'", Document::String(s.to_string()), "'"]
            } else {
                docvec!["#", Document::String(s.to_string())]
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
        Literal::Character(c) => {
            // $a or $\n etc.
            Document::String(format!("${c}"))
        }
    }
}

/// Format a float, preserving a decimal point.
fn format_float(f: f64) -> String {
    let s = format!("{f}");
    if s.contains('.') || s.contains('e') {
        s
    } else {
        format!("{f}.0")
    }
}

/// Escape bare `"` characters in a string that aren't already backslash-escaped.
/// The lexer unescapes doubled delimiters (`""` → `"`) but preserves backslash
/// escapes (`\"` stays as `\"`). We re-escape bare quotes using `\"`.
fn escape_string_quotes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Backslash escape sequence — preserve as-is
            result.push('\\');
            if let Some(next) = chars.next() {
                result.push(next);
            }
        } else if c == '"' {
            // Bare quote — re-escape it
            result.push('\\');
            result.push('"');
        } else {
            result.push(c);
        }
    }
    result
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
    Document::String(id.name.to_string())
}

// --- Message send unparsing ---

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
            docvec![recv_doc, " ", Document::String(name.to_string())]
        }
        MessageSelector::Binary(op) => {
            let arg = unparse_expression(&arguments[0]);
            docvec![recv_doc, " ", Document::String(op.to_string()), " ", arg]
        }
        MessageSelector::Keyword(parts) => {
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
        [single] => {
            let body = unparse_expression_statement(single);
            group(docvec![
                "[",
                params_doc,
                nest(2, docvec![break_("", ""), body]),
                break_("", ""),
                "]",
            ])
        }

        // Unreachable: the slice patterns above are exhaustive.
        _ => unreachable!(),
    }
}

fn unparse_block_parameter(param: &BlockParameter) -> Document<'static> {
    docvec![":", Document::String(param.name.to_string())]
}

// --- Cascade unparsing ---

fn unparse_cascade_message(msg: &CascadeMessage) -> Document<'static> {
    match &msg.selector {
        MessageSelector::Unary(name) => Document::String(name.to_string()),
        MessageSelector::Binary(op) => {
            let arg = unparse_expression(&msg.arguments[0]);
            docvec![Document::String(op.to_string()), " ", arg]
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

fn unparse_match(value: &Expression, arms: &[MatchArm]) -> Document<'static> {
    let arm_docs: Vec<Document<'static>> = arms.iter().map(unparse_match_arm).collect();
    if arm_docs.len() <= 1 {
        // Single arm: try inline, break if too wide
        let joined = join_docs(arm_docs, "; ");
        group(docvec![unparse_expression(value), " match: [", joined, "]"])
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
            " match: [",
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
        docvec![" when: ", unparse_expression(g)]
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
        Pattern::Variable(id) => unparse_identifier(id),
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
        docs.push(Document::String(unit.to_string()));
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
                inner.push(Document::String(escape_string_quotes(s)));
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
        TypeAnnotation::Simple(id) => Document::String(id.name.to_string()),
        TypeAnnotation::Singleton { name, .. } => {
            docvec!["#", Document::String(name.to_string())]
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
            docvec![Document::String(base.name.to_string()), "<", joined, ">"]
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            docvec![unparse_type_annotation(inner), " | False"]
        }
    }
}

fn unparse_type_annotation_opt(ty: Option<&TypeAnnotation>) -> Document<'static> {
    if let Some(t) = ty {
        docvec![": ", unparse_type_annotation(t)]
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
/// Line comments become `// content`, block comments become `/* content */`.
fn unparse_comment(comment: &Comment) -> Document<'static> {
    match comment.kind {
        CommentKind::Line => {
            if comment.content.is_empty() {
                Document::Str("//")
            } else {
                docvec!["// ", Document::String(comment.content.to_string())]
            }
        }
        CommentKind::Block => {
            docvec!["/* ", Document::String(comment.content.to_string()), " */"]
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Expression, ExpressionStatement, Identifier, Literal, MessageSelector, MethodDefinition,
        ParameterDefinition,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
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
        // Bare " in the AST (from doubled-delimiter unescaping) gets re-escaped as \"
        let expr = Expression::Literal(Literal::String("say \"hello\"".into()), span());
        assert_eq!(
            unparse_expression(&expr).to_pretty_string(),
            "\"say \\\"hello\\\"\""
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

    // --- Assignment ---

    #[test]
    fn assignment_expression() {
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", span()))),
            value: Box::new(Expression::Literal(Literal::Integer(5), span())),
            span: span(),
        };
        assert_eq!(unparse_expression(&expr).to_pretty_string(), "x := 5");
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
}
