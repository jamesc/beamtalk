// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression unparsing: statements, literals, message sends, pipeline re-sugaring, blocks, cascades, match arms and collection literals.
//! Split from `mod.rs`; see the module docs there.

use super::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, CommentAttachment, CommentKind, Document, Expression, ExpressionStatement,
    Identifier, Literal, LiteralCommentSlots, LiteralComments, MapPair, MapPatternKey, MatchArm,
    MessageSelector, Pattern, PipelineSends, Span, StringSegment, TypeAnnotation,
    binary_binding_power, break_, concat, docvec, group, join_docs, join_docs_vec, leaf, line,
    nest, nil, unparse_comment, unparse_comment_attachment_leading, unparse_expect_categories,
    unparse_keyword_part, unparse_type_annotation,
};
use crate::source_analysis::{TokenKind, lex_with_eof, parse_integer_literal};

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

#[cfg(test)]
thread_local! {
    /// Counts [`unparse_expression`] calls on this thread, so tests can
    /// assert that unparse does work linear in the tree size (BT-4531).
    pub(crate) static UNPARSE_EXPRESSION_CALLS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
}

/// Builds a [`Document`] for an [`Expression`].
#[must_use]
#[allow(clippy::too_many_lines)]
pub(crate) fn unparse_expression(expr: &Expression) -> Document<'static> {
    #[cfg(test)]
    UNPARSE_EXPRESSION_CALLS.with(|n| n.set(n.get() + 1));
    match expr {
        Expression::Literal(lit, span) => unparse_literal_at(lit, *span),
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
            if let Some(pipeline) = Pipeline::of(expr) {
                return pipeline.unparse();
            }
            // Flatten ++ chains for width-aware line breaking
            if matches!(selector, MessageSelector::Binary(op) if op.as_str() == "++")
                && !*is_cast
                && let Some(doc) = unparse_concat_chain(expr)
            {
                return doc;
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
        } => {
            // `x |> match: [...]`: the stage's keyword message is `match:`.
            if let Some(pipeline) = Pipeline::of(expr) {
                return pipeline.unparse();
            }
            unparse_match(unparse_expression(value), arms, *exhaustive)
        }
        Expression::MapLiteral { pairs, span } => unparse_map_literal(pairs, *span),
        Expression::ListLiteral {
            elements,
            tail,
            span,
        } => unparse_list_literal(elements, tail.as_deref(), *span),
        Expression::ArrayLiteral { elements, span } => unparse_array_literal(elements, *span),
        Expression::TupleLiteral { elements, span } => unparse_tuple_literal(elements, *span),
        Expression::Primitive {
            name,
            is_quoted,
            is_intrinsic,
            is_inferred,
            no_user_code,
            ..
        } => {
            let directive = if *is_intrinsic {
                "@intrinsic"
            } else {
                "@primitive"
            };
            let pragma = if *is_inferred {
                // Bare `@primitive` — selector inferred from the method.
                docvec![directive]
            } else if *is_quoted {
                docvec![directive, " \"", leaf::string_content(name), "\""]
            } else {
                docvec![directive, " ", leaf::ident(name)]
            };
            if *no_user_code {
                docvec![pragma, " no_user_code"]
            } else {
                pragma
            }
        }
        Expression::StringInterpolation { segments, .. } => unparse_string_interpolation(segments),
        Expression::BinaryConstruction { segments, .. } => {
            unparse_binary_segments(segments, unparse_expression)
        }
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

pub(super) fn unparse_literal(lit: &Literal) -> Document<'static> {
    match lit {
        Literal::Integer(n) => leaf::int_lit(*n),
        Literal::BigInteger(digits) => leaf::big_int_lit(digits),
        Literal::Float(f) => leaf::float_lit(*f),
        Literal::String(s) => {
            // The lexer unescapes doubled delimiters ("" → ") in the AST.
            // We re-escape any bare " using the Beamtalk convention: double it ("").
            docvec!["\"", leaf::string_content(s), "\""]
        }
        Literal::Symbol(s) => {
            // Symbols: #name or #'name with spaces'
            if needs_symbol_quoting(s) {
                docvec!["#'", leaf::quoted_symbol_content(s), "'"]
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

/// Whether symbol `s` must be printed quoted (`#'…'`).
///
/// A symbol is printed bare only when it is an identifier or keyword symbol
/// (`#foo`, `#at:put:`) **and** the lexer reads the bare spelling back as
/// that same single symbol token (BT-5183). Asking the lexer, rather than
/// restating its rules, keeps the two from drifting: the lexer's bare
/// symbols are ASCII-only, so `#'café'` must stay quoted. Binary-operator
/// symbols (`#'+'`), which the lexer also accepts bare, are kept quoted as
/// the canonical spelling.
fn needs_symbol_quoting(s: &str) -> bool {
    let starts_like_identifier = s
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_');
    if !starts_like_identifier {
        return true;
    }
    let mut bare = String::with_capacity(s.len() + 1);
    bare.push('#');
    bare.push_str(s);
    let tokens = lex_with_eof(&bare);
    !matches!(
        tokens.as_slice(),
        [symbol, eof]
            if matches!(symbol.kind(), TokenKind::Symbol(name) if name == s)
                && symbol.trailing_trivia().is_empty()
                && eof.leading_trivia().is_empty()
                && matches!(eof.kind(), TokenKind::Eof)
    )
}

/// Prints the literal at `span`, using its recorded source spelling when it
/// is a number (BT-5183): `16rFF` stays `16rFF` rather than becoming `255`.
pub(super) fn unparse_literal_at(lit: &Literal, span: Span) -> Document<'static> {
    if matches!(
        lit,
        Literal::Integer(_) | Literal::BigInteger(_) | Literal::Float(_)
    ) && let Some(doc) = source_number_spelling(lit, span)
    {
        return doc;
    }
    unparse_literal(lit)
}

/// The source spelling of the number `lit` at `span`, if one was recorded
/// and it reads back as exactly `lit`.
///
/// The spelling is keyed by the number token's span. A negated literal —
/// a match-arm pattern or binary segment written `- 5` or `<<- 16rFF:8>>`,
/// the only places the parser negates a number — holds the negated value,
/// and its span may also cover the `-`, so the one spelling inside the span
/// is used with the sign moved onto it (`-5`), or, where that does not lex
/// back as one token (`-16rFF` reads as `-16 rFF`), kept as a separate `- `. Every candidate is checked by lexing
/// and parsing it back; when none reads back as `lit`, `None` is returned
/// and the value is printed canonically (and the round-trip check in
/// `format_source` then reports the lost spelling rather than writing it).
fn source_number_spelling(lit: &Literal, span: Span) -> Option<Document<'static>> {
    let recorded = LITERAL_COMMENTS.with(|current| {
        let table = current.borrow();
        table.spelling(span).cloned().or_else(|| {
            let mut inside = table.spellings_within(span);
            if inside.len() == 1 {
                inside.pop()
            } else {
                None
            }
        })
    })?;
    if number_reads_back_as(&recorded, lit) {
        return Some(leaf::number_spelling(recorded));
    }
    let negated = negate_literal(lit)?;
    if let Some(positive) = recorded.strip_prefix('-') {
        return number_reads_back_as(positive, lit).then(|| leaf::number_spelling(positive));
    }
    let mut joined = String::from("-");
    joined.push_str(&recorded);
    if number_reads_back_as(&joined, lit) {
        return Some(leaf::number_spelling(joined));
    }
    number_reads_back_as(&recorded, &negated)
        .then(|| docvec!["- ", leaf::number_spelling(recorded)])
}

/// A binary segment's `unit:` value, in its source spelling (`unit:16r8`)
/// when one was recorded (BT-5183). The AST keeps no span for the unit, but
/// it is the last number written in its segment (the value and size come
/// before the `/`), so the segment's last recorded spelling is the unit's
/// when it reads back as the unit.
fn unparse_unit(unit: usize, segment: Span) -> Document<'static> {
    let spelling =
        LITERAL_COMMENTS.with(|current| current.borrow().spellings_within(segment).pop());
    match (spelling, i64::try_from(unit)) {
        (Some(spelling), Ok(value))
            if number_reads_back_as(&spelling, &Literal::Integer(value)) =>
        {
            leaf::number_spelling(spelling)
        }
        _ => leaf::nat_lit(unit),
    }
}

/// `-lit` for a number literal, if it is representable.
fn negate_literal(lit: &Literal) -> Option<Literal> {
    match lit {
        Literal::Integer(_) | Literal::BigInteger(_) | Literal::Float(_) => {
            Some(lit.clone().negated())
        }
        _ => None,
    }
}

/// Whether `spelling` lexes as one number token whose value is exactly `lit`.
fn number_reads_back_as(spelling: &str, lit: &Literal) -> bool {
    let tokens = lex_with_eof(spelling);
    let [token, eof] = tokens.as_slice() else {
        return false;
    };
    if !matches!(eof.kind(), TokenKind::Eof)
        || !token.trailing_trivia().is_empty()
        || !eof.leading_trivia().is_empty()
    {
        return false;
    }
    match (token.kind(), lit) {
        (TokenKind::Integer(text), Literal::Integer(_) | Literal::BigInteger(_)) => {
            text == spelling && parse_integer_literal(text).is_ok_and(|v| v == *lit)
        }
        (TokenKind::Float(text), Literal::Float(value)) => {
            text == spelling
                && text
                    .parse::<f64>()
                    .is_ok_and(|v| v.to_bits() == value.to_bits())
        }
        _ => false,
    }
}

// --- Identifier unparsing ---

fn unparse_identifier(id: &Identifier) -> Document<'static> {
    leaf::ident(&id.name)
}

// --- Message send unparsing ---

/// The operand slot an expression is unparsed into (BT-4059).
#[derive(Debug, Clone, Copy)]
enum OperandSlot<'a> {
    /// The receiver of a unary send.
    UnaryReceiver,
    /// The receiver of a binary send with this operator.
    BinaryReceiver(&'a str),
    /// The argument of a binary send with this operator.
    BinaryArgument(&'a str),
    /// A keyword send's receiver.
    KeywordOperand,
    /// A keyword send's argument.
    KeywordArgument,
}

impl<'a> OperandSlot<'a> {
    fn receiver_of(selector: &'a MessageSelector) -> Self {
        match selector {
            MessageSelector::Unary(_) => Self::UnaryReceiver,
            MessageSelector::Binary(op) => Self::BinaryReceiver(op),
            MessageSelector::Keyword(_) => Self::KeywordOperand,
        }
    }

    fn argument_of(selector: &'a MessageSelector) -> Self {
        match selector {
            // A unary send has no arguments; keyword is the conservative answer.
            MessageSelector::Unary(_) | MessageSelector::Keyword(_) => Self::KeywordArgument,
            MessageSelector::Binary(op) => Self::BinaryArgument(op),
        }
    }

    /// Whether this slot is a send's argument (rather than its receiver).
    const fn is_argument(self) -> bool {
        matches!(self, Self::BinaryArgument(_) | Self::KeywordArgument)
    }

    /// Whether `expr` must be parenthesised to re-parse as this slot's
    /// operand: whether it binds looser than the slot.
    ///
    /// Binary sends follow the parser's own operator table
    /// ([`binary_binding_power`]): Beamtalk binary operators have math
    /// precedence, not Smalltalk's flat left-to-right order, so `(a + b) * c`
    /// keeps its parentheses while `(a * b) + c` and `a + (b * c)` do not.
    ///
    /// A pipeline (ADR 0145) prints as `head |> stage`, and `|>` binds looser
    /// than every message, so as an argument it must be parenthesised
    /// whatever its own shape: `x at: (a |> foo)`. As a receiver it needs
    /// nothing extra: a message after it extends its last stage, which is the
    /// same tree.
    fn needs_parens(self, expr: &Expression) -> bool {
        if self.is_argument() && starts_with_pipeline(expr) {
            return true;
        }
        match OperandShape::of(expr) {
            OperandShape::Tight => false,
            OperandShape::Loose => true,
            OperandShape::Binary(inner) => match self {
                Self::UnaryReceiver => true,
                Self::KeywordOperand | Self::KeywordArgument => false,
                // `inner`'s chain swallows `outer` when `outer` binds its
                // left operand at least as tightly as `inner` binds its right.
                Self::BinaryReceiver(outer) => {
                    match (binary_binding_power(outer), binary_binding_power(inner)) {
                        (Some(outer), Some(inner)) => outer.left >= inner.right,
                        _ => true,
                    }
                }
                // The argument is parsed at `outer`'s right binding power, so
                // it holds `inner` only if `inner` binds at least that tightly.
                Self::BinaryArgument(outer) => {
                    match (binary_binding_power(outer), binary_binding_power(inner)) {
                        (Some(outer), Some(inner)) => inner.left < outer.right,
                        _ => true,
                    }
                }
            },
        }
    }
}

/// How an expression binds as a send operand (BT-4059).
#[derive(Debug, Clone, Copy)]
enum OperandShape<'e> {
    /// Self-delimiting, or a unary send: fits every operand slot.
    Tight,
    /// A binary send with this operator: fits depending on the slot.
    Binary(&'e str),
    /// A keyword or cast send, cascade, assignment, return, match or other
    /// statement-shaped node: needs parentheses in every operand slot.
    Loose,
}

impl<'e> OperandShape<'e> {
    fn of(expr: &'e Expression) -> Self {
        match expr {
            Expression::MessageSend {
                selector: MessageSelector::Unary(_),
                is_cast: false,
                ..
            }
            | Expression::Literal(..)
            | Expression::Identifier(..)
            | Expression::ClassReference { .. }
            | Expression::Super(..)
            | Expression::FieldAccess { .. }
            | Expression::Block(..)
            | Expression::Parenthesized { .. }
            | Expression::MapLiteral { .. }
            | Expression::ListLiteral { .. }
            | Expression::ArrayLiteral { .. }
            | Expression::TupleLiteral { .. }
            | Expression::StringInterpolation { .. }
            | Expression::BinaryConstruction { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => Self::Tight,
            Expression::MessageSend {
                selector: MessageSelector::Binary(op),
                is_cast: false,
                ..
            } => Self::Binary(op),
            Expression::MessageSend { .. }
            | Expression::Cascade { .. }
            | Expression::Assignment { .. }
            | Expression::DestructureAssignment { .. }
            | Expression::Return { .. }
            | Expression::Match { .. }
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. } => Self::Loose,
        }
    }
}

/// Unparses `expr` into `slot`, adding the parentheses a looser node needs —
/// and no others (BT-4059).
///
/// A tree straight from the parser keeps its `Parenthesized` nodes, which are
/// self-delimiting, and its unparenthesised operands already bind tightly
/// enough, so this never adds parentheses to it: the formatter's output is
/// unchanged. A tree whose redundant operand parentheses were peeled
/// ([`crate::ast::normalize`]) gets back exactly the ones its meaning needs.
fn unparse_operand(expr: &Expression, slot: OperandSlot<'_>) -> Document<'static> {
    let doc = unparse_expression(expr);
    if slot.needs_parens(expr) {
        docvec!["(", doc, ")"]
    } else {
        doc
    }
}

fn unparse_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
) -> Document<'static> {
    let recv_doc = unparse_operand(receiver, OperandSlot::receiver_of(selector));
    unparse_message(recv_doc, selector, arguments, is_cast)
}

/// Builds a send's document from its already-built receiver document: the
/// receiver, then the message (`recv_doc` must already carry any
/// parentheses its slot needs).
fn unparse_message(
    recv_doc: Document<'static>,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
) -> Document<'static> {
    unparse_message_laid_out(
        recv_doc,
        selector,
        arguments,
        is_cast,
        BrokenKeywords::UnderReceiver,
    )
}

/// Where a keyword message that breaks puts its keywords.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BrokenKeywords {
    /// Every keyword on its own line, indented under the receiver:
    /// `recv` / `  at: 1` / `  put: 2`.
    UnderReceiver,
    /// The first keyword on the receiver's line and the rest aligned under
    /// it, for a pipeline stage whose "receiver" is its `|>`:
    /// `|> at: 1` / `   put: 2`.
    AfterPipe,
}

/// [`unparse_message`] with the layout of a keyword message that breaks.
fn unparse_message_laid_out(
    recv_doc: Document<'static>,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
    broken_keywords: BrokenKeywords,
) -> Document<'static> {
    let cast_suffix = if is_cast { Document::Str("!") } else { nil() };
    let arg_doc = |arg: &Expression| unparse_operand(arg, OperandSlot::argument_of(selector));

    let msg = match selector {
        MessageSelector::Unary(name) => {
            docvec![recv_doc, " ", leaf::ident(name)]
        }
        MessageSelector::Binary(op) => {
            let arg = arg_doc(&arguments[0]);
            docvec![recv_doc, " ", leaf::ident(op), " ", arg]
        }
        MessageSelector::Keyword(parts) => {
            // Break all keywords to their own indented lines when:
            // - 3+ keyword parts (elm-format style: always break multi-keyword messages), or
            // - any argument renders multi-line (a block with several statements,
            //   a trailing line comment or a multi-line body — all of which
            //   `unparse_block` emits with a hard `line()` — or any other
            //   argument, e.g. a parenthesized 3-keyword send that itself breaks).
            // Otherwise use the compact inline form (1-2 keywords, width-aware).
            //
            // Each argument is unparsed exactly once and its document reused for
            // both the multi-line check and the output. Unparsing it again for
            // the check would double the work at every level of nested block
            // arguments, making unparse exponential in nesting depth (BT-4531).
            let arg_docs: Vec<Document<'static>> = arguments.iter().map(arg_doc).collect();
            let always_break = parts.len() >= 3
                || arg_docs
                    .iter()
                    .any(|doc| doc.to_pretty_string().contains('\n'));
            let mut arg_docs = arg_docs.into_iter();

            if always_break {
                let mut kw_docs: Vec<Document<'static>> = Vec::new();
                for part in parts {
                    kw_docs.push(line());
                    let part_doc = unparse_keyword_part(part);
                    if let Some(arg) = arg_docs.next() {
                        kw_docs.push(docvec![part_doc, " ", arg]);
                    } else {
                        kw_docs.push(part_doc);
                    }
                }
                match broken_keywords {
                    BrokenKeywords::UnderReceiver => docvec![recv_doc, nest(2, concat(kw_docs))],
                    // Drop the first keyword's line break and align the rest
                    // under it, past the `|> `.
                    BrokenKeywords::AfterPipe => {
                        let mut kw_docs = kw_docs.into_iter().skip(1);
                        let first = kw_docs.next().unwrap_or_else(nil);
                        docvec![
                            recv_doc,
                            " ",
                            first,
                            nest(3, concat(kw_docs.collect::<Vec<_>>()))
                        ]
                    }
                }
            } else {
                let mut docs: Vec<Document<'static>> = vec![recv_doc];
                for part in parts {
                    docs.push(Document::Str(" "));
                    docs.push(unparse_keyword_part(part));
                    if let Some(arg) = arg_docs.next() {
                        docs.push(Document::Str(" "));
                        docs.push(arg);
                    }
                }
                concat(docs)
            }
        }
    };

    docvec![msg, cast_suffix]
}

// --- Pipeline re-sugaring (ADR 0145) ---
//
// The parser builds `recv m1 |> m2` as the plain send `(recv m1) m2` and
// records the send each `|>` introduced on the owning definition
// ([`PipelineSends`]). Printing `|>` back needs that record wherever a send is
// unparsed, which is many functions deep. Rather than thread it through every
// one, the definition that owns it installs it for the duration of its own
// unparse ([`PipelineScope`]): `unparse_module_doc` for code outside methods,
// `unparse_method_definition_inner` for a method body. The scope is restored
// on exit, so a method's record never leaks into the module-level code
// around it.

thread_local! {
    /// The `|>` record of the definition being unparsed on this thread.
    static PIPELINE_SENDS: std::cell::RefCell<PipelineSends> =
        std::cell::RefCell::new(PipelineSends::default());
}

/// Installs a definition's `|>` record for unparsing its code, restoring the
/// enclosing definition's record when dropped.
pub(super) struct PipelineScope {
    previous: PipelineSends,
}

impl PipelineScope {
    pub(super) fn enter(sends: &PipelineSends) -> Self {
        let previous = PIPELINE_SENDS.with(|current| current.replace(sends.clone()));
        Self { previous }
    }
}

impl Drop for PipelineScope {
    fn drop(&mut self) {
        let previous = std::mem::take(&mut self.previous);
        PIPELINE_SENDS.with(|current| *current.borrow_mut() = previous);
    }
}

/// Whether `expr` prints as a pipeline (see [`Pipeline::of`]).
fn starts_with_pipeline(expr: &Expression) -> bool {
    Pipeline::of(expr).is_some()
}

/// Whether `expr`, in the definition whose `|>` record is `sends`, prints as
/// a pipeline: `head |> stage …`, which parentheses around it delimit.
///
/// The same recogniser the unparser prints pipelines with
/// ([`Pipeline::recognise`]), for callers that hold the record rather than
/// unparsing: the `unnecessary_parens` lint, which must not report
/// parentheses a pipeline needs.
#[must_use]
pub fn prints_as_pipeline(expr: &Expression, sends: &PipelineSends) -> bool {
    Pipeline::recognise(expr, sends).is_some()
}

/// One message on a pipeline's receiver spine: a send, or a `match:` (whose
/// tree node is [`Expression::Match`], not a send).
#[derive(Clone, Copy)]
enum SpineMessage<'e> {
    Send {
        selector: &'e MessageSelector,
        arguments: &'e [Expression],
        is_cast: bool,
    },
    Match {
        arms: &'e [MatchArm],
        exhaustive: bool,
    },
}

impl<'e> SpineMessage<'e> {
    /// Splits `expr` into its receiver, its span and its message, if it is a
    /// send or a `match:`.
    fn of(expr: &'e Expression) -> Option<(&'e Expression, Span, Self)> {
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast,
                span,
            } => Some((
                receiver,
                *span,
                Self::Send {
                    selector,
                    arguments,
                    is_cast: *is_cast,
                },
            )),
            Expression::Match {
                value,
                arms,
                exhaustive,
                span,
            } => Some((
                value,
                *span,
                Self::Match {
                    arms,
                    exhaustive: *exhaustive,
                },
            )),
            _ => None,
        }
    }

    /// Whether `receiver`, printed bare before this message, would re-parse
    /// as something else: the parentheses it needs in this message's
    /// receiver slot.
    fn receiver_needs_parens(self, receiver: &Expression) -> bool {
        let slot = match self {
            Self::Send { selector, .. } => OperandSlot::receiver_of(selector),
            Self::Match { .. } => OperandSlot::KeywordOperand,
        };
        slot.needs_parens(receiver)
    }

    /// Appends this message to `receiver_doc`, breaking a keyword message
    /// per `layout`.
    fn unparse_onto(
        self,
        receiver_doc: Document<'static>,
        layout: BrokenKeywords,
    ) -> Document<'static> {
        match self {
            Self::Send {
                selector,
                arguments,
                is_cast,
            } => unparse_message_laid_out(receiver_doc, selector, arguments, is_cast, layout),
            Self::Match { arms, exhaustive } => unparse_match(receiver_doc, arms, exhaustive),
        }
    }
}

/// One pipeline stage: the comments written before its `|>`, then its
/// messages in send order (the first is the recorded send).
struct PipelineStage<'e> {
    comments: CommentAttachment,
    messages: Vec<SpineMessage<'e>>,
}

/// A send chain that prints as a pipeline (ADR 0145): a head expression and
/// the stages piped through it, `head |> stage |> stage`.
///
/// Recognised from the tree and a `|>` record by [`Self::recognise`], the one
/// place that decides where a pipeline starts and ends, so printing
/// ([`Self::unparse`]), the "does this need parentheses" checks and the
/// `unnecessary_parens` lint ([`prints_as_pipeline`]) agree.
struct Pipeline<'e> {
    head: &'e Expression,
    stages: Vec<PipelineStage<'e>>,
}

impl<'e> Pipeline<'e> {
    /// The pipeline `expr` prints as under the installed `|>` record (see
    /// [`PipelineScope`]), or `None` if it prints as plain sends.
    fn of(expr: &'e Expression) -> Option<Self> {
        PIPELINE_SENDS.with(|current| Self::recognise(expr, &current.borrow()))
    }

    /// The pipeline `expr` prints as in the definition whose `|>` record is
    /// `sends`, or `None` if it prints as plain sends.
    ///
    /// Walks `expr`'s receiver spine (the send, its receiver, that one's
    /// receiver, …) for as long as each receiver prints bare before its
    /// message, and ends the pipeline at the deepest send written with
    /// `|>`: that send's receiver is the head, and every recorded send
    /// above it starts a stage. The walk stops at a receiver that needs
    /// parentheses, so `(a |> foo: 1) bar` is a send to a parenthesised
    /// pipeline, not a pipeline: its `bar` does not extend the last stage.
    fn recognise(expr: &'e Expression, sends: &PipelineSends) -> Option<Self> {
        // Every send asks, so code without `|>` (nearly all of it) must not
        // pay for a spine walk.
        if sends.is_empty() {
            return None;
        }
        // The spine's messages, outermost first, each with its receiver and
        // whether it was written with `|>`.
        let mut spine: Vec<(&'e Expression, Span, SpineMessage<'e>, bool)> = Vec::new();
        let mut current = expr;
        while let Some((receiver, span, message)) = SpineMessage::of(current) {
            let piped = sends.contains(span);
            if !piped && message.receiver_needs_parens(receiver) {
                break;
            }
            spine.push((receiver, span, message, piped));
            if piped && needs_parens_as_pipeline_head(receiver) {
                break;
            }
            current = receiver;
        }
        let deepest = spine.iter().rposition(|&(.., piped)| piped)?;
        spine.truncate(deepest + 1);
        let head = spine[deepest].0;

        let mut stages: Vec<PipelineStage<'e>> = Vec::new();
        for &(_, span, message, piped) in spine.iter().rev() {
            match stages.last_mut() {
                Some(stage) if !piped => stage.messages.push(message),
                _ => stages.push(PipelineStage {
                    comments: sends.comments(span).cloned().unwrap_or_default(),
                    messages: vec![message],
                }),
            }
        }
        Some(Self { head, stages })
    }

    /// Prints the pipeline: on one line when it fits, otherwise one stage
    /// per line, each starting with `|>` and indented under the head.
    ///
    /// ```text
    /// orders select: [:o | o isPaid] |> collect: [:o | o amount] |> sum
    ///
    /// total := orders select: [:o | o isPaid]  // paid only
    ///   // sum the amounts
    ///   |> collect: [:o | o amount]
    ///   |> inject: 0 into: [:sum :each | sum + each]
    /// ```
    ///
    /// A comment between stages, or a head or stage that spans lines,
    /// always breaks it. A comment is printed before the `|>` it was written
    /// before: an end-of-line comment after the previous stage, then
    /// comments on their own lines.
    fn unparse(self) -> Document<'static> {
        let head_doc = unparse_expression(self.head);
        let head_doc = if needs_parens_as_pipeline_head(self.head) {
            docvec!["(", head_doc, ")"]
        } else {
            head_doc
        };
        let stage_docs: Vec<Document<'static>> = self
            .stages
            .iter()
            .map(|stage| {
                // Only the stage's first message follows the `|>` directly;
                // a later keyword message has a real receiver before it.
                let mut layout = BrokenKeywords::AfterPipe;
                stage
                    .messages
                    .iter()
                    .fold(Document::Str("|>"), |doc, message| {
                        let doc = message.unparse_onto(doc, layout);
                        layout = BrokenKeywords::UnderReceiver;
                        doc
                    })
            })
            .collect();
        let spans_lines = |doc: &Document<'static>| doc.to_pretty_string().contains('\n');
        let always_break =
            self.stages.iter().any(|stage| {
                !stage.comments.leading.is_empty() || stage.comments.trailing.is_some()
            }) || spans_lines(&head_doc)
                || stage_docs.iter().any(spans_lines);

        let mut rest: Vec<Document<'static>> = Vec::new();
        for (stage, stage_doc) in self.stages.iter().zip(stage_docs) {
            if let Some(trail) = &stage.comments.trailing {
                rest.push(docvec!["  ", unparse_comment(trail)]);
            }
            rest.push(if always_break {
                line()
            } else {
                break_("", " ")
            });
            rest.extend(unparse_comment_attachment_leading(&stage.comments));
            rest.push(stage_doc);
        }
        group(docvec![head_doc, nest(2, concat(rest))])
    }
}

/// Whether a pipeline's head must be parenthesised before its first `|>`.
///
/// `|>` binds looser than every message, so a keyword send
/// (`a withX: 1 |> withY: 2`) or a `match:` prints bare. Only a
/// statement-shaped node, which the parser never builds as a head, or a cast
/// send needs them.
fn needs_parens_as_pipeline_head(head: &Expression) -> bool {
    matches!(
        head,
        Expression::Cascade { .. }
            | Expression::Assignment { .. }
            | Expression::DestructureAssignment { .. }
            | Expression::Return { .. }
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::MessageSend { is_cast: true, .. }
    )
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

    // Build the inline version to measure width. The first segment is the
    // chain's leftmost receiver, every other one a `++` argument.
    let seg_docs: Vec<Document<'static>> = segments
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let slot = if i == 0 {
                OperandSlot::BinaryReceiver("++")
            } else {
                OperandSlot::BinaryArgument("++")
            };
            unparse_operand(s, slot)
        })
        .collect();
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
        && op.as_str() == "++"
        && arguments.len() == 1
    {
        collect_concat_segments(receiver, out);
        out.push(&arguments[0]);
        return;
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
                    // Emit an extra blank line if present in source
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
    docvec![
        ":",
        leaf::ident(&param.name),
        unparse_block_param_annotation_opt(param.type_annotation.as_ref()),
    ]
}

/// Renders a block parameter's optional `:: Type` annotation (`:x` alone
/// when absent, `:x :: Integer` when present).
///
/// A top-level `|`/`&`/`\` annotation keeps grouping parens (`:x ::
/// (Integer | String)`): the parser reads this position with
/// `parse_single_type_annotation`, so a bare union would swallow the
/// block's `|` separator (see
/// [`TypeAnnotation::needs_parens_in_block_param`]).
fn unparse_block_param_annotation_opt(ty: Option<&TypeAnnotation>) -> Document<'static> {
    match ty {
        None => nil(),
        Some(t) if t.needs_parens_in_block_param() => {
            docvec![" :: (", unparse_type_annotation(t), ")"]
        }
        Some(t) => docvec![" :: ", unparse_type_annotation(t)],
    }
}

// --- Cascade unparsing ---

fn unparse_cascade_message(msg: &CascadeMessage) -> Document<'static> {
    let arg_doc = |arg: &Expression| unparse_operand(arg, OperandSlot::argument_of(&msg.selector));
    match &msg.selector {
        MessageSelector::Unary(name) => leaf::ident(name),
        MessageSelector::Binary(op) => {
            let arg = arg_doc(&msg.arguments[0]);
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
                    kw_docs.push(arg_doc(&msg.arguments[i]));
                }
            }
            concat(kw_docs)
        }
    }
}

fn unparse_cascade(receiver: &Expression, messages: &[CascadeMessage]) -> Document<'static> {
    // The cascade's target is the folded first send's receiver. A pipeline
    // there must keep its parentheses: `(a |> foo) bar; baz` written without
    // them is `a |> foo bar; baz`, which mixes `|>` and `;` and does not
    // re-parse (ADR 0145). After the operand-paren peel nothing else puts
    // them back, because as a plain unary receiver a pipeline needs none.
    let receiver_doc = match receiver {
        Expression::MessageSend {
            receiver: target,
            selector,
            arguments,
            is_cast,
            ..
        } if starts_with_pipeline(target) => unparse_message(
            docvec!["(", unparse_expression(target), ")"],
            selector,
            arguments,
            *is_cast,
        ),
        _ => unparse_expression(receiver),
    };
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

fn unparse_match(
    value_doc: Document<'static>,
    arms: &[MatchArm],
    exhaustive: bool,
) -> Document<'static> {
    // ADR 0106: `matchExhaustive:` round-trips through unparse just
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
        group(docvec![value_doc, keyword, joined, "]"])
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
        docvec![value_doc, keyword, nest(2, concat(body)), line(), "]"]
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
        Pattern::Literal(lit, span) => unparse_literal_at(lit, *span),
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
        Pattern::Binary { segments, .. } => unparse_binary_segments(segments, unparse_pattern),
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

/// `<<seg, seg, ...>>` for both directions (ADR 0143): a match pattern
/// passes [`unparse_pattern`] for the segment values and a construction
/// passes [`unparse_expression`]; sizes and specifiers are spelled by the one
/// [`unparse_binary_segment`], so a segment reads the same either way.
fn unparse_binary_segments<V>(
    segments: &[BinarySegment<V>],
    unparse_value: fn(&V) -> Document<'static>,
) -> Document<'static> {
    let seg_docs: Vec<Document<'static>> = segments
        .iter()
        .map(|seg| unparse_binary_segment(seg, unparse_value))
        .collect();
    let joined = join_docs(seg_docs, ", ");
    docvec!["<<", joined, ">>"]
}

fn unparse_binary_segment<V>(
    seg: &BinarySegment<V>,
    unparse_value: fn(&V) -> Document<'static>,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = vec![unparse_value(&seg.value)];
    if let Some(size) = &seg.size {
        docs.push(Document::Str(":"));
        docs.push(unparse_expression(size));
    }
    // `unit:N` rides in the `/`-specifier list, exactly as written
    // (BT-4143 mirrors `erlc`: `<<x:2/unit:8>>`), so a parsed segment
    // always unparses back to its own spelling.
    let mut specs: Vec<Document<'static>> = Vec::new();
    if let Some(t) = &seg.segment_type {
        specs.push(Document::Str(match t {
            BinarySegmentType::Integer => "integer",
            BinarySegmentType::Float => "float",
            BinarySegmentType::Binary => "binary",
            BinarySegmentType::Bitstring => "bitstring",
            BinarySegmentType::Utf8 => "utf8",
            BinarySegmentType::Utf16 => "utf16",
            BinarySegmentType::Utf32 => "utf32",
        }));
    }
    if let Some(s) = &seg.signedness {
        specs.push(Document::Str(match s {
            BinarySignedness::Signed => "signed",
            BinarySignedness::Unsigned => "unsigned",
        }));
    }
    if let Some(e) = &seg.endianness {
        specs.push(Document::Str(match e {
            BinaryEndianness::Big => "big",
            BinaryEndianness::Little => "little",
            BinaryEndianness::Native => "native",
        }));
    }
    if let Some(unit) = seg.unit {
        specs.push(docvec!["unit:", unparse_unit(unit, seg.span)]);
    }
    if !specs.is_empty() {
        docs.push(Document::Str("/"));
        docs.push(join_docs(specs, "-"));
    }
    concat(docs)
}

// --- Collection-literal comments (BT-3983) ---
//
// The parser records the comments inside each collection literal on the
// owning definition ([`LiteralComments`], keyed by the literal's span). Like
// the `|>` record, the definition installs it for the duration of its own
// unparse ([`LiteralCommentScope`]).

thread_local! {
    /// The literal-comment record of the definition being unparsed on this
    /// thread.
    static LITERAL_COMMENTS: std::cell::RefCell<LiteralComments> =
        std::cell::RefCell::new(LiteralComments::default());
}

/// Installs a definition's literal-comment record for unparsing its code,
/// restoring the enclosing definition's record when dropped.
pub(super) struct LiteralCommentScope {
    previous: LiteralComments,
}

impl LiteralCommentScope {
    pub(super) fn enter(comments: &LiteralComments) -> Self {
        let previous = LITERAL_COMMENTS.with(|current| current.replace(comments.clone()));
        Self { previous }
    }
}

impl Drop for LiteralCommentScope {
    fn drop(&mut self) {
        let previous = std::mem::take(&mut self.previous);
        LITERAL_COMMENTS.with(|current| *current.borrow_mut() = previous);
    }
}

/// The comments recorded for the literal spanning `span`, if any.
fn literal_comment_slots(span: Span) -> Option<LiteralCommentSlots> {
    LITERAL_COMMENTS.with(|current| current.borrow().get(span).cloned())
}

/// Prints a collection literal that has comments: one item per line, with
/// leading comments above their item, an end-of-line comment after the item's
/// `,`, and the comments before the closing delimiter on their own lines.
///
/// `items` are the printed elements; `tail` is a cons tail (`#(a | t)`),
/// printed on its own `| t` line after the last element.
fn unparse_commented_literal(
    open: &'static str,
    close: &'static str,
    items: Vec<Document<'static>>,
    tail: Option<Document<'static>>,
    slots: &LiteralCommentSlots,
) -> Document<'static> {
    let has_tail = tail.is_some();
    let total = items.len() + usize::from(has_tail);
    let mut body: Vec<Document<'static>> = Vec::new();
    let all = items
        .into_iter()
        .map(|d| (false, d))
        .chain(tail.map(|d| (true, d)));
    for (i, (is_tail, doc)) in all.enumerate() {
        body.push(line());
        let slot = slots.items.get(i);
        if let Some(slot) = slot {
            body.extend(unparse_comment_attachment_leading(slot));
        }
        if is_tail {
            body.push(Document::Str("| "));
        }
        body.push(doc);
        // A comma after every item but the last; the cons tail's `|` line
        // follows the last element with no comma.
        let tail_next = has_tail && i + 2 == total;
        if i + 1 < total && !tail_next {
            body.push(Document::Str(","));
        }
        if let Some(trailing) = slot.and_then(|s| s.trailing.as_ref()) {
            body.push(Document::Str(" "));
            body.push(unparse_comment(trailing));
        }
    }
    for comment in &slots.closing {
        body.push(line());
        body.push(unparse_comment(comment));
    }
    docvec![open, nest(2, concat(body)), line(), close]
}

// --- Map literal unparsing ---

fn unparse_map_literal(pairs: &[MapPair], span: Span) -> Document<'static> {
    if pairs.is_empty() {
        return match literal_comment_slots(span) {
            Some(slots) => unparse_commented_literal("#{", "}", Vec::new(), None, &slots),
            None => Document::Str("#{}"),
        };
    }
    let pair_docs: Vec<Document<'static>> = pairs.iter().map(unparse_map_pair).collect();
    if let Some(slots) = literal_comment_slots(span) {
        return unparse_commented_literal("#{", "}", pair_docs, None, &slots);
    }
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

fn unparse_list_literal(
    elements: &[Expression],
    tail: Option<&Expression>,
    span: Span,
) -> Document<'static> {
    let slots = literal_comment_slots(span);
    if elements.is_empty() && tail.is_none() {
        return match slots {
            Some(slots) => unparse_commented_literal("#(", ")", Vec::new(), None, &slots),
            None => Document::Str("#()"),
        };
    }
    let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_expression).collect();
    if let Some(slots) = slots {
        return unparse_commented_literal(
            "#(",
            ")",
            elem_docs,
            tail.map(unparse_expression),
            &slots,
        );
    }
    let joined = join_docs(elem_docs, ", ");
    if let Some(t) = tail {
        docvec!["#(", joined, " | ", unparse_expression(t), ")"]
    } else {
        docvec!["#(", joined, ")"]
    }
}

// --- Array literal unparsing ---

fn unparse_array_literal(elements: &[Expression], span: Span) -> Document<'static> {
    let slots = literal_comment_slots(span);
    if elements.is_empty() {
        return match slots {
            Some(slots) => unparse_commented_literal("#[", "]", Vec::new(), None, &slots),
            None => Document::Str("#[]"),
        };
    }
    let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_expression).collect();
    if let Some(slots) = slots {
        return unparse_commented_literal("#[", "]", elem_docs, None, &slots);
    }
    let joined = join_docs(elem_docs, ", ");
    docvec!["#[", joined, "]"]
}

// --- Tuple literal unparsing ---

/// `{e1, e2}` (ADR 0136). A trailing comma in the source is normalised away.
fn unparse_tuple_literal(elements: &[Expression], span: Span) -> Document<'static> {
    let slots = literal_comment_slots(span);
    if elements.is_empty() {
        return match slots {
            Some(slots) => unparse_commented_literal("{", "}", Vec::new(), None, &slots),
            None => Document::Str("{}"),
        };
    }
    let elem_docs: Vec<Document<'static>> = elements.iter().map(unparse_expression).collect();
    if let Some(slots) = slots {
        return unparse_commented_literal("{", "}", elem_docs, None, &slots);
    }
    let joined = join_docs(elem_docs, ", ");
    docvec!["{", joined, "}"]
}

/// Whether `expr` unparses to text that begins with a bare `{` — a tuple
/// literal (or tuple destructure pattern) in leftmost position.
///
/// Inside a string interpolation such an expression must be padded
/// (`"{ {a, b} }"`), because the lexer rejects an opening `"{{"` as a
/// mistaken literal-brace escape (ADR 0023, ADR 0136).
fn starts_with_open_brace(expr: &Expression) -> bool {
    match expr {
        Expression::TupleLiteral { .. } => true,
        Expression::MessageSend { receiver, .. }
        | Expression::Cascade { receiver, .. }
        | Expression::FieldAccess { receiver, .. } => starts_with_open_brace(receiver),
        Expression::Match { value, .. } => starts_with_open_brace(value),
        Expression::Assignment { target, .. } => starts_with_open_brace(target),
        Expression::DestructureAssignment { pattern, .. } => {
            matches!(pattern, Pattern::Tuple { .. })
        }
        _ => false,
    }
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
                if starts_with_open_brace(expr) {
                    inner.push(Document::Str("{ "));
                    inner.push(unparse_expression(expr));
                    inner.push(Document::Str(" }"));
                } else {
                    inner.push(Document::Str("{"));
                    inner.push(unparse_expression(expr));
                    inner.push(Document::Str("}"));
                }
            }
        }
    }
    docvec!["\"", concat(inner), "\""]
}
