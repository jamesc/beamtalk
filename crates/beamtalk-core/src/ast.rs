// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Abstract Syntax Tree (AST) definitions for Beamtalk.
//!
//! The AST represents the structure of a Beamtalk program after parsing.
//! Every AST node carries a [`Span`] for error reporting and IDE features.
//!
//! # Design Philosophy
//!
//! This AST is designed for IDE tooling first, batch compilation second:
//!
//! - **All nodes have spans** - Required for hover, go-to-definition, diagnostics
//! - **Error recovery** - Parser can produce incomplete ASTs with [`Expression::Error`]
//! - **Comment preservation** - Comments attached to nodes for formatting tools
//! - **Rich enough for roundtrip** - AST → source should preserve style
//!
//! # Message Sending
//!
//! Beamtalk follows Smalltalk's message precedence:
//!
//! 1. **Unary messages**: `object message` (highest precedence)
//! 2. **Binary messages**: `3 + 4` (standard math precedence, left-to-right)
//! 3. **Keyword messages**: `array at: 1 put: 'x'` (lowest precedence)
//!
//! # Example
//!
//! ```
//! use beamtalk_core::ast::*;
//! use beamtalk_core::parse::Span;
//!
//! // Source: x := 3 + 4
//! let module = Module {
//!     expressions: vec![
//!         Expression::Assignment {
//!             target: Box::new(Expression::Identifier(Identifier {
//!                 name: "x".into(),
//!                 span: Span::new(0, 1),
//!             })),
//!             value: Box::new(Expression::MessageSend {
//!                 receiver: Box::new(Expression::Literal(
//!                     Literal::Integer(3),
//!                     Span::new(5, 6)
//!                 )),
//!                 selector: MessageSelector::Binary("+".into()),
//!                 arguments: vec![Expression::Literal(
//!                     Literal::Integer(4),
//!                     Span::new(9, 10)
//!                 )],
//!                 span: Span::new(5, 10),
//!             }),
//!             span: Span::new(0, 10),
//!         }
//!     ],
//!     span: Span::new(0, 10),
//!     leading_comments: Vec::new(),
//! };
//! # assert_eq!(module.expressions.len(), 1);
//! ```

use crate::parse::Span;
use ecow::EcoString;

/// Top-level container for a Beamtalk module.
///
/// A module consists of a sequence of expressions, typically method definitions
/// or top-level statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// The expressions in this module.
    pub expressions: Vec<Expression>,
    /// Source location spanning the entire module.
    pub span: Span,
    /// Leading comments (module documentation).
    pub leading_comments: Vec<Comment>,
}

impl Module {
    /// Creates a new module with the given expressions and span.
    #[must_use]
    pub fn new(expressions: Vec<Expression>, span: Span) -> Self {
        Self {
            expressions,
            span,
            leading_comments: Vec::new(),
        }
    }

    /// Creates a new module with comments.
    #[must_use]
    pub fn with_comments(
        expressions: Vec<Expression>,
        span: Span,
        leading_comments: Vec<Comment>,
    ) -> Self {
        Self {
            expressions,
            span,
            leading_comments,
        }
    }
}

/// A comment in the source code.
///
/// Comments are preserved for formatting and documentation tools.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    /// The comment text (without delimiters).
    pub content: EcoString,
    /// Source location of the comment.
    pub span: Span,
    /// Whether this is a block comment (`/* */`) or line comment (`//`).
    pub kind: CommentKind,
}

/// The kind of comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CommentKind {
    /// A line comment (`// text`).
    Line,
    /// A block comment (`/* text */`).
    Block,
}

/// A Beamtalk expression.
///
/// Expressions are the fundamental building blocks of Beamtalk programs.
/// Everything is an expression, including assignments and returns.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// A literal value.
    Literal(Literal, Span),

    /// An identifier (variable or class name).
    Identifier(Identifier),

    /// Field access (`self.value`).
    ///
    /// This is direct field access within an actor, not a message send.
    /// Compiles to direct map access in BEAM.
    FieldAccess {
        /// The receiver (typically `self`).
        receiver: Box<Expression>,
        /// The field name being accessed.
        field: Identifier,
        /// Source location of the entire field access.
        span: Span,
    },

    /// A message send (method call).
    MessageSend {
        /// The receiver of the message.
        receiver: Box<Expression>,
        /// The message selector (method name).
        selector: MessageSelector,
        /// Arguments to the message.
        arguments: Vec<Expression>,
        /// Source location of the entire message send.
        span: Span,
    },

    /// A block (closure/lambda).
    Block(Block),

    /// An assignment statement (`x := value`).
    Assignment {
        /// The target being assigned to (identifier or field access).
        target: Box<Expression>,
        /// The value being assigned.
        value: Box<Expression>,
        /// Source location of the entire assignment.
        span: Span,
    },

    /// A compound assignment (`self.value += 1`, `x *= 2`).
    CompoundAssignment {
        /// The target being modified.
        target: Box<Expression>,
        /// The compound operator.
        operator: CompoundOperator,
        /// The value (right-hand side).
        value: Box<Expression>,
        /// Source location of the entire assignment.
        span: Span,
    },

    /// A return statement.
    Return {
        /// The value being returned.
        value: Box<Expression>,
        /// Source location of the return statement.
        span: Span,
    },

    /// A cascade (multiple messages to the same receiver).
    ///
    /// Syntax: `receiver message1; message2; message3`
    Cascade {
        /// The receiver (evaluated once).
        receiver: Box<Expression>,
        /// The sequence of messages to send.
        messages: Vec<CascadeMessage>,
        /// Source location of the entire cascade.
        span: Span,
    },

    /// A parenthesized expression (for precedence override).
    Parenthesized {
        /// The inner expression.
        expression: Box<Expression>,
        /// Source location including parentheses.
        span: Span,
    },

    /// A pipe expression (`data |> transform`).
    ///
    /// Pipes pass the left side as the first argument to the right side.
    Pipe {
        /// The value being piped.
        value: Box<Expression>,
        /// The pipe operator (sync or async).
        operator: PipeOperator,
        /// The function/message to pipe into.
        target: Box<Expression>,
        /// Source location of the entire pipe expression.
        span: Span,
    },

    /// An await expression (`future await`).
    ///
    /// Blocks until a future is resolved or rejected.
    /// Syntax: `result await` or `(counter increment) await`
    Await {
        /// The future expression to await.
        future: Box<Expression>,
        /// Source location of the await expression.
        span: Span,
    },

    /// A pattern match expression.
    ///
    /// Example: `value match: [{#ok, x} -> x; {#error, e} -> nil]`
    Match {
        /// The value being matched against.
        value: Box<Expression>,
        /// The match arms.
        arms: Vec<MatchArm>,
        /// Source location of the entire match.
        span: Span,
    },

    /// An error node for unparseable code.
    ///
    /// This allows the parser to recover from errors and continue.
    Error {
        /// A description of what went wrong.
        message: EcoString,
        /// Source location of the erroneous code.
        span: Span,
    },
}

impl Expression {
    /// Returns the span of this expression.
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::Literal(_, span)
            | Self::FieldAccess { span, .. }
            | Self::MessageSend { span, .. }
            | Self::Assignment { span, .. }
            | Self::CompoundAssignment { span, .. }
            | Self::Return { span, .. }
            | Self::Cascade { span, .. }
            | Self::Parenthesized { span, .. }
            | Self::Pipe { span, .. }
            | Self::Await { span, .. }
            | Self::Match { span, .. }
            | Self::Error { span, .. } => *span,
            Self::Identifier(id) => id.span,
            Self::Block(block) => block.span,
        }
    }

    /// Returns true if this expression is an error node.
    #[must_use]
    pub const fn is_error(&self) -> bool {
        matches!(self, Self::Error { .. })
    }
}

/// Compound assignment operators (`+=`, `-=`, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompoundOperator {
    /// Addition assignment (`+=`).
    Add,
    /// Subtraction assignment (`-=`).
    Subtract,
    /// Multiplication assignment (`*=`).
    Multiply,
    /// Division assignment (`/=`).
    Divide,
    /// Remainder assignment (`%=`).
    Remainder,
}

impl CompoundOperator {
    /// Returns the string representation of the operator.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Add => "+=",
            Self::Subtract => "-=",
            Self::Multiply => "*=",
            Self::Divide => "/=",
            Self::Remainder => "%=",
        }
    }

    /// Returns the corresponding binary operator.
    #[must_use]
    pub const fn to_binary(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Remainder => "%",
        }
    }
}

/// Pipe operators for data flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PipeOperator {
    /// Synchronous pipe (`|>`).
    Sync,
    /// Asynchronous pipe (`|>>`).
    Async,
}

impl PipeOperator {
    /// Returns the string representation of the operator.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Sync => "|>",
            Self::Async => "|>>",
        }
    }
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// An integer literal.
    ///
    /// Examples: `42`, `16rFF`, `2r1010`
    Integer(i64),

    /// A floating-point literal.
    ///
    /// Examples: `3.14`, `1.5e10`, `2.0e-5`
    Float(f64),

    /// A string literal (single-quoted in source).
    ///
    /// Example: `'hello, world'`
    /// Escapes: `''` for a single quote
    String(EcoString),

    /// A symbol literal (compile-time constant).
    ///
    /// Example: `#symbol`, `#'symbol with spaces'`
    Symbol(EcoString),

    /// An array literal.
    ///
    /// Example: `#(1 2 'three')`
    Array(Vec<Literal>),

    /// A character literal.
    ///
    /// Example: `$a`, `$\n`
    Character(char),
}

/// An identifier (variable or class name).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    /// The name of the identifier.
    pub name: EcoString,
    /// Source location.
    pub span: Span,
}

impl Identifier {
    /// Creates a new identifier.
    #[must_use]
    pub fn new(name: impl Into<EcoString>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// A message selector (method name).
///
/// Beamtalk has three types of message selectors corresponding
/// to different syntactic forms and precedence levels.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MessageSelector {
    /// A unary message (no arguments).
    ///
    /// Example: `object size`
    /// Selector: `size`
    Unary(EcoString),

    /// A binary message (one argument, operator syntax).
    ///
    /// Example: `3 + 4`
    /// Selector: `+`
    Binary(EcoString),

    /// A keyword message (one or more named arguments).
    ///
    /// Example: `array at: 1 put: 'x'`
    /// Selector: `at:put:` (concatenated keywords)
    Keyword(Vec<KeywordPart>),
}

impl MessageSelector {
    /// Returns the full selector name.
    ///
    /// For keyword messages, this concatenates all keyword parts.
    /// For unary and binary messages, returns the operator/name directly.
    #[must_use]
    pub fn name(&self) -> EcoString {
        match self {
            Self::Unary(name) | Self::Binary(name) => name.clone(),
            Self::Keyword(parts) => {
                let mut result = String::new();
                for part in parts {
                    result.push_str(&part.keyword);
                }
                result.into()
            }
        }
    }

    /// Returns the number of arguments this selector expects.
    #[must_use]
    pub fn arity(&self) -> usize {
        match self {
            Self::Unary(_) => 0,
            Self::Binary(_) => 1,
            Self::Keyword(parts) => parts.len(),
        }
    }
}

/// A keyword part in a keyword message.
///
/// Example: In `at: 1 put: 'x'`, there are two keyword parts:
/// - `KeywordPart { keyword: "at:", span: ... }`
/// - `KeywordPart { keyword: "put:", span: ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeywordPart {
    /// The keyword (including trailing colon).
    pub keyword: EcoString,
    /// Source location of this keyword.
    pub span: Span,
}

impl KeywordPart {
    /// Creates a new keyword part.
    #[must_use]
    pub fn new(keyword: impl Into<EcoString>, span: Span) -> Self {
        Self {
            keyword: keyword.into(),
            span,
        }
    }
}

/// A block (closure/lambda).
///
/// Example: `[:x :y | x + y]`
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// Block parameters.
    pub parameters: Vec<BlockParameter>,
    /// The expressions in the block body.
    pub body: Vec<Expression>,
    /// Source location of the entire block (including brackets).
    pub span: Span,
}

impl Block {
    /// Creates a new block.
    #[must_use]
    pub fn new(parameters: Vec<BlockParameter>, body: Vec<Expression>, span: Span) -> Self {
        Self {
            parameters,
            body,
            span,
        }
    }

    /// Returns the number of parameters.
    #[must_use]
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }
}

/// A block parameter.
///
/// Example: In `[:x :y | ...]`, `x` and `y` are block parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockParameter {
    /// The parameter name.
    pub name: EcoString,
    /// Source location.
    pub span: Span,
}

impl BlockParameter {
    /// Creates a new block parameter.
    #[must_use]
    pub fn new(name: impl Into<EcoString>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// A match arm in a match expression.
///
/// Example: `{#ok, value} -> value`
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    /// The pattern to match against.
    pub pattern: Pattern,
    /// Optional guard expression.
    pub guard: Option<Expression>,
    /// The expression to evaluate if the pattern matches.
    pub body: Expression,
    /// Source location of this match arm.
    pub span: Span,
}

impl MatchArm {
    /// Creates a new match arm without a guard.
    #[must_use]
    pub fn new(pattern: Pattern, body: Expression, span: Span) -> Self {
        Self {
            pattern,
            guard: None,
            body,
            span,
        }
    }

    /// Creates a new match arm with a guard.
    #[must_use]
    pub fn with_guard(pattern: Pattern, guard: Expression, body: Expression, span: Span) -> Self {
        Self {
            pattern,
            guard: Some(guard),
            body,
            span,
        }
    }
}

/// A pattern for pattern matching.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern (`_`) - matches anything.
    Wildcard(Span),

    /// Literal pattern - matches exact value.
    Literal(Literal, Span),

    /// Variable binding pattern - binds to a name.
    Variable(Identifier),

    /// Tuple pattern - matches tuple structure.
    ///
    /// Example: `{#ok, value}`
    Tuple {
        /// Elements of the tuple pattern.
        elements: Vec<Pattern>,
        /// Source location.
        span: Span,
    },

    /// List pattern - matches list structure.
    ///
    /// Example: `[head | tail]`
    List {
        /// Head elements.
        elements: Vec<Pattern>,
        /// Optional tail pattern.
        tail: Option<Box<Pattern>>,
        /// Source location.
        span: Span,
    },

    /// Binary pattern for binary matching.
    ///
    /// Example: `<<version:8, length:16/big>>`
    Binary {
        /// Segments of the binary pattern.
        segments: Vec<BinarySegment>,
        /// Source location.
        span: Span,
    },
}

impl Pattern {
    /// Returns the span of this pattern.
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::Variable(id) => id.span,
            Self::Wildcard(span)
            | Self::Literal(_, span)
            | Self::Tuple { span, .. }
            | Self::List { span, .. }
            | Self::Binary { span, .. } => *span,
        }
    }
}

/// A segment in a binary pattern.
///
/// Example: In `<<version:8, data/binary>>`, `version:8` and `data/binary` are segments.
#[derive(Debug, Clone, PartialEq)]
pub struct BinarySegment {
    /// The value or variable.
    pub value: Pattern,
    /// Size specifier (e.g., `8`, `16`).
    pub size: Option<Box<Expression>>,
    /// Type specifier (e.g., `binary`, `integer`, `float`).
    pub segment_type: Option<BinarySegmentType>,
    /// Signedness (for integer types).
    pub signedness: Option<BinarySignedness>,
    /// Endianness.
    pub endianness: Option<BinaryEndianness>,
    /// Unit size.
    pub unit: Option<usize>,
    /// Source location.
    pub span: Span,
}

/// Type specifier for binary segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinarySegmentType {
    /// Integer value.
    Integer,
    /// Float value.
    Float,
    /// Binary/bytes.
    Binary,
    /// UTF-8 string.
    Utf8,
}

/// Signedness for integer binary segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinarySignedness {
    /// Signed integer.
    Signed,
    /// Unsigned integer.
    Unsigned,
}

/// Endianness for binary segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryEndianness {
    /// Big-endian.
    Big,
    /// Little-endian.
    Little,
    /// Native endianness.
    Native,
}

/// A message in a cascade.
///
/// Cascades send multiple messages to the same receiver.
/// Example: `Transcript show: 'Hello'; cr; show: 'World'`
#[derive(Debug, Clone, PartialEq)]
pub struct CascadeMessage {
    /// The message selector.
    pub selector: MessageSelector,
    /// Arguments to the message.
    pub arguments: Vec<Expression>,
    /// Source location of this message in the cascade.
    pub span: Span,
}

impl CascadeMessage {
    /// Creates a new cascade message.
    #[must_use]
    pub fn new(selector: MessageSelector, arguments: Vec<Expression>, span: Span) -> Self {
        Self {
            selector,
            arguments,
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_creation() {
        let span = Span::new(0, 10);
        let module = Module::new(Vec::new(), span);
        assert!(module.expressions.is_empty());
        assert_eq!(module.span, span);
        assert!(module.leading_comments.is_empty());
    }

    #[test]
    fn identifier_creation() {
        let id = Identifier::new("myVar", Span::new(0, 5));
        assert_eq!(id.name, "myVar");
        assert_eq!(id.span, Span::new(0, 5));
    }

    #[test]
    fn message_selector_unary() {
        let selector = MessageSelector::Unary("size".into());
        assert_eq!(selector.name(), "size");
        assert_eq!(selector.arity(), 0);
    }

    #[test]
    fn message_selector_binary() {
        let selector = MessageSelector::Binary("+".into());
        assert_eq!(selector.name(), "+");
        assert_eq!(selector.arity(), 1);
    }

    #[test]
    fn message_selector_keyword() {
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(0, 3)),
            KeywordPart::new("put:", Span::new(5, 9)),
        ]);
        assert_eq!(selector.name(), "at:put:");
        assert_eq!(selector.arity(), 2);
    }

    #[test]
    fn block_creation() {
        let block = Block::new(
            vec![
                BlockParameter::new("x", Span::new(1, 2)),
                BlockParameter::new("y", Span::new(4, 5)),
            ],
            Vec::new(),
            Span::new(0, 10),
        );
        assert_eq!(block.arity(), 2);
        assert_eq!(block.parameters[0].name, "x");
        assert_eq!(block.parameters[1].name, "y");
    }

    #[test]
    fn expression_span() {
        let span = Span::new(10, 20);
        let expr = Expression::Literal(Literal::Integer(42), span);
        assert_eq!(expr.span(), span);

        let id = Identifier::new("x", span);
        let expr = Expression::Identifier(id);
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expression_is_error() {
        let error = Expression::Error {
            message: "parse error".into(),
            span: Span::new(0, 5),
        };
        assert!(error.is_error());

        let literal = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        assert!(!literal.is_error());
    }

    #[test]
    fn literal_variants() {
        let _int = Literal::Integer(42);
        let _float = Literal::Float(2.5);
        let _string = Literal::String("hello".into());
        let _symbol = Literal::Symbol("symbol".into());
        let _char = Literal::Character('a');
        let _array = Literal::Array(vec![Literal::Integer(1), Literal::Integer(2)]);
    }

    #[test]
    fn cascade_message_creation() {
        let msg = CascadeMessage::new(
            MessageSelector::Unary("cr".into()),
            Vec::new(),
            Span::new(0, 2),
        );
        assert_eq!(msg.selector.name(), "cr");
        assert!(msg.arguments.is_empty());
    }

    #[test]
    fn field_access_expression() {
        let receiver = Box::new(Expression::Identifier(Identifier::new(
            "self",
            Span::new(0, 4),
        )));
        let field = Identifier::new("value", Span::new(5, 10));
        let expr = Expression::FieldAccess {
            receiver,
            field,
            span: Span::new(0, 10),
        };
        assert_eq!(expr.span(), Span::new(0, 10));
    }

    #[test]
    fn compound_assignment() {
        let target = Box::new(Expression::Identifier(Identifier::new(
            "count",
            Span::new(0, 5),
        )));
        let value = Box::new(Expression::Literal(Literal::Integer(1), Span::new(9, 10)));
        let expr = Expression::CompoundAssignment {
            target,
            operator: CompoundOperator::Add,
            value,
            span: Span::new(0, 10),
        };
        assert_eq!(expr.span(), Span::new(0, 10));
        assert_eq!(CompoundOperator::Add.as_str(), "+=");
        assert_eq!(CompoundOperator::Add.to_binary(), "+");
    }

    #[test]
    fn compound_operators() {
        assert_eq!(CompoundOperator::Add.as_str(), "+=");
        assert_eq!(CompoundOperator::Subtract.as_str(), "-=");
        assert_eq!(CompoundOperator::Multiply.as_str(), "*=");
        assert_eq!(CompoundOperator::Divide.as_str(), "/=");
        assert_eq!(CompoundOperator::Remainder.as_str(), "%=");
    }

    #[test]
    fn pipe_operators() {
        assert_eq!(PipeOperator::Sync.as_str(), "|>");
        assert_eq!(PipeOperator::Async.as_str(), "|>>");
    }

    #[test]
    fn pipe_expression() {
        let value = Box::new(Expression::Identifier(Identifier::new(
            "data",
            Span::new(0, 4),
        )));
        let target = Box::new(Expression::Identifier(Identifier::new(
            "transform",
            Span::new(8, 17),
        )));
        let expr = Expression::Pipe {
            value,
            operator: PipeOperator::Sync,
            target,
            span: Span::new(0, 17),
        };
        assert_eq!(expr.span(), Span::new(0, 17));
    }

    #[test]
    fn pattern_wildcard() {
        let pattern = Pattern::Wildcard(Span::new(0, 1));
        assert_eq!(pattern.span(), Span::new(0, 1));
    }

    #[test]
    fn pattern_variable() {
        let pattern = Pattern::Variable(Identifier::new("x", Span::new(0, 1)));
        assert_eq!(pattern.span(), Span::new(0, 1));
    }

    #[test]
    fn pattern_tuple() {
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Literal(Literal::Symbol("ok".into()), Span::new(1, 4)),
                Pattern::Variable(Identifier::new("value", Span::new(6, 11))),
            ],
            span: Span::new(0, 12),
        };
        assert_eq!(pattern.span(), Span::new(0, 12));
    }

    #[test]
    fn match_arm_creation() {
        let pattern = Pattern::Wildcard(Span::new(0, 1));
        let body = Expression::Literal(Literal::Integer(0), Span::new(5, 6));
        let arm = MatchArm::new(pattern, body, Span::new(0, 6));
        assert!(arm.guard.is_none());

        let pattern = Pattern::Variable(Identifier::new("x", Span::new(0, 1)));
        let guard = Expression::Identifier(Identifier::new("x", Span::new(7, 8)));
        let body = Expression::Identifier(Identifier::new("x", Span::new(12, 13)));
        let arm = MatchArm::with_guard(pattern, guard, body, Span::new(0, 13));
        assert!(arm.guard.is_some());
    }

    #[test]
    fn match_expression() {
        let value = Box::new(Expression::Identifier(Identifier::new(
            "result",
            Span::new(0, 6),
        )));
        let arm = MatchArm::new(
            Pattern::Wildcard(Span::new(14, 15)),
            Expression::Literal(Literal::Integer(0), Span::new(19, 20)),
            Span::new(14, 20),
        );
        let expr = Expression::Match {
            value,
            arms: vec![arm],
            span: Span::new(0, 21),
        };
        assert_eq!(expr.span(), Span::new(0, 21));
    }
}
