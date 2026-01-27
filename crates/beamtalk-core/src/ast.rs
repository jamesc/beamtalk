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
//! ```ignore
//! // Source: x := 3 + 4
//! Module {
//!     expressions: vec![
//!         Expression::Assignment {
//!             target: Identifier { name: "x".into(), span: ... },
//!             value: Box::new(Expression::MessageSend {
//!                 receiver: Box::new(Expression::Literal(Literal::Integer(3))),
//!                 selector: MessageSelector::Binary("+".into()),
//!                 arguments: vec![Expression::Literal(Literal::Integer(4))],
//!                 span: ...
//!             }),
//!             span: ...
//!         }
//!     ],
//!     span: ...
//! }
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

    /// An assignment statement.
    Assignment {
        /// The variable being assigned to.
        target: Identifier,
        /// The value being assigned.
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
            | Self::MessageSend { span, .. }
            | Self::Assignment { span, .. }
            | Self::Return { span, .. }
            | Self::Cascade { span, .. }
            | Self::Parenthesized { span, .. }
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
        let _float = Literal::Float(3.14);
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
}
