// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Abstract Syntax Tree (AST) definitions for Beamtalk.
//!
//! **DDD Context:** Source Analysis
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
//! use beamtalk_core::source_analysis::Span;
//!
//! // Source: x := 3 + 4
//! let module = Module {
//!     classes: Vec::new(),
//!     method_definitions: Vec::new(),
//!     protocols: Vec::new(),
//!     expressions: vec![
//!         ExpressionStatement::bare(Expression::Assignment {
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
//!                 is_cast: false,
//!                 span: Span::new(5, 10),
//!             }),
//!             type_annotation: None,
//!             span: Span::new(0, 10),
//!         })
//!     ],
//!     span: Span::new(0, 10),
//!     file_leading_comments: Vec::new(),
//!     file_trailing_comments: Vec::new(),
//! };
//! # assert_eq!(module.expressions.len(), 1);
//! ```
//!
//! # Builder Pattern Convention
//!
//! AST nodes use `new()` for basic construction and `with_*()` for variant
//! constructors (e.g., `StateDeclaration::with_type()`, `Module::with_classes()`).
//! Prefer these constructors over manual struct initialization.
//!
//! # Module Organization
//!
//! The AST is split into submodules by category:
//!
//! - `expression` — Expression nodes, literals, identifiers, messages, types
//! - `pattern` — Pattern matching and destructuring
//! - `class` — Class, protocol, and state declarations
//! - `method` — Method definitions and parameters
//! - `well_known` — Compiler-recognised selectors ([`WellKnownSelector`])

mod class;
mod expression;
mod method;
mod pattern;
pub(crate) mod visitor;
pub mod well_known;

// Re-export all public types so `use crate::ast::Foo` continues to work.
pub use class::*;
pub use expression::*;
pub use method::*;
pub use pattern::*;
pub use well_known::*;

use crate::source_analysis::Span;
use ecow::EcoString;

/// Top-level container for a Beamtalk module (Aggregate Root).
///
/// A module consists of class definitions and/or top-level expressions.
/// Class definitions are the primary structure for actor-based code.
///
/// # DDD: Aggregate Root for Source Analysis Context
///
/// `Module` is the aggregate root for the Source Analysis bounded context.
/// It owns all parsed AST nodes and enforces invariants across the parse tree.
///
/// # Invariants
///
/// The following invariants should hold for a valid module:
///
/// 1. **Span coverage**: The module's span must encompass all child spans
/// 2. **No span overlap**: Expressions and classes should have non-overlapping spans
/// 3. **Span containment**: All child node spans must be within the module span
///
/// These invariants ensure consistent source mapping for error reporting and IDE features.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Class definitions in this module.
    pub classes: Vec<ClassDefinition>,
    /// Standalone method definitions (Tonel-style `Class >> method => body`).
    pub method_definitions: Vec<StandaloneMethodDefinition>,
    /// Protocol definitions (ADR 0068, Phase 2a).
    pub protocols: Vec<ProtocolDefinition>,
    /// Top-level expressions (scripts, REPL input).
    pub expressions: Vec<ExpressionStatement>,
    /// Source location spanning the entire module.
    pub span: Span,
    /// File-level leading comments (ADR 0044).
    ///
    /// Comments at the very start of the file, before any expressions or class definitions.
    pub file_leading_comments: Vec<Comment>,
    /// File-level trailing comments (ADR 0044).
    ///
    /// Comments at the end of the file, after the last class/method/expression.
    pub file_trailing_comments: Vec<Comment>,
}

impl Module {
    /// Creates a new module with the given expressions and span.
    #[must_use]
    pub fn new(expressions: Vec<ExpressionStatement>, span: Span) -> Self {
        Self {
            classes: Vec::new(),
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions,
            span,
            file_leading_comments: Vec::new(),
            file_trailing_comments: Vec::new(),
        }
    }

    /// Creates a new module with class definitions.
    #[must_use]
    pub fn with_classes(classes: Vec<ClassDefinition>, span: Span) -> Self {
        Self {
            classes,
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: Vec::new(),
            span,
            file_leading_comments: Vec::new(),
            file_trailing_comments: Vec::new(),
        }
    }

    /// Creates a new module with file-level leading comments.
    #[must_use]
    pub fn with_comments(
        expressions: Vec<ExpressionStatement>,
        span: Span,
        file_leading_comments: Vec<Comment>,
    ) -> Self {
        Self {
            classes: Vec::new(),
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions,
            span,
            file_leading_comments,
            file_trailing_comments: Vec::new(),
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
    /// Whether this comment was preceded by a blank line in the source.
    pub preceding_blank_line: bool,
}

impl Comment {
    /// Creates a line comment (`// text`).
    #[must_use]
    pub fn line(content: impl Into<EcoString>, span: Span) -> Self {
        Self {
            content: content.into(),
            span,
            kind: CommentKind::Line,
            preceding_blank_line: false,
        }
    }

    /// Creates a block comment (`/* text */`).
    #[must_use]
    pub fn block(content: impl Into<EcoString>, span: Span) -> Self {
        Self {
            content: content.into(),
            span,
            kind: CommentKind::Block,
            preceding_blank_line: false,
        }
    }
}

/// The kind of comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CommentKind {
    /// A line comment (`// text`).
    Line,
    /// A block comment (`/* text */`).
    Block,
}

/// Comments attached to an AST node (ADR 0044).
///
/// Every comment belongs to exactly one AST node, either as a leading comment
/// (appears before the node in source) or a trailing comment (appears at the
/// end of the same line as the node).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CommentAttachment {
    /// Comments appearing on lines immediately before this node.
    /// Ordered top-to-bottom as they appear in source.
    pub leading: Vec<Comment>,
    /// A single end-of-line comment on the same line as this node.
    pub trailing: Option<Comment>,
}

impl CommentAttachment {
    /// Returns true if no comments are attached.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.leading.is_empty() && self.trailing.is_none()
    }
}

/// An expression in a statement position, with optional surrounding comments (ADR 0044).
///
/// Wraps an [`Expression`] with a [`CommentAttachment`] for preserving comments
/// between statements. Statement-position fields (method bodies, block bodies,
/// module expressions) use `Vec<ExpressionStatement>` (BT-974).
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    /// Comments attached to this statement.
    pub comments: CommentAttachment,
    /// The expression.
    pub expression: Expression,
    /// Whether a blank line preceded this statement in the source (BT-987).
    ///
    /// Set by the parser when 2+ newlines appear before the statement.
    /// The unparser emits an extra blank line before such statements.
    /// Consecutive blank lines are collapsed to a single one.
    pub preceding_blank_line: bool,
}

impl ExpressionStatement {
    /// Creates an expression statement with no attached comments.
    #[must_use]
    pub fn bare(expression: Expression) -> Self {
        Self {
            comments: CommentAttachment::default(),
            expression,
            preceding_blank_line: false,
        }
    }
}

// --- Shared Kernel: Name Conversions ---

/// Converts a `CamelCase` class name to a `snake_case` Erlang module name.
///
/// **DDD Context:** Shared Kernel — used by both Code Generation and Language Service.
///
/// Note: Acronyms like `HTTPRouter` become `httprouter` (no underscores within acronyms).
///
/// # Examples
///
/// ```
/// use beamtalk_core::ast::to_module_name;
/// assert_eq!(to_module_name("Counter"), "counter");
/// assert_eq!(to_module_name("MyCounterActor"), "my_counter_actor");
/// ```
pub fn to_module_name(class_name: &str) -> String {
    let mut result = String::new();
    let mut prev_was_lowercase = false;

    for ch in class_name.chars() {
        if ch.is_uppercase() {
            if prev_was_lowercase {
                result.push('_');
            }
            result.extend(ch.to_lowercase());
            prev_was_lowercase = false;
        } else {
            result.push(ch);
            prev_was_lowercase = ch.is_lowercase();
        }
    }

    result
}

/// Resolves a package-qualified class reference to a BEAM module atom (ADR 0016 + 0070).
///
/// Given a class name and an optional package qualifier, produces the fully qualified
/// BEAM module name:
/// - `resolve_qualified_module_name("Parser", Some("json"))` → `"bt@json@parser"`
/// - `resolve_qualified_module_name("Counter", None)` → `"counter"` (unqualified, no prefix)
///
/// The caller is responsible for adding the `bt@` or `bt@stdlib@` prefix for unqualified
/// names based on compilation context. This function only handles the package-qualified case.
///
/// # Examples
///
/// ```
/// use beamtalk_core::ast::resolve_qualified_module_name;
/// assert_eq!(resolve_qualified_module_name("Parser", Some("json")), "bt@json@parser");
/// assert_eq!(resolve_qualified_module_name("MyClass", Some("utils")), "bt@utils@my_class");
/// assert_eq!(resolve_qualified_module_name("Counter", None), "counter");
/// ```
pub fn resolve_qualified_module_name(class_name: &str, package: Option<&str>) -> String {
    let snake = to_module_name(class_name);
    match package {
        Some(pkg) => format!("bt@{pkg}@{snake}"),
        None => snake,
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
        assert!(module.file_leading_comments.is_empty());
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
    fn message_selector_to_erlang_atom_unary() {
        let selector = MessageSelector::Unary("increment".into());
        assert_eq!(selector.to_erlang_atom(), "increment");
    }

    #[test]
    fn message_selector_to_erlang_atom_binary() {
        let selector = MessageSelector::Binary("+".into());
        assert_eq!(selector.to_erlang_atom(), "+");
    }

    #[test]
    fn message_selector_to_erlang_atom_keyword() {
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(0, 3)),
            KeywordPart::new("put:", Span::new(5, 9)),
        ]);
        assert_eq!(selector.to_erlang_atom(), "at:put:");
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
        let _array = Literal::List(vec![Literal::Integer(1), Literal::Integer(2)]);
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

    #[test]
    fn type_annotation_simple() {
        let ty = TypeAnnotation::simple("Integer", Span::new(0, 7));
        assert!(matches!(ty, TypeAnnotation::Simple(_)));
        assert_eq!(ty.span(), Span::new(0, 7));
    }

    #[test]
    fn type_annotation_singleton() {
        let ty = TypeAnnotation::singleton("north", Span::new(0, 6));
        assert!(matches!(ty, TypeAnnotation::Singleton { .. }));
        if let TypeAnnotation::Singleton { name, span } = ty {
            assert_eq!(name, "north");
            assert_eq!(span, Span::new(0, 6));
        }
    }

    #[test]
    fn type_annotation_union() {
        let ty = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", Span::new(0, 7)),
                TypeAnnotation::simple("String", Span::new(10, 16)),
            ],
            Span::new(0, 16),
        );
        if let TypeAnnotation::Union { types, span } = ty {
            assert_eq!(types.len(), 2);
            assert_eq!(span, Span::new(0, 16));
        }
    }

    #[test]
    fn type_annotation_generic() {
        let ty = TypeAnnotation::generic(
            Identifier::new("Collection", Span::new(0, 10)),
            vec![TypeAnnotation::simple("Integer", Span::new(11, 18))],
            Span::new(0, 19),
        );
        if let TypeAnnotation::Generic {
            base,
            parameters,
            span,
        } = ty
        {
            assert_eq!(base.name, "Collection");
            assert_eq!(parameters.len(), 1);
            assert_eq!(span, Span::new(0, 19));
        }
    }

    #[test]
    fn type_annotation_false_or() {
        let inner = TypeAnnotation::simple("Integer", Span::new(0, 7));
        let ty = TypeAnnotation::false_or(inner, Span::new(0, 15));
        assert!(matches!(ty, TypeAnnotation::FalseOr { .. }));
        assert_eq!(ty.span(), Span::new(0, 15));
    }

    #[test]
    fn type_annotation_span() {
        // Test that span() works for all variants
        let simple = TypeAnnotation::simple("Int", Span::new(0, 3));
        assert_eq!(simple.span(), Span::new(0, 3));

        let singleton = TypeAnnotation::singleton("ok", Span::new(0, 3));
        assert_eq!(singleton.span(), Span::new(0, 3));

        let union = TypeAnnotation::union(vec![], Span::new(0, 10));
        assert_eq!(union.span(), Span::new(0, 10));

        let generic = TypeAnnotation::generic(
            Identifier::new("List", Span::new(0, 4)),
            vec![],
            Span::new(0, 6),
        );
        assert_eq!(generic.span(), Span::new(0, 6));

        let false_or = TypeAnnotation::false_or(
            TypeAnnotation::simple("X", Span::new(0, 1)),
            Span::new(0, 9),
        );
        assert_eq!(false_or.span(), Span::new(0, 9));
    }

    #[test]
    fn type_annotation_type_name() {
        let simple = TypeAnnotation::simple("Integer", Span::new(0, 7));
        assert_eq!(simple.type_name(), "Integer");

        let singleton = TypeAnnotation::singleton("north", Span::new(0, 6));
        assert_eq!(singleton.type_name(), "#north");

        let union = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", Span::new(0, 7)),
                TypeAnnotation::simple("String", Span::new(10, 16)),
            ],
            Span::new(0, 16),
        );
        assert_eq!(union.type_name(), "Integer | String");

        let generic = TypeAnnotation::generic(
            Identifier::new("Collection", Span::new(0, 10)),
            vec![TypeAnnotation::simple("Integer", Span::new(11, 18))],
            Span::new(0, 19),
        );
        assert_eq!(generic.type_name(), "Collection(Integer)");

        let false_or = TypeAnnotation::false_or(
            TypeAnnotation::simple("Integer", Span::new(0, 7)),
            Span::new(0, 15),
        );
        assert_eq!(false_or.type_name(), "Integer | False");

        // FalseOr with union inner type should parenthesize
        let false_or_union = TypeAnnotation::false_or(
            TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("Integer", Span::new(0, 7)),
                    TypeAnnotation::simple("String", Span::new(10, 16)),
                ],
                Span::new(0, 16),
            ),
            Span::new(0, 24),
        );
        assert_eq!(false_or_union.type_name(), "(Integer | String) | False");
    }

    #[test]
    fn state_declaration_creation() {
        let name = Identifier::new("value", Span::new(0, 5));
        let state = StateDeclaration::new(name, Span::new(0, 5));
        assert_eq!(state.name.name, "value");
        assert!(state.type_annotation.is_none());
        assert!(state.default_value.is_none());
    }

    #[test]
    fn state_declaration_with_type() {
        let name = Identifier::new("count", Span::new(0, 5));
        let ty = TypeAnnotation::simple("Integer", Span::new(7, 14));
        let state = StateDeclaration::with_type(name, ty, Span::new(0, 14));
        assert!(state.type_annotation.is_some());
        assert!(state.default_value.is_none());
    }

    #[test]
    fn state_declaration_with_default() {
        let name = Identifier::new("value", Span::new(0, 5));
        let default = Expression::Literal(Literal::Integer(0), Span::new(8, 9));
        let state = StateDeclaration::with_default(name, default, Span::new(0, 9));
        assert!(state.type_annotation.is_none());
        assert!(state.default_value.is_some());
    }

    #[test]
    fn state_declaration_with_type_and_default() {
        let name = Identifier::new("value", Span::new(0, 5));
        let ty = TypeAnnotation::simple("Integer", Span::new(7, 14));
        let default = Expression::Literal(Literal::Integer(0), Span::new(17, 18));
        let state = StateDeclaration::with_type_and_default(name, ty, default, Span::new(0, 18));
        assert!(state.type_annotation.is_some());
        assert!(state.default_value.is_some());
    }

    #[test]
    fn method_definition_unary() {
        let selector = MessageSelector::Unary("increment".into());
        let body = vec![ExpressionStatement::bare(Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "self",
                    Span::new(0, 4),
                ))),
                field: Identifier::new("value", Span::new(5, 10)),
                span: Span::new(0, 10),
            }),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(14, 18),
                    ))),
                    field: Identifier::new("value", Span::new(19, 24)),
                    span: Span::new(14, 24),
                }),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(27, 28))],
                is_cast: false,
                span: Span::new(14, 28),
            }),
            type_annotation: None,
            span: Span::new(0, 28),
        })];
        let method = MethodDefinition::new(selector, Vec::new(), body, Span::new(0, 28));
        assert_eq!(method.selector.name(), "increment");
        assert!(method.parameters.is_empty());
        assert_eq!(method.body.len(), 1);
        assert!(method.return_type.is_none());
    }

    #[test]
    fn method_definition_keyword() {
        let selector = MessageSelector::Keyword(vec![KeywordPart::new("at:", Span::new(0, 3))]);
        let params = vec![ParameterDefinition::new(Identifier::new(
            "index",
            Span::new(4, 9),
        ))];
        let body = vec![ExpressionStatement::bare(Expression::Identifier(
            Identifier::new("result", Span::new(13, 19)),
        ))];
        let method = MethodDefinition::new(selector, params, body, Span::new(0, 19));
        assert_eq!(method.selector.name(), "at:");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "index");
    }

    #[test]
    fn method_definition_with_return_type() {
        let selector = MessageSelector::Unary("getValue".into());
        let body = vec![ExpressionStatement::bare(Expression::Return {
            value: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "self",
                    Span::new(0, 4),
                ))),
                field: Identifier::new("value", Span::new(5, 10)),
                span: Span::new(0, 10),
            }),
            span: Span::new(0, 10),
        })];
        let return_type = TypeAnnotation::simple("Integer", Span::new(12, 19));
        let method = MethodDefinition::with_return_type(
            selector,
            Vec::new(),
            body,
            return_type,
            Span::new(0, 19),
        );
        assert!(method.return_type.is_some());
    }

    #[test]
    fn class_definition_creation() {
        let name = Identifier::new("Counter", Span::new(0, 7));
        let superclass = Identifier::new("Actor", Span::new(18, 23));

        let state = vec![StateDeclaration::with_default(
            Identifier::new("value", Span::new(32, 37)),
            Expression::Literal(Literal::Integer(0), Span::new(40, 41)),
            Span::new(32, 41),
        )];

        let methods = vec![MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            Vec::new(),
            vec![ExpressionStatement::bare(Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(50, 54),
                    ))),
                    field: Identifier::new("value", Span::new(55, 60)),
                    span: Span::new(50, 60),
                }),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(Expression::FieldAccess {
                        receiver: Box::new(Expression::Identifier(Identifier::new(
                            "self",
                            Span::new(64, 68),
                        ))),
                        field: Identifier::new("value", Span::new(69, 74)),
                        span: Span::new(64, 74),
                    }),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(77, 78))],
                    is_cast: false,
                    span: Span::new(64, 78),
                }),
                type_annotation: None,
                span: Span::new(50, 78),
            })],
            Span::new(45, 78),
        )];

        let class = ClassDefinition::new(name, superclass, state, methods, Span::new(0, 78));

        assert_eq!(class.name.name, "Counter");
        assert_eq!(class.superclass.as_ref().unwrap().name, "Actor");
        assert_eq!(class.state.len(), 1);
        assert_eq!(class.methods.len(), 1);
    }

    #[test]
    fn module_with_classes() {
        let class = ClassDefinition::new(
            Identifier::new("Counter", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(18, 23)),
            Vec::new(),
            Vec::new(),
            Span::new(0, 23),
        );

        let module = Module::with_classes(vec![class], Span::new(0, 23));

        assert_eq!(module.classes.len(), 1);
        assert!(module.expressions.is_empty());
        assert_eq!(module.classes[0].name.name, "Counter");
    }

    #[test]
    fn module_creation_includes_classes() {
        let span = Span::new(0, 10);
        let module = Module::new(Vec::new(), span);
        assert!(module.classes.is_empty());
        assert!(module.expressions.is_empty());
    }

    // --- CommentAttachment tests (BT-973) ---

    #[test]
    fn comment_attachment_default_is_empty() {
        let ca = CommentAttachment::default();
        assert!(ca.is_empty());
        assert!(ca.leading.is_empty());
        assert!(ca.trailing.is_none());
    }

    #[test]
    fn comment_attachment_with_leading_not_empty() {
        let ca = CommentAttachment {
            leading: vec![Comment::line("a comment", Span::new(0, 11))],
            trailing: None,
        };
        assert!(!ca.is_empty());
    }

    #[test]
    fn comment_attachment_with_trailing_not_empty() {
        let ca = CommentAttachment {
            leading: Vec::new(),
            trailing: Some(Comment::line("trailing", Span::new(10, 20))),
        };
        assert!(!ca.is_empty());
    }

    #[test]
    fn comment_attachment_with_both_leading_and_trailing() {
        let ca = CommentAttachment {
            leading: vec![Comment::line("leading", Span::new(0, 9))],
            trailing: Some(Comment::line("trailing", Span::new(20, 30))),
        };
        assert!(!ca.is_empty());
        assert_eq!(ca.leading.len(), 1);
        assert!(ca.trailing.is_some());
    }

    #[test]
    fn comment_constructors() {
        let line = Comment::line("hello", Span::new(0, 7));
        assert_eq!(line.kind, CommentKind::Line);
        assert_eq!(line.content, "hello");

        let block = Comment::block("world", Span::new(0, 9));
        assert_eq!(block.kind, CommentKind::Block);
        assert_eq!(block.content, "world");
    }

    #[test]
    fn expression_statement_bare() {
        let expr = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        let stmt = ExpressionStatement::bare(expr.clone());
        assert!(stmt.comments.is_empty());
        assert_eq!(stmt.expression, expr);
    }

    #[test]
    fn expression_statement_with_comments() {
        let expr = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
        let stmt = ExpressionStatement {
            comments: CommentAttachment {
                leading: vec![Comment::line("before", Span::new(0, 8))],
                trailing: None,
            },
            expression: expr,
            preceding_blank_line: false,
        };
        assert!(!stmt.comments.is_empty());
        assert_eq!(stmt.comments.leading.len(), 1);
    }

    // --- ClassKind tests (BT-922) ---

    #[test]
    fn class_kind_from_superclass_name() {
        assert_eq!(ClassKind::from_superclass_name("Actor"), ClassKind::Actor);
        assert_eq!(ClassKind::from_superclass_name("Value"), ClassKind::Value);
        assert_eq!(ClassKind::from_superclass_name("Object"), ClassKind::Object);
        assert_eq!(
            ClassKind::from_superclass_name("Counter"),
            ClassKind::Object
        );
    }

    #[test]
    fn class_definition_new_derives_class_kind() {
        let actor_class = ClassDefinition::new(
            Identifier::new("Counter", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(0, 5)),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        assert_eq!(actor_class.class_kind, ClassKind::Actor);

        let value_class = ClassDefinition::new(
            Identifier::new("Point", Span::new(0, 5)),
            Identifier::new("Value", Span::new(0, 5)),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        assert_eq!(value_class.class_kind, ClassKind::Value);

        let object_class = ClassDefinition::new(
            Identifier::new("MyList", Span::new(0, 6)),
            Identifier::new("Object", Span::new(0, 6)),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        assert_eq!(object_class.class_kind, ClassKind::Object);
    }

    #[test]
    fn class_definition_backing_module_defaults_to_none() {
        let class = ClassDefinition::new(
            Identifier::new("Counter", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(0, 5)),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        assert_eq!(class.backing_module, None);
    }

    #[test]
    fn class_definition_with_modifiers_backing_module_defaults_to_none() {
        let class = ClassDefinition::with_modifiers(
            Identifier::new("Counter", Span::new(0, 7)),
            Some(Identifier::new("Actor", Span::new(0, 5))),
            ClassModifiers::default(),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        assert_eq!(class.backing_module, None);
    }

    #[test]
    fn class_definition_backing_module_can_be_set() {
        let mut class = ClassDefinition::new(
            Identifier::new("MyActor", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(0, 5)),
            vec![],
            vec![],
            Span::new(0, 20),
        );
        class.backing_module = Some(Identifier::new("my_erlang_module", Span::new(0, 16)));
        assert_eq!(
            class.backing_module.as_ref().map(|id| id.name.as_str()),
            Some("my_erlang_module")
        );
    }

    // --- resolve_qualified_module_name tests (ADR 0070 Phase 2) ---

    #[test]
    fn resolve_qualified_module_name_with_package() {
        assert_eq!(
            resolve_qualified_module_name("Parser", Some("json")),
            "bt@json@parser"
        );
    }

    #[test]
    fn resolve_qualified_module_name_multi_word_class() {
        assert_eq!(
            resolve_qualified_module_name("MyClass", Some("utils")),
            "bt@utils@my_class"
        );
    }

    #[test]
    fn resolve_qualified_module_name_without_package() {
        assert_eq!(resolve_qualified_module_name("Counter", None), "counter");
    }

    #[test]
    fn resolve_qualified_module_name_stdlib_like_class() {
        // Even well-known classes get the package prefix when explicitly qualified
        assert_eq!(
            resolve_qualified_module_name("Integer", Some("math")),
            "bt@math@integer"
        );
    }
}
