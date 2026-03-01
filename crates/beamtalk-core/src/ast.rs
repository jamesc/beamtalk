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
//! use beamtalk_core::source_analysis::Span;
//!
//! // Source: x := 3 + 4
//! let module = Module {
//!     classes: Vec::new(),
//!     method_definitions: Vec::new(),
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
//! constructors (e.g., `InstanceVariable::with_type()`, `Module::with_classes()`).
//! Prefer these constructors over manual struct initialization.

use crate::source_analysis::Span;
use ecow::{EcoString, eco_format};

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

/// The kind of class based on its declaration form (ADR 0042).
///
/// Determined at parse time from the superclass used in the `subclass:` declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ClassKind {
    /// An ordinary class (`Object subclass:` or any non-Actor, non-Value superclass).
    #[default]
    Object,
    /// An actor class (`Actor subclass:`). Uses `gen_server` semantics.
    Actor,
    /// A value class (`Value subclass:`). Immutable value-object semantics (ADR 0042).
    Value,
}

impl ClassKind {
    /// Derive the class kind from the direct superclass name used in a `subclass:` declaration.
    ///
    /// - `"Actor"` → [`ClassKind::Actor`]
    /// - `"Value"` → [`ClassKind::Value`]
    /// - anything else → [`ClassKind::Object`]
    #[must_use]
    pub fn from_superclass_name(name: &str) -> Self {
        match name {
            "Actor" => ClassKind::Actor,
            "Value" => ClassKind::Value,
            _ => ClassKind::Object,
        }
    }
}

/// A class definition (Aggregate Root).
///
/// Example:
/// ```text
/// Actor subclass: Counter
///   state: value = 0
///
///   increment => self.value += 1
///   getValue => ^self.value
/// ```
///
/// # DDD: Aggregate Root for Class Compilation
///
/// `ClassDefinition` is an aggregate root that encapsulates all class-related
/// entities: state declarations and method definitions. It enforces class-level
/// invariants and provides the boundary for class-specific code generation.
///
/// # Invariants
///
/// The following invariants should hold for a valid class:
///
/// 1. **Unique state names**: No two state declarations may have the same name
/// 2. **Unique method selectors**: No two methods may have the same selector
///    (within the same method kind)
/// 3. **Valid superclass**: Superclass must be a valid identifier (existence
///    checked during semantic analysis)
/// 4. **Span containment**: All child spans must be within the class span
/// 5. **Arity consistency**: Method parameter count must match selector arity
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    /// The class name (e.g., `Counter`).
    pub name: Identifier,
    /// The superclass name (e.g., `Actor`). `None` for root classes (`ProtoObject`).
    pub superclass: Option<Identifier>,
    /// The class kind derived from the declaration form (Actor/Value/Object).
    pub class_kind: ClassKind,
    /// Whether this is an abstract class (cannot be instantiated).
    pub is_abstract: bool,
    /// Whether this is a sealed class (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this is a typed class (requires type annotations on methods).
    pub is_typed: bool,
    /// Instance variable declarations.
    pub state: Vec<StateDeclaration>,
    /// Method definitions.
    pub methods: Vec<MethodDefinition>,
    /// Class-side method definitions (defined with `class` prefix).
    pub class_methods: Vec<MethodDefinition>,
    /// Class variable declarations (defined with `classState:`).
    pub class_variables: Vec<StateDeclaration>,
    /// Non-doc comments (`//` and `/* */`) appearing before this class.
    pub comments: CommentAttachment,
    /// Doc comment attached to this class (`///` lines).
    pub doc_comment: Option<String>,
    /// Source location of the entire class definition.
    pub span: Span,
}

impl ClassDefinition {
    /// Creates a new class definition.
    #[must_use]
    pub fn new(
        name: Identifier,
        superclass: Identifier,
        state: Vec<StateDeclaration>,
        methods: Vec<MethodDefinition>,
        span: Span,
    ) -> Self {
        let class_kind = ClassKind::from_superclass_name(superclass.name.as_str());
        Self {
            name,
            superclass: Some(superclass),
            class_kind,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state,
            methods,
            class_methods: Vec::new(),
            class_variables: Vec::new(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a class definition with modifiers.
    #[must_use]
    pub fn with_modifiers(
        name: Identifier,
        superclass: Option<Identifier>,
        is_abstract: bool,
        is_sealed: bool,
        state: Vec<StateDeclaration>,
        methods: Vec<MethodDefinition>,
        span: Span,
    ) -> Self {
        let class_kind = superclass.as_ref().map_or(ClassKind::Object, |s| {
            ClassKind::from_superclass_name(s.name.as_str())
        });
        Self {
            name,
            superclass,
            class_kind,
            is_abstract,
            is_sealed,
            is_typed: false,
            state,
            methods,
            class_methods: Vec::new(),
            class_variables: Vec::new(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Returns the superclass name, or `"none"` for root classes.
    #[must_use]
    pub fn superclass_name(&self) -> &str {
        self.superclass.as_ref().map_or("none", |s| s.name.as_str())
    }
}

/// A standalone method definition (Tonel-style).
///
/// Defines or replaces a single method on an existing class without
/// redefining the entire class. Works in `.bt` files, REPL, and via LSP.
///
/// Example: `Counter >> increment => self.value := self.value + 1`
/// Example: `Counter class >> withInitial: n => self spawnWith: #{value => n}`
#[derive(Debug, Clone, PartialEq)]
pub struct StandaloneMethodDefinition {
    /// The class this method belongs to.
    pub class_name: Identifier,
    /// Whether this is a class-side method (`Counter class >> ...`).
    pub is_class_method: bool,
    /// The method definition.
    pub method: MethodDefinition,
    /// Source location of the entire standalone method definition.
    pub span: Span,
}

/// A state (instance variable) declaration.
///
/// Example: `state: value: Integer = 0`
#[derive(Debug, Clone, PartialEq)]
pub struct StateDeclaration {
    /// The field name.
    pub name: Identifier,
    /// Optional type annotation.
    pub type_annotation: Option<TypeAnnotation>,
    /// Optional default value.
    pub default_value: Option<Expression>,
    /// Non-doc comments (`//` and `/* */`) appearing before this field.
    pub comments: CommentAttachment,
    /// Doc comment attached to this field (`///` lines).
    ///
    /// **Note:** Always `None` until parser support lands in BT-975.
    pub doc_comment: Option<String>,
    /// Source location.
    pub span: Span,
}

impl StateDeclaration {
    /// Creates a new state declaration.
    #[must_use]
    pub fn new(name: Identifier, span: Span) -> Self {
        Self {
            name,
            type_annotation: None,
            default_value: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with a type annotation.
    #[must_use]
    pub fn with_type(name: Identifier, type_annotation: TypeAnnotation, span: Span) -> Self {
        Self {
            name,
            type_annotation: Some(type_annotation),
            default_value: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with a default value.
    #[must_use]
    pub fn with_default(name: Identifier, default_value: Expression, span: Span) -> Self {
        Self {
            name,
            type_annotation: None,
            default_value: Some(default_value),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with both type and default value.
    #[must_use]
    pub fn with_type_and_default(
        name: Identifier,
        type_annotation: TypeAnnotation,
        default_value: Expression,
        span: Span,
    ) -> Self {
        Self {
            name,
            type_annotation: Some(type_annotation),
            default_value: Some(default_value),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }
}

/// The kind of method.
///
/// Currently only Primary methods exist. Additional kinds (e.g., AOP-style
/// before/after/around) may be added in the future if needed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum MethodKind {
    /// A normal/primary method.
    #[default]
    Primary,
}

/// A method parameter definition with optional type annotation.
///
/// Example: `amount: Integer` in `deposit: amount: Integer => ...`
#[derive(Debug, Clone, PartialEq)]
pub struct ParameterDefinition {
    /// The parameter name.
    pub name: Identifier,
    /// Optional type annotation (e.g., `Integer` in `amount: Integer`).
    pub type_annotation: Option<TypeAnnotation>,
}

impl ParameterDefinition {
    /// Creates an untyped parameter definition.
    #[must_use]
    pub fn new(name: Identifier) -> Self {
        Self {
            name,
            type_annotation: None,
        }
    }

    /// Creates a typed parameter definition.
    #[must_use]
    pub fn with_type(name: Identifier, type_annotation: TypeAnnotation) -> Self {
        Self {
            name,
            type_annotation: Some(type_annotation),
        }
    }

    /// Returns the span of the parameter name.
    #[must_use]
    pub const fn span(&self) -> Span {
        self.name.span
    }
}

/// A method definition.
///
/// Example: `getValue -> Integer => ^self.value`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    /// The method selector (name).
    pub selector: MessageSelector,
    /// Method parameters with optional type annotations.
    pub parameters: Vec<ParameterDefinition>,
    /// The method body expressions.
    pub body: Vec<ExpressionStatement>,
    /// Optional return type annotation.
    pub return_type: Option<TypeAnnotation>,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
    /// The kind of method.
    pub kind: MethodKind,
    /// Non-doc comments (`//` and `/* */`) appearing before this method.
    pub comments: CommentAttachment,
    /// Doc comment attached to this method (`///` lines).
    pub doc_comment: Option<String>,
    /// Source location.
    pub span: Span,
}

impl MethodDefinition {
    /// Creates a new method definition.
    #[must_use]
    pub fn new(
        selector: MessageSelector,
        parameters: Vec<ParameterDefinition>,
        body: Vec<ExpressionStatement>,
        span: Span,
    ) -> Self {
        Self {
            selector,
            parameters,
            body,
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a method definition with a return type.
    #[must_use]
    pub fn with_return_type(
        selector: MessageSelector,
        parameters: Vec<ParameterDefinition>,
        body: Vec<ExpressionStatement>,
        return_type: TypeAnnotation,
        span: Span,
    ) -> Self {
        Self {
            selector,
            parameters,
            body,
            return_type: Some(return_type),
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a method definition with full options.
    #[must_use]
    pub fn with_options(
        selector: MessageSelector,
        parameters: Vec<ParameterDefinition>,
        body: Vec<ExpressionStatement>,
        return_type: Option<TypeAnnotation>,
        is_sealed: bool,
        kind: MethodKind,
        span: Span,
    ) -> Self {
        Self {
            selector,
            parameters,
            body,
            return_type,
            is_sealed,
            kind,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }
}

/// A type annotation for optional typing.
///
/// Beamtalk supports gradual typing - add types where helpful.
/// All variants carry a [`Span`] for error reporting and IDE features.
///
/// Examples:
/// - `Integer`
/// - `String`
/// - `Counter` (custom class)
/// - `Integer | String` (union)
/// - `#north | #south | #east | #west` (singleton/enum)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    /// A simple named type (e.g., `Integer`, `String`, `Counter`).
    Simple(Identifier),
    /// A union type (e.g., `Integer | String`).
    Union {
        /// The types in the union.
        types: Vec<TypeAnnotation>,
        /// Source location of the entire union type.
        span: Span,
    },
    /// A singleton/literal type (e.g., `#north`).
    Singleton {
        /// The singleton value name.
        name: EcoString,
        /// Source location.
        span: Span,
    },
    /// A generic type (e.g., `Collection<Integer>`).
    Generic {
        /// The base type name.
        base: Identifier,
        /// Type parameters.
        parameters: Vec<TypeAnnotation>,
        /// Source location of the entire generic type.
        span: Span,
    },
    /// A false-or type (Option/Maybe pattern).
    ///
    /// Example: `Integer | False`
    FalseOr {
        /// The inner type.
        inner: Box<TypeAnnotation>,
        /// Source location of the entire false-or type.
        span: Span,
    },
}

impl TypeAnnotation {
    /// Returns the span of this type annotation.
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::Simple(id) => id.span,
            Self::Union { span, .. }
            | Self::Singleton { span, .. }
            | Self::Generic { span, .. }
            | Self::FalseOr { span, .. } => *span,
        }
    }

    /// Creates a simple type annotation.
    #[must_use]
    pub fn simple(name: impl Into<EcoString>, span: Span) -> Self {
        Self::Simple(Identifier::new(name, span))
    }

    /// Creates a singleton type annotation.
    #[must_use]
    pub fn singleton(name: impl Into<EcoString>, span: Span) -> Self {
        Self::Singleton {
            name: name.into(),
            span,
        }
    }

    /// Creates a union type annotation.
    #[must_use]
    pub fn union(types: Vec<TypeAnnotation>, span: Span) -> Self {
        Self::Union { types, span }
    }

    /// Creates a generic type annotation.
    #[must_use]
    pub fn generic(base: Identifier, parameters: Vec<TypeAnnotation>, span: Span) -> Self {
        Self::Generic {
            base,
            parameters,
            span,
        }
    }

    /// Creates a false-or type annotation.
    #[must_use]
    pub fn false_or(inner: TypeAnnotation, span: Span) -> Self {
        Self::FalseOr {
            inner: Box::new(inner),
            span,
        }
    }

    /// Returns a human-readable name for this type annotation.
    ///
    /// Used by the class hierarchy to store declared state field types
    /// and method parameter/return types.
    #[must_use]
    pub fn type_name(&self) -> EcoString {
        match self {
            Self::Simple(id) => id.name.clone(),
            Self::Singleton { name, .. } => eco_format!("#{name}"),
            Self::Union { types, .. } => {
                let mut result = EcoString::new();
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        result.push_str(" | ");
                    }
                    result.push_str(&ty.type_name());
                }
                result
            }
            Self::Generic {
                base, parameters, ..
            } => {
                let mut result = eco_format!("{}<", base.name);
                for (i, ty) in parameters.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&ty.type_name());
                }
                result.push('>');
                result
            }
            Self::FalseOr { inner, .. } => {
                let inner_name = match inner.as_ref() {
                    Self::Union { .. } | Self::FalseOr { .. } => {
                        eco_format!("({})", inner.type_name())
                    }
                    _ => inner.type_name(),
                };
                eco_format!("{inner_name} | False")
            }
        }
    }
}

/// The diagnostic category that an `@expect` directive suppresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectCategory {
    /// Suppress does-not-understand (DNU) hints.
    Dnu,
    /// Suppress type-related warnings/hints.
    Type,
    /// Suppress unused-variable warnings.
    Unused,
    /// Suppress any diagnostic on the following expression.
    All,
}

impl ExpectCategory {
    /// Parses a category name from a Beamtalk identifier string.
    #[must_use]
    pub fn from_name(s: &str) -> Option<Self> {
        match s {
            "dnu" => Some(Self::Dnu),
            "type" => Some(Self::Type),
            "unused" => Some(Self::Unused),
            "all" => Some(Self::All),
            _ => None,
        }
    }

    /// Returns the canonical string representation of this category.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Dnu => "dnu",
            Self::Type => "type",
            Self::Unused => "unused",
            Self::All => "all",
        }
    }
}

/// A Beamtalk expression.
///
/// Expressions are the fundamental building blocks of Beamtalk programs.
/// Everything is an expression, including assignments and returns.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// A literal value.
    Literal(Literal, Span),

    /// An identifier (variable name).
    Identifier(Identifier),

    /// A class reference (uppercase identifier).
    ///
    /// Class references are global and resolve to module names.
    /// Example: `Counter`, `Array`, `MyCustomClass`
    /// Compiles to module calls: `Counter spawn` → `call 'counter':'spawn'()`
    ClassReference {
        /// The class name (as written in source).
        name: Identifier,
        /// Source location.
        span: Span,
    },

    /// The `super` keyword for superclass method dispatch.
    ///
    /// When used as a message receiver, calls the superclass implementation.
    /// Example: `super increment` or `super at: 1 put: value`
    Super(Span),

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
        /// Whether this is a cast (fire-and-forget, terminated with `!`).
        ///
        /// `false` means a synchronous call (terminated with `.` or newline).
        /// `true` means a cast — no return value; codegen uses `gen_server:cast`.
        is_cast: bool,
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

    /// A map literal (dictionary/hash map).
    ///
    /// Example: `#{#name => 'Alice', #age => 30}`
    MapLiteral {
        /// The key-value pairs in the map.
        pairs: Vec<MapPair>,
        /// Source location of the entire map literal.
        span: Span,
    },

    /// A list literal.
    ///
    /// Example: `#(1, 2, 3)` or `#(head | tail)` (cons)
    ListLiteral {
        /// The elements of the list.
        elements: Vec<Expression>,
        /// Optional tail expression (cons syntax: `#(head | tail)`).
        tail: Option<Box<Expression>>,
        /// Source location of the entire list literal.
        span: Span,
    },

    /// An array literal.
    ///
    /// Example: `#[1, 2, 3]`
    ArrayLiteral {
        /// The elements of the array.
        elements: Vec<Expression>,
        /// Source location of the entire array literal.
        span: Span,
    },

    /// A primitive pragma (`@primitive 'selector'` or `@primitive intrinsicName`).
    ///
    /// Declares that a method body delegates to a runtime primitive or
    /// structural intrinsic. Only valid inside method bodies in stdlib code
    /// (see ADR 0007).
    ///
    /// Example: `+ other => @primitive '+'`
    /// Example: `new => @primitive basicNew`
    Primitive {
        /// The primitive name (selector string or intrinsic identifier).
        name: EcoString,
        /// Whether the name was quoted (`"+"`) vs bare (`basicNew`).
        is_quoted: bool,
        /// Whether the original source used `@intrinsic` instead of `@primitive`.
        is_intrinsic: bool,
        /// Source location of the entire `@primitive name` expression.
        span: Span,
    },

    /// A string interpolation expression (ADR 0023).
    ///
    /// Produced when a string contains `{expr}` interpolation segments.
    /// Plain strings without interpolation remain as `Literal(String(...))`.
    ///
    /// Example: `"Hello, {name}!"` produces segments:
    /// `[Literal("Hello, "), Expression(name), Literal("!")]`
    StringInterpolation {
        /// The segments of the interpolated string.
        segments: Vec<StringSegment>,
        /// Source location of the entire interpolated string.
        span: Span,
    },

    /// A diagnostic suppression directive (`@expect category`).
    ///
    /// Suppresses diagnostics of the given category on the immediately
    /// following expression in the same expression list.
    ///
    /// Example: `@expect dnu` before a message send that may produce a DNU hint.
    ExpectDirective {
        /// The category of diagnostic to suppress.
        category: ExpectCategory,
        /// Source location of the entire `@expect category` expression.
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
            | Self::ClassReference { span, .. }
            | Self::Super(span)
            | Self::FieldAccess { span, .. }
            | Self::MessageSend { span, .. }
            | Self::Assignment { span, .. }
            | Self::Return { span, .. }
            | Self::Cascade { span, .. }
            | Self::Parenthesized { span, .. }
            | Self::Match { span, .. }
            | Self::MapLiteral { span, .. }
            | Self::ListLiteral { span, .. }
            | Self::ArrayLiteral { span, .. }
            | Self::Primitive { span, .. }
            | Self::StringInterpolation { span, .. }
            | Self::ExpectDirective { span, .. }
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

    /// A list literal (compile-time constant list).
    ///
    /// Example: `#(1, 2, 3)`
    List(Vec<Literal>),

    /// A character literal.
    ///
    /// Example: `$a`, `$\n`
    Character(char),
}

/// A segment of an interpolated string (ADR 0023).
///
/// String interpolation breaks a string into alternating literal text
/// and embedded expressions. For example, `"Hello, {name}!"` becomes:
/// `[Literal("Hello, "), Interpolation(name_expr), Literal("!")]`
#[derive(Debug, Clone, PartialEq)]
pub enum StringSegment {
    /// A literal text segment.
    Literal(EcoString),

    /// An interpolated expression segment (`{expr}`).
    Interpolation(Expression),
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

    /// Converts this selector to a Core Erlang atom representation.
    ///
    /// This is the **domain service** for selector mangling - converting Beamtalk
    /// selectors to valid Erlang atoms. The returned string is ready to be wrapped
    /// in single quotes for Core Erlang output.
    ///
    /// # DDD: `SelectorMangler` Domain Service
    ///
    /// In the Code Generation bounded context, selector mangling is a key domain
    /// operation. This method encapsulates the domain knowledge of how Beamtalk
    /// selectors map to Erlang atoms.
    ///
    /// # Returns
    ///
    /// The selector as a string suitable for use as an Erlang atom:
    /// - Unary: `"increment"` → `'increment'`
    /// - Binary: `"+"` → `'+'`
    /// - Keyword: `["at:", "put:"]` → `'at:put:'`
    ///
    /// # Example
    ///
    /// ```
    /// use beamtalk_core::ast::{MessageSelector, KeywordPart};
    /// use beamtalk_core::source_analysis::Span;
    ///
    /// // Unary selector
    /// let selector = MessageSelector::Unary("increment".into());
    /// assert_eq!(selector.to_erlang_atom(), "increment");
    ///
    /// // Keyword selector
    /// let selector = MessageSelector::Keyword(vec![
    ///     KeywordPart::new("at:", Span::new(0, 3)),
    ///     KeywordPart::new("put:", Span::new(5, 9)),
    /// ]);
    /// assert_eq!(selector.to_erlang_atom(), "at:put:");
    /// ```
    #[must_use]
    pub fn to_erlang_atom(&self) -> String {
        match self {
            Self::Unary(name) => name.to_string(),
            Self::Binary(op) => op.to_string(),
            Self::Keyword(parts) => parts.iter().map(|p| p.keyword.as_str()).collect(),
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
    pub body: Vec<ExpressionStatement>,
    /// Source location of the entire block (including brackets).
    pub span: Span,
}

impl Block {
    /// Creates a new block.
    #[must_use]
    pub fn new(
        parameters: Vec<BlockParameter>,
        body: Vec<ExpressionStatement>,
        span: Span,
    ) -> Self {
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
    /// Comments attached to this match arm.
    pub comments: CommentAttachment,
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
            comments: CommentAttachment::default(),
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
            comments: CommentAttachment::default(),
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

/// A key-value pair in a map literal.
///
/// Example: In `#{#name => 'Alice', #age => 30}`, each pair is a `MapPair`.
#[derive(Debug, Clone, PartialEq)]
pub struct MapPair {
    /// The key expression.
    pub key: Expression,
    /// The value expression.
    pub value: Expression,
    /// Source location of this key-value pair.
    pub span: Span,
}

impl MapPair {
    /// Creates a new map pair.
    #[must_use]
    pub fn new(key: Expression, value: Expression, span: Span) -> Self {
        Self { key, value, span }
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
        assert_eq!(generic.type_name(), "Collection<Integer>");

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
}
