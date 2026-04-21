// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression AST nodes for Beamtalk.
//!
//! This module contains the [`Expression`] enum and its supporting types:
//! literals, identifiers, message selectors, blocks, cascades, and map pairs.

use crate::source_analysis::Span;
use ecow::{EcoString, eco_format};

use super::ExpressionStatement;
use super::pattern::{MatchArm, Pattern};

/// The diagnostic category that an `@expect` directive suppresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectCategory {
    /// Suppress does-not-understand (DNU) hints.
    Dnu,
    /// Suppress type-related warnings/hints.
    Type,
    /// Suppress unused-variable warnings.
    Unused,
    /// Suppress dead-block-assignment warnings (BT-1476).
    DeadAssignment,
    /// Suppress deprecation warnings — wrong keyword/class-kind (BT-1529).
    Deprecation,
    /// Suppress actor-new errors — using `new`/`new:` on an Actor subclass (BT-1559).
    ActorNew,
    /// Suppress visibility errors/warnings — cross-package internal method access (ADR 0071, BT-1702).
    Visibility,
    /// Suppress unresolved class warnings (BT-1726).
    UnresolvedClass,
    /// Suppress unresolved FFI module warnings (BT-1726).
    UnresolvedFfi,
    /// Suppress arity mismatch warnings (BT-1726).
    ArityMismatch,
    /// Suppress workspace-binding-shadows-class warnings (BT-1759).
    ShadowedClass,
    /// Suppress missing type annotation warnings in typed classes (BT-1918).
    TypeAnnotation,
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
            "dead_assignment" => Some(Self::DeadAssignment),
            "deprecation" => Some(Self::Deprecation),
            "actor_new" => Some(Self::ActorNew),
            "visibility" => Some(Self::Visibility),
            "unresolved_class" => Some(Self::UnresolvedClass),
            "unresolved_ffi" => Some(Self::UnresolvedFfi),
            "arity_mismatch" => Some(Self::ArityMismatch),
            "shadowed_class" => Some(Self::ShadowedClass),
            "type_annotation" => Some(Self::TypeAnnotation),
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
            Self::DeadAssignment => "dead_assignment",
            Self::Deprecation => "deprecation",
            Self::ActorNew => "actor_new",
            Self::Visibility => "visibility",
            Self::UnresolvedClass => "unresolved_class",
            Self::UnresolvedFfi => "unresolved_ffi",
            Self::ArityMismatch => "arity_mismatch",
            Self::ShadowedClass => "shadowed_class",
            Self::TypeAnnotation => "type_annotation",
            Self::All => "all",
        }
    }

    /// Returns the list of all valid category name strings, for use in error messages.
    #[must_use]
    pub const fn valid_names() -> &'static [&'static str] {
        &[
            "dnu",
            "type",
            "unused",
            "dead_assignment",
            "deprecation",
            "actor_new",
            "visibility",
            "unresolved_class",
            "unresolved_ffi",
            "arity_mismatch",
            "shadowed_class",
            "type_annotation",
            "all",
        ]
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

    /// A class reference (uppercase identifier), optionally package-qualified.
    ///
    /// Class references are global and resolve to module names.
    /// Example: `Counter`, `Array`, `MyCustomClass`, `json@Parser`
    /// Compiles to module calls: `Counter spawn` → `call 'counter':'spawn'()`
    ClassReference {
        /// The class name (as written in source).
        name: Identifier,
        /// Optional package qualifier (e.g., `json` in `json@Parser`).
        /// See ADR 0070, Section 4.
        package: Option<Identifier>,
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

    /// An assignment statement (`x := value` or `x :: Type := value`).
    Assignment {
        /// The target being assigned to (identifier or field access).
        target: Box<Expression>,
        /// The value being assigned.
        value: Box<Expression>,
        /// Optional type annotation (e.g., `x :: Integer := 5`).
        ///
        /// When present, the type checker uses this as the binding's type
        /// rather than the inferred RHS type. Codegen ignores it (erased).
        type_annotation: Option<TypeAnnotation>,
        /// Source location of the entire assignment.
        span: Span,
    },

    /// A destructuring assignment (`{a, b} := expr` or `#[a, b] := expr`).
    ///
    /// Binds multiple variables from a tuple or array in a single statement.
    ///
    /// Examples:
    /// ```beamtalk
    /// {a, b} := someTuple
    /// #[first, second] := someArray
    /// {#ok, val} := erlangResult
    /// ```
    DestructureAssignment {
        /// The destructuring pattern.
        pattern: Pattern,
        /// The value being destructured.
        value: Box<Expression>,
        /// Source location.
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

    /// A primitive pragma (`@primitive "selector"` or `@primitive intrinsicName`).
    ///
    /// Declares that a method body delegates to a runtime primitive or
    /// structural intrinsic. Only valid inside method bodies in stdlib code
    /// (see ADR 0007).
    ///
    /// Example: `+ other => @primitive "+"`
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
        /// Optional human-readable reason for the suppression (BT-1918).
        ///
        /// Parsed from `@expect category "reason string"`. When present, the
        /// reason is included in stale `@expect` warnings for better diagnostics.
        reason: Option<EcoString>,
        /// Source location of the entire `@expect category` expression.
        span: Span,
    },

    /// Spread/rest element inside an array literal: `...ident`
    ///
    /// Only valid inside array destructuring patterns (`#[a, ...rest] := expr`).
    /// Parsed as an expression so that the array literal parser can accept it,
    /// then converted to a rest pattern during destructure assignment lowering.
    Spread {
        /// The identifier being spread into (e.g. `rest` in `...rest`).
        name: Identifier,
        /// Source location covering `...ident`.
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
            | Self::DestructureAssignment { span, .. }
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
            | Self::Spread { span, .. }
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

    /// A string literal (double-quoted in source).
    ///
    /// Example: `"hello, world"`
    /// Escapes: `""` for an embedded double quote
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

/// A type parameter declaration with an optional protocol bound (ADR 0068 Phase 2d).
///
/// In class/protocol definitions, type parameters may be bounded by a protocol:
/// `T :: Printable` means "T must conform to Printable". Unbounded parameters
/// accept any type.
///
/// Examples:
/// - `Result(T, E)` — unbounded: `T` and `E` accept any type
/// - `Logger(T :: Printable)` — bounded: `T` must conform to `Printable`
/// - `SortedSet(E :: Comparable)` — bounded: `E` must conform to `Comparable`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParamDecl {
    /// The type parameter name (e.g., `T`, `E`, `K`, `V`).
    pub name: Identifier,
    /// Optional protocol bound (e.g., `Printable` in `T :: Printable`).
    ///
    /// When present, the type checker verifies that concrete type arguments
    /// conform to this protocol. When absent, any type is accepted.
    pub bound: Option<Identifier>,
    /// Source location spanning the entire declaration (name through bound).
    pub span: Span,
}

impl TypeParamDecl {
    /// Creates an unbounded type parameter declaration.
    #[must_use]
    pub fn unbounded(name: Identifier) -> Self {
        let span = name.span;
        Self {
            name,
            bound: None,
            span,
        }
    }

    /// Creates a bounded type parameter declaration.
    #[must_use]
    pub fn bounded(name: Identifier, bound: Identifier, span: Span) -> Self {
        Self {
            name,
            bound: Some(bound),
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
/// - `Self` (return type resolves to the static receiver class)
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
    /// A generic type (e.g., `Collection(Integer)`, `Result(T, E)`).
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
    /// The `Self` return type — resolves to the static receiver class at call sites.
    ///
    /// Only valid in return position. Placing `Self` in parameter position is an error.
    ///
    /// Example: `collect: block: Block -> Self`
    SelfType {
        /// Source location.
        span: Span,
    },
    /// The `Self class` metatype — resolves to the metaclass of the receiver at call sites.
    ///
    /// Only valid in return position. Indicates a method returns the class object
    /// (metaclass) of the receiver, not an instance.
    ///
    /// Example: `class -> Self class`
    SelfClass {
        /// Source location.
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
            | Self::FalseOr { span, .. }
            | Self::SelfType { span }
            | Self::SelfClass { span } => *span,
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
                let mut result = eco_format!("{}(", base.name);
                for (i, ty) in parameters.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&ty.type_name());
                }
                result.push(')');
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
            Self::SelfType { .. } => "Self".into(),
            Self::SelfClass { .. } => "Self class".into(),
        }
    }
}
