// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pattern matching AST nodes for Beamtalk.
//!
//! This module contains [`Pattern`], [`MatchArm`], and binary segment types
//! used in pattern matching and destructuring.

use crate::source_analysis::Span;
use ecow::EcoString;

use super::CommentAttachment;
use super::expression::{Expression, Identifier, Literal};

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

    /// Collection destructuring pattern (array or list).
    ///
    /// Example: `#[first, second, _]` or `#(first, second, _)`
    ///
    /// Both forms use `at:` message dispatch for element access.
    /// The `list_syntax` flag tracks whether the source used `#(...)` (list)
    /// or `#[...]` (array) syntax, for round-trip formatting fidelity
    /// and future type checking.
    Array {
        /// Elements of the pattern.
        elements: Vec<Pattern>,
        /// Optional rest pattern binding remaining elements: `...rest`
        rest: Option<Box<Pattern>>,
        /// Whether the source used `#(...)` list syntax (vs `#[...]` array syntax).
        list_syntax: bool,
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

    /// Map destructuring pattern.
    ///
    /// Example: `#{#sid => sid, #runner => runner}`
    Map {
        /// Key-value binding pairs.
        pairs: Vec<MapPatternPair>,
        /// Source location.
        span: Span,
    },

    /// Constructor pattern for sealed types.
    ///
    /// Matches a sealed Value type by its constructor class method.
    ///
    /// Example: `Result ok: v`, `Result error: e`, `Result ok: _`
    ///
    /// Each entry in `keywords` is a `(selector_keyword, binding_pattern)` pair.
    /// The binding pattern is typically `Variable` or `Wildcard`.
    Constructor {
        /// The class name (e.g. `"Result"`).
        class: Identifier,
        /// Keyword-binding pairs (e.g. `[(Identifier("ok:"), Variable("v"))]`).
        keywords: Vec<(Identifier, Pattern)>,
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
            | Self::Array { span, .. }
            | Self::List { span, .. }
            | Self::Binary { span, .. }
            | Self::Map { span, .. }
            | Self::Constructor { span, .. } => *span,
        }
    }
}

/// The key in a map destructuring pattern.
///
/// Distinguishes between symbol keys (`#key`) and string keys (`"key"`),
/// since they compile to different Core Erlang representations (atom vs binary).
#[derive(Debug, Clone, PartialEq)]
pub enum MapPatternKey {
    /// Symbol key: `#key` — compiled as an Erlang atom.
    Symbol(EcoString),
    /// String key: `"key"` — compiled as an Erlang binary.
    StringLit(EcoString),
}

impl MapPatternKey {
    /// Returns the inner string value regardless of key kind.
    pub fn as_str(&self) -> &str {
        match self {
            MapPatternKey::Symbol(s) | MapPatternKey::StringLit(s) => s.as_str(),
        }
    }
}

/// A key-value binding pair in a map destructuring pattern.
///
/// Example: in `#{#sid => sid}`, the pair has key `Symbol("sid")` and value `Pattern::Variable("sid")`.
/// Example: in `#{"event" => ev}`, the pair has key `StringLit("event")` and value `Pattern::Variable("ev")`.
#[derive(Debug, Clone, PartialEq)]
pub struct MapPatternPair {
    /// The map key — either a symbol atom or a string literal.
    pub key: MapPatternKey,
    /// The pattern to bind the value to.
    pub value: Pattern,
    /// Source location.
    pub span: Span,
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
