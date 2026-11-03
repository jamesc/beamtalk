// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pattern matching AST nodes for Beamtalk.
//!
//! This module contains [`Pattern`], [`MatchArm`], and binary segment types
//! used in pattern matching and destructuring.

use crate::span::Span;
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

    /// Nil pattern - matches exactly the value that `isNil` returns true for.
    ///
    /// Example: `nil -> defaultValue`
    ///
    /// `nil` is a reserved identifier, not a `Literal` variant; this is a
    /// dedicated pattern kind (ADR 0107 Phase A) that reuses the existing
    /// atom-literal codegen path (`Document::Str("'nil'")`) — no new runtime
    /// mechanism.
    Nil(Span),

    /// Type pattern - matches if the scrutinee's runtime class is exactly
    /// `class`, binding the value (narrowed to `class` for the arm body) to
    /// `binding`.
    ///
    /// Example: `path :: String -> self expandHome: path`
    ///
    /// Reuses the `::` token already established by ADR 0053 for
    /// parameter/state type annotations, in a new grammatical position
    /// (ADR 0107 Phase A). Phase A restricts `class` to concrete leaf
    /// classes (no subclasses) — a class with subclasses is a compile
    /// error, not silently-wrong behaviour; see ADR 0107 Phase B.
    Type {
        /// The name bound to the matched value inside the arm.
        binding: Identifier,
        /// The class the scrutinee's runtime class must equal.
        class: Identifier,
        /// Source location.
        span: Span,
    },

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
            | Self::Nil(span)
            | Self::Tuple { span, .. }
            | Self::Array { span, .. }
            | Self::List { span, .. }
            | Self::Binary { span, .. }
            | Self::Map { span, .. }
            | Self::Constructor { span, .. }
            | Self::Type { span, .. } => *span,
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

/// A segment in a binary pattern or a binary construction.
///
/// Example: In `<<version:8, data/binary>>`, `version:8` and `data/binary` are segments.
///
/// One segment shape serves both directions (ADR 0143): the value is a
/// [`Pattern`] in a match arm (the default, `BinarySegment`) and an
/// [`Expression`] in a construction (`BinarySegment<Expression>`, held by
/// [`Expression::BinaryConstruction`](crate::ast::Expression::BinaryConstruction)).
/// The size and the `/type-signedness-endianness-unit:N` specifiers are the
/// same fields, so there is one specifier vocabulary, not two.
#[derive(Debug, Clone, PartialEq)]
pub struct BinarySegment<V = Pattern> {
    /// The value or variable: a [`Pattern`] when matching, an
    /// [`Expression`] when constructing.
    pub value: V,
    /// Size specifier (e.g., `8`, `16`).
    pub size: Option<Box<Expression>>,
    /// Type specifier (e.g., `binary`, `integer`, `float`).
    pub segment_type: Option<BinarySegmentType>,
    /// Signedness (for integer types).
    pub signedness: Option<BinarySignedness>,
    /// Endianness.
    pub endianness: Option<BinaryEndianness>,
    /// Unit override from an explicit `/unit:N` specifier (BT-4143); `None`
    /// means the type default. Codegen also sets it on synthetic split
    /// tails (BT-4096).
    pub unit: Option<usize>,
    /// Source location.
    pub span: Span,
}

/// A binary segment's value as the literal checks and the lowering see it,
/// in either direction (ADR 0143 § Literal checks): one view, so a literal
/// reads the same in a pattern and in a construction.
pub trait BinarySegmentValue {
    /// The literal this value is, with its span: a `Pattern::Literal`, or an
    /// `Expression::Literal` through any parentheses (`<<(-1):4>>`).
    fn binary_literal(&self) -> Option<(&Literal, Span)>;

    /// The value's source span.
    fn binary_value_span(&self) -> Span;
}

impl BinarySegmentValue for Pattern {
    fn binary_literal(&self) -> Option<(&Literal, Span)> {
        match self {
            Pattern::Literal(lit, span) => Some((lit, *span)),
            _ => None,
        }
    }

    fn binary_value_span(&self) -> Span {
        self.span()
    }
}

impl BinarySegmentValue for Expression {
    fn binary_literal(&self) -> Option<(&Literal, Span)> {
        match self.unwrap_parens() {
            Expression::Literal(lit, span) => Some((lit, *span)),
            _ => None,
        }
    }

    fn binary_value_span(&self) -> Span {
        self.span()
    }
}

/// An integer literal size and its span, through any parentheses
/// (`<<v:(8)>>`, BT-4097). `None` for any other size expression.
#[must_use]
pub fn binary_literal_size(size: &Expression) -> Option<(i64, Span)> {
    match size.unwrap_parens() {
        Expression::Literal(Literal::Integer(n), span) => Some((*n, *span)),
        _ => None,
    }
}

impl<V> BinarySegment<V> {
    /// The segment's type: the explicit one, or `integer` (the default).
    #[must_use]
    pub fn effective_type(&self) -> BinarySegmentType {
        self.segment_type.unwrap_or(BinarySegmentType::Integer)
    }

    /// The segment's unit: the explicit `unit:N`, or its type's default (8
    /// for `binary`, 1 for `integer`/`float`/`bits`, none for `utf*`).
    #[must_use]
    pub fn effective_unit(&self) -> Option<u64> {
        match (self.unit, self.effective_type()) {
            (Some(unit), _) => u64::try_from(unit).ok(),
            (
                None,
                BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32,
            ) => None,
            (None, BinarySegmentType::Binary) => Some(8),
            (
                None,
                BinarySegmentType::Integer
                | BinarySegmentType::Float
                | BinarySegmentType::Bitstring,
            ) => Some(1),
        }
    }

    /// The segment's bit size when the source fixes it: a literal size ×
    /// the unit, or the default size of an `integer` (8) or `float` (64)
    /// segment with no size. `None` for a variable or computed size, a
    /// sizeless `binary`/`bits` segment (its value's size) and `utf*`
    /// segments (the character's encoding), and for a negative size (an
    /// error the parser reports).
    #[must_use]
    pub fn literal_bit_size(&self) -> Option<u64> {
        match &self.size {
            Some(size) => {
                let (n, _) = binary_literal_size(size)?;
                u64::try_from(n).ok()?.checked_mul(self.effective_unit()?)
            }
            None => match self.effective_type() {
                BinarySegmentType::Integer => Some(8),
                BinarySegmentType::Float => Some(64),
                _ => None,
            },
        }
    }
}

impl BinarySegment<Expression> {
    /// A construction segment's sub-expressions in evaluation order: the
    /// value, then the size (when present).
    ///
    /// The single place that enumerates a construction segment's children,
    /// so every walker visits the same expressions.
    pub fn sub_expressions(&self) -> impl Iterator<Item = &Expression> {
        std::iter::once(&self.value).chain(self.size.as_deref())
    }

    /// Mutable counterpart of [`Self::sub_expressions`].
    pub fn sub_expressions_mut(&mut self) -> impl Iterator<Item = &mut Expression> {
        std::iter::once(&mut self.value).chain(self.size.as_deref_mut())
    }
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
    /// Bitstring (possibly unaligned bits; BT-4097).
    Bitstring,
    /// UTF-8 string.
    Utf8,
    /// UTF-16 string (BT-4143).
    Utf16,
    /// UTF-32 string (BT-4143).
    Utf32,
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
