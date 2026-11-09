// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method definition AST nodes for Beamtalk.
//!
//! This module contains [`MethodDefinition`], [`ParameterDefinition`],
//! and [`MethodKind`].

use crate::span::Span;
use ecow::EcoString;

use super::CommentAttachment;
use super::ExpressionStatement;
use super::expression::{
    ExpectCategory, Expression, Identifier, Literal, MessageSelector, TypeAnnotation,
};

/// A literal a method body can be exactly, and that a class-literal send of
/// the method may answer in place (BT-4363, BT-4504): every literal kind with
/// a single typed Core Erlang leaf. Compound literals (`#(...)` lists, maps,
/// arrays) are not constants here.
///
/// The runtime certifies a folded send by exact equality (`=:=`) of the
/// value the call site answers with the one the class's metadata row
/// publishes, so the value is compared as the term it compiles to: `Float`
/// compares by bit pattern (`1.0` is not `1`, and `0.0` is not `-0.0`).
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ConstantValue {
    /// `42`
    Integer(i64),
    /// `1.5`
    Float(f64),
    /// `"text"`
    String(EcoString),
    /// `#name`
    Symbol(EcoString),
    /// `$a`
    Character(char),
    /// `nil`
    Nil,
    /// `true`
    True,
    /// `false`
    False,
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a.to_bits() == b.to_bits(),
            (Self::String(a), Self::String(b)) | (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::Nil, Self::Nil) | (Self::True, Self::True) | (Self::False, Self::False) => true,
            _ => false,
        }
    }
}

// Bit-pattern float equality is reflexive, so this is a full equivalence.
impl Eq for ConstantValue {}

impl ConstantValue {
    /// The constant a scalar literal is; `None` for a compound literal.
    #[must_use]
    pub fn from_literal(literal: &Literal) -> Option<Self> {
        match literal {
            Literal::Integer(value) => Some(Self::Integer(*value)),
            Literal::Float(value) => Some(Self::Float(*value)),
            Literal::String(value) => Some(Self::String(value.clone())),
            Literal::Symbol(value) => Some(Self::Symbol(value.clone())),
            Literal::Character(value) => Some(Self::Character(*value)),
            Literal::List(_) => None,
        }
    }

    /// The constant a reserved pseudo-variable (`nil`, `true`, `false`) is.
    #[must_use]
    pub fn from_pseudo_variable(name: &str) -> Option<Self> {
        match name {
            "nil" => Some(Self::Nil),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            _ => None,
        }
    }

    /// The literal this constant is written as, so codegen lowers it through
    /// the one literal lowering, or `Err` with the name of the reserved
    /// pseudo-variable (`nil`, `true`, `false`) it is written as.
    ///
    /// # Errors
    ///
    /// `Err(name)` for [`Self::Nil`], [`Self::True`] and [`Self::False`],
    /// which are pseudo-variables rather than literals.
    pub fn as_literal(&self) -> Result<Literal, &'static str> {
        match self {
            Self::Integer(value) => Ok(Literal::Integer(*value)),
            Self::Float(value) => Ok(Literal::Float(*value)),
            Self::String(value) => Ok(Literal::String(value.clone())),
            Self::Symbol(value) => Ok(Literal::Symbol(value.clone())),
            Self::Character(value) => Ok(Literal::Character(*value)),
            Self::Nil => Err("nil"),
            Self::True => Err("true"),
            Self::False => Err("false"),
        }
    }
}

/// Modifier flags for a method definition.
///
/// All flags default to `false` and kind defaults to `Primary`,
/// so callers can use struct update syntax:
/// ```ignore
/// MethodModifiers { is_sealed: true, ..Default::default() }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct MethodModifiers {
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    pub is_internal: bool,
    /// Whether this is a class-side method (the `class ` modifier).
    pub is_class_method: bool,
    /// The kind of method.
    pub kind: MethodKind,
}

/// The kind of method.
///
/// Currently only Primary methods exist. Additional kinds (e.g., AOP-style
/// before/after/around) may be added in the future if needed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    pub is_internal: bool,
    /// Whether this is a class-side method (declared with the `class ` modifier).
    ///
    /// Carried on the method itself — rather than implied only by living in a
    /// class's `class_methods` — so that the per-method source [`unparse_method`]
    /// produces re-emits the `class ` prefix. That keeps the stored/compiled
    /// `source_ref` byte-identical to the on-disk method span for class-side
    /// methods (ADR 0082). Whole-file unparse adds the prefix from its
    /// own context, so it ignores this flag.
    pub is_class_method: bool,
    /// The kind of method.
    pub kind: MethodKind,
    /// Optional `@expect` directive attached to this method declaration.
    ///
    /// When present, diagnostics matching any of these categories that fire
    /// on the method declaration are suppressed (a single directive
    /// may list more than one category, e.g. `@expect unresolved_ffi, type`).
    /// If none of the categories match a diagnostic, a "stale @expect"
    /// warning is emitted. The span is the source location of the
    /// `@expect category` directive itself (for stale warnings). The
    /// optional reason string is included in stale warnings for context.
    pub expect: Option<(Vec<ExpectCategory>, Option<EcoString>, Span)>,
    /// Non-doc comments (`//` and `/* */`) appearing before this method.
    pub comments: CommentAttachment,
    /// Doc comment attached to this method (`///` lines).
    pub doc_comment: Option<String>,
    /// The sends in this method's body that were written with `|>`
    /// (ADR 0145), so the unparser can print the body as written.
    pub pipeline_sends: super::PipelineSends,
    /// Source location.
    pub span: Span,
}

impl MethodDefinition {
    /// Returns `true` if the method body is exactly `self delegate`.
    ///
    /// This is the marker pattern for native facade classes (ADR 0056): the method
    /// body delegates to the backing `gen_server` rather than containing Beamtalk logic.
    #[must_use]
    pub fn is_self_delegate(&self) -> bool {
        if self.body.len() != 1 {
            return false;
        }
        matches!(
            &self.body[0].expression,
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Unary(sel),
                arguments,
                ..
            } if arguments.is_empty()
                && sel.as_str() == "delegate"
                && matches!(receiver.as_ref(), Expression::Identifier(Identifier { name, .. }) if name.as_str() == "self")
        )
    }

    /// The span of the declaration head: from the start of the method up to
    /// its first body statement, or the whole method when the body is empty.
    ///
    /// [`Self::span`] covers the body too, which for a multi-line body
    /// underlines far more than a declaration-site diagnostic is about.
    /// Narrowing to the head puts the caret on the selector.
    #[must_use]
    pub fn head_span(&self) -> Span {
        self.body.first().map_or(self.span, |first| {
            Span::new(self.span.start(), first.expression.span().start())
        })
    }

    /// The primitive name when the method body is exactly one quoted
    /// `@primitive "<name>"` pragma (e.g. `class signal: msg => @primitive
    /// "classSignal:"`), else `None`.
    ///
    /// Bare structural intrinsics (`@primitive basicNew`) answer `None`: only
    /// a quoted name identifies a selector-keyed runtime primitive.
    #[must_use]
    pub fn quoted_primitive_name(&self) -> Option<&EcoString> {
        match self.body.as_slice() {
            [stmt] => match &stmt.expression {
                Expression::Primitive {
                    name,
                    is_quoted: true,
                    ..
                } => Some(name),
                _ => None,
            },
            _ => None,
        }
    }

    /// The literal when the method body is exactly one literal of a kind
    /// [`ConstantValue`] covers (the named-constant idiom, `class
    /// protocolError => 1`, `class tag => #tag`, `class name => "x"`), else
    /// `None`.
    ///
    /// Codegen certifies it in the defining module's `__beamtalk_meta/0`
    /// (`constant_class_methods`) and a class-literal send of the selector
    /// answers it behind a static call key that the runtime installs only
    /// while that metadata still maps the selector to this value (BT-4363,
    /// generalised from integers in BT-4504).
    #[must_use]
    pub fn literal_constant_body(&self) -> Option<ConstantValue> {
        match self.body.as_slice() {
            [stmt] => match &stmt.expression {
                Expression::Literal(literal, _) => ConstantValue::from_literal(literal),
                Expression::Identifier(id) => ConstantValue::from_pseudo_variable(&id.name),
                _ => None,
            },
            _ => None,
        }
    }

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
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            pipeline_sends: super::PipelineSends::default(),
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
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            pipeline_sends: super::PipelineSends::default(),
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
        modifiers: MethodModifiers,
        span: Span,
    ) -> Self {
        Self {
            selector,
            parameters,
            body,
            return_type,
            is_sealed: modifiers.is_sealed,
            is_internal: modifiers.is_internal,
            is_class_method: modifiers.is_class_method,
            kind: modifiers.kind,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            pipeline_sends: super::PipelineSends::default(),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ConstantValue;
    use crate::source_analysis::{lex_with_eof, parse};

    /// `literal_constant_body` of the class-side `selector` of the one class in `src`.
    fn class_method_constant(src: &str, selector: &str) -> Option<ConstantValue> {
        let (module, diagnostics) = parse(lex_with_eof(src));
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        module.classes[0]
            .class_methods
            .iter()
            .find(|m| m.selector.name() == selector)
            .expect("method")
            .literal_constant_body()
    }

    #[test]
    fn literal_constant_body_is_only_a_lone_scalar_literal() {
        let src = "Object subclass: Errs\n  \
                   class one => 1\n  \
                   class minus => -7\n  \
                   class sum => 1 + 1\n  \
                   class float => 1.5\n  \
                   class str => \"hi\"\n  \
                   class sym => #one\n  \
                   class char => $a\n  \
                   class none => nil\n  \
                   class yes => true\n  \
                   class no => false\n  \
                   class list => #(1, 2)\n  \
                   class interp => \"a{1}\"\n  \
                   class selfish => self\n  \
                   class two =>\n    1\n    2\n";
        assert_eq!(
            class_method_constant(src, "one"),
            Some(ConstantValue::Integer(1))
        );
        assert_eq!(
            class_method_constant(src, "minus"),
            Some(ConstantValue::Integer(-7))
        );
        assert_eq!(class_method_constant(src, "sum"), None);
        assert_eq!(
            class_method_constant(src, "float"),
            Some(ConstantValue::Float(1.5))
        );
        assert_eq!(
            class_method_constant(src, "str"),
            Some(ConstantValue::String("hi".into()))
        );
        assert_eq!(
            class_method_constant(src, "sym"),
            Some(ConstantValue::Symbol("one".into()))
        );
        assert_eq!(
            class_method_constant(src, "char"),
            Some(ConstantValue::Character('a'))
        );
        assert_eq!(class_method_constant(src, "none"), Some(ConstantValue::Nil));
        assert_eq!(class_method_constant(src, "yes"), Some(ConstantValue::True));
        assert_eq!(class_method_constant(src, "no"), Some(ConstantValue::False));
        assert_eq!(class_method_constant(src, "list"), None);
        assert_eq!(class_method_constant(src, "interp"), None);
        assert_eq!(class_method_constant(src, "selfish"), None);
        assert_eq!(class_method_constant(src, "two"), None);
    }

    #[test]
    fn constant_values_compare_as_the_terms_they_compile_to() {
        assert_ne!(ConstantValue::Integer(1), ConstantValue::Float(1.0));
        assert_ne!(ConstantValue::Float(0.0), ConstantValue::Float(-0.0));
        assert_eq!(ConstantValue::Float(1.5), ConstantValue::Float(1.5));
        assert_ne!(
            ConstantValue::String("a".into()),
            ConstantValue::Symbol("a".into())
        );
    }
}
