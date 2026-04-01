// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method definition AST nodes for Beamtalk.
//!
//! This module contains [`MethodDefinition`], [`ParameterDefinition`],
//! and [`MethodKind`].

use crate::source_analysis::Span;

use super::CommentAttachment;
use super::ExpressionStatement;
use super::expression::{Expression, Identifier, MessageSelector, TypeAnnotation};

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
            is_internal: false,
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
            is_internal: false,
            kind,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }
}
