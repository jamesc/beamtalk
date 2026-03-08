// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for Core Erlang code generation.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`expressions`] — literals, binary ops, maps, string interpolation
//! - [`dispatch`] — message sends, futures, spawn, erlang interop, cascades
//! - [`control_flow`] — loops, stored closures, if-true/false conditionals
//! - [`gen_server`] — module/class codegen, REPL modules, value subclasses
//! - [`primitives`] — primitive selector and intrinsic codegen

pub use super::*;
pub use crate::ast::*;
pub use crate::source_analysis::Span;

pub(crate) fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

/// Builds a Module with a `Value subclass: Point` with x and y slots.
pub(crate) fn make_value_subclass_point() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

mod control_flow;
mod dispatch;
mod expressions;
mod gen_server;
mod primitives;
mod supervisor;
