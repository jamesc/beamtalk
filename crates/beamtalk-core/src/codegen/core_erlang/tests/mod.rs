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
//! - [`branch_context`] — BT-3131: `with_branch_context`'s per-prefix
//!   save/reset/restore discipline (state/`class_vars`/self)
//! - [`class_var_shadow_contract`] — BT-3135 (ADR 0111 Phase D): the
//!   cross-boundary ADR 0110 shadow-write key conformance fixture, asserted
//!   against `runtime/apps/beamtalk_runtime/include/beamtalk.hrl` and the
//!   `beamtalk_class_dispatch_tests.erl` `EUnit` suite
//! - [`recv_type`] — BT-3217 (ADR 0115 Phase 2): the xref `recv_type`
//!   write-path fixture matrix (typed/protocol/dynamic/union/native/alias
//!   locals, `Meta{C}`, self-send, FFI receiver)

pub use super::*;
pub use crate::ast::*;
pub use crate::source_analysis::Span;

pub(crate) fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

/// Parse `src` as a single-class Beamtalk module and run codegen in workspace mode.
///
/// Shared by control-flow and dispatch test modules to avoid copy-pasting this
/// boilerplate across every sub-module.
pub(crate) fn codegen(src: &str) -> String {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed")
}

/// Builds a Module with a `Value subclass: Point` with x and y slots.
pub(crate) fn make_value_subclass_point() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

mod analysis_handoff;
mod branch_context;
mod class_var_shadow_contract;
mod control_flow;
mod dispatch;
mod expressions;
mod gen_server;
mod primitives;
mod recv_type;
mod supervisor;
