// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for `gen_server` method code generation.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`source_extraction`] — `extract_method_source` return-type annotation
//!   stripping
//! - [`recv_type_projection`] — ADR 0115 Phase 2: `project_recv_type` unit
//!   coverage
//! - [`dispatch_generation`] — `generate_register_class`/
//!   `generate_method_dispatch`/`generate_class_method_dispatches`/
//!   `generate_class_method_functions`
//! - [`meta_type_repr`] — ADR 0068 `MetaTypeRepr` conversion and rendering
//! - [`meta_map`] — ADR 0098 `__beamtalk_meta` map construction (provenance,
//!   type params, package, kind, visibility)
//! - [`synthetic_accessors`] — synthetic value-accessor doc/signature
//!   metadata

use super::*;
use beamtalk_core::ast::{
    Expression, ExpressionStatement, Literal, MessageSelector, MethodDefinition,
};
use beamtalk_core::source_analysis::Span;

pub(crate) fn s() -> Span {
    Span::new(0, 0)
}

pub(crate) fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

pub(crate) fn simple_unary_method(selector: &str) -> MethodDefinition {
    MethodDefinition::new(
        MessageSelector::Unary(selector.into()),
        vec![],
        vec![bare(Expression::Literal(Literal::Integer(42), s()))],
        s(),
    )
}

mod dispatch_generation;
mod meta_map;
mod meta_type_repr;
mod recv_type_projection;
mod source_extraction;
mod synthetic_accessors;
