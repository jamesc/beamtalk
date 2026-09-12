// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for `gen_server` method code generation.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`source_extraction`] — `extract_method_source` return-type annotation
//!   stripping
//! - [`dispatch_generation`] — `generate_method_dispatch`/
//!   `generate_class_method_dispatches`/`generate_class_method_functions`
//!
//! `project_recv_type` (formerly `recv_type_projection`), `__beamtalk_meta`
//! map construction (formerly `meta_map`/`meta_type_repr`), synthetic
//! value-accessor metadata (formerly `synthetic_accessors`), and
//! `generate_register_class` moved with their production code to
//! `xref::tests`, `class_meta::tests`, `value_accessors::tests`, and
//! `class_registry::tests` respectively.

use super::*;
use beamtalk_core::ast::{Expression, Literal, MessageSelector, MethodDefinition};
use beamtalk_core::source_analysis::Span;

use crate::core_erlang::tests::bare;

pub(crate) fn s() -> Span {
    Span::new(0, 0)
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
mod source_extraction;
