// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unit tests for `block_facts`, split by analysis.

mod escape;
mod mutation;
mod producers;

use super::*;
use crate::ast::{
    Block, BlockParameter, Expression, ExpressionStatement, Identifier, MessageSelector,
};
use crate::source_analysis::Span;
use std::collections::HashSet;

pub(super) fn make_id(name: &str) -> Identifier {
    Identifier::new(name, Span::new(0, u32::try_from(name.len()).unwrap_or(0)))
}

pub(super) fn make_expr_id(name: &str) -> Expression {
    Expression::Identifier(make_id(name))
}

pub(super) use crate::test_helpers::test_support::bare;

pub(super) fn parse_first_expr(src: &str) -> Expression {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = crate::source_analysis::parse(tokens);
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    module
        .expressions
        .into_iter()
        .next()
        .expect("one expression")
        .expression
}
