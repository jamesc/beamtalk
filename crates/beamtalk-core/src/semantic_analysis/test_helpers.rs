// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for `semantic_analysis` tests.

use crate::source_analysis::Span;

pub fn test_span() -> Span {
    Span::new(0, 0)
}
