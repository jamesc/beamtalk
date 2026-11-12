// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for the span-keyed side tables of a definition
//! ([`PipelineSends`](super::PipelineSends),
//! [`LiteralComments`](super::LiteralComments)).
//!
//! **DDD Context:** Source Analysis
//!
//! Both tables key their entries by `(start, end)` of a node's span and both
//! must answer "which entries lie inside this span?" for a definition rebuilt
//! from part of another one's code (a `ClassBuilder` block method). The key
//! and that containment rule live here once, so the tables cannot disagree
//! about which entries belong to a rebuilt method.

use std::collections::BTreeMap;

use crate::span::Span;

/// The map key for the node spanning `span`.
pub(super) fn span_key(span: Span) -> (u32, u32) {
    (span.start(), span.end())
}

/// The entries of `table` whose keys lie inside `span` (start at or after
/// `span.start()`, end at or before `span.end()`).
pub(super) fn entries_within<V: Clone>(
    table: &BTreeMap<(u32, u32), V>,
    span: Span,
) -> BTreeMap<(u32, u32), V> {
    table
        .range((span.start(), span.start())..=(span.end(), span.end()))
        .filter(|&(&(_, end), _)| end <= span.end())
        .map(|(&key, value)| (key, value.clone()))
        .collect()
}
