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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn make_table(entries: &[(u32, u32)]) -> BTreeMap<(u32, u32), &'static str> {
        entries.iter().copied().map(|key| (key, "v")).collect()
    }

    #[test]
    fn span_key_returns_start_and_end() {
        assert_eq!(span_key(Span::new(10, 20)), (10, 20));
    }

    #[test]
    fn span_key_zero_length_span() {
        assert_eq!(span_key(Span::new(5, 5)), (5, 5));
    }

    #[test]
    fn entries_within_empty_table() {
        let table: BTreeMap<(u32, u32), i32> = BTreeMap::new();
        assert!(entries_within(&table, Span::new(0, 100)).is_empty());
    }

    #[test]
    fn entries_within_includes_entry_fully_inside() {
        let table = make_table(&[(5, 15)]);
        let result = entries_within(&table, Span::new(0, 20));
        assert_eq!(result.len(), 1);
        assert!(result.contains_key(&(5, 15)));
    }

    #[test]
    fn entries_within_includes_entry_at_exact_boundaries() {
        let table = make_table(&[(10, 30)]);
        let result = entries_within(&table, Span::new(10, 30));
        assert_eq!(result.len(), 1);
        assert!(result.contains_key(&(10, 30)));
    }

    #[test]
    fn entries_within_excludes_entry_starting_before_span() {
        // start < span.start → outside
        let table = make_table(&[(5, 15)]);
        assert!(entries_within(&table, Span::new(10, 30)).is_empty());
    }

    #[test]
    fn entries_within_excludes_entry_ending_after_span() {
        // end > span.end → outside
        let table = make_table(&[(15, 35)]);
        assert!(entries_within(&table, Span::new(10, 30)).is_empty());
    }

    #[test]
    fn entries_within_excludes_entry_fully_outside() {
        let table = make_table(&[(40, 50)]);
        assert!(entries_within(&table, Span::new(10, 30)).is_empty());
    }

    #[test]
    fn entries_within_mixed_entries_returns_only_contained_ones() {
        let table = make_table(&[
            (5, 10),  // starts before span — excluded
            (10, 20), // starts at span boundary, ends inside — included
            (12, 25), // fully inside — included
            (20, 35), // ends after span — excluded
            (40, 50), // fully outside — excluded
        ]);
        let result = entries_within(&table, Span::new(10, 30));
        assert_eq!(result.len(), 2);
        assert!(result.contains_key(&(10, 20)));
        assert!(result.contains_key(&(12, 25)));
    }

    #[test]
    fn entries_within_zero_length_query_span() {
        let table = make_table(&[(5, 5), (10, 10), (15, 15)]);
        let result = entries_within(&table, Span::new(10, 10));
        assert_eq!(result.len(), 1);
        assert!(result.contains_key(&(10, 10)));
    }

    #[test]
    fn entries_within_clones_values() {
        let mut table = BTreeMap::new();
        table.insert((5, 15), "inside".to_string());
        let result = entries_within(&table, Span::new(0, 20));
        assert_eq!(result[&(5, 15)], "inside");
    }
}
