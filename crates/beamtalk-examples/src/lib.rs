// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;

pub mod class_corpus;
mod class_search;
pub mod corpus;
mod search;

pub use class_corpus::{ClassCorpus, ClassEntry, load_class_corpus_from_file, merge_class_corpora};
pub use class_search::{ClassSearchResult, search_classes, search_classes_in};
pub use corpus::{Corpus, CorpusEntry, load_corpus_from_file, merge_corpora};
pub use search::{SearchResult, search, search_in};

/// Generic merge-by-key for corpus-like collections.
///
/// Given a base slice and extra slices of entries, deduplicates by the key
/// returned by `key_fn` (later entries override earlier ones with the same key),
/// then sorts the result by that same key.
pub(crate) fn merge_by_key<E, K, F>(base: &[E], extras: &[&[E]], key_fn: F) -> Vec<E>
where
    E: Clone,
    K: Eq + std::hash::Hash + Ord + ?Sized,
    F: Fn(&E) -> &K,
{
    let mut by_key: HashMap<&K, &E> = HashMap::new();

    for entry in base {
        by_key.insert(key_fn(entry), entry);
    }
    for slice in extras {
        for entry in *slice {
            by_key.insert(key_fn(entry), entry);
        }
    }

    let mut entries: Vec<E> = by_key.values().map(|e| (*e).clone()).collect();
    entries.sort_by(|a, b| key_fn(a).cmp(key_fn(b)));
    entries
}

#[cfg(test)]
mod merge_by_key_tests {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    struct Item {
        key: String,
        value: i32,
    }

    fn item(key: &str, value: i32) -> Item {
        Item {
            key: key.to_string(),
            value,
        }
    }

    #[test]
    fn empty_base_and_extras() {
        let base: Vec<Item> = vec![];
        let result = merge_by_key(&base, &[], |e| e.key.as_str());
        assert!(result.is_empty());
    }

    #[test]
    fn base_only() {
        let base = vec![item("b", 1), item("a", 2)];
        let result = merge_by_key(&base, &[], |e| e.key.as_str());
        assert_eq!(result, vec![item("a", 2), item("b", 1)]);
    }

    #[test]
    fn extras_override_base() {
        let base = vec![item("a", 1)];
        let extras = vec![item("a", 99)];
        let result = merge_by_key(&base, &[extras.as_slice()], |e| e.key.as_str());
        assert_eq!(result, vec![item("a", 99)]);
    }

    #[test]
    fn multiple_extras_merged() {
        let base = vec![item("a", 1)];
        let e1 = vec![item("b", 2)];
        let e2 = vec![item("c", 3)];
        let result = merge_by_key(&base, &[e1.as_slice(), e2.as_slice()], |e| e.key.as_str());
        assert_eq!(result, vec![item("a", 1), item("b", 2), item("c", 3)]);
    }

    #[test]
    fn later_extras_override_earlier() {
        let base = vec![item("a", 1)];
        let e1 = vec![item("a", 2)];
        let e2 = vec![item("a", 3)];
        let result = merge_by_key(&base, &[e1.as_slice(), e2.as_slice()], |e| e.key.as_str());
        assert_eq!(result, vec![item("a", 3)]);
    }
}
