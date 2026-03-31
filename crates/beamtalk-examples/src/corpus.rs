// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::sync::LazyLock;

use serde::{Deserialize, Serialize};

/// A single example in the corpus.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CorpusEntry {
    /// Unique identifier (e.g. "closures-value-capture").
    pub id: String,
    /// Human-readable title.
    pub title: String,
    /// Top-level category for grouping.
    pub category: String,
    /// Searchable tags — class names, selector names, concepts.
    pub tags: Vec<String>,
    /// Beamtalk source code (the example itself).
    pub source: String,
    /// Brief explanation of what the example demonstrates.
    pub explanation: String,
}

/// The complete example corpus.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Corpus {
    pub entries: Vec<CorpusEntry>,
}

static CORPUS_BYTES: &[u8] = include_bytes!("../corpus.json");

/// The lazily-deserialized corpus singleton.
pub static CORPUS: LazyLock<Corpus> = LazyLock::new(|| {
    serde_json::from_slice(CORPUS_BYTES).expect("embedded corpus.json must be valid")
});

/// Load a corpus from a JSON file on disk.
///
/// Returns `None` if the file does not exist or cannot be parsed.
pub fn load_corpus_from_file(path: &std::path::Path) -> Option<Corpus> {
    let bytes = std::fs::read(path).ok()?;
    serde_json::from_slice(&bytes).ok()
}

/// Merge multiple corpora into a single corpus, deduplicating by entry ID.
///
/// Entries from later corpora override earlier ones with the same ID.
pub fn merge_corpora(base: &Corpus, extras: &[Corpus]) -> Corpus {
    let extra_slices: Vec<&[CorpusEntry]> = extras.iter().map(|c| c.entries.as_slice()).collect();
    let entries = crate::merge_by_key(&base.entries, &extra_slices, |e| e.id.as_str());
    Corpus { entries }
}

#[cfg(test)]
mod merge_tests {
    use super::*;

    fn make_entry(id: &str, title: &str) -> CorpusEntry {
        CorpusEntry {
            id: id.to_string(),
            title: title.to_string(),
            category: "test".to_string(),
            tags: vec![],
            source: String::new(),
            explanation: String::new(),
        }
    }

    #[test]
    fn merge_empty_extras() {
        let base = Corpus {
            entries: vec![make_entry("a", "A")],
        };
        let merged = merge_corpora(&base, &[]);
        assert_eq!(merged.entries.len(), 1);
        assert_eq!(merged.entries[0].id, "a");
    }

    #[test]
    fn merge_adds_new_entries() {
        let base = Corpus {
            entries: vec![make_entry("a", "A")],
        };
        let extra = Corpus {
            entries: vec![make_entry("b", "B")],
        };
        let merged = merge_corpora(&base, &[extra]);
        assert_eq!(merged.entries.len(), 2);
    }

    #[test]
    fn merge_deduplicates_by_id() {
        let base = Corpus {
            entries: vec![make_entry("a", "Base A")],
        };
        let extra = Corpus {
            entries: vec![make_entry("a", "Extra A")],
        };
        let merged = merge_corpora(&base, &[extra]);
        assert_eq!(merged.entries.len(), 1);
        assert_eq!(merged.entries[0].title, "Extra A"); // Extra overrides base
    }

    #[test]
    fn merge_sorts_by_id() {
        let base = Corpus {
            entries: vec![make_entry("c", "C")],
        };
        let extra = Corpus {
            entries: vec![make_entry("a", "A"), make_entry("b", "B")],
        };
        let merged = merge_corpora(&base, &[extra]);
        let ids: Vec<&str> = merged.entries.iter().map(|e| e.id.as_str()).collect();
        assert_eq!(ids, vec!["a", "b", "c"]);
    }

    #[test]
    fn load_from_nonexistent_file_returns_none() {
        let result = load_corpus_from_file(std::path::Path::new("/nonexistent/corpus.json"));
        assert!(result.is_none());
    }
}
