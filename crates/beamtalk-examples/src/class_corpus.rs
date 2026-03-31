// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::sync::LazyLock;

use serde::{Deserialize, Serialize};

/// A single class entry in the class corpus.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ClassEntry {
    /// Class name (e.g. "String", "Actor").
    pub name: String,
    /// Direct superclass name (e.g. "Collection", "Object").
    pub superclass: String,
    /// One-line description of the class.
    pub doc: Option<String>,
    /// Method selectors defined on this class.
    pub methods: Vec<String>,
    /// Whether the class is sealed (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether the class is abstract (cannot be instantiated directly).
    pub is_abstract: bool,
}

/// The complete class corpus.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ClassCorpus {
    pub entries: Vec<ClassEntry>,
}

static CLASS_CORPUS_BYTES: &[u8] = include_bytes!("../class_corpus.json");

/// The lazily-deserialized class corpus singleton.
pub static CLASS_CORPUS: LazyLock<ClassCorpus> = LazyLock::new(|| {
    let entries: Vec<ClassEntry> = serde_json::from_slice(CLASS_CORPUS_BYTES)
        .expect("embedded class_corpus.json must be valid");
    ClassCorpus { entries }
});

/// Load a class corpus from a JSON file on disk.
///
/// Returns `None` if the file does not exist or cannot be parsed.
pub fn load_class_corpus_from_file(path: &std::path::Path) -> Option<ClassCorpus> {
    let bytes = std::fs::read(path).ok()?;
    let entries: Vec<ClassEntry> = serde_json::from_slice(&bytes).ok()?;
    Some(ClassCorpus { entries })
}

/// Merge multiple class corpora into a single corpus, deduplicating by class name.
///
/// Entries from later corpora override earlier ones with the same name.
pub fn merge_class_corpora(base: &ClassCorpus, extras: &[ClassCorpus]) -> ClassCorpus {
    let extra_slices: Vec<&[ClassEntry]> = extras.iter().map(|c| c.entries.as_slice()).collect();
    let entries = crate::merge_by_key(&base.entries, &extra_slices, |e| e.name.as_str());
    ClassCorpus { entries }
}

#[cfg(test)]
mod merge_tests {
    use super::*;

    fn make_class(name: &str) -> ClassEntry {
        ClassEntry {
            name: name.to_string(),
            superclass: "Object".to_string(),
            doc: None,
            methods: vec![],
            is_sealed: false,
            is_abstract: false,
        }
    }

    #[test]
    fn merge_empty_extras() {
        let base = ClassCorpus {
            entries: vec![make_class("Foo")],
        };
        let merged = merge_class_corpora(&base, &[]);
        assert_eq!(merged.entries.len(), 1);
    }

    #[test]
    fn merge_adds_new_classes() {
        let base = ClassCorpus {
            entries: vec![make_class("Foo")],
        };
        let extra = ClassCorpus {
            entries: vec![make_class("Bar")],
        };
        let merged = merge_class_corpora(&base, &[extra]);
        assert_eq!(merged.entries.len(), 2);
    }

    #[test]
    fn merge_deduplicates_by_name() {
        let base = ClassCorpus {
            entries: vec![make_class("Foo")],
        };
        let mut updated = make_class("Foo");
        updated.methods = vec!["greet:".to_string()];
        let extra = ClassCorpus {
            entries: vec![updated],
        };
        let merged = merge_class_corpora(&base, &[extra]);
        assert_eq!(merged.entries.len(), 1);
        assert_eq!(merged.entries[0].methods, vec!["greet:"]);
    }

    #[test]
    fn load_from_nonexistent_file_returns_none() {
        let result =
            load_class_corpus_from_file(std::path::Path::new("/nonexistent/class_corpus.json"));
        assert!(result.is_none());
    }
}
