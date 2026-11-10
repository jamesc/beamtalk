// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::path::{Path, PathBuf};
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

/// Where the bundled corpus lives, relative to an installation sysroot
/// (`{sysroot}/bin/beamtalk-mcp` → `{sysroot}/share/beamtalk/corpus.json`).
///
/// The one definition of that location (BT-4614): `build-corpus` writes the
/// file here under the staging sysroot `target/` (so a dev binary in
/// `target/debug/` or `target/release/` finds it), `just install` copies
/// `target/share/` into the install prefix as is, and [`bundled_corpus`]
/// reads it from the running binary's sysroot.
pub const BUNDLED_CORPUS_RELPATH: &str = "share/beamtalk/corpus.json";

/// The bundled corpus file for an installation (or staging) sysroot.
#[must_use]
pub fn bundled_corpus_path(sysroot: &Path) -> PathBuf {
    sysroot.join(BUNDLED_CORPUS_RELPATH)
}

/// The bundled example corpus, generated at build time rather than checked
/// in (BT-4614), or where it was looked for when it could not be loaded.
#[derive(Debug)]
pub enum BundledCorpus {
    Loaded(Corpus),
    /// No readable corpus. `path` is where it was looked for, or `None`
    /// when the running binary's sysroot could not be derived.
    Missing {
        path: Option<PathBuf>,
    },
}

static EMPTY_CORPUS: Corpus = Corpus {
    entries: Vec::new(),
};

impl BundledCorpus {
    /// Load the bundled corpus of the installation rooted at `sysroot`.
    #[must_use]
    pub fn load(sysroot: Option<&Path>) -> Self {
        let Some(sysroot) = sysroot else {
            return Self::Missing { path: None };
        };
        let path = bundled_corpus_path(sysroot);
        match load_corpus_from_file(&path) {
            Some(corpus) => Self::Loaded(corpus),
            None => Self::Missing { path: Some(path) },
        }
    }

    /// The loaded corpus, or an empty one when it is missing.
    #[must_use]
    pub fn corpus(&self) -> &Corpus {
        match self {
            Self::Loaded(corpus) => corpus,
            Self::Missing { .. } => &EMPTY_CORPUS,
        }
    }
}

static BUNDLED: LazyLock<BundledCorpus> =
    LazyLock::new(|| BundledCorpus::load(beamtalk_sysroot::current_sysroot().as_deref()));

/// The running binary's bundled corpus, loaded once on first use.
pub fn bundled_corpus() -> &'static BundledCorpus {
    &BUNDLED
}

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

    #[test]
    fn bundled_corpus_loads_from_the_sysroot_share_dir() {
        let sysroot = std::env::temp_dir().join(format!(
            "beamtalk-examples-bundled-corpus-{}",
            std::process::id()
        ));
        let path = bundled_corpus_path(&sysroot);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        let corpus = Corpus {
            entries: vec![make_entry("a", "A")],
        };
        std::fs::write(&path, serde_json::to_vec(&corpus).unwrap()).unwrap();

        let bundled = BundledCorpus::load(Some(&sysroot));
        std::fs::remove_dir_all(&sysroot).unwrap();

        assert!(matches!(bundled, BundledCorpus::Loaded(_)));
        assert_eq!(bundled.corpus().entries.len(), 1);
    }

    #[test]
    fn bundled_corpus_missing_reports_where_it_looked() {
        let sysroot = std::path::Path::new("/nonexistent-sysroot");
        let bundled = BundledCorpus::load(Some(sysroot));
        match &bundled {
            BundledCorpus::Missing { path } => {
                assert_eq!(
                    path.as_deref(),
                    Some(bundled_corpus_path(sysroot).as_path())
                );
            }
            BundledCorpus::Loaded(_) => panic!("expected a missing corpus"),
        }
        assert!(bundled.corpus().entries.is_empty());
    }

    #[test]
    fn bundled_corpus_missing_without_a_sysroot() {
        let bundled = BundledCorpus::load(None);
        assert!(matches!(bundled, BundledCorpus::Missing { path: None }));
    }
}
