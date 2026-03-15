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
