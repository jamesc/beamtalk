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
