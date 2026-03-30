// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

pub mod class_corpus;
mod class_search;
pub mod corpus;
mod search;

pub use class_corpus::{ClassCorpus, ClassEntry, load_class_corpus_from_file, merge_class_corpora};
pub use class_search::{ClassSearchResult, search_classes, search_classes_in};
pub use corpus::{Corpus, CorpusEntry, load_corpus_from_file, merge_corpora};
pub use search::{SearchResult, search, search_in};
