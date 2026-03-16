// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

mod class_corpus;
mod class_search;
mod corpus;
mod search;

pub use class_corpus::{ClassCorpus, ClassEntry};
pub use class_search::{ClassSearchResult, search_classes, search_classes_in};
pub use corpus::{Corpus, CorpusEntry};
pub use search::{SearchResult, search, search_in};
