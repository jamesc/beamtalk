// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::class_corpus::{CLASS_CORPUS, ClassCorpus, ClassEntry};

/// Maximum number of results that can be returned.
const MAX_LIMIT: usize = 20;

/// A class search result with its relevance score.
#[derive(Debug, Clone)]
pub struct ClassSearchResult<'a> {
    pub entry: &'a ClassEntry,
    pub score: u32,
}

/// Search the embedded class corpus for classes matching the given query.
///
/// Delegates to [`search_classes_in`] using the global `CLASS_CORPUS` singleton.
pub fn search_classes(query: &str, limit: Option<usize>) -> Vec<ClassSearchResult<'_>> {
    search_classes_in(&CLASS_CORPUS, query, limit)
}

/// Search a given class corpus for classes matching the given query.
///
/// The query is tokenized into keywords (split on whitespace, lowercased).
/// Each keyword is matched against each field with weighted scoring:
/// - class name (exact match): x100
/// - class name (contains): x50
/// - superclass name: x30
/// - doc description: x20
/// - method selectors: x10
///
/// Results are sorted by score descending and truncated to `limit` (max 20).
pub fn search_classes_in<'a>(
    corpus: &'a ClassCorpus,
    query: &str,
    limit: Option<usize>,
) -> Vec<ClassSearchResult<'a>> {
    let limit = limit.unwrap_or(5).min(MAX_LIMIT);

    let keywords: Vec<String> = query
        .split_whitespace()
        .map(|token| {
            token
                .trim_matches(|c: char| c.is_ascii_punctuation() && c != ':')
                .to_lowercase()
        })
        .filter(|token| !token.is_empty())
        .collect();

    if keywords.is_empty() {
        return Vec::new();
    }

    let mut results: Vec<ClassSearchResult<'a>> = corpus
        .entries
        .iter()
        .filter_map(|entry| {
            let score = score_class(entry, &keywords);
            if score > 0 {
                Some(ClassSearchResult { entry, score })
            } else {
                None
            }
        })
        .collect();

    results.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| a.entry.name.cmp(&b.entry.name))
    });
    results.truncate(limit);
    results
}

/// Score a single class entry against the query keywords.
fn score_class(entry: &ClassEntry, keywords: &[String]) -> u32 {
    let mut score = 0u32;
    let name_lower = entry.name.to_lowercase();
    let superclass_lower = entry.superclass.to_lowercase();
    let doc_lower = entry.doc.as_deref().unwrap_or("").to_lowercase();
    // Pre-join methods for substring matching.
    let methods_lower: Vec<String> = entry.methods.iter().map(|m| m.to_lowercase()).collect();

    for keyword in keywords {
        // Exact class name match.
        if name_lower == *keyword {
            score += 100;
        // Class name contains keyword.
        } else if name_lower.contains(keyword.as_str()) {
            score += 50;
        }
        // Superclass match.
        if superclass_lower == *keyword || superclass_lower.contains(keyword.as_str()) {
            score += 30;
        }
        // Doc contains keyword.
        if doc_lower.contains(keyword.as_str()) {
            score += 20;
        }
        // Method selector matches.
        for method in &methods_lower {
            if method.contains(keyword.as_str()) {
                score += 10;
                break; // Only count once per keyword across all methods.
            }
        }
    }
    score
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_corpus() -> ClassCorpus {
        ClassCorpus {
            entries: vec![
                ClassEntry {
                    name: "String".to_string(),
                    superclass: "Collection".to_string(),
                    doc: Some("UTF-8 text operations.".to_string()),
                    methods: vec![
                        "size".to_string(),
                        "isEmpty".to_string(),
                        "++".to_string(),
                        "trim".to_string(),
                        "uppercase".to_string(),
                        "split:".to_string(),
                    ],
                    is_sealed: true,
                    is_abstract: false,
                },
                ClassEntry {
                    name: "Actor".to_string(),
                    superclass: "Object".to_string(),
                    doc: Some("Base class for process-based objects.".to_string()),
                    methods: vec!["spawn".to_string(), "stop".to_string(), "kill".to_string()],
                    is_sealed: false,
                    is_abstract: false,
                },
                ClassEntry {
                    name: "HTTPClient".to_string(),
                    superclass: "Actor".to_string(),
                    doc: Some(
                        "Actor-based HTTP client for one-shot and persistent requests.".to_string(),
                    ),
                    methods: vec![
                        "get:".to_string(),
                        "post:body:".to_string(),
                        "put:body:".to_string(),
                    ],
                    is_sealed: false,
                    is_abstract: false,
                },
            ],
        }
    }

    #[test]
    fn empty_query_returns_no_results() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "", None);
        assert!(results.is_empty());
    }

    #[test]
    fn whitespace_only_query_returns_no_results() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "   ", None);
        assert!(results.is_empty());
    }

    #[test]
    fn limit_capped_at_max() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "a", Some(100));
        assert!(results.len() <= MAX_LIMIT);
    }

    #[test]
    fn default_limit_is_five() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "a", None);
        assert!(results.len() <= 5);
    }

    #[test]
    fn results_sorted_by_score_descending() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "actor process", Some(20));
        for window in results.windows(2) {
            assert!(
                window[0].score >= window[1].score,
                "results should be sorted by score descending"
            );
        }
    }

    #[test]
    fn case_insensitive_search() {
        let corpus = test_corpus();
        let upper = search_classes_in(&corpus, "STRING", Some(20));
        let lower = search_classes_in(&corpus, "string", Some(20));
        assert_eq!(
            upper.len(),
            lower.len(),
            "case should not affect result count"
        );
    }

    #[test]
    fn no_matches_returns_empty() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "zzzznonexistenttermzzzz", None);
        assert!(results.is_empty());
    }

    #[test]
    fn exact_class_name_scores_highest() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "actor", Some(20));
        assert!(!results.is_empty());
        assert_eq!(results[0].entry.name, "Actor");
    }

    #[test]
    fn method_selector_match() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "trim", Some(20));
        assert!(!results.is_empty());
        assert_eq!(results[0].entry.name, "String");
    }

    #[test]
    fn doc_match() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "process", Some(20));
        assert!(!results.is_empty());
        // Actor has "process-based" in doc.
        assert_eq!(results[0].entry.name, "Actor");
    }

    #[test]
    fn superclass_match() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "collection", Some(20));
        assert!(!results.is_empty());
        // String's superclass is Collection.
        assert!(results.iter().any(|r| r.entry.name == "String"));
    }

    #[test]
    fn partial_name_match() {
        let corpus = test_corpus();
        let results = search_classes_in(&corpus, "http", Some(20));
        assert!(!results.is_empty());
        assert_eq!(results[0].entry.name, "HTTPClient");
    }

    #[test]
    fn tie_breaking_uses_name() {
        let corpus = ClassCorpus {
            entries: vec![
                ClassEntry {
                    name: "Beta".to_string(),
                    superclass: "Object".to_string(),
                    doc: Some("Test".to_string()),
                    methods: vec![],
                    is_sealed: false,
                    is_abstract: false,
                },
                ClassEntry {
                    name: "Alpha".to_string(),
                    superclass: "Object".to_string(),
                    doc: Some("Test".to_string()),
                    methods: vec![],
                    is_sealed: false,
                    is_abstract: false,
                },
            ],
        };
        let results = search_classes_in(&corpus, "test", Some(20));
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].entry.name, "Alpha");
        assert_eq!(results[1].entry.name, "Beta");
    }
}
