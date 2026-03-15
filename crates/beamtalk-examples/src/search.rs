// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::corpus::{CORPUS, Corpus, CorpusEntry};

/// Maximum number of results that can be returned.
const MAX_LIMIT: usize = 20;

/// A search result with its relevance score.
#[derive(Debug, Clone)]
pub struct SearchResult<'a> {
    pub entry: &'a CorpusEntry,
    pub score: u32,
}

/// Search the embedded corpus for entries matching the given query.
///
/// Delegates to [`search_in`] using the global `CORPUS` singleton.
pub fn search(query: &str, limit: Option<usize>) -> Vec<SearchResult<'_>> {
    search_in(&CORPUS, query, limit)
}

/// Search a given corpus for entries matching the given query.
///
/// The query is tokenized into keywords (split on whitespace, lowercased).
/// Each keyword is matched against each field with weighted scoring:
/// - title: ×10
/// - tags: ×8
/// - category: ×5
/// - explanation: ×2
/// - source: ×1
///
/// Results are sorted by score descending and truncated to `limit` (max 20).
pub fn search_in<'a>(
    corpus: &'a Corpus,
    query: &str,
    limit: Option<usize>,
) -> Vec<SearchResult<'a>> {
    let limit = limit.unwrap_or(5).min(MAX_LIMIT);

    let keywords: Vec<String> = query.split_whitespace().map(str::to_lowercase).collect();

    if keywords.is_empty() {
        return Vec::new();
    }

    let mut results: Vec<SearchResult<'a>> = corpus
        .entries
        .iter()
        .filter_map(|entry| {
            let score = score_entry(entry, &keywords);
            if score > 0 {
                Some(SearchResult { entry, score })
            } else {
                None
            }
        })
        .collect();

    results.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| a.entry.id.cmp(&b.entry.id))
    });
    results.truncate(limit);
    results
}

/// Score a single entry against the query keywords.
fn score_entry(entry: &CorpusEntry, keywords: &[String]) -> u32 {
    let mut score = 0u32;
    let title_lower = entry.title.to_lowercase();
    let category_lower = entry.category.to_lowercase();
    let explanation_lower = entry.explanation.to_lowercase();
    let source_lower = entry.source.to_lowercase();

    for keyword in keywords {
        if title_lower.contains(keyword.as_str()) {
            score += 10;
        }
        if entry
            .tags
            .iter()
            .any(|tag| tag.to_lowercase().contains(keyword.as_str()))
        {
            score += 8;
        }
        if category_lower.contains(keyword.as_str()) {
            score += 5;
        }
        if explanation_lower.contains(keyword.as_str()) {
            score += 2;
        }
        if source_lower.contains(keyword.as_str()) {
            score += 1;
        }
    }
    score
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_corpus() -> Corpus {
        Corpus {
            entries: vec![
                CorpusEntry {
                    id: "closures-value-capture".to_string(),
                    title: "Value Capture in Closures".to_string(),
                    category: "closures".to_string(),
                    tags: vec!["blocks".to_string(), "closure".to_string()],
                    source: "x := 42.\n[:y | x + y] value: 8".to_string(),
                    explanation: "Demonstrates variable capture in blocks".to_string(),
                },
                CorpusEntry {
                    id: "actor-counter".to_string(),
                    title: "Counter Actor".to_string(),
                    category: "actors".to_string(),
                    tags: vec!["Actor".to_string(), "state".to_string()],
                    source: "Actor subclass: Counter\n  state: value = 0".to_string(),
                    explanation: "A simple stateful actor".to_string(),
                },
                CorpusEntry {
                    id: "collections-array-do".to_string(),
                    title: "Iterating an Array with do:".to_string(),
                    category: "collections".to_string(),
                    tags: vec![
                        "Array".to_string(),
                        "do:".to_string(),
                        "iteration".to_string(),
                    ],
                    source: "#(1 2 3) do: [:x | x printString]".to_string(),
                    explanation: "The do: message sends a block to each element".to_string(),
                },
            ],
        }
    }

    #[test]
    fn empty_query_returns_no_results() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "", None);
        assert!(results.is_empty());
    }

    #[test]
    fn whitespace_only_query_returns_no_results() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "   ", None);
        assert!(results.is_empty());
    }

    #[test]
    fn limit_capped_at_max() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "a", Some(100));
        assert!(results.len() <= MAX_LIMIT);
    }

    #[test]
    fn default_limit_is_five() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "a", None);
        assert!(results.len() <= 5);
    }

    #[test]
    fn results_sorted_by_score_descending() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "actor state", Some(20));
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
        let upper = search_in(&corpus, "COUNTER", Some(20));
        let lower = search_in(&corpus, "counter", Some(20));
        assert_eq!(
            upper.len(),
            lower.len(),
            "case should not affect result count"
        );
    }

    #[test]
    fn no_matches_returns_empty() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "zzzznonexistenttermzzzz", None);
        assert!(results.is_empty());
    }

    #[test]
    fn keyword_tokenization() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "actor state", Some(20));
        assert!(!results.is_empty(), "multi-word query should find results");
    }

    #[test]
    fn title_weighted_higher_than_source() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "counter", Some(20));
        // "Counter Actor" has "counter" in the title (×10), should rank high
        assert!(!results.is_empty());
        assert_eq!(results[0].entry.id, "actor-counter");
    }

    #[test]
    fn tag_match_scores_correctly() {
        let corpus = test_corpus();
        let results = search_in(&corpus, "closure", Some(20));
        assert!(!results.is_empty());
        // The closures entry has "closure" as a tag (×8)
        assert_eq!(results[0].entry.id, "closures-value-capture");
    }

    #[test]
    fn tie_breaking_uses_id() {
        // Two entries with equal scores should be sorted by ID
        let corpus = Corpus {
            entries: vec![
                CorpusEntry {
                    id: "b-entry".to_string(),
                    title: "Test".to_string(),
                    category: "cat".to_string(),
                    tags: vec![],
                    source: "xyz".to_string(),
                    explanation: "xyz".to_string(),
                },
                CorpusEntry {
                    id: "a-entry".to_string(),
                    title: "Test".to_string(),
                    category: "cat".to_string(),
                    tags: vec![],
                    source: "xyz".to_string(),
                    explanation: "xyz".to_string(),
                },
            ],
        };
        let results = search_in(&corpus, "test", Some(20));
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].entry.id, "a-entry");
        assert_eq!(results[1].entry.id, "b-entry");
    }
}
