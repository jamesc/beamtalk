// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::corpus::{CORPUS, CorpusEntry};

/// Maximum number of results that can be returned.
const MAX_LIMIT: usize = 20;

/// A search result with its relevance score.
#[derive(Debug, Clone)]
pub struct SearchResult<'a> {
    pub entry: &'a CorpusEntry,
    pub score: u32,
}

/// Search the corpus for entries matching the given query.
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
pub fn search(query: &str, limit: Option<usize>) -> Vec<SearchResult<'_>> {
    let limit = limit.unwrap_or(5).min(MAX_LIMIT);

    let keywords: Vec<String> = query.split_whitespace().map(str::to_lowercase).collect();

    if keywords.is_empty() {
        return Vec::new();
    }

    let mut results: Vec<SearchResult<'_>> = CORPUS
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

    #[test]
    fn empty_query_returns_no_results() {
        let results = search("", None);
        assert!(results.is_empty());
    }

    #[test]
    fn whitespace_only_query_returns_no_results() {
        let results = search("   ", None);
        assert!(results.is_empty());
    }

    #[test]
    fn limit_capped_at_max() {
        // Even if we request 100, we should get at most MAX_LIMIT (20)
        let results = search("a", Some(100));
        assert!(results.len() <= MAX_LIMIT);
    }

    #[test]
    fn default_limit_is_five() {
        let results = search("a", None);
        assert!(results.len() <= 5);
    }

    #[test]
    fn results_sorted_by_score_descending() {
        let results = search("counter", None);
        for window in results.windows(2) {
            assert!(
                window[0].score >= window[1].score,
                "results should be sorted by score descending"
            );
        }
    }

    #[test]
    fn case_insensitive_search() {
        let upper = search("COUNTER", Some(20));
        let lower = search("counter", Some(20));
        assert_eq!(
            upper.len(),
            lower.len(),
            "case should not affect result count"
        );
    }

    #[test]
    fn no_matches_returns_empty() {
        let results = search("zzzznonexistenttermzzzz", None);
        assert!(results.is_empty());
    }

    #[test]
    fn keyword_tokenization() {
        // Multi-word queries should match entries that contain any of the words
        let results = search("actor state", Some(20));
        // Should find more results than a very specific single term
        assert!(!results.is_empty(), "multi-word query should find results");
    }
}
