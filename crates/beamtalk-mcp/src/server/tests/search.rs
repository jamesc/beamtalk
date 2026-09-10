// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `search_examples` / `search_classes` / `list_classes` tool tests: result limits, empty queries, and tool registration.

use super::*;

// --- search_examples ---

#[test]
fn search_examples_returns_results_for_known_query() {
    let results = beamtalk_examples::search("closures", None);
    assert!(
        !results.is_empty(),
        "searching 'closures' should return results from the bundled corpus"
    );
}

#[test]
fn search_examples_respects_limit() {
    let results = beamtalk_examples::search("a", Some(2));
    assert!(
        results.len() <= 2,
        "limit=2 should return at most 2 results, got {}",
        results.len()
    );
}

#[test]
fn search_examples_empty_query_returns_empty() {
    let results = beamtalk_examples::search("", None);
    assert!(results.is_empty(), "empty query should return no results");
}

#[test]
fn search_examples_tool_registered() {
    // Verify that search_examples appears in the tool router by checking
    // that the tool_router lists it. The #[tool_router] macro generates
    // a tool_router() method that includes all #[tool] handlers.
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"search_examples"),
        "search_examples should be in tool list, found: {tool_names:?}"
    );
}

// --- search_classes ---

#[test]
fn search_classes_returns_results_for_known_query() {
    let results = beamtalk_examples::search_classes("http", None);
    assert!(
        !results.is_empty(),
        "searching 'http' should return results from the bundled class corpus"
    );
}

#[test]
fn search_classes_respects_limit() {
    let results = beamtalk_examples::search_classes("a", Some(2));
    assert!(
        results.len() <= 2,
        "limit=2 should return at most 2 results, got {}",
        results.len()
    );
}

#[test]
fn search_classes_empty_query_returns_empty() {
    let results = beamtalk_examples::search_classes("", None);
    assert!(results.is_empty(), "empty query should return no results");
}

#[test]
fn search_classes_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"search_classes"),
        "search_classes should be in tool list, found: {tool_names:?}"
    );
}

// --- list_classes param deserialization ---

#[test]
fn list_classes_params_no_filter() {
    let json = serde_json::json!({});
    let params: ListClassesParams = serde_json::from_value(json).unwrap();
    assert!(params.filter.is_none());
}

#[test]
fn list_classes_params_with_filter() {
    let json = serde_json::json!({"filter": "Value"});
    let params: ListClassesParams = serde_json::from_value(json).unwrap();
    assert_eq!(params.filter.as_deref(), Some("Value"));
}

#[test]
fn list_classes_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"list_classes"),
        "list_classes should be in tool list, found: {tool_names:?}"
    );
}
