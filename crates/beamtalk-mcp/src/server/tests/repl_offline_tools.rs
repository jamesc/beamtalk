// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP tool handlers exercised through the `fake_mcp` harness for their offline (no-REPL) paths: `lint/diagnostic_summary`, `search_examples/search_classes`, the tracing tools, and `describe/list_packages/package_classes`.

use super::*;

// --- lint / diagnostic_summary (offline — no REPL touched) ---

#[tokio::test]
async fn lint_tool_reports_clean_file() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("clean.bt");
    std::fs::write(&file, "Object subclass: Clean\n  foo => 1\n").unwrap();
    let result = mcp
        .lint(Parameters(LintParams {
            path: Some(file.to_str().unwrap().to_string()),
        }))
        .await
        .unwrap();
    assert_ne!(result.is_error, Some(true));
    assert!(result.structured_content.is_some());
}

#[tokio::test]
async fn lint_tool_reports_errors_for_nonexistent_path() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .lint(Parameters(LintParams {
            path: Some("/nonexistent/path/nope".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn diagnostic_summary_tool_runs_offline() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("clean.bt");
    std::fs::write(&file, "Object subclass: Clean\n  foo => 1\n").unwrap();
    let result = mcp
        .diagnostic_summary(Parameters(DiagnosticSummaryParams {
            path: Some(file.to_str().unwrap().to_string()),
        }))
        .await
        .unwrap();
    // Like `docs`, `diagnostic_summary` builds its `CallToolResult` via
    // `::default()` and never sets `is_error` — it never fails.
    assert_ne!(result.is_error, Some(true));
    assert!(result.structured_content.is_some());
}

// --- search_examples / search_classes (offline — no REPL touched) ---

#[tokio::test]
async fn search_examples_finds_results_for_a_known_topic() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .search_examples(Parameters(SearchExamplesParams {
            query: "closures".to_string(),
            limit: Some(3),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(!call_text(&result).contains("No examples found"));
}

#[tokio::test]
async fn search_examples_reports_no_results() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .search_examples(Parameters(SearchExamplesParams {
            query: "zzznonexistentqueryxyz999".to_string(),
            limit: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("No examples found"));
}

#[tokio::test]
async fn search_classes_finds_results_for_a_known_topic() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .search_classes(Parameters(SearchClassesParams {
            query: "collection".to_string(),
            limit: Some(3),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(!call_text(&result).contains("No classes found"));
}

#[tokio::test]
async fn search_classes_reports_no_results() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .search_classes(Parameters(SearchClassesParams {
            query: "zzznonexistentqueryxyz999".to_string(),
            limit: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("No classes found"));
}

// --- tracing tools ---

#[tokio::test]
async fn enable_tracing_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp.enable_tracing().await.unwrap();
    assert_eq!(ok.is_error, Some(false));

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2.enable_tracing().await.unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn disable_tracing_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp.disable_tracing().await.unwrap();
    assert_eq!(ok.is_error, Some(false));

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2.disable_tracing().await.unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn get_traces_returns_value_or_placeholder() {
    let (_fake, mcp) = fake_mcp(respond(
        serde_json::json!({"value": [{"actor": "<0.1.0>"}]}),
    ))
    .await;
    let result = mcp
        .get_traces(Parameters(GetTracesParams {
            actor: Some("<0.1.0>".to_string()),
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: Some(1000),
            limit: Some(10),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("0.1.0"));

    let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
    let empty = mcp2
        .get_traces(Parameters(GetTracesParams {
            actor: None,
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: None,
            limit: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&empty).contains("No traces captured"));
}

#[tokio::test]
async fn get_traces_error() {
    let (_fake, mcp) = fake_mcp(respond_error("tracing disabled")).await;
    let result = mcp
        .get_traces(Parameters(GetTracesParams {
            actor: None,
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: None,
            limit: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn export_traces_returns_value_or_placeholder() {
    let (_fake, mcp) = fake_mcp(respond(
        serde_json::json!({"value": {"path": "t.json", "count": 2}}),
    ))
    .await;
    let result = mcp
        .export_traces(Parameters(ExportTracesParams {
            path: Some("t.json".to_string()),
            actor: None,
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: None,
            limit: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("t.json"));

    let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
    let empty = mcp2
        .export_traces(Parameters(ExportTracesParams {
            path: None,
            actor: None,
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: None,
            limit: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&empty).contains("No traces to export"));
}

#[tokio::test]
async fn export_traces_error() {
    let (_fake, mcp) = fake_mcp(respond_error("disk full")).await;
    let result = mcp
        .export_traces(Parameters(ExportTracesParams {
            path: None,
            actor: None,
            selector: None,
            class: None,
            outcome: None,
            min_duration_ns: None,
            limit: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn actor_stats_returns_value_or_placeholder() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"value": {"calls": 5}}))).await;
    let result = mcp
        .actor_stats(Parameters(ActorStatsParams {
            actor: Some("<0.1.0>".to_string()),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("calls"));

    let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
    let empty = mcp2
        .actor_stats(Parameters(ActorStatsParams { actor: None }))
        .await
        .unwrap();
    assert_eq!(call_text(&empty), "No stats available.");
}

#[tokio::test]
async fn actor_stats_error() {
    let (_fake, mcp) = fake_mcp(respond_error("no such actor")).await;
    let result = mcp
        .actor_stats(Parameters(ActorStatsParams { actor: None }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- describe / list_packages / package_classes ---

#[tokio::test]
async fn describe_reports_ops_and_versions() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "ops": ["eval", "complete"],
        "versions": {"protocol": 1}
    })))
    .await;
    let result = mcp.describe().await.unwrap();
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(text.contains("eval"));
    assert!(text.contains("protocol"));
}

#[tokio::test]
async fn describe_empty_response_has_fallback_text() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp.describe().await.unwrap();
    assert_eq!(call_text(&result), "No describe information available");
}

#[tokio::test]
async fn describe_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
    let result = mcp.describe().await.unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn list_packages_success_with_details() {
    let (_fake, mcp) = fake_mcp(respond_by_code(vec![
        (
            "Package all collect:",
            serde_json::json!({"value": "stdlib v1 (10 classes)"}),
        ),
        ("Package all", serde_json::json!({"value": "#(\"stdlib\")"})),
    ]))
    .await;
    let result = mcp.list_packages().await.unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("stdlib v1"));
}

#[tokio::test]
async fn list_packages_detail_failure_falls_back_to_names() {
    let (_fake, mcp) = fake_mcp(respond_by_code(vec![
        (
            "Package all collect:",
            serde_json::json!({"status": ["done", "error"], "error": "boom"}),
        ),
        ("Package all", serde_json::json!({"value": "#(\"stdlib\")"})),
    ]))
    .await;
    let result = mcp.list_packages().await.unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("stdlib"));
}

#[tokio::test]
async fn list_packages_none_loaded() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"value": "nil"}))).await;
    let result = mcp.list_packages().await.unwrap();
    assert_eq!(call_text(&result), "No packages loaded");
}

#[tokio::test]
async fn list_packages_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
    let result = mcp.list_packages().await.unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn package_classes_success() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#(\"Counter\")"))).await;
    let result = mcp
        .package_classes(Parameters(PackageClassesParams {
            package: "stdlib".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("Counter"));
}

#[tokio::test]
async fn package_classes_rejects_invalid_package_name() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .package_classes(Parameters(PackageClassesParams {
            package: "bad name!".to_string(),
        }))
        .await
        .expect_err("package name with spaces/punctuation should be rejected");
    assert!(err.message.contains("Invalid package name"));
}

#[tokio::test]
async fn package_classes_empty_package_reports_not_loaded() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("nil"))).await;
    let result = mcp
        .package_classes(Parameters(PackageClassesParams {
            package: "missing_pkg".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("No classes found"));
}

#[tokio::test]
async fn package_classes_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
    let result = mcp
        .package_classes(Parameters(PackageClassesParams {
            package: "stdlib".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}
