// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP tool handlers that query a live REPL: evaluate, complete, `load_project`, `load_file`, inspect, `list_actors`, `supervision_tree`, `list_classes`, `reload_class`, docs, unload, interrupt, `show_codegen`, and test -- all driven through the `fake_mcp` harness in mod.rs.

use super::*;

// --- evaluate ---

#[tokio::test]
async fn evaluate_returns_value_and_output() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"value": "3", "output": "hi\n"}))).await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "1 + 2".to_string(),
            trace: None,
        }))
        .await
        .expect("evaluate should not raise a protocol error");
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(text.contains('3'), "expected value in {text:?}");
    assert!(text.contains("Output: hi"), "expected output in {text:?}");
}

#[tokio::test]
async fn evaluate_trace_renders_steps() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "steps": [{"src": "1 + 2", "value": "3"}]
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "1 + 2".to_string(),
            trace: Some(true),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(!call_text(&result).is_empty());
}

#[tokio::test]
async fn evaluate_error_includes_line_and_hint() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "status": ["done", "error"],
        "error": "boom",
        "line": 3,
        "hint": "try again"
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "bogus".to_string(),
            trace: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    let text = call_text(&result);
    assert!(text.contains("boom"), "expected error message in {text:?}");
}

// --- complete ---

#[tokio::test]
async fn complete_returns_joined_completions() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "completions": ["size", "sqrt"]
    })))
    .await;
    let result = mcp
        .complete(Parameters(CompleteParams {
            code: "3 s".to_string(),
            cursor: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(text.contains("size") && text.contains("sqrt"));
}

#[tokio::test]
async fn complete_empty_reports_no_completions() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"completions": []}))).await;
    let result = mcp
        .complete(Parameters(CompleteParams {
            code: "zzz".to_string(),
            cursor: Some(3),
        }))
        .await
        .unwrap();
    assert_eq!(call_text(&result), "No completions available");
}

// --- load_project ---

#[tokio::test]
async fn load_project_success_lists_classes_and_summary() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "classes": ["Counter", "Greeter"],
        "summary": "2 files compiled"
    })))
    .await;
    let result = mcp
        .load_project(Parameters(LoadProjectParams {
            path: ".".to_string(),
            include_tests: Some(true),
            force: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(text.contains("Counter") && text.contains("Greeter"));
    assert!(text.contains("2 files compiled"));
}

#[tokio::test]
async fn load_project_partial_errors_reports_failed_files() {
    // `errors` non-empty with an overall `status: done` (no top-level
    // error flag) is the per-file-partial-failure shape `check_response!`
    // does not catch — a distinct branch from a fully-failed response.
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "classes": ["Counter"],
            "errors": [{"path": "src/bad.bt", "line": 4, "message": "parse error", "hint": "check syntax"}],
            "summary": "1 of 2 files compiled"
        })))
        .await;
    let result = mcp
        .load_project(Parameters(LoadProjectParams {
            path: ".".to_string(),
            include_tests: None,
            force: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    let text = call_text(&result);
    assert!(text.contains("src/bad.bt"));
    assert!(text.contains("parse error"));
    assert!(text.contains("check syntax"));
    assert!(text.contains("Counter"));
}

#[tokio::test]
async fn load_project_full_failure_is_error() {
    let (_fake, mcp) = fake_mcp(respond_error("path does not exist")).await;
    let result = mcp
        .load_project(Parameters(LoadProjectParams {
            path: "/nope".to_string(),
            include_tests: None,
            force: Some(true),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(call_text(&result).contains("path does not exist"));
}

// --- load_file ---

#[tokio::test]
async fn load_file_success_with_warnings() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "value": "Counter",
        "warnings": ["unused variable x"]
    })))
    .await;
    let result = mcp
        .load_file(Parameters(LoadFileParams {
            path: "src/counter.bt".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(text.contains("Counter"));
    assert!(text.contains("unused variable x"));
}

#[tokio::test]
async fn load_file_error() {
    let (_fake, mcp) = fake_mcp(respond_error("file not found")).await;
    let result = mcp
        .load_file(Parameters(LoadFileParams {
            path: "src/missing.bt".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- inspect ---

#[tokio::test]
async fn inspect_string_state_is_used_verbatim() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"state": "a nice actor"}))).await;
    let result = mcp
        .inspect(Parameters(InspectParams {
            actor: "<0.1.0>".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(call_text(&result), "a nice actor");
}

#[tokio::test]
async fn inspect_object_state_is_pretty_printed() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"state": {"count": 3}}))).await;
    let result = mcp
        .inspect(Parameters(InspectParams {
            actor: "<0.1.0>".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("count"));
}

#[tokio::test]
async fn inspect_no_state_available() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .inspect(Parameters(InspectParams {
            actor: "<0.1.0>".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(call_text(&result), "No state available");
}

#[tokio::test]
async fn inspect_error() {
    let (_fake, mcp) = fake_mcp(respond_error("no such actor")).await;
    let result = mcp
        .inspect(Parameters(InspectParams {
            actor: "<0.999.0>".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- list_actors ---

#[tokio::test]
async fn list_actors_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "actors": [{"pid": "<0.1.0>", "class": "Counter", "module": "bt@counter", "spawned_at": 0}]
    })))
    .await;
    let result = mcp.list_actors().await.unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("Counter"));
}

#[tokio::test]
async fn list_actors_error() {
    let (_fake, mcp) = fake_mcp(respond_error("workspace unavailable")).await;
    let result = mcp.list_actors().await.unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- supervision_tree ---

#[tokio::test]
async fn supervision_tree_default_scope() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#()"))).await;
    let result = mcp
        .supervision_tree(Parameters(SupervisionTreeParams { scope: None }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
}

#[tokio::test]
async fn supervision_tree_system_scope() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#()"))).await;
    let result = mcp
        .supervision_tree(Parameters(SupervisionTreeParams {
            scope: Some("system".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
}

#[tokio::test]
async fn supervision_tree_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
    let result = mcp
        .supervision_tree(Parameters(SupervisionTreeParams { scope: None }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- list_classes ---

#[tokio::test]
async fn list_classes_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "class_list": [{"name": "Counter", "superclass": "Object", "doc": "a counter", "sealed": false, "abstract": false}]
        })))
        .await;
    let result = mcp
        .list_classes(Parameters(ListClassesParams { filter: None }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("Counter"));
}

#[tokio::test]
async fn list_classes_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
    let result = mcp
        .list_classes(Parameters(ListClassesParams {
            filter: Some("stdlib".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- reload_class ---

#[tokio::test]
async fn reload_class_success_default_message() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .reload_class(Parameters(ReloadClassParams {
            class: "Counter".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(call_text(&result), "Class reloaded successfully");
}

#[tokio::test]
async fn reload_class_rejects_invalid_class_name() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .reload_class(Parameters(ReloadClassParams {
            class: "not_a_class".to_string(),
        }))
        .await
        .expect_err("lowercase class name should be rejected before touching the client");
    assert!(err.message.contains("Invalid class name"));
}

#[tokio::test]
async fn reload_class_error() {
    let (_fake, mcp) = fake_mcp(respond_error("migration failed")).await;
    let result = mcp
        .reload_class(Parameters(ReloadClassParams {
            class: "Counter".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- docs ---

#[tokio::test]
async fn docs_class_lookup() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Counter docs"))).await;
    let result = mcp
        .docs(Parameters(DocsParams {
            class: Some("Counter".to_string()),
            erlang_module: None,
            selector: None,
        }))
        .await
        .unwrap();
    // `docs`' success path builds `CallToolResult` via `::default()` (to
    // also carry `structured_content`), so `is_error` is `None` on
    // success rather than `Some(false)` — unlike the `success()`
    // constructor most other tools use.
    assert_ne!(result.is_error, Some(true));
    assert!(call_text(&result).contains("Counter docs"));
}

#[tokio::test]
async fn docs_erlang_module_with_selector() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("lists:map/2 docs"))).await;
    let result = mcp
        .docs(Parameters(DocsParams {
            class: None,
            erlang_module: Some("lists".to_string()),
            selector: Some("map".to_string()),
        }))
        .await
        .unwrap();
    assert_ne!(result.is_error, Some(true));
}

#[tokio::test]
async fn docs_rejects_both_class_and_erlang_module() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .docs(Parameters(DocsParams {
            class: Some("Counter".to_string()),
            erlang_module: Some("lists".to_string()),
            selector: None,
        }))
        .await
        .expect_err("both class and erlang_module should be rejected");
    assert!(err.message.contains("either"));
}

#[tokio::test]
async fn docs_rejects_neither_class_nor_erlang_module() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .docs(Parameters(DocsParams {
            class: None,
            erlang_module: None,
            selector: None,
        }))
        .await
        .expect_err("neither class nor erlang_module should be rejected");
    assert!(err.message.contains("Provide"));
}

#[tokio::test]
async fn docs_error() {
    let (_fake, mcp) = fake_mcp(respond_error("no docs")).await;
    let result = mcp
        .docs(Parameters(DocsParams {
            class: Some("Counter".to_string()),
            erlang_module: None,
            selector: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- unload ---

#[tokio::test]
async fn unload_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .unload(Parameters(UnloadParams {
            class: "Counter".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("Counter"));
}

#[tokio::test]
async fn unload_rejects_invalid_class_name() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .unload(Parameters(UnloadParams {
            class: "bad".to_string(),
        }))
        .await
        .expect_err("lowercase class name should be rejected");
    assert!(err.message.contains("Invalid class name"));
}

#[tokio::test]
async fn unload_error() {
    let (_fake, mcp) = fake_mcp(respond_error("class in use")).await;
    let result = mcp
        .unload(Parameters(UnloadParams {
            class: "Counter".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- interrupt ---

#[tokio::test]
async fn interrupt_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp.interrupt().await.unwrap();
    assert_eq!(call_text(&result), "Interrupt sent");
}

#[tokio::test]
async fn interrupt_error() {
    let (_fake, mcp) = fake_mcp(respond_error("nothing running")).await;
    let result = mcp.interrupt().await.unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- show_codegen ---

#[tokio::test]
async fn show_codegen_from_code_snippet() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"core_erlang": "'add'/2 = ..."}))).await;
    let result = mcp
        .show_codegen(Parameters(ShowCodegenParams {
            code: Some("1 + 2".to_string()),
            class: None,
            selector: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("add"));
}

#[tokio::test]
async fn show_codegen_from_class_with_warnings() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "core_erlang": "module 'counter'",
        "warnings": ["deprecated selector"]
    })))
    .await;
    let result = mcp
        .show_codegen(Parameters(ShowCodegenParams {
            code: None,
            class: Some("Counter".to_string()),
            selector: Some("increment".to_string()),
        }))
        .await
        .unwrap();
    let text = call_text(&result);
    assert!(text.contains("module 'counter'"));
    assert!(text.contains("deprecated selector"));
}

#[tokio::test]
async fn show_codegen_rejects_selector_without_class() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .show_codegen(Parameters(ShowCodegenParams {
            code: None,
            class: None,
            selector: Some("increment".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(call_text(&result).contains("requires"));
}

#[tokio::test]
async fn show_codegen_rejects_neither_code_nor_class() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .show_codegen(Parameters(ShowCodegenParams {
            code: None,
            class: None,
            selector: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(call_text(&result).contains("Provide"));
}

#[tokio::test]
async fn show_codegen_error() {
    let (_fake, mcp) = fake_mcp(respond_error("compile failed")).await;
    let result = mcp
        .show_codegen(Parameters(ShowCodegenParams {
            code: Some("bogus".to_string()),
            class: None,
            selector: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- test ---

#[tokio::test]
async fn test_by_class_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "results": {"passed": 3, "failed": 0}
    })))
    .await;
    let result = mcp
        .test(Parameters(TestParams {
            class: Some("CounterTest".to_string()),
            file: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    assert!(call_text(&result).contains("passed"));
}

#[tokio::test]
async fn test_by_file_and_all_default() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "results": {"passed": 1, "failed": 0}
    })))
    .await;
    let by_file = mcp
        .test(Parameters(TestParams {
            class: None,
            file: Some("test/counter_test.bt".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(by_file.is_error, Some(false));

    let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({
        "results": {"passed": 1, "failed": 0}
    })))
    .await;
    let all = mcp2
        .test(Parameters(TestParams {
            class: None,
            file: None,
        }))
        .await
        .unwrap();
    assert_eq!(all.is_error, Some(false));
}

#[tokio::test]
async fn test_rejects_class_and_file_together() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .test(Parameters(TestParams {
            class: Some("CounterTest".to_string()),
            file: Some("test/counter_test.bt".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(call_text(&result).contains("mutually exclusive"));
}

#[tokio::test]
async fn test_failures_are_reported_as_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "status": ["done", "test-error"],
        "results": {"passed": 1, "failed": 1}
    })))
    .await;
    let result = mcp
        .test(Parameters(TestParams {
            class: None,
            file: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(call_text(&result).contains("TEST FAILURES"));
}

#[tokio::test]
async fn test_execution_error() {
    let (_fake, mcp) = fake_mcp(respond_error("no such class")).await;
    let result = mcp
        .test(Parameters(TestParams {
            class: Some("NoSuch".to_string()),
            file: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}
