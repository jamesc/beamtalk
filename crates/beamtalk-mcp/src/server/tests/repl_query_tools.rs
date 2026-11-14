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
async fn evaluate_renders_warning_with_its_hint() {
    // BT-4449: `warnings` stays message strings; the hint arrives in
    // `warning_details` and is shown on its own `Hint:` line.
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "value": "Client",
        "warnings": ["Udp class does not conform to Transport class", "plain"],
        "warning_details": [
            {"message": "Udp class does not conform to Transport class",
             "hint": "missing: class defaultPort"},
            {"message": "plain"}
        ]
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "Value subclass: Client".to_string(),
            trace: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
    let text = call_text(&result);
    assert!(
        text.contains(
            "Warning: Udp class does not conform to Transport class\nHint: missing: class defaultPort"
        ),
        "expected the hint under the warning in {text:?}"
    );
    assert!(text.contains("Warning: plain"), "{text:?}");
}

#[tokio::test]
async fn evaluate_error_still_renders_warnings_with_hints() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "error": "boom",
        "status": ["done", "error"],
        "warnings": ["w"],
        "warning_details": [{"message": "w", "hint": "h"}]
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "x".to_string(),
            trace: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    let text = call_text(&result);
    assert!(text.contains("Error: boom"), "{text:?}");
    assert!(text.contains("Warning: w\nHint: h"), "{text:?}");
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

/// BT-4830 (ADR 0139, decision 4): a REPL eval-error reply's `frames` pass
/// through `evaluate` as `structuredContent.frames`, every frame keeping the
/// key set pinned by `tests/repl-protocol/frame_fields.json`, hidden frames
/// included and flagged. Offline: a canned reply, no live workspace.
#[tokio::test]
async fn evaluate_error_passes_frames_through_as_structured_content() {
    let fixture_path =
        beamtalk_workspace::cargo_workspace_root!().join("tests/repl-protocol/frame_fields.json");
    let fixture: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&fixture_path).expect("read fixture"))
            .expect("parse fixture");
    let mut pinned: Vec<String> = fixture["frame_fields"]
        .as_array()
        .expect("frame_fields array")
        .iter()
        .map(|v| v.as_str().expect("string key").to_owned())
        .collect();
    pinned.sort();

    // The wire shape of `beamtalk_stack_frame:structured_frame/1`: every
    // pinned key present on every frame (null when unknown).
    let frames = serde_json::json!([
        {
            "display": "Counter>>increment (counter.bt:12)",
            "module": "bt@counter", "function": "increment", "arity": 1,
            "file": "counter.bt", "line": 12, "hidden": false
        },
        {
            "display": "gen_server:call/3",
            "module": "gen_server", "function": "call", "arity": 3,
            "file": null, "line": null, "hidden": true
        }
    ]);
    // Guard the canned reply itself against drifting from the fixture.
    for frame in frames.as_array().unwrap() {
        let mut keys: Vec<String> = frame.as_object().unwrap().keys().cloned().collect();
        keys.sort();
        assert_eq!(keys, pinned, "canned frame keys drifted from the fixture");
    }

    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "error": "boom",
        "status": ["done", "error"],
        "frames": frames.clone()
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "Counter new increment".to_string(),
            trace: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    let structured = result
        .structured_content
        .expect("structuredContent.frames must be present on an eval error with frames");
    assert_eq!(structured["frames"], frames, "{structured}");
    for frame in structured["frames"].as_array().unwrap() {
        let mut keys: Vec<String> = frame.as_object().unwrap().keys().cloned().collect();
        keys.sort();
        assert_eq!(keys, pinned, "passed-through frame keys: {frame}");
    }
}

/// An eval error whose reply carries no `frames` adds no structured content.
#[tokio::test]
async fn evaluate_error_without_frames_has_no_structured_content() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "error": "boom",
        "status": ["done", "error"]
    })))
    .await;
    let result = mcp
        .evaluate(Parameters(EvaluateParams {
            code: "x".to_string(),
            trace: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
    assert!(result.structured_content.is_none());
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
