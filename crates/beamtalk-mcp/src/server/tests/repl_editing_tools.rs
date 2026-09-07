// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP tool handlers that mutate the live image: `save_method` / `try_method` / `save_class`, `remove_method` / `remove_class` / `rename_class` / `rename_method`, flush / `list_changes` / `dirty_methods` / `precheck_method` / `recheck_image`, plus the `tool_router` and `ServerHandler::get_info` dispatch-surface tests.

use super::*;

// --- save_method / try_method / save_class ---

#[tokio::test]
async fn save_method_success_default_message() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .save_method(Parameters(SaveMethodParams {
            class: "Counter".to_string(),
            selector: "#increment".to_string(),
            body: "self value: self value + 1".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("Counter"));
    assert!(call_text(&result).contains("increment"));
}

#[tokio::test]
async fn save_method_rejects_invalid_selector() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .save_method(Parameters(SaveMethodParams {
            class: "Counter".to_string(),
            selector: "bad selector".to_string(),
            body: "1".to_string(),
        }))
        .await
        .expect_err("selector with a space should be rejected");
    assert!(err.message.contains("selector"));
}

#[tokio::test]
async fn save_method_error() {
    let (_fake, mcp) = fake_mcp(respond_error("compile error")).await;
    let result = mcp
        .save_method(Parameters(SaveMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            body: "bogus".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn try_method_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp
        .try_method(Parameters(TryMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            body: "1".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&ok).contains("ephemeral"));

    let (_fake2, mcp2) = fake_mcp(respond_error("compile error")).await;
    let error = mcp2
        .try_method(Parameters(TryMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            body: "bogus".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn save_class_success() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .save_class(Parameters(SaveClassParams {
            source: "Object subclass: Greeter\n  greet => \"hi\"".to_string(),
            path: "src/greeter.bt".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("src/greeter.bt"));
}

#[tokio::test]
async fn save_class_rejects_empty_path_and_source() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let empty_path = mcp
        .save_class(Parameters(SaveClassParams {
            source: "Object subclass: Greeter".to_string(),
            path: String::new(),
        }))
        .await
        .expect_err("empty path should be rejected");
    assert!(empty_path.message.contains("path"));

    let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
    let empty_source = mcp2
        .save_class(Parameters(SaveClassParams {
            source: String::new(),
            path: "src/greeter.bt".to_string(),
        }))
        .await
        .expect_err("empty source should be rejected");
    assert!(empty_source.message.contains("source"));
}

#[tokio::test]
async fn save_class_error() {
    let (_fake, mcp) = fake_mcp(respond_error("already exists")).await;
    let result = mcp
        .save_class(Parameters(SaveClassParams {
            source: "Object subclass: Greeter".to_string(),
            path: "src/greeter.bt".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

// --- remove_method / remove_class / rename_class / rename_method ---

#[tokio::test]
async fn remove_method_success_and_with_if_absent() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let plain = mcp
        .remove_method(Parameters(RemoveMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            if_absent: None,
        }))
        .await
        .unwrap();
    assert!(call_text(&plain).contains("removed"));

    let (_fake2, mcp2) = fake_mcp(respond_value(serde_json::json!("nil"))).await;
    let with_fallback = mcp2
        .remove_method(Parameters(RemoveMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            if_absent: Some("nil".to_string()),
        }))
        .await
        .unwrap();
    assert_eq!(with_fallback.is_error, Some(false));
}

#[tokio::test]
async fn remove_method_error() {
    let (_fake, mcp) = fake_mcp(respond_error("selector_not_found")).await;
    let result = mcp
        .remove_method(Parameters(RemoveMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            if_absent: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn remove_class_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp
        .remove_class(Parameters(RemoveClassParams {
            class: "Counter".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&ok).contains("not yet flushed"));

    let (_fake2, mcp2) = fake_mcp(respond_error("sealed class")).await;
    let error = mcp2
        .remove_class(Parameters(RemoveClassParams {
            class: "Object".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn rename_class_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Accumulator"))).await;
    let ok = mcp
        .rename_class(Parameters(RenameClassParams {
            class: "Counter".to_string(),
            new_name: "Accumulator".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&ok).contains("Accumulator"));

    let (_fake2, mcp2) = fake_mcp(respond_error("name collision")).await;
    let error = mcp2
        .rename_class(Parameters(RenameClassParams {
            class: "Counter".to_string(),
            new_name: "Object".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn rename_method_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Counter"))).await;
    let ok = mcp
        .rename_method(Parameters(RenameMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            new_selector: "incrementBy".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&ok).contains("Counter"));

    let (_fake2, mcp2) = fake_mcp(respond_error("selector collision")).await;
    let error = mcp2
        .rename_method(Parameters(RenameMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            new_selector: "value".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(error.is_error, Some(true));
}

// --- flush / list_changes / dirty_methods / precheck_method / recheck_image ---

#[tokio::test]
async fn flush_success_no_filter() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: None,
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    assert_eq!(call_text(&result), "Flushed");
}

#[tokio::test]
async fn flush_scoped_by_class_with_confirm_destructive() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("1 file written"))).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: Some("Counter".to_string()),
            file: None,
            kind: None,
            confirm_destructive: Some(true),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("1 file written"));
}

#[tokio::test]
async fn flush_scoped_by_kind() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: Some("new-class".to_string()),
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
}

#[tokio::test]
async fn flush_rejects_multiple_filters() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .flush(Parameters(FlushParams {
            class: Some("Counter".to_string()),
            file: Some("src/counter.bt".to_string()),
            kind: None,
            confirm_destructive: None,
        }))
        .await
        .expect_err("class + file together should be rejected");
    assert!(err.message.contains("mutually exclusive"));
}

#[tokio::test]
async fn flush_rejects_invalid_kind() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: Some("bad kind!".to_string()),
            confirm_destructive: None,
        }))
        .await
        .expect_err("kind with punctuation should be rejected");
    assert!(err.message.contains("identifier"));
}

#[tokio::test]
async fn flush_error() {
    let (_fake, mcp) = fake_mcp(respond_error("conflict")).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: None,
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(true));
}

#[tokio::test]
async fn list_changes_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp.list_changes().await.unwrap();
    assert_eq!(call_text(&ok), "No changes");

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2.list_changes().await.unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn dirty_methods_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp.dirty_methods().await.unwrap();
    assert_eq!(call_text(&ok), "No dirty methods");

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2.dirty_methods().await.unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn precheck_method_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp
        .precheck_method(Parameters(PrecheckMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            body: "1".to_string(),
        }))
        .await
        .unwrap();
    assert!(call_text(&ok).contains("no findings"));

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2
        .precheck_method(Parameters(PrecheckMethodParams {
            class: "Counter".to_string(),
            selector: "increment".to_string(),
            body: "1".to_string(),
        }))
        .await
        .unwrap();
    assert_eq!(error.is_error, Some(true));
}

#[tokio::test]
async fn recheck_image_success_and_error() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let ok = mcp.recheck_image().await.unwrap();
    assert!(call_text(&ok).contains("no findings"));

    let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
    let error = mcp2.recheck_image().await.unwrap();
    assert_eq!(error.is_error, Some(true));
}

// --- tool_router (the "tool listing" dispatch surface) ---
//
// `#[tool_router]` generates `call_tool`/`list_tools` on `ServerHandler`
// itself, but those need a live `RequestContext<RoleServer>` (a `Peer`
// wired to a real transport) to invoke — plumbing that belongs to rmcp's
// own test suite, not ours. `ToolRouter::list_all`/`get`/`has_route` give
// the same registry data without that transport dependency, so the tool
// list every `#[tool]` method above populates is covered directly.

#[test]
fn tool_router_registers_every_tool_exactly_once() {
    let router = BeamtalkMcp::tool_router();
    let names: Vec<String> = router
        .list_all()
        .iter()
        .map(|t| t.name.to_string())
        .collect();

    let expected = [
        "evaluate",
        "complete",
        "load_project",
        "load_file",
        "inspect",
        "list_actors",
        "supervision_tree",
        "list_classes",
        "reload_class",
        "docs",
        "unload",
        "interrupt",
        "show_codegen",
        "test",
        "lint",
        "diagnostic_summary",
        "search_examples",
        "search_classes",
        "enable_tracing",
        "disable_tracing",
        "get_traces",
        "export_traces",
        "actor_stats",
        "describe",
        "list_packages",
        "package_classes",
        "save_method",
        "try_method",
        "save_class",
        "remove_method",
        "remove_class",
        "rename_class",
        "rename_method",
        "flush",
        "list_changes",
        "dirty_methods",
        "precheck_method",
        "recheck_image",
    ];
    for name in expected {
        assert!(
            names.iter().any(|n| n == name),
            "tool_router should register {name:?}, got {names:?}"
        );
        assert!(router.has_route(name), "has_route({name:?}) should be true");
        assert!(
            router.get(name).is_some(),
            "get({name:?}) should find a Tool"
        );
    }
    assert_eq!(
        names.len(),
        expected.len(),
        "tool_router registered an unexpected tool — update this test's `expected` list \
             alongside any new #[tool] handler, got {names:?}"
    );
}

#[test]
fn tool_router_rejects_unknown_tool_name() {
    let router = BeamtalkMcp::tool_router();
    assert!(!router.has_route("no_such_tool"));
    assert!(router.get("no_such_tool").is_none());
}

// --- ServerHandler::get_info ---

#[test]
fn get_info_advertises_tool_capabilities_and_instructions() {
    // get_info is synchronous and never touches the client, but
    // BeamtalkMcp::new needs one to construct — a disconnected client
    // would work equally well here; reuse fake_mcp's async constructor
    // via a tiny blocking runtime for symmetry with the rest of this
    // module. `fake` is kept alive (unused otherwise) for the same
    // reason every other test holds onto it: dropping it early would
    // abort the listener task that owns the socket.
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (fake, mcp) = rt.block_on(fake_mcp(respond(serde_json::json!({}))));
    let info = mcp.get_info();
    assert!(info.capabilities.tools.is_some());
    assert!(info.instructions.unwrap().contains("evaluate"));
    drop(fake);
}
