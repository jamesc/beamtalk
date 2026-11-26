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

/// A fake REPL that answers only the exact request `expected` (compared on
/// every field but the per-request `id`): any other request, such as an `eval`
/// carrying a spliced expression, gets an error reply.
fn respond_only_to(expected: serde_json::Value, reply: serde_json::Value) -> FakeReplResponder {
    Box::new(move |req| {
        let mut got = req.clone();
        if let Some(map) = got.as_object_mut() {
            map.remove("id");
        }
        if got == expected {
            reply.clone()
        } else {
            serde_json::json!({
                "status": ["done", "error"],
                "error": format!("unexpected request: {got}")
            })
        }
    })
}

#[tokio::test]
async fn save_method_dispatches_the_save_method_op_with_structured_params() {
    // The body carries a quote, a brace and a backslash: it must reach the
    // workspace byte for byte, as a value, with no escaping and no `eval`.
    let body = "\"he said {x}\" \\ ^1";
    let (_fake, mcp) = fake_mcp(respond_only_to(
        serde_json::json!({
            "op": "save-method",
            "class": "Counter",
            "selector": "at:put:",
            "source": body,
            // ADR 0082: an MCP save is the agent's, never the connection's own
            // `repl`/human author. `respond_only_to` matches every field, so
            // this fails if the attribution is dropped.
            "author": "mcp:save_method",
            "author_kind": "agent"
        }),
        serde_json::json!({"value": {"class": "Counter", "selector": "at:put:"}}),
    ))
    .await;
    let result = mcp
        .save_method(Parameters(SaveMethodParams {
            class: "Counter".to_string(),
            selector: "#at:put:".to_string(),
            body: body.to_string(),
        }))
        .await
        .unwrap();
    assert_ne!(result.is_error, Some(true), "{}", call_text(&result));
    assert_eq!(call_text(&result), "Counter");
}

#[tokio::test]
async fn save_class_dispatches_the_save_class_op_with_structured_params() {
    let source = "Object subclass: Greeter\n  greet => \"hi {name}\"";
    let (_fake, mcp) = fake_mcp(respond_only_to(
        serde_json::json!({
            "op": "save-class",
            "source": source,
            "path": "src/greeter.bt",
            "author": "mcp:save_class",
            "author_kind": "agent"
        }),
        serde_json::json!({"value": {"path": "src/greeter.bt"}}),
    ))
    .await;
    let result = mcp
        .save_class(Parameters(SaveClassParams {
            source: source.to_string(),
            path: "src/greeter.bt".to_string(),
        }))
        .await
        .unwrap();
    assert_ne!(result.is_error, Some(true), "{}", call_text(&result));
    assert_eq!(call_text(&result), "New class queued for src/greeter.bt");
}

#[tokio::test]
async fn save_method_reports_changelog_warnings() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "value": {
            "class": "Counter",
            "selector": "inc",
            "warnings": ["The change is live but ChangeLog append failed"]
        }
    })))
    .await;
    let result = mcp
        .save_method(Parameters(SaveMethodParams {
            class: "Counter".to_string(),
            selector: "inc".to_string(),
            body: "1".to_string(),
        }))
        .await
        .unwrap();
    let text = call_text(&result);
    assert!(text.starts_with("Counter"), "{text}");
    assert!(
        text.contains("Warning: The change is live but ChangeLog append failed"),
        "{text}"
    );
}

#[tokio::test]
async fn save_class_reports_changelog_warnings() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
        "value": {"path": "src/greeter.bt", "warnings": ["ChangeLog append failed: disk full"]}
    })))
    .await;
    let result = mcp
        .save_class(Parameters(SaveClassParams {
            source: "Object subclass: Greeter".to_string(),
            path: "src/greeter.bt".to_string(),
        }))
        .await
        .unwrap();
    let text = call_text(&result);
    assert!(
        text.contains("New class queued for src/greeter.bt"),
        "{text}"
    );
    assert!(
        text.contains("Warning: ChangeLog append failed: disk full"),
        "{text}"
    );
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
    assert!(
        call_text(&ok).contains("Workspace: "),
        "names the workspace"
    );

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
            selector: None,
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    // BT-5080: the output names the attached workspace.
    assert_eq!(call_text(&result), "Flushed\nWorkspace: (explicit --port)");
}

#[tokio::test]
async fn flush_scoped_by_class_with_confirm_destructive() {
    let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("1 file written"))).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: Some("Counter".to_string()),
            file: None,
            kind: None,
            selector: None,
            confirm_destructive: Some(true),
        }))
        .await
        .unwrap();
    assert!(call_text(&result).contains("1 file written"));
}

/// A `flush` request carries the scope and the confirmation as structured
/// params and nothing is evaluated: only the exact request gets a good reply.
#[tokio::test]
async fn flush_dispatches_the_flush_op_with_structured_params() {
    let summary = serde_json::json!({
        "flushed": 0,
        "files": [],
        "skipped": [{"seq": 2, "class": "Gone", "reason": "destructive"}],
        "conflicts": []
    });
    let (_fake, mcp) = fake_mcp(respond_only_to(
        serde_json::json!({"op": "flush"}),
        serde_json::json!({"value": summary.clone()}),
    ))
    .await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: None,
            selector: None,
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    assert_ne!(result.is_error, Some(true), "{}", call_text(&result));
    // A withheld destructive entry is visible in the answer.
    assert!(call_text(&result).contains("\"reason\":\"destructive\""));

    for (params, expected) in [
        (
            FlushParams {
                class: Some("Counter".to_string()),
                file: None,
                kind: None,
                selector: None,
                confirm_destructive: Some(true),
            },
            serde_json::json!({
                "op": "flush", "class": "Counter", "confirm_destructive": true
            }),
        ),
        (
            FlushParams {
                class: None,
                file: Some("src/counter.bt".to_string()),
                kind: None,
                selector: None,
                confirm_destructive: Some(false),
            },
            serde_json::json!({"op": "flush", "file": "src/counter.bt"}),
        ),
        (
            FlushParams {
                class: None,
                file: None,
                kind: Some("#'new-class'".to_string()),
                selector: None,
                confirm_destructive: None,
            },
            serde_json::json!({"op": "flush", "kind": "new-class"}),
        ),
        (
            FlushParams {
                class: None,
                file: None,
                kind: None,
                selector: None,
                confirm_destructive: Some(true),
            },
            serde_json::json!({"op": "flush", "confirm_destructive": true}),
        ),
    ] {
        let (_fake, mcp) = fake_mcp(respond_only_to(
            expected.clone(),
            serde_json::json!({"value": summary.clone()}),
        ))
        .await;
        let result = mcp.flush(Parameters(params)).await.unwrap();
        assert_ne!(
            result.is_error,
            Some(true),
            "{expected}: {}",
            call_text(&result)
        );
    }
}

#[tokio::test]
async fn flush_scoped_by_kind() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let result = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: Some("new-class".to_string()),
            selector: None,
            confirm_destructive: None,
        }))
        .await
        .unwrap();
    assert_eq!(result.is_error, Some(false));
}

/// BT-5297: a selector scope is sent as the op's `selector` param, alone or
/// with its class; `#` and quotes are stripped like the `kind` spelling.
#[tokio::test]
async fn flush_selector_scope_dispatches_the_selector_param() {
    let summary = serde_json::json!({"flushed": 1, "files": [], "skipped": [], "conflicts": []});
    for (class, selector, expected) in [
        (
            None,
            "increment",
            serde_json::json!({"op": "flush", "selector": "increment"}),
        ),
        (
            Some("Counter"),
            "#'at:put:'",
            serde_json::json!({"op": "flush", "class": "Counter", "selector": "at:put:"}),
        ),
    ] {
        let (_fake, mcp) = fake_mcp(respond_only_to(
            expected.clone(),
            serde_json::json!({"value": summary.clone()}),
        ))
        .await;
        let result = mcp
            .flush(Parameters(FlushParams {
                class: class.map(str::to_string),
                file: None,
                kind: None,
                selector: Some(selector.to_string()),
                confirm_destructive: None,
            }))
            .await
            .unwrap();
        assert_ne!(
            result.is_error,
            Some(true),
            "{expected}: {}",
            call_text(&result)
        );
    }
}

#[tokio::test]
async fn flush_selector_combines_only_with_class() {
    for (file, kind, selector) in [
        (Some("a.bt"), None, "increment"),
        (None, Some("new-class"), "increment"),
        (None, None, ""),
        (None, None, "#"),
    ] {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"flushed": 1}))).await;
        mcp.flush(Parameters(FlushParams {
            class: None,
            file: file.map(str::to_string),
            kind: kind.map(str::to_string),
            selector: Some(selector.to_string()),
            confirm_destructive: Some(true),
        }))
        .await
        .expect_err("an unreadable or conflicting selector scope should be rejected");
    }
}

#[tokio::test]
async fn flush_rejects_multiple_filters() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .flush(Parameters(FlushParams {
            class: Some("Counter".to_string()),
            file: Some("src/counter.bt".to_string()),
            kind: None,
            selector: None,
            confirm_destructive: None,
        }))
        .await
        .expect_err("class + file together should be rejected");
    assert!(err.message.contains("mutually exclusive"));
}

/// An empty scope used to read as "no scope" and flush every pending change.
#[tokio::test]
async fn flush_rejects_an_empty_scope() {
    // The fake answers anything, so a request that reached it would succeed.
    for (class, file, kind, name) in [
        (Some(""), None, None, "class"),
        (None, Some(""), None, "file"),
        (None, None, Some(""), "kind"),
    ] {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"flushed": 1}))).await;
        let err = mcp
            .flush(Parameters(FlushParams {
                class: class.map(str::to_string),
                file: file.map(str::to_string),
                kind: kind.map(str::to_string),
                selector: None,
                confirm_destructive: Some(true),
            }))
            .await
            .expect_err("an empty scope should be rejected");
        assert!(
            err.message.contains(name) && err.message.contains("empty"),
            "{}",
            err.message
        );
    }
}

#[tokio::test]
async fn flush_rejects_invalid_kind() {
    let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
    let err = mcp
        .flush(Parameters(FlushParams {
            class: None,
            file: None,
            kind: Some("bad kind!".to_string()),
            selector: None,
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
            selector: None,
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
        "nodes",
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
    // BT-4812: server info carries the build identity, not Cargo's bare version.
    let server = info.server_info;
    assert_eq!(server.name, "beamtalk-mcp");
    assert_eq!(
        server.version,
        beamtalk_repl_protocol::handshake::build_id()
    );
    drop(fake);
}
