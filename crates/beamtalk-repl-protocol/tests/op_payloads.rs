// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The Rust half of the per-op wire payload fixture (BT-5049).
//!
//! `runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json` holds
//! one example reply per REPL op. The Erlang encoders are checked against it in
//! `beamtalk_repl_op_table_tests`; this file decodes the same replies into the
//! client types ([`ReplResponse`], [`ClassInfo`], [`ActorInfo`], …) and asserts
//! the fields clients read come out right, so a server-side shape change that
//! the Rust client would misread fails here and not in the field. The VS Code
//! twin is `editors/vscode/src/__tests__/opPayloads.test.ts`.
//!
//! Run it with `cargo test -p beamtalk-repl-protocol --test op_payloads`.

use std::collections::BTreeSet;
use std::path::PathBuf;

use beamtalk_repl_protocol::{ActorInfo, ClassInfo, ReplResponse, SessionInfo, StackFrameInfo};
use serde_json::Value;

fn root() -> PathBuf {
    beamtalk_workspace::cargo_workspace_root!()
}

fn read_json(rel: &str) -> Value {
    let path = root().join(rel);
    serde_json::from_str(&std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!("read {}: {e}", path.display());
    }))
    .unwrap_or_else(|e| panic!("parse {}: {e}", path.display()))
}

fn fixture() -> Value {
    read_json("runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json")
}

/// The raw JSON reply of payload `id`.
fn raw(id: &str) -> Value {
    fixture()["payloads"][id]["response"].clone()
}

/// Payload `id` decoded into the Rust client type.
fn reply(id: &str) -> ReplResponse {
    serde_json::from_value(raw(id)).unwrap_or_else(|e| panic!("payload `{id}` decodes: {e}"))
}

fn payload_ids() -> Vec<String> {
    fixture()["payloads"]
        .as_object()
        .expect("payloads is an object")
        .keys()
        .cloned()
        .collect()
}

fn key_set(v: &Value) -> BTreeSet<String> {
    v.as_object().expect("object").keys().cloned().collect()
}

/// Every op in the server's op table has a payload or a stated reason for none,
/// never both (the Erlang test checks the same from the other side).
#[test]
fn every_op_is_pinned_or_explained() {
    let table = read_json("runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json");
    let table_ops: BTreeSet<String> = table
        .as_array()
        .expect("op table is an array")
        .iter()
        .map(|e| e["op"].as_str().expect("op").to_owned())
        .collect();

    let fx = fixture();
    let pinned: BTreeSet<String> = fx["payloads"]
        .as_object()
        .expect("payloads")
        .values()
        .map(|p| p["op"].as_str().expect("payload op").to_owned())
        .collect();
    let explained = key_set(&fx["unpinned"]);

    assert!(
        pinned.is_disjoint(&explained),
        "ops both pinned and unpinned: {:?}",
        pinned.intersection(&explained).collect::<Vec<_>>()
    );
    let covered: BTreeSet<String> = pinned.union(&explained).cloned().collect();
    assert_eq!(covered, table_ops, "payload fixture vs repl_op_table.json");
    for (op, reason) in fx["unpinned"].as_object().expect("unpinned") {
        assert!(
            reason.as_str().is_some_and(|r| !r.is_empty()),
            "`unpinned.{op}` needs a reason"
        );
    }
}

/// Every payload decodes into `ReplResponse`, and every top-level key it carries
/// is one `response_keys.json` declares (so a new key is added to that list, and
/// to `ReplResponse` or its Erlang-only list, in the same change).
#[test]
fn every_payload_decodes_and_uses_declared_keys() {
    let keys = read_json("tests/repl-protocol/response_keys.json");
    let declared: BTreeSet<String> = ["rust_keys", "erlang_only_keys"]
        .iter()
        .flat_map(|list| {
            keys[list]
                .as_array()
                .expect("key list")
                .iter()
                .map(|k| k.as_str().expect("key").to_owned())
        })
        .collect();
    for id in payload_ids() {
        let value = raw(&id);
        let undeclared: Vec<_> = key_set(&value).difference(&declared).cloned().collect();
        assert!(
            undeclared.is_empty(),
            "payload `{id}` carries keys response_keys.json does not declare: {undeclared:?}"
        );
        // The envelope the fixture promises.
        assert_eq!(value["id"], "msg-1", "payload `{id}`");
        assert_eq!(value["session"], "sess-1", "payload `{id}`");
        let _ = reply(&id);
    }
}

#[test]
fn every_error_payload_is_an_error_and_every_other_is_not() {
    for id in payload_ids() {
        let r = reply(&id);
        assert_eq!(r.is_error(), id.ends_with("error"), "payload `{id}`");
        if r.is_error() {
            assert!(r.error_message().is_some(), "payload `{id}` has a message");
        }
    }
}

#[test]
fn eval_payloads() {
    let ok = reply("eval");
    assert_eq!(ok.value_string(), "3");
    assert_eq!(ok.value_type.as_deref(), Some("integer"));
    assert!(ok.warning_entries().is_empty());

    let noisy = reply("eval/output_and_warnings");
    assert_eq!(noisy.output.as_deref(), Some("hi\n"));
    assert_eq!(noisy.value_type.as_deref(), Some("string"));
    let warnings = noisy.warning_entries();
    assert_eq!(warnings.len(), 1);
    assert_eq!(warnings[0].message, "Unused variable: x");
    assert_eq!(
        warnings[0].hint.as_deref(),
        Some("remove it or prefix with _")
    );
    assert_eq!(
        noisy.warnings.as_deref(),
        Some(&[warnings[0].message.clone()][..])
    );

    let trace = reply("eval/trace");
    let steps = trace.steps.expect("steps");
    assert_eq!(steps.len(), 2);
    assert_eq!(steps[0]["src"], "1 + 2");
    assert_eq!(steps[0]["value"], 3);

    assert_eq!(reply("eval/script_exit").exit_code, Some(3));

    let err = reply("eval/error");
    assert_eq!(err.error_message(), Some("Error: boom"));
    assert_eq!(err.line, Some(3));
    assert_eq!(err.hint.as_deref(), Some("check the receiver"));
    assert_eq!(err.output.as_deref(), Some("out\n"));
    let frames: Vec<StackFrameInfo> = err.frames.expect("frames decode");
    assert_eq!(frames.len(), 2);
    assert_eq!(
        frames[0].raw_mfa().as_deref(),
        Some("bt@counter:increment/2")
    );
    assert!(!frames[0].hidden);
    assert!(frames[1].hidden);
    assert_eq!(frames[1].file, None);
}

#[test]
fn get_traces_error_names_the_missing_class() {
    let err = reply("get-traces/error");
    assert!(err.is_error());
    assert_eq!(err.error_message(), Some("Class not found: Missing"));
}

#[test]
fn list_classes_rows_decode_into_class_info() {
    let rows = reply("list-classes").class_list.expect("class_list");
    assert_eq!(rows.len(), 2);

    let counter: &ClassInfo = &rows[0];
    assert_eq!(counter.name, "Counter");
    assert_eq!(counter.superclass.as_deref(), Some("Actor"));
    assert_eq!(counter.doc.as_deref(), Some("A counter."));
    assert!(!counter.sealed && !counter.is_abstract && !counter.internal);
    assert_eq!(
        counter.source_file.as_deref(),
        Some("/work/proj/src/counter.bt")
    );
    assert_eq!(counter.actor_count, Some(2));
    assert_eq!(counter.source_origin.as_deref(), Some("project"));

    let object = &rows[1];
    assert_eq!(object.superclass, None);
    assert_eq!(object.doc, None);
    assert!(object.is_abstract);
    assert_eq!(object.source_file, None);
    assert_eq!(object.actor_count, Some(0));
    assert_eq!(object.source_origin.as_deref(), Some("stdlib"));
}

/// `ClassInfo` models every key the server sends for a class row. A key added
/// on the Erlang side (and so to the fixture) must be modelled here or listed
/// as deliberately ignored.
#[test]
fn class_info_models_every_key_the_server_sends() {
    let rows = raw("list-classes")["class_list"].clone();
    let sent = key_set(&rows[0]);
    let info: ClassInfo = serde_json::from_value(rows[0].clone()).expect("ClassInfo");
    let modelled = key_set(&serde_json::to_value(&info).expect("serialise"));
    // `abstract` is the wire name of `is_abstract`; serde renames it back.
    let ignored: BTreeSet<String> = BTreeSet::new();
    let missing: Vec<_> = sent
        .difference(&modelled)
        .filter(|k| !ignored.contains(*k))
        .collect();
    assert!(
        missing.is_empty(),
        "ClassInfo does not model class-row keys {missing:?}"
    );
}

#[test]
fn actors_decode_into_actor_info() {
    let actors: Vec<ActorInfo> = reply("actors").actors.expect("actors");
    assert_eq!(actors.len(), 2);
    assert_eq!(actors[0].pid, "<0.123.0>");
    assert_eq!(actors[0].class, "Counter");
    assert_eq!(actors[0].module, "bt@counter");
    assert_eq!(actors[0].spawned_at, Some(1_234_567_890));
    assert!(
        !actors[0].system,
        "the wire omits `system` for a user actor"
    );
    assert_eq!(actors[1].spawned_at, None);
    assert!(actors[1].system);
}

#[test]
fn sessions_decode_into_session_info() {
    let sessions: Vec<SessionInfo> = reply("sessions").sessions.expect("sessions");
    assert_eq!(sessions.len(), 1);
    // A session id (what `resume` accepts), not a pid string (BT-5012).
    assert_eq!(sessions[0].id, "sess-1");
    assert_eq!(sessions[0].created_at, None);
}

#[test]
fn test_payload_carries_results_and_a_test_error_status() {
    let r = reply("test");
    assert!(r.has_test_error());
    assert!(!r.is_error());
    let results = r.results.expect("results");
    assert_eq!(results["total"], 3);
    assert_eq!(results["passed"], 1);
    assert_eq!(results["failed"], 1);
    assert_eq!(results["skipped"], 1);
    let tests = results["tests"].as_array().expect("tests");
    let status: Vec<_> = tests
        .iter()
        .map(|t| t["status"].as_str().unwrap())
        .collect();
    assert_eq!(status, ["pass", "fail", "skip"]);
    assert_eq!(tests[1]["error"], "expected 1, got 2");
    assert_eq!(tests[2]["reason"], "not yet");
}

#[test]
fn load_payloads() {
    let source = reply("load-source");
    assert_eq!(source.classes.as_deref(), Some(&["Counter".to_owned()][..]));
    assert_eq!(source.warning_entries().len(), 1);

    let project = reply("load-project");
    assert_eq!(project.summary.as_deref(), Some("Loaded 1 class, 1 failed"));
    assert_eq!(project.errors.len(), 1);
    assert_eq!(project.errors[0]["path"], "src/bad.bt");
    assert_eq!(project.errors[0]["kind"], "compile_error");
    // A load-project reply is not an error reply, but still reports the failure.
    assert!(!project.is_error());
    assert_eq!(project.error_message(), Some("Unexpected token"));
}

#[test]
fn simple_payloads() {
    assert_eq!(
        reply("complete").completions.as_deref(),
        Some(&["Counter".to_owned(), "CounterTest".to_owned()][..])
    );
    assert_eq!(
        reply("show-codegen").core_erlang.as_deref(),
        Some("module 'bt@counter' []")
    );
    assert_eq!(reply("show-codegen").warnings.map(|w| w.len()), Some(1));
    assert_eq!(
        reply("erlang-help").docs.as_deref(),
        Some("lists:map/2\n\nApply a function.")
    );
    assert_eq!(reply("close").value_string(), "ok");
    let describe = reply("describe");
    assert!(describe.ops.expect("ops")["eval"]["params"].is_array());
    assert_eq!(describe.versions.expect("versions")["protocol"], "2.0");
    assert_eq!(reply("inspect").state.expect("state")["count"], 3);
}

/// Replies the Rust client reads out of `value` or the raw JSON: nested shapes
/// the typed struct deliberately leaves as `serde_json::Value`.
#[test]
fn value_and_raw_payloads_keep_their_nested_shape() {
    let sites = reply("nav-query").value.expect("value");
    assert_eq!(sites["sites"][0]["class"], "Counter");
    assert_eq!(sites["sites"][0]["class_side"], false);
    assert_eq!(sites["sites"][0]["line"], 12);

    let classes = reply("browse-classes").value.expect("value");
    assert_eq!(classes[0]["name"], "Counter");
    assert_eq!(classes[0]["is_protocol"], false);

    let protocols = reply("browse-protocols").value.expect("value");
    assert_eq!(protocols["protocols"][0]["name"], "accessing");
    assert_eq!(
        protocols["protocols"][0]["selectors"][0]["selector"],
        "increment"
    );

    let aliases = reply("browse-type-aliases").value.expect("value");
    assert_eq!(aliases[0]["expansion"], "Integer");

    // `methods` and `inherited-methods` carry keys `ReplResponse` does not model;
    // clients read them from the raw JSON.
    let methods = raw("methods");
    assert_eq!(methods["methods"][0]["side"], "instance");
    assert_eq!(methods["state_vars"][1]["line"], Value::Null);
    assert_eq!(
        raw("inherited-methods")["methods"][0]["defining_class"],
        "Object"
    );
    assert_eq!(raw("health")["workspace_id"], "ws-1");
    assert_eq!(raw("diagnostics")["diagnostics"][0]["start"], 4);
}
