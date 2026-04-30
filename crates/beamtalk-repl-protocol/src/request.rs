// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Request builders for the REPL JSON protocol.
//!
//! Each method returns a `serde_json::Value` ready to be serialized and sent
//! over the WebSocket transport. The transport layer is intentionally not
//! included here — callers provide their own sync or async WebSocket client.

use std::sync::atomic::{AtomicU64, Ordering};

/// Counter for generating unique message IDs.
static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique, monotonically increasing message ID.
///
/// IDs are formatted as `msg-001`, `msg-002`, etc. The counter is global
/// and atomic, so IDs are unique across threads and client instances within
/// a single process.
pub fn next_msg_id() -> String {
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}

/// Builder for REPL protocol JSON requests.
///
/// Provides typed constructors for every REPL operation. All methods return
/// a `serde_json::Value` with the correct `op`, `id`, and parameters.
#[derive(Debug)]
pub struct RequestBuilder;

impl RequestBuilder {
    // --- Core operations ---

    /// Build an `eval` request.
    #[must_use]
    pub fn eval(code: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "eval",
            "id": next_msg_id(),
            "code": code
        })
    }

    /// Build an `eval` request with optional trace mode (BT-1238).
    #[must_use]
    pub fn eval_with_trace(code: &str, trace: bool) -> serde_json::Value {
        let mut req = Self::eval(code);
        if trace {
            req["trace"] = serde_json::Value::Bool(true);
        }
        req
    }

    /// Build a `complete` request.
    #[must_use]
    pub fn complete(code: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "complete",
            "id": next_msg_id(),
            "code": code
        })
    }

    /// Build a `complete` request with cursor position.
    #[must_use]
    pub fn complete_with_cursor(code: &str, cursor: usize) -> serde_json::Value {
        serde_json::json!({
            "op": "complete",
            "id": next_msg_id(),
            "code": code,
            "cursor": cursor
        })
    }

    /// Build a `complete` request with cursor position and an optional session ID.
    #[must_use]
    pub fn complete_with_session(
        code: &str,
        cursor: usize,
        session: Option<&str>,
    ) -> serde_json::Value {
        let mut req = Self::complete_with_cursor(code, cursor);
        if let Some(sid) = session {
            req["session"] = serde_json::Value::String(sid.to_owned());
        }
        req
    }

    /// Build an `erlang-complete` request with optional module filter.
    #[must_use]
    pub fn erlang_complete(prefix: &str, module: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "erlang-complete",
            "id": next_msg_id(),
            "prefix": prefix,
        });
        if let Some(m) = module {
            req["module"] = serde_json::Value::String(m.to_owned());
        }
        req
    }

    /// Build an `info` request.
    #[must_use]
    pub fn info(symbol: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "info",
            "id": next_msg_id(),
            "symbol": symbol
        })
    }

    /// Build a `show-codegen` request for an expression.
    #[must_use]
    pub fn show_codegen(code: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "show-codegen",
            "id": next_msg_id(),
            "code": code
        })
    }

    /// Build a `show-codegen` request for a loaded class (BT-1236).
    #[must_use]
    pub fn show_codegen_class(class: &str, selector: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "show-codegen",
            "id": next_msg_id(),
            "class": class
        });
        if let Some(sel) = selector {
            req["selector"] = serde_json::Value::String(sel.to_owned());
        }
        req
    }

    /// Build a `load-source` request.
    #[must_use]
    pub fn load_source(source: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "load-source",
            "id": next_msg_id(),
            "source": source
        })
    }

    /// Build a `load-project` request.
    #[must_use]
    pub fn load_project(path: &str, include_tests: bool) -> serde_json::Value {
        serde_json::json!({
            "op": "load-project",
            "id": next_msg_id(),
            "path": path,
            "include_tests": include_tests
        })
    }

    /// Build a `load-project` request with force flag.
    #[must_use]
    pub fn load_project_with_force(
        path: &str,
        include_tests: bool,
        force: bool,
    ) -> serde_json::Value {
        serde_json::json!({
            "op": "load-project",
            "id": next_msg_id(),
            "path": path,
            "include_tests": include_tests,
            "force": force
        })
    }

    // --- Session operations ---

    /// Build a `clear` request.
    #[must_use]
    pub fn clear() -> serde_json::Value {
        Self::no_param("clear")
    }

    /// Build a `bindings` request.
    #[must_use]
    pub fn bindings() -> serde_json::Value {
        Self::no_param("bindings")
    }

    /// Build a `sessions` request.
    #[must_use]
    pub fn sessions() -> serde_json::Value {
        Self::no_param("sessions")
    }

    /// Build a `clone` request.
    #[must_use]
    pub fn clone_session() -> serde_json::Value {
        Self::no_param("clone")
    }

    /// Build a `close` request.
    #[must_use]
    pub fn close() -> serde_json::Value {
        Self::no_param("close")
    }

    // --- Actor operations ---

    /// Build an `actors` request.
    #[must_use]
    pub fn actors() -> serde_json::Value {
        Self::no_param("actors")
    }

    /// Build an `inspect` request.
    #[must_use]
    pub fn inspect(actor: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "inspect",
            "id": next_msg_id(),
            "actor": actor
        })
    }

    /// Build a `kill` request.
    #[must_use]
    pub fn kill(actor: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "kill",
            "id": next_msg_id(),
            "actor": actor
        })
    }

    /// Build an `interrupt` request.
    #[must_use]
    pub fn interrupt() -> serde_json::Value {
        Self::no_param("interrupt")
    }

    /// Build an `interrupt` request with a session ID.
    #[must_use]
    pub fn interrupt_with_session(session: &str) -> serde_json::Value {
        let mut req = Self::interrupt();
        req["session"] = serde_json::Value::String(session.to_owned());
        req
    }

    // --- Module operations ---

    /// Build an `unload` request.
    #[must_use]
    pub fn unload(module: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "unload",
            "id": next_msg_id(),
            "module": module
        })
    }

    // --- Test operations ---

    /// Build a `test` request for a class.
    #[must_use]
    pub fn test_class(class: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "test",
            "id": next_msg_id(),
            "class": class
        })
    }

    /// Build a `test` request for a file.
    #[must_use]
    pub fn test_file(file: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "test",
            "id": next_msg_id(),
            "file": file
        })
    }

    /// Build a `test` request for a class with an optional method filter.
    #[must_use]
    pub fn test_method(class: &str, method: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "test",
            "id": next_msg_id(),
            "class": class,
            "method": method
        })
    }

    /// Build a `test-all` request.
    #[must_use]
    pub fn test_all() -> serde_json::Value {
        Self::no_param("test-all")
    }

    // --- Documentation operations ---

    /// Build an `erlang-help` request (BT-1852).
    #[must_use]
    pub fn erlang_help(module: &str, function: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "erlang-help",
            "id": next_msg_id(),
            "module": module
        });
        if let Some(func) = function {
            req["function"] = serde_json::Value::String(func.to_owned());
        }
        req
    }

    // --- Server / discovery operations ---

    /// Build a `describe` request.
    #[must_use]
    pub fn describe() -> serde_json::Value {
        Self::no_param("describe")
    }

    /// Build a `health` request.
    #[must_use]
    pub fn health() -> serde_json::Value {
        Self::no_param("health")
    }

    /// Build a `shutdown` request.
    #[must_use]
    pub fn shutdown(cookie: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "shutdown",
            "id": next_msg_id(),
            "cookie": cookie
        })
    }

    /// Build a `list-classes` request (BT-1404).
    #[must_use]
    pub fn list_classes(filter: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "list-classes",
            "id": next_msg_id()
        });
        if let Some(f) = filter {
            req["filter"] = serde_json::Value::String(f.to_owned());
        }
        req
    }

    // --- Tracing operations (ADR 0069) ---

    /// Build an `enable-tracing` request.
    #[must_use]
    pub fn enable_tracing() -> serde_json::Value {
        Self::no_param("enable-tracing")
    }

    /// Build a `disable-tracing` request.
    #[must_use]
    pub fn disable_tracing() -> serde_json::Value {
        Self::no_param("disable-tracing")
    }

    /// Build a `get-traces` request with optional filters (ADR 0069).
    #[must_use]
    pub fn get_traces(
        actor: Option<&str>,
        selector: Option<&str>,
        class: Option<&str>,
        outcome: Option<&str>,
        min_duration_ns: Option<u64>,
        limit: Option<u32>,
    ) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "get-traces",
            "id": next_msg_id()
        });
        if let Some(a) = actor {
            req["actor"] = serde_json::Value::String(a.to_owned());
        }
        if let Some(s) = selector {
            req["selector"] = serde_json::Value::String(s.to_owned());
        }
        if let Some(c) = class {
            req["class"] = serde_json::Value::String(c.to_owned());
        }
        if let Some(o) = outcome {
            req["outcome"] = serde_json::Value::String(o.to_owned());
        }
        if let Some(d) = min_duration_ns {
            req["min_duration_ns"] = serde_json::json!(d);
        }
        if let Some(l) = limit {
            req["limit"] = serde_json::json!(l);
        }
        req
    }

    /// Build an `actor-stats` request with optional actor filter (ADR 0069).
    #[must_use]
    pub fn actor_stats(actor: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "actor-stats",
            "id": next_msg_id()
        });
        if let Some(a) = actor {
            req["actor"] = serde_json::Value::String(a.to_owned());
        }
        req
    }

    /// Build an `export-traces` request with optional filters (ADR 0069).
    #[must_use]
    pub fn export_traces(
        path: Option<&str>,
        actor: Option<&str>,
        selector: Option<&str>,
        class: Option<&str>,
        outcome: Option<&str>,
        min_duration_ns: Option<u64>,
        limit: Option<u32>,
    ) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "export-traces",
            "id": next_msg_id()
        });
        if let Some(p) = path {
            req["path"] = serde_json::Value::String(p.to_owned());
        }
        if let Some(a) = actor {
            req["actor"] = serde_json::Value::String(a.to_owned());
        }
        if let Some(s) = selector {
            req["selector"] = serde_json::Value::String(s.to_owned());
        }
        if let Some(c) = class {
            req["class"] = serde_json::Value::String(c.to_owned());
        }
        if let Some(o) = outcome {
            req["outcome"] = serde_json::Value::String(o.to_owned());
        }
        if let Some(d) = min_duration_ns {
            req["min_duration_ns"] = serde_json::json!(d);
        }
        if let Some(l) = limit {
            req["limit"] = serde_json::json!(l);
        }
        req
    }

    // --- Stdin (BT-698) ---

    /// Build a `stdin` request to provide input to a running eval.
    #[must_use]
    pub fn stdin(id: &str, value: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "stdin",
            "id": id,
            "value": value
        })
    }

    // --- Internal helpers ---

    /// Build a no-parameter request for the given op.
    fn no_param(op: &str) -> serde_json::Value {
        serde_json::json!({"op": op, "id": next_msg_id()})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_request_has_correct_shape() {
        let req = RequestBuilder::eval("1 + 2");
        assert_eq!(req["op"], "eval");
        assert_eq!(req["code"], "1 + 2");
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
    }

    #[test]
    fn msg_ids_are_monotonic() {
        let id1 = next_msg_id();
        let id2 = next_msg_id();
        assert_ne!(id1, id2);
    }

    #[test]
    fn eval_with_trace_true_includes_field() {
        let req = RequestBuilder::eval_with_trace("1 + 2", true);
        assert_eq!(req["op"], "eval");
        assert_eq!(req["code"], "1 + 2");
        assert_eq!(req["trace"], true);
    }

    #[test]
    fn eval_with_trace_false_omits_field() {
        let req = RequestBuilder::eval_with_trace("1 + 2", false);
        assert_eq!(req["op"], "eval");
        assert!(req["trace"].is_null());
    }

    #[test]
    fn complete_request_has_correct_shape() {
        let req = RequestBuilder::complete("Arr");
        assert_eq!(req["op"], "complete");
        assert_eq!(req["code"], "Arr");
    }

    #[test]
    fn complete_with_cursor_includes_cursor() {
        let req = RequestBuilder::complete_with_cursor("foo ", 4);
        assert_eq!(req["op"], "complete");
        assert_eq!(req["code"], "foo ");
        assert_eq!(req["cursor"], 4);
    }

    #[test]
    fn complete_with_session_some_includes_session() {
        let req = RequestBuilder::complete_with_session("Arr", 3, Some("sess-1"));
        assert_eq!(req["op"], "complete");
        assert_eq!(req["session"], "sess-1");
    }

    #[test]
    fn complete_with_session_none_omits_session() {
        let req = RequestBuilder::complete_with_session("Arr", 3, None);
        assert_eq!(req["op"], "complete");
        assert!(req["session"].is_null());
    }

    #[test]
    fn erlang_complete_without_module() {
        let req = RequestBuilder::erlang_complete("ets:", None);
        assert_eq!(req["op"], "erlang-complete");
        assert_eq!(req["prefix"], "ets:");
        assert!(req["module"].is_null());
    }

    #[test]
    fn erlang_complete_with_module() {
        let req = RequestBuilder::erlang_complete("insert", Some("ets"));
        assert_eq!(req["op"], "erlang-complete");
        assert_eq!(req["prefix"], "insert");
        assert_eq!(req["module"], "ets");
    }

    #[test]
    fn info_request_has_correct_shape() {
        let req = RequestBuilder::info("Counter");
        assert_eq!(req["op"], "info");
        assert_eq!(req["symbol"], "Counter");
    }

    #[test]
    fn show_codegen_request_has_correct_shape() {
        let req = RequestBuilder::show_codegen("1 + 2");
        assert_eq!(req["op"], "show-codegen");
        assert_eq!(req["code"], "1 + 2");
    }

    #[test]
    fn show_codegen_class_without_selector() {
        let req = RequestBuilder::show_codegen_class("Counter", None);
        assert_eq!(req["op"], "show-codegen");
        assert_eq!(req["class"], "Counter");
        assert!(req["selector"].is_null());
    }

    #[test]
    fn show_codegen_class_with_selector() {
        let req = RequestBuilder::show_codegen_class("Counter", Some("increment"));
        assert_eq!(req["op"], "show-codegen");
        assert_eq!(req["class"], "Counter");
        assert_eq!(req["selector"], "increment");
    }

    #[test]
    fn load_source_request_has_correct_shape() {
        let req = RequestBuilder::load_source("class Foo\n  x => 42");
        assert_eq!(req["op"], "load-source");
        assert_eq!(req["source"], "class Foo\n  x => 42");
    }

    #[test]
    fn load_project_request_has_correct_shape() {
        let req = RequestBuilder::load_project("/my/project", false);
        assert_eq!(req["op"], "load-project");
        assert_eq!(req["path"], "/my/project");
        assert_eq!(req["include_tests"], false);
        assert!(req["force"].is_null());
    }

    #[test]
    fn load_project_with_force() {
        let req = RequestBuilder::load_project_with_force(".", true, true);
        assert_eq!(req["op"], "load-project");
        assert_eq!(req["path"], ".");
        assert_eq!(req["include_tests"], true);
        assert_eq!(req["force"], true);
    }

    #[test]
    fn clear_request_has_correct_op() {
        let req = RequestBuilder::clear();
        assert_eq!(req["op"], "clear");
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
    }

    #[test]
    fn bindings_request_has_correct_op() {
        let req = RequestBuilder::bindings();
        assert_eq!(req["op"], "bindings");
    }

    #[test]
    fn sessions_request_has_correct_op() {
        let req = RequestBuilder::sessions();
        assert_eq!(req["op"], "sessions");
    }

    #[test]
    fn clone_session_request_has_correct_op() {
        let req = RequestBuilder::clone_session();
        assert_eq!(req["op"], "clone");
    }

    #[test]
    fn close_request_has_correct_op() {
        let req = RequestBuilder::close();
        assert_eq!(req["op"], "close");
    }

    #[test]
    fn actors_request_has_correct_op() {
        let req = RequestBuilder::actors();
        assert_eq!(req["op"], "actors");
    }

    #[test]
    fn inspect_request_has_correct_shape() {
        let req = RequestBuilder::inspect("Counter@pid");
        assert_eq!(req["op"], "inspect");
        assert_eq!(req["actor"], "Counter@pid");
    }

    #[test]
    fn kill_request_has_correct_shape() {
        let req = RequestBuilder::kill("Counter@pid");
        assert_eq!(req["op"], "kill");
        assert_eq!(req["actor"], "Counter@pid");
    }

    #[test]
    fn interrupt_request_has_correct_op() {
        let req = RequestBuilder::interrupt();
        assert_eq!(req["op"], "interrupt");
        assert!(req["session"].is_null());
    }

    #[test]
    fn interrupt_with_session() {
        let req = RequestBuilder::interrupt_with_session("sess-42");
        assert_eq!(req["op"], "interrupt");
        assert_eq!(req["session"], "sess-42");
    }

    #[test]
    fn unload_request_has_correct_shape() {
        let req = RequestBuilder::unload("bt@counter");
        assert_eq!(req["op"], "unload");
        assert_eq!(req["module"], "bt@counter");
    }

    #[test]
    fn test_class_request_has_correct_shape() {
        let req = RequestBuilder::test_class("CounterTest");
        assert_eq!(req["op"], "test");
        assert_eq!(req["class"], "CounterTest");
    }

    #[test]
    fn test_file_request_has_correct_shape() {
        let req = RequestBuilder::test_file("stdlib/test/counter_test.bt");
        assert_eq!(req["op"], "test");
        assert_eq!(req["file"], "stdlib/test/counter_test.bt");
    }

    #[test]
    fn test_method_request_has_correct_shape() {
        let req = RequestBuilder::test_method("CounterTest", "testIncrement");
        assert_eq!(req["op"], "test");
        assert_eq!(req["class"], "CounterTest");
        assert_eq!(req["method"], "testIncrement");
    }

    #[test]
    fn test_all_request_has_correct_op() {
        let req = RequestBuilder::test_all();
        assert_eq!(req["op"], "test-all");
    }

    #[test]
    fn erlang_help_without_function() {
        let req = RequestBuilder::erlang_help("ets", None);
        assert_eq!(req["op"], "erlang-help");
        assert_eq!(req["module"], "ets");
        assert!(req["function"].is_null());
    }

    #[test]
    fn erlang_help_with_function() {
        let req = RequestBuilder::erlang_help("ets", Some("insert"));
        assert_eq!(req["op"], "erlang-help");
        assert_eq!(req["module"], "ets");
        assert_eq!(req["function"], "insert");
    }

    #[test]
    fn describe_request_has_correct_op() {
        let req = RequestBuilder::describe();
        assert_eq!(req["op"], "describe");
    }

    #[test]
    fn health_request_has_correct_op() {
        let req = RequestBuilder::health();
        assert_eq!(req["op"], "health");
    }

    #[test]
    fn shutdown_request_has_correct_shape() {
        let req = RequestBuilder::shutdown("secret-cookie");
        assert_eq!(req["op"], "shutdown");
        assert_eq!(req["cookie"], "secret-cookie");
    }

    #[test]
    fn list_classes_without_filter() {
        let req = RequestBuilder::list_classes(None);
        assert_eq!(req["op"], "list-classes");
        assert!(req["filter"].is_null());
    }

    #[test]
    fn list_classes_with_filter() {
        let req = RequestBuilder::list_classes(Some("Counter"));
        assert_eq!(req["op"], "list-classes");
        assert_eq!(req["filter"], "Counter");
    }

    #[test]
    fn enable_tracing_request_has_correct_op() {
        let req = RequestBuilder::enable_tracing();
        assert_eq!(req["op"], "enable-tracing");
    }

    #[test]
    fn disable_tracing_request_has_correct_op() {
        let req = RequestBuilder::disable_tracing();
        assert_eq!(req["op"], "disable-tracing");
    }

    #[test]
    fn get_traces_all_none_has_only_op() {
        let req = RequestBuilder::get_traces(None, None, None, None, None, None);
        assert_eq!(req["op"], "get-traces");
        assert!(req["actor"].is_null());
        assert!(req["selector"].is_null());
        assert!(req["class"].is_null());
        assert!(req["outcome"].is_null());
        assert!(req["min_duration_ns"].is_null());
        assert!(req["limit"].is_null());
    }

    #[test]
    fn get_traces_all_some_includes_all_fields() {
        let req = RequestBuilder::get_traces(
            Some("Counter@pid"),
            Some("increment"),
            Some("Counter"),
            Some("ok"),
            Some(1_000_000),
            Some(50),
        );
        assert_eq!(req["op"], "get-traces");
        assert_eq!(req["actor"], "Counter@pid");
        assert_eq!(req["selector"], "increment");
        assert_eq!(req["class"], "Counter");
        assert_eq!(req["outcome"], "ok");
        assert_eq!(req["min_duration_ns"], 1_000_000u64);
        assert_eq!(req["limit"], 50u32);
    }

    #[test]
    fn actor_stats_without_actor() {
        let req = RequestBuilder::actor_stats(None);
        assert_eq!(req["op"], "actor-stats");
        assert!(req["actor"].is_null());
    }

    #[test]
    fn actor_stats_with_actor() {
        let req = RequestBuilder::actor_stats(Some("Counter@pid"));
        assert_eq!(req["op"], "actor-stats");
        assert_eq!(req["actor"], "Counter@pid");
    }

    #[test]
    fn export_traces_all_none_has_only_op() {
        let req = RequestBuilder::export_traces(None, None, None, None, None, None, None);
        assert_eq!(req["op"], "export-traces");
        assert!(req["path"].is_null());
        assert!(req["actor"].is_null());
    }

    #[test]
    fn export_traces_with_path_and_actor() {
        let req = RequestBuilder::export_traces(
            Some("/var/traces.json"),
            Some("Counter@pid"),
            None,
            None,
            None,
            None,
            None,
        );
        assert_eq!(req["op"], "export-traces");
        assert_eq!(req["path"], "/var/traces.json");
        assert_eq!(req["actor"], "Counter@pid");
        assert!(req["selector"].is_null());
    }

    #[test]
    fn stdin_request_preserves_id() {
        let req = RequestBuilder::stdin("msg-007", "hello\n");
        assert_eq!(req["op"], "stdin");
        assert_eq!(req["id"], "msg-007");
        assert_eq!(req["value"], "hello\n");
    }
}
