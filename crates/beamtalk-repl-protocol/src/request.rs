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

/// The `ChangeEntry` kinds a `flush` can be scoped to (its `kind` param).
/// `beamtalk_workspace_flush:kind_filters/0` is the one definition; a test in
/// this crate reads that function and fails when this list differs.
pub const FLUSH_KINDS: [&str; 5] = [
    "new-class",
    "remove-method",
    "remove-class",
    "rename-class",
    "rename-method",
];

/// What a `flush` request is scoped to (BT-5204). The operation takes at most
/// one of `class`, `file` and `kind`; this enum makes sending two impossible.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlushScope<'a> {
    /// Every pending durable change.
    All,
    /// One class's changes (`remove-class` entries match by class name).
    Class(&'a str),
    /// One source file's changes.
    File(&'a str),
    /// One `ChangeEntry` kind, such as `new-class`.
    Kind(&'a str),
}

/// Whether a `ChangeLog`-recording request comes from a person or an agent
/// (ADR 0082 `author_kind`). It is audit metadata, not a filter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthorKind {
    /// A person at a REPL, editor or browser. What the workspace records when a
    /// request names no author.
    Human,
    /// An agent, such as an MCP tool call.
    Agent,
}

impl AuthorKind {
    /// The wire value of the `author_kind` param.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Human => "human",
            Self::Agent => "agent",
        }
    }
}

/// Who a `save-method` or `save-class` request is recorded as in the
/// `ChangeLog` (ADR 0082): a name, such as the MCP tool, and a kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Author<'a> {
    /// The audit author, for example `mcp:save_method`.
    pub name: &'a str,
    /// Human or agent.
    pub kind: AuthorKind,
}

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

    /// Build a `run-entry` request — dispatch a class entry method with argv
    /// in a connected session (ADR 0099 §3).
    ///
    /// `selector` keeps its trailing `:` for the arity-1 keyword (`main:`) form;
    /// `args` is the program arguments delivered to the entry as a `List(String)`.
    /// The argv crosses as a structured JSON array — never spliced into source —
    /// so no string escaping is needed.
    #[must_use]
    pub fn run_entry(class: &str, selector: &str, args: &[String]) -> serde_json::Value {
        serde_json::json!({
            "op": "run-entry",
            "id": next_msg_id(),
            "class": class,
            "selector": selector,
            "args": args,
        })
    }

    /// Build an `eval` request with optional trace mode.
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

    /// Build a `show-codegen` request for an expression.
    #[must_use]
    pub fn show_codegen(code: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "show-codegen",
            "id": next_msg_id(),
            "code": code
        })
    }

    /// Build a `show-codegen` request for a loaded class.
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

    /// Attach the loaded project's package name to a `load-project` request.
    ///
    /// The Rust client already parses `beamtalk.toml`; passing the name lets
    /// the workspace resolve the per-project package without parsing TOML in
    /// Erlang (BT-3661). A `None` leaves the request unchanged.
    #[must_use]
    pub fn with_package_name(
        mut request: serde_json::Value,
        package_name: Option<&str>,
    ) -> serde_json::Value {
        if let Some(name) = package_name {
            request["package_name"] = serde_json::Value::String(name.to_string());
        }
        request
    }

    // --- Session operations ---
    //
    // ADR 0081 Phase 6: the `clear` and `bindings` ops were removed.
    // Session state is read and reset via the `Session` API
    // (`Session current bindings keys`, `Session current clear`) over `eval`.

    /// Build a `sessions` request. No Rust client sends it (VS Code and the
    /// LiveView facade do); the REPL-protocol e2e harness uses it to prove a
    /// release-mode listener answers a non-compiling op
    /// (`crates/beamtalk-cli/tests/repl_protocol.rs`).
    #[must_use]
    pub fn sessions() -> serde_json::Value {
        Self::no_param("sessions")
    }

    /// Build a `close` request: ends the sender's own session (the CLI REPL's
    /// `:exit` / Ctrl-D, BT-5199).
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

    /// Build a `kill` request. Clients send `kill` from VS Code and LiveView;
    /// in Rust the REPL-protocol e2e harness uses this to drive the real op.
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

    // --- Code editing operations (BT-5203) ---

    /// Build a `save-method` request: durably install `source` (the body to the
    /// right of `=>`) as `selector` on `class`.
    ///
    /// The three values cross as structured params, never spliced into a
    /// Beamtalk expression, so the body needs no escaping.
    #[must_use]
    pub fn save_method(class: &str, selector: &str, source: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "save-method",
            "id": next_msg_id(),
            "class": class,
            "selector": selector,
            "source": source
        })
    }

    /// [`save_method`](Self::save_method), recorded in the `ChangeLog` as
    /// `author` rather than as the connection's own (human) author.
    #[must_use]
    pub fn save_method_by(
        class: &str,
        selector: &str,
        source: &str,
        author: Author<'_>,
    ) -> serde_json::Value {
        Self::with_author(Self::save_method(class, selector, source), author)
    }

    /// [`save_class`](Self::save_class), recorded in the `ChangeLog` as
    /// `author`.
    #[must_use]
    pub fn save_class_by(source: &str, path: &str, author: Author<'_>) -> serde_json::Value {
        Self::with_author(Self::save_class(source, path), author)
    }

    fn with_author(mut req: serde_json::Value, author: Author<'_>) -> serde_json::Value {
        req["author"] = author.name.into();
        req["author_kind"] = author.kind.as_str().into();
        req
    }

    /// Build a `save-class` request: create a class from `source` at `path`.
    #[must_use]
    pub fn save_class(source: &str, path: &str) -> serde_json::Value {
        serde_json::json!({
            "op": "save-class",
            "id": next_msg_id(),
            "source": source,
            "path": path
        })
    }

    // --- Workspace state operations (BT-5204) ---

    /// Build a `flush` request: write pending `ChangeLog` entries to disk.
    ///
    /// `scope` narrows it to one class, source file or `ChangeEntry` kind (at
    /// most one, by construction). `confirm_destructive` is sent only when
    /// `true`: it is the explicit confirmation that also applies Tier 2
    /// (`remove-class`, `rename-class`, `rename-method`) entries, and the
    /// operation reads anything but a boolean `true` as "withhold them", so
    /// it is never implied by omission.
    #[must_use]
    pub fn flush(scope: FlushScope<'_>, confirm_destructive: bool) -> serde_json::Value {
        let mut req = Self::no_param("flush");
        match scope {
            FlushScope::All => {}
            FlushScope::Class(class) => req["class"] = class.into(),
            FlushScope::File(file) => req["file"] = file.into(),
            FlushScope::Kind(kind) => req["kind"] = kind.into(),
        }
        if confirm_destructive {
            req["confirm_destructive"] = true.into();
        }
        req
    }

    /// Build a `changes` request: every `ChangeLog` entry, with the flags a
    /// client uses to pick the pending view.
    #[must_use]
    pub fn changes() -> serde_json::Value {
        Self::no_param("changes")
    }

    /// Build an `info` request: version, node, workspace id and the settings
    /// a client needs.
    #[must_use]
    pub fn info() -> serde_json::Value {
        Self::no_param("info")
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

    /// Build a `test-all` request.
    #[must_use]
    pub fn test_all() -> serde_json::Value {
        Self::no_param("test-all")
    }

    // --- Documentation operations ---

    /// Build an `erlang-help` request.
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

    /// Build a `list-classes` request.
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

    /// Build a `nav-query` request — structured navigation query that
    /// returns `beamtalk_xref` site records to the LSP / MCP.
    ///
    /// `kind` is one of `"senders"`, `"implementors"`, `"references"`.
    /// For `senders` / `implementors`, pass a Beamtalk selector as the
    /// second arg; for `references`, pass a Beamtalk class name.
    #[must_use]
    pub fn nav_query(kind: &str, arg: &str) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "nav-query",
            "id": next_msg_id(),
            "kind": kind,
        });
        match kind {
            "senders" | "implementors" => {
                req["selector"] = serde_json::Value::String(arg.to_owned());
            }
            _ => {
                // Default "references" branch — class name argument.
                req["class"] = serde_json::Value::String(arg.to_owned());
            }
        }
        req
    }

    /// Build a `nav-symbols` request — bulk class+method symbol enumeration
    /// from the live class registry.
    ///
    /// Unlike `nav-query` (which is locked to selector-shaped navigation:
    /// senders / implementors / references), `nav-symbols` returns the full
    /// outline that backs the LSP `textDocument/documentSymbol` and
    /// `workspace/symbol` capabilities: every loaded class with its
    /// instance- and class-side method headers, including REPL-loaded and
    /// live-edited classes the AST walker can't see.
    ///
    /// `scope` filters the result set:
    /// * `Some("user")` — only classes with a backing `.bt` file (mirrors
    ///   `list-classes` `user` filter and matches the narrower
    ///   `textDocument/documentSymbol` usage).
    /// * `Some("all")` / `None` — every loaded class, including stdlib /
    ///   dynamic / REPL-loaded classes (the `workspace/symbol` behavior,
    ///   including live classes with no source file).
    #[must_use]
    pub fn nav_symbols(scope: Option<&str>) -> serde_json::Value {
        let mut req = serde_json::json!({
            "op": "nav-symbols",
            "id": next_msg_id(),
        });
        if let Some(s) = scope {
            req["scope"] = serde_json::Value::String(s.to_owned());
        }
        req
    }

    /// Build a `reload-findings` request (ADR 0105 surface-parity
    /// gap): a snapshot read of every currently-live reload-induced finding
    /// (`beamtalk_workspace_findings_store:all/0`), the request/response
    /// counterpart to the `reload_check`/`completed` push frame — for
    /// surfaces (e.g. the LSP on attach) that need the findings that already
    /// existed before they connected, not just new ones pushed afterward.
    #[must_use]
    pub fn reload_findings() -> serde_json::Value {
        Self::no_param("reload-findings")
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

    // --- Stdin ---

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
    fn run_entry_request_has_correct_shape() {
        let args = vec!["Alice".to_string(), "Bob".to_string()];
        let req = RequestBuilder::run_entry("Greeter", "main:", &args);
        assert_eq!(req["op"], "run-entry");
        assert_eq!(req["class"], "Greeter");
        assert_eq!(req["selector"], "main:");
        // argv crosses as a structured JSON array, not a source string.
        assert_eq!(req["args"][0], "Alice");
        assert_eq!(req["args"][1], "Bob");
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
    }

    #[test]
    fn run_entry_unary_has_empty_args() {
        let req = RequestBuilder::run_entry("Server", "run", &[]);
        assert_eq!(req["op"], "run-entry");
        assert_eq!(req["selector"], "run");
        assert_eq!(req["args"].as_array().unwrap().len(), 0);
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
        assert!(req.get("trace").is_none());
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
        assert!(req.get("session").is_none());
    }

    #[test]
    fn erlang_complete_without_module() {
        let req = RequestBuilder::erlang_complete("ets:", None);
        assert_eq!(req["op"], "erlang-complete");
        assert_eq!(req["prefix"], "ets:");
        assert!(req.get("module").is_none());
    }

    #[test]
    fn erlang_complete_with_module() {
        let req = RequestBuilder::erlang_complete("insert", Some("ets"));
        assert_eq!(req["op"], "erlang-complete");
        assert_eq!(req["prefix"], "insert");
        assert_eq!(req["module"], "ets");
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
        assert!(req.get("selector").is_none());
    }

    #[test]
    fn show_codegen_class_with_selector() {
        let req = RequestBuilder::show_codegen_class("Counter", Some("increment"));
        assert_eq!(req["op"], "show-codegen");
        assert_eq!(req["class"], "Counter");
        assert_eq!(req["selector"], "increment");
    }

    #[test]
    fn load_project_request_has_correct_shape() {
        let req = RequestBuilder::load_project("/my/project", false);
        assert_eq!(req["op"], "load-project");
        assert_eq!(req["path"], "/my/project");
        assert_eq!(req["include_tests"], false);
        assert!(req.get("force").is_none());
    }

    #[test]
    fn with_package_name_attaches_name() {
        let req = RequestBuilder::with_package_name(
            RequestBuilder::load_project("/my/project", false),
            Some("my_pkg"),
        );
        assert_eq!(req["op"], "load-project");
        assert_eq!(req["path"], "/my/project");
        assert_eq!(req["package_name"], "my_pkg");
    }

    #[test]
    fn with_package_name_none_leaves_request_unchanged() {
        let req = RequestBuilder::with_package_name(
            RequestBuilder::load_project("/my/project", false),
            None,
        );
        assert!(req.get("package_name").is_none());
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
    fn sessions_request_has_correct_op() {
        let req = RequestBuilder::sessions();
        assert_eq!(req["op"], "sessions");
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
        assert!(req.get("session").is_none());
    }

    #[test]
    fn interrupt_with_session() {
        let req = RequestBuilder::interrupt_with_session("sess-42");
        assert_eq!(req["op"], "interrupt");
        assert_eq!(req["session"], "sess-42");
    }

    #[test]
    fn save_method_sends_values_not_source() {
        let body = "\"he said {x}\" \\ ^1";
        let req = RequestBuilder::save_method("Counter", "at:put:", body);
        assert_eq!(req["op"], "save-method");
        assert_eq!(req["class"], "Counter");
        assert_eq!(req["selector"], "at:put:");
        // The body is the value, byte for byte: nothing is escaped.
        assert_eq!(req["source"], body);
    }

    /// `FLUSH_KINDS` is a copy of the Erlang list, so read the Erlang source and
    /// compare, and check the protocol doc names each kind.
    #[test]
    fn flush_kinds_match_the_runtime_and_the_protocol_doc() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let erl = std::fs::read_to_string(
            root.join("runtime/apps/beamtalk_workspace/src/beamtalk_workspace_flush.erl"),
        )
        .expect("read beamtalk_workspace_flush.erl");
        let body = erl
            .split("\nkind_filters() ->")
            .nth(1)
            .and_then(|rest| rest.split("\n\n").next())
            .expect("kind_filters/0 definition");
        let runtime: Vec<&str> = body.split('\'').skip(1).step_by(2).collect();
        assert_eq!(
            runtime, FLUSH_KINDS,
            "kind_filters/0 and FLUSH_KINDS differ"
        );
        let doc = std::fs::read_to_string(root.join("docs/repl-protocol.md"))
            .expect("read repl-protocol.md");
        // Every other place that spells the kinds out: the op table note, and the
        // MCP tool and param descriptions (attribute literals, so read the source).
        let others = [
            "runtime/apps/beamtalk_runtime/src/beamtalk_repl_op_table.erl",
            "crates/beamtalk-mcp/src/server/tools/flush.rs",
            "crates/beamtalk-mcp/src/server/params.rs",
        ];
        for kind in FLUSH_KINDS {
            assert!(
                doc.contains(&format!("`{kind}`")),
                "docs/repl-protocol.md does not list the flush kind `{kind}`"
            );
            for file in others {
                let text = std::fs::read_to_string(root.join(file)).expect(file);
                assert!(
                    text.contains(kind),
                    "{file} does not name the flush kind `{kind}`"
                );
            }
        }
    }

    #[test]
    fn authored_saves_send_the_author_and_its_kind() {
        let author = Author {
            name: "mcp:save_method",
            kind: AuthorKind::Agent,
        };
        let req = RequestBuilder::save_method_by("Counter", "inc", "1", author);
        assert_eq!(req["op"], "save-method");
        assert_eq!(req["author"], "mcp:save_method");
        assert_eq!(req["author_kind"], "agent");
        let req = RequestBuilder::save_class_by("Object subclass: G", "src/g.bt", author);
        assert_eq!(req["op"], "save-class");
        assert_eq!(req["author_kind"], "agent");
        // The plain builders name no author: the workspace records its default.
        let plain = RequestBuilder::save_class("Object subclass: G", "src/g.bt");
        assert!(plain.get("author").is_none() && plain.get("author_kind").is_none());
    }

    #[test]
    fn save_class_sends_source_and_path() {
        let req = RequestBuilder::save_class("Object subclass: Greeter", "src/greeter.bt");
        assert_eq!(req["op"], "save-class");
        assert_eq!(req["source"], "Object subclass: Greeter");
        assert_eq!(req["path"], "src/greeter.bt");
    }

    #[test]
    fn flush_unscoped_sends_no_scope_and_no_confirmation() {
        let req = RequestBuilder::flush(FlushScope::All, false);
        assert_eq!(req["op"], "flush");
        for key in ["class", "file", "kind", "confirm_destructive"] {
            assert!(req.get(key).is_none(), "{key} must be absent: {req}");
        }
    }

    #[test]
    fn flush_sends_exactly_one_scope() {
        let req = RequestBuilder::flush(FlushScope::Class("Counter"), false);
        assert_eq!(req["class"], "Counter");
        assert!(req.get("file").is_none() && req.get("kind").is_none());

        let req = RequestBuilder::flush(FlushScope::File("src/counter.bt"), false);
        assert_eq!(req["file"], "src/counter.bt");
        assert!(req.get("class").is_none() && req.get("kind").is_none());

        let req = RequestBuilder::flush(FlushScope::Kind("new-class"), false);
        assert_eq!(req["kind"], "new-class");
        assert!(req.get("class").is_none() && req.get("file").is_none());
    }

    #[test]
    fn flush_confirmation_is_a_json_boolean_sent_only_when_true() {
        let req = RequestBuilder::flush(FlushScope::Class("Counter"), true);
        assert_eq!(req["confirm_destructive"], serde_json::Value::Bool(true));
        let req = RequestBuilder::flush(FlushScope::All, true);
        assert_eq!(req["confirm_destructive"], serde_json::Value::Bool(true));
    }

    #[test]
    fn changes_and_info_requests_carry_no_params() {
        assert_eq!(RequestBuilder::changes()["op"], "changes");
        assert_eq!(RequestBuilder::info()["op"], "info");
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
    fn test_all_request_has_correct_op() {
        let req = RequestBuilder::test_all();
        assert_eq!(req["op"], "test-all");
    }

    #[test]
    fn erlang_help_without_function() {
        let req = RequestBuilder::erlang_help("ets", None);
        assert_eq!(req["op"], "erlang-help");
        assert_eq!(req["module"], "ets");
        assert!(req.get("function").is_none());
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
        assert!(req.get("filter").is_none());
    }

    #[test]
    fn list_classes_with_filter() {
        let req = RequestBuilder::list_classes(Some("Counter"));
        assert_eq!(req["op"], "list-classes");
        assert_eq!(req["filter"], "Counter");
    }

    #[test]
    fn nav_query_senders_routes_to_selector_field() {
        let req = RequestBuilder::nav_query("senders", "increment");
        assert_eq!(req["op"], "nav-query");
        assert_eq!(req["kind"], "senders");
        assert_eq!(req["selector"], "increment");
        assert!(req.get("class").is_none());
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
    }

    #[test]
    fn nav_query_implementors_routes_to_selector_field() {
        let req = RequestBuilder::nav_query("implementors", "asString");
        assert_eq!(req["op"], "nav-query");
        assert_eq!(req["kind"], "implementors");
        assert_eq!(req["selector"], "asString");
        assert!(req.get("class").is_none());
    }

    #[test]
    fn nav_query_references_routes_to_class_field() {
        let req = RequestBuilder::nav_query("references", "Counter");
        assert_eq!(req["op"], "nav-query");
        assert_eq!(req["kind"], "references");
        assert_eq!(req["class"], "Counter");
        assert!(req.get("selector").is_none());
    }

    #[test]
    fn nav_symbols_without_scope_omits_field() {
        let req = RequestBuilder::nav_symbols(None);
        assert_eq!(req["op"], "nav-symbols");
        assert!(req.get("scope").is_none());
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
    }

    #[test]
    fn nav_symbols_with_scope_user_sets_field() {
        let req = RequestBuilder::nav_symbols(Some("user"));
        assert_eq!(req["op"], "nav-symbols");
        assert_eq!(req["scope"], "user");
    }

    #[test]
    fn nav_symbols_with_scope_all_sets_field() {
        let req = RequestBuilder::nav_symbols(Some("all"));
        assert_eq!(req["op"], "nav-symbols");
        assert_eq!(req["scope"], "all");
    }

    #[test]
    fn reload_findings_request_has_correct_shape() {
        let req = RequestBuilder::reload_findings();
        assert_eq!(req["op"], "reload-findings");
        assert!(req["id"].as_str().unwrap().starts_with("msg-"));
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
        assert!(req.get("actor").is_none());
        assert!(req.get("selector").is_none());
        assert!(req.get("class").is_none());
        assert!(req.get("outcome").is_none());
        assert!(req.get("min_duration_ns").is_none());
        assert!(req.get("limit").is_none());
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
        assert!(req.get("actor").is_none());
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
        assert!(req.get("path").is_none());
        assert!(req.get("actor").is_none());
        assert!(req.get("selector").is_none());
        assert!(req.get("class").is_none());
        assert!(req.get("outcome").is_none());
        assert!(req.get("min_duration_ns").is_none());
        assert!(req.get("limit").is_none());
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
        assert!(req.get("selector").is_none());
    }

    #[test]
    fn export_traces_all_some_includes_all_fields() {
        let req = RequestBuilder::export_traces(
            Some("/var/traces.json"),
            Some("Counter@pid"),
            Some("increment"),
            Some("Counter"),
            Some("ok"),
            Some(1_000_000),
            Some(50),
        );
        assert_eq!(req["op"], "export-traces");
        assert_eq!(req["path"], "/var/traces.json");
        assert_eq!(req["actor"], "Counter@pid");
        assert_eq!(req["selector"], "increment");
        assert_eq!(req["class"], "Counter");
        assert_eq!(req["outcome"], "ok");
        assert_eq!(req["min_duration_ns"], 1_000_000u64);
        assert_eq!(req["limit"], 50u32);
    }

    #[test]
    fn stdin_request_preserves_id() {
        let req = RequestBuilder::stdin("msg-007", "hello\n");
        assert_eq!(req["op"], "stdin");
        assert_eq!(req["id"], "msg-007");
        assert_eq!(req["value"], "hello\n");
    }

    // --- Op table conformance (BT-4673) ---

    /// One request from every `RequestBuilder` constructor, with every
    /// optional argument supplied, so each param name the client can send is
    /// exercised. Add a new constructor here; the test below fails if its op or
    /// a param name is not in the server's op table.
    fn every_builder_request() -> Vec<serde_json::Value> {
        let args = vec!["a".to_string()];
        vec![
            RequestBuilder::eval("1"),
            RequestBuilder::eval_with_trace("1", true),
            RequestBuilder::run_entry("C", "main:", &args),
            RequestBuilder::complete("x"),
            RequestBuilder::complete_with_cursor("x", 1),
            RequestBuilder::complete_with_session("x", 1, Some("s")),
            RequestBuilder::erlang_complete("p", Some("m")),
            RequestBuilder::show_codegen("1"),
            RequestBuilder::show_codegen_class("C", Some("s")),
            RequestBuilder::load_project("p", true),
            RequestBuilder::with_package_name(
                RequestBuilder::load_project_with_force("p", true, true),
                Some("pkg"),
            ),
            RequestBuilder::sessions(),
            RequestBuilder::close(),
            RequestBuilder::actors(),
            RequestBuilder::inspect("a"),
            RequestBuilder::kill("a"),
            RequestBuilder::interrupt(),
            RequestBuilder::interrupt_with_session("s"),
            RequestBuilder::unload("m"),
            RequestBuilder::save_method("C", "s", "1"),
            RequestBuilder::save_class("Object subclass: C", "src/c.bt"),
            RequestBuilder::save_method_by(
                "C",
                "s",
                "1",
                Author {
                    name: "a",
                    kind: AuthorKind::Agent,
                },
            ),
            RequestBuilder::save_class_by(
                "Object subclass: C",
                "src/c.bt",
                Author {
                    name: "a",
                    kind: AuthorKind::Agent,
                },
            ),
            RequestBuilder::flush(FlushScope::Class("C"), true),
            RequestBuilder::changes(),
            RequestBuilder::info(),
            RequestBuilder::test_class("C"),
            RequestBuilder::test_file("f"),
            RequestBuilder::test_all(),
            RequestBuilder::erlang_help("m", Some("f")),
            RequestBuilder::describe(),
            RequestBuilder::health(),
            RequestBuilder::shutdown("c"),
            RequestBuilder::list_classes(Some("user")),
            RequestBuilder::nav_query("senders", "foo"),
            RequestBuilder::nav_query("references", "Foo"),
            RequestBuilder::nav_symbols(Some("user")),
            RequestBuilder::reload_findings(),
            RequestBuilder::enable_tracing(),
            RequestBuilder::disable_tracing(),
            RequestBuilder::get_traces(
                Some("a"),
                Some("s"),
                Some("C"),
                Some("ok"),
                Some(1),
                Some(2),
            ),
            RequestBuilder::actor_stats(Some("a")),
            RequestBuilder::export_traces(
                Some("p"),
                Some("a"),
                Some("s"),
                Some("C"),
                Some("ok"),
                Some(1),
                Some(2),
            ),
            RequestBuilder::stdin("msg-1", "x"),
        ]
    }

    /// Every op the builders emit, and every param they send, must exist in the
    /// server's op table, and every required param of the op must be sent. The
    /// table is the Erlang `beamtalk_repl_op_table`, consumed here through the shared fixture it is pinned to by
    /// `beamtalk_repl_op_table_tests:table_matches_shared_fixture_test/0` —
    /// no node boot needed. This is what stops a client builder (like the old
    /// `info`) from outliving the server op it targets.
    #[test]
    fn every_builder_op_and_param_exists_in_the_server_op_table() {
        let path = beamtalk_workspace::cargo_workspace_root!()
            .join("runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json");
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read op table fixture {}: {e}", path.display()));
        let table: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("op table fixture is a JSON array");
        let names = |entry: &serde_json::Value, key: &str| -> Vec<String> {
            entry[key]
                .as_array()
                .unwrap_or_else(|| panic!("`{key}` is an array in {entry}"))
                .iter()
                .map(|v| v.as_str().expect("param names are strings").to_owned())
                .collect()
        };
        // Envelope fields carried by every protocol message, not op params.
        let envelope = ["op", "id", "session"];
        let mut problems = Vec::new();
        for req in every_builder_request() {
            let op = req["op"].as_str().expect("request has an op");
            let Some(entry) = table.iter().find(|e| e["op"] == op) else {
                problems.push(format!("op `{op}` is not in the server op table"));
                continue;
            };
            let required = names(entry, "params");
            let known: Vec<String> = required
                .iter()
                .cloned()
                .chain(names(entry, "optional"))
                .collect();
            let sent = req.as_object().expect("request is an object");
            // Every builder must send the op's required params, even in its
            // sparsest form; a request missing one is rejected by the server.
            for key in &required {
                if !sent.contains_key(key) {
                    problems.push(format!(
                        "op `{op}` builder request omits required param `{key}`"
                    ));
                }
            }
            for key in sent.keys() {
                if !envelope.contains(&key.as_str()) && !known.contains(key) {
                    problems.push(format!(
                        "op `{op}` sends param `{key}` the server does not read"
                    ));
                }
            }
        }
        assert!(problems.is_empty(), "{}", problems.join("\n"));
    }

    /// Keeps `every_builder_request()` honest: it must exercise every public
    /// constructor, so a new builder cannot skip the op-table check. Builders
    /// that only decorate another request are not counted.
    #[test]
    fn conformance_list_covers_every_public_builder() {
        use std::collections::BTreeSet;

        let src = include_str!("request.rs");
        let impl_start = src.find("impl RequestBuilder {").expect("impl block");
        let impl_end = src.find("#[cfg(test)]").expect("test module");
        // Builders that only decorate another request are not constructors.
        let decorators = ["with_package_name"];
        let constructors: BTreeSet<&str> = src[impl_start..impl_end]
            .lines()
            .filter_map(|l| l.trim_start().strip_prefix("pub fn "))
            .filter_map(|rest| rest.split(['(', '<']).next())
            .filter(|name| !decorators.contains(name))
            .collect();

        let list_start = src
            .find("fn every_builder_request()")
            .expect("every_builder_request");
        let list_end = list_start
            + src[list_start..]
                .find("\n    }\n")
                .expect("end of every_builder_request");
        let called: BTreeSet<&str> = src[list_start..list_end]
            .split("RequestBuilder::")
            .skip(1)
            .filter_map(|rest| rest.split('(').next())
            .collect();

        let missing: Vec<_> = constructors.difference(&called).collect();
        let unknown: Vec<_> = called.difference(&constructors).collect();
        assert!(
            missing.is_empty(),
            "add these RequestBuilder constructors to every_builder_request(): {missing:?}"
        );
        // `with_package_name` wraps a request in the list, so it may appear.
        let unknown: Vec<_> = unknown
            .into_iter()
            .filter(|n| !decorators.contains(*n))
            .collect();
        assert!(
            unknown.is_empty(),
            "every_builder_request() names builders that do not exist: {unknown:?}"
        );
    }

    #[test]
    fn removed_ops_have_no_builder() {
        // `test` ignores a `method` filter, so no builder sends one (BT-4673).
        // (An `info` builder used to be here for an op the server never had;
        // the op exists since BT-5204.)
        for req in every_builder_request() {
            assert!(req.get("method").is_none());
        }
    }
}
