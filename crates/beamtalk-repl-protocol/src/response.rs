// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Response types for the REPL JSON protocol.
//!
//! The canonical `ReplResponse` is a flat "super-struct" with `Option` fields
//! for every possible response payload. Only the fields relevant to a given
//! operation will be populated; the rest will be `None`. This matches the
//! wire format — a single JSON object whose shape varies by op.

use serde::{Deserialize, Deserializer, Serialize};

/// Deserialize a `null` JSON value as an empty `Vec`.
///
/// The REPL backend sometimes sends `"errors": null` instead of `"errors": []`.
/// This custom deserializer normalises both to an empty `Vec<T>`.
fn deserialize_null_as_empty_vec<'de, T, D>(
    deserializer: D,
) -> std::result::Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<Vec<T>>::deserialize(deserializer).map(Option::unwrap_or_default)
}

/// JSON response from the REPL backend.
///
/// Supports both the legacy format (`type` field) and the current protocol
/// format (`status` field). Fields are a union of every possible response
/// payload across all operations.
#[derive(Debug, Deserialize, Serialize)]
pub struct ReplResponse {
    // --- Protocol envelope ---
    /// Legacy response type (`result`, `error`, `bindings`, `loaded`, `actors`).
    #[serde(rename = "type", default, skip_serializing_if = "Option::is_none")]
    pub response_type: Option<String>,

    /// Message correlation ID (echoed from request).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,

    /// Session ID (echoed from request).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub session: Option<String>,

    /// Status flags: `["done"]`, `["done", "error"]`, `["done", "test-error"]`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub status: Option<Vec<String>>,

    /// POSIX exit status from a connected-session `Program exit:` (ADR 0099
    /// §3). Present only on the `done` reply that ended the session.
    /// This is the wire contract a connecting client will adopt as its own
    /// process exit code once connected-mode `beamtalk run` dispatch lands;
    /// no consumer reads it yet, so it does not affect any CLI exit code
    /// today. `None` for every other response.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<u8>,

    // --- Common payload ---
    /// Result value (eval, clear, kill, clone, etc.).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub value: Option<serde_json::Value>,

    /// Captured stdout from evaluation.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output: Option<String>,

    /// Legacy error message field.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,

    /// Error message (current protocol).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// The raised exception's complete stack, innermost first (error
    /// responses, ADR 0139 decision 4). Every frame carries both forms and a
    /// `hidden` flag; hidden frames are present, not removed.
    #[serde(
        default,
        deserialize_with = "deserialize_frames_leniently",
        skip_serializing_if = "Option::is_none"
    )]
    pub frames: Option<Vec<StackFrameInfo>>,

    // --- Session / bindings ---
    /// Variable bindings map (bindings op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bindings: Option<serde_json::Value>,

    // --- Load operations ---
    /// Loaded class names (eval/load-file/load-source/load-project).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub classes: Option<Vec<String>>,

    /// Class info list with metadata (list-classes op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub class_list: Option<Vec<ClassInfo>>,

    // --- Actor operations ---
    /// Actor list (actors op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub actors: Option<Vec<ActorInfo>>,

    // --- Module operations ---
    /// Module list (modules op — legacy protocol, retained for backward compat).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub modules: Option<Vec<ModuleInfo>>,

    // --- Session operations ---
    /// Session list (sessions op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sessions: Option<Vec<SessionInfo>>,

    // --- Completion ---
    /// Completion suggestions (complete op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub completions: Option<Vec<String>>,

    // --- Symbol info ---
    /// Symbol information (info op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub info: Option<serde_json::Value>,

    /// Actor state (inspect op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub state: Option<serde_json::Value>,

    // --- Diagnostics ---
    /// Compilation warnings — message strings only. Kept as plain strings so
    /// a client that predates [`Self::warning_details`] keeps working; read
    /// warnings through [`Self::warning_entries`] to get their hints too.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub warnings: Option<Vec<String>>,

    /// The same warnings as [`Self::warnings`], in the same order, each with
    /// its diagnostic hint as a separate field (BT-4449). The server sends
    /// this only when at least one warning has a hint.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub warning_details: Option<Vec<Warning>>,

    /// Line number (1-based) of a compile error in the submitted snippet.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,

    /// Hint text for a compile error, where available.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,

    // --- Documentation ---
    /// Documentation text (docs op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,

    // --- Codegen ---
    /// Generated Core Erlang source (show-codegen op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub core_erlang: Option<String>,

    // --- Reload ---
    /// Number of actors affected by reload.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub affected_actors: Option<u32>,

    /// Number of actors that failed code migration.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub migration_failures: Option<u32>,

    // --- Load-project ---
    /// Per-file load errors (load-project op). Each entry is a structured map
    /// with at least `path`, `kind`, and `message` fields.
    #[serde(
        default,
        deserialize_with = "deserialize_null_as_empty_vec",
        skip_serializing_if = "Vec::is_empty"
    )]
    pub errors: Vec<serde_json::Value>,

    /// Incremental load summary (load-project op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,

    // --- Test operations ---
    /// Test results (test / test-all ops).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub results: Option<serde_json::Value>,

    // --- Tracing (ADR 0069) ---
    /// Per-statement trace steps (eval with trace=true).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub steps: Option<Vec<serde_json::Value>>,

    // --- Server / discovery ---
    /// Supported operations and protocol info (describe op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ops: Option<serde_json::Value>,

    /// Protocol and language versions (describe op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub versions: Option<serde_json::Value>,
}

impl ReplResponse {
    /// Every warning in this response, with its hint when it has one:
    /// [`Self::warning_details`] when the server sent it, otherwise
    /// [`Self::warnings`] as hint-less entries.
    #[must_use]
    pub fn warning_entries(&self) -> Vec<Warning> {
        if let Some(details) = &self.warning_details {
            return details.clone();
        }
        self.warnings
            .iter()
            .flatten()
            .map(|message| Warning {
                message: message.clone(),
                hint: None,
            })
            .collect()
    }

    /// Check if this is an error response (either legacy or current format).
    pub fn is_error(&self) -> bool {
        if self.response_type.as_deref() == Some("error") {
            return true;
        }
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "error");
        }
        false
    }

    /// Check if the response contains a test failure (`"test-error"` status).
    pub fn has_test_error(&self) -> bool {
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "test-error");
        }
        false
    }

    /// Get the error message (either legacy `message` or current `error` field).
    ///
    /// Falls back to the first `errors[].message` entry when neither top-level
    /// field is set — this matches load-project responses which surface
    /// per-file failures only via the `errors` list.
    pub fn error_message(&self) -> Option<&str> {
        if let Some(ref msg) = self.message {
            return Some(msg.as_str());
        }
        if let Some(ref err) = self.error {
            return Some(err.as_str());
        }
        self.errors
            .iter()
            .find_map(|e| e.get("message").and_then(serde_json::Value::as_str))
    }

    /// Get the result value as a formatted string.
    ///
    /// For JSON string values, returns the string content without quotes.
    /// For other JSON values, returns the JSON serialization.
    /// For `None`, returns an empty string.
    pub fn value_string(&self) -> String {
        match &self.value {
            Some(serde_json::Value::String(s)) => s.clone(),
            Some(v) => v.to_string(),
            None => String::new(),
        }
    }
}

/// `frames` is diagnostic data: an off-shape frame must not stop the primary
/// error from decoding, so any decode failure yields `None`.
fn deserialize_frames_leniently<'de, D>(d: D) -> Result<Option<Vec<StackFrameInfo>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let value = Option::<serde_json::Value>::deserialize(d)?;
    Ok(value.and_then(|v| serde_json::from_value(v).ok()))
}

/// One structured stack frame (ADR 0139, decision 4): the display string and
/// the raw MFA side by side. The key set is pinned to
/// `tests/repl-protocol/frame_fields.json`, which the Erlang
/// `beamtalk_stack_frame:structured_frame/1` is checked against too.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct StackFrameInfo {
    /// The shared renderer's text, e.g. `Counter>>increment (counter.bt:12)`.
    pub display: String,
    /// Raw Erlang module, `None` when the frame has none.
    pub module: Option<String>,
    /// Raw Erlang function, `None` when the frame has none.
    pub function: Option<String>,
    /// Raw arity.
    pub arity: u32,
    /// Display path of the source file, when known.
    pub file: Option<String>,
    /// Source line, when known.
    pub line: Option<u32>,
    /// `true` when the frame is collapsed out of default traces.
    pub hidden: bool,
}

impl StackFrameInfo {
    /// The raw `module:function/arity`, when the frame has a module and function.
    #[must_use]
    pub fn raw_mfa(&self) -> Option<String> {
        Some(format!(
            "{}:{}/{}",
            self.module.as_deref()?,
            self.function.as_deref()?,
            self.arity
        ))
    }

    /// The raw-output line for this frame: the display name with the raw MFA
    /// beside it, `Counter>>increment (counter.bt:12)  [bt@counter:increment/1]`
    /// (the text the Erlang `beamtalk_stack_frame:format_frame_raw/1` renders;
    /// both are checked against `tests/repl-protocol/raw_frame_lines.json`).
    /// A frame with no raw MFA shows the display name alone.
    #[must_use]
    pub fn display_raw(&self) -> String {
        match self.raw_mfa() {
            Some(raw) => format!("{}  [{raw}]", self.display),
            None => self.display.clone(),
        }
    }
}

/// The `--raw-mfa` listing of an error reply's complete stack: one `  at ` line
/// per frame, hidden ones included. Empty when the reply carries no frames.
#[must_use]
pub fn render_raw_frames(frames: &[StackFrameInfo]) -> Vec<String> {
    frames
        .iter()
        .map(|f| format!("  at {}", f.display_raw()))
        .collect()
}

/// What `beamtalk run --connect` prints after an uncaught error's message, the
/// client-side twin of what the script-mode harness prints
/// (`beamtalk_script_harness:raw_trace/3`): with `raw_mfa`, the complete stack as
/// [`render_raw_frames`] lists it; without, the visible frames in display form
/// and, when some were left out, `(N frames hidden; --raw-mfa shows all)` (the
/// text of `beamtalk_stack_frame:hidden_note/1`). Empty when there are no frames.
#[must_use]
pub fn render_run_error_frames(frames: &[StackFrameInfo], raw_mfa: bool) -> Vec<String> {
    if raw_mfa {
        return render_raw_frames(frames);
    }
    let mut lines: Vec<String> = frames
        .iter()
        .filter(|f| !f.hidden)
        .map(|f| format!("  at {}", f.display))
        .collect();
    let hidden = frames.iter().filter(|f| f.hidden).count();
    if hidden > 0 {
        let noun = if hidden == 1 { "frame" } else { "frames" };
        lines.push(format!("  ({hidden} {noun} hidden; --raw-mfa shows all)"));
    }
    lines
}

/// What a REPL client prints after an error reply that carries frames
/// (`:raw-mfa`, ADR 0139 decision 4): with the session's raw option on, the
/// complete stack as [`render_raw_frames`] lists it; off, a one-line notice that
/// the frames were left out and how to see them. Empty when there are no
/// frames. The one definition, shared by `beamtalk repl` and the REPL-protocol
/// test client, so neither can drift from the other.
#[must_use]
pub fn render_repl_error_frames(frames: &[StackFrameInfo], raw_mfa: bool) -> Vec<String> {
    if raw_mfa {
        return render_raw_frames(frames);
    }
    // The default display lists no frames, so every one of them is left out.
    let n = frames.len();
    if n == 0 {
        return Vec::new();
    }
    let noun = if n == 1 { "frame" } else { "frames" };
    vec![format!("({n} {noun} hidden; :raw-mfa shows all)")]
}

/// One compilation warning with its optional hint (`warning_details` entry).
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct Warning {
    /// The warning message (also listed in the response's `warnings`).
    pub message: String,
    /// The diagnostic's hint, e.g. `missing: class defaultPort`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
}

/// Information about a running actor, deserialized from REPL JSON.
#[derive(Debug, Deserialize, Serialize)]
pub struct ActorInfo {
    /// Erlang process identifier string (e.g., `"<0.123.0>"`).
    pub pid: String,
    /// Beamtalk class name of the actor (e.g., `"Counter"`).
    pub class: String,
    /// Source module the actor was compiled from.
    pub module: String,
    /// Unix timestamp (seconds) when the actor was spawned.
    pub spawned_at: i64,
}

/// Class information from the list-classes op.
#[derive(Debug, Deserialize, Serialize)]
pub struct ClassInfo {
    /// Class name (e.g., `"String"`).
    pub name: String,
    /// Superclass name (e.g., `"Value"`) or `null` for root classes.
    pub superclass: Option<String>,
    /// One-line class description, or `null` if undocumented.
    pub doc: Option<String>,
    /// Whether the class is sealed (cannot be subclassed).
    pub sealed: bool,
    /// Whether the class is abstract (cannot be instantiated directly).
    #[serde(rename = "abstract")]
    pub is_abstract: bool,
}

/// Information about a loaded module, deserialized from REPL JSON.
#[derive(Debug, Deserialize, Serialize)]
pub struct ModuleInfo {
    /// Module name as registered in the BEAM node.
    pub name: String,
    /// Path to the source `.bt` file that defined this module.
    pub source_file: String,
    /// Number of actors currently running from this module.
    pub actor_count: u32,
    /// Unix timestamp when the module was loaded.
    pub load_time: i64,
    /// Human-readable relative time since load (e.g., `"2 minutes ago"`).
    pub time_ago: String,
}

/// Information about an active REPL session, deserialized from REPL JSON.
#[derive(Debug, Deserialize, Serialize)]
pub struct SessionInfo {
    /// Unique session identifier.
    pub id: String,
    /// Unix timestamp when the session was created.
    #[serde(default)]
    pub created_at: Option<i64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_success_response() {
        let json = r#"{"id":"msg-001","value":42,"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(!resp.is_error());
        assert_eq!(resp.value_string(), "42");
        assert_eq!(resp.id.as_deref(), Some("msg-001"));
    }

    #[test]
    fn error_response_decodes_frames_with_both_forms() {
        let json = r#"{"error":"boom","status":["done","error"],"frames":[
            {"display":"Counter>>increment (counter.bt:12)","module":"bt@counter",
             "function":"increment","arity":2,"file":"counter.bt","line":12,"hidden":false},
            {"display":"gen_server:try_handle_call/4","module":"gen_server",
             "function":"try_handle_call","arity":4,"file":null,"line":null,"hidden":true}]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        let frames = resp.frames.expect("frames");
        assert_eq!(frames.len(), 2);
        assert_eq!(frames[0].display, "Counter>>increment (counter.bt:12)");
        assert_eq!(
            frames[0].raw_mfa().as_deref(),
            Some("bt@counter:increment/2")
        );
        assert!(!frames[0].hidden);
        assert!(frames[1].hidden, "hidden frames are kept, flagged");
        assert_eq!(frames[1].line, None);
    }

    #[test]
    fn raw_rendering_puts_the_raw_mfa_beside_the_display_name() {
        let json = r#"{"error":"boom","status":["done","error"],"frames":[
            {"display":"Counter>>increment (counter.bt:12)","module":"bt@counter","function":"increment","arity":1,"file":"counter.bt","line":12,"hidden":false},
            {"display":"gen_server:loop/7","module":"gen_server","function":"loop","arity":7,"file":null,"line":null,"hidden":true},
            {"display":"?","module":null,"function":null,"arity":0,"file":null,"line":null,"hidden":true}
        ]}"#;
        let frames = serde_json::from_str::<ReplResponse>(json)
            .expect("decodes")
            .frames
            .expect("frames");
        assert_eq!(
            render_raw_frames(&frames),
            vec![
                "  at Counter>>increment (counter.bt:12)  [bt@counter:increment/1]",
                "  at gen_server:loop/7  [gen_server:loop/7]",
                "  at ?",
            ]
        );
    }

    /// BT-4835: `run --connect` prints the visible frames by default, naming the
    /// hidden ones with the same notice the script-mode harness prints.
    #[test]
    fn render_run_error_frames_lists_visible_frames_and_notice() {
        let json = r#"{"id":"x","status":["error"],"error":"boom","frames":[
            {"display":"Counter>>increment (counter.bt:12)","module":"bt@counter","function":"increment","arity":1,"file":"counter.bt","line":12,"hidden":false},
            {"display":"gen_server:loop/7","module":"gen_server","function":"loop","arity":7,"file":null,"line":null,"hidden":true},
            {"display":"proc_lib:init_p/3","module":"proc_lib","function":"init_p","arity":3,"file":null,"line":null,"hidden":true}
        ]}"#;
        let frames = serde_json::from_str::<ReplResponse>(json)
            .expect("decodes")
            .frames
            .expect("frames");
        assert_eq!(
            render_run_error_frames(&frames, false),
            vec![
                "  at Counter>>increment (counter.bt:12)",
                "  (2 frames hidden; --raw-mfa shows all)",
            ]
        );
        assert_eq!(
            render_run_error_frames(&frames[..2], false).last().unwrap(),
            "  (1 frame hidden; --raw-mfa shows all)"
        );
        assert_eq!(
            render_run_error_frames(&frames, true),
            render_raw_frames(&frames)
        );
        assert!(render_run_error_frames(&[], false).is_empty());
    }

    #[test]
    fn repl_error_frames_list_everything_raw_or_name_the_switch() {
        let frame = |display: &str, module: &str| StackFrameInfo {
            display: display.to_string(),
            module: Some(module.to_string()),
            function: Some("f".to_string()),
            arity: 1,
            file: None,
            line: None,
            hidden: false,
        };
        let frames = vec![
            frame("Counter>>f", "bt@counter"),
            frame("lists:f/1", "lists"),
        ];
        // Raw on: the same listing as `--raw-mfa`.
        assert_eq!(
            render_repl_error_frames(&frames, true),
            render_raw_frames(&frames)
        );
        // Raw off: one notice, pluralised, and nothing for an empty stack.
        assert_eq!(
            render_repl_error_frames(&frames, false),
            vec!["(2 frames hidden; :raw-mfa shows all)"]
        );
        assert_eq!(
            render_repl_error_frames(&frames[..1], false),
            vec!["(1 frame hidden; :raw-mfa shows all)"]
        );
        assert!(render_repl_error_frames(&[], false).is_empty());
        assert!(render_repl_error_frames(&[], true).is_empty());
    }

    #[test]
    fn off_shape_frames_do_not_lose_the_error() {
        let json = r#"{"error":"boom","status":["done","error"],"frames":[{"display":7}]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.error.as_deref(), Some("boom"));
        assert!(resp.frames.is_none());
    }

    #[test]
    fn error_response_without_frames_decodes() {
        let resp: ReplResponse =
            serde_json::from_str(r#"{"error":"x","status":["done","error"]}"#).unwrap();
        assert!(resp.frames.is_none());
    }

    /// `display_raw` and `beamtalk_stack_frame:format_frame_raw/1` render the
    /// same lines, through `tests/repl-protocol/raw_frame_lines.json`.
    #[test]
    fn display_raw_matches_shared_fixture() {
        let path = beamtalk_workspace::cargo_workspace_root!()
            .join("tests/repl-protocol/raw_frame_lines.json");
        let raw = std::fs::read_to_string(&path).expect("read raw_frame_lines.json");
        let fixture: serde_json::Value = serde_json::from_str(&raw).unwrap();
        let cases = fixture["cases"].as_array().unwrap();
        assert!(!cases.is_empty());
        for case in cases {
            let frame = StackFrameInfo {
                display: case["display"].as_str().unwrap().into(),
                module: Some(case["module"].as_str().unwrap().into()),
                function: Some(case["function"].as_str().unwrap().into()),
                arity: u32::try_from(case["arity"].as_u64().unwrap()).unwrap(),
                file: None,
                line: None,
                hidden: false,
            };
            assert_eq!(frame.display_raw(), case["raw_line"].as_str().unwrap());
        }
    }

    /// The Rust frame type and `beamtalk_stack_frame:structured_frame/1` agree
    /// on the key set through `tests/repl-protocol/frame_fields.json`.
    #[test]
    fn stack_frame_info_keys_match_shared_fixture() {
        let path = beamtalk_workspace::cargo_workspace_root!()
            .join("tests/repl-protocol/frame_fields.json");
        let raw = std::fs::read_to_string(&path).expect("read frame_fields.json");
        let fixture: serde_json::Value = serde_json::from_str(&raw).unwrap();
        let mut expected: Vec<String> = fixture["frame_fields"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_owned())
            .collect();
        expected.sort();
        let frame = StackFrameInfo {
            display: "x".into(),
            module: None,
            function: None,
            arity: 0,
            file: None,
            line: None,
            hidden: false,
        };
        let value = serde_json::to_value(&frame).unwrap();
        let mut actual: Vec<String> = value.as_object().unwrap().keys().cloned().collect();
        actual.sort();
        assert_eq!(actual, expected);
    }

    #[test]
    fn warning_entries_fall_back_to_plain_warnings() {
        let json = r#"{"value":1,"warnings":["a","b"],"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(
            resp.warning_entries(),
            vec![
                Warning {
                    message: "a".to_string(),
                    hint: None
                },
                Warning {
                    message: "b".to_string(),
                    hint: None
                },
            ]
        );
    }

    #[test]
    fn warning_entries_prefer_warning_details_with_hints() {
        // BT-4449: `warnings` stays message strings for old clients;
        // `warning_details` carries the hint.
        let json = r#"{"value":1,"warnings":["m","n"],
            "warning_details":[{"message":"m","hint":"h"},{"message":"n"}],
            "status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(
            resp.warnings.as_deref(),
            Some(&["m".to_string(), "n".to_string()][..])
        );
        assert_eq!(
            resp.warning_entries(),
            vec![
                Warning {
                    message: "m".to_string(),
                    hint: Some("h".to_string())
                },
                Warning {
                    message: "n".to_string(),
                    hint: None
                },
            ]
        );
    }

    #[test]
    fn deserialize_error_response() {
        let json =
            r#"{"id":"msg-001","error":"Undefined variable: foo","status":["done","error"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(resp.is_error());
        assert_eq!(resp.error_message(), Some("Undefined variable: foo"));
    }

    #[test]
    fn deserialize_legacy_error() {
        let json = r#"{"type":"error","message":"Something went wrong"}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(resp.is_error());
        assert_eq!(resp.error_message(), Some("Something went wrong"));
    }

    #[test]
    fn deserialize_script_exit_response() {
        // A connected-session `Program exit: 5` — a `done` reply (not an
        // error) carrying the POSIX exit status the CLI adopts.
        let json = r#"{"id":"msg-008","value":null,"status":["done"],"exit_code":5}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(!resp.is_error());
        assert_eq!(resp.exit_code, Some(5));
    }

    #[test]
    fn exit_code_absent_on_ordinary_response() {
        let json = r#"{"id":"msg-009","value":42,"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.exit_code, None);
    }

    #[test]
    fn deserialize_bindings() {
        let json = r#"{"id":"msg-002","bindings":{"x":42},"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(!resp.is_error());
        let bindings = resp.bindings.unwrap();
        assert_eq!(bindings["x"], 42);
    }

    #[test]
    fn deserialize_actors() {
        let json = r#"{"id":"msg-003","actors":[{"pid":"<0.123.0>","class":"Counter","module":"counter","spawned_at":1234567890}],"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        let actors = resp.actors.unwrap();
        assert_eq!(actors.len(), 1);
        assert_eq!(actors[0].class, "Counter");
        assert_eq!(actors[0].pid, "<0.123.0>");
    }

    #[test]
    fn deserialize_null_errors_as_empty() {
        let json = r#"{"id":"msg-004","errors":null,"status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(resp.errors.is_empty());
    }

    #[test]
    fn test_error_status() {
        let json = r#"{"id":"msg-005","results":{},"status":["done","test-error"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert!(!resp.is_error());
        assert!(resp.has_test_error());
    }

    #[test]
    fn value_string_for_string_value() {
        let json = r#"{"id":"msg-006","value":"hello","status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.value_string(), "hello");
    }

    #[test]
    fn value_string_for_null_value() {
        let json = r#"{"id":"msg-007","status":["done"]}"#;
        let resp: ReplResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.value_string(), "");
    }

    #[test]
    fn deserialize_class_info() {
        let json = r#"{"name":"String","superclass":"Value","doc":"A string","sealed":true,"abstract":false}"#;
        let info: ClassInfo = serde_json::from_str(json).unwrap();
        assert_eq!(info.name, "String");
        assert_eq!(info.superclass.as_deref(), Some("Value"));
        assert!(info.sealed);
        assert!(!info.is_abstract);
    }

    #[test]
    fn deserialize_module_info() {
        let json = r#"{"name":"Counter","source_file":"counter.bt","actor_count":1,"load_time":0,"time_ago":"2m ago"}"#;
        let info: ModuleInfo = serde_json::from_str(json).unwrap();
        assert_eq!(info.name, "Counter");
        assert_eq!(info.actor_count, 1);
    }
}
