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

    // --- Common payload ---
    /// Result value (eval, clear, kill, clone, etc.).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub value: Option<serde_json::Value>,

    /// Captured stdout from evaluation (BT-355).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output: Option<String>,

    /// Legacy error message field.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,

    /// Error message (current protocol).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    // --- Session / bindings ---
    /// Variable bindings map (bindings op).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bindings: Option<serde_json::Value>,

    // --- Load operations ---
    /// Loaded class names (eval/load-file/load-source/load-project).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub classes: Option<Vec<String>>,

    /// Class info list with metadata (list-classes op, BT-1404).
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
    /// Compilation warnings (BT-407).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub warnings: Option<Vec<String>>,

    /// Line number (1-based) of a compile error in the submitted snippet (BT-1235).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,

    /// Hint text for a compile error, where available (BT-1235).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,

    // --- Documentation ---
    /// Documentation text (docs op, BT-500).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,

    // --- Codegen ---
    /// Generated Core Erlang source (show-codegen op, BT-724).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub core_erlang: Option<String>,

    // --- Reload ---
    /// Number of actors affected by reload (BT-266).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub affected_actors: Option<u32>,

    /// Number of actors that failed code migration (BT-266).
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

    /// Incremental load summary (load-project op, BT-1685).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,

    // --- Test operations ---
    /// Test results (test / test-all ops).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub results: Option<serde_json::Value>,

    // --- Tracing (ADR 0069) ---
    /// Per-statement trace steps (eval with trace=true, BT-1238).
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

/// Class information from the list-classes op (BT-1404).
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
