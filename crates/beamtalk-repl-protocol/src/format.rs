// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared output formatters for REPL response payloads (BT-2086).
//!
//! **DDD Context:** REPL — Presentation Contract
//!
//! These helpers produce the canonical text rendering of REPL response data
//! (values, diagnostics, trace steps, test results, actor/class summaries) and
//! are consumed by every surface that displays REPL output: `beamtalk-cli`
//! (interactive REPL + `attach`), `beamtalk-mcp` (tool responses), and any
//! future client.
//!
//! # Design
//!
//! Each formatter accepts an [`OutputMode`] that selects the rendering style:
//!
//! * [`OutputMode::Plain`] — no escape sequences, suitable for MCP, logs,
//!   and snapshot tests.
//! * [`OutputMode::Ansi`] — ANSI colour escapes for interactive terminals.
//!
//! Surface-specific decoration (prompts, leading bullets, trailing newlines)
//! stays at the call site; only the value-shape rendering is shared.
//!
//! # Why not LSP?
//!
//! The LSP server emits structured `lsp_types::Diagnostic` values, not
//! free-form text. The diagnostic-message construction in LSP shares
//! conceptual structure (`message + notes + hint`) with these formatters but
//! operates on a different input type (`beamtalk_core::language_service::Diagnostic`).
//! Including LSP would force a `beamtalk-core` dependency on this crate and
//! is deliberately out of scope here.

use crate::response::{ActorInfo, ClassInfo, ModuleInfo};

// ---------------------------------------------------------------------------
// OutputMode
// ---------------------------------------------------------------------------

/// Selects how formatters decorate their output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputMode {
    /// No escape sequences. Suitable for MCP responses, logs, and tests.
    #[default]
    Plain,
    /// ANSI colour escapes for interactive terminals.
    Ansi,
}

impl OutputMode {
    /// Returns true when output should include ANSI colour codes.
    #[must_use]
    pub fn is_ansi(self) -> bool {
        matches!(self, Self::Ansi)
    }
}

// ---------------------------------------------------------------------------
// ANSI colour codes (kept private — surfaces use the formatters, not the codes)
// ---------------------------------------------------------------------------

const RESET: &str = "\x1b[0m";
const BOLD: &str = "\x1b[1m";
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const BOLD_BLUE: &str = "\x1b[1;34m";

fn paint(mode: OutputMode, color: &str, text: &str) -> String {
    if mode.is_ansi() {
        format!("{color}{text}{RESET}")
    } else {
        text.to_string()
    }
}

// ---------------------------------------------------------------------------
// format_value
// ---------------------------------------------------------------------------

/// Format a REPL eval result for display.
///
/// Mirrors the conventions established by the legacy CLI formatter:
///
/// * Strings beginning with `#` and containing `<` (e.g. `"#Actor<0.123.0>"`)
///   or starting with `"a Block"` are rendered as cyan opaque values.
/// * Strings that look like floats (contain `.` and parse as `f64`) — sent
///   as strings to preserve trailing zeros (BT-1336) — are rendered as numbers.
/// * Plain strings render unquoted in green.
/// * Numbers render in yellow, booleans/`null` in bold-blue.
/// * Arrays render as Beamtalk list literals: `#(a, b, c)`.
/// * Objects render as `{k: v, ...}`.
#[must_use]
pub fn format_value(value: &serde_json::Value, mode: OutputMode) -> String {
    match value {
        serde_json::Value::String(s) => {
            if (s.starts_with('#') && s.contains('<')) || s.starts_with("a Block") {
                paint(mode, CYAN, s)
            } else if s.contains('.') && s.parse::<f64>().is_ok() {
                paint(mode, YELLOW, s)
            } else {
                paint(mode, GREEN, s)
            }
        }
        serde_json::Value::Number(n) => paint(mode, YELLOW, &n.to_string()),
        serde_json::Value::Bool(b) => paint(mode, BOLD_BLUE, &b.to_string()),
        serde_json::Value::Null => paint(mode, BOLD_BLUE, "nil"),
        serde_json::Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(|v| format_value(v, mode)).collect();
            format!("#({})", items.join(", "))
        }
        serde_json::Value::Object(obj) => {
            let pairs: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{k}: {}", format_value(v, mode)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
    }
}

// ---------------------------------------------------------------------------
// format_diagnostic
// ---------------------------------------------------------------------------

/// A diagnostic to render. Borrowed from a [`ReplResponse`] (or constructed
/// from a per-file error map inside `errors`).
///
/// [`ReplResponse`]: crate::response::ReplResponse
#[derive(Debug, Clone, Copy)]
pub struct Diagnostic<'a> {
    /// Primary error message (required).
    pub message: &'a str,
    /// Optional 1-based line number in the submitted snippet (BT-1235).
    pub line: Option<u32>,
    /// Optional hint text (BT-1235).
    pub hint: Option<&'a str>,
}

impl<'a> Diagnostic<'a> {
    /// Create a diagnostic with just a message.
    #[must_use]
    pub fn new(message: &'a str) -> Self {
        Self {
            message,
            line: None,
            hint: None,
        }
    }

    /// Attach a line number.
    #[must_use]
    pub fn with_line(mut self, line: u32) -> Self {
        self.line = Some(line);
        self
    }

    /// Attach a hint.
    #[must_use]
    pub fn with_hint(mut self, hint: &'a str) -> Self {
        self.hint = Some(hint);
        self
    }
}

/// Format a diagnostic as `Error: <msg>` with optional `Line:` and `Hint:`
/// continuation lines. The bold-red `Error:` prefix is only emitted in ANSI
/// mode.
///
/// This shape is shared by the CLI REPL (`format_error`) and MCP tool
/// responses (`evaluate` error path).
#[must_use]
pub fn format_diagnostic(diag: &Diagnostic<'_>, mode: OutputMode) -> String {
    use std::fmt::Write as _;

    let mut out = if mode.is_ansi() {
        format!("{BOLD}{RED}Error:{RESET} {}", diag.message)
    } else {
        format!("Error: {}", diag.message)
    };

    if let Some(line) = diag.line {
        let _ = write!(out, "\nLine: {line}");
    }
    if let Some(hint) = diag.hint {
        let _ = write!(out, "\nHint: {hint}");
    }

    out
}

/// Format a single per-file diagnostic from a `load-project`/`sync` response.
///
/// Each entry in `ReplResponse::errors` is a JSON map with at least
/// `message`, plus optional `path`, `line`, and `hint` fields. This helper
/// renders the legacy CLI line shape:
///
/// ```text
///   Error: <msg> in <path> at line <N> (<hint>)
/// ```
#[must_use]
pub fn format_file_diagnostic(err: &serde_json::Value, mode: OutputMode) -> Option<String> {
    // Non-object entries (raw strings or arbitrary JSON) — surface them rather
    // than silently dropping. They have no path/line/hint to attach.
    if !err.is_object() {
        let raw = err.as_str().map_or_else(|| err.to_string(), str::to_owned);
        return Some(format!("  {}", paint(mode, RED, &format!("Error: {raw}"))));
    }

    let msg = err
        .get("message")
        .and_then(|m| m.as_str())
        .map_or_else(|| err.to_string(), str::to_owned);
    let path = err
        .get("path")
        .and_then(|p| p.as_str())
        .unwrap_or("unknown");
    let line = err.get("line").and_then(serde_json::Value::as_u64);
    let hint = err.get("hint").and_then(serde_json::Value::as_str);

    let painted = paint(mode, RED, &format!("Error: {msg}"));
    let suffix = match (line, hint) {
        (Some(ln), Some(h)) => format!(" in {path} at line {ln} ({h})"),
        (Some(ln), None) => format!(" in {path} at line {ln}"),
        (None, Some(h)) => format!(" in {path} ({h})"),
        (None, None) => format!(" in {path}"),
    };
    Some(format!("  {painted}{suffix}"))
}

/// Format a compilation warning consistently across surfaces.
///
/// CLI uses `Warning: <msg>` (yellow); MCP/REPL eval uses `⚠ <msg>` for
/// stdout-friendly output. The output style is selected via `style`.
#[must_use]
pub fn format_warning(msg: &str, mode: OutputMode, style: WarningStyle) -> String {
    match style {
        WarningStyle::Prefixed => paint(mode, YELLOW, &format!("Warning: {msg}")),
        WarningStyle::Bullet => paint(mode, YELLOW, &format!("⚠ {msg}")),
    }
}

/// Style of warning rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarningStyle {
    /// `Warning: <msg>` — used by sync diagnostics, show-codegen, etc.
    Prefixed,
    /// `⚠ <msg>` — used inline in eval response output (BT-407).
    Bullet,
}

// ---------------------------------------------------------------------------
// format_trace_step
// ---------------------------------------------------------------------------

/// Format one entry from a trace-step list (BT-1238 / ADR 0069).
///
/// Each step is a JSON object with `src` (source text) and `value` (evaluated
/// result, may be a JSON string or any other value). Renders as:
///
/// ```text
/// <src> => <value>
/// ```
#[must_use]
pub fn format_trace_step(step: &serde_json::Value, mode: OutputMode) -> String {
    let src = step.get("src").and_then(|v| v.as_str()).unwrap_or("?");
    let val = step
        .get("value")
        .map_or_else(|| "nil".to_string(), |v| format_value(v, mode));
    format!("{src} => {val}")
}

// ---------------------------------------------------------------------------
// format_test_result
// ---------------------------------------------------------------------------

/// Format the structured `results` payload from a `test`/`test-all` response.
///
/// The wire shape varies (`BUnit` results use one schema, `Workspace test`
/// uses another), so this helper delegates to a pretty-printed JSON
/// rendering with stable key ordering for snapshot tests. Surfaces that
/// want a human summary line should compose it from named fields directly.
#[must_use]
pub fn format_test_result(results: &serde_json::Value, _mode: OutputMode) -> String {
    serde_json::to_string_pretty(results).unwrap_or_else(|_| results.to_string())
}

// ---------------------------------------------------------------------------
// format_actor_summary
// ---------------------------------------------------------------------------

/// Single-line actor summary in `<Class> (<pid>)` form. Used by the CLI REPL
/// startup banner ("Available actors:") and the MCP `list_actors` tool.
#[must_use]
pub fn format_actor_summary(actor: &ActorInfo, mode: OutputMode) -> String {
    let class = paint(mode, CYAN, &actor.class);
    format!("{class} ({})", actor.pid)
}

/// Format a list of actors as one summary per line. Returns `"No actors running"`
/// when the list is empty.
#[must_use]
pub fn format_actor_list(actors: &[ActorInfo], mode: OutputMode) -> String {
    if actors.is_empty() {
        return "No actors running".to_string();
    }
    actors
        .iter()
        .map(|a| format_actor_summary(a, mode))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// format_class_summary
// ---------------------------------------------------------------------------

/// Single-line class summary used by `list_classes` (BT-1404).
///
/// Renders as `<Name> < <Super>[ [sealed, abstract]] — <doc>`, with the
/// dash + doc suffix omitted when no documentation is present.
#[must_use]
pub fn format_class_summary(class: &ClassInfo, mode: OutputMode) -> String {
    let super_str = class.superclass.as_deref().unwrap_or("(root)");
    let doc_str = class
        .doc
        .as_deref()
        .unwrap_or("")
        .lines()
        .next()
        .unwrap_or("")
        .trim();
    let modifiers = {
        let mut m = Vec::new();
        if class.sealed {
            m.push("sealed");
        }
        if class.is_abstract {
            m.push("abstract");
        }
        if m.is_empty() {
            String::new()
        } else {
            format!(" [{}]", m.join(", "))
        }
    };
    let name = paint(mode, BOLD, &class.name);
    if doc_str.is_empty() {
        format!("{name} < {super_str}{modifiers}")
    } else {
        format!("{name} < {super_str}{modifiers} — {doc_str}")
    }
}

/// Format a class list, returning `"No classes found"` for an empty list.
#[must_use]
pub fn format_class_list(classes: &[ClassInfo], mode: OutputMode) -> String {
    if classes.is_empty() {
        return "No classes found".to_string();
    }
    classes
        .iter()
        .map(|c| format_class_summary(c, mode))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// format_module_summary
// ---------------------------------------------------------------------------

/// Single-line module summary: `<name> (<actor_count> actors, loaded <time_ago>)`.
#[must_use]
pub fn format_module_summary(module: &ModuleInfo, mode: OutputMode) -> String {
    let name = paint(mode, BOLD, &module.name);
    format!(
        "{name} ({} actor{}, loaded {})",
        module.actor_count,
        if module.actor_count == 1 { "" } else { "s" },
        module.time_ago,
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn strip_ansi(s: &str) -> String {
        let mut out = String::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\x1b' {
                for ch in chars.by_ref() {
                    if ch == 'm' {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    // --- format_value ---

    #[test]
    fn value_string_plain() {
        let v = serde_json::json!("hello");
        assert_eq!(format_value(&v, OutputMode::Plain), "hello");
    }

    #[test]
    fn value_pid_plain() {
        let v = serde_json::json!("#Actor<0.123.0>");
        assert_eq!(format_value(&v, OutputMode::Plain), "#Actor<0.123.0>");
    }

    #[test]
    fn value_block_plain() {
        let v = serde_json::json!("a Block/2");
        assert_eq!(format_value(&v, OutputMode::Plain), "a Block/2");
    }

    #[test]
    fn value_float_string_plain() {
        // BT-1336: floats arrive as strings to preserve ".0"
        let v = serde_json::json!("6.0");
        assert_eq!(format_value(&v, OutputMode::Plain), "6.0");
    }

    #[test]
    fn value_number_plain() {
        let v = serde_json::json!(42);
        assert_eq!(format_value(&v, OutputMode::Plain), "42");
    }

    #[test]
    fn value_bool_plain() {
        let v = serde_json::json!(true);
        assert_eq!(format_value(&v, OutputMode::Plain), "true");
    }

    #[test]
    fn value_null_plain() {
        let v = serde_json::json!(null);
        assert_eq!(format_value(&v, OutputMode::Plain), "nil");
    }

    #[test]
    fn value_array_plain() {
        let v = serde_json::json!([1, 2, 3]);
        assert_eq!(format_value(&v, OutputMode::Plain), "#(1, 2, 3)");
    }

    #[test]
    fn value_nested_array_plain() {
        let v = serde_json::json!([[1, 2], [3, 4]]);
        assert_eq!(format_value(&v, OutputMode::Plain), "#(#(1, 2), #(3, 4))");
    }

    #[test]
    fn value_string_ansi_is_green() {
        let v = serde_json::json!("hello");
        let out = format_value(&v, OutputMode::Ansi);
        assert!(out.contains(GREEN), "expected GREEN colour, got {out:?}");
        assert_eq!(strip_ansi(&out), "hello");
    }

    // --- format_diagnostic ---

    #[test]
    fn diagnostic_plain_message_only() {
        let d = Diagnostic::new("oops");
        assert_eq!(format_diagnostic(&d, OutputMode::Plain), "Error: oops");
    }

    #[test]
    fn diagnostic_plain_with_line_and_hint() {
        let d = Diagnostic::new("undefined")
            .with_line(3)
            .with_hint("did you mean foo?");
        assert_eq!(
            format_diagnostic(&d, OutputMode::Plain),
            "Error: undefined\nLine: 3\nHint: did you mean foo?"
        );
    }

    #[test]
    fn diagnostic_ansi_includes_red() {
        let d = Diagnostic::new("oops");
        let out = format_diagnostic(&d, OutputMode::Ansi);
        assert!(out.contains(RED));
        assert_eq!(strip_ansi(&out), "Error: oops");
    }

    #[test]
    fn file_diagnostic_full() {
        let err = serde_json::json!({
            "message": "parse error",
            "path": "src/foo.bt",
            "line": 12,
            "hint": "missing ]"
        });
        let out = format_file_diagnostic(&err, OutputMode::Plain).unwrap();
        assert_eq!(
            out,
            "  Error: parse error in src/foo.bt at line 12 (missing ])"
        );
    }

    #[test]
    fn file_diagnostic_no_message_falls_back_to_raw_json() {
        let err = serde_json::json!({"path": "x.bt"});
        let out = format_file_diagnostic(&err, OutputMode::Plain).unwrap();
        assert!(out.contains("x.bt"));
        assert!(out.contains("Error:"));
    }

    #[test]
    fn file_diagnostic_string_entry_surfaces_text() {
        let err = serde_json::json!("disk full");
        let out = format_file_diagnostic(&err, OutputMode::Plain).unwrap();
        assert_eq!(out, "  Error: disk full");
    }

    #[test]
    fn file_diagnostic_unknown_path() {
        let err = serde_json::json!({"message": "boom"});
        let out = format_file_diagnostic(&err, OutputMode::Plain).unwrap();
        assert_eq!(out, "  Error: boom in unknown");
    }

    // --- format_warning ---

    #[test]
    fn warning_prefixed_plain() {
        assert_eq!(
            format_warning("deprecated", OutputMode::Plain, WarningStyle::Prefixed),
            "Warning: deprecated"
        );
    }

    #[test]
    fn warning_bullet_plain() {
        assert_eq!(
            format_warning("deprecated", OutputMode::Plain, WarningStyle::Bullet),
            "⚠ deprecated"
        );
    }

    // --- format_trace_step ---

    #[test]
    fn trace_step_plain() {
        let step = serde_json::json!({"src": "1 + 2", "value": 3});
        assert_eq!(format_trace_step(&step, OutputMode::Plain), "1 + 2 => 3");
    }

    #[test]
    fn trace_step_string_value() {
        let step = serde_json::json!({"src": "Actor", "value": "#Actor<0.1.0>"});
        assert_eq!(
            format_trace_step(&step, OutputMode::Plain),
            "Actor => #Actor<0.1.0>"
        );
    }

    #[test]
    fn trace_step_missing_value_renders_nil() {
        let step = serde_json::json!({"src": "x"});
        assert_eq!(format_trace_step(&step, OutputMode::Plain), "x => nil");
    }

    // --- format_test_result ---

    #[test]
    fn test_result_pretty_json() {
        let r = serde_json::json!({"passed": 3, "failed": 0});
        let out = format_test_result(&r, OutputMode::Plain);
        assert!(out.contains("\"passed\": 3"));
    }

    // --- format_actor_summary ---

    fn make_actor(class: &str, pid: &str) -> ActorInfo {
        ActorInfo {
            pid: pid.to_string(),
            class: class.to_string(),
            module: "m".to_string(),
            spawned_at: 0,
        }
    }

    #[test]
    fn actor_summary_plain() {
        let a = make_actor("Counter", "<0.123.0>");
        assert_eq!(
            format_actor_summary(&a, OutputMode::Plain),
            "Counter (<0.123.0>)"
        );
    }

    #[test]
    fn actor_list_empty() {
        assert_eq!(
            format_actor_list(&[], OutputMode::Plain),
            "No actors running"
        );
    }

    #[test]
    fn actor_list_multi() {
        let actors = vec![
            make_actor("Counter", "<0.1.0>"),
            make_actor("Logger", "<0.2.0>"),
        ];
        assert_eq!(
            format_actor_list(&actors, OutputMode::Plain),
            "Counter (<0.1.0>)\nLogger (<0.2.0>)"
        );
    }

    // --- format_class_summary ---

    fn make_class(
        name: &str,
        superclass: Option<&str>,
        doc: Option<&str>,
        sealed: bool,
        is_abstract: bool,
    ) -> ClassInfo {
        ClassInfo {
            name: name.to_string(),
            superclass: superclass.map(String::from),
            doc: doc.map(String::from),
            sealed,
            is_abstract,
        }
    }

    #[test]
    fn class_summary_minimal() {
        let c = make_class("Counter", Some("Actor"), None, false, false);
        assert_eq!(
            format_class_summary(&c, OutputMode::Plain),
            "Counter < Actor"
        );
    }

    #[test]
    fn class_summary_root() {
        let c = make_class("Object", None, None, false, false);
        assert_eq!(
            format_class_summary(&c, OutputMode::Plain),
            "Object < (root)"
        );
    }

    #[test]
    fn class_summary_with_modifiers_and_doc() {
        let c = make_class(
            "String",
            Some("Value"),
            Some("Immutable string.\nMore details."),
            true,
            false,
        );
        assert_eq!(
            format_class_summary(&c, OutputMode::Plain),
            "String < Value [sealed] — Immutable string."
        );
    }

    #[test]
    fn class_summary_sealed_and_abstract() {
        let c = make_class("Base", None, None, true, true);
        assert_eq!(
            format_class_summary(&c, OutputMode::Plain),
            "Base < (root) [sealed, abstract]"
        );
    }

    #[test]
    fn class_list_empty() {
        assert_eq!(
            format_class_list(&[], OutputMode::Plain),
            "No classes found"
        );
    }

    // --- format_module_summary ---

    fn make_module(name: &str, count: u32, time_ago: &str) -> ModuleInfo {
        ModuleInfo {
            name: name.to_string(),
            source_file: "x.bt".to_string(),
            actor_count: count,
            load_time: 0,
            time_ago: time_ago.to_string(),
        }
    }

    #[test]
    fn module_summary_singular() {
        let m = make_module("Counter", 1, "2m ago");
        assert_eq!(
            format_module_summary(&m, OutputMode::Plain),
            "Counter (1 actor, loaded 2m ago)"
        );
    }

    #[test]
    fn module_summary_plural() {
        let m = make_module("Counter", 3, "now");
        assert_eq!(
            format_module_summary(&m, OutputMode::Plain),
            "Counter (3 actors, loaded now)"
        );
    }

    #[test]
    fn module_summary_zero() {
        let m = make_module("Counter", 0, "5m ago");
        assert_eq!(
            format_module_summary(&m, OutputMode::Plain),
            "Counter (0 actors, loaded 5m ago)"
        );
    }
}
