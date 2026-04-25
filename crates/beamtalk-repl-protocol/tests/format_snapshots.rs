// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Snapshot tests for shared output formatters (BT-2086).
//!
//! These snapshots pin the canonical text rendering for every REPL surface.
//! Both [`OutputMode::Plain`] and [`OutputMode::Ansi`] are exercised so
//! divergence between modes (or accidental ANSI bleed into MCP responses) is
//! caught at review time.
//!
//! When a snapshot needs updating, run `cargo insta review`.

use beamtalk_repl_protocol::format::{self, Diagnostic, OutputMode, WarningStyle};
use beamtalk_repl_protocol::{ActorInfo, ClassInfo, ModuleInfo};

// ---------------------------------------------------------------------------
// format_value
// ---------------------------------------------------------------------------

#[test]
fn snapshot_value_string_plain() {
    let v = serde_json::json!("hello world");
    insta::assert_snapshot!(format::format_value(&v, OutputMode::Plain), @"hello world");
}

#[test]
fn snapshot_value_array_plain() {
    let v = serde_json::json!([1, 2, 3, [4, 5]]);
    insta::assert_snapshot!(
        format::format_value(&v, OutputMode::Plain),
        @"#(1, 2, 3, #(4, 5))"
    );
}

#[test]
fn snapshot_value_pid_plain() {
    let v = serde_json::json!("#Counter<0.123.0>");
    insta::assert_snapshot!(
        format::format_value(&v, OutputMode::Plain),
        @"#Counter<0.123.0>"
    );
}

#[test]
fn snapshot_value_object_plain() {
    let v = serde_json::json!({"x": 1, "y": "two"});
    insta::assert_snapshot!(
        format::format_value(&v, OutputMode::Plain),
        @"{x: 1, y: two}"
    );
}

/// Strip ANSI escape sequences so ANSI-mode snapshots remain readable as text.
/// We separately assert the raw bytes contain the expected escape codes.
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

#[test]
fn snapshot_value_string_ansi() {
    let v = serde_json::json!("hello");
    let out = format::format_value(&v, OutputMode::Ansi);
    assert!(out.contains("\x1b[32m"), "expected GREEN ANSI code");
    insta::assert_snapshot!(strip_ansi(&out), @"hello");
}

#[test]
fn snapshot_value_pid_ansi() {
    let v = serde_json::json!("#Counter<0.1.0>");
    let out = format::format_value(&v, OutputMode::Ansi);
    assert!(out.contains("\x1b[36m"), "expected CYAN ANSI code");
    insta::assert_snapshot!(strip_ansi(&out), @"#Counter<0.1.0>");
}

// ---------------------------------------------------------------------------
// format_diagnostic
// ---------------------------------------------------------------------------

#[test]
fn snapshot_diagnostic_message_only_plain() {
    let d = Diagnostic::new("Undefined variable: foo");
    insta::assert_snapshot!(
        format::format_diagnostic(&d, OutputMode::Plain),
        @"Error: Undefined variable: foo"
    );
}

#[test]
fn snapshot_diagnostic_full_plain() {
    let d = Diagnostic::new("Parse error: expected ']'")
        .with_line(7)
        .with_hint("close the open bracket on line 5");
    insta::assert_snapshot!(
        format::format_diagnostic(&d, OutputMode::Plain),
        @r#"
    Error: Parse error: expected ']'
    Line: 7
    Hint: close the open bracket on line 5
    "#
    );
}

#[test]
fn snapshot_diagnostic_full_ansi() {
    let d = Diagnostic::new("oops").with_line(1).with_hint("nope");
    let out = format::format_diagnostic(&d, OutputMode::Ansi);
    assert!(
        out.contains("\x1b[1m\x1b[31m"),
        "expected BOLD+RED ANSI prefix"
    );
    insta::assert_snapshot!(
        strip_ansi(&out),
        @r"
    Error: oops
    Line: 1
    Hint: nope
    "
    );
}

#[test]
fn snapshot_file_diagnostic_plain() {
    let err = serde_json::json!({
        "message": "duplicate class definition",
        "path": "src/Counter.bt",
        "line": 4,
        "hint": "remove the second class block"
    });
    insta::assert_snapshot!(
        format::format_file_diagnostic(&err, OutputMode::Plain).unwrap(),
        @"  Error: duplicate class definition in src/Counter.bt at line 4 (remove the second class block)"
    );
}

// ---------------------------------------------------------------------------
// format_warning
// ---------------------------------------------------------------------------

#[test]
fn snapshot_warning_prefixed_plain() {
    insta::assert_snapshot!(
        format::format_warning("deprecated method", OutputMode::Plain, WarningStyle::Prefixed),
        @"Warning: deprecated method"
    );
}

#[test]
fn snapshot_warning_bullet_plain() {
    insta::assert_snapshot!(
        format::format_warning("partial match", OutputMode::Plain, WarningStyle::Bullet),
        @"⚠ partial match"
    );
}

// ---------------------------------------------------------------------------
// format_trace_step
// ---------------------------------------------------------------------------

#[test]
fn snapshot_trace_step_plain() {
    let step = serde_json::json!({"src": "x := 42", "value": 42});
    insta::assert_snapshot!(
        format::format_trace_step(&step, OutputMode::Plain),
        @"x := 42 => 42"
    );
}

#[test]
fn snapshot_trace_step_with_actor_value() {
    let step = serde_json::json!({"src": "Counter spawn", "value": "#Counter<0.7.0>"});
    insta::assert_snapshot!(
        format::format_trace_step(&step, OutputMode::Plain),
        @"Counter spawn => #Counter<0.7.0>"
    );
}

// ---------------------------------------------------------------------------
// format_test_result
// ---------------------------------------------------------------------------

#[test]
fn snapshot_test_result_summary() {
    // Parse from a string so the resulting map preserves insertion order.
    let r: serde_json::Value = serde_json::from_str(
        r#"{"passed":12,"failed":1,"errors":[{"name":"test_x","message":"expected 1, got 2"}]}"#,
    )
    .unwrap();
    insta::assert_snapshot!(
        format::format_test_result(&r, OutputMode::Plain),
        @r#"
    {
      "errors": [
        {
          "message": "expected 1, got 2",
          "name": "test_x"
        }
      ],
      "failed": 1,
      "passed": 12
    }
    "#
    );
}

// ---------------------------------------------------------------------------
// format_actor_summary / list
// ---------------------------------------------------------------------------

fn make_actor(class: &str, pid: &str) -> ActorInfo {
    ActorInfo {
        pid: pid.to_string(),
        class: class.to_string(),
        module: "m".to_string(),
        spawned_at: 0,
    }
}

#[test]
fn snapshot_actor_list_empty() {
    insta::assert_snapshot!(
        format::format_actor_list(&[], OutputMode::Plain),
        @"No actors running"
    );
}

#[test]
fn snapshot_actor_list_multi_plain() {
    let actors = [
        make_actor("Counter", "<0.123.0>"),
        make_actor("Logger", "<0.124.0>"),
    ];
    insta::assert_snapshot!(
        format::format_actor_list(&actors, OutputMode::Plain),
        @r"
    Counter (<0.123.0>)
    Logger (<0.124.0>)
    "
    );
}

#[test]
fn snapshot_actor_summary_ansi() {
    let a = make_actor("Counter", "<0.1.0>");
    let out = format::format_actor_summary(&a, OutputMode::Ansi);
    assert!(
        out.contains("\x1b[36m"),
        "expected CYAN ANSI code on class name"
    );
    insta::assert_snapshot!(strip_ansi(&out), @"Counter (<0.1.0>)");
}

// ---------------------------------------------------------------------------
// format_class_summary / list
// ---------------------------------------------------------------------------

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
fn snapshot_class_list_empty() {
    insta::assert_snapshot!(
        format::format_class_list(&[], OutputMode::Plain),
        @"No classes found"
    );
}

#[test]
fn snapshot_class_list_plain() {
    let classes = [
        make_class(
            "String",
            Some("Value"),
            Some("Immutable string."),
            true,
            false,
        ),
        make_class("Stream", Some("Object"), None, false, true),
        make_class("Object", None, None, false, false),
    ];
    insta::assert_snapshot!(
        format::format_class_list(&classes, OutputMode::Plain),
        @r"
    String < Value [sealed] — Immutable string.
    Stream < Object [abstract]
    Object < (root)
    "
    );
}

// ---------------------------------------------------------------------------
// format_module_summary
// ---------------------------------------------------------------------------

#[test]
fn snapshot_module_summary_singular() {
    let m = ModuleInfo {
        name: "Counter".into(),
        source_file: "counter.bt".into(),
        actor_count: 1,
        load_time: 0,
        time_ago: "2m ago".into(),
    };
    insta::assert_snapshot!(
        format::format_module_summary(&m, OutputMode::Plain),
        @"Counter (1 actor, loaded 2m ago)"
    );
}

#[test]
fn snapshot_module_summary_plural() {
    let m = ModuleInfo {
        name: "Counter".into(),
        source_file: "counter.bt".into(),
        actor_count: 7,
        load_time: 0,
        time_ago: "now".into(),
    };
    insta::assert_snapshot!(
        format::format_module_summary(&m, OutputMode::Plain),
        @"Counter (7 actors, loaded now)"
    );
}
