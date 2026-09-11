// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0103 `handleScope:` validity and block-capture
//! sendability diagnostics, including end-to-end actor-message
//! integration.

use super::*;

// --- ADR 0103: handleScope: only valid on Object-kind classes ---

#[test]
fn handle_scope_on_object_class_is_silent() {
    let src = "typed Object subclass: MetricsTable native: metrics\n  handleScope: #node\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    assert!(
        !result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("handleScope:")),
        "handleScope on an Object class must not warn, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn handle_scope_on_value_class_warns() {
    let src = "Value subclass: MyPoint\n  handleScope: #node\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    assert!(
        result.diagnostics.iter().any(|d| {
            d.message.contains("handleScope:")
                && d.message.contains("value")
                && d.category == Some(crate::source_analysis::DiagnosticCategory::Sendability)
        }),
        "handleScope on a Value class must warn, got: {:?}",
        result.diagnostics
    );
}

// --- ADR 0103: Announcement payload sendability + companion lint ---

#[test]
fn announcement_payload_port_warns() {
    let src = "Object subclass: Main\n  \
        fire: port :: Port =>\n    \
        Announcer new announce: port\n";
    let diags = sendability_diags(src);
    assert!(
        diags.iter().any(|d| {
            d.message.contains("Announcement payload") && d.message.contains("process-bound")
        }),
        "Port announcement payload must warn, got: {diags:?}"
    );
}

#[test]
fn undeclared_handle_class_nudged() {
    let src = "typed Object subclass: MyHandle native: my_backing\n  \
        read -> Integer => 0\n";
    let diags = sendability_diags(src);
    assert!(
        diags
            .iter()
            .any(|d| { d.message.contains("MyHandle") && d.message.contains("handleScope:") }),
        "undeclared FFI-wrapping handle class must be nudged, got: {diags:?}"
    );
}

#[test]
fn declared_handle_class_not_nudged() {
    let src = "typed Object subclass: MyHandle native: my_backing\n  \
        handleScope: #process\n  \
        read -> Integer => 0\n";
    let diags = sendability_diags(src);
    assert!(
        !diags.iter().any(|d| d.message.contains("declares no")),
        "a class with handleScope: must not be nudged, got: {diags:?}"
    );
}

#[test]
fn inherited_handle_scope_not_nudged() {
    // A native: Object subclass whose *parent* declares handleScope: inherits
    // the scope, so the companion lint must not nudge it (suppress path).
    let src = "typed Object subclass: HandleBase native: hb\n  \
        handleScope: #process\n  \
        read -> Integer => 0\n\n\
        typed HandleBase subclass: HandleChild native: hc\n  \
        write -> Integer => 0\n";
    let diags = sendability_diags(src);
    assert!(
        !diags.iter().any(|d| d.message.contains("declares no")),
        "a subclass inheriting handleScope: from a parent must not be nudged, got: {diags:?}"
    );
}

#[test]
fn plain_object_class_not_nudged() {
    // No `native:` — not an FFI-wrapping class; must not be nudged.
    let src = "Object subclass: Plain\n  \
        doThing -> Integer => 0\n";
    let diags = sendability_diags(src);
    assert!(
        diags.is_empty(),
        "a plain Object class must not be nudged, got: {diags:?}"
    );
}

// --- ADR 0103: block-capture sendability ---

fn sendability_diags(src: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(crate::source_analysis::DiagnosticCategory::Sendability))
        .collect()
}

// --- ADR 0103: end-to-end diagnostic integration ---
// User-declared handleScope crossing a boundary, and #node silence.

#[test]
fn e2e_user_process_handle_in_actor_message_warns() {
    let src = "typed Object subclass: DbConn native: db\n  \
        handleScope: #process\n  \
        query -> Integer => 0\n\n\
        Actor subclass: Worker\n  \
        use: conn => conn query\n\n\
        Object subclass: Main\n  \
        run: conn :: DbConn with: worker :: Worker =>\n    \
        worker use: conn\n";
    let diags = sendability_diags(src);
    assert!(
        diags.iter().any(|d| {
            d.message.contains("`conn`") && d.message.contains("passed in an actor message")
        }),
        "user handleScope: #process value in an actor message must warn, got: {diags:?}"
    );
}

#[test]
fn e2e_node_scoped_handle_in_actor_message_silent() {
    let src = "typed Object subclass: Cache native: c\n  \
        handleScope: #node\n  \
        get -> Integer => 0\n\n\
        Actor subclass: Worker\n  \
        use: cache => cache get\n\n\
        Object subclass: Main\n  \
        run: cache :: Cache with: worker :: Worker =>\n    \
        worker use: cache\n";
    let diags = sendability_diags(src);
    assert!(
        !diags.iter().any(|d| d.message.contains("`cache`")),
        "#node-scoped value in an actor message must be silent in v1, got: {diags:?}"
    );
}

#[test]
fn block_captures_port_sent_to_actor_warns() {
    let src = "Actor subclass: Worker\n  \
        schedule: aBlock => aBlock value\n\n\
        Object subclass: Main\n  \
        run: port :: Port with: worker :: Worker =>\n    \
        worker schedule: [port asString]\n";
    let diags = sendability_diags(src);
    assert_eq!(
        diags.len(),
        1,
        "expected one block-capture warning: {diags:?}"
    );
    assert!(
        diags[0].message.contains("block captures `port`") && diags[0].message.contains("sent to"),
        "unexpected message: {}",
        diags[0].message
    );
}

#[test]
fn block_captures_port_in_local_do_is_silent() {
    // `do:` is a state-threading selector — the block stays in this process.
    let src = "Object subclass: Main\n  \
        run: port :: Port =>\n    \
        #(1, 2) do: [:each | port asString]\n";
    let diags = sendability_diags(src);
    assert!(
        diags.is_empty(),
        "block in a local do: must not warn: {diags:?}"
    );
}

#[test]
fn block_captures_sendable_value_is_silent() {
    let src = "Actor subclass: Worker\n  \
        schedule: aBlock => aBlock value\n\n\
        Object subclass: Main\n  \
        run: count :: Integer with: worker :: Worker =>\n    \
        worker schedule: [count printString]\n";
    let diags = sendability_diags(src);
    assert!(
        diags.is_empty(),
        "capturing a Sendable value must not warn: {diags:?}"
    );
}
