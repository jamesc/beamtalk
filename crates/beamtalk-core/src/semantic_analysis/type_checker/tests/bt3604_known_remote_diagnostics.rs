// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3604 (ADR 0126 §6, Phase 6): known-remote provenance diagnostics.
//!
//! Covers the known-remote flow fact end to end, through the full
//! `check_module` pipeline (real parsed source, not hand-built AST) so the
//! assignment-tracking and block-argument passes are both exercised exactly
//! as they run in production:
//!
//! * a local binding becomes known-remote via each of the five direct
//!   remote spawn/lookup sends (`spawnOn:`, `spawnWith:on:`, `spawnAs:on:`,
//!   `spawnWith:as:on:`, `named:on:`) and via a `scope: #global` send;
//! * the fact does NOT flow through a field assignment or a collection;
//! * the fact survives `withTimeout:`;
//! * the three ADR 0126 §6 diagnostic rows (`#process` handle, `#node`
//!   handle, block argument) at both a known-remote and a not-known-remote
//!   receiver, confirming the not-known-remote cases are unchanged from
//!   ADR 0103's pre-existing behaviour.
//!
//! `scope: #global` selectors (`spawnAs:scope:`, `named:scope:`, …) are not
//! yet declared as builtin `Actor` methods — BT-3603 (parallel sibling, same
//! epic wave) adds them to the runtime/class metadata. The classification in
//! `known_remote.rs` is purely syntactic (selector name + `scope:` argument
//! shape), so it needs no hierarchy declaration; the send does additionally
//! surface a DNU diagnostic in these tests (selector not found), which is
//! expected and irrelevant to what this file asserts — every assertion here
//! filters to `DiagnosticCategory::Sendability`.

use super::common::*;
use crate::source_analysis::Severity;

fn check_source(src: &str) -> Vec<Diagnostic> {
    let module = parse_source(src);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    checker.take_diagnostics()
}

/// The full semantic-analysis pipeline (`TypeChecker` + the Phase 3
/// validators, including `sendability_validators::check_block_capture_sendability`)
/// — needed for the block-argument Hint, which is emitted by that separate,
/// post-hoc validator rather than by `TypeChecker::check_module` itself.
fn check_source_full(src: &str) -> Vec<Diagnostic> {
    let module = parse_source(src);
    crate::semantic_analysis::analyse(&module).diagnostics
}

fn sendability(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Sendability))
        .collect()
}

fn node_warnings<'a>(diags: &[&'a Diagnostic]) -> Vec<&'a Diagnostic> {
    diags
        .iter()
        .copied()
        .filter(|d| d.message.contains("node-bound handle"))
        .collect()
}

fn process_warnings<'a>(diags: &[&'a Diagnostic]) -> Vec<&'a Diagnostic> {
    diags
        .iter()
        .copied()
        .filter(|d| d.message.contains("process-bound handle"))
        .collect()
}

fn block_hints<'a>(diags: &[&'a Diagnostic]) -> Vec<&'a Diagnostic> {
    diags
        .iter()
        .copied()
        .filter(|d| d.message.contains("block sent to a remote actor"))
        .collect()
}

// --- Known-remote binding via each spawn/lookup form (#node Warning proves it) ---

/// Shared fixture: `Worker` (Actor), whose `useNode:` accepts an
/// arbitrary argument — sending it a node-bound handle (`Ets`) is the
/// observable probe for "was the receiver known-remote at this send".
const WORKER_CLASS: &str = "Actor subclass: Worker\n  useNode: n => n asString\n\n";

fn probe(bind_expr: &str) -> String {
    format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: node :: Node with: cache :: Ets =>\n    \
         remote := {bind_expr}\n    \
         remote useNode: cache\n"
    )
}

/// Variant of [`probe`] with an explicit `remote :: Worker` annotation.
///
/// `spawnAs:scope:`/`named:scope:` are not yet declared builtin selectors
/// (BT-3603, the parallel sibling, lands them this same wave), so a
/// `scope: #global` send resolves as an unrecognised selector (DNU,
/// expected and irrelevant here) and its own INFERRED type collapses to
/// `Dynamic` — which would otherwise also collapse `remote`'s inferred
/// type, skipping the `Known`-receiver branch the observable `#node`
/// probe needs. The annotation supplies the type the checker would give
/// `remote` once BT-3603 lands `scope: #global`'s declared return type;
/// the known-remote FLOW FACT itself is computed from the RHS
/// expression's shape, not from the (here, annotation-overridden)
/// inferred type, so this does not fake the fact being tested.
fn probe_annotated(bind_expr: &str) -> String {
    format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: node :: Node with: cache :: Ets =>\n    \
         remote :: Worker := {bind_expr}\n    \
         remote useNode: cache\n"
    )
}

#[test]
fn spawn_on_binding_is_known_remote() {
    let diags = check_source(&probe("(Worker spawnOn: node) unwrap"));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "spawnOn: binding must be known-remote: {diags:?}"
    );
}

#[test]
fn spawn_with_on_binding_is_known_remote() {
    let diags = check_source(&probe("(Worker spawnWith: #{} on: node) unwrap"));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "spawnWith:on: binding must be known-remote: {diags:?}"
    );
}

#[test]
fn spawn_as_on_binding_is_known_remote() {
    let diags = check_source(&probe("(Worker spawnAs: #w on: node) unwrap"));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "spawnAs:on: binding must be known-remote: {diags:?}"
    );
}

#[test]
fn spawn_with_as_on_binding_is_known_remote() {
    let diags = check_source(&probe("(Worker spawnWith: #{} as: #w on: node) unwrap"));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "spawnWith:as:on: binding must be known-remote: {diags:?}"
    );
}

#[test]
fn named_on_binding_is_known_remote() {
    let diags = check_source(&probe("(Worker named: #w on: node) unwrap"));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "named:on: binding must be known-remote: {diags:?}"
    );
}

#[test]
fn scope_global_binding_is_known_remote() {
    let diags = check_source(&probe_annotated(
        "(Worker spawnAs: #w scope: #global) unwrap",
    ));
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "scope: #global binding must be known-remote: {diags:?}"
    );
}

#[test]
fn scope_local_binding_is_not_known_remote() {
    // `scope: #local` is explicitly NOT known-remote (ADR 0126 §6 table).
    let diags = check_source(&probe_annotated(
        "(Worker spawnAs: #w scope: #local) unwrap",
    ));
    assert!(
        node_warnings(&sendability(&diags)).is_empty(),
        "scope: #local binding must NOT be known-remote: {diags:?}"
    );
}

#[test]
fn local_spawn_binding_is_not_known_remote() {
    // Baseline: an ordinary local `spawn` is not known-remote — the #node
    // Warning must stay silent, matching ADR 0103's unchanged v1 behaviour.
    let diags = check_source(&probe("Worker spawn"));
    assert!(
        node_warnings(&sendability(&diags)).is_empty(),
        "a local spawn binding must NOT be known-remote: {diags:?}"
    );
}

// --- Provenance is flow-local: does NOT survive a field or a collection ---

#[test]
fn provenance_does_not_flow_through_field_assignment() {
    let src = format!(
        "{WORKER_CLASS}Actor subclass: Holder\n  \
         state: target = nil\n  \
         stash: node :: Node =>\n    \
         self.target := (Worker spawnOn: node) unwrap\n  \
         useNode: cache :: Ets =>\n    \
         self.target useNode: cache\n"
    );
    let diags = check_source(&src);
    assert!(
        node_warnings(&sendability(&diags)).is_empty(),
        "known-remote must NOT survive a field write/read: {diags:?}"
    );
}

#[test]
fn provenance_does_not_flow_through_a_collection() {
    let src = format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: node :: Node with: cache :: Ets =>\n    \
         remote := (Worker spawnOn: node) unwrap\n    \
         bag := Array with: remote\n    \
         extracted := bag first\n    \
         extracted useNode: cache\n"
    );
    let diags = check_source(&src);
    assert!(
        node_warnings(&sendability(&diags)).is_empty(),
        "known-remote must NOT survive a round trip through a collection: {diags:?}"
    );
}

// --- Provenance survives `withTimeout:` ---

#[test]
fn provenance_survives_with_timeout() {
    let src = format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: node :: Node with: cache :: Ets =>\n    \
         remote := (Worker spawnOn: node) unwrap\n    \
         proxy := remote withTimeout: 5000\n    \
         proxy useNode: cache\n"
    );
    let diags = check_source(&src);
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "known-remote must survive withTimeout: (ADR 0126 §6): {diags:?}"
    );
}

// --- `ifOk:ifError:` ok-branch (trivial forward) ---

#[test]
fn provenance_survives_trivial_if_ok_if_error_forward() {
    let src = format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: node :: Node with: cache :: Ets =>\n    \
         remote := (Worker spawnOn: node) ifOk: [:v | v] ifError: [:e | Worker spawn]\n    \
         remote useNode: cache\n"
    );
    let diags = check_source(&src);
    assert_eq!(
        node_warnings(&sendability(&diags)).len(),
        1,
        "trivial ifOk:ifError: forward must carry known-remote: {diags:?}"
    );
}

// --- Diagnostic row 1: HandleScoped(#process) — Warning, unchanged either way ---

#[test]
fn process_handle_warns_at_known_remote_receiver() {
    let src = "Actor subclass: Worker\n  usePort: p => p asString\n\n\
         Object subclass: Main\n  \
         run: node :: Node with: port :: Port =>\n    \
         remote := (Worker spawnOn: node) unwrap\n    \
         remote usePort: port\n";
    let diags = check_source(src);
    let warns = process_warnings(&sendability(&diags));
    assert_eq!(warns.len(), 1, "expected one warning, got: {diags:?}");
    assert_eq!(warns[0].severity, Severity::Warning);
}

#[test]
fn process_handle_warns_at_non_remote_receiver_unchanged() {
    // ADR 0103's existing behaviour: a local receiver's #process handle
    // warning is unaffected by ADR 0126 — same severity, same wording.
    let src = "Actor subclass: Worker\n  usePort: p => p asString\n\n\
         Object subclass: Main\n  \
         run: port :: Port =>\n    \
         w := Worker spawn\n    \
         w usePort: port\n";
    let diags = check_source(src);
    let warns = process_warnings(&sendability(&diags));
    assert_eq!(warns.len(), 1, "expected one warning, got: {diags:?}");
    assert_eq!(warns[0].severity, Severity::Warning);
}

// --- Diagnostic row 2: HandleScoped(#node) — silent locally, Warning known-remote ---

#[test]
fn node_handle_warns_at_known_remote_receiver() {
    let diags = check_source(&probe("(Worker spawnOn: node) unwrap"));
    let warns = node_warnings(&sendability(&diags));
    assert_eq!(warns.len(), 1, "expected one warning, got: {diags:?}");
    assert_eq!(warns[0].severity, Severity::Warning);
    assert!(
        warns[0].message.contains("rejected at runtime"),
        "unexpected message: {}",
        warns[0].message
    );
}

#[test]
fn node_handle_silent_at_non_remote_receiver_unchanged() {
    // ADR 0103's existing (silent, v1) behaviour for a receiver that is not
    // known-remote — the local-spawn baseline test above already exercises
    // this via `probe`, so pin it directly here too for clarity.
    let src = format!(
        "{WORKER_CLASS}Object subclass: Main\n  \
         run: cache :: Ets =>\n    \
         w := Worker spawn\n    \
         w useNode: cache\n"
    );
    let diags = check_source(&src);
    assert!(
        node_warnings(&sendability(&diags)).is_empty(),
        "a #node handle to a non-remote receiver must stay silent: {diags:?}"
    );
}

// --- Diagnostic row 3: Block argument — silent locally, Hint known-remote ---

#[test]
fn block_argument_hints_at_known_remote_receiver() {
    let src = "Actor subclass: Worker\n  runBlock: b => b value\n\n\
         Object subclass: Main\n  \
         run: node :: Node =>\n    \
         remote := (Worker spawnOn: node) unwrap\n    \
         remote runBlock: [:x | x]\n";
    let diags = check_source_full(src);
    let hints = block_hints(&sendability(&diags));
    assert_eq!(hints.len(), 1, "expected one hint, got: {diags:?}");
    assert_eq!(
        hints[0].severity,
        Severity::Hint,
        "must be Hint, not Warning"
    );
    assert!(
        hints[0].message.contains("on `node`"),
        "hint should name the target node variable: {}",
        hints[0].message
    );
}

#[test]
fn block_argument_silent_at_non_remote_receiver() {
    let src = "Actor subclass: Worker\n  runBlock: b => b value\n\n\
         Object subclass: Main\n  \
         run: dummy :: Integer =>\n    \
         w := Worker spawn\n    \
         w runBlock: [:x | x]\n";
    let diags = check_source_full(src);
    assert!(
        block_hints(&sendability(&diags)).is_empty(),
        "a block sent to a non-remote receiver must stay silent: {diags:?}"
    );
}
