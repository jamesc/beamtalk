// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4650: edge cases of the BT-4636 block-write widening — match-arm
//! shadowing, receiver blocks inside a send argument, and the known-remote
//! fact (ADR 0126 §6) surviving a widening whose every arm is remote.

use super::common::*;

fn check(src: &str) -> Vec<Diagnostic> {
    let (module, perrs) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(perrs.is_empty(), "parse errors: {perrs:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn dnu(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect()
}

#[test]
fn match_arm_pattern_variable_does_not_widen_a_shadowed_outer_local() {
    // The arm binds its own `x`; the block's write is to that binding, so the
    // outer `x` stays an Integer and `abs` must not be reported.
    let diags = check(
        "Object subclass: Probe\n  class f: items :: List(Integer) with: v :: Integer -> Integer =>\n    x := 0\n    items inject: (v match: [x -> [x := \"a\"]]) into: [:acc :e | acc]\n    x abs\n",
    );
    assert!(dnu(&diags).is_empty(), "outer x was widened: {diags:#?}");
}

#[test]
fn receiver_block_write_is_visible_to_sibling_argument() {
    // `[i := "x". 1] value` runs before `i size` is evaluated, so the sibling
    // argument sees `i` widened to `Integer | String`. Before the fix it saw
    // only the stale `Integer`, and the DNU did not mention the union.
    let diags = check(
        "Object subclass: Probe\n  class f: d :: Dictionary(Integer, Integer) -> Integer =>\n    i := 0\n    d at: ([i := \"x\". 1] value) put: i size\n    0\n",
    );
    let found = dnu(&diags);
    assert!(
        found
            .iter()
            .any(|d| d.message.contains("in union Integer | String")),
        "sibling saw the old type: {diags:#?}"
    );
}

const WORKERS: &str = "Actor subclass: Worker\n  useNode: n => n asString\n\nWorker subclass: Other\n  useNode: n => n asString\n\n";

fn remote_probe(body: &str) -> String {
    format!(
        "{WORKERS}Object subclass: Main\n  run: node :: Node with: cache :: Ets with: items :: List(Integer) =>\n    remote := (Worker spawnOn: node) unwrap\n{body}    remote useNode: cache\n"
    )
}

/// Whether the final `remote` read in `src` carries the known-remote fact.
/// A joined type is a union, which no sendability warning fires on, so the
/// fact is read from the per-identifier table the block-capture hint uses.
fn last_read_is_known_remote(src: &str) -> bool {
    let (module, perrs) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(perrs.is_empty(), "parse errors: {perrs:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let spans = checker.take_known_remote_spans();
    let offset = src.rfind("remote useNode:").unwrap();
    spans.keys().any(|span| span.start() as usize == offset)
}

#[test]
fn widening_keeps_known_remote_when_every_arm_is_remote() {
    assert!(last_read_is_known_remote(&remote_probe(
        "    items do: [:e | remote := (Other spawnOn: node) unwrap]\n",
    )));
}

#[test]
fn widening_drops_known_remote_when_an_arm_is_not_remote() {
    assert!(!last_read_is_known_remote(&remote_probe(
        "    items do: [:e | remote := Other new]\n",
    )));
}
