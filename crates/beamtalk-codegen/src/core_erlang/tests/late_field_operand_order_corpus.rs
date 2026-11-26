// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5339: a `late` field read is a racing operand in every operand builder
//! and every scope kind, proved over a generated grid rather than the
//! beamtalk-watcher repro.
//!
//! Invariant: the operands of one Core Erlang construct evaluate in source
//! order whenever two or more of them may run code or raise, and a `late`
//! field read may raise (`uninitialized_state_error`, ADR 0124 §3). The grid is
//! operand builder (list, map, tuple, actor send, self send, cascade, binary
//! operator, FFI call) x operand pair (each ordered pair of a `late` read, an
//! eager read, a send, a local, a raising division and compound operands
//! holding a `late` read or a division, with at least one `late` read) x scope
//! (method root, conditional arm, evaluated block, `collect:` body, `on:do:`
//! body, `ensure:` body, actor closure). Every cell compiles clean under the
//! debug verifier (`operand_order_check`'s `RacingOperands`, the `ThreadedIr`
//! checks). With `Fault::LateFieldReadsStable` (the pre-fix rule) every list,
//! map, send and self-send cell holding two `late` reads outside a
//! cell-reading scope is reported, so the grid fails on the bug.

use super::{assert_parses, verifier_report};
use crate::core_erlang::fault::Fault;

const HEADER: &str = "typed Actor subclass: LateGrid
  late state: first :: Integer
  late state: second :: Integer
  state: count :: Integer = 0

  pair: a :: Object with: b :: Object -> Object => a
";

/// Operand builders, `{A}`/`{B}` the operands. The flag is whether the builder
/// lowers to a list cell or map literal, the constructs the operand-order
/// check scans.
const BUILDERS: &[(&str, &str, bool)] = &[
    ("list", "#({A}, {B})", true),
    ("map", "#{#a => {A}, #b => {B}}", true),
    ("tuple", "{{A}, {B}}", false),
    ("send", "s pair: {A} with: {B}", true),
    ("selfsend", "self pair: {A} with: {B}", true),
    ("cascade", "s yourself; pair: {A} with: {B}", true),
    ("binary", "{A} + {B}", false),
    ("ffi", "Erlang lists seq: {A} with: {B}", false),
];

/// Operands: name, source, whether it is a `late` read.
const OPERANDS: &[(&str, &str, bool)] = &[
    ("late1", "self.first", true),
    ("late2", "self.second", true),
    ("eager", "self.count", false),
    ("send", "(s n)", false),
    ("local", "s", false),
    ("div", "(self.count / 0)", false),
    ("lateplus", "(self.first + 1)", true),
    ("divplus", "(self.count / 1)", false),
];

/// Scopes, `{E}` the expression. The flag is whether `{E}` reads fields
/// through the actor's cell (an actor closure, an evaluated block, a
/// `collect:` body), where every field read is a live lookup that already
/// races (`last_racing_operand`), so the pre-fix rule changes nothing there.
const SCOPES: &[(&str, &str, bool)] = &[
    ("root", "{E}", false),
    ("arm", "f ifTrue: [{E}] ifFalse: [nil]", false),
    ("value", "[{E}] value", true),
    ("collect", "#(1) collect: [:i | {E}]", true),
    ("ondo", "[{E}] on: Error do: [:e | nil]", false),
    ("ensure", "[{E}] ensure: [nil]", false),
    ("closure", "[:k | {E}]", true),
];

/// One grid cell's method, and whether it holds two `late` reads in a scanned
/// construct outside a cell-reading scope (the pre-fix miscompile).
fn cells() -> Vec<(String, String, bool)> {
    let mut out = Vec::new();
    for (b, builder, scanned) in BUILDERS {
        for (a_name, a, a_late) in OPERANDS {
            for (b_name, b_src, b_late) in OPERANDS {
                if !(*a_late || *b_late) || a_name == b_name {
                    continue;
                }
                for (s, scope, cell) in SCOPES {
                    let expr = builder.replace("{A}", a).replace("{B}", b_src);
                    let body = scope.replace("{E}", &expr);
                    let name = format!("{b}_{a_name}_{b_name}_{s}");
                    let method = format!("  {name}: s :: Object f: f :: Boolean => {body}\n");
                    let racing = *scanned && *a_late && *b_late && !*cell;
                    out.push((name, method, racing));
                }
            }
        }
    }
    out
}

#[test]
fn late_operand_grid_compiles_clean() {
    let cells = cells();
    assert!(cells.len() > 200, "grid too small: {}", cells.len());
    let failures: Vec<String> = cells
        .iter()
        .filter_map(|(name, method, _)| {
            let src = format!("{HEADER}\n{method}");
            assert_parses(&src);
            verifier_report(&src, None).map(|r| format!("{name}: {r}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn late_operand_grid_fails_under_the_pre_fix_rule() {
    let racing: Vec<_> = cells().into_iter().filter(|(_, _, r)| *r).collect();
    assert!(!racing.is_empty());
    let missed: Vec<&str> = racing
        .iter()
        .filter(|(_, method, _)| {
            let src = format!("{HEADER}\n{method}");
            !verifier_report(&src, Some(Fault::LateFieldReadsStable))
                .is_some_and(|r| r.contains("RacingOperands"))
        })
        .map(|(name, _, _)| name.as_str())
        .collect();
    assert!(
        missed.is_empty(),
        "pre-fix rule not reported for: {missed:?}"
    );
}
