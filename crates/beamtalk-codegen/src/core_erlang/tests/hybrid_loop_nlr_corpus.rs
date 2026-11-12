// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4523 (ADR 0146 P0): a `^` in a loop body stores and carries a state that
//! includes every field the loop wrote so far, in every loop mode.
//!
//! A hybrid-params loop keeps each field it writes in a fun parameter
//! (`_NField`), so the `State` in scope is the loop-entry map. A `^` in its
//! body stored that map to the cell and carried it in the throw:
//! `1 to: 5 do: [:i | self.n := i. k := i. i == 3 ifTrue: [^k]]` answered 3 and
//! left `n` at 0. Mode selection now keeps a body that holds a `^` out of
//! hybrid mode (`BodyEffects::has_nlr`), so it threads `StateAcc`; the other
//! modes already carry every write (`StateAcc` threads the whole `State`;
//! direct-params and tuple-accumulator bodies write no field).
//!
//! The check is independent of that gate: every hybrid lowering reports a `^`
//! whose carried state was read while its loop marker was innermost
//! (`VerifyError::NlrThrowInHybridLoop`, `checkout_hybrid_loop`). The corpus
//! is loop shape x `^` position x field write before/after the `^`: clean and
//! `erlc`-valid with the gate on, and with the gate off
//! (`Fault::HybridAdmitsNlr`) every shape that lowers hybrid reports.

use super::assert_compiles_through_erlc;
use crate::core_erlang::fault::{self, Fault};
use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

const HEADER: &str = "Actor subclass: HybridNlrProbe
  state: n = 0
";

/// The loops, with body `BODY` and an index (or local) `i`. Guard-free (`==`
/// is a bare BIF), so a field write and a threaded local alone keep the
/// letrec loops eligible for hybrid mode.
const LOOPS: &[(&str, &str)] = &[
    ("to_do", "    1 to: 5 do: [:i | BODY].\n"),
    ("to_by_do", "    1 to: 5 by: 2 do: [:i | BODY].\n"),
    ("times_repeat", "    i := 3.\n    5 timesRepeat: [BODY].\n"),
    ("while_true", "    i := 3.\n    [true] whileTrue: [BODY].\n"),
    (
        "while_false",
        "    i := 3.\n    [false] whileFalse: [BODY].\n",
    ),
    // Fold modes (never hybrid): the invariant holds there too.
    ("do", "    #(1, 2, 3) do: [:i | BODY].\n"),
    (
        "inject_into",
        "    #(1, 2, 3) inject: 0 into: [:a :i | BODY. a].\n",
    ),
];

/// How many of [`LOOPS`] (the leading ones) are letrec loops, eligible for
/// hybrid mode.
const LETREC_LOOPS: usize = 5;

/// Where the `^` sits in the body.
const RETURNS: &[(&str, &str)] = &[
    ("if_true", "i == 3 ifTrue: [^k]"),
    ("if_true_if_false", "i == 3 ifTrue: [^k] ifFalse: [nil]"),
    ("if_false", "i == 3 ifFalse: [^k]"),
    ("nested_if", "i == 3 ifTrue: [k == 3 ifTrue: [^k]]"),
    ("bare", "^k"),
];

/// The field write before or after the `^`.
const ORDERS: &[(&str, &str)] = &[
    ("write_before", "self.n := i. k := i. RET"),
    ("write_after", "k := i. RET. self.n := i"),
];

/// A `^` in a while loop's condition, with the field write in its body.
const CONDITION_RETURNS: &[(&str, &str)] = &[
    (
        "while_true_condition",
        "    i := 3.\n    [i == 3 ifTrue: [^k]. true] whileTrue: [self.n := i. k := i].\n",
    ),
    (
        "while_false_condition",
        "    i := 3.\n    [i == 3 ifTrue: [^k]. false] whileFalse: [self.n := i. k := i].\n",
    ),
];

/// The repro (BT-4523), which lowered hybrid before the fix.
const REPRO: &str = "to_do / if_true / write_before";

fn corpus() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (loop_name, lp) in LOOPS {
        for (ret_name, ret) in RETURNS {
            for (order_name, order) in ORDERS {
                let body = order.replace("RET", ret);
                out.push((
                    format!("{loop_name} / {ret_name} / {order_name}"),
                    lp.replace("BODY", &body),
                ));
            }
        }
    }
    for (name, lp) in CONDITION_RETURNS {
        out.push(((*name).to_string(), (*lp).to_string()));
    }
    out.into_iter()
        .map(|(name, lp)| {
            (
                name,
                format!("{HEADER}\n  probe =>\n    k := 0.\n{lp}    k\n"),
            )
        })
        .collect()
}

fn compile(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

/// The panic message of compiling `src` (a verifier finding is a debug-build
/// panic), or `None` when it compiles.
fn report(src: &str) -> Option<String> {
    match std::panic::catch_unwind(|| compile(src)) {
        Ok(_) => None,
        Err(payload) => Some(
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| {
                    payload
                        .downcast_ref::<&str>()
                        .map(std::string::ToString::to_string)
                })
                .unwrap_or_else(|| "<non-string panic>".to_string()),
        ),
    }
}

fn with_hybrid_admits_nlr<T>(f: impl FnOnce() -> T) -> T {
    fault::set(Fault::HybridAdmitsNlr, true);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    fault::set(Fault::HybridAdmitsNlr, false);
    result.unwrap_or_else(|payload| std::panic::resume_unwind(payload))
}

/// Every shape is verifier-clean, compiles through `erlc`, and never lowers a
/// hybrid loop (no `_NField` parameter) around its `^`.
#[test]
fn nlr_in_a_field_writing_loop_corpus_is_clean_in_every_mode() {
    let mut failures = Vec::new();
    for (name, src) in corpus() {
        match std::panic::catch_unwind(|| {
            let code = compile(&src);
            assert_compiles_through_erlc("test", &code);
            code
        }) {
            Ok(code) => {
                if code.contains("NField") {
                    failures.push(format!("{name}: lowered hybrid around a `^`:\n{src}"));
                }
            }
            Err(_) => failures.push(format!("{name}: fails verified codegen:\n{src}")),
        }
    }
    assert!(
        failures.is_empty(),
        "{} case(s) failed:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// With mode selection's `^` exclusion off, every shape that lowers hybrid
/// reports `NlrThrowInHybridLoop`, the repro among them: every shape of every
/// letrec loop, whatever the `^`'s position or the write's order; nothing
/// reports anything else.
#[test]
fn hybrid_checkout_reports_nlr_in_a_hybrid_loop() {
    let mut reported: Vec<String> = Vec::new();
    for (name, src) in corpus() {
        let finding = with_hybrid_admits_nlr(|| report(&src));
        let Some(finding) = finding else {
            // Clean under the fault too: it must not have lowered hybrid.
            let code = with_hybrid_admits_nlr(|| compile(&src));
            assert!(
                !code.contains("NField"),
                "{name}: lowered hybrid around a `^` without a finding:\n{src}"
            );
            continue;
        };
        assert!(
            finding.contains("NlrThrowInHybridLoop"),
            "{name}: expected NlrThrowInHybridLoop, got:\n{finding}\n{src}"
        );
        reported.push(name);
    }
    assert!(
        reported.iter().any(|name| name == REPRO),
        "the repro must report under the fault (reported: {reported:?})"
    );
    // Every letrec shape (the first `LETREC_LOOPS` loops, and both condition
    // cases) lowers hybrid under the fault and reports; the folds never do.
    let expected = LETREC_LOOPS * RETURNS.len() * ORDERS.len() + CONDITION_RETURNS.len();
    assert_eq!(
        reported.len(),
        expected,
        "every letrec shape must report under the fault (reported: {reported:?})"
    );
}
