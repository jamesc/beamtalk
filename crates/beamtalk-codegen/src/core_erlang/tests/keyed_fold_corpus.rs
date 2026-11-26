// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5097: the keyed fold of a writing `collect:` / `select:` / `reject:`
//! (`KeyedFoldCall`). A non-list receiver (a `Dictionary`) is folded over its
//! `{K, V}` pairs with the fold's own lambda by `beamtalk_collection:keyed_foldl/5`,
//! which reads the accumulator's result list through a path and relies on the lambda
//! adding at most one element per call. Both are data the verifier checks, not a layout
//! the runtime assumes:
//!
//! - `KeyedFoldNoResultList` / `KeyedFoldPathMismatch` over the call (the kind's declared
//!   slot, under element 1 of the `{Acc, Family..}` wrapper exactly when the fold has family
//!   slots);
//! - `FoldResultListUndeclared` over the lambda body (its last accumulator declares the
//!   result list its `BodyKind` says it extends);
//! - BT-5117: `FoldResultListConsMismatch` / `FoldResultListOpaque` over the accumulator
//!   itself (every leaf arm conses into the result-list slot as often as the kind says).
//!
//! Proved over a generated corpus, not the repro: every selector x writing-block shape x
//! receiver x method kind compiles with a silent verifier, the keyed call is in the output, and
//! with each fault injected the verifier reports it on every program it applies to.

use super::{codegen, verifier_report};
use crate::core_erlang::fault::Fault;

const SELECTORS: &[&str] = &["collect:", "select:", "reject:"];

/// The block body for a selector: every shape writes the outer local `t`, some also a field.
/// `{field}` is `self.m` (value type, actor) or empty for a class method.
fn blocks(selector: &str) -> Vec<(&'static str, String)> {
    let tail = if selector == "collect:" {
        "e * 2"
    } else {
        "e > 1"
    };
    let mut out = vec![
        (
            "local write",
            format!("[:e |\n        t := t + 1\n        {tail}\n      ]"),
        ),
        (
            "conditional write",
            format!("[:e |\n        e > 1 ifTrue: [t := t + 1]\n        {tail}\n      ]"),
        ),
        (
            "nested writing fold",
            format!("[:e |\n        #(1, 2) do: [:j | t := t + j]\n        {tail}\n      ]"),
        ),
    ];
    out.push((
        "tail write",
        if selector == "collect:" {
            "[:e | t := t + e]".to_string()
        } else {
            "[:e |\n        t := t + e\n        t > 2\n      ]".to_string()
        },
    ));
    out
}

const RECEIVERS: &[(&str, &str)] = &[
    ("dictionary literal", "#{#a => 1, #b => 2}"),
    ("list literal", "#(1, 2, 3)"),
    ("parameter", "items"),
];

/// `(name, program, has_families)`. `has_families`: a value-type `collect:` that also writes a
/// field carries `Self` through the accumulator, so its keyed call walks through the wrapper.
fn programs() -> Vec<(String, String, bool)> {
    let mut out = Vec::new();
    for selector in SELECTORS {
        for (block_name, block) in blocks(selector) {
            for (recv_name, recv) in RECEIVERS {
                let body = format!("    t := 0\n    r := {recv} {selector} {block}\n    #[t, r]\n");
                out.push((
                    format!("class / {selector} / {block_name} / {recv_name}"),
                    format!("Object subclass: KeyedFoldClass\n  class go: items =>\n{body}"),
                    false,
                ));
                out.push((
                    format!("actor / {selector} / {block_name} / {recv_name}"),
                    format!(
                        "Actor subclass: KeyedFoldActor\n  state: m = 0\n\n  go: items =>\n{body}"
                    ),
                    false,
                ));
                out.push((
                    format!("value / {selector} / {block_name} / {recv_name}"),
                    format!(
                        "Value subclass: KeyedFoldValue\n  state: m = 0\n\n  go: items =>\n{body}"
                    ),
                    false,
                ));
            }
        }
        // A value-type fold that writes a field: the `Self` family slot. Only `collect:`
        // threads `Self` (`SELF_THREADING_FOLDS`); `select:` / `reject:` reject the write.
        let field_block = "[:e |\n      self.m := self.m + e\n      e * 2\n    ]";
        for (recv_name, recv) in RECEIVERS {
            if *selector != "collect:" {
                continue;
            }
            out.push((
                format!("value field / {selector} / {recv_name}"),
                format!(
                    "TestCase subclass: KeyedFoldFieldProbe\n  field: m = 0\n\n  go: items =>\n    r := {recv} {selector} {field_block}\n    r\n"
                ),
                true,
            ));
        }
    }
    out
}

#[test]
fn every_writing_collect_select_reject_compiles_with_a_silent_verifier_and_a_keyed_call() {
    let programs = programs();
    let failures: Vec<String> = programs
        .iter()
        .filter_map(|(name, src, _)| {
            if let Some(report) = verifier_report(src, None) {
                return Some(format!("{name}: {report}"));
            }
            let code = codegen(src);
            (!code.contains("'beamtalk_collection':'keyed_foldl'("))
                .then(|| format!("{name}: no keyed_foldl call\n{code}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
    assert!(
        programs.len() >= 100,
        "only {} programs: the corpus has thinned out",
        programs.len()
    );
}

#[test]
fn a_result_list_the_accumulator_does_not_declare_is_reported_on_every_program() {
    let mut unreported = Vec::new();
    for (name, src, _) in programs() {
        match verifier_report(&src, Some(Fault::FoldAccumulatorOmitsResultList)) {
            Some(report) if report.contains("FoldResultListUndeclared") => {}
            other => unreported.push(format!("{name}: {other:?}")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n"));
}

#[test]
fn a_filter_that_conses_twice_is_reported_on_every_select_and_reject() {
    let mut wrong = Vec::new();
    for (name, src, _) in programs() {
        let is_filter = name.contains("select:") || name.contains("reject:");
        let report = verifier_report(&src, Some(Fault::FilterConsTwice));
        match (is_filter, report) {
            (true, Some(r)) if r.contains("FoldResultListConsMismatch") => {}
            (false, None) => {}
            (expected, other) => wrong.push(format!("{name}: filter {expected}: {other:?}")),
        }
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}

/// BT-5190: every `select:` / `reject:` program reports `fault`'s `error` and no other program does.
fn assert_filter_fault_reported(fault: Fault, error: &str) {
    let mut wrong = Vec::new();
    for (name, src, _) in programs() {
        let is_filter = name.contains("select:") || name.contains("reject:");
        let report = verifier_report(&src, Some(fault));
        match (is_filter, report) {
            (true, Some(r)) if r.contains(error) => {}
            (false, None) => {}
            (expected, other) => wrong.push(format!("{name}: filter {expected}: {other:?}")),
        }
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}

#[test]
fn a_filter_that_conses_nothing_is_reported_on_every_select_and_reject() {
    assert_filter_fault_reported(Fault::FilterConsNothing, "FoldResultListUndeclared");
}

#[test]
fn a_filter_that_conses_in_both_arms_is_reported_on_every_select_and_reject() {
    assert_filter_fault_reported(Fault::FilterConsBothArms, "FoldResultListUndeclared");
}

#[test]
fn a_filter_with_swapped_arms_is_reported_on_every_select_and_reject() {
    assert_filter_fault_reported(Fault::FilterArmsSwapped, "FoldResultListUndeclared");
}

#[test]
fn a_filter_that_conses_the_wrong_item_is_reported_on_every_select_and_reject() {
    assert_filter_fault_reported(Fault::FilterConsWrongItem, "FoldResultListWrongItem");
}

#[test]
fn a_path_that_ignores_the_family_wrapper_is_reported_exactly_where_there_is_one() {
    let mut wrong = Vec::new();
    for (name, src, has_families) in programs() {
        let report = verifier_report(&src, Some(Fault::KeyedFoldPathIgnoresFamilies));
        match (has_families, report) {
            (true, Some(r)) if r.contains("KeyedFoldPathMismatch") => {}
            (false, None) => {}
            (expected, other) => wrong.push(format!("{name}: families {expected}: {other:?}")),
        }
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}

#[test]
fn the_result_list_slot_is_the_head_of_the_filter_and_collect_accumulators() {
    use crate::core_erlang::threaded_ir::FOLD_RESULT_LIST_SLOT;
    // The lambda binds `AccList` from `element(FOLD_RESULT_LIST_SLOT, Acc)`: the tuple
    // accumulators these kinds build lead with it.
    assert_eq!(FOLD_RESULT_LIST_SLOT, 1);
    for selector in SELECTORS {
        let src = format!(
            "Object subclass: Slot\n  class go: items =>\n    t := 0\n    items {selector} [:e |\n      t := t + 1\n      e > 1\n    ]\n    t\n"
        );
        let code = codegen(&src);
        let squashed: String = code.split_whitespace().collect::<Vec<_>>().join(" ");
        assert!(
            squashed.contains("let AccList = call 'erlang':'element'(1, "),
            "{selector}: the lambda no longer reads AccList from element 1:\n{code}"
        );
    }
}
