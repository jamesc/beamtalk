// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2d (BT-3795): the REPL assembler's use of the
//! local-threading producer, through every public REPL-codegen entry point.
//!
//! A loop or fold at an operand site of a REPL input (`repl_operand_sites`)
//! is lowered by the generator's producer (`repl_statement_with_producers`),
//! whose prelude each assembler path (single and multi-statement, eval and
//! trace, assignment and plain statement) must emit ahead of its own
//! `let Result = <value> in`. The generator's agreement with the Phase 0
//! check is `local_producer_agreement_repl.rs` in `beamtalk-codegen`; this
//! test pins that the assembler emits what the generator returns: one
//! carrier per producer site, and a module `erlc` accepts (so a `State`
//! version the prelude steps is in scope where the assembler reads it).

use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{
    conditional_arm_blocks, construct_outer_local_writes, local_threading_construct,
    producer_body_block, repl_operand_sites,
};
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_repl::codegen::{generate_repl_expressions, generate_repl_expressions_traced};
use std::collections::{HashMap, HashSet};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// REPL inputs with producers, and statements around them: operand sites of
/// a plain statement, of an assignment (single-statement inputs go through
/// the eval module body; the value of an `x := V` is compiled by the REPL's
/// own path), and of the intermediate and last statements of a
/// multi-statement input.
const INPUTS: &[&str] = &[
    "(#(1, 2) collect: [:x | t := t + x]) size",
    "r := (#(1, 2) reject: [:x | t := t + 1. false]) size",
    "r := t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])",
    "#[1 to: 3 do: [:i | t := t + i], t]",
    "r := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]",
    "(#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]) + 1",
    "t := 0\nr := (#(1, 2) collect: [:x | t := t + x]) size\n#[r, t]",
    "(#(1, 2) collect: [:x | t := t + x]) size\nt",
    "r := t + (#(5) inject: 0 into: [:a :x | t := t + x. a + x])\nr",
    "r := (#(1, 2) collect: [:x | t := t + x]) size\nr := r + (#(1) count: [:x | t := t + 1. true]) size\nr",
    "#(1, 2) do: [:x | t := t + x]\n(#(1) collect: [:x | u := u + x]) size",
    // A fold that is the whole value of an assignment, with a producer in its
    // receiver: the value is itself a `{Value, Bindings}` tuple the assembler
    // unpacks, as an intermediate and as the last statement.
    "t := 0\nu := 0\nr := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]\n#[r, t, u]",
    "t := 0\nu := 0\nr := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]\nr := r + 1\nr",
    "t := 0\nu := 0\nr := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]",
    // ADR 0131 Phase 3 (BT-3738): conditionals at an operand site, including
    // the nil-testing family, as an operand, in an assignment and in a
    // multi-statement input.
    "((s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]) + 1",
    "r := t + (s ifNil: [t := t + 1. 2])",
    "#[s ifNotNil: [:v | t := t + v. 1], t]",
    "((s < 3) and: [t := t + 1. true]) printString size",
    "t := 0\nr := ((s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]) + 1\n#[r, t]",
    "r := ((s < 3) or: [t := t + 1. true]) printString size\nr := r + ((s < 3) ifTrue: [t := t + 1. 2]) size\nr",
    // A conditional that is the statement or an assignment's whole value keeps
    // the assembler's lowering, the nil-testing family included.
    "r := (s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]\nr",
    "nil ifNil: [t := t + 1]",
    "r := nil ifNil: [t := t + 1. 1]\nr",
    // No producer: the statement keeps the assembler's lowering.
    "#(1, 2) do: [:x | t := t + x]",
    "r := #(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    "t := 0\nr := #(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]\nr",
];

fn statements(src: &str) -> Vec<Expression> {
    let (module, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "{src}: {diags:?}");
    module
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect()
}

/// How many producers the core rule lowers in `expressions`: the loops and
/// folds at `repl_operand_sites` that write a binding.
fn expected_producers(expressions: &[Expression]) -> usize {
    expressions
        .iter()
        .flat_map(repl_operand_sites)
        .filter(|site| {
            (producer_body_block(site).is_some() || conditional_arm_blocks(site).is_some())
                && local_threading_construct(site)
                    .is_some_and(|c| !construct_outer_local_writes(&c, &|_| true).is_empty())
        })
        .count()
}

/// The distinct construct-tuple carrier variables of `code`.
fn carriers(code: &str) -> usize {
    code.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .filter(|word| word.starts_with("_ThreadedResult"))
        .collect::<HashSet<_>>()
        .len()
}

/// Compiles `code` through `erlc +from_core`; skipped when `erlc` is absent.
fn assert_compiles(name: &str, code: &str) {
    // The directory is unique per call, not per `name`: the per-family tests
    // (BT-3888) reuse a name across families, and under the `cargo test`
    // fallback (no nextest) they share one process, so a name-keyed
    // directory could be removed by one test under another's `erlc` run.
    static CALLS: AtomicUsize = AtomicUsize::new(0);
    let dir = beamtalk_core::test_helpers::unique_temp_dir(&format!(
        "bt3795_{}_{name}",
        CALLS.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let file = dir.join("repl_producer.core");
    std::fs::write(&file, code).expect("write core");
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(&dir)
        .arg(&file)
        .output();
    let result = match output {
        Ok(out) if out.status.success() => Ok(()),
        Ok(out) => Err(format!(
            "erlc failed:\n{}\n{}\n\nGenerated Core Erlang:\n{code}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(format!("failed to invoke erlc: {e}")),
    };
    let _ = std::fs::remove_dir_all(&dir);
    if let Err(message) = result {
        panic!("{message}");
    }
}

#[test]
fn the_repl_assembler_emits_one_carrier_per_producer_in_every_entry_point() {
    let mut producers = 0;
    for (i, src) in INPUTS.iter().enumerate() {
        let expressions = statements(src);
        let expected = expected_producers(&expressions);
        let outputs: [(&str, String); 2] = [
            (
                "eval",
                generate_repl_expressions(&expressions, "repl_producer")
                    .unwrap_or_else(|e| panic!("{src}: {e:?}")),
            ),
            (
                "trace",
                generate_repl_expressions_traced(
                    &expressions,
                    src,
                    "repl_producer",
                    HashMap::new(),
                )
                .unwrap_or_else(|e| panic!("{src}: {e:?}")),
            ),
        ];
        for (entry, code) in outputs {
            assert_eq!(
                carriers(&code),
                expected,
                "{entry}: {expected} producer carriers expected\n{src}\n{code}"
            );
            assert_compiles(&format!("{i}_{entry}"), &code);
        }
        producers += expected;
    }
    assert!(producers >= 10, "only {producers} producers compared");
}

/// A fold that is the whole value of an assignment in a multi-statement input
/// compiles to a `{Value, Bindings}` tuple; with a producer in its receiver the
/// assembler must unpack it (the value is element 1, the bindings element 2)
/// before `put`ting the binding, in every entry point. Otherwise `r` is bound
/// to the tuple and the fold's writes are lost.
#[test]
fn a_whole_value_fold_after_a_producer_is_unpacked_in_every_multi_statement_path() {
    let fold =
        "r := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]";
    for src in [
        format!("t := 0\nu := 0\n{fold}\n#[r, t, u]"),
        format!("t := 0\nu := 0\n{fold}\nr := r + 1\nr"),
        format!("t := 0\nu := 0\n{fold}"),
    ] {
        let expressions = statements(&src);
        let outputs = [
            generate_repl_expressions(&expressions, "repl_producer").expect("eval"),
            generate_repl_expressions_traced(&expressions, &src, "repl_producer", HashMap::new())
                .expect("trace"),
        ];
        for code in outputs {
            assert!(
                code.contains("_AssignTuple"),
                "the whole-value fold's tuple is not unpacked\n{src}\n{code}"
            );
        }
    }
}

/// Loops, folds and other state-threading constructs, each writing a REPL
/// binding from its body.
const CONSTRUCTS: &[&str] = &[
    "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    "#(1, 2) collect: [:x | t := t + x]",
    "#(1, 2, 3) select: [:x | t := t + 1. x > 1]",
    "#(1, 2, 3) reject: [:x | t := t + 1. x > 1]",
    "#(1, 2, 3) detect: [:x | t := t + 1. x > 1]",
    "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 7. 0]",
    "#(1, 2) count: [:x | t := t + 1. true]",
    "#(1, 2) do: [:x | t := t + x]",
    "1 to: 3 do: [:i | t := t + i]",
    "3 timesRepeat: [t := t + 1]",
    "[t < 9] whileTrue: [t := t + 3]",
    // Other state-threading constructs that set `repl_loop_mutated`: a
    // mutating conditional, an inline block `value`, and (reaching codegen
    // only without the analyser, which rejects them, BT-3743) `on:do:` and
    // `ensure:`.
    "true ifTrue: [t := t + 1. 5] ifFalse: [6]",
    "[t := t + 1. 5] value",
    "[:x | t := t + x. x] value: 3",
    "[t := t + 1. 5] on: Error do: [:e | 0]",
    "[t := t + 1. 5] ensure: [u := u + 10]",
];

/// A receiver whose own producer runs before the construct (the whole-value
/// construct then has a producer in its receiver); `None` for no producer.
const RECEIVERS: &[Option<&str>] = &[None, Some("(#(1, 2) collect: [:y | u := u + y])")];

/// `template` with its `#(1, 2)` receiver replaced by `receiver`, if any.
fn construct(template: &str, receiver: Option<&str>) -> String {
    match receiver {
        Some(r) if template.starts_with("#(1, 2) ") => template.replacen("#(1, 2)", r, 1),
        _ => template.to_string(),
    }
}

/// Both generated modules (eval, trace) of `src`, erlc-checked when `compile`.
fn both_outputs(name: &str, src: &str, compile: bool) -> [(&'static str, String); 2] {
    let expressions = statements(src);
    let outputs = [
        (
            "eval",
            generate_repl_expressions(&expressions, "repl_producer")
                .unwrap_or_else(|e| panic!("{src}: {e:?}")),
        ),
        (
            "trace",
            generate_repl_expressions_traced(&expressions, src, "repl_producer", HashMap::new())
                .unwrap_or_else(|e| panic!("{src}: {e:?}")),
        ),
    ];
    if compile {
        for (entry, code) in &outputs {
            assert_compiles(&format!("{name}_{entry}"), code);
        }
    }
    outputs
}

/// BT-3800 and the ADR 0131 §2 REPL invariant: a construct the REPL threads
/// through its bindings map (a loop or fold whose body writes a binding)
/// always compiles to a `{Value, Bindings}` tuple, and a tuple that is the
/// whole value of an assignment in a multi-statement input is always
/// unpacked -- as an intermediate and as the last statement, with and without
/// a producer in the construct's receiver, in the eval and the trace module.
/// A construct that does not set `repl_loop_mutated` shows up here as a
/// missing `_AssignTuple` (and, at a producer site, a missing carrier). Over
/// one family of [`CONSTRUCTS`]: the whole proof is the per-family tests
/// below.
fn every_threaded_construct_is_a_tuple_and_a_whole_value_one_is_unpacked(constructs: &[&str]) {
    let mut compared = 0;
    for (c, template) in constructs.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            let value = construct(template, *receiver);
            for (k, src) in [
                format!("t := 0\nu := 0\nr := {value}\n#[r, t, u]"),
                format!("t := 0\nu := 0\nr := {value}\nr := r\nr"),
                format!("t := 0\nu := 0\nr := {value}"),
            ]
            .into_iter()
            .enumerate()
            {
                let expected = expected_producers(&statements(&src));
                for (entry, code) in both_outputs(&format!("fold_{c}_{r}_{k}"), &src, k == 0) {
                    assert!(
                        code.contains("_AssignTuple"),
                        "{entry}: the whole-value construct's tuple is not unpacked\n{src}\n{code}"
                    );
                    assert_eq!(
                        carriers(&code),
                        expected,
                        "{entry}: one carrier per producer expected\n{src}\n{code}"
                    );
                    compared += 1;
                }
            }
            // The same construct as an operand (where it is a producer site:
            // `do:` and its like are not, and the Phase 0 check rejects them
            // there), and as a statement of its own: not an assignment's whole
            // value, so nothing to unpack there.
            for (k, src) in [
                format!("t := 0\nu := 0\nr := ({value}) size\nr"),
                format!("t := 0\nu := 0\n{value}\nt"),
            ]
            .into_iter()
            .enumerate()
            {
                if k == 0 && producer_body_block(&statements(&value)[0]).is_none() {
                    continue;
                }
                for (entry, code) in both_outputs(&format!("operand_{c}_{r}_{k}"), &src, false) {
                    assert!(
                        !code.contains("_AssignTuple"),
                        "{entry}: not an assignment's whole value\n{src}\n{code}"
                    );
                }
            }
        }
    }
    // Every construct x receiver x whole-value input x entry point is compared:
    // nothing in that loop is skipped.
    assert_eq!(
        compared,
        constructs.len() * RECEIVERS.len() * 3 * 2,
        "whole-value outputs compared in {constructs:?}"
    );
}

/// The families of [`CONSTRUCTS`], as index ranges, one test each below
/// (`per_family_tests!`, BT-3888): the folds; the searches, `count:` and
/// `do:`; the letrec loops; the other state-threading constructs.
const CONSTRUCT_FAMILIES: &[std::ops::Range<usize>] = &[0..4, 4..8, 8..11, 11..16];

beamtalk_core::per_family_tests! {
    list = CONSTRUCTS,
    families = CONSTRUCT_FAMILIES,
    check = every_threaded_construct_is_a_tuple_and_a_whole_value_one_is_unpacked,
    partition = the_threaded_construct_tests_cover_every_construct,
    every_threaded_fold_is_a_tuple_and_a_whole_value_one_is_unpacked => 0,
    every_threaded_search_count_or_do_is_a_tuple_and_a_whole_value_one_is_unpacked => 1,
    every_threaded_letrec_loop_is_a_tuple_and_a_whole_value_one_is_unpacked => 2,
    every_other_threaded_construct_is_a_tuple_and_a_whole_value_one_is_unpacked => 3,
}

/// BT-3823: the REPL arms whose sites hold a producer, for the arm-site half of
/// the invariant above, with the number of carriers one producer in the arm
/// compiles to: one, except in an `ensure:` cleanup, which is lowered twice
/// (after the protected body succeeds, and before re-raising), plus one for a
/// construct around the arm that is itself a producer.
const REPL_ARMS: &[(&str, usize)] = &[
    ("r := (s < 3) ifTrue: [{p}. 7] ifFalse: [0]", 1),
    ("r := (s < 3) and: [{p}. true]", 1),
    ("r := s ifNotNil: [:v | {p}. v]", 1),
    ("r := [{p}. 7] on: Error do: [:e | 3]", 1),
    ("r := [7] ensure: [{p}]", 2),
    ("r := [1 / 0] on: Error do: [:e | {p}. 7]", 1),
    // An evaluated block is a REPL arm too, of either arity.
    ("r := [:z | {p}. 7] value: 1", 1),
    ("r := [{p}. 7] value", 1),
    // The `tryDo:` is itself a producer at an operand site of the input.
    ("r := (Result tryDo: [{p}. 7]) unwrap", 2),
];

/// The constructs at a REPL arm site (BT-3823): every family of
/// [`CONSTRUCTS`] plus `Result tryDo:`.
fn arm_site_constructs() -> Vec<&'static str> {
    CONSTRUCTS
        .iter()
        .copied()
        .chain(["Result tryDo: [t := t + 1. 5]"])
        .collect()
}

/// The families of [`arm_site_constructs`], one construct each, one test
/// each below (`per_family_tests!`, BT-3888): every construct is 9 arms x 3
/// statements x 2 entry points, each an `erlc` run, so one construct is the
/// unit nextest spreads over cores.
const ARM_SITE_CONSTRUCT_FAMILIES: &[std::ops::Range<usize>] = &[
    0..1,
    1..2,
    2..3,
    3..4,
    4..5,
    5..6,
    6..7,
    7..8,
    8..9,
    9..10,
    10..11,
    11..12,
    12..13,
    13..14,
    14..15,
    15..16,
    16..17,
];

/// BT-3823: every construct at a REPL arm site (the statement itself, an
/// operand and an assignment's value) compiles to a `{Value, Bindings}`
/// carrier the arm adopts: a family that reached the producer without setting
/// `repl_loop_mutated` would leave no carrier and drop its write. Every family
/// of [`CONSTRUCTS`] plus `tryDo:`, in every arm kind, in the eval and the
/// trace module, `erlc`-checked. Over one family of [`arm_site_constructs`]:
/// the whole proof is the per-family tests below.
fn every_construct_at_a_repl_arm_site_produces_its_carrier(constructs: &[&str]) {
    let mut compared = 0;
    for (c, value) in constructs.iter().enumerate() {
        for (a, (arm, expected)) in REPL_ARMS.iter().enumerate() {
            for (k, stmt) in [
                value.to_string(),
                format!("({value}) printString size"),
                format!("u := {value}"),
            ]
            .into_iter()
            .enumerate()
            {
                let src = format!(
                    "t := 0\nu := 0\ns := 1\n{}\n#[t, u]",
                    arm.replace("{p}", &stmt)
                );
                for (entry, code) in both_outputs(&format!("arm_{c}_{a}_{k}"), &src, true) {
                    assert_eq!(
                        carriers(&code),
                        *expected,
                        "{entry}: the arm-site producer's carrier\n{src}\n{code}"
                    );
                    compared += 1;
                }
            }
        }
    }
    // Every construct x arm x statement x entry point is compared: nothing in
    // that loop is skipped.
    assert_eq!(
        compared,
        constructs.len() * REPL_ARMS.len() * 3 * 2,
        "arm-site outputs compared in {constructs:?}"
    );
}

beamtalk_core::per_family_tests! {
    list = arm_site_constructs(),
    families = ARM_SITE_CONSTRUCT_FAMILIES,
    check = every_construct_at_a_repl_arm_site_produces_its_carrier,
    partition = the_repl_arm_site_tests_cover_every_construct,
    every_inject_into_at_a_repl_arm_site_produces_its_carrier => 0,
    every_collect_at_a_repl_arm_site_produces_its_carrier => 1,
    every_select_at_a_repl_arm_site_produces_its_carrier => 2,
    every_reject_at_a_repl_arm_site_produces_its_carrier => 3,
    every_detect_at_a_repl_arm_site_produces_its_carrier => 4,
    every_detect_if_none_at_a_repl_arm_site_produces_its_carrier => 5,
    every_count_at_a_repl_arm_site_produces_its_carrier => 6,
    every_do_at_a_repl_arm_site_produces_its_carrier => 7,
    every_to_do_at_a_repl_arm_site_produces_its_carrier => 8,
    every_times_repeat_at_a_repl_arm_site_produces_its_carrier => 9,
    every_while_true_at_a_repl_arm_site_produces_its_carrier => 10,
    every_if_true_if_false_at_a_repl_arm_site_produces_its_carrier => 11,
    every_block_value_at_a_repl_arm_site_produces_its_carrier => 12,
    every_block_value_with_argument_at_a_repl_arm_site_produces_its_carrier => 13,
    every_on_do_at_a_repl_arm_site_produces_its_carrier => 14,
    every_ensure_at_a_repl_arm_site_produces_its_carrier => 15,
    every_try_do_at_a_repl_arm_site_produces_its_carrier => 16,
}

/// A fold that writes no binding is a plain value: no tuple, nothing to
/// unpack, and the output of an input that answered correctly is unchanged.
#[test]
fn a_fold_that_writes_no_binding_is_not_unpacked() {
    for src in [
        "t := 0\nr := #(1, 2) inject: 0 into: [:a :x | a + x]\n#[r, t]",
        "t := 0\nr := #(1, 2) collect: [:x | x + 1]\nr",
        "t := 0\nr := 5\ns := r + 1\n#[r, s, t]",
        "t := 0\nr := true ifTrue: [5] ifFalse: [6]\nr",
        "t := 0\nr := [5] value\n#[r, t]",
    ] {
        for (entry, code) in both_outputs("plain", src, true) {
            assert!(
                !code.contains("_AssignTuple"),
                "{entry}: a plain value is not unpacked\n{src}\n{code}"
            );
        }
    }
}

/// ADR 0131 Phase 4 (BT-3750): `Result tryDo:` compiles to its
/// `{Value, Bindings}` tuple (`beamtalk_result:'tryDo:'/2`) only where the
/// REPL unpacks one: a statement, an assignment's value, a producer operand
/// site, or (BT-3823) a producer site of a REPL arm or fold body, whose
/// producer unpacks it the same way. This codegen is reached with the Phase 0
/// check bypassed, so it must not depend on that check keeping a `tryDo:` out
/// of a closure or an argument. (A `tryDo:` in a REPL loop body is the Phase 0
/// error the other REPL exception constructs have, BT-3876.)
#[test]
fn a_repl_try_do_is_a_construct_tuple_only_where_the_root_unpacks_it() {
    let tuple_call = "call 'beamtalk_result':'tryDo:'(fun (StateAcc)";
    for src in [
        "Result tryDo: [t := t + 1. 1]",
        "r := Result tryDo: [t := t + 1. 1]",
        "r := Result tryDo: [t := t + 1. 1]\nr",
        "(Result tryDo: [t := t + 1. 1]) isOk",
        "Transcript show: (Result tryDo: [t := t + 1. 1]) printString",
        "true ifTrue: [Result tryDo: [t := t + 1. 1]]",
        "r := [(Result tryDo: [t := t + 1. 1]) isOk] on: Error do: [:e | false]",
        // A statement of a REPL fold body, whose `StateAcc` is the bindings map
        // too: the producer adopts element 2 (the Phase 0 check still rejects
        // the input, BT-3876).
        "#(1) do: [:x | Result tryDo: [t := t + 1. 1]]",
        // An evaluated block is a REPL arm (BT-3823); its arity matches the
        // send's, since a block of another arity is applied, not inlined
        // (BT-3881).
        "#(1) collect: [:x | [:y | Result tryDo: [t := t + y. 1]] value: x]",
    ] {
        for (entry, code) in both_outputs("try_do_root", src, true) {
            assert!(code.contains(tuple_call), "{entry}: {src}\n{code}");
        }
    }
    for src in [
        "blk := [Result tryDo: [t := t + 1. 1]]",
        "#(1) collect: [:x | [:y | Result tryDo: [t := t + y. y]]]",
    ] {
        for (entry, code) in both_outputs("try_do_nested", src, false) {
            assert!(!code.contains(tuple_call), "{entry}: {src}\n{code}");
        }
    }
    // BT-3881: an evaluated block sent a `value` selector of another arity is
    // not an arm: it is applied to a stand-in fun of the block's own arity
    // (`generate_block_arity_mismatch`), so its `tryDo:` is never compiled.
    for src in [
        "#(1) collect: [:x | [Result tryDo: [t := t + 1. 1]] value: x]",
        "[:y | Result tryDo: [t := t + y. 1]] value",
    ] {
        for (entry, code) in both_outputs("try_do_arity", src, true) {
            assert!(!code.contains(tuple_call), "{entry}: {src}\n{code}");
            assert!(code.contains("apply _Arity"), "{entry}: {src}\n{code}");
        }
    }
}
