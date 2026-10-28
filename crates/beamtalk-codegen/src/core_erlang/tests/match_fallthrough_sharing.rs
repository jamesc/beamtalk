// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4141: a match chain's fallthrough is bound once (a `letrec` label per
//! multi-site arm) and applied at every miss site, so emitted Core grows
//! linearly in the number of arms. Before, every Array/Type/binary arm
//! embedded a full copy of the later arms at each miss site, and N such arms
//! emitted O(k^N) copies of the trailing arms.
//!
//! Each arm body is a distinct integer literal, so counting it in the output
//! counts how many copies of that arm the chain emitted.

use super::{assert_compiles_through_erlc, codegen};

/// Occurrences of the integer literal `n` as a whole token in `out`.
fn copies(out: &str, n: u32) -> usize {
    let needle = n.to_string();
    out.match_indices(&needle)
        .filter(|(at, _)| {
            let before = out[..*at].chars().next_back();
            let after = out[at + needle.len()..].chars().next();
            !before.is_some_and(|c| c.is_ascii_alphanumeric())
                && !after.is_some_and(|c| c.is_ascii_alphanumeric())
        })
        .count()
}

fn method(arms: &[String]) -> String {
    format!(
        "Object subclass: Foo\n  run: x =>\n    x match: [\n      {}\n    ]\n",
        arms.join(";\n      ")
    )
}

fn assert_each_arm_once(src: &str, bodies: impl IntoIterator<Item = u32>) {
    let out = codegen(src);
    for body in bodies {
        assert_eq!(
            copies(&out, body),
            1,
            "arm body {body} must be emitted exactly once.\nSource:\n{src}\nCore:\n{out}"
        );
    }
    assert_compiles_through_erlc("test", &out);
}

/// The #4609 review case: tuple-wrapped binary arms (HTTP-method dispatch).
#[test]
fn tuple_wrapped_binary_arms_emit_each_arm_once() {
    let n = 12;
    let mut arms: Vec<String> = (1..=n)
        .map(|i| format!("{{#ok, <<{i}, r/binary>>}} -> {}", 7_000_000 + i))
        .collect();
    arms.push("_ -> 7999999".to_string());
    assert_each_arm_once(
        &method(&arms),
        (1..=n).map(|i| 7_000_000 + i).chain([7_999_999]),
    );
}

/// The issue's acceptance case: a 3-chunk segmented binary arm over N
/// trailing arms.
#[test]
fn segmented_binary_arm_emits_trailing_arms_once() {
    let n = 8;
    let mut arms = vec!["<<k:8, a:k/binary, m:8, b:m/binary, rest/binary>> -> 7100000".to_string()];
    arms.extend((1..=n).map(|i| format!("<<{i}, r/binary>> -> {}", 7_100_000 + i)));
    arms.push("_ -> 7199999".to_string());
    assert_each_arm_once(
        &method(&arms),
        (0..=n).map(|i| 7_100_000 + i).chain([7_199_999]),
    );
}

/// Every Type-test shape, including `Binary` and `Boolean`, which also
/// emitted their own arm body twice.
#[test]
fn type_arms_emit_each_arm_once() {
    let classes = [
        "Binary",
        "Boolean",
        "Integer",
        "String",
        "List",
        "Float",
        "Symbol",
        "Tuple",
        "Dictionary",
        "Block",
        "True",
        "Nil",
    ];
    let mut arms: Vec<String> = classes
        .iter()
        .zip(1..)
        .map(|(class, i)| format!("v :: {class} -> {}", 7_200_000 + i))
        .collect();
    arms.push("_ -> 7299999".to_string());
    let bodies = (1..=u32::try_from(classes.len()).unwrap()).map(|i| 7_200_000 + i);
    assert_each_arm_once(&method(&arms), bodies.chain([7_299_999]));
}

/// Array arms, and a guard (whose failure is one more miss site).
#[test]
fn array_and_guarded_arms_emit_each_arm_once() {
    let arms = [
        "{a, b} when: [a > b] -> 7300001".to_string(),
        "{a, b} -> 7300002".to_string(),
        "n :: Integer when: [n > 5] -> 7300003".to_string(),
        "n :: Integer -> 7300004".to_string(),
        "_ -> 7399999".to_string(),
    ];
    assert_each_arm_once(
        &method(&arms),
        [7_300_001, 7_300_002, 7_300_003, 7_300_004, 7_399_999],
    );
}

/// The shared label must carry threaded state: a local-threading producer
/// `match:` (BT-3947) and an actor `State`-threading one, over multi-site
/// arms, still compile (the verifier panics in `codegen` otherwise).
#[test]
fn threaded_matches_over_shared_fallthrough_compile() {
    let arms = "[n :: Integer -> t := 1; {#ok, <<1, r/binary>>} -> t := 2; b :: Binary -> t := 3; _ -> t := 4]";
    let actor = format!(
        "Actor subclass: Ctr\n  state: n = 0\n\n  run: x =>\n    t := 0\n    x match: {arms}\n    t\n\n  write: x =>\n    x match: [n :: Integer -> self.n := n; b :: Binary -> self.n := 2; _ -> self.n := 3]\n    self.n\n"
    );
    let plain =
        format!("Object subclass: Foo\n  run: x =>\n    t := 0\n    x match: {arms}\n    t\n");
    for src in [actor, plain] {
        let out = codegen(&src);
        assert!(
            out.contains("letrec '_Fallthrough"),
            "expected a shared label:\n{out}"
        );
        assert_compiles_through_erlc("test", &out);
    }
}
