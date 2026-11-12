// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

/// The threaded set of `src`'s construct with `outer` bound outside it.
fn threaded_set(src: &str, outer: &[&str]) -> Vec<String> {
    let expr = parse_first_expr(src);
    let construct =
        local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not a construct"));
    construct_outer_local_writes(&construct, &|n| outer.contains(&n))
        .into_iter()
        .map(|w| w.name.to_string())
        .collect()
}

#[test]
fn only_result_try_do_is_an_exception_producer_and_an_inline_propagating_send() {
    // ADR 0131 Phase 4 (BT-3750): the one receiver-aware check.
    let result = parse_first_expr("Result tryDo: [t := t + 1. 1]");
    assert!(exception_arm_blocks(&result).is_some());
    assert_eq!(
        exception_producer_writes(&result, &|n| n == "t", RootMethodKind::Class).map(|w| w.len()),
        Some(1)
    );
    for src in [
        "Set tryDo: [t := t + 1. 1]",
        "r tryDo: [t := t + 1. 1]",
        "self tryDo: [t := t + 1. 1]",
    ] {
        let expr = parse_first_expr(src);
        assert!(exception_arm_blocks(&expr).is_none(), "{src}");
        assert!(
            exception_producer_writes(&expr, &|n| n == "t", RootMethodKind::Class).is_none(),
            "{src}"
        );
        // And its block is a closure: the write is not the enclosing
        // block's own.
        let outer = parse_first_expr(&format!("[:k | {src}]"));
        let Expression::Block(block) = &outer else {
            panic!("block")
        };
        assert!(analyze_block(block).local_writes.is_empty(), "{src}");
    }
    let outer = parse_first_expr("[:k | Result tryDo: [t := t + 1. 1]]");
    let Expression::Block(block) = &outer else {
        panic!("block")
    };
    assert!(analyze_block(block).local_writes.contains("t"));
}

/// BT-4603: a unary loop reaches the classifier only canonicalised. A
/// parameterless literal receiver is the keyword loop (its condition is
/// the receiver, its body the added empty block, its selector named as
/// written); a literal with parameters, or a block value, is an ordinary
/// send to `Block` and no construct.
#[test]
fn unary_loops_reach_the_classifier_only_canonicalised() {
    for (src, keyword, written) in [
        ("[i := i + 1. i < 3] whileTrue", "whileTrue:", "whileTrue"),
        (
            "[i := i + 1. i >= 3] whileFalse",
            "whileFalse:",
            "whileFalse",
        ),
        ("[i := i + 1] repeat", "whileTrue:", "repeat"),
    ] {
        let mut expr = parse_first_expr(src);
        crate::ast::normalize::desugar_unary_loops(&mut expr);
        assert!(
            !crate::ast::normalize::is_uncanonicalised_loop(&expr),
            "{src}"
        );
        let construct =
            local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not recognized"));
        assert_eq!(construct.family, LocalThreadingFamily::Loop, "{src}");
        assert_eq!(construct.selector, keyword, "{src}");
        assert_eq!(construct.written_selector, written, "{src}");
    }
    for src in [
        "[:k | i := k] whileTrue",
        "[:k | i := k] repeat",
        "cond whileTrue",
        "cond whileFalse",
        "cond repeat",
    ] {
        let mut expr = parse_first_expr(src);
        crate::ast::normalize::desugar_unary_loops(&mut expr);
        assert!(local_threading_construct(&expr).is_none(), "{src}");
    }
}

/// BT-4603: the classifier refuses a raw parse of a unary loop rather
/// than classifying its receiver as a loop body, which codegen would lower
/// as the keyword loop's condition.
#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "uncanonicalised unary loop")]
fn the_classifier_rejects_a_raw_unary_loop() {
    let _ = local_threading_construct(&parse_first_expr("[i := i + 1. i < 3] whileTrue"));
}

#[test]
fn local_threading_construct_classifies_every_family() {
    use LocalThreadingFamily::{BlockValue, Conditional, Exception, Fold, Lookup, Loop, TryDo};
    for (src, family, blocks) in [
        ("[i < 3] whileTrue: [i := i + 1]", Loop, 2),
        // Analysis sees the canonical loop shapes (`ast::normalize`): the
        // unary loop gains an empty body, `repeat` a `[true]` condition.
        ("[i < 3] whileTrue", Loop, 2),
        ("[i := i + 1] repeat", Loop, 2),
        ("3 timesRepeat: [i := i + 1]", Loop, 1),
        ("1 to: 3 do: [:k | i := k]", Loop, 1),
        ("1 to: 9 by: 2 do: [:k | i := k]", Loop, 1),
        ("items do: [:x | i := x]", Fold, 1),
        ("items inject: 0 into: [:a :x | a + x]", Fold, 1),
        ("items detect: [:x | x > 1] ifNone: [0]", Fold, 2),
        ("items eachWithIndex: [:x :k | i := k]", Fold, 1),
        ("items do: [:x | i := x] separatedBy: [i := 0]", Fold, 2),
        ("flag ifTrue: [i := 1] ifFalse: [i := 2]", Conditional, 2),
        ("flag and: [i := 1. true]", Conditional, 1),
        ("x ifNil: [i := 1]", Conditional, 1),
        ("[i := 1] on: Error do: [:e | i := 2]", Exception, 2),
        ("[i := 1] ensure: [i := 2]", Exception, 2),
        ("[i := 1] value", BlockValue, 1),
        ("[:a | i := a] value: 3", BlockValue, 1),
        ("d at: #k ifAbsent: [i := 1]", Lookup, 1),
        ("d at: #k ifAbsentPut: [i := 1]", Lookup, 1),
        ("Result tryDo: [i := 1]", TryDo, 1),
    ] {
        let mut expr = parse_first_expr(src);
        crate::ast::normalize::desugar_unary_loops(&mut expr);
        let construct =
            local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not recognized"));
        assert_eq!(construct.family, family, "{src}");
        assert_eq!(construct.blocks.len(), blocks, "{src}");
        // Only a lookup differs from its family: four of the five lookups
        // are not implemented by `Dictionary`.
        assert_eq!(
            construct.is_threaded_today(),
            !src.contains("ifAbsentPut:"),
            "{src}"
        );
        assert_eq!(
            construct.runs_in_place(),
            construct.is_threaded_today(),
            "{src}"
        );
    }
    // BT-4430: an `on:do:`/`ensure:` with a non-literal protected body or
    // handler is a construct whose literal block runs in place, but codegen
    // compiles it as a closure, so its writes are not threaded.
    for (src, blocks) in [
        ("b ensure: [i := 2]", 1),
        ("[i := 1] ensure: c", 1),
        ("b on: Error do: [:e | i := 2]", 1),
        ("[i := 1] on: Error do: h", 1),
        ("(self make) ensure: [i := 2]", 1),
    ] {
        let expr = parse_first_expr(src);
        let construct =
            local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not recognized"));
        assert_eq!(construct.family, Exception, "{src}");
        assert_eq!(construct.blocks.len(), blocks, "{src}");
        assert!(!construct.inlined, "{src}");
        assert!(!construct.is_threaded_today(), "{src}");
        assert!(construct.runs_in_place(), "{src}");
        assert!(exception_arm_blocks(&expr).is_none(), "{src}");
    }
    for src in [
        "items foo: [:x | i := x]",
        "([i := 1]) value",
        "b value",
        "items do: blk",
        "d at: #k ifAbsent: blk",
        "3 + 4",
    ] {
        assert!(
            local_threading_construct(&parse_first_expr(src)).is_none(),
            "{src} must not be a construct"
        );
    }
}

#[test]
fn construct_outer_local_writes_is_transitively_closed_over_producers() {
    // o7: the outer `on:do:` carries `t`, written only inside a
    // conditional arm's inner `on:do:`.
    assert_eq!(
        threaded_set(
            "[flag ifTrue: [[t := t + 1. 1] on: Error do: [:e | 0]] ifFalse: [0]] \
             on: Error do: [:e | 0]",
            &["flag", "t"],
        ),
        vec!["t"]
    );
    // A loop nested in a fold nested in a conditional.
    assert_eq!(
        threaded_set(
            "flag ifTrue: [#(1) do: [:x | 1 to: 2 do: [:k | s := s + k]]]",
            &["flag", "s"],
        ),
        vec!["s"]
    );
    // Write-only writes count.
    assert_eq!(
        threaded_set("#(1, 2) do: [:x | last := x]", &["last"]),
        vec!["last"]
    );
}

#[test]
fn construct_outer_local_writes_stops_at_closures_and_unthreaded_constructs() {
    // A block passed to an ordinary send is a closure: its write is the
    // §6 check's business, not the enclosing construct's.
    let src = "#(1) do: [:x | CvA ap: [t := t + 1. 1]]";
    assert!(threaded_set(src, &["t"]).is_empty());
    // `outer_local_writes`, which the §6 check uses, does see it.
    let expr = parse_first_expr(src);
    let construct = local_threading_construct(&expr).expect("construct");
    assert_eq!(
        outer_local_writes(construct.blocks[0], &|n| n == "t").len(),
        1
    );
    // A nested construct that is not threaded today is in the set, but
    // not in the part threaded today.
    let src = "#(1) do: [:x | d at: #k ifAbsentPut: [t := 1]]";
    assert_eq!(threaded_set(src, &["t"]), vec!["t"]);
    let expr = parse_first_expr(src);
    let construct = local_threading_construct(&expr).expect("construct");
    assert!(threaded_today_block_writes(&construct.blocks, &|n| n == "t").is_empty());
    // `tryDo:` is threaded since ADR 0131 Phase 4 (BT-3750).
    let src = "#(1) do: [:x | Result tryDo: [t := 1]]";
    assert_eq!(threaded_set(src, &["t"]), vec!["t"]);
    let expr = parse_first_expr(src);
    let construct = local_threading_construct(&expr).expect("construct");
    assert_eq!(
        threaded_today_block_writes(&construct.blocks, &|n| n == "t").len(),
        1
    );
    // Its own set is recognized too.
    assert_eq!(threaded_set("Result tryDo: [t := 1]", &["t"]), vec!["t"]);
    assert_eq!(
        threaded_set("d at: #k ifAbsent: [t := 1]", &["t"]),
        vec!["t"]
    );
}

#[test]
fn expression_threaded_writes_unions_every_reachable_construct() {
    let writes = |src: &str| -> Vec<String> {
        expression_threaded_writes(&parse_first_expr(src), &|n| n == "t" || n == "u")
            .into_iter()
            .map(|w| w.name.to_string())
            .collect()
    };
    // The expression itself, an argument and a receiver (BT-3748 review).
    assert_eq!(writes("flag ifTrue: [t := 1] ifFalse: [0]"), vec!["t"]);
    assert_eq!(
        writes("self id: (flag ifTrue: [t := t + 1. 1] ifFalse: [0])"),
        vec!["t"]
    );
    assert_eq!(
        writes("(flag ifTrue: [t := t + 1. 1] ifFalse: [0]) + 0"),
        vec!["t"]
    );
    // Two constructs, in source order.
    assert_eq!(
        writes("(flag ifTrue: [u := 1] ifFalse: [0]) + ([t := 1] on: Error do: [:e | 0])"),
        vec!["u", "t"]
    );
    // A closure is not entered, and the expression's own write is not a
    // construct's.
    assert!(writes("self id: [t := t + 1. 1]").is_empty());
    assert!(writes("t := 3").is_empty());
    assert!(writes("x + (t := 3)").is_empty());
}

#[test]
fn construct_outer_local_writes_respects_inner_bindings() {
    // `tmp` is a block local (first assigned inside), `x` a parameter.
    assert_eq!(
        threaded_set(
            "#(1) do: [:x | tmp := x. tmp := tmp + 1. x := 0. t := tmp]",
            &["t"],
        ),
        vec!["t"]
    );
    // A `match:` arm binding shadows the outer name.
    assert!(threaded_set("#(1) do: [:x | x match: [n -> n + 1]]", &["n"]).is_empty());
    // Source order, first write of each name only.
    assert_eq!(
        threaded_set("flag ifTrue: [b := 1. a := 2. b := 3]", &["a", "b"]),
        vec!["b", "a"]
    );
}
