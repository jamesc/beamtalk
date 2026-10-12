// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §1 (BT-3746): `threaded_locals_of`, the one recognizer and the
//! one threaded set of a local-threading construct.

use super::*;
use crate::core_erlang::threading_analysis::{ThreadedConstruct, ThreadedLocals};
use beamtalk_core::semantic_analysis::block_facts::LocalThreadingFamily;

fn parse_expr(src: &str) -> Expression {
    let (module, diagnostics) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    module
        .expressions
        .into_iter()
        .next()
        .expect("one expression")
        .expression
}

/// A generator in `context` with `locals` bound in the generated code.
fn make_generator(context: CodeGenContext, locals: &[&str]) -> CoreErlangGenerator {
    let mut generator = CoreErlangGenerator::new("threaded_locals_test");
    generator.context = context;
    generator.push_scope();
    for local in locals {
        let core = CoreErlangGenerator::to_core_erlang_var(local);
        generator.bind_var(local, &core);
    }
    generator
}

fn threaded(generator: &CoreErlangGenerator, src: &str) -> Option<ThreadedLocals> {
    generator.threaded_locals_of(&parse_expr(src))
}

#[test]
fn o7_outer_on_do_carries_a_local_written_only_in_an_inner_on_do() {
    // ADR 0131 o7: `t` is written only inside the `ifTrue:` arm's inner
    // `on:do:`, so the outer `on:do:`'s set, and the set today's lowering
    // packs, both carry it.
    let src = "[flag ifTrue: [[t := t + 1. 1] on: Error do: [:e | 0]] ifFalse: [0]] \
               on: Error do: [:e | 0]";
    for context in [CodeGenContext::Actor, CodeGenContext::ValueType] {
        let generator = make_generator(context, &["flag", "t"]);
        let set = threaded(&generator, src).expect("o7 threads `t`");
        assert_eq!(
            set.construct,
            ThreadedConstruct::Inline(LocalThreadingFamily::Exception)
        );
        assert_eq!(set.names, vec!["t"]);
        assert_eq!(set.lowered, vec!["t"]);
    }
}

#[test]
fn repl_returns_the_bindings_the_construct_writes() {
    // In the REPL a workspace binding is not bound in the generated code
    // (it lives in the bindings map), but it is still written by the
    // construct, so it is in the set. Today's REPL lowering threads loops
    // and folds through the bindings map, so it packs none of them.
    let mut generator = make_generator(CodeGenContext::Repl, &[]);
    generator.set_is_repl_mode(true);
    for (src, family) in [
        (
            "1 to: 3 do: [:i | sum := sum + i]",
            LocalThreadingFamily::Loop,
        ),
        ("#(1, 2) do: [:x | last := x]", LocalThreadingFamily::Fold),
    ] {
        let set = threaded(&generator, src).unwrap_or_else(|| panic!("{src}: no set"));
        assert_eq!(set.construct, ThreadedConstruct::Inline(family), "{src}");
        assert_eq!(set.names.len(), 1, "{src}: {:?}", set.names);
        assert!(set.lowered.is_empty(), "{src}: {:?}", set.lowered);
    }
    // A conditional packs the locals bound in the generated code (here the
    // enclosing block's `k`), and its set also names the binding `w`.
    generator.bind_var("k", "K");
    let set = threaded(&generator, "flag ifTrue: [k := 1. w := 2]").expect("conditional");
    assert_eq!(set.names, vec!["k", "w"]);
    assert_eq!(set.lowered, vec!["k"]);
}

#[test]
fn constructs_not_lowered_today_are_recognized_with_an_empty_lowered_set() {
    let generator = make_generator(CodeGenContext::ValueType, &["t", "d"]);
    for (src, family) in [
        ("Result tryDo: [t := t + 1]", LocalThreadingFamily::TryDo),
        ("d at: #k ifAbsent: [t := 1]", LocalThreadingFamily::Lookup),
        ("[t := t + 1] value", LocalThreadingFamily::BlockValue),
        ("[t := t + 1. t < 3] whileTrue", LocalThreadingFamily::Loop),
        (
            "#(1) eachWithIndex: [:x :i | t := i]",
            LocalThreadingFamily::Fold,
        ),
    ] {
        let set = threaded(&generator, src).unwrap_or_else(|| panic!("{src}: no set"));
        assert_eq!(set.construct, ThreadedConstruct::Inline(family), "{src}");
        assert_eq!(set.names, vec!["t"], "{src}");
        assert_eq!(set.clone().into_lowered(), None, "{src}");
    }
    // `eachWithIndex:` is lowered in an actor's own fold.
    let actor = make_generator(CodeGenContext::Actor, &["t"]);
    let set = threaded(&actor, "#(1) eachWithIndex: [:x :i | t := i]").expect("set");
    assert_eq!(set.into_lowered(), Some(vec!["t".to_string()]));
}

#[test]
fn a_nested_construct_not_threaded_today_is_in_names_but_not_lowered() {
    // ADR 0131 §1: the closure covers every nested producer, but today's
    // lowering does not thread a `tryDo:` block yet (BT-3743), so it packs nothing.
    let generator = make_generator(CodeGenContext::ValueType, &["t"]);
    let set = threaded(&generator, "#(1) do: [:x | Result tryDo: [t := 1]]").expect("set");
    assert_eq!(set.names, vec!["t"]);
    assert!(set.lowered.is_empty());
}

#[test]
fn detect_if_none_handler_is_in_the_set_but_not_lowered_today() {
    let generator = make_generator(CodeGenContext::Actor, &["a", "b"]);
    let set = threaded(
        &generator,
        "#(1) detect: [:x | a := x. true] ifNone: [b := 0. 0]",
    )
    .expect("set");
    assert_eq!(set.names, vec!["a", "b"]);
    assert_eq!(set.lowered, vec!["a"]);
}

#[test]
fn tier2_value_call_on_a_block_valued_local_is_recognized() {
    let mut generator = make_generator(CodeGenContext::Actor, &["t", "b"]);
    generator.tier2_local_vars.insert("b".to_string());
    generator
        .tier2_local_var_captured_mutations
        .insert("b".to_string(), vec!["t".to_string()]);
    let set = threaded(&generator, "b value").expect("Tier 2 value call");
    assert_eq!(set.construct, ThreadedConstruct::Tier2Value);
    assert_eq!(set.names, vec!["t"]);
    assert!(set.lowered.is_empty());
    let set = threaded(&generator, "#(1) do: b").expect("opaque fold");
    assert_eq!(set.construct, ThreadedConstruct::OpaqueFold);
    // ADR 0128: an opaque fold threads only in actor instance context.
    generator.context = CodeGenContext::ValueType;
    assert!(threaded(&generator, "#(1) do: b").is_none());
    // A local that is not a Tier 2 block is not a construct.
    assert!(threaded(&generator, "c value").is_none());
}

#[test]
fn a_construct_that_writes_no_outer_local_has_no_set() {
    let generator = make_generator(CodeGenContext::Actor, &["t"]);
    for src in [
        "#(1) do: [:x | tmp := x]",
        "#(1) do: [:x | CvA ap: [t := 1]]",
        "flag ifTrue: [1]",
        "3 + 4",
    ] {
        assert!(threaded(&generator, src).is_none(), "{src}");
    }
}
