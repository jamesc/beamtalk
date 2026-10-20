// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3745 (ADR 0131 Phase 0b): the §6 error for a Tier 2 block value with no
//! return channel, and the Phase 0 allow-set error for a local-threading
//! construct in a position that does not thread its outer-local writes yet (BT-3743).

use super::*;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use std::fmt::Write as _;

fn adr0131_diagnostics_with(src: &str, known_vars: &[&str]) -> Vec<Diagnostic> {
    let ctx = AnalysisContext::default().with_known_vars(known_vars);
    analyse_full(&parse_module(src), ctx)
        .diagnostics
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(
                    DiagnosticCategory::Tier2BlockNoReturnChannel
                        | DiagnosticCategory::UnmigratedLocalThreading
                )
            )
        })
        .collect()
}

fn adr0131_diagnostics(src: &str) -> Vec<Diagnostic> {
    adr0131_diagnostics_with(src, &[])
}

fn of_category(diags: &[Diagnostic], category: DiagnosticCategory) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(category))
        .collect()
}

/// A class with a user higher-order class method, a value type with a user
/// HOM, and an actor with one, followed by `body` as the given method.
fn in_class(body: &str) -> String {
    format!(
        "Object subclass: CvA\n  class ap: b => b value\n\n  class probe =>\n    t := 0\n{}\n",
        indent(body)
    )
}

fn in_value(body: &str) -> String {
    format!(
        "Object subclass: CvA\n  class ap: b => b value\n\nValue subclass: Vt\n  ap: b => b value\n\n  probe =>\n    t := 0\n{}\n",
        indent(body)
    )
}

/// An instance method of a `TestCase` subclass: a value-type context where
/// "Cannot assign to slot" does not fire (BT-3922).
fn in_testcase(body: &str) -> String {
    format!(
        "Object subclass: CvA\n  class ap: b => b value\n\nTestCase subclass: Tc\n  ap: b => b value\n\n  probe =>\n    t := 0\n{}\n",
        indent(body)
    )
}

/// An instance method of a plain `Object` subclass: likewise no slot error.
fn in_object(body: &str) -> String {
    format!(
        "Object subclass: CvA\n  class ap: b => b value\n\nObject subclass: Ob\n  ap: b => b value\n\n  probe =>\n    t := 0\n{}\n",
        indent(body)
    )
}

fn in_actor(body: &str) -> String {
    format!(
        "Object subclass: CvA\n  class ap: b => b value\n\nActor subclass: Ac\n  state: n = 0\n\n  sealed ap: b => b value\n\n  probe =>\n    t := 0\n{}\n",
        indent(body)
    )
}

fn indent(body: &str) -> String {
    body.lines()
        .map(|l| format!("    {l}"))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// §6: a Tier 2 block value with no return channel
// ---------------------------------------------------------------------------

#[test]
fn section6_literal_block_to_user_hom_in_class_method_is_an_error() {
    let src = in_class("r := CvA ap: [t := t + 1. 1]\n#[r, t]");
    let diags = adr0131_diagnostics(&src);
    let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
    assert_eq!(s6.len(), 1, "{diags:?}");
    let d = s6[0];
    assert_eq!(d.severity, Severity::Error);
    // ADR 0133: `CvA ap:` is not `class sealed`, so it runs in the class's
    // process.
    assert_eq!(
        d.message.as_str(),
        "`t` is assigned in a block passed to a class method (`ap:`); it would run in another \
         process"
    );
    let hint = d.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("return the new value from the block and assign it where it is called"),
        "{hint}"
    );
    // Points at the assignment, with a note at the block and the construct
    // list.
    assert_eq!(d.span.start() as usize, src.find("t := t + 1. 1]").unwrap());
    let block_start = src.find("[t := t + 1. 1]").unwrap();
    assert!(
        d.notes
            .iter()
            .any(|n| n.span.is_some_and(|s| s.start() as usize == block_start))
    );
    assert!(
        d.notes
            .iter()
            .any(|n| n.message.contains("(ADR 0133)") && n.message.contains("inject:into:")),
        "{:?}",
        d.notes
    );
}

/// ADR 0133 § Error examples: the same diagnostic for every escaping shape,
/// naming the shape, reported at the assignment.
#[test]
fn escaping_block_messages_name_the_shape() {
    let sealed =
        "sealed Object subclass: Twice\n  class sealed twice: b =>\n    b value\n    b value\n\n";
    for (src, message) in [
        (
            in_class("f := [t := t + 1]\nf value\nt"),
            "`t` is assigned in a block stored in `f`",
        ),
        (
            in_class("^[t := t + 1]"),
            "`t` is assigned in a block returned from `probe`",
        ),
        (
            in_value("self forEach: [:o | t := t + o]\nt"),
            "`t` is assigned in a block passed to `forEach:`, which is not a construct the \
             compiler threads it through",
        ),
        (
            in_value("CvA onEvent: [:e | t := t + 1]\nt"),
            "`t` is assigned in a block passed to a class method (`onEvent:`); it would run in \
             another process",
        ),
        (
            format!("{sealed}{}", in_class("Twice twice: [t := t + 1]\nt")),
            "`t` is assigned in a block passed to `twice:`, which is not a construct the \
             compiler threads it through",
        ),
        (
            in_class("#[[t := t + 1]]"),
            "`t` is assigned in a block placed in a collection literal",
        ),
        (
            in_class("r := #(1) collect: [:x | [t := t + x]]\nr"),
            "`t` is assigned in a block answered by the block of a collection operation",
        ),
        (
            in_class("r := #(1) select: [:x | [t := t + x]]\nr"),
            "`t` is assigned in a block answered by the block of a collection operation",
        ),
        (
            in_class("x := [t := t + 1] foo\nx"),
            "`t` is assigned in a block sent `foo`, which is not a construct the compiler \
             threads it through",
        ),
        (
            in_class("CvA ap: 1; ap: [t := t + 1]\nt"),
            "`t` is assigned in a block sent in a cascade (`ap:`), which the compiler does not \
             thread it through",
        ),
    ] {
        let diags = adr0131_diagnostics(&src);
        let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
        assert_eq!(s6.len(), 1, "{src}: {diags:?}");
        assert_eq!(s6[0].message.as_str(), message, "{src}");
        // At the assignment.
        let at = &src[s6[0].span.start() as usize..s6[0].span.end() as usize];
        assert!(at.starts_with("t := t + "), "{src}: {at}");
    }
}

/// The note's construct list is generated from `state_threading_selectors`,
/// the one list codegen reads: every selector it names threads a block literal
/// written directly as its argument (no diagnostic), and the one shape it
/// leaves out (a user method) does not.
#[test]
fn escaping_block_note_lists_exactly_the_threading_constructs() {
    let constructs = crate::state_threading_selectors::assignment_threading_constructs();
    let note = of_category(
        &adr0131_diagnostics(&in_class("CvA ap: [t := t + 1]\nt")),
        DiagnosticCategory::Tier2BlockNoReturnChannel,
    )[0]
    .notes
    .iter()
    .map(|n| n.message.to_string())
    .find(|m| m.contains("ADR 0133"))
    .expect("construct note");
    for selector in constructs.control.iter().chain(&constructs.collection) {
        assert!(note.contains(selector), "{selector} missing from {note}");
        let bare = selector.trim_start_matches("Result ");
        assert!(
            crate::state_threading_selectors::is_state_threading_keyword_selector(bare)
                || crate::state_threading_selectors::is_state_threaded_block_receiver(bare)
                || crate::state_threading_selectors::is_state_threading_unary_selector(bare)
                || crate::state_threading_selectors::is_dictionary_inline_lookup(bare)
                || crate::state_threading_selectors::is_try_do_selector(bare),
            "{selector} is not a threading construct"
        );
    }
}

/// A block literal that reaches a send as the value of an argument or receiver
/// expression (a conditional arm, an evaluated block, a protected body, a
/// parenthesized block) escapes like a bare one (PR #4382 review); an
/// argument that only *runs* a writing block in place does not.
#[test]
fn escaping_block_through_an_argument_or_receiver_value_is_reported() {
    for body in [
        "CvA ap: (4 > 3 ifTrue: [[t := t + 1]] ifFalse: [[0]])\nt",
        "CvA ap: [[t := t + 1]] value\nt",
        "CvA ap: ([[t := t + 1]] on: Error do: [:e | nil])\nt",
        "CvA ap: ([t := t + 1])\nt",
        "#(1) do: (4 > 3 ifTrue: [[:x | t := t + x]] ifFalse: [[:x | x]])\nt",
        "(4 > 3 ifTrue: [[t := t + 1]] ifFalse: [[0]]) value\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(
                section6_errors(&src),
                1,
                "{src}: {:?}",
                adr0131_diagnostics(&src)
            );
        }
    }
    for body in [
        "CvA ap: [t := t + 1. 5] value\nt",
        "CvA ap: (4 > 3 ifTrue: [t := t + 1. 1] ifFalse: [0])\nt",
        "CvA ap: (4 > 3 ifTrue: [[t + 1]] ifFalse: [[0]])\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(
                section6_errors(&src),
                0,
                "{src}: {:?}",
                adr0131_diagnostics(&src)
            );
        }
    }
}

/// One diagnostic per assignment: a value-position assignment inside an
/// escaping block is the escaping-block error only (PR #4382 review).
#[test]
fn value_position_write_in_an_escaping_block_is_reported_once() {
    for src in [
        in_class("b := [CvA ap: (t := 1)]\nt"),
        in_value("b := [CvA ap: (t := 1)]\nt"),
    ] {
        let all = analyse_full(&parse_module(&src), AnalysisContext::default()).diagnostics;
        let errors: Vec<_> = all
            .iter()
            // (`in_value` has two classes in one module, an uncategorised error.)
            .filter(|d| d.severity == Severity::Error && d.category.is_some())
            .collect();
        assert_eq!(errors.len(), 1, "{src}: {errors:?}");
        assert_eq!(
            errors[0].category,
            Some(DiagnosticCategory::Tier2BlockNoReturnChannel)
        );
    }
}

/// ADR 0133: reading a captured variable, and `^`, inside an escaping block
/// are unaffected; so is a write to the block's own locals or parameters.
#[test]
fn escaping_blocks_may_read_captured_variables_and_return() {
    for body in [
        "f := [t + 1]\nf value",
        "^[t + 1]",
        "CvA ap: [t + 1]\nt",
        "CvA ap: [^t]\nt",
        "CvA ap: [:x | x > 0 ifTrue: [^x]. t]\nt",
        "f := [:x | s := x. s := s + t. s]\nf value: 1",
        "#[[t]]",
        "r := #(1) collect: [:x | [x + t]]\nr",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(
                of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn section6_applies_in_value_type_methods_and_to_write_only_blocks() {
    let diags = adr0131_diagnostics(&in_value("CvA ap: [t := 5]\nt"));
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

/// ADR 0133 (BT-3867's I5 shape included): a block stored in a local escapes
/// however the local is used later, `value` sends included, in every context;
/// one diagnostic per block, at its first write.
#[test]
fn stored_block_is_an_error_however_it_is_used() {
    for body in [
        "b := [t := t + 1]\nb value\nt",
        "b := [:n | t := t + n]\nb value: 2\nt",
        "b := [:n :m | t := t + n + m]\nb value: 1 value: 2\nt",
        "b := [:n | t := t + n]\nb value: 1; value: 2\nt",
        "b := [t := t + 1. t]\nr := b value\n#[r, t]",
        "b := [t := t + 1. t]\n^b value",
        "b := [t := t + 1]\n#(1) do: [:e | b value]\nt",
        "b := [t := t + 1]\nb",
        "b := [t := t + 1]\nCvA ap: b\nt",
        "b := [:x | t := t + x]\n#(1, 2) do: b\nt",
        // Replaced or shadowed later: storing it was the escape.
        "b := [t := t + 1]\nb := [0]\nb value\nt",
        "b := [t := t + 1]\n#(1, 2) do: [:b | b printString]\nt",
        // Bound inside a block.
        "#(1) do: [:e | b := [t := t + 1]. b value]\nt",
        // The value of a conditional.
        "b := 4 > 3 ifTrue: [[t := t + 1]] ifFalse: [[0]]\nb value\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
            assert_eq!(s6.len(), 1, "{src}: {diags:?}");
            assert!(
                s6[0]
                    .message
                    .starts_with("`t` is assigned in a block stored in `b`")
                    || s6[0].message.contains("returned from `probe`"),
                "{src}: {}",
                s6[0].message
            );
        }
    }
}

/// ADR 0133 retires the actor self-send exemption (BT-912/BT-3755): a block
/// passed to any method of the actor's own class escapes, whatever the callee
/// does with it, and so does every near miss the exemption used to reject.
#[test]
fn former_actor_exemptions_and_their_near_misses_are_errors() {
    // The former exemptions.
    for callee in [
        "  sealed ap: b => b value",
        "  sealed ap: b => b value: 5",
        "  sealed ap: b =>\n    b value\n    b value",
        "  sealed ap: b => #(1, 2) do: b",
        "  sealed ap: b => self target: b\n\n  sealed target: c => c value",
    ] {
        assert_eq!(section6_errors(&actor_calling("", callee)), 1, "{callee}");
    }
    assert_eq!(
        section6_errors(&actor_calling("sealed ", "  ap: b => b value")),
        1
    );
    // The near misses, each probed in an actor instance method on the debug
    // build (BT-3745): each answers 0 (write dropped) or raises.
    for body in [
        // Self-send: stored block argument (detect_tier2_self_send promotes
        // only a bare block literal or a Tier 2 block *parameter*).
        "b := [t := t + 1]\nself ap: b\nt",
        "b := [t := t + 1]\nself ap: (b)\nt",
        // Self-send: parenthesized block argument.
        "self ap: ([t := t + 1])\nt",
        // Self-send: write-only block (not a captured mutation).
        "self ap: [t := 5]\nt",
        // Self-send: `super`, a cascade, or not at the method's top level.
        "super ap: [t := t + 1]\nt",
        "self ap: [t := t + 1]; yourself\nt",
        "4 > 3 ifTrue: [self ap: [t := t + 1]]\nt",
        "#(1, 2) do: [:e | self ap: [t := t + e]]\nt",
        "x := (self ap: [t := t + 1. 2]) + 1\n#[x, t]",
        // Stored block: folded by a collection HOM.
        "b := [:x | t := t + x]\n#(1, 2) do: b\nt",
        "b := [:x | t := t + x]\n#(1, 2) collect: b\nt",
        // Stored block: parenthesized receiver.
        "b := [t := t + 1]\n(b) value\nt",
        "b := [:n | t := t + n]\n(b) value: 1; value: 2\nt",
        // Stored block: `value` not as a method-body statement.
        "b := [t := t + 1. 7]\nr := b value\n#[r, t]",
        "b := [:n | t := t + n]\n#[b value: 3, t]",
        "b := [:n | t := t + n]\nx := 10 + (b value: 1)\n#[x, t]",
        "b := [t := t + 1]\n#(1) do: [:e | b value]\nt",
        // Stored block: bound inside a block, or write-only.
        "#(1) do: [:e | b := [t := t + 1]. b value]\nt",
        "b := [t := 5]\nb value\nt",
        // Literal block: parenthesized receiver sent `value`.
        "([t := t + 1]) value\nt",
    ] {
        let src = in_actor(body);
        let diags = adr0131_diagnostics(&src);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
            1,
            "{body}: {diags:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// BT-3755 (retired by ADR 0133): an actor self-send is an escape, whatever the
// callee does
// ---------------------------------------------------------------------------

/// An actor whose `probe` self-sends `ap:` a block writing `t`, with `callee`
/// (the text of zero or more methods, at class-body indentation) as the
/// rest of the class and `modifiers` before `Actor subclass:`.
fn actor_calling(modifiers: &str, callee: &str) -> String {
    format!(
        "Actor subclass: Base\n  inherited: b => b value\n\n{modifiers}Actor subclass: Ac\n  state: n = 0\n\n{callee}\n\n  probe =>\n    t := 0\n    self ap: [t := t + 1]\n    t\n"
    )
}

fn section6_errors(src: &str) -> usize {
    of_category(
        &adr0131_diagnostics(src),
        DiagnosticCategory::Tier2BlockNoReturnChannel,
    )
    .len()
}

/// The callee shapes BT-3755's exemption kept (a `sealed` same-class method,
/// or a method of a `sealed` class, whose every use of the parameter is a
/// direct `value` send as a body statement, assignment right-hand side or `^`
/// value): ADR 0133 makes each an escape.
#[test]
fn section6_actor_self_send_to_a_former_threading_callee_is_an_error() {
    for callee in [
        "  sealed ap: b => b value",
        "  sealed ap: b => b value: 5",
        "  sealed ap: b => ^b value",
        "  sealed ap: b =>\n    r := b value\n    r",
        "  sealed ap: b =>\n    b value\n    b value",
        "  sealed ap: b =>\n    b value: 1; value: 2",
        // A parameter that is never used is never invoked, so nothing is lost.
        "  sealed ap: b => 7",
        // A `do:` loop assigning the invocation to a local bound before it.
        "  sealed ap: b =>\n    last := nil\n    #(1, 2) do: [:e | last := b value: e]\n    last",
        // The callable of an opaque-callable collection HOM (ADR 0128), as a
        // statement, an operand, or inside a conditional or `to:do:` body.
        "  sealed ap: b => #(1, 2) do: b",
        "  sealed ap: b => (#(1, 2) collect: b) size",
        "  sealed ap: b => #(1, 2) inject: 0 into: b",
        "  sealed ap: b =>\n    4 > 3 ifTrue: [#(1) do: b]\n    nil",
        "  sealed ap: b =>\n    1 to: 2 do: [:i | #(1) do: b]\n    nil",
        "  sealed ap: b =>\n    [(#(1) count: b) > 5] whileTrue: [nil]\n    nil",
        // Passed on to another sealed callee that threads it.
        "  sealed ap: b => self target: b\n\n  sealed target: c => c value",
        "  sealed ap: b =>\n    r := self target: b\n    r\n\n  sealed target: c => ^c value",
    ] {
        assert_eq!(section6_errors(&actor_calling("", callee)), 1, "{callee}");
    }
    let two = "Actor subclass: Ac\n  sealed ap: a with: b => b value\n\n  probe =>\n    t := 0\n    self ap: 1 with: [t := t + 1]\n    t\n";
    assert_eq!(section6_errors(two), 1);
}

/// Each of these answered a wrong value or raised before BT-3755 (probed on
/// the debug build: the write was dropped, or `stateful_block_dispatch` / a
/// `perform:` arity error was raised), and is a compile error until the
/// callee-side `{Result, StateAcc}` protocol (BT-3742) lets a callee hand the
/// write back. Two are conservative rather than probed wrong: a callee that
/// passes the block on to another same-class callee (BT-3755) is rejected
/// only because the target does not thread it, and an overridable callee, which is
/// only wrong when a subclass overrides it. Since ADR 0133 (BT-3902) the §6
/// check rejects every such send whatever the callee does.
#[test]
fn section6_actor_self_send_to_a_non_threading_callee_is_an_error() {
    for (modifiers, callee) in [
        // Invoked inside a loop, conditional, operand or another block.
        ("", "  sealed ap: b => #(1) do: [:x | b value]"),
        ("", "  sealed ap: b => 4 > 3 ifTrue: [b value]"),
        ("", "  sealed ap: b => (b value) + 1"),
        ("", "  sealed ap: b =>\n    f := [b value]\n    f value"),
        ("", "  sealed ap: b =>\n    x := (b value) + 1\n    x"),
        // Stored, returned, passed on or used as an argument.
        ("", "  sealed ap: b =>\n    self.n := b\n    nil"),
        ("", "  sealed ap: b => b"),
        ("", "  sealed ap: b => self inherited: b"),
        ("", "  sealed ap: b => self ap: b"),
        (
            "",
            "  sealed ap: b => self target: b\n\n  target: c => c value",
        ),
        (
            "",
            "  sealed ap: b => self target: b\n\n  sealed target: c => #(1) do: [:e | c value]",
        ),
        (
            "",
            "  sealed ap: b => self target: b\n\n  sealed target: c => self ap: c",
        ),
        (
            "",
            "  sealed ap: b => self target: b with: b\n\n  sealed target: c with: d => c value",
        ),
        // A loop body that does not assign the invocation to a prior local,
        // or does more with the block (the bare `do:` is probed: the write is
        // dropped).
        ("", "  sealed ap: b => #(1, 2) do: [:e | b value: e]"),
        (
            "",
            "  sealed ap: b =>\n    #(1, 2) do: [:e | r := b value: e]\n    nil",
        ),
        (
            "",
            "  sealed ap: b =>\n    last := nil\n    #(1, 2) do: [:e | last := (b value: e) + 1]\n    last",
        ),
        (
            "",
            "  sealed ap: b =>\n    last := nil\n    #(1, 2) collect: [:e | last := b value: e]\n    last",
        ),
        (
            "",
            "  sealed ap: b =>\n    last := nil\n    1 to: 2 do: [:e | last := b value: e]\n    last",
        ),
        (
            "",
            "  sealed ap: b =>\n    last := nil\n    #(1, 2) do: [:e | last := b value: e. b value]\n    last",
        ),
        // The callable of a collection HOM from a closure, a loop or fold
        // body, or an argument position the opaque fold does not cover.
        ("", "  sealed ap: b =>\n    f := [#(1) do: b]\n    f value"),
        ("", "  sealed ap: b => #(1) do: [:e | #(2) do: b]"),
        ("", "  sealed ap: b => #(1) collect: [:e | #(2) do: b]"),
        ("", "  sealed ap: b => #(1) do: [:e | e] separatedBy: b"),
        ("", "  sealed ap: b => self.n sort: b"),
        ("", "  sealed ap: b => #(1) do: (b)"),
        ("", "  sealed ap: b => #(1) do: b; yourself"),
        ("", "  sealed ap: b => b value: b"),
        ("", "  sealed ap: b =>\n    b := b value\n    b"),
        // Overridable: an actor self-send late-binds, and a subclass override
        // is compiled with its parameter as a plain block (probed: raises).
        ("", "  ap: b => b value"),
        // Not defined by this class: unknown or class-side.
        ("", "  other: b => b value"),
        ("", "  class ap: b => b value"),
    ] {
        assert_eq!(
            section6_errors(&actor_calling(modifiers, callee)),
            1,
            "{modifiers}{callee}"
        );
    }
    // A callee defined by a superclass is not the class's own method, even
    // when the superclass is in the same module and sealed.
    let inherited = "Actor subclass: Parent\n  sealed ap: b => b value\n\nParent subclass: Ac\n  probe =>\n    t := 0\n    self ap: [t := t + 1]\n    t\n";
    assert_eq!(section6_errors(inherited), 1);
    // The second parameter is the one that must thread.
    let second = "Actor subclass: Ac\n  sealed ap: a with: b => #(1) do: [:x | b value]\n\n  probe =>\n    t := 0\n    self ap: 1 with: [t := t + 1]\n    t\n";
    assert_eq!(section6_errors(second), 1);
}

/// A standalone `Ac >> probe` method is checked with no callee set: codegen's
/// class pre-scan (`scan_class_for_tier2_blocks`) walks only the methods in the
/// class body, so a callee is never compiled with a Tier 2 block parameter
/// for a self-send made from a standalone method, even a `sealed` one in the
/// same module. The self-send is therefore a §6 error (BT-3755).
#[test]
fn section6_standalone_method_actor_self_send_is_an_error() {
    let src = "Actor subclass: Ac\n  sealed ap: b => b value\n\nAc >> probe =>\n  t := 0\n  self ap: [t := t + 1]\n  t\n";
    assert_eq!(section6_errors(src), 1);
}

/// ADR 0133 Phase 1 (BT-3902): the inert `*.bt.rejected` fixtures under
/// `stdlib/test/fixtures/` hold the escaping-block shapes as whole programs
/// over class, value-type and actor methods: the ADR 0132 §6 inventory
/// (I1-I4, I6-I8, formerly the `PIN-BUG BT-3865` pending fixtures), BT-3867's
/// stored-block `value` shape (I5) and BT-3755's former actor self-send
/// exemptions. ADR 0133 makes each a permanent compile error, so the files
/// never compile (the stdlib runner discovers only `.bt`); this test is their
/// assertion. Every method not named `callee*` is rejected with exactly one
/// escaping-block diagnostic, at an assignment inside it (`… is assigned in a
/// block …`); no `callee*` method carries any ADR 0131 threading diagnostic.
#[test]
fn adr0133_rejected_fixtures_report_the_escaping_assignment() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    let mut names: Vec<String> = std::fs::read_dir(&root)
        .expect("fixtures directory")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().into_string().ok()?;
            name.ends_with(".bt.rejected").then_some(name)
        })
        .collect();
    names.sort();
    assert!(names.len() >= 7, "{names:?}");
    for name in names {
        let src =
            std::fs::read_to_string(root.join(&name)).unwrap_or_else(|e| panic!("{name}: {e}"));
        let module = parse_module(&src);
        let diags = adr0131_diagnostics(&src);
        let class = &module.classes[0];
        let mut probes = 0;
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            let selector = method.selector.name();
            let within = |d: &&Diagnostic| {
                method.span.start() <= d.span.start() && d.span.end() <= method.span.end()
            };
            let found: Vec<&Diagnostic> = diags.iter().filter(within).collect();
            if selector.starts_with("callee") {
                assert!(found.is_empty(), "{name}: helper `{selector}`: {found:?}");
                continue;
            }
            probes += 1;
            assert_eq!(found.len(), 1, "{name}: `{selector}`: {found:?}");
            let d = found[0];
            assert_eq!(
                d.category,
                Some(DiagnosticCategory::Tier2BlockNoReturnChannel),
                "{name}: `{selector}`"
            );
            assert!(
                d.message.contains("is assigned in a block"),
                "{name}: `{selector}`: {}",
                d.message
            );
            let at = &src[d.span.start() as usize..d.span.end() as usize];
            assert!(at.contains(":="), "{name}: `{selector}` reported at `{at}`");
        }
        assert!(probes >= 2, "{name}: {probes} probes");
    }
}

#[test]
fn section6_actor_non_self_send_is_still_an_error() {
    let diags = adr0131_diagnostics(&in_actor("CvA ap: [t := t + 1]\nt"));
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

#[test]
fn adr0133_actor_self_send_of_a_collection_message_is_an_escaping_block() {
    // BT-3908: `self do: [...]` in an actor is a dispatched self-send to the
    // actor's own `do:`, not the inlined collection construct, so its block
    // escapes. Its write rode only the callee's ADR 0128 fold, now removed.
    for body in [
        "self do: [:x | t := t + x]\nt",
        "self inject: 0 into: [:a :x | t := a + x]\nt",
        "super collect: [:x | t := x]\nt",
    ] {
        let diags = adr0131_diagnostics(&in_actor(body));
        let found = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
        assert_eq!(found.len(), 1, "{body}: {diags:?}");
        assert!(
            (found[0]
                .message
                .contains("is assigned in a block passed to `self ")
                || found[0]
                    .message
                    .contains("is assigned in a block passed to `super "))
                && found[0].message.contains("actor's own"),
            "{body}: {diags:?}"
        );
    }
    // Any other receiver is the inlined construct; a value type's `self do:`
    // is lowered in place too.
    for src in [
        in_actor("#(1, 2) do: [:x | t := t + x]\nt"),
        in_value("self do: [:x | t := t + x]\nt"),
    ] {
        let diags = adr0131_diagnostics(&src);
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn bt3935_class_side_self_send_of_a_collection_message_is_an_escaping_block() {
    // `self do: [...]` in a class method is a dispatched send to the class's
    // own class-side `do:`, not the inlined collection construct.
    for body in [
        "self do: [:x | t := t + x]\nt",
        "self inject: 0 into: [:a :x | t := a + x]\nt",
        "super collect: [:x | t := x]\nt",
    ] {
        let diags = adr0131_diagnostics(&in_class(body));
        let found = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
        assert_eq!(found.len(), 1, "{body}: {diags:?}");
        assert!(
            found[0].message.contains("class's own"),
            "{body}: {diags:?}"
        );
    }
    // A non-writing block and a non-self receiver stay clean.
    for body in ["self do: [:x | x]\nt", "#(1, 2) do: [:x | t := t + x]\nt"] {
        let diags = adr0131_diagnostics(&in_class(body));
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
            "{body}: {diags:?}"
        );
    }
}

#[test]
fn section6_class_side_self_send_has_no_return_channel() {
    let diags = adr0131_diagnostics(&in_class("self ap: [t := t + 1]\nt"));
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

/// BT-3917: the escaping-block errors of `src`, by message.
fn ffi_escape_messages(src: &str) -> Vec<String> {
    adr0131_diagnostics(src)
        .iter()
        .filter(|d| d.message.contains("passed to Erlang"))
        .map(|d| d.message.to_string())
        .collect()
}

#[test]
fn erlang_ffi_argument_writing_block_is_an_escaping_block() {
    // ADR 0133 / BT-3917: a block literal that assigns a captured local, in
    // any argument position of an FFI send, escapes (Erlang calls it as a
    // plain fun and the write is dropped).
    for src in [
        in_class("r := (Erlang lists) map: [:x | t := t + 1. x] with: #(1)\n#[r, t]"),
        in_value("r := Erlang lists map: [:x | t := t + 1. x] with: #(1)\n#[r, t]"),
        // keyword argument in the trailing position
        in_value("r := Erlang lists foreach: [:x | t := t + x] with: #(1)\nt"),
        // the repro: the block is the init argument of a nested FFI send
        in_value(
            "b := Erlang erlang element: 1 of: (Erlang erlang make_tuple: 1 init: [:x | t := t + x])\nt",
        ),
        // parenthesized block literal
        in_value("r := Erlang lists map: ([:x | t := t + 1. x]) with: #(1)\n#[r, t]"),
    ] {
        let msgs = ffi_escape_messages(&src);
        assert_eq!(msgs.len(), 1, "{src}: {msgs:?}");
        assert!(
            msgs[0].starts_with("`t` is assigned in a block passed to Erlang `"),
            "{msgs:?}"
        );
        let diags = adr0131_diagnostics(&src);
        let hint = diags[0].hint.as_deref().unwrap_or_default();
        assert!(
            hint.contains("inject: t into:") && hint.contains("use the Erlang call's result"),
            "{hint}"
        );
    }
}

#[test]
fn erlang_ffi_argument_writing_block_nested_in_literal_is_rejected() {
    // A writing block inside a collection literal passed to FFI is an
    // escaping block too (placed in a collection literal).
    let src = in_value("r := Erlang erlang list_to_tuple: #([:x | t := t + x])\nt");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(
        diags[0].message.contains("`t` is assigned in a block"),
        "{diags:?}"
    );
}

#[test]
fn erlang_ffi_argument_non_writing_block_is_accepted() {
    for src in [
        in_class("r := (Erlang lists) map: [:x | x + t] with: #(1)\nr"),
        in_value("r := Erlang lists map: [:x | x + t] with: #(1)\nr"),
        // a block-local variable is not a captured write
        in_value("r := Erlang lists map: [:x | y := x + 1. y] with: #(1)\nr"),
    ] {
        assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
    }
}

#[test]
fn section6_section1_constructs_are_exempt() {
    for body in [
        "#(1, 2) do: [:x | t := t + x]\nt",
        "r := #(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]\n#[r, t]",
        "r := 4 > 5 ifTrue: [2] ifFalse: [t := t + 1. 1]\n#[r, t]",
        "r := [t := t + 1. 1] on: Error do: [:e | t := t + 2. 0]\n#[r, t]",
        "[t := t + 1] ensure: [t := t + 1]\nt",
        "Result tryDo: [t := t + 1. 1]\nt",
        "[t < 3] whileTrue: [t := t + 1]\nt",
        "[t := t + 1] value\nt",
    ] {
        let src = in_class(body);
        let diags = adr0131_diagnostics(&src);
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
            "{body}: {diags:?}"
        );
    }
}

#[test]
fn section6_block_local_writes_do_not_count() {
    let src = in_class("r := CvA ap: [:x | s := 0. s := s + x. s]\nr");
    assert!(adr0131_diagnostics(&src).is_empty());
}

#[test]
fn section6_nested_write_makes_the_outer_block_a_tier2_value() {
    // The `do:` is inlined into the outer block, so its write to `t` is the
    // outer block's write, which `ap:` cannot return.
    let src = in_class("CvA ap: [#(1, 2) do: [:x | t := t + x]]\nt");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

// ---------------------------------------------------------------------------
// Phase 0: the allow-set error
// ---------------------------------------------------------------------------

#[test]
fn allow_set_operand_position_is_an_error_naming_construct_position_and_epic() {
    // o1's operand shape with a construct that is not a producer: a `value:`
    // sent to a block literal (an `on:do:` is one since BT-3817, ADR 0131 Phase
    // 3b, and a unary `value` since BT-3841: its argument-binding form keeps the
    // legacy rows).
    let src = in_class("r := ([:v | t := t + v. 1] value: 2) + 1\n#[r, t]");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    let d = p0[0];
    assert_eq!(d.severity, Severity::Error);
    assert_eq!(
        d.message.as_str(),
        "`value:` writes outer local `t`, but as the receiver of a message in a class \
         method the write is not threaded back yet (BT-3743)"
    );
    assert!(
        d.hint
            .as_deref()
            .is_some_and(|h| h.contains("as a statement of its own")),
        "{d:?}"
    );
}

#[test]
fn allow_set_statement_and_listed_assign_rhs_are_accepted() {
    // o11, s3, s5, s8.
    for body in [
        "4 =:= 5 ifFalse: [t := t + 1]\na := t\n[t := t + 1] ensure: [nil]\n#[a, t]",
        "r := #(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]\n#[r, t]",
        "r := 4 =:= 5 ifTrue: [2] ifFalse: [t := t + 1. 1]\n#[r, t]",
        "#(1, 2) do: [:x | t := t + x]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn a_returned_do_inside_a_block_is_accepted_in_class_and_value_methods() {
    // BT-3796 made `do:` a producer whose closed form answers the
    // `{nil, StateAcc}` carrier in class and value-type methods.
    //
    // BT-3738 (ADR 0131 Phase 3): the arm of a conditional is a `BranchArm`
    // frame whose statements are producer sites, so `c ifTrue: [^xs do: ...]`
    // is lowered as a producer and answers `nil`.
    let in_arm = "c ifTrue: [^#(1, 2) do: [:x | t := t + x]]\nt";
    for src in [in_class(in_arm), in_value(in_arm), in_actor(in_arm)] {
        assert!(adr0131_diagnostics_with(&src, &["c"]).is_empty(), "{src}");
    }
    //
    // BT-3817 (ADR 0131 Phase 3b): the protected body, handler and cleanup of an
    // `on:do:`/`ensure:` that is a producer are `BranchArm` frames too, so a
    // `^xs do: [...]` in one is lowered the same way.
    for body in [
        "[^#(1, 2) do: [:x | t := t + x]] on: Error do: [:e | nil]\nt",
        "[nil] on: Error do: [:e | ^#(1, 2) do: [:x | t := t + x]]\nt",
        "[nil] ensure: [^#(1, 2) do: [:x | t := t + x]]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics_with(&src, &["c"]);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
    let keyed = "c ifTrue: [^#{#a => 1} keysAndValuesDo: [:k :v | t := t + v]]\nt";
    for src in [in_class(keyed), in_value(keyed)] {
        assert!(adr0131_diagnostics_with(&src, &["c"]).is_empty(), "{src}");
    }
    //
    // BT-3813: in any other closure (a `detect:ifNone:` handler, a sort block)
    // codegen lowers the `^` value through the producer with the `Discard`
    // tail (`nlr_return_construct_value`), so the `ALLOW_SET` row accepts it
    // in class and value-type methods (the BUnit pins are `doReturnedInClosure`
    // and friends in `adr0131do_producer_test.bt`). An actor method keeps the
    // error: its row lists only the conditionals.
    for body in [
        "#(1) detect: [:y | y > c] ifNone: [^#(1, 2) do: [:x | t := t + x]]\nt",
        "#(1) detect: [:y | y > c] ifNone: [^(#{#a => 1} do: [:x | t := t + x])]\nt",
        "#(1) detect: [:y | y > c] ifNone: [^#{#a => 1} keysAndValuesDo: [:k :v | t := t + v]]\nt",
        "#(2, 1) sort: [:a :b | ^#(1, 2) do: [:x | t := t + x]]\nt",
    ] {
        for src in [in_class(body), in_value(body)] {
            let diags = adr0131_diagnostics_with(&src, &["c"]);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
        let src = in_actor(body);
        let diags = adr0131_diagnostics_with(&src, &["c"]);
        assert!(
            !of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn a_loop_nested_in_an_actor_fold_through_a_closure_is_rejected() {
    // BT-3796: codegen's `in_fold_frame` crosses a closure between a fold and a
    // nested loop, as `local_producer_sites` does (the validator's
    // `frame_sites` stop being all frames there), so the nested loop is judged
    // by the rows, not as a frame site. In an actor it is a Phase 0 error and
    // so never reaches the narrowing; the agreement test's closure two-level
    // FRAMES pin that the two sides agree on the rest.
    //
    // ADR 0131 Phase 3c (BT-3822): an arm between the two is a `BranchArm`
    // frame now (the conditional is a producer in the fold body), so the loop in
    // it is a frame site in every context: `armLoopInFoldStatement` and
    // `armLoopInFoldAssigned` in `adr0131loop_body_producer_test.bt` pin the
    // answers.
    let body = "r := 0\n#(1, 2) do: [:k | [:z | 1 to: 2 do: [:i | r := #(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]]] value: k]\n#[t, r]";
    assert!(
        !adr0131_diagnostics(&in_actor(body)).is_empty(),
        "an actor accepted {body}"
    );
    for body in [
        "r := 0\n#(1, 2) do: [:k | k > 0 ifTrue: [1 to: 2 do: [:i | r := #(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]]]]\n#[t, r]",
        "#(1, 2) do: [:k | k > 0 ifTrue: [1 to: 2 do: [:i | t := t + 1]]]\nt",
        "#(1, 2) do: [:k | k > 0 ifTrue: [1 to: 2 do: [:i | #(1, 2) do: [:x | t := t + x]]]]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
        }
    }
}

#[test]
fn allow_set_do_assignment_is_accepted_in_every_context() {
    // `r := coll do: [...]` dropped the write in class and value-type methods
    // and worked in an actor method. ADR 0131 Phase 2c (BT-3796) makes `do:` a
    // local-threading producer (`producer_body_arg_index`), so it is accepted
    // in every context; the BUnit pins are `doAsAssignedValue` and friends in
    // `adr0131do_producer_test.bt`.
    let body = "r := #(1, 2) do: [:x | t := t + x]\nt";
    for src in [in_class(body), in_value(body), in_actor(body)] {
        assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
    }
}

#[test]
fn allow_set_field_assignment_in_class_method_is_an_error() {
    // o5's shape with a construct that is not a producer yet (an `on:do:` is one
    // since BT-3817): the tuple would be stored in the class variable.
    let src = "Object subclass: Cm\n  classState: n = 0\n\n  class probe =>\n    t := 0\n    self.n := [:v | t := t + v] value: 1\n    t\n";
    let diags = adr0131_diagnostics(src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert!(p0[0].message.contains("the value of a field assignment"));
}

#[test]
fn allow_set_nested_assignment_crossing_a_block_is_an_error() {
    // An assignment inside a loop body whose construct writes a method-level
    // local needs the loop to thread it on. A construct that is not a
    // local-threading producer in the body keeps the error.
    let src = in_class("r := 0\n#(1, 2) do: [:x | r := [:v | t := t + y] value: 1]\n#[r, t]");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert!(
        p0[0].message.contains("inside a block"),
        "{}",
        p0[0].message
    );
}

#[test]
fn a_conditional_assigned_in_a_loop_body_is_a_producer_in_class_and_value_methods() {
    // ADR 0131 Phase 3c (BT-3822): the conditional is a local-threading
    // producer at the assignment's whole value in a loop body
    // (`conditionalAssignedInDoBody` in adr0131loop_body_producer_test.bt). An
    // actor's fold frame does not lower a value-bearing site
    // (`is_actor_fold_value_site`), so the actor keeps the error.
    let body =
        "r := 0\n#(1, 2) do: [:x | r := x > 1 ifTrue: [t := t + x. 1] ifFalse: [0]]\n#[r, t]";
    for src in [in_class(body), in_value(body)] {
        assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
    }
    assert!(!adr0131_diagnostics(&in_actor(body)).is_empty());
}

#[test]
fn allow_set_nested_assignment_of_block_local_is_accepted() {
    // vt_threading_constructs_test.bt `branchNestedAssign:`: the construct only
    // writes locals of its own branch block, so it threads in that frame.
    let src = in_value(
        "4 > 3 ifTrue: [\n  n := 0\n  r := #(1, 2) collect: [:i | n := n + 1. i * 2]\n  t := r size + n\n]\nt",
    );
    let diags = adr0131_diagnostics(&src);
    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn allow_set_detect_if_none_handler_write_is_a_producer_everywhere() {
    // s2: a verifier panic (class), unbound `State` (value) or a silently
    // dropped write (actor) before BT-3745. ADR 0131 Phase 2c (BT-3794): the
    // fold packs the `ifNone:` handler's writes with its search block's, so a
    // handler-writing `detect:ifNone:` is a producer at every method-body
    // operand site and a statement, in every method context.
    for body in [
        "r := #(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]\n#[r, t]",
        "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1]\nt",
        "^(#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]) + t",
        "r := t + (#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0])\n#[r, t]",
        "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [t := t + 10. 0]\nt",
        "u := 0\nr := #(1) detect: [:x | t := t + 1. false] ifNone: [u := 1. 0]\n#[r, t, u]",
        "u := 0\nr := (#(1) detect: [:x | t := t + 1. false] ifNone: [u := 1. 0]) + 1\n#[r, t, u]",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
    // BT-3841: also an assignment in an arm; ADR 0133 Phase 3 (BT-3911): in an
    // actor method too, where the fold was measured wrong until its frames
    // lowered a `detect:ifNone:` like any other fold.
    let arm = "true ifTrue: [r := #(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]]\nt";
    for src in [in_class(arm), in_value(arm), in_actor(arm)] {
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

#[test]
fn detect_if_none_handler_write_in_a_counted_loop_body_is_a_producer_in_every_context() {
    // BT-3822 review: a handler that writes an outer local inside a
    // direct-params `to:do:` must never be a silently dropped write or a leaked
    // tuple. BT-3841 lowers it for a class or value-type method (the loop body
    // is a producer frame there); ADR 0133 Phase 3 (BT-3911, closing BT-3861)
    // lowers it in an actor method too.
    let body = "t := 0\nn := 0\n1 to: 3 do: [:i |\n  n := n + 1\n  \
                #(1, 2) detect: [:e | e > 5] ifNone: [t := t + 1]\n]\n#[t, n]";
    for src in [in_class(body), in_value(body), in_actor(body)] {
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

#[test]
fn allow_set_detect_if_none_needs_a_literal_search_block() {
    // BT-3794 review: codegen packs the handler's writes only when the search
    // block is a literal (`producer_body_block`). A parenthesised search block
    // routes to the fold but is not packed, so its handler's write was dropped
    // (statement) or leaked a raw tuple (`r :=`). Phase 0 rejects every
    // position in every method context; the REPL threads its bindings map.
    for body in [
        "#(1, 2) detect: ([:x | x > 5]) ifNone: [t := 7]\nt",
        "(#(1, 2) detect: ([:x | x > 5]) ifNone: [t := 7])\nt",
        "r := #(1, 2) detect: ([:x | x > 5]) ifNone: [t := t + 1. 0]\n#[r, t]",
        "^(#(1, 2) detect: ([:x | x > 5]) ifNone: [t := t + 1. 0]) + t",
        "r := t + (#(1, 2) detect: ([:x | x > 5]) ifNone: [t := t + 1. 0])\n#[r, t]",
        "#(1, 2) detect: ([:x | t := t + 1. x > 5]) ifNone: [t := t + 10. 0]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(
                !of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
                "{src}: {diags:?}"
            );
        }
    }
    let known = ["t"];
    let ok = adr0131_diagnostics_with(
        "r := #(1, 2) detect: ([:x | x > 5]) ifNone: [t := t + 1. 0]\n",
        &known,
    );
    assert!(ok.is_empty(), "{ok:?}");
}

#[test]
fn allow_set_repl_uses_its_own_row() {
    let known = ["t"];
    // REPL o4: receiver position. A local-threading producer since BT-3795
    // (ADR 0131 Phase 2d); a conditional's receiver is not a site, and keeps
    // the diagnostic.
    let diags = adr0131_diagnostics_with("(#(1, 2) collect: [:x | t := t + x]) size\n", &known);
    assert!(diags.is_empty(), "{diags:?}");
    let diags = adr0131_diagnostics_with(
        "(#(1, 2) collect: [:x | t := t + x]) isEmpty ifTrue: [0]\n",
        &known,
    );
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert!(p0[0].message.contains("in a top-level expression"));
    // REPL s2 works: detect:ifNone: RHS.
    let ok = adr0131_diagnostics_with(
        "r := #(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]\n",
        &known,
    );
    assert!(ok.is_empty(), "{ok:?}");
}

// ---------------------------------------------------------------------------
// The ADR 0131 probe matrix (stdlib/test/adr0131local_rebind_test.bt)
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// ADR 0131 Phase 4 (BT-3750): `Result tryDo:` as a local-threading producer
// ---------------------------------------------------------------------------

#[test]
fn try_do_is_a_producer_in_every_position_of_every_method_context() {
    for body in [
        // statement, last expression, assignment value, operand
        "Result tryDo: [t := t + 1. 1]\nt",
        "Result tryDo: [t := t + 1. 1]",
        "r := Result tryDo: [t := t + 1. 1]\n#[r isOk, t]",
        "r := (Result tryDo: [t := t + 1. 1]) valueOr: 0\n#[r, t]",
        "r := t + ((Result tryDo: [t := t + 1. 1]) valueOr: 0)\n#[r, t]",
        // a write-only block, two locals, and a nested producer
        "Result tryDo: [t := 5]\nt",
        "Result tryDo: [t := t + 1. u := u + 1. 1]\n#[t, u]",
        "Result tryDo: [#(1, 2) do: [:x | t := t + x]. t]\nt",
        // in the frames of other producers
        "#(1, 2) do: [:k | Result tryDo: [t := t + k. 1]]\nt",
        "(t < 3) ifTrue: [Result tryDo: [t := t + 1. 1]]\nt",
        "[Result tryDo: [t := t + 1. 1]] on: Error do: [:e | 0]\nt",
    ] {
        let body = format!("u := 0\n{body}");
        for src in [in_class(&body), in_value(&body), in_actor(&body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn try_do_that_is_not_a_producer_keeps_the_allow_set_error() {
    // A `tryDo:` whose Tier 2 block reaches the arity-0 native raises, so
    // wherever the producer does not lower it the write is a compile error:
    // inside a closure, as the argument of a send that does not sequence its
    // operands, and on a receiver other than the `Result` class.
    for body in [
        "CvA ap: [Result tryDo: [t := t + 1. 1]]\nt",
        "r := Set tryDo: [t := t + 1. 1]\nt",
    ] {
        for src in [in_class(body), in_value(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(
                diags.iter().any(|d| d.severity == Severity::Error),
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn a_try_do_sent_to_another_receiver_takes_the_ordinary_section6_path() {
    // Only `Result tryDo:` is a §1 construct. An actor's own `tryDo:` taking
    // the block is a plain `self` send, so the block escapes (ADR 0133),
    // whatever the callee does with it.
    let actor = |callee: &str| {
        format!(
            "Actor subclass: Ac\n  state: n = 0\n\n{callee}\n\n  probe =>\n    t := 0\n    self tryDo: [t := t + 1]\n    t\n"
        )
    };
    for callee in [
        "  sealed tryDo: b => b value",
        "  sealed tryDo: b => #(1) do: [:x | b value]",
    ] {
        let src = actor(callee);
        assert_eq!(section6_errors(&src), 1, "{src}");
    }
}

#[test]
fn a_try_do_assigned_in_an_actor_fold_body_keeps_the_phase0_error() {
    // The value of `r := Result tryDo: [...]` in the body of an actor `do:` is a
    // value-bearing site the fold frame does not lower (BT-3797), so it is
    // rejected rather than compiled to a dropped write; the statement form and
    // a lookup behind `on:do:` are accepted and pinned at runtime by
    // `result_try_do_tier2test.bt` / `result_try_do_tier2matrix_test.bt`.
    let rejected = in_actor(
        "#(1, 2) do: [:k | r := Result tryDo: [#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]. 2]]\nt",
    );
    let diags = adr0131_diagnostics(&rejected);
    assert!(
        of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading)
            .iter()
            .any(|d| d.message.contains("`tryDo:` writes outer local `t`")),
        "{diags:?}"
    );
    for body in [
        "#(1, 2) do: [:k | Result tryDo: [#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]. 2]]\nt",
        "#(1, 2) do: [:k | [#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]] on: Error do: [:e | 0]]\nt",
    ] {
        let diags = adr0131_diagnostics(&in_actor(body));
        assert!(diags.is_empty(), "{body}: {diags:?}");
    }
}

#[test]
fn try_do_with_a_block_that_writes_no_local_is_not_a_construct_that_threads() {
    // The block is Tier 1 and reaches `'tryDo:'/1`: nothing to thread, nothing
    // to reject, in a position that would reject a writing one.
    let src = in_class("CvA ap: [Result tryDo: [1]]\nt");
    assert!(adr0131_diagnostics(&src).is_empty());
}

/// PIN-BUG BT-3841: every method in the inert `*_pending.bt.pending` probe
/// fixtures is rejected by an ADR 0131 diagnostic today, and no method in the
/// compiled fixtures is. A phase of the BT-3743 epic that makes a shape
/// compile moves it out of the pending fixture (and its test into
/// `adr0131local_rebind_test.bt`), which keeps this green; BT-3841 deletes the
/// allow-set and with it the pending allow-set shapes.
#[test]
fn adr0131_probe_matrix_pins() {
    // The contexts whose last pending shape was lowered, so they have no
    // pending fixture left (the actor column's last shape, s1, moved to the
    // compiled fixture with BT-3798). Pinned explicitly (BT-3855): a context
    // not listed must have its pending fixture, so deleting one by accident
    // fails here instead of skipping the context.
    const NO_PENDING_FIXTURE: &[&str] = &["class", "value", "actor"];
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    let read = |name: &str| {
        std::fs::read_to_string(root.join(name)).unwrap_or_else(|e| panic!("{name}: {e}"))
    };
    let mut moved = Vec::new();
    for context in ["class", "value", "actor"] {
        let compiled = format!("adr0131local_rebind_{context}.bt");
        let diags = adr0131_diagnostics(&read(&compiled));
        assert!(diags.is_empty(), "{compiled}: {diags:?}");

        let pending = format!("adr0131local_rebind_{context}_pending.bt.pending");
        if NO_PENDING_FIXTURE.contains(&context) {
            assert!(
                !root.join(&pending).exists(),
                "{pending} exists, but `{context}` is pinned as having no pending \
                 fixture: drop it from NO_PENDING_FIXTURE"
            );
            continue;
        }
        let src = read(&pending);
        let module = parse_module(&src);
        let diags = adr0131_diagnostics(&src);
        let class = &module.classes[0];
        let methods = class.methods.iter().chain(class.class_methods.iter());
        let mut count = 0;
        let mut accepted = Vec::new();
        for method in methods {
            count += 1;
            if !diags
                .iter()
                .any(|d| method.span.start() <= d.span.start() && d.span.end() <= method.span.end())
            {
                accepted.push(method.selector.name().to_string());
            }
        }
        if !accepted.is_empty() {
            moved.push(format!("{pending}: {accepted:?}"));
        }
        assert!(count > 0, "{pending} has no methods");
    }
    assert!(
        moved.is_empty(),
        "no longer rejected by an ADR 0131 diagnostic; move them (and their tests) out of \
         the pending files:\n{}",
        moved.join("\n")
    );
}

/// ADR 0131 probe s4 (a stored closure invoked directly) was a permanent §6
/// error until ADR 0132 Phase 0b (I5, BT-3867) evaluated the stored block in
/// place in class and value-type methods; ADR 0133 makes it the escaping-block
/// error again, like a stored block passed as an argument (I4/I6).
#[test]
fn adr0131_probe_s4_is_an_escaping_block() {
    for src in [
        "Object subclass: P\n  class s4 =>\n    t := 0\n    b := [t := t + 1]\n    b value\n    t\n",
        "Value subclass: P\n  s4 =>\n    t := 0\n    b := [t := t + 1]\n    b value\n    t\n",
        "Object subclass: P\n  class s4 =>\n    t := 0\n    b := [t := t + 1]\n    P hom: b\n    t\n",
        "Value subclass: P\n  s4 =>\n    t := 0\n    b := [t := t + 1]\n    #(1) do: b\n    t\n",
    ] {
        let diags = adr0131_diagnostics(src);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
            1,
            "{src}: {diags:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// PR #4210 review: cascades, parenthesized receivers, scoping, destructuring
// ---------------------------------------------------------------------------

#[test]
fn section6_cascade_messages_are_never_inlined() {
    // `generate_cascade` sends every message through ordinary dispatch, so a
    // `do:` block in a cascade drops its write, in either message order.
    for body in [
        "#(1, 2) yourself; do: [:x | t := t + x]\nt",
        "#(1, 2) do: [:x | t := t + x]; yourself\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn section6_cascade_block_receiver_is_reported_once() {
    let src = in_class("[t := t + 1] value; value; yourself\nt");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

#[test]
fn section6_parenthesized_block_receiver_is_a_block_value() {
    for body in [
        "([t := t + 1]) value\nt",
        "([t := t + 1. 1]) on: Error do: [:e | 0]\nt",
        "([t := t + 1. t < 3]) whileTrue: [nil]\nt",
    ] {
        // Also in an actor method: codegen's Tier 2 `value` paths only match
        // a bare block, and `([t := t + 1]) value` raises there (probed).
        for src in [in_class(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn section6_stored_block_is_reported_once_whatever_reuses_its_name() {
    // A sibling scope's parameter or a destructuring rebind of the same name
    // does not add a second diagnostic, nor remove the binding's (ADR 0133).
    for src in [
        in_value(
            "#(1) do: [:e | blk := [t := t + 1]. blk]\n#(1, 2) do: [:blk | blk printString]\nt",
        ),
        in_class("b := [t := t + 1]\n#[b, _x] := #[[0], 1]\nb value\nt"),
    ] {
        assert_eq!(section6_errors(&src), 1, "{src}");
    }
}

#[test]
fn value_type_return_of_list_op_is_accepted_like_implicit_last() {
    // BT-3745 probed `^#(1, 2) inject: 0 into: [...]` in a value-type method
    // answering the leaked `{3, StateAcc}` tuple, and pinned it as a Phase 0
    // error. ADR 0131 Phase 2 (BT-3749) lowers the `^` value as a
    // local-threading producer, so it answers 3 like the implicit last
    // statement. The BUnit pins are `vtLastInject` and `vtReturnInject`
    // (adr0131local_rebind_value.bt).
    for body in [
        "^#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
        "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    ] {
        let src = in_value(body);
        assert!(adr0131_diagnostics(&src).is_empty(), "{body}");
    }
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2 (BT-3749): loops and folds as local-threading producers
// ---------------------------------------------------------------------------

#[test]
fn producer_operand_sites_are_accepted_in_every_method_context() {
    for body in [
        // o4, s9: a fold as a message receiver.
        "r := (#(1, 2) collect: [:x | t := t + x]) size\n#[r, t]",
        "r := (#(1, 2) reject: [:x | t := t + 1. false]) size\n#[r, t]",
        // A fold as an argument, after a read of `t` (ADR 0131 §1a).
        "r := t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])\n#[r, t]",
        // Loops as an element of a literal and as a `^` operand.
        "r := #[1 to: 3 do: [:i | t := t + i], t]\nr",
        "^(#(1, 2) detect: [:x | t := t + 1. x > 1]) + t",
        "s := 0\n^#[[s < 3] whileTrue: [s := s + 1. t := t + s], t]",
        // A plain statement whose argument is a fold.
        "#(1) add: (#(1, 2) count: [:x | t := t + 1. true])\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn producer_sites_stop_at_blocks_conditionals_and_unthreaded_writes() {
    for body in [
        // A conditional's receiver is threaded by the conditional (Phase 3).
        "(#(1, 2) collect: [:x | t := t + x]) isEmpty ifTrue: [0]\nt",
        // Under a cascade.
        "(#(1, 2) collect: [:x | t := t + x]) size; yourself\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len()
                    + of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2d (BT-3795): the REPL root and field-write values
// ---------------------------------------------------------------------------

#[test]
fn repl_operand_sites_are_producers() {
    let known = ["t", "u"];
    for src in [
        // o4, s9: a fold as a message receiver, bare and as an assignment's
        // value.
        "(#(1, 2) collect: [:x | t := t + x]) size\n",
        "r := (#(1, 2) reject: [:x | t := t + 1. false]) size\n",
        // A fold as an argument, after a read of `t` (ADR 0131 §1a).
        "r := t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])\n",
        "t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])\n",
        // A loop as an element of a literal.
        "#[1 to: 3 do: [:i | t := t + i], t]\n",
        "#[[t < 3] whileTrue: [t := t + 1], t]\n",
        // A fold in the receiver of the fold that is the whole value.
        "r := (#(1, 2) collect: [:x | t := t + x]) inject: 0 into: [:a :x | u := u + 1. a + x]\n",
        // A handler-writing `detect:ifNone:`.
        "(#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]) + 1\n",
        // Several statements in one input.
        "t := 0\nr := (#(1, 2) collect: [:x | t := t + x]) size\n#[r, t]\n",
        // BT-3823: at an operand site of a REPL arm (a conditional arm, an
        // `on:do:` block), where the producer adopts the bindings map.
        "true ifTrue: [(#(1, 2) collect: [:x | t := t + x]) size]\n",
        "r := [(#(1, 2) collect: [:x | t := t + x]) size] on: Error do: [:e | 0]\n",
        "r := [1 / 0] on: Error do: [:e | [:k | t := t + k] value: 4. t]\n",
        // An evaluated block is a REPL arm too.
        "[(#(1, 2) collect: [:x | t := t + x]) size] value\n",
    ] {
        let diags = adr0131_diagnostics_with(src, &known);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

#[test]
fn repl_producer_sites_stop_at_blocks_conditionals_and_statement_constructs() {
    let known = ["t", "u"];
    for src in [
        // Inside a block that is not a REPL arm: a loop body, a stored block.
        "#(1) do: [:z | (#(1, 2) collect: [:x | t := t + x]) size]\n",
        "b := [(#(1, 2) collect: [:x | t := t + x]) size]\n",
        // A conditional's receiver is threaded by the conditional (Phase 3).
        "(#(1, 2) collect: [:x | t := t + x]) isEmpty ifTrue: [0]\n",
        // Under a cascade.
        "(#(1, 2) collect: [:x | t := t + x]) size; yourself\n",
        // The receiver of a construct that is itself the statement: the
        // statement lowering compiles its own operands.
        "(#(1, 2) collect: [:x | t := t + x]) do: [:e | u := u + e]\n",
        // A destructuring's value.
        "#(a, b) := (#(1, 2) collect: [:x | t := t + x]) asArray\n",
        // A `self.f :=` has no REPL meaning.
        "self.n := (#(1, 2) collect: [:x | t := t + x]) size\n",
    ] {
        let diags = adr0131_diagnostics_with(src, &known);
        assert!(
            !of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn field_write_value_sites_are_producers_in_class_and_actor_methods() {
    for value in [
        "(#(1, 2) collect: [:x | t := t + x]) size",
        "t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])",
        "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
        "(#(1, 3) collect: [:e | t := t + 1. e]) inject: 0 into: [:a :e | u := u + 1. a + e]",
        "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]",
        "0 max: ((1 to: 3 do: [:i | t := t + i]) isNil ifTrue: [1] ifFalse: [2])",
    ] {
        let class = format!(
            "Object subclass: Cm\n  classState: n = 0\n\n  class probe =>\n    t := 0\n    u := 0\n    self.n := {value}\n    #[self.n, t, u]\n"
        );
        let actor = format!(
            "Actor subclass: Ac\n  state: n = 0\n\n  probe =>\n    t := 0\n    u := 0\n    self.n := {value}\n    #[self.n, t, u]\n"
        );
        for src in [class, actor] {
            let diags = adr0131_diagnostics(&src);
            // The last value is a conditional's receiver: still rejected.
            let expect_rejected = value.contains("ifTrue:");
            assert_eq!(
                !of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
                expect_rejected,
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn field_write_value_sites_stop_at_returns_assignments_and_blocks() {
    for body in [
        // The value of a `^` write is not a method-body site.
        "^self.n := (#(1, 2) collect: [:x | t := t + x]) size",
        // A write nested in a local assignment's value or another write's.
        "r := (self.n := (#(1, 2) collect: [:x | t := t + x]) size)\nr",
        "self.n := (self.n := (#(1, 2) collect: [:x | t := t + x]) size)\nt",
        // A cascade on the value.
        "self.n := (#(1, 2) collect: [:x | t := t + x]) size; yourself\nt",
    ] {
        let class = format!(
            "Object subclass: Cm\n  classState: n = 0\n\n  class probe =>\n    t := 0\n{}\n",
            indent(body)
        );
        let actor = format!(
            "Actor subclass: Ac\n  state: n = 0\n\n  probe =>\n    t := 0\n{}\n",
            indent(body)
        );
        for src in [class, actor] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn field_write_value_sites_include_the_statements_of_an_arm_and_a_loop_body() {
    // ADR 0131 Phase 3b (BT-3817): the frame's field-write lowering splices the
    // value's prelude ahead of the write, so a producer in the value of a
    // field write inside an arm or a loop body is lowered (`fieldValueInArm`,
    // `fieldValueInLoop` in `adr0131exc_producer_test.bt`).
    for body in [
        "true ifTrue: [self.n := (#(1, 2) collect: [:x | t := t + x]) size]\nt",
        "1 to: 2 do: [:i | self.n := (#(1, 2) collect: [:x | t := t + x]) size]\nt",
    ] {
        let class = format!(
            "Object subclass: Cm\n  classState: n = 0\n\n  class probe =>\n    t := 0\n{}\n",
            indent(body)
        );
        let actor = format!(
            "Actor subclass: Ac\n  state: n = 0\n\n  probe =>\n    t := 0\n{}\n",
            indent(body)
        );
        for src in [class, actor] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn field_write_in_a_value_type_is_still_a_hard_error() {
    // A value type cannot write `self.n :=` (ADR 0042), so codegen never sees
    // a field-write value site in one.
    let src = "Value subclass: Vt\n  field: n = 0\n\n  probe =>\n    t := 0\n    self.n := (#(1, 2) collect: [:x | t := t + x]) size\n    t\n";
    let diags = analyse_full(&parse_module(src), AnalysisContext::default()).diagnostics;
    assert!(
        diags
            .iter()
            .any(|d| d.severity == Severity::Error && d.message.contains("value type")),
        "{diags:?}"
    );
}

// ---------------------------------------------------------------------------
// BT-3753: statement positions that lose the write
// ---------------------------------------------------------------------------

#[test]
fn deny_set_nested_statement_names_the_enclosing_block() {
    let src = in_class("4 > 3 ifTrue: [[:v | t := t + v] value: 1]\nt");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert_eq!(
        p0[0].message.as_str(),
        "`value:` writes outer local `t`, but as a statement inside a conditional arm \
         in a class method the write is not threaded back yet (BT-3743)"
    );
    assert!(
        p0[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("compute the new value of `t`")),
        "{:?}",
        p0[0]
    );
}

#[test]
fn deny_set_method_body_statement_is_rejected_per_context() {
    // A `[...] value` loses the write as a class method statement and answers
    // right in an actor method.
    let body = "[:v | t := t + v] value: 1\nt";
    let diags = adr0131_diagnostics(&in_class(body));
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert!(p0[0].message.contains("as a statement in"), "{:?}", p0[0]);
    assert!(adr0131_diagnostics(&in_actor(body)).is_empty());
}

/// BT-3841: `eachWithIndex:`/`do:separatedBy:` are producers like `do:`
/// (`producer_body_arg_index`, the separator packed as a handler): accepted as
/// a statement, an operand, in an arm and in a loop body in every method
/// context.
#[test]
fn enumerations_are_producers_where_the_frame_lowers_them() {
    for construct in [
        "#(1, 2) eachWithIndex: [:x :i | t := t + x]",
        "#(1, 2) do: [:x | t := t + x] separatedBy: [t := t + 10]",
    ] {
        for body in [
            format!("t := 0\n{construct}\nt"),
            format!("t := 0\nr := {construct}\n#[r, t]"),
            format!("t := 0\ntrue ifTrue: [{construct}]\nt"),
            format!("t := 0\n#(1, 2) do: [:z | {construct}]\nt"),
            format!("t := 0\n[{construct}] on: Error do: [:e | 0]\nt"),
        ] {
            for src in [in_class(&body), in_value(&body), in_actor(&body)] {
                let diags = adr0131_diagnostics(&src);
                // An actor `r :=` of a `do:` body operand is the one shape the
                // actor frames do not lower (erlc), see the operand matrix.
                assert!(
                    diags.is_empty() || src.contains("Actor subclass"),
                    "{src}: {diags:?}"
                );
            }
        }
    }
}

/// BT-3776: a construct that writes only a local of its own enclosing block
/// (`crosses: false`) is judged by the measurement like every other cell. It
/// answers right, and is accepted, everywhere: an actor `detect:` /
/// `detect:ifNone:` in a protected body, a handler or a loop body read the
/// local unchanged or failed erlc until ADR 0133 Phase 3 (BT-3911, closing
/// BT-3878) lowered it as a producer in those frames (the `deny_block_local`
/// rows are gone).
#[test]
fn block_local_writes_are_accepted_in_every_frame() {
    let collect =
        "4 > 3 ifTrue: [\n  u := 0\n  #(1, 2) collect: [:i | u := u + 1. i]\n  t := t + u\n]\nt";
    for src in [in_class(collect), in_value(collect), in_actor(collect)] {
        assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
    }
    // A local bound inside the search block itself is not a write of the
    // construct.
    let own = "i := 0\n[i < 2] whileTrue: [i := i + 1. #(1, 2) detect: [:e | v := e * 2. v > 5] ifNone: [nil]]\nt";
    assert!(adr0131_diagnostics(&in_actor(own)).is_empty(), "{own}");
    let detect = "#(1, 2) detect: [:e | u := u + 1. e > 5] ifNone: [nil]";
    for body in [
        format!("4 > 3 ifTrue: [u := 0. {detect}. t := u]\nt"),
        format!("#(1, 2) do: [:z | u := 0. {detect}. t := u]\nt"),
        format!("[u := 0. {detect}. t := u] on: Error do: [:e | 0]\nt"),
        format!("[Error signal: \"x\"] on: Error do: [:e | u := 0. {detect}. t := u]\nt"),
        format!("2 timesRepeat: [u := 0. {detect}. t := u]\nt"),
        // A loop as the innermost block, under an arm or a `do:` body.
        format!("4 > 3 ifTrue: [2 timesRepeat: [u := 0. {detect}. t := u]]\nt"),
        format!(
            "#(1, 2) do: [:z | i := 0. [i < 2] whileTrue: [i := i + 1. u := 0. {detect}. t := u]]\nt"
        ),
        format!("2 timesRepeat: [#(1, 2) do: [:z | u := 0. {detect}. t := u]]\nt"),
        format!(
            "i := 0\n[i < 2] whileTrue: [i := i + 1. 4 > 3 ifTrue: [u := 0. {detect}. t := u]]\nt"
        ),
        format!("i := 0\n[i < 2] whileTrue: [i := i + 1. u := 0. {detect}. t := u]\nt"),
    ] {
        for src in [in_class(&body), in_value(&body), in_actor(&body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn a_nested_do_in_a_class_loop_body_is_accepted_with_or_without_state_access() {
    // A `do:` in a `to:do:` body used to lose the write unless the loop body
    // touched the class's state (class_method_protected_locals.bt
    // `nestedDoInACountedLoop`, probed). ADR 0131 Phase 2c (BT-3796) lowers it
    // as a local-threading producer in the enclosing frame, so both answer
    // right (`doInToDo`, `doInToDoStateful` in
    // `adr0131loop_body_producer_test.bt`).
    for body in [
        "1 to: 2 do: [:i | #(1, 2) do: [:j | t := t + j]]\nt",
        "1 to: 2 do: [:i | #(1, 2) do: [:j | t := t + j. self ap: [nil]]]\nt",
    ] {
        let diags = adr0131_diagnostics(&in_class(body));
        assert!(
            of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
            "{body}: {diags:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// BT-3753: the statement-position probe matrix (`DENY_SET`)
// ---------------------------------------------------------------------------

use crate::test_helpers::statement_probes::{
    Matrix, ProbeRow, class_source, probe_class_source, probe_method, probe_rows, statement_probe,
};

/// The ADR 0131 diagnostics the probe `(context, container, target, kind)`
/// draws (`repl` rows run as module-level expressions).
fn probe_diagnostics(context: &str, container: &str, target: &str, kind: &str) -> Vec<Diagnostic> {
    if context == "repl" {
        let body = statement_probe(kind, container, target, "4 =:= 4");
        adr0131_diagnostics_with(&format!("{body}\n"), &["t"])
    } else {
        adr0131_diagnostics(&probe_class_source(context, "P", container, target, kind))
    }
}

/// PIN-BUG BT-3841: the statement-position probe matrix, measured on the
/// real build (BT-3753). Every shape `adr0131_statement_probes.tsv` records
/// as answering wrong, raising or failing to compile (`bad`) is rejected by
/// an ADR 0131 diagnostic, and every shape it records as answering right
/// (`ok`) is not, except where a [`DENY_SET`] row is coarser than one
/// measurement (`over`: the row's kind family or block role also covers
/// shapes measured wrong). The block-local cells (targets `local` and
/// `dlocal`, a construct that writes only a local of its own enclosing block)
/// were `gap` lines until BT-3776 measured them and flipped them to `ok`,
/// `bad` or `over`; the only `gap` lines left are two REPL method-body rows
/// (unmeasured, BT-3823), which this test does not judge. A phase that makes a shape answer right
/// deletes or narrows its row and flips its line to `ok`, which keeps this
/// green.
#[test]
fn adr0131_statement_probe_pins() {
    let mut checked = 0;
    let mut wrong = Vec::new();
    for ProbeRow {
        context,
        container,
        target,
        kind,
        verdict,
    } in probe_rows()
    {
        let rejected = !probe_diagnostics(&context, &container, &target, &kind).is_empty();
        let expected_rejected = match verdict.as_str() {
            "bad" | "over" => Some(true),
            "ok" => Some(false),
            "gap" => None,
            other => panic!("unknown verdict {other}"),
        };
        if expected_rejected.is_some_and(|e| e != rejected) {
            wrong.push(format!(
                "{context}\t{container}\t{target}\t{kind}\tmeasured {verdict}, but {}",
                if rejected { "rejected" } else { "accepted" }
            ));
        }
        checked += 1;
    }
    assert!(checked > 1000, "only {checked} probes");
    assert!(
        wrong.is_empty(),
        "{} mismatches:\n{}",
        wrong.len(),
        wrong.join("\n")
    );
}

/// Cells that answer right but that Phase 0 still rejects, because the facts a
/// statement position carries cannot tell them from a sibling cell that
/// answers wrong. Empty since ADR 0133 Phase 3 (BT-3911): an actor
/// `detect:`/`detect:ifNone:` is a loop-body producer like any other fold, so
/// the last cells of this list (the `detect:` family's block-local rows) answer
/// right and are accepted. A shape that answers right but cannot be told from
/// one that does not goes back here with a `PIN-BUG` and an open issue.
const KNOWN_OVER_REJECTED: &[(&str, &str, &str, &str)] = &[];

/// PIN-BUG BT-3841: Phase 0 accepts exactly what codegen lowers correctly.
/// `adr0131_statement_measured.tsv` records, for every class, value-type and
/// actor cell the check once rejected, what the cell answers when compiled
/// with the check bypassed and run on a BEAM (regenerate it with `just
/// measure-adr0131-statements`, which fails when the numbers drift). Both
/// directions are asserted: a cell that answers `wrong` or fails `erlc` must
/// be rejected (soundness: no silent wrong answer), and a cell that answers
/// `right` must be accepted (the table is not coarser than the measurement)
/// except the [`KNOWN_OVER_REJECTED`] ones, and the verdict table agrees with
/// the measurement.
#[test]
fn adr0131_phase0_agrees_with_measured_answers() {
    let verdicts: std::collections::HashMap<(String, String, String, String), String> =
        probe_rows()
            .into_iter()
            .map(|r| ((r.context, r.container, r.target, r.kind), r.verdict))
            .collect();
    let mut problems = Vec::new();
    let measured = Matrix::Statement.measured();
    assert!(
        measured.len() >= 733,
        "only {} measured cells",
        measured.len()
    );
    for [context, container, target, kind, answer] in measured {
        let right = match answer.as_str() {
            "right" => true,
            "wrong" | "erlc" => false,
            other => panic!("unknown answer {other}"),
        };
        let rejected = !probe_diagnostics(&context, &container, &target, &kind).is_empty();
        let cell = format!("{context}\t{container}\t{target}\t{kind}");
        let over_rejected = KNOWN_OVER_REJECTED.contains(&(
            context.as_str(),
            container.as_str(),
            target.as_str(),
            kind.as_str(),
        ));
        // A known over-rejected cell answers right and is still rejected; the
        // phase that lowers it deletes its entry.
        let agrees = if over_rejected {
            rejected && right
        } else {
            rejected != right
        };
        if !agrees {
            problems.push(format!(
                "{cell}\tmeasured {answer}, but Phase 0 {}",
                match (rejected, over_rejected) {
                    (true, false) => "rejects it (coarser than the measurement)",
                    (false, false) => "accepts it (a silent wrong answer or a compile failure)",
                    (_, true) => "no longer rejects it (delete it from KNOWN_OVER_REJECTED)",
                }
            ));
        }
        match verdicts
            .get(&(context, container, target, kind))
            .map(String::as_str)
        {
            Some("ok") if !right => problems.push(format!("{cell}\tverdict ok, measured {answer}")),
            Some("bad") if right => {
                problems.push(format!("{cell}\tverdict bad, measured right"));
            }
            Some("over") if !over_rejected => {
                problems.push(format!(
                    "{cell}\tverdict over, but not in KNOWN_OVER_REJECTED"
                ));
            }
            Some("ok" | "bad" | "over") => {}
            other => problems.push(format!("{cell}\tno usable verdict line ({other:?})")),
        }
    }
    assert!(
        problems.is_empty(),
        "{} disagreements:\n{}",
        problems.len(),
        problems.join("\n")
    );
}

/// The ADR 0131 diagnostics the operand-matrix probe `(context, container,
/// position, kind)` draws.
fn operand_diagnostics(
    context: &str,
    container: &str,
    position: &str,
    kind: &str,
) -> Vec<Diagnostic> {
    let method = probe_method(
        Matrix::Operand,
        context,
        "probe:",
        container,
        position,
        kind,
    );
    adr0131_diagnostics(&class_source(context, "P", &[method]))
}

/// PIN-BUG BT-3841: the operand-position half of Phase 0 (`ALLOW_SET`) is
/// sound. `adr0131_operand_measured.tsv` records what a construct answers as
/// the value of an assignment, a receiver, an argument, a cascade part, a
/// literal element, a `match:` subject or an interpolation, with the check
/// bypassed and run on a BEAM; every cell that does not answer right must be
/// rejected. Not every right cell is accepted (`ALLOW_SET` is deliberately
/// coarse here): the count of those is the BT-3841 backlog, not asserted.
#[test]
fn adr0131_phase0_operand_positions_are_sound() {
    let measured = Matrix::Operand.measured();
    assert!(measured.len() > 1000, "only {} cells", measured.len());
    let mut unsound = Vec::new();
    for [context, container, position, kind, answer] in measured {
        let right = match answer.as_str() {
            "right" => true,
            "wrong" | "erlc" => false,
            other => panic!("unknown answer {other}"),
        };
        let rejected = !operand_diagnostics(&context, &container, &position, &kind).is_empty();
        if !right && !rejected {
            unsound.push(format!(
                "{context}\t{container}\t{position}\t{kind}\t{answer}"
            ));
        }
    }
    assert!(
        unsound.is_empty(),
        "{} operand cells answer wrong but compile:\n{}",
        unsound.len(),
        unsound.join("\n")
    );
}

/// The BT-3841 summary: for the statement matrix, how many cells answer right,
/// wrong or fail to compile, by family, by block-role group and by context; for
/// the operand matrix, the same by position plus what Phase 0 does with them.
fn phase0_summary() -> String {
    use crate::test_helpers::statement_probes::{container_group, statement_family};
    use std::collections::BTreeMap;
    let mut text = String::from(
        "# ADR 0131 Phase 0 measurement summary (BT-3841), generated from the two measured\n\
         # tables by `adr0131_phase0_summary_is_checked_in`; regenerate with\n\
         # `ADR0131_BLESS=1 cargo test -p beamtalk-core --lib adr0131_phase0_summary`.\n\
         # statement<TAB>family<TAB>context<TAB>right<TAB>wrong<TAB>erlc\n\
         # statement-group<TAB>group<TAB>family<TAB>context<TAB>not-right cells\n\
         # operand<TAB>position<TAB>context<TAB>cells<TAB>right<TAB>wrong<TAB>erlc<TAB>phase0-accepts<TAB>phase0-rejects-right\n",
    );
    let mut by_family: BTreeMap<(String, String), [usize; 3]> = BTreeMap::new();
    let mut by_group: BTreeMap<(String, String, String), usize> = BTreeMap::new();
    for [context, container, _target, kind, answer] in Matrix::Statement.measured() {
        let slot = match answer.as_str() {
            "right" => 0,
            "wrong" => 1,
            _ => 2,
        };
        let family = statement_family(&container, &kind);
        by_family
            .entry((family.to_string(), context.clone()))
            .or_default()[slot] += 1;
        if slot != 0 {
            *by_group
                .entry((
                    container_group(&container).to_string(),
                    family.to_string(),
                    context,
                ))
                .or_default() += 1;
        }
    }
    for ((family, context), [r, w, e]) in &by_family {
        let _ = writeln!(text, "statement\t{family}\t{context}\t{r}\t{w}\t{e}");
    }
    for ((group, family, context), n) in &by_group {
        let _ = writeln!(text, "statement-group\t{group}\t{family}\t{context}\t{n}");
    }
    let mut by_position: BTreeMap<(String, String), [usize; 6]> = BTreeMap::new();
    for [context, container, position, kind, answer] in Matrix::Operand.measured() {
        let rejected = !operand_diagnostics(&context, &container, &position, &kind).is_empty();
        let slot = match answer.as_str() {
            "right" => 1,
            "wrong" => 2,
            _ => 3,
        };
        let row = by_position.entry((position, context)).or_default();
        row[0] += 1;
        row[slot] += 1;
        if !rejected {
            row[4] += 1;
        } else if slot == 1 {
            row[5] += 1;
        }
    }
    for ((position, context), [n, r, w, e, acc, over]) in &by_position {
        let _ = writeln!(
            text,
            "operand\t{position}\t{context}\t{n}\t{r}\t{w}\t{e}\t{acc}\t{over}"
        );
    }
    text
}

/// PIN-BUG BT-3841: the checked-in summary of the measurement
/// (`adr0131_phase0_summary.tsv`) is the one the measured tables and Phase 0
/// produce now; the family counts in the PR and the ADR are read off it.
#[test]
fn adr0131_phase0_summary_is_checked_in() {
    let summary = phase0_summary();
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src/semantic_analysis/tests/adr0131_phase0_summary.tsv");
    if std::env::var_os("ADR0131_BLESS").is_some() {
        std::fs::write(&path, &summary).expect("write the summary");
        return;
    }
    assert!(
        summary == include_str!("adr0131_phase0_summary.tsv").replace("\r\n", "\n"),
        "the summary drifted; regenerate with ADR0131_BLESS=1:\n{summary}"
    );
}

/// BT-3841 review: the unfinished families (`detect:ifNone:` and a block
/// `value`; `anySatisfy:`, the keyed iterations and the enumerations were here
/// until their producers landed) are rejected past depth two unless a row names the exact
/// shape. An `eachWithIndex:` in an arm in a protected body in a `do:` body
/// (depth 3) is unmeasured and stays rejected in a value-type method, as does
/// depth four in every context. (This chain used to be accepted for
/// `anySatisfy:` by the coarse rows and raised `bad argument` in a value-type
/// method, measured on a BEAM; the producer now answers 4 in all three.)
#[test]
fn unfinished_families_are_rejected_past_the_measured_depth() {
    let header = |context: &str| match context {
        "class" => {
            "Object subclass: P\n  classState: n = 0\n  class plain => 0\n  class probe: c =>\n"
        }
        "value" => "Value subclass: P\n  plain => 0\n  probe: c =>\n",
        _ => "Actor subclass: P\n  state: n = 0\n  plain => 0\n  probe: c =>\n",
    };
    let source = |context: &str, body: &str| {
        let lines = body
            .lines()
            .fold(String::new(), |acc, l| acc + "    " + l + "\n");
        format!("{}{lines}", header(context))
    };
    let depth3 = "t := 0\n#(1, 2) do: [:z | [c ifTrue: [[:v | t := t + v] value: 1]] on: Error do: [:e | 0]]\nt";
    assert!(
        !adr0131_diagnostics(&source("value", depth3)).is_empty(),
        "the value-type depth-3 shape is unmeasured and must be rejected"
    );
    let depth4 = "t := 0\n#(1, 2) do: [:y | #(1, 2) do: [:z | [c ifTrue: [[:v | t := t + v] value: 1]] on: Error do: [:e | 0]]]\nt";
    for context in ["class", "value", "actor"] {
        assert!(
            !adr0131_diagnostics(&source(context, depth4)).is_empty(),
            "{context}: depth-4 shape accepted"
        );
    }
}

/// BT-3841 review: unmeasured middle roles of a three-block nesting stay
/// rejected for the unfinished families, whatever the outermost and innermost
/// roles are (no `RIGHT_STATEMENTS` row exists past depth two). (An evaluated
/// literal block is not one of them since BT-3841: it is a producer by rule at
/// any depth, `evaluated_literal_blocks_are_producers_at_any_depth`.)
#[test]
fn unmeasured_three_deep_chains_stay_rejected() {
    for (context, src) in [
        (
            "actor",
            in_actor(
                "t := 0\n#(1, 2) do: [:z | true ifTrue: [[:v | t := t + v] value: 1]]\n#(1, 2) do: [:z | [#(1, 2) do: [:y | true ifTrue: [[:v | t := t + v] value: 1]]] on: Error do: [:e | 0]]\nt",
            ),
        ),
        (
            "actor",
            in_actor(
                "t := 0\ntrue ifTrue: [#(1, 2) do: [:z | true ifTrue: [[:v | t := t + v] value: 1]]]\nt",
            ),
        ),
        (
            "actor",
            in_actor(
                "t := 0\n#(1, 2) do: [:z | [Error signal: \"x\"] on: Error do: [:e | true ifTrue: [[:v | t := t + v] value: 1]]]\nt",
            ),
        ),
    ] {
        let diags = adr0131_diagnostics(&src);
        assert!(!diags.is_empty(), "{context}: accepted:\n{src}");
    }
}

/// BT-3841: `keysAndValuesDo:`/`doWithKey:` are producers like `do:`. Accepted
/// as a statement, in an arm, in a loop body and in an evaluated literal block
/// (`[...] value`, an arm frame) in every method context; a `^` of one in a plain
/// closure is rejected like `^xs do: [...]` there.
#[test]
fn keyed_iterations_are_producers_where_the_frame_lowers_them() {
    for sel in ["keysAndValuesDo:", "doWithKey:"] {
        let kv = format!("#{{#a => 1}} {sel} [:k :v | t := t + v]");
        for body in [
            format!("t := 0\n{kv}\nt"),
            format!("t := 0\ntrue ifTrue: [{kv}]\nt"),
            format!("t := 0\n#(1, 2) do: [:z | {kv}]\nt"),
            format!("t := 0\n[{kv}] on: Error do: [:e | 0]\nt"),
            format!("t := 0\n[{kv}] value\nt"),
            format!("t := 0\ntrue ifTrue: [[{kv}] value]\nt"),
        ] {
            for src in [in_class(&body), in_value(&body), in_actor(&body)] {
                let diags = adr0131_diagnostics(&src);
                assert!(diags.is_empty(), "{src}: {diags:?}");
            }
        }
        for body in [format!(
            "t := 0\n#(1, 2) do: [:z | [:y | ^{kv}] value: z]\nt"
        )] {
            for src in [in_class(&body), in_value(&body)] {
                assert!(
                    !adr0131_diagnostics(&src).is_empty(),
                    "{src}: accepted but unmeasured or unlowered"
                );
            }
        }
    }
}

/// BT-3841 review: the inline keyed fold is valid for a `Dictionary` receiver
/// only (`Ets` and any class that defines the selector run their own method
/// with a closure). The one rule the lookups use
/// (`receiver_statically_not_dictionary`) keeps a permanent §6 error for a
/// receiver known not to be one; an unknown receiver compiles and is guarded at
/// run time (`beamtalk_map:pairs_for_threading/2`).
#[test]
fn keyed_iterations_on_a_statically_non_dictionary_receiver_keep_the_section6_error() {
    for sel in ["keysAndValuesDo:", "doWithKey:"] {
        let write = format!("t := 0\nself {sel} [:k :v | t := t + v]\nt");
        for src in [in_class(&write), in_value(&write), in_actor(&write)] {
            let diags = adr0131_diagnostics(&src);
            assert!(
                of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len() == 1,
                "{src}: {diags:?}"
            );
        }
        // A typed non-Dictionary local.
        let typed = format!("t := 0\ne := 3\ne {sel} [:k :v | t := t + v]\nt");
        let diags = adr0131_diagnostics(&in_class(&typed));
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len() == 1,
            "{typed}: {diags:?}"
        );
        // A block that writes a field (object state), not a local, is the same
        // error: the receiver's own method would run it as a closure.
        let field = format!("t := 0\ne := 3\ne {sel} [:k :v | self.n := self.n + v]\nt");
        let diags = adr0131_diagnostics(&in_actor(&field));
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len() == 1,
            "{field}: {diags:?}"
        );
        // ... but only in an actor method, the one context that inlines a
        // state-only keyed block: in a class or value-type method a self-send
        // block on such a receiver dispatches normally and is accepted.
        let selfsend = format!("t := 0\ne := 3\ne {sel} [:k :v | self plain]\nt");
        for src in [in_class(&selfsend), in_value(&selfsend)] {
            assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
        }
        // A class-variable write in the block is written in place (ADR 0130),
        // not threaded, so a class method accepts it too.
        let cvar = format!("t := 0\ne := 3\ne {sel} [:k :v | self.n := self.n + v]\nt");
        let diags = adr0131_diagnostics(&in_class(&cvar));
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
            "{cvar}: {diags:?}"
        );
        let selfrecv = format!("t := 0\nself {sel} [:k :v | self plain]\nt");
        for src in [in_class(&selfrecv), in_value(&selfrecv)] {
            assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
        }
        // A map literal and an unknown receiver are Dictionary-inline.
        for recv in ["#{#a => 1}", "d"] {
            let body = format!("t := 0\nd := #{{#a => 1}}\n{recv} {sel} [:k :v | t := t + v]\nt");
            for src in [in_class(&body), in_value(&body), in_actor(&body)] {
                assert!(adr0131_diagnostics(&src).is_empty(), "{src}");
            }
        }
        // A closure between the frame and the construct (an opaque or stored
        // block) is not a producer frame: the write is rejected there.
        let kv = format!("#{{#a => 1}} {sel} [:k :v | t := t + v]");
        for body in [
            format!("t := 0\nself ap: [{kv}]\nt"),
            format!("t := 0\n#(1) do: [:z | [:y | {kv}] value: z]\nt"),
        ] {
            for src in [in_class(&body), in_value(&body)] {
                assert!(
                    !adr0131_diagnostics(&src).is_empty(),
                    "{src}: a keyed iteration in a closure was accepted"
                );
            }
        }
    }
}

#[test]
fn deny_set_while_loop_in_a_loop_body_is_accepted_everywhere() {
    // BT-3746 follow-up (PR #4230): a `whileTrue:` nested as a statement in a
    // `to:do:` or `do:` body answered 0 instead of 3 in every method context.
    // ADR 0131 Phase 2b (BT-3793) lowers it as a local-threading producer in
    // the enclosing body's frame, so it answers 3 (the BUnit pins are
    // `whileInToDo`, `whileInDo` and `whileFalseInToDo` in
    // `adr0131loop_body_producer_test.bt`).
    for body in [
        "s := 0\n1 to: 3 do: [:i | [s < i] whileTrue: [s := s + 1]]\ns",
        "s := 0\n#(1, 2, 3) do: [:i | [s < i] whileTrue: [s := s + 1]]\ns",
        "s := 0\n1 to: 3 do: [:i | [s >= i] whileFalse: [s := s + 1]]\ns",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

/// ADR 0131 Phase 2b (BT-3793): what the loop-body producer accept does not
/// cover stays a Phase 0 error: a closure around the construct and, in an
/// actor, a value-bearing site (`detect:ifNone:` was on this list until ADR
/// 0133 Phase 3, BT-3911).
#[test]
fn loop_body_producer_accept_stops_where_codegen_does() {
    for (body, expect_error) in [
        // ADR 0131 Phase 3c (BT-3822): an arm between the frame and the
        // construct is a `BranchArm` frame (the conditional is a producer in
        // the loop body), so the construct is a frame site in every context
        // (`whileInArmInToDo` in adr0131loop_body_producer_test.bt).
        (
            "s := 0\n1 to: 3 do: [:i | true ifTrue: [[s < i] whileTrue: [s := s + 1]]]\ns",
            [false, false, false],
        ),
        // A closure between the frame and the construct.
        (
            "s := 0\n1 to: 3 do: [:i | [:k | [s < i] whileTrue: [s := s + 1]] value: 1]\ns",
            [true, true, true],
        ),
        // A search-block write inside a `do:` body answers right in every
        // context (`iter`/`outer`/`dinS` in `adr0131_statement_measured.tsv`;
        // the actor row was wrong until ADR 0133 Phase 3, BT-3911).
        (
            "t := 0\n#(1, 2) do: [:i | #(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [0]]\nt",
            [false, false, false],
        ),
        // ADR 0131 Phase 2c (BT-3796): a nested `do:` is a producer in every
        // context.
        (
            "t := 0\n#(1, 2) do: [:i | #(1, 2) do: [:j | t := t + j]]\nt",
            [false, false, false],
        ),
        // ... as a statement of a letrec loop or a fold body.
        (
            "t := 0\n1 to: 2 do: [:i | #(1, 2) do: [:j | t := t + j]]\nt",
            [false, false, false],
        ),
        // BT-3797: an actor's assigned fold in a `do:` body compiled and then
        // crashed ("expected a Dictionary"); a letrec loop frame
        // (`timesRepeat:`, `to:do:`) lowers it (`assignInjectInTimes`,
        // `assignInjectInToDo` in adr0131loop_body_producer_test.bt).
        (
            "t := 0\n#(1, 2) do: [:i | r := #(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]. t := t + r]\nt",
            [false, false, true],
        ),
        (
            "t := 0\n2 timesRepeat: [r := #(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]. t := t + r]\nt",
            [false, false, false],
        ),
        (
            "t := 0\n1 to: 2 do: [:i | r := #(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]. t := t + r]\nt",
            [false, false, false],
        ),
        // An actor's value-bearing site in a `do:` body.
        (
            "t := 0\n#(1, 2) do: [:i | r := (#(1, 2) collect: [:x | t := t + x]) size. t := t + r]\nt",
            [false, false, true],
        ),
        // BT-3796: and under *any* enclosing actor fold frame, not only the
        // innermost one (a letrec loop inside a `do:` wrote nothing back).
        (
            "t := 0\n#(1, 2) do: [:i | 1 to: 2 do: [:k | r := #[#(1, 2) do: [:x | t := t + x], t]. t := t + r size]]\nt",
            [false, false, true],
        ),
        // A letrec loop frame alone lowers value-bearing sites, in an actor too.
        (
            "t := 0\n1 to: 2 do: [:i | r := #[#(1, 2) do: [:x | t := t + x], t]. t := t + r size]\nt",
            [false, false, false],
        ),
    ] {
        for (src, expect_error) in [in_class(body), in_value(body), in_actor(body)]
            .into_iter()
            .zip(expect_error)
        {
            let diags = adr0131_diagnostics(&src);
            let errors = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len()
                + of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len();
            assert_eq!(errors > 0, expect_error, "{src}: {diags:?}");
        }
    }
}

/// BT-3749 review: a statement whose actor lowering compiles its operands
/// without sequencing them (`super`, `error:`, `fieldAt:put:`, a stored-block
/// `value:` call, a self-send with a block argument) must not make a fold in
/// its operands a producer site: the fold's construct tuple would leak and
/// the write be dropped. They stay Phase 0 errors in every context.
#[test]
fn producer_sites_stop_at_sends_that_do_not_sequence_their_operands() {
    let fold = "(#(1, 2) collect: [:x | t := t + x])";
    for body in [
        format!("r := super respondsTo: {fold} printString\nr"),
        format!("^super respondsTo: {fold} printString"),
        format!("self error: {fold} printString\nt"),
        format!("b := [:x | x]\nr := b value: {fold}\nr"),
        format!("r := self pick: [1] with: {fold}\nr"),
        format!("r := self.blk valueWithArguments: {fold}\nr"),
        format!("^self.blk valueWithArguments: {fold}"),
        format!("self.blk valueWithArguments: {fold}\nt"),
        format!("r := self.blk value: {fold}\nr"),
    ] {
        for src in [in_class(&body), in_value(&body), in_actor(&body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(
                !of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).is_empty(),
                "{src}: {diags:?}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// §6: a Tier 2 block that escapes the method without a send (BT-3756)
// ---------------------------------------------------------------------------

/// The shapes in which a block that writes the method local `t` leaves the
/// method with no send: returned (explicitly, as the last statement, or via a
/// local), stored through a field or class variable, or placed in a collection
/// literal. Probed on a real build before the fix: each compiled, then raised
/// `Cannot call 'value' on Block ... captures mutable state` on the first
/// later `value`, in class, value-type and actor methods, except the actor
/// `b := [...]. self.f := b`, which ran and silently lost the write.
///
/// BT-3806 added the rows from "returned via last-statement assignment" on: a
/// returned value followed through a local assignment, the arms of an inlined
/// conditional, an evaluated literal block, an `on:do:`/`ensure:` body or a
/// `match:` arm, and a local bound more than once. Probed on a real build
/// before the fix (a `BUnit` probe calling `value` twice on the answer): every
/// one raised `Cannot call 'value' on Block via perform: (... the block
/// captures mutable state ...)` in class, value-type and actor methods,
/// except the actor's last-statement `b := [...]`, which answered `nil` (the
/// block was lost, and `nil value` raised `does_not_understand`).
/// Each is `(name, body)`.
const ESCAPE_SHAPES: &[(&str, &str)] = &[
    ("explicit return", "^[t := t + 1]"),
    ("implicit return", "[t := t + 1]"),
    ("returned local", "b := [t := t + 1]\n^b"),
    ("implicit returned local", "b := [t := t + 1]\nb"),
    ("returned in a branch", "true ifTrue: [^[t := t + 1]]\nnil"),
    ("list literal", "#([t := t + 1])"),
    ("array literal", "#[[t := t + 1]]"),
    ("map literal", "#{#a => [t := t + 1]}"),
    ("local in array", "b := [t := t + 1]\n#[b]"),
    ("local in list", "b := [t := t + 1]\n#(b)"),
    (
        "local stored in a field",
        "b := [t := t + 1]\nself.n := b\nnil",
    ),
    ("write-only block", "^[t := 5]"),
    ("parenthesized", "^([t := t + 1])"),
    (
        "returned via last-statement assignment",
        "b := [t := t + 1]",
    ),
    ("returned assignment", "^(b := [t := t + 1])"),
    ("returned unparenthesized assignment", "^b := [t := t + 1]"),
    (
        "local returned through an assignment",
        "b := [t := t + 1]\n^(c := b)",
    ),
    (
        "implicit branch value",
        "true ifTrue: [[t := t + 1]] ifFalse: [[0]]",
    ),
    (
        "returned branch value",
        "^true ifTrue: [[t := t + 1]] ifFalse: [[0]]",
    ),
    ("one-armed branch value", "true ifTrue: [[t := t + 1]]"),
    (
        "nil-test branch value",
        "nil ifNil: [[t := t + 1]] ifNotNil: [:v | [v]]",
    ),
    (
        "nested branch value",
        "true ifTrue: [false ifTrue: [0] ifFalse: [[t := t + 1]]] ifFalse: [0]",
    ),
    (
        "branch assigning the block",
        "true ifTrue: [b := [t := t + 1]] ifFalse: [nil]",
    ),
    (
        "branch value stored in a local",
        "b := true ifTrue: [[t := t + 1]] ifFalse: [[0]]\n^b",
    ),
    ("evaluated block value", "[[t := t + 1]] value"),
    (
        "on:do: body value",
        "[[t := t + 1]] on: Error do: [:e | nil]",
    ),
    (
        "on:do: handler value",
        "[nil] on: Error do: [:e | [t := t + 1]]",
    ),
    ("ensure: body value", "[[t := t + 1]] ensure: [nil]"),
    (
        "match arm value",
        "#a match: [#a -> [t := t + 1]; _ -> nil]",
    ),
    ("reassigned local", "b := nil\nb := [t := t + 1]\n^b"),
    (
        "local in a branch",
        "b := [t := t + 1]\ntrue ifTrue: [b] ifFalse: [nil]",
    ),
    // BT-3859: a local that aliases a block-valued local, directly or through
    // a conditional arm, counts as that block.
    ("returned alias", "b := [t := t + 1]\nc := b\n^c"),
    ("alias in a literal", "b := [t := t + 1]\nc := b\n#[c]"),
    (
        "alias of a branch value",
        "b := true ifTrue: [[t := t + 1]] ifFalse: [nil]\nc := b\n^c",
    ),
    (
        "alias through a branch",
        "b := [t := t + 1]\nc := true ifTrue: [b] ifFalse: [nil]\n^c",
    ),
    (
        "alias of an arm-local binding",
        "c := true ifTrue: [b := [t := t + 1]. b] ifFalse: [nil]\n^c",
    ),
    ("alias as the last statement", "b := [t := t + 1]\nc := b"),
    (
        "branch value as the last statement",
        "b := true ifTrue: [[t := t + 1]] ifFalse: [[0]]",
    ),
    (
        "reassigned local as the last statement",
        "b := nil\nb := [t := t + 1]",
    ),
];

/// The [`ESCAPE_SHAPES`] whose block is the value of the method body's last
/// statement, a local assignment: the method answers the assigned value in
/// every context, actor methods included (BT-3882), so the block is returned
/// from the method.
const LAST_STATEMENT_ASSIGNMENT_SHAPES: &[&str] = &[
    "returned via last-statement assignment",
    "alias as the last statement",
    "branch value as the last statement",
    "reassigned local as the last statement",
];

/// BT-3859: the [`ESCAPE_SHAPES`] whose block writes `u`, a local first bound
/// in a conditional arm the value passes through, rather than the method
/// local `t`. Probed before the fix: each compiled, and the first `value` of
/// the answer raised `captures mutable state` in class, value-type and actor
/// methods.
const ARM_LOCAL_ESCAPE_SHAPES: &[(&str, &str)] = &[
    (
        "arm-local write in a returned binding",
        "b := true ifTrue: [u := 0. [u := u + 1]] ifFalse: [nil]\n^b",
    ),
    (
        "arm-local write in a binding in a literal",
        "b := true ifTrue: [u := 0. [u := u + 1]] ifFalse: [nil]\n#[b]",
    ),
    (
        "arm-local write through an alias",
        "b := true ifTrue: [u := 0. [u := u + 1]] ifFalse: [nil]\nc := b\n^c",
    ),
    (
        "arm-local write in a nested arm",
        "b := true ifTrue: [u := 0. false ifTrue: [0] ifFalse: [[u := u + 1]]] ifFalse: [nil]\n^b",
    ),
];

#[test]
fn section6_escaping_blocks_are_errors_in_every_context() {
    let rows = ESCAPE_SHAPES
        .iter()
        .map(|&(name, body)| (name, body, "t"))
        .chain(
            ARM_LOCAL_ESCAPE_SHAPES
                .iter()
                .map(|&(name, body)| (name, body, "u")),
        );
    for (name, body, local) in rows {
        for (context, src) in [
            ("class", in_class(body)),
            ("value", in_value(body)),
            ("actor", in_actor(body)),
        ] {
            let diags = adr0131_diagnostics(&src);
            let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
            assert_eq!(s6.len(), 1, "{name} in {context}: {src}\n{diags:?}");
            let d = s6[0];
            assert_eq!(d.severity, Severity::Error, "{name} in {context}");
            assert!(
                d.message
                    .starts_with(&format!("`{local}` is assigned in a block ")),
                "{name} in {context}: {}",
                d.message
            );
            let hint = d.hint.as_deref().unwrap_or_default();
            assert!(
                hint.contains("assign it where"),
                "{name} in {context}: {hint}"
            );
            let at = &src[d.span.start() as usize..d.span.end() as usize];
            assert!(
                at.starts_with(&format!("{local} := ")),
                "{name} in {context}: at `{at}`"
            );
            assert!(
                d.notes.iter().any(|n| n.message.contains("(ADR 0133)")),
                "{name} in {context}"
            );
        }
    }
}

#[test]
fn section6_escape_points_at_the_assignment_with_a_note_at_the_block() {
    let src = in_class("^[t := t + 1]");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(
        diags[0].span.start() as usize,
        src.find("t := t + 1]").unwrap()
    );
    assert!(diags[0].message.contains("returned from `probe`"));
    let block = src.find("[t := t + 1]").unwrap();
    assert!(
        diags[0]
            .notes
            .iter()
            .any(|n| n.span.is_some_and(|s| s.start() as usize == block)),
        "{diags:?}"
    );

    // A local bound to the block is reported where it is bound, whatever the
    // local does later (an alias, a return, a field store).
    for body in [
        "b := [t := t + 1]\n^b",
        "b := [t := t + 1]\nc := b\n^c",
        "b := [t := t + 1]\nself.n := b\nnil",
    ] {
        let src = in_actor(body);
        let diags = adr0131_diagnostics(&src);
        assert_eq!(diags.len(), 1, "{body}: {diags:?}");
        assert!(
            diags[0].message.contains("stored in `b`"),
            "{body}: {diags:?}"
        );
    }
    let src = in_class("#([t := t + 1])");
    assert!(
        adr0131_diagnostics(&src)[0]
            .message
            .contains("placed in a collection literal")
    );
}

/// A block literal assigned straight to a field is reported once per method
/// kind: by §6 for a class variable (`self.h := [...]` in a class method), by
/// the block analyzer's BT-2809 error for an instance (so the
/// `bt2809_mixed_...produces_one_error` test keeps its single diagnostic).
/// This pins that no context lets it through.
#[test]
fn section6_field_stored_literal_is_an_error_in_every_context() {
    let body = "self.n := [t := t + 1]\nnil";
    assert_eq!(section6_errors(&in_class(body)), 1);
    for src in [in_value(body), in_actor(body)] {
        let all = analyse_full(&parse_module(&src), AnalysisContext::default()).diagnostics;
        let reported = all.iter().filter(|d| {
            d.severity == Severity::Error
                && d.message.contains("cannot mutate captured variable 't'")
        });
        assert_eq!(reported.count(), 1, "{src}: {all:?}");
    }
}

#[test]
fn section6_escaping_blocks_that_write_no_outer_local_are_accepted() {
    // Own locals, block parameters and reads need no write-back; in an actor a
    // field write has a home that outlives the method frame.
    for body in [
        "^[:x | x + 1]",
        "^[:x | y := x. y + 1]",
        "^[t + 1]",
        "#([:x | x], [t])",
        "b := [:x | y := x. y]\n^b",
        "b := [t + 1]\n#[b]",
        "^[:t | t := t + 1]",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(section6_errors(&src), 0, "{src}");
        }
    }
}

/// BT-3922 (ADR 0133, the rule's other half): a block that assigns `self.field`
/// (an actor or value-type field, or a class variable in a class method) and
/// escapes gets the same diagnostic as one that assigns a captured local,
/// reported at the assignment. This flips BT-3806 item 4, where §6 accepted
/// these and codegen rejected only some shapes.
#[test]
fn section6_escaping_block_that_writes_a_field_is_an_error() {
    for body in [
        "^[self.n := self.n + 1]",
        "b := [self.n := self.n + 1]\n^b",
        "#[[self.n := self.n + 1]]",
        "self.items foo: [self.n := self.n + 1]\nnil",
        "self foo: [self.n := self.n + 1]\nnil",
        "^[:x | #(1) do: [:y | self.n := y]]",
    ] {
        // A value-type method cannot assign a slot: that error already covers
        // the write, so §6 stays silent there.
        assert_eq!(section6_errors(&in_value(body)), 0, "{body}");
        for src in [in_class(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            let d = diags
                .iter()
                .find(|d| d.category == Some(DiagnosticCategory::Tier2BlockNoReturnChannel))
                .unwrap_or_else(|| panic!("{src}: {diags:?}"));
            assert_eq!(section6_errors(&src), 1, "{src}: {diags:?}");
            assert!(
                d.message.contains("`self.n` is assigned in a block"),
                "{d:?}"
            );
            assert!(
                src[d.span.start() as usize..].starts_with("self.n :="),
                "reported at the assignment: {src}"
            );
        }
    }
}

/// BT-3922: a block literal stored straight into a field that assigns a field
/// (not a captured local, which is the block analyzer's BT-2809 error) is
/// reported here in class and actor methods (a value-type method's slot
/// assignment is the "Cannot assign to slot" error).
#[test]
fn section6_field_stored_literal_that_writes_a_field_is_an_error() {
    let body = "self.h := [:x | self.n := self.n + x]\nnil";
    for src in [in_class(body), in_actor(body)] {
        assert_eq!(section6_errors(&src), 1, "{src}");
    }
    assert_eq!(section6_errors(&in_value(body)), 0);
}

/// BT-3922: in a class method a `Dictionary` lookup handler that writes only a
/// class variable writes it in place (ADR 0130), so it is not an escape;
/// `stdlib/test/lookup_field_write_test.bt` proves the write survives.
#[test]
fn section6_lookup_handler_that_writes_a_class_variable_is_not_an_escape() {
    for (body, actor_errors) in [
        (
            "self.cache at: 1 ifAbsent: [self.misses := self.misses + 1. nil]\nnil",
            1,
        ),
        (
            "x := self.cache at: 1 ifAbsent: [self.misses := self.misses + 1. 0]\n^x",
            1,
        ),
        // One diagnostic per writing handler.
        (
            "self.cache at: 1 ifPresent: [:v | self.hits := self.hits + 1. v] ifAbsent: [self.misses := 1. nil]\nnil",
            2,
        ),
        (
            "^self.cache at: 1 ifAbsent: [self.misses := self.misses + 1. nil]",
            1,
        ),
    ] {
        let src = in_class(body);
        assert_eq!(
            section6_errors(&src),
            0,
            "{src}: {:?}",
            adr0131_diagnostics(&src)
        );
        // An actor method runs the handler as a closure and the field write
        // is dropped, so the handler is an escape there; a value-type method
        // cannot assign a slot, and that error already covers the write.
        let src = in_actor(body);
        assert_eq!(
            section6_errors(&src),
            actor_errors,
            "{src}: {:?}",
            adr0131_diagnostics(&src)
        );
        assert_eq!(section6_errors(&in_value(body)), 0, "{body}");
    }
}

/// BT-3922: where "Cannot assign to slot" does not fire (a `TestCase` subclass
/// and a plain `Object` subclass), an escaping block that writes a field is
/// reported by §6, or the write would be dropped silently. A true `Value`
/// subclass gets only the slot error.
#[test]
fn section6_field_write_in_testcase_and_object_methods_is_an_error() {
    for body in [
        "self.cache at: 1 ifAbsent: [self.misses := self.misses + 1. nil]\nnil",
        "^[self.n := self.n + 1]",
        "b := [self.n := self.n + 1]\n^b",
    ] {
        for src in [in_testcase(body), in_object(body)] {
            assert_eq!(
                section6_errors(&src),
                1,
                "{src}: {:?}",
                adr0131_diagnostics(&src)
            );
        }
        assert_eq!(section6_errors(&in_value(body)), 0, "{body}");
    }
}

/// BT-3952 (ADR 0133 amendment): in an actor instance method a block that reads
/// `self.field` and is handed to the actor's own method, stored, returned or
/// placed in a literal sees the `State` from when it was made, so it is an
/// error at the read. Measured on a BEAM: the self-send answers stale when the
/// callee writes the field before running the block, the stored, returned and
/// literal shapes when a write follows; no static rule tells the right-answering
/// cases apart, so every one is rejected.
#[test]
fn section6_actor_block_that_reads_a_field_and_escapes_is_an_error() {
    for body in [
        "self foo: [self.n]\nnil",
        "super foo: [self.n]\nnil",
        "self foo: [:x | x + self.n]\nnil",
        // Through a nested, non-inlined block.
        "self foo: [self.items collect: [:x | x + self.n]]\nnil",
        "self foo: [self.flag ifTrue: [self.n] ifFalse: [0]]\nnil",
        "^[self.n]",
        "self.h := [self.n]\nnil",
        "#[[self.n]]",
        "self do: [:x | self.n]\nnil",
        // Bound to a local, then escaping (measured: `^b` answers stale).
        "b := [self.n]\n^b",
        "b := [self.n]\nself foo: b\nnil",
        "b := [self.n]\nself.h := b\nnil",
        "b := [self.n]\n#[b]",
        // The reflective read is a field read too.
        "self foo: [self fieldAt: #n]\nnil",
        "super foo: [super fieldAt: #n]\nnil",
        "^[self fieldAt: #n]",
        "self.h := [self fieldAt: #n]\nnil",
        "#[[self fieldAt: #n]]",
        "self foo: [:k | self fieldAt: k]\nnil",
        "b := [self fieldAt: #n]\n^b",
        "b := [self fieldAt: #n]\nself foo: b\nnil",
        "b := [self fieldAt: #n]\nself.n := 5\nb value",
        "^[self fieldAt: #n; yourself]",
        // A cascade to `self` dispatches to the actor's own method.
        "self foo: [self.n]; yourself\nnil",
        "super foo: [self.n]; yourself\nnil",
        "b := [self.n]\nself foo: b; yourself\nnil",
        // The blocks a collection operation answers leave with its result.
        "^self.items collect: [:x | [self.n]]",
        // A cascade to `self` may change the state.
        "b := [self.n]\nself bump; yourself\nb value",
        // Followed through an alias, a capture, and a rebinding that may run.
        "b := [self.n]\nc := b\n^c",
        "b := [self.n]\n^[b value]",
        "b := [self.n]\nself.h := [b value]\nnil",
        "b := [self.n]\nself foo: [b value]\nnil",
        "b := [self.n]\nself.flag ifTrue: [b := [1]]\n^b",
        // Called after something that may change the state (measured: a field
        // write or a self-send between the binding and the call reads stale).
        "b := [self.n]\nself.n := 5\nb value",
        "b := [self.n]\nself bump\nb value",
        "b := [self.n]\nself.flag ifTrue: [self.n := 1]\nb value",
        // In a loop whose body may change it, before or after the call.
        "b := [self.n]\nself.items do: [:x | self.n := x. b value]\nnil",
        "b := [self.n]\nself.items do: [:x | b value. self.n := x]\nnil",
        "b := [self.n]\nself.items do: [:x | self bump. b value]\nnil",
    ] {
        let src = in_actor(body);
        let diags = adr0131_diagnostics(&src);
        let d = diags
            .iter()
            .find(|d| d.category == Some(DiagnosticCategory::Tier2BlockNoReturnChannel))
            .unwrap_or_else(|| panic!("{src}: {diags:?}"));
        assert_eq!(section6_errors(&src), 1, "{src}: {diags:?}");
        assert!(
            d.message.contains("is read in a block that runs later"),
            "{d:?}"
        );
        assert!(
            ["self.", "self fieldAt:", "super fieldAt:", "b value"]
                .iter()
                .any(|at| src[d.span.start() as usize..].starts_with(at)),
            "reported at the read: {src}"
        );
    }
}

/// BT-3952: the shapes that answer right keep compiling: a block run in place
/// by an inlined construct, passed to a non-self receiver, passed in as a
/// value, or in a context where a field read is not a stale `State` read.
#[test]
fn section6_actor_block_that_reads_a_field_in_place_is_not_an_error() {
    for body in [
        // Inlined control flow and collection messages on a non-self receiver.
        "self.items do: [:x | self.n]\nnil",
        "self.items detect: [:x | x > self.n] ifNone: [self.n]\nnil",
        "self.flag ifTrue: [self.n] ifFalse: [0]\nnil",
        "self.items collect: [:x | x + self.n]\nnil",
        // A non-self receiver runs its own code, not this actor's.
        "other foo: [self.n]\nnil",
        "self.peer foo: [self.n]\nnil",
        // The value is passed in, not read in the block.
        "self foo: self.n\nnil",
        "self foo: [:v | v + 1]\nnil",
        // A block that does not read a field of `self`.
        "self foo: [other.n]\nnil",
        "^[:x | x + 1]",
        // A local block run at once, before any write, answers right
        // (measured); with nothing that may change the state in between.
        "b := [self.n]\nb value",
        "b := [self.n]\nself.items do: [:x | b value]\nnil",
        "b := [self.n]\nb value\nself.n := 1\nnil",
        // Read directly in the method body, not in a block.
        "self foo: (self fieldAt: #n)\nnil",
        "x := self fieldAt: #n\n^x",
        "b := [self fieldAt: #n]\nb value",
        "b := [self.n]\nself.items do: [:x | b value. x]\nnil",
        // The operands run before the write or the self-send itself.
        "b := [self.n]\nself.n := b value + 1\nnil",
        "b := [self.n]\nself foo: b value\nnil",
        // A parameter that shadows the local is another variable.
        "b := [self.n]\nself.items do: [:b | self record: b]\nnil",
        "b := [self.n]\nself.items do: [:b | self.h := b]\nnil",
        // The local is rebound to a block that reads no field.
        "b := [self.n]\nb := [1]\n^b",
        // A class-builder method body rebinds `self` to its parameter.
        "^#{#get => [:self | self.n]}",
    ] {
        let src = in_actor(body);
        assert_eq!(
            section6_errors(&src),
            0,
            "{src}: {:?}",
            adr0131_diagnostics(&src)
        );
    }
    // Class methods read class variables live at home (ADR 0130 §3), and a
    // value type's fields never change: neither can go stale.
    for body in [
        "self foo: [self.n]\nnil",
        "^[self.n]",
        "self.h := [self.n]\nnil",
    ] {
        assert_eq!(section6_errors(&in_class(body)), 0, "{body}");
        assert_eq!(section6_errors(&in_value(body)), 0, "{body}");
    }
}

/// BT-3952: a block that sends to `self` or `super` dispatches against the
/// `State` it captured (measured: a stored and a returned `[self total]` both
/// answered the old value), so when it is stored, returned or placed in a
/// literal (directly or through a local) it is an error at the send. A block
/// passed to the actor's own method (BT-3936's runtime check covers it), run
/// in place, or called through a local later is not.
#[test]
fn section6_actor_block_that_sends_to_self_and_is_stored_is_an_error() {
    for body in [
        "^[self total]",
        "^[super total]",
        "self.h := [self total]\nnil",
        "#[[self total]]",
        "^[:x | #(1) do: [:y | self record: y]]",
        "b := [self total]\n^b",
        "b := [self total]\nself.h := b\nnil",
        "b := [self total]\n#[b]",
        "^self.items collect: [:x | [self total]]",
        "^[self foo; bar]",
        // Followed through a capture and an alias.
        "b := [self total]\n^[b value]",
        "b := [self total]\nc := b\n^c",
    ] {
        let src = in_actor(body);
        let diags = adr0131_diagnostics(&src);
        let d = diags
            .iter()
            .find(|d| d.category == Some(DiagnosticCategory::Tier2BlockNoReturnChannel))
            .unwrap_or_else(|| panic!("{src}: {diags:?}"));
        assert_eq!(section6_errors(&src), 1, "{src}: {diags:?}");
        assert!(
            d.message.contains("is sent in a block that runs later"),
            "{d:?}"
        );
        assert!(
            ["self ", "super ", "b value"]
                .iter()
                .any(|at| src[d.span.start() as usize..].starts_with(at)),
            "reported at the send: {src}"
        );
    }
    for body in [
        // Passed to the actor's own method: BT-3936's runtime check.
        "self each: [:x | self record: x]\nnil",
        "self each: [self total]\nnil",
        // Run in place, or a local called later.
        "self.items do: [:x | self record: x]\nnil",
        "b := [self total]\nb value",
        "b := [self total]\nself bump\nb value",
        "b := [self total]\nself each: b\nnil",
        // The value is computed first and captured.
        "t := self total\n^[t]",
    ] {
        let src = in_actor(body);
        assert_eq!(
            section6_errors(&src),
            0,
            "{src}: {:?}",
            adr0131_diagnostics(&src)
        );
    }
    // Class methods and value types are not actors.
    for body in ["^[self total]", "self.h := [self total]\nnil"] {
        assert_eq!(section6_errors(&in_class(body)), 0, "{body}");
        assert_eq!(section6_errors(&in_value(body)), 0, "{body}");
    }
}

/// A field write in a block that the compiler lowers in place, or that never
/// leaves the method, is not an escape.
#[test]
fn section6_in_place_field_writes_are_not_escapes() {
    for body in [
        "#(1, 2) do: [:x | self.n := self.n + x]\nnil",
        "true ifTrue: [self.n := 1] ifFalse: [self.n := 2]\nnil",
        "[self.n := 1] value\nnil",
        // A class-builder method body takes `self` as a parameter.
        "^#{#bump => [:self | self.n := self.n + 1]}",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(section6_errors(&src), 0, "{src}");
        }
    }
    // BT-3952: a returned block that only READS a field is no write, but in an
    // actor it reads the `State` it was made with: an error there (the read
    // rule), still none in a class method (live class variables, ADR 0130 §3)
    // or a value type (fields never change).
    let body = "^[:x | x + self.n]";
    assert_eq!(section6_errors(&in_class(body)), 0);
    assert_eq!(section6_errors(&in_value(body)), 0);
    assert_eq!(section6_errors(&in_actor(body)), 1);
}

#[test]
fn section6_non_escaping_uses_are_not_escapes() {
    assert_eq!(
        section6_errors(&in_class("true ifTrue: [t := t + 1]\n^t")),
        0
    );
    assert_eq!(
        section6_errors(&in_class("#(1, 2) do: [:x | t := t + x]\n#[t, t]")),
        0
    );
}

/// BT-3806: the value a returned operand is followed through is only what the
/// method answers. A block that is not the value of the last statement of an
/// arm and the cleanup of an `ensure:` are discarded, so they do not escape;
/// nor does a block that is evaluated in place.
#[test]
fn section6_values_that_are_not_returned_are_not_escapes() {
    for body in [
        "true ifTrue: [[t := t + 1]. 0] ifFalse: [0]",
        "[0] ensure: [[t := t + 1]]",
        "true ifTrue: [t := t + 1] ifFalse: [t := 0]",
        "b := [t := t + 1] value",
        "#(1) do: [:x | [t := t + x]]",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(
                section6_errors(&src),
                0,
                "{src}: {:?}",
                adr0131_diagnostics(&src)
            );
        }
    }
    // ADR 0133: storing the block in a local is the escape, so rebinding the
    // local before it is returned no longer clears it (BT-3806, BT-3859
    // accepted these).
    for body in [
        "b := [t := t + 1]\nb := nil\n^b",
        "b := [t := t + 1]\nc := b\nnil",
        "x := [[t := t + 1]]\nnil",
        "x := true ifTrue: [[t := t + 1]] ifFalse: [nil]\nnil",
        "b := [t := t + 1]\nc := b\nc := nil\n^c",
        "b := [t := t + 1]\nc := nil\n#(1) do: [:b | c := b]\n^c",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert_eq!(
                section6_errors(&src),
                1,
                "{src}: {:?}",
                adr0131_diagnostics(&src)
            );
        }
    }
}

/// A local assigned more than once is reported once per block it is bound to.
#[test]
fn section6_escape_reports_every_stored_block_of_a_reassigned_local() {
    let src = in_class("b := [t := t + 1]\nb := [t := t + 2]\n^b");
    let s6: Vec<_> = adr0131_diagnostics(&src)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Tier2BlockNoReturnChannel))
        .collect();
    assert_eq!(s6.len(), 2, "{s6:?}");
    assert_eq!(
        s6[1].span.start() as usize,
        src.find("t := t + 2]").unwrap()
    );
}

/// BT-3858 (ADR 0133): the REPL root reports the same diagnostic as a method
/// (surface parity): a block that is the input's value, stored in a session
/// binding, or placed in a literal escapes.
#[test]
fn section6_escape_in_a_repl_expression_list_is_checked() {
    for (src, message) in [
        (
            "[t := t + 1]",
            "`t` is assigned in a block that is the value of the expression",
        ),
        (
            "b := [t := t + 1]",
            "`t` is assigned in a block stored in `b`",
        ),
        (
            "d := #([t := t + 100])",
            "`t` is assigned in a block placed in a collection literal",
        ),
        (
            "c := true ifTrue: [[t := t + 10]] ifFalse: [[0]]",
            "`t` is assigned in a block stored in `c`",
        ),
    ] {
        let diags = adr0131_diagnostics_with(src, &["t"]);
        let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
        assert_eq!(s6.len(), 1, "{src}: {diags:?}");
        assert_eq!(s6[0].message.as_str(), message, "{src}");
    }
    // The inline shapes still compile at the REPL.
    for src in [
        "#(1, 2, 3) do: [:x | t := t + x]",
        "[t < 10] whileTrue: [t := t + 1]",
        "[t := t + 1] value",
    ] {
        let diags = adr0131_diagnostics_with(src, &["t"]);
        assert!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
            "{src}: {diags:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// BT-3817: a `match:` arm that assigns an outer local
// ---------------------------------------------------------------------------

#[test]
fn match_arm_writing_an_outer_local_is_a_structured_error_in_every_context() {
    let body = "#a match: [#a -> (t := t + 1); _ -> 0]\nt";
    for src in [in_class(body), in_value(body), in_actor(body)] {
        let diags = adr0131_diagnostics(&src);
        let errors = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
        assert_eq!(errors.len(), 1, "{src}: {diags:?}");
        assert!(
            errors[0]
                .message
                .contains("`match:` arm writes outer local `t`"),
            "{}",
            errors[0].message
        );
    }
}

#[test]
fn match_arm_write_nested_in_an_expression_is_an_error() {
    let nested = "#a match: [#a -> self ap: (t := t + 1); _ -> 0]\nt";
    for src in [in_class(nested), in_value(nested), in_actor(nested)] {
        let diags = adr0131_diagnostics(&src);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len(),
            1,
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn nested_match_arms_are_judged_once_by_their_own_scope() {
    // The write is reported once (by the inner arm), not once per enclosing arm.
    let nested = "#a match: [#a -> (3 match: [y -> (t := t + y)]); _ -> 0]\nt";
    for src in [in_class(nested), in_value(nested), in_actor(nested)] {
        let diags = adr0131_diagnostics(&src);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len(),
            1,
            "{src}: {diags:?}"
        );
    }
    // An inner arm's pattern variable shadows the outer local `x`.
    let shadow = "x := 0\n#a match: [#a -> (3 match: [x -> (x := x + 1)]); _ -> 0]\nx";
    for src in [in_class(shadow), in_value(shadow), in_actor(shadow)] {
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

#[test]
fn match_arm_binding_its_own_local_or_the_match_value_is_accepted() {
    for body in [
        "t := #a match: [#a -> 5; _ -> 0]\nt",
        "#a match: [#a -> (m := 1); _ -> 0]\nt",
        "#a match: [#a -> (t + 1); _ -> 0]",
        // The arm rebinds its own pattern variable.
        "3 match: [x -> (x := x + 1)]\nt",
        "#(1, 2) match: [{a, b} -> (a := a + b)]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2d (BT-3798): `Dictionary at:ifAbsent:` as a producer
// ---------------------------------------------------------------------------

#[test]
fn dictionary_lookup_handler_write_is_a_producer_in_every_method_context() {
    // A statement, an assignment's value, an operand, a `^` value and the last
    // expression, with the handler reading and write-only, and the lookup
    // parenthesised.
    for body in [
        "d := #{#a => 1}\nd at: #k ifAbsent: [t := t + 1]\nt",
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [t := 7. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := 1 + (d at: #k ifAbsent: [t := t + 1. 5])\n#[r, t]",
        "d := #{#a => 1}\nr := t + (d at: #k ifAbsent: [t := t + 1. 5])\n#[r, t]",
        "d := #{#a => 1}\n^(d at: #k ifAbsent: [t := t + 1. 5]) + t",
        "d := #{#a => 1}\n(d at: #k ifAbsent: [t := t + 1])\nt",
        "d := #{#a => 1}\nd at: #k ifAbsent: [t := t + 1. 3]",
        // The key and receiver are ordinary operands.
        "d := #{#a => 1}\nr := d at: t ifAbsent: [t := t + 1. 0]\n#[r, t]",
        // In an arm of a conditional producer and a protected body.
        "d := #{#a => 1}\nr := nil\n(t < 3) ifTrue: [r := d at: #k ifAbsent: [t := t + 1. 0]]\n#[r, t]",
        "d := #{#a => 1}\nr := [d at: #k ifAbsent: [t := t + 1. 0]] on: Error do: [:e | -1]\n#[r, t]",
        // The handler holds a loop, a conditional and another lookup.
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [#(1, 2) do: [:x | t := t + x]. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [(t < 3) ifTrue: [t := t + 1. 1] ifFalse: [2]]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [d at: #z ifAbsent: [t := t + 1. 4]]\n#[r, t]",
        // BT-3839: a statement of a loop or fold body, in an arm of a
        // conditional producer around the loop, and nested in two frames.
        "d := #{#a => 1}\n#(1, 2) do: [:x | d at: #k ifAbsent: [t := t + x]]\nt",
        "d := #{#a => 1}\n(t < 3) ifTrue: [#(1, 2) do: [:x | d at: #k ifAbsent: [t := t + x]]]\nt",
        "d := #{#a => 1}\n1 to: 2 do: [:i | #(1, 2) do: [:x | d at: x ifAbsent: [t := t + 1]]]\nt",
        "d := #{#a => 1}\n[t < 3] whileTrue: [d at: #k ifAbsent: [t := t + 1]]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn dictionary_lookup_handler_write_in_a_loop_or_fold_body_is_a_producer() {
    // BT-3839: the value of an assignment or an operand in a letrec loop body
    // is a producer site in every method context; the same sites in an actor
    // fold body are value-bearing sites the actor fold frame does not lower
    // (`is_actor_fold_value_site`, BT-3797), so the actor keeps the error there,
    // exactly as for a conditional.
    for body in [
        "d := #{#a => 1}\n1 to: 2 do: [:i | r := d at: #k ifAbsent: [t := t + i. 0]]\nt",
        "d := #{#a => 1}\n2 timesRepeat: [r := 1 + (d at: #k ifAbsent: [t := t + 1. 0])]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
    for body in [
        "d := #{#a => 1}\n#(1, 2) do: [:x | r := d at: #k ifAbsent: [t := t + x. 0]]\nt",
        "d := #{#a => 1}\nr := #(1, 2) inject: 0 into: [:a :x | a + (d at: x ifAbsent: [t := t + 1. 0])]\n#[r, t]",
    ] {
        for src in [in_class(body), in_value(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
        assert!(!adr0131_diagnostics(&in_actor(body)).is_empty(), "{body}");
    }
}

#[test]
fn dictionary_lookup_that_the_producer_cannot_lower_keeps_the_section6_error() {
    for body in [
        // A parenthesised handler is a block value.
        "d := #{#a => 1}\nr := d at: #k ifAbsent: ([t := t + 1. 0])\n#[r, t]",
        // A block key is a block value too.
        "d := #{#a => 1}\nr := d at: [t := t + 1. #k] ifAbsent: [0]\n#[r, t]",
        // The handler takes a parameter: the primitive calls it with none.
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [:x | t := t + 1. 0]\n#[r, t]",
        // A cascade message is never inlined.
        "d := #{#a => 1}\nd at: #k ifAbsent: [t := t + 1]; yourself\nt",
        // In a closure that is not inlined (BT-3839 keeps this: the lookup
        // producer has no frame there), alone and in a loop body.
        "d := #{#a => 1}\nb := [d at: #k ifAbsent: [t := t + 1. 0]]\nb value",
        "d := #{#a => 1}\n#(1, 2) do: [:x | [:z | d at: z ifAbsent: [t := t + 1. 0]] value: x]\nt",
        // A parenthesised handler in a loop body.
        "d := #{#a => 1}\n#(1, 2) do: [:x | d at: x ifAbsent: ([t := t + 1. 0])]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(!diags.is_empty(), "{src}");
        }
    }
}

#[test]
fn lookup_selectors_that_dictionary_does_not_implement_stay_rejected() {
    // `Dictionary` does not implement `at:ifAbsentPut:` or
    // `removeKey:ifAbsent:` (permanently: it is immutable; pinned against the
    // class hierarchy by `dictionary_inline_rows_are_dictionary_selectors`), so
    // those rows have no behaviour to lower: a handler that writes an outer
    // local is a §6 error in every context and position.
    for body in [
        "d := #{#a => 1}\nr := d at: #k ifAbsentPut: [t := t + 1. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := d removeKey: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}

#[test]
fn dictionary_if_present_handler_write_is_a_producer_in_every_method_context() {
    // BT-3824: `at:ifPresent:` and `at:ifPresent:ifAbsent:` are `Dictionary`
    // selectors, lowered like `at:ifAbsent:`; the present handler takes the
    // found value as its one parameter.
    for body in [
        "d := #{#a => 1}\nd at: #k ifPresent: [:v | t := t + v]\nt",
        "d := #{#a => 1}\nr := d at: #a ifPresent: [:v | t := t + v. v]\n#[r, t]",
        "d := #{#a => 1}\nr := 1 + (d at: #a ifPresent: [:v | t := t + v. v])\n#[r, t]",
        "d := #{#a => 1}\n^(d at: #a ifPresent: [:v | t := v. 5]) + t",
        "d := #{#a => 1}\nr := d at: #k ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifPresent: [:v | t := t + v. v] ifAbsent: [0]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifPresent: [:v | t := t + v. v] ifAbsent: [t := 9. 0]\n#[r, t]",
        "d := #{#a => 1}\nr := nil\n(t < 3) ifTrue: [r := d at: #a ifPresent: [:v | t := t + v]]\n#[r, t]",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn dictionary_if_present_that_the_producer_cannot_lower_keeps_the_section6_error() {
    for body in [
        // The present handler takes no parameter: the primitive passes one.
        "d := #{#a => 1}\nr := d at: #k ifPresent: [t := t + 1. 0]\n#[r, t]",
        // The absent handler takes a parameter: the primitive passes none.
        "d := #{#a => 1}\nr := d at: #k ifPresent: [:v | v] ifAbsent: [:x | t := t + 1. 0]\n#[r, t]",
        // A parenthesised handler is a block value.
        "d := #{#a => 1}\nr := d at: #k ifPresent: ([:v | t := t + v. 0])\n#[r, t]",
        // The present handler takes no parameter, in a loop body.
        "d := #{#a => 1}\n#(1, 2) do: [:x | d at: #a ifPresent: [t := t + 1]]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(!diags.is_empty(), "{src}");
        }
    }
    // A receiver statically known not to be a Dictionary.
    let src = format!(
        "Value subclass: Own\n  probe: cache :: Ets =>\n    t := 0\n{}\n",
        indent("r := cache at: #k ifPresent: [:v | t := t + v. 0]\n#[r, t]")
    );
    let diags = adr0131_diagnostics(&src);
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{src}: {diags:?}"
    );
}

#[test]
fn dictionary_lookup_is_not_a_producer_at_the_repl() {
    // The REPL threads its bindings map through its own lowering, which has no
    // lookup row: the §6 error stays.
    let known = ["t", "d"];
    for src in [
        "d at: #k ifAbsent: [t := t + 1. 0]\n",
        "r := d at: #k ifAbsent: [t := t + 1. 0]\n",
        "1 + (d at: #k ifAbsent: [t := t + 1. 0])\n",
    ] {
        let diags = adr0131_diagnostics_with(src, &known);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
            1,
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn dictionary_lookup_that_writes_no_outer_local_is_not_judged() {
    for body in [
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [0]\n#[r, t]",
        "d := #{#a => 1}\nr := d at: #k ifAbsent: [m := 1. m + 1]\n#[r, t]",
        "d := #{#a => 1}\n#(1, 2) do: [:x | d at: #k ifAbsent: [m := x. m]]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
}

#[test]
fn lookup_on_a_receiver_statically_known_not_to_be_a_dictionary_keeps_the_section6_error() {
    // BT-3798 review: the runtime guard would raise on every call, so the
    // compile-time error stays for `self`/`super` and for an inferred type
    // that is a known class other than Dictionary.
    let own = "  at: key ifAbsent: block => block value\n";
    for (header, body) in [
        // `self` in a class that defines its own `at:ifAbsent:`.
        (
            format!("Value subclass: Own\n{own}\n  probe =>\n    t := 0"),
            "r := self at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        // `super`.
        (
            format!("Value subclass: Own\n{own}\n  probe =>\n    t := 0"),
            "r := super at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        // `self` of an actor.
        (
            format!("Actor subclass: Own\n  state: n = 0\n\n{own}\n  probe =>\n    t := 0"),
            "r := self at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        // A parameter typed with a class that is not a Dictionary.
        (
            "Value subclass: Own\n  probe: cache :: Ets =>\n    t := 0".to_string(),
            "r := cache at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe: xs :: List =>\n    t := 0".to_string(),
            "xs at: 1 ifAbsent: [t := t + 1]\nt",
        ),
        // A class object.
        (
            "Value subclass: Own\n  probe =>\n    t := 0".to_string(),
            "r := Own at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
    ] {
        let src = format!("{header}\n{}\n", indent(body));
        let diags = adr0131_diagnostics(&src);
        assert_eq!(
            of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
            1,
            "{src}: {diags:?}"
        );
    }
}

#[test]
fn lookup_on_a_receiver_that_may_be_a_dictionary_stays_a_producer() {
    // Unknown, Dynamic, an ancestor of Dictionary and Dictionary itself keep
    // the producer and its runtime guard.
    for (header, body) in [
        (
            "Value subclass: Own\n  probe: d =>\n    t := 0",
            "r := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe: d :: Dictionary =>\n    t := 0",
            "r := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe: d :: Object =>\n    t := 0",
            "r := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe: d :: Collection =>\n    t := 0",
            "r := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe: d :: Dictionary | Ets =>\n    t := 0",
            "r := d at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
        (
            "Value subclass: Own\n  probe =>\n    t := 0",
            "r := #{#a => 1} at: #k ifAbsent: [t := t + 1. 0]\n#[r, t]",
        ),
    ] {
        let src = format!("{header}\n{}\n", indent(body));
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

/// ADR 0131 Phase 5a (BT-3751): the names an arm binds exclude those a block
/// nested in it binds. A stored closure in a protected body whose own loop
/// writes the closure's local (`i := 0. [i < 3] whileTrue: [i := i + 1]`) is
/// the closure's business, not the arm's, so it must not make the enclosing
/// `on:do:` (an operand here) or the `tryDo:` inside it stop being producers.
/// The class-variable corpus found it as a `did not compile` program (seed
/// 9462456777465831732).
#[test]
fn closure_local_loop_in_a_protected_body_does_not_unbind_the_producers() {
    let body = "t + ([_b := [i := 0. [i < 3] whileTrue: [i := i + 1]]. \
                true ifTrue: [true ifTrue: [^t + ((Result tryDo: [t := t + 1. 5]) valueOr: 0)]]. 9] \
                on: Error do: [:_e | 0])";
    for src in [in_class(body), in_value(body), in_actor(body)] {
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

/// BT-3782: a keyword `whileTrue:`/`whileFalse:` CONDITION block that writes an
/// outer local has a return channel only where codegen lowers it through the
/// loop's `StateAcc` — a state-effecting condition (a self-send) with a literal
/// body in a value-type or actor method. Every other combination compiled to a
/// Tier 2 closure called with the wrong arity (value-type, actor) or a hang
/// (class methods). Generated over context x selector x condition x body.
///
/// BT-3845: a body that writes a local (or sends to `self`) also selects the
/// stateful lowering, but a pure condition is then compiled as a plain block
/// body whose write is never packed (probed: the loop hangs), so it stays
/// rejected; the decision is the shared leaf
/// `block_facts::while_condition_writes_thread`. The accepted shapes run in
/// `stdlib/test/bt3746stateful_while_test.bt`.
#[test]
fn while_condition_outer_local_write_is_rejected_unless_codegen_threads_it() {
    type Wrap = fn(&str) -> String;
    let contexts: [(&str, Wrap); 3] = [
        ("class", in_class),
        ("value", in_value),
        ("actor", in_actor),
    ];
    let selectors = [("whileTrue:", "t < 3"), ("whileFalse:", "t >= 3")];
    let conditions = [
        // (shape, has state effects)
        ("t := t + 1. {c}", false),
        ("t := t + 1. self check: t. {c}", true),
        ("t := t + 1. self.n := t. {c}", true),
    ];
    let bodies = [
        // (body, literal block)
        ("[nil]", true),
        ("body", false),
        ("[u := u + 1]", true),
        ("[u := u + 1. self check: u]", true),
    ];
    let mut checked = 0;
    for (ctx_name, wrap) in contexts {
        for (selector, cmp) in selectors {
            for (cond_shape, effects) in conditions {
                for (body, literal) in bodies {
                    let cond = cond_shape.replace("{c}", cmp);
                    let source = wrap(&format!(
                        "u := 0\nbody := [nil]\n[{cond}] {selector} {body}\n#[t, u]"
                    ));
                    let diags = adr0131_diagnostics(&source);
                    let errors = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
                    let threads = ctx_name != "class" && effects && literal;
                    assert_eq!(
                        errors.is_empty(),
                        threads,
                        "{ctx_name} {selector} cond=[{cond}] body={body}: {diags:?}"
                    );
                    checked += 1;
                }
            }
        }
    }
    assert_eq!(checked, 72);
}

/// BT-3782: a condition that only reads outer locals, and a loop body that
/// writes them, stay accepted in every context.
#[test]
fn while_condition_without_outer_write_is_accepted() {
    for wrap in [in_class, in_value, in_actor] {
        let source = wrap("[t < 3] whileTrue: [t := t + 1]\nt");
        let diags = adr0131_diagnostics(&source);
        assert!(diags.is_empty(), "{source}: {diags:?}");
    }
}

/// BT-3841: an evaluated literal block (`[...] value`) that writes an outer local
/// is an arm-like local-threading producer by rule, the one `conditional_arm_blocks`
/// site rule: accepted as a statement, as an operand, in an arm, a protected body,
/// a handler and a loop body, and nested in itself, at any depth, in every method
/// context. What is not a producer keeps the legacy verdict: `value:` (its
/// arguments bind the block's parameters outside the frame), a parenthesised
/// receiver (a block value, §6), and the REPL.
#[test]
fn evaluated_literal_blocks_are_producers_at_any_depth() {
    for body in [
        "t := 0\n[t := t + 1] value\nt",
        "t := 0\nr := [t := t + 1. 5] value\n#[r, t]",
        "t := 0\nr := t + ([t := t + 1. 5] value)\n#[r, t]",
        "t := 0\ntrue ifTrue: [[t := t + 1] value]\nt",
        "t := 0\n[[t := t + 1] value] value\nt",
        "t := 0\n#(1, 2) do: [:z | [t := t + z] value]\nt",
        "t := 0\n#(1, 2) do: [:z | true ifTrue: [[t := t + z] value]]\nt",
        "t := 0\n[[t := t + 1] value] on: Error do: [:e | 0]\nt",
        "t := 0\n[Error signal: \"x\"] on: Error do: [:e | [t := t + 1] value. 0]\nt",
        "t := 0\ntrue ifTrue: [#(1, 2) do: [:z | [[t := t + z] value] on: Error do: [:e | 0]]]\nt",
        "t := 0\n#(1, 2) do: [:y | #(1, 2) do: [:z | true ifTrue: [[t := t + 1] value]]]\nt",
        "t := 0\n1 to: 2 do: [:i | [t := t + i. [t := t + 1] value] value]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert!(diags.is_empty(), "{src}: {diags:?}");
        }
    }
    // Not producers: a `value:` send and a parenthesised receiver (§6) keep their
    // legacy verdict.
    for body in [
        "t := 0\ntrue ifTrue: [[:v | t := t + v] value: 1]\nt",
        "t := 0\n([t := t + 1]) value\nt",
    ] {
        for src in [in_class(body), in_value(body)] {
            assert!(
                !adr0131_diagnostics(&src).is_empty(),
                "{src}: accepted but not a producer"
            );
        }
    }
    // The REPL keeps its own lowering and its verdict.
    let diags = adr0131_diagnostics_with("[t := t + 1] value\n", &["t"]);
    assert!(diags.is_empty(), "{diags:?}");
}

/// BT-3823 review: an arm or loop-body statement that assigns a Tier 2 block
/// call whose argument is a writing fold (`u := [:k | t := t + k. k] value:
/// (xs inject: ...)`, or the same through a stored block) is rejected in class,
/// value-type and actor methods, so codegen never lowers it there. (Codegen's
/// C3 lowering stays for any Tier 2 call that is not itself a producer.)
#[test]
fn a_tier2_call_with_a_producer_argument_in_an_arm_is_rejected_in_methods() {
    let call = "[:k | t := t + k. k] value: (#(1, 2) inject: 0 into: [:a :x | t := t + x. a + x])";
    let stored = "blk value: (#(1, 2) inject: 0 into: [:a :x | t := t + x. a + x])";
    for (header, prefix) in [
        ("Object subclass: P", "class "),
        ("Value subclass: P", ""),
        ("Actor subclass: P\n  state: n = 0\n", ""),
    ] {
        for value in [call, stored] {
            for stmt in [
                format!("4 =:= 4 ifTrue: [u := {value}]"),
                format!("#(1) do: [:z | u := {value}]"),
                format!("u := {value}"),
            ] {
                let src = format!(
                    "{header}\n  {prefix}probe =>\n    t := 0\n    u := 0\n    blk := [:k | t := t + k. k]\n    {stmt}\n    #[t, u]\n"
                );
                assert!(!adr0131_diagnostics(&src).is_empty(), "{src}");
            }
        }
    }
    // The third Tier 2 receiver codegen recognises, an actor field
    // (`self.blk value: ...`), whose block the §6 check does not see: the
    // writing fold in its argument is rejected all the same (probed on a BEAM:
    // the root, an arm and a loop body all report it).
    let field = "self.blk value: (#(1, 2) inject: 0 into: [:a :x | t := t + x. a + x])";
    for stmt in [
        format!("4 =:= 4 ifTrue: [u := {field}]"),
        format!("#(1) do: [:z | u := {field}]"),
        format!("u := {field}"),
    ] {
        let src = format!(
            "Actor subclass: P\n  state: blk = nil\n\n  probe =>\n    t := 0\n    u := 0\n    {stmt}\n    #[t, u]\n"
        );
        assert!(
            !of_category(
                &adr0131_diagnostics(&src),
                DiagnosticCategory::UnmigratedLocalThreading
            )
            .is_empty(),
            "{src}"
        );
    }
}

// ---------------------------------------------------------------------------
// BT-3910 (ADR 0131 §1): a local assignment in value position is a producer
// ---------------------------------------------------------------------------

/// The categorised errors of `src`, analysed as a module (with `known_vars` for
/// a top-level expression list). (`in_value` has two classes in one module, an
/// uncategorised error.)
fn analysis_errors(src: &str, known_vars: &[&str]) -> Vec<Diagnostic> {
    let ctx = AnalysisContext::default().with_known_vars(known_vars);
    analyse_full(&parse_module(src), ctx)
        .diagnostics
        .into_iter()
        .filter(|d| d.severity == Severity::Error && d.category.is_some())
        .collect()
}

/// A local assignment used as a value is accepted in every context, the REPL
/// included: codegen lowers it as a `LocalRebind` of the enclosing frame (the
/// BT-3864 stopgap error is gone), `^t := e` too.
#[test]
fn a_value_position_local_assignment_is_accepted_in_every_context() {
    for body in [
        "x := (t := t + 1) + 1\n#[t, x]",
        "#[t, (t := 7), t]",
        "x := t := 7\n#[t, x]",
        "(t := t + 1) > 0 ifTrue: [x := 9]\nt",
        "#(1, 2) do: [:k | (t := t + 1) > 1 ifTrue: [x := 9]]\nt",
        "^t := t + 1",
        "^(t := t + 1)",
        "^(t := t + 1) + 1",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let errors = analysis_errors(&src, &[]);
            assert!(errors.is_empty(), "{src}: {errors:?}");
        }
        let errors = analysis_errors(&format!("{body}\n"), &["t", "x"]);
        assert!(errors.is_empty(), "REPL {body}: {errors:?}");
    }
}

#[test]
fn a_match_arm_write_to_an_outer_local_is_reported_once() {
    let src = in_class("x := 3 match: [3 -> t := 7; _ -> 0]\nt");
    let diags = analyse_full(&parse_module(&src), AnalysisContext::default()).diagnostics;
    let at_arm: Vec<_> = diags
        .iter()
        .filter(|d| {
            d.severity == Severity::Error
                && &src[d.span.start() as usize..d.span.end() as usize] == "t := 7"
        })
        .collect();
    assert_eq!(at_arm.len(), 1, "{at_arm:?}");
}

/// PIN-BUG BT-3947: a `^` out of the protected block of an `ensure:` whose
/// cleanup reads a local the block writes is rejected: the cleanup would read
/// the old `t`, silently (measured on a BEAM in a class, a value type and an
/// actor: `t := 0. [t := t + 1. ^t] ensure: [sink record: t]` records 0; the
/// correct answer is 1). It is the same whether the write is the returned
/// value (`^t := t + 1`) or a statement before the `^`, and whether the `^` is
/// in the protected block or a block nested in it. BT-3947 lowers it and flips
/// this to an acceptance test. Without a `^` in the protected block, with a
/// cleanup that does not read the written local, or for `on:do:` and the
/// cleanup itself, it is accepted.
#[test]
fn a_returned_assignment_inside_an_ensure_protected_block_is_rejected() {
    for body in [
        "[^t := t + 1] ensure: [t printString]\nt",
        "[[^t := t + 1] on: Error do: [:e | 0]] ensure: [t printString]\nt",
        "#(1, 2) do: [:k | [k > 1 ifTrue: [^t := k]] ensure: [t printString]]\nt",
        "[t := t + 1. ^t] ensure: [t printString]\nt",
        "[t := t + 1. #(1) do: [:e | ^t]] ensure: [t printString]\nt",
        "[#(1, 2) do: [:k | t := t + k]. ^t] ensure: [t printString]\nt",
        "[[t < 3] whileTrue: [t := t + 1]. ^t] ensure: [t printString]\nt",
        "[#(1, 2) do: [:k | ^t := k]] ensure: [t printString]\nt",
        "[#(1, 2) collect: [:k | t := t + k]. ^t] ensure: [t printString]\nt",
        "[#(1, 2) inject: 0 into: [:a :k | t := t + k. a]. ^t] ensure: [t printString]\nt",
        "[1 to: 2 do: [:k | t := t + k]. ^t] ensure: [t printString]\nt",
        "[2 timesRepeat: [t := t + 1]. ^t] ensure: [t printString]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let errors = analysis_errors(&src, &[]);
            assert_eq!(errors.len(), 1, "{src}: {errors:?}");
            assert!(
                errors[0].message.contains("BT-3947"),
                "{}",
                errors[0].message
            );
            assert!(
                errors[0]
                    .hint
                    .as_deref()
                    .is_some_and(|h| h.contains("after the `ensure:`")),
                "{:?}",
                errors[0].hint
            );
        }
        let errors = analysis_errors(&format!("{body}\n"), &["t"]);
        assert!(
            errors.iter().any(|e| e.message.contains("BT-3947")),
            "REPL {body}: {errors:?}"
        );
    }
    for body in [
        "[0] ensure: [t printString]\n^t := t + 1",
        "[^t := t + 1] on: Error do: [:e | t]\nt",
        "[0] ensure: [^t := t + 1]\nt",
        "[t := t + 1] ensure: [t printString]\nt",
        "[t := t + 1. ^t] ensure: [0]\nt",
        "[^t := t + 1] ensure: [0]\nt",
        "[t := t + 1. ^t] on: Error do: [:e | t]\nt",
        "#(1, 2) do: [:k | [t := t + k] ensure: [t printString]]\nt",
        "#(1, 2) do: [:k | [t := t + k. ^t] ensure: [0]]\nt",
        "[#(1, 2) do: [:k | z := k. z printString]. ^t] ensure: [t printString]\nt",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let errors = analysis_errors(&src, &[]);
            assert!(errors.is_empty(), "{src}: {errors:?}");
        }
    }
}

/// BT-3910: every method of the `adr0131nested_assign_*.bt` fixtures
/// (BT-3799's matrix, once inert `.pending` files) is accepted.
#[test]
fn adr0131_nested_assign_fixtures_are_accepted() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    for context in ["class", "value", "actor"] {
        let name = format!("adr0131nested_assign_{context}.bt");
        let src =
            std::fs::read_to_string(root.join(&name)).unwrap_or_else(|e| panic!("{name}: {e}"));
        let errors = analysis_errors(&src, &[]);
        assert!(errors.is_empty(), "{name}: {errors:?}");
    }
}

/// BT-3882: a block that is the value of the method body's last-statement
/// assignment escapes in every context, so the actor diagnostic is the class
/// and value-type one (ADR 0133 names the local it is stored in).
#[test]
fn section6_last_statement_assignment_escape_is_the_same_in_every_context() {
    for &name in LAST_STATEMENT_ASSIGNMENT_SHAPES {
        let body = ESCAPE_SHAPES
            .iter()
            .find(|(shape, _)| *shape == name)
            .map(|&(_, body)| body)
            .expect("a LAST_STATEMENT_ASSIGNMENT_SHAPES row names an ESCAPE_SHAPES row");
        let reports: Vec<(EcoString, Option<EcoString>)> =
            [in_class(body), in_value(body), in_actor(body)]
                .iter()
                .map(|src| {
                    let diags = adr0131_diagnostics(src);
                    let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
                    assert_eq!(s6.len(), 1, "{name}: {src}\n{diags:?}");
                    (s6[0].message.clone(), s6[0].hint.clone())
                })
                .collect();
        assert!(
            reports[0].0.contains("stored in `b`"),
            "{name}: {}",
            reports[0].0
        );
        assert_eq!(reports[0], reports[1], "{name}: class vs value");
        assert_eq!(reports[0], reports[2], "{name}: class vs actor");
    }
}

// ---------------------------------------------------------------------------
// ADR 0133 Phase 3: the residuals made loud before the IR consolidation
// (BT-3909, `RESIDUAL_DENY_SET`)
// ---------------------------------------------------------------------------

/// The messages of the ADR 0131 errors `src` draws (as a module-level
/// expression list when `repl_vars` is `Some`, with those REPL bindings).
fn residual_messages(src: &str, repl_vars: Option<&[&str]>) -> Vec<String> {
    let diags = match repl_vars {
        Some(vars) => adr0131_diagnostics_with(src, vars),
        None => adr0131_diagnostics(src),
    };
    diags.into_iter().map(|d| d.message.to_string()).collect()
}

/// BT-3911 (closes BT-3891): an assignment inside a conditional arm nested in
/// a `whileTrue:`/`whileFalse:` condition block emitted an unbound `StateAcc`
/// (`erlc` rejected the module) in class, value-type and actor methods, until
/// the condition closure became a frame of its own (a nested conditional's
/// scratch map is then the closure's private one). The shapes are accepted in
/// every context; `stdlib/test/bt3911_arm_frames_test.bt` runs them.
#[test]
fn loop_condition_arm_write_is_accepted() {
    // The BT-3891 repro, verbatim.
    let repro = "Object subclass: Repro\n  class f: n :: Integer -> Integer =>\n    i := 0\n    [i < n and: [x := i. x < 5]] whileTrue: [i := i + 1]\n    i\n";
    assert!(residual_messages(repro, None).is_empty());
    // The BT-3891 shape matrix rows that failed, plus the measured nestings:
    // through a `do:` body or a closure in the condition, and a local bound
    // in the condition before the arm; and the controls (a direct write in
    // the condition, the same `and:` outside a loop, an arm write in the loop
    // body), which were always accepted.
    for body in [
        "i := 0\n[i < 3 and: [x := i. x < 5]] whileTrue: [i := i + 1]\ni",
        "i := 0\n[i >= 3 or: [x := i. x >= 5]] whileFalse: [i := i + 1]\ni",
        "i := 0\n[i < 3 ifTrue: [x := i. x < 5] ifFalse: [false]] whileTrue: [i := i + 1]\ni",
        "i := 0\n[nil ifNil: [x := 1]. i < 3] whileTrue: [i := i + 1]\ni",
        "i := 0\n[#(1) do: [:e | true ifTrue: [x := e]]. i < 3] whileTrue: [i := i + 1]\ni",
        "i := 0\n[[:k | true ifTrue: [x := k]] value: 1. i < 3] whileTrue: [i := i + 1]\ni",
        "i := 0\n[x := 0. true ifTrue: [x := 1]. i < 3] whileTrue: [i := i + 1]\ni",
        "i := 0\n[x := i. x < 3] whileTrue: [i := i + 1]\ni",
        "i := 3\ni < 5 and: [x := i. x < 5]\ni",
        "i := 0\n[i < 3] whileTrue: [true ifTrue: [x := i]. i := i + 1]\ni",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            assert!(residual_messages(&src, None).is_empty(), "{src}");
        }
    }
    let repl = "i := 0\n[i < 3 and: [x := i. x < 5]] whileTrue: [i := i + 1]\n";
    assert!(residual_messages(repl, Some(&[])).is_empty());
}

/// PIN-BUG BT-3912 (closes BT-3897): an actor or REPL `groupBy:` whose block
/// binds a local (or, in an actor, writes a field or sends to `self`)
/// answered the raw `{Map, StateAcc}` carrier wherever its value is used
/// outside the positions that unpack it; BT-3909 makes those positions a
/// compile error. BT-3912 lowers the fold body as a frame, deletes the
/// `GroupByValue` row and flips these to accepted. Class and value-type
/// methods answer the map and are not rejected.
#[test]
fn residual_group_by_value_is_an_error_where_the_carrier_leaks() {
    // The BT-3897 repro, verbatim.
    let repro = "Actor subclass: ZzGbProbe\n  letk =>\n    (#(1, 3) groupBy: [:x |\n      k := x > 2\n      k\n    ]) at: true\n";
    let messages = residual_messages(repro, None);
    assert_eq!(messages.len(), 1, "{messages:?}");
    assert!(
        messages[0].contains("`groupBy:` binds a local")
            && messages[0].contains("as the receiver of a message in an actor method")
            && messages[0].contains("BT-3897, until BT-3912"),
        "{messages:?}"
    );
    // The leaking positions measured on a BEAM, and the destructuring last
    // statement BT-3897 records.
    for body in [
        "(#(1, 3) groupBy: [:x | k := x > 2. k]) at: true",
        "(#(1, 3) groupBy: [:x | #(_a, _b) := #(x > 2, 0)]) size",
        "(#(1, 3) groupBy: [:x | self.n := self.n + 1. x > 2]) at: true",
        "#[(#(1, 3) groupBy: [:x | k := x > 2. k])] size",
        "#(1) collect: [:z | #(1, 3) groupBy: [:x | k := x > 2. k]]",
        "#(1) do: [:z | ^#(1, 3) groupBy: [:x | k := x > 2. k]]\nnil",
        "b := [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. r]\nb value: 1",
        "#(1) do: [:z | self.n := (#(1, 3) groupBy: [:x | k := x > 2. k]) size]\nnil",
        // An assignment directly in a loop or counted-loop body answered the
        // carrier (measured, PR #4383 review).
        "r := nil\n[r isNil] whileTrue: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        "r := nil\n1 to: 1 do: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        "r := nil\n1 timesRepeat: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        // Every block not in `GROUP_BY_ASSIGN_BLOCKS` (no BUnit cell proves
        // it unpacks) stays rejected, keyed on the exact construct block.
        // anySatisfy:
        "r := nil\n#(1) anySatisfy: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // allSatisfy:
        "r := nil\n#(1) allSatisfy: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // detect: search
        "r := nil\n#(1) detect: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // detect:ifNone: search
        "r := nil\n#(1) detect: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. true] ifNone: [0]\nr",
        // select:
        "r := nil\n#(1) select: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // ifNil:
        "r := nil\nnil ifNil: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        // ifTrue:ifFalse:
        "r := nil\ntrue ifTrue: [r := #(1, 3) groupBy: [:x | k := x > 2. k]] ifFalse: [nil]\nr",
        // and:
        "r := nil\ntrue and: [r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // or:
        "r := nil\nfalse or: [r := #(1, 3) groupBy: [:x | k := x > 2. k]. true]\nr",
        // at:ifAbsent:
        "r := nil\n#{#a => 1} at: #k ifAbsent: [r := #(1, 3) groupBy: [:x | k := x > 2. k]. 0]\nr",
        // tryDo:
        "r := nil\nResult tryDo: [r := #(1, 3) groupBy: [:x | k := x > 2. k]. 0]\nr",
        // on:do: handler
        "r := nil\n[Error signal: \"x\"] on: Error do: [:e | r := #(1, 3) groupBy: [:x | k := x > 2. k]. 0]\nr",
        // keysAndValuesDo:
        "r := nil\n#{#a => 1} keysAndValuesDo: [:kk :v | r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        // eachWithIndex:
        "r := nil\n#(1) eachWithIndex: [:z :j | r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        // [...] value
        "r := nil\n[r := #(1, 3) groupBy: [:x | k := x > 2. k]] value\nr",
        // Listed blocks nested in each other: single level only, nesting is
        // unmeasured.
        "r := nil\ntrue ifTrue: [#(1) do: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]]]\nr",
        "r := nil\n[#(1) collect: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. z]. 0] on: Error do: [:e | 0]\nr",
        // Measured right, but keyed on every enclosing role: an arm in a
        // loop body stays rejected (loud, not silent).
        "r := nil\n1 to: 1 do: [:z | true ifTrue: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]]\nr",
    ] {
        let src = in_actor(body);
        let messages = residual_messages(&src, None);
        assert!(
            messages.iter().any(|m| m.contains("BT-3897")),
            "{src}: {messages:?}"
        );
    }
    // The positions that unpack it (measured right) and a pure block.
    for body in [
        "#(1, 3) groupBy: [:x | k := x > 2. k]\nnil",
        // The method's last statement, its reply (`lastStatement` in
        // adr0133residuals_test.bt answers the map).
        "#(1, 3) groupBy: [:x | k := x > 2. k]",
        "r := #(1, 3) groupBy: [:x | k := x > 2. k]\nr at: true",
        "^#(1, 3) groupBy: [:x | k := x > 2. k]",
        "r := nil\n#(1) do: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr at: true",
        "r := nil\ntrue ifTrue: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr at: true",
        "#(1) do: [:z | #(1, 3) groupBy: [:x | k := x > 2. k]. nil]\nnil",
        // Assignments in the other blocks measured to unpack the fold
        // (`adr0133residuals_test.bt`), and a discarded statement in a loop.
        "r := nil\n[r := #(1, 3) groupBy: [:x | k := x > 2. k]. 0] on: Error do: [:e | 0]\nr",
        "r := nil\n[nil] ensure: [r := #(1, 3) groupBy: [:x | k := x > 2. k]]\nr",
        "r := nil\n#(1) collect: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. z]\nr",
        "r := nil\n#(1) inject: 0 into: [:a :z | r := #(1, 3) groupBy: [:x | k := x > 2. k]. a]\nr",
        "r := nil\n#(1) detect: [:z | false] ifNone: [r := #(1, 3) groupBy: [:x | k := x > 2. k]. 0]\nr",
        "1 timesRepeat: [#(1, 3) groupBy: [:x | k := x > 2. k]. nil]\nnil",
        "(#(1, 3) groupBy: [:x | x > 2]) at: true",
    ] {
        let src = in_actor(body);
        assert!(residual_messages(&src, None).is_empty(), "{src}");
    }
    let field = "Actor subclass: Ac\n  state: m = nil\n  probe =>\n    self.m := #(1, 3) groupBy: [:x | k := x > 2. k]\n    self.m\n";
    assert!(residual_messages(field, None).is_empty());
    for src in [
        in_class("(#(1, 3) groupBy: [:x | k := x > 2. k]) at: true"),
        in_value("(#(1, 3) groupBy: [:x | k := x > 2. k]) at: true"),
    ] {
        assert!(residual_messages(&src, None).is_empty(), "{src}");
    }
    // The REPL: only a top-level statement or assignment unpacks it.
    for input in [
        "(#(1, 3) groupBy: [:x | k := x > 2. k]) at: true\n",
        "#(1) do: [:z | #(1, 3) groupBy: [:x | k := x > 2. k]. nil]\n",
        "r := nil. #(1) do: [:z | r := #(1, 3) groupBy: [:x | k := x > 2. k]]\n",
    ] {
        let messages = residual_messages(input, Some(&[]));
        assert!(
            messages.iter().any(|m| m.contains("BT-3897")),
            "{input}: {messages:?}"
        );
    }
    for input in [
        "#(1, 3) groupBy: [:x | k := x > 2. k]\n",
        "r := #(1, 3) groupBy: [:x | k := x > 2. k]\n",
    ] {
        assert!(residual_messages(input, Some(&[])).is_empty(), "{input}");
    }
    // At the REPL only a local or field write threads the fold (the shared
    // `needs_mutation_threading` rule): a block that forwards an opaque
    // callable to a HOM is the plain `groupBy:` and answers the map
    // anywhere (PR #4383 review).
    let input = "(#(1, 3) groupBy: [:x | #(3) anySatisfy: f]) at: true\n";
    assert!(residual_messages(input, Some(&["f"])).is_empty(), "{input}");
}

/// PIN-BUG BT-3947 (closes BT-3875): a REPL `match:` arm that writes an
/// outer local draws the same `match:` arm error as a method (BT-3817), not
/// the value-position error it drew before BT-3909 (maintainer-approved REPL
/// output change), and only that one. BT-3947 lowers `match:` arms as frames,
/// deletes the `MatchArmWrite` row and flips this to accepted.
#[test]
fn residual_repl_match_arm_write_is_the_match_arm_error() {
    let input = "r := 3 match: [1 -> (t := 5); _ -> (t := t + 9)]\n";
    let messages = residual_messages(input, Some(&["t"]));
    assert_eq!(
        messages,
        vec![
            "a `match:` arm writes outer local `t`, but `match:` cannot return the write"
                .to_string();
            2
        ]
    );
    // An arm binding a local of its own is not an outer write; it is still
    // the value-position error.
    let messages = residual_messages("r := 3 match: [_ -> (z := 5)]\n", Some(&[]));
    assert!(
        messages.iter().all(|m| !m.contains("`match:` arm")),
        "{messages:?}"
    );
}
