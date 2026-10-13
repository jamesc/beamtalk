// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3745 (ADR 0131 Phase 0b): the §6 error for a Tier 2 block value with no
//! return channel, and the Phase 0 allow-set error for a local-threading
//! construct in a position that does not thread its outer-local writes yet (BT-3743).

use super::*;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

fn parse_module(src: &str) -> Module {
    let (module, diags) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "fixture must parse: {diags:?}"
    );
    module
}

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
    assert_eq!(
        d.message.as_str(),
        "block writes outer local `t`, but `ap:` cannot return the write"
    );
    let hint = d.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("return the new value from the block"),
        "{hint}"
    );
    assert!(hint.contains("`do:`, `inject:into:`, `on:do:`"), "{hint}");
    // Points at the block, with a note at the write.
    let block_start = src.find("[t := t + 1. 1]").unwrap();
    assert_eq!(d.span.start() as usize, block_start);
    assert!(
        d.notes
            .iter()
            .any(|n| n.message.contains("`t` is written here"))
    );
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

#[test]
fn section6_stored_closure_invoked_directly_is_an_error_outside_actors() {
    // s4: `b := [t := t + 1]. b value` — reported at the binding.
    for src in [
        in_class("b := [t := t + 1]\nb value\nt"),
        in_value("b := [t := t + 1]\nb value\nt"),
    ] {
        let diags = adr0131_diagnostics(&src);
        let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
        assert_eq!(s6.len(), 1, "{src}: {diags:?}");
        assert_eq!(
            s6[0].message.as_str(),
            "block stored in `b` writes outer local `t`, but `value` cannot return the write"
        );
        assert_eq!(
            s6[0].span.start() as usize,
            src.find("b := [").unwrap(),
            "the diagnostic points at the binding"
        );
    }
}

#[test]
fn section6_stored_closure_passed_on_through_a_user_hom_is_an_error() {
    let src = in_class("b := [t := t + 1]\nCvA ap: b\nt");
    let diags = adr0131_diagnostics(&src);
    let s6 = of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel);
    assert_eq!(s6.len(), 1, "{diags:?}");
    assert!(s6[0].message.contains("`ap:` cannot return the write"));
}

#[test]
fn section6_reassigned_local_is_not_a_tier2_block_value() {
    let src = in_class("b := [t := t + 1]\nb := [0]\nb value\nt");
    assert!(adr0131_diagnostics(&src).is_empty());
}

#[test]
fn section6_shadowing_block_parameter_is_not_the_tier2_local() {
    let src = in_class("b := [t := t + 1]\n#(1, 2) do: [:b | b printString]\nt");
    let diags = adr0131_diagnostics(&src);
    assert!(diags.is_empty(), "{diags:?}");
}

/// Each actor §6 exemption is exactly the syntactic shape codegen threads,
/// and each near miss is rejected. The exempt shapes are the methods of the
/// compiled fixture `stdlib/test/fixtures/adr0131section6exemptions_actor.bt`,
/// whose `BUnit` test (`adr0131section6exemptions_test.bt`) asserts they really
/// thread the write; this test checks the fixture compiles clean, so a change
/// to either the exemptions or codegen's recognizers (`detect_tier2_self_send`,
/// `prescan_tier2_local_vars`/`is_tier2_value_call`,
/// `inline_block_captured_mutations`) shows up as a failure.
#[test]
fn section6_exemptions_match_codegen_shapes() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    let fixture = std::fs::read_to_string(root.join("adr0131section6exemptions_actor.bt"))
        .expect("exemption fixture");
    let diags = adr0131_diagnostics(&fixture);
    assert!(diags.is_empty(), "every exempt shape compiles: {diags:?}");

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
// BT-3755: the actor self-send exemption needs a callee that threads the block
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

/// The callee shapes the exemption keeps: a `sealed` same-class method (or a
/// method of a `sealed` class) whose every use of the parameter is a direct
/// `value` send as a body statement, assignment right-hand side or `^` value.
/// These are the methods of `adr0131section6exemptions_actor.bt`, whose
/// `BUnit` test asserts they thread the write at runtime.
#[test]
fn section6_actor_self_send_to_a_threading_callee_is_exempt() {
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
        assert_eq!(section6_errors(&actor_calling("", callee)), 0, "{callee}");
    }
    // A `sealed` class seals every method.
    assert_eq!(
        section6_errors(&actor_calling("sealed ", "  ap: b => b value")),
        0
    );
    // The block's position selects the parameter that must thread.
    let two = "Actor subclass: Ac\n  sealed ap: a with: b => b value\n\n  probe =>\n    t := 0\n    self ap: 1 with: [t := t + 1]\n    t\n";
    assert_eq!(section6_errors(two), 0);
}

/// Each of these answered a wrong value or raised before BT-3755 (probed on
/// the debug build: the write was dropped, or `stateful_block_dispatch` / a
/// `perform:` arity error was raised), and is a compile error until the
/// callee-side `{Result, StateAcc}` protocol (BT-3742) lets a callee hand the
/// write back. Two are conservative rather than probed wrong: a callee that
/// passes the block on to another same-class callee (BT-3755) is rejected
/// only because the target does not thread it, and an overridable callee, which is
/// only wrong when a subclass overrides it. Widening the exemption
/// (`tier2_callee::callee_threads_block_param`) fails this test.
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

/// The inert pending fixture holds the rejected callee shapes as runtime
/// programs: every `probe*` method is rejected and its `callee*` helpers are
/// not.
#[test]
fn adr0131_section6_callee_pins() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    let name = "adr0131section6callee_actor_pending.bt.pending";
    let src = std::fs::read_to_string(root.join(name)).unwrap_or_else(|e| panic!("{name}: {e}"));
    let module = parse_module(&src);
    let diags = adr0131_diagnostics(&src);
    let (mut probes, mut callees) = (0, 0);
    for method in &module.classes[0].methods {
        let selector = method.selector.name();
        let rejected = diags
            .iter()
            .any(|d| method.span.start() <= d.span.start() && d.span.end() <= method.span.end());
        if selector.starts_with("probe") {
            probes += 1;
            assert!(rejected, "{name}: `{selector}` is no longer rejected");
        } else {
            callees += 1;
            assert!(!rejected, "{name}: helper `{selector}` is rejected");
        }
    }
    assert!(
        probes >= 4 && callees >= 4,
        "{probes} probes, {callees} callees"
    );
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
fn section6_class_side_self_send_has_no_return_channel() {
    let diags = adr0131_diagnostics(&in_class("self ap: [t := t + 1]\nt"));
    assert_eq!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).len(),
        1,
        "{diags:?}"
    );
}

#[test]
fn section6_erlang_ffi_argument_is_exempt() {
    // s6: lossy by design (ADR 0041 §Erlang Interop Boundary).
    for src in [
        in_class("r := (Erlang lists) map: [:x | t := t + 1. x] with: #(1)\n#[r, t]"),
        in_value("r := Erlang lists map: [:x | t := t + 1. x] with: #(1)\n#[r, t]"),
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
    // o1.
    let src = in_class("r := ([t := t + 1. 1] on: Error do: [:e | 0]) + 1\n#[r, t]");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    let d = p0[0];
    assert_eq!(d.severity, Severity::Error);
    assert_eq!(
        d.message.as_str(),
        "`on:do:` writes outer local `t`, but as the receiver of a message in a class \
         method the write is not threaded back yet (BT-3743)"
    );
    assert!(
        d.hint
            .as_deref()
            .is_some_and(|h| h.contains("in a statement of its own")),
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
    // o5 in a class method: the tuple is stored in the class variable.
    let src = "Object subclass: Cm\n  classState: n = 0\n\n  class probe =>\n    t := 0\n    self.n := [t := t + 1. 1] on: Error do: [:e | 0]\n    t\n";
    let diags = adr0131_diagnostics(src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert!(p0[0].message.contains("the value of a field assignment"));
}

#[test]
fn allow_set_nested_assignment_crossing_a_block_is_an_error() {
    // An assignment inside a loop body whose construct writes a method-level
    // local needs the loop to thread it on.
    let src = in_class(
        "r := 0\n#(1, 2) do: [:x | r := x > 1 ifTrue: [t := t + x. 1] ifFalse: [0]]\n#[r, t]",
    );
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
    // Still rejected inside a block that is not a threaded frame body: an arm.
    let arm = "true ifTrue: [r := #(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]]\nt";
    for src in [in_class(arm), in_value(arm)] {
        assert_eq!(
            of_category(
                &adr0131_diagnostics(&src),
                DiagnosticCategory::UnmigratedLocalThreading
            )
            .len(),
            1,
            "{src}"
        );
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

/// PIN-BUG BT-3743: every method in the inert `*_pending.bt.pending` probe
/// fixtures is rejected by an ADR 0131 diagnostic today, and no method in the
/// compiled fixtures is. A phase of the BT-3743 epic that makes a shape
/// compile moves it out of the pending fixture (and its test into
/// `adr0131local_rebind_test.bt`), which keeps this green; phase 5 (BT-3751)
/// deletes the allow-set and with it the pending allow-set shapes.
#[test]
fn adr0131_probe_matrix_pins() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test/fixtures");
    let read = |name: &str| {
        std::fs::read_to_string(root.join(name)).unwrap_or_else(|e| panic!("{name}: {e}"))
    };
    for context in ["class", "value", "actor"] {
        let compiled = format!("adr0131local_rebind_{context}.bt");
        let diags = adr0131_diagnostics(&read(&compiled));
        assert!(diags.is_empty(), "{compiled}: {diags:?}");

        let pending = format!("adr0131local_rebind_{context}_pending.bt.pending");
        let src = read(&pending);
        let module = parse_module(&src);
        let diags = adr0131_diagnostics(&src);
        let class = &module.classes[0];
        let methods = class.methods.iter().chain(class.class_methods.iter());
        let mut count = 0;
        for method in methods {
            count += 1;
            assert!(
                diags
                    .iter()
                    .any(|d| method.span.start() <= d.span.start()
                        && d.span.end() <= method.span.end()),
                "{pending}: `{}` is no longer rejected by an ADR 0131 diagnostic; \
                 move it (and its test) out of the pending files",
                method.selector.name()
            );
        }
        assert!(count > 0, "{pending} has no methods");
    }
}

/// ADR 0131 §6 is permanent for s4 in class and value-type methods (a
/// stored closure invoked directly): the `BUnit` matrix no longer has a cell
/// for it, so it is pinned here.
#[test]
fn adr0131_probe_s4_is_a_permanent_section6_error() {
    for src in [
        "Object subclass: P\n  class s4 =>\n    t := 0\n    b := [t := t + 1]\n    b value\n    t\n",
        "Value subclass: P\n  s4 =>\n    t := 0\n    b := [t := t + 1]\n    b value\n    t\n",
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
fn section6_sibling_scope_parameter_is_not_an_earlier_tier2_local() {
    let src = in_value(
        "#(1) do: [:e | blk := [t := t + 1]. blk]\n#(1, 2) do: [:blk | blk printString]\nt",
    );
    let diags = adr0131_diagnostics(&src);
    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn section6_destructuring_rebind_is_a_reassignment() {
    let src = in_class("b := [t := t + 1]\n#[b, _x] := #[[0], 1]\nb value\nt");
    let diags = adr0131_diagnostics(&src);
    assert!(diags.is_empty(), "{diags:?}");
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
        // Inside an arm: the construct's frame is not the method root, and an
        // arm is not a loop or fold body (BT-3793 only accepts those).
        "true ifTrue: [r := (#(1, 2) collect: [:x | t := t + x]) size]\nt",
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
    ] {
        let diags = adr0131_diagnostics_with(src, &known);
        assert!(diags.is_empty(), "{src}: {diags:?}");
    }
}

#[test]
fn repl_producer_sites_stop_at_blocks_conditionals_and_statement_constructs() {
    let known = ["t", "u"];
    for src in [
        // Inside a block that is not a loop or fold body.
        "[(#(1, 2) collect: [:x | t := t + x]) size] value\n",
        "true ifTrue: [(#(1, 2) collect: [:x | t := t + x]) size]\n",
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
        // A write inside a block.
        "true ifTrue: [self.n := (#(1, 2) collect: [:x | t := t + x]) size]\nt",
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
    let src = in_class("4 > 3 ifTrue: [#(1, 2) do: [:e | t := t + e]]\nt");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    assert_eq!(
        p0[0].message.as_str(),
        "`do:` writes outer local `t`, but as a statement inside a conditional arm in a \
         class method the write is not threaded back yet (BT-3743)"
    );
    assert!(
        p0[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("not inside a conditional arm")),
        "{:?}",
        p0[0]
    );
}

#[test]
fn deny_set_method_body_statement_is_rejected_per_context() {
    // `eachWithIndex:` loses the write as a class or value-type method
    // statement and answers right in an actor method.
    let body = "#(1, 2) eachWithIndex: [:x :i | t := t + x]\nt";
    for src in [in_class(body), in_value(body)] {
        let diags = adr0131_diagnostics(&src);
        let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
        assert_eq!(p0.len(), 1, "{src}: {diags:?}");
        assert!(p0[0].message.contains("as a statement in"), "{:?}", p0[0]);
    }
    assert!(adr0131_diagnostics(&in_actor(body)).is_empty());
}

#[test]
fn deny_set_block_local_writes_are_not_in_scope() {
    // A construct that writes only a local of its own enclosing block
    // (`crosses: false`) is BT-3776, not rejected here.
    let src = in_value(
        "4 > 3 ifTrue: [\n  n := 0\n  #(1, 2) collect: [:i | n := n + 1. i]\n  t := t + n\n]\nt",
    );
    assert!(adr0131_diagnostics(&src).is_empty());
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

/// The construct kinds of the statement-position probe matrix: each body,
/// with `@` for the local it writes and `X` for an extra statement inside its
/// block (the `stateful` variants), and how much it adds to that local.
/// `%c` is the condition: the method parameter `c`, or `4 =:= 4` at the REPL.
const STATEMENT_PROBE_KINDS: &[(&str, &str, i64)] = &[
    ("do", "#(1, 2) do: [:e | @ := @ + eX]", 3),
    ("while", "[@ < 2] whileTrue: [@ := @ + 1X]", 2),
    ("times", "2 timesRepeat: [@ := @ + 1X]", 2),
    ("toDo", "1 to: 2 do: [:i | @ := @ + iX]", 3),
    (
        "inject",
        "#(1, 2) inject: 0 into: [:a :x | @ := @ + xX. a + x]",
        3,
    ),
    ("collect", "#(1, 2) collect: [:x | @ := @ + xX]", 3),
    ("ifTrue", "%c ifTrue: [@ := @ + 1X]", 1),
    ("ifNil", "nil ifNil: [@ := @ + 1X]", 1),
    ("and", "%c and: [@ := @ + 1X. true]", 1),
    ("onDo", "[@ := @ + 1X] on: Error do: [:e | 0]", 1),
    ("ensure", "[@ := @ + 1X] ensure: [nil]", 1),
    ("value", "[@ := @ + 1X] value", 1),
    ("ewi", "#(1, 2) eachWithIndex: [:x :i | @ := @ + xX]", 3),
    (
        "dsb",
        "#(1, 2) do: [:x | @ := @ + xX] separatedBy: [@ := @ + 10]",
        13,
    ),
    (
        "kv",
        "#{#a => 1, #b => 2} keysAndValuesDo: [:k :w | @ := @ + wX]",
        3,
    ),
    (
        "dinH",
        "#(1, 2) detect: [:e | e > 5] ifNone: [@ := @ + 1X]",
        1,
    ),
    (
        "dinS",
        "#(1, 2) detect: [:e | @ := @ + 1X. e > 5] ifNone: [nil]",
        2,
    ),
    ("any", "#(1, 2) anySatisfy: [:e | @ := @ + 1X. e > 5]", 2),
];

/// The block roles of the statement-position probe matrix, with `S` for the
/// probed statement.
const STATEMENT_PROBE_CONTAINERS: &[(&str, &str)] = &[
    ("top", "S"),
    ("arm", "%c ifTrue: [S]"),
    ("armTF", "%c ifTrue: [S] ifFalse: [nil]"),
    ("armNil", "nil ifNil: [S]"),
    ("armAnd", "%c and: [S. true]"),
    ("armNL", "%c ifTrue: [S. 5]"),
    ("prot", "[S. 7] on: Error do: [:e | 0]"),
    ("protLast", "[S] on: Error do: [:e | 0]"),
    ("ens", "[S. 7] ensure: [nil]"),
    ("ensLast", "[S] ensure: [nil]"),
    ("handler", "[Error signal: \"x\"] on: Error do: [:e | S. 0]"),
    ("ensBlk", "[nil] ensure: [S]"),
    ("loop", "i := 0. [i < 1] whileTrue: [i := i + 1. S]"),
    ("timesC", "1 timesRepeat: [S]"),
    ("toDoC", "1 to: 1 do: [:z | S]"),
    ("iter", "#(1) do: [:z | S]"),
    ("coll", "#(1) collect: [:z | S. z]"),
    ("val", "[S] value"),
    ("armIter", "#(1) do: [:z | %c ifTrue: [S]]"),
    ("iterArm", "%c ifTrue: [#(1) do: [:z | S]]"),
    (
        "armLoop",
        "i := 0. [i < 1] whileTrue: [i := i + 1. %c ifTrue: [S]]",
    ),
    ("protArm", "%c ifTrue: [[S. 7] on: Error do: [:e | 0]]"),
    ("iterIter", "#(1) do: [:y | #(1) do: [:z | S]]"),
    // REPL-only roles.
    ("replLoop", "[t < 100] whileTrue: [S. t := t + 100]"),
];

/// The probe statement for `kind` in `container`. `target` says which local
/// the construct writes and how the block reads it back: `outer` writes the
/// method-level `t`; `local` writes a local `u` of the innermost block and
/// then sets `t := u`; `direct` first writes `t := t + 0` in the block;
/// `dlocal` writes `u` and then `t := t + u`; `cvar`, `selfsend` and
/// `cvarOuter` write `t` with a class-variable write or a self-send inside
/// the construct's block, or a class-variable write before it.
fn statement_probe(kind: &str, container: &str, target: &str, cond: &str) -> String {
    let (_, snippet, _) = STATEMENT_PROBE_KINDS
        .iter()
        .find(|(k, _, _)| *k == kind)
        .unwrap_or_else(|| panic!("unknown kind {kind}"));
    let (_, template) = STATEMENT_PROBE_CONTAINERS
        .iter()
        .find(|(c, _)| *c == container)
        .unwrap_or_else(|| panic!("unknown container {container}"));
    let extra = match target {
        "cvar" => ". self.n := 1",
        "selfsend" => ". self plain",
        "cvarOuter" | "outer" | "local" | "direct" | "dlocal" => "",
        other => panic!("unknown target {other}"),
    };
    let snippet = snippet.replace('X', extra);
    let stmt = match target {
        "local" => format!("u := 0. {}. t := u", snippet.replace('@', "u")),
        "dlocal" => format!("u := 0. {}. t := t + u", snippet.replace('@', "u")),
        "direct" => format!("t := t + 0. {}", snippet.replace('@', "t")),
        "cvarOuter" => format!("self.n := 1. {}", snippet.replace('@', "t")),
        _ => snippet.replace('@', "t"),
    };
    template.replace('S', &stmt).replace("%c", cond)
}

/// PIN-BUG BT-3743: the statement-position probe matrix, measured on the
/// real build (BT-3753). Every shape `adr0131_statement_probes.tsv` records
/// as answering wrong, raising or failing to compile (`bad`) is rejected by
/// an ADR 0131 diagnostic, and every shape it records as answering right
/// (`ok`) is not, except where a [`DENY_SET`] row is coarser than one
/// measurement (`over`: the row's kind family or block role also covers
/// shapes measured wrong). `gap` lines (a construct that writes only a local
/// of its own enclosing block, which the enclosing block then loses) are
/// wrong today but not in BT-3753's scope; BT-3776 rejects or fixes them and
/// flips them to `bad` or `ok`. A phase that makes a shape answer right
/// deletes or narrows its row and flips its line to `ok`, which keeps this
/// green.
#[test]
fn adr0131_statement_probe_pins() {
    let table = include_str!("adr0131_statement_probes.tsv");
    let mut checked = 0;
    let mut wrong = Vec::new();
    for line in table
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
    {
        let cols: Vec<&str> = line.split('\t').collect();
        let [context, container, target, kind, verdict] = cols[..] else {
            panic!("bad line {line:?}");
        };
        let diags = if context == "repl" {
            let body = statement_probe(kind, container, target, "4 =:= 4");
            adr0131_diagnostics_with(&format!("{body}\n"), &["t"])
        } else {
            let body = statement_probe(kind, container, target, "c");
            let method = format!("probe: c =>\n    t := 0\n    {body}\n    t\n");
            let src = match context {
                "class" => format!(
                    "Object subclass: P\n  classState: n = 0\n  class plain => 0\n  class {method}"
                ),
                "value" => format!("Value subclass: P\n  plain => 0\n  {method}"),
                "actor" => format!("Actor subclass: P\n  state: n = 0\n  plain => 0\n  {method}"),
                other => panic!("unknown context {other}"),
            };
            adr0131_diagnostics(&src)
        };
        let rejected = !diags.is_empty();
        let expected_rejected = match verdict {
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
/// cover stays a Phase 0 error: an arm or closure around the construct, a
/// `detect:ifNone:` (its search-block write is dropped by the frames around
/// it), and, in an actor, a value-bearing site and a nested `do:` (not a
/// producer).
#[test]
fn loop_body_producer_accept_stops_where_codegen_does() {
    for (body, expect_error) in [
        // An arm between the frame and the construct.
        (
            "s := 0\n1 to: 3 do: [:i | true ifTrue: [[s < i] whileTrue: [s := s + 1]]]\ns",
            [true, true, true],
        ),
        // A closure between the frame and the construct.
        (
            "s := 0\n1 to: 3 do: [:i | [:k | [s < i] whileTrue: [s := s + 1]] value: 1]\ns",
            [true, true, true],
        ),
        (
            "t := 0\n#(1, 2) do: [:i | #(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [0]]\nt",
            [true, true, true],
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
];

#[test]
fn section6_escaping_blocks_are_errors_in_every_context() {
    for (name, body) in ESCAPE_SHAPES {
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
                d.message.contains("writes outer local `t`")
                    && d.message.contains("nothing can return the write"),
                "{name} in {context}: {}",
                d.message
            );
            let hint = d.hint.as_deref().unwrap_or_default();
            assert!(
                hint.contains("`self.t := self.t + 1`") && hint.contains("assign it where"),
                "{name} in {context}: {hint}"
            );
            assert!(
                d.notes
                    .iter()
                    .any(|n| n.message.contains("`t` is written here")),
                "{name} in {context}"
            );
        }
    }
}

#[test]
fn section6_escape_points_at_the_block_or_its_binding() {
    let src = in_class("^[t := t + 1]");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(
        diags[0].span.start() as usize,
        src.find("[t := t + 1]").unwrap()
    );
    assert!(diags[0].message.contains("returned from the method"));

    let src = in_class("b := [t := t + 1]\n^b");
    let diags = adr0131_diagnostics(&src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(diags[0].span.start() as usize, src.find("b := [").unwrap());
    assert!(
        diags[0]
            .notes
            .iter()
            .any(|n| n.message.contains("`b` escapes here")),
        "{diags:?}"
    );

    let src = in_actor("b := [t := t + 1]\nself.n := b\nnil");
    assert!(
        adr0131_diagnostics(&src)[0]
            .message
            .contains("stored in a field or class variable")
    );
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
    for body in [
        "^[self.n := self.n + 1]",
        "b := [self.n := self.n + 1]\n^b",
        "#[[self.n := self.n + 1]]",
    ] {
        assert_eq!(section6_errors(&in_actor(body)), 0, "{body}");
    }
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

#[test]
fn section6_escape_in_a_repl_expression_list_is_not_checked() {
    // The REPL's last value goes to the display and its locals are session
    // bindings; only sends are checked there.
    let diags = adr0131_diagnostics_with("t := 0\n[t := t + 1]", &[]);
    assert!(
        of_category(&diags, DiagnosticCategory::Tier2BlockNoReturnChannel).is_empty(),
        "{diags:?}"
    );
}

/// BT-3755 review: the help for a non-threading actor self-send callee names
/// exactly the two accepted non-direct forms, not "a collection message".
#[test]
fn section6_self_send_help_names_only_the_accepted_forms() {
    let src = actor_calling("sealed ", "  ap: b => b foo: 1");
    let diags = adr0131_diagnostics(&src);
    let hint = diags[0].hint.as_deref().unwrap_or_default();
    assert!(hint.contains("`#(1) do: b`"), "{hint}");
    assert!(hint.contains("`last := b value: e`"), "{hint}");
    assert!(!hint.contains("`inject:into:`"), "{hint}");
}
