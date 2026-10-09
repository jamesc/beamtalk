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
        "Object subclass: CvA\n  class ap: b => b value\n\nActor subclass: Ac\n  state: n = 0\n\n  ap: b => b value\n\n  probe =>\n    t := 0\n{}\n",
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
    let src = in_class("b := [t := t + 1]\n#(1, 2) do: [:b | b printString]\n#[b, t]");
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
    // o4.
    let src = in_class("r := (#(1, 2) collect: [:x | t := t + x]) size\n#[r, t]");
    let diags = adr0131_diagnostics(&src);
    let p0 = of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading);
    assert_eq!(p0.len(), 1, "{diags:?}");
    let d = p0[0];
    assert_eq!(d.severity, Severity::Error);
    assert_eq!(
        d.message.as_str(),
        "`collect:` writes outer local `t`, but as the receiver of a message in a class \
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
fn allow_set_is_per_context() {
    // `r := coll do: [...]` silently drops the write in class and value-type
    // methods and works in an actor method.
    let body = "r := #(1, 2) do: [:x | t := t + x]\nt";
    for src in [in_class(body), in_value(body)] {
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
    assert!(adr0131_diagnostics(&in_actor(body)).is_empty());
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
fn allow_set_detect_if_none_handler_write_is_an_error_everywhere() {
    // s2: a verifier panic (class), unbound `State` (value) or a silently
    // dropped write (actor) before BT-3745.
    let body = "r := #(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]\n#[r, t]";
    for src in [in_class(body), in_value(body), in_actor(body)] {
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
    // Only the search block writing still threads.
    let search = "r := #(1, 2) detect: [:x | t := t + 1. x > 1] ifNone: [0]\n#[r, t]";
    assert!(adr0131_diagnostics(&in_class(search)).is_empty());
}

#[test]
fn allow_set_repl_uses_its_own_row() {
    let known = ["t"];
    // REPL o4: receiver position.
    let diags = adr0131_diagnostics_with("(#(1, 2) collect: [:x | t := t + x]) size\n", &known);
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
fn allow_set_value_type_return_of_list_op_differs_from_implicit_last() {
    // Probed on the debug build (BT-3745): `^#(1, 2) inject: 0 into: [...]`
    // in a value-type method answers the leaked `{3, StateAcc}` tuple, while
    // the same construct as the implicit last statement answers 3. The BUnit
    // pins are `vtLastInject` (adr0131local_rebind_value.bt) and
    // `vtReturnInject` (adr0131local_rebind_value_pending.bt.pending).
    let ret = in_value("^#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]");
    assert_eq!(
        of_category(
            &adr0131_diagnostics(&ret),
            DiagnosticCategory::UnmigratedLocalThreading
        )
        .len(),
        1
    );
    let last = in_value("#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]");
    assert!(adr0131_diagnostics(&last).is_empty());
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
fn deny_set_state_access_changes_a_class_loop_body() {
    // A `do:` in a `to:do:` body loses the write, unless the loop body
    // touches the class's state (class_method_protected_locals.bt
    // `nestedDoInACountedLoop`, probed).
    let plain = in_class("1 to: 2 do: [:i | #(1, 2) do: [:j | t := t + j]]\nt");
    assert_eq!(adr0131_diagnostics(&plain).len(), 1);
    let stateful = in_class("1 to: 2 do: [:i | #(1, 2) do: [:j | t := t + j. self ap: [nil]]]\nt");
    assert!(
        of_category(
            &adr0131_diagnostics(&stateful),
            DiagnosticCategory::UnmigratedLocalThreading
        )
        .is_empty()
    );
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
fn deny_set_while_loop_in_a_loop_body_is_rejected_everywhere() {
    // BT-3746 follow-up (PR #4230): a `whileTrue:` nested as a statement in a
    // `to:do:` or `do:` body answers 0 instead of 3 in every method context.
    for body in [
        "s := 0\n1 to: 3 do: [:i | [s < i] whileTrue: [s := s + 1]]\ns",
        "s := 0\n#(1, 2, 3) do: [:i | [s < i] whileTrue: [s := s + 1]]\ns",
        "s := 0\n1 to: 3 do: [:i | [s >= i] whileFalse: [s := s + 1]]\ns",
    ] {
        for src in [in_class(body), in_value(body), in_actor(body)] {
            let diags = adr0131_diagnostics(&src);
            assert_eq!(
                of_category(&diags, DiagnosticCategory::UnmigratedLocalThreading).len(),
                1,
                "{src}: {diags:?}"
            );
        }
    }
}
