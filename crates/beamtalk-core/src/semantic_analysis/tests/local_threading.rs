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

#[test]
fn section6_actor_self_send_and_stored_value_compile_unchanged() {
    // BT-912: an actor instance method can hand a Tier 2 block to a
    // self-send, call a stored one, or fold it with a collection HOM.
    for body in [
        "r := self ap: [t := t + 1. 1]\n#[r, t]",
        "b := [t := t + 1]\nb value\nt",
        "b := [t := t + 1]\nself ap: b\nt",
        "b := [:x | t := t + x]\n#(1, 2) do: b\nt",
    ] {
        let src = in_actor(body);
        let diags = adr0131_diagnostics(&src);
        assert!(diags.is_empty(), "{body}: {diags:?}");
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
