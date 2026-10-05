// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3681 (ADR 0110): the stored-closure class-variable advisory. A block
//! bound to a local and invoked by a later statement, or passed to a
//! user-defined class-side higher-order method, that makes a class-side
//! self-send which may write a class variable loses that write.

use super::*;
use crate::source_analysis::DiagnosticCategory;

fn advisories(src: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::StoredClosure))
        .collect()
}

const OPEN_HEADER: &str = "Object subclass: Counter
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class pure -> Integer => 7

  class section: aBlock :: Block -> Integer => aBlock value

";

fn open(body: &str) -> String {
    format!("{OPEN_HEADER}{body}")
}

#[test]
fn stored_closure_invoked_later_warns_with_hint_and_invocation_note() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    b value
    self pure
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
    let d = &diags[0];
    assert_eq!(d.severity, Severity::Warning);
    assert!(d.message.contains("stored closure 'b'"), "{}", d.message);
    assert!(d.message.contains("'bump'"), "{}", d.message);
    assert!(
        d.hint
            .as_deref()
            .is_some_and(|h| h.contains("statement that builds it")),
        "hint: {:?}",
        d.hint
    );
    assert_eq!(d.notes.len(), 1, "an invocation note: {d:?}");
}

#[test]
fn stored_closure_with_overridable_pure_selector_warns() {
    // `pure` is provably free of class-variable mutation here, but a subclass
    // override may write (the ADR 0110 case), so an open class still warns.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self pure]
    b value
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn invocation_with_arguments_and_in_a_later_nested_block_counts() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [:x | self bump]
    #(1, 2) do: [:i | b value: i]
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn stored_closure_inside_a_nested_block_body_is_checked() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    [
      b := [self bump]
      b value
      self pure
    ] on: Error do: [:e | nil]
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn block_invoked_by_the_statement_that_builds_it_is_not_flagged() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    [self bump] value
    #(1, 2) collect: [:x | self pure]
    #(1, 2) do: [:x | self bump]
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn stored_closure_not_invoked_by_a_later_statement_is_not_flagged() {
    let never = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    self pure
",
    ));
    assert!(never.is_empty(), "never invoked: {never:?}");
}

#[test]
fn stored_closure_without_a_class_side_send_is_not_flagged() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [1 + 2]
    b value
    c := [:x | x printString]
    c value: 3
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn class_without_class_variables_is_not_flagged() {
    let diags = advisories(
        "Object subclass: NoState
  class foo -> Integer => 1

  class section: aBlock :: Block -> Integer => aBlock value

  class run -> Integer =>
    b := [self foo]
    b value
    self section: [self foo]
",
    );
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn sealed_class_flags_only_selectors_that_may_write() {
    let header = "sealed Object subclass: SealedCounter
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class pure -> Integer => 7

";
    let writing = advisories(&format!(
        "{header}  class run -> Integer =>
    b := [self bump]
    b value
    0
"
    ));
    assert_eq!(writing.len(), 1, "got: {writing:?}");
    let pure = advisories(&format!(
        "{header}  class run -> Integer =>
    b := [self pure]
    b value
    0
"
    ));
    assert!(
        pure.is_empty(),
        "a sealed class cannot be overridden: {pure:?}"
    );
}

#[test]
fn block_passed_to_user_defined_class_side_hom_warns() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    self section: [self bump]
    self n
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
    assert!(
        diags[0].message.contains("class-side 'section:'"),
        "{}",
        diags[0].message
    );
}

#[test]
fn block_passed_to_stdlib_hom_or_without_a_class_send_is_not_flagged() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    #(1, 2) inject: 0 into: [:a :x | self pure]
    self section: [1 + 1]
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn sealed_subclass_inheriting_a_mutating_selector_warns() {
    let diags = advisories(
        "Object subclass: Base
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

sealed Base subclass: Leaf
  class run -> Integer =>
    b := [self bump]
    b value
    0
",
    );
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn open_subclass_inheriting_a_class_sealed_mutating_selector_warns() {
    let diags = advisories(
        "Object subclass: Base
  classState: n = 0

  class sealed bump -> Integer => self.n := self.n + 1

Base subclass: Child
  class run -> Integer =>
    b := [self bump]
    b value
    0
",
    );
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn explicit_own_class_reference_binds_directly_so_only_mutating_selectors_warn() {
    let pure = advisories(&open(
        "  class run -> Integer =>
    b := [Counter pure]
    b value
    0
",
    ));
    assert!(
        pure.is_empty(),
        "`Counter pure` cannot be overridden and is pure: {pure:?}"
    );
    let writing = advisories(&open(
        "  class run -> Integer =>
    b := [Counter bump]
    b value
    0
",
    ));
    assert_eq!(writing.len(), 1, "got: {writing:?}");
}

#[test]
fn rebinding_the_local_before_invoking_it_is_not_flagged() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    b := [0]
    b value
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn stored_closure_handed_to_a_user_defined_hom_by_name_warns() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    self section: b
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
    // The compiler cannot see that `section:` invokes the block, so the text
    // must not claim it is invoked.
    let d = &diags[0];
    assert!(
        d.message.contains("passes it to class-side 'section:'"),
        "{}",
        d.message
    );
    assert!(!d.message.contains("it is invoked"), "{}", d.message);
    assert_eq!(
        d.notes[0].message.as_str(),
        "the closure is passed to class-side 'section:' here"
    );
}

#[test]
fn stored_closure_passed_to_a_stdlib_hom_warns() {
    // BT-3688: the collection HOM table (`opaque_fold_callable_arg`) names the
    // selectors whose callable argument the runtime invokes; a stored block
    // handed to one is invoked after its building statement's scope closed.
    for (call, hom) in [
        ("#(1, 2) do: b", "do:"),
        ("#(1, 2) collect: b", "collect:"),
        ("#(1, 2) inject: 0 into: b", "inject:into:"),
        ("#(1, 2) detect: b ifNone: [nil]", "detect:ifNone:"),
    ] {
        let diags = advisories(&open(&format!(
            "  class run -> Integer =>
    b := [:x | self bump]
    {call}
    0
"
        )));
        assert_eq!(diags.len(), 1, "{call}: {diags:?}");
        let d = &diags[0];
        assert!(d.message.contains(hom), "{call}: {}", d.message);
        assert!(d.message.contains("stored closure 'b'"), "{}", d.message);
        assert!(!d.message.contains("it is invoked"), "{}", d.message);
        assert_eq!(d.notes.len(), 1, "{d:?}");
    }
}

#[test]
fn stored_closure_passed_to_a_non_invoking_send_is_not_flagged() {
    // Only the collection HOM table counts; `add:` merely stores the block.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    #() add: b
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn later_block_parameter_shadowing_the_stored_local_is_not_flagged() {
    // BT-3688: `:b` is a different binding, not the stored block.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    #(1) do: [:b | b value]
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
    // The shadow only covers its own block: a later use outside it is the
    // stored block again.
    let after = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    #(1) do: [:b | b value]
    b value
    0
",
    ));
    assert_eq!(after.len(), 1, "got: {after:?}");
    // A multi-parameter list shadows too.
    let multi = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    #(1) inject: 0 into: [:acc :b | b value]
    0
",
    ));
    assert!(multi.is_empty(), "got: {multi:?}");
}

#[test]
fn rebind_inside_a_later_block_before_the_invocation_is_not_flagged() {
    // BT-3688: within that block's own statement sequence the local is the
    // new block by the time it is invoked.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    true ifTrue: [
      b := [0]
      b value
    ]
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
}

#[test]
fn invocation_after_a_conditional_rebind_still_warns_by_design() {
    // BT-3688, closed as intended: when the branch is not taken, `b` is still
    // the stored block, so the invocation may reach it. The check stays
    // conservative (a rebind in every branch is not proved) rather than guess.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [self bump]
    true ifTrue: [b := [0]]
    b value
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}

#[test]
fn standalone_class_side_method_definitions_are_checked() {
    // BT-3688: `Counter class >> run => ...` is a class method of `Counter`.
    let stored = advisories(&open(
        "
Counter class >> runStandalone -> Integer =>
  b := [self bump]
  b value
  0
",
    ));
    assert_eq!(stored.len(), 1, "got: {stored:?}");
    let hom = advisories(&open(
        "
Counter class >> homStandalone -> Integer =>
  self section: [self bump]
",
    ));
    assert_eq!(hom.len(), 1, "got: {hom:?}");
    // Instance-side standalone methods and non-stateful classes stay silent.
    let instance = advisories(&open(
        "
Counter >> runInstance -> Integer =>
  b := [Counter bump]
  b value
  0
",
    ));
    assert!(instance.is_empty(), "got: {instance:?}");
    let stateless = advisories(
        "Object subclass: NoState
  class foo -> Integer => 1

NoState class >> run -> Integer =>
  b := [self foo]
  b value
  0
",
    );
    assert!(stateless.is_empty(), "got: {stateless:?}");
}

#[test]
fn inherited_pure_looking_method_that_self_sends_still_warns() {
    // BT-3688 review: `pure` is `class sealed` and its own body writes nothing,
    // but its `self helper` late-binds to the receiving class, whose override
    // writes. Judging it by the defining class's mutating set alone would miss
    // that, so an inherited method that makes any `self` send stays flagged.
    let base = "Object subclass: Base
  class sealed pure -> Integer => self helper

  class sealed nested -> Integer => #(1) inject: 0 into: [:a :x | self helper]

  class helper -> Integer => 0

";
    for (kind, leaf) in [
        ("sealed", "sealed Base subclass: Leaf"),
        ("open", "Base subclass: Leaf"),
    ] {
        for call in ["pure", "nested"] {
            let diags = advisories(&format!(
                "{base}{leaf}
  classState: n = 0

  class helper -> Integer => self.n := self.n + 1

  class run -> Integer =>
    b := [self {call}]
    b value
    0
"
            ));
            assert_eq!(diags.len(), 1, "{kind} Leaf, {call}: {diags:?}");
        }
    }
}

#[test]
fn collection_hom_check_skips_self_and_super_receivers() {
    // BT-3688 review: `super do: b` / `self do: b` reach a (possibly
    // user-defined) method, not the collection HOM, so the definite
    // "which invokes it" claim must not be made for them.
    let header = "Object subclass: Base
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class do: aBlock :: Block -> Integer => aBlock value: 1

";
    let via_super = advisories(&format!(
        "{header}Base subclass: Child
  class run -> Integer =>
    b := [:x | self bump]
    super do: b
    0
"
    ));
    assert!(via_super.is_empty(), "got: {via_super:?}");
    // `self do: b` resolves to the user-defined class-side `do:`, which keeps
    // the hedged user-HOM wording rather than the collection one.
    let via_self = advisories(&format!(
        "{header}Base subclass: Child
  class run -> Integer =>
    b := [:x | self bump]
    self do: b
    0
"
    ));
    assert_eq!(via_self.len(), 1, "got: {via_self:?}");
    assert!(
        via_self[0].message.contains("which may invoke it"),
        "{}",
        via_self[0].message
    );
}

#[test]
fn pure_class_sealed_method_inherited_from_a_module_parent_is_not_flagged() {
    // BT-3688: the parent is in this module, so its body is visible; a pure
    // `class sealed` method cannot be overridden and writes nothing.
    let header = "Object subclass: Base
  classState: n = 0

  class sealed pure -> Integer => 7

  class plain -> Integer => 7

  class bump -> Integer => self.n := self.n + 1

";
    let pure_sealed = advisories(&format!(
        "{header}Base subclass: Child
  class run -> Integer =>
    b := [self pure]
    b value
    0
"
    ));
    assert!(pure_sealed.is_empty(), "got: {pure_sealed:?}");
    // An overridable pure method on an open child may still be overridden to
    // write, so it keeps warning; a sealed child cannot be overridden.
    let open_child = advisories(&format!(
        "{header}Base subclass: Child
  class run -> Integer =>
    b := [self plain]
    b value
    0
"
    ));
    assert_eq!(open_child.len(), 1, "got: {open_child:?}");
    let sealed_child = advisories(&format!(
        "{header}sealed Base subclass: Leaf
  class run -> Integer =>
    b := [self plain]
    b value
    0
"
    ));
    assert!(sealed_child.is_empty(), "got: {sealed_child:?}");
}

#[test]
fn keyword_selector_help_text_does_not_interpolate_the_selector() {
    let diags = advisories(
        "Object subclass: Counter
  classState: n = 0

  class bumpBy: k :: Integer -> Integer => self.n := self.n + k

  class run -> Integer =>
    b := [self bumpBy: 2]
    b value
    0
",
    );
    assert_eq!(diags.len(), 1, "got: {diags:?}");
    assert!(
        !diags[0].message.contains("[self bumpBy:]"),
        "{}",
        diags[0].message
    );
}

#[test]
fn hom_block_in_a_loop_body_is_reported_once() {
    let diags = advisories(&open(
        "  class run -> Integer =>
    #(1) do: [:x | self section: [self bump]]
    0
",
    ));
    assert_eq!(diags.len(), 1, "got: {diags:?}");
}
