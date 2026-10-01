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
}

#[test]
fn stored_closure_passed_to_a_stdlib_hom_is_a_documented_gap() {
    // Not tracked (see the module docs): the check stays silent rather than
    // guess how a stdlib higher-order method invokes the block.
    let diags = advisories(&open(
        "  class run -> Integer =>
    b := [:x | self bump]
    #(1, 2) do: b
    0
",
    ));
    assert!(diags.is_empty(), "got: {diags:?}");
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
