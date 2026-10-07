// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3712 (ADR 0130 §5): the `class-state-abroad` lint. A block literal that
//! reads a class variable and is handed to an asynchronous send, stored or
//! returned, or that writes one and is handed to another class's class-side
//! method, runs outside an invocation of its home class. Blocks that stay at
//! home never warn.

use super::*;
use crate::compilation::diagnostics_policy::apply_expect_directives;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

fn parse_module(src: &str) -> Module {
    let (module, diags) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "fixture must parse: {diags:?}"
    );
    module
}

fn abroad(src: &str) -> Vec<Diagnostic> {
    analyse(&parse_module(src))
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::ClassStateAbroad))
        .collect()
}

const HEADER: &str = "Actor subclass: Worker
  state: blk = nil

  keep: aBlock => self.blk := aBlock

  run => self.blk value

Object subclass: Driver
  classState: ticks = 0

  class each: aBlock => aBlock value

Object subclass: OpenStateless

  class each: aBlock => aBlock value

Object subclass: Lib

  class sealed each: aBlock => aBlock value

sealed Object subclass: SealedDriver

  class sealed each: aBlock => aBlock value

Object subclass: Counter
  classState: n = 0

  class bump => self.n := self.n + 1

  class pure => 7

  class run: aBlock => aBlock value

";

/// Appends methods to `Counter` (the last class of [`HEADER`]) and lints.
fn only(body: &str) -> Vec<Diagnostic> {
    abroad(&format!("{HEADER}{body}"))
}

// ---- (a) reads ------------------------------------------------------------

#[test]
fn returned_block_reading_a_class_variable_warns() {
    let d = only("  class reader => [self.n]\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!(d[0].severity, Severity::Warning);
    assert!(d[0].message.contains("returned"), "{}", d[0].message);
    assert!(
        d[0].message
            .contains("reads the values captured at creation"),
        "{}",
        d[0].message
    );
}

#[test]
fn explicit_return_of_a_reading_block_warns() {
    let d = only("  class reader =>\n    ^[self.n]\n    nil\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn stored_in_local_class_variable_or_literal_warns() {
    let local = only("  class a =>\n    b := [self.n]\n    self bump\n    b value\n");
    assert_eq!(local.len(), 1, "{local:?}");
    assert!(local[0].message.contains("stored in a local"));

    let literal = only("  class a => #([self.n], [1])\n");
    assert_eq!(literal.len(), 1, "{literal:?}");
    assert!(literal[0].message.contains("collection literal"));

    let in_class_var = abroad(
        "Object subclass: Holder
  classState: n = 0
  classState: cb = nil

  class store =>
    self.cb := [self.n]
    nil
",
    );
    assert_eq!(in_class_var.len(), 1, "{in_class_var:?}");
    assert!(
        in_class_var[0]
            .message
            .contains("stored in a class variable")
    );
}

#[test]
fn reading_block_passed_to_an_actor_warns() {
    let d =
        only("  class rowAsyncRead =>\n    a := Worker spawn\n    a keep: [self.n]\n    a run\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(
        d[0].message
            .contains("handed to an actor by 'keep:', which may keep it"),
        "{}",
        d[0].message
    );
}

#[test]
fn reading_block_passed_to_a_cast_warns() {
    let d = only("  class a: w => w keep: [self.n]!\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(
        d[0].message.contains("asynchronous cast"),
        "{}",
        d[0].message
    );
}

#[test]
fn reading_block_passed_to_timer_warns() {
    let d = only("  class a => Timer after: 10 do: [self.n]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn standalone_class_method_definitions_are_checked() {
    let d = abroad(
        "Object subclass: Holder
  classState: n = 0

Holder class >> reader => [self.n]
",
    );
    assert_eq!(d.len(), 1, "{d:?}");
}

// ---- (b) writes -----------------------------------------------------------

#[test]
fn writing_block_passed_to_a_stateful_class_warns() {
    let d = only("  class a => Driver each: [self bump]\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(
        d[0].message.contains("class_state_unreachable"),
        "{}",
        d[0].message
    );
    assert!(d[0].message.contains("Driver each:"), "{}", d[0].message);
}

#[test]
fn class_sealed_method_of_an_open_stateless_class_still_warns() {
    // Not direct-called: the class is not sealed, so `each:` runs in `Lib`'s
    // gen_server even though the method is `class sealed` and `Lib` has no
    // class variables (codegen's direct-call gate 1).
    let d = only("  class a => Lib each: [self bump]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn directly_writing_block_passed_to_a_stateful_class_warns() {
    let d = only("  class a => Driver each: [self.n := 1]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn writing_block_passed_to_an_open_stateless_class_warns() {
    // Not `class sealed`: the method runs in that class's own process.
    let d = only("  class a => OpenStateless each: [self bump]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn write_through_a_nested_block_and_own_class_reference_warns() {
    let d = only("  class a => Driver each: [#(1) do: [:x | Counter bump]]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn write_through_a_transitively_mutating_self_send_warns() {
    let d = only("  class viaHelper => self bump\n  class a => Driver each: [self viaHelper]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn late_bound_self_send_that_a_subclass_may_override_warns() {
    // `pure` is free of class-variable writes here, but `self pure` late-binds
    // and an open class's subclass may override it to write.
    let d = only("  class a => Driver each: [self pure]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn self_send_in_a_sealed_class_is_proven_pure() {
    let d = abroad(&format!(
        "{HEADER}sealed Object subclass: Sealed
  classState: m = 0

  class pure => 7

  class a => Driver each: [self pure]
"
    ));
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn write_of_an_inherited_class_variable_is_a_write() {
    let d = abroad(
        "Object subclass: Driver
  class each: aBlock => aBlock value

Object subclass: Base
  classState: n = 0

Base subclass: Sub

  class sealed bumpN => self.n := self.n + 1

  class a => Driver each: [Sub bumpN]
",
    );
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn read_message_is_conditional_on_running_abroad() {
    let d = only("  class reader => [self.n]\n");
    assert!(
        d[0].message
            .contains("if run outside an invocation of Counter, it reads the values captured"),
        "{}",
        d[0].message
    );
}

// ---- never fires for blocks that stay at home ------------------------------

#[test]
fn inlined_control_flow_never_warns() {
    let d = only(
        "  class a: x =>
    x > 0 ifTrue: [self bump]
    i := 0
    [i < 3] whileTrue: [self bump. i := i + 1]
    3 timesRepeat: [self bump]
    1 to: 3 do: [:j | self bump. self.n]
    x > 1 ifTrue: [self.n] ifFalse: [self.n + 1]
",
    );
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn same_class_higher_order_methods_never_warn() {
    let d = only(
        "  class a =>
    self run: [self bump]
    Counter run: [self bump]
    self run: [self.n]
",
    );
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn collection_iteration_on_a_local_collection_never_warns() {
    let d = only(
        "  class a =>
    sum := 0
    #(1, 2, 3) do: [:x | sum := sum + x. self bump]
    #(1, 2, 3) collect: [:x | self.n + x]
    #(1, 2, 3) inject: 0 into: [:acc :x | self bump. acc + x]
    items := #(1, 2)
    items do: [:x | self bump]
    items collect: [:x | self.n]
",
    );
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn on_do_ensure_and_result_try_do_never_warn() {
    let d = only(
        "  class a =>
    [self bump] on: Error do: [:e | self bump]
    [self bump] ensure: [self bump]
    [self.n] on: Error do: [:e | self.n]
    Result tryDo: [self bump]
    Result tryDo: [self.n]
    [self bump] value
",
    );
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn sealed_stateless_driver_never_warns() {
    // `class sealed` method of a sealed, stateless class: direct-called, so the
    // block runs at home.
    let d = only("  class a => SealedDriver each: [self bump]\n");
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn reading_block_carried_synchronously_to_another_class_never_warns() {
    // The home invocation is blocked in the send: the captured value is live.
    let d = only("  class a =>\n    self bump\n    Driver each: [self.n]\n");
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn block_that_does_not_touch_class_state_never_warns() {
    let d = only(
        "  class a: w =>
    Driver each: [1 + 2]
    Driver each: [Counter pure]
    w keep: [3]
    b := [4]
    b value
    [5]
",
    );
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn instance_side_blocks_and_classes_without_class_state_never_warn() {
    let d = abroad(
        "Object subclass: Plain
  value => [self foo]
  class make => [1]
  class send: w => Driver each: [2]

Object subclass: Driver
  classState: ticks = 0
  class each: aBlock => aBlock value
",
    );
    assert!(d.is_empty(), "{d:?}");
}

// ---- suppression ------------------------------------------------------------

#[test]
fn expect_class_state_abroad_suppresses_the_warning() {
    let src = format!("{HEADER}  class reader =>\n    @expect class_state_abroad\n    [self.n]\n");
    let module = parse_module(&src);
    let mut diags = analyse(&module).diagnostics;
    apply_expect_directives(&module, &mut diags);
    assert!(
        !diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::ClassStateAbroad)),
        "{diags:?}"
    );
    assert!(
        !diags.iter().any(|d| d.message.contains("stale @expect")),
        "the directive matched: {diags:?}"
    );
}

#[test]
fn each_block_is_reported_once() {
    // Two distinct blocks, one passed to an actor and one returned.
    let d = only("  class a =>\n    w := Worker spawn\n    w keep: [self.n]\n    [self.n]\n");
    assert_eq!(d.len(), 2, "{d:?}");
}

// ---- cascades (BT-3716) -----------------------------------------------------
//
// The AST walker folds a cascade to its first send, so the later messages are
// judged against the shared receiver explicitly.

#[test]
fn cascaded_later_message_passing_a_writing_block_to_another_class_warns() {
    let d = only("  class a => Driver each: [3]; each: [self bump]\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(d[0].message.contains("Driver each:"), "{}", d[0].message);
}

#[test]
fn cascaded_later_message_passing_a_reading_block_to_an_actor_warns() {
    let d = only("  class a =>\n    w := Worker spawn\n    w keep: [1]; keep: [self.n]\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(
        d[0].message
            .contains("handed to an actor by 'keep:', which may keep it"),
        "{}",
        d[0].message
    );
}

#[test]
fn pure_first_cascade_message_does_not_hide_a_write_in_a_sealed_class() {
    let d = abroad(&format!(
        "{HEADER}sealed Object subclass: Sealed
  classState: m = 0

  class log => 7

  class bump => self.m := self.m + 1

  class a => Driver each: [self log; bump]
"
    ));
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(d[0].message.contains("'bump'"), "{}", d[0].message);
}

#[test]
fn pure_first_cascade_message_does_not_hide_a_write_to_the_own_class_name() {
    let d = only("  class a => Driver each: [Counter pure; bump]\n");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn own_class_cascade_inside_a_method_body_makes_the_method_mutating() {
    // `viaCascade` writes only through the cascaded `bump`; a sealed class
    // proves `self viaCascade` pure unless that later message is seen.
    let d = abroad(&format!(
        "{HEADER}sealed Object subclass: Sealed
  classState: m = 0

  class log => 7

  class bump => self.m := self.m + 1

  class viaCascade => Sealed log; bump

  class a => Driver each: [self viaCascade]
"
    ));
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn cascades_that_stay_at_home_never_warn() {
    let d = abroad(&format!(
        "{HEADER}sealed Object subclass: Sealed
  classState: m = 0

  class log => 7

  class viaCascade => Sealed log; log

  class a =>
    Driver each: [self log; log]
    Driver each: [self viaCascade; log]
    Sealed log; run: [self.m]
"
    ));
    assert!(d.is_empty(), "{d:?}");
}
