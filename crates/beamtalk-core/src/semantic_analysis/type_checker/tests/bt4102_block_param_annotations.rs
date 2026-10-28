// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4102: block parameter annotations are honored, and block literals in
//! return position / annotated locals are typed from the declared
//! `Block(P…, R)` type.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn dynamic_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("Dynamic"))
        .collect()
}

const PT: &str = r"
typed Object subclass: Pt
  x -> Integer => 1
";

fn with_pt(body: &str) -> String {
    format!("{PT}\n{body}")
}

fn mentions(diags: &[Diagnostic], needle: &str) -> bool {
    diags.iter().any(|d| d.message.contains(needle))
}

#[test]
fn annotation_honored_in_dict_literal() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class opts -> Dictionary => #{#handler => [:p :: Pt | p x]}
",
    ));
    assert!(dynamic_warnings(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn annotation_types_param_so_dnu_fires() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class opts -> Dictionary => #{#handler => [:p :: Pt | p nonexistent]}
",
    ));
    assert!(mentions(&diags, "nonexistent"), "got: {diags:#?}");
}

#[test]
fn return_position_tail_block_typed_from_declared_return() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Pt, Integer) => [:p | p x]
",
    ));
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn return_position_tail_block_param_is_really_typed() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Pt, Integer) => [:p | p nonexistent. 1]
",
    ));
    assert!(mentions(&diags, "nonexistent"), "got: {diags:#?}");
}

#[test]
fn return_position_mid_body_caret_block_typed() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Pt, Integer) =>
    ^[:p | p nonexistent. 1]
    2
",
    ));
    assert!(mentions(&diags, "nonexistent"), "got: {diags:#?}");
}

#[test]
fn annotated_local_block_typed() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class run -> Integer =>
    f :: Block(Pt, Integer) := [:p | p nonexistent. 1]
    1
",
    ));
    assert!(mentions(&diags, "nonexistent"), "got: {diags:#?}");
}

#[test]
fn annotation_beats_pushdown() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Object, Integer) => [:p :: Pt | p x]
",
    ));
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn annotation_vs_pushdown_mismatch_warns() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Pt, Integer) => [:p :: String | 1]
",
    ));
    assert!(mentions(&diags, "Type mismatch"), "got: {diags:#?}");
}

#[test]
fn responds_to_branch_with_nil_arm_does_not_collapse_to_nil() {
    let diags = type_diagnostics(
        r"
typed Object subclass: S
  class f: aScope :: Behaviour sel: sel :: Symbol -> Integer =>
    m := (aScope respondsTo: #'>>') ifTrue: [aScope >> sel] ifFalse: [nil]
    m isNil
      ifTrue: [1]
      ifFalse: [m source size]
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn body_return_mismatch_against_declared_block_warns() {
    let diags = type_diagnostics(&with_pt(
        r"
typed Object subclass: H
  class handler -> Block(Pt, String) => [:p | p x]
",
    ));
    assert!(!diags.is_empty(), "expected a mismatch, got none");
}

fn no_dnu_on_nil(src: &str) {
    let diags = type_diagnostics(src);
    assert!(
        !diags.iter().any(|d| d.message.contains("UndefinedObject")),
        "got: {diags:#?}"
    );
}

#[test]
fn dynamic_true_arm_nil_false_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  untypedThing => 1
  run: flag :: Boolean -> Integer =>
    x := flag ifTrue: [self untypedThing] ifFalse: [nil]
    x bar
    1
",
    );
}

#[test]
fn nil_true_arm_dynamic_false_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  untypedThing => 1
  run: flag :: Boolean -> Integer =>
    x := flag ifTrue: [nil] ifFalse: [self untypedThing]
    x bar
    1
",
    );
}

// BT-4207: a genuinely Dynamic arm (`d foo`, `d` unannotated) joined with a
// `nil` arm must stay Dynamic (`join_branch_arms`), not collapse to `Nil`.
#[test]
fn dynamic_param_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  run: flag :: Boolean with: d -> Integer =>
    x := flag ifTrue: [d foo] ifFalse: [nil]
    x bar
    1
",
    );
}

#[test]
fn dynamic_param_arm_swapped_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  run: flag :: Boolean with: d -> Integer =>
    x := flag ifTrue: [nil] ifFalse: [d foo]
    x bar
    1
",
    );
}

#[test]
fn responds_to_dynamic_arm_nil_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  run: o :: Object -> Integer =>
    x := (o respondsTo: #foo) ifTrue: [o foo] ifFalse: [nil]
    x bar
    1
",
    );
}

#[test]
fn opaque_block_variable_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: M
  run: flag :: Boolean with: blk :: Block -> Integer =>
    x := flag ifTrue: blk ifFalse: [nil]
    x bar
    1
",
    );
}

/// Phase 1 of block push-down stands literal blocks in with a placeholder
/// that must not count as an opaque (unknown) branch arm (BT-4207 review):
/// `R`'s only Known binding comes from the typed block variable `first`;
/// the literal `[2]` in the other `Block(R)` slot must not turn that into
/// `DynamicBranch`, so the last literal block's `:v` is still pushed down as
/// `Integer` and an unknown send on it reports against `Integer`.
#[test]
fn literal_block_placeholder_is_not_an_unknown_arm() {
    let diags = type_diagnostics(
        r"
typed Object subclass: M
  pick: a :: Block(R) or: b :: Block(R) then: c :: Block(R, Nil) -> Nil => nil
  run: first :: Block(Integer) -> Nil =>
    self pick: first or: [2] then: [:v | v fooBarBaz. nil]
",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("Integer") && d.message.contains("fooBarBaz")),
        "`:v` should be pushed down as Integer: {diags:#?}"
    );
}

/// A union-typed block argument contributes its members' results as one
/// branch arm (BT-4240): `Block(Integer) | Block(Nil)` joins to
/// `Integer | Nil`, so a sibling `nil` arm cannot claim `R` alone.
#[test]
fn union_of_blocks_arm_does_not_collapse_to_nil() {
    let diags = type_diagnostics(
        r"
typed Object subclass: M
  run: flag :: Boolean with: blk :: Block(Integer) | Block(Nil) -> Integer =>
    x := flag ifTrue: blk ifFalse: [nil]
    x bar
    1
",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("Integer") && d.message.contains("bar")),
        "`x` is Integer | Nil, so `bar` reports against Integer: {diags:#?}"
    );
}

/// A union of an opaque `Block` and a non-block member is an unknown arm
/// (BT-4240): the join is Dynamic, not `Nil`.
#[test]
fn union_of_block_and_non_block_arm_does_not_collapse_to_nil() {
    no_dnu_on_nil(
        r"
typed Object subclass: Handler
  value -> Integer => 1

typed Object subclass: M
  run: flag :: Boolean with: blk :: Block | Handler -> Integer =>
    x := flag ifTrue: blk ifFalse: [nil]
    x bar
    1
",
    );
}
