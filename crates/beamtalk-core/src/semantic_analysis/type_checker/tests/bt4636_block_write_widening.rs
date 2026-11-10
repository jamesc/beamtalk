// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4636: a local written inside a block passed to an iteration or
//! control-flow message is widened to the join of its prior type and the type
//! the block leaves it with, as `ifTrue:`/`ifFalse:` arms already are (BT-4207).

use super::common::*;

fn diagnostics_for(body: &str) -> Vec<Diagnostic> {
    let source = format!("Object subclass: Probe\n{body}");
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn dnu(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect()
}

#[test]
fn nil_init_written_in_do_is_widened_and_guard_narrows() {
    let diags = diagnostics_for(
        "  class nilLocal: items :: List(String) -> Integer =>\n    found := nil\n    items do: [:each | each isEmpty ifFalse: [found := each]]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn nil_init_written_directly_in_do_block() {
    let diags = diagnostics_for(
        "  class f: items :: List(String) -> Integer =>\n    found := nil\n    items do: [:each | found := each]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn nil_init_written_in_while_true() {
    let diags = diagnostics_for(
        "  class f: s :: String -> Integer =>\n    found := nil\n    i := 0\n    [i < 3] whileTrue: [found := s. i := i + 1]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn nil_init_written_in_to_do_and_times_repeat() {
    let diags = diagnostics_for(
        "  class f: s :: String -> Integer =>\n    found := nil\n    1 to: 3 do: [:i | found := s]\n    found isNil ifTrue: [^0]\n    found size\n\n  class g: s :: String -> Integer =>\n    found := nil\n    3 timesRepeat: [found := s]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn nil_init_written_in_inject_into_and_keys_and_values_do() {
    let diags = diagnostics_for(
        "  class f: items :: List(String) -> Integer =>\n    found := nil\n    items inject: 0 into: [:acc :each | found := each. acc + 1]\n    found isNil ifTrue: [^0]\n    found size\n\n  class g: d :: Dictionary(String, String) -> Integer =>\n    found := nil\n    d keysAndValuesDo: [:k :v | found := v]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn non_nil_initial_type_is_widened_to_a_union() {
    // `found` starts as a String and the block may set it to nil: after the
    // loop it is `String | Nil`, so the guard narrows it back to String.
    let diags = diagnostics_for(
        "  class f: items :: List(String) -> Integer =>\n    found := 'start'\n    items do: [:each | each isEmpty ifTrue: [found := nil]]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn find_first_idiom_with_nil_guard_arm_is_widened() {
    // The write sits in a narrowing arm (`found isNil ifTrue: [...]`), which
    // is inferred on its own path from plain blocks.
    let diags = diagnostics_for(
        "  class f: items :: List(String) -> Integer =>\n    found := nil\n    items do: [:e | found isNil ifTrue: [found := e]]\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn block_stored_in_a_local_is_widened_where_it_is_defined() {
    // A block literal on an assignment RHS goes through the same inference as
    // a message argument, so its write is joined in at the definition: the
    // block can run at any later point. This is approximate, not a soundness
    // guarantee: a reassignment between the definition and `setter value` is
    // not tracked.
    let diags = diagnostics_for(
        "  class f: s :: String -> Integer =>\n    found := nil\n    setter := [found := s]\n    setter value\n    found isNil ifTrue: [^0]\n    found size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn sibling_arms_do_not_see_each_others_writes() {
    // The arms are mutually exclusive: the false arm must still see `x` as an
    // Integer, not `Integer | String` from the true arm's write (`abs` would be
    // reported against the String member).
    let diags = diagnostics_for(
        "  class g: flag :: Boolean -> Integer =>\n    x := 0\n    flag ifTrue: [x := 'a'] ifFalse: [x abs]\n    0\n\n  class f: n :: Integer | Nil -> Integer =>\n    x := 0\n    n isNil ifTrue: [x := 'a'] ifFalse: [x abs]\n    0\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn both_arms_writes_are_joined_after_the_send() {
    let diags = diagnostics_for(
        "  class flag: flag :: Boolean with: s :: String -> Integer =>\n    result := ''\n    flag ifTrue: [result := nil] ifFalse: [result := s]\n    result isNil ifTrue: [^0]\n    result size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn control_sibling_arm_shape_is_checked() {
    // Pins the deferral: the false arm must still see `x` as the Integer it
    // was before the send, so `size` is rejected. If the true arm's write
    // leaked into the sibling arm, `x` would be a union there and the DNU
    // would be lost. Also proves the shapes above are parsed and checked.
    let diags = diagnostics_for(
        "  class g: flag :: Boolean -> Integer =>\n    x := 0\n    flag ifTrue: [x := 'a'] ifFalse: [x size]\n    0\n\n  class f: n :: Integer | Nil -> Integer =>\n    x := 0\n    n isNil ifTrue: [x := 'a'] ifFalse: [x size]\n    0\n",
    );
    assert_eq!(dnu(&diags).len(), 2, "expected two DNUs: {diags:#?}");
}

#[test]
fn control_two_keyword_class_method_shape_is_checked() {
    let diags = diagnostics_for(
        "  class flag: flag :: Boolean with: s :: String -> Integer =>\n    s fooBar\n    0\n",
    );
    assert_eq!(dnu(&diags).len(), 1, "expected one DNU: {diags:#?}");
}

#[test]
fn diverging_block_write_is_not_widened() {
    // The `^` means the write never reaches the code after the send, so
    // `result` stays a String and `size` is fine. Typed values, not literals:
    // a literal write is not reported against the union, so it could not tell
    // the guard apart from its absence.
    let diags = diagnostics_for(
        "  class f: n :: Integer with: s :: String -> Integer =>\n    result := s\n    n > 1 ifTrue: [result := n. ^1]\n    result size\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}

#[test]
fn non_diverging_block_write_is_widened() {
    // Control for the test above: the same shape without the `^` is widened
    // and `size` is rejected on the Integer member.
    let diags = diagnostics_for(
        "  class f: n :: Integer with: s :: String -> Integer =>\n    result := s\n    n > 1 ifTrue: [result := n]\n    result size\n",
    );
    assert_eq!(dnu(&diags).len(), 1, "expected one DNU: {diags:#?}");
}

#[test]
fn widened_local_is_a_union_not_dynamic() {
    // A Dynamic result would never report a DNU, so the other tests alone
    // could not tell a union from a lost type. `found` is `Integer | String`
    // after the loop: the Integer member must be rejected by `size`.
    let diags = diagnostics_for(
        "  class f: items :: List(String) -> Integer =>\n    found := 0\n    items do: [:each | found := each]\n    found size\n",
    );
    assert_eq!(dnu(&diags).len(), 1, "expected one DNU: {diags:#?}");
}
