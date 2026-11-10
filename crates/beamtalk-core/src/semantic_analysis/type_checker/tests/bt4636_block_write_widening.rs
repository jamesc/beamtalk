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
fn escaping_block_is_not_widened_at_the_definition() {
    // The block is stored, not passed to a message, so there is no point at
    // which its write is known to have happened: the local keeps its type.
    let diags = diagnostics_for(
        "  class f: s :: String -> Integer =>\n    found := nil\n    setter := [found := s]\n    setter value\n    found isNil ifTrue: [^0]\n    0\n",
    );
    assert!(dnu(&diags).is_empty(), "unexpected DNU: {diags:#?}");
}
