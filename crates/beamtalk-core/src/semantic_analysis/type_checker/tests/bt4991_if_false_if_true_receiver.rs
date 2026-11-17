// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4991: `c ifFalse: [a] ifTrue: [b]` is normalised to `ifTrue:ifFalse:`
//! for the whole pipeline, but a receiver that is not a Boolean is sent
//! `ifFalse:ifTrue:` as written. The checker must resolve such a send under
//! the selector the user wrote, so a class that defines only `ifFalse:ifTrue:`
//! gets no false does-not-understand and its return type comes from the right
//! method.

use super::common::*;

/// A non-Boolean class with `ifFalse:ifTrue:` only, answering a `String`.
const ONLY_IF_FALSE_IF_TRUE: &str = "Object subclass: OnlyIfFalse
  ifFalse: a ifTrue: b -> String => \"written\"
";

/// A non-Boolean class with both selectors, answering different types.
const BOTH_SELECTORS: &str = "Object subclass: Both
  ifFalse: a ifTrue: b -> String => \"written\"
  ifTrue: a ifFalse: b -> Integer => 0
";

fn probe(classes: &str, methods: &str) -> Vec<String> {
    messages(&format!("{classes}\nObject subclass: Probe\n{methods}\n"))
}

#[test]
fn plain_non_boolean_receiver_has_no_false_dnu() {
    let msgs = probe(
        ONLY_IF_FALSE_IF_TRUE,
        "  run: c :: OnlyIfFalse => c ifFalse: [1] ifTrue: [2]",
    );
    assert!(msgs.is_empty(), "no diagnostics expected, got: {msgs:#?}");
}

#[test]
fn union_with_boolean_has_no_false_warning() {
    let msgs = probe(
        ONLY_IF_FALSE_IF_TRUE,
        "  run: c :: Boolean | OnlyIfFalse => c ifFalse: [1] ifTrue: [2]",
    );
    assert!(msgs.is_empty(), "no diagnostics expected, got: {msgs:#?}");
}

#[test]
fn return_type_comes_from_the_written_selector() {
    // `Both>>ifFalse:ifTrue:` answers a String; `Both>>ifTrue:ifFalse:`
    // answers an Integer. The send as written must type as String.
    let msgs = probe(
        BOTH_SELECTORS,
        "  run: c :: Both -> String => c ifFalse: [1] ifTrue: [2]",
    );
    assert!(msgs.is_empty(), "no diagnostics expected, got: {msgs:#?}");
}

#[test]
fn boolean_receiver_still_checks_clean() {
    let msgs = probe(
        "",
        "  run: c :: Boolean -> Integer => c ifFalse: [1] ifTrue: [2]",
    );
    assert!(msgs.is_empty(), "no diagnostics expected, got: {msgs:#?}");
}

#[test]
fn a_class_with_neither_selector_reports_the_selector_as_written() {
    let msgs = probe(
        "Object subclass: Neither\n  noop => 1\n",
        "  run: c :: Neither => c ifFalse: [1] ifTrue: [2]",
    );
    assert_eq!(
        msgs,
        vec!["Neither does not understand 'ifFalse:ifTrue:'".to_string()],
        "the DNU must name the selector as written"
    );
}
