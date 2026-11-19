// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5025: a cascade's first message is folded into `Cascade::receiver` as a
//! whole `MessageSend`, so the validators' cascade arms must judge each later
//! message against the *shared* receiver (via `later_cascade_sends`). Each
//! test sends the offending message as a later cascade message (`Foo describe;
//! new`) and expects exactly one diagnostic, the same one the plain send draws.

use super::*;

/// Diagnostics from `analyse(src)` whose message contains `needle`.
fn matching(src: &str, needle: &str) -> Vec<String> {
    analyse(&parse_bt(src))
        .diagnostics
        .into_iter()
        .filter(|d| d.message.contains(needle))
        .map(|d| d.message.to_string())
        .collect()
}

fn assert_plain_and_cascade(prefix: &str, plain: &str, cascade: &str, needle: &str) {
    let plain_hits = matching(&format!("{prefix}\n{plain}"), needle);
    let cascade_hits = matching(&format!("{prefix}\n{cascade}"), needle);
    assert_eq!(plain_hits.len(), 1, "plain send: {plain_hits:?}");
    assert_eq!(cascade_hits.len(), 1, "cascade: {cascade_hits:?}");
}

#[test]
fn abstract_instantiation_in_later_cascade_message() {
    assert_plain_and_cascade(
        "abstract Object subclass: Abstract\n  foo => 1",
        "Abstract new",
        "Abstract foo; new",
        "Cannot instantiate abstract class",
    );
}

#[test]
fn actor_new_in_later_cascade_message() {
    assert_plain_and_cascade(
        "Actor subclass: Worker\n  state: n = 0\n  describe => 1",
        "Worker new",
        "Worker describe; new",
        "spawn",
    );
}

#[test]
fn self_new_in_object_class_method_later_cascade_message() {
    assert_plain_and_cascade(
        "",
        "Object subclass: Ns\n  class make => self new",
        "Object subclass: Ns\n  class make => self foo; new",
        "new",
    );
}

#[test]
fn uninstantiable_new_in_later_cascade_message() {
    assert_plain_and_cascade(
        "Object subclass: Ns\n  class describe => 1",
        "Ns new",
        "Ns describe; new",
        "cannot be instantiated",
    );
}

#[test]
fn new_field_names_in_later_cascade_message() {
    assert_plain_and_cascade(
        "Value subclass: Point\n  state: x = 0\n  describe => 1",
        "Point new: #{#bogus => 1}",
        "Point describe; new: #{#bogus => 1}",
        "Unknown field `bogus`",
    );
}

#[test]
fn class_state_access_in_later_cascade_message() {
    assert_plain_and_cascade(
        "Object subclass: Counter\n  classState: n = 0\n  class describe => 1",
        "Counter classState: #missing",
        "Counter describe; classState: #missing",
        "Undefined class variable",
    );
}

#[test]
fn literal_boolean_condition_in_later_cascade_message() {
    let hits = matching("true ifTrue: [1]; ifFalse: [2]", "Condition is always");
    assert_eq!(hits.len(), 2, "{hits:?}");
}
