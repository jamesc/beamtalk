// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5026: state / class-variable default expressions and protocol-only
//! provided-method bodies must be seen by name resolution and by the
//! `walk_module`-based validators; `-> UndefinedObject` is `-> Nil`.

use super::*;

fn messages(src: &str) -> Vec<String> {
    analyse(&parse_bt(src))
        .diagnostics
        .into_iter()
        .map(|d| d.message.to_string())
        .collect()
}

fn count(src: &str, needle: &str) -> usize {
    messages(src).iter().filter(|m| m.contains(needle)).count()
}

#[test]
fn undefined_name_in_state_default() {
    let src = "Object subclass: Foo\n  state: x = nosuchVar\n  get => 1\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn undefined_name_in_class_variable_default() {
    let src = "Object subclass: Foo\n  classState: x = nosuchVar\n  get => 1\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn state_default_does_not_see_sibling_state_or_defined_names() {
    // A well-formed default must stay clean.
    let src = "Object subclass: Foo\n  state: x = 1\n  state: y = #(1, 2)\n  get => 1\n";
    assert_eq!(count(src, "Undefined variable"), 0, "{:?}", messages(src));
}

#[test]
fn abstract_instantiation_in_state_default() {
    let src = "abstract Object subclass: Thing\n  foo => 1\n\
               Object subclass: Holder\n  state: w = Thing new\n  get => 1\n";
    assert_eq!(
        count(src, "Cannot instantiate abstract class"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn abstract_instantiation_in_class_variable_default() {
    let src = "abstract Object subclass: Thing\n  foo => 1\n\
               Object subclass: Holder\n  classState: w = Thing new\n  get => 1\n";
    assert_eq!(
        count(src, "Cannot instantiate abstract class"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn undefined_name_in_protocol_only_provided_method() {
    let src = "Protocol define: Greeter\n  name -> String\n\n  greet -> String => nosuchVar\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn abstract_instantiation_in_protocol_only_provided_method() {
    let src = "abstract Object subclass: Thing\n  foo => 1\n\
               Protocol define: Greeter\n  name -> String\n\n  greet -> Object => Thing new\n";
    assert_eq!(
        count(src, "Cannot instantiate abstract class"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn protocol_provided_method_with_user_is_reported_once() {
    let src = "Protocol define: Greeter\n  name -> String\n\n  greet -> String => nosuchVar\n\
               Object subclass: User\n  uses: Greeter\n  name -> String => \"a\"\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn value_type_returning_undefined_object_is_rejected_like_nil() {
    let nil = "Value subclass: V\n  field: x = 1\n  poke -> Nil => nil\n";
    let undef = "Value subclass: V\n  field: x = 1\n  poke -> UndefinedObject => nil\n";
    assert_eq!(
        count(nil, "returns Nil on a Value type"),
        1,
        "{:?}",
        messages(nil)
    );
    assert_eq!(
        count(undef, "returns Nil on a Value type"),
        1,
        "{:?}",
        messages(undef)
    );
}

#[test]
fn class_side_return_override_widening_is_reported() {
    let src = "Object subclass: Base\n  class make -> Integer => 1\n\
               Base subclass: Child\n  class make -> String => \"a\"\n";
    assert_eq!(
        count(src, "incompatible with parent's"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn class_side_return_override_narrowing_is_clean() {
    let src = "Object subclass: Base\n  class make -> Number => 1\n\
               Base subclass: Child\n  class make -> Integer => 1\n";
    assert_eq!(
        count(src, "incompatible with parent's"),
        0,
        "{:?}",
        messages(src)
    );
}

#[test]
fn provided_method_dropped_by_excluding_is_still_analysed() {
    let src = "Protocol define: Greeter\n  name -> String\n\n  greet -> String => nosuchVar\n\
               Object subclass: User\n  uses: Greeter excluding: #(#greet)\n  name -> String => \"a\"\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

#[test]
fn provided_method_overridden_by_user_is_still_analysed() {
    let src = "Protocol define: Greeter\n  name -> String\n\n  greet -> String => nosuchVar\n\
               Object subclass: User\n  uses: Greeter\n  name -> String => \"a\"\n  greet -> String => \"hi\"\n";
    assert_eq!(
        count(src, "Undefined variable: nosuchVar"),
        1,
        "{:?}",
        messages(src)
    );
}

// BT-5084: the new behaviour BT-5026 introduced is that a state default
// expression is analysed, so a default that names something only an instance
// can see is an error rather than being silently skipped.

#[test]
fn state_default_referring_to_sibling_field_is_an_error() {
    let src = "Object subclass: Foo\n  state: x = 1\n  state: y = x\n  get => 1\n";
    assert_eq!(
        count(src, "Undefined variable: x"),
        1,
        "a default cannot read a sibling field: {:?}",
        messages(src)
    );
}

#[test]
fn state_default_referring_to_self_is_an_error() {
    let src = "Object subclass: Foo\n  state: x = self\n  get => 1\n";
    let msgs = messages(src);
    assert!(
        !msgs.is_empty(),
        "a default has no receiver, so `self` must be diagnosed: {msgs:?}"
    );
}
