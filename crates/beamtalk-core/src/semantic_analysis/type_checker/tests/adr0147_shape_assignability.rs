// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4730): shape assignability — shape-to-shape,
//! shape-to-`Dictionary`, and the gradual `Dictionary`-to-shape rule — with
//! diagnostics that name the offending key.

use super::common::*;
use crate::source_analysis::Severity;

struct Diag {
    message: String,
    hint: String,
    severity: Severity,
}

fn diagnostics(source: &str) -> Vec<Diag> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| matches!(d.category, Some(DiagnosticCategory::Type)))
        .map(|d| Diag {
            message: d.message.to_string(),
            hint: d.hint.map(|h| h.to_string()).unwrap_or_default(),
            severity: d.severity,
        })
        .collect()
}

/// `give:` passes a value declared `actual` to `take:`, declared `expected`.
fn pass(actual: &str, expected: &str) -> Vec<Diag> {
    diagnostics(&format!(
        "Object subclass: Probe\n  take: o :: {expected} => o\n  give: s :: {actual} => self take: s\n"
    ))
}

/// (name, actual, expected, `None` = assignable / `Some(fragment)` = warning
/// whose message or hint contains `fragment`).
type Case = (
    &'static str,
    &'static str,
    &'static str,
    Option<&'static str>,
);

fn run(cases: &[Case]) {
    for (name, actual, expected, outcome) in cases {
        let diags = pass(actual, expected);
        match outcome {
            None => assert!(
                diags.is_empty(),
                "{name}: {actual} -> {expected} must be assignable, got: {:#?}",
                diags
                    .iter()
                    .map(|d| (&d.message, &d.hint))
                    .collect::<Vec<_>>()
            ),
            Some(fragment) => {
                assert_eq!(
                    diags.len(),
                    1,
                    "{name}: {actual} -> {expected} must warn once, got: {:#?}",
                    diags
                        .iter()
                        .map(|d| (&d.message, &d.hint))
                        .collect::<Vec<_>>()
                );
                let d = &diags[0];
                assert_eq!(d.severity, Severity::Warning, "{name}");
                assert!(
                    d.message.contains(fragment) || d.hint.contains(fragment),
                    "{name}: `{fragment}` missing from message {:?} / hint {:?}",
                    d.message,
                    d.hint
                );
            }
        }
    }
}

#[test]
fn shape_to_shape_required_and_optional_keys() {
    run(&[
        ("identical", "#{#a => Integer}", "#{#a => Integer}", None),
        (
            "key order is irrelevant",
            "#{#a => Integer, #b => String}",
            "#{#b => String, #a => Integer}",
            None,
        ),
        (
            "missing required key",
            "#{#a => Integer}",
            "#{#a => Integer, #b => String}",
            Some("missing required key `#b`"),
        ),
        (
            "wrong value type",
            "#{#a => String}",
            "#{#a => Integer}",
            Some("key `#a` expects Integer, got String"),
        ),
        (
            "covariant value",
            "#{#a => Integer}",
            "#{#a => Number}",
            None,
        ),
        (
            "contravariant value rejected",
            "#{#a => Number}",
            "#{#a => Integer}",
            Some("key `#a`"),
        ),
        (
            "optional key present as required",
            "#{#a => Integer}",
            "#{?#a => Integer}",
            None,
        ),
        (
            "optional key present as optional",
            "#{?#a => Integer}",
            "#{?#a => Integer}",
            None,
        ),
        (
            "optional where required",
            "#{?#a => Integer}",
            "#{#a => Integer}",
            Some("missing required key `#a`"),
        ),
        (
            "optional key absent in closed source",
            "#{#b => Integer}",
            "#{#b => Integer, ?#a => String}",
            None,
        ),
        (
            "optional key wrong type",
            "#{?#a => String}",
            "#{?#a => Integer}",
            Some("key `#a`"),
        ),
        ("empty closed to empty closed", "#{}", "#{}", None),
        (
            "empty closed to optional only",
            "#{}",
            "#{?#a => Integer}",
            None,
        ),
    ]);
}

#[test]
fn shape_to_shape_open_versus_closed() {
    run(&[
        (
            "extra key into closed",
            "#{#a => Integer, #b => String}",
            "#{#a => Integer}",
            Some("key `#b` is not named"),
        ),
        (
            "extra key into open",
            "#{#a => Integer, #b => String}",
            "#{#a => Integer, ...}",
            None,
        ),
        (
            "open into closed",
            "#{#a => Integer, ...}",
            "#{#a => Integer}",
            Some("open shape"),
        ),
        (
            "open into open",
            "#{#a => Integer, ...}",
            "#{#a => Integer, ...}",
            None,
        ),
        (
            "closed into open",
            "#{#a => Integer}",
            "#{#a => Integer, ...}",
            None,
        ),
        (
            "open source may hold an optional key of the target",
            "#{#b => Integer, ...}",
            "#{#b => Integer, ?#a => String, ...}",
            Some("may hold `#a`"),
        ),
        (
            "open into empty open",
            "#{#a => Integer, ...}",
            "#{...}",
            None,
        ),
        ("closed into empty open", "#{#a => Integer}", "#{...}", None),
    ]);
}

#[test]
fn unexpected_key_suggests_the_nearest_key() {
    run(&[(
        "typo",
        "#{#timout => Integer}",
        "#{?#timeout => Integer, ?#retries => Integer}",
        Some("did you mean `#timeout`"),
    )]);
}

#[test]
fn nested_shapes() {
    run(&[
        (
            "nested ok",
            "#{#in => #{#x => Integer}}",
            "#{#in => #{#x => Integer}}",
            None,
        ),
        (
            "nested covariant",
            "#{#in => #{#x => Integer, #y => Integer}}",
            "#{#in => #{#x => Integer, ...}}",
            None,
        ),
        (
            "nested missing key is named with its path",
            "#{#in => #{#x => Integer}}",
            "#{#in => #{#x => Integer, #y => Integer}}",
            Some("in key `#in`: missing required key `#y`"),
        ),
        (
            "nested wrong type",
            "#{#in => #{#x => String}}",
            "#{#in => #{#x => Integer}}",
            Some("key `#x` expects Integer, got String"),
        ),
        (
            "shape value to Dictionary value",
            "#{#in => #{#x => Integer}}",
            "#{#in => Dictionary}",
            None,
        ),
        (
            "shape value to Object value",
            "#{#in => #{#x => Integer}}",
            "#{#in => Object}",
            None,
        ),
    ]);
}

#[test]
fn shape_to_dictionary() {
    run(&[
        (
            "bare Dictionary accepts any shape",
            "#{#a => Integer}",
            "Dictionary",
            None,
        ),
        (
            "bare Dictionary accepts open shape",
            "#{#a => Integer, ...}",
            "Dictionary",
            None,
        ),
        (
            "bare Dictionary accepts empty shape",
            "#{}",
            "Dictionary",
            None,
        ),
        ("Object accepts a shape", "#{#a => Integer}", "Object", None),
        (
            "closed: key and value types fit",
            "#{#a => Integer, #b => Integer}",
            "Dictionary(Symbol, Integer)",
            None,
        ),
        (
            "closed: value types fit a wider value",
            "#{#a => Integer}",
            "Dictionary(Symbol, Number)",
            None,
        ),
        (
            "closed: value type does not fit",
            "#{#a => String}",
            "Dictionary(Symbol, Integer)",
            Some("Dictionary(Symbol, Integer)"),
        ),
        (
            "closed: mixed values do not fit",
            "#{#a => Integer, #b => String}",
            "Dictionary(Symbol, Integer)",
            Some("Dictionary(Symbol, Integer)"),
        ),
        (
            "closed: key type does not fit",
            "#{#a => Integer}",
            "Dictionary(String, Integer)",
            Some("Dictionary(String, Integer)"),
        ),
        (
            "closed: Dynamic arguments",
            "#{#a => Integer}",
            "Dictionary(Dynamic, Dynamic)",
            None,
        ),
        (
            "open: Object/Object",
            "#{#a => Integer, ...}",
            "Dictionary(Object, Object)",
            None,
        ),
        (
            "open: Dynamic/Dynamic",
            "#{#a => Integer, ...}",
            "Dictionary(Dynamic, Dynamic)",
            None,
        ),
        (
            "open: Symbol keys are not enough",
            "#{#a => Integer, ...}",
            "Dictionary(Symbol, Object)",
            Some("Dictionary(Symbol, Object)"),
        ),
        (
            "open: Integer values are not enough",
            "#{#a => Integer, ...}",
            "Dictionary(Object, Integer)",
            Some("Dictionary(Object, Integer)"),
        ),
        (
            "shape to unrelated class",
            "#{#a => Integer}",
            "String",
            Some("String"),
        ),
    ]);
}

#[test]
fn dictionary_to_shape_is_gradual() {
    run(&[
        ("bare Dictionary", "Dictionary", "#{#a => Integer}", None),
        (
            "bare Dictionary to open shape",
            "Dictionary",
            "#{#a => Integer, ...}",
            None,
        ),
        ("bare Dictionary to empty shape", "Dictionary", "#{}", None),
        (
            "Dynamic value argument",
            "Dictionary(Symbol, Dynamic)",
            "#{#a => Integer}",
            None,
        ),
        (
            "Dynamic key argument",
            "Dictionary(Dynamic, Integer)",
            "#{#a => Integer}",
            None,
        ),
        (
            "typed Dictionary is not assignable",
            "Dictionary(Symbol, Object)",
            "#{#a => Integer}",
            Some("Dictionary(Symbol, Object)"),
        ),
        (
            "typed Dictionary with matching value",
            "Dictionary(Symbol, Integer)",
            "#{#a => Integer}",
            Some("Dictionary(Symbol, Integer)"),
        ),
        (
            "non-Dictionary",
            "Integer",
            "#{#a => Integer}",
            Some("Integer"),
        ),
        ("nil", "Nil", "#{#a => Integer}", Some("Nil")),
    ]);
}

#[test]
fn shapes_inside_unions() {
    run(&[
        (
            "shape to union containing it",
            "#{#a => Integer}",
            "#{#a => Integer} | Nil",
            None,
        ),
        (
            "shape to union of other shapes",
            "#{#a => Integer}",
            "#{#b => String} | #{#a => Integer}",
            None,
        ),
        (
            "shape to union of Dictionary and nil",
            "#{#a => Integer}",
            "Dictionary | Nil",
            None,
        ),
        (
            "shape matching no member",
            "#{#c => Integer}",
            "#{#a => Integer} | #{#b => String}",
            Some("#{#c => Integer}"),
        ),
        (
            "single shape member explains itself",
            "#{#a => String}",
            "#{#a => Integer} | Nil",
            Some("key `#a` expects Integer"),
        ),
        (
            "union of shapes to a shape",
            "#{#a => Integer} | #{#a => Integer, #b => String}",
            "#{#a => Integer, ...}",
            None,
        ),
        (
            "one union member fails",
            "#{#a => Integer} | #{#b => String}",
            "#{#a => Integer, ...}",
            Some("missing required key `#a`"),
        ),
        (
            "nilable shape to shape",
            "#{#a => Integer} | Nil",
            "#{#a => Integer}",
            Some("Nil"),
        ),
        (
            "bare Dictionary in a union",
            "Dictionary | Nil",
            "#{#a => Integer} | Nil",
            None,
        ),
    ]);
}

#[test]
fn shapes_inside_generics() {
    run(&[
        (
            "List of identical shapes",
            "List(#{#a => Integer})",
            "List(#{#a => Integer})",
            None,
        ),
        (
            "List covariant value",
            "List(#{#a => Integer, #b => Integer})",
            "List(#{#a => Integer, ...})",
            None,
        ),
        (
            "List wrong element key",
            "List(#{#b => Integer})",
            "List(#{#a => Integer})",
            Some("missing required key `#a`"),
        ),
        (
            "List of shapes to List of Dictionary",
            "List(#{#a => Integer})",
            "List(Dictionary)",
            None,
        ),
        (
            "List of Dictionary to List of shapes",
            "List(Dictionary)",
            "List(#{#a => Integer})",
            None,
        ),
        (
            "bare List to List of shapes",
            "List",
            "List(#{#a => Integer})",
            None,
        ),
        (
            "Dictionary with shape values",
            "Dictionary(Symbol, #{#x => Integer})",
            "Dictionary(Symbol, #{#x => Integer})",
            None,
        ),
        (
            "Dictionary with a wrong shape value",
            "Dictionary(Symbol, #{#y => Integer})",
            "Dictionary(Symbol, #{#x => Integer})",
            Some("missing required key `#x`"),
        ),
        (
            "shape holding a List of shapes",
            "#{#xs => List(#{#a => Integer})}",
            "#{#xs => List(#{#a => Integer})}",
            None,
        ),
        (
            "shape holding a List of the wrong shape",
            "#{#xs => List(#{#b => Integer})}",
            "#{#xs => List(#{#a => Integer})}",
            Some("in key `#xs`"),
        ),
    ]);
}

#[test]
fn shape_aliases_are_expanded_before_judging() {
    let msgs = diagnostics(
        "type Options = #{#port => Integer, ?#host => String}\n\
         Object subclass: Probe\n  \
           take: o :: Options => o\n  \
           ok: s :: #{#port => Integer} => self take: s\n  \
           missing: s :: #{#host => String} => self take: s\n  \
           stray: s :: #{#port => Integer, #hots => String} => self take: s\n",
    );
    assert_eq!(
        msgs.len(),
        2,
        "got: {:#?}",
        msgs.iter()
            .map(|d| (&d.message, &d.hint))
            .collect::<Vec<_>>()
    );
    assert!(
        msgs[0].hint.contains("missing required key `#port`"),
        "{:?}",
        msgs[0].hint
    );
    assert!(
        msgs[1].hint.contains("did you mean `#host`"),
        "{:?}",
        msgs[1].hint
    );
    assert!(
        msgs.iter().all(|d| d.message.contains("Options")),
        "alias is named: {:?}",
        msgs[0].message
    );
}

#[test]
fn field_default_assignment_and_return_are_judged() {
    let msgs = diagnostics(
        "Actor subclass: Foo\n  \
           state: ok :: #{#a => Integer} = Dictionary new\n  \
           state: typed :: #{#a => Integer} = Dictionary new\n  \
           state: cfg :: #{#a => Integer} = nil\n  \
           setOk: d :: Dictionary => self.cfg := d\n  \
           setShape: s :: #{#a => String} => self.cfg := s\n  \
           setTyped: d :: Dictionary(Symbol, Object) => self.cfg := d\n  \
           giveBack: s :: #{#a => Integer} -> #{#a => String} => s\n  \
           giveOk: s :: #{#a => Integer, #b => Integer} -> #{#a => Integer, ...} => s\n",
    );
    let text: Vec<String> = msgs
        .iter()
        .map(|d| format!("{} | {}", d.message, d.hint))
        .collect();
    assert_eq!(text.len(), 4, "got: {text:#?}");
    assert!(
        text.iter()
            .any(|m| m.contains("state `cfg`") && m.contains("Nil")),
        "{text:#?}"
    );
    assert!(
        text.iter().any(
            |m| m.contains("field `cfg`") && m.contains("key `#a` expects Integer, got String")
        ),
        "{text:#?}"
    );
    assert!(
        text.iter()
            .any(|m| m.contains("field `cfg`") && m.contains("Dictionary(Symbol, Object)")),
        "{text:#?}"
    );
    assert!(
        text.iter()
            .any(|m| m.contains("giveBack:") && m.contains("key `#a` expects String")),
        "{text:#?}"
    );
}

#[test]
fn override_parameter_and_return_use_shape_assignability() {
    let msgs = diagnostics(
        "Object subclass: Parent\n  \
           take: o :: #{#a => Integer, ...} => o\n  \
           give -> #{#a => Integer, ...} => Dictionary new\n\n\
         Parent subclass: Child\n  \
           take: o :: #{#a => Integer, #b => String} => o\n  \
           give -> #{#a => Integer, #b => String} => Dictionary new\n",
    );
    let text: Vec<String> = msgs.iter().map(|d| d.message.clone()).collect();
    // The narrowing return is a legal refinement; the parameter override is
    // judged with the pre-existing child-vs-parent direction and is flagged.
    assert!(
        text.iter().all(|m| !m.contains("declares return type")),
        "{text:#?}"
    );
}

#[test]
fn spawn_with_value_is_judged_against_a_shape_slot() {
    let msgs = diagnostics(
        "Actor subclass: Holder\n  \
           state: cfg :: #{#port => Integer} = Dictionary new\n\n\
         Object subclass: Maker\n  \
           ok => Holder spawnWith: #{#cfg => Dictionary new}\n  \
           bad => Holder spawnWith: #{#cfg => 3}\n",
    );
    assert_eq!(
        msgs.len(),
        1,
        "got: {:#?}",
        msgs.iter()
            .map(|d| (&d.message, &d.hint))
            .collect::<Vec<_>>()
    );
    assert!(
        msgs[0].message.contains("state key `cfg`"),
        "{:?}",
        msgs[0].message
    );
}

#[test]
fn protocol_typed_slots_are_left_to_conformance_checks() {
    let msgs = diagnostics(
        "Protocol define: Named\n  name -> String\n\n\
         Object subclass: Probe\n  \
           take: o :: Named => o\n  \
           either: o :: Named | #{#a => Integer} => o\n  \
           go: s :: #{#a => Integer} => self either: s\n",
    );
    assert!(
        msgs.iter().all(|d| !d.message.contains("expects Named")),
        "{:#?}",
        msgs.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn shape_arriving_through_a_receiver_type_parameter_is_checked_at_send_sites() {
    let msgs = diagnostics(
        "Object subclass: Probe\n  \
           put: d :: Dictionary(Symbol, #{#port => Integer}) => d at: #a put: 42\n  \
           putOk: d :: Dictionary(Symbol, #{#port => Integer}) => d at: #a put: Dictionary new\n  \
           add: l :: List(#{#port => Integer}) => l add: \"x\"\n",
    );
    assert_eq!(
        msgs.len(),
        2,
        "got: {:#?}",
        msgs.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn block_parameters_are_contravariant_in_shapes() {
    run(&[
        (
            "block accepting a wider shape fits",
            "Block(#{#port => Integer, ...}, Integer)",
            "Block(#{#port => Integer, #host => String}, Integer)",
            None,
        ),
        (
            "block needing more keys is rejected",
            "Block(#{#port => Integer, #host => String}, Integer)",
            "Block(#{#port => Integer, ...}, Integer)",
            Some("Block("),
        ),
        (
            "block result is covariant",
            "Block(Integer, #{#a => Integer, #b => Integer})",
            "Block(Integer, #{#a => Integer, ...})",
            None,
        ),
    ]);
}

#[test]
fn closed_shape_widens_to_dictionary_symbol_v() {
    run(&[
        (
            "Object values accept any field types",
            "#{#a => Integer, #b => String}",
            "Dictionary(Symbol, Object)",
            None,
        ),
        (
            "Integer values when every field is Integer",
            "#{#a => Integer, #b => Integer}",
            "Dictionary(Symbol, Integer)",
            None,
        ),
        (
            "optional fields count",
            "#{#a => Integer, ?#b => String}",
            "Dictionary(Symbol, Object)",
            None,
        ),
        (
            "nested shape field is an Object",
            "#{#a => #{#b => Integer}}",
            "Dictionary(Symbol, Object)",
            None,
        ),
        (
            "empty closed shape",
            "#{}",
            "Dictionary(Symbol, Object)",
            None,
        ),
        (
            "field type mismatch with V",
            "#{#a => Integer, #b => String}",
            "Dictionary(Symbol, Integer)",
            Some("Dictionary(Symbol, Integer)"),
        ),
        (
            "non-Symbol key type",
            "#{#a => Integer}",
            "Dictionary(String, Object)",
            Some("Dictionary(String, Object)"),
        ),
    ]);
}

#[test]
fn closed_shape_widens_through_a_send_argument() {
    let diags = diagnostics(
        "Object subclass: Probe\n  check: d :: Dictionary(Symbol, Object) => d\n  go => self check: #{#a => 1, #b => \"x\"}\n  goShaped: s :: #{#a => Integer, #b => String} => self check: s\n",
    );
    assert!(
        diags.is_empty(),
        "{:#?}",
        diags
            .iter()
            .map(|d| (&d.message, &d.hint))
            .collect::<Vec<_>>()
    );
}

#[test]
fn misspelt_literal_key_against_a_shape_still_suggests() {
    let diags = diagnostics(
        "Object subclass: Probe\n  take: o :: #{?#maxFrameSize => Integer} => o\n  go => self take: #{#maxFramSize => 1}\n",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.hint.contains("did you mean `#maxFrameSize`")),
        "{:#?}",
        diags
            .iter()
            .map(|d| (&d.message, &d.hint))
            .collect::<Vec<_>>()
    );
}
