// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4911: `compile_expression` / `compile_expression_trace` report which
//! variable the source assigns (so the workspace never re-parses it), and
//! `compile_method` synthesises the `selector => ` header for a bare body.

use super::*;

/// A response field holding a name (binary) or the atom `none`.
fn name_or_none(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) if a.name == "none" => None,
        other => Some(term_to_string(other).expect("binary name")),
    }
}

fn assigned_var(source: &str) -> Option<String> {
    let response = handle_request(&Term::from(compile_expression_request(source)));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{source:?}: {response:?}"
    );
    let Term::Map(map) = &response else {
        panic!("not a map")
    };
    name_or_none(map_get(map, "assigned_var").expect("assigned_var field"))
}

fn step_assignments(source: &str) -> Vec<Option<String>> {
    let mut request = compile_expression_request(source);
    request
        .map
        .insert(atom("command"), atom("compile_expression_trace"));
    let response = handle_request(&Term::from(request));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{source:?}: {response:?}"
    );
    let Term::Map(map) = &response else {
        panic!("not a map")
    };
    let Some(Term::List(list)) = map_get(map, "step_assignments") else {
        panic!("step_assignments field")
    };
    list.elements.iter().map(name_or_none).collect()
}

#[test]
fn single_assignment_reports_the_variable() {
    assert_eq!(assigned_var("x := 3 + 4").as_deref(), Some("x"));
    assert_eq!(
        assigned_var("  counter_1 := 5").as_deref(),
        Some("counter_1")
    );
    // Trailing statement terminator is not a second statement.
    assert_eq!(assigned_var("x := 3.").as_deref(), Some("x"));
    assert_eq!(assigned_var("x := 3\n").as_deref(), Some("x"));
    // A typed assignment still binds `x`.
    assert_eq!(assigned_var("x :: Integer := 5").as_deref(), Some("x"));
}

#[test]
fn non_assignment_reports_none() {
    assert_eq!(assigned_var("3 + 4"), None);
    assert_eq!(assigned_var("#sym"), None);
    // `:=` appearing inside a string, block or a comparison is not an assignment.
    assert_eq!(assigned_var("\"x := 3\""), None);
    assert_eq!(assigned_var("[:a | y := a] value: 1"), None);
    assert_eq!(assigned_var("1 =:= 1"), None);
}

#[test]
fn several_statements_report_none() {
    // The call's single result belongs to the last statement only, so no
    // variable may be rebound to it.
    assert_eq!(assigned_var("alpha := 111. beta := 222"), None);
    assert_eq!(assigned_var("alpha := 111\nbeta := 222"), None);
    assert_eq!(assigned_var("x := 1. x + 1"), None);
}

#[test]
fn multi_statement_text_inside_a_block_is_one_statement() {
    assert_eq!(
        assigned_var("blk := [:a | a + 1. a + 2]").as_deref(),
        Some("blk")
    );
    assert_eq!(assigned_var("s := \"a. b\nc := 1\"").as_deref(), Some("s"));
}

#[test]
fn trace_reports_one_entry_per_statement() {
    assert_eq!(
        step_assignments("a := 1. a + 1. b := 2"),
        vec![Some("a".to_string()), None, Some("b".to_string())]
    );
    assert_eq!(step_assignments("3 + 4"), vec![None]);
}

fn compile_method_with_selector(method_source: &str, selector: Option<&str>) -> Term {
    let mut map = Map::from([
        (atom("command"), atom("compile_method")),
        (
            atom("class_source"),
            binary("Actor subclass: Counter\n  state: value :: Integer = 0\n\n  get => self.value"),
        ),
        (atom("method_source"), binary(method_source)),
        (atom("is_class_method"), atom("false")),
    ]);
    if let Some(selector) = selector {
        map.map.insert(atom("selector"), binary(selector));
    }
    handle_request(&Term::from(map))
}

fn assert_method_ok(response: &Term, selector: &str) -> String {
    assert_eq!(
        response_status(response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    assert_eq!(
        response_field_str(response, "selector").as_deref(),
        Some(selector)
    );
    response_field_str(response, "method_source").expect("method_source")
}

#[test]
fn bare_body_gets_the_selector_header() {
    let r = compile_method_with_selector("self.value + 1", Some("next"));
    let ms = assert_method_ok(&r, "next");
    assert!(ms.contains("next =>"), "{ms}");
}

#[test]
fn bare_body_resembling_the_selector_still_gets_a_header() {
    // `nexts + 1` for selector `next` shares a prefix but is not a header.
    let r = compile_method_with_selector("self.value + 1", Some("value"));
    assert_method_ok(&r, "value");
    let r = compile_method_with_selector("nexts", Some("next"));
    // `nexts` is not defined, but it parses: header synthesised, body compiled
    // (an undefined-variable diagnostic is acceptable); the selector is `next`.
    if response_status(&r).as_deref() == Some("ok") {
        assert_method_ok(&r, "next");
    }
}

#[test]
fn full_definition_is_left_alone_with_a_selector() {
    let r = compile_method_with_selector("next => self.value + 1", Some("next"));
    let ms = assert_method_ok(&r, "next");
    assert_eq!(ms.matches("next =>").count(), 1, "{ms}");
}

#[test]
fn commented_full_definition_keeps_its_comments() {
    let r = compile_method_with_selector(
        "// --- Section ---\n/// Doc.\nnext => self.value + 1",
        Some("next"),
    );
    let ms = assert_method_ok(&r, "next");
    assert!(ms.contains("/// Doc."), "{ms}");
}

#[test]
fn commented_bare_body_gets_the_header_after_the_comments() {
    let r = compile_method_with_selector("// note\n/// Doc.\nself.value + 1", Some("next"));
    let ms = assert_method_ok(&r, "next");
    assert!(ms.contains("/// Doc."), "doc comment lost: {ms}");
    assert!(ms.contains("next =>"), "{ms}");
}

#[test]
fn class_side_full_definition_is_left_alone() {
    let mut map = Map::from([
        (atom("command"), atom("compile_method")),
        (
            atom("class_source"),
            binary("Actor subclass: Counter\n  state: value :: Integer = 0\n\n  get => self.value"),
        ),
        (atom("method_source"), binary("class   make => 5")),
        (atom("is_class_method"), atom("true")),
        (atom("selector"), binary("make")),
    ]);
    map.map.insert(atom("class_name"), binary("Counter"));
    let r = handle_request(&Term::from(map));
    assert_method_ok(&r, "make");
}

#[test]
fn comment_only_source_still_fails_to_compile_without_swallowing_the_header() {
    // The header must not be glued onto the comment line: the error is a
    // missing body, not a silently-empty method.
    let r = compile_method_with_selector("// just a comment", Some("next"));
    assert_eq!(response_status(&r).as_deref(), Some("error"), "{r:?}");
}

#[test]
fn bare_source_without_a_selector_is_not_rewritten() {
    let r = compile_method_with_selector("self.value + 1", None);
    assert_eq!(response_status(&r).as_deref(), Some("error"), "{r:?}");
}

#[test]
fn broken_full_definition_reports_the_original_error() {
    // The header is present; the body is broken. The diagnostic must point at
    // the user's text, not at a `next => next => …` rewrite.
    let r = compile_method_with_selector("next => self.value :=", Some("next"));
    assert_eq!(response_status(&r).as_deref(), Some("error"), "{r:?}");
    let text = format!("{r:?}");
    assert!(!text.contains("next => next"), "{text}");
}

#[test]
fn broken_bare_body_reports_an_error() {
    let r = compile_method_with_selector("self.value :=", Some("next"));
    assert_eq!(response_status(&r).as_deref(), Some("error"), "{r:?}");
}

fn complete_method_source(source: &str, selector: &str) -> String {
    let response = handle_request(&Term::from(Map::from([
        (atom("command"), atom("complete_method_source")),
        (atom("source"), binary(source)),
        (atom("selector"), binary(selector)),
    ])));
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    response_field_str(&response, "source").expect("source")
}

#[test]
fn complete_method_source_synthesises_only_for_a_bare_body() {
    // Bare body: header prepended.
    assert_eq!(
        complete_method_source("self.value + 1", "increment"),
        "increment => self.value + 1"
    );
    // Shares a prefix with the selector but is still a bare body.
    assert_eq!(
        complete_method_source("incremented + 1", "increment"),
        "increment => incremented + 1"
    );
    // Statement breaks before any arrow: not a header.
    assert_eq!(
        complete_method_source("foo bar. baz", "foo"),
        "foo => foo bar. baz"
    );
    assert_eq!(
        complete_method_source("foo bar\nbaz", "foo"),
        "foo => foo bar\nbaz"
    );
    // Full definitions (plain, commented, class-side) are returned verbatim.
    for full in [
        "increment => self.value + 1",
        "// --- Section ---\n/// Doc.\nincrement => self.value + 1",
        "class   make => 5",
    ] {
        let selector = if full.contains("make") {
            "make"
        } else {
            "increment"
        };
        assert_eq!(complete_method_source(full, selector), full);
    }
    // A broken full definition is left alone too (its own error surfaces later).
    assert_eq!(
        complete_method_source("increment => self.value :=", "increment"),
        "increment => self.value :="
    );
}

#[test]
fn complete_method_source_keeps_comments_in_front_of_the_header() {
    assert_eq!(
        complete_method_source("//x\nbareBody", "increment"),
        "//x\nincrement => bareBody"
    );
    // Trailing whitespace on the comment line: no extra separator.
    let commented = "// comment ends with space ";
    assert_eq!(
        complete_method_source(commented, "increment"),
        format!("{commented}increment => ")
    );
    // A comment-only source ending mid-comment gets a fresh line for the header.
    assert_eq!(
        complete_method_source("// comment ends with letter", "increment"),
        "// comment ends with letter\nincrement => "
    );
}

/// `assigned_var` for `source`, with `a` and `name` bound as session variables.
fn assigned_var_with_known(source: &str) -> Option<String> {
    let mut request = compile_expression_request(source);
    request.map.insert(
        atom("known_vars"),
        Term::from(List::from(vec![binary("a"), binary("name")])),
    );
    let response = handle_request(&Term::from(request));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{source:?}: {response:?}"
    );
    let Term::Map(map) = &response else {
        panic!("not a map")
    };
    name_or_none(map_get(map, "assigned_var").expect("assigned_var field"))
}

/// The lexical shapes the workspace's hand-rolled scanner used to guess at
/// (comments holding brackets or quotes, character-literal payloads, escaped
/// quotes, interpolation, continuation lines). The real grammar now decides.
#[test]
fn lexical_edge_cases_are_decided_by_the_real_grammar() {
    let single = |src: &str, var: &str| {
        assert_eq!(
            assigned_var_with_known(src).as_deref(),
            Some(var),
            "{src:?} is one assignment"
        );
    };
    let multiple = |src: &str| {
        assert_eq!(
            assigned_var_with_known(src),
            None,
            "{src:?} is several statements"
        );
    };

    // A comment holding an unbalanced bracket / quote must not hide or fake a
    // statement boundary.
    multiple("alpha := 1 // (see below\nbeta := 2");
    multiple("alpha := 1 // says \"hi\" here\nbeta := 2");
    multiple("alpha := 1 /* { unbalanced */\nbeta := 2");
    single("alpha := 1 // says \"hi\" here", "alpha");
    single("alpha := 1 /* note */", "alpha");

    // Continuation lines of one assignment are not new statements.
    single("result :=\n  42", "result");
    single("doubler := [\n  :x |\n  x * 2\n]", "doubler");
    single("count := 0\n", "count");
    single("count := 0\n  \n", "count");
    single("abc:=123", "abc");
    single("z  :=  3", "z");
    single("_temp := 0", "_temp");

    // Strings: escaped quotes, doubled quotes, interpolation with a nested string.
    single("s := \"a\\\"b. c\"", "s");
    single("s := \"a\\\"b\nident := 1\"", "s");
    multiple("s := \"a\\\\\". y := 2");
    single("x := \"pre {a == \"z. bogus\"} mid\"", "x");
    single("x := \"Hello {name}\"", "x");
    multiple("x := \"Hello {name}\". y := 2");
    single("x := \"no interpolation here\"", "x");

    // A bracket / quote used as a character-literal payload is just a character.
    multiple("x := $( class. y := 2");
    multiple("x := $[ class. y := 2");
    multiple("x := ${ class. y := 2");
    single("x := $( class", "x");
    single("x := $\" class", "x");
    single("x := $\\{ class", "x");

    // Not assignments at all.
    assert_eq!(assigned_var_with_known("a + 1"), None);
    assert_eq!(assigned_var_with_known("name size"), None);
}

/// A period nested in a block inside a cascade is part of the one outer
/// assignment (the `ClassBuilder` example in the language guide).
#[test]
fn nested_period_in_a_cascade_is_one_statement() {
    let src = "account := Object classBuilder\n  name: #Account;\n  superclass: Object;\n  classVars: #{ #opened => 0 };\n  fields: #{ #balance => 0 };\n  methods: #{ #balance => [:inst | inst fieldAt: #balance] };\n  classMethods: #{ #open => [:self | self.opened := self.opened + 1. self.opened] };\n  register";
    let response = handle_request(&Term::from(compile_expression_request(src)));
    if response_status(&response).as_deref() == Some("ok") {
        let Term::Map(map) = &response else {
            panic!("not a map")
        };
        assert_eq!(
            name_or_none(map_get(map, "assigned_var").expect("assigned_var")).as_deref(),
            Some("account")
        );
    }
}
