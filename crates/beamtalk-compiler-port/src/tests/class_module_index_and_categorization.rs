// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `build_class_module_index_in_source`, `categorize_methods` divider grouping, and `class_state_field_defaults` presence reporting.

use super::*;

// --- build_class_module_index_in_source tests ---

#[test]
fn build_class_module_index_in_source_root_file() {
    let request = Map::from([
        (atom("command"), atom("build_class_module_index_in_source")),
        (
            atom("source"),
            binary("Object subclass: HttpResponse\n  ok -> Boolean => true\n"),
        ),
        (atom("relative_path"), binary("HttpResponse.bt")),
        (atom("package_name"), binary("web")),
    ]);
    let response = handle_build_class_module_index_in_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    assert_eq!(
        map_get(m, "module_name").and_then(term_to_string),
        Some("bt@web@http_response".to_string()),
        "{response:?}"
    );
    assert_eq!(
        map_get(m, "classes").and_then(term_to_string_list),
        Some(vec!["HttpResponse".to_string()]),
        "{response:?}"
    );
}

#[test]
fn build_class_module_index_in_source_subdirectory_and_multiple_classes() {
    // Exercises the exact bug shape a regex scanner would risk: a
    // subdirectory path segment and more than one
    // class declared in the same file.
    let request = Map::from([
        (atom("command"), atom("build_class_module_index_in_source")),
        (
            atom("source"),
            binary("Object subclass: Alpha\n\nActor subclass: Beta\n  state: x = 0\n"),
        ),
        (atom("relative_path"), binary("util/multi.bt")),
        (atom("package_name"), binary("web")),
    ]);
    let response = handle_build_class_module_index_in_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    assert_eq!(
        map_get(m, "module_name").and_then(term_to_string),
        Some("bt@web@util@multi".to_string()),
        "{response:?}"
    );
    assert_eq!(
        map_get(m, "classes").and_then(term_to_string_list),
        Some(vec!["Alpha".to_string(), "Beta".to_string()]),
        "{response:?}"
    );
}

#[test]
fn build_class_module_index_in_source_invalid_path_segment_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("build_class_module_index_in_source")),
        (atom("source"), binary("Object subclass: Foo\n")),
        (atom("relative_path"), binary("bad-segment.bt")),
        (atom("package_name"), binary("web")),
    ]);
    let response = handle_build_class_module_index_in_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(
        map_get(m, "reason"),
        Some(&atom("invalid_path_segment")),
        "{response:?}"
    );
}

#[test]
fn build_class_module_index_in_source_missing_field_is_error() {
    let request = Map::from([
        (atom("command"), atom("build_class_module_index_in_source")),
        (atom("relative_path"), binary("foo.bt")),
        (atom("package_name"), binary("web")),
    ]);
    let response = handle_build_class_module_index_in_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
}

// --- categorize_methods tests ---

const CATEGORY_FIXTURE: &str = "\
Object subclass: Counter
  // === Construction ===
  class new => self basicNew

  // === Arithmetic ===
  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
";

const DIVIDER_FIXTURE: &str = "\
Object subclass: Counter

  foo => 1

  // === Section ===

  class bar => 2
";

#[test]
fn categorize_methods_groups_by_divider() {
    let request = Map::from([
        (atom("command"), atom("categorize_methods")),
        (atom("source"), binary(CATEGORY_FIXTURE)),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_categorize_methods(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(categories)) = map_get(m, "categories") else {
        panic!("categories should be a list: {response:?}");
    };
    assert_eq!(categories.elements.len(), 2);

    let Term::Map(first) = &categories.elements[0] else {
        panic!("category should be a map");
    };
    assert_eq!(
        map_get(first, "name").and_then(term_to_string).as_deref(),
        Some("Construction")
    );
    let Some(Term::List(first_methods)) = map_get(first, "methods") else {
        panic!("methods should be a list");
    };
    assert_eq!(first_methods.elements.len(), 1);
    let Term::Map(first_method) = &first_methods.elements[0] else {
        panic!("method should be a map");
    };
    assert_eq!(
        map_get(first_method, "selector")
            .and_then(term_to_string)
            .as_deref(),
        Some("new")
    );
    assert_eq!(map_get(first_method, "side"), Some(&atom("class")));

    let Term::Map(second) = &categories.elements[1] else {
        panic!("category should be a map");
    };
    assert_eq!(
        map_get(second, "name").and_then(term_to_string).as_deref(),
        Some("Arithmetic")
    );
    let Some(Term::List(second_methods)) = map_get(second, "methods") else {
        panic!("methods should be a list");
    };
    assert_eq!(second_methods.elements.len(), 2);
}

// `divider_span`/method `span` extend the original
// `name`/`selector`/`side`-only shape — the Cockpit's `save-section`
// write path needs the divider's own byte span to splice a rename.
#[test]
fn categorize_methods_groups_by_divider_includes_spans() {
    let request = Map::from([
        (atom("command"), atom("categorize_methods")),
        (atom("source"), binary(DIVIDER_FIXTURE)),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_categorize_methods(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(categories)) = map_get(m, "categories") else {
        panic!("categories should be a list: {response:?}");
    };
    assert_eq!(categories.elements.len(), 2, "{response:?}");

    let Term::Map(ref leading) = categories.elements[0] else {
        panic!("category should be a map");
    };
    assert_eq!(map_get(leading, "name"), Some(&atom("undefined")));
    let Some(Term::List(leading_methods)) = map_get(leading, "methods") else {
        panic!("methods should be a list");
    };
    assert_eq!(leading_methods.elements.len(), 1);

    let Term::Map(ref section) = categories.elements[1] else {
        panic!("category should be a map");
    };
    assert_eq!(
        map_get(section, "name").and_then(term_to_string),
        Some("Section".to_string())
    );
    assert!(
        matches!(map_get(section, "divider_span"), Some(Term::Map(_))),
        "{response:?}"
    );
    let Some(Term::List(section_methods)) = map_get(section, "methods") else {
        panic!("methods should be a list");
    };
    assert_eq!(section_methods.elements.len(), 1);
    let Term::Map(ref bar) = section_methods.elements[0] else {
        panic!("method should be a map");
    };
    assert_eq!(
        map_get(bar, "selector").and_then(term_to_string),
        Some("bar".to_string())
    );
    assert_eq!(map_get(bar, "side"), Some(&atom("class")));
    assert!(
        matches!(map_get(bar, "span"), Some(Term::Map(_))),
        "{response:?}"
    );
}

#[test]
fn categorize_methods_no_dividers_is_single_unnamed_category() {
    let request = Map::from([
        (atom("command"), atom("categorize_methods")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_categorize_methods(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(categories)) = map_get(m, "categories") else {
        panic!("categories should be a list: {response:?}");
    };
    assert_eq!(categories.elements.len(), 1, "{response:?}");
    let Term::Map(ref category) = categories.elements[0] else {
        panic!("category should be a map");
    };
    // `name` is always present, using the atom `undefined` as
    // the "absent" sentinel — see `handle_categorize_methods`'s doc for
    // why this supersedes the original omitted-key convention.
    assert_eq!(map_get(category, "name"), Some(&atom("undefined")));
}

#[test]
fn categorize_methods_class_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("categorize_methods")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("NoSuchClass")),
    ]);
    let response = handle_categorize_methods(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("class_not_found")));
}

#[test]
fn categorize_methods_missing_source_field_is_error() {
    let request = Map::from([
        (atom("command"), atom("categorize_methods")),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_categorize_methods(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
}

// --- class_state_field_defaults tests (ADR 0082 extension) ---

#[test]
fn class_state_field_defaults_reports_presence_per_field() {
    let request = Map::from([
        (atom("command"), atom("class_state_field_defaults")),
        (
            atom("source"),
            binary("Actor subclass: Counter\n  state: count = 0\n  state: label :: String\n"),
        ),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_class_state_field_defaults(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::Map(field_defaults)) = map_get(m, "field_defaults") else {
        panic!("field_defaults should be a map: {response:?}");
    };
    // Field names are wire binaries (never atoms — user-controlled class
    // field names must never risk atom-table exhaustion), so look them up
    // directly rather than via `map_get` (atom-keyed lookups only).
    assert_eq!(
        field_defaults.map.get(&binary("count")),
        Some(&atom("true")),
        "{field_defaults:?}"
    );
    assert_eq!(
        field_defaults.map.get(&binary("label")),
        Some(&atom("false")),
        "{field_defaults:?}"
    );
}

#[test]
fn class_state_field_defaults_class_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("class_state_field_defaults")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("NoSuchClass")),
    ]);
    let response = handle_class_state_field_defaults(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("class_not_found")));
}

#[test]
fn class_state_field_defaults_missing_class_name_field_is_error() {
    let request = Map::from([
        (atom("command"), atom("class_state_field_defaults")),
        (atom("source"), binary(SPAN_FIXTURE)),
    ]);
    let response = handle_class_state_field_defaults(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
}
