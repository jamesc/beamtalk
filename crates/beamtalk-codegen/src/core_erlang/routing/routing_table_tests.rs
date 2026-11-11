// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The generator of `routing/table.rs` and its drift test (BT-4292).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! `table.rs` is rendered from the shared fixture
//! `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`.
//! [`routing_table_is_generated_from_fixture`] re-renders it and fails when
//! the checked-in file differs; `ROUTING_TABLE_BLESS=1` rewrites it instead.
//! The render is Rust source, not Core Erlang, so plain string building is
//! the right tool here.

use super::{Guard, Representation, table};
use serde_json::Value;
use std::collections::BTreeSet;
use std::fmt::Write as _;

const FIXTURE: &str =
    "runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json";
const TABLE: &str = "crates/beamtalk-codegen/src/core_erlang/routing/table.rs";

fn fixture() -> Value {
    let path = crate::core_erlang::tests::repo_root().join(FIXTURE);
    let raw = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    serde_json::from_str(&raw).expect("routing fixture is JSON")
}

fn str_field<'a>(row: &'a Value, key: &str) -> &'a str {
    row[key]
        .as_str()
        .unwrap_or_else(|| panic!("fixture field {key:?} is not a string in {row}"))
}

fn opt_str_field<'a>(row: &'a Value, key: &str) -> Option<&'a str> {
    match &row[key] {
        Value::Null => None,
        Value::String(s) => Some(s),
        other => panic!("fixture field {key:?} must be a string or null, got {other}"),
    }
}

fn str_list(row: &Value, key: &str) -> Vec<String> {
    match &row[key] {
        Value::Null => Vec::new(),
        Value::Array(items) => items
            .iter()
            .map(|item| {
                item.as_str()
                    .unwrap_or_else(|| panic!("{key:?} must hold strings, got {item}"))
                    .to_string()
            })
            .collect(),
        other => panic!("fixture field {key:?} must be a list, got {other}"),
    }
}

fn representation(name: &str) -> &'static str {
    match name {
        "raw_term" => "RawTerm",
        "untagged_map" => "UntaggedMap",
        "record" => "Record",
        "proxy_routed" => "ProxyRouted",
        "supervisor_tuple" => "SupervisorTuple",
        other => panic!("unknown representation {other:?} in the routing fixture"),
    }
}

fn guard(value: &Value) -> String {
    if value.is_null() {
        return "None".to_string();
    }
    let kind = str_field(value, "kind");
    let rendered = match kind {
        "bif" => format!("Guard::Bif({:?})", str_field(value, "bif")),
        "string_tag" => "Guard::StringTag".to_string(),
        "binary_or_string_tag" => "Guard::BinaryOrStringTag".to_string(),
        "unaligned_bitstring" => "Guard::UnalignedBitstring".to_string(),
        "unreserved_tuple" => "Guard::UnreservedTuple".to_string(),
        "symbol" => "Guard::Symbol".to_string(),
        "exact_atom" => format!("Guard::ExactAtom({:?})", str_field(value, "atom")),
        "untagged_map" => "Guard::UntaggedMap".to_string(),
        other => panic!("unknown guard kind {other:?} in the routing fixture"),
    };
    format!("Some({rendered})")
}

fn opt_str(value: Option<&str>) -> String {
    value.map_or_else(|| "None".to_string(), |s| format!("Some({s:?})"))
}

fn str_slice(items: &[String]) -> String {
    let quoted: Vec<String> = items.iter().map(|s| format!("{s:?}")).collect();
    format!("&[{}]", quoted.join(", "))
}

/// Renders `table.rs` from the fixture.
fn render(fixture: &Value) -> String {
    let mut out = String::from(
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         //! GENERATED from\n\
         //! `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`\n\
         //! by `routing_table_tests.rs` (BT-4292). Do not edit: change the fixture and\n\
         //! run `ROUTING_TABLE_BLESS=1 cargo test -p beamtalk-codegen --lib routing_table`.\n\
         \n\
         use super::{Guard, Representation, ReservedTupleTag, RoutingRow};\n\
         \n\
         #[rustfmt::skip]\n\
         pub(super) static ROUTING_ROWS: &[RoutingRow] = &[\n",
    );
    for row in fixture["rows"].as_array().expect("fixture `rows` list") {
        writeln!(
            out,
            "    RoutingRow {{ class: {:?}, aliases: {}, representation: Representation::{}, \
             guard: {}, target_module: {}, special_selectors: {}, dynamic_only: {} }},",
            str_field(row, "class"),
            str_slice(&str_list(row, "aliases")),
            representation(str_field(row, "representation")),
            guard(&row["guard"]),
            opt_str(opt_str_field(row, "target_module")),
            str_slice(&str_list(row, "special_selectors")),
            opt_str(opt_str_field(row, "dynamic_only")),
        )
        .expect("write to String");
    }
    out.push_str(
        "];\n\
         \n\
         #[rustfmt::skip]\n\
         pub(super) static RESERVED_TUPLE_TAGS: &[ReservedTupleTag] = &[\n",
    );
    for tag in fixture["reserved_tuple_tags"]
        .as_array()
        .expect("fixture `reserved_tuple_tags` list")
    {
        let size = tag["size"]
            .as_u64()
            .expect("reserved tag `size` is a number");
        writeln!(
            out,
            "    ReservedTupleTag {{ tag: {:?}, size: {size} }},",
            str_field(tag, "tag")
        )
        .expect("write to String");
    }
    writeln!(
        out,
        "];\n\
         \n\
         /// Selectors the dynamic entry point rewrites before classifying the receiver.\n\
         pub(super) static REWRITTEN_SELECTORS: &[&str] = {};\n\
         \n\
         /// The root of the exception family, which the runtime routes to\n\
         /// `bt@stdlib@exception:dispatch` instead of the instance's own module.\n\
         pub(in crate::core_erlang) const EXCEPTION_ROOT_CLASS: &str = {:?};",
        str_slice(&str_list(fixture, "rewritten_selectors")),
        str_field(fixture, "exception_root")
    )
    .expect("write to String");
    out
}

/// BT-4292: `table.rs` is exactly what the fixture renders to, so codegen
/// reads no representation fact the runtime's conformance test does not
/// check. On drift, regenerate (never hand-edit) with `ROUTING_TABLE_BLESS=1`.
#[test]
fn routing_table_is_generated_from_fixture() {
    let rendered = render(&fixture());
    let path = crate::core_erlang::tests::repo_root().join(TABLE);
    if std::env::var_os("ROUTING_TABLE_BLESS").is_some() {
        std::fs::write(&path, &rendered).unwrap_or_else(|e| panic!("write {path:?}: {e}"));
        return;
    }
    let checked_in =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    assert!(
        checked_in == rendered,
        "{TABLE} drifted from {FIXTURE}; regenerate it with \
         `ROUTING_TABLE_BLESS=1 cargo test -p beamtalk-codegen --lib routing_table`"
    );
}

/// A class listed twice would shadow: lookups return the first row, so the
/// second could never take effect however wrong it was.
#[test]
fn routing_table_names_each_class_once() {
    let mut seen = BTreeSet::new();
    for row in table::ROUTING_ROWS {
        for name in std::iter::once(&row.class).chain(row.aliases) {
            assert!(
                seen.insert(*name),
                "class {name:?} appears twice in the routing table"
            );
        }
    }
}

/// The `Tuple` test is rendered per reserved tag size; a tag listed twice at
/// one size would emit a duplicate (unreachable) case clause.
#[test]
fn routing_table_reserved_tags_are_distinct() {
    let mut seen = BTreeSet::new();
    for tag in table::RESERVED_TUPLE_TAGS {
        assert!(
            seen.insert((tag.tag, tag.size)),
            "reserved tag {tag:?} listed twice"
        );
        assert!(tag.size >= 1, "reserved tag {tag:?} has no first element");
    }
}

/// A row that names a target module but also a `dynamic_only` reason, or a
/// guard-less row without a reason, would leave a later guarded-send consumer
/// (BT-4294) to guess; every row says which it is.
#[test]
fn routing_table_rows_without_a_direct_route_say_why() {
    for row in table::ROUTING_ROWS {
        let direct_route = row.guard.is_some() && row.target_module.is_some();
        assert!(
            direct_route || row.dynamic_only.is_some(),
            "{} has no guard or no target module, so it must give a dynamic_only reason",
            row.class
        );
        if row.representation == Representation::SupervisorTuple {
            assert_eq!(
                row.guard, None::<Guard>,
                "{}: supervisor rows carry no guard",
                row.class
            );
        }
    }
}

/// BT-4294, BT-4390: the built-in rows a typed send of an ordinary selector
/// (one no row lists as special) may direct-call, as derived from the
/// fixture by `builtin_send_route`. Pins the derivation, not the facts
/// (those live in the fixture): a fixture change that adds or drops a direct
/// route fails here, so its author re-checks the guard against the dynamic
/// path's clause and updates the differential test's rows.
#[test]
fn builtin_send_routes_are_the_closed_table() {
    use super::{BuiltinRoute, SendGuard, StringTagRoute, builtin_send_route};
    let routed: Vec<(&str, BuiltinRoute)> = table::ROUTING_ROWS
        .iter()
        .filter_map(|row| match builtin_send_route(row.class, "frobnicate") {
            Some(Ok(route)) => Some((row.class, route)),
            _ => None,
        })
        .collect();
    let route = |guard, target_module| BuiltinRoute {
        guard,
        target_module,
        string_tag_route: None,
    };
    assert_eq!(
        routed,
        vec![
            (
                "Integer",
                route(
                    SendGuard::Exact(Guard::Bif("is_integer")),
                    "bt@stdlib@integer"
                )
            ),
            (
                "Float",
                route(SendGuard::Exact(Guard::Bif("is_float")), "bt@stdlib@float")
            ),
            (
                "String",
                route(SendGuard::Exact(Guard::StringTag), "bt@stdlib@string")
            ),
            // BT-4510: a bare binary goes to `Binary`, the `String` tag to
            // the `String` row's route.
            (
                "Binary",
                BuiltinRoute {
                    guard: SendGuard::BareBinary,
                    target_module: "bt@stdlib@binary",
                    string_tag_route: Some(StringTagRoute {
                        guard: SendGuard::Exact(Guard::StringTag),
                        target_module: "bt@stdlib@string",
                    }),
                }
            ),
            (
                "Symbol",
                route(SendGuard::Exact(Guard::Symbol), "bt@stdlib@symbol")
            ),
            (
                "True",
                route(SendGuard::Exact(Guard::ExactAtom("true")), "bt@stdlib@true")
            ),
            (
                "False",
                route(
                    SendGuard::Exact(Guard::ExactAtom("false")),
                    "bt@stdlib@false"
                )
            ),
            (
                "UndefinedObject",
                route(
                    SendGuard::Exact(Guard::ExactAtom("nil")),
                    "bt@stdlib@undefined_object"
                )
            ),
            (
                "Block",
                route(
                    SendGuard::Exact(Guard::Bif("is_function")),
                    "bt@stdlib@block"
                )
            ),
            (
                "Tuple",
                route(SendGuard::TupleWithoutReservedHead, "bt@stdlib@tuple")
            ),
            (
                "List",
                route(SendGuard::Exact(Guard::Bif("is_list")), "bt@stdlib@list")
            ),
            (
                "Pid",
                route(SendGuard::Exact(Guard::Bif("is_pid")), "bt@stdlib@pid")
            ),
            (
                "Port",
                route(SendGuard::Exact(Guard::Bif("is_port")), "bt@stdlib@port")
            ),
            (
                "Reference",
                route(
                    SendGuard::Exact(Guard::Bif("is_reference")),
                    "bt@stdlib@reference"
                )
            ),
            (
                "Dictionary",
                route(SendGuard::Exact(Guard::UntaggedMap), "bt@stdlib@dictionary")
            ),
        ]
    );
}

/// BT-4390: every special selector of a built-in row stays dynamic, naming
/// itself, read from the fixture rather than a copied list. Among the rows a
/// typed send can route (the supervisor rows' `isAlive`/`stop` belong to
/// the supervisor family, which never routes), only `String` and `Pid` have
/// any, and both are routed for every other selector.
#[test]
fn special_selectors_stay_dynamic_one_by_one() {
    use super::{NoBuiltinRoute, builtin_send_route, is_non_tagged_map_class};
    let routable = || {
        table::ROUTING_ROWS
            .iter()
            .filter(|row| is_non_tagged_map_class(row.class))
    };
    let with_specials: Vec<&str> = routable()
        .filter(|row| !row.special_selectors.is_empty())
        .map(|row| row.class)
        .collect();
    assert_eq!(with_specials, vec!["String", "Pid"]);
    for row in routable() {
        for special in row.special_selectors {
            assert_eq!(
                builtin_send_route(row.class, special),
                Some(Err(NoBuiltinRoute::SpecialSelector(special))),
                "{}>>{special} must stay dynamic",
                row.class
            );
        }
    }
    // Another row's special selector is ordinary here: `await` on a String
    // reaches bt@stdlib@string like any other selector.
    assert!(matches!(builtin_send_route("String", "await"), Some(Ok(_))));
    assert!(matches!(builtin_send_route("Pid", "asBase64"), Some(Ok(_))));
}

/// BT-4294: why each built-in row without a direct route stays dynamic.
#[test]
fn builtins_without_a_send_route_say_why() {
    use super::{NoBuiltinRoute, builtin_send_route};
    for class in [
        "Boolean",
        "Bitstring",
        "Character",
        "Class",
        "Metaclass",
        "ErlangModule",
    ] {
        assert!(
            matches!(
                builtin_send_route(class, "frobnicate"),
                Some(Err(NoBuiltinRoute::DynamicOnly(_)))
            ),
            "{class} must stay dynamic for its dynamic_only reason"
        );
    }
    assert_eq!(builtin_send_route("Point", "frobnicate"), None);
}

/// BT-4510: a `Binary` send of one of `String`'s special selectors keeps the
/// bare-binary route but has no `String` half: the dynamic path answers that
/// selector for a `String` outside `bt@stdlib@string`, so a `String` value
/// takes the dynamic send.
#[test]
fn binary_route_drops_the_string_half_for_string_special_selectors() {
    use super::{SendGuard, builtin_send_route};
    let string = table::ROUTING_ROWS
        .iter()
        .find(|row| row.class == "String")
        .expect("the fixture has a String row");
    for special in string.special_selectors {
        let Some(Ok(route)) = builtin_send_route("Binary", special) else {
            panic!("Binary {special} must keep its bare-binary route");
        };
        assert_eq!(route.guard, SendGuard::BareBinary);
        assert_eq!(route.string_tag_route, None, "Binary {special}");
    }
    let Some(Ok(route)) = builtin_send_route("Binary", "size") else {
        panic!("Binary size must be routed");
    };
    assert!(route.string_tag_route.is_some());
}
