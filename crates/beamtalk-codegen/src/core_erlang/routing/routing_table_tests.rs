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
         use super::{Guard, ReservedTupleTag, Representation, RoutingRow};\n\
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
        let size = tag["size"].as_u64().expect("reserved tag `size` is a number");
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
         /// The root of the exception family, which the runtime routes to\n\
         /// `bt@stdlib@exception:dispatch` instead of the instance's own module.\n\
         pub(in crate::core_erlang) const EXCEPTION_ROOT_CLASS: &str = {:?};",
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
    let checked_in = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
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
            assert!(seen.insert(*name), "class {name:?} appears twice in the routing table");
        }
    }
}

/// The `Tuple` test is rendered per reserved tag size; a tag listed twice at
/// one size would emit a duplicate (unreachable) case clause.
#[test]
fn routing_table_reserved_tags_are_distinct() {
    let mut seen = BTreeSet::new();
    for tag in table::RESERVED_TUPLE_TAGS {
        assert!(seen.insert((tag.tag, tag.size)), "reserved tag {tag:?} listed twice");
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
            assert_eq!(row.guard, None::<Guard>, "{}: supervisor rows carry no guard", row.class);
        }
    }
}
