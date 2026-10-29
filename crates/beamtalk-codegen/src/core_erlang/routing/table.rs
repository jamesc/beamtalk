// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! GENERATED from
//! `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`
//! by `routing_table_tests.rs` (BT-4292). Do not edit: change the fixture and
//! run `ROUTING_TABLE_BLESS=1 cargo test -p beamtalk-codegen --lib routing_table`.

use super::{Guard, Representation, ReservedTupleTag, RoutingRow};

#[rustfmt::skip]
pub(super) static ROUTING_ROWS: &[RoutingRow] = &[
    RoutingRow { class: "Integer", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_integer")), target_module: Some("bt@stdlib@integer"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Float", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_float")), target_module: Some("bt@stdlib@float"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Character", aliases: &[], representation: Representation::RawTerm, guard: None, target_module: None, special_selectors: &[], dynamic_only: Some("a Character is an integer at run time: the dynamic path routes it to Integer, and Character-typed codegen already claims Character receivers") },
    RoutingRow { class: "String", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::StringTag), target_module: Some("bt@stdlib@string"), special_selectors: &["asBase64", "asBase64Url", "asHex", "asStringUnchecked", "bitSize", "byteAt:", "concat:", "part:size:", "toBytes"], dynamic_only: None },
    RoutingRow { class: "Binary", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::BinaryOrStringTag), target_module: Some("bt@stdlib@binary"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Bitstring", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::UnalignedBitstring), target_module: Some("bt@stdlib@bitstring"), special_selectors: &[], dynamic_only: Some("send_bitstring/3 re-raises a Bitstring does_not_understand with a bit-size hint that bt@stdlib@bitstring:dispatch/3 does not add") },
    RoutingRow { class: "Symbol", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Symbol), target_module: Some("bt@stdlib@symbol"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "True", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::ExactAtom("true")), target_module: Some("bt@stdlib@true"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "False", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::ExactAtom("false")), target_module: Some("bt@stdlib@false"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Boolean", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_boolean")), target_module: None, special_selectors: &[], dynamic_only: Some("true and false dispatch to different modules (True, False)") },
    RoutingRow { class: "UndefinedObject", aliases: &["Nil"], representation: Representation::RawTerm, guard: Some(Guard::ExactAtom("nil")), target_module: Some("bt@stdlib@undefined_object"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Block", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_function")), target_module: Some("bt@stdlib@block"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Tuple", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::UnreservedTuple), target_module: Some("bt@stdlib@tuple"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "List", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_list")), target_module: Some("bt@stdlib@list"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Pid", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_pid")), target_module: Some("bt@stdlib@pid"), special_selectors: &["await", "await:", "awaitForever", "whenRejected:", "whenResolved:"], dynamic_only: None },
    RoutingRow { class: "Port", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_port")), target_module: Some("bt@stdlib@port"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Reference", aliases: &[], representation: Representation::RawTerm, guard: Some(Guard::Bif("is_reference")), target_module: Some("bt@stdlib@reference"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Dictionary", aliases: &[], representation: Representation::UntaggedMap, guard: Some(Guard::UntaggedMap), target_module: Some("bt@stdlib@dictionary"), special_selectors: &[], dynamic_only: None },
    RoutingRow { class: "Class", aliases: &[], representation: Representation::Record, guard: None, target_module: None, special_selectors: &[], dynamic_only: Some("a class object is a #beamtalk_object{} record answered by its class process") },
    RoutingRow { class: "Metaclass", aliases: &[], representation: Representation::Record, guard: None, target_module: None, special_selectors: &[], dynamic_only: Some("a metaclass object is a #beamtalk_object{} record dispatched through the Metaclass chain") },
    RoutingRow { class: "ErlangModule", aliases: &[], representation: Representation::ProxyRouted, guard: None, target_module: Some("beamtalk_erlang_proxy"), special_selectors: &[], dynamic_only: Some("the native proxy is reached through a tagged map whose tag a class-tag guard would have to name; no typed send targets it") },
    RoutingRow { class: "Supervisor", aliases: &[], representation: Representation::SupervisorTuple, guard: None, target_module: None, special_selectors: &["isAlive", "stop"], dynamic_only: Some("a supervisor handle is dispatched by a hierarchy walk in the caller (ADR 0059), not by a module's dispatch/3") },
    RoutingRow { class: "DynamicSupervisor", aliases: &[], representation: Representation::SupervisorTuple, guard: None, target_module: None, special_selectors: &["isAlive", "stop"], dynamic_only: Some("a supervisor handle is dispatched by a hierarchy walk in the caller (ADR 0059), not by a module's dispatch/3") },
];

#[rustfmt::skip]
pub(super) static RESERVED_TUPLE_TAGS: &[ReservedTupleTag] = &[
    ReservedTupleTag { tag: "beamtalk_string", size: 2 },
    ReservedTupleTag { tag: "beamtalk_future", size: 2 },
    ReservedTupleTag { tag: "beamtalk_object", size: 4 },
    ReservedTupleTag { tag: "beamtalk_supervisor", size: 4 },
];

/// The root of the exception family, which the runtime routes to
/// `bt@stdlib@exception:dispatch` instead of the instance's own module.
pub(in crate::core_erlang) const EXCEPTION_ROOT_CLASS: &str = "Exception";
