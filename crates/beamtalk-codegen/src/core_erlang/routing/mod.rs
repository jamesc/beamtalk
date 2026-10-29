// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Representation routing: how dynamic dispatch reaches the instances of
//! every class that is **not** a `'$beamtalk_class'`-tagged map of its own
//! name (BT-4200, BT-4292).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! The facts live in one place, the shared fixture
//! `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`.
//! [`table`] is generated from it (never edit it by hand), and the runtime's
//! `beamtalk_tagged_map_routing_conformance_tests` checks the same file
//! against `beamtalk_primitive`'s real routing. This module holds the types
//! the generated table is written in and the queries codegen asks of it:
//!
//! - [`type_test`]: the representation test of a type pattern
//!   (`x :: Integer`, `patterns::type_tests`), or the guard a typed send may
//!   stand behind;
//! - [`is_non_tagged_map_class`]: the classes a class-tag guard must never
//!   target (`dispatch_codegen::typed_sealed_value_target`);
//! - [`reserved_tuple_tags`]: the tuple tags the `Tuple` test excludes;
//! - [`row`]: a class's whole row, for the dynamic-site report's reason.
//!
//! To change a fact, edit the fixture and regenerate the table with
//! `ROUTING_TABLE_BLESS=1 cargo test -p beamtalk-codegen --lib routing_table`;
//! `routing_table_is_generated_from_fixture` fails until the two agree.

mod table;

pub(in crate::core_erlang) use table::EXCEPTION_ROOT_CLASS;

/// How a class's instances are represented, which decides the dynamic path
/// that reaches them (the fixture's `representation`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum Representation {
    /// A plain Erlang term, routed by `beamtalk_primitive:module_for_value`.
    RawTerm,
    /// A map with no class tag: `Dictionary` by convention.
    UntaggedMap,
    /// A `#beamtalk_object{}` class object.
    Record,
    /// A tagged map routed to a fixed native module
    /// (`beamtalk_primitive:proxy_routes/0`).
    ProxyRouted,
    /// A `{beamtalk_supervisor, Class, Module, Pid}` handle, shared by the
    /// class and every subclass.
    SupervisorTuple,
}

/// A representation test (the fixture's `guard`): what proves a value is an
/// instance of the row's class. Each kind has one Core Erlang renderer in
/// `patterns::type_tests`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum Guard {
    /// A guard-safe boolean-returning BIF (`is_integer`, `is_list`, …).
    Bif(&'static str),
    /// Exactly the `{'beamtalk_string', Bytes}` 2-tuple (BT-4173).
    StringTag,
    /// A bare binary or the `String` tag: `Binary` is sealed and `String`
    /// is its only subclass (BT-4228).
    BinaryOrStringTag,
    /// `is_bitstring` and not `is_binary` (ADR 0141).
    UnalignedBitstring,
    /// A tuple whose first element is not one of [`reserved_tuple_tags`] of
    /// its size.
    UnreservedTuple,
    /// An atom other than `true`/`false`/`nil`.
    Symbol,
    /// Exactly the one atom.
    ExactAtom(&'static str),
    /// A map with no `'$beamtalk_class'` and no `'__class_mod__'` key: the
    /// two keys `beamtalk_tagged_map:class_of/1` reads a class from.
    UntaggedMap,
}

/// One fixture row.
#[derive(Debug)]
pub(in crate::core_erlang) struct RoutingRow {
    pub(in crate::core_erlang) class: &'static str,
    /// Other names of the same class (`Nil` for `UndefinedObject`); a type
    /// pattern may name any of them.
    pub(in crate::core_erlang) aliases: &'static [&'static str],
    pub(in crate::core_erlang) representation: Representation,
    pub(in crate::core_erlang) guard: Option<Guard>,
    /// The module whose `dispatch/3` the dynamic path calls for an
    /// instance, for every selector outside `special_selectors`.
    pub(in crate::core_erlang) target_module: Option<&'static str>,
    /// Selectors the dynamic path answers without `target_module`.
    pub(in crate::core_erlang) special_selectors: &'static [&'static str],
    /// Why a send to this class must stay dynamic although it has a guard
    /// and a target module, if it must.
    pub(in crate::core_erlang) dynamic_only: Option<&'static str>,
}

/// A reserved tuple tag: a tuple of this size whose first element is this
/// atom is never a `Tuple`.
#[derive(Debug)]
pub(in crate::core_erlang) struct ReservedTupleTag {
    pub(in crate::core_erlang) tag: &'static str,
    pub(in crate::core_erlang) size: u32,
}

/// The fixture row for `class`, if it has one.
pub(in crate::core_erlang) fn row(class: &str) -> Option<&'static RoutingRow> {
    table::ROUTING_ROWS.iter().find(|row| row.class == class)
}

/// The representation test of a type pattern naming `class` (or one of its
/// aliases), when that test depends on the name alone. `None` for every
/// other class: an actor, a supervisor subclass or a tagged class gets the
/// hierarchy-dependent default in `patterns::type_tests`.
pub(in crate::core_erlang) fn type_test(class: &str) -> Option<Guard> {
    table::ROUTING_ROWS
        .iter()
        .find(|row| row.class == class || row.aliases.contains(&class))
        .and_then(|row| row.guard)
}

/// Whether instances of `class` are never `'$beamtalk_class'`-tagged maps
/// carrying their own name, so a class-tag guard on them could never match
/// (BT-4069). Every row except the supervisor family: those classes are open
/// and abstract, so the typed-sealed path never considers them, and the rows
/// that remain are exactly the sealed classes the `typed_sealed` hygiene
/// tests require.
pub(in crate::core_erlang) fn is_non_tagged_map_class(class: &str) -> bool {
    row(class).is_some_and(|row| row.representation != Representation::SupervisorTuple)
}

/// The classes [`is_non_tagged_map_class`] answers `true` for.
#[cfg(test)]
pub(in crate::core_erlang) fn non_tagged_map_classes() -> impl Iterator<Item = &'static str> {
    table::ROUTING_ROWS
        .iter()
        .filter(|row| row.representation != Representation::SupervisorTuple)
        .map(|row| row.class)
}

/// The reserved tuple tags, in fixture order.
pub(in crate::core_erlang) fn reserved_tuple_tags() -> &'static [ReservedTupleTag] {
    table::RESERVED_TUPLE_TAGS
}

#[cfg(test)]
mod routing_table_tests;
