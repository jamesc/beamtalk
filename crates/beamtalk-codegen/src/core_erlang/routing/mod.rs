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
//!   target (`dispatch_codegen::typed_tagged_target`);
//! - [`reserved_tuple_tags`]: the tuple tags the `Tuple` test excludes;
//! - [`builtin_send_route`]: the guard and module of a typed send's guarded
//!   direct call to a built-in receiver (BT-4294), or why there is none;
//! - [`rewritten_selectors`]: selectors the dynamic entry point rewrites
//!   before it classifies the receiver, which no guarded call may claim;
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

/// Every fixture row, in fixture order.
pub(in crate::core_erlang) fn rows() -> &'static [RoutingRow] {
    table::ROUTING_ROWS
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
/// (BT-4069). Every row except the supervisor family: a supervisor row's
/// representation is shared by every subclass, so the typed-tagged path
/// excludes the whole family by a superclass-chain walk
/// (`dispatch_codegen::supervisor_family_root`) instead, and the rows that
/// remain are exactly the sealed classes the `typed_sealed` hygiene tests
/// require.
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

/// Selectors `beamtalk_message_dispatch:send/3` rewrites before it classifies
/// the receiver (`perform:withArguments:timeout:` becomes `send/4`), so no
/// receiver's `dispatch/3` ever sees them through the dynamic path.
pub(in crate::core_erlang) fn rewritten_selectors() -> &'static [&'static str] {
    table::REWRITTEN_SELECTORS
}

/// The guard a typed send's guarded direct call to a built-in stands behind
/// (BT-4294): a value passes it only if the dynamic path would route it to the
/// row's `target_module` for every selector.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum SendGuard {
    /// The row's own representation test, which is exactly the dynamic
    /// path's clause.
    Exact(Guard),
    /// Any tuple whose first element is none of [`reserved_tuple_tags`], at
    /// **any** size. The dynamic path rejects some reserved tags at more than
    /// one size (`beamtalk_primitive:send/3` hands every tuple of size two or
    /// more headed `beamtalk_object` to `gen_server:call/2`), so the
    /// size-specific type-pattern test would admit tuples it does not route
    /// to `Tuple`. Rejecting a reserved head at every size is slightly
    /// over-conservative, and a miss is always sound.
    TupleWithoutReservedHead,
}

impl SendGuard {
    /// The send guard of a row whose representation test is `guard`, or
    /// `None` when that test admits values the dynamic path routes to more
    /// than one module. An exhaustive match, so a new guard kind must decide.
    fn of(guard: Guard) -> Option<Self> {
        match guard {
            Guard::Bif(_)
            | Guard::StringTag
            | Guard::UnalignedBitstring
            | Guard::Symbol
            | Guard::ExactAtom(_)
            | Guard::UntaggedMap => Some(Self::Exact(guard)),
            Guard::UnreservedTuple => Some(Self::TupleWithoutReservedHead),
            // A bare binary routes to `Binary`, a `{beamtalk_string, _}` to
            // `String`: the subclass-aware test spans two routes.
            Guard::BinaryOrStringTag => None,
        }
    }
}

/// A typed send's guarded direct call to a built-in receiver (BT-4294): when
/// the receiver passes `guard`, the dynamic path would call
/// `target_module:dispatch/3`, so the call site may too.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) struct BuiltinRoute {
    pub(in crate::core_erlang) guard: SendGuard,
    pub(in crate::core_erlang) target_module: &'static str,
}

/// Why a routing-fixture class has no [`BuiltinRoute`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum NoBuiltinRoute {
    /// The row's `dynamic_only` reason.
    DynamicOnly(&'static str),
    /// The row has no representation test or no single target module.
    NoSingleRoute,
    /// The dynamic path answers some selectors outside the target module, so
    /// the module does not speak for the whole protocol.
    SpecialSelectors,
    /// The row's representation test admits values routed to another module
    /// (`Binary`'s test admits a `String`).
    GuardSpansRoutes,
}

/// The guarded direct route of a typed send to an instance of `class`, read
/// from the routing fixture (BT-4294), or `None` when `class` has no row (it
/// is a tagged-map class, `dispatch_codegen::typed_tagged_target`'s concern).
pub(in crate::core_erlang) fn builtin_send_route(
    class: &str,
) -> Option<std::result::Result<BuiltinRoute, NoBuiltinRoute>> {
    let row = row(class)?;
    Some(route_of(row))
}

fn route_of(row: &RoutingRow) -> std::result::Result<BuiltinRoute, NoBuiltinRoute> {
    if let Some(why) = row.dynamic_only {
        return Err(NoBuiltinRoute::DynamicOnly(why));
    }
    let (Some(guard), Some(target_module)) = (row.guard, row.target_module) else {
        return Err(NoBuiltinRoute::NoSingleRoute);
    };
    if !row.special_selectors.is_empty() {
        return Err(NoBuiltinRoute::SpecialSelectors);
    }
    let guard = SendGuard::of(guard).ok_or(NoBuiltinRoute::GuardSpansRoutes)?;
    Ok(BuiltinRoute {
        guard,
        target_module,
    })
}

#[cfg(test)]
mod routing_table_tests;
