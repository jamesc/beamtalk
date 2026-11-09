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
//!   direct call to a built-in receiver with a given selector (BT-4294,
//!   BT-4390), or why there is none;
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

/// The module the fixture row for `class` names as its `target_module`, if
/// it has one (BT-4411). A class literal may bake this module into its class
/// object only when it is also the class's own compiled module: a stdlib
/// built-in's module can be neither removed nor redefined into another
/// module, and the runtime half of the fixture
/// (`beamtalk_tagged_map_routing_conformance_tests`) checks the live class's
/// module against it. A row whose target is not its class module
/// (`ErlangModule`, routed to `beamtalk_erlang_proxy`) is excluded by that
/// comparison at the call site.
pub(in crate::core_erlang) fn pinned_class_module(class: &str) -> Option<&'static str> {
    row(class).and_then(|row| row.target_module)
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
/// row's `target_module` for every selector outside the row's
/// `special_selectors`.
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
    /// A bare binary: the half of [`Guard::BinaryOrStringTag`] that the
    /// dynamic path routes to the row's own module (the other half, the
    /// `String` tag, is the [`BuiltinRoute::string_tag_route`], BT-4510).
    BareBinary,
}

impl SendGuard {
    /// The send guard of a row whose representation test is `guard`. An
    /// exhaustive match, so a new guard kind must decide.
    fn of(guard: Guard) -> Self {
        match guard {
            Guard::Bif(_)
            | Guard::StringTag
            | Guard::UnalignedBitstring
            | Guard::Symbol
            | Guard::ExactAtom(_)
            | Guard::UntaggedMap => Self::Exact(guard),
            Guard::UnreservedTuple => Self::TupleWithoutReservedHead,
            // A bare binary routes to `Binary`, a `{beamtalk_string, _}` to
            // `String`: the subclass-aware test spans two routes, so the
            // row's own route guards on the bare-binary half and
            // `route_of` adds the `String` half as a second route (BT-4510).
            Guard::BinaryOrStringTag => Self::BareBinary,
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
    /// BT-4510: for a row whose representation test also admits the
    /// `String` tag (`Binary`), the `String` row's own route for the same
    /// selector, tried when `guard` misses — `None` when the selector is one
    /// of `String`'s special selectors (the dynamic path answers those
    /// elsewhere, so a `String` value takes the dynamic send) and for every
    /// other row.
    pub(in crate::core_erlang) string_tag_route: Option<StringTagRoute>,
}

/// The `String` half of a [`Guard::BinaryOrStringTag`] row's route
/// (BT-4510): a value passing `guard` (the `String` row's representation
/// test) is routed by the dynamic path to `target_module`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) struct StringTagRoute {
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
    /// The send's selector is one of the row's `special_selectors`, which
    /// the dynamic path answers without the target module (BT-4390): the
    /// `Pid` row's future protocol (`beamtalk_future`), or the `String`
    /// row's byte-level selectors shared with `Binary`, which go to
    /// `bt@stdlib@binary` unless a `String` extension overrides them.
    SpecialSelector(&'static str),
}

/// The guarded direct route of a typed send of `selector` to an instance of
/// `class`, read from the routing fixture (BT-4294), or `None` when `class`
/// has no row (it is a tagged-map class,
/// `dispatch_codegen::typed_tagged_target`'s concern).
///
/// Selector-aware (BT-4390): a row with `special_selectors` routes every
/// *other* selector to its `target_module`, because the dynamic path only
/// diverts the listed ones (`beamtalk_primitive:send_pid/3`'s future
/// clauses, `module_for_value/2`'s `String`/`Binary` shared-selector fast
/// path) and sends the rest to `target_module:dispatch/3`.
///
/// A `String` extension (ADR 0066) does not move a non-special selector:
/// the runtime's `beamtalk_extensions:has('String', Sel)` check sits only
/// inside the shared-selector fast path, so it can only move a *special*
/// selector between `bt@stdlib@binary` and `bt@stdlib@string`. Every other
/// `String` selector reaches `bt@stdlib@string:dispatch/3` whatever the
/// extension registry holds, and that function consults the registry
/// itself (its `beamtalk_extensions:lookup` fallthrough arm), so an
/// extension added or replaced at run time is still honoured by the direct
/// call.
pub(in crate::core_erlang) fn builtin_send_route(
    class: &str,
    selector: &str,
) -> Option<std::result::Result<BuiltinRoute, NoBuiltinRoute>> {
    let row = row(class)?;
    Some(route_of(row, selector))
}

fn route_of(row: &RoutingRow, selector: &str) -> std::result::Result<BuiltinRoute, NoBuiltinRoute> {
    if let Some(why) = row.dynamic_only {
        return Err(NoBuiltinRoute::DynamicOnly(why));
    }
    let (Some(guard), Some(target_module)) = (row.guard, row.target_module) else {
        return Err(NoBuiltinRoute::NoSingleRoute);
    };
    if let Some(special) = row
        .special_selectors
        .iter()
        .find(|special| **special == selector)
    {
        return Err(NoBuiltinRoute::SpecialSelector(special));
    }
    let string_tag_route = if guard == Guard::BinaryOrStringTag {
        string_tag_route(selector)
    } else {
        None
    };
    let guard = SendGuard::of(guard);
    Ok(BuiltinRoute {
        guard,
        target_module,
        string_tag_route,
    })
}

/// The route of `selector` for the row whose representation test is exactly
/// the `String` tag ([`Guard::StringTag`]), the values a
/// [`Guard::BinaryOrStringTag`] test admits beyond a bare binary (BT-4510),
/// or `None` when that row has no route for the selector (a special
/// selector) or no such row exists.
fn string_tag_route(selector: &str) -> Option<StringTagRoute> {
    let row = rows()
        .iter()
        .find(|row| row.guard == Some(Guard::StringTag))?;
    let route = route_of(row, selector).ok()?;
    Some(StringTagRoute {
        guard: route.guard,
        target_module: route.target_module,
    })
}

#[cfg(test)]
mod routing_table_tests;
