// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pattern-matching and destructuring code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Submodules organize the code by domain:
//! - [`match_lowering`] — `match:` expression compilation (`generate_match`
//!   and its native/chain strategies), native `Pattern` → Core Erlang
//!   pattern lowering, constructor/binary-segment patterns, and guards
//! - [`type_tests`] — the `Pattern::Type` (`binding :: ClassName`) runtime
//!   test strategies `generate_match`'s chain dispatches to (ADR 0107
//!   Phase A)
//! - [`optional_map_keys`] — `match:` arms whose map pattern has an
//!   optional key (ADR 0155)
//! - [`destructure`] — the general-purpose destructuring-assignment
//!   extraction helpers shared by block bodies, loop/conditional/exception
//!   bodies, and the REPL (`beamtalk-repl`)
//!
//! Note: block (closure) compilation lives in [`super::blocks`]; other
//! expression code generation stays in [`super::expressions`].

mod array_patterns;
pub(in crate::core_erlang) mod binary_patterns;
mod destructure;
pub(in crate::core_erlang) mod match_lowering;
mod optional_map_keys;
mod type_tests;

pub(super) use match_lowering::MatchThreading;
pub(in crate::core_erlang) use type_tests::guard_boolean_test;

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{MapPatternDefault, MapPatternKey, MapPatternPair};

/// Generates a Core Erlang document for a map pattern key.
///
/// Symbol keys emit an atom: `'key'`.
/// String keys emit a tagged `String` literal via the Document pipeline
/// (BT-4170) — matching how `String`-keyed maps are constructed.
///
/// Shared by `match_lowering::generate_pattern`'s `Pattern::Map` case and
/// `destructure::generate_pattern_extractions_from_var`'s `Pattern::Map`
/// case (AGENTS.md's no-duplicate-implementations rule) — a plain free
/// function (not a `CoreErlangGenerator` associated function) since neither
/// caller needs generator state to compute it.
fn map_pattern_key_doc(key: &MapPatternKey) -> Document<'static> {
    match key {
        MapPatternKey::Symbol(s) => leaf::atom(s.as_str()),
        MapPatternKey::StringLit(s) => leaf::tagged_string_lit(s.as_str()),
    }
}

/// The Core Erlang constant an optional map-pattern key binds when it is
/// absent (ADR 0155): its default, or `'nil'` when it has none. Shared by
/// the `match:` arm and destructure lowerings.
fn map_pattern_default_doc(pair: &MapPatternPair) -> Document<'static> {
    match pair.default.as_ref().map(|(d, _)| d) {
        None | Some(MapPatternDefault::Nil) => leaf::atom("nil"),
        Some(MapPatternDefault::True) => leaf::atom("true"),
        Some(MapPatternDefault::False) => leaf::atom("false"),
        Some(MapPatternDefault::EmptyMap) => Document::Str("~{}~"),
        Some(MapPatternDefault::Literal(lit)) => super::CoreErlangGenerator::literal_doc(lit),
    }
}

#[cfg(test)]
mod tests;
