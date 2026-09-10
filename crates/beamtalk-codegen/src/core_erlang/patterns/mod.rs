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
//! - [`destructure`] — the general-purpose destructuring-assignment
//!   extraction helpers shared by block bodies, loop/conditional/exception
//!   bodies, and the REPL (`beamtalk-repl`)
//!
//! Note: block (closure) compilation lives in [`super::blocks`]; other
//! expression code generation stays in [`super::expressions`].

mod destructure;
mod match_lowering;
mod type_tests;

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::MapPatternKey;

/// Generates a Core Erlang document for a map pattern key.
///
/// Symbol keys emit an atom: `'key'`.
/// String keys emit a Core Erlang binary literal via the Document pipeline.
///
/// Shared by `match_lowering::generate_pattern`'s `Pattern::Map` case and
/// `destructure::generate_pattern_extractions_from_var`'s `Pattern::Map`
/// case (CLAUDE.md's no-duplicate-implementations rule) — a plain free
/// function (not a `CoreErlangGenerator` associated function) since neither
/// caller needs generator state to compute it.
fn map_pattern_key_doc(key: &MapPatternKey) -> Document<'static> {
    match key {
        MapPatternKey::Symbol(s) => leaf::atom(s.as_str()),
        MapPatternKey::StringLit(s) => leaf::binary_lit(s.as_str()),
    }
}

#[cfg(test)]
mod tests;
