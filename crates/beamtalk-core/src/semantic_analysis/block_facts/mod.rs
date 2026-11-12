// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! This domain service analyzes blocks to detect which variables and fields are
//! read/written, enabling proper state threading in tail-recursive loops.
//!
//! The analyses live in sibling modules and are re-exported here, so the public
//! path of every item is `semantic_analysis::block_facts::<name>`:
//!
//! - `mutation`: [`BlockMutationAnalysis`], class-variable accesses and the
//!   `while` condition predicates.
//! - `escape`: how a block literal leaves a class-method invocation.
//! - `outer_writes`: outer-local writes of a block or statement.
//! - `producers`: local-threading producer classification (ADR 0131).
//! - `sequencing`: operand sequencing and unsequenced operands.
//! - `helpers` / `value_position`: shared helpers and value-position
//!   assignments.

mod escape;
mod helpers;
mod mutation;
mod outer_writes;
mod producers;
mod sequencing;
mod value_position;

pub use escape::*;
pub use helpers::*;
pub use mutation::*;
pub use outer_writes::*;
pub use producers::*;
pub use sequencing::*;
pub use value_position::*;

#[cfg(test)]
mod tests;
