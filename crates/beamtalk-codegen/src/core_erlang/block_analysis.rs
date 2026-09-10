// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Re-exports from `semantic_analysis::block_facts`. The analysis logic
//! lives in the semantic analysis layer.

pub use beamtalk_core::semantic_analysis::block_facts::{
    BlockMutationAnalysis, analyze_block, compute_class_var_mutating_selectors,
};
