// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Re-exports from `semantic_analysis::block_facts`. The analysis logic
//! now lives in the semantic analysis layer (BT-1288).

pub use crate::semantic_analysis::block_facts::{analyze_block, BlockMutationAnalysis};
