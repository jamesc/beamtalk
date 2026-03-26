// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency resolution for Beamtalk packages (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! This module handles fetching and resolving package dependencies declared in
//! `beamtalk.toml`. Phase 1 supports:
//! - **Path dependencies:** local filesystem paths (for monorepo/development)
//! - **Git dependencies:** clone repos, check out tag/branch/rev, resolve to exact SHA
//! - **Lockfile:** `beamtalk.lock` pins exact commit SHAs for reproducible builds
//! - **Topological ordering:** compile dependencies in correct order (leaves first)
//! - **Cycle detection:** clear error when circular dependencies are found
//! - **Single-version policy:** error when same package appears at different versions

pub mod cli;
pub mod git;
pub mod graph;
pub mod lockfile;
pub mod path;

// Re-export commonly used items
pub use path::collect_dep_ebin_paths;
