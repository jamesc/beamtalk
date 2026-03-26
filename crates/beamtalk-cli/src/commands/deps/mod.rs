// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency resolution for Beamtalk packages (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! This module handles fetching and resolving package dependencies declared in
//! `beamtalk.toml`. Phase 1 supports:
//! - **Git dependencies:** clone repos, check out tag/branch/rev, resolve to exact SHA
//! - **Lockfile:** `beamtalk.lock` pins exact commit SHAs for reproducible builds

pub mod git;
pub mod lockfile;
