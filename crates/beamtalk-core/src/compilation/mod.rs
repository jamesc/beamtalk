// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compilation passes for Beamtalk.
//!
//! **DDD Context:** Compilation
//!
//! This module contains project-wide compilation passes that operate across
//! multiple source files. Individual file analysis lives in `semantic_analysis`;
//! these passes coordinate cross-file concerns like extension method indexing
//! and conflict detection.

pub mod dependency;
pub mod extension_conflicts;
pub mod extension_index;

pub use dependency::{DependencyMap, DependencySource, DependencySpec, GitReference};
pub use extension_conflicts::{
    ExtensionConflict, conflict_diagnostics, detect_extension_conflicts, shadowing_diagnostics,
};
pub use extension_index::{
    ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    collect_extensions,
};
