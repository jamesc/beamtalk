// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compilation passes for Beamtalk.
//!
//! **DDD Context:** Compilation
//!
//! This module contains project-wide compilation passes that operate across
//! multiple source files. Individual file analysis lives in `semantic_analysis`;
//! these passes coordinate cross-file concerns like dependency ordering and the
//! same-file extension fold.

pub mod dep_graph;
pub mod dep_order;
pub mod dependency;
pub mod diagnostics_policy;
pub mod ffi_stubs;
pub mod same_file_fold;

pub use dependency::{DependencyMap, DependencySource, DependencySpec, GitReference};
pub use diagnostics_policy::{
    DiagnosticSeverityOverride, DiagnosticsTable, DiagnosticsTableError, apply_diagnostics_table,
    apply_expect_directives, compute_diagnostics, compute_diagnostics_with_known_vars,
    load_diagnostics_table_for_root, parse_diagnostics_table,
    parse_diagnostics_table_from_manifest_toml, parse_package_name_from_manifest_toml,
};
pub use same_file_fold::{
    declared_class_names, fold_same_file_extensions, is_same_file_extension,
    reject_cross_file_extensions,
};
