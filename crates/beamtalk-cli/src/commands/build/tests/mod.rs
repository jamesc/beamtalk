// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for the `build` command.
//!
//! Tests are organized into feature-focused sub-modules:
//! - [`stub_registry`] — stub-registry resolution precedence and collisions
//!   (ADR 0075 Phase 2)
//! - [`build_basics`] — source discovery and basic single/multi-file builds
//! - [`module_naming`] — `compute_relative_module` and manifest-driven
//!   package/module naming, including declared-alias metadata
//! - [`cross_file_resolution`] — the class-module index and cross-file
//!   class/alias resolution
//! - [`incremental_build`] — stale-artifact cleanup and `detect_changes`
//! - [`native_erlang_build`] — ADR 0072 Phase 1 native Erlang compilation
//!   and native-module collision detection
//! - [`rebar3_integration`] — ADR 0072 Phase 2 rebar3 config generation and
//!   package-corpus generation
//! - [`native_deps_and_headers`] — transitive native-dependency detection
//!   and class-header generation
//! - [`native_erlang_compile`] — `compile_native_erlang_with_deps` and
//!   `collect_erl_files`

pub use super::*;
pub(crate) use std::fs;
pub(crate) use tempfile::TempDir;

pub(crate) fn create_test_project(temp: &TempDir) -> Utf8PathBuf {
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();
    project_path
}

pub(crate) fn write_test_file(path: &Utf8Path, content: &str) {
    fs::write(path, content).unwrap();
}

pub(crate) fn default_options() -> beamtalk_core::CompilerOptions {
    beamtalk_core::CompilerOptions::default()
}

mod build_basics;
mod cross_file_resolution;
mod incremental_build;
mod module_naming;
mod native_deps_and_headers;
mod native_erlang_build;
mod native_erlang_compile;
mod rebar3_integration;
mod stub_registry;
