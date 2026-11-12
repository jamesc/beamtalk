// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the stdlib build, split to mirror the module layout.
//!
//! **DDD Context:** CLI / Build System

mod cross_file_resolution;
mod generators;
mod metadata;
mod orchestrate;

use super::metadata::*;
use beamtalk_core::semantic_analysis::class_hierarchy::*;
use camino::{Utf8Path, Utf8PathBuf};
use std::fs;
use std::time::SystemTime;
use tempfile::TempDir;

pub(super) fn temp_utf8_dir() -> (TempDir, Utf8PathBuf) {
    let temp = TempDir::new().unwrap();
    let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    (temp, dir)
}

pub(super) fn sample_class_meta() -> ClassMeta {
    ClassMeta {
        module_name: "bt@stdlib@counter".to_string(),
        class_name: "Counter".to_string(),
        superclass_name: "Actor".to_string(),
        modifiers: ClassModifiers::default(),
        class_kind: beamtalk_core::ast::ClassKind::Actor,
        state: vec!["count".to_string()],
        state_types: vec![],
        state_has_default: vec![],
        state_kinds: vec![],
        initialize_assigns: vec![],
        has_dynamic_field_writer: false,
        methods: vec![
            MethodMeta {
                selector: "increment".to_string(),
                arity: 0,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
                primitive: None,
                constant_value: None,
            },
            MethodMeta {
                selector: "add:".to_string(),
                arity: 1,
                kind: MethodKindMeta::Primary,
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::simple("Counter")),
                param_types: vec![Some(DeclaredType::simple("Integer"))],
                doc: None,
                primitive: None,
                constant_value: None,
            },
        ],
        class_methods: vec![MethodMeta {
            selector: "default".to_string(),
            arity: 0,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::simple("Counter")),
            param_types: vec![],
            doc: None,
            primitive: None,
            constant_value: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        handle_scope: None,
        auto_kw: None,
        is_value_subclass: false,
        in_caller_new1: false,
    }
}

// --- is_stdlib_up_to_date / oldest_mtime_in_dir ---
//
// `is_stdlib_up_to_date` also checks the real compiler binary
// (`std::env::current_exe`), which isn't injectable, so the
// compiler-binary-newer branch is exercised deterministically via a
// beam/source mtime far enough in the past that any real compiler binary
// is newer.
//
// The runtime-`.beam`-newer check takes the runtime ebin directory list
// as a parameter, rather than calling
// `beamtalk_cli::repl_startup::find_runtime_dir_with_layout()` directly,
// so tests have a seam to point it at a synthetic directory (real
// discovery lives in `discover_runtime_ebin_dirs`, called only from the
// real `build_stdlib()` call site) — see the `runtime_ebin` tests below
// for both the "newer" (rebuild) and "older" (no rebuild) branches, plus
// the "discovery failed" (`None`) branch.

/// Sets a file's mtime directly — lets these tests control the
/// before/after ordering `is_stdlib_up_to_date` compares, without
/// depending on real wall-clock timing between writes.
pub(super) fn set_mtime(path: &Utf8Path, time: SystemTime) {
    // Windows' `SetFileTime` needs a handle opened with write access;
    // a read-only `File::open` handle gets `PermissionDenied` (code 5).
    fs::OpenOptions::new()
        .write(true)
        .open(path.as_std_path())
        .unwrap()
        .set_modified(time)
        .unwrap();
}
