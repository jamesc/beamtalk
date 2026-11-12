// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file consulted-surface recording (BT-4586, phase 1): what each file's
//! semantic analysis and codegen actually looked up in the cross-file surface.

use super::*;
use beamtalk_core::semantic_analysis::consulted::ConsultedSurface;

/// Compile `files` (path, source) in one package the way Pass 2 does, and
/// return the surface the file named `target` consulted.
fn consulted_by(files: &[(&str, &str)], target: &str) -> ConsultedSurface {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();
    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();
    let mut source_files = Vec::new();
    for (name, source) in files {
        write_test_file(&src_path.join(name), source);
        source_files.push(src_path.join(name));
    }
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "app").unwrap();
    let stem = target.trim_end_matches(".bt");
    let (_diags, surface) = compile_file_with_surface(
        &src_path.join(target),
        &format!("bt@app@{stem}"),
        &build_dir.join(format!("bt@app@{stem}.core")),
        &default_options(),
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .unwrap_or_else(|e| panic!("{e:?}"));
    surface
}

/// A file that calls a method on a class defined in another file consults that class.
#[test]
fn file_records_the_cross_file_class_it_calls_into() {
    let files = [
        (
            "helper.bt",
            "Value subclass: Helper\n  greet -> Integer => 1\n",
        ),
        (
            "main.bt",
            "Object subclass: Main\n  run -> Integer => Helper new greet\n",
        ),
        (
            "lone.bt",
            "Object subclass: Lone\n  value -> Integer => 1\n",
        ),
    ];
    let main = consulted_by(&files, "main.bt");
    assert!(main.names.contains("Helper"), "{main:?}");
    assert!(!main.is_empty());

    // A whole-package compile also reads every pre-loaded name while building
    // the file's hierarchy and scans the hierarchy in codegen, so a lone file's
    // surface is not empty here: phase 2 decides how to narrow those bulk reads.
    // The empty-surface and scan cases are pinned at the store level in
    // `semantic_analysis::consulted`.
    let lone = consulted_by(&files, "lone.bt");
    assert!(lone.names.contains("Lone"), "{lone:?}");
}

/// Recording never leaks past the per-file scope: after a compile the thread
/// records nothing, so the REPL and LSP (which install no scope) are unaffected.
#[test]
fn recorder_is_not_left_installed_after_a_file_compiles() {
    let files = [("a.bt", "Object subclass: A\n  value -> Integer => 1\n")];
    let _ = consulted_by(&files, "a.bt");
    assert!(
        !beamtalk_core::semantic_analysis::consulted::is_recording(),
        "a scope was left installed after the compile"
    );
}
