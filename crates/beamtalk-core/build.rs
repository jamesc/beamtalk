// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to generate embedded metadata from runtime sources.
//!
//! This ensures Rust stays in sync with runtime sources, eliminating
//! hand-maintained lists that can drift:
//! - `is_known_stdlib_type()` from `lib/*.bt` (BT-422)

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");
    let lib_dir = workspace_root.join("stdlib/src");

    assert!(
        lib_dir.exists(),
        "Expected stdlib directory at `{}` — \
         ensure `stdlib/src/` is present at the workspace root.",
        lib_dir.display()
    );

    // Rerun when stdlib/src/ changes
    println!("cargo:rerun-if-changed={}", lib_dir.display());

    // Generate stdlib class names
    generate_stdlib_types(&lib_dir);
}

fn generate_stdlib_types(lib_dir: &Path) {
    let mut class_names: Vec<String> = Vec::new();

    for entry in fs::read_dir(lib_dir).expect("Failed to read lib/ directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "bt") {
            if let Some(stem) = path.file_stem() {
                class_names.push(stem.to_string_lossy().to_string());
            }
        }
    }

    class_names.sort();

    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let dest_path = Path::new(&out_dir).join("stdlib_types.rs");

    let names_literal = class_names
        .iter()
        .map(|n| format!("    \"{n}\""))
        .collect::<Vec<_>>()
        .join(",\n");

    let code = format!(
        "/// Auto-generated from `lib/*.bt` — do not edit manually.\n\
         const STDLIB_CLASS_NAMES: &[&str] = &[\n{names_literal}\n];\n"
    );

    fs::write(dest_path, code).expect("Failed to write stdlib_types.rs");
}
