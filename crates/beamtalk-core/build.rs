// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to generate embedded metadata from runtime sources.
//!
//! This ensures Rust stays in sync with runtime sources, eliminating
//! hand-maintained lists that can drift:
//! - `is_known_stdlib_type()` from `lib/*.bt` (BT-422)
//! - `is_builtin_class_method()` from `beamtalk_class_dispatch.erl` (BT-722)

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");
    let lib_dir = workspace_root.join("lib");

    assert!(
        lib_dir.exists(),
        "Expected stdlib directory at `{}` — \
         ensure `lib/` is present at the workspace root.",
        lib_dir.display()
    );

    // Rerun when lib/ or runtime sources change
    println!("cargo:rerun-if-changed={}", lib_dir.display());
    let dispatch_erl =
        workspace_root.join("runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl");
    println!("cargo:rerun-if-changed={}", dispatch_erl.display());

    // Generate stdlib class names
    generate_stdlib_types(&lib_dir);

    // Generate builtin class methods
    generate_builtin_class_methods(&dispatch_erl);
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

fn generate_builtin_class_methods(dispatch_erl: &Path) {
    let content =
        fs::read_to_string(dispatch_erl).expect("Failed to read beamtalk_class_dispatch.erl");

    // Extract all selector atoms from class_send clauses.
    // Pattern: class_send(..., 'selector', [...]) ->
    // We match the second parameter (after the opening paren and first param).
    let mut methods = Vec::new();

    for line in content.lines() {
        if line.contains("class_send(") {
            // Find the class_send call and extract the selector
            if let Some(paren_start) = line.find("class_send(") {
                let rest = &line[paren_start + "class_send(".len()..];

                // Skip first parameter (whatever it is: ClassPid, _ClassPid, etc.)
                if let Some(comma_pos) = rest.find(',') {
                    let after_first = &rest[comma_pos + 1..].trim_start();

                    // Now extract the selector atom
                    if let Some(stripped) = after_first.strip_prefix('\'') {
                        // Quoted: 'new:' or 'printString'
                        if let Some(quote_end) = stripped.find('\'') {
                            let method = stripped[..quote_end].to_string();
                            methods.push(method);
                        }
                    } else if let Some(end) = after_first.find([',', ')', ' ']) {
                        // Unquoted: spawn, methods, superclass, class, etc.
                        let method = after_first[..end].trim();
                        // Filter valid method names
                        if !method.is_empty()
                            && method.chars().all(|c| c.is_alphanumeric() || c == '_')
                            && method
                                .chars()
                                .next()
                                .is_some_and(|c| c.is_lowercase() || c == '_')
                        {
                            methods.push(method.to_string());
                        }
                    }
                }
            }
        }
    }

    // Remove duplicates and sort
    methods.sort();
    methods.dedup();

    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let dest_path = Path::new(&out_dir).join("builtin_class_methods.rs");

    let methods_literal = methods
        .iter()
        .map(|m| format!("            \"{m}\""))
        .collect::<Vec<_>>()
        .join(" |\n");

    // Guard against empty extraction which would produce invalid Rust like
    // `matches!(selector, )`. Fail fast with a clear message so the build
    // author knows to check the Erlang source.
    assert!(
        !methods.is_empty(),
        "No builtin class methods found in {} — ensure class_send() calls exist in the source.",
        dispatch_erl.display()
    );

    let code = format!(
        "// Auto-generated from beamtalk_class_dispatch.erl — do not edit manually (BT-722).\n\
         matches!(\n\
             selector,\n\
             {methods_literal}\n\
         )"
    );

    fs::write(dest_path, code).expect("Failed to write builtin_class_methods.rs");
}
