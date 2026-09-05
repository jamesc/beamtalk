// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to generate embedded metadata from runtime sources.
//!
//! This ensures Rust stays in sync with runtime sources, eliminating
//! hand-maintained lists that can drift:
//! - `is_known_stdlib_type()` from `lib/*.bt` (BT-422)
//!
//! BT-3362 (ADR 0117 Decision step 5): relocated here from
//! `beamtalk-core/build.rs` — `STDLIB_CLASS_NAMES` is `include!`d by
//! `core_erlang::value_type_codegen`, which moved into this crate along with
//! the rest of `codegen`; `include!`'s `OUT_DIR` is always the *current*
//! crate's build-script output directory, so the generator had to move with
//! it. `beamtalk-core/build.rs` keeps only the `BEAMTALK_SPEC_MAPPING_STAMP`
//! emission, unrelated to codegen.

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
    collect_stdlib_class_names(lib_dir, &mut class_names);
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

/// Collect stdlib class names from `dir`, recursing into subdirectories.
///
/// `stdlib/src/` may be grouped into subdirectories (`collections/`, …).
/// Those are editorial only — module naming ignores them regardless of
/// depth, matching `build_stdlib::module_name_from_path`. Recursing keeps
/// `is_known_stdlib_type()` from silently missing nested classes.
///
/// Symlinks are skipped, matching `FileWalker`'s default (the walker
/// `build_stdlib` uses over the same tree). Beyond keeping the two in sync,
/// it stops a symlinked directory cycle from recursing until the build script
/// blows its stack.
///
/// The name is parsed from each file's actual `subclass:`/`Protocol define:`
/// declaration (BT-3432), not assumed from the file stem: a file/name
/// mismatch — same bug BT-3431 fixed for self-dispatch codegen — previously
/// produced a `STDLIB_CLASS_NAMES` entry that could never match the real,
/// AST-derived name `is_known_stdlib_type` looks up, silently sending that
/// class's (or protocol's — both compile to their own `bt@stdlib@{snake}`
/// module, see `protocol_modules` in the generated `.app.src`) module
/// references through the non-stdlib `bt@{snake}` fallback instead of
/// `bt@stdlib@{snake}`. Files with zero or multiple classes/protocols
/// (malformed) are skipped — `beamtalk build-stdlib`'s own
/// single-definition-per-file validation is the authority on rejecting those.
fn collect_stdlib_class_names(dir: &Path, out: &mut Vec<String>) {
    let entries = fs::read_dir(dir).expect("Failed to read stdlib source directory");
    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        // `file_type()` does not traverse symlinks, unlike `Path::is_dir()`.
        let file_type = entry.file_type().expect("Failed to read file type");
        if file_type.is_symlink() {
            continue;
        }
        let path = entry.path();
        if file_type.is_dir() {
            collect_stdlib_class_names(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "bt") {
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Failed to read '{}': {e}", path.display()));
            let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
            let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
            if let [class] = module.classes.as_slice() {
                out.push(class.name.name.to_string());
            } else if let [protocol] = module.protocols.as_slice() {
                out.push(protocol.name.name.to_string());
            }
        }
    }
}
