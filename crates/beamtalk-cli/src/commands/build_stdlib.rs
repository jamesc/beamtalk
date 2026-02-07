// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build the Beamtalk standard library.
//!
//! **DDD Context:** CLI / Build System
//!
//! Compiles all `lib/*.bt` files through the normal pipeline with `--stdlib-mode`
//! and outputs `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
//! Supports incremental rebuilds by comparing source and output timestamps.
//!
//! Part of ADR 0007 (Compilable Stdlib with Primitive Injection).

use crate::beam_compiler::{BeamCompiler, compile_source_with_bindings};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::{debug, info, instrument};

/// Default path to stdlib source files (relative to project root).
const STDLIB_SOURCE_DIR: &str = "lib";

/// Default output path for compiled stdlib BEAM files (relative to project root).
const STDLIB_EBIN_DIR: &str = "runtime/apps/beamtalk_stdlib/ebin";

/// Build the standard library.
///
/// Finds all `.bt` files in `lib/`, compiles them with stdlib mode enabled,
/// and writes `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
/// Skips files whose `.beam` output is newer than the `.bt` source.
#[instrument(skip_all)]
pub fn build_stdlib() -> Result<()> {
    info!("Starting stdlib build");

    let lib_dir = Utf8PathBuf::from(STDLIB_SOURCE_DIR);
    let ebin_dir = Utf8PathBuf::from(STDLIB_EBIN_DIR);

    if !lib_dir.exists() {
        miette::bail!("Standard library source directory '{}' not found", lib_dir);
    }

    // Find all .bt files in lib/
    let source_files = find_stdlib_files(&lib_dir)?;

    if source_files.is_empty() {
        println!("No .bt source files found in '{lib_dir}'");
        return Ok(());
    }

    info!(count = source_files.len(), "Found stdlib source files");

    // Create ebin directory
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create ebin directory '{ebin_dir}'"))?;

    // Partition into files that need compilation vs up-to-date
    let (to_compile, skipped) = partition_by_freshness(&source_files, &ebin_dir);

    if to_compile.is_empty() {
        println!(
            "Built 0 stdlib modules ({} skipped, up-to-date)",
            skipped.len()
        );
        // Still regenerate .app file to ensure consistency
        generate_app_file(&ebin_dir, &source_files)?;
        return Ok(());
    }

    println!("Compiling {} stdlib module(s)...", to_compile.len());

    // Create a temporary directory for .core files
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory for Core Erlang files")?;
    let temp_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Compiler options: stdlib mode enabled
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        allow_primitives: false,
    };

    // BT-295 / ADR 0007 Phase 3: Build primitive binding table from ALL stdlib sources.
    // This is used during compilation so that @primitive expressions in method bodies
    // can reference the runtime dispatch modules.
    info!("Building primitive binding table from stdlib sources");
    let bindings =
        beamtalk_core::erlang::primitive_bindings::load_from_directory(lib_dir.as_std_path());
    info!(
        binding_count = bindings.len(),
        "Loaded primitive bindings from stdlib"
    );

    // Compile each .bt file to .core (files are independent, no ordering required)
    let mut core_files = Vec::new();
    for source_file in &to_compile {
        let module_name = module_name_from_path(source_file)?;
        let core_file = temp_path.join(format!("{module_name}.core"));

        compile_stdlib_file(source_file, &module_name, &core_file, &options, &bindings)?;
        core_files.push(core_file);
    }

    // Batch compile .core → .beam into ebin directory
    info!("Compiling Core Erlang to BEAM");
    let compiler = BeamCompiler::new(ebin_dir.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile stdlib Core Erlang to BEAM")?;

    // Generate beamtalk_stdlib.app with all modules (compiled + skipped)
    generate_app_file(&ebin_dir, &source_files)?;

    println!(
        "Built {} stdlib modules ({} skipped, up-to-date)",
        to_compile.len(),
        skipped.len()
    );

    Ok(())
}

/// Find all `.bt` files in the stdlib source directory.
fn find_stdlib_files(lib_dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    for entry in fs::read_dir(lib_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{lib_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", lib_dir))?;

        if path.extension() == Some("bt") {
            files.push(path);
        }
    }

    files.sort();
    Ok(files)
}

/// Partition source files into those that need compilation and those that are up-to-date.
///
/// A file is considered up-to-date if its corresponding `.beam` file exists
/// and has a modification time newer than the source `.bt` file.
fn partition_by_freshness(
    source_files: &[Utf8PathBuf],
    ebin_dir: &Utf8Path,
) -> (Vec<Utf8PathBuf>, Vec<Utf8PathBuf>) {
    let mut to_compile = Vec::new();
    let mut skipped = Vec::new();

    for source in source_files {
        let Ok(module_name) = module_name_from_path(source) else {
            // If we can't determine the module name, compile it and let it fail properly
            to_compile.push(source.clone());
            continue;
        };

        let beam_file = ebin_dir.join(format!("{module_name}.beam"));

        if is_up_to_date(source, &beam_file) {
            debug!("Skipping up-to-date: {}", source);
            skipped.push(source.clone());
        } else {
            to_compile.push(source.clone());
        }
    }

    (to_compile, skipped)
}

/// Check if a `.beam` file is newer than its `.bt` source.
fn is_up_to_date(source: &Utf8Path, beam: &Utf8Path) -> bool {
    let Ok(source_mtime) = fs::metadata(source).and_then(|m| m.modified()) else {
        return false;
    };

    let Ok(beam_mtime) = fs::metadata(beam).and_then(|m| m.modified()) else {
        return false;
    };

    // Treat equal mtimes as stale to avoid missed rebuilds on coarse filesystems.
    beam_mtime > source_mtime
}

/// Extract the module name from a `.bt` file path.
///
/// Converts the file stem to a prefixed, lowercase module name to avoid
/// shadowing Erlang built-in modules on case-insensitive filesystems.
/// E.g., `String.bt` → `bt_stdlib_string`, `SequenceableCollection.bt` → `bt_stdlib_sequenceable_collection`.
fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path
        .file_stem()
        .ok_or_else(|| miette::miette!("File '{}' has no name", path))?;

    // Validate module name
    if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
        miette::bail!(
            "Invalid module name '{}': must contain only alphanumeric characters and underscores",
            stem
        );
    }

    let snake = beamtalk_core::codegen::core_erlang::to_module_name(stem);
    Ok(format!("bt_stdlib_{snake}"))
}

/// Compile a single stdlib `.bt` file to Core Erlang.
fn compile_stdlib_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
) -> Result<()> {
    println!("  Compiling {path}...");
    compile_source_with_bindings(path, module_name, core_file, options, bindings)
}

/// Generate the `beamtalk_stdlib.app` file in the ebin directory.
///
/// Lists all modules derived from the source files.
fn generate_app_file(ebin_dir: &Utf8Path, source_files: &[Utf8PathBuf]) -> Result<()> {
    let module_names: Vec<String> = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<_>>()?;

    let modules_list = module_names
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    let app_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"0.1.0\"}},\n\
         \x20   {{modules, [{modules_list}]}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, beamtalk_runtime]}},\n\
         \x20   {{env, []}}\n\
         ]}}.\n"
    );

    let app_file = ebin_dir.join("beamtalk_stdlib.app");
    fs::write(&app_file, app_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_file}'"))?;

    debug!("Generated {}", app_file);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use filetime::FileTime;
    use std::fs;
    use tempfile::TempDir;

    /// Set a file's mtime to a specific Unix timestamp (seconds).
    fn set_mtime(path: &Utf8Path, secs: i64) {
        let ft = FileTime::from_unix_time(secs, 0);
        filetime::set_file_mtime(path.as_std_path(), ft).unwrap();
    }

    #[test]
    fn test_find_stdlib_files() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(lib_dir.join("Integer.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("String.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("README.md"), "not a bt file").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("Integer.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("String.bt")));
    }

    #[test]
    fn test_find_stdlib_files_sorted() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(lib_dir.join("Zebra.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("Alpha.bt"), "// stub").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].as_str() < files[1].as_str());
    }

    #[test]
    fn test_find_stdlib_files_empty_dir() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert!(files.is_empty());
    }

    #[test]
    fn test_module_name_from_path() {
        let path = Utf8PathBuf::from("lib/Integer.bt");
        assert_eq!(module_name_from_path(&path).unwrap(), "bt_stdlib_integer");
    }

    #[test]
    fn test_module_name_from_path_lowercase() {
        let path = Utf8PathBuf::from("lib/beamtalk.bt");
        assert_eq!(module_name_from_path(&path).unwrap(), "bt_stdlib_beamtalk");
    }

    #[test]
    fn test_module_name_from_path_multi_word() {
        let path = Utf8PathBuf::from("lib/ProtoObject.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt_stdlib_proto_object"
        );
    }

    #[test]
    fn test_module_name_from_path_invalid() {
        let path = Utf8PathBuf::from("lib/my-module.bt");
        assert!(module_name_from_path(&path).is_err());
    }

    #[test]
    fn test_is_up_to_date_no_beam() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = lib_dir.join("test.bt");
        let beam = lib_dir.join("test.beam");

        fs::write(&source, "// test").unwrap();

        assert!(!is_up_to_date(&source, &beam));
    }

    #[test]
    fn test_is_up_to_date_beam_newer() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = lib_dir.join("test.bt");
        let beam = lib_dir.join("test.beam");

        fs::write(&source, "// test").unwrap();
        fs::write(&beam, "fake beam").unwrap();
        set_mtime(&source, 1000);
        set_mtime(&beam, 2000);

        assert!(is_up_to_date(&source, &beam));
    }

    #[test]
    fn test_is_up_to_date_source_newer() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = lib_dir.join("test.bt");
        let beam = lib_dir.join("test.beam");

        fs::write(&beam, "fake beam").unwrap();
        fs::write(&source, "// updated").unwrap();
        set_mtime(&beam, 1000);
        set_mtime(&source, 2000);

        assert!(!is_up_to_date(&source, &beam));
    }

    #[test]
    fn test_is_up_to_date_equal_mtimes_is_stale() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = lib_dir.join("test.bt");
        let beam = lib_dir.join("test.beam");

        fs::write(&source, "// test").unwrap();
        fs::write(&beam, "fake beam").unwrap();
        set_mtime(&source, 1000);
        set_mtime(&beam, 1000);

        // Equal mtimes treated as stale to avoid missed rebuilds on coarse filesystems
        assert!(!is_up_to_date(&source, &beam));
    }

    #[test]
    fn test_partition_by_freshness() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let ebin_dir = lib_dir.join("ebin");
        fs::create_dir_all(&ebin_dir).unwrap();

        let file_a = lib_dir.join("Alpha.bt");
        let file_b = lib_dir.join("Beta.bt");

        fs::write(&file_a, "// alpha").unwrap();
        fs::write(&file_b, "// beta").unwrap();

        // Make bt_stdlib_alpha.beam newer than Alpha.bt
        let beam_a = ebin_dir.join("bt_stdlib_alpha.beam");
        fs::write(&beam_a, "fake").unwrap();
        set_mtime(&file_a, 1000);
        set_mtime(&Utf8PathBuf::from(beam_a.as_str()), 2000);

        let sources = vec![file_a, file_b];
        let (to_compile, skipped) = partition_by_freshness(&sources, &ebin_dir);

        assert_eq!(to_compile.len(), 1);
        assert_eq!(skipped.len(), 1);
        assert!(to_compile[0].as_str().contains("Beta"));
        assert!(skipped[0].as_str().contains("Alpha"));
    }

    #[test]
    fn test_generate_app_file() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_files = vec![
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/String.bt"),
        ];

        generate_app_file(&ebin_dir, &source_files).unwrap();

        let app_file = ebin_dir.join("beamtalk_stdlib.app");
        assert!(app_file.exists());

        let content = fs::read_to_string(app_file).unwrap();
        assert!(content.contains("beamtalk_stdlib"));
        assert!(content.contains("'bt_stdlib_integer'"));
        assert!(content.contains("'bt_stdlib_string'"));
    }

    #[test]
    fn test_generate_app_file_empty() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        generate_app_file(&ebin_dir, &[]).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(content.contains("{modules, []}"));
    }

    #[test]
    fn test_generate_app_file_invalid_name_errors() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_files = vec![
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/my-bad-name.bt"),
        ];

        let result = generate_app_file(&ebin_dir, &source_files);
        assert!(result.is_err());
    }
}
