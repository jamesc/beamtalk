// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.

use crate::beam_compiler::{BeamCompiler, compile_source_with_bindings};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fs;
use tracing::{debug, error, info, instrument, warn};

use super::app_file;
use super::manifest;

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str, options: &beamtalk_core::CompilerOptions) -> Result<()> {
    info!("Starting build");
    let source_path = Utf8PathBuf::from(path);

    // Find source files
    let source_files = find_source_files(&source_path)?;

    if source_files.is_empty() {
        error!("No .bt source files found in '{}'", path);
        miette::bail!("No .bt source files found in '{path}'");
    }

    info!(count = source_files.len(), "Found source files");
    debug!("Building {} file(s)", source_files.len());

    // Determine project root (for directory input, use the path; for file input, use parent)
    let project_root = if source_path.is_dir() {
        source_path.clone()
    } else {
        source_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), Utf8Path::to_path_buf)
    };

    // Look for package manifest
    let pkg_manifest = manifest::find_manifest(&project_root)?;
    if let Some(ref pkg) = pkg_manifest {
        info!(name = %pkg.name, version = %pkg.version, "Found package manifest");
        debug!(?pkg, "Package manifest details");
    } else {
        debug!("No beamtalk.toml found, using default behavior");
    }

    // Create build directory relative to project root
    // ADR 0026 §5: Package mode outputs to _build/dev/ebin/, single-file mode keeps build/
    let build_dir = if pkg_manifest.is_some() {
        project_root.join("_build").join("dev").join("ebin")
    } else {
        project_root.join("build")
    };

    debug!("Creating build directory: {}", build_dir);
    std::fs::create_dir_all(&build_dir)
        .into_diagnostic()
        .wrap_err("Failed to create build directory")?;

    // Determine the source root for computing relative module paths
    let src_dir = project_root.join("src");
    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    // Pass 1: compute module names and build the class → module index.
    // This allows later files to resolve cross-file class references (including
    // classes in package subdirectories) during code generation.
    let mut file_module_pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)> = Vec::new();
    let (class_module_index, class_superclass_index) = if let Some(ref pkg) = pkg_manifest {
        build_class_module_index(&source_files, source_root.as_deref(), &pkg.name)?
    } else {
        (HashMap::new(), HashMap::new())
    };

    for file in &source_files {
        let stem = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name contains only safe characters
        if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
            miette::bail!(
                "Invalid module name '{}': must contain only alphanumeric characters and underscores",
                stem
            );
        }

        // ADR 0026: Package mode uses bt@{package}@{relative_path} naming
        // ADR 0016: Single-file mode uses bt@{module} naming
        let module_name = if let Some(ref pkg) = pkg_manifest {
            let relative_module = compute_relative_module(file, source_root.as_deref())?;
            format!("bt@{}@{}", pkg.name, relative_module)
        } else {
            format!(
                "bt@{}",
                beamtalk_core::codegen::core_erlang::to_module_name(stem)
            )
        };
        let core_file = build_dir.join(format!("{module_name}.core"));
        file_module_pairs.push((file.clone(), module_name, core_file));
    }

    // Pass 2: compile each file with the full class → module index.
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();
    for (file, module_name, core_file) in &file_module_pairs {
        compile_file(
            file,
            module_name,
            core_file,
            options,
            &class_module_index,
            &class_superclass_index,
        )?;
        core_files.push(core_file.clone());
        module_names.push(module_name.clone());
    }

    // Batch compile Core Erlang to BEAM
    info!("Compiling Core Erlang to BEAM");
    let compiler = BeamCompiler::new(build_dir.clone());
    let beam_files = compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile Core Erlang to BEAM")?;

    info!(
        beam_count = beam_files.len(),
        "Build completed successfully"
    );

    // ADR 0026 §3: Generate .app file in package mode
    if let Some(ref pkg) = pkg_manifest {
        // TODO: Extract class metadata from compiled modules in a future issue.
        // For now, pass an empty list — .app file still includes module list.
        let class_metadata: Vec<app_file::ClassMetadata> = Vec::new();
        app_file::generate_app_file(&build_dir, pkg, &module_names, &class_metadata)?;
        info!(name = %pkg.name, "Generated .app file");
    }

    Ok(())
}

/// Find all `.bt` source files at the given path.
///
/// If `path` is a file, returns it (must have `.bt` extension).
/// If `path` is a directory, searches `src/` subdirectory first, falling back
/// to the directory itself.
fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    if path.is_file() {
        if path.extension() == Some("bt") {
            files.push(path.to_path_buf());
        } else {
            miette::bail!("File '{}' is not a .bt source file", path);
        }
    } else if path.is_dir() {
        // Look for src directory or .bt files in current directory
        let src_dir = path.join("src");
        let search_dir = if src_dir.exists() {
            src_dir
        } else {
            path.to_path_buf()
        };

        collect_bt_files_recursive(&search_dir, &mut files)?;
    } else {
        miette::bail!("Path '{}' does not exist", path);
    }

    Ok(files)
}

/// Collect all `.bt` source files from a directory tree.
///
/// Returns an error if the directory does not exist or cannot be read.
pub fn collect_source_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();
    collect_bt_files_recursive(dir, &mut files)?;
    Ok(files)
}

/// Recursively collect all `.bt` files from a directory tree.
///
/// Symlinks are skipped to avoid potential infinite recursion from circular links.
fn collect_bt_files_recursive(dir: &Utf8Path, files: &mut Vec<Utf8PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let file_type = entry.file_type().into_diagnostic()?;
        if file_type.is_symlink() {
            continue;
        }
        let entry_path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path"))?;

        if file_type.is_dir() {
            collect_bt_files_recursive(&entry_path, files)?;
        } else if file_type.is_file() && entry_path.extension() == Some("bt") {
            files.push(entry_path);
        }
    }
    Ok(())
}

/// Compute the relative module path from a source file.
///
/// When `source_root` is provided (i.e., a `src/` directory exists), computes
/// the path relative to it. Subdirectories become `@` segments.
///
/// Examples:
/// - `src/counter.bt` → `counter`
/// - `src/util/math.bt` → `util@math`
///
/// Falls back to the file stem when no source root is available.
fn compute_relative_module(file: &Utf8Path, source_root: Option<&Utf8Path>) -> Result<String> {
    if let Some(root) = source_root {
        if let Ok(relative) = file.strip_prefix(root) {
            let without_ext = relative.with_extension("");
            let segments: Vec<String> = without_ext
                .components()
                .map(|c| {
                    let segment = c.as_str();
                    // Validate each path segment
                    if !segment
                        .chars()
                        .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                    {
                        miette::bail!(
                            "Invalid directory name '{}' in source path '{}': must contain only alphanumeric characters and underscores",
                            segment,
                            file
                        );
                    }
                    Ok(beamtalk_core::codegen::core_erlang::to_module_name(segment))
                })
                .collect::<Result<_>>()?;
            return Ok(segments.join("@"));
        }
    }
    // Fallback: use file stem
    let stem = file.file_stem().unwrap_or("unknown");
    Ok(beamtalk_core::codegen::core_erlang::to_module_name(stem))
}

/// Compile a single `.bt` source file to Core Erlang, printing progress.
fn compile_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
) -> Result<()> {
    debug!("Compiling {path}");

    compile_source_with_bindings(
        path,
        module_name,
        core_file,
        options,
        &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
        class_module_index,
        class_superclass_index,
    )?;

    debug!("Generated Core Erlang: {core_file}");

    Ok(())
}

/// Build class indexes from a set of source files.
///
/// Returns two indexes:
/// 1. **Class module index:** Maps class names to compiled module names
///    (e.g. `"SchemeEnv"` → `"bt@sicp_example@scheme@env"`).
/// 2. **Class superclass index:** Maps class names to their direct superclass names
///    (e.g. `"MyChild"` → `"MyParent"`). Used by BT-894 to resolve cross-file
///    inheritance so the compiler can determine value-object vs actor codegen.
pub fn build_class_module_index(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
) -> Result<(HashMap<String, String>, HashMap<String, String>)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();

    for file in source_files {
        let relative_module = compute_relative_module(file, source_root)?;
        let module_name = format!("bt@{pkg_name}@{relative_module}");

        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                warn!(
                    file = %file,
                    error = %e,
                    "Cannot read source file during index pass; class resolution from this file will be skipped"
                );
                continue;
            }
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        if !diagnostics.is_empty() {
            warn!(
                file = %file,
                diagnostic_count = diagnostics.len(),
                "Source file has parse errors during index pass; class resolution from this file may be incomplete"
            );
        }

        for class in &module.classes {
            let class_name = class.name.name.to_string();
            if let Some(existing) = module_index.get(&class_name) {
                if existing != &module_name {
                    eprintln!(
                        "Warning: class '{class_name}' is defined in both '{existing}' and \
                         '{module_name}'; using '{module_name}' for cross-file dispatch"
                    );
                }
            }
            module_index.insert(class_name.clone(), module_name.clone());
            // BT-894: Record direct superclass for cross-file hierarchy resolution
            if let Some(ref superclass) = class.superclass {
                superclass_index.insert(class_name, superclass.name.to_string());
            }
        }
    }

    Ok((module_index, superclass_index))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project(temp: &TempDir) -> Utf8PathBuf {
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        fs::create_dir_all(&src_path).unwrap();
        project_path
    }

    fn write_test_file(path: &Utf8Path, content: &str) {
        fs::write(path, content).unwrap();
    }

    fn default_options() -> beamtalk_core::CompilerOptions {
        beamtalk_core::CompilerOptions::default()
    }

    #[test]
    fn test_find_source_files_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        write_test_file(&test_file, "test := [1].");

        let files = find_source_files(&test_file).unwrap();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0], test_file);
    }

    #[test]
    fn test_find_source_files_in_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test := [1].");
        write_test_file(&src_path.join("file2.bt"), "test := [2].");
        write_test_file(&src_path.join("other.txt"), "not beamtalk");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("file1.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("file2.bt")));
    }

    #[test]
    fn test_find_source_files_no_src_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        write_test_file(&project_path.join("test.bt"), "test := [1].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_find_source_files_non_bt_file_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.txt");
        write_test_file(&test_file, "not beamtalk");

        let result = find_source_files(&test_file);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_source_files_nonexistent_path() {
        let path = Utf8PathBuf::from("/nonexistent/path");
        let result = find_source_files(&path);
        assert!(result.is_err());
    }

    #[test]
    fn test_compile_valid_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + 2].");

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &HashMap::new(),
            &HashMap::new(),
        );
        assert!(result.is_ok());
        assert!(core_file.exists());
    }

    #[test]
    fn test_compile_file_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + ]."); // Syntax error

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &HashMap::new(),
            &HashMap::new(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_build_empty_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = build(project_path.as_str(), &default_options());
        assert!(result.is_err());
    }

    #[test]
    fn test_build_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        let result = build(project_path.as_str(), &default_options());

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_multiple_files() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test1 := [1].");
        write_test_file(&src_path.join("file2.bt"), "test2 := [2].");

        let result = build(project_path.as_str(), &default_options());

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options());

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                eprintln!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_malformed_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "this is not valid toml {{{{",
        );

        let result = build(project_path.as_str(), &default_options());
        let err = result.expect_err("Expected build to fail due to malformed manifest");
        let error_msg = format!("{err:?}");
        assert!(
            error_msg.contains("Failed to parse manifest"),
            "Expected error to mention manifest parse failure, got: {error_msg}"
        );
    }

    #[test]
    fn test_find_source_files_recursive() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();

        write_test_file(&src_path.join("main.bt"), "main := [1].");
        write_test_file(&util_path.join("math.bt"), "math := [2].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("math.bt")));
    }

    #[test]
    fn test_find_source_files_deeply_nested() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let deep_path = src_path.join("a").join("b").join("c");
        fs::create_dir_all(&deep_path).unwrap();

        write_test_file(&deep_path.join("deep.bt"), "deep := [42].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().contains('a'));
    }

    #[test]
    fn test_compute_relative_module_flat() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/counter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "counter"
        );
    }

    #[test]
    fn test_compute_relative_module_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/util/math.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "util@math"
        );
    }

    #[test]
    fn test_compute_relative_module_deep_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/a/b/c.bt");
        assert_eq!(compute_relative_module(file, Some(root)).unwrap(), "a@b@c");
    }

    #[test]
    fn test_compute_relative_module_camel_case() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/MyCounter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "my_counter"
        );
    }

    #[test]
    fn test_compute_relative_module_no_root() {
        let file = Utf8Path::new("/project/counter.bt");
        assert_eq!(compute_relative_module(file, None).unwrap(), "counter");
    }

    #[test]
    fn test_compute_relative_module_invalid_dir_name() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/my-dir/counter.bt");
        let result = compute_relative_module(file, Some(root));
        assert!(result.is_err());
    }

    #[test]
    fn test_build_with_manifest_generates_package_module_name() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options());

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // Verify the .core file was generated with package-qualified name
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_preserves_bt_prefix() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options());

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("build/bt@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected bt@counter.core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_subdirectory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();
        write_test_file(&util_path.join("math.bt"), "math := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options());

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@util@math.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_creates_build_dev_ebin() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options());

        // Verify _build/dev/ebin/ directory structure exists
        let ebin_dir = project_path.join("_build/dev/ebin");
        assert!(ebin_dir.exists(), "Expected _build/dev/ebin/ directory");

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_uses_build_dir() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options());

        // Verify build/ directory is used (not _build/)
        let build_dir = project_path.join("build");
        assert!(
            build_dir.exists(),
            "Expected build/ directory for single-file mode"
        );
        let ebin_dir = project_path.join("_build");
        assert!(
            !ebin_dir.exists(),
            "_build/ should not exist in single-file mode"
        );

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_generates_app_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\ndescription = \"Test app\"\n",
        );

        let result = build(project_path.as_str(), &default_options());

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // .app is generated after BEAM compile — may not exist if escript missing
                // but the .core file should exist
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(core_file.exists(), "Expected .core file at {core_file}");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }

        // If BEAM compilation succeeded, .app file should exist
        let app_file = project_path.join("_build/dev/ebin/my_app.app");
        assert!(app_file.exists(), "Expected .app file at {app_file}");
        let content = fs::read_to_string(&app_file).unwrap();
        assert!(content.contains("{application, my_app, ["));
        assert!(content.contains("{description, \"Test app\"}"));
        assert!(content.contains("{vsn, \"0.1.0\"}"));
        assert!(content.contains("'bt@my_app@counter'"));
    }

    #[test]
    fn test_build_class_module_index_skips_unreadable_file() {
        // A path that does not exist on disk — simulates an unreadable file.
        // The function should succeed (not error) and return an empty index.
        let nonexistent = Utf8PathBuf::from("/nonexistent/no_such_file.bt");
        let result = build_class_module_index(&[nonexistent], None, "my_app");
        assert!(result.is_ok());
        let (module_index, superclass_index) = result.unwrap();
        assert!(module_index.is_empty());
        assert!(superclass_index.is_empty());
    }

    #[test]
    fn test_build_class_module_index_handles_parse_errors_gracefully() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let bad_file = project_path.join("bad.bt");
        // Write a file with a syntax error — parse diagnostics should be non-empty.
        write_test_file(&bad_file, "class Foo { @@@@invalid syntax }");

        // Should succeed and return whatever classes (if any) were parsed before the error.
        let result = build_class_module_index(&[bad_file], None, "my_app");
        assert!(result.is_ok());
    }
}
