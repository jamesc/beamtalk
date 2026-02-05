// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.

use crate::beam_compiler::{BeamCompiler, write_core_erlang};
use crate::diagnostic::CompileDiagnostic;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::{debug, error, info, instrument, warn};

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str) -> Result<()> {
    info!("Starting build");
    let source_path = Utf8PathBuf::from(path);

    // Find source files
    let source_files = find_source_files(&source_path)?;

    if source_files.is_empty() {
        error!("No .bt source files found in '{}'", path);
        miette::bail!("No .bt source files found in '{path}'");
    }

    info!(count = source_files.len(), "Found source files");
    println!("Building {} file(s)...", source_files.len());

    // Determine project root (for directory input, use the path; for file input, use parent)
    let project_root = if source_path.is_dir() {
        source_path.clone()
    } else {
        source_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), Utf8Path::to_path_buf)
    };

    // Create build directory relative to project root
    let build_dir = project_root.join("build");
    debug!("Creating build directory: {}", build_dir);
    std::fs::create_dir_all(&build_dir)
        .into_diagnostic()
        .wrap_err("Failed to create build directory")?;

    // Compile each file to Core Erlang
    let mut core_files = Vec::new();
    for file in &source_files {
        let module_name = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name contains only safe characters
        if !module_name
            .chars()
            .all(|c| c == '_' || c.is_ascii_alphanumeric())
        {
            miette::bail!(
                "Invalid module name '{}': must contain only alphanumeric characters and underscores",
                module_name
            );
        }

        let core_file = build_dir.join(format!("{module_name}.core"));

        compile_file(file, module_name, &core_file)?;
        core_files.push(core_file);
    }

    // Batch compile Core Erlang to BEAM
    info!("Compiling Core Erlang to BEAM");
    println!("  Compiling to BEAM bytecode...");
    let compiler = BeamCompiler::new(build_dir);
    let beam_files = compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile Core Erlang to BEAM")?;

    info!(
        beam_count = beam_files.len(),
        "Build completed successfully"
    );
    println!("Build complete");
    println!("  Generated {} BEAM file(s) in build/", beam_files.len());

    Ok(())
}

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

        for entry in fs::read_dir(&search_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read directory '{search_dir}'"))?
        {
            let entry = entry.into_diagnostic()?;
            let entry_path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| miette::miette!("Non-UTF-8 path"))?;

            if entry_path.extension() == Some("bt") {
                files.push(entry_path);
            }
        }
    } else {
        miette::bail!("Path '{}' does not exist", path);
    }

    Ok(files)
}

#[instrument(skip_all, fields(path = %path, module = module_name))]
fn compile_file(path: &Utf8Path, module_name: &str, core_file: &Utf8Path) -> Result<()> {
    debug!("Compiling module '{}'", module_name);
    println!("  Compiling {path}...");

    // Read source file
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read file '{path}'"))?;

    // Lex and parse the source using beamtalk-core
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Check for errors
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);

    // Display all diagnostics (errors and warnings) using miette formatting
    if !diagnostics.is_empty() {
        debug!(
            diagnostic_count = diagnostics.len(),
            "Found diagnostics during compilation"
        );
        for diagnostic in &diagnostics {
            let compile_diag =
                CompileDiagnostic::from_core_diagnostic(diagnostic, path.as_str(), &source);

            // Use appropriate log level based on diagnostic severity
            match diagnostic.severity {
                beamtalk_core::source_analysis::Severity::Error => {
                    error!("{:?}", miette::Report::new(compile_diag));
                }
                beamtalk_core::source_analysis::Severity::Warning => {
                    warn!("{:?}", miette::Report::new(compile_diag));
                }
            }
        }
    }

    // Fail compilation only if there are errors
    if has_errors {
        error!("Compilation failed for '{}'", path);
        miette::bail!("Failed to compile '{path}'");
    }

    debug!("Parsed successfully: {}", path);
    println!("    ✓ Parsed successfully");

    // Generate Core Erlang
    write_core_erlang(&module, module_name, core_file)
        .wrap_err_with(|| format!("Failed to generate Core Erlang for '{path}'"))?;

    debug!("Generated Core Erlang: {}", core_file);
    println!("    ✓ Generated Core Erlang: {core_file}");

    Ok(())
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

        let result = compile_file(&test_file, "test", &core_file);
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

        let result = compile_file(&test_file, "test", &core_file);
        assert!(result.is_err());
    }

    #[test]
    fn test_build_empty_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = build(project_path.as_str());
        assert!(result.is_err());
    }

    #[test]
    fn test_build_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        let result = build(project_path.as_str());

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

        let result = build(project_path.as_str());

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
}
