// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
pub fn build(path: &str) -> Result<()> {
    let source_path = Utf8PathBuf::from(path);

    // Find source files
    let source_files = find_source_files(&source_path)?;

    if source_files.is_empty() {
        miette::bail!("No .bt source files found in '{path}'");
    }

    println!("Building {} file(s)...", source_files.len());

    // Parse each file
    for file in &source_files {
        compile_file(file)?;
    }

    println!("Build complete");
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

fn compile_file(path: &Utf8Path) -> Result<()> {
    println!("  Compiling {path}...");

    // Read source file
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read file '{path}'"))?;

    // Lex and parse the source using beamtalk-core
    // Note: Full compilation will be implemented once codegen (BT-10) is complete
    let tokens = beamtalk_core::parse::lex_with_eof(&source);
    let (_module, diagnostics) = beamtalk_core::parse::parse(tokens);

    // Check for errors
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::parse::Severity::Error);

    if has_errors {
        // Display diagnostics
        for diagnostic in &diagnostics {
            let severity = match diagnostic.severity {
                beamtalk_core::parse::Severity::Error => "error",
                beamtalk_core::parse::Severity::Warning => "warning",
            };
            eprintln!(
                "  {} at {}:{}: {}",
                severity,
                diagnostic.span.start(),
                diagnostic.span.end(),
                diagnostic.message
            );
        }
        miette::bail!("Failed to compile '{path}'");
    }

    println!("    ✓ Parsed successfully");
    // TODO: Generate Core Erlang once codegen is implemented
    // TODO: Invoke erlc to compile to BEAM

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
        write_test_file(&test_file, "test := [1 + 2].");

        let result = compile_file(&test_file);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_file_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        write_test_file(&test_file, "test := [1 + ]."); // Syntax error

        let result = compile_file(&test_file);
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
        assert!(result.is_ok());
    }

    #[test]
    fn test_build_multiple_files() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test1 := [1].");
        write_test_file(&src_path.join("file2.bt"), "test2 := [2].");

        let result = build(project_path.as_str());
        assert!(result.is_ok());
    }
}
