// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Run beamtalk programs.

use miette::{Context, IntoDiagnostic, Result};
use std::process::Command;

/// Compile and run a beamtalk program.
pub fn run(path: &str) -> Result<()> {
    // First, build the project
    println!("Building...");
    super::build::build(path)?;

    println!("\nRunning...");
    // TODO: Once codegen is implemented, we need to:
    // 1. Find the compiled BEAM file
    // 2. Execute it with erl
    //
    // For now, we'll just show that it would run
    println!("(Execution will be implemented once codegen is complete)");

    // Example of how to run once BEAM files are generated:
    // let status = Command::new("erl")
    //     .args(["-noshell", "-s", "main", "-s", "init", "stop"])
    //     .status()
    //     .into_diagnostic()
    //     .wrap_err("Failed to execute erl")?;
    //
    // if !status.success() {
    //     miette::bail!("Program exited with error");
    // }

    Ok(())
}

/// Find the erlc executable in the system PATH.
///
/// TODO: This implementation uses `which` which is Unix-specific.
/// For Windows support, consider using the `which` crate or checking
/// common installation paths like `C:\Program Files\erl*\bin\erlc.exe`.
#[cfg_attr(not(test), allow(dead_code))]
fn find_erlc() -> Result<String> {
    // Try to find erlc in PATH
    let output = Command::new("which")
        .arg("erlc")
        .output()
        .into_diagnostic()
        .wrap_err("Failed to search for erlc")?;

    if output.status.success() {
        let path = String::from_utf8(output.stdout)
            .into_diagnostic()
            .wrap_err("Invalid UTF-8 in erlc path")?;
        Ok(path.trim().to_string())
    } else {
        miette::bail!(
            "erlc not found in PATH. Please install Erlang/OTP.\n\
             Visit: https://www.erlang.org/downloads"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project(temp: &TempDir) -> String {
        let project_path = temp.path().to_string_lossy().to_string();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(
            src_path.join("main.bt"),
            "main := [Transcript show: 'Hello'].",
        )
        .unwrap();
        project_path
    }

    #[test]
    fn test_run_calls_build() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = run(&project_path);
        
        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Run failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_run_with_invalid_path() {
        let result = run("/nonexistent/path");
        assert!(result.is_err());
    }

    #[test]
    fn test_run_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = temp.path().to_string_lossy().to_string();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(src_path.join("main.bt"), "main := [1 + ].").unwrap();

        // Should fail due to syntax error in build phase
        let result = run(&project_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_erlc_exists() {
        // This test will only pass if erlc is installed
        // We'll skip it if erlc is not available
        match find_erlc() {
            Ok(path) => {
                assert!(!path.is_empty());
                assert!(path.contains("erlc"));
            }
            Err(_) => {
                // Skip test if erlc not found - this is expected in many environments
                println!("Skipping test_find_erlc_exists - erlc not installed");
            }
        }
    }
}
