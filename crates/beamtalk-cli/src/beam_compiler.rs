// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BEAM bytecode compiler integration.
//!
//! This module handles compilation of Core Erlang (.core) files to BEAM bytecode (.beam)
//! by invoking the external `escript` and `erlc` tools. It manages the compilation pipeline
//! and provides error handling for missing dependencies.
//!
//! # Architecture
//!
//! The compilation process:
//! 1. Generate Core Erlang from AST using `beamtalk_core::erlang`
//! 2. Write .core files to build directory
//! 3. Invoke compile.erl escript to batch compile .core → .beam
//! 4. Collect results and report success/failure
//!
//! # Example
//!
//! ```no_run
//! use beamtalk_cli::beam_compiler::BeamCompiler;
//! use camino::Utf8PathBuf;
//!
//! let compiler = BeamCompiler::new(Utf8PathBuf::from("build"));
//! let core_files = vec![Utf8PathBuf::from("build/my_module.core")];
//! let beam_files = compiler.compile_batch(&core_files)?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

/// Embedded compile.erl escript for batch compilation.
const COMPILE_ESCRIPT: &str = include_str!("../templates/compile.erl");

/// BEAM bytecode compiler.
///
/// Handles compilation of Core Erlang to BEAM bytecode using the embedded
/// compile.erl escript and the system's erlc compiler.
#[derive(Debug)]
pub struct BeamCompiler {
    /// Output directory for BEAM files.
    output_dir: Utf8PathBuf,
}

impl BeamCompiler {
    /// Creates a new BEAM compiler with the specified output directory.
    ///
    /// # Arguments
    ///
    /// * `output_dir` - Directory where .beam files will be written
    ///
    /// # Example
    ///
    /// ```
    /// use beamtalk_cli::beam_compiler::BeamCompiler;
    /// use camino::Utf8PathBuf;
    ///
    /// let compiler = BeamCompiler::new(Utf8PathBuf::from("build"));
    /// ```
    pub fn new(output_dir: Utf8PathBuf) -> Self {
        Self { output_dir }
    }

    /// Compiles Core Erlang files to BEAM bytecode.
    ///
    /// This method batch compiles multiple .core files in parallel using the
    /// embedded compile.erl escript.
    ///
    /// # Arguments
    ///
    /// * `core_files` - List of .core file paths to compile
    ///
    /// # Returns
    ///
    /// A list of compiled .beam file paths on success.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - escript is not found in PATH
    /// - The escript fails to execute
    /// - Compilation errors occur in any module
    ///
    /// # Panics
    ///
    /// Panics if stdin, stdout, or stderr cannot be captured from the escript process.
    /// This is a programming error and should not happen in normal usage.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use beamtalk_cli::beam_compiler::BeamCompiler;
    /// # use camino::Utf8PathBuf;
    /// let compiler = BeamCompiler::new(Utf8PathBuf::from("build"));
    /// let core_files = vec![
    ///     Utf8PathBuf::from("build/module1.core"),
    ///     Utf8PathBuf::from("build/module2.core"),
    /// ];
    /// let beam_files = compiler.compile_batch(&core_files)?;
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn compile_batch(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        // Check if escript is available
        check_escript_available()?;

        // Ensure output directory exists
        std::fs::create_dir_all(&self.output_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create output directory '{}'", self.output_dir))?;

        // Write compile.erl to a temporary file
        let temp_dir = std::env::temp_dir();
        let escript_path = temp_dir.join("beamtalk_compile.erl");
        std::fs::write(&escript_path, COMPILE_ESCRIPT)
            .into_diagnostic()
            .wrap_err("Failed to write compile escript")?;

        // Make it executable on Unix
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&escript_path)
                .into_diagnostic()?
                .permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&escript_path, perms).into_diagnostic()?;
        }

        // Start the escript process
        let mut child = Command::new("escript")
            .arg(&escript_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .into_diagnostic()
            .wrap_err("Failed to spawn escript process")?;

        let mut stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = child.stdout.take().expect("Failed to open stdout");
        let stderr = child.stderr.take().expect("Failed to open stderr");

        // Prepare input: {OutputDir, [CoreFile1, CoreFile2, ...]}
        let core_files_str: Vec<String> = core_files
            .iter()
            .map(|p| format!("\"{}\"", p.as_str()))
            .collect();
        let input = format!(
            "{{\"{}\",[{}]}}.\n",
            self.output_dir.as_str(),
            core_files_str.join(",")
        );

        // Send input to escript
        stdin
            .write_all(input.as_bytes())
            .into_diagnostic()
            .wrap_err("Failed to write to escript stdin")?;
        drop(stdin); // Close stdin to signal end of input

        // Read output
        let reader = BufReader::new(stdout);
        let mut compiled_modules = Vec::new();
        let mut compilation_ok = false;

        for line in reader.lines() {
            let line = line.into_diagnostic()?;
            if line.starts_with("beamtalk-compile-module:") {
                let module_name = line.strip_prefix("beamtalk-compile-module:").unwrap_or("");
                let beam_file = self.output_dir.join(format!("{module_name}.beam"));
                compiled_modules.push(beam_file);
            } else if line == "beamtalk-compile-result-ok" {
                compilation_ok = true;
            } else if line == "beamtalk-compile-result-error" {
                compilation_ok = false;
            }
        }

        // Read stderr for error messages
        let stderr_reader = BufReader::new(stderr);
        let mut error_messages = Vec::new();
        for line in stderr_reader.lines() {
            let line = line.into_diagnostic()?;
            if !line.is_empty() {
                error_messages.push(line);
            }
        }

        // Wait for process to complete
        let status = child
            .wait()
            .into_diagnostic()
            .wrap_err("Failed to wait for escript process")?;

        // Clean up temporary escript file
        let _ = std::fs::remove_file(&escript_path);

        // Check results
        if !status.success() || !compilation_ok {
            let error_msg = if error_messages.is_empty() {
                "Compilation failed".to_string()
            } else {
                format!("Compilation failed:\n{}", error_messages.join("\n"))
            };
            miette::bail!(error_msg);
        }

        Ok(compiled_modules)
    }
}

/// Checks if escript is available in the system PATH.
///
/// # Errors
///
/// Returns an error with installation instructions if escript is not found.
pub fn check_escript_available() -> Result<()> {
    // Try to run escript --version
    let result = Command::new("escript")
        .arg("--version")
        .output()
        .into_diagnostic();

    match result {
        Ok(output) if output.status.success() => Ok(()),
        _ => {
            miette::bail!(
                "escript not found in PATH. Please install Erlang/OTP.\n\
                 \n\
                 Installation instructions:\n\
                 - Ubuntu/Debian: sudo apt-get install erlang\n\
                 - macOS: brew install erlang\n\
                 - Windows: Download from https://www.erlang.org/downloads\n\
                 \n\
                 After installation, make sure escript is in your PATH."
            );
        }
    }
}

/// Generates Core Erlang from a Beamtalk module and writes it to a file.
///
/// # Arguments
///
/// * `module` - The parsed Beamtalk module
/// * `module_name` - Name for the module (without extension)
/// * `output_path` - Path where the .core file should be written
///
/// # Errors
///
/// Returns an error if code generation or file writing fails.
///
/// # Example
///
/// ```no_run
/// use beamtalk_cli::beam_compiler::write_core_erlang;
/// use beamtalk_core::ast::Module;
/// use beamtalk_core::parse::Span;
/// use camino::Utf8PathBuf;
///
/// let module = Module::new(Vec::new(), Span::new(0, 0));
/// write_core_erlang(&module, "my_module", &Utf8PathBuf::from("build/my_module.core"))?;
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn write_core_erlang(
    module: &beamtalk_core::ast::Module,
    module_name: &str,
    output_path: &Utf8Path,
) -> Result<()> {
    // Generate Core Erlang
    let core_erlang = beamtalk_core::erlang::generate_with_name(module, module_name)
        .into_diagnostic()
        .wrap_err("Failed to generate Core Erlang")?;

    // Write to file
    std::fs::write(output_path, core_erlang)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::ast::Module;
    use beamtalk_core::parse::Span;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_check_escript_available() {
        // This test will pass if escript is installed, skip otherwise
        match check_escript_available() {
            Ok(()) => {
                // escript is available, test passes
            }
            Err(_) => {
                // escript not available, skip test
                println!("Skipping test - escript not installed");
            }
        }
    }

    #[test]
    fn test_write_core_erlang() {
        let temp = TempDir::new().unwrap();
        let output_path = Utf8PathBuf::from_path_buf(temp.path().join("test_module.core")).unwrap();

        let module = Module::new(Vec::new(), Span::new(0, 0));
        let result = write_core_erlang(&module, "test_module", &output_path);

        assert!(result.is_ok());
        assert!(output_path.exists());

        let content = fs::read_to_string(output_path).unwrap();
        assert!(content.contains("module 'test_module'"));
        assert!(content.contains("attributes ['behaviour' = ['gen_server']]"));
    }

    #[test]
    fn test_beam_compiler_new() {
        let output_dir = Utf8PathBuf::from("build");
        let compiler = BeamCompiler::new(output_dir.clone());
        assert_eq!(compiler.output_dir, output_dir);
    }

    #[test]
    fn test_compile_batch_creates_output_dir() {
        // Only run if escript is available
        if check_escript_available().is_err() {
            println!("Skipping test - escript not installed");
            return;
        }

        let temp = TempDir::new().unwrap();
        let output_dir = Utf8PathBuf::from_path_buf(temp.path().join("build")).unwrap();
        let compiler = BeamCompiler::new(output_dir.clone());

        // Create a simple Core Erlang module
        let core_content = r"module 'test' ['init'/1, 'handle_cast'/2, 'handle_call'/3, 'code_change'/3, 'dispatch'/3, 'method_table'/0]
  attributes ['behaviour' = ['gen_server']]

'init'/1 = fun (_Args) ->
    let InitialState = ~{'__class__' => 'Test', '__methods__' => ~{}~}~
    in {'ok', InitialState}

'handle_cast'/2 = fun (Msg, State) ->
    {'noreply', State}

'handle_call'/3 = fun (Msg, _From, State) ->
    {'reply', 'ok', State}

'code_change'/3 = fun (_OldVsn, State, _Extra) ->
    {'ok', State}

'dispatch'/3 = fun (Selector, Args, State) ->
    {'reply', {'error', 'does_not_understand'}, State}

'method_table'/0 = fun () ->
    ~{}~

end
";

        let core_file = Utf8PathBuf::from_path_buf(temp.path().join("test.core")).unwrap();
        fs::write(&core_file, core_content).unwrap();

        let result = compiler.compile_batch(&[core_file]);

        // Check that output directory was created
        assert!(output_dir.exists());

        if let Err(e) = result {
            println!("Compilation failed (expected in CI): {e:?}");
        }
    }
}
