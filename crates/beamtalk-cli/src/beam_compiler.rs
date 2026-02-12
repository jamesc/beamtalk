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
//! 3. Invoke compile.escript to batch compile .core → .beam
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
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc;
use std::thread;
use tracing::{debug, error, instrument, warn};

/// Embedded compile.escript for batch compilation.
const COMPILE_ESCRIPT: &str = include_str!("../templates/compile.escript");

/// Atomic counter for generating unique escript filenames.
///
/// This ensures each temporary escript file has a unique name, preventing
/// collisions when multiple compilation processes run in parallel.
static ESCRIPT_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Escapes a string for use in an Erlang term.
///
/// This escapes backslashes, quotes, and control characters to prevent
/// injection attacks and ensure valid Erlang term syntax when constructing
/// Erlang terms from file paths.
fn escape_erlang_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(c),
        }
    }
    result
}

/// Validates that a module name contains only safe identifier characters.
///
/// Module names should only contain ASCII alphanumeric characters and underscores
/// to prevent path traversal vulnerabilities.
fn is_valid_module_name(name: &str) -> bool {
    // ADR 0016: @ is legal in Erlang unquoted atoms and used for namespacing
    !name.is_empty()
        && name
            .chars()
            .all(|c| c == '_' || c == '@' || c.is_ascii_alphanumeric())
}

/// BEAM bytecode compiler.
///
/// Handles compilation of Core Erlang to BEAM bytecode using the embedded
/// compile.escript and the system's erlc compiler.
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
    /// embedded compile.escript.
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
    /// - stdin, stdout, or stderr cannot be captured from the escript process
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
    #[allow(clippy::too_many_lines)]
    #[instrument(skip_all, fields(file_count = core_files.len(), output_dir = %self.output_dir))]
    pub fn compile_batch(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        debug!(
            "Starting batch compilation of {} Core Erlang files",
            core_files.len()
        );
        // Check if escript is available
        check_escript_available()?;

        // Ensure output directory exists
        std::fs::create_dir_all(&self.output_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create output directory '{}'", self.output_dir))?;

        // Write compile.escript to a temporary file with unique name
        // Use both process ID and atomic counter to ensure uniqueness when tests run in parallel
        let temp_dir = std::env::temp_dir();
        let counter = ESCRIPT_COUNTER.fetch_add(1, Ordering::Relaxed);
        let escript_path = temp_dir.join(format!(
            "beamtalk_compile_{}_{}.escript",
            std::process::id(),
            counter
        ));
        debug!(
            "Writing escript to temporary file: {}",
            escript_path.display()
        );
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

        // Start the escript process with explicit working directory
        // Use temp_dir as working directory to avoid getcwd() errors
        // when test temp directories are deleted before subprocess completes
        debug!("Spawning escript process");
        let mut child = Command::new("escript")
            .arg(&escript_path)
            .current_dir(&temp_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .into_diagnostic()
            .wrap_err("Failed to spawn escript process")?;

        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture escript stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture escript stdout"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture escript stderr"))?;

        // Prepare input: {OutputDir, [CoreFile1, CoreFile2, ...]}
        // Use absolute paths since escript runs from temp_dir
        let abs_output_dir = std::fs::canonicalize(&self.output_dir)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!(
                    "Failed to canonicalize output directory '{}'",
                    self.output_dir
                )
            })?;
        let abs_output_dir_str = abs_output_dir.to_string_lossy();

        let core_files_str: Vec<String> = core_files
            .iter()
            .map(|p| {
                // Canonicalize each core file path to absolute
                // Since we require output_dir to exist (it gets canonicalized above),
                // core files should also exist and be canonicalizable
                match std::fs::canonicalize(p.as_std_path()) {
                    Ok(abs_path) => {
                        format!("\"{}\"", escape_erlang_string(&abs_path.to_string_lossy()))
                    }
                    Err(e) => {
                        // Log warning but continue - file may still be found if cwd is correct
                        warn!("Failed to canonicalize '{}': {}. Using path as-is.", p, e);
                        format!("\"{}\"", escape_erlang_string(p.as_str()))
                    }
                }
            })
            .collect();
        let input = format!(
            "{{\"{}\",[{}]}}.\n",
            escape_erlang_string(&abs_output_dir_str),
            core_files_str.join(",")
        );

        // Send input to escript
        stdin
            .write_all(input.as_bytes())
            .into_diagnostic()
            .wrap_err("Failed to write to escript stdin")?;
        drop(stdin); // Close stdin to signal end of input

        // Read stderr concurrently to avoid deadlock
        let (stderr_tx, stderr_rx) = mpsc::channel();
        let stderr_thread = thread::spawn(move || {
            let stderr_reader = BufReader::new(stderr);
            let mut error_messages = Vec::new();
            for line in stderr_reader.lines().map_while(Result::ok) {
                if !line.is_empty() {
                    error_messages.push(line);
                }
            }
            stderr_tx.send(error_messages).ok();
        });

        // Read stdout for compilation results
        let reader = BufReader::new(stdout);
        let mut compiled_modules = Vec::new();
        let mut compilation_ok = false;

        for line in reader.lines() {
            let line = line.into_diagnostic()?;
            if line.starts_with("beamtalk-compile-module:") {
                let module_name = line
                    .strip_prefix("beamtalk-compile-module:")
                    .ok_or_else(|| miette::miette!("Invalid compile output format"))?;

                if module_name.is_empty() {
                    miette::bail!("Compilation produced module with empty name");
                }

                // Validate module name to prevent path traversal
                if !is_valid_module_name(module_name) {
                    miette::bail!("Compilation produced module with invalid name: {module_name}");
                }

                let beam_file = self.output_dir.join(format!("{module_name}.beam"));
                compiled_modules.push(beam_file);
            } else if line == "beamtalk-compile-result-ok" {
                compilation_ok = true;
            } else if line == "beamtalk-compile-result-error" {
                compilation_ok = false;
            }
        }

        // Wait for stderr thread to complete and collect error messages
        let error_messages = stderr_rx.recv().unwrap_or_else(|_| Vec::new());
        let _ = stderr_thread.join();

        // Wait for process to complete
        let status = child
            .wait()
            .into_diagnostic()
            .wrap_err("Failed to wait for escript process")?;

        // Clean up temporary escript file
        if let Err(err) = std::fs::remove_file(&escript_path) {
            warn!(
                "Failed to remove temporary escript file '{}': {}",
                escript_path.display(),
                err
            );
        }

        // Check results
        if !status.success() || !compilation_ok {
            let error_msg = if error_messages.is_empty() {
                "Compilation failed".to_string()
            } else {
                format!("Compilation failed:\n{}", error_messages.join("\n"))
            };
            debug!("Compilation failed with status: {:?}", status);
            miette::bail!(error_msg);
        }

        debug!(
            beam_count = compiled_modules.len(),
            "Compilation completed successfully"
        );
        Ok(compiled_modules)
    }
}

/// Checks if escript is available in the system PATH.
///
/// # Errors
///
/// Returns an error with installation instructions if escript is not found.
pub fn check_escript_available() -> Result<()> {
    // Try to find escript in PATH using 'which' (Unix) or 'where' (Windows)
    let which_cmd = if cfg!(windows) { "where" } else { "which" };
    let result = Command::new(which_cmd)
        .arg("escript")
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
/// use beamtalk_core::source_analysis::Span;
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
    write_core_erlang_with_source(module, module_name, output_path, None)
}

/// Writes a parsed Beamtalk module as Core Erlang to the specified path, with source text.
///
/// When source text is provided, method source is captured in class registration
/// metadata for `CompiledMethod` introspection (BT-101).
///
/// # Errors
///
/// Returns an error if the module name is invalid, code generation fails,
/// or writing to the output path fails.
pub fn write_core_erlang_with_source(
    module: &beamtalk_core::ast::Module,
    module_name: &str,
    output_path: &Utf8Path,
    source_text: Option<&str>,
) -> Result<()> {
    // Validate module name to prevent path traversal and injection
    if !is_valid_module_name(module_name) {
        miette::bail!(
            "Invalid module name '{}': must be non-empty and contain only alphanumeric characters, underscores, and @",
            module_name
        );
    }

    // Generate Core Erlang
    let core_erlang =
        beamtalk_core::erlang::generate_with_name_and_source(module, module_name, source_text)
            .into_diagnostic()
            .wrap_err("Failed to generate Core Erlang")?;

    // Write to file
    std::fs::write(output_path, core_erlang)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))?;

    // BT-499: Write EEP-48 doc chunk file if module has doc comments
    write_docs_file(module, output_path);

    Ok(())
}

/// Writes Core Erlang code with primitive bindings.
///
/// BT-295 / ADR 0007 Phase 3: Same as [`write_core_erlang`] but accepts
/// a binding table for pragma-driven dispatch.
///
/// # Errors
///
/// Returns an error if the module name is invalid, code generation fails,
/// or the output file cannot be written.
pub fn write_core_erlang_with_bindings(
    module: &beamtalk_core::ast::Module,
    module_name: &str,
    output_path: &Utf8Path,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
    source_text: Option<&str>,
    workspace_mode: bool,
) -> Result<()> {
    if !is_valid_module_name(module_name) {
        miette::bail!(
            "Invalid module name '{}': must be non-empty and contain only alphanumeric characters, underscores, and @",
            module_name
        );
    }

    let core_erlang = beamtalk_core::erlang::generate_with_bindings(
        module,
        module_name,
        bindings.clone(),
        source_text,
        workspace_mode,
    )
    .into_diagnostic()
    .wrap_err("Failed to generate Core Erlang")?;

    std::fs::write(output_path, core_erlang)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))?;

    // BT-499: Write EEP-48 doc chunk file if module has doc comments
    write_docs_file(module, output_path);

    Ok(())
}

/// Writes an EEP-48 `.docs` file alongside a `.core` file if the module has doc comments.
///
/// BT-499: The `.docs` file contains an Erlang term that will be injected as a "Docs"
/// chunk into the compiled `.beam` file by `compile.escript`.
fn write_docs_file(module: &beamtalk_core::ast::Module, core_output_path: &Utf8Path) {
    let docs_path = core_output_path.with_extension("docs");
    if let Some(docs_term) =
        beamtalk_core::codegen::core_erlang::doc_chunks::generate_docs_term(module)
    {
        if let Err(e) = std::fs::write(&docs_path, &docs_term) {
            debug!("Failed to write docs file '{}': {}", docs_path, e);
        }
    } else {
        // Remove stale .docs file if no docs to generate
        let _ = std::fs::remove_file(&docs_path);
    }
}

/// Compiles a Beamtalk source file (.bt) to Core Erlang (.core).
///
/// This is the single compilation domain service used by all CLI commands
/// (build, build-stdlib). It handles the full pipeline:
/// read source → lex → parse → validate → report diagnostics → generate Core Erlang.
///
/// # Arguments
///
/// * `source_path` - Path to the `.bt` source file
/// * `module_name` - Module name for the generated Core Erlang
/// * `core_output` - Path where the `.core` file should be written
/// * `options` - Compiler options (stdlib mode, primitive handling)
///
/// # Errors
///
/// Returns an error if reading, parsing, or code generation fails,
/// or if any diagnostic has error severity.
#[instrument(skip_all, fields(path = %source_path, module = module_name))]
pub fn compile_source(
    source_path: &Utf8Path,
    module_name: &str,
    core_output: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
) -> Result<()> {
    compile_source_with_bindings(
        source_path,
        module_name,
        core_output,
        options,
        &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
    )
}

/// Compiles a Beamtalk source file to Core Erlang with primitive bindings.
///
/// BT-295 / ADR 0007 Phase 3: Same as [`compile_source`] but accepts a
/// [`PrimitiveBindingTable`] for pragma-driven dispatch.
///
/// # Errors
///
/// Returns an error if reading, parsing, or code generation fails,
/// or if any diagnostic has error severity.
#[instrument(skip_all, fields(path = %source_path, module = module_name))]
pub fn compile_source_with_bindings(
    source_path: &Utf8Path,
    module_name: &str,
    core_output: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
) -> Result<()> {
    use crate::diagnostic::CompileDiagnostic;

    debug!("Compiling module '{}' with bindings", module_name);

    // Read source file
    let source = std::fs::read_to_string(source_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read file '{source_path}'"))?;

    // Lex and parse
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, mut diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run @primitive validation (ADR 0007)
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, options,
        );
    diagnostics.extend(primitive_diags);

    // Run semantic analysis (BT-401: sealed enforcement, undefined vars, etc.)
    let analysis_result = beamtalk_core::semantic_analysis::analyse(&module);
    diagnostics.extend(analysis_result.diagnostics);

    // Check for errors
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);

    if !diagnostics.is_empty() {
        debug!(
            diagnostic_count = diagnostics.len(),
            "Found diagnostics during compilation"
        );
        for diagnostic in &diagnostics {
            let compile_diag =
                CompileDiagnostic::from_core_diagnostic(diagnostic, source_path.as_str(), &source);
            eprintln!("{:?}", miette::Report::new(compile_diag));
        }
    }

    if has_errors {
        error!("Compilation failed for '{}'", source_path);
        miette::bail!("Failed to compile '{source_path}'");
    }

    debug!("Parsed successfully: {}", source_path);

    // Generate Core Erlang (with source text for CompiledMethod introspection BT-101, and bindings BT-295)
    // BT-374: Pass workspace_mode for workspace binding dispatch
    write_core_erlang_with_bindings(
        &module,
        module_name,
        core_output,
        bindings,
        Some(&source),
        options.workspace_mode,
    )
    .wrap_err_with(|| format!("Failed to generate Core Erlang for '{source_path}'"))?;

    debug!("Generated Core Erlang: {}", core_output);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::ast::Module;
    use beamtalk_core::source_analysis::Span;
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
    fn test_compile_source_valid() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("test.bt");
        let core_file = temp_path.join("test.core");
        fs::write(&source_file, "test := [1 + 2].").unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "test", &core_file, &options);

        assert!(result.is_ok());
        assert!(core_file.exists());
        let content = fs::read_to_string(core_file).unwrap();
        assert!(content.contains("module 'test'"));
    }

    #[test]
    fn test_compile_source_syntax_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("bad.bt");
        let core_file = temp_path.join("bad.core");
        fs::write(&source_file, "test := [1 + ].").unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "bad", &core_file, &options);

        assert!(result.is_err());
    }

    #[test]
    fn test_compile_source_missing_file() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("nonexistent.bt");
        let core_file = temp_path.join("nonexistent.core");

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "nonexistent", &core_file, &options);

        assert!(result.is_err());
    }

    #[test]
    fn test_compile_source_sealed_class_violation() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = "Integer subclass: MyInt\n  doubled => self * 2\n";
        let source_file = temp_path.join("sealed.bt");
        let core_file = temp_path.join("sealed.core");
        fs::write(&source_file, source).unwrap();

        // Verify parse and primitive validation produce no errors —
        // so compile_source failure must come from semantic analysis.
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, parse_diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            !parse_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Source should parse without errors"
        );
        let options = beamtalk_core::CompilerOptions::default();
        let prim_diags = beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
        assert!(
            !prim_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Primitive validation should produce no errors"
        );

        let result = compile_source(&source_file, "sealed", &core_file, &options);
        assert!(
            result.is_err(),
            "Sealed class violation should fail compilation via semantic analysis"
        );
    }

    #[test]
    fn test_compile_source_sealed_method_override() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = "Object subclass: Parent\n  sealed frozen => 42\n\nParent subclass: Child\n  frozen => 99\n";
        let source_file = temp_path.join("sealed_method.bt");
        let core_file = temp_path.join("sealed_method.core");
        fs::write(&source_file, source).unwrap();

        // Verify parse and primitive validation produce no errors —
        // so compile_source failure must come from semantic analysis.
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, parse_diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            !parse_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Source should parse without errors"
        );
        let options = beamtalk_core::CompilerOptions::default();
        let prim_diags = beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
        assert!(
            !prim_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Primitive validation should produce no errors"
        );

        let result = compile_source(&source_file, "sealed_method", &core_file, &options);
        assert!(
            result.is_err(),
            "Sealed method override should fail compilation via semantic analysis"
        );
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
    let InitialState = ~{'$beamtalk_class' => 'Test', '__methods__' => ~{}~}~
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

    // Tests for escape_erlang_string
    #[test]
    fn test_escape_erlang_string_empty() {
        assert_eq!(escape_erlang_string(""), "");
    }

    #[test]
    fn test_escape_erlang_string_no_special_chars() {
        assert_eq!(escape_erlang_string("hello"), "hello");
        assert_eq!(escape_erlang_string("foo_bar"), "foo_bar");
        assert_eq!(escape_erlang_string("path/to/file"), "path/to/file");
    }

    #[test]
    fn test_escape_erlang_string_backslashes() {
        assert_eq!(escape_erlang_string("a\\b"), "a\\\\b");
        assert_eq!(escape_erlang_string("\\\\"), "\\\\\\\\");
    }

    #[test]
    fn test_escape_erlang_string_quotes() {
        assert_eq!(escape_erlang_string("a\"b"), "a\\\"b");
        assert_eq!(escape_erlang_string("\"test\""), "\\\"test\\\"");
    }

    #[test]
    fn test_escape_erlang_string_newlines() {
        assert_eq!(escape_erlang_string("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_erlang_string("\r\n"), "\\r\\n");
    }

    #[test]
    fn test_escape_erlang_string_tabs() {
        assert_eq!(escape_erlang_string("col1\tcol2"), "col1\\tcol2");
    }

    #[test]
    fn test_escape_erlang_string_combined() {
        assert_eq!(
            escape_erlang_string("path\\to\\\"file\"\n"),
            "path\\\\to\\\\\\\"file\\\"\\n"
        );
    }

    // Tests for is_valid_module_name
    #[test]
    fn test_is_valid_module_name_valid() {
        assert!(is_valid_module_name("hello"));
        assert!(is_valid_module_name("hello_world"));
        assert!(is_valid_module_name("HelloWorld"));
        assert!(is_valid_module_name("test123"));
        assert!(is_valid_module_name("_private"));
        assert!(is_valid_module_name("a"));
        // ADR 0016: @ is valid in Erlang atoms and used for namespacing
        assert!(is_valid_module_name("bt@counter"));
        assert!(is_valid_module_name("bt@stdlib@integer"));
    }

    #[test]
    fn test_is_valid_module_name_empty() {
        assert!(!is_valid_module_name(""));
    }

    #[test]
    fn test_is_valid_module_name_path_traversal() {
        assert!(!is_valid_module_name(".."));
        assert!(!is_valid_module_name("../secret"));
        assert!(!is_valid_module_name("foo/bar"));
        assert!(!is_valid_module_name("foo\\bar"));
    }

    #[test]
    fn test_is_valid_module_name_special_chars() {
        assert!(!is_valid_module_name("foo-bar"));
        assert!(!is_valid_module_name("foo.bar"));
        assert!(!is_valid_module_name("foo bar"));
    }

    #[test]
    fn test_is_valid_module_name_unicode() {
        assert!(!is_valid_module_name("héllo"));
        assert!(!is_valid_module_name("日本語"));
        assert!(!is_valid_module_name("emoji🎉"));
    }

    // Test write_core_erlang with invalid module name
    #[test]
    fn test_write_core_erlang_invalid_module_name() {
        let temp = TempDir::new().unwrap();
        let output_path = Utf8PathBuf::from_path_buf(temp.path().join("test.core")).unwrap();
        let module = Module::new(Vec::new(), Span::new(0, 0));

        // Empty name
        let result = write_core_erlang(&module, "", &output_path);
        assert!(result.is_err());

        // Path traversal attempt
        let result = write_core_erlang(&module, "../evil", &output_path);
        assert!(result.is_err());

        // Special characters
        let result = write_core_erlang(&module, "foo-bar", &output_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_escript_counter_uniqueness_parallel() {
        use std::collections::HashSet;
        use std::sync::Mutex;

        // Test that ESCRIPT_COUNTER produces unique values even when called from multiple threads
        let values = std::sync::Arc::new(Mutex::new(HashSet::new()));
        let mut handles = vec![];

        // Spawn 10 threads that each fetch 100 counter values
        for _ in 0..10 {
            let values_clone = std::sync::Arc::clone(&values);
            let handle = std::thread::spawn(move || {
                for _ in 0..100 {
                    let counter = ESCRIPT_COUNTER.fetch_add(1, Ordering::Relaxed);
                    let mut set = values_clone.lock().unwrap();
                    // Each counter value should be unique
                    assert!(
                        set.insert(counter),
                        "Counter value {counter} was not unique"
                    );
                }
            });
            handles.push(handle);
        }

        // Wait for all threads to complete
        for handle in handles {
            handle.join().unwrap();
        }

        // Verify we got exactly 1000 unique values
        let final_set = values.lock().unwrap();
        assert_eq!(final_set.len(), 1000, "Expected 1000 unique counter values");
    }
}
