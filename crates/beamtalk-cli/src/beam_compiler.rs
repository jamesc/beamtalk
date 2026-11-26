// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BEAM bytecode compiler integration.
//!
//! This module handles compilation of Core Erlang (.core) files to BEAM bytecode (.beam).
//! A BEAM node running `beamtalk_build_worker` compiles in memory via
//! `core_scan → core_parse → compile:forms` (ADR 0022). There is no other backend: if the
//! runtime (the `beamtalk_compiler` app) is missing, compilation fails with an
//! actionable error rather than falling back.
//!
//! # Architecture
//!
//! The compilation process:
//! 1. Generate Core Erlang from AST using `beamtalk_codegen::core_erlang`
//! 2. Write .core files to build directory
//! 3. Batch compile .core → .beam via the build worker
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

use crate::commands::util::to_forward_slash;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::io::{BufRead, BufReader, Write};
use std::sync::mpsc;
use std::thread;
use tracing::{debug, info, instrument, warn};

mod compile_source;
pub use compile_source::{
    ClassHierarchyContext, CompileContext, ProvisionSink, compile_source,
    write_core_erlang_with_bindings,
};
pub(crate) use compile_source::{
    analyse_source_with_bindings, compile_source_with_bindings, diagnostics_are_errors,
};

pub(crate) use beamtalk_codegen::core_erlang::escape_erlang_string;

/// Validates that a module name contains only safe identifier characters.
///
/// Module names should only contain ASCII alphanumeric characters and underscores
/// to prevent path traversal vulnerabilities.
pub(crate) fn is_valid_module_name(name: &str) -> bool {
    // ADR 0016: @ is legal in Erlang unquoted atoms and used for namespacing
    !name.is_empty()
        && name
            .chars()
            .all(|c| c == '_' || c == '@' || c.is_ascii_alphanumeric())
}

fn validate_module_name(name: &str) -> Result<()> {
    if !is_valid_module_name(name) {
        miette::bail!(
            "Invalid module name '{}': must be non-empty and contain only alphanumeric characters, underscores, and @",
            name
        );
    }
    Ok(())
}

fn write_core_erlang_bytes(bytes: impl AsRef<[u8]>, output_path: &Utf8Path) -> Result<()> {
    std::fs::write(output_path, bytes)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))
}

/// BEAM bytecode compiler.
///
/// Handles compilation of Core Erlang to BEAM bytecode through the
/// `beamtalk_build_worker` BEAM node.
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
    /// This method batch compiles multiple .core files with one
    /// `beamtalk_build_worker` BEAM node.
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
    /// - The Beamtalk runtime (`beamtalk_compiler` app / `beamtalk_build_worker`) is missing
    /// - The BEAM node fails to start or execute
    /// - Compilation errors occur in any module
    /// - stdin, stdout, or stderr cannot be captured from the worker process
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
    #[instrument(skip_all, fields(file_count = core_files.len(), output_dir = %self.output_dir))]
    pub fn compile_batch(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        use beamtalk_cli::repl_startup;

        debug!(
            "Starting batch compilation of {} Core Erlang files",
            core_files.len(),
        );

        // ADR 0125 §3.1: the build worker boots `erl` on the toolchain's `.beam`
        // files, which do not load below the support window's floor.
        crate::commands::build_stamp::require_supported_otp()?;

        // Ensure the output directory exists before starting the worker.
        std::fs::create_dir_all(&self.output_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create output directory '{}'", self.output_dir))?;

        info!("Compiling Core Erlang → BEAM via beamtalk_build_worker");

        // Find runtime and build -pa paths (canonicalize for absolute paths,
        // since the BEAM node runs from temp_dir).
        let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().map_err(|e| {
            runtime_missing_error("the Beamtalk runtime directory", &format!("Details: {e}"))
        })?;

        let runtime_dir = beamtalk_core::paths::canonicalize(&runtime_dir)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!(
                    "Failed to resolve the Beamtalk runtime directory '{}'",
                    runtime_dir.display()
                )
            })?;

        let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
        let compiler_ebin = require_build_worker(&paths.compiler_ebin)?;

        // Only the compiler ebin is needed (it contains beamtalk_build_worker).
        let pa_args = vec![
            "-pa".to_string(),
            to_forward_slash(&compiler_ebin.display().to_string()),
        ];

        // -mode minimal: only loads kernel+stdlib, skips scanning other -pa dirs at boot
        // -boot no_dot_erlang: skips .erlang config file
        debug!("Spawning BEAM node with beamtalk_build_worker");
        let mut child = beamtalk_package::native_type_specs::spawn_build_worker_node(&pa_args)
            .wrap_err("Failed to start BEAM node for compilation.\nIs Erlang/OTP installed?")?;

        self.drive_compilation_protocol(&mut child, core_files)
    }

    /// Drive the stdin/stdout compilation protocol with the build worker.
    ///
    /// Protocol:
    ///   INPUT:  Erlang term: `{OutputDir, [CoreFile1, CoreFile2, ...]}`
    ///   OUTPUT: `beamtalk-compile-module:<name>` per module, then
    ///           `beamtalk-compile-result-ok` or `beamtalk-compile-result-error`
    fn drive_compilation_protocol(
        &self,
        child: &mut std::process::Child,
        core_files: &[Utf8PathBuf],
    ) -> Result<Vec<Utf8PathBuf>> {
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture compiler stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture compiler stdout"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture compiler stderr"))?;

        // Send formatted input to compiler and close stdin
        let input = format_compilation_input(&self.output_dir, core_files)?;
        stdin
            .write_all(input.as_bytes())
            .into_diagnostic()
            .wrap_err("Failed to write to compiler stdin")?;
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
        let (compiled_modules, compilation_ok) =
            self.read_compilation_output(BufReader::new(stdout))?;

        // Wait for stderr thread to complete and collect error messages
        let error_messages = stderr_rx.recv().unwrap_or_else(|_| Vec::new());
        let _ = stderr_thread.join();

        // Wait for process to complete
        let status = child
            .wait()
            .into_diagnostic()
            .wrap_err("Failed to wait for compiler process")?;

        // Check results
        if !status.success() || !compilation_ok {
            let error_msg = if error_messages.is_empty() {
                "Compilation failed".to_string()
            } else {
                format!("Compilation failed:\n{}", error_messages.join("\n"))
            };
            debug!("Compilation failed with status: {:?}", status);
            miette::bail!("{}", error_msg);
        }

        debug!(
            beam_count = compiled_modules.len(),
            "Compilation completed successfully"
        );
        Ok(compiled_modules)
    }

    /// Read and parse compilation output from the compiler's stdout.
    ///
    /// Parses `beamtalk-compile-module:<name>` lines to collect compiled module
    /// paths and the final `beamtalk-compile-result-ok` / `beamtalk-compile-result-error`
    /// status line.
    fn read_compilation_output(
        &self,
        reader: BufReader<std::process::ChildStdout>,
    ) -> Result<(Vec<Utf8PathBuf>, bool)> {
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

        Ok((compiled_modules, compilation_ok))
    }
}

/// Build the one actionable error for a missing Port runtime: what is
/// missing, where it was looked for, and how to install or build it.
fn runtime_missing_error(what: &str, detail: &str) -> miette::Report {
    miette::miette!(
        "Cannot compile Beamtalk: {what} is missing.\n\
         {detail}\n\
         To fix it:\n\
         - installed copy: reinstall Beamtalk (the runtime lives in <prefix>/lib/beamtalk/lib/beamtalk_compiler/ebin)\n\
         - source checkout: run `just build` (or `cd runtime && rebar3 compile`)\n\
         - or set BEAMTALK_RUNTIME_DIR to a runtime directory that contains rebar.config"
    )
}

/// Check that `compiler_ebin` holds the compiled compiler app and its
/// `beamtalk_build_worker` module; returns the directory on success.
fn require_build_worker(compiler_ebin: &std::path::Path) -> Result<&std::path::Path> {
    let looked_in = format!("Looked in: {}", compiler_ebin.display());
    if !beamtalk_cli::repl_startup::has_beam_files(compiler_ebin) {
        return Err(runtime_missing_error(
            "the compiled beamtalk_compiler app",
            &looked_in,
        ));
    }
    if !compiler_ebin.join("beamtalk_build_worker.beam").exists() {
        return Err(runtime_missing_error(
            "the beamtalk_build_worker module",
            &looked_in,
        ));
    }
    Ok(compiler_ebin)
}

/// Format the Erlang term input for the compilation protocol.
///
/// Produces `{OutputDir, [CoreFile1, CoreFile2, ...]}.\n` with absolute,
/// Erlang-escaped paths. Uses absolute paths since the worker node runs from `temp_dir`.
fn format_compilation_input(
    output_dir: &Utf8PathBuf,
    core_files: &[Utf8PathBuf],
) -> Result<String> {
    let abs_output_dir = beamtalk_core::paths::canonicalize(output_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to canonicalize output directory '{output_dir}'"))?;
    let abs_output_dir_str = abs_output_dir.to_string_lossy();

    let core_files_str: Vec<String> = core_files
        .iter()
        .map(|p| {
            match beamtalk_core::paths::canonicalize(p.as_std_path()) {
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

    Ok(format!(
        "{{\"{}\",[{}]}}.\n",
        escape_erlang_string(&abs_output_dir_str),
        core_files_str.join(",")
    ))
}

/// Discover native Erlang module names from a package's `native/` directory
/// without compiling them.
///
/// ADR 0072 Phase 1: Used for collision detection across packages before
/// compilation begins. Returns the list of module names (file stems of
/// `native/*.erl` files), or an empty vec if no `native/` directory exists.
///
/// # Errors
///
/// Returns an error if the `native/` directory exists but cannot be read.
pub fn discover_native_modules(project_root: &Utf8Path) -> Result<Vec<String>> {
    let native_dir = project_root.join("native");

    if !native_dir.exists() || !native_dir.is_dir() {
        return Ok(Vec::new());
    }

    beamtalk_cli::path_util::erl_module_names_in(&native_dir)
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
/// metadata for `CompiledMethod` introspection.
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
    validate_module_name(module_name)?;

    let (core_erlang, verifier_diagnostics) =
        beamtalk_codegen::core_erlang::generate_module_surfacing_verifier(
            module,
            beamtalk_codegen::core_erlang::CodegenOptions::new(module_name)
                .with_source_opt(source_text)
                // ADR 0098 Phase 3: bake the producing-toolchain identity into __beamtalk_meta.
                .with_provenance(
                    env!("BEAMTALK_VERSION"),
                    crate::commands::build_stamp::current_otp_version(),
                ),
        )
        .into_diagnostic()
        .wrap_err("Failed to generate Core Erlang")?;

    // ADR 0111 amendment (BT-3724): a verifier finding is a warning; the
    // output is still written.
    crate::diagnostic::print_diagnostics_text(
        &verifier_diagnostics,
        output_path.as_str(),
        source_text.unwrap_or_default(),
        &beamtalk_core::CompilerOptions::default(),
    );

    write_core_erlang_bytes(&core_erlang, output_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::ast::Module;
    use beamtalk_core::source_analysis::Span;
    use std::fs;
    use tempfile::TempDir;

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

    /// A missing runtime is one actionable error: what is missing, where it
    /// was looked for, and how to fix it (BT-5198; no escript fallback).
    #[test]
    fn missing_compiler_runtime_error_names_what_where_and_how() {
        let temp = TempDir::new().unwrap();
        let ebin = temp.path().join("lib/beamtalk_compiler/ebin");
        let err = require_build_worker(&ebin).expect_err("no ebin must be an error");
        let msg = format!("{err}");
        assert!(msg.contains("beamtalk_compiler app"), "what: {msg}");
        assert!(msg.contains(&ebin.display().to_string()), "where: {msg}");
        assert!(msg.contains("just build"), "how (checkout): {msg}");
        assert!(msg.contains("reinstall"), "how (install): {msg}");
        assert!(
            !msg.to_lowercase().contains("escript"),
            "no fallback hint: {msg}"
        );

        // Compiled app present but without the build worker: same shape.
        std::fs::create_dir_all(&ebin).unwrap();
        std::fs::write(ebin.join("beamtalk_compiler.beam"), b"x").unwrap();
        let msg = format!("{}", require_build_worker(&ebin).unwrap_err());
        assert!(msg.contains("beamtalk_build_worker module"), "{msg}");
        assert!(msg.contains(&ebin.display().to_string()), "{msg}");

        // With the worker present it passes.
        std::fs::write(ebin.join("beamtalk_build_worker.beam"), b"x").unwrap();
        assert!(require_build_worker(&ebin).is_ok());
    }

    /// BT-5198: the retired backend selector is ignored, not honoured. With
    /// the old `escript` value set and no runtime available, `compile_batch`
    /// must fail with the missing-runtime error instead of falling back to a
    /// legacy backend (which used to compile successfully here).
    #[test]
    #[serial_test::serial(env_var)]
    fn retired_backend_env_var_does_not_enable_a_fallback() {
        let temp = TempDir::new().unwrap();
        // A runtime dir with rebar.config but nothing built under it.
        std::fs::write(temp.path().join("rebar.config"), b"{deps, []}.\n").unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("out")).unwrap();
        let core = Utf8PathBuf::from_path_buf(temp.path().join("m.core")).unwrap();
        fs::write(&core, "module 'm' [] attributes []\nend\n").unwrap();

        let saved: Vec<_> = ["BEAMTALK_RUNTIME_DIR", "BEAMTALK_COMPILER"]
            .iter()
            .map(|k| (*k, std::env::var_os(k)))
            .collect();
        // SAFETY: serialized via #[serial(env_var)]; restored below.
        unsafe {
            std::env::set_var("BEAMTALK_RUNTIME_DIR", temp.path());
            std::env::set_var("BEAMTALK_COMPILER", "escript");
        }
        let result = BeamCompiler::new(out).compile_batch(&[core]);
        for (k, v) in saved {
            // SAFETY: see above.
            unsafe {
                match v {
                    Some(v) => std::env::set_var(k, v),
                    None => std::env::remove_var(k),
                }
            }
        }

        let msg = format!("{}", result.expect_err("must not fall back to escript"));
        assert!(msg.contains("beamtalk_compiler app"), "{msg}");
        assert!(msg.contains("just build"), "{msg}");
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

    // ---- ADR 0072 Phase 1: discover_native_modules tests ----

    #[test]
    fn test_discover_native_modules_no_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let result = discover_native_modules(&project_root).unwrap();
        assert!(result.is_empty(), "No native/ dir should return empty vec");
    }

    #[test]
    fn test_discover_native_modules_with_erl_files() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        std::fs::create_dir(&native_dir).unwrap();

        std::fs::write(native_dir.join("alpha.erl"), "-module(alpha).").unwrap();
        std::fs::write(native_dir.join("beta.erl"), "-module(beta).").unwrap();
        std::fs::write(native_dir.join("readme.md"), "# Not an erl file").unwrap();

        let result = discover_native_modules(&project_root).unwrap();
        assert_eq!(result, vec!["alpha", "beta"]);
    }

    #[test]
    fn test_discover_native_modules_empty_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        std::fs::create_dir(&native_dir).unwrap();

        let result = discover_native_modules(&project_root).unwrap();
        assert!(
            result.is_empty(),
            "Empty native/ dir should return empty vec"
        );
    }
}
