// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BEAM bytecode compiler integration.
//!
//! This module handles compilation of Core Erlang (.core) files to BEAM bytecode (.beam).
//! It supports two backends selected via the `BEAMTALK_COMPILER` environment variable:
//!
//! - **Port** (default): Starts a BEAM node with `beamtalk_build_worker` for in-memory
//!   compilation via `core_scan → core_parse → compile:forms`. No escript needed.
//! - **Escript** (legacy fallback): Invokes the embedded `compile.escript`. Used when
//!   the runtime is not compiled, or when `BEAMTALK_COMPILER=escript`.
//!
//! # Architecture
//!
//! The compilation process:
//! 1. Generate Core Erlang from AST using `beamtalk_codegen::core_erlang`
//! 2. Write .core files to build directory
//! 3. Batch compile .core → .beam via the selected backend
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
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
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

/// Embedded compile.escript for batch compilation.
const COMPILE_ESCRIPT: &str = include_str!("../templates/compile.escript");

/// Atomic counter for generating unique escript filenames.
///
/// This ensures each temporary escript file has a unique name, preventing
/// collisions when multiple compilation processes run in parallel.
static ESCRIPT_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Compiler backend for Core Erlang → BEAM compilation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerBackend {
    /// Port backend: starts a BEAM node with `beamtalk_build_worker`
    /// for in-memory compilation (default, ADR 0022 Phase 3).
    Port,
    /// Escript backend: uses the embedded `compile.escript` (legacy fallback).
    Escript,
}

/// Resolve the compiler backend from the `BEAMTALK_COMPILER` environment variable.
///
/// - `"escript"` → [`CompilerBackend::Escript`] (legacy fallback)
/// - Anything else (including unset) → [`CompilerBackend::Port`] (default)
pub fn resolve_backend() -> CompilerBackend {
    match std::env::var("BEAMTALK_COMPILER").as_deref() {
        Ok("escript") => CompilerBackend::Escript,
        Ok(other) => {
            eprintln!("Warning: unknown BEAMTALK_COMPILER value '{other}', defaulting to port");
            CompilerBackend::Port
        }
        _ => CompilerBackend::Port,
    }
}

/// Sentinel prefix in error messages indicating the Port backend's runtime
/// is not available. Used by `compile_batch` to decide whether to fall back
/// to the escript backend.
const RUNTIME_UNAVAILABLE_PREFIX: &str = "Port backend requires";

/// Check whether an error indicates the Port backend's runtime is unavailable.
fn is_runtime_unavailable_error(err: &miette::Report) -> bool {
    let msg = format!("{err}");
    msg.starts_with(RUNTIME_UNAVAILABLE_PREFIX)
}

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
    #[instrument(skip_all, fields(file_count = core_files.len(), output_dir = %self.output_dir))]
    pub fn compile_batch(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        let backend = resolve_backend();
        debug!(
            "Starting batch compilation of {} Core Erlang files (backend: {:?})",
            core_files.len(),
            backend,
        );

        // ADR 0125 §3.1: both backends boot `erl` on the toolchain's `.beam`
        // files, which do not load below the support window's floor.
        crate::commands::build_stamp::require_supported_otp()?;

        // Ensure output directory exists before dispatching to either backend.
        std::fs::create_dir_all(&self.output_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create output directory '{}'", self.output_dir))?;

        match backend {
            CompilerBackend::Port => {
                // Try Port backend, fall back to escript if runtime not available
                match self.compile_batch_port(core_files) {
                    Ok(result) => Ok(result),
                    Err(e) if is_runtime_unavailable_error(&e) => {
                        info!("Port backend unavailable, falling back to escript");
                        self.compile_batch_escript(core_files)
                    }
                    Err(e) => Err(e),
                }
            }
            CompilerBackend::Escript => self.compile_batch_escript(core_files),
        }
    }

    /// Compile via the Port backend: starts a BEAM node with `beamtalk_build_worker`.
    ///
    /// Uses in-memory compilation via `core_scan → core_parse → compile:forms`
    /// (no escript needed). Requires Erlang/OTP and the runtime to be compiled.
    #[instrument(skip_all, fields(file_count = core_files.len()))]
    fn compile_batch_port(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        use beamtalk_cli::repl_startup;

        info!("Using Port backend for Core Erlang → BEAM compilation");

        // Find runtime and build -pa paths (canonicalize for absolute paths,
        // since the BEAM node runs from temp_dir).
        // Output directory is guaranteed to exist (created in compile_batch).
        let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().map_err(|_| {
            miette::miette!(
                "Port backend requires the Beamtalk runtime.\n\
                     Falling back to escript: set BEAMTALK_COMPILER=escript\n\
                     Or build the runtime: cd runtime && rebar3 compile"
            )
        })?;

        let runtime_dir = runtime_dir
            .canonicalize()
            .into_diagnostic()
            .wrap_err_with(|| {
                format!("{RUNTIME_UNAVAILABLE_PREFIX} a resolvable runtime directory")
            })?;

        let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

        // Check that the compiler app is compiled and includes the build worker
        if !repl_startup::has_beam_files(&paths.compiler_ebin) {
            return Err(miette::miette!(
                "{} compiled compiler at {}.\n\
                 Build it with: cd runtime && rebar3 compile\n\
                 Or use escript backend: BEAMTALK_COMPILER=escript",
                RUNTIME_UNAVAILABLE_PREFIX,
                paths.compiler_ebin.display()
            ));
        }
        if !paths
            .compiler_ebin
            .join("beamtalk_build_worker.beam")
            .exists()
        {
            return Err(miette::miette!(
                "{} beamtalk_build_worker module.\n\
                 Rebuild: cd runtime && rebar3 compile\n\
                 Or use escript backend: BEAMTALK_COMPILER=escript",
                RUNTIME_UNAVAILABLE_PREFIX,
            ));
        }

        // Build -pa arguments: only compiler ebin needed (contains beamtalk_build_worker)
        let compiler_path = to_forward_slash(&paths.compiler_ebin.display().to_string());

        let pa_args = vec!["-pa".to_string(), compiler_path];

        // Start BEAM node with beamtalk_build_worker
        // -mode minimal: only loads kernel+stdlib, skips scanning other -pa dirs at boot
        // -boot no_dot_erlang: skips .erlang config file
        debug!("Spawning BEAM node with beamtalk_build_worker");
        let mut child = spawn_build_worker_node(&pa_args)
            .wrap_err("Failed to start BEAM node for compilation.\nIs Erlang/OTP installed?")?;

        // From here, the protocol is identical to the escript backend
        self.drive_compilation_protocol(&mut child, core_files)
    }

    /// Compile via the legacy escript backend.
    #[instrument(skip_all, fields(file_count = core_files.len()))]
    fn compile_batch_escript(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        // Check if escript is available
        check_escript_available()?;

        // Output directory is guaranteed to exist (created in compile_batch).

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

        let result = self.drive_compilation_protocol(&mut child, core_files);

        // Clean up temporary escript file
        if let Err(err) = std::fs::remove_file(&escript_path) {
            warn!(
                "Failed to remove temporary escript file '{}': {}",
                escript_path.display(),
                err
            );
        }

        result
    }

    /// Drive the stdin/stdout compilation protocol shared by both backends.
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

/// Format the Erlang term input for the compilation protocol.
///
/// Produces `{OutputDir, [CoreFile1, CoreFile2, ...]}.\n` with absolute,
/// Erlang-escaped paths. Uses absolute paths since escript runs from `temp_dir`.
fn format_compilation_input(
    output_dir: &Utf8PathBuf,
    core_files: &[Utf8PathBuf],
) -> Result<String> {
    let abs_output_dir = std::fs::canonicalize(output_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to canonicalize output directory '{output_dir}'"))?;
    let abs_output_dir_str = abs_output_dir.to_string_lossy();

    let core_files_str: Vec<String> = core_files
        .iter()
        .map(|p| {
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

    let entries = std::fs::read_dir(&native_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read native directory '{native_dir}'"))?;

    let mut module_names = Vec::new();
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if path.is_file()
            && path.extension().is_some_and(|ext| ext == "erl")
            && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
        {
            module_names.push(stem.to_string());
        }
    }

    module_names.sort();
    Ok(module_names)
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

// OTP/dependency Erlang type-spec extraction lives in the
// `beamtalk_cli` lib crate so `beamtalk-mcp` can share it — mirrors
// where `dependency_classes`/`build_layout`/`manifest` also live.
// Re-exported here so existing `beam_compiler::X` references
// throughout this crate keep working unchanged.
pub use beamtalk_cli::native_type_specs::{
    OtpDiscovery, current_spec_mapping_stamp, discover_dependency_beam_files,
    discover_otp_beam_files, discover_otp_version, extract_beam_specs, extract_beam_specs_tiered,
    load_type_cache_registry, shared_otp_cache_dir, spawn_build_worker_node,
};

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

        // Check that output directory was created and compilation succeeded
        assert!(output_dir.exists());
        result.expect("compile_batch should succeed when escript is available");
    }

    /// A deliberately-malformed Core Erlang module — `foo/1`
    /// references `State`, which is never a parameter or let-bound — must
    /// fail with a readable `core_lint` message naming the unbound
    /// variable, actually reaching this process's stderr (and therefore
    /// this error message), not a raw term dump or a silently-dropped
    /// stdout line (`compile.escript`'s `worker_loop/2` used to rely on
    /// `report_errors`, which prints via the compiling process's default
    /// group leader — verified empirically to land on stdout, invisible to
    /// `read_compilation_output`'s protocol-marker-only parser). Exercises
    /// the same acceptance criterion the Port-backend `EUnit` tests
    /// (`beamtalk_build_worker_tests`/`beamtalk_compiler_server_tests`
    /// `compile_core_erlang_unbound_var_test`) cover for the other backend —
    /// together they demonstrate the two backends stay in parity.
    #[test]
    fn test_compile_batch_escript_core_lint_unbound_var() {
        if check_escript_available().is_err() {
            println!("Skipping test - escript not installed");
            return;
        }

        let temp = TempDir::new().unwrap();
        let output_dir = Utf8PathBuf::from_path_buf(temp.path().join("build")).unwrap();
        fs::create_dir_all(&output_dir).unwrap();
        let compiler = BeamCompiler::new(output_dir);

        let core_content = r"module 'bt_cli_unbound_var' ['foo'/1]
  attributes []
  'foo'/1 = fun (X) ->
    let Y = call 'erlang':'+' (X, State)
    in Y
end
";

        let core_file = Utf8PathBuf::from_path_buf(temp.path().join("bad.core")).unwrap();
        fs::write(&core_file, core_content).unwrap();

        let result = compiler.compile_batch_escript(&[core_file]);
        let err = result.expect_err("malformed Core Erlang must fail to compile");
        let message = format!("{err}");
        assert!(
            message.contains("unbound variable"),
            "expected a readable core_lint message, got: {message}"
        );
        assert!(
            message.contains("'State'"),
            "expected the offending variable name in the message, got: {message}"
        );
        assert!(
            !message.contains("{unbound_var,"),
            "expected readable prose, not a raw term dump: {message}"
        );
    }

    /// Returns every `"..."` substring on `line` (assumes no escaped
    /// quotes inside — true for every literal these conformance tests
    /// extract). `line.split('"')` alternates outside/inside segments
    /// starting outside, so the inside ones are every other element from
    /// index 1.
    fn quoted_strings(line: &str) -> Vec<&str> {
        line.split('"').skip(1).step_by(2).collect()
    }

    fn line_containing<'a>(source: &'a str, needle: &str) -> &'a str {
        source
            .lines()
            .find(|l| l.contains(needle))
            .unwrap_or_else(|| panic!("no line containing {needle:?} in source"))
    }

    /// Concatenates a `"lit1"\n"lit2"\n...` Erlang string-literal
    /// concatenation: starting at the first line containing `marker`,
    /// every immediately-following line that is itself a bare quoted
    /// segment (trimmed, starts and ends with `"`) is appended; stops at
    /// the first line that isn't.
    fn erlang_string_concat_from(source: &str, marker: &str) -> String {
        let start = source
            .find(marker)
            .unwrap_or_else(|| panic!("marker {marker:?} not found in source"));
        let mut out = String::new();
        for line in source[start..].lines() {
            let trimmed = line.trim();
            if trimmed.len() >= 2 && trimmed.starts_with('"') && trimmed.ends_with('"') {
                out.push_str(&trimmed[1..trimmed.len() - 1]);
            } else if !out.is_empty() {
                break;
            }
        }
        out
    }

    /// Review finding (#3997): `compile.escript`'s `print_bug_header/0` and
    /// its `demangle`-via-`re:replace` in `print_messages/2` are hand-copied
    /// from `beamtalk_compile_diagnostics:bug_header/0`/
    /// `demangle_class_methods/1` (the escript cannot depend on that
    /// module — see its own moduledoc), tagged only with a "mirrors X"
    /// comment and no test enforcing the two stay identical. This
    /// conformance test is that enforcement: it extracts both copies of
    /// each literal from the actual source files (not a third hand-copied
    /// expectation) and asserts they are byte-for-byte equal, so an edit to
    /// either copy that isn't mirrored in the other fails this test instead
    /// of silently drifting.
    #[test]
    fn test_escript_bug_header_and_demangle_match_beamtalk_compile_diagnostics() {
        let diagnostics_erl_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../runtime/apps/beamtalk_compiler/src/beamtalk_compile_diagnostics.erl");
        let diagnostics_erl = fs::read_to_string(&diagnostics_erl_path)
            .unwrap_or_else(|e| panic!("failed to read {diagnostics_erl_path}: {e}"));

        // bug_header/0 vs print_bug_header/0.
        let marker = "\"Beamtalk compiler bug: code generation produced Core Erlang that \"";
        let erl_header = erlang_string_concat_from(&diagnostics_erl, marker);
        let escript_header = erlang_string_concat_from(COMPILE_ESCRIPT, marker);
        assert!(
            !erl_header.is_empty(),
            "failed to extract bug_header/0's text"
        );
        assert_eq!(
            erl_header, escript_header,
            "compile.escript's print_bug_header/0 has drifted from \
             beamtalk_compile_diagnostics:bug_header/0 — keep them byte-identical"
        );

        // demangle_class_methods/1 vs print_messages/2's inline re:replace.
        let needle = "'class_(";
        let erl_line = line_containing(&diagnostics_erl, needle);
        let escript_line = line_containing(COMPILE_ESCRIPT, needle);
        let erl_quoted = quoted_strings(erl_line);
        let escript_quoted = quoted_strings(escript_line);
        assert_eq!(
            erl_quoted.len(),
            2,
            "expected exactly the regex pattern + replacement on beamtalk_compile_diagnostics.erl's re:replace line, got: {erl_line:?}"
        );
        assert_eq!(
            escript_quoted.len(),
            2,
            "expected exactly the regex pattern + replacement on compile.escript's re:replace line, got: {escript_line:?}"
        );
        assert_eq!(
            erl_quoted, escript_quoted,
            "compile.escript's demangle re:replace pattern/replacement has drifted from \
             beamtalk_compile_diagnostics:demangle_class_methods/1 — keep them byte-identical"
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
