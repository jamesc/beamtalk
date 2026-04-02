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
//! 1. Generate Core Erlang from AST using `beamtalk_core::erlang`
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

use beamtalk_core::semantic_analysis::type_checker::{
    NativeTypeRegistry, is_specs_line, is_specs_result_error, is_specs_result_ok, parse_specs_line,
};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::SystemTime;
use tracing::{debug, error, info, instrument, warn};

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

/// Escapes a string for use in an Erlang term.
///
/// This escapes backslashes, quotes, and control characters to prevent
/// injection attacks and ensure valid Erlang term syntax when constructing
/// Erlang terms from file paths.
pub(crate) fn escape_erlang_string(s: &str) -> String {
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
    #[instrument(skip_all, fields(file_count = core_files.len(), output_dir = %self.output_dir))]
    pub fn compile_batch(&self, core_files: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>> {
        let backend = resolve_backend();
        debug!(
            "Starting batch compilation of {} Core Erlang files (backend: {:?})",
            core_files.len(),
            backend,
        );

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
        #[cfg(windows)]
        let compiler_path = paths.compiler_ebin.to_string_lossy().replace('\\', "/");
        #[cfg(not(windows))]
        let compiler_path = paths.compiler_ebin.display().to_string();

        let pa_args = vec!["-pa".to_string(), compiler_path];

        // Start BEAM node with beamtalk_build_worker
        // -mode minimal: only loads kernel+stdlib, skips scanning other -pa dirs at boot
        // -boot no_dot_erlang: skips .erlang config file
        debug!("Spawning BEAM node with beamtalk_build_worker");
        let temp_dir = std::env::temp_dir();
        let mut child = Command::new("erl")
            .arg("-noshell")
            .arg("-mode")
            .arg("minimal")
            .arg("-boot")
            .arg("no_dot_erlang")
            // Redirect OTP default logger to stderr (BT-1431). Without this,
            // logger output goes to stdout and mixes into the compilation
            // protocol, causing parse failures or ugly error messages.
            .arg("-kernel")
            .arg("logger")
            .arg(beamtalk_cli::repl_startup::KERNEL_LOGGER_STDERR)
            .args(&pa_args)
            .arg("-s")
            .arg("beamtalk_build_worker")
            .arg("main")
            .current_dir(&temp_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .into_diagnostic()
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
        if path.is_file() && path.extension().is_some_and(|ext| ext == "erl") {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                module_names.push(stem.to_string());
            }
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
    let core_erlang = beamtalk_core::erlang::generate_module(
        module,
        beamtalk_core::erlang::CodegenOptions::new(module_name).with_source_opt(source_text),
    )
    .into_diagnostic()
    .wrap_err("Failed to generate Core Erlang")?;

    // Write to file
    std::fs::write(output_path, core_erlang)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))?;

    Ok(())
}

/// Cross-file class hierarchy data needed during compilation.
///
/// Groups the three class hierarchy indexes that are threaded through the
/// compilation pipeline — eliminating repeated parameter triples in
/// `compile_file`, `compile_source_with_bindings`, and
/// `write_core_erlang_with_bindings`.
///
/// **DDD Context:** Compilation
#[derive(Debug, Clone, Default)]
pub struct ClassHierarchyContext {
    /// Maps Beamtalk class names to their compiled Erlang module names.
    /// E.g., `"AppSup"` → `"bt@my_app@supervision@app_sup"`.
    pub class_module_index: std::collections::HashMap<String, String>,
    /// Maps class names to their direct superclass names.
    /// E.g., `"MyChild"` → `"MyParent"`.
    pub class_superclass_index: std::collections::HashMap<String, String>,
    /// Full `ClassInfo` entries from all source files in the compilation unit.
    /// Injected into the type checker's hierarchy during Pass 2 so cross-file
    /// method resolution works without reading BEAM files.
    pub pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
}

/// Compilation context bundling hierarchy data with dependency resolution settings.
///
/// Groups the `ClassHierarchyContext`, optional dependency registry, and
/// strict-deps flag that are threaded through `compile_file` and
/// `compile_source_with_bindings`. Avoids passing these as separate parameters.
///
/// **DDD Context:** Compilation
#[derive(Debug, Default)]
pub struct CompileContext<'a> {
    /// Cross-file class hierarchy indexes.
    pub hierarchy: ClassHierarchyContext,
    /// Optional dependency registry for cross-package collision detection.
    pub dep_registry: Option<&'a beamtalk_core::semantic_analysis::DependencyRegistry>,
    /// Whether to promote transitive dependency usage warnings to errors.
    pub strict_deps: bool,
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
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
    hierarchy: &ClassHierarchyContext,
    source: Option<(&str, Option<&str>)>,
) -> Result<()> {
    if !is_valid_module_name(module_name) {
        miette::bail!(
            "Invalid module name '{}': must be non-empty and contain only alphanumeric characters, underscores, and @",
            module_name
        );
    }

    let (source_text, source_path) = match source {
        Some((text, path)) => (Some(text), path),
        None => (None, None),
    };
    let core_erlang = beamtalk_core::erlang::generate_module(
        module,
        beamtalk_core::erlang::CodegenOptions::new(module_name)
            .with_bindings(bindings.clone())
            .with_source_opt(source_text)
            .with_workspace_mode(options.workspace_mode)
            .with_stdlib_mode(options.stdlib_mode)
            .with_class_module_index(hierarchy.class_module_index.clone())
            .with_class_superclass_index(hierarchy.class_superclass_index.clone())
            .with_source_path_opt(source_path)
            .with_class_hierarchy(hierarchy.pre_loaded_classes.clone()),
    )
    .into_diagnostic()
    .wrap_err("Failed to generate Core Erlang")?;

    std::fs::write(output_path, core_erlang)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write Core Erlang to '{output_path}'"))?;

    Ok(())
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
        &CompileContext::default(),
        None,
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
#[allow(clippy::too_many_lines)]
#[instrument(skip_all, fields(path = %source_path, module = module_name))]
pub(crate) fn compile_source_with_bindings(
    source_path: &Utf8Path,
    module_name: &str,
    core_output: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
    ctx: &CompileContext<'_>,
    cached_ast: Option<crate::commands::build::CachedAst>,
) -> Result<()> {
    use crate::diagnostic::CompileDiagnostic;

    debug!("Compiling module '{}' with bindings", module_name);

    // BT-1544: Reuse pre-parsed source + AST from Pass 1 when available,
    // otherwise read and parse from disk (single-file mode, REPL, etc.).
    let (source, module, mut diagnostics) = if let Some(cached) = cached_ast {
        debug!("Using cached AST from Pass 1 for '{}'", source_path);
        // Reuse parse diagnostics from Pass 1 so syntax errors are still reported.
        (cached.source, cached.module, cached.diagnostics)
    } else {
        // Read source file
        let source = std::fs::read_to_string(source_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read file '{source_path}'"))?;

        // Lex and parse
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        (source, module, diagnostics)
    };

    // Run @primitive validation (ADR 0007)
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, options,
        );
    diagnostics.extend(primitive_diags);

    // Run semantic analysis (BT-401: sealed enforcement, undefined vars, etc.)
    // Also includes single-class-per-file validation (BT-349)
    // BT-791: Pass options so stdlib_mode gates sealed-superclass exemptions.
    // BT-1523: Inject cross-file class metadata so the type checker can resolve
    // methods from parent classes defined in other files. Filter out classes
    // from the current file to avoid duplicates (they'll be added by build()).
    let cross_file_classes =
        beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
            &ctx.hierarchy.pre_loaded_classes,
            &module,
        );
    let analysis_result = beamtalk_core::semantic_analysis::analyse_with_options_and_classes(
        &module,
        options,
        cross_file_classes.clone(),
    );
    diagnostics.extend(analysis_result.diagnostics);

    // BT-1732: Enrich unresolved class warnings with dependency package hints.
    // When a class is unresolved but exists in a declared dependency's class_module_index,
    // update the hint to suggest the dependency package.
    if let Some(registry) = ctx.dep_registry {
        for diag in &mut diagnostics {
            if diag.category
                == Some(beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass)
            {
                // Extract class name from message "Unresolved class `ClassName`"
                if let Some(class_name) = diag
                    .message
                    .strip_prefix("Unresolved class `")
                    .and_then(|s| s.strip_suffix('`'))
                {
                    if let Some(exports) = registry.lookup(class_name) {
                        if let Some(export) = exports.first() {
                            diag.hint = Some(
                                format!(
                                    "Did you mean `{class_name}` from dependency '{}'? \
                                     Ensure the dependency is declared in beamtalk.toml.",
                                    export.package
                                )
                                .into(),
                            );
                        }
                    }
                }
            }
        }
    }

    // BT-738: Warn when user code shadows a stdlib class name.
    // Only applies to non-stdlib compilation (stdlib defines these names legitimately).
    if !options.stdlib_mode {
        let mut stdlib_shadow_diags = Vec::new();
        beamtalk_core::semantic_analysis::check_stdlib_name_shadowing(
            &module,
            &mut stdlib_shadow_diags,
        );
        diagnostics.extend(stdlib_shadow_diags);
    }

    // BT-1653 / ADR 0070 Phase 3: Cross-package class collision detection.
    // When dependencies are loaded, check for ambiguous unqualified class
    // references and stdlib name reservation violations.
    if let Some(registry) = ctx.dep_registry {
        beamtalk_core::semantic_analysis::check_collision_at_use_sites(
            &module,
            registry,
            &mut diagnostics,
        );

        // BT-1654 / ADR 0070 Phase 3: Warn on transitive dependency usage.
        // When strict-deps is true, this promotes the warning to an error.
        beamtalk_core::semantic_analysis::check_transitive_dep_usage(
            &module,
            registry,
            ctx.strict_deps,
            &mut diagnostics,
        );
    }

    // BT-782: Apply @expect directives to suppress matching diagnostics.
    beamtalk_core::queries::diagnostic_provider::apply_expect_directives(&module, &mut diagnostics);

    // Check for errors (and optionally treat warnings/hints as errors).
    // Deprecation-category warnings (BT-1529) and structural validation warnings
    // (BT-1726: UnresolvedClass, UnresolvedFfi, ArityMismatch) are excluded from
    // warnings-as-errors because they can produce false positives when compiling
    // single files that reference classes, FFI modules, or arities defined in
    // other compilation units.
    let has_errors = diagnostics.iter().any(|d| {
        d.severity == beamtalk_core::source_analysis::Severity::Error
            || (options.warnings_as_errors
                && matches!(
                    d.severity,
                    beamtalk_core::source_analysis::Severity::Warning
                        | beamtalk_core::source_analysis::Severity::Hint
                )
                && !matches!(
                    d.category,
                    Some(
                        beamtalk_core::source_analysis::DiagnosticCategory::Deprecation
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedFfi
                            | beamtalk_core::source_analysis::DiagnosticCategory::ArityMismatch
                    )
                ))
    });

    if !diagnostics.is_empty() {
        debug!(
            diagnostic_count = diagnostics.len(),
            "Found diagnostics during compilation"
        );
        for diagnostic in &diagnostics {
            // Skip warnings and hints when suppress_warnings is enabled.
            // Always skip Lint diagnostics during normal compilation — they are
            // only shown by `beamtalk lint`.
            if matches!(
                diagnostic.severity,
                beamtalk_core::source_analysis::Severity::Lint
            ) {
                continue;
            }
            // Suppress warnings/hints only when suppress_warnings is set AND
            // warnings_as_errors is not — if warnings_as_errors is on, the diagnostic
            // caused a failure and must be shown regardless of suppress_warnings.
            if options.suppress_warnings
                && !options.warnings_as_errors
                && matches!(
                    diagnostic.severity,
                    beamtalk_core::source_analysis::Severity::Warning
                        | beamtalk_core::source_analysis::Severity::Hint
                )
            {
                continue;
            }
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
    // BT-845: Use an absolute path so reload works regardless of the
    // working directory at reload time. Always pass the source path so that
    // line annotations and the 'file' attribute populate BEAM stacktraces
    // (even for stdlib). The `beamtalk_source` attribute is suppressed for
    // stdlib mode in codegen (source_path_attr checks stdlib_mode).
    let embed_source_path = std::fs::canonicalize(source_path)
        .ok()
        .and_then(|p| p.into_os_string().into_string().ok())
        .unwrap_or_else(|| source_path.as_str().to_string());
    let embed_source_path = Some(embed_source_path.as_str());
    // Build a codegen-specific hierarchy with cross-file classes (filtered to
    // exclude the current file's classes, which are added by codegen itself).
    let codegen_hierarchy = ClassHierarchyContext {
        class_module_index: ctx.hierarchy.class_module_index.clone(),
        class_superclass_index: ctx.hierarchy.class_superclass_index.clone(),
        pre_loaded_classes: cross_file_classes,
    };
    write_core_erlang_with_bindings(
        &module,
        module_name,
        core_output,
        options,
        bindings,
        &codegen_hierarchy,
        Some((&source, embed_source_path)),
    )
    .wrap_err_with(|| format!("Failed to generate Core Erlang for '{source_path}'"))?;

    debug!("Generated Core Erlang: {}", core_output);
    Ok(())
}

// ---------------------------------------------------------------------------
// Type cache: persists spec extraction results per Erlang module (ADR 0075)
// ---------------------------------------------------------------------------

/// Cache entry for a single Erlang module's spec extraction result.
///
/// Stores the raw protocol line (as emitted by `beamtalk_build_worker`) alongside
/// the `.beam` file's modification time. On subsequent builds, the cache is hit
/// when the `.beam` mtime matches — avoiding a round-trip to the BEAM node.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TypeCacheEntry {
    /// Unix timestamp (seconds since epoch) of the `.beam` file when specs were extracted.
    beam_mtime_secs: u64,
    /// Sub-second nanoseconds of the `.beam` file's modification time.
    /// Combined with `beam_mtime_secs` to avoid cache collisions on rapid rewrites.
    #[serde(default)]
    beam_mtime_nanos: u32,
    /// The raw `beamtalk-specs-module:...` protocol line (without newline).
    /// Empty string if the module had no specs or an error occurred.
    specs_line: String,
}

/// Manages the `_build/type_cache/` directory for incremental spec extraction.
///
/// Each Erlang module gets a JSON file `<module>.json` containing the cached
/// protocol line and the `.beam` file's mtime. On cache hit (matching mtime),
/// the protocol line is replayed into the `NativeTypeRegistry` without spawning
/// a BEAM node.
#[derive(Debug)]
struct TypeCache {
    cache_dir: Utf8PathBuf,
}

impl TypeCache {
    /// Creates a new type cache rooted at the given directory.
    ///
    /// The directory is created lazily on first write.
    pub fn new(cache_dir: Utf8PathBuf) -> Self {
        Self { cache_dir }
    }

    /// Returns the cache file path for a given Erlang module name.
    fn cache_path(&self, module_name: &str) -> Utf8PathBuf {
        self.cache_dir.join(format!("{module_name}.json"))
    }

    /// Checks if the cache entry for `module_name` is still valid.
    ///
    /// Returns `Some(specs_line)` if the cache is fresh (`.beam` mtime matches),
    /// or `None` if the cache is stale or missing.
    fn lookup(&self, module_name: &str, beam_mtime_secs: u64, beam_mtime_nanos: u32) -> Option<String> {
        let path = self.cache_path(module_name);
        let content = std::fs::read_to_string(path.as_std_path()).ok()?;
        let entry: TypeCacheEntry = serde_json::from_str(&content).ok()?;
        if entry.beam_mtime_secs == beam_mtime_secs && entry.beam_mtime_nanos == beam_mtime_nanos {
            Some(entry.specs_line)
        } else {
            None
        }
    }

    /// Writes a cache entry for the given module.
    fn store(&self, module_name: &str, beam_mtime_secs: u64, beam_mtime_nanos: u32, specs_line: &str) {
        if let Err(e) = std::fs::create_dir_all(self.cache_dir.as_std_path()) {
            debug!("Failed to create type cache dir: {e}");
            return;
        }
        let entry = TypeCacheEntry {
            beam_mtime_secs,
            beam_mtime_nanos,
            specs_line: specs_line.to_string(),
        };
        let path = self.cache_path(module_name);
        match serde_json::to_string(&entry) {
            Ok(json) => {
                if let Err(e) = std::fs::write(path.as_std_path(), json) {
                    debug!("Failed to write type cache for {module_name}: {e}");
                }
            }
            Err(e) => debug!("Failed to serialize type cache for {module_name}: {e}"),
        }
    }
}

/// Extracts type specs from `.beam` files and populates a `NativeTypeRegistry`.
///
/// Uses the `beamtalk_build_worker` `{read_specs, ...}` protocol to extract
/// `-spec` attributes from `.beam` files in batch. Results are cached in
/// `cache_dir` (typically `_build/type_cache/`) keyed by module name and
/// `.beam` mtime — incremental builds read zero `.beam` files on cache hit.
///
/// # Protocol
///
/// Sends `{read_specs, [BeamFile1, BeamFile2, ...]}.` to the build worker.
/// Reads `beamtalk-specs-module:<module>:<erlang_term>` lines and a final
/// `beamtalk-specs-result-ok` or `beamtalk-specs-result-error`.
///
/// # Arguments
///
/// * `beam_files` - List of `.beam` file paths to extract specs from
/// * `cache_dir` - Directory for caching results (e.g., `_build/type_cache/`)
///
/// # Returns
///
/// A populated `NativeTypeRegistry` on success.
///
/// # Errors
///
/// Returns an error if the build worker cannot be started (runtime not compiled).
#[instrument(skip_all, fields(beam_count = beam_files.len()))]
pub fn extract_beam_specs(
    beam_files: &[Utf8PathBuf],
    cache_dir: &Utf8Path,
) -> Result<NativeTypeRegistry> {
    if beam_files.is_empty() {
        return Ok(NativeTypeRegistry::new());
    }

    let cache = TypeCache::new(cache_dir.to_path_buf());
    let mut registry = NativeTypeRegistry::new();

    // Phase 1: Check cache, partition into hits and misses.
    let mut cache_misses = Vec::new();
    let mut cache_hit_count = 0;

    for beam_file in beam_files {
        let module_name = beam_file.file_stem().unwrap_or(beam_file.as_str());
        let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);

        if let Some(specs_line) = cache.lookup(module_name, mtime_secs, mtime_nanos) {
            if !specs_line.is_empty() {
                parse_specs_line(&specs_line, &mut registry);
            }
            cache_hit_count += 1;
        } else {
            cache_misses.push(beam_file.clone());
        }
    }

    if cache_hit_count > 0 {
        debug!(
            cache_hits = cache_hit_count,
            cache_misses = cache_misses.len(),
            "Type cache results"
        );
    }

    if cache_misses.is_empty() {
        info!(
            modules = registry.module_count(),
            functions = registry.function_count(),
            "Type specs loaded from cache (zero .beam files read)"
        );
        return Ok(registry);
    }

    // Phase 2: Extract specs from cache misses via build worker.
    let new_lines = extract_specs_via_build_worker(&cache_misses)?;

    // Phase 3: Parse results and update cache.
    // Build a set of modules that produced output to identify negative-cache candidates.
    let mut seen_modules: std::collections::HashSet<String> = std::collections::HashSet::new();
    for (module_name, specs_line) in &new_lines {
        seen_modules.insert(module_name.clone());
        // Find the mtime for caching.
        let beam_file = cache_misses.iter().find(|f| {
            f.file_stem()
                .is_some_and(|stem| stem == module_name.as_str())
        });
        let (mtime_secs, mtime_nanos) = beam_file.map_or((0, 0), |f| beam_mtime(f));

        if !specs_line.is_empty() {
            parse_specs_line(specs_line, &mut registry);
        }
        cache.store(module_name, mtime_secs, mtime_nanos, specs_line);
    }

    // Negative cache: modules that were sent for extraction but produced no output
    // (e.g., no debug_info, no specs). Cache them with empty specs_line to avoid
    // re-extracting on every build.
    for beam_file in &cache_misses {
        let module_name = beam_file.file_stem().unwrap_or(beam_file.as_str());
        if !seen_modules.contains(module_name) {
            let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);
            cache.store(module_name, mtime_secs, mtime_nanos, "");
        }
    }

    info!(
        modules = registry.module_count(),
        functions = registry.function_count(),
        cache_hits = cache_hit_count,
        extracted = new_lines.len(),
        "Type spec extraction complete"
    );

    Ok(registry)
}

/// Returns the modification time of a `.beam` file as `(seconds, nanoseconds)`
/// since Unix epoch. Sub-second precision avoids cache collisions on rapid rewrites.
fn beam_mtime(path: &Utf8Path) -> (u64, u32) {
    std::fs::metadata(path.as_std_path())
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map_or((0, 0), |d| (d.as_secs(), d.subsec_nanos()))
}

/// Extracts specs from `.beam` files by spawning a `beamtalk_build_worker` BEAM
/// node and sending the `{read_specs, [...]}` command.
///
/// Returns a list of `(module_name, specs_line)` pairs. The `specs_line` is the
/// raw protocol line for successful modules, or an empty string for modules that
/// had errors (`no_debug_info`, etc.).
fn extract_specs_via_build_worker(beam_files: &[Utf8PathBuf]) -> Result<Vec<(String, String)>> {
    let mut child = spawn_build_worker_for_specs()?;

    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stdin"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stderr"))?;

    // Format and send the {read_specs, [BeamFile1, ...]}. input
    let file_list: Vec<String> = beam_files
        .iter()
        .map(|p| {
            let abs = std::fs::canonicalize(p.as_std_path())
                .unwrap_or_else(|_| p.as_std_path().to_path_buf());
            format!("\"{}\"", escape_erlang_string(&abs.to_string_lossy()))
        })
        .collect();
    let input = format!("{{read_specs,[{}]}}.\n", file_list.join(","));

    stdin
        .write_all(input.as_bytes())
        .into_diagnostic()
        .wrap_err("Failed to write to spec extraction stdin")?;
    drop(stdin);

    let results = read_specs_protocol(stdout, stderr)?;

    let _ = child.wait();
    Ok(results)
}

/// Spawns a `beamtalk_build_worker` BEAM node for spec extraction.
fn spawn_build_worker_for_specs() -> Result<std::process::Child> {
    use beamtalk_cli::repl_startup;

    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().map_err(|_| {
        miette::miette!(
            "Spec extraction requires the Beamtalk runtime.\n\
             Build the runtime first: cd runtime && rebar3 compile"
        )
    })?;

    let runtime_dir = runtime_dir
        .canonicalize()
        .into_diagnostic()
        .wrap_err("Failed to resolve runtime directory for spec extraction")?;

    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    if !paths
        .compiler_ebin
        .join("beamtalk_build_worker.beam")
        .exists()
    {
        return Err(miette::miette!(
            "Spec extraction requires beamtalk_build_worker module.\n\
             Rebuild: cd runtime && rebar3 compile"
        ));
    }

    #[cfg(windows)]
    let compiler_path = paths.compiler_ebin.to_string_lossy().replace('\\', "/");
    #[cfg(not(windows))]
    let compiler_path = paths.compiler_ebin.display().to_string();

    let pa_args = vec!["-pa".to_string(), compiler_path];
    let temp_dir = std::env::temp_dir();

    Command::new("erl")
        .arg("-noshell")
        .arg("-mode")
        .arg("minimal")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-kernel")
        .arg("logger")
        .arg(repl_startup::KERNEL_LOGGER_STDERR)
        .args(&pa_args)
        .arg("-s")
        .arg("beamtalk_build_worker")
        .arg("main")
        .current_dir(&temp_dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .into_diagnostic()
        .wrap_err("Failed to start BEAM node for spec extraction")
}

/// Reads the `beamtalk-specs-module:` protocol lines from the build worker's stdout.
///
/// Returns `(module_name, raw_protocol_line)` pairs for each module.
fn read_specs_protocol(
    stdout: std::process::ChildStdout,
    stderr: std::process::ChildStderr,
) -> Result<Vec<(String, String)>> {
    // Read stderr in background to avoid deadlock
    let stderr_thread = thread::spawn(move || {
        let reader = BufReader::new(stderr);
        for line in reader.lines().map_while(Result::ok) {
            if !line.is_empty() {
                debug!(target: "spec_reader_stderr", "{}", line);
            }
        }
    });

    let reader = BufReader::new(stdout);
    let mut results = Vec::new();
    let mut success = false;

    for line in reader.lines() {
        let line = line.into_diagnostic()?;
        if is_specs_line(&line) {
            if let Some(rest) = line.strip_prefix("beamtalk-specs-module:") {
                if let Some(colon_pos) = rest.find(':') {
                    let module_name = rest[..colon_pos].to_string();
                    results.push((module_name, line.clone()));
                }
            }
        } else if is_specs_result_ok(&line) {
            success = true;
        } else if is_specs_result_error(&line) {
            warn!("Spec extraction reported errors (some modules may lack type info)");
            success = true;
        }
    }

    let _ = stderr_thread.join();

    if !success {
        warn!("Spec extraction did not receive a result line — partial results may be used");
    }

    Ok(results)
}

/// Discovers `.beam` files on the OTP code path.
///
/// Returns absolute paths to all `.beam` files in common OTP library ebin
/// directories (`stdlib`, `kernel`, etc.). Used to find modules available
/// for spec extraction.
///
/// # Errors
///
/// Returns an error if `erl` cannot be invoked to discover the OTP lib directory.
pub fn discover_otp_beam_files() -> Result<Vec<Utf8PathBuf>> {
    // Use `erl -noshell -noinput -eval '...'` to find the OTP lib directory.
    // We clear the environment to avoid user-specific Erlang config affecting the probe.
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg("io:format(\"~s\", [code:lib_dir()]), halt().")
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to discover OTP lib directory")?;

    if !output.status.success() {
        let stderr_msg = String::from_utf8_lossy(&output.stderr);
        warn!("erl probe failed (exit {}): {}", output.status, stderr_msg.trim());
        return Ok(Vec::new());
    }

    let lib_dir = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if lib_dir.is_empty() {
        warn!("erl probe returned empty lib_dir");
        return Ok(Vec::new());
    }

    // Collect .beam files from common OTP application ebin directories.
    // Each app lives in `lib_dir/<app>-<version>/ebin/`.
    let common_apps = [
        "stdlib", "kernel", "crypto", "ssl", "inets", "mnesia", "os_mon",
    ];

    let mut beam_files = Vec::new();
    let lib_path = std::path::Path::new(&lib_dir);
    if let Ok(entries) = std::fs::read_dir(lib_path) {
        for entry in entries.flatten() {
            let dir_name = entry.file_name();
            let dir_name_str = dir_name.to_string_lossy();
            // Check if this directory matches one of our common apps (e.g., "stdlib-5.2")
            let matches_app = common_apps.iter().any(|app| {
                dir_name_str.starts_with(app)
                    && dir_name_str.as_bytes().get(app.len()) == Some(&b'-')
            });
            if !matches_app {
                continue;
            }
            let ebin_dir = entry.path().join("ebin");
            if !ebin_dir.is_dir() {
                continue;
            }
            if let Ok(ebin_entries) = std::fs::read_dir(&ebin_dir) {
                for file in ebin_entries.flatten() {
                    let path = file.path();
                    if path.extension().is_some_and(|e| e == "beam") {
                        if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                            beam_files.push(utf8);
                        }
                    }
                }
            }
        }
    }

    debug!(count = beam_files.len(), "Discovered OTP .beam files");
    Ok(beam_files)
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

        // Check that output directory was created and compilation succeeded
        assert!(output_dir.exists());
        result.expect("compile_batch should succeed when escript is available");
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

    /// Source that triggers an actor-new Error (using `new` on an Actor subclass).
    /// BT-1524: Promoted from warning to error.
    const ACTOR_NEW_SOURCE: &str =
        "Actor subclass: TestActorWarnings\n  doNothing => nil\n\nTestActorWarnings new";

    #[test]
    fn test_actor_new_is_compile_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "actor_new", &core_file, &options);

        // BT-1524: actor new is now an error, not a warning — always fails
        assert!(
            result.is_err(),
            "Should fail to compile: actor new is a compile-time error: {result:?}"
        );
    }

    #[test]
    fn test_warnings_as_errors_overrides_suppress_warnings() {
        // suppress_warnings: true alone would suppress and allow compilation;
        // combined with warnings_as_errors: true, the failure must still happen.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            suppress_warnings: true,
            ..Default::default()
        };
        let result = compile_source(&source_file, "actor_new", &core_file, &options);

        assert!(
            result.is_err(),
            "warnings_as_errors must override suppress_warnings: compilation should fail"
        );
    }

    /// BT-1535: Keyword/class-kind mismatches are now hard compile errors
    /// (promoted from deprecation warnings in Phase 4 of ADR 0067).
    #[test]
    fn test_keyword_class_kind_mismatch_is_compile_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("mismatch.bt");
        let core_file = temp_path.join("mismatch.core");
        // Object subclass with state: is now a hard compile error
        fs::write(
            &source_file,
            "Object subclass: BadObj\n  state: x = 0\n  value => self.x",
        )
        .unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "bt@mismatch", &core_file, &options);

        assert!(
            result.is_err(),
            "Object subclass with state: should be a compile error: {result:?}"
        );
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

    // -----------------------------------------------------------------------
    // TypeCache tests (ADR 0075)
    // -----------------------------------------------------------------------

    #[test]
    fn type_cache_store_and_lookup() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);

        let specs_line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";
        cache.store("lists", 12345, 0, specs_line);

        // Cache hit with matching mtime
        let result = cache.lookup("lists", 12345, 0);
        assert_eq!(result, Some(specs_line.to_string()));

        // Cache miss with different mtime (seconds)
        let result = cache.lookup("lists", 99999, 0);
        assert!(result.is_none(), "Different mtime should be a cache miss");

        // Cache miss with different mtime (nanos only)
        let result = cache.lookup("lists", 12345, 1);
        assert!(result.is_none(), "Different nanos should be a cache miss");

        // Cache miss for unknown module
        let result = cache.lookup("maps", 12345, 0);
        assert!(result.is_none(), "Unknown module should be a cache miss");
    }

    #[test]
    fn type_cache_invalidates_on_mtime_change() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);

        cache.store("lists", 100, 0, "line1");
        assert_eq!(cache.lookup("lists", 100, 0), Some("line1".to_string()));

        // Overwrite with new mtime
        cache.store("lists", 200, 0, "line2");
        assert!(cache.lookup("lists", 100, 0).is_none());
        assert_eq!(cache.lookup("lists", 200, 0), Some("line2".to_string()));
    }

    #[test]
    fn type_cache_handles_empty_specs_line() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);

        cache.store("no_specs", 100, 0, "");
        assert_eq!(cache.lookup("no_specs", 100, 0), Some(String::new()));
    }

    #[test]
    fn extract_beam_specs_empty_input() {
        let result = extract_beam_specs(&[], Utf8Path::new("/tmp/nonexistent"));
        assert!(result.is_ok());
        let registry = result.unwrap();
        assert_eq!(registry.module_count(), 0);
    }

    #[test]
    fn beam_mtime_nonexistent() {
        let (secs, nanos) = beam_mtime(Utf8Path::new("/nonexistent/foo.beam"));
        assert_eq!(secs, 0, "Nonexistent file should return 0 seconds");
        assert_eq!(nanos, 0, "Nonexistent file should return 0 nanos");
    }
}
