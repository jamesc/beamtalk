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

/// Escapes a string for use in an Erlang string literal.
///
/// This escapes backslashes, quotes, and control characters to prevent
/// injection attacks and ensure valid Erlang term syntax when constructing
/// Erlang terms from file paths and other user-supplied strings.
///
/// Handles: `\`, `"`, `\n`, `\r`, `\t`, `\0`.
pub(crate) fn escape_erlang_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result
}

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
        beamtalk_core::erlang::CodegenOptions::new(module_name)
            .with_source_opt(source_text)
            // ADR 0098 Phase 3: bake the producing-toolchain identity into __beamtalk_meta.
            .with_provenance(
                env!("BEAMTALK_VERSION"),
                crate::commands::build_stamp::current_otp_version(),
            ),
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
    /// Full `ProtocolInfo` entries from other source files in the compilation
    /// unit (e.g. `BUnit` fixtures). Seeded into the protocol registry during
    /// semantic analysis so the unresolved-class validator and type checker
    /// recognise protocol names defined outside the current module (BT-2006).
    pub pre_loaded_protocols:
        Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Project-wide standalone extension definitions from Pass 1 (BT-2795).
    /// Registered into each file's class hierarchy during Pass 2 so
    /// same-project cross-file extensions resolve instead of producing
    /// false `Dnu` hints (ADR 0066 / ADR 0100 Rule 2 WS1).
    pub extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
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
    /// Native type registry for FFI call inference during build (ADR 0075).
    ///
    /// When set, semantic analysis uses [`beamtalk_core::semantic_analysis::analyse_with_natives`]
    /// so that `Erlang <module> <function>:` calls get return type inference and
    /// argument type checking in build output, not just the LSP.
    pub native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
    /// Per-category diagnostic severity overrides from `beamtalk.toml`'s
    /// `[diagnostics]` section (ADR 0100 Rule 3). Empty when the package has
    /// no manifest or no `[diagnostics]` section — absence preserves today's
    /// Rule 1 completeness-ladder defaults.
    pub diagnostics_overrides: crate::commands::manifest::DiagnosticsTable,
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
            .with_class_hierarchy(hierarchy.pre_loaded_classes.clone())
            // ADR 0098 Phase 3: bake the producing-toolchain identity into __beamtalk_meta.
            .with_provenance(
                env!("BEAMTALK_VERSION"),
                crate::commands::build_stamp::current_otp_version(),
            ),
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
    .map(|_diags| ())
}

/// Apply a package's `[diagnostics]` severity-override table (ADR 0100 Rule 3)
/// to a list of diagnostics.
///
/// For each diagnostic whose category has a table entry: `"off"` drops the
/// diagnostic; `"lint"` / `"hint"` / `"warn"` / `"error"` rewrite its
/// `severity` in place, becoming the category's *base* severity for the
/// package. Diagnostics with no category, or whose category has no table
/// entry, pass through unchanged — an empty table (no manifest, or no
/// `[diagnostics]` section) is a complete no-op, preserving today's Rule 1
/// defaults.
///
/// **Severity floor:** a diagnostic that already carries `Severity::Error`
/// is never touched by the table, even if its category has an entry. Rule 3
/// is an *escalation* mechanism for the soft, open-world diagnostics the
/// completeness ladder (Rule 1) produces (`Hint`/`Warning`) — it is not a
/// blanket switch that can quietly turn a hard structural compile error
/// (e.g. `ActorNew`, `Inheritance`, `EmptyBody`) into a passing build.
///
/// Must run after `@expect` suppression (Rule 3 precedence step 1, applied
/// inside `compute_project_diagnostics`) and before the `--warnings-as-errors`
/// promotion pass, which is a *final* pass over whatever this step resolves
/// to (ADR 0100 Rule 3).
fn apply_diagnostics_table(
    diagnostics: Vec<beamtalk_core::source_analysis::Diagnostic>,
    table: &crate::commands::manifest::DiagnosticsTable,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    use crate::commands::manifest::DiagnosticSeverityOverride;
    use beamtalk_core::source_analysis::Severity;

    if table.is_empty() {
        return diagnostics;
    }

    diagnostics
        .into_iter()
        .filter_map(|mut diagnostic| {
            let Some(category) = diagnostic.category else {
                return Some(diagnostic);
            };
            // Severity floor: a diagnostic that already arrived as `Error`
            // (e.g. `ActorNew` — BT-1524's "Actor subclass must use spawn,
            // not new" — or `Inheritance` / `EmptyBody` hard-error checks) is
            // never a Rule 1 completeness-ladder soft diagnostic; it's a
            // structural compile error unrelated to open-world uncertainty.
            // ADR 0100 Rule 3 frames the table as opt-in *escalation* of soft
            // diagnostics, not silent de-escalation of hard ones — a `warn`
            // or `off` entry for one of these categories must not quietly
            // turn a guaranteed compile error into a passing build.
            if diagnostic.severity == Severity::Error {
                return Some(diagnostic);
            }
            match table.get(&category) {
                None => Some(diagnostic),
                Some(DiagnosticSeverityOverride::Off) => None,
                Some(DiagnosticSeverityOverride::Lint) => {
                    diagnostic.severity = Severity::Lint;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Hint) => {
                    diagnostic.severity = Severity::Hint;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Warn) => {
                    diagnostic.severity = Severity::Warning;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Error) => {
                    diagnostic.severity = Severity::Error;
                    Some(diagnostic)
                }
            }
        })
        .collect()
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
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
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

    // BT-2009: Unified post-analysis diagnostic pipeline.
    // Semantic analysis + all post-analysis passes + @expect suppression are now
    // handled by a single function shared between CLI and LSP.
    let cross_file_classes =
        beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
            &ctx.hierarchy.pre_loaded_classes,
            &module,
        );
    let diag_ctx = beamtalk_core::queries::diagnostic_provider::ProjectDiagnosticContext {
        options: options.clone(),
        cross_file_classes: cross_file_classes.clone(),
        pre_loaded_protocols: ctx.hierarchy.pre_loaded_protocols.clone(),
        cross_file_extensions: ctx.hierarchy.extension_index.clone(),
        native_type_registry: ctx.native_type_registry.clone(),
        dep_registry: ctx.dep_registry,
        strict_deps: ctx.strict_deps,
    };
    diagnostics = beamtalk_core::queries::diagnostic_provider::compute_project_diagnostics(
        &module,
        diagnostics,
        &diag_ctx,
    );

    // ADR 0100 Rule 3 (BT-2793): apply the package's `[diagnostics]` table —
    // precedence step 2, after `@expect` suppression (step 1, applied inside
    // `compute_project_diagnostics` above) and ahead of the `has_errors`
    // promotion pass below. A no-op (empty table) when the package has no
    // manifest or no `[diagnostics]` section.
    diagnostics = apply_diagnostics_table(diagnostics, &ctx.diagnostics_overrides);

    // Check for errors (and optionally treat warnings/hints as errors).
    // Deprecation-category warnings (BT-1529) and structural validation warnings
    // (BT-1726: UnresolvedClass, UnresolvedFfi, ArityMismatch) are excluded from
    // warnings-as-errors because they can produce false positives when compiling
    // single files that reference classes, FFI modules, or arities defined in
    // other compilation units.
    //
    // ADR 0100 Rule 3 (BT-2793): that exclusion is itself the Rule 1 default
    // for those categories — an explicit `[diagnostics]` table entry for one
    // of them is a deliberate, package-level opt back in to promotion, so it
    // wins over the exclusion (per the "explicit table value wins over the
    // exclusion" precedence note).
    let has_errors = diagnostics.iter().any(|d| {
        d.severity == beamtalk_core::source_analysis::Severity::Error
            || (options.warnings_as_errors
                && matches!(
                    d.severity,
                    beamtalk_core::source_analysis::Severity::Warning
                        | beamtalk_core::source_analysis::Severity::Hint
                )
                && (!matches!(
                    d.category,
                    Some(
                        beamtalk_core::source_analysis::DiagnosticCategory::Deprecation
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedFfi
                            | beamtalk_core::source_analysis::DiagnosticCategory::ArityMismatch
                    )
                ) || d
                    .category
                    .is_some_and(|c| ctx.diagnostics_overrides.contains_key(&c))))
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

    // Save diagnostics for the caller to build a summary (BT-2014).
    // `diagnostics` is no longer needed after this point — the printing loop above
    // borrowed them by reference and the error check already bailed if needed.
    let returned_diags = diagnostics;

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
        ..ClassHierarchyContext::default()
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
    Ok(returned_diags)
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
    /// Absolute path to the `.beam` file at the time specs were extracted —
    /// canonicalised when possible, otherwise resolved against the build's
    /// cwd. Used by [`load_type_cache_registry`] to re-stat the file and
    /// skip the entry if the live mtime no longer matches what was cached.
    /// Persisting an absolute path means `beamtalk lint` can validate the
    /// cache regardless of which cwd it is invoked from. Empty only for
    /// legacy entries written before BT-2139 — those are tolerated as fresh
    /// until the next build rewrites them with a path.
    #[serde(default)]
    beam_path: String,
    /// The raw `beamtalk-specs-module:...` protocol line (without newline).
    /// Empty string if the module had no specs or an error occurred.
    specs_line: String,
}

/// Manages the type-spec cache for incremental spec extraction.
///
/// Each Erlang module gets a JSON file `<module>_<pathhash>.json` containing
/// the cached protocol line and the `.beam` file's mtime. On cache hit
/// (matching mtime), the protocol line is replayed into the
/// `NativeTypeRegistry` without spawning a BEAM node.
///
/// # Tiers (BT-2470)
///
/// The cache has two tiers:
///
/// * `local_dir` — the project-local `_build/type_cache/`. Always written, so
///   on-disk consumers ([`load_type_cache_registry`] for `beamtalk lint`, and
///   the LSP's `load_type_cache`) keep finding every module's specs unchanged.
/// * `shared_dir` — an optional shared, OTP-version-keyed tier (see
///   [`shared_otp_cache_dir`]). It survives `_build/` wipes and is reused
///   across projects and sessions, so a freshly cloned workspace does not pay
///   the cost of re-extracting hundreds of OTP modules. Only used for OTP
///   extraction; dependency extraction passes `None`.
///
/// On a `local` miss the `shared` tier is consulted; a shared hit is mirrored
/// back into the `local` tier so the on-disk consumers above see it. Writes go
/// to both tiers.
#[derive(Debug)]
struct TypeCache {
    local_dir: Utf8PathBuf,
    shared_dir: Option<Utf8PathBuf>,
}

impl TypeCache {
    /// Creates a single-tier type cache rooted at the given (project-local)
    /// directory. The directory is created lazily on first write.
    pub fn new(local_dir: Utf8PathBuf) -> Self {
        Self {
            local_dir,
            shared_dir: None,
        }
    }

    /// Creates a two-tier cache: a project-local tier plus a shared,
    /// OTP-version-keyed tier (BT-2470).
    pub fn with_shared(local_dir: Utf8PathBuf, shared_dir: Utf8PathBuf) -> Self {
        Self {
            local_dir,
            shared_dir: Some(shared_dir),
        }
    }

    /// Returns the cache file path for a given Erlang module name and beam path
    /// within `base`.
    ///
    /// The cache key incorporates a hash of the beam file's absolute path to
    /// prevent collisions when different projects have same-named `.beam` files
    /// (e.g., two projects both containing `my_app.beam` in the global stub cache).
    fn entry_path(base: &Utf8Path, module_name: &str, beam_path: &Utf8Path) -> Utf8PathBuf {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        beam_path.as_str().hash(&mut hasher);
        let path_hash = hasher.finish();
        base.join(format!("{module_name}_{path_hash:016x}.json"))
    }

    /// Checks if the cache entry for `module_name` at `beam_path` is still valid.
    ///
    /// Returns `Some(specs_line)` if the cache is fresh (`.beam` mtime matches),
    /// or `None` if the cache is stale or missing. The local tier is checked
    /// first; on a miss the shared tier is consulted and any shared hit is
    /// mirrored back into the local tier.
    fn lookup(
        &self,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
    ) -> Option<String> {
        if let Some(line) = Self::read_fresh(
            &self.local_dir,
            module_name,
            beam_path,
            beam_mtime_secs,
            beam_mtime_nanos,
        ) {
            return Some(line);
        }
        if let Some(shared) = &self.shared_dir {
            if let Some(line) = Self::read_fresh(
                shared,
                module_name,
                beam_path,
                beam_mtime_secs,
                beam_mtime_nanos,
            ) {
                // Mirror into the local tier so on-disk consumers (LSP, lint)
                // find OTP specs in `_build/type_cache/` exactly as before.
                Self::write_entry(
                    &self.local_dir,
                    module_name,
                    beam_path,
                    beam_mtime_secs,
                    beam_mtime_nanos,
                    &line,
                );
                return Some(line);
            }
        }
        None
    }

    /// Writes a cache entry for the given module to both tiers.
    fn store(
        &self,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
        specs_line: &str,
    ) {
        Self::write_entry(
            &self.local_dir,
            module_name,
            beam_path,
            beam_mtime_secs,
            beam_mtime_nanos,
            specs_line,
        );
        if let Some(shared) = &self.shared_dir {
            Self::write_entry(
                shared,
                module_name,
                beam_path,
                beam_mtime_secs,
                beam_mtime_nanos,
                specs_line,
            );
        }
    }

    /// Reads a fresh cache entry from `base`, or `None` if missing/stale.
    fn read_fresh(
        base: &Utf8Path,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
    ) -> Option<String> {
        let path = Self::entry_path(base, module_name, beam_path);
        let content = std::fs::read_to_string(path.as_std_path()).ok()?;
        let entry: TypeCacheEntry = serde_json::from_str(&content).ok()?;
        if entry.beam_mtime_secs == beam_mtime_secs && entry.beam_mtime_nanos == beam_mtime_nanos {
            Some(entry.specs_line)
        } else {
            None
        }
    }

    /// Writes a single cache entry into `base`, creating the directory if
    /// needed. The write is atomic (temp file + rename) so concurrent builds
    /// sharing the OTP tier never observe a half-written entry (BT-2470).
    fn write_entry(
        base: &Utf8Path,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
        specs_line: &str,
    ) {
        if let Err(e) = std::fs::create_dir_all(base.as_std_path()) {
            debug!("Failed to create type cache dir {base}: {e}");
            return;
        }
        // BT-2139: persist an absolute (canonicalised) path so freshness
        // validation in `load_type_cache_registry` works when `beamtalk lint`
        // runs from a different working directory than the build that wrote
        // the cache. If `canonicalize_utf8` fails (typically because the
        // `.beam` vanished or moved mid-build), fall back to a manually-built
        // absolute path: if `beam_path` is already absolute, use it as-is;
        // otherwise resolve it against the current working directory. We
        // intentionally do *not* leave `beam_path` empty here — that would
        // make `is_cache_entry_fresh` treat the entry as legacy/fresh, so
        // lint would keep replaying stale specs instead of detecting that
        // the underlying `.beam` is gone.
        let canonical_beam_path = beam_path.canonicalize_utf8().map_or_else(
            |_| {
                if beam_path.is_absolute() {
                    beam_path.as_str().to_string()
                } else {
                    std::env::current_dir()
                        .ok()
                        .and_then(|cwd| Utf8PathBuf::from_path_buf(cwd).ok())
                        .map(|cwd| cwd.join(beam_path).into_string())
                        .unwrap_or_default()
                }
            },
            Utf8PathBuf::into_string,
        );
        let entry = TypeCacheEntry {
            beam_mtime_secs,
            beam_mtime_nanos,
            beam_path: canonical_beam_path,
            specs_line: specs_line.to_string(),
        };
        let path = Self::entry_path(base, module_name, beam_path);
        let json = match serde_json::to_string(&entry) {
            Ok(json) => json,
            Err(e) => {
                debug!("Failed to serialize type cache for {module_name}: {e}");
                return;
            }
        };
        // Atomic publish: write to a unique temp file then rename into place.
        // The temp name includes the pid so concurrent writers don't clash.
        let tmp = base.join(format!(
            "{module_name}_{:016x}.{}.tmp",
            {
                use std::hash::{Hash, Hasher};
                let mut h = std::collections::hash_map::DefaultHasher::new();
                beam_path.as_str().hash(&mut h);
                h.finish()
            },
            std::process::id()
        ));
        if let Err(e) = std::fs::write(tmp.as_std_path(), &json) {
            debug!("Failed to write type cache temp for {module_name}: {e}");
            return;
        }
        if let Err(e) = std::fs::rename(tmp.as_std_path(), path.as_std_path()) {
            // On Windows, rename can fail with "Access denied" if the destination
            // is open without delete-sharing (a concurrent reader). Fall back to a
            // copy so the cache update isn't silently dropped, then always clean up
            // the temp file regardless of which path succeeded.
            debug!(
                "Failed to publish type cache for {module_name} via rename: {e}; falling back to copy"
            );
            if let Err(e2) = std::fs::copy(tmp.as_std_path(), path.as_std_path()) {
                debug!("Failed to publish type cache for {module_name} via copy: {e2}");
            }
            let _ = std::fs::remove_file(tmp.as_std_path());
        }
    }
}

/// Returns the shared, OTP-version-keyed type-spec cache directory for the
/// given OTP version string, or `None` if no suitable base directory can be
/// determined (BT-2470).
///
/// The OTP portion of the FFI type cache (stdlib, kernel, erts, crypto, …)
/// only changes when the OTP/ERTS version changes — it is not project-specific.
/// Caching it outside `_build/` lets a freshly cloned workspace (where
/// `_build/type_cache/` does not yet exist) reuse a previous extraction instead
/// of re-reading hundreds of `.beam` files on startup.
///
/// Base directory resolution, in priority order:
/// 1. `BEAMTALK_CACHE_DIR` environment variable (explicit override, used by
///    tests and CI cache mounts).
/// 2. The platform cache directory ([`dirs::cache_dir`], which honours
///    `XDG_CACHE_HOME` on Linux).
///
/// The version string is sanitised for filesystem safety, so an OTP upgrade
/// (new version key) lands in a fresh sub-directory and never reuses
/// stale-version entries.
pub fn shared_otp_cache_dir(otp_version: &str) -> Option<Utf8PathBuf> {
    let base = std::env::var_os("BEAMTALK_CACHE_DIR")
        .map(std::path::PathBuf::from)
        .or_else(dirs::cache_dir)?;
    let base = Utf8PathBuf::from_path_buf(base).ok()?;
    let version_key: String = otp_version
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '.' || c == '-' || c == '_' {
                c
            } else {
                '-'
            }
        })
        .collect();
    if version_key.is_empty() {
        return None;
    }
    Some(base.join("beamtalk").join("otp-specs").join(version_key))
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
pub fn extract_beam_specs(
    beam_files: &[Utf8PathBuf],
    cache_dir: &Utf8Path,
) -> Result<NativeTypeRegistry> {
    extract_beam_specs_with_cache(beam_files, &TypeCache::new(cache_dir.to_path_buf()))
}

/// Like [`extract_beam_specs`], but adds a shared, OTP-version-keyed cache tier
/// (BT-2470) in front of the project-local `local_cache_dir`.
///
/// On a fresh `_build/` (e.g. a newly cloned workspace) the local tier misses
/// for every OTP module, but a warm shared tier — populated by a previous build
/// in any project — supplies the specs without re-reading hundreds of `.beam`
/// files. Shared hits are mirrored into the local tier so the LSP and
/// `beamtalk lint` keep loading OTP specs from `_build/type_cache/` unchanged.
///
/// Passing `shared_cache_dir = None` is equivalent to [`extract_beam_specs`].
///
/// # Errors
///
/// Returns an error if the build worker cannot be started (runtime not compiled).
pub fn extract_beam_specs_tiered(
    beam_files: &[Utf8PathBuf],
    local_cache_dir: &Utf8Path,
    shared_cache_dir: Option<&Utf8Path>,
) -> Result<NativeTypeRegistry> {
    let cache = match shared_cache_dir {
        Some(shared) => TypeCache::with_shared(local_cache_dir.to_path_buf(), shared.to_path_buf()),
        None => TypeCache::new(local_cache_dir.to_path_buf()),
    };
    extract_beam_specs_with_cache(beam_files, &cache)
}

#[instrument(skip_all, fields(beam_count = beam_files.len()))]
fn extract_beam_specs_with_cache(
    beam_files: &[Utf8PathBuf],
    cache: &TypeCache,
) -> Result<NativeTypeRegistry> {
    if beam_files.is_empty() {
        return Ok(NativeTypeRegistry::new());
    }

    let mut registry = NativeTypeRegistry::new();

    // Phase 1: Check cache, partition into hits and misses.
    let mut cache_misses = Vec::new();
    let mut cache_hit_count = 0;

    for beam_file in beam_files {
        let module_name = sanitize_module_name(beam_file.file_stem().unwrap_or(beam_file.as_str()));
        let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);

        if let Some(specs_line) = cache.lookup(module_name, beam_file, mtime_secs, mtime_nanos) {
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
        // Find the beam file for caching (need path + mtime).
        let beam_file = cache_misses.iter().find(|f| {
            f.file_stem()
                .is_some_and(|stem| stem == module_name.as_str())
        });
        if !specs_line.is_empty() {
            parse_specs_line(specs_line, &mut registry);
        }
        if let Some(beam_file) = beam_file {
            let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);
            cache.store(module_name, beam_file, mtime_secs, mtime_nanos, specs_line);
        }
    }

    // Negative cache: modules that were sent for extraction but produced no output
    // (e.g., no debug_info, no specs). Cache them with empty specs_line to avoid
    // re-extracting on every build.
    for beam_file in &cache_misses {
        let module_name = sanitize_module_name(beam_file.file_stem().unwrap_or(beam_file.as_str()));
        if !seen_modules.contains(module_name) {
            let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);
            cache.store(module_name, beam_file, mtime_secs, mtime_nanos, "");
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

/// Reads `<module>_<hash>.json` files from `cache_dir` and replays the
/// freshest cached `specs_line` per module into a new [`NativeTypeRegistry`].
///
/// Used by `beamtalk lint` (BT-2134) to populate the same FFI type registry
/// `beamtalk build` uses, so the type checker's "Dynamic in typed class"
/// warning agrees with build on whether an FFI call is typed. Without this,
/// lint sees every `(Erlang m) f:` call as `Dynamic(UntypedFfi)` even when
/// the build cache has typed signatures.
///
/// `TypeCache::cache_path` keys filenames by the BEAM path hash, so multiple
/// `<module>_<hash>.json` entries can accumulate after dependency upgrades or
/// BEAM path changes. Replaying every file would let `read_dir` order pick a
/// stale signature, reintroducing the lint/build disagreement BT-2134 fixed.
/// Instead, group by module name and pick the entry with the latest file
/// mtime — that's the one the most recent build wrote, and it matches what
/// build's `extract_beam_specs` resolved for the current BEAM set.
///
/// Each entry's `beam_path` is re-stat'd against the live filesystem (BT-2139).
/// If the underlying `.beam` has changed since the build wrote the cache —
/// e.g. `cargo build` rebuilt a NIF module, or an OTP upgrade replaced
/// `gen_tcp.beam` — the entry is skipped so lint does not warn off stale FFI
/// signatures. Entries written before BT-2139 carry an empty `beam_path`;
/// those are pessimistically accepted as fresh until the next build rewrites
/// them with a path.
///
/// Returns `None` if `cache_dir` is not a directory or contains no entries.
pub fn load_type_cache_registry(cache_dir: &Utf8Path) -> Option<NativeTypeRegistry> {
    if !cache_dir.is_dir() {
        return None;
    }

    // Group cache files by module name, keeping the latest-mtime entry per
    // module so a stale `<module>_<old_hash>.json` doesn't shadow the fresh
    // one when `read_dir` happens to yield it second.
    let entries = std::fs::read_dir(cache_dir.as_std_path()).ok()?;
    let mut latest_by_module: std::collections::HashMap<String, (SystemTime, std::path::PathBuf)> =
        std::collections::HashMap::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(filename) = path.file_name().and_then(|s| s.to_str()) else {
            continue;
        };
        let Some(stem) = filename.strip_suffix(".json") else {
            continue;
        };
        // Cache filenames are exactly `<module>_<16-hex>`. Anything else is
        // foreign and must be ignored, including `<module>_socket_<hash>`
        // (where the module name itself ends in an underscore segment).
        let Some((module, hash)) = stem.rsplit_once('_') else {
            continue;
        };
        if hash.len() != 16 || !hash.chars().all(|c| c.is_ascii_hexdigit()) {
            continue;
        }
        let mtime = entry
            .metadata()
            .and_then(|m| m.modified())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        latest_by_module
            .entry(module.to_string())
            .and_modify(|(prev_mtime, prev_path)| {
                if mtime > *prev_mtime {
                    *prev_mtime = mtime;
                    prev_path.clone_from(&path);
                }
            })
            .or_insert((mtime, path));
    }

    let mut registry = NativeTypeRegistry::new();
    for (_, path) in latest_by_module.values() {
        let Ok(content) = std::fs::read_to_string(path) else {
            continue;
        };
        let Ok(entry) = serde_json::from_str::<TypeCacheEntry>(&content) else {
            continue;
        };
        if !is_cache_entry_fresh(&entry) {
            continue;
        }
        if !entry.specs_line.is_empty() {
            parse_specs_line(&entry.specs_line, &mut registry);
        }
    }

    if registry.module_count() == 0 {
        None
    } else {
        Some(registry)
    }
}

/// Sanitizes a module name derived from a beam file path by stripping any
/// directory components (path separators). This prevents path traversal when
/// the module name is used in cache filenames.
fn sanitize_module_name(name: &str) -> &str {
    // Take only the final component after any path separator.
    let after_slash = name.rfind('/').map_or(name, |i| &name[i + 1..]);
    after_slash
        .rfind('\\')
        .map_or(after_slash, |i| &after_slash[i + 1..])
}

/// Returns `true` if the cache entry still describes the live `.beam` file —
/// i.e. the file at `beam_path` exists and its mtime matches what was cached.
///
/// Legacy entries written before BT-2139 carry an empty `beam_path`; we have
/// no way to validate them, so they are pessimistically accepted as fresh.
/// The next `beamtalk build` rewrites them with a path, which then enables
/// validation on subsequent lint runs.
fn is_cache_entry_fresh(entry: &TypeCacheEntry) -> bool {
    if entry.beam_path.is_empty() {
        return true;
    }
    let path = Utf8Path::new(&entry.beam_path);
    if !path.exists() {
        return false;
    }
    let (secs, nanos) = beam_mtime(path);
    secs == entry.beam_mtime_secs && nanos == entry.beam_mtime_nanos
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

    let result = extract_specs_from_child(&mut child, beam_files);

    // Always wait/kill the child process to prevent zombies, even on error.
    if result.is_err() {
        let _ = child.kill();
    }
    let _ = child.wait();

    result
}

/// Inner helper for `extract_specs_via_build_worker`. Separated so that the
/// caller can guarantee `child.wait()` / `child.kill()` on all exit paths,
/// including early `?` returns from stdin/stdout/stderr capture or write errors.
fn extract_specs_from_child(
    child: &mut std::process::Child,
    beam_files: &[Utf8PathBuf],
) -> Result<Vec<(String, String)>> {
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

    read_specs_protocol(stdout, stderr)
}

/// Spawns a `beamtalk_build_worker` BEAM node with the given `-pa` path arguments.
///
/// Applies the standard boot flags (`-noshell -mode minimal -boot no_dot_erlang`) and
/// the kernel logger redirect (BT-1431).  Callers supply the variable `-pa` paths and
/// add a context-specific `wrap_err` message on the returned `Result`.
fn spawn_build_worker_node(pa_args: &[String]) -> Result<std::process::Child> {
    use beamtalk_cli::repl_startup;
    Command::new("erl")
        .arg("-noshell")
        .arg("-mode")
        .arg("minimal")
        .arg("-boot")
        .arg("no_dot_erlang")
        // Redirect OTP default logger to stderr (BT-1431). Without this, logger
        // output goes to stdout and mixes into the compilation protocol.
        .arg("-kernel")
        .arg("logger")
        .arg(repl_startup::KERNEL_LOGGER_STDERR)
        .args(pa_args)
        .arg("-s")
        .arg("beamtalk_build_worker")
        .arg("main")
        .current_dir(std::env::temp_dir())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .into_diagnostic()
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

    // Add all runtime ebin directories to the code path so the spec reader
    // can resolve remote types (e.g., `beamtalk_result:t()` needs the
    // beamtalk_stdlib ebin on the path for `code:which/1` to find it).
    let ebin_dirs = [
        &paths.compiler_ebin,
        &paths.runtime_ebin,
        &paths.stdlib_erlang_ebin,
        &paths.workspace_ebin,
    ];
    let mut pa_args = Vec::new();
    for ebin in &ebin_dirs {
        if ebin.exists() {
            pa_args.push("-pa".to_string());
            #[cfg(windows)]
            pa_args.push(ebin.to_string_lossy().replace('\\', "/"));
            #[cfg(not(windows))]
            pa_args.push(ebin.display().to_string());
        }
    }
    spawn_build_worker_node(&pa_args).wrap_err("Failed to start BEAM node for spec extraction")
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

/// Result of probing the OTP installation for spec extraction (BT-2470).
#[derive(Debug, Default, Clone)]
pub struct OtpDiscovery {
    /// OTP version key (`<otp_release>-<erts_version>`, e.g. `27-15.0.1`) used
    /// to key the shared type-spec cache. `None` if the probe could not report
    /// it, in which case the shared cache tier is skipped.
    pub version: Option<String>,
    /// Absolute paths to all `.beam` files in the common OTP library ebins.
    pub beam_files: Vec<Utf8PathBuf>,
}

/// Probes the running OTP installation for its compound version key
/// (`<otp_release>-<erts>`, e.g. `27-15.0.1`) without enumerating any `.beam`
/// files.
///
/// This is the **same** key [`discover_otp_beam_files`] reports and the same one
/// that keys the shared type-spec cache (BT-2470). ADR 0098 provenance stamps
/// must use this compound — not bare `erlang:system_info(otp_release)`, which
/// returns only `"27"` — so a minor OTP/ERTS bump still invalidates artifacts.
///
/// Returns `None` if `erl` cannot be invoked or did not report a version; the
/// caller then compares provenance on `beamtalk_version` alone.
pub fn discover_otp_version() -> Option<String> {
    // Prefix the value with a sentinel (mirroring `discover_otp_beam_files`) and
    // scan for it, rather than trusting the whole of stdout: some OTP/platform
    // combinations emit ERTS startup lines before `io:format`, and a value
    // polluted by that noise would never match a recorded stamp — turning every
    // build into a full rebuild.
    let probe = "io:format(\"otp-version:~s-~s~n\", [erlang:system_info(otp_release), erlang:system_info(version)]), halt().";
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg(probe)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .find_map(|line| line.trim().strip_prefix("otp-version:"))
        // A bare "-" means both probes returned empty; treat as unknown.
        .filter(|version| !version.is_empty() && *version != "-")
        .map(str::to_string)
}

/// Discovers `.beam` files on the OTP code path and the OTP version.
///
/// Returns absolute paths to all `.beam` files in common OTP library ebin
/// directories (`stdlib`, `kernel`, etc.) plus an OTP/ERTS version key. Used to
/// find modules available for spec extraction and to key the shared type-spec
/// cache.
///
/// # Errors
///
/// Returns an error if `erl` cannot be invoked to discover the OTP lib directory.
pub fn discover_otp_beam_files() -> Result<OtpDiscovery> {
    // Apps we want type specs from. `erts` is included so `erlang.beam`
    // (BIFs like `whereis/1`, `spawn/3`, `self/0`) gets covered — its specs
    // are on disk even though `code:which(erlang)` returns `preloaded`. BT-2159.
    //
    // We probe `code:lib_dir(App)` per app rather than globbing `<lib_dir>/<app>-*`
    // because OTP layouts differ: upstream/kerl/brew put `erts-<vsn>` directly
    // under the OTP root, while Debian also mirrors it under `lib/`. `code:lib_dir/1`
    // is Erlang's canonical resolution and handles both.
    const COMMON_APPS: &[&str] = &[
        "stdlib", "kernel", "erts", "crypto", "ssl", "inets", "mnesia", "os_mon",
    ];

    let apps_atom_list = COMMON_APPS.join(",");
    // The probe prints one `otp-version:<release>-<erts>` line (the shared
    // cache key, BT-2470) followed by one ebin directory per discovered app.
    let probe = format!(
        "io:format(\"otp-version:~s-~s~n\", [erlang:system_info(otp_release), erlang:system_info(version)]), \
         lists:foreach(fun(App) -> case code:lib_dir(App) of {{error,_}} -> ok; Dir -> io:format(\"~s~n\", [filename:join(Dir, \"ebin\")]) end end, [{apps_atom_list}]), halt()."
    );

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg(&probe)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to discover OTP ebin directories")?;

    if !output.status.success() {
        let stderr_msg = String::from_utf8_lossy(&output.stderr);
        warn!(
            "erl probe failed (exit {}): {}",
            output.status,
            stderr_msg.trim()
        );
        return Ok(OtpDiscovery::default());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut version = None;
    let mut beam_files = Vec::new();
    for line in stdout.lines() {
        let line = line.trim();
        if let Some(v) = line.strip_prefix("otp-version:") {
            if !v.is_empty() {
                version = Some(v.to_string());
            }
            continue;
        }
        let ebin_dir = std::path::Path::new(line);
        if !ebin_dir.is_dir() {
            continue;
        }
        if let Ok(entries) = std::fs::read_dir(ebin_dir) {
            for file in entries.flatten() {
                let path = file.path();
                if path.extension().is_some_and(|e| e == "beam") {
                    if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                        beam_files.push(utf8);
                    }
                }
            }
        }
    }

    debug!(
        count = beam_files.len(),
        version = ?version,
        "Discovered OTP .beam files"
    );
    Ok(OtpDiscovery {
        version,
        beam_files,
    })
}

/// Discover `.beam` files from project dependency directories.
///
/// Collects beams from:
/// - Path dependency ebin directories (`_build/deps/*/ebin/`)
/// - Native Erlang ebin (`_build/dev/native/ebin/`)
/// - Rebar3 hex dep ebins (`_build/dev/native/default/lib/*/ebin/`)
///
/// The caller combines these with OTP beams before passing the full set to
/// [`extract_beam_specs`], so the [`NativeTypeRegistry`] covers both OTP and
/// project dependencies.
pub fn discover_dependency_beam_files(ebin_dirs: &[Utf8PathBuf]) -> Vec<Utf8PathBuf> {
    let mut beam_files = Vec::new();

    for ebin_dir in ebin_dirs {
        if !ebin_dir.exists() {
            continue;
        }
        let Ok(entries) = std::fs::read_dir(ebin_dir) else {
            warn!(ebin = %ebin_dir, "Failed to read dependency ebin directory");
            continue;
        };
        for file in entries.flatten() {
            let path = file.path();
            if path.extension().is_some_and(|e| e == "beam") {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                    beam_files.push(utf8);
                }
            }
        }
    }

    debug!(
        count = beam_files.len(),
        "Discovered dependency .beam files"
    );
    beam_files
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
    fn test_escape_erlang_string_null_byte() {
        assert_eq!(escape_erlang_string("\0"), "\\0");
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

    // ---- BT-2793: ADR 0100 Rule 3 `[diagnostics]` table tests ----

    /// Source with a DNU hint on a known, closed receiver (`String` has no
    /// `frobnicate`) — mirrors the fixture `lint.rs` uses for the same hint.
    const DNU_HINT_SOURCE: &str = "\"hello\" frobnicate";

    /// Source with a top-level reference to a class that doesn't exist —
    /// triggers `DiagnosticCategory::UnresolvedClass` at `Warning` severity
    /// (see `structural_validators::tests::test_unresolved_class_warns_on_unknown`).
    const UNRESOLVED_CLASS_SOURCE: &str = "NonExistentClassBt2793";

    fn diagnostics_table_ctx(
        table: crate::commands::manifest::DiagnosticsTable,
    ) -> CompileContext<'static> {
        CompileContext {
            diagnostics_overrides: table,
            ..CompileContext::default()
        }
    }

    /// Like [`diagnostics_table_ctx`], but with a non-empty
    /// `pre_loaded_classes` — `check_unresolved_classes` only runs when
    /// cross-file metadata is present (`semantic_analysis::mod.rs`'s
    /// `has_cross_file_classes` gate), matching a real multi-file package
    /// build. A single standalone file otherwise can't tell an unresolved
    /// class name apart from one defined in a sibling file it hasn't seen.
    fn diagnostics_table_ctx_with_cross_file_classes(
        table: crate::commands::manifest::DiagnosticsTable,
    ) -> CompileContext<'static> {
        use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;

        let sibling = ClassInfo {
            surface_incomplete: false,
            name: "SiblingBt2793".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            handle_scope: None,
            state: vec![],
            state_types: std::collections::HashMap::new(),
            state_has_default: std::collections::HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        };

        CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_classes: vec![sibling],
                ..ClassHierarchyContext::default()
            },
            diagnostics_overrides: table,
            ..CompileContext::default()
        }
    }

    #[test]
    fn test_diagnostics_table_absent_dnu_hint_does_not_fail_build() {
        // Baseline: with no [diagnostics] table, a bare Dnu Hint never fails
        // the build on its own (Rule 1 default).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let diags = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(crate::commands::manifest::DiagnosticsTable::new()),
            None,
        )
        .expect("Dnu hint alone must not fail the build");

        assert!(
            diags.iter().any(
                |d| d.category == Some(beamtalk_core::source_analysis::DiagnosticCategory::Dnu)
            ),
            "fixture should still produce a Dnu diagnostic: {diags:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_off_drops_dnu_diagnostic() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Off,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let diags = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        )
        .expect("off-category diagnostics must not fail the build");

        assert!(
            !diags.iter().any(
                |d| d.category == Some(beamtalk_core::source_analysis::DiagnosticCategory::Dnu)
            ),
            "dnu = \"off\" must drop the Dnu diagnostic entirely: {diags:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_expect_directive_wins_over_error_override() {
        // ADR 0100 Rule 3 precedence step 1: a site-level `@expect dnu`
        // always wins, regardless of the `[diagnostics]` table. `@expect`
        // suppression runs inside `compute_project_diagnostics`, *before*
        // `apply_diagnostics_table` — so even `dnu = "error"` never sees a
        // diagnostic that `@expect dnu` already removed.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, "@expect dnu\n\"hello\" frobnicate").unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Error,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_ok(),
            "@expect dnu must suppress the diagnostic before dnu = \"error\" ever sees it: \
             {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_error_fails_build_without_warnings_as_errors() {
        // dnu = "error" sets the category's *base* severity to Error — this
        // must fail the build unconditionally, independent of
        // --warnings-as-errors (ADR 0100 Rule 3 precedence: the table sets
        // base severity; --warnings-as-errors is a separate, later promotion
        // pass over whatever the table leaves as Warning/Hint).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Error,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "dnu = \"error\" must fail the build even without --warnings-as-errors: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_severity_floor_protects_hard_errors() {
        // BT-2793 adversarial-review finding: `ActorNew` (BT-1524 — `Actor
        // subclass new` must always fail the build) is emitted at
        // Severity::Error unconditionally, not via the Rule 1 completeness
        // ladder. A table entry like `actor-new = "warn"` must NOT silently
        // downgrade it — the severity floor keeps hard structural errors
        // out of reach of Rule 3's escalation-only table.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::ActorNew,
            crate::commands::manifest::DiagnosticSeverityOverride::Warn,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "actor_new",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "actor-new = \"warn\" must not downgrade the hard ActorNew error: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_severity_floor_ignores_off_on_hard_error() {
        // Same severity-floor guarantee for "off": must not silently drop a
        // hard-error diagnostic either.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::ActorNew,
            crate::commands::manifest::DiagnosticSeverityOverride::Off,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "actor_new",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "actor-new = \"off\" must not drop the hard ActorNew error: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_absent_unresolved_class_excluded_from_warnings_as_errors() {
        // Baseline (today's default): UnresolvedClass is excluded from
        // --warnings-as-errors promotion, so this must still succeed.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(
                crate::commands::manifest::DiagnosticsTable::new(),
            ),
            None,
        );

        assert!(
            result.is_ok(),
            "UnresolvedClass must stay excluded from --warnings-as-errors with no \
             [diagnostics] override: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_explicit_entry_lifts_gradual_migration_exclusion() {
        // An explicit [diagnostics] entry for a gradual-migration category
        // (UnresolvedClass) is a deliberate opt back in to promotion — it
        // must win over the default --warnings-as-errors exclusion (ADR 0100
        // Rule 3: "explicit table value wins over the exclusion").
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Warn,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_err(),
            "explicit unresolved-class = \"warn\" + --warnings-as-errors must fail: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_hint_override_also_lifts_gradual_migration_exclusion() {
        // BT-2793 adversarial-review finding: the exclusion-lift keys off
        // table *presence* for the category, not the chosen severity value —
        // per ADR 0100 Rule 3's literal precedence note ("the explicit table
        // value wins over the exclusion"), so `unresolved-class = "hint"`
        // (not just `"warn"`) also opts the category back into
        // --warnings-as-errors promotion. This is intentional (see the
        // `[diagnostics]` section of docs/beamtalk-packages.md) — pinned here
        // so the behaviour doesn't drift silently.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Hint,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_err(),
            "unresolved-class = \"hint\" + --warnings-as-errors must also fail \
             (any explicit entry lifts the exclusion, not just \"warn\"): {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_lint_override_stays_inert_under_warnings_as_errors() {
        // Contrast with the "hint" case above: `"lint"` rewrites the
        // diagnostic to `Severity::Lint`, which `--warnings-as-errors` never
        // promotes (it only promotes `Warning`/`Hint`) — so even though the
        // exclusion is lifted, the build still succeeds. Lint diagnostics are
        // also suppressed from normal build output entirely (shown only by
        // `beamtalk lint`).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Lint,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_ok(),
            "unresolved-class = \"lint\" must stay inert under --warnings-as-errors \
             (Lint severity is never promoted): {result:?}"
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
        let beam_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        let specs_line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";
        cache.store("lists", beam_path, 12345, 0, specs_line);

        // Cache hit with matching mtime
        let result = cache.lookup("lists", beam_path, 12345, 0);
        assert_eq!(result, Some(specs_line.to_string()));

        // Cache miss with different mtime (seconds)
        let result = cache.lookup("lists", beam_path, 99999, 0);
        assert!(result.is_none(), "Different mtime should be a cache miss");

        // Cache miss with different mtime (nanos only)
        let result = cache.lookup("lists", beam_path, 12345, 1);
        assert!(result.is_none(), "Different nanos should be a cache miss");

        // Cache miss for unknown module
        let maps_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/maps.beam");
        let result = cache.lookup("maps", maps_path, 12345, 0);
        assert!(result.is_none(), "Unknown module should be a cache miss");
    }

    #[test]
    fn type_cache_invalidates_on_mtime_change() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);
        let beam_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        cache.store("lists", beam_path, 100, 0, "line1");
        assert_eq!(
            cache.lookup("lists", beam_path, 100, 0),
            Some("line1".to_string())
        );

        // Overwrite with new mtime
        cache.store("lists", beam_path, 200, 0, "line2");
        assert!(cache.lookup("lists", beam_path, 100, 0).is_none());
        assert_eq!(
            cache.lookup("lists", beam_path, 200, 0),
            Some("line2".to_string())
        );
    }

    #[test]
    fn type_cache_handles_empty_specs_line() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);
        let beam_path = Utf8Path::new("/some/path/no_specs.beam");

        cache.store("no_specs", beam_path, 100, 0, "");
        assert_eq!(
            cache.lookup("no_specs", beam_path, 100, 0),
            Some(String::new())
        );
    }

    #[test]
    fn type_cache_different_paths_do_not_collide() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);

        let path_a = Utf8Path::new("/project_a/_build/my_app.beam");
        let path_b = Utf8Path::new("/project_b/_build/my_app.beam");

        cache.store("my_app", path_a, 100, 0, "specs_from_a");
        cache.store("my_app", path_b, 100, 0, "specs_from_b");

        // Each path returns its own cached specs
        assert_eq!(
            cache.lookup("my_app", path_a, 100, 0),
            Some("specs_from_a".to_string()),
            "Path A should return its own cached specs"
        );
        assert_eq!(
            cache.lookup("my_app", path_b, 100, 0),
            Some("specs_from_b".to_string()),
            "Path B should return its own cached specs"
        );
    }

    /// BT-2470: a fresh project (empty local tier) resolves OTP specs via the
    /// shared tier, and the shared hit is mirrored into the local tier so the
    /// LSP and `beamtalk lint` keep finding specs in `_build/type_cache/`.
    #[test]
    fn type_cache_shared_tier_serves_and_mirrors_to_local() {
        let temp = TempDir::new().unwrap();
        let shared = Utf8PathBuf::from_path_buf(temp.path().join("shared")).unwrap();
        let beam = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        // A prior build in some project populates both its local tier and the
        // shared tier.
        let producer_local = Utf8PathBuf::from_path_buf(temp.path().join("producer")).unwrap();
        let producer = TypeCache::with_shared(producer_local, shared.clone());
        producer.store("lists", beam, 200, 5, "specs_for_lists");

        // A freshly cloned workspace has an empty local tier but the same shared
        // tier — the lookup still succeeds.
        let fresh_local = Utf8PathBuf::from_path_buf(temp.path().join("fresh")).unwrap();
        let consumer = TypeCache::with_shared(fresh_local.clone(), shared);
        assert_eq!(
            consumer.lookup("lists", beam, 200, 5),
            Some("specs_for_lists".to_string()),
            "shared tier should serve a fresh project's local miss"
        );

        // The shared hit was mirrored into the fresh local tier: a local-only
        // cache (no shared tier) now finds it too.
        let local_only = TypeCache::new(fresh_local);
        assert_eq!(
            local_only.lookup("lists", beam, 200, 5),
            Some("specs_for_lists".to_string()),
            "shared hit should be mirrored into the local tier for the LSP/lint"
        );
    }

    #[test]
    #[serial_test::serial(beamtalk_cache_env)]
    fn shared_otp_cache_dir_uses_env_override_and_sanitises_version() {
        let temp = TempDir::new().unwrap();
        let base = temp.path().to_string_lossy().to_string();
        // SAFETY: serialised via #[serial]; the var is removed before returning.
        unsafe {
            std::env::set_var("BEAMTALK_CACHE_DIR", &base);
        }
        let dir = shared_otp_cache_dir("27/15.0:weird");
        // SAFETY: serialised via #[serial]; restores the unset state.
        unsafe {
            std::env::remove_var("BEAMTALK_CACHE_DIR");
        }
        let dir = dir.expect("shared cache dir should resolve under the override");
        assert!(
            dir.starts_with(&base),
            "shared cache dir should honour BEAMTALK_CACHE_DIR: {dir}"
        );
        // Compare on path components, not a slash-joined string, so the
        // assertion holds on Windows (where `join` uses `\`) as well as Unix.
        let tail: Vec<&str> = dir.components().rev().take(3).map(|c| c.as_str()).collect();
        assert_eq!(
            tail,
            vec!["27-15.0-weird", "otp-specs", "beamtalk"],
            "version key should be filesystem-sanitised: {dir}"
        );
    }

    #[test]
    #[serial_test::serial(beamtalk_cache_env)]
    fn shared_otp_cache_dir_rejects_empty_version() {
        let temp = TempDir::new().unwrap();
        // SAFETY: serialised via #[serial]; the var is removed before returning.
        unsafe {
            std::env::set_var("BEAMTALK_CACHE_DIR", temp.path());
        }
        let dir = shared_otp_cache_dir("");
        // SAFETY: serialised via #[serial]; restores the unset state.
        unsafe {
            std::env::remove_var("BEAMTALK_CACHE_DIR");
        }
        assert!(dir.is_none(), "empty version must not yield a cache dir");
    }

    /// BT-2159: `erts` must be in the OTP discovery set so `erlang.beam`
    /// (BIFs like `whereis/1`, `spawn/3`, `self/0`) gets spec extraction.
    /// `code:which(erlang)` returns `preloaded`, but the `.beam` exists in
    /// `<erts-app>/ebin/erlang.beam` with full abstract code.
    #[test]
    fn discover_otp_beam_files_includes_erts() {
        let Ok(discovery) = discover_otp_beam_files() else {
            // Skip only when `erl` cannot be spawned (test env without Erlang).
            // A successful probe that returns zero beams is a real failure and
            // is caught by the assert below.
            return;
        };
        let beams = &discovery.beam_files;
        let has_erlang = beams
            .iter()
            .any(|p| p.file_stem().is_some_and(|s| s == "erlang"));
        assert!(
            has_erlang,
            "discover_otp_beam_files must include erts/ebin/erlang.beam so BIF specs reach NativeTypeRegistry (BT-2159). Got {} beams: {:?}",
            beams.len(),
            beams
        );
        // BT-2470: a successful probe must also report the OTP version key
        // used to scope the shared type-spec cache.
        assert!(
            discovery.version.is_some(),
            "discover_otp_beam_files must report an OTP version key for the shared cache"
        );
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

    #[test]
    fn sanitize_module_name_plain() {
        assert_eq!(sanitize_module_name("lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_unix_path() {
        assert_eq!(sanitize_module_name("/usr/lib/erlang/lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_windows_path() {
        assert_eq!(sanitize_module_name("C:\\otp\\lib\\lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_mixed_separators() {
        assert_eq!(sanitize_module_name("/usr/lib\\erlang/lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_empty_string() {
        assert_eq!(sanitize_module_name(""), "");
    }

    #[test]
    fn discover_dependency_beam_files_empty_dirs() {
        let result = discover_dependency_beam_files(&[]);
        assert!(result.is_empty());
    }

    #[test]
    fn discover_dependency_beam_files_finds_beams() {
        let temp = TempDir::new().unwrap();
        let ebin = temp.path().join("ebin");
        fs::create_dir_all(&ebin).unwrap();
        fs::write(ebin.join("my_mod.beam"), b"fake beam").unwrap();
        fs::write(ebin.join("other.erl"), b"not a beam").unwrap();

        let ebin_path = Utf8PathBuf::from_path_buf(ebin).unwrap();
        let result = discover_dependency_beam_files(&[ebin_path]);
        assert_eq!(result.len(), 1);
        assert!(result[0].as_str().ends_with("my_mod.beam"));
    }

    #[test]
    fn discover_dependency_beam_files_skips_missing_dirs() {
        let temp = TempDir::new().unwrap();
        let nonexistent = Utf8PathBuf::from_path_buf(temp.path().join("missing")).unwrap();
        let result = discover_dependency_beam_files(&[nonexistent]);
        assert!(result.is_empty());
    }
}
