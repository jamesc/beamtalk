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

use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc;
use std::thread;
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
    /// Type alias declarations (`type Name = ...`) from other source files in
    /// the same package (BT-2928, ADR 0108). Seeded into the `AliasRegistry`
    /// during semantic analysis so a cross-file alias reference resolves
    /// through `resolve_type_annotation` instead of staying an opaque,
    /// unresolved name. Structurally a `ctx.hierarchy.*` field like
    /// `pre_loaded_protocols` above, but populated differently in practice:
    /// the CLI build path (`execute_build_passes`) leaves `pre_loaded_protocols`
    /// empty while fully populating `pre_loaded_aliases` from every source
    /// file in the compilation unit, including the file being compiled —
    /// `analyse_full`'s merge order lets the module's own declaration take
    /// precedence over its duplicate pre-loaded entry.
    pub pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
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
#[allow(clippy::too_many_arguments)]
pub fn write_core_erlang_with_bindings(
    module: &beamtalk_core::ast::Module,
    module_name: &str,
    output_path: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
    hierarchy: &ClassHierarchyContext,
    source: Option<(&str, Option<&str>)>,
    native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
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
            // BT-2932: thread cross-module type aliases through to codegen so
            // an alias-typed annotation referencing a `type Name = ...` from
            // another module in the same compilation unit resolves to a
            // `user_type` reference in generated `-spec`/`-type` attributes
            // instead of falling through to `any()` — mirrors
            // `pre_loaded_classes` immediately above.
            .with_pre_loaded_aliases(hierarchy.pre_loaded_aliases.clone())
            .with_native_type_registry(native_type_registry)
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
        // BT-2928: cross-file/package type aliases from Pass 1 — see
        // `ClassHierarchyContext::pre_loaded_aliases`'s doc. `analyse_full`
        // filters out any name the current module redeclares itself, so no
        // "current file's own aliases" pre-filter is needed here (mirrors
        // `pre_loaded_protocols` immediately above).
        pre_loaded_aliases: ctx.hierarchy.pre_loaded_aliases.clone(),
        cross_file_extensions: ctx.hierarchy.extension_index.clone(),
        native_type_registry: ctx.native_type_registry.clone(),
        dep_registry: ctx.dep_registry,
        strict_deps: ctx.strict_deps,
        // ADR 0100 Rule 3 (BT-2793) / BT-2800: `compute_project_diagnostics`
        // applies this table itself (after `@expect` suppression, before
        // returning) — the single shared pipeline both the CLI and the LSP
        // call, so severity can never drift between the two surfaces.
        diagnostics_overrides: ctx.diagnostics_overrides.clone(),
    };
    diagnostics = beamtalk_core::queries::diagnostic_provider::compute_project_diagnostics(
        &module,
        diagnostics,
        &diag_ctx,
    );

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
    // BT-2932: `pre_loaded_aliases` is *not* filtered the way `cross_file_classes`
    // is — it already includes the current file's own aliases (see
    // `ClassHierarchyContext::pre_loaded_aliases`'s doc), and codegen's merge
    // (`AliasRegistry::from_module_declarations_with_pre_loaded`) lets the
    // module's own declaration take precedence over its duplicate pre-loaded
    // entry, mirroring how semantic analysis's `AnalysisContext::pre_loaded_aliases`
    // already handles the same list.
    let codegen_hierarchy = ClassHierarchyContext {
        class_module_index: ctx.hierarchy.class_module_index.clone(),
        class_superclass_index: ctx.hierarchy.class_superclass_index.clone(),
        pre_loaded_classes: cross_file_classes,
        pre_loaded_aliases: ctx.hierarchy.pre_loaded_aliases.clone(),
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
        ctx.native_type_registry.clone(),
    )
    .wrap_err_with(|| format!("Failed to generate Core Erlang for '{source_path}'"))?;

    debug!("Generated Core Erlang: {}", core_output);
    Ok(returned_diags)
}

// BT-2858: OTP/dependency Erlang type-spec extraction moved to the
// `beamtalk_cli` lib crate so `beamtalk-mcp` can share it — mirrors the
// `dependency_classes`/`build_layout`/`manifest` moves done for BT-2823/
// BT-2836. Re-exported here so existing `beam_compiler::X` references
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
}
