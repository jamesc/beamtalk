// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stdlib build orchestration and the up-to-date check.
//!
//! **DDD Context:** CLI / Build System
//!
//! Drives the `beamtalk build-stdlib` pipeline: discovers `stdlib/src/*.bt`
//! files, decides whether the previous build is still fresh
//! ([`is_stdlib_up_to_date`]), compiles every file, and hands the extracted
//! class metadata to the generators ([`super::generators`]).

use super::extract_stdlib_type_specs;
use super::generators::{
    generate_app_file, generate_app_src_file, generate_builtins_rs, generate_class_vars_keys_hrl,
    generate_erlang_builtins_hrl, generate_supervisor_config_hrl,
    provision_bearing_protocol_file_names,
};
use super::metadata::{
    AliasSource, ClassMeta, alias_source_texts_sorted_by_name, collect_stdlib_alias_sources,
    collect_stdlib_protocol_infos, extract_class_metadata, is_protocol_only_file,
    mark_value_subclasses,
};
use crate::beam_compiler::{
    BeamCompiler, ClassHierarchyContext, CompileContext, compile_source_with_bindings,
};
use crate::commands::build::build_alias_metadata;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::BTreeSet;
use std::fs;
use std::time::SystemTime;
use tracing::{info, instrument};

/// Default path to stdlib source files (relative to project root).
pub(super) const STDLIB_SOURCE_DIR: &str = "stdlib/src";

/// Default output path for compiled stdlib BEAM files (relative to project root).
const STDLIB_EBIN_DIR: &str = "runtime/apps/beamtalk_stdlib/ebin";

/// The `beamtalk_stdlib` OTP app's Erlang source directory (relative to
/// project root): where the generated `.app.src` is written, and whose
/// `*.erl` modules the generated `.app`'s `{modules, [...]}` lists alongside
/// the class modules.
const STDLIB_APP_SRC_DIR: &str = "runtime/apps/beamtalk_stdlib/src";

/// The runtime root [`STDLIB_EBIN_DIR`] lives under, for resolving the
/// dev-layout rebar3 output directory (see [`rebar3_stdlib_ebin`]).
const RUNTIME_DIR: &str = "runtime";

/// The generated `beamtalk_stdlib` app resource file's name.
pub(super) const STDLIB_APP_FILE: &str = "beamtalk_stdlib.app";

/// Build the standard library.
///
/// Finds all `.bt` files in `lib/`, compiles them with stdlib mode enabled,
/// and writes `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
/// Skips the build if all outputs are newer than all inputs (incremental).
#[instrument(skip_all)]
pub fn build_stdlib(quiet: bool, warnings_as_errors: bool) -> Result<()> {
    info!("Starting stdlib build");

    let lib_dir = Utf8PathBuf::from(STDLIB_SOURCE_DIR);
    let ebin_dir = Utf8PathBuf::from(STDLIB_EBIN_DIR);

    if !lib_dir.exists() {
        miette::bail!("Standard library source directory '{}' not found", lib_dir);
    }

    // Find all .bt files in lib/
    let source_files = find_stdlib_files(&lib_dir)?;

    if source_files.is_empty() {
        println!("No .bt source files found in '{lib_dir}'");
        return Ok(());
    }

    check_duplicate_module_names(&source_files)?;

    // Incremental build: skip if all outputs are newer than all inputs.
    // BT-3509's corpus `.core` diff harness (`BEAMTALK_CORE_SNAPSHOT_DIR`,
    // see `util::core_output_dir`'s doc) always wants a full recompile —
    // its whole job is comparing freshly generated `.core` output, and an
    // incremental skip would leave its snapshot stale or empty.
    let want_core_snapshot = std::env::var_os("BEAMTALK_CORE_SNAPSHOT_DIR").is_some();
    if !want_core_snapshot
        && is_stdlib_up_to_date(
            &ebin_dir,
            &source_files,
            discover_runtime_ebin_dirs().as_deref(),
        )
    {
        // Even a skipped build re-syncs rebar3's copy of the app: a
        // `rebar3 compile` since the last stdlib build (`just build-erlang`)
        // re-copies the class beams and regenerates its own `.app`.
        sync_rebar3_stdlib_ebin(&ebin_dir, &rebar3_stdlib_ebin())?;
        println!("Stdlib up to date (skipped)");
        return Ok(());
    }

    info!(count = source_files.len(), "Found stdlib source files");

    // BT-3509: `.core` files here are a throwaway intermediate to `.beam`
    // unless the corpus `.core` diff harness asks to keep them — see
    // `util::core_output_dir`'s doc. Resolved before `ebin/` is touched so
    // its snapshot-only mode (BT-3887, below) leaves `ebin/` alone entirely.
    let (temp_path, core_output_dir_guard) = crate::commands::util::core_output_dir()?;

    if !core_output_dir_guard.snapshot_only() {
        // Create ebin directory
        fs::create_dir_all(&ebin_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create ebin directory '{ebin_dir}'"))?;

        // Clean stale .beam files from previous builds (e.g. renamed/removed .bt sources)
        clean_ebin_dir(&ebin_dir)?;
    }

    if !quiet {
        println!("Compiling {} stdlib module(s)...", source_files.len());
    }

    // Compiler options: stdlib mode enabled
    let options = stdlib_compiler_options(warnings_as_errors);

    // ADR 0007 Phase 3: Build primitive binding table from ALL stdlib sources.
    // This is used during compilation so that @primitive expressions in method bodies
    // can reference the runtime dispatch modules.
    info!("Building primitive binding table from stdlib sources");
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::load_from_directory(
        lib_dir.as_std_path(),
    );
    info!(
        binding_count = bindings.len(),
        "Loaded primitive bindings from stdlib"
    );
    // ADR 0137 §4: a `no_user_code` mark is a promise the compiler relies on to
    // skip the actor-state cell bracket, so a mark that cannot hold fails the build.
    let mark_problems = bindings.check_no_user_code_marks();
    if !mark_problems.is_empty() {
        miette::bail!(
            "invalid `no_user_code` primitive marks:\n  {}",
            mark_problems.join("\n  ")
        );
    }

    // Extract native type specs from runtime .beam files so FFI calls in stdlib
    // get proper type inference instead of Dynamic (ADR 0075).
    let native_type_registry = extract_stdlib_type_specs();

    // Live same-run alias pre-pass — see its doc for why.
    let alias_sources = collect_stdlib_alias_sources(&source_files)?;
    // Live same-run protocol pre-pass, mirroring the alias pre-pass
    // immediately above — see `collect_stdlib_protocol_infos`'s doc for why.
    let (protocol_infos, protocol_defs, protocol_sources) =
        collect_stdlib_protocol_infos(&source_files);
    let protocol_file_names = provision_bearing_protocol_file_names(&protocol_sources);
    let compile_ctx = CompileContext {
        native_type_registry: native_type_registry.map(std::sync::Arc::new),
        hierarchy: ClassHierarchyContext {
            pre_loaded_protocols: protocol_infos,
            pre_loaded_protocol_defs: protocol_defs,
            pre_loaded_protocol_sources: protocol_sources,
            pre_loaded_aliases: stdlib_pre_loaded_aliases(&alias_sources),
            ..ClassHierarchyContext::default()
        },
        ..CompileContext::default()
    };

    // Compile each .bt file to .core (files are independent, no ordering required)
    let (core_files, class_metadata, protocol_modules) = compile_all_stdlib_files(
        &source_files,
        &temp_path,
        quiet,
        &options,
        &bindings,
        &compile_ctx,
    )?;

    // BT-3887: the corpus `.core` diff harness's snapshot-only mode wants
    // nothing past this point — every `.core` is on disk now, and the
    // `.beam`, `.app`/`.app.src`, `builtins.rs` and `.hrl` outputs below
    // are exactly what it must *not* produce (it runs inside checkouts it
    // doesn't own, and `ebin/` was left untouched above for the same
    // reason). See `util::CoreOutputDir::snapshot_only`.
    if core_output_dir_guard.snapshot_only() {
        println!(
            "Snapshotted {} stdlib module(s) to .core only (BEAMTALK_CORE_SNAPSHOT_ONLY=1); \
             skipped .beam/.app/generated-source output",
            source_files.len()
        );
        return Ok(());
    }

    // Batch compile .core → .beam into ebin directory
    info!("Compiling Core Erlang to BEAM");
    let compiler = BeamCompiler::new(ebin_dir.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile stdlib Core Erlang to BEAM")?;

    write_stdlib_generated_outputs(
        &ebin_dir,
        &source_files,
        &class_metadata,
        &protocol_modules,
        alias_sources,
        &protocol_file_names,
    )?;

    sync_rebar3_stdlib_ebin(&ebin_dir, &rebar3_stdlib_ebin())?;

    println!("Built {} stdlib modules", source_files.len());

    Ok(())
}

/// Everything `build_stdlib` writes *after* the `.beam` compile: the
/// `.app`/`.app.src` metadata and the generated Rust/Erlang builtin tables.
/// None of it is produced in snapshot-only mode (BT-3887).
fn write_stdlib_generated_outputs(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
    alias_sources: Vec<AliasSource>,
    protocol_file_names: &[String],
) -> Result<()> {
    // `{type_aliases, [...]}` `.app`/`.app.src` env metadata — the
    // same `build_alias_metadata` extraction the ordinary `beamtalk build`
    // pipeline already uses (`build.rs`'s `ClassIndexResult::all_alias_infos`
    // call site), independently re-parsing `source_files` for doc
    // comments/display expansions `AliasSource`/`ClassHierarchyContext`
    // in `build_stdlib` don't carry. This is what lets
    // `beamtalk_repl_state:new/3` (the REPL/workspace session's
    // alias-seeding path) and `browse-type-aliases` read stdlib's own
    // aliases back via `application:get_env(beamtalk_stdlib, type_aliases)`.
    let alias_metadata = build_alias_metadata(source_files);

    let app_src_dir = Utf8PathBuf::from(STDLIB_APP_SRC_DIR);
    generate_app_file(
        ebin_dir,
        source_files,
        &stdlib_erlang_modules(&app_src_dir)?,
        class_metadata,
        protocol_modules,
        &alias_metadata,
    )?;
    // Also update .app.src so rebar3 picks up the classes env
    if app_src_dir.exists() {
        generate_app_src_file(
            &app_src_dir,
            class_metadata,
            protocol_modules,
            &alias_metadata,
        )?;
    }

    // Generate Rust builtins file from parsed class metadata and aliases.
    generate_builtins_rs(
        class_metadata,
        &alias_source_texts_sorted_by_name(alias_sources),
        protocol_file_names,
    )?;

    // Generate the Erlang-side twin of the same builtin-class list,
    // replacing the hand-typed `beamtalk_class_metadata:all_builtins/0` table.
    generate_erlang_builtins_hrl(class_metadata)?;
    generate_class_vars_keys_hrl()?;
    generate_supervisor_config_hrl()
}

/// Compiles every stdlib source file to Core Erlang, collecting class
/// metadata and protocol module names along the way.
///
/// Protocol-only files (e.g. `printable.bt`) have no class definition — they
/// are still compiled so the protocol gets registered at runtime, but
/// contribute no `ClassMeta`. Files are independent (no compile ordering
/// required); cross-file class/alias visibility comes entirely from
/// `compile_ctx` (built by the caller before this runs).
pub(super) fn compile_all_stdlib_files(
    source_files: &[Utf8PathBuf],
    temp_path: &Utf8Path,
    quiet: bool,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    compile_ctx: &CompileContext<'_>,
) -> Result<(Vec<Utf8PathBuf>, Vec<ClassMeta>, Vec<String>)> {
    let mut core_files = Vec::new();
    let mut class_metadata = Vec::new();
    let mut protocol_modules = Vec::new();
    // Mirrors `semantic_analysis::mod.rs`'s own `external_protocols` build
    // from `pre_loaded_protocol_defs` — see `extract_class_metadata`'s doc
    // for why this generator needs it too (ADR 0127 §10a; BT-3594).
    let external_protocols: std::collections::HashMap<
        ecow::EcoString,
        beamtalk_core::ast::ProtocolDefinition,
    > = compile_ctx
        .hierarchy
        .pre_loaded_protocol_defs
        .iter()
        .map(|p| (p.name.name.clone(), p.clone()))
        .collect();
    for source_file in source_files {
        let module_name = module_name_from_path(source_file)?;
        let core_file = temp_path.join(format!("{module_name}.core"));

        if is_protocol_only_file(source_file)? {
            if !quiet {
                println!("  Compiling {source_file} (protocol)...");
            }
            compile_stdlib_file(
                source_file,
                &module_name,
                &core_file,
                options,
                bindings,
                compile_ctx,
            )?;
            core_files.push(core_file);
            protocol_modules.push(module_name);
            continue;
        }

        // Extract class metadata (class_name, superclass) before compilation
        let meta = extract_class_metadata(source_file, &module_name, &external_protocols)?;

        // ADR 0119 step 2: validate the file-stem-derived
        // `module_name` (computed above by `module_name_from_path`, from the
        // file's path alone) against the closed-form name a real parse of
        // the class would produce. The stdlib arm never becomes a free
        // lookup — the Erlang runtime derives `bt@stdlib@{snake}` closed-form
        // with no registry to consult (ADR 0119 Context) — so this doesn't
        // change what module a stdlib class compiles to; it catches a
        // mismatched-rename bug shape (a `.bt` file renamed without renaming,
        // or vice versa, the class it declares) at `build-stdlib` time instead of
        // silently trusting the file stem.
        beamtalk_core::semantic_analysis::validate_stdlib_module_name(
            &meta.class_name,
            &beamtalk_core::semantic_analysis::ModuleName::Generated(module_name.clone()),
        )
        .into_diagnostic()
        .wrap_err_with(|| format!("in stdlib source file '{source_file}'"))?;

        class_metadata.push(meta);

        if !quiet {
            println!("  Compiling {source_file}...");
        }
        compile_stdlib_file(
            source_file,
            &module_name,
            &core_file,
            options,
            bindings,
            compile_ctx,
        )?;
        core_files.push(core_file);
    }
    mark_value_subclasses(&mut class_metadata);
    Ok((core_files, class_metadata, protocol_modules))
}

/// Remove all `.beam` and `.app` files from the ebin directory.
///
/// Ensures no stale artifacts remain from renamed or removed `.bt` sources.
pub(super) fn clean_ebin_dir(ebin_dir: &Utf8Path) -> Result<()> {
    for entry in fs::read_dir(ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read ebin directory '{ebin_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if let Some(ext) = path.extension() {
            if ext == "beam" || ext == "app" {
                fs::remove_file(&path)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("Failed to remove '{}'", path.display()))?;
            }
        }
    }
    Ok(())
}

/// rebar3's dev-layout output directory for the `beamtalk_stdlib` app
/// (`runtime/_build/default/lib/beamtalk_stdlib/ebin`) — the same
/// `stdlib_erlang_ebin` the dev runtime puts on its code path.
fn rebar3_stdlib_ebin() -> std::path::PathBuf {
    beamtalk_cli::repl_startup::beam_paths_for_layout(
        std::path::Path::new(RUNTIME_DIR),
        beamtalk_cli::repl_startup::RuntimeLayout::Dev,
    )
    .stdlib_erlang_ebin
}

/// Bring rebar3's copy of the `beamtalk_stdlib` app in line with this
/// build's output (BT-4384).
///
/// `rebar3 compile` copies `ebin_dir` into its own
/// `_build/default/lib/beamtalk_stdlib/ebin` and compiles `.app.src` to a
/// `.app` there. `just build-erlang` runs it *before* `build-stdlib`, so
/// both copies are one stdlib build behind: a class added since is missing
/// from the `.app`'s `classes` env (never activated), a removed one is still
/// listed (activation fails at startup), and stale class beams linger. The
/// dev runtime puts this directory *ahead* of `ebin_dir` on the code path
/// (`beam_pa_args`'s later `-pa` wins), so its `.app` and class beams are
/// the ones loaded.
///
/// So, when that directory exists: delete every `bt@*.beam` in it (their
/// one home is `ebin_dir`, which is also on the code path), and replace its
/// `.app` with the one this build generated. A later `rebar3 compile`
/// re-copies the beams and regenerates the `.app` from the (now current)
/// `.app.src` — consistent with this build either way.
pub(super) fn sync_rebar3_stdlib_ebin(
    ebin_dir: &Utf8Path,
    rebar3_ebin: &std::path::Path,
) -> Result<()> {
    if !rebar3_ebin.is_dir() {
        return Ok(());
    }
    let entries = fs::read_dir(rebar3_ebin)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{}'", rebar3_ebin.display()))?;
    for entry in entries {
        let path = entry.into_diagnostic()?.path();
        if is_class_beam(&path) {
            fs::remove_file(&path)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to remove '{}'", path.display()))?;
        }
    }
    let app_file = ebin_dir.join(STDLIB_APP_FILE);
    if app_file.exists() {
        let dest = rebar3_ebin.join(STDLIB_APP_FILE);
        fs::copy(&app_file, &dest)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to copy '{app_file}' to '{}'", dest.display()))?;
    }
    Ok(())
}

/// Whether `path` is a Beamtalk-compiled class beam (`bt@*.beam`).
fn is_class_beam(path: &std::path::Path) -> bool {
    path.extension().is_some_and(|ext| ext == "beam")
        && path
            .file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with("bt@"))
}

/// The `beamtalk_stdlib` app's Erlang modules: one per `*.erl` in its
/// source directory (what rebar3 compiles into the app). Empty when the
/// directory doesn't exist.
pub(super) fn stdlib_erlang_modules(src_dir: &Utf8Path) -> Result<Vec<String>> {
    if !src_dir.is_dir() {
        return Ok(Vec::new());
    }
    let mut modules = Vec::new();
    for entry in fs::read_dir(src_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{src_dir}'"))?
    {
        let path = entry.into_diagnostic()?.path();
        if path.extension().is_some_and(|ext| ext == "erl") {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                modules.push(stem.to_string());
            }
        }
    }
    Ok(modules)
}

/// Discover the runtime/stdlib-Erlang/workspace/compiler `ebin/` directories
/// that `is_stdlib_up_to_date`'s final check treats as build inputs — a
/// `.beam` file in any of them newer than the stdlib output means native
/// specs may have changed underneath it.
///
/// Returns `None` when the runtime layout can't be located at all (dev vs.
/// installed candidates all missing, or an explicit `BEAMTALK_RUNTIME_DIR`
/// pointing nowhere useful); callers should treat that as "force rebuild to
/// be safe", same as any other missing-input case in this file.
///
/// Split out from `is_stdlib_up_to_date` itself so the directory
/// *list* is an injectable parameter there: this function's real-filesystem
/// discovery (`beamtalk_cli::repl_startup::find_runtime_dir_with_layout`) has
/// no test seam of its own, but `is_stdlib_up_to_date` no longer needs one —
/// unit tests can hand it a synthetic list of temp directories directly.
pub(super) fn discover_runtime_ebin_dirs() -> Option<Vec<std::path::PathBuf>> {
    use beamtalk_cli::repl_startup;
    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().ok()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    Some(vec![
        paths.runtime_ebin,
        paths.stdlib_erlang_ebin,
        paths.workspace_ebin,
        paths.compiler_ebin,
    ])
}

/// Check if the stdlib build is up to date (all outputs newer than all inputs).
///
/// Inputs: `lib/*.bt` source files, the compiler binary (`current_exe`), and
/// the runtime `.beam` directories in `runtime_ebin_dirs` (see
/// [`discover_runtime_ebin_dirs`] for how the real build discovers these).
/// Output: the `ebin/` directory modification time.
///
/// `runtime_ebin_dirs` being `None` means the caller couldn't discover the
/// runtime layout at all and is itself forcing a rebuild to be safe — same
/// as any other missing-input case below.
///
/// Returns `false` (needs rebuild) if any input is missing or any error occurs.
pub(super) fn is_stdlib_up_to_date(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    runtime_ebin_dirs: Option<&[std::path::PathBuf]>,
) -> bool {
    // Must have .beam files already
    let Ok(entries) = fs::read_dir(ebin_dir) else {
        return false;
    };
    let beam_modules: BTreeSet<String> = entries
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "beam"))
        .filter_map(|p| p.file_stem().and_then(|s| s.to_str()).map(str::to_string))
        .collect();
    if beam_modules.is_empty() {
        return false;
    }

    // Detect added/deleted/renamed source files: the beams must be exactly
    // one per source module. A count alone misses a rename (`mv` keeps the
    // source's mtime, so the checks below don't notice either), which would
    // leave the old name's beam installed and the new one never built.
    let Ok(source_modules) = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<BTreeSet<String>>>()
    else {
        return false;
    };
    if beam_modules != source_modules {
        info!(
            beam_count = beam_modules.len(),
            source_count = source_modules.len(),
            "Beam/source module set mismatch — forcing rebuild"
        );
        return false;
    }

    // The generated `.app` is an output too — the one installed and the one
    // dev mode loads (BT-4384) — so a missing one is never "up to date".
    if !ebin_dir.join(STDLIB_APP_FILE).is_file() {
        info!("Stdlib .app missing — forcing rebuild");
        return false;
    }

    // Get the oldest .beam output mtime
    let Some(oldest_output) = oldest_mtime_in_dir(ebin_dir, "beam") else {
        return false;
    };

    // Check source files
    for src in source_files {
        match fs::metadata(src.as_std_path()).and_then(|m| m.modified()) {
            Ok(t) if t > oldest_output => {
                info!(file = %src, "Source newer than stdlib output");
                return false;
            }
            Err(_) => return false,
            _ => {}
        }
    }

    // Check compiler binary — if we can't locate it, force rebuild to be safe
    match std::env::current_exe() {
        Ok(exe) => match fs::metadata(&exe).and_then(|m| m.modified()) {
            Ok(t) if t > oldest_output => {
                info!("Compiler binary newer than stdlib output");
                return false;
            }
            Err(_) => return false,
            _ => {}
        },
        Err(_) => return false,
    }

    // Check runtime .beam files — if any runtime/stdlib/workspace/compiler ebin
    // has a newer .beam, force rebuild (specs may have changed).
    // Caller passes layout-aware discovery so invalidation matches spec
    // extraction paths; `None` means discovery itself failed.
    let Some(runtime_ebins) = runtime_ebin_dirs else {
        return false;
    };
    for runtime_ebin in runtime_ebins {
        let Ok(entries) = fs::read_dir(runtime_ebin) else {
            continue;
        };
        for entry in entries.flatten() {
            // Class beams (`bt@*`) are this build's own output, which
            // `rebar3 compile` copies into the stdlib Erlang ebin — not a
            // native-spec input.
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "beam") && !is_class_beam(&path) {
                match entry.metadata().and_then(|m| m.modified()) {
                    Ok(t) if t > oldest_output => {
                        info!("Runtime .beam newer than stdlib output");
                        return false;
                    }
                    Err(_) => return false,
                    _ => {}
                }
            }
        }
    }

    info!("Stdlib is up to date");
    true
}

/// Find the oldest modification time of files with the given extension in a directory.
pub(super) fn oldest_mtime_in_dir(dir: &Utf8Path, ext: &str) -> Option<SystemTime> {
    let mut oldest: Option<SystemTime> = None;
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        if entry.path().extension().is_some_and(|e| e == ext) {
            if let Ok(mtime) = entry.metadata().and_then(|m| m.modified()) {
                oldest = Some(match oldest {
                    Some(prev) if mtime < prev => mtime,
                    Some(prev) => prev,
                    None => mtime,
                });
            }
        }
    }
    oldest
}

/// Find all `.bt` files in the stdlib source directory, recursively.
///
/// Recursive so `stdlib/src/` can be grouped into subdirectories
/// (`collections/`, `actors/`, …) without silently dropping classes from the
/// build. Subdirectories are purely editorial — they do *not* affect module
/// names; see [`module_name_from_path`].
pub(super) fn find_stdlib_files(lib_dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    beamtalk_core::file_walker::FileWalker::source_files().walk(lib_dir)
}

/// Reject two stdlib sources that would compile to the same module.
///
/// Module names come from the file stem alone, so `collections/array.bt` and
/// `legacy/array.bt` both become `bt@stdlib@array` — the second silently
/// clobbering the first in `ebin/`. Flat layouts got uniqueness for free from
/// the filesystem; nested ones have to check.
///
/// Keyed on the *derived module name*, not the raw stem, because
/// [`module_name_from_path`] case-folds: `beamerror.bt` and `Beamerror.bt` are
/// distinct filenames that both produce `bt@stdlib@beamerror`. Comparing stems
/// would wave that collision straight through.
pub(super) fn check_duplicate_module_names(source_files: &[Utf8PathBuf]) -> Result<()> {
    let mut seen: std::collections::HashMap<String, &Utf8Path> = std::collections::HashMap::new();
    for file in source_files {
        let module = module_name_from_path(file)?;
        if let Some(first) = seen.insert(module.clone(), file.as_path()) {
            miette::bail!(
                "Stdlib sources '{first}' and '{file}' both compile to module '{module}'. \
                 Module names come from the file stem alone (case-folded), and subdirectories \
                 do not namespace them, so stdlib class file names must be unique across all \
                 subdirectories. Rename one of the two files."
            );
        }
    }
    Ok(())
}

/// Extract the module name from a `.bt` file path.
///
/// ADR 0016: All stdlib classes use `bt@stdlib@{snake_case}` prefix.
/// The `@` separator is legal in unquoted Erlang atoms and follows
/// the Gleam convention (`gleam@list`, `gleam@string`).
///
/// Only the file *stem* is used — a subdirectory under `stdlib/src/` never
/// becomes part of the module name. This deliberately diverges from user
/// packages, where `src/util/math.bt` becomes `util@math`
/// (`build::compute_relative_module`). Stdlib keeps the flat mapping because
/// it is a closed-form function of the class name: `compiled_module_name`,
/// `PrimitiveBindingTable::runtime_module_for_class` and
/// `beamtalk_primitive:module_for_value/1` all derive `bt@stdlib@{snake}`
/// with no path and no lookup table available. Folding directories into the
/// atom would couple that hot dispatch path to a purely cosmetic layout
/// choice.
pub(super) fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path
        .file_stem()
        .ok_or_else(|| miette::miette!("File '{}' has no name", path))?;

    // Validate module name
    if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
        miette::bail!(
            "Invalid module name '{}': must contain only alphanumeric characters and underscores",
            stem
        );
    }

    let snake = beamtalk_codegen::core_erlang::to_module_name(stem);

    // ADR 0016: All stdlib modules use bt@stdlib@ prefix
    Ok(format!("bt@stdlib@{snake}"))
}

/// Compile a single stdlib `.bt` file to Core Erlang.
pub(super) fn compile_stdlib_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    ctx: &CompileContext<'_>,
) -> Result<()> {
    compile_source_with_bindings(path, module_name, core_file, options, bindings, ctx, None)
        .map(|_diags| ())
}

/// The `CompilerOptions` every stdlib compile in [`build_stdlib`] runs with.
///
/// `current_package` is `Some("stdlib")`, matching the
/// `package: Some("stdlib")` stamp on [`stdlib_pre_loaded_aliases`]'s entries
/// (and [`generate_class_entry`]'s `ClassInfo`s) and the LSP's
/// `STDLIB_PACKAGE_MARKER` — without it, `None` would make
/// `AliasRegistry::add_pre_loaded` fall into the open-world REPL path instead
/// of enforcing stdlib's own package boundary, and an `internal type` alias
/// would resolve by accident rather than by the same-package rule (ADR 0108).
/// Tests compile against this exact function so the two identities cannot
/// silently drift apart.
pub(super) fn stdlib_compiler_options(warnings_as_errors: bool) -> beamtalk_core::CompilerOptions {
    beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        allow_primitives: false,
        warnings_as_errors,
        current_package: Some("stdlib".into()),
        // BT-4724: stdlib has no project root, but installed OTP and the
        // runtime still apply, so a typo'd `Erlang lsts` FFI module still fails.
        known_erlang_modules: beamtalk_core::ffi_type_specs::collect_installed_erlang_modules(),
        ..Default::default()
    }
}

/// Extracts the `pre_loaded_aliases` value for
/// [`ClassHierarchyContext`](crate::beam_compiler::ClassHierarchyContext)
/// from [`collect_stdlib_alias_sources`]'s output, stamping every entry's
/// `package` as `"stdlib"` (mirroring [`generate_class_entry`]'s hardcoded
/// `package: Some("stdlib".into())` for generated `ClassInfo`).
pub(super) fn stdlib_pre_loaded_aliases(
    alias_sources: &[AliasSource],
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    alias_sources
        .iter()
        .map(|a| {
            let mut info = a.info.clone();
            info.package = Some("stdlib".into());
            info
        })
        .collect()
}
