// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Single-file escript packaging (ADR 0099 §4).
//!
//! **DDD Context:** Build System — Packaging
//!
//! `beamtalk build --escript --entry "ClassName selector" -o <name>` produces a
//! self-contained executable escript: a shebang header + an embedded zip archive
//! containing the project's `bt@*.beam`, the `beamtalk_runtime` /
//! `beamtalk_stdlib` / `beamtalk_workspace` (+ deps) application beams, and a
//! generated `main/1` bootstrap module.
//!
//! The bootstrap reuses ADR 0061's run-mode lifecycle: it starts the workspace
//! (`repl = false`), registers the project classes (topo-ordered), seeds the
//! `node_owning` / `program_name` app envs (ADR 0099 §§2-3), dispatches the
//! entry method, and maps the outcome to a POSIX exit status via the shared run
//! harness.

use std::path::Path;
use std::process::Command;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result, WrapErr, miette};
use tracing::{info, instrument};

use beamtalk_cli::repl_startup;

use beamtalk_cli::erlc::ErlcInvocation;

use super::build_layout::BuildLayout;
use super::manifest;

/// Build a single-file escript for the project at `project_root`.
///
/// - `entry`: `"ClassName selector"` (selector unary or a single arity-1 keyword).
/// - `output`: the output filename; if `None`, derived from the manifest package
///   name (lowercased).
#[instrument(skip_all)]
pub fn build_escript(
    project_root: &Utf8Path,
    entry: Option<&str>,
    output: Option<&str>,
    options: &beamtalk_core::CompilerOptions,
    force: bool,
) -> Result<()> {
    // Compile the project first (produces the project `bt@*.beam`).
    println!("Building...");
    super::build::build(project_root.as_str(), options, force)?;

    let pkg = manifest::find_manifest(project_root)?;

    let entry = entry.ok_or_else(|| {
        miette!(
            "`--escript` requires `--entry \"ClassName selector\"`.\n\
             Example: beamtalk build --escript --entry \"Greeter main:\" -o greeter"
        )
    })?;
    let (class_name, selector, is_keyword) = parse_entry(entry)?;

    // Output name: explicit, else the package name (lowercased).
    let output_name = match output {
        Some(o) => o.to_string(),
        None => pkg.as_ref().map(|p| p.name.to_lowercase()).ok_or_else(|| {
            miette!(
                "No `-o <name>` given and no package manifest to derive one from.\n\
                     Pass an explicit output name: beamtalk build --escript --entry \
                     \"{class_name} {selector}\" -o <name>"
            )
        })?,
    };

    // The escript boot module is named after the output basename, sanitised to a
    // valid Erlang atom; the bootstrap's `main/1` is its entry point.
    let boot_module = sanitize_module_name(&output_name);

    // Locate the runtime/stdlib beams.
    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let lib_dir = paths
        .runtime_ebin
        .parent()
        .and_then(Path::parent)
        .ok_or_else(|| miette!("Could not locate the runtime library directory"))?
        .to_path_buf();

    let build_layout = BuildLayout::new(project_root);
    let project_ebin = build_layout.ebin_dir();

    // Phase 4 bundles the runtime/stdlib (+ their deps), but not the *project's*
    // own hex/native dependencies (`[native.dependencies]`, under `_build/deps`).
    // A program that calls into one would package fine then fail at runtime with
    // `{undef, …}`, so warn loudly rather than ship a silently-broken artifact.
    if let Ok(beam_env) =
        super::beam_environment::BeamEnvironment::from_layout(&build_layout, project_root)
    {
        if !beam_env.otp_apps.is_empty() {
            eprintln!(
                "warning: this project declares hex/native dependencies ({}) which are \
                 NOT yet bundled into the escript. If the program calls into them it will \
                 fail at runtime with `{{undef, …}}`. (escript hex-dep bundling is a follow-up.)",
                beam_env.otp_apps.join(", ")
            );
        }
    }

    // Collect the project's compiled class modules (`bt@*.beam`).
    let project_modules = collect_project_modules(&project_ebin)?;
    if project_modules.is_empty() {
        miette::bail!(
            "No compiled project classes found in '{project_ebin}'. Did the build produce any?"
        );
    }

    // Generate + compile the bootstrap module into a temp dir under _build
    // (sibling of the `ebin/` output dir).
    let tmp_dir = project_ebin
        .parent()
        .map_or_else(|| project_ebin.join("escript"), |p| p.join("escript"));
    std::fs::create_dir_all(tmp_dir.as_std_path())
        .into_diagnostic()
        .wrap_err("Failed to create escript temp directory")?;

    let boot_src = generate_boot_module(
        &boot_module,
        &class_name,
        &selector,
        is_keyword,
        &project_modules,
    );
    let boot_erl = tmp_dir.join(format!("{boot_module}.erl"));
    std::fs::write(boot_erl.as_std_path(), boot_src)
        .into_diagnostic()
        .wrap_err("Failed to write escript bootstrap source")?;

    ErlcInvocation::new(tmp_dir.clone())
        .source_file(boot_erl.clone())
        .run("escript bootstrap module")?;
    let boot_beam = tmp_dir.join(format!("{boot_module}.beam"));

    // Assemble the escript archive (the file-collection + escript:create logic
    // lives in Erlang, where escript:create does).
    let output_path = Utf8PathBuf::from(&output_name);
    assemble_escript(
        &output_path,
        &boot_beam,
        &boot_module,
        &project_ebin,
        &lib_dir,
        &paths.stdlib_ebin,
        &tmp_dir,
    )?;

    set_executable(output_path.as_std_path())?;

    println!(
        "Created escript '{output_path}' (entry: {class_name} {selector})\n  Run it: ./{output_name} [args...]"
    );
    info!(output = %output_path, "escript created");
    Ok(())
}

/// Parse and validate `"ClassName selector"`. Returns `(class, selector, is_keyword)`.
fn parse_entry(entry: &str) -> Result<(String, String, bool)> {
    let mut parts = entry.split_whitespace();
    let class_name = parts.next().ok_or_else(|| {
        miette!("`--entry` is empty; expected \"ClassName selector\" (e.g. \"Greeter main:\")")
    })?;
    let selector = parts.next().ok_or_else(|| {
        miette!(
            "`--entry \"{entry}\"` is missing a selector; expected \"ClassName selector\" \
             (e.g. \"Greeter main:\")"
        )
    })?;
    if parts.next().is_some() {
        miette::bail!(
            "`--entry \"{entry}\"` has too many tokens; expected exactly \"ClassName selector\"."
        );
    }
    // Reuse the run-mode validator (unary or single arity-1 keyword; ADR 0099).
    let is_keyword = super::run::validate_class_and_selector(class_name, selector)?;
    Ok((class_name.to_string(), selector.to_string(), is_keyword))
}

/// Sanitise an output name into a valid lowercase Erlang atom for the boot module.
fn sanitize_module_name(name: &str) -> String {
    let basename = Utf8Path::new(name)
        .file_stem()
        .map_or(name, |s| s)
        .to_string();
    let mut out = String::new();
    for ch in basename.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    // Erlang atoms must start with a lowercase letter.
    if !out.chars().next().is_some_and(|c| c.is_ascii_lowercase()) {
        out.insert_str(0, "bt_");
    }
    format!("{out}_escript")
}

/// Collect the project's `bt@*.beam` module names from the project ebin.
fn collect_project_modules(project_ebin: &Utf8Path) -> Result<Vec<String>> {
    let mut modules = Vec::new();
    let read = std::fs::read_dir(project_ebin.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read project ebin '{project_ebin}'"))?;
    for entry in read {
        let entry = entry.into_diagnostic()?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if let Some(module) = name.strip_suffix(".beam") {
            if module.starts_with("bt@") {
                modules.push(module.to_string());
            }
        }
    }
    modules.sort();
    Ok(modules)
}

/// Generate the escript bootstrap module source.
fn generate_boot_module(
    boot_module: &str,
    class_name: &str,
    selector: &str,
    is_keyword: bool,
    project_modules: &[String],
) -> String {
    let module_list = project_modules
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // A keyword entry receives argv as one List(String); a unary entry takes none.
    let (arg_binding, dispatch_args) = if is_keyword {
        (
            "    BinArgs = [unicode:characters_to_binary(A) || A <- Args],\n",
            "[BinArgs]",
        )
    } else {
        ("    _ = Args,\n", "[]")
    };

    format!(
        "%% Generated by `beamtalk build --escript` — do not edit.\n\
         -module({boot_module}).\n\
         -export([main/1]).\n\
         \n\
         main(Args) ->\n\
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace),\n\
         \x20   application:set_env(beamtalk_runtime, node_owning, true),\n\
         \x20   application:set_env(beamtalk_runtime, program_name,\n\
         \x20       unicode:characters_to_binary(filename:basename(escript:script_name()))),\n\
         \x20   {{ok, _}} = beamtalk_workspace_sup:start_link(\n\
         \x20       #{{workspace_id => <<\"escript\">>, project_path => undefined,\n\
         \x20         repl => false, start_compiler => false}}),\n\
         \x20   {{ok, ActivationErrors}} =\n\
         \x20       beamtalk_module_activation:activate_modules([{module_list}], #{{}}),\n\
         \x20   case ActivationErrors of\n\
         \x20       [] -> ok;\n\
         \x20       _ ->\n\
         \x20           io:format(standard_error,\n\
         \x20               \"warning: ~b project class(es) failed to activate: ~p~n\",\n\
         \x20               [length(ActivationErrors), ActivationErrors])\n\
         \x20   end,\n\
         {arg_binding}\
         \x20   case beamtalk_class_registry:whereis_class('{class_name}') of\n\
         \x20       undefined ->\n\
         \x20           io:format(standard_error, \"Error: class '{class_name}' not found~n\", []),\n\
         \x20           halt(1);\n\
         \x20       ClassPid ->\n\
         \x20           beamtalk_script_harness:dispatch(ClassPid, '{selector}', {dispatch_args})\n\
         \x20   end.\n"
    )
}

/// Assemble the escript via an embedded Erlang `escript` builder script.
fn assemble_escript(
    output: &Utf8Path,
    boot_beam: &Utf8Path,
    boot_module: &str,
    project_ebin: &Utf8Path,
    lib_dir: &Path,
    stdlib_ebin: &Path,
    tmp_dir: &Utf8Path,
) -> Result<()> {
    let builder = tmp_dir.join("bt_escript_builder.escript");
    std::fs::write(builder.as_std_path(), ESCRIPT_BUILDER)
        .into_diagnostic()
        .wrap_err("Failed to write escript builder")?;

    let output_abs = std::fs::canonicalize(".")
        .into_diagnostic()?
        .join(output.as_std_path());

    let status = Command::new("escript")
        .arg(builder.as_str())
        .arg(output_abs.to_string_lossy().as_ref())
        .arg(boot_beam.as_str())
        .arg(boot_module)
        .arg(project_ebin.as_str())
        .arg(lib_dir.to_string_lossy().as_ref())
        .arg(stdlib_ebin.to_string_lossy().as_ref())
        .status()
        .into_diagnostic()
        .wrap_err("Failed to run the escript builder (is Erlang/OTP installed?)")?;

    if !status.success() {
        miette::bail!("escript assembly failed");
    }
    Ok(())
}

/// Set the Unix executable bit; on Windows, write a `.cmd` launcher (ADR 0027).
#[cfg(unix)]
fn set_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path).into_diagnostic()?.permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms)
        .into_diagnostic()
        .wrap_err("Failed to set the escript executable bit")?;
    Ok(())
}

/// On Windows, escripts are not directly executable; write a `.cmd` launcher
/// next to the archive that invokes `escript <name>` (ADR 0027).
#[cfg(not(unix))]
fn set_executable(path: &Path) -> Result<()> {
    let cmd_path = format!("{}.cmd", path.display());
    let launcher = format!("@echo off\r\nescript \"%~dp0{}\" %*\r\n", path.display());
    std::fs::write(&cmd_path, launcher)
        .into_diagnostic()
        .wrap_err("Failed to write the Windows escript launcher")?;
    Ok(())
}

/// The embedded Erlang escript builder. Collects the boot module, the project
/// class beams, every bundled application's beams, and the stdlib class beams,
/// de-duplicates by archive path, and emits a shebang escript with the archive.
const ESCRIPT_BUILDER: &str = r#"%% Generated escript builder — collects beams and emits the archive.
main([OutFile, BootBeam, BootMod, ProjEbin, LibDir, StdlibEbin]) ->
    {ok, BootBin} = file:read_file(BootBeam),
    ProjEntries = [{"btproject/ebin/" ++ filename:basename(F), readf(F)}
                   || F <- filelib:wildcard(filename:join(ProjEbin, "bt@*.beam"))],
    AppEntries = lists:flatten(
        [collect_app(D) || D <- filelib:wildcard(filename:join(LibDir, "*")),
                           filelib:is_dir(D)]),
    %% Overlay the stdlib class beams + classes-env .app (Dev keeps them in
    %% apps/ebin, separate from the rebar-built erlang modules in _build).
    StdlibEntries = [{"beamtalk_stdlib/ebin/" ++ filename:basename(F), readf(F)}
                     || F <- filelib:wildcard(filename:join(StdlibEbin, "*")),
                        filelib:is_regular(F)],
    Boot = [{BootMod ++ ".beam", BootBin}],
    %% De-dup by archive path (later entries win): the stdlib overlay and any
    %% repeated .app must appear once in the zip.
    Files = maps:to_list(maps:from_list(Boot ++ ProjEntries ++ AppEntries ++ StdlibEntries)),
    EmuArgs = "-escript main " ++ BootMod,
    case escript:create(OutFile, [shebang, {emu_args, EmuArgs}, {archive, Files, []}]) of
        ok -> halt(0);
        {error, Reason} ->
            io:format(standard_error, "escript:create failed: ~p~n", [Reason]),
            halt(1)
    end;
main(_) ->
    io:format(standard_error, "usage: builder OutFile BootBeam BootMod ProjEbin LibDir StdlibEbin~n", []),
    halt(2).

collect_app(Dir) ->
    App = filename:basename(Dir),
    [{App ++ "/ebin/" ++ filename:basename(F), readf(F)}
     || F <- filelib:wildcard(filename:join([Dir, "ebin", "*"])),
        filelib:is_regular(F)].

readf(F) ->
    case file:read_file(F) of
        {ok, Bin} -> Bin;
        {error, Reason} ->
            io:format(standard_error, "error: cannot read ~s: ~p~n", [F, Reason]),
            halt(1)
    end.
"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_entry_keyword() {
        let (class, sel, kw) = parse_entry("Greeter main:").unwrap();
        assert_eq!(class, "Greeter");
        assert_eq!(sel, "main:");
        assert!(kw);
    }

    #[test]
    fn test_parse_entry_unary() {
        let (class, sel, kw) = parse_entry("Main run").unwrap();
        assert_eq!(class, "Main");
        assert_eq!(sel, "run");
        assert!(!kw);
    }

    #[test]
    fn test_parse_entry_rejects_missing_selector() {
        assert!(parse_entry("Greeter").is_err());
    }

    #[test]
    fn test_parse_entry_rejects_extra_tokens() {
        // The CLI does not reopen multi-keyword dispatch; `--entry` is exactly two tokens.
        assert!(parse_entry("Robot move: 3").is_err());
    }

    #[test]
    fn test_parse_entry_rejects_multi_keyword_selector() {
        assert!(parse_entry("Robot move:to:").is_err());
    }

    #[test]
    fn test_sanitize_module_name() {
        assert_eq!(sanitize_module_name("greeter"), "greeter_escript");
        // Strips a directory + extension and lowercases.
        assert_eq!(sanitize_module_name("dist/My-App.exe"), "my_app_escript");
        // A leading non-letter is prefixed so the result is a valid atom.
        assert_eq!(sanitize_module_name("9lives"), "bt_9lives_escript");
    }

    #[test]
    fn test_generate_boot_module_keyword_threads_argv_as_list() {
        let src = generate_boot_module(
            "greeter_escript",
            "Greeter",
            "main:",
            true,
            &["bt@app@greeter".to_string()],
        );
        assert!(src.contains("-module(greeter_escript)."));
        assert!(src.contains("application:set_env(beamtalk_runtime, node_owning, true)"));
        assert!(src.contains("activate_modules(['bt@app@greeter'], #{})"));
        assert!(src.contains("BinArgs = [unicode:characters_to_binary(A) || A <- Args]"));
        assert!(src.contains("dispatch(ClassPid, 'main:', [BinArgs])"));
    }

    #[test]
    fn test_generate_boot_module_unary_passes_no_args() {
        let src =
            generate_boot_module("app_escript", "Main", "run", false, &["bt@app@main".into()]);
        assert!(src.contains("dispatch(ClassPid, 'run', [])"));
        assert!(!src.contains("BinArgs"));
    }
}
