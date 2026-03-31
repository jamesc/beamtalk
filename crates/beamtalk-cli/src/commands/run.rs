// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Run beamtalk programs.
//!
//! **DDD Context:** Build System — Program Execution
//!
//! Two modes (ADR 0061):
//!
//! **Script mode** (`beamtalk run ClassName selector`): starts a run-mode workspace
//! (no REPL server, not registered in `~/.beamtalk/workspaces/`), calls
//! `ClassName>>selector` via the class registry, and exits when the method returns.
//!
//! **Service mode** (`beamtalk run .`): starts a persistent workspace (with REPL
//! server), starts the `[application]` supervisor, prints the REPL port, and
//! returns the shell. If the service is already running, reports the existing port
//! and exits 0 (idempotent).

use std::ffi::OsString;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use camino::Utf8PathBuf;
use miette::{Result, miette};
use tracing::{info, instrument};

use beamtalk_cli::repl_startup;

use super::build_layout::BuildLayout;
use super::manifest;
use super::workspace;

/// Compile and run a beamtalk package.
///
/// - `class_or_dot`: either `"."` (service mode) or a class name (script mode).
/// - `selector`: required when `class_or_dot` is a class name; the method to call.
#[instrument(skip_all, fields(class_or_dot = %class_or_dot))]
pub fn run(class_or_dot: &str, selector: Option<&str>) -> Result<()> {
    info!("Starting run command");

    // Determine project root (always current directory for run)
    let project_root = Utf8PathBuf::from(".");

    let Some(pkg) = manifest::find_manifest(&project_root)? else {
        return Err(miette!(
            "No beamtalk.toml found in the current directory.\n\
             The run command requires a package manifest.\n\
             Create one with: beamtalk new <project_name>"
        ));
    };

    match (class_or_dot, selector) {
        (".", None) => {
            // Service mode: requires [application] section
            let app_config = manifest::find_application_config(&project_root)?;
            match app_config {
                Some(app) => run_package_as_otp_application(&project_root, &pkg, &app),
                None => Err(miette!(
                    "No entry point defined for `beamtalk run .`\n\
                     \n\
                     For a supervised OTP service, add to beamtalk.toml:\n\
                     \n\
                       [application]\n\
                       supervisor = \"MyRootSup\"\n\
                     \n\
                     For a script, use: beamtalk run ClassName selector"
                )),
            }
        }
        (class_name, Some(sel)) => {
            // Script mode: beamtalk run ClassName selector
            run_script(&project_root, &pkg, class_name, sel)
        }
        (class_name, None) if class_name != "." => {
            // Got class name but no selector
            Err(miette!(
                "Missing selector for script mode.\n\
                 Usage: beamtalk run {class_name} <selector>\n\
                 Example: beamtalk run {class_name} run"
            ))
        }
        _ => unreachable!(),
    }
}

/// Auto-build the Beamtalk runtime in dev mode if its ebin directory is missing.
fn ensure_runtime_built(
    runtime_dir: &std::path::Path,
    layout: repl_startup::RuntimeLayout,
    paths: &repl_startup::BeamPaths,
) -> Result<()> {
    if layout == repl_startup::RuntimeLayout::Dev && !paths.runtime_ebin.exists() {
        info!("Building Beamtalk runtime...");
        let status = Command::new("rebar3")
            .arg("compile")
            .current_dir(runtime_dir)
            .status()
            .map_err(|e| miette!("Failed to build runtime: {e}"))?;

        if !status.success() {
            return Err(miette!("Failed to build Beamtalk runtime"));
        }
    }
    Ok(())
}

/// Run a package script: start a run-mode workspace and call `ClassName>>selector`.
///
/// Starts a fresh run-mode workspace (no REPL server, not registered in
/// `~/.beamtalk/workspaces/`), bootstraps all project classes, dispatches the
/// class message, and exits when it returns.
#[allow(clippy::too_many_lines)] // code path setup for native + deps makes this slightly over limit
fn run_script(
    project_root: &Utf8PathBuf,
    _pkg: &manifest::PackageManifest,
    class_name: &str,
    selector: &str,
) -> Result<()> {
    // Validate class name: Beamtalk class names are UpperCamelCase identifiers.
    // Full validation is required because the name is injected into an Erlang -eval string.
    if class_name.is_empty()
        || !class_name.chars().next().is_some_and(char::is_uppercase)
        || class_name.chars().any(|c| !c.is_alphanumeric() && c != '_')
    {
        miette::bail!(
            "Invalid class name '{class_name}': class names must start with an uppercase letter \
             and contain only alphanumeric characters and underscores"
        );
    }

    // Validate selector: unary selectors are simple identifiers (keyword selectors
    // with ':' are not supported in the CLI form — wrap them in a unary entry point).
    // Full validation required: selector is injected into an Erlang -eval string.
    // Erlang unquoted atoms must start with a lowercase letter; an uppercase first
    // character would be parsed as a variable reference, causing a confusing error.
    if selector.is_empty()
        || !selector
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_lowercase())
        || selector.chars().any(|c| !c.is_alphanumeric() && c != '_')
    {
        miette::bail!(
            "Invalid selector '{selector}': selectors must start with a lowercase letter \
             and contain only alphanumeric characters and underscores.\n\
             Keyword selectors (e.g. `start: 'prod'`) are not supported in the CLI form — \
             wrap them in a unary entry method."
        );
    }

    info!(class = %class_name, selector = %selector, "Running script");

    // Build the project
    println!("Building...");
    super::build::build(
        project_root.as_str(),
        &beamtalk_core::CompilerOptions::default(),
        false,
    )?;

    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    ensure_runtime_built(&runtime_dir, layout, &paths)?;

    let layout = BuildLayout::new(project_root);
    let ebin_dir = layout.ebin_dir();

    println!("\nRunning {class_name}>>{selector}...");

    // Build run-mode eval command:
    // 1. Start beamtalk_workspace application
    // 2. Start workspace supervisor in run mode (repl=false, no TCP server, not registered)
    // 3. Look up class in registry and dispatch the message
    // 4. Halt when done
    let project_path_escaped =
        crate::beam_compiler::escape_erlang_string(&project_root.canonicalize().map_or_else(
            |_| project_root.to_string(),
            |p| p.to_string_lossy().into_owned(),
        ));

    // Use PID-scoped workspace ID so that if the workspace metadata or bootstrap
    // produce any per-id artifacts, concurrent invocations don't collide.
    // (Run-mode workspaces are not distributed nodes, so EPMD is not involved.)
    let pid = std::process::id();
    let workspace_id = format!("run_{pid}");

    // ADR 0072: Collect all hex dep names from lockfile (includes transitive BT deps' native deps)
    let hex_dep_names = super::deps::lockfile::Lockfile::collect_hex_dep_names(project_root)?;

    let eval_cmd = build_script_eval_cmd(
        &workspace_id,
        &project_path_escaped,
        class_name,
        selector,
        &hex_dep_names,
    );

    let mut args = repl_startup::beam_pa_args(&paths);

    // Add package ebin to code path
    args.push(OsString::from("-pa"));
    #[cfg(windows)]
    {
        let ebin_path = ebin_dir.as_str().replace('\\', "/");
        args.push(OsString::from(ebin_path));
    }
    #[cfg(not(windows))]
    {
        args.push(OsString::from(ebin_dir.as_str()));
    }

    // ADR 0070: Add dependency ebin directories to BEAM code path
    for dep_ebin in super::deps::collect_dep_ebin_paths(project_root) {
        args.push(OsString::from("-pa"));
        #[cfg(windows)]
        {
            let dep_ebin_path = dep_ebin.as_str().replace('\\', "/");
            args.push(OsString::from(dep_ebin_path));
        }
        #[cfg(not(windows))]
        {
            args.push(OsString::from(dep_ebin.as_str()));
        }
    }

    // ADR 0072: Add native Erlang ebin to code path if present
    let native_ebin = layout.native_ebin_dir();
    if native_ebin.exists() {
        args.push(OsString::from("-pa"));
        #[cfg(windows)]
        {
            let native_ebin_path = native_ebin.as_str().replace('\\', "/");
            args.push(OsString::from(native_ebin_path));
        }
        #[cfg(not(windows))]
        {
            args.push(OsString::from(native_ebin.as_str()));
        }
    }

    // ADR 0072 Phase 2: Add rebar3 hex dep ebin paths to code path (Path B)
    let rebar_base_dir = layout.native_dir();
    for ebin in super::build::collect_rebar3_ebin_paths(&rebar_base_dir) {
        args.push(OsString::from("-pa"));
        #[cfg(windows)]
        {
            let ebin_path = ebin.as_str().replace('\\', "/");
            args.push(OsString::from(ebin_path));
        }
        #[cfg(not(windows))]
        {
            args.push(OsString::from(ebin.as_str()));
        }
    }

    args.push(OsString::from("-eval"));
    args.push(OsString::from(&eval_cmd));

    let mut cmd = Command::new("erl");
    cmd.arg("-noshell")
        .args(&args)
        .current_dir(project_root.as_std_path())
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    if let Ok(exe) = std::env::current_exe() {
        if let Some(bin_dir) = exe.parent() {
            let compiler_port = bin_dir.join("beamtalk-compiler-port");
            if compiler_port.exists() {
                cmd.env("BEAMTALK_COMPILER_PORT_BIN", &compiler_port);
            }
        }
    }

    let mut child = cmd
        .spawn()
        .map_err(|e| miette!("Failed to start BEAM node: {e}\nIs Erlang/OTP installed?"))?;

    let status = child
        .wait()
        .map_err(|e| miette!("Failed to wait for BEAM node: {e}"))?;

    if !status.success() {
        if let Some(code) = status.code() {
            miette::bail!("Program exited with code {code}");
        }
        // Signal-terminated (e.g. Ctrl+C) — exit silently
    }

    info!("Script run completed");
    Ok(())
}

/// Build the Erlang `-eval` string for script mode.
///
/// Separated from `run_script` so the generated code can be unit-tested without
/// needing a full Erlang runtime or compiled project.
///
/// ADR 0072: When the package has `[native.dependencies]`, the corresponding
/// OTP applications (gun, cowboy, etc.) are started before the workspace so
/// that FFI calls into hex deps work at runtime.
fn build_script_eval_cmd(
    workspace_id: &str,
    project_path_escaped: &str,
    class_name: &str,
    selector: &str,
    hex_dep_names: &[String],
) -> String {
    let hex_deps_start = repl_startup::hex_deps_start_fragment(hex_dep_names);

    format!(
        "{hex_deps_start}\
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(\
         #{{workspace_id => <<\"{workspace_id}\">>, \
         project_path => <<\"{project_path_escaped}\">>, \
         repl => false}}), \
         ClassPid = beamtalk_class_registry:whereis_class('{class_name}'), \
         case ClassPid of \
             undefined -> \
                 io:format(standard_error, \"Error: class '{class_name}' not found~n\", []), \
                 halt(1); \
             _ -> \
                 beamtalk_class_dispatch:class_send(ClassPid, {selector}, []), \
                 halt(0) \
         end."
    )
}

/// Run a package as a persistent OTP service (BT-1191, BT-1319).
///
/// Starts a persistent workspace (with REPL server) so that all project classes
/// are registered via the workspace bootstrap before the OTP application's root
/// supervisor `init/1` runs. The workspace node is detached — the CLI returns
/// once the service is up, printing the REPL port for later connection.
///
/// If a workspace for this project is already running, prints the existing port
/// and exits 0 (idempotent, matching `systemctl start` conventions).
fn run_package_as_otp_application(
    project_root: &Utf8PathBuf,
    pkg: &manifest::PackageManifest,
    app_config: &manifest::ApplicationConfig,
) -> Result<()> {
    info!(name = %pkg.name, "Running as OTP service (workspace-first)");

    // Early probe: if the workspace+app is already running, skip building entirely.
    if let Some(port) = probe_running_workspace_app(project_root.as_std_path(), &pkg.name) {
        println!(
            "{} v{} is already running (REPL port {})\nConnect with: beamtalk repl",
            pkg.name, pkg.version, port
        );
        return Ok(());
    }

    println!("Building...");
    super::build::build(
        project_root.as_str(),
        &beamtalk_core::CompilerOptions::default(),
        false,
    )?;

    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    ensure_runtime_built(&runtime_dir, layout, &paths)?;

    let layout = BuildLayout::new(project_root);
    let ebin_dir: PathBuf = layout.ebin_dir().into_std_path_buf();
    let mut extra_code_paths = vec![ebin_dir.clone()];

    // ADR 0070: Add dependency ebin directories to code path
    for dep_ebin in super::deps::collect_dep_ebin_paths(project_root) {
        extra_code_paths.push(dep_ebin.into_std_path_buf());
    }

    // ADR 0072: Add native Erlang ebin to code path if present (Path A)
    let native_ebin_dir: PathBuf = layout.native_ebin_dir().into_std_path_buf();
    if native_ebin_dir.exists() {
        extra_code_paths.push(native_ebin_dir);
    }

    // ADR 0072 Phase 2: Add rebar3 hex dep ebin paths to code path (Path B)
    let rebar_base_dir = layout.native_dir();
    for ebin in super::build::collect_rebar3_ebin_paths(&rebar_base_dir) {
        extra_code_paths.push(ebin.into_std_path_buf());
    }

    // ADR 0072: Collect all hex dep names from lockfile (includes transitive BT deps' native deps)
    let service_hex_dep_names =
        super::deps::lockfile::Lockfile::collect_hex_dep_names(project_root)?;

    let (node_info, is_new, workspace_id) = workspace::get_or_start_workspace(
        project_root.as_std_path(),
        None,
        0, // ephemeral port: OS assigns
        &paths,
        &extra_code_paths,
        false, // persistent (not auto_cleanup)
        None,  // max_idle_seconds: use workspace default
        None,  // bind_addr: loopback default
        None,  // web_port
        Some(&pkg.name),
        &service_hex_dep_names,
    )?;

    if !is_new {
        let already_running = ensure_otp_app_in_workspace(
            &node_info.node_name,
            &workspace_id,
            &pkg.name,
            &ebin_dir,
            &paths,
            project_root.as_std_path(),
        )?;

        if already_running {
            println!(
                "{} v{} is already running (REPL port {})\nConnect with: beamtalk repl",
                pkg.name, pkg.version, node_info.port
            );
        } else {
            println!(
                "\nStarted {} v{} in existing workspace\n  Supervisor : {}\n  REPL port  : {}   (connect with: beamtalk repl)",
                pkg.name, pkg.version, app_config.supervisor, node_info.port
            );
        }
        return Ok(());
    }

    println!(
        "\nStarted {} v{}\n  Supervisor : {}\n  REPL port  : {}   (connect with: beamtalk repl)",
        pkg.name, pkg.version, app_config.supervisor, node_info.port
    );

    info!("OTP service started");
    Ok(())
}

/// Choose `-sname` or `-name` based on the node name's host part.
///
/// Short hostnames (e.g. `localhost`) require `-sname`; fully qualified
/// hostnames (containing `.`) require `-name`. Using `-name` with `localhost`
/// causes Erlang to reject it as "illegal" (BT-1418).
fn node_name_flag(node_name: &str) -> &'static str {
    if let Some((_local, host)) = node_name.split_once('@') {
        if host.contains('.') {
            "-name"
        } else {
            "-sname"
        }
    } else {
        "-sname"
    }
}

/// Probe whether the named OTP application is already running in the project's workspace.
///
/// Returns `Some(port)` if the application is running, `None` otherwise.
/// Failures are swallowed — callers fall through to the normal startup path.
fn probe_running_workspace_app(project_root: &std::path::Path, app_name: &str) -> Option<u16> {
    let workspace_id = workspace::workspace_id_for_project(project_root, None).ok()?;
    let node_info = workspace::get_node_info(&workspace_id).ok()??;
    if !workspace::is_node_running(&node_info, Some(&workspace_id)) {
        return None;
    }

    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().ok()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let cookie = workspace::read_workspace_cookie(&workspace_id).ok()?;
    let cookie = cookie.trim().to_string();
    let pid = std::process::id();
    let client_node = format!("beamtalk_run_probe_{pid}@localhost");

    let eval_cmd = format!(
        "Apps = rpc:call('{node}', application, which_applications, [], 30000), \
         Names = case Apps of \
             {{badrpc, _}} -> []; \
             L when is_list(L) -> [element(1, A) || A <- L] \
         end, \
         case lists:member('{app_name}', Names) of \
             true -> io:format(\"running~n\"), halt(0); \
             false -> io:format(\"not_running~n\"), halt(0) \
         end.",
        node = node_info.node_name,
    );

    let mut args = repl_startup::beam_pa_args(&paths);
    args.push(OsString::from(node_name_flag(&client_node)));
    args.push(OsString::from(&client_node));
    args.push(OsString::from("-setcookie"));
    args.push(OsString::from(&cookie));
    args.push(OsString::from("-eval"));
    args.push(OsString::from(&eval_cmd));

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-hidden")
        .args(&args)
        .current_dir(project_root)
        .output()
        .ok()?;

    if output.status.success()
        && String::from_utf8_lossy(&output.stdout)
            .trim()
            .starts_with("running")
    {
        Some(node_info.port)
    } else {
        None
    }
}

/// Ensure the named OTP application is running inside an existing workspace node.
///
/// Returns `true` if the app was already running, `false` if it was just started.
fn ensure_otp_app_in_workspace(
    workspace_node: &str,
    workspace_id: &str,
    app_name: &str,
    ebin_dir: &std::path::Path,
    beam_paths: &repl_startup::BeamPaths,
    project_root: &std::path::Path,
) -> Result<bool> {
    let cookie = workspace::read_workspace_cookie(workspace_id)?;
    let cookie = cookie.trim();

    let pid = std::process::id();
    let client_node = format!("beamtalk_run_rpc_{pid}@localhost");

    #[cfg(windows)]
    let ebin_escaped =
        crate::beam_compiler::escape_erlang_string(&ebin_dir.to_string_lossy().replace('\\', "/"));
    #[cfg(not(windows))]
    let ebin_escaped = crate::beam_compiler::escape_erlang_string(&ebin_dir.to_string_lossy());

    #[cfg(windows)]
    let project_path_escaped = crate::beam_compiler::escape_erlang_string(
        &project_root.to_string_lossy().replace('\\', "/"),
    );
    #[cfg(not(windows))]
    let project_path_escaped =
        crate::beam_compiler::escape_erlang_string(&project_root.to_string_lossy());

    let eval_cmd = format!(
        "rpc:call('{workspace_node}', code, add_path, [\"{ebin_escaped}\"]), \
         rpc:call('{workspace_node}', beamtalk_workspace_bootstrap, activate_project_modules, [<<\"{project_path_escaped}\">>], 30000), \
         Result = rpc:call('{workspace_node}', application, ensure_all_started, ['{app_name}'], 30000), \
         case Result of \
             {{ok, []}} -> io:format(\"already~n\"), halt(0); \
             {{ok, _}} -> io:format(\"started~n\"), halt(0); \
             _ -> io:format(\"error: ~p~n\", [Result]), halt(1) \
         end."
    );

    let mut args = repl_startup::beam_pa_args(beam_paths);
    args.push(OsString::from(node_name_flag(&client_node)));
    args.push(OsString::from(&client_node));
    args.push(OsString::from("-setcookie"));
    args.push(OsString::from(cookie));
    args.push(OsString::from("-eval"));
    args.push(OsString::from(&eval_cmd));

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-hidden")
        .args(&args)
        .current_dir(project_root)
        .output()
        .map_err(|e| miette!("Failed to start RPC probe: {e}\nIs Erlang/OTP installed?"))?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    if output.status.success() {
        Ok(stdout.trim().starts_with("already"))
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to start OTP application '{app_name}' in existing workspace.\n\
             The workspace may be in an incompatible state.\n\
             Try stopping the workspace first with: beamtalk workspace stop\n\
             stdout: {}\n\
             stderr: {stderr}",
            stdout.trim()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project_with_manifest(temp: &TempDir, manifest: &str) -> String {
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(
            src_path.join("main.bt"),
            "main := [\"Hello, World!\" length].",
        )
        .unwrap();
        fs::write(temp.path().join("beamtalk.toml"), manifest).unwrap();
        temp.path().to_string_lossy().to_string()
    }

    // Helper: temporarily set CWD for tests that need a manifest in "."
    fn with_project_dir<F: FnOnce() -> R, R>(dir: &std::path::Path, f: F) -> R {
        let orig = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir).unwrap();
        let result = f();
        std::env::set_current_dir(&orig).unwrap();
        result
    }

    #[test]
    #[serial(cwd)]
    fn test_run_no_manifest() {
        let temp = TempDir::new().unwrap();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(src_path.join("main.bt"), "main := [42].").unwrap();

        let result = with_project_dir(temp.path(), || run(".", None));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("No beamtalk.toml"),
            "Should mention missing manifest: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_dot_no_application() {
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = with_project_dir(temp.path(), || run(".", None));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("No entry point defined"),
            "Should mention missing entry point: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_class_missing_selector() {
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = with_project_dir(temp.path(), || run("Main", None));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Missing selector"),
            "Should mention missing selector: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_invalid_class_name_lowercase() {
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = with_project_dir(temp.path(), || run("main", Some("run")));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid class name"),
            "Should reject lowercase class name: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_path_like_string_requires_selector() {
        // Path-like strings are treated as class names (not paths); a missing selector
        // triggers a clear "Missing selector" error rather than a path-not-found error.
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );
        let result = with_project_dir(temp.path(), || run("/looks/like/path", None));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Missing selector"),
            "Path-like class name should require selector: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_invalid_selector_uppercase() {
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = with_project_dir(temp.path(), || run("Main", Some("Run")));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid selector"),
            "Uppercase selector should be rejected: {err}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_run_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(src_path.join("main.bt"), "main := [1 + ].").unwrap();
        fs::write(
            temp.path().join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let result = with_project_dir(temp.path(), || run("Main", Some("run")));
        assert!(result.is_err());
    }

    #[test]
    #[serial(cwd)]
    fn test_run_old_package_start_field_ignored() {
        // Old [package] start = "module" manifests should parse without error
        // (TOML ignores unknown fields); `run "."` then fails with no-entry-point error.
        let temp = TempDir::new().unwrap();
        create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\nstart = \"main\"\n",
        );

        let result = with_project_dir(temp.path(), || run(".", None));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("No entry point defined"),
            "Old [package] start should not be used: {err}"
        );
    }

    #[test]
    fn test_script_eval_cmd_no_erlang_comments() {
        // Regression: Erlang %% comments in a -eval string comment out
        // everything to end-of-line, which is the entire eval string.
        let cmd = build_script_eval_cmd("run_42", "/tmp/foo", "SmokeTest", "answer", &[]);
        assert!(
            !cmd.contains("%%"),
            "Eval string must not contain Erlang line comments: {cmd}"
        );
        assert!(
            !cmd.contains(" % "),
            "Eval string must not contain Erlang line comments: {cmd}"
        );
    }

    #[test]
    fn test_script_eval_cmd_contains_dispatch_and_halt() {
        let cmd = build_script_eval_cmd("run_1", "/proj", "MyClass", "run", &[]);
        assert!(
            cmd.contains("class_send(ClassPid, run, [])"),
            "Eval should dispatch the selector: {cmd}"
        );
        assert!(
            cmd.contains("halt(0)"),
            "Eval should halt on success: {cmd}"
        );
        assert!(
            cmd.contains("halt(1)"),
            "Eval should halt(1) on class-not-found: {cmd}"
        );
        assert!(cmd.ends_with("end."), "Eval should end with 'end.': {cmd}");
    }

    #[test]
    fn test_node_name_flag_short_hostname() {
        assert_eq!(node_name_flag("beamtalk_run_probe_1@localhost"), "-sname");
        assert_eq!(node_name_flag("beamtalk_workspace_abc@myhost"), "-sname");
    }

    #[test]
    fn test_node_name_flag_fqdn() {
        assert_eq!(node_name_flag("node@host.example.com"), "-name");
    }

    #[test]
    fn test_node_name_flag_no_host() {
        assert_eq!(node_name_flag("beamtalk_workspace_abc"), "-sname");
    }

    #[test]
    fn test_script_eval_cmd_interpolates_parameters() {
        let cmd = build_script_eval_cmd("run_99", "/my/project", "Counter", "increment", &[]);
        assert!(cmd.contains("run_99"), "workspace_id not interpolated");
        assert!(cmd.contains("/my/project"), "project_path not interpolated");
        assert!(
            cmd.contains("'Counter'"),
            "class_name not interpolated: {cmd}"
        );
        assert!(
            cmd.contains("increment"),
            "selector not interpolated: {cmd}"
        );
    }

    #[test]
    fn test_script_eval_cmd_starts_hex_deps() {
        let hex_deps = vec!["gun".to_string(), "cowboy".to_string()];
        let cmd = build_script_eval_cmd("run_1", "/proj", "MyClass", "run", &hex_deps);
        assert!(
            cmd.contains("application:ensure_all_started(cowboy)"),
            "Should start cowboy: {cmd}"
        );
        assert!(
            cmd.contains("application:ensure_all_started(gun)"),
            "Should start gun: {cmd}"
        );
        // Hex deps should be started before the workspace
        let cowboy_pos = cmd.find("ensure_all_started(cowboy)").unwrap();
        let workspace_pos = cmd.find("ensure_all_started(beamtalk_workspace)").unwrap();
        assert!(
            cowboy_pos < workspace_pos,
            "Hex deps should start before workspace: {cmd}"
        );
    }

    #[test]
    fn test_script_eval_cmd_no_hex_deps_unchanged() {
        let cmd = build_script_eval_cmd("run_1", "/proj", "MyClass", "run", &[]);
        // Should only have one ensure_all_started call (for workspace)
        let count = cmd.matches("ensure_all_started").count();
        assert_eq!(
            count, 1,
            "Should only start workspace when no hex deps: {cmd}"
        );
    }
}
