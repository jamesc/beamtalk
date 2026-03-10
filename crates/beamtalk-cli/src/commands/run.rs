// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Run beamtalk programs.
//!
//! **DDD Context:** Build System — Program Execution
//!
//! Compiles a Beamtalk package and starts a BEAM node to execute the
//! start module's `start` method. The node keeps running after the
//! method returns so that supervised actors remain alive.

use std::ffi::OsString;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use camino::Utf8PathBuf;
use miette::{Result, miette};
use tracing::{info, instrument};

use beamtalk_cli::repl_startup;

use super::manifest;
use super::workspace;

/// Compile and run a beamtalk package.
///
/// In package mode (beamtalk.toml with `start` field): compiles the package,
/// starts a BEAM node with the runtime, stdlib, and package code paths, then
/// calls the start module's `start` method. The BEAM node stays alive so
/// actors remain supervised.
///
/// Errors if no manifest is found or if the manifest lacks a `start` field.
#[instrument(skip_all, fields(path = %path))]
pub fn run(path: &str) -> Result<()> {
    info!("Starting run command");
    let input_path = Utf8PathBuf::from(path);

    // Derive project root: directory uses itself, file uses parent
    let project_root = if input_path.is_dir() {
        input_path
    } else {
        input_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), camino::Utf8Path::to_path_buf)
    };

    // Look for package manifest
    match manifest::find_manifest(&project_root)? {
        Some(pkg) => run_package(&project_root, &pkg),
        None => {
            // No manifest — error with helpful message
            Err(miette!(
                "No beamtalk.toml found in '{}'.\n\
                 The run command requires a package manifest.\n\
                 Create one with: beamtalk new <project_name>",
                project_root
            ))
        }
    }
}

/// Run a package.
///
/// If `[application] supervisor` is set, starts the package as a proper OTP
/// application via `application:ensure_all_started/1`. Otherwise, falls back
/// to calling the `start` module's `start` method imperatively.
fn run_package(project_root: &Utf8PathBuf, pkg: &manifest::PackageManifest) -> Result<()> {
    // Check for OTP application mode (BT-1191)
    let app_config = manifest::find_application_config(project_root)?;
    if let Some(app_config) = app_config {
        return run_package_as_otp_application(project_root, pkg, &app_config);
    }

    let start_module = pkg.start.as_deref().ok_or_else(|| {
        miette!(
            "No start module defined — add start = \"module_name\" to [package] in beamtalk.toml\n\
             Or use [application] supervisor = \"MyRootSup\" for a supervised OTP application"
        )
    })?;

    // Validate start module name (same rules as file stems in build.rs)
    if start_module.is_empty()
        || !start_module
            .chars()
            .all(|c| c == '_' || c.is_ascii_alphanumeric())
    {
        miette::bail!(
            "Invalid start module '{start_module}': must be non-empty and contain only alphanumeric characters and underscores"
        );
    }

    info!(start = %start_module, "Found start module in manifest");

    // Build the project
    println!("Building...");
    super::build::build(
        project_root.as_str(),
        &beamtalk_core::CompilerOptions::default(),
    )?;

    // Resolve the Erlang module name: bt@{package}@{start_module}
    let erlang_module = format!(
        "bt@{}@{}",
        pkg.name,
        beamtalk_core::codegen::core_erlang::to_module_name(start_module),
    );

    info!(erlang_module = %erlang_module, "Starting BEAM node");
    println!(
        "\nRunning {} v{} (start module: {start_module})...",
        pkg.name, pkg.version
    );

    // Start BEAM node with runtime + stdlib + package code paths
    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // Auto-build runtime if needed (dev mode only)
    if layout == repl_startup::RuntimeLayout::Dev && !paths.runtime_ebin.exists() {
        info!("Building Beamtalk runtime...");
        let status = Command::new("rebar3")
            .arg("compile")
            .current_dir(&runtime_dir)
            .status()
            .map_err(|e| miette!("Failed to build runtime: {e}"))?;

        if !status.success() {
            return Err(miette!("Failed to build Beamtalk runtime"));
        }
    }

    let mut args = repl_startup::beam_pa_args(&paths);

    // Add package ebin to code path
    let ebin_dir = project_root.join("_build").join("dev").join("ebin");
    args.push(OsString::from("-pa"));
    #[cfg(windows)]
    {
        // Convert Windows backslashes to forward slashes for Erlang (BT-661)
        let ebin_path = ebin_dir.as_str().replace('\\', "/");
        args.push(OsString::from(ebin_path));
    }
    #[cfg(not(windows))]
    {
        args.push(OsString::from(ebin_dir.as_str()));
    }

    // Build eval command:
    // 1. Start beamtalk_runtime application (for object system, actors, etc.)
    // 2. Call the start module's start method
    // 3. Keep the node alive (actors are supervised)
    let eval_cmd = format!(
        "{{ok, _}} = application:ensure_all_started(beamtalk_runtime), \
         '{erlang_module}':'start'(), \
         receive stop -> ok end."
    );

    args.push(OsString::from("-eval"));
    args.push(OsString::from(&eval_cmd));

    // Set compiler port binary path (for runtime compilation support)
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

    // Wait for the BEAM node to exit (Ctrl+C will propagate to child)
    let status = child
        .wait()
        .map_err(|e| miette!("Failed to wait for BEAM node: {e}"))?;

    if !status.success() {
        if let Some(code) = status.code() {
            miette::bail!("Program exited with code {code}");
        }
        // Signal-terminated (e.g. Ctrl+C) — exit silently
    }

    info!("Run command completed");
    Ok(())
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

    println!("Building...");
    super::build::build(
        project_root.as_str(),
        &beamtalk_core::CompilerOptions::default(),
    )?;

    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    if layout == repl_startup::RuntimeLayout::Dev && !paths.runtime_ebin.exists() {
        info!("Building Beamtalk runtime...");
        let status = Command::new("rebar3")
            .arg("compile")
            .current_dir(&runtime_dir)
            .status()
            .map_err(|e| miette!("Failed to build runtime: {e}"))?;

        if !status.success() {
            return Err(miette!("Failed to build Beamtalk runtime"));
        }
    }

    // Include the package ebin on the workspace code path so bootstrap can load
    // all bt@*.beam project classes before the OTP supervisor starts.
    let ebin_dir: PathBuf = project_root
        .join("_build")
        .join("dev")
        .join("ebin")
        .into_std_path_buf();
    let extra_code_paths = vec![ebin_dir.clone()];

    // Start or reconnect to a persistent workspace for this project.
    // otp_app_name causes application:ensure_all_started(app_name) to be
    // injected into the workspace eval after class bootstrap completes (BT-1319).
    let (node_info, is_new, workspace_id) = workspace::get_or_start_workspace(
        project_root.as_std_path(),
        None,
        0, // ephemeral port: OS assigns
        &paths,
        &extra_code_paths,
        false, // persistent (not auto_cleanup)
        None,  // max_idle_seconds: use workspace default
        None,  // bind_addr: loopback default
        None,  // ssl_dist_optfile
        None,  // web_port
        Some(&pkg.name),
    )?;

    if !is_new {
        // Workspace already exists — it may have been started by `beamtalk repl`
        // without the OTP application. Ensure the app is running via RPC so we
        // never falsely print "already running" when only the workspace is live.
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

/// Ensure the named OTP application is running inside an existing workspace node.
///
/// Uses a short-lived hidden BEAM node to RPC `application:ensure_all_started/1`
/// into the workspace. Also adds the package ebin to the workspace code path so
/// the app is loadable even if the workspace was started without it (e.g. via a
/// plain `beamtalk repl` session).
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
    // Must use -name (not -sname) to connect to the workspace which also uses -name.
    let client_node = format!("beamtalk_run_rpc_{pid}@localhost");

    #[cfg(windows)]
    let ebin_str = ebin_dir.to_string_lossy().replace('\\', "/");
    #[cfg(not(windows))]
    let ebin_str = ebin_dir.to_string_lossy();

    // RPC into the workspace: add package ebin then ensure_all_started.
    // {ok, []} = app already running; {ok, [_|_]} = freshly started.
    let eval_cmd = format!(
        "rpc:call('{workspace_node}', code, add_path, [\"{ebin_str}\"]), \
         Result = rpc:call('{workspace_node}', application, ensure_all_started, [{app_name}]), \
         case Result of \
             {{ok, []}} -> io:format(\"already~n\"), halt(0); \
             {{ok, _}} -> io:format(\"started~n\"), halt(0); \
             _ -> io:format(\"error: ~p~n\", [Result]), halt(1) \
         end."
    );

    let mut args = repl_startup::beam_pa_args(beam_paths);
    args.push(OsString::from("-name"));
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
             stdout: {}\n\
             stderr: {stderr}",
            stdout.trim()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project_with_manifest(temp: &TempDir, manifest: &str) -> String {
        let project_path = temp.path().to_string_lossy().to_string();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(
            src_path.join("main.bt"),
            "main := [\"Hello, World!\" length].",
        )
        .unwrap();
        fs::write(temp.path().join("beamtalk.toml"), manifest).unwrap();
        project_path
    }

    #[test]
    fn test_run_no_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = temp.path().to_string_lossy().to_string();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(src_path.join("main.bt"), "main := [42].").unwrap();
        // No beamtalk.toml

        let result = run(&project_path);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("No beamtalk.toml"),
            "Should mention missing manifest: {err}"
        );
    }

    #[test]
    fn test_run_no_start_field() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = run(&project_path);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("No start module defined"),
            "Should mention missing start field: {err}"
        );
    }

    #[test]
    fn test_run_with_invalid_path() {
        let result = run("/nonexistent/path");
        assert!(result.is_err());
    }

    #[test]
    fn test_run_with_start_field_builds() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\nstart = \"main\"\n",
        );

        let result = run(&project_path);

        // The build should succeed but the BEAM node start may fail
        // (no runtime in test environment). That's expected.
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            // These are acceptable failures in test environment
            if error_msg.contains("escript not found")
                || error_msg.contains("Could not find Beamtalk runtime")
                || error_msg.contains("Failed to build runtime")
                || error_msg.contains("Failed to build Beamtalk runtime")
                || error_msg.contains("Failed to start BEAM node")
                || error_msg.contains("Program exited with code")
            {
                return;
            }
            panic!("Run failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_run_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = temp.path().to_string_lossy().to_string();
        let src_path = temp.path().join("src");
        fs::create_dir_all(&src_path).unwrap();
        fs::write(src_path.join("main.bt"), "main := [1 + ].").unwrap();
        fs::write(
            temp.path().join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\nstart = \"main\"\n",
        )
        .unwrap();

        let result = run(&project_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_run_invalid_start_module_name() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\nstart = \"foo'bar\"\n",
        );

        let result = run(&project_path);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid start module"),
            "Should reject invalid start module name: {err}"
        );
    }

    #[test]
    fn test_run_empty_start_module_name() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project_with_manifest(
            &temp,
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\nstart = \"\"\n",
        );

        let result = run(&project_path);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid start module"),
            "Should reject empty start module name: {err}"
        );
    }
}
