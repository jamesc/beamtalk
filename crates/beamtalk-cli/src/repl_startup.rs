// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL BEAM node startup configuration and runtime discovery.
//!
//! **DDD Context:** REPL — Startup Configuration & Runtime Discovery
//!
//! This module provides the canonical eval command, code-path arguments,
//! and runtime directory discovery for starting a BEAM node with the
//! Beamtalk REPL backend.  Both the production `beamtalk repl` command
//! (`process.rs`) and the REPL-protocol test harness (`tests/repl_protocol.rs`) consume
//! these helpers so that any change to the startup sequence is
//! automatically reflected in tests.

use std::ffi::OsString;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};

use beamtalk_core::codegen::core_erlang::escape_atom_chars;
use miette::{Result, miette};

/// OTP kernel logger configuration that redirects the default handler to stderr.
///
/// Used as `-kernel logger <this>` VM arg across REPL, workspace, and compilation
/// nodes (BT-1431). Ensures early boot OTP logger events go to stderr instead of
/// stdout, preventing them from being lost (detached nodes) or mixed into protocol
/// output (REPL/compilation nodes).
pub const KERNEL_LOGGER_STDERR: &str =
    "[{handler, default, logger_std_h, #{config => #{type => standard_error}}}]";

/// Whether the runtime was found in a development checkout or an installed layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeLayout {
    /// Development: `runtime/_build/default/lib/<app>/ebin/`
    Dev,
    /// Installed: `PREFIX/lib/beamtalk/lib/<app>/ebin/`
    Installed,
}

/// Directories that must be on the BEAM code path (`-pa`) for the REPL to work.
#[derive(Debug, Clone)]
pub struct BeamPaths {
    /// Path to the `beamtalk_runtime` application's `ebin/` directory.
    pub runtime_ebin: PathBuf,
    /// Path to the `beamtalk_workspace` application's `ebin/` directory.
    pub workspace_ebin: PathBuf,
    /// Path to the `beamtalk_compiler` application's `ebin/` directory.
    pub compiler_ebin: PathBuf,
    /// Path to the `beamtalk_stdlib` application's `ebin/` directory.
    /// In Dev layout this is `apps/beamtalk_stdlib/ebin/` (Beamtalk-compiled class beams).
    pub stdlib_ebin: PathBuf,
    /// Path to the rebar3-compiled Erlang modules for `beamtalk_stdlib`.
    /// In Dev layout this is `_build/default/lib/beamtalk_stdlib/ebin/`
    /// (contains `beamtalk_json.beam`, `beamtalk_regex.beam`, etc.).
    /// In Installed layout this equals `stdlib_ebin`.
    pub stdlib_erlang_ebin: PathBuf,
    /// Path to the `cowboy` HTTP server's `ebin/` directory (ADR 0020).
    pub cowboy_ebin: PathBuf,
    /// Path to the `cowlib` library's `ebin/` directory (cowboy dependency).
    pub cowlib_ebin: PathBuf,
    /// Path to the `ranch` acceptor pool's `ebin/` directory (cowboy dependency).
    pub ranch_ebin: PathBuf,
    /// Path to the `telemetry` event bus's `ebin/` directory (ADR 0069).
    pub telemetry_ebin: PathBuf,
    /// Path to the `telemetry_poller` periodic VM stats `ebin/` directory (ADR 0069).
    pub telemetry_poller_ebin: PathBuf,
}

/// Compute the standard `-pa` directories from a runtime root.
///
/// For development layout (`runtime/` dir), paths are under `_build/default/lib/`.
/// For installed layout (`PREFIX/lib/beamtalk/`), paths are under `lib/`.
pub fn beam_paths(runtime_dir: &Path) -> BeamPaths {
    beam_paths_for_layout(runtime_dir, RuntimeLayout::Dev)
}

/// Compute `-pa` directories for a specific layout.
pub fn beam_paths_for_layout(runtime_dir: &Path, layout: RuntimeLayout) -> BeamPaths {
    match layout {
        RuntimeLayout::Dev => {
            let build_lib_dir = runtime_dir.join("_build/default/lib");
            BeamPaths {
                runtime_ebin: build_lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: build_lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: build_lib_dir.join("beamtalk_compiler/ebin"),
                cowboy_ebin: build_lib_dir.join("cowboy/ebin"),
                cowlib_ebin: build_lib_dir.join("cowlib/ebin"),
                ranch_ebin: build_lib_dir.join("ranch/ebin"),
                telemetry_ebin: build_lib_dir.join("telemetry/ebin"),
                telemetry_poller_ebin: build_lib_dir.join("telemetry_poller/ebin"),
                // Stdlib class beams are produced by `beamtalk build-stdlib` under apps/, not _build/
                stdlib_ebin: runtime_dir.join("apps/beamtalk_stdlib/ebin"),
                // Stdlib Erlang module beams are rebar3-compiled to _build/
                stdlib_erlang_ebin: build_lib_dir.join("beamtalk_stdlib/ebin"),
            }
        }
        RuntimeLayout::Installed => {
            let lib_dir = runtime_dir.join("lib");
            BeamPaths {
                runtime_ebin: lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: lib_dir.join("beamtalk_compiler/ebin"),
                cowboy_ebin: lib_dir.join("cowboy/ebin"),
                cowlib_ebin: lib_dir.join("cowlib/ebin"),
                ranch_ebin: lib_dir.join("ranch/ebin"),
                telemetry_ebin: lib_dir.join("telemetry/ebin"),
                telemetry_poller_ebin: lib_dir.join("telemetry_poller/ebin"),
                stdlib_ebin: lib_dir.join("beamtalk_stdlib/ebin"),
                // In installed layout, Erlang modules and class beams are in the same dir
                stdlib_erlang_ebin: lib_dir.join("beamtalk_stdlib/ebin"),
            }
        }
    }
}

/// Build the Erlang `-eval` command that starts the REPL backend.
///
/// The returned string is suitable for passing as `erl -eval <cmd>`.
/// It performs the following steps:
/// 1. Stores the port in the application environment
/// 2. Starts the `beamtalk_workspace` OTP application (and its dependencies)
/// 3. Starts the workspace supervisor (which starts the REPL TCP server,
///    actor registry, session supervisor, and all singletons)
/// 4. Optionally starts the project's OTP application (BT-1340)
/// 5. Prints a ready message
/// 6. Blocks forever (the BEAM VM stays alive while the REPL runs)
pub fn build_eval_cmd(
    port: u16,
    bind_addr: Option<Ipv4Addr>,
    web_port: Option<u16>,
    log_level: &str,
    otp_app_name: Option<&str>,
    hex_dep_names: &[String],
) -> String {
    let hex_deps_start = hex_deps_start_fragment(hex_dep_names);
    let otp_app_start = otp_app_start_fragment(otp_app_name);
    format!(
        "{}, \
         {hex_deps_start}\
         {otp_app_start}\
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"BEAMTALK_PORT:~B~n\", [ActualPort]), \
         receive stop -> ok end.",
        startup_prelude(port, bind_addr, web_port, log_level),
    )
}

/// Build the Erlang `-eval` command with an explicit node name.
///
/// Like [`build_eval_cmd`] but also stores the node name in the application
/// environment before starting the workspace application.
///
/// The `node_name` is escaped for safe interpolation into an Erlang
/// single-quoted atom literal.
pub fn build_eval_cmd_with_node(
    port: u16,
    node_name: &str,
    bind_addr: Option<Ipv4Addr>,
    web_port: Option<u16>,
    log_level: &str,
    otp_app_name: Option<&str>,
    hex_dep_names: &[String],
) -> String {
    let safe_name = escape_atom_chars(node_name);
    let hex_deps_start = hex_deps_start_fragment(hex_dep_names);
    let otp_app_start = otp_app_start_fragment(otp_app_name);
    format!(
        "application:set_env(beamtalk_runtime, node_name, '{safe_name}'), \
         {}, \
         {hex_deps_start}\
         {otp_app_start}\
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"BEAMTALK_PORT:~B~n\", [ActualPort]), \
         receive stop -> ok end.",
        startup_prelude(port, bind_addr, web_port, log_level),
    )
}

/// Format an optional IPv4 address as an Erlang tuple string.
///
/// Used in eval commands passed to the BEAM node. Returns a string like
/// `{192,168,1,5}` for `Some(Ipv4Addr::new(192, 168, 1, 5))`, or
/// `{127,0,0,1}` for `None`.
pub fn format_bind_addr_erl(bind_addr: Option<Ipv4Addr>) -> String {
    match bind_addr {
        Some(ip) => {
            let octets = ip.octets();
            format!(
                "{{{},{},{},{}}}",
                octets[0], octets[1], octets[2], octets[3]
            )
        }
        None => "{127,0,0,1}".to_string(),
    }
}

/// Build the Erlang fragment that starts hex dep OTP applications (BT-1724).
///
/// ADR 0072: When a package has `[native.dependencies]`, the corresponding
/// OTP applications must be started before user code can call into them.
/// Returns `{ok, _} = application:ensure_all_started(dep), ...` for each
/// hex dep, or an empty string when there are no hex deps.
///
/// Used by all startup paths: script mode, test mode, REPL (foreground +
/// detached workspace).
pub fn hex_deps_start_fragment(hex_dep_names: &[String]) -> String {
    if hex_dep_names.is_empty() {
        return String::new();
    }
    let mut sorted = hex_dep_names.to_vec();
    sorted.sort();
    sorted
        .iter()
        .map(|name| format!("{{ok, _}} = application:ensure_all_started({name})"))
        .collect::<Vec<_>>()
        .join(", ")
        + ", "
}

/// Build the Erlang fragment that starts a project's OTP application (BT-1340).
///
/// When `otp_app_name` is `Some(name)`, returns
/// `{ok, _} = application:ensure_all_started(name), ` so that the supervision
/// tree is running before the REPL prompt appears.  Returns an empty string
/// when `None`.
fn otp_app_start_fragment(otp_app_name: Option<&str>) -> String {
    match otp_app_name {
        Some(name) => format!("{{ok, _}} = application:ensure_all_started({name}), "),
        None => String::new(),
    }
}

/// The startup prelude shared by all startup modes.
///
/// Starts the workspace OTP application and the workspace supervisor,
/// which brings up the full supervision tree: REPL TCP server, actor
/// registry, session supervisor, transcript stream, system dictionary,
/// workspace actor, and idle monitor.
///
/// The workspace uses a unique per-run ID and resolves the project path
/// to an absolute directory at startup to avoid cross-run metadata reuse.
///
/// Removes the default logger handler to prevent crashes when stdout/stderr
/// are piped (BT-686). This means log messages during application startup
/// (before the file logger is added in `beamtalk_workspace_sup:init/1`) will
/// be discarded. This is acceptable because:
/// 1. Application startup errors cause the entire VM to crash anyway (visible via stderr)
/// 2. The file logger is set up very early in `workspace_sup:init/1`
/// 3. The alternative (keeping default handler) crashes the logger on pipe close
///
/// Callers append their own shutdown logic (e.g. `receive stop -> ok end`
/// or cover export).
pub fn startup_prelude(
    port: u16,
    bind_addr: Option<Ipv4Addr>,
    web_port: Option<u16>,
    log_level: &str,
) -> String {
    let bind_addr_erl = format_bind_addr_erl(bind_addr);
    let web_port_erl = match web_port {
        Some(p) => format!("{p}"),
        None => "undefined".to_string(),
    };
    format!(
        "logger:remove_handler(default), \
         application:set_env(beamtalk_runtime, repl_port, {port}), \
         application:set_env(beamtalk_runtime, web_port, {web_port_erl}), \
         application:set_env(beamtalk_runtime, log_level, {log_level}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, Cwd}} = file:get_cwd(), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{ \
             workspace_id => list_to_binary(\"foreground_\" ++ integer_to_list(erlang:unique_integer([positive]))), \
             project_path => list_to_binary(Cwd), \
             tcp_port => {port}, \
             bind_addr => {bind_addr_erl}, \
             web_port => {web_port_erl}, \
             auto_cleanup => false, \
             max_idle_seconds => 14400}})"
    )
}

/// Collect `-pa` paths as `OsString` arguments for passing to `Command::args`.
///
/// Returns alternating `["-pa", "<path>", "-pa", "<path>", ...]`.
/// Uses `OsString` so non-UTF8 filesystem paths are handled without panicking.
///
/// On Windows, converts backslashes to forward slashes since Erlang expects
/// Unix-style paths in `-pa` arguments (see BT-661).
pub fn beam_pa_args(paths: &BeamPaths) -> Vec<OsString> {
    let dirs = [
        &paths.runtime_ebin,
        &paths.workspace_ebin,
        &paths.compiler_ebin,
        &paths.cowboy_ebin,
        &paths.cowlib_ebin,
        &paths.ranch_ebin,
        &paths.telemetry_ebin,
        &paths.telemetry_poller_ebin,
        &paths.stdlib_ebin,
        &paths.stdlib_erlang_ebin,
    ];
    let mut args = Vec::with_capacity(dirs.len() * 2);
    for dir in dirs {
        args.push(OsString::from("-pa"));
        #[cfg(windows)]
        {
            // Convert Windows backslashes to forward slashes for Erlang
            let path_str = dir.to_string_lossy().replace('\\', "/");
            args.push(OsString::from(path_str));
        }
        #[cfg(not(windows))]
        {
            args.push(dir.as_os_str().to_os_string());
        }
    }
    args
}

// ── Runtime Discovery ──────────────────────────────────────────────

/// Find the runtime directory by checking multiple locations.
///
/// Delegates to [`find_runtime_dir_with_layout`]; see that function for
/// layout detection and resolution order.
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir() -> Result<PathBuf> {
    let (path, _layout) = find_runtime_dir_with_layout()?;
    Ok(path)
}

/// Like [`find_runtime_dir`] but also returns the detected [`RuntimeLayout`].
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir_with_layout() -> Result<(PathBuf, RuntimeLayout)> {
    // Check explicit env var first
    if let Ok(dir) = std::env::var("BEAMTALK_RUNTIME_DIR") {
        let path = PathBuf::from(dir);
        if path.join("rebar.config").exists() {
            return Ok((path, RuntimeLayout::Dev));
        }
        return Err(miette!(
            "BEAMTALK_RUNTIME_DIR is set but does not contain a valid runtime (no rebar.config)"
        ));
    }

    // Dev-mode candidates (checked via rebar.config)
    let dev_candidates = [
        // 1. CARGO_MANIFEST_DIR (when running via cargo run)
        std::env::var("CARGO_MANIFEST_DIR")
            .ok()
            .map(|d| PathBuf::from(d).join("../../runtime")),
        // 2. Current working directory (running from repo root)
        Some(PathBuf::from("runtime")),
        // 3. Executable's grandparent (target/debug/beamtalk -> repo root)
        std::env::current_exe().ok().and_then(|exe| {
            exe.parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .map(|p| p.join("runtime"))
        }),
    ];

    for candidate in dev_candidates.into_iter().flatten() {
        if candidate.join("rebar.config").exists() {
            return Ok((candidate, RuntimeLayout::Dev));
        }
    }

    // Installed-mode candidate: {exe_dir}/../lib/beamtalk/
    // Validated by checking for .beam files in the runtime ebin dir
    if let Some(installed_root) = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(|p| p.join("../lib/beamtalk")))
        .and_then(|p| p.canonicalize().ok())
    {
        if has_beam_files(&installed_root.join("lib/beamtalk_runtime/ebin")) {
            return Ok((installed_root, RuntimeLayout::Installed));
        }
    }

    Err(miette!(
        "Could not find Beamtalk runtime directory.\n\
        Please run from the repository root or set BEAMTALK_RUNTIME_DIR."
    ))
}

/// Check whether a directory contains compiled `.beam` files.
pub fn has_beam_files(dir: &Path) -> bool {
    dir.is_dir()
        && std::fs::read_dir(dir)
            .map(|entries| {
                entries
                    .flatten()
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            })
            .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_cmd_contains_required_steps() {
        let cmd = build_eval_cmd(9000, None, None, "info", None, &[]);
        // Must set application env before starting apps
        assert!(cmd.contains("application:set_env(beamtalk_runtime, repl_port, 9000)"));
        // Must start the workspace OTP application
        assert!(cmd.contains("application:ensure_all_started(beamtalk_workspace)"));
        // Must start the workspace supervisor
        assert!(cmd.contains("beamtalk_workspace_sup:start_link"));
        assert!(cmd.contains("tcp_port => 9000"));
        // Must query and print the actual bound port (for ephemeral port discovery)
        assert!(cmd.contains("beamtalk_repl_server:get_port()"));
        assert!(cmd.contains("BEAMTALK_PORT:"));
        // Must block to keep the VM alive
        assert!(cmd.contains("receive stop -> ok end"));
        // Default bind address should be loopback
        assert!(cmd.contains("bind_addr => {127,0,0,1}"));
        // No web_port by default
        assert!(cmd.contains("web_port => undefined"));
    }

    #[test]
    fn eval_cmd_with_node_includes_node_name() {
        let cmd = build_eval_cmd_with_node(9000, "mynode", None, None, "info", None, &[]);
        assert!(cmd.contains("application:set_env(beamtalk_runtime, node_name, 'mynode')"));
        assert!(cmd.contains("beamtalk_workspace_sup:start_link"));
    }

    #[test]
    fn eval_cmd_with_node_escapes_special_chars() {
        let cmd = build_eval_cmd_with_node(9000, "node'inject", None, None, "info", None, &[]);
        assert!(cmd.contains("node\\'inject"));
        assert!(!cmd.contains("node'inject"));
    }

    #[test]
    fn startup_prelude_contains_set_env_and_ensure_started() {
        let prelude = startup_prelude(9000, None, None, "info");
        assert!(prelude.contains("application:set_env(beamtalk_runtime, repl_port, 9000)"));
        assert!(prelude.contains("application:ensure_all_started(beamtalk_workspace)"));
        // Must start workspace supervisor (which starts all singletons and services)
        assert!(prelude.contains("beamtalk_workspace_sup:start_link"));
        assert!(prelude.contains("tcp_port => 9000"));
        // Uses unique workspace_id and absolute project_path from Cwd
        assert!(prelude.contains("foreground_"));
        assert!(prelude.contains("unique_integer"));
        assert!(prelude.contains("file:get_cwd()"));
        // Prelude should NOT contain the blocking receive
        assert!(!prelude.contains("receive"));
        // Default bind address
        assert!(prelude.contains("bind_addr => {127,0,0,1}"));
        // No web_port by default
        assert!(prelude.contains("web_port => undefined"));
    }

    #[test]
    fn startup_prelude_sets_log_level() {
        let prelude = startup_prelude(9000, None, None, "debug");
        assert!(
            prelude.contains("application:set_env(beamtalk_runtime, log_level, debug)"),
            "Should set log_level in app env: {prelude}"
        );
    }

    #[test]
    fn startup_prelude_with_custom_bind_addr() {
        use std::net::Ipv4Addr;
        let prelude = startup_prelude(9000, Some(Ipv4Addr::new(192, 168, 1, 5)), None, "info");
        assert!(prelude.contains("bind_addr => {192,168,1,5}"));
    }

    #[test]
    fn startup_prelude_with_all_interfaces() {
        use std::net::Ipv4Addr;
        let prelude = startup_prelude(9000, Some(Ipv4Addr::UNSPECIFIED), None, "info");
        assert!(prelude.contains("bind_addr => {0,0,0,0}"));
    }

    #[test]
    fn startup_prelude_with_web_port() {
        let prelude = startup_prelude(9000, None, Some(8080), "info");
        assert!(prelude.contains("web_port => 8080"));
        assert!(prelude.contains("application:set_env(beamtalk_runtime, web_port, 8080)"));
    }

    #[test]
    fn eval_cmd_with_web_port() {
        let cmd = build_eval_cmd(9000, None, Some(9090), "info", None, &[]);
        assert!(cmd.contains("web_port => 9090"));
    }

    #[test]
    fn eval_cmd_with_otp_app_starts_application() {
        let cmd = build_eval_cmd(9000, None, None, "info", Some("my_app"), &[]);
        // Must start the OTP application after workspace bootstrap
        assert!(cmd.contains("application:ensure_all_started(my_app)"));
        // OTP app start must come after workspace_sup:start_link
        let ws_pos = cmd.find("beamtalk_workspace_sup:start_link").unwrap();
        let app_pos = cmd.find("application:ensure_all_started(my_app)").unwrap();
        assert!(
            app_pos > ws_pos,
            "OTP app must start after workspace supervisor"
        );
        // OTP app start must come before get_port
        let port_pos = cmd.find("beamtalk_repl_server:get_port()").unwrap();
        assert!(app_pos < port_pos, "OTP app must start before port query");
    }

    #[test]
    fn eval_cmd_without_otp_app_has_no_ensure_all_started_extra() {
        let cmd = build_eval_cmd(9000, None, None, "info", None, &[]);
        // Should only have ensure_all_started for beamtalk_workspace, not any other
        let count = cmd.matches("ensure_all_started").count();
        assert_eq!(count, 1, "Only beamtalk_workspace should be started");
    }

    #[test]
    fn eval_cmd_with_node_and_otp_app() {
        let cmd = build_eval_cmd_with_node(9000, "mynode", None, None, "info", Some("my_app"), &[]);
        assert!(cmd.contains("application:ensure_all_started(my_app)"));
        assert!(cmd.contains("application:set_env(beamtalk_runtime, node_name, 'mynode')"));
    }

    #[test]
    fn eval_cmd_with_hex_deps_starts_them_before_workspace() {
        let hex_deps = vec!["gun".to_string(), "cowboy".to_string()];
        let cmd = build_eval_cmd(9000, None, None, "info", None, &hex_deps);
        assert!(
            cmd.contains("ensure_all_started(cowboy)"),
            "Should start cowboy: {cmd}"
        );
        assert!(
            cmd.contains("ensure_all_started(gun)"),
            "Should start gun: {cmd}"
        );
        // Hex deps should come after workspace startup but before get_port
        let cowboy_pos = cmd.find("ensure_all_started(cowboy)").unwrap();
        let port_pos = cmd.find("beamtalk_repl_server:get_port()").unwrap();
        assert!(
            cowboy_pos < port_pos,
            "Hex deps should start before port query: {cmd}"
        );
    }

    #[test]
    fn eval_cmd_with_node_and_hex_deps() {
        let hex_deps = vec!["hackney".to_string()];
        let cmd = build_eval_cmd_with_node(9000, "mynode", None, None, "info", None, &hex_deps);
        assert!(
            cmd.contains("ensure_all_started(hackney)"),
            "Should start hackney: {cmd}"
        );
        assert!(
            cmd.contains("application:set_env(beamtalk_runtime, node_name, 'mynode')"),
            "Should set node name: {cmd}"
        );
    }

    #[test]
    fn beam_paths_uses_correct_layout() {
        let paths = beam_paths(Path::new("/rt"));
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.cowboy_ebin,
            PathBuf::from("/rt/_build/default/lib/cowboy/ebin")
        );
        assert_eq!(
            paths.cowlib_ebin,
            PathBuf::from("/rt/_build/default/lib/cowlib/ebin")
        );
        assert_eq!(
            paths.ranch_ebin,
            PathBuf::from("/rt/_build/default/lib/ranch/ebin")
        );
        assert_eq!(
            paths.telemetry_ebin,
            PathBuf::from("/rt/_build/default/lib/telemetry/ebin")
        );
        assert_eq!(
            paths.telemetry_poller_ebin,
            PathBuf::from("/rt/_build/default/lib/telemetry_poller/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_pa_args_alternates_flag_and_path() {
        let paths = beam_paths(Path::new("/rt"));
        let args = beam_pa_args(&paths);
        // Should be 20 elements: 10 dirs × 2 (flag + path)
        assert_eq!(args.len(), 20);
        for i in (0..args.len()).step_by(2) {
            assert_eq!(args[i], "-pa");
        }
    }

    #[test]
    fn beam_paths_installed_layout() {
        let paths = beam_paths_for_layout(
            Path::new("/usr/local/lib/beamtalk"),
            RuntimeLayout::Installed,
        );
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.cowboy_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/cowboy/ebin")
        );
        assert_eq!(
            paths.cowlib_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/cowlib/ebin")
        );
        assert_eq!(
            paths.ranch_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/ranch/ebin")
        );
        assert_eq!(
            paths.telemetry_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/telemetry/ebin")
        );
        assert_eq!(
            paths.telemetry_poller_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/telemetry_poller/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_stdlib/ebin")
        );
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_paths_dev_uses_build_dir() {
        let paths = beam_paths_for_layout(Path::new("/rt"), RuntimeLayout::Dev);
        // Dev layout uses _build/default/lib/ for runtime/workspace/etc.
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        // Class beams are under apps/
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
        // Erlang module beams are under _build/
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    #[cfg(windows)]
    fn beam_pa_args_converts_backslashes_to_forward_slashes() {
        // Create a Windows-style path with backslashes
        let runtime_dir = PathBuf::from("C:\\Users\\test\\beamtalk\\runtime");
        let paths = beam_paths(&runtime_dir);
        let args = beam_pa_args(&paths);

        // All path arguments (at odd indices) should contain forward slashes, not backslashes
        for (i, arg) in args.iter().enumerate() {
            if i % 2 == 1 {
                // This is a path argument (not a "-pa" flag)
                let path_str = arg.to_string_lossy();
                assert!(
                    !path_str.contains('\\'),
                    "Path argument at index {i} contains backslashes: {path_str}"
                );
                assert!(
                    path_str.contains('/'),
                    "Path argument at index {i} doesn't contain forward slashes: {path_str}"
                );
            }
        }
    }
}
