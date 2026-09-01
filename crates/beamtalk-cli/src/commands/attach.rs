// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk workspace attach` — connect to a running workspace without starting one.
//!
//! **DDD Context:** CLI
//!
//! Unlike `beamtalk repl`, which auto-discovers or starts a workspace,
//! `beamtalk workspace attach` only connects to an already-running workspace.
//! This enables multiple REPLs sharing the same workspace and scripted
//! attach to pre-started workspaces.

use miette::{Result, miette};

use super::repl::client::ReplClient;
use super::repl::color;
use super::repl::display::output_mode;
use super::workspace::{self, get_node_info, read_workspace_cookie};

/// Run the `beamtalk workspace attach` command.
///
/// Resolves a workspace to connect to (by name/ID, by port, or by current
/// directory lookup), validates it is running, and enters the shared REPL loop.
pub fn run(
    name_or_id: Option<&str>,
    port: Option<u16>,
    cookie: Option<&str>,
    no_color: bool,
) -> Result<()> {
    color::init(no_color);

    if let Some(port) = port {
        // Explicit port mode: connect directly to localhost:port
        let connect_cookie = cookie.map(String::from).ok_or_else(|| {
            miette!(
                "When using --port, a --cookie is required.\n\
                 Hint: find the cookie in ~/.beamtalk/workspaces/<id>/cookie"
            )
        })?;

        connect_and_run("127.0.0.1", port, &connect_cookie)
    } else {
        // Resolve by name/ID or current directory (same as `workspace stop`/`status`)
        attach_by_workspace_id(name_or_id)
    }
}

/// Attach to a workspace resolved by name/ID or current directory.
fn attach_by_workspace_id(name_or_id: Option<&str>) -> Result<()> {
    let workspace_id = super::workspace::lifecycle::resolve_workspace_id_or_cwd(name_or_id)?;

    if !workspace::workspace_exists(&workspace_id)? {
        return Err(match name_or_id {
            Some(name) => miette!(
                "Workspace '{name}' does not exist. \
                 Create it with `beamtalk workspace create {name} --background` \
                 or start a new one with `beamtalk repl --workspace {name}`"
            ),
            None => miette!(
                "No workspace found for current directory. \
                 Specify a name: beamtalk workspace attach <name>"
            ),
        });
    }

    let node_info = get_node_info(&workspace_id)?
        .ok_or_else(|| workspace_not_running(&workspace_id, name_or_id))?;

    if !workspace::is_node_running(&node_info, Some(&workspace_id)) {
        return Err(workspace_not_running(&workspace_id, name_or_id));
    }

    let cookie = read_workspace_cookie(&workspace_id)?.trim().to_string();

    let host = node_info.connect_host();

    println!("Attaching to workspace: {workspace_id}");
    if let Ok(metadata) = workspace::get_workspace_metadata(&workspace_id) {
        println!("  Project: {}", metadata.project_path.display());
    }
    println!("  Port:    {}", node_info.port);
    println!();

    connect_and_run(host, node_info.port, &cookie)
}

/// Error for when a workspace exists but is not running.
fn workspace_not_running(workspace_id: &str, name_or_id: Option<&str>) -> miette::Report {
    let label = name_or_id.unwrap_or(workspace_id);
    miette!(
        "Workspace '{label}' is not running. \
         Start it with `beamtalk repl --workspace {label}` \
         or `beamtalk workspace create {label} --background`"
    )
}

/// Connect to a REPL backend and enter the interactive loop.
fn connect_and_run(host: &str, port: u16, cookie: &str) -> Result<()> {
    let mut client = ReplClient::connect(host, port, cookie).map_err(|e| {
        miette!(
            "Failed to connect to {host}:{port}: {e}\nHint: is a workspace running on that port?"
        )
    })?;

    println!("Connected to REPL backend on port {port}.");

    // Show available actors
    match client.list_actors() {
        Ok(response) => {
            if let Some(actors) = response.actors {
                if !actors.is_empty() {
                    println!("\nAvailable actors:");
                    for actor in &actors {
                        println!(
                            "  - {}",
                            beamtalk_repl_protocol::format::format_actor_summary(
                                actor,
                                output_mode(),
                            )
                        );
                    }
                }
            }
        }
        Err(e) => {
            tracing::debug!("Could not list actors: {}", e);
        }
    }

    println!();

    super::repl::repl_loop(&mut client, host, port, cookie)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::test_support::{BeamtalkHomeOverride, WorkspaceFixture, real_home_guard};

    /// Bind then immediately drop a `TcpListener` to obtain a port nothing
    /// is listening on, so a connect attempt fails fast (connection
    /// refused) instead of timing out — mirrors the pattern used by
    /// `repl/process.rs` and `protocol.rs`'s own tests.
    fn unbound_port() -> u16 {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        drop(listener);
        port
    }

    // --- run(): --port without --cookie ---

    #[test]
    fn run_with_port_requires_cookie() {
        let err = run(None, Some(9999), None, false).unwrap_err();
        assert!(
            err.to_string()
                .contains("When using --port, a --cookie is required"),
            "got: {err}"
        );
    }

    #[test]
    fn run_with_port_and_unreachable_cookie_fails_to_connect() {
        // A cookie is provided, so we reach connect_and_run — but nothing is
        // listening on the port, so the connect itself fails.
        let port = unbound_port();
        let err = run(None, Some(port), Some("cookie"), false).unwrap_err();
        assert!(err.to_string().contains("Failed to connect"), "got: {err}");
    }

    // --- attach_by_workspace_id: workspace does not exist ---

    #[test]
    fn attach_by_workspace_id_missing_named_workspace() {
        let tmp = std::env::temp_dir().join(format!(
            "bt3375-attach-missing-named-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&tmp).expect("create tempdir");
        let _override = BeamtalkHomeOverride::new(&tmp);

        let err = attach_by_workspace_id(Some("nope")).unwrap_err();
        assert!(
            err.to_string().contains("Workspace 'nope' does not exist"),
            "got: {err}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn attach_by_workspace_id_missing_cwd_workspace() {
        let tmp =
            std::env::temp_dir().join(format!("bt3375-attach-missing-cwd-{}", std::process::id()));
        std::fs::create_dir_all(&tmp).expect("create tempdir");
        let _override = BeamtalkHomeOverride::new(&tmp);

        let err = attach_by_workspace_id(None).unwrap_err();
        assert!(
            err.to_string()
                .contains("No workspace found for current directory"),
            "got: {err}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    // --- workspace_not_running: error message shape ---

    #[test]
    fn workspace_not_running_uses_provided_label() {
        let err = workspace_not_running("ws-id-123", Some("myname"));
        let msg = err.to_string();
        assert!(msg.contains("Workspace 'myname' is not running"), "{msg}");
        assert!(!msg.contains("ws-id-123"), "{msg}");
    }

    #[test]
    fn workspace_not_running_falls_back_to_workspace_id_label() {
        let err = workspace_not_running("ws-id-123", None);
        let msg = err.to_string();
        assert!(
            msg.contains("Workspace 'ws-id-123' is not running"),
            "{msg}"
        );
    }

    // --- attach_by_workspace_id: workspace exists but not running ---

    #[test]
    fn attach_by_workspace_id_reports_not_running() {
        let port = unbound_port();
        let fixture = WorkspaceFixture::new("attach-not-running", port, 999_999);

        let err = attach_by_workspace_id(Some(&fixture.id)).unwrap_err();
        assert!(err.to_string().contains("is not running"), "got: {err}");
    }

    // --- connect_and_run: connection failure surfaces a hint ---

    #[test]
    fn connect_and_run_fails_with_hint_when_unreachable() {
        let port = unbound_port();
        let err = connect_and_run("127.0.0.1", port, "cookie").unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Failed to connect to"), "{msg}");
        assert!(msg.contains("is a workspace running"), "{msg}");
    }

    // Ensure `real_home_guard` stays referenced: `WorkspaceFixture` already
    // holds it internally, but importing it here documents the invariant
    // for readers scanning this module's test imports (BT-3370's guard
    // pattern).
    #[test]
    fn real_home_guard_is_available_for_direct_use() {
        let _guard = real_home_guard();
    }
}
