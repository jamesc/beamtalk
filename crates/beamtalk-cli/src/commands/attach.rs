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
                 Create it with `beamtalk workspace create` or start a new one with `beamtalk repl`"
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
         Start it with `beamtalk repl` or `beamtalk workspace create --background`"
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
                    for actor in actors {
                        println!("  - {} ({})", actor.class, actor.pid);
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
