// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk attach` — connect to a running workspace without starting one.
//!
//! **DDD Context:** CLI
//!
//! Unlike `beamtalk repl`, which auto-discovers or starts a workspace,
//! `beamtalk attach` only connects to an already-running workspace. This
//! enables multiple REPLs sharing the same workspace, scripted attach to
//! pre-started workspaces, and explicit port-based connections.

use miette::{Result, miette};

use super::repl::client::ReplClient;
use super::repl::color;
use super::workspace::{
    self, WorkspaceStatus, WorkspaceSummary, get_node_info, list_workspaces, read_workspace_cookie,
    resolve_workspace_id,
};

/// Run the `beamtalk attach` command.
///
/// Resolves a workspace to connect to (by ID, by port, or interactively),
/// validates it is running, and enters the shared REPL loop.
pub fn run(
    workspace_id: Option<&str>,
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
    } else if let Some(ws_id) = workspace_id {
        // Explicit workspace ID
        attach_by_workspace_id(ws_id)
    } else {
        // No args: list running workspaces and prompt/select
        attach_interactive()
    }
}

/// Attach to a workspace by its ID.
fn attach_by_workspace_id(name_or_id: &str) -> Result<()> {
    let workspace_id = resolve_workspace_id(name_or_id)?;

    if !workspace::workspace_exists(&workspace_id)? {
        return Err(miette!(
            "Workspace '{name_or_id}' is not running. Start it with `beamtalk repl` or `beamtalk workspace create`"
        ));
    }

    let node_info = get_node_info(&workspace_id)?
        .ok_or_else(|| miette!(
            "Workspace '{name_or_id}' is not running. Start it with `beamtalk repl` or `beamtalk workspace create`"
        ))?;

    if !workspace::is_node_running(&node_info) {
        return Err(miette!(
            "Workspace '{name_or_id}' is not running. Start it with `beamtalk repl` or `beamtalk workspace create`"
        ));
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

/// When no args are provided, list running workspaces and auto-select or error.
fn attach_interactive() -> Result<()> {
    let workspaces = list_workspaces()?;
    let running: Vec<&WorkspaceSummary> = workspaces
        .iter()
        .filter(|ws| ws.status == WorkspaceStatus::Running)
        .collect();

    if running.is_empty() {
        return Err(miette!(
            "No workspaces running. Start one with `beamtalk repl`"
        ));
    }

    if running.len() == 1 {
        let ws = running[0];
        return attach_by_workspace_id(&ws.workspace_id);
    }

    // Multiple running workspaces — list them and ask user to specify
    eprintln!("Multiple workspaces running. Specify which one to attach to:\n");
    for ws in &running {
        let port_str = ws.port.map_or_else(String::new, |p| format!(" (port {p})"));
        eprintln!(
            "  beamtalk attach {}{}  — {}",
            ws.workspace_id,
            port_str,
            ws.project_path.display()
        );
    }
    eprintln!();
    Err(miette!(
        "Multiple workspaces running. Use `beamtalk attach <workspace-id>` to specify one."
    ))
}

/// Connect to a REPL backend and enter the interactive loop.
fn connect_and_run(host: &str, port: u16, cookie: &str) -> Result<()> {
    let mut client = ReplClient::connect(host, port, cookie).map_err(|_| {
        miette!("Connection refused at {host}:{port} — is a workspace running on that port?")
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
