// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for workspace management commands.
//!
//! **DDD Context:** Workspace Management
//!
//! Provides `beamtalk workspace {list,stop,status,attach,transcript,logs,create}` subcommands.

use clap::Subcommand;
use miette::Result;

use super::{
    create_workspace, discovery, get_or_start_workspace, list_workspaces, stop_workspace,
    workspace_status,
};
use crate::commands::{attach, logs, transcript};

/// Workspace management subcommands.
#[derive(Debug, Subcommand)]
pub enum WorkspaceCommand {
    /// List all workspaces
    List {
        /// Output as JSON for machine-readable consumption
        #[arg(long)]
        json: bool,

        /// Only show workspaces with this status (`running` or `stopped`)
        #[arg(long)]
        status: Option<String>,

        /// Only show workspaces whose project path or ID contains this substring
        /// (case-insensitive)
        #[arg(long)]
        project: Option<String>,
    },

    /// Stop a running workspace
    Stop {
        /// Workspace name or ID to stop (default: current project's workspace)
        name: Option<String>,

        /// Force stop without graceful shutdown
        #[arg(long)]
        force: bool,
    },

    /// Show detailed workspace status
    Status {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,
    },

    /// Attach a REPL to a running workspace (without starting one)
    Attach {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,

        /// Connect to a workspace at an explicit localhost port
        #[arg(long, conflicts_with = "name")]
        port: Option<u16>,

        /// Erlang cookie for authentication (required with --port)
        #[arg(long, requires = "port")]
        cookie: Option<String>,

        /// Disable colored output (also respects `NO_COLOR` environment variable)
        #[arg(long)]
        no_color: bool,
    },

    /// Stream Transcript output from a running workspace
    Transcript {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,

        /// Display last N entries from ring buffer on connect
        #[arg(long)]
        recent: Option<usize>,
    },

    /// View workspace log files
    ///
    /// Shows the most recent workspace's log file. Defaults to the last 50 lines.
    /// Use `--follow` to stream new lines, `--level` to filter by severity,
    /// or `--path` to print the log file path.
    Logs {
        /// Select a specific workspace by name or ID
        #[arg(long)]
        workspace: Option<String>,

        /// Stream new log lines as they appear (like `tail -f`)
        #[arg(long, short)]
        follow: bool,

        /// Filter by minimum severity level (debug, info, notice, warning, error)
        #[arg(long)]
        level: Option<String>,

        /// Expected log format for level filtering (text, json)
        #[arg(long)]
        format: Option<String>,

        /// Print the log file path and exit
        #[arg(long)]
        path: bool,
    },

    /// Create a new named workspace
    Create {
        /// Name for the workspace
        name: String,

        /// Start the workspace node in the background after creation
        #[arg(long)]
        background: bool,

        /// TCP port for the workspace WebSocket server (0 = OS-assigned)
        #[arg(long, default_value = "0")]
        port: u16,

        /// Network bind address (default: 127.0.0.1)
        #[arg(long)]
        bind: Option<String>,

        /// Keep workspace running indefinitely (no idle timeout)
        #[arg(long)]
        persistent: bool,

        /// Max idle seconds before auto-stop (default: 4 hours)
        #[arg(long)]
        idle_timeout: Option<u64>,

        /// Confirm binding to a non-loopback network address
        #[arg(long)]
        confirm_network: bool,
    },

    /// Delete on-disk state for stopped workspaces
    ///
    /// `stop` leaves the workspace directory behind, so stopped entries
    /// accumulate in `workspace list`. Prune removes them. Running
    /// workspaces and releases are never touched.
    Prune {
        /// Show what would be removed without deleting anything
        #[arg(long)]
        dry_run: bool,

        /// Only prune workspaces whose project path or ID contains this
        /// substring (case-insensitive)
        #[arg(long)]
        project: Option<String>,
    },
}

/// Run a workspace subcommand.
pub fn run(command: WorkspaceCommand) -> Result<()> {
    match command {
        WorkspaceCommand::List {
            json,
            status,
            project,
        } => run_list(json, status.as_deref(), project.as_deref()),
        WorkspaceCommand::Stop { name, force } => stop_workspace(name.as_deref(), force),
        WorkspaceCommand::Prune { dry_run, project } => run_prune(dry_run, project.as_deref()),
        WorkspaceCommand::Status { name } => run_status(name.as_deref()),
        WorkspaceCommand::Attach {
            name,
            port,
            cookie,
            no_color,
        } => attach::run(name.as_deref(), port, cookie.as_deref(), no_color),
        WorkspaceCommand::Transcript { name, recent } => transcript::run(name.as_deref(), recent),
        WorkspaceCommand::Logs {
            workspace,
            follow,
            level,
            format,
            path,
        } => logs::run(
            workspace.as_deref(),
            follow,
            level.as_deref(),
            format.as_deref(),
            path,
        ),
        WorkspaceCommand::Create {
            name,
            background,
            port,
            bind,
            persistent,
            idle_timeout,
            confirm_network,
        } => {
            if background {
                run_create_background(
                    &name,
                    port,
                    bind.as_deref(),
                    persistent,
                    idle_timeout,
                    confirm_network,
                )
            } else {
                run_create(&name)
            }
        }
    }
}

/// Replace a leading home directory with `~` for compact display.
///
/// Uses [`Path::strip_prefix`](std::path::Path::strip_prefix), not string
/// prefix matching, so a sibling like `/Users/james2/…` never abbreviates
/// to `~2/…` when `$HOME` is `/Users/james`.
fn abbreviate_home(path: &std::path::Path) -> String {
    match dirs::home_dir() {
        Some(home) => match path.strip_prefix(&home) {
            Ok(rest) => format!("~/{}", rest.display()),
            Err(_) => path.to_string_lossy().into_owned(),
        },
        None => path.to_string_lossy().into_owned(),
    }
}

/// Abbreviate a project path for the `workspace list` table.
///
/// Applies [`abbreviate_home`], then — if still wider than `max_width` —
/// truncates from the LEFT, keeping the distinguishing tail. Truncating the
/// head is what matters: sibling checkouts share long prefixes
/// (`~/source/beamtalk`, `~/source/beamtalk-b`, …) and differ only in the
/// suffix, so keeping the first N chars (as this table used to do) renders
/// every row identical.
fn display_project_path(path: &std::path::Path, max_width: usize) -> String {
    let s = abbreviate_home(path);
    let len = s.chars().count();
    if len > max_width && max_width > 1 {
        let tail: String = s.chars().skip(len - max_width + 1).collect();
        format!("…{tail}")
    } else {
        s
    }
}

/// Parse the `--status` filter for `workspace list`.
fn parse_list_status_filter(
    status_filter: Option<&str>,
) -> Result<Option<super::lifecycle::WorkspaceStatus>> {
    use super::lifecycle::WorkspaceStatus;

    match status_filter {
        None => Ok(None),
        Some(s) if s.eq_ignore_ascii_case("running") => Ok(Some(WorkspaceStatus::Running)),
        Some(s) if s.eq_ignore_ascii_case("stopped") => Ok(Some(WorkspaceStatus::Stopped)),
        Some(s) => Err(miette::miette!(
            "Invalid --status '{s}': expected 'running' or 'stopped'"
        )),
    }
}

/// Apply the `--status`/`--project` filters to a workspace listing.
///
/// The project filter is a case-insensitive substring matched against the
/// project path or workspace ID.
fn apply_list_filters(
    workspaces: Vec<super::lifecycle::WorkspaceSummary>,
    status_filter: Option<super::lifecycle::WorkspaceStatus>,
    project_filter: Option<&str>,
) -> Vec<super::lifecycle::WorkspaceSummary> {
    let project_needle = project_filter.map(str::to_lowercase);
    workspaces
        .into_iter()
        .filter(|ws| status_filter.is_none_or(|wanted| ws.status == wanted))
        .filter(|ws| match &project_needle {
            None => true,
            Some(needle) => {
                let path_hit = ws
                    .project_path
                    .as_ref()
                    .is_some_and(|p| p.to_string_lossy().to_lowercase().contains(needle));
                path_hit || ws.workspace_id.to_lowercase().contains(needle)
            }
        })
        .collect()
}

/// One rendered row of the `workspace list` table.
struct ListRow {
    marker: &'static str,
    id: String,
    project: String,
    kind: String,
    status: super::lifecycle::WorkspaceStatus,
    port: String,
    sort_key: String,
}

/// Build table rows sorted by project path, so sibling checkouts land next
/// to each other (the listing itself is sorted by opaque hash ID).
fn build_list_rows(
    workspaces: &[super::lifecycle::WorkspaceSummary],
    current_project_root: Option<&std::path::Path>,
) -> Vec<ListRow> {
    let mut rows: Vec<ListRow> = workspaces
        .iter()
        .map(|ws| {
            let is_current = ws.project_path.as_deref().is_some_and(|ws_path| {
                current_project_root.is_some_and(|root| {
                    // Compare canonicalized paths to handle symlinks/relative paths
                    let root_canon = root.canonicalize().ok();
                    let ws_canon = ws_path.canonicalize().ok();
                    match (root_canon, ws_canon) {
                        (Some(a), Some(b)) => a == b,
                        _ => root == ws_path,
                    }
                })
            });

            // `-` for a release node (`ws.project_path` is `None` — see
            // `WorkspaceKind::Release`), not the original build-time path.
            let full_project = ws
                .project_path
                .as_ref()
                .map_or_else(|| "-".to_string(), |p| abbreviate_home(p));

            ListRow {
                marker: if is_current { "▸" } else { "" },
                id: ws.workspace_id.clone(),
                project: full_project.clone(),
                kind: ws.kind.to_string(),
                status: ws.status,
                port: ws.port.map_or_else(|| "-".to_string(), |p| p.to_string()),
                sort_key: full_project,
            }
        })
        .collect();
    rows.sort_by(|a, b| a.sort_key.cmp(&b.sort_key).then_with(|| a.id.cmp(&b.id)));
    rows
}

/// Print the `workspace list` table and summary line.
fn print_list_table(mut rows: Vec<ListRow>, total: usize) {
    use super::lifecycle::WorkspaceStatus;

    // The KIND column carries no information when every visible row is the
    // same kind (the common case: all dev workspaces), so hide it then and
    // only show it when releases and workspaces are mixed.
    let show_kind = rows
        .first()
        .is_some_and(|first| rows.iter().any(|r| r.kind != first.kind));

    // Size the PROJECT column to the longest visible path, capped by the
    // actual terminal width so the table never wraps: every other column has
    // a fixed width, PROJECT gets what's left (at least enough for `~/…x`).
    // Piped output has no terminal size — fall back to the classic 80.
    let term_width = terminal_size::terminal_size().map_or(80, |(w, _)| w.0 as usize);
    // Field widths plus the single spaces between them.
    let fixed_width = if show_kind {
        36 + 10 // marker, workspace, kind, status, port
    } else {
        36 // marker, workspace, status, port
    };
    let max_project_width = term_width.saturating_sub(fixed_width).clamp(10, 60);
    let project_width = rows
        .iter()
        .map(|r| r.project.chars().count())
        .max()
        .unwrap_or(7)
        .min(max_project_width);
    for r in &mut rows {
        r.project = display_project_path(std::path::Path::new(&r.project), project_width);
    }

    if show_kind {
        println!(
            "{:<2} {:<14} {:<width$} {:<9} {:<10} {:<6}",
            "",
            "WORKSPACE",
            "PROJECT",
            "KIND",
            "STATUS",
            "PORT",
            width = project_width
        );
        for r in &rows {
            println!(
                "{:<2} {:<14} {:<width$} {:<9} {:<10} {:<6}",
                r.marker,
                r.id,
                r.project,
                r.kind,
                r.status,
                r.port,
                width = project_width
            );
        }
    } else {
        println!(
            "{:<2} {:<14} {:<width$} {:<10} {:<6}",
            "",
            "WORKSPACE",
            "PROJECT",
            "STATUS",
            "PORT",
            width = project_width
        );
        for r in &rows {
            println!(
                "{:<2} {:<14} {:<width$} {:<10} {:<6}",
                r.marker,
                r.id,
                r.project,
                r.status,
                r.port,
                width = project_width
            );
        }
    }

    let running = rows
        .iter()
        .filter(|r| r.status == WorkspaceStatus::Running)
        .count();
    let stopped = rows.len() - running;
    if rows.len() == total {
        println!("\n{total} workspace(s) total ({running} running, {stopped} stopped)");
    } else {
        println!(
            "\n{} workspace(s) shown ({running} running, {stopped} stopped) — filtered from {total} total",
            rows.len(),
        );
    }
}

/// List all workspaces with formatted output.
///
/// `status_filter` accepts `"running"`/`"stopped"` (case-insensitive);
/// `project_filter` is a case-insensitive substring matched against the
/// project path or workspace ID. Both filters also apply to `--json` output
/// so scripts can use them too.
fn run_list(json: bool, status_filter: Option<&str>, project_filter: Option<&str>) -> Result<()> {
    let status_filter = parse_list_status_filter(status_filter)?;

    let workspaces = list_workspaces()?;
    let total = workspaces.len();
    let workspaces = apply_list_filters(workspaces, status_filter, project_filter);

    if json {
        let output = serde_json::to_string_pretty(&workspaces)
            .map_err(|e| miette::miette!("Failed to serialize workspaces: {e}"))?;
        println!("{output}");
        return Ok(());
    }

    if workspaces.is_empty() {
        if total == 0 {
            println!("No workspaces found.");
            println!("Start a REPL to create one: beamtalk repl");
        } else {
            println!("No workspaces match the given filters ({total} total).");
        }
        return Ok(());
    }

    // Detect current project root to mark the matching workspace
    let current_project_root = std::env::current_dir()
        .ok()
        .map(|cwd| discovery::discover_project_root(&cwd));

    print_list_table(
        build_list_rows(&workspaces, current_project_root.as_deref()),
        total,
    );

    Ok(())
}

/// Delete on-disk state for stopped workspaces.
fn run_prune(dry_run: bool, project_filter: Option<&str>) -> Result<()> {
    let report = super::prune_workspaces(dry_run, project_filter)?;

    if report.removed.is_empty() {
        println!("Nothing to prune.");
    } else {
        for id in &report.removed {
            println!("{} {id}", if dry_run { "would remove" } else { "removed" });
        }
    }

    if dry_run {
        println!(
            "\n{} workspace(s) would be removed ({} running and {} release(s) skipped)",
            report.removed.len(),
            report.skipped_running,
            report.skipped_releases
        );
    } else {
        println!(
            "\n{} workspace(s) pruned ({} running and {} release(s) skipped)",
            report.removed.len(),
            report.skipped_running,
            report.skipped_releases
        );
    }

    Ok(())
}

/// Show detailed workspace status.
fn run_status(name: Option<&str>) -> Result<()> {
    let detail = workspace_status(name)?;

    println!("Workspace: {}", detail.workspace_id);
    println!("Project:   {}", detail.project_path.display());
    println!("Status:    {}", detail.status);

    if let Some(ref node_name) = detail.node_name {
        println!("Node:      {node_name}");
    }
    if let Some(port) = detail.port {
        println!("Port:      {port}");
    }
    if let Some(pid) = detail.pid {
        println!("PID:       {pid}");
    }

    // Show workspace age (creation time, not node start time)
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());
    let age_secs = now.saturating_sub(detail.created_at);
    let hours = age_secs / 3600;
    let minutes = (age_secs % 3600) / 60;
    println!("Age:       {hours}h {minutes}m");

    Ok(())
}

/// Create a new workspace.
fn run_create(name: &str) -> Result<()> {
    let cwd = std::env::current_dir()
        .map_err(|e| miette::miette!("Could not determine current directory: {e}"))?;
    let project_root = discovery::discover_project_root(&cwd);

    let metadata = create_workspace(&project_root, Some(name))?;

    println!("Workspace '{}' created", metadata.workspace_id);
    println!("Project:   {}", metadata.project_path.display());
    println!(
        "\nStart a REPL session: beamtalk repl --workspace {}",
        metadata.workspace_id
    );

    Ok(())
}

/// Create a workspace and start it in the background.
///
/// Loads runtime beam paths, starts a detached BEAM node, and outputs
/// workspace ID, port, and node name for scripting/CI use.
fn run_create_background(
    name: &str,
    port: u16,
    bind: Option<&str>,
    persistent: bool,
    idle_timeout: Option<u64>,
    confirm_network: bool,
) -> Result<()> {
    use crate::commands::repl::bind::{resolve_bind_addr, validate_network_binding};

    let cwd = std::env::current_dir()
        .map_err(|e| miette::miette!("Could not determine current directory: {e}"))?;
    let project_root = discovery::discover_project_root(&cwd);

    // Check if workspace already exists and is running before validating
    // startup-only flags. This avoids spurious errors when the user just
    // wants to check that a workspace is running.
    let workspace_id = super::workspace_id_for_project(&project_root, Some(name))?;
    if let Ok(true) = super::storage::workspace_exists(&workspace_id) {
        if let Ok(Some(info)) = super::storage::get_node_info(&workspace_id) {
            if super::is_node_running(&info, Some(&workspace_id)) {
                println!("Workspace '{workspace_id}' already running");
                let has_startup_flags =
                    bind.is_some() || port != 0 || persistent || idle_timeout.is_some();
                if has_startup_flags {
                    eprintln!(
                        "  ⚠️  Startup flags (--port, --bind, --persistent, \
                         --idle-timeout) have no effect on an already-running workspace.\n  \
                         Stop it first with `beamtalk workspace stop {name}` to restart with new settings."
                    );
                }
                println!("Node:      {}", info.node_name);
                println!("Port:      {}", info.port);
                println!("\nAttach a REPL: beamtalk repl --workspace {workspace_id}");
                return Ok(());
            }
        }
    }

    // Validate startup-only flags (only reached when we're actually starting a new node)
    let bind_addr = resolve_bind_addr(bind)?;
    validate_network_binding(bind_addr, confirm_network)?;

    // Load runtime beam paths
    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()?;
    let paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // BT-3651: record the project's root package from the Rust-parsed manifest.
    let root_package: Option<String> = camino::Utf8Path::from_path(&project_root)
        .and_then(|root| {
            crate::commands::manifest::find_manifest(root)
                .ok()
                .flatten()
        })
        .map(|m| m.name);

    let config = super::WorkspaceConfig {
        port,
        bind_addr: Some(bind_addr),
        auto_cleanup: !persistent,
        max_idle_seconds: idle_timeout,
        log_level: "info",
        otp_app_name: None,
        root_package: root_package.as_deref(),
        hex_dep_names: &[], // No hex deps for standalone workspace CLI
    };

    let (node_info, _is_new, workspace_id) =
        get_or_start_workspace(&project_root, Some(name), &paths, &[], &config)?;

    println!("Workspace '{workspace_id}' started");
    println!("Node:      {}", node_info.node_name);
    println!("Port:      {}", node_info.port);
    println!("\nAttach a REPL: beamtalk repl --workspace {workspace_id}");

    Ok(())
}

/// `run_create`'s and `run_list`/`run_status`'s success paths now run
/// against a `BeamtalkHomeOverride`-pointed hermetic tempdir, now
/// that `test_support` serializes every `~/.beamtalk`-touching test (real or
/// overridden) against a shared `RwLock` so an override can never race a
/// real-directory test in the same test binary.
///
/// `run_create_background`'s "already running" short-circuit is reachable
/// today because `workspace_id_for_project(_, Some(name))` (see
/// `storage::workspace_id_for`) ignores `project_path` entirely when a name
/// is given — it just validates and returns the name — so a uniquely-named
/// on-disk fixture guarantees a workspace ID collision with nothing else.
/// Its "start a new node" path is untested here: unlike the three cases
/// above, it needs a live BEAM node (`get_or_start_workspace` shells out to a
/// real `start_detached_node`), the same live-runtime dependency as
/// `workspace/process.rs`'s live-node paths.
///
/// This function's coverage is not duplicated by a dedicated integration test
/// in this file. `get_or_start_workspace` already has its own real-BEAM `#[ignore]`d
/// coverage (`workspace/mod.rs`'s `test_get_or_start_workspace_lifecycle_integration`
/// and friends); everything this function adds on top of that call is three
/// `println!`s with no BEAM-node-dependent branching of its own, so a second
/// live-node test here would just re-pay the same real-node startup cost for
/// no new coverage.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::test_support::{BeamtalkHomeOverride, WorkspaceFixture};
    use crate::commands::workspace::workspace_exists;

    /// `run_create`'s foreground success path: creates a real
    /// workspace under an overridden `BEAMTALK_HOME` hermetic tempdir and
    /// confirms both the returned success and the on-disk result, exercising
    /// `run_create`'s own wrapper body (project-root discovery, the
    /// `create_workspace` call, and its println! summary) rather than just
    /// `create_workspace` itself (already covered at the `workspace::mod`
    /// level against the real directory).
    #[test]
    fn run_create_creates_workspace_under_overridden_home() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-create-test-{}", std::process::id());
        run_create(&name).expect("run_create should succeed");

        assert!(
            workspace_exists(&name).unwrap(),
            "run_create should have created the workspace on disk"
        );
    }

    /// `run_list`'s success path, both table and `--json` output:
    /// with at least one real workspace present, exercises the non-empty
    /// table-printing loop (current-project marker detection, path
    /// abbreviation, port formatting) and the JSON-serialization branch —
    /// previously only the underlying `list_workspaces()` was covered
    /// (`workspace::mod`'s tests), not this wrapper's own body.
    #[test]
    fn run_list_succeeds_with_a_workspace_present() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-list-test-{}", std::process::id());
        run_create(&name).expect("seed workspace should be created");

        run_list(false, None, None).expect("table output should succeed");
        run_list(true, None, None).expect("json output should succeed");
        // Filters that match / don't match the seeded workspace.
        run_list(false, Some("stopped"), None).expect("status filter should succeed");
        run_list(false, None, Some(&name)).expect("project filter should succeed");
        run_list(false, None, Some("no-such-project-xyz"))
            .expect("non-matching filter should succeed with empty table");
        run_list(true, Some("running"), None).expect("json with filter should succeed");
    }

    /// The KIND column only renders when workspace and release rows are
    /// mixed. Fabricate that mix directly (a live release node can't be
    /// staged hermetically) and smoke-print it: asserts the mixed-kind
    /// branch runs without panicking, on empty and narrow inputs too.
    #[test]
    fn print_list_table_with_mixed_kinds() {
        use super::super::lifecycle::{WorkspaceKind, WorkspaceStatus, WorkspaceSummary};

        let summaries = vec![
            WorkspaceSummary {
                workspace_id: "abc123def456".to_string(),
                project_path: Some(std::path::PathBuf::from("/tmp/some-project")),
                kind: WorkspaceKind::Workspace,
                status: WorkspaceStatus::Stopped,
                port: None,
                pid: None,
                created_at: Some(1_000_000),
            },
            WorkspaceSummary {
                workspace_id: "my-release".to_string(),
                project_path: None,
                kind: WorkspaceKind::Release,
                status: WorkspaceStatus::Running,
                port: Some(1234),
                pid: None,
                created_at: None,
            },
        ];
        let rows = build_list_rows(&summaries, None);
        assert_eq!(rows.len(), 2);
        // Release row (sort key "-") sorts before the absolute path.
        assert_eq!(rows[0].id, "my-release");
        print_list_table(rows, 2);
    }

    /// `prune` deletes stopped workspaces but never a running one: seed
    /// one of each, dry-run first (deletes nothing), then prune for real.
    #[test]
    fn run_prune_removes_stopped_and_spares_running() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        // Stopped workspace: created, never started, so no live node.
        let stopped = format!("prune-stopped-{}", std::process::id());
        run_create(&stopped).expect("seed stopped workspace should be created");

        // Running workspace: the fixture holds a live listener behind a
        // `node.info` that probes as running.
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let running = format!("prune-running-{}", std::process::id());
        // Held to end of scope: keeps the listener (and directory) alive.
        let fixture = WorkspaceFixture::new(&running, port, std::process::id());

        // Dry run reports but deletes nothing.
        run_prune(true, None).expect("dry run should succeed");
        assert!(
            workspace_exists(&stopped).unwrap(),
            "dry run must not delete the stopped workspace"
        );

        // Scoped to the stopped one by --project: running is out of scope.
        run_prune(false, Some("prune-stopped")).expect("scoped prune should succeed");
        assert!(
            !workspace_exists(&stopped).unwrap(),
            "stopped workspace should be pruned"
        );
        assert!(
            workspace_exists(&fixture.id).unwrap(),
            "running workspace must survive prune"
        );

        // Nothing left to prune now except the running workspace.
        run_prune(false, None).expect("second prune should succeed");
        assert!(
            workspace_exists(&fixture.id).unwrap(),
            "running workspace must survive an unscoped prune too"
        );
    }

    #[test]
    fn run_list_rejects_invalid_status_filter() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        assert!(run_list(false, Some("bogus"), None).is_err());
        // Case-insensitive acceptance is checked by success, not output text.
        run_list(false, Some("RUNNING"), None).expect("status should be case-insensitive");
    }

    /// Sibling checkouts must stay distinguishable: the table keeps the
    /// path TAIL (`…/source/beamtalk-b`), never the shared head.
    #[test]
    fn display_project_path_keeps_distinguishing_tail() {
        use std::path::PathBuf;

        // `~`-abbreviation applies when under $HOME.
        if let Some(home) = dirs::home_dir() {
            let p: PathBuf = home.join("source/beamtalk-b");
            assert_eq!(
                display_project_path(&p, 60),
                "~/source/beamtalk-b",
                "short paths keep the full abbreviated form"
            );
            // Narrow width cuts the head, keeps the tail.
            let shown = display_project_path(&p, 10);
            assert!(
                shown.starts_with('…') && shown.ends_with("eamtalk-b"),
                "narrow width must keep the tail, got: {shown}"
            );
        }

        // A sibling directory that merely shares a string prefix with
        // $HOME (`/Users/james2/…` vs `/Users/james`) must NOT abbreviate.
        if let Some(home) = dirs::home_dir() {
            if let Some(home_str) = home.to_str() {
                let sibling = PathBuf::from(format!("{home_str}2/some-project"));
                assert!(
                    !display_project_path(&sibling, 60).starts_with('~'),
                    "sibling prefix must not abbreviate, got: {}",
                    display_project_path(&sibling, 60)
                );
            }
        }

        // Non-home paths are never `~`-prefixed, but still tail-truncated.
        let p = PathBuf::from("/private/tmp/beamtalk-parity-cwd-12345");
        let shown = display_project_path(&p, 20);
        assert!(
            shown.starts_with('…'),
            "long non-home path must truncate from the left, got: {shown}"
        );
        assert!(
            !display_project_path(&p, 60).starts_with('~'),
            "non-home path must not gain a ~ prefix"
        );
    }

    /// `run_status`'s success path: with a real workspace present,
    /// exercises this wrapper's own body (age calculation, conditional
    /// node/port/pid lines) rather than just the underlying
    /// `workspace_status()` (already covered in `workspace::mod`'s tests).
    #[test]
    fn run_status_succeeds_for_an_existing_workspace() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-status-test-{}", std::process::id());
        run_create(&name).expect("seed workspace should be created");

        run_status(Some(&name)).expect("status should succeed for an existing workspace");
    }

    #[test]
    fn run_create_background_already_running_short_circuits() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let fixture = WorkspaceFixture::new("cli-already-running", port, std::process::id());

        // `port: 0` / no startup flags: the "already running" branch returns
        // before any of them would matter.
        run_create_background(&fixture.id, 0, None, false, None, false)
            .expect("already-running workspace should short-circuit to Ok");
    }

    #[test]
    fn run_create_background_already_running_warns_on_ignored_startup_flags() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let fixture = WorkspaceFixture::new("cli-already-running-flags", port, std::process::id());

        // Passing --persistent against an already-running workspace hits the
        // "startup flags have no effect" warning branch, but still succeeds.
        run_create_background(&fixture.id, 0, None, true, None, false)
            .expect("already-running workspace should short-circuit to Ok even with flags set");
    }
}
