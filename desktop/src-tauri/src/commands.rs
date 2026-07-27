// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tauri commands wiring the picker frontend to `beamtalk-desktop-broker`
//! (process supervision) and `beamtalk-desktop-shell` (attach/empty-state
//! decisions) — ADR 0097 Implementation §3/§4, BT-2986.
//!
//! Every command here is a plain (non-`async`) `#[tauri::command]` — Tauri
//! dispatches these on its own thread pool rather than the main event loop,
//! so the blocking I/O inside (`spawn_front_with_port_retry`, `wait_ready`)
//! does not freeze the UI.

use std::time::Duration;

use tauri::{AppHandle, Emitter, Manager, State, WebviewUrl, WebviewWindowBuilder, WindowEvent};

use beamtalk_desktop_broker::monitor::{self, Monitor};
use beamtalk_desktop_broker::readiness::{self, ProbeTimeouts, ReadinessState};
use beamtalk_desktop_broker::spawn::SpawnAttemptConfig;
use beamtalk_desktop_broker::{cli_ops, discovery, reap};
use beamtalk_desktop_shell::attach::{AttachDecision, AttachedFront, window_label};

use crate::dto::{
    AttachOutcome, ConnectionStateChangedEvent, ConnectionStateView, EmptyStateView, PickerView,
    WorkspaceView,
};
use crate::state::AppState;

/// Overall attach timeout: generous enough for a slow-but-healthy front boot
/// plus the worst-case bad-cookie `/readiness` wait (Erlang's ~7s
/// `net_setuptime` — see `beamtalk_desktop_broker::readiness::ProbeTimeouts`'s
/// doc comment).
const ATTACH_TIMEOUT: Duration = Duration::from_secs(30);
const ATTACH_POLL_INTERVAL: Duration = Duration::from_millis(300);

/// List discovered workspaces plus the picker's first-run empty-state
/// classification (ADR 0097 Broker §5 / User Impact: "never a silent empty
/// list").
#[tauri::command]
pub fn list_workspaces(state: State<'_, AppState>) -> Result<PickerView, String> {
    let summaries = discovery::discover_workspaces().map_err(|e| e.to_string())?;
    let attach = state.attach.lock().map_err(|e| e.to_string())?;
    let workspaces: Vec<WorkspaceView> = summaries
        .iter()
        .map(|s| WorkspaceView::from_summary(s, attach.is_attached(&s.id)))
        .collect();
    let empty_state = beamtalk_desktop_shell::empty_state::classify_empty_state(
        workspaces.len(),
        cli_ops::resolve_cli_path,
    );
    Ok(PickerView {
        workspaces,
        empty_state: EmptyStateView::from(&empty_state),
    })
}

/// Attach to `workspace_id`: focus the existing window if already attached
/// (BT-2984 spike decision — attaching twice focuses, it does not spawn a
/// second front), else spawn a front, wait for two-stage readiness, open a
/// window, and start post-attach monitoring.
#[tauri::command]
pub fn attach(
    workspace_id: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<AttachOutcome, String> {
    let decision = {
        let attach = state.attach.lock().map_err(|e| e.to_string())?;
        attach.decide(&workspace_id)
    };

    if let AttachDecision::FocusExisting { window_id, .. } = decision {
        if let Some(window) = app.get_webview_window(&window_id) {
            let _ = window.set_focus();
        }
        return Ok(AttachOutcome::Focused);
    }

    emit_progress(&app, &workspace_id, "spawning");

    let launcher = state.launcher.clone();
    let spawn_config = SpawnAttemptConfig::new(launcher, workspace_id.clone());
    let (mut child, port) =
        beamtalk_desktop_broker::spawn::spawn_front_with_port_retry(&spawn_config)
            .map_err(|e| e.to_string())?;
    let pid = child.id();

    emit_progress(&app, &workspace_id, "probing");

    let timeouts = ProbeTimeouts::default_local();
    let probe = readiness::http_probe("127.0.0.1", port, timeouts);
    let final_state = readiness::wait_ready(
        ReadinessState::Spawning,
        ATTACH_TIMEOUT,
        ATTACH_POLL_INTERVAL,
        probe,
    );

    match final_state {
        ReadinessState::Ready(_version) => {}
        ReadinessState::Failed(reason) => {
            let _ = child.kill();
            return Err(format!(
                "workspace '{workspace_id}' is unreachable: {reason:?}"
            ));
        }
        ReadinessState::TimedOut(stage) => {
            let _ = child.kill();
            return Err(format!(
                "timed out waiting for workspace '{workspace_id}' ({stage:?})"
            ));
        }
        // wait_ready only ever returns one of the three arms above.
        _ => unreachable!("wait_ready only returns Ready, Failed, or TimedOut"),
    }

    persist_front_record(&workspace_id, port, pid);

    let label = window_label(&workspace_id);
    let url = format!("http://127.0.0.1:{port}/")
        .parse::<tauri::Url>()
        .map_err(|e| format!("invalid front URL: {e}"))?;
    let window = WebviewWindowBuilder::new(&app, label.clone(), WebviewUrl::External(url))
        .title(format!("Beamtalk — {workspace_id}"))
        .build()
        .map_err(|e| e.to_string())?;

    {
        let app_for_close = app.clone();
        let workspace_id_for_close = workspace_id.clone();
        window.on_window_event(move |event| {
            if let WindowEvent::CloseRequested { .. } = event {
                let state = app_for_close.state::<AppState>();
                let _ = detach_internal(&app_for_close, &state, &workspace_id_for_close);
            }
        });
    }

    {
        let mut attach = state.attach.lock().map_err(|e| e.to_string())?;
        attach.record_attached(AttachedFront {
            workspace_id: workspace_id.clone(),
            port,
            pid,
        });
    }
    state
        .children
        .lock()
        .map_err(|e| e.to_string())?
        .insert(workspace_id.clone(), child);

    spawn_monitor(app, workspace_id, port);

    Ok(AttachOutcome::Opened)
}

/// Detach `workspace_id`: kill its front process, clear bookkeeping, close
/// its window (ADR 0097 Broker §4 — "Detach/quit terminates the front
/// process").
#[tauri::command]
pub fn detach(
    workspace_id: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<(), String> {
    detach_internal(&app, &state, &workspace_id)
}

/// Shared by the `detach` command, "quit" (detach-all), and a workspace
/// window's own `CloseRequested` handler, so every path that ends an
/// attachment does the same three things: kill the process, clear
/// bookkeeping, close the window.
pub fn detach_internal(
    app: &AppHandle,
    state: &AppState,
    workspace_id: &str,
) -> Result<(), String> {
    let removed = state
        .attach
        .lock()
        .map_err(|e| e.to_string())?
        .remove(workspace_id);

    if let Some(front) = removed {
        if let Ok(dir) = reap::state_dir() {
            let _ = reap::remove_record(&dir, workspace_id, front.port);
        }
    }

    if let Some(mut child) = state
        .children
        .lock()
        .map_err(|e| e.to_string())?
        .remove(workspace_id)
    {
        let _ = child.kill();
        let _ = child.wait();
    }

    let label = window_label(workspace_id);
    if let Some(window) = app.get_webview_window(&label) {
        let _ = window.close();
    }

    Ok(())
}

/// `beamtalk workspace create <id> --background --persistent` via the
/// installed CLI (ADR 0097 Broker §5 — the first-run empty state's
/// "create a workspace" action).
#[tauri::command]
pub fn create_workspace(workspace_id: String) -> Result<(), String> {
    let cli_path = cli_ops::resolve_cli_path().map_err(|e| e.to_string())?;
    cli_ops::create_workspace(&cli_path, &workspace_id).map_err(|e| e.to_string())
}

/// Quit: detach every attached workspace (kills every front process, not
/// just the picker), then exit the app.
#[tauri::command]
pub fn quit(app: AppHandle, state: State<'_, AppState>) -> Result<(), String> {
    let ids: Vec<String> = {
        let attach = state.attach.lock().map_err(|e| e.to_string())?;
        attach
            .attached_ids()
            .into_iter()
            .map(str::to_string)
            .collect()
    };
    for id in ids {
        let _ = detach_internal(&app, &state, &id);
    }
    app.exit(0);
    Ok(())
}

fn emit_progress(app: &AppHandle, workspace_id: &str, stage: &str) {
    let _ = app.emit(
        "attach-progress",
        crate::dto::AttachProgressEvent {
            workspace_id: workspace_id.to_string(),
            stage: stage.to_string(),
        },
    );
}

fn persist_front_record(workspace_id: &str, port: u16, pid: u32) {
    let Ok(dir) = reap::state_dir() else {
        return;
    };
    let suffix = beamtalk_desktop_broker::sname::attach_node_suffix(workspace_id);
    let node_name = beamtalk_desktop_broker::sname::predict_node_name(&suffix, pid);
    let record = reap::FrontRecord {
        workspace_id: workspace_id.to_string(),
        port,
        pid,
        node_name,
        start_time: reap::read_start_time(pid),
    };
    let _ = reap::save_record(&dir, &record);
}

/// Post-attach monitoring (ADR 0097 Broker §3): periodically re-poll
/// `/readiness` and reflect transitions in the window (title prefix) and to
/// the picker frontend (an event), so a dead workspace shows as a clearly
/// disconnected window instead of the front's RPCs silently hanging or the
/// LiveView page filling with socket-error noise (spike criterion (f)).
/// Stops once the workspace is no longer tracked as attached (detach/quit
/// already ran).
fn spawn_monitor(app: AppHandle, workspace_id: String, port: u16) {
    std::thread::spawn(move || {
        let mut monitor = Monitor::new();
        loop {
            let still_attached = {
                let state = app.state::<AppState>();
                let Ok(attach) = state.attach.lock() else {
                    return;
                };
                attach.is_attached(&workspace_id)
            };
            if !still_attached {
                return;
            }

            let timeouts = ProbeTimeouts::default_local();
            let mut probe = readiness::http_probe("127.0.0.1", port, timeouts);
            let outcome = probe(&ReadinessState::WaitingReadiness);
            let poll_outcome = match outcome {
                readiness::ProbeOutcome::ReadinessOk(v) => monitor::PollOutcome::ReadinessOk(v),
                readiness::ProbeOutcome::ReadinessError(reason) => {
                    monitor::PollOutcome::ReadinessError(reason)
                }
                readiness::ProbeOutcome::HttpDown => monitor::PollOutcome::Unreachable,
                // http_probe only performs the HTTP-up check while
                // Spawning/WaitingHttp; passing WaitingReadiness above
                // always takes the /readiness branch, so HttpUp cannot be
                // observed here.
                readiness::ProbeOutcome::HttpUp => {
                    unreachable!("http_probe(WaitingReadiness) never returns HttpUp")
                }
            };

            if let Some(change) = monitor.observe(poll_outcome) {
                reflect_connection_state(&app, &workspace_id, &change.to);
            }

            std::thread::sleep(monitor::DEFAULT_POLL_INTERVAL);
        }
    });
}

/// Best-effort: prefix the workspace window's title on disconnect/
/// unreachable, clear it on reconnect. A richer in-page banner (injecting
/// DOM into the LiveView page via `WebviewWindow::eval`) was deliberately
/// not attempted here — the front's own CSP headers are untested against
/// eval-injected content from this shell, and a wrong guess there is worse
/// than the plain, always-works title-bar signal. Also emits an event so the
/// picker window's own list can show a status badge even when the workspace
/// window isn't focused.
fn reflect_connection_state(
    app: &AppHandle,
    workspace_id: &str,
    connection_state: &monitor::ConnectionState,
) {
    let label = window_label(workspace_id);
    if let Some(window) = app.get_webview_window(&label) {
        let base_title = format!("Beamtalk — {workspace_id}");
        let title = match connection_state {
            monitor::ConnectionState::Connected(_) => base_title,
            monitor::ConnectionState::Disconnected(_) => {
                format!("[Disconnected] {base_title}")
            }
            monitor::ConnectionState::FrontUnreachable => {
                format!("[Unreachable] {base_title}")
            }
        };
        let _ = window.set_title(&title);
    }

    let _ = app.emit(
        "connection-state-changed",
        ConnectionStateChangedEvent {
            workspace_id: workspace_id.to_string(),
            state: ConnectionStateView::from(connection_state),
        },
    );
}
