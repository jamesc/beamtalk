// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Windows-only, no-op elsewhere: without this, a release build links as a
// console-subsystem binary, so every launch of the installed app pops a
// black console window behind the picker's GUI (BT-2988, adversarial-review
// follow-up — missing from the standard Tauri v2 scaffold's own
// `main.rs.hbs` template was the actual bug; this restores it). Kept as
// console-subsystem in debug builds so `println!`/panics stay visible during
// `cargo tauri dev`.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

//! Beamtalk desktop picker (ADR 0097, BT-2986): a Tauri shell over
//! `beamtalk-desktop-broker`'s connection-broker core and
//! `beamtalk-desktop-shell`'s attach/empty-state decision logic.
//!
//! See `../../README.md` for the toolchain this crate needs — not available
//! in every dev sandbox, including the one this crate was authored in. This
//! crate could not be built, run, or manually QAed there; treat it as
//! reviewed-but-unverified until built on a machine with the Tauri
//! toolchain (`cargo tauri dev`).

mod commands;
mod dto;
mod launcher;
mod logging;
mod state;

use tauri::Manager;

use state::AppState;

fn main() {
    // Must run before any other `tracing::` call this process makes,
    // including the reap sweep and `resolve_launcher_path` calls in
    // `.setup()` just below — see `logging::init_logging`'s doc comment.
    // `_logging_guard` is never read again, but must stay alive until
    // `app.run()` below returns (dropping it early would silently stop
    // `launcher.log` file writes).
    let (_logging_guard, log_rx) = logging::init_logging();

    let app = tauri::Builder::default()
        .plugin(tauri_plugin_single_instance::init(|app, _argv, _cwd| {
            // Second launch: focus the existing picker rather than starting
            // a second broker (ADR 0097 / BT-2984 spike: "the broker/
            // coordinator process itself should be single-instance").
            if let Some(window) = app.get_webview_window("picker") {
                let _ = window.set_focus();
                let _ = window.unminimize();
            }
        }))
        .setup(move |app| {
            // The frontend log panel's live feed — needs a real AppHandle,
            // which only exists from here on (see `logging::spawn_log_relay`'s
            // doc comment for why this can't happen inside `init_logging`
            // itself).
            logging::spawn_log_relay(app.handle().clone(), log_rx);

            // Broker-restart duty (ADR 0097 Broker §4 / spike criterion (g)):
            // sweep any fronts orphaned by a previous, uncleanly-terminated
            // broker process before this one starts tracking anything new.
            match beamtalk_desktop_broker::reap::state_dir() {
                Ok(dir) => match beamtalk_desktop_broker::reap::sweep(&dir) {
                    Ok(report) => {
                        tracing::info!(?report, "swept orphaned fronts from a previous broker run");
                    }
                    Err(err) => tracing::warn!(%err, "orphan sweep failed"),
                },
                Err(err) => {
                    tracing::warn!(%err, "could not resolve broker state dir; skipped orphan sweep")
                }
            }

            let launcher = launcher::resolve_launcher_path(app.handle());
            tracing::info!(launcher = %launcher.display(), "resolved bt_attach launcher path");
            app.manage(AppState::new(launcher));
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            commands::list_workspaces,
            commands::attach,
            commands::detach,
            commands::create_workspace,
            commands::quit,
            commands::get_launcher_logs,
        ])
        .build(tauri::generate_context!())
        .expect("error while building the beamtalk desktop app");

    app.run(|app_handle, event| {
        // OS-level quit (Cmd-Q, taskbar close, SIGTERM, …) delivers
        // `ExitRequested` directly — it does not go through the `quit`
        // command's own detach-all (that only runs for the in-app "Quit"
        // button). Without this handler, an OS-level quit would leave every
        // attached front running as an orphan until the next broker
        // restart's sweep found it, instead of terminating it immediately
        // (ADR 0097 Broker §4 — "Detach/quit terminates the front process").
        if let tauri::RunEvent::ExitRequested { .. } = event {
            let state = app_handle.state::<AppState>();
            commands::detach_all(app_handle, &state);
        }
    });
}
