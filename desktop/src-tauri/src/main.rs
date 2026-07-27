// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

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
mod state;

use tauri::Manager;

use state::AppState;

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_single_instance::init(|app, _argv, _cwd| {
            // Second launch: focus the existing picker rather than starting
            // a second broker (ADR 0097 / BT-2984 spike: "the broker/
            // coordinator process itself should be single-instance").
            if let Some(window) = app.get_webview_window("picker") {
                let _ = window.set_focus();
                let _ = window.unminimize();
            }
        }))
        .setup(|app| {
            // Broker-restart duty (ADR 0097 Broker §4 / spike criterion (g)):
            // sweep any fronts orphaned by a previous, uncleanly-terminated
            // broker process before this one starts tracking anything new.
            if let Ok(dir) = beamtalk_desktop_broker::reap::state_dir() {
                let _ = beamtalk_desktop_broker::reap::sweep(&dir);
            }

            let launcher = launcher::resolve_launcher_path(app.handle());
            app.manage(AppState::new(launcher));
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            commands::list_workspaces,
            commands::attach,
            commands::detach,
            commands::create_workspace,
            commands::quit,
        ])
        .run(tauri::generate_context!())
        .expect("error while running the beamtalk desktop app");
}
