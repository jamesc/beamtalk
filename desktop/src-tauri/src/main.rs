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
mod menu;
mod state;

use std::sync::Mutex;

use tauri::Manager;

use logging::LoggingGuard;
use state::AppState;

fn main() {
    // Must run before any other `tracing::` call this process makes,
    // including the reap sweep and `resolve_launcher_path` calls in
    // `.setup()` just below — see `logging::init_logging`'s doc comment.
    // Managed as Tauri state (below, in `.setup()`) rather than left as a
    // local binding: `commands::quit` and this file's own
    // `RunEvent::ExitRequested` handler both need to call
    // `LoggingGuard::flush` explicitly at shutdown — a plain `let` binding
    // here would only flush on `main()`'s own scope exit, which
    // `app.run()` below may never reach (see `LoggingGuard::flush`'s doc
    // comment for why).
    let (logging_guard, log_rx) = logging::init_logging();

    #[allow(unused_mut)] // `mut` is only exercised by the macOS-only block below
    let mut builder =
        tauri::Builder::default().plugin(tauri_plugin_single_instance::init(|app, _argv, _cwd| {
            // Second launch: focus the existing picker rather than starting
            // a second broker (ADR 0097 / BT-2984 spike: "the broker/
            // coordinator process itself should be single-instance").
            if let Some(window) = app.get_webview_window("picker") {
                let _ = window.set_focus();
                let _ = window.unminimize();
            }
        }));

    // BT-3244: a custom app-wide menu, replacing Tauri's own auto-generated
    // default — see `menu::build`'s doc comment. Must be set via this
    // builder hook (not `AppHandle::set_menu` from inside `.setup()` below):
    // `Builder::build()` only auto-installs its own default menu when no
    // menu was configured here, so setting one here is what stops the
    // native ⌘W-bound "Close Window" item from ever being created, rather
    // than replacing it after the fact (`PredefinedMenuItem` has no
    // accelerator-override API to do that with anyway).
    //
    // macOS-only (adversarial-review follow-up): Tauri's own auto-install of
    // `Menu::default()` only ever happens on macOS (`Builder::build`'s
    // `#[cfg(target_os = "macos")]` block) — on Windows/Linux, a
    // `tauri::Builder` that never calls `.menu(...)` gets no menu bar at
    // all, since nothing else installs one. Calling `.menu(menu::build)`
    // unconditionally would therefore *add* a File/Edit/Window/Help menu bar
    // to every window on Windows/Linux that never had one, which is not
    // this issue's scope (its Windows/Linux acceptance criterion is limited
    // to verifying/documenting whether Ctrl+W reaches the page — see
    // `menu::build`'s doc comment). Gating the call, not `menu::build`
    // itself, mirrors Tauri's own source shape exactly: `Menu::default` is
    // written generically over any platform but is only ever *called* from
    // within that macOS-only block.
    #[cfg(target_os = "macos")]
    {
        builder = builder
            .menu(menu::build)
            .on_menu_event(|app_handle, event| menu::handle_event(app_handle, &event));
    }

    let app = builder
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
            app.manage(Mutex::new(logging_guard));

            // BT-3252: the picker (`tauri.conf.json`'s static `"picker"`
            // window) is the app's only always-present window and has no
            // tray icon or other affordance to reopen it — unlike each
            // per-workspace window (`commands::attach_and_open_window`'s own
            // `on_window_event`), closing it (traffic-light button, or the
            // app-wide "Close Window" menu item / ⇧⌘W now that it targets
            // whichever window has OS focus) previously just destroyed that
            // window with no app-level follow-up, leaving the app running
            // invisibly whenever a workspace window was still attached.
            // Closing the picker is therefore treated as equivalent to
            // quitting the whole app — the simplest, most predictable
            // behavior, and consistent with what closing what looks like the
            // app's main window intuitively does. Reuses `commands::quit`'s
            // exact detach-all-then-exit path (`quit_app`) rather than
            // duplicating it, the same way this file's own
            // `RunEvent::ExitRequested` handler below does for an OS-level
            // quit.
            if let Some(picker) = app.get_webview_window("picker") {
                let app_handle_for_close = app.handle().clone();
                picker.on_window_event(move |event| {
                    if let tauri::WindowEvent::CloseRequested { .. } = event {
                        let state = app_handle_for_close.state::<AppState>();
                        commands::quit_app(&app_handle_for_close, &state);
                    }
                });
            } else {
                // Should be unreachable — `tauri.conf.json` declares
                // `"picker"` as a static window Tauri creates before
                // `.setup()` runs — but logged rather than assumed, since a
                // future config change removing/renaming it would otherwise
                // silently regress back to this issue's exact bug with no
                // signal at all.
                tracing::warn!(
                    "picker window not found during setup; its CloseRequested handler was not \
                     registered (BT-3252 regression: closing it would leave the app running \
                     invisibly)"
                );
            }

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
            // Best-effort and idempotent (`LoggingGuard::flush` is a
            // `.take()`) — `commands::quit`'s in-app "Quit" path already
            // calls this itself, so this only matters for an OS-level quit
            // (Cmd-Q, taskbar close, SIGTERM, …), which never goes through
            // that command at all.
            if let Some(guard) = app_handle.try_state::<Mutex<LoggingGuard>>() {
                guard.lock().unwrap_or_else(|e| e.into_inner()).flush();
            }
        }
    });
}
