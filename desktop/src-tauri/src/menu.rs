// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! App-wide menu (BT-3244): identical in shape to Tauri v2's own
//! `Menu::default()` (`tauri-2.11.5/src/menu/menu.rs`), except every native
//! "Close Window" item is replaced with [`close_window_item`], a plain menu
//! item bound to ⇧⌘W instead of ⌘W.
//!
//! Why: the LiveView cockpit binds `mod+w` (Cmd/Ctrl+W) to
//! `tab_close_active` — close the focused editor tab — via the window-scoped
//! `KeyboardShortcuts` hook (`editors/liveview/lib/bt_attach_web/live/workspace_live.ex`,
//! `editors/liveview/assets/js/hooks/keyboard_shortcuts.js`). On macOS,
//! Tauri's default menu claims ⌘W in the OS menu bar for
//! `PredefinedMenuItem::close_window`, a *native* menu item — the OS handles
//! it before WKWebView ever sees a keydown, so the cockpit's own binding
//! never fires and ⌘W closes the whole window instead of just the tab.
//! [`PredefinedMenuItem`] has no accelerator-override API (only its label is
//! customizable), so the only way to free ⌘W is to not use it at all: build
//! a plain [`MenuItemBuilder`] item instead, bound to ⇧⌘W, and handle its
//! click in `main.rs`'s `on_menu_event` by closing the focused window
//! ourselves. With the native binding gone, ⌘W reaches the page and the
//! cockpit's existing `mod+w` handler takes it from there — no new JS
//! plumbing needed (see the issue's Context for this reasoning).
//!
//! Nothing else changes: the traffic-light button and this same item (now
//! under ⇧⌘W) still both close a window, and ⌘Q
//! (`PredefinedMenuItem::quit`, untouched below) still quits the app.
//!
//! Windows/Linux: `PredefinedMenuItem::close_window` binds Ctrl+W there too,
//! so the same swap applies uniformly by construction — *if* this menu ever
//! gets built there at all: `main.rs` only calls [`build`] under
//! `#[cfg(target_os = "macos")]`, since Tauri itself only auto-installs a
//! default menu on macOS — see that cfg block's own comment for why
//! unconditionally applying this on Windows/Linux would be a scope-creeping
//! regression (a menu bar those platforms never had before), not a fix.
//! Whether WebView2/WebKitGTK would even deliver a freed Ctrl+W keydown
//! through to the page the way WKWebView does is separately **unverified**
//! either way — this crate has never been run on a real Windows or Linux
//! desktop (see `../README.md`'s "What was and wasn't verified").
//!
//! Drift risk: this is a hand-copied replica of `Menu::default()`'s exact
//! shape as of `tauri 2.11.5` (`Cargo.lock`-pinned; `Cargo.toml` itself only
//! requires `"2"`). A `cargo update` that bumps `tauri` and changes
//! `Menu::default()`'s own shape (a new submenu, a reordered item, a new
//! predefined item) won't fail any build or test here — this file will just
//! silently stop matching upstream's shape. Re-diff this function against
//! `~/.cargo/registry/.../tauri-<new-version>/src/menu/menu.rs`'s
//! `Menu::default` after any `tauri` version bump.

use tauri::menu::{
    AboutMetadata, HELP_SUBMENU_ID, Menu, MenuItemBuilder, PredefinedMenuItem, Submenu,
    WINDOW_SUBMENU_ID,
};
use tauri::{AppHandle, Manager, Runtime};

/// Id of the custom "Close Window" item that stands in for
/// `PredefinedMenuItem::close_window` everywhere in [`build`] — matched in
/// `main.rs`'s `on_menu_event` handler to close the focused window.
pub const CLOSE_WINDOW_ITEM_ID: &str = "beamtalk-close-window";

/// Accelerator for the custom close-window item: ⌘W minus the OS-menu claim
/// moved to ⇧⌘W, freeing plain ⌘W for the cockpit's own `mod+w` binding (see
/// this module's doc comment).
const CLOSE_WINDOW_ACCELERATOR: &str = "CmdOrCtrl+Shift+W";

/// A fresh "Close Window" item bound to [`CLOSE_WINDOW_ACCELERATOR`]. Called
/// once per submenu that needs one (below), not shared — same pattern
/// Tauri's own `Menu::default()` uses for `PredefinedMenuItem::close_window`,
/// since a native menu item can't be attached to two submenus at once. On
/// macOS both the "File" and "Window" submenus get one, each a distinct
/// native item but deliberately sharing [`CLOSE_WINDOW_ITEM_ID`] — unlike
/// upstream's two `PredefinedMenuItem::close_window` calls, which get two
/// different auto-generated ids. That's intentional here, not an oversight:
/// [`handle_event`] treats every click on this id identically (close the
/// focused window), so there's nothing a per-submenu id would let it do
/// differently; sharing the id only matters if something later needs to
/// look one of these two items up individually (`Menu::get`/`Submenu::get`
/// only ever return the first match for a given id) — there is no such
/// caller today.
fn close_window_item<R: Runtime>(
    app_handle: &AppHandle<R>,
) -> tauri::Result<impl tauri::menu::IsMenuItem<R>> {
    MenuItemBuilder::with_id(CLOSE_WINDOW_ITEM_ID, "Close Window")
        .accelerator(CLOSE_WINDOW_ACCELERATOR)
        .build(app_handle)
}

/// Build the app-wide menu. Passed to `tauri::Builder::menu` in `main.rs` in
/// place of Tauri's own default-menu generation, so ⌘W is never claimed in
/// the first place — see this module's doc comment for why.
pub fn build<R: Runtime>(app_handle: &AppHandle<R>) -> tauri::Result<Menu<R>> {
    let pkg_info = app_handle.package_info();
    let config = app_handle.config();
    let about_metadata = AboutMetadata {
        name: Some(pkg_info.name.clone()),
        version: Some(pkg_info.version.to_string()),
        copyright: config.bundle.copyright.clone(),
        authors: config.bundle.publisher.clone().map(|p| vec![p]),
        ..Default::default()
    };

    let window_menu = Submenu::with_id_and_items(
        app_handle,
        WINDOW_SUBMENU_ID,
        "Window",
        true,
        &[
            &PredefinedMenuItem::minimize(app_handle, None)?,
            &PredefinedMenuItem::maximize(app_handle, None)?,
            #[cfg(target_os = "macos")]
            &PredefinedMenuItem::separator(app_handle)?,
            &close_window_item(app_handle)?,
        ],
    )?;

    let help_menu = Submenu::with_id_and_items(
        app_handle,
        HELP_SUBMENU_ID,
        "Help",
        true,
        &[
            #[cfg(not(target_os = "macos"))]
            &PredefinedMenuItem::about(app_handle, None, Some(about_metadata.clone()))?,
        ],
    )?;

    Menu::with_items(
        app_handle,
        &[
            #[cfg(target_os = "macos")]
            &Submenu::with_items(
                app_handle,
                pkg_info.name.clone(),
                true,
                &[
                    &PredefinedMenuItem::about(app_handle, None, Some(about_metadata))?,
                    &PredefinedMenuItem::separator(app_handle)?,
                    &PredefinedMenuItem::services(app_handle, None)?,
                    &PredefinedMenuItem::separator(app_handle)?,
                    &PredefinedMenuItem::hide(app_handle, None)?,
                    &PredefinedMenuItem::hide_others(app_handle, None)?,
                    &PredefinedMenuItem::separator(app_handle)?,
                    // Quit stays the native predefined item — ⌘Q is
                    // unaffected by this module (BT-3244 acceptance
                    // criteria: "Quit (⌘Q) is unaffected").
                    &PredefinedMenuItem::quit(app_handle, None)?,
                ],
            )?,
            #[cfg(not(any(
                target_os = "linux",
                target_os = "dragonfly",
                target_os = "freebsd",
                target_os = "netbsd",
                target_os = "openbsd"
            )))]
            &Submenu::with_items(
                app_handle,
                "File",
                true,
                &[
                    &close_window_item(app_handle)?,
                    #[cfg(not(target_os = "macos"))]
                    &PredefinedMenuItem::quit(app_handle, None)?,
                ],
            )?,
            &Submenu::with_items(
                app_handle,
                "Edit",
                true,
                &[
                    &PredefinedMenuItem::undo(app_handle, None)?,
                    &PredefinedMenuItem::redo(app_handle, None)?,
                    &PredefinedMenuItem::separator(app_handle)?,
                    &PredefinedMenuItem::cut(app_handle, None)?,
                    &PredefinedMenuItem::copy(app_handle, None)?,
                    &PredefinedMenuItem::paste(app_handle, None)?,
                    &PredefinedMenuItem::select_all(app_handle, None)?,
                ],
            )?,
            #[cfg(target_os = "macos")]
            &Submenu::with_items(
                app_handle,
                "View",
                true,
                &[&PredefinedMenuItem::fullscreen(app_handle, None)?],
            )?,
            &window_menu,
            &help_menu,
        ],
    )
}

/// Handle a menu-bar click: the only custom item this app's menu has is
/// [`CLOSE_WINDOW_ITEM_ID`], so close whichever window currently has focus —
/// the same target the native "Close Window" item it replaces would have
/// acted on. Uses [`tauri::WebviewWindow::close`], not `.destroy()`: `close()`
/// goes through the normal `WindowEvent::CloseRequested` path, so a
/// workspace window's existing close handler (`commands::attach_and_open_window`'s
/// `on_window_event` — detach the front, kill its process) still runs, same
/// as clicking the titlebar close button.
///
/// Finds the focused window via `Manager::webview_windows` +
/// `WebviewWindow::is_focused`, both stable APIs — deliberately *not*
/// `Manager::get_focused_window` (adversarial-review follow-up, BT-3244):
/// that one method is gated behind Tauri's `unstable` cargo feature only as
/// a docs/API-stability marker on its own signature, but the feature flag
/// itself is not scoped that narrowly — enabling it flips
/// `tauri-runtime-wry`'s webview creation strategy for *every* window in the
/// app (`WebviewKind::WindowChild` instead of `WindowContent`, plus
/// manual bounds tracking) and turns on the `create_webview` IPC command.
/// None of that is a tradeoff this one menu handler should be making on the
/// whole app's behalf, and it isn't necessary: this loop is exactly what
/// `AppManager::get_focused_window` itself does internally, just via the
/// stable webview-level accessor instead of the unstable window-level one.
/// No window reporting focused (all minimized, or the app is active with no
/// key window) is a real, if rare, state — logged rather than silently
/// swallowed so a "clicked Close Window but nothing happened" report has a
/// trail.
pub fn handle_event<R: Runtime>(app_handle: &AppHandle<R>, event: &tauri::menu::MenuEvent) {
    if event.id() != CLOSE_WINDOW_ITEM_ID {
        return;
    }
    let focused = app_handle
        .webview_windows()
        .into_values()
        .find(|window| window.is_focused().unwrap_or(false));
    match focused {
        Some(window) => {
            let _ = window.close();
        }
        None => tracing::debug!("Close Window menu item activated with no focused window"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tauri::menu::{MenuEvent, MenuId};

    // `build`'s own success isn't covered by a `#[test]` *here*: constructing
    // it (even against `tauri::test::mock_app`'s headless `MockRuntime`)
    // still creates real native `muda` menu items on macOS, and `muda`
    // enforces AppKit's rule that `NSMenuItem`s can only be created on the
    // process's actual main thread — confirmed by hand, not assumed: Rust's
    // default test harness runs every `#[test]` on its own worker thread, so
    // a `build(app.handle())` call here panics with `` `muda::MenuChild` can
    // only be created on the main thread `` every time, regardless of what
    // `build` itself does. That coverage lives in `../tests/menu_main_thread.rs`
    // instead — a `harness = false` integration test whose `fn main` *is*
    // the process main thread, `#[path]`-including this file to reach
    // `build` and its constants without making this bin-only crate a
    // library. See that file's doc comment for the full reasoning.

    #[test]
    fn handle_event_ignores_ids_other_than_the_close_window_item() {
        let app = tauri::test::mock_app();
        // No window is open in this mock app, so `webview_windows()` is
        // empty and no window can report `is_focused()` — this test's
        // purpose is confirming `handle_event` doesn't panic or act on an
        // unrelated menu id, not exercising the close path itself (which
        // needs a real, focusable window that this crate's dev sandboxes
        // can't create — see `../README.md`).
        let event = MenuEvent {
            id: MenuId::new("some-other-menu-item"),
        };
        handle_event(app.handle(), &event);
    }
}
