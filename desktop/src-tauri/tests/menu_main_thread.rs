// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `harness = false` (see `Cargo.toml`'s `[[test]]` entry for this file):
//! this file's own `fn main` becomes the entire test binary, run directly by
//! `cargo test` on a fresh process with no thread spawned for it — unlike
//! every `#[test]` in `src/menu.rs`'s own `#[cfg(test)] mod tests`, which
//! the standard libtest harness runs on a worker thread, not the process's
//! real main thread.
//!
//! That distinction is why this file exists at all (adversarial-review
//! follow-up, BT-3244): `muda` (the native menu backend `menu::build` goes
//! through) enforces AppKit's rule that `NSMenuItem`s can only be
//! constructed on the process's actual main thread, and panics otherwise —
//! confirmed by hand when a `menu::build` call was first tried as an
//! ordinary `#[test]` (see `src/menu.rs`'s test module for where that was
//! hit and abandoned). Running here instead is what makes it possible to
//! build a *real* menu — not just call into a `MockRuntime` handle without
//! ever materializing menu items — and assert on its actual shape. Without
//! this, nothing in the repo ever confirmed the custom "Close Window" item
//! is what actually gets built (as opposed to, say, silently falling back
//! to the native `PredefinedMenuItem::close_window` this whole change exists
//! to avoid), or that its accelerator string even parses — a typo there
//! would otherwise only surface as a panic at real app launch, via
//! `main.rs`'s `.expect("error while building the beamtalk desktop app")`.
//!
//! `#[path]`, not a `[lib]` target: this crate is bin-only by design (see
//! `Cargo.toml`'s header comment on why it's excluded from the root
//! workspace) and `menu.rs` has no reason to become a public library API
//! just for this one test to reach it. Including the same source file a
//! second time here is the common pattern for reaching a binary crate's
//! private modules from `tests/` — not a second, hand-copied implementation
//! (the "no duplicate implementations" rule doesn't apply to the compiler
//! including one file twice).
//!
//! macOS-only: `menu::build` is itself only ever *called* on macOS in
//! production (`main.rs`'s `.menu(menu::build)` is
//! `#[cfg(target_os = "macos")]`-gated — see `menu.rs`'s module doc for why:
//! Tauri never auto-installs a menu on Windows/Linux, so this app never
//! builds one there either), so scoping this test's real assertions to the
//! same platform matches what actually ships. That scoping also happens to
//! route around a real, separately-tracked Windows limitation: an earlier
//! version of this test ran unconditionally and crashed the compiled
//! `menu_main_thread.exe` at process startup on `windows-2022` CI with
//! `STATUS_ENTRYPOINT_NOT_FOUND` — an OS-loader symbol-resolution failure,
//! not a test assertion, and it happened before any test code executed at
//! all (see BT-3253 for the investigation and leading theory: `tauri-build`'s
//! Windows resource/manifest linking likely only reaches the crate's
//! `[[bin]]` target, not arbitrary `[[test]]` targets, so `muda`'s native
//! Win32 menu-construction code — genuinely exercised here, unlike in any
//! other test in this crate — can't resolve a symbol it expects). The same
//! run's `test-desktop (macos-latest)` and `test-desktop (ubuntu-latest)`
//! both passed; macOS is the only platform this needs to run on regardless,
//! so BT-3253 is a test-infra follow-up, not a blocker for this file.

#[cfg(target_os = "macos")]
#[path = "../src/menu.rs"]
#[allow(dead_code, unused_imports)]
mod menu;

#[cfg(target_os = "macos")]
fn main() {
    let app = tauri::test::mock_app();
    let built = menu::build(app.handle()).expect("menu should build against a mock app");

    let window_submenu = built
        .get(tauri::menu::WINDOW_SUBMENU_ID)
        .and_then(|item| match item {
            tauri::menu::MenuItemKind::Submenu(submenu) => Some(submenu),
            _ => None,
        })
        .expect("the Window submenu should be present, as in Tauri's own Menu::default()");

    let close_item = window_submenu
        .get(menu::CLOSE_WINDOW_ITEM_ID)
        .expect("the Window submenu should contain the custom close-window item");

    // The native `PredefinedMenuItem::close_window` this replaces would
    // surface as `MenuItemKind::Predefined` instead of `MenuItem` — this
    // assertion is itself the regression check that ⌘W's native OS-menu
    // claim was never built (see `menu.rs`'s module doc for why that native
    // item can't simply have its accelerator overridden instead).
    assert!(
        matches!(close_item, tauri::menu::MenuItemKind::MenuItem(_)),
        "expected a plain MenuItem, not a native PredefinedMenuItem, so ⌘W is free for the \
         cockpit's own `mod+w` binding"
    );

    println!("menu_main_thread: ok");
}

// Non-macOS: `menu::build` is never called in production here (see the
// module doc above), and — for Windows specifically — actually calling it
// from this harness=false binary is the exact thing BT-3253 tracks as
// broken. Trivially succeed rather than omitting the `[[test]]` target
// outright, so `cargo test`'s target list stays identical across platforms.
#[cfg(not(target_os = "macos"))]
fn main() {}
