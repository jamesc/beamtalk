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
//! Runs on every platform, not just macOS: `menu::build` itself is
//! `Runtime`-generic with no `#[cfg(target_os = "macos")]` gating around the
//! "Window" submenu or [`menu::CLOSE_WINDOW_ITEM_ID`] this test asserts on
//! (see `menu.rs`'s `build` — only a few *other* items in the menu, e.g. the
//! macOS app-name submenu, are platform-gated). The fact that `main.rs`
//! itself only *calls* `menu::build` under `#[cfg(target_os = "macos")]`
//! (Tauri never auto-installs a menu on Windows/Linux, so this app doesn't
//! build one there either — see `menu.rs`'s module doc) doesn't make the
//! function's own correctness a macOS-only concern; exercising it here on
//! every platform is strictly more coverage of genuinely shared code, not
//! scope creep.
//!
//! This file previously *did* gate its real assertions to
//! `#[cfg(target_os = "macos")]`, purely as a workaround: an earlier,
//! unconditional version crashed the compiled `menu_main_thread.exe` at
//! process startup on `windows-2022` CI with `STATUS_ENTRYPOINT_NOT_FOUND` —
//! an OS-loader symbol-resolution failure, before any test code ran at all.
//! BT-3253 tracked that down to a confirmed `tauri-build` limitation (not
//! ruled out — confirmed, both by reading `tauri-build`/`tauri-winres`
//! source and by finding the identical failure already reported upstream:
//! <https://github.com/tauri-apps/tauri/issues/13419>,
//! <https://github.com/orgs/tauri-apps/discussions/11179>): the Windows
//! resource `tauri_build::build()` compiles — which embeds the Common
//! Controls v6 manifest `tauri`'s `common-controls-v6` feature needs for its
//! native Win32 UI code — is linked via `cargo:rustc-link-arg-bins=...`,
//! which (per Cargo's own documented behavior) only reaches this crate's
//! `[[bin]]` target, never a `[[test]]` target. Without that manifest, the
//! OS loader resolves `comctl32.dll` to the old in-box version, which is
//! missing symbols (e.g. `SetWindowSubclass`) the v6 DLL exports — exactly
//! matching the observed crash. `build.rs`'s `embed_manifest_for_tests` now
//! embeds that same manifest into `[[test]]` targets too (mirroring the
//! identical fix `tauri` itself uses in its own test suite), which is what
//! makes running this file's real assertions on Windows possible at all.

#[path = "../src/menu.rs"]
#[allow(dead_code, unused_imports)]
mod menu;

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
