// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

fn main() {
    tauri_build::build();

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows")
        && std::env::var("CARGO_CFG_TARGET_ENV").as_deref() == Ok("msvc")
    {
        embed_manifest_for_tests();
    }
}

/// Workaround for a confirmed `tauri-build` limitation (BT-3253): on
/// Windows, `tauri_build::build()` compiles a `.res` resource file —
/// including a manifest declaring a dependency on Common Controls v6,
/// needed because `tauri`'s default `common-controls-v6` feature pulls in
/// native Win32 UI code (menus, dialogs, tray icons via `muda`/`wry`) that
/// only resolves against the WinSxS-redirected v6 `comctl32.dll`, not the
/// old in-box one. But `tauri-build` links that resource via
/// `WindowsResource::compile()`, which (through the `embed-resource` crate)
/// emits `cargo:rustc-link-arg-bins=...` — and per Cargo's own documented
/// behavior, the `-bins` suffix scopes a linker arg to this crate's
/// `[[bin]]` target only, never to `[[test]]` targets. A `[[test]]` binary
/// that exercises the same native Win32 code path (as
/// `tests/menu_main_thread.rs` does, calling `menu::build` for real against
/// `tauri::test::mock_app()`) then crashes at process startup with
/// `STATUS_ENTRYPOINT_NOT_FOUND`: the OS loader resolves `comctl32.dll` to
/// the unmanifested old version, which is missing symbols (e.g.
/// `SetWindowSubclass`) that only the v6 DLL exports — a load-time failure,
/// before any test code runs.
///
/// This is not specific to this crate: `tauri` itself hits the identical
/// failure in its own test suite, tracked upstream as
/// <https://github.com/tauri-apps/tauri/issues/13419> and
/// <https://github.com/orgs/tauri-apps/discussions/11179>, and works around
/// it in its own `build.rs` with the exact technique mirrored here — see
/// `embed_manifest_for_tests` at
/// <https://github.com/tauri-apps/tauri/blob/dev/crates/tauri/build.rs>
/// (added for <https://github.com/tauri-apps/tauri/pull/4383>). Rather than
/// another `.res` compile, it invokes the MSVC linker's own
/// manifest-embedding switches directly, scoped with `cargo`'s
/// `-tests`-suffixed directive so this never touches the `[[bin]]` target's
/// already-working, tauri-build-managed manifest:
///
/// - `/MANIFEST:EMBED` — embed a manifest resource in the linked binary.
/// - `/MANIFESTINPUT:<path>` — the manifest content to embed (vendored
///   locally as `windows-app-manifest.xml`, kept identical to tauri-build's
///   own default so the test binary gets the same Common Controls v6
///   declaration as the shipped `[[bin]]`).
/// - `/WX` — fail the link (rather than silently no-op) if `link.exe` can't
///   honor these, so a future toolchain change surfaces here instead of as
///   a mystery Windows-only test crash again.
///
/// MSVC-only (guarded above) because `/MANIFEST:EMBED`/`/MANIFESTINPUT:`
/// are `link.exe` switches, not something `rust-lld`/`gnu-ld` understand;
/// this repo's Windows CI lane uses the default `x86_64-pc-windows-msvc`
/// host toolchain (no `windows-gnu` target is built anywhere), so the
/// `windows-gnu` case is simply out of scope rather than silently broken.
fn embed_manifest_for_tests() {
    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR set by cargo");
    let manifest = std::path::Path::new(&manifest_dir).join("windows-app-manifest.xml");

    println!("cargo:rerun-if-changed={}", manifest.display());
    println!("cargo:rustc-link-arg-tests=/MANIFEST:EMBED");
    println!(
        "cargo:rustc-link-arg-tests=/MANIFESTINPUT:{}",
        manifest.display()
    );
    println!("cargo:rustc-link-arg-tests=/WX");
}
