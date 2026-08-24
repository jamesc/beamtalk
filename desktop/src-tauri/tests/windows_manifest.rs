// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3253 review follow-up: `windows-app-manifest.xml` is a vendored copy
//! of `tauri-build`'s own default Windows manifest, asserted (in both
//! `build.rs`'s and this file's own doc comments) to declare the same
//! Common Controls v6 dependency the production `[[bin]]` target gets from
//! `tauri_build::build()`. That's a "keep in sync" claim with nothing
//! enforcing it — a future `tauri-build` version bump could change its
//! default manifest without this vendored copy noticing. Runs on every
//! platform (it's a static content check, not the Windows-only linker
//! embedding itself) so a silent desync is caught by ordinary `cargo test`,
//! not just a Windows CI run.

#[test]
fn vendored_manifest_declares_common_controls_v6() {
    let manifest = include_str!("../windows-app-manifest.xml");

    assert!(
        manifest.contains("Microsoft.Windows.Common-Controls"),
        "vendored windows-app-manifest.xml no longer declares a dependency on \
         Microsoft.Windows.Common-Controls — re-diff against tauri-build's own \
         default manifest (tauri-build-<version>/src/windows-app-manifest.xml) \
         after any tauri-build version bump; see build.rs's embed_manifest_for_tests \
         doc comment for why this file must match it"
    );
    assert!(
        manifest.contains(r#"version="6.0.0.0""#),
        "vendored windows-app-manifest.xml's Common-Controls dependency no longer \
         pins version 6.0.0.0 — that's the specific version whose WinSxS-redirected \
         comctl32.dll exports the symbols (e.g. SetWindowSubclass) this fix exists \
         to make available to [[test]] targets"
    );
}
