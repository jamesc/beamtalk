// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Resolve the path to the bundled `bt_attach` release's launcher — `bin/server`
//! on Unix (ADR 0097 Decision: `PORT=<free-port> bin/server <id>`), or on
//! Windows (BT-2988) `bin\bt_attach.bat`, the release's own generated entry
//! point, since there is no `bin/server` shell script there —
//! `beamtalk_desktop_broker::spawn`'s `build_launch_command` sets every env
//! var `bin/server` would have and invokes it as `bin\bt_attach.bat start`.
//!
//! **Packaging is BT-2987/BT-2988's job, not this issue's** (ADR 0097
//! Implementation Tracking) — bundling the actual `dist-liveview` release
//! into the app's resources is out of scope here. This module resolves
//! *where* that bundle lives, with an env override for local development
//! against a from-source build (`just dist-liveview` under
//! `editors/liveview/`) in the meantime.

use std::path::PathBuf;

use tauri::{AppHandle, Manager};

/// Overrides launcher resolution entirely — points at a `bin/server` (or, on
/// Windows, `bin\bt_attach.bat`) built from source for local development,
/// e.g. against a `just dist-liveview` output outside any packaged bundle.
pub const LAUNCHER_PATH_OVERRIDE_ENV: &str = "BEAMTALK_ATTACH_LAUNCHER";

/// Where the bundled release's launcher lives, relative to the resource
/// directory — `bin/server` on Unix (BT-2987), `bin\bt_attach.bat` on
/// Windows (BT-2988): there is no Unix-style `bin/server` shell script in a
/// Windows `mix release`, so the broker invokes the release's own generated
/// entry point directly and sets every env var `bin/server` would have
/// itself (see `beamtalk_desktop_broker::spawn`'s doc comment).
#[cfg(unix)]
const BUNDLED_LAUNCHER_RELATIVE_PATH: &str = "dist-liveview/bin/server";

/// Windows counterpart of the Unix path above — see its doc comment.
#[cfg(windows)]
const BUNDLED_LAUNCHER_RELATIVE_PATH: &str = "dist-liveview/bin/bt_attach.bat";

/// Resolve the launcher path: [`LAUNCHER_PATH_OVERRIDE_ENV`] if set, else the
/// bundled resource path for the current platform.
///
/// Never fails outright — an unresolvable resource dir falls back to the
/// bare relative path as a last resort, so startup doesn't crash; the
/// resulting path simply won't exist yet on an unpackaged dev build, and
/// `attach` surfaces that as a normal spawn-I/O error rather than a panic.
#[must_use]
pub fn resolve_launcher_path(app: &AppHandle) -> PathBuf {
    if let Ok(override_path) = std::env::var(LAUNCHER_PATH_OVERRIDE_ENV) {
        if !override_path.is_empty() {
            return PathBuf::from(override_path);
        }
    }
    app.path()
        .resource_dir()
        .map(|dir| dir.join(BUNDLED_LAUNCHER_RELATIVE_PATH))
        .unwrap_or_else(|_| PathBuf::from(BUNDLED_LAUNCHER_RELATIVE_PATH))
}
