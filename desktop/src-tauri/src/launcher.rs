// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Resolve the path to the bundled `bt_attach` release's `bin/server`
//! launcher (ADR 0097 Decision: `PORT=<free-port> bin/server <id>`).
//!
//! **Packaging is BT-2987/BT-2988's job, not this issue's** (ADR 0097
//! Implementation Tracking) — bundling the actual `dist-liveview` release
//! into the app's resources is out of scope here. This module resolves
//! *where* that bundle will eventually live, with an env override for local
//! development against a from-source build (`just dist-liveview` under
//! `editors/liveview/`) in the meantime.

use std::path::PathBuf;

use tauri::{AppHandle, Manager};

/// Overrides launcher resolution entirely — points at a `bin/server` built
/// from source for local development, since no packaged bundle exists until
/// BT-2987 lands.
pub const LAUNCHER_PATH_OVERRIDE_ENV: &str = "BEAMTALK_ATTACH_LAUNCHER";

/// Where the bundled release's launcher will live once BT-2987 packages it
/// into the app's resources, relative to the resource directory.
const BUNDLED_LAUNCHER_RELATIVE_PATH: &str = "dist-liveview/bin/server";

/// Resolve the `bin/server` launcher path: [`LAUNCHER_PATH_OVERRIDE_ENV`] if
/// set, else the (not yet populated until BT-2987) bundled resource path.
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
