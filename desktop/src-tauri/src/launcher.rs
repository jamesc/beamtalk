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

use std::ffi::OsString;
use std::path::PathBuf;

use tauri::{AppHandle, Manager, Runtime};

/// Overrides launcher resolution entirely — points at a `bin/server` (or, on
/// Windows, `bin\bt_attach.bat`) built from source for local development,
/// e.g. against a `just dist-liveview` output outside any packaged bundle.
///
/// **Debug builds only** (BT-5194): it picks the program the app executes, so
/// a release build ignores it (and logs that it did) and always runs the
/// launcher bundled inside the signed app.
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

/// Resolve the launcher path: [`LAUNCHER_PATH_OVERRIDE_ENV`] if set in a debug
/// build, else the bundled resource path for the current platform.
///
/// Never fails outright — an unresolvable resource dir falls back to the
/// bare relative path as a last resort, so startup doesn't crash; the
/// resulting path simply won't exist yet on an unpackaged dev build.
/// `beamtalk_desktop_broker::spawn::spawn_front` now checks for that
/// explicitly (BT-3056) and surfaces it as a named
/// `BrokerError::LauncherNotFound`, not a panic and — unlike before that
/// check existed — not an opaque spawn-I/O error either.
#[must_use]
pub fn resolve_launcher_path<R: Runtime>(app: &AppHandle<R>) -> PathBuf {
    if let Some(override_path) = launcher_override(
        std::env::var_os(LAUNCHER_PATH_OVERRIDE_ENV),
        cfg!(debug_assertions),
    ) {
        return override_path;
    }
    app.path().resource_dir().map_or_else(
        |_| PathBuf::from(BUNDLED_LAUNCHER_RELATIVE_PATH),
        |dir| dir.join(BUNDLED_LAUNCHER_RELATIVE_PATH),
    )
}

/// The [`LAUNCHER_PATH_OVERRIDE_ENV`] value to use, if any: a non-empty value,
/// and only when `overrides_allowed` (`cfg!(debug_assertions)` in
/// [`resolve_launcher_path`]).
fn launcher_override(value: Option<OsString>, overrides_allowed: bool) -> Option<PathBuf> {
    let value = value.filter(|v| !v.is_empty())?;
    if overrides_allowed {
        Some(PathBuf::from(value))
    } else {
        tracing::warn!(
            env = LAUNCHER_PATH_OVERRIDE_ENV,
            "ignoring the launcher override: release builds run the bundled launcher only"
        );
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_override_is_honoured_only_when_allowed() {
        let value = || Some(OsString::from("/dev/build/bin/server"));
        assert_eq!(
            launcher_override(value(), true),
            Some(PathBuf::from("/dev/build/bin/server"))
        );
        assert_eq!(launcher_override(value(), false), None);
        assert_eq!(launcher_override(Some(OsString::new()), true), None);
        assert_eq!(launcher_override(None, true), None);
    }

    /// BT-5194: the env var chooses the program the app executes, so a
    /// release build must ignore it. Under `cargo test` (debug) this checks
    /// the override still works; `cargo test --release` checks it is ignored.
    #[test]
    fn resolve_launcher_path_honours_the_override_in_debug_builds_only() {
        let app = tauri::test::mock_app();
        let fake = "/nonexistent/override/bin/server";
        // SAFETY: the only test in this binary that reads or writes this var.
        unsafe { std::env::set_var(LAUNCHER_PATH_OVERRIDE_ENV, fake) };
        let resolved = resolve_launcher_path(app.handle());
        // SAFETY: as above.
        unsafe { std::env::remove_var(LAUNCHER_PATH_OVERRIDE_ENV) };
        assert_eq!(resolved == std::path::Path::new(fake), cfg!(debug_assertions));
    }
}
