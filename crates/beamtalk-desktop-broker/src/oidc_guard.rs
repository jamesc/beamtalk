// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Refuse-to-spawn guard for OIDC configuration (ADR 0097 "Local-only posture").
//!
//! The desktop broker is the single-user, localhost-only lane (ADR 0091 §"Local
//! dev stays zero-config"). `config/runtime.exs:32` runs `IdeConfig.load!()` on
//! every non-test boot, so a stray `~/.beamtalk/ide.toml` `[oidc]` table or
//! `BT_OIDC_*` env would make a broker-spawned front enforce OIDC login the
//! broker never accounted for — either crashing at boot (incomplete config,
//! fail-closed) or silently half-enforcing remote auth on what must stay
//! unauthenticated cookie-only. The broker's job is to *check* before
//! spawning and give a friendly error, not to re-implement the enforcement
//! (`BtAttach.IdeConfig` already does that, correctly, inside the front).
//!
//! Mirrors `BtAttach.IdeConfig.requested?/1`
//! (`editors/liveview/lib/bt_attach/ide_config.ex`): OIDC is "requested" when
//! any `BT_OIDC_*` env var is non-empty, or `ide.toml` has a non-empty
//! `[oidc]` table.
//!
//! **DDD Context:** Desktop Shell

use std::path::{Path, PathBuf};

/// The `BT_OIDC_*` env vars `BtAttach.IdeConfig.requested?/1` checks.
const OIDC_ENV_VARS: &[&str] = &[
    "BT_OIDC_ISSUER",
    "BT_OIDC_CLIENT_ID",
    "BT_OIDC_REDIRECT_URI",
    "BT_OIDC_GROUPS_CLAIM",
    "BT_OIDC_CLIENT_SECRET",
];

/// Default `ide.toml` path: `$BT_IDE_CONFIG` if set, else `~/.beamtalk/ide.toml`
/// (mirrors `BtAttach.IdeConfig.default_path/0`).
#[must_use]
pub fn default_ide_config_path() -> PathBuf {
    if let Ok(path) = std::env::var("BT_IDE_CONFIG") {
        if !path.is_empty() {
            return PathBuf::from(path);
        }
    }
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".beamtalk")
        .join("ide.toml")
}

/// Why OIDC was found to be configured, for the error message shown before
/// the broker refuses to spawn.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OidcSource {
    /// One or more `BT_OIDC_*` env vars are set (non-empty).
    EnvVar(&'static str),
    /// `ide.toml` has a non-empty `[oidc]` table.
    IdeToml(PathBuf),
}

impl std::fmt::Display for OidcSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EnvVar(name) => write!(f, "{name} is set"),
            Self::IdeToml(path) => write!(f, "{} has an [oidc] table", path.display()),
        }
    }
}

/// Check env vars only (no file I/O) — used by [`oidc_configured`] and
/// exposed separately so callers/tests don't need a real filesystem.
#[must_use]
pub fn oidc_requested_by_env() -> Option<OidcSource> {
    OIDC_ENV_VARS.iter().find_map(|&key| {
        std::env::var(key)
            .ok()
            .filter(|v| !v.is_empty())
            .map(|_| OidcSource::EnvVar(key))
    })
}

/// Check whether `ide_toml_path` declares a non-empty `[oidc]` table.
///
/// A missing file is not "configured" (matches `BtAttach.IdeConfig`'s
/// 12-factor-friendly "missing file is fine" stance). An unparsable file is
/// treated conservatively as "configured" — refusing to spawn is the safe
/// failure mode, not silently ignoring a file the front itself might still
/// choke on.
#[must_use]
pub fn oidc_requested_by_file(ide_toml_path: &Path) -> Option<OidcSource> {
    let Ok(content) = std::fs::read_to_string(ide_toml_path) else {
        return None;
    };
    let value: toml::Value = match toml::from_str(&content) {
        Ok(v) => v,
        Err(_) => return Some(OidcSource::IdeToml(ide_toml_path.to_path_buf())),
    };
    let non_empty = value
        .get("oidc")
        .is_some_and(|oidc| !matches!(oidc, toml::Value::Table(t) if t.is_empty()));
    non_empty.then(|| OidcSource::IdeToml(ide_toml_path.to_path_buf()))
}

/// Full check: env first (cheaper, matches `BtAttach.IdeConfig`'s "env wins"
/// resolution order), then `ide_toml_path`. Returns `None` when OIDC is not
/// requested — the broker may spawn.
#[must_use]
pub fn oidc_configured(ide_toml_path: &Path) -> Option<OidcSource> {
    oidc_requested_by_env().or_else(|| oidc_requested_by_file(ide_toml_path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::ENV_LOCK;

    fn clear_oidc_env() {
        for key in OIDC_ENV_VARS {
            // SAFETY: single-threaded within the ENV_LOCK-guarded critical
            // section; no other code in this test binary reads/writes these
            // specific env vars concurrently.
            unsafe { std::env::remove_var(key) };
        }
    }

    #[test]
    fn no_oidc_source_when_nothing_configured() {
        let _guard = ENV_LOCK.lock().unwrap();
        clear_oidc_env();
        assert_eq!(oidc_requested_by_env(), None);

        let tmp = tempfile::TempDir::new().unwrap();
        let missing = tmp.path().join("ide.toml");
        assert_eq!(oidc_requested_by_file(&missing), None);
        assert_eq!(oidc_configured(&missing), None);
    }

    #[test]
    fn env_var_present_is_detected() {
        let _guard = ENV_LOCK.lock().unwrap();
        clear_oidc_env();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BT_OIDC_ISSUER", "https://idp.example.com") };
        let found = oidc_requested_by_env();
        assert_eq!(found, Some(OidcSource::EnvVar("BT_OIDC_ISSUER")));
        clear_oidc_env();
    }

    #[test]
    fn empty_env_var_is_not_configured() {
        let _guard = ENV_LOCK.lock().unwrap();
        clear_oidc_env();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BT_OIDC_ISSUER", "") };
        assert_eq!(oidc_requested_by_env(), None);
        clear_oidc_env();
    }

    #[test]
    fn ide_toml_with_oidc_table_is_detected() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(
            &path,
            "[oidc]\nissuer = \"https://idp.example.com\"\nclient_id = \"beamtalk-ide\"\n",
        )
        .unwrap();

        let found = oidc_requested_by_file(&path);
        assert_eq!(found, Some(OidcSource::IdeToml(path)));
    }

    #[test]
    fn ide_toml_with_empty_oidc_table_is_not_configured() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(&path, "[oidc]\n").unwrap();

        assert_eq!(oidc_requested_by_file(&path), None);
    }

    #[test]
    fn ide_toml_without_oidc_table_is_not_configured() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(&path, "[some_other_table]\nkey = \"value\"\n").unwrap();

        assert_eq!(oidc_requested_by_file(&path), None);
    }

    #[test]
    fn unparsable_ide_toml_is_treated_as_configured_fail_closed() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(&path, "this is not [ valid toml").unwrap();

        assert!(
            oidc_requested_by_file(&path).is_some(),
            "malformed config should refuse-to-spawn (fail closed), not silently pass"
        );
    }

    #[test]
    fn oidc_configured_prefers_env_over_file() {
        let _guard = ENV_LOCK.lock().unwrap();
        clear_oidc_env();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BT_OIDC_CLIENT_ID", "beamtalk-ide") };

        let tmp = tempfile::TempDir::new().unwrap();
        let missing = tmp.path().join("ide.toml"); // doesn't even need to exist

        assert_eq!(
            oidc_configured(&missing),
            Some(OidcSource::EnvVar("BT_OIDC_CLIENT_ID"))
        );
        clear_oidc_env();
    }

    #[test]
    fn default_ide_config_path_respects_bt_ide_config_env() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BT_IDE_CONFIG", "/custom/path/ide.toml") };
        assert_eq!(
            default_ide_config_path(),
            PathBuf::from("/custom/path/ide.toml")
        );
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_IDE_CONFIG") };
    }

    #[test]
    fn oidc_source_display_is_human_readable() {
        assert_eq!(
            OidcSource::EnvVar("BT_OIDC_ISSUER").to_string(),
            "BT_OIDC_ISSUER is set"
        );
        let path = PathBuf::from("/home/user/.beamtalk/ide.toml");
        assert!(OidcSource::IdeToml(path).to_string().contains("[oidc]"));
    }
}
