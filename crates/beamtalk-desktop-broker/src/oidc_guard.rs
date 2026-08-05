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
//! ## Parser divergence safety argument (BT-3005)
//!
//! This module parses `ide.toml` with the full `toml` crate (TOML v1.0.0);
//! the front parses the same file with `BtAttach.Toml`
//! (`editors/liveview/lib/bt_attach/toml.ex`), a deliberately tiny hand-rolled
//! reader that only understands: line/trailing comments, `[table]` /
//! `[table.nested]` headers written with literal `.`-separated segments,
//! `key = "string"` pairs, and `key = ["a", "b"]` single-line string arrays.
//! Anything else (dotted-key pairs like `oidc.issuer = "x"`, quoted table
//! headers like `["oidc"]`, inline tables, non-string scalars, multi-line
//! strings/arrays) is rejected outright by `BtAttach.Toml` with a structured
//! `{:unsupported, _, _}` / `{:malformed, _, _}` error, which
//! `BtAttach.IdeConfig.load!/1` turns into a raise — the front refuses to
//! boot rather than silently mis-parse such a file.
//!
//! For every file where `BtAttach.IdeConfig.requested?/1` sees a populated
//! `[oidc]` table, [`oidc_requested_by_file`] here also refuses to spawn —
//! this side never under-refuses relative to the front's own check. That
//! holds for two different reasons depending on the construct:
//!
//!   * If the construct is standard TOML `BtAttach.Toml` doesn't expand
//!     (dotted-key pairs, quoted table-header keys — see the two examples
//!     below), the `toml` crate resolves it to the identical `oidc` table
//!     `BtAttach.Toml` would have needed the long-hand `[oidc]` form to see,
//!     so this side detects "configured" too — often in *more* cases than
//!     the front does, never fewer.
//!   * If the construct is something the `toml` crate rejects as invalid
//!     TOML but `BtAttach.Toml`'s more permissive line-based reader still
//!     accepts (e.g. silently merging a redefined `[oidc]` header, which
//!     standard TOML disallows — confirmed against `toml` 1.1), this side's
//!     unparsable-file fallback treats the parse error as "configured"
//!     anyway (fail-closed, see [`oidc_requested_by_file`]'s doc comment) —
//!     so the outcome is still a refusal, never a silent pass-through.
//!
//! Concretely:
//!
//!   * `oidc.issuer = "x"` at the top level (dotted-key pair) — `BtAttach.Toml`
//!     stores this under the literal key `"oidc.issuer"` (it doesn't expand
//!     dotted pair keys), so `requested?/1` sees no `"oidc"` entry and treats
//!     the front as unrequested. The `toml` crate resolves the identical file
//!     into `{"oidc": {"issuer": "x"}}`, so [`oidc_requested_by_file`] refuses
//!     to spawn anyway — the broker over-refuses here, it never under-refuses.
//!   * `["oidc"]` (a quoted table-header key) — `BtAttach.Toml` treats the
//!     quoted literal (quote characters included) as the table name, so it
//!     never matches the bare key `"oidc"`. The `toml` crate normalizes
//!     quoted and bare table-header keys to the same `oidc` entry, so this
//!     side again sees "configured" in a case `BtAttach.Toml` would miss.
//!
//! Regression tests below (`dotted_key_oidc_is_detected_even_though_bt_attach_toml_misses_it`,
//! `quoted_table_header_oidc_is_detected_even_though_bt_attach_toml_misses_it`,
//! `redefined_oidc_header_is_unparsable_here_and_still_fails_closed`) lock in
//! the divergence examples above: this resolves BT-3005 option (a) — the
//! grammars diverge only in the safe direction, so no behavior change is
//! required, just this documented argument and tests.
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
///
/// Uses the full `toml` crate, not the front's restricted `BtAttach.Toml`
/// reader — see this module's doc comment ("Parser divergence safety
/// argument (BT-3005)") for why that divergence only ever makes this check
/// *more* conservative than the front's, never less.
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

    // BT-3005: `BtAttach.Toml` (the front's hand-rolled reader) does not
    // expand dotted-key pairs — it would store this under the literal key
    // `"oidc.issuer"`, so `BtAttach.IdeConfig.requested?/1` would see no
    // `"oidc"` entry and treat OIDC as unrequested. The full `toml` crate
    // resolves the same file into `{"oidc": {"issuer": ...}}`, so this side
    // must still refuse to spawn — over-refusing relative to the front,
    // never under-refusing. See the module doc comment's safety argument.
    #[test]
    fn dotted_key_oidc_is_detected_even_though_bt_attach_toml_misses_it() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(
            &path,
            "oidc.issuer = \"https://idp.example.com\"\noidc.client_id = \"beamtalk-ide\"\n",
        )
        .unwrap();

        assert_eq!(
            oidc_requested_by_file(&path),
            Some(OidcSource::IdeToml(path)),
            "a dotted-key oidc.* pair is standard TOML for a populated [oidc] \
             table even though BtAttach.Toml's restricted grammar doesn't \
             expand it — this side must still refuse to spawn"
        );
    }

    // BT-3005: `BtAttach.Toml` treats a quoted table-header key (`["oidc"]`)
    // as a literal table name including the quote characters, so it never
    // matches the bare key `"oidc"` and `requested?/1` sees the front as
    // unrequested. The `toml` crate normalizes quoted and bare table-header
    // keys to the same entry, so this side must still detect it.
    #[test]
    fn quoted_table_header_oidc_is_detected_even_though_bt_attach_toml_misses_it() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(&path, "[\"oidc\"]\nissuer = \"https://idp.example.com\"\n").unwrap();

        assert_eq!(
            oidc_requested_by_file(&path),
            Some(OidcSource::IdeToml(path)),
            "a quoted table-header key is standard TOML for [oidc] even \
             though BtAttach.Toml's restricted grammar doesn't normalize it \
             — this side must still refuse to spawn"
        );
    }

    // BT-3005: `BtAttach.Toml`'s permissive line-based reader silently merges
    // a redefined `[oidc]` header (no duplicate-table check), while standard
    // TOML disallows redefining a table and the `toml` crate rejects this as
    // invalid. The rejection routes through the unparsable-file fallback,
    // which is already "configured" (fail-closed) — so even a construct
    // `BtAttach.Toml` accepts but the `toml` crate doesn't still ends in a
    // refusal, never a silent pass-through.
    #[test]
    fn redefined_oidc_header_is_unparsable_here_and_still_fails_closed() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("ide.toml");
        std::fs::write(&path, "[oidc]\nissuer = \"a\"\n[oidc]\nclient_id = \"b\"\n").unwrap();

        assert_eq!(
            oidc_requested_by_file(&path),
            Some(OidcSource::IdeToml(path)),
            "a redefined [oidc] header is invalid TOML the `toml` crate \
             rejects; the unparsable-file fallback must still refuse to spawn"
        );
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
