// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build artifact provenance stamp (ADR 0098).
//!
//! Records the toolchain that produced a build scope — the `beamtalk_version`
//! (`BEAMTALK_VERSION`, verbatim) and the compound OTP version
//! (`<otp_release>-<erts>`) — so artifacts left over from a *different*
//! toolchain are rebuilt rather than silently reused. mtime cannot detect a
//! toolchain change: mtimes are unreliable across git checkouts, worktrees, and
//! clock skew, and they say nothing about the compiler version that produced the
//! bytes.
//!
//! Provenance is a coarse gate *ahead of* the mtime fast paths: within one
//! toolchain version builds are exactly as incremental as before; across a
//! toolchain change the whole scope rebuilds once and the stamp is rewritten.
//!
//! The stamp lives under `_build/` (gitignored), so a fresh clone or new
//! worktree starts with no stamp → a provenance miss → a clean rebuild.

use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};
use tracing::{debug, warn};

/// Current provenance-stamp schema. Bump when the meaning of the invalidation
/// fields changes; an unrecognised schema is treated as a miss (rebuild), so a
/// newer toolchain's stamp never causes an older binary to reuse foreign bytes.
const STAMP_SCHEMA: u32 = 1;

/// The current toolchain's Beamtalk version: `BEAMTALK_VERSION`, verbatim — e.g.
/// `0.4.0` for a release tag, `0.4.0-dev+<sha>` otherwise. The `-dev+<sha>`
/// suffix is *intentionally* part of the invalidation key (ADR 0098 §2): two
/// compiler commits sharing `0.4.0-dev` must still invalidate each other's
/// artifacts.
pub(crate) fn current_beamtalk_version() -> &'static str {
    env!("BEAMTALK_VERSION")
}

/// On-disk provenance stamp written into a build scope after a successful build.
///
/// Only `beamtalk_version` and `otp_release` are invalidation inputs; `built_at`
/// is diagnostic and never compared.
#[derive(Debug, Serialize, Deserialize)]
struct ProvenanceStamp {
    /// Stamp schema version. An unrecognised value is a provenance miss.
    schema: u32,
    /// Producing `BEAMTALK_VERSION`, verbatim.
    beamtalk_version: String,
    /// Producing compound OTP version (`<otp_release>-<erts>`, e.g. `27-15.0.1`).
    /// `None` if OTP could not be probed when the stamp was written — which a
    /// later build with a known OTP version treats as a miss (fail toward
    /// rebuild).
    otp_release: Option<String>,
    /// ISO-8601 (UTC) build timestamp. Informational only — never an
    /// invalidation input.
    built_at: String,
}

/// Result of checking a build scope's stamp against the current toolchain.
pub(crate) enum StampStatus {
    /// Stamp present and both `beamtalk_version` and OTP version match — reuse
    /// the scope's artifacts (fall through to the mtime fast paths).
    Fresh,
    /// Stamp absent, corrupt, an unknown schema, or a version mismatch — the
    /// scope is stale and must be fully rebuilt. Carries a human-readable reason
    /// for the build log.
    Stale(String),
}

/// Read the stamp at `stamp_path` and decide whether the scope is fresh.
///
/// `current_otp` is the running toolchain's compound OTP version (the same key
/// [`crate::beam_compiler::discover_otp_version`] reports). When it is `None`
/// (OTP could not be probed) the OTP axis is skipped and only `beamtalk_version`
/// is compared — an inability to probe never forces a rebuild on its own.
pub(crate) fn read_stamp_status(stamp_path: &Utf8Path, current_otp: Option<&str>) -> StampStatus {
    let current_version = current_beamtalk_version();

    let Ok(data) = fs::read_to_string(stamp_path) else {
        return StampStatus::Stale("no provenance stamp".to_string());
    };

    let stamp: ProvenanceStamp = match serde_json::from_str(&data) {
        Ok(s) => s,
        Err(_) => return StampStatus::Stale("corrupt provenance stamp".to_string()),
    };

    if stamp.schema != STAMP_SCHEMA {
        return StampStatus::Stale(format!(
            "unrecognised provenance schema {} (current {STAMP_SCHEMA})",
            stamp.schema
        ));
    }

    if stamp.beamtalk_version != current_version {
        return StampStatus::Stale(format!(
            "built by beamtalk {}, current is {current_version}",
            stamp.beamtalk_version
        ));
    }

    // OTP axis: only meaningful when we know the running version. Any
    // disagreement — including a stamp with no OTP version against a known
    // current one — is a miss (fail toward rebuild).
    if let Some(current) = current_otp {
        match stamp.otp_release.as_deref() {
            Some(stamped) if stamped == current => {}
            Some(stamped) => {
                return StampStatus::Stale(format!(
                    "built with OTP {stamped}, current is OTP {current}"
                ));
            }
            None => {
                return StampStatus::Stale(format!(
                    "stamp records no OTP version, current is OTP {current}"
                ));
            }
        }
    }

    StampStatus::Fresh
}

/// Write the provenance stamp for a build scope.
///
/// Called **last**, after every artifact in the scope has landed, via
/// temp-write-then-rename so a crash mid-build never leaves a stamp claiming
/// freshness for incomplete output. Best-effort: an I/O failure is logged but
/// never fails the build (the next build simply sees a missing stamp and
/// rebuilds).
pub(crate) fn write_stamp(stamp_path: &Utf8Path, otp_release: Option<&str>) {
    let stamp = ProvenanceStamp {
        schema: STAMP_SCHEMA,
        beamtalk_version: current_beamtalk_version().to_string(),
        otp_release: otp_release.map(str::to_string),
        built_at: format_rfc3339_utc(SystemTime::now()),
    };

    let data = match serde_json::to_string_pretty(&stamp) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise provenance stamp");
            return;
        }
    };

    match write_atomic(stamp_path, &data) {
        Ok(()) => debug!("Wrote provenance stamp to {stamp_path}"),
        Err(e) => warn!(error = %e, "Failed to write provenance stamp to {stamp_path}"),
    }
}

/// Write `contents` to `path` atomically: stage in a sibling temp file, then
/// rename into place (atomic on the same filesystem). The temp name is keyed by
/// pid so concurrent builders don't clobber each other's staging file; the final
/// rename is the ADR 0098 §1 accepted last-write-wins race.
fn write_atomic(path: &Utf8Path, contents: &str) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(".beamtalk-stamp.{}.tmp", std::process::id()));
    fs::write(&tmp, contents)?;
    fs::rename(&tmp, path)
}

/// Format a `SystemTime` as a UTC RFC 3339 timestamp (`2026-06-23T10:04:11Z`).
/// `built_at` is informational, so second precision is plenty and no timezone
/// crate is needed.
//
// `secs / 86_400` is the post-epoch day count: it stays well within `i64`
// (millions of years), so the `u64 → i64` cast cannot wrap.
#[allow(clippy::cast_possible_wrap)]
fn format_rfc3339_utc(time: SystemTime) -> String {
    let secs = time
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let days = (secs / 86_400) as i64;
    let tod = secs % 86_400;
    let (hour, min, sec) = (tod / 3600, (tod % 3600) / 60, tod % 60);
    let (year, month, day) = civil_from_days(days);
    format!("{year:04}-{month:02}-{day:02}T{hour:02}:{min:02}:{sec:02}Z")
}

/// Convert a count of days since the Unix epoch to a `(year, month, day)` civil
/// date. Howard Hinnant's `civil_from_days` algorithm (proleptic Gregorian).
//
// The intermediate casts are intrinsic to the algorithm and provably in range:
// `z + 719_468 ≥ 0` for any post-epoch day, so `z - era * 146_097 ∈ [0, 146_096]`
// (`u64` is safe); `yoe ∈ [0, 399]` and `era` fit `i64` trivially; and `day`
// (`[1, 31]`) / `month` (`[1, 12]`) fit `u32`. None can wrap, truncate, or lose
// a sign in practice.
#[allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation
)]
fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64; // [0, 146096]
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365; // [0, 399]
    let year = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let day = (doy - (153 * mp + 2) / 5 + 1) as u32; // [1, 31]
    let month = if mp < 10 { mp + 3 } else { mp - 9 } as u32; // [1, 12]
    (if month <= 2 { year + 1 } else { year }, month, day)
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;
    use tempfile::TempDir;

    fn scratch() -> (TempDir, Utf8PathBuf) {
        let temp = TempDir::new().unwrap();
        let path = Utf8PathBuf::from_path_buf(temp.path().join(".beamtalk-stamp.json")).unwrap();
        (temp, path)
    }

    /// Manually write a stamp with arbitrary fields (bypassing `write_stamp` so
    /// tests can forge a different toolchain version).
    fn write_raw(path: &Utf8Path, schema: u32, version: &str, otp: Option<&str>) {
        let stamp = ProvenanceStamp {
            schema,
            beamtalk_version: version.to_string(),
            otp_release: otp.map(str::to_string),
            built_at: "2026-01-01T00:00:00Z".to_string(),
        };
        fs::write(path, serde_json::to_string_pretty(&stamp).unwrap()).unwrap();
    }

    #[test]
    fn write_then_read_is_fresh() {
        let (_t, path) = scratch();
        write_stamp(&path, Some("27-15.0.1"));
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Fresh
        ));
    }

    #[test]
    fn missing_stamp_is_stale() {
        let (_t, path) = scratch();
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn corrupt_stamp_is_stale() {
        let (_t, path) = scratch();
        fs::write(&path, "{ not valid json").unwrap();
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn unknown_schema_is_stale() {
        let (_t, path) = scratch();
        write_raw(&path, 999, current_beamtalk_version(), Some("27-15.0.1"));
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn version_mismatch_is_stale() {
        let (_t, path) = scratch();
        write_raw(&path, STAMP_SCHEMA, "0.0.0-ancient", Some("27-15.0.1"));
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn otp_mismatch_is_stale() {
        let (_t, path) = scratch();
        write_raw(
            &path,
            STAMP_SCHEMA,
            current_beamtalk_version(),
            Some("26-14.0.0"),
        );
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn stamp_without_otp_is_stale_against_known_otp() {
        let (_t, path) = scratch();
        write_raw(&path, STAMP_SCHEMA, current_beamtalk_version(), None);
        assert!(matches!(
            read_stamp_status(&path, Some("27-15.0.1")),
            StampStatus::Stale(_)
        ));
    }

    #[test]
    fn unprobeable_otp_compares_version_only() {
        // current_otp = None: matching version is Fresh even though the stamp
        // records an OTP version we can't currently verify.
        let (_t, path) = scratch();
        write_raw(
            &path,
            STAMP_SCHEMA,
            current_beamtalk_version(),
            Some("27-15.0.1"),
        );
        assert!(matches!(read_stamp_status(&path, None), StampStatus::Fresh));
    }

    #[test]
    fn rfc3339_epoch_is_unix_zero() {
        assert_eq!(format_rfc3339_utc(UNIX_EPOCH), "1970-01-01T00:00:00Z");
    }

    #[test]
    fn rfc3339_known_timestamp() {
        // 2026-06-23T10:04:11Z == 1_782_209_051 seconds since the epoch.
        let t = UNIX_EPOCH + std::time::Duration::from_secs(1_782_209_051);
        assert_eq!(format_rfc3339_utc(t), "2026-06-23T10:04:11Z");
    }
}
