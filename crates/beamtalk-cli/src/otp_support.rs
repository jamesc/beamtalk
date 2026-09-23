// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP version support window (ADR 0125 §3.1) — single declared source.
//!
//! The repo root's `otp-support.toml` is the single source of truth for
//! which Erlang/OTP majors Beamtalk builds and tests on. This module is the
//! **one** reader of that file — everything that needs the window
//! (`beamtalk doctor`, `beamtalk build`/`release`, `just otp-matrix` via the
//! hidden `otp-matrix` subcommand) goes through [`window`] rather than
//! re-declaring the bounds (`docs/development/architecture-principles.md` §
//! Duplication & the Shared-Leaf-Module Pattern).
//!
//! The file is embedded at compile time via `include_str!`, so an installed
//! `beamtalk` binary carries the window with it and does not need the repo
//! checked out at runtime.

use std::sync::OnceLock;

use serde::Deserialize;

/// `otp-support.toml`, embedded at compile time.
const OTP_SUPPORT_TOML: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../otp-support.toml"
));

/// On-disk shape of `otp-support.toml`.
#[derive(Debug, Deserialize)]
struct RawWindow {
    #[serde(rename = "min-major")]
    min_major: u32,
    #[serde(rename = "max-major")]
    max_major: u32,
}

/// The declared OTP support window: `min_major..=max_major`, inclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OtpSupportWindow {
    /// Lowest supported OTP major (inclusive).
    pub min_major: u32,
    /// Highest supported OTP major (inclusive).
    pub max_major: u32,
}

impl OtpSupportWindow {
    /// Whether `major` falls inside the declared window (inclusive).
    #[must_use]
    pub fn contains(&self, major: u32) -> bool {
        (self.min_major..=self.max_major).contains(&major)
    }

    /// Every major in the window, ascending (e.g. `[27, 28]`).
    #[must_use]
    pub fn majors(&self) -> Vec<u32> {
        (self.min_major..=self.max_major).collect()
    }

    /// The window rendered as a human-readable range, e.g. `"27-28"`.
    #[must_use]
    pub fn display_range(&self) -> String {
        if self.min_major == self.max_major {
            self.min_major.to_string()
        } else {
            format!("{}-{}", self.min_major, self.max_major)
        }
    }
}

/// The declared OTP support window, parsed once and cached.
///
/// # Panics
///
/// Panics if `otp-support.toml` fails to parse or declares `min-major >
/// max-major` — both indicate a corrupt checked-in repo file, not a runtime
/// condition callers can recover from.
#[must_use]
pub fn window() -> OtpSupportWindow {
    static WINDOW: OnceLock<OtpSupportWindow> = OnceLock::new();
    *WINDOW.get_or_init(|| {
        let raw: RawWindow = toml::from_str(OTP_SUPPORT_TOML)
            .unwrap_or_else(|e| panic!("otp-support.toml is malformed: {e}"));
        assert!(
            raw.min_major <= raw.max_major,
            "otp-support.toml: min-major ({}) must be <= max-major ({})",
            raw.min_major,
            raw.max_major
        );
        OtpSupportWindow {
            min_major: raw.min_major,
            max_major: raw.max_major,
        }
    })
}

/// Parse the leading OTP major version from a version string.
///
/// Handles every shape this codebase probes OTP with: a bare major
/// (`"27"`), a dotted release (`"27.2"`, from `beamtalk doctor`'s
/// `erlang:system_info(otp_release)` probe), and the compound
/// `<otp_release>-<erts_version>` form (`"27-15.0.1"`, from
/// [`crate::commands::build_stamp::current_otp_version`] /
/// `beamtalk_core::ffi_type_specs::discover_otp_version`). Stops at the
/// first non-digit character, so all three collapse to the same major.
#[must_use]
pub fn major_from_version_str(version: &str) -> Option<u32> {
    let digits: String = version.chars().take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        None
    } else {
        digits.parse().ok()
    }
}

/// JSON array of the window's majors as strings, e.g. `["27","28"]` — the
/// exact shape `just otp-matrix` prints for `ci.yml`'s `matrix.otp`.
#[must_use]
pub fn majors_as_json_array() -> String {
    let majors = window().majors();
    let quoted: Vec<String> = majors.iter().map(|m| format!("\"{m}\"")).collect();
    format!("[{}]", quoted.join(","))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn window_reads_declared_bounds() {
        let w = window();
        assert!(w.min_major <= w.max_major);
        assert_eq!(w.min_major, 27, "ADR 0125 §3.1: minimum is 27");
    }

    #[test]
    fn contains_checks_inclusive_range() {
        let w = OtpSupportWindow {
            min_major: 27,
            max_major: 28,
        };
        assert!(!w.contains(26));
        assert!(w.contains(27));
        assert!(w.contains(28));
        assert!(!w.contains(29));
    }

    #[test]
    fn majors_lists_every_major_in_range() {
        let w = OtpSupportWindow {
            min_major: 27,
            max_major: 28,
        };
        assert_eq!(w.majors(), vec![27, 28]);
    }

    #[test]
    fn display_range_formats_multi_major_window() {
        let w = OtpSupportWindow {
            min_major: 27,
            max_major: 28,
        };
        assert_eq!(w.display_range(), "27-28");
    }

    #[test]
    fn display_range_formats_single_major_window() {
        let w = OtpSupportWindow {
            min_major: 28,
            max_major: 28,
        };
        assert_eq!(w.display_range(), "28");
    }

    #[test]
    fn major_from_version_str_bare_major() {
        assert_eq!(major_from_version_str("27"), Some(27));
    }

    #[test]
    fn major_from_version_str_dotted_release() {
        assert_eq!(major_from_version_str("27.2"), Some(27));
    }

    #[test]
    fn major_from_version_str_compound_otp_erts() {
        assert_eq!(major_from_version_str("27-15.0.1"), Some(27));
    }

    #[test]
    fn major_from_version_str_empty_is_none() {
        assert_eq!(major_from_version_str(""), None);
    }

    #[test]
    fn major_from_version_str_garbage_is_none() {
        assert_eq!(major_from_version_str("abc"), None);
    }

    #[test]
    fn majors_as_json_array_matches_window() {
        let w = window();
        let expected = format!(
            "[{}]",
            w.majors()
                .iter()
                .map(|m| format!("\"{m}\""))
                .collect::<Vec<_>>()
                .join(",")
        );
        assert_eq!(majors_as_json_array(), expected);
    }
}
