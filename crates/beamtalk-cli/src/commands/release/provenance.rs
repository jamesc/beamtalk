// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `releases/<vsn>/beamtalk-provenance.json` (ADR 0125 §1.8).
//!
//! Pure Rust — every value it writes is already known to the CLI process
//! (the app closure, `BEAMTALK_VERSION`, the probed ERTS/platform, the
//! compound OTP version `build_stamp::current_otp_version()` already
//! computes) once staging/assembly has run, so this needs no `erl -eval`
//! round trip the way `.rel`/`start.boot`/`shapes.json` do.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use serde::Serialize;

use super::closure::AppClosure;

/// One `apps` entry in `beamtalk-provenance.json`.
#[derive(Debug, Serialize)]
struct ProvenanceApp {
    name: String,
    vsn: String,
}

/// The OTP major-version range this artifact's BEAM files are guaranteed to
/// load on (ADR 0125 §1.8/§3.2): `{min: build_major, max: build_major + 2}`,
/// per OTP's own two-subsequent-releases compatibility guarantee.
/// Deliberately **not** intersected with the §3.1 support window — see the
/// ADR section this struct cites for why the two answer different
/// questions.
#[derive(Debug, Serialize)]
struct RequiredOtp {
    min: u32,
    max: u32,
}

#[derive(Debug, Serialize)]
struct Provenance {
    schema: u32,
    release: String,
    release_version: String,
    beamtalk_version: String,
    otp_release: String,
    required_otp: RequiredOtp,
    include_erts: bool,
    erts_version: String,
    platform: String,
    built_at: String,
    apps: Vec<ProvenanceApp>,
}

/// Write `releases/<vsn>/beamtalk-provenance.json` (ADR 0125 §1.8).
///
/// `otp_release` must be `build_stamp::current_otp_version()`'s value —
/// never re-derived from `erlang:system_info(otp_release)`, which would
/// bake a bare major with no ERTS component (the acceptance criterion this
/// function exists to satisfy literally, and the reason it takes the
/// already-probed string as a parameter rather than probing again itself).
///
/// # Errors
///
/// Returns an error if `otp_release` is not a valid `<major>-<erts>`
/// compound string (see [`super::super::build_stamp::otp_build_major`]), or
/// if the file cannot be written.
#[allow(clippy::too_many_arguments)]
pub fn write_provenance_json(
    release_config_dir: &Utf8Path,
    release_name: &str,
    release_vsn: &str,
    beamtalk_version: &str,
    otp_release: &str,
    include_erts: bool,
    erts_version: &str,
    platform: &str,
    closure: &AppClosure,
) -> Result<Utf8PathBuf> {
    let build_major = super::super::build_stamp::otp_build_major(otp_release).ok_or_else(|| {
        miette::miette!(
            "Could not parse an OTP build major out of otp_release '{otp_release}' — expected \
             '<major>-<erts>' (build_stamp::current_otp_version()'s own shape)."
        )
    })?;

    let apps = closure
        .staged_apps
        .iter()
        .map(|a| ProvenanceApp {
            name: a.name.clone(),
            vsn: a.vsn.clone(),
        })
        .collect();

    let doc = Provenance {
        schema: 1,
        release: release_name.to_string(),
        release_version: release_vsn.to_string(),
        beamtalk_version: beamtalk_version.to_string(),
        otp_release: otp_release.to_string(),
        required_otp: RequiredOtp {
            min: build_major,
            max: build_major + 2,
        },
        include_erts,
        erts_version: erts_version.to_string(),
        platform: platform.to_string(),
        built_at: super::super::build_stamp::format_rfc3339_utc(std::time::SystemTime::now()),
        apps,
    };

    let json = serde_json::to_string_pretty(&doc)
        .into_diagnostic()
        .wrap_err("Failed to serialise beamtalk-provenance.json")?;
    let path = release_config_dir.join("beamtalk-provenance.json");
    std::fs::write(path.as_std_path(), json)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{path}'"))?;
    Ok(path)
}

/// The platform triple `beamtalk-provenance.json`'s `platform` field and
/// the command's own printed output use (ADR 0125 §1.3: `"linux/x86_64"`) —
/// `std::env::consts::OS`/`ARCH`, which already spell the two ADR names
/// (`"linux"`, `"x86_64"`) verbatim.
pub fn current_platform() -> String {
    format!("{}/{}", std::env::consts::OS, std::env::consts::ARCH)
}

#[cfg(test)]
mod tests {
    use super::super::closure::StagedApp;
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn closure() -> AppClosure {
        AppClosure {
            host_apps: vec!["kernel".to_string()],
            staged_apps: vec![StagedApp {
                name: "orders".to_string(),
                vsn: "1.4.0".to_string(),
                source_ebins: vec![],
                declared_deps: vec![],
            }],
        }
    }

    #[test]
    fn write_provenance_json_writes_expected_shape() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = write_provenance_json(
            &root,
            "orders",
            "1.4.0",
            "0.4.0",
            "28-16.0.2",
            true,
            "16.0.2",
            "linux/x86_64",
            &closure(),
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
        assert_eq!(parsed["schema"], 1);
        assert_eq!(parsed["release"], "orders");
        assert_eq!(parsed["release_version"], "1.4.0");
        assert_eq!(parsed["beamtalk_version"], "0.4.0");
        assert_eq!(parsed["otp_release"], "28-16.0.2");
        assert_eq!(parsed["required_otp"]["min"], 28);
        assert_eq!(parsed["required_otp"]["max"], 30);
        assert_eq!(parsed["include_erts"], true);
        assert_eq!(parsed["erts_version"], "16.0.2");
        assert_eq!(parsed["platform"], "linux/x86_64");
        assert_eq!(parsed["apps"][0]["name"], "orders");
        assert_eq!(parsed["apps"][0]["vsn"], "1.4.0");
    }

    #[test]
    fn write_provenance_json_required_otp_is_not_intersected_with_support_window() {
        // ADR 0125 §3.2: required_otp is build_major..+2, full stop — never
        // clamped against any separately-declared support window.
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = write_provenance_json(
            &root,
            "orders",
            "1.4.0",
            "0.4.0",
            "27-15.0.1",
            false,
            "15.0.1",
            "linux/x86_64",
            &closure(),
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
        assert_eq!(parsed["required_otp"]["min"], 27);
        assert_eq!(parsed["required_otp"]["max"], 29);
    }

    #[test]
    fn write_provenance_json_rejects_unparseable_otp_release() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let err = write_provenance_json(
            &root,
            "orders",
            "1.4.0",
            "0.4.0",
            "not-a-compound-version",
            true,
            "16.0.2",
            "linux/x86_64",
            &closure(),
        )
        .unwrap_err();
        assert!(err.to_string().contains("otp_release"), "{err}");
    }

    #[test]
    fn current_platform_matches_os_arch() {
        let platform = current_platform();
        assert!(platform.contains('/'), "{platform}");
        assert!(platform.contains(std::env::consts::OS), "{platform}");
        assert!(platform.contains(std::env::consts::ARCH), "{platform}");
    }
}
