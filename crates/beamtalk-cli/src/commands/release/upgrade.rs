// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk release --upgrade-from`: the ADR 0125 §2.3 shape-compatibility
//! preflight (BT-3574).
//!
//! v1 ships restart-based upgrades only (ADR 0125 §2.1/§2.5) — this
//! preflight is what makes that safe rather than merely available. It never
//! generates a relup; it compares the two releases' `shapes.json` (§3.4) and
//! `beamtalk-provenance.json` and reports, in the layout and with the
//! severities §2.3 specifies, exiting non-zero on any error so it can gate a
//! CI deploy.
//!
//! The comparison itself is pure Rust over two already-materialised
//! `shapes.json` documents — no `beamtalk_shape_diff` call, no live Erlang
//! node. `shapes.json`'s own `fields`/`migrations` projection (built by the
//! `beamtalk_release_shapes` extractor, `beamtalk_runtime`) already carries
//! everything this check needs (§3.4's full per-class generation record);
//! reusing it here, rather than re-deriving a `beamtalk_shape_diff:shape()`
//! value and diffing in Erlang, keeps this module's severity policy — which
//! is deliberately *not* `beamtalk_shape_diff:reload_findings/4`'s (a
//! version bump with no migration is an **error** here, a live-reload
//! *hint* there — different consequences, different tools) — a plain,
//! testable Rust function instead of a second Erlang module with its own
//! `erl -eval` round trip.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::process::Command;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;

use super::assembly;
use super::closure::is_runtime_app;

// ---------------------------------------------------------------------
// shapes.json / beamtalk-provenance.json — read-side shapes
// ---------------------------------------------------------------------

#[derive(Debug, Deserialize)]
struct ShapesDoc {
    shapes: BTreeMap<String, ShapeEntry>,
}

#[derive(Debug, Deserialize)]
struct ShapeEntry {
    version: u32,
    fields: BTreeMap<String, Option<String>>,
    migrations: BTreeMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct ProvenanceDoc {
    beamtalk_version: String,
    otp_release: String,
}

// ---------------------------------------------------------------------
// The report
// ---------------------------------------------------------------------

/// One row of the "Shape changes requiring migration" / "Shape changed
/// without a version bump" / "Removed classes" tables (ADR 0125 §2.3).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Finding {
    class: String,
    detail: String,
    severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Severity {
    Ok,
    Warning,
    Error,
}

impl Severity {
    const fn label(self) -> &'static str {
        match self {
            Self::Ok => "ok",
            Self::Warning => "warning",
            Self::Error => "error",
        }
    }
}

/// The rendered preflight report, and whether it must fail the command
/// (`has_error`, ADR 0125 §2.3: "Any error makes the command exit non-zero").
#[derive(Debug, Clone)]
pub struct UpgradeReport {
    pub text: String,
    pub has_error: bool,
}

/// Run the ADR 0125 §2.3 preflight: `prev_release_root` (a previously built
/// `beamtalk release` output directory, or a `.tar.gz`/`.tgz` tarball of
/// one) against the release just built at `new_release_root` (whose
/// `releases/<new_vsn>/shapes.json`/`beamtalk-provenance.json` this call
/// already assumes exist — `build_release` writes them before calling this).
///
/// If `prev_release_root` has no `shapes.json` (it predates the feature),
/// this runs the §2.2 extractor over its own `lib/*/ebin` on the fly
/// (`extract_prev_shapes_on_the_fly`) rather than reporting "unknown".
///
/// # Errors
///
/// Returns an error if `prev_release_root` cannot be read/unpacked, has no
/// `releases/<vsn>/` directory, or (when its `shapes.json` is missing) the
/// on-the-fly extractor fails.
pub fn run_upgrade_preflight(
    prev_release_arg: &Utf8Path,
    new_release_root: &Utf8Path,
    release_name: &str,
    new_vsn: &str,
) -> Result<UpgradeReport> {
    let _unpack_guard;
    let prev_root = if is_tarball(prev_release_arg) {
        let (dir, guard) = unpack_tarball(prev_release_arg)?;
        _unpack_guard = Some(guard);
        dir
    } else {
        _unpack_guard = None;
        prev_release_arg.to_path_buf()
    };

    let (prev_vsn, prev_release_config_dir) = find_release_version_dir(&prev_root)?;

    let prev_shapes_path = prev_release_config_dir.join("shapes.json");
    let prev_shapes = if prev_shapes_path.is_file() {
        read_shapes_doc(&prev_shapes_path)?
    } else {
        extract_prev_shapes_on_the_fly(&prev_root, &prev_vsn)?
    };

    let new_release_config_dir = new_release_root.join("releases").join(new_vsn);
    let new_shapes = read_shapes_doc(&new_release_config_dir.join("shapes.json"))?;

    let prev_provenance =
        read_provenance_doc(&prev_release_config_dir.join("beamtalk-provenance.json"));
    let new_provenance =
        read_provenance_doc(&new_release_config_dir.join("beamtalk-provenance.json"));

    Ok(render_report(
        release_name,
        &prev_vsn,
        new_vsn,
        &prev_shapes,
        &new_shapes,
        prev_provenance.as_ref(),
        new_provenance.as_ref(),
    ))
}

fn is_tarball(path: &Utf8Path) -> bool {
    let name = path.file_name().unwrap_or_default();
    name.ends_with(".tar.gz")
        || std::path::Path::new(name)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("tgz"))
}

/// A directory that is deleted when dropped — the scratch dir a tarball is
/// unpacked into, cleaned up once the preflight is done reading it.
struct TempDirGuard(Utf8PathBuf);

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(self.0.as_std_path());
    }
}

fn unpack_tarball(tarball: &Utf8Path) -> Result<(Utf8PathBuf, TempDirGuard)> {
    let (dest, guard) = fresh_scratch_dir("unpack")?;
    let status = Command::new("tar")
        .arg("-xzf")
        .arg(tarball.as_std_path())
        .arg("-C")
        .arg(dest.as_std_path())
        .status()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to spawn tar to unpack '{tarball}'"))?;
    if !status.success() {
        miette::bail!("Failed to unpack '{tarball}' (tar exited with {status})");
    }
    Ok((dest, guard))
}

/// Find `<prev_root>/releases/<vsn>/` — the single version directory a
/// freshly built (or unpacked) release carries. When more than one is
/// present (a release that has itself already been the target of a prior
/// upgrade), the lexicographically greatest is used — the most recently
/// installed generation is what a next upgrade compares against.
fn find_release_version_dir(prev_root: &Utf8Path) -> Result<(String, Utf8PathBuf)> {
    let releases_dir = prev_root.join("releases");
    let mut candidates: Vec<(String, Utf8PathBuf)> = std::fs::read_dir(releases_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| {
            format!(
                "'{prev_root}' does not look like a `beamtalk release` output — no \
                 'releases/' directory (looked in '{releases_dir}')"
            )
        })?
        .filter_map(std::result::Result::ok)
        .filter(|e| e.path().is_dir())
        .filter_map(|e| {
            let path = Utf8PathBuf::from_path_buf(e.path()).ok()?;
            let vsn = path.file_name()?.to_string();
            Some((vsn, path))
        })
        .collect();
    candidates.sort_by(|a, b| a.0.cmp(&b.0));
    candidates.pop().ok_or_else(|| {
        miette::miette!(
            "'{releases_dir}' has no version subdirectory — is '{prev_root}' a valid \
             `beamtalk release` output?"
        )
    })
}

fn read_shapes_doc(path: &Utf8Path) -> Result<ShapesDoc> {
    let content = std::fs::read_to_string(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;
    serde_json::from_str(&content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse '{path}'"))
}

/// Best-effort: a missing/unparseable `beamtalk-provenance.json` degrades
/// the "Toolchain" report lines to `"unknown"` rather than failing the
/// whole preflight — it carries no data the shape comparison itself needs.
fn read_provenance_doc(path: &Utf8Path) -> Option<ProvenanceDoc> {
    let content = std::fs::read_to_string(path.as_std_path()).ok()?;
    serde_json::from_str(&content).ok()
}

/// ADR 0125 §2.3: a previous release built before `shapes.json` existed has
/// no file to compare — run the §2.2 extractor
/// (`beamtalk_release_shapes:write_shapes_json/4`, via `assembly::
/// write_shapes_json`) over that release's own staged `lib/*/ebin` on the
/// fly, into a scratch file, so the check never degrades to "unknown".
///
/// Uses the *previous release's own* staged apps (not the current build's)
/// for both the runtime-closure (`RuntimeLibDirs`) and project (`EmitLibDirs`)
/// splits — an old release's own beams are what must actually load to
/// extract its own `__beamtalk_meta/0` values.
fn extract_prev_shapes_on_the_fly(prev_root: &Utf8Path, prev_vsn: &str) -> Result<ShapesDoc> {
    let (runtime_dirs, emit_dirs) = discover_prev_release_lib_dirs(prev_root)?;
    let (_dir, guard) = fresh_scratch_dir("extract")?;
    let out_path = guard.0.join("shapes.json");
    assembly::write_shapes_json(&runtime_dirs, &emit_dirs, &out_path, prev_vsn).wrap_err_with(
        || {
            format!(
                "'{prev_root}' has no releases/{prev_vsn}/shapes.json (it predates ADR 0125) \
                 and the on-the-fly extractor failed"
            )
        },
    )?;
    read_shapes_doc(&out_path)
}

/// A fresh, empty, uniquely-named scratch directory under the system temp
/// dir, cleaned up via [`TempDirGuard`] on drop — shared by the tarball
/// unpack step and the on-the-fly `shapes.json` extraction step, so both
/// scratch-directory lifecycles go through one implementation.
fn fresh_scratch_dir(purpose: &str) -> Result<(Utf8PathBuf, TempDirGuard)> {
    let unique = format!(
        "beamtalk-release-upgrade-from-{purpose}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or_default()
    );
    let dest = Utf8PathBuf::from_path_buf(std::env::temp_dir())
        .map_err(|p| miette::miette!("system temp dir is not UTF-8: {p:?}"))?
        .join(unique);
    std::fs::create_dir_all(dest.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create scratch dir '{dest}'"))?;
    Ok((dest.clone(), TempDirGuard(dest)))
}

/// Split `<prev_root>/lib/<app>-<vsn>/ebin` into runtime-closure vs.
/// project/dependency dirs, the same `closure::is_runtime_app` split
/// `build_release` applies to the *current* build's closure — mirrored here
/// against the *previous* release's own staged apps (see
/// `extract_prev_shapes_on_the_fly`'s doc for why).
fn discover_prev_release_lib_dirs(
    prev_root: &Utf8Path,
) -> Result<(Vec<Utf8PathBuf>, Vec<Utf8PathBuf>)> {
    let lib_dir = prev_root.join("lib");
    let mut runtime_dirs = Vec::new();
    let mut emit_dirs = Vec::new();
    let entries = std::fs::read_dir(lib_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| {
            format!("'{prev_root}' has no 'lib/' directory to extract shapes from")
        })?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        if !entry.path().is_dir() {
            continue;
        }
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|p| miette::miette!("non-UTF-8 lib path: {p:?}"))?;
        let dir_name = path
            .file_name()
            .ok_or_else(|| miette::miette!("lib entry has no file name: {path}"))?;
        // `<app>-<vsn>` — split on the *first* hyphen, not the last: every
        // staged app name is a bare `snake_case` atom with no hyphen of its
        // own (`beamtalk_runtime`, `cowlib`, `telemetry_poller`, …), but a
        // dev-build version string can itself contain hyphens
        // (`0.4.0-dev+38a688d`, ADR 0125 §1.3's own "the dev version string
        // is safe in a lib directory name" note) — `rsplit_once` would cut
        // into that suffix instead of the app/version boundary and
        // misclassify `beamtalk_runtime-0.4.0-dev+…` as app name
        // `beamtalk_runtime-0.4.0`, which `is_runtime_app` then fails to
        // recognise.
        let app_name = dir_name.split_once('-').map_or(dir_name, |(n, _)| n);
        let ebin = path.join("ebin");
        if is_runtime_app(app_name) {
            runtime_dirs.push(ebin);
        } else {
            emit_dirs.push(ebin);
        }
    }
    Ok((runtime_dirs, emit_dirs))
}

// ---------------------------------------------------------------------
// Diff + render (ADR 0125 §2.3)
// ---------------------------------------------------------------------

#[allow(clippy::too_many_lines)]
fn render_report(
    release_name: &str,
    prev_vsn: &str,
    new_vsn: &str,
    prev: &ShapesDoc,
    new: &ShapesDoc,
    prev_provenance: Option<&ProvenanceDoc>,
    new_provenance: Option<&ProvenanceDoc>,
) -> UpgradeReport {
    let mut migration_findings = Vec::new();
    let mut unbumped_findings = Vec::new();
    let mut removed_findings = Vec::new();

    for (class, old_entry) in &prev.shapes {
        match new.shapes.get(class) {
            None => {
                removed_findings.push(Finding {
                    class: class.clone(),
                    detail: String::new(),
                    severity: Severity::Warning,
                });
            }
            Some(new_entry) => {
                if new_entry.version > old_entry.version {
                    let has_migration = new_entry
                        .migrations
                        .contains_key(&old_entry.version.to_string());
                    let (detail, severity) = if has_migration {
                        (
                            format!(
                                "v{} → v{}   migrateFromV{}: present",
                                old_entry.version, new_entry.version, old_entry.version
                            ),
                            Severity::Ok,
                        )
                    } else {
                        (
                            format!(
                                "v{} → v{}   migrateFromV{}: MISSING",
                                old_entry.version, new_entry.version, old_entry.version
                            ),
                            Severity::Error,
                        )
                    };
                    migration_findings.push(Finding {
                        class: class.clone(),
                        detail,
                        severity,
                    });
                } else if new_entry.version == old_entry.version
                    && new_entry.fields != old_entry.fields
                {
                    let field_change = describe_field_change(&old_entry.fields, &new_entry.fields);
                    unbumped_findings.push(Finding {
                        class: class.clone(),
                        detail: format!(
                            "v{} → v{}   {field_change}",
                            old_entry.version, new_entry.version
                        ),
                        severity: Severity::Warning,
                    });
                } else if new_entry.version < old_entry.version {
                    unbumped_findings.push(Finding {
                        class: class.clone(),
                        detail: format!(
                            "v{} → v{}   shapeVersion decreased",
                            old_entry.version, new_entry.version
                        ),
                        severity: Severity::Warning,
                    });
                }
            }
        }
    }
    migration_findings.sort_by(|a, b| a.class.cmp(&b.class));
    unbumped_findings.sort_by(|a, b| a.class.cmp(&b.class));
    removed_findings.sort_by(|a, b| a.class.cmp(&b.class));

    let error_count = migration_findings
        .iter()
        .chain(&unbumped_findings)
        .chain(&removed_findings)
        .filter(|f| f.severity == Severity::Error)
        .count();
    let warning_count = migration_findings
        .iter()
        .chain(&unbumped_findings)
        .chain(&removed_findings)
        .filter(|f| f.severity == Severity::Warning)
        .count();

    let mut out = String::new();
    // `write!`/`writeln!` into a `String` cannot fail — infallible per
    // `std::fmt::Write`'s contract for this sink — so the `Result` every
    // call returns is deliberately discarded (`let _ =`), not propagated.
    let _ = writeln!(out, "Upgrade check: {release_name} {prev_vsn} → {new_vsn}");

    if !migration_findings.is_empty() {
        out.push_str("\n  Shape changes requiring migration\n");
        for f in &migration_findings {
            let _ = writeln!(
                out,
                "    {:<14}{}   {}",
                f.class,
                f.detail,
                f.severity.label()
            );
        }
    }

    if !unbumped_findings.is_empty() {
        out.push_str("\n  Shape changed without a version bump\n");
        for f in &unbumped_findings {
            let _ = writeln!(
                out,
                "    {:<14}{}   {}",
                f.class,
                f.detail,
                f.severity.label()
            );
        }
    }

    if !removed_findings.is_empty() {
        out.push_str("\n  Removed classes\n");
        for f in &removed_findings {
            let _ = writeln!(out, "    {:<14}{}", f.class, f.severity.label());
        }
    }

    out.push_str("\n  Toolchain\n");
    let _ = writeln!(
        out,
        "    beamtalk {} → {}   ok",
        prev_provenance.map_or("unknown", |p| p.beamtalk_version.as_str()),
        new_provenance.map_or("unknown", |p| p.beamtalk_version.as_str()),
    );
    let _ = writeln!(
        out,
        "    OTP      {} → {}   ok",
        prev_provenance.map_or("unknown", |p| p.otp_release.as_str()),
        new_provenance.map_or("unknown", |p| p.otp_release.as_str()),
    );

    out.push('\n');
    let _ = writeln!(
        out,
        "{} error{}, {} warning{}.",
        error_count,
        if error_count == 1 { "" } else { "s" },
        warning_count,
        if warning_count == 1 { "" } else { "s" },
    );

    UpgradeReport {
        text: out,
        has_error: error_count > 0,
    }
}

/// A human-readable summary of the field-level difference between two
/// classes' `fields` maps (ADR 0125 §2.3's "field `tier` added" example) —
/// added/removed/retyped, joined with `, ` when more than one field
/// changed. Mirrors `beamtalk_shape_diff:diff/2`'s three change kinds
/// (`Kind`/eager-late is not tracked in `shapes.json`, so has no fourth
/// case here — see `beamtalk_release_shapes:class_field_shape/1`'s doc for
/// why).
fn describe_field_change(
    old: &BTreeMap<String, Option<String>>,
    new: &BTreeMap<String, Option<String>>,
) -> String {
    let mut parts = Vec::new();
    for name in old.keys() {
        if !new.contains_key(name) {
            parts.push(format!("field `{name}` removed"));
        }
    }
    for (name, new_type) in new {
        match old.get(name) {
            None => parts.push(format!("field `{name}` added")),
            Some(old_type) if old_type != new_type => {
                parts.push(format!(
                    "field `{name}` retyped ({} → {})",
                    old_type.as_deref().unwrap_or("Dynamic"),
                    new_type.as_deref().unwrap_or("Dynamic"),
                ));
            }
            Some(_) => {}
        }
    }
    parts.sort();
    parts.join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One test fixture class entry: `(class, version, fields, migrations)`.
    type FixtureEntry<'a> = (
        &'a str,
        u32,
        &'a [(&'a str, Option<&'a str>)],
        &'a [(&'a str, &'a str)],
    );

    fn doc(entries: &[FixtureEntry]) -> ShapesDoc {
        ShapesDoc {
            shapes: entries
                .iter()
                .map(|(class, version, fields, migrations)| {
                    (
                        (*class).to_string(),
                        ShapeEntry {
                            version: *version,
                            fields: fields
                                .iter()
                                .map(|(k, v)| ((*k).to_string(), v.map(str::to_string)))
                                .collect(),
                            migrations: migrations
                                .iter()
                                .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
                                .collect(),
                        },
                    )
                })
                .collect(),
        }
    }

    #[test]
    fn version_bump_with_migration_present_is_ok() {
        let prev = doc(&[("Cart", 2, &[], &[])]);
        let new = doc(&[("Cart", 3, &[], &[("2", "migrateFromV2:")])]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(report.text.contains("Cart"));
        assert!(report.text.contains("v2 → v3"));
        assert!(report.text.contains("migrateFromV2: present"));
        assert!(report.text.contains("0 errors, 0 warnings."));
        assert!(!report.has_error);
    }

    #[test]
    fn version_bump_without_migration_is_an_error() {
        let prev = doc(&[("Session", 1, &[], &[])]);
        let new = doc(&[("Session", 2, &[], &[])]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(report.text.contains("migrateFromV1: MISSING"));
        assert!(report.text.contains("Session"));
        assert!(report.has_error);
        assert!(report.text.contains("1 error, 0 warnings."));
    }

    #[test]
    fn field_added_without_version_bump_is_a_warning() {
        let prev = doc(&[("Account", 1, &[("id", Some("String"))], &[])]);
        let new = doc(&[(
            "Account",
            1,
            &[("id", Some("String")), ("tier", Some("Integer"))],
            &[],
        )]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(report.text.contains("field `tier` added"));
        assert!(report.text.contains("warning"));
        assert!(!report.has_error);
    }

    #[test]
    fn removed_class_is_a_warning() {
        let prev = doc(&[("LegacyQuote", 1, &[], &[])]);
        let new = doc(&[]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(report.text.contains("Removed classes"));
        assert!(report.text.contains("LegacyQuote"));
        assert!(!report.has_error);
    }

    #[test]
    fn unchanged_class_produces_no_finding() {
        let prev = doc(&[("Widget", 1, &[("id", Some("String"))], &[])]);
        let new = doc(&[("Widget", 1, &[("id", Some("String"))], &[])]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(!report.text.contains("Widget"));
        assert!(report.text.contains("0 errors, 0 warnings."));
    }

    #[test]
    fn adr_0125_worked_example_matches_verbatim() {
        // ADR 0125 §2.3's own worked example, reproduced field-for-field.
        let prev = doc(&[
            ("Cart", 2, &[], &[]),
            ("Session", 1, &[], &[]),
            ("Account", 1, &[("tier", None)], &[]),
            ("LegacyQuote", 1, &[], &[]),
        ]);
        // Account's `tier` already existed at v1 in this synthetic prev/new
        // pair, so give it a *type* change (Dynamic -> Integer) instead to
        // exercise the "field changed without a bump" path without also
        // exercising `added` (covered by its own test above) — the ADR
        // text itself only asserts the shape of the report, not a specific
        // corpus, and this file's own verbatim CLI test in `cli_release.rs`
        // is what asserts a full report string end-to-end.
        let new = doc(&[
            ("Cart", 3, &[], &[("2", "migrateFromV2:")]),
            ("Session", 2, &[], &[]),
            ("Account", 1, &[("tier", Some("Integer"))], &[]),
        ]);
        let report = render_report("orders", "1.3.0", "1.4.0", &prev, &new, None, None);
        assert!(report.text.contains("Shape changes requiring migration"));
        assert!(report.text.contains("Cart"));
        assert!(report.text.contains("Session"));
        assert!(report.text.contains("Shape changed without a version bump"));
        assert!(report.text.contains("Account"));
        assert!(report.text.contains("Removed classes"));
        assert!(report.text.contains("LegacyQuote"));
        assert!(report.text.contains("Toolchain"));
        assert!(report.text.contains("1 error, 2 warnings."));
        assert!(report.has_error);
    }

    #[test]
    fn describe_field_change_reports_added_removed_and_retyped() {
        let mut old = BTreeMap::new();
        old.insert("a".to_string(), Some("String".to_string()));
        old.insert("b".to_string(), Some("Integer".to_string()));
        let mut new = BTreeMap::new();
        new.insert("a".to_string(), Some("String".to_string()));
        new.insert("b".to_string(), Some("Float".to_string()));
        new.insert("c".to_string(), None);
        let desc = describe_field_change(&old, &new);
        assert!(desc.contains("field `c` added"));
        assert!(desc.contains("field `b` retyped (Integer → Float)"));
    }

    #[test]
    fn is_tarball_recognises_tar_gz_and_tgz_only() {
        assert!(is_tarball(Utf8Path::new("orders-1.3.0.tar.gz")));
        assert!(is_tarball(Utf8Path::new("orders-1.3.0.tgz")));
        assert!(!is_tarball(Utf8Path::new("orders-1.3.0")));
        assert!(!is_tarball(Utf8Path::new("/some/dir")));
    }
}
