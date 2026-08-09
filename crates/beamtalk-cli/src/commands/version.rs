// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk version` — show, set, or bump the package version (BT-2980).
//!
//! **DDD Context:** Build System
//!
//! ```bash
//! beamtalk version               # print the current version
//! beamtalk version 0.3.0         # set an exact version (must be greater)
//! beamtalk version bump minor    # increment major.minor.patch
//! ```
//!
//! Edits are surgical: only the `version` value on the `[package]` section's
//! `version` line is rewritten (same approach as
//! `deps::cli::append_dependency_to_manifest`) — comments, key order, and
//! unrelated whitespace elsewhere in `beamtalk.toml` are left untouched.

use camino::Utf8Path;
use miette::{IntoDiagnostic, Result, WrapErr};
use std::cmp::Ordering;

use crate::commands::deps::registry::compare_versions;
use crate::commands::manifest::{self, validate_exact_version};
use crate::commands::util::find_project_root;

/// Which segment `bump` increments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BumpPart {
    Major,
    Minor,
    Patch,
}

/// Run `beamtalk version` with its raw positional arguments.
///
/// `args` is intentionally a free-form slice rather than a clap subcommand:
/// the desired surface is `beamtalk version X.Y.Z` and `beamtalk version bump
/// <part>` directly (no `set`/`bump` subcommand ambiguity with clap's derive
/// `Subcommand`), while staying distinct from clap's own `--version`/`-V`
/// flag on the top-level `Cli`.
///
/// # Errors
///
/// Returns an error if no `beamtalk.toml` is found in the current directory,
/// the arguments don't match a supported form, the requested version is
/// malformed or not greater than the current one, or the manifest can't be
/// read or written.
pub fn run(args: &[String]) -> Result<()> {
    let project_root = find_project_root()?;
    let manifest_path = project_root.join("beamtalk.toml");

    match args {
        [] => run_show(&manifest_path),
        [bump, part] if bump == "bump" => {
            let part = parse_bump_part(part)?;
            run_bump(&manifest_path, part)
        }
        [version] => run_set(&manifest_path, version),
        _ => miette::bail!(
            "Usage: beamtalk version [X.Y.Z | bump patch|minor|major]\n\n  \
             Got: beamtalk version {}",
            args.join(" ")
        ),
    }
}

fn parse_bump_part(s: &str) -> Result<BumpPart> {
    match s {
        "major" => Ok(BumpPart::Major),
        "minor" => Ok(BumpPart::Minor),
        "patch" => Ok(BumpPart::Patch),
        other => {
            miette::bail!("Unknown 'bump' part '{other}' — expected 'patch', 'minor', or 'major'")
        }
    }
}

/// Print the package's current version.
fn run_show(manifest_path: &Utf8Path) -> Result<()> {
    let manifest = manifest::parse_manifest(manifest_path)?;
    println!("{}", manifest.version);
    Ok(())
}

/// Set the version to an exact value, validated and required to be greater
/// than the current version.
fn run_set(manifest_path: &Utf8Path, new_version: &str) -> Result<()> {
    let manifest = manifest::parse_manifest(manifest_path)?;
    let current = &manifest.version;

    if let Err(reason) = validate_exact_version(new_version) {
        miette::bail!(
            "Invalid version '{new_version}' — {reason}\n\n  \
             Versions must be exact major.minor.patch, e.g. `beamtalk version 1.2.3`."
        );
    }

    if compare_versions(new_version, current) != Ordering::Greater {
        miette::bail!(
            "New version '{new_version}' must be greater than the current version '{current}'.\n\n  \
             To increment instead, use `beamtalk version bump patch|minor|major`."
        );
    }

    set_version_in_manifest(manifest_path, new_version)?;
    println!("{current} -> {new_version}");
    Ok(())
}

/// Bump the given segment of the current version, resetting lower segments
/// to zero (standard semver bump semantics).
fn run_bump(manifest_path: &Utf8Path, part: BumpPart) -> Result<()> {
    let manifest = manifest::parse_manifest(manifest_path)?;
    let current = &manifest.version;
    let (major, minor, patch) = parse_semver(current, manifest_path)?;

    let new_version = match part {
        BumpPart::Major => format!("{}.0.0", major + 1),
        BumpPart::Minor => format!("{major}.{}.0", minor + 1),
        BumpPart::Patch => format!("{major}.{minor}.{}", patch + 1),
    };

    set_version_in_manifest(manifest_path, &new_version)?;
    println!("{current} -> {new_version}");
    Ok(())
}

/// Parse the manifest's current version into `(major, minor, patch)`.
///
/// The manifest's existing version is expected to already be an exact
/// version (as required to `set` one), but a hand-edited `beamtalk.toml`
/// could carry something else — report that clearly rather than panicking.
fn parse_semver(version: &str, manifest_path: &Utf8Path) -> Result<(u64, u64, u64)> {
    if let Err(reason) = validate_exact_version(version) {
        miette::bail!(
            "Cannot bump — the current version '{version}' in '{manifest_path}' is not a \
             valid exact version: {reason}\n\n  \
             Fix it with `beamtalk version X.Y.Z` first."
        );
    }

    let mut segments = version.split('.');
    let parse_segment = |s: Option<&str>| -> Result<u64> {
        s.expect("validate_exact_version guarantees three segments")
            .parse::<u64>()
            .into_diagnostic()
            .wrap_err_with(|| format!("Version segment in '{version}' is out of range"))
    };

    let major = parse_segment(segments.next())?;
    let minor = parse_segment(segments.next())?;
    let patch = parse_segment(segments.next())?;

    Ok((major, minor, patch))
}

/// Rewrite only the `version` value on the `[package]` section's `version`
/// line, preserving indentation, quote style, and any trailing comment.
///
/// Mirrors `deps::cli::append_dependency_to_manifest`'s surgical-edit
/// approach: read the whole file, patch exactly one line, validate the
/// result still parses as TOML, then write it back.
pub(crate) fn set_version_in_manifest(manifest_path: &Utf8Path, new_version: &str) -> Result<()> {
    let content = std::fs::read_to_string(manifest_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{manifest_path}'"))?;

    let lines: Vec<&str> = content.lines().collect();

    let package_idx = lines
        .iter()
        .position(|l| l.trim() == "[package]")
        .ok_or_else(|| miette::miette!("No [package] section found in '{manifest_path}'"))?;

    let section_end = lines
        .iter()
        .enumerate()
        .skip(package_idx + 1)
        .find(|(_, l)| l.trim_start().starts_with('['))
        .map_or(lines.len(), |(i, _)| i);

    let version_idx = lines[package_idx + 1..section_end]
        .iter()
        .position(|l| {
            let trimmed = l.trim_start();
            trimmed
                .strip_prefix("version")
                .is_some_and(|rest| rest.trim_start().starts_with('='))
        })
        .map(|i| package_idx + 1 + i)
        .ok_or_else(|| {
            miette::miette!(
                "No 'version' field found in the [package] section of '{manifest_path}'"
            )
        })?;

    let new_line = replace_version_value(lines[version_idx], new_version, manifest_path)?;

    let mut new_lines: Vec<String> = lines.iter().map(|l| (*l).to_string()).collect();
    new_lines[version_idx] = new_line;

    let mut new_content = new_lines.join("\n");
    if content.ends_with('\n') {
        new_content.push('\n');
    }

    // Validate the new TOML parses correctly before writing.
    let _: toml::Value = toml::from_str(&new_content)
        .into_diagnostic()
        .wrap_err("Generated beamtalk.toml is invalid TOML — this is a bug")?;

    std::fs::write(manifest_path, new_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{manifest_path}'"))?;

    Ok(())
}

/// Replace the quoted value on a `version = "..."` line, keeping the
/// original indentation, quote character, and anything after the closing
/// quote (e.g. a trailing `# comment`).
fn replace_version_value(
    line: &str,
    new_version: &str,
    manifest_path: &Utf8Path,
) -> Result<String> {
    let indent_len = line.len() - line.trim_start().len();
    let indent = &line[..indent_len];

    let eq_idx = line
        .find('=')
        .ok_or_else(|| miette::miette!("Malformed 'version' line in '{manifest_path}' — no '='"))?;

    let after_eq = &line[eq_idx + 1..];
    let value_offset = after_eq.len() - after_eq.trim_start().len();
    let quote_idx = eq_idx + 1 + value_offset;

    let quote_char = line[quote_idx..].chars().next().ok_or_else(|| {
        miette::miette!("Malformed 'version' line in '{manifest_path}' — no value after '='")
    })?;

    if quote_char != '"' && quote_char != '\'' {
        miette::bail!(
            "The 'version' field in '{manifest_path}' is not a quoted string — refusing to \
             edit it automatically"
        );
    }

    let after_quote = &line[quote_idx + 1..];
    let close_offset = after_quote.find(quote_char).ok_or_else(|| {
        miette::miette!("Malformed 'version' line in '{manifest_path}' — unterminated string")
    })?;
    let trailing = &after_quote[close_offset + 1..];

    Ok(format!(
        "{indent}version = {quote_char}{new_version}{quote_char}{trailing}"
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;
    use serial_test::serial;
    use tempfile::TempDir;

    fn write_manifest(dir: &TempDir, content: &str) -> Utf8PathBuf {
        let path = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        std::fs::write(path.join("beamtalk.toml"), content).unwrap();
        path.join("beamtalk.toml")
    }

    fn flat_err(err: &miette::Report) -> String {
        format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Restores the process cwd to the directory captured at construction
    /// when dropped — including on an unwinding panic, so one failing test
    /// can't strand the process cwd inside a tempdir that a later
    /// `#[serial(cwd)]` test then deletes out from under it.
    struct CwdGuard(std::path::PathBuf);

    impl Drop for CwdGuard {
        fn drop(&mut self) {
            let _ = std::env::set_current_dir(&self.0);
        }
    }

    /// Run `f` with the process cwd set to `dir`, always restoring the
    /// original cwd afterward, even if `f` panics. Callers must serialize
    /// with `#[serial(cwd)]` since the working directory is process-global.
    fn with_cwd<T>(dir: &std::path::Path, f: impl FnOnce() -> T) -> T {
        let _guard = CwdGuard(std::env::current_dir().unwrap());
        std::env::set_current_dir(dir).unwrap();
        f()
    }

    // -----------------------------------------------------------------------
    // run() dispatch (exercises find_project_root via cwd)
    // -----------------------------------------------------------------------

    #[test]
    #[serial(cwd)]
    fn test_run_no_args_shows_version() {
        let temp = TempDir::new().unwrap();
        write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");
        let result = with_cwd(temp.path(), || run(&[]));
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    #[serial(cwd)]
    fn test_run_single_arg_sets_version() {
        let temp = TempDir::new().unwrap();
        write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");
        let args = ["0.2.0".to_string()];
        let result = with_cwd(temp.path(), || run(&args));
        assert!(result.is_ok(), "{:?}", result.err());

        let manifest_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf())
            .unwrap()
            .join("beamtalk.toml");
        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "0.2.0");
    }

    #[test]
    #[serial(cwd)]
    fn test_run_bump_args_bumps_version() {
        let temp = TempDir::new().unwrap();
        write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"1.2.3\"\n");
        let args = ["bump".to_string(), "minor".to_string()];
        let result = with_cwd(temp.path(), || run(&args));
        assert!(result.is_ok(), "{:?}", result.err());

        let manifest_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf())
            .unwrap()
            .join("beamtalk.toml");
        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "1.3.0");
    }

    #[test]
    #[serial(cwd)]
    fn test_run_rejects_too_many_args() {
        let temp = TempDir::new().unwrap();
        write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");
        let args = ["bump".to_string(), "patch".to_string(), "extra".to_string()];
        let result = with_cwd(temp.path(), || run(&args));
        assert!(result.is_err());
        assert!(flat_err(&result.unwrap_err()).contains("Usage"));
    }

    #[test]
    #[serial(cwd)]
    fn test_run_no_manifest_errors() {
        let temp = TempDir::new().unwrap();
        let result = with_cwd(temp.path(), || run(&[]));
        assert!(result.is_err());
        assert!(flat_err(&result.unwrap_err()).contains("No beamtalk.toml"));
    }

    // -----------------------------------------------------------------------
    // run_set
    // -----------------------------------------------------------------------

    #[test]
    fn test_set_updates_version() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");

        run_set(&manifest_path, "0.2.0").unwrap();

        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "0.2.0");
    }

    #[test]
    fn test_set_rejects_downgrade() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.5.0\"\n");

        let err = run_set(&manifest_path, "0.4.0").unwrap_err();
        assert!(flat_err(&err).contains("must be greater"), "{err:?}");
    }

    #[test]
    fn test_set_rejects_equal_version() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.5.0\"\n");

        let err = run_set(&manifest_path, "0.5.0").unwrap_err();
        assert!(flat_err(&err).contains("must be greater"), "{err:?}");
    }

    #[test]
    fn test_set_rejects_malformed_version() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.5.0\"\n");

        let err = run_set(&manifest_path, "not-a-version").unwrap_err();
        assert!(flat_err(&err).contains("Invalid version"), "{err:?}");
    }

    #[test]
    fn test_set_rejects_version_range() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.5.0\"\n");

        let err = run_set(&manifest_path, "~> 1.0").unwrap_err();
        assert!(
            flat_err(&err).contains("version ranges are not supported"),
            "{err:?}"
        );
    }

    // -----------------------------------------------------------------------
    // run_bump
    // -----------------------------------------------------------------------

    #[test]
    fn test_bump_patch() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"1.2.3\"\n");

        run_bump(&manifest_path, BumpPart::Patch).unwrap();

        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "1.2.4");
    }

    #[test]
    fn test_bump_minor_resets_patch() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"1.2.3\"\n");

        run_bump(&manifest_path, BumpPart::Minor).unwrap();

        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "1.3.0");
    }

    #[test]
    fn test_bump_major_resets_minor_and_patch() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"1.2.3\"\n");

        run_bump(&manifest_path, BumpPart::Major).unwrap();

        let manifest = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(manifest.version, "2.0.0");
    }

    #[test]
    fn test_bump_rejects_malformed_current_version() {
        let temp = TempDir::new().unwrap();
        let manifest_path =
            write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"1.2\"\n");

        let err = run_bump(&manifest_path, BumpPart::Patch).unwrap_err();
        assert!(flat_err(&err).contains("Cannot bump"), "{err:?}");
    }

    #[test]
    fn test_parse_bump_part_rejects_unknown() {
        let err = parse_bump_part("banana").unwrap_err();
        assert!(flat_err(&err).contains("Unknown"), "{err:?}");
    }

    // -----------------------------------------------------------------------
    // set_version_in_manifest — formatting preservation
    // -----------------------------------------------------------------------

    #[test]
    fn test_set_version_preserves_surrounding_formatting() {
        let temp = TempDir::new().unwrap();
        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"
description = \"A cool app\"

[dependencies]
utils = { path = \"../utils\" }
";
        let manifest_path = write_manifest(&temp, content);

        set_version_in_manifest(&manifest_path, "0.2.0").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(new_content.contains("version = \"0.2.0\""));
        assert!(new_content.contains("description = \"A cool app\""));
        assert!(new_content.contains("utils = { path = \"../utils\" }"));
        let _: toml::Value = toml::from_str(&new_content).unwrap();
    }

    #[test]
    fn test_set_version_preserves_indentation_and_trailing_comment() {
        let temp = TempDir::new().unwrap();
        let content = "[package]\nname = \"my_app\"\n  version = \"0.1.0\"  # pinned\n";
        let manifest_path = write_manifest(&temp, content);

        set_version_in_manifest(&manifest_path, "0.2.0").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert_eq!(
            new_content,
            "[package]\nname = \"my_app\"\n  version = \"0.2.0\"  # pinned\n"
        );
    }

    #[test]
    fn test_set_version_preserves_single_quote_style() {
        let temp = TempDir::new().unwrap();
        let content = "[package]\nname = \"my_app\"\nversion = '0.1.0'\n";
        let manifest_path = write_manifest(&temp, content);

        set_version_in_manifest(&manifest_path, "0.2.0").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(new_content.contains("version = '0.2.0'"));
    }

    #[test]
    fn test_set_version_missing_package_section_errors() {
        let temp = TempDir::new().unwrap();
        let manifest_path = write_manifest(&temp, "[dependencies]\n");

        let err = set_version_in_manifest(&manifest_path, "0.2.0").unwrap_err();
        assert!(flat_err(&err).contains("[package]"), "{err:?}");
    }

    #[test]
    fn test_set_version_missing_version_field_errors() {
        let temp = TempDir::new().unwrap();
        let manifest_path = write_manifest(&temp, "[package]\nname = \"my_app\"\n");

        let err = set_version_in_manifest(&manifest_path, "0.2.0").unwrap_err();
        assert!(flat_err(&err).contains("No 'version' field"), "{err:?}");
    }

    #[test]
    fn test_set_version_stops_at_next_section() {
        let temp = TempDir::new().unwrap();
        // A `version` key inside a *different* section must not be touched.
        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
version = { path = \"../version-lib\" }
";
        let manifest_path = write_manifest(&temp, content);

        set_version_in_manifest(&manifest_path, "0.2.0").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(new_content.contains("version = \"0.2.0\""));
        assert!(new_content.contains("version = { path = \"../version-lib\" }"));
    }
}
