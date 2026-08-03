// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static site generation for the Beamtalk package registry index (BT-2990).
//!
//! **DDD Context:** CLI / Documentation
//!
//! A read-only rendering layer over the registry index format introduced by
//! BT-2978 (`crate::commands::deps::registry`) — no server involved, the same
//! way the index itself is nothing more than a git repository. Mirrors the
//! existing `beamtalk doc --site` generator
//! (`crate::commands::doc::{renderer,layout,assets,site}`): hand-rolled
//! HTML/CSS, no templating crate, same file-split convention. Reuses that
//! module's index-parsing (`RegistryEntry`, `read_entry`, `list_all_entries`,
//! …) rather than re-implementing the `packages/<name>.toml` format.

mod assets;
pub mod cli;
mod layout;
mod renderer;

use camino::Utf8PathBuf;
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::{info, instrument};

use crate::commands::deps::registry;

use assets::write_css;
use renderer::{write_detail_page, write_index_page};

/// Generate the registry static site: an index page listing every package,
/// and one detail page per package.
///
/// `index` is a registry index location — a local directory or a git URL,
/// the same kind of value `BEAMTALK_REGISTRY`/`[registry] url` accept
/// elsewhere. `output_dir` is created if it does not already exist.
///
/// An index with no packages published yet renders a valid, non-erroring
/// site (an index page saying so, and no detail pages) rather than failing.
///
/// # Errors
///
/// Returns an error if the index cannot be resolved (an unreachable git URL,
/// or a local directory missing its `packages/` subdirectory), or if writing
/// the output fails.
#[instrument(skip_all, fields(index, output = %output_dir))]
pub fn run_site(index: &str, output_dir: &str) -> Result<()> {
    info!("Generating registry site");
    let output_path = Utf8PathBuf::from(output_dir);
    fs::create_dir_all(&output_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create output directory '{output_path}'"))?;

    // `registry site` runs outside any Beamtalk project, so there is no
    // project root to resolve a relative `--index` directory against —
    // the process's current directory stands in for it, matching how a
    // relative path on the command line is ordinarily interpreted.
    let cwd = std::env::current_dir()
        .into_diagnostic()
        .wrap_err("Failed to determine the current directory")?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|p| miette::miette!("Current directory is not valid UTF-8: {}", p.display()))?;

    let location = registry::classify_location(&cwd, index);
    let index_root = registry::ensure_index(&location, &cwd, false)?;
    let entries = registry::list_all_entries(&index_root)?;

    write_css(&output_path)?;
    write_index_page(&output_path, &entries)?;
    for entry in &entries {
        write_detail_page(&output_path, entry)?;
    }

    println!("Generated registry site for {} package(s)", entries.len());
    println!("  Output: {output_path}/");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;
    use std::fs;
    use tempfile::TempDir;

    fn utf8(dir: &TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    /// Build a local registry index directory with the given package files.
    fn make_index(entries: &[(&str, &str)]) -> (TempDir, Utf8PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = utf8(&dir);
        fs::create_dir_all(root.join("packages")).unwrap();
        for (name, content) in entries {
            fs::write(root.join("packages").join(format!("{name}.toml")), content).unwrap();
        }
        (dir, root)
    }

    const YAML_ENTRY: &str = r#"
name = "yaml"
description = "YAML parsing for Beamtalk"

[[versions]]
version = "0.1.0"
git = "https://example.test/yaml"

[[versions]]
version = "0.2.1"
git = "https://example.test/yaml"
tag = "release-0.2.1"
"#;

    #[test]
    fn test_run_site_generates_index_and_detail_pages() {
        let (_index_dir, index_root) = make_index(&[
            ("yaml", YAML_ENTRY),
            (
                "json",
                "name = \"json\"\ndescription = \"JSON for Beamtalk\"\n\n[[versions]]\nversion = \"1.0.0\"\ngit = \"https://example.test/json\"\n",
            ),
        ]);
        let out_dir = TempDir::new().unwrap();
        let out_root = utf8(&out_dir);

        run_site(index_root.as_str(), out_root.as_str()).unwrap();

        assert!(out_root.join("index.html").exists());
        assert!(out_root.join("style.css").exists());
        assert!(out_root.join("yaml.html").exists());
        assert!(out_root.join("json.html").exists());

        let index_html = fs::read_to_string(out_root.join("index.html")).unwrap();
        assert!(index_html.contains("yaml"));
        assert!(index_html.contains("json"));
        assert!(index_html.contains("0.2.1")); // yaml's latest version
        assert!(index_html.contains("yaml.html"));

        let yaml_html = fs::read_to_string(out_root.join("yaml.html")).unwrap();
        assert!(yaml_html.contains("YAML parsing for Beamtalk"));
        assert!(yaml_html.contains("0.2.1"));
        assert!(yaml_html.contains("0.1.0"));
        assert!(yaml_html.contains("release-0.2.1"));
        assert!(yaml_html.contains("https://example.test/yaml"));
        assert!(yaml_html.contains("yaml = &quot;0.2.1&quot;"));
    }

    #[test]
    fn test_run_site_empty_registry_renders_without_error() {
        let (_index_dir, index_root) = make_index(&[]);
        let out_dir = TempDir::new().unwrap();
        let out_root = utf8(&out_dir);

        run_site(index_root.as_str(), out_root.as_str()).unwrap();

        assert!(out_root.join("index.html").exists());
        let index_html = fs::read_to_string(out_root.join("index.html")).unwrap();
        assert!(index_html.contains("No packages"));
    }

    #[test]
    fn test_run_site_creates_output_dir() {
        let (_index_dir, index_root) = make_index(&[("yaml", YAML_ENTRY)]);
        let out_dir = TempDir::new().unwrap();
        let out_root = utf8(&out_dir).join("nested/site");

        run_site(index_root.as_str(), out_root.as_str()).unwrap();
        assert!(out_root.join("index.html").exists());
    }

    #[test]
    fn test_run_site_missing_index_errors_without_partial_output() {
        let out_dir = TempDir::new().unwrap();
        let out_root = utf8(&out_dir);
        let missing = out_root.join("does-not-exist-as-a-registry");

        let err = run_site(missing.as_str(), out_root.as_str()).unwrap_err();
        assert!(!format!("{err:?}").is_empty());
    }
}
