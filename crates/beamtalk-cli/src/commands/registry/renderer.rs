// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! HTML rendering for the registry index page and per-package detail pages.
//!
//! **DDD Context:** CLI / Documentation

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write as _;
use std::fs;
use tracing::debug;

use crate::commands::deps::registry::{RegistryEntry, RegistryRelease, compare_versions};

use super::layout::{page_footer, page_header};

/// Escape HTML special characters.
///
/// Index entries are third-party data (published by any package author, not
/// validated beyond TOML syntax), so every field is escaped before it lands
/// in generated HTML.
fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// The dependency snippet for a `(name, version)` pair, matching the bare
/// registry-string `[dependencies]` format `beamtalk.toml` accepts (BT-2978):
/// `name = "x.y.z"`.
fn dependency_snippet(name: &str, version: &str) -> String {
    format!(
        "{} = &quot;{}&quot;",
        html_escape(name),
        html_escape(version)
    )
}

/// Sort an entry's versions newest-first for display, without mutating the
/// caller's copy (entries are shared across the index and detail pages).
fn versions_newest_first(entry: &RegistryEntry) -> Vec<&RegistryRelease> {
    let mut versions: Vec<_> = entry.versions.iter().collect();
    versions.sort_by(|a, b| compare_versions(&b.version, &a.version));
    versions
}

/// Render the registry index page listing every package.
///
/// `entries` is sorted by name already ([`super::registry::list_all_entries`]
/// guarantees this); rendering does not re-sort it.
///
/// An empty `entries` list (a registry with no published packages yet) still
/// renders a complete, valid page — with a "no packages" message in place of
/// the list — rather than erroring (BT-2990 acceptance criterion).
pub(super) fn write_index_page(output_dir: &Utf8Path, entries: &[RegistryEntry]) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header("Beamtalk Package Registry"));
    html.push_str("<h1>Beamtalk Package Registry</h1>\n");
    html.push_str(
        "<p class=\"lede\">Every package published to the Beamtalk registry index. \
         Add one to a project with <code>beamtalk deps add &lt;name&gt;</code>, or \
         pin an exact version in <code>beamtalk.toml</code> — see a package's \
         page below for its copy-paste snippet.</p>\n",
    );

    if entries.is_empty() {
        html.push_str(
            "<p class=\"package-empty\">No packages have been published to this registry yet.</p>\n",
        );
    } else {
        html.push_str("<ul class=\"package-list\">\n");
        for entry in entries {
            let name = html_escape(&entry.name);
            let description = entry
                .description
                .as_deref()
                .map(html_escape)
                .unwrap_or_default();
            let latest_badge = entry
                .latest_version()
                .map(|r| {
                    format!(
                        "<span class=\"package-version\">{}</span>",
                        html_escape(&r.version)
                    )
                })
                .unwrap_or_default();
            let desc_html = if description.is_empty() {
                String::new()
            } else {
                format!("<p class=\"package-description\">{description}</p>\n")
            };
            let _ = writeln!(
                html,
                "<li>\n<a href=\"{name}.html\"><span class=\"package-name\">{name}</span>{latest_badge}</a>\n{desc_html}</li>"
            );
        }
        html.push_str("</ul>\n");
    }

    html.push_str(&page_footer());

    let index_path = output_dir.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write registry index.html")?;
    debug!("Generated {index_path}");
    Ok(())
}

/// Render a single package's detail page: description, every published
/// version with its git repo link and tag, and a copy-paste dependency
/// snippet for each.
pub(super) fn write_detail_page(output_dir: &Utf8Path, entry: &RegistryEntry) -> Result<()> {
    let name = html_escape(&entry.name);
    let mut html = String::new();
    html.push_str(&page_header(&format!("{name} — Beamtalk Package Registry")));
    html.push_str("<div class=\"breadcrumb\"><a href=\"index.html\">Registry</a> &rsaquo; ");
    let _ = writeln!(html, "{name}</div>");
    let _ = writeln!(html, "<h1>{name}</h1>");

    if let Some(description) = entry.description.as_deref() {
        let _ = writeln!(html, "<p class=\"lede\">{}</p>", html_escape(description));
    }

    let versions = versions_newest_first(entry);

    if versions.is_empty() {
        html.push_str(
            "<p class=\"package-empty\">This package has no published versions yet.</p>\n",
        );
    } else {
        // The latest version gets its own prominent snippet, ahead of the
        // full version table below.
        if let Some(latest) = entry.latest_version() {
            html.push_str("<h2>Add this dependency</h2>\n");
            let _ = writeln!(
                html,
                "<pre><code>{}</code></pre>",
                dependency_snippet(&entry.name, &latest.version)
            );
        }

        html.push_str("<h2>Published versions</h2>\n");
        html.push_str(
            "<table>\n<thead>\n<tr><th>Version</th><th>Git repository</th><th>Tag</th>\
             <th>Dependency snippet</th></tr>\n</thead>\n<tbody>\n",
        );
        for release in &versions {
            let version = html_escape(&release.version);
            let git = html_escape(&release.git);
            let tag = html_escape(&release.tag);
            let snippet = dependency_snippet(&entry.name, &release.version);
            // release.git is untrusted third-party data (parsed from a TOML index
            // entry, not necessarily written by `beamtalk publish`). html_escape
            // neutralizes markup but not a `javascript:`/`data:` URI scheme, so
            // only linkify known-safe schemes; otherwise render as plain text.
            let git_href =
                if release.git.starts_with("https://") || release.git.starts_with("http://") {
                    format!("<a href=\"{git}\">{git}</a>")
                } else {
                    git.clone()
                };
            let _ = writeln!(
                html,
                "<tr><td class=\"version-cell\">{version}</td>\
                 <td>{git_href}</td>\
                 <td>{tag}</td>\
                 <td class=\"snippet-cell\"><code>{snippet}</code></td></tr>"
            );
        }
        html.push_str("</tbody>\n</table>\n");
    }

    html.push_str(&page_footer());

    let out_path = output_dir.join(format!("{}.html", entry.name));
    fs::write(&out_path, html)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write registry page for '{}'", entry.name))?;
    debug!("Generated {out_path}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::deps::registry::parse_index_entry;
    use tempfile::TempDir;

    fn entry(toml: &str, name: &str) -> RegistryEntry {
        parse_index_entry(name, toml).unwrap()
    }

    const YAML_TOML: &str = r#"
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
    fn test_write_index_page_lists_packages_with_latest_version() {
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let entries = vec![entry(YAML_TOML, "yaml")];

        write_index_page(&out, &entries).unwrap();

        let html = fs::read_to_string(out.join("index.html")).unwrap();
        assert!(html.contains("yaml"));
        assert!(html.contains("YAML parsing for Beamtalk"));
        assert!(html.contains("0.2.1")); // latest, not 0.1.0
        assert!(html.contains("href=\"yaml.html\""));
    }

    #[test]
    fn test_write_index_page_empty_registry_still_valid() {
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();

        write_index_page(&out, &[]).unwrap();

        let html = fs::read_to_string(out.join("index.html")).unwrap();
        assert!(html.contains("No packages"));
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("</html>"));
    }

    #[test]
    fn test_write_detail_page_lists_all_versions_newest_first_with_snippets() {
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let e = entry(YAML_TOML, "yaml");

        write_detail_page(&out, &e).unwrap();

        let html = fs::read_to_string(out.join("yaml.html")).unwrap();
        assert!(html.contains("YAML parsing for Beamtalk"));
        assert!(html.contains("https://example.test/yaml"));
        assert!(html.contains("release-0.2.1"));
        assert!(html.contains("v0.1.0")); // default tag
        assert!(html.contains("yaml = &quot;0.2.1&quot;"));
        assert!(html.contains("yaml = &quot;0.1.0&quot;"));

        // Newest first: 0.2.1's row appears before 0.1.0's.
        let pos_latest = html.find("0.2.1").unwrap();
        let pos_older = html.find("0.1.0").unwrap();
        assert!(pos_latest < pos_older, "expected 0.2.1 listed before 0.1.0");
    }

    #[test]
    fn test_write_detail_page_no_versions_still_valid() {
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let e = entry("name = \"empty\"\n", "empty");

        write_detail_page(&out, &e).unwrap();

        let html = fs::read_to_string(out.join("empty.html")).unwrap();
        assert!(html.contains("no published versions"));
        assert!(html.contains("<!DOCTYPE html>"));
    }

    #[test]
    fn test_html_escape_applied_to_untrusted_fields() {
        let e = entry(
            "name = \"xss\"\ndescription = \"<script>alert(1)</script>\"\n",
            "xss",
        );
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();

        write_detail_page(&out, &e).unwrap();
        let html = fs::read_to_string(out.join("xss.html")).unwrap();
        assert!(!html.contains("<script>"));
        assert!(html.contains("&lt;script&gt;"));
    }

    #[test]
    fn test_unsafe_git_scheme_is_not_linkified() {
        let e = entry(
            "name = \"evil\"\n\n[[versions]]\nversion = \"1.0.0\"\ngit = \"javascript:alert(1)\"\n",
            "evil",
        );
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();

        write_detail_page(&out, &e).unwrap();
        let html = fs::read_to_string(out.join("evil.html")).unwrap();
        assert!(!html.contains("href=\"javascript:"));
        assert!(html.contains("javascript:alert(1)")); // still shown as plain text
    }

    #[test]
    fn test_https_git_scheme_is_still_linkified() {
        let e = entry(YAML_TOML, "yaml");
        let dir = TempDir::new().unwrap();
        let out = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();

        write_detail_page(&out, &e).unwrap();
        let html = fs::read_to_string(out.join("yaml.html")).unwrap();
        assert!(html.contains("href=\"https://example.test/yaml\""));
    }
}
