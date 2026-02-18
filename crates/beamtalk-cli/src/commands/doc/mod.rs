// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generate HTML API documentation from Beamtalk source files.
//!
//! **DDD Context:** CLI / Documentation
//!
//! Parses `.bt` source files, extracts doc comments and class hierarchy,
//! and generates static HTML reference documentation. The generated docs
//! match what would be available at runtime via EEP-48 `code:get_doc/1`.
//!
//! Part of ADR 0008 (Doc Comments and API Documentation).

mod assets;
mod extractor;
mod highlighter;
mod layout;
mod renderer;
mod site;

// Re-export for potential LSP reuse.
#[allow(unused_imports)]
pub use extractor::ClassInfo;
#[allow(unused_imports)]
pub use extractor::MethodInfo;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fs;
use tracing::{info, instrument};

use assets::{write_css, write_search_js};
use extractor::{collect_inherited_methods, find_source_files, parse_class_info};
use layout::build_sidebar_html;
use renderer::{write_class_page, write_index_page};
use site::{generate_prose_docs, write_site_landing_page};

/// Prose documentation pages to render from the docs/ directory.
const PROSE_PAGES: &[(&str, &str, &str)] = &[
    (
        "beamtalk-language-features.md",
        "language-features.html",
        "Language Features",
    ),
    (
        "beamtalk-principles.md",
        "principles.html",
        "Design Principles",
    ),
    (
        "beamtalk-syntax-rationale.md",
        "syntax-rationale.html",
        "Syntax Rationale",
    ),
    (
        "beamtalk-architecture.md",
        "architecture.html",
        "Architecture",
    ),
    ("beamtalk-ddd-model.md", "ddd-model.html", "Domain Model"),
    ("beamtalk-security.md", "security.html", "Security"),
    (
        "beamtalk-agent-native-development.md",
        "agent-native-development.html",
        "Agent-Native Development",
    ),
    (
        "known-limitations.md",
        "known-limitations.html",
        "Known Limitations",
    ),
];

/// Generate HTML API documentation.
///
/// Parses all `.bt` files in the given path, extracts doc comments and
/// class hierarchy, and generates static HTML to the output directory.
#[instrument(skip_all, fields(path = %path, output = %output_dir))]
pub fn run(path: &str, output_dir: &str) -> Result<()> {
    info!("Generating documentation");
    let source_path = Utf8PathBuf::from(path);
    let output_path = Utf8PathBuf::from(output_dir);

    generate_api_docs(&source_path, &output_path, "")?;

    Ok(())
}

/// Generate the full documentation site with landing page, API docs, and prose pages.
///
/// Site structure:
/// - `/` — Landing page with navigation
/// - `/apidocs/` — API reference (class docs from `.bt` files)
/// - `/docs/` — Prose documentation pages rendered from markdown
#[instrument(skip_all, fields(lib_path = %lib_path, docs_path = %docs_path, output = %output_dir))]
pub fn run_site(lib_path: &str, docs_path: &str, output_dir: &str) -> Result<()> {
    info!("Generating documentation site");
    let output_path = Utf8PathBuf::from(output_dir);

    // Create output directory
    fs::create_dir_all(&output_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create output directory '{output_path}'"))?;

    // 1. Generate API docs in apidocs/ subdirectory
    let lib_source = Utf8PathBuf::from(lib_path);
    let apidocs_path = output_path.join("apidocs");
    generate_api_docs(&lib_source, &apidocs_path, "../")?;

    // 2. Generate prose docs in docs/ subdirectory
    let docs_source = Utf8PathBuf::from(docs_path);
    generate_prose_docs(&docs_source, &output_path, PROSE_PAGES)?;

    // 3. Generate landing page
    write_site_landing_page(&output_path, PROSE_PAGES)?;

    // 4. Write shared CSS to root (prose pages and landing page reference it)
    write_css(&output_path)?;

    println!("  Site root: {output_path}/");
    Ok(())
}

/// Generate API reference docs into the given output directory.
///
/// `asset_prefix` is prepended to cross-site navigation links in the sidebar
/// (e.g., `"../"` when generating into a subdirectory like `apidocs/`).
fn generate_api_docs(
    source_path: &Utf8Path,
    output_path: &Utf8Path,
    asset_prefix: &str,
) -> Result<()> {
    // Find .bt source files
    let source_files = find_source_files(source_path)?;
    if source_files.is_empty() {
        miette::bail!("No .bt source files found in '{}'", source_path.as_str());
    }

    println!("Generating docs for {} file(s)...", source_files.len());

    // Parse all source files to extract class info
    let mut classes = Vec::new();
    for file in &source_files {
        if let Some(class_info) = parse_class_info(source_path, file)? {
            classes.push(class_info);
        }
    }

    // Sort classes alphabetically
    classes.sort_by(|a, b| a.name.cmp(&b.name));

    // Build class hierarchy map for inherited methods
    let hierarchy: HashMap<String, String> = classes
        .iter()
        .filter_map(|c| c.superclass.as_ref().map(|s| (c.name.clone(), s.clone())))
        .collect();

    // Collect all methods by class name for inheritance lookup
    let methods_by_class: HashMap<String, &ClassInfo> =
        classes.iter().map(|c| (c.name.clone(), c)).collect();

    // Create output directory
    fs::create_dir_all(output_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create output directory '{output_path}'"))?;

    // Generate CSS and JS assets
    write_css(output_path)?;
    write_search_js(output_path, &classes)?;

    // Build sidebar HTML (shared across all pages)
    let sidebar_html = build_sidebar_html(&classes, asset_prefix);

    // Generate index page (include README.md from source dir if present, per ADR 0008)
    let readme_path = source_path.join("README.md");
    let readme = fs::read_to_string(&readme_path).ok();
    write_index_page(output_path, &classes, readme.as_deref(), &sidebar_html)?;

    // Generate per-class pages
    for class in &classes {
        let inherited = collect_inherited_methods(class, &hierarchy, &methods_by_class);
        write_class_page(
            output_path,
            class,
            &inherited,
            &methods_by_class,
            &sidebar_html,
        )?;
    }

    info!(
        class_count = classes.len(),
        "Documentation generated successfully"
    );
    println!("Generated documentation for {} class(es)", classes.len());
    println!("  Output: {output_path}/");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    use camino::Utf8Path;
    use extractor::{find_source_files, parse_class_info};
    use renderer::{html_escape, method_anchor, render_doc};

    #[test]
    fn test_find_source_files_single_file() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("Test.bt");
        fs::write(&file, "// stub").unwrap();

        let files = find_source_files(&file).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_find_source_files_directory() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(dir.join("A.bt"), "// stub").unwrap();
        fs::write(dir.join("B.bt"), "// stub").unwrap();
        fs::write(dir.join("README.md"), "not bt").unwrap();

        let files = find_source_files(&dir).unwrap();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_find_source_files_sorted() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(dir.join("Zebra.bt"), "// stub").unwrap();
        fs::write(dir.join("Alpha.bt"), "// stub").unwrap();

        let files = find_source_files(&dir).unwrap();
        assert!(files[0].as_str() < files[1].as_str());
    }

    #[test]
    fn test_find_source_files_nonexistent() {
        let result = find_source_files(Utf8Path::new("/nonexistent"));
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_class_info() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("Counter.bt");
        fs::write(
            &file,
            "/// A simple counter.\nActor subclass: Counter\n  increment => 1\n",
        )
        .unwrap();

        let info = parse_class_info(&dir, &file).unwrap().unwrap();
        assert_eq!(info.name, "Counter");
        assert_eq!(info.superclass.as_deref(), Some("Actor"));
        assert!(info.doc_comment.unwrap().contains("simple counter"));
        assert_eq!(info.methods.len(), 1);
        assert_eq!(info.methods[0].signature, "increment");
    }

    #[test]
    fn test_parse_class_info_no_class() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("empty.bt");
        fs::write(&file, "x := 42\n").unwrap();

        let info = parse_class_info(&dir, &file).unwrap();
        assert!(info.is_none());
    }

    #[test]
    fn test_html_escape() {
        assert_eq!(html_escape("<div>"), "&lt;div&gt;");
        assert_eq!(html_escape("a & b"), "a &amp; b");
        assert_eq!(html_escape("\"hi\""), "&quot;hi&quot;");
    }

    #[test]
    fn test_method_anchor() {
        assert_eq!(method_anchor("increment"), "increment");
        assert_eq!(method_anchor("+ other"), "plus-other");
        assert_eq!(method_anchor("- other"), "minus-other");
        assert_eq!(method_anchor("* other"), "star-other");
        assert_eq!(
            method_anchor("at: index put: value"),
            "at--index-put--value"
        );
        assert_eq!(method_anchor("/= other"), "slasheq-other");
        assert_eq!(method_anchor("<= other"), "lteq-other");
    }

    #[test]
    fn test_render_doc_basic() {
        let html = render_doc("Hello world.");
        assert!(html.contains("<p>Hello world.</p>"));
    }

    #[test]
    fn test_render_doc_code_block() {
        let html = render_doc("Example:\n```beamtalk\n3 + 4\n```");
        assert!(html.contains("<pre><code class=\"language-beamtalk\">"));
        // Content is syntax-highlighted, so check for the number spans
        assert!(html.contains("hl-number"));
    }

    #[test]
    fn test_render_doc_heading() {
        let html = render_doc("## Examples");
        assert!(html.contains("<h2>Examples</h2>"));
    }

    #[test]
    fn test_render_doc_inline_code() {
        let html = render_doc("use `foo` here");
        assert!(html.contains("<code>foo</code>"));
    }

    #[test]
    fn test_render_doc_table() {
        let md = "| A | B |\n|---|---|\n| 1 | 2 |";
        let html = render_doc(md);
        assert!(html.contains("<table>"));
        assert!(html.contains("</table>"));
        // Table renders with header and body rows
        assert!(html.contains("<thead>"));
        assert!(html.contains("<th>"));
        assert!(html.contains("<tbody>"));
    }

    #[test]
    fn test_render_doc_link() {
        let html = render_doc("[click](https://example.com)");
        assert!(html.contains("<a href=\"https://example.com\">click</a>"));
    }

    #[test]
    fn test_render_doc_bold_italic() {
        let html = render_doc("**bold** and *italic*");
        assert!(html.contains("<strong>bold</strong>"));
        assert!(html.contains("<em>italic</em>"));
    }

    #[test]
    fn test_render_doc_list() {
        let html = render_doc("- one\n- two");
        assert!(html.contains("<ul>"));
        assert!(html.contains("<li>one</li>"));
    }

    #[test]
    fn test_render_doc_sanitizes_raw_html() {
        let html = render_doc("<script>alert('xss')</script>");
        assert!(!html.contains("<script>"));
        assert!(html.contains("&lt;script&gt;"));
    }

    #[test]
    fn test_collect_inherited_methods() {
        let parent = ClassInfo {
            name: "Parent".into(),
            superclass: None,
            doc_comment: None,
            methods: vec![MethodInfo {
                signature: "parentMethod".into(),
                doc_comment: None,
                line_number: None,
            }],
            class_methods: vec![],
            source_file: None,
            source_root: None,
        };
        let child = ClassInfo {
            name: "Child".into(),
            superclass: Some("Parent".into()),
            doc_comment: None,
            methods: vec![],
            class_methods: vec![],
            source_file: None,
            source_root: None,
        };

        let hierarchy: HashMap<String, String> = [("Child".into(), "Parent".into())].into();
        let methods_by_class: HashMap<String, &ClassInfo> =
            [("Parent".into(), &parent), ("Child".into(), &child)].into();

        let inherited = collect_inherited_methods(&child, &hierarchy, &methods_by_class);
        assert_eq!(inherited.len(), 1);
        assert_eq!(inherited[0].0, "Parent");
        assert_eq!(inherited[0].1.len(), 1);
    }

    #[test]
    fn test_generate_full_docs() {
        let temp = TempDir::new().unwrap();
        let src_dir = Utf8PathBuf::from_path_buf(temp.path().join("src")).unwrap();
        let out_dir = Utf8PathBuf::from_path_buf(temp.path().join("out")).unwrap();
        fs::create_dir_all(&src_dir).unwrap();

        fs::write(
            src_dir.join("Counter.bt"),
            "/// A counter class.\nActor subclass: Counter\n  /// Increment by one.\n  increment => 1\n",
        )
        .unwrap();

        run(src_dir.as_str(), out_dir.as_str()).unwrap();

        // Verify output files exist
        assert!(out_dir.join("index.html").exists());
        assert!(out_dir.join("Counter.html").exists());
        assert!(out_dir.join("style.css").exists());
        assert!(out_dir.join("search.js").exists());

        // Verify content
        let index = fs::read_to_string(out_dir.join("index.html")).unwrap();
        assert!(index.contains("Counter"));
        assert!(index.contains("A counter class."));

        let class_page = fs::read_to_string(out_dir.join("Counter.html")).unwrap();
        assert!(class_page.contains("Counter"));
        assert!(class_page.contains("increment"));
        assert!(class_page.contains("Increment by one."));
        assert!(class_page.contains("Actor"));
    }

    #[test]
    fn test_generate_cross_references() {
        let temp = TempDir::new().unwrap();
        let src_dir = Utf8PathBuf::from_path_buf(temp.path().join("src")).unwrap();
        let out_dir = Utf8PathBuf::from_path_buf(temp.path().join("out")).unwrap();
        fs::create_dir_all(&src_dir).unwrap();

        fs::write(
            src_dir.join("Parent.bt"),
            "Object subclass: Parent\n  base => 1\n",
        )
        .unwrap();
        fs::write(
            src_dir.join("Child.bt"),
            "Parent subclass: Child\n  extra => 2\n",
        )
        .unwrap();

        run(src_dir.as_str(), out_dir.as_str()).unwrap();

        let child_page = fs::read_to_string(out_dir.join("Child.html")).unwrap();
        // Should link to Parent
        assert!(child_page.contains("Parent.html"));
    }

    #[test]
    fn test_generate_site() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().join("lib")).unwrap();
        let docs_dir = Utf8PathBuf::from_path_buf(temp.path().join("docs")).unwrap();
        let out_dir = Utf8PathBuf::from_path_buf(temp.path().join("site")).unwrap();
        fs::create_dir_all(&lib_dir).unwrap();
        fs::create_dir_all(&docs_dir).unwrap();

        fs::write(
            lib_dir.join("Counter.bt"),
            "/// A counter.\nActor subclass: Counter\n  increment => 1\n",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-language-features.md"),
            "# Language Features\n\nBeamtalk syntax overview.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-principles.md"),
            "# Design Principles\n\nCore philosophy.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-architecture.md"),
            "# Architecture\n\nCompiler pipeline.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-agent-native-development.md"),
            "# Agent-Native Development\n\nAI agents.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-syntax-rationale.md"),
            "# Syntax Rationale\n\nWhy Beamtalk syntax diverges from Smalltalk.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-ddd-model.md"),
            "# Domain Model\n\nDDD bounded contexts.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("beamtalk-security.md"),
            "# Security\n\nSecurity model.",
        )
        .unwrap();
        fs::write(
            docs_dir.join("known-limitations.md"),
            "# Known Limitations\n\nWhat's not yet supported.",
        )
        .unwrap();

        run_site(lib_dir.as_str(), docs_dir.as_str(), out_dir.as_str()).unwrap();

        // Landing page exists
        let landing = fs::read_to_string(out_dir.join("index.html")).unwrap();
        assert!(landing.contains("Beamtalk"));
        assert!(landing.contains("apidocs/"));
        assert!(landing.contains("docs/language-features.html"));

        // API docs in subdirectory
        assert!(out_dir.join("apidocs/index.html").exists());
        assert!(out_dir.join("apidocs/Counter.html").exists());
        assert!(out_dir.join("apidocs/style.css").exists());

        // API docs have cross-navigation back to home
        let api_index = fs::read_to_string(out_dir.join("apidocs/index.html")).unwrap();
        assert!(api_index.contains("../"));

        // Prose docs
        assert!(out_dir.join("docs/language-features.html").exists());
        assert!(out_dir.join("docs/principles.html").exists());
        assert!(out_dir.join("docs/known-limitations.html").exists());

        let prose = fs::read_to_string(out_dir.join("docs/language-features.html")).unwrap();
        assert!(prose.contains("Language Features"));
        assert!(prose.contains("../style.css"));
        assert!(prose.contains("../apidocs/"));

        // Root CSS
        assert!(out_dir.join("style.css").exists());
    }
}
