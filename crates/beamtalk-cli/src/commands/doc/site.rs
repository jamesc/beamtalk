// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Prose documentation, ADR, and site landing page generation.
//!
//! **DDD Context:** CLI / Documentation

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write as _;
use std::fs;
use tracing::debug;

use super::highlighter::highlight_beamtalk;
use super::layout::{page_footer_simple, page_header};
use super::renderer::{html_escape, render_doc_trusted};

// ---------------------------------------------------------------------------
// Prose docs
// ---------------------------------------------------------------------------

/// Generate prose documentation pages from markdown files.
///
/// `extra_links` is a list of `(source_fragment, dest_html)` pairs used to
/// rewrite additional cross-references (e.g. ADR links) before rendering.
pub(super) fn generate_prose_docs(
    docs_source: &Utf8Path,
    site_root: &Utf8Path,
    prose_pages: &[(&str, &str, &str)],
    extra_links: &[(String, String)],
    learning_available: bool,
) -> Result<()> {
    let docs_output = site_root.join("docs");
    fs::create_dir_all(&docs_output)
        .into_diagnostic()
        .wrap_err("Failed to create docs/ output directory")?;

    // Verify all prose docs exist before generating (avoid broken navigation)
    let missing: Vec<&str> = prose_pages
        .iter()
        .filter(|&&(source_file, _, _)| !docs_source.join(source_file).exists())
        .map(|&(source_file, _, _)| source_file)
        .collect();
    if !missing.is_empty() {
        miette::bail!(
            "Missing prose docs in '{}': {}",
            docs_source.as_str(),
            missing.join(", ")
        );
    }

    render_docs_index(prose_pages, &docs_output, learning_available)?;

    let mut rendered_count = 0;
    for &(source_file, output_file, title) in prose_pages {
        let source = docs_source.join(source_file);

        let markdown = fs::read_to_string(&source)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read '{source}'"))?;

        // Rewrite cross-references to sibling prose docs and ADRs
        let markdown = rewrite_prose_links(&markdown, prose_pages, extra_links);

        let page_title = format!("{title} — Beamtalk");
        let mut html = String::new();
        html.push_str(&page_header(&page_title, "../style.css", "../"));
        html.push_str("<div class=\"page-wrapper\">\n");
        html.push_str(SIDEBAR_TOGGLE);
        html.push_str(&prose_nav(output_file, prose_pages, learning_available));
        html.push_str("<main class=\"main-content prose-content\">\n");
        html.push_str("<div class=\"breadcrumb\">");
        html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
        html.push_str("<a href=\"index.html\">Documentation</a> &rsaquo; ");
        html.push_str(&html_escape(title));
        html.push_str("</div>\n");
        html.push_str(&render_doc_trusted(&markdown));
        html.push_str("</main>\n");
        html.push_str(&page_footer_simple());

        let out_path = docs_output.join(output_file);
        fs::write(&out_path, html)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write {output_file}"))?;
        debug!("Generated {out_path}");
        rendered_count += 1;
    }

    println!("Generated {rendered_count} prose documentation page(s)");
    Ok(())
}

/// Render the docs index page listing all prose documentation pages.
fn render_docs_index(
    prose_pages: &[(&str, &str, &str)],
    docs_output: &Utf8Path,
    learning_available: bool,
) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header(
        "Documentation — Beamtalk",
        "../style.css",
        "../",
    ));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&prose_nav("index.html", prose_pages, learning_available));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("Documentation");
    html.push_str("</div>\n");
    html.push_str("<h1>Documentation</h1>\n");
    html.push_str("<ul class=\"docs-index-list\">\n");
    for &(_, file, title) in prose_pages {
        let (_, desc) = landing_card_meta(file);
        if desc.is_empty() {
            let _ = writeln!(
                html,
                "<li><a href=\"{file}\">{title}</a></li>",
                file = file,
                title = html_escape(title)
            );
        } else {
            let _ = writeln!(
                html,
                "<li><a href=\"{file}\">{title}</a> — {desc}</li>",
                file = file,
                title = html_escape(title),
                desc = html_escape(desc)
            );
        }
    }
    html.push_str("</ul>\n");
    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let index_path = docs_output.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write docs/index.html")?;
    debug!("Generated {index_path}");
    Ok(())
}

/// Build navigation sidebar for prose documentation pages.
fn prose_nav(
    active_file: &str,
    prose_pages: &[(&str, &str, &str)],
    learning_available: bool,
) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");

    html.push_str("<div class=\"sidebar-section-label\">Navigate</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    html.push_str("<li><a href=\"../\">Home</a></li>\n");
    if learning_available {
        html.push_str("<li><a href=\"../learning/\">Learn Beamtalk</a></li>\n");
    }
    html.push_str("<li><a href=\"../docs/\">Documentation</a></li>\n");
    html.push_str("<li><a href=\"../apidocs/\">API Reference</a></li>\n");
    html.push_str("<li><a href=\"../adr/\">Architecture Decisions</a></li>\n");
    html.push_str("</ul>\n");

    html.push_str("<div class=\"sidebar-section-label\">Documentation</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    for &(_, file, title) in prose_pages {
        let active = if file == active_file {
            " class=\"active\""
        } else {
            ""
        };
        let _ = writeln!(html, "<li><a href=\"{file}\"{active}>{title}</a></li>");
    }
    html.push_str("</ul>\n</nav>\n");
    html
}

/// Rewrite cross-references between prose docs from `.md` to `.html`.
///
/// When the source filename is used as both the link text and href
/// (e.g. `[beamtalk-foo.md](beamtalk-foo.md)`), the display text is replaced
/// with the human-readable page title instead of leaving `.html` visible.
///
/// Also applies `extra_links` rewrites (e.g. ADR source paths → rendered URLs).
fn rewrite_prose_links(
    markdown: &str,
    prose_pages: &[(&str, &str, &str)],
    extra_links: &[(String, String)],
) -> String {
    let mut result = markdown.to_string();
    for &(source_file, output_file, title) in prose_pages {
        // Replace full markdown links first so the display text becomes the title
        let md_link = format!("[{source_file}]({source_file})");
        let titled_link = format!("[{title}]({output_file})");
        result = result.replace(&md_link, &titled_link);
        // Replace bare filename references only outside [..] brackets so the
        // rewrite targets hrefs but not visible link text.
        result = replace_outside_md_brackets(&result, source_file, output_file);
    }
    // Apply extra_links (e.g. ADR paths and ADR number references) only outside
    // fenced code blocks, to avoid corrupting code examples.
    for (source, dest) in extra_links {
        result = rewrite_outside_code_fences(&result, source, dest);
    }
    result
}

/// Replace `source` with `dest` in `text`, skipping fenced code blocks and
/// existing markdown link brackets `[...]` (to avoid creating nested links).
fn rewrite_outside_code_fences(text: &str, source: &str, dest: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut in_fence = false;
    let mut lines = text.split('\n').peekable();
    while let Some(line) = lines.next() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("```") {
            in_fence = !in_fence;
            out.push_str(line);
        } else if in_fence {
            out.push_str(line);
        } else {
            out.push_str(&replace_outside_md_brackets(line, source, dest));
        }
        // Only re-insert the separator between lines, so a string that ends
        // (or doesn't end) in '\n' round-trips exactly when nothing matched.
        if lines.peek().is_some() {
            out.push('\n');
        }
    }
    out
}

/// Replace `source` with `dest` in `line`, skipping text inside `[...]` brackets
/// so that existing markdown links are not corrupted.
fn replace_outside_md_brackets(line: &str, source: &str, dest: &str) -> String {
    let mut out = String::with_capacity(line.len() + dest.len());
    let src = source.as_bytes();
    let src_len = source.len();
    let bytes = line.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    let mut depth = 0i32;
    while i < len {
        if bytes[i] == b'[' {
            depth += 1;
            out.push('[');
            i += 1;
        } else if bytes[i] == b']' {
            depth = (depth - 1).max(0);
            out.push(']');
            i += 1;
        } else if depth == 0 && i + src_len <= len && &bytes[i..i + src_len] == src {
            out.push_str(dest);
            i += src_len;
        } else {
            // Advance one Unicode code point
            let c_start = i;
            i += 1;
            while i < len && !line.is_char_boundary(i) {
                i += 1;
            }
            out.push_str(&line[c_start..i]);
        }
    }
    out
}

// ---------------------------------------------------------------------------
// ADR generation
// ---------------------------------------------------------------------------

/// Metadata for a single Architecture Decision Record.
struct AdrInfo {
    /// Zero-padded number string, e.g. `"0001"`.
    number: String,
    /// Full stem of the source file, e.g. `"0001-no-compound-assignment"`.
    slug: String,
    /// Human title extracted from the H1, e.g. `"No Compound Assignment in Beamtalk"`.
    title: String,
    /// Status line extracted from the `## Status` section, e.g. `"Implemented (2026-02-08)"`.
    status: String,
    /// Output HTML filename, e.g. `"0001-no-compound-assignment.html"`.
    output_file: String,
}

/// Generate ADR pages from `docs/ADR/*.md` files.
///
/// Returns a list of `(source_fragment, dest_html)` link rewriting pairs for
/// use in prose doc rendering, e.g.:
/// `("ADR/0004-persistent-workspace-management.md",
///   "../adr/0004-persistent-workspace-management.html")`.
///
/// Returns an empty vec (and does nothing) if the ADR directory does not exist.
pub(super) fn generate_adr_docs(
    docs_source: &Utf8Path,
    site_root: &Utf8Path,
    learning_available: bool,
) -> Result<Vec<(String, String)>> {
    let adr_source = docs_source.join("ADR");
    if !adr_source.exists() {
        return Ok(Vec::new());
    }

    let adr_output = site_root.join("adr");
    fs::create_dir_all(&adr_output)
        .into_diagnostic()
        .wrap_err("Failed to create adr/ output directory")?;

    let adrs = discover_adrs(&adr_source)?;
    if adrs.is_empty() {
        return Ok(Vec::new());
    }

    // Build intra-ADR "ADR NNNN" → link pairs (same directory — no ../adr/ prefix).
    let intra_links: Vec<(String, String)> = adrs
        .iter()
        .map(|a| {
            (
                format!("ADR {}", a.number),
                format!("[ADR {}]({})", a.number, a.output_file),
            )
        })
        .collect();

    // Render each ADR page
    for adr in &adrs {
        let source_path = adr_source.join(format!("{}.md", adr.slug));
        let content = fs::read_to_string(&source_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read ADR '{}'", adr.slug))?;
        // Rewrite within-ADR links: sibling .md → .html (same directory)
        let mut content = rewrite_adr_internal_links(&content, &adrs);
        // Rewrite "ADR NNNN" references to links (outside code fences and brackets)
        for (source, dest) in &intra_links {
            content = rewrite_outside_code_fences(&content, source, dest);
        }
        render_adr_page(adr, &adrs, &content, &adr_output, learning_available)?;
    }

    // Render ADR index
    render_adr_index(&adrs, &adr_output, learning_available)?;

    println!("Generated {} ADR page(s)", adrs.len());

    // Return link-rewriting pairs for prose doc rendering.
    // Prose pages live in /docs/, so the relative path to /adr/ is ../adr/.
    // Two pairs per ADR:
    //   1. Full filename (e.g. "ADR/0004-slug.md") → relative HTML path
    //   2. Short reference (e.g. "ADR 0004") → markdown link with title
    let mut links = Vec::new();
    for a in &adrs {
        links.push((
            format!("ADR/{}.md", a.slug),
            format!("../adr/{}", a.output_file),
        ));
        links.push((
            format!("ADR {}", a.number),
            format!("[ADR {}](../adr/{})", a.number, a.output_file),
        ));
    }
    Ok(links)
}

/// Discover and parse ADR files from the given directory.
fn discover_adrs(adr_source: &Utf8Path) -> Result<Vec<AdrInfo>> {
    let mut adrs: Vec<AdrInfo> = fs::read_dir(adr_source)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read ADR directory '{adr_source}'"))?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = Utf8PathBuf::from_path_buf(entry.path()).ok()?;
            let name = path.file_name()?.to_string();
            // Skip non-markdown and the template
            if path.extension() != Some("md") || name == "TEMPLATE.md" {
                return None;
            }
            let stem = path.file_stem()?.to_string();
            // Must start with digits (NNNN-)
            let number: String = stem.chars().take_while(char::is_ascii_digit).collect();
            if number.is_empty() {
                return None;
            }
            let content = fs::read_to_string(&path).ok()?;
            let title = extract_adr_title(&content);
            let status = extract_adr_status(&content);
            Some(AdrInfo {
                number,
                slug: stem,
                title,
                status,
                output_file: format!("{}.html", path.file_stem()?),
            })
        })
        .collect();

    adrs.sort_by(|a, b| a.number.cmp(&b.number));
    Ok(adrs)
}

/// Extract the human title from an ADR's first H1 heading.
///
/// Strips the `ADR NNNN: ` prefix if present, e.g.:
/// `# ADR 0001: No Compound Assignment` → `"No Compound Assignment"`.
fn extract_adr_title(content: &str) -> String {
    for line in content.lines() {
        if let Some(rest) = line.strip_prefix("# ") {
            // Strip "ADR NNNN: " prefix (case-insensitive check)
            let rest_lower = rest.to_ascii_lowercase();
            if rest_lower.starts_with("adr ") {
                if let Some(colon) = rest.find(": ") {
                    return rest[colon + 2..].trim().to_string();
                }
            }
            return rest.trim().to_string();
        }
    }
    String::from("Untitled ADR")
}

/// Extract the status from an ADR's `## Status` section (first non-empty line after it).
fn extract_adr_status(content: &str) -> String {
    let mut in_status = false;
    for line in content.lines() {
        if line.trim() == "## Status" {
            in_status = true;
            continue;
        }
        if in_status {
            if line.trim_start().starts_with('#') {
                break;
            }
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                // Return only up to the first " — " to keep it concise
                return trimmed.split(" — ").next().unwrap_or(trimmed).to_string();
            }
        }
    }
    String::from("Unknown")
}

/// Rewrite sibling ADR links within an ADR page (`.md` → `.html`, same dir).
fn rewrite_adr_internal_links(content: &str, adrs: &[AdrInfo]) -> String {
    let mut result = content.to_string();
    for adr in adrs {
        let md = format!("{}.md", adr.slug);
        result = result.replace(&md, &adr.output_file);
    }
    result
}

/// Render a single ADR page.
fn render_adr_page(
    adr: &AdrInfo,
    all_adrs: &[AdrInfo],
    content: &str,
    adr_output: &Utf8Path,
    learning_available: bool,
) -> Result<()> {
    let page_title = format!("ADR {} — Beamtalk", adr.number);
    let mut html = String::new();
    html.push_str(&page_header(&page_title, "../style.css", "../"));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&adr_nav(&adr.output_file, all_adrs, learning_available));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("<a href=\"index.html\">Architecture Decisions</a> &rsaquo; ");
    let _ = write!(html, "ADR {}", html_escape(&adr.number));
    html.push_str("</div>\n");
    html.push_str(&render_doc_trusted(content));
    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let out_path = adr_output.join(&adr.output_file);
    fs::write(&out_path, html)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write {}", adr.output_file))?;
    debug!("Generated {out_path}");
    Ok(())
}

/// Render the ADR index page listing all decisions.
fn render_adr_index(
    adrs: &[AdrInfo],
    adr_output: &Utf8Path,
    learning_available: bool,
) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header(
        "Architecture Decisions — Beamtalk",
        "../style.css",
        "../",
    ));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&adr_nav("index.html", adrs, learning_available));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("Architecture Decisions");
    html.push_str("</div>\n");
    html.push_str("<h1>Architecture Decisions</h1>\n");
    html.push_str(
        "<p>Key design decisions with context, alternatives considered, \
         and consequences.</p>\n",
    );
    html.push_str(
        "<table>\n\
         <colgroup>\
         <col style=\"width:5ch\">\
         <col>\
         <col style=\"width:18ch\">\
         </colgroup>\n\
         <thead>\n<tr><th>#</th><th>Decision</th><th>Status</th></tr>\n</thead>\n<tbody>\n",
    );
    for adr in adrs {
        let _ = writeln!(
            html,
            "<tr><td class=\"adr-num\">{num}</td><td><a href=\"{file}\">{title}</a></td><td>{status}</td></tr>",
            file = adr.output_file,
            num = html_escape(&adr.number),
            title = html_escape(&adr.title),
            status = html_escape(&adr.status),
        );
    }
    html.push_str("</tbody>\n</table>\n");
    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let index_path = adr_output.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write adr/index.html")?;
    debug!("Generated {index_path}");
    Ok(())
}

/// Build the sidebar navigation for ADR pages.
fn adr_nav(active_file: &str, adrs: &[AdrInfo], learning_available: bool) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");

    html.push_str("<div class=\"sidebar-section-label\">Navigate</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    html.push_str("<li><a href=\"../\">Home</a></li>\n");
    if learning_available {
        html.push_str("<li><a href=\"../learning/\">Learn Beamtalk</a></li>\n");
    }
    html.push_str("<li><a href=\"../docs/\">Documentation</a></li>\n");
    html.push_str("<li><a href=\"../apidocs/\">API Reference</a></li>\n");
    html.push_str("<li><a href=\"../adr/\">Architecture Decisions</a></li>\n");
    html.push_str("</ul>\n");

    html.push_str("<div class=\"sidebar-section-label\">Architecture Decisions</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    let index_active = if active_file == "index.html" {
        " class=\"active\""
    } else {
        ""
    };
    let _ = writeln!(
        html,
        "<li><a href=\"index.html\"{index_active}>All ADRs</a></li>"
    );

    for adr in adrs {
        let active = if adr.output_file == active_file {
            " class=\"active\""
        } else {
            ""
        };
        let _ = writeln!(
            html,
            "<li><a href=\"{file}\"{active}>{num} — {title}</a></li>",
            file = adr.output_file,
            num = html_escape(&adr.number),
            title = html_escape(&adr.title),
        );
    }
    html.push_str("</ul>\n</nav>\n");
    html
}

// ---------------------------------------------------------------------------
// Learning guide
// ---------------------------------------------------------------------------

/// Metadata for a single learning guide chapter.
struct ChapterInfo {
    /// Chapter number string, e.g. `"01"`.
    number: String,
    /// Full stem of the source file, e.g. `"01-getting-started"`.
    slug: String,
    /// Human title extracted from the H1, e.g. `"Getting Started"`.
    title: String,
    /// Output HTML filename, e.g. `"01-getting-started.html"`.
    output_file: String,
}

/// Generate learning guide pages from `docs/learning/*.md` files.
///
/// Returns immediately (no-op) if `docs/learning/` does not exist.
/// Returns `true` if any chapters were generated, `false` otherwise.
pub(super) fn generate_learning_guide(
    docs_source: &Utf8Path,
    site_root: &Utf8Path,
) -> Result<bool> {
    let learning_source = docs_source.join("learning");
    if !learning_source.exists() {
        return Ok(false);
    }

    let learning_output = site_root.join("learning");
    fs::create_dir_all(&learning_output)
        .into_diagnostic()
        .wrap_err("Failed to create learning/ output directory")?;

    let chapters = discover_chapters(&learning_source)?;
    if chapters.is_empty() {
        return Ok(false);
    }

    // Render each chapter page
    for chapter in &chapters {
        let source_path = learning_source.join(format!("{}.md", chapter.slug));
        let content = fs::read_to_string(&source_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read chapter '{}'", chapter.slug))?;
        // Rewrite sibling chapter links (.md → .html, same directory)
        let content = rewrite_chapter_internal_links(&content, &chapters);
        render_chapter_page(chapter, &chapters, &content, &learning_output)?;
    }

    // Render chapter index
    render_learning_index(&chapters, &learning_source, &learning_output)?;

    println!("Generated {} learning guide chapter(s)", chapters.len());
    Ok(true)
}

/// Discover and sort chapter files from the learning directory.
fn discover_chapters(learning_source: &Utf8Path) -> Result<Vec<ChapterInfo>> {
    let mut chapters = Vec::new();
    for entry in fs::read_dir(learning_source)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read learning directory '{learning_source}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path()).map_err(|p| {
            miette::miette!("Non-UTF-8 path in learning directory: {}", p.display())
        })?;
        if path.extension() != Some("md") {
            continue;
        }
        let Some(stem) = path.file_stem().map(ToString::to_string) else {
            continue;
        };
        // Skip README and files not starting with digits (NN-)
        let number: String = stem.chars().take_while(char::is_ascii_digit).collect();
        if number.is_empty() {
            continue;
        }
        let content = fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read chapter '{path}'"))?;
        chapters.push(ChapterInfo {
            number,
            slug: stem.clone(),
            title: extract_chapter_title(&content),
            output_file: format!("{stem}.html"),
        });
    }
    chapters.sort_by(|a, b| a.slug.cmp(&b.slug));
    Ok(chapters)
}

/// Extract the human title from a chapter's first heading (any level: `#`, `##`, …).
fn extract_chapter_title(content: &str) -> String {
    for line in content.lines() {
        let stripped = line.trim_start_matches('#');
        if stripped.len() < line.len() && stripped.starts_with(' ') {
            return stripped.trim().to_string();
        }
    }
    String::from("Untitled Chapter")
}

/// Rewrite sibling chapter links within a chapter page (.md → .html, same dir).
fn rewrite_chapter_internal_links(content: &str, chapters: &[ChapterInfo]) -> String {
    let mut result = content.to_string();
    for chapter in chapters {
        let md = format!("{}.md", chapter.slug);
        result = result.replace(&md, &chapter.output_file);
    }
    result
}

/// Render the learning guide index page from `docs/learning/README.md`.
///
/// If `README.md` exists, its content is rendered as the index page body (with
/// internal `.md` links rewritten to `.html`). Otherwise, falls back to a simple
/// ordered list of chapter links.
fn render_learning_index(
    chapters: &[ChapterInfo],
    learning_source: &Utf8Path,
    learning_output: &Utf8Path,
) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header(
        "Learn Beamtalk — Beamtalk",
        "../style.css",
        "../",
    ));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&learning_nav("index.html", chapters));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("Learn Beamtalk");
    html.push_str("</div>\n");

    let readme_path = learning_source.join("README.md");
    if readme_path.exists() {
        let content = fs::read_to_string(&readme_path)
            .into_diagnostic()
            .wrap_err("Failed to read learning/README.md")?;
        let content = rewrite_chapter_internal_links(&content, chapters);
        // Rewrite cross-doc links: README uses repo-relative paths like
        // ../beamtalk-language-features.md that need to point to the rendered
        // docs/ pages on the site.
        let content = content
            .replace(
                "../beamtalk-language-features.md",
                "../docs/language-features.html",
            )
            .replace(
                "../beamtalk-syntax-rationale.md",
                "../docs/syntax-rationale.html",
            );
        html.push_str(&render_doc_trusted(&content));
    } else {
        html.push_str("<h1>Learn Beamtalk</h1>\n");
        html.push_str(
            "<p>A progressive guide to the Beamtalk language. \
             Read linearly — each chapter builds on the last.</p>\n",
        );
        html.push_str("<ol>\n");
        for chapter in chapters {
            let _ = writeln!(
                html,
                "<li><a href=\"{file}\">{title}</a></li>",
                file = chapter.output_file,
                title = html_escape(&chapter.title),
            );
        }
        html.push_str("</ol>\n");
    }

    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let index_path = learning_output.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write learning/index.html")?;
    debug!("Generated {index_path}");
    Ok(())
}

/// Build the prev / up / next navigation bar for a chapter page.
fn chapter_nav(prev: Option<&ChapterInfo>, next: Option<&ChapterInfo>) -> String {
    let mut html = String::from("<nav class=\"chapter-nav\">\n");

    if let Some(p) = prev {
        let _ = writeln!(
            html,
            "<a class=\"chapter-nav-prev\" href=\"{file}\">← {title}</a>",
            file = p.output_file,
            title = html_escape(&p.title),
        );
    } else {
        html.push_str("<span class=\"chapter-nav-placeholder\"></span>\n");
    }

    html.push_str("<a class=\"chapter-nav-up\" href=\"index.html\">↑ Contents</a>\n");

    if let Some(n) = next {
        let _ = writeln!(
            html,
            "<a class=\"chapter-nav-next\" href=\"{file}\">{title} →</a>",
            file = n.output_file,
            title = html_escape(&n.title),
        );
    } else {
        html.push_str("<span class=\"chapter-nav-placeholder\"></span>\n");
    }

    html.push_str("</nav>\n");
    html
}

/// Render a single chapter page.
fn render_chapter_page(
    chapter: &ChapterInfo,
    all_chapters: &[ChapterInfo],
    content: &str,
    learning_output: &Utf8Path,
) -> Result<()> {
    let idx = all_chapters
        .iter()
        .position(|c| c.output_file == chapter.output_file)
        .unwrap_or(0);
    let prev = idx.checked_sub(1).map(|i| &all_chapters[i]);
    let next = all_chapters.get(idx + 1);

    let page_title = format!("{} — Beamtalk", chapter.title);
    let mut html = String::new();
    html.push_str(&page_header(&page_title, "../style.css", "../"));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&learning_nav(&chapter.output_file, all_chapters));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("<a href=\"index.html\">Learn Beamtalk</a> &rsaquo; ");
    html.push_str(&html_escape(&chapter.title));
    html.push_str("</div>\n");
    html.push_str(&chapter_nav(prev, next));
    html.push_str(&render_doc_trusted(content));
    html.push_str(&chapter_nav(prev, next));
    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let out_path = learning_output.join(&chapter.output_file);
    fs::write(&out_path, html)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write {}", chapter.output_file))?;
    debug!("Generated {out_path}");
    Ok(())
}

/// Build the sidebar navigation for learning guide pages.
fn learning_nav(active_file: &str, chapters: &[ChapterInfo]) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");

    html.push_str("<div class=\"sidebar-section-label\">Navigate</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    html.push_str("<li><a href=\"../\">Home</a></li>\n");
    html.push_str("<li><a href=\"../learning/\">Learn Beamtalk</a></li>\n");
    html.push_str("<li><a href=\"../docs/\">Documentation</a></li>\n");
    html.push_str("<li><a href=\"../apidocs/\">API Reference</a></li>\n");
    html.push_str("<li><a href=\"../adr/\">Architecture Decisions</a></li>\n");
    html.push_str("</ul>\n");

    html.push_str("<div class=\"sidebar-section-label\">Learn Beamtalk</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    let index_active = if active_file == "index.html" {
        " class=\"active\""
    } else {
        ""
    };
    let _ = writeln!(
        html,
        "<li><a href=\"index.html\"{index_active}>All Chapters</a></li>"
    );
    for chapter in chapters {
        let active = if chapter.output_file == active_file {
            " class=\"active\""
        } else {
            ""
        };
        let _ = writeln!(
            html,
            "<li><a href=\"{file}\"{active}>{num} — {title}</a></li>",
            file = chapter.output_file,
            num = html_escape(&chapter.number),
            title = html_escape(&chapter.title),
        );
    }
    html.push_str("</ul>\n</nav>\n");
    html
}

// ---------------------------------------------------------------------------
// Landing page
// ---------------------------------------------------------------------------

/// Sidebar toggle button + tap-to-close overlay (shared across page types).
const SIDEBAR_TOGGLE: &str = "\
<button class=\"sidebar-toggle\" \
  onclick=\"var s=document.querySelector('.sidebar'),o=document.getElementById('sidebar-overlay');\
s.classList.toggle('open');o.classList.toggle('active');\" \
  aria-label=\"Toggle navigation\">☰</button>\n\
<div id=\"sidebar-overlay\" class=\"sidebar-overlay\" \
  onclick=\"document.querySelector('.sidebar').classList.remove('open');\
this.classList.remove('active');\"></div>\n";

/// Return (emoji, description) for a prose doc card on the landing page.
///
/// Keyed by `output_file` (the stable `.html` filename) rather than display
/// title so that renaming a title in `PROSE_PAGES` never silently drops card
/// descriptions.
fn landing_card_meta(output_file: &str) -> (&'static str, &'static str) {
    match output_file {
        "installation.html" => (
            "",
            "Install the Beamtalk toolchain on Linux, macOS, or Windows and set up the VS Code extension.",
        ),
        "language-features.html" => (
            "🔤",
            "Syntax, semantics, and worked examples for the message-based programming model.",
        ),
        "principles.html" => (
            "🧭",
            "The core principles guiding all design and implementation decisions.",
        ),
        "architecture.html" => (
            "🏗",
            "Compiler pipeline, runtime, hot code reload, and live development flow.",
        ),
        "agent-native-development.html" => (
            "🤖",
            "Why Beamtalk is uniquely suited as a development environment for AI agents.",
        ),
        "syntax-rationale.html" => (
            "💬",
            "Why Beamtalk keeps certain Smalltalk conventions and diverges from others.",
        ),
        "ddd-model.html" => (
            "🗺",
            "Bounded contexts, aggregates, and ubiquitous language of the project.",
        ),
        "security.html" => (
            "🔒",
            "Security model, threat analysis, and sandboxing for untrusted code.",
        ),
        "tooling.html" => ("🛠", "CLI, REPL, VS Code extension, and testing framework."),
        "known-limitations.html" => (
            "⚠️",
            "Current limitations and unimplemented features to be aware of.",
        ),
        _ => ("📄", ""),
    }
}

/// Return the learning guide landing card description.
fn learning_card_desc() -> (&'static str, &'static str) {
    (
        "",
        "A progressive, chapter-by-chapter guide to learning Beamtalk from first principles.",
    )
}

/// Beamtalk code snippet shown on the landing page.
const LANDING_CODE_SNIPPET: &str = "Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  value => self.value

c := Counter spawn
c increment
c increment
c value // => 2";

/// Generate the site landing page at the root.
///
/// `learning_available` controls whether a "Learn Beamtalk" card is emitted
/// (only true when `generate_learning_guide` successfully produced chapters).
pub(super) fn write_site_landing_page(
    output_path: &Utf8Path,
    prose_pages: &[(&str, &str, &str)],
    learning_available: bool,
) -> Result<()> {
    let highlighted_code = highlight_beamtalk(LANDING_CODE_SNIPPET);

    let mut html = String::new();
    html.push_str(&page_header("Beamtalk", "style.css", "./"));
    html.push_str("<div class=\"page-wrapper landing-wrapper\">\n");
    html.push_str("<main class=\"landing-content\">\n");

    // Hero: two-column — left: text + CTA, right: code window
    html.push_str("<div class=\"landing-hero\">\n");

    // Left column
    html.push_str("<div class=\"landing-hero-text\">\n");
    html.push_str(
        "<picture class=\"landing-logo\">\
         <source srcset=\"images/beamtalk-logo-dark.svg\" media=\"(prefers-color-scheme: dark)\">\
         <img src=\"images/beamtalk-logo-light.svg\" alt=\"Beamtalk\" height=\"64\">\
         </picture>\n",
    );
    html.push_str(
        "<p class=\"landing-tagline\">A live, message-based language built on the \
         BEAM VM. Smalltalk semantics, Erlang reliability, compiled to native \
         bytecode.</p>\n",
    );
    html.push_str("<div class=\"landing-cta\">\n");
    html.push_str("<a href=\"docs/installation.html\" class=\"btn-primary\">Get started</a>\n");
    html.push_str("<a href=\"apidocs/\" class=\"btn-secondary\">API Reference</a>\n");
    html.push_str("</div>\n"); // .landing-cta
    html.push_str("</div>\n"); // .landing-hero-text

    // Right column: code window
    html.push_str("<div class=\"landing-code-window\">\n");
    html.push_str("<div class=\"code-window-bar\">\n");
    html.push_str("<span class=\"code-dot code-dot-r\"></span>\n");
    html.push_str("<span class=\"code-dot code-dot-y\"></span>\n");
    html.push_str("<span class=\"code-dot code-dot-g\"></span>\n");
    html.push_str("<span class=\"code-window-title\">counter.bt</span>\n");
    html.push_str("</div>\n"); // .code-window-bar
    html.push_str("<div class=\"code-window-body\">\n");
    html.push_str("<pre><code>");
    html.push_str(&highlighted_code);
    html.push_str("</code></pre>\n");
    html.push_str("</div>\n"); // .code-window-body
    html.push_str("</div>\n"); // .landing-code-window

    html.push_str("</div>\n"); // .landing-hero

    // Navigation cards
    html.push_str("<div class=\"landing-section-label\">Explore the docs</div>\n");
    html.push_str("<div class=\"landing-cards\">\n");

    // Learning guide card (shown first when available)
    if learning_available {
        let (_, desc) = learning_card_desc();
        let _ = writeln!(
            html,
            "<a href=\"learning/\" class=\"landing-card\">\n\
             <h2>Learn Beamtalk</h2>\n\
             <p>{desc}</p>\n\
             </a>"
        );
    }

    // Prose docs cards
    for &(_, file, title) in prose_pages {
        let (_, desc) = landing_card_meta(file);
        let _ = writeln!(
            html,
            "<a href=\"docs/{file}\" class=\"landing-card\">\n\
             <h2>{title}</h2>\n\
             <p>{desc}</p>\n\
             </a>"
        );
    }

    html.push_str("</div>\n"); // .landing-cards

    html.push_str("<div class=\"landing-links\">\n");
    html.push_str("<a href=\"https://github.com/jamesc/beamtalk\">View on GitHub</a>\n");
    html.push_str("</div>\n");

    html.push_str("</main>\n");
    html.push_str(&page_footer_simple());

    let index_path = output_path.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write site index.html")?;
    debug!("Generated site landing page");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn utf8(dir: &TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    // -----------------------------------------------------------------
    // Prose docs
    // -----------------------------------------------------------------

    const PAGES: &[(&str, &str, &str)] = &[
        ("a.md", "a.html", "Doc A"),
        ("installation.md", "installation.html", "Installation"),
    ];

    #[test]
    fn generate_prose_docs_renders_pages_and_index() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        fs::write(
            docs_source.join("a.md"),
            "# Doc A\n\nSee [installation.md](installation.md) and installation.md directly.\n",
        )
        .unwrap();
        fs::write(
            docs_source.join("installation.md"),
            "# Installation\n\nInstall it.\n",
        )
        .unwrap();

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        generate_prose_docs(&docs_source, &site_root, PAGES, &[], true).unwrap();

        let a_html = fs::read_to_string(site_root.join("docs/a.html")).unwrap();
        // Full markdown link retitled, bare filename reference rewritten too.
        assert!(a_html.contains("installation.html"));
        assert!(!a_html.contains("installation.md"));

        let index_html = fs::read_to_string(site_root.join("docs/index.html")).unwrap();
        assert!(index_html.contains("Doc A"));
        // installation.html has a landing_card_meta description, "a.html" does not.
        assert!(index_html.contains("Install the Beamtalk toolchain"));
        assert!(index_html.contains("<li><a href=\"a.html\">Doc A</a></li>"));
    }

    #[test]
    fn generate_prose_docs_errors_on_missing_source() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        // Only write one of the two pages.
        fs::write(docs_source.join("a.md"), "# Doc A\n").unwrap();

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        let err = generate_prose_docs(&docs_source, &site_root, PAGES, &[], false).unwrap_err();
        assert!(format!("{err:?}").contains("installation.md"));
    }

    #[test]
    fn prose_nav_reflects_active_page_and_learning_flag() {
        let with_learning = prose_nav("a.html", PAGES, true);
        assert!(with_learning.contains("Learn Beamtalk"));
        assert!(with_learning.contains("<a href=\"a.html\" class=\"active\">Doc A</a>"));

        let without_learning = prose_nav("installation.html", PAGES, false);
        assert!(!without_learning.contains("Learn Beamtalk"));
        assert!(
            without_learning
                .contains("<a href=\"installation.html\" class=\"active\">Installation</a>")
        );
    }

    #[test]
    fn rewrite_prose_links_retitles_full_links_and_bare_refs() {
        let markdown = "[installation.md](installation.md) then installation.md again.";
        let out = rewrite_prose_links(markdown, PAGES, &[]);
        assert!(out.contains("[Installation](installation.html)"));
        assert!(out.contains("installation.html again"));
        assert!(!out.contains("installation.md"));
    }

    #[test]
    fn rewrite_prose_links_applies_extra_links_outside_fences() {
        let markdown = "See ADR 0001.\n\n```text\nADR 0001 in code stays put\n```\n";
        let extra = vec![("ADR 0001".to_string(), "[ADR 1](0001.html)".to_string())];
        let out = rewrite_prose_links(markdown, &[], &extra);
        assert!(out.contains("See [ADR 1](0001.html)."));
        assert!(out.contains("ADR 0001 in code stays put"));
    }

    #[test]
    fn rewrite_outside_code_fences_skips_fenced_blocks() {
        let text = "before FOO\n```\nFOO inside fence\n```\nafter FOO";
        let out = rewrite_outside_code_fences(text, "FOO", "BAR");
        assert_eq!(out, "before BAR\n```\nFOO inside fence\n```\nafter BAR");
    }

    #[test]
    fn rewrite_outside_code_fences_preserves_trailing_newline_state() {
        let with_newline = "FOO\n";
        assert_eq!(
            rewrite_outside_code_fences(with_newline, "FOO", "BAR"),
            "BAR\n"
        );
        let without_newline = "FOO";
        assert_eq!(
            rewrite_outside_code_fences(without_newline, "FOO", "BAR"),
            "BAR"
        );
    }

    #[test]
    fn replace_outside_md_brackets_skips_link_text_but_not_href() {
        let line = "[FOO](FOO) and bare FOO";
        let out = replace_outside_md_brackets(line, "FOO", "BAR");
        // Inside [...] left untouched, href and bare text replaced.
        assert_eq!(out, "[FOO](BAR) and bare BAR");
    }

    #[test]
    fn replace_outside_md_brackets_handles_multibyte_chars() {
        let line = "caf\u{e9} FOO caf\u{e9}";
        let out = replace_outside_md_brackets(line, "FOO", "BAR");
        assert_eq!(out, "caf\u{e9} BAR caf\u{e9}");
    }

    // -----------------------------------------------------------------
    // ADRs
    // -----------------------------------------------------------------

    fn write_adr(dir: &Utf8Path, name: &str, content: &str) {
        fs::write(dir.join(name), content).unwrap();
    }

    #[test]
    fn generate_adr_docs_returns_empty_when_dir_missing() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        let links = generate_adr_docs(&docs_source, &site_root, false).unwrap();
        assert!(links.is_empty());
        assert!(!site_root.join("adr").exists());
    }

    #[test]
    fn generate_adr_docs_returns_empty_when_only_template_present() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let adr_dir = docs_source.join("ADR");
        fs::create_dir_all(&adr_dir).unwrap();
        write_adr(&adr_dir, "TEMPLATE.md", "# ADR NNNN: Template\n");
        write_adr(&adr_dir, "no-number.md", "# Not Numbered\n");

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        let links = generate_adr_docs(&docs_source, &site_root, false).unwrap();
        assert!(links.is_empty());
        // Output directory is still created even though nothing was rendered.
        assert!(site_root.join("adr").exists());
    }

    #[test]
    fn generate_adr_docs_renders_pages_index_and_cross_links() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let adr_dir = docs_source.join("ADR");
        fs::create_dir_all(&adr_dir).unwrap();
        write_adr(
            &adr_dir,
            "0001-first.md",
            "# ADR 0001: First Decision\n\n## Status\n\nAccepted — see notes\n\nSee 0002-second.md for the follow-up.\n",
        );
        write_adr(
            &adr_dir,
            "0002-second.md",
            "# ADR 0002: Second Decision\n\n## Status\n\nProposed\n\nRefers back to ADR 0001.\n",
        );

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        let links = generate_adr_docs(&docs_source, &site_root, true).unwrap();
        assert_eq!(links.len(), 4);
        assert!(
            links
                .iter()
                .any(|(src, dest)| src == "ADR/0001-first.md" && dest == "../adr/0001-first.html")
        );

        let first_html = fs::read_to_string(site_root.join("adr/0001-first.html")).unwrap();
        assert!(first_html.contains("0002-second.html"));
        assert!(!first_html.contains("0002-second.md"));
        assert!(first_html.contains("Accepted"));

        let second_html = fs::read_to_string(site_root.join("adr/0002-second.html")).unwrap();
        // "ADR 0001" reference rewritten into a link to the first ADR.
        assert!(second_html.contains("0001-first.html"));

        let index_html = fs::read_to_string(site_root.join("adr/index.html")).unwrap();
        assert!(index_html.contains("First Decision"));
        assert!(index_html.contains("Second Decision"));
        assert!(index_html.contains("Learn Beamtalk"));
    }

    #[test]
    fn discover_adrs_sorts_by_number_and_skips_non_adr_files() {
        let src_dir = TempDir::new().unwrap();
        let adr_dir = utf8(&src_dir);
        write_adr(&adr_dir, "0002-second.md", "# ADR 0002: Second\n");
        write_adr(&adr_dir, "0001-first.md", "# ADR 0001: First\n");
        write_adr(&adr_dir, "TEMPLATE.md", "# ADR NNNN: Template\n");
        write_adr(&adr_dir, "README.txt", "not markdown");

        let adrs = discover_adrs(&adr_dir).unwrap();
        assert_eq!(adrs.len(), 2);
        assert_eq!(adrs[0].number, "0001");
        assert_eq!(adrs[1].number, "0002");
    }

    #[test]
    fn extract_adr_title_variants() {
        assert_eq!(
            extract_adr_title("# ADR 0001: No Compound Assignment\n"),
            "No Compound Assignment"
        );
        // "adr " prefix but no colon falls back to the full heading text.
        assert_eq!(
            extract_adr_title("# ADR without a colon\n"),
            "ADR without a colon"
        );
        // Non-ADR heading is used verbatim.
        assert_eq!(extract_adr_title("# Just A Title\n"), "Just A Title");
        // No H1 present at all.
        assert_eq!(extract_adr_title("no heading here\n"), "Untitled ADR");
    }

    #[test]
    fn extract_adr_status_variants() {
        assert_eq!(
            extract_adr_status("## Status\n\nAccepted — with context\n"),
            "Accepted"
        );
        assert_eq!(
            extract_adr_status("## Status\n\nImplemented\n\n## Context\nmore\n"),
            "Implemented"
        );
        assert_eq!(extract_adr_status("no status section\n"), "Unknown");
        // Status heading with no non-empty line before the next heading.
        assert_eq!(
            extract_adr_status("## Status\n\n## Context\nSomething\n"),
            "Unknown"
        );
    }

    #[test]
    fn rewrite_adr_internal_links_rewrites_sibling_md_refs() {
        let adrs = vec![AdrInfo {
            number: "0001".to_string(),
            slug: "0001-first".to_string(),
            title: "First".to_string(),
            status: "Accepted".to_string(),
            output_file: "0001-first.html".to_string(),
        }];
        let content = "See 0001-first.md for details.";
        assert_eq!(
            rewrite_adr_internal_links(content, &adrs),
            "See 0001-first.html for details."
        );
    }

    #[test]
    fn adr_nav_marks_active_entries() {
        let adrs = vec![
            AdrInfo {
                number: "0001".to_string(),
                slug: "0001-first".to_string(),
                title: "First".to_string(),
                status: "Accepted".to_string(),
                output_file: "0001-first.html".to_string(),
            },
            AdrInfo {
                number: "0002".to_string(),
                slug: "0002-second".to_string(),
                title: "Second".to_string(),
                status: "Proposed".to_string(),
                output_file: "0002-second.html".to_string(),
            },
        ];

        let index_nav = adr_nav("index.html", &adrs, false);
        assert!(index_nav.contains("<a href=\"index.html\" class=\"active\">All ADRs</a>"));
        assert!(!index_nav.contains("Learn Beamtalk"));

        let item_nav = adr_nav("0002-second.html", &adrs, true);
        assert!(item_nav.contains("Learn Beamtalk"));
        assert!(item_nav.contains("0002-second.html\" class=\"active\">0002 — Second"));
    }

    // -----------------------------------------------------------------
    // Learning guide
    // -----------------------------------------------------------------

    #[test]
    fn generate_learning_guide_returns_false_when_dir_missing() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        assert!(!generate_learning_guide(&docs_source, &site_root).unwrap());
    }

    #[test]
    fn generate_learning_guide_returns_false_when_no_numbered_chapters() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let learning_dir = docs_source.join("learning");
        fs::create_dir_all(&learning_dir).unwrap();
        fs::write(learning_dir.join("README.md"), "# Learn Beamtalk\n").unwrap();

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        assert!(!generate_learning_guide(&docs_source, &site_root).unwrap());
    }

    #[test]
    fn generate_learning_guide_renders_chapters_with_readme_index() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let learning_dir = docs_source.join("learning");
        fs::create_dir_all(&learning_dir).unwrap();
        fs::write(
            learning_dir.join("01-getting-started.md"),
            "# Getting Started\n\nHello.\n",
        )
        .unwrap();
        fs::write(
            learning_dir.join("02-next-steps.md"),
            "## Next Steps\n\nSee 01-getting-started.md.\n",
        )
        .unwrap();
        fs::write(
            learning_dir.join("README.md"),
            "# Learn Beamtalk\n\nSee ../beamtalk-language-features.md and \
             ../beamtalk-syntax-rationale.md and 01-getting-started.md.\n",
        )
        .unwrap();

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        assert!(generate_learning_guide(&docs_source, &site_root).unwrap());

        let ch1 = fs::read_to_string(site_root.join("learning/01-getting-started.html")).unwrap();
        assert!(ch1.contains("↑ Contents"));
        assert!(ch1.contains("Next Steps")); // next-chapter nav link

        let ch2 = fs::read_to_string(site_root.join("learning/02-next-steps.html")).unwrap();
        assert!(ch2.contains("Getting Started")); // prev-chapter nav link
        assert!(ch2.contains("01-getting-started.html"));

        let index = fs::read_to_string(site_root.join("learning/index.html")).unwrap();
        assert!(index.contains("../docs/language-features.html"));
        assert!(index.contains("../docs/syntax-rationale.html"));
        assert!(index.contains("01-getting-started.html"));
    }

    #[test]
    fn generate_learning_guide_falls_back_to_list_without_readme() {
        let src_dir = TempDir::new().unwrap();
        let docs_source = utf8(&src_dir);
        let learning_dir = docs_source.join("learning");
        fs::create_dir_all(&learning_dir).unwrap();
        fs::write(
            learning_dir.join("01-getting-started.md"),
            "# Getting Started\n",
        )
        .unwrap();

        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        assert!(generate_learning_guide(&docs_source, &site_root).unwrap());

        let index = fs::read_to_string(site_root.join("learning/index.html")).unwrap();
        assert!(index.contains("<ol>"));
        assert!(index.contains("01-getting-started.html"));
    }

    #[test]
    fn discover_chapters_skips_non_markdown_and_unnumbered_files() {
        let src_dir = TempDir::new().unwrap();
        let learning_dir = utf8(&src_dir);
        fs::write(learning_dir.join("01-intro.md"), "# Intro\n").unwrap();
        fs::write(learning_dir.join("README.md"), "# Readme\n").unwrap();
        fs::write(learning_dir.join("notes.txt"), "not markdown").unwrap();

        let chapters = discover_chapters(&learning_dir).unwrap();
        assert_eq!(chapters.len(), 1);
        assert_eq!(chapters[0].slug, "01-intro");
    }

    #[test]
    fn extract_chapter_title_variants() {
        assert_eq!(
            extract_chapter_title("# Getting Started\n"),
            "Getting Started"
        );
        assert_eq!(extract_chapter_title("## Sub Heading\n"), "Sub Heading");
        assert_eq!(extract_chapter_title("no heading\n"), "Untitled Chapter");
        // Hash with no following space is not treated as a heading.
        assert_eq!(
            extract_chapter_title("#NoSpace\nmore text\n"),
            "Untitled Chapter"
        );
    }

    #[test]
    fn rewrite_chapter_internal_links_rewrites_sibling_md_refs() {
        let chapters = vec![ChapterInfo {
            number: "01".to_string(),
            slug: "01-intro".to_string(),
            title: "Intro".to_string(),
            output_file: "01-intro.html".to_string(),
        }];
        let content = "See 01-intro.md next.";
        assert_eq!(
            rewrite_chapter_internal_links(content, &chapters),
            "See 01-intro.html next."
        );
    }

    #[test]
    fn chapter_nav_placeholders_at_boundaries() {
        let first = ChapterInfo {
            number: "01".to_string(),
            slug: "01-a".to_string(),
            title: "A".to_string(),
            output_file: "01-a.html".to_string(),
        };
        let second = ChapterInfo {
            number: "02".to_string(),
            slug: "02-b".to_string(),
            title: "B".to_string(),
            output_file: "02-b.html".to_string(),
        };

        let start = chapter_nav(None, Some(&second));
        assert!(start.contains("chapter-nav-placeholder"));
        assert!(start.contains("02-b.html"));

        let end = chapter_nav(Some(&first), None);
        assert!(end.contains("01-a.html"));
        assert!(end.contains("chapter-nav-placeholder"));

        let none = chapter_nav(None, None);
        assert_eq!(none.matches("chapter-nav-placeholder").count(), 2);
    }

    #[test]
    fn learning_nav_marks_active_chapter() {
        let chapters = vec![ChapterInfo {
            number: "01".to_string(),
            slug: "01-a".to_string(),
            title: "A".to_string(),
            output_file: "01-a.html".to_string(),
        }];
        let nav = learning_nav("01-a.html", &chapters);
        assert!(nav.contains("01-a.html\" class=\"active\">01 — A"));
        let index_nav = learning_nav("index.html", &chapters);
        assert!(index_nav.contains("index.html\" class=\"active\">All Chapters"));
    }

    // -----------------------------------------------------------------
    // Landing page
    // -----------------------------------------------------------------

    #[test]
    fn landing_card_meta_known_and_unknown_files() {
        assert_eq!(
            landing_card_meta("security.html"),
            ("🔒", landing_card_meta("security.html").1)
        );
        assert!(!landing_card_meta("security.html").1.is_empty());
        assert_eq!(landing_card_meta("unknown-file.html"), ("📄", ""));
    }

    #[test]
    fn learning_card_desc_is_nonempty() {
        let (emoji, desc) = learning_card_desc();
        assert_eq!(emoji, "");
        assert!(!desc.is_empty());
    }

    #[test]
    fn write_site_landing_page_includes_cards_for_all_pages() {
        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        write_site_landing_page(&site_root, PAGES, true).unwrap();

        let html = fs::read_to_string(site_root.join("index.html")).unwrap();
        assert!(html.contains("Learn Beamtalk"));
        assert!(html.contains("docs/a.html"));
        assert!(html.contains("docs/installation.html"));
        assert!(html.contains("Install the Beamtalk toolchain"));
        assert!(html.contains("counter.bt"));
    }

    #[test]
    fn write_site_landing_page_omits_learning_card_when_unavailable() {
        let out_dir = TempDir::new().unwrap();
        let site_root = utf8(&out_dir);

        write_site_landing_page(&site_root, PAGES, false).unwrap();

        let html = fs::read_to_string(site_root.join("index.html")).unwrap();
        // The top-nav "Learn Beamtalk" link is always present in site mode
        // (from `layout::page_header`); only the landing-page card is gated
        // on `learning_available`.
        assert!(!html.contains("<h2>Learn Beamtalk</h2>"));
    }
}
