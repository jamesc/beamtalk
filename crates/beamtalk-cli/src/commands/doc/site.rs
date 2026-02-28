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
use super::renderer::{html_escape, render_doc};

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
        html.push_str(&prose_nav(output_file, prose_pages));
        html.push_str("<main class=\"main-content prose-content\">\n");
        html.push_str("<div class=\"breadcrumb\">");
        html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
        html.push_str("<a href=\"language-features.html\">Docs</a> &rsaquo; ");
        html.push_str(&html_escape(title));
        html.push_str("</div>\n");
        html.push_str(&render_doc(&markdown));
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

/// Build navigation sidebar for prose documentation pages.
fn prose_nav(active_file: &str, prose_pages: &[(&str, &str, &str)]) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");

    html.push_str("<div class=\"sidebar-section-label\">Navigate</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
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
/// Also applies `extra_links` rewrites (e.g. ADR source paths → rendered URLs).
fn rewrite_prose_links(
    markdown: &str,
    prose_pages: &[(&str, &str, &str)],
    extra_links: &[(String, String)],
) -> String {
    let mut result = markdown.to_string();
    for &(source_file, output_file, _) in prose_pages {
        result = result.replace(source_file, output_file);
    }
    for (source, dest) in extra_links {
        result = result.replace(source.as_str(), dest.as_str());
    }
    result
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

    // Render each ADR page
    for adr in &adrs {
        let source_path = adr_source.join(format!("{}.md", adr.slug));
        let content = fs::read_to_string(&source_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read ADR '{}'", adr.slug))?;
        // Rewrite within-ADR links: sibling .md → .html (same directory)
        let content = rewrite_adr_internal_links(&content, &adrs);
        render_adr_page(adr, &adrs, &content, &adr_output)?;
    }

    // Render ADR index
    render_adr_index(&adrs, &adr_output)?;

    println!("Generated {} ADR page(s)", adrs.len());

    // Return link-rewriting pairs for prose doc rendering.
    // Prose pages live in /docs/, so the relative path to /adr/ is ../adr/.
    let links = adrs
        .iter()
        .map(|a| {
            (
                format!("ADR/{}.md", a.slug),
                format!("../adr/{}", a.output_file),
            )
        })
        .collect();
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
            Some(AdrInfo {
                number,
                slug: stem,
                title,
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
) -> Result<()> {
    let page_title = format!("ADR {} — Beamtalk", adr.number);
    let mut html = String::new();
    html.push_str(&page_header(&page_title, "../style.css", "../"));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&adr_nav(&adr.output_file, all_adrs));
    html.push_str("<main class=\"main-content prose-content\">\n");
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
    html.push_str("<a href=\"index.html\">Architecture Decisions</a> &rsaquo; ");
    let _ = write!(html, "ADR {}", html_escape(&adr.number));
    html.push_str("</div>\n");
    html.push_str(&render_doc(content));
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
fn render_adr_index(adrs: &[AdrInfo], adr_output: &Utf8Path) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header(
        "Architecture Decisions — Beamtalk",
        "../style.css",
        "../",
    ));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(SIDEBAR_TOGGLE);
    html.push_str(&adr_nav("index.html", adrs));
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
    html.push_str("<ul class=\"adr-list\">\n");
    for adr in adrs {
        let _ = writeln!(
            html,
            "<li><a href=\"{file}\">{num} — {title}</a></li>",
            file = adr.output_file,
            num = html_escape(&adr.number),
            title = html_escape(&adr.title),
        );
    }
    html.push_str("</ul>\n");
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
fn adr_nav(active_file: &str, adrs: &[AdrInfo]) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");

    html.push_str("<div class=\"sidebar-section-label\">Navigate</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    html.push_str("<li><a href=\"../\">Home</a></li>\n");
    html.push_str("<li><a href=\"../apidocs/\">API Reference</a></li>\n");
    html.push_str("<li><a href=\"../docs/language-features.html\">Documentation</a></li>\n");
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
        "tooling.html" => (
            "🛠",
            "CLI, REPL, VS Code extension, and testing framework.",
        ),
        "known-limitations.html" => (
            "⚠️",
            "Current limitations and unimplemented features to be aware of.",
        ),
        _ => ("📄", ""),
    }
}

/// Beamtalk code snippet shown on the landing page.
const LANDING_CODE_SNIPPET: &str = "Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  getValue  => self.value

c := Counter spawn.
c increment.
c increment.
c getValue.   \"=> 2\"";

/// Generate the site landing page at the root.
pub(super) fn write_site_landing_page(
    output_path: &Utf8Path,
    prose_pages: &[(&str, &str, &str)],
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
    html.push_str("<h1>Beamtalk</h1>\n");
    html.push_str(
        "<p class=\"landing-tagline\">A live, message-based language built on the \
         BEAM VM. Smalltalk semantics, Erlang reliability, compiled to native \
         bytecode.</p>\n",
    );
    html.push_str("<div class=\"landing-cta\">\n");
    html.push_str(
        "<a href=\"docs/language-features.html\" class=\"btn-primary\">Get started</a>\n",
    );
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

    // API Reference card
    html.push_str("<a href=\"apidocs/\" class=\"landing-card\">\n");
    html.push_str("<span class=\"landing-card-emoji\">📚</span>\n");
    html.push_str("<h2>API Reference</h2>\n");
    html.push_str(
        "<p>Standard library classes — Actor, Block, Integer, String, \
         Collections, and more.</p>\n",
    );
    html.push_str("</a>\n");

    // ADR card
    html.push_str("<a href=\"adr/\" class=\"landing-card\">\n");
    html.push_str("<span class=\"landing-card-emoji\">📐</span>\n");
    html.push_str("<h2>Architecture Decisions</h2>\n");
    html.push_str(
        "<p>Key design decisions — context, alternatives considered, and consequences.</p>\n",
    );
    html.push_str("</a>\n");

    // Prose docs cards
    for &(_, file, title) in prose_pages {
        let (emoji, desc) = landing_card_meta(file);
        let _ = writeln!(
            html,
            "<a href=\"docs/{file}\" class=\"landing-card\">\n\
             <span class=\"landing-card-emoji\">{emoji}</span>\n\
             <h2>{title}</h2>\n\
             <p>{desc}</p>\n\
             </a>"
        );
    }

    html.push_str("</div>\n"); // .landing-cards

    html.push_str("<div class=\"landing-links\">\n");
    html.push_str(
        "<a href=\"https://github.com/jamesc/beamtalk\">View on GitHub</a> &nbsp;·&nbsp; \
         <a href=\"docs/known-limitations.html\">Known Limitations</a>\n",
    );
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
