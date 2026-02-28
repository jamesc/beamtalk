// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Prose documentation and site landing page generation.
//!
//! **DDD Context:** CLI / Documentation

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write as _;
use std::fs;
use tracing::debug;

use super::highlighter::highlight_beamtalk;
use super::layout::{page_footer_simple, page_header};
use super::renderer::{html_escape, render_doc};

/// Generate prose documentation pages from markdown files.
pub(super) fn generate_prose_docs(
    docs_source: &Utf8Path,
    site_root: &Utf8Path,
    prose_pages: &[(&str, &str, &str)],
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

        // Rewrite cross-references to sibling prose docs (.md → .html)
        let markdown = rewrite_prose_links(&markdown, prose_pages);

        let page_title = format!("{title} — Beamtalk");
        let mut html = String::new();
        html.push_str(&page_header(&page_title, "../style.css", "../"));
        html.push_str("<div class=\"page-wrapper\">\n");
        html.push_str(
            "<button class=\"sidebar-toggle\" \
             onclick=\"document.querySelector('.sidebar').classList.toggle('open')\" \
             aria-label=\"Toggle navigation\">☰</button>\n",
        );
        html.push_str(&prose_nav(output_file, prose_pages));
        html.push_str("<main class=\"main-content prose-content\">\n");
        html.push_str("<div class=\"breadcrumb\">");
        html.push_str("<a href=\"../\">Home</a> &rsaquo; ");
        html.push_str("<a href=\"../docs/language-features.html\">Docs</a> &rsaquo; ");
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
/// Prose markdown files contain links like `[principles](beamtalk-principles.md)`.
/// After rendering, those need to point to the generated `.html` files.
fn rewrite_prose_links(markdown: &str, prose_pages: &[(&str, &str, &str)]) -> String {
    let mut result = markdown.to_string();
    for &(source_file, output_file, _) in prose_pages {
        result = result.replace(source_file, output_file);
    }
    result
}

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
