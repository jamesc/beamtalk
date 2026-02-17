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
        html.push_str(&page_header(&page_title, Some("../style.css")));
        html.push_str("<div class=\"page-wrapper\">\n");
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
    html.push_str("<div class=\"sidebar-header\">\n");
    html.push_str("<h2><a href=\"../\">Beamtalk</a></h2>\n");
    html.push_str("</div>\n");
    html.push_str("<ul class=\"sidebar-nav\">\n");
    html.push_str("<li><a href=\"../apidocs/\">API Reference</a></li>\n");

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

/// Generate the site landing page at the root.
pub(super) fn write_site_landing_page(
    output_path: &Utf8Path,
    prose_pages: &[(&str, &str, &str)],
) -> Result<()> {
    let mut html = String::new();
    // Landing page has no sidebar, so omit the sidebar toggle button
    html.push_str(
        "<!DOCTYPE html>\n\
         <html lang=\"en\">\n\
         <head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>Beamtalk Documentation</title>\n\
         <link rel=\"stylesheet\" href=\"style.css\">\n\
         </head>\n\
         <body>\n",
    );
    html.push_str("<div class=\"page-wrapper landing-wrapper\">\n");
    html.push_str("<main class=\"main-content landing-content\">\n");

    // Hero section
    html.push_str("<div class=\"landing-hero\">\n");
    html.push_str("<h1>Beamtalk</h1>\n");
    html.push_str("<p class=\"landing-tagline\">A live, interactive Smalltalk-like language for the BEAM VM</p>\n");
    html.push_str("</div>\n");

    // Navigation cards
    html.push_str("<div class=\"landing-cards\">\n");

    // API Reference card
    html.push_str("<a href=\"apidocs/\" class=\"landing-card\">\n");
    html.push_str("<h2>API Reference</h2>\n");
    html.push_str("<p>Standard library class documentation — Actor, Block, Integer, String, Collections, and more.</p>\n");
    html.push_str("</a>\n");

    // Prose docs cards
    for &(_, file, title) in prose_pages {
        let desc = match title {
            "Language Features" => {
                "Syntax, semantics, and examples for Beamtalk's message-based programming model."
            }
            "Design Principles" => {
                "The 13 core principles guiding all design and implementation decisions."
            }
            "Architecture" => {
                "Compiler pipeline, runtime, hot code reload, and live development flow."
            }
            "Agent-Native Development" => {
                "Why Beamtalk is uniquely suited as a development environment for AI coding agents."
            }
            "Syntax Rationale" => {
                "Why Beamtalk keeps certain Smalltalk conventions and diverges from others."
            }
            "Domain Model" => {
                "Domain-Driven Design model: bounded contexts, aggregates, and ubiquitous language."
            }
            "Security" => "Security model, threat analysis, and sandboxing for untrusted code.",
            _ => "",
        };
        let _ = writeln!(
            html,
            "<a href=\"docs/{file}\" class=\"landing-card\">\n<h2>{title}</h2>\n<p>{desc}</p>\n</a>"
        );
    }

    html.push_str("</div>\n");

    // GitHub link
    html.push_str("<div class=\"landing-links\">\n");
    html.push_str("<a href=\"https://github.com/jamesc/beamtalk\">GitHub Repository</a>\n");
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
