// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! HTML rendering for API documentation pages and utility helpers.
//!
//! **DDD Context:** CLI / Documentation

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs;
use tracing::debug;

use super::extractor::{ClassInfo, MethodInfo};
use super::highlighter::highlight_beamtalk;
use super::layout::{page_footer, page_header, write_hierarchy_tree};

/// Escape HTML special characters.
pub(super) fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Convert a class name to a link if the class exists in the docs.
pub(super) fn class_link(name: &str, classes: &HashMap<String, &ClassInfo>) -> String {
    if classes.contains_key(name) {
        format!("<a href=\"{name}.html\">{name}</a>")
    } else {
        html_escape(name)
    }
}

/// Render markdown doc comment as HTML.
///
/// Uses `pulldown-cmark` for full `CommonMark` + GFM table support.
/// Beamtalk code blocks get syntax highlighting via `highlight_beamtalk()`.
///
/// Raw HTML events are escaped to prevent injection from doc comments.
pub(super) fn render_doc(doc: &str) -> String {
    use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag, TagEnd};

    let options =
        Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_HEADING_ATTRIBUTES;
    let parser = Parser::new_ext(doc, options);

    let mut html = String::new();
    let mut code_text = String::new();
    let mut in_beamtalk_code = false;
    let mut in_code_block = false;

    // Transform events in a single pass so push_html preserves formatter state
    // (table header/body context, list nesting, etc.).
    let events = parser.filter_map(|event| match event {
        Event::Start(Tag::CodeBlock(kind)) => {
            let lang = match &kind {
                CodeBlockKind::Fenced(lang) => lang.as_ref(),
                CodeBlockKind::Indented => "",
            };
            in_beamtalk_code = lang.is_empty() || lang == "beamtalk";
            in_code_block = true;
            code_text.clear();
            let css_class = if in_beamtalk_code {
                " class=\"language-beamtalk\""
            } else {
                ""
            };
            Some(Event::Html(CowStr::from(format!("<pre><code{css_class}>"))))
        }
        Event::End(TagEnd::CodeBlock) => {
            let body = if in_beamtalk_code {
                highlight_beamtalk(&code_text)
            } else {
                html_escape(&code_text)
            };
            in_code_block = false;
            code_text.clear();
            Some(Event::Html(CowStr::from(format!("{body}</code></pre>\n"))))
        }
        Event::Text(text) if in_code_block => {
            code_text.push_str(&text);
            None
        }
        // Drop HTML comments (e.g. license headers); sanitize all other raw HTML.
        Event::Html(raw) | Event::InlineHtml(raw) => {
            if raw.trim_start().starts_with("<!--") {
                None
            } else {
                Some(Event::Text(raw))
            }
        }
        _ if in_code_block => None,
        other => Some(other),
    });

    pulldown_cmark::html::push_html(&mut html, events);
    html
}

/// Write the index (landing) page.
///
/// If `readme` is provided (from source dir's README.md), it is rendered
/// above the class listing as the project overview (per ADR 0008).
pub(super) fn write_index_page(
    output_dir: &Utf8Path,
    classes: &[ClassInfo],
    readme: Option<&str>,
    sidebar_html: &str,
    asset_prefix: &str,
) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header(
        "Beamtalk API Reference",
        "style.css",
        asset_prefix,
    ));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(
        "<button class=\"sidebar-toggle\" \
         onclick=\"document.querySelector('.sidebar').classList.toggle('open')\" \
         aria-label=\"Toggle navigation\">☰</button>\n",
    );
    html.push_str(sidebar_html);
    html.push_str("<main class=\"main-content\">\n");
    html.push_str("<div id=\"search-results\" class=\"search-results\"></div>\n");

    html.push_str("<h1>Beamtalk API Reference</h1>\n");

    if let Some(readme_content) = readme {
        html.push_str("<section class=\"readme\">\n");
        html.push_str(&render_doc(readme_content));
        html.push_str("</section>\n");
    } else {
        html.push_str("<p>Standard library classes and API documentation.</p>\n");
    }

    // Class hierarchy tree
    write_hierarchy_tree(&mut html, classes);

    html.push_str("<h2>All Classes</h2>\n");
    html.push_str("<ul class=\"class-list\">\n");

    for class in classes {
        let summary = class
            .doc_comment
            .as_ref()
            .and_then(|d| d.lines().next())
            .map(html_escape)
            .unwrap_or_default();

        let _ = writeln!(
            html,
            "<li><a href=\"{name}.html\">{name}</a>\
             <span class=\"class-summary\">{summary}</span></li>",
            name = html_escape(&class.name),
        );
    }

    html.push_str("</ul>\n");
    html.push_str("</main>\n");
    html.push_str(&page_footer());

    let index_path = output_dir.join("index.html");
    fs::write(&index_path, html)
        .into_diagnostic()
        .wrap_err("Failed to write index.html")?;
    debug!("Generated {}", index_path);
    Ok(())
}

/// Write a per-class documentation page.
pub(super) fn write_class_page(
    output_dir: &Utf8Path,
    class: &ClassInfo,
    inherited: &[(&str, &[MethodInfo])],
    all_classes: &HashMap<String, &ClassInfo>,
    sidebar_html: &str,
    asset_prefix: &str,
) -> Result<()> {
    let title = format!("{} — Beamtalk", class.name);
    let mut html = String::new();
    html.push_str(&page_header(&title, "style.css", asset_prefix));
    html.push_str("<div class=\"page-wrapper\">\n");
    html.push_str(
        "<button class=\"sidebar-toggle\" \
         onclick=\"document.querySelector('.sidebar').classList.toggle('open')\" \
         aria-label=\"Toggle navigation\">☰</button>\n",
    );
    html.push_str(&sidebar_html.replace(
        &format!("\">{}</a>", html_escape(&class.name)),
        &format!("\" class=\"active\">{}</a>", html_escape(&class.name)),
    ));
    html.push_str("<main class=\"main-content\">\n");

    write_class_header(&mut html, class, all_classes);
    write_class_toc(&mut html, class);
    write_class_methods(&mut html, class, all_classes);
    write_inherited_methods(&mut html, inherited, all_classes);

    html.push_str("</main>\n");
    html.push_str(&page_footer());

    let class_path = output_dir.join(format!("{}.html", class.name));
    fs::write(&class_path, html)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write {}.html", class.name))?;
    debug!("Generated {}", class_path);
    Ok(())
}

/// Write class page header: breadcrumb, title, superclass, and class doc.
fn write_class_header(
    html: &mut String,
    class: &ClassInfo,
    all_classes: &HashMap<String, &ClassInfo>,
) {
    html.push_str("<div class=\"breadcrumb\">");
    html.push_str("<a href=\"index.html\">API Reference</a> &rsaquo; ");
    html.push_str(&html_escape(&class.name));
    html.push_str("</div>\n");

    let _ = writeln!(html, "<h1>{}</h1>", html_escape(&class.name));

    if let Some(ref superclass) = class.superclass {
        html.push_str("<div class=\"superclass\">Inherits from ");
        html.push_str(&class_link(superclass, all_classes));
        html.push_str("</div>\n");
    }

    if class.is_sealed {
        html.push_str("<span class=\"badge badge-sealed\">Sealed</span>\n");
    }
    if class.is_abstract {
        html.push_str("<span class=\"badge badge-abstract\">Abstract</span>\n");
    }

    if let Some(ref doc) = class.doc_comment {
        html.push_str("<div class=\"class-doc\">\n");
        html.push_str(&render_doc(doc));
        html.push_str("</div>\n");
    }
}

/// Write method table of contents.
fn write_class_toc(html: &mut String, class: &ClassInfo) {
    if class.methods.is_empty() && class.class_methods.is_empty() {
        return;
    }
    html.push_str("<div class=\"toc\">\n");
    html.push_str("<h2>Methods</h2>\n<ul>\n");
    for method in &class.class_methods {
        let anchor = method_anchor(&method.signature);
        let _ = writeln!(
            html,
            "<li><a href=\"#{anchor}\">class » {sig}</a></li>",
            sig = html_escape(&method.signature),
        );
    }
    for method in &class.methods {
        let anchor = method_anchor(&method.signature);
        let _ = writeln!(
            html,
            "<li><a href=\"#{anchor}\">{sig}</a></li>",
            sig = html_escape(&method.signature),
        );
    }
    html.push_str("</ul>\n</div>\n");
}

/// Write class and instance method sections with source links.
fn write_class_methods(
    html: &mut String,
    class: &ClassInfo,
    _all_classes: &HashMap<String, &ClassInfo>,
) {
    let source = class.source_file.as_deref();
    let root = class.source_root.as_deref();

    if !class.class_methods.is_empty() {
        html.push_str("<h2>Class Methods</h2>\n");
        for method in &class.class_methods {
            write_method_html_with_source(html, method, source, root);
        }
    }

    if !class.methods.is_empty() {
        html.push_str("<h2>Instance Methods</h2>\n");
        for method in &class.methods {
            write_method_html_with_source(html, method, source, root);
        }
    }
}

/// Write inherited methods section.
fn write_inherited_methods(
    html: &mut String,
    inherited: &[(&str, &[MethodInfo])],
    all_classes: &HashMap<String, &ClassInfo>,
) {
    if inherited.is_empty() {
        return;
    }
    html.push_str("<div class=\"inherited-section\">\n");
    html.push_str("<h2>Inherited Methods</h2>\n");
    for (parent_name, methods) in inherited {
        let _ = writeln!(
            html,
            "<h3>From {}</h3>",
            class_link(parent_name, all_classes)
        );
        for method in *methods {
            write_method_html(html, method, all_classes);
        }
    }
    html.push_str("</div>\n");
}

/// Render a single method's HTML, with optional source link.
fn write_method_html(
    html: &mut String,
    method: &MethodInfo,
    _all_classes: &HashMap<String, &ClassInfo>,
) {
    write_method_html_with_source(html, method, None, None);
}

/// Render a method with a source file link.
fn write_method_html_with_source(
    html: &mut String,
    method: &MethodInfo,
    source_file: Option<&str>,
    source_root: Option<&str>,
) {
    let anchor = method_anchor(&method.signature);
    let _ = writeln!(html, "<div class=\"method\" id=\"{anchor}\">");
    html.push_str("<div class=\"method-header\">\n");
    let _ = writeln!(
        html,
        "<span class=\"method-signature\">{}</span>",
        html_escape(&method.signature)
    );

    if method.is_sealed {
        html.push_str("<span class=\"badge badge-sealed\">Sealed</span>\n");
    }

    if let (Some(file), Some(line)) = (source_file, method.line_number) {
        let root = source_root.unwrap_or("stdlib/src").trim_end_matches('/');
        let _ = writeln!(
            html,
            "<a class=\"source-link\" \
             href=\"https://github.com/jamesc/beamtalk/blob/main/{root}/{file}#L{line}\" \
             title=\"View source\">source</a>",
        );
    }

    html.push_str("</div>\n");

    if let Some(ref doc) = method.doc_comment {
        html.push_str("<div class=\"method-doc\">\n");
        html.push_str(&render_doc(doc));
        html.push_str("</div>\n");
    }

    html.push_str("</div>\n");
}

/// Generate an HTML anchor ID from a method signature.
pub(super) fn method_anchor(signature: &str) -> String {
    let mut result = String::new();
    for c in signature.chars() {
        match c {
            _ if c.is_ascii_alphanumeric() || c == '_' => result.push(c),
            '+' => result.push_str("plus"),
            '-' => result.push_str("minus"),
            '*' => result.push_str("star"),
            '/' => result.push_str("slash"),
            '%' => result.push_str("percent"),
            '=' => result.push_str("eq"),
            '<' => result.push_str("lt"),
            '>' => result.push_str("gt"),
            '~' => result.push_str("tilde"),
            _ => result.push('-'),
        }
    }
    result
}
