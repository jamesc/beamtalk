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

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs;
use tracing::{debug, info, instrument, warn};

/// Information about a documented class.
struct ClassInfo {
    name: String,
    superclass: Option<String>,
    doc_comment: Option<String>,
    methods: Vec<MethodInfo>,
    class_methods: Vec<MethodInfo>,
    source_file: Option<String>,
}

/// Information about a documented method.
struct MethodInfo {
    signature: String,
    doc_comment: Option<String>,
    line_number: Option<usize>,
}

/// Generate HTML API documentation.
///
/// Parses all `.bt` files in the given path, extracts doc comments and
/// class hierarchy, and generates static HTML to the output directory.
#[instrument(skip_all, fields(path = %path, output = %output_dir))]
pub fn run(path: &str, output_dir: &str) -> Result<()> {
    info!("Generating documentation");
    let source_path = Utf8PathBuf::from(path);
    let output_path = Utf8PathBuf::from(output_dir);

    // Find .bt source files
    let source_files = find_source_files(&source_path)?;
    if source_files.is_empty() {
        miette::bail!("No .bt source files found in '{path}'");
    }

    println!("Generating docs for {} file(s)...", source_files.len());

    // Parse all source files to extract class info
    let mut classes = Vec::new();
    for file in &source_files {
        if let Some(class_info) = parse_class_info(file)? {
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
    fs::create_dir_all(&output_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create output directory '{output_path}'"))?;

    // Generate CSS and JS assets
    write_css(&output_path)?;
    write_search_js(&output_path, &classes)?;

    // Build sidebar HTML (shared across all pages)
    let sidebar_html = build_sidebar_html(&classes);

    // Generate index page (include README.md from source dir if present, per ADR 0008)
    let readme_path = source_path.join("README.md");
    let readme = fs::read_to_string(&readme_path).ok();
    write_index_page(&output_path, &classes, readme.as_deref(), &sidebar_html)?;

    // Generate per-class pages
    for class in &classes {
        let inherited = collect_inherited_methods(class, &hierarchy, &methods_by_class);
        write_class_page(
            &output_path,
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

/// Find all `.bt` source files in a path.
fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    if path.is_file() {
        if path.extension() == Some("bt") {
            files.push(path.to_path_buf());
        } else {
            miette::bail!("File '{}' is not a .bt source file", path);
        }
    } else if path.is_dir() {
        for entry in fs::read_dir(path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read directory '{path}'"))?
        {
            let entry = entry.into_diagnostic()?;
            let entry_path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| miette::miette!("Non-UTF-8 path"))?;

            if entry_path.extension() == Some("bt") {
                files.push(entry_path);
            }
        }
    } else {
        miette::bail!("Path '{}' does not exist", path);
    }

    files.sort();
    Ok(files)
}

/// Parse a `.bt` source file and extract class documentation info.
fn parse_class_info(path: &Utf8Path) -> Result<Option<ClassInfo>> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        warn!("Skipping '{}': parse errors detected", path);
        return Ok(None);
    }

    let Some(class) = module.classes.first() else {
        debug!("No class definition in '{}'", path);
        return Ok(None);
    };

    let source_file = path.file_name().map(String::from);

    let make_method_info = |m: &beamtalk_core::ast::MethodDefinition| {
        let line_number = {
            let offset = m.span.start() as usize;
            source[..offset].matches('\n').count() + 1
        };
        MethodInfo {
            signature: format_signature(&m.selector, &m.parameters),
            doc_comment: m.doc_comment.clone(),
            line_number: Some(line_number),
        }
    };

    let methods = class.methods.iter().map(&make_method_info).collect();
    let class_methods = class.class_methods.iter().map(&make_method_info).collect();

    Ok(Some(ClassInfo {
        name: class.name.name.to_string(),
        superclass: class.superclass.as_ref().map(|s| s.name.to_string()),
        doc_comment: class.doc_comment.clone(),
        methods,
        class_methods,
        source_file,
    }))
}

/// Format a method signature for display.
fn format_signature(
    selector: &beamtalk_core::ast::MessageSelector,
    parameters: &[beamtalk_core::ast::ParameterDefinition],
) -> String {
    use beamtalk_core::ast::MessageSelector;
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => {
            if let Some(param) = parameters.first() {
                format!("{op} {}", param.name.name)
            } else {
                op.to_string()
            }
        }
        MessageSelector::Keyword(parts) => {
            let mut sig = String::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig.push(' ');
                }
                sig.push_str(&part.keyword);
                if let Some(param) = parameters.get(i) {
                    sig.push(' ');
                    sig.push_str(&param.name.name);
                }
            }
            sig
        }
    }
}

/// Collect inherited methods by walking the class hierarchy.
///
/// Includes cycle detection to prevent infinite loops from malformed hierarchies.
fn collect_inherited_methods<'a>(
    class: &ClassInfo,
    hierarchy: &'a HashMap<String, String>,
    methods_by_class: &'a HashMap<String, &'a ClassInfo>,
) -> Vec<(&'a str, &'a [MethodInfo])> {
    let mut inherited = Vec::new();
    let mut visited = std::collections::HashSet::new();

    let Some(ref superclass_name) = class.superclass else {
        return inherited;
    };

    // Walk from superclass upward
    let mut current: Option<&'a String> = hierarchy
        .keys()
        .find(|k| k.as_str() == superclass_name.as_str());

    // If superclass not in hierarchy keys, try looking it up in methods_by_class directly
    if current.is_none() {
        if let Some((key, parent)) = methods_by_class.get_key_value(superclass_name.as_str()) {
            if !parent.methods.is_empty() {
                inherited.push((key.as_str(), parent.methods.as_slice()));
            }
        }
        return inherited;
    }

    while let Some(parent_name) = current {
        if !visited.insert(parent_name.as_str()) {
            break; // cycle detected
        }
        if let Some(parent) = methods_by_class.get(parent_name.as_str()) {
            if !parent.methods.is_empty() {
                inherited.push((parent_name.as_str(), parent.methods.as_slice()));
            }
        }
        current = hierarchy.get(parent_name.as_str());
    }

    inherited
}

/// Escape HTML special characters.
fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Convert a class name to a link if the class exists in the docs.
fn class_link(name: &str, classes: &HashMap<String, &ClassInfo>) -> String {
    if classes.contains_key(name) {
        format!("<a href=\"{name}.html\">{name}</a>")
    } else {
        html_escape(name)
    }
}

/// Render markdown doc comment as HTML.
///
/// Supports paragraphs, code blocks (with syntax highlighting for beamtalk),
/// headings, and inline code.
fn render_doc(doc: &str) -> String {
    let mut html = String::new();
    let mut in_code_block = false;
    let mut code_lang = String::new();
    let mut code_lines = Vec::new();
    let mut paragraph = String::new();

    for line in doc.lines() {
        if line.starts_with("```") {
            if in_code_block {
                let code = code_lines.join("\n");
                if code_lang.is_empty() || code_lang == "beamtalk" {
                    html.push_str(&highlight_beamtalk(&code));
                } else {
                    html.push_str(&html_escape(&code));
                }
                html.push_str("</code></pre>\n");
                in_code_block = false;
                code_lines.clear();
                code_lang.clear();
            } else {
                flush_paragraph(&mut paragraph, &mut html);
                code_lang = line.trim_start_matches('`').trim().to_string();
                let css_class = if code_lang.is_empty() || code_lang == "beamtalk" {
                    " class=\"language-beamtalk\""
                } else {
                    ""
                };
                let _ = write!(html, "<pre><code{css_class}>");
                in_code_block = true;
            }
            continue;
        }

        if in_code_block {
            code_lines.push(line.to_string());
            continue;
        }

        if line.is_empty() {
            flush_paragraph(&mut paragraph, &mut html);
            continue;
        }

        if let Some(heading) = line.strip_prefix("## ") {
            flush_paragraph(&mut paragraph, &mut html);
            let _ = writeln!(html, "<h4>{}</h4>", html_escape(heading.trim()));
            continue;
        }

        if !paragraph.is_empty() {
            paragraph.push(' ');
        }
        paragraph.push_str(line);
    }

    if in_code_block {
        let code = code_lines.join("\n");
        if code_lang.is_empty() || code_lang == "beamtalk" {
            html.push_str(&highlight_beamtalk(&code));
        } else {
            html.push_str(&html_escape(&code));
        }
        html.push_str("</code></pre>\n");
    }
    flush_paragraph(&mut paragraph, &mut html);

    html
}

/// Flush accumulated paragraph text as a `<p>` element.
fn flush_paragraph(paragraph: &mut String, html: &mut String) {
    if !paragraph.is_empty() {
        html.push_str("<p>");
        html.push_str(&render_inline(&html_escape(paragraph)));
        html.push_str("</p>\n");
        paragraph.clear();
    }
}

/// Render inline formatting (backtick code spans).
fn render_inline(text: &str) -> String {
    let mut result = String::new();
    let mut in_code = false;

    for ch in text.chars() {
        if ch == '`' {
            if in_code {
                result.push_str("</code>");
                in_code = false;
            } else {
                result.push_str("<code>");
                in_code = true;
            }
        } else {
            result.push(ch);
        }
    }

    if in_code {
        result.push_str("</code>");
    }

    result
}

/// CSS stylesheet content for generated documentation.
const CSS_STYLESHEET: &str = r":root {
  --bg: #ffffff;
  --fg: #1e1e2e;
  --fg-muted: #6c6f85;
  --accent: #1e66f5;
  --accent-hover: #1443a6;
  --accent-bg: #eff4fc;
  --border: #dce0e8;
  --code-bg: #f5f5f7;
  --method-bg: #fafbfd;
  --sidebar-bg: #f8f9fb;
  --sidebar-w: 260px;
  --header-h: 56px;
  --shadow: 0 1px 3px rgba(0,0,0,0.08);
  --radius: 8px;

  /* Syntax highlighting */
  --hl-keyword: #8839ef;
  --hl-string: #40a02b;
  --hl-number: #fe640b;
  --hl-comment: #9ca0b0;
  --hl-selector: #1e66f5;
  --hl-symbol: #df8e1d;
  --hl-class: #ea76cb;
  --hl-self: #d20f39;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg: #1e1e2e;
    --fg: #cdd6f4;
    --fg-muted: #a6adc8;
    --accent: #89b4fa;
    --accent-hover: #b4d0fb;
    --accent-bg: #313244;
    --border: #45475a;
    --code-bg: #313244;
    --method-bg: #24273a;
    --sidebar-bg: #181825;
    --shadow: 0 1px 3px rgba(0,0,0,0.3);

    --hl-keyword: #cba6f7;
    --hl-string: #a6e3a1;
    --hl-number: #fab387;
    --hl-comment: #6c7086;
    --hl-selector: #89b4fa;
    --hl-symbol: #f9e2af;
    --hl-class: #f5c2e7;
    --hl-self: #f38ba8;
  }
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif;
  color: var(--fg);
  background: var(--bg);
  line-height: 1.65;
}

/* --- Layout --- */
.page-wrapper {
  display: flex;
  min-height: 100vh;
}

.sidebar {
  width: var(--sidebar-w);
  background: var(--sidebar-bg);
  border-right: 1px solid var(--border);
  position: fixed;
  top: 0;
  left: 0;
  bottom: 0;
  overflow-y: auto;
  padding: 1.25rem 0;
  z-index: 10;
  transition: transform 0.2s;
}

.sidebar-header {
  padding: 0 1.25rem 1rem;
  border-bottom: 1px solid var(--border);
  margin-bottom: 0.75rem;
}

.sidebar-header h2 {
  font-size: 1.1rem;
  font-weight: 700;
  margin: 0;
}

.sidebar-header h2 a {
  color: var(--fg);
  text-decoration: none;
}

.sidebar-search {
  display: block;
  width: 100%;
  margin-top: 0.6rem;
  padding: 0.45rem 0.7rem;
  border: 1px solid var(--border);
  border-radius: 6px;
  font-size: 0.85rem;
  background: var(--bg);
  color: var(--fg);
  outline: none;
}

.sidebar-search:focus {
  border-color: var(--accent);
  box-shadow: 0 0 0 2px var(--accent-bg);
}

.sidebar-nav { list-style: none; }

.sidebar-nav li {
  margin: 0;
}

.sidebar-nav a {
  display: block;
  padding: 0.3rem 1.25rem;
  font-size: 0.88rem;
  color: var(--fg);
  text-decoration: none;
  border-left: 3px solid transparent;
  transition: background 0.15s, border-color 0.15s;
}

.sidebar-nav a:hover {
  background: var(--accent-bg);
}

.sidebar-nav a.active {
  background: var(--accent-bg);
  border-left-color: var(--accent);
  color: var(--accent);
  font-weight: 600;
}

.main-content {
  margin-left: var(--sidebar-w);
  flex: 1;
  max-width: 900px;
  padding: 2rem 2.5rem;
}

/* Mobile toggle */
.sidebar-toggle {
  display: none;
  position: fixed;
  top: 12px;
  left: 12px;
  z-index: 20;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 0.4rem 0.6rem;
  font-size: 1.2rem;
  cursor: pointer;
  box-shadow: var(--shadow);
  color: var(--fg);
}

@media (max-width: 768px) {
  .sidebar {
    transform: translateX(-100%);
  }
  .sidebar.open {
    transform: translateX(0);
    box-shadow: 4px 0 20px rgba(0,0,0,0.15);
  }
  .sidebar-toggle { display: block; }
  .main-content {
    margin-left: 0;
    padding: 2rem 1.25rem;
    padding-top: 3.5rem;
  }
}

/* --- Typography --- */
a { color: var(--accent); text-decoration: none; }
a:hover { color: var(--accent-hover); text-decoration: underline; }

h1 {
  font-size: 1.75rem;
  font-weight: 700;
  margin-bottom: 0.5rem;
  letter-spacing: -0.02em;
}

h2 {
  font-size: 1.3rem;
  font-weight: 600;
  margin-top: 2.5rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.4rem;
  border-bottom: 1px solid var(--border);
}

h3 { font-size: 1.1rem; font-weight: 600; margin-top: 1.5rem; margin-bottom: 0.4rem; }
h4 { font-size: 0.95rem; font-weight: 600; margin-top: 1rem; margin-bottom: 0.3rem; color: var(--fg-muted); }
p { margin-bottom: 0.75rem; }

code {
  font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace;
  font-size: 0.87em;
  background: var(--code-bg);
  padding: 0.15em 0.4em;
  border-radius: 4px;
}

pre {
  background: var(--code-bg);
  padding: 1rem 1.25rem;
  border-radius: var(--radius);
  overflow-x: auto;
  margin-bottom: 1rem;
  border: 1px solid var(--border);
  line-height: 1.5;
}

pre code { background: none; padding: 0; font-size: 0.85em; }

/* --- Breadcrumb --- */
.breadcrumb { font-size: 0.85rem; color: var(--fg-muted); margin-bottom: 0.75rem; }
.breadcrumb a { color: var(--fg-muted); }
.breadcrumb a:hover { color: var(--accent); }

/* --- Class page --- */
.class-doc { margin-bottom: 2rem; }

.superclass {
  display: inline-block;
  font-size: 0.9rem;
  color: var(--fg-muted);
  margin-bottom: 1rem;
  background: var(--accent-bg);
  padding: 0.25rem 0.75rem;
  border-radius: 999px;
}

/* --- Methods --- */
.method {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 1rem 1.25rem;
  margin-bottom: 0.75rem;
  transition: box-shadow 0.15s;
}

.method:hover { box-shadow: var(--shadow); }
.method:target { border-left: 3px solid var(--accent); }

.method-header {
  display: flex;
  align-items: baseline;
  gap: 0.75rem;
  flex-wrap: wrap;
}

.method-signature {
  font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace;
  font-size: 0.95rem;
  font-weight: 600;
  color: var(--accent);
}

.source-link {
  font-size: 0.78rem;
  color: var(--fg-muted);
  margin-left: auto;
}

.method-doc { margin-top: 0.5rem; color: var(--fg); }
.method-doc p { margin-bottom: 0.5rem; }

/* --- TOC --- */
.toc { margin-bottom: 2rem; }
.toc ul { list-style: none; column-count: 2; column-gap: 2rem; }
.toc li { margin-bottom: 0.25rem; font-family: monospace; font-size: 0.87rem; }
.toc .label {
  display: inline-block;
  font-family: -apple-system, BlinkMacSystemFont, sans-serif;
  font-size: 0.72rem;
  font-weight: 600;
  color: var(--fg-muted);
  background: var(--code-bg);
  padding: 0.1em 0.4em;
  border-radius: 3px;
  margin-right: 0.3rem;
  vertical-align: middle;
}

/* --- Class list (index) --- */
.class-list {
  list-style: none;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 0.6rem;
}

.class-list li {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 0.75rem 1rem;
  transition: box-shadow 0.15s;
}

.class-list li:hover { box-shadow: var(--shadow); }
.class-list li a { font-weight: 600; }

.class-list .class-summary {
  display: block;
  font-size: 0.83rem;
  color: var(--fg-muted);
  margin-top: 0.2rem;
}

/* --- Hierarchy tree --- */
.hierarchy-tree { margin-bottom: 2rem; }
.hierarchy-tree ul {
  list-style: none;
  padding-left: 1.5rem;
  border-left: 2px solid var(--border);
}
.hierarchy-tree > ul { border-left: none; padding-left: 0; }
.hierarchy-tree li { margin: 0.2rem 0; font-size: 0.9rem; }
.hierarchy-tree a { font-weight: 500; }

/* --- Inherited --- */
.inherited-section { margin-top: 2rem; }
.inherited-section .method {
  background: transparent;
  border-style: dashed;
  opacity: 0.85;
}

/* --- Search results --- */
.search-results {
  display: none;
  margin-bottom: 2rem;
}
.search-results.active { display: block; }
.search-results h2 { border-bottom: none; margin-top: 0; }
.search-result-item {
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--border);
}
.search-result-item:last-child { border-bottom: none; }
.search-result-class { font-weight: 600; }
.search-result-method { font-family: monospace; font-size: 0.88rem; }

/* --- Syntax highlighting --- */
.hl-keyword { color: var(--hl-keyword); font-weight: 600; }
.hl-string { color: var(--hl-string); }
.hl-number { color: var(--hl-number); }
.hl-comment { color: var(--hl-comment); font-style: italic; }
.hl-selector { color: var(--hl-selector); }
.hl-symbol { color: var(--hl-symbol); }
.hl-class { color: var(--hl-class); }
.hl-self { color: var(--hl-self); font-weight: 600; }

/* --- Footer --- */
footer {
  margin-top: 3rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border);
  font-size: 0.8rem;
  color: var(--fg-muted);
}
";

/// Write CSS stylesheet.
fn write_css(output_dir: &Utf8Path) -> Result<()> {
    let css_path = output_dir.join("style.css");
    fs::write(&css_path, CSS_STYLESHEET)
        .into_diagnostic()
        .wrap_err("Failed to write style.css")?;
    debug!("Generated {}", css_path);
    Ok(())
}

/// Write the index (landing) page.
///
/// If `readme` is provided (from source dir's README.md), it is rendered
/// above the class listing as the project overview (per ADR 0008).
fn write_index_page(
    output_dir: &Utf8Path,
    classes: &[ClassInfo],
    readme: Option<&str>,
    sidebar_html: &str,
) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header("Beamtalk API Reference", None));
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
fn write_class_page(
    output_dir: &Utf8Path,
    class: &ClassInfo,
    inherited: &[(&str, &[MethodInfo])],
    all_classes: &HashMap<String, &ClassInfo>,
    sidebar_html: &str,
) -> Result<()> {
    let title = format!("{} — Beamtalk", class.name);
    let mut html = String::new();
    html.push_str(&page_header(&title, Some("style.css")));
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

    if !class.class_methods.is_empty() {
        html.push_str("<h2>Class Methods</h2>\n");
        for method in &class.class_methods {
            write_method_html_with_source(html, method, source);
        }
    }

    if !class.methods.is_empty() {
        html.push_str("<h2>Instance Methods</h2>\n");
        for method in &class.methods {
            write_method_html_with_source(html, method, source);
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
    write_method_html_with_source(html, method, None);
}

/// Render a method with a source file link.
fn write_method_html_with_source(
    html: &mut String,
    method: &MethodInfo,
    source_file: Option<&str>,
) {
    let anchor = method_anchor(&method.signature);
    let _ = writeln!(html, "<div class=\"method\" id=\"{anchor}\">");
    html.push_str("<div class=\"method-header\">\n");
    let _ = writeln!(
        html,
        "<span class=\"method-signature\">{}</span>",
        html_escape(&method.signature)
    );

    if let (Some(file), Some(line)) = (source_file, method.line_number) {
        let _ = writeln!(
            html,
            "<a class=\"source-link\" \
             href=\"https://github.com/jamesc/beamtalk/blob/main/lib/{file}#L{line}\" \
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
fn method_anchor(signature: &str) -> String {
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

/// Generate HTML page header with sidebar toggle and search JS.
fn page_header(title: &str, css_path: Option<&str>) -> String {
    let css = css_path.unwrap_or("style.css");
    format!(
        "<!DOCTYPE html>\n\
         <html lang=\"en\">\n\
         <head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>{title}</title>\n\
         <link rel=\"stylesheet\" href=\"{css}\">\n\
         </head>\n\
         <body>\n\
         <button class=\"sidebar-toggle\" onclick=\"document.querySelector('.sidebar')\
         .classList.toggle('open')\" aria-label=\"Toggle navigation\">☰</button>\n\
         <div class=\"page-wrapper\">\n"
    )
}

/// Generate HTML page footer.
fn page_footer() -> String {
    "<footer>Generated by <code>beamtalk doc</code></footer>\n\
     </div>\n\
     <script src=\"search.js\"></script>\n\
     </body>\n\
     </html>\n"
        .to_string()
}

/// Build sidebar HTML with class list navigation.
fn build_sidebar_html(classes: &[ClassInfo]) -> String {
    let mut html = String::new();
    html.push_str("<nav class=\"sidebar\">\n");
    html.push_str("<div class=\"sidebar-header\">\n");
    html.push_str("<h2><a href=\"index.html\">Beamtalk</a></h2>\n");
    html.push_str(
        "<input type=\"search\" class=\"sidebar-search\" \
         id=\"sidebar-search\" placeholder=\"Search classes…\" \
         autocomplete=\"off\">\n",
    );
    html.push_str("</div>\n");
    html.push_str("<ul class=\"sidebar-nav\" id=\"sidebar-nav\">\n");

    for class in classes {
        let _ = writeln!(
            html,
            "<li><a href=\"{name}.html\">{name}</a></li>",
            name = html_escape(&class.name),
        );
    }

    html.push_str("</ul>\n</nav>\n");
    html
}

/// Write class hierarchy tree on the index page.
fn write_hierarchy_tree(html: &mut String, classes: &[ClassInfo]) {
    // Build parent → children map
    let mut children: HashMap<String, Vec<&str>> = HashMap::new();

    for class in classes {
        if let Some(ref superclass) = class.superclass {
            children
                .entry(superclass.clone())
                .or_default()
                .push(&class.name);
        }
    }

    // Sort children alphabetically
    for v in children.values_mut() {
        v.sort_unstable();
    }

    // Find roots (classes without a parent in our set)
    let class_names: std::collections::HashSet<&str> =
        classes.iter().map(|c| c.name.as_str()).collect();
    let mut roots: Vec<&str> = classes
        .iter()
        .filter(|c| {
            c.superclass
                .as_ref()
                .is_none_or(|s| !class_names.contains(s.as_str()))
        })
        .map(|c| c.name.as_str())
        .collect();
    roots.sort_unstable();

    if roots.is_empty() {
        return;
    }

    html.push_str("<div class=\"hierarchy-tree\">\n");
    html.push_str("<h2>Class Hierarchy</h2>\n");
    html.push_str("<ul>\n");
    for root in &roots {
        write_hierarchy_node(html, root, &children);
    }
    html.push_str("</ul>\n</div>\n");
}

/// Recursively write a hierarchy tree node.
fn write_hierarchy_node(html: &mut String, name: &str, children: &HashMap<String, Vec<&str>>) {
    let _ = write!(
        html,
        "<li><a href=\"{name}.html\">{name}</a>",
        name = html_escape(name),
    );

    if let Some(kids) = children.get(name) {
        html.push_str("\n<ul>\n");
        for kid in kids {
            write_hierarchy_node(html, kid, children);
        }
        html.push_str("</ul>\n");
    }

    html.push_str("</li>\n");
}

/// Generate search JavaScript and write search.js file.
fn write_search_js(output_dir: &Utf8Path, classes: &[ClassInfo]) -> Result<()> {
    // Build search index as JSON
    let mut index_entries = Vec::new();
    for class in classes {
        // Add class entry
        let summary = class
            .doc_comment
            .as_ref()
            .and_then(|d| d.lines().next())
            .unwrap_or("")
            .replace('\\', "\\\\")
            .replace('"', "\\\"");
        index_entries.push(format!(
            "{{\"t\":\"class\",\"n\":\"{}\",\"s\":\"{}\",\"u\":\"{}.html\"}}",
            json_escape(&class.name),
            json_escape(&summary),
            class.name,
        ));

        // Add method entries
        for method in class.class_methods.iter().chain(class.methods.iter()) {
            let anchor = method_anchor(&method.signature);
            index_entries.push(format!(
                "{{\"t\":\"method\",\"n\":\"{}\",\"c\":\"{}\",\"u\":\"{}.html#{}\"}}",
                json_escape(&method.signature),
                json_escape(&class.name),
                class.name,
                anchor,
            ));
        }
    }

    let index_json = format!("[{}]", index_entries.join(","));

    let js = format!(
        "{SEARCH_JS_TEMPLATE}\nvar searchIndex = {index_json};\ninitSearch(searchIndex);\n"
    );

    let js_path = output_dir.join("search.js");
    fs::write(&js_path, js)
        .into_diagnostic()
        .wrap_err("Failed to write search.js")?;
    debug!("Generated {}", js_path);
    Ok(())
}

/// Escape string for JSON embedding.
fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Client-side search JavaScript.
const SEARCH_JS_TEMPLATE: &str = r#"
function initSearch(index) {
  var searchInput = document.getElementById('sidebar-search');
  var navList = document.getElementById('sidebar-nav');
  var resultsDiv = document.getElementById('search-results');
  if (!searchInput) return;

  var originalNav = navList ? navList.innerHTML : '';

  searchInput.addEventListener('input', function() {
    var q = this.value.toLowerCase().trim();

    // Filter sidebar nav
    if (navList) {
      if (!q) {
        navList.innerHTML = originalNav;
      } else {
        var items = navList.querySelectorAll('li');
        for (var i = 0; i < items.length; i++) {
          var text = items[i].textContent.toLowerCase();
          items[i].style.display = text.indexOf(q) !== -1 ? '' : 'none';
        }
      }
    }

    // Show search results in main content (if on index page)
    if (!resultsDiv) return;
    if (!q) {
      resultsDiv.className = 'search-results';
      resultsDiv.innerHTML = '';
      return;
    }

    var matches = [];
    for (var i = 0; i < index.length && matches.length < 30; i++) {
      var entry = index[i];
      var name = (entry.n || '').toLowerCase();
      var cls = (entry.c || '').toLowerCase();
      if (name.indexOf(q) !== -1 || cls.indexOf(q) !== -1) {
        matches.push(entry);
      }
    }

    if (matches.length === 0) {
      resultsDiv.className = 'search-results active';
      resultsDiv.innerHTML = '<h2>Search Results</h2><p>No results for "' +
        q.replace(/</g,'&lt;') + '"</p>';
      return;
    }

    var html = '<h2>Search Results</h2>';
    for (var i = 0; i < matches.length; i++) {
      var m = matches[i];
      if (m.t === 'class') {
        html += '<div class="search-result-item">' +
          '<a class="search-result-class" href="' + m.u + '">' + esc(m.n) + '</a>' +
          (m.s ? ' — ' + esc(m.s) : '') + '</div>';
      } else {
        html += '<div class="search-result-item">' +
          '<a href="' + m.u + '"><span class="search-result-class">' + esc(m.c) +
          '</span> › <span class="search-result-method">' + esc(m.n) + '</span></a></div>';
      }
    }
    resultsDiv.className = 'search-results active';
    resultsDiv.innerHTML = html;
  });

  // Keyboard shortcut: / to focus search
  document.addEventListener('keydown', function(e) {
    if (e.key === '/' && document.activeElement !== searchInput) {
      e.preventDefault();
      searchInput.focus();
    }
  });
}

function esc(s) {
  var d = document.createElement('div');
  d.textContent = s;
  return d.innerHTML;
}
"#;

/// Syntax-highlight Beamtalk source code.
///
/// Applies HTML span tags for keywords, strings, numbers, comments,
/// selectors, symbols, class names, and `self`.
fn highlight_beamtalk(code: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = code.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        if let Some(new_i) = hl_comment(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_string(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_symbol(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_number(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_word(&chars, i, &mut result) {
            i = new_i;
        } else {
            match chars[i] {
                '&' => result.push_str("&amp;"),
                '<' => result.push_str("&lt;"),
                '>' => result.push_str("&gt;"),
                '"' => result.push_str("&quot;"),
                c => result.push(c),
            }
            i += 1;
        }
    }

    result
}

/// Highlight line or block comments. Returns new index if matched.
fn hl_comment(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    if i + 1 >= len || chars[i] != '/' {
        return None;
    }

    let mut j = i;
    if chars[i + 1] == '/' {
        while j < len && chars[j] != '\n' {
            j += 1;
        }
    } else if chars[i + 1] == '*' {
        j += 2;
        while j + 1 < len && !(chars[j] == '*' && chars[j + 1] == '/') {
            j += 1;
        }
        if j + 1 < len {
            j += 2;
        }
    } else {
        return None;
    }

    let text: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-comment\">{}</span>",
        html_escape(&text)
    );
    Some(j)
}

/// Highlight string literals. Returns new index if matched.
fn hl_string(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    if chars[i] != '\'' && chars[i] != '"' {
        return None;
    }
    let quote = chars[i];
    let len = chars.len();
    let mut j = i + 1;
    while j < len && chars[j] != quote {
        if chars[j] == '\\' {
            j += 1;
        }
        j += 1;
    }
    if j < len {
        j += 1;
    }
    let s: String = chars[i..j].iter().collect();
    let _ = write!(out, "<span class=\"hl-string\">{}</span>", html_escape(&s));
    Some(j)
}

/// Highlight symbol literals (#name). Returns new index if matched.
fn hl_symbol(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    if chars[i] != '#' || i + 1 >= len || !(chars[i + 1].is_alphabetic() || chars[i + 1] == '_') {
        return None;
    }
    let mut j = i + 1;
    while j < len && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == ':') {
        j += 1;
    }
    let sym: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-symbol\">{}</span>",
        html_escape(&sym)
    );
    Some(j)
}

/// Highlight numeric literals. Returns new index if matched.
fn hl_number(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    let is_neg = chars[i] == '-'
        && i + 1 < len
        && chars[i + 1].is_ascii_digit()
        && (i == 0 || !chars[i - 1].is_alphanumeric());

    if !chars[i].is_ascii_digit() && !is_neg {
        return None;
    }

    let mut j = i;
    if chars[j] == '-' {
        j += 1;
    }
    while j < len && (chars[j].is_ascii_digit() || chars[j] == '.') {
        j += 1;
    }
    let num: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-number\">{}</span>",
        html_escape(&num)
    );
    Some(j)
}

/// Highlight identifiers, keywords, and class names. Returns new index if matched.
fn hl_word(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    if !chars[i].is_alphabetic() && chars[i] != '_' {
        return None;
    }
    let len = chars.len();
    let mut j = i;
    while j < len && (chars[j].is_alphanumeric() || chars[j] == '_') {
        j += 1;
    }
    let word: String = chars[i..j].iter().collect();

    match word.as_str() {
        "self" => {
            let _ = write!(out, "<span class=\"hl-self\">{word}</span>");
        }
        "true" | "false" | "nil" | "super" | "sealed" => {
            let _ = write!(
                out,
                "<span class=\"hl-keyword\">{}</span>",
                html_escape(&word)
            );
        }
        "subclass" | "state" | "ifTrue" | "ifFalse" | "ifNil" | "ifNotNil" | "whileTrue"
        | "whileFalse" | "timesRepeat" => {
            let _ = write!(
                out,
                "<span class=\"hl-selector\">{}</span>",
                html_escape(&word)
            );
        }
        _ if word.starts_with(|c: char| c.is_uppercase()) => {
            let _ = write!(
                out,
                "<span class=\"hl-class\">{}</span>",
                html_escape(&word)
            );
        }
        _ => out.push_str(&html_escape(&word)),
    }
    Some(j)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

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

        let info = parse_class_info(&file).unwrap().unwrap();
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

        let info = parse_class_info(&file).unwrap();
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
        assert!(html.contains("<h4>Examples</h4>"));
    }

    #[test]
    fn test_render_inline_code() {
        let result = render_inline("use `foo` here");
        assert_eq!(result, "use <code>foo</code> here");
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
        };
        let child = ClassInfo {
            name: "Child".into(),
            superclass: Some("Parent".into()),
            doc_comment: None,
            methods: vec![],
            class_methods: vec![],
            source_file: None,
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
}
