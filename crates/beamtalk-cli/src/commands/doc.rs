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
use tracing::{debug, info, instrument};

/// Information about a documented class.
struct ClassInfo {
    name: String,
    superclass: Option<String>,
    doc_comment: Option<String>,
    methods: Vec<MethodInfo>,
    class_methods: Vec<MethodInfo>,
}

/// Information about a documented method.
struct MethodInfo {
    signature: String,
    doc_comment: Option<String>,
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

    // Generate CSS
    write_css(&output_path)?;

    // Generate index page
    write_index_page(&output_path, &classes)?;

    // Generate per-class pages
    for class in &classes {
        let inherited = collect_inherited_methods(class, &hierarchy, &methods_by_class);
        write_class_page(&output_path, class, &inherited, &methods_by_class)?;
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
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let Some(class) = module.classes.first() else {
        debug!("No class definition in '{}'", path);
        return Ok(None);
    };

    let methods = class
        .methods
        .iter()
        .map(|m| MethodInfo {
            signature: format_signature(&m.selector, &m.parameters),
            doc_comment: m.doc_comment.clone(),
        })
        .collect();

    let class_methods = class
        .class_methods
        .iter()
        .map(|m| MethodInfo {
            signature: format_signature(&m.selector, &m.parameters),
            doc_comment: m.doc_comment.clone(),
        })
        .collect();

    Ok(Some(ClassInfo {
        name: class.name.name.to_string(),
        superclass: class.superclass.as_ref().map(|s| s.name.to_string()),
        doc_comment: class.doc_comment.clone(),
        methods,
        class_methods,
    }))
}

/// Format a method signature for display.
fn format_signature(
    selector: &beamtalk_core::ast::MessageSelector,
    parameters: &[beamtalk_core::ast::Identifier],
) -> String {
    use beamtalk_core::ast::MessageSelector;
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => {
            if let Some(param) = parameters.first() {
                format!("{op} {}", param.name)
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
                    sig.push_str(&param.name);
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
/// For now, does basic line-based rendering: paragraphs, code blocks,
/// headings, and inline code.
fn render_doc(doc: &str) -> String {
    let mut html = String::new();
    let mut in_code_block = false;
    let mut paragraph = String::new();

    for line in doc.lines() {
        if line.starts_with("```") {
            if in_code_block {
                html.push_str("</code></pre>\n");
                in_code_block = false;
            } else {
                flush_paragraph(&mut paragraph, &mut html);
                html.push_str("<pre><code>");
                in_code_block = true;
            }
            continue;
        }

        if in_code_block {
            html.push_str(&html_escape(line));
            html.push('\n');
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
  --fg: #1a1a2e;
  --accent: #4361ee;
  --border: #dee2e6;
  --code-bg: #f5f5f5;
  --method-bg: #fafbfc;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  color: var(--fg);
  background: var(--bg);
  line-height: 1.6;
  max-width: 960px;
  margin: 0 auto;
  padding: 2rem;
}

a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }

h1 {
  font-size: 2rem;
  margin-bottom: 0.5rem;
  border-bottom: 2px solid var(--accent);
  padding-bottom: 0.5rem;
}

h2 { font-size: 1.4rem; margin-top: 2rem; margin-bottom: 0.5rem; }
h3 { font-size: 1.15rem; margin-top: 1.5rem; margin-bottom: 0.3rem; }
h4 { font-size: 1rem; margin-top: 1rem; margin-bottom: 0.3rem; color: #555; }

p { margin-bottom: 0.75rem; }

code {
  font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
  font-size: 0.9em;
  background: var(--code-bg);
  padding: 0.15em 0.35em;
  border-radius: 3px;
}

pre {
  background: var(--code-bg);
  padding: 1rem;
  border-radius: 6px;
  overflow-x: auto;
  margin-bottom: 1rem;
  border: 1px solid var(--border);
}

pre code { background: none; padding: 0; font-size: 0.85em; }

.breadcrumb { font-size: 0.9rem; color: #666; margin-bottom: 1rem; }
.class-doc { margin-bottom: 2rem; }
.superclass { color: #666; font-size: 0.95rem; margin-bottom: 1rem; }

.method {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 1rem;
  margin-bottom: 0.75rem;
}

.method-signature {
  font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
  font-size: 1rem;
  font-weight: 600;
  color: var(--accent);
}

.method-doc { margin-top: 0.5rem; color: #444; }
.method-doc p { margin-bottom: 0.5rem; }

.class-list {
  list-style: none;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 0.5rem;
}

.class-list li {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 0.6rem 1rem;
}

.class-list li a { font-weight: 500; }

.class-list .class-summary {
  display: block;
  font-size: 0.85rem;
  color: #666;
  margin-top: 0.2rem;
}

.inherited-section { opacity: 0.85; }
.inherited-section .method { background: transparent; border-style: dashed; }
.toc { margin-bottom: 2rem; }
.toc ul { list-style: none; column-count: 2; column-gap: 1rem; }
.toc li { margin-bottom: 0.2rem; font-family: monospace; font-size: 0.9rem; }

footer {
  margin-top: 3rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border);
  font-size: 0.8rem;
  color: #888;
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
fn write_index_page(output_dir: &Utf8Path, classes: &[ClassInfo]) -> Result<()> {
    let mut html = String::new();
    html.push_str(&page_header("Beamtalk API Reference", None));

    html.push_str("<h1>Beamtalk API Reference</h1>\n");
    html.push_str("<p>Standard library classes and API documentation.</p>\n");

    html.push_str("<h2>Classes</h2>\n");
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
) -> Result<()> {
    let title = format!("{} — Beamtalk", class.name);
    let mut html = String::new();
    html.push_str(&page_header(&title, Some("style.css")));

    write_class_header(&mut html, class, all_classes);
    write_class_toc(&mut html, class);
    write_class_methods(&mut html, class, all_classes);
    write_inherited_methods(&mut html, inherited, all_classes);

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

/// Write class and instance method sections.
fn write_class_methods(
    html: &mut String,
    class: &ClassInfo,
    all_classes: &HashMap<String, &ClassInfo>,
) {
    if !class.class_methods.is_empty() {
        html.push_str("<h2>Class Methods</h2>\n");
        for method in &class.class_methods {
            write_method_html(html, method, all_classes);
        }
    }

    if !class.methods.is_empty() {
        html.push_str("<h2>Instance Methods</h2>\n");
        for method in &class.methods {
            write_method_html(html, method, all_classes);
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

/// Render a single method's HTML.
fn write_method_html(
    html: &mut String,
    method: &MethodInfo,
    _all_classes: &HashMap<String, &ClassInfo>,
) {
    let anchor = method_anchor(&method.signature);
    let _ = writeln!(html, "<div class=\"method\" id=\"{anchor}\">");
    let _ = writeln!(
        html,
        "<div class=\"method-signature\">{}</div>",
        html_escape(&method.signature)
    );

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

/// Generate HTML page header.
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
         <body>\n"
    )
}

/// Generate HTML page footer.
fn page_footer() -> String {
    "<footer>Generated by <code>beamtalk doc</code></footer>\n\
     </body>\n\
     </html>\n"
        .to_string()
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
        assert_eq!(method_anchor("~= other"), "tildeeq-other");
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
        assert!(html.contains("<pre><code>"));
        assert!(html.contains("3 + 4"));
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
            }],
            class_methods: vec![],
        };
        let child = ClassInfo {
            name: "Child".into(),
            superclass: Some("Parent".into()),
            doc_comment: None,
            methods: vec![],
            class_methods: vec![],
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
