// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CSS and JavaScript asset generation for documentation.
//!
//! **DDD Context:** CLI / Documentation

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::debug;

use super::extractor::ClassInfo;
use super::renderer::method_anchor;

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

/* --- Tables (rendered from markdown) --- */
table {
  border-collapse: collapse;
  width: 100%;
  margin-bottom: 1rem;
  font-size: 0.9rem;
}
th, td {
  border: 1px solid var(--border);
  padding: 0.4rem 0.75rem;
  text-align: left;
}
th { background: var(--accent-bg); font-weight: 600; }
tbody tr:nth-child(even) { background: var(--code-bg); }

/* --- Lists (rendered from markdown) --- */
.class-doc ul, .class-doc ol, .method-doc ul, .method-doc ol, .readme ul, .readme ol {
  margin-bottom: 0.75rem;
  padding-left: 1.5rem;
}
.class-doc li, .method-doc li, .readme li { margin-bottom: 0.25rem; }

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

/* --- Landing page --- */
.landing-wrapper {
  justify-content: center;
}

.landing-content {
  margin-left: 0;
  max-width: 960px;
  margin: 0 auto;
  padding: 3rem 2.5rem;
}

.landing-hero {
  text-align: center;
  margin-bottom: 3rem;
}

.landing-hero h1 {
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
}

.landing-tagline {
  font-size: 1.15rem;
  color: var(--fg-muted);
}

.landing-cards {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.landing-card {
  display: block;
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 1.25rem 1.5rem;
  text-decoration: none;
  color: var(--fg);
  transition: box-shadow 0.15s, border-color 0.15s;
}

.landing-card:hover {
  box-shadow: var(--shadow);
  border-color: var(--accent);
  text-decoration: none;
}

.landing-card h2 {
  font-size: 1.1rem;
  margin-top: 0;
  margin-bottom: 0.4rem;
  border-bottom: none;
  padding-bottom: 0;
  color: var(--accent);
}

.landing-card p {
  font-size: 0.88rem;
  color: var(--fg-muted);
  margin-bottom: 0;
}

.landing-links {
  text-align: center;
  margin-top: 1.5rem;
  font-size: 0.9rem;
}

/* --- Prose pages --- */
.prose-content {
  max-width: 800px;
}

/* --- Sidebar cross-navigation --- */
.sidebar-section {
  padding: 0.5rem 1.25rem;
  border-bottom: 1px solid var(--border);
  margin-bottom: 0.5rem;
}

.sidebar-home-link, .sidebar-docs-link {
  display: block;
  font-size: 0.85rem;
  color: var(--fg-muted);
  padding: 0.2rem 0;
}

.sidebar-home-link:hover, .sidebar-docs-link:hover {
  color: var(--accent);
}
";

/// Write CSS stylesheet.
pub(super) fn write_css(output_dir: &Utf8Path) -> Result<()> {
    let css_path = output_dir.join("style.css");
    fs::write(&css_path, CSS_STYLESHEET)
        .into_diagnostic()
        .wrap_err("Failed to write style.css")?;
    debug!("Generated {}", css_path);
    Ok(())
}

/// Generate search JavaScript and write search.js file.
pub(super) fn write_search_js(output_dir: &Utf8Path, classes: &[ClassInfo]) -> Result<()> {
    // Build search index as JSON
    let mut index_entries = Vec::new();
    for class in classes {
        // Add class entry
        let summary = class
            .doc_comment
            .as_ref()
            .and_then(|d| d.lines().next())
            .unwrap_or("");
        index_entries.push(format!(
            "{{\"t\":\"class\",\"n\":\"{}\",\"s\":\"{}\",\"u\":\"{}.html\"}}",
            json_escape(&class.name),
            json_escape(summary),
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
        esc(q) + '"</p>';
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
