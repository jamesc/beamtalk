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
  --bg: #FAFAF8;
  --fg: #111827;
  --fg-muted: #6B7280;
  --accent: #6366F1;
  --accent-hover: #4F46E5;
  --accent-bg: #EEF2FF;
  --border: #E5E7EB;
  --code-bg: #18181B;
  --code-fg: #E4E4E7;
  --inline-code-bg: #F3F4F6;
  --method-bg: #FFFFFF;
  --sidebar-bg: #F9F9F7;
  --nav-bg: #FFFFFF;
  --sidebar-w: 272px;
  --nav-h: 60px;
  --shadow: 0 1px 3px rgba(0,0,0,0.07);
  --shadow-md: 0 4px 12px rgba(0,0,0,0.12);
  --radius: 8px;
  --badge-abstract-bg: #FEF3C7;
  --badge-abstract-fg: #92400E;
  /* Syntax highlighting — tuned for dark code blocks */
  --hl-keyword: #C084FC;
  --hl-string: #86EFAC;
  --hl-number: #FCA5A5;
  --hl-comment: #71717A;
  --hl-selector: #93C5FD;
  --hl-symbol: #FCD34D;
  --hl-class: #F9A8D4;
  --hl-self: #F87171;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg: #0A0A09;
    --fg: #FAFAF8;
    --fg-muted: #9CA3AF;
    --accent: #818CF8;
    --accent-hover: #A5B4FC;
    --accent-bg: #1E1B4B;
    --border: #27272A;
    --code-bg: #111110;
    --code-fg: #E4E4E7;
    --inline-code-bg: #27272A;
    --method-bg: #18181B;
    --sidebar-bg: #111110;
    --nav-bg: #0A0A09;
    --shadow: 0 1px 3px rgba(0,0,0,0.4);
    --shadow-md: 0 4px 12px rgba(0,0,0,0.5);
    --badge-abstract-bg: #3D2B08;
    --badge-abstract-fg: #FCD34D;
  }
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  font-size: 15px;
  color: var(--fg);
  background: var(--bg);
  line-height: 1.7;
}

/* --- Top navigation bar --- */
.top-nav {
  position: fixed;
  top: 0; left: 0; right: 0;
  height: var(--nav-h);
  background: var(--nav-bg);
  border-bottom: 1px solid var(--border);
  display: flex;
  align-items: center;
  padding: 0 1.5rem;
  z-index: 100;
  gap: 1.5rem;
}

.nav-logo {
  font-weight: 700;
  font-size: 1.05rem;
  color: var(--fg);
  text-decoration: none;
  letter-spacing: -0.02em;
  flex-shrink: 0;
}
.nav-logo:hover { color: var(--fg); text-decoration: none; }

.nav-links {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  flex: 1;
}

.nav-links a {
  font-size: 0.875rem;
  font-weight: 500;
  color: var(--fg-muted);
  text-decoration: none;
  padding: 0.35rem 0.7rem;
  border-radius: 6px;
  transition: background 0.12s, color 0.12s;
}
.nav-links a:hover {
  background: var(--accent-bg);
  color: var(--accent);
  text-decoration: none;
}
.nav-links a.nav-github {
  margin-left: auto;
  border: 1px solid var(--border);
}
.nav-links a.nav-github:hover { border-color: var(--accent); }

/* --- Layout --- */
.page-wrapper {
  display: flex;
  min-height: 100vh;
  padding-top: var(--nav-h);
}

.sidebar {
  width: var(--sidebar-w);
  background: var(--sidebar-bg);
  border-right: 1px solid var(--border);
  position: fixed;
  top: var(--nav-h);
  left: 0;
  bottom: 0;
  overflow-y: auto;
  padding: 1.25rem 0;
  z-index: 10;
  transition: transform 0.2s;
}

.sidebar-search-wrap {
  padding: 0 1rem 0.75rem;
  border-bottom: 1px solid var(--border);
  margin-bottom: 0.5rem;
}

.sidebar-search {
  display: block;
  width: 100%;
  padding: 0.45rem 0.7rem;
  border: 1px solid var(--border);
  border-radius: 6px;
  font-size: 0.83rem;
  background: var(--bg);
  color: var(--fg);
  outline: none;
  font-family: inherit;
}
.sidebar-search:focus {
  border-color: var(--accent);
  box-shadow: 0 0 0 3px var(--accent-bg);
}

.sidebar-section-label {
  padding: 0.75rem 1.25rem 0.3rem;
  font-size: 0.69rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--fg-muted);
}

.sidebar-nav { list-style: none; }
.sidebar-nav li { margin: 0; }
.sidebar-nav a {
  display: block;
  padding: 0.3rem 1.25rem;
  font-size: 0.875rem;
  color: var(--fg-muted);
  text-decoration: none;
  transition: background 0.1s, color 0.1s;
}
.sidebar-nav a:hover {
  background: var(--accent-bg);
  color: var(--accent);
  text-decoration: none;
}
.sidebar-nav a.active {
  background: var(--accent-bg);
  color: var(--accent);
  font-weight: 600;
}

.main-content {
  margin-left: var(--sidebar-w);
  flex: 1;
  max-width: 860px;
  padding: 2.5rem 3rem;
}

.sidebar-toggle {
  display: none;
  position: fixed;
  top: calc(var(--nav-h) + 10px);
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

.sidebar-overlay {
  display: none;
  position: fixed;
  inset: 0;
  background: rgba(0,0,0,0.4);
  z-index: 9;
}
.sidebar-overlay.active { display: block; }

@media (max-width: 768px) {
  .sidebar { transform: translateX(-100%); }
  .sidebar.open {
    transform: translateX(0);
    box-shadow: 4px 0 20px rgba(0,0,0,0.15);
  }
  .sidebar-toggle { display: block; }
  .main-content { margin-left: 0; padding: 2rem 1.25rem; padding-top: 3.5rem; }
  .nav-links a:not(.nav-github):not(:first-child) { display: none; }

  /* Collapse TOC to single column */
  .toc ul { column-count: 1; }

  /* Make tables scroll horizontally */
  table { display: block; overflow-x: auto; -webkit-overflow-scrolling: touch; }

  /* Scale hero heading */
  .landing-hero-text h1 { font-size: 2rem; }

  /* Scale page headings */
  h1 { font-size: 1.5rem; }

  /* Stack method header on narrow screens */
  .method-header { flex-direction: column; gap: 0.4rem; }
  .source-link { margin-left: 0; }

  /* Code block horizontal scroll with tighter padding */
  pre { padding: 1rem; font-size: 0.82rem; }
}

@media (max-width: 480px) {
  /* Very small screens: tighter padding */
  .main-content { padding: 1.5rem 1rem; padding-top: 3.5rem; }
  .landing-content { padding: 1.5rem 1rem 1.5rem; }

  /* Full-width CTAs */
  .landing-cta { flex-direction: column; }
  .btn-primary, .btn-secondary { width: 100%; text-align: center; }

  /* Smallest heading */
  .landing-hero-text h1 { font-size: 1.75rem; }

  /* Reduce method card padding */
  .method { padding: 1rem; }

}

/* --- Typography --- */
a { color: var(--accent); text-decoration: none; }
a:hover { color: var(--accent-hover); text-decoration: underline; }

h1 {
  font-size: 1.875rem;
  font-weight: 700;
  margin-bottom: 0.5rem;
  letter-spacing: -0.025em;
  line-height: 1.2;
}
h2 {
  font-size: 1.2rem;
  font-weight: 600;
  margin-top: 2.5rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border);
  letter-spacing: -0.01em;
}
h3 { font-size: 1.05rem; font-weight: 600; margin-top: 1.75rem; margin-bottom: 0.4rem; }
h4 {
  font-size: 0.78rem;
  font-weight: 700;
  margin-top: 1rem;
  margin-bottom: 0.3rem;
  color: var(--fg-muted);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
p { margin-bottom: 0.875rem; }

code {
  font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace;
  font-size: 0.85em;
  background: var(--inline-code-bg);
  color: var(--fg);
  padding: 0.15em 0.4em;
  border-radius: 4px;
}

pre {
  background: var(--code-bg);
  color: var(--code-fg);
  padding: 1.25rem 1.5rem;
  border-radius: var(--radius);
  overflow-x: auto;
  margin-bottom: 1.25rem;
  line-height: 1.6;
}
pre code { background: none; color: inherit; padding: 0; font-size: 0.875rem; }

/* --- Tables --- */
table { border-collapse: collapse; width: 100%; margin-bottom: 1.25rem; font-size: 0.9rem; }
th, td { border: 1px solid var(--border); padding: 0.5rem 0.875rem; text-align: left; }
th {
  background: var(--accent-bg);
  font-weight: 600;
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--accent);
}
tbody tr:hover { background: var(--sidebar-bg); }

/* --- Lists --- */
.class-doc ul, .class-doc ol, .method-doc ul, .method-doc ol, .readme ul, .readme ol {
  margin-bottom: 0.875rem;
  padding-left: 1.5rem;
}
.class-doc li, .method-doc li, .readme li { margin-bottom: 0.3rem; }

/* --- Breadcrumb --- */
.breadcrumb {
  font-size: 0.8rem;
  color: var(--fg-muted);
  margin-bottom: 1rem;
  display: flex;
  align-items: center;
  gap: 0.4rem;
}
.breadcrumb a { color: var(--fg-muted); }
.breadcrumb a:hover { color: var(--accent); text-decoration: none; }

/* --- Class page --- */
.class-doc { margin-bottom: 2.5rem; }

.superclass {
  display: inline-block;
  font-size: 0.875rem;
  color: var(--fg-muted);
  margin-bottom: 1.25rem;
  background: var(--accent-bg);
  padding: 0.2rem 0.75rem;
  border-radius: 999px;
}

.badge {
  display: inline-block;
  font-size: 0.68rem;
  font-weight: 700;
  padding: 0.15rem 0.5rem;
  border-radius: 999px;
  vertical-align: middle;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.badge-sealed { background: var(--inline-code-bg); color: var(--fg-muted); }
.badge-abstract { background: var(--badge-abstract-bg); color: var(--badge-abstract-fg); }

/* --- Methods --- */
.method {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 1.25rem 1.5rem;
  margin-bottom: 1rem;
  transition: box-shadow 0.15s;
}
.method:hover { box-shadow: var(--shadow-md); }
.method:target { border-left: 3px solid var(--accent); }

.method-header {
  display: flex;
  align-items: baseline;
  gap: 0.75rem;
  flex-wrap: wrap;
}
.method-signature {
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  font-size: 0.875rem;
  font-weight: 600;
  color: var(--accent);
}
.source-link { font-size: 0.75rem; color: var(--fg-muted); margin-left: auto; }
.method-doc { margin-top: 0.6rem; }
.method-doc p { margin-bottom: 0.5rem; }

/* --- TOC --- */
.toc { margin-bottom: 2rem; }
.toc ul { list-style: none; column-count: 2; column-gap: 2rem; }
.toc li { margin-bottom: 0.3rem; font-family: 'JetBrains Mono', monospace; font-size: 0.85rem; }
.toc .label {
  display: inline-block;
  font-family: 'Inter', sans-serif;
  font-size: 0.67rem;
  font-weight: 700;
  color: var(--fg-muted);
  background: var(--inline-code-bg);
  padding: 0.1em 0.4em;
  border-radius: 3px;
  margin-right: 0.3rem;
  vertical-align: middle;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

/* --- Class list (index) --- */
.class-list {
  list-style: none;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 0.75rem;
}
.class-list li {
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 0.875rem 1rem;
  transition: box-shadow 0.15s, border-color 0.15s;
}
.class-list li:hover { box-shadow: var(--shadow-md); border-color: var(--accent); }
.class-list li a { font-weight: 600; }
.class-list .class-summary {
  display: block;
  font-size: 0.82rem;
  color: var(--fg-muted);
  margin-top: 0.25rem;
  line-height: 1.4;
}

/* --- Hierarchy tree --- */
.hierarchy-tree { margin-bottom: 2.5rem; }
.hierarchy-tree ul {
  list-style: none;
  padding-left: 1.5rem;
  border-left: 2px solid var(--border);
}
.hierarchy-tree > ul { border-left: none; padding-left: 0; }
.hierarchy-tree li { margin: 0.25rem 0; font-size: 0.9rem; }
.hierarchy-tree a { font-weight: 500; }
.hierarchy-tree .class-summary {
  font-size: 0.8rem;
  color: var(--fg-muted);
  margin-left: 0.4rem;
}

/* --- ADR list --- */
.adr-list { list-style: none; padding: 0; margin: 1.5rem 0; }
.adr-list li { margin: 0.4rem 0; font-size: 0.95rem; }
.adr-list a { color: var(--accent); text-decoration: none; }
.adr-list a:hover { text-decoration: underline; }

/* --- Inherited methods --- */
.inherited-section { margin-top: 2.5rem; }
.inherited-section .method { background: transparent; border-style: dashed; opacity: 0.8; }

/* --- Search results --- */
.search-results { display: none; margin-bottom: 2rem; }
.search-results.active { display: block; }
.search-results h2 { border-bottom: none; margin-top: 0; }
.search-result-item { padding: 0.6rem 0; border-bottom: 1px solid var(--border); }
.search-result-item:last-child { border-bottom: none; }
.search-result-class { font-weight: 600; }
.search-result-method { font-family: 'JetBrains Mono', monospace; font-size: 0.85rem; }

/* --- Syntax highlighting (on dark code blocks) --- */
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
  margin-top: 4rem;
  padding-top: 1.25rem;
  border-top: 1px solid var(--border);
  font-size: 0.8rem;
  color: var(--fg-muted);
}

/* --- Landing page --- */
.landing-wrapper {
  flex-direction: column;
}
.landing-wrapper .sidebar-toggle { display: none; }

.landing-content {
  max-width: 1020px;
  margin: 0 auto;
  padding: 4rem 2.5rem 3rem;
  width: 100%;
}

.landing-hero {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4rem;
  align-items: center;
  margin-bottom: 5rem;
}
@media (max-width: 860px) {
  .landing-hero { grid-template-columns: 1fr; gap: 2.5rem; }
  .landing-content { padding: 2.5rem 1.5rem 2rem; }
}

.landing-hero-text h1 {
  font-size: 2.75rem;
  line-height: 1.1;
  letter-spacing: -0.04em;
  margin-bottom: 1rem;
}
.landing-tagline {
  font-size: 1.05rem;
  color: var(--fg-muted);
  line-height: 1.65;
  margin-bottom: 1.75rem;
}
.landing-cta {
  display: flex;
  gap: 0.75rem;
  flex-wrap: wrap;
  align-items: center;
}

.btn-primary {
  display: inline-block;
  background: var(--accent);
  color: #fff;
  font-weight: 600;
  font-size: 0.875rem;
  padding: 0.6rem 1.25rem;
  border-radius: 6px;
  text-decoration: none;
  transition: background 0.15s;
}
.btn-primary:hover { background: var(--accent-hover); color: #fff; text-decoration: none; }

.btn-secondary {
  display: inline-block;
  background: transparent;
  color: var(--fg-muted);
  font-weight: 500;
  font-size: 0.875rem;
  padding: 0.6rem 1.25rem;
  border-radius: 6px;
  border: 1px solid var(--border);
  text-decoration: none;
  transition: border-color 0.15s, color 0.15s;
}
.btn-secondary:hover { border-color: var(--accent); color: var(--accent); text-decoration: none; }

.landing-code-window {
  background: var(--code-bg);
  border-radius: 12px;
  overflow: hidden;
  box-shadow: var(--shadow-md);
}
.code-window-bar {
  background: #27272A;
  padding: 0.65rem 1rem;
  display: flex;
  align-items: center;
  gap: 0.4rem;
}
.code-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }
.code-dot-r { background: #EF4444; }
.code-dot-y { background: #F59E0B; }
.code-dot-g { background: #10B981; }
.code-window-title {
  margin-left: 0.5rem;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.72rem;
  color: #52525B;
}
.code-window-body { padding: 1.25rem 1.5rem; }
.code-window-body pre {
  margin: 0; padding: 0; background: none;
  font-size: 0.82rem;
  line-height: 1.75;
}
.code-window-body pre code { background: none; color: var(--code-fg); padding: 0; }

.landing-section-label {
  font-size: 0.72rem;
  font-weight: 700;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--accent);
  margin-bottom: 1rem;
}
.landing-cards {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}
.landing-card {
  display: block;
  background: var(--method-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 1.5rem;
  text-decoration: none;
  color: var(--fg);
  transition: box-shadow 0.15s, border-color 0.15s, transform 0.15s;
}
.landing-card:hover {
  box-shadow: var(--shadow-md);
  border-color: var(--accent);
  transform: translateY(-1px);
  text-decoration: none;
}
.landing-card-emoji { font-size: 1.35rem; margin-bottom: 0.65rem; display: block; }
.landing-card h2 {
  font-size: 0.95rem;
  margin-top: 0; margin-bottom: 0.4rem;
  border-bottom: none; padding-bottom: 0;
  color: var(--fg); font-weight: 600;
}
.landing-card p {
  font-size: 0.875rem;
  color: var(--fg-muted);
  margin-bottom: 0;
  line-height: 1.5;
}
.landing-links {
  text-align: center;
  margin-top: 2rem;
  font-size: 0.875rem;
  color: var(--fg-muted);
}

/* --- Prose pages --- */
.prose-content { max-width: 760px; }
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
