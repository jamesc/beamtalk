// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CSS asset generation for the registry site.
//!
//! **DDD Context:** CLI / Documentation
//!
//! Deliberately not shared with `commands::doc::assets` — that stylesheet's
//! selectors assume the doc site's sidebar/prose/API-reference layout, which
//! the registry site (a flat index + per-package pages, no sidebar) doesn't
//! use. The color palette mirrors it anyway, since both sites are meant to
//! read as one family under beamtalk.dev.

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::debug;

/// CSS stylesheet content for the generated registry site.
const CSS_STYLESHEET: &str = r":root {
  --bg: #FAFAF8;
  --fg: #111827;
  --fg-muted: #6B7280;
  --accent: #1E3A8A;
  --accent-hover: #1D4ED8;
  --accent-bg: #EFF6FF;
  --border: #E5E7EB;
  --code-bg: #18181B;
  --code-fg: #E4E4E7;
  --inline-code-bg: #F3F4F6;
  --card-bg: #FFFFFF;
  --nav-bg: #FFFFFF;
  --nav-h: 60px;
  --shadow: 0 1px 3px rgba(0,0,0,0.07);
  --shadow-md: 0 4px 12px rgba(0,0,0,0.12);
  --radius: 8px;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg: #0A0A09;
    --fg: #FAFAF8;
    --fg-muted: #9CA3AF;
    --accent: #60A5FA;
    --accent-hover: #93C5FD;
    --accent-bg: #1E3A8A;
    --border: #27272A;
    --code-bg: #111110;
    --code-fg: #E4E4E7;
    --inline-code-bg: #27272A;
    --card-bg: #18181B;
    --nav-bg: #0A0A09;
    --shadow: 0 1px 3px rgba(0,0,0,0.4);
    --shadow-md: 0 4px 12px rgba(0,0,0,0.5);
  }
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Inter, sans-serif;
  font-size: 15px;
  color: var(--fg);
  background: var(--bg);
  line-height: 1.7;
}

a { color: inherit; text-decoration: none; }

/* --- Top navigation bar --- */
.top-nav {
  height: var(--nav-h);
  background: var(--nav-bg);
  border-bottom: 1px solid var(--border);
  display: flex;
  align-items: center;
  padding: 0 1.5rem;
  gap: 1.5rem;
}
.nav-logo { font-weight: 700; font-size: 1.05rem; color: var(--fg); letter-spacing: -0.02em; }
.nav-links { display: flex; align-items: center; gap: 0.25rem; flex: 1; justify-content: flex-end; }
.nav-links a {
  font-size: 0.875rem;
  font-weight: 500;
  color: var(--fg-muted);
  padding: 0.55rem 0.7rem;
  border-radius: 6px;
  min-height: 44px;
  display: flex;
  align-items: center;
}
.nav-links a:hover { background: var(--accent-bg); color: var(--accent); }
.nav-links a.nav-github { border: 1px solid var(--border); }
.nav-links a.nav-github:hover { border-color: var(--accent); }

/* --- Layout --- */
.main-content {
  max-width: 860px;
  margin: 0 auto;
  padding: 2.5rem 1.5rem 3rem;
}

h1 { font-size: 1.875rem; font-weight: 700; margin-bottom: 0.5rem; letter-spacing: -0.025em; }
h2 {
  font-size: 1.2rem;
  font-weight: 600;
  margin-top: 2rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border);
}
p { margin-bottom: 0.875rem; color: var(--fg-muted); }
p.lede { font-size: 1.05rem; }

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
  padding: 1rem 1.25rem;
  border-radius: var(--radius);
  overflow-x: auto;
  -webkit-overflow-scrolling: touch;
  margin-bottom: 1.25rem;
}
pre code { background: none; color: inherit; padding: 0; font-size: 0.875rem; }

/* --- Package list (index page) --- */
.package-list {
  list-style: none;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(min(280px, 100%), 1fr));
  gap: 0.75rem;
  margin: 1.5rem 0;
}
.package-list li {
  background: var(--card-bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 1.1rem 1.25rem;
  transition: box-shadow 0.15s, border-color 0.15s;
}
.package-list li:hover { box-shadow: var(--shadow-md); border-color: var(--accent); }
.package-name { font-weight: 600; font-size: 1rem; color: var(--fg); }
.package-version {
  display: inline-block;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: var(--accent);
  background: var(--accent-bg);
  padding: 0.1em 0.5em;
  border-radius: 999px;
  margin-left: 0.5rem;
  vertical-align: middle;
}
.package-description { font-size: 0.875rem; margin-top: 0.4rem; margin-bottom: 0; }
.package-empty { color: var(--fg-muted); font-style: italic; }

/* --- Version table (detail page) --- */
table { border-collapse: collapse; width: 100%; margin-bottom: 1.25rem; font-size: 0.88rem; }
th, td { border: 1px solid var(--border); padding: 0.5rem 0.75rem; text-align: left; overflow-wrap: break-word; word-break: break-word; }
th {
  background: var(--accent-bg);
  font-weight: 600;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--accent);
}
tbody tr:hover { background: var(--accent-bg); }
td.version-cell { font-family: 'JetBrains Mono', monospace; white-space: nowrap; }
td.snippet-cell code { white-space: nowrap; }

/* --- Breadcrumb --- */
.breadcrumb { font-size: 0.8rem; color: var(--fg-muted); margin-bottom: 1rem; }
.breadcrumb a:hover { color: var(--accent); }

footer {
  max-width: 860px;
  margin: 0 auto;
  padding: 1.25rem 1.5rem 2.5rem;
  border-top: 1px solid var(--border);
  font-size: 0.8rem;
  color: var(--fg-muted);
}
footer a { text-decoration: underline; }
";

/// Write the registry site's stylesheet.
pub(super) fn write_css(output_dir: &Utf8Path) -> Result<()> {
    let css_path = output_dir.join("style.css");
    fs::write(&css_path, CSS_STYLESHEET)
        .into_diagnostic()
        .wrap_err("Failed to write style.css")?;
    debug!("Generated {}", css_path);
    Ok(())
}
