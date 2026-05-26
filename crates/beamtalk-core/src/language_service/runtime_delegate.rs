// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Runtime-attached navigation: shared types + source-location translation
//! (BT-2239, foundation for epic BT-2215).
//!
//! **DDD Context:** Language Service
//!
//! The LSP can answer navigation queries (`textDocument/references`,
//! `textDocument/implementation`, etc.) in one of two modes:
//!
//! * **Cold-file mode** — the in-process Rust AST walker
//!   ([`crate::queries`]). Used when no workspace runtime is attached, or
//!   when the `delegateToRuntime` initialization flag is off.
//! * **Runtime-attached mode** — the LSP forwards the query to the running
//!   workspace via the `nav-query` REPL op, which calls into
//!   `beamtalk_xref` (the maintained selector→sites index) and returns
//!   typed results. Reflects live patches (`Behaviour >>`, `compile:source:`,
//!   workspace flushes) that the AST walker can't see.
//!
//! This module owns the **transport-agnostic** pieces of the runtime mode:
//!
//! * [`NavQuery`] — the three navigation kinds (`SendersOf`, `ImplementorsOf`,
//!   `ReferencesTo`) plus their selector / class-name argument.
//! * [`NavSite`] — a single result row (class, method, line, source-file).
//! * [`nav_site_to_location`] — converts a `NavSite` to a
//!   [`crate::language_service::Location`] by canonicalising the runtime's
//!   `sourceFile` path against LSP workspace roots.
//!
//! The async transport (WebSocket / REPL op) and the per-method dispatch
//! seam (`Backend::delegate_nav_query`) live in `beamtalk-lsp` — they
//! depend on `tokio`, `tungstenite`, and `RuntimeClient`, which `beamtalk-
//! core` cannot pull in (DDD: dependencies flow down only).
//!
//! # Wire contract
//!
//! See `docs/repl-protocol.md` (op `nav-query`).
//!
//! Request:
//!
//! ```json
//! {"op": "nav-query", "id": "...", "kind": "senders",     "selector": "increment"}
//! {"op": "nav-query", "id": "...", "kind": "implementors", "selector": "asString"}
//! {"op": "nav-query", "id": "...", "kind": "references",  "class": "Counter"}
//! ```
//!
//! Success reply (status `["done"]`, `value` populated):
//!
//! ```json
//! {
//!   "value": {
//!     "sites": [
//!       {"class": "Counter", "class_side": false, "method": "increment",
//!        "line": 7, "source_file": "/abs/path/examples/counter.bt"}
//!     ]
//!   }
//! }
//! ```
//!
//! `source_file` is `null` for stdlib / bootstrap / dynamic classes that
//! have no backing `.bt` file; consumers must treat the row as
//! non-navigable in that case.

use crate::language_service::Position;
use camino::Utf8PathBuf;
use ecow::EcoString;
#[cfg(feature = "serde")]
use serde::Deserialize;
use std::path::{Path, PathBuf};

/// A navigation query the LSP can delegate to a running workspace.
///
/// The variants mirror `SystemNavigation` selectors:
/// * `SendersOf(sel)` — find call sites that send `sel`
/// * `ImplementorsOf(sel)` — find classes that define `sel`
/// * `ReferencesTo(cls)` — find call sites that reference class `cls`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NavQuery {
    /// All call sites sending the given selector.
    SendersOf(EcoString),
    /// All classes (instance or class-side) that implement the given selector.
    ImplementorsOf(EcoString),
    /// All call sites that reference the given class name.
    ReferencesTo(EcoString),
}

impl NavQuery {
    /// The string value of the `kind` field on the `nav-query` op.
    pub fn kind(&self) -> &'static str {
        match self {
            NavQuery::SendersOf(_) => "senders",
            NavQuery::ImplementorsOf(_) => "implementors",
            NavQuery::ReferencesTo(_) => "references",
        }
    }

    /// The selector argument, if this query carries one.
    pub fn selector(&self) -> Option<&str> {
        match self {
            NavQuery::SendersOf(s) | NavQuery::ImplementorsOf(s) => Some(s.as_str()),
            NavQuery::ReferencesTo(_) => None,
        }
    }

    /// The class-name argument, if this query carries one.
    pub fn class_name(&self) -> Option<&str> {
        match self {
            NavQuery::ReferencesTo(c) => Some(c.as_str()),
            _ => None,
        }
    }
}

/// One result row from a `nav-query` reply.
///
/// Decoded directly from the JSON payload returned by the runtime
/// (`beamtalk_xref` site records + resolved source-file path). All field
/// names match the wire shape so `serde` can derive deserialisation when
/// the `serde` feature is enabled (the LSP crate enables it).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize))]
pub struct NavSite {
    /// Class that contains the site (instance class) or whose metaclass
    /// contains the site (when `class_side` is true). The display name —
    /// no `class` suffix for class-side rows; consumers add that
    /// disambiguator when rendering.
    pub class: EcoString,

    /// `true` when the site lives in a class-side method (defined on the
    /// metaclass), `false` for instance-side.
    pub class_side: bool,

    /// The selector of the *enclosing method* that contains the call site,
    /// **or** for `implementors` queries the selector being implemented
    /// (since the query selector and the enclosing method coincide).
    pub method: EcoString,

    /// 1-based line number within the source file. The runtime stores
    /// method-defining lines as well as send / reference lines; both
    /// shapes use the same field.
    pub line: u32,

    /// Absolute path of the `.bt` file backing the class, or `None` for
    /// stdlib / bootstrap / dynamically-built classes. Consumers treat
    /// `None` as "not navigable" and skip the row.
    #[cfg_attr(feature = "serde", serde(default))]
    pub source_file: Option<String>,
}

/// JSON payload shape of a successful `nav-query` reply's `value` field.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize))]
pub struct NavQueryResponse {
    /// One entry per matching site.
    pub sites: Vec<NavSite>,
}

/// A resolved navigation target: the absolute path of the file backing
/// the [`NavSite`] plus the 1-based line number the runtime reported.
///
/// The LSP layer converts this to an LSP `Location` by re-reading the
/// file and computing a precise `Range` (selector token or full-line
/// fallback). Keeping the conversion in two steps lets the core crate
/// stay free of LSP-specific types while still owning the path-
/// canonicalisation policy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeLocation {
    /// Canonicalised filesystem path of the source file.
    pub file: Utf8PathBuf,
    /// 1-based line number reported by the runtime. The LSP layer
    /// converts to 0-based when emitting `lsp_types::Position`.
    pub line: u32,
}

/// Translate a `NavSite` into a [`RuntimeLocation`].
///
/// Returns `None` when:
/// * `source_file` is absent / `nil` (stdlib / dynamic / bootstrap class), or
/// * the source file lives outside every workspace root and isn't already an
///   absolute path the editor can open.
///
/// Path resolution:
/// 1. If `source_file` is already absolute and points at an existing file,
///    use it directly.
/// 2. Otherwise canonicalise against each workspace root in turn and pick
///    the first existing file.
/// 3. If no root matches, return the absolute form of the first root joined
///    with the relative path — the editor will surface a "file not found"
///    error rather than the LSP dropping the result silently.
///
/// The 1-based line is preserved verbatim — `beamtalk_xref` already emits
/// absolute file lines, not method-relative offsets, for the shapes
/// returned today. Consumers that need a precise LSP `Range` should
/// re-read the file and walk to the selector token; the foundation
/// surface is line-granular.
pub fn nav_site_to_location(
    site: &NavSite,
    workspace_roots: &[PathBuf],
) -> Option<RuntimeLocation> {
    let source_file = site.source_file.as_deref()?;
    if source_file.is_empty() {
        return None;
    }

    let path = resolve_source_path(source_file, workspace_roots)?;
    let utf8_path = Utf8PathBuf::from_path_buf(path).ok()?;

    Some(RuntimeLocation {
        file: utf8_path,
        line: site.line,
    })
}

/// Resolve `source_file` (as reported by the runtime) to a real filesystem
/// path.
///
/// See [`nav_site_to_location`] for the resolution policy.
fn resolve_source_path(source_file: &str, workspace_roots: &[PathBuf]) -> Option<PathBuf> {
    let candidate = Path::new(source_file);

    // 1. Already-absolute & exists → use as-is.
    if candidate.is_absolute() {
        if candidate.is_file() {
            return Some(candidate.to_path_buf());
        }
        // Absolute but missing — return it so the editor surfaces the
        // mismatch rather than the LSP swallowing the result.
        return Some(candidate.to_path_buf());
    }

    // 2. Walk workspace roots, prefer the first existing match.
    for root in workspace_roots {
        let joined = root.join(candidate);
        if joined.is_file() {
            return Some(joined);
        }
    }

    // 3. Best-effort: anchor to the first root so the URI is at least
    // absolute. Returns None when no roots are configured.
    workspace_roots.first().map(|r| r.join(candidate))
}

/// Position of the first byte of a 1-based line. Used by LSP-layer code
/// that re-reads the file and wants to convert the runtime-supplied line
/// to a real LSP `Position`/`Range`. Returns `None` when `line` is 0 (the
/// runtime should never emit 0, but defend against it).
pub fn line_to_position(line: u32) -> Option<Position> {
    if line == 0 {
        return None;
    }
    // LSP `Position` is 0-based; the runtime emits 1-based lines.
    Some(Position::new(line - 1, 0))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn site(class: &str, line: u32, source_file: Option<&str>) -> NavSite {
        NavSite {
            class: EcoString::from(class),
            class_side: false,
            method: EcoString::from("increment"),
            line,
            source_file: source_file.map(String::from),
        }
    }

    #[test]
    fn nav_query_kind_strings_match_wire_format() {
        assert_eq!(NavQuery::SendersOf("foo".into()).kind(), "senders");
        assert_eq!(
            NavQuery::ImplementorsOf("foo".into()).kind(),
            "implementors"
        );
        assert_eq!(NavQuery::ReferencesTo("Foo".into()).kind(), "references");
    }

    #[test]
    fn selector_arg_only_for_senders_and_implementors() {
        assert_eq!(NavQuery::SendersOf("foo".into()).selector(), Some("foo"));
        assert_eq!(
            NavQuery::ImplementorsOf("foo".into()).selector(),
            Some("foo")
        );
        assert_eq!(NavQuery::ReferencesTo("Foo".into()).selector(), None);
    }

    #[test]
    fn class_name_arg_only_for_references() {
        assert_eq!(NavQuery::SendersOf("foo".into()).class_name(), None);
        assert_eq!(
            NavQuery::ReferencesTo("Foo".into()).class_name(),
            Some("Foo")
        );
    }

    #[test]
    fn nav_site_to_location_returns_none_when_source_file_missing() {
        let s = site("Counter", 7, None);
        assert!(nav_site_to_location(&s, &[]).is_none());
    }

    #[test]
    fn nav_site_to_location_returns_none_when_source_file_empty() {
        let s = site("Counter", 7, Some(""));
        assert!(nav_site_to_location(&s, &[]).is_none());
    }

    #[test]
    fn nav_site_to_location_uses_absolute_path_as_is() {
        // Choose a path that exists everywhere on Linux. Doesn't need to be
        // a real `.bt` file — the resolver just checks existence.
        let s = site("Counter", 7, Some("/etc/hostname"));
        let loc = nav_site_to_location(&s, &[]).expect("path exists");
        assert_eq!(loc.file.as_str(), "/etc/hostname");
        assert_eq!(loc.line, 7);
    }

    #[test]
    fn nav_site_to_location_anchors_relative_path_to_workspace_root() {
        let s = site("Counter", 7, Some("relative/foo.bt"));
        let root = PathBuf::from("/tmp");
        let loc = nav_site_to_location(&s, &[root]).expect("anchored");
        assert!(loc.file.as_str().ends_with("relative/foo.bt"));
        assert_eq!(loc.line, 7);
    }

    #[test]
    fn line_to_position_zero_returns_none() {
        assert!(line_to_position(0).is_none());
    }

    #[test]
    fn line_to_position_converts_one_based_to_zero_based() {
        let pos = line_to_position(7).unwrap();
        assert_eq!(pos.line, 6);
        assert_eq!(pos.column, 0);
    }

    // The `nav-query` JSON round-trip is exercised in the LSP-layer
    // integration (`crates/beamtalk-lsp/src/runtime.rs::nav_query`)
    // where `serde_json` is a direct dep. Keeping the structured
    // payload assertions there avoids pulling serde_json into
    // `beamtalk-core`'s dev-dependencies just for an in-place test.
}
