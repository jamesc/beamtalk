// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Serialized call-target types and the runtime/AST symbol and
//! call-hierarchy conversions shared by the navigation handlers.

use super::convert::{
    method_symbol_kind_and_detail, path_to_uri, span_to_range, zero_width_range_for_line,
};
use std::collections::HashSet;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use beamtalk_language_service::queries::all_sends_query::{ReceiverKind, find_all_sends_in_source};
use beamtalk_language_service::{
    CallHierarchyTarget, Location as BtLocation, NavQuery, NavSite, NavSymbolClass,
    Position as BtPosition, RuntimeLocation, SimpleLanguageService, nav_site_to_location,
};
use camino::Utf8PathBuf;
use tower_lsp::lsp_types::{
    CallHierarchyItem, CallHierarchyOutgoingCall, Position, Range, SymbolInformation, SymbolKind,
    TypeHierarchyItem, Url,
};

/// serializable shape of a [`CallHierarchyTarget`] stored on the
/// `data` field of a [`CallHierarchyItem`] so the incoming/outgoing
/// follow-up requests can re-construct enough context to dispatch.
///
/// The LSP spec is explicit that `data` is the channel for preserving
/// prepare-side state across the three-call flow (prepare → incoming /
/// outgoing): `data` is opaque to the editor, round-tripped verbatim.
/// We encode everything we need to dispatch incoming (selector) and
/// outgoing (selector + file + body range to walk) calls.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(in crate::server) struct SerializedCallTarget {
    /// Selector of the method this item refers to. Drives the senders-of
    /// dispatch for incoming calls and serves as the displayable name.
    pub(in crate::server) selector: String,
    /// Enclosing class name when the prepare hit was on a method-definition
    /// header, `None` for a call-site hit. Outgoing-calls dispatch refuses
    /// to walk a body when this is `None` (no method context to anchor on).
    #[serde(default)]
    pub(in crate::server) class_name: Option<String>,
    /// Whether the method is class-side. Mirrored to the runtime-attached
    /// senders query; the AST walker is class-agnostic.
    #[serde(default)]
    pub(in crate::server) class_side: bool,
    /// File path the prepare hit landed in — used by outgoing-calls to
    /// re-locate the method body in the open document set.
    pub(in crate::server) file: String,
    /// Start byte offset of the method-definition span in `file`. Combined
    /// with `range_end` to extract the body slice for the outgoing AST
    /// walk.
    pub(in crate::server) range_start: u32,
    /// End byte offset of the method-definition span in `file`.
    pub(in crate::server) range_end: u32,
}
impl SerializedCallTarget {
    /// Build from a [`CallHierarchyTarget`] produced by the prepare
    /// classifier.
    fn from_target(target: &CallHierarchyTarget) -> Self {
        Self {
            selector: target.selector.to_string(),
            class_name: target
                .class_name
                .as_ref()
                .map(std::string::ToString::to_string),
            class_side: target.class_side,
            file: target.file.as_str().to_string(),
            range_start: target.range.start(),
            range_end: target.range.end(),
        }
    }

    /// Decode from a [`CallHierarchyItem`]'s `data` field, returning `None`
    /// when the field is absent or malformed. The LSP spec does not
    /// guarantee `data` survives every editor round-trip (some clients
    /// scrub it), so callers should treat `None` as "the editor didn't
    /// preserve our context — answer with no calls" rather than as an
    /// error condition.
    pub(in crate::server) fn from_item(item: &CallHierarchyItem) -> Option<Self> {
        let data = item.data.as_ref()?;
        serde_json::from_value(data.clone()).ok()
    }
}
/// Build a [`CallHierarchyItem`] from a prepare-side target, embedding
/// the [`SerializedCallTarget`] payload on `data` so the follow-up
/// incoming/outgoing requests can dispatch without re-classifying.
///
/// `kind` is always [`SymbolKind::METHOD`] — call hierarchy items are
/// methods by definition. `detail` includes the class name (with a
/// `class` suffix for class-side methods) when known, mirroring the
/// document-symbol convention.
pub(in crate::server) fn target_to_lsp_item(
    target: &CallHierarchyTarget,
    uri: &Url,
    source: &str,
) -> Option<CallHierarchyItem> {
    let range = span_to_range(target.range, source);
    let selection_range = span_to_range(target.selection_range, source);
    let detail = target
        .class_name
        .as_ref()
        .map(|cn| format_class_detail(cn.as_str(), target.class_side));
    let data = serde_json::to_value(SerializedCallTarget::from_target(target)).ok()?;
    Some(CallHierarchyItem {
        name: target.selector.to_string(),
        kind: SymbolKind::METHOD,
        tags: None,
        detail,
        uri: uri.clone(),
        range,
        selection_range,
        data: Some(data),
    })
}
/// Format a class-name detail string for the `detail` field on a
/// `CallHierarchyItem`. Mirrors the workspace-symbol convention of
/// rendering class-side methods with a `class` suffix so the editor
/// shows `Counter` vs `Counter class` in the hover.
pub(in crate::server) fn format_class_detail(class: &str, class_side: bool) -> String {
    if class_side {
        format!("{class} class")
    } else {
        class.to_string()
    }
}
/// walk a method body slice for outgoing calls.
///
/// Reads sends via [`find_all_sends_in_source`] (the same query that backs
/// the stdlib `SystemNavigation messagesSentBy:`), drops Erlang FFI sends,
/// and emits one [`CallHierarchyOutgoingCall`] per remaining send. Lines
/// are 1-based relative to `body_slice`; the LSP layer adds
/// `body_start_pos.line` (zero-based) so per-call ranges land at the right
/// absolute line in the file. Column is approximated to 0 because the
/// underlying query records only line granularity for selector tokens — a
/// precise column would require re-lexing the slice for each hit.
pub(in crate::server) fn outgoing_calls_for_body(
    body_slice: &str,
    body_start_pos: tower_lsp::lsp_types::Position,
    uri: &Url,
) -> Vec<CallHierarchyOutgoingCall> {
    let hits = find_all_sends_in_source(body_slice);
    let mut calls = Vec::new();
    for hit in hits {
        if hit.receiver == ReceiverKind::ErlangFfi {
            continue;
        }
        // 1-based line within the body slice, offset by the body's
        // starting line within the file. `find_all_sends_in_source`
        // returns at least 1 for any real hit (the `.max(1)` in its
        // implementation), so subtracting 1 is safe.
        let abs_line = body_start_pos.line + hit.line.saturating_sub(1);
        let range = Range {
            start: Position::new(abs_line, 0),
            end: Position::new(abs_line, 0),
        };
        let to = CallHierarchyItem {
            name: hit.selector.clone(),
            kind: SymbolKind::METHOD,
            tags: None,
            detail: None,
            uri: uri.clone(),
            range,
            selection_range: range,
            data: None,
        };
        calls.push(CallHierarchyOutgoingCall {
            to,
            from_ranges: vec![range],
        });
    }
    calls
}
/// Compute the AST-known declaration sites that correspond to the
/// symbol the cursor is sitting on, in the same classification order
/// [`SimpleLanguageService::references_query_at`] uses.
///
/// * Cursor on a selector (call site or method header) →
///   [`SimpleLanguageService::find_selector_declarations`].
/// * Cursor on a class-name identifier known to the hierarchy →
///   [`SimpleLanguageService::find_class_declarations`].
/// * Cursor on a local identifier (parameter, local variable) → empty —
///   locals are scope-bound declarations, but the cold-file
///   `find_references` walker emits all matching identifier spans without
///   distinguishing the binding site, so we have nothing to subtract.
///
/// Caller uses the returned `(file, span)` pairs as a "decl set" to filter
/// out of the AST `find_references` result when the LSP
/// `context.includeDeclaration` flag is `false`.
pub(in crate::server) fn ast_declarations_for_cursor(
    svc: &SimpleLanguageService,
    file: &Utf8PathBuf,
    position: BtPosition,
) -> Vec<BtLocation> {
    let Some(query) = svc.references_query_at(file, position) else {
        // ADR 0108 Phase 8: `references_query_at` is always
        // `None` for a type-alias name (see its doc — aliases have no
        // runtime representation for `NavQuery::ReferencesTo` to target),
        // so the usual match below never sees one. Check the alias
        // namespace directly so `include_declaration = false` still
        // excludes the alias's own declaration site from a cold-file
        // `references` response.
        return svc
            .alias_name_at(file, position)
            .map(|name| svc.find_class_declarations(name.as_str()))
            .unwrap_or_default();
    };
    match query {
        NavQuery::SendersOf(selector) | NavQuery::ImplementorsOf(selector) => {
            svc.find_selector_declarations(selector.as_str())
        }
        NavQuery::ReferencesTo(class_name) => svc.find_class_declarations(class_name.as_str()),
    }
}
/// Translate a list of `beamtalk-core` `Location` values to LSP
/// `Location`s by re-reading each file's cached source for the column
/// math.
///
/// Drops entries whose file source isn't cached (deleted file, unloaded
/// dependency) or whose path can't be converted to a `file://` URI.
pub(in crate::server) fn bt_locations_to_lsp(
    svc: &SimpleLanguageService,
    locations: Vec<BtLocation>,
) -> Vec<tower_lsp::lsp_types::Location> {
    locations
        .into_iter()
        .filter_map(|loc| {
            let source = svc.file_source(&loc.file)?;
            let range = span_to_range(loc.span, &source);
            Some(tower_lsp::lsp_types::Location {
                uri: path_to_uri(&loc.file)?,
                range,
            })
        })
        .collect()
}
/// Append `extras` to `base`, skipping any LSP `Location` already
/// present (matched by `(uri, range)`). Used to overlay declaration sites
/// onto runtime-attached `textDocument/references` results without
/// duplicating entries the runtime already returned.
///
/// `lsp_types::Range` does not implement `Hash`, so the key flattens it
/// to a 4-tuple of `(start.line, start.character, end.line, end.character)`
/// alongside the URI.
pub(in crate::server) fn merge_locations(
    base: &mut Vec<tower_lsp::lsp_types::Location>,
    extras: Vec<tower_lsp::lsp_types::Location>,
) {
    fn key(loc: &tower_lsp::lsp_types::Location) -> (Url, u32, u32, u32, u32) {
        (
            loc.uri.clone(),
            loc.range.start.line,
            loc.range.start.character,
            loc.range.end.line,
            loc.range.end.character,
        )
    }
    let mut seen: HashSet<(Url, u32, u32, u32, u32)> = base.iter().map(key).collect();
    for loc in extras {
        if seen.insert(key(&loc)) {
            base.push(loc);
        }
    }
}
/// Convert a runtime-supplied `NavSite` into an LSP `Location`.
///
/// Steps:
/// 1. Use [`nav_site_to_location`] (in `beamtalk-core`) to resolve the
///    runtime's `source_file` path against workspace roots.
/// 2. Translate the 1-based runtime line to a zero-width LSP `Range`
///    anchored at the start of the line. (The runtime tracks line, not
///    column, so a finer `Range` would require re-reading the file —
///    deferred to per-method consumers if they need it.)
/// 3. Build the `file://` URI from the resolved path.
///
/// Returns `None` when:
/// * The site has no backing `.bt` file (`source_file` is null — stdlib,
///   dynamic, bootstrap class), or
/// * The runtime reported line 0 (defensive — the runtime shouldn't).
/// * The resolved path is not file-URI-able.
pub(in crate::server) fn runtime_site_to_lsp_location(
    site: &NavSite,
    workspace_roots: &[PathBuf],
) -> Option<tower_lsp::lsp_types::Location> {
    let resolved: RuntimeLocation = nav_site_to_location(site, workspace_roots)?;
    let uri = path_to_uri(&resolved.file)?;
    let line = resolved.line.checked_sub(1)?;
    let range = Range {
        start: tower_lsp::lsp_types::Position::new(line, 0),
        end: tower_lsp::lsp_types::Position::new(line, 0),
    };
    Some(tower_lsp::lsp_types::Location { uri, range })
}
/// Convert a runtime [`NavSymbolClass`] to an LSP [`DocumentSymbol`]
/// for `textDocument/documentSymbol`.
///
/// Drops the row when:
/// * the class has no `source_file` (REPL-only / dynamic — these surface
///   in `workspace/symbol`, not the per-file outline), or
/// * the resolved `source_file` doesn't match the requested file (a
///   different class also lives in `nav-symbols`'s reply — we filter
///   here to match the AST-walker's per-file scope).
///
/// The class entry is tagged `Counter (class)` (preserving the
/// ADR 0013 outline disambiguator the cold-file path uses) and the
/// children are the class's locally-defined instance + class-side
/// method headers. Field children are omitted: the live class registry
/// doesn't expose field declarations as xref rows, so the runtime path
/// is method-only by construction. (When the AST fallback runs, fields
/// still appear — the two paths intentionally differ in detail because
/// the runtime carries the *current* class shape, not its source-text
/// declaration.)
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
pub(in crate::server) fn runtime_class_to_document_symbol(
    class: NavSymbolClass,
    requested_path: &Utf8PathBuf,
    workspace_roots: &[PathBuf],
) -> Option<tower_lsp::lsp_types::DocumentSymbol> {
    let source_file = class.source_file.as_deref()?;
    if source_file.is_empty() {
        return None;
    }
    // Canonicalise the runtime-reported `source_file` against the
    // workspace roots the same way nav-query results are canonicalised.
    // The path must equal the file the editor asked about; otherwise the
    // class belongs to a different file in the same reply.
    let resolved = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            // `nav_site_to_location` only reads `source_file` + `line`;
            // we reuse the class name here to satisfy the struct shape
            // without pulling `ecow` into this crate.
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    )?;
    if resolved.file != *requested_path {
        return None;
    }

    let class_range = zero_width_range_for_line(class.line.unwrap_or(1));
    let mut children = Vec::with_capacity(class.methods.len());
    for m in class.methods {
        // Runtime-mode methods carry `line: None` when xref has no
        // method_info entry (primitives whose source is `nil`, or a
        // method that hasn't re-registered after a hot reload). Render
        // them at row 0 — better than dropping the row entirely, which
        // would hide selectors the user knows exist.
        let line = m.line.unwrap_or(0);
        let range = zero_width_range_for_line(line);
        let (kind, detail) = method_symbol_kind_and_detail(m.class_side);
        children.push(tower_lsp::lsp_types::DocumentSymbol {
            name: m.selector.to_string(),
            detail,
            kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        });
    }

    Some(tower_lsp::lsp_types::DocumentSymbol {
        // ADR 0013 — class outline rows carry a ` (class)` suffix so the
        // editor disambiguates them from same-named selectors. Cold-file
        // path matches; we mirror it to keep the editor outline stable
        // across modes.
        name: format!("{} (class)", class.name),
        detail: None,
        kind: SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        range: class_range,
        selection_range: class_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}
/// Convert a runtime [`NavSymbolClass`] to an LSP
/// [`SymbolInformation`] for `workspace/symbol`.
///
/// Applies the `query` substring filter (case-insensitive, empty matches
/// everything) and produces:
/// * a normal source-anchored row when the class has a backing
///   `source_file` resolvable against the workspace roots, or
/// * a zero-width row anchored to the workspace root URI with the
///   detail `(no source file)` when the class has no source — so
///   REPL-loaded classes still surface in the editor's Ctrl-T picker.
///
/// Returns `None` when the class fails the substring filter, or when
/// the class has no source AND no workspace root URI is configured (no
/// safe URI to attach the row to).
pub(in crate::server) fn runtime_class_to_workspace_symbol(
    class: &NavSymbolClass,
    query_lower: &str,
    workspace_roots: &[PathBuf],
    workspace_root_uri: Option<&Url>,
) -> Option<SymbolInformation> {
    let name = class.name.to_string();
    if !query_lower.is_empty() && !name.to_ascii_lowercase().contains(query_lower) {
        return None;
    }

    let (uri, range, detail) = match class.source_file.as_deref() {
        Some(source_file) if !source_file.is_empty() => {
            let resolved = nav_site_to_location(
                &NavSite {
                    class: class.name.clone(),
                    class_side: false,
                    // `nav_site_to_location` only reads `source_file` + `line`;
                    // we reuse the class name here to satisfy the struct shape
                    // without pulling `ecow` into this crate.
                    method: class.name.clone(),
                    line: class.line.unwrap_or(1),
                    source_file: Some(source_file.to_string()),
                },
                workspace_roots,
            )?;
            let uri = path_to_uri(&resolved.file)?;
            (uri, zero_width_range_for_line(resolved.line), None)
        }
        _ => {
            // Headline win: surface source-less classes (REPL-loaded,
            // ClassBuilder, stdlib if the consumer asked for scope=all).
            // Attach to the workspace root URI with a (0, 0) range so
            // the editor renders the symbol; clicking opens the root.
            let uri = workspace_root_uri.cloned()?;
            (
                uri,
                Range {
                    start: Position::new(0, 0),
                    end: Position::new(0, 0),
                },
                Some("(no source file)".to_string()),
            )
        }
    };

    #[expect(deprecated, reason = "LSP SymbolInformation requires deprecated field")]
    let info = SymbolInformation {
        name,
        kind: SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        location: tower_lsp::lsp_types::Location { uri, range },
        container_name: detail,
    };
    Some(info)
}
/// Detect whether a runtime [`NavSymbolClass`] is backed by a
/// stdlib source file. Used by the `workspace/symbol` handler to exclude
/// stdlib classes from the runtime-attached result set, mirroring the
/// `ProjectIndex::is_stdlib_file` filter the AST-fallback path applies.
///
/// Returns `false` (i.e. *keep the row*) when:
/// * the class has no `source_file` at all (REPL-loaded / dynamic — the
///   *headline win*; these aren't stdlib by construction), or
/// * the class's `source_file` doesn't resolve against any workspace
///   root, or
/// * the resolved path isn't in the LSP's stdlib set.
pub(in crate::server) fn class_source_is_stdlib(
    class: &NavSymbolClass,
    workspace_roots: &[PathBuf],
    stdlib_paths: &HashSet<Utf8PathBuf>,
) -> bool {
    let Some(source_file) = class.source_file.as_deref() else {
        return false;
    };
    if source_file.is_empty() {
        return false;
    }
    let Some(resolved) = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    ) else {
        return false;
    };
    stdlib_paths.contains(&resolved.file)
}
/// Build a `TypeHierarchyItem` for `class_name` when its declaration
/// site is known. The selection range is collapsed to the class-name token
/// (the `span` field on the declaration `Location`), and the surrounding
/// `range` covers the same span — tower-lsp clients accept identical
/// `range`/`selectionRange` for symbols whose body the server doesn't
/// model (Beamtalk class bodies are method-level; the symbol itself is
/// the name).
///
/// `fallback_uri` is only consulted in the `declaration == None` branch —
/// when there is no indexed declaration at all, we still want to render
/// the symbol row, so we attach the originating URI with a zero-width
/// range. In the `Some(loc)` branch we never fall back: if either
/// `file_source(&loc.file)` or `path_to_uri(&loc.file)` fails the function
/// returns `None`. Returning `None` for a known-but-unloadable declaration
/// is the conservative choice — pointing the editor at the wrong file
/// would be worse than dropping the row.
pub(in crate::server) fn type_hierarchy_item(
    class_name: &str,
    declaration: Option<&beamtalk_language_service::Location>,
    svc: &SimpleLanguageService,
    fallback_uri: &Url,
) -> Option<TypeHierarchyItem> {
    let (uri, range) = if let Some(loc) = declaration {
        let source = svc.file_source(&loc.file)?;
        let range = span_to_range(loc.span, &source);
        let uri = path_to_uri(&loc.file)?;
        (uri, range)
    } else {
        // No indexed declaration — the class is real (the project index
        // knows about it; e.g. a stdlib class compiled without source) but
        // we don't have a file to point at. Attach the originating URI
        // with a zero-width range at (0, 0) so editors render the name.
        let zero = Range {
            start: tower_lsp::lsp_types::Position::new(0, 0),
            end: tower_lsp::lsp_types::Position::new(0, 0),
        };
        (fallback_uri.clone(), zero)
    };
    Some(TypeHierarchyItem {
        name: class_name.to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri,
        range,
        selection_range: range,
        data: None,
    })
}
/// Build a `TypeHierarchyItem` for a class whose declaration site
/// is not indexed in the language service (e.g. ancestor classes
/// compiled into the runtime without a `.bt` source mapping). Falls back
/// to the parent item's URI so the editor still renders the row.
///
/// Kept separate from [`type_hierarchy_item`] so the parent-URI fallback
/// is explicit at the call site — the `prepare` path passes the open
/// document, the `super-/subtypes` paths pass the parent item's URI from
/// the request.
pub(in crate::server) fn type_hierarchy_item_for_undeclared(
    class_name: &str,
    parent_uri: Url,
) -> TypeHierarchyItem {
    let zero = Range {
        start: tower_lsp::lsp_types::Position::new(0, 0),
        end: tower_lsp::lsp_types::Position::new(0, 0),
    };
    TypeHierarchyItem {
        name: class_name.to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri: parent_uri,
        range: zero,
        selection_range: zero,
        data: None,
    }
}
/// Convert a `Vec<(name, Option<Location>)>` (the shape
/// `supertypes_of` / `subtypes_of` return) into LSP items, falling back
/// to the parent item's URI for any class without an indexed declaration.
pub(in crate::server) fn collect_hierarchy_items<S>(
    rows: Vec<(S, Option<beamtalk_language_service::Location>)>,
    svc: &SimpleLanguageService,
    parent_uri: &Url,
) -> Vec<TypeHierarchyItem>
where
    S: AsRef<str>,
{
    rows.into_iter()
        .map(|(name, loc)| {
            let resolved = loc
                .as_ref()
                .and_then(|_| type_hierarchy_item(name.as_ref(), loc.as_ref(), svc, parent_uri));
            resolved.unwrap_or_else(|| {
                type_hierarchy_item_for_undeclared(name.as_ref(), parent_uri.clone())
            })
        })
        .collect()
}
