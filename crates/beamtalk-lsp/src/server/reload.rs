// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Live-reload event listeners: consumes class-changed and reload-check
//! pushes from the runtime, publishes the resulting diagnostics, and tracks
//! which paths are currently open so publication can be safely deferred.

use super::convert::{path_to_uri, to_lsp_diagnostic, uri_to_path};
use super::{NavCache, ReloadDiagnosticsByUriAndOrigin};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::runtime::{ClassChangedEvent, ReloadCheckEvent, ReloadFinding, RuntimeClient};

use beamtalk_language_service::{
    LanguageService, NavSite, NavSymbolClass, SimpleLanguageService, nav_site_to_location,
};
use camino::Utf8PathBuf;
use tower_lsp::Client;
use tower_lsp::lsp_types::{DiagnosticSeverity, MessageType, Position, Range, Url};

/// Shared handle to the LSP `Backend::versions` map, used by the flush
/// listener task to ask "is this file currently open?" on each `FlushEvent`
/// without copying the whole map up-front. Wrapping rather than passing
/// `Arc<Mutex<HashMap<...>>>` directly keeps the listener's API narrow:
/// it can only check membership, not mutate.
#[derive(Clone)]
pub(crate) struct OpenPathsHandle {
    pub(in crate::server) versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
}
impl OpenPathsHandle {
    /// Returns true if `path` is currently registered as open in the LSP
    /// document table. Locks the underlying mutex briefly; the lock is
    /// released before the caller awaits, so no deadlock risk against the
    /// LSP's `std::sync::Mutex`.
    pub(in crate::server) fn contains(&self, path: &Utf8PathBuf) -> bool {
        let guard = self.versions.lock().expect("versions lock poisoned");
        guard.contains_key(path)
    }
}
/// consume `ClassChangedEvent`s from the runtime listener and
/// bump the shared nav-cache generation.
///
/// The foundation issue uses a coarse single-counter invalidation:
/// any class load / reload / method install bumps the generation, and
/// readers compare entries against the current counter. The per-method
/// children can keep the same shape if they add their
/// own per-class buckets — the listener stays the same.
///
/// Holds an `Arc` to `Backend::nav_cache` rather than a back-reference to
/// the `Backend` so the task does not keep the backend alive on its own.
/// When `ensure_runtime_attached` stores the `JoinHandle`, the task ends
/// when the handle is aborted (during a subsequent attach or backend
/// drop) or when the `class_changed_rx` channel closes (`RuntimeClient`
/// disconnect).
pub(in crate::server) async fn class_changed_listener(
    nav_cache: Arc<Mutex<NavCache>>,
    mut class_changed_rx: tokio::sync::mpsc::UnboundedReceiver<ClassChangedEvent>,
) {
    while let Some(event) = class_changed_rx.recv().await {
        tracing::debug!(
            class_name = %event.class_name,
            "class_changed_listener: invalidating nav cache"
        );
        let mut guard = nav_cache.lock().expect("nav_cache lock poisoned");
        guard.invalidate();
    }
    tracing::debug!("class_changed_listener: channel closed, exiting");
}
/// ADR 0105 Phase 1: consume `ReloadCheckEvent`s from the runtime
/// listener and publish/clear reload-induced diagnostics on the affected
/// caller classes' documents.
///
/// For every owner in the event's `checked_owners` (the clearing-by-
/// replacement set — see [`ReloadCheckEvent`]'s doc), this:
/// 1. Resolves the owner class name to a document URI via `nav-symbols`
///    (one round-trip per event, not per owner — `nav-symbols` already
///    returns every user class).
/// 2. Builds LSP diagnostics from the event's findings restricted to that
///    owner (`reload_finding_to_lsp_diagnostics`), one per call site so a
///    finding with several sends in the same method surfaces at each line.
/// 3. Replaces (never merges) that `(owner, changed_class)` origin's entry
///    within that URI's bucket in `reload_diagnostics` — an origin with no
///    current findings gets its entry removed, which is exactly how a
///    clean re-check clears a stale diagnostic, without touching a
///    *different* class's entry that happens to share the same file, NOR a
///    *different changed class*'s still-valid findings for the *same*
///    owner (`ReloadDiagnosticsByUriAndOrigin`) — a caller broken by two
///    independently-reloading classes must not have one reload's
///    replacement silently discard the other's still-valid finding.
/// 4. Republishes the merged (static + every origin's reload) diagnostic
///    set for that URI.
///
/// An owner with no resolvable source file (a REPL-only / dynamically
/// defined class, or one `nav-symbols` doesn't know about) is skipped — the
/// LSP has nothing to attach a `publishDiagnostics` notification to.
/// Silent: this is a normal, expected case (surface-parity is preserved by
/// the REPL notice and workspace UI, which don't need a `.bt` file to
/// attribute a finding to).
pub(in crate::server) async fn reload_check_listener(
    client: Client,
    runtime: RuntimeClient,
    workspace_roots: Vec<PathBuf>,
    service: Arc<Mutex<SimpleLanguageService>>,
    reload_diagnostics: Arc<std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>>,
    mut reload_check_rx: tokio::sync::mpsc::UnboundedReceiver<ReloadCheckEvent>,
    open_paths: OpenPathsHandle,
) {
    while let Some(event) = reload_check_rx.recv().await {
        if event.checked_owners.is_empty() {
            continue;
        }
        // Echo the summary line every other surface shows (REPL notice,
        // workspace UI header — "N callers re-checked, M stale") to the LSP
        // client's output channel. Squiggles alone don't carry the
        // clean-recheck count, and `window/logMessage` is the LSP's own
        // best-effort notice channel (not a `publishDiagnostics` — this
        // never affects the diagnostic set).
        let cap_suffix = event
            .cap_note
            .as_deref()
            .map(|n| format!(" ({n})"))
            .unwrap_or_default();
        client
            .log_message(
                MessageType::INFO,
                format!(
                    "reload check: {}>>{} {}; {} checked, {} not checked{cap_suffix}",
                    event.changed_class,
                    event.changed_selector,
                    event.classification,
                    event.checked,
                    event.not_checked
                ),
            )
            .await;
        let classes = match runtime.nav_symbols(Some("user")).await {
            Ok(classes) => classes,
            Err(e) => {
                tracing::warn!(error = %e, "reload_check_listener: nav_symbols failed");
                continue;
            }
        };
        for owner in &event.checked_owners {
            let Some(class) = classes.iter().find(|c| c.name.as_str() == owner.as_str()) else {
                tracing::debug!(
                    owner,
                    "reload_check_listener: owner has no known source file, skipping"
                );
                continue;
            };
            let Some(uri) = resolve_class_uri(class, &workspace_roots) else {
                tracing::debug!(owner, "reload_check_listener: could not resolve class URI");
                continue;
            };
            let diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = event
                .findings
                .iter()
                .filter(|f| &f.owner == owner)
                .flat_map(reload_finding_to_lsp_diagnostics)
                .collect();
            {
                let mut guard = reload_diagnostics
                    .lock()
                    .expect("reload_diagnostics lock poisoned");
                // `owner == changed_class` means this owner's OWN source
                // just changed — the server unconditionally full-wipes it
                // (`beamtalk_workspace_findings_store:clear_owner/1`) before
                // any scoped replace, so every origin bucket for this owner
                // is stale, not just the one keyed to `changed_class`. Every
                // *other* owner only had its `(owner, changed_class)` origin
                // scoped-replaced server-side (`put_owner_origin/3`), so a
                // different changed class's still-valid finding for the
                // same owner must survive.
                if owner == &event.changed_class {
                    if let Some(by_origin) = guard.get_mut(&uri) {
                        by_origin.retain(|(o, _cc), _| o != owner);
                        if by_origin.is_empty() {
                            guard.remove(&uri);
                        }
                    }
                    if !diagnostics.is_empty() {
                        guard
                            .entry(uri.clone())
                            .or_default()
                            .insert((owner.clone(), event.changed_class.clone()), diagnostics);
                    }
                } else {
                    let origin_key = (owner.clone(), event.changed_class.clone());
                    if diagnostics.is_empty() {
                        // An empty `diagnostics` list is ambiguous: it means
                        // either a genuinely clean re-check (no finding at
                        // all for this owner — safe to clear), or a finding
                        // that exists but is siteless (no xref call-site
                        // line to anchor a `Diagnostic` to). Only the first
                        // case should clear the origin; conflating the two
                        // would silently drop a real finding whenever it
                        // happens to have no placeable site.
                        let has_finding_for_owner =
                            event.findings.iter().any(|f| &f.owner == owner);
                        if has_finding_for_owner {
                            tracing::warn!(
                                owner,
                                changed_class = %event.changed_class,
                                "reload_check_listener: finding present but produced no \
                                 placeable diagnostics (siteless); leaving prior diagnostics \
                                 for this origin untouched"
                            );
                        } else if let Some(by_origin) = guard.get_mut(&uri) {
                            by_origin.remove(&origin_key);
                            if by_origin.is_empty() {
                                guard.remove(&uri);
                            }
                        }
                    } else {
                        guard
                            .entry(uri.clone())
                            .or_default()
                            .insert(origin_key, diagnostics);
                    }
                }
            }
            // No tracked document version for a URI the editor may not even
            // have open — `publishDiagnostics`' `version` field is optional
            // per the LSP spec, so omitting it is correct here (unlike the
            // flush listener, which only touches already-open buffers).
            //
            // If the URI *is* open and startup preload
            // is still in-flight, `publish_diagnostics_impl` would recompute
            // `svc.diagnostics()` against the same partially-populated
            // `ProjectIndex` that caused the original false `Unresolved
            // class` positive, and its send could still race
            // `republish_open_diagnostics`'s later, correct one — the same
            // channel-ordering race `Backend::publish_diagnostics` was fixed
            // to avoid, just reached through this listener instead of
            // `did_open`/`did_change`/`did_save`. Skip the send in that case:
            // the `reload_diagnostics` map above is already updated, and
            // `republish_open_diagnostics` is guaranteed to pick it up in its
            // one, correct publish once preload completes.
            if !should_defer_reload_publish_for_preload(&service, &open_paths, &uri) {
                publish_diagnostics_impl(&client, &service, &reload_diagnostics, &uri, None).await;
            }
        }
    }
    tracing::debug!("reload_check_listener: channel closed, exiting");
}
/// Seed `reload_diagnostics` with any reload-induced findings that already
/// existed in `beamtalk_workspace_findings_store` before this client
/// attached (ADR 0105 surface-parity gap) — the `reload_check`
/// push channel [`reload_check_listener`] consumes only ever delivers *new*
/// outcomes, so a fresh LSP process attaching for the first time (e.g. the
/// editor just started, or restarted the language server after a crash)
/// would otherwise show nothing for a caller until the next reload happens
/// to touch it again.
///
/// Called once from [`Backend::ensure_runtime_attached`], after the push
/// listener is spawned but before the runtime client is handed back to
/// callers, so the first [`Backend::publish_diagnostics`] for any
/// already-open document picks the snapshot up through the normal merge
/// path (`publish_diagnostics_impl`). Findings are grouped by `(owner,
/// changed_class)` — the same origin key `reload_diagnostics` uses — so each
/// independent contribution to an owner's diagnostics stays an
/// independently-clearing entry once live pushes start arriving, exactly
/// mirroring [`reload_check_listener`]'s per-origin bucketing.
///
/// A document already open when the seed completes would otherwise have to
/// wait for an unrelated edit to surface its pre-existing findings, so any
/// URI touched by the seed is republished immediately.
///
/// Best-effort: a transport failure here only means the LSP starts cold —
/// exactly the behaviour before this feature existed — so it must not fail
/// the attach itself, only log and return.
///
/// **Additive-only, and only correct because it runs at most once per
/// `Backend`:** this never *clears* `reload_diagnostics`, it only inserts.
/// That is sound today because `ensure_runtime_attached` caches `self.runtime`
/// forever once set (nothing ever resets it back to `None`), so this
/// function's single call site only ever runs against an empty
/// `reload_diagnostics` map — there is no live LSP *process* reconnect path
/// today, only a fresh process attaching once. If a same-process reconnect
/// path is ever added, this must change to clear stale entries for origins
/// no longer in the fresh snapshot (not a blanket clear — a concurrent
/// `reload_check` push landing first, see the race note below, must not be
/// wiped) rather than staying purely additive, or a finding cleared while
/// disconnected could remain stuck forever. Two concurrent
/// `ensure_runtime_attached` callers *can* both pass the attach-cache's
/// `None` check and both reach this function (the existing "loser client"
/// race), but that is harmless here: both compute the same snapshot and the
/// inserts are idempotent over the same keys.
///
/// **Known narrow race, accepted:** the `reload-findings` RPC and the
/// `reload_check` push listener are two independent round-trips against the
/// same live store, so a real reload that clears an origin can have its
/// `ReloadCheckCompleted` push processed by [`reload_check_listener`]
/// *before* this function's own (slightly earlier) snapshot finishes being
/// written — in which case this seed re-inserts the origin the push had
/// already correctly cleared. This mirrors the "loser client" race
/// [`Backend::ensure_runtime_attached`] already documents and accepts for
/// the same reason: it needs a reload to land in the exact window between
/// attach and seed completion. It is not as fully self-healing as it may
/// first look: a re-inserted *clearing* finding only disappears the next
/// time `changed_class` (not just any reload touching `owner`) is reloaded
/// again — which may not happen again in the session — so the practical
/// effect is a stale squiggle that behaves exactly like the unseeded
/// baseline (nothing seeded) for that one origin, not a regression beyond
/// it. A fully race-free version would need the findings store to expose a
/// generation/version the client could compare against, which is out of
/// scope here.
pub(in crate::server) async fn seed_reload_diagnostics(
    client: &Client,
    runtime_client: &RuntimeClient,
    workspace_roots: &[PathBuf],
    service: &Mutex<SimpleLanguageService>,
    reload_diagnostics: &std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>,
    open_paths: &OpenPathsHandle,
) {
    // Awaited inline in the attach path (not `tokio::spawn`ed) so that by the
    // time `ensure_runtime_attached` returns, any already-open document has
    // already been republished with the seeded findings — a spawned version
    // would race the caller's own next `publish_diagnostics` call for no
    // real benefit, since first-attach latency here is bounded by two RPC
    // round-trips (`reload-findings` + `nav-symbols`) against a workspace
    // already proven reachable by the connect this immediately follows.
    let findings = match runtime_client.reload_findings().await {
        Ok(findings) => findings,
        Err(e) => {
            tracing::warn!(error = %e, "seed_reload_diagnostics: reload-findings failed");
            return;
        }
    };
    if findings.is_empty() {
        return;
    }
    let classes = match runtime_client.nav_symbols(Some("user")).await {
        Ok(classes) => classes,
        Err(e) => {
            tracing::warn!(error = %e, "seed_reload_diagnostics: nav_symbols failed");
            return;
        }
    };
    let by_origin = group_findings_by_origin(findings);
    let mut touched_uris: HashSet<Url> = HashSet::new();
    for ((owner, changed_class), owner_findings) in by_origin {
        let Some(class) = classes.iter().find(|c| c.name.as_str() == owner.as_str()) else {
            tracing::debug!(
                owner,
                "seed_reload_diagnostics: owner has no known source file, skipping"
            );
            continue;
        };
        let Some(uri) = resolve_class_uri(class, workspace_roots) else {
            tracing::debug!(
                owner,
                "seed_reload_diagnostics: could not resolve class URI"
            );
            continue;
        };
        let diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = owner_findings
            .iter()
            .flat_map(reload_finding_to_lsp_diagnostics)
            .collect();
        if diagnostics.is_empty() {
            continue;
        }
        {
            let mut guard = reload_diagnostics
                .lock()
                .expect("reload_diagnostics lock poisoned");
            guard
                .entry(uri.clone())
                .or_default()
                .insert((owner, changed_class), diagnostics);
        }
        touched_uris.insert(uri);
    }
    for uri in touched_uris {
        // Same deferral as `reload_check_listener` —
        // `ensure_runtime_attached` (this function's only caller) runs
        // on-demand, independently of the startup preload sequence, so it
        // can race it. Skip the send for an open URI while preload is
        // in-flight; `reload_diagnostics` above is already updated, so
        // `republish_open_diagnostics` picks this seed up correctly once
        // preload completes.
        if !should_defer_reload_publish_for_preload(service, open_paths, &uri) {
            publish_diagnostics_impl(client, service, reload_diagnostics, &uri, None).await;
        }
    }
}
/// Recompute static diagnostics for `uri`, merge in any live reload-induced
/// diagnostics (ADR 0105 Phase 1), and publish the combined set.
///
/// A free function (not a `&self` method) so both [`Backend::publish_diagnostics`]
/// and the detached `reload_check_listener` task — which only holds `Arc`
/// clones of the pieces it needs, not a `Backend` reference, since it
/// outlives any single request handler's borrow — can share one
/// implementation. LSP's `publishDiagnostics` fully replaces what the editor
/// shows for a URI (no incremental-append notification), so every call,
/// whether triggered by a normal edit/save or by a reload-check push, must
/// include both sources or one silently clobbers the other.
pub(in crate::server) async fn publish_diagnostics_impl(
    client: &Client,
    service: &Mutex<SimpleLanguageService>,
    reload_diagnostics: &std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>,
    uri: &Url,
    version: Option<i32>,
) {
    // Stdlib virtual documents have no user-facing diagnostics.
    if uri.scheme() == "beamtalk-stdlib" {
        return;
    }
    let Some(path) = uri_to_path(uri) else {
        return;
    };
    let mut diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = {
        let svc = service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        svc.diagnostics(&path)
            .into_iter()
            .map(|d| to_lsp_diagnostic(&d, source.as_deref()))
            .collect()
    };
    {
        let reload = reload_diagnostics
            .lock()
            .expect("reload_diagnostics lock poisoned");
        // Flatten every owner's diagnostics for this URI — a file can
        // define more than one class (see `ReloadDiagnosticsByUriAndOrigin`'s
        // doc), each with its own independently clearing entry.
        if let Some(by_owner) = reload.get(uri) {
            diagnostics.extend(by_owner.values().flatten().cloned());
        }
    }
    client
        .publish_diagnostics(uri.clone(), diagnostics, version)
        .await;
}
/// True if publishing diagnostics for `uri` right now
/// would risk the same startup-preload notification race
/// [`Backend::publish_diagnostics`] was fixed to avoid for
/// `did_open`/`did_change`/`did_save` — i.e. startup preload is still
/// in-flight *and* `uri` is currently open in the editor.
///
/// A closed-file URI is always safe to publish immediately: nothing else
/// (`republish_open_diagnostics` only iterates open files) will ever
/// resend it, so deferring would drop the diagnostic forever — matching
/// why `publish_diagnostics_impl` itself isn't gated unconditionally. An
/// open-file URI is safe to defer: `republish_open_diagnostics` is
/// guaranteed to (re)publish it, correctly, once preload finishes.
pub(in crate::server) fn should_defer_reload_publish_for_preload(
    service: &Mutex<SimpleLanguageService>,
    open_paths: &OpenPathsHandle,
    uri: &Url,
) -> bool {
    let Some(path) = uri_to_path(uri) else {
        return false;
    };
    if !open_paths.contains(&path) {
        return false;
    }
    let svc = service.lock().expect("service lock poisoned");
    svc.is_preload_in_progress()
}
/// Group a flat findings snapshot by `(owner, changed_class)` — the same
/// origin key [`ReloadDiagnosticsByUriAndOrigin`] uses — so
/// [`seed_reload_diagnostics`] can seed each independent contribution to an
/// owner's diagnostics as its own independently-clearing entry, exactly
/// mirroring [`reload_check_listener`]'s per-origin bucketing. Pulled out as
/// a pure function (no I/O) so this grouping — the one piece of genuinely
/// new logic `seed_reload_diagnostics` adds beyond what
/// [`reload_check_listener`] already does per-event — is unit-testable
/// without a live `RuntimeClient`.
pub(in crate::server) fn group_findings_by_origin(
    findings: Vec<ReloadFinding>,
) -> HashMap<(String, String), Vec<ReloadFinding>> {
    let mut by_origin: HashMap<(String, String), Vec<ReloadFinding>> = HashMap::new();
    for finding in findings {
        by_origin
            .entry((finding.owner.clone(), finding.changed_class.clone()))
            .or_default()
            .push(finding);
    }
    by_origin
}
/// Resolve a `NavSymbolClass`'s `source_file` to a document `Url`, the same
/// way [`runtime_class_to_document_symbol`] does — reusing
/// `nav_site_to_location` for the workspace-root canonicalisation via a
/// synthetic single-site `NavSite`.
pub(in crate::server) fn resolve_class_uri(
    class: &NavSymbolClass,
    workspace_roots: &[PathBuf],
) -> Option<Url> {
    let source_file = class.source_file.as_deref()?;
    if source_file.is_empty() {
        return None;
    }
    let resolved = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    )?;
    path_to_uri(&resolved.file)
}
/// Build one LSP `Diagnostic` per call site in a reload-induced finding
/// (ADR 0105 Phase 1). Uses the site's xref-recorded line number —
/// not the finding's `start`/`end` byte-offset span, since those are
/// offsets into the *live combined class source* the compiler re-checked
/// against, and there is no existing machinery to map that back onto an
/// on-disk position the way `nav_site_to_location`/`line_to_position`
/// already do for a line number (see `ReloadFinding::start`'s doc).
pub(in crate::server) fn reload_finding_to_lsp_diagnostics(
    finding: &ReloadFinding,
) -> Vec<tower_lsp::lsp_types::Diagnostic> {
    let severity = match finding.severity.as_str() {
        "error" => DiagnosticSeverity::ERROR,
        "warning" => DiagnosticSeverity::WARNING,
        _ => DiagnosticSeverity::HINT,
    };
    // `reload check (<classification> of <ChangedClass>>><selector>): …`
    // attributes the finding the same way the REPL notice / ADR demo do
    // (`format_reload_check_notice` in `beamtalk-cli`), so a squiggle's
    // hover text answers "why is this here" without cross-referencing
    // another surface.
    let mut message = format!(
        "reload check ({} of {}>>{}): {}",
        finding.classification, finding.changed_class, finding.selector, finding.message
    );
    if let Some(note) = &finding.note {
        message.push_str("\n  = ");
        message.push_str(note);
    }
    let code = finding
        .category
        .clone()
        .map(tower_lsp::lsp_types::NumberOrString::String);
    finding
        .sites
        .iter()
        .map(|site| {
            let row = site.line.saturating_sub(1);
            // Highlight the whole line: `character: u32::MAX` is the LSP
            // convention for "end of line" (the flush listener's
            // `workspace/applyEdit` uses the same trick for "end of file")
            // — clients clamp to the line's actual length. Byte-precise
            // spans aren't available here (see the doc comment above).
            let range = Range {
                start: Position::new(row, 0),
                end: Position::new(row, u32::MAX),
            };
            tower_lsp::lsp_types::Diagnostic {
                range,
                severity: Some(severity),
                code: code.clone(),
                source: Some("beamtalk (reload)".into()),
                message: format!("{message} (in {})", site.method),
                ..Default::default()
            }
        })
        .collect()
}
