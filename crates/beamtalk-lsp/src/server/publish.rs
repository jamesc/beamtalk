// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics publication for open documents, and the cross-file republish
//! worker (BT-5188).
//!
//! [`Publisher`] holds, by `Arc`, exactly the state a publish reads, so the
//! [`Backend`](super::Backend) and a background task share one
//! implementation. [`CrossFileRepublish`] coalesces "a file's interface
//! changed, re-check the other open files" requests into one pass per burst:
//! a client restoring N tabs costs O(N) analyses rather than one full pass
//! per opened file (O(N²)).

use super::ReloadDiagnosticsByUriAndOrigin;
use super::convert::{path_to_stdlib_uri, path_to_uri, uri_to_path};
use super::reload::compute_publish_diagnostics;
use crate::server::lock::LockRecover;
use beamtalk_language_service::SimpleLanguageService;
use camino::Utf8PathBuf;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex, OnceLock};
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;

/// How long the worker waits after a request before re-checking, so a burst
/// of requests (tabs restored on startup, a quick series of edits) is one
/// pass. Matches the `did_change` debounce.
const CROSS_FILE_REPUBLISH_DELAY: std::time::Duration = std::time::Duration::from_millis(150);

/// The state a diagnostics publish reads (see the module doc).
#[derive(Clone)]
pub(in crate::server) struct Publisher {
    pub(in crate::server) client: Client,
    pub(in crate::server) service: Arc<Mutex<SimpleLanguageService>>,
    pub(in crate::server) versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
    pub(in crate::server) stdlib_paths: Arc<Mutex<HashSet<Utf8PathBuf>>>,
    pub(in crate::server) reload_diagnostics: Arc<Mutex<ReloadDiagnosticsByUriAndOrigin>>,
    pub(in crate::server) publish_lock: Arc<tokio::sync::Mutex<()>>,
}

impl Publisher {
    /// Resolves a URI to the internal path key used by the language service.
    ///
    /// - For `file://` URIs: returns the real filesystem path.
    /// - For `untitled:` URIs: returns a synthetic `__untitled__/` path key.
    /// - For `beamtalk-stdlib:///ClassName.bt` URIs: looks up the real path
    ///   from `stdlib_paths`. Returns `None` for unknown class names, invalid
    ///   URI form, or ambiguous filenames.
    pub(in crate::server) fn resolve_path_for_uri(&self, uri: &Url) -> Option<Utf8PathBuf> {
        if uri.scheme() == "beamtalk-stdlib" {
            self.stdlib_uri_to_path(uri)
        } else {
            uri_to_path(uri)
        }
    }

    /// Looks up the real filesystem path for a `beamtalk-stdlib:///ClassName.bt` URI.
    ///
    /// Returns `None` for invalid URI form, unknown class names, or ambiguous filenames.
    fn stdlib_uri_to_path(&self, uri: &Url) -> Option<Utf8PathBuf> {
        // Only canonical form: no host, no query, no fragment.
        if uri.host().is_some() || uri.query().is_some() || uri.fragment().is_some() {
            return None;
        }
        let path = uri.path().trim_start_matches('/');
        // Require non-empty, no sub-paths, .bt extension only.
        if path.is_empty()
            || path.contains('/')
            || !std::path::Path::new(path)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("bt"))
        {
            return None;
        }

        let stdlib_paths = self.stdlib_paths.lock_recover();
        let mut matches = stdlib_paths.iter().filter(|p| p.file_name() == Some(path));
        let first = matches.next()?.clone();
        // Ambiguous: multiple files with the same name → None.
        if matches.next().is_some() {
            return None;
        }
        Some(first)
    }

    fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
        let path = self.resolve_path_for_uri(uri)?;
        let versions = self.versions.lock_recover();
        versions.get(&path).copied()
    }

    /// True while the editor has `uri` open (it has a recorded version).
    fn is_open_uri(&self, uri: &Url) -> bool {
        self.resolve_path_for_uri(uri)
            .is_some_and(|path| self.versions.lock_recover().contains_key(&path))
    }

    /// Publishes diagnostics for an open document (see
    /// [`Backend::publish_diagnostics`](super::Backend::publish_diagnostics)).
    pub(in crate::server) async fn publish(&self, uri: &Url) {
        // a didOpen/didChange/didSave racing the LSP's startup
        // workspace preload can compute diagnostics against a partially-
        // populated `ProjectIndex` (e.g. a sibling class not yet indexed),
        // producing a false `Unresolved class` warning. Sending it would
        // race `republish_open_diagnostics` — the self-healing pass the
        // startup sequence runs once preload completes — to be the last
        // `publishDiagnostics` notification the client sees for this URI,
        // and that race is not guaranteed to resolve in the correct
        // notification's favor. Skip the send entirely: every caller of
        // this method (`did_open`/`did_change`/`did_save`, plus
        // `republish_open_diagnostics` itself) has already recorded this
        // path as open (in `Backend::versions`) before calling here, so
        // `republish_open_diagnostics` is guaranteed to (re)publish it
        // once, correctly, after preload finishes — see
        // `is_preload_in_progress`'s doc for why this check and that
        // recording never race each other.
        //
        // This check lives here, not in the shared `publish_diagnostics_impl`,
        // precisely because that invariant does *not* unconditionally
        // hold for its other two callers, `reload_check_listener` and
        // `seed_reload_diagnostics` — both target URIs that need not be open
        // in the editor (see their own comments), and unconditionally gating
        // their sends here would silently and permanently drop a
        // reload-induced diagnostic for a *closed* file, with nothing left to
        // resend it. Those two callers instead gate themselves conditionally
        // via `should_defer_reload_publish_for_preload`: deferring only when
        // the target URI is *also* open, so the same
        // guarantee this method relies on — `republish_open_diagnostics`
        // resends every open path once preload completes — covers them too.
        {
            let svc = self.service.lock_recover();
            if svc.is_preload_in_progress() {
                return;
            }
        }
        let version = self.file_version_for_uri(uri);
        // BT-5078: analysis runs outside the lock; only the open-check plus
        // the send is serialised with `did_close`'s clearing publish, so a
        // publish that raced a close can't resurrect diagnostics on the
        // closed tab, and a slow analysis never blocks a close.
        let Some(diagnostics) =
            compute_publish_diagnostics(&self.service, &self.reload_diagnostics, uri)
        else {
            return;
        };
        let _publishing = self.publish_lock.lock().await;
        // The document may have closed, or moved to a newer version (whose own
        // publish is now ahead of or behind us), while we analysed. Sending
        // this older result could overwrite newer diagnostics for a client
        // that ignores versions, so drop it; the newer edit publishes its own.
        if !self.is_open_uri(uri) || self.file_version_for_uri(uri) != version {
            return;
        }
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;
    }

    /// Re-publishes diagnostics for every open file.
    ///
    /// `versions` is keyed by the internal path returned by
    /// `resolve_path_for_uri`, so paths may represent `file://`, `untitled:`
    /// (via the `__untitled__/` prefix), or `beamtalk-stdlib://` virtual docs
    /// (real stdlib paths stored in `stdlib_paths`). The URI reconstructed
    /// here must match the original scheme so `publish_diagnostics` routes
    /// correctly — in particular, stdlib docs must not be republished under
    /// `file://`, which would bypass the stdlib early-return and leak
    /// diagnostics for sources the user never opened.
    pub(in crate::server) async fn republish_open(&self) {
        let paths: Vec<Utf8PathBuf> = self.versions.lock_recover().keys().cloned().collect();
        for path in paths {
            let is_stdlib = self.stdlib_paths.lock_recover().contains(&path);
            let uri = if is_stdlib {
                path_to_stdlib_uri(&path)
            } else {
                path_to_uri(&path)
            };
            if let Some(uri) = uri {
                self.publish(&uri).await;
            }
        }
    }
}

/// Aborts a task when dropped, so the worker ends with its `Backend`.
struct AbortOnDrop(tokio::task::AbortHandle);

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        self.0.abort();
    }
}

/// Coalesces cross-file republish requests (see the module doc).
///
/// A pass re-checks every open file, including the ones whose edits asked
/// for it. Those published their own diagnostics already, but a later
/// request in the same burst may have changed what they resolve, so they are
/// not skipped: one extra analysis per triggering file, in exchange for never
/// leaving one stale.
#[derive(Default)]
pub(in crate::server) struct CrossFileRepublish {
    wake: Arc<tokio::sync::Notify>,
    /// Requests so far; the worker waits until this stops moving.
    requests: Arc<std::sync::atomic::AtomicU64>,
    /// Started on the first request (a tokio runtime is running by then).
    worker: OnceLock<AbortOnDrop>,
}

impl CrossFileRepublish {
    /// Asks for every open file to be re-checked soon. The pass runs once
    /// requests have been quiet for [`CROSS_FILE_REPUBLISH_DELAY`] (or after
    /// [`CROSS_FILE_REPUBLISH_MAX_WAIT`] of continuous requests), so a burst
    /// is served by one pass; a request during a pass schedules one more.
    pub(in crate::server) fn request(&self, publisher: &Publisher) {
        self.worker.get_or_init(|| {
            let wake = Arc::clone(&self.wake);
            let requests = Arc::clone(&self.requests);
            let publisher = publisher.clone();
            let task = tokio::spawn(super::listeners::supervise(
                "cross_file_republish",
                move || {
                    let (wake, requests, publisher) =
                        (Arc::clone(&wake), Arc::clone(&requests), publisher.clone());
                    async move {
                        // Requests covered by a finished pass. A fresh run
                        // (first start, or a restart after a panic mid-pass)
                        // starts at 0, so requests the interrupted pass did
                        // not finish are served without waiting for a new
                        // wake-up; a wake-up left over from requests a pass
                        // already covered is spent without another pass.
                        let mut served = 0;
                        loop {
                            if requests.load(std::sync::atomic::Ordering::SeqCst) == served {
                                wake.notified().await;
                                continue;
                            }
                            settle(&requests).await;
                            let covered = requests.load(std::sync::atomic::Ordering::SeqCst);
                            publisher.republish_open().await;
                            served = covered;
                        }
                    }
                },
            ));
            AbortOnDrop(task.abort_handle())
        });
        self.requests
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        self.wake.notify_one();
    }
}

/// Upper bound on how long continuous requests can postpone a pass.
const CROSS_FILE_REPUBLISH_MAX_WAIT: std::time::Duration = std::time::Duration::from_secs(2);

/// Waits until no request has arrived for [`CROSS_FILE_REPUBLISH_DELAY`],
/// or [`CROSS_FILE_REPUBLISH_MAX_WAIT`] has passed.
async fn settle(requests: &std::sync::atomic::AtomicU64) {
    let started = tokio::time::Instant::now();
    loop {
        let seen = requests.load(std::sync::atomic::Ordering::SeqCst);
        tokio::time::sleep(CROSS_FILE_REPUBLISH_DELAY).await;
        if requests.load(std::sync::atomic::Ordering::SeqCst) == seen
            || started.elapsed() >= CROSS_FILE_REPUBLISH_MAX_WAIT
        {
            return;
        }
    }
}
