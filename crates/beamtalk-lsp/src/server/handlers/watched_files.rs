// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `workspace/didChangeWatchedFiles` (BT-5078): keeps the index in step with
//! files that change on disk behind the editor's back (`git pull`, an
//! external edit, a delete or rename).
//!
//! The client sends one event per created/changed/deleted `.bt` file (a
//! rename arrives as a delete plus a create). Open documents are skipped: the
//! editor's buffer is the truth for those, and `did_close` reverts to disk.
//! Only files the preload would index (or that are already indexed) are
//! touched, so unrelated `.bt` files elsewhere in the tree never enter the
//! index. A deleted or renamed *directory* is not reported per file (the
//! `**/*.bt` glob does not match directories), so a second, delete-only
//! watcher reports it and every indexed, closed file under it is dropped
//! (BT-5109).
//! No file watcher is added to the server itself: the client does the
//! watching and this only reacts to what it reports.

use crate::server::config::preload_covers;
use crate::server::convert::uri_to_path;
use crate::server::lock::LockRecover;
use beamtalk_language_service::LanguageService;
use camino::Utf8PathBuf;
use std::fs;
use tower_lsp::lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions, FileChangeType,
    FileSystemWatcher, GlobPattern, Registration, WatchKind,
};

use super::super::Backend;

/// Registration id for the dynamic watcher.
const WATCHER_ID: &str = "beamtalk-watched-files";

/// What to do with one watched path.
enum Action {
    /// (Re)index the file from this on-disk text.
    Update(Utf8PathBuf, String),
    /// The file is gone (or unreadable): drop it from the index.
    Remove(Utf8PathBuf),
}

impl Backend {
    /// Asks the client to watch `**/*.bt` and `**/beamtalk.toml`.
    ///
    /// Failures are non-fatal: a client without dynamic registration simply
    /// never reports disk changes, and rename's on-disk check still protects
    /// closed files.
    pub(in crate::server) async fn register_file_watchers(&self) {
        let watcher = |glob: &str| FileSystemWatcher {
            glob_pattern: GlobPattern::String(glob.to_string()),
            kind: Some(WatchKind::Create | WatchKind::Change | WatchKind::Delete),
        };
        let opts = DidChangeWatchedFilesRegistrationOptions {
            watchers: vec![
                watcher("**/*.bt"),
                watcher("**/beamtalk.toml"),
                // BT-5109: matches directories (and every other path), so a
                // deleted directory is reported; the handler ignores deletes
                // that are not a prefix of an indexed file. Creates report a
                // directory renamed into place (BT-5131), whose `.bt` files
                // get no create events of their own.
                FileSystemWatcher {
                    glob_pattern: GlobPattern::String("**/*".to_string()),
                    kind: Some(WatchKind::Create | WatchKind::Delete),
                },
            ],
        };
        let registrations = vec![Registration {
            id: WATCHER_ID.to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: serde_json::to_value(&opts).ok(),
        }];
        if let Err(e) = self.client.register_capability(registrations).await {
            tracing::debug!("watched-files registration rejected: {e}");
        }
    }

    /// Indexed, closed, non-stdlib files at or under any of `dirs` (BT-5109).
    fn indexed_files_under(
        &self,
        dirs: &[Utf8PathBuf],
        open: &std::collections::HashSet<Utf8PathBuf>,
    ) -> Vec<Utf8PathBuf> {
        if dirs.is_empty() {
            return Vec::new();
        }
        let svc = self.service.lock_recover();
        let index = svc.project_index();
        index
            .indexed_files()
            .into_iter()
            .filter(|f| {
                !open.contains(*f)
                    && !index.is_stdlib_file(f)
                    && dirs.iter().any(|d| f.starts_with(d))
            })
            .cloned()
            .collect()
    }

    /// Applies a batch of on-disk changes to the index, then re-checks the
    /// open files once if anything changed.
    pub(in crate::server) async fn handle_did_change_watched_files(
        &self,
        params: DidChangeWatchedFilesParams,
    ) {
        let roots = self.workspace_roots.lock_recover().clone();
        let open: std::collections::HashSet<Utf8PathBuf> =
            { self.versions.lock_recover().keys().cloned().collect() };

        let mut manifest_changed = false;
        let mut candidates: Vec<(Utf8PathBuf, bool)> = Vec::new(); // (path, deleted)
        let mut deleted_dirs: Vec<Utf8PathBuf> = Vec::new();
        let mut created_dirs: Vec<Utf8PathBuf> = Vec::new();
        for event in params.changes {
            let Some(path) = uri_to_path(&event.uri) else {
                continue;
            };
            match path.file_name() {
                Some("beamtalk.toml") => manifest_changed = true,
                _ if path.extension() == Some("bt") && !open.contains(&path) => {
                    candidates.push((path, event.typ == FileChangeType::DELETED));
                }
                // BT-5109: a delete of anything else may be a directory.
                _ if path.extension() != Some("bt") && event.typ == FileChangeType::DELETED => {
                    deleted_dirs.push(path);
                }
                // BT-5131: a created non-`.bt` path may be a directory renamed
                // into place; it is walked off-executor below (a plain file
                // is ignored there).
                _ if path.extension() != Some("bt") && event.typ == FileChangeType::CREATED => {
                    created_dirs.push(path);
                }
                _ => {}
            }
        }

        for f in self.indexed_files_under(&deleted_dirs, &open) {
            if !candidates.iter().any(|(p, _)| *p == f) {
                candidates.push((f, true));
            }
        }

        let indexed: std::collections::HashSet<Utf8PathBuf> = {
            let svc = self.service.lock_recover();
            candidates
                .iter()
                .filter(|(p, _)| svc.file_source(p).is_some())
                .map(|(p, _)| p.clone())
                .collect()
        };

        // Disk reads — and `preload_covers`, which reads `beamtalk.toml` —
        // stay off the async executor; one batch after a `git pull` can be
        // many files.
        let actions = match tokio::task::spawn_blocking(move || {
            resolve_actions(candidates, &indexed, &created_dirs, &open, &roots)
        })
        .await
        {
            Ok(actions) => actions,
            Err(e) => {
                // BT-5131: never drop a batch silently. The index may now
                // lag the disk until the next event for these files.
                tracing::error!(%e, "watched-files batch task failed; batch dropped");
                return;
            }
        };

        let mut index_changed = false;
        {
            let _sync = self.text_sync.lock_recover();
            let mut svc = self.service.lock_recover();
            for action in actions {
                match action {
                    Action::Update(path, text) => {
                        // Re-check under the lock: the file may have been
                        // opened while we were reading it.
                        if self.versions.lock_recover().contains_key(&path)
                            || svc.project_index().is_stdlib_file(&path)
                        {
                            continue;
                        }
                        if svc.file_source(&path).as_deref() != Some(text.as_str()) {
                            svc.update_file(path, text);
                            index_changed = true;
                        }
                    }
                    Action::Remove(path) => {
                        if self.versions.lock_recover().contains_key(&path)
                            || svc.file_source(&path).is_none()
                        {
                            continue;
                        }
                        svc.remove_file(&path);
                        index_changed = true;
                    }
                }
            }
        }

        if manifest_changed {
            let roots = self.workspace_roots.lock_recover().clone();
            // Cheap, per-manifest-edit (not per keystroke): re-read the
            // `[package]` name and `[diagnostics]` table.
            self.load_root_packages(&roots).await;
            self.load_diagnostics_table(&roots).await;
        }
        if index_changed || manifest_changed {
            self.republish_open_diagnostics().await;
        }
    }
}

/// Cap on `.bt` files collected from directories reported as created.
const MAX_CREATED_DIR_FILES: usize = 5_000;

/// Reads `path`; `None` means "leave the index alone". Only a file that is
/// really gone is removed — a read error (permissions, non-UTF-8 bytes, a
/// transient lock) must not evict an indexed file (BT-5131).
fn read_action(path: Utf8PathBuf, deleted: bool) -> Option<Action> {
    if deleted {
        return Some(Action::Remove(path));
    }
    match fs::read_to_string(&path) {
        Ok(text) => Some(Action::Update(path, text)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Some(Action::Remove(path)),
        Err(e) => {
            tracing::warn!(%e, path = %path, "watched file unreadable; keeping indexed copy");
            None
        }
    }
}

/// `.bt` files under `dir` (a directory renamed into place), bounded.
fn bt_files_under(dir: &Utf8PathBuf, out: &mut Vec<Utf8PathBuf>) {
    let mut stack = vec![dir.clone()];
    while let Some(d) = stack.pop() {
        let Ok(entries) = fs::read_dir(&d) else {
            continue;
        };
        for entry in entries.flatten() {
            if out.len() >= MAX_CREATED_DIR_FILES {
                return;
            }
            let Ok(path) = Utf8PathBuf::from_path_buf(entry.path()) else {
                continue;
            };
            match entry.file_type() {
                Ok(t) if t.is_dir() => stack.push(path),
                Ok(t) if t.is_file() && path.extension() == Some("bt") => out.push(path),
                _ => {}
            }
        }
    }
}

/// Filters the candidates to files the index cares about and reads them.
fn resolve_actions(
    mut candidates: Vec<(Utf8PathBuf, bool)>,
    indexed: &std::collections::HashSet<Utf8PathBuf>,
    created_dirs: &[Utf8PathBuf],
    open: &std::collections::HashSet<Utf8PathBuf>,
    roots: &[std::path::PathBuf],
) -> Vec<Action> {
    for dir in created_dirs {
        let mut found = Vec::new();
        if dir.is_dir() {
            bt_files_under(dir, &mut found);
        }
        for f in found {
            if !open.contains(&f) && !candidates.iter().any(|(p, _)| *p == f) {
                candidates.push((f, false));
            }
        }
    }
    candidates
        .into_iter()
        .filter(|(p, _)| indexed.contains(p) || preload_covers(p.as_std_path(), roots))
        .filter_map(|(path, deleted)| read_action(path, deleted))
        .collect()
}
