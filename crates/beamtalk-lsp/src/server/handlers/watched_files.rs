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

use crate::server::config::preload_dirs;
use crate::server::convert::uri_to_path;
use crate::server::lock::LockRecover;
use beamtalk_language_service::LanguageService;
use camino::Utf8PathBuf;
use std::collections::HashSet;
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
        open: &HashSet<Utf8PathBuf>,
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
        let open: HashSet<Utf8PathBuf> = { self.versions.lock_recover().keys().cloned().collect() };

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

        let mut seen: HashSet<Utf8PathBuf> = candidates.iter().map(|(p, _)| p.clone()).collect();
        for f in self.indexed_files_under(&deleted_dirs, &open) {
            if seen.insert(f.clone()) {
                candidates.push((f, true));
            }
        }

        let indexed: HashSet<Utf8PathBuf> = {
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
        #[cfg(test)]
        let inject_failure = self
            .fail_next_watched_batch
            .swap(false, std::sync::atomic::Ordering::SeqCst);
        let actions = match tokio::task::spawn_blocking(move || {
            #[cfg(test)]
            assert!(!inject_failure, "injected watched-files batch failure");
            resolve_actions(candidates, &indexed, &created_dirs, &open, &roots)
        })
        .await
        {
            Ok(actions) => actions,
            Err(e) => {
                // BT-5131: never drop a batch silently. The index may now
                // lag the disk until the next event for these files.
                // BT-5188: the manifest reload and republish below still
                // run — a `beamtalk.toml` change in this batch is not part
                // of the failed task.
                tracing::error!(%e, "watched-files batch task failed; its file changes were dropped");
                Vec::new()
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
/// Cap on directory entries visited, of any kind, while walking directories
/// reported as created in one batch (BT-5188).
const MAX_CREATED_DIR_ENTRIES: usize = 50_000;

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

/// The `.bt` files found under directories reported as created.
#[derive(Debug, Default)]
struct CreatedDirWalk {
    files: Vec<Utf8PathBuf>,
    /// Directory entries visited, of any kind.
    visited: usize,
    /// A cap was hit, so `files` is incomplete.
    truncated: bool,
}

/// The parts of a created directory `dir` the preload would index
/// (BT-5188): all of it when it lies inside a preloaded directory, else only
/// the preloaded directories inside it (a whole package renamed into place),
/// else nothing — so an unrelated tree is never walked.
fn walk_targets(dir: &std::path::Path, preload: &[std::path::PathBuf]) -> Vec<std::path::PathBuf> {
    if let Some(covering) = preload.iter().find(|p| dir.starts_with(p)) {
        // Inside a skipped directory (`src/node_modules/x`) the preload
        // would not look, so neither does this walk.
        let skipped = dir.strip_prefix(covering).is_ok_and(|rel| {
            rel.components()
                .any(|c| c.as_os_str().to_str().is_some_and(is_skipped_dir))
        });
        return if skipped {
            Vec::new()
        } else {
            vec![dir.to_path_buf()]
        };
    }
    preload
        .iter()
        .filter(|p| p.starts_with(dir))
        .cloned()
        .collect()
}

/// Whether a walk skips the directory named `name` (the preload's own list).
fn is_skipped_dir(name: &str) -> bool {
    beamtalk_core::file_walker::SKIPPED_SOURCE_DIRS.contains(&name)
}

/// `.bt` files under the created directories, restricted to what the preload
/// covers and skipping the directories it skips. Bounded both by `.bt` files
/// found and by entries visited; hitting either cap is logged.
fn walk_created_dirs(
    created_dirs: &[Utf8PathBuf],
    preload: &[std::path::PathBuf],
    max_files: usize,
    max_entries: usize,
) -> CreatedDirWalk {
    let mut walk = CreatedDirWalk::default();
    let mut stack: Vec<std::path::PathBuf> = created_dirs
        .iter()
        .flat_map(|d| walk_targets(d.as_std_path(), preload))
        .filter(|d| d.is_dir())
        .collect();
    'walk: while let Some(d) = stack.pop() {
        let Ok(entries) = fs::read_dir(&d) else {
            continue;
        };
        for entry in entries.flatten() {
            if walk.visited >= max_entries || walk.files.len() >= max_files {
                walk.truncated = true;
                break 'walk;
            }
            walk.visited += 1;
            let Ok(path) = Utf8PathBuf::from_path_buf(entry.path()) else {
                continue;
            };
            match entry.file_type() {
                Ok(t) if t.is_dir() && !path.file_name().is_some_and(is_skipped_dir) => {
                    stack.push(path.into_std_path_buf());
                }
                Ok(t) if t.is_file() && path.extension() == Some("bt") => walk.files.push(path),
                // Like the preload's walker: a link to a file is collected
                // under the link's own path; a linked directory is not entered.
                Ok(t)
                    if t.is_symlink()
                        && path.extension() == Some("bt")
                        && fs::metadata(&path).is_ok_and(|m| m.is_file()) =>
                {
                    walk.files.push(path);
                }
                _ => {}
            }
        }
    }
    if walk.truncated {
        tracing::warn!(
            files = walk.files.len(),
            visited = walk.visited,
            "created-directory walk hit its cap; some new files were not indexed and \
             will be picked up when opened or changed"
        );
    }
    walk
}

/// Filters the candidates to files the index cares about and reads them.
fn resolve_actions(
    mut candidates: Vec<(Utf8PathBuf, bool)>,
    indexed: &HashSet<Utf8PathBuf>,
    created_dirs: &[Utf8PathBuf],
    open: &HashSet<Utf8PathBuf>,
    roots: &[std::path::PathBuf],
) -> Vec<Action> {
    let preload = preload_dirs(roots);
    let mut seen: HashSet<Utf8PathBuf> = candidates.iter().map(|(p, _)| p.clone()).collect();
    let walk = walk_created_dirs(
        created_dirs,
        &preload,
        MAX_CREATED_DIR_FILES,
        MAX_CREATED_DIR_ENTRIES,
    );
    for f in walk.files {
        if !open.contains(&f) && seen.insert(f.clone()) {
            candidates.push((f, false));
        }
    }
    candidates
        .into_iter()
        .filter(|(p, _)| {
            indexed.contains(p) || preload.iter().any(|dir| p.as_std_path().starts_with(dir))
        })
        .filter_map(|(path, deleted)| read_action(path, deleted))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::test_helpers::unique_temp_dir;

    fn utf8(p: std::path::PathBuf) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(p).expect("utf8")
    }

    /// BT-5188: a created directory outside every preloaded directory is not
    /// walked at all.
    #[test]
    fn uncovered_created_dir_is_not_walked() {
        let root = unique_temp_dir("bt-5188-uncovered");
        let scratch = root.join("scratch");
        fs::create_dir_all(&scratch).expect("dir");
        fs::write(scratch.join("x.bt"), "Object subclass: X\n").expect("x");
        let walk = walk_created_dirs(&[utf8(scratch)], &[root.join("src")], 10, 10);
        assert_eq!(walk.visited, 0, "nothing outside the preload is visited");
        assert!(walk.files.is_empty());
    }

    /// BT-5188: a package renamed into place walks only its preloaded
    /// directories.
    #[test]
    fn created_package_walks_only_its_preloaded_dirs() {
        let root = unique_temp_dir("bt-5188-pkg");
        fs::create_dir_all(root.join("src")).expect("src");
        fs::create_dir_all(root.join("scratch")).expect("scratch");
        fs::write(root.join("src").join("a.bt"), "").expect("a");
        fs::write(root.join("scratch").join("s.bt"), "").expect("s");
        let walk = walk_created_dirs(&[utf8(root.clone())], &[root.join("src")], 10, 10);
        assert_eq!(walk.files, vec![utf8(root.join("src").join("a.bt"))]);
        assert_eq!(walk.visited, 1);
    }

    /// BT-5188: the entry cap counts every entry, not only `.bt` hits, and
    /// reports truncation.
    #[test]
    fn entry_cap_counts_every_entry_and_reports_truncation() {
        let root = unique_temp_dir("bt-5188-cap");
        let src = root.join("src");
        fs::create_dir_all(&src).expect("src");
        for i in 0..20 {
            fs::write(src.join(format!("n{i}.txt")), "").expect("txt");
        }
        fs::write(src.join("a.bt"), "").expect("a");
        let walk = walk_created_dirs(&[utf8(src.clone())], &[src], 10, 5);
        assert_eq!(walk.visited, 5, "stops after the entry cap");
        assert!(walk.truncated, "truncation is reported");
    }

    /// BT-5188: like the preload, a symlinked `.bt` file is collected and a
    /// symlinked directory is not entered.
    #[cfg(unix)]
    #[test]
    fn symlinked_files_are_collected_and_linked_dirs_are_not_entered() {
        let root = unique_temp_dir("bt-5188-symlink");
        let src = root.join("src");
        let elsewhere = root.join("elsewhere");
        fs::create_dir_all(&src).expect("src");
        fs::create_dir_all(&elsewhere).expect("elsewhere");
        fs::write(elsewhere.join("shared.bt"), "").expect("shared");
        std::os::unix::fs::symlink(elsewhere.join("shared.bt"), src.join("link.bt"))
            .expect("file link");
        std::os::unix::fs::symlink(&elsewhere, src.join("dirlink")).expect("dir link");
        let walk = walk_created_dirs(&[utf8(src.clone())], std::slice::from_ref(&src), 10, 10);
        assert_eq!(walk.files, vec![utf8(src.join("link.bt"))]);
    }

    /// BT-5188: a created directory inside a skipped one is not walked.
    #[test]
    fn created_dir_inside_a_skipped_dir_is_not_walked() {
        let root = unique_temp_dir("bt-5188-inside-skipped");
        let deep = root.join("src").join("node_modules").join("pkg");
        fs::create_dir_all(&deep).expect("dir");
        fs::write(deep.join("x.bt"), "").expect("x");
        let walk = walk_created_dirs(&[utf8(deep)], &[root.join("src")], 10, 10);
        assert!(walk.files.is_empty());
        assert_eq!(walk.visited, 0);
    }
}
