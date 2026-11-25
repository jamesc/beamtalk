// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exclusive advisory directory locks shared by commands that mutate a
//! directory other beamtalk processes may touch at the same time — the
//! registry index cache, and the native (rebar3) build directory.
//!
//! **DDD Context:** Build System

use std::cell::RefCell;
use std::collections::HashSet;
use std::path::PathBuf;

use beamtalk_core::build_layout::BuildLayout;
use camino::Utf8Path;
use fs2::FileExt;
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, warn};

/// A held directory lock (registry cache, native build, workspace). Dropping
/// it releases the lock; the kernel also releases it if the process dies.
/// Holds nothing when locking is turned off with [`NO_BUILD_LOCK_ENV`].
#[derive(Debug)]
#[must_use = "the lock is released as soon as the guard is dropped"]
pub(crate) struct DirLock {
    _file: Option<std::fs::File>,
}

/// Acquire an exclusive advisory lock (`fs2`) on `<dir>/.lock`, creating
/// `dir` if needed. `what` names the directory in messages (e.g.
/// `"registry cache"`).
///
/// Returns the lock guard; the lock releases when it drops. Hold it only
/// for the sequence that needs it — it blocks every other beamtalk process
/// trying to take the same lock.
///
/// # Errors
///
/// Returns an error if `dir` cannot be created, or the lock file cannot be
/// opened or locked.
pub(crate) fn lock_dir(dir: &Utf8Path, what: &str) -> Result<DirLock> {
    std::fs::create_dir_all(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create {what} directory '{dir}'"))?;

    lock_file(dir.join(".lock").as_std_path(), what)
}

/// Open (creating if needed) `lock_path` and take an exclusive advisory lock
/// (`fs2`) on it, logging a "waiting" warning when another beamtalk process
/// holds it.
///
/// Shares the project build lock's acquisition path (BT-5186): honours
/// [`NO_BUILD_LOCK_ENV`], reports a filesystem without advisory locks as such
/// instead of waiting on it, and retries if `clean --all` unlinked the file.
///
/// Returns the lock guard; the lock releases when it drops. `what` names the
/// lock in messages (e.g. `"workspace"`).
///
/// # Errors
///
/// Returns an error if the lock file cannot be opened or locked.
pub(crate) fn lock_file(lock_path: &std::path::Path, what: &str) -> Result<DirLock> {
    if locking_disabled() {
        debug!(
            "not taking the {what} lock ({NO_BUILD_LOCK_ENV} is on): {}",
            lock_path.display()
        );
        return Ok(DirLock { _file: None });
    }
    // A blocking `lock_exclusive()` gives no indication *why* the process
    // appears to hang if another beamtalk process is stuck holding the lock
    // (e.g. a `git clone` against an unreachable host, or a workspace still
    // starting up, which can take over two minutes), so say so once.
    let file = open_and_lock(lock_path, what, || {
        warn!(
            "Waiting for {what} lock '{}' held by another beamtalk process...",
            lock_path.display()
        );
    })?;
    Ok(DirLock { _file: Some(file) })
}

// ---------------------------------------------------------------------------
// Project build lock (BT-5075)
// ---------------------------------------------------------------------------

/// File name of the project build lock, directly under `_build/`.
pub(crate) const PROJECT_BUILD_LOCK_NAME: &str = ".build.lock";

thread_local! {
    /// Project roots whose build lock the current thread already holds.
    /// `build` calls `ensure_deps_resolved`, which also locks; the nested
    /// acquire must not block on the outer one (two `flock`s on separate file
    /// descriptions of one file exclude each other even inside one process).
    static HELD_BUILD_LOCKS: RefCell<HashSet<PathBuf>> = RefCell::new(HashSet::new());
}

/// Guard for the project build lock. Dropping it releases the lock; the kernel
/// also releases it if the process dies, so no stale lock can outlive a crash.
#[derive(Debug)]
pub(crate) struct ProjectBuildLock {
    /// The held lock file; `None` for a nested (re-entrant) acquire.
    file: Option<std::fs::File>,
    /// The key registered in [`HELD_BUILD_LOCKS`] by the outermost acquire.
    key: Option<PathBuf>,
}

impl ProjectBuildLock {
    /// Release the OS lock now and forget this thread's hold, instead of at
    /// drop. Used by `clean --all` once it has removed `_build/` (and, on
    /// Unix, the lock file itself) while holding the lock.
    pub(crate) fn release(mut self) {
        self.release_in_place();
    }

    fn release_in_place(&mut self) {
        if let Some(key) = self.key.take() {
            HELD_BUILD_LOCKS.with(|held| {
                held.borrow_mut().remove(&key);
            });
        }
        self.file = None; // closing the descriptor drops the OS lock
    }
}

impl Drop for ProjectBuildLock {
    fn drop(&mut self) {
        self.release_in_place();
    }
}

/// Environment variable that turns every beamtalk file lock off (BT-5133,
/// BT-5186) — the project build lock and each [`lock_file`] / [`lock_dir`]
/// lock (registry cache, native build, workspace) — for filesystems without
/// advisory-lock support (some NFS/FUSE mounts). With it set, concurrent
/// beamtalk processes are not serialised.
pub(crate) const NO_BUILD_LOCK_ENV: &str = "BEAMTALK_NO_BUILD_LOCK";

fn locking_disabled() -> bool {
    std::env::var_os(NO_BUILD_LOCK_ENV).is_some_and(|v| !v.is_empty() && v != "0")
}

/// Whether a `try_lock_exclusive` failure means another holder has the lock
/// (as opposed to the filesystem not supporting locks, or an I/O error).
fn is_contention(e: &std::io::Error) -> bool {
    let contended = fs2::lock_contended_error();
    e.kind() == std::io::ErrorKind::WouldBlock
        || (e.raw_os_error().is_some() && e.raw_os_error() == contended.raw_os_error())
}

/// Path of the project build lock file for `project_root`.
pub(crate) fn project_build_lock_path(project_root: &Utf8Path) -> camino::Utf8PathBuf {
    BuildLayout::new(project_root)
        .build_root()
        .join(PROJECT_BUILD_LOCK_NAME)
}

/// Take the exclusive project build lock (`_build/.build.lock`), the one lock
/// that serialises every beamtalk operation that writes `_build/` or
/// `beamtalk.lock` for this project (build, test, lint's dependency
/// resolution, release, clean, `deps add`/`deps update`).
///
/// Blocks while another beamtalk process holds it, printing
/// `waiting for another beamtalk process in <dir>…` to stderr once. There is
/// no timeout: the holder is a build, and the kernel frees the lock if it dies.
/// Re-entrant on one thread: a nested call (e.g. `build` → `ensure_deps_resolved`)
/// returns immediately and releases nothing.
///
/// Lock order (outermost first): project build lock → registry cache lock →
/// native build lock. Workspace startup locks are never taken while this is
/// held (a workspace is started only after `build` returns).
///
/// # Errors
///
/// Returns an error if `_build/` cannot be created or the lock cannot be taken.
pub(crate) fn lock_project_build(project_root: &Utf8Path) -> Result<ProjectBuildLock> {
    if locking_disabled() {
        debug!("not taking the project build lock ({NO_BUILD_LOCK_ENV} is on)");
        return Ok(ProjectBuildLock {
            file: None,
            key: None,
        });
    }
    let lock_path = project_build_lock_path(project_root);
    let dir = lock_path.parent().unwrap_or(project_root);
    std::fs::create_dir_all(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create build directory '{dir}'"))?;

    // Canonical so `.`, `./proj` and `/abs/proj` name the same hold.
    let key = std::fs::canonicalize(lock_path.as_std_path())
        .or_else(|_| {
            std::fs::canonicalize(dir.as_std_path()).map(|d| d.join(PROJECT_BUILD_LOCK_NAME))
        })
        .unwrap_or_else(|_| lock_path.as_std_path().to_path_buf());
    if HELD_BUILD_LOCKS.with(|held| held.borrow().contains(&key)) {
        return Ok(ProjectBuildLock {
            file: None,
            key: None,
        });
    }

    let shown = std::path::absolute(project_root.as_std_path())
        .unwrap_or_else(|_| project_root.as_std_path().to_path_buf());
    let file = open_and_lock(lock_path.as_std_path(), "build", || {
        eprintln!(
            "waiting for another beamtalk process in {}…",
            shown.display()
        );
    })?;
    HELD_BUILD_LOCKS.with(|held| held.borrow_mut().insert(key.clone()));
    Ok(ProjectBuildLock {
        file: Some(file),
        key: Some(key),
    })
}

/// Open `lock_path` (creating it) and take an exclusive lock: try without
/// blocking first so the uncontended case costs one syscall, then announce and
/// block. `announce` runs once, only when the lock is contended.
fn open_and_lock(
    lock_path: &std::path::Path,
    what: &str,
    announce: impl FnOnce(),
) -> Result<std::fs::File> {
    const MAX_REPLACED: u32 = 16;
    let mut announce = Some(announce);
    let mut directory_vanished = 0;
    let mut replaced = 0;
    loop {
        // `clean --all` may have removed `_build/` itself since the last pass.
        if let Some(parent) = lock_path.parent() {
            std::fs::create_dir_all(parent)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to create {what} lock directory"))?;
        }
        let file = match std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(false)
            .open(lock_path)
        {
            Ok(file) => file,
            // `clean --all` removed the (empty) directory between the
            // `create_dir_all` above and this open: recreate it and retry.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound && directory_vanished < 8 => {
                directory_vanished += 1;
                continue;
            }
            Err(e) => {
                return Err(e).into_diagnostic().wrap_err_with(|| {
                    format!("Failed to open {what} lock file '{}'", lock_path.display())
                });
            }
        };
        if let Err(e) = file.try_lock_exclusive() {
            // Only `WouldBlock` is contention. Anything else (e.g. an NFS mount
            // without lock support) would never clear by waiting, so report it
            // as what it is rather than printing "waiting…" and then failing.
            if !is_contention(&e) {
                return Err(miette::miette!(
                    "Failed to take the {what} lock '{}': {e}\n\
                     The filesystem may not support advisory file locks.\n\
                     Set {NO_BUILD_LOCK_ENV}=1 to run without beamtalk's file locks \
                     (concurrent beamtalk processes are then not serialised).",
                    lock_path.display()
                ));
            }
            debug!(error = %e, "{what} lock contended, waiting: {}", lock_path.display());
            if let Some(announce) = announce.take() {
                announce();
            }
            file.lock_exclusive().into_diagnostic().wrap_err_with(|| {
                format!("Failed to acquire {what} lock '{}'", lock_path.display())
            })?;
        }
        // `clean --all` unlinks the lock file while it holds the lock (Unix
        // only). A waiter that was blocked on the old inode now holds a lock
        // nobody else will ever contend, so it must retry on the file the path
        // names now. (On Windows `clean --all` keeps the lock file, so the path
        // cannot change under a holder there.)
        // Bounded: on a filesystem whose path and descriptor stats never
        // agree (some FUSE/overlay mounts), the check cannot succeed, and the
        // lock we hold is then the best there is.
        replaced += 1;
        if still_names_locked_file(lock_path, &file) {
            return Ok(file);
        }
        if replaced > MAX_REPLACED {
            warn!(
                "{what} lock '{}' kept changing under us; proceeding with a lock that \
                 may not exclude other beamtalk processes",
                lock_path.display()
            );
            return Ok(file);
        }
    }
}

/// Whether `path` still names the file `locked` was opened from.
#[cfg(unix)]
fn still_names_locked_file(path: &std::path::Path, locked: &std::fs::File) -> bool {
    use std::os::unix::fs::MetadataExt as _;
    match (std::fs::metadata(path), locked.metadata()) {
        (Ok(on_disk), Ok(held)) => on_disk.dev() == held.dev() && on_disk.ino() == held.ino(),
        _ => false,
    }
}

#[cfg(not(unix))]
fn still_names_locked_file(_path: &std::path::Path, _locked: &std::fs::File) -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::time::Duration;

    fn root(dir: &tempfile::TempDir) -> camino::Utf8PathBuf {
        camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    #[test]
    fn nested_acquire_on_one_thread_does_not_block() {
        let dir = tempfile::tempdir().unwrap();
        let root = root(&dir);
        let outer = lock_project_build(&root).unwrap();
        // `.` / non-canonical spellings of the same project are the same hold.
        let inner = lock_project_build(&root.join(".")).unwrap();
        drop(inner);
        // Dropping the nested guard must not release the outer hold.
        let other = std::thread::spawn({
            let root = root.clone();
            move || {
                let file = std::fs::OpenOptions::new()
                    .write(true)
                    .open(project_build_lock_path(&root))
                    .unwrap();
                file.try_lock_exclusive().is_err()
            }
        });
        assert!(other.join().unwrap(), "outer hold released by nested drop");
        drop(outer);
    }

    #[test]
    fn contention_is_would_block_only() {
        assert!(is_contention(&fs2::lock_contended_error()));
        assert!(!is_contention(&std::io::Error::from(
            std::io::ErrorKind::Unsupported
        )));
        assert!(!is_contention(&std::io::Error::from(
            std::io::ErrorKind::PermissionDenied
        )));
    }

    #[test]
    fn second_holder_waits_until_first_releases() {
        let dir = tempfile::tempdir().unwrap();
        let root = root(&dir);
        let first = lock_project_build(&root).unwrap();
        let (tx, rx) = mpsc::channel();
        let waiter = std::thread::spawn({
            let root = root.clone();
            move || {
                let _g = lock_project_build(&root).unwrap();
                tx.send(()).unwrap();
            }
        });
        // sleep: prove the waiter is blocked, not merely slow to start
        assert!(rx.recv_timeout(Duration::from_millis(300)).is_err());
        drop(first);
        rx.recv_timeout(Duration::from_secs(30))
            .expect("waiter proceeds once freed");
        waiter.join().unwrap();
    }

    /// A waiter blocked on a lock file that `clean --all` unlinks must retry on
    /// the file the path names now, not proceed on the dead inode.
    #[cfg(unix)]
    #[test]
    fn waiter_blocked_on_an_unlinked_lock_file_retries_on_the_new_one() {
        let dir = tempfile::tempdir().unwrap();
        let root = root(&dir);
        let first = lock_project_build(&root).unwrap();
        let (tx, rx) = mpsc::channel();
        let waiter = std::thread::spawn({
            let root = root.clone();
            move || {
                let g = lock_project_build(&root).unwrap();
                tx.send(()).unwrap();
                g
            }
        });
        // sleep: let the waiter block on the old inode before it is unlinked
        std::thread::sleep(Duration::from_millis(300));
        // What `clean --all` does: unlink the lock file, then release.
        std::fs::remove_file(project_build_lock_path(&root)).unwrap();
        // A third process takes a fresh file at the same path.
        let third = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(false)
            .open(project_build_lock_path(&root))
            .unwrap();
        third.lock_exclusive().unwrap();
        drop(first);
        // The waiter must NOT proceed on the dead inode while `third` holds the new file.
        assert!(rx.recv_timeout(Duration::from_millis(500)).is_err());
        drop(third);
        rx.recv_timeout(Duration::from_secs(30))
            .expect("waiter proceeds on the new file");
        let _g = waiter.join().unwrap();
    }

    /// `clean --all` also removes `_build/`: the woken waiter recreates it and
    /// takes the lock rather than failing to reopen the file.
    #[cfg(unix)]
    #[test]
    fn waiter_survives_clean_all_removing_the_build_directory() {
        let dir = tempfile::tempdir().unwrap();
        let root = root(&dir);
        let first = lock_project_build(&root).unwrap();
        let waiter = std::thread::spawn({
            let root = root.clone();
            move || lock_project_build(&root).map(drop)
        });
        // sleep: let the waiter block on the old inode before `_build/` goes
        std::thread::sleep(Duration::from_millis(300));
        let path = project_build_lock_path(&root);
        std::fs::remove_file(&path).unwrap();
        // Remove the directory *before* releasing: the waiter is still blocked on
        // the old inode, so this cannot race its recreate of the lock file.
        std::fs::remove_dir(path.parent().unwrap()).unwrap();
        drop(first);
        waiter
            .join()
            .unwrap()
            .expect("waiter retries after _build/ is removed");
    }

    /// BT-5186: `BEAMTALK_NO_BUILD_LOCK` turns off every `dir_lock` lock, not
    /// only the project build lock. Runs in a child copy of this test binary so
    /// setting the variable cannot leak into tests running in parallel.
    #[test]
    fn opt_out_env_turns_off_directory_and_file_locks() {
        const CHILD: &str = "BEAMTALK_TEST_DIR_LOCK_CHILD";
        if let Some(dir) = std::env::var_os(CHILD) {
            let dir = camino::Utf8PathBuf::from_path_buf(PathBuf::from(dir)).unwrap();
            // Another holder has the lock, as a concurrent native build would.
            std::fs::create_dir_all(&dir).unwrap();
            let held = std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(false)
                .open(dir.join(".lock"))
                .unwrap();
            held.lock_exclusive().unwrap();
            let (tx, rx) = mpsc::channel();
            std::thread::spawn(move || {
                let dir_lock = lock_dir(&dir, "native build").map(drop);
                let file_lock = lock_file(dir.join("ws.lock").as_std_path(), "workspace").map(drop);
                tx.send((dir_lock, file_lock)).unwrap();
            });
            let (dir_lock, file_lock) = rx
                .recv_timeout(Duration::from_secs(10))
                .expect("lock_dir must not wait with the opt-out set");
            dir_lock.unwrap();
            file_lock.unwrap();
            return;
        }
        let dir = tempfile::tempdir().unwrap();
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "commands::dir_lock::tests::opt_out_env_turns_off_directory_and_file_locks",
                "--exact",
                "--nocapture",
            ])
            .env(CHILD, dir.path().join("native"))
            .env(NO_BUILD_LOCK_ENV, "1")
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success() && stdout.contains("1 passed"),
            "child test failed:\n{stdout}\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    /// A lock file left behind by a crashed process holds nothing: the OS
    /// lock, not the file's existence, is the lock.
    #[test]
    fn leftover_lock_file_does_not_block() {
        let dir = tempfile::tempdir().unwrap();
        let root = root(&dir);
        let path = project_build_lock_path(&root);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, b"stale").unwrap();
        let _g = lock_project_build(&root).unwrap();
    }
}
