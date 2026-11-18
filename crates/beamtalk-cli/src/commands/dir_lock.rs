// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exclusive advisory directory locks shared by commands that mutate a
//! directory other beamtalk processes may touch at the same time — the
//! registry index cache, and the native (rebar3) build directory.
//!
//! **DDD Context:** Build System

use camino::Utf8Path;
use fs2::FileExt;
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, warn};

/// Acquire an exclusive advisory lock (`fs2`) on `<dir>/.lock`, creating
/// `dir` if needed. `what` names the directory in messages (e.g.
/// `"registry cache"`).
///
/// Returns the locked file; the lock releases when it drops. Hold it only
/// for the sequence that needs it — it blocks every other beamtalk process
/// trying to take the same lock.
///
/// # Errors
///
/// Returns an error if `dir` cannot be created, or the lock file cannot be
/// opened or locked.
pub(crate) fn lock_dir(dir: &Utf8Path, what: &str) -> Result<std::fs::File> {
    std::fs::create_dir_all(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create {what} directory '{dir}'"))?;

    lock_file(dir.join(".lock").as_std_path(), what)
}

/// Open (creating if needed) `lock_path` and take an exclusive advisory lock
/// (`fs2`) on it, logging a "waiting" warning when another beamtalk process
/// holds it. The parent directory must already exist.
///
/// Returns the locked file; the lock releases when it drops. `what` names the
/// lock in messages (e.g. `"workspace"`).
///
/// # Errors
///
/// Returns an error if the lock file cannot be opened or locked.
pub(crate) fn lock_file(lock_path: &std::path::Path, what: &str) -> Result<std::fs::File> {
    let lockfile = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(false)
        .open(lock_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to open {what} lock file '{}'", lock_path.display()))?;

    // A blocking `lock_exclusive()` gives no indication *why* the process
    // appears to hang if another beamtalk process is stuck holding the lock
    // (e.g. a `git clone` against an unreachable host, or a workspace still
    // starting up, which can take over two minutes). Try non-blocking first
    // so the common uncontended case pays no extra cost; only log when
    // there's actually something to wait for.
    if let Err(e) = lockfile.try_lock_exclusive() {
        let shown = lock_path.display();
        debug!(error = %e, "{what} lock contended, waiting: {shown}");
        warn!("Waiting for {what} lock '{shown}' held by another beamtalk process...");
        lockfile
            .lock_exclusive()
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to acquire {what} lock '{}'", lock_path.display()))?;
    }

    Ok(lockfile)
}
