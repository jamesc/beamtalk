// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The one process-cwd guard for tests (BT-5039).
//!
//! The working directory is process-global, so a test that changes it must be
//! `#[serial(cwd)]`, and must put it back even when an assertion panics: a
//! test that strands the process cwd inside a temp directory another test then
//! deletes breaks every later cwd-relative read. [`with_cwd`] and [`CwdGuard`]
//! are the single implementation of that restore, shared by `beamtalk-cli`,
//! `beamtalk-core` and `beamtalk-package` (see
//! `docs/development/architecture-principles.md` § Duplication & the
//! Shared-Leaf-Module Pattern). Like [`cargo_workspace_root!`], this lives in
//! `beamtalk-workspace` because that crate is the existing shared leaf under
//! all three test suites.

use std::path::{Path, PathBuf};

/// Sets the process cwd on [`CwdGuard::enter`] and restores the previous cwd
/// when dropped, including during an unwinding panic.
///
/// Callers must be `#[serial(cwd)]`.
#[derive(Debug)]
#[must_use = "the cwd is restored when the guard is dropped"]
pub struct CwdGuard {
    previous: PathBuf,
}

impl CwdGuard {
    /// Change the process cwd to `dir`, remembering the current one.
    ///
    /// # Panics
    ///
    /// Panics if the current directory cannot be read or `dir` cannot be
    /// entered (test-only helper).
    pub fn enter(dir: impl AsRef<Path>) -> Self {
        let previous = std::env::current_dir().expect("read the current directory");
        std::env::set_current_dir(dir.as_ref()).expect("enter the test directory");
        Self { previous }
    }
}

impl Drop for CwdGuard {
    fn drop(&mut self) {
        // Best effort: panicking in a drop during unwinding would abort.
        let _ = std::env::set_current_dir(&self.previous);
    }
}

/// Run `f` with the process cwd set to `dir`, restoring the previous cwd
/// afterwards even if `f` panics. Callers must be `#[serial(cwd)]`.
///
/// # Panics
///
/// See [`CwdGuard::enter`].
pub fn with_cwd<T>(dir: impl AsRef<Path>, f: impl FnOnce() -> T) -> T {
    let _guard = CwdGuard::enter(dir);
    f()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    #[test]
    #[serial_test::serial(cwd)]
    fn with_cwd_restores_the_previous_cwd_after_a_panic() {
        let before = std::env::current_dir().unwrap();
        let temp = tempfile::TempDir::new().unwrap();
        let outcome = catch_unwind(AssertUnwindSafe(|| {
            with_cwd(temp.path(), || panic!("boom"));
        }));
        assert!(outcome.is_err(), "the closure's panic must propagate");
        assert_eq!(std::env::current_dir().unwrap(), before);
    }

    #[test]
    #[serial_test::serial(cwd)]
    fn with_cwd_runs_the_closure_inside_the_directory() {
        let temp = tempfile::TempDir::new().unwrap();
        let inside = with_cwd(temp.path(), || std::env::current_dir().unwrap());
        assert_eq!(
            inside.canonicalize().unwrap(),
            temp.path().canonicalize().unwrap()
        );
    }
}
