// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: Beamtalk installation sysroot resolution from the running
//! executable path.
//!
//! **DDD Context:** Infrastructure (shared leaf, no domain logic)
//!
//! This crate exists to give `beamtalk-cli` (`--print-sysroot`, distribution
//! FFI stub discovery) and `beamtalk-lsp` (installed stdlib source
//! discovery) a single authoritative sysroot derivation without either
//! depending on the other — the "shared-leaf-module pattern" from
//! `docs/development/architecture-principles.md` §6.

use std::path::{Path, PathBuf};

/// Derives the installation sysroot from a (real or hypothetical) binary
/// path: `{sysroot}/bin/beamtalk` → `{sysroot}`.
///
/// Returns `None` when `exe` has no grandparent directory (e.g. a bare
/// filename, or a path with only one or two components) — callers decide
/// their own fallback behavior (a hard error, a warning with a default
/// prefix, or silently skipping the sysroot-relative lookup).
#[must_use]
pub fn sysroot_from_exe_path(exe: &Path) -> Option<PathBuf> {
    exe.parent()?.parent().map(Path::to_path_buf)
}

/// Convenience wrapper: derives the sysroot from the current process's own
/// executable path ([`std::env::current_exe`]).
///
/// Returns `None` if the executable path can't be read, or has no
/// grandparent directory (see [`sysroot_from_exe_path`]).
#[must_use]
pub fn current_sysroot() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    sysroot_from_exe_path(&exe)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sysroot_from_exe_path_returns_grandparent_of_binary() {
        let exe = PathBuf::from("/opt/beamtalk/bin/beamtalk");
        assert_eq!(
            sysroot_from_exe_path(&exe),
            Some(PathBuf::from("/opt/beamtalk"))
        );
    }

    #[test]
    fn sysroot_from_exe_path_none_for_bare_filename() {
        // A bare filename has no parent directories to walk up from.
        let exe = PathBuf::from("beamtalk");
        assert_eq!(sysroot_from_exe_path(&exe), None);
    }

    #[test]
    fn sysroot_from_exe_path_none_for_single_component_path() {
        // "/beamtalk" has a parent ("/") but that parent has no parent itself.
        let exe = PathBuf::from("/beamtalk");
        assert_eq!(sysroot_from_exe_path(&exe), None);
    }
}
