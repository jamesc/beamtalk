// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: Beamtalk global config directory (`~/.beamtalk`) resolution.
//!
//! **DDD Context:** Infrastructure (shared leaf, no domain logic)
//!
//! This crate exists to give both `beamtalk-core` and `beamtalk-workspace` a
//! single authoritative `~/.beamtalk` resolution without either depending on
//! the other — the "shared-leaf-module pattern" from
//! `docs/development/architecture-principles.md` §6.

use std::path::PathBuf;

/// Returns the Beamtalk global config directory (`~/.beamtalk`), if resolvable.
///
/// Resolution, in priority order:
/// 1. `BEAMTALK_HOME` environment variable (explicit override) — used to
///    redirect every `~/.beamtalk/*` consumer (workspace storage, project
///    discovery's global-config-dir skip, ...) into a hermetic tempdir for
///    tests, the same pattern as `BEAMTALK_CACHE_DIR` for the FFI type-spec
///    cache and `BEAMTALK_WORKSPACE` for the session directory. An empty
///    value is treated as unset so `BEAMTALK_HOME=` in an inherited
///    environment doesn't silently resolve to the process's current
///    directory.
/// 2. The user's home directory ([`dirs::home_dir`]) joined with
///    `.beamtalk`.
///
/// Callers that need a `Result` (e.g. `beamtalk-workspace`) should wrap the
/// `None` case with their preferred error type.
pub fn beamtalk_root_dir() -> Option<PathBuf> {
    if let Ok(override_dir) = std::env::var("BEAMTALK_HOME") {
        if !override_dir.is_empty() {
            return Some(PathBuf::from(override_dir));
        }
    }
    dirs::home_dir().map(|h| h.join(".beamtalk"))
}

#[cfg(test)]
mod tests {
    use super::*;

    // Guards every test below that reads/mutates `BEAMTALK_HOME` — Rust runs
    // tests in this binary concurrently by default, and
    // `std::env::set_var`/`remove_var` racing a concurrent `env::var` read in
    // another thread is a real hazard, not a theoretical one (mirrors
    // `beamtalk-workspace`'s `EPMD_ENV_LOCK` discipline for the same reason).
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn beamtalk_root_dir_defaults_to_home_dot_beamtalk() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BEAMTALK_HOME") };
        let dir = beamtalk_root_dir().expect("home dir should resolve");
        assert_eq!(dir, dirs::home_dir().unwrap().join(".beamtalk"));
    }

    #[test]
    fn beamtalk_root_dir_respects_override() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BEAMTALK_HOME", "/tmp/some-hermetic-beamtalk-home") };
        let dir = beamtalk_root_dir();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BEAMTALK_HOME") };
        assert_eq!(dir, Some(PathBuf::from("/tmp/some-hermetic-beamtalk-home")));
    }

    #[test]
    fn beamtalk_root_dir_ignores_empty_override() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("BEAMTALK_HOME", "") };
        let dir = beamtalk_root_dir();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BEAMTALK_HOME") };
        assert_eq!(dir, dirs::home_dir().map(|h| h.join(".beamtalk")));
    }
}
