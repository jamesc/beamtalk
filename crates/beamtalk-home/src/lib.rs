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

/// Returns the Beamtalk global config directory (`~/.beamtalk`), if the home
/// directory is resolvable.
///
/// Callers that need a `Result` (e.g. `beamtalk-workspace`) should wrap the
/// `None` case with their preferred error type.
pub fn beamtalk_root_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".beamtalk"))
}
