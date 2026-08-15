// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk warm-otp-cache` — warm the shared, OTP-version-keyed type-spec
//! cache without a Beamtalk project (BT-2471).
//!
//! `beamtalk build`/`beamtalk lint` already populate the shared cache
//! introduced by BT-2470 (`<cache>/beamtalk/otp-specs/<otp>-<erts>/`) as a
//! side effect of extracting FFI type specs for a real project. This command
//! exists for callers that want to pay that cost *before* any project build
//! runs — e.g. `.claude/hooks/worktree-init.sh` (the `SessionStart` hook),
//! which fires this in the background at the top of a fresh
//! Claude-Code-on-the-web session so a freshly cloned container's first
//! interactive `beamtalk build` finds the shared OTP tier already warm
//! instead of paying cold extraction on the critical path. See
//! `docs/development/remote-sessions.md`.
//!
//! Deliberately project-agnostic: it extracts only the common OTP apps
//! (`stdlib`, `kernel`, `erts`, …) discovered on the running node's code
//! path — the same set [`beamtalk_core::ffi_type_specs::extract_type_specs`]
//! extracts for every project — and passes no dependency ebin dirs, so no
//! `beamtalk.toml`/project root is required. The local (non-shared) cache
//! tier it writes to is a fresh [`tempfile::TempDir`] — auto-deleted when
//! this process exits — never a real project's `_build/type_cache/`, since
//! only the shared tier this command exists to warm is meant to outlive it.
//!
//! Idempotent and cheap when already warm: [`extract_type_specs`] checks the
//! shared tier first and spawns no `beamtalk_build_worker` BEAM node at all
//! when every OTP module is already cached under the current OTP/ERTS
//! version key (BT-2470's cache-hit fast path) — the only work is discovering
//! the current OTP version and the on-disk `.beam` listing.
//!
//! Non-fatal by design: if the Beamtalk runtime isn't compiled yet, or `erl`
//! is unavailable, this prints a short status line and exits 0 rather than
//! failing — a background warmer must never surface as a build error, and a
//! session-start hook backgrounding this command shouldn't treat "runtime
//! not built yet" as anything other than "nothing to warm yet".

use beamtalk_core::ffi_type_specs::extract_type_specs;
use camino::Utf8PathBuf;
use miette::{Context, IntoDiagnostic, Result};

/// Runs OTP type-spec cache warming and prints a one-line status.
///
/// Returns `Ok(())` for every *extraction* outcome — missing runtime, `erl`
/// not on `PATH`, or no OTP `.beam` files found are non-fatal and reported
/// via the status line only, matching [`extract_type_specs`]'s own
/// non-fatal design. The only error this can return is failing to create the
/// scratch local-tier temp directory (e.g. a full or read-only system temp
/// dir) — callers that background this command already discard its exit
/// status, so even that is never treated as a build failure.
pub fn run() -> Result<()> {
    // Scratch local-tier directory: this command has no project `_build/` of
    // its own, and only the shared tier `extract_type_specs` resolves from
    // the running OTP version is meant to persist. A fresh `TempDir` (via the
    // cross-platform system temp dir, never a hardcoded `/tmp/`) keeps repeat
    // invocations honest — a stable path here would let a *local*-tier hit
    // mask a cold *shared* tier across runs with different
    // `BEAMTALK_CACHE_DIR`s — and is auto-removed on drop, so nothing
    // accumulates under the system temp dir across sessions.
    let local_cache_tempdir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create scratch local-tier cache directory")?;
    let local_cache_dir = Utf8PathBuf::from_path_buf(local_cache_tempdir.path().to_path_buf())
        .map_err(|path| miette::miette!("Non-UTF-8 temp directory path: {}", path.display()))?;

    match extract_type_specs(&local_cache_dir, &[]) {
        Some(registry) => {
            println!(
                "OTP type-spec cache warm: {} modules, {} functions",
                registry.module_count(),
                registry.function_count()
            );
        }
        None => {
            println!(
                "OTP type-spec cache not warmed (Beamtalk runtime not built yet, or no OTP .beam files found)"
            );
        }
    }

    Ok(())
}
