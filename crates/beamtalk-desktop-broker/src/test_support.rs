// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test-only shared state.
//!
//! `cargo test` runs every test in a crate's test binary concurrently by
//! default, but `std::env::set_var`/`remove_var` mutate process-global state.
//! Any test that touches the `BT_OIDC_*` env vars — in `oidc_guard.rs` and
//! `spawn.rs`, which both refuse-to-spawn on the same env — must serialize
//! against every *other* such test in this crate, not just the ones in its
//! own module. A single crate-wide lock does that; per-module locks would
//! not, since two different `Mutex` instances don't exclude each other.

use std::sync::Mutex;

/// Hold this for the duration of any test that reads or writes `BT_OIDC_*`
/// or `BT_IDE_CONFIG`.
pub(crate) static ENV_LOCK: Mutex<()> = Mutex::new(());

/// Acquire [`ENV_LOCK`], recovering from poisoning.
///
/// The lock guards no data (`Mutex<()>`), only mutual exclusion, so a
/// panicking test leaves nothing inconsistent behind. A bare
/// `.lock().unwrap()` would let one failing test poison the lock and turn
/// every later test that takes it into a spurious `PoisonError` failure
/// (BT-3698), hiding the real failure.
pub(crate) fn lock_env() -> std::sync::MutexGuard<'static, ()> {
    ENV_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}
