// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The ADR 0137 Phase 2a (BT-4020) synced-calls switch.
//!
//! **DDD Context:** Compilation
//!
//! With the switch on, codegen brackets every call that may run user code in
//! an actor instance method with a store of the threaded `State` to the
//! actor-state cell and a reload after it, and the block analysis counts such
//! a call as a state effect (so a loop, fold or arm that holds one threads
//! `State`). It is **off by default** and changes no output when off.
//!
//! The switch lives here, below both the analysis and the code generator,
//! because both read it. `BEAMTALK_SYNCED_CALLS` selects the mode, read once
//! per process: unset or `0` is [`SyncedCallsMode::Off`]; `count` builds the
//! brackets and records the calls left outside one without failing (the
//! count-by-region tool); anything else is [`SyncedCallsMode::Enforce`],
//! where such a call is a verifier finding. A test can override the mode for
//! the current thread with [`override_mode`].

use std::cell::Cell;
use std::sync::OnceLock;

/// The environment variable that selects the mode.
pub const SYNCED_CALLS_ENV: &str = "BEAMTALK_SYNCED_CALLS";

/// Whether and how the cell bracket is built and checked.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncedCallsMode {
    /// Off (the default): nothing is built or recorded.
    Off,
    /// Brackets are built; calls outside one are recorded, not reported.
    Count,
    /// Brackets are built; a call outside one is a verifier finding.
    Enforce,
}

thread_local! {
    static OVERRIDE: Cell<Option<SyncedCallsMode>> = const { Cell::new(None) };
}

/// The mode in force on this thread.
#[must_use]
pub fn mode() -> SyncedCallsMode {
    if let Some(overridden) = OVERRIDE.with(Cell::get) {
        return overridden;
    }
    static ENV: OnceLock<SyncedCallsMode> = OnceLock::new();
    *ENV.get_or_init(|| match std::env::var(SYNCED_CALLS_ENV) {
        Ok(v) if v == "count" => SyncedCallsMode::Count,
        Ok(v) if !v.is_empty() && v != "0" => SyncedCallsMode::Enforce,
        _ => SyncedCallsMode::Off,
    })
}

/// Whether the brackets are being built (the mode is not off).
#[must_use]
pub fn enabled() -> bool {
    mode() != SyncedCallsMode::Off
}

/// Restores the previous per-thread override on drop.
#[derive(Debug)]
pub struct OverrideGuard(Option<SyncedCallsMode>);

impl Drop for OverrideGuard {
    fn drop(&mut self) {
        OVERRIDE.with(|cell| cell.set(self.0));
    }
}

/// Overrides the mode for the current thread until the guard is dropped.
#[must_use]
pub fn override_mode(mode: SyncedCallsMode) -> OverrideGuard {
    OverrideGuard(OVERRIDE.with(|cell| cell.replace(Some(mode))))
}
