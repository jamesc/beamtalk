// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tauri-managed application state (ADR 0097, BT-2986).

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Condvar, Mutex, PoisonError};
use std::time::{Duration, Instant};

use beamtalk_desktop_broker::spawn::SpawnedFront;
use beamtalk_desktop_shell::attach::AttachManager;

/// Shared state behind `app.manage(...)`.
pub struct AppState {
    /// Attach-twice / focus-existing decisions and window-per-workspace
    /// bookkeeping (`beamtalk-desktop-shell`).
    pub attach: Mutex<AttachManager>,
    /// The OS child handle for each attached front, keyed by workspace id —
    /// kept separately from `AttachManager` so that crate stays free of
    /// `std::process` (see its module docs on why it's pure).
    ///
    /// `SpawnedFront`, not a bare `std::process::Child`: on Windows it also
    /// carries the Job Object handle tying the front's *entire* process tree
    /// to this map entry's lifetime (BT-2988, adversarial-review follow-up —
    /// see `beamtalk_desktop_broker::winjob`'s module doc). Dropping an
    /// entry (removed by `kill_and_untrack`/`detach_internal`, or all of
    /// them by a broker crash tearing down its handle table) kills
    /// `erl.exe` there too, not just the `cmd.exe`/wrapper `Child::kill()`
    /// alone would reach.
    pub children: Mutex<HashMap<String, SpawnedFront>>,
    /// Fronts taken out of `children` and still being stopped (BT-5194):
    /// stopping happens outside the `children` lock, so quit waits on this
    /// rather than on the lock for a stop another thread started.
    pub stops_in_flight: StopsInFlight,
    /// Path to the launcher for the bundled `bt_attach` release —
    /// `bin/server` on Unix, `bin\bt_attach.bat` on Windows (BT-2988).
    /// Resolved once at startup (see `crate::launcher`).
    pub launcher: PathBuf,
}

impl AppState {
    #[must_use]
    pub fn new(launcher: PathBuf) -> Self {
        Self {
            attach: Mutex::new(AttachManager::new()),
            children: Mutex::new(HashMap::new()),
            stops_in_flight: StopsInFlight::default(),
            launcher,
        }
    }
}

/// A count of front stops in progress, which [`StopsInFlight::wait_idle`] can
/// wait to reach zero.
#[derive(Debug, Default)]
pub struct StopsInFlight {
    count: Mutex<usize>,
    idle: Condvar,
}

impl StopsInFlight {
    /// Count one more stop until the returned guard drops (also on unwind).
    pub fn begin(&self) -> StopGuard<'_> {
        *self.count.lock().unwrap_or_else(PoisonError::into_inner) += 1;
        StopGuard(self)
    }

    /// Wait until no stop is in progress, or `timeout` passes. Returns
    /// whether it reached idle.
    pub fn wait_idle(&self, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut count = self.count.lock().unwrap_or_else(PoisonError::into_inner);
        while *count > 0 {
            let Some(left) = deadline.checked_duration_since(Instant::now()) else {
                return false;
            };
            count = self
                .idle
                .wait_timeout(count, left)
                .unwrap_or_else(PoisonError::into_inner)
                .0;
        }
        true
    }
}

/// Ends one counted stop on drop; see [`StopsInFlight::begin`].
#[derive(Debug)]
pub struct StopGuard<'a>(&'a StopsInFlight);

impl Drop for StopGuard<'_> {
    fn drop(&mut self) {
        let mut count = self.0.count.lock().unwrap_or_else(PoisonError::into_inner);
        *count -= 1;
        if *count == 0 {
            self.0.idle.notify_all();
        }
    }
}
