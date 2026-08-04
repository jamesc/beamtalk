// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tauri-managed application state (ADR 0097, BT-2986).

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Mutex;

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
            launcher,
        }
    }
}
