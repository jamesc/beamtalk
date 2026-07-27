// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tauri-managed application state (ADR 0097, BT-2986).

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Child;
use std::sync::Mutex;

use beamtalk_desktop_shell::attach::AttachManager;

/// Shared state behind `app.manage(...)`.
pub struct AppState {
    /// Attach-twice / focus-existing decisions and window-per-workspace
    /// bookkeeping (`beamtalk-desktop-shell`).
    pub attach: Mutex<AttachManager>,
    /// The OS child handle for each attached front, keyed by workspace id —
    /// kept separately from `AttachManager` so that crate stays free of
    /// `std::process` (see its module docs on why it's pure).
    pub children: Mutex<HashMap<String, Child>>,
    /// Path to the `bin/server` launcher for the bundled `bt_attach`
    /// release. Resolved once at startup (see `crate::launcher`).
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
