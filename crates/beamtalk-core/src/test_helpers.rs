// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for use in beamtalk-core and dependent crate tests.

use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// Creates a unique temporary directory path (does not create it on disk).
/// Uses PID + nanosecond timestamp to avoid collisions between parallel tests.
///
/// # Panics
///
/// Panics if the system clock is set before the Unix epoch.
pub fn unique_temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    std::env::temp_dir().join(format!("{prefix}_{}_{}", std::process::id(), nanos))
}
