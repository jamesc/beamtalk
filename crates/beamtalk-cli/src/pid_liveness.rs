// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-platform process-liveness check — re-exported from `beamtalk-workspace`
//! so `beamtalk-desktop-broker` (which cannot depend on `beamtalk-cli`) can
//! share the same implementation. See `beamtalk_workspace::pid_liveness`.

pub use beamtalk_workspace::pid_liveness::is_process_alive;
