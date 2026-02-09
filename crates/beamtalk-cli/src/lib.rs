// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL startup configuration.
//!
//! **DDD Context:** REPL — Startup Configuration
//!
//! This module provides the canonical BEAM node startup configuration used by
//! both the production `beamtalk repl` command and the E2E test harness.
//! Keeping these in one place prevents the test startup path from diverging
//! from production (see BT-390).

pub mod repl_startup;
