// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for the `beamtalk-repl` integration-test binaries.

// One shared batched-`erlc` driver lives in `beamtalk_core::test_helpers`
// (BT-4886) so the REPL and codegen test crates don't each carry a copy.
#[allow(unused_imports)] // some test binaries don't call every helper
pub use beamtalk_core::test_helpers::test_support::{erlc_batched, erlc_batched_in_temp_dir};
