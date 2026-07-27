// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Desktop picker shell-agnostic logic (ADR 0097, BT-2986).
//!
//! [`beamtalk_desktop_broker`] implements process supervision (discovery,
//! spawn, readiness, monitoring, reaping) with no GUI dependency. This crate
//! sits one layer up: the small amount of *picker* decision logic that is
//! itself still GUI-toolkit-agnostic (pure data + pure functions, no window
//! system, no I/O beyond what it delegates to the broker) but that the
//! broker deliberately left out of its own scope (`beamtalk-desktop-broker`'s
//! module docs: "does not build or wire a picker UI \[BT-2986\]").
//!
//! Kept separate from the actual GUI shell (a Tauri application — see
//! `desktop/` at the repo root, **not** a Cargo workspace member; it depends
//! on this crate and on [`beamtalk_desktop_broker`] as path dependencies) so
//! the decision logic that matters most for correctness — attach-twice
//! semantics, first-run empty-state classification — is unit-tested by
//! `just test`/`just ci` without requiring a Tauri toolchain or a display
//! server, neither of which the sandbox this crate was developed in has
//! available (see `desktop/README.md` for what that means for verification).
//!
//! ## What this crate is not
//!
//! - It does not open windows, register Tauri commands, or touch a webview —
//!   that is `desktop/`'s job.
//! - It does not spawn processes or poll HTTP — that is
//!   [`beamtalk_desktop_broker`]'s job; this crate only decides, given
//!   broker-shaped inputs, what a GUI shell should do next.
//!
//! **DDD Context:** Desktop Shell

/// Attach-twice / focus-existing decision and window-per-workspace label
/// bookkeeping (ADR 0097 "Single-instance policy and attach-twice
/// semantics", settled by the BT-2984 spike: attaching twice focuses the
/// existing front rather than spawning a second one).
pub mod attach;
/// First-run empty-state classification (ADR 0097 Broker §5 / User Impact:
/// "First run with no workspaces is a real state").
pub mod empty_state;
