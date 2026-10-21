// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 0 prototype switch (BT-4007).
//!
//! **DDD Context:** Compilation
//!
//! `BEAMTALK_ADR0137_PROTO=1` turns on the measurement prototype of the
//! "actor closures see the actor's live state" model: the analysis stops
//! rejecting a closure that writes a field, and codegen brackets calls that
//! may run user code with a store of `State` to the process-dictionary cell
//! (`'$bt_actor_state'`) and a load back, routes a closure's field access
//! through `beamtalk_actor_state_proto`, and lets a closure's self-send run
//! through `sync_send` on the cell. It is **off by default** and changes no
//! output when off. It is a measurement aid for Phase 0, not the Phase 2
//! implementation: the real flag-free lowering (`SyncedCall` in `ThreadedIr`,
//! the `dispatch(Selector, Args, Self)` calling convention) replaces it.
//!
//! Read once per process. The environment variable (rather than a
//! codegen-options field) keeps the prototype out of every constructor between
//! the CLI, the REPL and the test runners.

use std::sync::OnceLock;

/// The environment variable that turns the prototype on.
pub const ADR0137_PROTO_ENV: &str = "BEAMTALK_ADR0137_PROTO";

/// Whether the ADR 0137 Phase 0 prototype is on for this process.
#[must_use]
pub fn adr0137_proto_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var_os(ADR0137_PROTO_ENV).is_some_and(|v| !v.is_empty() && v != "0")
    })
}
