// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Error type for the desktop broker core.
//!
//! **DDD Context:** Desktop Shell

use std::fmt;

/// Display wrapper for [`BrokerError::PortsExhausted`]'s `last_exit_status`.
///
/// Renders `" — last launcher exit: <status>"` when a diagnostic is present,
/// or an empty string when `None` (the pre-BT-3045 message shape, unchanged
/// when there's nothing more to say).
struct DisplayLastExitStatus<'a>(&'a Option<String>);

impl fmt::Display for DisplayLastExitStatus<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(status) => write!(f, " — last launcher exit: {status}"),
            None => Ok(()),
        }
    }
}

/// Errors the broker core can produce.
///
/// Kept as a concrete enum (rather than `miette::Report`, which the CLI/
/// workspace crates use for terminal-formatted diagnostics) because a future
/// Tauri shell (BT-2986) needs to match on *why* an operation failed to pick
/// a UI treatment (e.g. `OidcConfigured` shows setup instructions, `PortsExhausted`
/// just retries) rather than only display a formatted string.
#[derive(Debug, thiserror::Error)]
pub enum BrokerError {
    /// `~/.beamtalk/ide.toml` or a `BT_OIDC_*` env var is present. Spawning a
    /// front would silently half-enforce remote auth on what must stay the
    /// local, unauthenticated posture (ADR 0097 "Local-only posture").
    #[error(
        "refusing to spawn: OIDC configuration detected ({0}) — the desktop broker is a \
         local-only tool and must not run against an OIDC-configured front (ADR 0097)"
    )]
    OidcConfigured(String),

    /// No workspace exists at `~/.beamtalk/workspaces/<id>/`.
    #[error("no workspace '{0}' under ~/.beamtalk/workspaces/")]
    UnknownWorkspace(String),

    /// A workspace directory exists but has no `cookie` file (or it's empty)
    /// — the Windows spawn path (BT-2988) needs to read `BT_WORKSPACE_COOKIE`
    /// itself before invoking `bin\bt_attach.bat` directly, since there is no
    /// `bin/server` shell script there to resolve it. On Unix this can't
    /// happen in practice (`bin/server` re-resolves the cookie itself and
    /// would fail the same way), but it's a real, reachable state here.
    #[error("workspace '{0}' has no cookie file under ~/.beamtalk/workspaces/{0}/cookie")]
    MissingCookie(String),

    /// Ran out of port-allocation attempts (see [`crate::port::allocate_port_with_retry`]).
    ///
    /// `last_exit_status`, when available, is the most recent spawn attempt's
    /// launcher process exit status (`std::process::ExitStatus`'s own
    /// `Display` — e.g. `"exit status: 1"` on Unix or `"exit code:
    /// 0xc0000135"` on Windows), threaded through from
    /// [`crate::port::SpawnAttempt::PortTaken`]. Carrying it matters because
    /// [`crate::spawn::spawn_front_with_port_retry`]'s retry signal is a
    /// heuristic (any early process exit looks like a port conflict — see its
    /// doc comment), not a proof: a genuine launcher failure unrelated to
    /// port conflicts (a crash, a missing DLL, `bin\bt_attach.bat` rejecting
    /// its `start` invocation for some Windows-specific reason) would
    /// previously report only a bare attempt count here, reading as "every
    /// candidate port really was taken" even when that was never true (BT-3045
    /// adversarial-review follow-up).
    #[error(
        "failed to allocate a free port after {attempts} attempt(s){}",
        DisplayLastExitStatus(.last_exit_status)
    )]
    PortsExhausted {
        attempts: u32,
        last_exit_status: Option<String>,
    },

    /// The installed `beamtalk` CLI could not be located (checked `PATH` and
    /// the configured fallback locations). GUI apps launched from a dock/
    /// Finder don't inherit the user's shell `PATH` (ADR 0097 Broker §1a).
    #[error(
        "could not locate the 'beamtalk' CLI (checked PATH and standard install locations) — \
         install it or set BEAMTALK_CLI_PATH"
    )]
    CliNotFound,

    /// The installed CLI exited non-zero.
    #[error("'beamtalk {0}' exited with status {1}: {2}")]
    CliFailed(String, i32, String),

    /// An I/O error (spawn, file read/write, socket).
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    /// Malformed `metadata.json` or another JSON payload.
    #[error("invalid JSON: {0}")]
    Json(#[from] serde_json::Error),

    /// A `miette`-reported error from `beamtalk-workspace` (path resolution,
    /// epmd protocol errors, …), converted to a string so `BrokerError` stays
    /// `Send + Sync + 'static` without depending on `miette::Report`'s own
    /// (non-`Sync` in some configurations) internals.
    #[error("{0}")]
    Workspace(String),

    /// `SpawnConfig::launcher` (or its `BEAMTALK_ATTACH_LAUNCHER` override,
    /// see `desktop/src-tauri/src/launcher.rs`) points at an entry point
    /// built for the wrong platform — e.g. a Unix `bin/server` shell script
    /// on Windows, or `bin\bt_attach.bat` on Unix (BT-3046). Caught by a
    /// cheap extension check in `spawn::build_launch_command` *before*
    /// `Command::spawn`, so the failure is this named error rather than an
    /// opaque OS error (`os error 193` — "not a valid Win32 application" —
    /// when Windows tries to exec a shebang script) or a confusing
    /// `PortsExhausted` from `spawn_front_with_port_retry`'s bind-failure
    /// heuristic misreading every immediate exit as a port conflict.
    #[error("launcher path '{0}' does not look like a valid entry point for this platform — {1}")]
    LauncherPlatformMismatch(String, &'static str),
}

impl From<miette::Report> for BrokerError {
    fn from(report: miette::Report) -> Self {
        Self::Workspace(report.to_string())
    }
}

/// Convenience alias.
pub type Result<T> = std::result::Result<T, BrokerError>;
