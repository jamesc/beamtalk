// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Error type for the desktop broker core.
//!
//! **DDD Context:** Desktop Shell

use std::time::Duration;

/// Errors the broker core can produce.
///
/// Kept as a concrete enum (rather than `miette::Report`, which the CLI/
/// workspace crates use for terminal-formatted diagnostics) because a future
/// Tauri shell needs to match on *why* an operation failed to pick
/// a UI treatment (e.g. `OidcConfigured` shows setup instructions,
/// `FrontExitedBeforeReportingPort` points at `attach.log`) rather than only display a formatted string.
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
    /// — the Windows spawn path needs to read `BT_WORKSPACE_COOKIE`
    /// itself before invoking `bin\bt_attach.bat` directly, since there is no
    /// `bin/server` shell script there to resolve it. On Unix this can't
    /// happen in practice (`bin/server` re-resolves the cookie itself and
    /// would fail the same way), but it's a real, reachable state here.
    #[error("workspace '{0}' has no cookie file under ~/.beamtalk/workspaces/{0}/cookie")]
    MissingCookie(String),

    /// A workspace directory exists but `metadata.json` has no `node_name`
    /// field yet — the workspace was created but never started, so
    /// `beamtalk_workspace_meta` (Erlang) never wrote a real node name back
    /// to disk. The Windows spawn path needs `node_name` itself
    /// before invoking `bin\bt_attach.bat` directly (there is no `bin/server`
    /// shell script there to resolve it), and hard-fails here rather than
    /// guessing at a name via [`crate::discovery::default_node_name`] —
    /// matching `bin/server`'s Unix fail-fast behavior instead of silently
    /// dist-connecting to a name that only *usually* matches the CLI's own
    /// naming convention.
    #[error(
        "workspace '{0}' has no node_name in ~/.beamtalk/workspaces/{0}/metadata.json yet — \
         start it first: beamtalk workspace create {0} --background --persistent"
    )]
    MissingNodeName(String),

    /// The spawned front exited before reporting the HTTP port it bound
    /// (see [`crate::port`]) — it crashed or refused during boot (bad
    /// release, config error, a port it couldn't bind). `exit_status` is the
    /// launcher process's `std::process::ExitStatus` `Display` output; the
    /// front's own explanation is in `~/.beamtalk/workspaces/<id>/attach.log`.
    #[error(
        "the front exited ({exit_status}) before reporting its HTTP port — \
         see ~/.beamtalk/workspaces/<id>/attach.log"
    )]
    FrontExitedBeforeReportingPort { exit_status: String },

    /// The spawned front is still running but did not report its HTTP port
    /// within the configured timeout (see
    /// [`crate::port::DEFAULT_PORT_REPORT_TIMEOUT`]).
    #[error("the front did not report its HTTP port within {timeout:?}")]
    PortReportTimedOut { timeout: Duration },

    /// The front's port file exists but doesn't hold a single non-zero port
    /// number. The front writes it atomically, so this means a foreign or
    /// incompatible writer, never a half-written file.
    #[error("invalid front port file {path}: {content:?}")]
    InvalidPortFile { path: String, content: String },

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
    /// on Windows, or `bin\bt_attach.bat` on Unix. Caught by a
    /// cheap extension check in `spawn::build_launch_command` *before*
    /// `Command::spawn`, so the failure is this named error rather than an
    /// opaque OS error (`os error 193` — "not a valid Win32 application" —
    /// when Windows tries to exec a shebang script) or a less specific
    /// [`BrokerError::FrontExitedBeforeReportingPort`] once the wrong
    /// launcher exits immediately.
    #[error("launcher path '{0}' does not look like a valid entry point for this platform — {1}")]
    LauncherPlatformMismatch(String, &'static str),

    /// `SpawnConfig::launcher` looks like a valid entry point for this
    /// platform ([`BrokerError::LauncherPlatformMismatch`] already passed),
    /// but no file exists there — the far more common real-world spawn
    /// failure in practice: an
    /// unpackaged dev build where `BEAMTALK_ATTACH_LAUNCHER` /
    /// `desktop/src-tauri/src/launcher.rs`'s `resolve_launcher_path` fallback
    /// path hasn't been built yet. Checked in `spawn::spawn_front`
    /// immediately before `Command::spawn` (deliberately *not* inside
    /// `build_launch_command`, which several of that function's own unit
    /// tests exercise against synthetic non-existent launcher paths purely
    /// for `Command` introspection) — this named variant replaces what used
    /// to surface as an opaque [`BrokerError::Io`] from the failed
    /// `Command::spawn` call itself.
    #[error(
        "launcher '{0}' does not exist — is the release built/bundled yet? \
         (see BEAMTALK_ATTACH_LAUNCHER for pointing at a local dev build)"
    )]
    LauncherNotFound(String),
}

impl From<miette::Report> for BrokerError {
    fn from(report: miette::Report) -> Self {
        Self::Workspace(report.to_string())
    }
}

/// Convenience alias.
pub type Result<T> = std::result::Result<T, BrokerError>;
