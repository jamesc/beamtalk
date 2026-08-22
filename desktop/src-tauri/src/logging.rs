// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Launcher-side structured logging (BT-3225): every real stage of the
//! attach/spawn/readiness flow gets a `tracing` event. Two sinks:
//! `~/.beamtalk/launcher.log` (persisted across restarts, `tail -f`-able —
//! mirrors `~/.beamtalk/workspaces/<id>/workspace.log`'s own convention on
//! the workspace-node side), and a channel drained by [`spawn_log_relay`]
//! once an [`AppHandle`] exists, forwarding each line to the picker UI's
//! log panel as a [`LOG_LINE_EVENT`].
//!
//! Without this, `beamtalk-desktop-broker`'s own `tracing::info!/warn!`
//! calls (`discovery.rs`, `reap.rs`) went nowhere — no subscriber was ever
//! installed, and a packaged build has no attached terminal to inherit
//! stdio from even if one had been (`windows_subsystem = "windows"` on
//! Windows; macOS/Linux app bundles have none either — see
//! `beamtalk_desktop_broker::spawn`'s `redirect_front_stdio`, the same gap
//! on the *spawned front's* side, fixed alongside this).

use std::io;
use std::path::PathBuf;
use std::sync::mpsc;

use tauri::{AppHandle, Emitter};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Frontend event name carrying one formatted log line — see
/// [`spawn_log_relay`].
pub const LOG_LINE_EVENT: &str = "launcher-log-line";

/// Keeps `tracing-appender`'s background flush thread alive; dropping this
/// stops file writes. [`init_logging`]'s caller must hold it for the
/// process lifetime — a `let` binding in `main()` that's never read again
/// is enough, since it only needs to out live `app.run()`, not be used by
/// it.
pub struct LoggingGuard {
    _file_guard: tracing_appender::non_blocking::WorkerGuard,
}

/// A line-buffering [`io::Write`] that forwards each complete line to `tx`
/// — the picker UI's log panel wants whole lines, not arbitrary byte
/// chunks, and `tracing_subscriber`'s `fmt` layer does not guarantee one
/// `write` call per event.
#[derive(Clone)]
struct ChannelWriter {
    tx: mpsc::Sender<String>,
}

impl io::Write for ChannelWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        for line in String::from_utf8_lossy(buf).lines() {
            // Best-effort: a closed receiver (log panel never opened, or
            // the app is mid-shutdown) just means no one is listening right
            // now — not a reason to fail a tracing write over.
            let _ = self.tx.send(line.to_string());
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// Initialize the launcher's `tracing` subscriber: a file layer at
/// `~/.beamtalk/launcher.log` (append, across restarts) plus a channel
/// layer whose receiver [`spawn_log_relay`] later drains. Falls back to the
/// OS temp dir if `~/.beamtalk` can't be resolved, rather than failing app
/// startup over a logging nicety.
///
/// Defaults to `info` (not the CLI's `warn`, see
/// `beamtalk-cli/src/main.rs`'s own `EnvFilter` setup) — this subscriber
/// exists specifically to make the attach/spawn/readiness stage events
/// (`crate::commands`) visible without requiring `RUST_LOG` to be set by
/// hand; `RUST_LOG` still overrides it exactly as the CLI's does.
///
/// Must be called exactly once, before any `tracing::` call this process
/// makes — including `beamtalk-desktop-broker`'s `discovery`/`reap` calls
/// `main()`'s own `.setup()` triggers.
#[must_use]
pub fn init_logging() -> (LoggingGuard, mpsc::Receiver<String>) {
    let root_dir = beamtalk_workspace::beamtalk_root_dir().unwrap_or_else(|_| std::env::temp_dir());
    let file_appender = tracing_appender::rolling::never(&root_dir, "launcher.log");
    let (non_blocking, file_guard) = tracing_appender::non_blocking(file_appender);

    let (tx, rx) = mpsc::channel();

    let file_layer = tracing_subscriber::fmt::layer()
        .with_writer(non_blocking)
        .with_ansi(false);
    let channel_layer = tracing_subscriber::fmt::layer()
        .with_writer(move || ChannelWriter { tx: tx.clone() })
        .with_ansi(false);

    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info"));

    tracing_subscriber::registry()
        .with(filter)
        .with(file_layer)
        .with(channel_layer)
        .init();

    (
        LoggingGuard {
            _file_guard: file_guard,
        },
        rx,
    )
}

/// Spawn the background thread that drains `rx` (fed by [`init_logging`]'s
/// channel layer) and forwards each line to the frontend as a
/// [`LOG_LINE_EVENT`]. Split out from `init_logging` because an
/// [`AppHandle`] only exists once the Tauri app is built — `init_logging`
/// itself must run earlier, before any other `tracing::` call in `main()`'s
/// own `.setup()`.
pub fn spawn_log_relay(app: AppHandle, rx: mpsc::Receiver<String>) {
    std::thread::spawn(move || {
        for line in rx {
            let _ = app.emit(LOG_LINE_EVENT, line);
        }
    });
}

/// Read the tail of `~/.beamtalk/launcher.log` — up to `limit` lines — to
/// seed the picker UI's log panel with recent history on open, before any
/// live [`LOG_LINE_EVENT`] has fired. Returns an empty vec (not an error)
/// if the file doesn't exist yet — nothing has logged since the log
/// directory was created, not a failure worth surfacing to the UI.
#[must_use]
pub fn read_recent_logs(limit: usize) -> Vec<String> {
    let Ok(root_dir) = beamtalk_workspace::beamtalk_root_dir() else {
        return Vec::new();
    };
    let path: PathBuf = root_dir.join("launcher.log");
    let Ok(contents) = std::fs::read_to_string(&path) else {
        return Vec::new();
    };
    let lines: Vec<String> = contents.lines().map(str::to_string).collect();
    let start = lines.len().saturating_sub(limit);
    lines[start..].to_vec()
}
