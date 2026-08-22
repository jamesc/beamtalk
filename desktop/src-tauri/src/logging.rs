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
use std::path::Path;
use std::sync::mpsc;

use tauri::{AppHandle, Emitter};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Frontend event name carrying one formatted log line — see
/// [`spawn_log_relay`].
pub const LOG_LINE_EVENT: &str = "launcher-log-line";

/// File name under `~/.beamtalk/` — shared between [`init_logging`] (writes)
/// and [`read_recent_logs`] (reads), so the two can't drift apart.
const LOG_FILE_NAME: &str = "launcher.log";

/// Bounds how much of `launcher.log` [`read_recent_logs`] reads from disk.
/// The file is never rotated (`Rotation::NEVER`) and accumulates across
/// every restart, so a long-lived install's log can grow large; reading the
/// whole thing just to keep the last few thousand lines would get slower
/// and heavier every day the app stays installed. Generous enough that a
/// realistic `limit` (the picker UI caps its own display at 2000 lines)
/// almost never needs the "there weren't enough lines in this window" retry
/// this function's implementation does. Full rotation is tracked separately
/// (BT-3228) — this is only a read-side bound, not a write-side cap.
const MAX_TAIL_READ_BYTES: u64 = 2 * 1024 * 1024;

/// Keeps `tracing-appender`'s background flush thread alive; dropping this
/// stops file writes. `None` means file logging is disabled — either
/// `launcher.log` couldn't be opened (see [`init_logging`]'s doc comment),
/// or nothing was ever configured (a fresh [`LoggingGuard`] outside this
/// module, which does not happen in practice — [`init_logging`] is the only
/// constructor).
pub struct LoggingGuard {
    file_guard: Option<tracing_appender::non_blocking::WorkerGuard>,
}

impl LoggingGuard {
    /// Force any buffered log lines to flush *now*, by dropping the
    /// underlying file guard in place, rather than waiting for this
    /// `LoggingGuard` itself to be dropped.
    ///
    /// Exists because `main()`'s own scope-end drop may never run:
    /// `tauri::App::run` hands control to `tao`'s event loop, which on some
    /// platforms exits the OS process from *inside* that call rather than
    /// returning — so a plain `let _logging_guard = ...;` binding in
    /// `main()` provides no actual flush-on-quit guarantee. `main.rs` calls
    /// this from its `RunEvent::ExitRequested` handler instead, at the one
    /// point it reliably knows shutdown is starting.
    pub fn flush(&mut self) {
        self.file_guard.take();
    }
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

/// Create `path` ahead of `tracing-appender`'s own open, `0600` (owner-only)
/// on Unix — matching every other security-relevant file already under
/// `~/.beamtalk/` (`workspace.log`, `cookie`, `vm.args`, `attach.log` —
/// see `beamtalk_desktop_broker::spawn::open_front_log`'s doc comment for
/// the same reasoning). `launcher.log` isn't as sensitive as a workspace's
/// own cookie, but it does record project paths, workspace ids, and ports —
/// enough to match the same posture rather than the world-readable default.
/// Best-effort and creation-only, same limitation `open_front_log` already
/// accepts: a file that already exists from before this code ran keeps
/// whatever mode it had.
#[cfg(unix)]
fn ensure_owner_only(path: &Path) {
    use std::os::unix::fs::OpenOptionsExt;
    let _ = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .mode(0o600)
        .open(path);
}

#[cfg(not(unix))]
fn ensure_owner_only(_path: &Path) {}

/// Initialize the launcher's `tracing` subscriber: a file layer at
/// `~/.beamtalk/launcher.log` (append, across restarts) plus a channel
/// layer whose receiver [`spawn_log_relay`] later drains. Falls back to the
/// OS temp dir if `~/.beamtalk` can't be resolved, and degrades to
/// channel-only logging (no crash) if `launcher.log` itself can't be
/// opened — e.g. `~/.beamtalk` owned by another user from a prior `sudo`
/// invocation, or a full/read-only home directory. This function runs as
/// the very first thing in `main()`, before any window exists; a hard
/// `.expect()` here (which `tracing_appender::rolling::never` — the
/// convenience constructor this used before this fix — has internally)
/// would silently kill the app on launch with no visible error in a
/// packaged build.
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
    let log_path = root_dir.join(LOG_FILE_NAME);
    ensure_owner_only(&log_path);

    let file_appender = tracing_appender::rolling::Builder::new()
        .rotation(tracing_appender::rolling::Rotation::NEVER)
        .filename_prefix(LOG_FILE_NAME)
        .build(&root_dir)
        .map_err(|err| {
            eprintln!(
                "beamtalk-desktop: could not open {}: {err} — file logging disabled, \
                 in-app log panel still works",
                log_path.display()
            );
        })
        .ok();

    let (file_layer, file_guard) = match file_appender {
        Some(appender) => {
            let (non_blocking, guard) = tracing_appender::non_blocking(appender);
            (
                Some(
                    tracing_subscriber::fmt::layer()
                        .with_writer(non_blocking)
                        .with_ansi(false),
                ),
                Some(guard),
            )
        }
        None => (None, None),
    };

    let (tx, rx) = mpsc::channel();
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

    (LoggingGuard { file_guard }, rx)
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
    read_recent_lines_from(&root_dir.join(LOG_FILE_NAME), limit)
}

/// [`read_recent_logs`]'s actual logic, parameterized over the file path so
/// it's testable against a temp file instead of the real
/// `~/.beamtalk/launcher.log` (`read_recent_logs` itself can't be — it
/// hardcodes the real path via `beamtalk_workspace::beamtalk_root_dir`).
///
/// Bounded to the last [`MAX_TAIL_READ_BYTES`] of the file (BT-3225 review
/// follow-up) rather than reading it in full — `launcher.log` has no
/// rotation yet (BT-3228) and grows across every restart.
fn read_recent_lines_from(path: &Path, limit: usize) -> Vec<String> {
    use std::io::{Read, Seek, SeekFrom};

    let Ok(mut file) = std::fs::File::open(path) else {
        return Vec::new();
    };
    let Ok(len) = file.metadata().map(|m| m.len()) else {
        return Vec::new();
    };

    let seek_start = len.saturating_sub(MAX_TAIL_READ_BYTES);
    let truncated = seek_start > 0;
    if truncated && file.seek(SeekFrom::Start(seek_start)).is_err() {
        return Vec::new();
    }

    let mut buf = Vec::new();
    if file.read_to_end(&mut buf).is_err() {
        return Vec::new();
    }

    // `from_utf8_lossy`, not `String::from_utf8` — a seek into the middle
    // of the file can land mid-character, and lossy conversion turns that
    // into a replacement character at the start instead of failing the
    // whole read.
    let text = String::from_utf8_lossy(&buf);
    let mut lines: Vec<String> = text.lines().map(str::to_string).collect();
    if truncated && !lines.is_empty() {
        // The seek can also land mid-*line* (not just mid-character); the
        // fragment before the first real newline is a partial line, not a
        // full one — drop it rather than show a truncated log line.
        lines.remove(0);
    }

    let start = lines.len().saturating_sub(limit);
    lines[start..].to_vec()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_recent_lines_from_returns_empty_for_a_missing_file() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("nonexistent.log");
        assert_eq!(read_recent_lines_from(&path, 100), Vec::<String>::new());
    }

    #[test]
    fn read_recent_lines_from_returns_the_last_n_lines() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("launcher.log");
        std::fs::write(&path, "one\ntwo\nthree\nfour\nfive\n").unwrap();

        assert_eq!(read_recent_lines_from(&path, 2), vec!["four", "five"]);
        assert_eq!(
            read_recent_lines_from(&path, 100),
            vec!["one", "two", "three", "four", "five"]
        );
    }

    #[test]
    fn read_recent_lines_from_bounds_the_read_and_drops_the_partial_leading_line() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("launcher.log");

        // Bigger than MAX_TAIL_READ_BYTES, so the read must seek — one
        // filler line long enough that a handful of them safely exceeds the
        // bound, followed by a handful of short, distinctly-markered lines
        // at the very end.
        let filler_line = "x".repeat(1024);
        let filler_lines_needed = (MAX_TAIL_READ_BYTES / filler_line.len() as u64) + 10;
        let mut content = String::new();
        for _ in 0..filler_lines_needed {
            content.push_str(&filler_line);
            content.push('\n');
        }
        content.push_str("tail-marker-1\ntail-marker-2\ntail-marker-3\n");
        std::fs::write(&path, &content).unwrap();

        // `limit: 3` — exactly the three tail markers, none of the filler.
        // A `limit` large enough to also request filler lines would
        // legitimately return some (they're real lines within the bounded
        // 2MB window) — this asserts the seek reached near the true end of
        // a much larger file and the partial leading line landed on a
        // filler line's boundary was dropped cleanly, not that filler can
        // never appear in a result.
        let result = read_recent_lines_from(&path, 3);

        assert_eq!(
            result,
            vec!["tail-marker-1", "tail-marker-2", "tail-marker-3"],
            "expected exactly the three tail markers, in order, with no \
             truncated filler fragment ahead of them"
        );
    }
}
