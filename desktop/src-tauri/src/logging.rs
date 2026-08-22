// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Launcher-side structured logging (BT-3225): every real stage of the
//! attach/spawn/readiness flow gets a `tracing` event. Two sinks:
//! `~/.beamtalk/launcher.log.<date>` (persisted across restarts, `tail
//! -f`-able, rotated daily and pruned after a week — BT-3229 — mirrors
//! `~/.beamtalk/workspaces/<id>/workspace.log`'s own convention on the
//! workspace-node side otherwise), and a channel drained by
//! [`spawn_log_relay`] once an [`AppHandle`] exists, forwarding each line
//! to the picker UI's log panel as a [`LOG_LINE_EVENT`].
//!
//! Without this, `beamtalk-desktop-broker`'s own `tracing::info!/warn!`
//! calls (`discovery.rs`, `reap.rs`) went nowhere — no subscriber was ever
//! installed, and a packaged build has no attached terminal to inherit
//! stdio from even if one had been (`windows_subsystem = "windows"` on
//! Windows; macOS/Linux app bundles have none either — see
//! `beamtalk_desktop_broker::spawn`'s `redirect_front_stdio`, the same gap
//! on the *spawned front's* side, fixed alongside this).

use std::io;
use std::path::{Path, PathBuf};
use std::sync::mpsc;

use tauri::{AppHandle, Emitter};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Frontend event name carrying one formatted log line — see
/// [`spawn_log_relay`].
pub const LOG_LINE_EVENT: &str = "launcher-log-line";

/// File prefix under `~/.beamtalk/` — shared between [`init_logging`]
/// (writes) and [`read_recent_logs`] (reads), so the two can't drift apart.
/// With `Rotation::DAILY` (see [`init_logging`]), the file actually written
/// to on a given day is `{LOG_FILE_NAME}.<date>`, not this bare name — a
/// bare `launcher.log` only exists as a leftover from before rotation
/// shipped (BT-3229). [`list_rotated_log_files`] is what resolves this
/// prefix to the current set of on-disk files.
const LOG_FILE_NAME: &str = "launcher.log";

/// How many daily rollovers [`init_logging`]'s appender keeps
/// (`tracing_appender::rolling::Builder::max_log_files`) before pruning the
/// oldest — one week, the retention window settled on for BT-3229.
///
/// This pruning is `tracing-appender`'s own, and — unlike
/// [`list_rotated_log_files`]'s stricter `YYYY-MM-DD`-shaped match — it
/// deletes anything on disk that merely starts with `LOG_FILE_NAME` once
/// there are more than [`MAX_LOG_FILES`] such names (internal to that
/// crate's `Inner::prune_old_logs`, keyed only on the prefix/suffix passed
/// to `Builder`, no date-shape check when a prefix is set). In practice
/// that's only ever this module's own rotated files plus, for a while, the
/// bare pre-BT-3229 `launcher.log` this module also still reads — but a
/// stray same-prefixed file dropped into `~/.beamtalk/` by hand (a manual
/// `launcher.log.bak`, say) is fair game for that pruning too. Accepted
/// tradeoff of using the crate's built-in rotation rather than reason to
/// avoid it.
const MAX_LOG_FILES: usize = 7;

/// Bounds how much log content [`read_recent_logs`] reads from disk in
/// total, across however many rotated files it takes to fill `limit` lines.
/// Even with daily rotation capping any single file to a day's worth of
/// writes, a very chatty day (or a `limit` requesting more history than one
/// file holds) can still mean reading across several files — this caps the
/// combined read the same way it capped the single unrotated file before
/// BT-3229. Generous enough that a realistic `limit` (the picker UI caps
/// its own display at 2000 lines) almost never needs the "there weren't
/// enough lines in this window" retry this function's implementation does.
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

/// Chmod every file already on disk under `root_dir` that belongs to the
/// launcher log (matched via [`list_rotated_log_files`]'s prefix rule) to
/// `0600` (owner-only) on Unix — matching every other security-relevant
/// file already under `~/.beamtalk/` (`workspace.log`, `cookie`, `vm.args`,
/// `attach.log` — see `beamtalk_desktop_broker::spawn::open_front_log`'s
/// doc comment for the same reasoning). `launcher.log` isn't as sensitive
/// as a workspace's own cookie, but it does record project paths,
/// workspace ids, and ports — enough to match the same posture rather than
/// the world-readable default.
///
/// Runs *after* [`init_logging`] opens today's file rather than
/// pre-creating it at the right mode: `tracing-appender`'s `DAILY`
/// rotation names that file by today's date
/// (`Rotation::date_format`/`join_date`, internal to the crate), so unlike
/// the old `Rotation::NEVER` single fixed path there's no name to
/// pre-create ahead of `tracing-appender`'s own open. Best-effort, and
/// applied to every matching file (not just today's) so a restart
/// self-heals any file `tracing-appender` created at the process umask's
/// default mode on a run before this existed, or before a given day's file
/// happened to get chmod'd.
#[cfg(unix)]
fn ensure_owner_only(root_dir: &Path) {
    use std::os::unix::fs::PermissionsExt;
    for path in list_rotated_log_files(root_dir) {
        let _ = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600));
    }
}

#[cfg(not(unix))]
fn ensure_owner_only(_root_dir: &Path) {}

/// Wraps the rolling file appender so a UTC day boundary crossed *while the
/// process keeps running* also gets [`ensure_owner_only`] applied, not just
/// the file that existed when [`init_logging`] started.
///
/// `tracing-appender`'s own rollover (`RollingFileAppender::write`,
/// internal to that crate) opens the new day's file with plain
/// `OpenOptions::new().append(true).create(true)` — no mode — so a
/// long-lived launcher left running across midnight would otherwise start
/// appending to a file at the process' default umask (typically
/// world-readable) until the next restart happened to call
/// `ensure_owner_only` again. A one-off call from [`init_logging`] alone
/// only covers the file that exists at startup.
///
/// Tracks "day" as a plain count of days since the Unix epoch rather than
/// reproducing `tracing-appender`'s own `Rotation::date_format`/`join_date`
/// logic (internal to that crate) — this only needs to detect *that* the
/// day changed, not compute the exact filename, so it stays independent of
/// that crate's internal date formatting. The check itself
/// (`SystemTime::now` + an `AtomicI64` compare-and-swap) is cheap enough to
/// run on every write; the actual directory sweep only runs on the write
/// that first observes a new day, so a chatty logger doesn't turn this into
/// a per-line filesystem scan.
struct RechmodOnRollover<W> {
    inner: W,
    root_dir: PathBuf,
    last_chmod_day: std::sync::atomic::AtomicI64,
}

impl<W> RechmodOnRollover<W> {
    fn new(inner: W, root_dir: PathBuf) -> Self {
        // Chmod immediately, matching the old pre-creation behavior's
        // "always 0600 before anything can read from the file", then seed
        // `last_chmod_day` so the *first* write doesn't immediately re-sweep
        // the directory it was just handed.
        ensure_owner_only(&root_dir);
        Self {
            inner,
            root_dir,
            last_chmod_day: std::sync::atomic::AtomicI64::new(days_since_unix_epoch()),
        }
    }
}

impl<W: io::Write> io::Write for RechmodOnRollover<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let today = days_since_unix_epoch();
        if self
            .last_chmod_day
            .swap(today, std::sync::atomic::Ordering::Relaxed)
            != today
        {
            ensure_owner_only(&self.root_dir);
        }
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

/// Days since the Unix epoch, UTC — matches the day boundary
/// `tracing_appender::rolling::Rotation::DAILY` rotates on
/// (`OffsetDateTime::now_utc()`, internal to that crate). `0` on a clock
/// that reports before 1970 (never in practice) rather than panicking —
/// [`RechmodOnRollover`] only needs *some* value that changes once per real
/// day, not a historically accurate one.
fn days_since_unix_epoch() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| (d.as_secs() / 86_400) as i64)
        .unwrap_or(0)
}

/// Initialize the launcher's `tracing` subscriber: a file layer rotating
/// daily under `~/.beamtalk/` (append, across restarts, [`MAX_LOG_FILES`]
/// days kept — BT-3229) plus a channel layer whose receiver
/// [`spawn_log_relay`] later drains. Falls back to the OS temp dir if
/// `~/.beamtalk` can't be resolved, and degrades to channel-only logging
/// (no crash) if today's log file itself can't be opened — e.g.
/// `~/.beamtalk` owned by another user from a prior `sudo` invocation, or a
/// full/read-only home directory. This function runs as the very first
/// thing in `main()`, before any window exists; a hard `.expect()` here
/// (which `tracing_appender::rolling::never` — the convenience constructor
/// this used before the BT-3225 fix — has internally) would silently kill
/// the app on launch with no visible error in a packaged build.
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

    let file_appender = tracing_appender::rolling::Builder::new()
        .rotation(tracing_appender::rolling::Rotation::DAILY)
        .filename_prefix(LOG_FILE_NAME)
        .max_log_files(MAX_LOG_FILES)
        .build(&root_dir)
        .map_err(|err| {
            eprintln!(
                "beamtalk-desktop: could not open a {LOG_FILE_NAME} file under {}: {err} — \
                 file logging disabled, in-app log panel still works",
                root_dir.display()
            );
        })
        .ok();

    let (file_layer, file_guard) = match file_appender {
        Some(appender) => {
            // `RechmodOnRollover` chmods today's file immediately (after,
            // not before, opening it: unlike the old `Rotation::NEVER`
            // fixed path, `tracing-appender` names today's file by today's
            // date, so there's nothing to pre-create ahead of its own
            // open — see its doc comment), and again on the first write
            // after any later UTC day boundary the process lives past.
            let appender = RechmodOnRollover::new(appender, root_dir.clone());
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

/// Read the tail of the launcher log — up to `limit` lines, newest content
/// last — to seed the picker UI's log panel with recent history on open,
/// before any live [`LOG_LINE_EVENT`] has fired. Returns an empty vec (not
/// an error) if no log file exists yet — nothing has logged since the log
/// directory was created, not a failure worth surfacing to the UI.
///
/// Since [`init_logging`]'s appender rotates daily (BT-3229), "the log" is
/// no longer one fixed file: this discovers the current file plus however
/// many of the recent rotations it takes to fill `limit` (see
/// [`list_rotated_log_files`] and [`read_recent_lines_from_files`]).
#[must_use]
pub fn read_recent_logs(limit: usize) -> Vec<String> {
    let Ok(root_dir) = beamtalk_workspace::beamtalk_root_dir() else {
        return Vec::new();
    };
    read_recent_lines_from_files(&list_rotated_log_files(&root_dir), limit)
}

/// Every on-disk file belonging to the launcher log under `root_dir`,
/// newest first: each `{LOG_FILE_NAME}.<date>` written by
/// [`init_logging`]'s `Rotation::DAILY` appender, plus a bare
/// `{LOG_FILE_NAME}` if one is still around from before BT-3229 added
/// rotation. `tracing-appender` names *today's* file by today's date
/// rather than a fixed name, so callers that want "the current log" have
/// to discover it this way instead of hardcoding a path.
///
/// Sorts by filename, descending, rather than parsing out and comparing
/// dates: `{LOG_FILE_NAME}.<date>` sorts chronologically because the date
/// suffix is ISO-8601 (`YYYY-MM-DD`, lexicographic == chronological), and
/// the bare `{LOG_FILE_NAME}` — being a strict prefix of every dated name —
/// sorts before all of them ascending, i.e. last descending, which is
/// exactly where a file that predates rotation belongs relative to any
/// dated one.
fn list_rotated_log_files(root_dir: &Path) -> Vec<PathBuf> {
    let Ok(entries) = std::fs::read_dir(root_dir) else {
        return Vec::new();
    };

    let mut files: Vec<(String, PathBuf)> = entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            // `DirEntry::metadata` is an `lstat`, not a `stat` — it does not
            // follow symlinks — so this excludes both directories and
            // symlinks, not just directories. Matters for both callers:
            // [`ensure_owner_only`] must never chmod through a symlink onto
            // an arbitrary target file (relevant mainly under
            // [`init_logging`]'s `std::env::temp_dir()` fallback, a
            // world-writable directory on Unix), and the read side has no
            // business trying to tail a directory.
            if !entry.metadata().ok()?.is_file() {
                return None;
            }
            let name = entry.file_name().to_str()?.to_string();
            let matches = name == LOG_FILE_NAME
                || name
                    .strip_prefix(&format!("{LOG_FILE_NAME}."))
                    .is_some_and(is_daily_rotation_date_suffix);
            matches.then_some((name, entry.path()))
        })
        .collect();

    files.sort_by(|(a, _), (b, _)| b.cmp(a));
    files.into_iter().map(|(_, path)| path).collect()
}

/// True if `suffix` has the exact `YYYY-MM-DD` shape
/// `tracing_appender::rolling::Rotation::DAILY` names files with
/// (`Rotation::date_format`, internal to that crate — `[year]-[month]-[day]`
/// zero-padded). Guards [`list_rotated_log_files`]'s prefix match against
/// picking up an unrelated file that merely starts with
/// `"{LOG_FILE_NAME}."`, e.g. a stray `launcher.log.bak`.
fn is_daily_rotation_date_suffix(suffix: &str) -> bool {
    let bytes = suffix.as_bytes();
    bytes.len() == 10
        && bytes[4] == b'-'
        && bytes[7] == b'-'
        && bytes
            .iter()
            .enumerate()
            .all(|(i, b)| i == 4 || i == 7 || b.is_ascii_digit())
}

/// [`read_recent_logs`]'s actual logic, parameterized over the candidate
/// files (newest first, as returned by [`list_rotated_log_files`]) so it's
/// testable against temp files instead of the real rotated logs under
/// `~/.beamtalk/`.
///
/// Reads each file's tail starting from the newest, stopping once `limit`
/// lines have been collected or the combined read across however many
/// files that took has hit [`MAX_TAIL_READ_BYTES`] — daily rotation caps
/// any single file to roughly a day's writes, so a `limit`-sized window can
/// legitimately need to reach into one or more earlier files to fill.
fn read_recent_lines_from_files(files: &[PathBuf], limit: usize) -> Vec<String> {
    let mut chunks_newest_first: Vec<Vec<String>> = Vec::new();
    let mut lines_so_far = 0usize;
    let mut bytes_budget = MAX_TAIL_READ_BYTES;

    for path in files {
        if lines_so_far >= limit || bytes_budget == 0 {
            break;
        }
        let (lines, bytes_read) = read_tail(path, limit - lines_so_far, bytes_budget);
        bytes_budget = bytes_budget.saturating_sub(bytes_read);
        if lines.is_empty() {
            continue;
        }
        lines_so_far += lines.len();
        chunks_newest_first.push(lines);
    }

    // Each chunk is already in on-disk (chronological) order internally;
    // reversing the chunk order — not each chunk's contents — puts the
    // oldest file's lines first overall.
    chunks_newest_first.into_iter().rev().flatten().collect()
}

/// Read up to `limit` lines from the tail of `path`, bounded to at most
/// `max_bytes` read from disk. Returns the matched lines in on-disk
/// (chronological) order, plus how many bytes were actually read so
/// [`read_recent_lines_from_files`] can track a shared budget across
/// several files. Returns `(vec![], 0)` for a missing or unreadable file.
fn read_tail(path: &Path, limit: usize, max_bytes: u64) -> (Vec<String>, u64) {
    use std::io::{Read, Seek, SeekFrom};

    let Ok(mut file) = std::fs::File::open(path) else {
        return (Vec::new(), 0);
    };
    let Ok(len) = file.metadata().map(|m| m.len()) else {
        return (Vec::new(), 0);
    };

    let seek_start = len.saturating_sub(max_bytes);
    let truncated = seek_start > 0;
    if truncated && file.seek(SeekFrom::Start(seek_start)).is_err() {
        return (Vec::new(), 0);
    }

    let mut buf = Vec::new();
    if file.read_to_end(&mut buf).is_err() {
        return (Vec::new(), 0);
    }
    let bytes_read = buf.len() as u64;

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
    (lines[start..].to_vec(), bytes_read)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_recent_lines_from(path: &Path, limit: usize) -> Vec<String> {
        read_tail(path, limit, MAX_TAIL_READ_BYTES).0
    }

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

    #[test]
    fn list_rotated_log_files_sorts_dated_files_newest_first() {
        let tmp = tempfile::TempDir::new().unwrap();
        for name in [
            "launcher.log.2026-08-20",
            "launcher.log.2026-08-22",
            "launcher.log.2026-08-21",
        ] {
            std::fs::write(tmp.path().join(name), "").unwrap();
        }

        let files = list_rotated_log_files(tmp.path());
        let names: Vec<_> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_str().unwrap())
            .collect();
        assert_eq!(
            names,
            vec![
                "launcher.log.2026-08-22",
                "launcher.log.2026-08-21",
                "launcher.log.2026-08-20",
            ]
        );
    }

    #[test]
    fn list_rotated_log_files_puts_the_undated_legacy_file_last() {
        let tmp = tempfile::TempDir::new().unwrap();
        std::fs::write(tmp.path().join("launcher.log"), "").unwrap();
        std::fs::write(tmp.path().join("launcher.log.2026-08-22"), "").unwrap();
        // Unrelated file sharing a prefix character-wise but not on a `.`
        // boundary — must not be picked up.
        std::fs::write(tmp.path().join("launcher.log.old.bak"), "").unwrap();

        let files = list_rotated_log_files(tmp.path());
        let names: Vec<_> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_str().unwrap())
            .collect();
        assert_eq!(names, vec!["launcher.log.2026-08-22", "launcher.log"]);
    }

    #[test]
    fn list_rotated_log_files_returns_empty_for_a_missing_directory() {
        let tmp = tempfile::TempDir::new().unwrap();
        assert_eq!(
            list_rotated_log_files(&tmp.path().join("does-not-exist")),
            Vec::<PathBuf>::new()
        );
    }

    #[test]
    fn read_recent_lines_from_files_returns_empty_for_no_files() {
        assert_eq!(read_recent_lines_from_files(&[], 100), Vec::<String>::new());
    }

    #[test]
    fn read_recent_lines_from_files_reads_a_single_newest_file_first() {
        let tmp = tempfile::TempDir::new().unwrap();
        let newer = tmp.path().join("launcher.log.2026-08-22");
        let older = tmp.path().join("launcher.log.2026-08-21");
        std::fs::write(&newer, "c\nd\n").unwrap();
        std::fs::write(&older, "a\nb\n").unwrap();

        // `limit: 2` is fully satisfied by the newest file alone — the
        // older file must not be touched at all.
        assert_eq!(
            read_recent_lines_from_files(&[newer.clone(), older.clone()], 2),
            vec!["c", "d"]
        );
    }

    #[test]
    fn read_recent_lines_from_files_spans_into_older_files_to_fill_the_limit() {
        let tmp = tempfile::TempDir::new().unwrap();
        let newer = tmp.path().join("launcher.log.2026-08-22");
        let older = tmp.path().join("launcher.log.2026-08-21");
        std::fs::write(&newer, "c\nd\n").unwrap();
        std::fs::write(&older, "a\nb\n").unwrap();

        // `limit: 3` can't be filled by the 2-line newest file alone, so
        // this must reach into the older file for one more line — and the
        // overall result stays chronological (oldest first).
        assert_eq!(
            read_recent_lines_from_files(&[newer, older], 3),
            vec!["b", "c", "d"]
        );
    }

    #[test]
    fn read_recent_lines_from_files_skips_an_unreadable_file_and_keeps_going() {
        let tmp = tempfile::TempDir::new().unwrap();
        let missing = tmp.path().join("launcher.log.2026-08-22");
        let older = tmp.path().join("launcher.log.2026-08-21");
        std::fs::write(&older, "a\nb\n").unwrap();

        assert_eq!(
            read_recent_lines_from_files(&[missing, older], 100),
            vec!["a", "b"]
        );
    }

    #[test]
    fn list_rotated_log_files_finds_a_real_tracing_appender_daily_file() {
        // End-to-end guard against a `tracing-appender` version bump
        // silently changing `Rotation::DAILY`'s file-naming contract
        // (`join_date`/`date_format`, internal to that crate) out from
        // under `list_rotated_log_files`'s own prefix/date-shape matching
        // — every other test in this module exercises
        // `list_rotated_log_files` against hand-written filenames, which
        // would stay green even if the two diverged.
        use std::io::Write as _;

        let tmp = tempfile::TempDir::new().unwrap();
        let mut appender = tracing_appender::rolling::Builder::new()
            .rotation(tracing_appender::rolling::Rotation::DAILY)
            .filename_prefix(LOG_FILE_NAME)
            .build(tmp.path())
            .unwrap();
        appender.write_all(b"hello\n").unwrap();
        appender.flush().unwrap();

        let files = list_rotated_log_files(tmp.path());
        assert_eq!(files.len(), 1, "expected exactly one file, found {files:?}");
        let name = files[0].file_name().unwrap().to_str().unwrap();
        assert!(
            name.starts_with("launcher.log."),
            "unexpected file name from a real Builder: {name}"
        );
        assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "hello\n");
    }

    #[test]
    fn list_rotated_log_files_excludes_a_same_prefixed_directory() {
        let tmp = tempfile::TempDir::new().unwrap();
        std::fs::create_dir(tmp.path().join("launcher.log.2026-08-22")).unwrap();
        std::fs::write(tmp.path().join("launcher.log.2026-08-21"), "").unwrap();

        let files = list_rotated_log_files(tmp.path());
        let names: Vec<_> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_str().unwrap())
            .collect();
        assert_eq!(names, vec!["launcher.log.2026-08-21"]);
    }

    #[cfg(unix)]
    #[test]
    fn list_rotated_log_files_excludes_a_same_prefixed_symlink() {
        let tmp = tempfile::TempDir::new().unwrap();
        let target = tmp.path().join("real-target.log");
        std::fs::write(&target, "secret").unwrap();
        std::os::unix::fs::symlink(&target, tmp.path().join("launcher.log.2026-08-22")).unwrap();
        std::fs::write(tmp.path().join("launcher.log.2026-08-21"), "").unwrap();

        let files = list_rotated_log_files(tmp.path());
        let names: Vec<_> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_str().unwrap())
            .collect();
        assert_eq!(
            names,
            vec!["launcher.log.2026-08-21"],
            "a symlink must not be chmod'd through to an arbitrary target"
        );
    }

    #[cfg(unix)]
    #[test]
    fn ensure_owner_only_chmods_matching_files_to_0600() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("launcher.log.2026-08-22");
        std::fs::write(&path, "").unwrap();
        // Start from a permissive mode so the assertion below proves the
        // call actually changed something, not that the file happened to
        // already be 0600 (e.g. from a restrictive umask).
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();

        ensure_owner_only(tmp.path());

        let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o600);
    }

    #[cfg(unix)]
    #[test]
    fn rechmod_on_rollover_rechmods_on_a_simulated_day_change() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("launcher.log.2026-08-22");
        std::fs::write(&path, "").unwrap();

        let mut wrapper = RechmodOnRollover::new(Vec::new(), tmp.path().to_path_buf());
        // Loosen the mode, then force `last_chmod_day` to a stale value so
        // the write below looks like it crossed a day boundary — isolating
        // the rollover path from the constructor's own initial chmod.
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();
        wrapper
            .last_chmod_day
            .store(0, std::sync::atomic::Ordering::Relaxed);

        std::io::Write::write_all(&mut wrapper, b"line\n").unwrap();

        let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o600, "a write crossing a day boundary must re-chmod");
    }

    #[cfg(unix)]
    #[test]
    fn rechmod_on_rollover_skips_the_sweep_within_the_same_day() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("launcher.log.2026-08-22");
        std::fs::write(&path, "").unwrap();

        let mut wrapper = RechmodOnRollover::new(Vec::new(), tmp.path().to_path_buf());
        // Loosen the mode *after* construction's own initial chmod, and
        // leave `last_chmod_day` alone (still "today") — the write below
        // must see no day change and skip the sweep, leaving the loosened
        // mode in place.
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();

        std::io::Write::write_all(&mut wrapper, b"line\n").unwrap();

        let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(
            mode, 0o644,
            "a same-day write must not re-sweep the directory"
        );
    }
}
