// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Orphan reaping (ADR 0097 Broker §4 / Implementation §6g).
//!
//! If the broker dies uncleanly (`SIGKILL`, logout, crash), the fronts it
//! spawned are orphaned — cookie-bearing BEAM processes with nothing left to
//! detach them. The spike (`docs/research/desktop-shell-spike.md`, criterion
//! (g)) prototyped and validated **PID-file sweep** over process-group kill
//! or a parent-death watch (`PR_SET_PDEATHSIG`): the only one of the three
//! portable to Windows, which the ADR's packaging phase targets alongside
//! macOS/Linux.
//!
//! The spike also flagged a real gap in that mechanism, to harden here: a
//! naive PID-file sweep kills whatever process currently holds the recorded
//! PID with no check that it's still the *same* process — if the orphan
//! already died and the OS recycled its PID before the next sweep, sweeping
//! kills an unrelated process. This module closes that gap by recording each
//! front's process start time at spawn time (the same technique
//! `beamtalk-cli`'s `NodeInfo.start_time` already uses for its own PID-reuse
//! detection) and refusing to signal a PID whose *current* start time
//! doesn't match what was recorded. [`read_start_time`] was inert on Windows
//! until BT-3046 (`None` unconditionally, which `classify_record` treats as
//! "unknown, trust liveness" — every sweep matched, closing nothing); it now
//! reads a real value there via `GetProcessTimes`.
//!
//! **What "the recorded PID" means on Windows** (BT-2988): the caller
//! persists `Child::id()` after `spawn_front`, which per [`crate::winjob`]'s
//! doc comment is `cmd.exe`'s PID, not `erl.exe`'s (`.bat` launches can only
//! run via a console-subsystem wrapper). A `Reap` here terminates that
//! `cmd.exe` — usually enough on its own, since Windows tears down a process
//! whose parent has already exited, though not synchronously or
//! unconditionally the way [`crate::winjob::JobHandle`]'s kill-on-close
//! guarantees the whole tree dies. This module's PID-reuse hardening is
//! still real and worth having (it stops a *different*, unrelated process
//! from being killed), but a stale record reaching this sweep at all should
//! be rare in the crash scenario it exists for: `JobHandle`'s kill-on-close
//! already tears down the entire `cmd.exe`/`erl.exe` tree the moment a live
//! broker dies, before any future broker's sweep would ever run. This
//! module is the fallback net for what that misses — see `crate::winjob`'s
//! doc comment for its own residual (assign-after-spawn) race.
//!
//! **DDD Context:** Desktop Shell

use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard, OnceLock, PoisonError};
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::error::Result;

/// Serializes every read-modify-write or delete of a [`FrontRecord`] file
/// ([`save_record`], [`update_record_node_name`], [`remove_record`]) so two
/// of those operations racing the same `(workspace_id, port)` key can never
/// interleave (BT-3062). Without this, `update_record_node_name`'s
/// read-check-write had a window in which a concurrent `remove_record` (a
/// racing `detach`/`quit`/failed-attach cleanup, per `kill_and_untrack`)
/// could delete the file between the read and the write — the write would
/// then silently *recreate* it, resurrecting a record for a front this
/// broker already killed. See `update_record_node_name`'s doc comment for
/// the full race description this closes.
///
/// One process-wide lock, not a per-key table: every operation it guards is
/// a single fast file read/write, and record mutations across *different*
/// keys are rare enough (one attach/detach at a time is the common case)
/// that briefly serializing unrelated keys is a better trade than a per-key
/// lock table's unbounded growth over a long-lived broker process's
/// lifetime. Cross-process contention (e.g. this broker racing a *second*
/// broker instance) is out of scope — ADR 0097 assumes one broker per user
/// session — so a plain in-process [`Mutex`], not a cross-process file lock,
/// is sufficient to close the specific same-process race this exists for.
fn record_ops_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

/// Lock [`record_ops_lock`], recovering from a poisoned mutex rather than
/// propagating the panic to every future record operation for the rest of
/// the process's life — matches `desktop/src-tauri/src/commands.rs`'s own
/// `locked()` helper's reasoning for `state.attach`/`state.children`: a
/// panic mid-mutation doesn't guarantee the record file itself was left
/// inconsistent, and this is the one lock standing between a stuck broker
/// and every future attach/detach silently failing to update its bookkeeping.
fn locked_record_ops() -> MutexGuard<'static, ()> {
    record_ops_lock()
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
}

/// Bookkeeping for one spawned front, persisted so a future broker process
/// (after a crash) can find and reap it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FrontRecord {
    pub workspace_id: String,
    pub port: u16,
    pub pid: u32,
    pub node_name: String,
    /// Process start time at record time (Linux: `/proc/{pid}/stat` field 22
    /// clock ticks since boot; `None` on platforms without a cheap
    /// equivalent, or if it couldn't be read). Used to detect PID reuse —
    /// see the module docs.
    pub start_time: Option<u64>,
}

/// Directory the broker keeps its own state in — deliberately separate from
/// `~/.beamtalk/workspaces/` (owned by the Rust CLI; the spike's `broker.sh`
/// established this same separation with its own `STATE_DIR`).
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn state_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| {
        crate::error::BrokerError::Io(std::io::Error::new(
            ErrorKind::NotFound,
            "could not determine home directory",
        ))
    })?;
    Ok(home.join(".beamtalk").join("desktop-broker"))
}

fn record_path(dir: &Path, workspace_id: &str, port: u16) -> PathBuf {
    dir.join(format!("{workspace_id}.{port}.json"))
}

/// Persist a front record so a future broker start can find it if this
/// broker dies before detaching it cleanly.
///
/// # Errors
///
/// Returns an error if `dir` can't be created or the record can't be written.
pub fn save_record(dir: &Path, record: &FrontRecord) -> Result<()> {
    std::fs::create_dir_all(dir)?;
    let path = record_path(dir, &record.workspace_id, record.port);
    let json = serde_json::to_string_pretty(record)?;
    let _guard = locked_record_ops();
    std::fs::write(path, json)?;
    Ok(())
}

/// Correct a front record's `node_name` after the fact (BT-3045) — used on
/// the Windows attach path, where [`save_record`]'s initial write (via
/// `desktop/src-tauri/src/commands.rs`'s `persist_front_record`) cannot yet
/// know the front's real epmd registration name (`sname::predict_node_name`'s
/// pid guess is wrong there — see that module's doc comment — and
/// `sname::resolve_registered_node_name`'s epmd query only has an answer once
/// the front has actually distributed, which happens lazily on the first
/// `/readiness` call, after the initial record write). Once readiness
/// confirms `Ready` and a real epmd-resolved name is available, this
/// overwrites the placeholder in place. The caller
/// (`desktop/src-tauri/src/commands.rs`'s `attach_and_open_window`) runs this
/// on a detached background thread rather than inline (`node_name` has no
/// reader today, so nothing should block on it) — which widens, not just
/// tolerates, the window for the race `expected_pid` exists to close below.
///
/// Compare-and-swap on `expected_pid`, not a blind overwrite: without it, a
/// record removed by a concurrent `detach`/`quit`/failed-attach cleanup
/// (`kill_and_untrack`) between this function's read and write would be
/// silently *recreated* by the write — describing a process this broker
/// already killed as if it were still the live front. Checking the pid also
/// guards the (currently unlikely, since ports are freshly OS-allocated per
/// spawn — see `crate::port`'s module docs — but not impossible) case where
/// the same `(workspace_id, port)` key got reused by an entirely different,
/// newer front in between: this must never stamp a stale epmd-resolved name
/// onto a record that isn't the one this call was resolving it for.
///
/// **The resurrection window itself (BT-3062)** — the compare-and-swap above
/// only ever protected against a *different* front's record being mutated;
/// it did nothing about the file being deleted by a racing `remove_record`
/// strictly between this function's read and its write, which would recreate
/// the very file that call just deleted. [`locked_record_ops`] now wraps the
/// whole read-check-write below in the same process-wide lock
/// [`remove_record`] takes around its own delete, so the two can never
/// interleave: either the delete completes first (this call then reads
/// `NotFound` and no-ops, per the paragraph below) or this call's write
/// completes first (and the delete, running after, removes the corrected
/// file as intended) — never the read-delete-write ordering that used to
/// resurrect it.
///
/// No-op (not an error) if the record no longer exists, or if it exists but
/// its `pid` no longer matches `expected_pid` — either way there's nothing
/// left for *this* call to correct, not a failure of this function's own job.
///
/// # Errors
///
/// Returns an error if the record exists but can't be read/parsed/rewritten.
pub fn update_record_node_name(
    dir: &Path,
    workspace_id: &str,
    port: u16,
    expected_pid: u32,
    node_name: &str,
) -> Result<()> {
    let path = record_path(dir, workspace_id, port);
    let _guard = locked_record_ops();
    let content = match std::fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) if e.kind() == ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e.into()),
    };
    let mut record: FrontRecord = serde_json::from_str(&content)?;
    if record.pid != expected_pid {
        return Ok(());
    }
    node_name.clone_into(&mut record.node_name);
    let json = serde_json::to_string_pretty(&record)?;
    std::fs::write(path, json)?;
    Ok(())
}

/// Remove a front record — called on clean detach, so a graceful stop
/// doesn't leave a stale record for the next sweep to trip over. Also
/// removes that front's per-front `RELEASE_TMP` directory on Windows
/// (BT-3046 adversarial-review follow-up): that directory holds the boot's
/// resolved `sys.config`, embedding `SECRET_KEY_BASE` and the workspace
/// cookie, which are meant to be ephemeral per-boot secrets — left behind,
/// they'd accumulate one live-secret directory per spawn/retry, forever.
/// Every path that clears a front record (this sweep, and both detach call
/// sites in `desktop/src-tauri/src/commands.rs`) goes through here, so this
/// one hook covers all of them.
///
/// The `RELEASE_TMP` removal runs on a detached background thread (BT-3059)
/// rather than inline: `remove_release_tmp_dir_with_retry` can legitimately
/// retry for up to ~1s absorbing `JobHandle`'s asynchronous kill-on-close,
/// but this function sits on latency-sensitive *synchronous* callers — the
/// Tauri `.setup()` hook's `sweep` runs before the picker window is created
/// (up to `N`×1s of app-startup stall after a crash left `N` workspaces
/// attached), and `detach_internal`/`detach_all` call this inline per
/// workspace on "Quit" (up to `N`×1s of added quit latency). This cleanup is
/// already best-effort — it only ever logs a `tracing::warn!` and moves on,
/// never a hard error surfaced to the caller — so nothing here depended on
/// it finishing before this function returns; the on-disk record-file
/// removal below (the part sweep/detach actually need to observe
/// synchronously — see [`load_all_records`]) is unaffected and still
/// happens inline.
///
/// The record-file delete below still takes [`locked_record_ops`] (BT-3062);
/// the backgrounded `RELEASE_TMP` removal deliberately does not, matching
/// its previous inline reasoning — it doesn't touch the record file itself,
/// so there's no reason to hold up an unrelated [`update_record_node_name`]
/// call for up to ~1s over a resource it never touches.
///
/// # Errors
///
/// Returns an error if the record file exists but can't be removed
/// (anything other than "already gone").
pub fn remove_record(dir: &Path, workspace_id: &str, port: u16) -> Result<()> {
    #[cfg(windows)]
    {
        let workspace_id = workspace_id.to_string();
        std::thread::spawn(move || {
            remove_release_tmp_dir_with_retry(&workspace_id, port);
        });
    }

    let path = record_path(dir, workspace_id, port);
    let _guard = locked_record_ops();
    match std::fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e.into()),
    }
}

/// How long to retry [`remove_dir_all`] on the per-front `RELEASE_TMP`
/// directory before giving up and logging (10 attempts, 100ms apart — up to
/// ~1s total).
#[cfg(windows)]
const RELEASE_TMP_REMOVE_ATTEMPTS: u32 = 10;
#[cfg(windows)]
const RELEASE_TMP_REMOVE_RETRY_DELAY: Duration = Duration::from_millis(100);

/// Best-effort removal of a front's `RELEASE_TMP` directory, retrying briefly
/// on failure (BT-3046 adversarial-review follow-up, second pass).
///
/// The caller (`remove_record`) spawns this on a detached background thread
/// (BT-3059) immediately after the front's `SpawnedFront` is dropped, which
/// closes its `JobHandle` (`crate::winjob`) —
/// `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` then kills `erl.exe` (the process
/// actually holding files open under `RELEASE_TMP`; `Child::kill()`/`.wait()`
/// upstream only ever touched `cmd.exe`, see `winjob.rs`'s doc comment), but
/// that kill-on-close is fire-and-forget — there is no handle to `erl.exe`
/// available here to wait on directly. A single immediate `remove_dir_all`
/// attempt can therefore plausibly race `erl.exe` still exiting and lose with
/// a sharing violation, on an entirely ordinary detach. Retrying briefly
/// absorbs that race without needing to discover `erl.exe`'s pid and poll it
/// separately — backgrounding this (rather than the retry loop itself) is
/// what keeps that up-to-~1s cost off `remove_record`'s synchronous callers.
#[cfg(windows)]
fn remove_release_tmp_dir_with_retry(workspace_id: &str, port: u16) {
    let release_tmp = crate::spawn::release_tmp_dir(workspace_id, port);
    let mut last_err = None;
    for attempt in 0..RELEASE_TMP_REMOVE_ATTEMPTS {
        match std::fs::remove_dir_all(&release_tmp) {
            Ok(()) => return,
            Err(e) if e.kind() == ErrorKind::NotFound => return,
            Err(e) => {
                last_err = Some(e);
                if attempt + 1 < RELEASE_TMP_REMOVE_ATTEMPTS {
                    std::thread::sleep(RELEASE_TMP_REMOVE_RETRY_DELAY);
                }
            }
        }
    }
    if let Some(e) = last_err {
        tracing::warn!(
            workspace = %workspace_id,
            port,
            path = %release_tmp.display(),
            error = %e,
            "remove_record: failed to remove per-front RELEASE_TMP directory after retrying"
        );
    }
}

/// Load every front record currently on disk. Corrupt/unparsable record
/// files are skipped (logged) rather than failing the whole sweep — one bad
/// file must not hide every other orphan.
///
/// # Errors
///
/// Returns an error if listing `dir` fails for a reason other than "does
/// not exist".
pub fn load_all_records(dir: &Path) -> Result<Vec<FrontRecord>> {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) if e.kind() == ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e.into()),
    };
    let mut records = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        match serde_json::from_str::<FrontRecord>(&content) {
            Ok(record) => records.push(record),
            Err(e) => {
                tracing::warn!(path = %path.display(), error = %e, "skipping unparsable front record");
            }
        }
    }
    Ok(records)
}

/// Disposition of one record, decided *before* any signal is sent — pure so
/// it's unit-testable without spawning real processes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    /// Process alive and confirmed to be the same one that was recorded —
    /// safe to reap.
    Reap,
    /// No live process at that PID — just a stale record to clear.
    ClearStale,
    /// A process is alive at that PID, but its start time doesn't match the
    /// recorded one — the OS recycled the PID for something unrelated.
    /// Clear the stale record but do **not** signal the process.
    SkipPidReused,
}

/// Decide what to do with a record, given what's actually true about its PID
/// right now (`is_alive`, `actual_start_time` — obtained via
/// [`is_process_alive`]/[`read_start_time`], injected here so the decision
/// logic itself needs no process I/O to test).
///
/// When either the recorded or the observed start time is unavailable (a
/// platform without the cheap read, or an old record predating this field),
/// this falls back to trusting liveness alone — the same "best effort"
/// stance `beamtalk-cli`'s own `NodeInfo.start_time` handling takes for
/// backward compatibility. This means PID-reuse detection is
/// best-effort, not a hard guarantee, on platforms/records where start time
/// isn't available — matching the spike's own framing of this as hardening
/// a known gap, not eliminating it outright.
#[must_use]
pub fn classify_record(
    record: &FrontRecord,
    is_alive: bool,
    actual_start_time: Option<u64>,
) -> Disposition {
    if !is_alive {
        return Disposition::ClearStale;
    }
    if start_time_matches(record.start_time, actual_start_time) {
        Disposition::Reap
    } else {
        Disposition::SkipPidReused
    }
}

/// Pure comparison shared by [`classify_record`] and `terminate_process`'s
/// pre-`SIGKILL` re-check: is `actual` consistent with `expected`? `None` on
/// either side means "unknown" and is treated as a match — the same
/// best-effort stance `beamtalk-cli`'s own `NodeInfo.start_time` handling
/// takes when start time isn't available (old record, unsupported platform).
fn start_time_matches(expected: Option<u64>, actual: Option<u64>) -> bool {
    match (expected, actual) {
        (Some(e), Some(a)) => e == a,
        _ => true,
    }
}

/// Result of a full sweep.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SweepReport {
    pub reaped: Vec<FrontRecord>,
    pub stale_cleared: Vec<FrontRecord>,
    pub pid_reused_skipped: Vec<FrontRecord>,
}

/// Grace period between `SIGTERM` and a forced `SIGKILL` (Unix only —
/// Windows' `TerminateProcess` has no graceful-signal equivalent to wait on).
#[cfg(unix)]
const TERMINATE_GRACE: Duration = Duration::from_millis(300);

/// Sweep `dir` for orphaned fronts, killing (with a `SIGTERM` → `SIGKILL`
/// grace period) any record that [`classify_record`] says to [`Disposition::Reap`],
/// and clearing every record's file regardless of disposition (a
/// `SkipPidReused` record is just as stale as a dead one from this broker's
/// point of view — the PID it named is no longer "our" front either way).
///
/// Intended to run once at broker startup — a full broker restart means any
/// prior in-memory "what's attached" state is gone, so there is no
/// legitimate reason for a pre-existing front to still be tracked.
///
/// # Errors
///
/// Returns an error if loading or clearing records in `dir` fails.
pub fn sweep(dir: &Path) -> Result<SweepReport> {
    let mut report = SweepReport::default();
    for record in load_all_records(dir)? {
        let alive = is_process_alive(record.pid);
        let actual_start_time = if alive {
            read_start_time(record.pid)
        } else {
            None
        };
        match classify_record(&record, alive, actual_start_time) {
            Disposition::Reap => {
                tracing::info!(
                    workspace = %record.workspace_id,
                    pid = record.pid,
                    "sweep: reaping orphaned front"
                );
                terminate_process(record.pid, record.start_time);
                report.reaped.push(record.clone());
            }
            Disposition::ClearStale => {
                report.stale_cleared.push(record.clone());
            }
            Disposition::SkipPidReused => {
                tracing::warn!(
                    workspace = %record.workspace_id,
                    pid = record.pid,
                    "sweep: PID reused by an unrelated process — clearing the stale \
                     record without signaling it"
                );
                report.pid_reused_skipped.push(record.clone());
            }
        }
        // `remove_record` below backgrounds its Windows RELEASE_TMP retry
        // removal onto its own thread (BT-3059) rather than retrying inline,
        // so it no longer delays this loop — but the retrying itself (BT-3046
        // adversarial-review follow-up) still matters: it absorbs
        // `TerminateProcess`'s documented asynchronicity, since a `Reap`
        // disposition's target may not have fully exited (and released files
        // it held open under RELEASE_TMP) by the time that call returns
        // above.
        remove_record(dir, &record.workspace_id, record.port)?;
    }
    Ok(report)
}

/// `SIGTERM`, wait [`TERMINATE_GRACE`], then `SIGKILL` if still alive (Unix).
/// `TerminateProcess` (Windows, no graceful-signal equivalent for an
/// arbitrary external process). Best-effort: does not surface an error for
/// "already gone by the time we got here" — that is a successful outcome for
/// a reap (the goal, the process not running, is achieved either way).
///
/// `expected_start_time` re-closes the exact PID-reuse gap [`classify_record`]
/// exists to close, on the kill path itself: `sweep` only calls this after
/// `classify_record` has already confirmed the PID is alive and (best-effort)
/// still the recorded process — but that confirmation is a snapshot from
/// *before* the kill signal. On Unix, the process could exit and the OS
/// could recycle its PID for something unrelated during [`TERMINATE_GRACE`];
/// on Windows, that same window exists (just smaller, with no grace-period
/// sleep) between `classify_record`'s check and `OpenProcess` here. Both
/// arms re-verify via [`still_same_process`] immediately before the actual
/// kill call, closing the window rather than just narrowing it.
fn terminate_process(pid: u32, expected_start_time: Option<u64>) {
    #[cfg(unix)]
    {
        if let Ok(pid_i) = i32::try_from(pid) {
            // SAFETY: kill(2) with a valid pid and standard signal number.
            unsafe { libc::kill(pid_i, libc::SIGTERM) };
            std::thread::sleep(TERMINATE_GRACE);
            if is_process_alive(pid) && still_same_process(pid, expected_start_time) {
                // SAFETY: kill(2) with a valid pid and standard signal number.
                unsafe { libc::kill(pid_i, libc::SIGKILL) };
            }
        }
    }
    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE};
        use windows_sys::Win32::System::Threading::{
            OpenProcess, PROCESS_TERMINATE, TerminateProcess,
        };
        // No grace-window sleep on this path (a single forceful
        // TerminateProcess, no graceful-signal equivalent to wait out first),
        // so the PID-reuse window this re-check closes is inherently much
        // smaller than the Unix arm's `TERMINATE_GRACE` — but now that
        // `read_start_time` is real on Windows too (BT-3046), re-verifying
        // costs nothing and closes it anyway rather than merely accepting it
        // as "small enough", matching the Unix arm's own belt-and-suspenders
        // stance.
        // SAFETY: Windows API call with documented parameters; handle is
        // checked for null before use and closed afterward.
        unsafe {
            let handle = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
            if !handle.is_null() {
                if still_same_process(pid, expected_start_time) {
                    TerminateProcess(handle, 1);
                }
                CloseHandle(handle);
            }
        }
    }
}

/// Re-read `pid`'s *current* start time and check it against
/// `expected_start_time` — the same [`start_time_matches`] comparison
/// [`classify_record`] uses, applied again immediately before killing to
/// close the PID-reuse window between `classify_record`'s check and the
/// actual kill call (see `terminate_process`'s doc comment). Available
/// wherever [`read_start_time`] returns a real value (Linux, Windows).
#[cfg(any(unix, windows))]
fn still_same_process(pid: u32, expected_start_time: Option<u64>) -> bool {
    start_time_matches(expected_start_time, read_start_time(pid))
}

/// Check whether a process is alive by PID.
///
/// Unix: `kill(pid, 0)` — signal 0 tests existence without signaling.
/// Windows: `OpenProcess` + `GetExitCodeProcess`.
#[must_use]
pub fn is_process_alive(pid: u32) -> bool {
    #[cfg(unix)]
    {
        let Ok(pid_i) = i32::try_from(pid) else {
            return false;
        };
        // SAFETY: kill(2) with signal 0 is a standard existence check.
        let ret = unsafe { libc::kill(pid_i, 0) };
        if ret == 0 {
            return true;
        }
        // EPERM means the process exists but we lack permission to signal
        // it — it is still alive.
        std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE, STILL_ACTIVE};
        use windows_sys::Win32::System::Threading::{
            GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };
        // SAFETY: Windows API call with documented parameters.
        let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
        if handle.is_null() {
            return false;
        }
        let mut exit_code: u32 = 0;
        // SAFETY: handle is valid, exit_code is a local variable.
        let ok = unsafe { GetExitCodeProcess(handle, &raw mut exit_code) };
        // SAFETY: handle is valid, obtained from OpenProcess above.
        unsafe { CloseHandle(handle) };
        ok != FALSE && exit_code == STILL_ACTIVE as u32
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = pid;
        false
    }
}

/// Read process start time from `/proc/{pid}/stat` (field 22 per proc(5)).
/// Linux-only: `/proc` doesn't exist on macOS/BSD/Windows, matching
/// `beamtalk-cli`'s own `read_proc_start_time` (this crate cannot reuse that
/// one directly — it is CLI-internal — so this is the same small technique
/// duplicated, not a novel mechanism).
#[cfg(target_os = "linux")]
#[must_use]
pub fn read_start_time(pid: u32) -> Option<u64> {
    let stat_path = format!("/proc/{pid}/stat");
    let content = std::fs::read_to_string(stat_path).ok()?;
    let after_comm = content.rsplit_once(')')?.1;
    let starttime_str = after_comm.split_whitespace().nth(19)?;
    starttime_str.parse::<u64>().ok()
}

/// Read process creation time via `GetProcessTimes` (BT-3046). Mirrors the
/// Linux `/proc` read above: without this, `classify_record`'s "unknown
/// start time falls back to trusting liveness" stance (see its doc comment)
/// meant the PID-reuse guard was **inert** on Windows — a stale on-disk
/// [`FrontRecord`] always classified as [`Disposition::Reap`], so a recycled
/// PID (Windows reuses them more aggressively than Linux) would get an
/// unconditional `TerminateProcess` against whatever unrelated process now
/// holds it.
///
/// Returns `None` if the process can't be opened (already gone, or a
/// permissions issue) or the call otherwise fails — same "unknown, fall back
/// to liveness" contract the Linux arm has.
#[cfg(windows)]
#[must_use]
pub fn read_start_time(pid: u32) -> Option<u64> {
    use windows_sys::Win32::Foundation::{CloseHandle, FALSE, FILETIME};
    use windows_sys::Win32::System::Threading::{
        GetProcessTimes, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    // SAFETY: Windows API call with documented parameters; handle is checked
    // for null before use and closed afterward. PROCESS_QUERY_LIMITED_INFORMATION
    // is sufficient per GetProcessTimes' own documented access-right
    // requirement (the same right `is_process_alive` above already requests).
    let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
    if handle.is_null() {
        return None;
    }

    let mut creation = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut exit = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut kernel = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut user = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    // SAFETY: handle is valid (checked above); the four out-params are valid,
    // local, correctly-typed FILETIME buffers for the duration of this call.
    let ok = unsafe {
        GetProcessTimes(
            handle,
            &raw mut creation,
            &raw mut exit,
            &raw mut kernel,
            &raw mut user,
        )
    };
    // SAFETY: handle is valid, obtained from OpenProcess above.
    unsafe { CloseHandle(handle) };
    if ok == FALSE {
        return None;
    }
    Some((u64::from(creation.dwHighDateTime) << 32) | u64::from(creation.dwLowDateTime))
}

#[cfg(not(any(target_os = "linux", windows)))]
#[must_use]
pub fn read_start_time(_pid: u32) -> Option<u64> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    // `TERMINATE_GRACE` (crate-level import above) only exists on
    // `cfg(unix)`; this module's own `#[cfg(windows)]` tests below need
    // `Duration` too (BT-3046).
    #[cfg(windows)]
    use std::time::Duration;

    fn record(pid: u32, start_time: Option<u64>) -> FrontRecord {
        FrontRecord {
            workspace_id: "abc123".to_string(),
            port: 4567,
            pid,
            node_name: "bt_attach_abc123_4242@localhost".to_string(),
            start_time,
        }
    }

    // ── pure classification ─────────────────────────────────────────────

    #[test]
    fn dead_process_is_cleared_as_stale() {
        let disposition = classify_record(&record(1234, Some(100)), false, None);
        assert_eq!(disposition, Disposition::ClearStale);
    }

    #[test]
    fn alive_matching_start_time_is_reaped() {
        let disposition = classify_record(&record(1234, Some(100)), true, Some(100));
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_mismatched_start_time_is_pid_reused_not_reaped() {
        // The exact gap the spike flagged: PID recycled by an unrelated
        // process between the orphan dying and the sweep running.
        let disposition = classify_record(&record(1234, Some(100)), true, Some(999));
        assert_eq!(disposition, Disposition::SkipPidReused);
    }

    #[test]
    fn alive_with_no_recorded_start_time_falls_back_to_reap() {
        // Backward compat: an older record with no start_time field, or a
        // platform without the cheap read — best-effort trusts liveness.
        let disposition = classify_record(&record(1234, None), true, Some(100));
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_with_no_observed_start_time_falls_back_to_reap() {
        let disposition = classify_record(&record(1234, Some(100)), true, None);
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_matching_zero_start_times_is_reaped() {
        // Boundary: 0 is a valid (if unlikely) start-time tick count, must
        // not be confused with "unavailable" (None).
        let disposition = classify_record(&record(1234, Some(0)), true, Some(0));
        assert_eq!(disposition, Disposition::Reap);
    }

    // ── record persistence round-trip ───────────────────────────────────

    #[test]
    fn save_and_load_round_trips_a_record() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(loaded, vec![rec]);
    }

    #[test]
    fn remove_record_clears_it() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();
        remove_record(tmp.path(), &rec.workspace_id, rec.port).unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert!(loaded.is_empty());
    }

    #[test]
    fn remove_record_is_idempotent_when_already_gone() {
        let tmp = tempfile::TempDir::new().unwrap();
        // Removing a record that was never saved must not error.
        remove_record(tmp.path(), "nonexistent", 1).unwrap();
    }

    // ── update_record_node_name (BT-3045) ───────────────────────────────

    #[test]
    fn update_record_node_name_corrects_the_persisted_value() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();

        update_record_node_name(
            tmp.path(),
            &rec.workspace_id,
            rec.port,
            rec.pid,
            "bt_attach_abc123_9999@localhost",
        )
        .unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].node_name, "bt_attach_abc123_9999@localhost");
        // Every other field must survive untouched — this only corrects
        // node_name, not a full re-save of caller-supplied data.
        assert_eq!(loaded[0].pid, rec.pid);
        assert_eq!(loaded[0].port, rec.port);
        assert_eq!(loaded[0].workspace_id, rec.workspace_id);
        assert_eq!(loaded[0].start_time, rec.start_time);
    }

    #[test]
    fn update_record_node_name_is_a_noop_when_the_record_is_already_gone() {
        let tmp = tempfile::TempDir::new().unwrap();
        // A concurrent detach/quit could have already cleared the record —
        // must not error, there's simply nothing left to correct.
        update_record_node_name(
            tmp.path(),
            "nonexistent",
            1,
            4242,
            "bt_attach_x_1@localhost",
        )
        .unwrap();
    }

    /// Regression test (adversarial-review follow-up): without the
    /// `expected_pid` compare-and-swap check, this call would have
    /// unconditionally overwritten `node_name`, which — combined with the
    /// caller now running this on a background thread (see
    /// `update_record_node_name`'s doc comment) — could resurrect/mutate a
    /// record for a process that's no longer the one this correction was
    /// actually resolving a name for (e.g. a concurrent detach recreated the
    /// same `(workspace_id, port)` key for a genuinely different, newer
    /// front in between this call being scheduled and running).
    #[test]
    fn update_record_node_name_is_a_noop_when_the_recorded_pid_no_longer_matches() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();

        // A stale correction for the *old* pid arrives after a new front
        // record (same workspace_id/port, different pid) has replaced it.
        update_record_node_name(
            tmp.path(),
            &rec.workspace_id,
            rec.port,
            9999, // not rec.pid
            "bt_attach_abc123_9999@localhost",
        )
        .unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(
            loaded,
            vec![rec],
            "record must be left completely untouched when expected_pid doesn't match"
        );
    }

    /// Regression test (BT-3062): `update_record_node_name`'s read-check-write
    /// and `remove_record`'s delete must never interleave, closing the
    /// resurrection window described in `update_record_node_name`'s doc
    /// comment (a racing `remove_record` landing strictly between the read
    /// and the write would otherwise recreate the file it just deleted).
    /// Rather than trying to force that exact interleaving from a test
    /// (inherently racy to arrange), this proves the mechanism that prevents
    /// it: both functions contend on the same [`record_ops_lock`], so a
    /// `remove_record` that starts while that lock is held blocks until it's
    /// released, rather than running concurrently.
    ///
    /// Trade-off, noted rather than avoided (adversarial-review follow-up):
    /// `record_ops_lock` is one process-wide `static`, not scoped per test,
    /// so holding it for this test's 200ms window incidentally blocks any
    /// other test in this binary that concurrently calls
    /// `save_record`/`update_record_node_name`/`remove_record` on its own
    /// unrelated tmp dir under default parallel `cargo test` execution. No
    /// tight timeouts exist elsewhere in this suite, so this doesn't cause
    /// failures — just up to 200ms of incidental cross-test latency.
    #[test]
    fn update_and_remove_are_mutually_exclusive_on_the_record_lock() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();

        // Stand in for `update_record_node_name`'s own critical section
        // being "mid-flight" between its read and its write.
        let guard = locked_record_ops();

        let dir = tmp.path().to_path_buf();
        let workspace_id = rec.workspace_id.clone();
        let port = rec.port;
        let handle = std::thread::spawn(move || {
            remove_record(&dir, &workspace_id, port).unwrap();
        });

        // Give the spawned remove_record ample time to have run if it were
        // (wrongly) unsynchronized, then confirm it's still blocked and the
        // record file is still present.
        std::thread::sleep(Duration::from_millis(200));
        assert!(
            !handle.is_finished(),
            "remove_record should still be blocked on record_ops_lock"
        );
        assert_eq!(
            load_all_records(tmp.path()).unwrap(),
            vec![rec.clone()],
            "record must still be on disk while the lock is held"
        );

        drop(guard);
        handle.join().unwrap();

        assert!(
            load_all_records(tmp.path()).unwrap().is_empty(),
            "remove_record should complete once the lock is released"
        );
    }

    #[cfg(windows)]
    #[test]
    fn remove_record_also_removes_the_release_tmp_directory() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        let release_tmp = crate::spawn::release_tmp_dir(&rec.workspace_id, rec.port);
        std::fs::create_dir_all(&release_tmp).unwrap();
        std::fs::write(release_tmp.join("sys.config"), b"secrets").unwrap();

        save_record(tmp.path(), &rec).unwrap();
        remove_record(tmp.path(), &rec.workspace_id, rec.port).unwrap();

        // The RELEASE_TMP removal now runs on a detached background thread
        // (BT-3059) rather than blocking `remove_record`, so its return no
        // longer proves the directory is gone — bounded-poll instead of
        // asserting synchronously. The retry loop itself caps at ~1s
        // (`RELEASE_TMP_REMOVE_ATTEMPTS` * `RELEASE_TMP_REMOVE_RETRY_DELAY`);
        // give it generous headroom above that for thread-scheduling jitter.
        let deadline = std::time::Instant::now() + Duration::from_secs(5);
        while release_tmp.exists() && std::time::Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(50));
        }

        assert!(
            !release_tmp.exists(),
            "RELEASE_TMP directory should be removed along with the front record"
        );
    }

    #[test]
    fn load_all_records_returns_empty_for_missing_dir() {
        let tmp = tempfile::TempDir::new().unwrap();
        let missing = tmp.path().join("does-not-exist");
        let loaded = load_all_records(&missing).unwrap();
        assert!(loaded.is_empty());
    }

    #[test]
    fn load_all_records_skips_unparsable_files_but_keeps_the_rest() {
        let tmp = tempfile::TempDir::new().unwrap();
        let good = record(4242, Some(555));
        save_record(tmp.path(), &good).unwrap();
        std::fs::write(tmp.path().join("garbage.json"), b"not json").unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(loaded, vec![good]);
    }

    #[test]
    fn load_all_records_ignores_non_json_files() {
        let tmp = tempfile::TempDir::new().unwrap();
        std::fs::write(tmp.path().join("notes.txt"), b"hello").unwrap();
        let loaded = load_all_records(tmp.path()).unwrap();
        assert!(loaded.is_empty());
    }

    // ── full sweep, using this test process's own PID as a live target ──

    #[test]
    fn sweep_clears_a_stale_record_for_a_pid_that_does_not_exist() {
        let tmp = tempfile::TempDir::new().unwrap();
        // A PID essentially guaranteed not to exist.
        let rec = record(u32::MAX, Some(1));
        save_record(tmp.path(), &rec).unwrap();

        let report = sweep(tmp.path()).unwrap();
        assert_eq!(report.stale_cleared, vec![rec]);
        assert!(report.reaped.is_empty());
        assert!(load_all_records(tmp.path()).unwrap().is_empty());
    }

    // Linux/Windows-only: `read_start_time` only returns a real value on
    // those two platforms (see its doc comments; BT-3046 added the Windows
    // `GetProcessTimes` arm) — `classify_record`'s fallback for a platform
    // where the *observed* start time is unavailable is `Reap` (best effort,
    // matching `beamtalk-cli`'s own stance), which on a CI runner without a
    // real `read_start_time` would make this test SIGTERM/TerminateProcess
    // its own test process. Gating this way keeps that real behavior safe to
    // exercise here; `classify_record`'s fallback branch itself is covered
    // platform-independently by `alive_with_no_observed_start_time_falls_back_to_reap`
    // above.
    #[cfg(any(target_os = "linux", windows))]
    #[test]
    fn sweep_skips_and_clears_a_pid_reused_record_without_reaping() {
        let tmp = tempfile::TempDir::new().unwrap();
        // This test process's own PID is alive, but its recorded start_time
        // is deliberately wrong — simulates PID reuse. `is_process_alive`
        // will report true; the actual observed start time (`/proc` on
        // Linux, `GetProcessTimes` on Windows) won't equal `Some(1)`, so
        // this must land in SkipPidReused rather than Reap (which would
        // SIGTERM/TerminateProcess this very test process — catastrophic if
        // this assertion is wrong).
        let own_pid = std::process::id();
        let rec = record(own_pid, Some(1));
        save_record(tmp.path(), &rec).unwrap();

        let report = sweep(tmp.path()).unwrap();
        assert!(
            report.reaped.is_empty(),
            "must never reap the test process itself"
        );
        assert_eq!(report.pid_reused_skipped, vec![rec]);
        assert!(
            is_process_alive(own_pid),
            "the test process must survive sweep"
        );
    }

    // ── terminate_process: PID-reuse re-check before SIGKILL ────────────
    //
    // Regression tests for the adversarial-review finding: `classify_record`
    // verifies start_time before `sweep` calls `terminate_process`, but the
    // SIGTERM→TERMINATE_GRACE→SIGKILL sequence has its own window in which
    // the PID could (in principle) be reused. `terminate_process` now
    // re-verifies immediately before SIGKILL — proven here against a real
    // spawned process rather than only the pure `start_time_matches` logic.

    #[cfg(target_os = "linux")]
    #[test]
    fn terminate_process_kills_when_start_time_matches() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("failed to spawn `sleep 30` — is coreutils installed?");
        let pid = child.id();
        // Give /proc a moment to have a stable stat entry before reading it.
        std::thread::sleep(Duration::from_millis(50));
        let real_start_time = read_start_time(pid);
        assert!(
            real_start_time.is_some(),
            "should be able to read the freshly-spawned child's start time"
        );

        terminate_process(pid, real_start_time);

        // wait() reaps the zombie and blocks until the process is actually gone.
        let status = child.wait().expect("wait on killed child should succeed");
        assert!(
            !status.success(),
            "child should have been signaled, not exit(0)"
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn terminate_process_does_not_kill_on_start_time_mismatch() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("failed to spawn `sleep 30` — is coreutils installed?");
        let pid = child.id();

        // A deliberately wrong expected start time — simulates the PID
        // having been reused by an unrelated process since the record was made.
        terminate_process(pid, Some(1));

        assert!(
            is_process_alive(pid),
            "must not signal a process whose start time doesn't match"
        );

        // Cleanup: the test owns this child and must not leak it regardless
        // of the assertion above.
        let _ = child.kill();
        let _ = child.wait();
    }

    // Windows counterparts of the two Linux `terminate_process` tests above
    // (BT-3046) — exercise the real `GetProcessTimes`/`TerminateProcess`
    // path rather than only `start_time_matches`'s pure logic. `ping` (not
    // `sleep`, which doesn't exist on Windows) gives a real, long-running
    // `.exe` to target — no `cmd.exe` wrapper involved, since `ping.exe` is
    // a real Win32 executable `Command::new` can exec directly. Windows now
    // re-verifies before `TerminateProcess` too (adversarial-review
    // follow-up during this same issue — `still_same_process` is no longer
    // Unix-only), so both cases apply here symmetrically with the Linux arm.

    #[cfg(windows)]
    #[test]
    fn terminate_process_kills_when_start_time_matches_windows() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "30", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 30 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        // Give the process a moment to be fully queryable before reading it.
        std::thread::sleep(Duration::from_millis(50));
        let real_start_time = read_start_time(pid);
        assert!(
            real_start_time.is_some(),
            "should be able to read the freshly-spawned child's start time"
        );

        terminate_process(pid, real_start_time);

        // wait() blocks until the process is actually gone.
        let status = child.wait().expect("wait on killed child should succeed");
        assert!(
            !status.success(),
            "child should have been terminated, not exit(0)"
        );
    }

    #[cfg(windows)]
    #[test]
    fn terminate_process_does_not_kill_on_start_time_mismatch_windows() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "30", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 30 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();

        // A deliberately wrong expected start time — simulates the PID
        // having been reused by an unrelated process since the record was made.
        terminate_process(pid, Some(1));

        assert!(
            is_process_alive(pid),
            "must not signal a process whose start time doesn't match"
        );

        // Cleanup: the test owns this child and must not leak it regardless
        // of the assertion above.
        let _ = child.kill();
        let _ = child.wait();
    }

    // ── read_start_time: sanity checks (BT-3046) ────────────────────────

    #[cfg(windows)]
    #[test]
    fn read_start_time_returns_some_for_a_live_process() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "5", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 5 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        std::thread::sleep(Duration::from_millis(50));

        assert!(
            read_start_time(pid).is_some(),
            "GetProcessTimes should succeed for a live, queryable process"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    /// Regression test for a mis-ordered `GetProcessTimes` out-param (BT-3046
    /// adversarial-review follow-up): the call takes four `*mut FILETIME`
    /// slots (creation, exit, kernel, user) and it's easy to read the wrong
    /// one back. `read_start_time_returns_some_for_a_live_process` above only
    /// asserts `.is_some()`, which a swapped-in `exit`/`kernel`/`user` read
    /// would still satisfy — `lpExitTime` is specifically documented to stay
    /// the zero `FILETIME` for a process that hasn't exited yet, so a
    /// same-process repeated read that's stable *and* non-zero is a much
    /// stronger signal that `dwLowDateTime`/`dwHighDateTime` came from the
    /// right (creation) slot.
    #[cfg(windows)]
    #[test]
    fn read_start_time_is_stable_and_nonzero_for_a_live_process() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "10", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 10 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        std::thread::sleep(Duration::from_millis(50));

        let first = read_start_time(pid);
        std::thread::sleep(Duration::from_millis(50));
        let second = read_start_time(pid);

        assert_ne!(
            first,
            Some(0),
            "a live process's creation time should never be the zero FILETIME \
             sentinel — Some(0) would indicate the wrong out-param (e.g. the \
             still-running process's zeroed lpExitTime) was read instead of \
             lpCreationTime"
        );
        assert_eq!(
            first, second,
            "repeated reads of the same live process's creation time must be stable"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[cfg(windows)]
    #[test]
    fn read_start_time_returns_none_for_a_nonexistent_pid() {
        assert_eq!(
            read_start_time(u32::MAX),
            None,
            "OpenProcess should fail for a PID essentially guaranteed not to exist"
        );
    }
}
