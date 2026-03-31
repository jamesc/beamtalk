// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP Port binary for subprocess management (ADR 0051).
//!
//! **DDD Context:** Runtime (Anti-Corruption Layer boundary)
//!
//! Reads ETF-encoded commands from BEAM via `{packet, 4}` framing, manages
//! subprocesses with separate stdout/stderr pipes, and sends events back
//! to the BEAM as ETF-encoded tuples.
//!
//! # Protocol
//!
//! ## Commands (BEAM → exec binary) — ETF maps
//!
//! - `#{command => spawn, child_id => N, executable => Bin, args => [Bin],
//!       env => #{Bin => Bin}, dir => Bin}`
//! - `#{command => kill, child_id => N}`
//! - `#{command => write_stdin, child_id => N, data => Bin}`
//! - `#{command => close_stdin, child_id => N}`
//!
//! ## Events (exec binary → BEAM) — ETF tuples
//!
//! - `{stdout, ChildId, Data}`
//! - `{stderr, ChildId, Data}`
//! - `{exit, ChildId, ExitCode}`

use std::collections::HashMap;
use std::io::{self, Read, Write};
#[cfg(windows)]
use std::os::windows::io::AsRawHandle;
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
#[cfg(unix)]
use std::time::{Duration, Instant};

use beamtalk_etf::{
    self as etf, atom, binary as binary_term, int_term, map_get, term_to_bytes, term_to_string,
    term_to_string_list, term_to_string_map, tuple3,
};
use clap::{ArgAction, Parser};
use eetf::{Map, Term};
use tracing::{debug, error, warn};
use tracing_subscriber::EnvFilter;

// ────────────────────────────────────────────────────────────────
// Windows Job Object support (BT-1133)

#[cfg(windows)]
mod job_object {
    use std::io;

    use windows_sys::Win32::Foundation::{CloseHandle, HANDLE};
    use windows_sys::Win32::System::JobObjects::{
        AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
        SetInformationJobObject, TerminateJobObject,
    };

    /// RAII wrapper for a Win32 Job Object handle.
    ///
    /// Closes the handle on drop. The Job Object itself is destroyed by the OS
    /// when all handles to it are closed and no processes remain assigned.
    pub(crate) struct JobObject {
        handle: HANDLE,
    }

    // SAFETY: Win32 HANDLEs are plain pointer-sized values with no thread
    // affinity — they can be used from any thread and sent between threads.
    unsafe impl Send for JobObject {}
    // SAFETY: see Send impl above — HANDLEs have no thread affinity.
    unsafe impl Sync for JobObject {}

    impl JobObject {
        /// Create an anonymous Job Object with `KILL_ON_JOB_CLOSE` enabled.
        ///
        /// `KILL_ON_JOB_CLOSE` ensures that if `beamtalk-exec` crashes or exits
        /// before `kill_all_children` runs, closing the last handle still terminates
        /// all assigned processes — preventing orphaned subprocess trees.
        pub(crate) fn create() -> io::Result<Self> {
            // SAFETY: CreateJobObjectW with null name/security creates an anonymous job.
            let handle = unsafe { CreateJobObjectW(std::ptr::null(), std::ptr::null()) };
            // CreateJobObjectW returns NULL on failure (not INVALID_HANDLE_VALUE).
            if handle.is_null() {
                return Err(io::Error::last_os_error());
            }

            // Enable KILL_ON_JOB_CLOSE so orphaned children are terminated when
            // the handle is closed (e.g. if the helper crashes).
            let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION =
                // SAFETY: zero-initializing a plain-old-data struct is valid.
                unsafe { std::mem::zeroed() };
            info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
            // SAFETY: valid job handle, correctly sized struct.
            let rc = unsafe {
                SetInformationJobObject(
                    handle,
                    JobObjectExtendedLimitInformation,
                    std::ptr::addr_of!(info).cast(),
                    u32::try_from(std::mem::size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>())
                        .expect("JOBOBJECT_EXTENDED_LIMIT_INFORMATION fits in u32"),
                )
            };
            if rc == 0 {
                let err = io::Error::last_os_error();
                // SAFETY: CloseHandle on a valid handle.
                unsafe { CloseHandle(handle) };
                return Err(err);
            }

            Ok(Self { handle })
        }

        /// Assign a process (by raw handle) to this Job Object.
        pub(crate) fn assign_process(&self, process_handle: HANDLE) -> io::Result<()> {
            // SAFETY: Both handles are valid at this point.
            let rc = unsafe { AssignProcessToJobObject(self.handle, process_handle) };
            if rc == 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        }

        /// Terminate all processes in the Job Object with the given exit code.
        pub(crate) fn terminate(&self, exit_code: u32) -> io::Result<()> {
            // SAFETY: Valid job handle.
            let rc = unsafe { TerminateJobObject(self.handle, exit_code) };
            if rc == 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        }
    }

    impl Drop for JobObject {
        fn drop(&mut self) {
            // SAFETY: CloseHandle on a valid handle is always safe.
            unsafe { CloseHandle(self.handle) };
        }
    }
}

// ────────────────────────────────────────────────────────────────
// Domain-specific ETF helpers (not shared via beamtalk-etf)

/// Extract a `u32` child ID from a `FixInteger` term.
fn term_to_child_id(term: &Term) -> Option<u32> {
    match term {
        Term::FixInteger(n) => u32::try_from(n.value).ok(),
        _ => None,
    }
}

// ────────────────────────────────────────────────────────────────
// Event encoding

type SharedWriter = Arc<Mutex<io::Stdout>>;

/// Encode an ETF term and write it as a `{packet, 4}` framed packet.
fn send_event(writer: &SharedWriter, event: &Term) {
    let mut buf = Vec::new();
    match event.encode(&mut buf) {
        Ok(()) => {
            if let Ok(mut out) = writer.lock() {
                if let Err(e) = etf::write_packet(&mut *out, &buf) {
                    error!("Failed to write event packet: {e}");
                }
            }
        }
        Err(e) => {
            error!("Failed to encode event term: {e}");
        }
    }
}

fn stdout_event(child_id: u32, data: &[u8]) -> Term {
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    tuple3(atom("stdout"), int_term(child_id as i32), binary_term(data))
}

fn stderr_event(child_id: u32, data: &[u8]) -> Term {
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    tuple3(atom("stderr"), int_term(child_id as i32), binary_term(data))
}

fn exit_event(child_id: u32, code: i32) -> Term {
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    tuple3(atom("exit"), int_term(child_id as i32), int_term(code))
}

// ────────────────────────────────────────────────────────────────
// Child process state

/// Per-child management state stored in the shared map.
///
/// The `Child` handle is given to a reaper thread at spawn time; only
/// `stdin` and the process-group ID remain here for lifecycle management.
struct ChildEntry {
    /// Stdin writer — a `ChildStdin` pipe.
    /// `None` while temporarily taken by `write_stdin` or after `close_stdin`.
    /// Use `stdin_closed` to distinguish the two cases.
    stdin: Option<Box<dyn Write + Send>>,
    /// Set to `true` by `close_stdin` so that a concurrent `write_stdin`
    /// (which takes stdin, releases the lock, does I/O, then restores) does
    /// not reopen the pipe after it has been intentionally closed.
    stdin_closed: bool,
    /// Process group ID (Unix only). After `setsid()`, pgid == child PID.
    #[cfg(unix)]
    pgid: libc::pid_t,
    /// Job Object handle (Windows only). All child processes are assigned to
    /// this Job Object so they can be terminated as a group.
    #[cfg(windows)]
    job: job_object::JobObject,
}

type ChildMap = Arc<Mutex<HashMap<u32, ChildEntry>>>;

// ────────────────────────────────────────────────────────────────
// Exit code helpers

/// Extract an exit code from an `ExitStatus`.
///
/// On Unix, if the process was terminated by a signal (`code()` returns `None`),
/// use the standard shell convention of 128 + signal number.
#[cfg(unix)]
fn exit_code_from_status(status: std::process::ExitStatus) -> i32 {
    use std::os::unix::process::ExitStatusExt;
    status
        .code()
        .unwrap_or_else(|| status.signal().map_or(-1, |sig| 128 + sig))
}

#[cfg(not(unix))]
fn exit_code_from_status(status: std::process::ExitStatus) -> i32 {
    status.code().unwrap_or(-1)
}

// ────────────────────────────────────────────────────────────────
// Command handlers

#[allow(clippy::too_many_lines)]
fn handle_spawn(request: &Map, writer: &SharedWriter, children: &ChildMap) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    let executable = map_get(request, "executable")
        .and_then(term_to_string)
        .ok_or("missing or invalid executable")?;

    let args = map_get(request, "args")
        .and_then(term_to_string_list)
        .unwrap_or_default();

    let env_vars = map_get(request, "env")
        .and_then(term_to_string_map)
        .unwrap_or_default();

    // Empty string means "inherit cwd" — only call current_dir for non-empty paths.
    let dir = map_get(request, "dir")
        .and_then(term_to_string)
        .filter(|s| !s.is_empty());

    handle_spawn_piped(
        child_id,
        &executable,
        &args,
        &env_vars,
        dir.as_deref(),
        writer,
        children,
    )
}

/// Spawn a child process using regular pipes (works on all platforms).
fn handle_spawn_piped(
    child_id: u32,
    executable: &str,
    args: &[String],
    env_vars: &HashMap<String, String>,
    dir: Option<&str>,
    writer: &SharedWriter,
    children: &ChildMap,
) -> Result<(), String> {
    let mut cmd = Command::new(executable);
    cmd.args(args);
    cmd.envs(env_vars);
    if let Some(d) = dir {
        cmd.current_dir(d);
    }
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    // On Unix, put the child in its own session so we can signal the whole
    // process group (not just the direct child).
    #[cfg(unix)]
    configure_process_group(&mut cmd);

    // On Windows, prevent child processes from opening visible console windows.
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        cmd.creation_flags(CREATE_NO_WINDOW);
    }

    let mut child = cmd.spawn().map_err(|e| format!("spawn failed: {e}"))?;

    let child_stdin: Option<Box<dyn Write + Send>> = child
        .stdin
        .take()
        .map(|s| Box::new(s) as Box<dyn Write + Send>);
    let child_stdout = child.stdout.take().ok_or("stdout not captured")?;
    let child_stderr = child.stderr.take().ok_or("stderr not captured")?;

    // After setsid(), the child's pgid equals its own pid.
    // Child PIDs fit in i32 on all Unix platforms (kernel enforces PID_MAX <= 2^22).
    #[cfg(unix)]
    #[allow(clippy::cast_possible_wrap)]
    let pgid = child.id() as libc::pid_t;

    // On Windows, create a Job Object and assign the child to it so the entire
    // process tree can be terminated as a group (BT-1133).
    #[cfg(windows)]
    let job = {
        let job =
            job_object::JobObject::create().map_err(|e| format!("CreateJobObject failed: {e}"))?;
        let process_handle = child.as_raw_handle() as windows_sys::Win32::Foundation::HANDLE;
        job.assign_process(process_handle)
            .map_err(|e| format!("AssignProcessToJobObject failed: {e}"))?;
        job
    };

    // Insert entry before spawning threads, so kill/write_stdin can find it.
    // Reject duplicate child_id to avoid silently orphaning the old process.
    {
        let mut map = children
            .lock()
            .map_err(|e| format!("children lock poisoned: {e}"))?;
        if map.contains_key(&child_id) {
            drop(map);
            let _ = child.kill();
            let _ = child.wait();
            return Err(format!("child_id {child_id} already in use"));
        }
        map.insert(
            child_id,
            ChildEntry {
                stdin: child_stdin,
                stdin_closed: false,
                #[cfg(unix)]
                pgid,
                #[cfg(windows)]
                job,
            },
        );
    }

    // Spawn stdout reader thread.
    let stdout_handle = spawn_reader_thread(child_id, child_stdout, Arc::clone(writer), "stdout");

    // Spawn stderr reader thread.
    let stderr_handle = spawn_reader_thread(child_id, child_stderr, Arc::clone(writer), "stderr");

    // Spawn reaper thread: waits for the child, joins reader threads, then sends exit event.
    //
    // Joining the reader threads before sending exit guarantees that all buffered
    // stdout/stderr data has already been written to the port before the BEAM
    // gen_server receives the exit signal.  Without this, the reaper could send
    // exit before the reader threads have flushed their final reads, causing the
    // gen_server to close the port and discard in-flight data (the race fixed by
    // the 10 ms drain timer in the previous workaround — see BT-1148).
    {
        let writer = Arc::clone(writer);
        let children = Arc::clone(children);
        thread::spawn(move || {
            let exit_code = match child.wait() {
                Ok(status) => exit_code_from_status(status),
                Err(e) => {
                    error!("wait() failed for child {child_id}: {e}");
                    -1
                }
            };
            // Remove the entry — at this point stdin has already been closed
            // (or was never opened) and the process is gone.
            if let Ok(mut map) = children.lock() {
                map.remove(&child_id);
            }
            // Join reader threads: blocks until both have finished sending all
            // stdout/stderr events.  Errors from join (thread panicked) are
            // ignored — we still send the exit event.
            let _ = stdout_handle.join();
            let _ = stderr_handle.join();
            send_event(&writer, &exit_event(child_id, exit_code));
        });
    }

    Ok(())
}

/// Spawn a reader thread that reads from a stream and sends events.
fn spawn_reader_thread<R: Read + Send + 'static>(
    child_id: u32,
    reader: R,
    writer: SharedWriter,
    stream: &'static str,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        let mut reader = reader;
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    let event = if stream == "stderr" {
                        stderr_event(child_id, &buf[..n])
                    } else {
                        stdout_event(child_id, &buf[..n])
                    };
                    send_event(&writer, &event);
                }
                Err(e) => {
                    debug!("{stream} read error for child {child_id}: {e}");
                    break;
                }
            }
        }
    })
}

/// Run in cat mode: read stdin line by line and write to stdout with explicit
/// flushing. This provides a Windows-compatible `cat` equivalent that works
/// with pipes (bypasses the C runtime's full buffering on non-console stdout).
fn run_cat_mode() -> ! {
    use std::io::BufRead;

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    for line in stdin.lock().lines() {
        match line {
            Ok(l) => {
                if writeln!(stdout, "{l}").is_err() || stdout.flush().is_err() {
                    break;
                }
            }
            Err(_) => break,
        }
    }

    std::process::exit(0);
}

/// Configure the child process to start in a new process group (Unix only).
///
/// Called before `Command::spawn` so the child can be signal-killed as a group.
#[cfg(unix)]
fn configure_process_group(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    // SAFETY: setsid(2) is async-signal-safe and has no per-thread invariants.
    unsafe {
        cmd.pre_exec(|| {
            if libc::setsid() < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        });
    }
}

fn handle_kill(request: &Map, children: &ChildMap) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    // Retrieve the process-group ID without holding the lock during the sleep.
    #[cfg(unix)]
    let pgid = {
        let map = children
            .lock()
            .map_err(|e| format!("children lock poisoned: {e}"))?;
        match map.get(&child_id) {
            Some(entry) => entry.pgid,
            None => return Err(format!("child {child_id} not found")),
        }
    };

    #[cfg(unix)]
    {
        // SIGTERM to the whole process group, then wait up to 2 s for exit.
        // SAFETY: kill(2) with a valid pgid is well-defined.
        let rc = unsafe { libc::kill(-pgid, libc::SIGTERM) };
        if rc < 0 {
            let err = io::Error::last_os_error();
            if err.raw_os_error() == Some(libc::ESRCH) {
                // Process group already gone — nothing to wait for.
                return Ok(());
            }
            warn!("SIGTERM to pgid -{pgid} failed: {err}");
        }

        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            {
                let map = children
                    .lock()
                    .map_err(|e| format!("children lock poisoned: {e}"))?;
                if !map.contains_key(&child_id) {
                    break; // Reaper thread already cleaned it up.
                }
            }
            if Instant::now() >= deadline {
                // SAFETY: SIGKILL to process group.
                unsafe { libc::kill(-pgid, libc::SIGKILL) };
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }
    }

    #[cfg(windows)]
    {
        // Terminate all processes in the Job Object (BT-1133).
        let map = children
            .lock()
            .map_err(|e| format!("children lock poisoned: {e}"))?;
        match map.get(&child_id) {
            Some(entry) => {
                if let Err(e) = entry.job.terminate(1) {
                    warn!("TerminateJobObject failed for child {child_id}: {e}");
                }
            }
            None => return Err(format!("child {child_id} not found")),
        }
    }

    Ok(())
}

fn handle_write_stdin(request: &Map, children: &ChildMap) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    let data = map_get(request, "data")
        .and_then(term_to_bytes)
        .ok_or("missing or invalid data")?;

    // Take stdin out of the entry so the children lock is released before
    // blocking I/O — holding the lock during write_all/flush would prevent
    // concurrent spawn/kill commands from making progress.
    let mut stdin = {
        let mut map = children
            .lock()
            .map_err(|e| format!("children lock poisoned: {e}"))?;
        let entry = map
            .get_mut(&child_id)
            .ok_or_else(|| format!("child {child_id} not found"))?;
        entry
            .stdin
            .take()
            .ok_or("stdin already closed for this child")?
    };
    // Lock released — perform blocking I/O without holding it.

    let result = stdin
        .write_all(&data)
        .and_then(|()| stdin.flush())
        .map_err(|e| format!("write_stdin error: {e}"));

    // Restore stdin unless close_stdin was called while we held it — in that
    // case stdin_closed is true and we must not reopen the pipe.
    if let Ok(mut map) = children.lock() {
        if let Some(entry) = map.get_mut(&child_id) {
            if entry.stdin_closed {
                // close_stdin ran concurrently; drop stdin here to close pipe.
                drop(stdin);
            } else {
                entry.stdin = Some(stdin);
            }
        }
    }

    result
}

fn handle_close_stdin(request: &Map, children: &ChildMap) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    let mut map = children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    let entry = map
        .get_mut(&child_id)
        .ok_or_else(|| format!("child {child_id} not found"))?;
    // Mark closed first so a concurrent write_stdin restore does not reopen
    // the pipe (see handle_write_stdin restore logic).
    entry.stdin_closed = true;

    // Dropping closes the OS pipe, sending EOF to the child.
    entry.stdin = None;
    Ok(())
}

fn handle_command(term: &Term, writer: &SharedWriter, children: &ChildMap) -> Result<(), String> {
    let Term::Map(map) = term else {
        return Err("command must be a map".to_string());
    };

    let command = match map_get(map, "command") {
        Some(Term::Atom(a)) => a.name.as_str(),
        _ => return Err("missing or invalid 'command' field".to_string()),
    };

    match command {
        "spawn" => handle_spawn(map, writer, children),
        "kill" => handle_kill(map, children),
        "write_stdin" => handle_write_stdin(map, children),
        "close_stdin" => handle_close_stdin(map, children),
        _ => Err(format!("unknown command: {command}")),
    }
}

/// Send SIGTERM to all running process groups, wait up to 2 s, then SIGKILL.
fn kill_all_children(children: &ChildMap) {
    #[cfg(unix)]
    {
        let pgids: Vec<libc::pid_t> = children
            .lock()
            .map(|m| m.values().map(|e| e.pgid).collect())
            .unwrap_or_default();

        for pgid in &pgids {
            // SAFETY: SIGTERM to a valid process group.
            unsafe { libc::kill(-pgid, libc::SIGTERM) };
        }

        thread::sleep(Duration::from_secs(2));

        // Send SIGKILL to any survivors.
        let pgids: Vec<libc::pid_t> = children
            .lock()
            .map(|m| m.values().map(|e| e.pgid).collect())
            .unwrap_or_default();
        for pgid in &pgids {
            // SAFETY: SIGKILL to a valid process group.
            unsafe { libc::kill(-pgid, libc::SIGKILL) };
        }
    }

    // Windows: terminate all Job Objects to kill all process trees (BT-1133).
    #[cfg(windows)]
    {
        if let Ok(map) = children.lock() {
            for (child_id, entry) in map.iter() {
                if let Err(e) = entry.job.terminate(1) {
                    warn!("TerminateJobObject failed for child {child_id}: {e}");
                }
            }
        }
    }
}

// ────────────────────────────────────────────────────────────────
// CLI

#[derive(Debug, Parser)]
#[command(
    name = "beamtalk-exec",
    about = "Beamtalk subprocess management port (ADR 0051)"
)]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,

    /// Run as a cat replacement: read stdin line by line and echo to stdout
    /// with explicit flushing. Used on Windows where pipe buffering prevents
    /// interactive I/O with regular programs.
    #[arg(long = "cat", hide = true)]
    cat_mode: bool,
}

fn directive_for_verbosity(v: u8) -> &'static str {
    match v {
        0 => "warn",
        1 => "debug",
        _ => "trace",
    }
}

fn main() {
    let cli = Cli::parse();

    // Handle cat mode: line-by-line stdin→stdout echo with explicit flushing.
    // Used on Windows as a cat replacement for interactive subprocess I/O,
    // since Windows programs use full buffering on pipe stdout.
    if cli.cat_mode {
        run_cat_mode();
    }

    // Only initialise tracing when explicitly requested — default must produce
    // no stderr output to avoid interfering with the OTP port protocol.
    let has_rust_log = std::env::var("RUST_LOG").is_ok();
    if has_rust_log || cli.verbose > 0 {
        let env_filter = if has_rust_log {
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"))
        } else {
            EnvFilter::new(directive_for_verbosity(cli.verbose))
        };
        let _ = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stderr)
            .with_ansi(false)
            .try_init();
    }

    let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
    let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));

    let mut stdin = io::stdin().lock();

    loop {
        let packet = match etf::read_packet(&mut stdin) {
            Ok(Some(data)) => data,
            Ok(None) => break, // EOF — BEAM closed the port.
            Err(e) => {
                error!("Failed to read packet: {e}");
                break;
            }
        };

        let term = match Term::decode(io::Cursor::new(&packet)) {
            Ok(t) => t,
            Err(e) => {
                error!("ETF decode error: {e}");
                continue;
            }
        };

        if let Err(e) = handle_command(&term, &writer, &children) {
            warn!("Command error: {e}");
            // If a spawn command failed, emit a synthetic exit event so the
            // BEAM caller does not wait forever for an exit that will never arrive.
            if let Term::Map(map) = &term {
                if matches!(map_get(map, "command"), Some(Term::Atom(a)) if a.name == "spawn") {
                    if let Some(child_id) = map_get(map, "child_id").and_then(term_to_child_id) {
                        send_event(&writer, &exit_event(child_id, -1));
                    }
                }
            }
        }
    }

    // BEAM closed the port — terminate all running children.
    kill_all_children(&children);
}

// ────────────────────────────────────────────────────────────────
// Tests

#[cfg(test)]
mod tests {
    use super::*;

    // NOTE: Packet framing tests are now in beamtalk-etf.

    // ── ETF event encoding ──────────────────────────────────────

    #[test]
    fn stdout_event_encodes_correctly() {
        let event = stdout_event(42, b"hello");
        let mut buf = Vec::new();
        event.encode(&mut buf).unwrap();
        let decoded = Term::decode(io::Cursor::new(buf)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 3);
                assert_eq!(t.elements[0], atom("stdout"));
                assert_eq!(t.elements[1], int_term(42));
                assert_eq!(t.elements[2], binary_term(b"hello"));
            }
            _ => panic!("expected Tuple, got {decoded:?}"),
        }
    }

    #[test]
    fn stderr_event_encodes_correctly() {
        let event = stderr_event(7, b"err");
        let mut buf = Vec::new();
        event.encode(&mut buf).unwrap();
        let decoded = Term::decode(io::Cursor::new(buf)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements[0], atom("stderr"));
                assert_eq!(t.elements[1], int_term(7));
            }
            _ => panic!("expected Tuple"),
        }
    }

    #[test]
    fn exit_event_encodes_correctly() {
        let event = exit_event(1, 0);
        let mut buf = Vec::new();
        event.encode(&mut buf).unwrap();
        let decoded = Term::decode(io::Cursor::new(buf)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements[0], atom("exit"));
                assert_eq!(t.elements[1], int_term(1));
                assert_eq!(t.elements[2], int_term(0));
            }
            _ => panic!("expected Tuple"),
        }
    }

    // ── Command parsing ─────────────────────────────────────────

    #[test]
    fn handle_command_not_a_map_returns_error() {
        let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = atom("not_a_map");
        assert!(handle_command(&req, &writer, &children).is_err());
    }

    #[test]
    fn handle_command_unknown_command_returns_error() {
        let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = Term::from(Map::from([(atom("command"), atom("unknown_cmd"))]));
        assert!(handle_command(&req, &writer, &children).is_err());
    }

    #[test]
    fn handle_command_missing_command_returns_error() {
        let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = Term::from(Map::from([(atom("other"), atom("value"))]));
        assert!(handle_command(&req, &writer, &children).is_err());
    }

    // ── term helpers ────────────────────────────────────────────

    #[test]
    fn term_to_child_id_valid() {
        assert_eq!(term_to_child_id(&int_term(99)), Some(99));
    }

    #[test]
    fn term_to_child_id_negative_returns_none() {
        assert_eq!(term_to_child_id(&int_term(-1)), None);
    }

    #[test]
    fn term_to_child_id_non_integer_returns_none() {
        assert_eq!(term_to_child_id(&atom("foo")), None);
    }

    #[test]
    fn term_to_string_valid_binary() {
        let t = binary_term(b"hello");
        assert_eq!(term_to_string(&t), Some("hello".to_string()));
    }

    // ── spawn / kill integration (Unix only) ────────────────────

    #[test]
    #[cfg(unix)]
    fn spawn_true_succeeds_and_exits() {
        use eetf::List;
        let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));

        let args_term = Term::from(List::from(vec![binary_term(b"true")]));
        let env_term = Term::from(Map::from(HashMap::<Term, Term>::new()));
        let req = Term::from(Map::from([
            (atom("command"), atom("spawn")),
            (atom("child_id"), int_term(1)),
            (atom("executable"), binary_term(b"/usr/bin/env")),
            (atom("args"), args_term),
            (atom("env"), env_term),
        ]));

        let result = handle_command(&req, &writer, &children);
        assert!(
            result.is_ok(),
            "spawn `env true` should succeed: {result:?}"
        );

        // Wait briefly for the reaper thread to clean up.
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            {
                let map = children.lock().unwrap();
                if !map.contains_key(&1) {
                    break; // Reaped.
                }
            }
            assert!(Instant::now() < deadline, "child 1 not reaped within 5s");
            thread::sleep(Duration::from_millis(50));
        }
    }

    #[test]
    fn write_stdin_to_nonexistent_child_returns_error() {
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = Map::from([
            (atom("child_id"), int_term(99)),
            (atom("data"), binary_term(b"hello\n")),
        ]);
        let result = handle_write_stdin(&req, &children);
        assert!(result.is_err());
    }

    #[test]
    fn kill_nonexistent_child_returns_error() {
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = Map::from([(atom("child_id"), int_term(999))]);
        let result = handle_kill(&req, &children);
        assert!(result.is_err());
    }

    #[test]
    #[cfg(windows)]
    fn spawn_cmd_exit_zero_succeeds_on_windows() {
        use eetf::List;
        use std::time::{Duration, Instant};

        let writer: SharedWriter = Arc::new(Mutex::new(io::stdout()));
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));

        let args_term = Term::from(List::from(vec![binary_term(b"/c"), binary_term(b"exit 0")]));
        let env_term = Term::from(Map::from(HashMap::<Term, Term>::new()));
        let req = Term::from(Map::from([
            (atom("command"), atom("spawn")),
            (atom("child_id"), int_term(1)),
            (atom("executable"), binary_term(b"cmd")),
            (atom("args"), args_term),
            (atom("env"), env_term),
        ]));

        let result = handle_command(&req, &writer, &children);
        assert!(
            result.is_ok(),
            "spawn `cmd /c exit 0` should succeed: {result:?}"
        );

        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            {
                let map = children.lock().unwrap();
                if !map.contains_key(&1) {
                    break;
                }
            }
            assert!(Instant::now() < deadline, "child 1 not reaped within 5s");
            thread::sleep(Duration::from_millis(50));
        }
    }
}
