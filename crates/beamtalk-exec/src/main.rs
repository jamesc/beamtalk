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
//! - `{exit, ChildId, ExitCode}` — the last event for a child. A failed
//!   `spawn` gets `{exit, ChildId, -1}` (unless the id belongs to a live child).
//! - `{error, ChildId, Reason}` — a command naming `ChildId` failed (malformed
//!   `args`/`env`, unknown child, full stdin queue, …). `Reason` is a binary.
//!
//! # Lifecycle (BT-5195)
//!
//! - **Exit.** `exit` is sent once the child has exited and its stdout and
//!   stderr have reached EOF, or after [`DRAIN_GRACE`] if a grandchild still
//!   holds them. Output arriving later is dropped, so `exit` stays last. The
//!   child's entry (and so its process group or Job Object) stays registered,
//!   and killable, until both pipes close.
//! - **Stdin.** Writes go to a per-child writer thread through a queue of
//!   [`STDIN_QUEUE_DEPTH`] writes; when it is full the write fails with an
//!   `error` event. The command thread never blocks on a child.
//! - **Port death.** Unix children run in their own session. They are killed
//!   when the BEAM closes the port (stdin EOF), on SIGTERM/SIGHUP, and, when
//!   the port dies any other way, by the [`watchdog`] process and (Linux,
//!   direct child) `PR_SET_PDEATHSIG`. Windows children run in a kill-on-close
//!   Job Object, assigned before they run.

mod cat;
#[cfg(unix)]
mod watchdog;

use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::process::{Command, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, SyncSender, TrySendError};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use beamtalk_etf::{
    self as etf, atom, binary as binary_term, int_term, map_get, term_to_bytes, term_to_string,
    term_to_string_list_checked, term_to_string_map_checked, tuple3,
};
#[cfg(unix)]
use beamtalk_process::escalate::{Outcome, terminate_and_wait};
use clap::{ArgAction, Parser};
use eetf::{Map, Term};
use tracing::{debug, error, warn};
use tracing_subscriber::EnvFilter;

// ────────────────────────────────────────────────────────────────
// Tunables

/// Grace period between SIGTERM and SIGKILL.
const KILL_GRACE: Duration = Duration::from_secs(2);
/// Poll interval while waiting for a child to be reaped.
#[cfg_attr(windows, allow(dead_code))]
const KILL_POLL: Duration = Duration::from_millis(50);
/// How long after a child exits its output readers get to reach EOF before
/// `exit` is sent anyway (a grandchild may hold the pipes open for ever).
const DRAIN_GRACE: Duration = Duration::from_secs(2);
/// Pending `write_stdin` commands per child before writes are refused.
const STDIN_QUEUE_DEPTH: usize = 64;

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

type SharedWriter = Arc<Mutex<Box<dyn Write + Send>>>;

/// Encode an ETF term and write it as a `{packet, 4}` framed packet.
fn send_event(writer: &SharedWriter, event: &Term) {
    let mut buf = Vec::new();
    match event.encode(&mut buf) {
        Ok(()) => {
            if let Ok(mut out) = writer.lock()
                && let Err(e) = etf::write_packet(&mut *out, &buf)
            {
                error!("Failed to write event packet: {e}");
            }
        }
        Err(e) => {
            error!("Failed to encode event term: {e}");
        }
    }
}

/// `child_id` as the ETF integer the BEAM sent.
///
/// Ids arrive as `FixInteger`s (`i32`) and are decoded with `u32::try_from`,
/// so every id this port holds fits back into an `i32`.
fn child_id_term(child_id: u32) -> Term {
    int_term(i32::try_from(child_id).unwrap_or(i32::MAX))
}

fn stdout_event(child_id: u32, data: &[u8]) -> Term {
    tuple3(atom("stdout"), child_id_term(child_id), binary_term(data))
}

fn stderr_event(child_id: u32, data: &[u8]) -> Term {
    tuple3(atom("stderr"), child_id_term(child_id), binary_term(data))
}

fn exit_event(child_id: u32, code: i32) -> Term {
    tuple3(atom("exit"), child_id_term(child_id), int_term(code))
}

fn error_event(child_id: u32, reason: &str) -> Term {
    tuple3(
        atom("error"),
        child_id_term(child_id),
        binary_term(reason.as_bytes()),
    )
}

// ────────────────────────────────────────────────────────────────
// Child process state

/// Serialises one child's events so that `exit` is always its last.
///
/// Reader threads send output through [`Self::send`]; the reaper sends
/// `exit` through [`Self::send_exit`]. Both hold the lock while writing, so
/// no output event can follow `exit`. The gate's identity (`Arc::ptr_eq`)
/// also tells one incarnation of a `child_id` from a later one.
#[derive(Default)]
struct EventGate {
    exit_sent: Mutex<bool>,
}

impl EventGate {
    /// Send an output event, unless `exit` has already been sent.
    fn send(&self, writer: &SharedWriter, event: &Term) {
        if let Ok(exit_sent) = self.exit_sent.lock()
            && !*exit_sent
        {
            send_event(writer, event);
        }
    }

    /// Send `exit` (once) and close the gate.
    fn send_exit(&self, writer: &SharedWriter, child_id: u32, code: i32) {
        if let Ok(mut exit_sent) = self.exit_sent.lock()
            && !*exit_sent
        {
            *exit_sent = true;
            send_event(writer, &exit_event(child_id, code));
        }
    }

    fn exit_sent(&self) -> bool {
        self.exit_sent.lock().is_ok_and(|g| *g)
    }
}

/// Per-child management state stored in the shared map.
///
/// The `Child` handle is given to a reaper thread at spawn time. The entry
/// lives until the child has exited *and* both output pipes have closed, so a
/// grandchild holding the pipes can still be killed through it.
struct ChildEntry {
    /// Queue to the child's stdin writer thread; `None` after `close_stdin`.
    stdin: Option<SyncSender<Vec<u8>>>,
    /// This incarnation's event gate.
    gate: Arc<EventGate>,
    /// Process group ID (Unix only). After `setsid()`, pgid == child PID.
    #[cfg(unix)]
    pgid: libc::pid_t,
    /// Job Object (Windows only) holding the child's whole process tree.
    #[cfg(windows)]
    job: beamtalk_process::winjob::JobHandle,
}

type ChildMap = Arc<Mutex<HashMap<u32, ChildEntry>>>;

/// Everything a command handler needs.
#[derive(Clone)]
struct Port {
    writer: SharedWriter,
    children: ChildMap,
    /// How long a reaper waits for the output pipes before sending `exit`.
    drain: Duration,
    /// Told about every child process group (Unix; absent in unit tests).
    #[cfg(unix)]
    watchdog: Option<Arc<watchdog::Watchdog>>,
}

impl Port {
    fn new(writer: SharedWriter) -> Self {
        Self {
            writer,
            children: Arc::new(Mutex::new(HashMap::new())),
            drain: DRAIN_GRACE,
            #[cfg(unix)]
            watchdog: None,
        }
    }

    /// Remove `child_id` if it is still the incarnation behind `gate`, and
    /// tell the watchdog its group is done.
    fn remove_child(&self, child_id: u32, gate: &Arc<EventGate>) {
        let Ok(mut map) = self.children.lock() else {
            return;
        };
        if map
            .get(&child_id)
            .is_some_and(|e| Arc::ptr_eq(&e.gate, gate))
            && let Some(entry) = map.remove(&child_id)
        {
            self.forget(&entry);
        }
    }

    /// Tell the watchdog that `entry`'s group must no longer be signalled.
    #[cfg_attr(windows, allow(clippy::unused_self))]
    fn forget(&self, entry: &ChildEntry) {
        #[cfg(unix)]
        if let Some(watchdog) = &self.watchdog {
            watchdog.unregister(entry.pgid);
        }
        #[cfg(windows)]
        let _ = entry;
    }
}

/// Force-stop everything in `entry`'s process group or job, at once.
fn force_kill(entry: &ChildEntry) {
    #[cfg(unix)]
    // SAFETY: SIGKILL to a process group this port created.
    unsafe {
        libc::kill(-entry.pgid, libc::SIGKILL);
    }
    #[cfg(windows)]
    if let Err(e) = entry.job.terminate(1) {
        warn!("TerminateJobObject failed: {e}");
    }
}

// ────────────────────────────────────────────────────────────────
// Command handlers

/// A validated `spawn` request.
struct SpawnRequest {
    child_id: u32,
    executable: String,
    args: Vec<String>,
    env: HashMap<String, String>,
    /// `None` inherits the port's working directory.
    dir: Option<String>,
}

/// Decode a `spawn` request. A field that is present but malformed is an
/// error, never a silent default (BT-5195).
fn parse_spawn(request: &Map) -> Result<SpawnRequest, String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;
    let executable = map_get(request, "executable")
        .and_then(term_to_string)
        .ok_or("missing or invalid executable")?;
    let args = map_get(request, "args")
        .map(|t| term_to_string_list_checked(t, "args"))
        .transpose()?
        .unwrap_or_default();
    let env = map_get(request, "env")
        .map(|t| term_to_string_map_checked(t, "env"))
        .transpose()?
        .unwrap_or_default();
    // Empty string means "inherit cwd" — only call current_dir for non-empty paths.
    let dir = map_get(request, "dir")
        .map(|t| term_to_string(t).ok_or("dir must be a UTF-8 string"))
        .transpose()?
        .filter(|s| !s.is_empty());
    Ok(SpawnRequest {
        child_id,
        executable,
        args,
        env,
        dir,
    })
}

fn handle_spawn(request: &Map, port: &Port) -> Result<(), String> {
    let request = parse_spawn(request)?;
    claim_child_id(request.child_id, port)?;
    spawn_child(&request, port)
}

/// Make `child_id` free for a new child.
///
/// A live child keeps its id: the spawn is refused. An entry whose child has
/// already been reported as exited, but whose pipes a grandchild still holds,
/// is no longer the BEAM's child, so its group is killed and the id reused.
fn claim_child_id(child_id: u32, port: &Port) -> Result<(), String> {
    let mut map = port
        .children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    match map.get(&child_id) {
        None => Ok(()),
        Some(entry) if entry.gate.exit_sent() => {
            if let Some(entry) = map.remove(&child_id) {
                force_kill(&entry);
                port.forget(&entry);
            }
            Ok(())
        }
        Some(_) => Err(format!("child_id {child_id} already in use")),
    }
}

fn spawn_child(request: &SpawnRequest, port: &Port) -> Result<(), String> {
    let child_id = request.child_id;
    let mut cmd = Command::new(&request.executable);
    cmd.args(&request.args);
    cmd.envs(&request.env);
    if let Some(d) = &request.dir {
        cmd.current_dir(d);
    }
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    #[cfg(unix)]
    configure_unix_child(&mut cmd);

    // Windows: start suspended, join a kill-on-close job, then resume, so
    // nothing the child starts can escape the job (BT-5195). No console window.
    #[cfg(windows)]
    let (mut child, job) = {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        cmd.creation_flags(CREATE_NO_WINDOW | beamtalk_process::winjob::CREATE_SUSPENDED);
        beamtalk_process::winjob::spawn_in_job(&mut cmd)
            .map_err(|e| format!("spawn failed: {e}"))?
    };
    #[cfg(not(windows))]
    let mut child = cmd.spawn().map_err(|e| format!("spawn failed: {e}"))?;

    // After setsid(), the child's pgid equals its own pid.
    #[cfg(unix)]
    let pgid = libc::pid_t::try_from(child.id()).map_err(|e| format!("child pid: {e}"))?;
    #[cfg(unix)]
    if let Some(watchdog) = &port.watchdog {
        watchdog.register(pgid);
    }

    let stdin = child.stdin.take().map(|s| spawn_stdin_writer(child_id, s));
    let (Some(stdout), Some(stderr)) = (child.stdout.take(), child.stderr.take()) else {
        // Unreachable with `Stdio::piped()`; do not leave the child running.
        let _ = child.kill();
        let _ = child.wait();
        return Err("stdout/stderr not captured".to_string());
    };

    let gate = Arc::new(EventGate::default());
    if let Ok(mut map) = port.children.lock() {
        map.insert(
            child_id,
            ChildEntry {
                stdin,
                gate: Arc::clone(&gate),
                #[cfg(unix)]
                pgid,
                #[cfg(windows)]
                job,
            },
        );
    }

    let (done_tx, done_rx) = mpsc::channel();
    spawn_reader_thread(
        child_id,
        stdout,
        port,
        &gate,
        Stream::Stdout,
        done_tx.clone(),
    );
    spawn_reader_thread(child_id, stderr, port, &gate, Stream::Stderr, done_tx);

    let port = port.clone();
    thread::spawn(move || {
        let exit_code = match child.wait() {
            Ok(status) => beamtalk_process::exit_code::shell_exit_code(status),
            Err(e) => {
                error!("wait() failed for child {child_id}: {e}");
                -1
            }
        };
        // Give the readers a bounded time to deliver the last of the output,
        // then report the exit whether or not a grandchild holds the pipes.
        let mut open_pipes = 2;
        drain_readers(&done_rx, &mut open_pipes, Some(port.drain));
        gate.send_exit(&port.writer, child_id, exit_code);
        // Keep the entry, so the group stays killable, until the pipes close.
        drain_readers(&done_rx, &mut open_pipes, None);
        port.remove_child(child_id, &gate);
    });

    Ok(())
}

/// Wait for reader threads to finish, up to `limit` (`None`: no limit).
///
/// Each reader sends once when its pipe reaches EOF; a reader that panicked
/// drops its sender instead, which ends the wait once no sender is left.
fn drain_readers(done: &Receiver<()>, open_pipes: &mut usize, limit: Option<Duration>) {
    let deadline = limit.map(|d| Instant::now() + d);
    while *open_pipes > 0 {
        let received = match deadline {
            Some(deadline) => done.recv_timeout(deadline.saturating_duration_since(Instant::now())),
            None => done.recv().map_err(|_| RecvTimeoutError::Disconnected),
        };
        match received {
            Ok(()) => *open_pipes -= 1,
            Err(RecvTimeoutError::Timeout) => return,
            Err(RecvTimeoutError::Disconnected) => {
                *open_pipes = 0;
            }
        }
    }
}

/// Start the thread that owns `stdin` and feeds it from a bounded queue.
///
/// A write that fails (the child closed its stdin or exited) ends the thread,
/// so later `write_stdin` commands are refused rather than queued. Dropping
/// the returned sender (`close_stdin`, or the entry going away) lets the
/// thread finish the queued writes and then close the pipe: EOF for the child.
fn spawn_stdin_writer(child_id: u32, stdin: impl Write + Send + 'static) -> SyncSender<Vec<u8>> {
    let (tx, rx) = mpsc::sync_channel::<Vec<u8>>(STDIN_QUEUE_DEPTH);
    thread::spawn(move || {
        let mut stdin = stdin;
        for data in rx {
            if let Err(e) = stdin.write_all(&data).and_then(|()| stdin.flush()) {
                debug!("stdin write failed for child {child_id}: {e}");
                return;
            }
        }
    });
    tx
}

#[derive(Clone, Copy)]
enum Stream {
    Stdout,
    Stderr,
}

/// Spawn a reader thread that forwards one output stream as events.
fn spawn_reader_thread<R: Read + Send + 'static>(
    child_id: u32,
    reader: R,
    port: &Port,
    gate: &Arc<EventGate>,
    stream: Stream,
    done: mpsc::Sender<()>,
) {
    let writer = Arc::clone(&port.writer);
    let gate = Arc::clone(gate);
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        let mut reader = reader;
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    let event = match stream {
                        Stream::Stdout => stdout_event(child_id, &buf[..n]),
                        Stream::Stderr => stderr_event(child_id, &buf[..n]),
                    };
                    gate.send(&writer, &event);
                }
                Err(e) => {
                    debug!("read error for child {child_id}: {e}");
                    break;
                }
            }
        }
        let _ = done.send(());
    });
}

/// Unix child setup, run in the forked child before `exec`.
///
/// - `setsid`: the child leads a new session and process group, so the whole
///   group can be signalled and a terminal signal aimed at the BEAM does not
///   reach it.
/// - Linux: `PR_SET_PDEATHSIG(SIGKILL)`, so the child dies with the port even
///   if the port is killed with SIGKILL. The signal fires when the *thread* that forked
///   exits, which is why children are spawned only on the command thread.
#[cfg(unix)]
fn configure_unix_child(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    #[cfg(target_os = "linux")]
    // SAFETY: getpid(2) has no preconditions.
    let parent = unsafe { libc::getpid() };
    // SAFETY: the closure calls only async-signal-safe functions (setsid,
    // prctl, getppid) and allocates nothing.
    unsafe {
        cmd.pre_exec(move || {
            if libc::setsid() < 0 {
                return Err(io::Error::last_os_error());
            }
            #[cfg(target_os = "linux")]
            {
                if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) < 0 {
                    return Err(io::Error::last_os_error());
                }
                // The port died between fork and prctl: the signal will never
                // come, so do not start.
                if libc::getppid() != parent {
                    return Err(io::Error::from_raw_os_error(libc::ESRCH));
                }
            }
            Ok(())
        });
    }
}

/// Whether `child_id` is still registered as the incarnation behind `gate`.
#[cfg(unix)]
fn still_registered(children: &ChildMap, child_id: u32, gate: &Arc<EventGate>) -> bool {
    children
        .lock()
        .is_ok_and(|m| m.get(&child_id).is_some_and(|e| Arc::ptr_eq(&e.gate, gate)))
}

fn handle_kill(request: &Map, port: &Port) -> Result<(), String> {
    handle_kill_with(request, port, KILL_GRACE, thread::sleep)
}

/// Unix kill: SIGTERM now, SIGKILL after `grace` on a helper thread. `wait`
/// is how the helper sleeps between polls (injected so tests can prove that
/// nothing waits on the command thread). Windows terminates the Job Object at
/// once and ignores both.
fn handle_kill_with(
    request: &Map,
    port: &Port,
    grace: Duration,
    wait: fn(Duration),
) -> Result<(), String> {
    #[cfg(windows)]
    let _ = (grace, wait);
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    let map = port
        .children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    let entry = map
        .get(&child_id)
        .ok_or_else(|| format!("child {child_id} not found"))?;

    #[cfg(unix)]
    {
        let (pgid, gate) = (entry.pgid, Arc::clone(&entry.gate));
        drop(map);
        // SIGTERM to the whole process group. The group outlives the child
        // while a grandchild holds the pipes, so this reaches it too.
        // SAFETY: kill(2) to a process group this port created.
        let rc = unsafe { libc::kill(-pgid, libc::SIGTERM) };
        if rc < 0 {
            let err = io::Error::last_os_error();
            if err.raw_os_error() == Some(libc::ESRCH) {
                return Ok(()); // Process group already gone.
            }
            warn!("SIGTERM to pgid -{pgid} failed: {err}");
        }

        // Escalate to SIGKILL on a helper thread: waiting here would freeze
        // every other command.
        let children = Arc::clone(&port.children);
        thread::spawn(move || {
            let outcome = terminate_and_wait(
                grace,
                KILL_POLL,
                wait,
                || {}, // SIGTERM already sent above.
                || !still_registered(&children, child_id, &gate),
            );
            // Re-check so a reused child_id (a new child) is never signalled.
            if outcome == Outcome::GraceExpired && still_registered(&children, child_id, &gate) {
                // SAFETY: SIGKILL to a process group this port created.
                unsafe { libc::kill(-pgid, libc::SIGKILL) };
            }
        });
    }

    #[cfg(windows)]
    if let Err(e) = entry.job.terminate(1) {
        warn!("TerminateJobObject failed for child {child_id}: {e}");
    }

    Ok(())
}

fn handle_write_stdin(request: &Map, port: &Port) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;
    let data = map_get(request, "data")
        .and_then(term_to_bytes)
        .ok_or("missing or invalid data")?;

    let map = port
        .children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    let entry = map
        .get(&child_id)
        .ok_or_else(|| format!("child {child_id} not found"))?;
    let queue = entry
        .stdin
        .as_ref()
        .ok_or("stdin already closed for this child")?;
    // Never blocks: the writer thread does the (possibly blocking) I/O.
    queue.try_send(data).map_err(|e| match e {
        TrySendError::Full(_) => format!(
            "stdin queue full for child {child_id}: {STDIN_QUEUE_DEPTH} writes pending, \
             the child is not reading its input"
        ),
        TrySendError::Disconnected(_) => {
            format!("stdin closed for child {child_id}: the child no longer reads its input")
        }
    })
}

fn handle_close_stdin(request: &Map, port: &Port) -> Result<(), String> {
    let child_id = map_get(request, "child_id")
        .and_then(term_to_child_id)
        .ok_or("missing or invalid child_id")?;

    let mut map = port
        .children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    let entry = map
        .get_mut(&child_id)
        .ok_or_else(|| format!("child {child_id} not found"))?;
    // Dropping the queue lets the writer thread finish the queued writes and
    // then close the pipe, sending EOF to the child.
    entry.stdin = None;
    Ok(())
}

fn handle_command(term: &Term, port: &Port) -> Result<(), String> {
    let Term::Map(map) = term else {
        return Err("command must be a map".to_string());
    };

    let command = match map_get(map, "command") {
        Some(Term::Atom(a)) => a.name.as_str(),
        _ => return Err("missing or invalid 'command' field".to_string()),
    };

    match command {
        "spawn" => handle_spawn(map, port),
        "kill" => handle_kill(map, port),
        "write_stdin" => handle_write_stdin(map, port),
        "close_stdin" => handle_close_stdin(map, port),
        _ => Err(format!("unknown command: {command}")),
    }
}

/// The `child_id` a command names, if it names a valid one.
fn command_child_id(term: &Term) -> Option<u32> {
    let Term::Map(map) = term else { return None };
    map_get(map, "child_id").and_then(term_to_child_id)
}

/// The `child_id` that needs a synthetic exit event after `term` failed, if any.
///
/// Only a failed `spawn` qualifies, and only when no live child holds the id:
/// a duplicate-id spawn is rejected while the id belongs to a different,
/// still-running child, and an exit event for it would tell the BEAM that
/// child had died.
fn failed_spawn_child_id(term: &Term, children: &ChildMap) -> Option<u32> {
    let Term::Map(map) = term else { return None };
    if !matches!(map_get(map, "command"), Some(Term::Atom(a)) if a.name == "spawn") {
        return None;
    }
    let child_id = command_child_id(term)?;
    let live = children.lock().is_ok_and(|m| m.contains_key(&child_id));
    (!live).then_some(child_id)
}

/// Read and run commands from `input` until EOF, then kill every child.
///
/// A failed command naming a `child_id` is answered with an `error` event;
/// a failed `spawn` is also answered with `exit -1` (see
/// [`failed_spawn_child_id`]).
fn run_command_loop(input: &mut impl Read, port: &Port) {
    loop {
        let packet = match etf::read_packet(input) {
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

        if let Err(e) = handle_command(&term, port) {
            warn!("Command error: {e}");
            if let Some(child_id) = command_child_id(&term) {
                send_event(&port.writer, &error_event(child_id, &e));
            }
            if let Some(child_id) = failed_spawn_child_id(&term, &port.children) {
                send_event(&port.writer, &exit_event(child_id, -1));
            }
        }
    }

    // BEAM closed the port — terminate all running children.
    kill_all_children(&port.children);
}

/// Send SIGTERM to all running process groups, wait up to 2 s, then SIGKILL.
fn kill_all_children(children: &ChildMap) {
    #[cfg(unix)]
    kill_all_children_within(children, KILL_GRACE, thread::sleep);

    #[cfg(windows)]
    kill_all_children_windows(children);
}

/// Unix: SIGTERM every group, wait up to `grace` for the reapers to drain the
/// map (each entry goes once its child has exited and its pipes have closed),
/// then SIGKILL the survivors.
#[cfg(unix)]
fn kill_all_children_within(children: &ChildMap, grace: Duration, wait: fn(Duration)) {
    let pgids = || -> Vec<libc::pid_t> {
        children
            .lock()
            .map(|m| m.values().map(|e| e.pgid).collect())
            .unwrap_or_default()
    };
    let initial = pgids();
    if initial.is_empty() {
        return; // Nothing to wait for — exit promptly.
    }
    let outcome = terminate_and_wait(
        grace,
        KILL_POLL,
        wait,
        || {
            for pgid in &initial {
                // SAFETY: SIGTERM to a process group this port created.
                unsafe { libc::kill(-pgid, libc::SIGTERM) };
            }
        },
        || children.lock().is_ok_and(|m| m.is_empty()),
    );
    if outcome == Outcome::GraceExpired {
        for pgid in pgids() {
            // SAFETY: SIGKILL to a process group this port created.
            unsafe { libc::kill(-pgid, libc::SIGKILL) };
        }
    }
}

/// Windows: terminate all Job Objects to kill all process trees.
#[cfg(windows)]
fn kill_all_children_windows(children: &ChildMap) {
    if let Ok(map) = children.lock() {
        for (child_id, entry) in map.iter() {
            if let Err(e) = entry.job.terminate(1) {
                warn!("TerminateJobObject failed for child {child_id}: {e}");
            }
        }
    }
}

// ────────────────────────────────────────────────────────────────
// Termination signals (Unix)

/// The signals that stop the port politely: SIGTERM, and SIGHUP.
#[cfg(unix)]
fn termination_signals() -> libc::sigset_t {
    // SAFETY: sigemptyset/sigaddset initialise and fill a local sigset_t.
    unsafe {
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&raw mut set);
        libc::sigaddset(&raw mut set, libc::SIGTERM);
        libc::sigaddset(&raw mut set, libc::SIGHUP);
        set
    }
}

/// Block the termination signals in this thread. Called first thing in
/// `main`, so every thread started later inherits the mask and only the
/// [`spawn_signal_thread`] thread receives them (via `sigwait`). Children do
/// not inherit it: `std::process::Command` resets the mask before `exec`.
#[cfg(unix)]
fn block_termination_signals() {
    let set = termination_signals();
    // SAFETY: pthread_sigmask with a valid set and a null old-set pointer.
    unsafe { libc::pthread_sigmask(libc::SIG_BLOCK, &raw const set, std::ptr::null_mut()) };
}

/// On SIGTERM or SIGHUP, kill every child (as at stdin EOF) and exit with
/// `128 + signal`.
#[cfg(unix)]
fn spawn_signal_thread(children: ChildMap) {
    thread::spawn(move || {
        let set = termination_signals();
        let mut sig: libc::c_int = 0;
        // SAFETY: sigwait with a valid set and out-pointer; the set is blocked
        // in every thread (see block_termination_signals).
        if unsafe { libc::sigwait(&raw const set, &raw mut sig) } != 0 {
            return;
        }
        debug!("received signal {sig}; stopping children");
        kill_all_children(&children);
        std::process::exit(128 + sig);
    });
}

// ────────────────────────────────────────────────────────────────
// CLI

#[derive(Debug, Parser)]
#[command(
    name = "beamtalk-exec",
    about = "Beamtalk subprocess management port (ADR 0051)",
    version = env!("BEAMTALK_BUILD_ID")
)]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,

    /// Run as a cat replacement: copy stdin to stdout byte for byte, flushing
    /// after every line. Used on Windows where pipe buffering prevents
    /// interactive I/O with regular programs.
    #[arg(long = "cat", hide = true)]
    cat_mode: bool,

    /// Run as the port's watchdog (Unix): kill the process groups named on
    /// stdin once stdin closes. Started by the port itself.
    #[arg(long = "watchdog", hide = true)]
    watchdog_mode: bool,
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

    if cli.cat_mode {
        cat::run();
    }
    #[cfg(unix)]
    if cli.watchdog_mode {
        watchdog::run(KILL_GRACE);
    }

    // Before any thread exists, so every thread inherits the mask.
    #[cfg(unix)]
    block_termination_signals();

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

    #[cfg_attr(not(unix), allow(unused_mut))]
    let mut port = Port::new(Arc::new(Mutex::new(Box::new(io::stdout()))));

    #[cfg(unix)]
    {
        port.watchdog = std::env::current_exe()
            .and_then(|exe| watchdog::Watchdog::start(&exe))
            .inspect_err(|e| warn!("watchdog not started: {e}"))
            .ok()
            .map(Arc::new);
        spawn_signal_thread(Arc::clone(&port.children));
    }

    // Children are spawned on this (the main) thread only: Linux's
    // PR_SET_PDEATHSIG fires when the forking thread exits.
    run_command_loop(&mut io::stdin().lock(), &port);
}

// ────────────────────────────────────────────────────────────────
// Tests

#[cfg(test)]
mod tests {
    use super::*;
    use eetf::List;
    use serial_test::serial;
    #[cfg(unix)]
    use std::path::Path;

    // NOTE: Packet framing tests are in beamtalk-etf.

    // ── Test harness ────────────────────────────────────────────

    /// Upper bound on any wait below. A hang guard, not a timing assumption:
    /// every wait ends as soon as its condition holds.
    const HANG_GUARD: Duration = Duration::from_secs(30);

    /// A writer that keeps every byte the port writes.
    #[derive(Clone, Default)]
    struct Capture(Arc<Mutex<Vec<u8>>>);

    impl Write for Capture {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.0.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    impl Capture {
        /// Every event written so far, decoded.
        fn events(&self) -> Vec<Term> {
            let bytes = self.0.lock().unwrap().clone();
            let mut cursor = io::Cursor::new(bytes);
            let mut events = Vec::new();
            while let Ok(Some(packet)) = etf::read_packet(&mut cursor) {
                events.push(Term::decode(io::Cursor::new(packet)).expect("decode event"));
            }
            events
        }

        /// Wait until some event matches `pred`, and return all events.
        fn wait_for(&self, what: &str, pred: impl Fn(&Term) -> bool) -> Vec<Term> {
            let deadline = Instant::now() + HANG_GUARD;
            loop {
                let events = self.events();
                if events.iter().any(&pred) {
                    return events;
                }
                assert!(
                    Instant::now() < deadline,
                    "no {what} event within {HANG_GUARD:?}; events: {events:?}"
                );
                thread::sleep(Duration::from_millis(10)); // sleep: poll the capture
            }
        }

        fn wait_for_exit(&self, child_id: i32) -> Vec<Term> {
            self.wait_for("exit", |e| is_event(e, "exit", child_id))
        }
    }

    fn test_port() -> (Port, Capture) {
        let capture = Capture::default();
        let port = Port::new(Arc::new(Mutex::new(Box::new(capture.clone()))));
        (port, capture)
    }

    /// `(tag, child_id, payload)` of an event tuple.
    fn parts(event: &Term) -> (&str, &Term, &Term) {
        let Term::Tuple(t) = event else {
            panic!("event is not a tuple: {event:?}")
        };
        let Term::Atom(tag) = &t.elements[0] else {
            panic!("event tag is not an atom: {event:?}")
        };
        (tag.name.as_str(), &t.elements[1], &t.elements[2])
    }

    fn is_event(event: &Term, tag: &str, child_id: i32) -> bool {
        let (t, id, _) = parts(event);
        t == tag && *id == int_term(child_id)
    }

    /// Concatenated `stream` payloads for `child_id`.
    fn output(events: &[Term], stream: &str, child_id: i32) -> Vec<u8> {
        events
            .iter()
            .filter(|e| is_event(e, stream, child_id))
            .flat_map(|e| term_to_bytes(parts(e).2).expect("binary payload"))
            .collect()
    }

    fn exit_code_of(events: &[Term], child_id: i32) -> Term {
        let exit = events
            .iter()
            .find(|e| is_event(e, "exit", child_id))
            .expect("exit event");
        parts(exit).2.clone()
    }

    fn spawn_term(child_id: i32, executable: &str, args: &[&str]) -> Term {
        let args: Vec<Term> = args.iter().map(|a| binary_term(a.as_bytes())).collect();
        Term::from(Map::from([
            (atom("command"), atom("spawn")),
            (atom("child_id"), int_term(child_id)),
            (atom("executable"), binary_term(executable.as_bytes())),
            (atom("args"), Term::from(List::from(args))),
            (
                atom("env"),
                Term::from(Map::from(HashMap::<Term, Term>::new())),
            ),
        ]))
    }

    fn command(name: &str, child_id: i32, extra: Vec<(Term, Term)>) -> Term {
        let mut fields = vec![
            (atom("command"), atom(name)),
            (atom("child_id"), int_term(child_id)),
        ];
        fields.extend(extra);
        Term::from(Map::from(fields.into_iter().collect::<HashMap<_, _>>()))
    }

    fn write_stdin_term(child_id: i32, data: &[u8]) -> Term {
        command(
            "write_stdin",
            child_id,
            vec![(atom("data"), binary_term(data))],
        )
    }

    fn is_registered(port: &Port, child_id: u32) -> bool {
        port.children.lock().unwrap().contains_key(&child_id)
    }

    fn wait_until_unregistered(port: &Port, child_id: u32) {
        let deadline = Instant::now() + HANG_GUARD;
        while is_registered(port, child_id) {
            assert!(Instant::now() < deadline, "child {child_id} not removed");
            thread::sleep(Duration::from_millis(10)); // sleep: poll for the reaper thread
        }
    }

    /// Run `f` on another thread and fail if it does not return in time: a
    /// command that blocks the command thread is the bug under test.
    fn must_return<T: Send + 'static>(what: &str, f: impl FnOnce() -> T + Send + 'static) -> T {
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let _ = tx.send(f());
        });
        rx.recv_timeout(HANG_GUARD)
            .unwrap_or_else(|_| panic!("{what} blocked the command thread"))
    }

    /// Whether `pid` is a running (not zombie) process.
    #[cfg(unix)]
    fn process_running(pid: libc::pid_t) -> bool {
        let out = Command::new("ps")
            .args(["-o", "stat=", "-p", &pid.to_string()])
            .output()
            .expect("run ps");
        let stat = String::from_utf8_lossy(&out.stdout);
        let stat = stat.trim();
        !stat.is_empty() && !stat.starts_with('Z')
    }

    // ── ETF event encoding ──────────────────────────────────────

    #[test]
    fn stdout_event_encodes_correctly() {
        let event = stdout_event(42, b"hello");
        let mut buf = Vec::new();
        event.encode(&mut buf).unwrap();
        let decoded = Term::decode(io::Cursor::new(buf)).unwrap();
        assert_eq!(
            decoded,
            tuple3(atom("stdout"), int_term(42), binary_term(b"hello"))
        );
    }

    #[test]
    fn stderr_event_encodes_correctly() {
        let event = stderr_event(7, b"err");
        let (tag, id, _) = parts(&event);
        assert_eq!((tag, id), ("stderr", &int_term(7)));
    }

    #[test]
    fn exit_event_encodes_correctly() {
        assert_eq!(
            exit_event(1, 0),
            tuple3(atom("exit"), int_term(1), int_term(0))
        );
    }

    #[test]
    fn error_event_encodes_correctly() {
        assert_eq!(
            error_event(3, "bad"),
            tuple3(atom("error"), int_term(3), binary_term(b"bad"))
        );
    }

    // ── Command parsing ─────────────────────────────────────────

    #[test]
    fn handle_command_not_a_map_returns_error() {
        let (port, _) = test_port();
        assert!(handle_command(&atom("not_a_map"), &port).is_err());
    }

    #[test]
    fn handle_command_unknown_command_returns_error() {
        let (port, _) = test_port();
        let req = Term::from(Map::from([(atom("command"), atom("unknown_cmd"))]));
        assert!(handle_command(&req, &port).is_err());
    }

    #[test]
    fn handle_command_missing_command_returns_error() {
        let (port, _) = test_port();
        let req = Term::from(Map::from([(atom("other"), atom("value"))]));
        assert!(handle_command(&req, &port).is_err());
    }

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

    // ── Malformed spawn requests (BT-5195 item 3) ───────────────

    /// Before BT-5195 a bad element made the whole list default to empty and
    /// the child was spawned with no arguments (or no environment).
    #[test]
    fn malformed_args_or_env_are_rejected_without_spawning() {
        let (port, _) = test_port();
        let mut bad_args = spawn_term(40, "/bin/sh", &[]);
        let mut bad_env = spawn_term(41, "/bin/sh", &[]);
        let mut bad_dir = spawn_term(42, "/bin/sh", &[]);
        let Term::Map(m) = &mut bad_args else {
            unreachable!()
        };
        m.map.insert(
            atom("args"),
            Term::from(List::from(vec![binary_term(b"-c"), atom("not_a_binary")])),
        );
        let Term::Map(m) = &mut bad_env else {
            unreachable!()
        };
        m.map.insert(
            atom("env"),
            Term::from(Map::from([(binary_term(b"K"), binary_term(&[0xff]))])),
        );
        let Term::Map(m) = &mut bad_dir else {
            unreachable!()
        };
        m.map.insert(atom("dir"), int_term(5));

        let err = handle_command(&bad_args, &port).unwrap_err();
        assert!(err.contains("args element 1"), "{err}");
        let err = handle_command(&bad_env, &port).unwrap_err();
        assert!(err.contains("env value for 'K'"), "{err}");
        let err = handle_command(&bad_dir, &port).unwrap_err();
        assert!(err.contains("dir"), "{err}");
        assert!(port.children.lock().unwrap().is_empty(), "nothing spawned");
    }

    #[test]
    fn absent_args_and_env_default_to_empty() {
        let req = Map::from([
            (atom("child_id"), int_term(1)),
            (atom("executable"), binary_term(b"x")),
        ]);
        let parsed = parse_spawn(&req).expect("parse");
        assert!(parsed.args.is_empty() && parsed.env.is_empty() && parsed.dir.is_none());
    }

    // ── Command loop (BT-5195 item 8) ───────────────────────────

    fn packet(term: &Term) -> Vec<u8> {
        let mut body = Vec::new();
        term.encode(&mut body).unwrap();
        let mut framed = Vec::new();
        etf::write_packet(&mut framed, &body).unwrap();
        framed
    }

    /// Commands that fail get an `error` event naming the child; a failed
    /// spawn also gets `exit -1`; an undecodable packet is skipped; EOF ends
    /// the loop.
    #[test]
    fn command_loop_answers_failures_and_stops_at_eof() {
        let (port, capture) = test_port();
        let mut bad_spawn = spawn_term(5, "/bin/sh", &[]);
        let Term::Map(m) = &mut bad_spawn else {
            unreachable!()
        };
        m.map.insert(atom("args"), atom("not_a_list"));

        let mut input = Vec::new();
        input.extend(packet(&bad_spawn));
        let mut garbage = Vec::new();
        etf::write_packet(&mut garbage, b"\x83garbage").unwrap();
        input.extend(garbage);
        input.extend(packet(&command("kill", 6, vec![])));
        input.extend(packet(&write_stdin_term(7, b"x")));
        input.extend(packet(&command("bogus", 8, vec![])));
        input.extend(packet(&atom("not_a_map")));

        run_command_loop(&mut io::Cursor::new(input), &port);

        let events = capture.events();
        let summary: Vec<(String, Term)> = events
            .iter()
            .map(|e| {
                let (tag, id, _) = parts(e);
                (tag.to_string(), id.clone())
            })
            .collect();
        assert_eq!(
            summary,
            vec![
                ("error".to_string(), int_term(5)),
                ("exit".to_string(), int_term(5)),
                ("error".to_string(), int_term(6)),
                ("error".to_string(), int_term(7)),
                ("error".to_string(), int_term(8)),
            ]
        );
        assert_eq!(exit_code_of(&events, 5), int_term(-1));
        let reason = term_to_string(parts(&events[0]).2).unwrap();
        assert!(reason.contains("args"), "{reason}");
    }

    /// A child spawned through the loop reports its output and exit; EOF on
    /// the command stream then returns.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn command_loop_runs_a_child_to_completion() {
        let (port, capture) = test_port();
        let (reader, mut writer) = io::pipe().expect("pipe");
        let loop_port = port.clone();
        let handle = thread::spawn(move || {
            let mut reader = reader;
            run_command_loop(&mut reader, &loop_port);
        });
        writer
            .write_all(&packet(&spawn_term(9, "/bin/sh", &["-c", "echo hello"])))
            .unwrap();
        let events = capture.wait_for_exit(9);
        assert_eq!(output(&events, "stdout", 9), b"hello\n");
        assert_eq!(exit_code_of(&events, 9), int_term(0));
        assert!(
            is_event(events.last().unwrap(), "exit", 9),
            "exit is the last event: {events:?}"
        );
        drop(writer);
        must_return("loop at EOF", move || handle.join().unwrap());
    }

    // ── spawn / kill integration (Unix) ─────────────────────────

    #[test]
    #[cfg(unix)]
    #[serial]
    fn spawn_true_succeeds_and_exits() {
        let (port, capture) = test_port();
        handle_command(&spawn_term(1, "/usr/bin/env", &["true"]), &port).expect("spawn `env true`");
        let events = capture.wait_for_exit(1);
        assert_eq!(exit_code_of(&events, 1), int_term(0));
        wait_until_unregistered(&port, 1);
    }

    #[test]
    #[cfg(unix)]
    #[serial]
    fn spawn_with_stderr_and_working_dir() {
        let (port, capture) = test_port();
        let mut req = spawn_term(11, "/bin/sh", &["-c", "pwd; echo err >&2"]);
        let dir = std::env::temp_dir().canonicalize().unwrap();
        let Term::Map(m) = &mut req else {
            unreachable!()
        };
        m.map
            .insert(atom("dir"), binary_term(dir.to_str().unwrap().as_bytes()));
        handle_command(&req, &port).expect("spawn");
        let events = capture.wait_for_exit(11);
        assert_eq!(output(&events, "stderr", 11), b"err\n");
        let pwd = String::from_utf8(output(&events, "stdout", 11)).unwrap();
        assert_eq!(
            Path::new(pwd.trim()).canonicalize().unwrap(),
            dir,
            "child ran in `dir`"
        );
    }

    #[test]
    fn write_stdin_to_nonexistent_child_returns_error() {
        let (port, _) = test_port();
        let req = Map::from([
            (atom("child_id"), int_term(99)),
            (atom("data"), binary_term(b"hello\n")),
        ]);
        assert!(handle_write_stdin(&req, &port).is_err());
    }

    #[test]
    fn kill_nonexistent_child_returns_error() {
        let (port, _) = test_port();
        let req = Map::from([(atom("child_id"), int_term(999))]);
        assert!(handle_kill(&req, &port).is_err());
    }

    #[test]
    #[cfg(unix)]
    #[serial]
    fn spawn_duplicate_child_id_is_rejected() {
        let (port, capture) = test_port();
        handle_command(&spawn_term(20, "/bin/sleep", &["60"]), &port)
            .expect("first spawn must succeed");
        let err = handle_command(&spawn_term(20, "/usr/bin/env", &["true"]), &port)
            .expect_err("duplicate child_id must be rejected");
        assert!(err.contains("already in use"), "{err}");
        assert_eq!(
            failed_spawn_child_id(&spawn_term(20, "x", &[]), &port.children),
            None,
            "no exit event for the live child"
        );

        handle_kill(&Map::from([(atom("child_id"), int_term(20))]), &port)
            .expect("kill must succeed");
        capture.wait_for_exit(20);
        wait_until_unregistered(&port, 20);
    }

    // ── Grandchild holding the pipes (BT-5195 item 1) ───────────

    /// Before BT-5195 the reaper joined the readers before sending `exit`, so
    /// a grandchild holding stdout meant `exit` never came, and the entry was
    /// already gone so the grandchild could not be killed.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn exit_is_sent_while_a_grandchild_holds_the_pipes() {
        let (mut port, capture) = test_port();
        port.drain = Duration::from_millis(100);
        // The shell prints its grandchild's pid and exits; `sleep` keeps the
        // shell's stdout and stderr open.
        handle_command(
            &spawn_term(50, "/bin/sh", &["-c", "sleep 300 & echo $!"]),
            &port,
        )
        .expect("spawn");
        let events = capture.wait_for_exit(50);
        assert_eq!(exit_code_of(&events, 50), int_term(0));
        let grandchild: libc::pid_t = String::from_utf8(output(&events, "stdout", 50))
            .unwrap()
            .trim()
            .parse()
            .expect("grandchild pid");
        assert!(process_running(grandchild));

        // Still registered, so the grandchild's group is still killable.
        assert!(is_registered(&port, 50));
        handle_kill(&Map::from([(atom("child_id"), int_term(50))]), &port)
            .expect("kill after exit");
        wait_until_unregistered(&port, 50);
        let deadline = Instant::now() + HANG_GUARD;
        while process_running(grandchild) {
            assert!(Instant::now() < deadline, "grandchild survived kill");
            thread::sleep(Duration::from_millis(10)); // sleep: poll for the grandchild to die
        }
        assert!(
            is_event(capture.events().last().unwrap(), "exit", 50),
            "nothing follows exit"
        );
    }

    /// Shutdown still signals a group whose leader has exited.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn kill_all_children_reaches_a_lingering_grandchild() {
        let (mut port, capture) = test_port();
        port.drain = Duration::ZERO;
        handle_command(&spawn_term(51, "/bin/sh", &["-c", "sleep 300 &"]), &port).expect("spawn");
        capture.wait_for_exit(51);
        assert!(is_registered(&port, 51));
        kill_all_children_within(&port.children, HANG_GUARD, thread::sleep);
        assert!(port.children.lock().unwrap().is_empty());
    }

    /// An id whose child has been reported as exited can be reused; the old
    /// group is killed and its late output never reaches the new child.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn an_exited_childs_id_can_be_reused() {
        let (mut port, capture) = test_port();
        port.drain = Duration::ZERO;
        handle_command(&spawn_term(52, "/bin/sh", &["-c", "sleep 300 &"]), &port)
            .expect("first spawn");
        capture.wait_for_exit(52);
        handle_command(&spawn_term(52, "/bin/sh", &["-c", "echo second"]), &port)
            .expect("reuse after exit");
        let deadline = Instant::now() + HANG_GUARD;
        loop {
            let events = capture.events();
            let exits = events.iter().filter(|e| is_event(e, "exit", 52)).count();
            if exits == 2 {
                assert_eq!(output(&events, "stdout", 52), b"second\n");
                break;
            }
            assert!(Instant::now() < deadline, "second exit missing: {events:?}");
            thread::sleep(Duration::from_millis(10)); // sleep: poll the capture
        }
        wait_until_unregistered(&port, 52);
    }

    // ── Stdin off the command thread (BT-5195 item 2) ───────────

    /// Before BT-5195 the first write past the pipe buffer blocked the command
    /// thread for ever, and with it `kill` and every other child.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn a_child_that_never_reads_cannot_block_the_command_thread() {
        let (port, capture) = test_port();
        handle_command(&spawn_term(60, "/bin/sleep", &["300"]), &port).expect("spawn");

        let chunk = vec![b'x'; 1 << 20]; // far beyond any pipe buffer
        let writer_port = port.clone();
        let refused = must_return("write_stdin", move || {
            (0..STDIN_QUEUE_DEPTH + 2)
                .map(|_| handle_command(&write_stdin_term(60, &chunk), &writer_port))
                .find_map(Result::err)
        });
        let err = refused.expect("a full queue refuses the write");
        assert!(err.contains("stdin queue full"), "{err}");

        let kill_port = port.clone();
        must_return("kill", move || {
            handle_command(&command("kill", 60, vec![]), &kill_port)
        })
        .expect("kill");
        let events = capture.wait_for_exit(60);
        assert_eq!(exit_code_of(&events, 60), int_term(128 + libc::SIGTERM));
    }

    /// A large write reaches a child that reads, and `close_stdin` delivers
    /// EOF after the queued data.
    #[test]
    #[cfg(unix)]
    #[serial]
    fn a_large_stdin_write_reaches_the_child() {
        let (port, capture) = test_port();
        handle_command(&spawn_term(61, "/bin/cat", &[]), &port).expect("spawn");
        let data: Vec<u8> = (0..(4u32 << 20))
            .map(|i| u8::try_from(i % 251).unwrap())
            .collect();
        handle_command(&write_stdin_term(61, &data[..1 << 20]), &port).expect("write 1");
        handle_command(&write_stdin_term(61, &data[1 << 20..]), &port).expect("write 2");
        handle_command(&command("close_stdin", 61, vec![]), &port).expect("close");
        let events = capture.wait_for_exit(61);
        assert_eq!(exit_code_of(&events, 61), int_term(0));
        assert!(
            output(&events, "stdout", 61) == data,
            "cat echoed every byte"
        );
        let err = handle_command(&write_stdin_term(61, b"late"), &port);
        assert!(err.is_err(), "write after close/exit is refused");
    }

    #[test]
    #[cfg(unix)]
    #[serial]
    fn write_to_a_child_that_closed_stdin_is_refused() {
        let (port, capture) = test_port();
        // `exec 0<&-` closes stdin; the child then waits to be killed.
        handle_command(
            &spawn_term(
                62,
                "/bin/sh",
                &["-c", "exec 0<&-; echo closed; exec sleep 300"],
            ),
            &port,
        )
        .expect("spawn");
        capture.wait_for("stdout", |e| is_event(e, "stdout", 62));
        let deadline = Instant::now() + HANG_GUARD;
        let err = loop {
            // The first write fails in the writer thread (EPIPE), which then
            // stops; the next write is refused here.
            if let Err(e) = handle_command(&write_stdin_term(62, b"x"), &port) {
                break e;
            }
            assert!(Instant::now() < deadline, "writes never refused");
            thread::sleep(Duration::from_millis(10)); // sleep: let the writer thread see EPIPE
        };
        assert!(err.contains("no longer reads"), "{err}");
        handle_command(&command("kill", 62, vec![]), &port).expect("kill");
        capture.wait_for_exit(62);
    }

    // ── Port death (BT-5195 item 4) ─────────────────────────────

    /// Linux: the child gets SIGKILL when the thread that spawned it (the
    /// port's command thread) dies, even if nothing else runs.
    #[test]
    #[cfg(target_os = "linux")]
    #[serial]
    fn linux_child_dies_with_the_spawning_thread() {
        let (port, capture) = test_port();
        let spawn_port = port.clone();
        thread::spawn(move || handle_command(&spawn_term(70, "/bin/sleep", &["300"]), &spawn_port))
            .join()
            .unwrap()
            .expect("spawn");
        let events = capture.wait_for_exit(70);
        assert_eq!(exit_code_of(&events, 70), int_term(128 + libc::SIGKILL));
    }

    // ── Shutdown / kill behaviour (BT-5028) ─────────────────────

    #[test]
    #[cfg(unix)]
    fn kill_all_children_with_no_children_does_not_wait() {
        // An hour-long grace would hang the test (not just slow it) if the
        // empty case waited, so no wall-clock assertion is needed.
        let (port, _) = test_port();
        kill_all_children_within(&port.children, Duration::from_hours(1), |_| {
            panic!("an empty child map must not wait");
        });
    }

    #[test]
    #[cfg(unix)]
    fn failed_spawn_of_live_child_id_emits_no_exit_event() {
        let (port, _) = test_port();
        port.children.lock().unwrap().insert(
            7,
            ChildEntry {
                stdin: None,
                gate: Arc::default(),
                pgid: 1,
            },
        );
        let spawn = |id: i32| spawn_term(id, "x", &[]);
        assert_eq!(failed_spawn_child_id(&spawn(7), &port.children), None);
        assert_eq!(failed_spawn_child_id(&spawn(8), &port.children), Some(8));
        assert_eq!(
            failed_spawn_child_id(&command("kill", 8, vec![]), &port.children),
            None
        );
        // Never signal pgid 1.
        port.children.lock().unwrap().clear();
    }

    #[test]
    #[cfg(unix)]
    #[serial]
    fn kill_of_term_ignoring_child_returns_without_blocking_the_loop() {
        let (port, capture) = test_port();
        // SIG_IGN survives exec, so the child starts with SIGTERM ignored and
        // there is no window in which a `trap` is not yet installed. `#[serial]`
        // keeps other spawning tests from inheriting the ignored disposition.
        // SAFETY: signal(2) with SIG_IGN/SIG_DFL is async-signal-safe.
        unsafe { libc::signal(libc::SIGTERM, libc::SIG_IGN) };
        let spawned = handle_command(
            &spawn_term(30, "/bin/sh", &["-c", "while :; do sleep 1; done"]),
            &port,
        );
        // SAFETY: as above, restoring the default disposition.
        unsafe { libc::signal(libc::SIGTERM, libc::SIG_DFL) };
        spawned.expect("spawn must succeed");
        let pgid = port
            .children
            .lock()
            .unwrap()
            .get(&30)
            .expect("registered")
            .pgid;

        // Test threads are named; the escalation helper is not. Waiting on the
        // command thread (the old behaviour) therefore panics here.
        let kill = Map::from([(atom("child_id"), int_term(30))]);
        handle_kill_with(&kill, &port, Duration::from_hours(1), |d| {
            assert!(
                thread::current().name().is_none(),
                "handle_kill must not wait on the command thread"
            );
            thread::sleep(d);
        })
        .expect("kill must succeed");
        assert!(is_registered(&port, 30));

        // Cleanup: SIGKILL the group ourselves; the reaper then drains the map,
        // which also ends the helper thread's poll loop.
        // SAFETY: SIGKILL to the process group of a child we spawned.
        unsafe { libc::kill(-pgid, libc::SIGKILL) };
        capture.wait_for_exit(30);
        wait_until_unregistered(&port, 30);
    }

    // ── Windows ─────────────────────────────────────────────────

    #[test]
    #[cfg(windows)]
    #[serial]
    fn spawn_cmd_reports_output_and_exit_on_windows() {
        let (port, capture) = test_port();
        handle_command(&spawn_term(1, "cmd", &["/c", "echo hi&& exit 3"]), &port)
            .expect("spawn cmd");
        let events = capture.wait_for_exit(1);
        assert_eq!(exit_code_of(&events, 1), int_term(3));
        let stdout = String::from_utf8_lossy(&output(&events, "stdout", 1)).into_owned();
        assert!(stdout.contains("hi"), "{stdout:?}");
        wait_until_unregistered(&port, 1);
    }

    /// `kill` terminates the job, and with it a grandchild holding the pipes.
    #[test]
    #[cfg(windows)]
    #[serial]
    fn kill_terminates_the_job_on_windows() {
        let (mut port, capture) = test_port();
        port.drain = Duration::from_millis(100);
        handle_command(
            &spawn_term(2, "cmd", &["/c", "ping -n 300 127.0.0.1"]),
            &port,
        )
        .expect("spawn");
        handle_command(&command("kill", 2, vec![]), &port).expect("kill");
        let events = capture.wait_for_exit(2);
        assert_eq!(exit_code_of(&events, 2), int_term(1));
        wait_until_unregistered(&port, 2);
    }

    #[test]
    #[cfg(windows)]
    #[serial]
    fn a_child_that_never_reads_cannot_block_the_command_thread_on_windows() {
        let (port, capture) = test_port();
        handle_command(
            &spawn_term(3, "cmd", &["/c", "ping -n 300 127.0.0.1 >nul"]),
            &port,
        )
        .expect("spawn");
        let chunk = vec![b'x'; 1 << 20];
        let writer_port = port.clone();
        let refused = must_return("write_stdin", move || {
            (0..STDIN_QUEUE_DEPTH + 2)
                .map(|_| handle_command(&write_stdin_term(3, &chunk), &writer_port))
                .find_map(Result::err)
        });
        assert!(refused.is_some(), "a full queue refuses the write");
        let kill_port = port.clone();
        must_return("kill", move || {
            handle_command(&command("kill", 3, vec![]), &kill_port)
        })
        .expect("kill");
        capture.wait_for_exit(3);
    }
}
