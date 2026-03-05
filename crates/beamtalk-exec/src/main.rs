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
use std::process::{ChildStdin, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use clap::{ArgAction, Parser};
use eetf::{Atom, Binary, FixInteger, Map, Term, Tuple};
use tracing::{debug, error, warn};
use tracing_subscriber::EnvFilter;

// ────────────────────────────────────────────────────────────────
// {packet, 4} framing

/// Read a `{packet, 4}` framed message from stdin.
fn read_packet(stdin: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
    let mut len_buf = [0u8; 4];
    match stdin.read_exact(&mut len_buf) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }
    let len = u32::from_be_bytes(len_buf) as usize;
    // Guard against unreasonably large packets (>64 MiB)
    if len > 64 * 1024 * 1024 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("packet too large: {len} bytes"),
        ));
    }
    let mut buf = vec![0u8; len];
    stdin.read_exact(&mut buf)?;
    Ok(Some(buf))
}

/// Write a `{packet, 4}` framed message to a writer.
fn write_packet(out: &mut impl Write, data: &[u8]) -> io::Result<()> {
    let len = u32::try_from(data.len())
        .expect("packet too large for {packet, 4} framing")
        .to_be_bytes();
    out.write_all(&len)?;
    out.write_all(data)?;
    out.flush()
}

// ────────────────────────────────────────────────────────────────
// ETF helpers

fn atom(name: &str) -> Term {
    Term::from(Atom::from(name))
}

fn binary_term(data: &[u8]) -> Term {
    Term::from(Binary::from(data))
}

fn int_term(n: i32) -> Term {
    Term::from(FixInteger::from(n))
}

fn tuple3(a: Term, b: Term, c: Term) -> Term {
    Term::from(Tuple::from(vec![a, b, c]))
}

/// Extract raw bytes from a `Binary` or `ByteList` term.
fn term_to_bytes(term: &Term) -> Option<Vec<u8>> {
    match term {
        Term::Binary(b) => Some(b.bytes.clone()),
        Term::ByteList(bl) => Some(bl.bytes.clone()),
        _ => None,
    }
}

/// Extract a UTF-8 string from a `Binary` or `ByteList` term.
fn term_to_string(term: &Term) -> Option<String> {
    String::from_utf8(term_to_bytes(term)?).ok()
}

/// Extract a `u32` child ID from a `FixInteger` term.
fn term_to_child_id(term: &Term) -> Option<u32> {
    match term {
        Term::FixInteger(n) => u32::try_from(n.value).ok(),
        _ => None,
    }
}

/// Extract a list of strings from a `List` term.
fn term_to_string_list(term: &Term) -> Option<Vec<String>> {
    match term {
        Term::List(list) => list.elements.iter().map(term_to_string).collect(),
        _ => None,
    }
}

/// Extract a `String → String` map from an ETF `Map` term (for environment variables).
fn term_to_string_map(term: &Term) -> Option<HashMap<String, String>> {
    match term {
        Term::Map(m) => {
            let mut result = HashMap::new();
            for (k, v) in &m.map {
                let key = term_to_string(k)?;
                let val = term_to_string(v)?;
                result.insert(key, val);
            }
            Some(result)
        }
        _ => None,
    }
}

/// Look up a key (atom name) in an ETF `Map`.
fn map_get<'a>(map: &'a Map, key: &str) -> Option<&'a Term> {
    map.map.get(&atom(key))
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
                if let Err(e) = write_packet(&mut *out, &buf) {
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
    /// Stdin pipe — `None` after `close_stdin` or if not captured.
    stdin: Option<ChildStdin>,
    /// Process group ID (Unix only). After `setsid()`, pgid == child PID.
    #[cfg(unix)]
    pgid: libc::pid_t,
}

type ChildMap = Arc<Mutex<HashMap<u32, ChildEntry>>>;

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

    let mut cmd = Command::new(&executable);
    cmd.args(&args);
    cmd.envs(&env_vars);
    if let Some(ref d) = dir {
        cmd.current_dir(d);
    }
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    // On Unix, put the child in its own session so we can signal the whole
    // process group (not just the direct child).
    #[cfg(unix)]
    configure_process_group(&mut cmd);

    let mut child = cmd.spawn().map_err(|e| format!("spawn failed: {e}"))?;

    let child_stdin = child.stdin.take();
    let child_stdout = child.stdout.take().ok_or("stdout not captured")?;
    let child_stderr = child.stderr.take().ok_or("stderr not captured")?;

    // After setsid(), the child's pgid equals its own pid.
    // Child PIDs fit in i32 on all Unix platforms (kernel enforces PID_MAX <= 2^22).
    #[cfg(unix)]
    #[allow(clippy::cast_possible_wrap)]
    let pgid = child.id() as libc::pid_t;

    // Insert entry before spawning threads, so kill/write_stdin can find it.
    {
        let mut map = children
            .lock()
            .map_err(|e| format!("children lock poisoned: {e}"))?;
        map.insert(
            child_id,
            ChildEntry {
                stdin: child_stdin,
                #[cfg(unix)]
                pgid,
            },
        );
    }

    // Spawn stdout reader thread.
    {
        let writer = Arc::clone(writer);
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut reader = child_stdout;
            loop {
                match reader.read(&mut buf) {
                    Ok(0) => break,
                    Ok(n) => {
                        send_event(&writer, &stdout_event(child_id, &buf[..n]));
                    }
                    Err(e) => {
                        debug!("stdout read error for child {child_id}: {e}");
                        break;
                    }
                }
            }
        });
    }

    // Spawn stderr reader thread.
    {
        let writer = Arc::clone(writer);
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut reader = child_stderr;
            loop {
                match reader.read(&mut buf) {
                    Ok(0) => break,
                    Ok(n) => {
                        send_event(&writer, &stderr_event(child_id, &buf[..n]));
                    }
                    Err(e) => {
                        debug!("stderr read error for child {child_id}: {e}");
                        break;
                    }
                }
            }
        });
    }

    // Spawn reaper thread: waits for the child and sends the exit event.
    {
        let writer = Arc::clone(writer);
        let children = Arc::clone(children);
        thread::spawn(move || {
            let exit_code = match child.wait() {
                Ok(status) => status.code().unwrap_or(-1),
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
            send_event(&writer, &exit_event(child_id, exit_code));
        });
    }

    Ok(())
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
        unsafe { libc::kill(-pgid, libc::SIGTERM) };

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

    #[cfg(not(unix))]
    {
        // Windows: process group kill via Job Objects is out of scope for Phase 1.
        // Close stdin to signal EOF to the child; the reaper will collect the exit.
        if let Ok(mut map) = children.lock() {
            if let Some(entry) = map.get_mut(&child_id) {
                entry.stdin = None;
            } else {
                return Err(format!("child {child_id} not found"));
            }
        }
        warn!("kill command on non-Unix: stdin closed, no SIGTERM sent");
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

    let mut map = children
        .lock()
        .map_err(|e| format!("children lock poisoned: {e}"))?;
    let entry = map
        .get_mut(&child_id)
        .ok_or_else(|| format!("child {child_id} not found"))?;
    let stdin = entry
        .stdin
        .as_mut()
        .ok_or("stdin already closed for this child")?;
    stdin
        .write_all(&data)
        .map_err(|e| format!("write_stdin error: {e}"))?;
    stdin.flush().map_err(|e| format!("flush error: {e}"))
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
        let packet = match read_packet(&mut stdin) {
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

    // ── Packet framing ──────────────────────────────────────────

    #[test]
    fn read_write_packet_roundtrip() {
        let data = b"hello world";
        let mut buf = Vec::new();
        write_packet(&mut buf, data).unwrap();
        // First 4 bytes are big-endian length.
        assert_eq!(
            &buf[..4],
            &(u32::try_from(data.len()).unwrap()).to_be_bytes()
        );
        assert_eq!(&buf[4..], data);
        let mut cursor = io::Cursor::new(buf);
        let result = read_packet(&mut cursor).unwrap().unwrap();
        assert_eq!(result, data);
    }

    #[test]
    fn read_packet_eof_returns_none() {
        let empty: &[u8] = &[];
        let mut cursor = io::Cursor::new(empty);
        let result = read_packet(&mut cursor).unwrap();
        assert!(result.is_none());
    }

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

        let args_term = Term::from(List::from(vec![] as Vec<Term>));
        let env_term = Term::from(Map::from(HashMap::<Term, Term>::new()));
        let req = Term::from(Map::from([
            (atom("command"), atom("spawn")),
            (atom("child_id"), int_term(1)),
            (atom("executable"), binary_term(b"/bin/true")),
            (atom("args"), args_term),
            (atom("env"), env_term),
        ]));

        let result = handle_command(&req, &writer, &children);
        assert!(result.is_ok(), "spawn /bin/true should succeed: {result:?}");

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
    #[cfg(unix)]
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
    #[cfg(unix)]
    fn kill_nonexistent_child_returns_error() {
        let children: ChildMap = Arc::new(Mutex::new(HashMap::new()));
        let req = Map::from([(atom("child_id"), int_term(999))]);
        let result = handle_kill(&req, &children);
        assert!(result.is_err());
    }
}
