// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! End-to-end tests of the `beamtalk-exec` binary over its port protocol
//! (BT-5195): `{packet, 4}` ETF commands on stdin, events on stdout.
//!
//! `BEAMTALK_EXEC_TEST_BIN` overrides the binary under test, so the same tests
//! can be pointed at an older build to show what they catch.

mod support;

use std::io::{self, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::{Duration, Instant};

use beamtalk_etf::{self as etf, atom, int_term, term_to_bytes, term_to_string};
use eetf::Term;
use support::{
    HANG_GUARD, command, is_event, payload, spawn_with as spawn, strs,
    write_stdin_term as write_stdin,
};

fn exec_binary() -> String {
    std::env::var("BEAMTALK_EXEC_TEST_BIN")
        .unwrap_or_else(|_| env!("CARGO_BIN_EXE_beamtalk-exec").to_string())
}

/// A running port: commands in, decoded events out.
struct PortProcess {
    process: Child,
    commands: Option<ChildStdin>,
    events: Receiver<Term>,
    seen: Vec<Term>,
}

impl PortProcess {
    fn start() -> Self {
        let mut process = Command::new(exec_binary())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("start beamtalk-exec");
        let commands = process.stdin.take();
        let mut stdout = process.stdout.take().expect("stdout");
        let (tx, events) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(Some(packet)) = etf::read_packet(&mut stdout) {
                let term = Term::decode(io::Cursor::new(packet)).expect("decode event");
                if tx.send(term).is_err() {
                    break;
                }
            }
        });
        Self {
            process,
            commands,
            events,
            seen: Vec::new(),
        }
    }

    fn send(&mut self, command: &Term) {
        let mut body = Vec::new();
        command.encode(&mut body).expect("encode");
        let stdin = self.commands.as_mut().expect("commands open");
        etf::write_packet(stdin, &body).expect("write command");
        stdin.flush().expect("flush");
    }

    /// Receive events until one matches; return it.
    fn wait_for(&mut self, what: &str, pred: impl Fn(&Term) -> bool) -> Term {
        let deadline = Instant::now() + HANG_GUARD;
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            match self.events.recv_timeout(left) {
                Ok(event) => {
                    self.seen.push(event.clone());
                    if pred(&event) {
                        return event;
                    }
                }
                Err(e) => panic!("no {what} within {HANG_GUARD:?} ({e}); saw {:?}", self.seen),
            }
        }
    }

    fn wait_for_exit(&mut self, child_id: i32) -> Term {
        let exit = self.wait_for(&format!("exit for child {child_id}"), |e| {
            is_event(e, "exit", child_id)
        });
        payload(&exit).clone()
    }

    fn output(&self, stream: &str, child_id: i32) -> Vec<u8> {
        self.seen
            .iter()
            .filter(|e| is_event(e, stream, child_id))
            .flat_map(|e| term_to_bytes(payload(e)).expect("binary payload"))
            .collect()
    }
}

impl Drop for PortProcess {
    fn drop(&mut self) {
        let _ = self.process.kill();
        let _ = self.process.wait();
    }
}

/// A shell command line that prints `text` and exits.
fn echo(text: &str) -> (&'static str, Vec<Term>) {
    if cfg!(windows) {
        ("cmd", strs(&["/c", &format!("echo {text}")]))
    } else {
        ("/bin/sh", strs(&["-c", &format!("echo {text}")]))
    }
}

/// A command that runs for minutes and never reads its stdin.
fn never_reads() -> (&'static str, Vec<Term>) {
    if cfg!(windows) {
        ("cmd", strs(&["/c", "ping -n 300 127.0.0.1 >nul"]))
    } else {
        ("/bin/sleep", strs(&["300"]))
    }
}

// ── Command loop ────────────────────────────────────────────────

#[test]
fn a_child_runs_to_completion_and_the_port_exits_at_eof() {
    let mut port = PortProcess::start();
    let (exe, args) = echo("hello");
    port.send(&spawn(1, exe, args));
    assert_eq!(port.wait_for_exit(1), int_term(0));
    let out = String::from_utf8(port.output("stdout", 1)).unwrap();
    assert_eq!(out.trim_end(), "hello");

    drop(port.commands.take());
    let deadline = Instant::now() + HANG_GUARD;
    loop {
        if let Some(status) = port.process.try_wait().expect("try_wait") {
            assert!(status.success(), "{status:?}");
            break;
        }
        assert!(Instant::now() < deadline, "port did not exit at EOF");
        thread::sleep(Duration::from_millis(10)); // sleep: poll for the port to exit
    }
}

// ── BT-5195 item 3: malformed args ──────────────────────────────

/// Before BT-5195 the child was spawned with no arguments.
#[test]
fn malformed_args_are_answered_with_an_error_and_no_spawn() {
    let mut port = PortProcess::start();
    let (exe, mut args) = echo("never");
    args.push(atom("not_a_binary"));
    port.send(&spawn(3, exe, args));
    let error = port.wait_for("error event", |e| is_event(e, "error", 3));
    let reason = term_to_string(payload(&error)).expect("reason is a binary");
    assert!(reason.contains("args"), "{reason}");
    assert_eq!(port.wait_for_exit(3), int_term(-1));
    assert!(port.output("stdout", 3).is_empty(), "nothing ran");
}

// ── BT-5195 item 2: stdin off the command thread ────────────────

/// Before BT-5195 the first large write to a child that never reads blocked
/// the port's only command thread, so nothing else (not even `kill`) ran.
#[test]
fn a_child_that_never_reads_does_not_stall_the_port() {
    let mut port = PortProcess::start();
    let (exe, args) = never_reads();
    port.send(&spawn(1, exe, args));
    let chunk = vec![b'x'; 1 << 20];
    for _ in 0..80 {
        port.send(&write_stdin(1, &chunk));
    }
    let error = port.wait_for("queue-full error", |e| is_event(e, "error", 1));
    let reason = term_to_string(payload(&error)).unwrap();
    assert!(reason.contains("stdin queue full"), "{reason}");

    // Other children still work...
    let (exe, args) = echo("other");
    port.send(&spawn(2, exe, args));
    assert_eq!(port.wait_for_exit(2), int_term(0));
    // ...and so does `kill`.
    port.send(&command("kill", 1, vec![]));
    port.wait_for_exit(1);
}

/// BT-5248: every `write_stdin` gets one answer, in order, so the BEAM can
/// tell which write was dropped: `stdin_queued` for the ones accepted, `error`
/// for the one that overflowed the queue (and every one after it).
#[test]
fn every_write_is_answered_and_the_overflowing_one_gets_the_error() {
    let mut port = PortProcess::start();
    let (exe, args) = never_reads();
    port.send(&spawn(1, exe, args));
    let chunk = vec![b'x'; 1 << 20];
    let writes = 12;
    for _ in 0..writes {
        port.send(&write_stdin(1, &chunk));
    }
    let mut answers = Vec::new();
    for _ in 0..writes {
        let answer = port.wait_for("answer to a write", |e| {
            is_event(e, "stdin_queued", 1) || is_event(e, "error", 1)
        });
        answers.push(is_event(&answer, "stdin_queued", 1));
    }
    // An accepted write is acked first; once one is refused, all later ones are.
    assert!(answers[0], "the first write is accepted: {answers:?}");
    let first_refused = answers.iter().position(|ok| !ok).expect("one overflows");
    assert!(
        answers[first_refused..].iter().all(|ok| !ok),
        "no write is accepted after one was dropped: {answers:?}"
    );
    port.send(&command("kill", 1, vec![]));
    port.wait_for_exit(1);
}

// ── BT-5195 item 1: a grandchild holding the pipes ──────────────

/// Before BT-5195 `exit` waited for the pipes to close, which a background
/// grandchild never does.
#[cfg(unix)]
#[test]
fn exit_arrives_while_a_grandchild_holds_the_pipes() {
    let mut port = PortProcess::start();
    port.send(&spawn(1, "/bin/sh", strs(&["-c", "sleep 300 & echo $!"])));
    assert_eq!(port.wait_for_exit(1), int_term(0));
    let grandchild: i32 = String::from_utf8(port.output("stdout", 1))
        .unwrap()
        .trim()
        .parse()
        .expect("grandchild pid");
    assert!(support::process_running(grandchild));
    // Its group is still killable through the port.
    port.send(&command("kill", 1, vec![]));
    support::wait_until_gone(grandchild);
}

// ── BT-5195 item 4: children die with the port ──────────────────

#[cfg(unix)]
mod unix {
    use super::*;
    use std::os::unix::process::ExitStatusExt;

    use support::{process_running as running, wait_until_gone};

    /// Start a child that prints its own pid and its grandchild's, then
    /// waits; return both.
    fn child_and_grandchild(port: &mut PortProcess) -> (i32, i32) {
        port.send(&spawn(
            1,
            "/bin/sh",
            strs(&["-c", "sleep 300 & echo $$ $!; wait"]),
        ));
        let line = port.wait_for("pids", |e| is_event(e, "stdout", 1));
        let text = String::from_utf8(term_to_bytes(payload(&line)).unwrap()).unwrap();
        let pids: Vec<i32> = text
            .split_whitespace()
            .map(|p| p.parse().expect("pid"))
            .collect();
        assert!(running(pids[0]) && running(pids[1]));
        (pids[0], pids[1])
    }

    /// Before BT-5195 a port killed with SIGKILL left its children running for ever.
    #[test]
    fn sigkill_of_the_port_kills_child_and_grandchild() {
        let mut port = PortProcess::start();
        let (child, grandchild) = child_and_grandchild(&mut port);
        port.process.kill().expect("SIGKILL the port");
        port.process.wait().expect("reap the port");
        wait_until_gone(child);
        wait_until_gone(grandchild);
    }

    /// SIGTERM is handled: children are stopped, then the port exits 143.
    /// Before BT-5195 the port died of the signal and the children survived.
    #[test]
    fn sigterm_of_the_port_stops_children_and_exits_143() {
        let mut port = PortProcess::start();
        let (child, grandchild) = child_and_grandchild(&mut port);
        let port_pid = i32::try_from(port.process.id()).unwrap();
        // SAFETY: kill(2) to the port process this test started.
        unsafe { libc::kill(port_pid, libc::SIGTERM) };
        let status = port.process.wait().expect("reap the port");
        assert_eq!(
            (status.code(), status.signal()),
            (Some(143), None),
            "the port handles SIGTERM and exits 128 + 15"
        );
        wait_until_gone(child);
        wait_until_gone(grandchild);
    }
}

// ── BT-5195 item 6: --cat ───────────────────────────────────────

/// `--cat` copies bytes unchanged: before BT-5195 it stopped at the first
/// non-UTF-8 line (still exiting 0) and rewrote `\r\n` as `\n`.
#[test]
fn cat_mode_passes_binary_input_through_unchanged() {
    let input: &[u8] = b"line one\r\n\xff\xfe\x00 not utf-8\nlast, no newline";
    let mut cat = Command::new(exec_binary())
        .arg("--cat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("start --cat");
    let mut stdin = cat.stdin.take().unwrap();
    let writer = thread::spawn(move || stdin.write_all(input));
    let mut out = Vec::new();
    cat.stdout.take().unwrap().read_to_end(&mut out).unwrap();
    writer.join().unwrap().expect("write input");
    let status = cat.wait().unwrap();
    assert!(status.success(), "{status:?}");
    assert_eq!(out, input);
}
