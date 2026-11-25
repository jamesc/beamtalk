// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test helpers shared by `beamtalk-exec`'s unit tests (`src/main.rs`) and its
//! end-to-end tests (`tests/port_protocol.rs`): protocol term builders, event
//! accessors and process checks.

// Each including test target uses a different subset.
#![allow(dead_code)]

use std::collections::HashMap;
use std::time::Duration;

use beamtalk_etf::{atom, binary, int_term};
use eetf::{List, Map, Term};

/// Upper bound on every wait in these tests. A hang guard, not a timing
/// assumption: each wait ends as soon as its condition holds.
pub const HANG_GUARD: Duration = Duration::from_secs(30);

/// Binaries for each of `args`.
pub fn strs(args: &[&str]) -> Vec<Term> {
    args.iter().map(|a| binary(a.as_bytes())).collect()
}

/// A `spawn` command with an empty `env`.
pub fn spawn_with(child_id: i32, executable: &str, args: Vec<Term>) -> Term {
    Term::from(Map::from([
        (atom("command"), atom("spawn")),
        (atom("child_id"), int_term(child_id)),
        (atom("executable"), binary(executable.as_bytes())),
        (atom("args"), Term::from(List::from(args))),
        (
            atom("env"),
            Term::from(Map::from(HashMap::<Term, Term>::new())),
        ),
    ]))
}

/// A `spawn` command with string arguments and an empty `env`.
pub fn spawn_term(child_id: i32, executable: &str, args: &[&str]) -> Term {
    spawn_with(child_id, executable, strs(args))
}

/// A command with just a `child_id`, plus `extra` fields.
pub fn command(name: &str, child_id: i32, extra: Vec<(Term, Term)>) -> Term {
    let mut fields = vec![
        (atom("command"), atom(name)),
        (atom("child_id"), int_term(child_id)),
    ];
    fields.extend(extra);
    Term::from(Map::from(fields.into_iter().collect::<HashMap<_, _>>()))
}

pub fn write_stdin_term(child_id: i32, data: &[u8]) -> Term {
    command("write_stdin", child_id, vec![(atom("data"), binary(data))])
}

/// `(tag, child_id)` of an event tuple.
pub fn parts(event: &Term) -> (&str, &Term) {
    let Term::Tuple(t) = event else {
        panic!("event is not a tuple: {event:?}")
    };
    let Term::Atom(tag) = &t.elements[0] else {
        panic!("event tag is not an atom: {event:?}")
    };
    (tag.name.as_str(), &t.elements[1])
}

/// An event's payload: its last element (data, exit code or error reason).
pub fn payload(event: &Term) -> &Term {
    let Term::Tuple(t) = event else {
        panic!("event is not a tuple: {event:?}")
    };
    t.elements.last().expect("non-empty event")
}

pub fn is_event(event: &Term, tag: &str, child_id: i32) -> bool {
    parts(event) == (tag, &int_term(child_id))
}

/// Whether `pid` is a running (not zombie) process.
#[cfg(unix)]
pub fn process_running(pid: i32) -> bool {
    let out = std::process::Command::new("ps")
        .args(["-o", "stat=", "-p", &pid.to_string()])
        .output()
        .expect("run ps");
    let stat = String::from_utf8_lossy(&out.stdout);
    let stat = stat.trim();
    !stat.is_empty() && !stat.starts_with('Z')
}

/// Wait (up to [`HANG_GUARD`]) for `pid` to stop running.
#[cfg(unix)]
pub fn wait_until_gone(pid: i32) {
    let deadline = std::time::Instant::now() + HANG_GUARD;
    while process_running(pid) {
        assert!(
            std::time::Instant::now() < deadline,
            "process {pid} still running"
        );
        std::thread::sleep(Duration::from_millis(20)); // sleep: poll for the process to die
    }
}
