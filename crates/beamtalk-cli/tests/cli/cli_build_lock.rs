// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5075: concurrent `beamtalk` processes on one project are serialised by
//! the project build lock (`_build/.build.lock`). Each test runs real
//! subprocesses against a fixture project with a git dependency; before the
//! lock, concurrent cold builds failed with "Failed to remove existing
//! checkout" / "Failed to clone git dependency", and concurrent `deps add`
//! calls lost a manifest or `beamtalk.lock` entry.

use crate::cli_common;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::{Command as StdCommand, Output, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const WAITING: &str = "waiting for another beamtalk process in";
const HEADER: &str = "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n";

fn git(dir: &Path, args: &[&str]) {
    let output = StdCommand::new("git")
        .args(["-c", "user.email=test@example.com", "-c", "user.name=test"])
        .args(args)
        .current_dir(dir)
        .output()
        .expect("run git");
    assert!(
        output.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// A git repo holding a one-class Beamtalk package `name`.
fn make_dep_repo(parent: &Path, name: &str) -> String {
    let repo = parent.join(name);
    std::fs::create_dir_all(repo.join("src")).unwrap();
    std::fs::write(
        repo.join("beamtalk.toml"),
        format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
    )
    .unwrap();
    std::fs::write(
        repo.join(format!("src/dep_{name}.bt")),
        format!("{HEADER}Object subclass: Dep{}\n  hello => 1\n", {
            let mut c = name.chars();
            c.next()
                .map(|f| f.to_uppercase().chain(c).collect::<String>())
                .unwrap()
        }),
    )
    .unwrap();
    git(&repo, &["init", "-q", "-b", "main"]);
    git(&repo, &["add", "-A"]);
    git(&repo, &["commit", "-q", "-m", "one"]);
    cli_common::file_url(&repo)
}

fn write_project(project: &Path, deps: &str) {
    std::fs::create_dir_all(project.join("src")).unwrap();
    std::fs::write(
        project.join("beamtalk.toml"),
        format!("[package]\nname = \"proj\"\nversion = \"0.1.0\"\n\n[dependencies]\n{deps}\n"),
    )
    .unwrap();
    for i in 1..=4 {
        std::fs::write(
            project.join(format!("src/c{i}.bt")),
            format!("{HEADER}Object subclass: C{i}\n  run => {i}\n"),
        )
        .unwrap();
    }
}

/// Run `beamtalk <args>` in `project` on its own thread; join for the output.
fn spawn_beamtalk(project: &Path, args: &[&str]) -> std::thread::JoinHandle<Output> {
    let mut cmd = cli_common::beamtalk();
    cmd.current_dir(project).args(args);
    std::thread::spawn(move || cmd.output().expect("run beamtalk"))
}

fn assert_ok(label: &str, out: &Output) {
    assert!(
        out.status.success(),
        "{label} failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn concurrent_cold_builds_both_succeed_and_agree() {
    let tmp = tempfile::tempdir().unwrap();
    let url = make_dep_repo(tmp.path(), "gdep");
    let project = tmp.path().join("proj");
    write_project(
        &project,
        &format!("gdep = {{ git = \"{url}\", branch = \"main\" }}"),
    );

    let runs: Vec<_> = (0..3)
        .map(|_| spawn_beamtalk(&project, &["build"]))
        .collect();
    for (i, run) in runs.into_iter().enumerate() {
        assert_ok(&format!("concurrent build {i}"), &run.join().unwrap());
    }

    // One more serial build: nothing left behind may break the next build, and
    // it must find everything up to date (consistent outputs and sidecars).
    let out = cli_common::beamtalk()
        .current_dir(&project)
        .arg("build")
        .output()
        .unwrap();
    assert_ok("follow-up build", &out);
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        report.contains("unchanged"),
        "expected a no-op rebuild: {report}"
    );
    for i in 1..=4 {
        assert!(
            project
                .join(format!("_build/dev/ebin/bt@proj@c{i}.beam"))
                .is_file()
        );
    }
    assert!(
        std::fs::read_to_string(project.join("beamtalk.lock"))
            .unwrap()
            .contains("name = \"gdep\"")
    );
}

#[test]
fn concurrent_deps_add_keeps_both_manifest_and_lock_entries() {
    let tmp = tempfile::tempdir().unwrap();
    let url_a = make_dep_repo(tmp.path(), "depa");
    let url_b = make_dep_repo(tmp.path(), "depb");
    let project = tmp.path().join("proj");
    write_project(&project, "");

    let a = spawn_beamtalk(
        &project,
        &["deps", "add", "depa", "--git", &url_a, "--branch", "main"],
    );
    let b = spawn_beamtalk(
        &project,
        &["deps", "add", "depb", "--git", &url_b, "--branch", "main"],
    );
    assert_ok("deps add depa", &a.join().unwrap());
    assert_ok("deps add depb", &b.join().unwrap());

    let manifest = std::fs::read_to_string(project.join("beamtalk.toml")).unwrap();
    let lock = std::fs::read_to_string(project.join("beamtalk.lock")).unwrap();
    for name in ["depa", "depb"] {
        assert!(
            manifest.contains(&format!("{name} =")),
            "manifest lost {name}:\n{manifest}"
        );
        assert!(
            lock.contains(&format!("name = \"{name}\"")),
            "lock lost {name}:\n{lock}"
        );
    }
}

/// A lock file left by a killed build is not a lock: the OS releases the lock
/// with the process, so the next build must not wait or need cleanup.
#[test]
fn stale_lock_file_from_a_dead_process_does_not_block_a_build() {
    let tmp = tempfile::tempdir().unwrap();
    let project = tmp.path().join("proj");
    write_project(&project, "");
    std::fs::create_dir_all(project.join("_build")).unwrap();
    std::fs::write(
        project.join("_build/.build.lock"),
        b"left by a crashed build",
    )
    .unwrap();

    let out = cli_common::beamtalk()
        .current_dir(&project)
        .arg("build")
        .output()
        .unwrap();
    assert_ok("build with stale lock file", &out);
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("waiting for another beamtalk process"),
        "must not wait on a dead holder"
    );
}

/// While one process holds the lock, a build announces the wait and finishes
/// once the holder lets go.
#[test]
fn build_waits_with_message_while_lock_is_held_then_proceeds() {
    use fs2::FileExt;
    let tmp = tempfile::tempdir().unwrap();
    let project = tmp.path().join("proj");
    write_project(&project, "");
    std::fs::create_dir_all(project.join("_build")).unwrap();
    let held = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(false)
        .open(project.join("_build/.build.lock"))
        .unwrap();
    held.lock_exclusive().unwrap();

    let configured = cli_common::beamtalk();
    let mut std_cmd = StdCommand::new(configured.get_program());
    for (key, val) in configured.get_envs() {
        match val {
            Some(val) => std_cmd.env(key, val),
            None => std_cmd.env_remove(key),
        };
    }
    let mut child = std_cmd
        .current_dir(&project)
        .arg("build")
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn beamtalk build");
    let stderr = child.stderr.take().expect("piped stderr");
    let (tx, rx) = mpsc::channel::<String>();
    let reader = std::thread::spawn(move || {
        for line in BufReader::new(stderr).lines().map_while(Result::ok) {
            if tx.send(line).is_err() {
                break;
            }
        }
    });

    // Release the lock only once the build reports it is waiting: the build
    // probes OTP before taking the lock, which can be arbitrarily slow on a
    // loaded runner, so no fixed delay is safe.
    let mut stderr_lines = Vec::new();
    let deadline = Instant::now() + Duration::from_mins(1);
    loop {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if let Ok(line) = rx.recv_timeout(remaining) {
            let seen = line.contains(WAITING);
            stderr_lines.push(line);
            if seen {
                break;
            }
        } else {
            let _ = child.kill();
            panic!("build never reported waiting for the lock; stderr so far: {stderr_lines:?}");
        }
    }
    assert!(
        child.try_wait().unwrap().is_none(),
        "build must wait for the held lock"
    );
    assert!(
        !project
            .join("_build/dev/ebin")
            .join("bt@proj@c1.beam")
            .exists(),
        "nothing may be built while the lock is held"
    );
    held.unlock().unwrap();

    let status = child.wait().unwrap();
    reader.join().unwrap();
    stderr_lines.extend(rx.try_iter());
    let stderr = stderr_lines.join("\n");
    assert!(status.success(), "build after lock freed failed:\n{stderr}");
    assert_eq!(
        stderr.matches(WAITING).count(),
        1,
        "expected the waiting message exactly once, got: {stderr}"
    );
}
