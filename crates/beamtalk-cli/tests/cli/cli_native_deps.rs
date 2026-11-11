// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `[native.dependencies]` git sources (BT-4566).
//!
//! rebar3 never re-fetches an existing dependency checkout because
//! `rebar.config` changed — only its `rebar.lock` moves one. These tests
//! drive `beamtalk build` against local `file://` git repositories (no
//! network) and check that `beamtalk.lock` is what decides the commit:
//! pins hold while the manifest is unchanged, a changed branch or
//! `beamtalk deps update` re-resolves, and transitive deps stay pinned on a
//! fresh `_build`.
//!
//! The hex path (BT-4582) runs rebar3 against a local hex repository built by
//! `fixtures/make_hex_repo.escript` and served over loopback HTTP, so a
//! rendered `rebar.lock` (`{pkg, ..}` with `pkg_hash`/`pkg_hash_ext`) is
//! exercised end to end with no network.
//!
//! Unix-only: `file://` URLs for Windows paths need separate handling.

#![cfg(unix)]

use crate::cli_common;

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

fn git(dir: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
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
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// Create a git repo holding a minimal OTP app `name` whose `version/0`
/// returns `version`, with `deps` as its rebar.config deps list.
fn make_app_repo(parent: &Path, name: &str, version: &str, deps: &str) -> std::path::PathBuf {
    let repo = parent.join(name);
    std::fs::create_dir_all(repo.join("src")).expect("mkdir src");
    git(&repo, &["init", "-q", "-b", "main"]);
    std::fs::write(
        repo.join(format!("src/{name}.app.src")),
        format!(
            "{{application, {name}, [{{vsn, \"0.1.0\"}}, {{applications, [kernel, stdlib]}}]}}.\n"
        ),
    )
    .expect("write .app.src");
    std::fs::write(repo.join("rebar.config"), format!("{{deps, [{deps}]}}.\n"))
        .expect("write rebar.config");
    commit_version(&repo, name, version);
    repo
}

/// Commit a new `version/0` for app `name` on the current branch.
fn commit_version(repo: &Path, name: &str, version: &str) -> String {
    std::fs::write(
        repo.join(format!("src/{name}.erl")),
        format!("-module({name}).\n-export([version/0]).\nversion() -> \"{version}\".\n"),
    )
    .expect("write .erl");
    git(repo, &["add", "-A"]);
    git(repo, &["commit", "-q", "-m", version]);
    git(repo, &["rev-parse", "HEAD"])
}

fn file_url(repo: &Path) -> String {
    format!("file://{}", repo.display())
}

fn write_manifest(project: &Path, native_deps: &str) {
    std::fs::write(
        project.join("beamtalk.toml"),
        format!(
            "[package]\nname = \"native_git_fixture\"\nversion = \"0.1.0\"\n\n\
             [native.dependencies]\n{native_deps}\n"
        ),
    )
    .expect("write beamtalk.toml");
}

fn build(project: &Path) {
    cli_common::beamtalk()
        .current_dir(project)
        .arg("build")
        .assert()
        .success();
}

/// The version string compiled into the checked-out dependency.
fn checked_out_version(project: &Path, name: &str) -> String {
    let src = project.join(format!(
        "_build/dev/native/default/lib/{name}/src/{name}.erl"
    ));
    let content = std::fs::read_to_string(&src).expect("read checked-out source");
    content
        .split('"')
        .nth(1)
        .expect("version string in source")
        .to_string()
}

fn lockfile(project: &Path) -> String {
    std::fs::read_to_string(project.join("beamtalk.lock")).expect("read beamtalk.lock")
}

#[test]
fn native_git_dep_follows_beamtalk_lock() {
    let repos = tempfile::tempdir().expect("tempdir");
    let foo = make_app_repo(repos.path(), "foo", "v1", "");
    let v1 = git(&foo, &["rev-parse", "HEAD"]);
    git(&foo, &["checkout", "-q", "-b", "next"]);
    let next = commit_version(&foo, "foo", "next");
    git(&foo, &["checkout", "-q", "main"]);

    let project = cli_common::fixture_project();
    let project = project.path();
    let url = file_url(&foo);
    write_manifest(
        project,
        &format!("foo = {{ git = \"{url}\", branch = \"main\" }}"),
    );

    // First build resolves main and records the commit and branch.
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "v1");
    let lock = lockfile(project);
    assert!(lock.contains(&format!("sha = \"{v1}\"")), "{lock}");
    assert!(lock.contains("reference = \"branch:main\""), "{lock}");

    // main moves on, the manifest doesn't: the lock pin holds.
    let v2 = commit_version(&foo, "foo", "v2");
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "v1");

    // Switching branch in the manifest re-resolves despite the old checkout.
    write_manifest(
        project,
        &format!("foo = {{ git = \"{url}\", branch = \"next\" }}"),
    );
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "next");
    assert!(lockfile(project).contains(&format!("sha = \"{next}\"")));

    // Back to main, then `deps update` moves it to the current tip.
    write_manifest(
        project,
        &format!("foo = {{ git = \"{url}\", branch = \"main\" }}"),
    );
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "v2");
    let v3 = commit_version(&foo, "foo", "v3");
    build(project);
    assert_eq!(
        checked_out_version(project, "foo"),
        "v2",
        "pinned until update"
    );
    cli_common::beamtalk()
        .current_dir(project)
        .args(["deps", "update"])
        .assert()
        .success();
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "v3");
    assert!(lockfile(project).contains(&format!("sha = \"{v3}\"")));
    assert_ne!(v2, v3);
}

/// A transitive git dep recorded in `beamtalk.lock` is pinned on a fresh
/// `_build` (e.g. a new clone or CI), even though no manifest names it.
#[test]
fn transitive_native_git_dep_pinned_on_fresh_build() {
    let repos = tempfile::tempdir().expect("tempdir");
    let bar = make_app_repo(repos.path(), "bar", "bar1", "");
    let bar1 = git(&bar, &["rev-parse", "HEAD"]);
    let foo = make_app_repo(
        repos.path(),
        "foo",
        "foo1",
        &format!(
            "{{bar, {{git, \"{}\", {{branch, \"main\"}}}}}}",
            file_url(&bar)
        ),
    );

    let project = cli_common::fixture_project();
    let project = project.path();
    write_manifest(
        project,
        &format!(
            "foo = {{ git = \"{}\", branch = \"main\" }}",
            file_url(&foo)
        ),
    );

    build(project);
    assert_eq!(checked_out_version(project, "bar"), "bar1");
    assert!(lockfile(project).contains(&format!("sha = \"{bar1}\"")));

    // bar moves on; a fresh _build must still get the locked commit.
    commit_version(&bar, "bar", "bar2");
    std::fs::remove_dir_all(project.join("_build")).expect("remove _build");
    build(project);
    assert_eq!(checked_out_version(project, "bar"), "bar1");
}

/// Changing one dependency re-resolves only that one: an unchanged direct
/// dependency stays on its locked commit even though its branch moved on.
/// Removing a dependency removes its checkout, so it leaves the code path.
#[test]
fn changing_one_native_dep_keeps_others_pinned() {
    let repos = tempfile::tempdir().expect("tempdir");
    let foo = make_app_repo(repos.path(), "foo", "foo1", "");
    git(&foo, &["checkout", "-q", "-b", "next"]);
    commit_version(&foo, "foo", "foo-next");
    git(&foo, &["checkout", "-q", "main"]);
    let baz = make_app_repo(repos.path(), "baz", "baz1", "");

    let project = cli_common::fixture_project();
    let project = project.path();
    let foo_dep = |branch: &str| {
        format!(
            "foo = {{ git = \"{}\", branch = \"{branch}\" }}",
            file_url(&foo)
        )
    };
    let baz_dep = format!(
        "baz = {{ git = \"{}\", branch = \"main\" }}",
        file_url(&baz)
    );

    write_manifest(project, &format!("{}\n{baz_dep}", foo_dep("main")));
    build(project);
    assert_eq!(checked_out_version(project, "baz"), "baz1");

    // baz's main moves on; switching foo's branch must not drag baz along.
    commit_version(&baz, "baz", "baz2");
    write_manifest(project, &format!("{}\n{baz_dep}", foo_dep("next")));
    build(project);
    assert_eq!(checked_out_version(project, "foo"), "foo-next");
    assert_eq!(checked_out_version(project, "baz"), "baz1");

    // Dropping baz removes its checkout and its lock entry.
    write_manifest(project, &foo_dep("next"));
    build(project);
    assert!(
        !project.join("_build/dev/native/default/lib/baz").exists(),
        "removed dependency's checkout must be deleted"
    );
    assert!(!lockfile(project).contains("name = \"baz\""));
}

// ── hex sources (BT-4582) ────────────────────────────────────────────

/// A local hex repository: a directory built by `make_hex_repo.escript`
/// and served over loopback HTTP (rebar3 speaks HTTP only, and insists on
/// an `ETag` for tarballs).
struct HexRepo {
    dir: tempfile::TempDir,
    cache: tempfile::TempDir,
    releases: Vec<String>,
    listing: String,
    url: String,
    stop: Arc<AtomicBool>,
    port: u16,
}

impl HexRepo {
    fn new(releases: &[&str]) -> Self {
        let dir = tempfile::tempdir().expect("tempdir");
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("addr").port();
        let stop = Arc::new(AtomicBool::new(false));
        let (root, flag) = (dir.path().to_path_buf(), Arc::clone(&stop));
        std::thread::spawn(move || {
            for stream in listener.incoming() {
                if flag.load(Ordering::SeqCst) {
                    break;
                }
                if let Ok(stream) = stream {
                    let root = root.clone();
                    std::thread::spawn(move || serve(stream, &root));
                }
            }
        });
        let mut repo = Self {
            dir,
            cache: tempfile::tempdir().expect("tempdir"),
            releases: Vec::new(),
            listing: String::new(),
            url: format!("http://127.0.0.1:{port}"),
            stop,
            port,
        };
        for release in releases {
            repo.releases.push((*release).to_string());
        }
        repo.rebuild();
        repo
    }

    /// Publish another release (`name@version`) and regenerate the index.
    fn publish(&mut self, release: &str) -> &mut Self {
        self.releases.push(release.to_string());
        self.rebuild();
        self
    }

    /// `(inner, outer)` checksums (lowercase hex) of a published release.
    fn checksums(&self, name: &str, version: &str) -> (String, String) {
        let line = self
            .listing
            .lines()
            .find(|l| l.starts_with(&format!("{name} {version} ")))
            .unwrap_or_else(|| panic!("release {name}@{version} not published"));
        let mut parts = line.split(' ').skip(2);
        (
            parts.next().expect("inner").to_string(),
            parts.next().expect("outer").to_string(),
        )
    }

    fn rebuild(&mut self) {
        let script =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/make_hex_repo.escript");
        let output = Command::new("escript")
            .arg(script)
            .arg(rebar3_escript())
            .arg(self.dir.path())
            .args(&self.releases)
            .output()
            .expect("run escript (is Erlang/OTP installed?)");
        assert!(
            output.status.success(),
            "make_hex_repo failed: {}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        self.listing = String::from_utf8_lossy(&output.stdout).to_string();
    }

    /// `beamtalk <args>` in `project`, pointed at this repository.
    fn beamtalk(&self, project: &Path) -> assert_cmd::Command {
        let mut cmd = cli_common::beamtalk();
        cmd.current_dir(project)
            .env("BEAMTALK_TEST_HEX_REPO_URL", &self.url)
            // The hex-mirror variables some environments set would
            // redirect rebar3 away from the local repository.
            .env_remove("HEX_MIRROR")
            .env("HEX_CDN", &self.url)
            .env("REBAR_CACHE_DIR", self.cache.path());
        cmd
    }

    fn build(&self, project: &Path) {
        self.beamtalk(project).arg("build").assert().success();
    }
}

impl Drop for HexRepo {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::SeqCst);
        // Wake the accept loop so the server thread exits.
        let _ = TcpStream::connect(("127.0.0.1", self.port));
    }
}

/// The rebar3 escript whose bundled `r3_hex_*` modules build the repository:
/// the runtime's copy when present, else the one on `$PATH`.
fn rebar3_escript() -> PathBuf {
    let bundled = cli_common::project_root().join("runtime/tools/rebar3");
    if bundled.exists() {
        return bundled;
    }
    std::env::var_os("PATH")
        .into_iter()
        .flat_map(|paths| std::env::split_paths(&paths).collect::<Vec<_>>())
        .map(|dir| dir.join("rebar3"))
        .find(|candidate| candidate.is_file())
        .expect("rebar3 not found")
}

/// Answer one HTTP request with a file under `root` (404 when missing,
/// 304 when `If-None-Match` matches the `ETag`).
fn serve(mut stream: TcpStream, root: &Path) {
    let mut reader = BufReader::new(stream.try_clone().expect("clone"));
    let mut request_line = String::new();
    if reader.read_line(&mut request_line).is_err() {
        return;
    }
    let mut if_none_match = None;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).unwrap_or(0) == 0 || line.trim().is_empty() {
            break;
        }
        if let Some(value) = line.to_ascii_lowercase().strip_prefix("if-none-match:") {
            if_none_match = Some(value.trim().to_string());
        }
    }
    let path = request_line
        .split(' ')
        .nth(1)
        .unwrap_or("/")
        .trim_start_matches('/');
    let Ok(body) = std::fs::read(root.join(path)) else {
        let _ = stream
            .write_all(b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
        return;
    };
    let mut hasher = DefaultHasher::new();
    body.hash(&mut hasher);
    let etag = format!("\"{:016x}\"", hasher.finish());
    let response = if if_none_match.as_deref() == Some(etag.as_str()) {
        format!("HTTP/1.1 304 Not Modified\r\nETag: {etag}\r\nConnection: close\r\n\r\n")
            .into_bytes()
    } else {
        let mut head = format!(
            "HTTP/1.1 200 OK\r\nETag: {etag}\r\nContent-Type: application/octet-stream\r\n\
             Content-Length: {}\r\nConnection: close\r\n\r\n",
            body.len()
        )
        .into_bytes();
        head.extend_from_slice(&body);
        head
    };
    let _ = stream.write_all(&response);
    let _ = stream.flush();
    let mut sink = Vec::new();
    let _ = reader.read_to_end(&mut sink);
}

fn write_hex_manifest(project: &Path, deps: &[(&str, &str)]) {
    let lines: Vec<String> = deps
        .iter()
        .map(|(name, constraint)| format!("{name} = \"{constraint}\""))
        .collect();
    write_manifest(project, &lines.join("\n"));
}

/// The `version = "..."` of a `[[native_package]]` entry in `beamtalk.lock`.
fn locked_hex_version(project: &Path, name: &str) -> String {
    let lock = lockfile(project);
    let entry = lock
        .split("[[native_package]]")
        .find(|block| block.contains(&format!("name = \"{name}\"")))
        .unwrap_or_else(|| panic!("no native lock entry for {name}:\n{lock}"));
    entry
        .lines()
        .find_map(|l| l.strip_prefix("version = \""))
        .and_then(|v| v.strip_suffix('"'))
        .expect("locked version")
        .to_string()
}

/// A locked hex version is honoured on a fresh `_build` even though a newer
/// release exists, and the rendered `rebar.lock` carries both hashes.
#[test]
fn locked_hex_version_honoured_on_fresh_build() {
    let mut repo = HexRepo::new(&["foo@1.0.0"]);
    let project = cli_common::fixture_project();
    let project = project.path();
    write_hex_manifest(project, &[("foo", "~> 1.0")]);

    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.0.0");
    let (inner, outer) = repo.checksums("foo", "1.0.0");
    let lock = lockfile(project);
    assert!(lock.contains("version = \"1.0.0\""), "{lock}");
    assert!(lock.to_lowercase().contains(&inner), "inner hash: {lock}");
    assert!(lock.to_lowercase().contains(&outer), "outer hash: {lock}");

    // A newer release appears; a fresh _build still gets the locked one.
    repo.publish("foo@1.1.0");
    std::fs::remove_dir_all(project.join("_build")).expect("remove _build");
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.0.0");
    assert_eq!(locked_hex_version(project, "foo"), "1.0.0");
}

/// A legacy `beamtalk.lock` entry without `outer_sha` renders a `rebar.lock`
/// with `pkg_hash` only, which rebar3 accepts; the build then records the
/// outer checksum again.
#[test]
fn legacy_hex_lock_without_outer_sha_is_accepted() {
    let mut repo = HexRepo::new(&["foo@1.0.0"]);
    let project = cli_common::fixture_project();
    let project = project.path();
    write_hex_manifest(project, &[("foo", "~> 1.0")]);
    repo.build(project);

    let legacy = lockfile(project)
        .lines()
        .filter(|l| !l.starts_with("outer_sha"))
        .collect::<Vec<_>>()
        .join("\n");
    assert!(!legacy.contains("outer_sha"));
    std::fs::write(project.join("beamtalk.lock"), legacy).expect("write legacy lock");

    repo.publish("foo@1.1.0");
    std::fs::remove_dir_all(project.join("_build")).expect("remove _build");
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.0.0");
    let (_, outer) = repo.checksums("foo", "1.0.0");
    assert!(lockfile(project).to_lowercase().contains(&outer));
}

/// Changing a constraint so the locked version no longer matches re-resolves
/// that package only; the other keeps its locked version.
#[test]
fn changed_hex_constraint_re_resolves_only_that_package() {
    let mut repo = HexRepo::new(&["foo@1.0.0", "bar@2.0.0"]);
    let project = cli_common::fixture_project();
    let project = project.path();
    write_hex_manifest(project, &[("foo", "~> 1.0"), ("bar", "~> 2.0")]);
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.0.0");
    assert_eq!(checked_out_version(project, "bar"), "2.0.0");

    repo.publish("foo@1.1.0").publish("bar@2.1.0");
    write_hex_manifest(project, &[("foo", "~> 1.1"), ("bar", "~> 2.0")]);
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.1.0");
    assert_eq!(checked_out_version(project, "bar"), "2.0.0");
    assert_eq!(locked_hex_version(project, "foo"), "1.1.0");
    assert_eq!(locked_hex_version(project, "bar"), "2.0.0");
}

/// `deps update <name>` moves one hex package and leaves the rest locked.
#[test]
fn deps_update_moves_one_hex_package() {
    let mut repo = HexRepo::new(&["foo@1.0.0", "bar@2.0.0"]);
    let project = cli_common::fixture_project();
    let project = project.path();
    write_hex_manifest(project, &[("foo", "~> 1.0"), ("bar", "~> 2.0")]);
    repo.build(project);

    repo.publish("foo@1.1.0").publish("bar@2.1.0");
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.0.0", "pinned");

    repo.beamtalk(project)
        .args(["deps", "update", "foo"])
        .assert()
        .success();
    repo.build(project);
    assert_eq!(checked_out_version(project, "foo"), "1.1.0");
    assert_eq!(checked_out_version(project, "bar"), "2.0.0");
    assert_eq!(locked_hex_version(project, "bar"), "2.0.0");
}
