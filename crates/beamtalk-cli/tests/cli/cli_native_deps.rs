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
//! Unix-only: `file://` URLs for Windows paths need separate handling.

#![cfg(unix)]

use crate::cli_common;

use std::path::Path;
use std::process::Command;

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
