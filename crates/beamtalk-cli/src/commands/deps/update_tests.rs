// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk deps update` re-resolves the whole dependency subtree (BT-4920).
//!
//! Every test runs against a local registry index whose packages are real
//! `file://` git repos, so nothing touches the network.

use super::cli::run_update;
use super::graph::resolve_dependency_graph;
use super::lockfile::{LockEntry, Lockfile};
use beamtalk_core::compilation::GitReference;
use camino::{Utf8Path, Utf8PathBuf};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

/// `name = "version"` lines for a `[dependencies]` table.
fn dep_lines(deps: &[(&str, &str)]) -> String {
    deps.iter().fold(String::new(), |mut out, (n, v)| {
        let _ = writeln!(out, "{n} = \"{v}\"");
        out
    })
}

fn git(dir: &std::path::Path, args: &[&str]) {
    let output = Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// A registry index plus one git repo per published package.
struct Registry {
    index: TempDir,
    repos: BTreeMap<String, (TempDir, String)>,
    versions: BTreeMap<String, Vec<String>>,
}

impl Registry {
    fn new() -> Self {
        let index = TempDir::new().unwrap();
        fs::create_dir_all(index.path().join("packages")).unwrap();
        Self {
            index,
            repos: BTreeMap::new(),
            versions: BTreeMap::new(),
        }
    }

    fn index_root(&self) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(self.index.path().to_path_buf()).unwrap()
    }

    /// Publish `name@version` (tag `v<version>`) declaring `deps` as
    /// `(dependency, exact version)` registry requirements.
    fn publish(&mut self, name: &str, version: &str, deps: &[(&str, &str)]) {
        if !self.repos.contains_key(name) {
            let dir = TempDir::new().unwrap();
            git(dir.path(), &["init"]);
            git(dir.path(), &["config", "user.email", "test@test.com"]);
            git(dir.path(), &["config", "user.name", "Test"]);
            git(dir.path(), &["config", "commit.gpgsign", "false"]);
            let mut p = dir.path().display().to_string().replace('\\', "/");
            if !p.starts_with('/') {
                p.insert(0, '/');
            }
            self.repos
                .insert(name.to_string(), (dir, format!("file://{p}")));
        }
        let (dir, _) = &self.repos[name];
        let dep_lines = dep_lines(deps);
        let dep_section = if deps.is_empty() {
            String::new()
        } else {
            format!("\n[dependencies]\n{dep_lines}")
        };
        fs::write(
            dir.path().join("beamtalk.toml"),
            format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n{dep_section}"),
        )
        .unwrap();
        git(dir.path(), &["add", "."]);
        git(dir.path(), &["commit", "-m", &format!("v{version}")]);
        git(dir.path(), &["tag", "-m", version, &format!("v{version}")]);

        self.versions
            .entry(name.to_string())
            .or_default()
            .push(version.to_string());
        let url = &self.repos[name].1;
        let mut entry = format!("name = \"{name}\"\n");
        for v in &self.versions[name] {
            let _ = write!(
                entry,
                "\n[[versions]]\nversion = \"{v}\"\ngit = \"{url}\"\n"
            );
        }
        fs::write(
            self.index
                .path()
                .join("packages")
                .join(format!("{name}.toml")),
            entry,
        )
        .unwrap();
    }

    /// Move `v<version>` of `name` to a new commit (a re-tagged release).
    fn move_tag(&self, name: &str, version: &str) {
        let (dir, _) = &self.repos[name];
        fs::write(dir.path().join("MOVED"), "moved").unwrap();
        git(dir.path(), &["add", "."]);
        git(dir.path(), &["commit", "-m", "moved"]);
        git(
            dir.path(),
            &["tag", "-f", "-m", version, &format!("v{version}")],
        );
    }
}

/// A root project depending on registry `deps`, already built once (so the
/// lock and checkouts exist).
fn project(registry: &Registry, deps: &[(&str, &str)]) -> (TempDir, Utf8PathBuf) {
    let dir = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(dir.path().join("root")).unwrap();
    fs::create_dir_all(&root).unwrap();
    set_deps(&root, registry, deps);
    resolve(&root);
    (dir, root)
}

/// Rewrite the root manifest's pins (what a human does before `deps update`).
fn set_deps(root: &Utf8Path, registry: &Registry, deps: &[(&str, &str)]) {
    let dep_lines = dep_lines(deps);
    fs::write(
        root.join("beamtalk.toml"),
        format!(
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n\
             [registry]\nurl = '{}'\n\n[dependencies]\n{dep_lines}",
            registry.index_root()
        ),
    )
    .unwrap();
}

fn resolve(root: &Utf8Path) {
    resolve_dependency_graph(root, &beamtalk_core::CompilerOptions::default()).unwrap();
}

fn lock(root: &Utf8Path) -> Lockfile {
    Lockfile::read(root).unwrap().unwrap()
}

fn locked_version(root: &Utf8Path, name: &str) -> Option<String> {
    lock(root)
        .get(name)
        .and_then(|e| e.registry_version.as_ref().map(|rv| rv.version.clone()))
}

fn entry(root: &Utf8Path, name: &str) -> LockEntry {
    lock(root).get(name).cloned().unwrap()
}

/// The fixture the issue describes: A@1 -> B@1, A@2 -> B@2, root pinned to A@1.
fn bumped_transitive() -> (Registry, TempDir, Utf8PathBuf) {
    let mut reg = Registry::new();
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("b_pkg", "2.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[("b_pkg", "2.0.0")]);
    let (dir, root) = project(&reg, &[("a_pkg", "1.0.0")]);
    assert_eq!(locked_version(&root, "b_pkg").as_deref(), Some("1.0.0"));
    set_deps(&root, &reg, &[("a_pkg", "2.0.0")]);
    (reg, dir, root)
}

#[test]
fn update_named_relocks_transitive_dep_bumped_by_new_version() {
    let (_reg, _dir, root) = bumped_transitive();
    run_update(&root, Some("a_pkg")).unwrap();
    assert_eq!(locked_version(&root, "a_pkg").as_deref(), Some("2.0.0"));
    assert_eq!(locked_version(&root, "b_pkg").as_deref(), Some("2.0.0"));
    assert_eq!(
        entry(&root, "b_pkg").reference,
        GitReference::Tag("v2.0.0".to_string())
    );
}

#[test]
fn update_all_relocks_transitive_dep_bumped_by_new_version() {
    let (_reg, _dir, root) = bumped_transitive();
    run_update(&root, None).unwrap();
    assert_eq!(locked_version(&root, "a_pkg").as_deref(), Some("2.0.0"));
    assert_eq!(locked_version(&root, "b_pkg").as_deref(), Some("2.0.0"));
}

/// Acceptance criterion 2: a stale transitive lock entry is not built
/// against. A plain build re-resolves it (the lock is repaired, not an error).
#[test]
fn build_after_pin_bump_without_update_still_relocks_transitive() {
    let (_reg, _dir, root) = bumped_transitive();
    resolve(&root);
    assert_eq!(locked_version(&root, "b_pkg").as_deref(), Some("2.0.0"));
}

#[test]
fn update_named_removes_transitive_dropped_by_new_version() {
    let mut reg = Registry::new();
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0")]);
    assert!(lock(&root).get("b_pkg").is_some());

    set_deps(&root, &reg, &[("a_pkg", "2.0.0")]);
    run_update(&root, Some("a_pkg")).unwrap();

    assert!(
        lock(&root).get("b_pkg").is_none(),
        "dropped dep stays locked"
    );
    assert_eq!(locked_version(&root, "a_pkg").as_deref(), Some("2.0.0"));
}

#[test]
fn update_all_removes_transitive_dropped_by_new_version() {
    let mut reg = Registry::new();
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0")]);

    set_deps(&root, &reg, &[("a_pkg", "2.0.0")]);
    run_update(&root, None).unwrap();

    assert!(lock(&root).get("b_pkg").is_none());
}

/// A transitive both A and C need at the same version stays locked as is.
#[test]
fn update_named_keeps_shared_transitive_when_requirements_agree() {
    let mut reg = Registry::new();
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("c_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0"), ("c_pkg", "1.0.0")]);
    let b_before = entry(&root, "b_pkg");
    // B's tag moves; a shared transitive still needed by C must not be
    // re-resolved by `update a_pkg`.
    reg.move_tag("b_pkg", "1.0.0");

    set_deps(&root, &reg, &[("a_pkg", "2.0.0"), ("c_pkg", "1.0.0")]);
    run_update(&root, Some("a_pkg")).unwrap();

    assert_eq!(locked_version(&root, "a_pkg").as_deref(), Some("2.0.0"));
    assert_eq!(entry(&root, "b_pkg"), b_before);
}

/// A@2 needs B@2 but C (not updated) still needs B@1: the single-version
/// policy rejects it, and the lock must be left untouched.
#[test]
fn update_named_conflicting_with_other_dependents_fails_and_keeps_lock() {
    let mut reg = Registry::new();
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("b_pkg", "2.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[("b_pkg", "2.0.0")]);
    reg.publish("c_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0"), ("c_pkg", "1.0.0")]);
    let lock_before = fs::read_to_string(root.join("beamtalk.lock")).unwrap();

    set_deps(&root, &reg, &[("a_pkg", "2.0.0"), ("c_pkg", "1.0.0")]);
    let err = format!("{:?}", run_update(&root, Some("a_pkg")).unwrap_err());

    assert!(
        err.contains("b_pkg"),
        "error should name the dependency: {err}"
    );
    assert!(err.contains("Version conflict"), "Error: {err}");
    assert_eq!(
        fs::read_to_string(root.join("beamtalk.lock")).unwrap(),
        lock_before,
        "a failed update must not rewrite the lock"
    );
}

/// `update <name>` leaves unrelated entries (including their transitives)
/// alone; a bare `update` moves everything.
#[test]
fn update_named_leaves_unrelated_entries_but_update_all_moves_them() {
    let mut reg = Registry::new();
    reg.publish("e_pkg", "1.0.0", &[]);
    reg.publish("d_pkg", "1.0.0", &[("e_pkg", "1.0.0")]);
    reg.publish("b_pkg", "1.0.0", &[]);
    reg.publish("b_pkg", "2.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[("b_pkg", "1.0.0")]);
    reg.publish("a_pkg", "2.0.0", &[("b_pkg", "2.0.0")]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0"), ("d_pkg", "1.0.0")]);
    let d_before = entry(&root, "d_pkg");
    let e_before = entry(&root, "e_pkg");
    reg.move_tag("d_pkg", "1.0.0");
    reg.move_tag("e_pkg", "1.0.0");

    set_deps(&root, &reg, &[("a_pkg", "2.0.0"), ("d_pkg", "1.0.0")]);
    run_update(&root, Some("a_pkg")).unwrap();
    assert_eq!(locked_version(&root, "b_pkg").as_deref(), Some("2.0.0"));
    assert_eq!(
        entry(&root, "d_pkg"),
        d_before,
        "unrelated direct dep moved"
    );
    assert_eq!(
        entry(&root, "e_pkg"),
        e_before,
        "unrelated transitive moved"
    );

    run_update(&root, None).unwrap();
    assert_ne!(entry(&root, "d_pkg").resolved_sha, d_before.resolved_sha);
    assert_ne!(entry(&root, "e_pkg").resolved_sha, e_before.resolved_sha);
}

/// An orphaned entry no dependency reaches is only pruned by a full update.
#[test]
fn update_all_prunes_orphans_but_named_update_keeps_them() {
    let mut reg = Registry::new();
    reg.publish("z_pkg", "1.0.0", &[]);
    reg.publish("a_pkg", "1.0.0", &[]);
    let (_dir, root) = project(&reg, &[("a_pkg", "1.0.0"), ("z_pkg", "1.0.0")]);
    set_deps(&root, &reg, &[("a_pkg", "1.0.0")]);

    run_update(&root, Some("a_pkg")).unwrap();
    assert!(lock(&root).get("z_pkg").is_some());

    run_update(&root, None).unwrap();
    assert!(lock(&root).get("z_pkg").is_none());
}
