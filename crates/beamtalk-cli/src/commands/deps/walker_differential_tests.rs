// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5073 differential tests: what each of the three dependency-graph
//! walkers answers for the same fixture graph.
//!
//! - **resolve**: `graph::resolve_dependency_graph` (depth-first, fetching,
//!   cycle and conflict errors, topological output).
//! - **roots**: `discover_all_dep_roots` (breadth-first, tolerant).
//! - **offline**: `beamtalk_project::dependency_graph::discover_dependency_graph`.
//!
//! The expectations record today's behaviour so sharing the traversal cannot
//! silently change package set, order, `via_chain` or error kind.

use super::test_support::write_manifest;
use super::*;
use std::fs;
use tempfile::TempDir;

struct Fixture {
    _temp: TempDir,
    root: Utf8PathBuf,
}

/// Build `root/` plus one directory per `(name, deps_toml)` package, all as
/// siblings. A `deps_toml` is the body of a `[dependencies]` table whose
/// entries name sibling packages as `name = { path = "../name" }`.
fn fixture(root_deps: &[&str], pkgs: &[(&str, &[&str])]) -> Fixture {
    let temp = TempDir::new().unwrap();
    let base = temp.path();
    let table = |deps: &[&str]| -> String {
        if deps.is_empty() {
            String::new()
        } else {
            let lines: Vec<String> = deps
                .iter()
                .map(|d| format!("{d} = {{ path = \"../{d}\" }}"))
                .collect();
            format!("[dependencies]\n{}\n", lines.join("\n"))
        }
    };
    fs::create_dir_all(base.join("root")).unwrap();
    write_manifest(&base.join("root"), "my_app", "0.1.0", &table(root_deps));
    for (name, deps) in pkgs {
        let dir = base.join(name);
        fs::create_dir_all(dir.join("src")).unwrap();
        write_manifest(&dir, name, "0.1.0", &table(deps));
    }
    let root = Utf8PathBuf::from_path_buf(base.join("root")).unwrap();
    Fixture { _temp: temp, root }
}

fn names(v: &[String]) -> String {
    v.join(",")
}

/// `resolve_dependency_graph`: `Ok("name[:via|via]...")` or `Err(kind)`.
fn resolve(f: &Fixture) -> String {
    let options = beamtalk_core::CompilerOptions::default();
    match graph::resolve_dependency_graph(&f.root, &options) {
        Ok(deps) => {
            let items: Vec<String> = deps
                .iter()
                .map(|d| {
                    if d.is_direct {
                        format!("{}*", d.name)
                    } else {
                        format!("{}:{}", d.name, names(&d.via_chain))
                    }
                })
                .collect();
            format!("ok {}", items.join(" "))
        }
        Err(e) => {
            let text = format!("{e:?}");
            let kind = if text.contains("Circular dependency") {
                "cycle"
            } else if text.contains("Version conflict") {
                "version-conflict"
            } else if text.contains("Source conflict") {
                "source-conflict"
            } else if text.contains("does not exist") {
                "missing-dir"
            } else if text.contains("missing beamtalk.toml") {
                "missing-manifest"
            } else {
                "other"
            };
            format!("err {kind}")
        }
    }
}

/// `discover_all_dep_roots`: `name[*]:via|deps` in discovery order.
fn roots(f: &Fixture) -> String {
    let manifest = manifest::parse_manifest_full(&f.root.join("beamtalk.toml")).unwrap();
    match discover_all_dep_roots(&f.root, &manifest) {
        Ok(deps) => {
            let items: Vec<String> = deps
                .iter()
                .map(|d| {
                    format!(
                        "{}{}:via[{}]:deps[{}]",
                        d.name,
                        if d.is_direct { "*" } else { "" },
                        names(&d.via_chain),
                        names(&d.deps)
                    )
                })
                .collect();
            format!("ok {}", items.join(" "))
        }
        Err(_) => "err".to_string(),
    }
}

/// The offline walk: `name:deps` in discovery order, plus `all_found`.
fn offline(f: &Fixture) -> String {
    let manifest = manifest::parse_manifest_full(&f.root.join("beamtalk.toml")).unwrap();
    let graph = beamtalk_project::dependency_graph::discover_dependency_graph(
        &f.root,
        manifest.dependencies,
    );
    let items: Vec<String> = graph
        .found
        .iter()
        .map(|d| format!("{}:deps[{}]", d.name, names(&d.dependencies)))
        .collect();
    format!("all_found={} {}", graph.all_found, items.join(" "))
}

#[test]
fn diamond() {
    // root -> a, b ; a -> c ; b -> c
    let f = fixture(&["a", "b"], &[("a", &["c"]), ("b", &["c"]), ("c", &[])]);
    assert_eq!(resolve(&f), "ok c:a b* a*");
    assert_eq!(
        roots(&f),
        "ok a*:via[]:deps[c] b*:via[]:deps[c] c:via[a]:deps[]"
    );
    assert_eq!(offline(&f), "all_found=true a:deps[c] b:deps[c] c:deps[]");
}

#[test]
fn chain_via() {
    // root -> a -> b -> c
    let f = fixture(&["a"], &[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
    assert_eq!(resolve(&f), "ok c:a,b b:a a*");
    assert_eq!(
        roots(&f),
        "ok a*:via[]:deps[b] b:via[a]:deps[c] c:via[a,b]:deps[]"
    );
    assert_eq!(offline(&f), "all_found=true a:deps[b] b:deps[c] c:deps[]");
}

#[test]
fn cycle() {
    // root -> a -> b -> a
    let f = fixture(&["a"], &[("a", &["b"]), ("b", &["a"])]);
    assert_eq!(resolve(&f), "err cycle");
    assert_eq!(roots(&f), "ok a*:via[]:deps[b] b:via[a]:deps[a]");
    assert_eq!(offline(&f), "all_found=true a:deps[b] b:deps[a]");
}

#[test]
fn missing_dependency() {
    // root -> a -> gone (no such directory)
    let f = fixture(&["a"], &[("a", &["gone"])]);
    assert_eq!(resolve(&f), "err missing-dir");
    assert_eq!(roots(&f), "ok a*:via[]:deps[gone] gone:via[a]:deps[]");
    assert_eq!(offline(&f), "all_found=false a:deps[gone]");
}

#[test]
fn unreadable_dependency_manifest() {
    // root -> a, where a's beamtalk.toml is not valid TOML
    let f = fixture(&["a"], &[("a", &[])]);
    let a = f.root.parent().unwrap().join("a");
    fs::write(a.join("beamtalk.toml"), "not [valid toml").unwrap();
    assert_eq!(resolve(&f), "err other");
    assert_eq!(roots(&f), "ok a*:via[]:deps[]");
    assert_eq!(offline(&f), "all_found=false a:deps[]");
}

#[test]
fn path_and_git_mix() {
    // root -> a (path) -> g (git, checkout already under _build/deps/g).
    // `resolve` would fetch `g`, so only the two offline walkers run here.
    let f = fixture(&["a"], &[("a", &[])]);
    let base = f.root.parent().unwrap().to_path_buf();
    fs::write(
        base.join("a/beamtalk.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\n\n[dependencies]\n\
         g = { git = \"https://example.com/g\", tag = \"v1\" }\n",
    )
    .unwrap();
    let g = f.root.join("_build/deps/g");
    fs::create_dir_all(g.join("src")).unwrap();
    write_manifest(g.as_std_path(), "g", "0.1.0", "");
    assert_eq!(roots(&f), "ok a*:via[]:deps[g] g:via[a]:deps[]");
    assert_eq!(offline(&f), "all_found=true a:deps[g] g:deps[]");
}
