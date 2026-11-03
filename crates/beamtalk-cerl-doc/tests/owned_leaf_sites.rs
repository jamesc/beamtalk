// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4376 (ADR 0089): the `Document::Owned` backing variant is built only
//! inside typed-leaf helpers. Rust cannot narrow the variant's visibility
//! below the enum's, so this test is the enforcement: it scans every
//! workspace crate's `src/` and fails on a construction anywhere but the
//! leaf modules (and the enum's own definition file).

use std::path::{Path, PathBuf};

/// Files allowed to name the variant, relative to the workspace `crates/`
/// directory. Each must name it at least once, so an allowance never
/// outlives its uses and silently exempts a later one.
const ALLOWED_FILES: &[&str] = &[
    // The enum definition, the renderer's match arms, and its unit tests.
    "beamtalk-cerl-doc/src/lib.rs",
    // The Core Erlang typed leaves.
    "beamtalk-cerl-doc/src/leaf.rs",
    // The Beamtalk-source (unparse) typed leaves.
    "beamtalk-core/src/unparse/leaf.rs",
];

/// The banned path, split so this file does not match itself.
const NEEDLE: &str = concat!("Document", "::", "Owned");

fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            rust_sources(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Lines of `src` that name the variant in code (whole-line `//` comments,
/// including doc comments, are skipped).
fn code_hits(src: &str) -> usize {
    src.lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .filter(|line| line.contains(NEEDLE))
        .count()
}

#[test]
fn document_owned_is_built_only_in_leaf_modules() {
    let crates_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate lives under crates/");
    let mut sources = Vec::new();
    for entry in std::fs::read_dir(crates_dir).expect("crates/ is readable") {
        let src_dir = entry.expect("dir entry").path().join("src");
        rust_sources(&src_dir, &mut sources);
    }
    assert!(
        !sources.is_empty(),
        "found no Rust sources under {}",
        crates_dir.display()
    );

    let mut offenders = Vec::new();
    let mut used_allowances = Vec::new();
    for path in &sources {
        let src = std::fs::read_to_string(path).expect("source is readable");
        let hits = code_hits(&src);
        if hits == 0 {
            continue;
        }
        let rel = path
            .strip_prefix(crates_dir)
            .expect("source is under crates/")
            .to_string_lossy()
            .replace('\\', "/");
        if ALLOWED_FILES.contains(&rel.as_str()) {
            used_allowances.push(rel);
        } else {
            offenders.push(format!("{rel} ({hits} line(s))"));
        }
    }

    assert!(
        offenders.is_empty(),
        "{NEEDLE} must only be built inside a typed-leaf helper (ADR 0089); \
         use or add a `leaf::*` constructor instead. Offending files:\n  {}",
        offenders.join("\n  ")
    );
    for allowed in ALLOWED_FILES {
        assert!(
            used_allowances.iter().any(|u| u == allowed),
            "{allowed} is allowed to name {NEEDLE} but no longer does; \
             remove it from ALLOWED_FILES"
        );
    }
}

#[test]
fn code_hits_skips_comments() {
    let src = format!("/// see [`{NEEDLE}`]\n// {NEEDLE}\nlet d = {NEEDLE}(s);\n");
    assert_eq!(code_hits(&src), 1);
}
