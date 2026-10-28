// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4180 (ADR 0137 §4b): every actor-state read site asks the one choke
//! point (`CoreErlangGenerator::actor_state_read_source` and its
//! `actor_state_read_var`/`actor_state_source` helpers, in `synced_call.rs`)
//! for its source, which records the source for the bracketing loop's
//! checkout (`VerifyError::StaleReadInBracketedLoop`). A site that resolved
//! the state map itself would bypass the record, so the accessors the choke
//! point is built from are called nowhere else in the code generator: a new
//! emission site that reaches for one fails here instead of silently.

use std::path::{Path, PathBuf};

/// The calls only the choke point (and the accessor's own definition) make,
/// matched with whitespace removed.
const CHOKE_POINT_ONLY: &[&str] = &[
    ".current_field_read_state_var()",
    ".guarded_op_take_reads()",
    ".hybrid_readonly_field_params.get(",
];

/// Files allowed to make those calls: the choke point and the accessor's
/// definition.
const ALLOWED_FILES: &[&str] = &["synced_call.rs", "generator/version.rs"];

/// Calls that are not read sources, with how many times each file makes one:
/// a hybrid field write rebinds the parameter it reads the chain source of
/// (`lower_hybrid_field_rebind`).
const NOT_READ_SOURCES: &[(&str, &str, usize)] = &[(
    "control_flow/conditionals.rs",
    ".hybrid_readonly_field_params.get(",
    1,
)];

fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("core_erlang source dir is readable") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "tests") {
                continue;
            }
            rust_sources(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// `src` with line comments dropped and all whitespace removed.
fn code_only(src: &str) -> String {
    src.lines()
        .map(|line| line.split("//").next().unwrap_or(""))
        .collect::<String>()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect()
}

#[test]
fn actor_state_reads_go_through_the_choke_point() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/core_erlang");
    let mut files = Vec::new();
    rust_sources(&root, &mut files);
    assert!(
        files.len() > 20,
        "the walk found the code generator sources"
    );
    let mut bypasses = Vec::new();
    for file in files {
        let rel = file
            .strip_prefix(&root)
            .expect("under core_erlang")
            .to_string_lossy()
            .replace('\\', "/");
        if ALLOWED_FILES.contains(&rel.as_str()) {
            continue;
        }
        let code = code_only(&std::fs::read_to_string(&file).expect("source is readable"));
        for call in CHOKE_POINT_ONLY {
            let allowed = NOT_READ_SOURCES
                .iter()
                .find(|(f, c, _)| *f == rel && c == call)
                .map_or(0, |(_, _, n)| *n);
            let found = code.matches(call).count();
            if found != allowed {
                bypasses.push(format!("{rel}: {call} x{found} (expected {allowed})"));
            }
        }
    }
    assert!(
        bypasses.is_empty(),
        "BT-4180: an actor-state read source resolved outside the choke point \
         (call `actor_state_read_source`/`actor_state_source`/`actor_state_read_var`, \
         or `state_var_to_store` for a store):\n{}",
        bypasses.join("\n")
    );
}
