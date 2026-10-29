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
//!
//! The one non-recording accessor, `state_var_to_store`, is for sites that
//! store the `State` rather than read from it (BT-4256). Its callers are
//! counted per file in [`STORE_SITES`], so a new call is a reviewed edit to
//! that list rather than a silent bypass.

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

/// The non-recording accessor for a `State` that is stored, not read from.
const STORE_CALL: &str = ".state_var_to_store()";

/// Every file that calls [`STORE_CALL`], with how many times. Each call is a
/// store or a plan seed, never a read source: a loop bracket's pre-store
/// (the list, dictionary, counted and `while` loops), an `on:do:` boundary's
/// pre-region `State` (`exception_handling.rs`) and a threading plan's
/// initial state (`plan.rs`, noted where a lowering emits it). Add an entry
/// only for another such site; a read goes through the choke point.
const STORE_SITES: &[(&str, usize)] = &[
    ("control_flow/counted_loops.rs", 1),
    ("control_flow/dict_ops.rs", 2),
    ("control_flow/exception_handling.rs", 2),
    ("control_flow/list_ops/basic_ops.rs", 2),
    ("control_flow/list_ops/filter_ops.rs", 1),
    ("control_flow/list_ops/search_ops.rs", 3),
    ("control_flow/list_ops/transform_ops.rs", 7),
    ("control_flow/plan.rs", 1),
    ("control_flow/while_loops.rs", 1),
];

/// `src` with whole-line comments dropped and all whitespace removed.
///
/// Only a line that *starts* with `//` is dropped: cutting at the first `//`
/// anywhere would also cut at one inside a string literal and hide the rest
/// of that line from the scan (BT-4256). A trailing comment that names a
/// guarded call is counted, which fails loudly rather than hiding a call.
fn code_only(src: &str) -> String {
    src.lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .collect::<String>()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect()
}

/// Every code generator source outside `tests/`, as (path relative to
/// `core_erlang/`, [`code_only`] text).
fn code_generator_sources() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/core_erlang");
    let mut files = Vec::new();
    rust_sources(&root, &mut files);
    assert!(
        files.len() > 20,
        "the walk found the code generator sources"
    );
    files
        .into_iter()
        .map(|file| {
            let rel = file
                .strip_prefix(&root)
                .expect("under core_erlang")
                .to_string_lossy()
                .replace('\\', "/");
            let code = code_only(&std::fs::read_to_string(&file).expect("source is readable"));
            (rel, code)
        })
        .collect()
}

#[test]
fn actor_state_reads_go_through_the_choke_point() {
    let mut bypasses = Vec::new();
    for (rel, code) in code_generator_sources() {
        if ALLOWED_FILES.contains(&rel.as_str()) {
            continue;
        }
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
         or `state_var_to_store` for a store, listed in `STORE_SITES`):\n{}",
        bypasses.join("\n")
    );
}

#[test]
fn state_var_to_store_is_called_only_at_listed_store_sites() {
    let mut unlisted = Vec::new();
    for (rel, code) in code_generator_sources() {
        let allowed = STORE_SITES
            .iter()
            .find(|(f, _)| *f == rel)
            .map_or(0, |(_, n)| *n);
        let found = code.matches(STORE_CALL).count();
        if found != allowed {
            unlisted.push(format!("{rel}: {STORE_CALL} x{found} (expected {allowed})"));
        }
    }
    assert!(
        unlisted.is_empty(),
        "BT-4256: `state_var_to_store` records nothing, so it is only for a site \
         that stores the `State` (a loop pre-store, an `on:do:` pre-region state, \
         a plan seed). A read source goes through `actor_state_read_source`/\
         `actor_state_source`/`actor_state_read_var`; a new store site updates \
         `STORE_SITES`:\n{}",
        unlisted.join("\n")
    );
}

#[test]
fn every_listed_file_still_exists() {
    // BT-4268: the checks above walk the sources and look each file up in
    // the lists, so an entry for a file that was deleted or renamed would sit
    // in a list forever without failing anything. Check the other direction.
    let walked: std::collections::HashSet<String> = code_generator_sources()
        .into_iter()
        .map(|(rel, _)| rel)
        .collect();
    let listed = ALLOWED_FILES
        .iter()
        .map(|f| ("ALLOWED_FILES", *f))
        .chain(
            NOT_READ_SOURCES
                .iter()
                .map(|(f, _, _)| ("NOT_READ_SOURCES", *f)),
        )
        .chain(STORE_SITES.iter().map(|(f, _)| ("STORE_SITES", *f)));
    let stale: Vec<String> = listed
        .filter(|(_, f)| !walked.contains(*f))
        .map(|(list, f)| format!("{list}: {f}"))
        .collect();
    assert!(
        stale.is_empty(),
        "BT-4268: list entries naming a file the walk did not find (deleted or \
         renamed; update or drop the entry):\n{}",
        stale.join("\n")
    );
}

#[test]
fn code_only_keeps_code_after_a_slash_slash_inside_a_string() {
    let src = "// a whole-line comment .state_var_to_store()\n    \
               let url = \"http://x\"; self.state_var_to_store()\n";
    assert_eq!(code_only(src).matches(STORE_CALL).count(), 1);
}
