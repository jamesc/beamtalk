// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4369 (ADR 0137 §4b): every actor field write the code generator mints
//! records itself on the innermost loop marker
//! (`CoreErlangGenerator::note_field_write`, `synced_call.rs`), so the loop
//! checkout that brackets can report it
//! (`VerifyError::FieldWriteInBracketedLoop`): a loop that brackets guarded
//! operators admits no field write in its body. A site that built a field
//! `Put` without the hook would slip past that rule, so the field `Put`
//! constructions and the hook's callers are counted per file in
//! [`FIELD_WRITE_SITES`]: a new site is a reviewed edit to that list, not a
//! silent bypass. Mirrors `stale_read_sites.rs` (BT-4180).
//!
//! Two kinds of write are counted. A static field write (`self.x := v`) builds
//! a `BindOp::Put { field: .. }` ([`FIELD_WRITE_SITES`]). A dynamic-key write
//! (`self fieldAt: k put: v`, `self clearField: k`) builds a raw `maps:put` or
//! `maps:remove` document on a name variable and is noted as `"<dynamic>"`
//! ([`DYNAMIC_FIELD_WRITE_SITES`]); a self-send keeps such a body off the
//! stateless paths too, so the record is a second guard, not the only one.

use std::path::{Path, PathBuf};

/// A field `Put` construction: `BindOp::Put { field: <expr>, .. }`, matched
/// with whitespace removed.
const FIELD_PUT: &str = "BindOp::Put{field:";

/// The slot-key `Put` of a local rebind (`lower_local_var_assignment_bind`):
/// not a field write, and not counted.
const LOCAL_SLOT_PUT: &str = "BindOp::Put{field:state_key";

/// The hook's call, with a leading `.` so its definition is not counted.
const NOTE_CALL: &str = ".note_field_write(";

/// The hook's call for a dynamic key, counted apart from [`NOTE_CALL`]'s
/// static callers.
const NOTE_DYNAMIC_CALL: &str = ".note_field_write(\"<dynamic>\"";

/// A dynamic-key field mutation: a `maps:put` or `maps:remove` on a name
/// variable, matched with whitespace removed.
const DYNAMIC_MUTATIONS: &[&str] = &[
    "'maps':'put'(\",leaf::var(name_var)",
    "'maps':'remove'(\",leaf::var(name_var)",
];

/// Production code ends where the inline test module starts.
const TEST_MODULE: &str = "#[cfg(test)]modtests";

/// Every file that builds a field `Put` or calls the hook, with how many of
/// each: (path relative to `core_erlang/`, field `Put`s, hook calls). A hook
/// call may cover more than one `Put` in its function, and
/// `lower_hybrid_field_rebind` calls the hook for a rebind that is a `Direct`
/// bind, not a `Put`.
const FIELD_WRITE_SITES: &[(&str, usize, usize)] = &[
    // `lower_simple_field_write_bind`, the C6 non-producer two-hop, and
    // `lower_hybrid_field_rebind` (no `Put`).
    ("control_flow/conditionals.rs", 2, 3),
    // The method-body field assignments: control-flow RHS and the two
    // `FieldAssignment` bodies.
    ("gen_server/methods.rs", 3, 3),
];

/// Field-shaped `Put`s that are not field writes, with how many each file
/// builds: the emitter renders a frame-slot rebind (`key` is a slot key naming
/// a local, `threaded_ir/emit.rs`). Not counted as field writes.
const NOT_FIELD_WRITES: &[(&str, usize)] = &[("threaded_ir/emit.rs", 1)];

/// Every file that builds a dynamic-key field mutation or notes one, with how
/// many of each: (path relative to `core_erlang/`, mutations, `"<dynamic>"`
/// hook calls). `fieldAt:put:` and `clearField:` in the open-document
/// lowering, in a conditional arm (three kinds) and in a control-flow method
/// body.
const DYNAMIC_FIELD_WRITE_SITES: &[(&str, usize, usize)] = &[
    ("control_flow/conditionals.rs", 3, 3),
    ("dispatch_codegen/field_assignment.rs", 2, 2),
    ("gen_server/methods.rs", 1, 1),
];

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

/// `src` with whole-line comments dropped and all whitespace removed, cut at
/// the inline test module so hand-built test fixtures are not counted.
fn production_code(src: &str) -> String {
    let code: String = src
        .lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .collect::<String>()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    match code.find(TEST_MODULE) {
        Some(at) => code[..at].to_string(),
        None => code,
    }
}

#[test]
fn field_writes_record_themselves_for_the_loop_checkout() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/core_erlang");
    let mut files = Vec::new();
    rust_sources(&root, &mut files);
    assert!(
        files.len() > 20,
        "the walk found the code generator sources"
    );
    let mut found: Vec<(String, usize, usize)> = Vec::new();
    let mut found_dynamic: Vec<(String, usize, usize)> = Vec::new();
    for file in files {
        let rel = file
            .strip_prefix(&root)
            .expect("under core_erlang")
            .to_string_lossy()
            .replace('\\', "/");
        let code = production_code(&std::fs::read_to_string(&file).expect("source is readable"));
        let not_field = NOT_FIELD_WRITES
            .iter()
            .find(|(f, _)| *f == rel)
            .map_or(0, |(_, n)| *n);
        let puts =
            code.matches(FIELD_PUT).count() - code.matches(LOCAL_SLOT_PUT).count() - not_field;
        let dynamic_notes = code.matches(NOTE_DYNAMIC_CALL).count();
        let notes = code.matches(NOTE_CALL).count() - dynamic_notes;
        let dynamic_writes: usize = DYNAMIC_MUTATIONS
            .iter()
            .map(|pattern| code.matches(pattern).count())
            .sum();
        if puts > 0 || notes > 0 {
            found.push((rel.clone(), puts, notes));
        }
        if dynamic_writes > 0 || dynamic_notes > 0 {
            found_dynamic.push((rel, dynamic_writes, dynamic_notes));
        }
    }
    found.sort();
    let mut expected: Vec<(String, usize, usize)> = FIELD_WRITE_SITES
        .iter()
        .map(|(f, p, n)| ((*f).to_string(), *p, *n))
        .collect();
    expected.sort();
    assert_eq!(
        found, expected,
        "BT-4369: a field `Put` (or a `note_field_write` call) appeared, moved or vanished. \
         Every actor field write must call `note_field_write` so a loop that brackets \
         can report it; update `FIELD_WRITE_SITES` only for a site that does."
    );
    for (rel, puts, notes) in &found {
        assert!(
            notes >= puts,
            "{rel}: {puts} field `Put`s but only {notes} `note_field_write` calls"
        );
    }
    found_dynamic.sort();
    let mut expected_dynamic: Vec<(String, usize, usize)> = DYNAMIC_FIELD_WRITE_SITES
        .iter()
        .map(|(f, w, n)| ((*f).to_string(), *w, *n))
        .collect();
    expected_dynamic.sort();
    assert_eq!(
        found_dynamic, expected_dynamic,
        "BT-4369: a dynamic-key field mutation (`fieldAt:put:`, `clearField:`) appeared, moved or \
         vanished. Each must call `note_field_write(\"<dynamic>\", ..)`; update \
         `DYNAMIC_FIELD_WRITE_SITES` only for a site that does."
    );
    for (rel, writes, notes) in &found_dynamic {
        assert!(
            notes >= writes,
            "{rel}: {writes} dynamic field mutations but only {notes} `<dynamic>` hook calls"
        );
    }
}
