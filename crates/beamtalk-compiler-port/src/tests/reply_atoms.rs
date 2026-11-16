// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4938: every atom the port can put in a reply is listed in the shared
//! fixture `compiler_port_reply_atoms.json`.
//!
//! The Erlang side decodes replies with `binary_to_term(Bin, [safe])`, which
//! refuses an atom the VM has not seen. `beamtalk_compiler_port:reply_atoms/0`
//! therefore lists every reply atom so they exist once that module is loaded,
//! and `beamtalk_compiler_port_atoms_tests` checks, in a fresh node, that
//! each atom in the fixture does. This test is the other half: the fixture
//! must be exactly the set of atoms this crate's production code emits, so a
//! new reply atom fails here until it is added to the fixture (and then to
//! `reply_atoms/0`, which the Erlang test enforces).

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

const FIXTURE: &str = "runtime/apps/beamtalk_compiler/test/fixtures/compiler_port_reply_atoms.json";

/// `let <name> = match ... { "literal" ... };` blocks whose result is passed
/// to `atom(<name>)`: the literals inside are reply atoms too.
const DYNAMIC_ATOM_VARS: &[&str] = &["reason", "side"];

fn production_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("read src dir") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n != "tests") {
                production_sources(&path, out);
            }
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Quoted `"..."` literals in `text`.
fn string_literals(text: &str) -> Vec<String> {
    text.split('"')
        .skip(1)
        .step_by(2)
        .map(str::to_string)
        .collect()
}

/// The text between the `(` that opens at `open` and its matching `)`.
fn call_argument(text: &str, open: usize) -> &str {
    let mut depth = 0usize;
    for (i, c) in text[open..].char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return &text[open + 1..open + i];
                }
            }
            _ => {}
        }
    }
    panic!("unbalanced parentheses after `atom(`");
}

fn emitted_atoms() -> BTreeSet<String> {
    let mut files = Vec::new();
    production_sources(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("src"),
        &mut files,
    );
    assert!(files.len() > 5, "found the crate's sources: {files:?}");

    let mut atoms = BTreeSet::new();
    for file in files {
        let raw = std::fs::read_to_string(&file).expect("read source");
        // Drop `#[cfg(test)]` modules and comment lines.
        assert!(
            raw.matches("#[cfg(test)]").count() <= 1,
            "{}: more than one `#[cfg(test)]`; only a trailing test module may be skipped",
            file.display()
        );
        let (raw, test_tail) = raw.split_once("#[cfg(test)]").unwrap_or((&raw, ""));
        assert!(
            test_tail.is_empty() || test_tail.trim_start().starts_with("mod "),
            "{}: `#[cfg(test)]` must introduce the trailing `mod tests`, or production atoms \
             after it would go unscanned",
            file.display()
        );
        let text: String = raw
            .lines()
            .filter(|l| !l.trim_start().starts_with("//"))
            .collect::<Vec<_>>()
            .join("\n");

        // `atom(...)` call sites.
        let mut from = 0;
        while let Some(rel) = text[from..].find("atom(") {
            let at = from + rel;
            from = at + "atom(".len();
            let prev = text[..at].chars().next_back();
            if prev.is_some_and(|c| c.is_alphanumeric() || c == '_') {
                continue; // `term_to_atom(`, `fn ..._atom(`
            }
            if text[..at].trim_end().ends_with("fn") {
                continue; // a definition
            }
            let arg = call_argument(&text, at + "atom".len()).trim();
            if let Some(lit) = arg.strip_prefix('"').and_then(|a| a.strip_suffix('"')) {
                atoms.insert(lit.to_string());
            } else if DYNAMIC_ATOM_VARS.contains(&arg) {
                // Collected from its `let` block below; that block must be
                // in this file, or the scan would silently miss its atoms.
                assert!(
                    text.lines()
                        .any(|l| l.trim_start().starts_with(&format!("let {arg} = match"))),
                    "{}: `atom({arg})` has no `let {arg} = match` block in the same file, so \
                     its atoms cannot be listed. Bind it that way, or teach this test the \
                     new shape.",
                    file.display()
                );
            } else if arg.starts_with("if ") {
                atoms.extend(string_literals(arg)); // `atom(if c { "true" } else { "false" })`
            } else {
                panic!(
                    "{}: `atom({arg})` is not a string literal, a known variable or an `if` of \
                     literals, so its atoms cannot be listed. Use literals, or teach this \
                     test about the new shape.",
                    file.display()
                );
            }
        }

        // `let reason = match ... { ... => "literal", ... };` blocks.
        let lines: Vec<&str> = text.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            let is_dynamic_let = DYNAMIC_ATOM_VARS
                .iter()
                .any(|v| line.trim_start().starts_with(&format!("let {v} = match")));
            if !is_dynamic_let {
                continue;
            }
            for body in &lines[i + 1..] {
                if body.trim() == "};" {
                    break;
                }
                atoms.extend(string_literals(body));
            }
        }
    }
    atoms
}

fn fixture_atoms() -> BTreeSet<String> {
    let path = beamtalk_workspace::cargo_workspace_root!().join(FIXTURE);
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
    let list: Vec<String> = serde_json::from_str(&raw).expect("fixture is a JSON array");
    let set: BTreeSet<String> = list.iter().cloned().collect();
    assert_eq!(set.len(), list.len(), "fixture has duplicate atoms");
    set
}

#[test]
fn reply_atoms_fixture_is_exactly_the_atoms_the_port_emits() {
    let emitted = emitted_atoms();
    let fixture = fixture_atoms();

    let unlisted: Vec<_> = emitted.difference(&fixture).collect();
    assert!(
        unlisted.is_empty(),
        "reply atoms the port emits but {FIXTURE} lacks: {unlisted:?}. Add them there and to \
         `reply_atoms/0` in beamtalk_compiler_port.erl, or a real reply carrying one fails to \
         decode ([safe]) as \"Compiler port response is malformed\"."
    );
    let stale: Vec<_> = fixture.difference(&emitted).collect();
    assert!(
        stale.is_empty(),
        "{FIXTURE} lists atoms the port no longer emits: {stale:?}"
    );
}

#[test]
fn reply_atoms_scan_sees_the_path_error_reasons() {
    // Guards the scanner itself: these two arrive through the dynamic
    // `let reason = match` shape that BT-4938 was about.
    let emitted = emitted_atoms();
    for a in [
        "invalid_path_segment",
        "reserved_test_directory",
        "status",
        "true",
    ] {
        assert!(emitted.contains(a), "scanner missed {a}");
    }
}
