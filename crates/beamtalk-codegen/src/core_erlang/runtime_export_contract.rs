// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Contract between the code generator and the runtime's exports (BT-4672).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generated modules call into the runtime with `call 'beamtalk_m':'f'(..)`.
//! Nothing at codegen time checks that `beamtalk_m` exports `f` with that
//! arity, so a missing target only shows up as `undef` when the generated
//! code runs (or never, if the arm is dead). This test collects the calls
//! the codegen can emit and checks each against the `-export` attributes of
//! the modules under `runtime/apps/*/src`, without booting a node.
//!
//! Calls are collected two ways:
//!
//! 1. **Source literals.** Every `'beamtalk_m':'f'` inside a Rust string
//!    literal in the non-test codegen source. When the same literal also
//!    holds the whole argument list, the arity is checked too; otherwise the
//!    function name must be exported at some arity.
//! 2. **Primitive tables.** Every lowering function in the primitive
//!    registry is run against every string literal in `primitives/*.rs` as a
//!    candidate selector. This reaches table-driven arms whose function name
//!    is spliced in at render time (the Behaviour tower's zero-argument
//!    table), and the rendered text gives an exact arity.
//!
//! What is excluded is excluded by rule, never by a list of names: comments
//! and doc comments, `#[cfg(test)] mod` bodies and files under a `tests`
//! directory (they hold expected-output strings and fake module names), and
//! any module not named `beamtalk_*` (generated `bt@…` modules, OTP
//! modules).
//!
//! Not covered: call sites that put the module atom in one document piece
//! and take the function name from an enum or argument at render time
//! (`beamtalk_actor_state` has its own export check in
//! `threaded_ir::emit::cell_helper_conformance`).

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::primitives;

/// One `'beamtalk_m':'f'` occurrence and where it came from.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct EmittedCall {
    module: String,
    function: String,
    /// `None` when the argument list was not visible (a source literal
    /// that ends before the closing parenthesis).
    arity: Option<usize>,
    origin: String,
}

/// The `name/arity` entries of every `-export([...])` block in an Erlang
/// source file: one-line and multi-line blocks, several entries per line,
/// entries on the block's opening line, and `%` comments all count. Quoted
/// atom names (`'atRandom'/1`) are returned unquoted.
pub(in crate::core_erlang) fn exported_entries(src: &str) -> BTreeSet<String> {
    let mut exported = BTreeSet::new();
    let mut in_export = false;
    for line in src.lines() {
        let code = line.split('%').next().unwrap_or("").trim_end();
        let mut rest = code;
        // A real attribute starts in column 0; a `-export([` inside a
        // `-doc """` body (indented or mid-line) or a string is not one.
        if let Some(after) = rest.strip_prefix("-export([") {
            in_export = true;
            rest = after;
        }
        if !in_export {
            continue;
        }
        if let Some(close) = rest.find("])") {
            rest = &rest[..close];
            in_export = false;
        }
        for entry in rest.split(',') {
            let entry = entry.trim();
            if let Some((name, arity)) = entry.rsplit_once('/') {
                let name = name.trim().trim_matches('\'');
                exported.insert(format!("{name}/{}", arity.trim()));
            }
        }
    }
    exported
}

/// `module -> {"f/N", ...}` for every `.erl` under `runtime/apps/*/src`.
fn runtime_exports(root: &Path) -> BTreeMap<String, BTreeSet<String>> {
    let apps = root.join("runtime/apps");
    let mut index = BTreeMap::new();
    for app in read_dir_sorted(&apps) {
        let src_dir = app.join("src");
        if !src_dir.is_dir() {
            continue;
        }
        for file in read_dir_sorted(&src_dir) {
            if file.extension().is_some_and(|e| e == "erl") {
                let module = file
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .expect("utf-8 module file name")
                    .to_string();
                let src = std::fs::read_to_string(&file)
                    .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
                index.insert(module, exported_entries(&src));
            }
        }
    }
    index
}

fn read_dir_sorted(dir: &Path) -> Vec<PathBuf> {
    let mut entries: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
        .map(|e| e.expect("dir entry").path())
        .collect();
    entries.sort();
    entries
}

/// Every non-test `.rs` file under `dir`, recursively. A directory named
/// `tests` and a file named `tests.rs` or `*_tests.rs` are test code.
fn non_test_rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for path in read_dir_sorted(dir) {
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if path.is_dir() {
            if name != "tests" {
                non_test_rust_files(&path, out);
            }
        } else if name.ends_with(".rs") && name != "tests.rs" && !name.ends_with("_tests.rs") {
            out.push(path);
        }
    }
}

/// The contents of every string literal in the non-test part of a Rust
/// source file, with the 1-based line it starts on. Comment lines are
/// skipped, and scanning stops at the first `#[cfg(test)]` that opens a
/// `mod` (the in-file unit tests).
fn string_literals(src: &str) -> Vec<(usize, String)> {
    let lines: Vec<&str> = src.lines().collect();
    let mut out = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("#[cfg(test)]")
            && lines[i + 1..]
                .iter()
                .find(|l| !l.trim().is_empty())
                .is_some_and(|l| l.trim_start().starts_with("mod "))
        {
            break;
        }
        if trimmed.starts_with("//") {
            continue;
        }
        let mut chars = line.chars();
        let mut current: Option<String> = None;
        while let Some(c) = chars.next() {
            match (&mut current, c) {
                (None, '"') => current = Some(String::new()),
                // A `//` comment after code ends the line's literals.
                (None, '/') if chars.clone().next() == Some('/') => break,
                // `'"'` is a char literal, not the start of a string.
                (None, '\'') => {
                    let rest: String = chars.clone().take(2).collect();
                    if rest == "\"'" {
                        chars.next();
                        chars.next();
                    }
                }
                (None, _) => {}
                (Some(s), '\\') => {
                    if let Some(escaped) = chars.next() {
                        s.push(escaped);
                    }
                }
                (Some(_), '"') => out.push((i + 1, current.take().expect("open literal"))),
                (Some(s), other) => s.push(other),
            }
        }
    }
    out
}

/// Every `'beamtalk_m':'f'` in `text`, with the arity when the argument list
/// that follows is complete within `text`.
fn calls_in(text: &str, origin: &str) -> Vec<EmittedCall> {
    let mut out = Vec::new();
    let mut search = text;
    while let Some(start) = search.find("'beamtalk_") {
        let after_quote = &search[start + 1..];
        let Some(module_end) = after_quote.find('\'') else {
            break;
        };
        let module = &after_quote[..module_end];
        let rest = &after_quote[module_end + 1..];
        search = rest;
        if !module
            .bytes()
            .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'_')
        {
            continue;
        }
        let Some(fn_part) = rest.strip_prefix(":'") else {
            continue;
        };
        let Some(fn_end) = fn_part.find('\'') else {
            continue;
        };
        let function = &fn_part[..fn_end];
        let tail = &fn_part[fn_end + 1..];
        let arity = tail.strip_prefix('(').and_then(count_args);
        out.push(EmittedCall {
            module: module.to_string(),
            function: function.to_string(),
            arity,
            origin: origin.to_string(),
        });
    }
    out
}

/// The number of top-level arguments in a Core Erlang argument list, given
/// the text just after its opening `(`. `None` if the list does not close.
fn count_args(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    let mut commas = 0usize;
    let mut saw_arg = false;
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\'' | '"' => {
                saw_arg = true;
                while let Some(q) = chars.next() {
                    if q == '\\' {
                        chars.next();
                    } else if q == c {
                        break;
                    }
                }
            }
            // `->` in a `fun` or `case` arm is not a closing bracket.
            '-' if chars.peek() == Some(&'>') => {
                chars.next();
            }
            '(' | '[' | '{' | '<' => {
                saw_arg = true;
                depth += 1;
            }
            ')' | ']' | '}' | '>' => {
                if depth == 0 {
                    return Some(if saw_arg { commas + 1 } else { 0 });
                }
                depth -= 1;
            }
            ',' if depth == 0 => commas += 1,
            c if c.is_whitespace() => {}
            _ => saw_arg = true,
        }
    }
    None
}

/// Calls from string literals in the non-test codegen source.
fn source_literal_calls(root: &Path) -> Vec<EmittedCall> {
    let mut files = Vec::new();
    non_test_rust_files(&root.join("crates/beamtalk-codegen/src"), &mut files);
    let mut out = Vec::new();
    for file in files {
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        let rel = file.strip_prefix(root).unwrap_or(&file).display().to_string();
        for (line, lit) in string_literals(&src) {
            out.extend(calls_in(&lit, &format!("{rel}:{line}")));
        }
    }
    out
}

/// Calls rendered by every primitive lowering for every candidate selector:
/// each string literal in the non-test `primitives/*.rs` source.
fn primitive_table_calls(root: &Path) -> Vec<EmittedCall> {
    let mut files = Vec::new();
    non_test_rust_files(
        &root.join("crates/beamtalk-codegen/src/core_erlang/primitives"),
        &mut files,
    );
    let mut candidates = BTreeSet::new();
    for file in files {
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        candidates.extend(string_literals(&src).into_iter().map(|(_, s)| s));
    }
    let params: Vec<String> = (0..6).map(|i| format!("P{i}")).collect();
    let mut out = Vec::new();
    for (class, lower) in primitives::REGISTRY {
        for selector in &candidates {
            if let Some(rendered) = primitives::doc_to_string(lower(selector, &params)) {
                let origin = format!("primitive {class} {selector:?}");
                out.extend(calls_in(&rendered, &origin));
            }
        }
    }
    out
}

/// The calls among `calls` that the runtime does not export.
fn unexported(
    calls: &[EmittedCall],
    exports: &BTreeMap<String, BTreeSet<String>>,
) -> Vec<String> {
    let mut problems = BTreeSet::new();
    for call in calls {
        let Some(entries) = exports.get(&call.module) else {
            problems.insert(format!(
                "{}: no runtime module {} (for {}:{})",
                call.origin, call.module, call.module, call.function
            ));
            continue;
        };
        let ok = match call.arity {
            Some(n) => entries.contains(&format!("{}/{n}", call.function)),
            None => {
                let prefix = format!("{}/", call.function);
                entries.iter().any(|e| e.starts_with(&prefix))
            }
        };
        if !ok {
            let shown = call
                .arity
                .map_or_else(|| call.function.clone(), |n| format!("{}/{n}", call.function));
            problems.insert(format!(
                "{}: {} does not export {shown}",
                call.origin, call.module
            ));
        }
    }
    problems.into_iter().collect()
}

#[test]
fn every_runtime_call_the_codegen_emits_is_exported() {
    let root = super::tests::repo_root();
    let exports = runtime_exports(&root);
    assert!(
        exports.len() > 50,
        "expected the runtime's modules under runtime/apps/*/src, found {}",
        exports.len()
    );
    let mut calls = source_literal_calls(&root);
    let literal_count = calls.len();
    calls.extend(primitive_table_calls(&root));
    // Guards against the scan silently finding nothing (a moved directory,
    // a changed literal style).
    assert!(
        literal_count > 200,
        "source scan found only {literal_count} runtime calls"
    );
    assert!(
        calls.len() > literal_count + 100,
        "primitive tables rendered only {} runtime calls",
        calls.len() - literal_count
    );
    let problems = unexported(&calls, &exports);
    assert!(
        problems.is_empty(),
        "generated code calls runtime functions that are not exported. Export \
         them from the runtime module, or delete the codegen arm if it is \
         unreachable:\n  {}",
        problems.join("\n  ")
    );
}

#[test]
fn the_tower_table_reaches_every_zero_arg_intrinsic() {
    // The Behaviour tower splices the function name in at render time, so
    // only the primitive-table pass can see these calls.
    let root = super::tests::repo_root();
    let calls = primitive_table_calls(&root);
    assert!(
        calls.iter().any(|c| c.module == "beamtalk_behaviour_intrinsics"
            && c.function == "className"
            && c.arity == Some(1)),
        "className/1 not rendered"
    );
}

#[test]
fn reports_a_missing_function_and_a_wrong_arity() {
    let exports = BTreeMap::from([(
        "beamtalk_m".to_string(),
        BTreeSet::from(["f/1".to_string()]),
    )]);
    let mut calls = calls_in("call 'beamtalk_m':'f'(A, B)", "two-args");
    calls.extend(calls_in("call 'beamtalk_m':'g'(", "open"));
    calls.extend(calls_in("call 'beamtalk_m':'f'(", "open-ok"));
    calls.extend(calls_in("call 'beamtalk_gone':'f'()", "no-module"));
    let problems = unexported(&calls, &exports);
    assert_eq!(problems.len(), 3, "{problems:?}");
    assert!(problems[0].starts_with("no-module:"), "{problems:?}");
    assert!(problems[1].contains("does not export g"), "{problems:?}");
    assert!(problems[2].contains("does not export f/2"), "{problems:?}");
}

#[test]
fn counts_top_level_arguments_only() {
    assert_eq!(count_args(")"), Some(0));
    assert_eq!(count_args("Self)"), Some(1));
    assert_eq!(count_args("Self, [A, B], {C, D})"), Some(3));
    assert_eq!(count_args("'a,b', \"c,d\")"), Some(2));
    assert_eq!(count_args("fun (X) -> X, ~{'k' => V}~)"), Some(2));
    assert_eq!(count_args("case X of <Y> when 'true' -> Y end)"), Some(1));
    assert_eq!(count_args("Self, "), None);
}

#[test]
fn string_literals_skip_comments_and_unit_tests() {
    let src = [
        "/// \"'beamtalk_doc':'x'(A)\"",
        "let a = \"call 'beamtalk_m':'f'(Self)\"; // \"'beamtalk_c':'y'\"",
        "let b = '\"'; let c = \"esc \\\"q\\\"\";",
        "#[cfg(test)]",
        "mod tests {",
        "    const T: &str = \"'beamtalk_test':'z'\";",
        "}",
    ]
    .join("\n");
    let lits: Vec<String> = string_literals(&src).into_iter().map(|(_, s)| s).collect();
    assert_eq!(lits, ["call 'beamtalk_m':'f'(Self)", "esc \"q\""]);
}

#[test]
fn parses_one_line_blocks_and_several_entries_per_line() {
    let src = [
        "% a comment mentioning -export([ignored/9]).",
        "-export([foo/1]).",
        "-export([",
        "    bar/2, baz/0, % a trailing comment",
        "    'qux'/3",
        "]).",
    ]
    .join("\n");
    let entries = exported_entries(&src);
    assert_eq!(entries.len(), 4);
    for entry in ["foo/1", "bar/2", "baz/0", "qux/3"] {
        assert!(entries.contains(entry), "missing {entry}: {entries:?}");
    }
}

#[test]
fn ignores_export_text_inside_doc_bodies() {
    let src = [
        "-doc \"\"\"",
        "Mentions -export([fake/1]). mid-line.",
        "    -export([indented_fake/2]).",
        "\"\"\".",
        "-export([real/0]).",
    ]
    .join("\n");
    let entries = exported_entries(&src);
    assert_eq!(entries.len(), 1, "{entries:?}");
    assert!(entries.contains("real/0"), "{entries:?}");
}

#[test]
fn finds_entries_on_the_opening_line_and_in_later_blocks() {
    let src = "-export([first/1,\n second/2]).\n-export([third/3]).\n";
    let entries = exported_entries(src);
    assert_eq!(entries.len(), 3);
    for entry in ["first/1", "second/2", "third/3"] {
        assert!(entries.contains(entry), "missing {entry}: {entries:?}");
    }
}
