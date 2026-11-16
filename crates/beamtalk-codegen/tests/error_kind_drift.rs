// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4912: drift test for the error-kind lists that cross the Rust/Erlang
//! boundary.
//!
//! Four hand-kept views of "which error kinds exist" must agree:
//!
//! 1. the kinds Rust codegen emits (`BeamtalkErrorChain::new("kind", ..)` and
//!    any literal `beamtalk_error:new('kind', ..)` text left in codegen source);
//! 2. `kind_to_class/1` in `beamtalk_exception_handler.erl` (the source of
//!    truth for which kinds classify into the `Exception` hierarchy);
//! 3. the kind list in the `#beamtalk_error{}` doc comment in `beamtalk.hrl`;
//! 4. `type ExceptionKind` in `stdlib/src/exception.bt`.
//!
//! "Unknown kinds default to `Error`" is the documented design (ADR 0015), so
//! most kinds need no clause. A kind that is emitted, documented in the
//! `.hrl`, or named in `ExceptionKind` must be either mapped or on the
//! explicit [`INTENTIONALLY_ERROR`] allowlist below, so a new kind cannot be
//! added to one list and silently forgotten in the others.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Kinds that deliberately classify as plain `Error` (no `kind_to_class/1`
/// clause needed): `user_error` is what `self error:` raises, `signal` is
/// what `Exception signal:` raises (the user decides the semantics), and the
/// file/IO kinds wait for a future `IOError` (ADR 0015 Phase 6).
const INTENTIONALLY_ERROR: &[&str] = &[
    "user_error",
    "signal",
    "file_not_found",
    "permission_denied",
    "io_error",
];

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn read(rel: &str) -> String {
    let path = repo_root().join(rel);
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

fn is_kind(s: &str) -> bool {
    !s.is_empty()
        && s.starts_with(|c: char| c.is_ascii_lowercase())
        && s.chars().all(|c| c.is_ascii_lowercase() || c == '_')
}

/// `kind_to_class/1` clauses: kind -> target class (or `None` when the clause
/// delegates, e.g. `invalid_node_name -> kind_to_class(type_error)`).
fn mapped_kinds() -> BTreeMap<String, Option<String>> {
    let src = read("runtime/apps/beamtalk_runtime/src/beamtalk_exception_handler.erl");
    let mut out = BTreeMap::new();
    for line in src.lines() {
        let Some(rest) = line.strip_prefix("kind_to_class(") else {
            continue;
        };
        let Some((kind, tail)) = rest.split_once(") ->") else {
            continue;
        };
        if !is_kind(kind) {
            continue; // the `_` catch-all
        }
        let tail = tail.trim().trim_end_matches(['.', ';']);
        let class = tail
            .strip_prefix('\'')
            .and_then(|t| t.strip_suffix('\''))
            .map(str::to_string);
        out.insert(kind.to_string(), class);
    }
    out
}

/// Kinds listed in the `kind: Error category — one of:` doc comment.
fn hrl_kinds() -> BTreeSet<String> {
    let src = read("runtime/apps/beamtalk_runtime/include/beamtalk.hrl");
    let start = src
        .find("- kind: Error category")
        .expect("hrl kind comment present");
    let end = src[start..].find("- class:").expect("hrl class comment") + start;
    let mut kinds = BTreeSet::new();
    for line in src[start..end].lines().skip(1) {
        // Only the `|`-separated list lines; the ADR note lines are prose.
        if !line.contains('|') && !line.trim_end().ends_with(|c: char| c.is_ascii_lowercase()) {
            continue;
        }
        let body = line.trim_start_matches('%').trim();
        if body.starts_with('(') || body.starts_with('`') {
            continue;
        }
        for tok in body.split('|') {
            let tok = tok.trim();
            if is_kind(tok) {
                kinds.insert(tok.to_string());
            }
        }
    }
    kinds
}

/// Kinds in `type ExceptionKind = #a | #b | ...`.
fn exception_kind_type() -> BTreeSet<String> {
    let src = read("stdlib/src/exception.bt");
    let line = src
        .lines()
        .find(|l| l.starts_with("type ExceptionKind"))
        .expect("type ExceptionKind present");
    line.split('=')
        .nth(1)
        .expect("ExceptionKind has a body")
        .split('|')
        .map(|t| t.trim().trim_start_matches('#').to_string())
        .collect()
}

fn rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("read_dir") {
        let path = entry.expect("entry").path();
        if path.is_dir() {
            rust_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// The text of the `n`-th (0-based) top-level argument of the call whose `(`
/// is at `open`, if it is a plain string literal.
fn string_literal_arg(src: &str, open: usize, n: usize) -> Option<String> {
    let bytes = src.as_bytes();
    let (mut depth, mut arg, mut i) = (0usize, 0usize, open);
    let mut arg_start = open + 1;
    while i < bytes.len() {
        match bytes[i] {
            b'"' => {
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
            }
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => {
                depth -= 1;
                if depth == 0 {
                    return (arg == n).then(|| literal(&src[arg_start..i])).flatten();
                }
            }
            b',' if depth == 1 => {
                if arg == n {
                    return literal(&src[arg_start..i]);
                }
                arg += 1;
                arg_start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    None
}

fn literal(arg: &str) -> Option<String> {
    let t = arg.trim();
    let inner = t.strip_prefix('"')?.strip_suffix('"')?;
    is_kind(inner).then(|| inner.to_string())
}

/// The source up to its trailing inline `#[cfg(test)] mod tests { .. }` block.
///
/// `#[cfg(test)]` also marks single items and out-of-line `mod x;`
/// declarations in the middle of a file, so cutting at the first marker would
/// silently skip everything after it. Only an inline test module is a cut, and
/// the last one is taken so production code after an earlier one is kept.
fn non_test_source(src: &str) -> &str {
    const MARKER: &str = "#[cfg(test)]\n";
    let mut cut = src.len();
    let mut from = 0;
    while let Some(rel) = src[from..].find(MARKER) {
        let at = from + rel;
        let next_line = src[at + MARKER.len()..].lines().next().unwrap_or("");
        if next_line.trim_start().starts_with("mod ") && next_line.trim_end().ends_with('{') {
            cut = at;
        }
        from = at + MARKER.len();
    }
    &src[..cut]
}

/// Kinds emitted by non-test codegen source.
fn emitted_kinds() -> BTreeSet<String> {
    let mut files = Vec::new();
    rust_files(&repo_root().join("crates/beamtalk-codegen/src"), &mut files);
    let mut kinds = BTreeSet::new();
    for file in files {
        let src = fs::read_to_string(&file).expect("read source");
        let src = non_test_source(&src);
        // `BeamtalkErrorChain::new(kind, ..)` (errors.rs) takes the kind first.
        for (idx, _) in src.match_indices("BeamtalkErrorChain::new(") {
            let open = idx + "BeamtalkErrorChain::new".len();
            let kind = string_literal_arg(src, open, 0).unwrap_or_else(|| {
                panic!(
                    "`BeamtalkErrorChain::new` in {} takes a kind the drift scan cannot read; \
                     pass it as an inline string literal of lowercase letters and `_`",
                    file.display()
                )
            });
            kinds.insert(kind);
        }
        for (idx, _) in src.match_indices("'beamtalk_error':'new'('") {
            let rest = &src[idx + "'beamtalk_error':'new'('".len()..];
            if let Some((kind, _)) = rest.split_once('\'')
                && is_kind(kind)
            {
                kinds.insert(kind.to_string());
            }
        }
    }
    kinds
}

fn known() -> BTreeSet<String> {
    let mut k: BTreeSet<String> = mapped_kinds().into_keys().collect();
    k.extend(INTENTIONALLY_ERROR.iter().map(|s| (*s).to_string()));
    k
}

fn unknown_in(label: &str, kinds: &BTreeSet<String>) -> Vec<String> {
    let known = known();
    kinds
        .difference(&known)
        .map(|k| format!("{label}: `{k}`"))
        .collect()
}

#[test]
fn scanners_find_the_known_anchors() {
    // Guard against a scanner silently matching nothing.
    let emitted = emitted_kinds();
    // `user_error` and `class_not_found` are emitted after a mid-file
    // `#[cfg(test)]` item; `empty_collection` goes through `BeamtalkErrorChain` in `primitives/list.rs`.
    for k in [
        "user_error",
        "class_not_found",
        "empty_collection",
        "stateful_block_dispatch",
        "pattern_mismatch",
    ] {
        assert!(
            emitted.contains(k),
            "emitted-kind scan missed `{k}`: {emitted:?}"
        );
    }
    assert!(mapped_kinds().contains_key("does_not_understand"));
    assert!(hrl_kinds().contains("does_not_understand"));
    assert!(exception_kind_type().contains("does_not_understand"));
}

#[test]
fn emitted_kinds_are_mapped_or_intentionally_error() {
    let bad = unknown_in("emitted by codegen", &emitted_kinds());
    assert!(
        bad.is_empty(),
        "add a `kind_to_class/1` clause in beamtalk_exception_handler.erl, or put the kind on \
         INTENTIONALLY_ERROR in this test:\n{}",
        bad.join("\n")
    );
}

#[test]
fn hrl_kinds_are_mapped_or_intentionally_error() {
    let bad = unknown_in("listed in beamtalk.hrl", &hrl_kinds());
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

#[test]
fn hrl_lists_every_mapped_kind() {
    let hrl = hrl_kinds();
    let missing: Vec<_> = mapped_kinds()
        .into_keys()
        .filter(|k| !hrl.contains(k))
        .collect();
    assert!(
        missing.is_empty(),
        "kinds mapped by kind_to_class/1 but missing from the beamtalk.hrl comment: {missing:?}"
    );
}

#[test]
fn exception_kind_type_equals_kind_to_class_set() {
    let mapped: BTreeSet<String> = mapped_kinds().into_keys().collect();
    let ty = exception_kind_type();
    let only_type: Vec<_> = ty.difference(&mapped).collect();
    let only_mapped: Vec<_> = mapped.difference(&ty).collect();
    assert!(
        only_type.is_empty() && only_mapped.is_empty(),
        "`type ExceptionKind` (stdlib/src/exception.bt) must equal the kind_to_class/1 set.\n\
         only in ExceptionKind: {only_type:?}\nonly in kind_to_class/1: {only_mapped:?}"
    );
}

#[test]
fn allowlist_is_not_stale() {
    // An allowlisted kind that gains a non-`Error` clause no longer belongs
    // on the "intentionally Error" list.
    let mapped = mapped_kinds();
    for k in INTENTIONALLY_ERROR {
        if let Some(Some(class)) = mapped.get(*k) {
            assert_eq!(
                class, "Error",
                "`{k}` is on INTENTIONALLY_ERROR but maps to `{class}`; remove it from the list"
            );
        }
    }
}
