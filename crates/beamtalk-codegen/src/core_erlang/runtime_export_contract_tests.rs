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
//! Calls are collected three ways:
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
//! 3. **Open call heads.** A site that writes the module (`'beamtalk_m':` or
//!    `leaf::atom("beamtalk_m"), ":"`) and takes the function name from the
//!    next document piece. When that piece is a literal `leaf::atom("f")`,
//!    the call is read directly. When it comes from an enum or a typed
//!    constant, [`dynamic_calls`] enumerates every value the emitter can
//!    take (`actor_state_helper::ALL`, `class_var_fn::ALL`, the `ALL` arrays
//!    of `SpawnVariant`, `NamedSpawn`, `GuardSense` and the supervisor
//!    kinds). An open head under `primitives/` is covered by pass 2. Any
//!    other open head whose module has no enumeration fails the test, so a
//!    new render-time call site cannot go unchecked.
//!
//! What is excluded is excluded by rule, never by a list of names: comments
//! and doc comments, `#[cfg(test)] mod` bodies and files under a `tests`
//! directory (they hold expected-output strings and fake module names), and
//! any module not named `beamtalk_*` (generated `bt@…` modules, OTP
//! modules).

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::actor_state_access::actor_state_helper;
use super::class_var_access::class_var_fn;
use super::dispatch_codegen::NamedSpawn;
use super::gen_server::SpawnVariant;
use super::guard_returns::GuardSense;
use super::intrinsics::KindOfFn;
use super::primitives;
use super::supervisor_codegen::{ALL_SUPERVISOR_KINDS, supervisor_init_fn};

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
        } else if path.extension().is_some_and(|e| e == "rs")
            && name != "tests.rs"
            && !name.ends_with("_tests.rs")
        {
            out.push(path);
        }
    }
}

/// Whether `lines[i]` is a `#[cfg(test)]` whose next item is an inline
/// `mod x { … }`: the in-file unit tests, where scanning stops. An
/// out-of-line `mod x;` sits among production items, so it does not count.
fn opens_inline_test_mod(lines: &[&str], i: usize) -> bool {
    lines[i].trim_start().starts_with("#[cfg(test)]")
        && lines[i + 1..]
            .iter()
            .find(|l| !l.trim().is_empty())
            .is_some_and(|l| {
                let l = l.trim();
                l.starts_with("mod ") && !l.ends_with(';')
            })
}

/// The contents of every string literal in the non-test part of a Rust
/// source file, with the 1-based line it starts on. Comment lines are
/// skipped, and scanning stops at the first `#[cfg(test)]` that opens an
/// inline `mod x {` (the in-file unit tests). An out-of-line
/// `#[cfg(test)] mod x;` does not stop it.
fn string_literals(src: &str) -> Vec<(usize, String)> {
    let lines: Vec<&str> = src.lines().collect();
    let mut out = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if opens_inline_test_mod(&lines, i) {
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

/// `file` relative to `root`, with `/` separators on every host, so origin
/// strings can be matched with `/`-based checks (e.g. `"/primitives/"`).
fn origin_path(file: &Path, root: &Path) -> String {
    normalize_separators(
        &file
            .strip_prefix(root)
            .unwrap_or(file)
            .display()
            .to_string(),
    )
}

fn normalize_separators(path: &str) -> String {
    path.replace('\\', "/")
}

/// Calls from string literals in the non-test codegen source.
fn source_literal_calls(root: &Path) -> Vec<EmittedCall> {
    let mut files = Vec::new();
    non_test_rust_files(&root.join("crates/beamtalk-codegen/src"), &mut files);
    let mut out = Vec::new();
    for file in files {
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        let rel = origin_path(&file, root);
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

/// The non-test part of a Rust source file with comment lines blanked, so
/// line numbers still match. It ends at the first `#[cfg(test)]` that opens
/// an inline `mod x {`, the same rule as [`string_literals`].
fn production_source(src: &str) -> String {
    let lines: Vec<&str> = src.lines().collect();
    let mut out = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if opens_inline_test_mod(&lines, i) {
            break;
        }
        out.push(if trimmed.starts_with("//") { "" } else { line });
    }
    out.join("\n")
}

/// An open call head: the module is written, and the function name is the
/// next document piece.
#[derive(Debug)]
struct OpenHead {
    module: String,
    /// The function, when the next piece is a literal `leaf::atom("f")`.
    function: Option<String>,
    origin: String,
}

/// Every open call head in `src` (already reduced by [`production_source`]).
fn open_heads(src: &str, origin_file: &str) -> Vec<OpenHead> {
    let patterns = [
        // `"... 'beamtalk_m':"` or `"... 'beamtalk_m':'"`, closing the literal.
        r#"'(beamtalk_[a-z0-9_]+)':'?",\s*"#,
        // `leaf::atom("beamtalk_m"), ":",` (optionally `Document::Str(":")`).
        r#"leaf::atom\("(beamtalk_[a-z0-9_]+)"\),\s*(?:Document::Str\()?":"\)?,\s*"#,
    ];
    let next_atom = regex::Regex::new(r#"^leaf::atom\("([^"]+)"\)"#).expect("regex");
    let mut out = Vec::new();
    for pattern in patterns {
        let re = regex::Regex::new(pattern).expect("regex");
        for caps in re.captures_iter(src) {
            let whole = caps.get(0).expect("match");
            let line = src[..whole.start()].matches('\n').count() + 1;
            out.push(OpenHead {
                module: caps[1].to_string(),
                function: next_atom
                    .captures(&src[whole.end()..])
                    .map(|c| c[1].to_string()),
                origin: format!("{origin_file}:{line}"),
            });
        }
    }
    // `form::call("beamtalk_m", "f", …)` (BT-4685): module and function are
    // both literal arguments of the structural constructor.
    let form_call =
        regex::Regex::new(r#"form::call\(\s*"(beamtalk_[a-z0-9_]+)",\s*"([^"]+)""#).expect("regex");
    for caps in form_call.captures_iter(src) {
        let whole = caps.get(0).expect("match");
        let line = src[..whole.start()].matches('\n').count() + 1;
        out.push(OpenHead {
            module: caps[1].to_string(),
            function: Some(caps[2].to_string()),
            origin: format!("{origin_file}:{line}"),
        });
    }
    out
}

/// Every open call head in the non-test codegen source.
fn source_open_heads(root: &Path) -> Vec<OpenHead> {
    let mut files = Vec::new();
    non_test_rust_files(&root.join("crates/beamtalk-codegen/src"), &mut files);
    let mut out = Vec::new();
    for file in files {
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        let rel = origin_path(&file, root);
        out.extend(open_heads(&production_source(&src), &rel));
    }
    out
}

/// The calls whose function name is chosen at render time, enumerated from
/// the enums and typed constants their emitters take.
fn dynamic_calls() -> Vec<EmittedCall> {
    let call = |module: &str, function: &str, arity: Option<usize>, origin: &str| EmittedCall {
        module: module.to_string(),
        function: function.to_string(),
        arity,
        origin: origin.to_string(),
    };
    let mut out = Vec::new();
    for (name, arity) in actor_state_helper::ALL {
        out.push(call(
            "beamtalk_actor_state",
            name,
            Some(*arity),
            "actor_state_helper::ALL",
        ));
    }
    // `class_var_fn`'s arity is not part of its type: read helpers take an
    // extra capture argument inside a block, so only the name is checked.
    for f in class_var_fn::ALL {
        out.push(call(
            "beamtalk_class_vars",
            f.name(),
            None,
            "class_var_fn::ALL",
        ));
    }
    for f in KindOfFn::ALL {
        out.push(call(
            "beamtalk_behaviour_intrinsics",
            f.name(),
            Some(3),
            "KindOfFn::ALL",
        ));
    }
    for v in SpawnVariant::ALL {
        out.push(call(
            "beamtalk_class_instantiation",
            v.class_self_helper(),
            None,
            "SpawnVariant::class_self_helper",
        ));
        out.push(call(
            "beamtalk_actor",
            v.runtime_function(),
            None,
            "SpawnVariant::runtime_function",
        ));
    }
    for n in NamedSpawn::ALL {
        out.push(call(
            "beamtalk_class_instantiation",
            n.helper(),
            None,
            "NamedSpawn::helper",
        ));
    }
    for g in GuardSense::ALL {
        out.push(call(
            "beamtalk_guard_return",
            g.helper(),
            None,
            "GuardSense::helper",
        ));
    }
    for k in ALL_SUPERVISOR_KINDS {
        out.push(call(
            "beamtalk_supervisor",
            supervisor_init_fn(k),
            None,
            "supervisor_init_fn",
        ));
    }
    out
}

/// Calls read directly off open heads, plus a problem for each open head
/// that nothing enumerates.
fn resolve_open_heads(
    heads: &[OpenHead],
    enumerated: &[EmittedCall],
) -> (Vec<EmittedCall>, Vec<String>) {
    let covered: BTreeSet<&str> = enumerated.iter().map(|c| c.module.as_str()).collect();
    let mut calls = Vec::new();
    let mut problems = Vec::new();
    for head in heads {
        if let Some(function) = &head.function {
            calls.push(EmittedCall {
                module: head.module.clone(),
                function: function.clone(),
                arity: None,
                origin: head.origin.clone(),
            });
        } else if head.origin.contains("/primitives/") || covered.contains(head.module.as_str()) {
            // Pass 2 renders every primitive; `dynamic_calls` covers the rest.
        } else {
            problems.push(format!(
                "{}: the function called on {} is chosen at render time, and nothing \
                 enumerates it. Give its emitter a typed set of values with an `ALL` \
                 list (like `class_var_fn`) and add it to `dynamic_calls`.",
                head.origin, head.module
            ));
        }
    }
    (calls, problems)
}

/// The calls among `calls` that the runtime does not export.
fn unexported(calls: &[EmittedCall], exports: &BTreeMap<String, BTreeSet<String>>) -> Vec<String> {
    let mut problems = BTreeSet::new();
    for call in calls {
        let Some(entries) = exports.get(&call.module) else {
            problems.insert(format!(
                "{}: no runtime module {} (for {}:{})",
                call.origin, call.module, call.module, call.function
            ));
            continue;
        };
        let prefix = format!("{}/", call.function);
        let ok = call.arity.map_or_else(
            || entries.iter().any(|e| e.starts_with(&prefix)),
            |n| entries.contains(&format!("{prefix}{n}")),
        );
        if !ok {
            let shown = call.arity.map_or_else(
                || call.function.clone(),
                |n| format!("{}/{n}", call.function),
            );
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
    let heads = source_open_heads(&root);
    // A floor against the scan silently finding nothing, not a count to keep:
    // sites that build their head with `leaf::atom` (as `call_head` in
    // `primitives/behaviour.rs` does) have no literal head to scan, and the
    // primitive tables they emit for are rendered by pass 2 instead.
    assert!(
        heads.len() > 5,
        "open-head scan found only {} sites",
        heads.len()
    );
    let enumerated = dynamic_calls();
    let (direct, mut problems) = resolve_open_heads(&heads, &enumerated);
    calls.extend(direct);
    calls.extend(enumerated);
    problems.extend(unexported(&calls, &exports));
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
        calls
            .iter()
            .any(|c| c.module == "beamtalk_behaviour_intrinsics"
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
fn string_literals_continue_past_an_out_of_line_test_mod() {
    let src = [
        "#[cfg(test)]",
        "mod sibling_tests;",
        "let a = \"after\";",
        "#[cfg(test)]",
        "mod tests { // unit tests",
        "    const T: &str = \"inside\";",
        "}",
    ]
    .join("\n");
    let lits: Vec<String> = string_literals(&src).into_iter().map(|(_, s)| s).collect();
    assert_eq!(lits, ["after"]);
}

#[test]
fn open_heads_read_literal_functions_and_flag_unenumerated_ones() {
    let src = [
        "let a = docvec![\"call 'beamtalk_known':\", leaf::atom(\"direct\"), \"(\"];",
        "let b = docvec![\"call \", leaf::atom(\"beamtalk_atom\"), \":\", leaf::atom(\"f\")];",
        "let c = docvec![\"call 'beamtalk_enum':'\", leaf::var(x), \"'()\"];",
        "let d = docvec![\"call 'beamtalk_gap':\", leaf::atom(kind.name()), \"(\"];",
    ]
    .join("\n");
    let heads = open_heads(&production_source(&src), "x.rs");
    assert_eq!(heads.len(), 4, "{heads:?}");
    let enumerated = vec![EmittedCall {
        module: "beamtalk_enum".to_string(),
        function: "g".to_string(),
        arity: None,
        origin: "enum".to_string(),
    }];
    let (direct, problems) = resolve_open_heads(&heads, &enumerated);
    let names: Vec<String> = direct
        .iter()
        .map(|c| format!("{}:{}", c.module, c.function))
        .collect();
    assert_eq!(names, ["beamtalk_known:direct", "beamtalk_atom:f"]);
    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(problems[0].starts_with("x.rs:4:"), "{problems:?}");
}

#[test]
fn backslash_origins_are_normalised_so_primitive_heads_are_exempt() {
    // Windows `Path::display()` yields backslashes; this must hold on any host.
    let origin =
        normalize_separators(r"crates\beamtalk-codegen\src\core_erlang\primitives\behaviour.rs:69");
    assert_eq!(
        origin,
        "crates/beamtalk-codegen/src/core_erlang/primitives/behaviour.rs:69"
    );
    let heads = vec![OpenHead {
        module: "beamtalk_behaviour_intrinsics".to_string(),
        function: None,
        origin,
    }];
    let (_, problems) = resolve_open_heads(&heads, &[]);
    assert!(problems.is_empty(), "{problems:?}");
}

#[test]
fn every_open_head_in_the_tree_is_read_or_enumerated() {
    // The direct calls this finds are the ones pass 1 cannot see: the
    // function is a separate `leaf::atom` piece.
    let heads = source_open_heads(&super::tests::repo_root());
    let (direct, problems) = resolve_open_heads(&heads, &dynamic_calls());
    assert!(problems.is_empty(), "{problems:#?}");
    assert!(
        direct
            .iter()
            .any(|c| c.module == "beamtalk_exception_handler" && c.function == "ensure_wrapped"),
        "{direct:?}"
    );
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
