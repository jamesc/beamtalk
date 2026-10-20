// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source lint: every **named** `erl` spawn in `beamtalk-cli` goes through
//! `beamtalk_workspace::pin_epmd_loopback` (BT-3987).
//!
//! A named node (`-sname`/`-name`) starts `epmd -daemon` when none is running,
//! and that daemon binds every interface unless `ERL_EPMD_ADDRESS` is set. One
//! unpinned spawn is enough to leave a wildcard epmd behind for the rest of
//! the session, so the rule is checked over the crate's source rather than
//! trusted to review.
//!
//! The scan is line-based (no `syn` dependency). For each `erl` spawn
//! (`Command::new("erl")` / `Command::new("erl.exe")`) it takes the enclosing
//! `fn` (from the preceding `fn` line to the next one) and requires a
//! `pin_epmd_loopback(` call there when:
//!
//! * that `fn` mentions a named-node marker (`"-sname"`, `"-name"`,
//!   `node_name_flag(`), or
//! * the *file* mentions one, unless the spawn carries an
//!   `// epmd: unnamed node` opt-out comment on one of the few lines above it.
//!   This catches node flags built in a different `fn` of the same file than
//!   the spawn.
//!
//! Comment-only lines are ignored, so a `-sname` in a doc comment is not a
//! marker, and file-level markers are only looked for above the first
//! `#[cfg(test)]` line.

use std::path::{Path, PathBuf};

/// Literals naming the `erl` program in a `Command::new(..)` spawn.
const ERL_SPAWNS: &[&str] = &["Command::new(\"erl\")", "Command::new(\"erl.exe\")"];

/// Code that makes an `erl` invocation a named (distributed) node.
const NAMED_NODE_MARKERS: &[&str] = &["\"-sname\"", "\"-name\"", "node_name_flag("];

/// The shared helper every named spawn must call.
const PIN_CALL: &str = "pin_epmd_loopback(";

/// Opt-out for an unnamed `erl` spawn in a file that also starts named ones.
const UNNAMED_OPT_OUT: &str = "// epmd: unnamed node";

/// How many lines above a spawn the opt-out comment may sit.
const OPT_OUT_WINDOW: usize = 3;

/// Whether `line` declares a function (`fn`, optionally `pub`/`async`/...).
fn is_fn_decl(line: &str) -> bool {
    let mut rest = line.trim_start();
    loop {
        let before = rest;
        for prefix in [
            "pub(crate) ",
            "pub(super) ",
            "pub ",
            "async ",
            "const ",
            "unsafe ",
        ] {
            if let Some(r) = rest.strip_prefix(prefix) {
                rest = r;
            }
        }
        if rest == before {
            break;
        }
    }
    rest.starts_with("fn ")
}

fn is_comment(line: &str) -> bool {
    line.trim_start().starts_with("//")
}

fn has_marker(lines: &[&str]) -> bool {
    lines
        .iter()
        .filter(|l| !is_comment(l))
        .any(|l| NAMED_NODE_MARKERS.iter().any(|m| l.contains(m)))
}

/// Violations in one file's `source`, as `"<label>:<line>"` strings.
fn violations_in(label: &str, source: &str) -> (usize, Vec<String>) {
    let lines: Vec<&str> = source.lines().collect();
    // File-level markers count only above a `#[cfg(test)]` module, so an
    // assertion like `!vm_args.contains("-sname")` doesn't make a file named.
    let non_test_end = lines
        .iter()
        .position(|l| l.trim() == "#[cfg(test)]")
        .unwrap_or(lines.len());
    let file_named = has_marker(&lines[..non_test_end]);
    let mut named_spawns = 0;
    let mut violations = Vec::new();

    for (idx, line) in lines.iter().enumerate() {
        if is_comment(line) || !ERL_SPAWNS.iter().any(|s| line.contains(s)) {
            continue;
        }
        let start = lines[..idx]
            .iter()
            .rposition(|l| !is_comment(l) && is_fn_decl(l))
            .unwrap_or(0);
        let end = lines[idx + 1..]
            .iter()
            .position(|l| !is_comment(l) && is_fn_decl(l))
            .map_or(lines.len(), |p| idx + 1 + p);
        let body = &lines[start..end];

        let fn_named = has_marker(body);
        let opted_out = lines[idx.saturating_sub(OPT_OUT_WINDOW)..idx]
            .iter()
            .any(|l| l.contains(UNNAMED_OPT_OUT));
        if !(fn_named || (file_named && !opted_out)) {
            continue;
        }
        named_spawns += 1;
        let pinned = body
            .iter()
            .filter(|l| !is_comment(l))
            .any(|l| l.contains(PIN_CALL));
        if !pinned {
            violations.push(format!("{label}:{}", idx + 1));
        }
    }
    (named_spawns, violations)
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

#[test]
fn named_erl_spawns_pin_epmd_loopback() {
    let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in ["src", "tests", "benches"] {
        collect_rs_files(&crate_root.join(dir), &mut files);
    }
    let mut named_spawns = 0;
    let mut violations = Vec::new();
    for path in files {
        // This file's own fixtures contain spawns and markers on purpose.
        if path.ends_with("named_erl_spawn_lint.rs") {
            continue;
        }
        let source = std::fs::read_to_string(&path).expect("read source file");
        let label = path.strip_prefix(crate_root).unwrap_or(&path).display();
        let (n, v) = violations_in(&label.to_string(), &source);
        named_spawns += n;
        violations.extend(v);
    }

    assert!(
        violations.is_empty(),
        "named-node `erl` spawns that skip `beamtalk_workspace::pin_epmd_loopback` \
         (an epmd they start would bind 0.0.0.0, BT-3987). Call the helper on the \
         Command, or mark a genuinely unnamed spawn with `{UNNAMED_OPT_OUT}`:\n  {}",
        violations.join("\n  ")
    );
    // Guard against the scan silently matching nothing: workspace startup,
    // `beamtalk run`'s RPC client, `beamtalk repl --node` and the
    // repl_protocol fixtures are all named spawns.
    assert!(
        named_spawns >= 4,
        "expected at least 4 named `erl` spawns, found {named_spawns}; is the scan broken?"
    );
}

#[test]
fn lint_flags_an_unpinned_named_spawn() {
    let src = r#"
fn start() {
    let mut cmd = Command::new("erl");
    cmd.arg("-sname").arg("n@localhost");
    cmd.spawn();
}
"#;
    let (n, v) = violations_in("x.rs", src);
    assert_eq!(n, 1);
    assert_eq!(v, vec!["x.rs:3".to_string()]);
}

#[test]
fn lint_accepts_a_pinned_named_spawn() {
    let src = r#"
pub(crate) fn start() {
    let mut cmd = Command::new("erl");
    cmd.arg(node_name_flag(&n)).arg(&n);
    beamtalk_workspace::pin_epmd_loopback(&mut cmd);
}
"#;
    assert_eq!(violations_in("x.rs", src), (1, vec![]));
}

#[test]
fn lint_flags_flags_built_in_another_fn_of_the_same_file() {
    let src = r#"
fn args() -> Vec<&'static str> {
    vec!["-name", "n@host.example.com"]
}

fn spawn() {
    Command::new("erl").args(args()).spawn();
}
"#;
    let (n, v) = violations_in("x.rs", src);
    assert_eq!(n, 1);
    assert_eq!(v, vec!["x.rs:7".to_string()]);
}

#[test]
fn lint_honours_the_unnamed_opt_out_and_ignores_comments() {
    let src = r#"
/// Starts `erl -sname` elsewhere; this one is plain.
fn script() {
    // epmd: unnamed node — `-noshell` script runner, no distribution.
    Command::new("erl").arg("-noshell").spawn();
}

fn other() {
    // a "-sname" in a comment is not a marker
    Command::new("erl").arg("-noshell").spawn();
}
"#;
    // No non-comment marker anywhere: nothing is named.
    assert_eq!(violations_in("x.rs", src), (0, vec![]));

    let with_named = format!("{src}\nfn named() {{ let f = \"-sname\"; }}\n");
    // `script` is opted out; `other` is not and the file is now named.
    let (n, v) = violations_in("x.rs", &with_named);
    assert_eq!(n, 1);
    assert_eq!(v, vec!["x.rs:10".to_string()]);
}

#[test]
fn lint_ignores_file_level_markers_inside_test_modules() {
    let src = r#"
fn extract() {
    Command::new("erl").arg("-noshell").output();
}

#[cfg(test)]
mod tests {
    fn no_sname() { assert!(!s.contains("-sname")); }
}
"#;
    assert_eq!(violations_in("x.rs", src), (0, vec![]));
}
