// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source lint: every `erl` spawn in `beamtalk-cli` pins epmd's bind address
//! through `beamtalk_workspace` (BT-3987).
//!
//! A distributed `erl` starts `epmd -daemon` when none is running, and that
//! daemon binds every interface unless `ERL_EPMD_ADDRESS` is set. Node flags
//! can reach any `erl` (`-sname`/`-name` in its args, or via `ERL_FLAGS`,
//! `ERL_AFLAGS`, `ERL_ZFLAGS` or an `-args_file`), and the variable is ignored
//! by a node that stays undistributed, so the rule is simply "every `erl`
//! spawn is pinned", not "every named one". One unpinned spawn is enough to
//! leave a wildcard epmd behind for the rest of the session, so the rule is
//! checked over the crate's source rather than trusted to review.
//!
//! The scan is line-based (no `syn` dependency) and skips comment-only lines:
//!
//! * A bare `Command::new("erl")` / `Command::new("erl.exe")` must be followed,
//!   in its enclosing `fn`, by a `pin_epmd_loopback(` call. Prefer
//!   `beamtalk_workspace::erl_command()`, which is already pinned.
//! * A `fn` that spawns `erl` (either way) and calls `env_clear()` must call
//!   `pin_epmd_loopback(` *after* its last `env_clear()`, which would
//!   otherwise wipe the pin.
//!
//! The enclosing `fn` is the span from the preceding `fn` line to the next
//! one. `erl` reached through a computed path (`Command::new(erl_path)`) is
//! not detected; no such spawn exists in this crate today.

use std::path::{Path, PathBuf};

/// A bare `erl` spawn, which must be pinned explicitly.
const BARE_ERL_SPAWNS: &[&str] = &["Command::new(\"erl\")", "Command::new(\"erl.exe\")"];

/// The pinned constructor.
const PINNED_CONSTRUCTOR: &str = "erl_command()";

/// The shared helper.
const PIN_CALL: &str = "pin_epmd_loopback(";

const ENV_CLEAR: &str = "env_clear()";

/// Whether `line` declares a function (`fn`, optionally `pub`/`async`/...).
fn is_fn_decl(line: &str) -> bool {
    let mut rest = line.trim_start();
    loop {
        let before = rest;
        if let Some(r) = rest.strip_prefix("pub(") {
            rest = r.split_once(") ").map_or(r, |(_, after)| after);
        }
        for prefix in ["pub ", "async ", "const ", "unsafe ", "extern \"C\" "] {
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

/// Violations in one file's `source` as `"<label>:<line>: <why>"`, and the
/// number of `erl` spawns seen.
fn violations_in(label: &str, source: &str) -> (usize, Vec<String>) {
    let lines: Vec<&str> = source.lines().collect();
    let code = |i: usize| (!is_comment(lines[i])).then_some(lines[i]);
    let mut spawns = 0;
    let mut violations = Vec::new();

    for idx in 0..lines.len() {
        let Some(line) = code(idx) else { continue };
        let bare = BARE_ERL_SPAWNS.iter().any(|s| line.contains(s));
        // The constructor's own definition is not a spawn site.
        let pinned_ctor = line.contains(PINNED_CONSTRUCTOR) && !line.contains("fn erl_command");
        if !bare && !pinned_ctor {
            continue;
        }
        spawns += 1;

        let start = (0..idx)
            .rev()
            .find(|&i| code(i).is_some_and(is_fn_decl))
            .unwrap_or(0);
        let end = (idx + 1..lines.len())
            .find(|&i| code(i).is_some_and(is_fn_decl))
            .unwrap_or(lines.len());
        let last = |pat: &str| {
            (start..end)
                .rev()
                .find(|&i| code(i).is_some_and(|l| l.contains(pat)))
        };
        let last_pin = last(PIN_CALL);
        let last_clear = last(ENV_CLEAR);

        if bare && last_pin.is_none_or(|p| p < idx) {
            violations.push(format!(
                "{label}:{}: bare `erl` spawn without `pin_epmd_loopback` \
                 (use `beamtalk_workspace::erl_command()`)",
                idx + 1
            ));
        } else if let Some(clear) = last_clear.filter(|&c| c > idx) {
            if last_pin.is_none_or(|p| p <= clear) {
                violations.push(format!(
                    "{label}:{}: `env_clear()` after the `erl` spawn wipes the epmd pin; \
                     call `pin_epmd_loopback` after it",
                    clear + 1
                ));
            }
        }
    }
    (spawns, violations)
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
fn erl_spawns_pin_epmd_loopback() {
    let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in ["src", "tests", "benches"] {
        collect_rs_files(&crate_root.join(dir), &mut files);
    }

    let mut spawns = 0;
    let mut violations = Vec::new();
    for path in files {
        // This file's own fixtures contain unpinned spawns on purpose.
        if path.ends_with("erl_spawn_lint.rs") {
            continue;
        }
        let source = std::fs::read_to_string(&path).expect("read source file");
        let label = path.strip_prefix(crate_root).unwrap_or(&path).display();
        let (n, v) = violations_in(&label.to_string(), &source);
        spawns += n;
        violations.extend(v);
    }

    assert!(
        violations.is_empty(),
        "`erl` spawns that skip the epmd loopback pin (an epmd they start would \
         bind 0.0.0.0, BT-3987):\n  {}",
        violations.join("\n  ")
    );
    // Guard against the scan silently matching nothing: this crate has well
    // over a dozen `erl` spawns (workspace startup, run, repl, test runners,
    // release assembly, erlfmt, doctor, and the integration-test fixtures).
    assert!(
        spawns >= 15,
        "expected at least 15 `erl` spawns, found {spawns}; is the scan broken?"
    );
}

#[test]
fn lint_flags_a_bare_unpinned_spawn() {
    let src = r#"
fn start() {
    let mut cmd = Command::new("erl");
    cmd.arg("-noshell");
    cmd.spawn();
}
"#;
    let (n, v) = violations_in("x.rs", src);
    assert_eq!(n, 1);
    assert_eq!(v.len(), 1);
    assert!(v[0].starts_with("x.rs:3:"), "{v:?}");
}

#[test]
fn lint_accepts_a_pinned_bare_spawn_and_the_constructor() {
    let src = r#"
pub(crate) fn start() {
    let mut cmd = Command::new("erl");
    cmd.env_clear();
    beamtalk_workspace::pin_epmd_loopback(&mut cmd);
}

pub(in crate::commands) fn other() {
    beamtalk_workspace::erl_command().arg("-noshell").output();
}
"#;
    assert_eq!(violations_in("x.rs", src), (2, vec![]));
}

#[test]
fn lint_flags_a_pin_before_the_spawn() {
    let src = r#"
fn start() {
    let mut other = Command::new("sh");
    beamtalk_workspace::pin_epmd_loopback(&mut other);
    let mut cmd = Command::new("erl");
    cmd.spawn();
}
"#;
    let (_, v) = violations_in("x.rs", src);
    assert_eq!(v.len(), 1);
    assert!(v[0].starts_with("x.rs:5:"), "{v:?}");
}

#[test]
fn lint_flags_env_clear_after_the_pin() {
    let src = r#"
fn start() {
    let mut cmd = beamtalk_workspace::erl_command();
    cmd.env_clear();
    cmd.spawn();
}

fn start_bare() {
    let mut cmd = Command::new("erl");
    beamtalk_workspace::pin_epmd_loopback(&mut cmd);
    cmd.env_clear();
}
"#;
    let (n, v) = violations_in("x.rs", src);
    assert_eq!(n, 2);
    assert_eq!(v.len(), 2, "{v:?}");
    assert!(v[0].starts_with("x.rs:4:"), "{v:?}");
    assert!(v[1].starts_with("x.rs:11:"), "{v:?}");
}

#[test]
fn lint_ignores_comments() {
    let src = r#"
/// Runs `Command::new("erl")` somewhere else.
fn doc() {
    // Command::new("erl") in a comment is not a spawn
}
"#;
    assert_eq!(violations_in("x.rs", src), (0, vec![]));
}
