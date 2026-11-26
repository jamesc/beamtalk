// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Op-sender coverage (BT-5199): every op in the REPL op table must have a
//! sender — a client in this repo that builds a request for it — or a row in
//! the parity doc's "Op Senders Not Found by the Scanner" table saying who
//! sends it (or which open issue wires one). An op nobody sends is dead
//! protocol surface.
//!
//! Senders are read from source, never from comments or tests:
//!
//! * **Rust** (CLI, MCP, LSP and every other crate): calls of a
//!   `RequestBuilder` constructor (`RequestBuilder::close(`) outside
//!   `#[cfg(test)]` modules and test-only files. Each constructor's op comes
//!   from its body in `request.rs` (`"op": "x"`, `no_param("x")`, or a
//!   `Self::other(` delegation).
//! * **VS Code**: `op: "x"` request literals in `editors/vscode/src/` (the
//!   `__tests__` tree excluded).
//! * **LiveView**: `"op" => "x"`, `dispatch_<kind>("x"`, `:dispatch, ["x"`,
//!   `:request, ["x"` and `request_op("x"` in `editors/liveview/lib/`.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// The `RequestBuilder` definitions (constructor name → op).
pub(crate) const REQUEST_BUILDER: &str = "crates/beamtalk-repl-protocol/src/request.rs";
/// Every crate under this directory is scanned for `RequestBuilder::` calls.
const RUST_CRATES: &str = "crates";
/// Crates whose `src/` is test infrastructure or this checker, not a client.
const NON_CLIENT_CRATES: &[&str] = &["beamtalk-parity-tests", "beamtalk-surface-drift"];
/// File names that hold test-only code even though they live under `src/`.
const TEST_ONLY_FILES: &[&str] = &["request.rs", "test_support.rs", "test_ports.rs", "tests.rs"];
pub(crate) const VSCODE_SRC: &str = "editors/vscode/src";
pub(crate) const LIVEVIEW_LIB: &str = "editors/liveview/lib";

/// Which in-repo client sends each op (op → surfaces, e.g. `rust`, `vscode`).
pub(crate) type Senders = BTreeMap<String, BTreeSet<&'static str>>;

/// Scan every client for op senders.
pub(crate) fn scan(repo_root: &Path) -> Result<Senders, String> {
    let mut senders = Senders::new();

    let builder_src = read(&repo_root.join(REQUEST_BUILDER))?;
    let builders = builder_ops(&builder_src);
    if builders.is_empty() {
        return Err(format!(
            "found 0 RequestBuilder constructors in {REQUEST_BUILDER} — update the \
             sender scanner in beamtalk-surface-drift"
        ));
    }
    for file in rust_client_files(&repo_root.join(RUST_CRATES))? {
        let text = crate::strip_cfg_test_mod_blocks(&read(&file)?);
        let code: String = code_lines(&text, "//").collect::<Vec<_>>().join("\n");
        for name in builder_calls(&code) {
            if let Some(op) = builders.get(&name) {
                senders.entry(op.clone()).or_default().insert("rust");
            }
        }
    }

    for file in files_with_ext(&repo_root.join(VSCODE_SRC), "ts", &|p| {
        !p.components().any(|c| c.as_os_str() == "__tests__")
    })? {
        for op in vscode_ops(&read(&file)?) {
            senders.entry(op).or_default().insert("vscode");
        }
    }

    for file in files_with_ext(&repo_root.join(LIVEVIEW_LIB), "ex", &|_| true)? {
        for op in liveview_ops(&read(&file)?) {
            senders.entry(op).or_default().insert("liveview");
        }
    }
    Ok(senders)
}

fn read(path: &Path) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))
}

/// Map each `RequestBuilder` constructor in `src` to the op it builds.
pub(crate) fn builder_ops(src: &str) -> BTreeMap<String, String> {
    let production = crate::production_portion(src);
    // name → (direct op, delegated constructor)
    let mut bodies: BTreeMap<String, (Option<String>, Option<String>)> = BTreeMap::new();
    let mut current: Option<String> = None;
    for line in production.lines() {
        let trimmed = line.trim_start();
        let fn_rest = trimmed
            .strip_prefix("pub fn ")
            .or_else(|| trimmed.strip_prefix("fn "));
        if let Some(rest) = fn_rest {
            let name = rest.split(['(', '<']).next().unwrap_or("").to_string();
            bodies.insert(name.clone(), (None, None));
            current = Some(name);
            continue;
        }
        let Some(name) = &current else { continue };
        let entry = bodies.get_mut(name).expect("inserted above");
        if entry.0.is_none() {
            entry.0 =
                quoted_after(trimmed, "\"op\": ").or_else(|| quoted_after(trimmed, "no_param("));
        }
        if entry.1.is_none()
            && let Some(idx) = trimmed.find("Self::")
        {
            let callee = trimmed[idx + "Self::".len()..]
                .split('(')
                .next()
                .unwrap_or("")
                .to_string();
            if !callee.is_empty() && callee != "no_param" {
                entry.1 = Some(callee);
            }
        }
    }
    let mut out = BTreeMap::new();
    for name in bodies.keys() {
        let mut seen = BTreeSet::new();
        let mut cursor = name.clone();
        while seen.insert(cursor.clone()) {
            match bodies.get(&cursor) {
                Some((Some(op), _)) => {
                    out.insert(name.clone(), op.clone());
                    break;
                }
                Some((None, Some(next))) => cursor = next.clone(),
                _ => break,
            }
        }
    }
    out
}

/// The string literal right after `prefix` on `line`, if any.
fn quoted_after(line: &str, prefix: &str) -> Option<String> {
    let idx = line.find(prefix)?;
    let rest = line[idx + prefix.len()..].strip_prefix('"')?;
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// Constructor names called as `RequestBuilder::name(` in `text`.
pub(crate) fn builder_calls(text: &str) -> BTreeSet<String> {
    text.split("RequestBuilder::")
        .skip(1)
        .filter_map(|rest| {
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            rest[name.len()..].starts_with('(').then_some(name)
        })
        .collect()
}

/// Ops a VS Code source file sends: `op: "x"` request literals.
pub(crate) fn vscode_ops(text: &str) -> BTreeSet<String> {
    code_lines(text, "//")
        .flat_map(|line| all_quoted_after(line, "op: "))
        .collect()
}

/// Ops a LiveView source file dispatches.
pub(crate) fn liveview_ops(text: &str) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for line in code_lines(text, "#") {
        out.extend(all_quoted_after(line, "\"op\" => "));
        out.extend(all_quoted_after(line, ":dispatch, ["));
        // BT-5201: `beamtalk_repl_ops:request/3` is the entry point, reached
        // through `request_op("x", ...)` and a direct `:request, ["x"` call.
        out.extend(all_quoted_after(line, ":request, ["));
        out.extend(all_quoted_after(line, "request_op("));
        for (idx, _) in line.match_indices("dispatch_") {
            let rest = &line[idx + "dispatch_".len()..];
            let kind_len = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .count();
            if kind_len == 0 {
                continue;
            }
            if let Some(op) =
                quoted_after(&rest[kind_len..], "(").filter(|_| rest[kind_len..].starts_with("(\""))
            {
                out.insert(op);
            }
        }
    }
    out
}

/// Lines of `text` that are not whole-line comments.
fn code_lines<'a>(text: &'a str, comment: &'a str) -> impl Iterator<Item = &'a str> {
    text.lines().filter(move |l| {
        let t = l.trim_start();
        !t.starts_with(comment) && !t.starts_with('*') && !t.starts_with("/*")
    })
}

/// Every string literal that directly follows `prefix` on `line`.
fn all_quoted_after(line: &str, prefix: &str) -> Vec<String> {
    line.match_indices(prefix)
        .filter_map(|(idx, _)| {
            let rest = line[idx + prefix.len()..].strip_prefix('"')?;
            let end = rest.find('"')?;
            let op = &rest[..end];
            (!op.is_empty()
                && op
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || c == '-' || c.is_ascii_digit()))
            .then(|| op.to_string())
        })
        .collect()
}

/// Production `.rs` files of every crate except test-only ones.
fn rust_client_files(crates_dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut out = Vec::new();
    let entries = fs::read_dir(crates_dir)
        .map_err(|e| format!("failed to read {}: {e}", crates_dir.display()))?;
    for entry in entries {
        let path = entry.map_err(|e| format!("dir entry error: {e}"))?.path();
        let is_test_crate = path
            .file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| NON_CLIENT_CRATES.contains(&n));
        let src = path.join("src");
        if is_test_crate || !src.is_dir() {
            continue;
        }
        out.extend(files_with_ext(&src, "rs", &|p| {
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            !TEST_ONLY_FILES.contains(&name)
                && !name.ends_with("_tests.rs")
                && !p.components().any(|c| c.as_os_str() == "tests")
        })?);
    }
    Ok(out)
}

/// Every file under `dir` (recursively) with extension `ext` that `keep` accepts.
fn files_with_ext(
    dir: &Path,
    ext: &str,
    keep: &dyn Fn(&Path) -> bool,
) -> Result<Vec<PathBuf>, String> {
    let mut out = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        let entries =
            fs::read_dir(&d).map_err(|e| format!("failed to read {}: {e}", d.display()))?;
        for entry in entries {
            let path = entry.map_err(|e| format!("dir entry error: {e}"))?.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|x| x == ext) && keep(&path) {
                out.push(path);
            }
        }
    }
    out.sort();
    Ok(out)
}

/// Report ops with no sender, documented senders for ops the table no longer
/// has, and documented external senders that an in-repo client now covers
/// (so the documented list can only shrink).
pub(crate) fn check(
    ops: &BTreeSet<String>,
    senders: &Senders,
    external: &BTreeSet<String>,
    parity_doc: &str,
    errors: &mut Vec<String>,
) {
    for op in ops {
        if !senders.contains_key(op) && !external.contains(op) {
            errors.push(format!(
                "REPL op `{op}` has no sender: no RequestBuilder call outside tests, no VS Code \
                 `op: \"{op}\"` request, no LiveView dispatch, and no row in the \
                 `Op Senders Not Found by the Scanner` table of {parity_doc}. \
                 Wire a client to it, document who sends it, or remove the op."
            ));
        }
    }
    for op in external {
        if !ops.contains(op) {
            errors.push(format!(
                "{parity_doc} lists an outside sender for `{op}`, which is not in the REPL op table."
            ));
        } else if let Some(surfaces) = senders.get(op) {
            errors.push(format!(
                "{parity_doc} lists an outside sender for `{op}`, but it is sent in this repo \
                 ({}). Remove the row.",
                surfaces.iter().copied().collect::<Vec<_>>().join(", ")
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder_ops_resolves_direct_no_param_and_delegated() {
        let src = r#"
impl RequestBuilder {
    pub fn eval(code: &str) -> Value {
        json!({
            "op": "eval",
            "code": code
        })
    }
    pub fn eval_with_trace(code: &str) -> Value {
        let mut req = Self::eval(code);
        req
    }
    pub fn close() -> Value {
        Self::no_param("close")
    }
    fn no_param(op: &str) -> Value { json!({"op": op}) }
}
#[cfg(test)]
mod tests { fn fake() { RequestBuilder::nope(); } }
"#;
        let ops = builder_ops(src);
        assert_eq!(ops.get("eval").map(String::as_str), Some("eval"));
        assert_eq!(ops.get("eval_with_trace").map(String::as_str), Some("eval"));
        assert_eq!(ops.get("close").map(String::as_str), Some("close"));
        assert!(!ops.contains_key("no_param"));
    }

    #[test]
    fn builder_calls_needs_a_call() {
        let calls = builder_calls("let a = RequestBuilder::close(); // RequestBuilder::kill\n");
        assert_eq!(calls, ["close".to_string()].into_iter().collect());
    }

    #[test]
    fn vscode_ops_reads_request_literals_and_skips_comments() {
        let src = "await this._request({ op: \"close\" });\n// op: \"kill\"\n * op: \"x\"\n";
        assert_eq!(vscode_ops(src), ["close".to_string()].into_iter().collect());
    }

    #[test]
    fn liveview_ops_reads_every_dispatch_form() {
        let src = r#"
    request = %{"op" => "eval", "code" => code}
    rpc(:beamtalk_repl_ops, :dispatch, ["inspect", params, msg, self()])
    def browse_classes, do: dispatch_browse("browse-classes", %{})
    dispatch_simple("kill", %{"actor" => pid})
    # dispatch_simple("close", %{})
    request_op("hover", params, session_pid, @rpc_interactive_timeout)
    rpc(:beamtalk_repl_ops, :request, ["pid-stats", params, pid])
"#;
        let got = liveview_ops(src);
        let want: BTreeSet<String> = [
            "eval",
            "inspect",
            "browse-classes",
            "kill",
            "hover",
            "pid-stats",
        ]
        .iter()
        .map(|s| (*s).to_string())
        .collect();
        assert_eq!(got, want);
    }

    fn set(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| (*s).to_string()).collect()
    }

    #[test]
    fn check_flags_an_op_with_no_sender() {
        let mut senders = Senders::new();
        senders.entry("eval".into()).or_default().insert("rust");
        let mut errors = Vec::new();
        check(
            &set(&["eval", "clone"]),
            &senders,
            &set(&[]),
            "doc",
            &mut errors,
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("`clone` has no sender"), "{errors:?}");
    }

    #[test]
    fn check_accepts_a_documented_outside_sender() {
        let mut errors = Vec::new();
        check(
            &set(&["shutdown"]),
            &Senders::new(),
            &set(&["shutdown"]),
            "doc",
            &mut errors,
        );
        assert!(errors.is_empty(), "{errors:?}");
    }

    #[test]
    fn check_flags_stale_outside_sender_rows() {
        let mut senders = Senders::new();
        senders.entry("eval".into()).or_default().insert("vscode");
        let mut errors = Vec::new();
        check(
            &set(&["eval"]),
            &senders,
            &set(&["eval", "gone"]),
            "doc",
            &mut errors,
        );
        assert_eq!(errors.len(), 2, "{errors:?}");
        assert!(
            errors
                .iter()
                .any(|e| e.contains("sent in this repo (vscode)"))
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("`gone`, which is not in the REPL op table"))
        );
    }
}
