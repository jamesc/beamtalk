// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The editor clients that speak the REPL protocol without `RequestBuilder`
//! (`editors/vscode/src/workspaceClient.ts`, `editors/liveview/lib/bt_attach/workspace.ex`)
//! must send only ops the server routes, with only params the op declares, and
//! must send every required param (BT-4901).
//!
//! The server side is `beamtalk_repl_op_table`, read through the fixture
//! `beamtalk_repl_op_table_tests` pins it to (the same source `op_table_doc.rs`
//! and `beamtalk-surface-drift` read). The clients are scanned as text: a
//! literal-op request in either file is parsed into `(op, keys)`; requests whose
//! op name is computed at run time are out of reach and skipped.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

/// Envelope keys every request may carry that are not op params.
const ENVELOPE: &[&str] = &["op", "id", "session"];

struct OpSpec {
    required: BTreeSet<String>,
    optional: BTreeSet<String>,
}

fn root() -> PathBuf {
    beamtalk_workspace::cargo_workspace_root!()
}

fn load_table() -> BTreeMap<String, OpSpec> {
    let path = root().join("runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json");
    let table: Vec<serde_json::Value> =
        serde_json::from_str(&std::fs::read_to_string(&path).expect("read op table fixture"))
            .expect("op table fixture is a JSON array");
    let set = |e: &serde_json::Value, k: &str| -> BTreeSet<String> {
        e[k].as_array()
            .unwrap_or_else(|| panic!("`{k}` is an array in {e}"))
            .iter()
            .map(|v| v.as_str().expect("string").to_owned())
            .collect()
    };
    table
        .iter()
        .map(|e| {
            (
                e["op"].as_str().expect("op").to_owned(),
                OpSpec {
                    required: set(e, "params"),
                    optional: set(e, "optional"),
                },
            )
        })
        .collect()
}

/// One request a client sends: its op, the param keys, and where it was found.
#[derive(Debug)]
struct Sent {
    op: String,
    keys: BTreeSet<String>,
    at: String,
}

/// Byte offset of `sub` (a sub-slice of `text`) within `text`.
fn offset_in(text: &str, sub: &str) -> usize {
    sub.as_ptr() as usize - text.as_ptr() as usize
}

fn line_of(text: &str, offset: usize) -> usize {
    text[..offset].matches('\n').count() + 1
}

/// Index of the delimiter closing the one opened at `open` (`{`/`}` or `(`/`)`).
fn matching(bytes: &[u8], open: usize) -> usize {
    let (o, c) = match bytes[open] {
        b'{' => (b'{', b'}'),
        b'(' => (b'(', b')'),
        other => panic!("not an opener: {}", other as char),
    };
    let mut depth = 0usize;
    for (i, &b) in bytes.iter().enumerate().skip(open) {
        if b == o {
            depth += 1;
        } else if b == c {
            depth -= 1;
            if depth == 0 {
                return i;
            }
        }
    }
    panic!("unbalanced delimiter at byte {open}");
}

/// `text` with string-literal contents (`"…"`, `'…'`, `` `…` ``) and `//` and
/// `/* … */` comments blanked to spaces (length-preserving), so braces and
/// colons inside them cannot confuse the object scanner.
fn blank_strings(text: &str) -> String {
    let chars: Vec<char> = text.chars().collect();
    let blank = |c: char| {
        if c == '\n' {
            "\n".to_owned()
        } else {
            " ".repeat(c.len_utf8())
        }
    };
    let mut out = String::with_capacity(text.len());
    let mut quote: Option<char> = None;
    let mut escaped = false;
    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];
        if let Some(q) = quote {
            if escaped {
                escaped = false;
                out.push_str(&blank(ch));
            } else if ch == '\\' {
                escaped = true;
                out.push_str(&blank(ch));
            } else if ch == q {
                quote = None;
                out.push(ch);
            } else {
                out.push_str(&blank(ch));
            }
        } else if ch == '/' && chars.get(i + 1) == Some(&'/') {
            while i < chars.len() && chars[i] != '\n' {
                out.push_str(&blank(chars[i]));
                i += 1;
            }
            continue;
        } else if ch == '/' && chars.get(i + 1) == Some(&'*') {
            while i < chars.len() && !(chars[i] == '*' && chars.get(i + 1) == Some(&'/')) {
                out.push_str(&blank(chars[i]));
                i += 1;
            }
            out.push_str("  ");
            i += 2;
            continue;
        } else {
            if ch == '"' || ch == '\'' || ch == '`' {
                quote = Some(ch);
            }
            out.push(ch);
        }
        i += 1;
    }
    out
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Keys of a (string-blanked) JS object literal `{ … }`: `ident:` and
/// shorthand `ident` entries, at any depth, so a conditional spread such as
/// `...(pkg ? { package: pkg } : {})` contributes `package`.
fn js_object_keys(obj: &str) -> BTreeSet<String> {
    let b = obj.as_bytes();
    let mut keys = BTreeSet::new();
    let mut i = 0;
    while i < b.len() {
        if b[i].is_ascii_alphabetic() || b[i] == b'_' {
            let start = i;
            while i < b.len() && is_ident_char(b[i]) {
                i += 1;
            }
            let ident = &obj[start..i];
            let before = obj[..start].trim_end().as_bytes().last().copied();
            let after = obj[i..].trim_start().as_bytes().first().copied();
            let after_is_colon = after == Some(b':') && !obj[i..].trim_start().starts_with("::");
            let starts_entry = matches!(before, Some(b'{' | b','));
            if starts_entry && (after_is_colon || matches!(after, Some(b',' | b'}'))) {
                keys.insert(ident.to_owned());
            }
        } else {
            i += 1;
        }
    }
    keys
}

/// Requests built in `workspaceClient.ts`: every `op: "<name>"` inside an
/// object literal, plus `<var>.<key> = …` assignments in the same method (the
/// `subscribe-logs` request builds its object incrementally).
fn scan_vscode(text: &str) -> Vec<Sent> {
    let blanked = blank_strings(text);
    let bytes = blanked.as_bytes();
    let mut out = Vec::new();
    let mut from = 0;
    while let Some(rel) = text[from..].find("op: \"") {
        let at = from + rel;
        from = at + 5;
        // Skip doc comments / line comments.
        let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
        let prefix = text[line_start..at].trim_start();
        if prefix.starts_with('*') || prefix.starts_with("//") || prefix.starts_with("/*") {
            continue;
        }
        let name_end = text[from..].find('"').expect("closing quote") + from;
        let op = text[from..name_end].to_owned();
        // Enclosing object: nearest unmatched `{` before `at`.
        let mut depth = 0i32;
        let mut open = None;
        for i in (0..at).rev() {
            match bytes[i] {
                b'}' => depth += 1,
                b'{' if depth == 0 => {
                    open = Some(i);
                    break;
                }
                b'{' => depth -= 1,
                _ => {}
            }
        }
        let open = open.expect("op is inside an object literal");
        let close = matching(bytes, open);
        let mut keys = js_object_keys(&blanked[open..=close]);
        // `params.level = level;` style additions in the same method.
        let method_end = blanked[close..]
            .find("\n  }\n")
            .map_or(blanked.len(), |n| close + n);
        let var_start = blanked[..open].rfind("const ").map(|p| p + 6);
        if let Some(vs) = var_start {
            let var: String = blanked[vs..]
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            let pat = format!("{var}.");
            let mut p = close;
            while let Some(rel) = blanked[p..method_end].find(&pat) {
                let k0 = p + rel + pat.len();
                let key: String = blanked[k0..]
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .collect();
                if blanked[k0 + key.len()..].trim_start().starts_with('=') {
                    keys.insert(key);
                }
                p = k0;
            }
        }
        out.push(Sent {
            op,
            keys,
            at: format!("workspaceClient.ts:{}", line_of(text, at)),
        });
    }
    out
}

/// `"key" =>` entries of an Elixir map body.
fn elixir_map_keys(body: &str) -> BTreeSet<String> {
    let mut keys = BTreeSet::new();
    let mut rest = body;
    while let Some(q) = rest.find('"') {
        let after = &rest[q + 1..];
        let Some(end) = after.find('"') else { break };
        let name = &after[..end];
        let tail = after[end + 1..].trim_start();
        if tail.starts_with("=>")
            && name
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            keys.insert(name.to_owned());
        }
        rest = &after[end + 1..];
    }
    keys
}

/// Keys added to a request map after its literal: `Map.put(request, "k", …)`,
/// `maybe_put_param("k", …)` — read from `window`.
fn elixir_added_keys(window: &str) -> BTreeSet<String> {
    let mut keys = BTreeSet::new();
    for marker in ["Map.put(request, \"", "maybe_put_param(\""] {
        let mut rest = window;
        while let Some(p) = rest.find(marker) {
            let after = &rest[p + marker.len()..];
            let name: String = after
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '-')
                .collect();
            keys.insert(name);
            rest = after;
        }
    }
    keys
}

/// Requests built in `workspace.ex`: `dispatch_browse("op", …)` /
/// `dispatch_simple("op", …)` / `request_op("op", …)` (BT-5201) calls, `%{"op" => "name", …}` request literals,
/// and the `{op, params} = case … {"name", %{…}}` tuple form in `run_tests`.
fn scan_liveview(text: &str) -> Vec<Sent> {
    let bytes = text.as_bytes();
    let mut out = Vec::new();
    let lines_before = |at: usize, n: usize| -> &str {
        let mut start = at;
        for _ in 0..n {
            start = text[..start].rfind('\n').unwrap_or(0);
        }
        &text[start..at]
    };

    for call in ["dispatch_browse(\"", "dispatch_simple(\"", "request_op(\""] {
        let mut from = 0;
        while let Some(rel) = text[from..].find(call) {
            let at = from + rel;
            from = at + call.len();
            let name_end = text[from..].find('"').expect("closing quote") + from;
            let op = text[from..name_end].to_owned();
            let after = text[name_end + 1..].trim_start();
            let after = after.strip_prefix(',').unwrap_or(after).trim_start();
            let keys = if after.starts_with("%{") {
                let open = offset_in(text, after) + 1;
                let close = matching(bytes, open);
                elixir_map_keys(&text[open..=close])
            } else {
                // Variable params map: the literal and `maybe_put_param`
                // additions sit in the lines just above the call.
                let window = lines_before(at, 8);
                let mut k = elixir_map_keys(window);
                k.extend(elixir_added_keys(window));
                k
            };
            out.push(Sent {
                op,
                keys,
                at: format!("workspace.ex:{}", line_of(text, at)),
            });
        }
    }

    let lit = "\"op\" => \"";
    let mut from = 0;
    while let Some(rel) = text[from..].find(lit) {
        let at = from + rel;
        from = at + lit.len();
        let name_end = text[from..].find('"').expect("closing quote") + from;
        let op = text[from..name_end].to_owned();
        // Enclosing `%{`: nearest unmatched `{` before the `"op" =>` entry.
        let mut depth = 0i32;
        let mut open = None;
        for i in (0..at).rev() {
            match bytes[i] {
                b'}' => depth += 1,
                b'{' if depth == 0 => {
                    open = Some(i);
                    break;
                }
                b'{' => depth -= 1,
                _ => {}
            }
        }
        let open = open.expect("op entry is inside a map literal");
        let close = matching(bytes, open);
        let mut keys = elixir_map_keys(&text[open..=close]);
        keys.remove("op");
        // `Map.put(request, "k", …)` additions before the `request_op(` call.
        let tail_end = text[close..]
            .find("request_op(")
            .map_or(text.len(), |n| close + n);
        keys.extend(elixir_added_keys(&text[close..tail_end]));
        out.push(Sent {
            op,
            keys,
            at: format!("workspace.ex:{}", line_of(text, at)),
        });
    }

    if let Some(at) = text.find("{op, params} =") {
        // The `case` that picks the op ends where the computed-op call begins.
        let end = at
            + text[at..]
                .find("dispatch_simple(op, params)")
                .expect("`{op, params} =` is followed by `dispatch_simple(op, params)`");
        let mut rest = &text[at..end];
        while let Some(p) = rest.find("{\"") {
            let after = &rest[p + 2..];
            let name_end = after.find('"').expect("closing quote");
            let op = after[..name_end].to_owned();
            let tail = after[name_end + 1..].trim_start();
            if let Some(map) = tail.strip_prefix(',').map(str::trim_start)
                && map.starts_with("%{")
            {
                let open = offset_in(text, map) + 1;
                let close = matching(bytes, open);
                out.push(Sent {
                    op,
                    keys: elixir_map_keys(&text[open..=close]),
                    at: format!("workspace.ex:{}", line_of(text, at)),
                });
            }
            rest = &after[name_end + 1..];
        }
    }
    out
}

fn check(client: &str, sent: &[Sent], table: &BTreeMap<String, OpSpec>) {
    assert!(!sent.is_empty(), "{client}: scanner found no requests");
    let mut problems = Vec::new();
    for s in sent {
        let Some(spec) = table.get(&s.op) else {
            problems.push(format!(
                "{} sends op `{}`, which is not in repl_op_table.json",
                s.at, s.op
            ));
            continue;
        };
        for k in &s.keys {
            if ENVELOPE.contains(&k.as_str())
                || spec.required.contains(k)
                || spec.optional.contains(k)
            {
                continue;
            }
            problems.push(format!(
                "{} sends param `{k}` for op `{}`, which declares only {:?} + {:?}",
                s.at, s.op, spec.required, spec.optional
            ));
        }
        for r in &spec.required {
            if !s.keys.contains(r) {
                problems.push(format!(
                    "{} omits required param `{r}` of op `{}`",
                    s.at, s.op
                ));
            }
        }
    }
    assert!(
        problems.is_empty(),
        "{client} drifted from repl_op_table.json:\n  {}",
        problems.join("\n  ")
    );
}

#[test]
fn vscode_workspace_client_matches_the_op_table() {
    let text = std::fs::read_to_string(root().join("editors/vscode/src/workspaceClient.ts"))
        .expect("read workspaceClient.ts");
    let sent = scan_vscode(&text);
    let ops: BTreeSet<&str> = sent.iter().map(|s| s.op.as_str()).collect();
    // The scanner must see the requests this file is known to send; a
    // refactor that hides them from it fails here, not silently.
    for expected in [
        "eval",
        "actors",
        "inspect",
        "subscribe-logs",
        "browse-alias-source",
    ] {
        assert!(
            ops.contains(expected),
            "scanner missed `{expected}`: {ops:?}"
        );
    }
    check("workspaceClient.ts", &sent, &load_table());
}

#[test]
fn liveview_workspace_matches_the_op_table() {
    let text = std::fs::read_to_string(root().join("editors/liveview/lib/bt_attach/workspace.ex"))
        .expect("read workspace.ex");
    let sent = scan_liveview(&text);
    let ops: BTreeSet<&str> = sent.iter().map(|s| s.op.as_str()).collect();
    for expected in [
        "eval",
        "hover",
        "browse-classes",
        "nav-query",
        "save-section",
        "test",
    ] {
        assert!(
            ops.contains(expected),
            "scanner missed `{expected}`: {ops:?}"
        );
    }
    check("workspace.ex", &sent, &load_table());
}

#[test]
fn scanners_flag_unknown_ops_and_params() {
    let table = load_table();
    let ts = scan_vscode("x({ op: \"no-such-op\", code });\n");
    assert!(std::panic::catch_unwind(|| check("ts", &ts, &load_table())).is_err());
    let ex = scan_liveview("dispatch_browse(\"browse-classes\", %{\"bogus\" => 1})\n");
    assert!(table.contains_key("browse-classes"));
    assert!(std::panic::catch_unwind(|| check("ex", &ex, &load_table())).is_err());
}
