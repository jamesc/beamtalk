// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `docs/repl-protocol.md` § Op Reference must list every op in the server's op
//! table (BT-4673). The table is the Erlang `beamtalk_repl_op_table`, read here
//! through the fixture `beamtalk_repl_op_table_tests` pins it to.
//!
//! Set `BEAMTALK_BLESS_OP_TABLE_DOC=1` to rewrite the generated block in place.

use std::fmt::Write as _;

const BEGIN: &str = "<!-- op-table:begin -->";
const END: &str = "<!-- op-table:end -->";

fn strings(entry: &serde_json::Value, key: &str) -> Vec<String> {
    entry[key]
        .as_array()
        .unwrap_or_else(|| panic!("`{key}` is an array in {entry}"))
        .iter()
        .map(|v| v.as_str().expect("string").to_owned())
        .collect()
}

fn code_list(items: &[String]) -> String {
    if items.is_empty() {
        "—".to_owned()
    } else {
        items
            .iter()
            .map(|i| format!("`{i}`"))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

fn render(table: &[serde_json::Value]) -> String {
    let mut out = String::new();
    out.push_str("| Op | Params | Optional | Capability | Handler |\n");
    out.push_str("|----|--------|----------|------------|---------|\n");
    for entry in table {
        let op = entry["op"].as_str().expect("op");
        let _ = writeln!(
            out,
            "| `{op}` | {} | {} | `{}` | `{}` |",
            code_list(&strings(entry, "params")),
            code_list(&strings(entry, "optional")),
            entry["capability"].as_str().expect("capability"),
            entry["handler"].as_str().expect("handler"),
        );
    }
    out
}

#[test]
fn repl_protocol_doc_lists_every_op_in_the_table() {
    let root = beamtalk_workspace::cargo_workspace_root!();
    let table_path = root.join("runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json");
    let table: Vec<serde_json::Value> =
        serde_json::from_str(&std::fs::read_to_string(&table_path).expect("read op table fixture"))
            .expect("op table fixture is a JSON array");
    assert!(!table.is_empty(), "op table must not be empty");

    let doc_path = root.join("docs/repl-protocol.md");
    let doc = std::fs::read_to_string(&doc_path).expect("read repl-protocol.md");
    let begin = doc.find(BEGIN).expect("op-table:begin marker in doc") + BEGIN.len();
    let end = doc.find(END).expect("op-table:end marker in doc");
    let expected = format!("\n{}", render(&table));

    if doc[begin..end] != expected {
        if std::env::var_os("BEAMTALK_BLESS_OP_TABLE_DOC").is_some() {
            let updated = format!("{}{}{}", &doc[..begin], expected, &doc[end..]);
            std::fs::write(&doc_path, updated).expect("write repl-protocol.md");
        } else {
            panic!(
                "docs/repl-protocol.md § Op Reference is out of date with the op table. \
                 Run `BEAMTALK_BLESS_OP_TABLE_DOC=1 cargo test -p beamtalk-repl-protocol \
                 --test op_table_doc` to regenerate it."
            );
        }
    }

    // The `info` op was documented but never existed on the server.
    assert!(
        !table.iter().any(|e| e["op"] == "info"),
        "`info` is not a server op"
    );
}
