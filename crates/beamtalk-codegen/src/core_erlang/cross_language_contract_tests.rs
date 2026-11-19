// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Contracts between Rust/Elixir text and the Erlang and Beamtalk it names
//! (BT-5033). Siblings of `runtime_export_contract_tests`, which checks the
//! code generator and the CLI's Erlang eval strings; this file covers two
//! more places where one language writes another's names as plain text:
//!
//! 1. **LiveView's `:rpc` calls.** `editors/liveview/lib/bt_attach/workspace.ex`
//!    calls `rpc(:beamtalk_m, :f, [args])` into the runtime, stdlib and
//!    workspace apps. The default LiveView tests use stubs, so a rename in
//!    `runtime/apps/*/src` would only show up as `{:badrpc, {:EXIT, undef}}`
//!    against a live node. Each call's module, function and arity is checked
//!    against the `-export` attributes, as the codegen's calls are.
//! 2. **Beamtalk expression templates.** The MCP server, the CLI and the LSP
//!    build Beamtalk source (`"Workspace flush: …"`, `"{} removeFromSystem"`)
//!    and send it to the workspace. Each selector such a template sends must
//!    be declared in `stdlib/src/*.bt`.
//!
//! The launcher templates (`launcher.sh`/`launcher.cmd`) are covered in
//! `runtime_export_contract_tests`, next to the CLI's eval strings.

use std::collections::BTreeSet;
use std::path::Path;

use super::runtime_export_contract_tests::{
    EmittedCall, count_args, non_test_rust_files, origin_path, read_dir_sorted, runtime_exports,
    string_literals, unexported,
};

const LIVEVIEW_WORKSPACE: &str = "editors/liveview/lib/bt_attach/workspace.ex";

/// Wrapper functions in `workspace.ex` that forward their first argument (an
/// atom) as the function of an `rpc(:module, fun, args)`, with the module they
/// pin. A call to the wrapper counts as a call to `module:fun`.
const RPC_WRAPPERS: [(&str, &str); 1] = [("git_mutate", "beamtalk_git")];

/// `(name, rest)` for a leading Elixir atom `:name` in `s`.
fn leading_atom(s: &str) -> Option<(&str, &str)> {
    let body = s.strip_prefix(':')?;
    let len = body
        .bytes()
        .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
        .count();
    (len > 0).then(|| (&body[..len], &body[len..]))
}

/// `(identifier, rest)` for a leading Elixir variable name in `s`.
fn leading_ident(s: &str) -> Option<(&str, &str)> {
    let len = s
        .bytes()
        .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
        .count();
    (len > 0 && !s.as_bytes()[0].is_ascii_digit()).then(|| (&s[..len], &s[len..]))
}

/// The arity of an Elixir argument list given as `[a, b]` or as a variable
/// bound earlier by `name = [a, b]`. `before` is the source above the call.
fn list_arity(arg: &str, before: &str) -> Option<usize> {
    if let Some(list) = arg.strip_prefix('[') {
        return count_args(list);
    }
    let (name, _) = leading_ident(arg)?;
    let bound = format!("{name} = [");
    // The nearest binding of exactly `name`: not the tail of `extra_args = [`.
    let starts: Vec<usize> = before
        .match_indices(bound.as_str())
        .map(|(i, _)| i)
        .collect();
    let at = *starts.iter().rev().find(|&&i| {
        i == 0
            || !(before.as_bytes()[i - 1].is_ascii_alphanumeric()
                || before.as_bytes()[i - 1] == b'_')
    })?;
    count_args(&before[at + bound.len()..])
}

/// Every call into a `beamtalk_*` module in an Elixir source: direct
/// `rpc(:m, :f, args)` calls, and calls through a wrapper in [`RPC_WRAPPERS`].
/// A call whose module or function is not a literal atom, and is not the
/// wrapper's own forwarding call, is returned in the second list.
fn elixir_rpc_calls(src: &str, origin: &str) -> (Vec<EmittedCall>, Vec<String>) {
    let mut calls = Vec::new();
    let mut open = Vec::new();
    let code: String = src
        .lines()
        .map(|l| {
            if l.trim_start().starts_with('#') {
                ""
            } else {
                l
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let line_of = |at: usize| code[..at].matches('\n').count() + 1;

    let mut from = 0;
    while let Some(pos) = code[from..].find("rpc(") {
        let start = from + pos;
        from = start + "rpc(".len();
        if start > 0 {
            let prev = code.as_bytes()[start - 1];
            if prev.is_ascii_alphanumeric() || prev == b'_' || prev == b'.' {
                continue;
            }
        }
        let args = &code[from..];
        // `defp rpc(mod, fun, args)`: the definition, not a call.
        let Some((module, rest)) = leading_atom(args.trim_start()) else {
            continue;
        };
        if !module.starts_with("beamtalk_") {
            continue; // OTP: `:erlang`, `:supervisor`
        }
        let Some(rest) = rest.trim_start().strip_prefix(',') else {
            continue;
        };
        let rest = rest.trim_start();
        let here = format!("{origin}:{}", line_of(start));
        let Some((function, rest)) = leading_atom(rest) else {
            // `rpc(:beamtalk_git, fun, args)` inside a wrapper.
            let wrapped = RPC_WRAPPERS.iter().any(|(_, m)| *m == module);
            if !wrapped {
                open.push(format!("{here}: rpc(:{module}, <not a literal atom>, …)"));
            }
            continue;
        };
        let Some(rest) = rest.trim_start().strip_prefix(',') else {
            continue;
        };
        calls.push(EmittedCall {
            module: module.to_string(),
            function: function.to_string(),
            arity: list_arity(rest.trim_start(), &code[..start]),
            origin: here,
        });
    }

    for (wrapper, module) in RPC_WRAPPERS {
        let head = format!("{wrapper}(");
        let mut from = 0;
        while let Some(pos) = code[from..].find(&head) {
            let start = from + pos;
            from = start + head.len();
            let prev = start.checked_sub(1).map(|i| code.as_bytes()[i]);
            if prev.is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_') {
                continue;
            }
            // The definition (`defp git_mutate(fun, args)`) has no atom here.
            let Some((function, rest)) = leading_atom(&code[from..]) else {
                continue;
            };
            let Some(rest) = rest.trim_start().strip_prefix(',') else {
                continue;
            };
            calls.push(EmittedCall {
                module: module.to_string(),
                function: function.to_string(),
                arity: list_arity(rest.trim_start(), &code[..start]),
                origin: format!("{origin}:{}", line_of(start)),
            });
        }
    }
    (calls, open)
}

#[test]
fn every_liveview_rpc_target_is_exported() {
    let root = super::tests::repo_root();
    let exports = runtime_exports(&root);
    let src = std::fs::read_to_string(root.join(LIVEVIEW_WORKSPACE))
        .unwrap_or_else(|e| panic!("read {LIVEVIEW_WORKSPACE}: {e}"));
    let (calls, open) = elixir_rpc_calls(&src, LIVEVIEW_WORKSPACE);
    // Floors against the scan silently finding nothing (a renamed helper, a
    // changed call style). The issue counts about 40 `:rpc` calls, 22 of them
    // into beamtalk modules; this is a floor, not a number to keep.
    assert!(
        calls.len() >= 20,
        "found only {} beamtalk rpc calls in {LIVEVIEW_WORKSPACE}: {calls:#?}",
        calls.len()
    );
    // The named sites must be read with their arity, so a parsing regression
    // cannot turn the check into a name-only one.
    for (module, function, arity) in [
        ("beamtalk_process_navigation", "child_handles", 1),
        ("beamtalk_repl_eval", "compile_method", 6),
        ("beamtalk_repl_ops", "dispatch", 4),
        ("beamtalk_git", "git_stage", 1),
    ] {
        assert!(
            calls
                .iter()
                .any(|c| c.module == module && c.function == function && c.arity == Some(arity)),
            "{LIVEVIEW_WORKSPACE} has no {module}:{function}/{arity} call: {calls:#?}"
        );
    }
    let mut problems = open;
    problems.extend(unexported(&calls, &exports));
    assert!(
        problems.is_empty(),
        "LiveView's workspace.ex calls runtime functions that are not exported. \
         Update the call, or export the function from the runtime module:\n  {}",
        problems.join("\n  ")
    );
}

#[test]
fn a_variable_argument_list_binds_by_whole_name() {
    let before = "extra_args = [a, b, c]\n    args = [x]\n    other_args = [p, q]\n";
    assert_eq!(list_arity("args", before), Some(1));
    assert_eq!(list_arity("missing", before), None);
}

#[test]
fn rpc_scan_reads_arity_wrappers_and_skips_otp_and_definitions() {
    let src = "\
      defp rpc(mod, fun, args) do\n\
        :rpc.call(node_name(), mod, fun, args)\n\
      end\n\
      # rpc(:beamtalk_commented, :out, [])\n\
      rpc(:supervisor, :count_children, [:x])\n\
      args = [a, b, [c, d]]\n\
      case rpc(:beamtalk_m, :f, args) do\n\
      rpc(:beamtalk_m, :g, [])\n\
      rpc(:beamtalk_m, :h, [x, \"a,b\", %{k: 1}])\n\
      defp git_mutate(fun, args) do\n\
        rpc(:beamtalk_git, fun, args)\n\
      do_it(), do: git_mutate(:git_stage, [path])\n";
    let (calls, open) = elixir_rpc_calls(src, "x.ex");
    let shown: Vec<(String, Option<usize>)> = calls
        .iter()
        .map(|c| (format!("{}:{}", c.module, c.function), c.arity))
        .collect();
    assert_eq!(
        shown,
        vec![
            ("beamtalk_m:f".to_string(), Some(3)),
            ("beamtalk_m:g".to_string(), Some(0)),
            ("beamtalk_m:h".to_string(), Some(3)),
            ("beamtalk_git:git_stage".to_string(), Some(1)),
        ]
    );
    assert!(open.is_empty(), "{open:?}");

    let (_, open) = elixir_rpc_calls("rpc(:beamtalk_m, fun, [])", "y.ex");
    assert_eq!(
        open.len(),
        1,
        "a dynamic function must be reported: {open:?}"
    );
}

// ── Beamtalk expression templates ───────────────────────────────────────

/// Sources whose Rust builds Beamtalk expressions for the workspace: the MCP,
/// CLI and LSP crates, and `tool_expr.rs`, the builders (`rename_class_expr`,
/// `remove_selector_expr`, …) that the MCP server and the CLI's meta-commands
/// share.
const TEMPLATE_SOURCES: [&str; 4] = [
    "crates/beamtalk-mcp/src",
    "crates/beamtalk-cli/src",
    "crates/beamtalk-lsp/src",
    "crates/beamtalk-core/src/tool_expr.rs",
];

/// Globals the templates name as a receiver. Each must be a class declared in
/// `stdlib/src` (checked), so this list cannot drift from the stdlib.
const GLOBAL_RECEIVERS: [&str; 3] = ["Workspace", "Beamtalk", "Package"];

/// The selector declared by a method line in a stdlib `.bt` file, if the line
/// is one: `[sealed] [class] name -> T =>`, `[class] kw: a :: T kw2: b => …`.
/// An over-approximation is fine here (a state declaration `state: x` yields
/// a `state:` that nothing sends): the check is only that a sent selector is
/// declared somewhere.
fn declared_selector(line: &str) -> Option<String> {
    let t = line.trim();
    if t.starts_with("//") || !(t.contains("=>") || t.contains(" -> ")) {
        return None;
    }
    let head = t.split(" -> ").next()?.split("=>").next()?.trim();
    let tokens: Vec<&str> = head.split_whitespace().collect();
    let keywords: String = tokens
        .iter()
        .filter(|tok| tok.ends_with(':') && !tok.ends_with("::") && tok.len() > 1)
        .copied()
        .collect();
    if !keywords.is_empty() {
        return Some(keywords);
    }
    let last = *tokens.last()?;
    let ident = !last.is_empty()
        && last.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
        && !last.as_bytes()[0].is_ascii_digit();
    ident.then(|| last.to_string())
}

/// Every selector and class name declared in `stdlib/src/*.bt`.
fn stdlib_declarations(root: &Path) -> (BTreeSet<String>, BTreeSet<String>) {
    let mut selectors = BTreeSet::new();
    let mut classes = BTreeSet::new();
    for file in read_dir_sorted(&root.join("stdlib/src")) {
        if file.extension().is_none_or(|e| e != "bt") {
            continue;
        }
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        for line in src.lines() {
            if let Some(sel) = declared_selector(line) {
                selectors.insert(sel);
            }
            if let Some((_, rest)) = line.split_once(" subclass: ")
                && let Some(name) = rest.split_whitespace().next()
            {
                classes.insert(name.to_string());
            }
        }
    }
    (selectors, classes)
}

/// The selector a Beamtalk send template sends, when `literal` is one:
/// `<receiver> <unary>` or `<receiver> kw: … kw2: …`, where the receiver is a
/// [`GLOBAL_RECEIVERS`] name or a `{…}` format placeholder.
fn template_selector(literal: &str) -> Option<String> {
    let rest = GLOBAL_RECEIVERS
        .iter()
        .find_map(|r| literal.strip_prefix(r).and_then(|s| s.strip_prefix(' ')))
        .or_else(|| {
            let after = literal.strip_prefix('{')?;
            let close = after.find('}')?;
            after[close + 1..].strip_prefix(' ')
        })?;
    let head_len = rest
        .bytes()
        .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
        .count();
    if head_len == 0 || !rest.as_bytes()[0].is_ascii_lowercase() {
        return None;
    }
    let after_head = &rest[head_len..];
    if after_head.is_empty() {
        return Some(rest.to_string()); // unary
    }
    // A keyword is followed by a space and its argument. This drops
    // `"{what} failed:\n…"` (a message, whose `\n` the literal reader keeps
    // as an `n`) and `"…: "`-less prose.
    if !after_head.starts_with(": ") {
        return None; // prose, not a send
    }
    // Keyword send: the first keyword, then every later `word:` that follows
    // a space. A `#sym:` symbol argument and a `::` annotation do not count.
    let mut selector = format!("{}:", &rest[..head_len]);
    let mut tokens = after_head[1..].split(' ');
    tokens.next(); // glue between the first colon and its argument
    for tok in tokens {
        if let Some(kw) = tok.strip_suffix(':')
            && !kw.is_empty()
            && kw.as_bytes()[0].is_ascii_lowercase()
            && kw.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
        {
            selector.push_str(kw);
            selector.push(':');
        }
    }
    Some(selector)
}

/// Literals that read as a send but are user-facing text. Each is a prefix of
/// the literal with the file it is in and why it is not a send. Keep this
/// list at the size it has: a new entry should be rare.
const NOT_SENDS: [(&str, &str, &str); 1] = [(
    "crates/beamtalk-cli/src/commands/doctor.rs",
    "Beamtalk runtime: ",
    "the `doctor` check's output line, not an expression",
)];

/// `(selector, origin)` for every send template in the MCP, CLI and LSP
/// production sources.
fn sent_selectors(root: &Path) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for source in TEMPLATE_SOURCES {
        let path = root.join(source);
        let mut files = Vec::new();
        if path.is_file() {
            files.push(path);
        } else {
            non_test_rust_files(&path, &mut files);
        }
        for file in files {
            let src = std::fs::read_to_string(&file)
                .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
            let rel = origin_path(&file, root);
            for (line, literal) in string_literals(&src) {
                if NOT_SENDS
                    .iter()
                    .any(|(file, prefix, _why)| rel == *file && literal.starts_with(prefix))
                {
                    continue;
                }
                if let Some(sel) = template_selector(&literal) {
                    out.push((sel, format!("{rel}:{line}")));
                }
            }
        }
    }
    out
}

/// Whether `sel` is one keyword that a declared selector contains after its
/// first. `tool_expr` builds a send in two steps (`flush_expr(filter)`, then
/// `"{} confirmDestructive: true"`), so the second literal holds only the tail
/// of `flush:confirmDestructive:`. A single keyword is read as such a tail
/// when no selector is declared under that exact name.
fn continues_a_declared_selector(sel: &str, declared: &BTreeSet<String>) -> bool {
    let is_one_keyword = sel.ends_with(':') && sel.matches(':').count() == 1;
    is_one_keyword
        && declared
            .iter()
            .any(|d| d.split_inclusive(':').skip(1).any(|part| part == sel))
}

#[test]
fn every_selector_the_mcp_cli_and_lsp_send_is_declared_in_the_stdlib() {
    let root = super::tests::repo_root();
    let (declared, classes) = stdlib_declarations(&root);
    for receiver in GLOBAL_RECEIVERS {
        assert!(
            classes.contains(receiver),
            "{receiver} is not a class in stdlib/src; update GLOBAL_RECEIVERS"
        );
    }
    let sent = sent_selectors(&root);
    // The selectors the issue names; each must be found by the scan, so a
    // changed template style cannot silently empty the check.
    for needle in [
        "compile:source:",
        "flush:",
        "flush:confirmDestructive:",
        "recheckImage",
        "removeFromSystem",
        "renameTo:",
        "renameSelector:to:",
        "help:",
        "help:selector:",
        "classes:",
    ] {
        assert!(
            sent.iter().any(|(s, _)| s == needle),
            "no template sends {needle}; scan found {:#?}",
            sent.iter().map(|(s, _)| s).collect::<BTreeSet<_>>()
        );
    }
    let missing: BTreeSet<String> = sent
        .iter()
        .filter(|(sel, _)| {
            !declared.contains(sel) && !continues_a_declared_selector(sel, &declared)
        })
        .map(|(sel, origin)| {
            format!("{origin}: sends #{sel}, which no class in stdlib/src declares")
        })
        .collect();
    assert!(
        missing.is_empty(),
        "Beamtalk expression templates send selectors that are not declared in \
         stdlib/src. Rename the template, or restore the method:\n  {}",
        missing.into_iter().collect::<Vec<_>>().join("\n  ")
    );
}

#[test]
fn template_scan_reads_unary_and_keyword_sends_and_skips_prose() {
    for (literal, want) in [
        ("Workspace recheckImage", Some("recheckImage")),
        ("Workspace flush: #{ #file => \"a\" }", Some("flush:")),
        (
            "Workspace flush: Counter confirmDestructive: true",
            Some("flush:confirmDestructive:"),
        ),
        ("{} compile: #{} source: \"{}\"", Some("compile:source:")),
        ("{class} removeFromSystem", Some("removeFromSystem")),
        (
            "Beamtalk help: {class} selector: #{sel}",
            Some("help:selector:"),
        ),
        ("Package classes: \"{pkg}\"", Some("classes:")),
        ("{} v{} is already running (REPL port {})", None),
        ("{what} failed:n{}", None),
        ("{file_count} file(s), {total} tests", None),
        ("Method {}>>#{} saved", None),
        ("Workspace", None),
    ] {
        assert_eq!(template_selector(literal).as_deref(), want, "{literal}");
    }
}

#[test]
fn a_tail_keyword_continues_only_a_declared_selector() {
    let declared: BTreeSet<String> = ["flush:confirmDestructive:", "at:put:"]
        .map(String::from)
        .into();
    assert!(continues_a_declared_selector(
        "confirmDestructive:",
        &declared
    ));
    // The first keyword is not a tail, a renamed one is not declared, and a
    // multi-keyword selector is checked whole.
    assert!(!continues_a_declared_selector("flush:", &declared));
    assert!(!continues_a_declared_selector("confirmDestroy:", &declared));
    assert!(!continues_a_declared_selector("at:put:", &declared));
}

#[test]
fn declared_selector_reads_unary_keyword_and_modified_methods() {
    for (line, want) in [
        (
            "  class flush -> Dictionary => self delegate",
            Some("flush"),
        ),
        (
            "  sealed reload -> Behaviour => @primitive \"x\"",
            Some("reload"),
        ),
        (
            "  sealed compile: aSelector :: Symbol source: aSource :: String -> Behaviour =>",
            Some("compile:source:"),
        ),
        ("  /// Counter removeFromSystem   // => nil", None),
        (
            "  class help: aClass :: Object -> String => self delegate",
            Some("help:"),
        ),
    ] {
        assert_eq!(declared_selector(line).as_deref(), want, "{line}");
    }
}
