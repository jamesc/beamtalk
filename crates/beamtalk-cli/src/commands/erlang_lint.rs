// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Text-based lint checks for native Erlang (`.erl`) files.
//!
//! These are heuristic checks that scan Erlang source text without a full
//! AST parse. They catch common issues in native Erlang files that are part
//! of a Beamtalk package:
//!
//! 1. Missing `-moduledoc` attribute (EEP-59)
//! 2. Missing `-doc` attribute on exported functions (EEP-59)
//! 3. Hardcoded `'bt@...'` module references (BT-1730)

use camino::{Utf8Path, Utf8PathBuf};

/// A lint diagnostic for a native Erlang file.
#[derive(Debug, Clone)]
pub struct ErlangLintDiagnostic {
    pub file: Utf8PathBuf,
    pub line: usize,
    pub column: usize,
    pub message: String,
    pub hint: Option<String>,
}

/// OTP behaviour callbacks that don't need `-doc` attributes.
const OTP_CALLBACKS: &[(&str, usize)] = &[
    ("init", 1),
    ("handle_call", 3),
    ("handle_cast", 2),
    ("handle_info", 2),
    ("handle_continue", 2),
    ("terminate", 2),
    ("code_change", 3),
    ("format_status", 1),
    ("format_status", 2),
    ("handle_event", 4),  // gen_statem
    ("callback_mode", 0), // gen_statem
];

/// Run all Erlang lint checks on a single file.
pub fn lint_erl_file(file: &Utf8Path, source: &str) -> Vec<ErlangLintDiagnostic> {
    let mut diags = Vec::new();

    check_moduledoc(file, source, &mut diags);
    check_doc_on_exports(file, source, &mut diags);
    check_hardcoded_bt_refs(file, source, &mut diags);

    diags
}

/// Check 1: Missing `-moduledoc` attribute.
fn check_moduledoc(file: &Utf8Path, source: &str, diags: &mut Vec<ErlangLintDiagnostic>) {
    let mut has_moduledoc = false;
    let mut module_line = None;

    for (line_no, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("-moduledoc") {
            has_moduledoc = true;
            break;
        }
        if trimmed.starts_with("-module(") && module_line.is_none() {
            module_line = Some(line_no);
        }
    }

    if !has_moduledoc {
        let line = module_line.unwrap_or(0);
        diags.push(ErlangLintDiagnostic {
            file: file.to_path_buf(),
            line: line + 1,
            column: 1,
            message: "missing `-moduledoc` attribute".to_string(),
            hint: Some(
                "add `-moduledoc \"Module description.\".` after the `-module()` attribute"
                    .to_string(),
            ),
        });
    }
}

/// Check 2: Missing `-doc` on exported functions.
fn check_doc_on_exports(file: &Utf8Path, source: &str, diags: &mut Vec<ErlangLintDiagnostic>) {
    let exports = parse_exports(source);
    if exports.is_empty() {
        return;
    }

    // Build set of exported function names for quick lookup.
    let export_set: std::collections::HashSet<(&str, usize)> =
        exports.iter().map(|(n, a)| (n.as_str(), *a)).collect();

    let lines: Vec<&str> = source.lines().collect();

    // Track which exported functions we've already seen the first clause of,
    // so multi-clause functions (foo(1) -> ...; foo(2) -> ...) only check once.
    let mut seen_functions: std::collections::HashSet<(&str, usize)> =
        std::collections::HashSet::new();

    for (line_no, line) in lines.iter().enumerate() {
        let trimmed = line.trim();

        // Look for function head definitions: `name(` at the start of a line.
        if let Some(func_name) = parse_function_head(trimmed) {
            let arity = count_function_arity(trimmed);
            if !export_set.contains(&(func_name, arity)) {
                continue;
            }

            // Only check the first clause of each function.
            if !seen_functions.insert((func_name, arity)) {
                continue;
            }

            // Skip OTP behaviour callbacks.
            if OTP_CALLBACKS
                .iter()
                .any(|(n, a)| *n == func_name && *a == arity)
            {
                continue;
            }

            // Check preceding lines for a `-doc` attribute.
            if !has_preceding_doc(line_no, &lines) {
                diags.push(ErlangLintDiagnostic {
                    file: file.to_path_buf(),
                    line: line_no + 1,
                    column: 1,
                    message: format!(
                        "exported function `{func_name}/{arity}` missing `-doc` attribute"
                    ),
                    hint: Some(
                        "add `-doc \"Description.\".` before the function definition".to_string(),
                    ),
                });
            }
        }
    }
}

/// Check 3: Hardcoded `'bt@...'` module references.
///
/// Reuses the detection pattern from `build.rs:validate_native_class_references()`.
fn check_hardcoded_bt_refs(file: &Utf8Path, source: &str, diags: &mut Vec<ErlangLintDiagnostic>) {
    for (line_no, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        // Skip comments and empty lines.
        if trimmed.starts_with('%') || trimmed.is_empty() {
            continue;
        }

        let mut search_from = 0;
        while let Some(offset) = line[search_from..].find("'bt@") {
            let pos = search_from + offset;
            let after_quote = &line[pos + 1..]; // skip leading quote
            if let Some(end) = after_quote.find('\'') {
                let module_atom = &after_quote[..end];

                diags.push(ErlangLintDiagnostic {
                    file: file.to_path_buf(),
                    line: line_no + 1,
                    column: pos + 1,
                    message: format!(
                        "hardcoded Beamtalk module reference `'{module_atom}'`"
                    ),
                    hint: Some(
                        "use `?BT_CLASS_MODULE_*` macro from beamtalk_classes.hrl instead (BT-1730)"
                            .to_string(),
                    ),
                });

                // Advance past this atom.
                search_from = pos + 1 + end + 1;
            } else {
                break;
            }
        }
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────

/// Parse all `-export([...]).` attributes and collect `(name, arity)` pairs.
///
/// Handles multi-line export lists and multiple export attributes.
fn parse_exports(source: &str) -> Vec<(String, usize)> {
    let mut exports = Vec::new();
    let mut in_export = false;
    let mut buffer = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        if in_export {
            buffer.push(' ');
            buffer.push_str(trimmed);
            if buffer.contains("]).") {
                extract_exports_from_buffer(&buffer, &mut exports);
                buffer.clear();
                in_export = false;
            }
        } else if trimmed.starts_with("-export(") || trimmed.starts_with("-export (") {
            buffer = trimmed.to_string();
            if buffer.contains("]).") {
                extract_exports_from_buffer(&buffer, &mut exports);
                buffer.clear();
            } else {
                in_export = true;
            }
        }
    }

    exports
}

/// Extract `name/arity` pairs from a complete `-export([...]).` buffer.
fn extract_exports_from_buffer(buffer: &str, exports: &mut Vec<(String, usize)>) {
    // Find content between [ and ]
    let Some(start) = buffer.find('[') else {
        return;
    };
    let Some(end) = buffer.find(']') else { return };
    let content = &buffer[start + 1..end];

    for item in content.split(',') {
        let item = item.trim();
        if let Some((name, arity_str)) = item.split_once('/') {
            let name = name.trim();
            // Strip surrounding quotes from quoted atom names (e.g. 'foo' -> foo).
            let name = name.strip_prefix('\'').and_then(|n| n.strip_suffix('\''))
                .unwrap_or(name);
            if let Ok(arity) = arity_str.trim().parse::<usize>() {
                exports.push((name.to_string(), arity));
            }
        }
    }
}

/// Try to parse a function name from a line that looks like a function head.
///
/// Returns `Some(name)` if the line starts with a lowercase letter and contains `(`.
fn parse_function_head(line: &str) -> Option<&str> {
    if line.is_empty() {
        return None;
    }
    let first_char = line.chars().next()?;
    if !first_char.is_ascii_lowercase() && first_char != '\'' {
        return None;
    }

    // Handle quoted atom function names: 'name'(...)
    if first_char == '\'' {
        let end_quote = line[1..].find('\'')?;
        let after_name = &line[end_quote + 2..];
        if after_name.starts_with('(') {
            return Some(&line[1..=end_quote]);
        }
        return None;
    }

    // Regular function name: name(...)
    let paren_pos = line.find('(')?;
    let name = &line[..paren_pos];

    // Validate: function name must be alphanumeric + underscores
    if name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') && !name.is_empty() {
        Some(name)
    } else {
        None
    }
}

/// Count the arity of a function from its first clause head.
///
/// Simple heuristic: count top-level commas in the parameter list.
fn count_function_arity(line: &str) -> usize {
    let Some(open) = line.find('(') else {
        return 0;
    };

    // Find matching close paren.
    let after_open = &line[open + 1..];

    // Check for empty args: foo() ->
    let trimmed = after_open.trim_start();
    if trimmed.starts_with(')') {
        return 0;
    }

    // Count top-level commas (not inside nested parens/brackets/braces).
    let mut depth = 0;
    let mut commas = 0;
    for ch in after_open.chars() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            ',' if depth == 0 => commas += 1,
            _ => {}
        }
    }

    commas + 1
}

/// Check if a `-doc` attribute appears in the lines immediately preceding `line_no`.
///
/// Scans backwards over blank lines, comments, and `-spec` attributes (including
/// multi-line specs) to find a `-doc` attribute. Stops at function boundaries
/// to avoid borrowing a previous function's `-doc`.
fn has_preceding_doc(line_no: usize, lines: &[&str]) -> bool {
    let mut i = line_no;
    let mut in_spec = false;
    while i > 0 {
        i -= 1;
        let trimmed = lines[i].trim();
        if trimmed.is_empty() || trimmed.starts_with('%') {
            continue;
        }
        if trimmed.starts_with("-doc") {
            return true;
        }
        if trimmed.starts_with("-spec") {
            // Found start of a (possibly multi-line) spec — keep scanning.
            in_spec = false;
            continue;
        }
        if in_spec {
            // Inside a multi-line spec (scanning backwards) — skip.
            continue;
        }
        // If this line looks like a function clause (starts with a function
        // name at column 0 — not indented), it's a boundary — stop to avoid
        // borrowing the previous function's `-doc`. Erlang function definitions
        // always start at column 0; spec continuations are indented.
        if trimmed.len() == lines[i].len() && parse_function_head(trimmed).is_some() {
            break;
        }
        // Otherwise it's likely a continuation of a multi-line attribute
        // (e.g. the tail of a multi-line spec). Enter spec-scanning mode.
        if !trimmed.starts_with('-') {
            in_spec = true;
            continue;
        }
        // Hit a different attribute — stop.
        break;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8Path;

    fn lint(source: &str) -> Vec<ErlangLintDiagnostic> {
        lint_erl_file(Utf8Path::new("test.erl"), source)
    }

    // ── moduledoc checks ────────────────────────────────────────────────

    #[test]
    fn missing_moduledoc_detected() {
        let source = "-module(foo).\n-export([bar/0]).\n-doc \"Bar.\"\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("moduledoc"))
            .collect();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].line, 1);
    }

    #[test]
    fn moduledoc_present_no_diagnostic() {
        let source = "-module(foo).\n-moduledoc \"Foo module.\".\n-export([bar/0]).\n-doc \"Bar.\"\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("moduledoc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn moduledoc_false_no_diagnostic() {
        let source =
            "-module(foo).\n-moduledoc false.\n-export([bar/0]).\n-doc \"Bar.\"\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("moduledoc"))
            .collect();
        assert!(diags.is_empty());
    }

    // ── doc on exports checks ───────────────────────────────────────────

    #[test]
    fn missing_doc_on_export_detected() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/0]).\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("bar/0"));
    }

    #[test]
    fn doc_present_no_diagnostic() {
        let source =
            "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/0]).\n-doc \"Bar.\"\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn doc_with_spec_between() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/1]).\n-doc \"Bar.\"\n-spec bar(integer()) -> integer().\nbar(X) -> X.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn otp_callback_skipped() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([init/1, handle_call/3]).\ninit(Args) -> {ok, Args}.\nhandle_call(_Request, _From, State) -> {reply, ok, State}.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn non_exported_function_not_checked() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/0]).\n-doc \"Bar.\"\nbar() -> baz().\nbaz() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn multi_line_export() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([\n    bar/0,\n    baz/1\n]).\n-doc \"Bar.\"\nbar() -> ok.\n-doc \"Baz.\"\nbaz(X) -> X.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn multi_line_export_missing_doc() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([\n    bar/0,\n    baz/1\n]).\nbar() -> ok.\nbaz(X) -> X.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert_eq!(diags.len(), 2);
    }

    #[test]
    fn multi_clause_function_only_checked_once() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/1]).\n-doc \"Bar.\"\nbar(1) -> one;\nbar(2) -> two.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(
            diags.is_empty(),
            "multi-clause function with -doc on first clause should not trigger; got: {diags:?}"
        );
    }

    #[test]
    fn multi_line_spec_with_doc() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([bar/2]).\n-doc \"Bar.\"\n-spec bar(integer(),\n          binary()) -> ok.\nbar(X, Y) -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("-doc"))
            .collect();
        assert!(
            diags.is_empty(),
            "multi-line spec between -doc and function should not trigger; got: {diags:?}"
        );
    }

    #[test]
    fn undocumented_function_does_not_borrow_previous_doc() {
        // bar() should not borrow foo()'s -doc attribute.
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n-export([foo/0, bar/0]).\n-doc \"Foo.\"\nfoo() -> ok.\n\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("bar/0"))
            .collect();
        assert_eq!(
            diags.len(),
            1,
            "bar/0 should be flagged as missing -doc; got: {diags:?}"
        );
    }

    // ── hardcoded bt@ references ────────────────────────────────────────

    #[test]
    fn hardcoded_bt_ref_detected() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\nbar() -> 'bt@pkg@myclass':new().\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("hardcoded"))
            .collect();
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("bt@pkg@myclass"));
    }

    #[test]
    fn bt_ref_in_comment_not_detected() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\n% 'bt@pkg@myclass' is the module atom\nbar() -> ok.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("hardcoded"))
            .collect();
        assert!(diags.is_empty());
    }

    #[test]
    fn multiple_bt_refs_on_one_line() {
        let source = "-module(foo).\n-moduledoc \"Foo.\".\nbar() -> {'bt@a@b', 'bt@c@d'}.\n";
        let diags: Vec<_> = lint(source)
            .into_iter()
            .filter(|d| d.message.contains("hardcoded"))
            .collect();
        assert_eq!(diags.len(), 2);
    }

    // ── parse_exports ───────────────────────────────────────────────────

    #[test]
    fn parse_single_export() {
        let exports = parse_exports("-export([foo/2]).");
        assert_eq!(exports, vec![("foo".to_string(), 2)]);
    }

    #[test]
    fn parse_multiple_exports() {
        let exports = parse_exports("-export([foo/0, bar/1, baz/2]).");
        assert_eq!(exports.len(), 3);
    }

    #[test]
    fn parse_multi_line_exports() {
        let exports = parse_exports("-export([\n    foo/0,\n    bar/1\n]).\n");
        assert_eq!(exports.len(), 2);
    }

    #[test]
    fn parse_multiple_export_attributes() {
        let exports = parse_exports("-export([foo/0]).\n-export([bar/1]).\n");
        assert_eq!(exports.len(), 2);
    }

    // ── count_function_arity ────────────────────────────────────────────

    #[test]
    fn arity_zero() {
        assert_eq!(count_function_arity("foo() ->"), 0);
    }

    #[test]
    fn arity_one() {
        assert_eq!(count_function_arity("foo(X) ->"), 1);
    }

    #[test]
    fn arity_three() {
        assert_eq!(count_function_arity("foo(A, B, C) ->"), 3);
    }

    #[test]
    fn arity_with_nested_tuple() {
        assert_eq!(count_function_arity("foo({A, B}, C) ->"), 2);
    }

    #[test]
    fn arity_with_nested_list() {
        assert_eq!(count_function_arity("foo([H|T], Acc) ->"), 2);
    }
}
