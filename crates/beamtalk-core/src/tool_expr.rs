// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk expression synthesis shared by the MCP and LSP tooling surfaces
//! (BT-3193).
//!
//! **DDD Context:** Language Service — Tooling Surface Parity
//!
//! `beamtalk-mcp`'s typed tools (`save_class`, `precheck_method`, `flush`,
//! `remove_method`) and `beamtalk-lsp`'s `workspace/executeCommand` handlers
//! (`CMD_SAVE_CLASS`, `CMD_PRECHECK_METHOD`, `CMD_FLUSH*`, `CMD_REMOVE_METHOD`)
//! each compile client-supplied parameters into a Beamtalk expression string
//! submitted through the workspace's existing `evaluate` REPL op (ADR 0082
//! "Rationale: why no new REPL ops"). Both surfaces used to hand-roll the
//! same `format!()` shape independently, documented only in prose as
//! "mirroring" the other surface — nothing enforced it, so the two could
//! silently drift (found in BT-3188 review).
//!
//! Per `docs/development/architecture-principles.md` § Duplication & the
//! Shared-Leaf-Module Pattern, and its § Consistency-Test Disposition Rule:
//! `beamtalk-mcp` and `beamtalk-lsp` are two Rust crates in the same
//! workspace, not a permanent cross-language/cross-process boundary, and
//! both already depend on `beamtalk-core` — so this is a *deletable*
//! duplication. The fix is to give the expression-building logic a single
//! home here, below both crates, rather than adding a test that just checks
//! two copies still agree. A single definition can't drift from itself; the
//! unit tests below are the conformance test BT-3193 asks for.

use crate::unparse::escape_string_literal;

/// Build the Beamtalk expression for the `save_class` MCP tool / LSP
/// `beamtalk.saveClass` command — new-class creation path (ADR 0082 Phase 3).
pub fn save_class_expr(source: &str, path: &str) -> String {
    format!(
        "Workspace newClass: \"{}\" at: \"{}\"",
        escape_string_literal(source),
        escape_string_literal(path),
    )
}

/// Build the Beamtalk expression for the `precheck_method` MCP tool / LSP
/// `beamtalk.precheckMethod` command — the pre-save advisory precheck (ADR
/// 0105 Phase 3, BT-2782). Selector is the bare form (no leading `#`).
/// Nothing installs; `Behaviour>>precheckCompile:source:` is read-only.
pub fn precheck_method_expr(class: &str, selector: &str, body: &str) -> String {
    format!(
        "{} precheckCompile: #{} source: \"{}\"",
        class,
        selector,
        escape_string_literal(body),
    )
}

/// Build the Beamtalk expression for the `remove_method` MCP tool / LSP
/// `beamtalk.removeMethod` command — the no-fallback path (ADR 0112 Phase 4,
/// BT-3188). Selector is the bare form (no leading `#`). Raises
/// `selector_not_found` if the selector is not defined locally or as an
/// extension.
pub fn remove_method_expr(class: &str, selector: &str) -> String {
    format!("{class} removeSelector: #{selector}")
}

/// Build the Beamtalk expression for the `remove_method` MCP tool's /
/// LSP `beamtalk.removeMethod` command's `if_absent`/`ifAbsent` fallback
/// path (ADR 0112 Phase 4, BT-3188). Unlike `precheck_method_expr`'s `body`,
/// `if_absent` is raw Beamtalk expression code, not a String value: it
/// becomes the body of the `ifAbsent:` fallback block literal, which the
/// runtime evaluates as code on an absent selector — it is never passed
/// through a `compile:source:`-style primitive that takes a source string as
/// data.
pub fn remove_method_if_absent_expr(class: &str, selector: &str, if_absent: &str) -> String {
    format!("{class} removeSelector: #{selector} ifAbsent: [{if_absent}]")
}

/// Scope filter for the `flush` MCP tool / LSP `beamtalk.flush*` commands
/// (ADR 0082 Phase 3). Mutually exclusive; each tool wrapper enforces this
/// before constructing the expression.
#[derive(Clone, Copy, Debug)]
pub enum FlushFilter<'a> {
    None,
    Class(&'a str),
    File(&'a str),
    Kind(&'a str),
}

/// Build the Beamtalk expression for the `flush` MCP tool / LSP
/// `beamtalk.flush*` commands.
///
/// Surface map: `Workspace flush` / `Workspace flush: ClassName` /
/// `Workspace flush: #{ #file => "path" }` / `Workspace flush: #'kind'`.
pub fn flush_expr(filter: FlushFilter<'_>) -> String {
    match filter {
        FlushFilter::None => "Workspace flush".to_string(),
        FlushFilter::Class(class) => format!("Workspace flush: {class}"),
        FlushFilter::File(file) => format!(
            "Workspace flush: #{{ #file => \"{}\" }}",
            escape_string_literal(file)
        ),
        FlushFilter::Kind(kind) => format!("Workspace flush: #'{kind}'"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // These golden tests are the BT-3193 conformance suite: both
    // `beamtalk-mcp` and `beamtalk-lsp` call these functions directly, so a
    // single passing suite here is enough to guarantee the two surfaces
    // agree — there is no second implementation left to drift.

    #[test]
    fn precheck_method_expr_compiles_precheck_compile_source() {
        // `precheck_method` / `beamtalk.precheckMethod` → `aClass
        // precheckCompile: #selector source: body`.
        assert_eq!(
            precheck_method_expr("Counter", "getCount", "getCount => \"nope\""),
            "Counter precheckCompile: #getCount source: \"getCount => \\\"nope\\\"\"",
        );
    }

    #[test]
    fn precheck_method_expr_preserves_keyword_selectors() {
        assert_eq!(
            precheck_method_expr("Dict", "at:put:", "..."),
            "Dict precheckCompile: #at:put: source: \"...\"",
        );
    }

    #[test]
    fn save_class_expr_compiles_new_class_creation() {
        // `save_class` / `beamtalk.saveClass` → `Workspace newClass: source
        // at: path`.
        assert_eq!(
            save_class_expr("Object subclass: Greeter", "src/greeter.bt"),
            "Workspace newClass: \"Object subclass: Greeter\" at: \"src/greeter.bt\"",
        );
    }

    #[test]
    fn save_class_expr_escapes_source_quotes_and_braces() {
        // A class source containing string-interpolation literals must come
        // through as a String value, not as embedded Beamtalk source.
        // Newlines pass through verbatim — Beamtalk strings are multi-line.
        let source = "Object subclass: Greeter\n  greet => \"hi {name}\"";
        let got = save_class_expr(source, "src/greeter.bt");
        assert_eq!(
            got,
            "Workspace newClass: \"Object subclass: Greeter\n  greet => \\\"hi \\{name}\\\"\" at: \"src/greeter.bt\"",
        );
    }

    // --- ADR 0112 Phase 4 (BT-3188): remove_method / beamtalk.removeMethod ---

    #[test]
    fn remove_method_expr_compiles_remove_selector() {
        // `remove_method` / `beamtalk.removeMethod` → `aClass
        // removeSelector: #selector`.
        assert_eq!(
            remove_method_expr("Counter", "increment"),
            "Counter removeSelector: #increment",
        );
    }

    #[test]
    fn remove_method_expr_preserves_keyword_selectors() {
        assert_eq!(
            remove_method_expr("Dict", "at:put:"),
            "Dict removeSelector: #at:put:",
        );
    }

    #[test]
    fn remove_method_if_absent_expr_compiles_fallback_block() {
        // `if_absent` is raw Beamtalk code — it becomes the fallback block's
        // body verbatim, not an escaped String value.
        assert_eq!(
            remove_method_if_absent_expr("Counter", "bogus", "\"not found\""),
            "Counter removeSelector: #bogus ifAbsent: [\"not found\"]",
        );
    }

    #[test]
    fn flush_expr_no_filter() {
        assert_eq!(flush_expr(FlushFilter::None), "Workspace flush");
    }

    #[test]
    fn flush_expr_class_filter() {
        assert_eq!(
            flush_expr(FlushFilter::Class("Counter")),
            "Workspace flush: Counter",
        );
    }

    #[test]
    fn flush_expr_file_filter_uses_file_dict() {
        // The Beamtalk-side flush: selector accepts a Dictionary
        // `#{ #file => "..." }` (see Workspace.bt flush: docs).
        assert_eq!(
            flush_expr(FlushFilter::File("src/foo.bt")),
            "Workspace flush: #{ #file => \"src/foo.bt\" }",
        );
    }

    #[test]
    fn flush_expr_file_filter_escapes_path() {
        assert_eq!(
            flush_expr(FlushFilter::File("src/\"weird\".bt")),
            "Workspace flush: #{ #file => \"src/\\\"weird\\\".bt\" }",
        );
    }

    #[test]
    fn flush_expr_kind_filter_uses_quoted_symbol() {
        // Hyphenated kinds (e.g. `new-class`) require a quoted-symbol literal
        // — Beamtalk symbol literals without quotes only accept identifiers.
        assert_eq!(
            flush_expr(FlushFilter::Kind("new-class")),
            "Workspace flush: #'new-class'",
        );
    }
}
