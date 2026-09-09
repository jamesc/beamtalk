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
//! two copies still agree. MCP and LSP can no longer drift from each other,
//! since both now call the same definition; the unit tests below are the
//! conformance test BT-3193 asks for.
//!
//! The REPL-CLI surface (`beamtalk-cli`'s `:remove-method`/`:flush <sel>`
//! meta-commands) still hand-rolls its own copy of the `removeSelector:` and
//! `flush:` shapes rather than calling into this module — that pre-existing
//! duplication is tracked separately by BT-3196, not fixed here.
//!
//! # Caller responsibility: `class`/`selector`/`kind` are not validated here
//!
//! Only the string-*value* arguments (`source`, `path`, `body`) are escaped
//! via [`escape_string_literal`] before being embedded as Beamtalk string
//! literals. `class`, `selector`, and `kind` are interpolated **unescaped**
//! as bare Beamtalk source tokens (class names, symbol literals) — that's
//! the correct shape for e.g. `Counter removeSelector: #increment`, where
//! `Counter` must appear as a real identifier, not a quoted string. Passing
//! attacker-controlled or otherwise unvalidated text through as `class` or
//! `selector` lets it inject arbitrary Beamtalk source into the expression
//! that gets evaluated. Both current callers (`beamtalk-mcp`'s tool handlers
//! and `beamtalk-lsp`'s `validate_class_name`/`validate_selector`) validate
//! these arguments before calling in here — any new caller must do the same.

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
///
/// `class` and `selector` are interpolated unescaped — see the module docs'
/// "Caller responsibility" note; the caller must validate them first.
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
///
/// `class` and `selector` are interpolated unescaped — see the module docs'
/// "Caller responsibility" note; the caller must validate them first.
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
///
/// `class` and `selector` are interpolated unescaped — see the module docs'
/// "Caller responsibility" note; the caller must validate them first.
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
///
/// `FlushFilter::Class` and `FlushFilter::Kind` are interpolated unescaped —
/// see the module docs' "Caller responsibility" note; the caller must
/// validate them first (`FlushFilter::File`'s path is a String value and is
/// escaped).
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

/// Build the Beamtalk expression for the `flush` MCP tool's / LSP
/// `beamtalk.flush*` command's Tier-2 (destructive) gate (ADR 0113 "Surface",
/// BT-3207/BT-3210/BT-3209).
///
/// `confirm_destructive: false` is textually identical to [`flush_expr`] —
/// Tier 1 only, unchanged. `confirm_destructive: true` reaches Tier 2:
/// unscoped (`FlushFilter::None`) routes to the distinct unscoped
/// `Workspace flushIncludingDestructive` selector, since Smalltalk keyword
/// messages cannot omit an argument the way an unscoped
/// `confirmDestructive:` keyword would need to (ADR 0113 "Decision"); every
/// scoped filter appends ` confirmDestructive: true` to the same
/// `Workspace flush: <filter>` expression `flush_expr` already builds for
/// that filter, reusing `workspace_interface.bt`'s
/// `flush: filter confirmDestructive: confirmDestructive` keyword form.
///
/// Per the ADR's Surface section, this is the caller's confirmation gate for
/// MCP specifically: the tool schema's required boolean argument (no
/// default) *is* the confirmation, mirroring `try_method` → `save_method`'s
/// existing two-step promotion idiom — there is no interactive dialog to
/// gate on at this surface.
pub fn flush_expr_with_confirm_destructive(
    filter: FlushFilter<'_>,
    confirm_destructive: bool,
) -> String {
    if !confirm_destructive {
        return flush_expr(filter);
    }
    match filter {
        FlushFilter::None => "Workspace flushIncludingDestructive".to_string(),
        _ => format!("{} confirmDestructive: true", flush_expr(filter)),
    }
}

/// Build the Beamtalk expression for the `remove_class` MCP tool (ADR 0113
/// Phase 4, BT-3210) — wraps `Behaviour>>removeFromSystem` (BT-785; gains its
/// own `kind: #'remove-class'` ChangeLog-logging fix in ADR 0113 Phase 1,
/// BT-3206).
///
/// Two statements, period-separated (`docs/learning/07-blocks.md`'s
/// statement-separator convention, not block-scoped here): the first removes
/// the class from memory (refusing stdlib/subclassed classes, per BT-785,
/// unchanged by ADR 0113); the second looks up and returns the resulting
/// `remove-class` `ChangeEntry` the removal just appended, so the tool's
/// response reports the entry's `flushable` state directly rather than the
/// bare `nil` `removeFromSystem` itself returns. The lookup is unconditional
/// after a successful removal — ADR 0113 "Fixing `removeFromSystem`'s
/// missing `ChangeLog` entry" establishes the entry always exists once
/// `removeFromSystem` succeeds, so `last` never runs against an empty
/// collection here. Nothing is written to disk by this expression alone —
/// see `flush_expr_with_confirm_destructive` for the required follow-up
/// Tier-2 flush step.
///
/// `class` is interpolated unescaped — see the module docs' "Caller
/// responsibility" note; the caller must validate it first.
pub fn remove_class_expr(class: &str) -> String {
    format!(
        "{class} removeFromSystem. \
         (Workspace changes select: [:e | e isRemoveClass and: [e className =:= #{class}]]) last"
    )
}

/// Build the Beamtalk expression for the `rename_class` MCP tool (ADR 0114
/// Phase 5, BT-3276) — wraps `Behaviour>>renameTo:` (ADR 0114 Phase 2,
/// BT-3278).
///
/// Unlike [`remove_class_expr`], no follow-up `ChangeLog` lookup is chained
/// on: `renameTo:` (return type `Behaviour`) already returns the renamed
/// class itself, a useful response value on its own — `removeFromSystem`
/// (the operation `remove_class_expr` wraps) returns `nil`, which is why
/// that one needs the extra lookup and this one doesn't.
///
/// `class` and `new_name` are interpolated unescaped — see the module docs'
/// "Caller responsibility" note; the caller must validate both first.
pub fn rename_class_expr(class: &str, new_name: &str) -> String {
    format!("{class} renameTo: #{new_name}")
}

/// Build the Beamtalk expression for the `rename_method` MCP tool (ADR 0114
/// Phase 5, BT-3276) — wraps `Behaviour>>renameSelector:to:` (ADR 0114 Phase
/// 3, BT-3279). Instance-side only — sent to a bare class name, this always
/// touches the instance-side method table; a class-side rename needs a
/// direct `Counter class renameSelector: ... to: ...` eval, the same
/// chokepoint limitation `remove_method_expr` has (`docs/development/
/// surface-parity.md`'s `remove-method` row).
///
/// `class`, `selector`, and `new_selector` are interpolated unescaped — see
/// the module docs' "Caller responsibility" note; the caller must validate
/// all three first.
pub fn rename_method_expr(class: &str, selector: &str, new_selector: &str) -> String {
    format!("{class} renameSelector: #{selector} to: #{new_selector}")
}

#[cfg(test)]
mod tests {
    use super::*;

    // These golden tests are the BT-3193 conformance suite: both
    // `beamtalk-mcp` and `beamtalk-lsp` call these functions directly, so a
    // single passing suite here is enough to guarantee those two surfaces
    // agree with each other.

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

    // --- ADR 0113 Phase 4 (BT-3210/BT-3209): destructive-tier flush +
    // remove_class / beamtalk.removeClass ---

    #[test]
    fn flush_expr_with_confirm_destructive_false_matches_flush_expr() {
        for filter in [
            FlushFilter::None,
            FlushFilter::Class("Counter"),
            FlushFilter::File("src/foo.bt"),
            FlushFilter::Kind("new-class"),
        ] {
            assert_eq!(
                flush_expr_with_confirm_destructive(filter, false),
                flush_expr(filter),
            );
        }
    }

    #[test]
    fn flush_expr_with_confirm_destructive_true_and_no_filter_uses_unscoped_selector() {
        // No class/kind/file argument to attach a `confirmDestructive:`
        // keyword to once the call is unscoped (ADR 0113 "Decision") — the
        // distinct bare `flushIncludingDestructive` selector is required.
        assert_eq!(
            flush_expr_with_confirm_destructive(FlushFilter::None, true),
            "Workspace flushIncludingDestructive",
        );
    }

    #[test]
    fn flush_expr_with_confirm_destructive_true_and_class_filter_appends_keyword() {
        assert_eq!(
            flush_expr_with_confirm_destructive(FlushFilter::Class("Counter"), true),
            "Workspace flush: Counter confirmDestructive: true",
        );
    }

    #[test]
    fn flush_expr_with_confirm_destructive_true_and_file_filter_appends_keyword() {
        assert_eq!(
            flush_expr_with_confirm_destructive(FlushFilter::File("src/foo.bt"), true),
            "Workspace flush: #{ #file => \"src/foo.bt\" } confirmDestructive: true",
        );
    }

    #[test]
    fn flush_expr_with_confirm_destructive_true_and_kind_filter_appends_keyword() {
        assert_eq!(
            flush_expr_with_confirm_destructive(FlushFilter::Kind("remove-class"), true),
            "Workspace flush: #'remove-class' confirmDestructive: true",
        );
    }

    #[test]
    fn remove_class_expr_compiles_remove_from_system_then_entry_lookup() {
        assert_eq!(
            remove_class_expr("Counter"),
            "Counter removeFromSystem. \
             (Workspace changes select: [:e | e isRemoveClass and: [e className =:= #Counter]]) last",
        );
    }

    // --- ADR 0114 Phase 5 (BT-3276): rename_class / rename_method ---

    #[test]
    fn rename_class_expr_compiles_rename_to_send() {
        assert_eq!(
            rename_class_expr("Counter", "Accumulator"),
            "Counter renameTo: #Accumulator",
        );
    }

    #[test]
    fn rename_method_expr_compiles_rename_selector_to_send() {
        assert_eq!(
            rename_method_expr("Counter", "increment", "incrementBy"),
            "Counter renameSelector: #increment to: #incrementBy",
        );
    }

    #[test]
    fn rename_method_expr_preserves_keyword_selectors() {
        assert_eq!(
            rename_method_expr("Dict", "at:put:", "atKey:put:"),
            "Dict renameSelector: #at:put: to: #atKey:put:",
        );
    }
}
