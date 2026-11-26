// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk-expression builders for REPL meta-commands, shared between the
//! `beamtalk` binary's dispatch (`commands/repl/handlers.rs`) and the
//! `tests/repl_protocol.rs` integration-test harness.
//!
//! These live in the library target (rather than `commands/repl/mod.rs`,
//! which is private to the binary target) specifically so the integration
//! test — which links only the library target — can call the real parsing
//! logic instead of re-implementing it. A hand-copied "mirror" of
//! `remove_method_expr_for`'s logic in a test harness risks drifting
//! from the real implementation; `:flush <scope>`'s equivalent duplication
//! was extracted the same way, for the same reason.

/// Construct the `<Class> removeSelector: #<selector>` expression a
/// `:remove-method <Class> <selector>` REPL line dispatches to (ADR 0112
/// Phase 4).
///
/// Splits on the first run of whitespace: the first token is the class, the
/// remainder (trimmed) is the selector. A leading `#` on the selector is
/// stripped and the result re-trimmed, mirroring the MCP `remove_method`
/// tool's `selector` parameter and the LSP `beamtalk.removeMethod` command,
/// so `:remove-method Counter #increment` and `:remove-method Counter
/// increment` both work. Returns `None` when either half is missing or
/// empty (including a selector that strips down to nothing, e.g. bare `#`
/// or `#` followed only by whitespace), so the caller can print a usage
/// hint instead of evaluating a malformed expression.
pub fn remove_method_expr_for(arg: &str) -> Option<String> {
    let arg = arg.trim();
    let mut parts = arg.splitn(2, char::is_whitespace);
    let class = parts.next().unwrap_or("").trim();
    let selector = parts.next().unwrap_or("").trim();
    let selector = selector.strip_prefix('#').unwrap_or(selector).trim();
    if class.is_empty() || selector.is_empty() {
        return None;
    }
    Some(beamtalk_core::tool_expr::remove_method_expr(
        class, selector,
    ))
}

/// What a `:flush [<scope>]` / `:flush-destructive [<scope>]` REPL line is
/// scoped to, parsed from its argument. The `flush` operation takes the scope
/// as one of three params, so the line is parsed once here, for both
/// commands, and sent through [`ReplFlushScope::as_scope`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplFlushScope {
    /// A class name (`Counter`, or `#Counter` for a class `:remove-class`
    /// already unbound: the `ChangeLog` matches it by name).
    Class(String),
    /// An entry kind from [`beamtalk_repl_protocol::FLUSH_KINDS`]
    /// (`#'new-class'`, `#remove-class`).
    Kind(String),
    /// A source file (`#{ #file => "path" }`).
    File(String),
}

impl ReplFlushScope {
    /// The scope as the `flush` request builder takes it.
    #[must_use]
    pub fn as_scope(&self) -> beamtalk_repl_protocol::FlushScope<'_> {
        match self {
            Self::Class(c) => beamtalk_repl_protocol::FlushScope::Class(c),
            Self::Kind(k) => beamtalk_repl_protocol::FlushScope::Kind(k),
            Self::File(f) => beamtalk_repl_protocol::FlushScope::File(f),
        }
    }
}

/// Parse the argument of `:flush <scope>` / `:flush-destructive <scope>`.
///
/// Accepts a class (`Counter`), a Symbol (`#'new-class'`, `#remove-class`,
/// `#Counter`) and a one-key dictionary (`#{ #file => "src/foo.bt" }`), the
/// forms these commands took when they evaluated `Workspace flush:`. A Symbol
/// naming one of the five entry kinds is a kind scope; any other Symbol names a
/// class, which `:flush-destructive #Class` needs after `:remove-class` has
/// unbound the name.
///
/// Returns `None` for an empty (or whitespace-only) argument and for one in
/// none of those forms, so the caller prints a usage hint instead of sending an
/// unscoped flush: an unreadable scope must never widen into "everything".
#[must_use]
pub fn flush_scope_for(arg: &str) -> Option<ReplFlushScope> {
    let arg = arg.trim();
    if let Some(dict) = arg.strip_prefix("#{") {
        let (key, path) = dict.trim_end().strip_suffix('}')?.split_once("=>")?;
        if key.trim() != "#file" {
            return None;
        }
        let path = path.trim().strip_prefix('"')?.strip_suffix('"')?;
        return (!path.is_empty() && !path.contains('"'))
            .then(|| ReplFlushScope::File(path.to_string()));
    }
    if let Some(symbol) = arg.strip_prefix('#') {
        let name = symbol.trim().trim_matches('\'');
        if name.is_empty() {
            return None;
        }
        if beamtalk_repl_protocol::FLUSH_KINDS.contains(&name) {
            return Some(ReplFlushScope::Kind(name.to_string()));
        }
        return is_class_name(name).then(|| ReplFlushScope::Class(name.to_string()));
    }
    is_class_name(arg).then(|| ReplFlushScope::Class(arg.to_string()))
}

fn is_class_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_ascii_uppercase())
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Construct the `<Class> removeFromSystem` expression a `:remove-class
/// <Class>` REPL line dispatches to (ADR 0113 Phase 4).
///
/// `arg` is a single class name — trimmed, and returned as `None` when empty
/// (including whitespace-only) so the caller can print a usage hint instead
/// of evaluating a malformed bare `removeFromSystem` send. Unlike
/// [`remove_method_expr_for`], there is no second (selector) token to split
/// off and no leading-`#` stripping — a class name is never symbol-decorated.
///
/// The real REPL dispatch (`commands/repl/handlers.rs`) prompts for `y/N`
/// confirmation before calling this — a destructive, memory-mutating
/// operation per ADR 0113's Surface section — but that terminal-only
/// confirmation step is orthogonal to expression construction, so it isn't
/// modeled here (matching how `flush_expr_for`/`remove_method_expr_for`
/// don't model any REPL-side confirmation either).
pub fn remove_class_expr_for(arg: &str) -> Option<String> {
    let class = arg.trim();
    if class.is_empty() {
        return None;
    }
    Some(format!("{class} removeFromSystem"))
}

/// Construct the `<Class> renameTo: #<NewName>` expression a `:rename-class
/// <Class> <NewName>` REPL line dispatches to (ADR 0114 Phase 5).
///
/// Splits on whitespace into exactly two tokens — the current class name and
/// the new one. Unlike [`remove_method_expr_for`]'s selector, the first
/// token is never symbol-decorated (a class name is a bare identifier); the
/// second gets the same optional-leading-`#`-strip treatment for symmetry
/// with how the resulting Symbol literal is written. Returns `None` when the
/// argument doesn't split into exactly two non-empty tokens (after
/// `#`-stripping the second), so the caller can print a usage hint instead
/// of evaluating a malformed `renameTo:` send.
///
/// The real REPL dispatch (`commands/repl/handlers.rs`) prompts for `y/N`
/// confirmation before calling this — a destructive, memory-mutating
/// operation per ADR 0114's Surface section (reusing ADR 0113's) — but that
/// terminal-only confirmation step is orthogonal to expression construction,
/// so it isn't modeled here, matching [`remove_class_expr_for`].
pub fn rename_class_expr_for(arg: &str) -> Option<String> {
    let mut parts = arg.split_whitespace();
    let old_name = parts.next()?;
    let new_name = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let new_name = new_name.strip_prefix('#').unwrap_or(new_name);
    if new_name.is_empty() {
        return None;
    }
    Some(beamtalk_core::tool_expr::rename_class_expr(
        old_name, new_name,
    ))
}

/// Construct the `<Class> renameSelector: #<OldSelector> to: #<NewSelector>`
/// expression a `:rename-method <Class> <OldSelector> <NewSelector>` REPL
/// line dispatches to (ADR 0114 Phase 5). Instance-side only — sent
/// to a bare class name, this always touches the instance-side method table;
/// a class-side rename needs a direct `Counter class renameSelector: ... to:
/// ...` eval, the same chokepoint limitation [`remove_method_expr_for`] has
/// (`docs/development/surface-parity.md`'s `remove-method` row).
///
/// Splits on whitespace into exactly three tokens — the class, the current
/// selector, and the new selector — stripping an optional leading `#` off
/// each selector token (mirroring `remove_method_expr_for`), so
/// `:rename-method Counter increment incrementBy` and `:rename-method
/// Counter #increment #incrementBy` both work. Returns `None` when the
/// argument doesn't split into exactly three tokens, or either selector
/// strips down to empty (e.g. a bare `#`), so the caller can print a usage
/// hint instead of evaluating a malformed `renameSelector:to:` send.
pub fn rename_method_expr_for(arg: &str) -> Option<String> {
    let mut parts = arg.split_whitespace();
    let class = parts.next()?;
    let old_selector = parts.next()?;
    let new_selector = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let old_selector = old_selector.strip_prefix('#').unwrap_or(old_selector);
    let new_selector = new_selector.strip_prefix('#').unwrap_or(new_selector);
    if old_selector.is_empty() || new_selector.is_empty() {
        return None;
    }
    Some(beamtalk_core::tool_expr::rename_method_expr(
        class,
        old_selector,
        new_selector,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Asserts each `(case, argument, expected)` row of `table` through `f`.
    /// The case name and the argument are in the failure message.
    fn check(f: fn(&str) -> Option<String>, table: &[(&str, &str, Option<&str>)]) {
        for &(case, argument, expected) in table {
            assert_eq!(
                f(argument).as_deref(),
                expected,
                "case `{case}`: argument {argument:?}"
            );
        }
    }

    #[test]
    fn remove_method_expr_for_cases() {
        check(
            remove_method_expr_for,
            &[
                (
                    "translates_to_remove_selector_send",
                    "Counter increment",
                    Some("Counter removeSelector: #increment"),
                ),
                (
                    "preserves_keyword_selectors",
                    "Dict at:put:",
                    Some("Dict removeSelector: #at:put:"),
                ),
                (
                    "strips_leading_hash_on_selector",
                    "Counter #increment",
                    Some("Counter removeSelector: #increment"),
                ),
                (
                    "trims_surrounding_whitespace",
                    "  Counter   increment  ",
                    Some("Counter removeSelector: #increment"),
                ),
                (
                    "with_missing_selector_reports_no_expression",
                    "Counter",
                    None,
                ),
                (
                    "with_missing_selector_reports_no_expression",
                    "Counter ",
                    None,
                ),
                ("with_empty_argument_reports_no_expression", "", None),
                ("with_empty_argument_reports_no_expression", "   ", None),
                // A selector of just "#" strips down to empty and must hit the
                // usage-error path, not a malformed `Counter removeSelector: #` eval.
                (
                    "with_bare_hash_selector_reports_no_expression",
                    "Counter #",
                    None,
                ),
                // "#" followed by only whitespace must re-trim to empty and hit the
                // usage-error path, not just the bare-hash case above.
                (
                    "with_only_whitespace_after_hash_reports_no_expression",
                    "Counter #  ",
                    None,
                ),
                // Whitespace right after "#" (before real selector content) must be
                // trimmed away, producing a clean expression instead of the
                // malformed `Counter removeSelector: #  increment`.
                (
                    "strips_whitespace_between_hash_and_selector",
                    "Counter #  increment",
                    Some("Counter removeSelector: #increment"),
                ),
            ],
        );
    }

    #[test]
    fn flush_scope_for_cases() {
        use ReplFlushScope::{Class, File, Kind};
        let class = |c: &str| Some(Class(c.to_string()));
        let kind = |k: &str| Some(Kind(k.to_string()));
        for (arg, want) in [
            ("Counter", class("Counter")),
            ("  Counter  ", class("Counter")),
            // The five entry kinds are kind scopes, in either Symbol spelling.
            ("#'new-class'", kind("new-class")),
            ("#new-class", kind("new-class")),
            ("#'remove-class'", kind("remove-class")),
            ("#rename-method", kind("rename-method")),
            // Any other Symbol names a class: `:flush-destructive #Gone` after
            // `:remove-class Gone` has unbound the bare name.
            ("#Counter", class("Counter")),
            (
                "#{ #file => \"src/foo.bt\" }",
                Some(File("src/foo.bt".to_string())),
            ),
            (
                "#{#file=>\"src/foo.bt\"}",
                Some(File("src/foo.bt".to_string())),
            ),
            // Nothing readable is never an unscoped flush.
            ("", None),
            ("   ", None),
            ("#", None),
            ("#''", None),
            ("#{ #file => \"\" }", None),
            ("#{ #class => \"Counter\" }", None),
            ("#{ #file => src/foo.bt }", None),
            ("#{ #file => \"a\" , #x => 1 }", None),
            ("two words", None),
            ("new-class", None),
            ("9Lives", None),
            // A selector is not a class: a lowercase name would match no entry
            // and report `Flushed 0`, so it is a usage error.
            ("#increment", None),
            ("counter", None),
        ] {
            assert_eq!(flush_scope_for(arg), want, "{arg:?}");
        }
    }

    /// `FlushScope` is what the request builder takes: a class, a kind and a
    /// file each map to exactly one param.
    #[test]
    fn repl_flush_scope_maps_onto_the_request_scope() {
        use beamtalk_repl_protocol::FlushScope;
        assert_eq!(
            ReplFlushScope::Class("C".into()).as_scope(),
            FlushScope::Class("C")
        );
        assert_eq!(
            ReplFlushScope::Kind("new-class".into()).as_scope(),
            FlushScope::Kind("new-class")
        );
        assert_eq!(
            ReplFlushScope::File("a.bt".into()).as_scope(),
            FlushScope::File("a.bt")
        );
    }

    #[test]
    fn remove_class_expr_for_cases() {
        check(
            remove_class_expr_for,
            &[
                (
                    "remove_class_translates_to_remove_from_system_send",
                    "Counter",
                    Some("Counter removeFromSystem"),
                ),
                (
                    "remove_class_trims_surrounding_whitespace",
                    "  Counter  ",
                    Some("Counter removeFromSystem"),
                ),
                (
                    "remove_class_with_empty_argument_reports_no_expression",
                    "",
                    None,
                ),
                (
                    "remove_class_with_empty_argument_reports_no_expression",
                    "   ",
                    None,
                ),
            ],
        );
    }

    #[test]
    fn rename_class_expr_for_cases() {
        check(
            rename_class_expr_for,
            &[
                (
                    "rename_class_translates_to_rename_to_send",
                    "Counter Accumulator",
                    Some("Counter renameTo: #Accumulator"),
                ),
                (
                    "rename_class_strips_leading_hash_on_new_name",
                    "Counter #Accumulator",
                    Some("Counter renameTo: #Accumulator"),
                ),
                (
                    "rename_class_trims_surrounding_whitespace",
                    "  Counter   Accumulator  ",
                    Some("Counter renameTo: #Accumulator"),
                ),
                (
                    "rename_class_with_missing_new_name_reports_no_expression",
                    "Counter",
                    None,
                ),
                (
                    "rename_class_with_missing_new_name_reports_no_expression",
                    "Counter ",
                    None,
                ),
                (
                    "rename_class_with_empty_argument_reports_no_expression",
                    "",
                    None,
                ),
                (
                    "rename_class_with_empty_argument_reports_no_expression",
                    "   ",
                    None,
                ),
                (
                    "rename_class_with_extra_token_reports_no_expression",
                    "Counter Accumulator Extra",
                    None,
                ),
                (
                    "rename_class_with_bare_hash_new_name_reports_no_expression",
                    "Counter #",
                    None,
                ),
            ],
        );
    }

    #[test]
    fn rename_method_expr_for_cases() {
        check(
            rename_method_expr_for,
            &[
                (
                    "rename_method_translates_to_rename_selector_to_send",
                    "Counter increment incrementBy",
                    Some("Counter renameSelector: #increment to: #incrementBy"),
                ),
                (
                    "rename_method_preserves_keyword_selectors",
                    "Dict at:put: atKey:put:",
                    Some("Dict renameSelector: #at:put: to: #atKey:put:"),
                ),
                (
                    "rename_method_strips_leading_hash_on_both_selectors",
                    "Counter #increment #incrementBy",
                    Some("Counter renameSelector: #increment to: #incrementBy"),
                ),
                (
                    "rename_method_trims_surrounding_whitespace",
                    "  Counter   increment   incrementBy  ",
                    Some("Counter renameSelector: #increment to: #incrementBy"),
                ),
                (
                    "rename_method_with_missing_tokens_reports_no_expression",
                    "Counter",
                    None,
                ),
                (
                    "rename_method_with_missing_tokens_reports_no_expression",
                    "Counter increment",
                    None,
                ),
                (
                    "rename_method_with_missing_tokens_reports_no_expression",
                    "",
                    None,
                ),
                (
                    "rename_method_with_missing_tokens_reports_no_expression",
                    "   ",
                    None,
                ),
                (
                    "rename_method_with_extra_token_reports_no_expression",
                    "Counter increment incrementBy Extra",
                    None,
                ),
                (
                    "rename_method_with_bare_hash_selector_reports_no_expression",
                    "Counter # incrementBy",
                    None,
                ),
                (
                    "rename_method_with_bare_hash_selector_reports_no_expression",
                    "Counter increment #",
                    None,
                ),
            ],
        );
    }
}
