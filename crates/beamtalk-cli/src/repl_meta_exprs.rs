// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk-expression builders for REPL meta-commands, shared between the
//! `beamtalk` binary's dispatch (`commands/repl/mod.rs`) and the
//! `tests/repl_protocol.rs` integration-test harness.
//!
//! These live in the library target (rather than `commands/repl/mod.rs`,
//! which is private to the binary target) specifically so the integration
//! test — which links only the library target — can call the real parsing
//! logic instead of re-implementing it. A hand-copied "mirror" of
//! `remove_method_expr_for`'s logic in the test harness previously drifted
//! from the real implementation twice in a row (BT-3189); `:flush <sel>`'s
//! equivalent duplication was extracted the same way in BT-3196.

/// Construct the `<Class> removeSelector: #<selector>` expression a
/// `:remove-method <Class> <selector>` REPL line dispatches to (ADR 0112
/// Phase 4, BT-3189).
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
    Some(format!("{class} removeSelector: #{selector}"))
}

/// Construct the `Workspace flush: <selector>` expression a `:flush
/// <selector>` REPL line dispatches to (ADR 0082 Phase 3, BT-2287).
///
/// `selector` is passed through **verbatim** (only trimmed) into the
/// generated expression, so callers can pass a Class (`Counter`), a Symbol
/// kind (`#'new-class'`), or a Dictionary (`#{ #file => "path" }`) — unlike
/// [`remove_method_expr_for`], there is no leading-`#` stripping here since
/// a `#`-prefixed selector is a legitimate Symbol-kind argument, not a
/// Symbol-literal-decorated method name.
///
/// Returns `None` when `selector` is empty (including a selector that is
/// only whitespace, once trimmed), so the caller can print a usage hint
/// instead of evaluating a malformed bare `Workspace flush: ` send — this
/// emptiness check is exactly the kind of guard a hand-copied test-harness
/// mirror can silently drop (as `remove_method_expr_for`'s did for its own
/// `#`-stripping order in BT-3189); routing both callers through this one
/// function makes that class of drift structurally impossible.
pub fn flush_expr_for(selector: &str) -> Option<String> {
    let selector = selector.trim();
    if selector.is_empty() {
        return None;
    }
    Some(format!("Workspace flush: {selector}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translates_to_remove_selector_send() {
        assert_eq!(
            remove_method_expr_for("Counter increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn preserves_keyword_selectors() {
        assert_eq!(
            remove_method_expr_for("Dict at:put:"),
            Some("Dict removeSelector: #at:put:".to_string())
        );
    }

    #[test]
    fn strips_leading_hash_on_selector() {
        assert_eq!(
            remove_method_expr_for("Counter #increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn trims_surrounding_whitespace() {
        assert_eq!(
            remove_method_expr_for("  Counter   increment  "),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn with_missing_selector_reports_no_expression() {
        assert_eq!(remove_method_expr_for("Counter"), None);
        assert_eq!(remove_method_expr_for("Counter "), None);
    }

    #[test]
    fn with_empty_argument_reports_no_expression() {
        assert_eq!(remove_method_expr_for(""), None);
        assert_eq!(remove_method_expr_for("   "), None);
    }

    #[test]
    fn with_bare_hash_selector_reports_no_expression() {
        // A selector of just "#" strips down to empty and must hit the
        // usage-error path, not a malformed `Counter removeSelector: #` eval.
        assert_eq!(remove_method_expr_for("Counter #"), None);
    }

    #[test]
    fn with_only_whitespace_after_hash_reports_no_expression() {
        // "#" followed by only whitespace must re-trim to empty and hit the
        // usage-error path, not just the bare-hash case above.
        assert_eq!(remove_method_expr_for("Counter #  "), None);
    }

    #[test]
    fn strips_whitespace_between_hash_and_selector() {
        // Whitespace right after "#" (before real selector content) must be
        // trimmed away, producing a clean expression instead of the
        // malformed `Counter removeSelector: #  increment`.
        assert_eq!(
            remove_method_expr_for("Counter #  increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    // -----------------------------------------------------------------------
    // flush_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn flush_class_selector_translates_to_workspace_flush_message() {
        assert_eq!(
            flush_expr_for("Counter"),
            Some("Workspace flush: Counter".to_string())
        );
    }

    #[test]
    fn flush_symbol_kind_selector_translates_verbatim() {
        // A leading "#" here is a legitimate Symbol-kind argument (not a
        // decorated method name like `remove_method_expr_for`'s selector),
        // so it must survive untouched.
        assert_eq!(
            flush_expr_for("#'new-class'"),
            Some("Workspace flush: #'new-class'".to_string())
        );
    }

    #[test]
    fn flush_dictionary_selector_translates_verbatim() {
        assert_eq!(
            flush_expr_for("#{ #file => \"src/foo.bt\" }"),
            Some("Workspace flush: #{ #file => \"src/foo.bt\" }".to_string())
        );
    }

    #[test]
    fn flush_trims_surrounding_whitespace() {
        assert_eq!(
            flush_expr_for("  Counter  "),
            Some("Workspace flush: Counter".to_string())
        );
    }

    #[test]
    fn flush_with_empty_selector_reports_no_expression() {
        assert_eq!(flush_expr_for(""), None);
    }

    #[test]
    fn flush_with_whitespace_only_selector_reports_no_expression() {
        // The historical-bug-class risk this guards against: a hand-copied
        // test-harness mirror of `Workspace flush: {selector}` that skips
        // this emptiness check (as the old `tests/repl_protocol.rs` inline
        // duplicate did) would blindly eval a malformed bare
        // `Workspace flush: ` send instead of hitting the usage-error path.
        // Because both the real dispatch and the test harness now call this
        // one function, that drift can no longer happen.
        assert_eq!(flush_expr_for("   "), None);
    }
}
