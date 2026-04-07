// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native actor validators (ADR 0056).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators for native actor classes:
//! - State field rejection for native actors (BT-1207)
//! - Delegate return type warnings (BT-1207)

use crate::ast::{MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

// ── BT-1207: Native actor validation (ADR 0056) ──────────────────────────────

/// BT-1207: Error if a `native:` class declares `state:` fields.
///
/// Native actors delegate to a backing Erlang `gen_server` — state is owned by
/// that Erlang module, not by the Beamtalk class. Declaring `state:` fields
/// would create an impossible situation where the actor has Beamtalk-managed
/// state alongside Erlang-managed state.
pub(crate) fn check_native_state_fields(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        if class.backing_module.is_some() && !class.state.is_empty() {
            let class_name = &class.name.name;
            let module_name = class
                .backing_module
                .as_ref()
                .map_or("?", |id| id.name.as_str());
            for field in &class.state {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Native actor `{class_name}` cannot declare state fields — \
                             state is owned by the backing gen_server `{module_name}`"
                        ),
                        field.name.span,
                    )
                    .with_hint(
                        "Remove the state: declaration. Manage state in the Erlang module instead."
                            .to_string(),
                    ),
                );
            }
        }
    }
}

/// BT-1207: Warn if a `self delegate` method has no return type annotation.
///
/// `self delegate` methods forward to Erlang — the compiler cannot infer the
/// return type. Without an annotation, callers have no type information and
/// the type checker cannot validate usage sites.
pub(crate) fn check_native_delegate_return_type(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Check inline class methods.
    for class in &module.classes {
        // Only check classes that have a backing module (native: classes).
        // On non-native classes, `self delegate` is a regular message send to
        // Actor.delegate (which raises at runtime), so no special warning needed.
        if class.backing_module.is_none() {
            continue;
        }

        for method in &class.methods {
            if method.is_self_delegate() && method.return_type.is_none() {
                emit_delegate_return_type_warning(&method.selector, method.span, diagnostics);
            }
        }
    }

    // Check standalone (Tonel-style) method definitions: `MyActor >> selector => self delegate`.
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if hierarchy.is_native(class_name)
            && standalone.method.is_self_delegate()
            && standalone.method.return_type.is_none()
        {
            emit_delegate_return_type_warning(
                &standalone.method.selector,
                standalone.method.span,
                diagnostics,
            );
        }
    }
}

/// Emit the warning diagnostic for a `self delegate` method missing a return type.
fn emit_delegate_return_type_warning(
    selector: &MessageSelector,
    span: crate::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let selector_name = selector.name();
    diagnostics.push(
        Diagnostic::warning(
            format!(
                "Method `{selector_name}` uses `self delegate` without a return type annotation"
            ),
            span,
        )
        .with_hint("Add a return type: `selector -> ReturnType => self delegate`".to_string())
        .with_category(DiagnosticCategory::Type),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    // ── BT-1207: Native actor validation tests ────────────────────────────────

    /// Native actor with state: field → compile error.
    #[test]
    fn native_actor_state_field_is_error() {
        let src = "Actor subclass: MyActor native: my_mod\n  state: count = 0\n  increment => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for state: on native actor, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0]
                .message
                .contains("cannot declare state fields"),
            "Expected 'cannot declare state fields' in message, got: {}",
            diagnostics[0].message
        );
        assert!(diagnostics[0].message.contains("my_mod"));
    }

    /// Native actor with multiple state: fields → one error per field.
    #[test]
    fn native_actor_multiple_state_fields_multiple_errors() {
        let src = "Actor subclass: MyActor native: my_mod\n  state: x = 0\n  state: y = 0\n  get => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 errors (one per state field), got: {diagnostics:?}"
        );
    }

    /// Non-native actor with state: fields → no error.
    #[test]
    fn non_native_actor_state_fields_ok() {
        let src = "Actor subclass: Counter\n  state: count = 0\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for non-native actor, got: {diagnostics:?}"
        );
    }

    /// classState: is allowed on native actors — no error.
    #[test]
    fn native_actor_class_state_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  classState: instances = 0\n  get => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "classState: should be allowed on native actors, got: {diagnostics:?}"
        );
    }

    /// self delegate method without return type → warning on native actor.
    #[test]
    fn native_delegate_missing_return_type_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for missing return type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(diagnostics[0].message.contains("return type annotation"));
    }

    /// self delegate method with return type → no warning.
    #[test]
    fn native_delegate_with_return_type_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  getValue -> Integer => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning when return type is present, got: {diagnostics:?}"
        );
    }

    /// self delegate on non-native class → no warning (it's a regular message send).
    #[test]
    fn non_native_delegate_no_warning() {
        let src = "Actor subclass: Counter\n  state: count = 0\n  getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning for non-native class, got: {diagnostics:?}"
        );
    }

    /// Non-delegate method body on native class → no warning.
    #[test]
    fn native_non_delegate_method_no_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  version => 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning for non-delegate method, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) self delegate without return type on native class → warning.
    #[test]
    fn native_standalone_delegate_missing_return_type_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  get -> Integer => self delegate\nMyActor >> getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for standalone method missing return type, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("getValue"));
    }

    /// Standalone self delegate with return type on native class → no warning.
    #[test]
    fn native_standalone_delegate_with_return_type_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  get -> Integer => self delegate\nMyActor >> getValue -> Integer => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning when standalone method has return type, got: {diagnostics:?}"
        );
    }
}
