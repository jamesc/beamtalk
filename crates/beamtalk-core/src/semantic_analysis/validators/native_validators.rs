// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native class validators (ADR 0056, ADR 0101).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators for `native:` classes:
//! - State field rejection for native actors (BT-1207)
//! - Delegate return type warnings (BT-1207)
//! - Reserved-word backing-function check for native Objects (BT-2720)

use crate::ast::{MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

// ── BT-1207: Native actor validation (ADR 0056) ──────────────────────────────

/// The complete set of Erlang reserved words (BT-2720, ADR 0101).
///
/// A `native:` Object's `self delegate` method lowers to a direct Erlang
/// function call `BackingModule:fn(...)`, where `fn` is the first keyword of the
/// selector. If that name is an Erlang reserved word the backing module cannot
/// define a function with it, so codegen would emit a call no `.erl` could
/// satisfy. We reject it at compile time instead.
const ERLANG_RESERVED_WORDS: &[&str] = &[
    "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case",
    "catch", "cond", "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "receive",
    "rem", "try", "when", "xor",
];

/// BT-1207: Error if a `native:` *actor* declares `state:` fields.
///
/// Native actors delegate to a backing Erlang `gen_server` — state is owned by
/// that Erlang module, not by the Beamtalk class. Declaring `state:` fields
/// would create an impossible situation where the actor has Beamtalk-managed
/// state alongside Erlang-managed state.
///
/// ADR 0101 / BT-2720: this rule is actor-only. A `native:` *Object* is a
/// stateless value whose representation is the backing Erlang term, so the
/// state-field rule does not apply to it.
pub(crate) fn check_native_state_fields(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        if class.backing_module.is_some()
            && !class.state.is_empty()
            && hierarchy.is_actor_subclass(class.name.name.as_str())
        {
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

/// BT-2720 (ADR 0101): Error if a `native:` Object's `self delegate` method
/// would lower to a backing function whose name is an Erlang reserved word.
///
/// Instance- and class-side `self delegate` on a `native:` Object lower to a
/// direct Erlang call `BackingModule:fn(...)` where `fn` is the first keyword of
/// the selector (`receive: x` → `receive`). An Erlang module cannot define a
/// function named after a reserved word, so the generated call would be
/// unsatisfiable — reject it here rather than emit invalid output.
///
/// Native *Actors* are exempt: their `self delegate` lowers to
/// `sync_send(Pid, 'receive', [])`, where the selector is a quoted atom that is
/// always valid.
pub(crate) fn check_native_delegate_reserved_word(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let is_native_object = |class_name: &str| {
        hierarchy.is_native(class_name) && !hierarchy.is_actor_subclass(class_name)
    };

    // Inline class methods (instance- and class-side).
    for class in &module.classes {
        if class.backing_module.is_none() || !is_native_object(class.name.name.as_str()) {
            continue;
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if method.is_self_delegate() {
                check_selector_reserved_word(&method.selector, method.span, diagnostics);
            }
        }
    }

    // Standalone (Tonel-style) method definitions: `MyClass >> sel => self delegate`.
    for standalone in &module.method_definitions {
        let class_name = standalone.class_name.name.as_str();
        if is_native_object(class_name) && standalone.method.is_self_delegate() {
            check_selector_reserved_word(
                &standalone.method.selector,
                standalone.method.span,
                diagnostics,
            );
        }
    }
}

/// Emit a reserved-word error if the backing function name (first keyword, colon
/// stripped, or the bare unary/binary selector) collides with an Erlang keyword.
fn check_selector_reserved_word(
    selector: &MessageSelector,
    span: crate::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let fn_name = match selector {
        MessageSelector::Unary(name) | MessageSelector::Binary(name) => name.as_str(),
        MessageSelector::Keyword(parts) => parts[0].keyword.trim_end_matches(':'),
    };
    if ERLANG_RESERVED_WORDS.contains(&fn_name) {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "`self delegate` method `{}` lowers to the Erlang function \
                     `{fn_name}`, which is a reserved word",
                    selector.name()
                ),
                span,
            )
            .with_hint(format!(
                "Rename the method so its first keyword is not the Erlang reserved \
                 word `{fn_name}`, or implement it with an explicit `(Erlang …)` \
                 call to a differently-named backing function."
            )),
        );
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &hierarchy, &mut diagnostics);
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &hierarchy, &mut diagnostics);
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &hierarchy, &mut diagnostics);
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &hierarchy, &mut diagnostics);
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

    // ── BT-2720: native: Object validation (ADR 0101) ─────────────────────────

    /// ADR 0101: a `native:` *Object* with a `state:` field is NOT an error —
    /// the state-field rule is actor-only.
    #[test]
    fn native_object_state_field_no_error() {
        let src = "Object subclass: MyObj native: my_mod\n  state: count = 0\n  get -> Integer => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no state-field error on a native Object, got: {diagnostics:?}"
        );
    }

    /// ADR 0101 / BT-2720: a `self delegate` method on a `native:` Object whose
    /// first keyword is an Erlang reserved word → compile error.
    #[test]
    fn native_object_reserved_word_first_keyword_is_error() {
        let src = "Object subclass: MyObj native: my_mod\n  receive: x :: Object -> Object => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_reserved_word(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 reserved-word error, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("receive"));
    }

    /// ADR 0101: a non-reserved first keyword on a `native:` Object → no error.
    #[test]
    fn native_object_non_reserved_first_keyword_ok() {
        let src = "Object subclass: MyObj native: my_mod\n  select: x :: Object -> Object => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_reserved_word(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for non-reserved keyword, got: {diagnostics:?}"
        );
    }

    /// ADR 0101: a native *Actor* with a reserved-word `self delegate` selector
    /// is exempt — its lowering uses a quoted selector atom, not a function name.
    #[test]
    fn native_actor_reserved_word_exempt() {
        let src = "Actor subclass: MyActor native: my_mod\n  receive: x :: Object -> Object => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_reserved_word(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected native Actors to be exempt from the reserved-word check, got: {diagnostics:?}"
        );
    }
}
