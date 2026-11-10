// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor secret declarations: `class secretState` and `class secretSelectors`
//! (BT-4567).
//!
//! **DDD Context:** Semantic Analysis
//!
//! An actor declares which state slots and which messages are secret with two
//! class-side methods whose bodies are literal symbol lists:
//!
//! ```beamtalk
//! Actor subclass: SessionStore
//!   state: verifier :: String | Nil = nil
//!   class secretState -> List(Symbol) => #(#verifier)
//!   class secretSelectors -> List(Symbol) => #(#create:boundTo:)
//! ```
//!
//! The compiler reads the lists here, at compile time, and codegen bakes them
//! into the actor module's `'__beamtalk_secrets'/0`. The generated
//! `format_status/1` redacts from that compiled metadata, so no user code runs
//! on the status or crash-report path.
//!
//! This module is the single reader of the declarations: the validator
//! ([`check_actor_secret_declarations`]) and codegen both go through
//! [`declared_secrets`].

use crate::ast::{ClassDefinition, Expression, Literal, MethodDefinition, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;

/// Class-side selector naming the secret state slots.
pub const SECRET_STATE_SELECTOR: &str = "secretState";

/// Class-side selector naming the secret message selectors.
pub const SECRET_SELECTORS_SELECTOR: &str = "secretSelectors";

/// One name in a secret declaration, with its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecretName {
    /// The symbol's text (without the leading `#`).
    pub name: EcoString,
    /// Where the symbol appears.
    pub span: Span,
}

/// What a class declares for one of the two secret lists.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum SecretList {
    /// The class does not define the method itself (it inherits).
    #[default]
    Undeclared,
    /// The method's body is a literal list of symbols.
    Literal(Vec<SecretName>),
    /// The method exists but its body is not a literal list of symbols, so the
    /// compiler cannot read it. The span is the method's.
    NotLiteral(Span),
}

impl SecretList {
    /// The declared names, or none when undeclared or unreadable.
    #[must_use]
    pub fn names(&self) -> &[SecretName] {
        match self {
            Self::Literal(names) => names,
            Self::Undeclared | Self::NotLiteral(_) => &[],
        }
    }
}

/// A class's own secret declarations (not including its superclasses').
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ActorSecrets {
    /// `class secretState`.
    pub state: SecretList,
    /// `class secretSelectors`.
    pub selectors: SecretList,
}

/// Reads `class`'s own `secretState` / `secretSelectors` declarations, from
/// its inline class-side methods or from standalone `Class class >> ...`
/// definitions in `module`.
#[must_use]
pub fn declared_secrets(module: &Module, class: &ClassDefinition) -> ActorSecrets {
    ActorSecrets {
        state: read_list(find_class_method(module, class, SECRET_STATE_SELECTOR)),
        selectors: read_list(find_class_method(module, class, SECRET_SELECTORS_SELECTOR)),
    }
}

fn find_class_method<'a>(
    module: &'a Module,
    class: &'a ClassDefinition,
    selector: &str,
) -> Option<&'a MethodDefinition> {
    class
        .class_methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .or_else(|| {
            module
                .method_definitions
                .iter()
                .find(|s| {
                    s.is_class_method
                        && s.class_name.name == class.name.name
                        && s.method.selector.name() == selector
                })
                .map(|s| &s.method)
        })
}

fn read_list(method: Option<&MethodDefinition>) -> SecretList {
    let Some(method) = method else {
        return SecretList::Undeclared;
    };
    let not_literal = SecretList::NotLiteral(method.span);
    let [stmt] = method.body.as_slice() else {
        return not_literal;
    };
    match &stmt.expression {
        Expression::ListLiteral {
            elements,
            tail: None,
            ..
        } => {
            let mut names = Vec::with_capacity(elements.len());
            for element in elements {
                let Expression::Literal(Literal::Symbol(name), span) = element else {
                    return not_literal;
                };
                names.push(SecretName {
                    name: name.clone(),
                    span: *span,
                });
            }
            SecretList::Literal(names)
        }
        Expression::Literal(Literal::List(items), span) => {
            let mut names = Vec::with_capacity(items.len());
            for item in items {
                let Literal::Symbol(name) = item else {
                    return not_literal;
                };
                names.push(SecretName {
                    name: name.clone(),
                    span: *span,
                });
            }
            SecretList::Literal(names)
        }
        _ => not_literal,
    }
}

/// Validates every actor class's secret declarations in `module`:
///
/// - a `secretState` / `secretSelectors` whose body is not a literal list of
///   symbols is an error, since the compiler reads it at compile time;
/// - a `secretState` name that is not a state slot of the class (own or
///   inherited) is a warning;
/// - a `secretSelectors` name the class does not understand is a warning.
///
/// The unknown-name warnings are skipped when the class's superclass chain is
/// not fully known (a cross-file parent, or a parse-incomplete surface).
pub(crate) fn check_actor_secret_declarations(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !hierarchy.is_actor_subclass(class_name) {
            continue;
        }
        let secrets = declared_secrets(module, class);
        for (list, selector) in [
            (&secrets.state, SECRET_STATE_SELECTOR),
            (&secrets.selectors, SECRET_SELECTORS_SELECTOR),
        ] {
            if let SecretList::NotLiteral(span) = list {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "`class {selector}` must answer a literal list of symbols; \
                             the compiler reads it at compile time"
                        ),
                        *span,
                    )
                    .with_hint(format!(
                        "Write it as `class {selector} => #(#name1, #name2)`"
                    )),
                );
            }
        }

        let chain_known = !hierarchy.has_cross_file_parent(class_name)
            && !hierarchy.has_incomplete_surface_in_chain(class_name);
        if !chain_known {
            continue;
        }
        let slots = hierarchy.all_state(class_name);
        for secret in secrets.state.names() {
            if !slots.contains(&secret.name) {
                diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "`secretState` names `{}`, which is not a state slot of `{class_name}`",
                            secret.name
                        ),
                        secret.span,
                    )
                    .with_hint("Name a slot declared with `state:` in this class or a superclass"),
                );
            }
        }
        for secret in secrets.selectors.names() {
            if !hierarchy.resolves_selector(class_name, &secret.name) {
                diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "`secretSelectors` names `#{}`, which `{class_name}` does not understand",
                            secret.name
                        ),
                        secret.span,
                    )
                    .with_hint("Name an instance-side method of this class or a superclass"),
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn check(src: &str) -> (Module, Vec<Diagnostic>) {
        let (module, parse_diags) = parse(lex_with_eof(src));
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_actor_secret_declarations(&module, &hierarchy, &mut diagnostics);
        (module, diagnostics)
    }

    fn names(list: &SecretList) -> Vec<&str> {
        list.names().iter().map(|n| n.name.as_str()).collect()
    }

    #[test]
    fn reads_inline_declarations() {
        let src = "Actor subclass: Store\n  state: verifier = nil\n  state: count = 0\n  \
                   class secretState -> List(Symbol) => #(#verifier)\n  \
                   class secretSelectors -> List(Symbol) => #(#create:boundTo:, #peek)\n  \
                   create: a boundTo: b => a\n  peek => self.verifier";
        let (module, diagnostics) = check(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let secrets = declared_secrets(&module, &module.classes[0]);
        assert_eq!(names(&secrets.state), ["verifier"]);
        assert_eq!(names(&secrets.selectors), ["create:boundTo:", "peek"]);
    }

    #[test]
    fn reads_standalone_declarations() {
        let src = "Actor subclass: Store\n  state: verifier = nil\n  go => nil\n\
                   Store class >> secretState => #(#verifier)";
        let (module, diagnostics) = check(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let secrets = declared_secrets(&module, &module.classes[0]);
        assert_eq!(names(&secrets.state), ["verifier"]);
        assert_eq!(secrets.selectors, SecretList::Undeclared);
    }

    #[test]
    fn empty_list_is_a_literal() {
        let src = "Actor subclass: Store\n  class secretState => #()\n  go => nil";
        let (module, diagnostics) = check(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let secrets = declared_secrets(&module, &module.classes[0]);
        assert_eq!(secrets.state, SecretList::Literal(vec![]));
    }

    #[test]
    fn inherited_slot_is_known() {
        let src = "Actor subclass: Base\n  state: token = nil\n  go => nil\n\
                   Base subclass: Leaf\n  class secretState => #(#token)\n  stop2 => nil";
        let (_, diagnostics) = check(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }

    #[test]
    fn unknown_slot_warns() {
        let src = "Actor subclass: Store\n  state: verifier = nil\n  \
                   class secretState => #(#verifer)\n  go => nil";
        let (_, diagnostics) = check(src);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(diagnostics[0].message.contains("verifer"));
    }

    #[test]
    fn unknown_selector_warns() {
        let src = "Actor subclass: Store\n  class secretSelectors => #(#nope:)\n  go => nil";
        let (_, diagnostics) = check(src);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(diagnostics[0].message.contains("#nope:"));
    }

    #[test]
    fn non_literal_body_is_an_error() {
        let src = "Actor subclass: Store\n  state: verifier = nil\n  \
                   class secretState => #(#verifier) reversed\n  go => nil";
        let (_, diagnostics) = check(src);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("literal list of symbols"));
    }

    #[test]
    fn non_symbol_element_is_an_error() {
        let src = "Actor subclass: Store\n  class secretSelectors => #(\"go\")\n  go => nil";
        let (_, diagnostics) = check(src);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn non_actor_classes_are_ignored() {
        let src = "Object subclass: Plain\n  class secretState => 42\n  go => nil";
        let (_, diagnostics) = check(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }
}
