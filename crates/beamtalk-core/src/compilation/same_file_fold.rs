// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Same-file extension fold (BT-4721).
//!
//! **DDD Context:** Compilation
//!
//! A file is one compilation unit for the class it declares. A standalone
//! `X >> m => ...` / `X class >> m => ...` in the file that declares `X` is
//! therefore part of `X`: this pass moves it into the class body
//! (`ClassDefinition::methods` / `class_methods`) before semantic analysis and
//! codegen, so a folded method behaves exactly like a body method (dispatch
//! arm, static/direct calls, type checking, reflection).
//!
//! This is the **only** place same-file extensions are resolved. The build
//! pipeline and the REPL compiler port both call [`fold_same_file_extensions`];
//! nothing registers a same-file extension through `beamtalk_extensions`.
//!
//! * A selector defined both in the class body and by a same-file extension is
//!   a duplicate method definition. The fold appends without replacing, so the
//!   existing class-body duplicate check reports it — the same diagnostic as
//!   defining the selector twice in the body.
//! * An instance-side extension on a `native:` actor is a compile error: its
//!   instance sends go to the backing `gen_server`. Class-side extensions fold
//!   into the facade like any class method.
//! * Extensions whose target class is declared elsewhere (cross-file) are left
//!   in `module.method_definitions` unchanged.

use crate::ast::{ClassDefinition, Module};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Whether `class` is a `native:` actor, judged syntactically (direct
/// `Actor subclass: X native: m`). Indirect native actors are caught by the
/// native-actor method-body validator instead, since the hierarchy is not
/// available before the fold.
fn is_native_actor_decl(class: &ClassDefinition) -> bool {
    class.backing_module.is_some() && class.superclass_name() == "Actor"
}

/// Folds every same-file extension in `module` into its host class.
///
/// Returns the diagnostics the fold itself produces (instance-side extensions
/// on `native:` actors). Duplicate selectors are left for the class-body
/// duplicate check. Idempotent: a folded module has no same-file extensions
/// left.
pub fn fold_same_file_extensions(module: &mut Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    if module.method_definitions.is_empty() {
        return diagnostics;
    }
    let defs = std::mem::take(&mut module.method_definitions);
    for def in defs {
        let Some(class) = module
            .classes
            .iter_mut()
            .find(|c| c.name.name == def.class_name.name)
        else {
            module.method_definitions.push(def);
            continue;
        };
        if !def.is_class_method && is_native_actor_decl(class) {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Instance-side extension `{}` on native actor `{}` declared in the same file",
                        def.method.selector.name(),
                        class.name.name
                    ),
                    def.method.head_span(),
                )
                .with_hint(
                    "A native actor's instance sends go to its backing gen_server, so a Beamtalk \
                     body would never run. Move the logic into a class method."
                        .to_string(),
                )
                .with_category(DiagnosticCategory::Type),
            );
            continue;
        }
        if def.is_class_method {
            class.class_methods.push(def.method);
        } else {
            class.methods.push(def.method);
        }
    }
    diagnostics
}

/// The names of the classes declared in `module`. A standalone method
/// definition whose target is in this set is a same-file extension; this is the
/// one place that rule lives (codegen and analysis both use it).
#[must_use]
pub fn declared_class_names(module: &Module) -> std::collections::HashSet<ecow::EcoString> {
    module.classes.iter().map(|c| c.name.name.clone()).collect()
}

/// Whether a standalone method definition targeting `class_name` is a
/// same-file extension (its class is declared in `module`).
#[must_use]
pub fn is_same_file_extension(module: &Module, class_name: &str) -> bool {
    declared_class_names(module).contains(class_name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_module(src: &str) -> Module {
        let (module, diags) = parse(lex_with_eof(src));
        assert!(diags.is_empty(), "parse diagnostics: {diags:?}");
        module
    }

    #[test]
    fn folds_instance_and_class_side() {
        let mut m = parse_module(
            "Object subclass: Foo\n  foo => 1\n\nFoo >> bar => 2\nFoo class >> make => 3\n",
        );
        assert_eq!(m.method_definitions.len(), 2);
        let diags = fold_same_file_extensions(&mut m);
        assert!(diags.is_empty());
        assert!(m.method_definitions.is_empty());
        assert_eq!(m.classes[0].methods.len(), 2);
        assert_eq!(m.classes[0].class_methods.len(), 1);
    }

    #[test]
    fn leaves_foreign_extensions() {
        let mut m = parse_module("Object subclass: Foo\n  foo => 1\n\nString >> shout => 2\n");
        let diags = fold_same_file_extensions(&mut m);
        assert!(diags.is_empty());
        assert_eq!(m.method_definitions.len(), 1);
        assert_eq!(m.classes[0].methods.len(), 1);
    }

    #[test]
    fn duplicate_selector_is_appended_not_replaced() {
        let mut m = parse_module("Object subclass: Foo\n  foo => 1\n\nFoo >> foo => 2\n");
        fold_same_file_extensions(&mut m);
        assert_eq!(m.classes[0].methods.len(), 2);
    }

    #[test]
    fn instance_extension_on_native_actor_is_an_error() {
        let mut m = parse_module(
            "Actor subclass: Svc native: svc_backing\n  ping => self delegate\n\nSvc >> extra => 1\n",
        );
        let diags = fold_same_file_extensions(&mut m);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("native actor"));
    }

    #[test]
    fn class_extension_on_native_actor_folds() {
        let mut m = parse_module(
            "Actor subclass: Svc native: svc_backing\n  ping => self delegate\n\nSvc class >> make => 1\n",
        );
        let diags = fold_same_file_extensions(&mut m);
        assert!(diags.is_empty(), "{diags:?}");
        assert_eq!(m.classes[0].class_methods.len(), 1);
    }
}
