// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Visibility validation checks (ADR 0071 Phase 3, BT-1702).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators for method-level `internal` visibility enforcement:
//! - E0402: Internal method implementing a public protocol requirement (leaked visibility)
//! - W0401: Subclass method shadowing an internal superclass method

use crate::ast::Module;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

/// E0402 (BT-1702): Emit error when an internal method satisfies a public protocol.
///
/// If a class implements a selector required by a public protocol but declares
/// the method `internal`, that's a leaked visibility error — the protocol
/// promises the method is part of the public API, but the method is hidden.
///
/// An internal method satisfying an *internal* protocol is fine (both are
/// package-scoped).
pub fn check_leaked_method_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    current_package: Option<&str>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Only enforce when a package context is available
    let Some(_pkg) = current_package else {
        return;
    };

    for class in &module.classes {
        let class_name = &class.name.name;

        // Check each protocol the class might conform to
        for (proto_name, proto_info) in protocol_registry.protocols() {
            // Skip internal protocols — internal method implementing internal protocol is fine
            // (Protocols don't have is_internal yet, so all protocols are public for now)
            let _ = proto_name;

            // Check instance methods
            for req in &proto_info.methods {
                check_method_leaked_visibility(
                    class,
                    class_name,
                    &req.selector,
                    false,
                    proto_info,
                    hierarchy,
                    diagnostics,
                );
            }

            // Check class methods
            for req in &proto_info.class_methods {
                check_method_leaked_visibility(
                    class,
                    class_name,
                    &req.selector,
                    true,
                    proto_info,
                    hierarchy,
                    diagnostics,
                );
            }
        }
    }
}

/// Helper: check a single selector for leaked visibility.
fn check_method_leaked_visibility(
    class: &crate::ast::ClassDefinition,
    class_name: &EcoString,
    selector: &EcoString,
    is_class_side: bool,
    proto_info: &crate::semantic_analysis::protocol_registry::ProtocolInfo,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Check if this class actually has the selector (locally defined and internal)
    let methods = if is_class_side {
        &class.class_methods
    } else {
        &class.methods
    };

    for method in methods {
        if method.selector.name() == *selector && method.is_internal {
            // The class declares this method as internal, but a public protocol requires it.
            // Verify the class actually conforms to the protocol (has all required methods).
            let conforms = proto_info
                .methods
                .iter()
                .all(|req| hierarchy.resolves_selector(class_name, &req.selector))
                && proto_info
                    .class_methods
                    .iter()
                    .all(|req| hierarchy.resolves_class_selector(class_name, &req.selector));

            if conforms {
                let proto_name = &proto_info.name;
                let message: EcoString = format!(
                    "error[E0402]: Internal method '{selector}' on '{class_name}' satisfies public protocol '{proto_name}' — make the method public"
                ).into();
                diagnostics.push(
                    Diagnostic::error(message, method.span)
                        .with_hint(format!(
                            "'{selector}' is required by protocol '{proto_name}' but declared 'internal'"
                        ))
                        .with_category(DiagnosticCategory::Visibility),
                );
            }
        }
    }
}

/// W0401 (BT-1702): Warn when a subclass defines a selector that shadows an
/// internal method on the superclass.
///
/// This is a potential footgun — the subclass author may not know the superclass
/// has an internal helper with that name, leading to accidental shadowing.
pub fn check_internal_method_shadow(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = &class.name.name;

        // Get the superclass name
        let Some(superclass) = class.superclass.as_ref() else {
            continue;
        };
        let superclass_name = &superclass.name;

        // Skip if the superclass is not in the hierarchy
        if !hierarchy.has_class(superclass_name) {
            continue;
        }

        // Check each instance method
        for method in &class.methods {
            check_shadow_for_method(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        }

        // Check each class method
        for method in &class.class_methods {
            check_shadow_for_method_class_side(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        }
    }
}

/// Helper: check if an instance method shadows a superclass internal method.
fn check_shadow_for_method(
    class_name: &EcoString,
    superclass_name: &EcoString,
    selector: &str,
    span: crate::source_analysis::Span,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Look up the method on the superclass (not on our class — we want the inherited version)
    let Some(super_method) = hierarchy.find_method(superclass_name, selector) else {
        return;
    };

    if !super_method.is_internal {
        return;
    }

    let defined_in = &super_method.defined_in;
    let message: EcoString = format!(
        "warning[W0401]: Method '{selector}' on '{class_name}' shadows internal method from '{defined_in}'"
    )
    .into();
    diagnostics.push(
        Diagnostic::warning(message, span)
            .with_hint(format!(
                "'{defined_in}' has an internal method '{selector}' — shadowing may cause unexpected behavior"
            ))
            .with_category(DiagnosticCategory::Visibility),
    );
}

/// Helper: check if a class method shadows a superclass internal class method.
fn check_shadow_for_method_class_side(
    class_name: &EcoString,
    superclass_name: &EcoString,
    selector: &str,
    span: crate::source_analysis::Span,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(super_method) = hierarchy.find_class_method(superclass_name, selector) else {
        return;
    };

    if !super_method.is_internal {
        return;
    }

    let defined_in = &super_method.defined_in;
    let message: EcoString = format!(
        "warning[W0401]: Class method '{selector}' on '{class_name}' shadows internal class method from '{defined_in}'"
    )
    .into();
    diagnostics.push(
        Diagnostic::warning(message, span)
            .with_hint(format!(
                "'{defined_in}' has an internal class method '{selector}' — shadowing may cause unexpected behavior"
            ))
            .with_category(DiagnosticCategory::Visibility),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassDefinition, CommentAttachment, ExpressionStatement, Identifier, MessageSelector,
        MethodDefinition, MethodKind, Module,
    };
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::source_analysis::Span;
    use std::collections::HashMap;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn make_method(selector_name: &str, is_internal: bool) -> MethodDefinition {
        MethodDefinition {
            selector: MessageSelector::Unary(selector_name.into()),
            parameters: vec![],
            body: vec![ExpressionStatement::bare(crate::ast::Expression::Literal(
                crate::ast::Literal::Integer(42),
                span(),
            ))],
            return_type: None,
            is_sealed: false,
            is_internal,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    fn make_class_with_methods(
        name: &str,
        superclass: &str,
        methods: Vec<MethodDefinition>,
    ) -> ClassDefinition {
        ClassDefinition::with_modifiers(
            ident(name),
            Some(ident(superclass)),
            false,
            false,
            vec![],
            methods,
            span(),
        )
    }

    fn make_module_with_classes(classes: Vec<ClassDefinition>) -> Module {
        let mut module = Module::new(vec![], span());
        module.classes = classes;
        module
    }

    // ── W0401: Shadow warning tests ──

    #[test]
    fn w0401_shadow_internal_superclass_method() {
        // Parent has internal method `helper`, Child redefines `helper`
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            name: "Parent".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: Some("lib".into()),
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: "helper".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "Parent".into(),
                is_sealed: false,
                is_internal: true,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("helper", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        let w0401: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("W0401"))
            .collect();
        assert_eq!(
            w0401.len(),
            1,
            "Expected W0401 shadow warning, got: {w0401:?}"
        );
        assert!(w0401[0].message.contains("helper"));
        assert!(w0401[0].message.contains("Parent"));
    }

    #[test]
    fn w0401_no_warning_for_public_superclass_method() {
        // Parent has a public method `helper` — Child overriding it is fine (no warning)
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            name: "Parent".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: Some("lib".into()),
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: "helper".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "Parent".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("helper", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        let w0401: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("W0401"))
            .collect();
        assert!(
            w0401.is_empty(),
            "No W0401 for public method override, got: {w0401:?}"
        );
    }

    #[test]
    fn w0401_no_warning_for_new_selector() {
        // Child defines a new method that doesn't exist on superclass — no warning
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            name: "Parent".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: Some("lib".into()),
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("newMethod", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "New method should not trigger W0401, got: {diagnostics:?}"
        );
    }

    // ── E0402: Leaked visibility tests ──

    #[test]
    fn e0402_internal_method_satisfying_public_protocol() {
        // Register the class in the hierarchy (needed for resolves_selector)
        let module = make_module_with_classes(vec![make_class_with_methods(
            "MyPrinter",
            "Object",
            vec![make_method("asString", true)], // internal!
        )]);
        let (h, _) = ClassHierarchy::build(&module);
        let hierarchy = h.unwrap();

        // Create a protocol registry with a Printable protocol requiring `asString`
        let mut registry = ProtocolRegistry::new();
        // Manually insert a protocol (the registry doesn't have a public insert method,
        // so we use register_module with a module containing a protocol definition)
        let proto_module = {
            use crate::ast::{ProtocolDefinition, ProtocolMethodSignature};
            let mut m = Module::new(vec![], span());
            m.protocols = vec![ProtocolDefinition {
                name: ident("Printable"),
                type_params: vec![],
                extending: None,
                method_signatures: vec![ProtocolMethodSignature {
                    selector: MessageSelector::Unary("asString".into()),
                    parameters: vec![],
                    return_type: None,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: span(),
                }],
                class_method_signatures: vec![],
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }];
            m
        };
        let _reg_diags = registry.register_module(&proto_module, &hierarchy);

        let mut diagnostics = Vec::new();
        check_leaked_method_visibility(
            &module,
            &hierarchy,
            &registry,
            Some("my_pkg"),
            &mut diagnostics,
        );

        let e0402: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("E0402"))
            .collect();
        assert_eq!(
            e0402.len(),
            1,
            "Expected E0402 for internal method satisfying public protocol, got: {e0402:?}"
        );
        assert!(e0402[0].message.contains("asString"));
        assert!(e0402[0].message.contains("Printable"));
    }

    #[test]
    fn e0402_public_method_satisfying_protocol_is_fine() {
        let module = make_module_with_classes(vec![make_class_with_methods(
            "MyPrinter",
            "Object",
            vec![make_method("asString", false)], // public
        )]);
        let (h, _) = ClassHierarchy::build(&module);
        let hierarchy = h.unwrap();

        let mut registry = ProtocolRegistry::new();
        let proto_module = {
            use crate::ast::{ProtocolDefinition, ProtocolMethodSignature};
            let mut m = Module::new(vec![], span());
            m.protocols = vec![ProtocolDefinition {
                name: ident("Printable"),
                type_params: vec![],
                extending: None,
                method_signatures: vec![ProtocolMethodSignature {
                    selector: MessageSelector::Unary("asString".into()),
                    parameters: vec![],
                    return_type: None,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: span(),
                }],
                class_method_signatures: vec![],
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }];
            m
        };
        let _reg_diags = registry.register_module(&proto_module, &hierarchy);

        let mut diagnostics = Vec::new();
        check_leaked_method_visibility(
            &module,
            &hierarchy,
            &registry,
            Some("my_pkg"),
            &mut diagnostics,
        );

        let e0402: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("E0402"))
            .collect();
        assert!(
            e0402.is_empty(),
            "Public method should not trigger E0402, got: {e0402:?}"
        );
    }
}
