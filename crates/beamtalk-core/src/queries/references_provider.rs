// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! References provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `ReferencesProvider` from the DDD model.
//! It finds all references to a symbol (class name or method selector) across
//! all indexed files.
//!
//! # Design
//!
//! References are collected by walking the AST of each file:
//! - **Class/protocol references**: All `Identifier` and `ClassReference` nodes matching the name,
//!   plus class definitions referencing it as a superclass, plus protocol definitions referencing
//!   it as an `extending:` target, plus `TypeParamDecl` protocol bounds on both class and protocol
//!   generic type parameters (BT-1936). Classes and protocols share a namespace and are collected
//!   through the same entry point.
//! - **Method selector references**: All `MessageSend` and `Cascade` nodes with matching selector,
//!   plus method definitions with matching selector
//!
//! # Performance
//!
//! Must respond in <500ms for typical project sizes (project-wide search).
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - ADR 0024: Static-First, Live-Augmented IDE Tooling
//! - LSP specification: Language Server Protocol textDocument/references

use crate::ast::{
    ClassDefinition, Expression, MethodDefinition, Module, ParameterDefinition, Pattern,
    ProtocolDefinition, ProtocolMethodSignature, StateDeclaration, TypeAnnotation, TypeParamDecl,
};
use crate::language_service::Location;
use crate::source_analysis::Span;
use camino::Utf8PathBuf;

/// Find all references to a class or protocol name across files.
///
/// Classes and protocols share a single namespace (BT-1933 registers protocol
/// names as synthetic class entries), so this function collects references for
/// both shapes through the same entry point.
///
/// Collects all locations where the name appears as:
/// - An identifier in expressions (variable references to class/protocol)
/// - A class reference expression
/// - A superclass in class definitions
/// - A class definition name
/// - A protocol definition name (BT-1936)
/// - An `extending:` target in another protocol (BT-1936)
/// - A protocol bound on a generic type parameter — e.g. `T :: Printable` —
///   on either a class or a protocol definition (BT-1936)
pub fn find_class_references<'a>(
    class_name: &str,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Vec<Location> {
    let mut results = Vec::new();

    for (file_path, module) in files {
        // Class definitions: name, superclass, and type-parameter bounds.
        for class in &module.classes {
            collect_class_definition_refs(class, class_name, file_path, &mut results);
        }

        // Protocol definitions: name, `extending:` target, and type-parameter bounds.
        for protocol in &module.protocols {
            collect_protocol_definition_refs(protocol, class_name, file_path, &mut results);
        }

        // Standalone method definitions: class name references and signature
        // type annotations (BT-1936: parity with class-method signatures).
        for smd in &module.method_definitions {
            if smd.class_name.name == class_name {
                results.push(Location::new(file_path.clone(), smd.class_name.span));
            }
            collect_method_signature_refs(&smd.method, class_name, file_path, &mut results);
        }

        // Expressions: identifiers and class references
        for stmt in &module.expressions {
            collect_class_refs(&stmt.expression, class_name, file_path, &mut results);
        }

        // Class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    collect_class_refs(&stmt.expression, class_name, file_path, &mut results);
                }
            }
        }

        // Standalone method bodies
        for smd in &module.method_definitions {
            for stmt in &smd.method.body {
                collect_class_refs(&stmt.expression, class_name, file_path, &mut results);
            }
        }
    }

    results
}

/// Emit class-definition references (definition name, superclass, type-parameter
/// bounds, state/class-variable type annotations, method signature type annotations).
///
/// Walks every site at which a class or protocol name can appear *textually* as
/// part of a class declaration. Keeps parity with `find_identifier_in_class` in
/// `language_service::mod.rs` so goto-definition and find-references see the
/// same set of sites.
fn collect_class_definition_refs(
    class: &ClassDefinition,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    if class.name.name == name {
        results.push(Location::new(file_path.clone(), class.name.span));
    }
    if let Some(ref superclass) = class.superclass {
        if superclass.name == name {
            results.push(Location::new(file_path.clone(), superclass.span));
        }
    }
    collect_type_param_bound_refs(&class.type_params, name, file_path, results);

    // BT-1936: state and class-variable type annotations — e.g. `state: x :: Printable = nil`.
    for state in class.state.iter().chain(class.class_variables.iter()) {
        collect_state_type_annotation_refs(state, name, file_path, results);
    }

    // Method signature type annotations (parameters and return type). The
    // method bodies themselves are walked separately by the caller.
    for method in class.methods.iter().chain(class.class_methods.iter()) {
        collect_method_signature_refs(method, name, file_path, results);
    }
}

/// Emit protocol-definition references (definition name, `extending:`,
/// type-parameter bounds, method signature type annotations).
fn collect_protocol_definition_refs(
    protocol: &ProtocolDefinition,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    if protocol.name.name == name {
        results.push(Location::new(file_path.clone(), protocol.name.span));
    }
    if let Some(ref extending) = protocol.extending {
        if extending.name == name {
            results.push(Location::new(file_path.clone(), extending.span));
        }
    }
    collect_type_param_bound_refs(&protocol.type_params, name, file_path, results);

    // BT-1936: protocol method signatures can mention protocol names in
    // parameter and return type annotations, e.g. `do: block :: Block(E, Object)`.
    for sig in protocol
        .method_signatures
        .iter()
        .chain(protocol.class_method_signatures.iter())
    {
        collect_protocol_method_signature_refs(sig, name, file_path, results);
    }
}

/// Emit protocol-bound references for a list of generic type parameters.
///
/// A bound like `T :: Printable` contributes the span of `Printable` when the
/// bound name matches.
fn collect_type_param_bound_refs(
    type_params: &[TypeParamDecl],
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    for param in type_params {
        if let Some(bound) = &param.bound {
            if bound.name == name {
                results.push(Location::new(file_path.clone(), bound.span));
            }
        }
    }
}

/// Emit references to `name` inside a `state:` or `class:` declaration's type
/// annotation (e.g. `state: x :: Printable = nil`).
fn collect_state_type_annotation_refs(
    state: &StateDeclaration,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    if let Some(type_annotation) = &state.type_annotation {
        collect_type_annotation_refs(type_annotation, name, file_path, results);
    }
}

/// Emit references to `name` inside a class method's signature (parameter type
/// annotations and return type).
fn collect_method_signature_refs(
    method: &MethodDefinition,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    for parameter in &method.parameters {
        collect_parameter_type_annotation_refs(parameter, name, file_path, results);
    }
    if let Some(return_type) = &method.return_type {
        collect_type_annotation_refs(return_type, name, file_path, results);
    }
}

/// Emit references to `name` inside a protocol method signature (parameter
/// type annotations and return type).
fn collect_protocol_method_signature_refs(
    sig: &ProtocolMethodSignature,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    for parameter in &sig.parameters {
        collect_parameter_type_annotation_refs(parameter, name, file_path, results);
    }
    if let Some(return_type) = &sig.return_type {
        collect_type_annotation_refs(return_type, name, file_path, results);
    }
}

/// Emit references to `name` inside a parameter's optional type annotation.
fn collect_parameter_type_annotation_refs(
    parameter: &ParameterDefinition,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    if let Some(type_annotation) = &parameter.type_annotation {
        collect_type_annotation_refs(type_annotation, name, file_path, results);
    }
}

/// Recursively collect every occurrence of `name` inside a type annotation.
///
/// Mirrors the variant coverage of
/// `SimpleLanguageService::find_identifier_in_type_annotation` but emits *all*
/// matches rather than finding one at a cursor offset.
fn collect_type_annotation_refs(
    annotation: &TypeAnnotation,
    name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match annotation {
        TypeAnnotation::Simple(identifier) => {
            if identifier.name == name {
                results.push(Location::new(file_path.clone(), identifier.span));
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for ty in types {
                collect_type_annotation_refs(ty, name, file_path, results);
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            if base.name == name {
                results.push(Location::new(file_path.clone(), base.span));
            }
            for ty in parameters {
                collect_type_annotation_refs(ty, name, file_path, results);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_type_annotation_refs(inner, name, file_path, results);
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

/// Recursively collect class name references in expressions.
fn collect_class_refs(
    expr: &Expression,
    class_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match expr {
        Expression::Identifier(ident) if ident.name == class_name => {
            results.push(Location::new(file_path.clone(), ident.span));
        }
        Expression::ClassReference { name, .. } if name.name == class_name => {
            results.push(Location::new(file_path.clone(), name.span));
        }
        Expression::Assignment { target, value, .. } => {
            collect_class_refs(target, class_name, file_path, results);
            collect_class_refs(value, class_name, file_path, results);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_class_refs(receiver, class_name, file_path, results);
            for arg in arguments {
                collect_class_refs(arg, class_name, file_path, results);
            }
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_class_refs(&stmt.expression, class_name, file_path, results);
            }
        }
        Expression::Return { value, .. } => {
            collect_class_refs(value, class_name, file_path, results);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_class_refs(expression, class_name, file_path, results);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_class_refs(receiver, class_name, file_path, results);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_class_refs(receiver, class_name, file_path, results);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_class_refs(arg, class_name, file_path, results);
                }
            }
        }
        Expression::Match { value, arms, .. } => {
            collect_class_refs(value, class_name, file_path, results);
            for arm in arms {
                // BT-1940: walk arm patterns so constructor pattern class names
                // (e.g. `Result` in `Result ok: v`) are reported as references.
                collect_pattern_class_refs(&arm.pattern, class_name, file_path, results);
                if let Some(guard) = &arm.guard {
                    collect_class_refs(guard, class_name, file_path, results);
                }
                collect_class_refs(&arm.body, class_name, file_path, results);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    collect_class_refs(expr, class_name, file_path, results);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_class_refs(element, class_name, file_path, results);
            }
            if let Some(tail_expr) = tail {
                collect_class_refs(tail_expr, class_name, file_path, results);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_class_refs(&pair.key, class_name, file_path, results);
                collect_class_refs(&pair.value, class_name, file_path, results);
            }
        }
        _ => {}
    }
}

/// Recursively collect class name references inside a destructuring pattern. (BT-1940)
///
/// Only `Pattern::Constructor` carries a class identifier (`Result` in
/// `Result ok: v`). All other variants just host nested patterns, which we walk
/// so a constructor nested inside a tuple, list, array, map, or binary pattern
/// is still reported. Mirrors `find_identifier_in_pattern` in
/// `crate::language_service` so goto-definition and find-references see the
/// same set of sites.
fn collect_pattern_class_refs(
    pattern: &Pattern,
    class_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match pattern {
        Pattern::Constructor {
            class, keywords, ..
        } => {
            if class.name == class_name {
                results.push(Location::new(file_path.clone(), class.span));
            }
            for (_, nested) in keywords {
                collect_pattern_class_refs(nested, class_name, file_path, results);
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_class_refs(element, class_name, file_path, results);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_class_refs(element, class_name, file_path, results);
            }
            if let Some(rest_pat) = rest {
                collect_pattern_class_refs(rest_pat, class_name, file_path, results);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_class_refs(element, class_name, file_path, results);
            }
            if let Some(tail_pat) = tail {
                collect_pattern_class_refs(tail_pat, class_name, file_path, results);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_class_refs(&pair.value, class_name, file_path, results);
            }
        }
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_class_refs(&segment.value, class_name, file_path, results);
            }
        }
        Pattern::Wildcard(_) | Pattern::Literal(_, _) | Pattern::Variable(_) => {}
    }
}

/// Find all references to a method selector across files.
///
/// Collects all locations where the selector appears as:
/// - A message send with matching selector
/// - A cascade message with matching selector
/// - A method definition with matching selector
pub fn find_selector_references<'a>(
    selector_name: &str,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Vec<Location> {
    let mut results = Vec::new();

    for (file_path, module) in files {
        // Method definitions in classes
        for class in &module.classes {
            collect_method_def_refs(&class.methods, selector_name, file_path, &mut results);
            collect_method_def_refs(&class.class_methods, selector_name, file_path, &mut results);
        }

        // Standalone method definitions
        for smd in &module.method_definitions {
            if smd.method.selector.name() == selector_name {
                results.push(Location::new(file_path.clone(), smd.method.span));
            }
        }

        // Message sends and cascades in expressions
        for stmt in &module.expressions {
            collect_selector_refs(&stmt.expression, selector_name, file_path, &mut results);
        }

        // Message sends and cascades in class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    collect_selector_refs(&stmt.expression, selector_name, file_path, &mut results);
                }
            }
        }

        // Message sends and cascades in standalone method bodies
        for smd in &module.method_definitions {
            for stmt in &smd.method.body {
                collect_selector_refs(&stmt.expression, selector_name, file_path, &mut results);
            }
        }
    }

    results
}

/// Collect method definition references with matching selector.
fn collect_method_def_refs(
    methods: &[crate::ast::MethodDefinition],
    selector_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    for method in methods {
        if method.selector.name() == selector_name {
            results.push(Location::new(file_path.clone(), method.span));
        }
    }
}

/// Recursively collect selector references in expressions.
fn collect_selector_refs(
    expr: &Expression,
    selector_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            if selector.name() == selector_name {
                // Use the span of the selector keywords if available, otherwise the send span
                let ref_span = selector_reference_span(selector).unwrap_or(*span);
                results.push(Location::new(file_path.clone(), ref_span));
            }
            collect_selector_refs(receiver, selector_name, file_path, results);
            for arg in arguments {
                collect_selector_refs(arg, selector_name, file_path, results);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_selector_refs(receiver, selector_name, file_path, results);
            for msg in messages {
                if msg.selector.name() == selector_name {
                    let ref_span = selector_reference_span(&msg.selector).unwrap_or(msg.span);
                    results.push(Location::new(file_path.clone(), ref_span));
                }
                for arg in &msg.arguments {
                    collect_selector_refs(arg, selector_name, file_path, results);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_selector_refs(target, selector_name, file_path, results);
            collect_selector_refs(value, selector_name, file_path, results);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_selector_refs(&stmt.expression, selector_name, file_path, results);
            }
        }
        Expression::Return { value, .. } => {
            collect_selector_refs(value, selector_name, file_path, results);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_selector_refs(expression, selector_name, file_path, results);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_selector_refs(receiver, selector_name, file_path, results);
        }
        Expression::Match { value, arms, .. } => {
            collect_selector_refs(value, selector_name, file_path, results);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_selector_refs(guard, selector_name, file_path, results);
                }
                collect_selector_refs(&arm.body, selector_name, file_path, results);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    collect_selector_refs(expr, selector_name, file_path, results);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_selector_refs(element, selector_name, file_path, results);
            }
            if let Some(tail_expr) = tail {
                collect_selector_refs(tail_expr, selector_name, file_path, results);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_selector_refs(&pair.key, selector_name, file_path, results);
                collect_selector_refs(&pair.value, selector_name, file_path, results);
            }
        }
        _ => {}
    }
}

/// Get the span covering a selector's keywords (for keyword messages)
/// or None for unary/binary selectors (whose span is part of the message send).
fn selector_reference_span(selector: &crate::ast::MessageSelector) -> Option<Span> {
    match selector {
        crate::ast::MessageSelector::Keyword(parts) if !parts.is_empty() => {
            let first = parts.first().unwrap().span;
            let last = parts.last().unwrap().span;
            Some(first.merge(last))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_source(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        parse(tokens).0
    }

    #[test]
    fn find_class_references_in_expressions() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x := Foo new\ny := Foo bar");

        let refs = find_class_references("Foo", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().all(|r| r.file == file));
    }

    #[test]
    fn find_class_references_in_class_definition() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let refs = find_class_references("Foo", [(&file, &module)]);
        // Should find the class name in the definition
        assert!(!refs.is_empty());
    }

    #[test]
    fn find_class_references_as_superclass() {
        let file = Utf8PathBuf::from("test.bt");
        let module =
            parse_source("Object subclass: Base\n  foo => 1\nBase subclass: Child\n  bar => 2");

        let refs = find_class_references("Base", [(&file, &module)]);
        // Should find: class definition name + superclass reference
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_class_references_cross_file() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new");

        let refs = find_class_references("Foo", [(&file_a, &module_a), (&file_b, &module_b)]);
        // file_a: class definition name, file_b: Foo identifier
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn find_selector_references_in_message_sends() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x bar\ny bar");

        let refs = find_selector_references("bar", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_selector_references_in_method_definition() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let refs = find_selector_references("bar", [(&file, &module)]);
        // Should find method definition
        assert!(!refs.is_empty());
    }

    #[test]
    fn find_selector_references_cross_file() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new\nx bar");

        let refs = find_selector_references("bar", [(&file_a, &module_a), (&file_b, &module_b)]);
        // file_a: method definition, file_b: message send
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn find_selector_references_keyword_message() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x at: 1 put: 2\ny at: 3 put: 4");

        let refs = find_selector_references("at:put:", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_selector_references_no_match() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x bar");

        let refs = find_selector_references("baz", [(&file, &module)]);
        assert!(refs.is_empty());
    }

    #[test]
    fn find_class_references_in_method_bodies() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Widget\n  make => Foo new");

        // Foo appears inside method body — should be found
        let refs = find_class_references("Foo", [(&file, &module)]);
        assert!(!refs.is_empty(), "Should find Foo in method body");
    }

    #[test]
    fn find_selector_references_in_method_bodies() {
        let file = Utf8PathBuf::from("test.bt");
        let module =
            parse_source("Object subclass: Widget\n  doWork: x => x bar\n  go => self bar");

        // "bar" appears as method send inside method bodies
        let refs = find_selector_references("bar", [(&file, &module)]);
        // Should find: 2 message sends in method bodies
        assert!(
            refs.len() >= 2,
            "Should find bar in method bodies, found {}",
            refs.len()
        );
    }

    // BT-1936: Find-references for protocol names.
    //
    // `find_class_references` now also walks `module.protocols`, protocol
    // `extending:` targets, and `TypeParamDecl` bounds on class and protocol
    // type-parameter lists. Expression-site references were already covered
    // by `collect_class_refs` — the regression test below pins that behaviour.

    #[test]
    fn find_references_protocol_definition_name() {
        let file = Utf8PathBuf::from("protocols.bt");
        let module = parse_source("Protocol define: Printable\n  asString -> String\n");

        let refs = find_class_references("Printable", [(&file, &module)]);
        assert_eq!(refs.len(), 1, "definition site should be reported once");
        assert_eq!(refs[0].span, module.protocols[0].name.span);
    }

    #[test]
    fn find_references_protocol_extending_target() {
        let file = Utf8PathBuf::from("protocols.bt");
        let module = parse_source(
            "Protocol define: Comparable\n\
             Protocol define: Sortable\n  \
             extending: Comparable\n  \
             sortKey -> Object\n",
        );

        let refs = find_class_references("Comparable", [(&file, &module)]);
        // Expect: Comparable definition name + extending target in Sortable.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        let spans: Vec<_> = refs.iter().map(|r| r.span).collect();
        assert!(spans.contains(&module.protocols[0].name.span));
        assert!(
            spans.contains(&module.protocols[1].extending.as_ref().unwrap().span),
            "extending: target span missing"
        );
    }

    #[test]
    fn find_references_protocol_bound_on_class_type_param() {
        let file = Utf8PathBuf::from("logger.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\
             Actor subclass: Logger(T :: Printable)\n  \
             log: msg => self\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: Printable definition name + bound reference on Logger's T.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        let bound_span = module.classes[0].type_params[0]
            .bound
            .as_ref()
            .unwrap()
            .span;
        assert!(refs.iter().any(|r| r.span == bound_span));
    }

    #[test]
    fn find_references_protocol_bound_on_protocol_type_param() {
        let file = Utf8PathBuf::from("mapper.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\
             Protocol define: Mapper(T :: Printable)\n  \
             map: x :: T -> Object\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: Printable definition name + bound reference on Mapper's T.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        let bound_span = module.protocols[1].type_params[0]
            .bound
            .as_ref()
            .unwrap()
            .span;
        assert!(refs.iter().any(|r| r.span == bound_span));
    }

    #[test]
    fn find_references_protocol_name_in_method_body() {
        // Regression: expression-site references to a protocol name should
        // still be collected through the existing `collect_class_refs` walker.
        // We embed the reference inside a class method body because bare
        // `x := Printable ...` at the top level of a file with a preceding
        // protocol definition does not parse cleanly (parser state carryover).
        let file = Utf8PathBuf::from("use_protocol.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\n\
             Object subclass: Widget\n  show => Printable requiredMethods\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        // First: protocol definition.
        assert!(refs.iter().any(|r| r.span == module.protocols[0].name.span));
        // Second: ClassReference inside the `show` method body.
        let method_span = module.classes[0].methods[0].span;
        assert!(
            refs.iter().any(|r| {
                r.span.start() >= method_span.start() && r.span.end() <= method_span.end()
            }),
            "expected a method-body reference, got {refs:?}"
        );
    }

    #[test]
    fn find_references_protocol_cross_file() {
        let file_a = Utf8PathBuf::from("Printable.bt");
        let module_a = parse_source("Protocol define: Printable\n  asString -> String\n");

        let file_b = Utf8PathBuf::from("Logger.bt");
        let module_b = parse_source(
            "Actor subclass: Logger(T :: Printable)\n  \
             log: msg => self\n",
        );

        let refs = find_class_references("Printable", [(&file_a, &module_a), (&file_b, &module_b)]);
        // Expect: definition in file_a + type-param bound in file_b.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    // -----------------------------------------------------------------------
    // BT-1936: Parity with find_identifier_at_position — state / class-variable /
    // method-signature type annotations must be reported as references so that
    // goto-definition and find-references see the same set of sites.
    // -----------------------------------------------------------------------

    #[test]
    fn find_references_protocol_in_state_type_annotation() {
        let file = Utf8PathBuf::from("widget.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\n\
             Actor subclass: Widget\n  state: label :: Printable = nil\n  \
             show => self.label\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: protocol definition + state type annotation reference.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        assert!(refs.iter().any(|r| r.span == module.protocols[0].name.span));
        let state_ty_span = match &module.classes[0].state[0].type_annotation {
            Some(TypeAnnotation::Simple(id)) => id.span,
            other => panic!("expected Simple type annotation, got {other:?}"),
        };
        assert!(refs.iter().any(|r| r.span == state_ty_span));
    }

    #[test]
    fn find_references_protocol_in_method_parameter_type_annotation() {
        let file = Utf8PathBuf::from("renderer.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\n\
             Object subclass: Renderer\n  \
             render: x :: Printable => x asString\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: protocol definition + method parameter type annotation reference.
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        assert!(refs.iter().any(|r| r.span == module.protocols[0].name.span));
    }

    #[test]
    fn find_references_protocol_in_method_return_type() {
        let file = Utf8PathBuf::from("factory.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\n\
             Object subclass: Factory\n  \
             make -> Printable => Factory new\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: protocol definition + method return type reference.
        // (The `Factory` in the body is unrelated.)
        assert_eq!(refs.len(), 2, "expected 2 refs, got {refs:?}");
        assert!(refs.iter().any(|r| r.span == module.protocols[0].name.span));
    }

    #[test]
    fn find_references_protocol_in_protocol_method_signature() {
        let file = Utf8PathBuf::from("collection.bt");
        let module = parse_source(
            "Protocol define: Printable\n  asString -> String\n\n\
             Protocol define: Renderable\n  \
             printAll: items :: Printable -> Printable\n",
        );

        let refs = find_class_references("Printable", [(&file, &module)]);
        // Expect: Printable definition + parameter type + return type in Renderable.
        assert_eq!(refs.len(), 3, "expected 3 refs, got {refs:?}");
    }

    // -----------------------------------------------------------------------
    // BT-1940: Constructor patterns in match arms must report the class name
    // as a reference. Prior to this fix, `find_class_references` walked the
    // arm body and guard but not the pattern, so `Result ok: v -> v` was
    // invisible to find-references on `Result`.
    // -----------------------------------------------------------------------

    #[test]
    fn find_class_references_in_constructor_pattern() {
        let file = Utf8PathBuf::from("classify.bt");
        let module = parse_source(
            "Value subclass: Result\n  ok: v\n  error: e\n\n\
             Object subclass: Classifier\n  \
             classify: r => r match: [\n    \
             Result ok: v -> v;\n    \
             Result error: _ -> 0\n  ]\n",
        );

        let refs = find_class_references("Result", [(&file, &module)]);
        // Expect: class definition + two constructor pattern sites.
        // The method body does not reference `Result` as an expression.
        assert_eq!(refs.len(), 3, "expected 3 refs, got {refs:?}");
        // First ref is the class definition.
        assert!(refs.iter().any(|r| r.span == module.classes[0].name.span));
    }

    #[test]
    fn find_class_references_constructor_pattern_plus_expression() {
        let file = Utf8PathBuf::from("flow.bt");
        let module = parse_source(
            "Value subclass: Result\n  ok: v\n  error: e\n\n\
             Object subclass: Flow\n  \
             run => (Result ok: 1) match: [\n    \
             Result ok: v -> v;\n    \
             Result error: _ -> 0\n  ]\n",
        );

        let refs = find_class_references("Result", [(&file, &module)]);
        // Expect: class definition + expression-site `Result ok: 1`
        // + two constructor pattern sites.
        assert_eq!(refs.len(), 4, "expected 4 refs, got {refs:?}");
    }

    #[test]
    fn find_class_references_constructor_pattern_with_guard() {
        let file = Utf8PathBuf::from("guarded.bt");
        let module = parse_source(
            "Value subclass: Result\n  ok: v\n  error: e\n\n\
             Object subclass: Guarded\n  \
             run: r => r match: [\n    \
             Result ok: v when: v > 0 -> v;\n    \
             Result ok: _ -> 0;\n    \
             Result error: _ -> -1\n  ]\n",
        );

        let refs = find_class_references("Result", [(&file, &module)]);
        // Expect: class definition + three constructor pattern sites
        // (one with a guard, two without).
        assert_eq!(refs.len(), 4, "expected 4 refs, got {refs:?}");
    }
}
