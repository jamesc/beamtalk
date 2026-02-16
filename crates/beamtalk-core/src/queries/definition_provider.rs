// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Definition provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `DefinitionProvider` from the DDD model.
//! It locates symbol definitions for go-to-definition requests, supporting
//! both single-file and cross-file lookups via `ProjectIndex`.
//!
//! # Design
//!
//! Definition resolution follows a priority order:
//! 1. Local variable assignment in the current file (first assignment heuristic)
//! 2. Class definition in any indexed file (via `ProjectIndex`)
//!
//! # Performance
//!
//! Must respond in <100ms for typical project sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - ADR 0024: Static-First, Live-Augmented IDE Tooling
//! - LSP specification: Language Server Protocol textDocument/definition

use crate::ast::{Expression, MessageSelector, Module};
use crate::language_service::{Location, ProjectIndex};
use crate::semantic_analysis::{ClassHierarchy, InferredType, infer_types};
use crate::source_analysis::Span;
use camino::Utf8PathBuf;
use ecow::EcoString;

/// Find the definition of a variable by name within a module (first assignment heuristic).
///
/// Walks the AST looking for the first `Assignment` where the target identifier
/// matches `name`. Returns the span of the target identifier if found.
#[must_use]
pub fn find_definition_in_module(module: &Module, name: &str) -> Option<Span> {
    for expr in &module.expressions {
        if let Some(span) = find_definition_in_expr(expr, name) {
            return Some(span);
        }
    }
    None
}

/// Find the definition of a symbol, searching across all indexed files.
///
/// Resolution order:
/// 1. Local variable assignment in `current_file`
/// 2. Class definition matching the identifier name in any indexed file
///
/// The `files` parameter provides access to parsed modules for all indexed files.
#[must_use]
pub fn find_definition_cross_file<'a>(
    name: &str,
    current_file: &Utf8PathBuf,
    current_module: &Module,
    project_index: &ProjectIndex,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<Location> {
    // 1. Local variable definition in current file
    if let Some(span) = find_definition_in_module(current_module, name) {
        return Some(Location::new(current_file.clone(), span));
    }

    // 2. Class definition in the project index (skip builtins — they have no file location)
    if !crate::semantic_analysis::ClassHierarchy::is_builtin_class(name)
        && project_index.hierarchy().has_class(name)
    {
        // Find which file defines this class
        for (file_path, module) in files {
            for class in &module.classes {
                if class.name.name.as_str() == name {
                    return Some(Location::new(file_path.clone(), class.name.span));
                }
            }
        }
    }

    None
}

/// Find the definition of a method selector, searching across all indexed files.
///
/// Uses `ClassHierarchy` to determine which class defines the method, then
/// locates the `MethodDefinition` AST node in the defining file.
#[must_use]
pub fn find_method_definition_cross_file<'a>(
    selector: &str,
    project_index: &ProjectIndex,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<Location> {
    find_method_definition_cross_file_with_receiver(selector, None, project_index, files)
}

/// Receiver-derived context for selector definition resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReceiverClassContext {
    pub class_name: EcoString,
    pub class_side: bool,
}

/// Receiver hint captured at a selector call site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReceiverHint {
    ClassReference(EcoString),
    SelfReceiver,
    SuperReceiver,
    Expression(Span),
}

/// Selector lookup result including receiver hint for contextual resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorLookup {
    pub selector_name: EcoString,
    pub selector_span: Span,
    pub receiver_hint: ReceiverHint,
}

/// Find the definition of a method selector with optional receiver class context.
#[must_use]
pub fn find_method_definition_cross_file_with_receiver<'a>(
    selector: &str,
    receiver_context: Option<&ReceiverClassContext>,
    project_index: &ProjectIndex,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<Location> {
    // Find which class defines this selector via the merged hierarchy
    let hierarchy = project_index.hierarchy();
    let (defining_class, class_side) = receiver_context
        .and_then(|context| {
            find_defining_class_from_receiver(selector, context, hierarchy)
                .map(|class_name| (class_name, Some(context.class_side)))
        })
        .or_else(|| {
            find_defining_class(selector, hierarchy).map(|class_name| (class_name, None))
        })?;

    // Search files for the class definition containing this method
    for (file_path, module) in files {
        if let Some(span) = find_method_in_module(module, &defining_class, selector, class_side) {
            return Some(Location::new(file_path.clone(), span));
        }
    }

    None
}

/// Resolve receiver class context for a selector lookup at the given offset.
#[must_use]
pub fn resolve_receiver_class_context(
    module: &Module,
    offset: u32,
    selector_lookup: &SelectorLookup,
    hierarchy: &ClassHierarchy,
) -> Option<ReceiverClassContext> {
    let class_context = find_class_context(module, offset);
    let type_map = infer_types(module, hierarchy);
    match &selector_lookup.receiver_hint {
        ReceiverHint::ClassReference(name) => Some(ReceiverClassContext {
            class_name: name.clone(),
            class_side: true,
        }),
        ReceiverHint::SelfReceiver => {
            class_context
                .self_class()
                .map(|(class_name, class_side)| ReceiverClassContext {
                    class_name,
                    class_side,
                })
        }
        ReceiverHint::SuperReceiver => {
            class_context
                .superclass_name(hierarchy)
                .map(|(class_name, class_side)| ReceiverClassContext {
                    class_name,
                    class_side,
                })
        }
        ReceiverHint::Expression(span) => type_map
            .get(*span)
            .and_then(InferredType::as_known)
            .cloned()
            .map(|class_name| ReceiverClassContext {
                class_name,
                class_side: false,
            }),
    }
}

/// Find which class defines a given selector in the hierarchy.
///
/// Prefers user-defined classes over builtin/stdlib classes to ensure
/// go-to-definition navigates to user code when a selector is defined
/// in both user code and stdlib. Ambiguous matches are resolved deterministically
/// by scanning class names in lexical order.
fn find_defining_class(selector: &str, hierarchy: &ClassHierarchy) -> Option<EcoString> {
    let mut builtin_match = None;
    let mut class_names: Vec<&EcoString> = hierarchy.class_names().collect();
    class_names.sort_unstable_by(|a, b| a.as_str().cmp(b.as_str()));

    for class_name in class_names {
        for method in hierarchy.all_methods(class_name) {
            if method.selector == selector && method.defined_in == *class_name {
                if ClassHierarchy::is_builtin_class(class_name) {
                    // Remember but keep searching for user-defined match
                    if builtin_match.is_none() {
                        builtin_match = Some(class_name.clone());
                    }
                } else {
                    return Some(class_name.clone());
                }
            }
        }
    }

    builtin_match
}

fn find_defining_class_from_receiver(
    selector: &str,
    receiver_context: &ReceiverClassContext,
    hierarchy: &ClassHierarchy,
) -> Option<EcoString> {
    if receiver_context.class_side {
        hierarchy
            .find_class_method(receiver_context.class_name.as_str(), selector)
            .map(|method| method.defined_in)
    } else {
        hierarchy
            .find_method(receiver_context.class_name.as_str(), selector)
            .map(|method| method.defined_in)
    }
}

/// Find a method definition within a module, given the class name and selector.
fn find_method_in_module(
    module: &Module,
    class_name: &str,
    selector: &str,
    class_side: Option<bool>,
) -> Option<Span> {
    // Search class definitions
    for class in &module.classes {
        if class.name.name == class_name {
            if class_side != Some(true) {
                for method in &class.methods {
                    if method.selector.name() == selector {
                        return Some(method.span);
                    }
                }
            }
            if class_side != Some(false) {
                for method in &class.class_methods {
                    if method.selector.name() == selector {
                        return Some(method.span);
                    }
                }
            }
        }
    }
    // Search standalone method definitions (Tonel-style)
    for smd in &module.method_definitions {
        if smd.class_name.name == class_name
            && smd.method.selector.name() == selector
            && class_side.is_none_or(|expected| expected == smd.is_class_method)
        {
            return Some(smd.span);
        }
    }
    None
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassContext<'a> {
    InstanceMethod(&'a str),
    ClassMethod(&'a str),
    StandaloneInstanceMethod(&'a str),
    StandaloneClassMethod(&'a str),
    TopLevel,
}

impl ClassContext<'_> {
    fn self_class(&self) -> Option<(EcoString, bool)> {
        match self {
            Self::InstanceMethod(name) | Self::StandaloneInstanceMethod(name) => {
                Some((EcoString::from(*name), false))
            }
            Self::ClassMethod(name) | Self::StandaloneClassMethod(name) => {
                Some((EcoString::from(*name), true))
            }
            Self::TopLevel => None,
        }
    }

    fn superclass_name(&self, hierarchy: &ClassHierarchy) -> Option<(EcoString, bool)> {
        let (class_name, class_side) = self.self_class()?;
        hierarchy
            .get_class(class_name.as_str())
            .and_then(|info| info.superclass.clone())
            .map(|super_name| (super_name, class_side))
    }
}

fn find_class_context(module: &Module, offset: u32) -> ClassContext<'_> {
    for class in &module.classes {
        for method in &class.methods {
            if offset >= method.span.start() && offset < method.span.end() {
                return ClassContext::InstanceMethod(class.name.name.as_str());
            }
        }
        for method in &class.class_methods {
            if offset >= method.span.start() && offset < method.span.end() {
                return ClassContext::ClassMethod(class.name.name.as_str());
            }
        }
    }
    for smd in &module.method_definitions {
        if offset >= smd.method.span.start() && offset < smd.method.span.end() {
            if smd.is_class_method {
                return ClassContext::StandaloneClassMethod(smd.class_name.name.as_str());
            }
            return ClassContext::StandaloneInstanceMethod(smd.class_name.name.as_str());
        }
    }
    ClassContext::TopLevel
}

/// Find the selector at a given byte offset in an expression.
///
/// Returns the full selector name and the span of the selector keyword/operator
/// that the cursor is on.
pub fn find_selector_in_expr(expr: &Expression, offset: u32) -> Option<(EcoString, Span)> {
    find_selector_lookup_in_expr(expr, offset)
        .map(|lookup| (lookup.selector_name, lookup.selector_span))
}

/// Find selector lookup details at a given byte offset in an expression.
///
/// Returns a [`SelectorLookup`] containing:
/// - full selector name,
/// - selector span under the cursor, and
/// - receiver hint used for context-aware method resolution.
///
/// Unlike [`find_selector_in_expr`], this API keeps receiver metadata so
/// callers can resolve class-side/instance-side selector targets.
pub fn find_selector_lookup_in_expr(expr: &Expression, offset: u32) -> Option<SelectorLookup> {
    let span = expr.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }

    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
        } => find_selector_lookup_in_message_send(receiver, selector, arguments, *span, offset),
        Expression::Cascade {
            receiver, messages, ..
        } => find_selector_lookup_in_cascade(receiver, messages, offset),
        Expression::Assignment { target, value, .. } => {
            find_selector_lookup_in_expr(target, offset)
                .or_else(|| find_selector_lookup_in_expr(value, offset))
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|e| find_selector_lookup_in_expr(e, offset)),
        Expression::Return { value, .. } => find_selector_lookup_in_expr(value, offset),
        Expression::Parenthesized { expression, .. } => {
            find_selector_lookup_in_expr(expression, offset)
        }
        Expression::FieldAccess { receiver, .. } => find_selector_lookup_in_expr(receiver, offset),
        Expression::Pipe { value, target, .. } => find_selector_lookup_in_expr(value, offset)
            .or_else(|| find_selector_lookup_in_expr(target, offset)),
        Expression::Match { value, arms, .. } => {
            if let Some(result) = find_selector_lookup_in_expr(value, offset) {
                return Some(result);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    if let Some(result) = find_selector_lookup_in_expr(guard, offset) {
                        return Some(result);
                    }
                }
                if let Some(result) = find_selector_lookup_in_expr(&arm.body, offset) {
                    return Some(result);
                }
            }
            None
        }
        Expression::StringInterpolation { segments, .. } => segments.iter().find_map(|seg| {
            if let crate::ast::StringSegment::Interpolation(expr) = seg {
                find_selector_lookup_in_expr(expr, offset)
            } else {
                None
            }
        }),
        Expression::ListLiteral { elements, tail, .. } => elements
            .iter()
            .find_map(|e| find_selector_lookup_in_expr(e, offset))
            .or_else(|| {
                tail.as_ref()
                    .and_then(|t| find_selector_lookup_in_expr(t, offset))
            }),
        Expression::MapLiteral { pairs, .. } => pairs.iter().find_map(|pair| {
            find_selector_lookup_in_expr(&pair.key, offset)
                .or_else(|| find_selector_lookup_in_expr(&pair.value, offset))
        }),
        _ => None,
    }
}

fn find_selector_lookup_in_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    message_span: Span,
    offset: u32,
) -> Option<SelectorLookup> {
    // Check receiver first
    if let Some(result) = find_selector_lookup_in_expr(receiver, offset) {
        return Some(result);
    }
    // Check arguments
    for arg in arguments {
        if let Some(result) = find_selector_lookup_in_expr(arg, offset) {
            return Some(result);
        }
    }
    // Check if cursor is on the selector itself
    if let Some((selector_name, selector_span)) = selector_span_contains(
        selector,
        offset,
        selector_span_for_message_send(receiver.span(), arguments, message_span),
    ) {
        return Some(SelectorLookup {
            selector_name,
            selector_span,
            receiver_hint: receiver_hint_for_expr(receiver),
        });
    }
    None
}

fn find_selector_lookup_in_cascade(
    receiver: &Expression,
    messages: &[crate::ast::CascadeMessage],
    offset: u32,
) -> Option<SelectorLookup> {
    if let Some(result) = find_selector_lookup_in_expr(receiver, offset) {
        return Some(result);
    }
    for msg in messages {
        // Check arguments
        for arg in &msg.arguments {
            if let Some(result) = find_selector_lookup_in_expr(arg, offset) {
                return Some(result);
            }
        }
        // Check cascade message selector
        if let Some((selector_name, selector_span)) = selector_span_contains(
            &msg.selector,
            offset,
            selector_span_for_cascade_message(&msg.arguments, msg.span),
        ) {
            return Some(SelectorLookup {
                selector_name,
                selector_span,
                receiver_hint: receiver_hint_for_expr(receiver),
            });
        }
    }
    None
}

fn receiver_hint_for_expr(receiver: &Expression) -> ReceiverHint {
    match receiver {
        Expression::ClassReference { name, .. } => ReceiverHint::ClassReference(name.name.clone()),
        Expression::Identifier(ident) if ident.name == "self" => ReceiverHint::SelfReceiver,
        Expression::Super(_) => ReceiverHint::SuperReceiver,
        _ => ReceiverHint::Expression(receiver.span()),
    }
}

fn selector_span_for_message_send(
    receiver_span: Span,
    arguments: &[Expression],
    message_span: Span,
) -> Span {
    let start = receiver_span.end();
    let end = arguments
        .first()
        .map_or(message_span.end(), |arg| arg.span().start());
    Span::new(start, end)
}

fn selector_span_for_cascade_message(arguments: &[Expression], message_span: Span) -> Span {
    let end = arguments
        .first()
        .map_or(message_span.end(), |arg| arg.span().start());
    Span::new(message_span.start(), end)
}

/// Check if cursor offset is within a selector's span, returning full name + span.
fn selector_span_contains(
    selector: &MessageSelector,
    offset: u32,
    computed_span: Span,
) -> Option<(EcoString, Span)> {
    match selector {
        MessageSelector::Unary(_) | MessageSelector::Binary(_) => (offset >= computed_span.start()
            && offset < computed_span.end())
        .then(|| (selector.name(), computed_span)),
        MessageSelector::Keyword(parts) => {
            // Each keyword part has its own span
            let full_name = selector.name();
            for part in parts {
                if offset >= part.span.start() && offset < part.span.end() {
                    return Some((full_name, part.span));
                }
            }
            None
        }
    }
}

/// Recursively search for a variable definition (first assignment) in an expression.
fn find_definition_in_expr(expr: &Expression, name: &str) -> Option<Span> {
    match expr {
        Expression::Assignment { target, .. } => {
            if let Expression::Identifier(ident) = target.as_ref() {
                if ident.name == name {
                    return Some(ident.span);
                }
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|e| find_definition_in_expr(e, name)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_source(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        parse(tokens).0
    }

    #[test]
    fn find_definition_variable_assignment() {
        let module = parse_source("x := 42\ny := x");
        let span = find_definition_in_module(&module, "x");
        assert!(span.is_some());
        assert_eq!(span.unwrap().start(), 0);
    }

    #[test]
    fn find_definition_not_found() {
        let module = parse_source("x := 42");
        let span = find_definition_in_module(&module, "y");
        assert!(span.is_none());
    }

    #[test]
    fn find_definition_in_block() {
        let module = parse_source("[x := 42]");
        let span = find_definition_in_module(&module, "x");
        assert!(span.is_some());
    }

    #[test]
    fn cross_file_local_variable_preferred() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("x := 42\ny := x");
        let index = ProjectIndex::new();

        let loc =
            find_definition_cross_file("x", &file_a, &module_a, &index, [(&file_a, &module_a)]);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn cross_file_class_definition() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0;

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new");

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);

        let loc = find_definition_cross_file(
            "Foo",
            &file_b,
            &module_b,
            &index,
            [(&file_a, &module_a), (&file_b, &module_b)],
        );
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn cross_file_stdlib_class() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x := Integer new");
        let index = ProjectIndex::new();

        // Integer is a built-in, not defined in any file — so no location
        let loc = find_definition_cross_file("Integer", &file, &module, &index, [(&file, &module)]);
        assert!(loc.is_none());
    }

    #[test]
    fn cross_file_method_definition() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1\n  baz: x => x + 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0;

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);

        // Should find 'bar' method definition
        let loc = find_method_definition_cross_file("bar", &index, [(&file_a, &module_a)]);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);

        // Should find 'baz:' method definition
        let loc = find_method_definition_cross_file("baz:", &index, [(&file_a, &module_a)]);
        assert!(loc.is_some());
    }

    #[test]
    fn cross_file_method_definition_not_found() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0;

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);

        let loc = find_method_definition_cross_file("nonexistent", &index, [(&file_a, &module_a)]);
        assert!(loc.is_none());
    }

    #[test]
    fn cross_file_method_definition_across_files() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0;

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new\nx bar");
        let hierarchy_b = ClassHierarchy::build(&module_b).0;

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);
        index.update_file(file_b.clone(), &hierarchy_b);

        // From file_b, should find method in file_a
        let loc = find_method_definition_cross_file(
            "bar",
            &index,
            [(&file_a, &module_a), (&file_b, &module_b)],
        );
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn find_selector_in_keyword_message() {
        let source = "obj at: 1 put: 2";
        let module = parse_source(source);
        // The keyword "at:" starts at position 4
        let result = find_selector_in_expr(&module.expressions[0], 4);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "at:put:");
    }

    #[test]
    fn find_selector_not_on_selector() {
        let source = "obj at: 1 put: 2";
        let module = parse_source(source);
        // Position 0 is on "obj" (receiver), not a selector
        let result = find_selector_in_expr(&module.expressions[0], 0);
        assert!(result.is_none());
    }

    #[test]
    fn find_selector_in_unary_message() {
        let source = "obj size";
        let module = parse_source(source);
        // Position 4 is on/near "size"
        let result = find_selector_in_expr(&module.expressions[0], 4);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "size");
    }

    #[test]
    fn find_selector_in_binary_message() {
        let source = "3 + 4";
        let module = parse_source(source);
        // Position 2 is on/near "+"
        let result = find_selector_in_expr(&module.expressions[0], 2);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "+");
    }
}
