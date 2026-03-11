// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Completion provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `CompletionProvider` from the DDD model.
//! It suggests completions at the cursor position based on the current editing
//! context. The provider follows LSP terminology and aligns with the ubiquitous
//! language defined in `docs/beamtalk-ddd-model.md`.
//!
//! # Design
//!
//! Completions are context-sensitive and consider:
//! - Position in the source (is it after a dot? after whitespace?)
//! - Surrounding AST nodes (in a block? in a message send?)
//! - Available identifiers in scope
//! - Language keywords and built-in types
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol completion requests

use crate::ast::{ClassDefinition, Expression, Module};
use crate::language_service::{Completion, CompletionKind, Position};
use crate::queries::enrich_hierarchy_with_inferred_returns;
use crate::queries::erlang_modules;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{ClassHierarchy, InferredType};
use ecow::EcoString;
use std::collections::HashSet;
use std::fmt::Write;

/// The class context at the cursor position.
#[derive(Debug, Clone)]
enum ClassContext<'a> {
    /// Cursor is inside an instance method of the named class.
    InstanceMethod(&'a ClassDefinition),
    /// Cursor is inside a class-side method of the named class.
    ClassMethod(&'a ClassDefinition),
    /// Cursor is at top level (not inside any class).
    TopLevel,
}

/// Computes code completions at a given position in a module.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `source` - The source text
/// * `position` - The cursor position
///
/// # Returns
///
/// A list of completion suggestions appropriate for the context.
///
/// # Examples
///
/// ```
/// use beamtalk_core::queries::completion_provider::compute_completions;
/// use beamtalk_core::language_service::Position;
/// use beamtalk_core::semantic_analysis::ClassHierarchy;
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
///
/// let source = "x := 42";
/// let tokens = lex_with_eof(source);
/// let (module, _) = parse(tokens);
/// let hierarchy = ClassHierarchy::build(&module).0.unwrap();
///
/// let completions = compute_completions(&module, source, Position::new(0, 0), &hierarchy);
/// assert!(!completions.is_empty());
/// // Should include keywords like "self", "true", "false"
/// assert!(completions.iter().any(|c| c.label == "self"));
/// ```
#[must_use]
pub fn compute_completions(
    module: &Module,
    source: &str,
    position: Position,
    hierarchy: &ClassHierarchy,
) -> Vec<Completion> {
    // Validate position is within bounds
    let offset = match position.to_offset(source) {
        Some(o) => {
            // Source files won't exceed u32::MAX bytes
            #[expect(clippy::cast_possible_truncation, reason = "source files < 4GB")]
            let offset = o as u32;
            offset
        }
        None => return Vec::new(),
    };

    let mut completions = Vec::new();

    // Check for Erlang module context first — if we're completing
    // after "Erlang" or "Erlang <module>", return specialized completions
    if let Some(erlang_completions) = compute_erlang_completions(module, source, offset) {
        return erlang_completions;
    }

    // Determine class context at cursor position
    let context = find_class_context(module, offset);

    // Enrich hierarchy with inferred return types and get the type map in a single
    // TypeChecker pass (BT-1014, BT-1047). The TypeChecker now consults its own
    // inferred method_return_types during chain resolution, so a second pass is
    // no longer needed for correct chain completion filtering (BT-1005).
    let enriched_hierarchy;
    let (hierarchy, type_map) = {
        let (enriched, type_map) = enrich_hierarchy_with_inferred_returns(module, hierarchy);
        let h = match enriched {
            Some(h) => {
                enriched_hierarchy = h;
                &enriched_hierarchy
            }
            None => hierarchy,
        };
        (h, type_map)
    };

    // Try to find receiver type at cursor for type-filtered completions
    let receiver_type = find_receiver_type(module, offset, &type_map);

    // Add keyword completions
    add_keyword_completions(&mut completions);

    // Add identifiers from the current scope
    add_identifier_completions(module, &mut completions);

    // Add class names as completions
    add_class_name_completions(module, hierarchy, &mut completions);

    // Add message completions from class hierarchy (context-aware, type-filtered)
    add_hierarchy_completions(
        module,
        hierarchy,
        &context,
        receiver_type.as_ref(),
        &mut completions,
    );

    // Remove duplicates
    deduplicate_completions(&mut completions);

    completions
}

/// Checks if the cursor is in an `Erlang <module>` context and returns
/// specialized completions.
///
/// Returns `Some(completions)` if the cursor is:
/// - Right after `Erlang ` → suggests common Erlang module names
/// - Right after `Erlang <module> ` → suggests module exports as Beamtalk selectors
///
/// Returns `None` if the cursor is not in an Erlang context.
fn compute_erlang_completions(
    module: &Module,
    source: &str,
    offset: u32,
) -> Option<Vec<Completion>> {
    // Look for the Erlang context by examining the text before the cursor
    let text_before = &source[..offset as usize];
    let trimmed = text_before.trim_end();

    // Check for "Erlang <module>" context (cursor after module name)
    // Pattern: text ends with "Erlang <word>" where <word> is a known module
    if let Some(erlang_module) = detect_erlang_module_context(trimmed) {
        if let Some(module_info) = erlang_modules::find_module(erlang_module) {
            let mut completions = Vec::new();
            let mut seen = HashSet::new();
            for &(name, arity) in module_info.exports {
                let selector = erlang_modules::export_to_selector(name, arity);
                if seen.insert(selector.clone()) {
                    let detail = erlang_modules::export_detail(module_info.name, name, arity);
                    let doc = format!(
                        "Erlang function `{module_name}:{name}/{arity}`",
                        module_name = module_info.name
                    );
                    completions.push(
                        Completion::new(selector.as_str(), CompletionKind::Function)
                            .with_detail(detail)
                            .with_documentation(doc),
                    );
                }
            }
            return Some(completions);
        }
    }

    // Check for "Erlang" context (cursor right after "Erlang")
    // Also check by walking the AST for a ClassReference("Erlang") near cursor
    if detect_erlang_class_context(module, trimmed, offset) {
        let mut completions = Vec::new();
        for module_info in erlang_modules::COMMON_MODULES {
            completions.push(
                Completion::new(module_info.name, CompletionKind::Module)
                    .with_detail(format!("Erlang module: {}", module_info.name))
                    .with_documentation(module_info.description),
            );
        }
        return Some(completions);
    }

    None
}

/// Returns `true` if `b` is a valid Beamtalk identifier character (letter, digit, or underscore).
fn is_identifier_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Detects if the text before cursor ends with `Erlang <module_name>`.
///
/// Returns the module name if found and recognized.
fn detect_erlang_module_context(trimmed: &str) -> Option<&str> {
    // Find the last word (potential module name)
    let last_space = trimmed.rfind(' ')?;
    let module_name = &trimmed[last_space + 1..];

    // Check the text before the module name ends with "Erlang"
    let before_module = trimmed[..last_space].trim_end();
    if before_module.ends_with("Erlang") {
        // Verify "Erlang" is at a word boundary
        let erlang_start = before_module.len() - "Erlang".len();
        if erlang_start == 0 || !is_identifier_char(before_module.as_bytes()[erlang_start - 1]) {
            return Some(module_name);
        }
    }
    None
}

/// Detects if the cursor is right after the `Erlang` class reference.
fn detect_erlang_class_context(module: &Module, trimmed: &str, offset: u32) -> bool {
    // Text-based check: does the text before cursor end with "Erlang"?
    if trimmed.ends_with("Erlang") {
        let erlang_start = trimmed.len() - "Erlang".len();
        if erlang_start == 0 || !is_identifier_char(trimmed.as_bytes()[erlang_start - 1]) {
            return true;
        }
    }

    // Cheap textual hint before AST fallback — only scan when "Erlang" is near cursor
    if let Some(idx) = trimmed.rfind("Erlang") {
        if trimmed.len().saturating_sub(idx) <= 32 {
            return find_erlang_class_ref(module, offset);
        }
    }
    false
}

/// Walks the AST to find a `ClassReference("Erlang")` expression near the cursor.
fn find_erlang_class_ref(module: &Module, offset: u32) -> bool {
    let all_exprs = module.expressions.iter().map(|s| &s.expression).chain(
        module
            .classes
            .iter()
            .flat_map(|c| {
                c.methods
                    .iter()
                    .chain(c.class_methods.iter())
                    .flat_map(|m| m.body.iter().map(|s| &s.expression))
            })
            .chain(
                module
                    .method_definitions
                    .iter()
                    .flat_map(|smd| smd.method.body.iter().map(|s| &s.expression)),
            ),
    );

    for expr in all_exprs {
        if has_erlang_class_ref_at(expr, offset) {
            return true;
        }
    }
    false
}

/// Recursively checks if an expression contains a `ClassReference("Erlang")`
/// just before the cursor offset.
fn has_erlang_class_ref_at(expr: &Expression, offset: u32) -> bool {
    match expr {
        Expression::ClassReference { name, span } => {
            name.name == "Erlang" && offset >= span.end() && offset <= span.end() + 2
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            has_erlang_class_ref_at(receiver, offset)
                || arguments
                    .iter()
                    .any(|arg| has_erlang_class_ref_at(arg, offset))
        }
        Expression::Assignment { target, value, .. } => {
            has_erlang_class_ref_at(target, offset) || has_erlang_class_ref_at(value, offset)
        }
        Expression::Parenthesized { expression, .. } => has_erlang_class_ref_at(expression, offset),
        Expression::Block(block) => block
            .body
            .iter()
            .any(|s| has_erlang_class_ref_at(&s.expression, offset)),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            has_erlang_class_ref_at(receiver, offset)
                || messages.iter().any(|msg| {
                    msg.arguments
                        .iter()
                        .any(|a| has_erlang_class_ref_at(a, offset))
                })
        }
        Expression::Return { value, .. } => has_erlang_class_ref_at(value, offset),
        _ => false,
    }
}

/// Adds keyword completions.
fn add_keyword_completions(completions: &mut Vec<Completion>) {
    let keywords = [
        ("self", "Reference to the current object"),
        ("super", "Reference to the superclass"),
        ("true", "Boolean true value"),
        ("false", "Boolean false value"),
        ("nil", "Null/absent value"),
        ("match:", "Pattern matching expression"),
        ("if:then:else:", "Conditional expression"),
    ];

    for (keyword, doc) in &keywords {
        completions
            .push(Completion::new(*keyword, CompletionKind::Keyword).with_documentation(*doc));
    }
}

/// Adds identifier completions from the module.
fn add_identifier_completions(module: &Module, completions: &mut Vec<Completion>) {
    let mut identifiers = HashSet::new();

    // Collect all identifiers from the module
    for stmt in &module.expressions {
        collect_identifiers_from_expr(&stmt.expression, &mut identifiers);
    }

    // Add them as completions
    for ident in identifiers {
        completions.push(Completion::new(ident, CompletionKind::Variable));
    }
}

/// Determines the class context at a given byte offset.
///
/// Walks the module's class definitions to find if the offset is inside
/// an instance method or a class-side method.
fn find_class_context(module: &Module, offset: u32) -> ClassContext<'_> {
    for class in &module.classes {
        // Check instance methods
        for method in &class.methods {
            let span = method.span;
            if offset >= span.start() && offset < span.end() {
                return ClassContext::InstanceMethod(class);
            }
        }
        // Check class-side methods
        for method in &class.class_methods {
            let span = method.span;
            if offset >= span.start() && offset < span.end() {
                return ClassContext::ClassMethod(class);
            }
        }
    }
    ClassContext::TopLevel
}

/// Adds class names as completions (from module + hierarchy builtins).
fn add_class_name_completions(
    module: &Module,
    hierarchy: &ClassHierarchy,
    completions: &mut Vec<Completion>,
) {
    let mut seen = HashSet::new();

    // Add user-defined classes from the module (with richer documentation)
    for class in &module.classes {
        let name = &class.name.name;
        if seen.insert(name.clone()) {
            let doc = class.superclass.as_ref().map_or_else(
                || format!("Class: {name}"),
                |s| format!("Class: {name} (extends {})", s.name),
            );
            completions.push(
                Completion::new(name.as_str(), CompletionKind::Class).with_documentation(doc),
            );
        }
    }

    // Add all known classes from the hierarchy (builtins + any others)
    for class_name in hierarchy.class_names() {
        if seen.insert(class_name.clone()) {
            let doc = hierarchy
                .get_class(class_name.as_str())
                .and_then(|info| info.superclass.as_ref())
                .map_or_else(
                    || format!("Class: {class_name}"),
                    |s| format!("Class: {class_name} (extends {s})"),
                );
            completions.push(
                Completion::new(class_name.as_str(), CompletionKind::Class).with_documentation(doc),
            );
        }
    }
}

/// Recursively collects identifiers from an expression.
fn collect_identifiers_from_expr(expr: &Expression, identifiers: &mut HashSet<EcoString>) {
    match expr {
        Expression::Identifier(ident) => {
            identifiers.insert(ident.name.clone());
        }
        Expression::Assignment { target, value, .. } => {
            collect_identifiers_from_expr(target, identifiers);
            collect_identifiers_from_expr(value, identifiers);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_identifiers_from_expr(receiver, identifiers);
            for arg in arguments {
                collect_identifiers_from_expr(arg, identifiers);
            }
        }
        Expression::Block(block) => {
            for param in &block.parameters {
                identifiers.insert(param.name.clone());
            }
            for stmt in &block.body {
                collect_identifiers_from_expr(&stmt.expression, identifiers);
            }
        }
        Expression::Return { value, .. } => {
            collect_identifiers_from_expr(value, identifiers);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_identifiers_from_expr(expression, identifiers);
        }
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            identifiers.insert(field.name.clone());
            collect_identifiers_from_expr(receiver, identifiers);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_identifiers_from_expr(receiver, identifiers);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_identifiers_from_expr(arg, identifiers);
                }
            }
        }
        Expression::Match { value, arms, .. } => {
            collect_identifiers_from_expr(value, identifiers);
            for arm in arms {
                collect_identifiers_from_expr(&arm.body, identifiers);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    collect_identifiers_from_expr(expr, identifiers);
                }
            }
        }
        _ => {}
    }
}

/// Distinguishes between instance-side and class-side receivers for type-filtered completions.
#[derive(Debug)]
enum ReceiverSide {
    /// An instance of the class (show instance methods)
    Instance(EcoString),
    /// The class itself (show class-side methods like `spawn`, `new`)
    Class(EcoString),
}

/// Finds the inferred type of the expression immediately before the cursor.
///
/// This enables type-filtered completions: if the cursor follows an expression
/// with a known type, we can restrict method suggestions to that type.
///
/// Returns `None` if no receiver can be determined (falls back to showing all methods).
fn find_receiver_type(module: &Module, offset: u32, type_map: &TypeMap) -> Option<ReceiverSide> {
    // Search expressions for one that ends just before the cursor
    // (the cursor is after the receiver, typing a message to send)
    let expressions = module
        .expressions
        .iter()
        .map(|s| &s.expression)
        .chain(module.classes.iter().flat_map(|c| {
            c.methods
                .iter()
                .chain(c.class_methods.iter())
                .flat_map(|m| m.body.iter().map(|s| &s.expression))
        }))
        .chain(
            module
                .method_definitions
                .iter()
                .flat_map(|smd| smd.method.body.iter().map(|s| &s.expression)),
        );

    for expr in expressions {
        if let Some(ty) = find_receiver_in_expr(expr, offset, type_map) {
            return Some(ty);
        }
    }
    None
}

/// Recursively search for a receiver expression at the given cursor offset.
fn find_receiver_in_expr(
    expr: &Expression,
    offset: u32,
    type_map: &TypeMap,
) -> Option<ReceiverSide> {
    let span = expr.span();
    // Check if cursor is within or just after this expression
    if offset < span.start() || offset > span.end() + 1 {
        return None;
    }

    match expr {
        // If cursor is right after a message send, the result type is the receiver
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            // Check arguments first (inner-most match wins)
            for arg in arguments {
                if let Some(ty) = find_receiver_in_expr(arg, offset, type_map) {
                    return Some(ty);
                }
            }
            if let Some(ty) = find_receiver_in_expr(receiver, offset, type_map) {
                return Some(ty);
            }
            // If cursor is just after the message send span, use the send's result type
            if offset >= span.end() && offset <= span.end() + 1 {
                // `SomeClass class <TAB>` — `class` sent to a ClassReference stays on class-side
                if selector.name() == "class" {
                    if let Expression::ClassReference { name, .. } = receiver.as_ref() {
                        return Some(ReceiverSide::Class(name.name.clone()));
                    }
                }
                return type_map.get(*span).and_then(|ty| match ty {
                    InferredType::Known(n) => Some(ReceiverSide::Instance(n.clone())),
                    InferredType::Dynamic => None,
                });
            }
            None
        }
        // If cursor is right after an identifier, use its type
        Expression::Identifier(ident) => {
            if offset >= ident.span.end() && offset <= ident.span.end() + 1 {
                type_map.get(ident.span).and_then(|ty| match ty {
                    InferredType::Known(n) => Some(ReceiverSide::Instance(n.clone())),
                    InferredType::Dynamic => None,
                })
            } else {
                None
            }
        }
        // If cursor is right after a class reference, use class-side methods
        Expression::ClassReference { name, span } => {
            if offset >= span.end() && offset <= span.end() + 1 {
                Some(ReceiverSide::Class(name.name.clone()))
            } else {
                None
            }
        }
        // If cursor is after a literal, use its type
        Expression::Literal(_, span) => {
            if offset >= span.end() && offset <= span.end() + 1 {
                type_map.get(*span).and_then(|ty| match ty {
                    InferredType::Known(n) => Some(ReceiverSide::Instance(n.clone())),
                    InferredType::Dynamic => None,
                })
            } else {
                None
            }
        }
        // Recurse into assignments
        Expression::Assignment { target, value, .. } => {
            find_receiver_in_expr(target, offset, type_map)
                .or_else(|| find_receiver_in_expr(value, offset, type_map))
        }
        // Recurse into parenthesized
        Expression::Parenthesized { expression, span } => {
            if offset >= span.end() && offset <= span.end() + 1 {
                type_map.get(*span).and_then(|ty| match ty {
                    InferredType::Known(n) => Some(ReceiverSide::Instance(n.clone())),
                    InferredType::Dynamic => None,
                })
            } else {
                find_receiver_in_expr(expression, offset, type_map)
            }
        }
        // Recurse into blocks
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|s| find_receiver_in_expr(&s.expression, offset, type_map)),
        _ => None,
    }
}

/// Adds completions filtered by a known receiver type.
///
/// Returns `true` if completions were added (caller should skip context-based fallback).
fn add_receiver_type_completions(
    hierarchy: &ClassHierarchy,
    receiver_type: Option<&ReceiverSide>,
    context: &ClassContext<'_>,
    completions: &mut Vec<Completion>,
) -> bool {
    let mut seen = HashSet::new();
    match receiver_type {
        Some(ReceiverSide::Instance(class_name)) if hierarchy.has_class(class_name) => {
            for method in hierarchy.all_methods(class_name) {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, false);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc)
                            .with_detail(method_completion_detail(&method, class_name)),
                    );
                }
            }
            true
        }
        Some(ReceiverSide::Class(class_name)) if hierarchy.has_class(class_name) => {
            for method in hierarchy.all_class_methods(class_name) {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, true);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc)
                            .with_detail(method_completion_detail(&method, class_name)),
                    );
                }
            }
            true
        }
        _ => false,
    }
}

/// Builds a detail string for a completion item showing type information.
///
/// Example: `on Counter -> Integer` or `on Integer (other: Number) -> Number`
fn method_completion_detail(
    method: &crate::semantic_analysis::class_hierarchy::MethodInfo,
    class_name: &str,
) -> String {
    let mut detail = format!("on {class_name}");

    // Add parameter types for keyword/binary methods
    let has_param_types = method.param_types.iter().any(Option::is_some);
    if has_param_types {
        let type_parts: Vec<String> = method
            .param_types
            .iter()
            .map(|t| t.as_deref().unwrap_or("Dynamic").to_string())
            .collect();
        let _ = write!(detail, " ({})", type_parts.join(", "));
    }

    if let Some(ref return_type) = method.return_type {
        let _ = write!(detail, " -> {return_type}");
    }
    detail
}

/// Returns the documentation string for a method completion.
///
/// Uses the generated `doc` string if present; otherwise falls back to the
/// `DefinedIn#selector` format used for user-written methods.
fn method_completion_doc(
    method: &crate::semantic_analysis::class_hierarchy::MethodInfo,
    class_side: bool,
) -> String {
    if let Some(doc) = &method.doc {
        return doc.to_string();
    }
    if class_side {
        format!("class {}#{}", method.defined_in, method.selector)
    } else {
        format!("{}#{}", method.defined_in, method.selector)
    }
}

/// Returns `true` if the selector `delegate` should be excluded from completions.
///
/// The sealed `delegate` method on `Actor` is only meaningful for native classes
/// (those with a `native:` declaration). Showing it on non-native actors would be
/// misleading since calling it raises an error at runtime (ADR 0056).
fn should_exclude_delegate(selector: &str, context: &ClassContext<'_>) -> bool {
    if selector != "delegate" {
        return false;
    }
    match context {
        ClassContext::InstanceMethod(class) | ClassContext::ClassMethod(class) => {
            class.backing_module.is_none()
        }
        ClassContext::TopLevel => true,
    }
}

/// Adds method completions from the class hierarchy.
///
/// Uses the cursor's class context to provide relevant completions:
/// - In an instance method: instance methods of the enclosing class
/// - In a class method: class-side methods of the enclosing class
/// - At top level: all methods from all module classes + Object methods
///
/// When `receiver_type` is `Some`, completions are filtered to only show methods
/// available on that type — instance methods for `Instance`, class-side methods
/// for `Class` (type-aware completions).
fn add_hierarchy_completions(
    module: &Module,
    hierarchy: &ClassHierarchy,
    context: &ClassContext<'_>,
    receiver_type: Option<&ReceiverSide>,
    completions: &mut Vec<Completion>,
) {
    // If we have a known receiver type, show only methods for that type
    if add_receiver_type_completions(hierarchy, receiver_type, context, completions) {
        return;
    }

    let mut seen = HashSet::new();

    // Fall back to context-based completions (Dynamic or no receiver)
    match context {
        ClassContext::InstanceMethod(class) => {
            // Add instance methods for the enclosing class (including inherited)
            let class_name = class.name.name.as_str();
            for method in hierarchy.all_methods(class_name) {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, false);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc),
                    );
                }
            }
            // Add state variable names as field completions
            for state_var in &class.state {
                completions.push(
                    Completion::new(state_var.name.name.as_str(), CompletionKind::Field)
                        .with_documentation(format!("Field: {}", state_var.name.name)),
                );
            }
        }
        ClassContext::ClassMethod(class) => {
            // Add class-side methods for the enclosing class (including inherited)
            let class_name = class.name.name.as_str();
            for method in hierarchy.all_class_methods(class_name) {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, true);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc),
                    );
                }
            }
            // Also include instance methods (class methods can access them via instances)
            for method in hierarchy.all_methods(class_name) {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, false);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc),
                    );
                }
            }
        }
        ClassContext::TopLevel => {
            // Add methods from all classes defined in the module
            for class in &module.classes {
                for method in hierarchy.all_methods(class.name.name.as_str()) {
                    if should_exclude_delegate(&method.selector, context) {
                        continue;
                    }
                    if seen.insert(method.selector.clone()) {
                        let doc = method_completion_doc(&method, false);
                        completions.push(
                            Completion::new(method.selector.as_str(), CompletionKind::Function)
                                .with_documentation(doc),
                        );
                    }
                }
                // Also add class-side methods for top-level completions
                for method in hierarchy.all_class_methods(class.name.name.as_str()) {
                    if should_exclude_delegate(&method.selector, context) {
                        continue;
                    }
                    if seen.insert(method.selector.clone()) {
                        let doc = method_completion_doc(&method, true);
                        completions.push(
                            Completion::new(method.selector.as_str(), CompletionKind::Function)
                                .with_documentation(doc),
                        );
                    }
                }
            }

            // Always include common Object/ProtoObject methods for general completions
            for method in hierarchy.all_methods("Object") {
                if should_exclude_delegate(&method.selector, context) {
                    continue;
                }
                if seen.insert(method.selector.clone()) {
                    let doc = method_completion_doc(&method, false);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc),
                    );
                }
            }
        }
    }
}

/// Removes duplicate completions based on label.
///
/// Keeps the first occurrence of each unique label.
fn deduplicate_completions(completions: &mut Vec<Completion>) {
    let mut seen = HashSet::new();
    completions.retain(|c| {
        // Check for existence before cloning
        if seen.contains(&c.label) {
            false
        } else {
            seen.insert(c.label.clone());
            true
        }
    });
}

/// Resolve the inferred type of an expression for REPL completion fallback (BT-1068).
///
/// Parses `source` as a Beamtalk expression, runs type inference using `hierarchy`,
/// and returns the class name of the last top-level expression's type.
///
/// Returns `None` when:
/// - No top-level expression was parsed (empty or only class/method definitions)
/// - The type cannot be statically determined (`Dynamic`)
///
/// Used by the `resolve_completion_type` compiler port command to handle complex
/// expressions that `tokenise_send_chain/1` cannot parse (parenthesised
/// subexpressions, binary message chains, keyword sends mid-chain).
#[must_use]
pub fn resolve_expression_type(source: &str, hierarchy: &ClassHierarchy) -> Option<String> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let last_expr = module.expressions.last()?;
    let span = last_expr.expression.span();
    let type_map = crate::semantic_analysis::infer_types(&module, hierarchy);
    match type_map.get(span) {
        Some(InferredType::Known(class_name)) => Some(class_name.to_string()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    //! Unit tests for code completion functionality.
    //!
    //! Tests verify that completions:
    //! - Include language keywords (self, super, true, false, nil)
    //! - Include identifiers from the current scope
    //! - Include common message selectors (at:, do:, size)
    //! - Deduplicate repeated identifiers
    //! - Handle edge cases (invalid positions, empty source, block parameters)
    //! - Provide appropriate documentation and completion kinds

    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    /// Parse source and compute completions with a fresh hierarchy.
    fn completions_at(source: &str, position: Position) -> Vec<Completion> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        compute_completions(&module, source, position, &hierarchy)
    }

    #[test]
    fn compute_completions_includes_keywords() {
        let completions = completions_at("x := 42", Position::new(0, 0));

        assert!(completions.iter().any(|c| c.label == "self"));
        assert!(completions.iter().any(|c| c.label == "true"));
        assert!(completions.iter().any(|c| c.label == "false"));
    }

    #[test]
    fn compute_completions_includes_identifiers() {
        let completions = completions_at("x := 42.\ny := x", Position::new(1, 0));

        assert!(completions.iter().any(|c| c.label == "x"));
        assert!(completions.iter().any(|c| c.label == "y"));
    }

    #[test]
    fn compute_completions_includes_messages() {
        let completions = completions_at("", Position::new(0, 0));

        // Should include Object/ProtoObject methods
        assert!(completions.iter().any(|c| c.label == "isNil"));
        assert!(completions.iter().any(|c| c.label == "class"));
        assert!(completions.iter().any(|c| c.label == "respondsTo:"));
    }

    #[test]
    fn compute_completions_no_duplicates() {
        let completions = completions_at("x := 1.\nx := 2", Position::new(0, 0));

        let x_count = completions.iter().filter(|c| c.label == "x").count();
        assert_eq!(x_count, 1);
    }

    #[test]
    fn compute_completions_with_invalid_position() {
        // Position beyond end of file
        let completions = completions_at("x := 42", Position::new(100, 100));

        // Should return empty vec for out-of-bounds position
        assert!(completions.is_empty());
    }

    #[test]
    fn compute_completions_with_block_expressions() {
        let completions = completions_at("block := [:x | x + 1]", Position::new(0, 0));

        // Should include both the block variable and parameter
        assert!(completions.iter().any(|c| c.label == "block"));
        assert!(completions.iter().any(|c| c.label == "x"));
    }

    #[test]
    fn compute_completions_with_message_sends() {
        let completions = completions_at("obj doSomething", Position::new(0, 0));

        // Should include the identifier
        assert!(completions.iter().any(|c| c.label == "obj"));
    }

    #[test]
    fn compute_completions_empty_source() {
        let completions = completions_at("", Position::new(0, 0));

        // Should still return keywords and common messages
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "self"));
    }

    #[test]
    fn keyword_completions_have_documentation() {
        let mut completions = Vec::new();
        add_keyword_completions(&mut completions);

        // All keywords should have documentation
        for completion in completions {
            if let CompletionKind::Keyword = completion.kind {
                assert!(completion.documentation.is_some());
            }
        }
    }

    #[test]
    fn hierarchy_completions_include_object_methods() {
        let completions = completions_at("", Position::new(0, 0));

        // Should include Object methods like isNil, class, respondsTo:
        assert!(completions.iter().any(|c| c.label == "isNil"));
        assert!(completions.iter().any(|c| c.label == "class"));
        assert!(completions.iter().any(|c| c.label == "respondsTo:"));

        // All method completions should have Function kind
        let method_completions: Vec<_> = completions
            .iter()
            .filter(|c| matches!(c.kind, CompletionKind::Function))
            .collect();
        assert!(!method_completions.is_empty());
        for completion in method_completions {
            assert!(completion.documentation.is_some());
        }
    }

    #[test]
    fn completions_include_inherited_methods_for_class_module() {
        // Parse a module with an Actor subclass
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let completions = compute_completions(&module, source, Position::new(0, 0), &hierarchy);

        // Should include inherited Actor methods (spawn)
        assert!(
            completions.iter().any(|c| c.label == "spawn"),
            "Should include inherited 'spawn' from Actor"
        );
        // Should include inherited Object methods (isNil)
        assert!(
            completions.iter().any(|c| c.label == "isNil"),
            "Should include inherited 'isNil' from Object"
        );
        // Should include inherited ProtoObject methods (class)
        assert!(
            completions.iter().any(|c| c.label == "class"),
            "Should include inherited 'class' from ProtoObject"
        );
    }

    #[test]
    fn completions_in_instance_method_include_instance_methods() {
        // Position inside the `increment` method body
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Position at the method body (after "=> ")
        let completions = compute_completions(&module, source, Position::new(3, 17), &hierarchy);

        // Should include the class's own method
        assert!(
            completions.iter().any(|c| c.label == "increment"),
            "Should include own method 'increment'"
        );
        // Should include inherited methods
        assert!(
            completions.iter().any(|c| c.label == "isNil"),
            "Should include inherited 'isNil' from Object"
        );
    }

    #[test]
    fn completions_in_instance_method_include_state_fields() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Position inside the method body
        let completions = compute_completions(&module, source, Position::new(3, 17), &hierarchy);

        // Should include state variable as field completion
        assert!(
            completions
                .iter()
                .any(|c| c.label == "count" && c.kind == CompletionKind::Field),
            "Should include state variable 'count' as Field"
        );
    }

    #[test]
    fn completions_include_class_names() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let completions = compute_completions(&module, source, Position::new(0, 0), &hierarchy);

        // Should include user-defined class
        assert!(
            completions
                .iter()
                .any(|c| c.label == "Counter" && c.kind == CompletionKind::Class),
            "Should include class name 'Counter'"
        );
        // Should include builtin classes
        assert!(
            completions
                .iter()
                .any(|c| c.label == "Integer" && c.kind == CompletionKind::Class),
            "Should include builtin class 'Integer'"
        );
    }

    #[test]
    fn completions_in_class_method_include_class_methods() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  class withInitial: n => self new: #{count => n}\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Position inside the class method body (line 3, after "=> ")
        let completions = compute_completions(&module, source, Position::new(3, 22), &hierarchy);

        // Should include the class method itself
        assert!(
            completions.iter().any(|c| c.label == "withInitial:"),
            "Should include class method 'withInitial:'"
        );
    }

    #[test]
    fn top_level_completions_include_class_side_methods() {
        // When a class has class-side methods, top-level completions should include them
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  class withInitial: n => self new: #{count => n}\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Position at top level (line 0 is part of class definition,
        // but completions at any position include all module methods)
        // Use Position(0, 0) which is before class keyword
        let completions = compute_completions(&module, source, Position::new(0, 0), &hierarchy);

        // Should include class-side methods from module classes
        assert!(
            completions.iter().any(|c| c.label == "withInitial:"),
            "Should include class-side method 'withInitial:'"
        );
        // Should also include instance methods
        assert!(
            completions.iter().any(|c| c.label == "increment"),
            "Should include instance method 'increment'"
        );
    }

    #[test]
    fn completions_filtered_by_receiver_type() {
        // When cursor is after a known-typed expression, completions should be filtered
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\n  value => self.count\n\nc := Counter spawn\nc ";
        let completions = completions_at(source, Position::new(8, 2));
        // Should include Counter's methods
        let has_increment = completions.iter().any(|c| c.label == "increment");
        let has_value = completions.iter().any(|c| c.label == "value");
        // If type-filtering works, increment and value should appear
        // (they're Counter instance methods)
        assert!(
            has_increment || has_value,
            "Should include Counter instance methods when receiver type is known. Got labels: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn completions_dynamic_type_shows_all_methods() {
        // For untyped variables, should fall back to showing all methods
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nx := foo\nx ";
        let completions = completions_at(source, Position::new(5, 2));
        // Should include methods from all classes (fall back behavior)
        let has_is_nil = completions.iter().any(|c| c.label == "isNil");
        assert!(
            has_is_nil,
            "Dynamic type should include Object methods (fallback). Got labels: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    // --- Erlang module completion tests ---

    #[test]
    fn erlang_class_reference_suggests_module_names() {
        // Cursor right after "Erlang " should suggest module names
        let source = "Erlang ";
        let completions = completions_at(source, Position::new(0, 7));

        assert!(
            completions.iter().any(|c| c.label == "lists"),
            "Should suggest 'lists' module. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "maps"),
            "Should suggest 'maps' module"
        );
        assert!(
            completions.iter().any(|c| c.label == "erlang"),
            "Should suggest 'erlang' module"
        );
        assert!(
            completions.iter().any(|c| c.label == "string"),
            "Should suggest 'string' module"
        );

        // All should have Module kind
        for c in &completions {
            assert_eq!(
                c.kind,
                CompletionKind::Module,
                "Erlang module completions should have Module kind"
            );
        }
    }

    #[test]
    fn erlang_module_suggests_exports() {
        // Cursor right after "Erlang lists " should suggest list exports
        let source = "Erlang lists ";
        let completions = completions_at(source, Position::new(0, 13));

        assert!(
            completions.iter().any(|c| c.label == "reverse:"),
            "Should suggest 'reverse:' from lists. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "map:with:"),
            "Should suggest 'map:with:' (map/2) from lists"
        );
        assert!(
            completions.iter().any(|c| c.label == "sort:"),
            "Should suggest 'sort:' (sort/1) from lists"
        );
        assert!(
            completions.iter().any(|c| c.label == "foldl:with:with:"),
            "Should suggest 'foldl:with:with:' (foldl/3) from lists"
        );

        // All should have Function kind
        for c in &completions {
            assert_eq!(
                c.kind,
                CompletionKind::Function,
                "Erlang export completions should have Function kind"
            );
        }
    }

    #[test]
    fn erlang_module_exports_have_detail() {
        let source = "Erlang lists ";
        let completions = completions_at(source, Position::new(0, 13));

        let reverse = completions.iter().find(|c| c.label == "reverse:");
        assert!(reverse.is_some(), "Should have reverse: completion");
        let reverse = reverse.unwrap();
        assert_eq!(
            reverse.detail.as_deref(),
            Some("lists:reverse/1"),
            "Detail should show Erlang function signature"
        );
    }

    #[test]
    fn erlang_unknown_module_returns_no_exports() {
        // Unknown module should not return export completions,
        // falls back to normal completions
        let source = "Erlang nonexistent_module ";
        let completions = completions_at(source, Position::new(0, 25));

        // Should not have any Erlang export completions
        // Instead should have normal completions (keywords, etc.)
        assert!(
            completions.iter().any(|c| c.label == "self"),
            "Should fall back to normal completions for unknown module. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn erlang_maps_module_exports() {
        let source = "Erlang maps ";
        let completions = completions_at(source, Position::new(0, 12));

        assert!(
            completions.iter().any(|c| c.label == "get:with:"),
            "Should suggest 'get:with:' (get/2) from maps. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "keys:"),
            "Should suggest 'keys:' from maps"
        );
        assert!(
            completions.iter().any(|c| c.label == "new"),
            "Should suggest 'new' (new/0) from maps"
        );
    }

    #[test]
    fn erlang_math_module_exports() {
        let source = "Erlang math ";
        let completions = completions_at(source, Position::new(0, 12));

        assert!(
            completions.iter().any(|c| c.label == "pi"),
            "Should suggest 'pi' (pi/0) from math. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "pow:with:"),
            "Should suggest 'pow:with:' (pow/2) from math"
        );
        assert!(
            completions.iter().any(|c| c.label == "sqrt:"),
            "Should suggest 'sqrt:' (sqrt/1) from math"
        );
    }

    #[test]
    fn erlang_completions_deduplicate_overloaded_selectors() {
        // lists has both sort/1 and sort/2 — the selector "sort:" should appear,
        // and "sort:with:" should also appear, but no duplicate "sort:"
        let source = "Erlang lists ";
        let completions = completions_at(source, Position::new(0, 13));

        let sort_count = completions.iter().filter(|c| c.label == "sort:").count();
        assert_eq!(sort_count, 1, "sort: should appear exactly once");
    }

    #[test]
    fn erlang_completions_in_assignment() {
        // Should work in assignment context too
        let source = "result := Erlang lists ";
        let completions = completions_at(source, Position::new(0, 23));

        assert!(
            completions.iter().any(|c| c.label == "reverse:"),
            "Should suggest exports after 'Erlang lists' in assignment. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn erlang_module_completions_have_documentation() {
        let source = "Erlang ";
        let completions = completions_at(source, Position::new(0, 7));

        let lists = completions.iter().find(|c| c.label == "lists");
        assert!(lists.is_some());
        assert!(
            lists.unwrap().documentation.is_some(),
            "Module completions should have documentation"
        );
    }

    #[test]
    fn detect_erlang_module_context_basic() {
        assert_eq!(detect_erlang_module_context("Erlang lists"), Some("lists"));
        assert_eq!(detect_erlang_module_context("Erlang maps"), Some("maps"));
        assert_eq!(
            detect_erlang_module_context("x := Erlang lists"),
            Some("lists")
        );
    }

    #[test]
    fn detect_erlang_module_context_no_match() {
        assert_eq!(detect_erlang_module_context("NotErlang lists"), None);
        assert_eq!(detect_erlang_module_context("Erlang"), None);
        assert_eq!(detect_erlang_module_context("FooErlang lists"), None);
        assert_eq!(detect_erlang_module_context("Foo_Erlang lists"), None);
    }

    // --- Chain resolution tests (BT-1014) ---
    //
    // Verify that completions are type-filtered when the receiver is a chained
    // message send whose return type is known (stdlib annotations from BT-1003,
    // user-defined method annotations, or inferred types from BT-1005).

    #[test]
    fn chain_resolution_stdlib_single_send_offers_typed_completions() {
        // `"hello" size ` → cursor after Integer result → should offer Integer methods
        // "hello" is 7 chars, space, size is 4 chars = 12 chars total. Cursor at 13.
        let source = r#""hello" size "#;
        let completions = completions_at(source, Position::new(0, 13));
        assert!(
            completions.iter().any(|c| c.label == "abs"),
            "Should offer Integer#abs after String#size chain. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "negated"),
            "Should offer Integer#negated after String#size chain. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn chain_resolution_stdlib_two_hop_chain_offers_typed_completions() {
        // `"hello" size abs ` → Integer#abs -> Integer → should offer Integer methods again
        // "hello" size abs = 16 chars, space at 16, cursor at 17
        let source = r#""hello" size abs "#;
        let completions = completions_at(source, Position::new(0, 17));
        assert!(
            completions.iter().any(|c| c.label == "negated"),
            "Should offer Integer methods after two-hop chain. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
        assert!(
            completions.iter().any(|c| c.label == "isEven"),
            "Should offer Integer#isEven after two-hop chain. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn chain_resolution_user_defined_annotated_method() {
        // A user-defined method with an explicit return type annotation:
        // `Object subclass: Box\n  value -> Integer => 42`
        // When completing after `(Box new) value `, completions should include Integer methods
        let source = "Object subclass: Box\n  value -> Integer => 42\n\nb := Box new\nb value ";
        // Line 4, col 8 is after "b value "
        let completions = completions_at(source, Position::new(4, 8));
        assert!(
            completions.iter().any(|c| c.label == "abs"),
            "Should offer Integer#abs for annotated method. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn chain_resolution_user_defined_inferred_method() {
        // A user-defined method whose return type is inferrable from its body (BT-1005):
        // `Object subclass: Box\n  value => 42`  ← body is Integer literal → inferred Integer
        // Completions after `b value ` should include Integer methods
        let source = "Object subclass: Box\n  value => 42\n\nb := Box new\nb value ";
        let completions = completions_at(source, Position::new(4, 8));
        assert!(
            completions.iter().any(|c| c.label == "abs"),
            "Should offer Integer#abs for inferred-return-type method. Got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn chain_resolution_detail_shows_receiver_class() {
        // Completions after `"hello" size ` should have detail showing "on Integer"
        let source = r#""hello" size "#;
        let completions = completions_at(source, Position::new(0, 13));
        let abs = completions.iter().find(|c| c.label == "abs");
        assert!(abs.is_some(), "Should include abs completion");
        let abs = abs.unwrap();
        assert!(
            abs.detail.as_deref().is_some_and(|d| d.contains("Integer")),
            "Detail should show receiver class Integer. Got: {:?}",
            abs.detail
        );
    }

    // --- Actor class vs instance method set tests (BT-1056) ---

    #[test]
    fn actor_instance_completions_exclude_spawn_methods() {
        // `c <TAB>` where c is an actor instance should NOT offer spawn/spawnWith: (class-side only)
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nc := Counter spawn\nc ";
        let completions = completions_at(source, Position::new(6, 2));
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(
            !labels.contains(&"spawn"),
            "Actor instance should NOT offer 'spawn'. Got: {labels:?}"
        );
        assert!(
            !labels.contains(&"spawnWith:"),
            "Actor instance should NOT offer 'spawnWith:'. Got: {labels:?}"
        );
        // Should still offer instance methods
        assert!(
            labels.contains(&"increment"),
            "Actor instance should offer 'increment'. Got: {labels:?}"
        );
    }

    #[test]
    fn counter_class_reference_completions_include_spawn() {
        // `Counter <TAB>` (ClassReference) should offer spawn/spawnWith:/new/new:
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nCounter ";
        let completions = completions_at(source, Position::new(5, 8));
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(
            labels.contains(&"spawn"),
            "Counter class-side should offer 'spawn'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"spawnWith:"),
            "Counter class-side should offer 'spawnWith:'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"new"),
            "Counter class-side should offer 'new'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"new:"),
            "Counter class-side should offer 'new:'. Got: {labels:?}"
        );
    }

    #[test]
    fn counter_class_message_completions_include_spawn() {
        // `Counter class <TAB>` should offer the same class-side methods as `Counter <TAB>`
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nCounter class ";
        let completions = completions_at(source, Position::new(5, 14));
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(
            labels.contains(&"spawn"),
            "Counter class <TAB> should offer 'spawn'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"spawnWith:"),
            "Counter class <TAB> should offer 'spawnWith:'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"new"),
            "Counter class <TAB> should offer 'new'. Got: {labels:?}"
        );
        // Should NOT offer instance methods via `class` keyword
        assert!(
            !labels.contains(&"increment"),
            "Counter class <TAB> should NOT offer instance method 'increment'. Got: {labels:?}"
        );
    }

    #[test]
    fn actor_instance_completions_include_instance_methods() {
        // `c <TAB>` should offer stop/kill/isAlive (Actor instance methods)
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nc := Counter spawn\nc ";
        let completions = completions_at(source, Position::new(6, 2));
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(
            labels.contains(&"stop"),
            "Actor instance should offer 'stop'. Got: {labels:?}"
        );
        assert!(
            labels.contains(&"isAlive"),
            "Actor instance should offer 'isAlive'. Got: {labels:?}"
        );
    }

    // --- resolve_expression_type tests (BT-1068) ---

    #[test]
    fn resolve_expression_type_string_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("\"hello\"", &hierarchy);
        assert_eq!(result.as_deref(), Some("String"));
    }

    #[test]
    fn resolve_expression_type_integer_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("42", &hierarchy);
        assert_eq!(result.as_deref(), Some("Integer"));
    }

    #[test]
    fn resolve_expression_type_parenthesized() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("(\"hello\")", &hierarchy);
        assert_eq!(result.as_deref(), Some("String"));
    }

    #[test]
    fn resolve_expression_type_binary_send() {
        // "foo" ++ "bar" should resolve to String (String#++: returns String)
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("\"foo\" ++ \"bar\"", &hierarchy);
        assert_eq!(result.as_deref(), Some("String"));
    }

    #[test]
    fn resolve_expression_type_parenthesized_binary_send() {
        // ("foo" ++ "bar") should resolve to String
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("(\"foo\" ++ \"bar\")", &hierarchy);
        assert_eq!(result.as_deref(), Some("String"));
    }

    #[test]
    fn resolve_expression_type_chained_sends() {
        // "hello" size — String#size returns Integer
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("\"hello\" size", &hierarchy);
        assert_eq!(result.as_deref(), Some("Integer"));
    }

    #[test]
    fn resolve_expression_type_empty_source() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("", &hierarchy);
        assert_eq!(result, None);
    }

    #[test]
    fn resolve_expression_type_unknown_variable() {
        // Bare variable with no bindings — type is Dynamic
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("x", &hierarchy);
        assert_eq!(result, None);
    }

    // --- BT-1070: parenthesised subexpression as receiver ---

    #[test]
    fn resolve_expression_type_parenthesized_unary_send() {
        // ("hello" size) — the result of String#size (Integer) wrapped in parens
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("(\"hello\" size)", &hierarchy);
        assert_eq!(result.as_deref(), Some("Integer"));
    }

    #[test]
    fn resolve_expression_type_parenthesized_unknown_variable() {
        // (myList size) — myList is untyped, graceful fallback
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("(myList size)", &hierarchy);
        assert_eq!(result, None);
    }

    // --- BT-1072: keyword sends mid-chain ---

    #[test]
    fn resolve_expression_type_keyword_send_collect_returns_array() {
        // #[1, 2, 3] collect: [:x | x * 2] — Array#collect: returns Array
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("#[1, 2, 3] collect: [:x | x * 2]", &hierarchy);
        assert_eq!(result.as_deref(), Some("Array"));
    }

    #[test]
    fn resolve_expression_type_keyword_send_inject_no_annotation_returns_none() {
        // #[1, 2, 3] inject:into: has no return type annotation — graceful fallback
        let hierarchy = ClassHierarchy::with_builtins();
        let result =
            resolve_expression_type("#[1, 2, 3] inject: 0 into: [:acc :x | acc + x]", &hierarchy);
        assert_eq!(result, None);
    }

    #[test]
    fn resolve_expression_type_keyword_send_untyped_receiver_returns_none() {
        // myList collect: [...] — myList is untyped (Dynamic), graceful fallback
        let hierarchy = ClassHierarchy::with_builtins();
        let result = resolve_expression_type("myList collect: [:x | x]", &hierarchy);
        assert_eq!(result, None);
    }

    // ── delegate completion filtering (BT-1215) ─────────────────────────────

    #[test]
    fn delegate_excluded_from_completions_in_non_native_actor() {
        // Inside an instance method of a non-native Actor subclass,
        // `delegate` should not appear in completions.
        // Define Actor with delegate method, then a non-native subclass.
        let source = "Object subclass: Actor\n  delegate => 1\nActor subclass: Counter\n  increment => self ";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let completions = compute_completions(&module, source, Position::new(3, 19), &hierarchy);
        assert!(
            !completions.iter().any(|c| c.label == "delegate"),
            "delegate should be excluded for non-native actors"
        );
    }

    #[test]
    fn delegate_included_in_completions_for_native_actor() {
        // Inside an instance method of a native Actor subclass,
        // `delegate` SHOULD appear in completions.
        let source = "Object subclass: Actor\n  delegate => 1\nActor subclass: Proc native: beamtalk_proc\n  run => self del";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        // Position inside the method body (on "del" partial text)
        let completions = compute_completions(&module, source, Position::new(3, 13), &hierarchy);
        assert!(
            completions.iter().any(|c| c.label == "delegate"),
            "delegate should be included for native actors"
        );
    }
}
