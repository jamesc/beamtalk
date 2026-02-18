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
use crate::queries::erlang_modules;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{ClassHierarchy, InferredType, infer_types};
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
/// let hierarchy = ClassHierarchy::build(&module).0;
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

    // Run type inference to get receiver types at positions
    let type_map = infer_types(module, hierarchy);

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
    let all_exprs = module.expressions.iter().chain(
        module
            .classes
            .iter()
            .flat_map(|c| {
                c.methods
                    .iter()
                    .chain(c.class_methods.iter())
                    .flat_map(|m| m.body.iter())
            })
            .chain(
                module
                    .method_definitions
                    .iter()
                    .flat_map(|smd| smd.method.body.iter()),
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
            .any(|e| has_erlang_class_ref_at(e, offset)),
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
    for expr in &module.expressions {
        collect_identifiers_from_expr(expr, &mut identifiers);
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
            for expr in &block.body {
                collect_identifiers_from_expr(expr, identifiers);
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
        .chain(module.classes.iter().flat_map(|c| {
            c.methods
                .iter()
                .chain(c.class_methods.iter())
                .flat_map(|m| m.body.iter())
        }))
        .chain(
            module
                .method_definitions
                .iter()
                .flat_map(|smd| smd.method.body.iter()),
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
            .find_map(|e| find_receiver_in_expr(e, offset, type_map)),
        _ => None,
    }
}

/// Adds completions filtered by a known receiver type.
///
/// Returns `true` if completions were added (caller should skip context-based fallback).
fn add_receiver_type_completions(
    hierarchy: &ClassHierarchy,
    receiver_type: Option<&ReceiverSide>,
    completions: &mut Vec<Completion>,
) -> bool {
    let mut seen = HashSet::new();
    match receiver_type {
        Some(ReceiverSide::Instance(class_name)) if hierarchy.has_class(class_name) => {
            for method in hierarchy.all_methods(class_name) {
                if seen.insert(method.selector.clone()) {
                    let doc = format!("{}#{}", method.defined_in, method.selector);
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
                if seen.insert(method.selector.clone()) {
                    let doc = format!("{}#{}", method.defined_in, method.selector);
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
    if add_receiver_type_completions(hierarchy, receiver_type, completions) {
        return;
    }

    let mut seen = HashSet::new();

    // Fall back to context-based completions (Dynamic or no receiver)
    match context {
        ClassContext::InstanceMethod(class) => {
            // Add instance methods for the enclosing class (including inherited)
            let class_name = class.name.name.as_str();
            for method in hierarchy.all_methods(class_name) {
                if seen.insert(method.selector.clone()) {
                    let doc = format!("{}#{}", method.defined_in, method.selector);
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
                if seen.insert(method.selector.clone()) {
                    let doc = format!("class {}#{}", method.defined_in, method.selector);
                    completions.push(
                        Completion::new(method.selector.as_str(), CompletionKind::Function)
                            .with_documentation(doc),
                    );
                }
            }
            // Also include instance methods (class methods can access them via instances)
            for method in hierarchy.all_methods(class_name) {
                if seen.insert(method.selector.clone()) {
                    let doc = format!("{}#{}", method.defined_in, method.selector);
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
                    if seen.insert(method.selector.clone()) {
                        let doc = format!("{}#{}", method.defined_in, method.selector);
                        completions.push(
                            Completion::new(method.selector.as_str(), CompletionKind::Function)
                                .with_documentation(doc),
                        );
                    }
                }
                // Also add class-side methods for top-level completions
                for method in hierarchy.all_class_methods(class.name.name.as_str()) {
                    if seen.insert(method.selector.clone()) {
                        let doc = format!("class {}#{}", method.defined_in, method.selector);
                        completions.push(
                            Completion::new(method.selector.as_str(), CompletionKind::Function)
                                .with_documentation(doc),
                        );
                    }
                }
            }

            // Always include common Object/ProtoObject methods for general completions
            for method in hierarchy.all_methods("Object") {
                if seen.insert(method.selector.clone()) {
                    let doc = format!("{}#{}", method.defined_in, method.selector);
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
        let hierarchy = ClassHierarchy::build(&module).0;
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
        let hierarchy = ClassHierarchy::build(&module).0;
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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
}
