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
use std::collections::{HashMap, HashSet};

/// Find the definition of a variable by name within a module (first assignment heuristic).
///
/// Walks the AST looking for the first `Assignment` where the target identifier
/// matches `name`. Returns the span of the target identifier if found.
#[must_use]
pub fn find_definition_in_module(module: &Module, name: &str) -> Option<Span> {
    for stmt in &module.expressions {
        if let Some(span) = find_definition_in_expr(&stmt.expression, name) {
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
/// 3. Protocol definition matching the identifier name in any indexed file —
///    but *only* when the hierarchy marks the name as a synthetic protocol
///    class entry (BT-1933). This prevents a preindexed real class (from
///    `with_stdlib` / `with_project_index`) from being shadowed by an open
///    protocol with the same name when the real class source file isn't
///    present in `files`.
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

    // 2. Class or protocol definition in the project index.
    //
    // BT-1933 registers protocol names as synthetic class entries, so
    // `has_class` returns true for both. We walk the indexed files once
    // looking for a real class declaration — that wins unconditionally.
    // Otherwise, fall back to a protocol match only when the hierarchy
    // explicitly marks the name as a protocol class (`is_protocol_class`).
    //
    // Built-in/stdlib classes can still have source files (e.g. stdlib/src/*.bt),
    // so we always search indexed files for a concrete declaration span.
    if project_index.hierarchy().has_class(name) {
        let mut protocol_match: Option<Location> = None;
        for (file_path, module) in files {
            for class in &module.classes {
                if class.name.name.as_str() == name {
                    return Some(Location::new(file_path.clone(), class.name.span));
                }
            }
            if protocol_match.is_none() {
                for protocol in &module.protocols {
                    if protocol.name.name.as_str() == name {
                        protocol_match = Some(Location::new(file_path.clone(), protocol.name.span));
                        break;
                    }
                }
            }
        }
        // Only return the protocol fallback when the hierarchy definitively
        // knows this name as a protocol. Otherwise, the hierarchy entry refers
        // to a real class whose source isn't open; returning None keeps the
        // behaviour consistent with class-only lookup (stdlib classes with no
        // open source file already return None).
        if let Some(location) = protocol_match {
            if project_index.hierarchy().is_protocol_class(name) {
                return Some(location);
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

/// Find the definition of a method selector scoped to the MRO starting at
/// `receiver_class`, without the cross-class global-search fallback used by
/// [`find_method_definition_cross_file_with_receiver`].
///
/// This is the scoped lookup needed for "Go to Definition on a method header"
/// (BT-1939): given the enclosing class's superclass, return the nearest
/// ancestor that defines the selector, or `None` if no ancestor does. The
/// global fallback present in the standard receiver-based lookup must be
/// disabled here, because it would otherwise navigate back to the current
/// class's own definition of the selector (or to an unrelated class that
/// happens to define the same name) — the exact opposite of "go to my
/// parent's method".
///
/// The walk visits `receiver_class` first and then its superclass chain.
/// Both class-body methods (`class.methods` / `class.class_methods`) and
/// Tonel-style standalone definitions (`module.method_definitions`) are
/// considered. Tonel-style definitions live on the module, not on the
/// `ClassDefinition`, so candidate spans are harvested by walking each
/// module's AST directly; the MRO walk itself still uses
/// [`ClassHierarchy::superclass_chain`] to produce the class ordering.
///
/// # Complexity (BT-1943)
///
/// This is a single-pass implementation: each file is walked *at most once*
/// per request, regardless of MRO length. We first collect a workspace-wide
/// `HashMap<class_name, Location>` by walking every module's classes and
/// standalone method definitions, filtering to class names present in the
/// MRO and selectors matching `selector` on the given side. Then we iterate
/// the MRO in order and return the first class with a recorded candidate,
/// preserving the strict nearest-ancestor semantics.
///
/// Cost: `O(Σ(|module.classes| + |module.method_definitions|))` AST node
/// visits, plus `O(|MRO|)` for the final lookup walk. This replaces the
/// earlier `O(|MRO| × |files|)` implementation, which re-walked every file
/// for every MRO class.
#[must_use]
pub fn find_overridden_method_definition<'a>(
    selector: &str,
    receiver_class: &ReceiverClassContext,
    project_index: &ProjectIndex,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<Location> {
    let hierarchy = project_index.hierarchy();

    // MRO walk order: the receiver class itself, then its ancestors. The
    // initial capacity covers the typical hierarchy depth (receiver → a
    // handful of user ancestors → Object → ProtoObject) without forcing
    // an immediate reallocation when `extend` appends the superclass chain.
    let mut mro: Vec<EcoString> = Vec::with_capacity(8);
    mro.push(receiver_class.class_name.clone());
    mro.extend(hierarchy.superclass_chain(receiver_class.class_name.as_str()));

    // Build a lookup set of MRO class names so each module walk can cheaply
    // filter out unrelated classes without any per-class linear search.
    let mro_set: HashSet<&str> = mro.iter().map(EcoString::as_str).collect();

    // Single pass over files: for each module, walk its classes and
    // standalone method definitions exactly once, recording the first span
    // we see for each MRO class that defines `selector` on the right side.
    // Cross-file first-wins matches the previous behaviour — the earlier
    // implementation also returned the first file in iteration order that
    // contained the match.
    //
    // Common-case short-circuit: the MRO starts with the receiver class
    // itself, so if any file contributes a match for that class, it's
    // automatically the best possible answer and no later file can beat it.
    // Return early to avoid an unnecessary full-workspace scan in the
    // typical "immediate parent defines the method" case. This preserves
    // the early-exit behaviour the pre-BT-1943 implementation had.
    let nearest = receiver_class.class_name.as_str();
    let mut candidates: HashMap<EcoString, Location> = HashMap::new();
    for (file_path, module) in files {
        collect_method_definitions_in_module(
            module,
            file_path,
            &mro_set,
            selector,
            receiver_class.class_side,
            &mut candidates,
        );
        if let Some(location) = candidates.remove(nearest) {
            return Some(location);
        }
    }

    // Walk the rest of the MRO in nearest-first order. We already handled
    // `mro[0]` (the receiver class itself) inside the loop above, so skip
    // it here — if it's not in `candidates` by this point, no file ever
    // contributed it and there's nothing more to do for that entry.
    //
    // Strict MRO-only: if none of the remaining ancestors defines the
    // selector either, return None rather than falling back to any other
    // class. We use `remove` (not `get`) to move the Location out of the
    // map without a clone — each MRO entry is visited at most once.
    for class_name in mro.iter().skip(1) {
        if let Some(location) = candidates.remove(class_name) {
            return Some(location);
        }
    }
    None
}

/// Walk a single module once, collecting the first matching `(class, span)`
/// pair for every class name in `mro_set` that defines `selector` on the
/// given side.
///
/// Populates `candidates` with `class_name → Location` entries. Existing
/// entries are NOT overwritten (first-file-wins across the workspace).
/// This is the building block for [`find_overridden_method_definition`]'s
/// single-pass behaviour: the caller invokes it once per file and then
/// walks the MRO against the accumulated map.
fn collect_method_definitions_in_module(
    module: &Module,
    file_path: &Utf8PathBuf,
    mro_set: &HashSet<&str>,
    selector: &str,
    class_side: bool,
    candidates: &mut HashMap<EcoString, Location>,
) {
    // Inline class-body methods. For each matching class in the MRO set,
    // scan the correct side (instance vs class methods) and record the
    // first matching selector.
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !mro_set.contains(class_name) {
            continue;
        }
        if candidates.contains_key(class_name) {
            continue;
        }
        let methods = if class_side {
            &class.class_methods
        } else {
            &class.methods
        };
        for method in methods {
            if method.selector.name() == selector {
                candidates.insert(
                    class.name.name.clone(),
                    Location::new(file_path.clone(), method.span),
                );
                break;
            }
        }
    }

    // Standalone (Tonel-style) method definitions: `Foo >> bar => ...`.
    // These are NOT attached to `class.methods`, so we look at them
    // separately. The existing `find_method_in_module` helper returns
    // `smd.span` (not `smd.method.span`); mirror that so navigation lands
    // in the same place it did before this optimization.
    for smd in &module.method_definitions {
        let class_name = smd.class_name.name.as_str();
        if !mro_set.contains(class_name) {
            continue;
        }
        if candidates.contains_key(class_name) {
            continue;
        }
        if smd.is_class_method != class_side {
            continue;
        }
        if smd.method.selector.name() == selector {
            candidates.insert(
                smd.class_name.name.clone(),
                Location::new(file_path.clone(), smd.span),
            );
        }
    }
}

/// Resolve the immediate superclass of the class enclosing `offset`, returning
/// a [`ReceiverClassContext`] suitable for passing to
/// [`find_overridden_method_definition`].
///
/// This powers "Go to Definition on a method header" (BT-1939): when the
/// cursor is on the selector in a method definition header, the desired
/// navigation target is the overridden parent method. Callers detect the
/// header click separately (via `offset_in_method_header_selector`) and then
/// use this helper to produce a receiver context pointing at the enclosing
/// class's immediate superclass. [`find_overridden_method_definition`] then
/// walks the MRO from that superclass upward to locate the nearest ancestor
/// that defines the selector, matching standard override semantics.
///
/// Returns `None` when:
/// - The offset is not inside any method definition (top-level code), or
/// - The enclosing class has no superclass (e.g. `ProtoObject`).
///
/// The `class_side` flag in the returned context mirrors the enclosing
/// method's side (instance-side or class-side), so a class-side method header
/// click correctly resolves to the parent's class-side method.
#[must_use]
pub fn resolve_enclosing_superclass_context(
    module: &Module,
    offset: u32,
    hierarchy: &ClassHierarchy,
) -> Option<ReceiverClassContext> {
    find_class_context(module, offset)
        .superclass_name(hierarchy)
        .map(|(class_name, class_side)| ReceiverClassContext {
            class_name,
            class_side,
        })
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
            ..
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
            .find_map(|s| find_selector_lookup_in_expr(&s.expression, offset)),
        Expression::Return { value, .. } => find_selector_lookup_in_expr(value, offset),
        Expression::Parenthesized { expression, .. } => {
            find_selector_lookup_in_expr(expression, offset)
        }
        Expression::FieldAccess { receiver, .. } => find_selector_lookup_in_expr(receiver, offset),
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

/// Info about a native delegate method's backing module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeDelegateInfo {
    /// The backing Erlang module name (e.g., `beamtalk_subprocess`).
    pub backing_module: EcoString,
}

/// Check if a location from `goto_definition` points to a self-delegate method
/// in a native class. If so, returns the backing module name.
///
/// This is used by the LSP to redirect navigation from the `.bt` method stub
/// to the backing `.erl` implementation file (ADR 0056).
#[must_use]
pub fn check_native_delegate<'a>(
    location: &Location,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<NativeDelegateInfo> {
    for (file_path, module) in files {
        if *file_path != location.file {
            continue;
        }
        // Check inline class methods
        for class in &module.classes {
            let backing_module = match &class.backing_module {
                Some(ident) => &ident.name,
                None => continue,
            };
            for method in &class.methods {
                if method.span == location.span && method.is_self_delegate() {
                    return Some(NativeDelegateInfo {
                        backing_module: backing_module.clone(),
                    });
                }
            }
        }
        // Check Tonel-style standalone method definitions (e.g., `MyNative >> sel => self delegate`)
        for smd in &module.method_definitions {
            if smd.method.span == location.span && smd.method.is_self_delegate() {
                // Find the backing module from the class definition in this or any file
                let class_name = smd.class_name.name.as_str();
                for class in &module.classes {
                    if class.name.name == class_name {
                        if let Some(ident) = &class.backing_module {
                            return Some(NativeDelegateInfo {
                                backing_module: ident.name.clone(),
                            });
                        }
                    }
                }
            }
        }
    }
    None
}

/// Info about an Erlang FFI call site for goto-definition.
///
/// Returned when the cursor is on the selector of an `Erlang <module> <function>:` call,
/// so the LSP can redirect navigation to the backing `.erl` source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FfiCallInfo {
    /// The Erlang module name (e.g., `lists`, `beamtalk_runtime`).
    pub module_name: EcoString,
    /// The Erlang function name (e.g., `reverse`).
    pub function_name: EcoString,
    /// Erlang arity of the function.
    pub arity: u8,
    /// Source line number (1-based) of the function definition in the `.erl` file.
    ///
    /// Populated from `FunctionSignature::line` when the registry is available.
    /// `None` if the registry has no line info or the function isn't found.
    pub line: Option<u32>,
}

/// Check if the cursor offset is on the selector of an Erlang FFI call.
///
/// Detects the pattern `Erlang <module> <function>:` where `<module>` is a
/// unary message to the `Erlang` class reference and `<function>:` is the
/// outer selector. Returns the module and function names for `.erl` lookup.
#[must_use]
pub fn check_ffi_call(module: &Module, offset: u32) -> Option<FfiCallInfo> {
    // Check top-level expressions
    for stmt in &module.expressions {
        if let Some(info) = find_ffi_call_in_expr(&stmt.expression, offset) {
            return Some(info);
        }
    }
    // Check class method bodies
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                if let Some(info) = find_ffi_call_in_expr(&stmt.expression, offset) {
                    return Some(info);
                }
            }
        }
    }
    // Check standalone method bodies
    for smd in &module.method_definitions {
        for stmt in &smd.method.body {
            if let Some(info) = find_ffi_call_in_expr(&stmt.expression, offset) {
                return Some(info);
            }
        }
    }
    None
}

/// Recursively search for an FFI call pattern at the given offset.
fn find_ffi_call_in_expr(expr: &Expression, offset: u32) -> Option<FfiCallInfo> {
    let span = expr.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }

    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            // First check arguments and receiver recursively
            for arg in arguments {
                if let Some(info) = find_ffi_call_in_expr(arg, offset) {
                    return Some(info);
                }
            }
            if let Some(info) = find_ffi_call_in_expr(receiver, offset) {
                return Some(info);
            }

            // Check if cursor is on the FFI selector or module name
            check_ffi_message_send(receiver, selector, arguments, span, offset)
        }
        Expression::Assignment { target, value, .. } => {
            find_ffi_call_in_expr(target, offset).or_else(|| find_ffi_call_in_expr(value, offset))
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|s| find_ffi_call_in_expr(&s.expression, offset)),
        Expression::Return { value, .. } => find_ffi_call_in_expr(value, offset),
        Expression::Parenthesized { expression, .. } => find_ffi_call_in_expr(expression, offset),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(info) = find_ffi_call_in_expr(receiver, offset) {
                return Some(info);
            }
            for msg in messages {
                for arg in &msg.arguments {
                    if let Some(info) = find_ffi_call_in_expr(arg, offset) {
                        return Some(info);
                    }
                }
            }
            None
        }
        Expression::FieldAccess { receiver, .. } => find_ffi_call_in_expr(receiver, offset),
        Expression::Match { value, arms, .. } => {
            if let Some(info) = find_ffi_call_in_expr(value, offset) {
                return Some(info);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    if let Some(info) = find_ffi_call_in_expr(guard, offset) {
                        return Some(info);
                    }
                }
                if let Some(info) = find_ffi_call_in_expr(&arm.body, offset) {
                    return Some(info);
                }
            }
            None
        }
        Expression::StringInterpolation { segments, .. } => segments.iter().find_map(|seg| {
            if let crate::ast::StringSegment::Interpolation(expr) = seg {
                find_ffi_call_in_expr(expr, offset)
            } else {
                None
            }
        }),
        Expression::ListLiteral { elements, tail, .. } => elements
            .iter()
            .find_map(|e| find_ffi_call_in_expr(e, offset))
            .or_else(|| tail.as_ref().and_then(|t| find_ffi_call_in_expr(t, offset))),
        Expression::MapLiteral { pairs, .. } => pairs.iter().find_map(|pair| {
            find_ffi_call_in_expr(&pair.key, offset)
                .or_else(|| find_ffi_call_in_expr(&pair.value, offset))
        }),
        _ => None,
    }
}

/// Check if a `MessageSend` is an FFI call with the cursor on the selector or module name.
///
/// Handles two patterns:
/// - Cursor on `reverse:` in `Erlang lists reverse: x` → returns module + function
/// - Cursor on `lists` in `Erlang lists` → returns module only
fn check_ffi_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    send_span: Span,
    offset: u32,
) -> Option<FfiCallInfo> {
    // Pattern 1: receiver is `Erlang <module>`, cursor is on the outer selector.
    // Use selector_span_contains for proper multi-keyword hit testing (e.g.,
    // `seq: 1 to: 10` — cursor on either `seq:` or `to:` should navigate).
    if let Expression::MessageSend {
        receiver: inner_receiver,
        selector: MessageSelector::Unary(module_name_str),
        ..
    } = receiver
    {
        if let Expression::ClassReference { name, .. } = inner_receiver.as_ref() {
            if name.name == "Erlang" {
                let computed_span =
                    selector_span_for_message_send(receiver.span(), arguments, send_span);
                if selector_span_contains(selector, offset, computed_span).is_some() {
                    let selector_name = selector.name();
                    let function_name = selector_name
                        .split(':')
                        .next()
                        .unwrap_or(selector_name.as_str());
                    let arity = u8::try_from(arguments.len()).unwrap_or(u8::MAX);
                    return Some(FfiCallInfo {
                        module_name: module_name_str.clone(),
                        function_name: EcoString::from(function_name),
                        arity,
                        line: None, // Populated by language service from registry
                    });
                }
            }
        }
    }

    // Pattern 2: receiver is `Erlang`, cursor is on the module name
    if let Expression::ClassReference { name, .. } = receiver {
        if name.name == "Erlang" {
            if let MessageSelector::Unary(module_name_str) = selector {
                let sel_start = receiver.span().end();
                let sel_end = send_span.end();
                if offset >= sel_start && offset < sel_end {
                    return Some(FfiCallInfo {
                        module_name: module_name_str.clone(),
                        function_name: EcoString::new(),
                        arity: 0,
                        line: None, // Module-level navigation, no specific function
                    });
                }
            }
        }
    }

    None
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
            .find_map(|s| find_definition_in_expr(&s.expression, name)),
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
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();

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
    fn cross_file_stdlib_class_with_source_loaded() {
        let file_collection = Utf8PathBuf::from("stdlib/src/Collection.bt");
        let module_collection = parse_source("abstract Object subclass: Collection");
        let hierarchy_collection = ClassHierarchy::build(&module_collection).0.unwrap();

        let file_set = Utf8PathBuf::from("stdlib/src/Set.bt");
        let module_set = parse_source("sealed Collection subclass: Set");
        let hierarchy_set = ClassHierarchy::build(&module_set).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file_collection.clone(), &hierarchy_collection);
        index.update_file(file_set.clone(), &hierarchy_set);

        let loc = find_definition_cross_file(
            "Collection",
            &file_set,
            &module_set,
            &index,
            [
                (&file_collection, &module_collection),
                (&file_set, &module_set),
            ],
        );

        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_collection);
    }

    #[test]
    fn cross_file_method_definition() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1\n  baz: x => x + 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();

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
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);

        let loc = find_method_definition_cross_file("nonexistent", &index, [(&file_a, &module_a)]);
        assert!(loc.is_none());
    }

    #[test]
    fn cross_file_method_definition_across_files() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new\nx bar");
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();

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
        let result = find_selector_in_expr(&module.expressions[0].expression, 4);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "at:put:");
    }

    #[test]
    fn find_selector_not_on_selector() {
        let source = "obj at: 1 put: 2";
        let module = parse_source(source);
        // Position 0 is on "obj" (receiver), not a selector
        let result = find_selector_in_expr(&module.expressions[0].expression, 0);
        assert!(result.is_none());
    }

    #[test]
    fn find_selector_in_unary_message() {
        let source = "obj size";
        let module = parse_source(source);
        // Position 4 is on/near "size"
        let result = find_selector_in_expr(&module.expressions[0].expression, 4);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "size");
    }

    #[test]
    fn find_selector_in_binary_message() {
        let source = "3 + 4";
        let module = parse_source(source);
        // Position 2 is on/near "+"
        let result = find_selector_in_expr(&module.expressions[0].expression, 2);
        assert!(result.is_some());
        let (name, _span) = result.unwrap();
        assert_eq!(name, "+");
    }

    #[test]
    fn check_native_delegate_returns_backing_module() {
        let file = Utf8PathBuf::from("native.bt");
        let module =
            parse_source("Actor subclass: Foo native: bar_mod\n  doStuff => self delegate");
        let method_span = module.classes[0].methods[0].span;

        let location = Location::new(file.clone(), method_span);
        let result = check_native_delegate(&location, [(&file, &module)]);
        assert!(result.is_some());
        assert_eq!(result.unwrap().backing_module, "bar_mod");
    }

    #[test]
    fn check_native_delegate_returns_none_for_non_native_class() {
        let file = Utf8PathBuf::from("normal.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 42");
        let method_span = module.classes[0].methods[0].span;

        let location = Location::new(file.clone(), method_span);
        let result = check_native_delegate(&location, [(&file, &module)]);
        assert!(result.is_none());
    }

    #[test]
    fn check_native_delegate_returns_none_for_non_delegate_method() {
        let file = Utf8PathBuf::from("native.bt");
        let module = parse_source(
            "Actor subclass: Foo native: bar_mod\n  doStuff => self delegate\n  helper => 42",
        );
        // helper method is at index 1 — not a self delegate
        let method_span = module.classes[0].methods[1].span;

        let location = Location::new(file.clone(), method_span);
        let result = check_native_delegate(&location, [(&file, &module)]);
        assert!(result.is_none());
    }

    // BT-1936: Goto-definition for protocol names.
    //
    // BT-1933 already registers protocol names as synthetic class entries in
    // `ClassHierarchy`, so `has_class("Printable")` returns true. These tests
    // verify that `find_definition_cross_file` now returns the protocol's
    // declaration span (from `module.protocols`) rather than falling through
    // to `None` after not finding a matching `ClassDefinition`.

    /// Build a `ClassHierarchy` for a module with protocols registered as
    /// synthetic class entries (mirrors the `update_file` call sites that
    /// pre-populate hierarchies in `language_service::SimpleLanguageService`).
    fn hierarchy_with_protocols(module: &Module) -> ClassHierarchy {
        let mut hierarchy = ClassHierarchy::build(module).0.unwrap();
        hierarchy.register_protocol_classes(module);
        hierarchy
    }

    #[test]
    fn goto_definition_protocol_same_file() {
        let file = Utf8PathBuf::from("protocols.bt");
        let module = parse_source("Protocol define: Printable\n  asString -> String\n");
        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy_with_protocols(&module));

        let loc =
            find_definition_cross_file("Printable", &file, &module, &index, [(&file, &module)]);
        let loc = loc.expect("goto-def should resolve to protocol declaration");
        assert_eq!(loc.file, file);
        assert_eq!(loc.span, module.protocols[0].name.span);
    }

    #[test]
    fn goto_definition_protocol_cross_file() {
        let file_a = Utf8PathBuf::from("Printable.bt");
        let module_a = parse_source("Protocol define: Printable\n  asString -> String\n");

        let file_b = Utf8PathBuf::from("User.bt");
        let module_b = parse_source("Object subclass: User\n  describe => self asString\n");

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_with_protocols(&module_a));
        index.update_file(file_b.clone(), &hierarchy_with_protocols(&module_b));

        let loc = find_definition_cross_file(
            "Printable",
            &file_b,
            &module_b,
            &index,
            [(&file_a, &module_a), (&file_b, &module_b)],
        );
        let loc = loc.expect("protocol defined in file_a should be found from file_b");
        assert_eq!(loc.file, file_a);
        assert_eq!(loc.span, module_a.protocols[0].name.span);
    }

    #[test]
    fn goto_definition_class_wins_over_protocol_on_name_collision() {
        // If a class and protocol share a name in different files, the real
        // class definition should win (parity with `register_protocol_classes`
        // precedence — see project_index.rs::real_class_not_overwritten_by_protocol_cross_file).
        let file_class = Utf8PathBuf::from("real_class.bt");
        let module_class = parse_source("Object subclass: Foo\n  bar => 1\n");

        let file_protocol = Utf8PathBuf::from("protocol_foo.bt");
        let module_protocol = parse_source("Protocol define: Foo\n  baz -> Integer\n");

        let mut index = ProjectIndex::new();
        index.update_file(file_class.clone(), &hierarchy_with_protocols(&module_class));
        index.update_file(
            file_protocol.clone(),
            &hierarchy_with_protocols(&module_protocol),
        );

        let loc = find_definition_cross_file(
            "Foo",
            &file_class,
            &module_class,
            &index,
            // Iterate with the protocol file first to prove class still wins.
            [
                (&file_protocol, &module_protocol),
                (&file_class, &module_class),
            ],
        );
        let loc = loc.expect("name collision should still resolve");
        assert_eq!(loc.file, file_class, "real class should win over protocol");
        assert_eq!(loc.span, module_class.classes[0].name.span);
    }

    #[test]
    fn goto_definition_unknown_protocol_returns_none() {
        let file = Utf8PathBuf::from("protocols.bt");
        let module = parse_source("Protocol define: Printable\n  asString -> String\n");
        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy_with_protocols(&module));

        let loc =
            find_definition_cross_file("Nonexistent", &file, &module, &index, [(&file, &module)]);
        assert!(loc.is_none());
    }

    #[test]
    fn goto_definition_prefers_preindexed_real_class_over_open_protocol() {
        // Regression for CodeRabbit finding on BT-1936:
        // A preindexed real class (e.g. from stdlib) whose source file is NOT
        // passed into `files` must not be shadowed by an open protocol that
        // happens to share a name. Returning `None` here is correct: we know a
        // real class exists but cannot produce a concrete location for it.
        //
        // Build a hierarchy that contains a real class `Foo` from a closed
        // file, then merge in an open protocol `Foo` from the user's file.
        let file_real_class = Utf8PathBuf::from("stdlib/src/Foo.bt");
        let module_real_class = parse_source("Object subclass: Foo\n  bar => 1\n");
        let hierarchy_real_class = ClassHierarchy::build(&module_real_class).0.unwrap();

        let file_protocol = Utf8PathBuf::from("user.bt");
        let module_protocol = parse_source("Protocol define: Foo\n  baz -> Integer\n");

        let mut index = ProjectIndex::new();
        // Step 1: index the real class file so the hierarchy knows Foo is a class.
        index.update_file(file_real_class.clone(), &hierarchy_real_class);
        // Step 2: index the protocol file — register_protocol_classes refuses to
        // overwrite the real class entry, so `is_protocol_class("Foo")` stays false.
        index.update_file(
            file_protocol.clone(),
            &hierarchy_with_protocols(&module_protocol),
        );

        // Only `file_protocol` is "open" — the real class source isn't in `files`.
        let loc = find_definition_cross_file(
            "Foo",
            &file_protocol,
            &module_protocol,
            &index,
            [(&file_protocol, &module_protocol)],
        );
        assert!(
            loc.is_none(),
            "open protocol must not shadow a preindexed real class, got {loc:?}"
        );
    }

    // ── BT-1943: single-pass find_overridden_method_definition ─────────────
    //
    // These tests exercise the optimized single-pass implementation. The key
    // invariants the rewrite must preserve are:
    //
    //   1. Strict MRO-only semantics — no fall back to unrelated classes that
    //      happen to define the same selector.
    //   2. Correct nearest-ancestor precedence when multiple ancestors define
    //      the selector (closer wins).
    //   3. No regressions on the single-class and single-file cases covered
    //      by the BT-1939 test suite above.
    //
    // The "deep MRO × many files" test is the worst case that prompted the
    // optimization: 100 files, each adding one link to a 100-class chain, and
    // a single matching method at the very top of the chain. With the old
    // implementation this required `O(|MRO| × |files|)` = 100 × 100 passes
    // over module ASTs; with the new implementation it's a single pass.

    #[test]
    fn find_overridden_strict_mro_only_ignores_unrelated_class_with_same_selector() {
        // Regression guard: ensures the rewrite preserves the BT-1939
        // strict-MRO-only invariant. Class `Sibling` is unrelated to
        // `Child`'s ancestry but defines the same selector — it must be
        // ignored even though the single-pass walk sees it.
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Parent\n\
             Object subclass: Sibling\n  greet => 99\n\
             Parent subclass: Child\n  greet => 2",
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy);

        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Parent"),
            class_side: false,
        };
        let loc = find_overridden_method_definition("greet", &receiver, &index, [(&file, &module)]);
        assert!(
            loc.is_none(),
            "Parent has no `greet`; Sibling must not leak through the single-pass walk, got {loc:?}"
        );
    }

    #[test]
    fn find_overridden_class_side_methods_resolve_via_class_side_branch() {
        // Locks the `class.class_methods` branch of
        // `collect_method_definitions_in_module`: with `class_side: true`,
        // the single-pass walk must look at class-side definitions only.
        // An instance-side method with the same selector must NOT leak
        // through (that would indicate the class-side filter is broken).
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Parent\n  class greet => 1\n  greet => 99\n\
             Parent subclass: Child\n  class greet => 2",
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy);

        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Parent"),
            class_side: true,
        };
        let loc = find_overridden_method_definition("greet", &receiver, &index, [(&file, &module)])
            .expect("Parent has a class-side greet");
        assert_eq!(loc.file, file);
        // The returned span must point at a class-side method, not at the
        // instance-side one with the same selector. Parent's class-side
        // `greet` is authored before its instance-side `greet`, so its span
        // should come first in the source. Assert explicitly that the
        // returned span matches the class-side method's recorded span.
        let parent = &module.classes[0];
        let class_side_span = parent
            .class_methods
            .iter()
            .find(|m| m.selector.name() == "greet")
            .map(|m| m.span)
            .expect("Parent has a class-side greet in the AST");
        assert_eq!(
            loc.span, class_side_span,
            "class_side=true must return the class-side method span"
        );
    }

    #[test]
    fn find_overridden_standalone_tonel_method_resolves() {
        // Locks the `module.method_definitions` branch of
        // `collect_method_definitions_in_module`: Tonel-style
        // `Parent >> greet => ...` lives on the module, not the class
        // definition. The single-pass walk must harvest its span from
        // `module.method_definitions` (not `class.methods`).
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Parent\n\
             Parent subclass: Child\n\
             Parent >> greet => 1\n\
             Child >> greet => 2",
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy);

        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Parent"),
            class_side: false,
        };
        let loc = find_overridden_method_definition("greet", &receiver, &index, [(&file, &module)])
            .expect("Parent has a standalone greet");
        assert_eq!(loc.file, file);
        // The returned span must match the standalone-definition span
        // (`smd.span`), not the inner `smd.method.span`, mirroring the
        // behaviour of `find_method_in_module`.
        let parent_standalone = module
            .method_definitions
            .iter()
            .find(|smd| smd.class_name.name == "Parent" && smd.method.selector.name() == "greet")
            .expect("Parent's standalone greet is in module.method_definitions");
        assert_eq!(
            loc.span, parent_standalone.span,
            "standalone lookup must return smd.span, not smd.method.span"
        );
    }

    #[test]
    fn find_overridden_nearest_ancestor_wins_over_further_ancestor() {
        // If multiple ancestors define the selector, the nearest one must
        // win. A single-pass implementation that accidentally iterates the
        // candidate map in hash order rather than MRO order would fail this.
        let source = "Object subclass: Grandparent\n  greet => 1\n\
                      Grandparent subclass: Parent\n  greet => 2\n\
                      Parent subclass: Child\n  greet => 3";
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(source);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let mut index = ProjectIndex::new();
        index.update_file(file.clone(), &hierarchy);

        // Receiver = Parent — the nearest ancestor of Parent defining `greet`
        // is Parent itself (MRO walks self first).
        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Parent"),
            class_side: false,
        };
        let loc = find_overridden_method_definition("greet", &receiver, &index, [(&file, &module)])
            .expect("Parent defines greet");
        assert_eq!(loc.file, file);
        // Parent's greet lives strictly after Grandparent's greet in source
        // order, so its span must start at or after Parent's class declaration
        // (and definitely after Grandparent's greet body).
        let parent_decl_offset = source.find("Grandparent subclass: Parent").unwrap();
        assert!(
            (loc.span.start() as usize) >= parent_decl_offset,
            "expected Parent's greet (≥ {parent_decl_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn find_overridden_deep_mro_many_files_returns_top_of_chain() {
        // BT-1943 worst case: deep MRO across many files. Build a chain
        // Class0 <- Class1 <- ... <- Class99, each in its own file, with
        // `greet` defined only on `Class0`. From `Class99`, the MRO walk
        // must traverse 100 classes and resolve to `Class0`.
        //
        // With the old O(|MRO| × |files|) implementation this would require
        // ~10,000 per-file scans; the single-pass implementation does it in
        // 100 file walks total. The correctness check here ensures the
        // rewrite still returns the right answer for the worst shape.
        const N: usize = 100;

        let mut files_modules: Vec<(Utf8PathBuf, Module)> = Vec::with_capacity(N);
        for i in 0..N {
            let source = if i == 0 {
                // Only Class0 defines the selector we're looking for.
                "Object subclass: Class0\n  greet => 0".to_string()
            } else {
                format!("Class{} subclass: Class{i}\n  other => {i}", i - 1)
            };
            let path = Utf8PathBuf::from(format!("class{i}.bt"));
            files_modules.push((path, parse_source(&source)));
        }

        // Build the merged hierarchy across every file so
        // `superclass_chain("Class99")` knows the full ancestry.
        let mut index = ProjectIndex::new();
        for (path, module) in &files_modules {
            let hierarchy = ClassHierarchy::build(module).0.unwrap();
            index.update_file(path.clone(), &hierarchy);
        }

        // Sanity check: the hierarchy really walks through every user class.
        // The chain also includes Object and ProtoObject above Class0, so its
        // length is >= N-1, and Class0 must appear in it.
        let chain = index.hierarchy().superclass_chain("Class99");
        assert!(
            chain.len() >= N - 1,
            "expected a ≥{}-deep superclass chain for Class99, got {}",
            N - 1,
            chain.len()
        );
        assert!(
            chain.iter().any(|c| c == "Class0"),
            "Class0 should appear in Class99's ancestry, got {chain:?}"
        );

        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Class99"),
            class_side: false,
        };
        let files_iter = files_modules.iter().map(|(p, m)| (p, m));
        let loc = find_overridden_method_definition("greet", &receiver, &index, files_iter)
            .expect("Class0 defines greet — the top of the MRO must be found");

        assert_eq!(
            loc.file,
            Utf8PathBuf::from("class0.bt"),
            "expected navigation to class0.bt (the only file defining `greet`)"
        );
    }

    #[test]
    #[ignore = "wall-clock timing — opt in via `cargo test -- --ignored` for \
                BT-1943 perf regression checks"]
    fn find_overridden_deep_mro_many_files_under_target_latency() {
        // BT-1943: verify the optimization actually meets the <10ms target
        // for the worst-case shape described above. Wall-clock assertions
        // are inherently noisy on shared CI runners, so this test is
        // `#[ignore]`d by default and only runs when opted in via
        // `cargo test -- --ignored`. Run it manually after any change to
        // `find_overridden_method_definition` or its helpers.
        //
        // Debug threshold: 250ms. That's ~25x the <10ms release-mode goal
        // stated on the issue, which is enough to catch a catastrophic
        // regression (e.g. someone accidentally reintroducing the quadratic
        // `O(|MRO| × |files|)` walk) without over-specifying the absolute
        // number. Release-mode measurements should comfortably fit 10ms.
        use std::time::Instant;

        const N: usize = 100;

        // Reuse the shape from the correctness test above. Parsing and
        // hierarchy construction run *before* the timed region so the
        // measurement reflects only the definition-lookup hot path — which
        // is what an LSP request actually pays on already-parsed modules.
        let mut files_modules: Vec<(Utf8PathBuf, Module)> = Vec::with_capacity(N);
        for i in 0..N {
            let source = if i == 0 {
                "Object subclass: Class0\n  greet => 0".to_string()
            } else {
                format!("Class{} subclass: Class{i}\n  other => {i}", i - 1)
            };
            let path = Utf8PathBuf::from(format!("class{i}.bt"));
            files_modules.push((path, parse_source(&source)));
        }

        let mut index = ProjectIndex::new();
        for (path, module) in &files_modules {
            let hierarchy = ClassHierarchy::build(module).0.unwrap();
            index.update_file(path.clone(), &hierarchy);
        }

        let receiver = ReceiverClassContext {
            class_name: EcoString::from("Class99"),
            class_side: false,
        };

        // The actual timed region: just the definition lookup, not the
        // index/parse setup. This matches what an LSP request pays on the
        // hot path.
        let start = Instant::now();
        let loc = find_overridden_method_definition(
            "greet",
            &receiver,
            &index,
            files_modules.iter().map(|(p, m)| (p, m)),
        );
        let elapsed = start.elapsed();

        assert!(loc.is_some(), "worst-case lookup should still succeed");
        assert!(
            elapsed.as_millis() < 250,
            "expected <250ms for 100 files × 100-deep MRO, got {}ms — \
             this indicates find_overridden_method_definition has regressed \
             to an O(|MRO|×|files|) walk",
            elapsed.as_millis()
        );
    }
}
