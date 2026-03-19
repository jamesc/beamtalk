// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-hierarchy-dependent validation checks.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators that require the `ClassHierarchy` to validate class relationships:
//! - Abstract class instantiation (BT-105)
//! - Actor `new` vs `spawn` usage (BT-563)
//! - Field name validation in `new:` maps (BT-563)
//! - Class variable access (BT-563)
//! - Stdlib name shadowing (BT-738)
//! - Value type slot assignment (BT-914)
//! - Cast on value types (BT-919)
//! - Value type `-> Nil` return annotations (BT-1052)
//! - Data keyword / class-kind mismatch errors (BT-1529, BT-1535)
//! - Object-kind `new`/`new:` usage errors (BT-1540)

use crate::ast::{
    ClassKind, DeclaredKeyword, Expression, Identifier, MessageSelector, MethodDefinition, Module,
};
use crate::ast_walker::{walk_expression, walk_module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

/// BT-105: Check for attempts to instantiate abstract classes.
///
/// Walks all expressions looking for `MessageSend` where the receiver is an
/// identifier matching an abstract class and the selector is an instantiation
/// method (`spawn`, `spawnWith:`, `new`, or `new:`).
pub(crate) fn check_abstract_instantiation(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_abstract_instantiation);
}

/// Returns true if the selector name is an instantiation method (spawn, new, etc.)
fn is_instantiation_selector(name: &str) -> bool {
    matches!(name, "spawn" | "spawnWith:" | "new" | "new:")
}

/// Visitor for abstract class instantiation checks (BT-105).
///
/// Called by `walk_expression` on each expression node; the walker handles
/// recursive traversal, so this function only inspects the current node.
fn visit_abstract_instantiation(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            span,
            ..
        } => {
            let receiver_name = match receiver.as_ref() {
                Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
                Expression::ClassReference { name, .. } => Some(name.name.as_str()),
                _ => None,
            };

            if let Some(name) = receiver_name {
                let selector_name = selector.name();

                if is_instantiation_selector(&selector_name) && hierarchy.is_abstract(name) {
                    diagnostics.push(Diagnostic::error(
                        format!("Cannot instantiate abstract class `{name}`. Subclass it first.",),
                        *span,
                    ));
                }
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            let receiver_name = match receiver.as_ref() {
                Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
                Expression::ClassReference { name, .. } => Some(name.name.as_str()),
                _ => None,
            };
            if let Some(name) = receiver_name {
                for msg in messages {
                    let sel = msg.selector.name();
                    if is_instantiation_selector(&sel) && hierarchy.is_abstract(name) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "Cannot instantiate abstract class `{name}`. Subclass it first.",
                            ),
                            msg.span,
                        ));
                    }
                }
            }
        }
        _ => {}
    }
}

// ── BT-563: Class-aware diagnostics ──────────────────────────────────────────

/// Extracts a class name from a receiver expression (`Identifier` or `ClassReference`).
fn receiver_class_name(receiver: &Expression) -> Option<&str> {
    match receiver {
        Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
        Expression::ClassReference { name, .. } => Some(name.name.as_str()),
        _ => None,
    }
}

/// Walks all expressions in a module, calling `visitor` on each node (pre-order).
///
/// This is a thin adapter over [`walk_module`] that threads the `hierarchy`
/// and `diagnostics` through a closure, allowing callers to pass a simple
/// `fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>)` visitor.
fn walk_module_with_hierarchy(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
    visitor: fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>),
) {
    walk_module(module, &mut |expr| visitor(expr, hierarchy, diagnostics));
}

/// BT-563 / BT-1524: Error when Actor subclasses use `new` or `new:` instead of `spawn`.
///
/// Promoted from warning to error in BT-1524: actors are process-based and must
/// use `spawn`/`spawnWith:` for instantiation. Using `new`/`new:` on an actor
/// class is always a bug.
pub(crate) fn check_actor_new_usage(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_actor_new);
}

fn visit_actor_new(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        span,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if (sel == "new" || sel == "new:")
                && class_name != "Actor"
                && hierarchy.is_actor_subclass(class_name)
            {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Actor subclass `{class_name}` must use `spawn` instead of `{sel}`"
                        ),
                        *span,
                    )
                    .with_hint("Actors are processes — use spawn/spawnWith: instead of new/new:")
                    .with_category(DiagnosticCategory::ActorNew),
                );
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if (sel == "new" || sel == "new:")
                    && class_name != "Actor"
                    && hierarchy.is_actor_subclass(class_name)
                {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Actor subclass `{class_name}` must use `spawn` instead of `{sel}`"
                            ),
                            msg.span,
                        )
                        .with_hint(
                            "Actors are processes — use spawn/spawnWith: instead of new/new:",
                        )
                        .with_category(DiagnosticCategory::ActorNew),
                    );
                }
            }
        }
    }
}

/// BT-1540: Error when Object-kind classes use `new` or `new:`.
///
/// Object-kind classes are class-method namespaces and should not be instantiated.
/// `new`/`new:` are only available on Value subclasses. Classes that define their
/// own class-side `new`/`new:` factory methods (e.g. `AtomicCounter`, `Ets`) are exempt.
pub(crate) fn check_object_new_usage(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_object_new);
}

/// Returns true if `class_name` with `selector` should trigger an Object-kind new error.
///
/// Only fires for classes known in the hierarchy (unknown classes get DNU instead).
fn is_object_new_error(class_name: &str, selector: &str, hierarchy: &ClassHierarchy) -> bool {
    (selector == "new" || selector == "new:")
        && !matches!(class_name, "self" | "super")
        && hierarchy.has_class(class_name)
        && hierarchy.resolve_class_kind(class_name) == ClassKind::Object
        && !hierarchy.has_own_class_method(class_name, selector)
}

fn visit_object_new(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        span,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if is_object_new_error(class_name, &sel, hierarchy) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Object-kind class `{class_name}` cannot be instantiated with `{sel}`"
                        ),
                        *span,
                    )
                    .with_hint(
                        "Object subclasses are not instantiable. Use `Value subclass:` for data types",
                    ),
                );
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if is_object_new_error(class_name, &sel, hierarchy) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Object-kind class `{class_name}` cannot be instantiated with `{sel}`"
                            ),
                            msg.span,
                        )
                        .with_hint(
                            "Object subclasses are not instantiable. Use `Value subclass:` for data types",
                        ),
                    );
                }
            }
        }
    }
}

/// BT-563: Validate field names in `ClassName new: #{field => value}`.
pub(crate) fn check_new_field_names(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_new_field_names);
}

fn visit_new_field_names(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if sel == "new:" || sel == "spawn:" {
                if let Some(Expression::MapLiteral { pairs, .. }) = arguments.first() {
                    let declared_state = hierarchy.all_state(class_name);
                    if !declared_state.is_empty() {
                        validate_map_field_names(pairs, class_name, &declared_state, diagnostics);
                    }
                }
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if sel == "new:" || sel == "spawn:" {
                    if let Some(Expression::MapLiteral { pairs, .. }) = msg.arguments.first() {
                        let declared_state = hierarchy.all_state(class_name);
                        if !declared_state.is_empty() {
                            validate_map_field_names(
                                pairs,
                                class_name,
                                &declared_state,
                                diagnostics,
                            );
                        }
                    }
                }
            }
        }
    }
}

/// Checks that symbol keys in a map literal match declared state fields.
fn validate_map_field_names(
    pairs: &[crate::ast::MapPair],
    class_name: &str,
    declared_state: &[EcoString],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for pair in pairs {
        if let Expression::Literal(crate::ast::Literal::Symbol(sym), sym_span) = &pair.key {
            if !declared_state.iter().any(|s| s.as_str() == sym.as_str()) {
                let fields: Vec<&str> = declared_state.iter().map(EcoString::as_str).collect();
                diagnostics.push(
                    Diagnostic::warning(
                        format!("Unknown field `{sym}` for class `{class_name}`"),
                        *sym_span,
                    )
                    .with_hint(format!("Declared fields: {}", fields.join(", "))),
                );
            }
        }
    }
}

/// BT-563: Warn on access to undeclared class variables.
pub(crate) fn check_class_variable_access(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_classvar_access);
}

fn visit_classvar_access(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if sel == "classState:" {
                if let Some(Expression::Literal(crate::ast::Literal::Symbol(var_name), _)) =
                    arguments.first()
                {
                    let class_vars = hierarchy.class_variable_names(class_name);
                    if hierarchy.has_class(class_name)
                        && !class_vars.iter().any(|cv| cv.as_str() == var_name.as_str())
                    {
                        let hint = if class_vars.is_empty() {
                            format!("`{class_name}` has no declared class variables")
                        } else {
                            let vars: Vec<&str> =
                                class_vars.iter().map(EcoString::as_str).collect();
                            format!("Declared class variables: {}", vars.join(", "))
                        };
                        diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "Undefined class variable `{var_name}` on class `{class_name}`"
                                ),
                                *span,
                            )
                            .with_hint(hint),
                        );
                    }
                }
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if sel == "classState:" {
                    if let Some(Expression::Literal(crate::ast::Literal::Symbol(var_name), _)) =
                        msg.arguments.first()
                    {
                        let class_vars = hierarchy.class_variable_names(class_name);
                        if hierarchy.has_class(class_name)
                            && !class_vars.iter().any(|cv| cv.as_str() == var_name.as_str())
                        {
                            let hint = if class_vars.is_empty() {
                                format!("`{class_name}` has no declared class variables")
                            } else {
                                let vars: Vec<&str> =
                                    class_vars.iter().map(EcoString::as_str).collect();
                                format!("Declared class variables: {}", vars.join(", "))
                            };
                            diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Undefined class variable `{var_name}` on class `{class_name}`"
                                    ),
                                    msg.span,
                                )
                                .with_hint(hint),
                            );
                        }
                    }
                }
            }
        }
    }
}

/// BT-738: Warn when a user-defined class name matches a stdlib built-in class name.
///
/// Stdlib class names are protected at runtime (via `update_class` in the runtime).
/// This compile-time warning surfaces the conflict earlier, before the BEAM module
/// even loads. Uses `ClassHierarchy::is_runtime_protected_class` which covers
/// only generated stdlib classes (from `lib/*.bt`) that have the `bt@stdlib@`
/// module prefix and are actually protected at runtime.
///
/// Since BT-813, `Future` and `FileHandle` are also stdlib classes with
/// `bt@stdlib@` prefixed modules, so they trigger shadowing warnings too.
///
/// This must NOT be called during stdlib compilation (`stdlib_mode = true`).
/// Call it alongside `validate_primitives`, guarded by `!options.stdlib_mode`.
pub fn check_stdlib_name_shadowing(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        let name = class.name.name.as_str();
        if ClassHierarchy::is_runtime_protected_class(name) {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Class name `{name}` conflicts with a stdlib class. \
                         Loading will fail because stdlib class names are protected."
                    ),
                    class.name.span,
                )
                .with_hint(format!(
                    "Choose a different name. `{name}` is a protected stdlib class name."
                )),
            );
        }
        // BT-1041: `Self` is reserved as a return type keyword
        if name == "Self" {
            diagnostics.push(
                Diagnostic::error(
                    "`Self` is reserved as a return type keyword and cannot be used as a class name"
                        .to_string(),
                    class.name.span,
                )
                .with_category(DiagnosticCategory::Type),
            );
        }
    }
}

// ── BT-914: Value type slot assignment validation ──────────────────────────────

/// BT-914: Reject `self.slot :=` in Value type methods; warn when it bypasses
/// an overridden `withSlot:` method in Actor methods.
///
/// Value types (ADR 0042) are immutable — direct slot assignment is a compile
/// error. Actors allow direct slot assignment but warn when a custom `with*:`
/// method exists in the hierarchy and would be bypassed.
pub(crate) fn check_value_slot_assignment(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let is_value = hierarchy.is_value_subclass(class_name);

        // BT-1533: TestCase subclasses are exempt from slot assignment checks
        // during the deprecation period. BT-1534 will migrate `self.slot :=`
        // to `self withSlot:` syntax in all test subclasses.
        if hierarchy.is_testcase_subclass(class_name) {
            continue;
        }

        // Check instance methods only — classState: slots on class methods are
        // separate and not subject to value-type immutability rules.
        for method in &class.methods {
            let method_selector = method.selector.name();
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| {
                    check_slot_assignment_at(
                        e,
                        class_name,
                        &method_selector,
                        is_value,
                        hierarchy,
                        diagnostics,
                    );
                });
            }
        }
    }

    // Also check standalone method definitions (Tonel-style: `Counter >> setX: v => ...`).
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue; // Class-side methods are not subject to value-type rules.
        }
        let class_name = standalone.class_name.name.as_str();
        let is_value = hierarchy.is_value_subclass(class_name);

        // BT-1533: TestCase subclasses exempt (see above).
        if hierarchy.is_testcase_subclass(class_name) {
            continue;
        }

        let method_selector = standalone.method.selector.name();
        for stmt in &standalone.method.body {
            walk_expression(&stmt.expression, &mut |e| {
                check_slot_assignment_at(
                    e,
                    class_name,
                    &method_selector,
                    is_value,
                    hierarchy,
                    diagnostics,
                );
            });
        }
    }
}

/// Capitalise the first character of a string (used to build `with<Slot>:` selectors).
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
    }
}

/// Checks a single `self.slot := value` node and reports diagnostics.
///
/// Reports an error for value-type classes and a warning for actor classes
/// that have an overriding `withSlot:` method in the hierarchy.
fn report_self_slot_assignment(
    slot_name: &str,
    span: crate::source_analysis::Span,
    class_name: &str,
    method_selector: &str,
    is_value: bool,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let with_selector = format!("with{}:", capitalize_first(slot_name));
    if is_value {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Cannot assign to slot `{slot_name}` on value type \
                     — use `self {with_selector} newValue` to create a new instance"
                ),
                span,
            )
            .with_hint(format!(
                "Value types are immutable. \
                 Use `self {with_selector}` to return a new instance with the updated slot."
            )),
        );
    } else if method_selector != with_selector
        && hierarchy.find_method(class_name, &with_selector).is_some()
    {
        // Actor: warn only when we are NOT inside the withSlot: method
        // itself, and the method exists in the hierarchy (user-defined).
        diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Direct slot assignment `self.{slot_name} :=` bypasses \
                     the `{with_selector}` method defined in the class hierarchy"
                ),
                span,
            )
            .with_hint(format!(
                "Consider using `self {with_selector} newValue` \
                 to go through the `{with_selector}` method."
            )),
        );
    }
}

/// Checks a single expression node for `self.slot :=` patterns.
///
/// Called via [`walk_expression`] from [`check_value_slot_assignment`]; the
/// walker handles recursive traversal, so this function only inspects the
/// current node.
fn check_slot_assignment_at(
    expr: &Expression,
    class_name: &str,
    method_selector: &str,
    is_value: bool,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::Assignment { target, span, .. } = expr {
        if let Expression::FieldAccess {
            receiver, field, ..
        } = target.as_ref()
        {
            if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self") {
                report_self_slot_assignment(
                    field.name.as_str(),
                    *span,
                    class_name,
                    method_selector,
                    is_value,
                    hierarchy,
                    diagnostics,
                );
            }
        }
    }
}

/// BT-919: Error when `!` (cast) is used on a statically-known value type.
///
/// Value types are not actors — they do not have a mailbox and cannot receive
/// asynchronous messages. Using `!` on a value type receiver is always wrong.
pub(crate) fn check_cast_on_value_type(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_cast_on_value_type);
}

fn visit_cast_on_value_type(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        is_cast,
        span,
        ..
    } = expr
    {
        if !is_cast {
            return;
        }
        if let Some(class_name) = receiver_class_name(receiver) {
            if hierarchy.has_class(class_name) && hierarchy.is_value_subclass(class_name) {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "Cannot use ! (cast) on value type `{class_name}`. Value types are not actors."
                    ),
                    *span,
                ));
            }
        }
    }
}

// ── BT-1052: Value type `-> Nil` return annotation ────────────────────────────

/// BT-1052: Error when an instance method on a Value type has an explicit `-> Nil`
/// return type annotation.
///
/// Value types (ADR 0042) are immutable transformations — methods should return
/// new values, not perform side effects. A `-> Nil` return type signals a
/// side-effecting void method, which contradicts value-type semantics.
///
/// Class-side methods and `subclassResponsibility` placeholders are exempt.
pub(crate) fn check_value_nil_return(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !hierarchy.is_value_subclass(class_name) {
            continue;
        }
        // BT-1533: TestCase is a Value subclass whose assertion methods
        // intentionally return Nil (side-effecting by design). Exempt
        // TestCase and its subclasses from the `-> Nil` lint.
        if hierarchy.is_testcase_subclass(class_name) {
            continue;
        }
        // Only check instance methods — class_methods are exempt.
        for method in &class.methods {
            check_method_nil_return(method, class_name, diagnostics);
        }
    }
    // Also check standalone method definitions (Tonel-style).
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if !hierarchy.is_value_subclass(class_name) {
            continue;
        }
        // BT-1533: Exempt TestCase and its subclasses (see above).
        if hierarchy.is_testcase_subclass(class_name) {
            continue;
        }
        check_method_nil_return(&standalone.method, class_name, diagnostics);
    }
}

/// Returns `true` if the method body contains a `self subclassResponsibility`
/// call anywhere (including inside `^ self subclassResponsibility`, cascades,
/// or other wrapped forms), marking it as an abstract placeholder that should
/// be exempt from the value `-> Nil` lint.
fn is_subclass_responsibility(method: &MethodDefinition) -> bool {
    let mut found = false;
    for stmt in &method.body {
        walk_expression(&stmt.expression, &mut |e| {
            if found {
                return;
            }
            if matches!(
                e,
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Unary(sel),
                    ..
                } if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                    && sel.as_str() == "subclassResponsibility"
            ) {
                found = true;
            }
        });
    }
    found
}

/// Check a single method for `-> Nil` on a value type and push an error diagnostic.
fn check_method_nil_return(
    method: &MethodDefinition,
    class_name: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(return_type) = &method.return_type else {
        return;
    };
    let crate::ast::TypeAnnotation::Simple(id) = return_type else {
        return;
    };
    if id.name.as_str() != "Nil" {
        return;
    }
    // Exempt abstract placeholder methods.
    if is_subclass_responsibility(method) {
        return;
    }
    // Exempt iteration/callback methods that accept a Block parameter.
    // These methods delegate side-effects to the block rather than performing
    // them directly on the Value — consistent with Value-type semantics.
    let has_block_param = method.parameters.iter().any(|p| {
        matches!(&p.type_annotation, Some(crate::ast::TypeAnnotation::Simple(id)) if id.name.as_str() == "Block")
    });
    if has_block_param {
        return;
    }
    let selector = method.selector.name();
    diagnostics.push(
        Diagnostic::error(
            format!(
                "method `{selector}` returns Nil on a Value type \
                 — Value types should not have side-effecting void methods; \
                 consider `Object subclass:` instead"
            ),
            method.span,
        )
        .with_hint(format!(
            "Value types (ADR 0042) are immutable transformations. \
             Move `{class_name}` to inherit from `Object` if it needs void methods."
        )),
    );
}

/// BT-1529/BT-1535: Enforce data declaration keywords matching the class kind.
///
/// Checks each class's `state:` / `field:` declarations against the resolved
/// `ClassKind` (via hierarchy propagation from BT-1528) and emits errors:
///
/// - `state:` on a Value subclass → "use 'field:' instead"
/// - `field:` on an Actor subclass → "use 'state:' instead"
/// - `state:` or `field:` on an Object subclass → "Object cannot have instance data"
///
/// Originally warnings (Phase 2, BT-1529), promoted to hard errors in Phase 4
/// (BT-1535) after all stdlib/test/example migrations were completed.
pub(crate) fn check_data_keyword_class_kind(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let resolved_kind = hierarchy.resolve_class_kind(class_name);

        // For Object kind, only error if the superclass is actually known in
        // the hierarchy. When the superclass is unknown (e.g. cross-file class
        // not loaded via pre_loaded_classes), resolve_class_kind defaults to
        // Object — producing false positives.
        if resolved_kind == ClassKind::Object {
            let superclass_known = class
                .superclass
                .as_ref()
                .is_some_and(|s| hierarchy.has_class(s.name.as_str()));
            if !superclass_known {
                continue;
            }
        }

        for decl in &class.state {
            check_keyword_for_kind(
                decl.declared_keyword,
                resolved_kind,
                class_name,
                decl.name.name.as_str(),
                decl.name.span,
                diagnostics,
            );
        }
    }
}

/// Checks a single data declaration keyword against the resolved class kind
/// and emits a compile error if they don't match.
fn check_keyword_for_kind(
    keyword: DeclaredKeyword,
    kind: ClassKind,
    class_name: &str,
    field_name: &str,
    span: crate::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match (kind, keyword) {
        // Correct combinations — no error
        (ClassKind::Value, DeclaredKeyword::Field) | (ClassKind::Actor, DeclaredKeyword::State) => {
        }

        // state: on Value → should be field:
        (ClassKind::Value, DeclaredKeyword::State) => {
            diagnostics.push(Diagnostic::error(
                format!(
                    "Use 'field:' for Value subclass `{class_name}` data declarations, \
                     not 'state:' — `{field_name}` is immutable"
                ),
                span,
            ));
        }

        // field: on Actor → should be state:
        (ClassKind::Actor, DeclaredKeyword::Field) => {
            diagnostics.push(Diagnostic::error(
                format!(
                    "Use 'state:' for Actor subclass `{class_name}` data declarations, \
                     not 'field:' — `{field_name}` is mutable process state"
                ),
                span,
            ));
        }

        // Any data keyword on Object → Object cannot have instance data
        (ClassKind::Object, _) => {
            diagnostics.push(Diagnostic::error(
                format!(
                    "Object subclass `{class_name}` cannot have instance data declarations; \
                     use 'Value subclass:' for immutable data or 'Actor subclass:' for mutable state"
                ),
                span,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    /// Future is a runtime-only built-in class (BT-1057 removed the stub).
    /// User-defined `Future` classes should trigger a stdlib shadowing warning.
    #[test]
    fn future_class_triggers_shadowing_warning() {
        let tokens = lex_with_eof("Object subclass: Future\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(module.classes.len(), 1);
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert!(
            !diagnostics.is_empty() && diagnostics[0].severity == Severity::Warning,
            "Expected warning for Future shadowing, got: {diagnostics:?}"
        );
    }

    /// BT-738: A class named `Integer` (generated stdlib) SHOULD trigger the warning.
    #[test]
    fn stdlib_class_triggers_shadowing_warning() {
        let tokens = lex_with_eof("Object subclass: Integer\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(module.classes.len(), 1);
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("Integer"));
    }

    /// BT-1041: A class named `Self` should be rejected.
    #[test]
    fn self_as_class_name_is_error() {
        let tokens = lex_with_eof("Object subclass: Self\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected error for class named Self, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("reserved"));
    }

    // ── BT-914: Value type slot assignment tests ──────────────────────────────

    /// `self.slot :=` inside a Value subclass method is a compile error.
    #[test]
    fn value_subclass_slot_assign_is_error() {
        let src = "Value subclass: Point\n  state: x = 0\n  setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for slot assignment in value type, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Error,
            "Expected error severity, got: {:?}",
            diagnostics[0].severity
        );
        assert!(
            diagnostics[0].message.contains("value type"),
            "Expected 'value type' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `self.slot :=` inside an Actor subclass method is allowed (no diagnostic).
    #[test]
    fn actor_subclass_slot_assign_is_allowed() {
        let src = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for actor slot assignment, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` in an Actor method warns when `withSlot:` is overridden in the hierarchy.
    #[test]
    fn actor_slot_assign_warns_when_with_selector_overridden() {
        // The class defines both `withValue:` (custom override) and a method that
        // directly assigns `self.value :=` — the validator should warn.
        let src = "Actor subclass: BankAccount\n  state: value = 0\n  withValue: v => v\n  setDirectly: v => self.value := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 bypass warning, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Warning,
            "Expected warning severity"
        );
        assert!(
            diagnostics[0].message.contains("bypasses"),
            "Expected 'bypasses' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Inside `withValue:` itself, `self.value :=` is NOT warned (it's the implementation).
    #[test]
    fn no_bypass_warning_inside_with_selector_method() {
        let src =
            "Actor subclass: BankAccount\n  state: value = 0\n  withValue: v => self.value := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning inside withValue: implementation, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` in an Object subclass (neither Value nor Actor) is allowed without warning.
    #[test]
    fn object_subclass_slot_assign_no_diagnostic() {
        let src = "Object subclass: Plain\n  state: x = 0\n  setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for Object subclass slot assignment, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` nested inside a block within a Value subclass method is still an error.
    /// `self.slot :=` nested inside a block within a Value subclass method is still an error.
    #[test]
    fn value_slot_assign_inside_block_is_error() {
        let src = "Value subclass: Point\n  state: x = 0\n  setX: v => [self.x := v] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for slot assignment inside block in value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Standalone method definition (`Point >> setX: v => self.x := v`) on a Value subclass
    /// must be caught by the `module.method_definitions` path.
    #[test]
    fn value_standalone_method_slot_assign_is_error() {
        // class declared separately; standalone method adds a mutating method to the value type
        let src = "Value subclass: Point\n  state: x = 0\nPoint >> setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(
            module.method_definitions.len(),
            1,
            "Expected 1 standalone method definition"
        );
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for standalone method slot assignment on value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    // BT-919: Cast (!) on value type tests

    #[test]
    fn cast_on_value_type_is_error() {
        // `Point` is a Value subclass, so `Point foo!` should produce an error
        let src = "Value subclass: Point\n  state: x = 0\n  doStuff => Point foo!";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_cast_on_value_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for cast on value type, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("Cannot use ! (cast) on value type"),
            "Expected value-type cast error, got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn cast_on_actor_type_is_ok() {
        // Actor subclass should NOT produce an error for cast
        let src = "Actor subclass: Worker\n  doStuff => Worker foo!";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_cast_on_value_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for cast on actor type, got: {diagnostics:?}"
        );
    }

    #[test]
    fn value_instance_method_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for -> Nil on value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("doSomething"),
            "Expected selector in message, got: {:?}",
            diagnostics[0].message
        );
    }

    /// Value class-side method with `-> Nil` is exempt.
    #[test]
    fn value_class_method_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  class create -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for class-side -> Nil, got: {diagnostics:?}"
        );
    }

    /// Object instance method with `-> Nil` is not flagged.
    #[test]
    fn object_instance_method_nil_return_is_exempt() {
        let src = "Object subclass: MyObj\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Object -> Nil, got: {diagnostics:?}"
        );
    }

    /// Actor instance method with `-> Nil` is not flagged.
    #[test]
    fn actor_instance_method_nil_return_is_exempt() {
        let src = "Actor subclass: MyActor\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Actor -> Nil, got: {diagnostics:?}"
        );
    }

    /// Value instance method with a Block parameter and `-> Nil` is exempt (iteration pattern).
    #[test]
    fn value_block_param_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  each: block :: Block -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Block-param -> Nil (iteration pattern), got: {diagnostics:?}"
        );
    }

    /// Value instance method WITHOUT a Block parameter and `-> Nil` is still an error.
    #[test]
    fn value_no_block_param_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  sideEffect -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected error for -> Nil without Block param, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Value instance method with `self subclassResponsibility` is exempt.
    #[test]
    fn value_subclass_responsibility_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => self subclassResponsibility";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for subclassResponsibility placeholder, got: {diagnostics:?}"
        );
    }

    /// `^ self subclassResponsibility` (early-return form) is also exempt.
    #[test]
    fn value_subclass_responsibility_early_return_is_exempt() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => ^ self subclassResponsibility";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for ^ self subclassResponsibility, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) Value method with `-> Nil` is also an error.
    #[test]
    fn standalone_value_method_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  doSomething => nil\nMyVal >> doVoid -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for standalone -> Nil on value type, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0].message.contains("doVoid"),
            "Expected selector in message, got: {:?}",
            diagnostics[0].message
        );
    }

    // --- BT-1535: Data keyword / class-kind mismatch errors (promoted from warnings) ---

    #[test]
    fn state_on_value_errors() {
        let src = "Value subclass: Point\n  state: x = 0\n  state: y = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 errors for state: on Value, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("field:"));
        assert!(diagnostics[0].message.contains("not 'state:'"));
    }

    #[test]
    fn field_on_actor_errors() {
        let src = "Actor subclass: Counter\n  field: count = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for field: on Actor, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("state:"));
        assert!(diagnostics[0].message.contains("not 'field:'"));
    }

    #[test]
    fn state_on_object_errors() {
        let src = "Object subclass: BadObj\n  state: x = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for state: on Object, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("cannot have instance data"));
    }

    #[test]
    fn field_on_object_errors() {
        let src = "Object subclass: BadObj\n  field: x = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for field: on Object, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("cannot have instance data"));
    }

    #[test]
    fn field_on_value_no_error() {
        let src = "Value subclass: Point\n  field: x = 0\n  field: y = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for field: on Value, got: {diagnostics:?}"
        );
    }

    #[test]
    fn state_on_actor_no_error() {
        let src = "Actor subclass: Counter\n  state: count = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for state: on Actor, got: {diagnostics:?}"
        );
    }

    #[test]
    fn indirect_value_subclass_state_errors() {
        // A class that inherits from a Value subclass should also get errors
        // when using state: instead of field:
        let src = "Value subclass: Shape\n  field: color = \"red\"\n\nShape subclass: Circle\n  state: radius = 0";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for state: on indirect Value subclass, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("Circle"));
        assert!(diagnostics[0].message.contains("field:"));
    }

    #[test]
    fn object_no_state_no_error() {
        // Object subclass with no data declarations should produce no errors
        let src = "Object subclass: Helper\n  class doStuff => 42";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for Object without data, got: {diagnostics:?}"
        );
    }

    #[test]
    fn testcase_subclass_state_errors() {
        // BT-1535: TestCase subclasses are no longer exempt (migration complete)
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\nTestCase subclass: MyTest\n  state: counter = nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_data_keyword_class_kind(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for state: on TestCase subclass, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].severity == Severity::Error);
        assert!(diagnostics[0].message.contains("field:"));
    }
}
