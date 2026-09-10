// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression-shape predicates.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Shared leaf module (see `architecture-principles.md` § Duplication &
//! the Shared-Leaf-Module Pattern): the `is_*` classifiers that recognize a
//! *syntactic* shape of an `Expression` — a field assignment, a class-var
//! assignment, a self-send that dispatches, a Character-typed receiver, and
//! so on. None of these are dispatch-specific; they previously lived in
//! `dispatch_codegen.rs` only because dispatch was the first (and widest)
//! caller — `is_class_var_assignment` alone has call sites in 8 other
//! modules. "Module X sits below Y in the dependency graph" is not a reason
//! to duplicate a classifier rather than extract it (CLAUDE.md).
//!
//! Each predicate is a plain function over `&Expression` (plus [`ShapeCtx`]
//! for the handful that need a borrowed slice of generator state —
//! `context`, `class_var_names`, and similar). [`CoreErlangGenerator`] keeps
//! a same-named, same-signature inherent method for every predicate that
//! previously took `&self`, forwarding straight to the free function via
//! [`CoreErlangGenerator::shape_ctx`] — so no call site elsewhere in the
//! crate needed to change.

use super::{CodeGenContext, CoreErlangGenerator};
use beamtalk_core::ast::{Expression, Literal, MessageSelector, WellKnownSelector};
use std::collections::HashSet;

/// The small borrowed slice of [`CoreErlangGenerator`] state a shape
/// predicate needs, so the predicate itself can be a pure function over
/// `&Expression` instead of taking the whole generator. Built fresh at each
/// call site via [`CoreErlangGenerator::shape_ctx`] — cheap (a handful of
/// `Copy`/borrowed fields plus one cloned class name), never stored.
pub(super) struct ShapeCtx<'a> {
    /// [`CoreErlangGenerator::context`] — actor vs. value-type vs. REPL.
    pub(super) context: CodeGenContext,
    /// [`CoreErlangGenerator::in_class_method`].
    pub(super) in_class_method: bool,
    /// [`CoreErlangGenerator::class_var_names`].
    pub(super) class_var_names: &'a HashSet<String>,
    /// [`CoreErlangGenerator::class_method_selectors`].
    pub(super) class_method_selectors: &'a HashSet<String>,
    /// [`CoreErlangGenerator::class_name`] — cloned since that accessor
    /// computes (rather than stores) the name.
    pub(super) class_name: String,
    /// Whether `"self"` is bound to a local var in the current scope
    /// (`lookup_var("self").is_some()`) — a shadowed `self` (e.g. a REPL
    /// binding) is not the receiver [`is_self_field_at_put`] means.
    pub(super) self_var_bound: bool,
}

impl CoreErlangGenerator {
    /// Builds the [`ShapeCtx`] every stateful shape predicate below needs.
    pub(super) fn shape_ctx(&self) -> ShapeCtx<'_> {
        ShapeCtx {
            context: self.context,
            in_class_method: self.in_class_method(),
            class_var_names: self.class_var_names(),
            class_method_selectors: self.class_method_selectors(),
            class_name: self.class_name(),
            self_var_bound: self.lookup_var("self").is_some(),
        }
    }
}

/// Strips any number of `Parenthesized` wrappers to expose the syntactic
/// shape underneath — `(expr)`, `((expr))`, etc. all see through to `expr`.
///
/// Parentheses carry no runtime meaning (they only affect parse-time
/// precedence), so any codegen specialization that pattern-matches on the
/// *syntactic shape* of an expression (as [`is_character_typed_receiver`]
/// does) must look past them or a receiver as simple as `(Character value:
/// 10) asString` — parenthesized only to disambiguate the keyword send from
/// the trailing unary `asString` — would silently miss the fast path.
fn unwrap_parens(expr: &Expression) -> &Expression {
    let mut current = expr;
    while let Expression::Parenthesized { expression, .. } = current {
        current = expression;
    }
    current
}

/// True if `expr`'s static type is Character,
/// determined purely from its syntactic shape — no general static type
/// inference exists in codegen, so this recognizes exactly the syntactic
/// forms that `character.bt` declares as producing a Character: a Character
/// literal (`$A`), the class factory `Character value:`, and the two
/// instance methods with a `-> Character` return type, `uppercase` and
/// `lowercase` (applied recursively, since their own receiver must itself
/// be Character-typed — e.g. `$a uppercase lowercase`).
///
/// This distinction matters because Character values are bare integers at
/// the BEAM level (`Character` is declared `Integer subclass:`), so the
/// runtime `beamtalk_primitive:class_of/1` and `module_for_value/1` both
/// match `is_integer/1` unconditionally and route to `Integer`'s BIF module
/// — they cannot tell a Character-tagged integer from a `SmallInteger`,
/// because there is no runtime tag to tell them apart. The literal case
/// (`$A asString`) is special-cased by the receiver's AST shape at codegen.
/// `(Character value: 10) asString` and `$a uppercase
/// asString` are the same problem: the receiver is statically Character
/// (per the sender's declared `-> Character` return type), so without
/// recognizing these additional shapes it would fall through to the generic
/// runtime-dispatch path and be misrouted to `Integer>>asString`,
/// producing `"10"` instead of a genuine 1-byte LF string. Recognizing
/// these additional shapes closes that gap without requiring general
/// static type inference in codegen.
pub(super) fn is_character_typed_receiver(expr: &Expression) -> bool {
    match unwrap_parens(expr) {
        Expression::Literal(Literal::Character(_), _) => true,
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let is_value_factory_call = arguments.len() == 1
                && matches!(
                    selector,
                    MessageSelector::Keyword(parts)
                        if parts.len() == 1 && parts[0].keyword == "value:"
                )
                && matches!(
                    unwrap_parens(receiver),
                    Expression::ClassReference { name, package: None, .. }
                        if name.name == "Character"
                );
            let is_character_returning_unary_send = arguments.is_empty()
                && matches!(
                    selector,
                    MessageSelector::Unary(name) if name == "uppercase" || name == "lowercase"
                )
                && is_character_typed_receiver(receiver);
            is_value_factory_call || is_character_returning_unary_send
        }
        _ => false,
    }
}

/// This is used to detect state mutations that require threading through
/// control flow constructs.
pub(super) fn is_field_assignment(expr: &Expression) -> bool {
    if let Expression::Assignment { target, .. } = expr {
        if let Expression::FieldAccess { receiver, .. } = target.as_ref() {
            if let Expression::Identifier(recv_id) = receiver.as_ref() {
                return recv_id.name == "self";
            }
        }
    }
    false
}

/// Checks if an expression is a self-field access (`self.field`).
///
/// Used to scope the runtime Tier 1/Tier 2 discrimination for block value
/// calls (`self.field value: ...`) to exactly the shape that needs it — a
/// block stored in an instance field, whose Tier-ness can't be known
/// statically since it may have been assigned from a different method.
pub(super) fn is_self_field_access(expr: &Expression) -> bool {
    if let Expression::FieldAccess { receiver, .. } = expr {
        if let Expression::Identifier(recv_id) = receiver.as_ref() {
            return recv_id.name == "self";
        }
    }
    false
}

/// Checks if an expression is a class variable assignment (`self.classVar := value`).
pub(super) fn is_class_var_assignment(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if !ctx.in_class_method {
        return false;
    }
    if let Expression::Assignment { target, .. } = expr {
        if let Expression::FieldAccess {
            receiver, field, ..
        } = target.as_ref()
        {
            if let Expression::Identifier(recv_id) = receiver.as_ref() {
                return recv_id.name == "self" && ctx.class_var_names.contains(field.name.as_str());
            }
        }
    }
    false
}

/// Checks if an expression is a self-send to a class method,
/// including an explicit same-class-name receiver (`ClassName
/// foo` from inside `ClassName`'s own class method dispatches exactly
/// like `self foo` — `try_handle_class_reference` routes both through
/// [`CoreErlangGenerator::generate_class_method_self_send`] identically).
/// These need special scoping in class method bodies because they may
/// update `ClassVars` via `let ClassVarsN = ... in` which must not be
/// wrapped.
///
/// ADR 0118 phase 5b: without the `ClassReference` shape here,
/// `subexpr_needs_prelude` is blind to it — a locally-declared
/// same-class-name self-send nested as a cascade/message argument
/// (`w add: … value: (CascadeNestedKeywordArg noop: 1)`) would compile
/// as an opaque, self-contained value instead of a real prelude, so the
/// `ClassVarsN` it introduces would never become visible to a LATER sibling
/// argument that also needs it (see `bt3406_cascade_nested_keyword_arg`).
pub(super) fn is_class_method_self_send(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if !ctx.in_class_method || ctx.class_method_selectors.is_empty() {
        return false;
    }
    let Expression::MessageSend {
        receiver, selector, ..
    } = expr
    else {
        return false;
    };
    let is_self_receiver =
        matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self");
    let is_own_class_reference = matches!(
        receiver.as_ref(),
        Expression::ClassReference { name, package, .. }
            if package.is_none() && name.name == ctx.class_name
    );
    if !(is_self_receiver || is_own_class_reference) {
        return false;
    }
    let sel_atom = selector.name().to_string();
    ctx.class_method_selectors.contains(&sel_atom)
}

/// Checks if an expression is a local variable assignment (`identifier := value`).
pub(super) fn is_local_var_assignment(expr: &Expression) -> bool {
    if let Expression::Assignment { target, .. } = expr {
        matches!(target.as_ref(), Expression::Identifier(_))
    } else {
        false
    }
}

/// Checks if an expression is a super message send (`super methodName:`).
pub(super) fn is_super_message_send(expr: &Expression) -> bool {
    if let Expression::MessageSend { receiver, .. } = expr {
        matches!(receiver.as_ref(), Expression::Super(_))
    } else {
        false
    }
}

/// Checks if an expression is a self-send in actor context.
/// These may mutate actor state and need state threading in loop bodies.
/// Excludes cast sends (`self method!`), which are fire-and-forget
/// and must not thread state through the loop accumulator.
pub(super) fn is_actor_self_send(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if ctx.context != CodeGenContext::Actor {
        return false;
    }
    if let Expression::MessageSend {
        receiver, is_cast, ..
    } = expr
    {
        if *is_cast {
            return false;
        }
        if let Expression::Identifier(id) = receiver.as_ref() {
            return id.name == "self";
        }
    }
    false
}

/// Checks if an expression is a self-send that goes through `safe_dispatch`
/// (or sealed dispatch) and returns `{reply, Result, NewState}`.
///
/// Excludes self-sends with selectors that are intercepted by handlers before
/// `try_handle_self_dispatch` in `generate_message_send`:
/// - Binary operators (`+`, `-`, `*`, etc.)
/// - `asType:` (compile-time erasure)
/// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
/// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
/// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
/// - Identity (`yourself`, `hash`)
/// - Error signaling (`error:`)
/// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
pub(super) fn is_dispatching_actor_self_send(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if !is_actor_self_send(ctx, expr) {
        return false;
    }
    if let Expression::MessageSend { selector, .. } = expr {
        return selector_dispatches_via_self(selector);
    }
    true
}

/// The selector half of [`is_dispatching_actor_self_send`]'s check —
/// extracted (ADR 0118 phase 1b) so a caller that already knows
/// the receiver is a bare `self` without owning an `Expression::MessageSend`
/// node to hand back (a cascade message, whose selector/arguments come from
/// `CascadeMessage` — see `util.rs`'s `cascade_self_dispatch_messages`) can
/// reuse the exact same rule instead of copying it (CLAUDE.md: no duplicate
/// implementations).
///
/// Excludes selectors that are intercepted by handlers before
/// `try_handle_self_dispatch` in `generate_message_send`:
/// - Binary operators (`+`, `-`, `*`, etc.)
/// - `asType:` (compile-time erasure)
/// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
/// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
/// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
/// - Identity (`yourself`, `hash`)
/// - Error signaling (`error:`)
/// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
pub(super) fn selector_dispatches_via_self(selector: &MessageSelector) -> bool {
    // Binary operators are always intercepted by generate_binary_op
    if matches!(selector, MessageSelector::Binary(_)) {
        return false;
    }
    // Well-known selectors that the intrinsics
    // layer **unconditionally** handles before `try_handle_self_dispatch`.
    // Covers ProtoObject (`class`, `perform:`/`perform:withArguments:`/
    // `performLocally:withArguments:`), Object reflection (`respondsTo:`,
    // `fieldAt:`, `fieldAt:put:`, `fieldNames`), Nil protocol
    // (`isNil`/`notNil`/`ifNil:`/`ifNotNil:`/`ifNil:ifNotNil:`/
    // `ifNotNil:ifNil:`), exception handling (`on:do:`, `ensure:`),
    // block application (`value`/`value:`/`value:value:`/
    // `value:value:value:`), block loops (`repeat`/`whileTrue:`/
    // `whileFalse:`), object identity (`hash`) and error signaling
    // (`error:`).
    //
    // NOTE: Boolean conditionals (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`)
    // are NOT included here — `try_generate_boolean_protocol` returns
    // `Ok(None)` (falls through) when no mutation-threading is needed,
    // allowing the send to reach self-dispatch.
    if let Some(wk) = selector.well_known() {
        if matches!(
            wk,
            WellKnownSelector::Class
                | WellKnownSelector::RespondsTo
                | WellKnownSelector::IsNil
                | WellKnownSelector::NotNil
                | WellKnownSelector::IfNil
                | WellKnownSelector::IfNotNil
                | WellKnownSelector::IfNilIfNotNil
                | WellKnownSelector::IfNotNilIfNil
                | WellKnownSelector::OnDo
                | WellKnownSelector::Value
                | WellKnownSelector::ValueColon
                | WellKnownSelector::ValueValue
                | WellKnownSelector::ValueValueValue
                | WellKnownSelector::WhileTrue
                | WellKnownSelector::WhileFalse
                | WellKnownSelector::Repeat
                | WellKnownSelector::Ensure
                | WellKnownSelector::Hash
                | WellKnownSelector::Error
                | WellKnownSelector::FieldAt
                | WellKnownSelector::FieldAtPut
                | WellKnownSelector::FieldNames
                | WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs
        ) {
            return false;
        }
    }
    // Remaining intrinsics not modelled as `WellKnownSelector` variants
    // — these are class-specific or compile-time-only constructs that
    // do not warrant universal selector classification.
    let name = selector.name();
    if matches!(
        name.as_str(),
        // asType: (compile-time erasure)
        "asType:"
        // Identity
        | "yourself"
    ) {
        return false;
    }
    true
}

/// Checks if an expression is an `error:` message send.
///
/// Since `erlang:error/1` never returns (always throws an exception),
/// expressions ending with `error:` should not be wrapped in reply tuples.
pub(super) fn is_error_message_send(expr: &Expression) -> bool {
    // Classify via the well-known enum so a future rename of the
    // `Error` variant forces this site to update too. The classifier
    // guarantees keyword/arity = 1, but we still gate on arguments.len()
    // for the same defensive reason the original predicate did.
    if let Expression::MessageSend {
        selector,
        arguments,
        ..
    } = expr
    {
        return matches!(selector.well_known(), Some(WellKnownSelector::Error))
            && arguments.len() == 1;
    }
    false
}

/// Checks if an expression is `self fieldAt: <name> put: <value>` in actor context.
/// These need state threading via maps:put, similar to field assignments.
pub(super) fn is_self_field_at_put(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if ctx.context != CodeGenContext::Actor {
        return false;
    }
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    {
        if let Expression::Identifier(id) = receiver.as_ref() {
            // Classify via the well-known enum. The classifier
            // already guarantees the two-part keyword shape; arguments.len()
            // is checked defensively for parser-shape consistency.
            if id.name == "self"
                && !ctx.self_var_bound
                && matches!(selector.well_known(), Some(WellKnownSelector::FieldAtPut))
                && arguments.len() == 2
            {
                return true;
            }
        }
    }
    false
}

impl CoreErlangGenerator {
    /// See [`is_class_var_assignment`].
    pub(super) fn is_class_var_assignment(&self, expr: &Expression) -> bool {
        is_class_var_assignment(&self.shape_ctx(), expr)
    }

    /// See [`is_class_method_self_send`].
    pub(super) fn is_class_method_self_send(&self, expr: &Expression) -> bool {
        is_class_method_self_send(&self.shape_ctx(), expr)
    }

    /// See [`is_actor_self_send`].
    pub(super) fn is_actor_self_send(&self, expr: &Expression) -> bool {
        is_actor_self_send(&self.shape_ctx(), expr)
    }

    /// See [`is_dispatching_actor_self_send`].
    pub(super) fn is_dispatching_actor_self_send(&self, expr: &Expression) -> bool {
        is_dispatching_actor_self_send(&self.shape_ctx(), expr)
    }

    /// See [`is_self_field_at_put`].
    pub(super) fn is_self_field_at_put(&self, expr: &Expression) -> bool {
        is_self_field_at_put(&self.shape_ctx(), expr)
    }

    /// See [`is_field_assignment`].
    pub(super) fn is_field_assignment(expr: &Expression) -> bool {
        is_field_assignment(expr)
    }

    /// See [`is_self_field_access`].
    pub(super) fn is_self_field_access(expr: &Expression) -> bool {
        is_self_field_access(expr)
    }

    /// See [`is_local_var_assignment`].
    pub(super) fn is_local_var_assignment(expr: &Expression) -> bool {
        is_local_var_assignment(expr)
    }

    /// See [`is_super_message_send`].
    pub(super) fn is_super_message_send(expr: &Expression) -> bool {
        is_super_message_send(expr)
    }

    /// See [`is_error_message_send`].
    pub(super) fn is_error_message_send(expr: &Expression) -> bool {
        is_error_message_send(expr)
    }

    /// See [`selector_dispatches_via_self`].
    pub(super) fn selector_dispatches_via_self(selector: &MessageSelector) -> bool {
        selector_dispatches_via_self(selector)
    }
}
