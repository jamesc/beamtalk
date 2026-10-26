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
//! caller. "Module X sits below Y in the dependency graph" is not a reason
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
    /// [`CoreErlangGenerator::in_cell_closure`]: inside an actor closure, which
    /// has no threaded `State` (ADR 0137 §3).
    pub(super) in_cell_closure: bool,
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
            in_cell_closure: self.in_cell_closure(),
            class_var_names: self.class_var_names(),
            class_method_selectors: self.class_method_selectors(),
            class_name: self.class_name(),
            self_var_bound: self.lookup_var("self").is_some(),
        }
    }
}

/// True if `expr`'s static type is Character,
/// determined purely from its syntactic shape — no general static type
/// inference exists in codegen, so this recognizes exactly the syntactic
/// forms that `character.bt` declares as producing a Character: a Character
/// literal (`$A`), the class factory `Character value:`, and the two
/// instance methods with a `-> Character` return type, `uppercase` and
/// `lowercase` (applied recursively, since their own receiver must itself
/// be Character-typed — e.g. `$a uppercase lowercase`) — plus an
/// `ifTrue:ifFalse:` conditional whose outcome is statically Character (see
/// [`conditional_result_is_character_typed`]).
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
///
/// Casts (`!`) never match any arm: a cast always evaluates to `'ok'`.
pub(super) fn is_character_typed_receiver(expr: &Expression) -> bool {
    match expr.unwrap_parens() {
        Expression::Literal(Literal::Character(_), _) => true,
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast: false,
            ..
        } => {
            let is_value_factory_call = arguments.len() == 1
                && matches!(
                    selector,
                    MessageSelector::Keyword(parts)
                        if parts.len() == 1 && parts[0].keyword == "value:"
                )
                && matches!(
                    receiver.unwrap_parens(),
                    Expression::ClassReference { name, package: None, .. }
                        if name.name == "Character"
                );
            let is_character_returning_unary_send = arguments.is_empty()
                && matches!(
                    selector,
                    MessageSelector::Unary(name) if name == "uppercase" || name == "lowercase"
                )
                && is_character_typed_receiver(receiver);
            let is_character_typed_conditional = arguments.len() == 2
                && matches!(
                    selector.well_known(),
                    Some(WellKnownSelector::IfTrueIfFalse)
                )
                && conditional_result_is_character_typed(receiver, &arguments[0], &arguments[1]);
            is_value_factory_call
                || is_character_returning_unary_send
                || is_character_typed_conditional
        }
        _ => false,
    }
}

/// True if an `ifTrue:ifFalse:` send with this condition and these branch
/// arguments statically answers a Character (BT-4161).
///
/// A conditional answers exactly one branch's value (`True>>ifTrue:ifFalse:`
/// answers `trueBlock value`, `False>>ifTrue:ifFalse:` answers `falseBlock
/// value` — both pinned by
/// `true_false_if_true_if_false_bodies_match_codegen_recognizer`, which must
/// stay in sync with this function), so:
/// - a literal-`true`/`false` condition takes the statically known branch,
///   and only that branch's type matters — the untaken branch is never
///   evaluated, so its type is irrelevant;
/// - a non-constant condition takes an unknown branch, so both branches
///   must be Character-typed.
///
/// Each branch must be a block literal whose last body statement — the
/// block's answer — is itself Character-typed (recursively, so nested
/// conditionals and factory calls compose). Anything else (an empty block,
/// which answers `nil`; a non-block argument, whose `value` is dynamically
/// typed) fails closed to `false`, preserving today's Integer dispatch.
/// Single-branch `ifTrue:`/`ifFalse:` are deliberately not recognized: on
/// the untaken path they answer the receiver boolean (`True>>ifFalse:` and
/// `False>>ifTrue:` answer `self`), so only a constant-taken single-branch
/// send would ever qualify — that sliver is out of scope for BT-4161.
fn conditional_result_is_character_typed(
    condition: &Expression,
    true_branch: &Expression,
    false_branch: &Expression,
) -> bool {
    let true_typed = branch_value_is_character_typed(true_branch);
    let false_typed = branch_value_is_character_typed(false_branch);
    match condition.unwrap_parens().as_literal_bool() {
        Some(true) => true_typed,
        Some(false) => false_typed,
        None => true_typed && false_typed,
    }
}

/// True if a conditional branch argument — a block literal — answers a
/// Character: its last body statement's expression is Character-typed.
fn branch_value_is_character_typed(branch: &Expression) -> bool {
    match branch.unwrap_parens() {
        Expression::Block(block) => block
            .body
            .last()
            .is_some_and(|stmt| is_character_typed_receiver(&stmt.expression)),
        _ => false,
    }
}

/// This is used to detect state mutations that require threading through
/// control flow constructs.
pub(super) fn is_field_assignment(expr: &Expression) -> bool {
    field_assignment_name(expr).is_some()
}

/// BT-3488: the assigned field's name for exactly the shape
/// [`is_field_assignment`] accepts (`self.<field> := ...`), or `None`.
///
/// [`is_field_assignment`] is defined in terms of this function rather than
/// beside it, so the "is this a field write?" test and the "which field?"
/// answer can never disagree. A caller that needs the name — such as
/// `reject_unthreadable_value_self_field_write`, which puts it in a user-facing
/// diagnostic — would otherwise have to re-destructure the same shape by hand,
/// and a later widening of one copy (say, to accept an aliased `self`) would
/// silently turn a rejection into a no-op rather than a compile error
/// (CLAUDE.md's no-duplicate-implementations rule).
pub(super) fn field_assignment_name(expr: &Expression) -> Option<&str> {
    let Expression::Assignment { target, .. } = expr else {
        return None;
    };
    let Expression::FieldAccess { field, .. } = target.as_ref() else {
        return None;
    };
    // The `self.<field>` shape is the core rule's (`is_self_field_target`),
    // which `exception_producer_writes` reads for a field-only value-type
    // exception.
    beamtalk_core::semantic_analysis::block_facts::is_self_field_target(target)
        .then(|| field.name.as_str())
}

/// BT-3493: `Some` of the field write nested one level inside a local
/// assignment's RHS (`r := (self.x := ...)`, at any parenthesization depth)
/// when `rhs` — a `var := rhs` local assignment's own value — is itself
/// `self.field := ...`; `None` otherwise. The shared "see through the
/// value-carrying local-assign wrapper" rule every loop-body/branch-arm
/// local-var-assignment lowering needs identically:
/// `lower_local_var_assignment_bind`/`lower_direct_var_update_in_loop_bind`
/// (the field write must thread through the SAME real-`Bind` producer a
/// bare `self.field := ...` statement uses) and
/// `try_generate_block_local_plain_let`'s own bail-out to them (a plain,
/// unthreaded `let` has no way to fold the field's mutation in).
pub(super) fn local_assign_field_write(rhs: &Expression) -> Option<&Expression> {
    let inner = rhs.unwrap_parens();
    is_field_assignment(inner).then_some(inner)
}

/// The syntactic shape `self.classVar := value` in a class method.
fn class_var_assignment_shape(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if !ctx.in_class_method {
        return false;
    }
    let Expression::Assignment { target, .. } = expr else {
        return false;
    };
    let Expression::FieldAccess { field, .. } = target.as_ref() else {
        return false;
    };
    beamtalk_core::semantic_analysis::block_facts::is_self_field_target(target)
        && ctx.class_var_names.contains(field.name.as_str())
}

/// Checks if an expression is a self-send to a class method,
/// including an explicit same-class-name receiver (`ClassName
/// foo` from inside `ClassName`'s own class method dispatches exactly
/// like `self foo` — `try_handle_class_reference` routes both through
/// [`CoreErlangGenerator::generate_class_method_self_send`] identically).
///
/// ADR 0118 phase 5b: without the `ClassReference` shape here,
/// `subexpr_needs_prelude` is blind to it — a locally-declared
/// same-class-name self-send nested as a cascade/message argument would
/// compile as an opaque, self-contained value instead of an ordered
/// prelude (see `bt3406_cascade_nested_keyword_arg`).
fn class_method_self_send_shape(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if !ctx.in_class_method || ctx.class_method_selectors.is_empty() {
        return false;
    }
    let Expression::MessageSend {
        receiver, selector, ..
    } = expr
    else {
        return false;
    };
    if !(is_self_identifier(receiver) || is_own_class_reference(ctx, receiver)) {
        return false;
    }
    let sel_atom = selector.name().to_string();
    ctx.class_method_selectors.contains(&sel_atom)
}

/// Whether `expr` is an explicit, unqualified reference to the class whose
/// class method is being compiled (`C` inside `C class run => …`). Such a
/// receiver dispatches exactly like `self` there, for every selector shape
/// (unary, binary, keyword): the send is already running inside the class's
/// own `gen_server:call`, so it must become a direct class-side call
/// ([`CoreErlangGenerator::generate_class_method_self_send`]), never a
/// `class_send` back into that same `gen_server`.
pub(super) fn is_own_class_reference(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    ctx.in_class_method
        && beamtalk_core::semantic_analysis::block_facts::is_own_class_reference(
            expr,
            &ctx.class_name,
        )
}

/// Whether `expr` is the bare `self` identifier.
pub(super) fn is_self_identifier(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
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

/// Checks if an expression is a self-send in actor INSTANCE context.
/// These may mutate actor state and need state threading in loop bodies.
/// Excludes cast sends (`self method!`), which are fire-and-forget
/// and must not thread state through the loop accumulator.
///
/// BT-3581: also excludes a class method's own self-sends
/// (`ctx.in_class_method`) — a same-class self-send there routes through
/// `class_<selector>(ClassSelf, …)`
/// ([`CoreErlangGenerator::generate_class_method_self_send`]), never
/// `safe_dispatch` (which reads/threads the actor's own `State` — a
/// parameter that does not exist in a class method, and which would dispatch
/// through the class's own `gen_server` from inside a call it is already
/// synchronously handling, deadlocking). Before this fix, `ctx.context`
/// alone gated this: for an ACTOR subclass's own class method, `context` is
/// still `Actor`, so a same-class self-send inside a loop/conditional body
/// (`body.rs`) or an `on:do:`/`ensure:` construct
/// (`exception_handling.rs`'s E2 branch) was misrouted through
/// `safe_dispatch` — this codebase's other callers already re-derive the
/// same guard ad hoc (e.g. [`CoreErlangGenerator::in_actor_instance_context`]),
/// so folding it in here removes the one hand-listed exception rather than
/// adding a new special case.
pub(super) fn is_actor_self_send(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if ctx.context != CodeGenContext::Actor || ctx.in_class_method || ctx.in_cell_closure {
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
/// - Structurally lowered binary operators (`>>`, `**`, `++`, `%`, `==`, `=:=`,
///   `/=`, `=/=`); every other binary selector sent to `self` (`+`, `-`, `<`,
///   user-defined ones) IS a dispatching self-send (BT-3985,
///   [`binary_self_send_dispatches`])
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
        // BT-3985: a user-defined binary selector sent to `self` is a real
        // self-dispatch (its callee may write fields), not an Erlang operator.
        if let MessageSelector::Binary(op) = selector {
            return binary_self_send_dispatches(op);
        }
        return selector_dispatches_via_self(selector);
    }
    true
}

/// BT-3985: whether a binary selector sent to `self` in an actor instance
/// method is a user-defined message dispatched through the actor's own
/// self-dispatch (`safe_dispatch` + State bind), rather than an operator the
/// compiler lowers structurally (`operators::is_structurally_lowered_binary`,
/// the one list). An actor's `self` is never a number, so the
/// arithmetic/comparison operators always dispatch. The single predicate shared
/// by `is_dispatching_actor_self_send` and `generate_message_send`.
pub(super) fn binary_self_send_dispatches(op: &str) -> bool {
    !super::operators::is_structurally_lowered_binary(op)
}

/// BT-3985 / BT-4005: whether the binary send `receiver op …` is a send to
/// the dispatching `self` — taking `generate_message_send`'s `HANDLERS` path
/// (`try_handle_self_dispatch`, `try_handle_class_method_self_send`,
/// `try_handle_class_reference`) instead of the operator lowering
/// (`generate_binary_op`, whose non-number fallback is a runtime send that,
/// for the class object inside its own class method, re-enters the class's
/// own `gen_server` and deadlocks).
///
/// The receiver is the dispatching self when it is:
/// - bare `self` in an actor instance method or any class method, or
/// - an own-class reference ([`is_own_class_reference`]) in a class method,
///   which dispatches exactly like `self` there for every selector shape —
///   the unary/keyword shapes already reach `try_handle_class_reference`
///   unconditionally, so this is the one place the binary shape is admitted.
///
/// And the selector must be one that dispatches at all
/// ([`binary_self_send_dispatches`]): a structurally lowered operator stays
/// lowered whatever the receiver.
pub(super) fn binary_send_dispatches_to_self(
    ctx: &ShapeCtx<'_>,
    receiver: &Expression,
    op: &str,
) -> bool {
    if !binary_self_send_dispatches(op) {
        return false;
    }
    if is_self_identifier(receiver) {
        return ctx.in_class_method
            || (ctx.context == CodeGenContext::Actor && !ctx.in_cell_closure);
    }
    is_own_class_reference(ctx, receiver)
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
/// - Binary selectors — excluded here only for this function's own callers
///   (`cascade_self_dispatch_messages`: a `self` cascade holding a binary
///   message is not threaded message by message). A standalone binary send to `self` is decided by
///   [`binary_self_send_dispatches`] (and, receiver included,
///   [`binary_send_dispatches_to_self`]), not by this function: every
///   binary selector there other than a structurally lowered operator is a
///   real self-dispatch (BT-3985)
/// - `asType:` (compile-time erasure)
/// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
/// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
/// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
/// - Identity (`yourself`, `hash`)
/// - Error signaling (`error:`)
/// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
pub(super) fn selector_dispatches_via_self(selector: &MessageSelector) -> bool {
    // Binary selectors: excluded for cascades only — a standalone binary
    // self-send is decided by `binary_self_send_dispatches` (see the doc).
    if matches!(selector, MessageSelector::Binary(_)) {
        return false;
    }
    // Well-known selectors that the intrinsics
    // layer **unconditionally** handles before `try_handle_self_dispatch`.
    // Covers ProtoObject (`class`, `perform:`/`perform:withArguments:`/
    // `performLocally:withArguments:`), Object reflection (`respondsTo:`,
    // `fieldAt:`, `fieldAt:put:`, `fieldNames`, `hasField:`, `clearField:`
    // — ADR 0124 §1/B4), Nil protocol
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
                | WellKnownSelector::HasField
                | WellKnownSelector::ClearField
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
    ) || name.as_str() == super::intrinsics::PERFORM_WITH_ARGS_TIMEOUT
    {
        // `perform:withArguments:timeout:` — intercepted by name in
        // `try_generate_protoobject_message` (BT-3968).
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
    if ctx.context != CodeGenContext::Actor || ctx.in_cell_closure {
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

/// Checks if an expression is `self clearField: <name>` in actor **instance**
/// context (ADR 0124 §1/B4) — the arity-1 write counterpart to
/// [`is_self_field_at_put`], recognized identically (same receiver/context/
/// self-binding guard) but for `WellKnownSelector::ClearField`. Excludes a
/// class method (`ctx.in_class_method`): a `classState:` slot lives in the
/// class process (ADR 0130), not `State`.
pub(super) fn is_self_clear_field(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    if ctx.context != CodeGenContext::Actor || ctx.in_class_method || ctx.in_cell_closure {
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
            if id.name == "self"
                && !ctx.self_var_bound
                && matches!(selector.well_known(), Some(WellKnownSelector::ClearField))
                && arguments.len() == 1
            {
                return true;
            }
        }
    }
    false
}

/// A class-side expression with an in-place class-variable effect whose
/// evaluation order against its siblings must be pinned (ADR 0130): a
/// class-variable write or a same-class self-send, in a class method.
///
/// Nothing is threaded (a write is a `put`, a send rebinds nothing), but
/// Core Erlang leaves the evaluation order of call arguments and tuple/list
/// elements unspecified, and a class variable is now shared mutable state: an
/// argument that writes it, or sends a message that may, must run in source
/// order relative to the arguments around it. `subexpr_needs_prelude` and
/// `class_method_prelude_producer` hoist these into ordered `let`s.
pub(super) fn is_class_side_effect(ctx: &ShapeCtx<'_>, expr: &Expression) -> bool {
    class_var_assignment_shape(ctx, expr) || class_method_self_send_shape(ctx, expr)
}

impl CoreErlangGenerator {
    /// See [`is_class_side_effect`].
    pub(super) fn is_class_side_effect(&self, expr: &Expression) -> bool {
        is_class_side_effect(&self.shape_ctx(), expr)
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

    /// See [`is_self_clear_field`].
    pub(super) fn is_self_clear_field(&self, expr: &Expression) -> bool {
        is_self_clear_field(&self.shape_ctx(), expr)
    }

    /// See [`is_field_assignment`]. ADR 0130 §3: `false` in a class method,
    /// where `self.x := v` writes a class variable through a plain expression
    /// (`generate_field_assignment`) and is never a state-threaded producer.
    pub(super) fn is_field_assignment(&self, expr: &Expression) -> bool {
        !self.in_class_method() && !self.in_cell_closure() && is_field_assignment(expr)
    }

    /// See [`field_assignment_name`]; `None` in a class method (see
    /// [`Self::is_field_assignment`]).
    pub(super) fn field_assignment_name<'e>(&self, expr: &'e Expression) -> Option<&'e str> {
        if self.in_class_method() || self.in_cell_closure() {
            return None;
        }
        field_assignment_name(expr)
    }

    /// See [`local_assign_field_write`]; `None` in a class method (see
    /// [`Self::is_field_assignment`]).
    pub(super) fn local_assign_field_write<'e>(
        &self,
        rhs: &'e Expression,
    ) -> Option<&'e Expression> {
        if self.in_class_method() || self.in_cell_closure() {
            return None;
        }
        local_assign_field_write(rhs)
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
