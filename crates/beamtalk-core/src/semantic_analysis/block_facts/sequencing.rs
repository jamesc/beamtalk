// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Operand sequencing and unsequenced-operand analysis (ADR 0131).
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! Which sub-expressions of a statement codegen sequences ahead of their parent
//! (operand sites, loop-body sites), and which operands of a send are left
//! unsequenced ([`UnsequencedOperand`]).

use super::{
    LocalThreadingFamily, conditional_arm_blocks, exception_arm_blocks, local_threading_construct,
    match_arms, producer_body_block,
};
use crate::ast::{BinarySegment, Block, Expression, ExpressionStatement, MessageSelector};
use crate::source_analysis::Span;

/// ADR 0131 Phase 2 (BT-3749): the sub-expressions of the method-body
/// statement `stmt` that codegen's sequencing rule (ADR 0118 §Decision 3,
/// `threaded_expression`) compiles ahead of their parent and splices into
/// the method body, so a local-threading producer there rebinds its locals
/// at the method root (ADR 0131 §2's `MethodBody` row).
///
/// From the value of `x := V` or `^V`, or from a statement that is a plain
/// send (not a local-threading construct, whose statement lowering compiles
/// its own operands): every receiver and non-block argument of a send that
/// is not a conditional, a cast, or a local-threading construct of another
/// family than a loop or fold (whose receivers and arguments their own
/// lowerings compile), and every element of a literal array, list or map.
/// A loop or fold reached this way is a site whose own operands are sites
/// too (a producer sequences them first; one that threads nothing compiles
/// them through ordinary expression codegen). The statement itself is not
/// a site (statement positions keep their own lowering), nor is anything
/// inside a block, a cascade, a destructuring,
/// a `match:` or an interpolation.
///
/// ADR 0131 Phase 2d (BT-3795): the value of a method-body field or
/// class-variable write `self.f := V` is a site too, and so are its operands
/// (`self.n := (xs collect: [...]) size`): the actor field-write arms and the
/// class method's class-variable write splice the producer's prelude before
/// the write. Not the value of a `^self.f := V`, nor of a write inside a block.
///
/// The Phase 0 allow-set check and codegen's producer both read this, so a
/// construct the diagnostic accepts in operand position is exactly one the
/// producer lowers.
///
/// A send whose statement lowering compiles its operands itself, without
/// sequencing them ([`operands_not_sequenced`]), is a site but its operands
/// are not: a producer there would leak its construct tuple.
#[must_use]
pub fn sequenced_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    operand_sites_of_statement(stmt, false)
}

/// BT-4287: every loop or fold ([`local_threading_construct`] of the `Loop` or
/// `Fold` family) among the operands of the statement `stmt`, at any depth,
/// without paren wrappers: a send's receiver and arguments (a cast's too), a
/// cascade's receiver and message arguments, a literal's elements, an
/// interpolated segment, a destructuring's or `match:`'s subject. Nothing
/// inside a block literal or a `match:` arm (those are statements of their own
/// frames), and not the statement's own value (the statement, or the value of
/// `x := V`, `self.f := V` or `^V`), which its statement lowering handles.
///
/// The superset of a statement's sites ([`sequenced_operand_sites`],
/// [`loop_body_producer_sites`]): a construct here that is not one of them
/// sits behind an operand the sequencing rule leaves to another lowering (a
/// conditional's receiver, an `operands_not_sequenced` send, a cast, a
/// cascade message, a lookup). Such a construct that writes an outer local
/// is the Phase 0 check's; one that threads only an actor's `State` is a
/// producer site of the frame that lowers the statement all the same
/// (BT-4310, codegen's `family_only_operand_site_exprs`), and each of those
/// lowerings sequences the operand that holds it.
#[must_use]
pub fn operand_loop_constructs(stmt: &Expression) -> Vec<&Expression> {
    operand_loop_constructs_with_holders(stmt)
        .into_iter()
        .map(|(construct, _)| construct)
        .collect()
}

/// [`operand_loop_constructs`], each with the kinds of the operand holders
/// the site walk stops at between the statement and the construct
/// ([`unsequenced_operand_holder`]; empty for a construct at a sequenced
/// site), outermost first.
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "one walk over every operand-holding expression kind, mirroring the site walk"
)]
pub fn operand_loop_constructs_with_holders(
    stmt: &Expression,
) -> Vec<(&Expression, Vec<UnsequencedOperand>)> {
    type Found<'a> = Vec<(&'a Expression, Vec<UnsequencedOperand>)>;
    // A wrapper the normalisation pass keeps (a literal element's) is walked
    // through by `children`'s `Parenthesized` arm, so `walk` pushes the
    // unwrapped construct, as the site lists do.
    fn walk<'a>(expr: &'a Expression, holders: &mut Vec<UnsequencedOperand>, out: &mut Found<'a>) {
        if !matches!(expr, Expression::Parenthesized { .. })
            && local_threading_construct(expr).is_some_and(|c| {
                matches!(
                    c.family,
                    LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                )
            })
        {
            out.push((expr, holders.clone()));
        }
        children(expr, holders, out);
    }
    fn children<'a>(
        expr: &'a Expression,
        holders: &mut Vec<UnsequencedOperand>,
        out: &mut Found<'a>,
    ) {
        let holder = if matches!(expr, Expression::Parenthesized { .. }) {
            None
        } else {
            unsequenced_operand_holder(expr)
        };
        if let Some(kind) = holder {
            holders.push(kind);
        }
        match expr {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                walk(receiver, holders, out);
                for arg in arguments {
                    walk(arg, holders, out);
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // The shared receiver is the cascade lowering's own: it binds
                // it once, unpacking a construct's carrier
                // (`cascade_operand_state_corpus`), so only what is inside it
                // and the messages' arguments are operands here.
                let shared = crate::ast_walker::cascade_shared_receiver(receiver);
                children(shared.unwrap_parens(), holders, out);
                if let Expression::MessageSend { arguments, .. } = receiver.as_ref() {
                    for arg in arguments {
                        walk(arg, holders, out);
                    }
                }
                for message in messages {
                    for arg in &message.arguments {
                        walk(arg, holders, out);
                    }
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    walk(e, holders, out);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    walk(e, holders, out);
                }
                if let Some(t) = tail {
                    walk(t, holders, out);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    walk(&pair.key, holders, out);
                    walk(&pair.value, holders, out);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        walk(e, holders, out);
                    }
                }
            }
            Expression::BinaryConstruction { segments, .. } => {
                for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    walk(e, holders, out);
                }
            }
            Expression::FieldAccess { receiver, .. } => walk(receiver, holders, out),
            Expression::Assignment { value, .. }
            | Expression::DestructureAssignment { value, .. }
            | Expression::Return { value, .. }
            | Expression::Match { value, .. } => walk(value, holders, out),
            Expression::Parenthesized { expression, .. } => walk(expression, holders, out),
            _ => {}
        }
        if holder.is_some() {
            holders.pop();
        }
    }
    // The statement's own value: the statement, or the value of `x := V`,
    // `self.f := V` or `^V`, seen through its parentheses.
    let (value, statement) = match stmt.unwrap_parens() {
        Expression::Assignment { target, value, .. }
            if matches!(target.as_ref(), Expression::Identifier(_))
                || is_self_field_target(target) =>
        {
            (value.as_ref(), false)
        }
        Expression::Return { value, .. } => (value.as_ref(), false),
        other => (other, true),
    };
    let value = value.unwrap_parens();
    let mut holders = Vec::new();
    // A method-body statement that is a construct compiles its own operands
    // (`sequenced_operand_sites`).
    if statement && local_threading_construct(value).is_some() {
        holders.push(UnsequencedOperand::ConstructStatement);
    }
    let mut out = Vec::new();
    children(value, &mut holders, &mut out);
    out
}

/// ADR 0131 Phase 2d (BT-3795): the sub-expressions of the REPL input
/// statement `stmt` where a loop or fold is a local-threading producer
/// (ADR 0131 §2's `MethodBody (REPL)` row): its [`sequenced_operand_sites`],
/// less the value of `x := V` / `^V` itself and every site of a `self.f := V`
/// write.
///
/// The REPL threads a loop or fold that is the statement, or the whole value
/// of an assignment, through its own lowering (the assembler unpacks the
/// `{Value, Bindings}` tuple and adopts the bindings map, ADR 0041). Only a
/// loop or fold nested in an operand (`(xs collect: [...]) size`,
/// `r := t + (...)`) had no lowering, so that is where the producer fires. A
/// field write has no REPL meaning, so it keeps its diagnostic.
///
/// BT-3913: the `eachWithIndex:`/`do:separatedBy:` exclusion is gone. It
/// dated from when those became producers for methods but the REPL root's
/// fold frames were unmeasured (BT-3841): the desugared separator
/// conditional failed the verifier there. The BT-4288 corpus proves those
/// rows compile and answer like a method on a BEAM, so the REPL accepts
/// them at operand sites like every other loop or fold.
///
/// The one rule the validator and codegen's REPL producer both read.
#[must_use]
pub fn repl_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    let inner = stmt.unwrap_parens();
    let whole_value = match inner {
        Expression::Assignment { target, value, .. } => {
            if is_self_field_target(target) {
                return Vec::new();
            }
            Some(value.unwrap_parens().span())
        }
        Expression::Return { value, .. } => Some(value.unwrap_parens().span()),
        _ => None,
    };
    sequenced_operand_sites(stmt)
        .into_iter()
        .filter(|site| Some(site.span()) != whole_value)
        .collect()
}

/// ADR 0131 Phase 2b (BT-3793): [`sequenced_operand_sites`] for a statement
/// of a loop or fold body, which codegen compiles through
/// `threaded_expression` (`thread_ahead`) first even when the statement is
/// itself a local-threading construct: its own receiver and non-block
/// arguments are then operand sites too (`(xs collect: [:x | t := t + x])
/// inject: 0 into: [:a :x | a + x]` as a statement of a body). A method-body
/// statement that is a construct keeps its own statement lowering instead,
/// which compiles those operands itself ([`sequenced_operand_sites`]).
#[must_use]
pub fn loop_body_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    operand_sites_of_statement(stmt, true)
}

/// ADR 0131 Phase 2b (BT-3793): the sub-expressions of `stmt`, a statement of
/// a loop or fold body, where a local-threading producer rebinds its locals
/// in that body's frame: the statement itself when it is a loop or fold
/// ([`producer_body_block`]), and its [`loop_body_operand_sites`]. The one
/// rule the Phase 0 allow-set check and codegen's body lowerings
/// (`with_body_producer_sites`) both read, so a construct the diagnostic
/// accepts in a loop body is exactly one the producer lowers.
#[must_use]
pub fn loop_body_producer_sites(stmt: &Expression) -> Vec<&Expression> {
    let mut sites = loop_body_operand_sites(stmt);
    let inner = stmt.unwrap_parens();
    // (A conditional statement is a site too, but only in an arm frame:
    // [`is_loop_body_producer_site`] drops it in a loop or fold body.)
    if producer_body_block(inner).is_some()
        || conditional_arm_blocks(inner).is_some()
        || exception_arm_blocks(inner).is_some()
        || match_arms(inner).is_some()
    {
        sites.push(inner);
    }
    sites
}

/// ADR 0131 Phase 2b (BT-3793/BT-3797): whether `site`, one of a loop or
/// fold body statement's [`loop_body_producer_sites`], carries a value (an
/// assignment's value or an operand) rather than being the statement
/// itself.
#[must_use]
pub fn is_value_bearing_site(statement: Span, site: &Expression) -> bool {
    site.unwrap_parens().span() != statement
}

/// What the loop-body site rule ([`is_loop_body_producer_site`]) reads off the
/// current statement of a loop or fold body and the frames around it.
#[derive(Debug, Clone, Copy, Default)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "one flag per fact the site rule reads"
)]
pub struct LoopBodyContext {
    /// The statement, without parentheses.
    pub statement: Span,
    /// The statement's whole value: the value of `x := V` or `^V`, without
    /// parentheses, else the statement itself.
    pub value: Span,
    /// Whether the innermost enclosing frame is a fold's
    /// ([`is_fold_frame_body`]).
    pub innermost_fold: bool,
    /// Whether an enclosing frame other than the innermost one is a fold's.
    pub outer_fold: bool,
    /// Whether the method is an actor instance method.
    pub in_actor: bool,
    /// ADR 0131 Phase 3 (BT-3738): whether the innermost enclosing frame is
    /// an arm of a conditional that is a local-threading producer
    /// (`BranchArm`, [`conditional_arms_lower_as_producer`](super::conditional_arms_lower_as_producer)) rather than a
    /// loop or fold body. A nested conditional is a producer in an arm frame,
    /// not in a loop body.
    pub arm: bool,
}

impl LoopBodyContext {
    /// This context for a statement of an arm frame ([`Self::arm`]).
    #[must_use]
    pub fn in_arm(mut self) -> Self {
        self.arm = true;
        self
    }

    /// The context of the statement `stmt` of a frame body.
    #[must_use]
    pub fn new(stmt: &Expression, innermost_fold: bool, outer_fold: bool, in_actor: bool) -> Self {
        let statement = stmt.unwrap_parens();
        let value = match statement {
            Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
                value.unwrap_parens().span()
            }
            other => other.span(),
        };
        Self {
            statement: statement.span(),
            value,
            innermost_fold,
            outer_fold,
            in_actor,
            arm: false,
        }
    }
}

/// ADR 0131 Phase 2c (BT-3796, from the BT-3793/BT-3797 reviews): whether
/// `site`, one of the statement's [`loop_body_producer_sites`], is a
/// value-bearing site that an actor method's fold frame does not lower
/// through the producer.
///
/// An actor fold's accumulator is a tuple the construct's carrier does not
/// fit: `r := xs inject: 0 into: [...]` in a `do:` body compiled and then
/// crashed (BT-3797). So every value-bearing site (an assignment's value or an
/// operand, [`is_value_bearing_site`]) of a statement directly in a fold body
/// is out. Under a letrec loop nested in a fold (BT-3796 probe) the whole value
/// of an assignment is lowered, but an operand of a larger expression
/// (`#[xs do: [...], t]`, `(xs do: [...]) printString`) wrote nothing back or
/// crashed, so operands are out under *any* enclosing fold frame. A letrec
/// loop frame alone lowers every value-bearing site.
///
/// This exclusion is about producers that rebind *outer locals*. A loop or
/// fold that writes none but threads the actor's `State` (its block sends to
/// `self` or writes a field) is a producer at these sites all the same:
/// codegen registers it (`is_family_only_fold_value_site`, BT-4287).
#[must_use]
pub fn is_actor_fold_value_site(site: &Expression, ctx: &LoopBodyContext) -> bool {
    if !ctx.in_actor || !is_value_bearing_site(ctx.statement, site) {
        return false;
    }
    ctx.innermost_fold || (ctx.outer_fold && site.unwrap_parens().span() != ctx.value)
}

/// ADR 0131 Phase 2b/2c (BT-3796): whether `site`, one of the statement's
/// [`loop_body_producer_sites`], is lowered as a local-threading producer. The
/// one narrowing of that site list, read by the Phase 0 check, by codegen's
/// `with_body_producer_sites` and by the agreement test, so the diagnostic, the
/// lowering and the test's expectation cannot disagree:
///
/// - [`is_actor_fold_value_site`].
///
/// `detect:`/`detect:ifNone:` are loop-body producers in every method kind
/// (BT-3841: the fold stops at the first match, BT-3805). In an actor method
/// they were excluded here (BT-3793) while the frames around them dropped or
/// miscompiled the write; ADR 0133 Phase 3 (BT-3911, closing BT-3878 and
/// BT-3861) lowers them like any other fold.
///
/// A conditional, an `on:do:`/`ensure:` or a `Dictionary` lookup is a producer
/// in a loop or fold body (ADR 0131 Phase 3b/3c, BT-3817/BT-3822/BT-3839)
/// wherever the frame has a site for it.
#[must_use]
pub fn is_loop_body_producer_site(site: &Expression, ctx: &LoopBodyContext) -> bool {
    // ADR 0131 Phase 3 (BT-3738) / Phase 3c (BT-3822): a conditional is a
    // producer at a method-root site, in an arm of a conditional that is one
    // (`ctx.arm`) and in a loop or fold body (the frame lowers its
    // `LocalRebind`s by the frame's mode, ADR 0131 §2), like an
    // `on:do:`/`ensure:` (Phase 3b, BT-3817). Whether a given conditional is a
    // producer is `conditional_producer_writes`'s question, not the site's.
    //
    // BT-3839: a `Dictionary` lookup (Phase 2d) is a conditional in the core
    // rule's eyes, and lowers by the frame's mode like one. (Codegen's loop
    // gate sees the handler's write through `threaded_locals_of`, so the
    // enclosing loop or fold threads it in an actor too.)
    !is_actor_fold_value_site(site, ctx)
}

/// Whether the literal block `block`, an argument of the send `expr`, is the
/// body of a loop or fold that codegen lowers as a threaded frame
/// (`lower_letrec_body`/`lower_foldl_body`), so its statements are
/// [`loop_body_producer_sites`]: a block argument of a loop or fold family
/// construct ([`local_threading_construct`]) at an index
/// [`crate::state_threading_selectors::state_threaded_block_arg_indices`]
/// lists. Not a `whileTrue:`/`whileFalse:` condition (the receiver), not
/// `detect:ifNone:`'s handler, not an arm, a handler or a protected body.
#[must_use]
pub fn is_threaded_frame_body(expr: &Expression, block: &Block) -> bool {
    let Some(construct) = local_threading_construct(expr) else {
        return false;
    };
    if !matches!(
        construct.family,
        LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
    ) {
        return false;
    }
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return false;
    };
    crate::state_threading_selectors::state_threaded_block_arg_indices(&construct.selector)
        .iter()
        .any(|&i| matches!(arguments.get(i), Some(Expression::Block(b)) if std::ptr::eq(b, block)))
}

/// ADR 0131 Phase 2b (BT-3797): whether `block` is the body of a *fold*
/// frame ([`is_threaded_frame_body`] with a `LocalThreadingFamily::Fold`
/// construct: `do:`, `collect:`, `inject:into:`, …), as opposed to a
/// `whileTrue:`/counted-loop letrec frame. An actor's fold frame carries
/// its locals in a tuple accumulator, which a value-bearing producer in the
/// body does not lower through.
#[must_use]
pub fn is_fold_frame_body(expr: &Expression, block: &Block) -> bool {
    is_threaded_frame_body(expr, block)
        && local_threading_construct(expr)
            .is_some_and(|c| matches!(c.family, LocalThreadingFamily::Fold))
}

fn operand_sites_of_statement(stmt: &Expression, construct_operands: bool) -> Vec<&Expression> {
    fn operands<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
        let expr = expr.unwrap_parens();
        match expr {
            _ if unsequenced_operand_holder(expr).is_some() => {}
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_none() {
                    site(receiver, out);
                }
                for arg in arguments {
                    if !matches!(arg, Expression::Block(_)) {
                        site(arg, out);
                    }
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    site(e, out);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    site(e, out);
                }
                if let Some(t) = tail {
                    site(t, out);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    site(&pair.key, out);
                    site(&pair.value, out);
                }
            }
            _ => {}
        }
    }
    fn site<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
        let inner = expr.unwrap_parens();
        if matches!(inner, Expression::Block(_)) {
            return;
        }
        out.push(inner);
        operands(inner, out);
    }
    let mut out = Vec::new();
    match stmt.unwrap_parens() {
        Expression::Assignment { target, value, .. }
            if matches!(target.as_ref(), Expression::Identifier(_)) =>
        {
            site(value, &mut out);
        }
        // ADR 0131 Phase 2d (BT-3795): the value of a method-body field or
        // class-variable write `self.f := V`. Its three consumers (the actor
        // `FieldAssignment`/`FieldAssignmentControlFlow` arms and the class
        // method's `class_method_prelude_producer`) sequence `V` through
        // `threaded_expression` before the write. ADR 0131 Phase 3b (BT-3817):
        // also a statement of a loop, fold or arm frame body
        // (`construct_operands`): the frame's field-write lowering
        // (`lower_field_assignment_bind`, the class-variable write's
        // `thread_ahead`) splices the value's prelude ahead of the write too.
        Expression::Assignment { target, value, .. } if is_self_field_target(target) => {
            site(value, &mut out);
        }
        Expression::Return { value, .. } => site(value, &mut out),
        other if !construct_operands && local_threading_construct(other).is_some() => {}
        other => operands(other, &mut out),
    }
    out
}

/// The statements of `body` that compile to code: every one but the
/// compile-time-only `@expect` directives. The one filter codegen's
/// `collect_body_exprs` and the type checker's
/// [`ends_with_self_field_write`] share, so the two always agree on which
/// statement is a body's last.
pub fn body_exprs(body: &[ExpressionStatement]) -> impl DoubleEndedIterator<Item = &Expression> {
    body.iter()
        .map(|stmt| &stmt.expression)
        .filter(|expr| !matches!(expr, Expression::ExpectDirective { .. }))
}

/// BT-4235: whether `body`'s last statement ([`body_exprs`]), seen through
/// statement-level parentheses (BT-4092), is a `self.<field> := …` write
/// ([`is_self_field_target`]). On codegen's value-type body path such a
/// method answers its updated `self` (BT-833), and the type checker infers
/// the receiver type from this; codegen's `generate_vt_body_exprs_in_sites`
/// makes the same decision on the same leaves. A body-level `^` ends the
/// body where it stands (codegen emits nothing after it, `infer_stmts` stops
/// there), so a write after one is dead and the answer is `false`.
#[must_use]
pub fn ends_with_self_field_write(body: &[ExpressionStatement]) -> bool {
    let mut last = None;
    for expr in body_exprs(body) {
        let expr = expr.unwrap_parens();
        if matches!(expr, Expression::Return { .. }) {
            return false;
        }
        last = Some(expr);
    }
    matches!(
        last,
        Some(Expression::Assignment { target, .. }) if is_self_field_target(target)
    )
}

/// Whether `target` is `self.<field>`, the target of a field or class-variable
/// write: the one shape the field-write rules share (here, and codegen's
/// `field_assignment_name` and `class_var_assignment_shape`).
#[must_use]
pub fn is_self_field_target(target: &Expression) -> bool {
    matches!(
        target,
        Expression::FieldAccess { receiver, .. }
            if receiver.is_self()
    )
}

/// ADR 0131 Phase 2 (BT-3749): whether the send `receiver selector: arguments`
/// is lowered by a path that compiles its operands directly instead of
/// sequencing them through `threaded_expression`, so a local-threading
/// producer among them would leak its raw `{Value, State}` tuple and drop the
/// write. Those are the actor body lowerings of a `super` send, an `error:`
/// send (the actor `ErrorSend` body lowering, which takes only an `error:` to
/// an instance receiver; this stops at every `error:` send whatever its
/// receiver, a safe over-approximation, since the `error:` lowering sequences
/// its own operands for every receiver kind, see
/// [`crate::ast::is_error_signal_instance_receiver`]),
/// `self fieldAt:put:` / `self clearField:`, a block `value`-family send
/// (`value:`, `valueWithArguments:`; a possible Tier 2 call on a local or a
/// `self.field`) and a self-send carrying a block argument (a
/// possible Tier 2 self-send). Applied in every context, so the diagnostic does
/// not depend on which method kind it sits in.
///
/// The `is_block_invocation()` stop is deliberately an over-approximation for
/// **any receiver**: only a Tier 2 call on a bare local or a `self.field`
/// takes the unsequenced actor lowering, but a `value`-family or
/// `valueWithArguments:` send stops the sites whatever its receiver is (a
/// literal block, a call result, a parenthesised expression). Deciding from
/// the receiver would duplicate codegen's `is_tier2_value_call`, and the two
/// could drift. A producer the over-approximation leaves out of the sites is
/// never a miscompile: it keeps its Phase 0 diagnostic. (A loop or fold that
/// rebinds no local but threads an actor's `State` is a producer at these
/// operands too, BT-4310: each of these lowerings sequences its operands for
/// it, `operand_loop_constructs`.)
#[must_use]
fn operands_not_sequenced(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> Option<UnsequencedOperand> {
    use crate::ast::WellKnownSelector;
    let is_self = receiver.is_self();
    if matches!(receiver, Expression::Super(_)) {
        return Some(UnsequencedOperand::SuperSend);
    }
    match selector.well_known() {
        Some(WellKnownSelector::Error) if arguments.len() == 1 => {
            return Some(UnsequencedOperand::ErrorSend);
        }
        Some(WellKnownSelector::FieldAtPut) if is_self => {
            return Some(UnsequencedOperand::FieldAtPut);
        }
        Some(WellKnownSelector::ClearField) if is_self => {
            return Some(UnsequencedOperand::ClearField);
        }
        _ => {}
    }
    if selector.is_block_invocation() {
        return Some(UnsequencedOperand::BlockInvocation);
    }
    (is_self && arguments.iter().any(|a| matches!(a, Expression::Block(_))))
        .then_some(UnsequencedOperand::BlockTakingSelfSend)
}

/// BT-4310: why the sequencing rule's site walk ([`sequenced_operand_sites`],
/// [`loop_body_producer_sites`]) does not descend into the operands of an
/// expression, while [`operand_loop_constructs`] does: the lowering of each
/// of these compiles its operands itself. A loop or fold that writes an outer
/// local is the Phase 0 check's there; one that threads only an actor's
/// `State` is a producer site of the frame all the same (codegen's
/// `family_only_operand_site_exprs`), and each of these lowerings sequences
/// its operands for it. [`unsequenced_operand_holder`] is the one classifier
/// both walks read, so a generated corpus can check that it covers every kind
/// ([`UnsequencedOperand::ALL`], [`operand_loop_constructs_with_holders`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnsequencedOperand {
    /// A method-body statement that is itself a local-threading construct
    /// (`(xs collect: [...]) do: [...]`, a conditional): its statement
    /// lowering compiles its receiver and arguments. (A loop or fold body's
    /// statement walks them: `loop_body_operand_sites`.)
    ConstructStatement,
    /// The receiver of a conditional (`ifTrue:`, `and:`, `ifNil:`, ...).
    ConditionalReceiver,
    /// An operand of a nested construct other than a loop or fold: a lookup
    /// (`at:ifAbsent:`), an `on:do:`/`ensure:`, an evaluated block, a
    /// `Result tryDo:`.
    NestedConstruct,
    /// An operand of a cast (`x foo: y!`).
    Cast,
    /// An argument of a `super` send.
    SuperSend,
    /// The argument of an `error:` send, for any receiver: an
    /// over-approximation of the actor `ErrorSend` body lowering, which takes
    /// only an instance receiver (see [`operands_not_sequenced`]).
    ErrorSend,
    /// An argument of `self fieldAt:put:`.
    FieldAtPut,
    /// The argument of `self clearField:`.
    ClearField,
    /// An operand of a block invocation (`value:`, `valueWithArguments:`).
    BlockInvocation,
    /// An argument of a self-send that also takes a block literal.
    BlockTakingSelfSend,
    /// Inside a cascade: what its shared receiver holds, and its messages'
    /// arguments.
    Cascade,
    /// An interpolated segment of a string.
    Interpolation,
    /// The value of an assignment nested in an operand (`(q := xs size) + 1`).
    Assignment,
    /// The value of a destructuring assignment.
    Destructure,
    /// The subject of a `match:`.
    MatchSubject,
}

impl UnsequencedOperand {
    /// Every kind, in declaration order.
    pub const ALL: &'static [Self] = &[
        Self::ConstructStatement,
        Self::ConditionalReceiver,
        Self::NestedConstruct,
        Self::Cast,
        Self::SuperSend,
        Self::ErrorSend,
        Self::FieldAtPut,
        Self::ClearField,
        Self::BlockInvocation,
        Self::BlockTakingSelfSend,
        Self::Cascade,
        Self::Interpolation,
        Self::Assignment,
        Self::Destructure,
        Self::MatchSubject,
    ];
}

/// BT-4310: why the site walk stops at the operands of `expr` (already
/// without parentheses, as both walks hand it over), or `None` when it walks
/// them (a plain send, a loop or fold, a literal collection). See
/// [`UnsequencedOperand`].
#[must_use]
pub fn unsequenced_operand_holder(expr: &Expression) -> Option<UnsequencedOperand> {
    match expr {
        Expression::MessageSend { is_cast: true, .. } => Some(UnsequencedOperand::Cast),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            if local_threading_construct(expr).is_some_and(|c| {
                !matches!(
                    c.family,
                    LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                )
            }) {
                // A conditional is a construct too; its receiver is the one
                // operand it has.
                return Some(
                    if crate::state_threading_selectors::is_conditional_selector(&selector.name()) {
                        UnsequencedOperand::ConditionalReceiver
                    } else {
                        UnsequencedOperand::NestedConstruct
                    },
                );
            }
            // BT-4451: only an inlined conditional (literal block arguments)
            // holds its receiver back; any other conditional-selector send is
            // a plain send whose operands codegen sequences.
            if crate::state_threading_selectors::is_inlined_conditional_send(
                &selector.name(),
                arguments,
            ) {
                return Some(UnsequencedOperand::ConditionalReceiver);
            }
            operands_not_sequenced(receiver, selector, arguments)
        }
        Expression::Cascade { .. } => Some(UnsequencedOperand::Cascade),
        Expression::StringInterpolation { .. } => Some(UnsequencedOperand::Interpolation),
        Expression::Assignment { .. } => Some(UnsequencedOperand::Assignment),
        Expression::DestructureAssignment { .. } => Some(UnsequencedOperand::Destructure),
        Expression::Match { .. } => Some(UnsequencedOperand::MatchSubject),
        // A literal collection's elements are walked, and so are a binary
        // construction's segment values and sizes (ADR 0143): codegen
        // sequences them as literal-container children, left to right.
        // Nothing else holds an operand the walks reach that can be a loop or
        // fold (a field access's receiver is an identifier: the parser forms
        // one only after it).
        _ => None,
    }
}
