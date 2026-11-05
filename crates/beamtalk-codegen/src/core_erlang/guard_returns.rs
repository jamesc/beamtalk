// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Guard-style early returns as direct returns (BT-4415).
//!
//! **DDD Context:** Code Generation
//!
//! A `^` whose enclosing block is an *inlined* conditional at the method's
//! top level is a plain early return of the method: the block is lowered
//! into a `case` arm of the method body, never a closure. The NLR recognizer
//! ([`body_has_block_nlr`]) nonetheless counts it as a `^` inside a block,
//! so the method pays for a non-local-return frame — `make_ref/0`, a `try`
//! around the whole body and a `throw` for the `^` — on every call, taken
//! or not. The guard idiom the language guide teaches,
//!
//! ```beamtalk
//! canon: bytes =>
//!   bytes isEmpty ifTrue: [^#[]]
//!   bytes reversed
//! ```
//!
//! is the same program as the expression form
//!
//! ```beamtalk
//! canon: bytes =>
//!   bytes isEmpty ifTrue: [#[]] ifFalse: [bytes reversed]
//! ```
//!
//! which compiles to a `case` and needs no frame. [`rewrite_guard_returns`]
//! performs that rewrite on codegen's own clone of the module, so every
//! method kind (Actor, Value, Object, class-side, standalone extension)
//! reaches the existing inlined-conditional lowering — and with it
//! `ThreadedIr` and its `verify()` for state threading — rather than a
//! second, per-method-kind early-return lowering.
//!
//! # The rewrite
//!
//! A method body is a *tail body*: the value of its last statement is the
//! method's result. Over a tail body:
//!
//! - **Guard.** A non-last statement that is an inlined conditional
//!   (`ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNil:`, `ifNotNil:`,
//!   `ifNil:ifNotNil:`, `ifNotNil:ifNil:` with literal block arguments)
//!   where exactly one branch slot *falls through* (is absent, or a block
//!   whose last statement is not a `^`) and every other branch ends in a `^`,
//!   absorbs the statements after it: they become the tail of the falling
//!   branch (a fresh block when the slot was absent), and the conditional
//!   becomes the body's last statement.
//! - **Tail.** In tail position a `^ v` is `v`; an inlined conditional's
//!   branch bodies are tail bodies; a `match:` arm body `^ v` is `v`.
//!
//! A falling branch with block parameters (`ifNotNil: [:x | …]`) never
//! absorbs the statements after it, since its parameter would shadow a name
//! they use.
//!
//! The Boolean family (`ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`) is
//! rewritten only when its receiver is statically a Boolean (the caller's
//! [`IsBoolean`]). On any other receiver the inlined conditional sends its
//! own selector, with the branches as closures (BT-4244): a receiver that
//! implements `ifTrue:` itself must still receive `ifTrue:` and a block that
//! returns from the method, which only the form as written gives it. The nil
//! family needs no such check: it lowers to a total `case` on `nil`, with no
//! send.
//!
//! # When it applies
//!
//! Only to a method that holds a block NLR as written and none once
//! rewritten, so the frame is gone entirely: a method that keeps a `^`
//! inside a real closure (`do:`, `collect:`, a stored block) keeps its frame
//! and is compiled exactly as written. Facts derived from the tree
//! ([`SemanticFacts`](beamtalk_core::semantic_analysis::SemanticFacts)) are
//! recomputed by the caller over the rewritten module; the original bodies
//! are returned so a method's reflected source stays its text as written.

use std::collections::HashMap;

use beamtalk_core::ast::{
    Block, Expression, ExpressionStatement, KeywordPart, MessageSelector, MethodDefinition, Module,
};
use beamtalk_core::semantic_analysis::facts::body_has_block_nlr;
use beamtalk_core::source_analysis::Span;

/// The original bodies of the methods [`rewrite_guard_returns`] rewrote,
/// keyed by method span.
pub(in crate::core_erlang) type RewrittenBodies = HashMap<Span, Vec<ExpressionStatement>>;

/// Whether an expression is statically a Boolean: the receivers of the
/// Boolean-family conditionals the rewrite may change.
pub(in crate::core_erlang) type IsBoolean<'a> = &'a dyn Fn(&Expression) -> bool;

/// Whether `ty` (an expression's inferred type) is a Boolean: `Boolean`,
/// `True`, `False`, or a union of them. Any provenance: a value of a
/// declared-but-wrong type reaching the rewritten conditional only changes
/// which selector the resulting `does_not_understand` names.
pub(in crate::core_erlang) fn is_boolean_type(
    ty: Option<&beamtalk_core::semantic_analysis::InferredType>,
) -> bool {
    use beamtalk_core::semantic_analysis::InferredType;
    match ty {
        Some(InferredType::Known { class_name, .. }) => {
            matches!(class_name.as_str(), "Boolean" | "True" | "False")
        }
        Some(InferredType::Union { members, .. }) => {
            members.iter().all(|m| is_boolean_type(Some(m)))
        }
        _ => false,
    }
}

/// Rewrites, in place, every method of `module` whose guard-style `^`s are
/// its only block NLRs (see the module doc), returning the original bodies
/// of the methods it changed. Empty when nothing changed.
pub(in crate::core_erlang) fn rewrite_guard_returns(
    module: &mut Module,
    is_boolean: IsBoolean<'_>,
) -> RewrittenBodies {
    let mut originals = RewrittenBodies::new();
    let methods = module
        .classes
        .iter_mut()
        .flat_map(|class| {
            class
                .methods
                .iter_mut()
                .chain(class.class_methods.iter_mut())
        })
        .chain(module.method_definitions.iter_mut().map(|m| &mut m.method));
    for method in methods {
        if let Some(original) = rewrite_method(method, is_boolean) {
            originals.insert(method.span, original);
        }
    }
    originals
}

/// Whether `module` has a method [`rewrite_guard_returns`] would change, so
/// a caller can skip cloning the module when it has none.
pub(in crate::core_erlang) fn has_guard_returns(
    module: &Module,
    is_boolean: IsBoolean<'_>,
) -> bool {
    module
        .classes
        .iter()
        .flat_map(|class| class.methods.iter().chain(class.class_methods.iter()))
        .chain(module.method_definitions.iter().map(|m| &m.method))
        .any(|method| rewritten_body(&method.body, is_boolean).is_some())
}

/// Rewrites `method`'s body when that removes every block NLR from it,
/// returning the body as it was.
fn rewrite_method(
    method: &mut MethodDefinition,
    is_boolean: IsBoolean<'_>,
) -> Option<Vec<ExpressionStatement>> {
    let body = rewritten_body(&method.body, is_boolean)?;
    Some(std::mem::replace(&mut method.body, body))
}

/// `body` rewritten, when it holds a block NLR and the rewrite leaves none.
fn rewritten_body(
    body: &[ExpressionStatement],
    is_boolean: IsBoolean<'_>,
) -> Option<Vec<ExpressionStatement>> {
    if !body_has_block_nlr(body) {
        return None;
    }
    let rewritten = rewrite_tail_body(body.to_vec(), is_boolean);
    (!body_has_block_nlr(&rewritten)).then_some(rewritten)
}

/// Rewrites a tail body: absorbs the statements after the first guard into
/// it, then rewrites the (new) last statement in tail position.
fn rewrite_tail_body(
    mut body: Vec<ExpressionStatement>,
    is_boolean: IsBoolean<'_>,
) -> Vec<ExpressionStatement> {
    let guard_at = body.iter().enumerate().position(|(i, stmt)| {
        i + 1 < body.len() && guard_shape(&stmt.expression, is_boolean).is_some()
    });
    if let Some(i) = guard_at {
        let rest = body.split_off(i + 1);
        let guard = body.pop().expect("guard statement at index i");
        let absorbed = absorb_rest(guard.expression, rest, is_boolean);
        body.push(ExpressionStatement {
            expression: absorbed,
            ..guard
        });
    }
    if let Some(last) = body.pop() {
        let expression = rewrite_tail_expression(last.expression, is_boolean);
        body.push(ExpressionStatement { expression, ..last });
    }
    body
}

/// Rewrites `expr` in tail position: `^ v` is `v`, and an inlined
/// conditional's branches (a `match:`'s arm bodies) are in tail position too.
fn rewrite_tail_expression(expr: Expression, is_boolean: IsBoolean<'_>) -> Expression {
    match expr {
        Expression::Return { value, .. } => *value,
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            is_cast: false,
            span,
        } if is_inlined_conditional(&parts, &arguments)
            && receiver_qualifies(&parts, &receiver, is_boolean) =>
        {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments: arguments
                    .into_iter()
                    .map(|arg| match arg {
                        Expression::Block(mut block) => {
                            let body = std::mem::take(&mut block.body);
                            Expression::Block(with_body(block, rewrite_tail_body(body, is_boolean)))
                        }
                        other => other,
                    })
                    .collect(),
                is_cast: false,
                span,
            }
        }
        Expression::Match {
            value,
            arms,
            exhaustive,
            span,
        } => Expression::Match {
            value,
            arms: arms
                .into_iter()
                .map(|mut arm| {
                    if let Expression::Return { value, .. } = arm.body {
                        arm.body = *value;
                    }
                    arm
                })
                .collect(),
            exhaustive,
            span,
        },
        other => other,
    }
}

/// `block` with `body` in place of its own, keeping its identity.
fn with_body(mut block: Block, body: Vec<ExpressionStatement>) -> Block {
    block.body = body;
    block
}

/// The inlined conditionals whose literal-block branches are tail bodies
/// when the conditional is.
const INLINED_CONDITIONALS: [&str; 7] = [
    "ifTrue:",
    "ifFalse:",
    "ifTrue:ifFalse:",
    "ifNil:",
    "ifNotNil:",
    "ifNil:ifNotNil:",
    "ifNotNil:ifNil:",
];

fn selector_of(parts: &[KeywordPart]) -> String {
    parts.iter().map(|p| p.keyword.as_str()).collect()
}

/// Whether the conditional `parts` names may be rewritten on `receiver`:
/// always for the nil family, only on a static Boolean for the Boolean one.
fn receiver_qualifies(
    parts: &[KeywordPart],
    receiver: &Expression,
    is_boolean: IsBoolean<'_>,
) -> bool {
    family_of(parts) == Family::Nil || is_boolean(receiver)
}

/// Whether `parts`/`arguments` name an inlined conditional with literal
/// block arguments.
fn is_inlined_conditional(parts: &[KeywordPart], arguments: &[Expression]) -> bool {
    INLINED_CONDITIONALS.contains(&selector_of(parts).as_str())
        && arguments.iter().all(|a| matches!(a, Expression::Block(_)))
}

/// The two branch slots of a conditional, by the selector of its two-branch
/// form: (`ifTrue:`, `ifFalse:`) or (`ifNil:`, `ifNotNil:`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Family {
    Boolean,
    Nil,
}

impl Family {
    fn keywords(self) -> [&'static str; 2] {
        match self {
            Self::Boolean => ["ifTrue:", "ifFalse:"],
            Self::Nil => ["ifNil:", "ifNotNil:"],
        }
    }
}

/// The family of the conditional `parts` names.
fn family_of(parts: &[KeywordPart]) -> Family {
    if parts[0].keyword.starts_with("ifTrue") || parts[0].keyword.starts_with("ifFalse") {
        Family::Boolean
    } else {
        Family::Nil
    }
}

/// A guard's shape: its family, and which slot (0 or 1, in
/// [`Family::keywords`] order) falls through.
struct GuardShape {
    family: Family,
    falling_slot: usize,
}

/// `expr`'s [`GuardShape`] when it is a guard statement (see the module
/// doc), else `None`.
fn guard_shape(expr: &Expression, is_boolean: IsBoolean<'_>) -> Option<GuardShape> {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr
    else {
        return None;
    };
    if !is_inlined_conditional(parts, arguments) || !receiver_qualifies(parts, receiver, is_boolean)
    {
        return None;
    }
    let slots = branch_slots(parts, arguments)?;
    let returns = |b: Option<&Block>| {
        b.is_some_and(|b| {
            b.body
                .last()
                .is_some_and(|s| matches!(s.expression, Expression::Return { .. }))
        })
    };
    let falling: Vec<usize> = (0..2).filter(|&i| !returns(slots.1[i])).collect();
    let [falling_slot] = falling.as_slice() else {
        return None;
    };
    // A falling branch's parameters would shadow names the absorbed
    // statements use.
    if slots.1[*falling_slot].is_some_and(|b| !b.parameters.is_empty()) {
        return None;
    }
    Some(GuardShape {
        family: slots.0,
        falling_slot: *falling_slot,
    })
}

/// The conditional's family and its two branch blocks in
/// [`Family::keywords`] order (`None` for an absent slot).
fn branch_slots<'a>(
    parts: &[KeywordPart],
    arguments: &'a [Expression],
) -> Option<(Family, [Option<&'a Block>; 2])> {
    let family = family_of(parts);
    let keywords = family.keywords();
    let mut slots = [None, None];
    for (part, arg) in parts.iter().zip(arguments) {
        let Expression::Block(block) = arg else {
            return None;
        };
        let slot = keywords.iter().position(|k| *k == part.keyword)?;
        slots[slot] = Some(block);
    }
    Some((family, slots))
}

/// The guard `expr` (a [`guard_shape`]) with `rest` appended to its falling
/// branch, as the two-branch conditional (`ifTrue:ifFalse:` /
/// `ifNil:ifNotNil:`).
fn absorb_rest(
    expr: Expression,
    rest: Vec<ExpressionStatement>,
    is_boolean: IsBoolean<'_>,
) -> Expression {
    let shape = guard_shape(&expr, is_boolean).expect("absorb_rest is only called on a guard");
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast,
        span,
    } = expr
    else {
        unreachable!("guard_shape matched a keyword send");
    };
    let keywords = shape.family.keywords();
    let mut slots: [Option<Block>; 2] = [None, None];
    for (part, arg) in parts.iter().zip(arguments) {
        if let Expression::Block(block) = arg {
            let slot = keywords
                .iter()
                .position(|k| *k == part.keyword)
                .expect("guard_shape resolved every keyword");
            slots[slot] = Some(block);
        }
    }
    // The absent falling slot becomes a fresh block. Its span is the empty
    // span at the guard's end: no parsed node has it, so nothing keyed by
    // span (facts, sequencing registrations) can collide with it.
    let falling = slots[shape.falling_slot]
        .take()
        .unwrap_or_else(|| Block::new(Vec::new(), Vec::new(), Span::new(span.end(), span.end())));
    let mut falling = falling;
    falling.body.extend(rest);
    slots[shape.falling_slot] = Some(falling);
    let [first, second] = slots;
    Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(
            keywords
                .iter()
                .map(|k| KeywordPart::new(*k, span))
                .collect(),
        ),
        arguments: vec![
            Expression::Block(first.expect("both slots are filled")),
            Expression::Block(second.expect("both slots are filled")),
        ],
        is_cast,
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::unparse::unparse_method;

    fn parse(source: &str) -> Module {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        module
    }

    /// Treats every receiver as a Boolean (the type map is codegen's).
    fn any_boolean(_: &Expression) -> bool {
        true
    }

    /// The body of the first method of `class_source`'s class, rewritten,
    /// unparsed (`None` when the rewrite does not apply).
    fn rewritten(class_source: &str) -> Option<String> {
        let mut module = parse(class_source);
        assert_eq!(
            has_guard_returns(&module, &any_boolean),
            {
                let mut probe = module.clone();
                !rewrite_guard_returns(&mut probe, &any_boolean).is_empty()
            },
            "has_guard_returns agrees with rewrite_guard_returns"
        );
        let originals = rewrite_guard_returns(&mut module, &any_boolean);
        let method = &module.classes[0].methods[0];
        originals
            .contains_key(&method.span)
            .then(|| unparse_method(method))
    }

    fn class(body: &str) -> String {
        format!("Object subclass: C\n  m: x =>\n{body}\n")
    }

    #[test]
    fn if_true_guard_becomes_if_true_if_false() {
        let out = rewritten(&class("    x > 0 ifTrue: [^1]\n    2")).unwrap();
        assert!(out.contains("ifTrue: [1] ifFalse: [2]"), "{out}");
    }

    #[test]
    fn if_false_guard_falls_through_on_true() {
        let out = rewritten(&class("    x > 0 ifFalse: [^1]\n    2")).unwrap();
        assert!(out.contains("ifTrue: [2] ifFalse: [1]"), "{out}");
    }

    #[test]
    fn consecutive_guards_nest() {
        let out = rewritten(&class(
            "    x > 0 ifTrue: [^1]\n    x < 0 ifTrue: [^2]\n    3",
        ))
        .unwrap();
        assert!(!out.contains('^'), "{out}");
        assert!(out.contains("ifTrue: [2] ifFalse: [3]"), "{out}");
    }

    #[test]
    fn if_nil_guard_absorbs_rest_into_if_not_nil() {
        let out = rewritten(&class("    x ifNil: [^0]\n    x + 1")).unwrap();
        assert!(out.contains("ifNil: [0] ifNotNil: [x + 1]"), "{out}");
    }

    #[test]
    fn two_branch_guard_appends_rest_to_falling_branch() {
        let out = rewritten(&class(
            "    y := 0\n    x > 0 ifTrue: [^1] ifFalse: [y := 5]\n    y",
        ))
        .unwrap();
        assert!(!out.contains('^'), "{out}");
        let falling = out.split("ifFalse:").nth(1).expect("ifFalse: branch");
        assert!(falling.contains("y := 5"), "{out}");
    }

    #[test]
    fn tail_match_arm_returns_are_stripped() {
        let out = rewritten(&class("    x match: [0 -> ^#zero; _ -> #other]")).unwrap();
        assert!(!out.contains('^'), "{out}");
    }

    #[test]
    fn closure_nlr_keeps_the_method_as_written() {
        assert_eq!(
            rewritten(&class(
                "    x > 0 ifTrue: [^1]\n    #[1, 2] do: [:e | e =:= x ifTrue: [^e]]\n    2"
            )),
            None
        );
    }

    #[test]
    fn method_without_block_nlr_is_untouched() {
        assert_eq!(
            rewritten(&class("    x > 0 ifTrue: [1] ifFalse: [2]")),
            None
        );
    }

    #[test]
    fn boolean_family_needs_a_boolean_receiver() {
        let mut module = parse(&class("    x > 0 ifTrue: [^1]\n    2"));
        assert!(!has_guard_returns(&module, &|_| false));
        assert!(rewrite_guard_returns(&mut module, &|_| false).is_empty());
    }

    #[test]
    fn nil_family_needs_no_boolean_receiver() {
        let mut module = parse(&class("    x ifNil: [^0]\n    x"));
        assert!(!rewrite_guard_returns(&mut module, &|_| false).is_empty());
    }

    #[test]
    fn falling_branch_with_parameters_does_not_absorb() {
        assert_eq!(
            rewritten(&class("    x ifNil: [^0] ifNotNil: [:v | v]\n    x")),
            None
        );
    }
}
