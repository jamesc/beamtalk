// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0132 Phase 0b (I5, BT-3867): a stored Tier 2 block sent `value` in a
//! class or value-type method is an inline producer over a fresh map.
//!
//! `b := [t := t + 1]. b value. t` has no callee: the block is created and
//! evaluated in the same method body, so the write has nowhere to go but the
//! enclosing frame. ADR 0131 §1 already names the evaluated literal block
//! (`[t := t + 1] value`, [`literal_block_value_arm`]) as a local-threading
//! producer in every method kind. This pass reduces the stored shape to that
//! one: it replaces each `value`-family send to the stored local with the
//! block literal itself, evaluated in place, and drops the binding. There is
//! no second lowering and no per-position path: the producer rule, the site
//! rules, the Phase 0 check and codegen all read the literal form, so the
//! positions that accept `[...] value` accept the stored form and the rest
//! keep the ADR 0131 §6 error (reported at the inlined block).
//!
//! The one function [`inline_stored_tier2_values`] is read by both the §6
//! check (`validators/local_threading.rs`) and the pre-codegen lowering
//! (`lowering.rs`), so the check and codegen cannot disagree about which
//! stored blocks are inline producers. Actor instance methods keep the Tier 2
//! protocol (`prescan_tier2_local_vars`) and the REPL keeps its own lowering;
//! neither is rewritten.
//!
//! # Shapes
//!
//! A statement `b := [...]` of the method body is inlined when
//!
//! - `b` is assigned nowhere else, is not a method parameter and is not a block
//!   parameter anywhere in the method;
//! - the block writes an outer local, every such write is one the Tier 2
//!   protocol threads ([`captured_local_mutations`]), and it writes no field;
//! - every later reference to `b` is the receiver of a `value`-family send
//!   outside any nested block, the same rule codegen's
//!   `prescan_tier2_local_vars` reads ([`stored_block_var_uses`]), and `b` is not
//!   mentioned before the binding;
//! - no later statement assigns a local the block reads or writes, so
//!   evaluating the block at the send reads the locals the block closed over;
//! - a `value:`-family send passes exactly as many arguments as the block has
//!   parameters, and (for a block with parameters) no other binder of the
//!   parameter names exists in the method.
//!
//! A cascade on `b` (`b value: 1; value: 2`) is split into one send per
//! message when it is a statement, an assignment's value or a `^` value; in any
//! other position the stored local is left alone and keeps the §6 error.
//!
//! The arguments of a `value:`-family send are bound to the block's parameters
//! by assignments at the head of the inlined block (`n := arg`), so the arm is
//! the parameterless `[n := arg. ...] value` the producer rule recognizes.

use crate::ast::{
    Block, CascadeMessage, Expression, ExpressionStatement, Identifier, MessageSelector,
};
use crate::ast_walker::{walk_expression, walk_expression_mut};
use crate::semantic_analysis::block_facts::{
    analyze_block, captured_local_mutations, is_safe_value_family_selector, normalize_cascade,
    outer_local_writes, stored_block_var_uses,
};
use crate::semantic_analysis::{extract_match_arm_bindings, extract_pattern_bindings};
use ecow::EcoString;
use std::collections::HashSet;

/// The statements of a class-side or non-actor instance method body with each
/// promotable stored Tier 2 block inlined at its `value`-family sends (see the
/// module docs), or `None` when no stored block is inlined.
///
/// `params` are the method's parameter names.
#[must_use]
pub fn inline_stored_tier2_values(
    body: &[ExpressionStatement],
    params: &[EcoString],
) -> Option<Vec<ExpressionStatement>> {
    let mut current: Option<Vec<ExpressionStatement>> = None;
    // Each success removes one binding statement, so this terminates.
    while let Some(next) = inline_one(current.as_deref().unwrap_or(body), params) {
        current = Some(next);
    }
    current
}

/// Inlines the first promotable stored block of `stmts`, or `None`.
fn inline_one(
    stmts: &[ExpressionStatement],
    params: &[EcoString],
) -> Option<Vec<ExpressionStatement>> {
    (0..stmts.len()).find_map(|index| inline_binding(stmts, index, params))
}

/// Inlines the stored block bound by `stmts[index]`, or `None` when that
/// statement is not a promotable binding.
fn inline_binding(
    stmts: &[ExpressionStatement],
    index: usize,
    params: &[EcoString],
) -> Option<Vec<ExpressionStatement>> {
    let Expression::Assignment { target, value, .. } = &stmts[index].expression else {
        return None;
    };
    let (Expression::Identifier(id), Expression::Block(block)) = (target.as_ref(), value.as_ref())
    else {
        return None;
    };
    let name = id.name.as_str();
    if params.iter().any(|p| p == name) || !binding_is_promotable(stmts, index, id, block, params) {
        return None;
    }
    let mut out: Vec<ExpressionStatement> = Vec::with_capacity(stmts.len());
    for (position, stmt) in stmts.iter().enumerate() {
        match position.cmp(&index) {
            std::cmp::Ordering::Less => out.push(stmt.clone()),
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Greater => out.extend(rewrite_statement(stmt, name, block)?),
        }
    }
    Some(out)
}

/// Whether the binding `stmts[index]` (`id := block`) satisfies every rule in
/// the module docs.
fn binding_is_promotable(
    stmts: &[ExpressionStatement],
    index: usize,
    id: &Identifier,
    block: &Block,
    params: &[EcoString],
) -> bool {
    let name = id.name.as_str();
    if !bound_once_and_unshadowed(stmts, id) {
        return false;
    }
    let mut self_use = false;
    let mut returns = false;
    for stmt in &block.body {
        walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Identifier(i) if i.name == id.name => self_use = true,
            Expression::Return { .. } => returns = true,
            _ => {}
        });
    }
    if self_use || returns {
        return false;
    }
    // Every later reference is a safe `value`-family send, and there is at
    // least one (a binding that is never sent `value` escapes).
    let uses = |range: &[ExpressionStatement]| {
        range
            .iter()
            .map(|s| stored_block_var_uses(&s.expression, name))
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2))
    };
    if uses(&stmts[..index]) != (false, false) {
        return false;
    }
    let (has_unsafe, has_safe) = uses(&stmts[index + 1..]);
    if has_unsafe || !has_safe {
        return false;
    }
    // The block writes an outer local, and the Tier 2 protocol threads each
    // write; it writes no field (a value-type field write is a second family).
    let mut bound: HashSet<EcoString> = params.iter().cloned().collect();
    for stmt in &stmts[..index] {
        bound.extend(
            crate::semantic_analysis::block_facts::names_assigned_outside_nested_blocks(
                &stmt.expression,
            ),
        );
    }
    let writes = outer_local_writes(block, &|n| bound.contains(n));
    let captured = captured_local_mutations(block);
    if writes.is_empty() || !writes.iter().all(|w| captured.iter().any(|c| *c == w.name)) {
        return false;
    }
    let analysis = analyze_block(block);
    if !analysis.field_writes.is_empty() {
        return false;
    }
    // The block closed over the locals as they were at the binding: an
    // assignment to one of them before the last send would be seen by the
    // inlined block. Past the last send nothing reads the block, and the last
    // send's own statement may assign the value of the send to such a local
    // (`t := b value`), which happens after the block ran.
    let mut touched: HashSet<&str> = HashSet::new();
    touched.extend(analysis.captured_reads.iter().map(String::as_str));
    touched.extend(analysis.local_writes.iter().map(String::as_str));
    // A pattern binder (a `match:` arm, a destructuring assignment) of a name
    // the block touches would capture the inlined block's reads and writes.
    if binds_any_of(&stmts[index + 1..], &touched) {
        return false;
    }
    let last_use = (index + 1..stmts.len())
        .rev()
        .find(|&j| stored_block_var_uses(&stmts[j].expression, name).1)
        .unwrap_or(index);
    let assigns_touched = |expr: &Expression| {
        let mut found = false;
        walk_expression(expr, &mut |e| match e {
            Expression::Assignment { target, .. } if matches!(target.as_ref(), Expression::Identifier(t) if touched.contains(t.name.as_str())) =>
            {
                found = true;
            }
            Expression::DestructureAssignment { .. } => found = true,
            _ => {}
        });
        found
    };
    for stmt in &stmts[index + 1..last_use] {
        if assigns_touched(&stmt.expression) {
            return false;
        }
    }
    // In the last send's statement only the root assignment's target is exempt.
    let last = &stmts[last_use].expression;
    let evaluated = match last {
        Expression::Assignment { value, .. } => value.as_ref(),
        other => other,
    };
    if last_use > index && assigns_touched(evaluated) {
        return false;
    }
    parameters_are_unambiguous(stmts, index, block, params)
}

/// Whether `id` is assigned exactly once in `stmts` and no block parameter,
/// `match:` arm or destructuring pattern binds its name.
fn bound_once_and_unshadowed(stmts: &[ExpressionStatement], id: &Identifier) -> bool {
    // Bound once, never a block parameter, never mentioned by its own block.
    let mut assigns = 0usize;
    let mut shadowed = false;
    for stmt in stmts {
        walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Assignment { target, .. } => {
                if matches!(target.as_ref(), Expression::Identifier(t) if t.name == id.name) {
                    assigns += 1;
                }
            }
            Expression::Block(b) if b.parameters.iter().any(|p| p.name == id.name) => {
                shadowed = true;
            }
            // Every other binder the NameResolver defines fresh: `match:` arm
            // patterns and destructuring patterns.
            Expression::Match { .. } | Expression::DestructureAssignment { .. }
                if pattern_binders(e).contains(&id.name) =>
            {
                shadowed = true;
            }
            _ => {}
        });
    }
    assigns == 1 && !shadowed
}

/// Whether a `match:` arm or destructuring assignment in `stmts` binds one of
/// `names`.
fn binds_any_of(stmts: &[ExpressionStatement], names: &HashSet<&str>) -> bool {
    let mut clash = false;
    for stmt in stmts {
        walk_expression(&stmt.expression, &mut |e| {
            if pattern_binders(e)
                .iter()
                .any(|n| names.contains(n.as_str()))
            {
                clash = true;
            }
        });
    }
    clash
}

/// The names a `match:` expression's arm patterns or a destructuring
/// assignment's pattern bind (empty for any other expression), via the same
/// helpers the `NameResolver` defines them with.
fn pattern_binders(e: &Expression) -> Vec<EcoString> {
    match e {
        Expression::Match { arms, .. } => arms
            .iter()
            .flat_map(|arm| extract_match_arm_bindings(&arm.pattern).0)
            .map(|id| id.name)
            .collect(),
        Expression::DestructureAssignment { pattern, .. } => extract_pattern_bindings(pattern)
            .0
            .into_iter()
            .map(|id| id.name)
            .collect(),
        _ => Vec::new(),
    }
}

/// A block with parameters becomes `[p := arg. ...] value`: each parameter is
/// then a block-local assignment, which is the parameter exactly when no other
/// binder of its name exists in the method.
fn parameters_are_unambiguous(
    stmts: &[ExpressionStatement],
    index: usize,
    block: &Block,
    params: &[EcoString],
) -> bool {
    if block.parameters.is_empty() {
        return true;
    }
    let names: HashSet<&str> = block.parameters.iter().map(|p| p.name.as_str()).collect();
    if names.len() != block.parameters.len() || params.iter().any(|p| names.contains(p.as_str())) {
        return false;
    }
    let mut clash = false;
    let mut visit = |e: &Expression| match e {
        Expression::Identifier(i) if names.contains(i.name.as_str()) => clash = true,
        Expression::Block(b) if b.parameters.iter().any(|p| names.contains(p.name.as_str())) => {
            clash = true;
        }
        // Pattern binders are not inspected: any pattern binds names this pass
        // does not enumerate.
        Expression::Match { .. } | Expression::DestructureAssignment { .. } => clash = true,
        _ => {}
    };
    for (position, stmt) in stmts.iter().enumerate() {
        if position == index {
            // The binding's own block: only its patterns matter.
            for inner in &block.body {
                walk_expression(&inner.expression, &mut |e| {
                    if matches!(
                        e,
                        Expression::Match { .. } | Expression::DestructureAssignment { .. }
                    ) {
                        visit(e);
                    }
                });
            }
        } else {
            walk_expression(&stmt.expression, &mut visit);
        }
    }
    !clash
}

/// `stmt` with every `value`-family send to the stored local `name` replaced
/// by `block` evaluated in place, as one or more statements (a cascade on the
/// local is split into its sends), or `None` when a use cannot be rewritten.
fn rewrite_statement(
    stmt: &ExpressionStatement,
    name: &str,
    block: &Block,
) -> Option<Vec<ExpressionStatement>> {
    let with_expression = |expression: Expression| ExpressionStatement {
        comments: stmt.comments.clone(),
        expression,
        preceding_blank_line: stmt.preceding_blank_line,
    };
    // A cascade on the local is split when it is the statement, an assignment's
    // value or a `^` value; the last send then carries the assignment or `^`.
    let (mut sends, wrap) = match &stmt.expression {
        Expression::Cascade {
            receiver, messages, ..
        } if is_cascade_on(receiver, messages, name) => {
            (split_cascade(receiver, messages, name), None)
        }
        Expression::Assignment {
            target,
            value,
            type_annotation,
            span,
        } => match value.as_ref() {
            Expression::Cascade {
                receiver, messages, ..
            } if is_cascade_on(receiver, messages, name) => (
                split_cascade(receiver, messages, name),
                Some(Wrap::Assign(target.clone(), type_annotation.clone(), *span)),
            ),
            _ => (vec![stmt.expression.clone()], None),
        },
        Expression::Return { value, span } => match value.as_ref() {
            Expression::Cascade {
                receiver, messages, ..
            } if is_cascade_on(receiver, messages, name) => (
                split_cascade(receiver, messages, name),
                Some(Wrap::Return(*span)),
            ),
            _ => (vec![stmt.expression.clone()], None),
        },
        _ => (vec![stmt.expression.clone()], None),
    };
    if let Some(wrap) = wrap {
        if let Some(last) = sends.pop() {
            sends.push(wrap.around(last));
        }
    }
    let mut out = Vec::with_capacity(sends.len());
    for mut expression in sends {
        let mut failed = false;
        walk_expression_mut(&mut expression, &mut |e| {
            if !inline_send(e, name, block) {
                failed = true;
            }
        });
        if failed {
            return None;
        }
        out.push(with_expression(expression));
    }
    Some(out)
}

/// The assignment or `^` a split cascade's last send goes back under.
enum Wrap {
    Assign(
        Box<Expression>,
        Option<crate::ast::TypeAnnotation>,
        crate::source_analysis::Span,
    ),
    Return(crate::source_analysis::Span),
}

impl Wrap {
    fn around(self, send: Expression) -> Expression {
        match self {
            Self::Assign(target, type_annotation, span) => Expression::Assignment {
                target,
                value: Box::new(send),
                type_annotation,
                span,
            },
            Self::Return(span) => Expression::Return {
                value: Box::new(send),
                span,
            },
        }
    }
}

/// Whether the cascade's shared receiver is the local `name`.
fn is_cascade_on(receiver: &Expression, messages: &[CascadeMessage], name: &str) -> bool {
    matches!(normalize_cascade(receiver, messages).0, Expression::Identifier(i) if i.name == name)
}

/// The sends of a cascade on `name`, one per message, in order. The folded
/// first message keeps its span; a later message takes its own.
fn split_cascade(
    receiver: &Expression,
    messages: &[CascadeMessage],
    name: &str,
) -> Vec<Expression> {
    let shared = Expression::Identifier(Identifier::new(
        name,
        match normalize_cascade(receiver, messages).0 {
            Expression::Identifier(i) => i.span,
            other => other.span(),
        },
    ));
    let mut sends = Vec::with_capacity(messages.len() + 1);
    if let Expression::MessageSend {
        selector,
        arguments,
        span,
        ..
    } = receiver
    {
        sends.push(Expression::MessageSend {
            receiver: Box::new(shared.clone()),
            selector: selector.clone(),
            arguments: arguments.clone(),
            is_cast: false,
            span: *span,
        });
    }
    for message in messages {
        sends.push(Expression::MessageSend {
            receiver: Box::new(shared.clone()),
            selector: message.selector.clone(),
            arguments: message.arguments.clone(),
            is_cast: false,
            span: message.span,
        });
    }
    sends
}

/// Replaces `e` by the stored block evaluated in place when it is a
/// `value`-family send to the local `name`; `false` when `e` is a use that
/// cannot be rewritten (a cascade nested in another expression, a cast, an
/// argument count that does not match the block).
fn inline_send(e: &mut Expression, name: &str, block: &Block) -> bool {
    match e {
        Expression::Cascade {
            receiver, messages, ..
        } => !is_cascade_on(receiver, messages, name),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            span,
        } if matches!(receiver.as_ref(), Expression::Identifier(i) if i.name == name)
            && is_safe_value_family_selector(selector) =>
        {
            if *is_cast || arguments.len() != block.parameters.len() {
                return false;
            }
            let mut body: Vec<ExpressionStatement> = block
                .parameters
                .iter()
                .zip(std::mem::take(arguments))
                .map(|(param, arg)| {
                    ExpressionStatement::bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            param.name.clone(),
                            param.span,
                        ))),
                        type_annotation: None,
                        span: arg.span(),
                        value: Box::new(arg),
                    })
                })
                .collect();
            body.extend(block.body.iter().cloned());
            let inlined = Block::new(Vec::new(), body, block.span);
            *e = Expression::MessageSend {
                receiver: Box::new(Expression::Block(inlined)),
                selector: MessageSelector::Unary("value".into()),
                arguments: Vec::new(),
                is_cast: false,
                span: *span,
            };
            true
        }
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;
    use crate::unparse::unparse_method;

    /// The class method `m` of `Object subclass: P` with `body` (one statement
    /// per line, indented), rewritten and unparsed, or `None` when unchanged.
    fn inlined(body: &str) -> Option<String> {
        let src = format!("Object subclass: P\n  class m =>\n{body}\n");
        let module = parse_bt(&src);
        let method = &module.classes[0].class_methods[0];
        let params: Vec<EcoString> = method
            .parameters
            .iter()
            .map(|p| p.name.name.clone())
            .collect();
        inline_stored_tier2_values(&method.body, &params).map(|stmts| {
            let mut m = method.clone();
            m.body = stmts;
            unparse_method(&m)
        })
    }

    #[test]
    fn unary_value_is_the_literal_block_evaluated_in_place() {
        let out = inlined("    t := 0\n    b := [t := t + 1]\n    b value\n    t").unwrap();
        assert!(out.contains("[t := t + 1] value"), "{out}");
        assert!(!out.contains("b :="), "{out}");
    }

    #[test]
    fn parameters_become_block_local_assignments() {
        let out = inlined("    t := 0\n    b := [:n | t := t + n]\n    b value: 5\n    t").unwrap();
        assert!(out.contains("n := 5"), "{out}");
        assert!(out.contains("] value"), "{out}");
    }

    #[test]
    fn cascade_is_split_into_sends() {
        let out =
            inlined("    t := 0\n    b := [:n | t := t + n]\n    b value: 1; value: 2\n    t")
                .unwrap();
        assert!(out.contains("n := 1") && out.contains("n := 2"), "{out}");
    }

    #[test]
    fn every_unsafe_shape_is_left_alone() {
        for body in [
            // Never sent `value`: the block escapes as the result.
            "    t := 0\n    b := [t := t + 1]\n    b",
            // Passed as an argument.
            "    t := 0\n    b := [t := t + 1]\n    #(1) do: b\n    t",
            // Reassigned.
            "    t := 0\n    b := [t := t + 1]\n    b := [t := t + 2]\n    b value\n    t",
            // Used inside another block.
            "    t := 0\n    b := [t := t + 1]\n    #(1) do: [:e | b value]\n    t",
            // A local the block reads is reassigned before the send.
            "    t := 0\n    b := [t := t + 1]\n    t := 10\n    b value\n    t",
            // Wrong argument count.
            "    t := 0\n    b := [:n | t := t + n]\n    b value\n    t",
            // Writes no outer local.
            "    b := [3]\n    b value",
            // A cascade nested in another expression.
            "    t := 0\n    b := [:n | t := t + n]\n    (b value: 1; value: 2) + 1\n    t",
            // A `match:` arm binder with the stored local's name shadows it.
            "    t := 0\n    b := [t := t + 1]\n    b value\n    r := [99] match: [b -> b value]\n    #[r, t]",
            // ... also when the arm binder is nested in a larger pattern.
            "    t := 0\n    b := [t := t + 1]\n    b value\n    r := [99] match: [{1, b} -> b value]\n    #[r, t]",
            // A destructuring assignment rebinds the stored local.
            "    t := 0\n    b := [t := t + 1]\n    {b, c} := Tuple withAll: #(1, 2)\n    b value\n    t",
            // A match arm binder named like a local the block touches.
            "    t := 0\n    b := [t := t + 1]\n    r := [99] match: [t -> b value]\n    #[r, t]",
            // A destructuring assignment rebinding a local the block touches.
            "    t := 0\n    b := [t := t + 1]\n    {t, c} := Tuple withAll: #(1, 2)\n    b value\n    t",
            // A block parameter named like the stored local.
            "    t := 0\n    b := [t := t + 1]\n    #(1) do: [:b | b printString]\n    b value\n    t",
            // A block-local assignment of the stored local's name in a nested block.
            "    t := 0\n    b := [t := t + 1]\n    [b := 5] value\n    b value\n    t",
        ] {
            assert!(inlined(body).is_none(), "{body}");
        }
    }
}
