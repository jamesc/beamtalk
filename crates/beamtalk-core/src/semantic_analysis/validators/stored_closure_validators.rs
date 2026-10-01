// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stored-closure class-variable advisories (BT-3681, ADR 0110).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Class-variable writes made through a class-side self-send are kept by
//! per-scope commit tokens (ADR 0110, BT-3675): every scope that cannot
//! thread a `ClassVars` rebind out lexically commits what its sends returned,
//! and its own refresh carries it on. Two shapes fall outside that scheme and
//! silently drop (or mis-read) the class variable:
//!
//! - A block bound to a local and invoked by a *later* statement
//!   (`b := [self foo]. b value. self noop`): the block exports into the scope
//!   of the statement that built it, which that statement's refresh already
//!   consumed.
//! - A block literal passed to a user-defined class-side higher-order method
//!   (`self section: [self foo]`): the callee returns the `ClassVars` it was
//!   given, so the block's write is overwritten (the callee's own writes are
//!   kept).
//!
//! The compiler already rejects the analogous `self.field :=` inside a stored
//! closure (`block_analyzer`). Class-variable writes here go through a
//! *send*, so whether the write happens is only known at run time (a subclass
//! override may write); this pass therefore warns rather than errors.
//!
//! A send "may write" by [`ClassCtx::may_write_class_var`]: a method this class
//! defines is judged by the codegen gates' own rule
//! ([`compute_class_var_mutating_selectors`], reused, not copied) and, for a
//! `self` send, by whether a subclass may override it; a user-defined method
//! this class only inherits is assumed to write, which is the same "assume the
//! worst for a selector the class does not define" call that rule makes.
//!
//! False positives are kept low: only classes that declare or inherit a class
//! variable are checked, and only blocks that make a class-side
//! `self`/own-class send to a user-defined (non-stdlib) class method are
//! considered; `[self foo] value`, `collect:`/`do:` and friends (blocks
//! invoked by the statement that builds them) are never flagged, and neither
//! is a stored block that is never invoked by a later statement, or one that
//! is rebound before it is invoked.
//!
//! Known gaps (false negatives): a stored block passed to a stdlib
//! higher-order method (`items do: b`), or invoked after a conditional rebind,
//! is not tracked; a stored block passed by name to a user-defined class-side
//! higher-order method is.

use crate::ast::{Block, ClassDefinition, Expression, ExpressionStatement, MethodKind, Module};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::compute_class_var_mutating_selectors;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use std::collections::HashSet;

/// Warns about stored closures / user-HOM block arguments whose class-side
/// self-send class-variable write is not kept (see the module docs).
pub(crate) fn check_stored_closure_class_var_writes(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        // A class with no class variable (own or inherited) has nothing for a
        // send to write: skip it, so a class that never uses class state is
        // never flagged.
        if hierarchy
            .class_variable_names(class.name.name.as_str())
            .is_empty()
        {
            continue;
        }
        let class_var_names: HashSet<String> = class
            .class_variables
            .iter()
            .map(|cv| cv.name.name.to_string())
            .collect();
        let mutating = compute_class_var_mutating_selectors(class, &class_var_names);
        let ctx = ClassCtx {
            class,
            hierarchy,
            mutating: &mutating,
        };
        for method in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            check_hom_arguments(&method.body, &ctx, diagnostics);
            check_statement_list(&method.body, &ctx, diagnostics);
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| {
                    if let Expression::Block(block) = e {
                        check_statement_list(&block.body, &ctx, diagnostics);
                    }
                });
            }
        }
    }
}

/// What the checks need to know about the class being validated.
struct ClassCtx<'a> {
    class: &'a ClassDefinition,
    hierarchy: &'a ClassHierarchy,
    mutating: &'a HashSet<String>,
}

impl ClassCtx<'_> {
    /// Whether a class-side send of `selector` may write a class variable that
    /// the surrounding scope cannot keep. `via_class_reference` is true for an
    /// explicit own-class receiver (`Counter foo`), which binds `Counter`'s
    /// method directly, so no subclass override of `foo` can run (BT-3666);
    /// `self foo` late-binds.
    ///
    /// Only user-defined class methods count (a stdlib class method is never a
    /// class-variable writer of the user's class, and an unresolvable selector
    /// is not guessed at).
    ///
    /// - A method this class defines counts when
    ///   [`compute_class_var_mutating_selectors`] cannot prove it free of
    ///   class-variable mutation, or, for a `self` send, when a subclass may
    ///   override it (neither the class nor the method is sealed): an override
    ///   may write, the very case ADR 0110 covers.
    /// - A user-defined method this class only inherits counts unconditionally,
    ///   whatever the sealing: that rule's "not defined locally, so assume the
    ///   worst" call (the class's own mutating set says nothing about it).
    fn may_write_class_var(&self, selector: &str, via_class_reference: bool) -> bool {
        let class_name = self.class.name.name.as_str();
        let Some(method) = self.hierarchy.find_class_method(class_name, selector) else {
            return false;
        };
        if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
            return false;
        }
        if method.defined_in.as_str() != class_name {
            return true;
        }
        let overridable = !via_class_reference && !self.class.is_sealed && !method.is_sealed;
        overridable || self.mutating.contains(selector)
    }

    /// How `receiver` reaches this class's own class-side methods:
    /// `Some(false)` for `self`, `Some(true)` for the class's own name (an
    /// explicit reference), `None` for anything else.
    fn own_class_receiver(&self, receiver: &Expression) -> Option<bool> {
        match receiver {
            Expression::Identifier(id) if id.name == "self" => Some(false),
            Expression::ClassReference { name, package, .. }
                if package.is_none() && name.name == self.class.name.name =>
            {
                Some(true)
            }
            _ => None,
        }
    }

    /// Whether `selector` names a user-defined (non-stdlib) class-side method
    /// of this class or an ancestor: a candidate user-defined higher-order
    /// method.
    fn is_user_defined_class_method(&self, selector: &str) -> bool {
        self.hierarchy
            .find_class_method(self.class.name.name.as_str(), selector)
            .is_some_and(|m| !ClassHierarchy::is_builtin_class(m.defined_in.as_str()))
    }

    /// The first selector sent to `self`/the own class inside `block` (nested
    /// blocks included) that may write a class variable.
    fn first_may_write_send(&self, block: &Block) -> Option<String> {
        let mut found = None;
        for stmt in &block.body {
            walk_expression(&stmt.expression, &mut |e| {
                if found.is_some() {
                    return;
                }
                if let Expression::MessageSend {
                    receiver, selector, ..
                } = e
                {
                    let name = selector.name();
                    if self
                        .own_class_receiver(receiver)
                        .is_some_and(|by_ref| self.may_write_class_var(&name, by_ref))
                    {
                        found = Some(name.to_string());
                    }
                }
            });
        }
        found
    }
}

/// Checks one statement list (a method body or a block body): every `b :=
/// [...]` whose block may write a class variable and whose local `b` is
/// invoked by a later statement of the list.
fn check_statement_list(
    stmts: &[ExpressionStatement],
    ctx: &ClassCtx<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (i, stmt) in stmts.iter().enumerate() {
        // A stored closure: `b := [ ... ]`.
        if let Expression::Assignment { target, value, .. } = &stmt.expression {
            if let (Expression::Identifier(local), Expression::Block(block)) =
                (target.as_ref(), value.as_ref())
            {
                if let Some(selector) = ctx.first_may_write_send(block) {
                    if let Some(call_span) = later_invocation(&stmts[i + 1..], &local.name, ctx) {
                        diagnostics.push(stored_closure_diagnostic(
                            &local.name,
                            &selector,
                            block,
                            call_span,
                        ));
                    }
                }
            }
        }
    }
}

/// Every block literal passed to a user-defined class-side higher-order method
/// (`self section: [self foo]`) inside `body` (nested blocks included) that
/// makes a send which may write a class variable.
fn check_hom_arguments(
    body: &[ExpressionStatement],
    ctx: &ClassCtx<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in body {
        walk_expression(&stmt.expression, &mut |e| {
            let Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } = e
            else {
                return;
            };
            if ctx.own_class_receiver(receiver).is_none() {
                return;
            }
            let hom = selector.name();
            if !ctx.is_user_defined_class_method(&hom) {
                return;
            }
            for arg in arguments {
                if let Expression::Block(block) = arg {
                    if let Some(inner) = ctx.first_may_write_send(block) {
                        diagnostics.push(hom_argument_diagnostic(&hom, &inner, block));
                    }
                }
            }
        });
    }
}

/// The span of the first statement of `later` that invokes the local `name`
/// (`name value`, `name value: x`, ...) or hands it to a user-defined class-side
/// higher-order method (`self section: name`), if any. The search stops at a
/// statement that rebinds `name` (`name := ...`) without using it, since later
/// invocations then reach a different block.
fn later_invocation(
    later: &[ExpressionStatement],
    name: &str,
    ctx: &ClassCtx<'_>,
) -> Option<crate::source_analysis::Span> {
    let is_local = |x: &Expression| matches!(x, Expression::Identifier(id) if id.name == name);
    for stmt in later {
        let mut found = None;
        walk_expression(&stmt.expression, &mut |e| {
            if found.is_some() {
                return;
            }
            let Expression::MessageSend {
                receiver,
                selector,
                arguments,
                span,
                ..
            } = e
            else {
                return;
            };
            let invoked = is_local(receiver) && selector.is_block_invocation();
            let handed_to_hom = ctx.own_class_receiver(receiver).is_some()
                && ctx.is_user_defined_class_method(&selector.name())
                && arguments.iter().any(is_local);
            if invoked || handed_to_hom {
                found = Some(*span);
            }
        });
        if found.is_some() {
            return found;
        }
        if let Expression::Assignment { target, .. } = &stmt.expression {
            if is_local(target) {
                return None;
            }
        }
    }
    None
}

fn stored_closure_diagnostic(
    local: &str,
    selector: &str,
    block: &Block,
    call_span: crate::source_analysis::Span,
) -> Diagnostic {
    Diagnostic::warning(
        format!(
            "stored closure '{local}' sends class-side '{selector}', which may write a class \
             variable, but it is invoked by a later statement: that write (and any read of a \
             class variable it relies on) is not kept\n\
             \n\
             = help: invoke the block in the statement that builds it (`[...] value`)\n\
             = help: or have the method return the value and write the class variable from the \
             method body"
        ),
        block.span,
    )
    .with_hint(
        "Invoke the block in the statement that builds it, or return the value and write the \
         class variable from the method body (ADR 0110)",
    )
    .with_note("the closure is invoked here", Some(call_span))
    .with_category(DiagnosticCategory::StoredClosure)
}

fn hom_argument_diagnostic(hom: &str, selector: &str, block: &Block) -> Diagnostic {
    Diagnostic::warning(
        format!(
            "block passed to class-side '{hom}' sends '{selector}', which may write a class \
             variable: that write is not kept (the user-defined method returns the class \
             variables it was given; its own writes are kept)\n\
             \n\
             = help: invoke the send outside the block, or return the value from the block and \
             write the class variable in the calling method"
        ),
        block.span,
    )
    .with_hint(
        "Make the class-side send in the calling method instead of inside the block passed to a \
         user-defined class-side higher-order method (ADR 0110)",
    )
    .with_category(DiagnosticCategory::StoredClosure)
}
