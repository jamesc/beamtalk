// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stored-closure class-variable advisories (BT-3681, BT-3688, ADR 0110).
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
//! A send "may write" by [`ClassCtx::may_write_class_var`]: a method defined
//! by a class of this module is judged by the codegen gates' own rule
//! ([`compute_class_var_mutating_selectors`], reused, not copied) run on the
//! *defining* class (so a pure `class sealed` method inherited from a parent in
//! the same module is not flagged) and, for a `self` send, by whether a
//! subclass may override it; a user-defined method whose defining class's body
//! is not visible here (a parent in another file or package, or a method added
//! by a standalone `Foo class >> bar` definition) is assumed to write, which is
//! the same "assume the worst for a selector the class does not define" call
//! that rule makes.
//!
//! Class methods are checked wherever they are written: inside a class body
//! and as standalone `Foo class >> bar => ...` definitions.
//!
//! False positives are kept low: only classes that declare or inherit a class
//! variable are checked, and only blocks that make a class-side
//! `self`/own-class send to a user-defined (non-stdlib) class method are
//! considered; `[self foo] value`, `collect:`/`do:` and friends (blocks
//! invoked by the statement that builds them) are never flagged, and neither
//! is a stored block that is never invoked by a later statement, one that is
//! rebound before it is invoked, or one whose name a later block shadows with
//! its own parameter.
//!
//! A stored block counts as used by a later statement when it is invoked
//! (`b value`, ...), handed by name to a user-defined class-side higher-order
//! method (`self section: b`), or handed by name to a collection higher-order
//! method from the shared table
//! ([`opaque_fold_callable_arg`](crate::state_threading_selectors::opaque_fold_callable_arg):
//! `items do: b`).
//!
//! Deliberately conservative (kept, with tests pinning them): an invocation
//! after a *conditional* rebind (`cond ifTrue: [b := [0]]. b value`) still
//! warns, because when the branch is not taken `b` is still the stored block
//! and the check does not prove that every branch rebinds; a pure inherited
//! method whose defining class is not in this module is flagged; and a
//! collection HOM outside the shared table (or a user-defined *instance*-side
//! HOM) is not tracked.

use crate::ast::{Block, ClassDefinition, Expression, ExpressionStatement, MethodKind, Module};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::compute_class_var_mutating_selectors;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use crate::state_threading_selectors::opaque_fold_callable_arg;
use std::collections::{HashMap, HashSet};

/// A class defined in the module being validated, with the class-variable
/// mutating selectors of its own class methods.
struct ModuleClass<'a> {
    class: &'a ClassDefinition,
    mutating: HashSet<String>,
}

impl ModuleClass<'_> {
    /// Whether a method of this class, by selector, is defined in its body
    /// (as opposed to added by a standalone definition elsewhere).
    fn defines(&self, selector: &str) -> bool {
        self.class
            .class_methods
            .iter()
            .any(|m| m.kind == MethodKind::Primary && m.selector.name() == selector)
    }
}

/// Warns about stored closures / user-HOM block arguments whose class-side
/// self-send class-variable write is not kept (see the module docs).
pub(crate) fn check_stored_closure_class_var_writes(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let module_classes: HashMap<&str, ModuleClass<'_>> = module
        .classes
        .iter()
        .map(|class| {
            let class_var_names: HashSet<String> = class
                .class_variables
                .iter()
                .map(|cv| cv.name.name.to_string())
                .collect();
            (
                class.name.name.as_str(),
                ModuleClass {
                    class,
                    mutating: compute_class_var_mutating_selectors(class, &class_var_names),
                },
            )
        })
        .collect();

    let check_class_methods =
        |class_name: &str, bodies: &[&[ExpressionStatement]], diagnostics: &mut Vec<Diagnostic>| {
            // A class with no class variable (own or inherited) has nothing for
            // a send to write: skip it, so a class that never uses class state
            // is never flagged.
            if hierarchy.class_variable_names(class_name).is_empty() {
                return;
            }
            let is_sealed = module_classes.get(class_name).map_or_else(
                || hierarchy.get_class(class_name).is_some_and(|c| c.is_sealed),
                |c| c.class.is_sealed,
            );
            let ctx = ClassCtx {
                class_name,
                is_sealed,
                hierarchy,
                module_classes: &module_classes,
            };
            for body in bodies {
                check_method_body(body, &ctx, diagnostics);
            }
        };

    for class in &module.classes {
        let bodies: Vec<&[ExpressionStatement]> = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| m.body.as_slice())
            .collect();
        check_class_methods(class.name.name.as_str(), &bodies, diagnostics);
    }

    // BT-3688: standalone `Foo class >> bar => ...` definitions are class
    // methods of `Foo` too. Package-qualified extensions (`pkg@Foo class >>`)
    // are left alone: their class is resolved by package, which the simple-name
    // hierarchy lookup here does not model.
    for standalone in &module.method_definitions {
        if standalone.is_class_method
            && standalone.package.is_none()
            && standalone.method.kind == MethodKind::Primary
        {
            check_class_methods(
                standalone.class_name.name.as_str(),
                &[standalone.method.body.as_slice()],
                diagnostics,
            );
        }
    }
}

/// Checks one class-method body, and every block nested in it.
fn check_method_body(
    body: &[ExpressionStatement],
    ctx: &ClassCtx<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_hom_arguments(body, ctx, diagnostics);
    check_statement_list(body, ctx, diagnostics);
    for stmt in body {
        walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Block(block) = e {
                check_statement_list(&block.body, ctx, diagnostics);
            }
        });
    }
}

/// What the checks need to know about the class being validated.
struct ClassCtx<'a> {
    class_name: &'a str,
    /// Whether the class is `sealed` (no subclass can override a method).
    is_sealed: bool,
    hierarchy: &'a ClassHierarchy,
    /// Every class defined in the module, by name (an ancestor's body is
    /// visible only when it is in this module).
    module_classes: &'a HashMap<&'a str, ModuleClass<'a>>,
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
    /// - A method counts when a subclass may override it (for a `self` send,
    ///   neither the class nor the method is sealed): an override may write,
    ///   the very case ADR 0110 covers.
    /// - Otherwise it counts when [`compute_class_var_mutating_selectors`],
    ///   run on the class that defines it, cannot prove it free of
    ///   class-variable mutation. That needs the defining class's body: when it
    ///   is not in this module (or the method comes from a standalone
    ///   definition), the call is "not defined here, assume the worst".
    fn may_write_class_var(&self, selector: &str, via_class_reference: bool) -> bool {
        let Some(method) = self.hierarchy.find_class_method(self.class_name, selector) else {
            return false;
        };
        if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
            return false;
        }
        let overridable = !via_class_reference && !self.is_sealed && !method.is_sealed;
        if overridable {
            return true;
        }
        match self.module_classes.get(method.defined_in.as_str()) {
            Some(defining) if defining.defines(selector) => defining.mutating.contains(selector),
            _ => true,
        }
    }

    /// How `receiver` reaches this class's own class-side methods:
    /// `Some(false)` for `self`, `Some(true)` for the class's own name (an
    /// explicit reference), `None` for anything else.
    fn own_class_receiver(&self, receiver: &Expression) -> Option<bool> {
        match receiver {
            Expression::Identifier(id) if id.name == "self" => Some(false),
            Expression::ClassReference { name, package, .. }
                if package.is_none() && name.name == self.class_name =>
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
            .find_class_method(self.class_name, selector)
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
                    if let Some(later) = later_use(&stmts[i + 1..], &local.name, ctx) {
                        diagnostics.push(stored_closure_diagnostic(
                            &local.name,
                            &selector,
                            block,
                            &later,
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

/// How a later statement uses a stored closure.
enum LaterUse {
    /// `name value`, `name value: x`, ...: the closure is invoked there.
    Invoked(Span),
    /// `self section: name`: the closure is handed to a user-defined class-side
    /// higher-order method (named here). The compiler cannot see whether that
    /// method ever invokes it, only that the write is lost if it does.
    PassedTo(Span, String),
    /// `items do: name`: the closure is handed to a collection higher-order
    /// method (named here) that invokes it.
    PassedToCollection(Span, String),
}

/// Whether `inner` lies within `outer`.
fn span_within(inner: Span, outer: Span) -> bool {
    outer.start() <= inner.start() && inner.end() <= outer.end()
}

/// The source regions of `expr` in which the local `name` is no longer the
/// stored block (BT-3688): a nested block that declares `name` as a parameter
/// (the whole block), and the statements of a nested block that follow a
/// rebind of `name` in that same block's statement sequence.
fn rebinding_regions(expr: &Expression, name: &str) -> Vec<Span> {
    let is_local = |x: &Expression| matches!(x, Expression::Identifier(id) if id.name == name);
    let mut regions = Vec::new();
    walk_expression(expr, &mut |e| {
        let Expression::Block(block) = e else {
            return;
        };
        if block.parameters.iter().any(|p| p.name == name) {
            regions.push(block.span);
            return;
        }
        let rebind = block.body.iter().position(
            |s| matches!(&s.expression, Expression::Assignment { target, .. } if is_local(target)),
        );
        if let Some(next) = rebind.and_then(|k| block.body.get(k + 1)) {
            regions.push(Span::new(next.expression.span().start(), block.span.end()));
        }
    });
    regions
}

/// The first use, by a statement of `later`, of the local `name`: invoked
/// (`name value`, `name value: x`, ...), handed to a user-defined class-side
/// higher-order method (`self section: name`), or handed to a collection
/// higher-order method (`items do: name`). The search stops at a statement
/// that rebinds `name` (`name := ...`) without using it, since later
/// invocations then reach a different block, and skips regions where a nested
/// block shadows or rebinds `name` ([`rebinding_regions`]). A rebind nested in
/// a conditional does not stop the search: the branch may not run.
fn later_use(later: &[ExpressionStatement], name: &str, ctx: &ClassCtx<'_>) -> Option<LaterUse> {
    let is_local = |x: &Expression| matches!(x, Expression::Identifier(id) if id.name == name);
    for stmt in later {
        let regions = rebinding_regions(&stmt.expression, name);
        let live = |span: Span| !regions.iter().any(|r| span_within(span, *r));
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
            if !live(*span) {
                return;
            }
            let sel = selector.name();
            if is_local(receiver) && selector.is_block_invocation() {
                found = Some(LaterUse::Invoked(*span));
            } else if ctx.own_class_receiver(receiver).is_some()
                && ctx.is_user_defined_class_method(&sel)
                && arguments.iter().any(is_local)
            {
                found = Some(LaterUse::PassedTo(*span, sel.to_string()));
            } else if opaque_fold_callable_arg(&sel, arguments)
                .is_some_and(|callable| is_local(callable.unwrap_parens()))
            {
                found = Some(LaterUse::PassedToCollection(*span, sel.to_string()));
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
    later: &LaterUse,
) -> Diagnostic {
    let (use_clause, note, note_span) = match later {
        LaterUse::Invoked(span) => (
            "it is invoked by a later statement".to_string(),
            "the closure is invoked here".to_string(),
            *span,
        ),
        LaterUse::PassedTo(span, hom) => (
            format!("a later statement passes it to class-side '{hom}', which may invoke it"),
            format!("the closure is passed to class-side '{hom}' here"),
            *span,
        ),
        LaterUse::PassedToCollection(span, hom) => (
            format!("a later statement passes it to '{hom}', which invokes it"),
            format!("the closure is passed to '{hom}' here"),
            *span,
        ),
    };
    Diagnostic::warning(
        format!(
            "stored closure '{local}' sends class-side '{selector}', which may write a class \
             variable, but {use_clause}: that write (and any read of a \
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
    .with_note(note, Some(note_span))
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
