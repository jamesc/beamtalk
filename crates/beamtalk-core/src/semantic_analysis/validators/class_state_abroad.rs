// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `class-state-abroad` lint (ADR 0130 §5, BT-3712).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A block literal written in a class method reads and writes its home
//! class's variables live while it runs inside an invocation of that class.
//! Outside one it reads the values the variables had when the block was
//! created, and a write raises `class_state_unreachable`. Where the compiler
//! can see the block leave home it says so, at the block:
//!
//! - **(a) reads.** A block literal that reads a class variable and is passed
//!   to an asynchronous send (a cast, or a class-side method that spawns its
//!   block such as `Timer after:do:`), handed to a statically known actor
//!   (which may keep it: a plain send to an actor is a synchronous call, so
//!   this case is broader than the synchronous class-side exemption below),
//!   stored in a local, class variable or literal, or returned, "reads the
//!   values captured at creation".
//! - **(b) writes.** A block literal that writes a class variable, directly or
//!   through a `self`/own-class send that
//!   [`compute_class_var_mutating_selectors`] cannot prove pure, and is passed
//!   to a class-side send whose receiver is statically another (user-defined)
//!   class and which is not direct-called, "raises `class_state_unreachable`".
//!   A send is direct-called in the caller (so a block passed there stays home)
//!   only for a `class sealed` method of a sealed class with no class
//!   variables ([`ClassHierarchy::is_direct_call_eligible`], the rule codegen's
//!   direct-call table uses): a `class sealed` method of an open or stateful
//!   class still runs in that class's process.
//!
//! This is a lint, not a guarantee: a block passed through a variable or an
//! instance method is only caught at run time, and only for writes. A block
//! that stays home never fires: inlined control flow, same-class class-side
//! higher-order methods (`self run: [...]`), `collect:`/`do:` on a local
//! collection, `on:do:`/`ensure:` and `Result tryDo:` are none of the shapes
//! above (their receivers are the block, a local, or a stdlib class).
//!
//! Known imprecision (intentional over-approximation): a block that sends an
//! inherited class method warns when that method, or a sealed method it
//! self-sends, makes a `self` send to a non-sealed selector of an open class
//! (BT-3717, BT-3736). The selector is resolved from the defining class, not
//! the receiving class, so `Leaf viaHelper` warns even when `Leaf` does not
//! override `helper`; a further subclass of `Leaf` still could, and the lint
//! does not check whether any override exists.
//!
//! Shapes the lint does not see(follow-up, none occurred in the Phase 0
//! census): a reading block passed to an actor held in a field or a
//! parameter, a reading block returned from inside a nested conditional
//! branch, a block reaching class state only through a self-send (the access
//! sits in the callee), a block handed to a `Future`, and a *writing* block
//! passed to an actor (`a each: [self bump]`), which runs in the actor's
//! process and raises, but rule (b) only covers class-side receivers.

use crate::ast::{Block, ClassDefinition, Expression, ExpressionStatement, MethodKind, Module};
use crate::ast_walker::{SendRef, walk_expression, walk_sends};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::{
    EscapeShape, analyze_method_body, class_var_accesses, compute_class_var_mutating_selectors,
    escaping_blocks,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use std::collections::{HashMap, HashSet};

/// A class defined in the module being validated, with the class-variable
/// mutating selectors of its own class methods.
struct ModuleClass<'a> {
    class: &'a ClassDefinition,
    mutating: HashSet<String>,
}

/// What the checks need to know about the class whose method is checked.
struct ClassCtx<'a> {
    class_name: &'a str,
    /// Class variables visible to the class (own and inherited).
    class_vars: HashSet<String>,
    hierarchy: &'a ClassHierarchy,
    module_classes: &'a HashMap<&'a str, ModuleClass<'a>>,
}

/// Warns about blocks that use their home class's variables where they run
/// outside an invocation of that class (see the module docs).
pub(crate) fn check_class_state_abroad(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let module_classes: HashMap<&str, ModuleClass<'_>> = module
        .classes
        .iter()
        .map(|class| {
            // Own and inherited: a subclass method writing an inherited class
            // variable is a writer too.
            let names: HashSet<String> = hierarchy
                .class_variable_names(class.name.name.as_str())
                .iter()
                .map(ToString::to_string)
                .collect();
            (
                class.name.name.as_str(),
                ModuleClass {
                    class,
                    mutating: compute_class_var_mutating_selectors(class, &names),
                },
            )
        })
        .collect();

    let mut check = |class_name: &str, body: &[ExpressionStatement]| {
        let class_vars: HashSet<String> = hierarchy
            .class_variable_names(class_name)
            .iter()
            .map(ToString::to_string)
            .collect();
        // A class with no class variable has nothing to read or write.
        if class_vars.is_empty() {
            return;
        }
        let ctx = ClassCtx {
            class_name,
            class_vars,
            hierarchy,
            module_classes: &module_classes,
        };
        check_method_body(body, &ctx, diagnostics);
    };

    for class in &module.classes {
        for method in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            check(class.name.name.as_str(), &method.body);
        }
    }
    // Standalone `Foo class >> bar => ...` definitions are class methods of
    // `Foo` too. Package-qualified extensions are left alone: their class is
    // resolved by package, which the simple-name hierarchy lookup does not model.
    for standalone in &module.method_definitions {
        if standalone.is_class_method
            && standalone.package.is_none()
            && standalone.method.kind == MethodKind::Primary
        {
            check(standalone.class_name.name.as_str(), &standalone.method.body);
        }
    }
}

fn check_method_body(
    body: &[ExpressionStatement],
    ctx: &ClassCtx<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // A block is reported at most once, whichever shape finds it first.
    let mut reported: HashSet<(u32, u32)> = HashSet::new();
    let mut report = |block: &Block, diagnostic: Diagnostic| {
        if reported.insert((block.span.start(), block.span.end())) {
            diagnostics.push(diagnostic);
        }
    };

    // (a) reads: returned or stored.
    for (block, shape) in escaping_blocks(body) {
        let reads = class_var_accesses(&block, &ctx.class_vars).read_names();
        if !reads.is_empty() {
            report(
                &block,
                reads_diagnostic(ctx, &block, &reads, &shape_clause(shape)),
            );
        }
    }

    let actors = actor_locals(body, ctx);
    for stmt in body {
        // Cascades expanded: a later cascade message is judged against the
        // shared receiver like any send (BT-3716).
        walk_sends(&stmt.expression, &mut |send| {
            check_send(ctx, &actors, send, &mut report);
        });
    }
}

/// Rules (a) (reads) and (b) (writes) for one send.
fn check_send(
    ctx: &ClassCtx<'_>,
    actors: &HashSet<String>,
    send: SendRef<'_>,
    report: &mut impl FnMut(&Block, Diagnostic),
) {
    let receiver = send.receiver;
    let sel = &send.selector.name();
    let blocks = || {
        send.arguments
            .iter()
            .filter_map(|a| match a.unwrap_parens() {
                Expression::Block(b) => Some(b),
                _ => None,
            })
    };
    // (a) reads: passed to a send that runs or keeps it elsewhere.
    if let Some(how) = ctx.abroad_send_clause(receiver, sel, send.is_cast, actors) {
        for block in blocks() {
            let reads = class_var_accesses(block, &ctx.class_vars).read_names();
            if !reads.is_empty() {
                report(block, reads_diagnostic(ctx, block, &reads, &how));
            }
        }
    }
    // (b) writes: passed to another class's class-side method.
    if let Some(target) = ctx.foreign_class_send_target(receiver, sel) {
        for block in blocks() {
            if let Some(what) = ctx.first_class_var_write(block) {
                report(block, writes_diagnostic(ctx, block, &what, &target, sel));
            }
        }
    }
}

impl ClassCtx<'_> {
    /// How a send hands its block argument somewhere it can run outside this
    /// invocation, as a clause for the diagnostic; `None` when it does not.
    ///
    /// - a cast (`!`) and a class-side method that spawns its block
    ///   (`Timer after:do:`) run the block asynchronously; `Parallel` blocks
    ///   until its workers finish, so the captured value is the live one;
    /// - a send to a statically known actor is a synchronous `gen_server:call`,
    ///   so an actor that merely evaluates the block runs it while this
    ///   invocation is blocked (like `Driver each: [self.n]`, which is exempt).
    ///   The hazard is an actor that *keeps* the block and runs it later, which
    ///   the compiler cannot see, so this case is deliberately broader than the
    ///   synchronous class-side exemption and worded "may keep it".
    fn abroad_send_clause(
        &self,
        receiver: &Expression,
        selector: &str,
        is_cast: bool,
        actors: &HashSet<String>,
    ) -> Option<String> {
        if is_cast {
            return Some(format!("passed to the asynchronous cast '{selector}'"));
        }
        let to_actor = || format!("handed to an actor by '{selector}', which may keep it");
        match receiver.unwrap_parens() {
            Expression::Identifier(id) => actors.contains(id.name.as_str()).then(to_actor),
            // `Worker spawn keep: [...]`.
            Expression::MessageSend {
                receiver: inner,
                selector: spawn,
                ..
            } => self.is_actor_spawn(inner, &spawn.name()).then(to_actor),
            Expression::ClassReference { name, package, .. } if package.is_none() => self
                .hierarchy
                .find_class_method(name.name.as_str(), selector)
                .filter(|m| m.spawns_block && m.defined_in != "Parallel")
                .map(|_| {
                    format!(
                        "passed to '{} {selector}', which runs it asynchronously",
                        name.name
                    )
                }),
            _ => None,
        }
    }

    /// `ActorClass spawn` / `spawnWith:` / `new` ... : an instance of an actor.
    fn is_actor_spawn(&self, receiver: &Expression, selector: &str) -> bool {
        selector.starts_with("spawn")
            && matches!(
                receiver.unwrap_parens(),
                Expression::ClassReference { name, package, .. }
                    if package.is_none() && self.hierarchy.is_actor_subclass(name.name.as_str())
            )
    }

    /// The class a class-side send reaches when its receiver is statically
    /// *another* user-defined class whose method runs in that class's process
    /// (it is not direct-called: see `ClassHierarchy::is_direct_call_eligible`).
    fn foreign_class_send_target(&self, receiver: &Expression, selector: &str) -> Option<String> {
        let Expression::ClassReference { name, package, .. } = receiver.unwrap_parens() else {
            return None;
        };
        let target = name.name.as_str();
        if package.is_some() || target == self.class_name {
            return None;
        }
        // A stdlib class is stateless and its blocks run at home (`Result
        // tryDo:`, `Array`, ...).
        if ClassHierarchy::is_builtin_class(target) {
            return None;
        }
        let method = self.hierarchy.find_class_method(target, selector)?;
        if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
            return None;
        }
        // Direct-called in the caller (codegen's rule, shared): the block stays
        // home. Anything else runs in `target`'s process.
        (!self.hierarchy.is_direct_call_eligible(target, selector)).then(|| target.to_string())
    }

    /// The first class-variable write `block` makes: a direct `self.n := ...`
    /// ([`class_var_accesses`]), else a `self`/own-class send, cascade messages
    /// included, that may write one. Nested blocks included.
    fn first_class_var_write(&self, block: &Block) -> Option<String> {
        if let Some(var) = class_var_accesses(block, &self.class_vars)
            .writes
            .into_iter()
            .next()
        {
            return Some(format!("class variable '{var}'"));
        }
        let mut found = None;
        for stmt in &block.body {
            walk_sends(&stmt.expression, &mut |send| {
                if found.is_some() {
                    return;
                }
                let name = send.selector.name();
                if let Some(by_reference) = self.own_class_receiver(send.receiver) {
                    if self.may_write_class_var(&name, by_reference) {
                        found = Some(format!("class variables through '{name}'"));
                    }
                }
            });
        }
        found
    }

    /// How `receiver` reaches this class's own class-side methods:
    /// `Some(false)` for `self` (late-bound), `Some(true)` for the class's own
    /// name (binds this class's method directly), `None` for anything else.
    fn own_class_receiver(&self, receiver: &Expression) -> Option<bool> {
        match receiver.unwrap_parens() {
            Expression::Identifier(id) if id.name == "self" => Some(false),
            Expression::ClassReference { name, package, .. }
                if package.is_none() && name.name == self.class_name =>
            {
                Some(true)
            }
            _ => None,
        }
    }

    /// Whether a class-side send of `selector` to this class may write a class
    /// variable: judged by [`compute_class_var_mutating_selectors`] run on the
    /// class that defines the method. A method whose defining class body is not
    /// in this module (a parent in another file, a standalone definition) is
    /// assumed to write; an unresolvable or stdlib selector is not guessed at.
    /// A `self` send late-binds, so when neither the class nor the method is
    /// sealed a subclass override may write even though this body is pure
    /// (the `CvsOverrideSub` shape): it cannot be proven pure either.
    fn may_write_class_var(&self, selector: &str, via_class_reference: bool) -> bool {
        let Some(method) = self.hierarchy.find_class_method(self.class_name, selector) else {
            return false;
        };
        if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
            return false;
        }
        let class_sealed = self
            .hierarchy
            .get_class(self.class_name)
            .is_some_and(|c| c.is_sealed);
        if !via_class_reference && !class_sealed && !method.is_sealed {
            return true;
        }
        // An inherited method, sealed or not, still late-binds its `self` sends
        // to the receiving class, which may override a non-sealed selector to
        // write (BT-3717, BT-3736). Reached through `ClassName sel` it is exact
        // only when the method is defined by that very class.
        if !(via_class_reference && method.defined_in == self.class_name)
            && self.late_binds_unsealed_self_send(method.defined_in.as_str(), selector)
        {
            return true;
        }
        match self.module_classes.get(method.defined_in.as_str()) {
            Some(defining)
                if defining
                    .class
                    .class_methods
                    .iter()
                    .any(|m| m.kind == MethodKind::Primary && m.selector.name() == selector) =>
            {
                defining.mutating.contains(selector)
            }
            _ => true,
        }
    }

    /// Whether the method `selector` defined by the *open* class `defining`
    /// (sealed or not; directly, or through the sealed methods it self-sends)
    /// makes a `self` send to a selector that is not sealed. Such a send binds
    /// late, to a subclass override this body cannot see (BT-3717, BT-3736). A
    /// selector that does not resolve, or resolves to a stdlib method, is not
    /// guessed at. Sent selectors are resolved from `defining`, not from the
    /// receiving class, so this over-approximates when the receiving class does
    /// not actually override the sent selector (see the module docs).
    fn late_binds_unsealed_self_send(&self, defining: &str, selector: &str) -> bool {
        let mut visited = HashSet::new();
        self.late_binds_from(defining, selector, &mut visited)
    }

    fn late_binds_from(
        &self,
        defining: &str,
        selector: &str,
        visited: &mut HashSet<String>,
    ) -> bool {
        // A sealed class has no subclass to override anything.
        if self
            .hierarchy
            .get_class(defining)
            .is_none_or(|c| c.is_sealed)
        {
            return false;
        }
        if !visited.insert(selector.to_string()) {
            return false;
        }
        let Some(class) = self.module_classes.get(defining).map(|m| m.class) else {
            return false;
        };
        let Some(def) = class
            .class_methods
            .iter()
            .find(|m| m.kind == MethodKind::Primary && m.selector.name() == selector)
        else {
            return false;
        };
        let sends = analyze_method_body(&def.parameters, &def.body).self_send_selectors;
        sends.iter().any(|sent| {
            let Some(target) = self.hierarchy.find_class_method(defining, sent) else {
                return false;
            };
            if ClassHierarchy::is_builtin_class(target.defined_in.as_str()) {
                return false;
            }
            if !target.is_sealed {
                return true;
            }
            self.late_binds_from(target.defined_in.as_str(), sent, visited)
        })
    }
}

/// Locals bound to an actor instance (`a := Worker spawn`) anywhere in `body`.
fn actor_locals(body: &[ExpressionStatement], ctx: &ClassCtx<'_>) -> HashSet<String> {
    let mut actors = HashSet::new();
    for stmt in body {
        walk_expression(&stmt.expression, &mut |e| {
            let Expression::Assignment { target, value, .. } = e else {
                return;
            };
            let Expression::Identifier(local) = &**target else {
                return;
            };
            if let Expression::MessageSend {
                receiver, selector, ..
            } = value.unwrap_parens()
            {
                if ctx.is_actor_spawn(receiver, &selector.name()) {
                    actors.insert(local.name.to_string());
                }
            }
        });
    }
    actors
}

fn shape_clause(shape: EscapeShape) -> String {
    match shape {
        EscapeShape::Returned => "returned".to_string(),
        EscapeShape::StoredLocal => "stored in a local".to_string(),
        EscapeShape::StoredClassVar => "stored in a class variable".to_string(),
        EscapeShape::StoredInLiteral => "stored in a collection literal".to_string(),
    }
}

fn reads_diagnostic(ctx: &ClassCtx<'_>, block: &Block, reads: &[String], how: &str) -> Diagnostic {
    let vars = reads.join(", ");
    let class = ctx.class_name;
    Diagnostic::warning(
        format!(
            "block reads class variable {vars} of {class} and is {how}: if run outside an \
             invocation of {class}, it reads the values captured at creation"
        ),
        block.span,
    )
    .with_hint(
        "Read the class variable into a local before building the block, or have the block \
         call a class method that returns it (ADR 0130 §5)",
    )
    .with_category(DiagnosticCategory::ClassStateAbroad)
}

fn writes_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    what: &str,
    target: &str,
    selector: &str,
) -> Diagnostic {
    let class = ctx.class_name;
    Diagnostic::warning(
        format!(
            "block writes {what} of {class} and is passed to '{target} {selector}', which runs \
             it in {target}'s process: outside an invocation of {class}, the write raises \
             `class_state_unreachable`"
        ),
        block.span,
    )
    .with_hint(
        "Return the value from the block and write the class variable from the calling \
         method (ADR 0130 §5)",
    )
    .with_category(DiagnosticCategory::ClassStateAbroad)
}
