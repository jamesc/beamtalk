// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `actor-state-abroad` lint (ADR 0137, BT-4011).
//!
//! **DDD Context:** Compilation
//!
//! A block literal written in an actor's instance method reads and writes its
//! fields live while it runs inside the actor. Anywhere else (another actor's
//! process, a `Timer` callback, a spawned worker) it reads the values the
//! fields had when it was made, a write raises `actor_state_unreachable`, and
//! a synchronous `self` send is an ordinary message to the actor. Where the
//! compiler can see the block leave home it says so, at the block:
//!
//! - **writes.** A block that writes a field (`self.name := v`, `self
//!   fieldAt: #name put: v` or `self clearField: #name`) and is passed to
//!   `Timer`, to a send whose receiver is statically another actor, or to a
//!   class-side method that is not direct-called, "raises
//!   `actor_state_unreachable`".
//! - **reads.** A block that reads a `state:` field (`self.name` or `self
//!   fieldAt: #name`), or probes with `self hasField:`, in the same
//!   positions, "reads the values captured at creation". A synchronous call blocks the actor, so the capture is live
//!   unless the callee keeps the block and runs it later: the actor and
//!   class-method clauses say "may keep it". `Parallel all:`/`any:` always
//!   blocks, so reads there are exempt outright.
//! - **self-sends.** A block that makes a synchronous `self` send, directly
//!   or through a local bound to `self` (a cast is fine, and so is a
//!   reflective field access on `self` — `fieldAt:`, `fieldAt:put:`,
//!   `clearField:`, `hasField:` — which reads or writes the actor cell in
//!   place rather than messaging the actor), in the same
//!   positions "is served remotely rather than inline, and can deadlock if
//!   the actor is waiting on the callee".
//!
//! A block is reported at most once, in flavor priority order: writes, then
//! reads, then self-sends.
//!
//! This is a lint, not a guarantee: a block passed through a variable, a
//! field, a parameter or an instance method is only caught at run time, and
//! only for writes. A block that stays home never fires: self-sends and casts
//! to `self`, inlined control flow, same-process collection operations,
//! `on:do:`/`ensure:` and `Result tryDo:` (their receivers are `self`, the
//! block, a local or a stdlib class), class methods that run in the caller
//! (no class variables in the receiving class's chain, judged by
//! [`ClassHierarchy::runs_class_method_in_caller`], ADR 0142 §1), and blocks
//! that touch no field and send nothing to `self`.
//!
//! Differences from the `class-state-abroad` lint this is modelled on
//! (`semantic_analysis::validators::class_state_abroad`): there is no
//! stored/returned rule (ADR 0137: the lint sees only blocks passed directly
//! to `Timer`, another actor or a class method); a send to the actor's *own*
//! class's class-side method is abroad (the class process is not the actor
//! process); and there is no transitive write analysis (a `self` send from an
//! away block messages home, where its writes are kept — only direct field
//! writes raise). Field reads, writes and `hasField:` probes are read off
//! [`class_var_accesses`], which is generic over the name set it is given:
//! here the actor's mutable instance fields, not class variables.
//!
//! Overlap: a synchronous `self` send in a `Timer` block also fires
//! `sync_send_in_timer_block` (at the send); this lint fires at the block,
//! about the away execution.
//!
//! A block literal that is the *receiver* of a `spawns_block` send
//! (`[...] fork`, BT-4198) is away from home too and is judged like a
//! `Timer` block. Only a literal receiver is seen: `blk fork` on a variable
//! is missed.
//!
//! Cross-file classes resolve through the hierarchy `beamtalk lint` and the
//! MCP lint tool hand in (BT-4183): a subclass whose actor ancestor is
//! declared in another file is an actor, `Sub spawn` against it is an actor
//! receiver, and a standalone definition for an out-of-file actor is checked.
//! Called module-locally ([`crate::run_lint_passes`]), those classes resolve
//! non-actor and are skipped.
//!
//! Shapes the lint does not see: a block handed to a `Future` (runtime-only, no
//! hierarchy entry); `parallelCollect:` (an instance send on an unknown
//! receiver — the lint judges sends by receiver, except `Timer`/`Parallel`
//! via the hierarchy's `spawns_block` mark); a `super` send in the block
//! (away-`super` runs the superclass method against the capture and needs its
//! own message); a block nested in a `spawnWith:` argument map; reflective
//! field access through a local bound to `self` (only a literal `self`
//! receiver is a reflective access: an alias send is an ordinary message and
//! warns as a self-send); a block reaching an actor held in a field or
//! parameter not typed as another actor class, or an actor behind
//! `unwrap`/`value`.
//!
//! **Category.** These diagnostics carry
//! `DiagnosticCategory::ActorStateAbroad`: site-level `@expect
//! actor_state_abroad`, `[diagnostics]` key `actor-state-abroad`.

use std::collections::HashSet;

use crate::{LintContext, LintPass, async_cast_hint};
use beamtalk_core::ast::{
    Block, ClassDefinition, ClassKind, DeclaredKeyword, Expression, ExpressionStatement,
    MessageSelector, MethodKind, Module, ParameterDefinition, TypeAnnotation, WellKnownSelector,
};
use beamtalk_core::ast_walker::{SendRef, SendSite, walk_expression, walk_sends};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::semantic_analysis::actor_spawn::{actor_locals, is_actor_spawn};
use beamtalk_core::semantic_analysis::block_facts::{
    ClassVarAccesses, class_var_accesses, probe_clause,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory};

/// Lint pass that warns where a block touching actor state runs away from
/// home (see the module docs).
pub(crate) struct ActorStateAbroadPass;

impl LintPass for ActorStateAbroadPass {
    fn check(&self, module: &Module, ctx: &LintContext<'_>, diagnostics: &mut Vec<Diagnostic>) {
        // `ctx.hierarchy`, not `class.class_kind` (see `LintPass`): on the
        // lint surfaces it is the cross-file hierarchy, so an actor ancestor
        // declared in another file resolves (BT-4183).
        check_module(module, ctx.hierarchy, diagnostics);
    }
}

/// The pass over `module`, against `hierarchy`.
fn check_module(module: &Module, hierarchy: &ClassHierarchy, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let Some(fields) = actor_state_fields(module, hierarchy, class_name) else {
            continue;
        };
        let ctx = ClassCtx {
            class_name,
            fields,
            actor_fields: actor_typed_fields(module, hierarchy, class_name),
            hierarchy,
        };
        for method in class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            check_method_body(&method.body, &method.parameters, &ctx, diagnostics);
        }
    }
    // Standalone `Counter >> tick => ...` definitions are instance methods
    // of `Counter` too. Package-qualified extensions are left alone: their
    // class is resolved by package, which the simple-name hierarchy lookup
    // does not model.
    for standalone in &module.method_definitions {
        if standalone.is_class_method
            || standalone.package.is_some()
            || standalone.method.kind != MethodKind::Primary
        {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        let Some(fields) = actor_state_fields(module, hierarchy, class_name) else {
            continue;
        };
        let ctx = ClassCtx {
            class_name,
            fields,
            actor_fields: actor_typed_fields(module, hierarchy, class_name),
            hierarchy,
        };
        check_method_body(
            &standalone.method.body,
            &standalone.method.parameters,
            &ctx,
            diagnostics,
        );
    }
}

/// What the checks need to know about the class whose method is checked.
struct ClassCtx<'a> {
    class_name: &'a str,
    /// Mutable (`state:`) instance fields visible to the class's methods, own
    /// and inherited.
    fields: HashSet<String>,
    /// Fields (own and in-module-ancestor) declared `:: AnotherActor`.
    actor_fields: HashSet<String>,
    hierarchy: &'a ClassHierarchy,
}

/// The mutable instance fields of an actor class: `None` when `class_name`
/// is not an actor. Value types and plain objects have no actor cell, so a
/// block written in one has no away hazard; a standalone definition for a
/// class the hierarchy does not know resolves non-actor too and is skipped.
///
/// `field:` (immutable) declarations visible in this module are excluded:
/// data that cannot change reads the same anywhere, so warning on it would
/// be noise. A `field:` inherited from another module keeps its slot in the
/// set — its keyword is not visible cross-module — and may warn on a read.
fn actor_state_fields(
    module: &Module,
    hierarchy: &ClassHierarchy,
    class_name: &str,
) -> Option<HashSet<String>> {
    if hierarchy.resolve_class_kind(class_name) != ClassKind::Actor {
        return None;
    }
    // Most-derived declaration wins: a subclass redeclaring an ancestor's
    // `field:` as `state:` is mutable again.
    let mut seen = HashSet::new();
    let mut immutable = HashSet::new();
    for class in module_ancestry(module, class_name) {
        for declaration in &class.state {
            if seen.insert(declaration.name.name.as_str())
                && declaration.declared_keyword == DeclaredKeyword::Field
            {
                immutable.insert(declaration.name.name.as_str());
            }
        }
    }
    Some(
        hierarchy
            .all_state(class_name)
            .iter()
            .map(ToString::to_string)
            .filter(|name| !immutable.contains(name.as_str()))
            .collect(),
    )
}

/// The class and its in-module ancestors, nearest first: follows `superclass`
/// while it names another unqualified class in this module. Cycle-guarded.
/// Shared by the `field:` filter above and [`actor_typed_fields`].
fn module_ancestry<'m>(module: &'m Module, class_name: &'m str) -> Vec<&'m ClassDefinition> {
    let mut chain = Vec::new();
    let mut visited = HashSet::new();
    let mut current = Some(class_name);
    while let Some(name) = current {
        if !visited.insert(name) {
            break;
        }
        let Some(class) = module.classes.iter().find(|c| c.name.name.as_str() == name) else {
            break;
        };
        chain.push(class);
        current = class
            .superclass
            .as_ref()
            .filter(|_| class.superclass_package.is_none())
            .map(|superclass| superclass.name.as_str());
    }
    chain
}

/// Names of fields (own and in-module-ancestor, most-derived declaration
/// wins) declared `:: ActorClass` for statically another actor.
fn actor_typed_fields(
    module: &Module,
    hierarchy: &ClassHierarchy,
    class_name: &str,
) -> HashSet<String> {
    let mut seen = HashSet::new();
    let mut actors = HashSet::new();
    for class in module_ancestry(module, class_name) {
        for declaration in &class.state {
            if !seen.insert(declaration.name.name.as_str()) {
                continue;
            }
            if is_another_actor_annotation(
                declaration.type_annotation.as_ref(),
                hierarchy,
                class_name,
            ) {
                actors.insert(declaration.name.name.to_string());
            }
        }
    }
    actors
}

/// Parameter names declared `:: ActorClass` for statically another actor.
fn actor_typed_params(
    parameters: &[ParameterDefinition],
    hierarchy: &ClassHierarchy,
    class_name: &str,
) -> HashSet<String> {
    parameters
        .iter()
        .filter(|parameter| {
            is_another_actor_annotation(parameter.type_annotation.as_ref(), hierarchy, class_name)
        })
        .map(|parameter| parameter.name.name.to_string())
        .collect()
}

/// Whether a type annotation statically names another actor: a simple
/// `:: ActorClass` where the class is an actor subclass, but not the
/// enclosing class itself (which may hold `self`). Unions, generics and
/// anything else are not read.
fn is_another_actor_annotation(
    annotation: Option<&TypeAnnotation>,
    hierarchy: &ClassHierarchy,
    class_name: &str,
) -> bool {
    let Some(TypeAnnotation::Simple(name)) = annotation else {
        return false;
    };
    let annotated = name.name.as_str();
    annotated != class_name && hierarchy.is_actor_subclass(annotated)
}

fn check_method_body(
    body: &[ExpressionStatement],
    parameters: &[ParameterDefinition],
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

    let mut actors = actor_locals(ctx.hierarchy, body);
    actors.extend(actor_typed_params(
        parameters,
        ctx.hierarchy,
        ctx.class_name,
    ));
    for stmt in body {
        // Cascades expanded: a later cascade message is judged against the
        // shared receiver like any send (BT-3716).
        walk_sends(&stmt.expression, &mut |send| {
            check_send(ctx, &actors, send, &mut report);
        });
    }
}

/// How a send carries a block argument away from the actor.
enum AbroadShape {
    /// `Timer after:do:`/`every:do:` (or any non-`Parallel` `spawns_block`
    /// class method): runs the block later in another process while the actor
    /// moves on.
    Spawned { class: String, selector: String },
    /// A block literal that is the *receiver* of a send whose method carries
    /// the `spawns_block` mark (`[...] fork`): the receiver block itself runs
    /// in another process.
    Forked { selector: String },
    /// `Parallel all:`/`any:`: runs the block in another process while this
    /// call blocks.
    Parallel { class: String, selector: String },
    /// A send to statically another actor: runs the block in that actor's
    /// process.
    Actor { selector: String },
    /// A class-side send that runs in the target class's process.
    ClassMethod { target: String, selector: String },
}

impl AbroadShape {
    /// `passed to 'Timer after:do:', which runs it in another process`: the
    /// clause for the write and self-send rules, where even an immediately
    /// evaluated block hazards.
    fn run_clause(&self, class: &str) -> String {
        match self {
            Self::Spawned { class, selector } => {
                format!("passed to '{class} {selector}', which runs it in another process")
            }
            Self::Forked { selector } => {
                format!("sent '{selector}', which runs it in another process")
            }
            Self::Parallel { class, selector } => format!(
                "passed to '{class} {selector}', which runs it in another process \
                 while this call blocks"
            ),
            Self::Actor { selector } => format!(
                "handed to another actor by '{selector}', which runs it outside {class}'s process"
            ),
            Self::ClassMethod { target, selector } => {
                format!("passed to '{target} {selector}', which runs it in {target}'s process")
            }
        }
    }

    /// The read rule's clause: a synchronous call blocks the actor, so the
    /// capture is live unless the callee keeps the block and runs it later.
    fn read_clause(&self, class: &str) -> String {
        match self {
            Self::Actor { selector } => {
                format!("handed to another actor by '{selector}', which may keep it")
            }
            Self::ClassMethod { target, selector } => {
                format!("passed to '{target} {selector}', which may keep it")
            }
            Self::Spawned { .. } | Self::Forked { .. } | Self::Parallel { .. } => {
                self.run_clause(class)
            }
        }
    }
}

fn check_send(
    ctx: &ClassCtx<'_>,
    actors: &HashSet<String>,
    send: SendRef<'_>,
    report: &mut impl FnMut(&Block, Diagnostic),
) {
    let receiver = send.receiver;
    let sel = &send.selector.name();
    let direct_blocks = || {
        send.arguments.iter().filter_map(|a| match a {
            Expression::Block(b) => Some(b),
            _ => None,
        })
    };
    // Shape A': a block literal receiver whose method spawns its receiver
    // (`[...] fork`).
    if let Expression::Block(b) = receiver.unwrap_parens() {
        if ctx
            .hierarchy
            .find_method("Block", sel)
            .is_some_and(|m| m.spawns_block)
        {
            let shape = AbroadShape::Forked {
                selector: sel.to_string(),
            };
            check_block(ctx, b, &shape, false, report);
        }
    }
    // Shape A: a class-side send that runs its block in another process.
    if let Some(shape) = ctx.spawned_shape(receiver, sel) {
        // `Parallel` blocks until its workers finish, so a read there sees
        // the live value; a write still raises and a self-send still
        // deadlocks, so only reads are exempt.
        let is_parallel = matches!(shape, AbroadShape::Parallel { .. });
        for block in direct_blocks() {
            check_block(ctx, block, &shape, is_parallel, report);
        }
        if is_parallel {
            for block in parallel_list_blocks(send.arguments) {
                check_block(ctx, block, &shape, true, report);
            }
        }
    }
    // Shape B: a send to statically another actor.
    if let Some(shape) = ctx.actor_shape(receiver, sel, actors) {
        for block in direct_blocks() {
            check_block(ctx, block, &shape, false, report);
        }
    }
    // Shape C: a class-side send that is not direct-called.
    if let Some(shape) = ctx.class_side_shape(receiver, sel) {
        for block in direct_blocks() {
            check_block(ctx, block, &shape, false, report);
        }
    }
}

/// The write, read and self-send rules for one block carried by `shape`.
/// At most one fires per block (see `check_method_body`'s `reported` set):
/// writes first (a certain raise), then reads (certain staleness), then
/// self-sends (a possible deadlock).
fn check_block(
    ctx: &ClassCtx<'_>,
    block: &Block,
    shape: &AbroadShape,
    skip_reads: bool,
    report: &mut impl FnMut(&Block, Diagnostic),
) {
    // `class_var_accesses` is generic over the name set it is given: here
    // the actor's mutable fields, so `reads`/`writes` are field reads/writes
    // and `has_field_probes` are actor-state probes (nested blocks included).
    // Its `reflective_writes` (`fieldAt:put:`/`clearField:` on `self`,
    // BT-4184) are the same cell writes, reflectively, of any name: a write
    // away from home raises whatever field it names.
    let accesses = class_var_accesses(block, &ctx.fields);
    let first_write = accesses
        .writes
        .iter()
        .map(|(field, span)| {
            (
                span.start(),
                format!("writes field '{field}' of {}", ctx.class_name),
            )
        })
        .chain(
            accesses
                .reflective_writes
                .iter()
                .map(|w| (w.start, w.describe("field", ctx.class_name))),
        )
        .min_by_key(|(start, _)| *start);
    if let Some((_, what)) = first_write {
        report(block, writes_diagnostic(ctx, block, &what, shape));
    } else if !skip_reads && accesses.reads_class_state() {
        report(block, reads_diagnostic(ctx, block, &accesses, shape));
    } else if let Some(self_send) = first_sync_self_send(block) {
        report(block, self_send_diagnostic(ctx, block, &self_send, shape));
    }
}

impl ClassCtx<'_> {
    /// Shape A: a class-side send whose method carries the hierarchy's
    /// `spawns_block` mark (`Timer after:do:`/`every:do:`,
    /// `Parallel all:`/`any:`) — the same mark `class-state-abroad` reads.
    fn spawned_shape(&self, receiver: &Expression, selector: &str) -> Option<AbroadShape> {
        let Expression::ClassReference { name, package, .. } = receiver.unwrap_parens() else {
            return None;
        };
        if package.is_some() {
            return None;
        }
        let method = self
            .hierarchy
            .find_class_method(name.name.as_str(), selector)?;
        if !method.spawns_block {
            return None;
        }
        let class = name.name.to_string();
        let selector = selector.to_string();
        if method.defined_in == "Parallel" {
            Some(AbroadShape::Parallel { class, selector })
        } else {
            Some(AbroadShape::Spawned { class, selector })
        }
    }

    /// Shape B: a send, cast or plain, whose receiver is statically another
    /// actor: a local bound to `ActorClass spawn...` or typed `::
    /// ActorClass`, a field typed `:: ActorClass`, or an inline
    /// `ActorClass spawn...` chain. A send to `self` never matches (it runs
    /// in this process, threaded or bracketed); anything unresolvable is
    /// missed.
    fn actor_shape(
        &self,
        receiver: &Expression,
        selector: &str,
        actors: &HashSet<String>,
    ) -> Option<AbroadShape> {
        let to_actor = || AbroadShape::Actor {
            selector: selector.to_string(),
        };
        match receiver.unwrap_parens() {
            Expression::Identifier(id) => actors.contains(id.name.as_str()).then(to_actor),
            Expression::FieldAccess {
                receiver, field, ..
            } if receiver.is_self() && self.actor_fields.contains(field.name.as_str()) => {
                Some(to_actor())
            }
            // `Worker spawn keep: [...]`.
            Expression::MessageSend {
                receiver: inner,
                selector: spawn,
                ..
            } => is_actor_spawn(self.hierarchy, inner, &spawn.name()).then(to_actor),
            _ => None,
        }
    }

    /// Shape C: a class-side send that runs in the target class's process.
    /// The target is any user-defined class that is not direct-called —
    /// including the actor's own class, whose class-side methods run in the
    /// class process, away from an instance method's actor process — reached
    /// as a class reference or through `self class` / `super class`. A
    /// stdlib class is stateless and its blocks run at home (`Result tryDo:`,
    /// `Array`, ...; `Timer` and `Parallel` are Shape A, not this shape).
    ///
    /// A class reference asks `ClassHierarchy::class_reference_send_runs_in_caller`,
    /// which a shadowed name never satisfies (BT-4283): one last-wins runtime
    /// class that a reference reaches through the class registry, qualified
    /// or not, so this hierarchy's single entry cannot say the block stays
    /// home. `self class` is the running class itself.
    fn class_side_shape(&self, receiver: &Expression, selector: &str) -> Option<AbroadShape> {
        let (target, is_reference) = match receiver.unwrap_parens() {
            Expression::ClassReference { name, package, .. }
                if package.is_none() || self.hierarchy.is_shadowed_class(&name.name) =>
            {
                (name.name.as_str(), true)
            }
            Expression::MessageSend {
                receiver: inner,
                selector: class,
                arguments,
                ..
            } if class.name() == "class"
                && arguments.is_empty()
                && (inner.is_self() || inner.is_super()) =>
            {
                (self.class_name, false)
            }
            _ => return None,
        };
        if ClassHierarchy::is_builtin_class(target) {
            return None;
        }
        // A shadowed name's entry is only one of its rivals: whether it
        // defines the method says nothing about the class that answers.
        if !(is_reference && self.hierarchy.is_shadowed_class(target)) {
            let method = self.hierarchy.find_class_method(target, selector)?;
            if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
                return None;
            }
        }
        // Runs in the caller (ADR 0142 §1, the one rule): the block stays home.
        // Anything else runs in `target`'s process.
        let runs_in_caller = if is_reference {
            self.hierarchy
                .class_reference_send_runs_in_caller(target, selector)
        } else {
            self.hierarchy.runs_class_method_in_caller(target, selector)
        };
        (!runs_in_caller).then(|| AbroadShape::ClassMethod {
            target: target.to_string(),
            selector: selector.to_string(),
        })
    }
}

/// Whether `send` is a reflective field access on `self` that reads or
/// writes the actor cell in place (`fieldAt:`, `fieldAt:put:`,
/// `clearField:`, `hasField:`) rather than messaging the actor. A cascade
/// message is not one: a cascade dispatches every message as an ordinary
/// send.
fn is_reflective_self_access(send: &SendRef<'_>) -> bool {
    send.site == SendSite::Plain
        && send.receiver.is_self()
        && matches!(
            send.selector.well_known(),
            Some(
                WellKnownSelector::FieldAt
                    | WellKnownSelector::FieldAtPut
                    | WellKnownSelector::ClearField
                    | WellKnownSelector::HasField
            )
        )
}

/// Blocks carried one level inside a collection-literal argument:
/// `Parallel all:` takes a *list* of blocks, not a block, so its abroad
/// blocks are literal elements (`Parallel all: #([self.n := 1])`), never
/// direct arguments. A variable holding the list is not inspected.
fn parallel_list_blocks(arguments: &[Expression]) -> Vec<&Block> {
    let mut blocks = Vec::new();
    for arg in arguments {
        let (Expression::ListLiteral { elements, .. }
        | Expression::ArrayLiteral { elements, .. }
        | Expression::TupleLiteral { elements, .. }) = arg
        else {
            continue;
        };
        for element in elements {
            if let Expression::Block(block) = element.unwrap_parens() {
                blocks.push(block);
            }
        }
    }
    blocks
}

/// A synchronous send to the actor from inside a block.
struct SyncSelfSend {
    /// How to name the send in the message (`self bump`, `alias bump`).
    send: String,
    /// The concrete cast spelling for the hint (`self bump!`), when one
    /// exists: only a plain unary send — `!` terminates the full send, so
    /// `self update: 42!`, not `self update:!`, and a cascade message has
    /// no cast spelling.
    cast: Option<String>,
}

/// The first synchronous `self` send in `block`, in walk order, if any —
/// directly or through a local bound to `self` (ADR 0137 treats aliased
/// self-sends as self-sends). Casts (`self bump!`) message the actor's own
/// mailbox and are fine; cascades on `self` count (their messages are
/// inherently synchronous). Nested blocks included: a nested block runs
/// wherever the block carrying it runs.
fn first_sync_self_send(block: &Block) -> Option<SyncSelfSend> {
    let aliases = self_aliases(block);
    let mut found = None;
    for stmt in &block.body {
        walk_sends(&stmt.expression, &mut |send| {
            if found.is_some() || send.is_cast || is_reflective_self_access(&send) {
                return;
            }
            let Expression::Identifier(id) = send.receiver.unwrap_parens() else {
                return;
            };
            let name = id.name.as_str();
            if name != "self" && !aliases.contains(name) {
                return;
            }
            let send_display = format!("{name} {}", send.selector.name());
            let cast = (matches!(send.selector, MessageSelector::Unary(_))
                && send.site == SendSite::Plain)
                .then(|| format!("{send_display}!"));
            found = Some(SyncSelfSend {
                send: send_display,
                cast,
            });
        });
    }
    found
}

/// Locals bound to `self` anywhere in `block` (`alias := self`): a send to
/// one messages the actor like a `self` send (ADR 0137 treats aliased
/// self-sends as self-sends).
fn self_aliases(block: &Block) -> HashSet<String> {
    let mut aliases = HashSet::new();
    for stmt in &block.body {
        walk_expression(&stmt.expression, &mut |e| {
            let Expression::Assignment { target, value, .. } = e else {
                return;
            };
            let Expression::Identifier(local) = &**target else {
                return;
            };
            if value.is_self() {
                aliases.insert(local.name.to_string());
            }
        });
    }
    aliases
}

/// `what` names the write: `writes field 'n' of Counter`, or a reflective
/// [`ReflectiveWrite::describe`](beamtalk_core::semantic_analysis::block_facts::ReflectiveWrite::describe).
fn writes_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    what: &str,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    Diagnostic::lint(
        format!(
            "block {what} and is {}: the write raises `actor_state_unreachable`",
            shape.run_clause(class)
        ),
        block.span,
    )
    .with_hint(format!(
        "Return the value from the block and write the field from the calling method, \
         or send {class} an async cast (`!`) that makes the write (ADR 0137)"
    ))
    .with_category(DiagnosticCategory::ActorStateAbroad)
}

fn reads_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    accesses: &ClassVarAccesses,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    // `self.n` reads and reflective `fieldAt:` reads, joined: ``reads field n
    // of Counter and field m with `fieldAt:` ``.
    let reads = accesses.reads_phrase("field", class);
    // The fix advice and the verb for the Timer-only message alternative.
    let (what, base, verb) = if accesses.has_field_probes.is_empty() {
        (
            reads.unwrap_or_default(),
            "Read the field into a local before building the block",
            "returns it",
        )
    } else {
        let probe = probe_clause(&accesses.has_field_probes);
        if let Some(reads) = reads {
            (
                format!("{reads} and probes its state with {probe}"),
                "Read the field and evaluate the `hasField:` test into locals before \
                 building the block",
                "does",
            )
        } else {
            (
                format!("probes the state of {class} with {probe}"),
                "Evaluate the `hasField:` test into a local before building the block",
                "makes it",
            )
        }
    };
    // A `self` send that returns the value is safe from a `Timer` block (the
    // actor is free) but deadlocks from a synchronous call the actor waits
    // on, so only the `Timer` hint offers it.
    let hint = if matches!(
        shape,
        AbroadShape::Spawned { .. } | AbroadShape::Forked { .. }
    ) {
        format!("{base}, or have the block send {class} a message that {verb} (ADR 0137)")
    } else {
        format!(
            "{base}; a `self` send from the block can deadlock while {class} waits on this \
             call (ADR 0137)"
        )
    };
    Diagnostic::lint(
        format!(
            "block {what} and is {}: it reads the values captured at creation",
            shape.read_clause(class)
        ),
        block.span,
    )
    .with_hint(hint)
    .with_category(DiagnosticCategory::ActorStateAbroad)
}

fn self_send_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    self_send: &SyncSelfSend,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    let hint = async_cast_hint(self_send.cast.as_deref());
    Diagnostic::lint(
        format!(
            "block sends `{}` and is {}: the send is served by {class} remotely \
             rather than inline, and can deadlock if {class} is waiting on the callee",
            self_send.send,
            shape.run_clause(class)
        ),
        block.span,
    )
    .with_hint(hint)
    .with_category(DiagnosticCategory::ActorStateAbroad)
}

#[cfg(test)]
mod tests {
    use crate::run_lint_passes;
    use beamtalk_core::source_analysis::{Diagnostic, Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Parse failed for lint fixture: {parse_diags:?}"
        );
        run_lint_passes(&module)
    }

    /// This pass's diagnostics only: every message starts with one of the
    /// flavor verbs. Other passes' diagnostics on the same fixture
    /// (e.g. `sync_send_in_timer_block` on a Timer self-send) are excluded.
    fn abroad(source: &str) -> Vec<Diagnostic> {
        lint(source)
            .into_iter()
            .filter(|d| {
                d.message.starts_with("block writes field")
                    || d.message.starts_with("block writes a field")
                    || d.message.starts_with("block clears ")
                    || d.message.starts_with("block reads field")
                    || d.message.starts_with("block reads a field")
                    || d.message.starts_with("block probes the state")
                    || d.message.starts_with("block sends `")
            })
            .collect()
    }

    const HEADER: &str = "Actor subclass: Worker
  state: blk = nil

  keep: aBlock => self.blk := aBlock

  run => self.blk value

Actor subclass: Runner
  run: aBlock => aBlock value

Object subclass: Lib
  classState: hits = 0

  class each: aBlock => aBlock value

  class sealed eachSealed: aBlock => aBlock value

Object subclass: OpenLib
  class each: aBlock => aBlock value

sealed Object subclass: SealedLib
  class sealed each: aBlock => aBlock value

sealed Object subclass: StatefulSealed
  classState: seen = 0

  class sealed each: aBlock => aBlock value

Actor subclass: Counter
  state: n = 0
  state: onTick = nil
  field: tag = #c
  classState: ticks = 0

  class each: aBlock => aBlock value

  bump => self.n := self.n + 1

";

    /// Appends instance methods to `Counter` (the last class of [`HEADER`])
    /// and lints.
    fn only(body: &str) -> Vec<Diagnostic> {
        abroad(&format!("{HEADER}{body}"))
    }

    // ---- Shape A': Block fork (BT-4198) ------------------------------------

    #[test]
    fn fork_write_warns() {
        let d = only("  kick => [self.n := self.n + 1] fork\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert_eq!(d[0].severity, Severity::Lint);
        assert!(
            d[0].message.contains("writes field 'n' of Counter"),
            "{}",
            d[0].message
        );
        assert!(d[0].message.contains("sent 'fork'"), "{}", d[0].message);
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn fork_self_send_warns() {
        let d = only("  kick => [self bump] fork\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn fork_no_actor_state_quiet() {
        let d = only("  kick => [1 + 1] fork\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- Shape A: Timer ----------------------------------------------------

    #[test]
    fn timer_write_warns() {
        let d = only("  startLate => Timer after: 100 do: [self.n := self.n + 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert_eq!(d[0].severity, Severity::Lint);
        assert!(
            d[0].message.contains("writes field 'n' of Counter"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Timer after:do:'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].hint.as_ref().unwrap().contains("async cast"),
            "hint should suggest an async cast: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn timer_read_warns() {
        let d = only("  startPeek => Timer every: 1000 do: [self.n printNl]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("reads field n of Counter"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Timer every:do:'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message
                .contains("reads the values captured at creation"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].hint
                .as_ref()
                .unwrap()
                .contains("send Counter a message"),
            "Timer hint offers the message alternative: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn timer_self_send_warns() {
        // `sync_send_in_timer_block` fires on this fixture too; the `abroad`
        // filter excludes it, so this asserts exactly this pass's diagnostic.
        let d = only("  startTick => Timer after: 100 do: [self bump]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("remotely rather than inline"),
            "{}",
            d[0].message
        );
        assert!(d[0].message.contains("can deadlock"), "{}", d[0].message);
        assert!(
            d[0].hint.as_ref().unwrap().contains("self bump!"),
            "hint should suggest async cast: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn keyword_self_send_hint_is_generic() {
        // `!` terminates the full send: `self update:!` would not parse.
        let d = only("  startKw => Timer after: 1 do: [self update: 42]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self update:`"),
            "{}",
            d[0].message
        );
        let hint = d[0].hint.as_ref().unwrap();
        assert!(
            hint.contains("`!` after the full send"),
            "keyword hint must be generic: {hint:?}"
        );
        assert!(!hint.contains("update:!"), "invalid spelling: {hint:?}");
    }

    #[test]
    fn aliased_self_send_warns() {
        let d = only("  startAlias => Timer after: 1 do: [alias := self. alias bump]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `alias bump`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].hint.as_ref().unwrap().contains("alias bump!"),
            "hint should name the alias: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn timer_cast_to_self_quiet() {
        let d = only("  startWell => Timer after: 100 do: [self bump!]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn timer_block_touching_no_field_quiet() {
        let d = only("  start: logger =>\n    Timer after: 100 do: [logger info: \"tick\"]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn hasfield_probe_warns_as_read() {
        let d = only("  startProbe => Timer after: 1 do: [self hasField: #n]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("probes the state of Counter with `hasField: #n`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Timer after:do:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn computed_probe_warns() {
        let d = only("  probeDynamic: k => Timer after: 1 do: [self hasField: k]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("probes the state of Counter with `hasField:`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn read_and_probe_combined() {
        let d = only("  startBoth => Timer after: 1 do: [self.n printNl. self hasField: #m]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("reads field n of Counter and probes its state with `hasField: #m`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn timer_every_write_warns() {
        let d = only("  startEvery => Timer every: 5 do: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Timer every:do:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn paren_wrapped_block_warns() {
        let d = only("  startParen => Timer after: 1 do: ([self.n := 1])\n");
        assert_eq!(d.len(), 1, "{d:?}");
    }

    #[test]
    fn nested_touching_block_warns() {
        let d = only("  startNest =>\n    Timer after: 1 do: [items do: [:i | self.n := i]]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("writes field 'n'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Timer after:do:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn write_beats_self_send() {
        let d = only("  startBoth => Timer after: 1 do: [self.n := 1. self bump]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn read_beats_self_send() {
        let d = only("  startBoth => Timer after: 1 do: [self.n printNl. self bump]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("reads the values captured at creation"),
            "{}",
            d[0].message
        );
    }

    // ---- Reflective field access (BT-4184) ---------------------------------

    /// The one diagnostic `body` produces, checked to carry each of `parts`
    /// in its message.
    fn one_with(body: &str, parts: &[&str]) -> Diagnostic {
        let d = only(body);
        assert_eq!(d.len(), 1, "{d:?}");
        for part in parts {
            assert!(d[0].message.contains(part), "{part:?}: {}", d[0].message);
        }
        d.into_iter().next().unwrap()
    }

    #[test]
    fn reflective_fieldatput_warns_as_write() {
        let d = one_with(
            "  startPut => Timer after: 1 do: [self fieldAt: #n put: 1]\n",
            &[
                "block writes field 'n' of Counter with `fieldAt:put:`",
                "passed to 'Timer after:do:'",
                "raises `actor_state_unreachable`",
            ],
        );
        assert!(
            d.hint.as_ref().unwrap().contains("async cast"),
            "write hint: {:?}",
            d.hint
        );
    }

    #[test]
    fn reflective_computed_fieldatput_warns_as_write() {
        one_with(
            "  startPut: k => Timer after: 1 do: [self fieldAt: k put: 1]\n",
            &[
                "block writes a field of Counter with `fieldAt:put:`",
                "raises `actor_state_unreachable`",
            ],
        );
    }

    #[test]
    fn reflective_clearfield_warns_as_write() {
        let d = one_with(
            "  startClear => Timer after: 1 do: [self clearField: #onTick]\n",
            &[
                "block clears field 'onTick' of Counter with `clearField:`",
                "raises `actor_state_unreachable`",
            ],
        );
        assert!(
            d.hint.as_ref().unwrap().contains("async cast"),
            "write hint: {:?}",
            d.hint
        );
    }

    #[test]
    fn reflective_write_of_immutable_field_warns() {
        // A write away raises whatever field it names.
        one_with(
            "  startTag => Timer after: 1 do: [self fieldAt: #tag put: #d]\n",
            &["block writes field 'tag' of Counter with `fieldAt:put:`"],
        );
    }

    #[test]
    fn reflective_write_to_another_actor_warns() {
        one_with(
            "  hand => (Worker spawn) keep: [self fieldAt: #n put: 0]\n",
            &[
                "block writes field 'n' of Counter with `fieldAt:put:`",
                "handed to another actor by 'keep:'",
            ],
        );
    }

    #[test]
    fn reflective_fieldat_warns_as_read() {
        let d = one_with(
            "  startPeek => Timer after: 1 do: [(self fieldAt: #n) printNl]\n",
            &[
                "block reads field n of Counter with `fieldAt:`",
                "reads the values captured at creation",
            ],
        );
        assert!(
            d.hint
                .as_ref()
                .unwrap()
                .contains("Read the field into a local"),
            "read hint: {:?}",
            d.hint
        );
    }

    #[test]
    fn reflective_computed_fieldat_warns_as_read() {
        one_with(
            "  startPeek: k => Timer after: 1 do: [(self fieldAt: k) printNl]\n",
            &[
                "block reads a field of Counter with `fieldAt:`",
                "reads the values captured at creation",
            ],
        );
    }

    #[test]
    fn reflective_fieldat_class_method_may_keep_it() {
        one_with(
            "  peekVia => Lib each: [(self fieldAt: #n) printNl]\n",
            &[
                "block reads field n of Counter with `fieldAt:`",
                "passed to 'Lib each:', which may keep it",
            ],
        );
    }

    #[test]
    fn reflective_fieldat_immutable_field_quiet() {
        let d = only("  startTag => Timer after: 1 do: [(self fieldAt: #tag) printNl]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn direct_and_reflective_reads_combined() {
        one_with(
            "  startBoth => Timer after: 1 do: [self.n printNl. (self fieldAt: #onTick) printNl]\n",
            &["block reads field n of Counter and field onTick with `fieldAt:`"],
        );
    }

    #[test]
    fn reflective_read_and_probe_combined() {
        one_with(
            "  startBoth => Timer after: 1 do: [(self fieldAt: #n) printNl. self hasField: #m]\n",
            &["reads field n of Counter with `fieldAt:` and probes its state with `hasField: #m`"],
        );
    }

    #[test]
    fn reflective_write_beats_read_and_self_send() {
        one_with(
            "  startAll => Timer after: 1 do: [self.n printNl. self bump. self clearField: #n]\n",
            &["block clears field 'n' of Counter with `clearField:`"],
        );
    }

    #[test]
    fn earliest_write_names_the_block() {
        one_with(
            "  startTwo => Timer after: 1 do: [self fieldAt: #onTick put: 1. self.n := 2]\n",
            &["block writes field 'onTick' of Counter with `fieldAt:put:`"],
        );
        one_with(
            "  startTwo => Timer after: 1 do: [self.n := 2. self fieldAt: #onTick put: 1]\n",
            &["block writes field 'n' of Counter and is"],
        );
    }

    #[test]
    fn reflective_read_beats_self_send() {
        one_with(
            "  startBoth => Timer after: 1 do: [(self fieldAt: #n) printNl. self bump]\n",
            &["reads the values captured at creation"],
        );
    }

    #[test]
    fn reflective_cast_to_self_quiet() {
        let d = only("  startCast => Timer after: 1 do: [self fieldAt: #n put: 1!]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn aliased_reflective_write_warns_as_self_send() {
        // Only a literal `self` receiver reaches the cell; through an alias
        // it is an ordinary message to the actor.
        one_with(
            "  startAlias => Timer after: 1 do: [a := self. a fieldAt: #n put: 1]\n",
            &["sends `a fieldAt:put:`"],
        );
    }

    #[test]
    fn parallel_reflective_write_warns() {
        one_with(
            "  fanPut => Parallel all: #([self fieldAt: #n put: 1])\n",
            &[
                "block writes field 'n' of Counter with `fieldAt:put:`",
                "passed to 'Parallel all:'",
            ],
        );
    }

    #[test]
    fn parallel_reflective_read_quiet() {
        // `Parallel` blocks until its workers finish, so the capture is live;
        // a reflective read is a cell access, not a self-send.
        let d = only("  fanPeek => Parallel all: #([(self fieldAt: #n) printNl])\n");
        assert!(d.is_empty(), "{d:?}");
        let d = only("  fanProbe => Parallel all: #([self hasField: #n])\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- Shape A: Parallel -------------------------------------------------

    #[test]
    fn parallel_write_warns() {
        let d = only("  fanOut => Parallel all: #([self.n := 1])\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Parallel all:'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn parallel_self_send_warns() {
        let d = only("  fanTick => Parallel all: #([self bump])\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Parallel all:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn parallel_any_write_warns() {
        let d = only("  fanAny => Parallel any: #([self.n := 1])\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Parallel any:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn parallel_read_quiet() {
        // `Parallel` blocks until its workers finish, so the capture is live.
        let d = only("  fanPeek => Parallel all: #([self.n printNl]) timeout: 50\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- Shape B: another actor --------------------------------------------

    #[test]
    fn actor_local_write_warns() {
        let d = only("  poke =>\n    r := Runner spawn\n    r run: [self.n := 0]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'run:'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn actor_local_read_warns_it_may_keep_it() {
        let d = only("  peek =>\n    a := Worker spawn\n    a keep: [self.n]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("which may keep it"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message
                .contains("reads the values captured at creation"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].hint.as_ref().unwrap().contains("deadlock"),
            "sync-shape hint must not offer the message alternative: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn actor_inline_spawn_warns() {
        let d = only("  pokeInline => Worker spawn keep: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'keep:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn actor_cast_write_warns() {
        let d = only("  pokeCast =>\n    a := Worker spawn\n    a keep: [self.n := 1]!\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'keep:'"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn typed_param_receiver_warns() {
        let d = only("  pokeTyped: other :: Runner => other run: [self.n := 0]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'run:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn typed_field_receiver_warns() {
        let d = abroad(
            "Actor subclass: Runner
  run: aBlock => aBlock value

Actor subclass: Counter
  state: n = 0
  state: helper :: Runner = nil

  pokeField => self.helper run: [self.n := 0]
",
        );
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'run:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn own_class_typed_receiver_quiet() {
        // Typed as the enclosing class itself: it may hold `self`.
        let d = only("  pokeSelf: other :: Counter => other run: [self.n := 0]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn named_spawn_result_quiet() {
        // `spawnAs:` answers `Result(Self, Error)`, not an actor.
        let d = only("  named =>\n    a := Worker spawnAs: #w\n    a run: [self.n := 1]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn actor_nested_self_send_warns() {
        let d =
            only("  fanRun =>\n    r := Runner spawn\n    r run: [items do: [:i | self bump]]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn cascade_on_actor_warns() {
        // Later cascade messages are judged against the shared receiver.
        let d = only("  peekCasc =>\n    a := Worker spawn\n    a keep: [self.n]; run\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'keep:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn cascade_on_self_in_timer_warns_once() {
        let d = only("  startCasc => Timer after: 1 do: [self bump; tick]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
        // A cascade message has no cast spelling: the hint stays generic.
        assert!(
            d[0].hint
                .as_ref()
                .unwrap()
                .contains("`!` after the full send"),
            "cascade hint must be generic: {:?}",
            d[0].hint
        );
    }

    #[test]
    fn actor_param_receiver_quiet() {
        // A parameter may or may not hold another actor; the lint only judges
        // statically known actors.
        let d = only("  pokeVia: other => other run: [self.n := 0]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn block_through_variable_quiet() {
        // The lint sees only blocks passed directly.
        let d =
            only("  peekVia: other =>\n    b := [self.n]\n    self.n := 99\n    other run: b\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- Shape C: a class method that runs in its class process -----------
    //
    // ADR 0142 §1 (BT-4301): only a class whose chain declares class variables
    // keeps its process (`Lib`, `StatefulSealed`, `Counter` declare
    // `classState:`); sealing plays no part.

    #[test]
    fn open_class_method_write_warns() {
        let d = only("  viaLib => Lib each: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("passed to 'Lib each:', which runs it in Lib's process"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn sealed_method_of_open_class_warns() {
        // A `class sealed` method of a class with class variables still runs
        // in its class process.
        let d = only("  viaLibSealed => Lib eachSealed: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Lib eachSealed:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn sealed_stateful_class_method_warns() {
        // Direct-called needs no class variables.
        let d = only("  viaStateful => StatefulSealed each: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'StatefulSealed each:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn inherited_class_side_method_warns() {
        // An inherited method of a chain with class variables runs in the
        // receiving class's process.
        let d = abroad(
            "Object subclass: Lib
  classState: hits = 0

  class each: aBlock => aBlock value

Lib subclass: Kid
  kidHelper => nil

Actor subclass: Counter
  state: n = 0

  viaKid => Kid each: [self.n := 1]
",
        );
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Kid each:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn class_side_self_send_warns() {
        let d = only("  viaLibTick => Lib each: [self bump]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("sends `self bump`"),
            "{}",
            d[0].message
        );
        assert!(
            d[0].message.contains("passed to 'Lib each:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn own_class_side_method_warns() {
        // An instance method's home is the actor process; the class's own
        // class-side method runs in the class process.
        let d = only("  viaOwn => Counter each: [self.n]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message
                .contains("passed to 'Counter each:', which may keep it"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn direct_called_class_method_quiet() {
        let d = only("  viaSealed => SealedLib each: [self.n := 1]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn open_stateless_class_method_quiet() {
        // ADR 0142 §1 (BT-4301): a class with no class variables in its chain
        // runs its class methods in the caller, `class sealed` or not.
        let d = only("  viaOpen => OpenLib each: [self.n := 1]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    /// `body` appended to `Counter`, linted against this module's hierarchy
    /// with `shadowed` declared by more than one package in scope.
    fn only_with_shadowed(body: &str, shadowed: &[&str]) -> Vec<Diagnostic> {
        let (module, parse_diags) = parse(lex_with_eof(&format!("{HEADER}{body}")));
        assert!(parse_diags.is_empty(), "{parse_diags:?}");
        let mut hierarchy = crate::module_hierarchy_for_test(&module);
        hierarchy.set_shadowed_class_names(shadowed.iter().map(|&n| n.into()).collect());
        let mut diagnostics = Vec::new();
        super::check_module(&module, &hierarchy, &mut diagnostics);
        diagnostics
    }

    #[test]
    fn shadowed_stateless_class_method_warns() {
        // BT-4283: a name another package in scope also declares is one
        // last-wins runtime class, sent dynamically; the stateless `OpenLib`
        // this module declares need not be the class that answers, qualified
        // or not. The unshadowed control is `open_stateless_class_method_quiet`.
        for send in ["OpenLib", "dep@OpenLib"] {
            let d = only_with_shadowed(
                &format!("  viaOpen => {send} each: [self.n := 1]\n"),
                &["OpenLib"],
            );
            assert_eq!(d.len(), 1, "{send}: {d:?}");
            assert!(
                d[0].message.contains("passed to 'OpenLib each:'"),
                "{}",
                d[0].message
            );
        }
        let d = only_with_shadowed("  viaOpen => dep@OpenLib each: [self.n := 1]\n", &[]);
        assert!(d.is_empty(), "unshadowed qualified control: {d:?}");
    }

    #[test]
    fn shadowing_leaves_self_class_alone() {
        // `self class` is the running class itself, whatever its name: a
        // stateless actor's `self class each:` stays quiet when shadowed, as
        // in `self_class_direct_called_quiet`.
        let src = "sealed Actor subclass: SealedCounter
  state: n = 0

  class sealed each: aBlock => aBlock value

  viaSelf => self class each: [self.n := 1]
";
        let (module, _) = parse(lex_with_eof(src));
        let mut hierarchy = crate::module_hierarchy_for_test(&module);
        hierarchy.set_shadowed_class_names(std::iter::once("SealedCounter".into()).collect());
        let mut diagnostics = Vec::new();
        super::check_module(&module, &hierarchy, &mut diagnostics);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }

    #[test]
    fn inherited_stateless_class_method_quiet() {
        let d = abroad(
            "Object subclass: Lib
  class each: aBlock => aBlock value

Lib subclass: Kid
  kidHelper => nil

Actor subclass: Counter
  state: n = 0

  viaKid => Kid each: [self.n := 1]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn self_class_receiver_warns() {
        let d = only("  viaSelfClass => self class each: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Counter each:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn super_class_receiver_warns() {
        let d = only("  viaSuperClass => super class each: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Counter each:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn self_class_direct_called_quiet() {
        let d = abroad(
            "sealed Actor subclass: SealedCounter
  state: n = 0

  class sealed each: aBlock => aBlock value

  viaSelf => self class each: [self.n := 1]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- staying home stays quiet ------------------------------------------

    #[test]
    fn self_send_stays_home() {
        let d = only("  addTwice =>\n    self twice: [self.n := self.n + 1]\n    self.n\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn cast_to_self_stays_home() {
        let d = only("  saveLater => self keep: [self.n := 1]!\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn inlined_control_flow_stays_home() {
        let d = only("  sum: items =>\n    items do: [:i | self.n := self.n + i]\n    self.n\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn immutable_field_read_quiet() {
        // `field:` data cannot change, so the capture is always current.
        let d = only("  startTag => Timer after: 1 do: [self.tag printNl]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn inherited_immutable_field_read_quiet() {
        let d = abroad(
            "Actor subclass: Base
  field: tag = #c

Base subclass: Child
  state: n = 0

  startTag => Timer after: 1 do: [self.tag printNl]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn same_module_subclass_warns() {
        // A subclass in the same module resolves actor through its parent,
        // and sees inherited fields.
        let d = abroad(
            "Actor subclass: Base
  state: n = 0

Base subclass: Sub
  startLate => Timer after: 1 do: [self.n := 1]
",
        );
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("writes field 'n' of Sub"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn super_send_quiet() {
        // A `super` send in the block is a documented miss: away-`super`
        // runs the superclass method against the capture and needs its own
        // message.
        let d = only("  startSuper => Timer after: 1 do: [super tick]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn parallel_collect_quiet() {
        // An instance send on an unknown receiver: the lint judges sends by
        // receiver. The block touches state; the shape is the miss.
        let d = only("  collectP: xs => xs parallelCollect: [:x | self.n + x]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn untyped_field_receiver_quiet() {
        let d = abroad(
            "Actor subclass: Runner
  run: aBlock => aBlock value

Actor subclass: Counter
  state: n = 0
  state: helper = nil

  pokeHelper => self.helper run: [self.n := 1]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn spawnwith_map_block_quiet() {
        // A block nested in a `spawnWith:` argument map is not inspected.
        let d = only("  spawnCb => Worker spawnWith: #{#cb => [self.n := 1]}\n");
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn value_class_unchecked() {
        let d = abroad(
            "Value subclass: Point
  field: x = 0

  peek => Timer after: 1 do: [self.x printNl]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn plain_object_unchecked() {
        let d = abroad(
            "Object subclass: Datum
  state: n = 0

  poke => Timer after: 1 do: [self.n := 1]
",
        );
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn stored_block_in_own_actor_quiet() {
        // Stored in a field and run later in the actor's own process: home.
        let d = only("  arm => self.onTick := [self.n := self.n + 10]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- standalone definitions --------------------------------------------

    #[test]
    fn standalone_instance_method_warns() {
        let d = abroad(&format!(
            "{HEADER}Counter >> startLate => Timer after: 100 do: [self.n := 1]\n"
        ));
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("passed to 'Timer after:do:'"),
            "{}",
            d[0].message
        );
    }

    #[test]
    fn standalone_non_actor_quiet() {
        let d = abroad(&format!(
            "{HEADER}Lib >> helper => Timer after: 1 do: [self.n := 1]\n"
        ));
        assert!(d.is_empty(), "{d:?}");
    }

    #[test]
    fn standalone_unknown_class_quiet() {
        // A standalone definition for a class the hierarchy does not know
        // resolves non-actor and is skipped.
        let d = abroad("Ghost >> m => Timer after: 1 do: [self.n := 1]\n");
        assert!(d.is_empty(), "{d:?}");
    }

    // ---- cross-file hierarchy (BT-4183) ------------------------------------

    /// `source` linted through `run_lint_passes_with_hierarchy` against its
    /// module-local hierarchy with `other_file`'s classes injected the way
    /// `analyse_full` injects a package's out-of-file classes
    /// (`add_from_beam_meta`), filtered to this pass's diagnostics.
    fn abroad_with_other_file(source: &str, other_file: &str) -> Vec<Diagnostic> {
        use beamtalk_core::semantic_analysis::ClassHierarchy;
        let (other, other_diags) = parse(lex_with_eof(other_file));
        assert!(other_diags.is_empty(), "{other_diags:?}");
        let (module, parse_diags) = parse(lex_with_eof(source));
        assert!(parse_diags.is_empty(), "{parse_diags:?}");
        let mut hierarchy = crate::module_hierarchy_for_test(&module);
        hierarchy.add_from_beam_meta(ClassHierarchy::extract_class_infos(&other));
        crate::run_lint_passes_with_hierarchy(&module, &hierarchy)
            .into_iter()
            .filter(|d| {
                d.category
                    == Some(beamtalk_core::source_analysis::DiagnosticCategory::ActorStateAbroad)
            })
            .collect()
    }

    const BASE_FILE: &str = "Actor subclass: Base\n  state: count = 0\n";

    #[test]
    fn out_of_file_actor_ancestor_warns() {
        let src = "Base subclass: Sub\n  startLate => Timer after: 100 do: [self.count := 1]\n";
        let d = abroad_with_other_file(src, BASE_FILE);
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("count") && d[0].message.contains("Timer after:do:"),
            "{}",
            d[0].message
        );
        // Module-local only, `Base` is unknown and `Sub` resolves non-actor.
        assert!(abroad(src).is_empty(), "module-local lint must stay quiet");
    }

    #[test]
    fn out_of_file_actor_subclass_spawn_is_actor_receiver() {
        // `Sub spawn` resolves `Sub` as an actor through its out-of-file
        // ancestor, so a block handed to the spawned instance is abroad.
        let src = "Base subclass: Sub\n  run: aBlock => aBlock value\n\n\
                   Actor subclass: Counter\n  state: n = 0\n\n\
                   poke =>\n    s := Sub spawn\n    s run: [self.n := 1]\n";
        let d = abroad_with_other_file(src, BASE_FILE);
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(
            d[0].message.contains("handed to another actor by 'run:'"),
            "{}",
            d[0].message
        );
        assert!(abroad(src).is_empty(), "module-local lint must stay quiet");
    }

    #[test]
    fn standalone_definition_for_out_of_file_actor_warns() {
        let src = "Base >> startLate => Timer after: 1 do: [self.count := 1]\n";
        let d = abroad_with_other_file(src, BASE_FILE);
        assert_eq!(d.len(), 1, "{d:?}");
        assert!(abroad(src).is_empty(), "module-local lint must stay quiet");
    }

    // ── @expect actor_state_abroad suppression ─────────────────────────────

    /// `@expect actor_state_abroad` suppresses the actor-state-abroad lint.
    #[test]
    fn expect_actor_state_abroad_suppresses_lint() {
        let src = "Actor subclass: Counter\n  state: n = 0\n\n  bump => self.n := self.n + 1\n\n  startLate =>\n    @expect actor_state_abroad\n    Timer after: 100 do: [self.n := self.n + 1]\n";
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        let mut diags = Vec::new();
        crate::run_pass(&super::ActorStateAbroadPass, &module, &mut diags);
        // Apply @expect directives
        beamtalk_core::compilation::diagnostics_policy::apply_expect_directives(
            &module, &mut diags,
        );
        let lint_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Lint)
            .collect();
        assert!(
            lint_diags.is_empty(),
            "@expect actor_state_abroad should suppress lint, got: {lint_diags:?}"
        );
    }

    /// Lint has `ActorStateAbroad` category for `@expect` matching.
    #[test]
    fn lint_has_actor_state_abroad_category() {
        let diags = only("  startLate => Timer after: 100 do: [self.n := self.n + 1]\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].category,
            Some(beamtalk_core::source_analysis::DiagnosticCategory::ActorStateAbroad),
            "Expected ActorStateAbroad category on lint diagnostic"
        );
    }
}
