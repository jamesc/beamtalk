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
//! - **writes.** A block that writes a `state:` field and is passed to
//!   `Timer`, to a send whose receiver is statically another actor, or to a
//!   class-side method that is not direct-called, "raises
//!   `actor_state_unreachable`".
//! - **reads.** A block that reads a `state:` field, or probes with `self
//!   hasField:`, in the same positions, "reads the values captured at
//!   creation". A synchronous call blocks the actor, so the capture is live
//!   unless the callee keeps the block and runs it later: the actor and
//!   class-method clauses say "may keep it". `Parallel all:`/`any:` always
//!   blocks, so reads there are exempt outright.
//! - **self-sends.** A block that makes a synchronous `self` send (a cast is
//!   fine) in the same positions "is served remotely and can deadlock if the
//!   actor is waiting on the callee".
//!
//! A block is reported at most once, whichever flavor finds it first (writes,
//! then reads, then self-sends).
//!
//! This is a lint, not a guarantee: a block passed through a variable, a
//! field, a parameter or an instance method is only caught at run time, and
//! only for writes. A block that stays home never fires: self-sends and casts
//! to `self`, inlined control flow, same-process collection operations,
//! `on:do:`/`ensure:` and `Result tryDo:` (their receivers are `self`, the
//! block, a local or a stdlib class), direct-called class methods (`class
//! sealed` methods of sealed stateless classes, judged by
//! [`ClassHierarchy::is_direct_call_eligible`], the rule codegen's
//! direct-call table uses), and blocks that touch no field and send nothing
//! to `self`.
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
//! `sync_send_in_timer_block` (at the send, about the deadlock); this lint
//! fires at the block, about the away execution. Both are accurate.
//!
//! Shapes the lint does not see: a block handed to a `Future` (runtime-only,
//! no hierarchy entry); `parallelCollect:` (an instance send on an unknown
//! receiver — the lint judges sends by receiver, except `Timer`/`Parallel`
//! via the hierarchy's `spawns_block` mark); a `super` send in the block
//! (away-`super` runs the superclass method against the capture and needs its
//! own message); a block passed as a `spawnWith:` argument; reflective
//! `fieldAt:`/`fieldAt:put:` access; a block reaching an actor held in a
//! field or a parameter; and a standalone definition for a class outside this
//! module.
//!
//! **Category.** These diagnostics carry `DiagnosticCategory::Lint`
//! (site-level `@expect lint`) until the sequenced `beamtalk-core` change
//! adds `DiagnosticCategory::ActorStateAbroad` with `@expect
//! actor_state_abroad` (BT-4011 follow-up): the switch is one
//! `.with_category(...)` call in each diagnostic builder below, plus a
//! suppression test mirroring `dead_block_assignment`'s.

use std::collections::HashSet;

use crate::{LintPass, hierarchy_for_lint};
use beamtalk_core::ast::{
    Block, ClassKind, DeclaredKeyword, Expression, ExpressionStatement, MethodKind, Module,
};
use beamtalk_core::ast_walker::{SendRef, walk_expression, walk_sends};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::semantic_analysis::block_facts::{HasFieldProbe, class_var_accesses};
use beamtalk_core::source_analysis::Diagnostic;

/// Lint pass that warns where a block touching actor state runs away from
/// home (see the module docs).
pub(crate) struct ActorStateAbroadPass;

impl LintPass for ActorStateAbroadPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        // See `hierarchy_for_lint` doc comment for why this is needed
        // instead of `class.class_kind`.
        let hierarchy = hierarchy_for_lint(module);
        for class in &module.classes {
            let class_name = class.name.name.as_str();
            let Some(fields) = actor_state_fields(module, &hierarchy, class_name) else {
                continue;
            };
            let ctx = ClassCtx {
                class_name,
                fields,
                hierarchy: &hierarchy,
            };
            for method in class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
            {
                check_method_body(&method.body, &ctx, diagnostics);
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
            let Some(fields) = actor_state_fields(module, &hierarchy, class_name) else {
                continue;
            };
            let ctx = ClassCtx {
                class_name,
                fields,
                hierarchy: &hierarchy,
            };
            check_method_body(&standalone.method.body, &ctx, diagnostics);
        }
    }
}

/// What the checks need to know about the class whose method is checked.
struct ClassCtx<'a> {
    class_name: &'a str,
    /// Mutable (`state:`) instance fields visible to the class's methods, own
    /// and inherited.
    fields: HashSet<String>,
    hierarchy: &'a ClassHierarchy,
}

/// The mutable instance fields of an actor class: `None` when `class_name`
/// is not an actor. Value types and plain objects have no actor cell, so a
/// block written in one has no away hazard; a standalone definition for a
/// class outside this module resolves non-actor too and is skipped.
///
/// Own `field:` (immutable) declarations are excluded: data that cannot
/// change reads the same anywhere, so warning on it would be noise. An
/// inherited `field:` keeps its slot in the set — its keyword is not visible
/// cross-module — and may warn on a read.
fn actor_state_fields(
    module: &Module,
    hierarchy: &ClassHierarchy,
    class_name: &str,
) -> Option<HashSet<String>> {
    if hierarchy.resolve_class_kind(class_name) != ClassKind::Actor {
        return None;
    }
    let own_immutable: HashSet<&str> = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == class_name)
        .map(|c| {
            c.state
                .iter()
                .filter(|d| d.declared_keyword == DeclaredKeyword::Field)
                .map(|d| d.name.name.as_str())
                .collect()
        })
        .unwrap_or_default();
    Some(
        hierarchy
            .all_state(class_name)
            .iter()
            .map(ToString::to_string)
            .filter(|name| !own_immutable.contains(name.as_str()))
            .collect(),
    )
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

    let actors = actor_locals(body, ctx);
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
            Self::Spawned { .. } | Self::Parallel { .. } => self.run_clause(class),
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
        send.arguments
            .iter()
            .filter_map(|a| match a.unwrap_parens() {
                Expression::Block(b) => Some(b),
                _ => None,
            })
    };
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
    let accesses = class_var_accesses(block, &ctx.fields);
    if let Some((field, _)) = accesses.writes.iter().min_by_key(|(_, span)| span.start()) {
        report(block, writes_diagnostic(ctx, block, field, shape));
    } else if !skip_reads && accesses.reads_class_state() {
        report(block, reads_diagnostic(ctx, block, &accesses, shape));
    } else if let Some(selector) = first_sync_self_send(block) {
        report(block, self_send_diagnostic(ctx, block, &selector, shape));
    }
}

impl ClassCtx<'_> {
    /// Shape A: a class-side send whose method carries the hierarchy's
    /// `spawns_block` mark (`Timer after:do:`/`every:do:`,
    /// `Parallel all:`/`any:`) — the same mark codegen reads.
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
    /// actor: a local bound to `ActorClass spawn...`, or an inline
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
            // `Worker spawn keep: [...]`.
            Expression::MessageSend {
                receiver: inner,
                selector: spawn,
                ..
            } => self.is_actor_spawn(inner, &spawn.name()).then(to_actor),
            _ => None,
        }
    }

    /// `ActorClass spawn` / `spawnWith:` / ... : an instance of an actor.
    fn is_actor_spawn(&self, receiver: &Expression, selector: &str) -> bool {
        selector.starts_with("spawn")
            && matches!(
                receiver.unwrap_parens(),
                Expression::ClassReference { name, package, .. }
                    if package.is_none() && self.hierarchy.is_actor_subclass(name.name.as_str())
            )
    }

    /// Shape C: a class-side send that runs in the target class's process.
    /// The target is any user-defined class that is not direct-called —
    /// including the actor's own class, whose class-side methods run in the
    /// class process, away from an instance method's actor process. A stdlib
    /// class is stateless and its blocks run at home (`Result tryDo:`,
    /// `Array`, ...; `Timer` and `Parallel` are Shape A, not this shape).
    fn class_side_shape(&self, receiver: &Expression, selector: &str) -> Option<AbroadShape> {
        let Expression::ClassReference { name, package, .. } = receiver.unwrap_parens() else {
            return None;
        };
        let target = name.name.as_str();
        if package.is_some() {
            return None;
        }
        if ClassHierarchy::is_builtin_class(target) {
            return None;
        }
        let method = self.hierarchy.find_class_method(target, selector)?;
        if ClassHierarchy::is_builtin_class(method.defined_in.as_str()) {
            return None;
        }
        // Direct-called in the caller (codegen's rule, shared): the block stays
        // home. Anything else runs in `target`'s process.
        (!self.hierarchy.is_direct_call_eligible(target, selector)).then(|| {
            AbroadShape::ClassMethod {
                target: target.to_string(),
                selector: selector.to_string(),
            }
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

/// Blocks carried one level inside a collection-literal argument:
/// `Parallel all:` takes a *list* of blocks, not a block, so its abroad
/// blocks are literal elements (`Parallel all: #([self.n := 1])`), never
/// direct arguments. A variable holding the list is not inspected.
fn parallel_list_blocks(arguments: &[Expression]) -> Vec<&Block> {
    let mut blocks = Vec::new();
    for arg in arguments {
        let (Expression::ListLiteral { elements, .. }
        | Expression::ArrayLiteral { elements, .. }
        | Expression::TupleLiteral { elements, .. }) = arg.unwrap_parens()
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

/// The first synchronous `self` send in `block`, in walk order, if any.
/// Casts (`self bump!`) message the actor's own mailbox and are fine;
/// cascades on `self` count (their messages are inherently synchronous).
/// Nested blocks included: a nested block runs wherever the block carrying
/// it runs.
fn first_sync_self_send(block: &Block) -> Option<String> {
    let mut found = None;
    for stmt in &block.body {
        walk_sends(&stmt.expression, &mut |send| {
            if found.is_none() && !send.is_cast && is_self(send.receiver) {
                found = Some(send.selector.name().to_string());
            }
        });
    }
    found
}

/// Returns `true` if `expr` is the identifier `self`.
fn is_self(expr: &Expression) -> bool {
    matches!(
        expr.unwrap_parens(),
        Expression::Identifier(id) if id.name.as_str() == "self"
    )
}

/// `` `hasField: #n` ``, `` `hasField:` (#a, #b) `` or, when no probe names a
/// symbol, `` `hasField:` ``.
fn probe_clause(probes: &std::collections::BTreeSet<HasFieldProbe>) -> String {
    let symbols: Vec<String> = probes
        .iter()
        .filter_map(|p| match p {
            HasFieldProbe::Symbol(name) => Some(format!("#{name}")),
            HasFieldProbe::Computed => None,
        })
        .collect();
    match symbols.as_slice() {
        [] => "`hasField:`".to_string(),
        [one] => format!("`hasField: {one}`"),
        many => format!("`hasField:` ({})", many.join(", ")),
    }
}

fn writes_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    field: &str,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    Diagnostic::lint(
        format!(
            "block writes field '{field}' of {class} and is {}: the write raises \
             `actor_state_unreachable`",
            shape.run_clause(class)
        ),
        block.span,
    )
    .with_hint(format!(
        "Return the value from the block and write the field from the calling method, \
         or send {class} a message that makes the write (ADR 0137)"
    ))
}

fn reads_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    accesses: &beamtalk_core::semantic_analysis::block_facts::ClassVarAccesses,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    let fields = accesses
        .reads
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .join(", ");
    let (what, hint) = if accesses.has_field_probes.is_empty() {
        (
            format!("reads field {fields} of {class}"),
            format!(
                "Read the field into a local before building the block, or have the block \
                 send {class} a message that returns it (ADR 0137)"
            ),
        )
    } else {
        let probe = probe_clause(&accesses.has_field_probes);
        if fields.is_empty() {
            (
                format!("probes the state of {class} with {probe}"),
                format!(
                    "Evaluate the `hasField:` test into a local before building the block, or \
                     have the block send {class} a message that makes it (ADR 0137)"
                ),
            )
        } else {
            (
                format!("reads field {fields} of {class} and probes its state with {probe}"),
                format!(
                    "Read the field and evaluate the `hasField:` test into locals before \
                     building the block, or have the block send {class} a message that does \
                     (ADR 0137)"
                ),
            )
        }
    };
    Diagnostic::lint(
        format!(
            "block {what} and is {}: it reads the values captured at creation",
            shape.read_clause(class)
        ),
        block.span,
    )
    .with_hint(hint)
}

fn self_send_diagnostic(
    ctx: &ClassCtx<'_>,
    block: &Block,
    selector: &str,
    shape: &AbroadShape,
) -> Diagnostic {
    let class = ctx.class_name;
    Diagnostic::lint(
        format!(
            "block sends `self {selector}` and is {}: the send is served by {class} remotely \
             and can deadlock if {class} is waiting on the callee",
            shape.run_clause(class)
        ),
        block.span,
    )
    .with_hint(format!(
        "Use `self {selector}!` (async cast) instead (ADR 0137)"
    ))
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
    /// four flavor verbs. Other passes' diagnostics on the same fixture
    /// (e.g. `sync_send_in_timer_block` on a Timer self-send) are excluded.
    fn abroad(source: &str) -> Vec<Diagnostic> {
        lint(source)
            .into_iter()
            .filter(|d| {
                d.message.starts_with("block writes field")
                    || d.message.starts_with("block reads field")
                    || d.message.starts_with("block probes the state")
                    || d.message.starts_with("block sends `self")
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
  class each: aBlock => aBlock value

  class sealed eachSealed: aBlock => aBlock value

sealed Object subclass: SealedLib
  class sealed each: aBlock => aBlock value

sealed Object subclass: StatefulSealed
  classState: seen = 0

  class sealed each: aBlock => aBlock value

Actor subclass: Counter
  state: n = 0
  state: onTick = nil
  field: tag = #c

  class each: aBlock => aBlock value

  bump => self.n := self.n + 1

";

    /// Appends instance methods to `Counter` (the last class of [`HEADER`])
    /// and lints.
    fn only(body: &str) -> Vec<Diagnostic> {
        abroad(&format!("{HEADER}{body}"))
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
        assert!(d[0].hint.is_some(), "expected a hint");
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
            d[0].message
                .contains("reads the values captured at creation"),
            "{}",
            d[0].message
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
        assert!(d[0].message.contains("can deadlock"), "{}", d[0].message);
        assert!(
            d[0].hint.as_ref().unwrap().contains("self bump!"),
            "hint should suggest async cast: {:?}",
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
    }

    #[test]
    fn parallel_read_quiet() {
        // `Parallel` blocks until its workers finish, so the capture is live.
        let d = only("  fanPeek => Parallel all: #([self.n printNl])\n");
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
            d[0].message.contains("raises `actor_state_unreachable`"),
            "{}",
            d[0].message
        );
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

    // ---- Shape C: non-direct class method ----------------------------------

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
        // Direct-called needs a sealed class too, not just a sealed method.
        let d = only("  viaLibSealed => Lib eachSealed: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
    }

    #[test]
    fn sealed_stateful_class_method_warns() {
        // Direct-called needs no class variables.
        let d = only("  viaStateful => StatefulSealed each: [self.n := 1]\n");
        assert_eq!(d.len(), 1, "{d:?}");
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
    }

    #[test]
    fn standalone_non_actor_quiet() {
        let d = abroad(&format!(
            "{HEADER}Lib >> helper => Timer after: 1 do: [self.n := 1]\n"
        ));
        assert!(d.is_empty(), "{d:?}");
    }
}
