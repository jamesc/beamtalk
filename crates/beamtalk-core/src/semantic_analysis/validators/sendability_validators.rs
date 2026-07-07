// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sendability validators (ADR 0103) that run outside the type checker.
//!
//! The type checker owns the message-argument, `spawnWith:`, and Announcement
//! sendability checks (it has the inferred argument types in hand). This module
//! holds the checks that need data the type checker does not carry:
//!
//! * **Block-capture check (Phase 2, BT-2756):** a Phase 3 validator that joins
//!   the semantic-analysis `CapturedVar` set (name-only, computed after type
//!   checking) with the type checker's `type_map` to recover each captured
//!   variable's inferred type, then applies the shared [`sendability::tier_of`]
//!   core. This is genuinely new analysis — there is no closure-conversion pass
//!   and the existing capture datasets are invisible to the type checker
//!   (ADR 0103 §Checked boundaries #2).
//!
//! All findings reuse the single tier-derivation core so the tiers cannot drift
//! from the type checker's boundary checks.

use std::collections::HashMap;

use ecow::EcoString;

use crate::ast::{Block, ClassKind, Expression, Module};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::type_checker::sendability::{self, HandleScope, Tier};
use crate::semantic_analysis::{BlockInfo, ClassHierarchy};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use crate::state_threading_selectors::is_state_threading_keyword_selector;

/// ADR 0103 (Phase 2): warn when a block **sent to another process** captures a
/// process-bound handle (`HandleScoped(#process)`) — the handle is only usable
/// by its owning process, so the copy that lands in the receiver is dead.
///
/// A block crosses a process boundary when it is an argument to a postfix `!`
/// cast (the async send), a message to a `Timer` (`every:do:`, `after:do:`), or
/// a non-self, non-state-threading message to an actor. Blocks in local
/// positions (`do:`, `collect:`, `ifTrue:`, `whileTrue:`, self-sends) stay
/// silent. `#node`-scoped and `Dynamic` captures are silent in v1.
pub(crate) fn check_block_capture_sendability(
    module: &Module,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
    block_info: &HashMap<Span, BlockInfo>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut visit = |expr: &Expression| {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            ..
        } = expr
        else {
            return;
        };
        if !send_crosses_process_boundary(receiver, &selector.name(), *is_cast, type_map, hierarchy)
        {
            return;
        }
        for arg in arguments {
            if let Expression::Block(block) = arg {
                check_block_captures(block, hierarchy, type_map, block_info, diagnostics);
            }
        }
    };

    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut visit);
            }
        }
    }
    for standalone in &module.method_definitions {
        for stmt in &standalone.method.body {
            walk_expression(&stmt.expression, &mut visit);
        }
    }
    for stmt in &module.expressions {
        walk_expression(&stmt.expression, &mut visit);
    }
}

/// Whether a message send hands its block argument(s) to another process.
fn send_crosses_process_boundary(
    receiver: &Expression,
    selector: &str,
    is_cast: bool,
    type_map: &TypeMap,
    hierarchy: &ClassHierarchy,
) -> bool {
    // A postfix `!` cast is the async send (ADR 0104) — always cross-process.
    if is_cast {
        return true;
    }
    // Self-sends and state-threading selectors keep the block local (same
    // process; inline or Tier-2 promoted).
    let is_self_send = matches!(receiver, Expression::Identifier(id) if id.name == "self");
    if is_self_send || is_state_threading_keyword_selector(selector) {
        return false;
    }
    // `Timer every:do:` / `after:do:` schedule the block to run elsewhere.
    if is_timer_receiver(receiver) {
        return true;
    }
    // A message to an actor instance copies the block into the actor process.
    receiver_actor_class(receiver, type_map)
        .is_some_and(|class_name| hierarchy.is_actor_subclass(&class_name))
}

/// The `Timer` class reference (`Timer every:do:` etc.).
fn is_timer_receiver(receiver: &Expression) -> bool {
    matches!(receiver, Expression::ClassReference { name, .. } if name.name == "Timer")
}

/// The receiver's inferred class name, if known.
fn receiver_actor_class(receiver: &Expression, type_map: &TypeMap) -> Option<EcoString> {
    type_map
        .get(receiver.span())
        .and_then(|ty| ty.as_known())
        .cloned()
}

/// Warn for each `HandleScoped(#process)` value the block captures.
///
/// The authoritative *set* of captured variables comes from the semantic
/// analyser's `CapturedVar` list (name + definition span). Their **types**,
/// though, are recovered from each variable's use inside the block: the
/// captured var's definition span (a method param / outer binding) is not a
/// `type_map` key, whereas the type checker does record every identifier *use*.
fn check_block_captures(
    block: &Block,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
    block_info: &HashMap<Span, BlockInfo>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(info) = block_info.get(&block.span) else {
        return;
    };
    for captured in &info.captures {
        let Some(use_span) = first_use_span(block, captured.name.as_str()) else {
            continue;
        };
        let Some(ty) = type_map.get(use_span) else {
            continue;
        };
        let Tier::HandleScoped(HandleScope::Process) = sendability::tier_of(ty, hierarchy) else {
            continue;
        };
        let ty_name = ty
            .as_known()
            .cloned()
            .unwrap_or_else(|| EcoString::from("handle"));
        diagnostics.push(
            Diagnostic::warning(
                format!(
                    "block captures `{}` ({ty_name} — process-bound handle) and is sent to \
                     another process; the handle is only usable by its owning process",
                    captured.name
                ),
                block.span,
            )
            .with_hint("Capture data instead, or have an Actor that owns the handle do the work")
            .with_category(DiagnosticCategory::Sendability),
        );
    }
}

/// ADR 0103 (Phase 2): nudge FFI-wrapping `Object` classes to declare a
/// `handleScope:`.
///
/// Declaring `handleScope:` is what turns silence into (advisory) findings, so
/// authors of handle-wrapping classes are mildly incentivised *not* to declare
/// (ADR 0103 Negative consequences). This companion lint offsets that: an
/// `Object subclass:` constructed via the FFI-wrapping `native:` pattern
/// (ADR 0101) that has instance behaviour but **no** classification — neither a
/// builtin-table entry nor a `handleScope:` — gets an advisory hint.
///
/// Scoped to the FFI-wrapping pattern with instance methods so it does not fire
/// on ordinary Object classes (which wrap no runtime handle) or stateless
/// `native:` facades (class-method-only APIs like `Console`/`System`).
/// Suppressed under `stdlib_mode` — the builtin table already covers the stdlib.
pub(crate) fn check_undeclared_handle_class(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        // FFI-wrapping construction pattern (ADR 0101 `native:` / delegate).
        if class.backing_module.is_none() {
            continue;
        }
        // Already classified — nothing to nudge. Uses the inherited accessor so
        // a subclass that inherits a scope from a parent is not nudged either.
        if hierarchy.handle_scope(&class.name.name).is_some() {
            continue;
        }
        // Only Object-kind instances can carry a scoped handle.
        if hierarchy.resolve_class_kind(&class.name.name) != ClassKind::Object {
            continue;
        }
        // Stateless facades expose only class-side methods — no per-instance
        // handle to scope. Require instance behaviour before nudging.
        if class.methods.is_empty() {
            continue;
        }
        diagnostics.push(
            Diagnostic::hint(
                format!(
                    "FFI-wrapping class `{}` declares no `handleScope:` — its instances may \
                     wrap a runtime handle whose validity is scoped",
                    class.name.name
                ),
                class.name.span,
            )
            .with_hint(
                "Add a class-side `handleScope: #process` or `handleScope: #node` so sends of \
                 this handle across a process boundary are checked (ADR 0103)",
            )
            .with_category(DiagnosticCategory::Sendability),
        );
    }
}

/// Span of the first identifier use of `name` inside the block (recursing into
/// nested expressions/blocks), for recovering the captured variable's type.
fn first_use_span(block: &Block, name: &str) -> Option<Span> {
    let mut found: Option<Span> = None;
    for stmt in &block.body {
        walk_expression(&stmt.expression, &mut |e| {
            if found.is_none() {
                if let Expression::Identifier(id) = e {
                    if id.name.as_str() == name {
                        found = Some(id.span);
                    }
                }
            }
        });
        if found.is_some() {
            break;
        }
    }
    found
}
