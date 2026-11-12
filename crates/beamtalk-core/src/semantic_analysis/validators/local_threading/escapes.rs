// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0133 escaping-block shapes ([`Escape`]) and the note every such
//! diagnostic carries.
//!
//! **DDD Context:** Semantic Analysis

use ecow::EcoString;

/// ADR 0133: how a block literal that assigns a captured local escapes, the
/// shape-specific part of the diagnostic (the ADR's error table).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Escape {
    /// Assigned to the local (`f := [...]`).
    Stored(EcoString),
    /// Returned from the method (the operand of `^` or the method's value);
    /// at the REPL root, the value of the input.
    Returned,
    /// Assigned to a field or class variable.
    StoredState,
    /// An element of a collection literal.
    InLiteral,
    /// The value of the block of a value-returning collection operation
    /// (`collect:`, `inject:into:`, `select:`, `detect:ifNone:`, ...): it
    /// leaves the block into the operation's machinery (its result, for
    /// `collect:` and the folds; a predicate's answer otherwise).
    CollectionResult,
    /// An argument of an ordinary send of this selector: a user-defined method,
    /// or a stdlib method outside the known list.
    Passed(EcoString),
    /// An argument of this collection message sent to `self` or `super` in
    /// an actor instance method: a dispatched self-send, so the actor's own
    /// method runs the block, not the inlined collection loop (BT-3908).
    ActorSelfSend(&'static str, &'static str, EcoString),
    /// A handler of this `Dictionary` lookup where the compiler does not lower
    /// it in place (BT-3798: its receiver is statically not a `Dictionary`,
    /// or the lookup is not in a producer position).
    Lookup(EcoString),
    /// An argument of this class-side selector, which runs in the class's
    /// process (not a direct-called `class sealed` method, ADR 0129 Phase 0b).
    ClassMethod(EcoString),
    /// An argument of an Erlang FFI send (`Erlang <module> <selector>`): the
    /// block is handed to Erlang as a plain fun (BT-3917).
    Ffi(EcoString, EcoString),
    /// An argument of a cascade message: a cascade sends every message
    /// through ordinary dispatch.
    Cascade(EcoString),
    /// The shared receiver of a cascade, named by its first message as
    /// written (BT-4603: `[...] whileTrue; yourself`, not the `whileTrue:` it
    /// is canonicalised to).
    CascadeReceiver(EcoString),
    /// The receiver of this selector, which does not run its receiver block in
    /// place.
    Receiver(EcoString),
    /// A block of this `on:do:`/`ensure:` send whose protected body or handler
    /// is not a block literal: the compiler runs its literal block as a closure
    /// (BT-4430, `block_facts::exception_send_inlined`).
    ExceptionNotInlined(EcoString),
    /// The condition of a `whileTrue:`/`whileFalse:` whose body is not a block
    /// literal: the loop runs its condition as a closure in every context
    /// (BT-4062, `block_facts::while_body_block`).
    ConditionBodyNotLiteral(EcoString),
}

impl Escape {
    /// What follows "`x` is assigned in a block " in the message. `method` is
    /// the enclosing method's selector, `None` at the REPL root.
    pub(super) fn fragment(&self, method: Option<&str>) -> String {
        match self {
            Self::Stored(local) => format!("stored in `{local}`"),
            Self::Returned => match method {
                Some(method) => format!("returned from `{method}`"),
                None => "that is the value of the expression".to_string(),
            },
            Self::StoredState => "stored in a field or class variable".to_string(),
            Self::InLiteral => "placed in a collection literal".to_string(),
            Self::CollectionResult => "answered by the block of a collection operation".to_string(),
            Self::Passed(selector) => format!(
                "passed to `{selector}`, which is not a construct the compiler threads it through"
            ),
            Self::ActorSelfSend(receiver, owner, selector) => format!(
                "passed to `{receiver} {selector}`, which runs the {owner} own `{selector}` method \
                 rather than the collection loop the compiler threads it through"
            ),
            Self::Lookup(selector) => format!(
                "passed to `{selector}` where the compiler does not thread it (the receiver is \
                 not known to be a `Dictionary`, or the lookup is not where it is lowered in place)"
            ),
            Self::ClassMethod(selector) => {
                format!("passed to a class method (`{selector}`); it would run in another process")
            }
            Self::Ffi(module, selector) => {
                format!("passed to Erlang `{module}` `{selector}`, which calls it as a plain fun")
            }
            Self::Cascade(selector) | Self::CascadeReceiver(selector) => format!(
                "sent in a cascade (`{selector}`), which the compiler does not thread it through"
            ),
            Self::Receiver(selector) => format!(
                "sent `{selector}`, which is not a construct the compiler threads it through"
            ),
            Self::ConditionBodyNotLiteral(selector) => format!(
                "used as the condition of a `{selector}` whose body is not a block literal, \
                 which the compiler runs as a closure and does not thread it through"
            ),
            Self::ExceptionNotInlined(selector) => format!(
                "of an `{selector}` whose blocks are not all written in place, which the compiler \
                 runs as a closure and does not thread it through"
            ),
        }
    }

    /// The fix: the ADR's idiom for the shape.
    pub(super) fn help(&self, name: &str) -> String {
        match self {
            Self::Stored(local) => format!(
                "have the block answer the new value and assign it where the block is \
                 called: `{local} := [{name} + 1]`, then `{name} := {local} value`"
            ),
            Self::Returned | Self::StoredState | Self::InLiteral | Self::CollectionResult => {
                format!(
                    "once the block has left this scope, no later call of it can hand `{name}` \
                 back: keep the state in a field (`self.{name} := self.{name} + 1`) or return \
                 the new value from the block and assign it where the block is called"
                )
            }
            Self::ActorSelfSend(_, "class's", selector) => format!(
                "send `{selector}` to the collection itself rather than to `self`, \
                 or return the new value from the block and assign it where it is called"
            ),
            Self::ActorSelfSend(_, _, selector) => format!(
                "send `{selector}` to the collection itself (`self.items {selector} [...]`), \
                 or return the new value from the block and assign it where it is called"
            ),
            Self::Passed(selector)
            | Self::Lookup(selector)
            | Self::ClassMethod(selector)
            | Self::Cascade(selector) => {
                format!(
                    "return the new value from the block and assign it where it is called: \
                     `{name} := ... {selector} [... {name} + 1]`"
                )
            }
            Self::Ffi(..) => format!(
                "the Erlang function cannot hand `{name}` back from a fun it calls: fold on the \
                 Beamtalk collection (`{name} := xs inject: {name} into: [:acc :e | acc + e]`), \
                 or return the new value from the block and use the Erlang call's result \
                 (`rs := Erlang lists map: [:e | ... {name} + e] with: xs`)"
            ),
            Self::Receiver(selector) | Self::CascadeReceiver(selector) => format!(
                "return the new value from the block and assign it where it is called: \
                 `{name} := [... {name} + 1] {selector}`"
            ),
            Self::ConditionBodyNotLiteral(selector) => format!(
                "write the loop body directly, wrapping a block value in a literal \
                 (`[...] {selector} [body value]`), or assign `{name}` in a literal loop body"
            ),
            Self::ExceptionNotInlined(selector) => format!(
                "write both blocks of the `{selector}` directly, wrapping a block value in a \
                 literal (`[b value] {selector} [...]`, `[...] {selector} [c value]`), or return \
                 the new value from the block and assign `{name}` where it is called"
            ),
        }
    }
}

/// The ADR 0133 note on every escaping-block diagnostic: the rule, and the
/// messages that do thread assignments, generated from
/// [`crate::state_threading_selectors::assignment_threading_constructs`] (the
/// one list codegen reads), so the note cannot drift from what compiles.
pub(super) fn threading_constructs_note() -> String {
    let constructs = crate::state_threading_selectors::assignment_threading_constructs();
    format!(
        "a block that is stored, returned, or passed to a method the compiler does not lower in \
         place may read captured variables but not assign them (ADR 0133). The messages that \
         thread assignments, with the block written directly as the argument, are the control \
         constructs ({}) and the collection messages ({})",
        constructs.control.join(", "),
        constructs.collection.join(", ")
    )
}
