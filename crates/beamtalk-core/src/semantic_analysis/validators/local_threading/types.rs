// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared vocabulary of the ADR 0131 local-threading checks: where a
//! construct sits ([`MethodContext`], [`Container`], [`Position`]) and which
//! construct it is ([`ConstructKind`]).
//!
//! **DDD Context:** Semantic Analysis

use crate::semantic_analysis::block_facts::RootMethodKind;
use std::fmt;

use self::ConstructKind::{
    AndOr, BlockValue, CountedLoop, DetectIfNone, DetectIfNoneSearch, Do, Enumeration, Exception,
    IfNil, IfTrue, KeyedDo, ListOp, Lookup, Loop, Satisfy, TryDo,
};

/// Which kind of method body a construct sits in. Codegen threads outer
/// locals differently in each (ADR 0131 §2's frame modes), so each passes a
/// different set of shapes today.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MethodContext {
    /// A class-side method (of any class kind).
    Class,
    /// An instance method of a non-actor class (`Object`/`Value` subclass).
    ValueType,
    /// An instance method of an `Actor` subclass.
    Actor,
    /// A module-level expression (a REPL input or a script).
    Repl,
}

impl MethodContext {
    /// The [`RootMethodKind`] of a method body of this context; `None` for a
    /// module-level expression list.
    pub(super) fn root_method_kind(self) -> Option<RootMethodKind> {
        match self {
            Self::Class => Some(RootMethodKind::Class),
            Self::ValueType => Some(RootMethodKind::ValueType),
            Self::Actor => Some(RootMethodKind::Actor),
            Self::Repl => None,
        }
    }
}

impl fmt::Display for MethodContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Class => "a class method",
            Self::ValueType => "a value-type method",
            Self::Actor => "an actor method",
            Self::Repl => "a top-level expression",
        })
    }
}

/// The local-threading construct families. Members of one family thread
/// their writes through the same codegen path, so they pass in the same
/// places.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ConstructKind {
    /// `whileTrue:`, `whileFalse:` and the unary `whileTrue`/`whileFalse`
    /// (a literal `repeat` is canonicalised to `whileTrue:`, BT-4589).
    Loop,
    /// `to:do:`, `to:by:do:`, `timesRepeat:` and the unary `timesRepeat`.
    CountedLoop,
    /// `do:`.
    Do,
    /// `keysAndValuesDo:`, `doWithKey:`.
    KeyedDo,
    /// The value-returning list ops: `collect:`, `select:`, `reject:`,
    /// `inject:into:`, `count:`, … (a plain `detect:` too in the REPL; elsewhere
    /// it is [`Self::DetectIfNoneSearch`])
    ListOp,
    /// `detect:ifNone:` when only its search block writes an outer local, and a
    /// plain `detect:`.
    DetectIfNoneSearch,
    /// `anySatisfy:`, `allSatisfy:`.
    Satisfy,
    /// `detect:ifNone:` whose `ifNone:` handler writes an outer local.
    DetectIfNone,
    /// `eachWithIndex:`, `do:separatedBy:`.
    Enumeration,
    /// `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`.
    IfTrue,
    /// `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`.
    IfNil,
    /// `and:`, `or:`.
    AndOr,
    /// `on:do:`, `ensure:`.
    Exception,
    /// `value`/`value:`… sent to a block literal.
    BlockValue,
    /// `Result tryDo:` (ADR 0131 §5, Phase 4): a producer wherever
    /// [`exception_producer_writes`] says so, and in no other position.
    TryDo,
    /// A lookup selector whose handler is lowered inline for a `Dictionary`
    /// receiver (`at:ifAbsent:`, ADR 0131 Phase 2d, BT-3798). Never judged by
    /// the allow-set: it is accepted exactly where it is a producer
    /// (`conditional_arm_blocks`), and otherwise the §6 check reports it.
    Lookup,
}

impl ConstructKind {
    /// The family of `selector`. `handler_writes` is whether a
    /// `detect:ifNone:` send's `ifNone:` handler block writes an outer local
    /// (the case that fails today, s2), as opposed to only its search block.
    pub(super) fn of(selector: &str, handler_writes: bool) -> Self {
        use crate::state_threading_selectors::{is_exception_selector, is_try_do_selector};
        match selector {
            // The unary loops reach here as the keyword loops they are
            // canonicalised to (`ast::normalize`, BT-4313, BT-4603).
            "whileTrue:" | "whileFalse:" => Self::Loop,
            "to:do:" | "to:by:do:" | "timesRepeat:" => Self::CountedLoop,
            "do:" => Self::Do,
            "keysAndValuesDo:" | "doWithKey:" => Self::KeyedDo,
            "anySatisfy:" | "allSatisfy:" => Self::Satisfy,
            "eachWithIndex:" | "do:separatedBy:" => Self::Enumeration,
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" => Self::IfTrue,
            "ifNil:" | "ifNotNil:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => Self::IfNil,
            "and:" | "or:" => Self::AndOr,
            "detect:ifNone:" if handler_writes => Self::DetectIfNone,
            // A plain `detect:` is the same fold with no handler (BT-3841).
            "detect:ifNone:" | "detect:" => Self::DetectIfNoneSearch,
            s if is_exception_selector(s) => Self::Exception,
            s if is_try_do_selector(s) => Self::TryDo,
            s if crate::state_threading_selectors::lookup_shape(s).is_some() => Self::Lookup,
            s if s.starts_with("value") => Self::BlockValue,
            _ => Self::ListOp,
        }
    }
}

impl fmt::Display for ConstructKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Loop => "loop",
            Self::CountedLoop => "counted loop",
            Self::Do => "`do:` iteration",
            Self::KeyedDo => "`keysAndValuesDo:` iteration",
            Self::ListOp => "collection operation",
            Self::Satisfy => "`anySatisfy:`/`allSatisfy:` test",
            Self::DetectIfNone => "`detect:ifNone:` handler",
            Self::DetectIfNoneSearch => "`detect:`/`detect:ifNone:` search",
            Self::Enumeration => "`eachWithIndex:`/`do:separatedBy:` iteration",
            Self::IfTrue => "conditional",
            Self::IfNil => "nil test",
            Self::AndOr => "`and:`/`or:`",
            Self::Exception => "exception handler",
            Self::BlockValue => "block evaluation",
            Self::TryDo => "`tryDo:` block",
            Self::Lookup => "lookup",
        })
    }
}

/// The role of the block literal a statement sits in, read off the
/// local-threading construct that block belongs to
/// ([`local_threading_construct`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Container {
    /// An arm of a conditional: the `ifTrue:`, `ifNil:` and `and:`/`or:`
    /// families.
    Arm,
    /// The protected body of `on:do:`/`ensure:` (the receiver block).
    ProtectedBody,
    /// An `on:do:` handler or an `ensure:` cleanup block.
    Handler,
    /// The condition or body of a [`ConstructKind::Loop`].
    LoopBody,
    /// The body of a [`ConstructKind::CountedLoop`].
    CountedLoopBody,
    /// A block of a `do:`-style iteration, whose value is discarded
    /// ([`ConstructKind::Do`], [`ConstructKind::KeyedDo`],
    /// [`ConstructKind::Enumeration`]).
    DoBody,
    /// A block of a value-returning collection operation
    /// ([`ConstructKind::ListOp`], [`ConstructKind::Satisfy`] and the
    /// `detect:ifNone:` kinds).
    ListOpBody,
    /// A block literal sent `value`.
    EvaluatedBlock,
    /// Any other block: a block value, whose statements run in a frame of
    /// their own.
    Other,
}

impl Container {
    /// The role of a block that is the receiver (`receiver: true`) or an
    /// argument of a construct send of `selector`.
    pub(super) fn of(selector: &str, receiver: bool) -> Self {
        match ConstructKind::of(selector, false) {
            IfTrue | IfNil | AndOr | Lookup => Self::Arm,
            Exception if receiver => Self::ProtectedBody,
            Exception => Self::Handler,
            Loop => Self::LoopBody,
            CountedLoop => Self::CountedLoopBody,
            BlockValue => Self::EvaluatedBlock,
            // A `tryDo:` block is a protected body: the block of a catch
            // boundary, lowered like an `on:do:`'s (ADR 0131 Phase 4).
            TryDo => Self::ProtectedBody,
            Do | KeyedDo | Enumeration => Self::DoBody,
            ListOp | Satisfy | DetectIfNone | DetectIfNoneSearch => Self::ListOpBody,
        }
    }
}

impl fmt::Display for Container {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Arm => "a conditional arm",
            Self::ProtectedBody => "a protected body (`on:do:`/`ensure:`/`Result tryDo:`)",
            Self::Handler => "an exception handler or `ensure:` block",
            Self::LoopBody => "a loop",
            Self::CountedLoopBody => "a counted loop",
            Self::DoBody => "a `do:` iteration",
            Self::ListOpBody => "a collection operation",
            Self::EvaluatedBlock => "an evaluated block",
            Self::Other => "a block",
        })
    }
}

/// Where a construct's value goes. `nested` is true inside any block body
/// (the construct's frame is then a block, not the method).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Position {
    /// An expression statement of a method body (including the last).
    Statement,
    /// An expression statement of a block body (including the last).
    /// `container` is the role of the outermost enclosing block (the one
    /// directly under the method body), and `last` is whether the statement
    /// of that block containing this one is its last (its value). `crosses`
    /// is whether the construct writes a local bound outside its innermost
    /// enclosing block (as opposed to only locals of that block). `deep` is
    /// whether it is nested in two blocks or more. `stateful` is whether the
    /// outermost block of a method sends to `self` or writes a field
    /// ([`touches_state`]): codegen then threads the block's loops with
    /// the method's state, which threads some outer locals in class and
    /// value-type methods and loses more of them in actor methods (probed,
    /// BT-3753). `direct` is whether an enclosing block
    /// has a statement `x := ...x...` that reads and writes a local bound
    /// outside it ([`rebinds_outer_local`]): codegen then threads that block
    /// as a read+write scope. `inner` is the role of the innermost enclosing
    /// block (`container` is the outermost): codegen lowers an arm inside a
    /// `do:` body differently from a `do:` inside an arm (BT-3841). `depth` is
    /// the number of enclosing blocks; with `container` and `inner` it names the
    /// whole chain only up to depth 2.
    NestedStatement {
        container: Container,
        crosses: bool,
        last: bool,
        deep: bool,
        stateful: bool,
        direct: bool,
        inner: Container,
        depth: usize,
    },
    /// `x := <here>`.
    AssignValue { nested: bool },
    /// `self.f := <here>`.
    FieldAssignValue { nested: bool },
    /// `^<here>`.
    ReturnValue { nested: bool },
    /// The receiver of a message (including a binary operator's left operand).
    Receiver,
    /// An argument of a message (including a binary operator's right operand).
    Argument,
    /// Part of a cascade.
    Cascade,
    /// An element of a literal (array, list, map, tuple), or a segment value
    /// or size of a binary construction (ADR 0143).
    LiteralElement,
    /// The subject of `match:`.
    MatchSubject,
    /// A `match:` arm's body or guard.
    MatchArm,
    /// A string interpolation segment.
    Interpolation,
    /// The value of a destructuring assignment.
    DestructureValue,
    /// Any other operand.
    Operand,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nested_suffix = |nested: bool| if nested { " inside a block" } else { "" };
        match self {
            Self::Statement => f.write_str("a statement"),
            Self::NestedStatement { container, .. } => {
                write!(f, "a statement inside {container}")
            }
            Self::AssignValue { nested } => {
                write!(f, "the value of an assignment{}", nested_suffix(*nested))
            }
            Self::FieldAssignValue { nested } => {
                write!(
                    f,
                    "the value of a field assignment{}",
                    nested_suffix(*nested)
                )
            }
            Self::ReturnValue { nested } => {
                write!(f, "a `^` value{}", nested_suffix(*nested))
            }
            Self::Receiver => f.write_str("the receiver of a message"),
            Self::Argument => f.write_str("an argument of a message"),
            Self::Cascade => f.write_str("part of a cascade"),
            Self::LiteralElement => f.write_str("an element of a literal"),
            Self::MatchSubject => f.write_str("the subject of `match:`"),
            Self::MatchArm => f.write_str("a `match:` arm"),
            Self::Interpolation => f.write_str("a string interpolation"),
            Self::DestructureValue => f.write_str("the value of a destructuring assignment"),
            Self::Operand => f.write_str("an operand"),
        }
    }
}
