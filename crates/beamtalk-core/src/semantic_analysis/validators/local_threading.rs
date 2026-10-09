// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 outer-local threading diagnostics (BT-3745).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A block that writes a local of its enclosing method (an *outer local*) is
//! ordinary Smalltalk. Beamtalk threads such writes back through the
//! construct's `{Value, StateAcc}` tuple (ADR 0041), and today that only works
//! for some constructs in some positions. ADR 0131 makes every
//! local-threading construct a `ThreadedValue` producer (phases 1-4); until it
//! does, this module turns the shapes that crash or silently answer wrong into
//! compile errors, so nothing is silently wrong in between. Two checks share
//! one walk and one fact, [`outer_local_writes`]:
//!
//! - **§6, a Tier 2 block value with no return channel**
//!   ([`DiagnosticCategory::Tier2BlockNoReturnChannel`], permanent). A block
//!   literal that writes an outer local, or a local bound to one and never
//!   reassigned, that flows to a send that cannot return the write: anything
//!   but a §1 construct (a [`crate::state_threading_selectors`] selector, the
//!   conditional family, `on:do:`/`ensure:`, `tryDo:`, a block `value` send on
//!   a literal block), an actor instance self-send (BT-912) in the exact
//!   shape codegen's `detect_tier2_self_send` threads (a bare `self`
//!   receiver, not a cascade message, at the method body's top level, with a
//!   bare block argument whose writes are all captured mutations), or an
//!   Erlang FFI argument (ADR 0041 §Erlang Interop Boundary: lossy by design,
//!   codegen's `generate_erlang_interop_wrapper` warns). In an actor instance
//!   method a stored block may also be sent a `value`-family message, but
//!   only as a bare identifier receiver in a method-body statement, for a
//!   binding codegen's `prescan_tier2_local_vars` promotes; passing a stored
//!   block on (to a self-send or a collection HOM) drops its write.
//!   Everywhere else no callee can hand a `StateAcc` back.
//! - **The Phase 0 allow-set** ([`DiagnosticCategory::UnmigratedLocalThreading`],
//!   temporary). A local-threading construct
//!   ([`local_threading_construct_blocks`]) whose blocks write an outer local
//!   is accepted as a statement unless [`DENY_SET`] lists the statement
//!   shape (BT-3753), and otherwise only in the
//!   `(construct, position, context)` combinations listed in [`ALLOW_SET`]:
//!   the ones that answer right today. Anywhere else it is an error naming the
//!   construct, the position and BT-3743.

use crate::ast::{Block, Expression, ExpressionStatement, MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::{
    OuterLocalWrite, captured_local_mutations, is_safe_value_family_selector,
    local_threading_construct_blocks, outer_local_writes, stored_block_var_uses,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// The epic that removes the Phase 0 allow-set (ADR 0131).
const EPIC: &str = "BT-3743";

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
    /// `whileTrue:`, `whileFalse:`, `repeat` and the unary `whileTrue`/
    /// `whileFalse`.
    Loop,
    /// `to:do:`, `to:by:do:`, `timesRepeat:` and the unary `timesRepeat`.
    CountedLoop,
    /// `do:`.
    Do,
    /// `keysAndValuesDo:`, `doWithKey:`.
    KeyedDo,
    /// The value-returning list ops: `collect:`, `select:`, `reject:`,
    /// `inject:into:`, `detect:`, `count:`, …
    ListOp,
    /// `detect:ifNone:` when only its search block writes an outer local.
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
}

impl ConstructKind {
    /// The family of `selector`. `handler_writes` is whether a
    /// `detect:ifNone:` send's `ifNone:` handler block writes an outer local
    /// (the case that fails today, s2), as opposed to only its search block.
    fn of(selector: &str, handler_writes: bool) -> Self {
        use crate::state_threading_selectors::is_exception_selector;
        match selector {
            "whileTrue:" | "whileFalse:" | "whileTrue" | "whileFalse" | "repeat" => Self::Loop,
            "to:do:" | "to:by:do:" | "timesRepeat:" | "timesRepeat" => Self::CountedLoop,
            "do:" => Self::Do,
            "keysAndValuesDo:" | "doWithKey:" => Self::KeyedDo,
            "anySatisfy:" | "allSatisfy:" => Self::Satisfy,
            "eachWithIndex:" | "do:separatedBy:" => Self::Enumeration,
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" => Self::IfTrue,
            "ifNil:" | "ifNotNil:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => Self::IfNil,
            "and:" | "or:" => Self::AndOr,
            "detect:ifNone:" if handler_writes => Self::DetectIfNone,
            "detect:ifNone:" => Self::DetectIfNoneSearch,
            s if is_exception_selector(s) => Self::Exception,
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
            Self::DetectIfNoneSearch => "`detect:ifNone:` search",
            Self::Enumeration => "`eachWithIndex:`/`do:separatedBy:` iteration",
            Self::IfTrue => "conditional",
            Self::IfNil => "nil test",
            Self::AndOr => "`and:`/`or:`",
            Self::Exception => "exception handler",
            Self::BlockValue => "block evaluation",
        })
    }
}

/// The role of the block literal a statement sits in, read off the
/// local-threading construct that block belongs to
/// ([`local_threading_construct_blocks`]).
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
    fn of(selector: &str, receiver: bool) -> Self {
        match ConstructKind::of(selector, false) {
            IfTrue | IfNil | AndOr => Self::Arm,
            Exception if receiver => Self::ProtectedBody,
            Exception => Self::Handler,
            Loop => Self::LoopBody,
            CountedLoop => Self::CountedLoopBody,
            BlockValue => Self::EvaluatedBlock,
            Do | KeyedDo | Enumeration => Self::DoBody,
            ListOp | Satisfy | DetectIfNone | DetectIfNoneSearch => Self::ListOpBody,
        }
    }
}

impl fmt::Display for Container {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Arm => "a conditional arm",
            Self::ProtectedBody => "a protected body (`on:do:`/`ensure:`)",
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
    /// outermost block of a method sends to `self` or `super` or writes a
    /// field ([`touches_state`]): codegen then threads the block's loops with
    /// the method's state, which threads some outer locals in class and
    /// value-type methods and loses more of them in actor methods (probed,
    /// BT-3753). `direct` is whether an enclosing block
    /// has a statement `x := ...x...` that reads and writes a local bound
    /// outside it ([`rebinds_outer_local`]): codegen then threads that block
    /// as a read+write scope.
    NestedStatement {
        container: Container,
        crosses: bool,
        last: bool,
        deep: bool,
        stateful: bool,
        direct: bool,
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
    /// An element of a literal (array, list, map).
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

use ConstructKind::{
    AndOr, BlockValue, CountedLoop, DetectIfNone, DetectIfNoneSearch, Do, Enumeration, Exception,
    IfNil, IfTrue, KeyedDo, ListOp, Loop, Satisfy,
};
use Container::{
    Arm, CountedLoopBody, DoBody, EvaluatedBlock, Handler, ListOpBody, LoopBody, ProtectedBody,
};
use MethodContext::{Actor, Class, Repl, ValueType};

/// One allow-set row: every listed construct kind, in `position`, in every
/// listed context.
struct Allowed {
    kinds: &'static [ConstructKind],
    position: Position,
    contexts: &'static [MethodContext],
}

/// ADR 0131 Phase 0: the `(construct, position, context)` combinations that
/// thread outer-local writes correctly today, measured with the ADR's probe
/// matrix (`stdlib/test/adr0131local_rebind_test.bt`,
/// `tests/repl-protocol/cases/adr0131_local_rebind.btscript`) and the probes
/// recorded on BT-3745. A statement is accepted unless [`DENY_SET`] lists it.
/// Every other combination not listed here is a compile error
/// ([`DiagnosticCategory::UnmigratedLocalThreading`]).
///
/// This is the one table: phases 2-4 of ADR 0131 (BT-3749, BT-3738, BT-3750)
/// grow it as each producer lands, and phase 5 (BT-3751) deletes the check.
/// Add a row only for a combination that answers right today.
const ALLOW_SET: &[Allowed] = &[
    // `r := <construct>` in the method body (s3, s5, and the per-context
    // passes of the ADR's second probe table).
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            ListOp,
            DetectIfNoneSearch,
            IfTrue,
            Exception,
        ],
        position: Position::AssignValue { nested: false },
        contexts: &[Class, ValueType],
    },
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            Do,
            KeyedDo,
            ListOp,
            DetectIfNoneSearch,
            Satisfy,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
            Exception,
        ],
        position: Position::AssignValue { nested: false },
        contexts: &[Actor],
    },
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            Do,
            KeyedDo,
            ListOp,
            DetectIfNoneSearch,
            Satisfy,
            DetectIfNone,
            Enumeration,
            IfTrue,
            AndOr,
            BlockValue,
        ],
        position: Position::AssignValue { nested: false },
        contexts: &[Repl],
    },
    // `r := <list op>` inside a conditional arm or loop body whose block
    // writes a method-level local (BT-3425, BT-3428, BT-1329).
    Allowed {
        kinds: &[ListOp, DetectIfNoneSearch],
        position: Position::AssignValue { nested: true },
        contexts: &[Actor],
    },
    // `#(a, b) := <list op>` at the REPL (`destructuring.btscript`).
    Allowed {
        kinds: &[ListOp, DetectIfNoneSearch],
        position: Position::DestructureValue,
        contexts: &[Repl],
    },
    // `self.n := <construct>` in the method body (o5, o13).
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            Do,
            KeyedDo,
            ListOp,
            DetectIfNoneSearch,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
            Exception,
        ],
        position: Position::FieldAssignValue { nested: false },
        contexts: &[Actor],
    },
    // `^<construct>` as a method-body statement (the value only: the method
    // ends, so its rebinds are dropped).
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            Do,
            KeyedDo,
            ListOp,
            DetectIfNoneSearch,
            IfTrue,
            Exception,
        ],
        position: Position::ReturnValue { nested: false },
        contexts: &[Class],
    },
    Allowed {
        kinds: &[Do, KeyedDo],
        position: Position::ReturnValue { nested: false },
        contexts: &[ValueType],
    },
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            Do,
            KeyedDo,
            ListOp,
            DetectIfNoneSearch,
            DetectIfNone,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
            Exception,
            BlockValue,
        ],
        position: Position::ReturnValue { nested: false },
        contexts: &[Actor],
    },
    // `^<construct>` inside a block (a non-local return).
    Allowed {
        kinds: &[Do, KeyedDo],
        position: Position::ReturnValue { nested: true },
        contexts: &[Class, ValueType],
    },
    Allowed {
        kinds: &[IfTrue, IfNil, AndOr],
        position: Position::ReturnValue { nested: true },
        contexts: &[Actor],
    },
];

/// One deny-set row: every listed construct kind, as a statement in every
/// listed context, of the method body (`container: None`) or nested in a
/// block whose [`Position::NestedStatement`] fields match (`None` matches
/// either value).
struct Denied {
    kinds: &'static [ConstructKind],
    container: Option<Container>,
    crosses: Option<bool>,
    last: Option<bool>,
    deep: Option<bool>,
    stateful: Option<bool>,
    direct: Option<bool>,
    contexts: &'static [MethodContext],
}

impl Denied {
    fn denies(&self, kind: ConstructKind, position: Position, context: MethodContext) -> bool {
        let at = match position {
            Position::Statement => self.container.is_none(),
            Position::NestedStatement {
                container,
                crosses,
                last,
                deep,
                stateful,
                direct,
            } => {
                self.container == Some(container)
                    && self.crosses.is_none_or(|c| c == crosses)
                    && self.last.is_none_or(|l| l == last)
                    && self.deep.is_none_or(|d| d == deep)
                    && self.stateful.is_none_or(|s| s == stateful)
                    && self.direct.is_none_or(|d| d == direct)
            }
            _ => false,
        };
        at && self.kinds.contains(&kind) && self.contexts.contains(&context)
    }
}

/// Every construct kind.
const ALL: &[ConstructKind] = &[
    Loop,
    CountedLoop,
    Do,
    KeyedDo,
    ListOp,
    DetectIfNoneSearch,
    Satisfy,
    DetectIfNone,
    Enumeration,
    IfTrue,
    IfNil,
    AndOr,
    Exception,
    BlockValue,
];

/// Every construct kind but the conditionals.
const ALL_BUT_CONDITIONALS: &[ConstructKind] = &[
    Loop,
    CountedLoop,
    Do,
    KeyedDo,
    ListOp,
    DetectIfNoneSearch,
    Satisfy,
    DetectIfNone,
    Enumeration,
    Exception,
    BlockValue,
];

/// Every construct kind but the conditionals and `on:do:`/`ensure:`.
const ALL_BUT_CONDITIONALS_AND_HANDLERS: &[ConstructKind] = &[
    Loop,
    CountedLoop,
    Do,
    KeyedDo,
    ListOp,
    DetectIfNoneSearch,
    Satisfy,
    DetectIfNone,
    Enumeration,
    BlockValue,
];

/// A [`DENY_SET`] row.
#[allow(clippy::too_many_arguments)] // one argument per key, so a row fits a line
const fn deny(
    container: Option<Container>,
    crosses: Option<bool>,
    last: Option<bool>,
    deep: Option<bool>,
    stateful: Option<bool>,
    direct: Option<bool>,
    contexts: &'static [MethodContext],
    kinds: &'static [ConstructKind],
) -> Denied {
    Denied {
        kinds,
        container,
        crosses,
        last,
        deep,
        stateful,
        direct,
        contexts,
    }
}

/// ADR 0131 Phase 0 (BT-3753): the statement positions that do *not* thread
/// outer-local writes today. A construct in statement position is accepted
/// unless a row here lists it; every other position is accepted only if
/// [`ALLOW_SET`] lists it.
///
/// Measured on the real build (debug, BT-3753) with the probe matrix in
/// `semantic_analysis/tests/adr0131_statement_probes.tsv`, pinned by
/// `adr0131_statement_probe_pins`: one `t := 0` method per construct kind,
/// per block role, per method context, per write target. A row lists the
/// kinds whose probe answered wrong, raised, panicked codegen or failed erlc
/// for its [`Position::NestedStatement`] facts. Only constructs that write a
/// local bound outside their innermost block (`crosses: true`) are rows; a
/// construct that writes only a local of that block is BT-3776. The same
/// phases that grow [`ALLOW_SET`] shrink this table, and phase 5 (BT-3751)
/// deletes it. Never add a row for a shape an existing test shows answering
/// right.
#[rustfmt::skip] // one row per line: a measured table
const DENY_SET: &[Denied] = &[
    deny(None, None, None, None, None, None, &[Actor], &[DetectIfNone]),
    deny(None, None, None, None, None, None, &[Class], &[KeyedDo, Satisfy, DetectIfNone, Enumeration, IfNil, AndOr, BlockValue]),
    deny(None, None, None, None, None, None, &[Repl], &[IfNil]),
    deny(None, None, None, None, None, None, &[ValueType], &[KeyedDo, Satisfy, DetectIfNone, Enumeration, IfNil, AndOr]),
    deny(Some(Arm), Some(true), None, Some(true), None, None, &[Actor], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), None, Some(false), None, Some(false), &[Actor], &[Loop, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(Arm), Some(true), None, None, None, None, &[Class, ValueType], ALL),
    deny(Some(Arm), Some(true), Some(false), None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), Some(true), Some(true), None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), Some(true), Some(false), None, Some(false), &[Repl], &[CountedLoop, Do, ListOp, Satisfy, Exception]),
    deny(Some(Arm), Some(true), Some(true), Some(false), None, Some(true), &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(true), None, &[Actor], ALL_BUT_CONDITIONALS),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), Some(false), &[Actor], &[Loop, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), Some(true), &[Actor], &[Loop, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), None, &[Class, ValueType], &[Loop, Do, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(DoBody), Some(true), None, None, Some(true), None, &[Actor], &[Loop, KeyedDo, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(false), Some(false), Some(false), &[Actor], ALL_BUT_CONDITIONALS_AND_HANDLERS),
    deny(Some(DoBody), Some(true), None, Some(false), Some(false), Some(true), &[Actor], &[BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(true), Some(false), Some(false), &[Actor], ALL),
    deny(Some(DoBody), Some(true), None, Some(false), None, Some(true), &[Class], &[KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(true), None, Some(true), &[Class], &[Do, KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(false), None, Some(false), &[Class, ValueType], &[Loop, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(true), None, Some(false), &[Class, ValueType], &[Loop, Do, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(DoBody), Some(true), None, None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(DoBody), Some(true), None, Some(false), None, Some(true), &[ValueType], &[KeyedDo, BlockValue]),
    deny(Some(DoBody), Some(true), None, Some(true), None, Some(true), &[ValueType], &[Do, KeyedDo, BlockValue]),
    deny(Some(EvaluatedBlock), Some(true), None, None, None, Some(false), &[Actor], ALL_BUT_CONDITIONALS_AND_HANDLERS),
    deny(Some(EvaluatedBlock), Some(true), None, None, None, None, &[Class, ValueType], ALL),
    deny(Some(EvaluatedBlock), Some(true), None, None, None, None, &[Repl], &[Exception]),
    deny(Some(Handler), Some(true), Some(true), Some(false), None, Some(true), &[Actor], &[BlockValue]),
    deny(Some(Handler), Some(true), Some(false), None, None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(Handler), Some(true), Some(true), Some(true), None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(Handler), Some(true), Some(true), Some(false), None, Some(false), &[Actor, ValueType], ALL_BUT_CONDITIONALS_AND_HANDLERS),
    deny(Some(Handler), Some(true), Some(true), Some(false), None, Some(false), &[Class], &[Loop, Do, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(Handler), Some(true), Some(true), Some(false), None, Some(true), &[Class], &[Do, KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(Handler), Some(true), Some(true), Some(false), None, Some(true), &[ValueType], &[Do, KeyedDo, BlockValue]),
    deny(Some(ListOpBody), Some(true), None, None, None, Some(true), &[Actor], &[BlockValue]),
    deny(Some(ListOpBody), Some(true), None, None, None, Some(false), &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS_AND_HANDLERS),
    deny(Some(ListOpBody), Some(true), None, None, None, Some(true), &[Class], &[KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(ListOpBody), Some(true), None, None, None, Some(true), &[ValueType], &[KeyedDo, BlockValue]),
    deny(Some(LoopBody), Some(true), None, None, Some(true), None, &[Actor], ALL_BUT_CONDITIONALS),
    deny(Some(LoopBody), Some(true), None, Some(false), Some(false), None, &[Actor], &[Loop, Do, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, Some(true), Some(false), Some(false), &[Actor], ALL),
    deny(Some(LoopBody), Some(true), None, Some(true), Some(false), Some(true), &[Actor], &[Loop, Do, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, None, Some(false), None, &[Class, ValueType], ALL),
    deny(Some(LoopBody), Some(true), None, None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Actor], &[Loop, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Actor], &[DetectIfNoneSearch, DetectIfNone, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(false), None, None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(true), None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Class], &[Do, KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Class, ValueType], &[Loop, Do, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(false), None, None, None, &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(true), None, None, &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Repl], &[IfTrue, IfNil, AndOr, Exception]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[ValueType], &[Do, KeyedDo, BlockValue]),
    // A self-send or field write in the outermost block (`stateful`).
    deny(Some(LoopBody), Some(true), None, None, Some(true), None, &[Class, ValueType], &[Loop, CountedLoop, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(true), None, &[Class, ValueType], &[Loop, KeyedDo, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, BlockValue]),
];

/// Whether the Phase 0 check accepts `kind` in `position` in `context`: a
/// statement unless [`DENY_SET`] lists it, anything else only if
/// [`ALLOW_SET`] does.
pub(crate) fn is_allowed(kind: ConstructKind, position: Position, context: MethodContext) -> bool {
    match position {
        Position::Statement | Position::NestedStatement { .. } => !DENY_SET
            .iter()
            .any(|row| row.denies(kind, position, context)),
        _ => ALLOW_SET.iter().any(|row| {
            row.position == position && row.kinds.contains(&kind) && row.contexts.contains(&context)
        }),
    }
}

/// Runs both ADR 0131 checks over every method body and module-level
/// expression list in `module` (see the module docs).
pub(crate) fn check_local_threading(
    module: &Module,
    hierarchy: &ClassHierarchy,
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let instance_context = |class_name: &str| {
        if hierarchy.is_actor_subclass(class_name) {
            Actor
        } else {
            ValueType
        }
    };
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        for method in &class.methods {
            let context = instance_context(class_name);
            check_body(context, param_names(method), &method.body, diagnostics);
        }
        for method in &class.class_methods {
            check_body(Class, param_names(method), &method.body, diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        let context = if standalone.is_class_method {
            Class
        } else {
            instance_context(standalone.class_name.name.as_str())
        };
        let method = &standalone.method;
        check_body(context, param_names(method), &method.body, diagnostics);
    }
    if !module.expressions.is_empty() {
        let known: Vec<EcoString> = known_vars.iter().map(|v| EcoString::from(*v)).collect();
        check_body(Repl, known, &module.expressions, diagnostics);
    }
}

/// Walks one method body (or module-level expression list) whose
/// outermost frame binds `params`.
fn check_body(
    context: MethodContext,
    params: Vec<EcoString>,
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut walker = Walker::new(context, body, diagnostics);
    walker.scope.push(params.into_iter().collect());
    walker.body(body);
}

fn param_names(method: &crate::ast::MethodDefinition) -> Vec<EcoString> {
    method
        .parameters
        .iter()
        .map(|p| p.name.name.clone())
        .collect()
}

/// A local bound (once) to a block literal that writes an outer local.
struct Tier2Local {
    /// The `b := [...]` assignment.
    binding: Span,
    /// The block's first outer-local write.
    write: OuterLocalWrite,
    /// The index in `Walker::scope` of the frame that binds the local.
    frame: usize,
    /// Whether codegen promotes the binding to a Tier 2 local
    /// (`prescan_tier2_local_vars`): see [`Walker::note_tier2_binding`].
    promotable: bool,
}

/// Where a send sits relative to the method body, for the actor exemptions:
/// codegen threads an actor Tier 2 call only at the top level of the method
/// body (probed, BT-3745), never inside a block or an operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TopLevel {
    /// A method-body statement.
    Statement,
    /// The value of a method-body `x := ...` or `^...`.
    Value,
    /// Anywhere else.
    None,
}

/// The walk behind [`check_local_threading`] for one method body: tracks
/// scope (so a write can be told apart from a block-local definition), the
/// block nesting depth (a construct inside a block is in a nested frame) and
/// the Tier 2 block-valued locals bound so far.
struct Walker<'d> {
    context: MethodContext,
    /// Names bound in the method so far, innermost frame last.
    scope: Vec<HashSet<EcoString>>,
    /// Block nesting depth of the expression being walked.
    depth: usize,
    /// For each enclosing block, innermost last: the index in `scope` of
    /// its first frame.
    block_frames: Vec<usize>,
    /// For each enclosing block, innermost last: its role.
    containers: Vec<Container>,
    /// For the method body and each enclosing block, innermost last:
    /// whether the statement being walked in that body is its last.
    last_statements: Vec<bool>,
    /// Whether the outermost enclosing block touches the method's state
    /// (see [`Position::NestedStatement`]).
    outer_stateful: bool,
    /// For each enclosing block, innermost last: whether it rebinds a local
    /// bound outside it (see [`Position::NestedStatement`]).
    direct_blocks: Vec<bool>,
    /// How many times each name is assigned anywhere in the method.
    assign_counts: HashMap<EcoString, usize>,
    tier2_locals: HashMap<EcoString, Tier2Local>,
    /// Tier 2 locals already reported (one diagnostic per binding).
    reported_locals: HashSet<EcoString>,
    /// For each method-body statement `b := [...]`, whether every later use
    /// of `b` in the method body is a safe `value` send
    /// ([`stored_block_var_uses`], shared with codegen's
    /// `prescan_tier2_local_vars`).
    safe_stored_uses: HashMap<EcoString, bool>,
    diagnostics: &'d mut Vec<Diagnostic>,
}

impl<'d> Walker<'d> {
    fn new(
        context: MethodContext,
        body: &[ExpressionStatement],
        diagnostics: &'d mut Vec<Diagnostic>,
    ) -> Self {
        let mut assign_counts = HashMap::new();
        for stmt in body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
                Expression::Assignment { target, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        *assign_counts.entry(id.name.clone()).or_insert(0) += 1;
                    }
                }
                Expression::DestructureAssignment { pattern, .. } => {
                    let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                    for id in ids {
                        *assign_counts.entry(id.name).or_insert(0) += 1;
                    }
                }
                _ => {}
            });
        }
        let mut safe_stored_uses = HashMap::new();
        for (i, stmt) in body.iter().enumerate() {
            let Expression::Assignment { target, value, .. } = &stmt.expression else {
                continue;
            };
            let (Expression::Identifier(id), Expression::Block(_)) =
                (target.as_ref(), value.as_ref())
            else {
                continue;
            };
            let (has_unsafe, has_safe) = body[i + 1..]
                .iter()
                .map(|later| stored_block_var_uses(&later.expression, &id.name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            safe_stored_uses.insert(id.name.clone(), has_safe && !has_unsafe);
        }
        Self {
            context,
            scope: Vec::new(),
            depth: 0,
            block_frames: Vec::new(),
            containers: Vec::new(),
            last_statements: Vec::new(),
            outer_stateful: false,
            direct_blocks: Vec::new(),
            assign_counts,
            tier2_locals: HashMap::new(),
            reported_locals: HashSet::new(),
            safe_stored_uses,
            diagnostics,
        }
    }

    fn is_bound(&self, name: &str) -> bool {
        self.scope.iter().any(|f| f.contains(name))
    }

    /// The index of the innermost scope frame binding `name`.
    fn binding_frame(&self, name: &str) -> Option<usize> {
        self.scope.iter().rposition(|f| f.contains(name))
    }

    /// Whether `name` is bound in the innermost enclosing block (or, at
    /// method level, anywhere in the method).
    fn is_frame_local(&self, name: &str) -> bool {
        let start = self.block_frames.last().copied().unwrap_or(0);
        self.scope[start..].iter().any(|f| f.contains(name))
    }

    fn define(&mut self, name: &EcoString) {
        if !self.is_bound(name) {
            if let Some(frame) = self.scope.last_mut() {
                frame.insert(name.clone());
            }
        }
    }

    fn writes_of(&self, block: &Block) -> Vec<OuterLocalWrite> {
        outer_local_writes(block, &|name| self.is_bound(name))
    }

    /// Where an expression at `position` sits relative to the method body.
    fn top_level(&self, position: Position) -> TopLevel {
        if self.depth > 0 {
            return TopLevel::None;
        }
        match position {
            Position::Statement => TopLevel::Statement,
            Position::AssignValue { nested: false } | Position::ReturnValue { nested: false } => {
                TopLevel::Value
            }
            _ => TopLevel::None,
        }
    }

    fn body(&mut self, body: &[ExpressionStatement]) {
        self.last_statements.push(false);
        for (i, stmt) in body.iter().enumerate() {
            if let Some(last) = self.last_statements.last_mut() {
                *last = i + 1 == body.len();
            }
            self.expr(&stmt.expression, Position::Statement);
        }
        self.last_statements.pop();
    }

    fn block(&mut self, block: &Block, container: Container) {
        if self.depth == 0 {
            self.outer_stateful = self.context != Repl && touches_state(block);
        }
        let direct = rebinds_outer_local(block, &|name| self.is_bound(name));
        self.direct_blocks.push(direct);
        self.block_frames.push(self.scope.len());
        self.containers.push(container);
        self.scope
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        self.depth += 1;
        self.body(&block.body);
        self.depth -= 1;
        self.pop_frame();
        self.containers.pop();
        self.direct_blocks.pop();
        self.block_frames.pop();
    }

    /// Leaves the innermost scope frame, forgetting the Tier 2 block-valued
    /// locals it bound (a later same-named binding in a sibling scope is a
    /// different variable).
    fn pop_frame(&mut self) {
        self.scope.pop();
        let depth = self.scope.len();
        self.tier2_locals.retain(|_, local| local.frame < depth);
    }

    /// Walks `expr` as an operand: a block literal is walked as a block (of
    /// role [`Container::Other`]), any other expression at `position`.
    fn operand(&mut self, expr: &Expression, position: Position) {
        self.operand_in(expr, position, Container::Other);
    }

    /// As [`Walker::operand`], walking a block literal as a block of role
    /// `container`.
    fn operand_in(&mut self, expr: &Expression, position: Position, container: Container) {
        match expr {
            Expression::Block(block) => self.block(block, container),
            other => self.expr(other, position),
        }
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr(&mut self, expr: &Expression, position: Position) {
        let nested = self.depth > 0;
        match expr {
            Expression::Parenthesized { expression, .. } => self.expr(expression, position),
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::Identifier(id) => {
                    self.operand(value, Position::AssignValue { nested });
                    self.define(&id.name);
                    let top_statement = self.top_level(position) == TopLevel::Statement;
                    self.note_tier2_binding(&id.name, value, expr.span(), top_statement);
                }
                Expression::FieldAccess { receiver, .. } => {
                    self.expr(receiver, Position::Receiver);
                    self.operand(value, Position::FieldAssignValue { nested });
                }
                other => {
                    self.expr(other, Position::Operand);
                    self.operand(value, Position::Operand);
                }
            },
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.operand(value, Position::DestructureValue);
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.define(&id.name);
                }
            }
            Expression::Return { value, .. } => {
                self.operand(value, Position::ReturnValue { nested });
            }
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                self.check_construct(expr, position);
                let top = self.top_level(position);
                self.check_send(receiver, selector, arguments, true, top);
                // The role of each block literal the construct inlines (the
                // same recognizer as `check_construct`); any other block is a
                // block value.
                let construct = local_threading_construct_blocks(expr);
                let role = |e: &Expression, is_receiver: bool| match (e, &construct) {
                    (Expression::Block(b), Some((sel, blocks)))
                        if blocks.iter().any(|c| std::ptr::eq(*c, b)) =>
                    {
                        Container::of(sel, is_receiver)
                    }
                    _ => Container::Other,
                };
                let receiver_role = role(receiver, true);
                self.operand_in(receiver, Position::Receiver, receiver_role);
                for arg in arguments {
                    let arg_role = role(arg, false);
                    self.operand_in(arg, Position::Argument, arg_role);
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // The parser folds the first message into `receiver`; the
                // later messages go to that send's own receiver.
                if let Expression::MessageSend {
                    receiver: shared,
                    selector,
                    arguments,
                    ..
                } = receiver.as_ref()
                {
                    // Codegen sends every cascade message through ordinary
                    // dispatch (`generate_cascade`) and inlines none of
                    // their blocks, so a block literal in any of them is a
                    // block value with no return channel (§6), whatever its
                    // selector.
                    // The shared receiver is evaluated once: check it once.
                    if let Expression::Block(block) = shared.unwrap_parens() {
                        self.report_literal(block, &selector.name());
                    }
                    let top = self.top_level(position);
                    self.check_send(shared, selector, arguments, false, top);
                    for msg in messages {
                        self.check_send(shared, &msg.selector, &msg.arguments, false, top);
                    }
                    self.operand(shared, Position::Cascade);
                    for arg in arguments {
                        self.operand(arg, Position::Argument);
                    }
                } else {
                    self.operand(receiver, Position::Cascade);
                }
                for msg in messages {
                    for arg in &msg.arguments {
                        self.operand(arg, Position::Argument);
                    }
                }
            }
            Expression::Block(block) => self.block(block, Container::Other),
            Expression::FieldAccess { receiver, .. } => self.expr(receiver, Position::Receiver),
            Expression::Match { value, arms, .. } => {
                self.operand(value, Position::MatchSubject);
                for arm in arms {
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    self.scope.push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(guard) = &arm.guard {
                        self.operand(guard, Position::MatchArm);
                    }
                    self.operand(&arm.body, Position::MatchArm);
                    self.pop_frame();
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.operand(&pair.key, Position::LiteralElement);
                    self.operand(&pair.value, Position::LiteralElement);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    self.operand(e, Position::LiteralElement);
                }
                if let Some(t) = tail {
                    self.operand(t, Position::LiteralElement);
                }
            }
            Expression::ArrayLiteral { elements, .. } => {
                for e in elements {
                    self.operand(e, Position::LiteralElement);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.operand(e, Position::Interpolation);
                    }
                }
            }
            Expression::Literal(..)
            | Expression::Identifier(_)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }

    /// `name := value`: remembers `name` as a Tier 2 block-valued local when
    /// `value` is a block literal that writes an outer local and `name` is
    /// assigned nowhere else in the method.
    ///
    /// The binding is *promotable* (an actor may then send it `value`) only
    /// in the shape codegen's `prescan_tier2_local_vars` promotes: a
    /// method-body statement, whose block threads every outer local it writes
    /// ([`captured_local_mutations`]), with every later use a safe `value`
    /// send ([`stored_block_var_uses`]).
    fn note_tier2_binding(
        &mut self,
        name: &EcoString,
        value: &Expression,
        binding: Span,
        top_statement: bool,
    ) {
        let Expression::Block(block) = value else {
            return;
        };
        if self.assign_counts.get(name).copied() != Some(1) {
            return;
        }
        let Some(frame) = self.binding_frame(name) else {
            return;
        };
        let writes = self.writes_of(block);
        let Some(write) = writes.first().cloned() else {
            return;
        };
        let promotable = top_statement
            && self.safe_stored_uses.get(name).copied() == Some(true)
            && threads_all(block, &writes);
        self.tier2_locals.insert(
            name.clone(),
            Tier2Local {
                binding,
                write,
                frame,
                promotable,
            },
        );
    }

    /// The Phase 0 allow-set check for a construct at `position`.
    fn check_construct(&mut self, expr: &Expression, position: Position) {
        let Some((selector, blocks)) = local_threading_construct_blocks(expr) else {
            return;
        };
        let writes: Vec<OuterLocalWrite> = blocks.iter().flat_map(|b| self.writes_of(b)).collect();
        let Some(first) = writes.first().cloned() else {
            return;
        };
        let handler_writes = selector == "detect:ifNone:"
            && matches!(
                expr.unwrap_parens(),
                Expression::MessageSend { arguments, .. }
                    if matches!(arguments.get(1), Some(Expression::Block(h)) if !self.writes_of(h).is_empty())
            );
        let kind = ConstructKind::of(&selector, handler_writes);
        // An assignment inside a block whose construct only writes locals of
        // that same block is threaded in that block's own frame, exactly as
        // in a method body; only a write that crosses the block boundary
        // needs the enclosing construct to thread it on (ADR 0131 §1's
        // transitive closure, which is not there yet).
        let crosses_block = writes.iter().any(|w| !self.is_frame_local(&w.name));
        let position = match position {
            // Codegen lowers a nested statement with the construct that owns
            // the *outermost* enclosing block (the one directly under the
            // method body): an arm inside a `do:` body threads as the `do:`
            // body does, a `do:` inside an arm as the arm does (probed,
            // BT-3753).
            Position::Statement if self.depth > 0 => Position::NestedStatement {
                container: self.containers.first().copied().unwrap_or(Container::Other),
                crosses: crosses_block,
                last: self.last_statements.get(1).copied().unwrap_or(false),
                deep: self.depth > 1,
                stateful: self.outer_stateful,
                direct: self.direct_blocks.iter().any(|d| *d),
            },
            Position::AssignValue { nested: true } if !crosses_block => {
                Position::AssignValue { nested: false }
            }
            Position::FieldAssignValue { nested: true } if !crosses_block => {
                Position::FieldAssignValue { nested: false }
            }
            other => other,
        };
        if is_allowed(kind, position, self.context) {
            return;
        }
        let hint = self.construct_hint(&selector, kind, position, &first.name);
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "`{selector}` writes outer local `{name}`, but as {position} in {context} \
                     the write is not threaded back yet ({EPIC})",
                    name = first.name,
                    context = self.context,
                ),
                expr.span(),
            )
            .with_hint(hint)
            .with_note(
                format!("`{}` is written here", first.name),
                Some(first.span),
            )
            .with_note(
                format!(
                    "ADR 0131 makes every {kind} thread outer locals in every position; until \
                     then only the positions that work today are accepted"
                ),
                None,
            )
            .with_category(DiagnosticCategory::UnmigratedLocalThreading),
        );
    }

    /// The help text for a construct the Phase 0 check rejects at
    /// `position`: the nearest position that does thread `name` today.
    fn construct_hint(
        &self,
        selector: &str,
        kind: ConstructKind,
        position: Position,
        name: &str,
    ) -> String {
        let top_ok = is_allowed(kind, Position::Statement, self.context);
        let rhs_ok = is_allowed(kind, Position::AssignValue { nested: false }, self.context);
        match position {
            Position::NestedStatement { container, .. } if top_ok => format!(
                "evaluate the `{selector}` as a statement of the method body, not inside \
                 {container}, and read the locals it writes afterwards"
            ),
            Position::Statement | Position::NestedStatement { .. } => format!(
                "compute the new value of `{name}` without writing it from the block, and \
                 assign it in a statement of the method body (`{name} := coll inject: {name} \
                 into: [:acc :x | ...]`)"
            ),
            Position::AssignValue { .. } => format!(
                "evaluate the `{selector}` as a statement of its own, at the level of the \
                 method body, and read the locals it writes afterwards"
            ),
            _ if rhs_ok => format!(
                "assign the `{selector}` result to a local in a statement of its own \
                 (`v := ...`) and use `v` here"
            ),
            _ => format!(
                "evaluate the `{selector}` as a statement of its own, at the level of the \
                 method body, and read the locals it writes afterwards"
            ),
        }
    }

    /// The §6 check for one send: its literal block arguments and receiver,
    /// and any Tier 2 block-valued local it is sent to or passed. `inlined`
    /// is false for a cascade message, which codegen never inlines.
    fn check_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        inlined: bool,
        top: TopLevel,
    ) {
        let sel = selector.name().to_string();
        if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_some() {
            // ADR 0041 §Erlang Interop Boundary: lossy by design (warned by codegen).
            return;
        }
        // EXEMPTION (actor self-send), the shape codegen's
        // `detect_tier2_self_send` (dispatch_codegen.rs) promotes: receiver a
        // bare `self` (not `super`, not parenthesized), an ordinary send (not
        // a cascade message), at the top level of the method body, with a
        // bare `[...]` argument whose block threads every outer local it
        // writes (`captured_mutations_for_block`). Anything else drops the
        // write or raises (probed, BT-3745). Pinned by
        // `section6_exemptions_match_codegen_shapes` and the compiled
        // fixture `adr0131section6exemptions_actor.bt`.
        let is_self_send = matches!(receiver, Expression::Identifier(id) if id.name == "self");
        let actor_self_send =
            inlined && self.context == Actor && is_self_send && top != TopLevel::None;
        // EXEMPTION (§1 construct): a bare `[...]` argument of an inlined
        // send of a §1 selector (`local_threading_construct_blocks`).
        let construct_send = inlined && is_section1_selector(&sel);

        // A block literal receiver. EXEMPTION: a bare `[...]` receiver of an
        // inlined `value`/loop/protected-body send (codegen's
        // `inline_block_captured_mutations` and the loop and exception
        // generators match only a bare block). A cascade checks its shared
        // receiver once itself, so `inlined: false` skips it here.
        if inlined {
            match receiver {
                Expression::Block(block) if !receiver_has_channel(&sel) => {
                    self.report_literal(block, &sel);
                }
                Expression::Parenthesized { .. } => {
                    if let Expression::Block(block) = receiver.unwrap_parens() {
                        self.report_literal(block, &sel);
                    }
                }
                _ => {}
            }
        }
        // Block literal arguments. A parenthesized block argument is a block
        // value in every send.
        for arg in arguments {
            match arg {
                Expression::Block(block) if construct_send => {}
                Expression::Block(block) if actor_self_send => {
                    let writes = self.writes_of(block);
                    if !threads_all(block, &writes) {
                        self.report_literal(block, &sel);
                    }
                }
                Expression::Block(block) => self.report_literal(block, &sel),
                Expression::Parenthesized { .. } => {
                    if let Expression::Block(block) = arg.unwrap_parens() {
                        self.report_literal(block, &sel);
                    }
                }
                _ => {}
            }
        }

        // Tier 2 block-valued locals. EXEMPTION (actor stored block): a bare
        // identifier receiver of a `value`-family send, as a method-body
        // statement (cascade or not), for a binding codegen's
        // `prescan_tier2_local_vars` promotes (see `note_tier2_binding`);
        // `is_tier2_value_call` and the cascade path match only a bare
        // identifier. Every other use, including any argument use (a
        // self-send or a collection fold of a stored block drops the write,
        // probed), has no return channel.
        if let Expression::Identifier(id) = receiver.unwrap_parens() {
            let bare = matches!(receiver, Expression::Identifier(_));
            let promotable = self
                .tier2_locals
                .get(&id.name)
                .is_some_and(|l| l.promotable);
            let ok = bare
                && self.context == Actor
                && promotable
                && top == TopLevel::Statement
                && is_safe_value_family_selector(selector);
            if !ok {
                self.report_local(&id.name, &sel, receiver.span());
            }
        }
        for arg in arguments {
            if let Expression::Identifier(id) = arg.unwrap_parens() {
                self.report_local(&id.name, &sel, arg.span());
            }
        }
    }

    fn report_literal(&mut self, block: &Block, selector: &str) {
        let Some(write) = self.writes_of(block).into_iter().next() else {
            return;
        };
        let name = &write.name;
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "block writes outer local `{name}`, but `{selector}` cannot return the write"
                ),
                block.span,
            )
            .with_hint(section6_help(name, selector))
            .with_note(format!("`{name}` is written here"), Some(write.span))
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    fn report_local(&mut self, local: &EcoString, selector: &str, use_span: Span) {
        let Some(info) = self.tier2_locals.get(local) else {
            return;
        };
        // A block parameter or match binding of the same name shadows it.
        if self.binding_frame(local) != Some(info.frame) {
            return;
        }
        if !self.reported_locals.insert(local.clone()) {
            return;
        }
        let name = &info.write.name;
        let diagnostic = Diagnostic::error(
            format!(
                "block stored in `{local}` writes outer local `{name}`, but `{selector}` cannot \
                 return the write"
            ),
            info.binding,
        )
        .with_hint(section6_help(name, selector))
        .with_note(format!("`{name}` is written here"), Some(info.write.span))
        .with_note(
            format!("`{local}` flows to `{selector}` here"),
            Some(use_span),
        )
        .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel);
        self.diagnostics.push(diagnostic);
    }
}

/// Whether a statement of `block` (not of a nested block) is an assignment
/// `x := ...` to a local bound outside `block` (`is_outer`) whose value reads
/// `x`.
fn rebinds_outer_local(block: &Block, is_outer: &dyn Fn(&str) -> bool) -> bool {
    let params: HashSet<&str> = block.parameters.iter().map(|p| p.name.as_str()).collect();
    block.body.iter().any(|stmt| {
        let Expression::Assignment { target, value, .. } = &stmt.expression else {
            return false;
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return false;
        };
        if params.contains(id.name.as_str()) || !is_outer(&id.name) {
            return false;
        }
        let mut reads = false;
        crate::ast_walker::walk_expression(value, &mut |e| {
            if matches!(e, Expression::Identifier(r) if r.name == id.name) {
                reads = true;
            }
        });
        reads
    })
}

/// Whether `block` (at any depth) sends to `self` or `super`, or assigns a
/// field (`self.x := ...`).
fn touches_state(block: &Block) -> bool {
    let mut found = false;
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::MessageSend { receiver, .. } | Expression::Cascade { receiver, .. } => {
                if matches!(receiver.unwrap_parens(), Expression::Super(_))
                    || matches!(receiver.unwrap_parens(), Expression::Identifier(id) if id.name == "self")
                {
                    found = true;
                }
            }
            Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::FieldAccess { .. }) =>
            {
                found = true;
            }
            _ => {}
        });
    }
    found
}

/// Whether codegen's Tier 2 block protocol threads back every outer local in
/// `writes` (each is in [`captured_local_mutations`]).
fn threads_all(block: &Block, writes: &[OuterLocalWrite]) -> bool {
    let captured = captured_local_mutations(block);
    writes
        .iter()
        .all(|w| captured.iter().any(|c| c == w.name.as_str()))
}

/// The ADR 0131 §6 help text.
fn section6_help(name: &str, selector: &str) -> String {
    format!(
        "return the new value from the block and assign it: `{name} := ... {selector} [... {name} + 1]`, \
         or use a control-flow message (`do:`, `inject:into:`, `on:do:`) that threads locals"
    )
}

/// ADR 0131 §6: a §1 construct selector, whose literal block arguments are
/// inlined (so their outer-local writes have a way back): a state-threading
/// selector, the conditional family, `on:do:`/`ensure:`, or `tryDo:` (keyed
/// on the selector, not on the receiver being spelled `Result`, ADR 0131 §5).
fn is_section1_selector(selector: &str) -> bool {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threading_keyword_selector,
    };
    is_state_threading_keyword_selector(selector)
        || is_conditional_selector(selector)
        || is_exception_selector(selector)
        || selector == "tryDo:"
}

/// Whether a block literal *receiver* of `selector` is inlined: a block
/// `value` send, a loop's condition, or a protected body.
fn receiver_has_channel(selector: &str) -> bool {
    crate::state_threading_selectors::is_state_threaded_block_receiver(selector)
        || crate::state_threading_selectors::is_state_threading_unary_selector(selector)
        || matches!(selector, "whileTrue:" | "whileFalse:" | "repeat")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allow_set_rows_are_non_empty_and_never_list_a_statement() {
        for row in ALLOW_SET {
            assert!(!row.kinds.is_empty() && !row.contexts.is_empty());
            assert!(
                !matches!(
                    row.position,
                    Position::Statement | Position::NestedStatement { .. }
                ),
                "statements are accepted unless DENY_SET lists them"
            );
        }
    }

    #[test]
    fn deny_set_rows_are_non_empty() {
        for row in DENY_SET {
            assert!(!row.kinds.is_empty() && !row.contexts.is_empty());
        }
    }

    #[test]
    fn statement_is_allowed_unless_denied() {
        // `do:` as a method-body statement answers right everywhere (s8).
        for context in [Class, ValueType, Actor, Repl] {
            assert!(is_allowed(Do, Position::Statement, context));
        }
        // `detect:ifNone:` with a writing handler loses the write as a
        // statement in an actor method (BT-3753).
        assert!(!is_allowed(DetectIfNone, Position::Statement, Actor));
    }
}
