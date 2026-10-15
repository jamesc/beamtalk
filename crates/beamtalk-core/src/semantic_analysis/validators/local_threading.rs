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
//! local-threading construct a `ThreadedValue` producer (phases 1-4); for the
//! shapes no producer lowers yet, this module turns the ones that crash or
//! silently answer wrong into compile errors, so nothing is silently wrong.
//! Two checks share
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
//!   bare block argument whose writes are all captured mutations) to a callee
//!   that threads the block (BT-3755, [`callee_threads_block_param`]), or an
//!   Erlang FFI argument (ADR 0041 §Erlang Interop Boundary: lossy by design,
//!   codegen's `generate_erlang_interop_wrapper` warns). In an actor instance
//!   method a stored block may also be sent a `value`-family message, but
//!   only as a bare identifier receiver in a method-body statement, for a
//!   binding codegen's `prescan_tier2_local_vars` promotes; passing a stored
//!   block on (to a self-send or a collection HOM) drops its write.
//!   Everywhere else no callee can hand a `StateAcc` back.
//! - **The Phase 0 allow-set** ([`DiagnosticCategory::UnmigratedLocalThreading`],
//!   still needed: BT-3751 measured 552 of 733 rejected statement shapes still
//!   dropping their writes silently and 43 failing erlc with it bypassed, so
//!   BT-3841 retires it as producers land). A local-threading construct
//!   ([`local_threading_construct`], of a family threaded today) whose
//!   threaded set ([`construct_outer_local_writes`], the same recognizer and
//!   set codegen's `threaded_locals_of` reads) is not empty
//!   is accepted as a statement unless [`DENY_SET`] lists the statement
//!   shape (BT-3753), and otherwise only in the
//!   `(construct, position, context)` combinations listed in [`ALLOW_SET`]:
//!   the ones that answer right today. Anywhere else it is an error naming the
//!   construct, the position and BT-3743.

use crate::ast::{Block, Expression, ExpressionStatement, MatchArm, MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::{
    EscapeShape, LocalThreadingConstruct, LocalThreadingFamily, LoopBodyContext, OuterLocalWrite,
    RootMethodKind, captured_local_mutations, conditional_arm_blocks, conditional_producer_writes,
    construct_outer_local_writes, escape_sites, exception_arm_blocks, exception_producer_writes,
    is_actor_fold_value_site, is_fold_frame_body, is_loop_body_producer_site,
    is_safe_value_family_selector, is_threaded_frame_body, local_threading_construct,
    loop_body_producer_sites, operand_producer_writes, outer_local_writes, producer_body_block,
    receiver_statically_not_dictionary, repl_operand_sites, root_producer_writes,
    sequenced_operand_sites, stored_block_var_uses,
};
use crate::semantic_analysis::tier2_callee::callee_threads_block_param;
use crate::semantic_analysis::type_checker::TypeMap;
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
    fn of(selector: &str, handler_writes: bool) -> Self {
        use crate::state_threading_selectors::{is_exception_selector, is_try_do_selector};
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
            Self::DetectIfNoneSearch => "`detect:ifNone:` search",
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
    fn of(selector: &str, receiver: bool) -> Self {
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
    /// whole chain only up to depth 2, which is why [`RIGHT_STATEMENTS`] rows
    /// match the depth exactly.
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
    IfNil, IfTrue, KeyedDo, ListOp, Lookup, Loop, Satisfy, TryDo,
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
/// grew it as each producer landed. Phase 5a (BT-3751) could not delete the
/// check (see [`DENY_SET`]); BT-3841 does, once the shapes it rejects are
/// producers. Add a row only for a combination that answers right today.
const ALLOW_SET: &[Allowed] = &[
    // `r := <construct>` in the method body (s3, and the per-context passes of
    // the ADR's second probe table). A conditional is not here since ADR 0131
    // Phase 3 (BT-3738): in a class or value-type method it is accepted only as
    // a local-threading producer (`root_producer_writes`), the one lowering.
    Allowed {
        kinds: &[Loop, CountedLoop, ListOp, DetectIfNoneSearch],
        position: Position::AssignValue { nested: false },
        contexts: &[Class, ValueType],
    },
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            ListOp,
            DetectIfNoneSearch,
            Satisfy,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
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
            IfNil,
            AndOr,
            Exception,
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
            ListOp,
            DetectIfNoneSearch,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
        ],
        position: Position::FieldAssignValue { nested: false },
        contexts: &[Actor],
    },
    // `^<construct>` as a method-body statement (the value only: the method
    // ends, so its rebinds are dropped).
    Allowed {
        kinds: &[Loop, CountedLoop, ListOp, DetectIfNoneSearch, IfTrue],
        position: Position::ReturnValue { nested: false },
        contexts: &[Class],
    },
    Allowed {
        kinds: &[
            Loop,
            CountedLoop,
            ListOp,
            DetectIfNoneSearch,
            DetectIfNone,
            Enumeration,
            IfTrue,
            IfNil,
            AndOr,
            BlockValue,
        ],
        position: Position::ReturnValue { nested: false },
        contexts: &[Actor],
    },
    // `^<construct>` inside a block (a non-local return). `Do` is not here
    // (BT-3796): it is a producer now and its closed form answers the
    // `{nil, StateAcc}` carrier, which only a producer site unpacks. A `^` in
    // an arm of a conditional or the protected body, handler or cleanup of an
    // `on:do:`/`ensure:` producer is one (a `BranchArm` frame's statement,
    // ADR 0131 Phase 3 / 3b, BT-3813's arm and handler cases), so it never
    // reaches this row; a `^` inside any other closure is not, and its
    // carrier would be returned raw (BT-3813's remaining closure case). The
    // keyed `do:` forms are producers too (BT-3841).
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
                inner: _,
                depth: _,
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
/// phases that grow [`ALLOW_SET`] shrink this table. Phase 5a (BT-3751)
/// re-measured every `bad`/`over` line with the check bypassed (class, value
/// and actor methods, run on a BEAM): 552 of the 733 answered wrong, 43 failed
/// to compile and 138 answered right. BT-3841's first step carved the 138 out
/// ([`RIGHT_STATEMENTS`]: 134 of them, as 95 position rows; twenty stay rejected, see the pin test's
/// `KNOWN_OVER_REJECTED`); the 595 that answer wrong or fail stay rejected until
/// they become producers. The measurement is a checked-in harness
/// (`just measure-adr0131-statements`, `adr0131_statement_measured.tsv`) and
/// `adr0131_phase0_agrees_with_measured_answers` asserts, in both directions,
/// that this table rejects exactly the cells that do not answer right.
/// Never add a row for a shape an existing test shows answering right.
#[rustfmt::skip] // one row per line: a measured table
const DENY_SET: &[Denied] = &[
    deny(None, None, None, None, None, None, &[Class], &[Satisfy, Enumeration, IfNil, AndOr, BlockValue]),
    deny(None, None, None, None, None, None, &[ValueType], &[Satisfy, Enumeration, IfNil, AndOr]),
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
    deny(Some(LoopBody), Some(true), None, Some(true), Some(false), Some(true), &[Actor], &[KeyedDo, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, None, Some(false), None, &[Class, ValueType], &[Do, KeyedDo, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
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

/// A statement shape (a construct kind at a [`Position::Statement`] or
/// [`Position::NestedStatement`] position in a method context) that the
/// BT-3841 measurement found answering right on a BEAM wherever the probe
/// matrix draws it, though a coarser [`DENY_SET`] row covers it. A row names
/// every fact of the position, the exact block depth and the outermost and
/// innermost block roles included (the matrix nests at most two blocks, so
/// those name the whole chain); a statement whose facts match no row, a
/// deeper nesting among them, keeps the coarse rows' verdict, so this table
/// narrows [`DENY_SET`] without widening it past a measurement. Generated from `adr0131_statement_measured.tsv`;
/// `adr0131_phase0_agrees_with_measured_answers` keeps it honest in both
/// directions: no cell that answers wrong or fails to compile is accepted,
/// and every cell that answers right is accepted except the
/// [`KNOWN_OVER_REJECTED`] ones.
struct RightStatement {
    kind: ConstructKind,
    position: Position,
    context: MethodContext,
}

/// A [`RIGHT_STATEMENTS`] row for a [`Position::NestedStatement`]; `flags` are
/// `crosses`, `last`, `deep`, `stateful` and `direct`, in that order, and
/// `depth` the exact number of enclosing blocks.
const fn right(
    kind: ConstructKind,
    container: Container,
    inner: Container,
    flags: [bool; 5],
    depth: usize,
    context: MethodContext,
) -> RightStatement {
    RightStatement {
        kind,
        position: Position::NestedStatement {
            container,
            crosses: flags[0],
            last: flags[1],
            deep: flags[2],
            stateful: flags[3],
            direct: flags[4],
            inner,
            depth,
        },
        context,
    }
}

/// The measured-right carve-outs of [`DENY_SET`] (see [`RightStatement`]).
#[rustfmt::skip] // one row per line: a measured table
const RIGHT_STATEMENTS: &[RightStatement] = &[
    right(BlockValue, Arm, Arm, [true, false, false, false, false], 1, Actor),
    right(BlockValue, Arm, Arm, [true, true, false, false, false], 1, Actor),
    right(BlockValue, Arm, Arm, [true, true, false, true, false], 1, Actor),
    right(BlockValue, DoBody, DoBody, [true, true, false, true, false], 1, Actor),
    right(BlockValue, LoopBody, Arm, [true, true, true, false, true], 2, Actor),
    right(DetectIfNone, Arm, Arm, [true, false, false, false, false], 1, Actor),
    right(DetectIfNone, Arm, Arm, [true, true, false, false, false], 1, Actor),
    right(DetectIfNone, Arm, DoBody, [true, true, true, false, true], 2, Actor),
    right(DetectIfNone, Handler, Handler, [true, true, false, false, false], 1, Actor),
    right(DetectIfNone, LoopBody, Arm, [true, true, true, false, true], 2, Actor),
    right(DetectIfNoneSearch, Arm, Arm, [true, false, false, false, false], 1, Actor),
    right(DetectIfNoneSearch, Arm, Arm, [true, true, false, false, false], 1, Actor),
    right(DetectIfNoneSearch, Arm, DoBody, [true, true, true, false, true], 2, Actor),
    right(DetectIfNoneSearch, Handler, Handler, [true, true, false, false, false], 1, Actor),
    right(DetectIfNoneSearch, LoopBody, Arm, [true, true, true, false, true], 2, Actor),
    right(Enumeration, Arm, Arm, [true, false, false, false, false], 1, Actor),
    right(Enumeration, Arm, Arm, [true, true, false, false, false], 1, Actor),
    right(Enumeration, Arm, Arm, [true, true, false, true, false], 1, Actor),
    right(Enumeration, Arm, DoBody, [true, true, true, false, true], 2, Actor),
    right(Enumeration, DoBody, DoBody, [true, true, false, true, false], 1, Actor),
    right(Enumeration, Handler, Handler, [true, true, false, false, false], 1, Actor),
    right(Enumeration, LoopBody, Arm, [true, true, true, false, true], 2, Actor),
    right(Enumeration, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, Actor),
    right(Satisfy, Arm, DoBody, [true, true, true, false, false], 2, Actor),
    right(Satisfy, Arm, DoBody, [true, true, true, false, true], 2, Actor),
    right(Satisfy, DoBody, Arm, [true, true, true, false, false], 2, Actor),
    right(Satisfy, DoBody, DoBody, [true, true, false, false, false], 1, Actor),
    right(Satisfy, DoBody, DoBody, [true, true, true, false, false], 2, Actor),
    right(Satisfy, Handler, Handler, [true, true, false, false, false], 1, Actor),
    right(Satisfy, ListOpBody, ListOpBody, [true, false, false, false, false], 1, Actor),
    right(Satisfy, LoopBody, Arm, [true, true, true, false, true], 2, Actor),
    right(DetectIfNone, Arm, DoBody, [true, true, true, false, false], 2, Class),
    right(DetectIfNone, Arm, DoBody, [true, true, true, false, true], 2, Class),
    right(DetectIfNone, DoBody, Arm, [true, true, true, false, true], 2, Class),
    right(DetectIfNone, DoBody, DoBody, [true, true, false, false, false], 1, Class),
    right(DetectIfNone, DoBody, DoBody, [true, true, false, false, true], 1, Class),
    right(DetectIfNone, DoBody, DoBody, [true, true, true, false, false], 2, Class),
    right(DetectIfNone, Handler, Handler, [true, true, false, false, false], 1, Class),
    right(DetectIfNone, Handler, Handler, [true, true, false, false, true], 1, Class),
    right(DetectIfNone, ListOpBody, ListOpBody, [true, false, false, false, false], 1, Class),
    right(DetectIfNone, ListOpBody, ListOpBody, [true, false, false, false, true], 1, Class),
    right(DetectIfNone, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, Class),
    right(DetectIfNone, ProtectedBody, ProtectedBody, [true, true, false, false, true], 1, Class),
    right(DetectIfNoneSearch, Arm, DoBody, [true, true, true, false, false], 2, Class),
    right(DetectIfNoneSearch, Arm, DoBody, [true, true, true, false, true], 2, Class),
    right(DetectIfNoneSearch, DoBody, DoBody, [true, true, false, false, false], 1, Class),
    right(DetectIfNoneSearch, DoBody, DoBody, [true, true, true, false, false], 2, Class),
    right(DetectIfNoneSearch, Handler, Handler, [true, true, false, false, false], 1, Class),
    right(DetectIfNoneSearch, ListOpBody, ListOpBody, [true, false, false, false, false], 1, Class),
    right(DetectIfNoneSearch, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, Class),
    right(Enumeration, Arm, DoBody, [true, true, true, false, false], 2, Class),
    right(Enumeration, Arm, DoBody, [true, true, true, false, true], 2, Class),
    right(Enumeration, CountedLoopBody, CountedLoopBody, [true, true, false, true, false], 1, Class),
    right(Enumeration, DoBody, DoBody, [true, true, false, false, false], 1, Class),
    right(Enumeration, DoBody, DoBody, [true, true, false, true, false], 1, Class),
    right(Enumeration, DoBody, DoBody, [true, true, true, false, false], 2, Class),
    right(Enumeration, Handler, Handler, [true, true, false, false, false], 1, Class),
    right(Enumeration, ListOpBody, ListOpBody, [true, false, false, false, false], 1, Class),
    right(Enumeration, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, Class),
    right(Satisfy, Arm, DoBody, [true, true, true, false, false], 2, Class),
    right(Satisfy, Arm, DoBody, [true, true, true, false, true], 2, Class),
    right(Satisfy, CountedLoopBody, CountedLoopBody, [true, true, false, false, false], 1, Class),
    right(Satisfy, CountedLoopBody, CountedLoopBody, [true, true, false, false, true], 1, Class),
    right(Satisfy, ListOpBody, ListOpBody, [true, false, false, false, false], 1, Class),
    right(DetectIfNone, Arm, DoBody, [true, true, true, false, false], 2, ValueType),
    right(DetectIfNone, Arm, DoBody, [true, true, true, false, true], 2, ValueType),
    right(DetectIfNone, DoBody, DoBody, [true, true, false, false, false], 1, ValueType),
    right(DetectIfNone, DoBody, DoBody, [true, true, true, false, false], 2, ValueType),
    right(DetectIfNone, ListOpBody, ListOpBody, [true, false, false, false, false], 1, ValueType),
    right(DetectIfNone, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, ValueType),
    right(DetectIfNoneSearch, Arm, DoBody, [true, true, true, false, false], 2, ValueType),
    right(DetectIfNoneSearch, Arm, DoBody, [true, true, true, false, true], 2, ValueType),
    right(DetectIfNoneSearch, DoBody, DoBody, [true, true, false, false, false], 1, ValueType),
    right(DetectIfNoneSearch, DoBody, DoBody, [true, true, true, false, false], 2, ValueType),
    right(DetectIfNoneSearch, ListOpBody, ListOpBody, [true, false, false, false, false], 1, ValueType),
    right(DetectIfNoneSearch, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, ValueType),
    right(Enumeration, Arm, DoBody, [true, true, true, false, false], 2, ValueType),
    right(Enumeration, Arm, DoBody, [true, true, true, false, true], 2, ValueType),
    right(Enumeration, CountedLoopBody, CountedLoopBody, [true, true, false, true, false], 1, ValueType),
    right(Enumeration, DoBody, DoBody, [true, true, false, false, false], 1, ValueType),
    right(Enumeration, DoBody, DoBody, [true, true, false, true, false], 1, ValueType),
    right(Enumeration, DoBody, DoBody, [true, true, true, false, false], 2, ValueType),
    right(Enumeration, ListOpBody, ListOpBody, [true, false, false, false, false], 1, ValueType),
    right(Enumeration, ProtectedBody, ProtectedBody, [true, true, false, false, false], 1, ValueType),
    right(Satisfy, Arm, DoBody, [true, true, true, false, false], 2, ValueType),
    right(Satisfy, Arm, DoBody, [true, true, true, false, true], 2, ValueType),
    right(Satisfy, CountedLoopBody, CountedLoopBody, [true, true, false, false, false], 1, ValueType),
    right(Satisfy, CountedLoopBody, CountedLoopBody, [true, true, false, false, true], 1, ValueType),
    right(Satisfy, ListOpBody, ListOpBody, [true, false, false, false, false], 1, ValueType),
];

/// Whether `ADR0131_MEASURE_BYPASS_PHASE0` bypasses the Phase 0 position
/// check ([`DENY_SET`] and [`ALLOW_SET`]), so the measurement harness (`just
/// measure-adr0131-statements`, BT-3841) can compile the shapes the check
/// rejects and run them on a BEAM. Only a build with the `measure-bypass`
/// feature reads it; no other build can turn the check off. It goes with the
/// check (the last BT-3841 PR deletes both).
#[cfg(feature = "measure-bypass")]
fn position_check_bypassed() -> bool {
    use crate::test_helpers::ADR0131_MEASURE_BYPASS_ENV;
    static BYPASSED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *BYPASSED.get_or_init(|| std::env::var_os(ADR0131_MEASURE_BYPASS_ENV).is_some())
}

/// Without the `measure-bypass` feature the check cannot be bypassed.
#[cfg(not(feature = "measure-bypass"))]
const fn position_check_bypassed() -> bool {
    false
}

/// The constructs that still fail somewhere in the statement matrix
/// (the families of the probe matrix: `anySatisfy:`, `detect:ifNone:`, the
/// enumerations, `keysAndValuesDo:` and a block `value`). They are rejected
/// past two nested blocks in class, value-type and actor methods; no
/// [`RIGHT_STATEMENTS`] row may name a deeper shape (a row keyed on the
/// outermost and innermost roles would accept every unmeasured middle role,
/// and `is_allowed` reads the rows before this gate). BT-3841: an arm in a
/// protected body in a `do:` body raised in a value-type method while the
/// coarse rows accepted it.
const UNFINISHED_FAMILIES: &[ConstructKind] = &[
    Satisfy,
    DetectIfNone,
    DetectIfNoneSearch,
    Enumeration,
    KeyedDo,
    BlockValue,
];

/// The kinds lowered as local-threading producers wherever a producer frame
/// holds them (BT-3841): `keysAndValuesDo:`/`doWithKey:` join `do:` in
/// `producer_body_arg_index`. [`DENY_SET`] does not judge them any more, except
/// in a `[...] value` body (still unfinished) or past two nested blocks, where
/// the chain is unmeasured, and in the REPL.
const LOWERED_KINDS: &[ConstructKind] = &[KeyedDo];

/// Whether the chain of blocks around a [`LOWERED_KINDS`] statement is fully
/// described by the position and holds only frames the producer lowers: at
/// most two nested blocks, none of them an evaluated block or a closure
/// (`Container::Other`: a block value passed on or stored, whose write only the
/// §6 check can judge).
fn lowered_chain(position: Position) -> bool {
    match position {
        Position::Statement => true,
        Position::NestedStatement {
            container,
            inner,
            depth,
            ..
        } => {
            depth <= 2
                && ![container, inner]
                    .iter()
                    .any(|role| matches!(role, EvaluatedBlock | Container::Other))
        }
        _ => false,
    }
}

/// What the coarse rows say about a statement, before the measured carve-outs.
fn coarse_allows(kind: ConstructKind, position: Position, context: MethodContext) -> bool {
    if context != Repl && LOWERED_KINDS.contains(&kind) && lowered_chain(position) {
        return true;
    }
    if let Position::NestedStatement { depth, .. } = position {
        // The REPL rows are unmeasured past depth one (BT-3823) and keep the
        // coarse rows' verdict, which may accept unmeasured three-deep shapes
        // until BT-3823 measures them.
        if depth >= 3 && context != Repl && UNFINISHED_FAMILIES.contains(&kind) {
            return false;
        }
    }
    !DENY_SET
        .iter()
        .any(|row| row.denies(kind, position, context))
}

/// Whether the Phase 0 check accepts `kind` in `position` in `context`: a
/// statement unless [`DENY_SET`] lists it, anything else only if
/// [`ALLOW_SET`] does.
pub(crate) fn is_allowed(kind: ConstructKind, position: Position, context: MethodContext) -> bool {
    match position {
        Position::Statement | Position::NestedStatement { .. } => {
            position_check_bypassed()
                || RIGHT_STATEMENTS
                    .iter()
                    .any(|r| r.kind == kind && r.position == position && r.context == context)
                || coarse_allows(kind, position, context)
        }
        _ => {
            position_check_bypassed()
                || ALLOW_SET.iter().any(|row| {
                    row.position == position
                        && row.kinds.contains(&kind)
                        && row.contexts.contains(&context)
                })
        }
    }
}

/// Runs both ADR 0131 checks over every method body and module-level
/// expression list in `module` (see the module docs).
pub(crate) fn check_local_threading(
    module: &Module,
    hierarchy: &ClassHierarchy,
    type_map: Option<&TypeMap>,
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let facts = |class_name: &str, class_side: bool| ReceiverFacts {
        hierarchy,
        type_map,
        self_is_dictionary: !class_side
            && (class_name == "Dictionary"
                || hierarchy
                    .superclass_chain(class_name)
                    .iter()
                    .any(|c| c == "Dictionary")),
    };
    let instance_context = |class_name: &str| {
        if hierarchy.is_actor_subclass(class_name) {
            Actor
        } else {
            ValueType
        }
    };
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let context = instance_context(class_name);
        let callees = if context == Actor {
            threading_callees(class)
        } else {
            HashSet::new()
        };
        for method in &class.methods {
            check_body(
                context,
                param_names(method),
                &method.body,
                &callees,
                &facts(class_name, false),
                diagnostics,
            );
        }
        for method in &class.class_methods {
            check_body(
                Class,
                param_names(method),
                &method.body,
                &HashSet::new(),
                &facts(class_name, true),
                diagnostics,
            );
        }
    }
    for standalone in &module.method_definitions {
        let context = if standalone.is_class_method {
            Class
        } else {
            instance_context(standalone.class_name.name.as_str())
        };
        let method = &standalone.method;
        check_body(
            context,
            param_names(method),
            &method.body,
            &HashSet::new(),
            &facts(
                standalone.class_name.name.as_str(),
                standalone.is_class_method,
            ),
            diagnostics,
        );
    }
    if !module.expressions.is_empty() {
        let known: Vec<EcoString> = known_vars.iter().map(|v| EcoString::from(*v)).collect();
        check_body(
            Repl,
            known,
            &module.expressions,
            &HashSet::new(),
            &facts("", true),
            diagnostics,
        );
    }
}

/// What the lookup-receiver rule
/// ([`receiver_statically_not_dictionary`]) reads for one method body.
#[derive(Clone)]
struct ReceiverFacts<'a> {
    hierarchy: &'a ClassHierarchy,
    /// The inferred types of the module's expressions; `None` when the caller
    /// has none (`beamtalk-lint`'s query), so only `self`/`super` are judged.
    type_map: Option<&'a TypeMap>,
    /// Whether `self` may be a `Dictionary`: an instance method of `Dictionary`
    /// or a subclass.
    self_is_dictionary: bool,
}

/// Walks one method body (or module-level expression list) whose
/// outermost frame binds `params`.
fn check_body(
    context: MethodContext,
    params: Vec<EcoString>,
    body: &[ExpressionStatement],
    callees: &HashSet<(EcoString, usize)>,
    receivers: &ReceiverFacts<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut walker = Walker::new(context, body, callees, diagnostics);
    walker.receivers = Some(receivers.clone());
    walker.scope.push(params.into_iter().collect());
    walker.body(body);
}

/// The `(selector, parameter index)` pairs of `class`'s instance methods that
/// thread a Tier 2 block passed to them ([`callee_threads_block_param`], the
/// predicate codegen's class pre-scan reads as well).
fn threading_callees(class: &crate::ast::ClassDefinition) -> HashSet<(EcoString, usize)> {
    let mut callees = HashSet::new();
    for method in class.methods.iter().filter(|m| !m.is_class_method) {
        let selector = method.selector.name();
        for index in 0..method.parameters.len() {
            if callee_threads_block_param(class, &selector, index) {
                callees.insert((selector.clone(), index));
            }
        }
    }
    callees
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
/// The current statement of a threaded frame body (ADR 0131 Phase 2b).
#[derive(Default)]
struct FrameSites {
    /// The statement's [`loop_body_producer_sites`].
    sites: HashSet<Span>,
    /// The statement's [`LoopBodyContext`] with the frame facts that do not
    /// change per statement filled in at the statement.
    statement: LoopBodyContext,
    /// Whether the frame is a fold's ([`is_fold_frame_body`]).
    fold: bool,
    /// Whether the frame is an arm of a conditional that is a local-threading
    /// producer (ADR 0131 Phase 3): a `BranchArm`, not a loop or fold body.
    arm: bool,
}

/// What kind of threaded frame body a block operand is about to be walked as
/// ([`Walker::pending_frame_body`]).
#[derive(Debug, Clone, Copy)]
struct FrameBody {
    /// A fold's body ([`is_fold_frame_body`]).
    fold: bool,
    /// An arm of a conditional producer (ADR 0131 Phase 3).
    arm: bool,
}

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
    /// The spans of the current method-body statement's
    /// [`sequenced_operand_sites`]: where a loop or fold is lowered as an
    /// ADR 0131 local-threading producer (BT-3749).
    operand_sites: HashSet<Span>,
    /// ADR 0131 Phase 2b (BT-3793): for each enclosing block, innermost last,
    /// `Some(sites)` when it is the body of a loop or fold that codegen lowers
    /// as a threaded frame ([`is_threaded_frame_body`]) — `sites` being the
    /// current statement's [`loop_body_producer_sites`] — and `None` for any
    /// other block (an arm, a handler, a protected body, a closure).
    frame_sites: Vec<Option<FrameSites>>,
    /// Set by a send's walk just before it descends into a block operand that
    /// is a threaded frame body; consumed by [`Walker::block`].
    pending_frame_body: Option<FrameBody>,
    /// ADR 0131 Phase 2d (BT-3798): the spans of the lookup sends
    /// ([`LocalThreadingFamily::Lookup`]) accepted as local-threading
    /// producers at their position by [`Walker::check_construct`], whose
    /// handler blocks the §6 check then exempts ([`Walker::check_send`]).
    lookup_producers: HashSet<Span>,
    /// What the lookup-receiver rule reads; `None` only before [`check_body`]
    /// has set it.
    receivers: Option<ReceiverFacts<'d>>,
    /// The actor self-send callees of the enclosing class that thread a Tier 2
    /// block argument ([`threading_callees`]).
    callees: &'d HashSet<(EcoString, usize)>,
    /// ADR 0131 §6 (BT-3756): the block literals and bare locals of the method
    /// body that escape the method without a send — returned, stored in a
    /// field or class variable, or an element of a collection literal
    /// ([`escape_sites`], the classification behind `escaping_blocks`). Keyed
    /// by the operand's span.
    escapes: HashMap<Span, EscapeShape>,
    diagnostics: &'d mut Vec<Diagnostic>,
}

impl<'d> Walker<'d> {
    fn new(
        context: MethodContext,
        body: &[ExpressionStatement],
        callees: &'d HashSet<(EcoString, usize)>,
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
            operand_sites: HashSet::new(),
            frame_sites: Vec::new(),
            pending_frame_body: None,
            lookup_producers: HashSet::new(),
            receivers: None,
            callees,
            escapes: escape_map(context, body),
            diagnostics,
        }
    }

    fn is_bound(&self, name: &str) -> bool {
        self.scope.iter().any(|f| f.contains(name))
    }

    /// The [`LoopBodyContext`] of the loop or fold body statement being walked:
    /// the innermost frame's statement, with whether that frame and the ones
    /// around it are folds.
    fn loop_body_context(&self) -> LoopBodyContext {
        let frames: Vec<&FrameSites> = self.frame_sites.iter().flatten().collect();
        let Some((innermost, outer)) = frames.split_last() else {
            return LoopBodyContext::default();
        };
        LoopBodyContext {
            innermost_fold: innermost.fold,
            outer_fold: outer.iter().any(|frame| frame.fold),
            in_actor: self.context == Actor,
            ..innermost.statement
        }
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
            if self.depth == 0 {
                // ADR 0131 Phase 2d (BT-3795): the REPL root's sites are its
                // own subset of the method-body rule.
                let sites = if self.context == Repl {
                    repl_operand_sites(&stmt.expression)
                } else {
                    sequenced_operand_sites(&stmt.expression)
                };
                self.operand_sites = sites.into_iter().map(Expression::span).collect();
            } else if let Some(Some(frame)) = self.frame_sites.last_mut() {
                frame.statement = LoopBodyContext::new(&stmt.expression, false, false, false);
                if frame.arm {
                    frame.statement = frame.statement.in_arm();
                }
                frame.sites = loop_body_producer_sites(&stmt.expression)
                    .into_iter()
                    .map(Expression::span)
                    .collect();
            }
            self.expr(&stmt.expression, Position::Statement);
        }
        self.last_statements.pop();
    }

    fn block(&mut self, block: &Block, container: Container) {
        if let Some(shape) = self.escapes.get(&block.span).copied() {
            self.report_escaping_literal(block, shape);
        }
        let frame_body = self.pending_frame_body.take();
        self.frame_sites.push(frame_body.map(|body| FrameSites {
            fold: body.fold,
            arm: body.arm,
            ..FrameSites::default()
        }));
        self.block_in_frame(block, container);
        self.frame_sites.pop();
    }

    fn block_in_frame(&mut self, block: &Block, container: Container) {
        if self.depth == 0 {
            self.outer_stateful =
                self.context != Repl && touches_state(block, self.context == Class);
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
                let lookup_producer = self.lookup_producers.contains(&expr.unwrap_parens().span());
                self.check_send(receiver, selector, arguments, true, top, lookup_producer);
                // The role of each block literal the construct inlines (the
                // same recognizer as `check_construct`); any other block is a
                // block value.
                let construct = local_threading_construct(expr)
                    .filter(LocalThreadingConstruct::is_threaded_today);
                let role = |e: &Expression, is_receiver: bool| match (e, &construct) {
                    (Expression::Block(b), Some(c))
                        if c.blocks.iter().any(|blk| std::ptr::eq(*blk, b)) =>
                    {
                        Container::of(&c.selector, is_receiver)
                    }
                    _ => Container::Other,
                };
                let receiver_role = role(receiver, true);
                // ADR 0131 Phase 3 (BT-3738): the arms of a conditional that is
                // a local-threading producer are `BranchArm` frames, so a loop
                // or fold in an arm is a producer there (codegen's
                // `generate_conditional_arm`). Phase 3b (BT-3817): so are the
                // protected body (the receiver block), handler and cleanup of an
                // `on:do:`/`ensure:` producer.
                let arm_frames = self.is_arm_frame_conditional(expr, position);
                self.pending_frame_body = match receiver.as_ref() {
                    Expression::Block(_) if arm_frames && exception_arm_blocks(expr).is_some() => {
                        Some(FrameBody {
                            fold: false,
                            arm: true,
                        })
                    }
                    _ => None,
                };
                self.operand_in(receiver, Position::Receiver, receiver_role);
                self.pending_frame_body = None;
                for arg in arguments {
                    let arg_role = role(arg, false);
                    self.pending_frame_body = match arg {
                        Expression::Block(b) if is_threaded_frame_body(expr, b) => {
                            Some(FrameBody {
                                fold: is_fold_frame_body(expr, b),
                                arm: false,
                            })
                        }
                        Expression::Block(_) if arm_frames => Some(FrameBody {
                            fold: false,
                            arm: true,
                        }),
                        _ => None,
                    };
                    self.operand_in(arg, Position::Argument, arg_role);
                    self.pending_frame_body = None;
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
                    self.check_send(shared, selector, arguments, false, top, false);
                    for msg in messages {
                        self.check_send(shared, &msg.selector, &msg.arguments, false, top, false);
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
                    if self.context != Repl {
                        self.report_match_arm_writes(arm);
                    }
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
            Expression::Identifier(id) => {
                if let Some(shape) = self.escapes.get(&expr.span()).copied() {
                    self.report_escaping_local(&id.name, shape, expr.span());
                }
            }
            Expression::Literal(..)
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

    /// Whether `expr` is a conditional or an `on:do:`/`ensure:` that is a
    /// local-threading producer at the method root (an operand site, an
    /// assignment's whole value or a method-body statement), whose arms are
    /// therefore `BranchArm` frames (ADR 0131 §2, Phase 3 and 3b). The REPL
    /// keeps its own lowering.
    fn is_arm_frame_conditional(&self, expr: &Expression, position: Position) -> bool {
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        let producer = conditional_producer_writes(expr, &bound, kind)
            .or_else(|| exception_producer_writes(expr, &bound, kind));
        if self.depth == 0 {
            return (self.operand_sites.contains(&expr.span()) || position == Position::Statement)
                && producer.is_some();
        }
        // A conditional nested in an arm frame is a producer there, so its own
        // arms are frames too.
        producer.is_some() && self.arm_frame_conditional_producer(expr)
    }

    /// The [`RootMethodKind`] of the method being walked, `None` in the REPL.
    fn root_method_kind(&self) -> Option<RootMethodKind> {
        match self.context {
            Class => Some(RootMethodKind::Class),
            ValueType => Some(RootMethodKind::ValueType),
            Actor => Some(RootMethodKind::Actor),
            Repl => None,
        }
    }

    /// Whether `expr`, a conditional or `on:do:`/`ensure:` at a site of the
    /// innermost frame, is a local-threading producer in that frame: a
    /// conditional ([`conditional_producer_writes`]) or an `on:do:`/`ensure:`
    /// ([`exception_producer_writes`]) in any threaded frame, a loop or fold
    /// body included (ADR 0131 Phase 3b and 3c, BT-3817/BT-3822), at one of the
    /// statement's sites.
    fn arm_frame_conditional_producer(&self, expr: &Expression) -> bool {
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        self.frame_sites
            .last()
            .and_then(Option::as_ref)
            .is_some_and(|frame| frame.sites.contains(&expr.span()))
            && is_loop_body_producer_site(expr, &self.loop_body_context())
            && (conditional_producer_writes(expr, &bound, kind).is_some()
                || exception_producer_writes(expr, &bound, kind).is_some())
    }

    /// The Phase 0 allow-set check for a construct at `position`.
    #[allow(clippy::too_many_lines)] // one rule per accepted position, each documented
    fn check_construct(&mut self, expr: &Expression, position: Position) {
        let Some(construct) =
            local_threading_construct(expr).filter(LocalThreadingConstruct::is_threaded_today)
        else {
            return;
        };
        let is_lookup = construct.family == LocalThreadingFamily::Lookup;
        let writes = construct_outer_local_writes(&construct, &|name| self.is_bound(name));
        let selector = construct.selector;
        let Some(first) = writes.first().cloned() else {
            return;
        };
        // ADR 0131 Phase 2d (BT-3798 review): a lookup whose receiver is
        // statically known not to be a `Dictionary` would only ever raise at
        // run time (the guard), so it keeps its §6 compile error.
        if is_lookup && self.receiver_is_not_dictionary(expr) {
            return;
        }
        // BT-3841: the same rule for `keysAndValuesDo:`/`doWithKey:`, whose
        // dictionary fold is inlined for a `Dictionary` receiver only (`Ets`
        // and any class that defines the selector run their own method with a
        // closure that cannot return the write). The §1 block-send exemption
        // does not cover them, so the permanent §6 error is reported here.
        if matches!(selector.as_str(), "keysAndValuesDo:" | "doWithKey:")
            && self.receiver_is_not_dictionary(expr)
        {
            if let Some((_, body)) = producer_body_block(expr) {
                self.report_literal_with_hint(
                    body,
                    &selector,
                    format!(
                        "`{selector}` runs the block inline only for a `Dictionary` receiver; \
                         this receiver is not one, so it would call its own `{selector}` with \
                         a closure. Compute the new value without writing it from the block, \
                         or iterate a `Dictionary` (`asDictionary`)"
                    ),
                );
            }
            return;
        }
        // ADR 0131 Phase 2 (BT-3749): a loop or fold at a sequenced operand
        // site of a method-body statement is a local-threading producer,
        // wherever it sits there. Phase 2d (BT-3795): and at an operand site
        // of a REPL input (`repl_operand_sites`), whose root frame threads the
        // bindings map.
        //
        // ADR 0131 Phase 3 (BT-3738): a conditional with a block-literal arm
        // that writes an outer local is one too (`root_producer_writes`), at an
        // operand site, as an assignment's whole value, and as a method-body
        // statement (the statement lowering of a class or value-type method
        // routes it through the producer as well). The REPL keeps its own
        // conditional lowering.
        let bound = |name: &str| self.is_bound(name);
        let root_producer = match self.context {
            // ADR 0131 Phase 2d (BT-3798): a lookup is a producer in a method
            // only; the REPL keeps the §6 rejection.
            Repl if is_lookup => None,
            // The REPL's conditionals at an operand site (`repl_operand_sites`)
            // are producers that adopt the bindings map whole.
            Repl | Actor => root_producer_writes(expr, &bound, RootMethodKind::Actor),
            Class => root_producer_writes(expr, &bound, RootMethodKind::Class),
            ValueType => root_producer_writes(expr, &bound, RootMethodKind::ValueType),
        };
        let root_site = self.operand_sites.contains(&expr.span())
            || (position == Position::Statement
                && self.context != Repl
                && (conditional_arm_blocks(expr).is_some()
                    || exception_arm_blocks(expr).is_some()));
        if self.depth == 0 && root_site && root_producer.is_some() {
            if is_lookup {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        }
        // ADR 0131 Phase 2b (BT-3793): in the body of a loop or fold that
        // codegen lowers as a threaded frame — every enclosing block a
        // frame body, none an arm, handler or closure — a loop or fold at
        // one of the statement's `loop_body_producer_sites` is a
        // local-threading producer there: its prelude rebinds the locals it
        // writes, lowered by the enclosing frame's mode.
        //
        // `is_loop_body_producer_site` is the one narrowing of those sites,
        // shared with codegen's `with_body_producer_sites`: `detect:`/
        // `detect:ifNone:` stay out (they keep their statement rows), and an
        // actor fold frame's value-bearing sites are not lowered by the
        // producer at all, so no other row may accept them either
        // (`r := xs inject: 0 into: [...]` in a `do:` body compiled and then
        // crashed, BT-3797).
        let frame_site = self.depth > 0
            && self.context != Repl
            && !self.frame_sites.is_empty()
            && self.frame_sites.iter().all(Option::is_some)
            && self
                .frame_sites
                .last()
                .and_then(Option::as_ref)
                .is_some_and(|frame| frame.sites.contains(&expr.span()))
            && (operand_producer_writes(expr, &|name| self.is_bound(name)).is_some()
                || self.arm_frame_conditional_producer(expr));
        let ctx = self.loop_body_context();
        let actor_value_site = frame_site && is_actor_fold_value_site(expr, &ctx);
        if frame_site && is_loop_body_producer_site(expr, &ctx) {
            if is_lookup {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        }
        // ADR 0131 Phase 2d (BT-3798): a lookup that is not a producer here has
        // no allow-set row: the §6 check reports its handler's write.
        if is_lookup {
            return;
        }
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
                inner: self.containers.last().copied().unwrap_or(Container::Other),
                depth: self.depth,
            },
            Position::AssignValue { nested: true } if !crosses_block => {
                Position::AssignValue { nested: false }
            }
            Position::FieldAssignValue { nested: true } if !crosses_block => {
                Position::FieldAssignValue { nested: false }
            }
            other => other,
        };
        // ADR 0131 Phase 2c (BT-3794): a handler-writing `detect:ifNone:` is
        // only threaded through the producer shape — a literal search block
        // (`producer_body_block`), the one rule codegen's router and fold
        // plan read. A parenthesised search block routes to the fold but is
        // not packed, so the write would be dropped. The REPL threads its
        // bindings map and is not shaped by it.
        let producer_shape =
            kind != DetectIfNone || self.context == Repl || producer_body_block(expr).is_some();
        if !actor_value_site
            && producer_shape
            && self.allowed_beyond_producers(kind, position, root_producer.is_some())
        {
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

    /// Whether the allow-set accepts `kind` at `position` once the producer
    /// checks of [`Self::check_construct`] have not (`root_producer`: the core
    /// rule names it a producer at a method root or REPL site).
    ///
    /// `tryDo:` has no allow-set row and no statement position that works other
    /// than as a producer (ADR 0131 Phase 4, BT-3750): a Tier 2 block reaching
    /// the arity-0 native raises, so anything the producer checks did not
    /// accept is rejected. The REPL root is the exception: it compiles a
    /// `tryDo:` that is the statement or the whole value of an assignment to the
    /// `{Value, Bindings}` tuple its assembler unpacks
    /// (`repl_try_do_tier2_send`).
    fn allowed_beyond_producers(
        &self,
        kind: ConstructKind,
        position: Position,
        root_producer: bool,
    ) -> bool {
        if kind != TryDo {
            return is_allowed(kind, position, self.context);
        }
        root_producer
            && self.context == Repl
            && self.depth == 0
            && matches!(
                position,
                Position::Statement | Position::AssignValue { nested: false }
            )
    }

    /// Whether the receiver of the lookup send `expr` is statically known not
    /// to be a `Dictionary` ([`receiver_statically_not_dictionary`]).
    fn receiver_is_not_dictionary(&self, expr: &Expression) -> bool {
        let (Some(facts), Expression::MessageSend { receiver, .. }) =
            (&self.receivers, expr.unwrap_parens())
        else {
            return false;
        };
        let ty = facts
            .type_map
            .and_then(|map| map.get(receiver.unwrap_parens().span()));
        receiver_statically_not_dictionary(receiver, ty, facts.self_is_dictionary, facts.hierarchy)
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
        lookup_producer: bool,
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
        // write or raises (probed, BT-3745). The callee must also thread the
        // block (BT-3755): see `semantic_analysis::tier2_callee`'s module docs
        // for the exact callee shapes ([`callee_threads_block_param`], the
        // predicate codegen's class pre-scan reads too). Pinned by
        // `section6_exemptions_match_codegen_shapes` and the compiled
        // fixture `adr0131section6exemptions_actor.bt`.
        let is_self_send = matches!(receiver, Expression::Identifier(id) if id.name == "self");
        let actor_self_send =
            inlined && self.context == Actor && is_self_send && top != TopLevel::None;
        // EXEMPTION (§1 construct): a bare `[...]` argument of an inlined
        // send of a §1 selector (`local_threading_construct`).
        // (`tryDo:` is a §1 construct only sent to the `Result` class: any other
        // receiver takes the ordinary §6 path, actor self-send exemption and
        // callee check included.)
        let construct_send = inlined
            && (is_section1_selector(&sel)
                || crate::semantic_analysis::block_facts::is_result_try_do(receiver, &sel));
        // EXEMPTION (lookup producer, BT-3798): the handler blocks of a lookup
        // accepted as a producer at this position (`check_construct`); a
        // block in any other argument position is a key that is a block value.
        let handler_indices: &[usize] = if lookup_producer {
            crate::state_threading_selectors::lookup_block_arg_indices(&sel)
        } else {
            &[]
        };

        // A block literal receiver. EXEMPTION: a bare `[...]` receiver of an
        // inlined `value`/loop/protected-body send (codegen's
        // `inline_block_captured_mutations` and the loop and exception
        // generators match only a bare block). A cascade checks its shared
        // receiver once itself, so `inlined: false` skips it here.
        if inlined {
            match receiver {
                // BT-3782: a `whileTrue:`/`whileFalse:` CONDITION block has a
                // return channel only where codegen lowers it through the
                // loop's `StateAcc` (`while_loop_threads`).
                Expression::Block(block)
                    if matches!(sel.as_str(), "whileTrue:" | "whileFalse:")
                        && !self.while_condition_threads(block, arguments) =>
                {
                    self.report_literal(block, &sel);
                }
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
        for (index, arg) in arguments.iter().enumerate() {
            match arg {
                Expression::Block(_) if construct_send || handler_indices.contains(&index) => {}
                Expression::Block(block) if actor_self_send => {
                    let writes = self.writes_of(block);
                    // BT-3755: the callee must hand the block's `StateAcc`
                    // back; only then does the write survive.
                    let callee_threads = self
                        .callees
                        .contains(&(EcoString::from(sel.as_str()), index));
                    if !threads_all(block, &writes) {
                        self.report_literal(block, &sel);
                    } else if !callee_threads {
                        self.report_literal_with_hint(block, &sel, self_send_help(&sel));
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

    /// A `match:` arm that assigns an outer local. The arm is an expression,
    /// not a block, so no construct threads its write back to the method
    /// (BT-3817): compiling it would drop the write silently. Walks the whole
    /// arm (guard and body) without entering a nested block literal, whose
    /// writes are the constructs' own business; the arm's pattern variables
    /// (the innermost scope frame, already pushed) are the arm's own.
    fn report_match_arm_writes(&mut self, arm: &MatchArm) {
        let Some((own, outer)) = self.scope.split_last() else {
            return;
        };
        let roots: Vec<&Expression> = arm.guard.iter().chain([&arm.body]).collect();
        let mut block_spans: Vec<Span> = Vec::new();
        for root in &roots {
            crate::ast_walker::walk_expression(root, &mut |e| {
                match e {
                    Expression::Block(b) => block_spans.push(b.span),
                    // A nested `match:` is judged by its own arms (its pattern
                    // variables may shadow an outer local); only its subject
                    // belongs to this arm.
                    Expression::Match { arms, .. } => {
                        for nested in arms {
                            block_spans.push(nested.body.span());
                            if let Some(guard) = &nested.guard {
                                block_spans.push(guard.span());
                            }
                        }
                    }
                    _ => {}
                }
            });
        }
        let mut found: Vec<(EcoString, Span)> = Vec::new();
        for root in &roots {
            crate::ast_walker::walk_expression(root, &mut |e| {
                let Expression::Assignment { target, .. } = e else {
                    return;
                };
                let Expression::Identifier(id) = target.as_ref() else {
                    return;
                };
                let span = e.span();
                let in_block = block_spans
                    .iter()
                    .any(|b| b.start() <= span.start() && span.end() <= b.end());
                if !in_block
                    && !own.contains(&id.name)
                    && outer.iter().any(|f| f.contains(&id.name))
                {
                    found.push((id.name.clone(), span));
                }
            });
        }
        for (name, span) in found {
            self.diagnostics.push(
                Diagnostic::error(
                    format!(
                        "a `match:` arm writes outer local `{name}`, but `match:` cannot return the write"
                    ),
                    span,
                )
                .with_hint(format!(
                    "bind the arm's value instead: `{name} := subject match: [pattern -> newValue; ...]`, \
                     or use `ifTrue:`/`ifNil:` for a branch that writes `{name}`"
                ))
                .with_category(DiagnosticCategory::UnmigratedLocalThreading),
            );
        }
    }

    /// Whether codegen threads an outer-local write in the condition block of
    /// a keyword `whileTrue:`/`whileFalse:` (BT-3782). It does only when the
    /// loop is lowered by the stateful generator and the condition has state
    /// effects (a self-send or field write), in a non-class method: then the
    /// condition is compiled in the loop's own frame. Every other condition
    /// is compiled as a Tier 2 closure that is called with the wrong arity
    /// (value-type, actor) or hangs (class methods); a top-level expression
    /// is left to the REPL's own handling.
    ///
    /// Mirrors `CoreErlangGenerator::while_loop_threads` and
    /// `condition_has_state_effects` in `beamtalk-codegen`; pinned by
    /// `stdlib/test/bt3746stateful_while_test.bt` and the
    /// `while_condition_*` unit tests below.
    fn while_condition_threads(&self, cond: &Block, arguments: &[Expression]) -> bool {
        match self.context {
            Repl => true,
            Class => false,
            ValueType | Actor => {
                matches!(arguments.first(), Some(Expression::Block(_)))
                    && crate::semantic_analysis::block_facts::analyze_block(cond)
                        .has_state_effects()
            }
        }
    }

    fn report_literal(&mut self, block: &Block, selector: &str) {
        let Some(write) = self.writes_of(block).into_iter().next() else {
            return;
        };
        let hint = section6_help(&write.name, selector);
        self.push_literal(block, selector, &write, hint);
    }

    fn report_literal_with_hint(&mut self, block: &Block, selector: &str, hint: String) {
        let Some(write) = self.writes_of(block).into_iter().next() else {
            return;
        };
        self.push_literal(block, selector, &write, hint);
    }

    fn push_literal(
        &mut self,
        block: &Block,
        selector: &str,
        write: &OuterLocalWrite,
        hint: String,
    ) {
        let name = &write.name;
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "block writes outer local `{name}`, but `{selector}` cannot return the write"
                ),
                block.span,
            )
            .with_hint(hint)
            .with_note(format!("`{name}` is written here"), Some(write.span))
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    /// ADR 0131 §6 (BT-3756): a block literal that writes an outer local of
    /// the method and escapes it without a send.
    fn report_escaping_literal(&mut self, block: &Block, shape: EscapeShape) {
        // A block literal assigned straight to a field of an instance is
        // already a compile error from the block analyzer (BT-2809, "cannot
        // mutate captured variable ... inside a closure stored in a field"),
        // which reports it once; `section6_field_stored_literal_is_an_error_in_every_context`
        // pins that some error survives. A class variable store has no such
        // error, so the class-method context reports it here.
        if shape == EscapeShape::StoredLocal
            || (shape == EscapeShape::StoredClassVar && self.context != Class)
        {
            return;
        }
        let Some(write) = self.writes_of(block).into_iter().next() else {
            return;
        };
        let name = &write.name;
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "block writes outer local `{name}`, but it is {} and nothing can return \
                     the write",
                    escape_phrase(shape)
                ),
                block.span,
            )
            .with_hint(escape_help(name))
            .with_note(format!("`{name}` is written here"), Some(write.span))
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    /// As [`Walker::report_escaping_literal`] for a local bound to such a
    /// block (`b := [...]`, then `^b`).
    fn report_escaping_local(&mut self, local: &EcoString, shape: EscapeShape, use_span: Span) {
        if shape == EscapeShape::StoredLocal {
            return;
        }
        let Some((binding, write)) = self.tier2_local_to_report(local) else {
            return;
        };
        let name = &write.name;
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "block stored in `{local}` writes outer local `{name}`, but `{local}` is {} \
                     and nothing can return the write",
                    escape_phrase(shape)
                ),
                binding,
            )
            .with_hint(escape_help(name))
            .with_note(format!("`{name}` is written here"), Some(write.span))
            .with_note(format!("`{local}` escapes here"), Some(use_span))
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    /// The binding and first write of the Tier 2 block-valued local `local`
    /// if it has not been reported yet (one diagnostic per binding); `None`
    /// for a name that is not one, or that a block parameter or match binding
    /// of the same name shadows.
    fn tier2_local_to_report(&mut self, local: &EcoString) -> Option<(Span, OuterLocalWrite)> {
        let info = self.tier2_locals.get(local)?;
        if self.binding_frame(local) != Some(info.frame) {
            return None;
        }
        let found = (info.binding, info.write.clone());
        self.reported_locals.insert(local.clone()).then_some(found)
    }

    fn report_local(&mut self, local: &EcoString, selector: &str, use_span: Span) {
        let Some((binding, write)) = self.tier2_local_to_report(local) else {
            return;
        };
        let name = &write.name;
        let diagnostic = Diagnostic::error(
            format!(
                "block stored in `{local}` writes outer local `{name}`, but `{selector}` cannot \
                 return the write"
            ),
            binding,
        )
        .with_hint(section6_help(name, selector))
        .with_note(format!("`{name}` is written here"), Some(write.span))
        .with_note(
            format!("`{local}` flows to `{selector}` here"),
            Some(use_span),
        )
        .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel);
        self.diagnostics.push(diagnostic);
    }
}

/// The [`escape_sites`] of a method body that §6 inspects (BT-3756). A REPL
/// expression list is skipped: its locals are session bindings, and the value
/// of its last expression goes to the display, not to a caller.
fn escape_map(context: MethodContext, body: &[ExpressionStatement]) -> HashMap<Span, EscapeShape> {
    if context == Repl {
        return HashMap::new();
    }
    escape_sites(body).into_iter().collect()
}

/// How a block escapes, for the §6 message.
fn escape_phrase(shape: EscapeShape) -> &'static str {
    match shape {
        EscapeShape::Returned => "returned from the method",
        EscapeShape::StoredClassVar => "stored in a field or class variable",
        EscapeShape::StoredInLiteral => "placed in a collection literal",
        EscapeShape::StoredLocal => "stored in a local",
    }
}

/// The ADR 0131 §6 help text for a block that escapes the method.
fn escape_help(name: &str) -> String {
    format!(
        "once the method returns, no later call of the block can hand `{name}` back: keep the \
         state in a field (`self.{name} := self.{name} + 1`) or return the new value from the \
         block and assign it where the block is called"
    )
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

/// Whether `block`'s statements touch the method's state: a `self` send or a
/// `self.x := ...` write, through the blocks codegen inlines. This is the
/// shared [`crate::semantic_analysis::facts::StateEffects`] fact codegen's
/// state threading reads.
fn touches_state(block: &Block, in_class_method: bool) -> bool {
    block.body.iter().any(|stmt| {
        crate::semantic_analysis::facts::compute_state_effects_for_expr(
            &stmt.expression,
            in_class_method,
        )
        .any()
    })
}

/// Whether codegen's Tier 2 block protocol threads back every outer local in
/// `writes` (each is in [`captured_local_mutations`]).
fn threads_all(block: &Block, writes: &[OuterLocalWrite]) -> bool {
    let captured = captured_local_mutations(block);
    writes
        .iter()
        .all(|w| captured.iter().any(|c| c == w.name.as_str()))
}

/// The help text for an actor self-send whose callee does not hand the
/// block's write back (BT-3755).
fn self_send_help(selector: &str) -> String {
    format!(
        "`{selector}` must be a `sealed` method of this class that only invokes the block \
         directly (`b value`) as a statement, an assignment or a `^` in its body, or hands it \
         on to another such method. Only two other forms are accepted: the bare callable form \
         (`#(1) do: b`, never `do: [:e | b value: e]`) and the loop shape \
         `last := b value: e` inside `items do: [:e | ...]`, with `last` bound before the loop. \
         Otherwise return the new value from the block and assign it"
    )
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
/// selector, the conditional family or `on:do:`/`ensure:`. (`tryDo:` is a §1
/// construct only when sent to the `Result` class, `is_result_try_do`, which
/// the call site checks with the receiver; ADR 0131 §5.)
fn is_section1_selector(selector: &str) -> bool {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threading_keyword_selector,
    };
    is_state_threading_keyword_selector(selector)
        || is_conditional_selector(selector)
        || is_exception_selector(selector)
}

/// Whether a block literal *receiver* of `selector` is inlined: a block
/// `value` send, a loop's condition, or a protected body. A keyword
/// `whileTrue:`/`whileFalse:` condition is narrowed first by
/// `while_condition_threads` (BT-3782).
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

    /// BT-3841 review: `is_allowed` reads `RIGHT_STATEMENTS` before the depth
    /// gate, so a row past two blocks would silently reopen the hole.
    #[test]
    fn right_statements_name_at_most_two_nested_blocks() {
        for row in RIGHT_STATEMENTS {
            match row.position {
                Position::NestedStatement { depth, .. } => {
                    assert!(
                        (1..=2).contains(&depth),
                        "{:?} {:?}",
                        row.kind,
                        row.position
                    );
                }
                other => panic!("a RIGHT_STATEMENTS row is nested: {other:?}"),
            }
        }
    }

    #[test]
    fn statement_is_allowed_unless_denied() {
        // `do:` as a method-body statement answers right everywhere (s8).
        for context in [Class, ValueType, Actor, Repl] {
            assert!(is_allowed(Do, Position::Statement, context));
        }
        // `detect:ifNone:` with a writing handler packs the write with its
        // search block's, as a statement in every method context (BT-3794).
        for context in [Class, ValueType, Actor] {
            assert!(is_allowed(DetectIfNone, Position::Statement, context));
        }
    }
}
