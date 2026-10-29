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
//! - **ADR 0133: a block that assigns a captured variable and escapes**
//!   ([`DiagnosticCategory::Tier2BlockNoReturnChannel`], permanent; it began
//!   as ADR 0131 §6). A block may assign a local it captured only where the
//!   compiler lowers the block in place: as the direct argument (or receiver,
//!   for `value` and the loops) of a control construct or a compiler-known
//!   collection message ([`crate::state_threading_selectors`], the one list,
//!   which [`threading_constructs_note`] also reads). A block literal that
//!   does so and **escapes** (is stored in a local, returned, stored in a
//!   field or class variable, placed in a collection literal, passed to any
//!   other method, a cascade message included, passed as an Erlang FFI
//!   argument (BT-3917: Erlang calls it as a plain fun and drops the write;
//!   ADR 0041 §Erlang Interop Boundary), or sent a message other than
//!   `value`) is reported at the assignment, naming the shape ([`Escape`]),
//!   in every context, the REPL root included (BT-3858). Reads of captured
//!   variables and `^` inside an escaping block are unaffected.
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
//!
//! Two body-level checks run after the walk: the `ensure:` cleanup that would
//! read a stale local after a `^` or an error ([`report_nlr_out_of_ensure`],
//! permanent, ADR 0133), and the `^` shapes that answer wrong silently
//! ([`report_stale_returns`], BT-4003).

use std::collections::BTreeSet;

use crate::ast::{Block, Expression, ExpressionStatement, MatchArm, MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::{
    EscapeShape, LocalThreadingConstruct, LocalThreadingFamily, LoopBodyContext, OuterLocalWrite,
    RootMethodKind, WriteKind, analyze_block, block_literal_of_other_arity, conditional_arm_blocks,
    conditional_producer_writes, construct_outer_local_writes, escape_sites, exception_arm_blocks,
    exception_producer_writes, is_actor_fold_value_site, is_fold_frame_body,
    is_loop_body_producer_site, is_self_or_super, is_threaded_frame_body, literal_block_value_arm,
    local_threading_construct, lookup_arm_blocks, loop_body_producer_sites, match_arm_block,
    match_arm_body_writes, match_arms, match_guard_writes, match_producer_writes,
    operand_producer_writes, outer_local_writes, outer_state_writes, producer_body_block,
    receiver_statically_not_dictionary, repl_arm_producer_sites, repl_evaluated_block,
    repl_operand_sites, repl_producer_writes, root_producer_writes, sequenced_operand_sites,
    value_blocks,
};
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

impl MethodContext {
    /// The [`RootMethodKind`] of a method body of this context; `None` for a
    /// module-level expression list.
    fn root_method_kind(self) -> Option<RootMethodKind> {
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
    // `^<construct>` inside a block (a non-local return).
    Allowed {
        kinds: &[IfTrue, IfNil, AndOr],
        position: Position::ReturnValue { nested: true },
        contexts: &[Actor],
    },
    // BT-3813: `^xs do: [...]` (and the keyed `do:` forms, BT-3841) inside a
    // block of a class or value-type method. `do:` is a producer (BT-3796)
    // and its closed form answers the `{nil, StateAcc}` carrier; a `^` at a
    // producer site of an arm or loop frame is sequenced by that frame, and
    // every other `^` lowers its construct through the producer with the
    // `Discard` tail (codegen's `nlr_return_construct_value`, the `^` ends
    // the method), so the throw carries `nil`, never the carrier. The
    // agreement test `every_nested_return_value_the_check_accepts_is_lowered_by_the_producer`
    // pins that codegen lowers every construct this row accepts that the
    // core rule (`operand_producer_writes`) names a producer.
    Allowed {
        kinds: &[Do, KeyedDo],
        position: Position::ReturnValue { nested: true },
        contexts: &[Class, ValueType],
    },
];

/// One deny-set row: every listed construct kind, as a statement in every
/// listed context, of the method body (`container: None`) or nested in a
/// block whose [`Position::NestedStatement`] fields match (`None` matches
/// either value). `inner` (the innermost enclosing block's role) is keyed only
/// by the block-local rows ([`deny_block_local`]); [`deny`] leaves it `None`.
struct Denied {
    kinds: &'static [ConstructKind],
    container: Option<Container>,
    inner: Option<Container>,
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
                inner,
                depth: _,
            } => {
                self.container == Some(container)
                    && self.inner.is_none_or(|i| i == inner)
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
        inner: None,
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
/// for its [`Position::NestedStatement`] facts. Most rows are constructs that
/// write a local bound outside their innermost block (`crosses: true`); the
/// [`deny_block_local`] rows at the end are the constructs that write only a
/// local of that block (`crosses: false`) and still lose it (BT-3776, measured
/// with every former `gap` line of the probe matrix). The same
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
    deny(None, None, None, None, None, None, &[Class], &[Enumeration, IfNil, AndOr, BlockValue]),
    deny(None, None, None, None, None, None, &[ValueType], &[Enumeration, IfNil, AndOr]),
    deny(Some(Arm), Some(true), None, Some(true), None, None, &[Actor], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), None, Some(false), None, Some(false), &[Actor], &[Loop, KeyedDo, Enumeration, BlockValue]),
    deny(Some(Arm), Some(true), None, None, None, None, &[Class, ValueType], ALL),
    deny(Some(Arm), Some(true), Some(false), None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), Some(true), Some(true), None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(Arm), Some(true), Some(true), Some(false), None, Some(false), &[Repl], &[CountedLoop, Do, ListOp, Satisfy, Exception]),
    deny(Some(Arm), Some(true), Some(true), Some(false), None, Some(true), &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), Some(false), &[Actor], &[Loop, KeyedDo, ListOp, Satisfy, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), Some(true), &[Actor], &[Loop, ListOp, Satisfy, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, Some(false), None, &[Class, ValueType], &[Loop, Do, KeyedDo, ListOp, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(CountedLoopBody), Some(true), None, None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
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
    deny(Some(LoopBody), Some(true), None, Some(false), Some(false), None, &[Actor], &[Loop, Do, KeyedDo, ListOp, Satisfy, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, Some(true), Some(false), Some(false), &[Actor], ALL),
    deny(Some(LoopBody), Some(true), None, Some(true), Some(false), Some(true), &[Actor], &[KeyedDo, Satisfy, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, None, Some(false), None, &[Class, ValueType], &[Do, KeyedDo, DetectIfNoneSearch, Satisfy, DetectIfNone, Enumeration, IfTrue, IfNil, AndOr, Exception, BlockValue]),
    deny(Some(LoopBody), Some(true), None, None, None, None, &[Repl], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Actor], &[Loop, KeyedDo, Enumeration, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Actor], &[BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(false), None, None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(true), None, None, &[Actor, Class, ValueType], ALL_BUT_CONDITIONALS),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Class], &[Do, KeyedDo, DetectIfNone, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Class, ValueType], &[Loop, Do, KeyedDo, DetectIfNoneSearch, DetectIfNone, Enumeration, BlockValue]),
    deny(Some(ProtectedBody), Some(true), Some(false), None, None, None, &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(true), None, None, &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(false), &[Repl], &[IfTrue, IfNil, AndOr, Exception]),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[Repl], ALL),
    deny(Some(ProtectedBody), Some(true), Some(true), Some(false), None, Some(true), &[ValueType], &[Do, KeyedDo, BlockValue]),
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
    right(DetectIfNone, EvaluatedBlock, EvaluatedBlock, [true, true, false, false, false], 1, Actor),
    right(DetectIfNoneSearch, EvaluatedBlock, EvaluatedBlock, [true, true, false, false, false], 1, Actor),
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
const LOWERED_KINDS: &[ConstructKind] = &[KeyedDo, Satisfy, Enumeration];

/// The kinds lowered only where the fold is measured right: `detect:ifNone:`
/// (its search block or its handler writing an outer local) in a class or
/// value-type method. In an actor method the same folds still answer wrong or
/// fail to compile in nested positions and stay with [`DENY_SET`].
const LOWERED_CLASS_VALUE_KINDS: &[ConstructKind] = &[DetectIfNone, DetectIfNoneSearch];

/// Whether `kind` is lowered as a producer in `context`.
fn lowered_in(kind: ConstructKind, context: MethodContext) -> bool {
    context != Repl
        && (LOWERED_KINDS.contains(&kind)
            || (matches!(context, Class | ValueType) && LOWERED_CLASS_VALUE_KINDS.contains(&kind)))
}

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
    if lowered_in(kind, context) && lowered_chain(position) {
        return true;
    }
    if let Position::NestedStatement { depth, .. } = position {
        // The REPL rows are unmeasured past depth one and keep the coarse
        // rows' verdict, which may accept unmeasured three-deep shapes; a
        // construct at a REPL arm site never reaches them (it is a producer,
        // BT-3823), and REPL loop bodies are BT-3876.
        if depth >= 3 && context != Repl && UNFINISHED_FAMILIES.contains(&kind) {
            return false;
        }
    }
    !DENY_SET
        .iter()
        .any(|row| row.denies(kind, position, context))
}

/// A shape of [`RESIDUAL_DENY_SET`]: a residual of ADR 0131 that crashes or
/// answers wrong today but that [`DENY_SET`] and [`ALLOW_SET`] cannot name,
/// because its threaded set ([`construct_outer_local_writes`]) is empty (the
/// block binds a local of its own, or writes only a field) or it is not a
/// construct send at all (`match:`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Residual {
    /// BT-3875: a `match:` arm (body or guard) that writes an outer local
    /// drops the write unless the `match:` is a local-threading producer where
    /// it sits. BT-3947 (BT-3912) lowers one that is, with each arm a
    /// `BranchArm` frame ([`match_producer_writes`], and at the REPL
    /// `is_repl_arm_construct`); what stays this error is a `match:` that is
    /// not a producer there: a guard that writes, or a `match:` at no producer
    /// site (the subject of another `match:`).
    MatchArmWrite,
    /// BT-3948: an actor `[:z | ...] value: x` (a literal block applied in
    /// place) whose body assigns a local of its own, with a loop among the
    /// enclosing blocks. The loop frame carries its locals as parameters (no
    /// `StateAcc`) while the block body is lowered as a state-threading
    /// scope. Measured on a BEAM (the rows marked conservative below were
    /// not measured failing; BT-3947 flips them with the rest), these fail
    /// `erlc` (unbound `StateAcc`), raise, or answer a `{Value, Locals}` carrier instead of the value:
    /// the send in a `whileTrue:`/`whileFalse:` condition (also with a
    /// state-touching body, and as a statement of the condition unless the
    /// block is one statement with every assignment in an arm); in the
    /// loop's own body when it rebinds an outer local and touches no state;
    /// below conditional arms of the body (`ifTrue:`, `and:`, `ifNil:`...)
    /// unless it is `r := [..] value: i` of a fresh local, or a statement of
    /// an arm in a state-touching body; and whenever the innermost loop sits
    /// inside another block (a method-level arm, `on:do:`, `ensure:`,
    /// `value`, a `do:` body or another loop) or has a `do:`/`collect:`/
    /// `inject:` body below it. A loop body that touches state, `do:`/
    /// `collect:` bodies outside a loop, and class and value-type methods
    /// answer right. A `to:do:` directly in a non-rebinding `to:do:` also
    /// answered right, but `timesRepeat:` outside it and a third level did
    /// not, and the block roles do not tell them apart, so it is rejected.
    ///
    /// Conservative rejections (no BEAM measurement of a failure; the
    /// anchor on the innermost loop rejects them and BT-3947 flips them): a
    /// loop inside a closure stored by `add:` or inside a non-arm block, a
    /// loop in a method-level arm whose block is statement-only, and a
    /// `to:do:` in a non-rebinding `to:do:` (measured right). Every other
    /// denied shape in the pin test was measured failing or answering wrong.
    InlineBlockLocalInLoop,
    /// BT-4003: in an actor method, a loop or fold whose block sends to `self`
    /// or writes a field, and writes no outer local (one that does is a
    /// local-threading producer, the Phase 0 check's), used as an operand
    /// inside the body of another loop or fold. Measured on a BEAM: an `inject:into:` as a binary or keyword
    /// argument crashes (`Tuple does not understand 'plusFromNumber:'`) or
    /// answers the raw `{Value, State}` pair, a `do:` as a receiver drops
    /// every write, and a `collect:` whose block writes a field drops half of
    /// them. Measured right, and accepted: the statement of the body, the
    /// value of an assignment in it, and a `collect:`/`select:` whose block
    /// writes no field (only sends to `self`) as the receiver of a message.
    /// The same folds at the method level, and class and value-type methods,
    /// answer right (a value-type self-send never changes the caller's
    /// `self`; a value-type fold that writes a field in a fold body is already
    /// a codegen error). BT-4276: the search for the enclosing loop or fold
    /// stops at the nearest closure boundary, so the same fold inside a block
    /// literal that is not run in place in that body (stored, returned, or
    /// passed to a method) is judged by the closure's own context; measured
    /// right in `stdlib/test/bt4276fold_in_closure_test.bt`.
    StateFoldOperandInFold,
}

/// One [`RESIDUAL_DENY_SET`] row: `shape` is an error in `contexts`.
struct ResidualDenied {
    shape: Residual,
    contexts: &'static [MethodContext],
    /// The issue that recorded the shape.
    issue: &'static str,
    /// The ADR 0133 Phase 3 issue whose lowering fixes it, deleting the row
    /// and flipping its `PIN-BUG` tests.
    fixed_by: &'static str,
}

/// ADR 0133 Phase 3 (BT-3909): the residual shapes made loud before the IR
/// consolidation lowers them, each a compile error in the contexts where it
/// was measured crashing or answering wrong on a BEAM. The probe cells are the
/// `cond`/`fresh` and `groupBy` lines of `adr0131_statement_probes.tsv` and
/// the REPL `match` line; the `residual_*` tests in
/// `semantic_analysis/tests/local_threading.rs` pin each repro. BT-3911
/// deleted the `LoopConditionArmWrite` row, BT-3912 the `GroupByValue` row. The
/// `MatchArmWrite` row (BT-3947, BT-3912) is narrowed to the `match:`s that are
/// not producers where they sit; BT-3913 retires it with the other position
/// rows. The `InlineBlockLocalInLoop` row (BT-3948) stays: BT-3912's closure
/// frames lower the shapes below a conditional arm and a loop in another block,
/// but a `value:` applied in place in a loop frame that carries its locals as
/// parameters still emits an unbound `StateAcc`.
const RESIDUAL_DENY_SET: &[ResidualDenied] = &[
    // Class, value-type and actor methods since BT-3817; the REPL since
    // BT-3909 (maintainer-approved REPL output change: the same input drew
    // the BT-3864 value-position error before). BT-3947 (BT-3912) lowers a
    // `match:` that is a producer; this is the rest.
    ResidualDenied {
        shape: Residual::MatchArmWrite,
        contexts: &[Class, ValueType, Actor, Repl],
        issue: "BT-3875",
        fixed_by: "BT-3913",
    },
    // Closure bodies need a frame identity of their own (BT-3947 item 2).
    ResidualDenied {
        shape: Residual::InlineBlockLocalInLoop,
        contexts: &[Actor],
        issue: "BT-3948",
        fixed_by: "BT-3947",
    },
    ResidualDenied {
        shape: Residual::StateFoldOperandInFold,
        contexts: &[Actor],
        issue: "BT-4003",
        fixed_by: "BT-3947",
    },
];

/// The [`RESIDUAL_DENY_SET`] row that makes `shape` an error in `context`.
fn residual_denied(shape: Residual, context: MethodContext) -> Option<&'static ResidualDenied> {
    // The measurement harness compiles the probe cells these rows reject
    // (`match:` arms are not probe cells: BT-3817's error is not bypassed).
    if position_check_bypassed() && shape != Residual::MatchArmWrite {
        return None;
    }
    RESIDUAL_DENY_SET
        .iter()
        .find(|row| row.shape == shape && row.contexts.contains(&context))
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
        class_name: EcoString::from(class_name),
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
        for method in &class.methods {
            check_method(context, method, &facts(class_name, false), diagnostics);
        }
        for method in &class.class_methods {
            check_method(Class, method, &facts(class_name, true), diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        let context = if standalone.is_class_method {
            Class
        } else {
            instance_context(standalone.class_name.name.as_str())
        };
        check_method(
            context,
            &standalone.method,
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
            None,
            &known,
            &module.expressions,
            &facts("", true),
            diagnostics,
        );
    }
}

/// What the lookup-receiver rule
/// ([`receiver_statically_not_dictionary`]) and the escaping-block message
/// ([`Walker::passed_escape`]) read for one method body.
#[derive(Clone)]
struct ReceiverFacts<'a> {
    hierarchy: &'a ClassHierarchy,
    /// The inferred types of the module's expressions; `None` when the caller
    /// has none (`beamtalk-lint`'s query), so only `self`/`super` are judged.
    type_map: Option<&'a TypeMap>,
    /// Whether `self` may be a `Dictionary`: an instance method of `Dictionary`
    /// or a subclass.
    self_is_dictionary: bool,
    /// The class the method belongs to (empty at the REPL root): where a
    /// class-side `self` send goes.
    class_name: EcoString,
}

/// [`check_body`] for one method.
fn check_method(
    context: MethodContext,
    method: &crate::ast::MethodDefinition,
    receivers: &ReceiverFacts<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_body(
        context,
        Some(method.selector.name()),
        &param_names(method),
        &method.body,
        receivers,
        diagnostics,
    );
}

/// Walks one method body (`selector` its method's), or the module-level
/// expression list (`selector` `None`), whose outermost frame binds `params`.
fn check_body(
    context: MethodContext,
    selector: Option<EcoString>,
    params: &[EcoString],
    body: &[ExpressionStatement],
    receivers: &ReceiverFacts<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut walker = Walker::new(context, body, diagnostics);
    walker.receivers = Some(receivers.clone());
    walker.method_selector = selector;
    walker.scope.push(params.iter().cloned().collect());
    walker.body(body);
    walker.finish();
    report_nlr_out_of_ensure(context, params, body, diagnostics);
    report_stale_returns(context, params, body, diagnostics);
}

/// BT-4003: the two `^` shapes that answer wrong silently.
///
/// - **A `^` block that reads a stale local.** A block literal that is not run
///   in place (a closure: stored, passed to a method, ...) captures the values
///   of the locals it reads when it is created, so a `^` in it answers a
///   local's value at creation, not at the throw (measured on a BEAM in a
///   class, a value type, an actor and the REPL: `t := 0. b := [^t]. t := 5.
///   b value` answers 0). Such a block that holds a `^` is an error when it
///   reads a local that the method assigns after the block literal, or inside
///   the body of a loop or fold that creates the block (a later iteration's
///   write); at the REPL, when it reads any binding (a later input may assign
///   it). A block that reads only its own parameters and locals, or locals
///   written only before it, is measured right.
///
///   Conservative: every block that is not a [`local_threading_construct`]
///   body counts as escaping, consistent with ADR 0133 (a block passed to a
///   user method escapes). So `t := 0. self withFallback: [^t]. t := 5` is
///   rejected even when `withFallback:` runs the block before it returns and
///   the `^` would answer right; at the REPL, so is any such block that reads
///   a workspace binding. This cannot be narrowed with `escape_sites`, which
///   does not count a block passed to a method: `b := self keep: [^t]. t :=
///   5. b value` hands the block back and answers the stale 0 (BT-4275, closed
///   won't-do). Telling `withFallback:` from `keep:` would need a per-callee
///   "does not retain its block" fact.
/// - **A `^` in a REPL loop or fold body** (a block a loop or collection
///   operation runs in place, [`local_threading_construct`]) is ignored: the
///   loop runs on and the expression answers the loop's own value
///   (`[n < 3] whileTrue: [^10]` never ends). Measured right at the REPL: a
///   `^` at the root, in a conditional arm, an evaluated block, a protected
///   block, and a stored closure.
fn report_stale_returns(
    context: MethodContext,
    params: &[EcoString],
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut assigned: Vec<(EcoString, Span)> = Vec::new();
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Assignment { target, .. } => {
                if let Expression::Identifier(id) = target.as_ref() {
                    assigned.push((id.name.clone(), e.span()));
                }
            }
            Expression::DestructureAssignment { pattern, .. } => {
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                assigned.extend(ids.into_iter().map(|id| (id.name, e.span())));
            }
            _ => {}
        });
    }
    let mut scan = StaleReturnScan {
        context,
        bindings: params,
        assigned,
        loops: Vec::new(),
        in_loop_body: false,
        diagnostics,
    };
    for stmt in body {
        scan.expr(&stmt.expression);
    }
}

/// The walk of [`report_stale_returns`].
struct StaleReturnScan<'a> {
    context: MethodContext,
    /// At the REPL, the bindings of earlier inputs.
    bindings: &'a [EcoString],
    /// Every local assignment of the body, with its span.
    assigned: Vec<(EcoString, Span)>,
    /// The spans of the enclosing loop and fold bodies run in place.
    loops: Vec<Span>,
    /// Whether the walk is in a loop or fold body run in place, with no
    /// closure between.
    in_loop_body: bool,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl StaleReturnScan<'_> {
    fn body(&mut self, block: &Block) {
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
    }

    /// A block literal that is not run in place.
    fn closure(&mut self, block: &Block) {
        if block_holds_return(block) {
            let mut reads: Vec<String> = analyze_block(block).captured_reads.into_iter().collect();
            reads.sort();
            if let Some(name) = reads.into_iter().find(|n| self.is_stale(n, block)) {
                let (what, why) = if self.context == Repl {
                    (
                        "the binding",
                        "a later assignment of it is not seen by the block",
                    )
                } else {
                    (
                        "the local",
                        "the method assigns it after the block is created",
                    )
                };
                self.diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "a block that returns non-locally (`^`) and is not run where it is \
                             written reads {what} `{name}`, but {why}: the block keeps the value \
                             `{name}` had when it was created, so if it runs later its `^` may \
                             answer a stale `{name}`. This is rejected conservatively because \
                             the block escapes, even where it is in fact run before the \
                             assignment (BT-4003)"
                        ),
                        block.span,
                    )
                    .with_hint(format!(
                        "pass the current value in as a block argument (`[:v | ^v] value: \
                         {name}`), or write the block where it is run"
                    ))
                    .with_category(DiagnosticCategory::UnmigratedLocalThreading),
                );
            }
        }
        let outer = std::mem::replace(&mut self.in_loop_body, false);
        self.body(block);
        self.in_loop_body = outer;
    }

    fn is_stale(&self, name: &str, block: &Block) -> bool {
        let inside = |outer: Span, s: Span| outer.start() <= s.start() && s.end() <= outer.end();
        if self.context == Repl {
            return self.bindings.iter().any(|b| b == name)
                || self.assigned.iter().any(|(n, _)| n == name);
        }
        self.assigned.iter().any(|(n, s)| {
            n == name
                && !inside(block.span, *s)
                && (s.start() >= block.span.end() || self.loops.iter().any(|l| inside(*l, *s)))
        })
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Return { value, span } => {
                if self.context == Repl && self.in_loop_body {
                    self.diagnostics.push(
                        Diagnostic::error(
                            "a non-local return (`^`) in the body of a loop or collection \
                             operation is not supported in a top-level expression: the loop \
                             would ignore it and run on (BT-4003)",
                            *span,
                        )
                        .with_hint(
                            "answer the value with `detect:ifNone:` or `inject:into:`, or move \
                             the loop into a method and return from there",
                        )
                        .with_category(DiagnosticCategory::UnmigratedLocalThreading),
                    );
                }
                self.expr(value);
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                let construct = local_threading_construct(expr)
                    .filter(LocalThreadingConstruct::is_threaded_today);
                for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                    let Expression::Block(b) = operand else {
                        self.expr(operand);
                        continue;
                    };
                    match &construct {
                        Some(c) if c.blocks.iter().any(|blk| std::ptr::eq(*blk, b)) => {
                            if matches!(
                                c.family,
                                LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                            ) {
                                self.loops.push(b.span);
                                let outer = std::mem::replace(&mut self.in_loop_body, true);
                                self.body(b);
                                self.in_loop_body = outer;
                                self.loops.pop();
                            } else {
                                self.body(b);
                            }
                        }
                        _ => self.closure(b),
                    }
                }
            }
            Expression::Block(b) => self.closure(b),
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::Assignment { target, value, .. } => {
                self.expr(target);
                self.expr(value);
            }
            Expression::DestructureAssignment { value, .. } => self.expr(value),
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.expr(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.expr(arg);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                }
            }
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.expr(&pair.key);
                    self.expr(&pair.value);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements.iter().chain(tail.as_deref()) {
                    self.expr(e);
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    self.expr(e);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.expr(e);
                    }
                }
            }
            Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }
}

/// BT-3910 / BT-3947 / BT-3993 / BT-4003: an `ensure:` cleanup may not read a
/// local its protected block writes when the protected block can leave
/// abruptly after the write, by a non-local return or (BT-4003) by an error.
/// This is a compile error by design (ADR 0133, Consequences): seeing the new
/// value would need a runtime mechanism that carries the protected block's
/// locals out with the throw, which was rejected (BT-4063). The `^` is a throw; the cleanup runs from the `catch`
/// arm over the entry `StateAcc`, with no `LocalRebind` of the protected body's
/// writes, so the cleanup would read the old value (the correct answer is the
/// new one). It makes no difference whether the write is the returned value
/// (`^t := e`) or a statement before the `^`, whether the `^` sits directly in
/// the protected block or in a block literal nested in it, or whether the write
/// is a statement or a value. Without a `^` in the protected block, or with a
/// cleanup that does not read what it writes, the write is seen (ADR 0131 §1).
/// An `on:do:` protected body has no such position: a `^` passes through its
/// catch untouched and no handler runs.
///
/// BT-3993: the throw can also come from a block value created elsewhere
/// (measured on a BEAM in a class, a value type and an actor: all read the old
/// `t`). The static view cannot tell which value is a block, so it is fully
/// conservative ([`nlr_cause`]): any `value`-family send to a receiver that is
/// not a block literal analysed in place (a call result and a collection
/// element included), and any non-constant local, parameter or field (parentheses
/// unwrapped, cascades included) used as the receiver or an argument of a
/// non-binary send, counts as a possible non-local return. Such a protected
/// block is rejected when its cleanup reads a write of it, even if no block
/// returns. Residuals (BT-3950): a self-send whose method evaluates a stored
/// `^` block, and a block reached as a call result or collection element that
/// is passed as an argument or sent a non-`value` message, are not seen.
///
/// BT-4003: an error is a throw too, and the cleanup that runs on it reads the
/// entry locals (measured on a BEAM in a class, a value type, an actor and the
/// REPL: `[[t := 9. 1 / 0] ensure: [rec record: t]] on: Error do: [...]`
/// records 0). Without a `^` cause, a protected block is rejected when
/// something that may raise ([`raise_after_write`]: any send, a destructuring
/// match, a `match:` or an interpolation) runs after a write the cleanup reads,
/// in evaluation order, a loop or fold body counting twice (its next iteration
/// runs after the write). A write that is the last thing the protected block
/// does, or one only preceded by what may raise, is seen (measured right).
///
/// Every local the cleanup reads counts, except one the cleanup binds itself
/// before reading it (its own first assignment): a cleanup that binds its own
/// `x` (`[x := 5. ^x] ensure: [x := 0. x]`) reads that, not the protected
/// block's.
///
/// The one recognizer of this rule: `beamtalk lint`'s `dead_returned_assignment`
/// skips a `^t := e` inside the span of this error rather than re-deriving it.
fn report_nlr_out_of_ensure(
    context: MethodContext,
    params: &[EcoString],
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Which locals only ever hold a literal (never a block): every other
    // binding site (a computed assignment, a block parameter, a destructuring
    // or `match:` pattern variable) makes a name "other".
    let mut literal_bound: BTreeSet<EcoString> = BTreeSet::new();
    let mut other_bound: BTreeSet<EcoString> = BTreeSet::new();
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Block(b) => {
                other_bound.extend(b.parameters.iter().map(|p| p.name.clone()));
            }
            Expression::Assignment { target, value, .. } => {
                if let Expression::Identifier(id) = target.as_ref() {
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        literal_bound.insert(id.name.clone());
                    } else {
                        other_bound.insert(id.name.clone());
                    }
                }
            }
            Expression::DestructureAssignment { pattern, .. } => {
                other_bound.extend(
                    crate::semantic_analysis::extract_pattern_bindings(pattern)
                        .0
                        .into_iter()
                        .map(|i| i.name),
                );
            }
            Expression::Match { arms, .. } => {
                for arm in arms {
                    other_bound.extend(
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern)
                            .0
                            .into_iter()
                            .map(|i| i.name),
                    );
                }
            }
            _ => {}
        });
    }
    let sources = NlrSources {
        constant_locals: literal_bound
            .difference(&other_bound)
            .filter(|n| !params.contains(n))
            .cloned()
            .collect(),
    };
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } = e
                && parts.len() == 1
                && parts[0].keyword == "ensure:"
                && let Expression::Block(protected) = receiver.as_ref()
                && let [Expression::Block(cleanup)] = arguments.as_slice()
            {
                if let Some(dropped) = nlr_dropped_local(protected, cleanup, &sources) {
                    let EnsureDroppedLocal { name, cause, exit } = dropped;
                    let fix = match exit {
                        AbruptExit::Return => format!(
                            "assign `{name}` and return after the `ensure:`, or keep the \
                             non-local return out of the protected block"
                        ),
                        AbruptExit::Error => format!(
                            "assign `{name}` after the `ensure:`, or move what may raise \
                             before the assignment (the check is conservative: anything after \
                             the write that may raise counts, even inside an `on:do:` that \
                             catches it)"
                        ),
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "an `ensure:` protected block that writes `{name}` {cause}, and \
                                 its cleanup reads `{name}`: in {context} the cleanup would not \
                                 see the new `{name}` (ADR 0133)"
                            ),
                            protected.span,
                        )
                        .with_hint(format!(
                            "{fix}; or do not read `{name}` in the cleanup (an `ensure:` \
                             cleanup cannot read a local its protected block writes when the \
                             block can exit early)"
                        ))
                        .with_category(DiagnosticCategory::UnmigratedLocalThreading),
                    );
                }
            }
        });
    }
}

/// What the conservative reading of "may return non-locally" needs to know
/// about one method (see [`report_nlr_out_of_ensure`]).
struct NlrSources {
    /// Locals that only ever hold a literal: never a block, so passing one on
    /// cannot hand a block over.
    constant_locals: BTreeSet<EcoString>,
}

fn block_holds_return(block: &Block) -> bool {
    let mut found = false;
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            found |= matches!(e, Expression::Return { .. });
        });
    }
    found
}

/// What in `protected` may throw a non-local return: a `^` written in it (or in
/// a block literal nested in it), or, since the static view cannot tell which
/// block a value is, any evaluation of a block that is not a literal analysed
/// in place: a `value`-family send to anything but a block literal (a local, a
/// parameter, a field, a call result, a collection element, ...), or a
/// non-literal value (a local that is not a constant, a parameter, a field)
/// passed as an argument of a keyword send, which may hand a block over to be
/// evaluated. Binary operands are not block hand-overs. `None` when nothing
/// does.
fn nlr_cause(protected: &Block, sources: &NlrSources) -> Option<String> {
    if block_holds_return(protected) {
        return Some("holds a non-local return (`^`)".to_string());
    }
    // A local, parameter or field (parentheses unwrapped) that may hold a
    // block: anything but `self`/`super`, the constants and a local that only
    // ever holds a literal.
    let blockish = |e: &Expression| -> Option<String> {
        match e.unwrap_parens() {
            Expression::Identifier(id)
                if !matches!(
                    id.name.as_str(),
                    "self" | "super" | "nil" | "true" | "false"
                ) && !sources.constant_locals.contains(&id.name) =>
            {
                Some(id.name.to_string())
            }
            Expression::FieldAccess { field, .. } => Some(field.name.to_string()),
            _ => None,
        }
    };
    let check_send = |receiver: &Expression,
                      selector: &MessageSelector,
                      args: &[Expression]|
     -> Option<String> {
        if selector.is_block_invocation() {
            if matches!(receiver.unwrap_parens(), Expression::Block(_)) {
                return None;
            }
            return Some("may return non-locally (it evaluates a block with `value`)".to_string());
        }
        // Binary operands are not block hand-overs.
        if matches!(selector, MessageSelector::Binary(_)) {
            return None;
        }
        std::iter::once(receiver)
            .chain(args)
            .find_map(&blockish)
            .map(|name| format!("may return non-locally (`{name}` may be a block it runs)"))
    };
    let mut cause = None;
    for stmt in &protected.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if cause.is_some() {
                return;
            }
            match e {
                Expression::MessageSend {
                    receiver,
                    selector,
                    arguments,
                    ..
                } => cause = check_send(receiver, selector, arguments),
                Expression::Cascade {
                    receiver, messages, ..
                } => {
                    cause = messages
                        .iter()
                        .find_map(|m| check_send(receiver, &m.selector, &m.arguments));
                }
                _ => {}
            }
        });
    }
    cause
}

/// The names `cleanup` binds itself before it reads them: its parameters, and a
/// name assigned by a top-level statement of the cleanup (executed
/// unconditionally) whose value does not mention it, when no earlier statement
/// mentions it. An assignment inside an arm, a loop body or any nested block
/// literal might not run, so it does not bind.
fn names_bound_by_cleanup_first(cleanup: &Block) -> BTreeSet<EcoString> {
    let mut own: BTreeSet<EcoString> = cleanup.parameters.iter().map(|p| p.name.clone()).collect();
    let mut seen: BTreeSet<EcoString> = BTreeSet::new();
    for stmt in &cleanup.body {
        let mut mentioned: BTreeSet<EcoString> = BTreeSet::new();
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Identifier(id) = e {
                mentioned.insert(id.name.clone());
            }
        });
        if let Expression::Assignment { target, value, .. } = &stmt.expression
            && let Expression::Identifier(id) = target.as_ref()
        {
            let mut in_value = false;
            crate::ast_walker::walk_expression(value, &mut |v| {
                in_value |= matches!(v, Expression::Identifier(i) if i.name == id.name);
            });
            if !in_value && !seen.contains(&id.name) {
                own.insert(id.name.clone());
            }
        }
        seen.extend(mentioned);
    }
    own
}

/// How the protected block of an `ensure:` leaves abruptly
/// ([`report_nlr_out_of_ensure`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AbruptExit {
    /// A non-local return (`^`), possibly from a block value.
    Return,
    /// An error raised after the write (BT-4003).
    Error,
}

/// A local an `ensure:` cleanup would read stale ([`nlr_dropped_local`]).
struct EnsureDroppedLocal {
    name: String,
    /// What makes the protected block leave abruptly, as a phrase.
    cause: String,
    exit: AbruptExit,
}

/// The local `protected` writes and `cleanup` reads (but does not bind itself
/// first) when the protected block can leave abruptly after writing it (see
/// [`report_nlr_out_of_ensure`]): with a non-local return cause, the first such
/// local in name order; otherwise the first one written before something that
/// may raise ([`raise_after_write`]).
fn nlr_dropped_local(
    protected: &Block,
    cleanup: &Block,
    sources: &NlrSources,
) -> Option<EnsureDroppedLocal> {
    // The writes that reach the protected block's frame, nested loop, fold and
    // conditional bodies included (ADR 0131 threads those back out, and the `^`
    // drops them too): the one outer-local-writes recognizer, with the locals
    // the cleanup reads, and the enclosing scope binds, as the bound-outside set.
    let read = analyze_block(cleanup).local_reads;
    let own = names_bound_by_cleanup_first(cleanup);
    let written: BTreeSet<EcoString> =
        outer_local_writes(protected, &|n| read.contains(n) && !own.contains(n))
            .into_iter()
            .map(|w| w.name)
            .collect();
    let first = written.iter().next()?;
    if let Some(cause) = nlr_cause(protected, sources) {
        return Some(EnsureDroppedLocal {
            name: first.to_string(),
            cause,
            exit: AbruptExit::Return,
        });
    }
    raise_after_write(protected, &written).map(|name| EnsureDroppedLocal {
        name: name.to_string(),
        cause: "may raise an error after writing it".to_string(),
        exit: AbruptExit::Error,
    })
}

/// BT-4003: the first local of `tracked` that `protected` writes before
/// something that may raise an error runs, in evaluation order, or `None`.
///
/// Conservative: every message send (binary operators included) may raise, and
/// so may a destructuring assignment, a `match:` and a string interpolation. A
/// send's receiver and non-block arguments run before it; the block literals a
/// local-threading construct runs in place ([`local_threading_construct`], the
/// one recognizer) run during it: the bodies of a loop or fold twice (a next
/// iteration runs after a write of this one), each arm of a conditional or
/// lookup from the state before it, and the blocks of the other constructs in
/// order. Any other block literal is a closure, whose body does not run here.
///
/// This over-approximates: anything after the write that may raise counts,
/// even inside an `on:do:` (in the protected block) that catches it, so
/// `[[t := 9. 1 / 0] on: Error do: [:e | 0]] ensure: [log record: t]` is
/// rejected though the cleanup would see 9. Documented in ADR 0133
/// § Consequences and the language reference rather than modelled.
fn raise_after_write(protected: &Block, tracked: &BTreeSet<EcoString>) -> Option<EcoString> {
    let mut scan = RaiseScan {
        tracked,
        shadowed: Vec::new(),
        written: None,
        found: None,
    };
    scan.block(protected);
    scan.found
}

/// The walk of [`raise_after_write`].
struct RaiseScan<'a> {
    tracked: &'a BTreeSet<EcoString>,
    /// The parameters (and pattern variables) of the enclosing in-place blocks
    /// and `match:` arms, which shadow a tracked local of the same name.
    shadowed: Vec<BTreeSet<EcoString>>,
    /// The first tracked local written so far on the path being walked.
    written: Option<EcoString>,
    /// The answer: the local written when something that may raise ran.
    found: Option<EcoString>,
}

impl RaiseScan<'_> {
    fn may_raise(&mut self) {
        if self.found.is_none() {
            self.found.clone_from(&self.written);
        }
    }

    fn write(&mut self, name: &EcoString) {
        if self.written.is_none()
            && self.tracked.contains(name)
            && !self.shadowed.iter().any(|s| s.contains(name))
        {
            self.written = Some(name.clone());
        }
    }

    fn block(&mut self, block: &Block) {
        self.shadowed
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
        self.shadowed.pop();
    }

    /// Each alternative from the state before them; afterwards a local is
    /// written when some alternative wrote it.
    fn alternatives(&mut self, count: usize, mut each: impl FnMut(&mut Self, usize)) {
        let start = self.written.clone();
        let mut after = start.clone();
        for i in 0..count {
            self.written.clone_from(&start);
            each(self, i);
            if after.is_none() {
                after.clone_from(&self.written);
            }
        }
        self.written = after;
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::Assignment { target, value, .. } => {
                self.expr(value);
                match target.as_ref() {
                    Expression::Identifier(id) => self.write(&id.name),
                    Expression::FieldAccess { receiver, .. } => self.expr(receiver),
                    _ => {}
                }
            }
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.expr(value);
                self.may_raise();
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.write(&id.name);
                }
            }
            Expression::Return { value, .. } => self.expr(value),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                let construct = local_threading_construct(expr)
                    .filter(LocalThreadingConstruct::is_threaded_today);
                let in_place = |e: &Expression| match (e, &construct) {
                    (Expression::Block(b), Some(c)) => {
                        c.blocks.iter().any(|blk| std::ptr::eq(*blk, b))
                    }
                    _ => false,
                };
                for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                    if !in_place(operand) {
                        self.expr(operand);
                    }
                }
                self.may_raise();
                let Some(c) = construct else {
                    return;
                };
                match c.family {
                    LocalThreadingFamily::Loop | LocalThreadingFamily::Fold => {
                        for _ in 0..2 {
                            for b in &c.blocks {
                                self.block(b);
                            }
                        }
                    }
                    LocalThreadingFamily::Conditional | LocalThreadingFamily::Lookup => {
                        self.alternatives(c.blocks.len(), |s, i| s.block(c.blocks[i]));
                    }
                    _ => {
                        for b in &c.blocks {
                            self.block(b);
                        }
                    }
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // No cascade message inlines its blocks (`generate_cascade`).
                self.expr(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.expr(arg);
                    }
                    self.may_raise();
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                self.may_raise();
                self.alternatives(arms.len(), |s, i| {
                    let arm = &arms[i];
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    s.shadowed.push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(guard) = &arm.guard {
                        s.expr(guard);
                    }
                    s.expr(&arm.body);
                    s.shadowed.pop();
                });
            }
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.expr(&pair.key);
                    self.expr(&pair.value);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements.iter().chain(tail.as_deref()) {
                    self.expr(e);
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    self.expr(e);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.expr(e);
                    }
                }
                self.may_raise();
            }
            Expression::Primitive { .. } => self.may_raise(),
            // A block literal here is a closure: creating it runs nothing.
            Expression::Block(_)
            | Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }
}

/// BT-3823: whether `expr` is an evaluated block in the REPL sense
/// ([`repl_evaluated_block`]: a `value`-family send of the block's own arity to
/// a bare block literal), whose block the REPL inlines over its bindings map
/// when it writes a binding: a REPL arm construct.
fn is_repl_evaluated_block(expr: &Expression) -> bool {
    repl_evaluated_block(expr).is_some()
}

fn param_names(method: &crate::ast::MethodDefinition) -> Vec<EcoString> {
    method
        .parameters
        .iter()
        .map(|p| p.name.name.clone())
        .collect()
}

/// A local bound (once) to a block literal that writes an outer local.
/// ADR 0133: how a block literal that assigns a captured local escapes, the
/// shape-specific part of the diagnostic (the ADR's error table).
#[derive(Debug, Clone, PartialEq, Eq)]
enum Escape {
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
    /// An argument or the receiver of a cascade message: a cascade sends every
    /// message through ordinary dispatch.
    Cascade(EcoString),
    /// The receiver of this selector, which does not run its receiver block in
    /// place.
    Receiver(EcoString),
    /// The condition of a `whileTrue:`/`whileFalse:` whose loop does not
    /// thread a condition write here (BT-3782).
    Condition(EcoString),
}

impl Escape {
    /// What follows "`x` is assigned in a block " in the message. `method` is
    /// the enclosing method's selector, `None` at the REPL root.
    fn fragment(&self, method: Option<&str>) -> String {
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
            Self::Cascade(selector) => format!(
                "sent in a cascade (`{selector}`), which the compiler does not thread it through"
            ),
            Self::Receiver(selector) => format!(
                "sent `{selector}`, which is not a construct the compiler threads it through"
            ),
            Self::Condition(selector) => format!(
                "used as the condition of `{selector}`, which does not thread an assignment in \
                 its condition here"
            ),
        }
    }

    /// The fix: the ADR's idiom for the shape.
    fn help(&self, name: &str) -> String {
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
            Self::Receiver(selector) => format!(
                "return the new value from the block and assign it where it is called: \
                 `{name} := [... {name} + 1] {selector}`"
            ),
            Self::Condition(selector) => format!(
                "assign `{name}` in the loop body instead: `[{name} < limit] {selector} [{name} := \
                 {name} + 1]`"
            ),
        }
    }
}

/// The ADR 0133 note on every escaping-block diagnostic: the rule, and the
/// messages that do thread assignments, generated from
/// [`crate::state_threading_selectors::assignment_threading_constructs`] (the
/// one list codegen reads), so the note cannot drift from what compiles.
fn threading_constructs_note() -> String {
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

/// The walk behind [`check_local_threading`] for one method body: tracks
/// scope (so a write can be told apart from a block-local definition) and the
/// block nesting depth (a construct inside a block is in a nested frame).
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

#[allow(clippy::struct_excessive_bools)] // independent one-shot walk flags, each documented
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
    /// The spans of the block literals that make up the value of some
    /// `x := ...` in the method ([`value_blocks`]: `b := [..]`, `b := c
    /// ifTrue: [[..]] ifFalse: [..]`): the blocks
    /// [`Walker::note_block_binding`] reports as stored (BT-3859).
    value_operands: HashSet<Span>,
    /// For each block in [`Walker::value_operands`], its first outer-local
    /// write, taken while the walk is at the block, with the scope it is
    /// written in (BT-3859): by the time the assignment's binding is noted,
    /// the frames of the conditional arms the value passes through have been
    /// popped, so a local first bound in an arm (`b := c ifTrue: [u := 0.
    /// [u := u + 1]] ...`) would look like the block's own.
    value_block_writes: HashMap<Span, OuterLocalWrite>,
    /// The block literals already reported as escaping (one diagnostic per
    /// block, whichever way it is first seen to escape).
    reported_blocks: HashSet<Span>,
    /// The escaping-block diagnostics, keyed by the assignment they report
    /// and the block that escapes, in walk order ([`Walker::report_escape`]).
    escaping_writes: Vec<(Span, Span, Diagnostic)>,
    /// The enclosing method's selector, for [`Escape::Returned`]; `None` at
    /// the REPL root.
    method_selector: Option<EcoString>,
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
    /// Whether the local being assigned by the assignment whose value is
    /// being walked was not bound before it ([`Residual::InlineBlockLocalInLoop`]).
    assign_target_fresh: bool,
    /// The in-place block sends already reported as
    /// [`Residual::InlineBlockLocalInLoop`].
    inline_local_reported: Vec<Span>,
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
    /// ADR 0131 §6 (BT-3756): the block literals of the method body that
    /// escape the method without a send — returned, stored in a field or
    /// class variable, or an element of a collection literal
    /// ([`escape_sites`], the classification behind `escaping_blocks`). Keyed
    /// by the operand's span.
    escapes: HashMap<Span, EscapeShape>,
    diagnostics: &'d mut Vec<Diagnostic>,
}

impl<'d> Walker<'d> {
    fn new(
        context: MethodContext,
        body: &[ExpressionStatement],
        diagnostics: &'d mut Vec<Diagnostic>,
    ) -> Self {
        let mut value_operands = HashSet::new();
        for stmt in body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, value, .. } = e {
                    if matches!(target.as_ref(), Expression::Identifier(_)) {
                        value_operands.extend(value_blocks(value).into_iter().map(|b| b.span));
                    }
                }
            });
        }
        Self {
            context,
            scope: Vec::new(),
            depth: 0,
            block_frames: Vec::new(),
            containers: Vec::new(),
            assign_target_fresh: false,
            inline_local_reported: Vec::new(),
            last_statements: Vec::new(),
            outer_stateful: false,
            direct_blocks: Vec::new(),
            value_operands,
            value_block_writes: HashMap::new(),
            reported_blocks: HashSet::new(),
            escaping_writes: Vec::new(),
            method_selector: None,
            operand_sites: HashSet::new(),
            frame_sites: Vec::new(),
            pending_frame_body: None,
            lookup_producers: HashSet::new(),
            receivers: None,
            escapes: escape_map(body),
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

    /// The first write of `block` that makes it a state-carrying block when it
    /// escapes (ADR 0133): a captured local, else a `self.field` assignment of
    /// a value type, named `self.field`. An actor field or a class variable is
    /// not one (ADR 0137): the block writes the cell, and a write from another
    /// process raises `actor_state_unreachable` at run time.
    fn escape_write_of(&self, block: &Block) -> Option<OuterLocalWrite> {
        self.writes_of(block).into_iter().next().or_else(|| {
            // A module-level expression has no `self`: "self can only be used
            // inside a method body" is already the error there.
            if matches!(self.context, Repl | Actor | Class) {
                return None;
            }
            // Where "Cannot assign to slot" fires (a `Value` subclass, not a
            // `TestCase`; `rejects_slot_assignment`, the validator's own
            // predicate) that error already covers the write, with the right
            // fix. A `TestCase` or plain `Object` subclass has no such error,
            // so the write would be dropped silently.
            if self.context == ValueType
                && self
                    .receivers
                    .as_ref()
                    .is_some_and(|r| r.hierarchy.rejects_slot_assignment(&r.class_name))
            {
                return None;
            }
            outer_state_writes(block).into_iter().next()
        })
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
            } else if let (Repl, Some(Some(frame))) = (self.context, self.frame_sites.last_mut()) {
                // BT-3823: a REPL arm's sites (codegen's
                // `with_repl_arm_producer_sites` reads the same rule).
                frame.sites = repl_arm_producer_sites(&stmt.expression)
                    .into_iter()
                    .map(Expression::span)
                    .collect();
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
            // ADR 0133: a block literal that is the value of a collection
            // operation's block (`collect: [:x | [t := t + x]]`) escapes into
            // the operation's result.
            if i + 1 == body.len() && self.containers.last() == Some(&Container::ListOpBody) {
                if let Expression::Block(block) = stmt.expression.unwrap_parens() {
                    self.report_literal(block, &Escape::CollectionResult);
                }
            }
            self.expr(&stmt.expression, Position::Statement);
        }
        self.last_statements.pop();
    }

    fn block(&mut self, block: &Block, container: Container) {
        if let Some(shape) = self.escapes.get(&block.span).copied() {
            self.report_escaping_literal(block, shape);
        }
        if self.value_operands.contains(&block.span) {
            if let Some(write) = self.escape_write_of(block) {
                self.value_block_writes.insert(block.span, write);
            }
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

    /// Leaves the innermost scope frame.
    fn pop_frame(&mut self) {
        self.scope.pop();
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

    fn expr(&mut self, expr: &Expression, position: Position) {
        self.expr_inner(expr, position);
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr_inner(&mut self, expr: &Expression, position: Position) {
        let nested = self.depth > 0;
        match expr {
            Expression::Parenthesized { expression, .. } => {
                self.expr(expression, position);
            }
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::Identifier(id) => {
                    let fresh = !self.is_bound(&id.name);
                    let outer = std::mem::replace(&mut self.assign_target_fresh, fresh);
                    self.operand(value, Position::AssignValue { nested });
                    self.assign_target_fresh = outer;
                    self.define(&id.name);
                    self.note_block_binding(&id.name, value);
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
                self.check_residuals(expr, position);
                self.check_construct(expr, position);
                let lookup_producer = self.lookup_producers.contains(&expr.unwrap_parens().span());
                let dispatched = self.is_actor_self_send_fold(expr);
                self.check_send(
                    receiver,
                    selector,
                    arguments,
                    true,
                    lookup_producer,
                    dispatched,
                );
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
                    // The receiver block of an `on:do:`/`ensure:` (the protected
                    // body) or of an evaluated literal block (`[...] value`, the
                    // arm that always runs) is an arm frame.
                    Expression::Block(_)
                        if arm_frames
                            && (exception_arm_blocks(expr).is_some()
                                || literal_block_value_arm(expr).is_some()
                                || (self.context == Repl && is_repl_evaluated_block(expr))) =>
                    {
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
                        self.report_literal(block, &Escape::Cascade(selector.name()));
                    }
                    self.check_send(shared, selector, arguments, false, false, false);
                    for msg in messages {
                        self.check_send(shared, &msg.selector, &msg.arguments, false, false, false);
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
                // BT-3947 (closes BT-3875): a `match:` that is a local-threading
                // producer here has an arm that is a `BranchArm` frame, like a
                // conditional's block (codegen's `generate_match_producer`): its
                // writes are threaded back and a loop or fold in an arm is a
                // producer there.
                if self.is_match_producer(expr, position) {
                    for arm in arms {
                        let block = match_arm_block(arm);
                        self.pending_frame_body = Some(FrameBody {
                            fold: false,
                            arm: true,
                        });
                        if let Some(guard) = &arm.guard {
                            self.operand(guard, Position::MatchArm);
                        }
                        self.block(&block, Container::Arm);
                        self.pending_frame_body = None;
                    }
                    return;
                }
                for arm in arms {
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    self.scope.push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(row) = residual_denied(Residual::MatchArmWrite, self.context) {
                        self.report_match_arm_writes(arm, row);
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
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
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
            // A local bound to a writing block was reported where it was bound
            // (`note_block_binding`), so a later escape of the local is not.
            Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }

    /// `name := value` (ADR 0133): each block literal that makes up `value`
    /// ([`value_blocks`]: `b := [..]`, `b := c ifTrue: [[..]] ifFalse: [..]`)
    /// and assigns a captured local escapes by being stored in `name`, however
    /// `name` is used later. Its write is the one seen where the block was
    /// walked (`value_block_writes`), else as seen here (`b := [b := b + 1]`
    /// writes the `b` this assignment binds). A later use of `name` (`^b`,
    /// `self foo: b`) is not reported again.
    fn note_block_binding(&mut self, name: &EcoString, value: &Expression) {
        for block in value_blocks(value) {
            let write = self
                .value_block_writes
                .get(&block.span)
                .cloned()
                .or_else(|| self.escape_write_of(block));
            if let Some(write) = write {
                self.report_escape(block, &write, &Escape::Stored(name.clone()), None);
            }
        }
    }

    /// Whether `expr` is a conditional or an `on:do:`/`ensure:` that is a
    /// local-threading producer at the method root (an operand site, an
    /// assignment's whole value or a method-body statement), whose arms are
    /// therefore `BranchArm` frames (ADR 0131 §2, Phase 3 and 3b). The REPL
    /// keeps its own lowering.
    fn is_arm_frame_conditional(&self, expr: &Expression, position: Position) -> bool {
        if self.context == Repl {
            return self.is_repl_arm_construct(expr, position);
        }
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

    /// BT-3947: whether `expr`, a `match:`, is a local-threading producer at
    /// `position` (the `match:` counterpart of [`Self::is_arm_frame_conditional`]):
    /// at the method root (an operand site or a statement) or at a site of the
    /// frame it is nested in. The REPL keeps its own lowering.
    fn is_match_producer(&self, expr: &Expression, position: Position) -> bool {
        if self.context == Repl {
            return self.is_repl_arm_construct(expr, position);
        }
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        if match_producer_writes(expr, &bound, kind).is_none() {
            return false;
        }
        if self.depth == 0 {
            return self.operand_sites.contains(&expr.span()) || position == Position::Statement;
        }
        self.arm_frame_conditional_producer(expr)
    }

    /// The [`RootMethodKind`] of the method being walked, `None` in the REPL.
    fn root_method_kind(&self) -> Option<RootMethodKind> {
        self.context.root_method_kind()
    }

    /// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): whether `expr` is a
    /// REPL construct whose blocks are *REPL arms*: a conditional (not a
    /// lookup), an `on:do:`/`ensure:`/`Result tryDo:` or a `value`-family send
    /// to a block literal ([`is_repl_evaluated_block`]) that writes a binding,
    /// so
    /// codegen inlines its blocks over the bindings map
    /// (`generate_repl_arm_inline`, `with_repl_arm_producer_sites`), at a
    /// position whose lowering compiles it that way: a statement, an
    /// assignment's whole value or an operand site of the input
    /// ([`repl_operand_sites`]), or a site of a REPL arm it is nested in
    /// ([`repl_arm_producer_sites`]).
    fn is_repl_arm_construct(&self, expr: &Expression, position: Position) -> bool {
        let inner = expr.unwrap_parens();
        if lookup_arm_blocks(inner).is_some() {
            return false;
        }
        // BT-3947: a `match:` whose arm bodies write a binding (no guard does)
        // is a REPL construct too, one arm per `match:` arm.
        let match_writes = match_arms(inner).map(|arms| {
            match_guard_writes(arms, &|name| self.is_bound(name)).is_empty()
                && !match_arm_body_writes(arms, &|name| self.is_bound(name)).is_empty()
        });
        if conditional_arm_blocks(inner).is_none()
            && exception_arm_blocks(inner).is_none()
            && !is_repl_evaluated_block(inner)
            && match_writes.is_none()
        {
            return false;
        }
        let writes = match_writes.unwrap_or_else(|| {
            local_threading_construct(inner).is_some_and(|c| {
                !construct_outer_local_writes(&c, &|name| self.is_bound(name)).is_empty()
            })
        });
        if !writes {
            return false;
        }
        if self.depth == 0 {
            return matches!(
                position,
                Position::Statement | Position::AssignValue { nested: false }
            ) || self.operand_sites.contains(&inner.span());
        }
        self.at_repl_arm_site(inner)
    }

    /// BT-3823: whether every enclosing block is a REPL arm and `expr` is one
    /// of the current arm statement's [`repl_arm_producer_sites`].
    fn at_repl_arm_site(&self, expr: &Expression) -> bool {
        self.context == Repl
            && !self.frame_sites.is_empty()
            && self
                .frame_sites
                .iter()
                .all(|frame| frame.as_ref().is_some_and(|f| f.arm))
            && self
                .frame_sites
                .last()
                .and_then(Option::as_ref)
                .is_some_and(|frame| frame.sites.contains(&expr.unwrap_parens().span()))
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
                || exception_producer_writes(expr, &bound, kind).is_some()
                || match_producer_writes(expr, &bound, kind).is_some())
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
        // BT-3922: in a class method, a lookup handler that writes object
        // state (a class variable) and no outer local writes it in place
        // (ADR 0130), so the §6 check must not take it for an escaping block
        // (`lookup_field_write_test.bt` proves the write survives). In an
        // actor or value-type method codegen runs the handler as a closure and
        // the field write is dropped, so it stays an escape.
        let state_only_lookup = is_lookup
            && self.context == Class
            && writes.is_empty()
            && construct
                .blocks
                .iter()
                .any(|block| !outer_state_writes(block).is_empty());
        let selector = construct.selector;
        // BT-3841: a keyed iteration on a statically non-`Dictionary` receiver
        // keeps the permanent §6 error whether its block writes an outer local
        // or, in an actor instance method, object state (a field write or a
        // self-send): the receiver's own method would run the block as a closure,
        // which can return neither. Only an actor method inlines a state-only
        // keyed block (`needs_mutation_threading` threads the actor `State`);
        // a class or value-type method dispatches it normally, so there the
        // block is accepted unless it writes an outer local.
        if matches!(selector.as_str(), "keysAndValuesDo:" | "doWithKey:")
            && self.receiver_is_not_dictionary(expr)
        {
            if let Some((_, body)) = producer_body_block(expr) {
                let hint = format!(
                    "`{selector}` runs the block inline only for a `Dictionary` receiver; this \
                     receiver is not one, so it would call its own `{selector}` with a closure. \
                     Compute the new value without writing it from the block, or iterate a \
                     `Dictionary` (`asDictionary`)"
                );
                if let Some(write) = writes.first() {
                    let escape = Escape::Passed(EcoString::from(selector.as_str()));
                    self.report_escape(body, write, &escape, Some(hint));
                } else if self.context == Actor && touches_state(body, false) {
                    self.report_state_literal(body, &selector, hint);
                }
            }
            return;
        }
        let Some(first) = writes.first().cloned() else {
            if state_only_lookup && !self.receiver_is_not_dictionary(expr) {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        };
        // ADR 0131 Phase 2d (BT-3798 review): a lookup whose receiver is
        // statically known not to be a `Dictionary` would only ever raise at
        // run time (the guard), so it keeps its §6 compile error.
        if is_lookup && self.receiver_is_not_dictionary(expr) {
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
            // only; the REPL keeps the §6 rejection (codegen's
            // `conditional_producer_locals` agrees).
            // The REPL's constructs at an operand site (`repl_operand_sites`)
            // or a REPL arm site (`repl_arm_producer_sites`) are producers that
            // adopt the bindings map whole (BT-3823: `repl_producer_writes`,
            // the rule codegen's REPL producer reads, a `value` send to a block
            // literal included).
            Repl => repl_producer_writes(expr, &bound),
            Actor => root_producer_writes(expr, &bound, RootMethodKind::Actor),
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
        // `detect:ifNone:` are producers there in class and value-type methods
        // but stay out in an actor method (they keep their statement rows), and an
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
        // BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): in a REPL arm — every
        // enclosing block an arm of a REPL construct inlined over the bindings
        // map (`is_repl_arm_construct`) — a construct at one of the statement's
        // `repl_arm_producer_sites` is a producer that adopts that map, as one
        // at an operand site of the input is.
        if self.depth > 0 && root_producer.is_some() && self.at_repl_arm_site(expr) {
            return;
        }
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
        // BT-3841 PR 5: the REPL has no measured rows for the detect search,
        // so a writing plain `detect:` that is not a producer here (one at a
        // REPL arm site is, BT-3823) stays a generic list op, which the REPL
        // rows deny.
        let kind = if self.context == Repl && selector == "detect:" {
            ListOp
        } else {
            kind
        };
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
        let ty = facts.type_map.and_then(|map| map.get(receiver.span()));
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

    /// Whether `expr` is a collection-message send (a `Fold`-family
    /// [`local_threading_construct`]: `do:`, `collect:`, `inject:into:`, ...)
    /// to `self` or `super` in an actor instance method or a class-side method
    /// (BT-3935: `self` is the class, whose own class-side `do:` runs in the
    /// class process and is reached only by a dispatched send). Codegen never lowers
    /// it in place: it is a dispatched self-send, so its block literal is
    /// passed to the actor's own method and escapes. Its writes were carried
    /// only by the callee's ADR 0128 opaque-callable fold, which ADR 0133
    /// removed (BT-3908).
    fn is_actor_self_send_fold(&self, expr: &Expression) -> bool {
        let Expression::MessageSend { receiver, .. } = expr.unwrap_parens() else {
            return false;
        };
        matches!(self.context, Actor | Class)
            && is_self_or_super(receiver)
            && local_threading_construct(expr)
                .is_some_and(|c| c.family == LocalThreadingFamily::Fold)
    }

    /// The ADR 0133 check for one send: its literal block arguments and
    /// receiver. `inlined` is false for a cascade message, which codegen never
    /// inlines; `dispatched` is true for an actor self-send of a collection
    /// message ([`Self::is_actor_self_send_fold`]), whose block escapes. A
    /// block passed to any other method escapes, an actor self-send to a
    /// `sealed` method of the same class included (ADR 0133 retires the
    /// BT-912/BT-3755 exemption).
    fn check_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        inlined: bool,
        lookup_producer: bool,
        dispatched: bool,
    ) {
        let sel = selector.name();
        if let Some(module) = crate::ffi_receiver::erlang_module_of_receiver(receiver) {
            // BT-3917: a block literal in an FFI argument position is handed to
            // Erlang, which calls it as a plain fun, so a write to a captured
            // local is dropped: it escapes (ADR 0133, ADR 0041).
            let escape = Escape::Ffi(EcoString::from(module), sel.clone());
            for arg in arguments {
                for block in value_blocks(arg) {
                    self.report_literal(block, &escape);
                }
            }
            return;
        }
        // EXEMPTION (§1 construct): a bare `[...]` argument of an inlined
        // send of a §1 selector (`local_threading_construct`).
        // (`tryDo:` is a §1 construct only sent to the `Result` class: any other
        // receiver takes the ordinary path.)
        // Not for an actor self-send of a collection message, which is
        // dispatched (`is_actor_self_send_fold`).
        let construct_send = inlined
            && !dispatched
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
                    self.report_literal(block, &Escape::Condition(sel.clone()));
                }
                // BT-3884: a block literal sent a `value`-family selector of
                // another arity (`[t := t + 1] value: 1`) is applied, never
                // inlined ([`block_literal_of_other_arity`]): the send raises
                // `arity_mismatch` and the body never runs, so the block
                // neither threads its writes nor escapes with them. A
                // parenthesized literal is the same literal (BT-3942).
                _ if block_literal_of_other_arity(receiver, selector, arguments.len())
                    .is_some() => {}
                Expression::Block(block) if !receiver_has_channel(&sel) => {
                    self.report_literal(block, &Escape::Receiver(sel.clone()));
                }
                Expression::Block(_) => {}
                // Any other receiver that evaluates to a block literal, a
                // parenthesized one or one answered by a conditional arm, an
                // evaluated block or a protected body ([`value_blocks`]), is a
                // block value sent `sel`.
                _ => {
                    for block in value_blocks(receiver) {
                        self.report_literal(block, &Escape::Receiver(sel.clone()));
                    }
                }
            }
        }
        // Block arguments. Only a bare `[...]` argument of a construct send (or a
        // lookup producer's handler) is lowered in place; any other argument that
        // evaluates to a block literal ([`value_blocks`]: the literal itself,
        // parenthesized, or answered by a conditional arm, an evaluated block or
        // a protected body) is a block value passed to `sel`, in every send.
        let escape = if dispatched {
            let word = if matches!(receiver.unwrap_parens(), Expression::Super(_)) {
                "super"
            } else {
                "self"
            };
            let owner = if self.context == Class {
                "class's"
            } else {
                "actor's"
            };
            Escape::ActorSelfSend(word, owner, sel.clone())
        } else if inlined {
            self.passed_escape(receiver, &sel)
        } else {
            Escape::Cascade(sel.clone())
        };
        for (index, arg) in arguments.iter().enumerate() {
            if matches!(arg, Expression::Block(_))
                && (construct_send || handler_indices.contains(&index))
            {
                continue;
            }
            for block in value_blocks(arg) {
                self.report_literal(block, &escape);
            }
        }
    }

    /// How a block argument of an ordinary send of `selector` to `receiver`
    /// escapes: to a class method that runs in the class's process (a class
    /// reference receiver, or `self` in a class method, unless the method is
    /// direct-called in the caller, ADR 0129 Phase 0b), or to any other method.
    fn passed_escape(&self, receiver: &Expression, selector: &EcoString) -> Escape {
        let receivers = self.receivers.as_ref();
        let class = match receiver {
            Expression::ClassReference { name, .. } => Some(name.name.clone()),
            Expression::Identifier(id) if id.name == "self" && self.context == Class => {
                receivers.map(|r| r.class_name.clone())
            }
            _ => None,
        };
        match class {
            Some(class)
                if !receivers
                    .is_some_and(|r| r.hierarchy.is_direct_call_eligible(&class, selector)) =>
            {
                Escape::ClassMethod(selector.clone())
            }
            _ if crate::state_threading_selectors::is_dictionary_inline_lookup(selector) => {
                Escape::Lookup(selector.clone())
            }
            _ => Escape::Passed(selector.clone()),
        }
    }

    /// The [`RESIDUAL_DENY_SET`] checks of a send.
    fn check_residuals(&mut self, expr: &Expression, position: Position) {
        // BT-3948's check first: a site it reports is not reported twice.
        self.check_inline_block_local_in_loop(expr, position);
        self.check_state_fold_operand_in_fold(expr, position);
    }

    /// [`Residual::StateFoldOperandInFold`] (BT-4003): see the shape's
    /// documentation for what was measured.
    fn check_state_fold_operand_in_fold(&mut self, expr: &Expression, position: Position) {
        let Some(row) = residual_denied(Residual::StateFoldOperandInFold, self.context) else {
            return;
        };
        // BT-4276: a loop or fold body counts only up to the nearest closure
        // boundary (`Container::Other`, a block value with a frame of its
        // own). A fold inside a closure that sits in a `do:` body is judged
        // by the closure's own context: the closure's body does not run per
        // iteration of the `do:`. Blocks run in place (arms, protected
        // bodies, handlers, evaluated blocks) are looked through.
        if !self
            .containers
            .iter()
            .rev()
            .take_while(|c| **c != Container::Other)
            .any(|c| {
                matches!(
                    c,
                    Container::DoBody
                        | Container::ListOpBody
                        | Container::LoopBody
                        | Container::CountedLoopBody
                )
            })
        {
            return;
        }
        let Some(construct) = local_threading_construct(expr) else {
            return;
        };
        if !matches!(
            construct.family,
            LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
        ) {
            return;
        }
        // One that writes an outer local is a local-threading producer, judged
        // (and measured right where accepted) by the Phase 0 check.
        if !construct_outer_local_writes(&construct, &|name| self.is_bound(name)).is_empty() {
            return;
        }
        let effects: Vec<_> = construct
            .blocks
            .iter()
            .flat_map(|b| &b.body)
            .map(|stmt| {
                crate::semantic_analysis::facts::compute_state_effects_for_expr(
                    &stmt.expression,
                    false,
                )
            })
            .collect();
        if !effects.iter().any(|e| e.any()) {
            return;
        }
        let writes_field = effects.iter().any(|e| e.field_write);
        let accepted = match position {
            Position::Statement | Position::AssignValue { .. } => true,
            Position::Receiver => {
                !writes_field && matches!(construct.selector.as_str(), "collect:" | "select:")
            }
            _ => false,
        };
        if accepted {
            return;
        }
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "a `{selector}` whose block sends to `self` or writes a field is {position} \
                     inside the body of another loop or collection operation, which is not \
                     supported yet in {context}: its state changes would be lost or its value \
                     would be wrong ({issue}, until {fixed_by})",
                    selector = construct.selector,
                    context = self.context,
                    issue = row.issue,
                    fixed_by = row.fixed_by,
                ),
                expr.span(),
            )
            .with_hint(
                "assign its value to a local in a statement of the body (`r := ...`) and use \
                 the local here",
            )
            .with_category(DiagnosticCategory::UnmigratedLocalThreading),
        );
    }

    /// [`Residual::InlineBlockLocalInLoop`] (BT-3948): see the shape's
    /// documentation for what was measured. The block writes no outer local
    /// (the Phase 0 check's business); it assigns a local of its own or one
    /// of its parameters, anywhere in its body. The shape is anchored on the
    /// innermost enclosing loop block.
    #[allow(clippy::too_many_lines)] // one measured rule per loop position, each documented
    fn check_inline_block_local_in_loop(&mut self, expr: &Expression, position: Position) {
        let Some(row) = residual_denied(Residual::InlineBlockLocalInLoop, self.context) else {
            return;
        };
        let Some(l) = self
            .containers
            .iter()
            .rposition(|c| matches!(c, Container::LoopBody | Container::CountedLoopBody))
        else {
            return;
        };
        let Some(construct) = local_threading_construct(expr)
            .filter(|c| c.family == LocalThreadingFamily::BlockValue)
        else {
            return;
        };
        let Some(block) = construct.blocks.first().copied() else {
            return;
        };
        if !self.writes_of(block).is_empty() {
            return;
        }
        let params: HashSet<&str> = block.parameters.iter().map(|p| p.name.as_str()).collect();
        // Every block literal below the applied block, and which of them are
        // the arms of a conditional send.
        let mut all_blocks: Vec<(Span, bool)> = Vec::new();
        let mut arm_blocks: Vec<Span> = Vec::new();
        for stmt in &block.body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
                Expression::Block(b) => all_blocks.push((b.span, false)),
                Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } if Container::of(&selector.name(), false) == Container::Arm => {
                    arm_blocks.extend(arguments.iter().filter_map(|a| match a {
                        Expression::Block(b) => Some(b.span),
                        _ => None,
                    }));
                }
                _ => {}
            });
        }
        for (span, is_arm) in &mut all_blocks {
            *is_arm = arm_blocks.contains(span);
        }
        let mut local: Option<(EcoString, Span)> = None;
        // Whether some assignment to a local is not inside only conditional
        // arms: the BT-3911 condition cell is positive, so a statement of
        // the block, an operand, or any `do:` body, closure, evaluated or
        // protected block between the assignment and the applied block
        // leaves it.
        let mut outside_arms = false;
        for stmt in &block.body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                let Expression::Assignment { target, .. } = e else {
                    return;
                };
                let Expression::Identifier(id) = target.as_ref() else {
                    return;
                };
                if !params.contains(id.name.as_str()) && self.is_bound(&id.name) {
                    return;
                }
                let span = e.span();
                let mut chain = all_blocks
                    .iter()
                    .filter(|(b, _)| b.start() <= span.start() && span.end() <= b.end())
                    .peekable();
                if chain.peek().is_none() || chain.any(|(_, is_arm)| !is_arm) {
                    outside_arms = true;
                }
                if local.is_none() {
                    local = Some((id.name.clone(), span));
                }
            });
        }
        let Some((name, span)) = local else {
            return;
        };
        // A nested in-place block inside one already reported adds nothing.
        if self
            .inline_local_reported
            .iter()
            .any(|r| r.start() <= expr.span().start() && expr.span().end() <= r.end())
        {
            return;
        }
        let below = &self.containers[l + 1..];
        // A loop condition is not a frame body (`frame_sites` is `None`).
        let in_condition = self.frame_sites.get(l).is_some_and(Option::is_none);
        let denied = if in_condition {
            // Right only for the BT-3911 cell: a statement of the condition
            // whose block is one statement with every assignment in an arm.
            !(l == 0
                && below.is_empty()
                && position == Position::Statement
                && block.body.len() == 1
                && !outside_arms)
        } else if l > 0 || !below.iter().all(|c| *c == Container::Arm) {
            // Inside another block, or a `do:`/`collect:`/`inject:` body
            // below the loop.
            true
        } else if below.is_empty() {
            self.direct_blocks.first() == Some(&true) && !self.outer_stateful
        } else {
            // Below arms of the body: right for `r := [..] value: i` of a
            // fresh local, and for a statement of an arm when the body
            // touches state.
            !((matches!(position, Position::AssignValue { .. }) && self.assign_target_fresh)
                || (self.outer_stateful && position == Position::Statement))
        };
        if !denied {
            return;
        }
        self.inline_local_reported.push(expr.span());
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "a block evaluated in place assigns its local `{name}` inside a loop, \
                     which an actor method does not lower yet ({issue}, until {fixed_by})",
                    issue = row.issue,
                    fixed_by = row.fixed_by,
                ),
                expr.span(),
            )
            .with_hint(
                "move the block's work into a method, or compute the value without \
                 assigning a local inside the block",
            )
            .with_note(format!("`{name}` is assigned here"), Some(span))
            .with_category(DiagnosticCategory::UnmigratedLocalThreading),
        );
    }

    /// A `match:` arm that assigns an outer local, in a `match:` that is not a
    /// local-threading producer where it sits ([`Walker::is_match_producer`]:
    /// BT-3947 lowers one that is, each arm a `BranchArm` frame). No construct
    /// threads such a write back to the method (BT-3817): compiling it would
    /// drop the write silently. The contexts are the
    /// [`Residual::MatchArmWrite`] row's: the REPL was exempt (its arm
    /// assignment drew only the value-position error, BT-3864) until BT-3909
    /// gave it the same error as a method, pinned in
    /// `adr0131_local_rebind.btscript`. Walks the whole
    /// arm (guard and body) without entering a nested block literal, whose
    /// writes are the constructs' own business; the arm's pattern variables
    /// (the innermost scope frame, already pushed) are the arm's own.
    fn report_match_arm_writes(&mut self, arm: &MatchArm, row: &ResidualDenied) {
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
                     or use `ifTrue:`/`ifNil:` for a branch that writes `{name}` ({issue}, until \
                     {fixed_by})",
                    issue = row.issue,
                    fixed_by = row.fixed_by,
                ))
                .with_category(DiagnosticCategory::UnmigratedLocalThreading),
            );
        }
    }

    /// Whether codegen threads an outer-local write in the condition block of
    /// a keyword `whileTrue:`/`whileFalse:` (BT-3782): the shared leaf
    /// [`while_condition_writes_thread`](crate::semantic_analysis::block_facts::while_condition_writes_thread),
    /// whose condition half is the predicate codegen's loop gates call
    /// (BT-3845). Every other condition is compiled as a Tier 2 closure that
    /// is called with the wrong arity, or as a plain block body whose write
    /// is never packed (the loop hangs); a top-level expression is left to
    /// the REPL's own handling.
    fn while_condition_threads(&self, cond: &Block, arguments: &[Expression]) -> bool {
        match self.context {
            Repl => true,
            Class | ValueType | Actor => {
                crate::semantic_analysis::block_facts::while_condition_writes_thread(
                    cond,
                    arguments.first(),
                    self.context == Class,
                )
            }
        }
    }

    /// Reports `block` as escaping (`escape`) if it assigns a captured local.
    fn report_literal(&mut self, block: &Block, escape: &Escape) {
        let Some(write) = self.escape_write_of(block) else {
            return;
        };
        self.report_escape(block, &write, escape, None);
    }

    /// The ADR 0133 diagnostic: `write` (the block's first assignment to a
    /// captured local) is reported at the assignment, with the shape, the fix
    /// (`hint`, else the shape's own) and the note listing the constructs that
    /// do thread assignments. One diagnostic per block literal.
    fn report_escape(
        &mut self,
        block: &Block,
        write: &OuterLocalWrite,
        escape: &Escape,
        hint: Option<String>,
    ) {
        if !self.reported_blocks.insert(block.span) {
            return;
        }
        // A `self.field` write (BT-3922) is named as written and has its own
        // fix: the field is object state, which a block that has left this
        // scope cannot hand back.
        let name: &EcoString = &match write.kind {
            WriteKind::Local => write.name.clone(),
            WriteKind::ObjectState => format!("self.{}", write.name).into(),
        };
        let hint = hint.or_else(|| {
            (write.kind == WriteKind::ObjectState).then(|| {
                format!(
                    "`{name}` is object state: once the block has left this scope no later call \
                     of it can hand the write back. Return the new value from the block and \
                     assign it where the block is called (`{name} := f value`), or move the \
                     write into a method that runs it directly"
                )
            })
        });
        let diagnostic = Diagnostic::error(
            format!(
                "`{name}` is assigned in a block {}",
                escape.fragment(self.method_selector.as_deref())
            ),
            write.span,
        )
        .with_hint(hint.unwrap_or_else(|| escape.help(name)))
        .with_note("the block", Some(block.span))
        .with_note(threading_constructs_note(), None)
        .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel);
        // One diagnostic per assignment: a write inside nested escaping blocks
        // (`f := [:d | d at: k ifAbsent: [t := t + 1]]`) is reported for the
        // outermost one, which is the escape to fix.
        self.push_escaping(write.span, block.span, diagnostic);
    }

    /// Records the escaping-block `diagnostic` found at `at` for `block`,
    /// keeping the outermost block's when nested blocks report the same site.
    fn push_escaping(&mut self, at: Span, block: Span, diagnostic: Diagnostic) {
        let within =
            |outer: Span, inner: Span| outer.start() <= inner.start() && inner.end() <= outer.end();
        match self
            .escaping_writes
            .iter_mut()
            .find(|(found, ..)| *found == at)
        {
            Some(entry) if within(block, entry.1) => {
                *entry = (at, block, diagnostic);
            }
            Some(_) => {}
            None => self.escaping_writes.push((at, block, diagnostic)),
        }
    }

    /// Hands the escaping-block diagnostics found by the walk
    /// ([`Walker::report_escape`]) to the caller.
    fn finish(self) {
        self.diagnostics
            .extend(self.escaping_writes.into_iter().map(|(.., d)| d));
    }

    /// A keyed iteration's block that writes object state (a field or a class
    /// variable) rather than a captured local, on a receiver that would run it
    /// as a closure (BT-3841).
    fn report_state_literal(&mut self, block: &Block, selector: &str, hint: String) {
        self.diagnostics.push(
            Diagnostic::error(
                format!("block writes object state, but `{selector}` cannot return the write"),
                block.span,
            )
            .with_hint(hint)
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    /// ADR 0131 §6 (BT-3756): a block literal that assigns a captured local
    /// and escapes the method without a send (returned, stored in a field or
    /// class variable, or placed in a literal). A block stored in a local is
    /// reported by [`Walker::note_block_binding`].
    fn report_escaping_literal(&mut self, block: &Block, shape: EscapeShape) {
        // A block literal assigned straight to a field of an instance is
        // already a compile error from the block analyzer (BT-2809, "cannot
        // mutate captured variable ... inside a closure stored in a field"),
        // which reports it once; `section6_field_stored_literal_is_an_error_in_every_context`
        // pins that some error survives. A class variable store has no such
        // error, so the class-method context reports it here.
        // A block that is (part of) the value of a local assignment is
        // reported as stored in that local, whatever else it does.
        if self.value_operands.contains(&block.span) {
            return;
        }
        let escape = match shape {
            EscapeShape::StoredLocal => return,
            // A literal that assigns a captured local is the block analyzer's
            // (BT-2809); one that only assigns `self.field` (BT-3922) is
            // reported here.
            EscapeShape::StoredClassVar
                if self.context != Class && !self.writes_of(block).is_empty() =>
            {
                return;
            }
            EscapeShape::StoredClassVar => Escape::StoredState,
            EscapeShape::Returned => Escape::Returned,
            EscapeShape::StoredInLiteral => Escape::InLiteral,
        };
        self.report_literal(block, &escape);
    }
}

/// The [`escape_sites`] of a method body (or REPL expression list, BT-3858:
/// the value of its last expression is the input's value) that ADR 0133
/// inspects (BT-3756).
///
/// An operand listed with several shapes (`^(b := [..])` is returned and
/// stored in a local, BT-3806) keeps the one that leaves the method.
fn escape_map(body: &[ExpressionStatement]) -> HashMap<Span, EscapeShape> {
    let mut escapes = HashMap::new();
    for (span, shape) in escape_sites(body) {
        let entry = escapes.entry(span).or_insert(shape);
        if *entry == EscapeShape::StoredLocal {
            *entry = shape;
        }
    }
    escapes
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
