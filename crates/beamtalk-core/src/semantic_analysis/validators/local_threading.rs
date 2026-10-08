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
//!   a literal block), an actor instance self-send (BT-912), or an Erlang FFI
//!   argument (ADR 0041 §Erlang Interop Boundary: lossy by design, codegen's
//!   `generate_erlang_interop_wrapper` warns). In an actor instance method a
//!   stored block may also be sent `value`, passed to a self-send or folded
//!   by a collection HOM (ADR 0128); everywhere else no callee can hand a
//!   `StateAcc` back.
//! - **The Phase 0 allow-set** ([`DiagnosticCategory::UnmigratedLocalThreading`],
//!   temporary). A local-threading construct
//!   ([`local_threading_construct_blocks`]) whose blocks write an outer local
//!   is accepted as a statement, and otherwise only in the
//!   `(construct, position, context)` combinations listed in [`ALLOW_SET`],
//!   which are the ones that answer right today. Anywhere else it is an error
//!   naming the construct, the position and BT-3743.

use crate::ast::{Block, Expression, ExpressionStatement, MessageSelector, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::{
    OuterLocalWrite, local_threading_construct_blocks, outer_local_writes,
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
    /// `whileTrue:`, `whileFalse:`, `timesRepeat:`, `to:do:`, `to:by:do:`,
    /// `repeat` and the unary loops.
    Loop,
    /// `do:`, `keysAndValuesDo:`, `doWithKey:`.
    Do,
    /// The value-returning list ops: `collect:`, `select:`, `reject:`,
    /// `inject:into:`, `detect:`, `count:`, … (and `detect:ifNone:` when only
    /// its search block writes).
    ListOp,
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
            "whileTrue:" | "whileFalse:" | "timesRepeat:" | "to:do:" | "to:by:do:"
            | "whileTrue" | "whileFalse" | "timesRepeat" | "repeat" => Self::Loop,
            "do:" | "keysAndValuesDo:" | "doWithKey:" => Self::Do,
            "anySatisfy:" | "allSatisfy:" => Self::Satisfy,
            "eachWithIndex:" | "do:separatedBy:" => Self::Enumeration,
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" => Self::IfTrue,
            "ifNil:" | "ifNotNil:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => Self::IfNil,
            "and:" | "or:" => Self::AndOr,
            "detect:ifNone:" if handler_writes => Self::DetectIfNone,
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
            Self::Do => "`do:` iteration",
            Self::ListOp => "collection operation",
            Self::Satisfy => "`anySatisfy:`/`allSatisfy:` test",
            Self::DetectIfNone => "`detect:ifNone:` handler",
            Self::Enumeration => "`eachWithIndex:`/`do:separatedBy:` iteration",
            Self::IfTrue => "conditional",
            Self::IfNil => "nil test",
            Self::AndOr => "`and:`/`or:`",
            Self::Exception => "exception handler",
            Self::BlockValue => "block evaluation",
        })
    }
}

/// Where a construct's value goes. `nested` is true inside any block body
/// (the construct's frame is then a block, not the method).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Position {
    /// An expression statement of a method or block body (including the last).
    Statement,
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
    AndOr, BlockValue, DetectIfNone, Do, Enumeration, Exception, IfNil, IfTrue, ListOp, Loop,
    Satisfy,
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
/// recorded on BT-3745. A statement is always accepted. Every combination
/// not listed here is a compile error ([`DiagnosticCategory::UnmigratedLocalThreading`]).
///
/// This is the one table: phases 2-4 of ADR 0131 (BT-3749, BT-3738, BT-3750)
/// grow it as each producer lands, and phase 5 (BT-3751) deletes the check.
/// Add a row only for a combination that answers right today.
const ALLOW_SET: &[Allowed] = &[
    // `r := <construct>` in the method body (s3, s5, and the per-context
    // passes of the ADR's second probe table).
    Allowed {
        kinds: &[Loop, ListOp, IfTrue, Exception],
        position: Position::AssignValue { nested: false },
        contexts: &[Class, ValueType],
    },
    Allowed {
        kinds: &[
            Loop,
            Do,
            ListOp,
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
            Do,
            ListOp,
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
        kinds: &[ListOp],
        position: Position::AssignValue { nested: true },
        contexts: &[Actor],
    },
    // `#(a, b) := <list op>` at the REPL (`destructuring.btscript`).
    Allowed {
        kinds: &[ListOp],
        position: Position::DestructureValue,
        contexts: &[Repl],
    },
    // `self.n := <construct>` in the method body (o5, o13).
    Allowed {
        kinds: &[
            Loop,
            Do,
            ListOp,
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
        kinds: &[Loop, Do, ListOp, IfTrue, Exception],
        position: Position::ReturnValue { nested: false },
        contexts: &[Class],
    },
    Allowed {
        kinds: &[Do],
        position: Position::ReturnValue { nested: false },
        contexts: &[ValueType],
    },
    Allowed {
        kinds: &[
            Loop,
            Do,
            ListOp,
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
        kinds: &[Do],
        position: Position::ReturnValue { nested: true },
        contexts: &[Class, ValueType],
    },
    Allowed {
        kinds: &[IfTrue, IfNil, AndOr],
        position: Position::ReturnValue { nested: true },
        contexts: &[Actor],
    },
];

/// Whether the allow-set accepts `kind` in `position` in `context`.
pub(crate) fn is_allowed(kind: ConstructKind, position: Position, context: MethodContext) -> bool {
    position == Position::Statement
        || ALLOW_SET.iter().any(|row| {
            row.position == position && row.kinds.contains(&kind) && row.contexts.contains(&context)
        })
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
    /// How many times each name is assigned anywhere in the method.
    assign_counts: HashMap<EcoString, usize>,
    tier2_locals: HashMap<EcoString, Tier2Local>,
    /// Tier 2 locals already reported (one diagnostic per binding).
    reported_locals: HashSet<EcoString>,
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
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, .. } = e {
                    if let Expression::Identifier(id) = target.as_ref() {
                        *assign_counts.entry(id.name.clone()).or_insert(0) += 1;
                    }
                }
            });
        }
        Self {
            context,
            scope: Vec::new(),
            depth: 0,
            block_frames: Vec::new(),
            assign_counts,
            tier2_locals: HashMap::new(),
            reported_locals: HashSet::new(),
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

    fn body(&mut self, body: &[ExpressionStatement]) {
        for stmt in body {
            self.expr(&stmt.expression, Position::Statement);
        }
    }

    fn block(&mut self, block: &Block) {
        self.block_frames.push(self.scope.len());
        self.scope
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        self.depth += 1;
        self.body(&block.body);
        self.depth -= 1;
        self.scope.pop();
        self.block_frames.pop();
    }

    /// Walks `expr` as an operand: a block literal is walked as a block, any
    /// other expression at `position`.
    fn operand(&mut self, expr: &Expression, position: Position) {
        match expr {
            Expression::Block(block) => self.block(block),
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
                    self.note_tier2_binding(&id.name, value, expr.span());
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
                self.check_send(receiver, selector, arguments);
                self.operand(receiver, Position::Receiver);
                for arg in arguments {
                    self.operand(arg, Position::Argument);
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
                    self.check_construct(receiver, Position::Cascade);
                    self.check_send(shared, selector, arguments);
                    for msg in messages {
                        self.check_send(shared, &msg.selector, &msg.arguments);
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
            Expression::Block(block) => self.block(block),
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
                    self.scope.pop();
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
    fn note_tier2_binding(&mut self, name: &EcoString, value: &Expression, binding: Span) {
        let Expression::Block(block) = value else {
            return;
        };
        if self.assign_counts.get(name).copied() != Some(1) {
            return;
        }
        let Some(frame) = self.binding_frame(name) else {
            return;
        };
        if let Some(write) = self.writes_of(block).into_iter().next() {
            self.tier2_locals.insert(
                name.clone(),
                Tier2Local {
                    binding,
                    write,
                    frame,
                },
            );
        }
    }

    /// The Phase 0 allow-set check for a construct at `position`.
    fn check_construct(&mut self, expr: &Expression, position: Position) {
        if position == Position::Statement {
            return;
        }
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
        let rhs_ok = is_allowed(kind, Position::AssignValue { nested: false }, self.context);
        let hint = if rhs_ok && !matches!(position, Position::AssignValue { .. }) {
            format!(
                "assign the `{selector}` result to a local in a statement of its own \
                 (`v := ...`) and use `v` here"
            )
        } else {
            format!(
                "evaluate the `{selector}` as a statement of its own, at the level of the \
                 method body, and read the locals it writes afterwards"
            )
        };
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

    /// The §6 check for one send: its literal block arguments and receiver,
    /// and any Tier 2 block-valued local it is sent to or passed.
    fn check_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) {
        let sel = selector.name().to_string();
        if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_some() {
            // ADR 0041 §Erlang Interop Boundary: lossy by design (warned by codegen).
            return;
        }
        let is_self_send = matches!(receiver, Expression::Super(_))
            || crate::semantic_analysis::block_facts::is_self_reference(receiver);
        let actor_self_send = self.context == Actor && is_self_send;
        let construct_send = is_section1_selector(&sel);

        // A block literal receiver.
        if let Expression::Block(block) = receiver {
            if !receiver_has_channel(&sel) {
                self.report_literal(block, &sel);
            }
        }
        // Block literal arguments.
        for arg in arguments {
            match arg {
                Expression::Block(block) if !construct_send && !actor_self_send => {
                    self.report_literal(block, &sel);
                }
                Expression::Parenthesized { .. } => {
                    if let Expression::Block(block) = arg.unwrap_parens() {
                        if !actor_self_send {
                            self.report_literal(block, &sel);
                        }
                    }
                }
                _ => {}
            }
        }

        // Tier 2 block-valued locals.
        if let Expression::Identifier(id) = receiver.unwrap_parens() {
            let ok = self.context == Actor && is_block_value_selector(selector);
            if !ok {
                self.report_local(&id.name, &sel, receiver.span());
            }
        }
        let fold_callable =
            crate::state_threading_selectors::opaque_fold_callable_arg(&sel, arguments);
        for arg in arguments {
            let Expression::Identifier(id) = arg.unwrap_parens() else {
                continue;
            };
            let ok = self.context == Actor
                && (is_self_send || fold_callable.is_some_and(|c| std::ptr::eq(c, arg)));
            if !ok {
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

fn is_block_value_selector(selector: &MessageSelector) -> bool {
    selector
        .well_known()
        .is_some_and(crate::ast::WellKnownSelector::is_block_value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allow_set_rows_are_non_empty_and_never_list_a_statement() {
        for row in ALLOW_SET {
            assert!(!row.kinds.is_empty() && !row.contexts.is_empty());
            assert_ne!(
                row.position,
                Position::Statement,
                "a statement is always accepted; the table only lists other positions"
            );
        }
    }

    #[test]
    fn statement_is_always_allowed() {
        for context in [Class, ValueType, Actor, Repl] {
            assert!(is_allowed(DetectIfNone, Position::Statement, context));
        }
    }
}
