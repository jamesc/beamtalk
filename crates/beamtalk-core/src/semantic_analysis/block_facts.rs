// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! This domain service analyzes blocks to detect which variables and fields are
//! read/written, enabling proper state threading in tail-recursive loops.

use crate::ast::well_known::WellKnownSelector;
use crate::ast::{
    Block, ClassDefinition, Expression, ExpressionStatement, MessageSelector, MethodDefinition,
    MethodKind, ParameterDefinition,
};
use crate::ast_walker::{
    SendSite, WalkEvent, cascade_sends, walk_expression_and_sends, walk_sends,
};
use crate::semantic_analysis::type_checker::InferredType;
use crate::source_analysis::Span;
use crate::state_threading_selectors::{is_opaque_callable_hom_send, opaque_fold_callable_arg};
use ecow::EcoString;
use std::collections::HashSet;

/// Analysis results for a block's variable and field usage.
#[derive(Debug, Clone, Default)]
pub struct BlockMutationAnalysis {
    /// Local variables that are read in the block.
    pub local_reads: HashSet<String>,
    /// Local variables that are written to in the block.
    pub local_writes: HashSet<String>,
    /// Variables read before being locally defined (captured from outer scope).
    pub captured_reads: HashSet<String>,
    /// Fields (self.field) that are read in the block.
    pub field_reads: HashSet<String>,
    /// Fields (self.field) that are written to in the block.
    pub field_writes: HashSet<String>,
    /// Whether the block contains self-sends (which may mutate actor state).
    pub has_self_sends: bool,
    /// ADR 0137 (BT-4020): whether the block contains a send to something
    /// other than `self` that may run user code in the calling process
    /// ([`send_user_code`], and a binary operator that is not a bare BIF or a
    /// numeric-literal operation), nested blocks included. Counted as a state
    /// effect only while the synced-calls switch is on
    /// ([`crate::synced_calls`]), because such a call is bracketed with a
    /// store and a reload of the threaded `State`.
    pub has_user_code_sends: bool,
    /// Selectors sent to `self` anywhere in the block, including inside
    /// nested blocks (e.g. a `do:`/`collect:` argument) — unlike `has_self_sends`,
    /// tracked by name so a caller can distinguish a self-send to a provably
    /// non-mutating class method from one that is (or might be) mutating.
    pub self_send_selectors: HashSet<String>,
}

impl BlockMutationAnalysis {
    /// Creates a new empty analysis.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns true if the block has any mutations (local or field).
    pub fn has_mutations(&self) -> bool {
        !self.local_writes.is_empty() || !self.field_writes.is_empty()
    }

    /// Returns true if the block has any state-affecting operations.
    /// This includes field writes and self-sends (which may mutate actor
    /// state). A `self.field value(:...)` send is not one: a block stored in a
    /// field cannot write a field or a captured local (ADR 0133, BT-3922).
    pub fn has_state_effects(&self) -> bool {
        !self.field_writes.is_empty() || self.has_self_sends
    }

    /// ADR 0137: whether the block holds a call that is bracketed with a
    /// store/reload of `State` in an actor instance method (the switch is on
    /// and the block has a send that may run user code). It threads `State`
    /// like a state effect does, and cannot keep its fields in pre-extracted
    /// loop parameters (hybrid mode): the cell must hold the whole `State`
    /// map at the call. Deliberately not part of [`Self::has_state_effects`]:
    /// that one is context-free, and a value-type, class-side or REPL block
    /// has no actor `State` to bracket (the caller knows the context).
    #[must_use]
    pub fn has_synced_user_code_sends(&self) -> bool {
        self.has_user_code_sends && crate::synced_calls::enabled()
    }

    /// Whether a loop or fold over this block is lowered with mutation
    /// threading, in a method of kind `root` (`None`: a top-level expression,
    /// the REPL or a script). The one per-context rule: codegen's
    /// `needs_mutation_threading` and the ADR 0133 `groupBy:` residual check
    /// (BT-3909, `validators/local_threading.rs`) both call it.
    ///
    /// - REPL: any local or field write (the bindings map carries both).
    /// - Class method: no `State`, so field writes and self-sends never
    ///   thread; a captured local both read and written does, through a
    ///   fresh local map.
    /// - Actor instance method: field writes, self-sends and other state
    ///   effects ([`Self::has_state_effects`]) when `State` is an eligible
    ///   family (`actor_state_eligible`, always true in an actor instance
    ///   method, ADR 0122), or any local write.
    /// - Value-type method: a field write, or a captured local both read and
    ///   written.
    #[must_use]
    pub fn needs_mutation_threading(
        &self,
        root: Option<RootMethodKind>,
        actor_state_eligible: bool,
    ) -> bool {
        self.needs_mutation_threading_with(
            root,
            actor_state_eligible,
            self.has_synced_user_code_sends(),
        )
    }

    /// [`Self::needs_mutation_threading`] with the ADR 0137 bracket decided by
    /// the caller (`synced`: this block holds a call bracketed with a
    /// `State` store/reload where it is compiled). Codegen passes its own
    /// context-aware answer (`synced_calls_active`); the context-free default
    /// above reads only the global switch.
    #[must_use]
    pub fn needs_mutation_threading_with(
        &self,
        root: Option<RootMethodKind>,
        actor_state_eligible: bool,
        synced: bool,
    ) -> bool {
        let captured_local_mutation = || {
            self.captured_reads
                .iter()
                .any(|v| self.local_writes.contains(v))
        };
        match root {
            None => self.has_mutations(),
            Some(RootMethodKind::Class) => captured_local_mutation(),
            Some(RootMethodKind::Actor) => {
                (actor_state_eligible && (self.has_state_effects() || synced))
                    || !self.local_writes.is_empty()
            }
            Some(RootMethodKind::ValueType) => {
                !self.field_writes.is_empty() || captured_local_mutation()
            }
        }
    }

    /// Returns all variables that need threading (read AND written).
    #[cfg(test)]
    pub fn threaded_vars(&self) -> HashSet<String> {
        self.local_reads
            .intersection(&self.local_writes)
            .cloned()
            .collect()
    }
}

/// Analyzes a block to detect variable and field mutations.
pub fn analyze_block(block: &Block) -> BlockMutationAnalysis {
    let mut ctx = AnalysisContext::new();
    for param in &block.parameters {
        ctx.local_bindings.insert(param.name.to_string());
    }
    analyze_statements(&block.body, &mut ctx)
}

/// ADR 0133 Phase 3 (BT-3909): whether a `groupBy:` with the literal block
/// `block`, which writes no local bound outside it, is lowered as the threaded
/// fold (`{GroupMap, ...}` accumulator) in a method of kind `root` (`None`:
/// the REPL). Codegen's fold gate for such a block:
/// [`BlockMutationAnalysis::needs_mutation_threading`] (`State` always
/// eligible in an actor instance method) and, at the REPL, a binding written
/// anywhere through nested constructs; the gate's other disjuncts are
/// outer-local writes, which the Phase 0 check judges. Read by the
/// `GroupByValue` residual check; `group_by_fold_gate_agreement` in
/// `beamtalk-codegen` compiles a corpus of blocks and asserts codegen takes
/// the fold exactly when this says so.
#[must_use]
pub fn group_by_fold_threads(block: &Block, root: Option<RootMethodKind>) -> bool {
    inline_block_threads(block, root)
}

/// BT-3981 (ADR 0133 Phase 3): whether a block literal that writes no local
/// bound outside it, inlined where it is sent (`groupBy:`'s fold, an evaluated
/// `[...] value`), is lowered as a state-threading block in a method of kind
/// `root` (`None`: the REPL): the block yields a `{Value, State}` pair that
/// the site must unpack. The one gate `group_by_fold_threads` and the
/// `EvaluatedBlockValue` residual check read; codegen's
/// `needs_mutation_threading` (with `State` always eligible in an actor
/// instance method) is the rule.
#[must_use]
pub fn inline_block_threads(block: &Block, root: Option<RootMethodKind>) -> bool {
    analyze_block(block).needs_mutation_threading(root, true)
        // The REPL also threads a block that writes a binding anywhere, a
        // nested construct's block included (codegen's
        // `repl_blocks_write_bindings`, BT-3823): every name is a binding
        // there.
        || (root.is_none() && !threaded_block_writes(&[block], &|_| true).is_empty())
}

/// Analyzes a method body (top-level statements, not wrapped in a
/// `Block`) the same way [`analyze_block`] analyzes a block body — used by
/// the class-var-mutating-selector purity check (`compute_class_var_mutating_selectors`)
/// to inspect each class method's own body directly, since `MethodDefinition`
/// isn't a `Block`.
pub fn analyze_method_body(
    parameters: &[ParameterDefinition],
    body: &[ExpressionStatement],
) -> BlockMutationAnalysis {
    let mut ctx = AnalysisContext::new();
    for param in parameters {
        ctx.local_bindings.insert(param.name.name.to_string());
    }
    analyze_statements(body, &mut ctx)
}

/// Computes the set of this class's own class-method selectors that
/// are *known or suspected* to mutate a class variable — directly (`self.cv
/// := ...` for `cv` in `class_var_names`) or transitively (a same-class send
/// — `self foo` OR `ClassName foo`, anywhere in the method body including
/// inside nested blocks — to another selector already in this set).
///
/// The transitive closure walks [`BlockMutationAnalysis::self_send_selectors`]
/// (a `self foo` send) unioned with [`same_class_reference_send_selectors`]
/// (a `ClassName foo` send to the class's own name, which binds the same
/// method), so a mutation reached only through the `ClassName`-spelled call is
/// not mistaken for pure.
///
/// A same-class send to a selector NOT defined in this class's own
/// `class_methods` (inherited, or otherwise unresolvable at this class's
/// compile time) is conservatively treated as mutating: a selector is only
/// excluded from the mutating set when its target is a *locally defined*
/// method that this same pass has proven pure.
///
/// Consumed by the `class-state-abroad` lint (`validators/class_state_abroad.rs`,
/// `may_write_class_var`) to judge whether a `self`/own-class send inside a
/// block may write a class variable. The analysis is syntactic and whole-class
/// (it needs the class's own call graph for the fixed point) and lives in
/// `beamtalk-core`, which cannot depend on `beamtalk-codegen`.
#[allow(clippy::implicit_hasher)] // concrete HashSet (matches ClassContext::class_var_names) is simpler for callers
pub fn compute_class_var_mutating_selectors(
    class: &ClassDefinition,
    class_var_names: &HashSet<String>,
) -> HashSet<String> {
    let class_name = class.name.name.as_str();
    let methods: Vec<(String, BlockMutationAnalysis, HashSet<String>)> = class
        .class_methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .map(|m| {
            let analysis = analyze_method_body(&m.parameters, &m.body);
            let mut same_class_call_targets = analysis.self_send_selectors.clone();
            same_class_call_targets
                .extend(same_class_reference_send_selectors(&m.body, class_name));
            (
                m.selector.name().to_string(),
                analysis,
                same_class_call_targets,
            )
        })
        .collect();
    let local_selectors: HashSet<&str> = methods.iter().map(|(sel, _, _)| sel.as_str()).collect();

    let mut mutating: HashSet<String> = methods
        .iter()
        .filter(|(_, analysis, _)| {
            analysis
                .field_writes
                .iter()
                .any(|f| class_var_names.contains(f))
        })
        .map(|(sel, _, _)| sel.clone())
        .collect();

    // Fixed-point closure over same-class sends: a method becomes "mutating"
    // if it same-class-sends (`self foo` or `ClassName foo`) a selector
    // already known to mutate, or one this class doesn't itself define
    // (unresolvable — assume the worst).
    loop {
        let mut changed = false;
        for (sel, _, same_class_call_targets) in &methods {
            if mutating.contains(sel) {
                continue;
            }
            let calls_unsafe = same_class_call_targets.iter().any(|called| {
                mutating.contains(called) || !local_selectors.contains(called.as_str())
            });
            if calls_unsafe {
                mutating.insert(sel.clone());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    mutating
}

/// Whether `receiver` is the unqualified class reference `class_name`: the
/// one own-class-receiver shape the class-side rules share (here, and
/// codegen's `expr_shape::is_own_class_reference`, BT-4005).
#[must_use]
pub fn is_own_class_reference(receiver: &Expression, class_name: &str) -> bool {
    matches!(
        receiver,
        Expression::ClassReference { name, package, .. }
            if package.is_none() && name.name == class_name
    )
}

/// Selectors sent via a same-class `ClassName selector` receiver (as opposed
/// to `self selector`) anywhere in `body`, including nested blocks — the
/// [`Expression::ClassReference`] counterpart to [`is_self_reference`]-based
/// `self_send_selectors` tracking, unioned in by
/// [`compute_class_var_mutating_selectors`].
fn same_class_reference_send_selectors(
    body: &[ExpressionStatement],
    class_name: &str,
) -> HashSet<String> {
    let mut selectors = HashSet::new();
    for stmt in body {
        // Cascades expanded: `Counter log; bump` sends both (BT-3716).
        walk_sends(&stmt.expression, &mut |send| {
            if is_own_class_reference(send.receiver, class_name) {
                selectors.insert(send.selector.name().to_string());
            }
        });
    }
    selectors
}

/// How a block literal leaves the class-method invocation that created it
/// (ADR 0130 §5): the shapes the Phase 0 census (BT-3703) counts and the
/// `class-state-abroad` lint (BT-3712) warns on. One definition, shared by
/// both, so they cannot disagree about what "escaping" means.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EscapeShape {
    /// Returned: the method's last statement, or the operand of `^`.
    Returned,
    /// Assigned to a local variable (`b := [...]`).
    StoredLocal,
    /// Assigned to a class variable (`self.cb := [...]`).
    StoredClassVar,
    /// An element of a list, array or map literal.
    StoredInLiteral,
}

/// The block literal an expression is, looking through parentheses.
fn as_block(expr: &Expression) -> Option<&Block> {
    match expr.unwrap_parens() {
        Expression::Block(block) => Some(block),
        _ => None,
    }
}

/// How far [`for_each_escape_operand`] follows a returned operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReturnedOperands {
    /// Only the method's last statement and the operand of each `^`, as
    /// written: what the class-state-abroad lint and the Phase 0 census have
    /// counted since BT-3703 ([`escaping_blocks`]).
    Direct,
    /// As [`Self::Direct`], and also through what makes up such an operand's
    /// value (BT-3806): the value of a local assignment (`b := [..]` as the
    /// last statement, `^(b := [..])`), the last statement of each arm of an
    /// inlined conditional, `[..] value`, `on:do:` or `ensure:` body
    /// ([`value_arm_blocks`]) and the body of each `match:` arm, repeatedly.
    /// What the ADR 0131 §6 check reads ([`escape_sites`]).
    ThroughValue,
}

/// The literal blocks whose last statement's value is the value of `expr`
/// when it runs: the arms of a conditional, a lookup handler or an evaluated
/// literal block ([`conditional_arm_blocks`]), and the protected body and
/// handler of an `on:do:` or the protected body of an `ensure:`
/// ([`exception_arm_blocks`]; a `tryDo:` answers a `Result`, not its block's
/// value). Empty for any other expression.
fn value_arm_blocks(expr: &Expression) -> Vec<&Block> {
    if let Some(arms) = conditional_arm_blocks(expr) {
        return arms;
    }
    let Some(construct) = local_threading_construct(expr) else {
        return Vec::new();
    };
    if construct.family != LocalThreadingFamily::Exception {
        return Vec::new();
    }
    let mut arms = exception_arm_blocks(expr).unwrap_or_default();
    if construct.selector == "ensure:" {
        // `ensure:` answers its protected body's value, not the cleanup's.
        arms.truncate(1);
    }
    arms
}

/// Calls `visit` with `operand`, a returned operand, and (for
/// [`ReturnedOperands::ThroughValue`]) with each expression whose value it
/// answers, all as [`EscapeShape::Returned`].
fn visit_returned<'a>(
    operand: &'a Expression,
    returns: ReturnedOperands,
    visit: &mut dyn FnMut(&'a Expression, EscapeShape),
) {
    let operand = operand.unwrap_parens();
    visit(operand, EscapeShape::Returned);
    if returns == ReturnedOperands::Direct {
        return;
    }
    match operand {
        Expression::Assignment { target, value, .. }
            if matches!(**target, Expression::Identifier(_)) =>
        {
            visit_returned(value, returns, visit);
        }
        Expression::Match { arms, .. } => {
            for arm in arms {
                visit_returned(&arm.body, returns, visit);
            }
        }
        _ => {
            for block in value_arm_blocks(operand) {
                if let Some(last) = block.body.last() {
                    visit_returned(&last.expression, returns, visit);
                }
            }
        }
    }
}

/// The block literals `expr` may evaluate to, through what makes up its value
/// as [`ReturnedOperands::ThroughValue`] follows it (`[..]`,
/// `c ifTrue: [[..]] ifFalse: [[..]]`, `b := [..]`, `[[..]] value`): the
/// blocks [`escape_sites`] would report for a returned `expr`. ADR 0131 §6
/// reads it for a local bound to such a value (BT-3806).
#[must_use]
pub fn value_blocks(expr: &Expression) -> Vec<&Block> {
    let mut found = Vec::new();
    visit_returned(expr, ReturnedOperands::ThroughValue, &mut |operand, _| {
        if let Expression::Block(block) = operand {
            found.push(block);
        }
    });
    found
}

/// The bare local-variable references `expr` may evaluate to, followed as
/// [`value_blocks`] follows it (`c := b`, `c := x ifTrue: [b] ifFalse: [..]`):
/// ADR 0131 §6 reads it for a local that aliases a block-valued local
/// (BT-3859).
#[must_use]
pub fn value_locals(expr: &Expression) -> Vec<&crate::ast::Identifier> {
    let mut found = Vec::new();
    visit_returned(expr, ReturnedOperands::ThroughValue, &mut |operand, _| {
        if let Expression::Identifier(id) = operand {
            found.push(id);
        }
    });
    found
}

/// Calls `visit` with every operand of a method body (nested blocks included)
/// that is returned or stored, and how it escapes: the one classification
/// behind [`escaping_blocks`] and [`escape_sites`], so they cannot disagree
/// about what an escape site is. Purely syntactic: an operand passed as a
/// message argument is not an escape here (whether it escapes depends on the
/// callee). The body's last statement is a returned operand. `returns` says
/// how far a returned operand is followed ([`ReturnedOperands`]). An operand
/// may be visited more than once, with different shapes (`^(b := [..])` is
/// both returned and stored in a local under
/// [`ReturnedOperands::ThroughValue`]).
fn for_each_escape_operand(
    body: &[ExpressionStatement],
    returns: ReturnedOperands,
    visit: &mut dyn FnMut(&Expression, EscapeShape),
) {
    if let Some(last) = body.last() {
        visit_returned(&last.expression, returns, visit);
    }
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::Return { value, .. } => visit_returned(value, returns, visit),
            Expression::Assignment { target, value, .. } => {
                let shape = if matches!(**target, Expression::FieldAccess { .. }) {
                    EscapeShape::StoredClassVar
                } else {
                    EscapeShape::StoredLocal
                };
                visit(value.unwrap_parens(), shape);
            }
            Expression::ListLiteral { elements, .. }
            | Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for element in elements {
                    visit(element.unwrap_parens(), EscapeShape::StoredInLiteral);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    visit(pair.value.unwrap_parens(), EscapeShape::StoredInLiteral);
                }
            }
            _ => {}
        });
    }
}

/// Every block literal in a method body (nested blocks included) that is
/// returned or stored, with how it escapes (cloned: the walker's borrows do
/// not outlive its callback). See [`for_each_escape_operand`] for what counts;
/// a returned operand is taken as written ([`ReturnedOperands::Direct`]), so a
/// block that is only the value of a conditional arm or of a last-statement
/// `b := [..]` is not one here (the census and the class-state-abroad lint
/// keep the counts they were measured with; the ADR 0131 §6 check follows
/// both, [`escape_sites`]).
pub fn escaping_blocks(body: &[ExpressionStatement]) -> Vec<(Block, EscapeShape)> {
    let mut found = Vec::new();
    for_each_escape_operand(body, ReturnedOperands::Direct, &mut |operand, shape| {
        if let Some(block) = as_block(operand) {
            found.push((block.clone(), shape));
        }
    });
    found
}

/// The spans of the block literals and bare local-variable references in a
/// method body that are themselves a returned or stored operand (`^[..]`,
/// `self.f := b`, `#[b]`), with how they escape: the same classification as
/// [`escaping_blocks`], the body's last statement included, with a returned
/// operand also followed through what makes up its value
/// ([`ReturnedOperands::ThroughValue`], BT-3806: `c ifTrue: [[..]]`,
/// `b := [..]` as the last statement, `^(b := [..])`). A local stored into
/// another local is reported as [`EscapeShape::StoredLocal`]. A span may
/// appear more than once with different shapes (`^(b := [..])`: the block is
/// both returned and stored in a local).
#[must_use]
pub fn escape_sites(body: &[ExpressionStatement]) -> Vec<(Span, EscapeShape)> {
    let mut found = Vec::new();
    for_each_escape_operand(
        body,
        ReturnedOperands::ThroughValue,
        &mut |operand, shape| match operand {
            Expression::Block(block) => found.push((block.span, shape)),
            Expression::Identifier(id) => found.push((id.span, shape)),
            _ => {}
        },
    );
    found
}

/// How a block literal written in a class method touches its home class's
/// variables (ADR 0130 §5), nested blocks included: the one answer behind
/// codegen's block-creation capture (`block_reads_class_var`), the
/// `class-state-abroad` lint and the Phase 0 census (BT-3761).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassVarAccesses {
    /// Class variables read as `self.name`, sorted. The target of a
    /// `self.name := v` write is not a read.
    pub reads: std::collections::BTreeSet<String>,
    /// Class variables written as `self.name := v`, sorted, each with the
    /// span of its first write in source order.
    pub writes: std::collections::BTreeMap<String, Span>,
    /// `self hasField: x` probes, which ask the class-variable home (they
    /// lower to `beamtalk_class_vars:has`, which reads through a capture
    /// abroad). A probe is not a class-variable read: its argument may name no
    /// declared variable, or be computed. A `hasField:` that is a cascade
    /// message is not a probe: a cascade dispatches every message, the first
    /// included, as an ordinary send, never through the `HasField` intrinsic.
    pub has_field_probes: std::collections::BTreeSet<HasFieldProbe>,
}

/// The argument of a `self hasField:` probe ([`ClassVarAccesses::has_field_probes`]).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HasFieldProbe {
    /// A symbol literal, `self hasField: #n`: the probed name, declared or not.
    Symbol(String),
    /// Any other argument, `self hasField: k`.
    Computed,
}

impl ClassVarAccesses {
    /// Whether the block reads class state at all: a `self.name` read or a
    /// `hasField:` probe. Exactly the blocks for which codegen binds a capture
    /// (outside a direct-called method) and the lint's read rule considers.
    #[must_use]
    pub fn reads_class_state(&self) -> bool {
        !self.reads.is_empty() || !self.has_field_probes.is_empty()
    }
}

/// THE walker for "which class variables does `block` read and write"
/// (BT-3761): see [`ClassVarAccesses`]. `vars` are the class variables
/// visible to the class (own and inherited); a `self.name` for any other
/// `name` is an instance-field access and not counted.
#[allow(clippy::implicit_hasher)] // concrete HashSet, matching ClassContext-style callers
#[must_use]
pub fn class_var_accesses(block: &Block, vars: &HashSet<String>) -> ClassVarAccesses {
    let mut accesses = ClassVarAccesses::default();
    // Write targets, by identity: the walk visits an assignment before its target.
    let mut written_targets: HashSet<*const Expression> = HashSet::new();
    for stmt in &block.body {
        walk_expression_and_sends(&stmt.expression, &mut |event| match event {
            WalkEvent::Expr(Expression::Assignment { target, .. }) => {
                if let Expression::FieldAccess {
                    receiver, field, ..
                } = target.as_ref()
                {
                    if is_self_reference(receiver) && vars.contains(field.name.as_str()) {
                        accesses
                            .writes
                            .entry(field.name.to_string())
                            .or_insert_with(|| target.span());
                        written_targets.insert(std::ptr::from_ref::<Expression>(target));
                    }
                }
            }
            WalkEvent::Expr(
                e @ Expression::FieldAccess {
                    receiver, field, ..
                },
            ) => {
                if is_self_reference(receiver)
                    && vars.contains(field.name.as_str())
                    && !written_targets.contains(&std::ptr::from_ref(e))
                {
                    accesses.reads.insert(field.name.to_string());
                }
            }
            WalkEvent::Send(send)
                if send.site == SendSite::Plain
                    && is_self_reference(send.receiver)
                    && send.selector.well_known() == Some(WellKnownSelector::HasField) =>
            {
                let probed = match send.arguments.first() {
                    Some(Expression::Literal(crate::ast::Literal::Symbol(name), _)) => {
                        HasFieldProbe::Symbol(name.to_string())
                    }
                    _ => HasFieldProbe::Computed,
                };
                accesses.has_field_probes.insert(probed);
            }
            _ => {}
        });
    }
    accesses
}

/// Shared statement-list walker behind [`analyze_block`] and [`analyze_method_body`].
fn analyze_statements(
    body: &[ExpressionStatement],
    ctx: &mut AnalysisContext,
) -> BlockMutationAnalysis {
    let mut analysis = BlockMutationAnalysis::new();
    for stmt in body {
        analyze_expression(&stmt.expression, &mut analysis, ctx);
    }
    analysis
}

/// Context tracking for analysis traversal.
struct AnalysisContext {
    /// Local variables bound in the current scope (block params, let bindings).
    local_bindings: HashSet<String>,
}

impl AnalysisContext {
    fn new() -> Self {
        Self {
            local_bindings: HashSet::new(),
        }
    }
}

/// Recursively analyzes an expression for variable/field access.
#[allow(clippy::too_many_lines)] // Analysis needs comprehensive pattern matching
fn analyze_expression(
    expr: &Expression,
    analysis: &mut BlockMutationAnalysis,
    ctx: &mut AnalysisContext,
) {
    match expr {
        Expression::Literal(..)
        | Expression::Error { .. }
        | Expression::Super(_)
        | Expression::ClassReference { .. }
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. } => {
            // No variable access (ClassReference resolves at compile time)
            // Primitive is a pragma, no variable access
        }

        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    analyze_expression(expr, analysis, ctx);
                }
            }
        }

        Expression::Identifier(id) => {
            // Read of a variable - track ALL reads, not just known locals
            // This is important for detecting outer scope variables that need threading
            analysis.local_reads.insert(id.name.to_string());
            // Track reads of variables not yet locally defined (captured from outer scope)
            if !ctx.local_bindings.contains(id.name.as_str()) {
                analysis.captured_reads.insert(id.name.to_string());
            }
        }

        Expression::FieldAccess {
            receiver, field, ..
        } => {
            // Read of a field (self.field)
            analyze_expression(receiver, analysis, ctx);
            if is_self_reference(receiver) {
                analysis.field_reads.insert(field.name.to_string());
            }
        }

        Expression::Assignment { target, value, .. } => {
            // Assignment: target is written, value is read
            analyze_expression(value, analysis, ctx);

            match target.as_ref() {
                Expression::Identifier(id) => {
                    // Local variable write
                    if ctx.local_bindings.contains(id.name.as_str()) {
                        analysis.local_writes.insert(id.name.to_string());
                    } else {
                        // New binding - add to context
                        ctx.local_bindings.insert(id.name.to_string());
                        analysis.local_writes.insert(id.name.to_string());
                    }
                }
                Expression::FieldAccess {
                    receiver, field, ..
                } => {
                    // Field assignment
                    if is_self_reference(receiver) {
                        analysis.field_writes.insert(field.name.to_string());
                    }
                }
                _ => {
                    // Complex assignment target - analyze it
                    analyze_expression(target, analysis, ctx);
                }
            }
        }

        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            ..
        } => {
            record_send_target(receiver, selector, arguments, *is_cast, analysis);
            // on:do:/ensure: run their receiver (the try/protected
            // block) inline too, in the same activation — so it needs the same
            // local_writes propagation as an inline-conditional block argument
            // (see below), not the isolated-closure treatment the generic
            // `Expression::Block` arm gives an ordinary block operand.
            let selector_name = selector.name();
            if is_exception_selector_name(&selector_name) {
                if let Expression::Block(block) = receiver.as_ref() {
                    propagate_inline_block_writes(block, analysis, ctx);
                } else {
                    analyze_expression(receiver, analysis, ctx);
                }
            } else if let Some(block) = literal_block_value_arm(expr) {
                // BT-3841: `[...] value` is the same inline activation (an arm
                // that always runs, [`literal_block_value_arm`]), so its writes
                // are the enclosing block's writes: a `do:` body whose only
                // outer write is inside a nested `[...] value` threads it.
                propagate_inline_block_writes(block, analysis, ctx);
            } else {
                analyze_expression(receiver, analysis, ctx);
            }
            // ifTrue:/ifFalse:/ifTrue:ifFalse:/ifNotNil:/on:do:/
            // ensure: blocks are compiled inline (not as closures), so their
            // local_writes and some captured_reads affect the enclosing scope.
            // Propagate them to allow the outer loop analysis to detect that a
            // captured local variable is mutated inside one of these constructs.
            //
            // captured_reads from the inner block are propagated selectively: only
            // variables that are NOT already defined in the outer block's local bindings
            // context are considered captured from the method scope. This prevents
            // variables introduced within the outer block body (e.g. `newI := i + 1`
            // before an `ifTrue: [^newI]`) from being misclassified as outer captures.
            if is_inline_propagating_selector(&selector_name)
                || is_result_try_do(receiver, &selector_name)
            {
                for arg in arguments {
                    if let Expression::Block(block) = arg {
                        propagate_inline_block_writes(block, analysis, ctx);
                    } else {
                        analyze_expression(arg, analysis, ctx);
                    }
                }
            } else {
                for arg in arguments {
                    analyze_expression(arg, analysis, ctx);
                }
            }
        }

        Expression::Block(block) => {
            // Nested block - analyze it separately
            let nested_analysis = analyze_block(block);
            // Merge reads (nested block reads outer vars)
            analysis
                .local_reads
                .extend(nested_analysis.local_reads.iter().cloned());
            analysis
                .field_reads
                .extend(nested_analysis.field_reads.iter().cloned());
            // Don't merge local_writes - nested block local mutations are isolated
            // DO merge field_writes - field mutations (self.x := ...) modify shared
            // actor state and must be visible to outer loops for state threading
            analysis
                .field_writes
                .extend(nested_analysis.field_writes.iter().cloned());
            // Propagate self-sends the same way — a self-send inside a
            // block passed to select:/collect:/do:/etc. (this is exactly that
            // shape: a `Block` argument that isn't an inline-conditional
            // selector, handled above) is itself a potential mutation source,
            // and callers like `analyze_method_body`'s purity check need to see
            // it at any nesting depth, not just at this block's own top level.
            if nested_analysis.has_self_sends {
                analysis.has_self_sends = true;
            }
            if nested_analysis.has_user_code_sends {
                analysis.has_user_code_sends = true;
            }
            analysis
                .self_send_selectors
                .extend(nested_analysis.self_send_selectors.iter().cloned());
        }

        Expression::Return { value, .. } => {
            analyze_expression(value, analysis, ctx);
        }

        Expression::Cascade { receiver, .. } => {
            // The parser folds the FIRST message into `receiver` as a whole
            // `MessageSend`, which the `MessageSend` arm analyses (receiver,
            // send target and arguments). The later messages go to the same
            // shared receiver but are not nodes of their own, so record each
            // one's send target here, paired with that receiver by the shared
            // cascade expansion (BT-3716, BT-3761): a mutating self-send hidden
            // behind an earlier pure cascade message (`self pureLog: x; check:
            // x`) must reach `compute_class_var_mutating_selectors`.
            analyze_expression(receiver, analysis, ctx);
            for send in cascade_sends(expr).filter(|s| s.site == SendSite::CascadeLater) {
                record_send_target(
                    send.receiver,
                    send.selector,
                    send.arguments,
                    false,
                    analysis,
                );
                for arg in send.arguments {
                    analyze_expression(arg, analysis, ctx);
                }
            }
        }

        Expression::Parenthesized { expression, .. } => {
            analyze_expression(expression, analysis, ctx);
        }

        Expression::Match { value, arms, .. } => {
            analyze_expression(value, analysis, ctx);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    analyze_expression(guard, analysis, ctx);
                }
                analyze_expression(&arm.body, analysis, ctx);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                analyze_expression(&pair.key, analysis, ctx);
                analyze_expression(&pair.value, analysis, ctx);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                analyze_expression(elem, analysis, ctx);
            }
            if let Some(t) = tail {
                analyze_expression(t, analysis, ctx);
            }
        }

        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            for elem in elements {
                analyze_expression(elem, analysis, ctx);
            }
        }

        Expression::DestructureAssignment { pattern, value, .. } => {
            // Destructure assignment: walk into value, then bind all pattern variables
            analyze_expression(value, analysis, ctx);
            collect_pattern_bindings(pattern, analysis, ctx);
        }
    }
}

/// Records what sending `selector` to `receiver` may do to actor state: a
/// self-send (which may mutate it; the selector is kept, see
/// `self_send_selectors`). Shared by an ordinary send and each later cascade
/// message.
fn record_send_target(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
    analysis: &mut BlockMutationAnalysis,
) {
    if is_self_reference(receiver) {
        analysis.has_self_sends = true;
        analysis
            .self_send_selectors
            .insert(selector.name().to_string());
    } else if !is_cast && send_may_run_user_code(receiver, selector, arguments) {
        analysis.has_user_code_sends = true;
    }
}

/// ADR 0137: a conservative, syntactic answer to "may this send to something
/// other than `self` run user code in this process" — a superset of what
/// codegen brackets (which also knows static operand types), so a construct
/// that holds a bracketed call is always seen as holding one here. Read off
/// the shared selector tables ([`send_user_code`],
/// [`crate::state_threading_selectors::structural_binary`]).
fn send_may_run_user_code(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    use crate::state_threading_selectors::{SendUserCode, send_user_code, structural_binary};
    if matches!(receiver, Expression::Super(_)) {
        return false;
    }
    if let MessageSelector::Binary(op) = selector {
        if structural_binary(op).is_some() {
            return false;
        }
        let numeric_literal = |e: &Expression| {
            matches!(
                e.unwrap_parens(),
                Expression::Literal(
                    crate::ast::Literal::Integer(..) | crate::ast::Literal::Float(..),
                    _
                )
            )
        };
        return !(numeric_literal(receiver) && arguments.first().is_some_and(numeric_literal));
    }
    matches!(
        send_user_code(receiver, selector, arguments),
        SendUserCode::MayRun | SendUserCode::InlinedOrClosureCall
    )
}

/// Adds all variable names bound by `pattern` to `ctx.local_bindings` and
/// `analysis.local_writes`, and processes binary segment size expressions as reads.
///
/// Delegates variable collection to `semantic_analysis::extract_pattern_bindings`
/// for leaf patterns. Binary patterns are handled segment-by-segment to preserve
/// correct `captured_reads` semantics: a size expression that references a variable
/// bound by a *later* segment in the same binary must still be recorded as a
/// captured read (read before its local definition).
fn collect_pattern_bindings(
    pattern: &crate::ast::Pattern,
    analysis: &mut BlockMutationAnalysis,
    ctx: &mut AnalysisContext,
) {
    use crate::ast::Pattern;
    match pattern {
        // Binary patterns: bind each segment's value variables *before* analyzing
        // its size expression so that forward-referenced names (e.g. `len` used as
        // a size in segment N but bound by segment N+1) are correctly classified as
        // captured reads.
        Pattern::Binary { segments, .. } => {
            for seg in segments {
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(&seg.value);
                for id in ids {
                    let name = id.name.to_string();
                    ctx.local_bindings.insert(name.clone());
                    analysis.local_writes.insert(name);
                }
                if let Some(size_expr) = &seg.size {
                    analyze_expression(size_expr, analysis, ctx);
                }
            }
        }

        // Container patterns may contain nested Binary segments; recurse to
        // ensure sequential binding semantics are applied throughout the tree.
        Pattern::Tuple { elements, .. } => {
            for elem in elements {
                collect_pattern_bindings(elem, analysis, ctx);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for elem in elements {
                collect_pattern_bindings(elem, analysis, ctx);
            }
            if let Some(rest_pat) = rest {
                collect_pattern_bindings(rest_pat, analysis, ctx);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for elem in elements {
                collect_pattern_bindings(elem, analysis, ctx);
            }
            if let Some(t) = tail {
                collect_pattern_bindings(t, analysis, ctx);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_bindings(&pair.value, analysis, ctx);
            }
        }

        Pattern::Constructor { keywords, .. } => {
            for (_, binding) in keywords {
                collect_pattern_bindings(binding, analysis, ctx);
            }
        }

        // Leaf patterns have no ordering constraints or size expressions.
        // Delegate to the canonical semantic analysis extractor.
        Pattern::Variable(_)
        | Pattern::Wildcard(..)
        | Pattern::Literal(..)
        | Pattern::Nil(..)
        | Pattern::Type { .. } => {
            let (identifiers, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
            for id in identifiers {
                let name = id.name.to_string();
                ctx.local_bindings.insert(name.clone());
                analysis.local_writes.insert(name);
            }
        }
    }
}

/// Returns true if the expression is a reference to `self`.
///
/// `pub(crate)`: also used by [`crate::semantic_analysis::initialize_assigns`]'s
/// must-analysis (ADR 0124 A2a) — same "is this receiver `self`" question this
/// may-analysis already answers, imported rather than redefined (CLAUDE.md's
/// no-duplicate-implementations rule).
/// Whether `expr` (parentheses looked through) is `self` or `super`: the
/// receivers whose sends run the object's own methods.
#[must_use]
pub(crate) fn is_self_or_super(expr: &Expression) -> bool {
    let expr = expr.unwrap_parens();
    is_self_reference(expr) || matches!(expr, Expression::Super(_))
}

/// The receiver a cascade sends all its messages to: the parser folds the
/// first message into `receiver`, so it is that send's own receiver.
#[must_use]
pub(crate) fn cascade_shared_receiver(receiver: &Expression) -> &Expression {
    match receiver {
        Expression::MessageSend {
            receiver: shared, ..
        } => shared,
        other => other,
    }
}

pub(crate) fn is_self_reference(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
}

/// Returns true if `selector_name` is `on:do:` or `ensure:` — exception
/// selectors whose *receiver* (the try/protected block) runs inline in the
/// enclosing activation, not as an isolated closure. Delegates to the shared
/// classifier in [`crate::state_threading_selectors`] so this stays in sync
/// with codegen's own state-threading selector classification.
fn is_exception_selector_name(selector_name: &str) -> bool {
    crate::state_threading_selectors::is_exception_selector(selector_name)
}

/// Returns true if the selector's block arguments (and, for `on:do:`/`ensure:`,
/// receiver — handled separately, see [`is_exception_selector_name`]) are
/// compiled inline rather than as isolated closures, so mutations inside them
/// affect the enclosing scope: `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:`
/// (via [`crate::state_threading_selectors::is_conditional_selector`]), plus
/// `on:do:`/`ensure:` (their non-receiver block arguments — e.g.
/// `on:do:`'s handler — are inline for the same reason as the receiver).
/// `Result tryDo:` is receiver-dependent: see [`is_result_try_do`].
fn is_inline_propagating_selector(selector_name: &str) -> bool {
    crate::state_threading_selectors::is_conditional_selector(selector_name)
        || is_exception_selector_name(selector_name)
}

/// ADR 0131 Phase 4 (BT-3750): whether `receiver selector_name:` is
/// `Result tryDo:`, the only `tryDo:` that compiles to
/// `beamtalk_result:'tryDo:'/2` and so threads its block's writes. The one
/// receiver-aware check [`exception_arm_blocks`] and every analysis that
/// treats the block as inline read; a `tryDo:` sent to any other receiver is
/// an ordinary send whose block is a closure.
#[must_use]
pub fn is_result_try_do(receiver: &Expression, selector_name: &str) -> bool {
    crate::state_threading_selectors::is_try_do_selector(selector_name)
        && matches!(
            receiver,
            Expression::ClassReference {
                name,
                package: None,
                ..
            } if name.name == "Result"
        )
}

/// ADR 0137 (BT-4020): whether a `whileTrue:`/`whileFalse:` `condition` holds
/// a call bracketed with a `State` store/reload
/// ([`BlockMutationAnalysis::has_synced_user_code_sends`]). The caller adds it
/// to [`while_condition_has_state_effects`] in an actor instance method only.
#[must_use]
pub fn while_condition_has_synced_user_code_sends(condition: &Expression) -> bool {
    matches!(condition, Expression::Block(block)
        if analyze_block(block).has_synced_user_code_sends())
}

/// ADR 0118 phase 3 / BT-3845: whether a `whileTrue:`/`whileFalse:`
/// `condition` has state effects (a field write, a self-send, a stored-block
/// call or an opaque-callable HOM send) that the loop must thread. Only a
/// literal block condition can; in a class method nothing does (ADR 0130 §3:
/// a class method threads no family, and answering `true` there made the loop
/// open a `StateAcc` whose initial state does not exist, BT-3694).
///
/// The one rule codegen's loop gates read (`condition_has_state_effects`:
/// `while_loop_threads`, the stateful condition lowering, the actor
/// statement classifier) and the one the ADR 0131 §6 validator reads through
/// [`while_condition_writes_thread`].
#[must_use]
pub fn while_condition_has_state_effects(condition: &Expression, in_class_method: bool) -> bool {
    matches!(condition, Expression::Block(block)
        if block_condition_has_state_effects(block, in_class_method))
}

/// BT-3782/BT-3845: whether an outer-local write in the condition block of a
/// keyword `whileTrue:`/`whileFalse:` is threaded back to the method in a
/// compiled (non-REPL) method. It is exactly when codegen compiles the
/// condition in the loop's own frame and packs its local writes into the
/// loop's `StateAcc` (`generate_stateful_while_condition`): the `body`
/// argument is a literal block (only then is the stateful lowering
/// considered at all) and the condition has state effects
/// ([`while_condition_has_state_effects`]).
///
/// A body that writes a local does **not** extend this: it also selects the
/// stateful lowering, but with a pure condition the condition is compiled as a
/// plain block body whose own writes are never packed, so the loop re-reads
/// the pre-loop value forever (probed for BT-3845:
/// `[t := t + 1. t < 3] whileTrue: [u := u + 1]` hangs in value-type, actor and
/// class methods). Such a condition write stays a compile error; pinned by the
/// `while_condition_*` matrix in `semantic_analysis/tests/local_threading.rs`
/// and, for the accepted shapes, `stdlib/test/bt3746stateful_while_test.bt`.
///
/// BT-4015: codegen now carries a condition's local write whether or not the
/// condition has a state effect (`while_condition_has_carried_effects`, for the
/// REPL and legacy workspace block bodies, which this check does not gate).
/// Accepting such a write in a compiled method too would be a separate change,
/// proven over the compiled contexts first.
#[must_use]
pub fn while_condition_writes_thread(
    condition: &Block,
    body: Option<&Expression>,
    in_class_method: bool,
) -> bool {
    matches!(body, Some(Expression::Block(_)))
        && block_condition_has_state_effects(condition, in_class_method)
}

fn block_condition_has_state_effects(condition: &Block, in_class_method: bool) -> bool {
    !in_class_method && analyze_block(condition).has_state_effects()
}

/// Propagates a nested inline-compiled block's mutation
/// analysis into the enclosing block's `analysis` — used for the block
/// receiver/arguments of `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:`/
/// `on:do:`/`ensure:`, none of which introduce a separate closure activation
/// (unlike an ordinary block operand, e.g. a `do:`/`collect:` argument, which
/// isolates its own `local_writes` — see the `Expression::Block` arm below).
fn propagate_inline_block_writes(
    block: &Block,
    analysis: &mut BlockMutationAnalysis,
    ctx: &AnalysisContext,
) {
    let nested = analyze_block(block);
    analysis
        .local_reads
        .extend(nested.local_reads.iter().cloned());
    // Exclude this block's own parameters (e.g.
    // on:do:'s exception var, ifNotNil:'s bound value) before merging
    // local_writes into the enclosing analysis — a write to the block's own
    // param (`on: Error do: [:e | e := 1]`) is confined to that param's own
    // shadowed binding, not a genuine outer-scope mutation. Mirrors the same
    // exclusion `collect_list_op_cross_scope_mutations`/
    // `construct_outer_local_writes` applies for the identical
    // construct shape.
    let block_params: HashSet<String> = block
        .parameters
        .iter()
        .map(|p| p.name.to_string())
        .collect();
    for v in &nested.local_writes {
        if !block_params.contains(v.as_str()) {
            analysis.local_writes.insert(v.clone());
        }
    }
    // Only propagate captured_reads for vars not yet defined locally.
    for v in &nested.captured_reads {
        if !ctx.local_bindings.contains(v.as_str()) {
            analysis.captured_reads.insert(v.clone());
        }
    }
    analysis
        .field_reads
        .extend(nested.field_reads.iter().cloned());
    analysis
        .field_writes
        .extend(nested.field_writes.iter().cloned());
    if nested.has_self_sends {
        analysis.has_self_sends = true;
    }
    if nested.has_user_code_sends {
        analysis.has_user_code_sends = true;
    }
    analysis
        .self_send_selectors
        .extend(nested.self_send_selectors.iter().cloned());
}

/// What an [`OuterLocalWrite`] writes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteKind {
    /// A local of an enclosing scope ([`outer_local_writes`]).
    Local,
    /// Object state: `self.field :=`, an instance field or, in a class-side
    /// method, a class variable ([`outer_state_writes`], BT-3922).
    ObjectState,
}

/// One outer write found by [`outer_local_writes`] or [`outer_state_writes`]:
/// the name written and the span of the assignment that writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OuterLocalWrite {
    /// What is written: a captured local, or object state.
    pub kind: WriteKind,
    /// The outer local, or the field, written.
    pub name: ecow::EcoString,
    /// The assignment (or destructuring pattern) that writes it.
    pub span: Span,
}

/// ADR 0131 (BT-3745): the outer locals `block` writes, anywhere in its body
/// including nested blocks, in source order (first write of each name only).
///
/// A name is an outer local when `bound_outside(name)` says it is bound in
/// the scope enclosing `block` and nothing between that scope and the write
/// (the block's own parameters, a parameter of a nested block, a match-arm or
/// destructuring binding, or an earlier first assignment that introduced it
/// as a block local) rebinds it. Write-only writes count: a block that
/// assigns an outer local without reading it still writes the caller's local
/// in Smalltalk, so it is a Tier 2 block value (ADR 0041) and its write must
/// be threaded back or it is lost.
///
/// This is the single "does this block write an outer local" fact that both
/// ADR 0131 diagnostics (§6 and the Phase 0 allow-set, in
/// `validators/local_threading.rs`) are computed from.
pub fn outer_local_writes(
    block: &Block,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::All,
    };
    walker.block(block);
    walker.writes
}

/// ADR 0133 (BT-3922): the `self.field` assignments `block` makes anywhere in
/// its body, nested blocks included, first write of each field. The
/// object-state half of the escaping-block rule: in an instance method these
/// are instance fields, in a class-side method class variables (ADR 0130).
/// Computed by the same walk as [`outer_local_writes`].
pub fn outer_state_writes(block: &Block) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside: &|_| false,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::All,
    };
    walker.block(block);
    walker.state_writes
}

/// The spans of the blocks nested in `block` that take a `self` parameter (a
/// class-builder method body): inside one, `self` is that parameter, not the
/// enclosing object. `None` when `block` itself takes one.
fn self_rebound_spans(block: &Block) -> Option<Vec<Span>> {
    if block.parameters.iter().any(|p| p.name == "self") {
        return None;
    }
    let mut rebound = Vec::new();
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Block(inner) = e {
                if inner.parameters.iter().any(|p| p.name == "self") {
                    rebound.push(inner.span);
                }
            }
        });
    }
    Some(rebound)
}

/// ADR 0133 (BT-3952): the first message `block` sends to `self` or `super`
/// anywhere in its body, nested blocks included: its selector and the span of
/// the send. A self-send dispatches against the `State` the block captured
/// when it was made, so it can answer a stale value as `self.field` can.
pub fn outer_self_send(block: &Block) -> Option<(&'static str, ecow::EcoString, Span)> {
    let rebound = self_rebound_spans(block)?;
    let mut found = None;
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::MessageSend {
                receiver,
                selector,
                span,
                ..
            } = e
            {
                if found.is_none()
                    && is_self_or_super(receiver)
                    && !rebound
                        .iter()
                        .any(|b| b.start() <= span.start() && span.end() <= b.end())
                {
                    let word = if matches!(receiver.unwrap_parens(), Expression::Super(_)) {
                        "super"
                    } else {
                        "self"
                    };
                    found = Some((word, selector.name(), *span));
                }
            }
        });
    }
    found
}

/// ADR 0133 (BT-3952): the `self.field` reads (and `self fieldAt: k` reads, named
/// `self fieldAt:` or `super fieldAt:`) `block` makes anywhere in its
/// body, nested blocks included, first read of each field. An assignment's
/// target is a write, not a read (`outer_state_writes`); a `self.x` inside the
/// assigned value is a read. The read-side twin of [`outer_state_writes`].
pub fn outer_state_reads(block: &Block) -> Vec<OuterLocalWrite> {
    let mut targets: HashSet<Span> = HashSet::new();
    let mut reads: Vec<OuterLocalWrite> = Vec::new();
    let Some(rebound) = self_rebound_spans(block) else {
        return reads;
    };
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Assignment { target, .. } = e {
                if let Expression::FieldAccess { span, .. } = target.as_ref() {
                    targets.insert(*span);
                }
            }
        });
    }
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            // The reflective read `self fieldAt: k` (or `super fieldAt: k`)
            // reads the same captured `State` as `self.k`.
            if let Expression::MessageSend {
                receiver,
                selector,
                span,
                ..
            } = e
            {
                if selector.name() == "fieldAt:"
                    && is_self_or_super(receiver)
                    && !rebound
                        .iter()
                        .any(|b| b.start() <= span.start() && span.end() <= b.end())
                {
                    let word = if matches!(receiver.unwrap_parens(), Expression::Super(_)) {
                        "super"
                    } else {
                        "self"
                    };
                    let name: ecow::EcoString = format!("{word} fieldAt:").into();
                    if !reads.iter().any(|r| r.name == name) {
                        reads.push(OuterLocalWrite {
                            kind: WriteKind::ObjectState,
                            name,
                            span: *span,
                        });
                    }
                }
                return;
            }
            let Expression::FieldAccess {
                receiver,
                field,
                span,
            } = e
            else {
                return;
            };
            if is_self_reference(receiver.unwrap_parens())
                && !targets.contains(span)
                && !rebound
                    .iter()
                    .any(|b| b.start() <= span.start() && span.end() <= b.end())
                && !reads.iter().any(|r| r.name == field.name)
            {
                reads.push(OuterLocalWrite {
                    kind: WriteKind::ObjectState,
                    name: field.name.clone(),
                    span: *span,
                });
            }
        });
    }
    reads
}

/// Whether evaluating `expr` itself (not its operands) can change the
/// actor's `State` (BT-3952): a `self.field :=` write, or a message sent to
/// `self` or `super` (a user method, `fieldAt:put:`, ... any of which may
/// write a field). Conservative: every self-send counts.
#[must_use]
pub fn may_change_actor_state(expr: &Expression) -> bool {
    match expr {
        Expression::Assignment { target, .. } => matches!(
            target.as_ref(),
            Expression::FieldAccess { receiver, .. } if is_self_reference(receiver.unwrap_parens())
        ),
        Expression::MessageSend { receiver, .. } => is_self_or_super(receiver),
        Expression::Cascade { receiver, .. } => is_self_or_super(cascade_shared_receiver(receiver)),
        _ => false,
    }
}

/// Whether `block` contains, anywhere (nested blocks included), an expression
/// that [`may_change_actor_state`].
#[must_use]
pub fn block_may_change_actor_state(block: &Block) -> bool {
    let mut changes = false;
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            changes |= may_change_actor_state(e);
        });
    }
    changes
}

/// The names `stmt` itself binds by assignment or destructuring: those not
/// inside a block nested in it, and not a match-arm binding. A nested block's
/// own first assignment binds the name in that block
/// (`_b := [i := 0. [i < 3] whileTrue: [i := i + 1]]` stores a closure whose
/// `i` is not the statement's). The block-scope rule is [`OuterWriteWalker`]'s,
/// the one encoding of "every nested `Block` opens its own binding scope".
/// A name assigned inside a `match:` arm body is not counted either: the
/// walker gives each arm its own frame and pops it after the arm, matching
/// the name resolver's per-arm scope.
#[must_use]
pub fn names_assigned_outside_nested_blocks(stmt: &Expression) -> HashSet<ecow::EcoString> {
    let mut walker = OuterWriteWalker {
        bound_outside: &|_| false,
        frames: vec![HashSet::new()],
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 1,
        nested: NestedBlocks::None,
    };
    walker.expr(stmt);
    walker.frames.pop().unwrap_or_default()
}

/// Scope-tracking walk behind [`outer_local_writes`].
struct OuterWriteWalker<'a> {
    bound_outside: &'a dyn Fn(&str) -> bool,
    /// Names bound inside the block being analysed, innermost last.
    frames: Vec<HashSet<ecow::EcoString>>,
    writes: Vec<OuterLocalWrite>,
    /// Assignments to `self.field` (an instance field, or a class variable in a
    /// class-side method) inside the walked block, first write of each field.
    state_writes: Vec<OuterLocalWrite>,
    /// Which nested blocks the walk descends into.
    nested: NestedBlocks,
    /// How many walked blocks enclose the current expression.
    blocks_entered: usize,
}

/// Which nested blocks an [`OuterWriteWalker`] descends into.
#[derive(Clone, Copy, PartialEq, Eq)]
enum NestedBlocks {
    /// No block at all ([`names_assigned_outside_nested_blocks`]): every
    /// nested `Block` opens its own binding scope, so none is entered.
    None,
    /// Every block ([`outer_local_writes`]).
    All,
    /// Only the blocks of nested local-threading constructs
    /// ([`construct_outer_local_writes`]).
    Producers,
    /// Only the blocks of nested constructs whose family is threaded today
    /// ([`threaded_today_block_writes`]).
    ProducersThreadedToday,
}

impl OuterWriteWalker<'_> {
    fn bound_inside(&self, name: &str) -> bool {
        self.frames.iter().any(|f| f.contains(name))
    }

    fn define(&mut self, name: &ecow::EcoString) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.clone());
        }
    }

    /// An assignment to `name` at `span`: an outer write, or the first
    /// assignment of a block local (which then shadows nothing outside).
    fn assign(&mut self, name: &ecow::EcoString, span: Span) {
        // Outside every walked block (only [`expression_threaded_writes`]
        // walks there): the expression's own write, not a construct's.
        if self.blocks_entered == 0 || self.bound_inside(name) {
            return;
        }
        if (self.bound_outside)(name) {
            if !self.writes.iter().any(|w| &w.name == name) {
                self.writes.push(OuterLocalWrite {
                    kind: WriteKind::Local,
                    name: name.clone(),
                    span,
                });
            }
        } else {
            self.define(name);
        }
    }

    /// Whether an assignment to a field of `receiver` is a write to the state
    /// of a captured `self`: `self.field :=` inside a walked block. A block
    /// that takes `self` as a parameter (`[:self | self.n := ...]`, a
    /// class-builder method body) writes its own receiver's state instead.
    fn is_captured_self_field_write(&self, receiver: &Expression) -> bool {
        self.blocks_entered > 0
            && !self.bound_inside("self")
            && matches!(receiver.unwrap_parens(), Expression::Identifier(r) if r.name == "self")
    }

    /// The target of an assignment at `span`: a local, a captured `self`'s
    /// field, or an expression walked for what it contains.
    fn assignment_target(&mut self, target: &Expression, span: Span) {
        match target {
            Expression::Identifier(id) => self.assign(&id.name, span),
            Expression::FieldAccess {
                receiver, field, ..
            } if self.is_captured_self_field_write(receiver) => {
                self.state_write(&field.name, span);
            }
            other => self.expr(other),
        }
    }

    /// Records the first `self.field :=` write of each field.
    fn state_write(&mut self, field: &ecow::EcoString, span: Span) {
        if !self.state_writes.iter().any(|w| &w.name == field) {
            self.state_writes.push(OuterLocalWrite {
                kind: WriteKind::ObjectState,
                name: field.clone(),
                span,
            });
        }
    }

    fn block(&mut self, block: &Block) {
        self.frames
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        self.blocks_entered += 1;
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
        self.blocks_entered -= 1;
        self.frames.pop();
    }

    /// A message send. Unless every block is walked, only the blocks of a
    /// nested construct are (its writes are the enclosing construct's too);
    /// any other block operand is a closure.
    fn send(&mut self, send: &Expression, receiver: &Expression, arguments: &[Expression]) {
        let nested: Vec<&Block> = match self.nested {
            NestedBlocks::None | NestedBlocks::All => Vec::new(),
            NestedBlocks::Producers => local_threading_construct(send)
                .map(|c| c.blocks)
                .unwrap_or_default(),
            NestedBlocks::ProducersThreadedToday => local_threading_construct(send)
                .filter(LocalThreadingConstruct::is_threaded_today)
                .map(|c| c.blocks)
                .unwrap_or_default(),
        };
        for operand in std::iter::once(receiver).chain(arguments) {
            match operand {
                Expression::Block(b) if nested.iter().any(|n| std::ptr::eq(*n, b)) => {
                    self.block(b);
                }
                other => self.expr(other),
            }
        }
    }

    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Assignment {
                target,
                value,
                span,
                ..
            } => {
                self.expr(value);
                self.assignment_target(target, *span);
            }
            Expression::DestructureAssignment {
                pattern,
                value,
                span,
            } => {
                self.expr(value);
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.assign(&id.name, *span);
                }
            }
            Expression::Block(block) => {
                if self.nested == NestedBlocks::All {
                    self.block(block);
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                for arm in arms {
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    self.frames
                        .push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                    self.frames.pop();
                }
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => self.send(expr, receiver, arguments),
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
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::Return { value, .. } => self.expr(value),
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.expr(&pair.key);
                    self.expr(&pair.value);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    self.expr(e);
                }
                if let Some(t) = tail {
                    self.expr(t);
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
}

/// ADR 0131 §1: the kind of a local-threading construct (see
/// [`local_threading_construct`]). Codegen lowers each family with its own
/// generator. The families not threaded yet are still recognized, so the
/// phase that makes one a producer only flips
/// [`LocalThreadingConstruct::is_threaded_today`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalThreadingFamily {
    /// `whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`, the
    /// unary `whileTrue`/`whileFalse`/`timesRepeat`, and `repeat`.
    Loop,
    /// A collection fold or `do:`: any other
    /// [`crate::state_threading_selectors::is_state_threading_keyword_selector`]
    /// selector, including `detect:ifNone:`, `eachWithIndex:` and
    /// `do:separatedBy:`.
    Fold,
    /// The conditional family
    /// ([`crate::state_threading_selectors::is_conditional_selector`]).
    Conditional,
    /// `on:do:` / `ensure:`.
    Exception,
    /// `value`/`value:`… sent to a block literal of the send's arity
    /// ([`block_literal_arity_matches`], BT-3884).
    BlockValue,
    /// A block-taking lookup selector
    /// ([`crate::state_threading_selectors::LOOKUP_SHAPES`]). Threaded only
    /// for the rows that are
    /// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)
    /// (ADR 0131 Phase 2d, BT-3798): see
    /// [`LocalThreadingConstruct::is_threaded_today`].
    Lookup,
    /// `Result tryDo:` (ADR 0131 §5, Phase 4): a catch-boundary construct
    /// whose block literal compiles to a Tier 2 `fun(StateAcc)` and is a
    /// producer through [`exception_producer_writes`], like `on:do:`.
    TryDo,
}

/// ADR 0131 §1: a local-threading construct. It is a send whose literal
/// block(s) run in the enclosing activation, so an outer-local write inside
/// them is a state effect of the send. Built by
/// [`local_threading_construct`].
#[derive(Debug, Clone)]
pub struct LocalThreadingConstruct<'a> {
    /// The send's selector.
    pub selector: String,
    /// Which construct it is.
    pub family: LocalThreadingFamily,
    /// The construct's block literals, receiver first.
    pub blocks: Vec<&'a Block>,
}

impl LocalThreadingConstruct<'_> {
    /// Whether codegen threads this construct's outer-local writes back today
    /// (every family but a lookup, whose selector must be
    /// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)).
    /// The one predicate the nested-construct walks ([`threaded_today_block_writes`])
    /// and the Phase 0 check filter on.
    #[must_use]
    pub fn is_threaded_today(&self) -> bool {
        match self.family {
            LocalThreadingFamily::Lookup => {
                crate::state_threading_selectors::is_dictionary_inline_lookup(&self.selector)
            }
            _ => true,
        }
    }
}

/// ADR 0131 §1 (BT-3745, BT-3746): the one recognizer for a local-threading
/// construct. `None` when `expr` (parentheses peeled) is not one.
///
/// Derived only from [`crate::state_threading_selectors`], with no selector
/// table of its own. The blocks it collects:
///
/// - a state-threading, conditional or exception keyword selector: each
///   literal block argument, plus the literal block receiver for
///   `whileTrue:`/`whileFalse:` (the condition) and `on:do:`/`ensure:` (the
///   protected body);
/// - a lookup selector: its block arguments; `tryDo:`: its block argument;
/// - a unary loop selector (`whileTrue`, `whileFalse`, `timesRepeat`,
///   `repeat`) or a block `value` send: its literal block receiver.
///
/// A `value`-family send is a [`LocalThreadingFamily::BlockValue`] construct
/// only when its block literal's arity is the send's (BT-3884): a block
/// literal of another arity ([`block_literal_of_other_arity`], the predicate
/// codegen's lowering reads) is applied and raises `arity_mismatch`, so the
/// send is no construct.
///
/// Only direct `[...]` literals count, because only those are inlined. A
/// parenthesized or stored block is a block value (ADR 0131 §6).
///
/// The Phase 0 allow-set check (`validators/local_threading.rs`) and
/// codegen's `threaded_locals_of` both recognize constructs with this.
#[must_use]
pub fn local_threading_construct(expr: &Expression) -> Option<LocalThreadingConstruct<'_>> {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threaded_block_receiver,
        is_state_threading_keyword_selector, is_state_threading_unary_selector,
        lookup_block_arg_indices,
    };
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let sel = selector.name().to_string();
    let receiver_block = match receiver.as_ref() {
        Expression::Block(b) => Some(b),
        _ => None,
    };
    // BT-3884: a `value`-family send is a block evaluation only when the
    // block literal's arity is the send's. One of another arity
    // ([`block_literal_of_other_arity`]: `[t := t + 1] value: 1`) is applied,
    // never inlined: it raises `arity_mismatch` and its body never runs, so it
    // is no construct and its writes are no state effect of the send.
    let evaluated_block = receiver_block
        .filter(|_| block_literal_of_other_arity(receiver, selector, arguments.len()).is_none());
    let literal_args = || {
        arguments.iter().filter_map(|a| match a {
            Expression::Block(b) => Some(b),
            _ => None,
        })
    };
    let mut blocks: Vec<&Block> = Vec::new();
    let family = match selector {
        MessageSelector::Keyword(_) => {
            if is_exception_selector(&sel) {
                blocks.extend(receiver_block);
                blocks.extend(literal_args());
                LocalThreadingFamily::Exception
            } else if is_conditional_selector(&sel) {
                blocks.extend(literal_args());
                LocalThreadingFamily::Conditional
            } else if is_state_threading_keyword_selector(&sel) {
                let is_while = matches!(sel.as_str(), "whileTrue:" | "whileFalse:");
                if is_while {
                    blocks.extend(receiver_block);
                }
                blocks.extend(literal_args());
                if is_while || matches!(sel.as_str(), "timesRepeat:" | "to:do:" | "to:by:do:") {
                    LocalThreadingFamily::Loop
                } else {
                    LocalThreadingFamily::Fold
                }
            } else if is_state_threaded_block_receiver(&sel) {
                blocks.extend(evaluated_block);
                LocalThreadingFamily::BlockValue
            } else if !lookup_block_arg_indices(&sel).is_empty() {
                blocks.extend(lookup_block_arg_indices(&sel).iter().filter_map(|&i| {
                    match arguments.get(i) {
                        Some(Expression::Block(b)) => Some(b),
                        _ => None,
                    }
                }));
                LocalThreadingFamily::Lookup
            } else if is_result_try_do(receiver, &sel) {
                blocks.extend(literal_args());
                LocalThreadingFamily::TryDo
            } else {
                return None;
            }
        }
        MessageSelector::Unary(_) => {
            if is_state_threading_unary_selector(&sel) || sel == "repeat" {
                blocks.extend(receiver_block);
                LocalThreadingFamily::Loop
            } else if is_state_threaded_block_receiver(&sel) {
                blocks.extend(evaluated_block);
                LocalThreadingFamily::BlockValue
            } else {
                return None;
            }
        }
        MessageSelector::Binary(_) => return None,
    };
    if blocks.is_empty() {
        None
    } else {
        Some(LocalThreadingConstruct {
            selector: sel,
            family,
            blocks,
        })
    }
}

/// ADR 0131 §1 "Transitive closure" (BT-3746): the outer locals a
/// local-threading construct writes, in source order (first write of each
/// name only). This is the construct's threaded set.
///
/// It holds the writes in the construct's own blocks, plus the writes of
/// every construct nested in them, transitively (o7/o8: an outer `on:do:`
/// carries a local written only inside a conditional arm's inner `on:do:`).
/// A nested construct counts whether or not its family is threaded today:
/// the set is what ADR 0131 threads, and
/// [`threaded_today_block_writes`] is the part threaded now. Any other
/// nested block is a closure, and its writes are not this construct's: the
/// §6 check deals with them. `bound_outside` and the scope rules are those
/// of [`outer_local_writes`].
#[must_use]
pub fn construct_outer_local_writes(
    construct: &LocalThreadingConstruct<'_>,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    threaded_block_writes(&construct.blocks, bound_outside)
}

/// [`construct_outer_local_writes`] over a given list of construct blocks.
#[must_use]
pub fn threaded_block_writes(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    walk_construct_blocks(blocks, bound_outside, NestedBlocks::Producers)
}

/// [`threaded_block_writes`] closed only over nested constructs whose
/// construct [`LocalThreadingConstruct::is_threaded_today`]: the writes today's
/// lowering threads back. Codegen's tuple builders pack this set; ADR 0131
/// phases 2-4 grow it until it is [`threaded_block_writes`].
#[must_use]
pub fn threaded_today_block_writes(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    walk_construct_blocks(blocks, bound_outside, NestedBlocks::ProducersThreadedToday)
}

/// ADR 0131 §1a: the outer locals written inside every local-threading
/// construct reachable from `expr` without crossing a closure boundary, in
/// source order (first write of each name only) — the union of the threaded
/// sets ([`construct_outer_local_writes`]) of `expr` itself if it is a
/// construct, of a construct passed as an argument or used as a receiver
/// anywhere inside it (`self id: (c ifTrue: [t := …] …)`, `(c ifTrue: [t := …]
/// …) + 0`), and transitively of the constructs nested in those. A block
/// operand that is not a construct block is a closure and is not entered;
/// an assignment in `expr` outside every construct block is `expr`'s own
/// write and is not included. `bound_outside` and the scope rules are those
/// of [`outer_local_writes`].
#[must_use]
pub fn expression_threaded_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::Producers,
    };
    walker.expr(expr);
    walker.writes
}

/// ADR 0131 Phase 2 (BT-3749): a loop or fold in
/// [`crate::state_threading_selectors::producer_body_arg_index`]'s table, as
/// its selector and its literal body block, or `None`.
#[must_use]
pub fn producer_body_block(expr: &Expression) -> Option<(String, &Block)> {
    let Expression::MessageSend {
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
    let index = crate::state_threading_selectors::producer_body_arg_index(&sel, arguments.len())?;
    match arguments.get(index) {
        Some(Expression::Block(body)) => Some((sel, body)),
        _ => None,
    }
}

/// ADR 0131 Phase 2 (BT-3749, BT-3794): the literal blocks a loop or fold
/// producer packs into its construct tuple, in source order: the condition
/// block of `whileTrue:`/`whileFalse:` (the receiver), the body block
/// ([`producer_body_block`]) and, for `detect:ifNone:`, the `ifNone:` handler
/// ([`crate::state_threading_selectors::producer_handler_arg_index`]). Empty
/// when `expr` is not a producer send.
///
/// The one rule for what a producer packs: [`operand_producer_writes`] (the
/// Phase 0 allow-set check) and codegen's `lowered_construct_blocks` and fold
/// plans all read it, so the diagnostic and the lowering cannot pack
/// different sets.
#[must_use]
pub fn producer_packed_blocks(expr: &Expression) -> Vec<&Block> {
    let Some((sel, body)) = producer_body_block(expr) else {
        return Vec::new();
    };
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return Vec::new();
    };
    let mut packed: Vec<&Block> = Vec::with_capacity(3);
    if matches!(sel.as_str(), "whileTrue:" | "whileFalse:") {
        if let Expression::Block(cond) = receiver.as_ref() {
            packed.push(cond);
        }
    }
    packed.push(body);
    if let Some(index) =
        crate::state_threading_selectors::producer_handler_arg_index(&sel, arguments.len())
    {
        if let Some(Expression::Block(handler)) = arguments.get(index) {
            packed.push(handler);
        }
    }
    packed
}

/// BT-3855: the handler block of the producer `expr` when the producer runs it
/// inside its own body frame as the arm of a conditional statement
/// ([`crate::state_threading_selectors::producer_handler_is_body_arm`]:
/// `do:separatedBy:`'s separator), or `None`.
///
/// That conditional is a statement of the fold body, lowered by the body's
/// frame at the handler block's span: a lowering the producer performs for its
/// own construct, like the statements of its body, not a site of the sites
/// rule. Codegen's `do:separatedBy:` desugar gives the conditional this span,
/// and the producer agreement test reads the same rule.
#[must_use]
pub fn producer_body_arm_handler(expr: &Expression) -> Option<&Block> {
    let (sel, _) = producer_body_block(expr)?;
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return None;
    };
    if !crate::state_threading_selectors::producer_handler_is_body_arm(&sel, arguments.len()) {
        return None;
    }
    let index =
        crate::state_threading_selectors::producer_handler_arg_index(&sel, arguments.len())?;
    match arguments.get(index) {
        Some(Expression::Block(handler)) => Some(handler),
        _ => None,
    }
}

/// ADR 0131 Phase 2 (BT-3749): the threaded set of a loop or fold that
/// codegen lowers as a local-threading producer in operand position, or
/// `None` when `expr` is not one.
///
/// `expr` qualifies when it is a [`producer_body_block`] send whose
/// threaded set ([`construct_outer_local_writes`]) is not empty and equals
/// the set its tuple packs ([`threaded_today_block_writes`] over
/// [`producer_packed_blocks`]). A construct whose set is larger than what it
/// packs (a write inside a nested lookup or `tryDo:`) would drop the
/// difference, so it does not qualify.
///
/// Read by the Phase 0 allow-set check; codegen's producer applies the same
/// rule through `threaded_locals_of` (`ThreadedLocals::lowered ==
/// ThreadedLocals::names`).
#[must_use]
pub fn operand_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let construct = local_threading_construct(expr)?;
    let packed = producer_packed_blocks(expr);
    if packed.is_empty() {
        return None;
    }
    packed_writes_agree(&construct, &packed, bound_outside)
}

/// The agreement tail of every producer rule: the construct's threaded set
/// ([`construct_outer_local_writes`]) is not empty and equals the set its
/// `packed` blocks pack (`threaded_today_block_writes`), so no write is dropped.
/// Returns the construct's set, or `None` when the rule fails.
fn packed_writes_agree(
    construct: &LocalThreadingConstruct<'_>,
    packed: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let names = |writes: &[OuterLocalWrite]| {
        let mut names: Vec<&str> = writes.iter().map(|w| w.name.as_str()).collect();
        names.sort_unstable();
        names.dedup();
        names.into_iter().map(str::to_string).collect::<Vec<_>>()
    };
    let all = construct_outer_local_writes(construct, bound_outside);
    let today = threaded_today_block_writes(packed, bound_outside);
    (!all.is_empty() && names(&all) == names(&today)).then_some(all)
}

/// ADR 0131 (BT-3841): the block of an *evaluated literal block*, the unary
/// `value` sent to a block literal with no parameters (`[t := t + 1. t] value`),
/// or `None` for any other expression.
///
/// The **receiver rule** of this family: the receiver is a `[...]` literal
/// (parentheses not peeled: `([...]) value` is a block value, ADR 0131 §6), so
/// its class is statically `Block` and no other class's `value` can run; the
/// send is never dispatched, the block body is inlined where the send sits. A
/// stored or computed block (`b value`, `(f at: 1) value`) is not a literal and
/// keeps the Tier 2 protocol (`prescan_tier2_local_vars`) or the §6 error.
/// `value:`, `value:value:` and `valueWithArguments:` are not recognized here:
/// their arguments are evaluated outside the block and bound to its parameters,
/// which the arm frame does not model, so they keep their own lowering.
///
/// The one site rule for the family: read by [`conditional_arm_blocks`] (and so
/// by the Phase 0 check, the producer rule and the site rules) and by codegen's
/// producer, which lowers the block as a `BranchArm` frame that always runs.
#[must_use]
pub fn literal_block_value_arm(expr: &Expression) -> Option<&Block> {
    let Expression::MessageSend {
        receiver,
        selector: selector @ MessageSelector::Unary(_),
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let Expression::Block(block) = receiver.as_ref() else {
        return None;
    };
    (selector.well_known() == Some(crate::ast::WellKnownSelector::Value)
        && arguments.is_empty()
        && block_literal_arity_matches(block, 0))
    .then_some(block)
}

/// ADR 0131 Phase 3 (BT-3738): the literal arm blocks of a conditional
/// (`ifTrue:` … `ifNil:ifNotNil:`, `and:`/`or:`) whose every block position
/// holds a block literal, in source order, or `None` for any other send.
///
/// A conditional with a non-literal arm (`c ifTrue: aBlock`) is not inlined
/// by codegen (the arm is a closure value), so it is not a producer.
///
/// ADR 0131 Phase 2d (BT-3798): a [`dictionary_inline`] lookup
/// (`d at: k ifAbsent: [...]`) is a conditional in this sense: its handler is
/// an arm that runs only when the key is missing, lowered by the same
/// producer (a `BranchArm` frame, seeded set, `LocalRebind`s) behind a
/// runtime receiver guard. Its arm blocks are its handler blocks
/// ([`lookup_arm_blocks`]).
///
/// [`dictionary_inline`]: crate::state_threading_selectors::LookupShape::dictionary_inline
///
/// ADR 0131 (BT-3841): an evaluated literal block (`[...] value`, see
/// [`literal_block_value_arm`]) is the degenerate conditional: one arm that
/// always runs. Its block is the single arm, so every rule that reads this
/// function (the conditional producer rule, the site rules, the Phase 0 check
/// and codegen's producer) treats it as an inline arm-like frame.
#[must_use]
pub fn conditional_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    if construct.family == LocalThreadingFamily::Lookup {
        return lookup_arm_blocks(expr);
    }
    if construct.family == LocalThreadingFamily::BlockValue {
        return literal_block_value_arm(expr).map(|block| vec![block]);
    }
    if construct.family != LocalThreadingFamily::Conditional {
        return None;
    }
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return None;
    };
    (!arguments.is_empty() && arguments.iter().all(|a| matches!(a, Expression::Block(_))))
        .then_some(construct.blocks)
}

/// ADR 0131 Phase 2d (BT-3798 review) and BT-3841: whether the receiver of a
/// `Dictionary`-inline send (a lookup, `keysAndValuesDo:`/`doWithKey:`) is
/// statically known NOT to be a `Dictionary`, so the send can only ever reach
/// another class's method of that name (`Ets`, a user class) and the runtime
/// guard (`beamtalk_map:lookup_for_threading/3`, `pairs_for_threading/2`) would
/// raise on every call (for a lookup, even when the key is present). Such a
/// send keeps its §6 compile error instead of becoming a producer; the guard is
/// for receivers whose class is not known (unannotated, `Dynamic`, or a class
/// that may hold a `Dictionary`).
///
/// The one rule the Phase 0 check reads for both families (codegen lowers every
/// send core's [`lookup_arm_blocks`] or keyed-iteration rule names and never sees a program Phase 0 rejects, so it
/// needs no copy of it). A receiver is statically non-`Dictionary` when it is
///
/// - `super`, or `self` where `self_is_dictionary` is false (the enclosing
///   class is not `Dictionary` or a subclass, or the method is class-side, whose
///   `self` is the class object);
/// - of an inferred type `ty` that is a known class (or the metatype of one)
///   that is neither `Dictionary`, a subclass of it, nor one of its ancestors
///   (`Object`, `Collection`, … may hold a `Dictionary`), or a union of such.
#[must_use]
pub fn receiver_statically_not_dictionary(
    receiver: &Expression,
    ty: Option<&InferredType>,
    self_is_dictionary: bool,
    hierarchy: &crate::semantic_analysis::ClassHierarchy,
) -> bool {
    match receiver.unwrap_parens() {
        Expression::Super(_) => return !self_is_dictionary,
        Expression::Identifier(id) if id.name == "self" => return !self_is_dictionary,
        _ => {}
    }
    ty.is_some_and(|ty| excludes(ty, hierarchy))
}

/// Whether `name` is a known class that can never hold a `Dictionary`.
fn class_excludes_dictionary(
    name: &str,
    hierarchy: &crate::semantic_analysis::ClassHierarchy,
) -> bool {
    hierarchy.has_class(name)
        && name != "Dictionary"
        && !hierarchy
            .superclass_chain(name)
            .iter()
            .any(|c| c == "Dictionary")
        && !hierarchy
            .superclass_chain("Dictionary")
            .iter()
            .any(|c| c == name)
}
/// [`class_excludes_dictionary`] over an inferred type.
fn excludes(ty: &InferredType, hierarchy: &crate::semantic_analysis::ClassHierarchy) -> bool {
    match ty {
        InferredType::Known { class_name, .. } => class_excludes_dictionary(class_name, hierarchy),
        InferredType::Meta { .. } => true,
        InferredType::Union { members, .. } => members.iter().all(|m| excludes(m, hierarchy)),
        _ => false,
    }
}

/// ADR 0131 Phase 2d (BT-3798): the handler blocks of a lookup send that
/// codegen lowers inline for a `Dictionary` receiver (a
/// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)
/// row of [`crate::state_threading_selectors::LOOKUP_SHAPES`]), in source
/// order, or `None` for any other send.
///
/// The lowering needs every handler position to hold a block literal taking
/// the parameters the primitive calls it with (the row's
/// [`block_params`](crate::state_threading_selectors::LookupShape::block_params):
/// none for an absent handler, the found value for an `at:ifPresent:`
/// handler, which the lowering binds to the parameter), every
/// other argument to be an ordinary value (a block literal there is a key that
/// is a block value, not a handler), and a receiver that is not itself a block
/// literal. Anything else keeps the closure lowering, which the §6 check
/// rejects when its block writes an outer local.
#[must_use]
pub fn lookup_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    if construct.family != LocalThreadingFamily::Lookup {
        return None;
    }
    let shape = crate::state_threading_selectors::lookup_shape(&construct.selector)?;
    if !shape.dictionary_inline {
        return None;
    }
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let handler_is_arm = |i: usize, params: usize| matches!(arguments.get(i), Some(Expression::Block(b)) if b.parameters.len() == params);
    let values_are_not_blocks = arguments
        .iter()
        .enumerate()
        .all(|(i, a)| shape.block_args.contains(&i) || !matches!(a, Expression::Block(_)));
    (!matches!(receiver.as_ref(), Expression::Block(_))
        && values_are_not_blocks
        && shape
            .block_args
            .iter()
            .zip(shape.block_params)
            .all(|(&i, &params)| handler_is_arm(i, params)))
    .then_some(construct.blocks)
}

/// ADR 0131 Phase 3b (BT-3817): the literal blocks of an `on:do:` / `ensure:`
/// send whose every block position holds a block literal, in source order
/// (the protected body, then the handler or cleanup), or `None` for any other
/// send. A construct with a non-literal protected body or handler
/// (`aBlock on: Error do: aHandler`) is not inlined by codegen, so it is not a
/// producer.
///
/// ADR 0131 Phase 4 (BT-3750): and the one block of a `tryDo:` send whose
/// argument is a block literal, the other exception-like catch boundary. It
/// is not inlined into a `try`: it compiles to a `fun(StateAcc)` handed to
/// `beamtalk_result:'tryDo:'/2`, whose body is a `BranchArm` frame like any
/// other arm of an exception producer.
#[must_use]
pub fn exception_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    match construct.family {
        LocalThreadingFamily::Exception => {
            let &[handler_index] =
                crate::state_threading_selectors::state_threaded_block_arg_indices(
                    &construct.selector,
                )
            else {
                return None;
            };
            match (receiver.as_ref(), arguments.get(handler_index)) {
                (Expression::Block(protected), Some(Expression::Block(handler))) => {
                    Some(vec![protected, handler])
                }
                _ => None,
            }
        }
        // Only the `Result` class lowers to `beamtalk_result:'tryDo:'/2`: any
        // other receiver keeps its own `tryDo:` and a dynamic send.
        LocalThreadingFamily::TryDo => match arguments.as_slice() {
            [Expression::Block(body)] if is_result_try_do(receiver, &construct.selector) => {
                Some(vec![body])
            }
            _ => None,
        },
        _ => None,
    }
}

/// The kind of method body a method-root local-threading producer sits in
/// (ADR 0131 §2's frame modes). The rules for which conditionals are
/// producers depend on it ([`conditional_arms_lower_as_producer`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RootMethodKind {
    /// An actor instance method: `State` is a family, so a conditional's
    /// scratch map continues it.
    Actor,
    /// A class-side method: no `State`, the scratch map is private.
    Class,
    /// An instance method of a non-actor class: no `State`, the scratch map
    /// is private, and a field write is a second family.
    ValueType,
}

/// ADR 0131 Phase 3 (BT-3738): whether the arms of a conditional can be
/// lowered by the conditional producer in a method of kind `kind`.
///
/// In a class or value-type method a conditional's scratch map is private
/// (a fresh map seeded with the construct's own locals, ADR 0131 §3), and each
/// arm is a `BranchArm` frame (§2): a loop or fold in an arm rebinds its
/// locals into the arm by a `LocalRebind`, which is a producer at one of the
/// arm statement's [`loop_body_producer_sites`] (narrowed by
/// [`is_loop_body_producer_site`], the rule a loop body uses), and so is a
/// nested conditional or `on:do:`/`ensure:` that is itself such a producer. A
/// construct in an arm that writes an outer local, or a local the arm itself
/// defines, and is not such a producer would rebuild the arm's `StateAcc` from
/// scratch and drop its other keys: the conditional is not a producer then, and
/// keeps its statement lowering. In a value-type method an arm that writes a
/// field carries a second family the producer's tuple does not. An actor method
/// has neither restriction: its `State` map is continued, never rebuilt.
///
/// `bound_outside` is the scope at the conditional; the names the arms assign
/// are bound inside them. One rule, read by the Phase 0 check and by codegen's
/// producer predicate.
#[must_use]
pub fn conditional_arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    arms_lower_as_producer(arms, kind, bound_outside, false)
}

/// ADR 0131 Phase 3b (BT-3817): whether the protected body and handler or
/// cleanup of an `on:do:`/`ensure:` can be lowered by the exception producer
/// in a method of kind `kind`. The rule of
/// [`conditional_arms_lower_as_producer`], except that a value-type arm may
/// write a field: an exception construct's result tuple carries the `SelfVt`
/// family in a trailing slot of its own (ADR 0122, BT-3506), which the producer
/// extracts after the construct.
#[must_use]
pub fn exception_arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    arms_lower_as_producer(arms, kind, bound_outside, true)
}

/// The one rule behind [`conditional_arms_lower_as_producer`] and
/// [`exception_arms_lower_as_producer`]; `field_writes_carried` says whether a
/// value-type arm's field write rides the construct's own tuple.
fn arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
    field_writes_carried: bool,
) -> bool {
    if kind == RootMethodKind::Actor {
        return true;
    }
    // The names the arm itself binds: assigned by one of its statements, not
    // inside a block nested in them. A block's own first assignment binds the
    // name in that block (`_b := [i := 0. [i < 3] whileTrue: [i := i + 1]]`
    // stores a closure whose `i` is not the arm's), so a construct writing
    // only such a name writes no local of the arm.
    let mut assigned: HashSet<ecow::EcoString> = HashSet::new();
    for arm in arms {
        for stmt in &arm.body {
            assigned.extend(names_assigned_outside_nested_blocks(&stmt.expression));
        }
    }
    let arm_bound = |name: &str| bound_outside(name) || assigned.contains(name);
    for arm in arms {
        for stmt in &arm.body {
            if !arm_statement_lowers(&stmt.expression, kind, &arm_bound, field_writes_carried) {
                return false;
            }
        }
    }
    true
}

/// [`conditional_arms_lower_as_producer`] for one arm statement.
fn arm_statement_lowers(
    stmt: &Expression,
    kind: RootMethodKind,
    arm_bound: &dyn Fn(&str) -> bool,
    field_writes_carried: bool,
) -> bool {
    let ctx = LoopBodyContext::new(stmt, false, false, false).in_arm();
    let sites: std::collections::HashSet<Span> = loop_body_producer_sites(stmt)
        .into_iter()
        .filter(|site| is_loop_body_producer_site(site, &ctx))
        .map(Expression::span)
        .collect();
    // The producers of the statement: constructs at its sites that are
    // loop/fold producers. A construct inside one is its own frame's business.
    let mut producers: Vec<Span> = Vec::new();
    crate::ast_walker::walk_expression(stmt, &mut |e| {
        if sites.contains(&e.span())
            && (operand_producer_writes(e, arm_bound).is_some()
                || conditional_producer_writes(e, arm_bound, kind).is_some()
                || exception_producer_writes(e, arm_bound, kind).is_some())
        {
            producers.push(e.span());
        }
    });
    let inside_producer = |span: Span| {
        producers
            .iter()
            .any(|p| p.start() <= span.start() && span.end() <= p.end())
    };
    let mut ok = true;
    crate::ast_walker::walk_expression(stmt, &mut |e| {
        // (`local_threading_construct` peels parentheses, so a `Parenthesized`
        // node would be judged as its construct, with a span wider than the
        // producer's own.)
        if matches!(e, Expression::Parenthesized { .. }) {
            return;
        }
        if let Some(construct) = local_threading_construct(e) {
            if !inside_producer(e.span())
                && !construct_outer_local_writes(&construct, arm_bound).is_empty()
            {
                ok = false;
            }
        }
        if kind == RootMethodKind::ValueType && !field_writes_carried {
            if let Expression::Assignment { target, .. } = e {
                if is_self_field_target(target) {
                    ok = false;
                }
            }
        }
    });
    ok
}

/// ADR 0131 Phase 3 (BT-3738): the threaded set of a conditional that
/// codegen lowers as a local-threading producer at a method-root site
/// ([`sequenced_operand_sites`], a method-body assignment's whole value, a
/// method-body `^` value), or `None` when `expr` is not one.
///
/// The conditional counterpart of [`operand_producer_writes`], with the same
/// agreement rule: the construct writes an outer local, and the set its
/// arms pack (`threaded_today_block_writes` over [`conditional_arm_blocks`])
/// is the whole set ([`construct_outer_local_writes`]), so no write is
/// dropped. Read by the Phase 0 allow-set check; codegen's producer applies
/// the same rule through `threaded_locals_of` (`ThreadedLocals::lowered ==
/// ThreadedLocals::names`).
#[must_use]
pub fn conditional_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    let arms = conditional_arm_blocks(expr)?;
    if !conditional_arms_lower_as_producer(&arms, kind, bound_outside) {
        return None;
    }
    let construct = local_threading_construct(expr)?;
    packed_writes_agree(&construct, &arms, bound_outside)
}

/// ADR 0131 Phase 3b (BT-3817): the threaded set of an `on:do:`/`ensure:` that
/// codegen lowers as a local-threading producer at a method-root site (or in an
/// arm frame), or `None` when `expr` is not one.
///
/// The exception counterpart of [`conditional_producer_writes`], with the same
/// agreement rule: the protected body and the handler or cleanup are block
/// literals ([`exception_arm_blocks`]), the construct writes an outer local, the
/// set its blocks pack (`threaded_today_block_writes` over them) is the whole
/// set ([`construct_outer_local_writes`]) so no write is dropped, and the
/// blocks are ones the producer can lower in this kind of method
/// ([`exception_arms_lower_as_producer`]). Read by the Phase 0 allow-set check;
/// codegen's producer applies the same rule through `threaded_locals_of`
/// (`ThreadedLocals::lowered == ThreadedLocals::names`).
#[must_use]
pub fn exception_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    let arms = exception_arm_blocks(expr)?;
    if !exception_send_lowers_as_producer(expr, &arms, kind, bound_outside) {
        return None;
    }
    let construct = local_threading_construct(expr)?;
    if construct_outer_local_writes(&construct, bound_outside).is_empty() {
        // No outer local is written. A value-type exception that writes only
        // `self.field` still threads the `Self` family (its result tuple's
        // trailing slot, ADR 0122), so it is a producer with an empty set and
        // no `LocalRebind`s; anything else is not a producer. (`tryDo:`'s
        // Tier 2 block threads no field: it never is.)
        return (construct.family == LocalThreadingFamily::Exception
            && kind == RootMethodKind::ValueType
            && arms_write_self_field(&arms))
        .then(Vec::new);
    }
    packed_writes_agree(&construct, &arms, bound_outside)
}

/// ADR 0131 Phase 4 (BT-3750): whether the blocks `arms` of the exception-like
/// send `expr` ([`exception_arm_blocks`]) can be lowered by the exception
/// producer in a method of kind `kind`: [`exception_arms_lower_as_producer`]
/// for `on:do:`/`ensure:`, [`conditional_arms_lower_as_producer`] for `tryDo:`'s one block. The one
/// predicate the Phase 0 check and codegen's `exception_producer_locals` read.
#[must_use]
pub fn exception_send_lowers_as_producer(
    expr: &Expression,
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    match local_threading_construct(expr).map(|c| c.family) {
        // The block is a `BranchArm` frame whose `StateAcc` the runtime's
        // `tryDo:/2` threads, so the rule is a conditional arm's: a nested
        // construct that writes a local must itself be a producer, and a
        // value-type block writes no field (its tuple carries no `Self`).
        Some(LocalThreadingFamily::TryDo) => {
            matches!(arms, [body] if body.parameters.is_empty())
                && conditional_arms_lower_as_producer(arms, kind, bound_outside)
        }
        _ => exception_arms_lower_as_producer(arms, kind, bound_outside),
    }
}

/// Whether any statement of `arms` assigns a `self.field`.
fn arms_write_self_field(arms: &[&Block]) -> bool {
    let mut found = false;
    for arm in arms {
        for stmt in &arm.body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, .. } = e {
                    found |= is_self_field_target(target);
                }
            });
        }
    }
    found
}

/// ADR 0131 Phase 3 (BT-3738): [`operand_producer_writes`],
/// [`conditional_producer_writes`] or [`exception_producer_writes`]: the
/// threaded set of a local-threading producer at a method-root site, whichever
/// family it is.
#[must_use]
pub fn root_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    operand_producer_writes(expr, bound_outside)
        .or_else(|| conditional_producer_writes(expr, bound_outside, kind))
        .or_else(|| exception_producer_writes(expr, bound_outside, kind))
}

/// ADR 0131 Phase 2 (BT-3749): the sub-expressions of the method-body
/// statement `stmt` that codegen's sequencing rule (ADR 0118 §Decision 3,
/// `threaded_expression`) compiles ahead of their parent and splices into
/// the method body, so a local-threading producer there rebinds its locals
/// at the method root (ADR 0131 §2's `MethodBody` row).
///
/// From the value of `x := V` or `^V`, or from a statement that is a plain
/// send (not a local-threading construct, whose statement lowering compiles
/// its own operands): every receiver and non-block argument of a send that
/// is not a conditional, a cast, or a local-threading construct of another
/// family than a loop or fold (whose receivers and arguments their own
/// lowerings compile), and every element of a literal array, list or map.
/// A loop or fold reached this way is a site whose own operands are sites
/// too (a producer sequences them first; one that threads nothing compiles
/// them through ordinary expression codegen). The statement itself is not
/// a site (statement positions keep their own lowering), nor is anything
/// inside a block, a cascade, a destructuring,
/// a `match:` or an interpolation.
///
/// ADR 0131 Phase 2d (BT-3795): the value of a method-body field or
/// class-variable write `self.f := V` is a site too, and so are its operands
/// (`self.n := (xs collect: [...]) size`): the actor field-write arms and the
/// class method's class-variable write splice the producer's prelude before
/// the write. Not the value of a `^self.f := V`, nor of a write inside a block.
///
/// The Phase 0 allow-set check and codegen's producer both read this, so a
/// construct the diagnostic accepts in operand position is exactly one the
/// producer lowers.
///
/// A send whose statement lowering compiles its operands itself, without
/// sequencing them ([`operands_not_sequenced`]), is a site but its operands
/// are not: a producer there would leak its construct tuple.
#[must_use]
pub fn sequenced_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    operand_sites_of_statement(stmt, false)
}

/// ADR 0131 Phase 2d (BT-3795): the sub-expressions of the REPL input
/// statement `stmt` where a loop or fold is a local-threading producer
/// (ADR 0131 §2's `MethodBody (REPL)` row): its [`sequenced_operand_sites`],
/// less the value of `x := V` / `^V` itself and every site of a `self.f := V`
/// write.
///
/// The REPL threads a loop or fold that is the statement, or the whole value
/// of an assignment, through its own lowering (the assembler unpacks the
/// `{Value, Bindings}` tuple and adopts the bindings map, ADR 0041), and the
/// Phase 0 check accepts those through its REPL allow-set rows. Only a loop or
/// fold nested in an operand (`(xs collect: [...]) size`, `r := t + (...)`) had
/// no lowering, so that is where the producer fires. A field write has no
/// REPL meaning, so it keeps its diagnostic.
///
/// The one rule the Phase 0 check and codegen's REPL producer both read.
#[must_use]
pub fn repl_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    let inner = stmt.unwrap_parens();
    let whole_value = match inner {
        Expression::Assignment { target, value, .. } => {
            if is_self_field_target(target) {
                return Vec::new();
            }
            Some(value.unwrap_parens().span())
        }
        Expression::Return { value, .. } => Some(value.unwrap_parens().span()),
        _ => None,
    };
    sequenced_operand_sites(stmt)
        .into_iter()
        .filter(|site| Some(site.span()) != whole_value)
        .filter(|site| !is_repl_unmeasured_enumeration(site))
        .collect()
}

/// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): the threaded set of `expr`
/// when it is a local-threading producer at a REPL site (an operand site of
/// the input, [`repl_operand_sites`], or a REPL arm site,
/// [`repl_arm_producer_sites`]), or `None`.
///
/// The REPL threads its bindings map, so its producer adopts the construct's
/// next map whole whatever the family (codegen's `lower_repl_construct_tuple`):
/// a loop, fold, conditional or exception construct by the method rule
/// ([`root_producer_writes`], read as in an actor method, whose `State` the
/// bindings map plays), and a `value`-family send to a block literal with or
/// without parameters (`[:k | t := t + k] value: 4`), whose block the REPL
/// inlines over the map. A lookup is not one: the REPL keeps its §6 rejection.
/// The one rule the Phase 0 check and codegen's REPL producer both read.
#[must_use]
pub fn repl_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let inner = expr.unwrap_parens();
    let construct = local_threading_construct(inner)?;
    match construct.family {
        LocalThreadingFamily::Lookup => None,
        LocalThreadingFamily::BlockValue => {
            repl_evaluated_block(inner)?;
            let writes = construct_outer_local_writes(&construct, bound_outside);
            (!writes.is_empty()).then_some(writes)
        }
        _ => root_producer_writes(inner, bound_outside, RootMethodKind::Actor),
    }
}

/// BT-3881: whether a `value`-family send with `arity` arguments to the block
/// literal `block` may inline it: only when the arity is the block's own. A
/// block literal sent a selector of another arity (`[t := t + 1] value: 1`,
/// `[:k | t := t + k] value`) is never inlined, in the REPL (root or arm) or
/// in a class, value-type or actor method: it is applied, so it raises the
/// same structured `arity_mismatch` error as any other block called with the
/// wrong number of arguments, and its body never runs
/// ([`block_literal_of_other_arity`]). The one arity rule every inlining of a
/// block literal `value` send reads.
#[must_use]
pub fn block_literal_arity_matches(block: &Block, arity: usize) -> bool {
    block.parameters.len() == arity
}

/// BT-3884: the block literal `receiver` when it is sent the `value`-family
/// `selector` ([`is_safe_value_family_selector`]) with `arity` arguments and
/// its arity is not the send's ([`block_literal_arity_matches`]), or `None`.
/// Such a send is applied, never inlined: codegen lowers it through
/// `generate_block_arity_mismatch`, a stand-in fun of the block's own arity
/// whose application raises the structured `arity_mismatch` error, and the
/// body never runs. So it is no [`LocalThreadingFamily::BlockValue`]
/// construct ([`local_threading_construct`]), and the ADR 0133 escaping-block
/// check reports no write of it. The one predicate codegen's lowering, the
/// recognizer and the escape check read.
#[must_use]
pub fn block_literal_of_other_arity<'a>(
    receiver: &'a Expression,
    selector: &MessageSelector,
    arity: usize,
) -> Option<&'a Block> {
    // BT-3942: a parenthesized literal (`([t := t + 1]) value: 1`) is the
    // same literal, so it is applied and raises `arity_mismatch` alike.
    match receiver.unwrap_parens() {
        Expression::Block(block)
            if is_safe_value_family_selector(selector)
                && !block_literal_arity_matches(block, arity) =>
        {
            Some(block)
        }
        _ => None,
    }
}

/// BT-3823: the block of an *evaluated block* in the REPL sense: a
/// `value`-family send to a bare block literal (`[...] value`,
/// `[:k | ...] value: x`) whose arity matches the block's
/// ([`block_literal_arity_matches`]), or `None`. The REPL inlines such a block
/// over its bindings map when it writes a binding (a REPL arm); a block sent a
/// selector of another arity is applied instead, and raises `arity_mismatch`
/// at the root and in an arm alike (BT-3881). The one rule the Phase 0 check
/// and codegen read.
#[must_use]
pub fn repl_evaluated_block(expr: &Expression) -> Option<&Block> {
    local_threading_construct(expr)
        .filter(|construct| construct.family == LocalThreadingFamily::BlockValue)
        .and_then(|construct| construct.blocks.first().copied())
}

/// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): the sub-expressions of
/// `stmt`, a statement of a *REPL arm* — a block the REPL inlines over its
/// bindings map (the arms of a conditional, the protected body, handler and
/// cleanup of an `on:do:`/`ensure:`, the block of a `Result tryDo:`) — where a
/// local-threading producer adopts that map: the statement's
/// [`sequenced_operand_sites`], and the statement itself when it is a
/// local-threading construct ([`local_threading_construct`]; whether it is a
/// producer there is [`repl_producer_writes`]). The arm's `StateAcc` *is* the
/// bindings map, so a producer there continues it exactly as one at an operand
/// site of the input does (codegen's `lower_repl_construct_tuple`).
///
/// Unlike the input root ([`repl_operand_sites`]), the whole value of an
/// assignment is a site (no assembler unpacks it in an arm) and so are
/// `eachWithIndex:`/`do:separatedBy:` (measured right in every REPL arm,
/// BT-3823). The value of a `self.f := V` write is not: a field write has no
/// REPL meaning.
///
/// The one rule the Phase 0 check and codegen's REPL arms
/// (`with_repl_arm_producer_sites`) both read.
#[must_use]
pub fn repl_arm_producer_sites(stmt: &Expression) -> Vec<&Expression> {
    let inner = stmt.unwrap_parens();
    if let Expression::Assignment { target, .. } = inner {
        if is_self_field_target(target) {
            return Vec::new();
        }
    }
    let mut sites = sequenced_operand_sites(stmt);
    if local_threading_construct(inner).is_some() {
        sites.push(inner);
    }
    sites
}

/// BT-3841: `eachWithIndex:`/`do:separatedBy:` became producers for class,
/// value-type and actor methods; the REPL's operand sites keep excluding them
/// (they keep their statement and assignment lowering and the Phase 0 operand
/// rows), because the REPL root's fold frames are unmeasured (BT-3876): the
/// desugared fold's separator conditional fails the `ThreadedIr` verifier there.
/// (In a REPL arm, [`repl_arm_producer_sites`], both are producers and answer
/// right, BT-3823.)
fn is_repl_unmeasured_enumeration(site: &Expression) -> bool {
    matches!(
        site.unwrap_parens(),
        Expression::MessageSend { selector, .. }
            if matches!(selector.name().as_str(), "eachWithIndex:" | "do:separatedBy:")
    )
}

/// ADR 0131 Phase 2b (BT-3793): [`sequenced_operand_sites`] for a statement
/// of a loop or fold body, which codegen compiles through
/// `threaded_expression` (`thread_ahead`) first even when the statement is
/// itself a local-threading construct: its own receiver and non-block
/// arguments are then operand sites too (`(xs collect: [:x | t := t + x])
/// inject: 0 into: [:a :x | a + x]` as a statement of a body). A method-body
/// statement that is a construct keeps its own statement lowering instead,
/// which compiles those operands itself ([`sequenced_operand_sites`]).
#[must_use]
pub fn loop_body_operand_sites(stmt: &Expression) -> Vec<&Expression> {
    operand_sites_of_statement(stmt, true)
}

/// ADR 0131 Phase 2b (BT-3793): the sub-expressions of `stmt`, a statement of
/// a loop or fold body, where a local-threading producer rebinds its locals
/// in that body's frame: the statement itself when it is a loop or fold
/// ([`producer_body_block`]), and its [`loop_body_operand_sites`]. The one
/// rule the Phase 0 allow-set check and codegen's body lowerings
/// (`with_body_producer_sites`) both read, so a construct the diagnostic
/// accepts in a loop body is exactly one the producer lowers.
#[must_use]
pub fn loop_body_producer_sites(stmt: &Expression) -> Vec<&Expression> {
    let mut sites = loop_body_operand_sites(stmt);
    let inner = stmt.unwrap_parens();
    // (A conditional statement is a site too, but only in an arm frame:
    // [`is_loop_body_producer_site`] drops it in a loop or fold body.)
    if producer_body_block(inner).is_some()
        || conditional_arm_blocks(inner).is_some()
        || exception_arm_blocks(inner).is_some()
    {
        sites.push(inner);
    }
    sites
}

/// ADR 0131 Phase 2b (BT-3793/BT-3797): whether `site`, one of a loop or
/// fold body statement's [`loop_body_producer_sites`], carries a value (an
/// assignment's value or an operand) rather than being the statement
/// itself.
#[must_use]
pub fn is_value_bearing_site(statement: Span, site: &Expression) -> bool {
    site.unwrap_parens().span() != statement
}

/// What the loop-body site rule ([`is_loop_body_producer_site`]) reads off the
/// current statement of a loop or fold body and the frames around it.
#[derive(Debug, Clone, Copy, Default)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "one flag per fact the site rule reads"
)]
pub struct LoopBodyContext {
    /// The statement, without parentheses.
    pub statement: Span,
    /// The statement's whole value: the value of `x := V` or `^V`, without
    /// parentheses, else the statement itself.
    pub value: Span,
    /// Whether the innermost enclosing frame is a fold's
    /// ([`is_fold_frame_body`]).
    pub innermost_fold: bool,
    /// Whether an enclosing frame other than the innermost one is a fold's.
    pub outer_fold: bool,
    /// Whether the method is an actor instance method.
    pub in_actor: bool,
    /// ADR 0131 Phase 3 (BT-3738): whether the innermost enclosing frame is
    /// an arm of a conditional that is a local-threading producer
    /// (`BranchArm`, [`conditional_arms_lower_as_producer`]) rather than a
    /// loop or fold body. A nested conditional is a producer in an arm frame,
    /// not in a loop body.
    pub arm: bool,
}

impl LoopBodyContext {
    /// This context for a statement of an arm frame ([`Self::arm`]).
    #[must_use]
    pub fn in_arm(mut self) -> Self {
        self.arm = true;
        self
    }

    /// The context of the statement `stmt` of a frame body.
    #[must_use]
    pub fn new(stmt: &Expression, innermost_fold: bool, outer_fold: bool, in_actor: bool) -> Self {
        let statement = stmt.unwrap_parens();
        let value = match statement {
            Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
                value.unwrap_parens().span()
            }
            other => other.span(),
        };
        Self {
            statement: statement.span(),
            value,
            innermost_fold,
            outer_fold,
            in_actor,
            arm: false,
        }
    }
}

/// ADR 0131 Phase 2c (BT-3796, from the BT-3793/BT-3797 reviews): whether
/// `site`, one of the statement's [`loop_body_producer_sites`], is a
/// value-bearing site that an actor method's fold frame does not lower
/// through the producer.
///
/// An actor fold's accumulator is a tuple the construct's carrier does not
/// fit: `r := xs inject: 0 into: [...]` in a `do:` body compiled and then
/// crashed (BT-3797). So every value-bearing site (an assignment's value or an
/// operand, [`is_value_bearing_site`]) of a statement directly in a fold body
/// is out. Under a letrec loop nested in a fold (BT-3796 probe) the whole value
/// of an assignment is lowered, but an operand of a larger expression
/// (`#[xs do: [...], t]`, `(xs do: [...]) printString`) wrote nothing back or
/// crashed, so operands are out under *any* enclosing fold frame. A letrec
/// loop frame alone lowers every value-bearing site.
#[must_use]
pub fn is_actor_fold_value_site(site: &Expression, ctx: &LoopBodyContext) -> bool {
    if !ctx.in_actor || !is_value_bearing_site(ctx.statement, site) {
        return false;
    }
    ctx.innermost_fold || (ctx.outer_fold && site.unwrap_parens().span() != ctx.value)
}

/// ADR 0131 Phase 2b/2c (BT-3796): whether `site`, one of the statement's
/// [`loop_body_producer_sites`], is lowered as a local-threading producer. The
/// one narrowing of that site list, read by the Phase 0 check, by codegen's
/// `with_body_producer_sites` and by the agreement test, so the diagnostic, the
/// lowering and the test's expectation cannot disagree:
///
/// - [`is_actor_fold_value_site`].
///
/// `detect:`/`detect:ifNone:` are loop-body producers in every method kind
/// (BT-3841: the fold stops at the first match, BT-3805). In an actor method
/// they were excluded here (BT-3793) while the frames around them dropped or
/// miscompiled the write; ADR 0133 Phase 3 (BT-3911, closing BT-3878 and
/// BT-3861) lowers them like any other fold.
///
/// A conditional, an `on:do:`/`ensure:` or a `Dictionary` lookup is a producer
/// in a loop or fold body (ADR 0131 Phase 3b/3c, BT-3817/BT-3822/BT-3839)
/// wherever the frame has a site for it.
#[must_use]
pub fn is_loop_body_producer_site(site: &Expression, ctx: &LoopBodyContext) -> bool {
    // ADR 0131 Phase 3 (BT-3738) / Phase 3c (BT-3822): a conditional is a
    // producer at a method-root site, in an arm of a conditional that is one
    // (`ctx.arm`) and in a loop or fold body (the frame lowers its
    // `LocalRebind`s by the frame's mode, ADR 0131 §2), like an
    // `on:do:`/`ensure:` (Phase 3b, BT-3817). Whether a given conditional is a
    // producer is `conditional_producer_writes`'s question, not the site's.
    //
    // BT-3839: a `Dictionary` lookup (Phase 2d) is a conditional in the core
    // rule's eyes, and lowers by the frame's mode like one. (Codegen's loop
    // gate sees the handler's write through `threaded_locals_of`, so the
    // enclosing loop or fold threads it in an actor too.)
    !is_actor_fold_value_site(site, ctx)
}

/// Whether the literal block `block`, an argument of the send `expr`, is the
/// body of a loop or fold that codegen lowers as a threaded frame
/// (`lower_letrec_body`/`lower_foldl_body`), so its statements are
/// [`loop_body_producer_sites`]: a block argument of a loop or fold family
/// construct ([`local_threading_construct`]) at an index
/// [`crate::state_threading_selectors::state_threaded_block_arg_indices`]
/// lists. Not a `whileTrue:`/`whileFalse:` condition (the receiver), not
/// `detect:ifNone:`'s handler, not an arm, a handler or a protected body.
#[must_use]
pub fn is_threaded_frame_body(expr: &Expression, block: &Block) -> bool {
    let Some(construct) = local_threading_construct(expr) else {
        return false;
    };
    if !matches!(
        construct.family,
        LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
    ) {
        return false;
    }
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return false;
    };
    crate::state_threading_selectors::state_threaded_block_arg_indices(&construct.selector)
        .iter()
        .any(|&i| matches!(arguments.get(i), Some(Expression::Block(b)) if std::ptr::eq(b, block)))
}

/// ADR 0131 Phase 2b (BT-3797): whether `block` is the body of a *fold*
/// frame ([`is_threaded_frame_body`] with a `LocalThreadingFamily::Fold`
/// construct: `do:`, `collect:`, `inject:into:`, …), as opposed to a
/// `whileTrue:`/counted-loop letrec frame. An actor's fold frame carries
/// its locals in a tuple accumulator, which a value-bearing producer in the
/// body does not lower through.
#[must_use]
pub fn is_fold_frame_body(expr: &Expression, block: &Block) -> bool {
    is_threaded_frame_body(expr, block)
        && local_threading_construct(expr)
            .is_some_and(|c| matches!(c.family, LocalThreadingFamily::Fold))
}

fn operand_sites_of_statement(stmt: &Expression, construct_operands: bool) -> Vec<&Expression> {
    fn operands<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
        match expr.unwrap_parens() {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                ..
            } => {
                let nested_construct = local_threading_construct(expr).is_some_and(|c| {
                    !matches!(
                        c.family,
                        LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                    )
                });
                if nested_construct
                    || crate::state_threading_selectors::is_conditional_selector(&selector.name())
                    || operands_not_sequenced(receiver, selector, arguments)
                {
                    return;
                }
                if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_none() {
                    site(receiver, out);
                }
                for arg in arguments {
                    if !matches!(arg, Expression::Block(_)) {
                        site(arg, out);
                    }
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    site(e, out);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    site(e, out);
                }
                if let Some(t) = tail {
                    site(t, out);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    site(&pair.key, out);
                    site(&pair.value, out);
                }
            }
            _ => {}
        }
    }
    fn site<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
        let inner = expr.unwrap_parens();
        if matches!(inner, Expression::Block(_)) {
            return;
        }
        out.push(inner);
        operands(inner, out);
    }
    let mut out = Vec::new();
    match stmt.unwrap_parens() {
        Expression::Assignment { target, value, .. }
            if matches!(target.as_ref(), Expression::Identifier(_)) =>
        {
            site(value, &mut out);
        }
        // ADR 0131 Phase 2d (BT-3795): the value of a method-body field or
        // class-variable write `self.f := V`. Its three consumers (the actor
        // `FieldAssignment`/`FieldAssignmentControlFlow` arms and the class
        // method's `class_method_prelude_producer`) sequence `V` through
        // `threaded_expression` before the write. ADR 0131 Phase 3b (BT-3817):
        // also a statement of a loop, fold or arm frame body
        // (`construct_operands`): the frame's field-write lowering
        // (`lower_field_assignment_bind`, the class-variable write's
        // `thread_ahead`) splices the value's prelude ahead of the write too.
        Expression::Assignment { target, value, .. } if is_self_field_target(target) => {
            site(value, &mut out);
        }
        Expression::Return { value, .. } => site(value, &mut out),
        other if !construct_operands && local_threading_construct(other).is_some() => {}
        other => operands(other, &mut out),
    }
    out
}

/// Whether `target` is `self.<field>`, the target of a field or class-variable
/// write: the one shape the field-write rules share (here, and codegen's
/// `field_assignment_name` and `class_var_assignment_shape`).
#[must_use]
pub fn is_self_field_target(target: &Expression) -> bool {
    matches!(
        target,
        Expression::FieldAccess { receiver, .. }
            if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
    )
}

/// ADR 0131 Phase 2 (BT-3749): whether the send `receiver selector: arguments`
/// is lowered by a path that compiles its operands directly instead of
/// sequencing them through `threaded_expression`, so a local-threading
/// producer among them would leak its raw `{Value, State}` tuple and drop the
/// write. Those are the actor body lowerings of a `super` send, an `error:`
/// send, `self fieldAt:put:` / `self clearField:`, a block `value`-family send
/// (`value:`, `valueWithArguments:`; a possible Tier 2 call on a local or a
/// `self.field`) and a self-send carrying a block argument (a
/// possible Tier 2 self-send). Applied in every context, so the diagnostic does
/// not depend on which method kind it sits in.
///
/// The `is_block_invocation()` stop is deliberately an over-approximation for
/// **any receiver**: only a Tier 2 call on a bare local or a `self.field`
/// takes the unsequenced actor lowering, but a `value`-family or
/// `valueWithArguments:` send stops the sites whatever its receiver is (a
/// literal block, a call result, a parenthesised expression). Deciding from
/// the receiver would duplicate codegen's `is_tier2_value_call`, and the two
/// could drift. A producer the over-approximation leaves out of the sites is
/// never a miscompile: it keeps its Phase 0 diagnostic.
#[must_use]
fn operands_not_sequenced(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    use crate::ast::WellKnownSelector;
    let is_self = matches!(receiver, Expression::Identifier(id) if id.name == "self");
    matches!(receiver, Expression::Super(_))
        || matches!(
            selector.well_known(),
            Some(WellKnownSelector::Error)
                if arguments.len() == 1
        )
        || (is_self
            && matches!(
                selector.well_known(),
                Some(WellKnownSelector::FieldAtPut | WellKnownSelector::ClearField)
            ))
        || selector.is_block_invocation()
        || (is_self && arguments.iter().any(|a| matches!(a, Expression::Block(_))))
}

fn walk_construct_blocks(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
    nested: NestedBlocks,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested,
    };
    for block in blocks {
        walker.block(block);
    }
    walker.writes
}

// ---------------------------------------------------------------------------
// Tier 2 stored-block promotion facts (ADR 0041). Moved from beamtalk-codegen's
// `gen_server/methods.rs` (BT-3745) so codegen's `prescan_tier2_local_vars` and
// the ADR 0131 §6 check (`validators/local_threading.rs`) share one
// implementation of "which uses of a stored Tier 2 block are safe".
// ---------------------------------------------------------------------------

/// The outer locals a block literal both reads and writes (`local_writes` ∩
/// `captured_reads` of its [`analyze_block`] analysis), sorted: the set codegen's Tier 2
/// block protocol threads back (`captured_mutations_for_block`). A block whose
/// outer-local write is not in this set (a write-only write, or one this
/// analysis does not propagate out of a nested block) is not promoted, and
/// the write is lost.
#[must_use]
pub fn captured_local_mutations_from_analysis(analysis: &BlockMutationAnalysis) -> Vec<String> {
    analysis
        .local_writes
        .intersection(&analysis.captured_reads)
        .cloned()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// Normalizes a `Cascade` into its true underlying receiver and the
/// full ordered list of messages sent to it.
///
/// The parser (`parse_cascade`) folds the cascade's *first* message into
/// `Cascade.receiver` as a whole `MessageSend` — e.g. for `blk value: x;
/// value: y`, `receiver` is `MessageSend(blk, value:, [x])` and `messages`
/// holds only the remaining `value: y`. Every safety/codegen decision needs
/// the TRUE receiver (`blk`) and ALL messages sent to it (both `value: x`
/// and `value: y`), so this mirrors the same normalization
/// `generate_cascade` (expressions.rs) already performs for ordinary
/// (non-Tier-2) cascade codegen.
#[must_use]
pub fn normalize_cascade<'a>(
    receiver: &'a Expression,
    messages: &'a [crate::ast::CascadeMessage],
) -> (&'a Expression, Vec<(&'a MessageSelector, &'a [Expression])>) {
    if let Expression::MessageSend {
        receiver: inner,
        selector: first_selector,
        arguments: first_arguments,
        ..
    } = receiver
    {
        let mut all: Vec<(&MessageSelector, &[Expression])> =
            Vec::with_capacity(messages.len() + 1);
        all.push((first_selector, first_arguments.as_slice()));
        for msg in messages {
            all.push((&msg.selector, msg.arguments.as_slice()));
        }
        (inner.as_ref(), all)
    } else {
        let all: Vec<(&MessageSelector, &[Expression])> = messages
            .iter()
            .map(|msg| (&msg.selector, msg.arguments.as_slice()))
            .collect();
        (receiver, all)
    }
}

/// Returns true if `selector` is a `value`/`value:`/
/// `value:value:`/`value:value:value:` send — the "safe" family that lets a
/// Tier 2 block value be invoked without escaping to a call site that
/// doesn't know to thread state through it.
#[must_use]
pub fn is_safe_value_family_selector(selector: &MessageSelector) -> bool {
    selector
        .well_known()
        .is_some_and(crate::ast::WellKnownSelector::is_block_value)
}

/// Scans `expr` for references to `var_name`, returning
/// `(has_unsafe_use, has_safe_use)`.
///
/// A *safe* use is the receiver of a `value`/`value:`/`value:value:`/
/// `value:value:value:` send. Any other reference — a bare return, an
/// argument to another call, a reassignment, ... — is *unsafe*, since it
/// would let a Tier 2 block value escape to a call site that doesn't know
/// to thread state through it. A variable that's *never* referenced at
/// all yields `(false, false)`, which the caller must treat as unsafe
/// (not "no unsafe use found") — see `prescan_tier2_local_vars`.
///
/// Deliberately conservative: exhaustively matches every `Expression`
/// variant so a use hidden inside e.g. a map literal or string
/// interpolation is never silently missed. A shadowing block parameter
/// with the same name is *not* special-cased — that only makes this
/// over-conservative (a missed promotion), never unsafe.
#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over every Expression variant, kept as one function for locality with its single caller"
)]
#[must_use]
pub fn stored_block_var_uses(expr: &Expression, var_name: &str) -> (bool, bool) {
    match expr {
        Expression::Identifier(id) => (id.name == var_name, false),
        Expression::Literal(..)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. } => (false, false),
        Expression::Spread { name, .. } => (name.name == var_name, false),
        Expression::FieldAccess { receiver, .. } => stored_block_var_uses(receiver, var_name),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let is_safe_value_send = matches!(
                receiver.as_ref(),
                Expression::Identifier(id) if id.name == var_name
            ) && is_safe_value_family_selector(selector);
            let (mut unsafe_, mut safe) = if is_safe_value_send {
                (false, true)
            } else {
                stored_block_var_uses(receiver, var_name)
            };
            for arg in arguments {
                let (u, s) = stored_block_var_uses(arg, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::Block(block) => {
            // Any reference to var_name inside a nested block literal is
            // unsafe — see the safety invariant note on
            // prescan_tier2_local_vars above (a nested block compiles
            // through a completely different path with no Tier2-tuple
            // unpacking and no tier2_local_vars reset of its own).
            let (any_unsafe, any_safe) = block
                .body
                .iter()
                .map(|stmt| stored_block_var_uses(&stmt.expression, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            (any_unsafe || any_safe, false)
        }
        Expression::Assignment { target, value, .. } => {
            let (u1, s1) = stored_block_var_uses(target, var_name);
            let (u2, s2) = stored_block_var_uses(value, var_name);
            (u1 || u2, s1 || s2)
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            stored_block_var_uses(value, var_name)
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // When the cascade's true underlying receiver (see
            // `normalize_cascade`) *is* var_name itself (e.g. `blk value: x;
            // value: y`), the generic recursive scan would hit the plain
            // `Identifier` arm and unconditionally report it unsafe. Mirror the
            // `MessageSend` arm's `is_safe_value_send` check instead: if EVERY
            // message sent to that receiver (including the one folded into
            // `receiver` by the parser) is itself a safe
            // `value`/`value:`/`value:value:`/`value:value:value:` send, the
            // whole cascade is as safe as a single safe value send would be.
            let (underlying_receiver, all_messages) = normalize_cascade(receiver, messages);
            let receiver_is_var = matches!(
                underlying_receiver,
                Expression::Identifier(id) if id.name == var_name
            );
            let all_messages_safe_value_sends = receiver_is_var
                && !all_messages.is_empty()
                && all_messages
                    .iter()
                    .all(|(sel, _)| is_safe_value_family_selector(sel));
            let (mut unsafe_, mut safe) = if all_messages_safe_value_sends {
                (false, true)
            } else {
                stored_block_var_uses(underlying_receiver, var_name)
            };
            for (_, args) in &all_messages {
                for arg in *args {
                    let (u, s) = stored_block_var_uses(arg, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
            }
            (unsafe_, safe)
        }
        Expression::Parenthesized { expression, .. } => stored_block_var_uses(expression, var_name),
        Expression::Match { value, arms, .. } => {
            let (mut unsafe_, mut safe) = stored_block_var_uses(value, var_name);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    let (u, s) = stored_block_var_uses(guard, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                let (u, s) = stored_block_var_uses(&arm.body, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .map(|pair| {
                let (u1, s1) = stored_block_var_uses(&pair.key, var_name);
                let (u2, s2) = stored_block_var_uses(&pair.value, var_name);
                (u1 || u2, s1 || s2)
            })
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        Expression::ListLiteral { elements, tail, .. } => {
            let (mut unsafe_, mut safe) = elements
                .iter()
                .map(|e| stored_block_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            if let Some(t) = tail {
                let (u, s) = stored_block_var_uses(t, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            elements
                .iter()
                .map(|e| stored_block_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2))
        }
        Expression::StringInterpolation { segments, .. } => segments
            .iter()
            .map(|seg| match seg {
                crate::ast::StringSegment::Interpolation(e) => stored_block_var_uses(e, var_name),
                crate::ast::StringSegment::Literal(_) => (false, false),
            })
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
    }
}

// --- ADR 0131 §6 actor channel (BT-3755): which actor self-send callees thread a Tier 2 block ---
//
// Moved here from the deleted `tier2_callee` module (ADR 0133 Phase 2,
// BT-3906) with its one surviving caller, codegen's class pre-scan
// (`scan_class_for_tier2_blocks`), which asks [`callee_threads_block_param`]
// and [`passed_on_targets`]. Since ADR 0133 (BT-3902) the §6 diagnostic no
// longer reads this predicate: it rejects every escaping writing block, so a
// captured-local write reaches the promotion only from codegen tests; a
// field-writing block (BT-3922, not yet rejected) still reaches it through
// [`passed_on_targets`].
//
// `self ap: [t := t + 1]` in an actor instance method is compiled by codegen's
// `detect_tier2_self_send`: the block becomes a Tier 2 closure
// (`fun(Args, StateAcc) -> {Result, StateAcc}`), its writes are packed into
// the actor state around the dispatch, and the callee `ap:` is compiled with
// the matching parameter as a Tier 2 block parameter. That only answers right
// when the callee actually hands the `StateAcc` of the block back.
//
// A callee threads parameter `i` of selector `sel` in class `C` when:
//
// 1. `C` itself defines `sel` as an instance method (a callee inherited from
//    another class is compiled with its parameter as a plain Tier 1 block and
//    raises on a Tier 2 closure);
// 2. no subclass can replace it: `C` or the method is `sealed` (an actor
//    self-send late-binds through the receiver's own class module, and an
//    override is compiled without a call site, so its parameter is Tier 1);
// 3. every use of the parameter in the method body is one of the shapes
//    codegen unpacks, and no other use exists (a use inside another block, a
//    conditional or loop outside these shapes, an operand, a store, a return
//    of the block itself, or an argument of anything else drops the write or
//    raises):
//    - a **direct invocation** (`b value`, `b value: x`, a `value`-family
//      cascade) written as a method-body statement, the right-hand side of a
//      body-level `x := ...` or the value of a body-level `^...`;
//    - the same direct invocation as the right-hand side of `x := ...` in the
//      body of a statement-level `do:` loop, where `x` is a local bound
//      before the loop;
//    - the **callable argument of a collection HOM**
//      ([`is_opaque_callable_hom_send`]; codegen lowered it through the
//      ADR 0128 opaque-callable fold until ADR 0133 removed that fold,
//      BT-3908), in operand position of a
//      method-body statement or inside the branches of a conditional, the
//      body of `to:do:` or the condition of `whileTrue:`/`whileFalse:`;
//    - passed as a bare argument of a method-body self-send to another
//      callee of the same class that itself threads that parameter.
//
// A cycle of such forwards (self-recursion, mutual recursion) does not thread
// (BT-3922 makes these field-writing self-send shapes a compile error).

/// The instance method of `class` that a self-send of `selector` resolves to,
/// when `class` itself defines it.
fn same_class_callee<'a>(
    class: &'a ClassDefinition,
    selector: &str,
) -> Option<&'a MethodDefinition> {
    class
        .methods
        .iter()
        .find(|m| !m.is_class_method && m.selector.name() == selector)
}

/// Whether `class`'s instance method `selector` threads the `StateAcc` of the
/// Tier 2 block passed as its parameter number `index` (see the section docs).
#[must_use]
pub fn callee_threads_block_param(class: &ClassDefinition, selector: &str, index: usize) -> bool {
    threads(class, selector, index, &mut Vec::new())
}

/// The `(selector, parameter index)` pairs of `class` that the Tier 2 block
/// passed as parameter `index` of `selector` is passed on to as a bare
/// argument of a method-body self-send. Codegen compiles each of them with a
/// Tier 2 block parameter too (BT-3755). Empty if `selector` is not a
/// threading callee.
#[must_use]
pub fn passed_on_targets(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
) -> Vec<(EcoString, usize)> {
    let mut targets = Vec::new();
    let mut seen = HashSet::new();
    collect_targets(class, selector, index, &mut seen, &mut targets);
    targets
}

fn collect_targets(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
    seen: &mut HashSet<(EcoString, usize)>,
    targets: &mut Vec<(EcoString, usize)>,
) {
    if !callee_threads_block_param(class, selector, index) || !seen.insert((selector.into(), index))
    {
        return;
    }
    let Some(method) = same_class_callee(class, selector) else {
        return;
    };
    let Some(param) = method.parameters.get(index) else {
        return;
    };
    for stmt in &method.body {
        if let Some((target, position)) =
            pass_on(peel(&stmt.expression, &param.name.name), &param.name.name)
        {
            targets.push((target.clone(), position));
            collect_targets(class, &target, position, seen, targets);
        }
    }
}

fn threads(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
    visiting: &mut Vec<(EcoString, usize)>,
) -> bool {
    let Some(method) = same_class_callee(class, selector) else {
        return false;
    };
    if !(class.is_sealed || method.is_sealed) {
        return false;
    }
    let Some(param) = method.parameters.get(index) else {
        return false;
    };
    let key = (EcoString::from(selector), index);
    if visiting.contains(&key) {
        return false;
    }
    visiting.push(key);
    let ctx = Ctx {
        class,
        method,
        name: param.name.name.as_str(),
    };
    let ok = method
        .body
        .iter()
        .enumerate()
        .all(|(i, stmt)| ctx.statement_ok(i, &stmt.expression, visiting));
    visiting.pop();
    ok
}

struct Ctx<'a> {
    class: &'a ClassDefinition,
    method: &'a MethodDefinition,
    name: &'a str,
}

impl Ctx<'_> {
    fn mentions(&self, expr: &Expression) -> bool {
        stored_block_var_uses(expr, self.name) != (false, false)
    }

    /// Whether method-body statement number `index` uses the parameter only
    /// in a shape the section docs list (or not at all).
    fn statement_ok(
        &self,
        index: usize,
        statement: &Expression,
        visiting: &mut Vec<(EcoString, usize)>,
    ) -> bool {
        if !self.mentions(statement) {
            return true;
        }
        let inner = peel(statement, self.name);
        if self.is_direct_invocation(inner) {
            return true;
        }
        if let Some((target, position)) = pass_on(inner, self.name) {
            return threads(self.class, &target, position, visiting);
        }
        if self.is_stateful_do_loop(index, statement) {
            return true;
        }
        self.opaque_ok(statement)
    }

    /// `name value...` or a `value`-family cascade on `name`, whose arguments
    /// do not mention `name`.
    fn is_direct_invocation(&self, expr: &Expression) -> bool {
        direct_invocation(expr, self.name)
    }

    /// `items do: [:e | ... x := name value: e ...]` as a method-body
    /// statement, where `x` is bound before the loop. Each loop-body statement
    /// that mentions `name` must be such an `x := name value: ...` assignment
    /// (`x` neither `name` nor a loop parameter); statements that do not
    /// mention `name` are unconstrained. The caller ([`Self::statement_ok`])
    /// checks the method's other statements separately.
    fn is_stateful_do_loop(&self, index: usize, statement: &Expression) -> bool {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast: false,
            ..
        } = statement
        else {
            return false;
        };
        let [Expression::Block(body)] = arguments.as_slice() else {
            return false;
        };
        if selector.name() != "do:"
            || self.mentions(receiver)
            || !is_threaded_frame_body(statement, body)
            || body.parameters.iter().any(|p| p.name.as_str() == self.name)
        {
            return false;
        }
        let bound_before = |x: &str| {
            self.method.parameters.iter().any(|p| p.name.name == x)
                || self.method.body[..index].iter().any(|s| {
                    matches!(&s.expression, Expression::Assignment { target, .. }
                        if matches!(target.as_ref(), Expression::Identifier(id) if id.name == x))
                })
        };
        body.body.iter().all(|stmt| {
            if !self.mentions(&stmt.expression) {
                return true;
            }
            let Expression::Assignment { target, value, .. } = &stmt.expression else {
                return false;
            };
            let Expression::Identifier(x) = target.as_ref() else {
                return false;
            };
            x.name != self.name
                && !body.parameters.iter().any(|p| p.name == x.name)
                && bound_before(&x.name)
                && self.is_direct_invocation(value)
        })
    }

    /// Whether every mention of the parameter in `expr` is the callable
    /// argument of an opaque-callable collection HOM ([`is_opaque_callable_hom_send`])
    /// reached through the positions the section docs list.
    fn opaque_ok(&self, expr: &Expression) -> bool {
        if !self.mentions(expr) {
            return true;
        }
        match expr {
            Expression::Parenthesized { expression, .. } => self.opaque_ok(expression),
            Expression::Return { value, .. } => self.opaque_ok(value),
            Expression::Assignment { target, value, .. } => {
                !self.mentions(target) && self.opaque_ok(value)
            }
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                ..
            } => {
                let sel = selector.name();
                if let Some(ok) = callable_hom_use(expr, self.name) {
                    return ok;
                }
                match (sel.as_str(), receiver.as_ref()) {
                    ("ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifFalse:ifTrue:", _) => {
                        self.opaque_ok(receiver) && arguments.iter().all(|a| self.branch_ok(a))
                    }
                    ("to:do:", _) => {
                        !self.mentions(receiver)
                            && match arguments.as_slice() {
                                [limit, body] => !self.mentions(limit) && self.branch_ok(body),
                                _ => false,
                            }
                    }
                    ("whileTrue:" | "whileFalse:", Expression::Block(condition)) => {
                        self.block_ok(condition) && arguments.iter().all(|a| !self.mentions(a))
                    }
                    _ => {
                        self.opaque_ok(receiver)
                            && arguments
                                .iter()
                                .all(|a| !matches!(a, Expression::Block(_)) && self.opaque_ok(a))
                    }
                }
            }
            _ => false,
        }
    }

    fn branch_ok(&self, arg: &Expression) -> bool {
        match arg {
            Expression::Block(block) => self.block_ok(block),
            other => !self.mentions(other),
        }
    }

    fn block_ok(&self, block: &Block) -> bool {
        !block
            .parameters
            .iter()
            .any(|p| p.name.as_str() == self.name)
            && block
                .body
                .iter()
                .all(|s: &ExpressionStatement| self.opaque_ok(&s.expression))
    }
}

/// `name <invocation>...` or a cascade of `value`-family invocations on
/// `name` ([`is_safe_value_family_selector`]), where no argument mentions
/// `name`.
fn direct_invocation(expr: &Expression, name: &str) -> bool {
    let accepts = is_safe_value_family_selector;
    let mentions = |e: &Expression| stored_block_var_uses(e, name) != (false, false);
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => is_name(receiver, name) && accepts(selector) && !arguments.iter().any(mentions),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            let (underlying, all) = normalize_cascade(receiver, messages);
            is_name(underlying, name)
                && !all.is_empty()
                && all
                    .iter()
                    .all(|(sel, args)| accepts(sel) && !args.iter().any(mentions))
        }
        _ => false,
    }
}

/// `Some(ok)` when `expr` is a collection-HOM send ([`is_opaque_callable_hom_send`])
/// whose callable argument is exactly the identifier `name`; `ok` is whether
/// that is the parameter's only mention (neither the receiver nor any other
/// argument, a seed included, mentions it). `None` for any other expression.
/// The callable argument is excluded by identity, not by name, so
/// `items inject: b into: b` is not accepted.
fn callable_hom_use(expr: &Expression, name: &str) -> Option<bool> {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    if !is_opaque_callable_hom_send(expr) {
        return None;
    }
    let callable = opaque_fold_callable_arg(&selector.name(), arguments)?;
    if !is_name(callable, name) {
        return None;
    }
    let mentions = |e: &Expression| stored_block_var_uses(e, name) != (false, false);
    Some(
        !mentions(receiver)
            && arguments
                .iter()
                .filter(|a| !std::ptr::eq(*a, callable))
                .all(|a| !mentions(a)),
    )
}

fn is_name(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == name)
}

/// `expr` without a body-level `x := ...` (for an `x` other than `name`) or
/// `^...` wrapper.
fn peel<'a>(expr: &'a Expression, name: &str) -> &'a Expression {
    match expr {
        Expression::Assignment { target, value, .. } if matches!(target.as_ref(), Expression::Identifier(id) if id.name != name) => {
            value.as_ref()
        }
        Expression::Return { value, .. } => value.as_ref(),
        other => other,
    }
}

/// `self target: ... name ...` where `name` is a bare argument (at position
/// `j`) and no other argument mentions it.
fn pass_on(expr: &Expression, name: &str) -> Option<(EcoString, usize)> {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expr
    else {
        return None;
    };
    if !is_name(receiver, "self") {
        return None;
    }
    let mut positions = arguments
        .iter()
        .enumerate()
        .filter(|(_, a)| is_name(a, name))
        .map(|(i, _)| i);
    let position = positions.next()?;
    if positions.next().is_some()
        || arguments
            .iter()
            .enumerate()
            .any(|(i, a)| i != position && stored_block_var_uses(a, name) != (false, false))
    {
        return None;
    }
    Some((selector.name(), position))
}

/// Checks if a block is a literal block (not a variable reference).
#[cfg(test)]
pub fn is_literal_block(expr: &Expression) -> bool {
    matches!(expr, Expression::Block(_))
}

/// Checks if a message send is a control flow construct with a literal block.
#[cfg(test)]
pub fn is_control_flow_construct(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    match selector {
        MessageSelector::Keyword(parts) => {
            let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

            match selector_name.as_str() {
                // whileTrue: / whileFalse: - block receiver + literal block arg
                "whileTrue:" | "whileFalse:" => {
                    is_literal_block(receiver) && arguments.first().is_some_and(is_literal_block)
                }

                // timesRepeat: - integer receiver + literal block
                "timesRepeat:" => arguments.first().is_some_and(is_literal_block),

                // to:do: and inject:into: - literal block as second arg
                "to:do:" | "inject:into:" => arguments.get(1).is_some_and(is_literal_block),

                // to:by:do: - literal block as third arg
                "to:by:do:" => arguments.get(2).is_some_and(is_literal_block),

                // Collection iteration: do:, collect:, select:, reject: - literal block as first arg
                "do:" | "collect:" | "select:" | "reject:" => {
                    arguments.first().is_some_and(is_literal_block)
                }

                _ => false,
            }
        }
        _ => false,
    }
}

/// BT-3910 (ADR 0131 §1): every local (identifier-target) assignment in `body`
/// that sits in *value position*: a producer, lowered as a `LocalRebind` of the
/// enclosing frame. `root` is the kind of method `body` is the body of, or
/// `None` for a top-level expression list (a REPL input or a script).
///
/// A local assignment is in statement position when it is, bare, a statement
/// of a body: a method body, a block body or a top-level expression list.
/// Every body lowering binds the local there. Parentheses around a statement
/// that is not the body's last are unwrapped only at the root of a value-type
/// or actor method (BT-3830, BT-3838: `(t := t + 1)` followed by `t`) and in
/// an inlined arm ([`conditional_arm_blocks`]: a conditional's arm, a lookup
/// handler, a `[...] value` body); a method body's own last statement is
/// unwrapped too (its value is the method's, and nothing reads a local after
/// it). The class-method root, the REPL root, a last statement of a block and
/// every other block body (a loop or fold body, an exception body, a closure)
/// compile a parenthesised statement as an expression. Anywhere else the
/// assignment is in value position: an operand (`x := (t := t + 1) + 1`), a
/// collection-literal element (`#[t, (t := 7), t]`), the value of another
/// assignment (`x := t := 7`) or of `^`, a conditional's or a cascade's
/// receiver (`(t := t + 1) > 0 ifTrue: [...]`), an interpolated segment, a
/// `match:` subject or arm. A field assignment (`self.x := ...`) is never one:
/// it threads `State` (BT-3803's producer).
///
/// The one rule codegen's producer gates read (as
/// [`value_position_assignment_spans`]) and the agreement tests
/// (`beamtalk-codegen`'s and `beamtalk-repl`'s
/// `value_position_assignment_agreement`) prove: codegen drops none of them.
#[must_use]
pub fn value_position_local_assignments(
    body: &[ExpressionStatement],
    root: Option<RootMethodKind>,
) -> Vec<ValuePositionAssignment> {
    let exprs: Vec<&Expression> = body.iter().map(|stmt| &stmt.expression).collect();
    value_position_assignments_in(&exprs, root)
}

/// [`value_position_local_assignments`] over a body given as its statements'
/// expressions, so a caller holding `&Expression`s need not clone them into
/// `ExpressionStatement`s.
fn value_position_assignments_in(
    body: &[&Expression],
    root: Option<RootMethodKind>,
) -> Vec<ValuePositionAssignment> {
    let mut finder = ValuePositionAssignments::default();
    let unwraps_parens = matches!(
        root,
        Some(RootMethodKind::ValueType | RootMethodKind::Actor)
    );
    finder.body(body, unwraps_parens, root.is_some());
    finder.found
}

/// BT-3910 (ADR 0131 §1): the spans of the value-position local assignments
/// ([`value_position_local_assignments`]) of one body: a method body of kind
/// `root`, or a top-level expression list (`None`). Codegen installs the set per
/// method body (`with_local_producer_sites`), never module-wide: a `Span` carries
/// no file id, and a `uses:` provision's body keeps the spans of the protocol's
/// file, so only spans of one body are guaranteed distinct from each other.
/// Codegen's producer gates read this set: one rule decides which assignments
/// are producers, so a gate cannot disagree with the lowering.
#[must_use]
pub fn value_position_assignment_spans<'a>(
    body: impl IntoIterator<Item = &'a Expression>,
    root: Option<RootMethodKind>,
) -> HashSet<Span> {
    let exprs: Vec<&Expression> = body.into_iter().collect();
    value_position_assignments_in(&exprs, root)
        .into_iter()
        .map(|found| found.span)
        .collect()
}

/// BT-3910: every identifier-target assignment inside `expr` (itself
/// included) that is not inside a block literal, as its target and span, in
/// source order. A block literal is a closure or an inlined arm whose
/// statements are lowered by their own statement lowering, so its assignments
/// are not `expr`'s. Codegen filters the result by
/// [`value_position_assignment_spans`] to find the value-position assignments
/// a sequencing pass or a snapshot rule must see in `expr`.
#[must_use]
pub fn assignments_outside_blocks(expr: &Expression) -> Vec<(EcoString, Span)> {
    use crate::ast::visitor::Visitor;
    #[derive(Default)]
    struct Found(Vec<(EcoString, Span)>);
    impl<'ast> Visitor<'ast> for Found {
        fn visit_expr(&mut self, expr: &'ast Expression) {
            if let Expression::Assignment { target, .. } = expr
                && let Expression::Identifier(id) = target.as_ref()
            {
                self.0.push((id.name.clone(), expr.span()));
            }
            crate::ast::visitor::walk_expr(self, expr);
        }
    }
    let mut found = Found::default();
    found.visit_expr(expr);
    found.0
}

/// One local assignment [`value_position_local_assignments`] names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuePositionAssignment {
    /// The local it assigns.
    pub name: ecow::EcoString,
    /// The whole assignment (`t := t + 1`, without enclosing parentheses).
    pub span: Span,
    /// Whether the assignment is the whole value of a `^` (`^t := e`), whose
    /// usual rewrite is `^e` (an `ensure:` cleanup can still read `t`, so it
    /// is rejected all the same).
    pub returned: bool,
}

/// The walk behind [`value_position_local_assignments`].
#[derive(Default)]
struct ValuePositionAssignments {
    found: Vec<ValuePositionAssignment>,
}

impl ValuePositionAssignments {
    /// The statements of a body; `unwraps_parens` is whether its lowering
    /// binds a parenthesised local assignment that is not its last statement.
    ///
    /// `method_root` is whether `body` is a method's own body: its last
    /// statement is the method's value and nothing reads a local after it, so
    /// a parenthesised assignment there writes nothing observable and is
    /// judged like a bare one (the REPL's last statement is not: its
    /// bindings persist).
    fn statements(
        &mut self,
        body: &[ExpressionStatement],
        unwraps_parens: bool,
        method_root: bool,
    ) {
        let exprs: Vec<&Expression> = body.iter().map(|stmt| &stmt.expression).collect();
        self.body(&exprs, unwraps_parens, method_root);
    }

    fn body(&mut self, body: &[&Expression], unwraps_parens: bool, method_root: bool) {
        use crate::ast::visitor::Visitor;
        for (i, stmt_expr) in body.iter().enumerate() {
            let last = i + 1 == body.len();
            let expr = if (unwraps_parens && !last) || (method_root && last) {
                stmt_expr.unwrap_parens()
            } else {
                *stmt_expr
            };
            match expr {
                // Binds its local: only its value is judged.
                Expression::Assignment { target, value, .. }
                    if matches!(target.as_ref(), Expression::Identifier(_)) =>
                {
                    self.visit_expr(value);
                }
                other => self.visit_expr(other),
            }
        }
    }
}

impl<'ast> crate::ast::visitor::Visitor<'ast> for ValuePositionAssignments {
    fn visit_expr(&mut self, expr: &'ast Expression) {
        // `^t := e`: the same value position, told apart only for the hint.
        if let Expression::Return {
            value: returned, ..
        } = expr
            && let assignment @ Expression::Assignment {
                target,
                value: assigned,
                ..
            } = returned.unwrap_parens()
            && let Expression::Identifier(id) = target.as_ref()
        {
            self.found.push(ValuePositionAssignment {
                name: id.name.clone(),
                span: assignment.span(),
                returned: true,
            });
            self.visit_expr(assigned);
            return;
        }
        if let Expression::Assignment { target, .. } = expr
            && let Expression::Identifier(id) = target.as_ref()
        {
            self.found.push(ValuePositionAssignment {
                name: id.name.clone(),
                span: expr.span(),
                returned: false,
            });
        }
        // An inlined arm (a conditional's, a lookup handler, `[...] value`)
        // is lowered like a method root: a parenthesised statement that is
        // not the arm's value binds its local.
        if let Some(arms) = conditional_arm_blocks(expr)
            && let Expression::MessageSend {
                receiver,
                arguments,
                ..
            } = expr.unwrap_parens()
        {
            for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                match operand {
                    Expression::Block(block)
                        if arms.iter().any(|arm| std::ptr::eq(*arm, block)) =>
                    {
                        self.statements(&block.body, true, false);
                    }
                    other => self.visit_expr(other),
                }
            }
            return;
        }
        crate::ast::visitor::walk_expr(self, expr);
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.statements(&block.body, false, false);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BlockParameter, ExpressionStatement, Identifier};
    use crate::source_analysis::Span;

    fn make_id(name: &str) -> Identifier {
        Identifier::new(name, Span::new(0, u32::try_from(name.len()).unwrap_or(0)))
    }

    fn make_expr_id(name: &str) -> Expression {
        Expression::Identifier(make_id(name))
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    // ─── ADR 0137 (BT-4020): sends that may run user code ───────────────────

    fn analyzed(src: &str) -> BlockMutationAnalysis {
        use crate::source_analysis::{lex_with_eof, parse};
        let (module, _) = parse(lex_with_eof(src));
        let Expression::Block(block) = &module.expressions[0].expression else {
            panic!("not a block: {src}");
        };
        analyze_block(block)
    }

    #[test]
    fn a_send_to_another_object_may_run_user_code() {
        for src in [
            "[x foo]",
            "[x printString]",
            "[a + b]",
            "[x < 1]",
            "[xs do: blk]",
            // A literal-block collection message may be a closure handed to a runtime fold.
            "[xs do: [:e | e]]",
        ] {
            assert!(analyzed(src).has_user_code_sends, "{src}");
        }
    }

    #[test]
    fn intrinsics_inlined_constructs_and_self_sends_do_not_count() {
        for src in [
            "[x isNil]",
            "[x == y]",
            "[x ifTrue: [1] ifFalse: [2]]",
            "[1 + 2]",
            "[self foo]",
            "[super foo]",
            "[x foo!]",
        ] {
            assert!(!analyzed(src).has_user_code_sends, "{src}");
        }
    }

    #[test]
    fn a_nested_block_contributes_its_user_code_sends() {
        assert!(analyzed("[xs do: [:e | e foo]]").has_user_code_sends);
    }

    #[test]
    fn the_switch_gates_the_state_effect_not_the_fact() {
        use crate::synced_calls::{SyncedCallsMode, override_mode};
        let analysis = analyzed("[x foo]");
        {
            let _off = override_mode(SyncedCallsMode::Off);
            assert!(analysis.has_user_code_sends);
            assert!(!analysis.has_synced_user_code_sends());
            assert!(!analysis.needs_mutation_threading(Some(RootMethodKind::Actor), true));
        }
        let _on = override_mode(SyncedCallsMode::Enforce);
        assert!(analysis.has_synced_user_code_sends());
        // `has_state_effects` stays context-free; only the actor root counts the bracket.
        assert!(!analysis.has_state_effects());
        assert!(analysis.needs_mutation_threading(Some(RootMethodKind::Actor), true));
        assert!(!analysis.needs_mutation_threading(Some(RootMethodKind::ValueType), true));
        assert!(!analysis.needs_mutation_threading(Some(RootMethodKind::Class), true));
        assert!(!analysis.needs_mutation_threading(None, true));
    }

    #[test]
    fn test_analyze_empty_block() {
        let block = Block::new(vec![], vec![], Span::new(0, 2));
        let analysis = analyze_block(&block);
        assert!(analysis.local_reads.is_empty());
        assert!(analysis.local_writes.is_empty());
        assert!(analysis.field_reads.is_empty());
        assert!(analysis.field_writes.is_empty());
    }

    #[test]
    fn test_analyze_local_variable_read() {
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(0, 1))],
            vec![bare(make_expr_id("x"))],
            Span::new(0, 5),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.local_reads.contains("x"));
        assert!(analysis.local_writes.is_empty());
    }

    #[test]
    fn test_analyze_local_variable_write() {
        let block = Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("count")),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(0),
                    Span::new(9, 10),
                )),
                type_annotation: None,
                span: Span::new(0, 10),
            })],
            Span::new(0, 12),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.local_writes.contains("count"));
    }

    #[test]
    fn test_analyze_local_variable_mutation() {
        // [:count | count := count + 1]
        // Variable is a parameter, so it's in scope for both read and write
        let block = Block::new(
            vec![BlockParameter::new("count", Span::new(1, 6))],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("count")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("count")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(16, 17),
                    )],
                    is_cast: false,
                    span: Span::new(9, 17),
                }),
                type_annotation: None,
                span: Span::new(0, 17),
            })],
            Span::new(0, 19),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.local_reads.contains("count"));
        assert!(analysis.local_writes.contains("count"));
        assert_eq!(analysis.threaded_vars().len(), 1);
        assert!(analysis.threaded_vars().contains("count"));
    }

    #[test]
    fn test_analyze_field_read() {
        // [self.value]
        let block = Block::new(
            vec![],
            vec![bare(Expression::FieldAccess {
                receiver: Box::new(make_expr_id("self")),
                field: make_id("value"),
                span: Span::new(0, 10),
            })],
            Span::new(0, 12),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.field_reads.contains("value"));
        assert!(analysis.field_writes.is_empty());
    }

    #[test]
    fn test_analyze_field_write() {
        // [self.value := 0]
        let block = Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(make_expr_id("self")),
                    field: make_id("value"),
                    span: Span::new(0, 10),
                }),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(0),
                    Span::new(14, 15),
                )),
                type_annotation: None,
                span: Span::new(0, 15),
            })],
            Span::new(0, 17),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.field_writes.contains("value"));
        assert!(!analysis.field_reads.contains("value"));
    }

    /// Parses `src` (a single block-literal expression) into its `Block`.
    fn parse_block(src: &str) -> Block {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
        match module.expressions.into_iter().next().map(|s| s.expression) {
            Some(Expression::Block(block)) => block,
            other => panic!("{src}: expected a block literal, got {other:?}"),
        }
    }

    fn cv_accesses(src: &str) -> ClassVarAccesses {
        let vars: HashSet<String> = ["n".to_string(), "m".to_string()].into();
        class_var_accesses(&parse_block(src), &vars)
    }

    fn names(items: &[&str]) -> std::collections::BTreeSet<String> {
        items.iter().map(ToString::to_string).collect()
    }

    fn written(acc: &ClassVarAccesses) -> std::collections::BTreeSet<String> {
        acc.writes.keys().cloned().collect()
    }

    #[test]
    fn class_var_accesses_write_only_block_reads_nothing() {
        let acc = cv_accesses("[:x | self.n := x]");
        assert_eq!(written(&acc), names(&["n"]));
        assert!(!acc.reads_class_state(), "{acc:?}");
        // `self.n := self.n + 1` both writes and reads.
        let acc = cv_accesses("[self.n := self.n + 1]");
        assert_eq!(written(&acc), names(&["n"]));
        assert_eq!(acc.reads, names(&["n"]));
    }

    #[test]
    fn class_var_accesses_records_the_first_write_site() {
        let src = "[self.n := 1. self.n := 2]";
        let acc = cv_accesses(src);
        let first = u32::try_from(src.find("self.n").expect("fixture")).expect("small");
        assert_eq!(acc.writes["n"].start(), first, "{acc:?}");
    }

    #[test]
    fn class_var_accesses_sees_reads_in_nested_blocks_and_cascade_messages() {
        let acc = cv_accesses("[:x | x foo; bar: self.n]");
        assert_eq!(acc.reads, names(&["n"]));
        let acc = cv_accesses("[[:y | y + self.m]]");
        assert_eq!(acc.reads, names(&["m"]));
        // An instance field of the same spelling is not a class variable.
        let acc = cv_accesses("[self.other + other.n]");
        assert!(!acc.reads_class_state(), "{acc:?}");
    }

    #[test]
    fn class_var_accesses_has_field_probe_is_a_read_unless_cascaded() {
        let acc = cv_accesses("[self hasField: #n]");
        assert_eq!(
            acc.has_field_probes,
            [HasFieldProbe::Symbol("n".to_string())].into()
        );
        assert!(acc.reads_class_state());
        assert!(
            acc.reads.is_empty(),
            "a probe is not a variable read: {acc:?}"
        );
        // An undeclared name is still a probe of the class-variable home.
        let acc = cv_accesses("[self hasField: #nope]");
        assert_eq!(
            acc.has_field_probes,
            [HasFieldProbe::Symbol("nope".to_string())].into()
        );
        let acc = cv_accesses("[:k | self hasField: k]");
        assert_eq!(acc.has_field_probes, [HasFieldProbe::Computed].into());
        let acc = cv_accesses("[self hasField: #n; hasField: #m]");
        assert!(!acc.reads_class_state(), "{acc:?}");
        // Not to `self`: not a probe of the class-variable home.
        let acc = cv_accesses("[:o | o hasField: #n]");
        assert!(!acc.reads_class_state(), "{acc:?}");
    }

    #[test]
    fn analyze_block_records_later_cascade_self_sends() {
        let analysis = analyze_block(&parse_block("[self log; bump: 1]"));
        assert_eq!(
            analysis.self_send_selectors,
            ["log".to_string(), "bump:".to_string()].into()
        );
    }

    #[test]
    fn test_nested_block_propagates_field_writes() {
        // [:i | [:j | self.value := self.value + 1]]
        // Field writes in nested blocks must propagate to outer analysis
        let inner_block = Expression::Block(Block::new(
            vec![BlockParameter::new("j", Span::new(1, 2))],
            vec![bare(Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(make_expr_id("self")),
                    field: make_id("value"),
                    span: Span::new(0, 10),
                }),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(Expression::FieldAccess {
                        receiver: Box::new(make_expr_id("self")),
                        field: make_id("value"),
                        span: Span::new(0, 10),
                    }),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(20, 21),
                    )],
                    is_cast: false,
                    span: Span::new(0, 21),
                }),
                type_annotation: None,
                span: Span::new(0, 21),
            })],
            Span::new(0, 25),
        ));

        let outer_block = Block::new(
            vec![BlockParameter::new("i", Span::new(1, 2))],
            vec![bare(inner_block)],
            Span::new(0, 30),
        );

        let analysis = analyze_block(&outer_block);
        // Field writes from nested blocks MUST propagate
        assert!(
            analysis.field_writes.contains("value"),
            "field_writes should propagate from nested blocks"
        );
        // Local writes should NOT propagate
        assert!(
            analysis.local_writes.is_empty(),
            "local_writes should not propagate from nested blocks"
        );
    }

    #[test]
    fn test_ensure_receiver_propagates_local_writes() {
        // [t := t + 1] ensure: [nil] — ensure:'s receiver (the
        // protected block) runs inline in the enclosing activation, so its
        // write to `t` must be visible at this block's own top-level
        // analysis (previously only ifTrue:/ifFalse:/ifTrue:ifFalse:
        // propagated local_writes out of a nested block).
        let protected_block = Expression::Block(Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("t")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("t")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(9, 10),
                    )],
                    is_cast: false,
                    span: Span::new(0, 10),
                }),
                type_annotation: None,
                span: Span::new(0, 10),
            })],
            Span::new(0, 12),
        ));
        let handler_block = Expression::Block(Block::new(vec![], vec![], Span::new(20, 26)));

        let outer_block = Block::new(
            vec![],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(protected_block),
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                    "ensure:",
                    Span::new(13, 20),
                )]),
                arguments: vec![handler_block],
                is_cast: false,
                span: Span::new(0, 26),
            })],
            Span::new(0, 28),
        );

        let analysis = analyze_block(&outer_block);
        assert!(
            analysis.local_writes.contains("t"),
            "ensure:'s protected-block write to t must propagate to the enclosing block"
        );
    }

    #[test]
    fn test_on_do_receiver_propagates_local_writes() {
        // [t := t + 1] on: Error do: [:e | nil] — on:do:'s receiver
        // (the try body) runs inline, same as ensure:'s.
        let try_block = Expression::Block(Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("t")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("t")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(9, 10),
                    )],
                    is_cast: false,
                    span: Span::new(0, 10),
                }),
                type_annotation: None,
                span: Span::new(0, 10),
            })],
            Span::new(0, 12),
        ));
        // Handler block binds its own exception param `e` — it must stay a
        // local binding of the nested block, not leak into the outer
        // analysis's captured_reads.
        let handler_block = Expression::Block(Block::new(
            vec![BlockParameter::new("e", Span::new(30, 31))],
            vec![],
            Span::new(29, 34),
        ));

        let outer_block = Block::new(
            vec![],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(try_block),
                selector: MessageSelector::Keyword(vec![
                    crate::ast::KeywordPart::new("on:", Span::new(13, 16)),
                    crate::ast::KeywordPart::new("do:", Span::new(25, 28)),
                ]),
                arguments: vec![make_expr_id("Error"), handler_block],
                is_cast: false,
                span: Span::new(0, 34),
            })],
            Span::new(0, 36),
        );

        let analysis = analyze_block(&outer_block);
        assert!(
            analysis.local_writes.contains("t"),
            "on:do:'s try-body write to t must propagate to the enclosing block"
        );
        assert!(
            !analysis.captured_reads.contains("e"),
            "on:do:'s handler block param must not leak as a captured read"
        );
    }

    #[test]
    fn test_on_do_handler_param_write_does_not_leak_as_outer_local_write() {
        // [nil] on: Error do: [:e | e := 1] — the
        // handler writes its OWN exception param `e`. That write is confined
        // to the handler's own shadowed binding, not a genuine outer-scope
        // mutation, so it must NOT propagate into the enclosing block's
        // local_writes (which would otherwise misclassify the enclosing
        // block as needing mutation threading for a nonexistent outer `e`,
        // or spuriously fold into a same-named real outer local via
        // shadowing).
        let try_block = Expression::Block(Block::new(vec![], vec![], Span::new(0, 6)));
        let handler_block = Expression::Block(Block::new(
            vec![BlockParameter::new("e", Span::new(15, 16))],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("e")),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(23, 24),
                )),
                type_annotation: None,
                span: Span::new(18, 24),
            })],
            Span::new(14, 26),
        ));

        let outer_block = Block::new(
            vec![],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(try_block),
                selector: MessageSelector::Keyword(vec![
                    crate::ast::KeywordPart::new("on:", Span::new(7, 10)),
                    crate::ast::KeywordPart::new("do:", Span::new(19, 22)),
                ]),
                arguments: vec![make_expr_id("Error"), handler_block],
                is_cast: false,
                span: Span::new(0, 26),
            })],
            Span::new(0, 28),
        );

        let analysis = analyze_block(&outer_block);
        assert!(
            !analysis.local_writes.contains("e"),
            "on:do:'s handler writing its own param must not leak as an outer local_write"
        );
    }

    #[test]
    fn test_if_not_nil_propagates_local_writes() {
        // x ifNotNil: [:v | t := v] — ifNotNil: must be
        // included in is_inline_conditional_selector even for this
        // single-level (non-nested) case.
        let handler_block = Expression::Block(Block::new(
            vec![BlockParameter::new("v", Span::new(15, 16))],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("t")),
                value: Box::new(make_expr_id("v")),
                type_annotation: None,
                span: Span::new(18, 25),
            })],
            Span::new(14, 26),
        ));

        let outer_block = Block::new(
            vec![],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(make_expr_id("x")),
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                    "ifNotNil:",
                    Span::new(2, 11),
                )]),
                arguments: vec![handler_block],
                is_cast: false,
                span: Span::new(0, 26),
            })],
            Span::new(0, 28),
        );

        let analysis = analyze_block(&outer_block);
        assert!(
            analysis.local_writes.contains("t"),
            "ifNotNil:'s handler-block write to t must propagate to the enclosing block"
        );
    }

    #[test]
    fn test_is_literal_block() {
        let block_expr = Expression::Block(Block::new(vec![], vec![], Span::new(0, 2)));
        assert!(is_literal_block(&block_expr));

        let var_expr = make_expr_id("myBlock");
        assert!(!is_literal_block(&var_expr));
    }

    #[test]
    fn test_is_control_flow_construct_while_true() {
        let condition = Expression::Block(Block::new(vec![], vec![], Span::new(0, 10)));
        let body = Expression::Block(Block::new(vec![], vec![], Span::new(20, 30)));
        let selector = MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "whileTrue:",
            Span::new(11, 21),
        )]);

        assert!(is_control_flow_construct(&condition, &selector, &[body]));
    }

    #[test]
    fn test_is_not_control_flow_with_stored_block() {
        let condition = make_expr_id("conditionBlock");
        let body = Expression::Block(Block::new(vec![], vec![], Span::new(20, 30)));
        let selector = MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "whileTrue:",
            Span::new(11, 21),
        )]);

        // Not a control flow construct because receiver is not a literal block
        assert!(!is_control_flow_construct(&condition, &selector, &[body]));
    }

    #[test]
    fn test_captured_reads_for_outer_variable_mutation() {
        // [count := count + 1] — `count` is read before being locally defined
        let block = Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(make_expr_id("count")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("count")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(16, 17),
                    )],
                    is_cast: false,
                    span: Span::new(9, 17),
                }),
                type_annotation: None,
                span: Span::new(0, 17),
            })],
            Span::new(0, 19),
        );
        let analysis = analyze_block(&block);
        assert!(
            analysis.captured_reads.contains("count"),
            "count should be a captured read (read before definition)"
        );
        assert!(analysis.local_writes.contains("count"));
    }

    #[test]
    fn test_no_captured_reads_for_new_local_definition() {
        // [:x | temp := x * 2. temp + 1] — `temp` is defined then read (not captured)
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(1, 2))],
            vec![
                bare(Expression::Assignment {
                    target: Box::new(make_expr_id("temp")),
                    value: Box::new(Expression::MessageSend {
                        receiver: Box::new(make_expr_id("x")),
                        selector: MessageSelector::Binary("*".into()),
                        arguments: vec![Expression::Literal(
                            crate::ast::Literal::Integer(2),
                            Span::new(16, 17),
                        )],
                        is_cast: false,
                        span: Span::new(9, 17),
                    }),
                    type_annotation: None,
                    span: Span::new(0, 17),
                }),
                bare(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("temp")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(26, 27),
                    )],
                    is_cast: false,
                    span: Span::new(19, 27),
                }),
            ],
            Span::new(0, 29),
        );
        let analysis = analyze_block(&block);
        assert!(
            !analysis.captured_reads.contains("temp"),
            "temp should NOT be a captured read (defined locally before use)"
        );
        assert!(analysis.local_writes.contains("temp"));
        assert!(analysis.local_reads.contains("temp"));
    }

    #[test]
    fn test_destructure_assignment_binds_variables() {
        // [{a, b} := expr. a + b] — a and b must be local bindings after destructure
        use crate::ast::Pattern;

        let tuple_pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(make_id("a")),
                Pattern::Variable(make_id("b")),
            ],
            span: Span::new(1, 7),
        };

        let block = Block::new(
            vec![],
            vec![
                bare(Expression::DestructureAssignment {
                    pattern: tuple_pattern,
                    value: Box::new(make_expr_id("someTuple")),
                    span: Span::new(0, 20),
                }),
                bare(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("a")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![make_expr_id("b")],
                    is_cast: false,
                    span: Span::new(22, 30),
                }),
            ],
            Span::new(0, 32),
        );
        let analysis = analyze_block(&block);

        // a and b are locally defined by the destructure — not captured from outer scope
        assert!(
            analysis.local_writes.contains("a"),
            "a should be in local_writes after destructure"
        );
        assert!(
            analysis.local_writes.contains("b"),
            "b should be in local_writes after destructure"
        );
        assert!(
            !analysis.captured_reads.contains("a"),
            "a should NOT be a captured read"
        );
        assert!(
            !analysis.captured_reads.contains("b"),
            "b should NOT be a captured read"
        );
        assert!(analysis.local_reads.contains("a"));
        assert!(analysis.local_reads.contains("b"));
    }

    #[test]
    fn test_array_destructure_binds_variables() {
        // [#[first, second] := arr. first] — first and second are local after destructure
        use crate::ast::Pattern;

        let array_pattern = Pattern::Array {
            elements: vec![
                Pattern::Variable(make_id("first")),
                Pattern::Variable(make_id("second")),
            ],
            list_syntax: false,
            rest: None,
            span: Span::new(1, 15),
        };

        let block = Block::new(
            vec![],
            vec![
                bare(Expression::DestructureAssignment {
                    pattern: array_pattern,
                    value: Box::new(make_expr_id("arr")),
                    span: Span::new(0, 20),
                }),
                // Read both bound variables so captured_reads assertions are meaningful
                bare(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("first")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![make_expr_id("second")],
                    is_cast: false,
                    span: Span::new(22, 30),
                }),
            ],
            Span::new(0, 32),
        );
        let analysis = analyze_block(&block);

        assert!(analysis.local_writes.contains("first"));
        assert!(analysis.local_writes.contains("second"));
        assert!(!analysis.captured_reads.contains("first"));
        assert!(!analysis.captured_reads.contains("second"));
    }

    #[test]
    fn test_binary_destructure_size_expr_recorded_as_read() {
        // [<<payload:len/binary>> := bin] where `len` is a variable —
        // the size expression `len` must appear in local_reads/captured_reads.
        use crate::ast::{BinarySegment, Pattern};

        let binary_pattern = Pattern::Binary {
            segments: vec![BinarySegment {
                value: Pattern::Variable(make_id("payload")),
                size: Some(Box::new(make_expr_id("len"))),
                segment_type: None,
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(2, 14),
            }],
            span: Span::new(0, 16),
        };

        let block = Block::new(
            vec![],
            vec![bare(Expression::DestructureAssignment {
                pattern: binary_pattern,
                value: Box::new(make_expr_id("bin")),
                span: Span::new(0, 25),
            })],
            Span::new(0, 27),
        );
        let analysis = analyze_block(&block);

        // `payload` is a binding introduced by the pattern
        assert!(
            analysis.local_writes.contains("payload"),
            "payload should be in local_writes"
        );
        // `len` is read as a size expression — should appear in local_reads
        assert!(
            analysis.local_reads.contains("len"),
            "len (size expression) should be in local_reads"
        );
        // `len` is not locally defined, so it should be a captured read
        assert!(
            analysis.captured_reads.contains("len"),
            "len should be in captured_reads (read before definition)"
        );
    }

    #[test]
    fn test_binary_forward_ref_size_is_captured_read() {
        // <<payload:len/binary, len:8>> — the size expression
        // `len` in segment 1 must still be a captured read even though `len` is
        // bound by segment 2 of the same binary pattern.
        use crate::ast::{BinarySegment, Pattern};

        let binary_pattern = Pattern::Binary {
            segments: vec![
                // Segment 1: payload:len/binary  (size refers to `len`, not yet bound)
                BinarySegment {
                    value: Pattern::Variable(make_id("payload")),
                    size: Some(Box::new(make_expr_id("len"))),
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: Span::new(2, 16),
                },
                // Segment 2: len:8  (binds `len`)
                BinarySegment {
                    value: Pattern::Variable(make_id("len")),
                    size: Some(Box::new(Expression::Literal(
                        crate::ast::Literal::Integer(8),
                        Span::new(19, 20),
                    ))),
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: Span::new(18, 22),
                },
            ],
            span: Span::new(0, 24),
        };

        let block = Block::new(
            vec![],
            vec![bare(Expression::DestructureAssignment {
                pattern: binary_pattern,
                value: Box::new(make_expr_id("bin")),
                span: Span::new(0, 30),
            })],
            Span::new(0, 32),
        );
        let analysis = analyze_block(&block);

        // Both pattern variables must be recorded as writes
        assert!(
            analysis.local_writes.contains("payload"),
            "payload should be in local_writes"
        );
        assert!(
            analysis.local_writes.contains("len"),
            "len should be in local_writes"
        );
        // `len` is used as a size expression before it is bound by segment 2;
        // it must be a captured read (read before local definition).
        assert!(
            analysis.local_reads.contains("len"),
            "len (size expression) should be in local_reads"
        );
        assert!(
            analysis.captured_reads.contains("len"),
            "len should be in captured_reads — it is read before it is bound"
        );
    }

    #[test]
    fn test_block_param_read_is_not_captured() {
        // [:x | x + 1] — `x` is a block param, not a captured read
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(1, 2))],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(make_expr_id("x")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(6, 7),
                )],
                is_cast: false,
                span: Span::new(3, 7),
            })],
            Span::new(0, 9),
        );
        let analysis = analyze_block(&block);
        assert!(
            !analysis.captured_reads.contains("x"),
            "block parameter should NOT be in captured_reads"
        );
        assert!(analysis.local_reads.contains("x"));
    }

    // -- ADR 0131 §1 recognizer and threaded set (BT-3746) -------------------

    fn parse_first_expr(src: &str) -> Expression {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
        module
            .expressions
            .into_iter()
            .next()
            .expect("one expression")
            .expression
    }

    /// The threaded set of `src`'s construct with `outer` bound outside it.
    fn threaded_set(src: &str, outer: &[&str]) -> Vec<String> {
        let expr = parse_first_expr(src);
        let construct =
            local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not a construct"));
        construct_outer_local_writes(&construct, &|n| outer.contains(&n))
            .into_iter()
            .map(|w| w.name.to_string())
            .collect()
    }

    #[test]
    fn only_result_try_do_is_an_exception_producer_and_an_inline_propagating_send() {
        // ADR 0131 Phase 4 (BT-3750): the one receiver-aware check.
        let result = parse_first_expr("Result tryDo: [t := t + 1. 1]");
        assert!(exception_arm_blocks(&result).is_some());
        assert_eq!(
            exception_producer_writes(&result, &|n| n == "t", RootMethodKind::Class)
                .map(|w| w.len()),
            Some(1)
        );
        for src in [
            "Set tryDo: [t := t + 1. 1]",
            "r tryDo: [t := t + 1. 1]",
            "self tryDo: [t := t + 1. 1]",
        ] {
            let expr = parse_first_expr(src);
            assert!(exception_arm_blocks(&expr).is_none(), "{src}");
            assert!(
                exception_producer_writes(&expr, &|n| n == "t", RootMethodKind::Class).is_none(),
                "{src}"
            );
            // And its block is a closure: the write is not the enclosing
            // block's own.
            let outer = parse_first_expr(&format!("[:k | {src}]"));
            let Expression::Block(block) = &outer else {
                panic!("block")
            };
            assert!(analyze_block(block).local_writes.is_empty(), "{src}");
        }
        let outer = parse_first_expr("[:k | Result tryDo: [t := t + 1. 1]]");
        let Expression::Block(block) = &outer else {
            panic!("block")
        };
        assert!(analyze_block(block).local_writes.contains("t"));
    }

    #[test]
    fn local_threading_construct_classifies_every_family() {
        use LocalThreadingFamily::{BlockValue, Conditional, Exception, Fold, Lookup, Loop, TryDo};
        for (src, family, blocks) in [
            ("[i < 3] whileTrue: [i := i + 1]", Loop, 2),
            ("[i < 3] whileTrue", Loop, 1),
            ("[i := i + 1] repeat", Loop, 1),
            ("3 timesRepeat: [i := i + 1]", Loop, 1),
            ("1 to: 3 do: [:k | i := k]", Loop, 1),
            ("1 to: 9 by: 2 do: [:k | i := k]", Loop, 1),
            ("items do: [:x | i := x]", Fold, 1),
            ("items inject: 0 into: [:a :x | a + x]", Fold, 1),
            ("items detect: [:x | x > 1] ifNone: [0]", Fold, 2),
            ("items eachWithIndex: [:x :k | i := k]", Fold, 1),
            ("items do: [:x | i := x] separatedBy: [i := 0]", Fold, 2),
            ("flag ifTrue: [i := 1] ifFalse: [i := 2]", Conditional, 2),
            ("flag and: [i := 1. true]", Conditional, 1),
            ("x ifNil: [i := 1]", Conditional, 1),
            ("[i := 1] on: Error do: [:e | i := 2]", Exception, 2),
            ("[i := 1] ensure: [i := 2]", Exception, 2),
            ("[i := 1] value", BlockValue, 1),
            ("[:a | i := a] value: 3", BlockValue, 1),
            ("d at: #k ifAbsent: [i := 1]", Lookup, 1),
            ("d at: #k ifAbsentPut: [i := 1]", Lookup, 1),
            ("Result tryDo: [i := 1]", TryDo, 1),
        ] {
            let expr = parse_first_expr(src);
            let construct =
                local_threading_construct(&expr).unwrap_or_else(|| panic!("{src}: not recognized"));
            assert_eq!(construct.family, family, "{src}");
            assert_eq!(construct.blocks.len(), blocks, "{src}");
            // Only a lookup differs from its family: four of the five lookups
            // are not implemented by `Dictionary`.
            assert_eq!(
                construct.is_threaded_today(),
                !src.contains("ifAbsentPut:"),
                "{src}"
            );
        }
        for src in [
            "items foo: [:x | i := x]",
            "([i := 1]) value",
            "b value",
            "items do: blk",
            "d at: #k ifAbsent: blk",
            "3 + 4",
        ] {
            assert!(
                local_threading_construct(&parse_first_expr(src)).is_none(),
                "{src} must not be a construct"
            );
        }
    }

    #[test]
    fn construct_outer_local_writes_is_transitively_closed_over_producers() {
        // o7: the outer `on:do:` carries `t`, written only inside a
        // conditional arm's inner `on:do:`.
        assert_eq!(
            threaded_set(
                "[flag ifTrue: [[t := t + 1. 1] on: Error do: [:e | 0]] ifFalse: [0]] \
                 on: Error do: [:e | 0]",
                &["flag", "t"],
            ),
            vec!["t"]
        );
        // A loop nested in a fold nested in a conditional.
        assert_eq!(
            threaded_set(
                "flag ifTrue: [#(1) do: [:x | 1 to: 2 do: [:k | s := s + k]]]",
                &["flag", "s"],
            ),
            vec!["s"]
        );
        // Write-only writes count.
        assert_eq!(
            threaded_set("#(1, 2) do: [:x | last := x]", &["last"]),
            vec!["last"]
        );
    }

    #[test]
    fn construct_outer_local_writes_stops_at_closures_and_unthreaded_constructs() {
        // A block passed to an ordinary send is a closure: its write is the
        // §6 check's business, not the enclosing construct's.
        let src = "#(1) do: [:x | CvA ap: [t := t + 1. 1]]";
        assert!(threaded_set(src, &["t"]).is_empty());
        // `outer_local_writes`, which the §6 check uses, does see it.
        let expr = parse_first_expr(src);
        let construct = local_threading_construct(&expr).expect("construct");
        assert_eq!(
            outer_local_writes(construct.blocks[0], &|n| n == "t").len(),
            1
        );
        // A nested construct that is not threaded today is in the set, but
        // not in the part threaded today.
        let src = "#(1) do: [:x | d at: #k ifAbsentPut: [t := 1]]";
        assert_eq!(threaded_set(src, &["t"]), vec!["t"]);
        let expr = parse_first_expr(src);
        let construct = local_threading_construct(&expr).expect("construct");
        assert!(threaded_today_block_writes(&construct.blocks, &|n| n == "t").is_empty());
        // `tryDo:` is threaded since ADR 0131 Phase 4 (BT-3750).
        let src = "#(1) do: [:x | Result tryDo: [t := 1]]";
        assert_eq!(threaded_set(src, &["t"]), vec!["t"]);
        let expr = parse_first_expr(src);
        let construct = local_threading_construct(&expr).expect("construct");
        assert_eq!(
            threaded_today_block_writes(&construct.blocks, &|n| n == "t").len(),
            1
        );
        // Its own set is recognized too.
        assert_eq!(threaded_set("Result tryDo: [t := 1]", &["t"]), vec!["t"]);
        assert_eq!(
            threaded_set("d at: #k ifAbsent: [t := 1]", &["t"]),
            vec!["t"]
        );
    }

    #[test]
    fn expression_threaded_writes_unions_every_reachable_construct() {
        let writes = |src: &str| -> Vec<String> {
            expression_threaded_writes(&parse_first_expr(src), &|n| n == "t" || n == "u")
                .into_iter()
                .map(|w| w.name.to_string())
                .collect()
        };
        // The expression itself, an argument and a receiver (BT-3748 review).
        assert_eq!(writes("flag ifTrue: [t := 1] ifFalse: [0]"), vec!["t"]);
        assert_eq!(
            writes("self id: (flag ifTrue: [t := t + 1. 1] ifFalse: [0])"),
            vec!["t"]
        );
        assert_eq!(
            writes("(flag ifTrue: [t := t + 1. 1] ifFalse: [0]) + 0"),
            vec!["t"]
        );
        // Two constructs, in source order.
        assert_eq!(
            writes("(flag ifTrue: [u := 1] ifFalse: [0]) + ([t := 1] on: Error do: [:e | 0])"),
            vec!["u", "t"]
        );
        // A closure is not entered, and the expression's own write is not a
        // construct's.
        assert!(writes("self id: [t := t + 1. 1]").is_empty());
        assert!(writes("t := 3").is_empty());
        assert!(writes("x + (t := 3)").is_empty());
    }

    #[test]
    fn construct_outer_local_writes_respects_inner_bindings() {
        // `tmp` is a block local (first assigned inside), `x` a parameter.
        assert_eq!(
            threaded_set(
                "#(1) do: [:x | tmp := x. tmp := tmp + 1. x := 0. t := tmp]",
                &["t"],
            ),
            vec!["t"]
        );
        // A `match:` arm binding shadows the outer name.
        assert!(threaded_set("#(1) do: [:x | x match: [n -> n + 1]]", &["n"]).is_empty());
        // Source order, first write of each name only.
        assert_eq!(
            threaded_set("flag ifTrue: [b := 1. a := 2. b := 3]", &["a", "b"]),
            vec!["b", "a"]
        );
    }

    // -- BT-3806: returned operands followed through their value -------------

    fn module_body(src: &str) -> Vec<ExpressionStatement> {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
        module.expressions
    }

    fn span_text(src: &str, span: Span) -> String {
        src[span.start() as usize..span.end() as usize].to_string()
    }

    /// The source text of each block literal `escape_sites` reports as
    /// returned, and of each one `escaping_blocks` reports, for `src`.
    fn returned_blocks(src: &str) -> (Vec<String>, Vec<String>) {
        let body = module_body(src);
        let sites = escape_sites(&body)
            .into_iter()
            .filter(|&(span, shape)| {
                shape == EscapeShape::Returned && src[span.start() as usize..].starts_with('[')
            })
            .map(|(span, _)| span_text(src, span))
            .collect();
        let blocks = escaping_blocks(&body)
            .into_iter()
            .filter(|(_, shape)| *shape == EscapeShape::Returned)
            .map(|(block, _)| span_text(src, block.span))
            .collect();
        (sites, blocks)
    }

    /// `escape_sites` (ADR 0131 §6) follows a returned value through a local
    /// assignment and the value arms of inlined constructs; `escaping_blocks`
    /// (the census and the class-state-abroad lint) keeps taking it as
    /// written, so their counts do not move.
    #[test]
    fn escape_sites_follow_returned_values_and_escaping_blocks_do_not() {
        for (src, followed) in [
            ("b := [1]", "[1]"),
            ("^b := [1]", "[1]"),
            ("^(b := [1])", "[1]"),
            ("c ifTrue: [[1]] ifFalse: [2]", "[1]"),
            ("c ifTrue: [2] ifFalse: [x := [1]]", "[1]"),
            ("c ifTrue: [c ifFalse: [[1]]]", "[1]"),
            ("x ifNil: [[1]] ifNotNil: [:v | v]", "[1]"),
            ("c and: [[1]]", "[1]"),
            ("[[1]] value", "[1]"),
            ("[[1]] on: Error do: [:e | 2]", "[1]"),
            ("[2] on: Error do: [:e | [1]]", "[1]"),
            ("[[1]] ensure: [2]", "[1]"),
            ("#a match: [#a -> [1]; _ -> 2]", "[1]"),
        ] {
            let (sites, blocks) = returned_blocks(src);
            assert_eq!(sites, vec![followed.to_string()], "{src}");
            assert!(blocks.is_empty(), "{src}: {blocks:?}");
        }
        // Not the construct's value: the cleanup of `ensure:`, a non-last arm
        // statement, the block a `value` send evaluates, a non-inlined send,
        // a value stored and not returned.
        for src in [
            "[2] ensure: [[1]]",
            "c ifTrue: [[1]. 2]",
            "b := [1] value",
            "c foo: [[1]]",
            "x := c ifTrue: [[1]]\nnil",
        ] {
            let (sites, blocks) = returned_blocks(src);
            assert!(sites.is_empty(), "{src}: {sites:?}");
            assert!(blocks.is_empty(), "{src}: {blocks:?}");
        }
        // As written, both agree.
        let (sites, blocks) = returned_blocks("^[1]");
        assert_eq!(sites, blocks);
        assert_eq!(sites, vec!["[1]".to_string()]);
    }

    #[test]
    fn value_blocks_are_the_blocks_a_returned_expression_evaluates_to() {
        let src = "c ifTrue: [[1]] ifFalse: [d := [2]]";
        let expr = parse_first_expr(src);
        let found: Vec<String> = value_blocks(&expr)
            .into_iter()
            .map(|b| span_text(src, b.span))
            .collect();
        assert_eq!(found, vec!["[1]", "[2]"]);
        assert!(value_blocks(&parse_first_expr("[1] value")).is_empty());
    }

    #[test]
    fn value_locals_are_the_locals_a_returned_expression_evaluates_to() {
        let src = "c ifTrue: [b] ifFalse: [d := e]";
        let expr = parse_first_expr(src);
        let found: Vec<String> = value_locals(&expr)
            .into_iter()
            .map(|id| span_text(src, id.span))
            .collect();
        assert_eq!(found, vec!["b", "e"]);
        assert!(value_locals(&parse_first_expr("b value")).is_empty());
    }

    #[test]
    fn actor_channel_rejects_the_callable_also_used_as_a_seed() {
        let module = crate::semantic_analysis::test_helpers::parse_module(
            "Actor subclass: Ac\n  sealed fold: b => ^self items inject: b into: b\n  \
             sealed ok: b => ^self items inject: 0 into: b\n",
        );
        let class = &module.classes[0];
        assert!(!callee_threads_block_param(class, "fold:", 0));
        assert!(callee_threads_block_param(class, "ok:", 0));
    }

    #[test]
    fn actor_channel_predicate_rejects_a_cycle() {
        let module = crate::semantic_analysis::test_helpers::parse_module(
            "Actor subclass: Ac\n  sealed loop: b => self loop: b\n",
        );
        assert!(!callee_threads_block_param(&module.classes[0], "loop:", 0));
    }
}
