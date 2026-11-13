// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! This domain service analyzes blocks to detect which variables and fields are
//! read/written, enabling proper state threading in tail-recursive loops. Also
//! holds the class-variable access census ([`ClassVarAccesses`]) and the
//! while-loop condition predicates that read the analysis.

use super::{RootMethodKind, captured_local_mutations_from_analysis, literal_block_value_arm};
use crate::ast::well_known::WellKnownSelector;
use crate::ast::{
    BinarySegment, Block, ClassDefinition, Expression, ExpressionStatement, MessageSelector,
    MethodKind, ParameterDefinition,
};
use crate::ast_walker::{
    SendSite, WalkEvent, cascade_sends, walk_expression_and_sends, walk_sends,
};
use crate::source_analysis::Span;
use std::collections::HashSet;

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
    /// effect in an actor instance method, because such a call is bracketed with a
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

    /// Whether running the block may write the actor's fields: a direct
    /// field write, a self-send (which may mutate), or a send that may run
    /// user code (a `super` send counts — in a block it runs against the
    /// cell). Reads don't count: they see current values through either
    /// channel. The Tier 2 merge predicate (BT-4122): only such a block
    /// threads its fields through the cell instead of `StateAcc`.
    pub fn may_write_actor_state(&self) -> bool {
        !self.field_writes.is_empty() || self.has_self_sends || self.has_user_code_sends
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
        self.has_user_code_sends
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
    /// Variables read reflectively as `self fieldAt: #name`, sorted (BT-4184,
    /// BT-4421): in a class method this lowers to an in-place read
    /// (`beamtalk_class_vars:field_at`, through a capture abroad), on an actor
    /// a read of the cell. A literal name outside `vars` is left out, as a
    /// `self.name` of a non-variable is: it reads no captured state and
    /// raises anyway (at home `undeclared_class_variable`, abroad with no
    /// capture `class_state_unreachable`). `vars` may include inherited
    /// names, while the runtime's `field_at` checks only the class's own
    /// declared set (BT-3801). Not a cascade message or a cast, which are
    /// ordinary sends.
    pub field_at_reads: std::collections::BTreeSet<String>,
    /// Whether some `self fieldAt: k` read has a computed argument (it may
    /// name a variable).
    pub computed_field_at_read: bool,
    /// Reflective writes, `self fieldAt: #name put: v` and
    /// `self clearField: #name`, in walk order (BT-4184, BT-4428): in a class
    /// method these lower to in-place writes (`beamtalk_class_vars:field_at_put`
    /// / `clear`), on an actor to writes of the cell, so away from home they
    /// raise like a `self.name := v`. Every name is recorded, in `vars` or
    /// not: what a write of a non-variable does is the caller's to judge. Not
    /// a cascade message or a cast, which are ordinary sends.
    pub reflective_writes: Vec<ReflectiveWrite>,
}

/// Which reflective write a [`ReflectiveWrite`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReflectiveWriteKind {
    /// `self fieldAt: #name put: v`.
    FieldAtPut,
    /// `self clearField: #name`.
    ClearField,
}

/// A reflective write on `self` ([`ClassVarAccesses::reflective_writes`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReflectiveWrite {
    /// `fieldAt:put:` or `clearField:`.
    pub kind: ReflectiveWriteKind,
    /// The name a symbol-literal argument gives; `None` when computed.
    pub name: Option<String>,
    /// Where the send starts, for source-order priority against `self.x :=`.
    pub start: u32,
}

impl ReflectiveWrite {
    /// The write as a verb phrase: ``writes {noun} 'n' of {class} with
    /// `fieldAt:put:` ``, ``clears a {noun} of {class} with `clearField:` ``.
    /// How the `class-state-abroad` and `actor-state-abroad` lints name it.
    #[must_use]
    pub fn describe(&self, noun: &str, class: &str) -> String {
        let (verb, selector) = match self.kind {
            ReflectiveWriteKind::FieldAtPut => ("writes", "fieldAt:put:"),
            ReflectiveWriteKind::ClearField => ("clears", "clearField:"),
        };
        match &self.name {
            Some(name) => format!("{verb} {noun} '{name}' of {class} with `{selector}`"),
            None => format!("{verb} a {noun} of {class} with `{selector}`"),
        }
    }
}

/// The argument of a `self hasField:` probe ([`ClassVarAccesses::has_field_probes`]).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HasFieldProbe {
    /// A symbol literal, `self hasField: #n`: the probed name, declared or not.
    Symbol(String),
    /// Any other argument, `self hasField: k`.
    Computed,
}

/// `` `hasField: #n` ``, `` `hasField:` (#a, #b) `` or, when no probe names a
/// symbol, `` `hasField:` ``: how the `class-state-abroad` and
/// `actor-state-abroad` lints name a block's probes.
#[must_use]
pub fn probe_clause(probes: &std::collections::BTreeSet<HasFieldProbe>) -> String {
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

impl ClassVarAccesses {
    /// Whether the block reads class state at all: a `self.name` read or a
    /// `hasField:` probe. Exactly the blocks for which codegen binds a capture
    /// (outside a direct-called method) and the lint's read rule considers.
    #[must_use]
    pub fn reads_class_state(&self) -> bool {
        !self.reads.is_empty() || !self.has_field_probes.is_empty() || self.reads_with_field_at()
    }

    /// Whether the block makes a reflective `self fieldAt:` read
    /// ([`Self::field_at_reads`], [`Self::computed_field_at_read`]).
    #[must_use]
    pub fn reads_with_field_at(&self) -> bool {
        !self.field_at_reads.is_empty() || self.computed_field_at_read
    }

    /// The `fieldAt:` reads as a noun phrase: `{noun} n, m`, or `a {noun}`
    /// when only computed reads were seen; `None` when there are none. A
    /// name also read as `self.name` ([`Self::reads`]) is left out, so
    /// [`Self::reads_phrase`] names each variable once.
    #[must_use]
    pub fn field_at_read_noun(&self, noun: &str) -> Option<String> {
        let names: Vec<&str> = self
            .field_at_reads
            .difference(&self.reads)
            .map(String::as_str)
            .collect();
        if !names.is_empty() {
            Some(format!("{noun} {}", names.join(", ")))
        } else if self.computed_field_at_read {
            Some(format!("a {noun}"))
        } else {
            None
        }
    }

    /// The block's reads as a verb phrase, `self.name` and reflective
    /// `fieldAt:` reads joined: ``reads {noun} n of {class}``, ``reads
    /// {noun} n of {class} and {noun} m with `fieldAt:` ``; `None` when the
    /// block makes neither. How the `class-state-abroad` and
    /// `actor-state-abroad` lints name them.
    #[must_use]
    pub fn reads_phrase(&self, noun: &str, class: &str) -> Option<String> {
        let direct = (!self.reads.is_empty()).then(|| {
            let names = self
                .reads
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{noun} {names}")
        });
        match (direct, self.field_at_read_noun(noun)) {
            (None, None) => None,
            (Some(direct), None) => Some(format!("reads {direct} of {class}")),
            (None, Some(refl)) => Some(format!("reads {refl} of {class} with `fieldAt:`")),
            (Some(direct), Some(refl)) => Some(format!(
                "reads {direct} of {class} and {refl} with `fieldAt:`"
            )),
        }
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
                    && receiver.is_self()
                    && vars.contains(field.name.as_str())
                {
                    accesses
                        .writes
                        .entry(field.name.to_string())
                        .or_insert_with(|| target.span());
                    written_targets.insert(std::ptr::from_ref::<Expression>(target));
                }
            }
            WalkEvent::Expr(
                e @ Expression::FieldAccess {
                    receiver, field, ..
                },
            ) if receiver.is_self()
                && vars.contains(field.name.as_str())
                && !written_targets.contains(&std::ptr::from_ref(e)) =>
            {
                accesses.reads.insert(field.name.to_string());
            }
            WalkEvent::Send(send)
                if send.site == SendSite::Plain
                    && send.receiver.is_self()
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
            WalkEvent::Send(send)
                if send.site == SendSite::Plain
                    && !send.is_cast
                    && send.receiver.is_self()
                    && send.selector.well_known() == Some(WellKnownSelector::FieldAt) =>
            {
                match send.arguments.first() {
                    Some(Expression::Literal(crate::ast::Literal::Symbol(name), _)) => {
                        if vars.contains(name.as_str()) {
                            accesses.field_at_reads.insert(name.to_string());
                        }
                    }
                    _ => accesses.computed_field_at_read = true,
                }
            }
            WalkEvent::Send(send)
                if send.site == SendSite::Plain && !send.is_cast && send.receiver.is_self() =>
            {
                let kind = match send.selector.well_known() {
                    Some(WellKnownSelector::FieldAtPut) => ReflectiveWriteKind::FieldAtPut,
                    Some(WellKnownSelector::ClearField) => ReflectiveWriteKind::ClearField,
                    _ => return,
                };
                let name = match send.arguments.first() {
                    Some(Expression::Literal(crate::ast::Literal::Symbol(name), _)) => {
                        Some(name.to_string())
                    }
                    _ => None,
                };
                accesses.reflective_writes.push(ReflectiveWrite {
                    kind,
                    name,
                    start: send.receiver.span().start(),
                });
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
        Expression::BinaryConstruction { segments, .. } => {
            for expr in segments.iter().flat_map(BinarySegment::sub_expressions) {
                analyze_expression(expr, analysis, ctx);
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
            if receiver.is_self() {
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
                    if receiver.is_self() {
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
                // BT-4263: so is `[:y | ...] value: x`, whose arguments are
                // analysed below as operands of the enclosing block.
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
    if receiver.is_self() {
        analysis.has_self_sends = true;
        analysis
            .self_send_selectors
            .insert(selector.name().to_string());
    } else if !is_cast && send_may_run_user_code(receiver, selector, arguments) {
        analysis.has_user_code_sends = true;
    }
}

/// ADR 0137: a conservative, syntactic answer to "may this send to something
/// other than `self` run user code in this process". Read off the shared
/// selector tables ([`send_user_code`],
/// [`crate::state_threading_selectors::structural_binary`]).
///
/// It is not a superset of what codegen brackets: it leaves two classes to
/// codegen, which knows static operand types and which loops it lowers in
/// place. A binary operator never counts here, and neither does a loop or
/// evaluated literal block (`SendUserCode::LoopOrClosureCall`). Codegen's
/// loop mode selection therefore also asks its own classifier before it
/// lets a loop drop its `State` (BT-4255, `expr_holds_bracketed_call`).
fn send_may_run_user_code(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    use crate::state_threading_selectors::{SendUserCode, send_user_code};
    if matches!(receiver, Expression::Super(_)) {
        // The superclass method may change state and run a block: in a loop or
        // closure position it runs against the cell, so the body holds a call
        // that brackets (ADR 0137 §3, BT-4009).
        return true;
    }
    if matches!(selector, MessageSelector::Binary(_)) {
        // A binary operator is a guarded BIF: the message send is its rare
        // slow path (a receiver that is not a number). It is bracketed where
        // the body threads `State`, but never makes a body thread: an
        // arithmetic loop over untyped locals (`sum := sum + i`) must keep its
        // direct parameters, and the slow path reaching a user-defined
        // operator that touches this actor's fields is the narrow case a
        // loop with no whole `State` accepts (ADR 0137 Negative consequences,
        // BT-4121).
        return false;
    }
    match send_user_code(receiver, selector, arguments) {
        SendUserCode::MayRun | SendUserCode::ClosureOperand => true,
        // A collection message is a call only when its block is a closure
        // handed to the runtime. A block that carries something (writes a
        // captured local or a field, or sends to `self`) is lowered in place
        // as a threaded fold and is not one (the block's own sends are counted
        // as the block is analysed).
        SendUserCode::InlinedOrClosureCall => !arguments.iter().any(|argument| {
            matches!(argument, Expression::Block(block) if {
                let analysis = analyze_block(block);
                !captured_local_mutations_from_analysis(&analysis).is_empty()
                    || !analysis.field_writes.is_empty()
                    || analysis.has_self_sends
            })
        }),
        SendUserCode::Never | SendUserCode::Inlined | SendUserCode::LoopOrClosureCall => false,
    }
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
/// receiver-aware check [`exception_arm_blocks`](super::exception_arm_blocks) and every analysis that
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
/// statement classifier).
#[must_use]
pub fn while_condition_has_state_effects(condition: &Expression, in_class_method: bool) -> bool {
    matches!(condition, Expression::Block(block)
        if block_condition_has_state_effects(block, in_class_method))
}

/// BT-4062: the body block of a keyword `whileTrue:`/`whileFalse:` when it is
/// lowered in place: a bare block literal. Only then does codegen consider the
/// stateful lowering (`generate_while_true`/`generate_while_false`), which is
/// the only lowering that carries a write in the condition; any other body (a
/// variable, a computed block) takes the simple lowering,
/// which runs the condition as a plain closure. (The parser drops the
/// parentheses around a block argument, so `([...])` is a literal.) The one
/// rule the Phase 0 check
/// (`Walker`'s `whileTrue:` condition arm) and the codegen loop gates read.
#[must_use]
pub fn while_body_block(body: &Expression) -> Option<&Block> {
    match body {
        Expression::Block(block) => Some(block),
        _ => None,
    }
}

/// BT-4602: for a keyword `whileTrue:`/`whileFalse:` send (`None` for any
/// other selector), whether its block literal receiver, the loop's condition,
/// threads its outer-local writes back to the method: exactly when the body is
/// a block literal ([`while_body_block`]), in every context. Codegen then
/// carries the condition's writes in the loop's `StateAcc` (BT-4015); with any
/// other body the loop runs its condition as a closure (BT-4062).
///
/// Callers pass the normalised selector (`ast::normalize`): a unary
/// `[...] whileTrue` is the keyword loop with an empty body there (BT-4313),
/// so it always threads. On a raw parse it would answer `None`.
///
/// The one rule the ADR 0131 §6 validator (`Escape::ConditionBodyNotLiteral`
/// on `Some(false)`) and `beamtalk-lint`'s `dead_block_assignment` (no warning
/// on `Some(true)`) read.
#[must_use]
pub fn while_condition_threads(selector: &str, arguments: &[Expression]) -> Option<bool> {
    matches!(selector, "whileTrue:" | "whileFalse:")
        .then(|| arguments.first().and_then(while_body_block).is_some())
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
