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
    Block, ClassDefinition, Expression, ExpressionStatement, MessageSelector, MethodKind,
    ParameterDefinition,
};
use crate::ast_walker::{
    SendSite, WalkEvent, cascade_sends, walk_expression_and_sends, walk_sends,
};
use crate::source_analysis::Span;
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
    /// Selectors sent to `self` anywhere in the block, including inside
    /// nested blocks (e.g. a `do:`/`collect:` argument) — unlike `has_self_sends`,
    /// tracked by name so a caller can distinguish a self-send to a provably
    /// non-mutating class method from one that is (or might be) mutating.
    pub self_send_selectors: HashSet<String>,
    /// Whether the block contains a `self.field value(:...)` send — invoking
    /// a block stored in a field. The stored block's body isn't visible here (it may
    /// be assigned anywhere), so this is conservative: any such call is treated as a
    /// potential mutation source, since the field may hold a Tier 2 (state-mutating)
    /// block that would otherwise silently skip state threading.
    pub has_field_value_call: bool,
    /// ADR 0128 (BT-3615): whether the block contains a collection-HOM send
    /// (`items do: block`, `items inject: 0 into: block`, …) forwarding an
    /// OPAQUE (non-literal) callable —
    /// [`crate::state_threading_selectors::is_opaque_callable_hom_send`].
    /// Like `has_field_value_call`, conservative: the callable may be a
    /// Tier 2 block whose captured-local writes ride the actor `State` map,
    /// so in an Actor instance method the send threads `State` (codegen's
    /// opaque-callable fold) and an enclosing conditional/loop must thread
    /// it too rather than discard the send's updated `State`. Propagated
    /// out of nested blocks the same way.
    pub has_opaque_callable_hom_send: bool,
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
    /// This includes field writes, self-sends, `self.field value(:...)` calls,
    /// and collection HOMs forwarding an opaque callable (which may all
    /// mutate actor state).
    pub fn has_state_effects(&self) -> bool {
        !self.field_writes.is_empty()
            || self.has_self_sends
            || self.has_field_value_call
            || self.has_opaque_callable_hom_send
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

/// Whether `receiver` is the unqualified class reference `class_name`.
fn is_own_class_reference(receiver: &Expression, class_name: &str) -> bool {
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

/// Every block literal in a method body (nested blocks included) that is
/// returned or stored, with how it escapes (cloned: the walker's borrows do
/// not outlive its callback). Purely syntactic: a block passed
/// as a message argument is not an escape here (whether it escapes depends on
/// the callee). A block returned from inside a nested inlined conditional
/// branch is not seen (only the method's own last statement and explicit `^`).
pub fn escaping_blocks(body: &[ExpressionStatement]) -> Vec<(Block, EscapeShape)> {
    let mut found = Vec::new();
    if let Some(block) = body.last().and_then(|last| as_block(&last.expression)) {
        found.push((block.clone(), EscapeShape::Returned));
    }
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::Return { value, .. } => {
                if let Some(block) = as_block(value) {
                    found.push((block.clone(), EscapeShape::Returned));
                }
            }
            Expression::Assignment { target, value, .. } => {
                if let Some(block) = as_block(value) {
                    let shape = if matches!(**target, Expression::FieldAccess { .. }) {
                        EscapeShape::StoredClassVar
                    } else {
                        EscapeShape::StoredLocal
                    };
                    found.push((block.clone(), shape));
                }
            }
            Expression::ListLiteral { elements, .. }
            | Expression::ArrayLiteral { elements, .. } => {
                for block in elements.iter().filter_map(as_block) {
                    found.push((block.clone(), EscapeShape::StoredInLiteral));
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for block in pairs.iter().filter_map(|p| as_block(&p.value)) {
                    found.push((block.clone(), EscapeShape::StoredInLiteral));
                }
            }
            _ => {}
        });
    }
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
            ..
        } => {
            record_send_target(receiver, selector, analysis);
            if crate::state_threading_selectors::is_opaque_callable_hom_send(expr) {
                analysis.has_opaque_callable_hom_send = true;
            }
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
            if is_inline_propagating_selector(&selector_name) {
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
            // Propagate `self.field value(:...)` calls the same way as
            // field_writes — a nested block invoking a stored (possibly Tier 2) block
            // is itself a potential mutation source visible to the outer analysis.
            if nested_analysis.has_field_value_call {
                analysis.has_field_value_call = true;
            }
            if nested_analysis.has_opaque_callable_hom_send {
                analysis.has_opaque_callable_hom_send = true;
            }
            // Propagate self-sends the same way — a self-send inside a
            // block passed to select:/collect:/do:/etc. (this is exactly that
            // shape: a `Block` argument that isn't an inline-conditional
            // selector, handled above) is itself a potential mutation source,
            // and callers like `analyze_method_body`'s purity check need to see
            // it at any nesting depth, not just at this block's own top level.
            if nested_analysis.has_self_sends {
                analysis.has_self_sends = true;
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
                record_send_target(send.receiver, send.selector, analysis);
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

        Expression::ArrayLiteral { elements, .. } => {
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
/// `self_send_selectors`), or a `self.field value(:...)` send invoking a block
/// stored in a field, which may be a Tier 2 (state-mutating) block and is
/// conservatively treated as a potential mutation source. Shared by an
/// ordinary send and each later cascade message.
fn record_send_target(
    receiver: &Expression,
    selector: &MessageSelector,
    analysis: &mut BlockMutationAnalysis,
) {
    if is_self_reference(receiver) {
        analysis.has_self_sends = true;
        analysis
            .self_send_selectors
            .insert(selector.name().to_string());
    }
    if is_self_field_value_send(receiver, selector) {
        analysis.has_field_value_call = true;
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

/// Returns true if the expression is a reference to `self`.
///
/// `pub(crate)`: also used by [`crate::semantic_analysis::initialize_assigns`]'s
/// must-analysis (ADR 0124 A2a) — same "is this receiver `self`" question this
/// may-analysis already answers, imported rather than redefined (CLAUDE.md's
/// no-duplicate-implementations rule).
pub(crate) fn is_self_reference(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
}

/// Returns true if `receiver`/`selector` form a `self.field value(:...)`
/// send — a `value`/`value:`/`value:value:`/`value:value:value:` message sent
/// directly to a field access on `self`. Used to detect a stored (possibly Tier 2)
/// block being invoked, which `analyze_block` otherwise has no visibility into.
///
/// Also recognizes `valueWithArguments:` —
/// without this, a `self.field valueWithArguments: #(...)` send nested inside
/// another block (e.g. a `do:` body) wouldn't mark the enclosing block as
/// needing state threading, silently discarding the mutated state the Tier 2
/// runtime discrimination at the call site itself still correctly computes.
/// `valueWithArguments:` has no `WellKnownSelector` variant (see
/// `gen_server/methods.rs`'s `is_tier2_value_call`); [`MessageSelector::is_block_invocation`]
/// names it alongside the `value` family.
fn is_self_field_value_send(receiver: &Expression, selector: &MessageSelector) -> bool {
    matches!(
        receiver,
        Expression::FieldAccess { receiver: r, .. } if is_self_reference(r)
    ) && selector.is_block_invocation()
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
fn is_inline_propagating_selector(selector_name: &str) -> bool {
    crate::state_threading_selectors::is_conditional_selector(selector_name)
        || is_exception_selector_name(selector_name)
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
    analysis
        .self_send_selectors
        .extend(nested.self_send_selectors.iter().cloned());
    if nested.has_field_value_call {
        analysis.has_field_value_call = true;
    }
    if nested.has_opaque_callable_hom_send {
        analysis.has_opaque_callable_hom_send = true;
    }
}

/// One outer-local write found by [`outer_local_writes`]: the local's name and
/// the span of the assignment that writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OuterLocalWrite {
    /// The outer local written.
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
        blocks_entered: 0,
        nested: NestedBlocks::All,
    };
    walker.block(block);
    walker.writes
}

/// Scope-tracking walk behind [`outer_local_writes`].
struct OuterWriteWalker<'a> {
    bound_outside: &'a dyn Fn(&str) -> bool,
    /// Names bound inside the block being analysed, innermost last.
    frames: Vec<HashSet<ecow::EcoString>>,
    writes: Vec<OuterLocalWrite>,
    /// Which nested blocks the walk descends into.
    nested: NestedBlocks,
    /// How many walked blocks enclose the current expression.
    blocks_entered: usize,
}

/// Which nested blocks an [`OuterWriteWalker`] descends into.
#[derive(Clone, Copy, PartialEq, Eq)]
enum NestedBlocks {
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
                    name: name.clone(),
                    span,
                });
            }
        } else {
            self.define(name);
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
            NestedBlocks::All => Vec::new(),
            NestedBlocks::Producers => local_threading_construct(send)
                .map(|c| c.blocks)
                .unwrap_or_default(),
            NestedBlocks::ProducersThreadedToday => local_threading_construct(send)
                .filter(|c| c.family.is_threaded_today())
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
                match target.as_ref() {
                    Expression::Identifier(id) => self.assign(&id.name, *span),
                    other => self.expr(other),
                }
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
            Expression::ArrayLiteral { elements, .. } => {
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
/// [`Self::is_threaded_today`].
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
    /// `value`/`value:`… sent to a block literal.
    BlockValue,
    /// A block-taking lookup selector
    /// ([`crate::state_threading_selectors::lookup_block_arg_indices`]).
    /// Not threaded yet (ADR 0131 phase 2).
    Lookup,
    /// `Result tryDo:` (ADR 0131 §5). Not threaded yet (phase 4).
    TryDo,
}

impl LocalThreadingFamily {
    /// Whether codegen threads this family's outer-local writes back today.
    /// A family that answers `false` compiles its blocks as closures. Neither
    /// the Phase 0 allow-set check nor codegen's tuple builders act on it
    /// yet, and a nested one adds nothing to an enclosing construct's
    /// [`threaded_today_block_writes`] (it does add to
    /// [`construct_outer_local_writes`]).
    #[must_use]
    pub fn is_threaded_today(self) -> bool {
        !matches!(self, Self::Lookup | Self::TryDo)
    }
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
/// Only direct `[...]` literals count, because only those are inlined. A
/// parenthesized or stored block is a block value (ADR 0131 §6).
///
/// The Phase 0 allow-set check (`validators/local_threading.rs`) and
/// codegen's `threaded_locals_of` both recognize constructs with this.
#[must_use]
pub fn local_threading_construct(expr: &Expression) -> Option<LocalThreadingConstruct<'_>> {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threaded_block_receiver,
        is_state_threading_keyword_selector, is_state_threading_unary_selector, is_try_do_selector,
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
                blocks.extend(receiver_block);
                LocalThreadingFamily::BlockValue
            } else if !lookup_block_arg_indices(&sel).is_empty() {
                blocks.extend(lookup_block_arg_indices(&sel).iter().filter_map(|&i| {
                    match arguments.get(i) {
                        Some(Expression::Block(b)) => Some(b),
                        _ => None,
                    }
                }));
                LocalThreadingFamily::Lookup
            } else if is_try_do_selector(&sel) {
                blocks.extend(literal_args());
                LocalThreadingFamily::TryDo
            } else {
                return None;
            }
        }
        MessageSelector::Unary(_) => {
            blocks.extend(receiver_block);
            if is_state_threading_unary_selector(&sel) || sel == "repeat" {
                LocalThreadingFamily::Loop
            } else if is_state_threaded_block_receiver(&sel) {
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
/// family [`LocalThreadingFamily::is_threaded_today`]: the writes today's
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
        blocks_entered: 0,
        nested: NestedBlocks::Producers,
    };
    walker.expr(expr);
    walker.writes
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
/// `captured_reads` of [`analyze_block`]), sorted: the set codegen's Tier 2
/// block protocol threads back (`captured_mutations_for_block`). A block whose
/// outer-local write is not in this set (a write-only write, or one this
/// analysis does not propagate out of a nested block) is not promoted, and
/// the write is lost.
#[must_use]
pub fn captured_local_mutations(block: &Block) -> Vec<String> {
    captured_local_mutations_from_analysis(&analyze_block(block))
}

/// [`captured_local_mutations`] from an already-computed analysis.
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
        Expression::ArrayLiteral { elements, .. } => elements
            .iter()
            .map(|e| stored_block_var_uses(e, var_name))
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        Expression::StringInterpolation { segments, .. } => segments
            .iter()
            .map(|seg| match seg {
                crate::ast::StringSegment::Interpolation(e) => stored_block_var_uses(e, var_name),
                crate::ast::StringSegment::Literal(_) => (false, false),
            })
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
    }
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

    #[test]
    fn test_analyze_self_field_value_call() {
        // [self.onTick value: x] — invoking a block stored in a field must
        // be flagged as a potential mutation source, even with no literal field write.
        let block = Block::new(
            vec![],
            vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::FieldAccess {
                    receiver: Box::new(make_expr_id("self")),
                    field: make_id("onTick"),
                    span: Span::new(0, 14),
                }),
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                    "value:",
                    Span::new(15, 21),
                )]),
                arguments: vec![make_expr_id("x")],
                is_cast: false,
                span: Span::new(0, 23),
            })],
            Span::new(0, 25),
        );
        let analysis = analyze_block(&block);
        assert!(
            analysis.has_field_value_call,
            "self.field value: should set has_field_value_call"
        );
        assert!(analysis.field_writes.is_empty());
        assert!(
            analysis.has_state_effects(),
            "has_field_value_call should make has_state_effects true"
        );
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
    fn test_opaque_callable_hom_send_is_a_state_effect() {
        // ADR 0128 / BT-3615: a collection HOM forwarding an opaque callable
        // may thread actor `State` (the callable can be a Tier 2 block), so
        // an enclosing conditional/loop block must see it as a state effect —
        // at the block's own top level and propagated out of a nested block
        // or an inline conditional arm.
        for src in [
            "[items do: blk]",
            "[:x | x inject: 0 into: blk]",
            "[flag ifTrue: [items count: blk]]",
            "[items do: [:x | x collect: blk]]",
        ] {
            let analysis = analyze_block(&parse_block(src));
            assert!(
                analysis.has_opaque_callable_hom_send,
                "{src}: should set has_opaque_callable_hom_send"
            );
            assert!(
                analysis.has_state_effects(),
                "{src}: should be a state effect"
            );
        }
        for src in [
            "[items do: [:x | x]]",
            "[self do: blk]",
            "[items reject: blk]",
        ] {
            let analysis = analyze_block(&parse_block(src));
            assert!(
                !analysis.has_opaque_callable_hom_send,
                "{src}: should NOT set has_opaque_callable_hom_send"
            );
        }
    }

    #[test]
    fn test_analyze_self_field_read_is_not_value_call() {
        // Sanity check: a plain field read (no `value` send) must NOT set
        // has_field_value_call.
        let block = Block::new(
            vec![],
            vec![bare(Expression::FieldAccess {
                receiver: Box::new(make_expr_id("self")),
                field: make_id("onTick"),
                span: Span::new(0, 14),
            })],
            Span::new(0, 16),
        );
        let analysis = analyze_block(&block);
        assert!(!analysis.has_field_value_call);
    }

    #[test]
    fn test_analyze_self_field_value_call_as_non_first_cascade_message() {
        // [self.onTick displayString; value: x] —
        // the parser folds the cascade's FIRST message ("displayString") into
        // Cascade.receiver as a whole MessageSend, so the true underlying
        // receiver ("self.onTick") is one level deeper than Cascade.receiver
        // itself. A self.field value(:...) send appearing as the SECOND (or
        // later) cascaded message must still be detected, not just a first-message
        // self.field value(:...) send.
        let block = Block::new(
            vec![],
            vec![bare(Expression::Cascade {
                receiver: Box::new(Expression::MessageSend {
                    receiver: Box::new(Expression::FieldAccess {
                        receiver: Box::new(make_expr_id("self")),
                        field: make_id("onTick"),
                        span: Span::new(0, 14),
                    }),
                    selector: MessageSelector::Unary("displayString".into()),
                    arguments: vec![],
                    is_cast: false,
                    span: Span::new(0, 28),
                }),
                messages: vec![crate::ast::CascadeMessage::new(
                    MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                        "value:",
                        Span::new(30, 36),
                    )]),
                    vec![make_expr_id("x")],
                    Span::new(30, 38),
                )],
                span: Span::new(0, 38),
            })],
            Span::new(0, 40),
        );
        let analysis = analyze_block(&block);
        assert!(
            analysis.has_field_value_call,
            "a self.field value(:...) send as the second cascade message must \
             still set has_field_value_call"
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
            assert_eq!(
                family.is_threaded_today(),
                !matches!(family, Lookup | TryDo),
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
        let src = "#(1) do: [:x | Result tryDo: [t := 1]]";
        assert_eq!(threaded_set(src, &["t"]), vec!["t"]);
        let expr = parse_first_expr(src);
        let construct = local_threading_construct(&expr).expect("construct");
        assert!(threaded_today_block_writes(&construct.blocks, &|n| n == "t").is_empty());
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
}
