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
use std::collections::HashSet;

/// Analysis results for a block's variable and field usage.
#[derive(Debug, Clone, Default)]
pub struct BlockMutationAnalysis {
    /// Local variables that are read in the block.
    pub local_reads: HashSet<String>,
    /// Local variables that are written to in the block.
    pub local_writes: HashSet<String>,
    /// BT-665: Variables read before being locally defined (captured from outer scope).
    pub captured_reads: HashSet<String>,
    /// Fields (self.field) that are read in the block.
    pub field_reads: HashSet<String>,
    /// Fields (self.field) that are written to in the block.
    pub field_writes: HashSet<String>,
    /// BT-245: Whether the block contains self-sends (which may mutate actor state).
    pub has_self_sends: bool,
    /// BT-3151: Selectors sent to `self` anywhere in the block, including inside
    /// nested blocks (e.g. a `do:`/`collect:` argument) — unlike `has_self_sends`,
    /// tracked by name so a caller can distinguish a self-send to a provably
    /// non-mutating class method from one that is (or might be) mutating.
    pub self_send_selectors: HashSet<String>,
    /// BT-2807: Whether the block contains a `self.field value(:...)` send — invoking
    /// a block stored in a field. The stored block's body isn't visible here (it may
    /// be assigned anywhere), so this is conservative: any such call is treated as a
    /// potential mutation source, since the field may hold a Tier 2 (state-mutating)
    /// block that would otherwise silently skip state threading.
    pub has_field_value_call: bool,
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

    /// BT-245/BT-2807: Returns true if the block has any state-affecting operations.
    /// This includes field writes, self-sends, and `self.field value(:...)` calls
    /// (which may all mutate actor state).
    pub fn has_state_effects(&self) -> bool {
        !self.field_writes.is_empty() || self.has_self_sends || self.has_field_value_call
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

/// BT-3151: Analyzes a method body (top-level statements, not wrapped in a
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

/// BT-3151: Computes the set of this class's own class-method selectors that
/// are *known or suspected* to mutate a class variable — directly (`self.cv
/// := ...` for `cv` in `class_var_names`) or transitively (a self-send,
/// anywhere in the method body including inside nested blocks, to another
/// selector already in this set).
///
/// A self-send to a selector NOT defined in this class's own `class_methods`
/// (inherited from a superclass, or otherwise unresolvable at this class's
/// compile time) is conservatively treated as mutating too — the same "can't
/// know statically, so assume the worst" call BT-3150 makes for self-sends in
/// threaded loop bodies. This keeps the analysis sound without needing
/// cross-class information codegen doesn't have at this point: a self-send is
/// only ever excluded from the mutating set when its target is a *locally
/// defined* method that this same pass has proven pure.
///
/// Used to let a self-send to a provably pure class method (the common case —
/// see `stdlib/test/fixtures/class_method_block.bt`'s `self double:`-style
/// helpers) keep compiling in a bare, unthreaded block passed to
/// `select:`/`collect:`/`do:`/etc., while rejecting one whose target may
/// mutate class state, where BT-3150's `Letrec`-only guard doesn't reach.
///
/// BT-3430 (ADR 0118 §Decision 5 follow-up — design decision): investigated
/// replacing this whole-class, syntax-only pre-flight fixed point with
/// `beamtalk-codegen`'s `ThreadedValue::close(ctx, CloseContext::Opaque)` /
/// `VerifyError::StateEffectEscapesExpression` — a post-hoc check of one
/// already-compiled expression's real prelude. Structurally impossible to
/// do here regardless of that mechanism's own maturity: this function lives
/// in `beamtalk-core` (Compilation), which never depends on
/// `beamtalk-codegen` (Code Generation) —
/// `docs/development/architecture-principles.md` §1 — so it cannot name
/// `ThreadedValue`/`close()`/`VerifyError` at all, the same constraint
/// BT-3423's `StateEffects` fact hit for its own, differently-shaped
/// "genuinely different questions" split. This function must also run
/// BEFORE any codegen of any of the class's methods (it needs the whole
/// class's own call graph to compute a fixed point), where `close()`'s input
/// — a real, already-compiled expression's prelude — does not exist yet
/// either. The consuming predicate
/// (`check_no_unsafe_class_method_self_sends`, `beamtalk-codegen/src/core_erlang/expressions.rs`)
/// carries the complementary half of this finding (why its `beamtalk-codegen`-side
/// call sites can't route through `close()` either) and the disposition:
/// kept separate, cross-referenced, not unified.
#[allow(clippy::implicit_hasher)] // concrete HashSet (matches ClassContext::class_var_names) is simpler for callers
pub fn compute_class_var_mutating_selectors(
    class: &ClassDefinition,
    class_var_names: &HashSet<String>,
) -> HashSet<String> {
    let methods: Vec<(String, BlockMutationAnalysis)> = class
        .class_methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .map(|m| {
            (
                m.selector.name().to_string(),
                analyze_method_body(&m.parameters, &m.body),
            )
        })
        .collect();
    let local_selectors: HashSet<&str> = methods.iter().map(|(sel, _)| sel.as_str()).collect();

    let mut mutating: HashSet<String> = methods
        .iter()
        .filter(|(_, analysis)| {
            analysis
                .field_writes
                .iter()
                .any(|f| class_var_names.contains(f))
        })
        .map(|(sel, _)| sel.clone())
        .collect();

    // Fixed-point closure over self-sends: a method becomes "mutating" if it
    // self-sends a selector already known to mutate, or one this class
    // doesn't itself define (unresolvable — assume the worst).
    loop {
        let mut changed = false;
        for (sel, analysis) in &methods {
            if mutating.contains(sel) {
                continue;
            }
            let calls_unsafe = analysis.self_send_selectors.iter().any(|called| {
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
            // BT-665: Track reads of variables not yet locally defined (captured from outer scope)
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
            // BT-245: Detect self-sends (may mutate actor state)
            if is_self_reference(receiver) {
                analysis.has_self_sends = true;
                // BT-3151: record the selector too (see `self_send_selectors` doc).
                analysis
                    .self_send_selectors
                    .insert(selector.name().to_string());
            }
            // BT-2807: Detect `self.field value(:...)` — invoking a block stored in a
            // field. The field may hold a Tier 2 (state-mutating) block, so this is
            // conservatively treated as a potential mutation source.
            if is_self_field_value_send(receiver, selector) {
                analysis.has_field_value_call = true;
            }
            // BT-3173: on:do:/ensure: run their receiver (the try/protected
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
            // BT-1053/BT-3173: ifTrue:/ifFalse:/ifTrue:ifFalse:/ifNotNil:/on:do:/
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
            // actor state and must be visible to outer loops for state threading (BT-478)
            analysis
                .field_writes
                .extend(nested_analysis.field_writes.iter().cloned());
            // BT-2807: propagate `self.field value(:...)` calls the same way as
            // field_writes — a nested block invoking a stored (possibly Tier 2) block
            // is itself a potential mutation source visible to the outer analysis.
            if nested_analysis.has_field_value_call {
                analysis.has_field_value_call = true;
            }
            // BT-3151: propagate self-sends the same way — a self-send inside a
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

        Expression::Cascade {
            receiver, messages, ..
        } => {
            // `analyze_expression(receiver, ..)` below already checks whether the
            // cascade's FIRST message is a `self.field value(:...)` send: the
            // parser folds it into `receiver` as a whole `MessageSend` (see
            // `parse_cascade`), so `receiver` here IS that MessageSend, and the
            // `MessageSend` arm's own `is_self_field_value_send` check covers it.
            //
            // Code review follow-up (BT-2807): the SECOND and later cascaded
            // messages are sent to that same underlying receiver too — cascade
            // semantics evaluate the receiver once and send every message to it —
            // but `messages` here only stores their selector/arguments, not a
            // re-wrapped MessageSend, so nothing before this fix ever checked
            // whether one of THEM was a `self.field value(:...)` send. Extract the
            // one true underlying receiver shared by every cascaded message (the
            // inner receiver of the folded first-message MessageSend, or
            // `receiver` itself if there was no message to fold) and check each
            // later message's own selector against it directly.
            analyze_expression(receiver, analysis, ctx);
            let cascade_receiver = match receiver.as_ref() {
                Expression::MessageSend {
                    receiver: inner, ..
                } => inner.as_ref(),
                other => other,
            };
            for msg in messages {
                if is_self_field_value_send(cascade_receiver, &msg.selector) {
                    analysis.has_field_value_call = true;
                }
                // BT-3151 review follow-up: a cascade's 2nd+ message is sent to
                // the same shared receiver as the first (see the comment above),
                // so a self-send there needs the same `self_send_selectors`
                // recording the `MessageSend` arm does for the first message —
                // otherwise a mutating self-send hidden behind an earlier pure
                // cascade message (`self pureLog: x; check: x`) is invisible to
                // `check_no_unsafe_class_method_self_sends` and to
                // `compute_class_var_mutating_selectors`'s purity closure.
                if is_self_reference(cascade_receiver) {
                    analysis.has_self_sends = true;
                    analysis
                        .self_send_selectors
                        .insert(msg.selector.name().to_string());
                }
                for arg in &msg.arguments {
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
fn is_self_reference(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
}

/// BT-2807: Returns true if `receiver`/`selector` form a `self.field value(:...)`
/// send — a `value`/`value:`/`value:value:`/`value:value:value:` message sent
/// directly to a field access on `self`. Used to detect a stored (possibly Tier 2)
/// block being invoked, which `analyze_block` otherwise has no visibility into.
///
/// BT-2803 (adversarial review): also recognizes `valueWithArguments:` —
/// without this, a `self.field valueWithArguments: #(...)` send nested inside
/// another block (e.g. a `do:` body) wouldn't mark the enclosing block as
/// needing state threading, silently discarding the mutated state the Tier 2
/// runtime discrimination at the call site itself still correctly computes.
/// `valueWithArguments:` has no `WellKnownSelector` variant (see
/// `gen_server/methods.rs`'s `is_tier2_value_call`), so it needs an explicit
/// name check alongside the `well_known()` match.
fn is_self_field_value_send(receiver: &Expression, selector: &MessageSelector) -> bool {
    matches!(
        receiver,
        Expression::FieldAccess { receiver: r, .. } if is_self_reference(r)
    ) && (matches!(
        selector.well_known(),
        Some(
            WellKnownSelector::Value
                | WellKnownSelector::ValueColon
                | WellKnownSelector::ValueValue
                | WellKnownSelector::ValueValueValue
        )
    ) || selector.name() == "valueWithArguments:")
}

/// BT-3173: Returns true if `selector_name` is `on:do:` or `ensure:` — exception
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
/// `on:do:`/`ensure:` (BT-3173: their non-receiver block arguments — e.g.
/// `on:do:`'s handler — are inline for the same reason as the receiver).
fn is_inline_propagating_selector(selector_name: &str) -> bool {
    crate::state_threading_selectors::is_conditional_selector(selector_name)
        || is_exception_selector_name(selector_name)
}

/// BT-1053/BT-3173: Propagates a nested inline-compiled block's mutation
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
    // BT-3173 review follow-up: exclude this block's own parameters (e.g.
    // on:do:'s exception var, ifNotNil:'s bound value) before merging
    // local_writes into the enclosing analysis — a write to the block's own
    // param (`on: Error do: [:e | e := 1]`) is confined to that param's own
    // shadowed binding, not a genuine outer-scope mutation. Mirrors the same
    // exclusion `collect_list_op_cross_scope_mutations`/
    // `collect_nested_loop_outer_local_writes` apply for the identical
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
        // BT-2807: [self.onTick value: x] — invoking a block stored in a field must
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
        // Code review follow-up (BT-2807): [self.onTick displayString; value: x] —
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
        // BT-478: [:i | [:j | self.value := self.value + 1]]
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
        // Field writes from nested blocks MUST propagate (BT-478)
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
        // BT-3173: [t := t + 1] ensure: [nil] — ensure:'s receiver (the
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
        // BT-3173: [t := t + 1] on: Error do: [:e | nil] — on:do:'s receiver
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
        // BT-3173 review follow-up: [nil] on: Error do: [:e | e := 1] — the
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
        // BT-3173: x ifNotNil: [:v | t := v] — ifNotNil: was previously
        // excluded from is_inline_conditional_selector even for this
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
        // BT-665: [count := count + 1] — `count` is read before being locally defined
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
        // BT-665: [:x | temp := x * 2. temp + 1] — `temp` is defined then read (not captured)
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
        // BT-1263: [{a, b} := expr. a + b] — a and b must be local bindings after destructure
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
        // BT-1263: [#[first, second] := arr. first] — first and second are local after destructure
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
        // BT-1263: [<<payload:len/binary>> := bin] where `len` is a variable —
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
        // BT-1269 regression: <<payload:len/binary, len:8>> — the size expression
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
        // BT-665: [:x | x + 1] — `x` is a block param, not a captured read
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
}
