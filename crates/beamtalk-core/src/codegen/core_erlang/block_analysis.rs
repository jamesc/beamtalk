// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service analyzes blocks to detect which variables and fields are
//! read/written, enabling proper state threading in tail-recursive loops.

use crate::ast::{Block, Expression, MessageSelector};
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

    /// BT-245: Returns true if the block has any state-affecting operations.
    /// This includes field writes AND self-sends (which may mutate actor state).
    pub fn has_state_effects(&self) -> bool {
        !self.field_writes.is_empty() || self.has_self_sends
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
    let mut analysis = BlockMutationAnalysis::new();
    let mut ctx = AnalysisContext::new();

    // Block parameters are local bindings
    for param in &block.parameters {
        ctx.local_bindings.insert(param.name.to_string());
    }

    // Analyze all expressions in the block body
    for stmt in &block.body {
        analyze_expression(&stmt.expression, &mut analysis, &mut ctx);
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
        | Expression::ExpectDirective { .. } => {
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
            }
            analyze_expression(receiver, analysis, ctx);
            // BT-1053: ifTrue:/ifFalse:/ifTrue:ifFalse: blocks are compiled inline
            // (not as closures), so their local_writes and some captured_reads affect
            // the enclosing scope. Propagate them to allow the outer loop analysis to
            // detect that a captured local variable is mutated inside a conditional.
            //
            // captured_reads from the inner block are propagated selectively: only
            // variables that are NOT already defined in the outer block's local bindings
            // context are considered captured from the method scope. This prevents
            // variables introduced within the outer block body (e.g. `newI := i + 1`
            // before an `ifTrue: [^newI]`) from being misclassified as outer captures.
            if is_inline_conditional_selector(selector) {
                for arg in arguments {
                    if let Expression::Block(block) = arg {
                        let nested = analyze_block(block);
                        analysis
                            .local_reads
                            .extend(nested.local_reads.iter().cloned());
                        analysis
                            .local_writes
                            .extend(nested.local_writes.iter().cloned());
                        // Only propagate captured_reads for vars not yet defined locally
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
        }

        Expression::Return { value, .. } => {
            analyze_expression(value, analysis, ctx);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            analyze_expression(receiver, analysis, ctx);
            for msg in messages {
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
/// `analysis.local_writes`, then processes binary segment size expressions as reads.
///
/// Delegates variable collection to `semantic_analysis::extract_pattern_bindings`
/// and handles the codegen-specific binary segment size expression reads separately.
fn collect_pattern_bindings(
    pattern: &crate::ast::Pattern,
    analysis: &mut BlockMutationAnalysis,
    ctx: &mut AnalysisContext,
) {
    // Delegate variable name collection to the canonical semantic analysis function.
    // Diagnostics (duplicate bindings) are discarded — they are surfaced in the
    // semantic analysis pass and need not be re-emitted during block mutation analysis.
    let (identifiers, _diagnostics) = crate::semantic_analysis::extract_pattern_bindings(pattern);
    for id in identifiers {
        let name = id.name.to_string();
        ctx.local_bindings.insert(name.clone());
        analysis.local_writes.insert(name);
    }

    // Binary segment size expressions (e.g. `len` in `<<payload:len/binary>>`) are
    // reads in the codegen context. This concern is not part of semantic pattern
    // extraction, so we handle it with a separate traversal.
    collect_binary_size_reads(pattern, analysis, ctx);
}

/// Walks `pattern` recursively and calls `analyze_expression` for any binary segment
/// size expressions, recording them as reads in the analysis context.
fn collect_binary_size_reads(
    pattern: &crate::ast::Pattern,
    analysis: &mut BlockMutationAnalysis,
    ctx: &mut AnalysisContext,
) {
    use crate::ast::Pattern;
    match pattern {
        Pattern::Binary { segments, .. } => {
            for seg in segments {
                if let Some(size_expr) = &seg.size {
                    analyze_expression(size_expr, analysis, ctx);
                }
            }
        }
        Pattern::Tuple { elements, .. } | Pattern::Array { elements, .. } => {
            for elem in elements {
                collect_binary_size_reads(elem, analysis, ctx);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for elem in elements {
                collect_binary_size_reads(elem, analysis, ctx);
            }
            if let Some(t) = tail {
                collect_binary_size_reads(t, analysis, ctx);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_binary_size_reads(&pair.value, analysis, ctx);
            }
        }
        Pattern::Variable(_) | Pattern::Wildcard(..) | Pattern::Literal(..) => {}
    }
}

/// Returns true if the expression is a reference to `self`.
fn is_self_reference(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
}

/// Returns true if the selector is an inline conditional (`ifTrue:`, `ifFalse:`, or
/// `ifTrue:ifFalse:`). These are compiled inline rather than as closures, so mutations
/// inside their block arguments affect the enclosing scope.
fn is_inline_conditional_selector(selector: &MessageSelector) -> bool {
    if let MessageSelector::Keyword(parts) = selector {
        let name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        matches!(name.as_str(), "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:")
    } else {
        false
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
                span: Span::new(0, 15),
            })],
            Span::new(0, 17),
        );
        let analysis = analyze_block(&block);
        assert!(analysis.field_writes.contains("value"));
        assert!(!analysis.field_reads.contains("value"));
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
