// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block mutation analysis for control flow constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service analyzes blocks to detect which variables and fields are
//! read/written, enabling proper state threading in tail-recursive loops.

use crate::ast::{Block, Expression};
#[cfg(test)]
use crate::ast::MessageSelector;
use std::collections::HashSet;

/// Analysis results for a block's variable and field usage.
#[derive(Debug, Clone, Default)]
pub struct BlockMutationAnalysis {
    /// Local variables that are read in the block.
    pub local_reads: HashSet<String>,
    /// Local variables that are written to in the block.
    pub local_writes: HashSet<String>,
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
    for expr in &block.body {
        analyze_expression(expr, &mut analysis, &mut ctx);
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
        | Expression::Primitive { .. } => {
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
            arguments,
            ..
        } => {
            // BT-245: Detect self-sends (may mutate actor state)
            if is_self_reference(receiver) {
                analysis.has_self_sends = true;
            }
            analyze_expression(receiver, analysis, ctx);
            for arg in arguments {
                analyze_expression(arg, analysis, ctx);
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

        Expression::Pipe { value, target, .. } => {
            analyze_expression(value, analysis, ctx);
            analyze_expression(target, analysis, ctx);
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
    }
}

/// Returns true if the expression is a reference to `self`.
fn is_self_reference(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
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
    use crate::ast::{BlockParameter, Identifier};
    use crate::source_analysis::Span;

    fn make_id(name: &str) -> Identifier {
        Identifier::new(name, Span::new(0, u32::try_from(name.len()).unwrap_or(0)))
    }

    fn make_expr_id(name: &str) -> Expression {
        Expression::Identifier(make_id(name))
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
            vec![make_expr_id("x")],
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
            vec![Expression::Assignment {
                target: Box::new(make_expr_id("count")),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(0),
                    Span::new(9, 10),
                )),
                span: Span::new(0, 10),
            }],
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
            vec![Expression::Assignment {
                target: Box::new(make_expr_id("count")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("count")),
                    selector: MessageSelector::Binary("+".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(1),
                        Span::new(16, 17),
                    )],
                    span: Span::new(9, 17),
                }),
                span: Span::new(0, 17),
            }],
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
            vec![Expression::FieldAccess {
                receiver: Box::new(make_expr_id("self")),
                field: make_id("value"),
                span: Span::new(0, 10),
            }],
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
            vec![Expression::Assignment {
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
            }],
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
            vec![Expression::Assignment {
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
                    span: Span::new(0, 21),
                }),
                span: Span::new(0, 21),
            }],
            Span::new(0, 25),
        ));

        let outer_block = Block::new(
            vec![BlockParameter::new("i", Span::new(1, 2))],
            vec![inner_block],
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
}
