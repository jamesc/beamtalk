// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block context detection for semantic analysis.
//!
//! This module determines the context in which blocks are used:
//! - **`ControlFlow`**: Literal block in control flow position (whileTrue:, ifTrue:, etc.)
//! - **`Stored`**: Block assigned to a variable
//! - **`Passed`**: Block variable used as message argument
//! - **`Other`**: Other known contexts
//! - **`Unknown`**: Context could not be determined

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::BlockContext;
use crate::source_analysis::Span;

/// Determines if a selector takes blocks for control flow.
///
/// Returns true if the argument at the given index is expected to be a
/// literal block for control flow purposes.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::block_context::is_control_flow_selector;
/// assert!(is_control_flow_selector("whileTrue:", 0));
/// assert!(is_control_flow_selector("to:do:", 1));
/// assert!(is_control_flow_selector("ifTrue:ifFalse:", 0));
/// assert!(is_control_flow_selector("ifTrue:ifFalse:", 1));
/// assert!(is_control_flow_selector("on:do:", 1));
/// assert!(is_control_flow_selector("ensure:", 0));
/// assert!(!is_control_flow_selector("at:put:", 0));
/// ```
pub fn is_control_flow_selector(selector: &str, arg_index: usize) -> bool {
    match selector {
        // Loop constructs - block at index 0
        "whileTrue:" | "whileFalse:" | "timesRepeat:" | "do:" | "collect:" | "select:"
        | "reject:" | "ensure:" => arg_index == 0,

        // Range iteration, fold/reduce, or exception handling - block at index 1 or 2
        "to:do:" | "inject:into:" | "on:do:" => arg_index == 1,
        "to:by:do:" => arg_index == 2,

        // Conditionals - all arguments are control flow blocks
        "ifTrue:" | "ifFalse:" | "ifNil:" | "ifNotNil:" | "ifTrue:ifFalse:" | "ifNil:ifNotNil:" => {
            true
        }

        // Not a control flow selector
        _ => false,
    }
}

/// Determines if an expression is a block variable (identifier).
///
/// Returns true if the expression is an identifier that could be a block variable.
pub fn is_block_variable(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(_))
}

/// Determines if an expression is a literal block.
///
/// Returns true if the expression is a block literal (not a block variable).
pub fn is_literal_block(expr: &Expression) -> bool {
    matches!(expr, Expression::Block(_))
}

/// Extracts the selector string from a message selector.
pub fn selector_to_string(selector: &MessageSelector) -> String {
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => op.to_string(),
        MessageSelector::Keyword(parts) => parts.iter().map(|p| p.keyword.as_str()).collect(),
    }
}

/// Classifies the context of a block based on its position in the AST.
///
/// This function determines whether a block is used for control flow, stored in
/// a variable, passed as an argument, or used in some other context.
///
/// # Key Distinctions
///
/// - **Literal vs Variable**: Only *literal* blocks (`[...]`) in control flow
///   positions are classified as `ControlFlow`. Block *variables* in any position
///   are always `Passed`.
/// - **Control Flow Selectors**: Includes whileTrue:, ifTrue:, timesRepeat:, do:,
///   to:do:, inject:into:, and conditional messages.
///
/// # Parameters
///
/// - `block_span`: The span of the block being classified
/// - `parent_expr`: The parent expression containing the block
/// - `in_assignment`: Whether the block appears on the RHS of an assignment
///
/// # Returns
///
/// The `BlockContext` for this block:
/// - `ControlFlow`: Literal block in control flow selector position
/// - `Stored`: Block on RHS of assignment
/// - `Passed`: Block variable passed as argument
/// - `Other`: Valid context that doesn't match above (e.g., return value)
///
/// Note: `BlockContext::Unknown` is reserved for error recovery in the caller
/// and is not returned by this function.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::block_context::classify_block;
/// # use beamtalk_core::semantic_analysis::BlockContext;
/// # use beamtalk_core::ast::{Expression, Block, MessageSelector, Identifier};
/// # use beamtalk_core::source_analysis::Span;
/// // Control flow: literal block in whileTrue:
/// // [x < 10] whileTrue: [x := x + 1]
/// // The argument block [x := x + 1] is ControlFlow
///
/// // Stored: block on RHS of assignment
/// // myBlock := [x + 1]
/// // let block_expr = Expression::Block(...);
/// // let context = classify_block(block_expr.span(), &parent, true);
/// // assert_eq!(context, BlockContext::Stored);
/// ```
pub fn classify_block(
    block_span: Span,
    parent_expr: &Expression,
    in_assignment: bool,
) -> BlockContext {
    // Case 1: Block on RHS of assignment => Stored
    if in_assignment {
        return BlockContext::Stored;
    }

    // Case 2: Block in message send
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = parent_expr
    {
        let selector_str = selector_to_string(selector);

        // Check each argument to see if it's our block
        for (i, arg) in arguments.iter().enumerate() {
            if arg.span() == block_span {
                // Is this a literal block in control flow position?
                if is_literal_block(arg) && is_control_flow_selector(&selector_str, i) {
                    return BlockContext::ControlFlow;
                }

                // Is this a block variable being passed?
                if is_block_variable(arg) {
                    return BlockContext::Passed;
                }

                // Literal block not in control flow position
                return BlockContext::Other;
            }
        }

        // Special case: whileTrue:/whileFalse: receiver must be literal block
        if matches!(selector_str.as_str(), "whileTrue:" | "whileFalse:")
            && receiver.span() == block_span
            && is_literal_block(receiver)
        {
            return BlockContext::ControlFlow;
        }

        // BT-410: on:do: and ensure: receiver (try body) is a control flow block
        if matches!(selector_str.as_str(), "on:do:" | "ensure:")
            && receiver.span() == block_span
            && is_literal_block(receiver)
        {
            return BlockContext::ControlFlow;
        }
    }

    // Case 3: Other known contexts (return value, nested blocks, etc.)
    BlockContext::Other
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, BlockParameter, Identifier, KeywordPart};
    use crate::source_analysis::Span;

    #[test]
    fn test_is_control_flow_selector_loops() {
        assert!(is_control_flow_selector("whileTrue:", 0));
        assert!(is_control_flow_selector("whileFalse:", 0));
        assert!(is_control_flow_selector("timesRepeat:", 0));
        assert!(is_control_flow_selector("do:", 0));
        assert!(is_control_flow_selector("collect:", 0));
        assert!(is_control_flow_selector("select:", 0));
        assert!(is_control_flow_selector("reject:", 0));

        // Wrong index
        assert!(!is_control_flow_selector("whileTrue:", 1));
    }

    #[test]
    fn test_is_control_flow_selector_range() {
        assert!(!is_control_flow_selector("to:do:", 0));
        assert!(is_control_flow_selector("to:do:", 1));
        assert!(!is_control_flow_selector("to:do:", 2));
    }

    #[test]
    fn test_is_control_flow_selector_fold() {
        assert!(!is_control_flow_selector("inject:into:", 0));
        assert!(is_control_flow_selector("inject:into:", 1));
    }

    #[test]
    fn test_is_control_flow_selector_conditionals() {
        assert!(is_control_flow_selector("ifTrue:", 0));
        assert!(is_control_flow_selector("ifFalse:", 0));
        assert!(is_control_flow_selector("ifNil:", 0));
        assert!(is_control_flow_selector("ifNotNil:", 0));

        // Both arguments are control flow
        assert!(is_control_flow_selector("ifTrue:ifFalse:", 0));
        assert!(is_control_flow_selector("ifTrue:ifFalse:", 1));
        assert!(is_control_flow_selector("ifNil:ifNotNil:", 0));
        assert!(is_control_flow_selector("ifNil:ifNotNil:", 1));
    }

    #[test]
    fn test_is_control_flow_selector_non_control_flow() {
        assert!(!is_control_flow_selector("at:", 0));
        assert!(!is_control_flow_selector("at:put:", 0));
        assert!(!is_control_flow_selector("at:put:", 1));
        assert!(!is_control_flow_selector("value", 0));
        assert!(!is_control_flow_selector("+", 0));
    }

    #[test]
    fn test_is_block_variable_with_identifier() {
        let expr = Expression::Identifier(Identifier::new("block", Span::default()));
        assert!(is_block_variable(&expr));
    }

    #[test]
    fn test_is_block_variable_with_block() {
        let expr = Expression::Block(Block::new(vec![], vec![], Span::default()));
        assert!(!is_block_variable(&expr));
    }

    #[test]
    fn test_is_literal_block_with_block() {
        let expr = Expression::Block(Block::new(vec![], vec![], Span::default()));
        assert!(is_literal_block(&expr));
    }

    #[test]
    fn test_is_literal_block_with_identifier() {
        let expr = Expression::Identifier(Identifier::new("block", Span::default()));
        assert!(!is_literal_block(&expr));
    }

    #[test]
    fn test_selector_to_string_unary() {
        let selector = MessageSelector::Unary("value".into());
        assert_eq!(selector_to_string(&selector), "value");
    }

    #[test]
    fn test_selector_to_string_binary() {
        let selector = MessageSelector::Binary("+".into());
        assert_eq!(selector_to_string(&selector), "+");
    }

    #[test]
    fn test_selector_to_string_keyword() {
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::default()),
            KeywordPart::new("put:", Span::default()),
        ]);
        assert_eq!(selector_to_string(&selector), "at:put:");
    }

    #[test]
    fn test_block_parameter_construction() {
        let param = BlockParameter::new("x", Span::default());
        assert_eq!(param.name, "x");
    }

    // Tests for classify_block()

    #[test]
    fn test_classify_block_stored_context() {
        // myBlock := [x + 1]
        let block = Block::new(vec![], vec![], Span::new(0, 10));
        let block_expr = Expression::Block(block);
        let assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "myBlock",
                Span::new(0, 7),
            ))),
            value: Box::new(block_expr),
            span: Span::new(0, 20),
        };

        let context = classify_block(Span::new(0, 10), &assignment, true);
        assert!(matches!(context, BlockContext::Stored));
    }

    #[test]
    fn test_classify_block_control_flow_whiletrue() {
        use crate::ast::KeywordPart;

        // 10 timesRepeat: [x := x + 1]
        let block = Block::new(vec![], vec![], Span::new(16, 30));
        let block_expr = Expression::Block(block);
        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(10),
                Span::new(0, 2),
            )),
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "timesRepeat:",
                Span::new(3, 15),
            )]),
            arguments: vec![block_expr],
            span: Span::new(0, 30),
        };

        let context = classify_block(Span::new(16, 30), &msg, false);
        assert!(matches!(context, BlockContext::ControlFlow));
    }

    #[test]
    fn test_classify_block_control_flow_to_do() {
        use crate::ast::KeywordPart;

        // 1 to: 10 do: [x := x + 1]
        let block = Block::new(vec![], vec![], Span::new(14, 28));
        let block_expr = Expression::Block(block);
        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(1),
                Span::new(0, 1),
            )),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("to:", Span::new(2, 5)),
                KeywordPart::new("do:", Span::new(9, 12)),
            ]),
            arguments: vec![
                Expression::Literal(crate::ast::Literal::Integer(10), Span::new(6, 8)),
                block_expr,
            ],
            span: Span::new(0, 28),
        };

        let context = classify_block(Span::new(14, 28), &msg, false);
        assert!(matches!(context, BlockContext::ControlFlow));
    }

    #[test]
    fn test_classify_block_passed_context() {
        use crate::ast::KeywordPart;

        // items do: myBlock  (where myBlock is an identifier)
        let block_var = Identifier::new("myBlock", Span::new(10, 17));
        let block_var_expr = Expression::Identifier(block_var);
        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "items",
                Span::new(0, 5),
            ))),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("do:", Span::new(6, 9))]),
            arguments: vec![block_var_expr],
            span: Span::new(0, 17),
        };

        let context = classify_block(Span::new(10, 17), &msg, false);
        assert!(matches!(context, BlockContext::Passed));
    }

    #[test]
    fn test_classify_block_whiletrue_receiver() {
        use crate::ast::KeywordPart;

        // [x < 10] whileTrue: [x := x + 1]
        let receiver_block = Block::new(vec![], vec![], Span::new(0, 8));
        let receiver_expr = Expression::Block(receiver_block);
        let arg_block = Block::new(vec![], vec![], Span::new(20, 34));
        let arg_expr = Expression::Block(arg_block);

        let msg = Expression::MessageSend {
            receiver: Box::new(receiver_expr),
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "whileTrue:",
                Span::new(9, 19),
            )]),
            arguments: vec![arg_expr],
            span: Span::new(0, 34),
        };

        // Receiver block should be ControlFlow
        let context = classify_block(Span::new(0, 8), &msg, false);
        assert!(matches!(context, BlockContext::ControlFlow));
    }

    #[test]
    fn test_classify_block_if_true() {
        use crate::ast::KeywordPart;

        // condition ifTrue: [self doSomething]
        let block = Block::new(vec![], vec![], Span::new(18, 36));
        let block_expr = Expression::Block(block);
        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "condition",
                Span::new(0, 9),
            ))),
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "ifTrue:",
                Span::new(10, 17),
            )]),
            arguments: vec![block_expr],
            span: Span::new(0, 36),
        };

        let context = classify_block(Span::new(18, 36), &msg, false);
        assert!(matches!(context, BlockContext::ControlFlow));
    }

    #[test]
    fn test_classify_block_if_true_if_false() {
        use crate::ast::KeywordPart;

        // condition ifTrue: [x] ifFalse: [y]
        let block1 = Block::new(vec![], vec![], Span::new(18, 21));
        let block1_expr = Expression::Block(block1);
        let block2 = Block::new(vec![], vec![], Span::new(31, 34));
        let block2_expr = Expression::Block(block2);

        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "condition",
                Span::new(0, 9),
            ))),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("ifTrue:", Span::new(10, 17)),
                KeywordPart::new("ifFalse:", Span::new(22, 30)),
            ]),
            arguments: vec![block1_expr, block2_expr],
            span: Span::new(0, 34),
        };

        // Both blocks should be ControlFlow
        let context1 = classify_block(Span::new(18, 21), &msg, false);
        assert!(matches!(context1, BlockContext::ControlFlow));

        let context2 = classify_block(Span::new(31, 34), &msg, false);
        assert!(matches!(context2, BlockContext::ControlFlow));
    }

    #[test]
    fn test_classify_block_other_context() {
        // ^[x + 1]  (return statement with block)
        let block = Block::new(vec![], vec![], Span::new(1, 8));
        let block_expr = Expression::Block(block);
        let return_expr = Expression::Return {
            value: Box::new(block_expr),
            span: Span::new(0, 8),
        };

        let context = classify_block(Span::new(1, 8), &return_expr, false);
        assert!(matches!(context, BlockContext::Other));
    }

    #[test]
    fn test_classify_block_variable_in_control_flow() {
        use crate::ast::KeywordPart;

        // condition ifTrue: myBlock (block variable in control flow)
        // Should be classified as Passed, not ControlFlow
        let block_var = Identifier::new("myBlock", Span::new(18, 25));
        let block_var_expr = Expression::Identifier(block_var);
        let msg = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "condition",
                Span::new(0, 9),
            ))),
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "ifTrue:",
                Span::new(10, 17),
            )]),
            arguments: vec![block_var_expr],
            span: Span::new(0, 25),
        };

        // Block variable is Passed even in control flow position
        let context = classify_block(Span::new(18, 25), &msg, false);
        assert!(matches!(context, BlockContext::Passed));
    }
}
