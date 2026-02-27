// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block context detection for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module classifies blocks into contexts that route between codegen tiers
//! (ADR 0041):
//!
//! - **`ControlFlow`** → **Tier 1** inline codegen (whileTrue:, ifTrue:, do:, etc.)
//! - **`Stored`** → **Tier 2** stateful protocol (block assigned to a variable)
//! - **`Passed`** → **Tier 2** stateful protocol (block variable as message argument)
//! - **`Other`** → **Tier 2** (other known contexts)
//! - **`Unknown`** → **Tier 2** (context could not be determined)
//!
//! The `is_control_flow_selector()` whitelist identifies Tier 1 optimization sites
//! — it is not a correctness gate. New collection methods get Tier 2 state threading
//! automatically without whitelist additions.

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::BlockContext;
use crate::source_analysis::Span;

/// Identifies selectors that take literal blocks for Tier 1 inline codegen.
///
/// This function is a **Tier 1 optimization hint**, not a correctness gate.
/// It identifies call sites where the compiler can generate optimized
/// pack/unpack codegen (whileTrue:, do:, collect:, etc.) directly inline,
/// bypassing the universal Tier 2 stateful block protocol.
///
/// After ADR 0041 Phase 3 (BT-856), state threading is universal — all blocks
/// at unknown call sites use the Tier 2 protocol. The whitelist no longer
/// determines *whether* state threading happens; it only identifies sites
/// where Tier 1 optimized inline codegen is applicable.
///
/// New collection methods do NOT need to be added here to get state threading —
/// they receive Tier 2 blocks automatically.
///
/// Returns true if the argument at the given index is a Tier 1 inline site
/// (literal block for optimized control flow codegen).
///
/// # Examples
///
/// ```text
/// is_control_flow_selector("whileTrue:", 0)  // true  → Tier 1 inline codegen
/// is_control_flow_selector("to:do:", 1)      // true  → Tier 1 inline codegen
/// is_control_flow_selector("at:put:", 0)     // false → Tier 2 universal protocol
/// is_control_flow_selector("myHOM:", 0)      // false → Tier 2 universal protocol
/// ```
pub(crate) fn is_control_flow_selector(selector: &str, arg_index: usize) -> bool {
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
pub(crate) fn is_block_variable(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(_))
}

/// Determines if an expression is a literal block.
///
/// Returns true if the expression is a block literal (not a block variable).
pub(crate) fn is_literal_block(expr: &Expression) -> bool {
    matches!(expr, Expression::Block(_))
}

/// Identifies selectors that iterate over a collection using a block.
///
/// These are the higher-order methods where `self` captures inside literal blocks
/// can deadlock via the `calling_self` re-entrancy mechanism (BT-953). Unlike
/// `is_control_flow_selector`, this function excludes conditional selectors
/// (`ifTrue:`, `ifFalse:`, etc.) that are safe for `self` captures.
///
/// Returns `true` if the argument at the given index is a block passed into
/// collection iteration — a position where `self` captures are dangerous.
pub(crate) fn is_collection_hof_selector(selector: &str, arg_index: usize) -> bool {
    match selector {
        "collect:" | "do:" | "select:" | "reject:" | "detect:" => arg_index == 0,
        "inject:into:" | "detect:ifNone:" => arg_index == 1,
        _ => false,
    }
}

/// Extracts the selector string from a message selector.
pub(crate) fn selector_to_string(selector: &MessageSelector) -> String {
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => op.to_string(),
        MessageSelector::Keyword(parts) => parts.iter().map(|p| p.keyword.as_str()).collect(),
    }
}

/// Classifies the context of a block based on its position in the AST.
///
/// This function routes blocks between **Tier 1** and **Tier 2** codegen:
///
/// - **`ControlFlow`** → **Tier 1** inline codegen (whileTrue:, do:, collect:,
///   ifTrue:, etc.). The compiler generates optimized pack/unpack scaffolding
///   directly. These blocks are compiled via `generate_block_body()` and never
///   reach the universal Tier 2 path.
///
/// - **`Stored`** → **Tier 2** stateful protocol: blocks assigned to variables
///   emit `fun(Args..., StateAcc) -> {Result, NewStateAcc}`. Captured variable
///   mutations are threaded through `StateAcc` maps.
///
/// - **`Passed`** → **Tier 2** stateful protocol: block variables passed as
///   arguments to message sends use the universal stateful calling convention.
///
/// - **`Other`** / **`Unknown`** → **Tier 2** by default; unknown sites use
///   the universal protocol.
///
/// # ADR 0041 Phase 3 (BT-856)
///
/// After BT-852/BT-853, the Tier 2 protocol is universal. `classify_block()`
/// is retained to route between Tier 1 (optimized inline) and Tier 2 (universal
/// protocol) codegen. The whitelist (`is_control_flow_selector()`) is now an
/// optimization hint — it identifies Tier 1 sites, not a correctness gate.
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
/// - `ControlFlow`: Literal block in control flow selector position → Tier 1 codegen
/// - `Stored`: Block on RHS of assignment → Tier 2 codegen
/// - `Passed`: Block variable passed as argument → Tier 2 codegen
/// - `Other`: Valid context that doesn't match above (e.g., return value) → Tier 2
///
/// Note: `BlockContext::Unknown` is reserved for error recovery in the caller
/// and is not returned by this function.
///
/// # Examples
///
/// ```text
/// // ControlFlow (Tier 1): literal block in whileTrue:
/// // [x < 10] whileTrue: [x := x + 1]
/// // The argument block [x := x + 1] → ControlFlow → Tier 1 inline codegen
///
/// // Stored (Tier 2): block on RHS of assignment
/// // myBlock := [count := count + 1]
/// // → Stored → Tier 2: fun(StateAcc) -> {result, NewStateAcc}
///
/// // Passed (Tier 2): block variable as message argument
/// // self myHOM: myBlock
/// // → Passed → Tier 2 stateful calling convention
/// ```
pub(crate) fn classify_block(
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

    // Tests for is_collection_hof_selector()

    #[test]
    fn test_is_collection_hof_selector_iteration() {
        assert!(is_collection_hof_selector("collect:", 0));
        assert!(is_collection_hof_selector("do:", 0));
        assert!(is_collection_hof_selector("select:", 0));
        assert!(is_collection_hof_selector("reject:", 0));
        assert!(is_collection_hof_selector("detect:", 0));
        assert!(is_collection_hof_selector("inject:into:", 1));
        assert!(is_collection_hof_selector("detect:ifNone:", 1));
    }

    #[test]
    fn test_is_collection_hof_selector_wrong_index() {
        assert!(!is_collection_hof_selector("collect:", 1));
        assert!(!is_collection_hof_selector("inject:into:", 0));
    }

    #[test]
    fn test_is_collection_hof_selector_excludes_conditionals() {
        // Conditional selectors must NOT be flagged — self inside ifTrue: is safe
        assert!(!is_collection_hof_selector("ifTrue:", 0));
        assert!(!is_collection_hof_selector("ifFalse:", 0));
        assert!(!is_collection_hof_selector("ifTrue:ifFalse:", 0));
        assert!(!is_collection_hof_selector("whileTrue:", 0));
        assert!(!is_collection_hof_selector("timesRepeat:", 0));
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
            is_cast: false,
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
            is_cast: false,
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
            is_cast: false,
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
            is_cast: false,
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
            is_cast: false,
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
            is_cast: false,
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
            is_cast: false,
            span: Span::new(0, 25),
        };

        // Block variable is Passed even in control flow position
        let context = classify_block(Span::new(18, 25), &msg, false);
        assert!(matches!(context, BlockContext::Passed));
    }
}
