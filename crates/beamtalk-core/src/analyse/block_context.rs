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

/// Determines if a selector takes blocks for control flow.
///
/// Returns true if the argument at the given index is expected to be a
/// literal block for control flow purposes.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::analyse::block_context::is_control_flow_selector;
/// assert!(is_control_flow_selector("whileTrue:", 0));
/// assert!(is_control_flow_selector("to:do:", 1));
/// assert!(is_control_flow_selector("ifTrue:ifFalse:", 0));
/// assert!(is_control_flow_selector("ifTrue:ifFalse:", 1));
/// assert!(!is_control_flow_selector("at:put:", 0));
/// ```
pub fn is_control_flow_selector(selector: &str, arg_index: usize) -> bool {
    match selector {
        // Loop constructs - block at index 0
        "whileTrue:" | "whileFalse:" | "timesRepeat:" | "do:" | "collect:" | "select:"
        | "reject:" => arg_index == 0,

        // Range iteration or fold/reduce - block at index 1
        "to:do:" | "inject:into:" => arg_index == 1,

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, BlockParameter, Identifier, KeywordPart};
    use crate::parse::Span;

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
}
