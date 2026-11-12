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

/// Identifies selectors that take literal blocks for Tier 1 inline codegen.
///
/// This function is a **Tier 1 optimization hint**, not a correctness gate.
/// It identifies call sites where the compiler can generate optimized
/// pack/unpack codegen (whileTrue:, do:, collect:, etc.) directly inline,
/// bypassing the universal Tier 2 stateful block protocol.
///
/// Under ADR 0041 Phase 3, state threading is universal — all blocks
/// at unknown call sites use the Tier 2 protocol. The whitelist does not
/// determine *whether* state threading happens; it only identifies sites
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
