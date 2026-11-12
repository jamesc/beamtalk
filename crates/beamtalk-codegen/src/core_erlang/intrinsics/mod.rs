// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compiler intrinsics for language-level constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! These are message patterns that the compiler must generate inline code for,
//! regardless of receiver type. They represent language-level semantics, not
//! type-specific dispatch:
//!
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Function application & loops
//! - **`ProtoObject`**: `class` → Type introspection via pattern matching
//! - **Object protocol** (split into domain-aligned groups):
//!   - **Nil protocol**: `isNil`, `notNil`, `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:` → Nil-testing boolean protocol
//!   - **Error signaling**: `error:` → Error construction and signaling
//!   - **Object identity**: `yourself`, `hash` → Identity and representation
//!   - **Object reflection**: `respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:` → Runtime introspection
//! - **Dynamic dispatch**: `perform:`, `perform:withArguments:` → Runtime type-based dispatch (actors → async/Future, primitives → sync/value)
//!
//! Unlike type-specific dispatch (which goes through `beamtalk_primitive:send/3`
//! at runtime), these intrinsics generate efficient inline code because they are
//! fundamental language operations that cannot be deferred to runtime dispatch.
//!
//! This module holds the shared validators, types and constants; the lowering
//! is split by concern into submodules:
//! - `block_calls` — block and Tier 2 value calls, loops, list/dict messages
//! - `object` — `ProtoObject`, `Object`, nil, error-signalling and reflection
//! - `boolean` — Boolean conditionals
//! - `logger` — `Logger` call sites

use super::class_var_access::class_var_fn;
use super::control_flow::SoloBoolean;
use super::control_flow::{PureArm, SoloAnswer};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, primitives};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::{Block, Expression, MessageSelector, WellKnownSelector};
use beamtalk_core::semantic_analysis::ClassHierarchy;

/// Hint shown when a structural intrinsic reached via generic dispatch
/// (`perform:`/`perform:withArguments:`) can't tell — from an
/// `erlang:is_function/2` arity check alone — whether it's looking at a
/// genuine Tier 2 (stateful, ADR-0041) block or a Tier 1 block called with
/// the wrong argument count. Shared by the `value*` fallback, the
/// loop/exception-handling fallbacks (`while_loops.rs`, `exception_handling.rs`),
/// and the List/Collection guard (which uses its own, differently-worded
/// variant — see `generate_stateful_block_guard`).
pub(in crate::core_erlang) const STATEFUL_BLOCK_DISPATCH_HINT: &str = "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:";

/// BT-3968: the `ProtoObject` selector intercepted by name in
/// `try_generate_protoobject_message` (and so excluded from actor
/// self-dispatch by `expr_shape::selector_dispatches_via_self`).
pub(in crate::core_erlang) const PERFORM_WITH_ARGS_TIMEOUT: &str = "perform:withArguments:timeout:";

/// BT-4075: what an `error:` receiver is statically known to be. Only an
/// instance takes the `Object>>error:` raise intrinsic unconditionally; a class
/// object with a class-side `error:` (`Result class>>error:`) receives the send,
/// decided at run time by `beamtalk_class_dispatch:class_object_understands/2`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ErrorSignalReceiver {
    /// A literal, or `self` in an instance-side method: raises unconditionally.
    /// Usually an instance. In an instance-side method of `Behaviour`/`Class`,
    /// `self` is a class object, and raising is still intended there: those
    /// methods signal errors about the class, which must not be handed to a
    /// class-side `error:` such as `Result class>>error:`.
    Instance,
    /// `self` in a class method: a class object, sent to via `class_self_send/3`.
    ClassSelf,
    /// Anything else: sent to via `beamtalk_message_dispatch:send/3`.
    Unknown,
}

/// Returns the arity of a block expression, or `None` if the expression is not a block literal.
fn block_arity(expr: &Expression) -> Option<usize> {
    match expr {
        Expression::Block(b) => Some(b.parameters.len()),
        _ => None,
    }
}

/// Validates that a block has exactly the expected arity.
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if wrong arity.
pub(in crate::core_erlang) fn validate_block_arity_exact(
    expr: &Expression,
    expected: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual != expected {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: expected.to_string(),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// Validates that a block has arity within a range (inclusive).
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if out of range.
#[cfg(test)]
fn validate_block_arity_range(
    expr: &Expression,
    min: usize,
    max: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual < min || actual > max {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: format!("{min} or {max}"),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// Validates that an `on:do:` handler block has arity 0 or 1.
/// Returns `true` if the handler takes an argument (arity 1 or non-literal), `false` for arity 0.
/// Follows the same pattern as `validate_if_not_nil_block`.
pub(in crate::core_erlang) fn validate_on_do_handler(
    expr: &Expression,
    selector: &str,
) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityError {
            selector: selector.to_string(),
            expected: "0 or 1".to_string(),
            actual: n,
            hint: "Fix: The handler block must take 0 or 1 arguments (the exception):\n\
                   \x20 [...] on: Exception do: [:e | e message]\n\
                   \x20 [...] on: Exception do: ['error occurred']"
                .to_string(),
        }),
        // Arity 1 or non-literal block — pass the exception
        _ => Ok(true),
    }
}

/// Validates that an `ifNotNil:` block has arity 0 or 1.
/// Returns `true` if the block takes an argument (arity 1 or non-literal), `false` for arity 0.
fn validate_if_not_nil_block(expr: &Expression, selector: &str) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityMismatch {
            selector: selector.to_string(),
            arity: n,
        }),
        // Arity 1 or non-literal block — pass the receiver (Smalltalk convention)
        _ => Ok(true),
    }
}

/// Generates a Core Erlang `apply` call that adapts to block arity.
/// For 0-arg blocks: `apply BlockVar ()`, for 1-arg blocks: `apply BlockVar (RecvVar)`.
fn not_nil_apply(block_var: &str, recv_var: &str, block_takes_arg: bool) -> Document<'static> {
    if block_takes_arg {
        docvec![
            "apply ",
            leaf::var(block_var.to_string()),
            " (",
            leaf::var(recv_var.to_string()),
            ")"
        ]
    } else {
        docvec!["apply ", leaf::var(block_var.to_string()), " ()"]
    }
}

/// The shape [`CoreErlangGenerator::pure_boolean_shape`] lowers a boolean
/// conditional as, inline with a non-Boolean fallback send.
pub(in crate::core_erlang) enum PureBooleanShape<'a> {
    /// `ifTrue:`, `ifFalse:`, `and:`, `or:` with one literal block.
    Solo(SoloBoolean, &'a Block),
    /// `R not`.
    Not,
    /// `ifTrue:ifFalse:` with two literal blocks.
    IfTrueIfFalse(&'a Block, &'a Block),
}

mod block_calls;
mod boolean;
mod logger;
mod object;

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::ast::{Block, BlockParameter, ExpressionStatement, Literal};
    use beamtalk_core::source_analysis::Span;

    #[test]
    fn test_validate_if_not_nil_block_zero_args() {
        let block = Expression::Block(Block::new(
            vec![],
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(1, 2),
            ))],
            Span::new(0, 3),
        ));
        assert!(!validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_one_arg() {
        let block = Expression::Block(Block::new(
            vec![BlockParameter::new("v", Span::new(1, 2))],
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(5, 6),
            ))],
            Span::new(0, 7),
        ));
        assert!(validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_two_args_errors() {
        let block = Expression::Block(Block::new(
            vec![
                BlockParameter::new("a", Span::new(1, 2)),
                BlockParameter::new("b", Span::new(3, 4)),
            ],
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(7, 8),
            ))],
            Span::new(0, 9),
        ));
        let result = validate_if_not_nil_block(&block, "ifNotNil:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityMismatch { selector, arity }) = result {
            assert_eq!(selector, "ifNotNil:");
            assert_eq!(arity, 2);
        } else {
            panic!("Expected BlockArityMismatch error");
        }
    }

    #[test]
    fn test_validate_if_not_nil_block_non_literal() {
        // Non-block expression (variable) — assumes 1 arg
        let expr = Expression::Identifier(beamtalk_core::ast::Identifier::new(
            "myBlock",
            Span::new(0, 7),
        ));
        assert!(validate_if_not_nil_block(&expr, "ifNotNil:").unwrap());
    }

    // Tests for validate_block_arity_exact

    fn make_block(arity: usize) -> Expression {
        let params: Vec<BlockParameter> = (0..arity)
            .map(|i| {
                #[allow(clippy::cast_possible_truncation)]
                // synthetic span positions for generated block params
                let span = Span::new((i * 2 + 1) as u32, (i * 2 + 2) as u32);
                BlockParameter::new(format!("p{i}"), span)
            })
            .collect();
        Expression::Block(Block::new(
            params,
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(1, 2),
            ))],
            Span::new(0, 10),
        ))
    }

    #[test]
    fn test_validate_exact_correct_arity() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_wrong_arity() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "use to:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            hint,
        }) = result
        {
            assert_eq!(selector, "timesRepeat:");
            assert_eq!(expected, "0");
            assert_eq!(actual, 1);
            assert!(hint.contains("to:do:"));
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_exact_non_literal_passes() {
        // Non-block expressions can't be checked at compile time — always pass
        let expr = Expression::Identifier(beamtalk_core::ast::Identifier::new(
            "myBlock",
            Span::new(0, 7),
        ));
        assert!(validate_block_arity_exact(&expr, 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&expr, 1, "do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_zero_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_to_do_one_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_to_do_zero_arg_error() {
        let result = validate_block_arity_exact(&make_block(0), 1, "to:do:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_inject_into_two_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_inject_into_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 2, "inject:into:", "hint");
        assert!(result.is_err());
    }

    // Tests for validate_block_arity_range

    #[test]
    fn test_validate_range_within_bounds() {
        assert!(validate_block_arity_range(&make_block(0), 0, 1, "on:do:", "hint").is_ok());
        assert!(validate_block_arity_range(&make_block(1), 0, 1, "on:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_range_out_of_bounds() {
        let result = validate_block_arity_range(&make_block(2), 0, 1, "on:do:", "hint");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            ..
        }) = result
        {
            assert_eq!(selector, "on:do:");
            assert_eq!(expected, "0 or 1");
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_range_non_literal_passes() {
        let expr = Expression::Identifier(beamtalk_core::ast::Identifier::new(
            "myBlock",
            Span::new(0, 7),
        ));
        assert!(validate_block_arity_range(&expr, 0, 1, "on:do:", "hint").is_ok());
    }

    // Tests for validate_on_do_handler

    #[test]
    fn test_validate_on_do_handler_zero_args() {
        // 0-arg handler: valid, returns false (don't pass exception)
        assert!(!validate_on_do_handler(&make_block(0), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_one_arg() {
        // 1-arg handler: valid, returns true (pass exception)
        assert!(validate_on_do_handler(&make_block(1), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_two_args_errors() {
        let result = validate_on_do_handler(&make_block(2), "on:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError { actual, .. }) = result {
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_on_do_handler_non_literal() {
        // Non-block expression — assumes 1 arg (pass exception)
        let expr = Expression::Identifier(beamtalk_core::ast::Identifier::new(
            "myBlock",
            Span::new(0, 7),
        ));
        assert!(validate_on_do_handler(&expr, "on:do:").unwrap());
    }
}
