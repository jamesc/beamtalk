// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5216: `Integer>>to:by:do:` takes a `Number` step (BT-5175), so a Float
//! step hands the block Floats (`1 to: 2 by: 0.5 do:` passes 1, 1.5, 2.0).
//! The block is therefore `Block(Number, Object)`: the checker must not tell
//! the body its parameter is an `Integer`.

use super::common::*;

fn dnu_messages(source: &str) -> Vec<String> {
    diags_parse_ok(source)
        .into_iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.to_string())
        .collect()
}

/// The issue's repro: `bitAnd:` is Integer-only, so with a `Number`
/// parameter it warns instead of passing the check and failing at run time.
#[test]
fn bt5216_float_step_block_param_is_number_so_bit_and_warns() {
    let source = r"
typed Value subclass: Tv
  run -> Nil =>
    1 to: 2 by: 0.5 do: [:x | x bitAnd: 1]
";
    let dnus = dnu_messages(source);
    assert!(
        dnus.iter()
            .any(|m| m.contains("Number does not understand 'bitAnd:'")),
        "expected `Number does not understand 'bitAnd:'`, got: {dnus:?}"
    );
}

/// A typed caller that annotates the block parameter `:: Float` for a Float
/// step type-checks. Before BT-5216 the declared `Block(Integer, Object)`
/// made this `Type mismatch: declared as Float, got Integer`.
#[test]
fn bt5216_typed_float_step_caller_type_checks() {
    let source = r"
typed Value subclass: TvSum
  total -> Number =>
    sum :: Number := 0
    1 to: 2 by: 0.5 do: [:x :: Float | sum := sum + x]
    sum
";
    let diags = diags_parse_ok(source);
    assert!(
        diags.is_empty(),
        "typed Float-step caller should type-check cleanly, got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// BT-5246: with an `Integer` start and a statically-`Integer` step every
/// value is an Integer, so the parameter is `Integer` and Integer-only sends
/// do not warn.
#[test]
fn bt5246_integer_step_block_param_is_integer() {
    let source = r"
typed Value subclass: TvInt
  run: xs :: List(Integer) -> Nil =>
    1 to: 10 by: 2 do: [:i | xs at: i]
    1 to: 10 by: 2 do: [:i | i bitAnd: 1]
    1 to: 10 by: 2 do: [:i | i isEven]
    1 to: 10 by: 2 do: [:i | 1 to: i do: [:j | j]]
";
    let diags = diags_parse_ok(source);
    assert!(
        diags.is_empty(),
        "got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// A step typed `Number` (not statically Integer) keeps the `Number`
/// parameter, so the BT-5216 warning still fires.
#[test]
fn bt5246_number_step_keeps_number_param() {
    let source = r"
typed Value subclass: TvNum
  run: step :: Number -> Nil =>
    1 to: 10 by: step do: [:x | x bitAnd: 1]
";
    let dnus = dnu_messages(source);
    assert!(
        dnus.iter()
            .any(|m| m.contains("Number does not understand 'bitAnd:'")),
        "got: {dnus:?}"
    );
}

/// `Number` protocol stays clean for an Integer step too.
#[test]
fn bt5216_integer_step_number_protocol_has_no_warning() {
    let source = r"
typed Value subclass: TvIntNum
  run -> Nil =>
    1 to: 10 by: 2 do: [:i | i isPositive]
";
    assert_eq!(dnu_messages(source), Vec::<String>::new());
}

/// An explicit `:: Integer` annotation is still accepted.
#[test]
fn bt5216_integer_step_with_integer_annotation_has_no_warning() {
    let source = r"
typed Value subclass: TvIntAnn
  run -> Nil =>
    1 to: 10 by: 2 do: [:i :: Integer | i bitAnd: 1]
";
    let diags = diags_parse_ok(source);
    assert!(
        diags.is_empty(),
        "got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}
