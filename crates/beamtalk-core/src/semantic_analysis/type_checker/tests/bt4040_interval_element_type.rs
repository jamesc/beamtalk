// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Interval` binds `Collection`'s element type to `Integer`.
//!
//! `Interval` is declared `typed Collection(Integer) subclass: Interval`,
//! so everything it inherits from `Collection(E)` (`collect:`, `select:`,
//! `inject:into:`, `detect:`, ...) binds `E` to `Integer` instead of
//! `Dynamic`. Only its own `do:` was typed before (BT-4040): `(1 to: n)
//! collect: [:i | ...]` in a typed class inferred `i` as
//! `Dynamic(UnannotatedParam)`.
//!
//! `Interval` also overrides `collect:`/`select:`/`reject:` with honest
//! `List` return types — an interval can only represent an arithmetic
//! sequence, so the mapped result is a `List`, not an `Interval` (matching
//! what the species fallback already produced at runtime).

use super::common::*;

fn check_source(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    checker.take_diagnostics()
}

fn assert_no_dynamic_block_params(diagnostics: &[crate::source_analysis::Diagnostic], what: &str) {
    let dnu_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "{what} should resolve — block param inferred as Integer; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let unannotated_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "{what} should be typed Integer (from Collection(Integer)), not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// The BT-4040 repro: `(1 to: 3) collect: [:i | i + 1]` in a typed class
/// infers `i` as `Integer` — no `Dynamic (unannotated parameter)` warning.
#[test]
fn bt4040_interval_collect_binds_block_param_to_integer() {
    let source = r"
typed Value subclass: Tv
  useA -> Integer =>
    ((1 to: 3) collect: [:i | i + 1]) size
";
    assert_no_dynamic_block_params(&check_source(source), "`i` in Interval collect:");
}

/// `collect:` propagates the transform's return type: mapping to `String`
/// answers `List(String)`, so a chained `select:` infers its param as
/// `String` (the BT-2835 convention).
#[test]
fn bt4040_interval_collect_propagates_return_type_to_chained_select() {
    let source = r"
typed Value subclass: TvChain
  useChain -> List(String) =>
    ^((1 to: 3) collect: [:i | i printString]) select: [:s | s isEmpty not]
";
    assert_no_dynamic_block_params(&check_source(source), "`s` chained onto Interval collect:");
}

/// `(1 to: 6) select: [:i | i isEven]` infers `i` as `Integer`.
#[test]
fn bt4040_interval_select_binds_block_param_to_integer() {
    let source = r"
typed Value subclass: TvSelect
  useSelect -> Integer =>
    ((1 to: 6) select: [:i | i isEven]) size
";
    assert_no_dynamic_block_params(&check_source(source), "`i` in Interval select:");
}

/// `(1 to: 5) inject: 0 into: [:acc :x | acc + x]` infers the element
/// param `x` as `Integer` via the inherited `Block(A, E, A)`.
#[test]
fn bt4040_interval_inject_binds_element_param_to_integer() {
    let source = r"
typed Value subclass: TvInject
  useInject -> Integer =>
    ^((1 to: 5) inject: 0 into: [:acc :x | acc + x])
";
    assert_no_dynamic_block_params(&check_source(source), "`x` in Interval inject:into:");
}
