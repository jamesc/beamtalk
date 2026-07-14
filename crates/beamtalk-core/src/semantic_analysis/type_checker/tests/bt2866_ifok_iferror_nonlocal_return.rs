// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ifOk:ifError:`'s `R` type param no longer gets polluted by a non-local
//! return (`^`) inside the `ifError:` block (BT-2866).
//!
//! `Result(T, E)>>ifOk:ifError:` is declared:
//!
//! ```text
//! sealed ifOk: okBlock :: Block(T, R) ifError: errorBlock :: Block(E, R) -> R
//! ```
//!
//! The `ifOk:` branch genuinely produces a local value (`R` = whatever it
//! returns). The `ifError:` branch in the issue's repro never "returns" a
//! block value at all — it's a non-local return (`^Result error: ...`) that
//! returns from the *enclosing method* directly. A non-local return
//! shouldn't contribute a type to `R` (it's effectively bottom/`Never` — the
//! block doesn't "return" in the local sense).
//!
//! Root cause: `infer_block_with_typed_params` used `infer_stmts`' reported
//! body type unconditionally as the block's own inferred return type. For a
//! block whose body is `^expr`, `infer_stmts` reports `expr`'s type — which
//! naturally matches the *enclosing method's own* declared return type
//! (that's what makes `^` type-check) — not something signalling "this
//! block never produces a local value". That unrelated type then polluted
//! `R`'s union with the *other* block argument's genuine local value.

use super::common::*;

/// Shared fixture: `MyResult(T, E)` with `ifOk:ifError:`, matching the
/// stdlib `Result` shape closely enough to reproduce the bug without
/// depending on the real stdlib class.
const FIXTURE_PREFIX: &str = "\
typed Object subclass: LinearError\n\
  class unknownPayload: msg :: String -> LinearError => self\n\
typed Value subclass: MyResult(T, E)\n\
  class ok: v :: T -> MyResult(T, E) => self\n\
  class error: e :: E -> MyResult(T, E) => self\n\
  ifOk: okBlock :: Block(T, R) ifError: errorBlock :: Block(E, R) -> R => okBlock value: nil\n\
typed Object subclass: HTTPResponse\n\
  bodyAsJson -> MyResult(Dictionary, String) => MyResult ok: nil\n";

/// AC / exact repro shape: the `ifOk:` branch produces `Dictionary`
/// (via `@expect type` narrowing `v`), the `ifError:` branch is a bare
/// non-local return. `jsonBody` must infer as `Dictionary` (from `ifOk:`
/// alone), so `jsonBody at:ifAbsent:` (a real `Dictionary` selector) must
/// not DNU. Before the fix, `R` was polluted to `MyResult(Dictionary,
/// LinearError)` (the enclosing method's own return type, coincidentally
/// matching the `^`-expression), producing a spurious "`MyResult` does not
/// understand 'at:ifAbsent:'" hint.
#[test]
fn ifok_iferror_r_not_polluted_by_nonlocal_return_in_iferror_branch() {
    let source = format!(
        "{FIXTURE_PREFIX}\
typed Object subclass: Repro\n\
  parseResponse: resp :: HTTPResponse -> MyResult(Dictionary, LinearError) =>\n\
    jsonBody := resp bodyAsJson\n\
      ifOk: [:v |\n\
        @expect type\n\
        body :: Dictionary := v\n\
        body\n\
      ]\n\
      ifError: [:e |\n\
        ^MyResult error: (LinearError unknownPayload: e)\n\
      ]\n\
    jsonBody at: \"k\" ifAbsent: [nil]\n"
    );
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "jsonBody should infer as Dictionary (from ifOk: alone) — the ifError: \
         branch's non-local return must not pollute R; got DNU: {dnu:?}"
    );
}

/// Regression guard: when *neither* branch is a non-local return, `R` still
/// correctly unifies both branches' local values (unchanged behaviour) —
/// here both branches return `Dictionary`-compatible values, so no DNU.
#[test]
fn ifok_iferror_unifies_both_branches_when_neither_is_nonlocal_return() {
    let source = format!(
        "{FIXTURE_PREFIX}\
typed Object subclass: Repro\n\
  parseResponse: resp :: HTTPResponse -> Dictionary =>\n\
    jsonBody := resp bodyAsJson\n\
      ifOk: [:v |\n\
        @expect type\n\
        body :: Dictionary := v\n\
        body\n\
      ]\n\
      ifError: [:e | Dictionary new]\n\
    jsonBody at: \"k\" ifAbsent: [nil]\n"
    );
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "both branches produce Dictionary — jsonBody should infer as Dictionary, \
         got DNU: {dnu:?}"
    );
}

/// Regression guard: a non-local return in the *first* block argument
/// (`ifOk:`) must be equally excluded from `R` — the fix isn't specific to
/// `ifError:`'s position.
#[test]
fn ifok_iferror_r_not_polluted_by_nonlocal_return_in_ifok_branch() {
    let source = format!(
        "{FIXTURE_PREFIX}\
typed Object subclass: Repro\n\
  parseResponse: resp :: HTTPResponse -> MyResult(Dictionary, LinearError) =>\n\
    jsonBody := resp bodyAsJson\n\
      ifOk: [:v | ^MyResult ok: nil]\n\
      ifError: [:e |\n\
        @expect type\n\
        body :: Dictionary := (Dictionary new)\n\
        body\n\
      ]\n\
    jsonBody at: \"k\" ifAbsent: [nil]\n"
    );
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "jsonBody should infer as Dictionary (from ifError: alone) — the ifOk: \
         branch's non-local return must not pollute R; got DNU: {dnu:?}"
    );
}
