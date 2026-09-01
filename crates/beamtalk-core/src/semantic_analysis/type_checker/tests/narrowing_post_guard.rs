// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Post-guard narrowing of nullable locals/fields after divergent `isNil ifTrue: [^err]` (BT-2049),
//! extended to `isKindOf:` class-test guards (BT-2825), singleton-equality
//! (`==`/`=:=`/`/=`) and `respondsTo:` guards (BT-3369).

use super::common::*;

// ── BT-2049: Post-guard narrowing for locals and self.field after diverging guards ──

/// BT-2049: `x isNil ifTrue: [self error: "..."]` should narrow the local
/// `x` to non-Nil for subsequent statements, even though the block has no
/// `^` return — the `error:` call returns `Never` and therefore diverges.
#[test]
fn bt2049_local_narrows_after_self_error_guard() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [self error: "missing"]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`ms` should be narrowed to Integer after `self error:` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `self.field isNil ifTrue: [^err]` should narrow `self.field`
/// to non-Nil for subsequent statements, so passing it to a typed parameter
/// does not warn.
#[test]
fn bt2049_self_field_narrows_after_return_guard() {
    let source = r"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [^nil]
    Store process: self.eventStore
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`self.eventStore` should be narrowed to Integer after `^nil` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `self.field isNil ifTrue: [self error: "..."]` should narrow
/// `self.field` to non-Nil via the diverging-call path (no `^`).
#[test]
fn bt2049_self_field_narrows_after_self_error_guard() {
    let source = r#"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [self error: "no event store"]
    Store process: self.eventStore
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`self.eventStore` should be narrowed to Integer after `self error:` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: A non-diverging `ifTrue:` block (no `^`, last expression is not
/// `Never`) must NOT narrow the variable — the binding may still be Nil
/// when control falls through.
#[test]
fn bt2049_non_diverging_guard_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [42]
    Receiver process: ms
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    // The negative case must produce the specific argument-mismatch
    // diagnostic at the `process:` call site: `ms` should still be
    // `Integer | Nil` because the `ifTrue: [42]` block does not
    // diverge. A bare "any Type warning" check could hide regressions where
    // narrowing silently happens but some other Type warning appears.
    //
    // BT-2066: user-facing messages render the source-sympathetic `Nil`
    // spelling, not the canonical `UndefinedObject` hierarchy name. The
    // assertion below is deliberately strict — if the renderer regresses
    // and leaks `UndefinedObject` back into diagnostics, this test fails.
    let mismatch_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type)
                && d.message.contains("'process:'")
                && d.message.contains("Nil")
        })
        .collect();
    assert!(
        !mismatch_warnings.is_empty(),
        "non-diverging guard must leave `ms` nullable and warn at `process:`; \
         got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // BT-2066: the canonical `UndefinedObject` name must NOT leak into the
    // user-facing message. Guard against regressions explicitly.
    for d in &mismatch_warnings {
        assert!(
            !d.message.contains("UndefinedObject"),
            "user-facing diagnostic leaked canonical `UndefinedObject`: {}",
            d.message
        );
    }
}

/// BT-2049: Bare identifier `eventStore` (sugar for `self.eventStore`) should
/// also be narrowed after a `self.eventStore isNil ifTrue: [^nil]` guard,
/// because both spellings resolve through the same synthetic `self.field` key.
#[test]
fn bt2049_bare_field_narrows_after_guard() {
    let source = r"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [^nil]
    Store process: eventStore
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Bare `eventStore` should be narrowed to Integer after guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `block_diverges` must treat a block as diverging when a
/// `Never`-typed statement appears anywhere in the body, not only as the
/// trailing statement. `[self error: "...". 42]` — the trailing `42` looks
/// reachable but the block actually diverges at `self error:`.
#[test]
fn bt2049_guard_with_diverge_then_trailing_nondiverging() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [
      self error: "missing"
      42
    ]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "ms should narrow: non-tail `self error:` still diverges. got: {type_warnings:?}"
    );
}

/// BT-2049: `ifTrue: [diverge] ifFalse: [reassign to nil]` must NOT narrow
/// after the statement — execution reaches the next statement through the
/// `ifFalse:` branch, and the reassignment makes the variable nil again.
#[test]
fn bt2049_if_true_if_false_reassigning_false_branch_does_not_narrow() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil
      ifTrue: [self error: "missing"]
      ifFalse: [ms := nil]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    // Because the ifFalse: block reassigns `ms` to nil, we must still warn
    // on the `process:` call — otherwise we'd be unsound.
    //
    // BT-2066: user-facing messages render the source-sympathetic `Nil`
    // spelling, not the canonical `UndefinedObject` hierarchy name.
    let mismatch_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type)
                && d.message.contains("'process:'")
                && d.message.contains("Nil")
        })
        .collect();
    assert!(
        !mismatch_warnings.is_empty(),
        "ifFalse: branch reassigns `ms` to nil — `process:` call must still warn; got: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // BT-2066: the canonical `UndefinedObject` name must NOT leak into the
    // user-facing message.
    for d in &mismatch_warnings {
        assert!(
            !d.message.contains("UndefinedObject"),
            "user-facing diagnostic leaked canonical `UndefinedObject`: {}",
            d.message
        );
    }
}

// ── BT-2825: Post-guard narrowing for `isKindOf:` class-test guards ──

/// BT-2825 AC (a): `(x isKindOf: Integer) ifFalse: [^default]` should narrow
/// a union-typed local to `Integer` for the rest of the method — the
/// concrete guard-and-early-return shape from the issue
/// (`(coll isKindOf: List) ifFalse: [^""]`), which `apply_early_return_narrowing`
/// previously only handled for `isNil`, never for `isKindOf:`, and never for
/// a solo `ifFalse:` at all.
#[test]
fn bt2825_union_narrows_after_is_kind_of_if_false_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: x :: Integer | String =>
    (x isKindOf: Integer) ifFalse: [^nil]
    Receiver process: x
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to Integer after `(x isKindOf: Integer) ifFalse: [^nil]`, got: {type_warnings:?}"
    );
}

/// BT-2825 AC (b): a Protocol-typed local narrowed by an `isKindOf:` guard
/// against a concrete class must be assignable to that concrete class
/// afterward without an `@expect type` escape hatch — the exact
/// `prompt_renderer.bt` shape from the issue (`coll :: Printable | Nil`,
/// `(coll isKindOf: List) ifFalse: [^""]`, `items :: List := coll`).
#[test]
fn bt2825_protocol_narrows_after_is_kind_of_if_false_guard_no_expect_needed() {
    let source = r#"
Protocol define: Printable
  asString -> String

typed Object subclass: MyList
  asString => "a list"

typed Object subclass: Caller
  run: coll :: Printable | Nil -> String =>
    coll isNil ifTrue: [^""]
    (coll isKindOf: MyList) ifFalse: [^""]
    items :: MyList := coll
    items asString
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`coll` narrowed to MyList should be assignable to `items :: MyList` \
         without `@expect type`, got: {type_warnings:?}"
    );
}

/// BT-2825: `(x isKindOf: Integer) ifTrue: [<diverge>]` should narrow `x` to
/// the *complement* (`String`, via `difference`) for the rest of the
/// method — the mirror image of the `ifFalse:` guard, extending the
/// existing `ifTrue: [<diverge>]` machinery (previously `isNil`-only)
/// to `isKindOf:`.
#[test]
fn bt2825_is_kind_of_if_true_diverge_narrows_to_complement() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: String => v

typed Object subclass: Caller
  run: x :: Integer | String =>
    (x isKindOf: Integer) ifTrue: [self error: "was integer"]
    Receiver process: x
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to String (the complement of Integer) after the \
         diverging `ifTrue:` guard, got: {type_warnings:?}"
    );
}

/// BT-2825 review follow-up: the combined `ifTrue: [<diverge>] ifFalse:
/// [...]` shape (`is_if_true_if_false` in `apply_early_return_narrowing`)
/// shares its code path with the solo `ifTrue: [<diverge>]` case above, but
/// had no dedicated `isKindOf:` test — only the solo-`ifTrue:` and
/// solo-`ifFalse:` shapes were covered. Locks in that `x` still narrows to
/// the complement after the combined form, matching the docs table row
/// `x isKindOf: Foo ifTrue: [^...] ifFalse: [...]`.
#[test]
fn bt2825_is_kind_of_if_true_if_false_diverge_narrows_to_complement() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: String => v

typed Object subclass: Caller
  run: x :: Integer | String =>
    (x isKindOf: Integer) ifTrue: [self error: "was integer"] ifFalse: [Transcript show: "ok"]
    Receiver process: x
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to String (the complement of Integer) after the \
         combined `ifTrue: [<diverge>] ifFalse: [...]` guard, got: {type_warnings:?}"
    );
}

/// BT-2825 AC (c): `isKindOf:` used directly as an `ifTrue:`/`ifFalse:`
/// branch condition (not a guard-and-return) already narrows within the
/// branch (BT-1573/BT-2741/BT-2744) — this locks that behaviour in as an
/// end-to-end regression alongside the new post-guard cases above.
#[test]
fn bt2825_is_kind_of_narrows_inside_if_true_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: x :: Integer | String =>
    (x isKindOf: Integer) ifTrue: [Receiver process: x]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to Integer inside the `ifTrue:` branch, got: {type_warnings:?}"
    );
}

/// BT-2825: A non-diverging `ifFalse:` block (no `^`, last expression is not
/// `Never`) must NOT narrow the variable — the binding may still fail the
/// `isKindOf:` test when control falls through.
#[test]
fn bt2825_non_diverging_is_kind_of_if_false_guard_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: x :: Integer | String =>
    (x isKindOf: Integer) ifFalse: [42]
    Receiver process: x
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let mismatch_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type) && d.message.contains("'process:'")
        })
        .collect();
    assert!(
        !mismatch_warnings.is_empty(),
        "non-diverging `ifFalse:` guard must leave `x` as `Integer | String` \
         and warn at `process:`; got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// BT-2825 / ADR 0102 §5 (BT-2744): `x class =:= Integer`'s false branch
/// stays unnarrowed (a `Character` subclass could still be "not exactly
/// Integer" yet satisfy `isKindOf: Integer`) — so the post-guard case must
/// NOT narrow `x` after `ifTrue: [<diverge>]` either, unlike `isKindOf:`.
/// This locks in the `ClassTestKind::Exact` vs `KindOf` distinction for the
/// new post-guard path added by this issue.
#[test]
fn bt2825_class_eq_exact_if_true_diverge_does_not_narrow() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: x :: Number =>
    (x class =:= Integer) ifTrue: [self error: "was exactly integer"]
    Receiver process: x
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let mismatch_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type) && d.message.contains("'process:'")
        })
        .collect();
    assert!(
        !mismatch_warnings.is_empty(),
        "`class =:=`'s false branch must stay unnarrowed even post-guard; \
         `x` should still be `Number` at `process:`; got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// BT-2825 review follow-up: `apply_early_return_narrowing`'s new
/// `is_if_false` arm is gated on `info.is_nil_check || info.class_test.is_some()`,
/// so it also fires for plain `isNil` guards, not just `isKindOf:` — `x isNil
/// ifFalse: [^...]` is the mirror of the already-covered `isNil ifTrue:
/// [^...]` case, and narrows `x` to `Nil` (only the nil case falls through)
/// for the rest of the method. Locks in this implicit-but-correct behavior.
#[test]
fn bt2825_is_nil_if_false_diverge_narrows_to_nil() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Nil => v

typed Object subclass: Caller
  run: x :: Integer | Nil =>
    x isNil ifFalse: [^nil]
    Receiver process: x
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to Nil after `x isNil ifFalse: [^nil]`, got: {type_warnings:?}"
    );
}

// ── BT-3369: Post-guard narrowing for singleton-equality guards ──

/// BT-3369: the issue's exact repro shape — `pid == #undefined ifTrue:
/// [^false]` (loose equality, `==`) should narrow `pid` to `Integer` for the
/// rest of the method, closing the gap without requiring `@expect type`.
#[test]
fn bt3369_double_equal_narrows_after_if_true_return_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: pid :: Integer | #undefined =>
    pid == #undefined ifTrue: [^false]
    Receiver process: pid
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`pid` should narrow to Integer after `pid == #undefined ifTrue: [^false]`, got: {type_warnings:?}"
    );
}

/// BT-3369 AC: `x =:= #undefined ifTrue: [^...]` (strict equality) also
/// narrows post-guard — the post-guard gate was missing for *all*
/// `singleton_eq` guards, independent of the `==` operator extension.
#[test]
fn bt3369_strict_equal_narrows_after_if_true_return_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: pid :: Integer | #undefined =>
    pid =:= #undefined ifTrue: [^false]
    Receiver process: pid
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`pid` should narrow to Integer after `pid =:= #undefined ifTrue: [^false]`, got: {type_warnings:?}"
    );
}

/// BT-3369 AC: `x /= #undefined ifFalse: [^...]` (loose inequality, already
/// accepted by the detector before this issue) also narrows post-guard —
/// the mirror `ifFalse:` guard shape, not just `ifTrue:`.
#[test]
fn bt3369_loose_not_equal_narrows_after_if_false_return_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: pid :: Integer | #undefined =>
    pid /= #undefined ifFalse: [^false]
    Receiver process: pid
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`pid` should narrow to Integer after `pid /= #undefined ifFalse: [^false]`, got: {type_warnings:?}"
    );
}

/// BT-3369 AC: narrowing only removes the specific literal member compared —
/// a 3-member union `Integer | #south | #east` checked against `#south`
/// narrows to `Integer | #east`, not just `Integer` (the other singleton
/// member must survive).
#[test]
fn bt3369_multi_member_union_keeps_other_singleton_after_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer | #east -> Integer | #east => v

typed Object subclass: Caller
  run: d :: Integer | #south | #east =>
    d == #south ifTrue: [^nil]
    Receiver process: d
    true
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`d` should narrow to `Integer | #east` (not just `Integer`) after `d == #south ifTrue: [^nil]`, got: {type_warnings:?}"
    );
}

/// BT-3369 AC: chained guards narrow incrementally — checking `#south` then
/// `#east` in sequence on a 3-member union narrows down to the single
/// remaining member, `Integer`.
#[test]
fn bt3369_chained_guards_narrow_incrementally() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: d :: Integer | #south | #east =>
    d == #south ifTrue: [^nil]
    d == #east ifTrue: [^nil]
    Receiver process: d
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`d` should narrow to Integer after two chained singleton guards, got: {type_warnings:?}"
    );
}

/// BT-3369 AC: no narrowing attempted for an equality check against a
/// non-literal `Symbol`-typed value — `pid == atomName` (both operands
/// variables, neither a `#foo`-style literal) must not be detected as a
/// singleton-equality guard at all, since there's no specific literal to
/// subtract from the union (narrowing on an arbitrary Symbol value would be
/// unsound — the checker can't know all possible Symbol values). This is
/// guaranteed structurally by `symbol_literal()` requiring a literal AST
/// node on one side; this test locks that guarantee in for the guard path
/// this issue extends. (The already-covered literal case — `s =:= #foo` on
/// a bare `Symbol` receiver narrowing its false branch to the `Symbol \
/// #foo` negation, never collapsing further — is exercised by
/// `test_refine_singleton_eq_symbol_false_branch_is_negation` in
/// `narrowing_control_flow_legacy.rs`; `apply_early_return_narrowing`
/// reuses that exact same `refine_singleton_narrowing` function, so its
/// soundness carries over to the post-guard path for free.)
#[test]
fn bt3369_equality_against_non_literal_symbol_is_not_detected_as_narrowing() {
    let expr = msg_send(
        var("pid"),
        MessageSelector::Binary("==".into()),
        vec![var("atomName")],
    );
    assert!(
        TypeChecker::detect_narrowing(&expr).is_none(),
        "`pid == atomName` (neither operand a literal) must not be detected as narrowing"
    );
}

// ── BT-3369: Post-guard narrowing for `respondsTo:` guards ──

/// BT-3369: `(x respondsTo: #foo) ifFalse: [^default]` should narrow `x` to
/// `Dynamic` post-guard (no protocol registered for the selector), so a
/// subsequent send of that selector does not raise a DNU warning — the same
/// gap as singleton-equality, for `responded_selector` instead.
#[test]
fn bt3369_responds_to_if_false_guard_narrows_to_dynamic() {
    let source = r"
typed Object subclass: Caller
  run: x :: Object =>
    (x respondsTo: #customMethod) ifFalse: [^nil]
    x customMethod
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let dnu_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "`x` should narrow to Dynamic after `(x respondsTo: #customMethod) ifFalse: [^nil]`, \
         got DNU warnings: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// BT-3369 (matches BT-1833 in-branch behaviour): when exactly one protocol
/// requires the tested selector, `(x respondsTo: #foo) ifFalse: [^default]`
/// narrows `x` to that concrete `Protocol` type post-guard, not just
/// `Dynamic`.
#[test]
fn bt3369_responds_to_if_false_guard_narrows_to_protocol() {
    let source = r#"
Protocol define: Printable
  asString -> String

typed Object subclass: Caller
  run: x :: Object -> String =>
    (x respondsTo: #asString) ifFalse: [^""]
    x asString
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`x` should narrow to Printable after `(x respondsTo: #asString) ifFalse: [^\"\"]`, \
         got: {type_warnings:?}"
    );
}

/// BT-3369 AC: `(x respondsTo: #foo) ifTrue: [^default]` correctly still
/// does NOT narrow `x` post-guard — there is no sound complement type for
/// "doesn't respond to X" (mirrors the already-locked-in in-branch
/// behaviour from BT-1833/`bt2825_non_diverging_is_kind_of_if_false_guard_does_not_narrow`-style
/// negative tests). `x customMethod` must still raise a DNU warning.
#[test]
fn bt3369_responds_to_if_true_guard_does_not_narrow() {
    let source = r"
typed Object subclass: Caller
  run: x :: Object =>
    (x respondsTo: #customMethod) ifTrue: [^nil]
    x customMethod
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse(&module);
    let dnu_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        !dnu_warnings.is_empty(),
        "`x` must stay unnarrowed after `(x respondsTo: #customMethod) ifTrue: [^nil]` — no \
         sound complement type exists for \"doesn't respond to X\"; got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}
