// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4000: an actor conditional whose receiver advances `State`, as a loop
//! body statement.
//!
//! `[n > 0] whileTrue: [(self done) ifTrue: [Timer sleep: 0]]` in an actor
//! instance method panicked the debug verifier (`UnboundVersion State1`). The
//! conditional's producer compiled its receiver's self-send prelude into the
//! conditional's own tuple document, so the dispatch's `State` `Bind` was out
//! of the loop frame's sight: the conditional's `State` extraction read a
//! version no `Bind` of the frame produced, and the loop's recursive call
//! carried the pre-statement `State` (release builds dropped the self-send's
//! state change).
//!
//! **Invariant** (every scope kind): a statement whose evaluation produces a
//! `State` version binds it in the frame the statement runs in, before anything
//! reads it. A conditional's receiver runs first and unconditionally, so its
//! prelude is spliced into the caller's frame (`conditional_receiver_operand`,
//! the receiver-prelude splice BT-3910 introduced for local-assignment
//! receivers) at the method root, in a conditional arm, a `Letrec` loop body, a
//! fold body and an `on:do:`/`ensure:` arm alike.
//!
//! [`loop_body_conditional_receiver_corpus_compiles`] proves it over receiver
//! form x conditional x arm write x loop kind x actor context: every row passes
//! the debug verifier and compiles through `erlc`.
//! [`loop_body_conditional_receiver_corpus_reports_the_pre_fix_lowering`] shows
//! the verifier catches the pre-fix lowering
//! ([`LocalRebindFault::ReceiverPreludeInTuple`]) over the same rows.

use super::{assert_compiles_through_erlc, codegen};
#[cfg(debug_assertions)]
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use std::panic::{AssertUnwindSafe, catch_unwind};

const HELPERS: &str = "  done => false
  isAlive: k => k > 0
  maybe: k => nil
  + k => k > 0
";

/// Conditional receivers whose evaluation advances `State`: a unary, keyword
/// and binary self-send, and a self-send as the operand of `and:` / `or:`.
const RECEIVERS: &[&str] = &[
    "(self done)",
    "(self isAlive: n)",
    "(self + n)",
    "((self isAlive: n) and: [n > 1])",
    "((self done) or: [n > 1])",
];

/// The conditionals over a receiver `{R}` and an arm body `{B}`.
const CONDITIONALS: &[&str] = &[
    "{R} ifTrue: [{B}]",
    "{R} ifFalse: [{B}]",
    "{R} ifTrue: [{B}] ifFalse: [nil]",
    "{R} ifNil: [{B}]",
    "{R} and: [{B}. true]",
    "{R} or: [{B}. false]",
];

/// What the arm writes: nothing, an outer local, a field.
const ARMS: &[&str] = &["Timer sleep: 0", "n := n - 1", "self.f := self.f + 1"];

/// Where the conditional statement `{S}` runs: every `Letrec` loop kind
/// (`LOOPS[..5]`), a fold body, the method root, a conditional arm, an
/// `on:do:` body, an `ensure:` body, and a `Letrec` loop nested in a
/// conditional arm.
const LOOPS: &[&str] = &[
    "[n > 0] whileTrue: [{S}]",
    "[n <= 0] whileFalse: [{S}]",
    "3 timesRepeat: [{S}]",
    "1 to: 3 do: [:i | {S}]",
    "1 to: 5 by: 2 do: [:i | {S}]",
    "#(1, 2) do: [:e | {S}]",
    "{S}",
    "(n > 0) ifTrue: [{S}]",
    "[{S}] on: Error do: [:err | 0]",
    "[{S}] ensure: [nil]",
    "(n > 0) ifTrue: [3 timesRepeat: [{S}]]",
];

/// The actor contexts: a method whose first `State` version is the
/// conditional's receiver, and one that advances `State` first.
const PRELUDES: &[(&str, &str)] = &[("actor", ""), ("actor_field_write", "    self.f := 1\n")];

const HEADER: &str = "Actor subclass: LoopCondReceiverProbe\n  state: f = 0\n";

/// Every `(name, loop source)` row.
fn rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (c, cond) in CONDITIONALS.iter().enumerate() {
                for (a, arm) in ARMS.iter().enumerate() {
                    let stmt = cond.replace("{R}", receiver).replace("{B}", arm);
                    out.push((
                        format!("loop{l} recv{r} cond{c} arm{a}"),
                        lp.replace("{S}", &stmt),
                    ));
                }
            }
        }
    }
    out
}

fn method(prelude: &str, i: usize, body: &str) -> String {
    format!("  probe{i} =>\n{prelude}    n := 3\n    {body}\n    n\n")
}

/// Every generated row must be well-formed source, so a placeholder slip
/// cannot turn rows into parser recoveries that pass vacuously.
fn assert_parses(src: &str) {
    let (_, diags) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    assert!(diags.is_empty(), "malformed corpus row:\n{src}\n{diags:?}");
}

fn passes(src: &str) -> bool {
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(src));
    }))
    .is_ok()
}

/// BT-4000: every row passes the debug verifier and compiles through `erlc`
/// (one run per context; bisected to the failing rows on failure).
#[test]
fn loop_body_conditional_receiver_corpus_compiles() {
    let rows = rows();
    assert!(rows.len() >= 600, "the corpus has thinned out");
    for (_, body) in &rows {
        assert_parses(&format!("{HEADER}{HELPERS}{}", method("", 0, body)));
    }
    let mut failures = Vec::new();
    for (ctx, prelude) in PRELUDES {
        let methods: Vec<String> = rows
            .iter()
            .enumerate()
            .map(|(i, (_, body))| method(prelude, i, body))
            .collect();
        if passes(&format!("{HEADER}{HELPERS}{}", methods.concat())) {
            continue;
        }
        failures.extend(rows.iter().zip(&methods).filter_map(|((name, _), method)| {
            let single = format!("{HEADER}{HELPERS}{method}");
            (!passes(&single)).then(|| format!("{ctx} / {name}:\n{single}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} loop-body conditional-receiver row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Whether the ADR 0131/0133 Phase 0 check rejects `src` before codegen (an
/// arm's outer-local write in an operand position no construct threads).
fn phase0_rejects(src: &str) -> bool {
    use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .any(|d| {
            d.severity == Severity::Error
                && matches!(
                    d.category,
                    Some(
                        DiagnosticCategory::UnmigratedLocalThreading
                            | DiagnosticCategory::Tier2BlockNoReturnChannel
                    )
                )
        })
}

/// The conditional `{C}` in value position inside the body statement: an
/// assigned local, a field write, a self-send argument, the last statement of
/// a nested block value, and a non-local return. (Multi-character
/// placeholders: a letter would also match receiver text such as `isAlive`.)
const VALUE_POSITIONS: &[&str] = &[
    "r := {C}",
    "self.f := ({C}) printString size",
    "self isAlive: ({C}) printString size",
    "x := 0. x := {C}. x",
    "(n > 5) ifTrue: [^ {C}]",
    // The RHS of a field write and of a destructuring statement (`{P}` is the
    // conditional answering a two-element array): the arm's C6 -> C1 and C9
    // reroutes (`is_conditional_with_sequenced_receiver`).
    "self.f := {C}",
    "#(a, b) := {P}. self.f := a + b",
];

/// BT-4000: the conditional in value position (every [`VALUE_POSITIONS`]
/// wrapper) in every position of [`LOOPS`], over every receiver with
/// `ifTrue:ifFalse:` and each arm write. Every row passes the debug verifier
/// and compiles through `erlc`.
#[test]
fn loop_body_conditional_receiver_value_positions_compile() {
    let mut methods = Vec::new();
    let mut names = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (a, arm) in ARMS.iter().enumerate() {
                let cond = format!("{receiver} ifTrue: [{arm}. 1] ifFalse: [2]");
                let pair = format!("{receiver} ifTrue: [{arm}. #(1, 2)] ifFalse: [#(3, 4)]");
                for (v, wrap) in VALUE_POSITIONS.iter().enumerate() {
                    let stmt = wrap.replace("{C}", &cond).replace("{P}", &pair);
                    let m = method("", methods.len(), &lp.replace("{S}", &stmt));
                    assert_parses(&format!("{HEADER}{HELPERS}{m}"));
                    if phase0_rejects(&format!("{HEADER}{HELPERS}{m}")) {
                        continue;
                    }
                    names.push(format!("loop{l} recv{r} arm{a} value{v}"));
                    methods.push(m);
                }
            }
        }
    }
    assert!(
        methods.len() >= 400,
        "only {} rows; the corpus has thinned out",
        methods.len()
    );
    let mut failures = Vec::new();
    if !passes(&format!("{HEADER}{HELPERS}{}", methods.concat())) {
        failures.extend(names.iter().zip(&methods).filter_map(|(name, method)| {
            let single = format!("{HEADER}{HELPERS}{method}");
            (!passes(&single)).then(|| format!("{name}:\n{single}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} value-position row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4000: with the receiver prelude rendered into the conditional's tuple
/// again ([`LocalRebindFault::ReceiverPreludeInTuple`]), the verifier reports
/// `UnboundVersion` for every row whose conditional is a statement of a
/// `Letrec` loop body, a conditional arm or an `on:do:`/`ensure:` body (the
/// frames the bug reached). The check that was missing already existed, and
/// it is what catches the pre-fix lowering, not the repro. The fold body and
/// the method root (`LOOPS[5]`, `LOOPS[6]`) are verified with opaque version
/// gaps, so the corpus run is their proof.
#[cfg(debug_assertions)]
#[test]
fn loop_body_conditional_receiver_corpus_reports_the_pre_fix_lowering() {
    let mut missed = Vec::new();
    for (name, body) in rows()
        .into_iter()
        .filter(|(name, _)| !name.starts_with("loop5 ") && !name.starts_with("loop6 "))
    {
        let src = format!("{HEADER}{HELPERS}{}", method("", 0, &body));
        let report = super::verifier_report(&src, Some(LocalRebindFault::ReceiverPreludeInTuple));
        if !report
            .as_deref()
            .is_some_and(|r| r.contains("UnboundVersion"))
        {
            missed.push(format!("{name}: {report:?}\n{body}"));
        }
    }
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the pre-fix lowering:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// The issue's repros (BT-4000), each with a silent verifier and through
/// `erlc`.
#[test]
fn bt4000_repros_compile() {
    for body in [
        "[n > 0] whileTrue: [(self done) ifTrue: [Timer sleep: 0]]",
        "3 timesRepeat: [(self done) ifTrue: [Timer sleep: 0]]",
        "[n > 0] whileTrue: [(self done) ifTrue: [n := 1]]",
        "[n > 0] whileTrue: [self done ifTrue: [3]]",
    ] {
        let src = format!("{HEADER}{HELPERS}{}", method("", 0, body));
        assert_eq!(super::verifier_report(&src, None), None, "{src}");
        assert!(passes(&src), "{src}");
    }
}

// ─── BT-4017: every fold kind, and a non-literal block argument ─────────────
//
// `#(1, 2) do: [:e | (self done) ifTrue: blk]` raised `badarg` at run time:
// the fold body classified the conditional as a construct answering
// `{Result, StateAcc}` and unpacked `element(2, ...)` of it, but with a
// non-literal block argument the conditional compiles to a plain `ifTrue:`
// send. The fold body now lowers every statement `thread_ahead` registers
// unwrapped (`thread_ahead_unwraps`) through `thread_ahead`, as the other
// frames do (`fold_statement_threads_ahead`).
//
// The verifier cannot see this bug: the shape of a run-time value is not in
// the IR, the fold body's raw-tuple unpack is an opaque statement, and `erlc`
// accepts `element(2, X)` of any `X`. The proof is a codegen-shape check
// ([`fold_raw_tuple_unpacks`]) over the generated corpus, shown to catch the
// pre-fix routing ([`LocalRebindFault::FoldRawTupleUnpack`]), plus the runtime
// rows in `stdlib/test/bt4017fold_body_block_arg_test.bt`.

/// Every fold kind, with the conditional statement `{S}` as the body's last
/// statement (the kind's value) and before the kind's own tail.
const FOLDS: &[&str] = &[
    "#(1, 2) do: [:e | {S}]",
    "#(1, 2) do: [:e | {S}. e]",
    "#{#a => 1} keysAndValuesDo: [:k :w | {S}]",
    "#(1, 2) collect: [:e | {S}]",
    "#(1, 2) collect: [:e | {S}. e]",
    "#(1, 2) inject: 0 into: [:a :e | {S}]",
    "#(1, 2) inject: 0 into: [:a :e | {S}. a + e]",
    "#(1, 2) select: [:e | {S}]",
    "#(1, 2) reject: [:e | {S}. true]",
    "#(1, 2) anySatisfy: [:e | {S}]",
    "#(1, 2) allSatisfy: [:e | {S}. true]",
    "#(1, 2) detect: [:e | {S}]",
    "#(1, 2) detect: [:e | {S}. false] ifNone: [0]",
    "#(1, 2) count: [:e | {S}]",
    "#(1, 2) takeWhile: [:e | {S}]",
    "#(1, 2) dropWhile: [:e | {S}. false]",
    "#(1, 2) partition: [:e | {S}]",
    "#(1, 2) groupBy: [:e | {S}]",
    "#(2, 1) sort: [:a :b | {S}. a < b]",
];

/// Conditionals over a receiver `{R}` with a non-literal block argument (the
/// method's `blk` parameter), alone and next to a literal arm. (A literal arm
/// that writes a field next to a non-literal one is not inlined, so it is
/// `FieldAssignmentInUnsupportedBlock`, a compile error.)
const BLOCK_ARG_CONDITIONALS: &[&str] = &[
    "{R} ifTrue: blk",
    "{R} ifFalse: blk",
    "{R} ifTrue: blk ifFalse: blk",
    "{R} ifTrue: [0] ifFalse: blk",
    "{R} ifNil: blk",
    "{R} and: blk",
    "{R} or: blk",
];

/// The fold-body statements, each with whether the pre-BT-4017 fold body
/// routed it to the raw-tuple unpack: every non-literal-block conditional
/// (it did), and every literal-arm conditional whose arm writes a field (it
/// did; element 2 of that producer's tuple was right) or a local (it did not:
/// a local-threading producer already went through `thread_ahead`).
/// BT-4000's fold row covered `do:` only.
fn fold_statements() -> Vec<(String, bool)> {
    let mut out: Vec<(String, bool)> = BLOCK_ARG_CONDITIONALS
        .iter()
        .map(|c| ((*c).to_string(), true))
        .collect();
    for cond in CONDITIONALS {
        out.push((cond.replace("{B}", ARMS[1]), false));
        out.push((cond.replace("{B}", ARMS[2]), true));
    }
    out
}

fn block_arg_method(prelude: &str, i: usize, body: &str) -> String {
    format!("  probe{i}: blk =>\n{prelude}    n := 3\n    {body}\n    n\n")
}

/// Fold-body statements this change also reroutes that have no sequenced
/// receiver: an inline-threaded conditional over a plain receiver whose arm
/// writes a field, and a `match:` whose arm does. The pre-BT-4017 fold body
/// sent them to the raw-tuple unpack too (correct there: they are
/// producers whose element 2 is the new state); they now go through
/// `thread_ahead` like every other `thread_ahead_unwraps` statement.
const PLAIN_RECEIVER_STATEMENTS: &[&str] = &[
    "(n > 0) ifTrue: [self.f := self.f + 1]",
    "(n > 0) ifTrue: [self.f := self.f + 1] ifFalse: [nil]",
    "(n > 0) and: [self.f := self.f + 1. true]",
    "n match: [3 -> self.f := self.f + 1; _ -> 0]",
];

/// `(name, method body)` rows: every fold kind x receiver x fold statement,
/// and every other frame of [`LOOPS`] x receiver x non-literal-block
/// conditional (BT-4000 fixed those; pinned here so they stay fixed).
fn block_arg_rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    let statements = fold_statements();
    for (l, fold) in FOLDS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (s, (stmt, raw_pre_fix)) in statements.iter().enumerate() {
                let tag = if *raw_pre_fix { "raw" } else { "producer" };
                out.push((
                    format!("fold{l} recv{r} stmt{s} {tag}"),
                    fold.replace("{S}", &stmt.replace("{R}", receiver)),
                ));
            }
        }
        for (s, stmt) in PLAIN_RECEIVER_STATEMENTS.iter().enumerate() {
            out.push((format!("fold{l} plain{s} raw"), fold.replace("{S}", stmt)));
        }
    }
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (c, cond) in BLOCK_ARG_CONDITIONALS.iter().enumerate() {
                out.push((
                    format!("loop{l} recv{r} blockarg{c}"),
                    lp.replace("{S}", &cond.replace("{R}", receiver)),
                ));
            }
        }
    }
    out
}

/// The codegen-shape check: how many raw `{Value, StateAcc}` unpacks of a
/// statement (`lower_foldl_body`'s `CondResult` tuple) `code` holds. A
/// conditional with a sequenced receiver is never one (BT-4017), so for every
/// row of [`block_arg_rows`] this is zero.
fn fold_raw_tuple_unpacks(code: &str) -> usize {
    code.matches("_CondResult").count()
}

/// The rows Phase 0 accepts, as `(name, single-method source)`, each checked
/// to be well-formed.
fn block_arg_sources(prelude: &str) -> Vec<(String, String)> {
    block_arg_rows()
        .into_iter()
        .enumerate()
        .filter_map(|(i, (name, body))| {
            let src = format!("{HEADER}{HELPERS}{}", block_arg_method(prelude, i, &body));
            assert_parses(&src);
            (!phase0_rejects(&src)).then_some((name, src))
        })
        .collect()
}

/// BT-4017: every row passes the debug verifier, compiles through `erlc`, and
/// holds no raw-tuple unpack of its conditional (one module per context;
/// bisected to the failing rows on failure).
#[test]
fn fold_body_block_arg_corpus_compiles_without_a_raw_tuple_unpack() {
    let mut failures = Vec::new();
    for (ctx, prelude) in PRELUDES {
        let rows = block_arg_sources(prelude);
        assert!(
            rows.len() >= 1500,
            "only {} rows; the corpus has thinned out",
            rows.len()
        );
        let methods: Vec<String> = rows
            .iter()
            .map(|(_, src)| src[HEADER.len() + HELPERS.len()..].to_string())
            .collect();
        let module = format!("{HEADER}{HELPERS}{}", methods.concat());
        let whole = catch_unwind(AssertUnwindSafe(|| codegen(&module)));
        if let Ok(code) = &whole
            && fold_raw_tuple_unpacks(code) == 0
            && passes(&module)
        {
            continue;
        }
        failures.extend(rows.iter().filter_map(|(name, src)| {
            let ok = catch_unwind(AssertUnwindSafe(|| codegen(src)))
                .is_ok_and(|code| fold_raw_tuple_unpacks(&code) == 0)
                && passes(src);
            (!ok).then(|| format!("{ctx} / {name}:\n{src}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} block-argument row(s) fail verified codegen or hold a raw-tuple unpack:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4017: with the fold body's pre-fix routing restored
/// ([`LocalRebindFault::FoldRawTupleUnpack`]), the fold rows the pre-fix routing
/// sent to the raw-tuple unpack are one again, which the codegen-shape check
/// reports, while the verifier stays silent: the reason the proof is a shape
/// check and a runtime test rather than a `VerifyError`. Under ADR 0137 a fold
/// whose blocks only read state or call out is no longer a threaded fold but a
/// closure on the actor-state cell, so the fault has nothing to route for those
/// rows: the proof is that the shape check still sees the fault in the rows that
/// still thread, and that the verifier never does.
#[cfg(debug_assertions)]
#[test]
fn fold_body_block_arg_corpus_reports_the_pre_fix_routing() {
    use crate::core_erlang::threaded_expr::fault;
    let mut verifier_saw = Vec::new();
    let mut detected = 0usize;
    let rows: Vec<_> = block_arg_sources("")
        .into_iter()
        .filter(|(name, _)| name.starts_with("fold") && name.ends_with(" raw"))
        .collect();
    assert!(rows.len() >= 900, "only {} rows", rows.len());
    let total = rows.len();
    for (name, src) in rows {
        if let Some(report) =
            super::verifier_report(&src, Some(LocalRebindFault::FoldRawTupleUnpack))
        {
            verifier_saw.push(format!("{name}: the verifier saw it ({report})\n{src}"));
            continue;
        }
        let code = fault::with(LocalRebindFault::FoldRawTupleUnpack, || codegen(&src));
        if fold_raw_tuple_unpacks(&code) > 0 {
            detected += 1;
        }
    }
    assert!(
        verifier_saw.is_empty(),
        "{} row(s) where the verifier saw the pre-fix routing:\n{}",
        verifier_saw.len(),
        verifier_saw.join("\n")
    );
    assert!(
        detected >= 200,
        "the shape check sees the pre-fix routing in only {detected} of {total} rows"
    );
}

/// The issue's repro (BT-4017), with a silent verifier, through `erlc` and
/// without a raw-tuple unpack.
#[test]
fn bt4017_repro_compiles() {
    let src = "Actor subclass: Nonlit\n  state: f = 0\n  done =>\n    self.f := self.f + 1\n    true\n  fold: blk =>\n    #(1, 2) do: [:e | (self done) ifTrue: blk]\n    self.f\n";
    assert_eq!(super::verifier_report(src, None), None, "{src}");
    assert_eq!(fold_raw_tuple_unpacks(&codegen(src)), 0, "{src}");
    assert!(passes(src), "{src}");
}

// ─── BT-4017: the fold-body rows in every other scope kind ──────────────────
//
// `fold_statement_threads_ahead` is not actor-only: in a value-type method
// (`SelfVt` arm writes), a `TestCase` method, and a class method (class
// variables are written in place, ADR 0130) a fold body statement for which
// `thread_ahead_unwraps` holds (an inline-threaded conditional or `match:`, an
// `on:do:`/`ensure:` or `[...] value` producer) is rerouted too. The same rows
// run there, with the same checks.

/// A non-actor method context: the class header (with a field or class
/// variable `f`), its helpers, whether methods are class-side, and a statement
/// prefixed to each method.
struct ScopeContext {
    name: &'static str,
    header: &'static str,
    helpers: &'static str,
    class_side: bool,
    prelude: &'static str,
}

const INSTANCE_HELPERS: &str = "  done => false
  isAlive: k => k > 0
  + k => k > 0
";

const CLASS_HELPERS: &str = "  class done => false
  class isAlive: k => k > 0
  class + k => k > 0
";

const SCOPE_CONTEXTS: &[ScopeContext] = &[
    ScopeContext {
        name: "value_field",
        header: "Value subclass: FoldScopeProbe\n  field: f = 0\n",
        helpers: INSTANCE_HELPERS,
        class_side: false,
        prelude: "",
    },
    ScopeContext {
        name: "value_field_write",
        header: "Value subclass: FoldScopeProbe\n  field: f = 0\n",
        helpers: INSTANCE_HELPERS,
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    ScopeContext {
        name: "test_case",
        header: "TestCase subclass: FoldScopeProbe\n  field: f = 0\n",
        helpers: INSTANCE_HELPERS,
        class_side: false,
        prelude: "",
    },
    ScopeContext {
        name: "test_case_field_write",
        header: "TestCase subclass: FoldScopeProbe\n  field: f = 0\n",
        helpers: INSTANCE_HELPERS,
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    ScopeContext {
        name: "object_class_side",
        header: "Object subclass: FoldScopeProbe\n  classState: f = 0\n",
        helpers: CLASS_HELPERS,
        class_side: true,
        prelude: "",
    },
    ScopeContext {
        name: "actor_class_side",
        header: "Actor subclass: FoldScopeProbe\n  classState: f = 0\n",
        helpers: CLASS_HELPERS,
        class_side: true,
        prelude: "",
    },
];

/// Fold-body statements that are `thread_ahead_unwraps` producers outside an
/// actor too: an `on:do:`/`ensure:` body and an evaluated literal block that
/// write the field (the `SelfVt` family in a value type).
const SCOPE_PRODUCER_STATEMENTS: &[&str] = &[
    "[self.f := self.f + 1] on: Error do: [:err | 0]",
    "[self.f := self.f + 1] ensure: [nil]",
    "[self.f := self.f + 1] value",
];

/// How one row of a non-actor scope compiles.
enum ScopeCodegen {
    /// Codegen succeeded; the generated module.
    Compiles(String),
    /// The Phase 0 check, or codegen with a structured (non-internal) error,
    /// rejects the program: a language rule, not a lowering failure.
    Rejected,
}

/// Compiles one program (debug: the verifier panics on a finding); `Err` with
/// the panic or internal error.
fn scope_codegen(src: &str) -> Result<ScopeCodegen, String> {
    if phase0_rejects(src) {
        return Ok(ScopeCodegen::Rejected);
    }
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    let compiled = catch_unwind(AssertUnwindSafe(|| {
        crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test"),
        )
    }));
    match compiled {
        Ok(Ok(code)) => Ok(ScopeCodegen::Compiles(code)),
        Ok(Err(crate::core_erlang::CodeGenError::Internal(msg))) => Err(msg),
        Ok(Err(_)) => Ok(ScopeCodegen::Rejected),
        Err(payload) => Err(payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()),
    }
}

/// `(name, method body)`: every fold frame x (the actor corpus's fold
/// statements over every receiver, the plain-receiver / `match:` statements,
/// and [`SCOPE_PRODUCER_STATEMENTS`]).
fn scope_rows() -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = block_arg_rows()
        .into_iter()
        .filter(|(name, _)| name.starts_with("fold"))
        .collect();
    for (l, fold) in FOLDS.iter().enumerate() {
        for (s, stmt) in SCOPE_PRODUCER_STATEMENTS.iter().enumerate() {
            out.push((format!("fold{l} producer{s}"), fold.replace("{S}", stmt)));
        }
    }
    out
}

fn scope_method(ctx: &ScopeContext, i: usize, body: &str) -> String {
    let side = if ctx.class_side { "class " } else { "" };
    format!(
        "  {side}probe{i}: blk =>\n{}    n := 3\n    {body}\n    n\n",
        ctx.prelude
    )
}

/// BT-4017: in every non-actor scope kind, every fold row passes the debug
/// verifier, compiles through `erlc` and holds no raw-tuple unpack, except a
/// row writing the field `f` inside the fold body in a value-type or `TestCase`
/// method, which a language rule rejects with a structured error (class-side,
/// `f` is a class variable written in place, and every row compiles).
/// (One `erlc` module per context; bisected to the failing rows on failure.)
#[test]
fn fold_body_statements_in_every_scope_compile_without_a_raw_tuple_unpack() {
    let rows = scope_rows();
    let field_write_rows = rows
        .iter()
        .filter(|(_, body)| body.contains("self.f :="))
        .count();
    // Guard against the corpus thinning out: exact per-context accounting
    // below only proves the rows that exist, so pin a floor on the row set
    // and on the rows every context must compile (1938 / 1235 today).
    assert!(
        rows.len() >= 1500,
        "scope corpus shrank to {} rows; expected >= 1500",
        rows.len()
    );
    assert!(
        rows.len() - field_write_rows >= 1000,
        "only {} of {} scope rows are non-field-write rows; expected >= 1000",
        rows.len() - field_write_rows,
        rows.len()
    );
    let mut failures = Vec::new();
    let mut counts = Vec::new();
    for ctx in SCOPE_CONTEXTS {
        let mut compiled = Vec::new();
        let mut rejected = 0usize;
        for (i, (name, body)) in rows.iter().enumerate() {
            let m = scope_method(ctx, i, body);
            let src = format!("{}{}{m}", ctx.header, ctx.helpers);
            assert_parses(&src);
            match scope_codegen(&src) {
                // Only a field write nested in the fold body of an instance
                // method is refused by a language rule (a value type's slots
                // are immutable; a `TestCase` field write in a nested block
                // has no channel back). Class-side, `f` is a class variable
                // written in place, so every row must compile.
                Ok(ScopeCodegen::Rejected) if !ctx.class_side && body.contains("self.f :=") => {
                    rejected += 1;
                }
                Ok(ScopeCodegen::Rejected) => {
                    failures.push(format!("{} / {name}: rejected\n{src}", ctx.name));
                }
                Ok(ScopeCodegen::Compiles(code)) => {
                    if fold_raw_tuple_unpacks(&code) == 0 {
                        compiled.push((name, m, src));
                    } else {
                        failures.push(format!("{} / {name}: raw-tuple unpack\n{src}", ctx.name));
                    }
                }
                Err(e) => failures.push(format!("{} / {name}: {e}\n{src}", ctx.name)),
            }
        }
        counts.push(format!(
            "{}: {} compile, {rejected} rejected",
            ctx.name,
            compiled.len()
        ));
        // Exact accounting: every row either compiles or is one of the
        // rejected field-write rows, and an instance context rejects all of
        // them (none slips through to compile), a class-side context none.
        let expected_rejected = if ctx.class_side { 0 } else { field_write_rows };
        if compiled.len() + rejected != rows.len() || rejected != expected_rejected {
            failures.push(format!(
                "{}: {} compile + {rejected} rejected of {} rows; expected {} compile + \
                 {expected_rejected} rejected",
                ctx.name,
                compiled.len(),
                rows.len(),
                rows.len() - expected_rejected
            ));
        }
        let methods: String = compiled.iter().map(|(_, m, _)| m.as_str()).collect();
        if !passes(&format!("{}{}{methods}", ctx.header, ctx.helpers)) {
            failures.extend(
                compiled
                    .iter()
                    .filter(|(_, _, src)| !passes(src))
                    .map(|(name, _, src)| format!("{} / {name}: erlc\n{src}", ctx.name)),
            );
        }
    }
    println!(
        "BT-4017 scope rows ({} per context): {}",
        rows.len(),
        counts.join("; ")
    );
    assert!(
        failures.is_empty(),
        "{} fold row(s) fail in a non-actor scope:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
