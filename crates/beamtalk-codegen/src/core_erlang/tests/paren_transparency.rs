// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4059: redundant parentheses around a send's receiver or argument are
//! semantically transparent, so `(e)` must compile exactly like `e`.
//!
//! A differential property over corpora of method bodies: for every message
//! send (and every cascade message) in a module, a variant wraps one slot —
//! the receiver, or one argument — in an extra `Expression::Parenthesized`
//! (what the parser builds for `( … )`), with the wrapper's span equal to the
//! wrapped expression's so no location can leak into the output. The variant's
//! Core Erlang (or its codegen error, or its debug-verifier panic) must be
//! byte-identical to the unwrapped module's, apart from what derives from the
//! method's reflected source text (`without_reflected_text`): reflected source
//! is the text as written, so a wrapped variant's legitimately shows the added
//! parentheses.
//!
//! A `super` receiver is never wrapped: `(super) foo` is not a super send.
//!
//! Each source is first checked with every slot of every method wrapped at
//! once, then each method with all its slots wrapped; only a method whose
//! output differs is drilled into slot by slot, so the cost is two compiles
//! per source on a transparent compiler. (A slot whose own difference another
//! wrapped slot cancels out would be missed by that shortcut; every reported
//! row is a real, single-slot difference.)
//!
//! Corpora:
//! - [`TEMPLATE_HOSTS`] x [`TEMPLATE_BODY`] (fast tier): every class kind
//!   (Object, Actor, Value), instance and class side, with receivers that are
//!   `self`, the own class, another class, a local, a literal, a cascade, a
//!   block, for every selector shape, at top level and inside blocks and
//!   loops;
//! - `synced_call_sites_corpus`'s construct matrix, statement corpus and
//!   generated programs (`BT4059_PROGRAMS`, default 1024), and every
//!   `stdlib/test` source with its fixtures (slow tier, the `paren_*_corpus`
//!   tests, one per corpus).
//!
//! Each corpus compiles through self-sufficient codegen ([`outcome`]); the
//! templates, the construct matrix and the stdlib corpora also compile the
//! way a driver does, through `analyse_full` and the lowering
//! ([`outcome_analysed`]), where analysis facts keyed by span would show a
//! wrapper one side saw and the other did not.
//!
//! Each report counts the rows whose wrapped variant failed to compile (`ERR`)
//! or panicked (`PANIC`) apart from those that merely compiled differently
//! (BT-4091). Before the operand-paren normalisation pass
//! (`beamtalk_core::ast::normalize`), 261 of 604 template rows differed.

use super::synced_call_sites_corpus::{construct_sources, generated_sources_of, statement_sources};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{ClassKind, Expression, ExpressionStatement, MessageSelector, Module};
use beamtalk_core::semantic_analysis::{
    AnalysisContext, analyse_full, lower_analysed_module_for_codegen,
};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Applies `f` to every wrappable slot under `expr`, children first (so a
/// slot is visited before the slot that contains it, and wrapping it never
/// re-visits it).
fn walk_slots(expr: &mut Expression, f: &mut dyn FnMut(&mut Expression)) {
    match expr {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_slots(receiver, f);
            for a in arguments.iter_mut() {
                walk_slots(a, f);
            }
            if !matches!(receiver.as_ref(), Expression::Super(_)) {
                f(receiver);
            }
            for a in arguments.iter_mut() {
                f(a);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // The cascade's `receiver` is its first send; that send's own
            // receiver (the cascade's target) is a slot through the
            // `MessageSend` arm above.
            walk_slots(receiver, f);
            for m in messages.iter_mut() {
                for a in &mut m.arguments {
                    walk_slots(a, f);
                }
                for a in &mut m.arguments {
                    f(a);
                }
            }
        }
        Expression::FieldAccess { receiver, .. } => walk_slots(receiver, f),
        Expression::Block(block) => walk_body(&mut block.body, f),
        Expression::Assignment { value, .. }
        | Expression::DestructureAssignment { value, .. }
        | Expression::Return { value, .. } => walk_slots(value, f),
        Expression::Parenthesized { expression, .. } => walk_slots(expression, f),
        Expression::Match { value, arms, .. } => {
            walk_slots(value, f);
            for arm in arms {
                if let Some(g) = &mut arm.guard {
                    walk_slots(g, f);
                }
                walk_slots(&mut arm.body, f);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for p in pairs {
                walk_slots(&mut p.key, f);
                walk_slots(&mut p.value, f);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for e in elements {
                walk_slots(e, f);
            }
            if let Some(t) = tail {
                walk_slots(t, f);
            }
        }
        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            for e in elements {
                walk_slots(e, f);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for s in segments {
                if let beamtalk_core::ast::StringSegment::Interpolation(e) = s {
                    walk_slots(e, f);
                }
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

fn walk_body(body: &mut [ExpressionStatement], f: &mut dyn FnMut(&mut Expression)) {
    for stmt in body {
        walk_slots(&mut stmt.expression, f);
    }
}

/// Wraps `e` in one more pair of parentheses, spanning exactly `e`.
fn wrap(e: &mut Expression) {
    let span = e.span();
    let inner = std::mem::replace(
        e,
        Expression::Error {
            message: "".into(),
            span,
        },
    );
    *e = Expression::Parenthesized {
        expression: Box::new(inner),
        span,
    };
}

/// A method of the module: (class index, class side?, method index).
type MethodId = (usize, bool, usize);

fn methods_of(module: &Module) -> Vec<MethodId> {
    let mut out = Vec::new();
    for (c, class) in module.classes.iter().enumerate() {
        out.extend((0..class.methods.len()).map(|m| (c, false, m)));
        out.extend((0..class.class_methods.len()).map(|m| (c, true, m)));
    }
    out
}

fn body_mut(module: &mut Module, (c, side, m): MethodId) -> &mut Vec<ExpressionStatement> {
    let class = &mut module.classes[c];
    if side {
        &mut class.class_methods[m].body
    } else {
        &mut class.methods[m].body
    }
}

/// The slots of one method, as (label, source text) in walk order.
fn slots_of(module: &Module, id: MethodId, source: &str) -> Vec<(String, String)> {
    let mut copy = module.clone();
    let mut out = Vec::new();
    walk_body(body_mut(&mut copy, id), &mut |e| {
        let span = e.span();
        let text = source
            .get(span.start() as usize..span.end() as usize)
            .unwrap_or("?")
            .to_string();
        out.push((shape_label(e), text));
    });
    out
}

fn shape_label(e: &Expression) -> String {
    match e {
        Expression::Identifier(id) if id.name == "self" => "self".into(),
        Expression::Identifier(_) => "identifier".into(),
        Expression::ClassReference { .. } => "class".into(),
        Expression::Literal(..) => "literal".into(),
        Expression::Block(_) => "block".into(),
        Expression::MessageSend { selector, .. } => match selector {
            MessageSelector::Unary(_) => "unary send".into(),
            MessageSelector::Binary(_) => "binary send".into(),
            MessageSelector::Keyword(_) => "keyword send".into(),
        },
        Expression::Cascade { .. } => "cascade".into(),
        Expression::Parenthesized { .. } => "parenthesized".into(),
        Expression::FieldAccess { .. } => "field".into(),
        other => format!("{:?}", std::mem::discriminant(other)),
    }
}

/// Wraps the slots of method `id` selected by `pick` (by walk index).
fn wrapped(module: &Module, id: MethodId, pick: &dyn Fn(usize) -> bool) -> Module {
    let mut copy = module.clone();
    let mut i = 0;
    walk_body(body_mut(&mut copy, id), &mut |e| {
        if pick(i) {
            wrap(e);
        }
        i += 1;
    });
    copy
}

/// Blanks what a module derives from its methods' reflected source text, and
/// nothing else. Reflected source is the text as written, so a wrapped
/// variant's legitimately shows the added parentheses; a `( … )` can change
/// none of it but by changing that text, so it carries no codegen decision
/// this property tests. Two things are derived from it:
///
/// - every Core Erlang binary literal (`#{…}#`): the embedded method source;
/// - the `'line'` numbers in `'methodXref'`, which are positions in that
///   source (an added `( … )` can move a line break when it is re-printed).
///   The rest of each xref row — selectors, receiver kinds, types — is
///   compared: the xref walk peels operand parentheses like the compiler.
fn without_reflected_text(code: &str) -> String {
    without_xref_lines(&without_binaries(code))
}

fn without_binaries(code: &str) -> String {
    let mut out = String::with_capacity(code.len());
    let mut rest = code;
    while let Some(start) = rest.find("#{") {
        out.push_str(&rest[..start]);
        let Some(len) = rest[start..].find("}#") else {
            rest = &rest[start..];
            break;
        };
        out.push_str("#{}#");
        rest = &rest[start + len + 2..];
    }
    out.push_str(rest);
    out
}

/// Blanks each `'line' => N` inside a `'methodXref' => [ … ]` list.
fn without_xref_lines(code: &str) -> String {
    const XREF: &str = "'methodXref' => [";
    const LINE: &str = "'line' => ";
    let mut out = String::with_capacity(code.len());
    let mut rest = code;
    while let Some(start) = rest.find(XREF) {
        let body = start + XREF.len();
        let mut depth = 1;
        let end = rest[body..]
            .char_indices()
            .find_map(|(i, c)| {
                match c {
                    '[' => depth += 1,
                    ']' => depth -= 1,
                    _ => {}
                }
                (depth == 0).then_some(body + i)
            })
            .unwrap_or(rest.len());
        out.push_str(&rest[..body]);
        let mut pieces = rest[body..end].split(LINE);
        out.push_str(pieces.next().unwrap_or_default());
        for piece in pieces {
            out.push_str(LINE);
            out.push('_');
            out.push_str(piece.trim_start_matches(|c: char| c.is_ascii_digit()));
        }
        rest = &rest[end..];
    }
    out.push_str(rest);
    out
}

/// Codegen's whole observable outcome: the Core Erlang (with what derives
/// from the reflected source text blanked, [`without_reflected_text`]), the
/// error, or the panic.
fn outcome(module: &Module) -> String {
    outcome_of(|| generate_module_with_warnings(module, CodegenOptions::new("paren_probe")))
}

/// [`outcome`] through the driver path every real compile takes: `analyse_full`
/// on the module, `lower_analysed_module_for_codegen` on the driver's own copy,
/// then codegen with the analysis handed off. Analysis keys its facts and
/// types by span, so this is where a wrapper the analysis saw but codegen did
/// not (or the reverse) would show.
fn outcome_analysed(module: &Module) -> String {
    outcome_of(|| {
        let analysis = analyse_full(module, AnalysisContext::default());
        let mut lowered = module.clone();
        lower_analysed_module_for_codegen(&mut lowered, &analysis);
        generate_module_with_warnings(
            &lowered,
            CodegenOptions::new("paren_probe").with_analysis(analysis),
        )
    })
}

fn outcome_of(
    compile: impl FnOnce() -> crate::core_erlang::Result<crate::core_erlang::GeneratedModule>,
) -> String {
    let compiled = catch_unwind(AssertUnwindSafe(compile));
    match compiled {
        Ok(Ok(out)) => {
            let mut s = without_reflected_text(&out.code);
            for w in &out.warnings {
                s.push_str("\n%% warning: ");
                s.push_str(&w.message);
            }
            s
        }
        Ok(Err(e)) => format!("ERR {e}"),
        Err(_) => "PANIC".into(),
    }
}

/// What a differing row's wrapped variant produced (BT-4091 asks for compile
/// errors and verifier panics to be counted apart from mere output changes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RowKind {
    /// Different Core Erlang.
    Diff,
    /// A codegen error where the unwrapped module compiled (or a different one).
    Err,
    /// A panic (e.g. a `ThreadedIr` verifier panic) the unwrapped module did not hit.
    Panic,
}

impl RowKind {
    fn of(variant: &str) -> Self {
        if variant.starts_with("ERR ") {
            Self::Err
        } else if variant == "PANIC" {
            Self::Panic
        } else {
            Self::Diff
        }
    }
}

/// One differing row: a single wrapped slot whose output changed.
#[derive(Debug)]
struct Row {
    kind: RowKind,
    context: String,
    slot: String,
    text: String,
    method: String,
    first_diff: String,
}

#[derive(Default)]
struct Tally {
    sources: usize,
    rows: usize,
    differing: Vec<Row>,
}

fn first_diff(a: &str, b: &str) -> String {
    // The window around the first differing byte, on both sides.
    let at = a
        .bytes()
        .zip(b.bytes())
        .position(|(x, y)| x != y)
        .unwrap_or_else(|| a.len().min(b.len()));
    let window = |s: &str| {
        let mut lo = at.saturating_sub(60);
        while !s.is_char_boundary(lo) {
            lo -= 1;
        }
        let mut hi = (at + 100).min(s.len());
        while !s.is_char_boundary(hi) {
            hi += 1;
        }
        s[lo..hi].replace('\n', " ")
    };
    format!("- {}\n+ {}", window(a), window(b))
}

fn check_source(src: &str, tally: &mut Tally, outcome: fn(&Module) -> String) {
    let (module, diagnostics) = parse(lex_with_eof(src));
    if let Some(d) = diagnostics.iter().find(|d| d.severity == Severity::Error) {
        if std::env::var_os("BT4059_PARSE_DUMP").is_some() {
            println!("PARSE {}: {:?}", d.message, d.span);
        }
        return;
    }
    tally.sources += 1;
    let started = std::time::Instant::now();
    let base = outcome(&module);
    let methods = methods_of(&module);
    let slot_lists: Vec<_> = methods
        .iter()
        .map(|&id| slots_of(&module, id, src))
        .collect();
    tally.rows += slot_lists.iter().map(Vec::len).sum::<usize>();
    // Every slot of every method wrapped at once: on a transparent compiler
    // this one compile settles the whole source.
    let mut all = module.clone();
    for &id in &methods {
        all = wrapped(&all, id, &|_| true);
    }
    if outcome(&all) == base {
        report_timing(src, started);
        return;
    }
    for (&id, slots) in methods.iter().zip(&slot_lists) {
        if slots.is_empty() {
            continue;
        }
        if outcome(&wrapped(&module, id, &|_| true)) == base {
            continue;
        }
        let class = &module.classes[id.0];
        let kind = match class.class_kind {
            ClassKind::Object => "Object",
            ClassKind::Actor => "Actor",
            ClassKind::Value => "Value",
        };
        let (side, method) = if id.1 {
            ("class", &class.class_methods[id.2])
        } else {
            ("instance", &class.methods[id.2])
        };
        for (k, (label, text)) in slots.iter().enumerate() {
            let variant = outcome(&wrapped(&module, id, &|i| i == k));
            if variant != base {
                tally.differing.push(Row {
                    kind: RowKind::of(&variant),
                    context: format!("{kind} {side} method"),
                    slot: label.clone(),
                    text: text.clone(),
                    method: format!("{}>>{}", class.name.name, method.selector.name()),
                    first_diff: first_diff(&base, &variant),
                });
            }
        }
    }
    report_timing(src, started);
}

/// `BT4059_TIMING=1` prints how long each source took, to keep the slow
/// tier's sampling honest.
fn report_timing(src: &str, started: std::time::Instant) {
    if std::env::var_os("BT4059_TIMING").is_some() {
        let class = src
            .lines()
            .find(|l| l.contains("subclass:"))
            .unwrap_or_default();
        println!("TIMING {:>10.3}s {class}", started.elapsed().as_secs_f64());
    }
}

fn report(name: &str, tally: &Tally) -> String {
    let mut by_kind: BTreeMap<(String, String), usize> = BTreeMap::new();
    for r in &tally.differing {
        *by_kind
            .entry((r.context.clone(), r.slot.clone()))
            .or_default() += 1;
    }
    let count = |kind| tally.differing.iter().filter(|r| r.kind == kind).count();
    let mut out = format!(
        "{name}: {} sources, {} rows, {} differ ({} ERR, {} PANIC)\n",
        tally.sources,
        tally.rows,
        tally.differing.len(),
        count(RowKind::Err),
        count(RowKind::Panic),
    );
    for ((ctx, slot), n) in &by_kind {
        let _ = writeln!(out, "  {n:5}  {ctx}, wrapped {slot}");
    }
    for r in tally.differing.iter().take(40) {
        let _ = writeln!(
            out,
            "  {:?} {} [{}] ({}) `{}`\n    {}",
            r.kind,
            r.method,
            r.context,
            r.slot,
            r.text,
            r.first_diff.replace('\n', "\n    ")
        );
    }
    out
}

/// Hosts for [`TEMPLATE_BODY`]: `@C` is the class name, `@B` the body, which
/// runs once as an instance method and once as a class method.
const TEMPLATE_HOSTS: &[&str] = &[
    "Object subclass: @C\n  class foo => 1\n  class bar => 2\n  class twice => 2\n  class + o => o\n  class add: a to: b => a + b\n  foo => 1\n  bar => 2\n  + o => o\n  add: a to: b => a + b\n  run: x =>\n@B\n  class run: x =>\n@B\n",
    "Actor subclass: @C\n  state: n = 0\n  class foo => 1\n  class bar => 2\n  class twice => 2\n  class + o => o\n  class add: a to: b => a + b\n  foo => self.n := self.n + 1\n  bar => 2\n  + o => self.n := self.n + o\n  add: a to: b => a + b\n  run: x =>\n@B\n  class run: x =>\n@B\n",
    "Value subclass: @C\n  field: n = 0\n  class foo => 1\n  class bar => 2\n  class twice => 2\n  class + o => o\n  class add: a to: b => a + b\n  foo => 1\n  bar => 2\n  + o => o\n  add: a to: b => a + b\n  run: x =>\n@B\n  class run: x =>\n@B\n",
];

/// Receivers `self`, the own class `@C`, another class, a local, literals, a
/// cascade and a block, for unary/binary/keyword selectors, at top level and
/// inside blocks, loops and conditionals.
const TEMPLATE_BODY: &str = "    y := 0
    y := self foo
    y := self + 1
    y := self add: 1 to: 2
    y := @C twice
    y := @C + 1
    y := @C add: 3 to: 4
    y := @C add: (@C + 5) to: @C twice
    y := OrderedCollection new
    y := Point x: 1 y: 2
    y := x size
    y := x + 1
    y := x printString: 10
    y := 3 + 4
    y := 3 max: 4
    y := #(1, 2) size
    y := \"a\" ++ \"b\"
    y := (OrderedCollection new) add: 1; add: 2; yourself
    y := self foo; bar
    y := @C foo; bar
    1 to: 3 do: [:i | y := self + i]
    #(1, 2) do: [:e | y := @C + e]
    #(1, 2) do: [:e | y := self add: e to: 1]
    [y < 3] whileTrue: [y := y + 1]
    3 timesRepeat: [y := self foo]
    y := x > 0 ifTrue: [self foo] ifFalse: [@C twice]
    y := #(1, 2) collect: [:e | self add: e to: @C twice]
    y := #(1, 2) inject: 0 into: [:a :e | a + e]
    y := [:a | a + 1] value: 3
    y := [self foo] on: Error do: [:e | e messageText]
    y := [@C + 1] ensure: [y := 0]
    y isNil ifTrue: [^ self bar]
    y";

fn template_sources() -> Vec<String> {
    TEMPLATE_HOSTS
        .iter()
        .enumerate()
        .map(|(i, host)| {
            let class = format!("ParenProbe{i}");
            host.replace("@B", &TEMPLATE_BODY.replace("@C", &class))
                .replace("@C", &class)
        })
        .collect()
}

/// Fast tier: the template matrix, every host x side x template statement.
#[test]
fn templates_are_paren_transparent() {
    let mut tally = Tally::default();
    for src in template_sources() {
        check_source(&src, &mut tally, outcome);
    }
    assert_eq!(tally.sources, TEMPLATE_HOSTS.len(), "every template parses");
    let report = report("templates", &tally);
    println!("{report}");
    assert!(tally.differing.is_empty(), "{report}");
}

/// Fast tier, through analysis: the same matrix compiled the way a driver
/// compiles it ([`outcome_analysed`]).
#[test]
fn templates_are_paren_transparent_through_analysis() {
    let mut tally = Tally::default();
    for src in template_sources() {
        check_source(&src, &mut tally, outcome_analysed);
    }
    assert_eq!(tally.sources, TEMPLATE_HOSTS.len(), "every template parses");
    let report = report("templates (analysed)", &tally);
    println!("{report}");
    assert!(tally.differing.is_empty(), "{report}");
}

/// Every `.bt` file directly under `dir`.
fn bt_files(dir: &std::path::Path) -> Vec<String> {
    let mut paths: Vec<_> = std::fs::read_dir(dir)
        .map(|rd| rd.filter_map(|e| e.ok().map(|e| e.path())).collect())
        .unwrap_or_default();
    paths.retain(|p| p.extension().is_some_and(|x| x == "bt"));
    paths.sort();
    paths
        .into_iter()
        .filter_map(|p| std::fs::read_to_string(p).ok())
        .collect()
}

/// Checks one corpus and fails with its report if any row differs.
fn check_corpus(name: &str, sources: &[String], outcome: fn(&Module) -> String) {
    let mut tally = Tally::default();
    for src in sources {
        check_source(src, &mut tally, outcome);
    }
    let report = report(name, &tally);
    println!("{report}");
    assert!(tally.sources > 0, "{name}: no source parsed");
    assert!(tally.differing.is_empty(), "{report}");
}

fn stdlib_test_dir() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test")
}

// Slow tier (`just test-corpus`: the `_corpus` suffix puts these in the
// corpus filterset). One test per corpus; each checks every source (about
// 10-20 s apiece in the dev profile), so none needs sampling.

#[test]
fn paren_constructs_corpus() {
    check_corpus("constructs", &construct_sources(), outcome);
}

#[test]
fn paren_statements_corpus() {
    check_corpus("statements", &statement_sources(), outcome);
}

#[test]
fn paren_generated_corpus() {
    let programs = std::env::var("BT4059_PROGRAMS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1024);
    check_corpus("generated", &generated_sources_of(programs), outcome);
}

#[test]
fn paren_stdlib_test_corpus() {
    check_corpus("stdlib/test", &bt_files(&stdlib_test_dir()), outcome);
}

#[test]
fn paren_stdlib_fixtures_corpus() {
    check_corpus(
        "stdlib/test/fixtures",
        &bt_files(&stdlib_test_dir().join("fixtures")),
        outcome,
    );
}

#[test]
fn paren_stdlib_test_analysed_corpus() {
    check_corpus(
        "stdlib/test (analysed)",
        &bt_files(&stdlib_test_dir()),
        outcome_analysed,
    );
}

#[test]
fn paren_stdlib_fixtures_analysed_corpus() {
    check_corpus(
        "stdlib/test/fixtures (analysed)",
        &bt_files(&stdlib_test_dir().join("fixtures")),
        outcome_analysed,
    );
}

#[test]
fn paren_constructs_analysed_corpus() {
    check_corpus(
        "constructs (analysed)",
        &construct_sources(),
        outcome_analysed,
    );
}

/// Operands that only parse parenthesised — an assignment, a field write, a
/// keyword send, a cascade, a conditional, a match, a binary send as a unary
/// receiver — reach codegen *bare* once the wrapper is peeled, a shape the
/// parser never produced before BT-4059. `@O` is the operand.
const NEEDED_PAREN_OPERANDS: &[&str] = &[
    "(y := x + 1)",
    "(self.n := x)",
    "(o foo: x)",
    "(o foo; bar: x)",
    "(x > 0 ifTrue: [1] ifFalse: [2])",
    "(x match: [0 -> 1; _ -> 2])",
    "(x + 1)",
    "(#(1, 2) inject: 0 into: [:a :e | y := y + e. a + e])",
];

/// Operand slots `@S` holds `@O` in: unary receiver, both binary operands,
/// a keyword argument, a cascade argument and a cascade target.
const NEEDED_PAREN_SLOTS: &[&str] = &[
    "@O printString",
    "@O + 1",
    "1 + @O",
    "o take: @O",
    "o foo; take: @O",
    "@O printString; displayString",
];

/// Frames the slot's statement `@S` sits in.
const NEEDED_PAREN_FRAMES: &[&str] = &[
    "r := @S",
    "#(1, 2) do: [:e | r := @S]",
    "[y < 3] whileTrue: [y := y + 1. r := @S]",
    "x > 0 ifTrue: [r := @S] ifFalse: [r := 0]",
    "r := [@S] on: Error do: [:e | 0]",
    "r := #(1, 2) inject: 0 into: [:a :e | @S. a]",
    "r := #(1, 2) collect: [:e | @S]",
];

/// Compiles `src` the way a driver does and, when analysis accepts it,
/// through the `ThreadedIr` verifier and `erlc`: `None` when it is rejected
/// by analysis, `Some(Ok)` when it compiles, `Some(Err)` with the failure.
fn compile_accepted(src: &str) -> Option<Result<(), String>> {
    let (module, diagnostics) = parse(lex_with_eof(src));
    if diagnostics.iter().any(|d| d.severity == Severity::Error) {
        return Some(Err(format!("PARSE {src}")));
    }
    let analysis = analyse_full(&module, AnalysisContext::default());
    if analysis
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return None;
    }
    let mut lowered = module.clone();
    lower_analysed_module_for_codegen(&mut lowered, &analysis);
    let result = catch_unwind(AssertUnwindSafe(|| {
        let code = generate_module_with_warnings(
            &lowered,
            CodegenOptions::new("needed_paren").with_analysis(analysis),
        )
        .map_err(|e| e.to_string())?
        .code;
        super::assert_compiles_through_erlc("needed_paren", &code);
        Ok::<(), String>(())
    }));
    Some(match result {
        Ok(Ok(())) => Ok(()),
        Ok(Err(e)) => Err(format!("ERR {e}\n{src}")),
        Err(_) => Err(format!("PANIC\n{src}")),
    })
}

/// Every needed-parentheses operand, in every slot and frame, in an actor,
/// an object and a class method: the peeled tree must compile through the
/// `ThreadedIr` verifier and `erlc` whenever analysis accepts it. The frames
/// of one operand and slot share a module (one `erlc` run); a failing module
/// is drilled into method by method.
#[test]
fn paren_needed_operand_shapes_corpus() {
    let hosts = [
        "Actor subclass: NeededParen\n  state: n = 0\n  foo => 1\n",
        "Object subclass: NeededParen\n  state: n = 0\n  foo => 1\n",
        "Object subclass: NeededParen\n  classState: n = 0\n  class foo => 1\n",
    ];
    let method = |host: &str, k: usize, stmt: &str| {
        let side = if host.contains("classState:") {
            "class "
        } else {
            ""
        };
        format!("  {side}run{k}: x with: o =>\n    y := 0\n    r := 0\n    {stmt}\n    #[r, y]\n")
    };
    let mut compiled = 0;
    let mut failures = Vec::new();
    for host in hosts {
        for operand in NEEDED_PAREN_OPERANDS {
            for slot in NEEDED_PAREN_SLOTS {
                let statement = slot.replace("@O", operand);
                // The frames analysis accepts, alone.
                let accepted: Vec<String> = NEEDED_PAREN_FRAMES
                    .iter()
                    .map(|frame| frame.replace("@S", &statement))
                    .filter(|stmt| {
                        let src = format!("{host}{}", method(host, 0, stmt));
                        let (module, _) = parse(lex_with_eof(&src));
                        !analyse_full(&module, AnalysisContext::default())
                            .diagnostics
                            .iter()
                            .any(|d| d.severity == Severity::Error)
                    })
                    .collect();
                let batch: String = accepted
                    .iter()
                    .enumerate()
                    .map(|(k, stmt)| method(host, k, stmt))
                    .collect();
                if accepted.is_empty()
                    || compile_accepted(&format!("{host}{batch}")) == Some(Ok(()))
                {
                    compiled += accepted.len();
                    continue;
                }
                for stmt in &accepted {
                    match compile_accepted(&format!("{host}{}", method(host, 0, stmt))) {
                        Some(Ok(())) => compiled += 1,
                        Some(Err(e)) => failures.push(e),
                        None => {}
                    }
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} of the needed-parentheses operand programs fail:\n{}",
        failures.len(),
        failures
            .iter()
            .take(5)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(compiled > 500, "only {compiled} programs compiled");
}

/// BT-4059: compiled code is paren-transparent, but a method's reflected
/// source (`methodSource`, which the IDE method editor is seeded from and the
/// workspace compares with the file) is its text as written — through
/// self-sufficient codegen and through the driver path, whose lowering leaves
/// the parentheses for codegen's own recorded peel.
#[test]
fn reflected_method_source_keeps_operand_parens_as_written() {
    let written =
        "Object subclass: Summer\n  summed: a => (a * 2) + 1\n  m => (self) foo\n  foo => 1\n";
    // The same program with each parenthesis blanked, so every node keeps
    // its byte offsets.
    let bare = written.replace(['(', ')'], " ");
    let (module, diagnostics) = parse(lex_with_eof(written));
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let (bare_module, _) = parse(lex_with_eof(&bare));

    let self_sufficient = generate_module_with_warnings(&module, CodegenOptions::new("summer"))
        .expect("compiles")
        .code;
    let analysis = analyse_full(&module, AnalysisContext::default());
    let mut lowered = module.clone();
    lower_analysed_module_for_codegen(&mut lowered, &analysis);
    assert!(
        beamtalk_core::ast::normalize::has_parenthesised_operand(&lowered),
        "lowering must leave the written parentheses for codegen"
    );
    let analysed = generate_module_with_warnings(
        &lowered,
        CodegenOptions::new("summer").with_analysis(analysis),
    )
    .expect("compiles")
    .code;

    for (path, code) in [
        ("self-sufficient", &self_sufficient),
        ("analysed", &analysed),
    ] {
        for (selector, source) in [
            ("summed:", "summed: a => (a * 2) + 1"),
            ("m", "m => (self) foo"),
        ] {
            assert!(
                code.contains(&source_entry(selector, source)),
                "{path}: `{selector}` should reflect `{source}` as written. Got:\n{code}"
            );
        }
    }
    // Everything but the reflected source compiles as the bare program does.
    assert_eq!(
        without_reflected_text(&self_sufficient),
        without_reflected_text(
            &generate_module_with_warnings(&bare_module, CodegenOptions::new("summer"))
                .expect("compiles")
                .code
        )
    );
}

/// `'selector' => #{source}#`: how `source` appears as a reflected-source map
/// entry in generated Core Erlang.
fn source_entry(selector: &str, source: &str) -> String {
    format!(
        "'{selector}' => #{{{}}}#",
        beamtalk_cerl_doc::binary::binary_byte_segments(source)
    )
}

/// BT-4059: the peel record is scoped by method. A flattened module holds a
/// cross-file protocol provision whose byte offsets index its own file, so its
/// parenthesised operand `(x)` can sit at the very span of the class method's
/// bare `x`; neither may take the other's parentheses.
#[test]
fn a_cross_file_provision_and_a_class_method_with_colliding_operand_spans_reflect_as_written() {
    let protocol_src = format!(
        "// {}\nProtocol define: P\n  pro: x => (x) + 1\n",
        "=".repeat(60)
    );
    let target = protocol_src.find("(x)").unwrap() + 1;
    let body = "Object subclass: C\n  uses: P\n  foo: x => x + 1\n";
    let base = body.find("=> x").unwrap() + 3;
    let class_src = format!("//{}\n{body}", "-".repeat(target - base - 3));
    assert_eq!(
        class_src.find("=> x").unwrap() + 3,
        target,
        "spans must collide"
    );

    let protocol = {
        let (module, _) = parse(lex_with_eof(&protocol_src));
        module.protocols.into_iter().next().expect("a protocol")
    };
    let (mut module, _) = parse(lex_with_eof(&class_src));
    let analysis = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_protocol_defs(vec![protocol]),
    );
    assert!(
        !analysis
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "{:?}",
        analysis.diagnostics
    );
    lower_analysed_module_for_codegen(&mut module, &analysis);
    assert!(
        module.classes[0]
            .methods
            .iter()
            .any(|m| m.selector.name() == "pro:"),
        "the provision is flattened into C"
    );
    let code =
        generate_module_with_warnings(&module, CodegenOptions::new("c").with_analysis(analysis))
            .expect("compiles")
            .code;
    for (selector, source) in [("foo:", "foo: x => x + 1"), ("pro:", "pro: x => (x) + 1")] {
        assert!(
            code.contains(&source_entry(selector, source)),
            "`{selector}` should reflect `{source}` as written. Got:\n{code}"
        );
    }
}

/// A `ClassBuilder` block method inside a class method reflects its operand
/// parentheses as written: the block's operands are recorded under the
/// enclosing method's scope, which codegen restores them from.
#[test]
fn a_class_builder_block_method_inside_a_method_reflects_as_written() {
    let src = "Object subclass: Maker\n  class make =>\n    Object classBuilder name: #Made; methods: #{ #m => [:self | (self) foo] }; register\n";
    let (module, diagnostics) = parse(lex_with_eof(src));
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let code = generate_module_with_warnings(&module, CodegenOptions::new("maker"))
        .expect("compiles")
        .code;
    let entry = format!(
        "#{{{}}}#",
        beamtalk_cerl_doc::binary::binary_byte_segments("m => (self) foo")
    );
    assert!(code.contains(&entry), "Got:\n{code}");
}
