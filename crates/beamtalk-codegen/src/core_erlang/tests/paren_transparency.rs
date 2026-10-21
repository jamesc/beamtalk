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
//! byte-identical to the unwrapped module's.
//!
//! A `super` receiver is never wrapped: `(super) foo` is not a super send.
//!
//! Each method is first checked with all its slots wrapped at once; only a
//! method whose output differs is drilled into slot by slot, so the cost is
//! about two compiles per method on a transparent compiler. (A slot whose own
//! difference another wrapped slot cancels out would be missed by that
//! shortcut; every reported row is a real, single-slot difference.)
//!
//! Corpora:
//! - [`TEMPLATE_HOSTS`] x [`TEMPLATE_BODY`] (fast tier): every class kind
//!   (Object, Actor, Value), instance and class side, with receivers that are
//!   `self`, the own class, another class, a local, a literal, a cascade, a
//!   block, for every selector shape, at top level and inside blocks and
//!   loops;
//! - `synced_call_sites_corpus`'s construct matrix, statement corpus and
//!   generated programs, and every `stdlib/test` source with its fixtures
//!   (slow tier, [`generated_and_stdlib_paren_corpus`]).

use super::synced_call_sites_corpus::{construct_sources, generated_sources, statement_sources};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{ClassKind, Expression, ExpressionStatement, MessageSelector, Module};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use std::collections::BTreeMap;
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

/// Blanks every Core Erlang binary literal (`#{…}#`). The module embeds each
/// method's source text (re-printed from the AST, so it shows the added
/// parentheses) as a binary; a `( … )` can change no string literal's
/// content, so the binaries carry no codegen decision this property tests.
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

/// Codegen's whole observable outcome: the Core Erlang (binaries blanked,
/// [`without_binaries`]), the error, or the panic.
fn outcome(module: &Module) -> String {
    let compiled = catch_unwind(AssertUnwindSafe(|| {
        generate_module_with_warnings(module, CodegenOptions::new("paren_probe"))
    }));
    match compiled {
        Ok(Ok(out)) => {
            let mut s = without_binaries(&out.code);
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

/// One differing row: a single wrapped slot whose output changed.
#[derive(Debug)]
struct Row {
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

fn check_source(src: &str, tally: &mut Tally) {
    let (module, diagnostics) = parse(lex_with_eof(src));
    if let Some(d) = diagnostics.iter().find(|d| d.severity == Severity::Error) {
        if std::env::var_os("BT4059_PARSE_DUMP").is_some() {
            println!("PARSE {}: {:?}", d.message, d.span);
        }
        return;
    }
    tally.sources += 1;
    let base = outcome(&module);
    for id in methods_of(&module) {
        let slots = slots_of(&module, id, src);
        if slots.is_empty() {
            continue;
        }
        tally.rows += slots.len();
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
                    context: format!("{kind} {side} method"),
                    slot: label.clone(),
                    text: text.clone(),
                    method: format!("{}>>{}", class.name.name, method.selector.name()),
                    first_diff: first_diff(&base, &variant),
                });
            }
        }
    }
}

fn report(name: &str, tally: &Tally) -> String {
    let mut by_kind: BTreeMap<(String, String), usize> = BTreeMap::new();
    for r in &tally.differing {
        *by_kind
            .entry((r.context.clone(), r.slot.clone()))
            .or_default() += 1;
    }
    let mut out = format!(
        "{name}: {} sources, {} rows, {} differ\n",
        tally.sources,
        tally.rows,
        tally.differing.len()
    );
    for ((ctx, slot), n) in &by_kind {
        out.push_str(&format!("  {n:5}  {ctx}, wrapped {slot}\n"));
    }
    for r in tally.differing.iter().take(40) {
        out.push_str(&format!(
            "  {} [{}] ({}) `{}`\n    {}\n",
            r.method,
            r.context,
            r.slot,
            r.text,
            r.first_diff.replace('\n', "\n    ")
        ));
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

#[test]
fn templates_are_paren_transparent() {
    let mut tally = Tally::default();
    for src in template_sources() {
        check_source(&src, &mut tally);
    }
    assert_eq!(tally.sources, TEMPLATE_HOSTS.len(), "every template parses");
    let report = report("templates", &tally);
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

#[test]
fn generated_and_stdlib_paren_corpus() {
    let programs = std::env::var("BT4059_PROGRAMS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(256);
    let stdlib = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/test");
    let corpora: Vec<(&str, Vec<String>)> = vec![
        ("constructs", construct_sources()),
        ("statements", statement_sources()),
        ("generated", generated_sources(programs)),
        ("stdlib/test", bt_files(&stdlib)),
        ("stdlib/test/fixtures", bt_files(&stdlib.join("fixtures"))),
    ];
    let mut failures = String::new();
    for (name, sources) in corpora {
        let mut tally = Tally::default();
        for src in &sources {
            check_source(src, &mut tally);
        }
        let report = report(name, &tally);
        println!("{report}");
        assert!(tally.sources > 0, "{name}: no source parsed");
        if !tally.differing.is_empty() {
            failures.push_str(&report);
        }
    }
    assert!(failures.is_empty(), "{failures}");
}
