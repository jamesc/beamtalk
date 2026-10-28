// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4059: precedence-aware unparse of the peeled tree, and restoring the
//! peeled operand parentheses to recover a method's text as written.
//!
//! The send-operand normalisation pass (`ast::normalize`) removes redundant
//! `Parenthesized` wrappers, so the unparser can no longer rely on the tree
//! to carry the parentheses a program's meaning needs: it adds them where an
//! operand binds looser than its slot, and nowhere else. The property is
//! `peel(parse(unparse(peel(m)))) == peel(m)` up to spans (the re-parse
//! carries the parentheses unparse added back as wrappers, which the second
//! peel removes again), checked over the generated program corpus and over
//! every stdlib and example source.
//!
//! Unparse only *adds* parentheses, so on a raw parse (the formatter's tree)
//! its output is unchanged. Restoring the wrappers a recording peel removed
//! (`restore_operand_parens`) gives back the as-written method: its
//! `unparse_method` is that of the raw parse, which is what a method reflects
//! as.

use proptest::prelude::*;

use super::super::{format_source, unparse_method, unparse_module};
use crate::ast::normalize::{
    PeeledParens, peel_operand_parens, peel_operand_parens_recording, restore_operand_parens,
};
use crate::ast::{MethodDefinition, Module};
use crate::source_analysis::corpus_test_support::{read_corpus_file, repo_root};
use crate::source_analysis::{Severity, lex_with_eof, parse};
use crate::test_helpers::test_support::{arb_program, parse_bt, proptest_config_default};

/// `module`'s `Debug` form with every span erased, so two trees that differ
/// only in source positions compare equal.
fn without_spans(module: &Module) -> String {
    let debug = format!("{module:?}");
    let mut out = String::with_capacity(debug.len());
    let mut rest = debug.as_str();
    while let Some(at) = rest.find("Span { start: ") {
        out.push_str(&rest[..at]);
        out.push_str("Span");
        match rest[at..].find('}') {
            Some(close) => rest = &rest[at + close + 1..],
            None => {
                rest = "";
            }
        }
    }
    out.push_str(rest);
    out
}

fn peeled(mut module: Module) -> Module {
    peel_operand_parens(&mut module);
    module
}

/// Checks the round-trip property for `module`, returning a description of
/// the failure if it does not hold.
fn round_trip_failure(module: Module) -> Option<String> {
    let canonical = peeled(module);
    let text = unparse_module(&canonical);
    let (reparsed, diagnostics) = parse(lex_with_eof(&text));
    if let Some(error) = diagnostics.iter().find(|d| d.severity == Severity::Error) {
        return Some(format!(
            "unparse output does not parse ({error:?}):\n{text}"
        ));
    }
    let (got, want) = (without_spans(&peeled(reparsed)), without_spans(&canonical));
    (got != want).then(|| {
        let at = got
            .bytes()
            .zip(want.bytes())
            .position(|(g, w)| g != w)
            .unwrap_or_else(|| got.len().min(want.len()));
        let window = |s: &str| s.get(at.saturating_sub(200)..(at + 200).min(s.len())).unwrap_or("").to_string();
        format!(
            "re-parse differs from the peeled tree:\n{text}\n--- re-parsed: …{}…\n--- peeled:    …{}…",
            window(&got),
            window(&want)
        )
    })
}

proptest! {
    #![proptest_config(proptest_config_default())]

    /// Generated programs (`arb_program`): their trees carry the wrappers
    /// keyword-send receivers need; peeled, unparse must put them back.
    #[test]
    fn peeled_generated_programs_round_trip(module in arb_program("ParenRoundTrip")) {
        let failure = round_trip_failure(module);
        prop_assert!(failure.is_none(), "{}", failure.unwrap_or_default());
    }
}

/// Every method of `module` (class and class side, standalone extensions,
/// protocol provisions), in a fixed order.
fn methods_of(module: &Module) -> Vec<&MethodDefinition> {
    let mut methods = Vec::new();
    for class in &module.classes {
        methods.extend(class.methods.iter().chain(&class.class_methods));
    }
    methods.extend(module.method_definitions.iter().map(|s| &s.method));
    for protocol in &module.protocols {
        methods.extend(&protocol.provided_methods);
    }
    methods
}

/// `method` with the wrappers `peeled` recorded put back.
fn restored(method: &MethodDefinition, peeled: &PeeledParens) -> MethodDefinition {
    let mut restored = method.clone();
    restore_operand_parens(&mut restored, peeled);
    restored
}

/// Checks that restoring the recorded wrappers on each peeled method gives
/// back its as-written source (`unparse_method` of the raw parse), and that
/// a second restore changes nothing, returning a description of each failure.
fn restore_failures(raw: &Module) -> Vec<String> {
    let mut peeled = raw.clone();
    let record = peel_operand_parens_recording(&mut peeled);
    let (raw_methods, peeled_methods) = (methods_of(raw), methods_of(&peeled));
    assert_eq!(raw_methods.len(), peeled_methods.len());
    let mut failures = Vec::new();
    for (raw_method, peeled_method) in raw_methods.into_iter().zip(peeled_methods) {
        let written = unparse_method(raw_method);
        let once = restored(peeled_method, &record);
        let got = unparse_method(&once);
        if got != written {
            failures.push(format!(
                "restored source differs:\n--- restored:\n{got}\n--- as written:\n{written}"
            ));
        }
        if restored(&once, &record) != once {
            failures.push(format!("a second restore changes:\n{written}"));
        }
    }
    failures
}

/// Every `.bt` source of the stdlib (sources and tests, with fixtures) and the
/// examples: the hand-written programs, parenthesised every way people write.
/// The peeled tree round-trips through unparse, and restoring what the peel
/// recorded gives back every method as written.
#[test]
fn peeled_stdlib_and_examples_round_trip() {
    let root = repo_root();
    let walker = crate::file_walker::FileWalker::new()
        .extensions(&["bt"])
        .skip_symlinks(false)
        .strict_errors(false);
    let mut checked = 0;
    let mut failures = Vec::new();
    for dir in ["stdlib/src", "stdlib/test", "examples"] {
        let dir = root.join(dir);
        if !dir.is_dir() {
            continue;
        }
        for path in walker.walk_pathbuf(&dir).unwrap_or_default() {
            let source = read_corpus_file(&path);
            let (module, diagnostics) = parse(lex_with_eof(&source));
            if diagnostics.iter().any(|d| d.severity == Severity::Error) {
                continue;
            }
            checked += 1;
            for failure in restore_failures(&module) {
                failures.push(format!("{}: {failure}", path.display()));
            }
            if let Some(failure) = round_trip_failure(module) {
                failures.push(format!("{}: {failure}", path.display()));
            }
        }
    }
    if root.join("stdlib/src").is_dir() {
        assert!(checked > 100, "only {checked} sources checked");
    }
    assert!(
        failures.is_empty(),
        "{} of {checked} sources fail:\n{}",
        failures.len(),
        failures
            .iter()
            .take(5)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// `unparse_method` of the first method of `source`'s first class, peeled:
/// the text the compiled tree unparses as, with only the parentheses its
/// meaning needs.
fn peeled_source(source: &str) -> String {
    let mut module = parse_bt(source);
    peel_operand_parens(&mut module);
    unparse_method(&module.classes[0].methods[0])
}

#[test]
fn unparse_of_the_peeled_tree_keeps_only_needed_parens() {
    let cases = [
        ("(self) foo", "self foo"),
        ("((self)) foo: (1) bar: ((2))", "self foo: 1 bar: 2"),
        ("(3 + 4) * 2", "(3 + 4) * 2"),
        ("(3 * 4) + 2", "3 * 4 + 2"),
        ("3 + (4 * 2)", "3 + 4 * 2"),
        ("(a - b) - c", "a - b - c"),
        ("a - (b - c)", "a - (b - c)"),
        ("(a ** b) ** c", "(a ** b) ** c"),
        ("a ** (b ** c)", "a ** b ** c"),
        ("(a < b) == (b < c)", "a < b == b < c"),
        ("(a == b) < c", "(a == b) < c"),
        ("a * (b + c)", "a * (b + c)"),
        ("x at: (y at: 1)", "x at: (y at: 1)"),
        ("(x at: 1) foo", "(x at: 1) foo"),
        ("(a + b) foo", "(a + b) foo"),
        ("(a foo) + (b bar)", "a foo + b bar"),
        ("x foo: (y + 1)", "x foo: y + 1"),
        ("x + (y := 3)", "x + (y := 3)"),
        ("(a foo; bar) baz", "(a foo; bar) baz"),
        ("(a foo); bar", "(a foo); bar"),
        ("(a) foo; bar: (1 + 2)", "a foo; bar: 1 + 2"),
        ("(super) foo", "(super) foo"),
        ("^ (self) foo", "^self foo"),
        ("y := (x)", "y := (x)"),
    ];
    for (body, expected) in cases {
        let source = format!("Object subclass: A\n  m: a b: b c: c x: x y: y =>\n    {body}\n");
        let unparsed = peeled_source(&source);
        let unparsed_body = unparsed
            .split_once("=>")
            .map_or("", |(_, body)| body)
            .trim();
        assert_eq!(unparsed_body, expected, "{body}\n{unparsed}");
        // Restoring what the peel recorded gives back the body as written.
        let failures = restore_failures(&parse_bt(&source));
        assert!(failures.is_empty(), "{body}: {failures:?}");
    }
}

#[test]
fn formatter_keeps_redundant_operand_parens() {
    for source in [
        "Object subclass: A\n\n  m => (self) foo\n",
        "Object subclass: A\n\n  m => (3 + 4) * 2\n",
    ] {
        assert_eq!(format_source(source).as_deref(), Some(source));
    }
}

#[test]
fn awkward_operands_round_trip() {
    for body in [
        "x - (-1)",
        "(-1) abs",
        "(-1.5) abs + (-2)",
        "(#(1, 2)) size",
        "(x foo: 1) bar; baz: (y qux: 2)",
        "x foo: ([:a | a]) bar: ((x))",
        "(3) factorial",
        "x + (y foo: 1)",
        "(a -> b) key",
        "a -> (b -> c)",
        "(a -> b) -> c",
        "(Foo >> #bar) foo",
        "x ++ (y ++ z)",
        "(x ++ y) size",
        "(x ++ y) ++ (z foo: 1)",
        "(x ifTrue: [1] ifFalse: [2]) + (y match: [1 -> 2; _ -> 3])",
        "(#{#a => 1}) at: (#a)",
        "(self.x) + (self.y)",
        "x foo: (#[1, 2]) bar: (\"s\")",
    ] {
        let source = format!("Object subclass: A\n  m: x y: y =>\n    {body}\n");
        let failure = round_trip_failure(parse_bt(&source));
        assert!(failure.is_none(), "{body}: {}", failure.unwrap_or_default());
    }
}
