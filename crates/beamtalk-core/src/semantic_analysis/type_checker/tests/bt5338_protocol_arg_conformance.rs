// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5338: protocol-typed call-site arguments. A `Dynamic` argument is
//! accepted (gradual typing), and a conformance diagnostic points at the
//! offending argument, not the whole send.

use super::common::*;

const SINK: &str = r"
Object subclass: Sink
  show: a :: Printable => a
  smaller: a :: Comparable => a

ProtoObject subclass: Bare
  ping => 1
";

#[test]
fn dynamic_argument_against_protocol_param_does_not_warn() {
    let source = format!(
        "{SINK}\nObject subclass: H\n  go: x => Sink new show: x\n  pick: y => Sink new smaller: y\n"
    );
    let msgs: Vec<_> = analyse_diags(&source)
        .iter()
        .map(|d| d.message.to_string())
        .filter(|m| m.contains("Printable") || m.contains("Comparable"))
        .collect();
    assert!(msgs.is_empty(), "{msgs:?}");
}

#[test]
fn conformance_diagnostic_points_at_the_argument() {
    let source = format!("{SINK}\nObject subclass: H\n  go => Sink new show: Bare new\n");
    let diags: Vec<_> = analyse_diags(&source)
        .into_iter()
        .filter(|d| d.message.contains("does not conform to protocol Printable"))
        .collect();
    assert_eq!(diags.len(), 1, "{diags:?}");
    let arg = "Bare new";
    let start = source.rfind(arg).expect("argument in source");
    assert_eq!(
        (diags[0].span.start() as usize, diags[0].span.end() as usize),
        (start, start + arg.len()),
        "span should cover exactly the offending argument"
    );
}

/// BT-5345: strictness for a `Dynamic` argument to a `:: Protocol` parameter
/// comes from the "Dynamic in typed class" lint, which fires on the argument
/// expression itself inside a `typed` class (and stays silent outside one).
#[test]
fn dynamic_argument_to_protocol_param_hits_dynamic_in_typed_class_lint() {
    let typed = format!("{SINK}\ntyped Object subclass: H\n  go: x => Sink new show: x\n");
    let lint: Vec<_> = analyse_diags(&typed)
        .into_iter()
        .filter(|d| d.message.contains("inferred as Dynamic in typed class"))
        .collect();
    assert!(!lint.is_empty(), "lint should cover the argument: {lint:?}");
    let start = typed.rfind("show: x").unwrap() + "show: ".len();
    assert!(
        lint.iter()
            .any(|d| (d.span.start() as usize, d.span.end() as usize) == (start, start + 1)),
        "lint should anchor on the argument `x`: {lint:?}"
    );

    let untyped = format!("{SINK}\nObject subclass: H\n  go: x => Sink new show: x\n");
    assert!(
        !analyse_diags(&untyped)
            .iter()
            .any(|d| d.message.contains("inferred as Dynamic in typed class")),
        "no lint outside a typed class"
    );
}

const GENERIC_SINK: &str = r"
Object subclass: ListSink
  take: xs :: List(Integer) => xs
";

/// BT-5345: the type-argument mismatch warning points at the argument.
#[test]
fn type_argument_mismatch_points_at_the_argument() {
    let source = format!(
        "{GENERIC_SINK}\nObject subclass: H\n  go => ListSink new take: #('a', 'b')\n"
    );
    let diags: Vec<_> = analyse_diags(&source)
        .into_iter()
        .filter(|d| d.message.contains("Argument 1 of 'take:'"))
        .collect();
    assert_eq!(diags.len(), 1, "{diags:?}");
    let arg = "#('a', 'b')";
    let start = source.rfind(arg).expect("argument in source");
    assert_eq!(
        (diags[0].span.start() as usize, diags[0].span.end() as usize),
        (start, start + arg.len()),
        "span should cover exactly the offending argument"
    );
}
