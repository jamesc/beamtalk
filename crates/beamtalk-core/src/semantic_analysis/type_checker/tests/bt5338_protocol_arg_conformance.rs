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
