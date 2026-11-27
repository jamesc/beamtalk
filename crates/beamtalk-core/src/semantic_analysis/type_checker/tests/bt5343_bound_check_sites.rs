// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5343: the bounded-type-parameter check (BT-5333) also covers block
//! parameters and local annotations inside state-field default expressions,
//! and inside match-arm bodies. A typed *pattern* binding (`b :: T`) names a
//! bare class only — generic arguments there are a parse error (ADR 0107) —
//! so a pattern carries no type argument to bound-check.

use super::common::*;

fn violations(source: &str) -> Vec<String> {
    analyse_diags(source)
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .map(|d| d.message.to_string())
        .collect()
}

const FIXTURE: &str = r"
Protocol define: Frob
  frob -> Object

Value subclass: Boxed(T :: Frob)
  field: item :: T = 0

Value subclass: Plain
  field: n :: Integer = 0

Value subclass: Good
  frob -> Object => self
";

const VIOLATION: &str = "Type argument Plain for T in Boxed does not conform to Frob";

fn parses_clean(source: &str) {
    let (_, diags) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
    assert!(diags.is_empty(), "fixture must parse: {diags:?}");
}

#[test]
fn state_default_block_param_is_bound_checked() {
    let source = format!("{FIXTURE}\nObject subclass: H\n  state: f = [:b :: Boxed(Plain) | b]\n");
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION]);
}

#[test]
fn state_default_local_annotation_is_bound_checked() {
    let source =
        format!("{FIXTURE}\nObject subclass: H\n  state: f = [c :: Boxed(Plain) := nil. c]\n");
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION]);
}

#[test]
fn state_default_with_conforming_arg_is_clean() {
    let source = format!("{FIXTURE}\nObject subclass: H\n  state: f = [:b :: Boxed(Good) | b]\n");
    parses_clean(&source);
    assert!(violations(&source).is_empty(), "{:?}", violations(&source));
}

#[test]
fn unused_protocol_provided_method_is_bound_checked() {
    let source = format!(
        "{FIXTURE}\nProtocol define: Pick\n  pick -> Object\n  wrap => [:b :: Boxed(Plain) | b]\n  take: x :: Boxed(Plain) => x\n"
    );
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION, VIOLATION]);
}

#[test]
fn match_arm_body_annotations_are_bound_checked() {
    let source = format!(
        "{FIXTURE}\nObject subclass: H\n  go: v =>\n    v match: [\n      _ -> [:b :: Boxed(Plain) | b]\n    ]\n"
    );
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION]);
    // A typed local annotation inside an arm body.
    let source = format!(
        "{FIXTURE}\nObject subclass: H\n  go: v =>\n    v match: [\n      _ -> [c :: Boxed(Plain) := nil. c]\n    ]\n"
    );
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION]);
    // A state default containing a `match:`.
    let source = format!(
        "{FIXTURE}\nObject subclass: H\n  state: f = [:v | v match: [_ -> [:b :: Boxed(Plain) | b]]]\n"
    );
    parses_clean(&source);
    assert_eq!(violations(&source), vec![VIOLATION]);
}

#[test]
fn typed_pattern_binding_cannot_carry_type_arguments() {
    let source = format!(
        "{FIXTURE}\nObject subclass: H\n  go: v =>\n    v match: [\n      b :: Boxed(Plain) -> b;\n      _ -> 0\n    ]\n"
    );
    let (_, diags) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(&source));
    assert!(
        diags.iter().any(|d| d
            .message
            .contains("generic type arguments are not supported")),
        "{diags:?}"
    );
    // A parseable typed pattern raises no bound-check noise.
    let ok = format!(
        "{FIXTURE}\nObject subclass: H\n  go: v =>\n    v match: [\n      {{#ok, n :: Integer}} -> n;\n      _ -> 0\n    ]\n"
    );
    parses_clean(&ok);
    assert!(violations(&ok).is_empty(), "{:?}", violations(&ok));
}
