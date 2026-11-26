// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5333: a bounded type parameter (`T :: Bound`) is enforced at every
//! annotation site (ADR 0068 Phase 2d) — including method-body local
//! annotations, and including bounds that name a stdlib protocol
//! (`Comparable`), which user compiles seed into the protocol registry.

use super::common::*;

fn violations(source: &str) -> Vec<String> {
    analyse_diags(source)
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .map(|d| d.message.to_string())
        .collect()
}

const LOCAL_PROTOCOL: &str = r"
Protocol define: Frob
  frob -> Object

Value subclass: Boxed(T :: Frob)
  field: item :: T = 0
";

#[test]
fn local_annotation_is_bound_checked() {
    let source = format!(
        "{LOCAL_PROTOCOL}\nObject subclass: Holder\n  go =>\n    b :: Boxed(Integer) := nil.\n    b\n"
    );
    assert_eq!(
        violations(&source),
        vec!["Type argument Integer for T in Boxed does not conform to Frob"]
    );
}

#[test]
fn local_annotation_in_nested_block_is_bound_checked() {
    let source = format!(
        "{LOCAL_PROTOCOL}\nObject subclass: Holder\n  go =>\n    #(1) do: [:x | b :: Boxed(Integer) := nil. b]\n"
    );
    assert_eq!(violations(&source).len(), 1, "{:?}", violations(&source));
}

#[test]
fn local_annotation_with_conforming_arg_is_clean() {
    let source = r"
Protocol define: Frob
  frob -> Object

Value subclass: Boxed(T :: Frob)
  field: item :: T = 0

Value subclass: Good
  frob -> Object => self

Object subclass: Holder
  go =>
    b :: Boxed(Good) := nil.
    b
";
    assert!(violations(source).is_empty(), "{:?}", violations(source));
}

const COMPARABLE_BOX: &str = r"
Value subclass: Boxed(T :: Comparable)
  field: item :: T = 0

Value subclass: Plain
  field: n :: Integer = 0
";

#[test]
fn stdlib_protocol_bound_is_enforced_on_field() {
    let source =
        format!("{COMPARABLE_BOX}\nObject subclass: H\n  state: h :: Boxed(Plain) = nil\n");
    assert_eq!(
        violations(&source),
        vec!["Type argument Plain for T in Boxed does not conform to Comparable"]
    );
}

#[test]
fn stdlib_protocol_bound_is_enforced_on_param_return_and_local() {
    let source = format!(
        "{COMPARABLE_BOX}\nObject subclass: H\n  take: b :: Boxed(Plain) -> Boxed(Plain) => b\n  go =>\n    c :: Boxed(Plain) := nil.\n    c\n"
    );
    assert_eq!(violations(&source).len(), 3, "{:?}", violations(&source));
}

#[test]
fn stdlib_protocol_bound_accepts_conforming_arg() {
    let source = format!(
        "{COMPARABLE_BOX}\nObject subclass: H\n  state: h :: Boxed(Integer) = nil\n  take: b :: Boxed(String) => b\n"
    );
    assert!(violations(&source).is_empty(), "{:?}", violations(&source));
}

#[test]
fn stdlib_protocol_annotation_is_enforced() {
    let source = r"
Value subclass: Plain
  field: n :: Integer = 0

Object subclass: H
  smaller: a :: Comparable => a
  go => self smaller: Plain new
";
    let msgs = messages(source);
    assert!(
        msgs.iter()
            .any(|m| m.contains("Plain") && m.contains("Comparable")),
        "{msgs:?}"
    );
}

#[test]
fn package_declared_protocol_shadows_stdlib_one() {
    // A package's own `Comparable` wins over the baked stdlib one.
    let source = r"
Protocol define: Comparable
  frob -> Object

Value subclass: Boxed(T :: Comparable)
  field: item :: T = 0

Value subclass: Good
  frob -> Object => self

Object subclass: H
  state: h :: Boxed(Good) = nil
";
    assert!(violations(source).is_empty(), "{:?}", violations(source));
}
