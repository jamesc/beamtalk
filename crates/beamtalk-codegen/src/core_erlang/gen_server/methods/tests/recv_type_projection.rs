// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3217 (ADR 0115 Phase 2): `project_recv_type` unit coverage.
//!
//! The codegen fixture matrix (`codegen/core_erlang/tests/recv_type.rs`)
//! exercises this rule end-to-end through real `.bt` source for every case
//! reachable from actual inference (typed/protocol/dynamic/union/native/
//! alias locals, `Meta{C}`, self-send, FFI receiver). `Intersection` and
//! `Negation` are not reachable that way without substantially more
//! fixture machinery (ADR 0068 protocol composition, ADR 0102 negation
//! narrowing) for a result this rule treats identically to `Union` —
//! tested directly here instead, alongside every other variant, as a
//! complete case-by-case pin of the write-path projection rule.

use super::*;
use beamtalk_core::semantic_analysis::{DynamicReason, InferredType, TypeProvenance};
use ecow::EcoString;

fn known(class_name: &str, provenance: TypeProvenance) -> InferredType {
    InferredType::Known {
        class_name: class_name.into(),
        type_args: vec![],
        provenance,
    }
}

#[test]
fn project_recv_type_known_declared_yields_name() {
    let ty = known("Counter", TypeProvenance::Declared(s()));
    assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
}

#[test]
fn project_recv_type_known_inferred_yields_name() {
    let ty = known("Counter", TypeProvenance::Inferred(s()));
    assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
}

#[test]
fn project_recv_type_known_substituted_yields_name() {
    let ty = known("Counter", TypeProvenance::Substituted(s()));
    assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
}

#[test]
fn project_recv_type_known_with_type_args_drops_them() {
    // `Collection(Integer)` still keys `recv_type: 'Collection'` — the
    // generic parameter doesn't change which class/protocol a reader
    // needs to reason about (ADR 0115 §Write path).
    let ty = InferredType::Known {
        class_name: "Collection".into(),
        type_args: vec![InferredType::known("Integer")],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Collection"));
}

#[test]
fn project_recv_type_known_extracted_native_type_coarsens_to_dynamic() {
    // ADR 0075 native/FFI type name — no `beamtalk_class_metadata` row.
    let ty = known("List", TypeProvenance::Extracted);
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_known_aliased_coarsens_to_dynamic() {
    // ADR 0108 alias display name — no `beamtalk_class_metadata` row.
    let ty = known(
        "RestartStrategy",
        TypeProvenance::Aliased {
            name: "RestartStrategy".into(),
            span: s(),
        },
    );
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_meta_yields_class_object() {
    let ty = InferredType::Meta {
        class_name: "Counter".into(),
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::ClassObject(n) if n == "Counter"));
}

#[test]
fn project_recv_type_dynamic_coarsens_to_dynamic() {
    let ty = InferredType::Dynamic(DynamicReason::Unknown);
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_never_coarsens_to_dynamic() {
    assert!(matches!(
        project_recv_type(&InferredType::Never),
        RecvType::Dynamic
    ));
}

#[test]
fn project_recv_type_union_of_resolvable_members_yields_union() {
    // BT-3215: every member resolves to a clean single name, so the
    // whole union keys precisely instead of coarsening to `dynamic`.
    let ty = InferredType::simple_union(&["String", "Integer"]);
    assert!(matches!(
        project_recv_type(&ty),
        RecvType::Union(names) if names == vec![EcoString::from("Integer"), EcoString::from("String")]
    ));
}

#[test]
fn project_recv_type_union_dedupes_members() {
    // Two members that resolve to the same name (e.g. distinct
    // provenance for the same class) must not double up in the list.
    let ty = InferredType::Union {
        members: vec![
            known("Foo", TypeProvenance::Inferred(s())),
            known("Foo", TypeProvenance::Declared(s())),
        ],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(
        project_recv_type(&ty),
        RecvType::Union(names) if names == vec![EcoString::from("Foo")]
    ));
}

#[test]
fn project_recv_type_union_with_unresolvable_member_coarsens_to_dynamic() {
    // BT-3215: a partial member list would be unsound (Constraint 2) —
    // one member that can't resolve to a clean name (here, `Dynamic`)
    // must coarsen the *whole* union, not just drop that member.
    let ty = InferredType::Union {
        members: vec![
            known("Foo", TypeProvenance::Inferred(s())),
            InferredType::Dynamic(DynamicReason::Unknown),
        ],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_union_with_nested_composed_member_coarsens_to_dynamic() {
    // A member that is itself a `Union`/`Intersection` never resolves
    // to a single name (`project_composed` only builds one level of
    // member list), so it coarsens the outer union too.
    let ty = InferredType::Union {
        members: vec![
            known("Foo", TypeProvenance::Inferred(s())),
            InferredType::simple_union(&["Bar", "Baz"]),
        ],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_intersection_of_resolvable_members_yields_intersection() {
    // BT-3215: ADR 0068 protocol composition
    // (`Collection(Object) & Comparable`) now keys precisely instead of
    // deferring to `dynamic`.
    let ty = InferredType::Intersection {
        members: vec![
            known("Printable", TypeProvenance::Inferred(s())),
            known("Comparable", TypeProvenance::Inferred(s())),
        ],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(
        project_recv_type(&ty),
        RecvType::Intersection(names)
            if names == vec![EcoString::from("Comparable"), EcoString::from("Printable")]
    ));
}

#[test]
fn project_recv_type_intersection_with_unresolvable_member_coarsens_to_dynamic() {
    let ty = InferredType::Intersection {
        members: vec![
            known("Comparable", TypeProvenance::Inferred(s())),
            known("List", TypeProvenance::Extracted),
        ],
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}

#[test]
fn project_recv_type_negation_coarsens_to_dynamic() {
    let ty = InferredType::Negation {
        base: Box::new(known("Symbol", TypeProvenance::Inferred(s()))),
        excluded: Box::new(known("#foo", TypeProvenance::Inferred(s()))),
        provenance: TypeProvenance::Inferred(s()),
    };
    assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
}
