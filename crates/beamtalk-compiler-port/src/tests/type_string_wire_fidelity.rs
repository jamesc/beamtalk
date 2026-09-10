// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests pinning that a type-annotation *string* —
//! `method_signature_terms`'s `TypeAnnotation::type_name()` rendering,
//! carried in `return_type`/`param_types` response fields (see
//! `method_definition_ok_response`, ADR 0105 Phase 1) —
//! crosses the compiler port's `{packet, 4}`-framed ETF wire unchanged.
//!
//! `Term::encode`/`Term::decode` (`eetf`) implement the same External
//! Term Format the BEAM's `term_to_binary`/`binary_to_term` speak, so
//! round-tripping a real response `Term` through them here exercises
//! the identical byte-level encoding the live Rust ⇄ Erlang port pipe
//! uses — there is no separate "real" wire representation this test
//! would be missing by not spawning a BEAM. What it *doesn't* cover is
//! the Erlang-side decode; that's `beamtalk_compiler_port.erl`'s job,
//! exercised by the runtime `EUnit` suite, not this crate.
//!
//! Generated strings reuse `DeclaredType`'s `Display` rendering
//! (byte-identical to `TypeAnnotation::type_name()`, enforced by
//! `declared_type.rs`'s own `assert_display_parity` tests) so the
//! shapes this test carries across the wire — union, generic,
//! singleton, `FalseOr`, difference, intersection, self-types — are
//! the exact same canonical text a real compiled method signature
//! would send, not an ad hoc string format.
//!
//! The generator itself (`arb_declared_type`) lives in beamtalk-core's
//! `test_helpers::test_support` (behind the `test` Cargo feature this
//! crate's dev-dependency enables) rather than being copied here, so
//! this test and `declared_type.rs`'s own fixed-point property tests
//! exercise the exact same shape space by construction — see this
//! repo's "No duplicate implementations" rule.

use super::*;
use beamtalk_core::test_helpers::test_support::arb_declared_type;

fn arb_type_annotation_string() -> impl Strategy<Value = String> {
    arb_declared_type().prop_map(|dt| dt.to_string())
}

proptest! {
    #![proptest_config(proptest_config())]

    /// A `return_type`/`param_types` string embedded in a real
    /// production response (`method_definition_ok_response`)
    /// survives ETF encode → decode byte-for-byte: no truncation,
    /// no re-encoding drift, regardless of how the type-annotation
    /// text is shaped.
    #[test]
    fn type_annotation_string_survives_wire_roundtrip(type_str in arb_type_annotation_string()) {
        let response = method_definition_ok_response(
            "Foo",
            "bar",
            false,
            "bar => self",
            &type_str,
            std::slice::from_ref(&type_str),
            &[],
        );

        let mut buf = Vec::new();
        response.encode(&mut buf).expect("encoding a response Term must not fail");
        let decoded = Term::decode(io::Cursor::new(buf))
            .expect("decoding a just-encoded response Term must not fail");

        prop_assert_eq!(
            response_field_str(&decoded, "return_type"),
            Some(type_str.clone()),
            "return_type did not survive the ETF wire round trip for {:?}",
            type_str,
        );
        prop_assert_eq!(
            response_field_str_list(&decoded, "param_types"),
            Some(vec![type_str.clone()]),
            "param_types did not survive the ETF wire round trip for {:?}",
            type_str,
        );
    }
}
