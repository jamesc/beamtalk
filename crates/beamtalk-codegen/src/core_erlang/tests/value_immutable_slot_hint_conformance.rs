// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-boundary conformance fixture for the Value `immutable_value` slot
//! hint (BT-4273).
//!
//! A user-defined Value raises `immutable_value` for `fieldAt:put:` (and
//! `clearField:`) from three places: the `fieldAt:put:` object-reflection
//! intrinsic and every generated Value `dispatch/3` arm (both Rust, both
//! reading the single [`errors::VALUE_IMMUTABLE_SLOT_HINT`]), and the
//! runtime's dynamic pre-check `beamtalk_primitive:is_ivar_method/1`
//! (Erlang). The Rust/Erlang pair is a boundary that cannot be deleted
//! (`docs/development/architecture-principles.md` §7), so both sides are
//! pinned to the shared fixture
//! `runtime/apps/beamtalk_runtime/test/fixtures/value_immutable_slot_hint_conformance.json`.
//! This half asserts the constant and the emitted Core Erlang of both
//! compiled paths; the Erlang half is
//! `beamtalk_primitive_tests:value_type_field_at_put_hint_matches_shared_fixture_test/0`.
//!
//! [`errors::VALUE_IMMUTABLE_SLOT_HINT`]: super::super::errors::VALUE_IMMUTABLE_SLOT_HINT

use super::{codegen, repo_root};
use crate::core_erlang::errors::VALUE_IMMUTABLE_SLOT_HINT;
use beamtalk_cerl_doc::leaf;

/// The canonical hint text from the shared fixture.
fn fixture_hint() -> String {
    let path = repo_root().join(
        "runtime/apps/beamtalk_runtime/test/fixtures/value_immutable_slot_hint_conformance.json",
    );
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
    let fixture: serde_json::Value = serde_json::from_str(&raw).expect("fixture is JSON");
    fixture["hint"]
        .as_str()
        .expect("fixture.hint is a string")
        .to_string()
}

/// The hint as it appears in emitted Core Erlang (a binary literal).
fn emitted_hint() -> String {
    leaf::binary_lit(fixture_hint()).to_pretty_string()
}

#[test]
fn rust_hint_constant_matches_shared_fixture() {
    assert_eq!(VALUE_IMMUTABLE_SLOT_HINT, fixture_hint());
}

#[test]
fn value_dispatch_arms_emit_shared_fixture_hint() {
    let code = codegen(concat!(
        "Value subclass: HintPoint\n",
        "  field: x :: Integer = 0\n",
        "  x -> Integer => self.x\n",
    ));
    let hint = emitted_hint();
    // One for the `fieldAt:put:` arm, one for the `clearField:` arm.
    assert_eq!(
        code.matches(hint.as_str()).count(),
        2,
        "dispatch/3 fieldAt:put: and clearField: arms must both carry the shared hint. Got:\n{code}"
    );
}

#[test]
fn field_at_put_intrinsic_emits_shared_fixture_hint() {
    let code = codegen(concat!(
        "Object subclass: HintCaller\n",
        "  poke: w :: Object -> Object =>\n",
        "    w fieldAt: #x put: 1\n",
    ));
    assert!(
        code.contains(emitted_hint().as_str()),
        "the fieldAt:put: intrinsic must carry the shared hint. Got:\n{code}"
    );
}
