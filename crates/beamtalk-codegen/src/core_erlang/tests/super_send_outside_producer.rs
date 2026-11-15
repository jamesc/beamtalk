// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4318: `generate_super_send`'s state-threading form reports
//! `StateEffectEscapesExpression` when compiled outside `super_send_producer`.
//!
//! Covers `dispatch_codegen/super_send.rs` lines 148-157: the guard that fires
//! when `from_producer` is false in an actor-instance context where
//! `super_send_threads_state()` is true.

use super::{codegen, verifier_report};
use crate::core_erlang::fault::Fault;

const ACTOR_WITH_SUPER: &str = "Actor subclass: SuperProbe
  state: n = 0

  superProbe =>
    super printString
";

#[test]
fn super_send_state_threading_guard_fires_without_producer() {
    // `Fault::SuperSendNotAProducer` makes `is_super_send_producer()` return
    // false, so the fallback path calls `generate_super_send` with
    // `in_super_send_producer = false` — triggering the guard at
    // `dispatch_codegen/super_send.rs` lines 148-157.
    let report = verifier_report(ACTOR_WITH_SUPER, Some(Fault::SuperSendNotAProducer))
        .expect("should panic when super send is compiled outside its producer");
    assert!(
        report.contains("StateEffectEscapesExpression"),
        "expected StateEffectEscapesExpression; got: {report}"
    );
}

#[test]
fn super_send_state_threading_guard_silent_with_producer() {
    // Normal path: compiled through `super_send_producer`. No panic expected.
    assert!(
        verifier_report(ACTOR_WITH_SUPER, None).is_none(),
        "should compile cleanly when super send goes through its producer"
    );
    // The normal path emits `super_reply/5` (BT-4592).
    let output = codegen(ACTOR_WITH_SUPER);
    assert!(
        output.contains("'super_reply'("),
        "normal path should emit super_reply/5; got:\n{output}"
    );
}
