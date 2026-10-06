// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Coverage for the two untested paths in `gen_server/state.rs`:
//!
//! 1. `generate_own_state_fields` — the `omit_late_defaultless_slot` skip
//!    when a subclass (`has_parent_init`) has a late defaultless field.
//! 2. `generate_initial_state_fields` — the `else` fallback branch that runs
//!    when no class identity is set but the module has classes with state
//!    fields.

use super::*;

/// Extract the body of `ChildFields` from generated Core Erlang.
///
/// Returns the text between `let ChildFields = ~{` and the first `}~`.
/// Panics if the expected delimiters are absent.
fn extract_child_fields(code: &str) -> &str {
    code.split("let ChildFields = ~{")
        .nth(1)
        .expect("init/1 must emit ChildFields for a subclass")
        .split("}~")
        .next()
        .expect("ChildFields must close with }~")
}

// ── Gap 1: generate_own_state_fields late-defaultless skip ───────────────────

/// A subclass that extends a user-defined Actor (`has_parent_init=true`)
/// and carries a `late state:` field without a default must have that field
/// absent from `ChildFields`. The sibling eager field must still appear.
///
/// This exercises the `omit_late_defaultless_slot` `continue` inside
/// `generate_own_state_fields`, which is only reachable when a class's
/// superclass is not Actor/Object.
#[test]
fn late_defaultless_slot_skipped_in_own_state_fields() {
    // LoggingCounter extends Counter (a user-defined Actor subclass), so
    // has_parent_init=true — generate_own_state_fields is called for
    // LoggingCounter's own state fields only (parent state comes from
    // bt@counter:init/1).  The `late state: audit` has no default, so
    // omit_late_defaultless_slot returns true → the continue fires.
    let src = concat!(
        "Counter subclass: LoggingCounter\n",
        "  late state: audit :: Logger\n",
        "  state: logCount = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_counter"))
        .expect("codegen should succeed");

    let child_fields = extract_child_fields(&code);

    assert!(
        !child_fields.contains("'audit'"),
        "Late defaultless slot 'audit' must be absent from ChildFields. Got:\n{child_fields}"
    );
    assert!(
        child_fields.contains("'logCount'"),
        "Eager slot 'logCount' must still appear in ChildFields. Got:\n{child_fields}"
    );
}

/// A subclass with ONLY a late defaultless field produces a `ChildFields`
/// containing no user-defined state keys — only the mandatory internal
/// `$bt*` keys (`__class_mod__`, `__shape_version__`).
#[test]
fn own_state_fields_empty_when_only_late_defaultless() {
    let src = concat!(
        "Counter subclass: LoggingCounter\n",
        "  late state: audit :: Logger\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_counter"))
        .expect("codegen should succeed");

    let child_fields = extract_child_fields(&code);

    // No user-defined key may appear — only internal `__*__` entries.
    let user_entries: Vec<&str> = child_fields
        .lines()
        .filter(|l| l.contains("=>") && !l.contains("'__"))
        .collect();
    assert!(
        user_entries.is_empty(),
        "ChildFields must contain only internal keys when the sole field is late defaultless. \
         Unexpected user entries: {user_entries:?}. Full body:\n{child_fields}"
    );
}

// ── Gap 2: generate_initial_state_fields fallback branch ─────────────────────

/// When `generate_initial_state_fields` is called on a generator whose
/// derived class name does not match any class in the module, the `else`
/// fallback branch runs and emits state fields from all module classes.
///
/// This path is documented in state.rs as "Load-bearing for those tests,
/// not dead code" — for hand-constructed test fixtures that call the method
/// directly without going through `setup_class_identity`.
#[test]
fn fallback_state_fields_emitted_when_class_identity_absent() {
    // Parse a module whose sole class ("Counter") has a state field.
    let src = concat!("Actor subclass: Counter\n", "  state: count = 0\n",);
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    // A fresh generator for "some_other": class_name() derives to "SomeOther",
    // which does not match "Counter" → current_class() returns None → fallback.
    let mut generator = CoreErlangGenerator::new("some_other");

    let fields = generator
        .generate_initial_state_fields(&module)
        .expect("fallback path should succeed");

    assert!(
        !fields.is_empty(),
        "fallback branch should emit at least one field document"
    );

    let text = fields
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect::<String>();

    assert!(
        text.contains("count"),
        "fallback should emit 'count' from Counter's state. Got: {text:?}"
    );
}

/// A late defaultless field is also omitted by the fallback branch — the same
/// `omit_late_defaultless_slot` guard applies there too.
#[test]
fn fallback_skips_late_defaultless_slot_too() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  late state: handle :: Handle\n",
        "  state: count = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let mut generator = CoreErlangGenerator::new("some_other");

    let fields = generator
        .generate_initial_state_fields(&module)
        .expect("fallback path should succeed");

    let text = fields
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect::<String>();

    assert!(
        text.contains("count"),
        "fallback should emit eager 'count'. Got: {text:?}"
    );
    assert!(
        !text.contains("handle"),
        "fallback must skip late defaultless 'handle'. Got: {text:?}"
    );
}
