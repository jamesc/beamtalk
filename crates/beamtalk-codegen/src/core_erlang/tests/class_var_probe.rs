// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 Phase 0 (BT-3703): the `BEAMTALK_CLASS_VAR_PROBE` codegen flag.
//!
//! With the flag off the generated Core Erlang must not mention the probe at
//! all (the byte-identity guarantee against `main` is checked over the whole
//! corpus by `just core-diff`, `docs/development/testing-strategy.md` § 7b);
//! with it on, every class-variable read or write emits a
//! `beamtalk_class_var_probe:report/6` call whose last argument says whether
//! the access sits inside a non-inlined block.

use crate::core_erlang::{CodegenOptions, generate_module};

const SOURCE: &str = "Object subclass: ProbeFoo
  classState: n = 0

  class viaCollect => #(1, 2) collect: [:x | self.n + x]
  class bump => self.n := self.n + 1
  class clear => self clearField: #n
";

fn generate(probe: bool) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(SOURCE);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(
        &module,
        CodegenOptions::new("probe_foo").with_class_var_probe(probe),
    )
    .expect("codegen should succeed")
}

const PROBE: &str = "call 'beamtalk_class_var_probe':'report'(";

#[test]
fn probe_off_emits_no_probe_calls() {
    let code = generate(false);
    assert!(
        !code.contains("beamtalk_class_var_probe"),
        "flag off must not reference the probe module. Got:\n{code}"
    );
}

#[test]
fn probe_off_is_deterministic_and_independent_of_probe_on() {
    // Turning the probe on and off again must give the same text as never
    // turning it on: no state leaks through the generator between runs.
    let before = generate(false);
    let _on = generate(true);
    assert_eq!(before, generate(false));
}

#[test]
fn probe_on_reports_block_read_as_in_block() {
    let code = generate(true);
    assert!(
        code.contains(&format!(
            "{PROBE}ClassSelf, 'ProbeFoo', 'viaCollect', 'read', 'n', 'true')"
        )),
        "a read inside a non-inlined block must report in_block = true. Got:\n{code}"
    );
}

#[test]
fn probe_on_reports_method_level_read_and_write() {
    let code = generate(true);
    for kind in ["read", "write"] {
        assert!(
            code.contains(&format!(
                "{PROBE}ClassSelf, 'ProbeFoo', 'bump', '{kind}', 'n', 'false')"
            )),
            "a method-level {kind} must report in_block = false. Got:\n{code}"
        );
    }
}

#[test]
fn probe_on_reports_clear_field_as_write() {
    let code = generate(true);
    assert!(
        code.contains(&format!(
            "{PROBE}ClassSelf, 'ProbeFoo', 'clear', 'write', 'n', 'false')"
        )),
        "clearField: is a class-variable write. Got:\n{code}"
    );
}

#[test]
fn probe_on_output_compiles_through_erlc() {
    super::assert_compiles_through_erlc("probe_foo", &generate(true));
}

#[test]
fn probe_on_reports_has_field_as_read() {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(
        "Object subclass: ProbeBar
  classState: n = 0

  class check => self hasField: #n
",
    );
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("probe_bar").with_class_var_probe(true),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains(&format!(
            "{PROBE}ClassSelf, 'ProbeBar', 'check', 'read', 'n', 'false')"
        )),
        "hasField: is a class-variable read. Got:\n{code}"
    );
}
