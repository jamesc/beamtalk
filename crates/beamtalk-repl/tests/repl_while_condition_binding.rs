// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! PIN-BUG BT-4015: at the REPL a binding written in a `whileTrue:` /
//! `whileFalse:` condition is not carried out of the condition closure (BT-4002
//! carried the value-type `Self` a condition writes; the REPL bindings family
//! is BT-4015). A body that reads the binding references the closure's own
//! variable, which `erlc` rejects as unbound. A fix for BT-4015 flips this pin
//! (the input must compile, and answer `#[3, 3]` in the REPL protocol suite).

use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;
use std::process::Command;

#[test]
fn pin_bug_bt4015_repl_condition_binding_read_by_the_body() {
    let src = "[cw := cw + 1. cw < 3] whileTrue: [ck := ck + cw]";
    let (module, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "{diags:?}");
    let expressions: Vec<_> = module
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect();
    let code = generate_repl_expressions(&expressions, "bt4015_pin").expect("codegen succeeds");
    let dir = beamtalk_core::test_helpers::unique_temp_dir("bt4015_pin");
    std::fs::create_dir_all(&dir).expect("temp dir");
    let file = dir.join("bt4015_pin.core");
    std::fs::write(&file, &code).expect("write core");
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(&dir)
        .arg(&file)
        .output();
    let _ = std::fs::remove_dir_all(&dir);
    let Ok(output) = output else {
        println!("skipping: erlc not available");
        return;
    };
    let stderr = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !output.status.success() && stderr.contains("unbound variable"),
        "BT-4015 pin: expected erlc to reject an unbound condition binding, got:\n{stderr}\n{code}"
    );
}
