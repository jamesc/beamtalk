// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3675: per-scope class-variable tokens (ADR 0110, BT-3667 amendment).
//!
//! A scope that cannot thread a `ClassVars` rebind out lexically (a bare
//! block, a loop body, a conditional arm, an `on:do:` body) binds a token,
//! commits what its sends returned under it only after the callee returned,
//! and refreshes from its own entry. Straight-line code and the sealed-class
//! pure shortcut take no part, so they pay nothing. The runtime helpers the
//! generated code calls are cross-checked against the Erlang module's export
//! list (CLAUDE.md: a rule crossing the Rust/Erlang boundary needs a shared
//! conformance fixture, not a comment).

use crate::core_erlang::{CodegenOptions, generate_module};
use std::path::{Path, PathBuf};

fn compile(module_name: &str, src: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(&module, CodegenOptions::new(module_name)).expect("codegen should succeed")
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

const OPEN_CLASS: &str = "Object subclass: ScopeTokenOpen
  classState: n = 0

  class foo => 0

  class viaCollect => #(1, 2) collect: [:x | self foo]

  class viaCondition: x =>
    x > 0 ifTrue: [self foo]
    x

  class straight =>
    self foo
    self foo
";

#[test]
fn confined_send_binds_token_syncs_commits_and_refreshes() {
    let code = compile("bt@scopetokenopen", OPEN_CLASS);
    assert!(
        code.contains("call 'erlang':'make_ref'()"),
        "a scope with a class-side send must bind a token. Got:\n{code}"
    );
    assert!(
        code.contains("'class_var_scope_read'(ClassSelf, [_CVTok"),
        "a confined send must sync from the scope chain before the call. Got:\n{code}"
    );
    assert!(
        code.contains("'class_var_scope_commit'(ClassSelf, _CVTok"),
        "a confined send must commit its returned class vars. Got:\n{code}"
    );
    assert!(
        code.contains("'class_var_scope_take'(ClassSelf, _CVTok"),
        "the scope's refresh must consume only its own entry. Got:\n{code}"
    );
}

#[test]
fn compiled_code_never_reads_the_adr_0110_shadow() {
    // The shadow is written when a callee WRITES, so a read from compiled
    // code resurrects the write of a callee that raised and was caught.
    let code = compile("bt@scopetokenopen", OPEN_CLASS);
    assert!(
        !code.contains("call 'erlang':'get'({'$bt_class_vars_shadow'"),
        "compiled code must not read the shadow. Got:\n{code}"
    );
}

#[test]
fn straight_line_sends_pay_nothing() {
    let code = compile(
        "bt@scopetokenstraight",
        "Object subclass: ScopeTokenStraight
  classState: n = 0

  class foo => 0

  class straight =>
    self foo
    self foo
",
    );
    assert!(
        !code.contains("class_var_scope_") && !code.contains("make_ref"),
        "straight-line sends must not touch the scope helpers. Got:\n{code}"
    );
}

#[test]
fn sealed_pure_send_in_block_pays_nothing() {
    let code = compile(
        "bt@scopetokensealedpure",
        "sealed Object subclass: ScopeTokenSealedPure
  classState: n = 0

  class pure => 0

  class viaDo =>
    #(1, 2) do: [:x | self pure]
    0
",
    );
    assert!(
        !code.contains("class_var_scope_") && !code.contains("make_ref"),
        "a provably pure sealed-class send takes no part in a scope. Got:\n{code}"
    );
}

#[test]
fn sealed_class_conditional_arm_send_commits_like_an_open_class() {
    // BT-3675: sealed and open classes agree for a class's own mutating
    // self-send nested in a conditional arm.
    let code = compile(
        "bt@scopetokensealedarm",
        "sealed Object subclass: ScopeTokenSealedArm
  classState: n = 0

  class increment => self.n := self.n + 1

  class condArm: x =>
    x > 0 ifTrue: [self increment]
    x
",
    );
    assert!(
        code.contains("'class_var_scope_commit'") && code.contains("'class_var_scope_take'"),
        "a sealed class's own mutating self-send in a conditional arm must be \
         recovered like an open class's. Got:\n{code}"
    );
}

#[test]
fn closure_body_exports_to_its_enclosing_scope_on_return() {
    let code = compile("bt@scopetokenopen", OPEN_CLASS);
    assert!(
        code.contains("'class_var_scope_export'(ClassSelf, _CVTok"),
        "a closure body must hand its commit to the enclosing scope as its last \
         step, so a closure that raises exports nothing. Got:\n{code}"
    );
}

#[test]
fn compiled_output_compiles_through_erlc() {
    let code = compile("bt@scopetokenopen", OPEN_CLASS);
    crate::core_erlang::tests::assert_compiles_through_erlc("bt@scopetokenopen", &code);
}

#[test]
fn direct_write_between_sends_in_a_scope_commits_but_straight_line_does_not() {
    let code = compile(
        "bt@scopetokendirect",
        "Object subclass: ScopeTokenDirect
  classState: n = 0

  class bump => self.n := self.n + 1

  class straight =>
    self.n := 1
    self.n := 2
    self.n

  class mixed =>
    self.n := 0
    seen := 0
    1 to: 3 do: [:i |
      self bump
      self.n := self.n + 1
      seen := seen + 1]
    self.n
",
    );
    let straight = code
        .split("'class_straight'/2 = ")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("class_straight present");
    assert!(
        !straight.contains("class_var_scope_"),
        "straight-line direct writes must not touch the scope helpers. Got:\n{straight}"
    );
    let mixed = code
        .split("'class_mixed'/2 = ")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("class_mixed present");
    assert!(
        mixed.matches("'class_var_scope_commit'").count() >= 2,
        "a direct write in a loop body must commit like the send beside it. Got:\n{mixed}"
    );
}

#[test]
fn stored_closure_invoked_later_compiles_through_erlc() {
    // A closure stored in a local, with statements of its own, invoked by a
    // later statement: every scope token named in the output must be bound
    // (an unbound `_CVTokN` is an erlc failure). The closure's writes are
    // NOT kept (known limit, ADR 0110).
    let src = "Object subclass: ScopeTokenStored
  classState: n = 0

  class foo => 0

  class noop => 0

  class run: aBlock => aBlock value: 1

  // A closure with no enclosing scope (an argument of a precise send) whose
  // body has a statement scope a nested closure exported into: nothing of it
  // may leak into the statements after it.
  class leak =>
    self run: [:x |
      y := [self foo] value
      y]
    z := [self noop] value
    z

  class deferred =>
    b := [
      x := self noop
      self foo
      x]
    b value
    b value
    self noop
    self.n

  class deferredInArm =>
    [
      b := [self foo]
      b value
      self noop
      nil
    ] on: Error do: [:e | nil]
    self.n
";
    let code = compile("bt@scopetokenstored", src);
    crate::core_erlang::tests::assert_compiles_through_erlc("bt@scopetokenstored", &code);
}

#[test]
fn runtime_exports_every_helper_codegen_calls() {
    let erl_path =
        repo_root().join("runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl");
    let erl = std::fs::read_to_string(&erl_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", erl_path.display()));
    let code = compile("bt@scopetokenopen", OPEN_CLASS);
    for helper in [
        "class_var_scope_commit",
        "class_var_scope_read",
        "class_var_scope_take",
        "class_var_scope_export",
    ] {
        assert!(
            code.contains(&format!("'beamtalk_class_dispatch':'{helper}'")),
            "codegen no longer calls {helper}; update this conformance list"
        );
        assert!(
            erl.contains(&format!("    {helper}/3")),
            "beamtalk_class_dispatch.erl must export {helper}/3 (codegen calls it)"
        );
    }
}
