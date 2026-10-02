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
fn nested_scope_refresh_falls_back_to_the_enclosing_scopes_commit() {
    // BT-3683: an arm of a threaded loop body that is not taken leaves its own
    // token empty, so its refresh answers the fallback and commits it to the
    // enclosing loop scope. The fallback must be the enclosing scope's newest
    // commit (an earlier iteration's write), not the stale lexical version
    // captured at arm entry.
    let src = "Object subclass: ScopeTokenArms
  classState: n = 0

  class foo => self.n := self.n + 1

  class loopArm =>
    seen := 0
    #(1, 2) do: [:i |
      seen := seen + 1
      i =:= 1 ifTrue: [self foo]
    ]
    self.n
";
    let code = compile("bt@scopetokenarms", src);
    let takes: Vec<&str> = code.matches("'class_var_scope_take'(ClassSelf, ").collect();
    assert!(!takes.is_empty(), "expected a scope refresh. Got:\n{code}");
    let nested_fallback = code
        .match_indices("'class_var_scope_take'(ClassSelf, ")
        .any(|(i, m)| {
            code[i + m.len()..]
                .split_once(", ")
                .is_some_and(|(_, rest)| {
                    rest.starts_with(
                        "call 'beamtalk_class_dispatch':'class_var_scope_read'(ClassSelf, [_CVTok",
                    )
                })
        });
    assert!(
        nested_fallback,
        "a nested scope's refresh must fall back to the enclosing scopes' commit. Got:\n{code}"
    );
}

/// How many commits of a send's reply are conditional on a
/// `{'class_var_result', _, _}` reply (BT-3690).
fn conditional_send_commits(code: &str) -> usize {
    code.matches("of <{'class_var_result', _CW").count()
}

#[test]
fn confined_send_commits_only_a_class_var_result_reply() {
    // BT-3690: a plain reply means the callee left the class variables as they
    // were passed in, so the send's commit is made only inside the
    // `class_var_result` arm. Both late-bound sends of the loop body qualify.
    let src = "Object subclass: ScopeTokenPlain
  classState: n = 0

  class foo => 0

  class twice => #(1, 2) do: [:x |
    self foo
    self foo
  ]
";
    let code = compile("bt@scopetokenplain", src);
    assert_eq!(
        conditional_send_commits(&code),
        2,
        "each confined send commits only a class_var_result reply. Got:\n{code}"
    );
    assert!(
        code.contains("'class_var_scope_read'(ClassSelf, [_CVTok"),
        "the pre-call sync is unchanged (a plain reader still sees the scope's writes). Got:\n{code}"
    );
}

#[test]
fn confined_send_with_a_block_argument_keeps_committing_a_plain_reply() {
    // BT-3690: a block built in the send's arguments may be invoked by the
    // callee and export its writes into the scope; the send's own commit has
    // to overwrite that export exactly as before (the pinned ADR 0110 / BT-3682
    // limit), so only the argument-free send is conditional.
    let src = "Object subclass: ScopeTokenHom
  classState: n = 0

  class foo => 0

  class section: aBlock :: Block => aBlock value

  class viaLoop => #(1) do: [:x | self section: [self foo]]
";
    let code = compile("bt@scopetokenhom", src);
    assert_eq!(
        conditional_send_commits(&code),
        1,
        "only the block-free `self foo` commits conditionally; the HOM send keeps \
         its unconditional commit. Got:\n{code}"
    );
}

/// The text of the fold-body lambda (`fun (I, _AccCV…) -> … `) of the first
/// `lists:foldl` in `method`'s generated code.
fn fold_lambda<'a>(code: &'a str, method: &str) -> &'a str {
    let body = code
        .split(&format!("'{method}'/2 = "))
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .unwrap_or_else(|| panic!("{method} present"));
    let start = body.find("fun (I, _AccCV").expect("fold lambda present");
    let end = body
        .find("call 'lists':'foldl'(")
        .expect("foldl call present");
    &body[start..end]
}

const SEALED_ARMS: &str = "sealed Object subclass: ScopeTokenSealedArms
  classState: n = 0

  class bump => self.n := self.n + 1

  class plain => 0

  class reader => self.n

  class armsDo =>
    seen := 0
    #(1, 2, 3) do: [:i |
      seen := seen + 1
      i =:= 1 ifTrue: [self bump]
      self plain
    ]
    self.n

  class readerAfterArm =>
    seen := 0
    #(1, 2, 3) collect: [:i |
      seen := seen + 1
      i =:= 1 ifTrue: [self bump]
      self reader
    ]
";

#[test]
fn sealed_fold_body_syncs_from_the_scope_before_carrying_class_vars_out() {
    // BT-3691: the arm's write is exported into the loop scope's token only; the
    // sealed pure `self plain` mints no version. The accumulator's trailing
    // `ClassVars` must therefore be re-synced from the scope's commit at the end
    // of the body, or the fold's stale version is committed over the newer entry.
    let code = compile("bt@scopetokensealedarms", SEALED_ARMS);
    let lambda = fold_lambda(&code, "class_armsDo");
    let sync = "'class_var_scope_read'(ClassSelf, [_CVTok";
    let last_sync = lambda
        .rfind(sync)
        .unwrap_or_else(|| panic!("the body must sync at its end. Got:\n{lambda}"));
    let last_export = lambda
        .rfind("'class_var_scope_export'")
        .expect("the arm exports its commit");
    assert!(
        last_sync > last_export,
        "the end-of-iteration sync must follow the arm's export. Got:\n{lambda}"
    );
    // The accumulator carries the version that sync bound.
    let bound = lambda[..last_sync]
        .rsplit("let ")
        .next()
        .and_then(|l| l.split(" = ").next())
        .expect("sync binds a version");
    assert!(
        bound.starts_with("ClassVars")
            && lambda.contains(&format!(", {bound}}} in let _RawFoldCV")),
        "the fold accumulator must carry the synced version `{bound}`. Got:\n{lambda}"
    );
}

#[test]
fn sealed_pure_reader_after_a_scope_commit_syncs_before_the_call() {
    // BT-3691: a sealed send that never writes still READS the class variables,
    // so once the scope holds a commit it must sync from it before the call.
    let code = compile("bt@scopetokensealedarms", SEALED_ARMS);
    let lambda = fold_lambda(&code, "class_readerAfterArm");
    let call = lambda
        .find("'class_reader'(ClassSelf, ")
        .expect("the reader is called directly");
    assert!(
        lambda[call..].starts_with(
            "'class_reader'(ClassSelf, call 'beamtalk_class_dispatch':'class_var_scope_read'(ClassSelf, [_CVTok"
        ),
        "the reader must be passed the scope's newest commit. Got:\n{lambda}"
    );
    assert!(
        lambda[..call].contains("'class_var_scope_export'"),
        "the arm exports before the reader. Got:\n{lambda}"
    );
    assert!(
        !lambda[call..].contains("'class_var_scope_commit'"),
        "a pure reply commits nothing after the reader. Got:\n{}",
        &lambda[call..]
    );
}

#[test]
fn sealed_pure_send_in_an_untouched_arm_of_a_used_loop_pays_no_token_or_take() {
    // BT-3691: the pure reader sits in an arm of its own, inside a loop whose
    // token the other arm already uses. Its inline read names only the tokens
    // already referenced, so the reader's arm binds no token of its own, exports
    // nothing and needs no refresh: only the loop's token and the writing arm's
    // closure token exist, and the loop is refreshed once.
    let code = compile(
        "bt@scopetokensealedpurearm",
        "sealed Object subclass: ScopeTokenSealedPureArm
  classState: n = 0

  class bump => self.n := self.n + 1

  class reader => self.n

  class armsDo =>
    seen := 0
    #(1, 2, 3) do: [:i |
      seen := seen + 1
      i =:= 1 ifTrue: [self bump]
      i =:= 2 ifTrue: [self reader]
    ]
    self.n
",
    );
    let method = code
        .split("'class_armsDo'/2 = ")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("class_armsDo present");
    assert_eq!(
        method.matches("call 'erlang':'make_ref'()").count(),
        2,
        "only the loop's token and the writing arm's closure token. Got:\n{method}"
    );
    assert_eq!(
        method.matches("'class_var_scope_export'").count(),
        1,
        "only the writing arm exports. Got:\n{method}"
    );
    assert_eq!(
        method.matches("'class_var_scope_take'").count(),
        1,
        "only the loop is refreshed. Got:\n{method}"
    );
    let read_prefix = "'class_reader'(ClassSelf, call 'beamtalk_class_dispatch':'class_var_scope_read'(ClassSelf, [";
    let (_, after) = method
        .split_once(read_prefix)
        .unwrap_or_else(|| panic!("the reader reads the scope inline. Got:\n{method}"));
    let tokens = after.split(']').next().expect("token list");
    assert!(
        !tokens.contains(','),
        "the inline read names only the referenced loop token, got [{tokens}]. Got:\n{method}"
    );
}

#[test]
fn sealed_pure_reader_before_the_writing_arm_reads_the_loop_token() {
    // BT-3691: the loop's token is bound once per loop entry, outside the loop,
    // and the writing arm AFTER the reader commits into it before the reader runs
    // again in the next iteration. At codegen time that token is still unused
    // when the reader is generated, but a read that left it out would answer an
    // older (or the stale lexical) version. A loop body that can write class
    // variables therefore names the tokens that outlive its iterations.
    for (loop_src, name) in [
        ("1 to: 3 do: [:i |", "ToDo"),
        ("3 timesRepeat: [", "TimesRepeat"),
        ("#(1, 2, 3) do: [:i |", "Do"),
    ] {
        let src = format!(
            "sealed Object subclass: ScopeTokenReaderFirst{name}
  classState: n = 0

  class bump => self.n := self.n + 1

  class reader => self.n

  class m =>
    seen := 0
    acc := 0
    {loop_src}
      seen := seen + 1
      acc := acc + self reader
      seen =:= 1 ifTrue: [self bump]
    ]
    acc
"
        );
        let code = compile(&format!("bt@scopetokenreaderfirst{name}"), &src);
        let method = code
            .split("'class_m'/2 = ")
            .nth(1)
            .and_then(|rest| rest.split("\n\n").next())
            .expect("class_m present");
        let read_prefix = "'class_reader'(ClassSelf, call 'beamtalk_class_dispatch':'class_var_scope_read'(ClassSelf, [_CVTok";
        let (_, after) = method
            .split_once(read_prefix)
            .unwrap_or_else(|| panic!("{name}: the reader reads the scope. Got:\n{method}"));
        let loop_token = format!("_CVTok{}", after.split(']').next().expect("token list"));
        assert!(
            method.contains(&format!("let {loop_token} = call 'erlang':'make_ref'()")),
            "{name}: the named token must be bound. Got:\n{method}"
        );
        if name != "Do" {
            // (a `do:` fold's arm closure exports into a token of its own statement
            // scope, which the body then refreshes into the loop's)
            assert!(
                method.contains(&format!(", {loop_token}) in")),
                "{name}: the writing arm exports into the token the reader reads. Got:\n{method}"
            );
        }
    }
}

#[test]
fn sealed_arm_export_loop_compiles_through_erlc() {
    let code = compile("bt@scopetokensealedarms", SEALED_ARMS);
    crate::core_erlang::tests::assert_compiles_through_erlc("bt@scopetokensealedarms", &code);
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
