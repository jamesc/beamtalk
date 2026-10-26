// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tier 2 stored/invoked block-value-call codegen: immediately-
//! invoked blocks, field- and local-var-stored blocks invoked
//! across methods, and their state-threading and tuple-unpacking
//! fixtures.

use super::*;

#[test]
fn test_immediately_invoked_literal_block_with_field_mutation_compiles() {
    // `[self.total := self.total + n] value` — a literal block that is
    // immediately invoked (the block is the *receiver* of `value`, not stored or
    // passed) — must NOT hit the FieldAssignmentInUnsupportedBlock rejection. The
    // compiler inlines this case correctly (state threads through StateAcc, same
    // as ifTrue:/do:), unlike a block bound to a variable and invoked later.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    [self.total := self.total + n] value\n";
    let code = codegen(src);
    assert!(
        code.contains("'maps':'put'('total'"),
        "Immediately-invoked block with a field mutation must thread state via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_immediately_invoked_literal_block_value_keyword_with_field_mutation_compiles() {
    // PR review follow-up: same as the unary `value` case above, but
    // for the keyword form `[...] value: arg`. This goes through a separate
    // code path (`try_generate_block_value_keyword`'s check in
    // intrinsics.rs) that must also inline field mutations rather than falling
    // through to generate_block's rejection.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    [:x | self.total := self.total + x] value: n\n";
    let code = codegen(src);
    assert!(
        code.contains("'maps':'put'('total'"),
        "Immediately-invoked block (value: form) with a field mutation must thread state via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_block_returned_from_method_with_field_mutation_compiles_against_the_cell() {
    // `^[self.total := self.total + 1]` — a block *returned as a value*
    // (never invoked in this method) is not caught by any of the semantic-analysis
    // passes that guard field mutations in blocks (they only flag blocks that are
    // stored to a variable or passed as a literal argument to an unsafe message
    // send — see block_analyzer.rs's BlockContext::Stored check and
    // class_validators.rs's check). It still reaches generate_block's
    // generic fallback and must be rejected there.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  makeBlock =>\n    ^[self.total := self.total + 1]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    let code = result.expect("a field-writing block compiles (ADR 0137)");
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the actor's field through the cell. Got:\n{code}"
    );
}

/// BT-3806 (ADR 0131 §6 item 4): an actor block that writes a field and
/// escapes the method without a send is accepted by the §6 check (a field
/// has a home that outlives the method frame), and rejected here instead, in
/// every escape shape, not only `^[...]`: run, it would write the field of a
/// state map no actor sees. Probed on a real build: every one of these fails
/// to compile with this error. (A block stored in a field and run by a later
/// call of the same actor, `self.h := [self.n := self.n + 1]`, is the shape
/// that works: `adr0131section6escape_test.bt` `testActorStoredFieldWriter`.)
#[test]
fn test_escaping_field_writing_actor_block_compiles_against_the_cell_in_every_shape() {
    for body in [
        "^[self.total := self.total + 1]",
        "[self.total := self.total + 1]",
        "^[:x | self.total := x]",
        "#[[self.total := self.total + 1]]",
        "c ifTrue: [[self.total := self.total + 1]] ifFalse: [[0]]",
        "b := [self.total := self.total + 1]\n    ^b",
    ] {
        let src =
            format!("Actor subclass: Ctr\n  state: total = 0\n\n  makeBlock: c =>\n    {body}\n");
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let result = crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test"),
        );
        let code = result.unwrap_or_else(|e| panic!("{body}: {e:?}"));
        assert!(
            code.contains("call 'beamtalk_actor_state':'put'("),
            "{body}: the escaping block writes the actor's field through the cell. Got:\n{code}"
        );
    }
}

#[test]
fn test_block_with_mixed_local_and_field_mutation_stored_then_invoked_compiles() {
    // PR review follow-up: a block with BOTH a captured-local
    // mutation and a field write — e.g. `[:x | outerCount := outerCount + x.
    // self.total := self.total + outerCount]` stored in a var and invoked
    // later — used to bypass the field-write check entirely: captured_mutations
    // is non-empty (from `outerCount`), so generate_block routed straight to
    // Tier 2 for the local mutation, never reaching validate_stored_closure.
    // That produced Core Erlang that *compiled* (passed erlc) but crashed at
    // runtime: the resulting block is a 2-arity stateful fun (params + State),
    // but generate_block_value_call and friends called it with only its
    // declared params (no State argument) — `badarity`. That gap is now
    // closed by making it a compile-time error instead.
    //
    // For exactly this shape, though, a real fix applies instead of the
    // compile-time error: `blk`'s only use in the rest of the method is the
    // `value:` call below, which `prescan_tier2_local_vars` proves is safe,
    // so the block is compiled via `generate_block_stateful` and invoked
    // through the Tier 2 calling convention (`apply Fun(Args, State)`,
    // unpacking the `{Result, NewState}` tuple) — see
    // `test_bt2797_same_method_tier2_local_var_threads_state_correctly` below
    // for a check of the generated Core Erlang shape itself.
    //
    // `outerCount := outerCount + x` is deliberately the block's first use of
    // `outerCount`: block_analysis classifies a name as a *captured* mutation
    // only when it's read before being locally defined *within the block*,
    // and `outerCount + x` on the assignment's right-hand side reads the
    // name before this statement (the block's only mention of it) defines
    // it. Writing the block as `outerCount := 0. outerCount := outerCount +
    // x` instead would make `outerCount` a fresh block-local, not a captured
    // one, and the mixed local+field shape this test targets wouldn't
    // reproduce. The outer method's own `outerCount := 0` — appearing after
    // `blk`'s definition in program order — only needs to exist so the
    // source parses as a valid Beamtalk program; it has no bearing on the
    // capture classification itself.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | outerCount := outerCount + x. self.total := self.total + outerCount]\n    outerCount := 0\n    blk value: item\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    assert!(
        result.is_ok(),
        "A block with both a local and a field mutation, stored then invoked \
         via `value:` later in the same method, must compile now that the \
         compiler can prove the call site threads state correctly (BT-2797). \
         Got: {result:?}"
    );
}

#[test]
fn test_bt2797_same_method_tier2_local_var_threads_state_correctly() {
    // verifies the *shape* of the generated Core Erlang for the
    // scenario above, not just that codegen returns Ok(..). `blk` must be a
    // 2-arity fun taking a trailing state accumulator and returning a
    // `{Result, NewState}` tuple, and the call site must `apply` it with the
    // outer method's State and unpack the tuple — not naively `apply Fun
    // (Arg)` (which would compile but badarity-crash at runtime).
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | outerCount := outerCount + x. self.total := self.total + outerCount]\n    outerCount := 0\n    blk value: item\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile (BT-2797)");

    // Structural checks (not hardcoded fresh-variable counter values, which
    // shift whenever unrelated codegen changes the counter sequence before
    // this point).
    assert!(
        regex::Regex::new(r"let Blk = fun \(_\w+, StateAcc\) ->")
            .unwrap()
            .is_match(&code),
        "blk must compile to a 2-arity Tier 2 fun (block param + trailing \
         state accumulator). Got: {code}"
    );
    assert!(
        regex::Regex::new(r"apply _Fun\w* \(_item\w*, State\)")
            .unwrap()
            .is_match(&code),
        "the `blk value: item` call site must apply the block with the \
         outer method's State as a trailing argument. Got: {code}"
    );
    assert!(
        regex::Regex::new(r"call 'erlang':'element'\(1, _T2Tuple\w*\)")
            .unwrap()
            .is_match(&code)
            && regex::Regex::new(r"call 'erlang':'element'\(2, _T2Tuple\w*\)")
                .unwrap()
                .is_match(&code),
        "the call site must unpack the returned {{Result, NewState}} tuple \
         rather than treating the raw apply result as the method's return \
         value. Got: {code}"
    );
}

#[test]
fn test_bt2808_cascade_on_tier2_local_var_compiles_and_threads_state() {
    // `blk value: item; value: item` — a cascade sending two safe
    // `value:` sends to the SAME Tier 2 local var. Before the fix,
    // `scan_var_uses`'s `Cascade` arm hit the generic `Identifier` arm on the
    // receiver (since the receiver *is* `blk`) and unconditionally reported it
    // unsafe, so `prescan_tier2_local_vars` never promoted `blk` and this hit
    // the `FieldAssignmentInUnsupportedBlock` compile-time diagnostic even
    // though the pattern is exactly as safe as a single `blk value: item`.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | self.total := self.total + x]\n    blk value: item; value: item\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("cascade of safe value: sends on a stored block must compile (BT-2808)");

    // ADR 0137: `blk` writes only a field, so it is an ordinary closure over the
    // actor's state, not a Tier 2 block: it writes the cell, and each of the two
    // cascaded `value:` sends is a call that may run it, bracketed with a store
    // and a reload of the threaded `State`. The cascade is one call: one bracket
    // holds both sends.
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the field through the cell. Got: {code}"
    );
    let stores = code.matches("call 'beamtalk_actor_state':'store'(").count();
    assert!(
        stores >= 1 && code.matches("'send'(_Receiver").count() >= 2,
        "the cascaded value: sends must be bracketed (found {stores} stores). Got: {code}"
    );
}

#[test]
fn test_bt2797_local_field_writing_block_never_invoked_again_compiles() {
    // Regression guard: `blk := [block needing Tier 2]` where `blk` is
    // never referenced again in the rest of the method — here because the
    // assignment is the method's *last* statement, so the raw Tier 2 fun value
    // implicitly escapes as the method's own return value. `prescan_tier2_local_vars`
    // must NOT promote this: an early, buggy version of the safety check asked
    // "is there no *unsafe* use of blk afterward?", which is vacuously true
    // when there's no use at all (`[].iter().all(...)` on an empty slice), so
    // it wrongly promoted variables that are simply never used again. The fix
    // requires proof of at least one *safe* use, not just the absence of an
    // unsafe one. This must keep hitting the compile-time diagnostic instead of
    // producing Core Erlang that returns a raw 2-arity fun to a caller with no
    // idea it needs to thread state through it.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | self.total := self.total + x]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    let code = result.expect("a field-writing block compiles (ADR 0137)");
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the actor's field through the cell. Got:\n{code}"
    );
}

#[test]
fn test_bt2797_local_field_writing_block_invoked_inside_nested_do_block_compiles() {
    // Regression guard: `blk value: item` found
    // only *inside* a nested block literal (here, the `do:` iteration block)
    // must NOT be treated as a safe use, even though it looks identical to a
    // safe top-level `value:` call. A nested block compiles through a
    // completely separate path (`generate_block_body_slice`/`BlockExprKind`,
    // not `lower_body_exprs_with_reply`/`BodyExprKind`) that has no
    // Tier2-tuple-unpacking logic and never resets `tier2_local_vars` for its
    // own body — so wrongly promoting `blk` here would either leak an
    // unpacked `{Result, NewState}` tuple as the inner block's return value,
    // or badarity-crash calling a 2-arity Tier 2 fun with 1 argument.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    blk := [:x | self.total := self.total + x]\n    items do: [:item | blk value: item]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    let code = result.expect("a field-writing block compiles (ADR 0137)");
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the actor's field through the cell. Got:\n{code}"
    );
}

#[test]
fn test_bt2797_local_field_writing_block_invoked_inside_nested_if_true_block_compiles() {
    // Regression guard: same as the `do:` case
    // above, but for a `ifTrue:` control-flow block — the other concrete
    // trigger the reviewer flagged.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    blk := [:x | self.total := self.total + x]\n    n > 0 ifTrue: [blk value: n]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    let code = result.expect("a field-writing block compiles (ADR 0137)");
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the actor's field through the cell. Got:\n{code}"
    );
}

#[test]
fn test_bt2797_field_stored_block_with_captured_local_and_field_write_is_still_compile_error() {
    // PR #2899 review fix: a block stored in a field that mutates
    // BOTH a captured outer local AND a field must still be rejected at
    // compile time, not silently promoted to Tier 2 like the field-writes-only
    // case. `generate_block_stateful`'s captured-local handling reads a
    // `'__local__<var>'` key from the *calling* method's StateAcc, falling
    // back to the value closed over at block-definition time when absent —
    // correct only when the block is invoked from the same method it was
    // defined in. A field-stored block can be invoked from a *different*
    // method (this is deliberately supported), so that fallback would
    // silently return a stale value forever, and the key would then leak
    // into the actor's persistent state once the returned NewState is merged
    // back in. This combination must remain a compile-time error.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: callback = nil\n\n  setup =>\n    count := 0\n    self.callback := [:n | count := count + n. self.total := self.total + count]\n\n  process: n =>\n    self.callback value: n\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::LocalMutationInStoredClosure { .. })
        ),
        "a block stored in a field that also captures and mutates an outer \
         local must still be rejected at compile time — promoting it would \
         leak a '__local__<var>' state key into the actor's persistent state \
         and read a stale definition-time fallback value when invoked from a \
         different method than the one that stored it. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_nonliteral_field_mutating_block_passed_to_self_send_compiles() {
    // A field-mutating block held in a local var and passed as a
    // *non-literal* argument to a self-send — `self applyBlock: blk to: x`,
    // where `blk` was assigned separately. Under ADR 0137 the block is an
    // ordinary closure over the actor-state cell: it compiles, and writes
    // the live field when it runs inside the actor.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  applyBlock: aBlock to: x =>\n    aBlock value: x\n\n  run: x =>\n    blk := [:y | self.total := self.total + y]\n    self applyBlock: blk to: x\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    );
    let code = result.expect("a field-writing block compiles (ADR 0137)");
    assert!(
        code.contains("call 'beamtalk_actor_state':'put'("),
        "the block writes the actor's field through the cell. Got:\n{code}"
    );
}

#[test]
fn test_bt2797_no_regression_pure_block_value_fast_path() {
    // acceptance criterion 7: the zero-cost fast path for a pure
    // (non-mutating) block literal immediately invoked via `value`/`value:`
    // must be untouched — no `is_function` runtime check, no state-threading
    // overhead. A literal
    // block receiver is intercepted earlier and takes the plain
    // generate_block_value_call path regardless.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    [42] value\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile");

    assert!(
        !code.contains("is_function"),
        "A pure block literal invoked via `value` must not emit any \
         is_function runtime check. Got: {code}"
    );
}

#[test]
fn test_bt2797_no_regression_pure_local_var_block_value_fast_path() {
    // a block held in a local var (not a field) that has NO captured
    // or field mutations must also stay on the pre-existing plain
    // `is_function` guard (generate_value_keyword_guard) — never the
    // self.field-scoped runtime-discrimination path,
    // and never the Tier 2 stateful protocol.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | x + 1]\n    blk value: item\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile");

    assert!(
        !code.contains("StateAcc"),
        "A pure block stored in a local var must not be promoted to the \
         Tier 2 stateful protocol. Got: {code}"
    );
}

#[test]
fn test_bt2803_no_regression_pure_block_value_with_arguments_fast_path() {
    // Mirrors the same fast-path regression guard as the field case: a
    // literal block receiver never needs the runtime is_function guard —
    // try_generate_block_value_with_arguments_keyword's literal-block fast
    // path applies Args directly via erlang:apply, no runtime check at all.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    [:x :y | x + y] valueWithArguments: #(3, 4)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile");

    assert!(
        !code.contains("is_function"),
        "A block literal invoked via valueWithArguments: must not emit any \
         is_function runtime check. Got: {code}"
    );
    assert!(
        code.contains("call 'erlang':'apply'("),
        "must still apply the block to the runtime Args list. Got: {code}"
    );
}

#[test]
fn test_bt2803_no_regression_pure_local_var_value_with_arguments_fast_path() {
    // a block held in a local var (not a field, no captured/field
    // mutations) reaches the generic is_function/1 guard
    // (generate_block_value_with_arguments_call) — never the Tier 2 runtime-
    // discriminated path, and never the stateful protocol.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    blk := [:x | x + 1]\n    blk valueWithArguments: #(5)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile");

    assert!(
        !code.contains("StateAcc"),
        "A pure block stored in a local var must not be promoted to the \
         Tier 2 stateful protocol. Got: {code}"
    );
    assert!(
        regex::Regex::new(r"is_function'\(_ValRecv\w*\) of")
            .unwrap()
            .is_match(&code),
        "must use the plain is_function/1 guard on the hoisted receiver \
         (generate_block_value_with_arguments_call), not the runtime-length \
         Tier 1/Tier 2 discrimination. Got: {code}"
    );
    assert!(
        !code.contains("'erlang':'length'("),
        "must not compute a runtime Args length — that's only needed for \
         the Tier 1/Tier 2 discriminated path. Got: {code}"
    );
}

#[test]
fn test_bt2814_local_var_tier2_value_call_in_argument_position_unpacks_result() {
    // a Tier 2 block held in a local var, invoked via `value:` in
    // *argument* (sub-expression) position. Before the fix,
    // `try_generate_block_value_keyword` intercepted this receiver shape
    // (tier2_local_vars) and called `generate_block_value_call_stateful`
    // directly, returning the raw {Result, NewState} tuple straight into the
    // arithmetic — `10 + {Result, NewState}` crashes with badarith at
    // runtime. `close_tier2_value_subexpr_doc` now unpacks element(1) so the
    // arithmetic sees a plain value. Structural check only (see
    // the former stdlib/test/tier2stored_block_matrix_test.bt, removed by BT-3922 for the runtime
    // end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run: x =>\n    r := 0\n    blk := [:n | r := r + n]\n    10 + (blk value: x)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile (BT-2814)");

    // ADR 0116: the right operand (`blk value: x`) is not statically
    // numeric, so this call site let-binds it to `_BinRight<N>` before
    // wrapping the addition in the number-on-the-left coercion try/catch —
    // the underlying unwrap (the block's `{Result, NewState}` tuple is
    // unpacked to a plain value via `element(1, ...)`, never leaked raw into
    // the arithmetic) still holds; it's just bound to a variable ahead of
    // the `try` now instead of inlined directly as `erlang:'+'`'s 2nd arg.
    assert!(
        regex::Regex::new(
            r"let _Synced\w* = let _T2SubTuple\w* = .*apply.*in call 'erlang':'element'\(1, _T2SubTuple\w*\) in .*let _BinRight\w* = _Synced\w* in try call 'erlang':'\+'\("
        )
        .unwrap()
        .is_match(&code),
        "the local-var Tier2 block's value: call in argument position must \
         be let-bound to a self-contained let-chain that extracts element(1) \
         of the returned tuple before the addition, not the raw tuple. \
         Got: {code}"
    );
}

#[test]
fn test_bt2815_named_local_var_captured_mutation_rebinds_after_call() {
    // a block assigned to a LOCAL variable (not a field) whose only
    // mutation is a captured outer local, invoked later via `value:` in the
    // same method. `get_inline_block_captured_mutations` recognizes only an
    // INLINE block literal receiver — a NAMED `tier2_local_vars` identifier
    // receiver needs `prescan_tier2_local_vars` instead, which records the
    // captured-mutation var names keyed by variable name
    // (`tier2_local_var_captured_mutations`) so the call site can rebind
    // them the same way it already does for an inline literal; otherwise
    // the caller's own `outer` variable would silently keep its stale
    // pre-call value even though the call itself succeeded and internally
    // computed the right value. Structural
    // check only (see the former stdlib/test/tier2stored_block_matrix_test.bt, removed by BT-3922 for the
    // runtime end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run =>\n    outer := 0\n    blk := [:n | outer := outer + n]\n    blk value: 5\n    outer\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile (BT-2815)");

    assert!(
        regex::Regex::new(
            r"let State1 = call 'erlang':'element'\(2, _T2Tuple\w*\) in let Outer\w* = call 'maps':'get'\('__local__outer', State1\)"
        )
        .unwrap()
        .is_match(&code),
        "after the named-local-var Tier2 block's value: call, the caller's \
         own `outer` binding must be rebound from '__local__outer' in the \
         call's returned NewState, mirroring the inline-block-literal case. \
         Got: {code}"
    );
}

#[test]
fn test_bt2815_named_local_var_cascade_captured_mutation_rebinds_after_call() {
    // Verify the cascade variant too —
    // `blk value: x; value: x` (cascade codegen) invoked twice
    // must also rebind the caller's `outer` var from the cascade's final
    // NewState, not just the single-send case above.
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run =>\n    outer := 0\n    blk := [:n | outer := outer + n]\n    blk value: 4; value: 4\n    outer\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test"),
    )
    .expect("should compile (BT-2815)");

    assert!(
        regex::Regex::new(
            r"let Outer\w* = call 'maps':'get'\('__local__outer', State\w*\) in let _Result = Outer"
        )
        .unwrap()
        .is_match(&code),
        "after the named-local-var Tier2 block's cascade (value: x; value: \
         x), the caller's own `outer` binding must be rebound from \
         '__local__outer' in the cascade's final NewState. Got: {code}"
    );
}

#[test]
fn test_bt3755_self_send_does_not_promote_a_writing_block() {
    // ADR 0137 Phase 3 (BT-4010): the Tier 2 self-send promotion
    // (`scan_class_for_tier2_blocks` / `detect_tier2_self_send`) is deleted.
    // A block that writes a captured local and is passed to a self-send is
    // an escaping-block compile error (ADR 0133), so no callee shape gets
    // the Tier 2 pack/extract around the send, whatever the callee does
    // with the block. (Codegen tests skip validation, so this shape still
    // compiles here; the validator rejects it.)
    for callee in [
        "sealed ap: b => b value",
        "sealed ap: b => #(1) do: [:x | b value]",
        "ap: b => b value",
    ] {
        let src = format!(
            "Actor subclass: Ctr\n  {callee}\n\n  run =>\n    t := 0\n    self ap: [t := t + 1]\n    t\n"
        );
        let code = codegen(&src);
        assert!(
            !code.contains("'maps':'put'('__local__t', T, State"),
            "`{callee}`: no Tier 2 pack/extract around the self-send. Got:\n{code}"
        );
    }
}
