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
fn test_block_returned_from_method_with_field_mutation_is_compile_error() {
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A field-mutating block returned as a value must be a compile-time error \
         (BT-2792), not silently accepted. Got: {result:?}"
    );
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("cascade of safe value: sends on a Tier 2 local var must compile (BT-2808)");

    // Both cascaded `value:` sends must apply the block with a threaded state
    // argument, and each must unpack its own {Result, NewState} tuple — not
    // just the first message (which would silently drop the second mutation).
    let apply_count = regex::Regex::new(r"apply _Fun\w* \(_item\w*, \w*State\w*\)")
        .unwrap()
        .find_iter(&code)
        .count();
    assert_eq!(
        apply_count, 2,
        "both cascaded value: sends must apply the block with a threaded \
         state argument. Got: {code}"
    );
    let element1_count = regex::Regex::new(r"call 'erlang':'element'\(1, _\w+\)")
        .unwrap()
        .find_iter(&code)
        .count();
    let element2_count = regex::Regex::new(r"call 'erlang':'element'\(2, _\w+\)")
        .unwrap()
        .find_iter(&code)
        .count();
    assert!(
        element1_count >= 2 && element2_count >= 2,
        "each cascaded value: send's {{Result, NewState}} tuple must be \
         unpacked separately so both mutations thread through. Got: {code}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_never_invoked_again_is_still_compile_error() {
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 block stored in a local var and never invoked again (so it \
         escapes as the method's implicit return value) must remain a \
         compile-time error. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_invoked_inside_nested_do_block_is_still_compile_error() {
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 local block invoked only from inside a nested `do:` block \
         must remain a compile-time error, not silently-broken Core Erlang. \
         Got: {result:?}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_invoked_inside_nested_if_true_block_is_still_compile_error() {
    // Regression guard: same as the `do:` case
    // above, but for a `ifTrue:` control-flow block — the other concrete
    // trigger the reviewer flagged.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    blk := [:x | self.total := self.total + x]\n    n > 0 ifTrue: [blk value: n]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 local block invoked only from inside a nested `ifTrue:` \
         block must remain a compile-time error, not a silent state-mutation \
         loss at runtime. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_field_stored_block_invoked_from_different_method_threads_state_correctly() {
    // the main real-world motivator — a block with field mutations,
    // assigned to an instance field in one method (`setup`) and invoked via
    // `value:` from a *different* method (`tick:`). Static per-method tracking
    // (tier2_block_params / tier2_local_vars) can't see across methods, so this
    // relies on:
    // 1. `generate_field_assignment_value_doc` (dispatch_codegen.rs) promoting
    //    the stored block to Tier 2 unconditionally (safe because every
    //    `self.field value(:...)` call site now runtime-discriminates), and
    // 2. `generate_block_value_call_runtime_discriminated` (intrinsics.rs)
    //    checking the field's *runtime* arity (`is_function/2`) at the call
    //    site to decide whether to thread state — the same precedent already
    //    used for Erlang FFI interop, generalized to Beamtalk-level block calls.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup =>\n    self.onTick := [:x | self.total := self.total + x]\n\n  tick: x =>\n    self.onTick value: x\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2797)");

    // Structural checks (not hardcoded fresh-variable counter values).
    assert!(
        regex::Regex::new(r"fun \(_\w+, StateAcc\) ->")
            .unwrap()
            .is_match(&code)
            && code.contains("'maps':'put'('onTick',"),
        "setup must store a 2-arity Tier 2 fun (block param + trailing state \
         accumulator) into the onTick field. Got: {code}"
    );
    assert!(
        code.contains("'maps':'get'('onTick', State)"),
        "tick: must read the block back out of the onTick field. Got: {code}"
    );
    // The Tier 1 and Tier 2 arity checks, and the Tier 2 apply, must all
    // reference the *same* captured block variable — tie them together via
    // the name captured from the arity-1 check rather than three independent
    // (and therefore looser) pattern matches.
    let arity_check_re = regex::Regex::new(r"is_function'\((_Fun\w*), 1\)").unwrap();
    let fun_var = &arity_check_re.captures(&code).unwrap_or_else(|| {
        panic!(
            "tick: must runtime-discriminate Tier 1 (arity 1: just the block \
                 param) before applying. Got: {code}"
        )
    })[1];
    assert!(
        code.contains(&format!("is_function'({fun_var}, 2)")),
        "tick: must also check Tier 2 arity (block param + state) for the \
         same block variable ({fun_var}). Got: {code}"
    );
    assert!(
        regex::Regex::new(&format!(r"apply {fun_var} \(_\w+, State\)"))
            .unwrap()
            .is_match(&code),
        "the Tier 2 branch must apply the field's block ({fun_var}) with the \
         calling method's State as a trailing argument. Got: {code}"
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "a block stored in a field that also captures and mutates an outer \
         local must still be rejected at compile time — promoting it would \
         leak a '__local__<var>' state key into the actor's persistent state \
         and read a stale definition-time fallback value when invoked from a \
         different method than the one that stored it. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_nonliteral_field_mutating_block_passed_to_self_send_is_compile_error() {
    // verification, acceptance criterion 5: a field-mutating block
    // held in a local var and passed as a *non-literal* argument to a
    // self-send — `self applyBlock: blk to: x`, where `blk` was assigned
    // separately — is a case `scan_class_for_tier2_blocks`
    // (dispatch_codegen.rs) can't see: it only recognizes a *literal* block
    // at the call site to promote the callee's parameter into
    // `tier2_method_info`/`tier2_block_params`. If this compiled anyway with
    // `aBlock value: x` inside `applyBlock:to:` naively applying with no
    // state, it would badarity-crash at runtime whenever `blk` is actually a
    // Tier 2 fun.
    //
    // Confirms this is instead a compile-time error: `prescan_tier2_local_vars`
    // only promotes `blk` when every later use is a *safe* value/value: call —
    // here `blk` is passed as an *argument* to `applyBlock:to:`, not a value:
    // receiver, so prescan correctly leaves it unpromoted and it falls through
    // to `generate_block`'s existing `FieldAssignmentInUnsupportedBlock` gate
    // — a safe compile-time failure, not a silent runtime crash.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  applyBlock: aBlock to: x =>\n    aBlock value: x\n\n  run: x =>\n    blk := [:y | self.total := self.total + y]\n    self applyBlock: blk to: x\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A field-mutating block passed as a non-literal argument to a \
         self-send must be a compile-time error, not code that compiles but \
         risks badarity at runtime. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_no_regression_pure_block_value_fast_path() {
    // acceptance criterion 7: the zero-cost fast path for a pure
    // (non-mutating) block literal immediately invoked via `value`/`value:`
    // must be untouched — no `is_function` runtime check, no state-threading
    // overhead. The runtime-discrimination codegen
    // (generate_block_value_call_runtime_discriminated) is deliberately
    // scoped to `self.field value(:...)` receivers only (see
    // try_generate_block_value_unary/keyword in intrinsics.rs) — a literal
    // block receiver is intercepted earlier and takes the plain
    // generate_block_value_call path regardless.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    [42] value\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile");

    assert!(
        !code.contains("StateAcc"),
        "A pure block stored in a local var must not be promoted to the \
         Tier 2 stateful protocol. Got: {code}"
    );
}

#[test]
fn test_bt2803_field_stored_block_invoked_via_value_with_arguments_threads_state_correctly() {
    // valueWithArguments: on a self.field receiver needs the same
    // runtime Tier 1/Tier 2 discrimination as `value:`, but the
    // argument count is a runtime list length instead of a compile-time-known
    // static arity —
    // generate_block_value_with_arguments_call_runtime_discriminated
    // generalizes generate_block_value_call_runtime_discriminated for this.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup =>\n    self.onTick := [:x | self.total := self.total + x]\n\n  tick =>\n    self.onTick valueWithArguments: #(5)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2803)");

    assert!(
        code.contains("'maps':'get'('onTick', State)"),
        "tick must read the block back out of the onTick field. Got: {code}"
    );

    // Tie the length computation, both arity checks, and both applies
    // together via the captured fun/args/length variable names rather than
    // three independent (and therefore looser) pattern matches.
    let len_re =
        regex::Regex::new(r"let (_Fun\w*) = .*'onTick', State\) in let (_Args\w*) = \[5\] in let (_ArgsLen\w*) = call 'erlang':'length'\((_Args\w*)\) in let (_ArgsLenPlusOne\w*) = call 'erlang':'\+'\((_ArgsLen\w*), 1\)")
            .unwrap();
    let caps = len_re.captures(&code).unwrap_or_else(|| {
        panic!(
            "tick must bind the field's block, hoist Args to a runtime list, \
             and compute both length(Args) and length(Args) + 1 before \
             discriminating. Got: {code}"
        )
    });
    let fun_var = &caps[1];
    let args_var = &caps[2];
    let tier1_len_var = &caps[3];
    let tier2_len_var = &caps[5];

    assert!(
        code.contains(&format!("is_function'({fun_var}, {tier1_len_var})")),
        "tick must runtime-discriminate Tier 1 arity (length(Args)) for the \
         field's block ({fun_var}). Got: {code}"
    );
    assert!(
        code.contains(&format!("is_function'({fun_var}, {tier2_len_var})")),
        "tick must also check the Tier 2 arity (length(Args) + 1) for the \
         same block variable ({fun_var}). Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "{{call 'erlang':'apply'({fun_var}, {args_var}), State}}"
        )),
        "the Tier 1 branch must apply the field's block ({fun_var}) with the \
         plain Args list, leaving State unchanged. Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "call 'erlang':'apply'({fun_var}, call 'erlang':'++'({args_var}, [State]))"
        )),
        "the Tier 2 branch must apply the field's block ({fun_var}) with the \
         calling method's State appended to the Args list. Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "{{call 'beamtalk_message_dispatch':'send'({fun_var}, 'valueWithArguments:', [{args_var}]), State}}"
        )),
        "the non-function fallback must dispatch valueWithArguments: through \
         beamtalk_message_dispatch:send (BT-3377), not beamtalk_primitive:send \
         — the field may hold a live actor whose gen_server reply only \
         beamtalk_actor:sync_send knows how to unwrap. State must stay \
         unchanged. Got: {code}"
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
fn test_bt2813_bare_tier2_value_call_inside_do_loop_body_unpacks_tuple() {
    // A bare (non-assigned) `self.field value:` statement inside a
    // `do:` loop body. The outer loop is correctly routed
    // into the state-threading (StateAcc) path by `block_needs_mutation_threading`'s
    // `has_field_value_call` fact, but the loop body's own
    // statement codegen (`lower_foldl_body`) needs its own case for
    // a bare Tier2ValueCall — falling through to `lower_non_assign_expr`
    // would emit a plain (Tier-1-only) apply and crash with badarity for
    // a genuinely Tier 2 (2-arity) field-stored block. Structural check only
    // (see stdlib/test/tier2stored_block_matrix_test.bt for the runtime
    // end-to-end check).
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  tickEach: items =>\n    items do: [:x | self.onTick value: x]\n    self.total\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2813)");

    assert!(
        regex::Regex::new(r"let _T2LoopTuple\w* = .*is_function.*in let StateAcc\w* = call 'erlang':'element'\(2, _T2LoopTuple")
            .unwrap()
            .is_match(&code),
        "the bare Tier2ValueCall statement inside the do: loop body must \
         runtime-discriminate the field's block and unpack the returned \
         {{Result, NewState}} tuple via element/2, threading the new state \
         forward into the next fold iteration. Got: {code}"
    );
}

#[test]
fn test_bt2813_bare_tier2_value_call_inside_collect_block_unpacks_tuple() {
    // same gap as the do: case above, but for collect: — the loop
    // body must also extract element(1) of the tuple as the collected value.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  tickEachCollect: items =>\n    items collect: [:x | self.onTick value: x]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2813)");

    assert!(
        regex::Regex::new(r"let _T2LoopVal\w* = call 'erlang':'element'\(1, _T2LoopTuple\w*\) in \{\[_T2LoopVal\w* \| AccList\]")
            .unwrap()
            .is_match(&code),
        "the bare Tier2ValueCall statement inside the collect: loop body \
         must extract element(1) of the returned tuple as the collected \
         item value. Got: {code}"
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
    // stdlib/test/tier2stored_block_matrix_test.bt for the runtime
    // end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run: x =>\n    r := 0\n    blk := [:n | r := r + n]\n    10 + (blk value: x)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
            r"let _BinRight\w* = let _T2SubTuple\w* = .*apply.*in call 'erlang':'element'\(1, _T2SubTuple\w*\) in try call 'erlang':'\+'\("
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
fn test_bt2814_field_stored_tier2_value_call_in_argument_position_unpacks_result() {
    // the self.field variant of the same gap. Before the fix,
    // `try_generate_block_value_keyword`/`_unary` deliberately did NOT
    // intercept a self.field receiver in sub-expression position at all,
    // falling back to a Tier-1-only (arity-N, no State) apply — badarity for
    // a genuinely Tier 2 block. `close_tier2_value_subexpr_doc` now
    // intercepts and unpacks it, consistent with the local-var case above.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  addTickResult: x =>\n    self.total := self.total + (self.onTick value: x)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2814)");

    assert!(
        regex::Regex::new(
            r"let _T2SubTuple\w* = .*is_function.*in call 'erlang':'element'\(1, _T2SubTuple"
        )
        .unwrap()
        .is_match(&code),
        "the field-stored Tier2 block's value: call in argument position \
         must runtime-discriminate the field's block and extract element(1) \
         of the returned tuple, not leak the raw tuple or fall back to a \
         Tier-1-only apply. Got: {code}"
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
    // check only (see stdlib/test/tier2stored_block_matrix_test.bt for the
    // runtime end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run =>\n    outer := 0\n    blk := [:n | outer := outer + n]\n    blk value: 5\n    outer\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
        crate::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
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
fn test_local_var_assignment_with_tier2_block_value_call_inside_conditional_branch() {
    // ADR 0111 Addendum 5 C3: `lower_local_var_assignment_bind`'s Tier 2
    // sub-branch — `result := aBlock value` where `aBlock` is a Tier 2 block
    // parameter, inside an `ifTrue:` branch that also has field mutations.
    //
    // `scan_class_for_tier2_blocks` sees `self applyWith:ifTrue:` called from
    // `runExample` with a literal block that has field writes, so `aBlock`
    // (position 0) is registered as Tier 2. Inside the `ifTrue:` branch,
    // `result := aBlock value` hits `is_tier2_value_call` → the Gensym two-hop
    // extraction path (element(1,...) for value, element(2,...) for new state).
    let src = concat!(
        "Actor subclass: Applier\n",
        "  state: count = 0\n\n",
        "  applyWith: aBlock ifTrue: flag =>\n",
        "    flag ifTrue: [\n",
        "      result := aBlock value.\n",
        "      self.count := self.count + result\n",
        "    ].\n",
        "    self.count\n\n",
        "  runExample =>\n",
        "    self applyWith: [self.count := self.count + 10] ifTrue: true\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("applier_tier2_in_cond").with_workspace_mode(true),
    )
    .expect("Tier 2 block value call inside ifTrue: branch with field mutation must compile");

    eprintln!("Generated code for Tier 2 local-var assignment inside conditional:\n{code}");

    // Tier 2 two-hop extraction: element(1, ...) for value, element(2, ...) for new state.
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Tier 2 path must extract the value from the {{Result, NewState}} tuple \
         via erlang:element(1, ...). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Tier 2 path must extract the new state from the {{Result, NewState}} tuple \
         via erlang:element(2, ...). Got:\n{code}"
    );

    // Field mutation `self.count := self.count + result` inside the branch.
    assert!(
        code.contains("maps':'put'('count'"),
        "Field mutation inside the true branch must produce maps:put('count', ...). \
         Got:\n{code}"
    );

    // The whole conditional must be inlined as a case expression.
    assert!(
        code.contains("case "),
        "ifTrue: with field mutation must compile to an inline case expression. \
         Got:\n{code}"
    );
}
