%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_case_stdlib_tests).

-moduledoc """
Stdlib-layer EUnit tests for beamtalk_test_case error paths (BT-2391).

These tests run under coverage-bunit (which instruments beamtalk_stdlib),
closing the coverage gap left by beamtalk_test_case_tests.erl living in
beamtalk_runtime/test/ (a different instrumentation scope).

Targets uncovered paths: skip/1 atom/other, fail/1 atom/other,
should_raise/2 type errors, execute_tests/5 with real methods,
format_results/2 all branches, and run_test_method/5 failure catch clauses.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% skip/1 — atom and non-binary/non-atom paths (lines 150, 152)
%%% ============================================================================

skip_with_atom_test() ->
    ?assertThrow(
        {bunit_skip, <<"unix_only">>},
        beamtalk_test_case:skip(unix_only)
    ).

skip_with_binary_test() ->
    ?assertThrow(
        {bunit_skip, <<"Windows only">>},
        beamtalk_test_case:skip(<<"Windows only">>)
    ).

skip_with_integer_test() ->
    ?assertThrow(
        {bunit_skip, _},
        beamtalk_test_case:skip(42)
    ).

skip_with_list_test() ->
    ?assertThrow(
        {bunit_skip, _},
        beamtalk_test_case:skip([1, 2, 3])
    ).

%%% ============================================================================
%%% fail/1 — atom and non-binary/non-atom paths (lines 133, 135)
%%% ============================================================================

fail_with_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail(broken)
    ).

fail_with_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail(42)
    ).

fail_with_tuple_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail({some, tuple})
    ).

%%% ============================================================================
%%% should_raise/2 — type-error clauses (lines 99–115)
%%% ============================================================================

should_raise_non_function_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_test_case:should_raise(not_a_fun, some_error)
    ).

should_raise_non_atom_kind_raises_type_error_test() ->
    Block = fun() -> ok end,
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_test_case:should_raise(Block, <<"not_atom">>)
    ).

should_raise_both_wrong_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_test_case:should_raise(42, <<"bad">>)
    ).

%%% ============================================================================
%%% run_test_method/5 via tc_run_helper — failure catch clauses (lines 817–835)
%%%
%%% tc_run_helper exports new/0 + dispatch/3 so run_test_method/5 can exercise
%%% all catch branches without needing a running gen_server.
%%% ============================================================================

run_test_method_pass_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testPass, #{}, nil
    ),
    ?assertEqual({pass, testPass}, Result).

run_test_method_error_test() ->
    %% Hits: error:TestReason:TestST -> {fail, MethodName, ...}  (line 830–832)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testError, #{}, nil
    ),
    ?assertMatch({fail, testError, _}, Result).

run_test_method_throw_test() ->
    %% Hits: TestClass:TestReason:TestST  catch-all (line 833–835)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testThrow, #{}, nil
    ),
    ?assertMatch({fail, testThrow, _}, Result).

run_test_method_exit_test() ->
    %% Hits: TestClass:TestReason:TestST  catch-all with exit class
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testExit, #{}, nil
    ),
    ?assertMatch({fail, testExit, _}, Result).

run_test_method_skip_test() ->
    %% Hits: throw:{bunit_skip, Reason} -> {skip, MethodName, Reason}
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testSkip, #{}, nil
    ),
    ?assertEqual({skip, testSkip, <<"skip reason">>}, Result).

run_test_method_assert_fail_test() ->
    %% Hits: error:#beamtalk_error{kind = assertion_failed, ...} (line 816–819)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testAssertFail, #{}, nil
    ),
    ?assertMatch({fail, testAssertFail, _}, Result).

run_test_method_beamtalk_error_test() ->
    %% Hits: error:#beamtalk_error{message = ErrMsg} catch-all (line 820–823)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBeamtalkError, #{}, nil
    ),
    ?assertMatch({fail, testBeamtalkError, _}, Result).

run_test_method_undef_test() ->
    %% Hits: error:undef:TestST -> beamtalk_exception_handler:ensure_wrapped (line 824–829)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testUndef, #{}, nil
    ),
    ?assertMatch({fail, testUndef, _}, Result).

%%% ============================================================================
%%% execute_tests/5 — runAll with methods (lines 163–187)
%%% ============================================================================

execute_tests_runall_pass_test() ->
    %% FlatMethods values are ignored — only keys drive lifecycle detection.
    %% tc_run_helper:dispatch/3 is called directly by run_test_method.
    FlatMethods = #{
        testPass => {'FakeTest', {}},
        setUp => {'FakeTest', {}},
        tearDown => {'FakeTest', {}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', tc_run_helper, FlatMethods),
    ?assert(is_binary(Result)),
    %% Should mention "tests" and "passed"
    ?assertNotEqual(nomatch, binary:match(Result, <<"passed">>)).

execute_tests_runall_with_failure_test() ->
    %% mix: testPass passes, testError fails — exercises the fail branch of format_results
    FlatMethods = #{
        testPass => {'FakeTest', {}},
        testError => {'FakeTest', {}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', tc_run_helper, FlatMethods),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"failed">>)).

execute_tests_runall_with_skip_test() ->
    %% testSkip skips, testPass passes — exercises the skip branch of format_results
    FlatMethods = #{
        testSkip => {'FakeTest', {}},
        testPass => {'FakeTest', {}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', tc_run_helper, FlatMethods),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"skipped">>)).

execute_tests_runall_skip_and_fail_test() ->
    %% testSkip + testError — exercises the mixed (skip + fail) branch of format_results
    FlatMethods = #{
        testSkip => {'FakeTest', {}},
        testError => {'FakeTest', {}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', tc_run_helper, FlatMethods),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"skipped">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"failed">>)).

%%% ============================================================================
%%% execute_tests/5 — 'run:' path (lines 190–210)
%%% ============================================================================

execute_tests_run_single_pass_test() ->
    FlatMethods = #{testPass => {'FakeTest', {}}},
    Result = beamtalk_test_case:execute_tests(
        'run:', [testPass], 'FakeTest', tc_run_helper, FlatMethods
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"passed">>)).

execute_tests_run_single_fail_test() ->
    FlatMethods = #{testError => {'FakeTest', {}}},
    Result = beamtalk_test_case:execute_tests(
        'run:', [testError], 'FakeTest', tc_run_helper, FlatMethods
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"failed">>)).

execute_tests_run_single_not_found_test() ->
    Result = beamtalk_test_case:execute_tests(
        'run:', [testMissing], 'FakeTest', tc_run_helper, #{}
    ),
    ?assertNotEqual(nomatch, binary:match(Result, <<"not found">>)).

%%% ============================================================================
%%% execute_tests/5 — runAll with no test methods (line 166)
%%% ============================================================================

execute_tests_runall_no_methods_test() ->
    %% FlatMethods has no "test*" keys → discover_test_methods returns [] → line 166
    Result = beamtalk_test_case:execute_tests(
        runAll, [], 'FakeTest', tc_run_helper, #{setUp => {'FakeTest', {}}}
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"No test methods">>)).

%%% ============================================================================
%%% decode_beam_error/1 clause coverage via run_test_method/5 (BT-2404)
%%%
%%% decode_beam_error is private but reachable through run_test_method when
%%% errors fall through to the generic `error:TestReason:TestST` catch clause
%%% (line 830 of beamtalk_test_case.erl). All dispatchers below raise error
%%% shapes that are NOT #beamtalk_error{} records and NOT `undef`, so they
%%% bypass the specialised catch clauses and hit format_test_error/3.
%%% ============================================================================

decode_error_future_rejected_test() ->
    %% {future_rejected, Inner} → "future rejected: ..."
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testFutureRejected, #{}, nil
    ),
    ?assertMatch({fail, testFutureRejected, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"future rejected">>)).

decode_error_error_map_with_key_test() ->
    %% {error, Map} with error key present → recurses into inner value
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testErrorMapWithKey, #{}, nil
    ),
    ?assertMatch({fail, testErrorMapWithKey, _}, Result).

decode_error_error_map_no_key_test() ->
    %% {error, Map} with no error key → print_string(Map)
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testErrorMapNoKey, #{}, nil
    ),
    ?assertMatch({fail, testErrorMapNoKey, _}, Result).

decode_error_exception_map_test() ->
    %% #{class := 'Exception', error := #beamtalk_error{message}} → extracts message
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testExceptionMap, #{}, nil
    ),
    ?assertMatch({fail, testExceptionMap, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"exc msg">>)).

decode_error_class_map_test() ->
    %% #{'$beamtalk_class' := _, error := #beamtalk_error{message}} → extracts message
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testClassMap, #{}, nil
    ),
    ?assertMatch({fail, testClassMap, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"cls msg">>)).

decode_error_badkey_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadKey, #{}, nil
    ),
    ?assertMatch({fail, testBadKey, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"not found in dictionary">>)).

decode_error_badmap_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadMap, #{}, nil
    ),
    ?assertMatch({fail, testBadMap, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"is not a map">>)).

decode_error_badmatch_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadMatch, #{}, nil
    ),
    ?assertMatch({fail, testBadMatch, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no match of right-hand side">>)).

decode_error_case_clause_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testCaseClause, #{}, nil
    ),
    ?assertMatch({fail, testCaseClause, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no case clause matched">>)).

decode_error_try_clause_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testTryClause, #{}, nil
    ),
    ?assertMatch({fail, testTryClause, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no try clause matched">>)).

decode_error_function_clause_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testFunctionClause, #{}, nil
    ),
    ?assertMatch({fail, testFunctionClause, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no matching function clause">>)).

decode_error_if_clause_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testIfClause, #{}, nil
    ),
    ?assertMatch({fail, testIfClause, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no true branch">>)).

decode_error_badarity_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadArity, #{}, nil
    ),
    ?assertMatch({fail, testBadArity, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"function expects">>)).

decode_error_badfun_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadFun, #{}, nil
    ),
    ?assertMatch({fail, testBadFun, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"is not a function">>)).

decode_error_badarg_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadArg, #{}, nil
    ),
    ?assertMatch({fail, testBadArg, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"bad argument">>)).

decode_error_badarith_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testBadArith, #{}, nil
    ),
    ?assertMatch({fail, testBadArith, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"bad arithmetic">>)).

decode_error_noproc_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testNoproc, #{}, nil
    ),
    ?assertMatch({fail, testNoproc, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"no such process">>)).

decode_error_timeout_test() ->
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_run_helper, testTimeout, #{}, nil
    ),
    ?assertMatch({fail, testTimeout, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"timed out">>)).

%%% ============================================================================
%%% run_all/1, run_single/2, run_all_structured/1, run_single_structured/2
%%% BIF-fallback paths (lines 221–360) — require bt@tc_stub on code path
%%% ============================================================================

run_all_with_tests_test() ->
    %% resolve_module('TcStub') → 'bt@tc_stub'; discover finds testStubPass + testStubFail
    Result = beamtalk_test_case:run_all('TcStub'),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"failed">>)).

run_all_no_test_methods_test() ->
    %% resolve_module('EmptyStub') → 'bt@empty_stub'; no test* exports → "No test methods"
    Result = beamtalk_test_case:run_all('EmptyStub'),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"No test methods">>)).

run_single_pass_test() ->
    Result = beamtalk_test_case:run_single('TcStub', testStubPass),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"passed">>)).

run_single_fail_test() ->
    Result = beamtalk_test_case:run_single('TcStub', testStubFail),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"failed">>)).

run_all_structured_no_test_methods_test() ->
    %% 'bt@empty_stub' has no test* exports → no-test-methods branch (lines 295–307)
    Result = beamtalk_test_case:run_all_structured('EmptyStub'),
    ?assert(is_map(Result)),
    ?assertEqual('EmptyStub', maps:get(class, Result)),
    ?assertEqual(0, maps:get(total, Result)),
    ?assertEqual(0, maps:get(passed, Result)),
    ?assertEqual([], maps:get(tests, Result)).

run_all_structured_with_tests_test() ->
    Result = beamtalk_test_case:run_all_structured('TcStub'),
    ?assert(is_map(Result)),
    ?assertEqual('TcStub', maps:get(class, Result)),
    ?assert(is_integer(maps:get(total, Result))),
    ?assert(maps:get(total, Result) > 0),
    ?assert(is_list(maps:get(tests, Result))).

run_single_structured_pass_test() ->
    Result = beamtalk_test_case:run_single_structured('TcStub', testStubPass),
    ?assert(is_map(Result)),
    ?assertEqual('TcStub', maps:get(class, Result)),
    ?assertEqual(1, maps:get(total, Result)),
    ?assertEqual(1, maps:get(passed, Result)),
    ?assertEqual(0, maps:get(failed, Result)).

run_single_structured_fail_test() ->
    Result = beamtalk_test_case:run_single_structured('TcStub', testStubFail),
    ?assert(is_map(Result)),
    ?assertEqual(1, maps:get(total, Result)),
    ?assertEqual(0, maps:get(passed, Result)),
    ?assertEqual(1, maps:get(failed, Result)).

%%% ============================================================================
%%% extract_error_kind/1 clauses via should_raise/2
%%%
%%% extract_error_kind/1 is private but reachable via the public should_raise/2
%%% API: Block raises an exception; should_raise calls extract_error_kind on the
%%% exception value to get the kind to compare against ExpectedKind.
%%%
%%% Covers lines 652, 655, 657, 660, 669 of beamtalk_test_case.erl.
%%% ============================================================================

%% Line 657: #beamtalk_error{kind = Kind} direct record match
should_raise_matches_beamtalk_error_record_test() ->
    Block = fun() ->
        error(#beamtalk_error{kind = my_kind, class = 'TestCase', message = <<"m">>})
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, my_kind)).

%% Line 660: #{class := 'Exception', error := #beamtalk_error{}} map match
should_raise_matches_exception_map_test() ->
    Block = fun() ->
        error(#{
            class => 'Exception',
            '$beamtalk_class' => 'Exception',
            error => #beamtalk_error{kind = my_kind, class = 'TestCase', message = <<"m">>}
        })
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, my_kind)).

%% Line 652: {future_rejected, Reason} → recursive delegation → hits line 657
should_raise_matches_future_rejected_test() ->
    Block = fun() ->
        error(
            {future_rejected, #beamtalk_error{
                kind = my_kind, class = 'TestCase', message = <<"m">>
            }}
        )
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, my_kind)).

%% Line 655: {error, Map} when is_map(Map) → recursive → hits $beamtalk_class clause
should_raise_matches_error_map_wrapped_test() ->
    Block = fun() ->
        error(
            {error, #{
                '$beamtalk_class' => 'RuntimeError',
                error => #beamtalk_error{kind = my_kind, class = 'TestCase', message = <<"m">>}
            }}
        )
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, my_kind)).

%% Line 669: unknown error shape → catch-all returns atom 'error'
should_raise_unknown_error_shape_test() ->
    Block = fun() -> error({some_weird_tuple, not_recognized}) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, error)).

%%% ============================================================================
%%% format_stacktrace/1 edge cases (BT-1743)
%%%
%%% format_stacktrace/1 is exported under -ifdef(TEST).
%%% Covers lines 440, 537–538, 556, 573 of beamtalk_test_case.erl.
%%% ============================================================================

%% Line 440: empty list → <<>>
format_stacktrace_empty_test() ->
    ?assertEqual(<<>>, beamtalk_test_case:format_stacktrace([])).

%% Lines 537–538: anonymous fun name stripping ('-fun_name/N-fun-M-' → 'fun_name')
format_stacktrace_anon_fun_name_test() ->
    Frame = {'bt@my_class', '-my_fun/0-fun-0-', 0, [{file, "my_class.erl"}, {line, 42}]},
    Result = beamtalk_test_case:format_stacktrace([Frame]),
    ?assertNotEqual(nomatch, binary:match(Result, <<"my_fun">>)).

%% Line 556: malformed frame (not a 4-tuple) filtered out → <<>>
format_stacktrace_malformed_frame_test() ->
    Result = beamtalk_test_case:format_stacktrace([bad_frame]),
    ?assertEqual(<<>>, Result).

%% Line 573: >3 bt@ frames → select_frames returns [first, second, last]
format_stacktrace_more_than_3_frames_test() ->
    Frames = [
        {'bt@a', f, 0, [{file, "a.erl"}, {line, 1}]},
        {'bt@b', f, 0, [{file, "b.erl"}, {line, 2}]},
        {'bt@c', f, 0, [{file, "c.erl"}, {line, 3}]},
        {'bt@d', f, 0, [{file, "d.erl"}, {line, 4}]}
    ],
    Result = beamtalk_test_case:format_stacktrace(Frames),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<>>, Result).

%%% ============================================================================
%%% setUp failure paths in run_test_method/5 outer catch block
%%%
%%% The outer try in run_test_method/5 wraps Module:new() and the setUp
%%% dispatch. Exceptions from setUp bypass the inner catch and land in the
%%% outer catch clauses at lines 850–871 of beamtalk_test_case.erl.
%%% ============================================================================

%% Line 853: setUp throws {bunit_skip, Reason} → {skip, MethodName, Reason}
setup_throws_bunit_skip_test() ->
    FlatMethods = #{setUp => {'FakeTest', {}}, testPass => {'FakeTest', {}}},
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_setup_skip_helper, testPass, FlatMethods, nil
    ),
    ?assertEqual({skip, testPass, <<"skipping in setUp">>}, Result).

%% Lines 868–871: setUp raises generic Erlang error → {fail, MethodName, "setUp failed: ..."}
setup_raises_generic_error_test() ->
    FlatMethods = #{setUp => {'FakeTest', {}}, testPass => {'FakeTest', {}}},
    Result = beamtalk_test_case:run_test_method(
        'FakeTest', tc_setup_fail_helper, testPass, FlatMethods, nil
    ),
    ?assertMatch({fail, testPass, Msg} when is_binary(Msg), Result),
    {fail, _, Msg} = Result,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"setUp failed">>)).
