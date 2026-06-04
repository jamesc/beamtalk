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
