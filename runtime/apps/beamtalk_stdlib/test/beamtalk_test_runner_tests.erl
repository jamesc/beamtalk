%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

%%% @doc EUnit tests for beamtalk_test_runner module.
%%%
%%% Covers path_suffix_match/2 which is the core logic for the `file`
%%% parameter of the `test` MCP tool (BT-1234).

-module(beamtalk_test_runner_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% path_suffix_match/2 tests
%%% ============================================================================

path_suffix_exact_match_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"/absolute/path/test/foo_test.bt">>
        )
    ).

path_suffix_relative_suffix_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"test/foo_test.bt">>
        )
    ).

path_suffix_filename_only_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"foo_test.bt">>
        )
    ).

path_suffix_no_match_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"bar_test.bt">>
        )
    ).

path_suffix_partial_component_no_match_test() ->
    %% "oo_test.bt" must NOT match — it is not a path-boundary-aligned suffix.
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"oo_test.bt">>
        )
    ).

path_suffix_longer_than_stored_no_match_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"test/foo.bt">>,
            <<"absolute/path/test/foo.bt">>
        )
    ).

path_suffix_empty_stored_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"">>,
            <<"foo.bt">>
        )
    ).

path_suffix_both_empty_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"">>,
            <<"">>
        )
    ).

path_suffix_windows_backslash_stored_test() ->
    %% Stored path uses Windows-style backslashes (from std::fs::canonicalize);
    %% suffix uses forward slashes as passed by the MCP caller.
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"test/foo_test.bt">>
        )
    ).

path_suffix_windows_exact_match_test() ->
    %% Both stored and suffix use Windows-style backslashes.
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"C:\\Users\\project\\test\\foo_test.bt">>
        )
    ).

path_suffix_windows_boundary_no_partial_match_test() ->
    %% "oo_test.bt" must NOT match even against a Windows path.
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"oo_test.bt">>
        )
    ).

%%% ============================================================================
%%% result_print_string/1 tests (BT-1669)
%%% ============================================================================

print_string_all_pass_test() ->
    Result = make_result(2, 2, 0, 0, 1.5, [
        #{name => testA, class => 'MyTest', status => pass},
        #{name => testB, class => 'MyTest', status => pass}
    ]),
    Str = beamtalk_test_runner:result_print_string(Result),
    %% No failure details, just the summary line
    ?assertEqual(<<"TestResult(2 tests, 2 passed (1.5s))">>, Str).

print_string_with_failures_test() ->
    Result = make_result(3, 1, 2, 0, 2.0, [
        #{name => testA, class => 'MyTest', status => pass},
        #{
            name => testB,
            class => 'MyTest',
            status => fail,
            error => <<"Expected 4, got 6 (my_test.bt:42)">>
        },
        #{
            name => testC,
            class => 'OtherTest',
            status => fail,
            error => <<"Assertion failed (other_test.bt:10)">>
        }
    ]),
    Str = beamtalk_test_runner:result_print_string(Result),
    %% Should contain failure details before the summary
    ?assertNotEqual(nomatch, binary:match(Str, <<"MyTest>>testB">>)),
    ?assertNotEqual(nomatch, binary:match(Str, <<"Expected 4, got 6">>)),
    ?assertNotEqual(nomatch, binary:match(Str, <<"OtherTest>>testC">>)),
    %% Summary at the end
    ?assertNotEqual(nomatch, binary:match(Str, <<"TestResult(3 tests, 1 passed, 2 failed">>)).

print_string_failure_without_class_test() ->
    %% Legacy entries without class key should still format correctly
    Result = make_result(1, 0, 1, 0, 0.5, [
        #{name => testX, status => fail, error => <<"boom">>}
    ]),
    Str = beamtalk_test_runner:result_print_string(Result),
    ?assertNotEqual(nomatch, binary:match(Str, <<"unknown>>testX">>)),
    ?assertNotEqual(nomatch, binary:match(Str, <<"boom">>)).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

make_result(Total, Passed, Failed, Skipped, Duration, Tests) ->
    #{
        '$beamtalk_class' => 'TestResult',
        total => Total,
        passed => Passed,
        failed => Failed,
        skipped => Skipped,
        duration => Duration,
        tests => Tests
    }.
