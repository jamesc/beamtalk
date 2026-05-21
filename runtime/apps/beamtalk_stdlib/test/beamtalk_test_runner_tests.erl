%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_test_runner_tests).

-moduledoc """
EUnit tests for beamtalk_test_runner module.

Covers path_suffix_match/2 which is the core logic for the `file`
parameter of the `test` MCP tool (BT-1234).
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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
%%% run_class/1 and run_method/2 input validation tests (BT-1927)
%%% ============================================================================

run_class_rejects_atom_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner', selector = 'run:'}},
        beamtalk_test_runner:run_class(not_a_tuple)
    ).

run_class_rejects_integer_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner'}},
        beamtalk_test_runner:run_class(42)
    ).

run_class_rejects_list_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner'}},
        beamtalk_test_runner:run_class([1, 2, 3])
    ).

run_class_rejects_malformed_tuple_test() ->
    %% Tuple with non-atom element 2 — should raise type_error, not badarg
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner'}},
        beamtalk_test_runner:run_class({beamtalk_object, 12345, mod, self()})
    ).

run_class_rejects_tuple_without_class_suffix_test() ->
    %% Tuple with atom element 2 but no " class" suffix
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner'}},
        beamtalk_test_runner:run_class({beamtalk_object, 'NotAClass', mod, self()})
    ).

run_method_rejects_non_tuple_test() ->
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = type_error, class = 'TestRunner', selector = 'run:method:'
            }
        },
        beamtalk_test_runner:run_method(not_a_tuple, testFoo)
    ).

run_method_rejects_list_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'TestRunner'}},
        beamtalk_test_runner:run_method([1, 2, 3], testFoo)
    ).

run_method_rejects_non_atom_test_name_test() ->
    %% Valid class tuple but non-atom TestName — should blame TestName, not ClassRef
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = type_error, class = 'TestRunner', selector = 'run:method:'
            }
        },
        beamtalk_test_runner:run_method(
            {beamtalk_object, 'Test class', mod, self()}, <<"not_an_atom">>
        )
    ).

%%% ============================================================================
%%% TestResult accessor tests
%%% ============================================================================

result_passed_test() ->
    Result = make_result(3, 2, 1, 0, 1.0, []),
    ?assertEqual(2, beamtalk_test_runner:result_passed(Result)).

result_failed_test() ->
    Result = make_result(3, 2, 1, 0, 1.0, []),
    ?assertEqual(1, beamtalk_test_runner:result_failed(Result)).

result_skipped_test() ->
    Result = make_result(4, 2, 1, 1, 1.0, []),
    ?assertEqual(1, beamtalk_test_runner:result_skipped(Result)).

result_total_test() ->
    Result = make_result(4, 2, 1, 1, 1.0, []),
    ?assertEqual(4, beamtalk_test_runner:result_total(Result)).

result_duration_test() ->
    Result = make_result(1, 1, 0, 0, 3.14, []),
    ?assertEqual(3.14, beamtalk_test_runner:result_duration(Result)).

result_failures_empty_test() ->
    Result = make_result(2, 2, 0, 0, 1.0, [
        #{name => testA, class => 'T', status => pass},
        #{name => testB, class => 'T', status => pass}
    ]),
    ?assertEqual([], beamtalk_test_runner:result_failures(Result)).

result_failures_skipped_not_included_test() ->
    Result = make_result(2, 1, 0, 1, 1.0, [
        #{name => testA, class => 'T', status => pass},
        #{name => testB, class => 'T', status => skip, reason => <<"unix only">>}
    ]),
    ?assertEqual([], beamtalk_test_runner:result_failures(Result)).

result_failures_returns_failing_tests_test() ->
    Result = make_result(3, 1, 2, 0, 1.0, [
        #{name => testA, class => 'T', status => pass},
        #{name => testB, class => 'T', status => fail, error => <<"boom">>},
        #{name => testC, class => 'T', status => fail, error => <<"crash">>}
    ]),
    Failures = beamtalk_test_runner:result_failures(Result),
    ?assertEqual(2, length(Failures)),
    Names = [maps:get(name, F) || F <- Failures],
    ?assertEqual([testB, testC], Names).

result_has_passed_all_pass_test() ->
    Result = make_result(2, 2, 0, 0, 1.0, []),
    ?assert(beamtalk_test_runner:result_has_passed(Result)).

result_has_passed_with_failures_test() ->
    Result = make_result(2, 1, 1, 0, 1.0, []),
    ?assertNot(beamtalk_test_runner:result_has_passed(Result)).

result_has_passed_zero_tests_test() ->
    Result = make_result(0, 0, 0, 0, 0.0, []),
    ?assert(beamtalk_test_runner:result_has_passed(Result)).

%%% ============================================================================
%%% result_summary/1 branch tests
%%% ============================================================================

result_summary_all_pass_test() ->
    Result = make_result(3, 3, 0, 0, 1.5, []),
    ?assertEqual(<<"3 tests, 3 passed (1.5s)">>, beamtalk_test_runner:result_summary(Result)).

result_summary_only_skipped_test() ->
    %% {0, S} branch — skipped but no failures
    Result = make_result(3, 2, 0, 1, 1.0, []),
    Summary = beamtalk_test_runner:result_summary(Result),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"3 tests">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"2 passed">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"1 skipped">>)).

result_summary_only_failed_test() ->
    %% {F, 0} branch — failures but no skipped
    Result = make_result(3, 2, 1, 0, 1.0, []),
    Summary = beamtalk_test_runner:result_summary(Result),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"3 tests">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"2 passed">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"1 failed">>)).

result_summary_failed_and_skipped_test() ->
    %% {F, S} branch — both failures and skipped
    Result = make_result(5, 2, 2, 1, 2.0, []),
    Summary = beamtalk_test_runner:result_summary(Result),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"5 tests">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"2 passed">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"1 skipped">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"2 failed">>)).

%%% ============================================================================
%%% result_to_json/1 tests
%%% ============================================================================

result_to_json_all_pass_test() ->
    Result = make_result(2, 2, 0, 0, 1.0, [
        #{name => testA, class => 'MyTest', status => pass},
        #{name => testB, class => 'MyTest', status => pass}
    ]),
    Json = beamtalk_test_runner:result_to_json(Result),
    ?assert(is_binary(Json)),
    Decoded = json:decode(Json),
    ?assertEqual(2, maps:get(<<"total">>, Decoded)),
    ?assertEqual(2, maps:get(<<"passed">>, Decoded)),
    ?assertEqual(0, maps:get(<<"failed">>, Decoded)),
    Tests = maps:get(<<"tests">>, Decoded),
    ?assertEqual(2, length(Tests)).

result_to_json_with_failure_test() ->
    Result = make_result(1, 0, 1, 0, 0.5, [
        #{name => testBad, class => 'T', status => fail, error => <<"expected 1 got 2">>}
    ]),
    Json = beamtalk_test_runner:result_to_json(Result),
    Decoded = json:decode(Json),
    ?assertEqual(1, maps:get(<<"failed">>, Decoded)),
    [Test] = maps:get(<<"tests">>, Decoded),
    ?assertEqual(<<"testBad">>, maps:get(<<"name">>, Test)),
    ?assertEqual(<<"fail">>, maps:get(<<"status">>, Test)),
    ?assertEqual(<<"expected 1 got 2">>, maps:get(<<"error">>, Test)).

result_to_json_skip_has_no_error_key_test() ->
    Result = make_result(1, 0, 0, 1, 0.1, [
        #{name => testSkip, class => 'T', status => skip, reason => <<"unix only">>}
    ]),
    Json = beamtalk_test_runner:result_to_json(Result),
    Decoded = json:decode(Json),
    [Test] = maps:get(<<"tests">>, Decoded),
    ?assertEqual(<<"testSkip">>, maps:get(<<"name">>, Test)),
    ?assertEqual(<<"skip">>, maps:get(<<"status">>, Test)),
    %% skip entries have no error key
    ?assertNot(maps:is_key(<<"error">>, Test)).

%%% ============================================================================
%%% run_all/1 invalid-argument error path
%%% ============================================================================

run_all_negative_integer_test() ->
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = invalid_argument, class = 'TestRunner', selector = 'runAll:'
            }
        },
        beamtalk_test_runner:run_all(-1)
    ).

run_all_atom_argument_test() ->
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = invalid_argument, class = 'TestRunner', selector = 'runAll:'
            }
        },
        beamtalk_test_runner:run_all(sequential)
    ).

%%% ============================================================================
%%% ensure_loaded_or_warn/1 tests
%%% ============================================================================

ensure_loaded_or_warn_existing_module_test() ->
    %% erlang module is always loaded — must return ok without warning
    ?assertEqual(ok, beamtalk_test_runner:ensure_loaded_or_warn(erlang)).

ensure_loaded_or_warn_nonexistent_module_test() ->
    %% Should return ok even when loading fails — errors are logged, not raised
    ?assertEqual(ok, beamtalk_test_runner:ensure_loaded_or_warn(nonexistent_module_bt2230)).

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
