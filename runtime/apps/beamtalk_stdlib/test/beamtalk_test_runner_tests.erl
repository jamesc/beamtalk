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
%%% TestResult instance FFI shim tests
%%%
%%% The lowercase shims (passed/1, failed/1, ...) are the selectors dispatched
%%% from compiled Beamtalk for `aResult passed` etc. They delegate to the
%%% result_* accessors. Cover them directly.
%%% ============================================================================

shim_passed_test() ->
    Result = make_result(3, 2, 1, 0, 1.0, []),
    ?assertEqual(2, beamtalk_test_runner:passed(Result)).

shim_failed_test() ->
    Result = make_result(3, 2, 1, 0, 1.0, []),
    ?assertEqual(1, beamtalk_test_runner:failed(Result)).

shim_skipped_test() ->
    Result = make_result(4, 2, 1, 1, 1.0, []),
    ?assertEqual(1, beamtalk_test_runner:skipped(Result)).

shim_total_test() ->
    Result = make_result(4, 2, 1, 1, 1.0, []),
    ?assertEqual(4, beamtalk_test_runner:total(Result)).

shim_duration_test() ->
    Result = make_result(1, 1, 0, 0, 2.5, []),
    ?assertEqual(2.5, beamtalk_test_runner:duration(Result)).

shim_failures_test() ->
    Result = make_result(2, 1, 1, 0, 1.0, [
        #{name => testA, class => 'T', status => pass},
        #{name => testB, class => 'T', status => fail, error => <<"boom">>}
    ]),
    Failures = beamtalk_test_runner:failures(Result),
    ?assertEqual(1, length(Failures)),
    ?assertEqual(testB, maps:get(name, hd(Failures))).

shim_has_passed_true_test() ->
    Result = make_result(2, 2, 0, 0, 1.0, []),
    ?assert(beamtalk_test_runner:hasPassed(Result)).

shim_has_passed_false_test() ->
    Result = make_result(2, 1, 1, 0, 1.0, []),
    ?assertNot(beamtalk_test_runner:hasPassed(Result)).

shim_summary_test() ->
    Result = make_result(3, 3, 0, 0, 1.5, []),
    ?assertEqual(<<"3 tests, 3 passed (1.5s)">>, beamtalk_test_runner:summary(Result)).

shim_print_string_test() ->
    Result = make_result(2, 2, 0, 0, 1.0, [
        #{name => testA, class => 'T', status => pass},
        #{name => testB, class => 'T', status => pass}
    ]),
    ?assertEqual(
        <<"TestResult(2 tests, 2 passed (1.0s))">>, beamtalk_test_runner:printString(Result)
    ).

%%% ============================================================================
%%% Non-binary error formatting (BT-2384)
%%%
%%% format_single_failure / serialize_test_result both have a branch for an
%%% error term that is not a binary (it is printed via beamtalk_primitive),
%%% plus a fallback for a fail entry that carries no error key at all.
%%% ============================================================================

print_string_non_binary_error_test() ->
    %% error is an atom, not a binary — exercises print_string conversion.
    Result = make_result(1, 0, 1, 0, 0.5, [
        #{name => testX, class => 'T', status => fail, error => some_atom_error}
    ]),
    Str = beamtalk_test_runner:result_print_string(Result),
    ?assertNotEqual(nomatch, binary:match(Str, <<"T>>testX">>)),
    ?assertNotEqual(nomatch, binary:match(Str, <<"some_atom_error">>)).

print_string_fail_without_error_key_test() ->
    %% fail entry with no error key — exercises the fallback format clause.
    Result = make_result(1, 0, 1, 0, 0.5, [
        #{name => testY, class => 'T', status => fail}
    ]),
    Str = beamtalk_test_runner:result_print_string(Result),
    ?assertNotEqual(nomatch, binary:match(Str, <<"T>>testY">>)).

to_json_non_binary_error_test() ->
    Result = make_result(1, 0, 1, 0, 0.5, [
        #{name => testZ, class => 'T', status => fail, error => {tuple, error}}
    ]),
    Json = beamtalk_test_runner:result_to_json(Result),
    Decoded = json:decode(Json),
    [Test] = maps:get(<<"tests">>, Decoded),
    %% error key present, printed from the non-binary term.
    ?assert(is_binary(maps:get(<<"error">>, Test))).

to_json_test_without_class_key_test() ->
    %% A test entry with no class key — serialize_test_result omits "class".
    Result = make_result(1, 1, 0, 0, 0.1, [
        #{name => testW, status => pass}
    ]),
    Json = beamtalk_test_runner:result_to_json(Result),
    Decoded = json:decode(Json),
    [Test] = maps:get(<<"tests">>, Decoded),
    ?assertNot(maps:is_key(<<"class">>, Test)),
    ?assertEqual(<<"testW">>, maps:get(<<"name">>, Test)).

%%% ============================================================================
%%% Live execution tests (BT-2384)
%%%
%%% These tests drive the real test-execution paths (run_class_by_name,
%%% discover_methods_via_registry, run_all, concurrent execution, run_file,
%%% run_class/run_method on a real class tuple) by loading the stdlib and
%%% registering a synthetic TestCase subclass whose backing module implements
%%% new/0 + dispatch/3 for setUp/tearDown/test methods. A passing test
%%% (testAlpha), a failing test (testBravo), and a skipped test (testCharlie)
%%% exercise the pass/fail/skip aggregation branches.
%%% ============================================================================

-define(SYNTH_CLASS, 'BtTestRunnerSynthTest').
-define(SYNTH_MODULE, bt_test_runner_synth_mod).

live_setup() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    case whereis(beamtalk_bootstrap) of
        undefined -> {ok, _} = beamtalk_bootstrap:start_link();
        _ -> ok
    end,
    beamtalk_stdlib:init(),
    load_synth_module(),
    register_synth_class(),
    %% Two extra concurrent classes so run_all(2) exceeds MaxJobs and exercises
    %% the feed-and-collect loop (spawn-next-on-result) in run_classes_concurrent.
    register_extra_class('BtTestRunnerSynthTest2'),
    register_extra_class('BtTestRunnerSynthTest3'),
    ok.

live_teardown(_) ->
    %% Inverse of live_setup/0: stop the synthetic class gen_servers (their
    %% terminate/2 leaves the pg group and clears the registry ETS tables) and
    %% unload the synthetic module, so fixture state doesn't leak into later
    %% EUnit modules sharing this VM and make find_test_classes/0 + run_all/*
    %% order-dependent across the suite.
    stop_synth_class(?SYNTH_CLASS),
    stop_synth_class('BtTestRunnerSynthTest2'),
    stop_synth_class('BtTestRunnerSynthTest3'),
    code:purge(?SYNTH_MODULE),
    code:delete(?SYNTH_MODULE),
    ok.

%% Stop a synthetic class gen_server if still registered. Its terminate/2
%% removes the class from pg + the registry tables.
stop_synth_class(Name) ->
    case whereis(beamtalk_class_registry:registry_name(Name)) of
        undefined ->
            ok;
        Pid ->
            try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end
    end.

%% Compile and load a tiny Erlang module that backs the synthetic TestCase.
load_synth_module() ->
    Src =
        "-module(bt_test_runner_synth_mod).\n"
        "-beamtalk_source([\"test/bt_test_runner_synth_test.bt\"]).\n"
        "-export([new/0, dispatch/3, module_info/0, module_info/1]).\n"
        "new() -> #{'$beamtalk_class' => 'BtTestRunnerSynthTest'}.\n"
        "dispatch(setUp, _, Self) -> Self;\n"
        "dispatch(tearDown, _, Self) -> Self;\n"
        "dispatch(testAlpha, _, Self) -> Self;\n"
        "dispatch(testBravo, _, _Self) -> beamtalk_test_case:fail(<<\"intentional failure\">>);\n"
        "dispatch(testCharlie, _, _Self) -> beamtalk_test_case:skip(<<\"intentional skip\">>).\n",
    {ok, Tokens, _} = erl_scan:string(Src),
    Forms = [
        begin
            {ok, Form} = erl_parse:parse_form(Ts),
            Form
        end
     || Ts <- split_token_forms(Tokens)
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "bt_test_runner_synth_mod.erl", Bin),
    ok.

register_synth_class() ->
    Info = #{
        name => ?SYNTH_CLASS,
        module => ?SYNTH_MODULE,
        superclass => 'TestCase',
        instance_methods => #{
            setUp => #{arity => 0},
            tearDown => #{arity => 0},
            testAlpha => #{arity => 0},
            testBravo => #{arity => 0},
            testCharlie => #{arity => 0}
        },
        instance_variables => []
    },
    case beamtalk_object_class:start_link(?SYNTH_CLASS, Info) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% Register an additional TestCase subclass backed by the same synthetic
%% module so multiple classes run concurrently in run_all(N>1).
register_extra_class(Name) ->
    Info = #{
        name => Name,
        module => ?SYNTH_MODULE,
        superclass => 'TestCase',
        instance_methods => #{
            setUp => #{arity => 0},
            tearDown => #{arity => 0},
            testAlpha => #{arity => 0},
            testBravo => #{arity => 0},
            testCharlie => #{arity => 0}
        },
        instance_variables => []
    },
    case beamtalk_object_class:start_link(Name, Info) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% Split a flat token list into per-form token lists at each `dot`.
split_token_forms(Tokens) -> split_token_forms(Tokens, [], []).
split_token_forms([], _Cur, Acc) ->
    lists:reverse(Acc);
split_token_forms([{dot, _} = Dot | Rest], Cur, Acc) ->
    split_token_forms(Rest, [], [lists:reverse([Dot | Cur]) | Acc]);
split_token_forms([Tok | Rest], Cur, Acc) ->
    split_token_forms(Rest, [Tok | Cur], Acc).

%% A class-reference tuple for the synthetic class (element 2 = 'Name class').
synth_class_ref() ->
    {beamtalk_object, 'BtTestRunnerSynthTest class', ?SYNTH_MODULE, self()}.

live_execution_test_() ->
    {setup, fun live_setup/0, fun live_teardown/1, fun(_) ->
        [
            {"find_test_classes includes the synthetic class", fun() ->
                Classes = beamtalk_test_case:find_test_classes(),
                ?assert(lists:member(?SYNTH_CLASS, Classes))
            end},
            {"run_class_by_name returns a TestResult with pass/fail/skip counts", fun() ->
                R = beamtalk_test_runner:run_class_by_name(?SYNTH_CLASS),
                ?assertEqual('TestResult', maps:get('$beamtalk_class', R)),
                ?assertEqual(3, maps:get(total, R)),
                ?assertEqual(1, maps:get(passed, R)),
                ?assertEqual(1, maps:get(failed, R)),
                ?assertEqual(1, maps:get(skipped, R))
            end},
            {"run_class on a class tuple delegates to run_class_by_name", fun() ->
                R = beamtalk_test_runner:run_class(synth_class_ref()),
                ?assertEqual(3, maps:get(total, R)),
                ?assertEqual(1, maps:get(failed, R))
            end},
            {"run_method runs a single passing test method", fun() ->
                R = beamtalk_test_runner:run_method(synth_class_ref(), testAlpha),
                ?assertEqual(1, maps:get(total, R)),
                ?assertEqual(1, maps:get(passed, R)),
                ?assertEqual(0, maps:get(failed, R))
            end},
            {"run_method runs a single failing test method", fun() ->
                R = beamtalk_test_runner:run_method(synth_class_ref(), testBravo),
                ?assertEqual(1, maps:get(total, R)),
                ?assertEqual(1, maps:get(failed, R))
            end},
            {"run_all (sequential) includes the synthetic class results", fun() ->
                R = beamtalk_test_runner:run_all(1),
                ?assertEqual('TestResult', maps:get('$beamtalk_class', R)),
                %% At least our 3 synthetic tests are present.
                ?assert(maps:get(total, R) >= 3),
                ?assert(maps:get(failed, R) >= 1),
                ?assert(maps:get(skipped, R) >= 1)
            end},
            {"run_all (concurrent) aggregates with wall-clock duration", fun() ->
                R = beamtalk_test_runner:run_all(2),
                ?assertEqual('TestResult', maps:get('$beamtalk_class', R)),
                ?assert(maps:get(total, R) >= 3),
                ?assert(is_float(maps:get(duration, R)))
            end},
            {"run_all (auto / 0 jobs) resolves to scheduler count", fun() ->
                R = beamtalk_test_runner:run_all(0),
                ?assertEqual('TestResult', maps:get('$beamtalk_class', R)),
                ?assert(maps:get(total, R) >= 3)
            end},
            {"runAll/0 shim runs the full suite", fun() ->
                R = beamtalk_test_runner:runAll(),
                ?assertEqual('TestResult', maps:get('$beamtalk_class', R)),
                ?assert(maps:get(total, R) >= 3)
            end},
            {"run/1 shim delegates to run_class", fun() ->
                R = beamtalk_test_runner:run(synth_class_ref()),
                ?assertEqual(3, maps:get(total, R))
            end},
            {"run/2 shim delegates to run_method", fun() ->
                R = beamtalk_test_runner:run(synth_class_ref(), testAlpha),
                ?assertEqual(1, maps:get(passed, R))
            end},
            {"run_class_by_name on an unknown class raises class_not_found", fun() ->
                ?assertError(
                    #{
                        error := #beamtalk_error{
                            kind = class_not_found, class = 'TestRunner'
                        }
                    },
                    beamtalk_test_runner:run_class_by_name('ZzzNoSuchClass')
                )
            end},
            {"run_file with a non-matching path yields an empty TestResult", fun() ->
                R = beamtalk_test_runner:run_file(<<"no/such/path_zzz_test.bt">>),
                ?assertEqual(0, maps:get(total, R)),
                ?assertEqual([], maps:get(tests, R))
            end},
            {"run_file matching the synthetic class source runs its tests", fun() ->
                %% All three synthetic classes share the same backing module
                %% (and thus the same beamtalk_source attribute), so matching
                %% by path suffix drives class_source_matches/2 + the run_file
                %% match branch for each: 3 classes x 3 tests = 9 total.
                R = beamtalk_test_runner:run_file(
                    <<"test/bt_test_runner_synth_test.bt">>
                ),
                ?assertEqual(9, maps:get(total, R)),
                ?assertEqual(3, maps:get(passed, R)),
                ?assertEqual(3, maps:get(failed, R)),
                ?assertEqual(3, maps:get(skipped, R))
            end}
        ]
    end}.

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
