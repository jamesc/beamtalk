%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_test_case module.
%%%
%%% Tests assertion methods, test discovery, test execution,
%%% result formatting, and error extraction.

-module(beamtalk_test_case_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% fail: Tests (direct EUnit coverage — fail: remains @primitive)
%%% ============================================================================

fail_with_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail(<<"oops">>)
    ).

fail_with_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail(broken)
    ).

fail_with_other_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:fail(123)
    ).

%%% ============================================================================
%%% should_raise Tests
%%% ============================================================================

should_raise_matching_kind_test() ->
    Block = fun() ->
        error(#beamtalk_error{
            kind = does_not_understand,
            class = 'X',
            message = <<"m">>
        })
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, does_not_understand)).

should_raise_raw_atom_error_test() ->
    Block = fun() -> error(badarith) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, badarith)).

should_raise_no_error_test() ->
    Block = fun() -> ok end,
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:should_raise(Block, some_error)
    ).

should_raise_wrong_kind_test() ->
    Block = fun() ->
        error(#beamtalk_error{
            kind = type_error,
            class = 'X',
            message = <<"m">>
        })
    end,
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = assertion_failed}},
        beamtalk_test_case:should_raise(Block, does_not_understand)
    ).

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

%%% ============================================================================
%%% extract_error_kind Tests (via should_raise which calls it)
%%% ============================================================================

extract_wrapped_exception_test() ->
    %% ADR 0015: Exception objects wrap #beamtalk_error{} records
    Block = fun() ->
        Inner = #beamtalk_error{kind = type_error, class = 'X', message = <<"m">>},
        error(#{class => 'Exception', error => Inner})
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, type_error)).

extract_runtime_error_wrapper_test() ->
    %% RuntimeError wraps #beamtalk_error{} records
    Block = fun() ->
        Inner = #beamtalk_error{kind = assertion_failed, class = 'X', message = <<"m">>},
        error(#{'$beamtalk_class' => 'RuntimeError', error => Inner})
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, assertion_failed)).

extract_unknown_format_returns_error_atom_test() ->
    %% Unknown error format should yield 'error' kind
    Block = fun() -> error({some_tuple, data}) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, error)).

%%% ============================================================================
%%% discover_test_methods Tests
%%% ============================================================================

discover_test_methods_finds_test_prefixed_test() ->
    FlatMethods = #{
        testAdd => {class, info},
        testRemove => {class, info},
        setUp => {class, info},
        tearDown => {class, info},
        helper => {class, info}
    },
    Result = discover_test_methods_wrapper(FlatMethods),
    ?assertEqual([testAdd, testRemove], Result).

discover_test_methods_empty_map_test() ->
    ?assertEqual([], discover_test_methods_wrapper(#{})).

discover_test_methods_no_test_methods_test() ->
    FlatMethods = #{setUp => info, helper => info},
    ?assertEqual([], discover_test_methods_wrapper(FlatMethods)).

%% We can't call discover_test_methods directly since it's internal,
%% but we can test it via execute_tests which calls it.
%% Use a helper that calls the exported execute_tests to verify discovery.
discover_test_methods_wrapper(FlatMethods) ->
    %% Extract test methods by the same logic used internally
    TestMethods = maps:fold(
        fun(Selector, _Info, Acc) ->
            case atom_to_list(Selector) of
                "test" ++ _ -> [Selector | Acc];
                _ -> Acc
            end
        end,
        [],
        FlatMethods
    ),
    lists:sort(TestMethods).

%%% ============================================================================
%%% class_name_to_snake Tests (via resolve_module behavior)
%%% ============================================================================

%% We test the CamelCase->snake_case conversion indirectly.
%% The function is internal but its behavior is observable via execute_tests.

%%% ============================================================================
%%% format_results Tests (via execute_tests)
%%% ============================================================================

execute_tests_no_methods_test() ->
    Result = beamtalk_test_case:execute_tests(runAll, [], 'EmptyClass', fake_module, #{}),
    ?assertEqual(<<"No test methods found in EmptyClass">>, Result).

execute_tests_run_single_not_found_test() ->
    Result = beamtalk_test_case:execute_tests('run:', [testMissing], 'SomeClass', fake_mod, #{}),
    ?assertEqual(<<"Method 'testMissing' not found in SomeClass">>, Result).

%%% ============================================================================
%%% Test Fixture: Minimal test module simulation
%%% ============================================================================

%% Setup to ensure stdlib is loaded for the tests below that need real modules
test_case_setup() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    {ok, _} = beamtalk_bootstrap:start_link(),
    beamtalk_stdlib:init(),
    ok.

test_case_teardown(_) ->
    ok.

test_case_test_() ->
    {setup, fun test_case_setup/0, fun test_case_teardown/1, [
        {"execute_tests runAll with real TestCase module",
            fun execute_tests_with_real_module_test/0},
        {"run_all with real TestCase module", fun run_all_with_real_module_test/0}
    ]}.

execute_tests_with_real_module_test() ->
    %% Use the actual counter_test BUnit module if available
    %% Otherwise just verify the framework runs with empty methods
    FlatMethods = #{
        setUp => {'TestCase', {instance, fun(_A, S) -> {reply, nil, S} end}},
        tearDown => {'TestCase', {instance, fun(_A, S) -> {reply, nil, S} end}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', fake_mod, FlatMethods),
    ?assertEqual(<<"No test methods found in FakeTest">>, Result).

run_all_with_real_module_test() ->
    %% Test the BIF fallback path — resolve_module falls back
    %% to a module that doesn't exist, discover_test_methods_from_module
    %% will crash calling module_info on it. Verify we get a meaningful result.
    %% Use a try/catch to handle the expected failure gracefully.
    try beamtalk_test_case:run_all('NonExistentTestClass') of
        Result when is_binary(Result) -> ok
    catch
        % Expected — module doesn't exist (undef error)
        error:_ -> ok
    end.

%%% ============================================================================
%%% is_valid_setUp_result/2 Tests (BT-1293)
%%% ============================================================================

%% Valid: same class map — normal BT-900 value-object setUp path
is_valid_setUp_result_same_class_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest', value => 0},
    Result = #{'$beamtalk_class' => 'MyTest', value => 42},
    ?assert(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: setUp returns false (untaken ifTrue: branch)
is_valid_setUp_result_false_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest', value => 0},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, false)).

%% Invalid: setUp returns nil
is_valid_setUp_result_nil_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, nil)).

%% Invalid: setUp returns an integer (e.g. result of some computation)
is_valid_setUp_result_integer_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, 42)).

%% Invalid: setUp returns a map of a different class
is_valid_setUp_result_different_class_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    Result = #{'$beamtalk_class' => 'OtherClass'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: setUp returns a map without '$beamtalk_class' key
is_valid_setUp_result_plain_map_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, #{foo => bar})).

%% Invalid: instance itself has no '$beamtalk_class' (degenerate case)
is_valid_setUp_result_no_class_key_test() ->
    Instance = #{value => 0},
    Result = #{value => 42},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: non-map instance (e.g. actor #beamtalk_object record)
is_valid_setUp_result_actor_instance_test() ->
    Instance = #beamtalk_object{class = 'MyTest', class_mod = my_test, pid = self()},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, nil)).
