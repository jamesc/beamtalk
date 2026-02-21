%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestRunner and TestResult primitive implementations (BT-762).
%%%
%%% **DDD Context:** Runtime (Testing subdomain)
%%%
%%% TestRunner provides programmatic test execution returning structured
%%% TestResult objects. TestResult wraps the structured maps from
%%% beamtalk_test_case into first-class Beamtalk value objects.
%%%
%%% TestResult is a tagged map:
%%% ```
%%% #{'$beamtalk_class' => 'TestResult',
%%%    passed => integer(), failed => integer(), total => integer(),
%%%    duration => float(), tests => [#{name, status, error?}]}
%%% ```

-module(beamtalk_test_runner).
-include("beamtalk.hrl").

-export([
    %% TestRunner class-side primitives
    run_all/0,
    run_class/1,
    run_method/2,
    %% TestResult instance primitives
    result_passed/1,
    result_failed/1,
    result_total/1,
    result_duration/1,
    result_failures/1,
    result_has_passed/1,
    result_summary/1,
    result_print_string/1
]).

%%====================================================================
%% TestRunner primitives
%%====================================================================

%% @doc Run all tests across all loaded TestCase subclasses.
%%
%% Discovers all TestCase subclasses, runs each, and aggregates
%% results into a single TestResult.
-spec run_all() -> map().
run_all() ->
    Classes = beamtalk_test_case:find_test_classes(),
    case Classes of
        [] ->
            make_test_result(0, 0, 0, 0.0, []);
        _ ->
            ClassResults = lists:map(
                fun(ClassName) ->
                    run_class_by_name(ClassName)
                end,
                Classes
            ),
            aggregate_results(ClassResults)
    end.

%% @doc Run all tests in a single class.
%%
%% ClassRef is a class tuple — element 2 is 'ClassName class' atom.
-spec run_class(tuple()) -> map().
run_class(ClassRef) ->
    ClassName = extract_class_name(ClassRef),
    run_class_by_name(ClassName).

%% @doc Run a single test method in a class.
%%
%% ClassRef is a class tuple, TestName is a symbol atom.
-spec run_method(tuple(), atom()) -> map().
run_method(ClassRef, TestName) when is_atom(TestName) ->
    ClassName = extract_class_name(ClassRef),
    run_method_by_name(ClassName, TestName).

%%====================================================================
%% TestResult instance primitives
%%====================================================================

%% @doc Number of passing tests.
-spec result_passed(map()) -> non_neg_integer().
result_passed(#{'$beamtalk_class' := 'TestResult', passed := Passed}) ->
    Passed.

%% @doc Number of failing tests.
-spec result_failed(map()) -> non_neg_integer().
result_failed(#{'$beamtalk_class' := 'TestResult', failed := Failed}) ->
    Failed.

%% @doc Total test count.
-spec result_total(map()) -> non_neg_integer().
result_total(#{'$beamtalk_class' := 'TestResult', total := Total}) ->
    Total.

%% @doc Elapsed time in seconds.
-spec result_duration(map()) -> float().
result_duration(#{'$beamtalk_class' := 'TestResult', duration := Duration}) ->
    Duration.

%% @doc Collection of failure details.
%%
%% Returns a list of maps with name and error keys.
-spec result_failures(map()) -> [map()].
result_failures(#{'$beamtalk_class' := 'TestResult', tests := Tests}) ->
    [
        #{name => Name, error => Error}
     || #{name := Name, status := fail, error := Error} <- Tests
    ].

%% @doc True if all tests passed.
-spec result_has_passed(map()) -> boolean().
result_has_passed(#{'$beamtalk_class' := 'TestResult', failed := 0}) ->
    true;
result_has_passed(#{'$beamtalk_class' := 'TestResult'}) ->
    false.

%% @doc Formatted summary string.
-spec result_summary(map()) -> binary().
result_summary(#{
    '$beamtalk_class' := 'TestResult',
    total := Total,
    passed := Passed,
    failed := Failed,
    duration := Duration
}) ->
    case Failed of
        0 ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed (~.1fs)", [Total, Passed, Duration]
                )
            );
        _ ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed, ~p failed (~.1fs)",
                    [Total, Passed, Failed, Duration]
                )
            )
    end.

%% @doc printString representation for REPL display.
-spec result_print_string(map()) -> binary().
result_print_string(Result) ->
    Summary = result_summary(Result),
    <<"TestResult(", Summary/binary, ")">>.

%%====================================================================
%% Internal: test execution (uses class registry, not module_info)
%%====================================================================

%% @doc Run all tests in a class by name.
%%
%% Discovers test methods via the class gen_server (not module_info,
%% which is unavailable on Core Erlang compiled modules from .bt files).
-spec run_class_by_name(atom()) -> map().
run_class_by_name(ClassName) ->
    {TestMethods, FlatMethods} = discover_methods_via_registry(ClassName),
    case TestMethods of
        [] ->
            make_test_result(0, 0, 0, 0.0, []);
        _ ->
            Module = beamtalk_test_case:resolve_module(ClassName),
            StartTime = erlang:monotonic_time(millisecond),
            Results = lists:map(
                fun(Method) ->
                    beamtalk_test_case:run_test_method(ClassName, Module, Method, FlatMethods)
                end,
                TestMethods
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            Structured = beamtalk_test_case:structure_results(ClassName, Results, Duration),
            structured_to_test_result(Structured)
    end.

%% @doc Run a single test method in a class by name.
-spec run_method_by_name(atom(), atom()) -> map().
run_method_by_name(ClassName, TestName) ->
    Module = beamtalk_test_case:resolve_module(ClassName),
    {_TestMethods, FlatMethods} = discover_methods_via_registry(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Result = beamtalk_test_case:run_test_method(ClassName, Module, TestName, FlatMethods),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    Structured = beamtalk_test_case:structure_results(ClassName, [Result], Duration),
    structured_to_test_result(Structured).

%% @doc Discover methods via class registry gen_server.
%%
%% Returns {TestMethods, FlatMethods} where TestMethods is the sorted list
%% of test* selectors and FlatMethods is a map of all methods (used by
%% run_test_method to check for setUp/tearDown without module_info).
%% Raises a structured error if the class is not registered.
-spec discover_methods_via_registry(atom()) -> {[atom()], map()}.
discover_methods_via_registry(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Msg = iolist_to_binary(
                io_lib:format("Class '~s' not found in class registry", [ClassName])),
            Error0 = beamtalk_error:new(class_not_found, 'TestRunner'),
            Error1 = beamtalk_error:with_selector(Error0, 'run:'),
            Error2 = beamtalk_error:with_message(Error1, Msg),
            beamtalk_error:raise(Error2);
        ClassPid ->
            Methods = gen_server:call(ClassPid, methods),
            FlatMethods = maps:from_list([{M, true} || M <- Methods]),
            TestMethods = [
                M
             || M <- Methods,
                lists:prefix("test", atom_to_list(M))
            ],
            {lists:sort(TestMethods), FlatMethods}
    end.

%%====================================================================
%% Internal helpers
%%====================================================================

%% @doc Convert a structured result map to a TestResult tagged map.
-spec structured_to_test_result(map()) -> map().
structured_to_test_result(#{
    total := T,
    passed := P,
    failed := F,
    duration := D,
    tests := Tests
}) ->
    make_test_result(T, P, F, D, Tests).

%% @doc Extract class name from a class reference tuple.
%%
%% Class references have element 2 = 'ClassName class' atom.
%% Strip the trailing " class" (6 chars) to get the bare class name.
-spec extract_class_name(tuple()) -> atom().
extract_class_name(ClassRef) when is_tuple(ClassRef) ->
    Tag = element(2, ClassRef),
    TagStr = atom_to_list(Tag),
    Len = length(TagStr),
    NameStr = lists:sublist(TagStr, Len - 6),
    list_to_atom(NameStr).

%% @doc Create a TestResult tagged map.
-spec make_test_result(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    float(),
    list()
) -> map().
make_test_result(Total, Passed, Failed, Duration, Tests) ->
    #{
        '$beamtalk_class' => 'TestResult',
        total => Total,
        passed => Passed,
        failed => Failed,
        duration => Duration,
        tests => Tests
    }.

%% @doc Aggregate results from multiple classes into a single TestResult.
-spec aggregate_results([map()]) -> map().
aggregate_results(ClassResults) ->
    {TotalSum, PassedSum, FailedSum, DurationSum, RevTests} =
        lists:foldl(
            fun(
                #{
                    total := T,
                    passed := P,
                    failed := F,
                    duration := D,
                    tests := Tests
                },
                {TAcc, PAcc, FAcc, DAcc, TestsAcc}
            ) ->
                {TAcc + T, PAcc + P, FAcc + F, DAcc + D,
                 lists:reverse(Tests) ++ TestsAcc}
            end,
            {0, 0, 0, 0.0, []},
            ClassResults
        ),
    make_test_result(TotalSum, PassedSum, FailedSum, DurationSum,
                     lists:reverse(RevTests)).
