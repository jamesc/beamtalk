%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestRunner and TestResult primitive implementations (BT-762).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% TestRunner provides programmatic test execution returning structured
%%% TestResult objects. TestResult wraps the structured maps from
%%% beamtalk_test_case into first-class Beamtalk value objects.
%%%
%%% TestResult is a tagged map:
%%% ```
%%% #{'$beamtalk_class' => 'TestResult',
%%%    passed => integer(), failed => integer(), skipped => integer(), total => integer(),
%%%    duration => float(), tests => [#{name, status, error?}]}
%%% ```

-module(beamtalk_test_runner).
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    %% TestRunner class-side primitives
    run_all/0,
    run_file/1,
    run_class/1,
    run_class_by_name/1,
    run_method/2,
    %% TestResult instance primitives
    result_passed/1,
    result_failed/1,
    result_skipped/1,
    result_total/1,
    result_duration/1,
    result_failures/1,
    result_has_passed/1,
    result_summary/1,
    result_print_string/1
]).

%% FFI shims for (Erlang beamtalk_test_runner) dispatch
-export([runAll/0, run/1, run/2]).
%% Exported for testing
-export([path_suffix_match/2]).
%% TestResult instance-side
-export([
    passed/1,
    failed/1,
    skipped/1,
    total/1,
    duration/1,
    failures/1,
    hasPassed/1,
    summary/1,
    printString/1
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
            make_test_result(0, 0, 0, 0, 0.0, []);
        _ ->
            ClassResults = lists:map(
                fun(ClassName) ->
                    run_class_by_name(ClassName)
                end,
                Classes
            ),
            aggregate_results(ClassResults)
    end.

%% @doc Run all TestCase subclasses whose source file matches FilePath.
%%
%% FilePath is a binary, matched as a path suffix against the absolute path
%% stored in each compiled module's beamtalk_source attribute. This allows
%% passing relative paths like <<"test/foo_test.bt">> to match the full path.
%%
%% Returns an aggregated TestResult. If no matching classes are found,
%% returns an empty TestResult.
-spec run_file(binary()) -> map().
run_file(FilePath) when is_binary(FilePath) ->
    AllTestClasses = beamtalk_test_case:find_test_classes(),
    MatchingClasses = lists:filter(
        fun(ClassName) ->
            class_source_matches(ClassName, FilePath)
        end,
        AllTestClasses
    ),
    case MatchingClasses of
        [] ->
            make_test_result(0, 0, 0, 0, 0.0, []);
        _ ->
            ClassResults = lists:map(fun run_class_by_name/1, MatchingClasses),
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

%% @doc Number of skipped tests.
-spec result_skipped(map()) -> non_neg_integer().
result_skipped(#{'$beamtalk_class' := 'TestResult', skipped := Skipped}) ->
    Skipped.

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
    skipped := Skipped,
    duration := Duration
}) ->
    case {Failed, Skipped} of
        {0, 0} ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed (~.1fs)", [Total, Passed, Duration]
                )
            );
        {0, _} ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed, ~p skipped (~.1fs)",
                    [Total, Passed, Skipped, Duration]
                )
            );
        {_, 0} ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed, ~p failed (~.1fs)",
                    [Total, Passed, Failed, Duration]
                )
            );
        _ ->
            iolist_to_binary(
                io_lib:format(
                    "~p tests, ~p passed, ~p skipped, ~p failed (~.1fs)",
                    [Total, Passed, Skipped, Failed, Duration]
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

%% @doc Run all tests in a named class.
%%
%% Discovers test methods via the class gen_server (not module_info,
%% which is unavailable on Core Erlang compiled modules from .bt files).
%% Public so op handlers can call it directly by class name atom.
-spec run_class_by_name(atom()) -> map().
run_class_by_name(ClassName) ->
    {TestMethods, FlatMethods, Module} = discover_methods_via_registry(ClassName),
    case TestMethods of
        [] ->
            make_test_result(0, 0, 0, 0, 0.0, []);
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = beamtalk_test_case:run_suite_lifecycle(
                ClassName,
                Module,
                FlatMethods,
                TestMethods,
                fun(SuiteFixture) ->
                    lists:map(
                        fun(Method) ->
                            beamtalk_test_case:run_test_method(
                                ClassName, Module, Method, FlatMethods, SuiteFixture
                            )
                        end,
                        TestMethods
                    )
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            Structured = beamtalk_test_case:structure_results(ClassName, Results, Duration),
            structured_to_test_result(Structured)
    end.

%% @doc Run a single test method in a class by name.
-spec run_method_by_name(atom(), atom()) -> map().
run_method_by_name(ClassName, TestName) ->
    {_TestMethods, FlatMethods, Module} = discover_methods_via_registry(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Results = beamtalk_test_case:run_suite_lifecycle(
        ClassName,
        Module,
        FlatMethods,
        [TestName],
        fun(SuiteFixture) ->
            [
                beamtalk_test_case:run_test_method(
                    ClassName, Module, TestName, FlatMethods, SuiteFixture
                )
            ]
        end
    ),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    Structured = beamtalk_test_case:structure_results(ClassName, Results, Duration),
    structured_to_test_result(Structured).

%% @doc Discover methods and module via class registry gen_server.
%%
%% Returns {TestMethods, FlatMethods, Module} where:
%% - TestMethods is the sorted list of test* selectors
%% - FlatMethods is a map of all methods (for setUp/tearDown detection)
%% - Module is the authoritative BEAM module atom from the class gen_server
%%
%% Using Module from gen_server state (rather than resolve_module/1) ensures
%% we use the exact module atom the class was compiled with, avoiding the
%% "setUp failed: error:undef" misdiagnosis when module naming differs.
%% Raises a structured error if the class is not registered.
-spec discover_methods_via_registry(atom()) -> {[atom()], map(), atom()}.
discover_methods_via_registry(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Msg = iolist_to_binary(
                io_lib:format("Class '~s' not found in class registry", [ClassName])
            ),
            Error0 = beamtalk_error:new(class_not_found, 'TestRunner'),
            Error1 = beamtalk_error:with_selector(Error0, 'run:'),
            Error2 = beamtalk_error:with_message(Error1, Msg),
            beamtalk_error:raise(Error2);
        ClassPid ->
            Methods = gen_server:call(ClassPid, methods),
            Module = beamtalk_object_class:module_name(ClassPid),
            FlatMethods = maps:from_list([{M, true} || M <- Methods]),
            TestMethods = [
                M
             || M <- Methods,
                lists:prefix("test", atom_to_list(M))
            ],
            {lists:sort(TestMethods), FlatMethods, Module}
    end.

%%====================================================================
%% Internal helpers
%%====================================================================

%% @doc Check whether a class's source file matches the given path.
%%
%% Looks up the class in the registry, reads its module's beamtalk_source
%% attribute, and checks if the stored path ends with FilePath.
%% Returns false if the class is not registered or has no source attribute.
-spec class_source_matches(atom(), binary()) -> boolean().
class_source_matches(ClassName, FilePath) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            false;
        ClassPid ->
            try
                ModuleName = beamtalk_object_class:module_name(ClassPid),
                case beamtalk_reflection:source_file_from_module(ModuleName) of
                    nil -> false;
                    StoredPath -> path_suffix_match(StoredPath, FilePath)
                end
            catch
                exit:{noproc, _} -> false;
                exit:{timeout, _} -> false
            end
    end.

%% @doc Check whether Stored (absolute) ends with Suffix (relative or absolute).
%%
%% Both inputs are normalised to forward slashes before comparison, so that
%% Windows-style stored paths (backslash separators from canonicalize) and
%% Unix-style suffix arguments both work correctly. Exact match, or the stored
%% path ends with "/Suffix" (path-boundary safe).
-spec path_suffix_match(binary(), binary()) -> boolean().
path_suffix_match(Stored, Suffix) when is_binary(Stored), is_binary(Suffix) ->
    StoredStr = normalize_separators(binary_to_list(Stored)),
    SuffixStr = normalize_separators(binary_to_list(Suffix)),
    SL = length(StoredStr),
    SuffL = length(SuffixStr),
    StoredStr =:= SuffixStr orelse
        (SL > SuffL andalso
            lists:suffix(SuffixStr, StoredStr) andalso
            lists:nth(SL - SuffL, StoredStr) =:= $/).

%% @doc Normalise path separators to forward slash.
%%
%% Converts backslashes to forward slashes so that Windows paths stored in
%% the beamtalk_source module attribute compare correctly against Unix-style
%% relative paths passed by callers.
-spec normalize_separators(string()) -> string().
normalize_separators(S) ->
    [
        case C of
            $\\ -> $/;
            _ -> C
        end
     || C <- S
    ].

%% @doc Convert a structured result map to a TestResult tagged map.
-spec structured_to_test_result(map()) -> map().
structured_to_test_result(#{
    total := T,
    passed := P,
    failed := F,
    skipped := S,
    duration := D,
    tests := Tests
}) ->
    make_test_result(T, P, F, S, D, Tests).

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
    non_neg_integer(),
    float(),
    list()
) -> map().
make_test_result(Total, Passed, Failed, Skipped, Duration, Tests) ->
    #{
        '$beamtalk_class' => 'TestResult',
        total => Total,
        passed => Passed,
        failed => Failed,
        skipped => Skipped,
        duration => Duration,
        tests => Tests
    }.

%% @doc Aggregate results from multiple classes into a single TestResult.
-spec aggregate_results([map()]) -> map().
aggregate_results(ClassResults) ->
    {TotalSum, PassedSum, FailedSum, SkippedSum, DurationSum, RevTests} =
        lists:foldl(
            fun(
                #{
                    total := T,
                    passed := P,
                    failed := F,
                    skipped := S,
                    duration := D,
                    tests := Tests
                },
                {TAcc, PAcc, FAcc, SAcc, DAcc, TestsAcc}
            ) ->
                {TAcc + T, PAcc + P, FAcc + F, SAcc + S, DAcc + D, lists:reverse(Tests) ++ TestsAcc}
            end,
            {0, 0, 0, 0, 0.0, []},
            ClassResults
        ),
    make_test_result(
        TotalSum,
        PassedSum,
        FailedSum,
        SkippedSum,
        DurationSum,
        lists:reverse(RevTests)
    ).

%%====================================================================
%% FFI shims — (Erlang beamtalk_test_runner) dispatch
%%====================================================================

%% runAll → runAll/0  (TestRunner runAll)
runAll() -> run_all().

%% run: → run/1  (TestRunner run: testClass)
run(TestClass) -> run_class(TestClass).

%% run:method: → run/2  (TestRunner run: testClass method: testName)
run(TestClass, TestName) -> run_method(TestClass, TestName).

%% TestResult instance shims — passed: → passed/1, etc.
passed(Self) -> result_passed(Self).
failed(Self) -> result_failed(Self).
skipped(Self) -> result_skipped(Self).
total(Self) -> result_total(Self).
duration(Self) -> result_duration(Self).
failures(Self) -> result_failures(Self).
hasPassed(Self) -> result_has_passed(Self).
summary(Self) -> result_summary(Self).
printString(Self) -> result_print_string(Self).
