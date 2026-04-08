%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_runner).

%%% **DDD Context:** Object System Context

-moduledoc """
TestRunner and TestResult primitive implementations (BT-762).

TestRunner provides programmatic test execution returning structured
TestResult objects. TestResult wraps the structured maps from
beamtalk_test_case into first-class Beamtalk value objects.

TestResult is a tagged map:
```
#{'$beamtalk_class' => 'TestResult',
   passed => integer(), failed => integer(), skipped => integer(), total => integer(),
   duration => float(), tests => [#{name, status, error?}]}
```
""".
-include_lib("kernel/include/logger.hrl").

-export([
    %% TestRunner class-side primitives
    run_all/0,
    run_all/1,
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
    result_print_string/1,
    result_to_json/1
]).

-type test_result() :: #{'$beamtalk_class' := 'TestResult', atom() => term()}.
-export_type([test_result/0]).

%% FFI shims for (Erlang beamtalk_test_runner) dispatch
-export([runAll/0, runAll/1, run/1, run/2]).
%% Exported for testing
-export([path_suffix_match/2]).
%% BT-1732: Module loading with on_load failure reporting
-export([ensure_loaded_or_warn/1]).
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

-doc """
Run all tests across all loaded TestCase subclasses (sequential).

Equivalent to `run_all(1)` — runs all classes sequentially.
""".
-spec run_all() -> test_result().
run_all() ->
    run_all(1).

-doc """
Run all tests across all loaded TestCase subclasses with concurrency.

MaxJobs controls how many test classes run concurrently:
- `0`: auto (uses `erlang:system_info(schedulers)`)
- `1`: sequential (backward-compatible)
- `N > 1`: up to N concurrent classes

Classes declaring `serial => true` are run sequentially after all
concurrent classes complete. Classes that do not override `serial`
(default `false`) run concurrently.
""".
-spec run_all(non_neg_integer()) -> test_result().
run_all(0) ->
    run_all(erlang:system_info(schedulers));
run_all(MaxJobs) when is_integer(MaxJobs), MaxJobs >= 1 ->
    run_all_impl(MaxJobs);
run_all(Invalid) ->
    Msg = iolist_to_binary(
        io_lib:format(
            "runAll: expects a non-negative integer, got: ~p",
            [Invalid]
        )
    ),
    Error0 = beamtalk_error:new(invalid_argument, 'TestRunner'),
    Error1 = beamtalk_error:with_selector(Error0, 'runAll:'),
    Error2 = beamtalk_error:with_message(Error1, Msg),
    beamtalk_error:raise(Error2).

-doc "Implementation of run_all after argument validation.".
-spec run_all_impl(pos_integer()) -> test_result().
run_all_impl(MaxJobs) ->
    Classes = beamtalk_test_case:find_test_classes(),
    case Classes of
        [] ->
            make_test_result(0, 0, 0, 0, 0.0, []);
        _ when MaxJobs =:= 1 ->
            %% Sequential: original behavior
            ClassResults = [run_class_by_name(C) || C <- Classes],
            aggregate_results(ClassResults);
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            %% Partition into concurrent and serial classes
            {Concurrent, Serial} = partition_classes(Classes),
            %% Run concurrent classes with bounded parallelism
            ConcurrentResults = run_classes_concurrent(Concurrent, MaxJobs),
            %% Run serial classes one at a time
            SerialResults = [run_class_by_name(C) || C <- Serial],
            EndTime = erlang:monotonic_time(millisecond),
            WallDuration = (EndTime - StartTime) / 1000.0,
            Aggregated = aggregate_results(ConcurrentResults ++ SerialResults),
            %% Replace summed per-class durations with actual wall-clock time
            Aggregated#{duration => WallDuration}
    end.

-doc """
Run all TestCase subclasses whose source file matches FilePath.

FilePath is a binary, matched as a path suffix against the absolute path
stored in each compiled module's beamtalk_source attribute. This allows
passing relative paths like <<"test/foo_test.bt">> to match the full path.

Returns an aggregated TestResult. If no matching classes are found,
returns an empty TestResult.
""".
-spec run_file(binary()) -> test_result().
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
            ClassResults = [run_class_by_name(C) || C <- MatchingClasses],
            aggregate_results(ClassResults)
    end.

-doc """
Run all tests in a single class.

ClassRef is a class tuple — element 2 is 'ClassName class' atom.
""".
-spec run_class(term()) -> test_result().
run_class(ClassRef) ->
    ClassName = extract_class_name(ClassRef),
    run_class_by_name(ClassName).

-doc """
Run a single test method in a class.

ClassRef is a class tuple, TestName is a symbol atom.
""".
-spec run_method(term(), Method :: atom()) -> map().
run_method(ClassRef, TestName) when is_atom(TestName) ->
    ClassName = extract_class_name(ClassRef),
    run_method_by_name(ClassName, TestName).

%%====================================================================
%% TestResult instance primitives
%%====================================================================

-doc "Number of passing tests.".
-spec result_passed(map()) -> non_neg_integer().
result_passed(#{'$beamtalk_class' := 'TestResult', passed := Passed}) ->
    Passed.

-doc "Number of failing tests.".
-spec result_failed(map()) -> non_neg_integer().
result_failed(#{'$beamtalk_class' := 'TestResult', failed := Failed}) ->
    Failed.

-doc "Number of skipped tests.".
-spec result_skipped(map()) -> non_neg_integer().
result_skipped(#{'$beamtalk_class' := 'TestResult', skipped := Skipped}) ->
    Skipped.

-doc "Total test count.".
-spec result_total(map()) -> non_neg_integer().
result_total(#{'$beamtalk_class' := 'TestResult', total := Total}) ->
    Total.

-doc "Elapsed time in seconds.".
-spec result_duration(map()) -> float().
result_duration(#{'$beamtalk_class' := 'TestResult', duration := Duration}) ->
    Duration.

-doc """
Collection of failure details.

Returns a list of maps with name and error keys.
""".
-spec result_failures(map()) -> [map()].
result_failures(#{'$beamtalk_class' := 'TestResult', tests := Tests}) ->
    [
        #{name => Name, error => Error}
     || #{name := Name, status := fail, error := Error} <- Tests
    ].

-doc "True if all tests passed.".
-spec result_has_passed(map()) -> boolean().
result_has_passed(#{'$beamtalk_class' := 'TestResult', failed := 0}) ->
    true;
result_has_passed(#{'$beamtalk_class' := 'TestResult'}) ->
    false.

-doc "Formatted summary string.".
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

-doc """
printString representation for REPL display.

When there are failures, prints each failure with class>>method, error
message, and source location before the summary line. This matches the
output format of `beamtalk test` (CLI).
""".
-spec result_print_string(map()) -> binary().
result_print_string(#{'$beamtalk_class' := 'TestResult', tests := Tests} = Result) ->
    Summary = result_summary(Result),
    SummaryLine = <<"TestResult(", Summary/binary, ")">>,
    FailureLines = format_failure_details(Tests),
    case FailureLines of
        <<>> -> SummaryLine;
        _ -> <<FailureLines/binary, SummaryLine/binary>>
    end.

-doc """
Format failure details for display.

Returns a binary with one line per failure in the format:
  ✗ ClassName>>testMethod
    Error message (source_file.bt:42)
Returns an empty binary if there are no failures.
""".
-spec format_failure_details([map()]) -> binary().
format_failure_details(Tests) ->
    Failures = [T || #{status := fail} = T <- Tests],
    case Failures of
        [] ->
            <<>>;
        _ ->
            Lines = [format_single_failure(F) || F <- Failures],
            iolist_to_binary(Lines)
    end.

-doc "Format a single test failure for display.".
-spec format_single_failure(map()) -> iolist().
format_single_failure(#{name := Name, error := Error} = Entry) ->
    ClassName = maps:get(class, Entry, unknown),
    NameStr = atom_to_list(Name),
    ClassStr = atom_to_list(ClassName),
    ErrorBin =
        case is_binary(Error) of
            true -> Error;
            false -> beamtalk_primitive:print_string(Error)
        end,
    [
        "  \xe2\x9c\x97 ",
        ClassStr,
        ">>",
        NameStr,
        "\n",
        "    ",
        ErrorBin,
        "\n"
    ];
format_single_failure(#{name := Name} = Entry) ->
    %% Fallback for fail entries without an error message
    ClassName = maps:get(class, Entry, unknown),
    NameStr = atom_to_list(Name),
    ClassStr = atom_to_list(ClassName),
    [
        "  \xe2\x9c\x97 ",
        ClassStr,
        ">>",
        NameStr,
        "\n"
    ].

-doc """
Serialize a TestResult to JSON for CLI consumption.

Returns a JSON binary that can be parsed by the CLI to reconstruct the result.
Includes all test details (pass/fail/skip per test, durations, errors).
""".
-spec result_to_json(map()) -> binary().
result_to_json(#{
    '$beamtalk_class' := 'TestResult',
    total := Total,
    passed := Passed,
    failed := Failed,
    skipped := Skipped,
    duration := Duration,
    tests := Tests
}) ->
    JsonStruct = #{
        <<"total">> => Total,
        <<"passed">> => Passed,
        <<"failed">> => Failed,
        <<"skipped">> => Skipped,
        <<"duration">> => Duration,
        <<"tests">> => [serialize_test_result(T) || T <- Tests]
    },
    iolist_to_binary(json:encode(JsonStruct)).

-doc "Serialize a single test result.".
-spec serialize_test_result(map()) -> map().
serialize_test_result(#{name := Name, status := Status} = Test) ->
    Base0 = #{
        <<"name">> => atom_to_binary(Name, utf8),
        <<"status">> => atom_to_binary(Status, utf8)
    },
    Base =
        case Test of
            #{class := ClassName} when is_atom(ClassName) ->
                Base0#{<<"class">> => atom_to_binary(ClassName, utf8)};
            _ ->
                Base0
        end,
    case Test of
        #{error := Error} when is_binary(Error) ->
            Base#{<<"error">> => Error};
        #{error := Error} ->
            Base#{<<"error">> => beamtalk_primitive:print_string(Error)};
        _ ->
            Base
    end.

%%====================================================================
%% Internal: test execution (uses class registry, not module_info)
%%====================================================================

-doc """
Run all tests in a named class.

Discovers test methods via the class gen_server (not module_info,
which is unavailable on Core Erlang compiled modules from .bt files).
Public so op handlers can call it directly by class name atom.
""".
-spec run_class_by_name(atom()) -> test_result().
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
                    [
                        beamtalk_test_case:run_test_method(
                            ClassName, Module, Method, FlatMethods, SuiteFixture
                        )
                     || Method <- TestMethods
                    ]
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            Structured = beamtalk_test_case:structure_results(ClassName, Results, Duration),
            structured_to_test_result(Structured)
    end.

-doc "Run a single test method in a class by name.".
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

-doc """
Discover methods and module via class registry gen_server.

Returns {TestMethods, FlatMethods, Module} where:
- TestMethods is the sorted list of test* selectors
- FlatMethods is a map of all methods (for setUp/tearDown detection)
- Module is the authoritative BEAM module atom from the class gen_server

Using Module from gen_server state (rather than resolve_module/1) ensures
we use the exact module atom the class was compiled with, avoiding the
"setUp failed: error:undef" misdiagnosis when module naming differs.
Raises a structured error if the class is not registered.
""".
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
            FlatMethods = maps:from_keys(Methods, true),
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

-doc """
Check whether a class's source file matches the given path.

Looks up the class in the registry, reads its module's beamtalk_source
attribute, and checks if the stored path ends with FilePath.
Returns false if the class is not registered or has no source attribute.
""".
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

-doc """
Check whether Stored (absolute) ends with Suffix (relative or absolute).

Both inputs are normalised to forward slashes before comparison, so that
Windows-style stored paths (backslash separators from canonicalize) and
Unix-style suffix arguments both work correctly. Exact match, or the stored
path ends with "/Suffix" (path-boundary safe).
""".
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

-doc """
Normalise path separators to forward slash.

Converts backslashes to forward slashes so that Windows paths stored in
the beamtalk_source module attribute compare correctly against Unix-style
relative paths passed by callers.
""".
-spec normalize_separators(string()) -> string().
normalize_separators(S) ->
    [
        case C of
            $\\ -> $/;
            _ -> C
        end
     || C <- S
    ].

-doc "Convert a structured result map to a TestResult tagged map.".
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

-doc """
Extract class name from a class reference tuple.

Class references have element 2 = 'ClassName class' atom.
Strip the trailing " class" (6 chars) to get the bare class name.
""".
-spec extract_class_name(term()) -> atom().
extract_class_name(ClassRef) when is_tuple(ClassRef) ->
    Tag = element(2, ClassRef),
    TagStr = atom_to_list(Tag),
    Len = length(TagStr),
    NameStr = lists:sublist(TagStr, Len - 6),
    list_to_existing_atom(NameStr).

-doc "Create a TestResult tagged map.".
-spec make_test_result(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    float(),
    list()
) -> test_result().
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

-doc "Aggregate results from multiple classes into a single TestResult.".
-spec aggregate_results([test_result()]) -> test_result().
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
                {TAcc + T, PAcc + P, FAcc + F, SAcc + S, DAcc + D, lists:reverse(Tests, TestsAcc)}
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
%% Internal: concurrent test execution (BT-1624)
%%====================================================================

-doc """
Partition classes into concurrent (default) and serial sets.

Queries each class's `serial` class method. Classes returning `true`
go into the serial list; all others are concurrent.
""".
-spec partition_classes([atom()]) -> {[atom()], [atom()]}.
partition_classes(Classes) ->
    lists:partition(
        fun(ClassName) -> not is_serial(ClassName) end,
        Classes
    ).

-doc """
Check whether a class declares `serial => true`.

Sends the `serial` class method message to the class process.
Returns `false` if the class is not registered or doesn't respond.
""".
-spec is_serial(atom()) -> boolean().
is_serial(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            false;
        ClassPid ->
            try
                beamtalk_object_class:class_send(ClassPid, serial, []) =:= true
            catch
                _:_ -> false
            end
    end.

-doc """
Run test classes concurrently with bounded parallelism.

Spawns up to MaxJobs processes at a time, each running one test class.
Collects results via message passing as each process completes.
""".
-spec run_classes_concurrent([atom()], pos_integer()) -> [test_result()].
run_classes_concurrent([], _MaxJobs) ->
    [];
run_classes_concurrent(Classes, MaxJobs) ->
    Ref = make_ref(),
    Total = length(Classes),
    {Initial, Rest} = safe_split(MaxJobs, Classes),
    %% Start initial batch with monitors
    PidMap = lists:foldl(
        fun(ClassName, Acc) ->
            {Pid, MonRef} = spawn_class_worker(ClassName, self(), Ref),
            Acc#{MonRef => ClassName, Pid => MonRef}
        end,
        #{},
        Initial
    ),
    %% Run the feed-and-collect loop
    run_concurrent_loop(Rest, Ref, self(), Total, [], PidMap).

-doc """
Feed remaining classes as workers complete, collecting all results.

Each time a result arrives, we spawn the next class (if any remain)
and accumulate the result. Handles 'DOWN' messages for workers that
die without sending a result (OOM, kill signal, etc.) to prevent
indefinite hangs.
""".
-spec run_concurrent_loop([atom()], reference(), pid(), non_neg_integer(), [map()], map()) ->
    [map()].
run_concurrent_loop(_Remaining, _Ref, _Self, 0, Acc, _PidMap) ->
    lists:reverse(Acc);
run_concurrent_loop(Remaining, Ref, Self, Expected, Acc, PidMap) ->
    receive
        {Ref, Result} ->
            %% Spawn next class if any remain
            case Remaining of
                [Next | Rest] ->
                    {Pid, MonRef} = spawn_class_worker(Next, Self, Ref),
                    NewPidMap = PidMap#{MonRef => Next, Pid => MonRef},
                    run_concurrent_loop(Rest, Ref, Self, Expected - 1, [Result | Acc], NewPidMap);
                [] ->
                    run_concurrent_loop([], Ref, Self, Expected - 1, [Result | Acc], PidMap)
            end;
        {'DOWN', _MonRef, process, _Pid, normal} ->
            %% Worker exited normally after sending its result — ignore
            run_concurrent_loop(Remaining, Ref, Self, Expected, Acc, PidMap);
        {'DOWN', MonRef, process, _Pid, Reason} ->
            %% Worker died without sending a result — synthesize a failure
            ClassName = maps:get(MonRef, PidMap, unknown_class),
            ErrMsg = <<"Test worker crashed: ", (beamtalk_primitive:print_string(Reason))/binary>>,
            Result = make_test_result(
                1,
                0,
                1,
                0,
                0.0,
                [#{name => ClassName, status => fail, error => ErrMsg}]
            ),
            case Remaining of
                [Next | Rest] ->
                    {NewPid, NewMonRef} = spawn_class_worker(Next, Self, Ref),
                    NewPidMap = PidMap#{NewMonRef => Next, NewPid => NewMonRef},
                    run_concurrent_loop(Rest, Ref, Self, Expected - 1, [Result | Acc], NewPidMap);
                [] ->
                    run_concurrent_loop([], Ref, Self, Expected - 1, [Result | Acc], PidMap)
            end
    end.

-doc """
Spawn a monitored worker process to run a test class.

Uses spawn_monitor so the runner detects worker crashes (OOM, kill,
etc.) that bypass the try/catch. Returns {Pid, MonitorRef}.
The worker still catches trapped exceptions to produce friendly errors.
""".
-spec spawn_class_worker(atom(), pid(), reference()) -> {pid(), reference()}.
spawn_class_worker(ClassName, Parent, Ref) ->
    spawn_monitor(fun() ->
        Result =
            try
                run_class_by_name(ClassName)
            catch
                Class:Reason:ST ->
                    ErrMsg = iolist_to_binary(
                        io_lib:format("Test class crashed: ~p:~p~n~p", [Class, Reason, ST])
                    ),
                    make_test_result(
                        1,
                        0,
                        1,
                        0,
                        0.0,
                        [#{name => ClassName, status => fail, error => ErrMsg}]
                    )
            end,
        Parent ! {Ref, Result}
    end).

-doc "Split a list at position N, clamping to list length.".
-spec safe_split(non_neg_integer(), [T]) -> {[T], [T]} when T :: term().
safe_split(N, List) ->
    case length(List) of
        Len when Len =< N -> {List, []};
        _ -> lists:split(N, List)
    end.

%%====================================================================
%% FFI shims — (Erlang beamtalk_test_runner) dispatch
%%====================================================================

%% runAll → runAll/0  (TestRunner runAll)
-spec runAll() -> test_result().
runAll() -> run_all().

%% runAll: → runAll/1  (TestRunner runAll: maxJobs)
-spec runAll(non_neg_integer()) -> test_result().
runAll(MaxJobs) -> run_all(MaxJobs).

%% run: → run/1  (TestRunner run: testClass)
-spec run(term()) -> test_result().
run(TestClass) -> run_class(TestClass).

%% run:method: → run/2  (TestRunner run: testClass method: testName)
-spec run(term(), Method :: atom()) -> test_result().
run(TestClass, TestName) -> run_method(TestClass, TestName).

%% TestResult instance shims — passed: → passed/1, etc.
-spec passed(test_result()) -> non_neg_integer().
passed(Self) -> result_passed(Self).
-spec failed(test_result()) -> non_neg_integer().
failed(Self) -> result_failed(Self).
-spec skipped(test_result()) -> non_neg_integer().
skipped(Self) -> result_skipped(Self).
-spec total(test_result()) -> non_neg_integer().
total(Self) -> result_total(Self).
-spec duration(test_result()) -> float().
duration(Self) -> result_duration(Self).
-spec failures(test_result()) -> [map()].
failures(Self) -> result_failures(Self).
-spec hasPassed(test_result()) -> boolean().
hasPassed(Self) -> result_has_passed(Self).
-spec summary(test_result()) -> binary().
summary(Self) -> result_summary(Self).
-spec printString(test_result()) -> binary().
printString(Self) -> result_print_string(Self).

%%====================================================================
%% BT-1732: Module loading with on_load failure reporting
%%====================================================================

-doc """
Load a module via code:ensure_loaded/1, logging a warning via OTP logger
if loading fails (e.g., on_load hook crashes). This replaces bare
code:ensure_loaded/1 calls in the test harness eval command so that
class on_load failures are reported instead of silently ignored.
""".
-spec ensure_loaded_or_warn(module()) -> ok.
ensure_loaded_or_warn(Module) ->
    case code:ensure_loaded(Module) of
        {module, _} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                #{event => test_module_load_failed, module => Module, reason => Reason},
                #{domain => [beamtalk, stdlib]}
            ),
            ok
    end.
