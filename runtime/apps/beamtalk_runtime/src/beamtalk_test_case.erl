%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestCase primitive implementations for BUnit test framework.
%%%
%%% **DDD Context:** Runtime (Testing subdomain)
%%%
%%% This module provides the runtime support for TestCase assertion methods.
%%% All assertions create structured #beamtalk_error{} records with kind
%%% `assertion_failed` when they fail.
%%%
%%% Part of ADR 0014: BUnit — Beamtalk Test Framework (Phase 2).

-module(beamtalk_test_case).
-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    assert/1,
    assert_equals/2,
    deny/1,
    should_raise/2,
    fail/1,
    run_all/1,
    run_single/2,
    execute_tests/5,
    run_all_structured/1,
    run_single_structured/2,
    find_test_classes/0,
    spawn_test_execution/6,
    %% BT-762: Exported for beamtalk_test_runner
    run_test_method/4,
    structure_results/3,
    resolve_module/1
]).

%% @doc Assert that a condition is true.
%%
%% Raises assertion_failed error if condition is false.
%% Condition must be a boolean (true or false atom).
%%
%% Example:
%%   assert(true)   % => passes
%%   assert(false)  % => fails with assertion_failed
-spec assert(boolean()) -> nil.
assert(true) ->
    nil;
assert(false) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:'),
    Error2 = beamtalk_error:with_message(
        Error1, <<"Assertion failed: expected true but got false">>
    ),
    Error3 = beamtalk_error:with_details(Error2, #{expected => true, actual => false}),
    beamtalk_error:raise(Error3);
assert(Other) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:'),
    Message = iolist_to_binary(io_lib:format("Expected boolean, got: ~p", [Other])),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Assert that two values are equal.
%%
%% Uses Erlang's =:= operator for comparison (exact equality, per ADR 0002).
%% Raises assertion_failed error if values are not equal.
%%
%% Example:
%%   assert_equals(3, 3)   % => passes
%%   assert_equals(3, 4)   % => fails with assertion_failed
-spec assert_equals(term(), term()) -> nil.
assert_equals(Expected, Actual) when Expected =:= Actual ->
    nil;
assert_equals(Expected, Actual) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:equals:'),
    Message = format_comparison_error(Expected, Actual),
    Error2 = beamtalk_error:with_message(Error1, Message),
    Error3 = beamtalk_error:with_details(Error2, #{expected => Expected, actual => Actual}),
    beamtalk_error:raise(Error3).

%% @doc Assert that a condition is false.
%%
%% Raises assertion_failed error if condition is true.
%% Condition must be a boolean (true or false atom).
%%
%% Example:
%%   deny(false)  % => passes
%%   deny(true)   % => fails with assertion_failed
-spec deny(boolean()) -> nil.
deny(false) ->
    nil;
deny(true) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'deny:'),
    Error2 = beamtalk_error:with_message(
        Error1, <<"Assertion failed: expected false but got true">>
    ),
    Error3 = beamtalk_error:with_details(Error2, #{expected => false, actual => true}),
    beamtalk_error:raise(Error3);
deny(Other) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'deny:'),
    Message = iolist_to_binary(io_lib:format("Expected boolean, got: ~p", [Other])),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Assert that a block raises an error of the specified kind.
%%
%% Executes the block and verifies that it raises an error with the
%% given kind atom. If the block completes normally or raises a different
%% kind of error, the assertion fails.
%%
%% Example:
%%   Block = fun() -> error(my_error) end,
%%   should_raise(Block, my_error)  % => passes
%%
%% Note: Block is a zero-argument Erlang fun in Core Erlang codegen.
-spec should_raise(fun(() -> term()), atom()) -> nil.
should_raise(Block, ExpectedKind) when is_function(Block, 0), is_atom(ExpectedKind) ->
    try Block() of
        _ ->
            % Block completed without error
            NoRaiseErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
            NoRaiseErr1 = beamtalk_error:with_selector(NoRaiseErr0, 'should:raise:'),
            NoRaiseMsg = iolist_to_binary(
                io_lib:format("Expected block to raise ~s but it completed normally", [ExpectedKind])
            ),
            NoRaiseErr2 = beamtalk_error:with_message(NoRaiseErr1, NoRaiseMsg),
            NoRaiseErr3 = beamtalk_error:with_details(NoRaiseErr2, #{
                expected_kind => ExpectedKind, actual => completed
            }),
            beamtalk_error:raise(NoRaiseErr3)
    catch
        _:Exception ->
            ActualKind = extract_error_kind(Exception),
            case ActualKind of
                ExpectedKind ->
                    nil;
                _ ->
                    MismatchErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
                    MismatchErr1 = beamtalk_error:with_selector(MismatchErr0, 'should:raise:'),
                    MismatchMsg = iolist_to_binary(
                        io_lib:format("Expected error kind ~s but got ~s", [
                            ExpectedKind, ActualKind
                        ])
                    ),
                    MismatchErr2 = beamtalk_error:with_message(MismatchErr1, MismatchMsg),
                    MismatchErr3 = beamtalk_error:with_details(MismatchErr2, #{
                        expected_kind => ExpectedKind, actual_kind => ActualKind
                    }),
                    beamtalk_error:raise(MismatchErr3)
            end
    end;
should_raise(Block, ExpectedKind) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'should:raise:'),
    Message =
        case {is_function(Block, 0), is_atom(ExpectedKind)} of
            {false, _} ->
                iolist_to_binary(io_lib:format("Expected a zero-argument block, got: ~p", [Block]));
            {_, false} ->
                iolist_to_binary(
                    io_lib:format("Expected an error kind atom, got: ~p", [ExpectedKind])
                )
        end,
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Unconditionally fail with a message.
%%
%% Always raises an assertion_failed error with the given message.
%% Used when a test detects an invariant violation.
%%
%% Example:
%%   fail(<<"This should not happen">>)
-spec fail(binary() | atom()) -> no_return().
fail(Message) when is_binary(Message) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'fail:'),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2);
fail(Message) when is_atom(Message) ->
    fail(atom_to_binary(Message, utf8));
fail(Message) ->
    MessageBin = iolist_to_binary(io_lib:format("~p", [Message])),
    fail(MessageBin).

%% @doc Execute tests from class-side dispatch (BT-440).
%%
%% Called from beamtalk_object_class handle_call via spawned process.
%% Receives Selector (runAll | 'run:'), Args, ClassName, Module, and
%% FlatMethods directly from gen_server state — no gen_server calls needed.
-spec execute_tests(atom(), list(), atom(), atom(), map()) -> binary().
execute_tests(runAll, _Args, ClassName, Module, FlatMethods) ->
    TestMethods = discover_test_methods(FlatMethods),
    case TestMethods of
        [] ->
            iolist_to_binary(
                io_lib:format(
                    "No test methods found in ~s", [ClassName]
                )
            );
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = lists:map(
                fun(Method) ->
                    run_test_method(ClassName, Module, Method, FlatMethods)
                end,
                TestMethods
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end;
execute_tests('run:', [TestMethodName], ClassName, Module, FlatMethods) ->
    case maps:is_key(TestMethodName, FlatMethods) of
        false ->
            iolist_to_binary(
                io_lib:format(
                    "Method '~s' not found in ~s", [TestMethodName, ClassName]
                )
            );
        true ->
            StartTime = erlang:monotonic_time(millisecond),
            Result = run_test_method(ClassName, Module, TestMethodName, FlatMethods),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results([Result], Duration)
    end.

%% @doc Run all test methods (BT-440 — BIF fallback path).
%% WARNING: May deadlock if called from within a class gen_server handle_call.
%% Prefer execute_tests/5 which receives gen_server state directly and avoids
%% any gen_server:call back to the class process.
-spec run_all(atom()) -> binary().
run_all(ClassName) ->
    Module = resolve_module(ClassName),
    TestMethods = discover_test_methods_from_module(Module),
    case TestMethods of
        [] ->
            iolist_to_binary(
                io_lib:format(
                    "No test methods found in ~s", [ClassName]
                )
            );
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = lists:map(
                fun(Method) ->
                    run_test_method(ClassName, Module, Method, none)
                end,
                TestMethods
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end.

%% @doc Run a single test method (BT-440 — BIF fallback path).
%% WARNING: May deadlock if called from within a class gen_server handle_call.
-spec run_single(atom(), atom()) -> binary().
run_single(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    Module = resolve_module(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Result = run_test_method(ClassName, Module, TestMethodName, none),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    format_results([Result], Duration).

%% @doc Run all tests for a class and return structured results (BT-699).
%%
%% Returns a map with per-test results suitable for JSON encoding.
%% Uses the BIF fallback path (safe outside gen_server).
-spec run_all_structured(atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        duration := float(),
        tests := [#{name := atom(), status := pass | fail, error => binary()}]
    }.
run_all_structured(ClassName) ->
    Module = resolve_module(ClassName),
    TestMethods = discover_test_methods_from_module(Module),
    case TestMethods of
        [] ->
            #{
                class => ClassName,
                total => 0,
                passed => 0,
                failed => 0,
                duration => 0.0,
                tests => []
            };
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = lists:map(
                fun(Method) ->
                    run_test_method(ClassName, Module, Method, none)
                end,
                TestMethods
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            structure_results(ClassName, Results, Duration)
    end.

%% @doc Run a single test method and return structured results (BT-699).
-spec run_single_structured(atom(), atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        duration := float(),
        tests := [#{name := atom(), status := pass | fail, error => binary()}]
    }.
run_single_structured(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    Module = resolve_module(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Result = run_test_method(ClassName, Module, TestMethodName, none),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    structure_results(ClassName, [Result], Duration).

%% @doc Find all loaded TestCase subclasses (BT-699).
%%
%% Uses the class hierarchy ETS table to find all classes that inherit
%% from TestCase. Returns class names as atoms.
-spec find_test_classes() -> [atom()].
find_test_classes() ->
    beamtalk_class_registry:all_subclasses('TestCase').

%%% Internal helpers

%% @doc Format a comparison error message.
-spec format_comparison_error(term(), term()) -> binary().
format_comparison_error(Expected, Actual) ->
    ExpectedStr = format_value(Expected),
    ActualStr = format_value(Actual),
    iolist_to_binary(io_lib:format("Expected ~s, got ~s", [ExpectedStr, ActualStr])).

%% @doc Format a value for display in error messages.
-spec format_value(term()) -> binary().
format_value(Value) when is_binary(Value) ->
    % String - show as 'string'
    <<"'", Value/binary, "'">>;
format_value(Value) when is_atom(Value) ->
    % Atom - show as #symbol
    atom_to_binary(Value, utf8);
format_value(Value) when is_list(Value) ->
    % List - show as [...] (truncated if long)
    case length(Value) of
        Len when Len =< 5 ->
            iolist_to_binary(io_lib:format("~p", [Value]));
        Len ->
            Truncated = lists:sublist(Value, 5),
            iolist_to_binary(io_lib:format("~p... (~p items)", [Truncated, Len]))
    end;
format_value(Value) ->
    % Other types - use Erlang's ~p formatter
    iolist_to_binary(io_lib:format("~p", [Value])).

%% @doc Extract the error kind from an exception.
%%
%% Handles #beamtalk_error{} records, wrapped Exception objects (ADR 0015),
%% and raw Erlang atom errors (e.g., badarith, badarg).
-spec extract_error_kind(term()) -> atom().
extract_error_kind(#beamtalk_error{kind = Kind}) ->
    Kind;
extract_error_kind(#{class := 'Exception', error := #beamtalk_error{kind = Kind}}) ->
    % ADR 0015: Exception objects wrap #beamtalk_error{} records
    Kind;
extract_error_kind(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = Kind}}) ->
    % RuntimeError wraps #beamtalk_error{} records
    Kind;
extract_error_kind(Kind) when is_atom(Kind) ->
    % Raw Erlang error atoms (badarith, badarg, function_clause, etc.)
    Kind;
extract_error_kind(_) ->
    % Unknown error format - return generic 'error' kind
    error.

%% @doc Discover test methods from flattened methods map (no gen_server call).
%% FlatMethods is #{Selector => {DefiningClass, MethodInfo}}.
-spec discover_test_methods(map()) -> [atom()].
discover_test_methods(FlatMethods) ->
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

%% @doc Discover test methods from module exports (BIF fallback path).
-spec discover_test_methods_from_module(atom()) -> [atom()].
discover_test_methods_from_module(Module) ->
    Exports = Module:module_info(exports),
    TestMethods = lists:filtermap(
        fun({FunName, _Arity}) ->
            case atom_to_list(FunName) of
                "test" ++ _ -> {true, FunName};
                _ -> false
            end
        end,
        Exports
    ),
    lists:sort(TestMethods).

%% @doc Resolve the BEAM module atom for a class name (no gen_server call).
%% Uses the beamtalk compiler naming convention: bt@snake_case_name.
%% Falls back to bt@stdlib@snake_case_name for stdlib classes.
-spec resolve_module(atom()) -> atom().
resolve_module(ClassName) ->
    SnakeName = class_name_to_snake(atom_to_list(ClassName)),
    UserModule = list_to_atom("bt@" ++ SnakeName),
    case code:is_loaded(UserModule) of
        {file, _} ->
            UserModule;
        false ->
            StdlibModule = list_to_atom("bt@stdlib@" ++ SnakeName),
            case code:is_loaded(StdlibModule) of
                {file, _} ->
                    StdlibModule;
                false ->
                    %% Try loading (module may be on code path but not yet loaded)
                    case code:ensure_loaded(UserModule) of
                        {module, _} ->
                            UserModule;
                        _ ->
                            case code:ensure_loaded(StdlibModule) of
                                {module, _} -> StdlibModule;
                                % fall back, will error at call site
                                _ -> UserModule
                            end
                    end
            end
    end.

%% @doc Convert CamelCase class name to snake_case module name component.
%% Matches the Rust to_module_name() convention.
-spec class_name_to_snake(string()) -> string().
class_name_to_snake(Name) ->
    class_name_to_snake(Name, false, []).

class_name_to_snake([], _PrevLower, Acc) ->
    lists:reverse(Acc);
class_name_to_snake([H | T], PrevLower, Acc) when H >= $A, H =< $Z ->
    % ASCII uppercase to lowercase
    Lower = H + 32,
    case PrevLower of
        true -> class_name_to_snake(T, false, [Lower, $_ | Acc]);
        false -> class_name_to_snake(T, false, [Lower | Acc])
    end;
class_name_to_snake([H | T], _PrevLower, Acc) ->
    class_name_to_snake(T, H >= $a andalso H =< $z, [H | Acc]).

%% @doc Run a single test method with setUp/tearDown lifecycle (BT-440).
%%
%% FlatMethods is either a map (from gen_server state) or 'none' (BIF fallback).
%% When 'none', uses module_info(exports) to check for setUp/tearDown.
%% Creates fresh instance, runs lifecycle, returns pass/fail.
%% tearDown always runs, even if the test fails.
-spec run_test_method(atom(), atom(), atom(), map() | none) ->
    {pass, atom()} | {fail, atom(), binary()}.
run_test_method(ClassName, Module, MethodName, FlatMethods) ->
    try
        Instance = Module:new(),
        {HasSetUp, HasTearDown} = check_lifecycle_methods(Module, FlatMethods),
        case HasSetUp of
            true -> Module:dispatch(setUp, [], Instance);
            false -> ok
        end,
        TestResult =
            try
                Module:dispatch(MethodName, [], Instance),
                {pass, MethodName}
            catch
                error:#beamtalk_error{kind = assertion_failed, message = AssertMsg} ->
                    {fail, MethodName, AssertMsg};
                error:#beamtalk_error{message = ErrMsg} ->
                    {fail, MethodName, ErrMsg};
                error:TestReason ->
                    FailMsg = case TestReason of
                        {undef, {Mod, Fun, Arity}} ->
                            iolist_to_binary(io_lib:format("Undefined function: ~p:~p/~p", [Mod, Fun, Arity]));
                        _ ->
                            iolist_to_binary(io_lib:format("~p", [TestReason]))
                    end,
                    {fail, MethodName, FailMsg};
                TestClass:TestReason:TestST ->
                    FailMsg = iolist_to_binary(
                        io_lib:format("~p:~p", [TestClass, TestReason])
                    ),
                    ?LOG_DEBUG(
                        "Test ~p:~p failed with stacktrace: ~p",
                        [ClassName, MethodName, TestST]
                    ),
                    {fail, MethodName, FailMsg}
            after
                case HasTearDown of
                    true ->
                        try
                            Module:dispatch(tearDown, [], Instance)
                            % Don't mask the original test failure
                        catch
                            _:_ -> ok
                        end;
                    false ->
                        ok
                end
            end,
        TestResult
    catch
        %% setUp or new() itself failed
        SetupClass:SetupReason ->
            SetupMsg = iolist_to_binary(
                io_lib:format("setUp failed: ~p:~p", [SetupClass, SetupReason])
            ),
            {fail, MethodName, SetupMsg}
    end.

%% @doc Check for setUp/tearDown methods, using FlatMethods map or module exports.
-spec check_lifecycle_methods(atom(), map() | none) -> {boolean(), boolean()}.
check_lifecycle_methods(_Module, FlatMethods) when is_map(FlatMethods) ->
    {maps:is_key(setUp, FlatMethods), maps:is_key(tearDown, FlatMethods)};
check_lifecycle_methods(Module, none) ->
    Exports = Module:module_info(exports),
    {lists:keymember(setUp, 1, Exports), lists:keymember(tearDown, 1, Exports)}.

%% @doc Format test results for REPL display.
-spec format_results([{pass, atom()} | {fail, atom(), binary()}], float()) -> binary().
format_results(Results, Duration) ->
    Total = length(Results),
    Passed = length([ok || {pass, _} <- Results]),
    Failed = Total - Passed,
    IoList =
        case Failed of
            0 ->
                io_lib:format(
                    "~p tests, ~p passed (~.1fs)", [Total, Passed, Duration]
                );
            _ ->
                Summary = io_lib:format(
                    "~p tests, ~p passed, ~p failed (~.1fs)",
                    [Total, Passed, Failed, Duration]
                ),
                Failures = [
                    io_lib:format("  FAIL: ~s\n    ~s\n", [M, Msg])
                 || {fail, M, Msg} <- Results
                ],
                [Summary, "\n\n" | Failures]
        end,
    unicode:characters_to_binary(IoList).

%% @doc Convert raw test results to structured map (BT-699).
-spec structure_results(atom(), [{pass, atom()} | {fail, atom(), binary()}], float()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        duration := float(),
        tests := [#{name := atom(), status := pass | fail, error => binary()}]
    }.
structure_results(ClassName, Results, Duration) ->
    Total = length(Results),
    Passed = length([ok || {pass, _} <- Results]),
    Failed = Total - Passed,
    Tests = lists:map(
        fun
            ({pass, Name}) ->
                #{name => Name, status => pass};
            ({fail, Name, Msg}) ->
                #{name => Name, status => fail, error => Msg}
        end,
        Results
    ),
    #{
        class => ClassName,
        total => Total,
        passed => Passed,
        failed => Failed,
        duration => Duration,
        tests => Tests
    }.

%% @doc Spawn test execution in a separate process to avoid gen_server deadlock.
%%
%% BT-440/BT-704: Test execution calls back into the class system,
%% so it must run outside the class process.
-spec spawn_test_execution(atom(), list(), atom(), atom(), map(), {pid(), term()}) -> pid().
spawn_test_execution(Selector, Args, ClassName, TestModule, FlatMethods, From) ->
    spawn(fun() ->
        try
            Result = execute_tests(Selector, Args, ClassName, TestModule, FlatMethods),
            gen_server:reply(From, {ok, Result})
        catch
            C:E:ST ->
                Error =
                    case E of
                        #beamtalk_error{} ->
                            E;
                        _ ->
                            Err0 = beamtalk_error:new(test_execution_failed, ClassName),
                            beamtalk_error:with_selector(Err0, Selector)
                    end,
                ?LOG_ERROR(
                    "Test execution ~p:~p failed: ~p:~p",
                    [ClassName, Selector, C, E],
                    #{
                        class => ClassName,
                        selector => Selector,
                        error_class => C,
                        error => E,
                        stacktrace => ST
                    }
                ),
                gen_server:reply(From, {error, Error})
        end
    end).
