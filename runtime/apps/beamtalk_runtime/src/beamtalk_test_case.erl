%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestCase runtime support for BUnit test framework.
%%%
%%% Implements test discovery, execution, and result collection for
%%% TestCase subclasses. Part of ADR 0014 Phase 2: SUnit-style testing.
%%%
%%% Provides class-side methods:
%%%   - runAll: discover and run all test* methods
%%%   - run: run a single test method by name
%%%
%%% Test execution includes setUp/tearDown lifecycle and result formatting.

-module(beamtalk_test_case).
-include("beamtalk.hrl").

-export([
    dispatch/3,
    signal_failure/2,
    run_all/1,
    run_single/2
]).

%% @doc Primitive dispatch for TestCase methods.
%%
%% Routes @primitive method calls to the appropriate implementation.
%% This is called by compiled TestCase class methods.
-spec dispatch(atom(), list(), term()) -> term().
dispatch('signalFailure:', [Message], Self) ->
    signal_failure(Self, Message);
dispatch(runAll, [], ClassSelf) ->
    %% Extract class name from ClassSelf (beamtalk_object record)
    ClassName = case ClassSelf of
        {beamtalk_object, _Tag, _Module, ClassPid} ->
            gen_server:call(ClassPid, class_name);
        _ ->
            %% Fallback for testing
            'TestCase'
    end,
    run_all(ClassName);
dispatch('run:', [TestName], ClassSelf) ->
    %% Extract class name from ClassSelf
    ClassName = case ClassSelf of
        {beamtalk_object, _Tag, _Module, ClassPid} ->
            gen_server:call(ClassPid, class_name);
        _ ->
            'TestCase'
    end,
    run_single(ClassName, TestName);
dispatch(Selector, _Args, _Self) ->
    %% Unknown primitive
    Error0 = beamtalk_error:new(does_not_understand, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(Error1).

%% @doc Signal a test failure.
%%
%% Called from TestCase>>signalFailure: primitive.
%% Creates a test_failure error and raises it.
-spec signal_failure(term(), binary()) -> no_return().
signal_failure(_Self, Message) when is_binary(Message) ->
    Error = beamtalk_error:new(test_failure, 'TestCase'),
    Error2 = beamtalk_error:with_message(Error, Message),
    beamtalk_error:raise(Error2).

%% @doc Run all test methods in a TestCase class.
%%
%% Class-side method implementation for TestCase>>runAll.
%% Discovers test* methods, instantiates test case, runs setUp/test/tearDown,
%% collects results, and formats output.
%%
%% Returns formatted test results as a string.
-spec run_all(atom()) -> binary().
run_all(TestClassName) ->
    %% Get the class object
    Class = beamtalk_object_class:get_class(TestClassName),
    
    %% Discover test* methods
    AllMethods = maps:get(instance_methods, Class, #{}),
    TestMethods = discover_test_methods(AllMethods),
    
    %% Run each test and collect results
    StartTime = erlang:monotonic_time(millisecond),
    Results = lists:map(fun(Method) ->
        run_test_method(TestClassName, Method)
    end, TestMethods),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    
    %% Format results
    format_results(Results, Duration).

%% @doc Run a single test method by name.
%%
%% Class-side method implementation for TestCase>>run:
%%
%% testName is a symbol (atom) naming the test method to run.
-spec run_single(atom(), atom()) -> binary().
run_single(TestClassName, TestMethodName) when is_atom(TestMethodName) ->
    %% Verify method exists
    Class = beamtalk_object_class:get_class(TestClassName),
    AllMethods = maps:get(instance_methods, Class, #{}),
    
    case maps:is_key(TestMethodName, AllMethods) of
        false ->
            Msg = io_lib:format("Method '~s' not found in ~s", [TestMethodName, TestClassName]),
            list_to_binary(Msg);
        true ->
            %% Run the test
            StartTime = erlang:monotonic_time(millisecond),
            Result = run_test_method(TestClassName, TestMethodName),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            
            %% Format single result
            format_results([Result], Duration)
    end.

%%% Internal functions

%% Discover methods starting with 'test'
discover_test_methods(Methods) ->
    TestMethods = maps:fold(fun(MethodName, _Impl, Acc) ->
        case atom_to_list(MethodName) of
            "test" ++ _ -> [MethodName | Acc];
            _ -> Acc
        end
    end, [], Methods),
    lists:sort(TestMethods).

%% Run a single test method with setUp/tearDown
run_test_method(TestClassName, MethodName) ->
    try
        %% Instantiate test case
        Instance = beamtalk_object_instances:new(TestClassName, #{}),
        
        %% Run setUp
        case has_method(TestClassName, setUp) of
            true -> beamtalk_dispatch:dispatch(setUp, [], Instance);
            false -> ok
        end,
        
        %% Run test method
        beamtalk_dispatch:dispatch(MethodName, [], Instance),
        
        %% Run tearDown
        case has_method(TestClassName, tearDown) of
            true -> beamtalk_dispatch:dispatch(tearDown, [], Instance);
            false -> ok
        end,
        
        %% Test passed
        {pass, MethodName}
    catch
        error:#{kind := test_failure, message := Msg} ->
            %% Test assertion failed
            {fail, MethodName, Msg};
        error:#{kind := _Kind, message := Msg} ->
            %% Other Beamtalk error during test
            {fail, MethodName, Msg};
        error:Reason ->
            %% Raw Erlang error
            Msg = io_lib:format("~p", [Reason]),
            {fail, MethodName, list_to_binary(Msg)};
        Class:Reason:Stacktrace ->
            %% Other exception
            Msg = io_lib:format("~p:~p", [Class, Reason]),
            logger:debug("Test ~s failed with stacktrace: ~p", [MethodName, Stacktrace]),
            {fail, MethodName, list_to_binary(Msg)}
    end.

%% Check if a class has a method
has_method(ClassName, MethodName) ->
    Class = beamtalk_object_class:get_class(ClassName),
    Methods = maps:get(instance_methods, Class, #{}),
    maps:is_key(MethodName, Methods).

%% Format test results for display
format_results(Results, Duration) ->
    Total = length(Results),
    Passed = length([R || {pass, _} <- Results]),
    Failed = Total - Passed,
    
    %% Build result string
    Lines = case Failed of
        0 ->
            %% All passed
            Summary = io_lib:format("~p tests, ~p passed ✓ (~.1fs)", [Total, Passed, Duration]),
            [list_to_binary(Summary)];
        _ ->
            %% Some failed
            Summary = io_lib:format("~p tests, ~p passed, ~p failed ✗ (~.1fs)", [Total, Passed, Failed, Duration]),
            Failures = [format_failure(F) || F <- Results, is_failure(F)],
            [list_to_binary(Summary), <<"\n\n">> | Failures]
    end,
    
    iolist_to_binary(Lines).

is_failure({fail, _, _}) -> true;
is_failure(_) -> false.

format_failure({fail, MethodName, Message}) ->
    io_lib:format("FAIL: ~s\n  ~s\n", [MethodName, Message]).
