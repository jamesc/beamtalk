%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_tracing shim (ADR 0069 Phase 3).
%%%
%%% Tests cover: lifecycle (enable/disable/isEnabled/clear), query delegation,
%%% pid extraction from #beamtalk_object{} records, error behavior for
%%% invalid arguments, and graceful degradation when trace store is not running.
-module(beamtalk_tracing_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Ensure telemetry is available
    _ = application:ensure_all_started(telemetry),
    _ = application:ensure_all_started(telemetry_poller),
    %% Start the trace store if not already running
    case whereis(beamtalk_trace_store) of
        undefined ->
            {ok, _Pid} = beamtalk_trace_store:start_link(),
            ok;
        _Pid ->
            ok
    end.

cleanup(_State) ->
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable()
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Tests: Lifecycle
%%====================================================================

lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"enable returns nil", ?_assertEqual(nil, beamtalk_tracing:enable())},
            {"disable returns nil", ?_assertEqual(nil, beamtalk_tracing:disable())},
            {"clear returns nil", ?_assertEqual(nil, beamtalk_tracing:clear())},
            {"isEnabled returns false by default",
                ?_test(begin
                    beamtalk_tracing:disable(),
                    ?assertEqual(false, beamtalk_tracing:isEnabled())
                end)},
            {"enable/disable cycle",
                ?_test(begin
                    beamtalk_tracing:enable(),
                    ?assertEqual(true, beamtalk_tracing:isEnabled()),
                    beamtalk_tracing:disable(),
                    ?assertEqual(false, beamtalk_tracing:isEnabled())
                end)}
        ]
    end}.

%%====================================================================
%% Tests: Trace queries with empty store
%%====================================================================

empty_queries_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"traces returns [] when empty", ?_assertEqual([], beamtalk_tracing:traces())},
            {"tracesFor returns [] for unknown pid",
                ?_assertEqual([], beamtalk_tracing:tracesFor(self()))},
            {"tracesFor/2 returns [] for unknown pid+selector",
                ?_assertEqual([], beamtalk_tracing:tracesFor(self(), increment))},
            {"stats returns #{} when empty", ?_assertEqual(#{}, beamtalk_tracing:stats())},
            {"statsFor returns #{} for unknown pid",
                ?_assertEqual(#{}, beamtalk_tracing:statsFor(self()))},
            {"slowMethods returns [] when empty",
                ?_assertEqual([], beamtalk_tracing:slowMethods(10))},
            {"hotMethods returns [] when empty",
                ?_assertEqual([], beamtalk_tracing:hotMethods(10))},
            {"errorMethods returns [] when empty",
                ?_assertEqual([], beamtalk_tracing:errorMethods(5))},
            {"bottlenecks returns [] when empty",
                ?_assertEqual([], beamtalk_tracing:bottlenecks(5))}
        ]
    end}.

%%====================================================================
%% Tests: Pid extraction from #beamtalk_object{}
%%====================================================================

pid_extraction_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"tracesFor accepts #beamtalk_object{}",
                ?_test(begin
                    Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
                    ?assertEqual([], beamtalk_tracing:tracesFor(Obj))
                end)},
            {"statsFor accepts #beamtalk_object{}",
                ?_test(begin
                    Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
                    ?assertEqual(#{}, beamtalk_tracing:statsFor(Obj))
                end)},
            {"tracesFor accepts raw pid",
                ?_test(begin
                    ?assertEqual([], beamtalk_tracing:tracesFor(self()))
                end)},
            {"tracesFor rejects non-pid non-object",
                ?_test(begin
                    ?assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:tracesFor(42))
                end)},
            {"healthFor accepts #beamtalk_object{}",
                ?_test(begin
                    Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
                    Result = beamtalk_tracing:healthFor(Obj),
                    ?assertMatch(#{status := _}, Result)
                end)}
        ]
    end}.

%%====================================================================
%% Tests: Live health
%%====================================================================

health_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"healthFor live process returns status",
                ?_test(begin
                    Result = beamtalk_tracing:healthFor(self()),
                    ?assertMatch(
                        #{pid := _, status := _, queue_depth := _, memory_kb := _}, Result
                    ),
                    ?assertNotEqual(dead, maps:get(status, Result))
                end)},
            {"healthFor dead process returns dead status",
                ?_test(begin
                    Pid = spawn(fun() -> ok end),
                    timer:sleep(50),
                    Result = beamtalk_tracing:healthFor(Pid),
                    ?assertEqual(dead, maps:get(status, Result)),
                    ?assertEqual(<<"process not alive">>, maps:get(error, Result))
                end)},
            {"systemHealth returns VM info",
                ?_test(begin
                    Result = beamtalk_tracing:systemHealth(),
                    ?assertMatch(#{scheduler_count := _, process_count := _, memory := _}, Result),
                    ?assert(maps:get(scheduler_count, Result) > 0)
                end)}
        ]
    end}.

%%====================================================================
%% Tests: Configuration
%%====================================================================

config_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"maxEvents returns default", ?_assertEqual(100000, beamtalk_tracing:maxEvents())},
            {"maxEvents/1 sets value",
                ?_test(begin
                    beamtalk_tracing:maxEvents(50000),
                    ?assertEqual(50000, beamtalk_tracing:maxEvents()),
                    %% Reset
                    beamtalk_tracing:maxEvents(100000)
                end)},
            {"maxEvents/1 returns nil", ?_assertEqual(nil, beamtalk_tracing:maxEvents(25000))}
        ]
    end}.

%%====================================================================
%% Tests: Type error validation
%%====================================================================

type_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"slowMethods rejects 0",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:slowMethods(0))},
            {"slowMethods rejects negative",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:slowMethods(-1))},
            {"hotMethods rejects 0",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:hotMethods(0))},
            {"errorMethods rejects 0",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:errorMethods(0))},
            {"bottlenecks rejects 0",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:bottlenecks(0))},
            {"maxEvents/1 rejects 0",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:maxEvents(0))},
            {"maxEvents/1 rejects negative",
                ?_assertError(#beamtalk_error{kind = type_error}, beamtalk_tracing:maxEvents(-1))}
        ]
    end}.

%%====================================================================
%% Tests: Query delegation with data
%%====================================================================

query_with_data_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"stats populated after record_dispatch",
                ?_test(begin
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(TestPid, increment, 5000, ok, sync),
                    Stats = beamtalk_tracing:stats(),
                    PidKey = pid_to_list(TestPid),
                    ?assert(maps:is_key(PidKey, Stats)),
                    PidStats = maps:get(PidKey, Stats),
                    Methods = maps:get(methods, PidStats),
                    ?assert(maps:is_key("increment", Methods))
                end)},
            {"tracesFor returns events after record_trace_event",
                ?_test(begin
                    beamtalk_tracing:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    Traces = beamtalk_tracing:tracesFor(TestPid),
                    ?assert(length(Traces) >= 1),
                    [First | _] = Traces,
                    ?assertEqual(increment, maps:get(selector, First)),
                    beamtalk_tracing:disable()
                end)},
            {"tracesFor/2 filters by selector",
                ?_test(begin
                    beamtalk_tracing:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', getValue, sync, 1000, ok, #{}, stop
                    ),
                    Traces = beamtalk_tracing:tracesFor(TestPid, increment),
                    ?assert(length(Traces) >= 1),
                    lists:foreach(
                        fun(T) ->
                            ?assertEqual(increment, maps:get(selector, T))
                        end,
                        Traces
                    ),
                    beamtalk_tracing:disable()
                end)}
        ]
    end}.
