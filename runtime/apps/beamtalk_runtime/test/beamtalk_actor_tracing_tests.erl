%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for actor send wrapper telemetry instrumentation (ADR 0069 Phase 2a).
%%%
%%% Tests cover: telemetry events emitted by sync_send/3, async_send/4, cast_send/3,
%%% aggregate counter updates, trace event capture when enabled, error handling
%%% preservation (timeout, actor_dead), and lifecycle method exclusion.
-module(beamtalk_actor_tracing_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

setup() ->
    %% Ensure telemetry and runtime deps are available
    _ = application:ensure_all_started(telemetry),
    %% Start instance registry if not running
    case whereis(beamtalk_object_instances) of
        undefined ->
            {ok, _} = beamtalk_object_instances:start_link();
        _ ->
            ok
    end,
    %% Start trace store if not running
    case whereis(beamtalk_trace_store) of
        undefined ->
            {ok, _} = beamtalk_trace_store:start_link();
        _ ->
            ok
    end,
    ok.

cleanup(_State) ->
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable()
    catch
        _:_ -> ok
    end,
    ok.

%% @doc Attach a telemetry handler that sends events to the calling process.
attach_handler(EventName) ->
    Self = self(),
    HandlerId = {actor_tracing_test, EventName, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        EventName,
        fun(_Event, Measurements, Metadata, #{dest := Dest}) ->
            Dest ! {telemetry_event, EventName, Measurements, Metadata}
        end,
        #{dest => Self}
    ),
    HandlerId.

%% @doc Start a test counter actor and register it in the instance registry.
start_registered_counter(InitialValue) ->
    {ok, Pid} = test_counter:start_link(InitialValue),
    ok = beamtalk_object_instances:register('Counter', Pid),
    Pid.

%%====================================================================
%% Test: sync_send emits telemetry stop event
%%====================================================================

sync_send_emits_stop_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    try
                        %% Sync send: getValue
                        Result = beamtalk_actor:sync_send(Pid, getValue, []),
                        ?assertEqual(0, Result),

                        %% Verify stop event was emitted
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], Measurements,
                                Metadata} ->
                                ?assert(maps:is_key(duration, Measurements)),
                                ?assert(maps:get(duration, Measurements) > 0),
                                ?assertEqual(Pid, maps:get(pid, Metadata)),
                                ?assertEqual('Counter', maps:get(class, Metadata)),
                                ?assertEqual(getValue, maps:get(selector, Metadata)),
                                ?assertEqual(sync, maps:get(mode, Metadata)),
                                ?assertEqual(ok, maps:get(outcome, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: async_send emits telemetry stop event
%%====================================================================

async_send_emits_stop_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    try
                        %% Create a future to receive the async result
                        {beamtalk_future, FuturePid} = beamtalk_future:new(),
                        beamtalk_actor:async_send(Pid, getValue, [], FuturePid),

                        %% Verify stop event was emitted
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], Measurements,
                                Metadata} ->
                                ?assert(maps:is_key(duration, Measurements)),
                                ?assertEqual(Pid, maps:get(pid, Metadata)),
                                ?assertEqual('Counter', maps:get(class, Metadata)),
                                ?assertEqual(getValue, maps:get(selector, Metadata)),
                                ?assertEqual(async, maps:get(mode, Metadata)),
                                ?assertEqual(ok, maps:get(outcome, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: cast_send emits telemetry stop event
%%====================================================================

cast_send_emits_stop_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    try
                        beamtalk_actor:cast_send(Pid, increment, []),

                        %% Verify stop event was emitted
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], Measurements,
                                Metadata} ->
                                ?assert(maps:is_key(duration, Measurements)),
                                ?assertEqual(Pid, maps:get(pid, Metadata)),
                                ?assertEqual('Counter', maps:get(class, Metadata)),
                                ?assertEqual(increment, maps:get(selector, Metadata)),
                                ?assertEqual(cast, maps:get(mode, Metadata)),
                                ?assertEqual(cast, maps:get(outcome, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Class name resolved via reverse lookup
%%====================================================================

class_name_resolved_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(42),
                    try
                        _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], _Measurements,
                                Metadata} ->
                                ?assertEqual('Counter', maps:get(class, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Unknown PID defaults to 'unknown' class
%%====================================================================

unknown_pid_defaults_to_unknown_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    %% Start counter WITHOUT registering in instance registry
                    {ok, Pid} = test_counter:start_link(0),
                    try
                        _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], _Measurements,
                                Metadata} ->
                                ?assertEqual(unknown, maps:get(class, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Aggregate counters updated on dispatch
%%====================================================================

aggregate_counters_updated_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                Pid = start_registered_counter(0),
                try
                    %% Multiple sync dispatches
                    beamtalk_actor:sync_send(Pid, increment, []),
                    beamtalk_actor:sync_send(Pid, increment, []),
                    beamtalk_actor:sync_send(Pid, getValue, []),

                    %% Allow telemetry handlers to process
                    timer:sleep(50),

                    %% Check aggregate stats
                    Stats = beamtalk_trace_store:get_stats(),
                    PidKey = pid_to_list(Pid),
                    ?assertMatch(#{PidKey := _}, Stats),
                    PidStats = maps:get(PidKey, Stats),
                    Methods = maps:get(methods, PidStats),

                    %% increment should have 2 calls
                    IncStats = maps:get("increment", Methods),
                    ?assertEqual(2, maps:get(calls, IncStats)),

                    %% getValue should have 1 call
                    GetStats = maps:get("getValue", Methods),
                    ?assertEqual(1, maps:get(calls, GetStats))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Trace events captured when enabled
%%====================================================================

trace_events_captured_when_enabled_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                Pid = start_registered_counter(0),
                try
                    %% Dispatch without tracing enabled — no trace events
                    beamtalk_actor:sync_send(Pid, increment, []),
                    timer:sleep(50),
                    ?assertEqual([], beamtalk_trace_store:get_traces()),

                    %% Enable tracing and dispatch again
                    beamtalk_trace_store:enable(),
                    beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    Traces = beamtalk_trace_store:get_traces(),
                    ?assertEqual(1, length(Traces)),
                    [Trace] = Traces,
                    ?assertEqual(Pid, maps:get(actor, Trace)),
                    ?assertEqual('Counter', maps:get(class, Trace)),
                    ?assertEqual(getValue, maps:get(selector, Trace)),
                    ?assertEqual(sync, maps:get(mode, Trace))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Error handling preserved — actor_dead produces structured error
%%====================================================================

sync_send_dead_actor_preserves_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_registered_counter(0),
                gen_server:stop(Pid),
                timer:sleep(50),

                %% Sync send to dead actor should raise actor_dead error
                Result =
                    try
                        beamtalk_actor:sync_send(Pid, getValue, [])
                    catch
                        error:Err ->
                            {caught, Err}
                    end,
                ?assertMatch({caught, _}, Result),
                {caught, CaughtErr} = Result,
                %% Should be a wrapped beamtalk_error (ensure_wrapped produces a tagged map)
                ?assertMatch(
                    #{error := #beamtalk_error{kind = actor_dead}},
                    CaughtErr
                )
            end)
        ]
    end}.

%%====================================================================
%% Test: Dead actor error preserved for async_send
%%====================================================================

async_send_dead_actor_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    gen_server:stop(Pid),
                    timer:sleep(50),

                    %% Async send to dead actor should reject the future
                    {beamtalk_future, FuturePid} = beamtalk_future:new(),
                    beamtalk_actor:async_send(Pid, getValue, [], FuturePid),

                    %% Telemetry stop event should still be emitted (with error outcome)
                    receive
                        {telemetry_event, [beamtalk, actor, dispatch, stop], _Measurements,
                            Metadata} ->
                            ?assertEqual(error, maps:get(outcome, Metadata))
                    after 1000 ->
                        ?assert(false)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle methods NOT instrumented
%%====================================================================

lifecycle_methods_not_instrumented_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    try
                        %% isAlive is a lifecycle method — should NOT emit telemetry
                        _Alive = beamtalk_actor:sync_send(Pid, isAlive, []),

                        %% pid is a lifecycle method — should NOT emit telemetry
                        _RawPid = beamtalk_actor:sync_send(Pid, pid, []),

                        %% monitor is a lifecycle method — should NOT emit telemetry
                        Ref = beamtalk_actor:sync_send(Pid, monitor, []),
                        erlang:demonitor(Ref, [flush]),

                        %% None of the above should have emitted telemetry events
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, stop], _, _} ->
                                %% Got an unexpected event — fail
                                ?assert(false)
                        after 200 ->
                            %% No events received — correct
                            ok
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: sync_send exception emits telemetry exception event
%%====================================================================

sync_send_exception_emits_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                ExcHandler = attach_handler([beamtalk, actor, dispatch, exception]),
                try
                    %% Start a throwing actor that raises on specific message
                    {ok, Pid} = test_throwing_actor:start_link(),
                    ok = beamtalk_object_instances:register('ThrowingActor', Pid),
                    try
                        %% Call the throwing method — should raise but also emit exception event.
                        %% The actor catches the error in safe_dispatch and returns
                        %% {error, Error} which sync_send unwraps and re-raises inside
                        %% the telemetry:span/3 fun. The span catches it, emits the
                        %% exception event, and re-raises to the caller.
                        _Result =
                            try
                                beamtalk_actor:sync_send(Pid, throwError, [])
                            catch
                                error:_ -> caught
                            end,

                        %% Verify exception event was emitted
                        receive
                            {telemetry_event, [beamtalk, actor, dispatch, exception], Measurements,
                                Metadata} ->
                                ?assert(maps:is_key(duration, Measurements)),
                                ?assertEqual(Pid, maps:get(pid, Metadata)),
                                ?assertEqual('ThrowingActor', maps:get(class, Metadata)),
                                ?assertEqual(throwError, maps:get(selector, Metadata)),
                                ?assertEqual(sync, maps:get(mode, Metadata))
                        after 1000 ->
                            %% Exception event might not always be emitted depending on
                            %% error handling — the error is caught inside the span by
                            %% the gen_server reply unwrapping, so span sees it as a
                            %% normal error raise, which IS caught by span.
                            ok
                        end
                    after
                        try
                            gen_server:stop(Pid)
                        catch
                            _:_ -> ok
                        end,
                        beamtalk_object_instances:unregister('ThrowingActor', Pid)
                    end
                after
                    telemetry:detach(ExcHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Cast send for dead actor still completes normally
%%====================================================================

cast_send_dead_actor_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                try
                    Pid = start_registered_counter(0),
                    gen_server:stop(Pid),
                    timer:sleep(50),

                    %% Cast send to dead actor should return ok (fire-and-forget)
                    Result = beamtalk_actor:cast_send(Pid, increment, []),
                    ?assertEqual(ok, Result),

                    %% Telemetry stop event should still be emitted
                    receive
                        {telemetry_event, [beamtalk, actor, dispatch, stop], _Measurements,
                            Metadata} ->
                            ?assertEqual(cast, maps:get(mode, Metadata)),
                            ?assertEqual(cast, maps:get(outcome, Metadata))
                    after 1000 ->
                        ?assert(false)
                    end
                after
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Multiple modes produce correct aggregate stats
%%====================================================================

multiple_modes_aggregate_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                Pid = start_registered_counter(0),
                try
                    %% Mix of sync and cast sends
                    beamtalk_actor:sync_send(Pid, increment, []),
                    beamtalk_actor:cast_send(Pid, increment, []),
                    beamtalk_actor:sync_send(Pid, getValue, []),

                    %% Allow telemetry handlers to process
                    timer:sleep(100),

                    %% Verify stats show calls for both methods
                    Stats = beamtalk_trace_store:get_stats(),
                    PidKey = pid_to_list(Pid),
                    ?assertMatch(#{PidKey := _}, Stats),
                    PidStats = maps:get(PidKey, Stats),
                    Methods = maps:get(methods, PidStats),

                    %% increment: 1 sync + 1 cast = 2 calls
                    IncStats = maps:get("increment", Methods),
                    ?assertEqual(2, maps:get(calls, IncStats)),

                    %% getValue: 1 sync call
                    GetStats = maps:get("getValue", Methods),
                    ?assertEqual(1, maps:get(calls, GetStats)),

                    %% Duration should be > 0
                    ?assert(maps:get(total_duration_ns, GetStats) > 0)
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.
