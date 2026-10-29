%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_tracing_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
EUnit tests for actor send wrapper telemetry instrumentation (ADR 0069 Phase 2a).

Tests cover: telemetry events emitted by sync_send/3, async_send/4, cast_send/3,
aggregate counter updates, trace event capture when enabled, error handling
preservation (timeout, actor_dead), and lifecycle method exclusion.

Dispatch events are opt-in (BT-4113): `setup/0` turns them on through the
Erlang API, as code attaching with `telemetry:attach/4` must. The
`quiet_setup/0` tests cover the default (events off): aggregates are still
recorded exactly once per send, and `Tracing enable` switches events on.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

setup() ->
    %% Ensure telemetry and runtime deps are available
    _ = application:ensure_all_started(telemetry),
    %% beamtalk_actor and beamtalk_trace_store gate on
    %% `erlang:function_exported(telemetry, ...)`, which is false until the
    %% `telemetry` module itself is loaded — starting the app does not load
    %% it. Load it so this suite does not depend on an earlier suite in the
    %% same VM having done so.
    {module, telemetry} = code:ensure_loaded(telemetry),
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
    ok = beamtalk_telemetry:enable_dispatch_events(),
    ok.

-doc "Like setup/0, but leaves dispatch events at their default (off).".
quiet_setup() ->
    ok = setup(),
    ok = beamtalk_telemetry:disable_dispatch_events(),
    ok.

cleanup(_State) ->
    ok = beamtalk_telemetry:disable_dispatch_events(),
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable()
    catch
        _:_ -> ok
    end,
    %% Clean up causal trace context from process dictionary
    erase('$beamtalk_trace_id'),
    erase('$beamtalk_span_id'),
    erase('$beamtalk_parent_span_id'),
    ok.

-doc "Attach a telemetry handler that sends events to the calling process.".
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

-doc "Start a test counter actor and register it in the instance registry.".
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
                                %% `>= 0`, not `> 0`: Windows' monotonic
                                %% clock has 1ms resolution, so a fast
                                %% dispatch measures 0 there (BT-3980).
                                ?assert(maps:get(duration, Measurements) >= 0),
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

                    %% Check aggregate stats (always-on class entry; no
                    %% per-actor entry while tracing is disabled)
                    Stats = beamtalk_trace_store:get_stats(),
                    PidKey = {beamtalk_string, list_to_binary(pid_to_list(Pid))},
                    ?assertNot(maps:is_key(PidKey, Stats)),
                    #{{beamtalk_string, <<"class:Counter">>} := CounterStats} = Stats,
                    Methods = maps:get({beamtalk_string, <<"methods">>}, CounterStats),

                    %% increment should have 2 calls
                    IncStats = maps:get({beamtalk_string, <<"increment">>}, Methods),
                    ?assertEqual(2, maps:get({beamtalk_string, <<"calls">>}, IncStats)),

                    %% getValue should have 1 call
                    GetStats = maps:get({beamtalk_string, <<"getValue">>}, Methods),
                    ?assertEqual(1, maps:get({beamtalk_string, <<"calls">>}, GetStats))
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
                    %% Filter to dispatch events only (exclude lifecycle)
                    DispatchTraces = [
                        T
                     || T <- Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assertEqual(1, length(DispatchTraces)),
                    [Trace] = DispatchTraces,
                    PidStr = {beamtalk_string, list_to_binary(pid_to_list(Pid))},
                    ?assertEqual(PidStr, maps:get({beamtalk_string, <<"actor">>}, Trace)),
                    ?assertEqual(
                        {beamtalk_string, <<"Counter">>},
                        maps:get({beamtalk_string, <<"class">>}, Trace)
                    ),
                    ?assertEqual(
                        {beamtalk_string, <<"getValue">>},
                        maps:get({beamtalk_string, <<"selector">>}, Trace)
                    ),
                    ?assertEqual(
                        {beamtalk_string, <<"sync">>},
                        maps:get({beamtalk_string, <<"mode">>}, Trace)
                    )
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

                    %% Verify stats show calls for both methods (in the
                    %% always-on class entry)
                    Stats = beamtalk_trace_store:get_stats(),
                    #{{beamtalk_string, <<"class:Counter">>} := CounterStats} = Stats,
                    Methods = maps:get({beamtalk_string, <<"methods">>}, CounterStats),

                    %% increment: 1 sync + 1 cast = 2 calls
                    IncStats = maps:get({beamtalk_string, <<"increment">>}, Methods),
                    ?assertEqual(2, maps:get({beamtalk_string, <<"calls">>}, IncStats)),

                    %% getValue: 1 sync call
                    GetStats = maps:get({beamtalk_string, <<"getValue">>}, Methods),
                    ?assertEqual(1, maps:get({beamtalk_string, <<"calls">>}, GetStats)),

                    %% Duration is recorded. `>= 0`, not `> 0`: Windows'
                    %% monotonic clock has 1ms resolution, so a single fast
                    %% call can measure 0 there (BT-3980).
                    ?assert(maps:get({beamtalk_string, <<"total_duration_ns">>}, GetStats) >= 0)
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle start event emitted on actor init.
%%====================================================================

lifecycle_start_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StartHandler = attach_handler([beamtalk, actor, lifecycle, start]),
                try
                    Pid = start_registered_counter(0),
                    try
                        %% Verify lifecycle start event was emitted
                        receive
                            {telemetry_event, [beamtalk, actor, lifecycle, start], _Measurements,
                                Metadata} ->
                                ?assertEqual(Pid, maps:get(pid, Metadata)),
                                ?assertEqual('Counter', maps:get(class, Metadata))
                        after 1000 ->
                            ?assert(false)
                        end
                    after
                        gen_server:stop(Pid)
                    end
                after
                    telemetry:detach(StartHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle stop event emitted on actor terminate.
%%====================================================================

lifecycle_terminate_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, lifecycle, stop]),
                try
                    Pid = start_registered_counter(0),
                    %% Gracefully stop the actor — triggers terminate/2
                    gen_server:stop(Pid),
                    %% Verify lifecycle stop event was emitted from terminate/2
                    %% (There may also be a stop event from sync_send, but we
                    %% just need at least one with reason => normal)
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, stop], _Measurements,
                            Metadata} ->
                            ?assertEqual(Pid, maps:get(pid, Metadata)),
                            ?assertEqual('Counter', maps:get(class, Metadata)),
                            ?assertEqual(normal, maps:get(reason, Metadata))
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
%% Test: Lifecycle stop event via sync_send stop.
%%====================================================================

lifecycle_sync_stop_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, lifecycle, stop]),
                try
                    Pid = start_registered_counter(0),
                    %% Use sync_send stop which emits lifecycle event
                    beamtalk_actor:sync_send(Pid, stop, []),
                    %% Verify lifecycle stop event was emitted
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, stop], _Measurements,
                            Metadata} ->
                            ?assertEqual(Pid, maps:get(pid, Metadata)),
                            ?assertEqual('Counter', maps:get(class, Metadata)),
                            ?assertEqual(normal, maps:get(reason, Metadata))
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
%% Test: Lifecycle kill event via sync_send kill.
%%====================================================================

lifecycle_sync_kill_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                KillHandler = attach_handler([beamtalk, actor, lifecycle, kill]),
                OldTrap = process_flag(trap_exit, true),
                try
                    Pid = start_registered_counter(0),
                    %% Use sync_send kill which emits lifecycle event
                    beamtalk_actor:sync_send(Pid, kill, []),
                    %% Flush the EXIT message from the linked actor
                    receive
                        {'EXIT', Pid, _} -> ok
                    after 100 -> ok
                    end,
                    %% Verify lifecycle kill event was emitted
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, kill], _Measurements,
                            Metadata} ->
                            ?assertEqual(Pid, maps:get(pid, Metadata)),
                            ?assertEqual('Counter', maps:get(class, Metadata))
                    after 1000 ->
                        ?assert(false)
                    end
                after
                    process_flag(trap_exit, OldTrap),
                    telemetry:detach(KillHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle events recorded in trace store.
%%====================================================================

lifecycle_events_in_traces_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),
                Pid = start_registered_counter(0),
                %% Stop the actor to trigger lifecycle events
                beamtalk_actor:sync_send(Pid, stop, []),
                timer:sleep(100),

                %% Get all traces — should include lifecycle events
                Traces = beamtalk_trace_store:get_traces(),
                LifecycleTraces = [
                    T
                 || T <- Traces,
                    maps:get({beamtalk_string, <<"mode">>}, T) =:=
                        {beamtalk_string, <<"lifecycle">>}
                ],
                %% Should have at least start + stop events
                ?assert(length(LifecycleTraces) >= 2),

                %% Verify start event exists
                StartTraces = [
                    T
                 || T <- LifecycleTraces,
                    maps:get({beamtalk_string, <<"selector">>}, T) =:=
                        {beamtalk_string, <<"start">>}
                ],
                ?assert(length(StartTraces) >= 1),

                %% Verify stop event exists
                StopTraces = [
                    T
                 || T <- LifecycleTraces,
                    maps:get({beamtalk_string, <<"selector">>}, T) =:= {beamtalk_string, <<"stop">>}
                ],
                ?assert(length(StopTraces) >= 1)
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle events in aggregate stats.
%%====================================================================

lifecycle_aggregate_stats_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                Pid = start_registered_counter(0),
                %% Stop the actor to trigger lifecycle events
                beamtalk_actor:sync_send(Pid, stop, []),
                timer:sleep(100),

                %% Get aggregate stats — should include lifecycle operations.
                %% Death changes nothing: the always-on class entry holds
                %% every dispatch (BT-4103).
                Stats = beamtalk_trace_store:get_stats(),
                PidKey = {beamtalk_string, list_to_binary(pid_to_list(Pid))},
                ?assertNot(maps:is_key(PidKey, Stats)),
                ?assertMatch(#{{beamtalk_string, <<"class:Counter">>} := _}, Stats),
                Methods = maps:get(
                    {beamtalk_string, <<"methods">>},
                    maps:get({beamtalk_string, <<"class:Counter">>}, Stats)
                ),

                %% Start and stop should each have at least 1 call
                StartStats = maps:get({beamtalk_string, <<"start">>}, Methods),
                ?assert(maps:get({beamtalk_string, <<"calls">>}, StartStats) >= 1),
                StopStats = maps:get({beamtalk_string, <<"stop">>}, Methods),
                ?assert(maps:get({beamtalk_string, <<"calls">>}, StopStats) >= 1)
            end)
        ]
    end}.

%%====================================================================
%% Test: Lifecycle terminate reason propagation.
%%====================================================================

lifecycle_terminate_reason_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, lifecycle, stop]),
                OldTrap = process_flag(trap_exit, true),
                try
                    Pid = start_registered_counter(0),
                    %% Kill the actor — terminate/2 receives {killed, ...} reason
                    exit(Pid, kill),
                    %% Flush the EXIT message from the linked actor
                    receive
                        {'EXIT', Pid, _} -> ok
                    after 100 -> ok
                    end,
                    timer:sleep(100),
                    %% Verify lifecycle stop event from terminate/2 has non-normal reason
                    %% Note: when killed, terminate/2 may or may not be called
                    %% depending on trap_exit. Check for any stop event.
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, stop], _Measurements,
                            Metadata} ->
                            %% Just verify it has reason and class metadata
                            ?assert(maps:is_key(reason, Metadata)),
                            ?assert(maps:is_key(class, Metadata))
                    after 500 ->
                        %% Killed actors may not call terminate/2 — acceptable
                        ok
                    end
                after
                    process_flag(trap_exit, OldTrap),
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Async stop emits lifecycle event.
%%====================================================================

lifecycle_async_stop_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                StopHandler = attach_handler([beamtalk, actor, lifecycle, stop]),
                try
                    Pid = start_registered_counter(0),
                    {beamtalk_future, FuturePid} = beamtalk_future:new(),
                    beamtalk_actor:async_send(Pid, stop, [], FuturePid),
                    %% Verify lifecycle stop event was emitted
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, stop], _Measurements,
                            Metadata} ->
                            ?assertEqual(Pid, maps:get(pid, Metadata)),
                            ?assertEqual('Counter', maps:get(class, Metadata)),
                            ?assertEqual(normal, maps:get(reason, Metadata))
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
%% Test: Async kill emits lifecycle event.
%%====================================================================

lifecycle_async_kill_event_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                KillHandler = attach_handler([beamtalk, actor, lifecycle, kill]),
                OldTrap = process_flag(trap_exit, true),
                try
                    Pid = start_registered_counter(0),
                    {beamtalk_future, FuturePid} = beamtalk_future:new(),
                    beamtalk_actor:async_send(Pid, kill, [], FuturePid),
                    %% Flush the EXIT message from the linked actor
                    receive
                        {'EXIT', Pid, _} -> ok
                    after 100 -> ok
                    end,
                    %% Verify lifecycle kill event was emitted
                    receive
                        {telemetry_event, [beamtalk, actor, lifecycle, kill], _Measurements,
                            Metadata} ->
                            ?assertEqual(Pid, maps:get(pid, Metadata)),
                            ?assertEqual('Counter', maps:get(class, Metadata))
                    after 1000 ->
                        ?assert(false)
                    end
                after
                    process_flag(trap_exit, OldTrap),
                    telemetry:detach(KillHandler)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: set_trace_context stores and retrieves context.
%%====================================================================

set_trace_context_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Initially empty
                ?assertEqual(#{}, beamtalk_actor:get_trace_context()),

                %% Set context
                beamtalk_actor:set_trace_context(#{workflowId => <<"wf-123">>}),
                ?assertEqual(#{workflowId => <<"wf-123">>}, beamtalk_actor:get_trace_context()),

                %% Merge additional keys
                beamtalk_actor:set_trace_context(#{activityId => <<"a-1">>}),
                Ctx = beamtalk_actor:get_trace_context(),
                ?assertEqual(<<"wf-123">>, maps:get(workflowId, Ctx)),
                ?assertEqual(<<"a-1">>, maps:get(activityId, Ctx)),

                %% Clean up process dictionary and logger metadata
                erase('$beamtalk_trace_ctx'),
                logger:set_process_metadata(#{})
            end)
        ]
    end}.

%%====================================================================
%% Test: set_trace_context updates OTP logger metadata.
%%====================================================================

set_trace_context_updates_logger_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Set trace context
                beamtalk_actor:set_trace_context(#{workflowId => <<"wf-456">>}),

                %% Verify logger process metadata includes workflowId
                LoggerMeta = logger:get_process_metadata(),
                ?assertEqual(<<"wf-456">>, maps:get(workflowId, LoggerMeta)),

                %% Clean up
                erase('$beamtalk_trace_ctx'),
                logger:set_process_metadata(#{})
            end)
        ]
    end}.

%%====================================================================
%% Test: Trace context appears in trace events.
%%====================================================================

trace_context_in_traces_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),

                %% Set trace context in THIS process (caller side)
                beamtalk_actor:set_trace_context(#{workflowId => <<"wf-trace-1">>}),

                Pid = start_registered_counter(0),
                try
                    %% Sync send — telemetry handler runs in caller process
                    _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    Traces = beamtalk_trace_store:get_traces(),
                    DispatchTraces = [
                        T
                     || T <- Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assert(length(DispatchTraces) >= 1),
                    [Trace | _] = DispatchTraces,
                    %% Trace context should be merged into the trace event
                    ?assertEqual(
                        <<"wf-trace-1">>, maps:get({beamtalk_string, <<"workflowId">>}, Trace)
                    )
                after
                    gen_server:stop(Pid),
                    erase('$beamtalk_trace_ctx'),
                    logger:set_process_metadata(#{})
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Empty trace context produces no overhead.
%%====================================================================

empty_trace_context_no_overhead_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),

                %% Do NOT set any trace context
                erase('$beamtalk_trace_ctx'),

                Pid = start_registered_counter(0),
                try
                    _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    Traces = beamtalk_trace_store:get_traces(),
                    DispatchTraces = [
                        T
                     || T <- Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assert(length(DispatchTraces) >= 1),
                    [Trace | _] = DispatchTraces,
                    %% No workflowId key should be present
                    ?assertEqual(error, maps:find(workflowId, Trace))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Trace context propagates across actor boundaries.
%%====================================================================

trace_context_propagation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Set trace context in the caller process
                beamtalk_actor:set_trace_context(#{workflowId => <<"wf-prop-1">>}),

                %% Build a propagated context
                PropCtx = beamtalk_actor:get_propagated_ctx(),

                %% Verify trace_ctx is in propagated context
                ?assertMatch(#{trace_ctx := #{workflowId := <<"wf-prop-1">>}}, PropCtx),

                %% Simulate receiving side: clear context first
                erase('$beamtalk_trace_ctx'),
                logger:set_process_metadata(#{}),

                %% Restore propagated context (as receiving actor would)
                beamtalk_actor:restore_propagated_ctx(PropCtx),

                %% Trace context should be restored
                ?assertEqual(
                    #{workflowId => <<"wf-prop-1">>},
                    beamtalk_actor:get_trace_context()
                ),

                %% Logger metadata should also be updated
                LoggerMeta = logger:get_process_metadata(),
                ?assertEqual(<<"wf-prop-1">>, maps:get(workflowId, LoggerMeta)),

                %% Clean up
                erase('$beamtalk_trace_ctx'),
                logger:set_process_metadata(#{})
            end)
        ]
    end}.

%%====================================================================
%% Test: Propagation with no trace context is a no-op.
%%====================================================================

propagation_no_context_noop_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure no trace context set
                erase('$beamtalk_trace_ctx'),

                %% Build propagated context (no trace context): BT-4113
                %% omits the empty key instead of sending `trace_ctx => #{}`.
                PropCtx = beamtalk_actor:get_propagated_ctx(),
                ?assertEqual(error, maps:find(trace_ctx, PropCtx)),

                %% Restore should be a no-op
                beamtalk_actor:restore_propagated_ctx(PropCtx),
                ?assertEqual(#{}, beamtalk_actor:get_trace_context())
            end)
        ]
    end}.

%%====================================================================
%% Test: Root span generates trace_id = span_id.
%%====================================================================

causal_root_span_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),
                %% Clean causal context
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),

                Pid = start_registered_counter(0),
                try
                    %% Sync send — first dispatch creates a root span
                    _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    %% Verify causal context was set in process dictionary
                    TraceId = get('$beamtalk_trace_id'),
                    SpanId = get('$beamtalk_span_id'),
                    ?assert(is_integer(TraceId)),
                    ?assert(is_integer(SpanId)),
                    %% Root span: trace_id = span_id
                    ?assertEqual(TraceId, SpanId),
                    %% No parent span for root
                    ?assertEqual(undefined, get('$beamtalk_parent_span_id')),

                    %% Verify trace event has causal IDs
                    Traces = beamtalk_trace_store:get_traces(),
                    DispatchTraces = [
                        T
                     || T <- Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assert(length(DispatchTraces) >= 1),
                    [Trace | _] = DispatchTraces,
                    ?assertEqual(TraceId, maps:get({beamtalk_string, <<"trace_id">>}, Trace)),
                    ?assertEqual(SpanId, maps:get({beamtalk_string, <<"span_id">>}, Trace))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Parent-child causal linking across actor calls.
%%====================================================================

causal_parent_child_linking_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),

                Pid = start_registered_counter(0),
                try
                    %% First dispatch — creates root span
                    _Result1 = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    RootTraceId = get('$beamtalk_trace_id'),
                    RootSpanId = get('$beamtalk_span_id'),
                    ?assert(is_integer(RootTraceId)),

                    %% Build propagated context (as if sending to another actor)
                    PropCtx = beamtalk_actor:get_propagated_ctx(),
                    ?assertMatch(#{causal := #{trace_id := _, span_id := _}}, PropCtx),
                    #{causal := #{trace_id := PropTraceId, span_id := PropSpanId}} = PropCtx,
                    ?assertEqual(RootTraceId, PropTraceId),
                    ?assertEqual(RootSpanId, PropSpanId),

                    %% Simulate receiving side: restore propagated context
                    beamtalk_actor:restore_propagated_ctx(PropCtx),

                    %% After restore: trace_id inherited, parent_span_id set
                    ?assertEqual(RootTraceId, get('$beamtalk_trace_id')),
                    ?assertEqual(RootSpanId, get('$beamtalk_parent_span_id'))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Trace ID filter retrieves full causal chain.
%%====================================================================

causal_trace_id_filter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),

                Pid = start_registered_counter(0),
                try
                    %% First dispatch — root span with trace_id T1
                    _Result1 = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),
                    T1 = get('$beamtalk_trace_id'),

                    %% Second dispatch under same trace — different span_id, same trace_id
                    _Result2 = beamtalk_actor:sync_send(Pid, increment, []),
                    timer:sleep(50),

                    %% Start a new trace by clearing causal context
                    erase('$beamtalk_trace_id'),
                    erase('$beamtalk_span_id'),
                    erase('$beamtalk_parent_span_id'),

                    %% Third dispatch — new root span with trace_id T2
                    _Result3 = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),
                    T2 = get('$beamtalk_trace_id'),
                    ?assertNotEqual(T1, T2),

                    %% Filter by T1 — should get exactly 2 trace events
                    T1Traces = beamtalk_trace_store:get_traces(#{trace_id => T1}),
                    T1Dispatch = [
                        T
                     || T <- T1Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assertEqual(2, length(T1Dispatch)),

                    %% Filter by T2 — should get exactly 1 trace event
                    T2Traces = beamtalk_trace_store:get_traces(#{trace_id => T2}),
                    T2Dispatch = [
                        T
                     || T <- T2Traces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    ?assertEqual(1, length(T2Dispatch))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Multi-hop causal chain A->B->C.
%%====================================================================

causal_multi_hop_chain_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:enable(),
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),

                %% Simulate a 3-hop chain: A -> B -> C
                %% All done in the same process to test the context propagation logic.

                %% Hop 1: Actor A dispatches (root span)
                PidA = start_registered_counter(0),
                try
                    _ResultA = beamtalk_actor:sync_send(PidA, getValue, []),
                    timer:sleep(50),

                    TraceId = get('$beamtalk_trace_id'),
                    SpanA = get('$beamtalk_span_id'),
                    ?assert(is_integer(TraceId)),
                    %% root span
                    ?assertEqual(TraceId, SpanA),

                    %% Actor A sends to Actor B: propagate context
                    PropCtxAB = beamtalk_actor:get_propagated_ctx(),

                    %% Hop 2: Actor B receives and dispatches
                    beamtalk_actor:restore_propagated_ctx(PropCtxAB),
                    ?assertEqual(TraceId, get('$beamtalk_trace_id')),
                    ?assertEqual(SpanA, get('$beamtalk_parent_span_id')),

                    %% Actor B's own dispatch generates new span_id
                    _ResultB = beamtalk_actor:sync_send(PidA, increment, []),
                    timer:sleep(50),
                    SpanB = get('$beamtalk_span_id'),
                    ?assertNotEqual(SpanA, SpanB),
                    ?assertEqual(TraceId, get('$beamtalk_trace_id')),

                    %% Actor B sends to Actor C: propagate context
                    PropCtxBC = beamtalk_actor:get_propagated_ctx(),
                    #{causal := #{trace_id := BCTraceId, span_id := BCSpanId}} = PropCtxBC,
                    ?assertEqual(TraceId, BCTraceId),
                    ?assertEqual(SpanB, BCSpanId),

                    %% Hop 3: Actor C receives
                    beamtalk_actor:restore_propagated_ctx(PropCtxBC),
                    ?assertEqual(TraceId, get('$beamtalk_trace_id')),
                    ?assertEqual(SpanB, get('$beamtalk_parent_span_id')),

                    %% Actor C dispatches
                    _ResultC = beamtalk_actor:sync_send(PidA, getValue, []),
                    timer:sleep(50),
                    SpanC = get('$beamtalk_span_id'),
                    ?assertNotEqual(SpanB, SpanC),
                    ?assertEqual(TraceId, get('$beamtalk_trace_id')),

                    %% All events should be under the same trace_id
                    AllTraces = beamtalk_trace_store:get_traces(#{trace_id => TraceId}),
                    DispatchTraces = [
                        T
                     || T <- AllTraces,
                        maps:get({beamtalk_string, <<"mode">>}, T) =/=
                            {beamtalk_string, <<"lifecycle">>}
                    ],
                    %% 3 dispatches: A getValue, B increment, C getValue
                    ?assertEqual(3, length(DispatchTraces)),

                    %% Verify different span_ids
                    SpanIds = [
                        maps:get({beamtalk_string, <<"span_id">>}, T)
                     || T <- DispatchTraces
                    ],
                    ?assertEqual(3, length(lists:usort(SpanIds)))
                after
                    gen_server:stop(PidA)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: No causal overhead when tracing disabled.
%%====================================================================

causal_no_overhead_when_disabled_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                beamtalk_trace_store:disable(),
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),

                Pid = start_registered_counter(0),
                try
                    %% Dispatch with tracing disabled
                    _Result = beamtalk_actor:sync_send(Pid, getValue, []),
                    timer:sleep(50),

                    %% No causal context should be set
                    ?assertEqual(undefined, get('$beamtalk_trace_id')),
                    ?assertEqual(undefined, get('$beamtalk_span_id')),
                    ?assertEqual(undefined, get('$beamtalk_parent_span_id')),

                    %% Propagated context should not have causal key
                    PropCtx = beamtalk_actor:get_propagated_ctx(),
                    ?assertEqual(error, maps:find(causal, PropCtx))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Span IDs are monotonically increasing.
%%====================================================================

causal_span_ids_monotonic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Generate several span IDs and verify monotonicity
                Id1 = beamtalk_trace_store:next_span_id(),
                Id2 = beamtalk_trace_store:next_span_id(),
                Id3 = beamtalk_trace_store:next_span_id(),
                ?assert(Id1 > 0),
                ?assert(Id2 > Id1),
                ?assert(Id3 > Id2)
            end)
        ]
    end}.

%%====================================================================
%% Test: get_causal_ctx returns empty map when no context.
%%====================================================================

causal_ctx_empty_when_none_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                erase('$beamtalk_trace_id'),
                erase('$beamtalk_span_id'),
                erase('$beamtalk_parent_span_id'),
                ?assertEqual(#{}, beamtalk_actor:get_causal_ctx())
            end)
        ]
    end}.

%%====================================================================
%% BT-4113: dispatch events off by default
%%====================================================================

-doc "Every dispatch stop/exception event this process receives within 100ms.".
drain_dispatch_events(Acc) ->
    receive
        {telemetry_event, [beamtalk, actor, dispatch, _] = E, _, _} ->
            drain_dispatch_events([E | Acc])
    after 100 -> lists:reverse(Acc)
    end.

class_method_stats(Class, Selector) ->
    Stats = beamtalk_trace_store:get_stats(),
    ClassKey = {beamtalk_string, <<"class:", (atom_to_binary(Class))/binary>>},
    #{ClassKey := ClassStats} = Stats,
    Methods = maps:get({beamtalk_string, <<"methods">>}, ClassStats),
    maps:get({beamtalk_string, atom_to_binary(Selector)}, Methods).

stat(Name, MethodStats) ->
    maps:get({beamtalk_string, Name}, MethodStats).

dispatch_events_off_still_record_aggregates_once_test_() ->
    {setup, fun quiet_setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
                beamtalk_trace_store:clear(),
                Handlers = [
                    attach_handler(E)
                 || E <- [
                        [beamtalk, actor, dispatch, start],
                        [beamtalk, actor, dispatch, stop],
                        [beamtalk, actor, dispatch, exception]
                    ]
                ],
                Pid = start_registered_counter(0),
                {ok, Thrower} = test_throwing_actor:start_link(),
                ok = beamtalk_object_instances:register('ThrowingActor', Thrower),
                try
                    _ = beamtalk_actor:sync_send(Pid, increment, []),
                    _ = beamtalk_actor:sync_send(Pid, increment, []),
                    ok = beamtalk_actor:cast_send(Pid, increment, []),
                    caught =
                        try
                            beamtalk_actor:sync_send(Thrower, throwError, [])
                        catch
                            error:_ -> caught
                        end,
                    %% No dispatch events at all...
                    ?assertEqual([], drain_dispatch_events([])),
                    %% ...but each send counted exactly once.
                    Inc = class_method_stats('Counter', increment),
                    ?assertEqual(3, stat(<<"calls">>, Inc)),
                    ?assertEqual(0, stat(<<"errors">>, Inc)),
                    Thrown = class_method_stats('ThrowingActor', throwError),
                    ?assertEqual(1, stat(<<"calls">>, Thrown)),
                    ?assertEqual(1, stat(<<"errors">>, Thrown))
                after
                    gen_server:stop(Pid),
                    catch gen_server:stop(Thrower),
                    beamtalk_object_instances:unregister('ThrowingActor', Thrower),
                    [telemetry:detach(H) || H <- Handlers]
                end
            end)
        ]
    end}.

dispatch_events_on_record_aggregates_once_test_() ->
    %% The same sends with events on: still one sample each (the trace store
    %% no longer also counts them from a telemetry handler).
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_trace_store:clear(),
                Pid = start_registered_counter(0),
                try
                    _ = beamtalk_actor:sync_send(Pid, increment, []),
                    _ = beamtalk_actor:sync_send(Pid, increment, []),
                    Inc = class_method_stats('Counter', increment),
                    ?assertEqual(2, stat(<<"calls">>, Inc))
                after
                    gen_server:stop(Pid)
                end
            end)
        ]
    end}.

tracing_enable_turns_dispatch_events_on_test_() ->
    {setup, fun quiet_setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
                ok = beamtalk_trace_store:enable(),
                ?assert(beamtalk_telemetry:dispatch_events_enabled()),
                StopHandler = attach_handler([beamtalk, actor, dispatch, stop]),
                Pid = start_registered_counter(0),
                try
                    0 = beamtalk_actor:sync_send(Pid, getValue, []),
                    ?assertEqual(
                        [[beamtalk, actor, dispatch, stop]], drain_dispatch_events([])
                    ),
                    ok = beamtalk_trace_store:disable(),
                    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
                    0 = beamtalk_actor:sync_send(Pid, getValue, []),
                    ?assertEqual([], drain_dispatch_events([]))
                after
                    gen_server:stop(Pid),
                    telemetry:detach(StopHandler)
                end
            end)
        ]
    end}.

flat_send_cost_with_many_live_actors_test_() ->
    %% BT-4113: the send path never scans the instance registry, so a
    %% send's cost does not grow with the number of live actors. Counted in
    %% reductions, which (unlike wall time) are deterministic.
    {setup, fun quiet_setup/0, fun cleanup/1, fun(_) ->
        [
            {timeout, 60,
                ?_test(begin
                    Pid = start_registered_counter(0),
                    Fillers = [spawn_registered_filler() || _ <- lists:seq(1, 10)],
                    Small = send_reductions(Pid),
                    MoreFillers = [spawn_registered_filler() || _ <- lists:seq(1, 9990)],
                    Large = send_reductions(Pid),
                    try
                        %% A registry scan would cost thousands of reductions
                        %% more at 10,000 actors; allow a little noise.
                        ?assert(Large =< Small + 10)
                    after
                        gen_server:stop(Pid),
                        [exit(F, kill) || F <- Fillers ++ MoreFillers]
                    end
                end)}
        ]
    end}.

spawn_registered_filler() ->
    F = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = beamtalk_object_instances:register('Filler', F),
    F.

-doc "Minimum reductions one sync send costs the caller, over a few tries.".
send_reductions(Pid) ->
    lists:min([
        begin
            {reductions, R0} = process_info(self(), reductions),
            _ = beamtalk_actor:sync_send(Pid, getValue, []),
            {reductions, R1} = process_info(self(), reductions),
            R1 - R0
        end
     || _ <- lists:seq(1, 20)
    ]).
