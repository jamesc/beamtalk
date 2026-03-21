%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Telemetry validation spike tests (ADR 0069 Phase 0).
%%%
%%% Proves core assumptions about the telemetry library before committing
%%% to the full actor observability architecture:
%%%
%%% 1. telemetry:span/3 emits stop events with correct duration
%%% 2. telemetry:span/3 exception events preserve exit:{timeout, _} shape
%%% 3. telemetry handlers can increment counters:add/3 from the calling process
%%% 4. beamtalk_object_instances reverse lookup (Pid -> Class) works with measured overhead
%%% 5. Microbenchmark: telemetry:span/3 + counters:add/3 overhead
%%% 6. telemetry_poller emits periodic VM stats as telemetry events
%%% 7. telemetry/try-catch composition with sync_send/3 pattern
-module(beamtalk_telemetry_spike_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% @doc Ensure the telemetry application is started.
ensure_telemetry() ->
    _ = application:ensure_all_started(telemetry),
    ok.

%% @doc Start a minimal gen_server for testing telemetry:span/3 wrapping.
-spec start_echo_server() -> pid().
start_echo_server() ->
    {ok, Pid} = gen_server:start(
        beamtalk_telemetry_spike_echo_server,
        [],
        []
    ),
    Pid.

%% @doc Attach a telemetry handler and return a unique handler ID.
attach_handler(EventName, DestPid) ->
    HandlerId = {telemetry_spike, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        EventName,
        fun(_Event, Measurements, Metadata, #{dest := Dest}) ->
            Dest ! {telemetry_event, Measurements, Metadata}
        end,
        #{dest => DestPid}
    ),
    HandlerId.

%% @doc Detach a telemetry handler, ignoring errors.
detach_handler(HandlerId) ->
    telemetry:detach(HandlerId).

%%====================================================================
%% Test 1: telemetry:span/3 emits stop event with correct duration
%%====================================================================

span_emits_stop_event_with_duration_test() ->
    ensure_telemetry(),
    HandlerId = attach_handler(
        [beamtalk, spike, stop_test, stop],
        self()
    ),
    try
        Pid = start_echo_server(),
        try
            %% Wrap a gen_server:call with telemetry:span/3
            Metadata = #{selector => echo, mode => sync},
            %% telemetry:span/3 returns just the first element of the fun's return tuple
            Result = telemetry:span(
                [beamtalk, spike, stop_test],
                Metadata,
                fun() ->
                    Reply = gen_server:call(Pid, {echo, hello}),
                    {Reply, Metadata#{outcome => ok}}
                end
            ),
            ?assertEqual({ok, hello}, Result),

            %% Verify the stop event was received
            receive
                {telemetry_event, Measurements, RecvMeta} ->
                    %% Duration must be present and positive
                    Duration = maps:get(duration, Measurements),
                    ?assert(is_integer(Duration)),
                    ?assert(Duration > 0),
                    %% Monotonic time should be present
                    ?assert(maps:is_key(monotonic_time, Measurements)),
                    %% Our metadata should be preserved
                    ?assertEqual(echo, maps:get(selector, RecvMeta)),
                    ?assertEqual(ok, maps:get(outcome, RecvMeta))
            after 1000 ->
                ?assert(false)
            end
        after
            gen_server:stop(Pid)
        end
    after
        detach_handler(HandlerId)
    end.

%%====================================================================
%% Test 2: telemetry:span/3 exception preserves exit:{timeout, _} shape
%%====================================================================

span_exception_preserves_timeout_shape_test() ->
    ensure_telemetry(),
    HandlerId = attach_handler(
        [beamtalk, spike, exc_test, exception],
        self()
    ),
    try
        Pid = start_echo_server(),
        try
            Metadata = #{selector => slow, mode => sync},

            %% The gen_server:call with a 1ms timeout should exit with {timeout, _}
            CaughtExit =
                try
                    telemetry:span(
                        [beamtalk, spike, exc_test],
                        Metadata,
                        fun() ->
                            %% Ask the echo server to sleep for 500ms, but timeout after 1ms
                            Reply = gen_server:call(Pid, {sleep, 500}, 1),
                            {Reply, Metadata#{outcome => ok}}
                        end
                    ),
                    no_exception
                catch
                    exit:{timeout, _} = ExitReason ->
                        {got_exit, ExitReason}
                end,

            %% Verify the exit was re-raised with the original shape
            ?assertMatch({got_exit, {timeout, _}}, CaughtExit),

            %% Verify the exception event was emitted
            receive
                {telemetry_event, Measurements, RecvMeta} ->
                    %% Duration must be present
                    ?assert(maps:is_key(duration, Measurements)),
                    %% Exception metadata: kind, reason, stacktrace are merged into
                    %% the StartMetadata by telemetry:span/3
                    ?assertEqual(exit, maps:get(kind, RecvMeta)),
                    ?assertMatch({timeout, _}, maps:get(reason, RecvMeta)),
                    ?assert(is_list(maps:get(stacktrace, RecvMeta)))
            after 1000 ->
                ?assert(false)
            end
        after
            gen_server:stop(Pid)
        end
    after
        detach_handler(HandlerId)
    end.

%%====================================================================
%% Test 3: telemetry handler can increment counters:add/3 from calling process
%%====================================================================

handler_increments_counters_test() ->
    ensure_telemetry(),
    %% Create a counters ref with 2 slots: [call_count, total_duration]
    CounterRef = counters:new(2, [write_concurrency]),

    HandlerId = {telemetry_spike_counter, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        [beamtalk, spike, counter, stop],
        fun(_Event, Measurements, _Metadata, #{counter_ref := Ref}) ->
            %% Increment call count
            counters:add(Ref, 1, 1),
            %% Add duration to total
            Duration = maps:get(duration, Measurements),
            counters:add(Ref, 2, Duration)
        end,
        #{counter_ref => CounterRef}
    ),
    try
        Pid = start_echo_server(),
        try
            %% Make 5 calls, each wrapped in telemetry:span
            lists:foreach(
                fun(_) ->
                    telemetry:span(
                        [beamtalk, spike, counter],
                        #{},
                        fun() ->
                            Reply = gen_server:call(Pid, {echo, ok}),
                            {Reply, #{}}
                        end
                    )
                end,
                lists:seq(1, 5)
            ),

            %% Verify counters
            ?assertEqual(5, counters:get(CounterRef, 1)),
            TotalDuration = counters:get(CounterRef, 2),
            ?assert(TotalDuration > 0)
        after
            gen_server:stop(Pid)
        end
    after
        telemetry:detach(HandlerId)
    end.

%%====================================================================
%% Test 4: beamtalk_object_instances reverse lookup (Pid -> Class)
%%====================================================================

pid_to_class_reverse_lookup_test() ->
    %% Ensure the instance registry is running
    case whereis(beamtalk_object_instances) of
        undefined ->
            {ok, _} = beamtalk_object_instances:start_link();
        _ ->
            ok
    end,

    %% Spawn a process to act as an actor
    Self = self(),
    ActorPid = spawn(fun() ->
        Self ! ready,
        receive
            stop -> ok
        end
    end),
    receive
        ready -> ok
    end,

    %% Register it as a 'Counter' instance
    ok = beamtalk_object_instances:register('Counter', ActorPid),
    try
        %% Reverse lookup: find class by Pid using ets:match on the bag table
        Matches = ets:match(beamtalk_instance_registry, {'$1', ActorPid}),
        Classes = [Class || [Class] <- Matches],
        ?assertEqual(['Counter'], Classes),

        %% Measure overhead of reverse lookup (should be < 1us typically)
        {Time, _Result} = timer:tc(fun() ->
            lists:foreach(
                fun(_) ->
                    ets:match(beamtalk_instance_registry, {'$1', ActorPid})
                end,
                lists:seq(1, 1000)
            )
        end),
        AvgUs = Time / 1000,
        %% Log the overhead for review — use ~w for portable float formatting
        io:format(user, "Pid->Class reverse lookup overhead: ~w us/call~n", [AvgUs]),
        %% Should be well under 10us per call (ETS single-key match)
        ?assert(AvgUs < 10.0)
    after
        beamtalk_object_instances:unregister('Counter', ActorPid),
        ActorPid ! stop
    end.

%% Test that unknown Pid returns empty (no crash)
pid_to_class_unknown_pid_test() ->
    case whereis(beamtalk_object_instances) of
        undefined ->
            {ok, _} = beamtalk_object_instances:start_link();
        _ ->
            ok
    end,

    %% Lookup a pid that was never registered
    FakePid = spawn(fun() -> ok end),
    timer:sleep(10),
    Matches = ets:match(beamtalk_instance_registry, {'$1', FakePid}),
    ?assertEqual([], Matches).

%%====================================================================
%% Test 5: Microbenchmark — telemetry:span/3 + counters:add/3 overhead
%%====================================================================

microbenchmark_span_plus_counters_test() ->
    ensure_telemetry(),
    CounterRef = counters:new(2, [write_concurrency]),

    HandlerId = {telemetry_spike_bench, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        [beamtalk, spike, bench, stop],
        fun(_Event, Measurements, _Metadata, #{counter_ref := Ref}) ->
            counters:add(Ref, 1, 1),
            counters:add(Ref, 2, maps:get(duration, Measurements))
        end,
        #{counter_ref => CounterRef}
    ),
    try
        Iterations = 100000,

        %% Baseline: just counters:add/3 in a tight loop
        {BaselineUs, _} = timer:tc(fun() ->
            BaseRef = counters:new(1, [write_concurrency]),
            lists:foreach(
                fun(_) -> counters:add(BaseRef, 1, 1) end,
                lists:seq(1, Iterations)
            )
        end),
        BaselineNsPerOp = (BaselineUs * 1000) / Iterations,

        %% Measured: telemetry:span/3 wrapping a no-op + counters handler
        {MeasuredUs, _} = timer:tc(fun() ->
            lists:foreach(
                fun(_) ->
                    telemetry:span(
                        [beamtalk, spike, bench],
                        #{},
                        fun() -> {ok, #{}} end
                    )
                end,
                lists:seq(1, Iterations)
            )
        end),
        MeasuredNsPerOp = (MeasuredUs * 1000) / Iterations,

        %% The overhead of telemetry:span + handler
        OverheadNs = MeasuredNsPerOp - BaselineNsPerOp,

        %% Log results for review — use ~w for portable float formatting
        io:format(
            user,
            "~nMicrobenchmark results (~w iterations):~n"
            "  Baseline (counters:add):     ~w ns/op~n"
            "  Measured (span+counters):    ~w ns/op~n"
            "  Overhead (span+handler):     ~w ns/op~n"
            "  Scheduler count:             ~w~n",
            [
                Iterations,
                round(BaselineNsPerOp),
                round(MeasuredNsPerOp),
                round(OverheadNs),
                erlang:system_info(schedulers)
            ]
        ),

        %% Verify counters were actually incremented
        ?assertEqual(Iterations, counters:get(CounterRef, 1)),

        %% ADR 0069 budget: ~150ns for counters:add/3 (always-on aggregates).
        %% telemetry:span/3 overhead (~1-5us) is higher due to ETS handler lookups +
        %% 2x telemetry:execute + monotonic_time calls. This matches the ADR's estimate
        %% of "~1-5us for telemetry dispatch." In CI/virtualized environments variance
        %% is high, so we use a generous upper bound and log the actual value for review.
        ?assert(OverheadNs < 10000)
    after
        telemetry:detach(HandlerId)
    end.

%%====================================================================
%% Test 6: telemetry_poller emits periodic VM stats
%%====================================================================

telemetry_poller_emits_vm_stats_test() ->
    ensure_telemetry(),
    Self = self(),

    %% Attach handler for the VM memory event that telemetry_poller emits
    HandlerId = {telemetry_spike_poller, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        [vm, memory],
        fun(_Event, Measurements, Metadata, #{dest := Dest}) ->
            Dest ! {vm_memory, Measurements, Metadata}
        end,
        #{dest => Self}
    ),
    try
        %% Start a telemetry_poller with a fast period (100ms) for testing
        {ok, PollerPid} = telemetry_poller:start_link([
            {measurements, [memory]},
            {period, 100}
        ]),
        try
            %% Wait for at least one event
            receive
                {vm_memory, Measurements, _Metadata} ->
                    %% Should have total, processes, system etc.
                    ?assert(maps:is_key(total, Measurements)),
                    ?assert(maps:is_key(processes, Measurements)),
                    ?assert(maps:is_key(system, Measurements)),
                    Total = maps:get(total, Measurements),
                    ?assert(is_integer(Total)),
                    ?assert(Total > 0)
            after 2000 ->
                ?assert(false)
            end
        after
            gen_server:stop(PollerPid)
        end
    after
        telemetry:detach(HandlerId)
    end.

%%====================================================================
%% Test 7: Fallback plan documentation — telemetry/try-catch composition
%%====================================================================

%% This test verifies that if telemetry:span/3 is used around code that
%% does its own try/catch (like sync_send/3), the error handling composes
%% correctly. This is the critical assumption for ADR 0069.
span_composes_with_outer_try_catch_test() ->
    ensure_telemetry(),
    HandlerId = attach_handler(
        [beamtalk, spike, compose, exception],
        self()
    ),
    try
        Pid = start_echo_server(),
        try
            %% Simulate sync_send/3 pattern: outer try/catch wraps telemetry:span
            Result =
                try
                    telemetry:span(
                        [beamtalk, spike, compose],
                        #{selector => slow},
                        fun() ->
                            Reply = gen_server:call(Pid, {sleep, 500}, 1),
                            {Reply, #{outcome => ok}}
                        end
                    )
                catch
                    exit:{timeout, _} ->
                        %% This is what sync_send/3 does — catches the exit and
                        %% converts to a structured beamtalk_error.
                        {caught_timeout, ok}
                end,

            %% Verify the outer catch got the original exit shape
            ?assertEqual({caught_timeout, ok}, Result),

            %% Verify telemetry still emitted the exception event
            receive
                {telemetry_event, Measurements, RecvMeta} ->
                    ?assert(maps:is_key(duration, Measurements)),
                    ?assertEqual(exit, maps:get(kind, RecvMeta)),
                    ?assertMatch({timeout, _}, maps:get(reason, RecvMeta))
            after 1000 ->
                ?assert(false)
            end
        after
            gen_server:stop(Pid)
        end
    after
        detach_handler(HandlerId)
    end.
