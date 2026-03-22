%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Trace store gen_server for actor observability (ADR 0069 Phase 1).
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Owns ETS tables and counters refs for lock-free aggregate tracking and
%%% opt-in trace event capture. The gen_server handles queries and lifecycle
%%% but is NOT in the write path — all writes happen in the calling process
%%% via counters:add/3 and ets:insert/2.
%%%
%%% Storage layout:
%%% - beamtalk_agg_index (ETS set): {Pid, Selector} → {Key, CounterRef, SlotBase}
%%% - beamtalk_trace_events (ETS ordered_set): composite key → trace event
%%% - counters ref in persistent_term: lock-free aggregate increments (~50ns)
%%% - persistent_term toggle: beamtalk_tracing_enabled (true | false)
%%%
%%% ETS tables use {heir, SupervisorPid, HeirData} for crash resilience.
%%% Counter refs stored in persistent_term survive gen_server crashes.
%%%
%%% @see docs/ADR/0069-actor-observability-and-tracing.md
-module(beamtalk_trace_store).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    %% Control
    enable/0,
    disable/0,
    is_enabled/0,
    clear/0,
    %% Write-path helpers (called from telemetry handlers, NOT via gen_server)
    record_dispatch/5,
    record_trace_event/8,
    %% Query API
    get_traces/0,
    get_traces/1,
    get_traces/2,
    %% Export
    export_traces/1,
    get_stats/0,
    get_stats/1,
    slow_methods/1,
    hot_methods/1,
    error_methods/1,
    bottlenecks/1,
    actor_health/1,
    system_health/0,
    %% Configuration
    max_events/0,
    max_events/1,
    %% Health check
    telemetry_attached/0,
    %% Telemetry handler callbacks
    handle_dispatch_stop/4,
    handle_dispatch_exception/4,
    handle_lifecycle_event/4,
    handle_vm_measurements/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Counter slot offsets (per {Pid, Selector} entry)
-define(SLOT_CALLS, 1).
-define(SLOT_OK, 2).
-define(SLOT_ERRORS, 3).
-define(SLOT_TIMEOUTS, 4).
-define(SLOT_TOTAL_DURATION, 5).
-define(SLOT_RESERVED, 6).
-define(SLOTS_PER_KEY, 6).

%% Table names
-define(AGG_INDEX, beamtalk_agg_index).
-define(TRACE_EVENTS, beamtalk_trace_events).

%% persistent_term keys
-define(PT_ENABLED, beamtalk_tracing_enabled).
-define(PT_COUNTERS, beamtalk_trace_counters).
-define(PT_MAX_EVENTS, beamtalk_trace_max_events).
-define(PT_SLOT_ALLOCATOR, beamtalk_trace_slot_allocator).

%% Defaults
-define(DEFAULT_MAX_EVENTS, 100000).
% 1000 unique keys × 6 slots
-define(INITIAL_COUNTER_SIZE, 6000).
-define(EVICTION_INTERVAL_MS, 1000).
-define(EVICTION_PERCENT, 10).

-record(state, {
    sweep_timer :: reference() | undefined,
    %% VM stats from telemetry_poller
    vm_stats = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the trace store gen_server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Enable trace event capture. Aggregates are always on.
-spec enable() -> ok.
enable() ->
    gen_server:call(?MODULE, enable).

%% @doc Disable trace event capture.
-spec disable() -> ok.
disable() ->
    gen_server:call(?MODULE, disable).

%% @doc Check whether trace event capture is active.
-spec is_enabled() -> boolean().
is_enabled() ->
    try persistent_term:get(?PT_ENABLED) of
        Val -> Val
    catch
        error:badarg -> false
    end.

%% @doc Clear all trace events and aggregate stats.
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%% @doc Record a dispatch aggregate (called from telemetry handler, NOT via gen_server).
%% This runs in the calling process for lock-free performance.
-spec record_dispatch(pid(), atom(), non_neg_integer(), atom(), atom()) -> ok.
record_dispatch(Pid, Selector, Duration, Outcome, _Mode) ->
    {CounterRef, SlotBase} = ensure_counter_slot(Pid, Selector),
    counters:add(CounterRef, SlotBase + ?SLOT_CALLS, 1),
    counters:add(CounterRef, SlotBase + ?SLOT_TOTAL_DURATION, Duration),
    case Outcome of
        ok -> counters:add(CounterRef, SlotBase + ?SLOT_OK, 1);
        error -> counters:add(CounterRef, SlotBase + ?SLOT_ERRORS, 1);
        timeout -> counters:add(CounterRef, SlotBase + ?SLOT_TIMEOUTS, 1);
        cast -> counters:add(CounterRef, SlotBase + ?SLOT_OK, 1);
        _ -> ok
    end,
    ok.

%% @doc Record a trace event (called from telemetry handler, NOT via gen_server).
%% Only inserts if tracing is enabled. Runs in the calling process.
-spec record_trace_event(pid(), atom(), atom(), atom(), non_neg_integer(), atom(), map(), atom()) ->
    ok.
record_trace_event(Pid, Class, Selector, Mode, Duration, Outcome, Metadata, _Phase) ->
    case is_enabled() of
        true ->
            Key = {erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])},
            WallClockUs = erlang:system_time(microsecond),
            ets:insert(
                ?TRACE_EVENTS,
                {Key, Pid, Class, Selector, Mode, Duration, Outcome, Metadata, WallClockUs}
            ),
            ok;
        false ->
            ok
    end.

%% @doc Get all trace events, newest first.
-spec get_traces() -> [map()].
get_traces() ->
    get_traces(#{}).

%% @doc Get trace events filtered by opts map or actor pid, newest first.
%% Opts map supports: actor, selector, class, outcome, min_duration_ns.
-spec get_traces(map() | pid() | undefined) -> [map()].
get_traces(Opts) when is_map(Opts) ->
    gen_server:call(?MODULE, {get_traces, Opts});
get_traces(Pid) ->
    get_traces(#{actor => Pid}).

%% @doc Get trace events for a specific actor + selector, newest first.
-spec get_traces(pid() | undefined, atom() | undefined) -> [map()].
get_traces(Pid, Selector) ->
    get_traces(#{actor => Pid, selector => Selector}).

%% @doc Export trace events to a JSON file.
%% Opts is a map with optional keys:
%%   path     - binary file path (default: timestamped file in cwd)
%%   actor    - pid() to filter by actor
%%   selector - atom() to filter by selector
%%   limit    - integer() max events to export
%% Returns {ok, #{path => Path, count => Count}} on success.
-spec export_traces(map()) -> {ok, map()} | {error, term()}.
export_traces(Opts) when is_map(Opts) ->
    gen_server:call(?MODULE, {export_traces, Opts});
export_traces(_) ->
    {error, badarg}.

%% @doc Get aggregate stats for all actors.
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Get aggregate stats for a specific actor pid.
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(?MODULE, {get_stats, Pid}).

%% @doc Top N methods by average duration (descending).
-spec slow_methods(pos_integer()) -> [map()].
slow_methods(Limit) ->
    gen_server:call(?MODULE, {slow_methods, Limit}).

%% @doc Top N methods by call count (descending).
-spec hot_methods(pos_integer()) -> [map()].
hot_methods(Limit) ->
    gen_server:call(?MODULE, {hot_methods, Limit}).

%% @doc Top N methods by error + timeout rate (descending).
-spec error_methods(pos_integer()) -> [map()].
error_methods(Limit) ->
    gen_server:call(?MODULE, {error_methods, Limit}).

%% @doc Top N actors by message queue length (live snapshot).
-spec bottlenecks(pos_integer()) -> [map()].
bottlenecks(Limit) ->
    gen_server:call(?MODULE, {bottlenecks, Limit}).

%% @doc Live process health for a specific actor.
-spec actor_health(pid()) -> map().
actor_health(Pid) ->
    gen_server:call(?MODULE, {actor_health, Pid}).

%% @doc VM overview: scheduler count, memory, process count, run queues.
-spec system_health() -> map().
system_health() ->
    gen_server:call(?MODULE, system_health).

%% @doc Get current ring buffer capacity.
-spec max_events() -> pos_integer().
max_events() ->
    try persistent_term:get(?PT_MAX_EVENTS) of
        Val -> Val
    catch
        error:badarg -> ?DEFAULT_MAX_EVENTS
    end.

%% @doc Set ring buffer capacity.
-spec max_events(pos_integer()) -> ok.
max_events(Size) when is_integer(Size), Size > 0 ->
    gen_server:call(?MODULE, {max_events, Size}).

%% @doc Check whether telemetry handlers are attached to actor dispatch events.
%% Returns true if the trace store's telemetry handlers are active, false if
%% telemetry is unavailable or handlers failed to attach.
-spec telemetry_attached() -> boolean().
telemetry_attached() ->
    try
        Handlers = telemetry:list_handlers([beamtalk, actor, dispatch, stop]),
        lists:any(
            fun(#{id := Id}) -> Id =:= beamtalk_trace_store_dispatch_stop end,
            Handlers
        )
    catch
        error:undef -> false
    end.

%%====================================================================
%% Telemetry handler callbacks
%%====================================================================

%% @doc Handler for [beamtalk, actor, dispatch, stop] events.
-spec handle_dispatch_stop([atom()], map(), map(), map()) -> ok.
handle_dispatch_stop(_EventName, #{duration := Duration}, Metadata, _Config) ->
    #{pid := Pid, selector := Selector, mode := Mode} = Metadata,
    Outcome = maps:get(outcome, Metadata, ok),
    Class = maps:get(class, Metadata, unknown),
    DurationNs = erlang:convert_time_unit(Duration, native, nanosecond),
    record_dispatch(Pid, Selector, DurationNs, Outcome, Mode),
    record_trace_event(Pid, Class, Selector, Mode, DurationNs, Outcome, #{}, stop),
    ok;
handle_dispatch_stop(_EventName, _Measurements, _Metadata, _Config) ->
    ok.

%% @doc Handler for [beamtalk, actor, dispatch, exception] events.
-spec handle_dispatch_exception([atom()], map(), map(), map()) -> ok.
handle_dispatch_exception(_EventName, #{duration := Duration}, Metadata, _Config) ->
    #{pid := Pid, selector := Selector, mode := Mode} = Metadata,
    Class = maps:get(class, Metadata, unknown),
    Kind = maps:get(kind, Metadata, error),
    Reason = maps:get(reason, Metadata, unknown),
    DurationNs = erlang:convert_time_unit(Duration, native, nanosecond),
    record_dispatch(Pid, Selector, DurationNs, error, Mode),
    EventMeta = #{error => Reason, kind => Kind},
    record_trace_event(Pid, Class, Selector, Mode, DurationNs, error, EventMeta, exception),
    ok;
handle_dispatch_exception(_EventName, _Measurements, _Metadata, _Config) ->
    ok.

%% @doc Handler for [beamtalk, actor, lifecycle, *] events (BT-1629).
%% Lifecycle events (start, stop, kill) are instantaneous — no duration.
%% Recorded in the shared trace ring buffer with mode => lifecycle.
-spec handle_lifecycle_event([atom()], map(), map(), map()) -> ok.
handle_lifecycle_event(EventName, _Measurements, Metadata, _Config) ->
    #{pid := Pid, class := Class} = Metadata,
    %% Extract lifecycle action from event name: [beamtalk, actor, lifecycle, Action]
    Action = lists:last(EventName),
    %% Derive outcome from reason for stop events: normal/shutdown = ok, crash = error
    Outcome = lifecycle_outcome(Action, Metadata),
    %% Record aggregate stats with selector = lifecycle action (e.g., start, stop, kill)
    record_dispatch(Pid, Action, 0, Outcome, lifecycle),
    %% Record trace event with mode => lifecycle, duration 0 (instantaneous)
    EventMeta = maps:without([pid, class], Metadata),
    record_trace_event(Pid, Class, Action, lifecycle, 0, Outcome, EventMeta, stop),
    ok.

%% @private Derive outcome from lifecycle event action and metadata.
%% For stop events, normal/shutdown exits are ok; anything else is an error.
%% Start and kill events are always ok.
lifecycle_outcome(stop, #{reason := normal}) -> ok;
lifecycle_outcome(stop, #{reason := shutdown}) -> ok;
lifecycle_outcome(stop, #{reason := {shutdown, _}}) -> ok;
lifecycle_outcome(stop, #{reason := _}) -> error;
lifecycle_outcome(_, _) -> ok.

%% @doc Handler for telemetry_poller VM measurement events.
-spec handle_vm_measurements([atom()], map(), map(), map()) -> ok.
handle_vm_measurements(_EventName, Measurements, _Metadata, _Config) ->
    gen_server:cast(?MODULE, {vm_measurements, Measurements}),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),

    %% Get supervisor pid for heir option
    SupPid = find_supervisor(),

    %% Create or inherit ETS tables
    ensure_ets_tables(SupPid),

    %% Initialise counters in persistent_term (if not already present from crash recovery)
    ensure_counters(),

    %% Initialise persistent_term defaults
    init_persistent_terms(),

    %% Ensure telemetry apps are started (not in app.src to avoid breaking
    %% environments where telemetry isn't on the code path, e.g. stdlib tests).
    %% If telemetry is not available, the trace store still works for direct
    %% record_dispatch/record_trace_event calls — just no automatic handler attachment.
    case application:ensure_all_started(telemetry) of
        {ok, _} ->
            attach_telemetry_handlers(),
            configure_poller();
        {error, _} ->
            ?LOG_INFO("telemetry not available, skipping handler attachment", #{}),
            ok
    end,

    %% Start periodic sweep timer
    TimerRef = start_sweep_timer(),

    ?LOG_INFO("Trace store started", #{}),
    {ok, #state{sweep_timer = TimerRef}}.

%% @private
handle_call(enable, _From, State) ->
    persistent_term:put(?PT_ENABLED, true),
    ?LOG_INFO("Trace capture enabled", #{}),
    {reply, ok, State};
handle_call(disable, _From, State) ->
    persistent_term:put(?PT_ENABLED, false),
    ?LOG_INFO("Trace capture disabled", #{}),
    {reply, ok, State};
handle_call(clear, _From, State) ->
    do_clear(),
    ?LOG_INFO("Trace store cleared", #{}),
    {reply, ok, State};
handle_call({grow_counters, CallerRef, CallerSize}, _From, State) ->
    %% BT-1621: Serialized counter grow to prevent concurrent persistent_term overwrites.
    %% Check if another caller already grew while we were queued.
    CurrentRef = persistent_term:get(?PT_COUNTERS),
    Result =
        case CurrentRef =:= CallerRef of
            true ->
                %% We're still on the same ref — grow is needed
                grow_counters(CallerRef, CallerSize);
            false ->
                %% Another call already grew — return the current ref
                CurrentRef
        end,
    {reply, Result, State};
handle_call({get_traces, Opts}, _From, State) when is_map(Opts) ->
    Result = do_get_traces(Opts),
    {reply, Result, State};
handle_call({export_traces, Opts}, _From, State) ->
    Result = do_export_traces(Opts),
    {reply, Result, State};
handle_call(get_stats, _From, State) ->
    Result = to_binary_keys(do_get_stats(undefined)),
    {reply, Result, State};
handle_call({get_stats, Pid}, _From, State) ->
    Result = to_binary_keys(do_get_stats(Pid)),
    {reply, Result, State};
handle_call({slow_methods, Limit}, _From, State) ->
    Result = [to_binary_keys(M) || M <- do_slow_methods(Limit)],
    {reply, Result, State};
handle_call({hot_methods, Limit}, _From, State) ->
    Result = [to_binary_keys(M) || M <- do_hot_methods(Limit)],
    {reply, Result, State};
handle_call({error_methods, Limit}, _From, State) ->
    Result = [to_binary_keys(M) || M <- do_error_methods(Limit)],
    {reply, Result, State};
handle_call({bottlenecks, Limit}, _From, State) ->
    Result = [to_binary_keys(M) || M <- do_bottlenecks(Limit)],
    {reply, Result, State};
handle_call({actor_health, Pid}, _From, State) ->
    Result = to_binary_keys(do_actor_health(Pid)),
    {reply, Result, State};
handle_call(system_health, _From, State) ->
    Result = to_binary_keys(do_system_health(State)),
    {reply, Result, State};
handle_call({max_events, Size}, _From, State) ->
    persistent_term:put(?PT_MAX_EVENTS, Size),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({vm_measurements, Measurements}, State) ->
    {noreply, State#state{vm_stats = Measurements}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(sweep_eviction, State) ->
    do_sweep_eviction(),
    TimerRef = start_sweep_timer(),
    {noreply, State#state{sweep_timer = TimerRef}};
handle_info({'ETS-TRANSFER', TableName, _FromPid, _HeirData}, State) ->
    ?LOG_INFO("Inherited ETS table via heir", #{table => TableName}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    case State#state.sweep_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    detach_telemetry_handlers(),
    ok.

%% @private
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal: ETS table management
%%====================================================================

%% @private Find the supervisor pid for heir option.
find_supervisor() ->
    case whereis(beamtalk_runtime_sup) of
        undefined -> self();
        Pid -> Pid
    end.

%% @private Create or verify ETS tables exist.
ensure_ets_tables(SupPid) ->
    ensure_table(?AGG_INDEX, [
        named_table,
        public,
        set,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, SupPid, ?AGG_INDEX}
    ]),
    ensure_table(?TRACE_EVENTS, [
        named_table,
        public,
        ordered_set,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, SupPid, ?TRACE_EVENTS}
    ]),
    ok.

%% @private Create a table if it doesn't exist.
%% If the table already exists (inherited by supervisor via heir after crash),
%% it's a public table so reads/writes work regardless of ownership. The data
%% is preserved, which is the primary goal of the heir mechanism.
ensure_table(Name, Opts) ->
    case ets:whereis(Name) of
        undefined ->
            Name = ets:new(Name, Opts);
        _Tid ->
            %% Table already exists (inherited via heir after crash).
            %% Public table — reads/writes work regardless of owner.
            ok
    end.

%% @private Ensure counters ref and slot allocator exist in persistent_term.
ensure_counters() ->
    try persistent_term:get(?PT_COUNTERS) of
        _Ref -> ok
    catch
        error:badarg ->
            Ref = counters:new(?INITIAL_COUNTER_SIZE, [write_concurrency]),
            persistent_term:put(?PT_COUNTERS, Ref),
            %% Atomic slot allocator — single atomics ref with 1 element for the next slot base.
            %% atomics:add_get/3 is truly atomic, preventing concurrent slot collisions.
            Allocator = atomics:new(1, [{signed, false}]),
            atomics:put(Allocator, 1, 1),
            persistent_term:put(?PT_SLOT_ALLOCATOR, Allocator)
    end.

%% @private Initialise persistent_term defaults.
init_persistent_terms() ->
    try persistent_term:get(?PT_ENABLED) of
        _ -> ok
    catch
        error:badarg ->
            persistent_term:put(?PT_ENABLED, false)
    end,
    try persistent_term:get(?PT_MAX_EVENTS) of
        _ -> ok
    catch
        error:badarg ->
            persistent_term:put(?PT_MAX_EVENTS, ?DEFAULT_MAX_EVENTS)
    end.

%%====================================================================
%% Internal: Counter slot allocation
%%====================================================================

%% @private Get or allocate counter slots for a {Pid, Selector} pair.
%% Called from the write path (calling process), not the gen_server.
-spec ensure_counter_slot(pid(), atom()) -> {counters:counters_ref(), non_neg_integer()}.
ensure_counter_slot(Pid, Selector) ->
    Key = {Pid, Selector},
    case ets:lookup(?AGG_INDEX, Key) of
        [{_Key, CounterRef, SlotBase}] ->
            {CounterRef, SlotBase};
        [] ->
            allocate_counter_slot(Key)
    end.

%% @private Allocate new counter slots for a key.
%% Uses atomics-style CAS on persistent_term slot counter.
%% In the rare concurrent allocation race, both callers get valid (different) slots
%% and the last ets:insert wins — acceptable for first-observation allocation.
allocate_counter_slot(Key) ->
    CounterRef = persistent_term:get(?PT_COUNTERS),
    SlotBase = allocate_next_slot(),
    #{size := CounterSize} = counters:info(CounterRef),
    %% Grow counters if needed — serialized through the gen_server (BT-1621)
    %% to prevent concurrent grows from overwriting each other's persistent_term.
    NewCounterRef =
        case SlotBase + ?SLOTS_PER_KEY > CounterSize of
            true ->
                %% Grow serialized through gen_server (BT-1621). Gracefully degrade
                %% if the trace store is down or slow — observability should never
                %% crash the actor being observed.
                try
                    gen_server:call(?MODULE, {grow_counters, CounterRef, CounterSize}, 10000)
                catch
                    exit:{noproc, _} -> CounterRef;
                    exit:{timeout, _} -> CounterRef
                end;
            false ->
                CounterRef
        end,
    ets:insert(?AGG_INDEX, {Key, NewCounterRef, SlotBase}),
    {NewCounterRef, SlotBase}.

%% @private Atomically allocate the next slot base.
%% Uses atomics:add_get/3 for true lock-free atomic allocation — no race condition
%% even under concurrent first-observation of different {Pid, Selector} keys.
allocate_next_slot() ->
    Allocator = persistent_term:get(?PT_SLOT_ALLOCATOR),
    %% add_get returns the NEW value after adding, so subtract to get the old (allocated) base
    NewVal = atomics:add_get(Allocator, 1, ?SLOTS_PER_KEY),
    NewVal - ?SLOTS_PER_KEY.

%% @private Grow the counters array by doubling its size.
grow_counters(OldRef, OldSize) ->
    NewSize = OldSize * 2,
    NewRef = counters:new(NewSize, [write_concurrency]),
    %% Copy existing counter values
    copy_counters(OldRef, NewRef, 1, OldSize),
    persistent_term:put(?PT_COUNTERS, NewRef),
    %% Update all index entries to point to new ref
    update_counter_refs(NewRef),
    NewRef.

%% @private Copy counter values from old ref to new ref.
copy_counters(_OldRef, _NewRef, Idx, MaxIdx) when Idx > MaxIdx ->
    ok;
copy_counters(OldRef, NewRef, Idx, MaxIdx) ->
    Val = counters:get(OldRef, Idx),
    case Val of
        0 -> ok;
        _ -> counters:add(NewRef, Idx, Val)
    end,
    copy_counters(OldRef, NewRef, Idx + 1, MaxIdx).

%% @private Update all agg_index entries to point to a new counter ref.
update_counter_refs(NewRef) ->
    ets:foldl(
        fun({Key, _OldRef, SlotBase}, Acc) ->
            ets:insert(?AGG_INDEX, {Key, NewRef, SlotBase}),
            Acc
        end,
        ok,
        ?AGG_INDEX
    ).

%%====================================================================
%% Internal: Telemetry handlers
%%====================================================================

%% @private Attach telemetry event handlers.
%% Detach first to handle restarts after crash (terminate/2 not called on kill).
attach_telemetry_handlers() ->
    detach_telemetry_handlers(),
    ok = telemetry:attach(
        beamtalk_trace_store_dispatch_stop,
        [beamtalk, actor, dispatch, stop],
        fun ?MODULE:handle_dispatch_stop/4,
        #{}
    ),
    ok = telemetry:attach(
        beamtalk_trace_store_dispatch_exception,
        [beamtalk, actor, dispatch, exception],
        fun ?MODULE:handle_dispatch_exception/4,
        #{}
    ),
    %% BT-1629: Lifecycle event handlers (start, stop, kill)
    ok = telemetry:attach(
        beamtalk_trace_store_lifecycle_start,
        [beamtalk, actor, lifecycle, start],
        fun ?MODULE:handle_lifecycle_event/4,
        #{}
    ),
    ok = telemetry:attach(
        beamtalk_trace_store_lifecycle_stop,
        [beamtalk, actor, lifecycle, stop],
        fun ?MODULE:handle_lifecycle_event/4,
        #{}
    ),
    ok = telemetry:attach(
        beamtalk_trace_store_lifecycle_kill,
        [beamtalk, actor, lifecycle, kill],
        fun ?MODULE:handle_lifecycle_event/4,
        #{}
    ),
    ok = telemetry:attach(
        beamtalk_trace_store_vm_measurements,
        [vm, memory],
        fun ?MODULE:handle_vm_measurements/4,
        #{}
    ),
    ok.

%% @private Detach telemetry handlers on shutdown (safe if telemetry not loaded).
detach_telemetry_handlers() ->
    try
        telemetry:detach(beamtalk_trace_store_dispatch_stop),
        telemetry:detach(beamtalk_trace_store_dispatch_exception),
        %% BT-1629: Lifecycle event handlers
        telemetry:detach(beamtalk_trace_store_lifecycle_start),
        telemetry:detach(beamtalk_trace_store_lifecycle_stop),
        telemetry:detach(beamtalk_trace_store_lifecycle_kill),
        telemetry:detach(beamtalk_trace_store_vm_measurements)
    catch
        error:undef -> ok
    end,
    ok.

%% @private Configure telemetry_poller for periodic VM measurements.
configure_poller() ->
    %% telemetry_poller is started by the telemetry_poller application.
    %% It emits [vm, memory], [vm, total_run_queue_lengths], etc. by default.
    %% We just need to ensure the application is started.
    _ = application:ensure_all_started(telemetry_poller),
    ok.

%%====================================================================
%% Internal: Sweep / eviction
%%====================================================================

%% @private Start the periodic sweep timer.
start_sweep_timer() ->
    erlang:send_after(?EVICTION_INTERVAL_MS, self(), sweep_eviction).

%% @private Evict oldest events if over the soft bound.
do_sweep_eviction() ->
    MaxEvents = max_events(),
    Size = ets:info(?TRACE_EVENTS, size),
    case Size > MaxEvents of
        true ->
            %% Delete oldest 10%
            ToDelete = max(1, Size * ?EVICTION_PERCENT div 100),
            delete_oldest_n(?TRACE_EVENTS, ToDelete);
        false ->
            ok
    end.

%% @private Delete the N oldest entries from an ordered_set.
delete_oldest_n(_Table, 0) ->
    ok;
delete_oldest_n(Table, N) ->
    case ets:first(Table) of
        '$end_of_table' ->
            ok;
        Key ->
            ets:delete(Table, Key),
            delete_oldest_n(Table, N - 1)
    end.

%%====================================================================
%% Internal: Query implementations
%%====================================================================

%% @private Get trace events, filtered by opts map.
%% Supported opts: actor, selector, class, outcome, min_duration_ns.
do_get_traces(Opts) when is_map(Opts) ->
    Pid = maps:get(actor, Opts, undefined),
    Selector = maps:get(selector, Opts, undefined),
    Class = maps:get(class, Opts, undefined),
    Outcome = maps:get(outcome, Opts, undefined),
    MinDurationNs = maps:get(min_duration_ns, Opts, undefined),
    All = ets:tab2list(?TRACE_EVENTS),
    Filtered = lists:filter(
        fun(Ev) ->
            EvPid = element(2, Ev),
            EvClass = element(3, Ev),
            EvSel = element(4, Ev),
            EvDuration = element(6, Ev),
            EvOutcome = element(7, Ev),
            (Pid =:= undefined orelse EvPid =:= Pid) andalso
                (Selector =:= undefined orelse EvSel =:= Selector) andalso
                (Class =:= undefined orelse EvClass =:= Class) andalso
                (Outcome =:= undefined orelse EvOutcome =:= Outcome) andalso
                (MinDurationNs =:= undefined orelse EvDuration >= MinDurationNs)
        end,
        All
    ),
    %% Sort newest first (reverse of ordered_set natural order)
    Sorted = lists:reverse(Filtered),
    [trace_event_to_map(Ev) || Ev <- Sorted].

%% @private Export traces to a JSON file with optional filters.
do_export_traces(Opts) ->
    Limit = maps:get(limit, Opts, undefined),
    %% Pass filter opts through to do_get_traces (actor, selector, class, outcome, min_duration_ns)
    FilterOpts = maps:without([path, limit], Opts),
    Traces = do_get_traces(FilterOpts),
    Limited =
        case Limit of
            undefined -> Traces;
            L when is_integer(L), L > 0 -> lists:sublist(Traces, L);
            _ -> Traces
        end,
    Count = length(Limited),
    Path =
        case maps:get(path, Opts, undefined) of
            undefined -> default_export_path();
            P -> P
        end,
    %% Build metadata header
    FilterParams = maps:without([path], Opts),
    FilterMap = maps:fold(
        fun(K, V, Acc) ->
            BinKey = atom_to_binary(K, utf8),
            BinVal =
                case V of
                    _ when is_pid(V) -> list_to_binary(pid_to_list(V));
                    _ when is_atom(V) -> atom_to_binary(V, utf8);
                    _ when is_integer(V) -> V;
                    _ when is_binary(V) -> V;
                    _ -> iolist_to_binary(io_lib:format("~p", [V]))
                end,
            Acc#{BinKey => BinVal}
        end,
        #{},
        FilterParams
    ),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    TimestampBin = iolist_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Year, Month, Day, Hour, Min, Sec]
        )
    ),
    Export = #{
        <<"metadata">> => #{
            <<"export_timestamp">> => TimestampBin,
            <<"filters">> => FilterMap,
            <<"total_events">> => Count
        },
        <<"traces">> => Limited
    },
    JsonBin = jsx:encode(Export, [{space, 2}, {indent, 2}]),
    case file:write_file(Path, JsonBin) of
        ok ->
            {ok, #{path => Path, count => Count}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Generate a default export path with timestamp.
default_export_path() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Unique = erlang:unique_integer([positive, monotonic]),
    Filename = io_lib:format(
        "traces-~4..0B-~2..0B-~2..0BT~2..0B-~2..0B-~2..0B-~B.json",
        [Year, Month, Day, Hour, Min, Sec, Unique]
    ),
    iolist_to_binary(Filename).

%% @private Convert a trace event tuple to a map.
%% Supports both 9-tuple (with wall-clock) and legacy 8-tuple (without) formats.
trace_event_to_map({
    {_MonotonicNs, _Unique},
    Pid,
    Class,
    Selector,
    Mode,
    DurationNs,
    Outcome,
    Metadata,
    WallClockUs
}) ->
    Base = #{
        <<"timestamp_us">> => WallClockUs,
        <<"actor">> => list_to_binary(pid_to_list(Pid)),
        <<"class">> => atom_to_binary(Class, utf8),
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"mode">> => atom_to_binary(Mode, utf8),
        <<"duration_us">> => DurationNs div 1000,
        <<"outcome">> => atom_to_binary(Outcome, utf8)
    },
    case maps:size(Metadata) of
        0 -> Base;
        _ -> maps:merge(Base, Metadata)
    end;
trace_event_to_map({
    {MonotonicNs, _Unique}, Pid, Class, Selector, Mode, DurationNs, Outcome, Metadata
}) ->
    %% Legacy format (pre-BT-1620): fall back to monotonic-derived timestamp
    Base = #{
        <<"timestamp_us">> => MonotonicNs div 1000,
        <<"actor">> => list_to_binary(pid_to_list(Pid)),
        <<"class">> => atom_to_binary(Class, utf8),
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"mode">> => atom_to_binary(Mode, utf8),
        <<"duration_us">> => DurationNs div 1000,
        <<"outcome">> => atom_to_binary(Outcome, utf8)
    },
    case maps:size(Metadata) of
        0 -> Base;
        _ -> maps:merge(Base, Metadata)
    end.

%% @private Get aggregate stats, optionally filtered by Pid.
do_get_stats(FilterPid) ->
    ets:foldl(
        fun({{Pid, Selector}, CounterRef, SlotBase}, Acc) ->
            case FilterPid =:= undefined orelse Pid =:= FilterPid of
                true ->
                    Stats = read_counter_stats(CounterRef, SlotBase),
                    PidKey = list_to_binary(pid_to_list(Pid)),
                    PidStats =
                        case maps:find(PidKey, Acc) of
                            {ok, Existing} -> Existing;
                            error -> #{class => lookup_class_for_pid(Pid), methods => #{}}
                        end,
                    Methods = maps:get(methods, PidStats),
                    SelKey = atom_to_binary(Selector, utf8),
                    NewMethods = maps:put(SelKey, Stats, Methods),
                    maps:put(PidKey, PidStats#{methods => NewMethods}, Acc);
                false ->
                    Acc
            end
        end,
        #{},
        ?AGG_INDEX
    ).

%% @private Read counter stats for a slot.
read_counter_stats(CounterRef, SlotBase) ->
    Calls = counters:get(CounterRef, SlotBase + ?SLOT_CALLS),
    Ok = counters:get(CounterRef, SlotBase + ?SLOT_OK),
    Errors = counters:get(CounterRef, SlotBase + ?SLOT_ERRORS),
    Timeouts = counters:get(CounterRef, SlotBase + ?SLOT_TIMEOUTS),
    TotalDuration = counters:get(CounterRef, SlotBase + ?SLOT_TOTAL_DURATION),
    AvgDuration =
        case Calls of
            0 -> 0;
            _ -> TotalDuration div Calls
        end,
    #{
        calls => Calls,
        ok => Ok,
        errors => Errors,
        timeouts => Timeouts,
        total_duration_ns => TotalDuration,
        avg_duration_ns => AvgDuration
    }.

%% @private Collect all method stats as a flat list for sorting.
collect_method_stats() ->
    ets:foldl(
        fun({{Pid, Selector}, CounterRef, SlotBase}, Acc) ->
            Stats = read_counter_stats(CounterRef, SlotBase),
            [
                Stats#{
                    pid => list_to_binary(pid_to_list(Pid)),
                    class => lookup_class_for_pid(Pid),
                    selector => Selector
                }
                | Acc
            ]
        end,
        [],
        ?AGG_INDEX
    ).

%% @private Top N methods by average duration (descending).
do_slow_methods(Limit) ->
    AllStats = collect_method_stats(),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(avg_duration_ns, A) >= maps:get(avg_duration_ns, B) end,
        AllStats
    ),
    lists:sublist(Sorted, Limit).

%% @private Top N methods by call count (descending).
do_hot_methods(Limit) ->
    AllStats = collect_method_stats(),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(calls, A) >= maps:get(calls, B) end,
        AllStats
    ),
    lists:sublist(Sorted, Limit).

%% @private Top N methods by error + timeout rate (descending).
do_error_methods(Limit) ->
    AllStats = collect_method_stats(),
    WithRate = lists:map(
        fun(S) ->
            Calls = maps:get(calls, S),
            Errors = maps:get(errors, S) + maps:get(timeouts, S),
            Rate =
                case Calls of
                    0 -> 0.0;
                    _ -> Errors / Calls
                end,
            S#{error_rate => Rate}
        end,
        AllStats
    ),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(error_rate, A) >= maps:get(error_rate, B) end,
        WithRate
    ),
    lists:sublist(Sorted, Limit).

%% @private Top N actors by message queue length (live snapshot).
do_bottlenecks(Limit) ->
    %% Get all unique pids from the agg index
    Pids = lists:usort(
        ets:foldl(
            fun({{Pid, _Sel}, _Ref, _Slot}, Acc) -> [Pid | Acc] end,
            [],
            ?AGG_INDEX
        )
    ),
    %% Get queue depths for live processes
    WithQueue = lists:filtermap(
        fun(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    case erlang:process_info(Pid, [message_queue_len, memory]) of
                        undefined ->
                            false;
                        Info ->
                            QLen = proplists:get_value(message_queue_len, Info, 0),
                            Mem = proplists:get_value(memory, Info, 0),
                            {true, #{
                                actor => list_to_binary(pid_to_list(Pid)),
                                class => lookup_class_for_pid(Pid),
                                queue_depth => QLen,
                                memory_kb => Mem div 1024
                            }}
                    end;
                false ->
                    false
            end
        end,
        Pids
    ),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(queue_depth, A) >= maps:get(queue_depth, B) end,
        WithQueue
    ),
    lists:sublist(Sorted, Limit).

%% @private Live health for a specific actor.
do_actor_health(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            case
                erlang:process_info(Pid, [
                    message_queue_len, memory, reductions, status, current_function
                ])
            of
                undefined ->
                    #{
                        pid => list_to_binary(pid_to_list(Pid)),
                        class => lookup_class_for_pid(Pid),
                        status => dead,
                        error => <<"process not alive">>
                    };
                Info ->
                    #{
                        pid => list_to_binary(pid_to_list(Pid)),
                        class => lookup_class_for_pid(Pid),
                        status => proplists:get_value(status, Info),
                        queue_depth => proplists:get_value(message_queue_len, Info, 0),
                        memory_kb => proplists:get_value(memory, Info, 0) div 1024,
                        reductions => proplists:get_value(reductions, Info, 0)
                    }
            end;
        false ->
            #{
                pid => list_to_binary(pid_to_list(Pid)),
                class => lookup_class_for_pid(Pid),
                status => dead,
                error => <<"process not alive">>
            }
    end.

%% @private VM overview.
do_system_health(State) ->
    MemInfo = erlang:memory(),
    #{
        scheduler_count => erlang:system_info(schedulers_online),
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        memory => #{
            total_mb => proplists:get_value(total, MemInfo, 0) div (1024 * 1024),
            processes_mb => proplists:get_value(processes, MemInfo, 0) div (1024 * 1024),
            ets_mb => proplists:get_value(ets, MemInfo, 0) div (1024 * 1024),
            atom_mb => proplists:get_value(atom, MemInfo, 0) div (1024 * 1024)
        },
        run_queue => erlang:statistics(run_queue),
        vm_stats => State#state.vm_stats
    }.

%% @private Convert atom map keys to binary for user-facing output.
%% Nested maps are converted recursively.
-spec to_binary_keys(map()) -> map().
to_binary_keys(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            BinKey =
                case K of
                    _ when is_atom(K) -> atom_to_binary(K, utf8);
                    _ when is_binary(K) -> K;
                    _ -> iolist_to_binary(io_lib:format("~p", [K]))
                end,
            BinVal =
                case V of
                    _ when is_map(V) -> to_binary_keys(V);
                    _ -> V
                end,
            maps:put(BinKey, BinVal, Acc)
        end,
        #{},
        Map
    ).

%% @private Resolve actor class name from pid via beamtalk_object_instances.
%% Returns binary class name or <<"unknown">>.
-spec lookup_class_for_pid(pid()) -> binary().
lookup_class_for_pid(Pid) ->
    try ets:match(beamtalk_instance_registry, {'$1', Pid}) of
        [[Class] | _] -> atom_to_binary(Class, utf8);
        [] -> <<"unknown">>
    catch
        error:badarg -> <<"unknown">>
    end.

%%====================================================================
%% Internal: Clear
%%====================================================================

%% @private Clear all data.
do_clear() ->
    %% Clear trace events
    ets:delete_all_objects(?TRACE_EVENTS),
    %% Clear agg index
    ets:delete_all_objects(?AGG_INDEX),
    %% Reset counters — create a fresh counters ref
    NewRef = counters:new(?INITIAL_COUNTER_SIZE, [write_concurrency]),
    persistent_term:put(?PT_COUNTERS, NewRef),
    %% Reset the atomic slot allocator
    Allocator = persistent_term:get(?PT_SLOT_ALLOCATOR),
    atomics:put(Allocator, 1, 1),
    ok.
