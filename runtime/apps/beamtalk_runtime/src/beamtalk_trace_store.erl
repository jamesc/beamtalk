%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_trace_store).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Trace store gen_server for actor observability (ADR 0069 Phase 1).

Owns ETS tables and atomics refs for lock-free aggregate tracking and
opt-in trace event capture. The gen_server handles queries and lifecycle
but is NOT in the write path — all writes happen in the calling process
via atomics:add/3, CAS loops, and ets:insert/2.

Storage layout:
- beamtalk_agg_index (ETS set): {Pid, Selector} → {Key, CounterRef, SlotBase}
- beamtalk_trace_events (ETS ordered_set): composite key → trace event
- atomics ref in persistent_term: lock-free aggregate increments + CAS min/max (~50ns)
- persistent_term toggle: beamtalk_tracing_enabled (true | false)

ETS tables use {heir, SupervisorPid, HeirData} for crash resilience.
Atomics refs stored in persistent_term survive gen_server crashes.

See also: docs/ADR/0069-actor-observability-and-tracing.md
""".

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
    record_dispatch/6,
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
    slow_methods/2,
    hot_methods/1,
    error_methods/1,
    bottlenecks/1,
    actor_health/1,
    system_health/0,
    %% Configuration
    max_events/0,
    max_events/1,
    %% Causal trace linking (BT-1633)
    next_span_id/0,
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

%% Atomics slot offsets (per {Pid, Selector} entry)
-define(SLOT_CALLS, 1).
-define(SLOT_OK, 2).
-define(SLOT_ERRORS, 3).
-define(SLOT_TIMEOUTS, 4).
-define(SLOT_TOTAL_DURATION, 5).
-define(SLOT_MIN_DURATION, 6).
-define(SLOT_MAX_DURATION, 7).
-define(SLOTS_PER_KEY, 8).

%% Sentinel value for min duration — large enough that any real duration will be smaller.
%% 2^62 - 1 (~4.6 × 10^18 ns ≈ 146 years). Fits comfortably in a signed 64-bit integer.
-define(MIN_SENTINEL, 4611686018427387903).

%% Table names
-define(AGG_INDEX, beamtalk_agg_index).
-define(AGG_CLASS, beamtalk_agg_class).
-define(TRACE_EVENTS, beamtalk_trace_events).

%% persistent_term keys
-define(PT_ENABLED, beamtalk_tracing_enabled).
-define(PT_COUNTERS, beamtalk_trace_counters).
-define(PT_MAX_EVENTS, beamtalk_trace_max_events).
-define(PT_SLOT_ALLOCATOR, beamtalk_trace_slot_allocator).
-define(PT_SPAN_ID_COUNTER, beamtalk_span_id_counter).

%% Defaults
-define(DEFAULT_MAX_EVENTS, 100000).
% 1000 unique keys × 8 slots
-define(INITIAL_COUNTER_SIZE, 8000).
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

-doc "Start the trace store gen_server.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Enable trace event capture. Aggregates are always on.".
-spec enable() -> ok.
enable() ->
    gen_server:call(?MODULE, enable).

-doc "Disable trace event capture.".
-spec disable() -> ok.
disable() ->
    gen_server:call(?MODULE, disable).

-doc "Check whether trace event capture is active.".
-spec is_enabled() -> boolean().
is_enabled() ->
    try persistent_term:get(?PT_ENABLED) of
        Val -> Val
    catch
        error:badarg -> false
    end.

-doc """
Generate the next monotonic span ID for causal trace linking (BT-1633).
Uses a global atomics counter — one atomics:add_get/3 call (~10ns).
Only called from within maybe_span when tracing is enabled.
""".
-spec next_span_id() -> pos_integer().
next_span_id() ->
    Ref = persistent_term:get(?PT_SPAN_ID_COUNTER),
    atomics:add_get(Ref, 1, 1).

-doc "Clear all trace events and aggregate stats.".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

-doc """
Record a dispatch aggregate (called from telemetry handler, NOT via gen_server).
This runs in the calling process for lock-free performance.
Class comes from telemetry metadata — the authoritative source (BT-1640).
""".
-spec record_dispatch(pid(), atom(), non_neg_integer(), atom(), atom(), atom()) -> ok.
record_dispatch(Pid, Selector, Duration, Outcome, _Mode, Class) ->
    %% BT-1640: Store Pid→Class mapping from telemetry metadata so actor_stats
    %% shows correct class names instead of relying on instance registry lookup.
    case Class of
        unknown -> ok;
        _ -> ets:insert(?AGG_CLASS, {Pid, Class})
    end,
    {AtomicsRef, SlotBase} = ensure_counter_slot(Pid, Selector),
    atomics:add(AtomicsRef, SlotBase + ?SLOT_CALLS, 1),
    atomics:add(AtomicsRef, SlotBase + ?SLOT_TOTAL_DURATION, Duration),
    case Outcome of
        ok -> atomics:add(AtomicsRef, SlotBase + ?SLOT_OK, 1);
        error -> atomics:add(AtomicsRef, SlotBase + ?SLOT_ERRORS, 1);
        timeout -> atomics:add(AtomicsRef, SlotBase + ?SLOT_TIMEOUTS, 1);
        cast -> atomics:add(AtomicsRef, SlotBase + ?SLOT_OK, 1);
        _ -> ok
    end,
    %% Update min/max duration using lock-free CAS loops
    cas_min(AtomicsRef, SlotBase + ?SLOT_MIN_DURATION, Duration),
    cas_max(AtomicsRef, SlotBase + ?SLOT_MAX_DURATION, Duration),
    ok.

-doc """
Record a trace event (called from telemetry handler, NOT via gen_server).
Only inserts if tracing is enabled. Runs in the calling process.
""".
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

-doc "Get all trace events, newest first.".
-spec get_traces() -> [map()].
get_traces() ->
    get_traces(#{}).

-doc """
Get trace events filtered by opts map or actor pid, newest first.
Opts map supports: actor, selector, class, outcome, min_duration_ns.
""".
-spec get_traces(map() | pid() | undefined) -> [map()].
get_traces(Opts) when is_map(Opts) ->
    gen_server:call(?MODULE, {get_traces, Opts});
get_traces(Pid) when is_pid(Pid); Pid =:= undefined ->
    get_traces(#{actor => Pid}).

-doc "Get trace events for a specific actor + selector, newest first.".
-spec get_traces(pid() | undefined, atom() | undefined) -> [map()].
get_traces(Pid, Selector) ->
    get_traces(#{actor => Pid, selector => Selector}).

-doc """
Export trace events to a JSON file.
Opts is a map with optional keys:
  path     - binary file path (default: timestamped file in cwd)
  actor    - pid() to filter by actor
  selector - atom() to filter by selector
  limit    - integer() max events to export
Returns {ok, #{path => Path, count => Count}} on success.
""".
-spec export_traces(map()) -> {ok, map()} | {error, term()}.
export_traces(Opts) when is_map(Opts) ->
    gen_server:call(?MODULE, {export_traces, Opts});
export_traces(_) ->
    {error, badarg}.

-doc "Get aggregate stats for all actors.".
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

-doc "Get aggregate stats for a specific actor pid.".
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(?MODULE, {get_stats, Pid}).

-doc "Top N methods by average duration (descending).".
-spec slow_methods(pos_integer()) -> [map()].
slow_methods(Limit) ->
    gen_server:call(?MODULE, {slow_methods, Limit}).

-doc """
Top N methods sorted by the given duration metric (descending).
SortBy can be: avg_duration_ns | max_duration_ns | min_duration_ns.
""".
-spec slow_methods(pos_integer(), atom()) -> [map()].
slow_methods(Limit, SortBy) ->
    gen_server:call(?MODULE, {slow_methods, Limit, SortBy}).

-doc "Top N methods by call count (descending).".
-spec hot_methods(pos_integer()) -> [map()].
hot_methods(Limit) ->
    gen_server:call(?MODULE, {hot_methods, Limit}).

-doc "Top N methods by error + timeout rate (descending).".
-spec error_methods(pos_integer()) -> [map()].
error_methods(Limit) ->
    gen_server:call(?MODULE, {error_methods, Limit}).

-doc "Top N actors by message queue length (live snapshot).".
-spec bottlenecks(pos_integer()) -> [map()].
bottlenecks(Limit) ->
    gen_server:call(?MODULE, {bottlenecks, Limit}).

-doc "Live process health for a specific actor.".
-spec actor_health(pid()) -> map().
actor_health(Pid) ->
    gen_server:call(?MODULE, {actor_health, Pid}).

-doc "VM overview: scheduler count, memory, process count, run queues.".
-spec system_health() -> map().
system_health() ->
    gen_server:call(?MODULE, system_health).

-doc "Get current ring buffer capacity.".
-spec max_events() -> pos_integer().
max_events() ->
    try persistent_term:get(?PT_MAX_EVENTS) of
        Val -> Val
    catch
        error:badarg -> ?DEFAULT_MAX_EVENTS
    end.

-doc "Set ring buffer capacity.".
-spec max_events(pos_integer()) -> ok.
max_events(Size) when is_integer(Size), Size > 0 ->
    gen_server:call(?MODULE, {max_events, Size}).

-doc """
Check whether telemetry handlers are attached to actor dispatch events.
Returns true if the trace store's telemetry handlers are active, false if
telemetry is unavailable or handlers failed to attach.
""".
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

-doc "Handler for [beamtalk, actor, dispatch, stop] events.".
-spec handle_dispatch_stop([atom()], map(), map(), map()) -> ok.
handle_dispatch_stop(_EventName, #{duration := Duration}, Metadata, _Config) ->
    #{pid := Pid, selector := Selector, mode := Mode} = Metadata,
    Outcome = maps:get(outcome, Metadata, ok),
    Class = maps:get(class, Metadata, unknown),
    DurationNs = erlang:convert_time_unit(Duration, native, nanosecond),
    record_dispatch(Pid, Selector, DurationNs, Outcome, Mode, Class),
    %% BT-1625: Only fetch trace context when tracing is enabled to avoid
    %% per-dispatch overhead when trace capture is disabled.
    case is_enabled() of
        true ->
            TraceCtx = beamtalk_actor:get_trace_context(),
            %% BT-1633: Merge causal trace IDs (trace_id, span_id, parent_span_id)
            %% from process dictionary into trace event metadata.
            CausalCtx = beamtalk_actor:get_causal_ctx(),
            EventMeta = maps:merge(TraceCtx, CausalCtx),
            record_trace_event(Pid, Class, Selector, Mode, DurationNs, Outcome, EventMeta, stop);
        false ->
            ok
    end,
    ok;
handle_dispatch_stop(_EventName, _Measurements, _Metadata, _Config) ->
    ok.

-doc "Handler for [beamtalk, actor, dispatch, exception] events.".
-spec handle_dispatch_exception([atom()], map(), map(), map()) -> ok.
handle_dispatch_exception(_EventName, #{duration := Duration}, Metadata, _Config) ->
    #{pid := Pid, selector := Selector, mode := Mode} = Metadata,
    Class = maps:get(class, Metadata, unknown),
    Kind = maps:get(kind, Metadata, error),
    Reason = maps:get(reason, Metadata, unknown),
    DurationNs = erlang:convert_time_unit(Duration, native, nanosecond),
    record_dispatch(Pid, Selector, DurationNs, error, Mode, Class),
    %% BT-1625: Only fetch trace context and build enriched metadata when
    %% tracing is enabled to avoid per-dispatch overhead.
    case is_enabled() of
        true ->
            TraceCtx = beamtalk_actor:get_trace_context(),
            %% BT-1633: Merge causal trace IDs into exception metadata.
            CausalCtx = beamtalk_actor:get_causal_ctx(),
            BaseMeta = #{error => Reason, kind => Kind},
            EventMeta = maps:merge(maps:merge(TraceCtx, CausalCtx), BaseMeta),
            record_trace_event(Pid, Class, Selector, Mode, DurationNs, error, EventMeta, exception);
        false ->
            ok
    end,
    ok;
handle_dispatch_exception(_EventName, _Measurements, _Metadata, _Config) ->
    ok.

-doc """
Handler for [beamtalk, actor, lifecycle, *] events (BT-1629).
Lifecycle events (start, stop, kill) are instantaneous — no duration.
Recorded in the shared trace ring buffer with mode => lifecycle.
""".
-spec handle_lifecycle_event([atom()], map(), map(), map()) -> ok.
handle_lifecycle_event(EventName, _Measurements, Metadata, _Config) ->
    #{pid := Pid, class := Class} = Metadata,
    %% Extract lifecycle action from event name: [beamtalk, actor, lifecycle, Action]
    Action = lists:last(EventName),
    %% Derive outcome from reason for stop events: normal/shutdown = ok, crash = error
    Outcome = lifecycle_outcome(Action, Metadata),
    %% Record aggregate stats with selector = lifecycle action (e.g., start, stop, kill)
    record_dispatch(Pid, Action, 0, Outcome, lifecycle, Class),
    %% Record trace event with mode => lifecycle, duration 0 (instantaneous)
    EventMeta = maps:without([pid, class], Metadata),
    record_trace_event(Pid, Class, Action, lifecycle, 0, Outcome, EventMeta, stop),
    ok.

-doc """
Derive outcome from lifecycle event action and metadata.
For stop events, normal/shutdown exits are ok; anything else is an error.
Start and kill events are always ok.
""".
lifecycle_outcome(stop, #{reason := normal}) -> ok;
lifecycle_outcome(stop, #{reason := shutdown}) -> ok;
lifecycle_outcome(stop, #{reason := {shutdown, _}}) -> ok;
lifecycle_outcome(stop, #{reason := _}) -> error;
lifecycle_outcome(_, _) -> ok.

-doc "Handler for telemetry_poller VM measurement events.".
-spec handle_vm_measurements([atom()], map(), map(), map()) -> ok.
handle_vm_measurements(_EventName, Measurements, _Metadata, _Config) ->
    gen_server:cast(?MODULE, {vm_measurements, Measurements}),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),

    %% Get supervisor pid for heir option
    SupPid = find_supervisor(),

    %% Create or inherit ETS tables
    ensure_ets_tables(SupPid),

    %% Initialise atomics in persistent_term (if not already present from crash recovery)
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
    %% BT-1621: Serialized atomics grow to prevent concurrent persistent_term overwrites.
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
handle_call({slow_methods, Limit, SortBy}, _From, State) ->
    Result = [to_binary_keys(M) || M <- do_slow_methods(Limit, SortBy)],
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

handle_cast({vm_measurements, Measurements}, State) ->
    {noreply, State#state{vm_stats = Measurements}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sweep_eviction, State) ->
    do_sweep_eviction(),
    TimerRef = start_sweep_timer(),
    {noreply, State#state{sweep_timer = TimerRef}};
handle_info({'ETS-TRANSFER', TableName, _FromPid, _HeirData}, State) ->
    ?LOG_INFO("Inherited ETS table via heir", #{table => TableName}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.sweep_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    detach_telemetry_handlers(),
    ok.

code_change(OldVsn, State, Extra) ->
    %% Ensure new ETS tables (e.g. beamtalk_agg_class) exist after live upgrade.
    ensure_ets_tables(find_supervisor()),
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal: ETS table management
%%====================================================================

-doc "Find the supervisor pid for heir option.".
find_supervisor() ->
    case whereis(beamtalk_runtime_sup) of
        undefined -> self();
        Pid -> Pid
    end.

-doc "Create or verify ETS tables exist.".
ensure_ets_tables(SupPid) ->
    ensure_table(?AGG_INDEX, [
        named_table,
        public,
        set,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, SupPid, ?AGG_INDEX}
    ]),
    ensure_table(?AGG_CLASS, [
        named_table,
        public,
        set,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, SupPid, ?AGG_CLASS}
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

-doc """
Create a table if it doesn't exist.
If the table already exists (inherited by supervisor via heir after crash),
it's a public table so reads/writes work regardless of ownership. The data
is preserved, which is the primary goal of the heir mechanism.
""".
ensure_table(Name, Opts) ->
    case ets:whereis(Name) of
        undefined ->
            _Name = ets:new(Name, Opts);
        _Tid ->
            %% Table already exists (inherited via heir after crash).
            %% Public table — reads/writes work regardless of owner.
            ok
    end.

-doc "Ensure atomics ref and slot allocator exist in persistent_term.".
ensure_counters() ->
    try persistent_term:get(?PT_COUNTERS) of
        _Ref -> ok
    catch
        error:badarg ->
            Ref = atomics:new(?INITIAL_COUNTER_SIZE, [{signed, true}]),
            persistent_term:put(?PT_COUNTERS, Ref),
            %% Atomic slot allocator — single atomics ref with 1 element for the next slot base.
            %% atomics:add_get/3 is truly atomic, preventing concurrent slot collisions.
            Allocator = atomics:new(1, [{signed, false}]),
            atomics:put(Allocator, 1, 1),
            persistent_term:put(?PT_SLOT_ALLOCATOR, Allocator)
    end,
    %% BT-1633: Span ID counter for causal trace linking.
    %% Monotonically incrementing integers, one atomics:add_get/3 per span (~10ns).
    try persistent_term:get(?PT_SPAN_ID_COUNTER) of
        _SpanRef -> ok
    catch
        error:badarg ->
            SpanIdRef = atomics:new(1, [{signed, false}]),
            persistent_term:put(?PT_SPAN_ID_COUNTER, SpanIdRef)
    end.

-doc "Initialise persistent_term defaults.".
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

-doc """
Get or allocate atomics slots for a {Pid, Selector} pair.
Called from the write path (calling process), not the gen_server.
""".
-spec ensure_counter_slot(pid(), atom()) -> {atomics:atomics_ref(), non_neg_integer()}.
ensure_counter_slot(Pid, Selector) ->
    Key = {Pid, Selector},
    case ets:lookup(?AGG_INDEX, Key) of
        [{_Key, AtomicsRef, SlotBase}] ->
            {AtomicsRef, SlotBase};
        [] ->
            allocate_counter_slot(Key)
    end.

-doc """
Allocate new atomics slots for a key.
Uses atomics-style CAS on persistent_term slot counter.
In the rare concurrent allocation race, both callers get valid (different) slots
and the last ets:insert wins — acceptable for first-observation allocation.
""".
allocate_counter_slot(Key) ->
    AtomicsRef = persistent_term:get(?PT_COUNTERS),
    SlotBase = allocate_next_slot(),
    #{size := AtomicsSize} = atomics:info(AtomicsRef),
    %% Grow atomics if needed — serialized through the gen_server (BT-1621)
    %% to prevent concurrent grows from overwriting each other's persistent_term.
    case SlotBase + ?SLOTS_PER_KEY > AtomicsSize of
        true ->
            %% Grow serialized through gen_server (BT-1621). Gracefully degrade
            %% if the trace store is down or slow — observability should never
            %% crash the actor being observed.
            try
                NewAtomicsRef = gen_server:call(
                    ?MODULE, {grow_counters, AtomicsRef, AtomicsSize}, 10000
                ),
                %% Initialise min duration to sentinel so first call sets it
                atomics:put(NewAtomicsRef, SlotBase + ?SLOT_MIN_DURATION, ?MIN_SENTINEL),
                ets:insert(?AGG_INDEX, {Key, NewAtomicsRef, SlotBase}),
                {NewAtomicsRef, SlotBase}
            catch
                exit:{noproc, _} ->
                    %% Trace store down — slot was allocated but can't grow.
                    %% Return a dummy ref that safely absorbs writes.
                    DummyRef = atomics:new(?SLOTS_PER_KEY, [{signed, true}]),
                    {DummyRef, 0};
                exit:{timeout, _} ->
                    DummyRef = atomics:new(?SLOTS_PER_KEY, [{signed, true}]),
                    {DummyRef, 0}
            end;
        false ->
            %% Initialise min duration to sentinel so first call sets it
            atomics:put(AtomicsRef, SlotBase + ?SLOT_MIN_DURATION, ?MIN_SENTINEL),
            ets:insert(?AGG_INDEX, {Key, AtomicsRef, SlotBase}),
            {AtomicsRef, SlotBase}
    end.

-doc """
Atomically allocate the next slot base.
Uses atomics:add_get/3 for true lock-free atomic allocation — no race condition
even under concurrent first-observation of different {Pid, Selector} keys.
""".
allocate_next_slot() ->
    Allocator = persistent_term:get(?PT_SLOT_ALLOCATOR),
    %% add_get returns the NEW value after adding, so subtract to get the old (allocated) base
    NewVal = atomics:add_get(Allocator, 1, ?SLOTS_PER_KEY),
    NewVal - ?SLOTS_PER_KEY.

-doc "Grow the atomics array by doubling its size.".
grow_counters(OldRef, OldSize) ->
    NewSize = OldSize * 2,
    NewRef = atomics:new(NewSize, [{signed, true}]),
    %% Copy existing values
    copy_counters(OldRef, NewRef, 1, OldSize),
    persistent_term:put(?PT_COUNTERS, NewRef),
    %% Update all index entries to point to new ref
    update_counter_refs(NewRef),
    NewRef.

-doc "Copy atomics values from old ref to new ref.".
copy_counters(_OldRef, _NewRef, Idx, MaxIdx) when Idx > MaxIdx ->
    ok;
copy_counters(OldRef, NewRef, Idx, MaxIdx) ->
    Val = atomics:get(OldRef, Idx),
    case Val of
        0 -> ok;
        _ -> atomics:put(NewRef, Idx, Val)
    end,
    copy_counters(OldRef, NewRef, Idx + 1, MaxIdx).

-doc "Update all agg_index entries to point to a new counter ref.".
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

-doc """
Attach telemetry event handlers.
Detach first to handle restarts after crash (terminate/2 not called on kill).
""".
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

-doc "Detach telemetry handlers on shutdown (safe if telemetry not loaded).".
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

-doc "Configure telemetry_poller for periodic VM measurements.".
configure_poller() ->
    %% telemetry_poller is started by the telemetry_poller application.
    %% It emits [vm, memory], [vm, total_run_queue_lengths], etc. by default.
    %% We just need to ensure the application is started.
    _ = application:ensure_all_started(telemetry_poller),
    ok.

%%====================================================================
%% Internal: Sweep / eviction
%%====================================================================

-doc "Start the periodic sweep timer.".
start_sweep_timer() ->
    erlang:send_after(?EVICTION_INTERVAL_MS, self(), sweep_eviction).

-doc "Evict oldest events if over the soft bound.".
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

-doc "Delete the N oldest entries from an ordered_set.".
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

-doc """
Get trace events, filtered by opts map.
Supported opts: actor, selector, class, outcome, min_duration_ns, trace_id (BT-1633).
""".
do_get_traces(Opts) when is_map(Opts) ->
    Pid = maps:get(actor, Opts, undefined),
    Selector = maps:get(selector, Opts, undefined),
    Class = maps:get(class, Opts, undefined),
    Outcome = maps:get(outcome, Opts, undefined),
    MinDurationNs = maps:get(min_duration_ns, Opts, undefined),
    TraceId = maps:get(trace_id, Opts, undefined),
    All = ets:tab2list(?TRACE_EVENTS),
    Filtered = lists:filter(
        fun(Ev) ->
            EvPid = element(2, Ev),
            EvClass = element(3, Ev),
            EvSel = element(4, Ev),
            EvDuration = element(6, Ev),
            EvOutcome = element(7, Ev),
            EvMetadata = element(8, Ev),
            (Pid =:= undefined orelse EvPid =:= Pid) andalso
                (Selector =:= undefined orelse EvSel =:= Selector) andalso
                (Class =:= undefined orelse EvClass =:= Class) andalso
                (Outcome =:= undefined orelse EvOutcome =:= Outcome) andalso
                (MinDurationNs =:= undefined orelse EvDuration >= MinDurationNs) andalso
                (TraceId =:= undefined orelse
                    (is_map(EvMetadata) andalso
                        maps:get(trace_id, EvMetadata, undefined) =:= TraceId))
        end,
        All
    ),
    %% Sort newest first (reverse of ordered_set natural order)
    Sorted = lists:reverse(Filtered),
    [trace_event_to_map(Ev) || Ev <- Sorted].

-doc "Export traces to a JSON file with optional filters.".
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
    JsonBin = beamtalk_json:prettify_term(Export),
    case file:write_file(Path, JsonBin) of
        ok ->
            {ok, #{path => Path, count => Count}};
        {error, Reason} ->
            {error, Reason}
    end.

-doc "Generate a default export path with timestamp.".
default_export_path() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Unique = erlang:unique_integer([positive, monotonic]),
    Filename = io_lib:format(
        "traces-~4..0B-~2..0B-~2..0BT~2..0B-~2..0B-~2..0B-~B.json",
        [Year, Month, Day, Hour, Min, Sec, Unique]
    ),
    iolist_to_binary(Filename).

-doc """
Convert a trace event tuple to a map.
Supports both 9-tuple (with wall-clock) and legacy 8-tuple (without) formats.
""".
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
        %% Base wins on key collisions (metadata cannot override core fields)
        _ -> maps:merge(sanitize_metadata(Metadata), Base)
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
        %% Base wins on key collisions (metadata cannot override core fields)
        _ -> maps:merge(sanitize_metadata(Metadata), Base)
    end.

-doc """
Sanitize metadata map values for JSON serialization (BT-1641).
JSX requires all values to be JSON-compatible (binaries, numbers, booleans,
null, lists, maps). Atom values like `normal` from lifecycle events, pid
values, and tuples must be converted to binary strings.
""".
-spec sanitize_metadata(map()) -> map().
sanitize_metadata(Meta) ->
    maps:fold(
        fun(K, V, Acc) ->
            BinKey =
                case K of
                    _ when is_atom(K) -> atom_to_binary(K, utf8);
                    _ when is_binary(K) -> K;
                    _ -> iolist_to_binary(io_lib:format("~p", [K]))
                end,
            Acc#{BinKey => sanitize_value(V)}
        end,
        #{},
        Meta
    ).

-doc "Convert a single value to a JSON-safe representation.".
-spec sanitize_value(term()) -> term().
sanitize_value(V) when is_binary(V) -> V;
sanitize_value(V) when is_integer(V) -> V;
sanitize_value(V) when is_float(V) -> V;
sanitize_value(true) -> true;
sanitize_value(false) -> false;
sanitize_value(null) -> null;
sanitize_value(undefined) -> null;
sanitize_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
sanitize_value(V) when is_pid(V) -> list_to_binary(pid_to_list(V));
sanitize_value(V) when is_list(V) -> [sanitize_value(E) || E <- V];
sanitize_value(V) when is_map(V) -> sanitize_metadata(V);
sanitize_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

-doc "Get aggregate stats, optionally filtered by Pid.".
do_get_stats(FilterPid) ->
    ets:foldl(
        fun({{Pid, Selector}, CounterRef, SlotBase}, Acc) ->
            case FilterPid =:= undefined orelse Pid =:= FilterPid of
                true ->
                    Stats = read_counter_stats(CounterRef, SlotBase),
                    PidKey = list_to_binary(pid_to_list(Pid)),
                    PidStats =
                        % elp:fixme W0032 maps:find with complex branch logic
                        case maps:find(PidKey, Acc) of
                            {ok, Existing} -> Existing;
                            error -> #{class => lookup_class_for_pid(Pid), methods => #{}}
                        end,
                    Methods = map_get(methods, PidStats),
                    SelKey = atom_to_binary(Selector, utf8),
                    NewMethods = Methods#{SelKey => Stats},
                    Acc#{PidKey => PidStats#{methods => NewMethods}};
                false ->
                    Acc
            end
        end,
        #{},
        ?AGG_INDEX
    ).

-doc "Read stats from atomics slots.".
read_counter_stats(AtomicsRef, SlotBase) ->
    Calls = atomics:get(AtomicsRef, SlotBase + ?SLOT_CALLS),
    Ok = atomics:get(AtomicsRef, SlotBase + ?SLOT_OK),
    Errors = atomics:get(AtomicsRef, SlotBase + ?SLOT_ERRORS),
    Timeouts = atomics:get(AtomicsRef, SlotBase + ?SLOT_TIMEOUTS),
    TotalDuration = atomics:get(AtomicsRef, SlotBase + ?SLOT_TOTAL_DURATION),
    RawMin = atomics:get(AtomicsRef, SlotBase + ?SLOT_MIN_DURATION),
    MaxDuration = atomics:get(AtomicsRef, SlotBase + ?SLOT_MAX_DURATION),
    AvgDuration =
        case Calls of
            0 -> 0;
            _ -> TotalDuration div Calls
        end,
    %% If min is still the sentinel, report 0 (no calls recorded)
    MinDuration =
        case RawMin of
            ?MIN_SENTINEL -> 0;
            _ -> RawMin
        end,
    #{
        calls => Calls,
        ok => Ok,
        errors => Errors,
        timeouts => Timeouts,
        total_duration_ns => TotalDuration,
        avg_duration_ns => AvgDuration,
        min_duration_ns => MinDuration,
        max_duration_ns => MaxDuration
    }.

-doc "Collect all method stats as a flat list for sorting.".
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

-doc "Top N methods by average duration (descending).".
do_slow_methods(Limit) ->
    do_slow_methods(Limit, avg_duration_ns).

-doc "Top N methods sorted by the given duration metric (descending).".
do_slow_methods(Limit, SortBy) when
    SortBy =:= avg_duration_ns;
    SortBy =:= max_duration_ns;
    SortBy =:= min_duration_ns
->
    AllStats = collect_method_stats(),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(SortBy, A) >= maps:get(SortBy, B) end,
        AllStats
    ),
    lists:sublist(Sorted, Limit);
do_slow_methods(Limit, _SortBy) ->
    %% Fall back to avg for unknown sort keys
    do_slow_methods(Limit, avg_duration_ns).

-doc "Top N methods by call count (descending).".
do_hot_methods(Limit) ->
    AllStats = collect_method_stats(),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(calls, A) >= maps:get(calls, B) end,
        AllStats
    ),
    lists:sublist(Sorted, Limit).

-doc "Top N methods by error + timeout rate (descending).".
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

-doc "Top N actors by message queue length (live snapshot).".
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

-doc "Live health for a specific actor.".
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

-doc "VM overview.".
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

-doc """
Convert atom map keys to binary for user-facing output.
Nested maps are converted recursively.
""".
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
            Acc#{BinKey => BinVal}
        end,
        #{},
        Map
    ).

-doc """
Resolve actor class name from the aggregate class table (BT-1640).
The agg class table is populated from telemetry metadata — the authoritative
source — during record_dispatch/6. Falls back to instance registry lookup.
""".
-spec lookup_class_for_pid(pid()) -> binary().
lookup_class_for_pid(Pid) ->
    try ets:lookup(?AGG_CLASS, Pid) of
        [{_, Class}] -> atom_to_binary(Class, utf8);
        [] -> lookup_class_from_registry(Pid)
    catch
        error:badarg -> lookup_class_from_registry(Pid)
    end.

-doc """
Fallback: resolve actor class name from beamtalk_instance_registry.
Returns binary class name or <<"unknown">>.
""".
-spec lookup_class_from_registry(pid()) -> binary().
lookup_class_from_registry(Pid) ->
    try ets:match(beamtalk_instance_registry, {'$1', Pid}) of
        [[Class] | _] -> atom_to_binary(Class, utf8);
        [] -> <<"unknown">>
    catch
        error:badarg -> <<"unknown">>
    end.

%%====================================================================
%% Internal: Clear
%%====================================================================

-doc "Clear all data.".
do_clear() ->
    %% Clear trace events
    ets:delete_all_objects(?TRACE_EVENTS),
    %% Clear agg index and class mappings
    ets:delete_all_objects(?AGG_INDEX),
    ets:delete_all_objects(?AGG_CLASS),
    %% Reset atomics — create a fresh atomics ref
    NewRef = atomics:new(?INITIAL_COUNTER_SIZE, [{signed, true}]),
    persistent_term:put(?PT_COUNTERS, NewRef),
    %% Reset the atomic slot allocator
    Allocator = persistent_term:get(?PT_SLOT_ALLOCATOR),
    atomics:put(Allocator, 1, 1),
    ok.

%%====================================================================
%% Internal: Lock-free CAS loops for min/max
%%====================================================================

-doc """
CAS loop to update the minimum value in an atomics slot.
If NewVal < current value, atomically swap it in. Retries on contention.
""".
-spec cas_min(atomics:atomics_ref(), pos_integer(), integer()) -> ok.
cas_min(Ref, Idx, NewVal) ->
    Current = atomics:get(Ref, Idx),
    case NewVal < Current of
        true ->
            case atomics:compare_exchange(Ref, Idx, Current, NewVal) of
                ok ->
                    ok;
                _OtherVal ->
                    %% Another writer changed it — retry
                    cas_min(Ref, Idx, NewVal)
            end;
        false ->
            ok
    end.

-doc """
CAS loop to update the maximum value in an atomics slot.
If NewVal > current value, atomically swap it in. Retries on contention.
""".
-spec cas_max(atomics:atomics_ref(), pos_integer(), integer()) -> ok.
cas_max(Ref, Idx, NewVal) ->
    Current = atomics:get(Ref, Idx),
    case NewVal > Current of
        true ->
            case atomics:compare_exchange(Ref, Idx, Current, NewVal) of
                ok ->
                    ok;
                _OtherVal ->
                    %% Another writer changed it — retry
                    cas_max(Ref, Idx, NewVal)
            end;
        false ->
            ok
    end.
