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
via atomics:add/3, CAS loops, and ets:insert_new/2 on a key miss.

Storage layout (BT-4103):
- beamtalk_agg_class_stats (ETS set): {Class, Selector} → {Key, CounterRef,
  SlotBase}. The always-on aggregates, bounded by classes × selectors. Rows
  are never deleted once created; `clear/0` zeroes their slots in place and
  readers skip zero-call rows.
- beamtalk_agg_pid (ETS set): {Pid, Selector} → {Key, CounterRef, SlotBase,
  Class}. Per-actor aggregates, written only while tracing is enabled and
  dropped on `disable/0`. Bounded by `?MAX_PID_KEYS`; new keys past the cap
  are not tracked until the sweep reclaims dead pids' rows.
- beamtalk_agg_pid_keys (ETS bag): Pid → Selector, one row per agg_pid key,
  so an actor's rows (and dead pids) are found without scanning agg_pid
- beamtalk_agg_free_slots (ETS ordered_set): {{Shard, CounterRef, SlotBase}}
  for atomics slots released by swept pid rows, reused before the slot
  allocator bumps. The shard spreads concurrent takers across the table so
  they stop racing for a single head. Only pid rows use it, so it
  is idle unless tracing is enabled.
- beamtalk_trace_events (ETS ordered_set): composite key → trace event
- current array triple in persistent_term: lock-free aggregate increments + CAS min/max (~50ns)
- persistent_term toggle: beamtalk_tracing_enabled (true | false)

A grow switches to a fresh array of double the size; rows keep their birth
array forever, so in-flight writers can never lose a sample to a grow (no
copy, no re-pointing). Old arrays are freed once unreferenced; sticky class
rows pin their birth array, so retained arrays total at most ~2× peak.

No monitors, no retirement (BT-4103): per-pid rows used to be folded into a
per-class rollup on each actor's `DOWN` (BT-4066), which monitored every actor
and put ~2 messages per short-lived actor through this gen_server. Now the
class table carries the always-on totals directly — it needs no per-actor
machinery at all — and the enabled-only pid table is swept for dead pids by
the periodic sweep timer (off the dispatch path), with no monitors armed. An
actor's `monitored_by` is never touched by the trace store. Sweeping claims
rows with `ets:take/2`. A caller still mid-write to a slot as it is released
can, in a microseconds-wide window, add one dispatch to the slot's next
owner; slots are zeroed when allocated, not when released. `clear/0` zeroes
the class slots in place rather than replacing the counters. Only pids get
aggregates: a non-pid `pid` in telemetry metadata is dropped, since per-actor
rows are keyed by pid and malformed keys would poison the stats readers.

ETS tables name `beamtalk_ets_heir` as heir for crash resilience and are
reclaimed from it on restart.
Atomics refs stored in persistent_term survive gen_server crashes.

Upgrading from a pre-BT-4103 store resets the aggregates (the old per-pid
index and retired rollup are deleted once, on first boot) but keeps the
captured trace events.

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
    %% Causal trace linking
    next_span_id/0,
    %% Health check
    telemetry_attached/0,
    telemetry_handler_ids/0,
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

%% Atomics slot offsets (per aggregate key: {Class, Selector} or {Pid, Selector})
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
-define(AGG_CLASS_STATS, beamtalk_agg_class_stats).
-define(AGG_PID, beamtalk_agg_pid).
-define(AGG_PID_KEYS, beamtalk_agg_pid_keys).
-define(AGG_FREE_SLOTS, beamtalk_agg_free_slots).
-define(TRACE_EVENTS, beamtalk_trace_events).

%% Pre-BT-4103 aggregate tables, deleted once on upgrade (see
%% reset_aggregates_after_upgrade/0). Trace events keep their table and shape
%% and are preserved across the upgrade.
-define(RETIRED_AGG_TABLES, [beamtalk_agg_index, beamtalk_agg_class, beamtalk_agg_retired]).

%% persistent_term keys. PT_COUNTERS holds the current array triple
%% {Ref, Size, BumpAllocator}; rows keep their birth array forever, so a grow
%% only swaps this triple — nothing is copied or re-pointed.
-define(PT_ENABLED, beamtalk_tracing_enabled).
-define(PT_COUNTERS, beamtalk_trace_counters).
-define(PT_MAX_EVENTS, beamtalk_trace_max_events).
-define(PT_SPAN_ID_COUNTER, beamtalk_span_id_counter).

%% Defaults
-define(DEFAULT_MAX_EVENTS, 100000).
% 1000 unique keys × 8 slots
-define(INITIAL_COUNTER_SIZE, 8000).
-define(EVICTION_INTERVAL_MS, 1000).
-define(EVICTION_PERCENT, 10).
%% Cap on enabled-only per-{Pid, Selector} aggregate keys (BT-4103). New keys
%% past the cap are not tracked until the sweep reclaims dead pids' rows. A
%% soft cap: two callers racing past it can overshoot by a row each.
-define(MAX_PID_KEYS, 100000).
%% Shard count for the free-slots table. Fixed rather than derived
%% from schedulers_online, so releases and takes always agree however many
%% schedulers are online.
-define(FREE_LIST_SHARDS, 64).

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

-doc """
Enable trace event capture and per-actor aggregate stats.
Class/selector aggregates are always on.
""".
-spec enable() -> ok.
enable() ->
    gen_server:call(?MODULE, enable).

-doc """
Disable trace event capture. Drops the per-actor aggregate stats;
class/selector aggregates keep accumulating.
""".
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
Generate the next monotonic span ID for causal trace linking.
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
Class comes from telemetry metadata — the authoritative source.
""".
-spec record_dispatch(term(), term(), non_neg_integer(), atom(), atom(), term()) -> ok.
record_dispatch(Pid, Selector, _Duration, _Outcome, _Mode, Class) when
    not is_pid(Pid); not is_atom(Selector); not is_atom(Class)
->
    %% Malformed metadata (the function is exported and any code can emit the
    %% event): per-actor rows are keyed by pid, and a non-atom class or
    %% selector would poison the stats readers. Drop the sample.
    ok;
record_dispatch(Pid, Selector, Duration, Outcome, Mode, Class) ->
    %% A missing table (the gen_server mid-restart, or a hot upgrade whose new
    %% code runs before code_change/3 creates the new tables) must not raise:
    %% telemetry permanently detaches a handler that does. Drop the sample.
    try
        do_record_dispatch(Pid, Selector, Duration, Outcome, Mode, Class)
    catch
        error:badarg -> ok
    end.

do_record_dispatch(Pid, Selector, Duration, Outcome, _Mode, Class) ->
    %% Always-on aggregates, keyed by {Class, Selector} only: no per-pid ETS
    %% write, no message to the gen_server, no monitor (BT-4103).
    maybe_add_sample(ensure_class_slot(Class, Selector), Duration, Outcome),
    %% Per-actor detail only while tracing is enabled.
    case is_enabled() of
        true ->
            maybe_add_sample(ensure_pid_slot(Pid, Selector, Class), Duration, Outcome);
        false ->
            ok
    end.

-doc """
Add a sample unless the slot lookup answered `dummy` (pid table at cap, or a
grow the trace store did not answer): the sample is dropped without
allocating or writing anything.
""".
maybe_add_sample({AtomicsRef, SlotBase}, Duration, Outcome) ->
    add_sample(AtomicsRef, SlotBase, Duration, Outcome);
maybe_add_sample(dummy, _Duration, _Outcome) ->
    ok.

add_sample(AtomicsRef, SlotBase, Duration, Outcome) ->
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

-doc """
Get aggregate stats: per-{Class, Selector} entries always, plus per-actor
entries while tracing is enabled.
""".
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

-doc """
Get aggregate stats for a specific actor pid. Answers `#{}` unless tracing
is enabled.
""".
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
    %% Only fetch trace context when tracing is enabled to avoid
    %% per-dispatch overhead when trace capture is disabled.
    case is_enabled() of
        true ->
            TraceCtx = beamtalk_actor:get_trace_context(),
            %% Merge causal trace IDs (trace_id, span_id, parent_span_id)
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
    %% Only fetch trace context and build enriched metadata when
    %% tracing is enabled to avoid per-dispatch overhead.
    case is_enabled() of
        true ->
            TraceCtx = beamtalk_actor:get_trace_context(),
            %% Merge causal trace IDs into exception metadata.
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
Handler for [beamtalk, actor, lifecycle, *] events.
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
    %% No cleanup here, even on stop: the class aggregates accumulate, and a
    %% pid row for this actor (tracing enabled) is swept once it is dead.
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
    beamtalk_logging_config:set_domain(runtime),

    %% Create the ETS tables, or take them back from `beamtalk_ets_heir`
    %% after a crash/restart.
    ensure_ets_tables(),

    %% Initialise atomics in persistent_term (if not already present from crash recovery)
    ensure_counters(),

    %% Initialise persistent_term defaults
    init_persistent_terms(),

    %% telemetry and telemetry_poller are declared in beamtalk_runtime.app.src,
    %% so OTP starts and loads their modules before this init/1 runs in normal
    %% application startup. Guard the attach call with a function_exported
    %% check anyway, mirroring the maybe_span/3 and emit/3
    %% pattern in beamtalk_telemetry.erl, so the trace store still degrades
    %% gracefully if it's started outside its OTP application context (e.g.
    %% direct gen_server:start_link/3 in a partial test harness).
    %% function_exported/3 does not load a module, and in an interactive-mode
    %% boot nothing may have loaded `telemetry` yet: load it once here, so
    %% this guard and the beamtalk_telemetry emit guards see it.
    _ = code:ensure_loaded(telemetry),
    case erlang:function_exported(telemetry, attach, 4) of
        true ->
            attach_telemetry_handlers(),
            configure_poller();
        false ->
            ?LOG_INFO("telemetry not loaded, skipping handler attachment", #{})
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
    drop_pid_data(),
    ?LOG_INFO("Trace capture disabled", #{}),
    {reply, ok, State};
handle_call(clear, _From, State) ->
    do_clear(),
    ?LOG_INFO("Trace store cleared", #{}),
    {reply, ok, State};
handle_call({grow_counters, CallerRef}, _From, State) ->
    %% Serialized counters grow to prevent concurrent persistent_term overwrites.
    %% Check if another caller already grew while we were queued.
    {reply, grow_if_current(CallerRef), State};
handle_call({get_traces, Opts}, _From, State) when is_map(Opts) ->
    Result = do_get_traces(Opts),
    {reply, Result, State};
handle_call({export_traces, Opts}, _From, State) ->
    Result = do_export_traces(Opts),
    {reply, Result, State};
handle_call(get_stats, _From, State) ->
    Result = to_bt_keys(do_get_stats(undefined)),
    {reply, Result, State};
handle_call({get_stats, Pid}, _From, State) ->
    Result = to_bt_keys(do_get_stats(Pid)),
    {reply, Result, State};
handle_call({slow_methods, Limit}, _From, State) ->
    Result = [to_bt_keys(M) || M <- do_slow_methods(Limit)],
    {reply, Result, State};
handle_call({slow_methods, Limit, SortBy}, _From, State) ->
    Result = [to_bt_keys(M) || M <- do_slow_methods(Limit, SortBy)],
    {reply, Result, State};
handle_call({hot_methods, Limit}, _From, State) ->
    Result = [to_bt_keys(M) || M <- do_hot_methods(Limit)],
    {reply, Result, State};
handle_call({error_methods, Limit}, _From, State) ->
    Result = [to_bt_keys(M) || M <- do_error_methods(Limit)],
    {reply, Result, State};
handle_call({bottlenecks, Limit}, _From, State) ->
    Result = [to_bt_keys(M) || M <- do_bottlenecks(Limit)],
    {reply, Result, State};
handle_call({actor_health, Pid}, _From, State) ->
    Result = to_bt_keys(do_actor_health(Pid)),
    {reply, Result, State};
handle_call(system_health, _From, State) ->
    Result = to_bt_keys(do_system_health(State)),
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
    %% `beamtalk_ets_heir` handed a table back during init (ensure_ets_tables/0).
    ?LOG_DEBUG("Reclaimed ETS table from heir", #{table => TableName}),
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
    %% Ensure new ETS tables exist after live upgrade (old aggregate tables
    %% are reset by ensure_ets_tables/0).
    ensure_ets_tables(),
    beamtalk_hot_reload:code_change(OldVsn, upgrade_state(State), Extra).

-doc """
Migrate an old state record: pre-BT-4066 `{state, Sweep, Vm}` is already the
current shape, and the BT-4066 `{state, Sweep, Vm, Monitors}` has its monitors
dismantled (BT-4103 arms none, so inherited ones are released rather than
left to linger on actors' `monitored_by`).
""".
upgrade_state({state, SweepTimer, VmStats, Monitors}) when is_map(Monitors) ->
    lists:foreach(
        fun({_Pid, MRef}) -> erlang:demonitor(MRef, [flush]) end,
        maps:to_list(Monitors)
    ),
    #state{sweep_timer = SweepTimer, vm_stats = VmStats};
upgrade_state(State) ->
    State.

%%====================================================================
%% Internal: ETS table management
%%====================================================================

-doc """
Create the ETS tables, or reclaim them from `beamtalk_ets_heir` when they
survived the previous trace store's death (crash resilience, ADR 0069).
""".
ensure_ets_tables() ->
    reset_aggregates_after_upgrade(),
    Opts = [named_table, public, {write_concurrency, true}, {read_concurrency, true}],
    ok = beamtalk_ets_heir:ensure_table(?AGG_CLASS_STATS, [set | Opts]),
    ok = beamtalk_ets_heir:ensure_table(?AGG_PID, [set | Opts]),
    ok = beamtalk_ets_heir:ensure_table(?AGG_PID_KEYS, [bag | Opts]),
    reset_free_slots_after_upgrade(),
    ok = beamtalk_ets_heir:ensure_table(?AGG_FREE_SLOTS, [ordered_set | Opts]),
    ok = beamtalk_ets_heir:ensure_table(?TRACE_EVENTS, [ordered_set | Opts]),
    ok.

-doc """
Reset the aggregates once when booting over pre-BT-4103 tables: the old
per-pid index, pid→class map and retired rollup have no counterpart in the
new layout, and their rows reference slots of an array layout we no longer
share, so they are deleted along with the free list and the counters start
fresh. Trace events keep their table and are preserved. A same-version
restart finds no retired tables and reclaims everything from the heir.
""".
reset_aggregates_after_upgrade() ->
    case lists:any(fun(T) -> ets:whereis(T) =/= undefined end, ?RETIRED_AGG_TABLES) of
        false ->
            ok;
        true ->
            ?LOG_INFO("Resetting pre-BT-4103 aggregate tables", #{}),
            lists:foreach(
                fun(T) -> catch ets:delete(T) end,
                ?RETIRED_AGG_TABLES ++
                    [?AGG_CLASS_STATS, ?AGG_PID, ?AGG_PID_KEYS, ?AGG_FREE_SLOTS]
            ),
            fresh_counters()
    end.

-doc """
Drop a pre-sharding free-slots table: it is a `set` of unsharded
`{{Ref, Base}}` rows, which the sharded take path cannot use. The free list
is a pure reuse cache — no live row references a freed slot — so dropping
it only costs some slot reuse, and `ensure_table/2` recreates it sharded
below. A concurrent writer racing the drop raises `badarg`, which
`record_dispatch/6` already turns into a dropped sample.
""".
reset_free_slots_after_upgrade() ->
    case ets:info(?AGG_FREE_SLOTS) of
        undefined ->
            ok;
        Info ->
            case proplists:get_value(type, Info) of
                ordered_set ->
                    ok;
                _ ->
                    _ = catch ets:delete(?AGG_FREE_SLOTS),
                    ok
            end
    end.

-doc "Ensure the counters triple exists in persistent_term.".
ensure_counters() ->
    try persistent_term:get(?PT_COUNTERS) of
        {_Ref, _Size, _Alloc} ->
            ok;
        %% A stale shape (e.g. the bare ref a pre-BT-4103 store left when
        %% its tables are already gone): start fresh rather than crash
        %% callers on a badmatch.
        _Stale ->
            fresh_counters()
    catch
        error:badarg ->
            fresh_counters()
    end,
    %% Span ID counter for causal trace linking.
    %% Monotonically incrementing integers, one atomics:add_get/3 per span (~10ns).
    try persistent_term:get(?PT_SPAN_ID_COUNTER) of
        _SpanRef -> ok
    catch
        error:badarg ->
            SpanIdRef = atomics:new(1, [{signed, false}]),
            persistent_term:put(?PT_SPAN_ID_COUNTER, SpanIdRef)
    end.

-doc """
Create a fresh counters array and slot allocator, replacing any previous
ones. Only a code upgrade (which deletes the aggregate tables first) or a
fresh boot may call this: every live row references its birth array.
""".
fresh_counters() ->
    Ref = atomics:new(?INITIAL_COUNTER_SIZE, [{signed, true}]),
    %% Atomic slot allocator — single atomics ref with 1 element for the next slot base.
    %% atomics:add_get/3 is truly atomic, preventing concurrent slot collisions.
    Allocator = atomics:new(1, [{signed, false}]),
    atomics:put(Allocator, 1, 1),
    persistent_term:put(?PT_COUNTERS, {Ref, ?INITIAL_COUNTER_SIZE, Allocator}).

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
Get or allocate atomics slots for a {Class, Selector} pair (the always-on
aggregates). Called from the write path (calling process), not the gen_server.
Class rows are sticky — never deleted, never released — so their slots come
from the bump allocator only and the free list stays off the always-on path.
""".
-spec ensure_class_slot(atom(), atom()) ->
    {atomics:atomics_ref(), non_neg_integer()} | dummy.
ensure_class_slot(Class, Selector) ->
    Key = {Class, Selector},
    case ets:lookup(?AGG_CLASS_STATS, Key) of
        [{_Key, AtomicsRef, SlotBase}] ->
            {AtomicsRef, SlotBase};
        [] ->
            MakeRow = fun(Ref, Base) -> {Key, Ref, Base} end,
            case allocate_counter_slot(?AGG_CLASS_STATS, Key, MakeRow, bump) of
                {AtomicsRef, SlotBase, _} ->
                    {AtomicsRef, SlotBase};
                dummy ->
                    dummy
            end
    end.

-doc """
Get or allocate atomics slots for a {Pid, Selector} pair (per-actor detail,
written only while tracing is enabled). The table is capped at
`?MAX_PID_KEYS`: past the cap new keys answer `dummy` (their samples are
dropped without allocating anything) until the sweep reclaims dead pids'
rows. Freed slots are recycled through the free list.
""".
-spec ensure_pid_slot(pid(), atom(), atom()) ->
    {atomics:atomics_ref(), non_neg_integer()} | dummy.
ensure_pid_slot(Pid, Selector, Class) ->
    Key = {Pid, Selector},
    case ets:lookup(?AGG_PID, Key) of
        [Row] ->
            row_slots(Row);
        [] ->
            ensure_pid_slot_capped(Key, Pid, Selector, Class)
    end.

ensure_pid_slot_capped(Key, Pid, Selector, Class) ->
    case ets:info(?AGG_PID, size) >= ?MAX_PID_KEYS of
        true ->
            dummy;
        false ->
            %% The keys entry goes in first: a crash between the two writes
            %% leaves a keys entry with no row (tolerated: the sweep and the
            %% readers skip missing rows), never a row the sweep cannot see.
            %% The bag dedupes the identical object on a lost insert race.
            true = ets:insert(?AGG_PID_KEYS, {Pid, Selector}),
            MakeRow = fun(Ref, Base) -> {Key, Ref, Base, Class} end,
            case allocate_counter_slot(?AGG_PID, Key, MakeRow, free_list) of
                {AtomicsRef, SlotBase, _} ->
                    {AtomicsRef, SlotBase};
                dummy ->
                    dummy
            end
    end.

-doc """
The slot pointer of an aggregate row. Class rows are `{Key, Ref, Base}` and
pid rows are `{Key, Ref, Base, Class}`: the ref and base sit at positions 2
and 3 in both, so the slot machinery below is shape-agnostic.
""".
row_slots({_Key, AtomicsRef, SlotBase}) ->
    {AtomicsRef, SlotBase};
row_slots({_Key, AtomicsRef, SlotBase, _Class}) ->
    {AtomicsRef, SlotBase}.

-doc """
Allocate atomics slots for `Key` in `Table`, building the row with `MakeRow`.

With `free_list` the slot base comes from the free list when a swept pid row
released one, otherwise from the bump allocator; with `bump` it always comes
from the bump allocator (class rows are never released, so they never need
the free list). The slots are zeroed before use, since a reused slot still
holds its previous owner's counts. The row is inserted with
`ets:insert_new/2`: when two processes observe the same key first, the loser
hands its slot back and uses the winner's row, so no slot leaks. Answers
`new` when this call created the row, `existing` when it lost that race, and
`dummy` when the array could not grow (trace store down or slow); the caller
drops the sample, because observability must never crash the actor being
observed.
""".
-spec allocate_counter_slot(atom(), term(), fun(), bump | free_list) ->
    {atomics:atomics_ref(), non_neg_integer(), new | existing} | dummy.
allocate_counter_slot(Table, Key, MakeRow, SlotSource) ->
    case claim_slot(SlotSource) of
        {ok, AtomicsRef, SlotBase} ->
            reset_slot(AtomicsRef, SlotBase),
            case ets:insert_new(Table, MakeRow(AtomicsRef, SlotBase)) of
                true ->
                    {AtomicsRef, SlotBase, new};
                false ->
                    release_slot(AtomicsRef, SlotBase),
                    case ets:lookup(Table, Key) of
                        [WinnerRow] ->
                            {WinnerRef, WinnerBase} = row_slots(WinnerRow),
                            {WinnerRef, WinnerBase, existing};
                        [] ->
                            %% The winner's row was swept in between — retry.
                            allocate_counter_slot(Table, Key, MakeRow, SlotSource)
                    end
            end;
        error ->
            dummy
    end.

-doc """
Claim a slot: from the free list when `SlotSource` is `free_list` and one is
free, else by bumping the current array's allocator (growing first when the
array is full). Answers `error` when a grow was needed but the trace store
did not answer; the caller then drops the sample.
""".
-spec claim_slot(bump | free_list) ->
    {ok, atomics:atomics_ref(), non_neg_integer()} | error.
claim_slot(free_list) ->
    case take_slot() of
        {Ref, Base} -> {ok, Ref, Base};
        none -> claim_bump_slot()
    end;
claim_slot(bump) ->
    claim_bump_slot().

-doc """
Bump-allocate from the current array. The base is bounds-checked before use:
a caller that read a stale triple (a grow switched arrays in between) or
that genuinely overran the array grows and retries on the fresh array.
`add_get/3` returns the new value, so the base is the old one.
""".
claim_bump_slot() ->
    {Ref, Size, Alloc} = persistent_term:get(?PT_COUNTERS),
    Base = atomics:add_get(Alloc, 1, ?SLOTS_PER_KEY) - ?SLOTS_PER_KEY,
    case Base + ?SLOTS_PER_KEY =< Size of
        true ->
            {ok, Ref, Base};
        false ->
            grow_and_claim(Ref)
    end.

-doc """
Grow past `OldRef`'s array, then bump-allocate from the fresh array. When the
fresh array is already overrun too (many writers racing past it), grow again:
arrays double, so the loop ends.
""".
grow_and_claim(OldRef) ->
    case request_grow(OldRef) of
        {ok, {NewRef, NewSize, NewAlloc}} ->
            Base = atomics:add_get(NewAlloc, 1, ?SLOTS_PER_KEY) - ?SLOTS_PER_KEY,
            case Base + ?SLOTS_PER_KEY =< NewSize of
                true ->
                    {ok, NewRef, Base};
                false ->
                    grow_and_claim(NewRef)
            end;
        error ->
            error
    end.

-doc """
Grow the counters, serialized through the gen_server so concurrent grows
cannot overwrite each other's persistent_term. Answers the current array
triple. Nothing in the gen_server allocates, so this always calls.
""".
-spec request_grow(atomics:atomics_ref()) ->
    {ok, {atomics:atomics_ref(), pos_integer(), atomics:atomics_ref()}} | error.
request_grow(AtomicsRef) ->
    try gen_server:call(?MODULE, {grow_counters, AtomicsRef}, 10000) of
        {ok, Triple} -> {ok, Triple}
    catch
        %% Trace store down, restarting or slow.
        exit:_ -> error
    end.

-doc """
Zero a slot group before (re)use; min starts at the sentinel so the first
call sets it.
""".
reset_slot(AtomicsRef, SlotBase) ->
    lists:foreach(
        fun(Offset) -> atomics:put(AtomicsRef, SlotBase + Offset, 0) end,
        [
            ?SLOT_CALLS,
            ?SLOT_OK,
            ?SLOT_ERRORS,
            ?SLOT_TIMEOUTS,
            ?SLOT_TOTAL_DURATION,
            ?SLOT_MAX_DURATION
        ]
    ),
    atomics:put(AtomicsRef, SlotBase + ?SLOT_MIN_DURATION, ?MIN_SENTINEL).

-doc """
Take a released slot if one is free. Rows are `{{Shard, Ref, Base}}` in an
`ordered_set`, so `ets:next/2` from a shard's lower bound jumps straight to
that shard's first entry. The taker probes from its hashed shard and walks
on — to the next shard on an empty shard or a lost `ets:take/2` race — so
concurrent takers claim different slots instead of racing for one head.
Answers `none` when the table is empty or every probe missed;
the caller then bumps a fresh slot, so a miss only costs reuse, never a
sample. `ets:take/2` hands a slot to exactly one caller, so a taken slot is
never double-allocated.
""".
-spec take_slot() -> {atomics:atomics_ref(), non_neg_integer()} | none.
take_slot() ->
    %% Fast path: probing every shard of an empty table on each call would
    %% tax the drained-tail window where the free list sits empty.
    case ets:info(?AGG_FREE_SLOTS, size) of
        0 ->
            none;
        _ ->
            take_slot_shard(erlang:phash2(self(), ?FREE_LIST_SHARDS), ?FREE_LIST_SHARDS)
    end.

take_slot_shard(_Shard, 0) ->
    none;
take_slot_shard(Shard, TriesLeft) ->
    %% `{Shard, 0, 0}` is below every `{Shard, Ref, Base}` key: 0 is below
    %% every atomics ref in the term order.
    case ets:next(?AGG_FREE_SLOTS, {Shard, 0, 0}) of
        {Shard, Ref, Base} = Key ->
            case ets:take(?AGG_FREE_SLOTS, Key) of
                [{Key}] ->
                    {Ref, Base};
                [] ->
                    take_slot_shard((Shard + 1) rem ?FREE_LIST_SHARDS, TriesLeft - 1)
            end;
        _Other ->
            take_slot_shard((Shard + 1) rem ?FREE_LIST_SHARDS, TriesLeft - 1)
    end.

-doc """
Put a slot on the free list for reuse. The shard stripes entries across the
table: consecutive `unique_integer` values land on consecutive shards, so a
sweep's sequential releases spread over all of them. Each release must
pair with an exclusive claim: a twice-released slot would be taken twice.
""".
-spec release_slot(atomics:atomics_ref(), non_neg_integer()) -> true.
release_slot(AtomicsRef, SlotBase) ->
    Shard = erlang:unique_integer([monotonic, positive]) rem ?FREE_LIST_SHARDS,
    ets:insert(?AGG_FREE_SLOTS, {{Shard, AtomicsRef, SlotBase}}).

-doc """
Grow the counters unless another caller already has (the current ref is no
longer `CallerRef`). Answers `{ok, Triple}` with the current array triple.
Runs in the gen_server.
""".
grow_if_current(CallerRef) ->
    case persistent_term:get(?PT_COUNTERS) of
        {CallerRef, _Size, _Alloc} -> {ok, grow_counters()};
        Current -> {ok, Current}
    end.

-doc """
Grow the counters by switching to a fresh array of double the size. Rows keep
their birth array forever — nothing is copied and no row is re-pointed — so a
writer holding a stale ref keeps writing valid counters. Old arrays are freed
once no row or free-list entry references them; sticky class rows pin their
birth array, so the retained arrays total at most ~2× the peak size.
""".
grow_counters() ->
    {_OldRef, OldSize, _OldAlloc} = persistent_term:get(?PT_COUNTERS),
    NewSize = OldSize * 2,
    NewRef = atomics:new(NewSize, [{signed, true}]),
    NewAlloc = atomics:new(1, [{signed, false}]),
    atomics:put(NewAlloc, 1, 1),
    New = {NewRef, NewSize, NewAlloc},
    persistent_term:put(?PT_COUNTERS, New),
    New.

%%====================================================================
%% Internal: Class resolution for health queries
%%====================================================================

-doc """
Resolve the class atom from beamtalk_instance_registry, else `unknown`. A
full-table scan per call: fine for one actor's health, but batch callers
should use `bottlenecks/1` (one scan total) rather than polling per actor.
""".
-spec class_from_registry(pid()) -> atom().
class_from_registry(Pid) ->
    try ets:match(beamtalk_instance_registry, {'$1', Pid}) of
        [[Class] | _] when is_atom(Class) -> Class;
        _ -> unknown
    catch
        error:badarg -> unknown
    end.

%%====================================================================
%% Internal: Telemetry handlers
%%====================================================================

-doc """
The trace store's telemetry handlers: `{HandlerId, EventName, HandlerFun}`.
One list drives both attach and detach so they cannot drift apart.
""".
telemetry_handlers() ->
    [
        {beamtalk_trace_store_dispatch_stop, [beamtalk, actor, dispatch, stop],
            fun ?MODULE:handle_dispatch_stop/4},
        {beamtalk_trace_store_dispatch_exception, [beamtalk, actor, dispatch, exception],
            fun ?MODULE:handle_dispatch_exception/4},
        %% Lifecycle event handlers (start, stop, kill)
        {beamtalk_trace_store_lifecycle_start, [beamtalk, actor, lifecycle, start],
            fun ?MODULE:handle_lifecycle_event/4},
        {beamtalk_trace_store_lifecycle_stop, [beamtalk, actor, lifecycle, stop],
            fun ?MODULE:handle_lifecycle_event/4},
        {beamtalk_trace_store_lifecycle_kill, [beamtalk, actor, lifecycle, kill],
            fun ?MODULE:handle_lifecycle_event/4},
        {beamtalk_trace_store_vm_measurements, [vm, memory], fun ?MODULE:handle_vm_measurements/4}
    ].

-doc "The trace store's telemetry handler ids (refused by `Telemetry detach:`).".
-spec telemetry_handler_ids() -> [atom()].
telemetry_handler_ids() ->
    [Id || {Id, _EventName, _Handler} <- telemetry_handlers()].

-doc """
Attach telemetry event handlers.
Detach first to handle restarts after crash (terminate/2 not called on kill).
""".
attach_telemetry_handlers() ->
    detach_telemetry_handlers(),
    lists:foreach(
        fun({Id, EventName, Handler}) ->
            ok = telemetry:attach(Id, EventName, Handler, #{})
        end,
        telemetry_handlers()
    ).

-doc """
Detach telemetry handlers on shutdown (safe if telemetry not loaded or handler
table not running — `beamtalk_telemetry:detach/1` is idempotent).
""".
detach_telemetry_handlers() ->
    lists:foreach(
        fun({Id, _EventName, _Handler}) -> beamtalk_telemetry:detach_handler(Id) end,
        telemetry_handlers()
    ).

-doc "Configure telemetry_poller for periodic VM measurements.".
configure_poller() ->
    %% telemetry_poller is declared as a dependency in beamtalk_runtime.app.src
    %% and started automatically by OTP. It emits [vm, memory],
    %% [vm, total_run_queue_lengths], etc. by default.
    ok.

%%====================================================================
%% Internal: Sweep / eviction
%%====================================================================

-doc "Start the periodic sweep timer.".
start_sweep_timer() ->
    erlang:send_after(?EVICTION_INTERVAL_MS, self(), sweep_eviction).

-doc "Evict oldest events if over the soft bound; sweep dead pids' pid rows.".
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
    end,
    case is_enabled() of
        %% The pid table is empty unless tracing is enabled (disable/0 drops
        %% it), so there is nothing to sweep while disabled.
        true -> sweep_dead_pids();
        false -> ok
    end.

-doc """
Delete the pid rows of dead actors, releasing their slots (BT-4103). This is
what bounds the enabled-only pid table: no monitors are armed, so a dead
pid's rows linger until this sweep (at most one eviction interval). Each row
is claimed with `ets:take/2`: a row a caller re-creates for the pid afterwards
(a late dispatch for a dead actor) stays until the next sweep.
""".
sweep_dead_pids() ->
    case ets:info(?AGG_PID, size) of
        0 ->
            ok;
        _ ->
            Pids = lists:usort(
                ets:foldl(fun({Pid, _Selector}, Acc) -> [Pid | Acc] end, [], ?AGG_PID_KEYS)
            ),
            lists:foreach(fun sweep_pid_rows/1, Pids)
    end.

sweep_pid_rows(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            ok;
        false ->
            Keys = ets:take(?AGG_PID_KEYS, Pid),
            lists:foreach(
                fun({_Pid, Selector}) ->
                    case ets:take(?AGG_PID, {Pid, Selector}) of
                        [{_Key, Ref, SlotBase, _Class}] -> release_slot(Ref, SlotBase);
                        [] -> ok
                    end
                end,
                Keys
            )
    end.

-doc """
Drop all per-actor aggregate data: every pid row's slot is claimed with
`ets:take/2` and released to the free list. Runs in the gen_server on
`disable/0` and as part of `clear/0`. The freed slots stay on the free list
as a reuse cache: disabling drops the rows but does not shrink the arrays.
""".
drop_pid_data() ->
    ets:delete_all_objects(?AGG_PID_KEYS),
    take_all_and_release(?AGG_PID).

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
Supported opts: actor, selector, class, outcome, min_duration_ns, trace_id.
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
    %% BT-4173: keys and display-text values are Strings; numbers stay.
    S = fun(Bin) -> beamtalk_primitive:string_from_binary(Bin) end,
    Base = #{
        S(<<"timestamp_us">>) => WallClockUs,
        S(<<"actor">>) => S(list_to_binary(pid_to_list(Pid))),
        S(<<"class">>) => S(atom_to_binary(Class, utf8)),
        S(<<"selector">>) => S(atom_to_binary(Selector, utf8)),
        S(<<"mode">>) => S(atom_to_binary(Mode, utf8)),
        S(<<"duration_us">>) => DurationNs div 1000,
        S(<<"outcome">>) => S(atom_to_binary(Outcome, utf8))
    },
    case maps:size(Metadata) of
        0 -> Base;
        %% Base wins on key collisions (metadata cannot override core fields)
        _ -> maps:merge(tag_metadata_keys(sanitize_metadata(Metadata)), Base)
    end;
trace_event_to_map({
    {MonotonicNs, _Unique}, Pid, Class, Selector, Mode, DurationNs, Outcome, Metadata
}) ->
    %% Legacy format: fall back to monotonic-derived timestamp
    S = fun(Bin) -> beamtalk_primitive:string_from_binary(Bin) end,
    Base = #{
        S(<<"timestamp_us">>) => MonotonicNs div 1000,
        S(<<"actor">>) => S(list_to_binary(pid_to_list(Pid))),
        S(<<"class">>) => S(atom_to_binary(Class, utf8)),
        S(<<"selector">>) => S(atom_to_binary(Selector, utf8)),
        S(<<"mode">>) => S(atom_to_binary(Mode, utf8)),
        S(<<"duration_us">>) => DurationNs div 1000,
        S(<<"outcome">>) => S(atom_to_binary(Outcome, utf8))
    },
    case maps:size(Metadata) of
        0 -> Base;
        %% Base wins on key collisions (metadata cannot override core fields)
        _ -> maps:merge(tag_metadata_keys(sanitize_metadata(Metadata)), Base)
    end.

-doc """
Sanitize metadata map values for JSON serialization.
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

-doc """
Tag sanitized metadata keys as Strings for Beamtalk lookup. Values pass
through untouched: a user Binary stays a Binary (only generated display text
would want tagging, and the two are indistinguishable here).
""".
-spec tag_metadata_keys(map()) -> map().
tag_metadata_keys(Meta) ->
    maps:from_list([
        {beamtalk_primitive:string_from_binary(K), V}
     || {K, V} <- maps:to_list(Meta)
    ]).

-doc "Convert a single value to a JSON-safe representation.".
-spec sanitize_value(term()) -> term().
sanitize_value({beamtalk_string, Bin}) when is_binary(Bin) ->
    %% BT-4173: unwrap — without this a String falls into the ~p catch-all.
    Bin;
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

-doc """
Get aggregate stats, optionally filtered by Pid.

Unfiltered, the map holds one `<<"class:Class">>` entry per {Class, Selector}
seen (the always-on aggregates), plus — only while tracing is enabled — one
entry per pid with per-actor detail. Filtered by pid, it holds that actor's
entry while tracing is enabled and `#{}` otherwise. Rows whose slots were
zeroed by `clear/0` but kept (class rows are never deleted) read as zero
calls and are skipped, so a cleared store reports `#{}`.
""".
do_get_stats(undefined) ->
    ClassEntries = ets:foldl(
        fun({{Class, Selector}, CounterRef, SlotBase}, Acc) ->
            ClassBin = atom_to_binary(Class, utf8),
            add_method_stats_nonzero(
                <<"class:", ClassBin/binary>>,
                fun() -> #{class => ClassBin, methods => #{}} end,
                Selector,
                read_counter_stats(CounterRef, SlotBase),
                Acc
            )
        end,
        #{},
        ?AGG_CLASS_STATS
    ),
    case is_enabled() of
        true ->
            ets:foldl(
                fun({{Pid, Selector}, CounterRef, SlotBase, Class}, Acc) ->
                    add_method_stats_nonzero(
                        list_to_binary(pid_to_list(Pid)),
                        fun() -> #{class => atom_to_binary(Class, utf8), methods => #{}} end,
                        Selector,
                        read_counter_stats(CounterRef, SlotBase),
                        Acc
                    )
                end,
                ClassEntries,
                ?AGG_PID
            );
        false ->
            ClassEntries
    end;
do_get_stats(FilterPid) ->
    case is_enabled() of
        false ->
            %% Per-actor detail exists only while tracing is enabled.
            #{};
        true ->
            lists:foldl(
                fun({Pid, Selector}, Acc) ->
                    case ets:lookup(?AGG_PID, {Pid, Selector}) of
                        [{_Key, CounterRef, SlotBase, Class}] ->
                            add_method_stats_nonzero(
                                list_to_binary(pid_to_list(Pid)),
                                fun() ->
                                    #{class => atom_to_binary(Class, utf8), methods => #{}}
                                end,
                                Selector,
                                read_counter_stats(CounterRef, SlotBase),
                                Acc
                            );
                        [] ->
                            Acc
                    end
                end,
                #{},
                ets:lookup(?AGG_PID_KEYS, FilterPid)
            )
    end.

-doc "Add one selector's stats unless it has zero calls (a cleared-kept row).".
add_method_stats_nonzero(_EntryKey, _NewEntry, _Selector, #{calls := 0}, Acc) ->
    Acc;
add_method_stats_nonzero(EntryKey, NewEntry, Selector, Stats, Acc) ->
    add_method_stats(EntryKey, NewEntry, Selector, Stats, Acc).

-doc "Add one selector's stats under `EntryKey`, creating the entry with `NewEntry`.".
add_method_stats(EntryKey, NewEntry, Selector, Stats, Acc) ->
    Entry =
        case Acc of
            #{EntryKey := Existing} -> Existing;
            _ -> NewEntry()
        end,
    Methods = map_get(methods, Entry),
    SelKey = atom_to_binary(Selector, utf8),
    Acc#{EntryKey => Entry#{methods => Methods#{SelKey => Stats}}}.

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

-doc """
Collect all method stats as a flat list for sorting: one entry per
{Class, Selector} (the always-on aggregates), with no pid — a class entry
covers every actor of that class. Zero-call rows (cleared but kept) are
skipped.
""".
collect_method_stats() ->
    ets:foldl(
        fun({{Class, Selector}, CounterRef, SlotBase}, Acc) ->
            case read_counter_stats(CounterRef, SlotBase) of
                #{calls := 0} ->
                    Acc;
                Stats ->
                    [
                        Stats#{
                            class => atom_to_binary(Class, utf8),
                            selector => Selector
                        }
                        | Acc
                    ]
            end
        end,
        [],
        ?AGG_CLASS_STATS
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

-doc """
Top N actors by message queue length (live snapshot). Scans the instance
registry, so it covers every live actor whether or not tracing is enabled —
unregistered processes never appear.
""".
do_bottlenecks(Limit) ->
    %% Get all unique {Class, Pid} pairs from the instance registry,
    %% deduplicated by pid (a pid registered under two classes must still
    %% report once).
    Pairs = lists:usort(
        try
            ets:foldl(
                fun
                    ({Class, Pid}, Acc) when is_atom(Class), is_pid(Pid) ->
                        [{Class, Pid} | Acc];
                    %% Another writer's malformed row: skip, never crash
                    %% the query.
                    (_Row, Acc) ->
                        Acc
                end,
                [],
                beamtalk_instance_registry
            )
        catch
            %% Registry table missing (partial harness): no actors to report.
            error:badarg -> []
        end
    ),
    ByPid = maps:values(maps:from_list([{Pid, {Class, Pid}} || {Class, Pid} <- Pairs])),
    %% Get queue depths for live processes
    WithQueue = lists:filtermap(
        fun({Class, Pid}) ->
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
                                class => atom_to_binary(Class, utf8),
                                queue_depth => QLen,
                                memory_kb => Mem div 1024
                            }}
                    end;
                false ->
                    false
            end
        end,
        ByPid
    ),
    Sorted = lists:sort(
        fun(A, B) -> maps:get(queue_depth, A) >= maps:get(queue_depth, B) end,
        WithQueue
    ),
    lists:sublist(Sorted, Limit).

-doc "Live health for a specific actor.".
do_actor_health(Pid) ->
    ClassBin = atom_to_binary(class_from_registry(Pid), utf8),
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
                        class => ClassBin,
                        status => dead,
                        error => <<"process not alive">>
                    };
                Info ->
                    #{
                        pid => list_to_binary(pid_to_list(Pid)),
                        class => ClassBin,
                        status => proplists:get_value(status, Info),
                        queue_depth => proplists:get_value(message_queue_len, Info, 0),
                        memory_kb => proplists:get_value(memory, Info, 0) div 1024,
                        reductions => proplists:get_value(reductions, Info, 0)
                    }
            end;
        false ->
            #{
                pid => list_to_binary(pid_to_list(Pid)),
                class => ClassBin,
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
Convert diagnostic maps to user-facing output (BT-4173): text keys and text
values become Strings, nested maps and lists recurse. Atoms (statuses,
selectors), numbers, booleans and nil pass through — Beamtalk compares them
to symbols/numbers. Every binary here is generated display text, never a
user byte payload, so tagging all of them is safe.
""".
-spec to_bt_keys(map()) -> map().
to_bt_keys(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            RawKey =
                case K of
                    _ when is_atom(K) -> atom_to_binary(K, utf8);
                    _ when is_binary(K) -> K;
                    _ -> iolist_to_binary(io_lib:format("~p", [K]))
                end,
            Acc#{beamtalk_primitive:string_from_binary(RawKey) => to_bt_value(V)}
        end,
        #{},
        Map
    ).

-spec to_bt_value(term()) -> term().
to_bt_value(V) when is_map(V) -> to_bt_keys(V);
to_bt_value(V) when is_list(V) -> [to_bt_value(E) || E <- V];
to_bt_value(V) when is_binary(V) -> beamtalk_primitive:string_from_binary(V);
to_bt_value(V) -> V.

%%====================================================================
%% Internal: Clear
%%====================================================================

-doc "Clear all data.".
do_clear() ->
    %% Clear trace events
    ets:delete_all_objects(?TRACE_EVENTS),
    %% Clear the aggregates. The atomics array and slot allocator are kept:
    %% class rows stay (their slots are zeroed in place, and readers skip
    %% zero-call rows), while pid rows are dropped and their slots released
    %% for reuse. Resetting the allocator instead would let a slot held by a
    %% concurrent writer be handed out twice. A dispatch racing the zeroing
    %% lands in a zeroed slot, so clear-during-traffic is approximate — the
    %% same tolerance class `clear/0` always had.
    zero_class_slots(),
    drop_pid_data(),
    ok.

-doc "Zero every class-stats slot in place, keeping the rows.".
zero_class_slots() ->
    ets:foldl(
        fun({_Key, CounterRef, SlotBase}, Acc) ->
            reset_slot(CounterRef, SlotBase),
            Acc
        end,
        ok,
        ?AGG_CLASS_STATS
    ).

-doc """
Remove every row of a slot table, releasing each claimed slot. The slot base
sits at element 3 in both class rows ({Key, Ref, Base}) and pid rows
({Key, Ref, Base, Class}).
""".
take_all_and_release(Table) ->
    Keys = ets:foldl(fun(Row, Acc) -> [element(1, Row) | Acc] end, [], Table),
    lists:foreach(
        fun(Key) ->
            case ets:take(Table, Key) of
                [Taken] -> release_slot(element(2, Taken), element(3, Taken));
                [] -> ok
            end
        end,
        Keys
    ).

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
