%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Erlang shim for the Tracing stdlib class (ADR 0069 Phase 3).
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Delegates all operations to beamtalk_trace_store. This shim exists
%%% so that the Beamtalk Tracing class can use the `(Erlang module) selector`
%%% FFI pattern. It also handles unwrapping #beamtalk_object{} records to
%%% extract pids for actor-specific queries.
%%%
%%% FFI name mapping: the Beamtalk `(Erlang mod) selector: arg` syntax
%%% calls the Erlang function named after the first keyword WITHOUT the
%%% trailing colon. Multi-keyword selectors use the first keyword as the
%%% function name, arity = number of arguments.
%%%
%%%   (Erlang beamtalk_tracing) enable          → enable/0
%%%   (Erlang beamtalk_tracing) tracesFor: a    → tracesFor/1
%%%   (Erlang beamtalk_tracing) tracesFor: a selector: s → tracesFor/2
%%%   (Erlang beamtalk_tracing) maxEvents: sz   → maxEvents/1
%%%   (Erlang beamtalk_tracing) maxEvents       → maxEvents/0
%%%
%%% @see beamtalk_trace_store
%%% @see docs/ADR/0069-actor-observability-and-tracing.md
-module(beamtalk_tracing).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    %% Lifecycle
    enable/0,
    disable/0,
    isEnabled/0,
    clear/0,
    %% Trace queries
    traces/0,
    tracesFor/1,
    tracesFor/2,
    %% Aggregate stats
    stats/0,
    statsFor/1,
    %% Analysis
    slowMethods/1,
    hotMethods/1,
    errorMethods/1,
    bottlenecks/1,
    %% Live health
    healthFor/1,
    systemHealth/0,
    %% Configuration
    maxEvents/0,
    maxEvents/1
]).

%%====================================================================
%% Lifecycle
%%====================================================================

%% @doc Enable trace event capture.
%% Logs a warning if telemetry handlers are not attached (e.g. telemetry
%% not on the code path), since tracing will be enabled but non-functional.
%% Always returns nil — the warning is advisory, not fatal.
-spec enable() -> nil.
enable() ->
    call_trace_store(fun beamtalk_trace_store:enable/0),
    case beamtalk_trace_store:telemetry_attached() of
        true ->
            nil;
        false ->
            ?LOG_WARNING(
                "Tracing enabled but telemetry handlers are not attached. "
                "Traces and stats will be empty. This usually means the Beamtalk "
                "installation is incomplete — try reinstalling or updating.",
                [],
                #{domain => [beamtalk, runtime]}
            ),
            nil
    end.

%% @doc Disable trace event capture.
-spec disable() -> nil.
disable() ->
    call_trace_store(fun beamtalk_trace_store:disable/0),
    nil.

%% @doc Check whether trace capture is active.
-spec isEnabled() -> boolean().
isEnabled() ->
    %% is_enabled reads persistent_term directly, no gen_server call needed.
    %% Safe even when trace store is not running.
    beamtalk_trace_store:is_enabled().

%% @doc Clear all trace events and aggregate stats.
-spec clear() -> nil.
clear() ->
    call_trace_store(fun beamtalk_trace_store:clear/0),
    nil.

%%====================================================================
%% Trace event queries
%%====================================================================

%% @doc All captured trace events, newest first.
%% Returns [] when not enabled or store not running.
-spec traces() -> [map()].
traces() ->
    call_trace_store_default(fun beamtalk_trace_store:get_traces/0, []).

%% @doc Trace events for a specific actor.
%% FFI: (Erlang beamtalk_tracing) tracesFor: actor
-spec tracesFor(term()) -> [map()].
tracesFor(Actor) ->
    Pid = extract_pid(Actor),
    call_trace_store_default(fun() -> beamtalk_trace_store:get_traces(Pid) end, []).

%% @doc Trace events for a specific actor + selector.
%% FFI: (Erlang beamtalk_tracing) tracesFor: actor selector: sel
-spec tracesFor(term(), atom()) -> [map()].
tracesFor(Actor, Selector) ->
    Pid = extract_pid(Actor),
    call_trace_store_default(fun() -> beamtalk_trace_store:get_traces(Pid, Selector) end, []).

%%====================================================================
%% Aggregate stats
%%====================================================================

%% @doc Per-actor, per-method aggregate stats.
-spec stats() -> map().
stats() ->
    call_trace_store_default(fun beamtalk_trace_store:get_stats/0, #{}).

%% @doc Aggregate stats for a specific actor.
%% Returns #{} for unknown actors.
%% FFI: (Erlang beamtalk_tracing) statsFor: actor
-spec statsFor(term()) -> map().
statsFor(Actor) ->
    Pid = extract_pid(Actor),
    call_trace_store_default(fun() -> beamtalk_trace_store:get_stats(Pid) end, #{}).

%%====================================================================
%% Analysis
%%====================================================================

%% @doc Top N methods by average duration (descending).
%% FFI: (Erlang beamtalk_tracing) slowMethods: limit
-spec slowMethods(integer()) -> [map()].
slowMethods(Limit) when is_integer(Limit), Limit > 0 ->
    call_trace_store_default(fun() -> beamtalk_trace_store:slow_methods(Limit) end, []);
slowMethods(Limit) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = 'slowMethods:',
        message = iolist_to_binary(
            io_lib:format("slowMethods: requires a positive Integer, got: ~p", [Limit])
        ),
        hint = <<"Pass a positive integer, e.g. Tracing slowMethods: 10">>,
        details = #{}
    }).

%% @doc Top N methods by call count (descending).
%% FFI: (Erlang beamtalk_tracing) hotMethods: limit
-spec hotMethods(integer()) -> [map()].
hotMethods(Limit) when is_integer(Limit), Limit > 0 ->
    call_trace_store_default(fun() -> beamtalk_trace_store:hot_methods(Limit) end, []);
hotMethods(Limit) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = 'hotMethods:',
        message = iolist_to_binary(
            io_lib:format("hotMethods: requires a positive Integer, got: ~p", [Limit])
        ),
        hint = <<"Pass a positive integer, e.g. Tracing hotMethods: 10">>,
        details = #{}
    }).

%% @doc Top N methods by error + timeout rate (descending).
%% FFI: (Erlang beamtalk_tracing) errorMethods: limit
-spec errorMethods(integer()) -> [map()].
errorMethods(Limit) when is_integer(Limit), Limit > 0 ->
    call_trace_store_default(fun() -> beamtalk_trace_store:error_methods(Limit) end, []);
errorMethods(Limit) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = 'errorMethods:',
        message = iolist_to_binary(
            io_lib:format("errorMethods: requires a positive Integer, got: ~p", [Limit])
        ),
        hint = <<"Pass a positive integer, e.g. Tracing errorMethods: 5">>,
        details = #{}
    }).

%% @doc Top N actors by message queue length (live snapshot).
%% FFI: (Erlang beamtalk_tracing) bottlenecks: limit
-spec bottlenecks(integer()) -> [map()].
bottlenecks(Limit) when is_integer(Limit), Limit > 0 ->
    call_trace_store_default(fun() -> beamtalk_trace_store:bottlenecks(Limit) end, []);
bottlenecks(Limit) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = 'bottlenecks:',
        message = iolist_to_binary(
            io_lib:format("bottlenecks: requires a positive Integer, got: ~p", [Limit])
        ),
        hint = <<"Pass a positive integer, e.g. Tracing bottlenecks: 5">>,
        details = #{}
    }).

%%====================================================================
%% Live process health
%%====================================================================

%% @doc Live process health for a specific actor.
%% FFI: (Erlang beamtalk_tracing) healthFor: actor
-spec healthFor(term()) -> map().
healthFor(Actor) ->
    Pid = extract_pid(Actor),
    call_trace_store_default(
        fun() -> beamtalk_trace_store:actor_health(Pid) end,
        #{pid => Pid, status => dead, error => <<"process not alive">>}
    ).

%% @doc VM overview: scheduler count, memory, process count, run queue lengths.
-spec systemHealth() -> map().
systemHealth() ->
    call_trace_store_default(fun beamtalk_trace_store:system_health/0, #{}).

%%====================================================================
%% Configuration
%%====================================================================

%% @doc Current ring buffer capacity.
-spec maxEvents() -> integer().
maxEvents() ->
    beamtalk_trace_store:max_events().

%% @doc Set ring buffer capacity.
%% FFI: (Erlang beamtalk_tracing) maxEvents: size
-spec maxEvents(integer()) -> nil.
maxEvents(Size) when is_integer(Size), Size > 0 ->
    call_trace_store(fun() -> beamtalk_trace_store:max_events(Size) end),
    nil;
maxEvents(Size) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = 'maxEvents:',
        message = iolist_to_binary(
            io_lib:format("maxEvents: requires a positive Integer, got: ~p", [Size])
        ),
        hint = <<"Pass a positive integer, e.g. Tracing maxEvents: 50000">>,
        details = #{}
    }).

%%====================================================================
%% Internal helpers
%%====================================================================

%% @private Extract a pid from a beamtalk_object record, a raw pid, or a dead reference.
-spec extract_pid(term()) -> pid().
extract_pid(#beamtalk_object{pid = Pid}) ->
    Pid;
extract_pid(Pid) when is_pid(Pid) -> Pid;
extract_pid(Other) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'Tracing',
        selector = undefined,
        message = iolist_to_binary(
            io_lib:format("Expected an actor or pid, got: ~p", [Other])
        ),
        hint = <<"Pass an actor instance, e.g. Tracing tracesFor: myActor">>,
        details = #{}
    }).

%% @private Call the trace store, ignoring errors if not running.
%% Used for fire-and-forget operations (enable, disable, clear).
-spec call_trace_store(fun(() -> term())) -> ok.
call_trace_store(Fun) ->
    try
        Fun(),
        ok
    catch
        exit:{noproc, _} -> ok;
        exit:{shutdown, _} -> ok
    end.

%% @private Call the trace store, returning a default value if not running.
%% Used for query operations where we need a meaningful fallback.
-spec call_trace_store_default(fun(() -> T), T) -> T when T :: term().
call_trace_store_default(Fun, Default) ->
    try
        Fun()
    catch
        exit:{noproc, _} -> Default;
        exit:{shutdown, _} -> Default
    end.
