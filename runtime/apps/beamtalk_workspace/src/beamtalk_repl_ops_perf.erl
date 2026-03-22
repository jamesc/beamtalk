%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for tracing and performance operations (ADR 0069 Phase 4).
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Provides REPL protocol ops for the MCP tracing workflow:
%%% - enable-tracing: start trace event capture
%%% - get-traces: query trace events with filtering
%%% - actor-stats: query aggregate actor statistics
%%%
%%% Delegates to beamtalk_tracing which wraps beamtalk_trace_store.
%%% @see beamtalk_tracing
%%% @see docs/ADR/0069-actor-observability-and-tracing.md

-module(beamtalk_repl_ops_perf).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4]).

%% @doc Handle enable-tracing, get-traces, and actor-stats ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"enable-tracing">>, _Params, Msg, _SessionPid) ->
    beamtalk_tracing:enable(),
    beamtalk_repl_protocol:encode_result(
        <<"Tracing enabled">>, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"get-traces">>, Params, Msg, _SessionPid) ->
    Actor = maps:get(<<"actor">>, Params, undefined),
    Selector = maps:get(<<"selector">>, Params, undefined),
    Limit = maps:get(<<"limit">>, Params, undefined),
    Traces = get_traces(Actor, Selector),
    Limited = apply_limit(Traces, Limit),
    beamtalk_repl_protocol:encode_result(
        Limited, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"actor-stats">>, Params, Msg, _SessionPid) ->
    Actor = maps:get(<<"actor">>, Params, undefined),
    Stats = get_stats(Actor),
    beamtalk_repl_protocol:encode_result(
        Stats, Msg, fun beamtalk_repl_json:term_to_json/1
    ).

%%% Internal helpers

%% @private Get traces with optional actor and selector filters.
-spec get_traces(binary() | undefined, binary() | undefined) -> [map()].
get_traces(undefined, _Selector) ->
    beamtalk_tracing:traces();
get_traces(ActorBin, undefined) ->
    Pid = parse_pid(ActorBin),
    beamtalk_tracing:tracesFor(Pid);
get_traces(ActorBin, SelectorBin) ->
    Pid = parse_pid(ActorBin),
    Sel = parse_selector(SelectorBin),
    beamtalk_tracing:tracesFor(Pid, Sel).

%% @private Get stats with optional actor filter.
-spec get_stats(binary() | undefined) -> map().
get_stats(undefined) ->
    beamtalk_tracing:stats();
get_stats(ActorBin) ->
    Pid = parse_pid(ActorBin),
    beamtalk_tracing:statsFor(Pid).

%% @private Apply optional limit to trace results.
-spec apply_limit([map()], integer() | undefined) -> [map()].
apply_limit(Traces, undefined) ->
    Traces;
apply_limit(Traces, Limit) when is_integer(Limit), Limit > 0 ->
    lists:sublist(Traces, Limit);
apply_limit(Traces, _) ->
    Traces.

%% @private Parse a PID string (e.g. <<"<0.123.0>">>) to an Erlang pid.
%% Raises a structured beamtalk_error on invalid input.
-spec parse_pid(binary()) -> pid().
parse_pid(PidBin) when is_binary(PidBin) ->
    try
        list_to_pid(binary_to_list(PidBin))
    catch
        error:badarg ->
            error(#beamtalk_error{
                kind = invalid_argument,
                class = 'Tracing',
                message = iolist_to_binary(
                    [<<"Invalid actor PID: ">>, PidBin]
                ),
                hint = <<"Use list-actors to find valid PIDs, e.g. \"<0.123.0>\"">>,
                details = #{}
            })
    end.

%% @private Parse a selector binary to an existing atom.
%% Uses binary_to_existing_atom to avoid atom table exhaustion from user input.
%% Raises a structured beamtalk_error if the selector is unknown.
-spec parse_selector(binary()) -> atom().
parse_selector(SelectorBin) ->
    try
        binary_to_existing_atom(SelectorBin, utf8)
    catch
        error:badarg ->
            error(#beamtalk_error{
                kind = invalid_argument,
                class = 'Tracing',
                message = iolist_to_binary(
                    [<<"Unknown selector: ">>, SelectorBin]
                ),
                hint = <<"The selector must match an existing method name.">>,
                details = #{}
            })
    end.
