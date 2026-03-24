%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for tracing and performance operations (ADR 0069 Phase 4).
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Provides REPL protocol ops for the MCP tracing workflow:
%%% - enable-tracing: start trace event capture
%%% - disable-tracing: stop trace event capture
%%% - get-traces: query trace events with filtering
%%% - actor-stats: query aggregate actor statistics
%%% - export-traces: export trace events to a JSON file
%%%
%%% Delegates to beamtalk_tracing which wraps beamtalk_trace_store.
%%% @see beamtalk_tracing
%%% @see docs/ADR/0069-actor-observability-and-tracing.md

-module(beamtalk_repl_ops_perf).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, describe_ops/0]).

%% @doc Handle enable-tracing, get-traces, actor-stats, and export-traces ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"enable-tracing">>, _Params, Msg, _SessionPid) ->
    beamtalk_tracing:enable(),
    beamtalk_repl_protocol:encode_result(
        <<"Tracing enabled">>, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"disable-tracing">>, _Params, Msg, _SessionPid) ->
    beamtalk_tracing:disable(),
    beamtalk_repl_protocol:encode_result(
        <<"Tracing disabled">>, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"get-traces">>, Params, Msg, _SessionPid) ->
    Limit = maps:get(<<"limit">>, Params, undefined),
    Opts = build_trace_filter_opts(Params),
    Traces = beamtalk_tracing:traces(Opts),
    Limited = apply_limit(Traces, Limit),
    beamtalk_repl_protocol:encode_result(
        Limited, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"actor-stats">>, Params, Msg, _SessionPid) ->
    Actor = maps:get(<<"actor">>, Params, undefined),
    Stats = get_stats(Actor),
    beamtalk_repl_protocol:encode_result(
        Stats, Msg, fun beamtalk_repl_json:term_to_json/1
    );
handle(<<"export-traces">>, Params, Msg, _SessionPid) ->
    Opts = build_export_opts(Params),
    case beamtalk_tracing:exportTraces(Opts) of
        {ok, #{path := Path, count := Count}} ->
            Result = #{
                <<"path">> => iolist_to_binary(Path),
                <<"count">> => Count
            },
            %% Result is already a JSX-compatible map with binary keys;
            %% use identity to avoid term_to_json stringifying the map.
            beamtalk_repl_protocol:encode_result(
                Result, Msg, fun(X) -> X end
            );
        {error, Reason} ->
            ErrMsg = iolist_to_binary(
                io_lib:format("Failed to export traces: ~p", [Reason])
            ),
            error(#beamtalk_error{
                kind = io_error,
                class = 'Tracing',
                message = ErrMsg,
                hint = <<"Check file path and permissions.">>,
                details = #{}
            })
    end.

%%% Internal helpers

%% @private Build export options map from REPL params.
%% Reuses build_trace_filter_opts for filter params, adds path and limit.
-spec build_export_opts(map()) -> map().
build_export_opts(Params) ->
    FilterOpts = build_trace_filter_opts(Params),
    Opts1 =
        case maps:get(<<"path">>, Params, undefined) of
            undefined -> FilterOpts;
            PathBin -> FilterOpts#{path => PathBin}
        end,
    case maps:get(<<"limit">>, Params, undefined) of
        undefined -> Opts1;
        Limit when is_integer(Limit) -> Opts1#{limit => Limit};
        _ -> Opts1
    end.

%% @private Build trace filter opts map from REPL protocol params.
%% Supports: actor, selector, class, outcome, min_duration_ns.
-spec build_trace_filter_opts(map()) -> map().
build_trace_filter_opts(Params) ->
    Opts0 = #{},
    Opts1 =
        case maps:get(<<"actor">>, Params, undefined) of
            undefined -> Opts0;
            ActorBin -> Opts0#{actor => parse_pid(ActorBin)}
        end,
    Opts2 =
        case maps:get(<<"selector">>, Params, undefined) of
            undefined -> Opts1;
            SelectorBin -> Opts1#{selector => parse_selector(SelectorBin)}
        end,
    Opts3 =
        case maps:get(<<"class">>, Params, undefined) of
            undefined -> Opts2;
            ClassBin -> Opts2#{class => parse_class(ClassBin)}
        end,
    Opts4 =
        case maps:get(<<"outcome">>, Params, undefined) of
            undefined -> Opts3;
            OutcomeBin -> Opts3#{outcome => parse_outcome(OutcomeBin)}
        end,
    case maps:get(<<"min_duration_ns">>, Params, undefined) of
        undefined ->
            Opts4;
        MinNs when is_integer(MinNs), MinNs >= 0 -> Opts4#{min_duration_ns => MinNs};
        BadNs ->
            error(#beamtalk_error{
                kind = invalid_argument,
                class = 'Tracing',
                message = iolist_to_binary(
                    io_lib:format("Invalid min_duration_ns: ~p", [BadNs])
                ),
                hint = <<"min_duration_ns must be a non-negative integer (nanoseconds).">>,
                details = #{}
            })
    end.

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

%% @private Parse a class filter binary to an existing atom.
%% Uses binary_to_existing_atom to avoid atom table exhaustion from user input.
%% Raises a structured beamtalk_error if the class name is unknown.
-spec parse_class(binary()) -> atom().
parse_class(ClassBin) ->
    try
        binary_to_existing_atom(ClassBin, utf8)
    catch
        error:badarg ->
            error(#beamtalk_error{
                kind = invalid_argument,
                class = 'Tracing',
                message = iolist_to_binary(
                    [<<"Unknown class: ">>, ClassBin]
                ),
                hint = <<"The class must match an existing actor class name.">>,
                details = #{}
            })
    end.

%% @private Parse an outcome filter binary to one of the valid outcome atoms.
%% Valid outcomes: ok, error, timeout.
%% Raises a structured beamtalk_error for invalid values.
-spec parse_outcome(binary()) -> ok | error | timeout.
parse_outcome(<<"ok">>) ->
    ok;
parse_outcome(<<"error">>) ->
    error;
parse_outcome(<<"timeout">>) ->
    timeout;
parse_outcome(OutcomeBin) ->
    error(#beamtalk_error{
        kind = invalid_argument,
        class = 'Tracing',
        message = iolist_to_binary(
            [<<"Invalid outcome: ">>, OutcomeBin]
        ),
        hint = <<"Valid outcomes are: ok, error, timeout.">>,
        details = #{}
    }).

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

%% @doc Return op descriptors for the tracing/performance operations.
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"enable-tracing">> => #{<<"params">> => []},
        <<"disable-tracing">> => #{<<"params">> => []},
        <<"get-traces">> => #{
            <<"params">> => [],
            <<"optional">> => [
                <<"actor">>,
                <<"selector">>,
                <<"class">>,
                <<"outcome">>,
                <<"min_duration_ns">>,
                <<"limit">>
            ]
        },
        <<"actor-stats">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"actor">>]
        },
        <<"export-traces">> => #{
            <<"params">> => [],
            <<"optional">> => [
                <<"path">>,
                <<"actor">>,
                <<"selector">>,
                <<"class">>,
                <<"outcome">>,
                <<"min_duration_ns">>,
                <<"limit">>
            ],
            <<"notes">> => <<"Exports trace events to a JSON file with optional filters.">>
        }
    }.
