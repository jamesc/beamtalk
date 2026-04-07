%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_perf_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_perf module (ADR 0069 Phase 4).

Tests the enable-tracing, get-traces, and actor-stats REPL protocol ops.
Uses a running trace store for integration-style tests.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op) ->
    {protocol_msg, Op, <<"test-1">>, <<"session-1">>, #{}, false}.

decode_response(Bin) ->
    json:decode(Bin).

-doc """
Start trace store if not running.
Returns `{started, Pid}` when we started it, `{existing, Pid}` if already running.
""".
setup_trace_store() ->
    case whereis(beamtalk_trace_store) of
        undefined ->
            {ok, Pid} = beamtalk_trace_store:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup_trace_store({started, Pid}) ->
    %% Only stop if we started it — avoid interfering with other test modules.
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable(),
        gen_server:stop(Pid)
    catch
        exit:{noproc, _} -> ok
    end;
cleanup_trace_store({existing, _Pid}) ->
    %% Someone else owns the process — just reset state, don't stop it.
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable()
    catch
        exit:{noproc, _} -> ok
    end.

%%====================================================================
%% enable-tracing tests
%%====================================================================

enable_tracing_test_() ->
    {setup, fun setup_trace_store/0, fun cleanup_trace_store/1, fun(_Setup) ->
        [
            {"enable-tracing returns success", fun() ->
                Result = beamtalk_repl_ops_perf:handle(
                    <<"enable-tracing">>, #{}, make_msg(<<"enable-tracing">>), self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                ?assertEqual(<<"Tracing enabled">>, maps:get(<<"value">>, Decoded))
            end},
            {"enable-tracing actually enables", fun() ->
                beamtalk_trace_store:disable(),
                ?assertNot(beamtalk_trace_store:is_enabled()),
                _ = beamtalk_repl_ops_perf:handle(
                    <<"enable-tracing">>, #{}, make_msg(<<"enable-tracing">>), self()
                ),
                ?assert(beamtalk_trace_store:is_enabled())
            end}
        ]
    end}.

%%====================================================================
%% get-traces tests
%%====================================================================

get_traces_test_() ->
    {setup, fun setup_trace_store/0, fun cleanup_trace_store/1, fun(_Setup) ->
        [
            {"get-traces returns empty list when no traces", fun() ->
                Result = beamtalk_repl_ops_perf:handle(
                    <<"get-traces">>, #{}, make_msg(<<"get-traces">>), self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                ?assertEqual([], maps:get(<<"value">>, Decoded))
            end},
            {"get-traces with limit param", fun() ->
                Result = beamtalk_repl_ops_perf:handle(
                    <<"get-traces">>,
                    #{<<"limit">> => 5},
                    make_msg(<<"get-traces">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                ?assertEqual([], maps:get(<<"value">>, Decoded))
            end},
            {"get-traces with actor filter", fun() ->
                PidStr = list_to_binary(pid_to_list(self())),
                Result = beamtalk_repl_ops_perf:handle(
                    <<"get-traces">>,
                    #{<<"actor">> => PidStr},
                    make_msg(<<"get-traces">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                ?assertEqual([], maps:get(<<"value">>, Decoded))
            end}
        ]
    end}.

%%====================================================================
%% actor-stats tests
%%====================================================================

actor_stats_test_() ->
    {setup, fun setup_trace_store/0, fun cleanup_trace_store/1, fun(_Setup) ->
        [
            {"actor-stats returns success with value when no stats", fun() ->
                Result = beamtalk_repl_ops_perf:handle(
                    <<"actor-stats">>, #{}, make_msg(<<"actor-stats">>), self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                %% Verify response includes a value (empty map serialized by term_to_json)
                ?assert(maps:is_key(<<"value">>, Decoded))
            end},
            {"actor-stats with actor filter", fun() ->
                PidStr = list_to_binary(pid_to_list(self())),
                Result = beamtalk_repl_ops_perf:handle(
                    <<"actor-stats">>,
                    #{<<"actor">> => PidStr},
                    make_msg(<<"actor-stats">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded))
            end}
        ]
    end}.

%%====================================================================
%% export-traces tests
%%====================================================================

export_traces_test_() ->
    {setup, fun setup_trace_store/0, fun cleanup_trace_store/1, fun(_Setup) ->
        [
            {"export-traces with no traces returns path and count", fun() ->
                TmpFile = tmp_export_path("ops_no_traces"),
                Result = beamtalk_repl_ops_perf:handle(
                    <<"export-traces">>,
                    #{<<"path">> => TmpFile},
                    make_msg(<<"export-traces">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                Value = maps:get(<<"value">>, Decoded),
                ?assertEqual(0, maps:get(<<"count">>, Value)),
                ?assert(is_binary(maps:get(<<"path">>, Value))),
                file:delete(TmpFile)
            end},
            {"export-traces with filters", fun() ->
                beamtalk_trace_store:enable(),
                beamtalk_trace_store:record_trace_event(
                    self(), 'Counter', increment, sync, 5000, ok, #{}, stop
                ),
                TmpFile = tmp_export_path("ops_filtered"),
                PidStr = list_to_binary(pid_to_list(self())),
                Result = beamtalk_repl_ops_perf:handle(
                    <<"export-traces">>,
                    #{
                        <<"path">> => TmpFile,
                        <<"actor">> => PidStr,
                        <<"selector">> => <<"increment">>,
                        <<"limit">> => 10
                    },
                    make_msg(<<"export-traces">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                Value = maps:get(<<"value">>, Decoded),
                ?assertEqual(1, maps:get(<<"count">>, Value)),
                file:delete(TmpFile)
            end},
            {"export-traces with default path", fun() ->
                Result = beamtalk_repl_ops_perf:handle(
                    <<"export-traces">>,
                    #{},
                    make_msg(<<"export-traces">>),
                    self()
                ),
                Decoded = decode_response(Result),
                ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
                Value = maps:get(<<"value">>, Decoded),
                Path = maps:get(<<"path">>, Value),
                ?assert(is_binary(Path)),
                file:delete(Path)
            end}
        ]
    end}.

-doc "Generate a unique temporary export path.".
tmp_export_path(Tag) ->
    iolist_to_binary([
        "beamtalk_test_ops_export_",
        Tag,
        "_",
        integer_to_list(erlang:unique_integer([positive])),
        ".json"
    ]).

%%====================================================================
%% Error handling tests
%%====================================================================

error_handling_test_() ->
    {setup, fun setup_trace_store/0, fun cleanup_trace_store/1, fun(_Setup) ->
        [
            {"invalid PID raises structured error", fun() ->
                ?assertError(
                    #beamtalk_error{kind = invalid_argument, class = 'Tracing'},
                    beamtalk_repl_ops_perf:handle(
                        <<"get-traces">>,
                        #{<<"actor">> => <<"not-a-pid">>},
                        make_msg(<<"get-traces">>),
                        self()
                    )
                )
            end},
            {"unknown selector raises structured error", fun() ->
                PidStr = list_to_binary(pid_to_list(self())),
                ?assertError(
                    #beamtalk_error{kind = invalid_argument, class = 'Tracing'},
                    beamtalk_repl_ops_perf:handle(
                        <<"get-traces">>,
                        #{
                            <<"actor">> => PidStr,
                            <<"selector">> => <<"nonexistent_selector_xyz_12345">>
                        },
                        make_msg(<<"get-traces">>),
                        self()
                    )
                )
            end},
            {"invalid PID in actor-stats raises structured error", fun() ->
                ?assertError(
                    #beamtalk_error{kind = invalid_argument, class = 'Tracing'},
                    beamtalk_repl_ops_perf:handle(
                        <<"actor-stats">>,
                        #{<<"actor">> => <<"garbage">>},
                        make_msg(<<"actor-stats">>),
                        self()
                    )
                )
            end}
        ]
    end}.
