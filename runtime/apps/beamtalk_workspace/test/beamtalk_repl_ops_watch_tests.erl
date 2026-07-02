%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_watch_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for `beamtalk_repl_ops_watch` — the live-Inspector `pid-stats` read
op (ADR 0095 §5, BT-2489 / Cockpit Phase 3).

Covers:
* invalid / missing pid string → `{error, #beamtalk_error{}}` (JSON error at the
  WebSocket edge);
* a valid-but-unknown pid → error (it is not a registered actor);
* a live registered actor → a `{value, StatsMap}` term whose binary-keyed metrics
  map carries the process_info fields (queue depth, memory, reductions, status).
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op, Id, Session, Legacy) ->
    {protocol_msg, Op, Id, Session, #{}, Legacy}.

stop_registry_if_running() ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            ok;
        Old ->
            Ref = erlang:monitor(process, Old),
            catch gen_server:stop(Old),
            receive
                {'DOWN', Ref, process, Old, _} -> ok
            after 1000 -> ok
            end
    end.

%%====================================================================
%% handle/4 — invalid / unknown pid paths
%%====================================================================

handle_pid_stats_invalid_pid_string_returns_error_test() ->
    Msg = make_msg(<<"pid-stats">>, <<"s-1">>, undefined, false),
    Result = beamtalk_repl_ops_watch:handle(
        <<"pid-stats">>, #{<<"actor">> => <<"not-a-pid">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(
        binary:match(maps:get(<<"error">>, Decoded), <<"Invalid actor PID">>) =/= nomatch
    ).

handle_pid_stats_missing_actor_returns_error_test() ->
    Msg = make_msg(<<"pid-stats">>, <<"s-2">>, undefined, false),
    Result = beamtalk_repl_ops_watch:handle(<<"pid-stats">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_pid_stats_unknown_actor_returns_error_test() ->
    %% Valid pid format but not a registered actor → unknown_actor.
    PidBin = list_to_binary(pid_to_list(self())),
    Msg = make_msg(<<"pid-stats">>, <<"s-3">>, undefined, false),
    Result = beamtalk_repl_ops_watch:handle(
        <<"pid-stats">>, #{<<"actor">> => PidBin}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% handle_term/4 — live registered actor returns a stats map
%%====================================================================

handle_term_pid_stats_live_actor_returns_value_map_test() ->
    stop_registry_if_running(),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    {ok, ActorPid} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    PidBin = list_to_binary(pid_to_list(ActorPid)),
    Msg = make_msg(<<"pid-stats">>, <<"s-live">>, undefined, false),
    try
        Result = beamtalk_repl_ops_watch:handle_term(
            <<"pid-stats">>, #{<<"actor">> => PidBin}, Msg, self()
        ),
        ?assertMatch({value, M} when is_map(M), Result),
        {value, Stats} = Result,
        ?assertEqual(true, maps:get(<<"alive">>, Stats)),
        ?assertEqual(PidBin, maps:get(<<"pid">>, Stats)),
        ?assert(is_integer(maps:get(<<"queue_depth">>, Stats))),
        ?assert(is_integer(maps:get(<<"memory_bytes">>, Stats))),
        ?assert(is_integer(maps:get(<<"reductions">>, Stats))),
        ?assert(maps:is_key(<<"status">>, Stats))
    after
        catch gen_server:stop(ActorPid),
        catch gen_server:stop(RegistryPid)
    end.

%%====================================================================
%% Internal helpers — exposed via -ifdef(TEST)
%%====================================================================

describe_ops_returns_pid_stats_descriptor_test() ->
    Ops = beamtalk_repl_ops_watch:describe_ops(),
    ?assert(is_map(Ops)),
    ?assert(maps:is_key(<<"pid-stats">>, Ops)).

pid_stats_dead_process_returns_dead_map_test() ->
    Pid = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> ok
    end,
    ?assertNot(erlang:is_process_alive(Pid)),
    Stats = beamtalk_repl_ops_watch:pid_stats(Pid),
    PidBin = list_to_binary(pid_to_list(Pid)),
    ?assertMatch(#{<<"pid">> := PidBin, <<"alive">> := false, <<"status">> := <<"dead">>}, Stats).

format_mfa_undefined_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_repl_ops_watch:format_mfa(undefined)).

format_mfa_catch_all_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_repl_ops_watch:format_mfa(not_an_mfa)).
