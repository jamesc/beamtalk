%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_actors_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_actors module.

Tests the validate_actor_pid/1 and is_known_actor/1 exported helpers plus
the handle/4 operation paths (the actor lists are process scans, ADR 0150):
- actors op → a scan; a synchronous spawn is visible at once (async: documented)
- inspect/kill ops with invalid or unknown PID strings → error JSON
- interrupt op with a dead SessionPid → noproc catch → ok status JSON

Plus the term-returning `handle_term/4`: the inspect success path
through a live tagged-map actor must return an `{inspect, FieldMap}` term with
internal fields filtered — exercising the introspection inside the `inspect`
`try` body (the body whose exception-safety was restored by moving it back
inside the `try`).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% validate_actor_pid/1
%%====================================================================

validate_invalid_string_returns_invalid_pid_test() ->
    ?assertEqual(
        {error, invalid_pid},
        beamtalk_repl_params:validate_actor_pid("not-a-pid")
    ).

validate_empty_string_returns_invalid_pid_test() ->
    ?assertEqual(
        {error, invalid_pid},
        beamtalk_repl_params:validate_actor_pid("")
    ).

validate_non_actor_pid_returns_unknown_actor_test() ->
    %% list_to_pid succeeds, but the pid carries no actor marker, so
    %% is_known_actor/1 is false -> unknown_actor.
    PidStr = pid_to_list(beamtalk_test_proc:spawn_idle()),
    ?assertEqual(
        {error, unknown_actor},
        beamtalk_repl_params:validate_actor_pid(PidStr)
    ).

validate_live_actor_pid_is_known_test() ->
    {ok, ActorPid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        ?assertMatch(
            {ok, ActorPid},
            beamtalk_repl_params:validate_actor_pid(pid_to_list(ActorPid))
        )
    after
        unlink(ActorPid),
        exit(ActorPid, kill)
    end.

%%====================================================================
%% is_known_actor/1
%%====================================================================

is_known_actor_non_actor_returns_false_test() ->
    ?assertEqual(false, beamtalk_repl_params:is_known_actor(beamtalk_test_proc:spawn_idle())).

%%====================================================================
%% handle/4 — actors op
%%====================================================================

handle_actors_op_shape_test() ->
    Msg = beamtalk_test_repl:make_msg(<<"actors">>, <<"a-1">>, undefined),
    Result = beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(is_list(maps:get(<<"actors">>, Decoded))),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%% `:actors` right after a synchronous spawn lists the actor (ADR 0150, BT-4950).
actors_op_sees_sync_spawn_immediately_test() ->
    {ok, ActorPid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        {actors, Actors} = beamtalk_repl_ops_actors:handle_term(
            <<"actors">>,
            #{},
            beamtalk_test_repl:make_msg(<<"actors">>, <<"a-2">>, undefined),
            self()
        ),
        ?assertMatch(
            [#{class := 'Counter', spawned_at := _}],
            [M || #{pid := P} = M <- Actors, P =:= ActorPid]
        ),
        %% The wire form is unchanged: pid / class / module / spawned_at.
        Msg = beamtalk_test_repl:make_msg(<<"actors">>, <<"a-3">>, undefined),
        Json = json:decode(beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self())),
        PidBin = list_to_binary(pid_to_list(ActorPid)),
        [Wire] = [W || #{<<"pid">> := P} = W <- maps:get(<<"actors">>, Json), P =:= PidBin],
        ?assertEqual(
            [<<"class">>, <<"module">>, <<"pid">>, <<"spawned_at">>],
            lists:sort(maps:keys(Wire))
        ),
        ?assert(is_integer(maps:get(<<"spawned_at">>, Wire)))
    after
        unlink(ActorPid),
        exit(ActorPid, kill)
    end.

%% An asynchronous spawn returns before `init/1` has run, so the very next
%% `:actors` MAY miss the actor (the ADR accepts this brief invisibility). What
%% is guaranteed: once `init/1` has run, the next `:actors` lists it.
actors_op_async_spawn_visible_once_started_test() ->
    {ok, {ActorPid, MonRef}} = beamtalk_actor:safe_spawn_monitored_async(test_counter, 0),
    Msg = beamtalk_test_repl:make_msg(<<"actors">>, <<"a-4">>, undefined),
    Listed = fun() ->
        {actors, As} = beamtalk_repl_ops_actors:handle_term(<<"actors">>, #{}, Msg, self()),
        lists:member(ActorPid, [P || #{pid := P} <- As])
    end,
    try
        _MayMiss = Listed(),
        _ = gen_server:call(ActorPid, {getValue, []}),
        ?assert(Listed())
    after
        erlang:demonitor(MonRef, [flush]),
        unlink(ActorPid),
        exit(ActorPid, kill)
    end.

%%====================================================================
%% handle_term/4 — inspect success path (live tagged-map actor)
%%====================================================================

handle_term_inspect_live_tagged_actor_returns_inspect_map_term_test() ->
    %% The term-returning inspect path must return an {inspect, FieldMap}
    %% *term* (not JSON) for a live tagged-map actor, with internal bookkeeping
    %% keys filtered out. This drives the whole introspection body of the inspect
    %% `try` (is_tagged → field_names → maps:with) to its normal completion — the
    %% body that was moved back inside the `try` so any future throw there
    %% is caught rather than escaping the op handler.
    (fun() ->
        %% A synchronous spawn: the marker is written, so the scan sees it.
        {ok, ActorPid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
        PidBin = list_to_binary(pid_to_list(ActorPid)),
        Msg = beamtalk_test_repl:make_msg(<<"inspect">>, <<"i-live">>, undefined),
        try
            Result = beamtalk_repl_ops_actors:handle_term(
                <<"inspect">>, #{<<"actor">> => PidBin}, Msg, self()
            ),
            ?assertMatch({inspect, M} when is_map(M), Result),
            {inspect, Fields} = Result,
            %% User-visible field is present; internal bookkeeping keys are filtered.
            ?assert(maps:is_key(value, Fields)),
            ?assertNot(maps:is_key('$beamtalk_class', Fields)),
            ?assertNot(maps:is_key('__methods__', Fields))
        after
            try
                gen_server:stop(ActorPid)
            catch
                _:_ -> ok
            end
        end
    end)().

%%====================================================================
%% System actors (BT-4960): listed and flagged; inspect works; kill refuses
%%====================================================================

actors_op_flags_system_actor_on_the_wire_test() ->
    Class = 'Bt4960WireSystem',
    beamtalk_test_fake_native:with_system_native([Class], fun(Pid) ->
        Msg = beamtalk_test_repl:make_msg(<<"actors">>, <<"s-1">>, undefined),
        Json = json:decode(beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self())),
        PidBin = list_to_binary(pid_to_list(Pid)),
        [Wire] = [W || #{<<"pid">> := P} = W <- maps:get(<<"actors">>, Json), P =:= PidBin],
        ?assertEqual(true, maps:get(<<"system">>, Wire)),
        ?assertEqual(atom_to_binary(Class, utf8), maps:get(<<"class">>, Wire))
    end).

inspect_system_actor_works_test() ->
    beamtalk_test_fake_native:with_system_native(['Bt4960InspectSystem'], fun(Pid) ->
        Msg = beamtalk_test_repl:make_msg(<<"inspect">>, <<"s-2">>, undefined),
        PidBin = list_to_binary(pid_to_list(Pid)),
        Result = beamtalk_repl_ops_actors:handle_term(
            <<"inspect">>, #{<<"actor">> => PidBin}, Msg, self()
        ),
        ?assertMatch({inspect, _}, Result)
    end).

kill_system_actor_is_refused_test() ->
    beamtalk_test_fake_native:with_system_native(['Bt4960KillSystem'], fun(Pid) ->
        Msg = beamtalk_test_repl:make_msg(<<"kill">>, <<"s-3">>, undefined),
        PidBin = list_to_binary(pid_to_list(Pid)),
        Result = beamtalk_repl_ops_actors:handle_term(
            <<"kill">>, #{<<"actor">> => PidBin}, Msg, self()
        ),
        ?assertMatch({error, #beamtalk_error{kind = system_actor}}, Result),
        ?assert(is_process_alive(Pid))
    end).

kill_user_native_actor_works_test() ->
    beamtalk_test_fake_native:with_native(['Bt4960KillUser'], fun(Pid) ->
        Msg = beamtalk_test_repl:make_msg(<<"kill">>, <<"s-4">>, undefined),
        PidBin = list_to_binary(pid_to_list(Pid)),
        ?assertEqual(
            {status, ok},
            beamtalk_repl_ops_actors:handle_term(
                <<"kill">>, #{<<"actor">> => PidBin}, Msg, self()
            )
        ),
        %% BT-5199: the reply comes only once the actor is gone, so a client
        %% reading `actors` straight after it — from another process, as the
        %% next request does — never lists the killed actor. (Signal order
        %% only holds per sender, so the scan runs in a fresh process.)
        Self = self(),
        spawn(fun() -> Self ! {listed, beamtalk_workspace_actors:list()} end),
        Listed =
            receive
                {listed, L} -> L
            after 5000 -> error(no_actor_list)
            end,
        ?assertEqual([], [A || #{pid := P} = A <- Listed, P =:= Pid])
    end).

%% BT-5199: killing an actor that has already stopped is refused, never
%% reported as a kill (the clients say "Killed X." only on `{status, ok}`).
kill_stopped_actor_is_refused_test() ->
    beamtalk_test_fake_native:with_native(['Bt5199KillTwice'], fun(Pid) ->
        Msg = beamtalk_test_repl:make_msg(<<"kill">>, <<"s-5">>, undefined),
        Params = #{<<"actor">> => list_to_binary(pid_to_list(Pid))},
        ?assertEqual(
            {status, ok}, beamtalk_repl_ops_actors:handle_term(<<"kill">>, Params, Msg, self())
        ),
        ?assertMatch(
            {error, #beamtalk_error{}},
            beamtalk_repl_ops_actors:handle_term(<<"kill">>, Params, Msg, self())
        )
    end).
