%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_actors_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_actors module.

Tests the validate_actor_pid/1 and is_known_actor/1 exported helpers plus
the handle/4 operation paths that require no running actor registry:
- actors op → empty list when registry absent
- inspect/kill ops with invalid or unknown PID strings → error JSON
- interrupt op with a dead SessionPid → noproc catch → ok status JSON

Plus the term-returning `handle_term/4` (BT-2399): the inspect success path
through a live tagged-map actor must return an `{inspect, FieldMap}` term with
internal fields filtered — exercising the introspection inside the `inspect`
`try` body (the body whose exception-safety the BT-2399 fix restored).
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op, Id, Session, Legacy) ->
    {protocol_msg, Op, Id, Session, #{}, Legacy}.

%% Stop any stale globally-registered actor registry so start_link/4 below
%% does not fail with {already_started, _}. Mirrors the pattern in
%% beamtalk_repl_server_tests.
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
%% validate_actor_pid/1
%%====================================================================

validate_invalid_string_returns_invalid_pid_test() ->
    ?assertEqual(
        {error, invalid_pid},
        beamtalk_repl_ops_actors:validate_actor_pid("not-a-pid")
    ).

validate_empty_string_returns_invalid_pid_test() ->
    ?assertEqual(
        {error, invalid_pid},
        beamtalk_repl_ops_actors:validate_actor_pid("")
    ).

validate_valid_pid_no_registry_returns_unknown_actor_test() ->
    %% list_to_pid succeeds, but beamtalk_actor_registry is not running in unit
    %% tests so is_known_actor/1 returns false → unknown_actor.
    PidStr = pid_to_list(self()),
    ?assertEqual(
        {error, unknown_actor},
        beamtalk_repl_ops_actors:validate_actor_pid(PidStr)
    ).

%%====================================================================
%% is_known_actor/1
%%====================================================================

is_known_actor_no_registry_returns_false_test() ->
    %% whereis(beamtalk_actor_registry) returns undefined in unit tests → false.
    ?assertEqual(false, beamtalk_repl_ops_actors:is_known_actor(self())).

%%====================================================================
%% handle/4 — actors op
%%====================================================================

handle_actors_no_registry_returns_empty_list_test() ->
    Msg = make_msg(<<"actors">>, <<"a-1">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"actors">>, Decoded)),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

handle_actors_no_registry_legacy_format_test() ->
    %% Legacy protocol: response uses <<"type">> field instead of <<"status">>.
    Msg = make_msg(<<"actors">>, undefined, undefined, true),
    Result = beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assertEqual(<<"actors">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)).

%%====================================================================
%% handle/4 — inspect op (invalid / unknown PID paths)
%%====================================================================

handle_inspect_invalid_pid_string_returns_error_test() ->
    Msg = make_msg(<<"inspect">>, <<"i-1">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(
        <<"inspect">>, #{<<"actor">> => <<"not-a-pid">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(
        binary:match(maps:get(<<"error">>, Decoded), <<"Invalid actor PID">>) =/= nomatch
    ).

handle_inspect_empty_actor_param_returns_error_test() ->
    %% Empty binary → empty string → list_to_pid raises badarg → invalid_pid.
    Msg = make_msg(<<"inspect">>, <<"i-2">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(
        <<"inspect">>, #{<<"actor">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_inspect_missing_actor_param_returns_error_test() ->
    %% No <<"actor">> key: defaults to <<>> → invalid_pid.
    Msg = make_msg(<<"inspect">>, <<"i-3">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(<<"inspect">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_inspect_unknown_actor_pid_returns_error_test() ->
    %% Valid PID string format but not registered in actor registry → unknown_actor.
    %% invalid_pid_error wraps it with the same "Invalid actor PID" message.
    PidBin = list_to_binary(pid_to_list(self())),
    Msg = make_msg(<<"inspect">>, <<"i-4">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(
        <<"inspect">>, #{<<"actor">> => PidBin}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(
        binary:match(maps:get(<<"error">>, Decoded), <<"Invalid actor PID">>) =/= nomatch
    ).

%%====================================================================
%% handle/4 — kill op (invalid PID paths)
%%====================================================================

handle_kill_invalid_pid_actor_key_returns_error_test() ->
    Msg = make_msg(<<"kill">>, <<"k-1">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(
        <<"kill">>, #{<<"actor">> => <<"bad-pid">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(
        binary:match(maps:get(<<"error">>, Decoded), <<"Invalid actor PID">>) =/= nomatch
    ).

handle_kill_invalid_pid_pid_key_returns_error_test() ->
    %% kill/2 falls back to <<"pid">> when <<"actor">> is absent.
    Msg = make_msg(<<"kill">>, <<"k-2">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(
        <<"kill">>, #{<<"pid">> => <<"bad-pid">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_kill_missing_pid_returns_error_test() ->
    %% No PID key → defaults to <<>> → list_to_pid raises badarg → invalid_pid.
    Msg = make_msg(<<"kill">>, <<"k-3">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(<<"kill">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% handle/4 — interrupt op
%%====================================================================

handle_interrupt_dead_session_catches_noproc_test() ->
    %% Spawn a process that exits immediately, wait for its DOWN signal,
    %% then pass the dead PID as SessionPid. The gen_server:call inside
    %% beamtalk_repl_shell:interrupt/1 raises exit:{noproc,_}, which the
    %% interrupt handler catches and converts to an ok status response.
    Pid = spawn(fun() -> ok end),
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end,
    Msg = make_msg(<<"interrupt">>, <<"int-1">>, undefined, false),
    Result = beamtalk_repl_ops_actors:handle(<<"interrupt">>, #{}, Msg, Pid),
    Decoded = json:decode(Result),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% handle_term/4 — inspect success path (live tagged-map actor)
%%====================================================================

handle_term_inspect_live_tagged_actor_returns_inspect_map_term_test() ->
    %% BT-2399: the term-returning inspect path must return an {inspect, FieldMap}
    %% *term* (not JSON) for a live tagged-map actor, with internal bookkeeping
    %% keys filtered out. This drives the whole introspection body of the inspect
    %% `try` (is_tagged → field_names → maps:with) to its normal completion — the
    %% body the BT-2399 fix moved back inside the `try` so any future throw there
    %% is caught rather than escaping the op handler.
    stop_registry_if_running(),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    {ok, ActorPid} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    PidBin = list_to_binary(pid_to_list(ActorPid)),
    Msg = make_msg(<<"inspect">>, <<"i-live">>, undefined, false),
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
        catch gen_server:stop(ActorPid),
        catch gen_server:stop(RegistryPid)
    end.
